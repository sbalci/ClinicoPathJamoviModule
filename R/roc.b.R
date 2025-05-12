#' @title ROC Curve Analysis
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import pROC
#' @import ROCR
#' @import cutpointr
#' @importFrom ggplot2 ggplot aes geom_line geom_ribbon geom_point theme_minimal
#' @importFrom ggplot2 labs scale_color_manual scale_fill_manual element_text
#' @importFrom dplyr filter mutate arrange desc
#' @importFrom tidyr pivot_longer

rocClass <- if (requireNamespace("jmvcore"))
    R6::R6Class(
        "rocClass",
        inherit = rocBase,
        private = list(
            # Private fields to store result objects for plotting
            .rocData = NULL,
            .rocResult = NULL,
            .optimalCriteria = NULL,

            # Initialization
            .init = function() {
                message("Initializing ROC analysis...")

                if (is.null(self$options$classvar) ||
                    is.null(self$options$testvar))
                    return()

                # The table is now initialized on the fly in the .run function
                # based on the youden parameter
            },

            # Main analysis
            .run = function() {
                # Check if we have all required variables
                if (is.null(self$options$classvar) ||
                    is.null(self$options$testvar)) {
                    message("Missing required variables: classvar or testvar")
                    return()
                }

                # Extract options
                classvar <- self$options$classvar
                testvar <- self$options$testvar
                classpos <- self$options$classpos
                direction <- self$options$direction
                ci <- self$options$ci
                cimethod <- self$options$cimethod
                youden <- self$options$youden
                optimcrit <- self$options$optimcrit
                pp <- self$options$pp
                pprob <- self$options$pprob
                costratioFP <- self$options$costratioFP

                if (is.null(classvar) ||
                    is.null(testvar)) {
                    message("Variables are null after extraction")
                    return()
                }

                # Check data
                if (nrow(self$data) == 0) {
                    message("Data contains no rows")
                    stop("Data contains no (complete) rows")
                }

                # Print extraction info
                message("Variables successfully extracted:")
                message("classvar: ", classvar)
                message("testvar: ", testvar)
                message("classpos: ", classpos)
                message("direction: ", direction)
                message("ci: ", ci)
                message("cimethod: ", cimethod)

                # Prepare data
                data <- self$data
                data <- jmvcore::naOmit(data)

                # Convert class variable to binary factor
                data[[classvar]] <- factor(data[[classvar]])

                # Check if class variable has exactly 2 levels
                if (nlevels(data[[classvar]]) != 2) {
                    stop("Classification variable must have exactly 2 levels")
                }

                # Create binary response vector (0/1 for negative/positive)
                response <- as.numeric(data[[classvar]] == classpos)
                predictor <- data[[testvar]]

                # Calculate total positives and negatives
                n_pos <- sum(response == 1)
                n_neg <- sum(response == 0)

                # Determine direction
                direction_text <- ifelse(direction == "greatpos", ">", "<")

                # Print debug information
                message("ROC analysis parameters:")
                message("Class variable: ", classvar)
                message("Test variable: ", testvar)
                message("Direction: ", direction_text)
                message("Data rows: ", nrow(data))
                message("Number of positives: ", n_pos)
                message("Number of negatives: ", n_neg)

                # Perform ROC analysis using pROC
                # Wrap in tryCatch to handle any errors
                roc_result <- tryCatch({
                    message("Attempting pROC analysis...")
                    message("User-selected direction: ", direction_text)

                    # First try with the user-specified direction
                    roc_obj <- pROC::roc(
                        response = response,
                        predictor = predictor,
                        direction = direction_text,
                        levels = c(0, 1),
                        ci = ci,
                        ci.method = cimethod,
                        quiet = TRUE
                    )

                    # Check the AUC - if it's less than 0.5, we need to invert
                    auc_value <- as.numeric(roc_obj$auc)
                    message("AUC with user direction: ", auc_value)

                    # If AUC < 0.5, invert the predictor instead of changing direction
                    # This preserves the user's direction preference while ensuring AUC > 0.5
                    if (auc_value < 0.5) {
                        message("AUC < 0.5, inverting the predictor (1 - AUC = ",
                                1 - auc_value,
                                ")")

                        # Re-run ROC analysis with the same direction but inverted predictor
                        roc_obj <- pROC::roc(
                            response = response,
                            predictor = -predictor,
                            # Invert the predictor
                            direction = direction_text,
                            levels = c(0, 1),
                            ci = ci,
                            ci.method = cimethod,
                            quiet = TRUE
                        )

                        # Store that we inverted the predictor
                        roc_obj$predictor_inverted <- TRUE
                    } else {
                        roc_obj$predictor_inverted <- FALSE
                    }

                    message("Final AUC: ", roc_obj$auc)

                    # Store additional information that might be needed for fallback plotting
                    roc_obj$n_pos <- n_pos
                    roc_obj$n_neg <- n_neg
                    roc_obj$user_direction <- direction_text

                    roc_obj
                }, error = function(e) {
                    # If pROC fails, try a simpler approach using manual calculation
                    message("pROC error: ",
                            e$message,
                            ". Using manual ROC implementation.")

                    # Check both orientations but respect user's direction preference
                    # Use the predictor as is for the first calculation
                    pred1 <- predictor
                    sorted_idx1 <- order(pred1, decreasing = (direction_text == ">"))
                    sorted_response1 <- response[sorted_idx1]
                    sorted_predictor1 <- pred1[sorted_idx1]
                    sens1 <- cumsum(sorted_response1) / n_pos
                    spec1 <- (n_neg - cumsum(1 - sorted_response1)) / n_neg
                    auc1 <- sum(diff(1 - spec1) * (sens1[-1] + sens1[-length(sens1)]) / 2)

                    # Invert the predictor for the second calculation
                    pred2 <- -predictor
                    sorted_idx2 <- order(pred2, decreasing = (direction_text == ">"))
                    sorted_response2 <- response[sorted_idx2]
                    sorted_predictor2 <- pred2[sorted_idx2]
                    sens2 <- cumsum(sorted_response2) / n_pos
                    spec2 <- (n_neg - cumsum(1 - sorted_response2)) / n_neg
                    auc2 <- sum(diff(1 - spec2) * (sens2[-1] + sens2[-length(sens2)]) / 2)

                    # Choose the orientation that gives AUC > 0.5
                    # But keep user's direction preference
                    if (auc1 >= 0.5) {
                        message("Using original predictor with AUC = ", auc1)
                        auc_value <- auc1
                        sens <- sens1
                        spec <- spec1
                        sorted_predictor <- sorted_predictor1
                        predictor_inverted <- FALSE
                    } else {
                        message("Using inverted predictor with AUC = ", auc2)
                        auc_value <- auc2
                        sens <- sens2
                        spec <- spec2
                        sorted_predictor <- sorted_predictor2
                        predictor_inverted <- TRUE
                    }

                    # Calculate SE using the formula: SE = sqrt(AUC*(1-AUC)/(n_pos*n_neg))
                    auc_se <- sqrt((auc_value * (1 - auc_value)) / (n_pos * n_neg))

                    # Calculate 95% CI using normal approximation
                    z_critical <- qnorm(0.975)  # 1.96 for 95% CI
                    auc_lci <- max(0, auc_value - z_critical * auc_se)
                    auc_uci <- min(1, auc_value + z_critical * auc_se)

                    # Calculate z-statistic and p-value
                    z_stat <- (auc_value - 0.5) / auc_se
                    p_val <- 2 * (1 - pnorm(abs(z_stat)))

                    message(
                        "Manual calculation - AUC: ",
                        auc_value,
                        ", SE: ",
                        auc_se,
                        ", 95% CI: [",
                        auc_lci,
                        ", ",
                        auc_uci,
                        "]",
                        ", z: ",
                        z_stat,
                        ", p: ",
                        p_val
                    )

                    list(
                        auc = auc_value,
                        sensitivities = sens,
                        specificities = spec,
                        thresholds = sorted_predictor,
                        ci = c(auc_lci, auc_value, auc_uci),
                        se = auc_se,
                        z = z_stat,
                        p = p_val,
                        n_pos = n_pos,
                        n_neg = n_neg,
                        user_direction = direction_text,
                        predictor_inverted = predictor_inverted
                    )
                })

                # Store for plotting
                private$.rocResult <- roc_result

                # Get coordinates
                coords <- data.frame(
                    threshold = as.numeric(roc_result$thresholds),
                    sens = as.numeric(roc_result$sensitivities),
                    spec = as.numeric(roc_result$specificities)
                )

                # If the ROC curve has no valid points, create a default point
                if (nrow(coords) == 0) {
                    coords <- data.frame(threshold = NA,
                                         sens = NA,
                                         spec = NA)
                }

                # Calculate total positives and negatives
                n_pos <- sum(response == 1)
                n_neg <- sum(response == 0)

                # Calculate predictive values and likelihood ratios
                if (pp) {
                    # Use specified prevalence
                    prevalence <- pprob
                } else {
                    # Use sample prevalence
                    prevalence <- n_pos / (n_pos + n_neg)
                }

                # Calculate PPV, NPV, LR+, LR-
                coords$ppv <- (coords$sens * prevalence) /
                    ((coords$sens * prevalence) + ((1 - coords$spec) * (1 - prevalence)))
                coords$npv <- (coords$spec * (1 - prevalence)) /
                    ((coords$spec * (1 - prevalence)) + ((1 - coords$sens) * prevalence))
                coords$lrp <- coords$sens / (1 - coords$spec)
                coords$lrn <- (1 - coords$sens) / coords$spec

                # Calculate Youden's J
                coords$J <- coords$sens + coords$spec - 1

                # Store coordinates
                private$.rocData <- coords

                # Find optimal criterion values
                if (optimcrit) {
                    # 1. Youden Index (J) - maximize sensitivity + specificity - 1
                    j_max_idx <- which.max(coords$J)

                    # 2. Cost ratio optimized
                    # Formula: maximize sensitivity * prevalence - (1 - specificity) * (1 - prevalence) * cost ratio
                    cost_fn <- function(i) {
                        return(
                            coords$sens[i] * prevalence -
                                (1 - coords$spec[i]) * (1 - prevalence) * costratioFP
                        )
                    }
                    cost_values <- sapply(1:nrow(coords), cost_fn)
                    cost_max_idx <- which.max(cost_values)

                    # 3. Closest to (0,1) point
                    # Calculate Euclidean distance to (0,1) point
                    distances <- sqrt((1 - coords$sens)^2 + (1 - coords$spec)^2)
                    closest_idx <- which.min(distances)

                    # 4. Equal sensitivity and specificity
                    eq_diff <- abs(coords$sens - coords$spec)
                    eq_idx <- which.min(eq_diff)

                    # Store optimal criteria
                    optimalCriteria <- data.frame(
                        type = c(
                            "Youden Index (J)",
                            "Cost-Ratio Optimized",
                            "Closest to (0,1)",
                            "Equal Sensitivity & Specificity"
                        ),
                        threshold = c(
                            coords$threshold[j_max_idx],
                            coords$threshold[cost_max_idx],
                            coords$threshold[closest_idx],
                            coords$threshold[eq_idx]
                        ),
                        sens = c(
                            coords$sens[j_max_idx],
                            coords$sens[cost_max_idx],
                            coords$sens[closest_idx],
                            coords$sens[eq_idx]
                        ),
                        spec = c(
                            coords$spec[j_max_idx],
                            coords$spec[cost_max_idx],
                            coords$spec[closest_idx],
                            coords$spec[eq_idx]
                        ),
                        ppv = c(coords$ppv[j_max_idx], coords$ppv[cost_max_idx], coords$ppv[closest_idx], coords$ppv[eq_idx]),
                        npv = c(coords$npv[j_max_idx], coords$npv[cost_max_idx], coords$npv[closest_idx], coords$npv[eq_idx]),
                        j = c(coords$J[j_max_idx], coords$J[cost_max_idx], coords$J[closest_idx], coords$J[eq_idx])
                    )

                    private$.optimalCriteria <- optimalCriteria
                }

                # Fill summary table
                summary <- self$results$summary

                # Extract values safely ensuring they are single values
                auc_value <- as.numeric(roc_result$auc)[1]  # Ensure single value

                # Handle confidence intervals and SE safely
                auc_se <- NA
                auc_lci <- NA
                auc_uci <- NA
                z_stat <- NA
                p_val <- NA

                # Try to get SE from the roc_result
                if (ci &&
                    !is.null(attr(roc_result$ci, "specs"))) {
                    if ("se" %in% names(attr(roc_result$ci, "specs"))) {
                        auc_se <- as.numeric(attr(roc_result$ci, "specs")$se)[1]
                    }

                    if (length(roc_result$ci) >= 3) {
                        auc_lci <- as.numeric(roc_result$ci[1])
                        auc_uci <- as.numeric(roc_result$ci[3])
                    }
                }

                # If SE is missing but we have auc, calculate it
                if (is.na(auc_se) &&
                    !is.na(auc_value)) {
                    auc_se <- sqrt((auc_value * (1 - auc_value)) / (n_pos * n_neg))
                }

                # If CI is missing but we have SE, calculate it
                if (ci &&
                    (is.na(auc_lci) || is.na(auc_uci)) && !is.na(auc_se)) {
                    z_critical <- qnorm(0.975) # 1.96 for 95% CI
                    auc_lci <- max(0, auc_value - z_critical * auc_se)
                    auc_uci <- min(1, auc_value + z_critical * auc_se)
                }

                # Calculate z and p safely
                if (!is.na(auc_se) &&
                    auc_se > 0) {
                    z_stat <- (auc_value - 0.5) / auc_se
                    p_val <- 2 * (1 - pnorm(abs(z_stat)))
                }

                # If we have a 'se' directly in roc_result, use it
                if (!is.null(roc_result$se)) {
                    auc_se <- as.numeric(roc_result$se)[1]
                }

                # If we have z and p directly in roc_result, use them
                if (!is.null(roc_result$z)) {
                    z_stat <- as.numeric(roc_result$z)[1]
                }

                if (!is.null(roc_result$p)) {
                    p_val <- as.numeric(roc_result$p)[1]
                }

                # Print out the values we're going to use
                message("Final values for table:")
                message("AUC: ", auc_value)
                message("SE: ", auc_se)
                message("95% CI: [", auc_lci, ", ", auc_uci, "]")
                message("z: ", z_stat)
                message("p: ", p_val)

                # Set row with single values - use NA for missing values
                # But convert NA to "." for display in the table
                summary$setRow(
                    rowNo = 1,
                    values = list(
                        nobs = as.integer(nrow(data)),
                        npos = as.integer(n_pos),
                        nneg = as.integer(n_neg),
                        auc = if (is.na(auc_value))
                            "."
                        else
                            auc_value,
                        auc_se = if (is.na(auc_se))
                            "."
                        else
                            auc_se,
                        auc_lci = if (is.na(auc_lci))
                            "."
                        else
                            auc_lci,
                        auc_uci = if (is.na(auc_uci))
                            "."
                        else
                            auc_uci,
                        z = if (is.na(z_stat))
                            "."
                        else
                            z_stat,
                        p = if (is.na(p_val))
                            "."
                        else
                            p_val
                    )
                )

                # Fill optimal criteria table
                if (optimcrit &&
                    !is.null(private$.optimalCriteria)) {
                    optimal <- self$results$optimal

                    # Instead of clearing rows, we'll check for existing rows
                    # and update them, or add new ones as needed
                    existingRowCount <- optimal$rowCount

                    # Add new rows and update existing ones
                    for (i in 1:nrow(private$.optimalCriteria)) {
                        # If the row already exists, use setRow
                        if (i <= existingRowCount) {
                            optimal$setRow(
                                rowNo = i,
                                values = list(
                                    type = as.character(private$.optimalCriteria$type[i]),
                                    threshold = as.numeric(private$.optimalCriteria$threshold[i]),
                                    sens = as.numeric(private$.optimalCriteria$sens[i]),
                                    spec = as.numeric(private$.optimalCriteria$spec[i]),
                                    ppv = as.numeric(private$.optimalCriteria$ppv[i]),
                                    npv = as.numeric(private$.optimalCriteria$npv[i]),
                                    j = as.numeric(private$.optimalCriteria$j[i])
                                )
                            )
                        } else {
                            # If it's a new row, use addRow
                            optimal$addRow(
                                rowKey = i,
                                values = list(
                                    type = as.character(private$.optimalCriteria$type[i]),
                                    threshold = as.numeric(private$.optimalCriteria$threshold[i]),
                                    sens = as.numeric(private$.optimalCriteria$sens[i]),
                                    spec = as.numeric(private$.optimalCriteria$spec[i]),
                                    ppv = as.numeric(private$.optimalCriteria$ppv[i]),
                                    npv = as.numeric(private$.optimalCriteria$npv[i]),
                                    j = as.numeric(private$.optimalCriteria$j[i])
                                )
                            )
                        }
                    }

                    # If we have more existing rows than criteria, we need to hide them
                    # (unfortunately, we can't delete rows in jamovi, only set them to NA)
                    if (existingRowCount > nrow(private$.optimalCriteria)) {
                        for (i in (nrow(private$.optimalCriteria) + 1):existingRowCount) {
                            optimal$setRow(
                                rowNo = i,
                                values = list(
                                    type = NA,
                                    threshold = NA,
                                    sens = NA,
                                    spec = NA,
                                    ppv = NA,
                                    npv = NA,
                                    j = NA
                                )
                            )
                        }
                    }
                }

                # Fill coordinates table
                if (self$options$coords) {
                    coords_table <- self$results$coords

                    # Check if we have coordinates to display
                    if (nrow(coords) > 0) {
                        # Add a limited number of coordinates to avoid excessive output
                        # Take every nth point to limit to ~20-30 points
                        n_points <- min(30, nrow(coords))
                        step <- max(1, floor(nrow(coords) / n_points))
                        indices <- seq(1, nrow(coords), by = step)

                        # Add the optimal point based on Youden index
                        if (optimcrit &&
                            exists("j_max_idx") && !is.na(j_max_idx)) {
                            indices <- unique(c(indices, j_max_idx))
                            indices <- sort(indices)
                        }

                        # Make sure indices are within bounds
                        indices <- indices[indices <= nrow(coords)]

                        # Add rows safely
                        for (i in indices) {
                            if (i > 0 && i <= nrow(coords)) {
                                # Safe extraction of each value
                                coord_values <- list(
                                    threshold = as.numeric(coords$threshold[i]),
                                    sens = as.numeric(coords$sens[i]),
                                    spec = as.numeric(coords$spec[i]),
                                    ppv = as.numeric(coords$ppv[i]),
                                    npv = as.numeric(coords$npv[i]),
                                    lrp = as.numeric(coords$lrp[i]),
                                    lrn = as.numeric(coords$lrn[i])
                                )

                                # Replace NaN or Inf values with NA
                                coord_values <- lapply(coord_values, function(x) {
                                    if (is.nan(x) || is.infinite(x))
                                        return(NA)
                                    return(x)
                                })

                                # Add the row
                                coords_table$addRow(rowKey = i, values = coord_values)
                            }
                        }
                    }
                }
            },

            # ROC Curve Plot
            .plotRoc = function(image, ...) {
                if (is.null(private$.rocResult) || is.null(private$.rocData))
                    return(FALSE)

                rocobj <- private$.rocResult
                plotci <- self$options$plotci

                # Check if we have a direction recorded
                actual_direction <- if (!is.null(rocobj$actual_direction))
                    rocobj$actual_direction
                else
                    if (self$options$direction == "greatpos")
                        ">"
                else
                    "<"

                # Check if the ROC object is structured for direct plotting
                tryCatch({
                    # Try to plot using pROC's plot function
                    plot(
                        rocobj,
                        auc.polygon = TRUE,
                        grid = TRUE,
                        ci = plotci,
                        legacy.axes = TRUE,
                        # Use traditional 'Sensitivity' and '1 - Specificity' labels
                        xlab = "False Positive Rate (1 - Specificity)",
                        ylab = "True Positive Rate (Sensitivity)",
                        print.auc = TRUE,
                        main = "ROC Curve"
                    )

                    # Add diagonal reference line
                    abline(0, 1, lty = 2, col = "gray")

                    # Add optimal point based on Youden index if available
                    if (!is.null(private$.optimalCriteria)) {
                        optPoint <- private$.optimalCriteria[1, ]  # Youden index
                        if (!is.na(optPoint$sens) &&
                            !is.na(optPoint$spec)) {
                            points(
                                1 - optPoint$spec,
                                optPoint$sens,
                                pch = 19,
                                col = "red"
                            )
                            j_value <- round(optPoint$j, 3)
                            # Don't show negative J values
                            if (!is.na(j_value) &&
                                j_value >= 0) {
                                text(
                                    1 - optPoint$spec,
                                    optPoint$sens,
                                    labels = paste0(" J = ", j_value),
                                    pos = 4,
                                    col = "red"
                                )
                            }
                        }
                    }
                }, error = function(e) {
                    # Fallback to manual plotting if pROC's plot function fails
                    message("Error in pROC plot: ",
                            e$message,
                            ". Using manual ROC plot.")

                    # Extract the ROC coordinates from our data
                    coords <- private$.rocData

                    # Create plot
                    plot(
                        1 - coords$spec,
                        coords$sens,
                        type = "l",
                        xlab = "False Positive Rate (1 - Specificity)",
                        ylab = "True Positive Rate (Sensitivity)",
                        main = "ROC Curve",
                        xlim = c(0, 1),
                        ylim = c(0, 1)
                    )

                    # Add diagonal reference line
                    abline(0, 1, lty = 2, col = "gray")

                    # Add AUC annotation
                    auc_value <- private$.rocResult$auc
                    legend("bottomright",
                           legend = paste("AUC =", round(auc_value, 3)),
                           bty = "n")

                    # Add optimal point based on Youden index if available
                    if (!is.null(private$.optimalCriteria)) {
                        optPoint <- private$.optimalCriteria[1, ]  # Youden index
                        if (!is.na(optPoint$sens) &&
                            !is.na(optPoint$spec)) {
                            points(
                                1 - optPoint$spec,
                                optPoint$sens,
                                pch = 19,
                                col = "red"
                            )
                            j_value <- round(optPoint$j, 3)
                            # Don't show negative J values
                            if (!is.na(j_value) &&
                                j_value >= 0) {
                                text(
                                    1 - optPoint$spec,
                                    optPoint$sens,
                                    labels = paste0(" J = ", j_value),
                                    pos = 4,
                                    col = "red"
                                )
                            }
                        }
                    }

                    # Add confidence band if requested
                    if (plotci &&
                        !is.na(auc_value)) {
                        # Get or calculate CI
                        auc_se <- if (!is.null(rocobj$se))
                            rocobj$se
                        else
                            sqrt((auc_value * (1 - auc_value)) /
                                     (
                                         private$.rocResult$n_pos * private$.rocResult$n_neg
                                     ))
                        z_critical <- qnorm(0.975)

                        # Simple method to draw approximate confidence band
                        # This is a simplified approach and not as accurate as pROC's method
                        # but it gives a visual indication of the confidence interval
                        adjusted_sens <- pmin(1, pmax(0, coords$sens + z_critical * auc_se))
                        adjusted_sens_lower <- pmin(1, pmax(0, coords$sens - z_critical * auc_se))

                        lines(1 - coords$spec,
                              adjusted_sens,
                              lty = 2,
                              col = "blue")
                        lines(
                            1 - coords$spec,
                            adjusted_sens_lower,
                            lty = 2,
                            col = "blue"
                        )
                    }
                })

                return(TRUE)
            },

            # Sensitivity/Specificity vs. Criterion Plot
            .plotBars = function(image, ...) {
                if (is.null(private$.rocData))
                    return(FALSE)

                coords <- private$.rocData

                # Convert to long format for ggplot
                plot_data <- coords %>%
                    dplyr::select(threshold, sens, spec) %>%
                    tidyr::pivot_longer(
                        cols = c("sens", "spec"),
                        names_to = "metric",
                        values_to = "value"
                    ) %>%
                    dplyr::mutate(metric = factor(
                        metric,
                        levels = c("sens", "spec"),
                        labels = c("Sensitivity", "Specificity")
                    ))

                # Plot
                p <- ggplot2::ggplot(plot_data,
                                     ggplot2::aes(x = threshold, y = value, color = metric)) +
                    ggplot2::geom_line(size = 1) +
                    ggplot2::scale_color_manual(values = c(
                        "Sensitivity" = "blue",
                        "Specificity" = "red"
                    )) +
                    ggplot2::labs(
                        title = "Sensitivity and Specificity vs. Criterion Value",
                        x = "Criterion Value",
                        y = "Value",
                        color = "Metric"
                    ) +
                    ggplot2::theme_minimal() +
                    ggplot2::theme(
                        legend.position = "top",
                        plot.title = ggplot2::element_text(hjust = 0.5, face = "bold")
                    )

                # Add vertical line for optimal threshold (Youden Index)
                if (!is.null(private$.optimalCriteria)) {
                    optimal_threshold <- private$.optimalCriteria$threshold[1]  # Youden index
                    p <- p + ggplot2::geom_vline(
                        xintercept = optimal_threshold,
                        linetype = "dashed",
                        color = "darkgreen"
                    )
                }

                print(p)
                return(TRUE)
            },

            # Predictive Values vs. Prevalence Plot
            .plotPrev = function(image, ...) {
                if (is.null(private$.rocData) || is.null(private$.optimalCriteria))
                    return(FALSE)

                # Use Youden's optimal criterion
                optimal <- private$.optimalCriteria[1, ]

                # Create prevalence sequence
                prevalence_seq <- seq(0.01, 0.99, by = 0.01)

                # Calculate PPV and NPV for different prevalence values
                ppv_vals <- (optimal$sens * prevalence_seq) /
                    ((optimal$sens * prevalence_seq) + ((1 - optimal$spec) * (1 - prevalence_seq)))
                npv_vals <- (optimal$spec * (1 - prevalence_seq)) /
                    ((optimal$spec * (1 - prevalence_seq)) + ((1 - optimal$sens) * prevalence_seq))

                # Create data frame for plotting
                plot_data <- data.frame(
                    prevalence = rep(prevalence_seq, 2),
                    value = c(ppv_vals, npv_vals),
                    metric = factor(
                        rep(c("PPV", "NPV"), each = length(prevalence_seq)),
                        levels = c("PPV", "NPV"),
                        labels = c("Positive Predictive Value", "Negative Predictive Value")
                    )
                )

                # Plot
                p <- ggplot2::ggplot(plot_data,
                                     ggplot2::aes(x = prevalence, y = value, color = metric)) +
                    ggplot2::geom_line(size = 1) +
                    ggplot2::scale_color_manual(values = c(
                        "Positive Predictive Value" = "blue",
                        "Negative Predictive Value" = "red"
                    )) +
                    ggplot2::labs(
                        title = "Predictive Values vs. Disease Prevalence",
                        subtitle = paste0(
                            "At Optimal Criterion = ",
                            round(optimal$threshold, 3),
                            " (Sensitivity = ",
                            round(optimal$sens * 100, 1),
                            "%, Specificity = ",
                            round(optimal$spec * 100, 1),
                            "%)"
                        ),
                        x = "Disease Prevalence",
                        y = "Value",
                        color = "Metric"
                    ) +
                    ggplot2::theme_minimal() +
                    ggplot2::theme(
                        legend.position = "top",
                        plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
                        plot.subtitle = ggplot2::element_text(hjust = 0.5)
                    )

                # Add vertical line for sample prevalence if not using custom prevalence
                if (!self$options$pp) {
                    sample_prevalence <- self$results$summary$getRow(1)$get("npos") /
                        self$results$summary$getRow(1)$get("nobs")
                    p <- p + ggplot2::geom_vline(
                        xintercept = sample_prevalence,
                        linetype = "dashed",
                        color = "darkgreen"
                    )
                } else {
                    # Add vertical line for specified prevalence
                    p <- p + ggplot2::geom_vline(
                        xintercept = self$options$pprob,
                        linetype = "dashed",
                        color = "darkgreen"
                    )
                }

                print(p)
                return(TRUE)
            },

            # Interactive Dot Diagram
            .plotIDR = function(image, ...) {
                if (is.null(self$options$classvar) || is.null(self$options$testvar))
                    return(FALSE)

                # Extract data
                data <- self$data
                data <- jmvcore::naOmit(data)

                classvar <- self$options$classvar
                testvar <- self$options$testvar
                classpos <- self$options$classpos

                # Get optimal threshold if available
                threshold <- NULL
                if (!is.null(private$.optimalCriteria)) {
                    threshold <- private$.optimalCriteria$threshold[1]  # Youden index
                }

                # Create binary factor for plotting
                data$Class <- factor(
                    ifelse(data[[classvar]] == classpos, 1, 0),
                    levels = c(0, 1),
                    labels = c("Negative", "Positive")
                )

                # Create plot
                p <- ggplot2::ggplot(data, ggplot2::aes(x = Class, y = .data[[testvar]], color = Class)) +
                    ggplot2::geom_jitter(width = 0.2,
                                         height = 0,
                                         alpha = 0.7) +
                    ggplot2::scale_color_manual(values = c("Negative" = "blue", "Positive" = "red")) +
                    ggplot2::labs(title = "Interactive Dot Diagram", x = "Class", y = testvar) +
                    ggplot2::theme_minimal() +
                    ggplot2::theme(
                        legend.position = "none",
                        plot.title = ggplot2::element_text(hjust = 0.5, face = "bold")
                    )

                # Add horizontal line for optimal threshold
                if (!is.null(threshold)) {
                    # Get direction
                    direction <- self$options$direction

                    # Add threshold line and annotation
                    p <- p + ggplot2::geom_hline(
                        yintercept = threshold,
                        linetype = "dashed",
                        color = "darkgreen"
                    )

                    # Add annotation based on the direction
                    if (direction == "greatpos") {
                        p <- p + ggplot2::annotate(
                            "text",
                            x = 1.5,
                            y = threshold,
                            label = paste0(
                                " Threshold = ",
                                round(threshold, 3),
                                "\n Values >= are classified as Positive"
                            ),
                            hjust = 0,
                            vjust = 0
                        )
                    } else {
                        p <- p + ggplot2::annotate(
                            "text",
                            x = 1.5,
                            y = threshold,
                            label = paste0(
                                " Threshold = ",
                                round(threshold, 3),
                                "\n Values <= are classified as Positive"
                            ),
                            hjust = 0,
                            vjust = 1
                        )
                    }

                    # Get sensitivity and specificity at optimal threshold
                    sens <- private$.optimalCriteria$sens[1]
                    spec <- private$.optimalCriteria$spec[1]

                    # Add subtitle with performance metrics
                    p <- p + ggplot2::labs(subtitle = paste0(
                        "Sensitivity = ",
                        round(sens * 100, 1),
                        "%, Specificity = ",
                        round(spec * 100, 1),
                        "%"
                    ))

                    p <- p + ggplot2::theme(plot.subtitle = ggplot2::element_text(hjust = 0.5))
                }

                print(p)
                return(TRUE)
            }
        )
    )
