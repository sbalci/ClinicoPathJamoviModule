#' @title Decision Curve Analysis
#' @importFrom R6 R6Class
#' @import jmvcore
#' @importFrom ggplot2 ggplot aes geom_line geom_ribbon geom_vline geom_hline
#' @importFrom ggplot2 labs theme_minimal scale_color_brewer annotate xlim ylim
#' @importFrom ggplot2 scale_x_continuous geom_text geom_bar facet_wrap scale_fill_manual
#' @importFrom ggplot2 scale_x_discrete element_text
#' @importFrom dplyr filter mutate group_by summarise arrange
#' @importFrom tidyr gather
#' @importFrom stats quantile complete.cases

decisioncurveClass <- if (requireNamespace("jmvcore")) R6::R6Class(
    "decisioncurveClass",
    inherit = decisioncurveBase,
    private = list(

        # Store analysis results
        .dcaResults = NULL,
        .plotData = NULL,
        .clinicalImpactData = NULL,

        # Calculate net benefit for a model at given threshold
        .calculateNetBenefit = function(predictions, outcomes, threshold, positive_outcome) {
            # Convert outcomes to binary (1 = positive, 0 = negative)
            binary_outcomes <- as.numeric(outcomes == positive_outcome)

            # Calculate predictions at threshold
            predicted_positive <- predictions >= threshold

            # Calculate confusion matrix elements
            tp <- sum(predicted_positive & binary_outcomes == 1)
            fp <- sum(predicted_positive & binary_outcomes == 0)
            tn <- sum(!predicted_positive & binary_outcomes == 0)
            fn <- sum(!predicted_positive & binary_outcomes == 1)

            n <- length(outcomes)
            prevalence <- sum(binary_outcomes) / n

            # Calculate net benefit
            if (tp + fn == 0) {
                sensitivity <- 0
            } else {
                sensitivity <- tp / (tp + fn)
            }

            if (fp + tn == 0) {
                specificity <- 1
            } else {
                specificity <- tn / (fp + tn)
            }

            # Net benefit formula
            nb <- (tp / n) - (fp / n) * (threshold / (1 - threshold))

            return(list(
                net_benefit = nb,
                sensitivity = sensitivity,
                specificity = specificity,
                tp = tp, fp = fp, tn = tn, fn = fn,
                prevalence = prevalence,
                interventions_per_100 = sum(predicted_positive) / n * 100,
                true_positives_per_100 = tp / n * 100,
                false_positives_per_100 = fp / n * 100
            ))
        },

        # Calculate net benefit for treat all strategy
        .calculateTreatAllNetBenefit = function(outcomes, threshold, positive_outcome) {
            binary_outcomes <- as.numeric(outcomes == positive_outcome)
            prevalence <- mean(binary_outcomes)

            # For treat all: sensitivity = 1, specificity = 0
            nb <- prevalence - (1 - prevalence) * (threshold / (1 - threshold))
            return(nb)
        },

        # Calculate net benefit for treat none strategy (always 0)
        .calculateTreatNoneNetBenefit = function() {
            return(0)
        },

        # Generate threshold sequence
        .generateThresholds = function() {
            range_type <- self$options$thresholdRange
            step <- self$options$thresholdStep

            if (range_type == "auto") {
                thresholds <- seq(0.01, 0.99, by = step)
            } else if (range_type == "clinical") {
                thresholds <- seq(0.05, 0.50, by = step)
            } else { # custom
                min_thresh <- self$options$thresholdMin
                max_thresh <- self$options$thresholdMax
                thresholds <- seq(min_thresh, max_thresh, by = step)
            }

            return(thresholds)
        },

        # Parse selected thresholds for table
        .parseSelectedThresholds = function() {
            threshold_str <- self$options$selectedThresholds
            if (threshold_str == "") {
                return(c(0.05, 0.10, 0.15, 0.20, 0.25, 0.30))
            }

            # Parse comma-separated values
            thresholds <- as.numeric(unlist(strsplit(threshold_str, "[,;\\s]+")))
            thresholds <- thresholds[!is.na(thresholds)]
            thresholds <- thresholds[thresholds > 0 & thresholds < 1]

            if (length(thresholds) == 0) {
                return(c(0.05, 0.10, 0.15, 0.20, 0.25, 0.30))
            }

            return(sort(thresholds))
        },

        # Parse model names
        .parseModelNames = function() {
            model_names_str <- self$options$modelNames
            model_vars <- self$options$models

            if (model_names_str == "" || is.null(model_names_str)) {
                return(model_vars)
            }

            # Parse comma-separated names
            names <- trimws(unlist(strsplit(model_names_str, ",")))

            # If number of names doesn't match variables, use variable names
            if (length(names) != length(model_vars)) {
                return(model_vars)
            }

            return(names)
        },

        # Bootstrap confidence intervals
        .calculateBootstrapCI = function(predictions, outcomes, thresholds, positive_outcome, n_boot = 1000) {
            n <- length(outcomes)
            boot_results <- array(NA, dim = c(n_boot, length(thresholds)))

            for (i in 1:n_boot) {
                # Bootstrap sample
                boot_idx <- sample(n, n, replace = TRUE)
                boot_pred <- predictions[boot_idx]
                boot_out <- outcomes[boot_idx]

                # Calculate net benefits for this bootstrap sample
                for (j in seq_along(thresholds)) {
                    thresh <- thresholds[j]
                    nb_result <- private$.calculateNetBenefit(
                        boot_pred, boot_out, thresh, positive_outcome
                    )
                    boot_results[i, j] <- nb_result$net_benefit
                }
            }

            # Calculate confidence intervals
            ci_lower <- apply(boot_results, 2, quantile, probs = (1 - self$options$ciLevel) / 2, na.rm = TRUE)
            ci_upper <- apply(boot_results, 2, quantile, probs = 1 - (1 - self$options$ciLevel) / 2, na.rm = TRUE)

            return(list(lower = ci_lower, upper = ci_upper))
        },

        # Find optimal threshold for a model
        .findOptimalThreshold = function(net_benefits, thresholds) {
            # Find threshold with maximum net benefit
            max_idx <- which.max(net_benefits)
            optimal_threshold <- thresholds[max_idx]
            max_net_benefit <- net_benefits[max_idx]

            # Find range where model is beneficial (net benefit > 0)
            beneficial <- net_benefits > 0
            if (any(beneficial)) {
                beneficial_thresholds <- thresholds[beneficial]
                range_start <- min(beneficial_thresholds)
                range_end <- max(beneficial_thresholds)
            } else {
                range_start <- NA
                range_end <- NA
            }

            return(list(
                optimal_threshold = optimal_threshold,
                max_net_benefit = max_net_benefit,
                range_start = range_start,
                range_end = range_end
            ))
        },

        # Calculate weighted AUC
        .calculateWeightedAUC = function(net_benefits, thresholds) {
            # Remove any missing values
            valid_idx <- !is.na(net_benefits) & !is.na(thresholds)
            nb_clean <- net_benefits[valid_idx]
            th_clean <- thresholds[valid_idx]

            if (length(nb_clean) < 2) {
                return(NA)
            }

            # Calculate AUC using trapezoidal rule
            # Sort by threshold
            ord <- order(th_clean)
            nb_sorted <- nb_clean[ord]
            th_sorted <- th_clean[ord]

            # Trapezoidal integration
            auc <- 0
            for (i in 2:length(th_sorted)) {
                width <- th_sorted[i] - th_sorted[i-1]
                height <- (nb_sorted[i] + nb_sorted[i-1]) / 2
                auc <- auc + width * height
            }

            # Normalize by range
            total_range <- max(th_sorted) - min(th_sorted)
            return(auc / total_range)
        },

        # Main analysis function
        .run = function() {

            # Show instructions if needed
            if (is.null(self$options$outcome) || is.null(self$options$models) ||
                length(self$options$models) == 0) {

                instructions <- "
                <html>
                <head></head>
                <body>
                <div class='instructions'>
                <p><b>Decision Curve Analysis</b></p>
                <p>Decision Curve Analysis evaluates the clinical utility of prediction models by calculating net benefit across different threshold probabilities.</p>
                <p>To get started:</p>
                <ol>
                <li>Select a binary <b>Outcome Variable</b> (the condition you want to predict)</li>
                <li>Specify which level represents the positive outcome</li>
                <li>Add one or more <b>Prediction Variables/Models</b> (predicted probabilities or risk scores)</li>
                <li>Configure the threshold range and other analysis options</li>
                </ol>
                <p>The analysis will show whether using your prediction model(s) provides more clinical benefit than treating all patients or treating no patients.</p>
                </div>
                </body>
                </html>
                "

                self$results$instructions$setContent(instructions)
                return()
            }

            # Hide instructions when analysis can proceed
            self$results$instructions$setVisible(FALSE)

            # Get data and variables
            data <- self$data
            outcome_var <- self$options$outcome
            outcome_positive <- self$options$outcomePositive
            model_vars <- self$options$models

            # Parse model names
            model_names <- private$.parseModelNames()

            # Get complete cases
            complete_vars <- c(outcome_var, model_vars)
            complete_cases <- complete.cases(data[complete_vars])

            if (sum(complete_cases) < 10) {
                stop("Insufficient complete cases for analysis (minimum 10 required)")
            }

            # Filter data to complete cases
            analysis_data <- data[complete_cases, ]
            outcomes <- analysis_data[[outcome_var]]

            # Check outcome is binary
            unique_outcomes <- unique(outcomes)
            if (length(unique_outcomes) != 2) {
                stop("Outcome variable must be binary (exactly 2 levels)")
            }

            # Validate positive outcome level
            if (!outcome_positive %in% unique_outcomes) {
                outcome_positive <- unique_outcomes[1]
                warning("Selected positive outcome level not found. Using first level.")
            }

            # Generate threshold sequence
            thresholds <- private$.generateThresholds()

            # Initialize results storage
            dca_results <- list()
            plot_data <- data.frame()

            # Calculate decision curves for each model
            for (i in seq_along(model_vars)) {
                model_var <- model_vars[i]
                model_name <- model_names[i]
                predictions <- analysis_data[[model_var]]

                # Validate predictions are between 0 and 1 (or convert if needed)
                if (min(predictions, na.rm = TRUE) < 0 || max(predictions, na.rm = TRUE) > 1) {
                    # If not probabilities, assume they are risk scores and need conversion
                    # Simple normalization to 0-1 range
                    pred_range <- range(predictions, na.rm = TRUE)
                    if (pred_range[1] != pred_range[2]) {
                        predictions <- (predictions - pred_range[1]) / (pred_range[2] - pred_range[1])
                    }
                }

                # Calculate net benefits across thresholds
                net_benefits <- numeric(length(thresholds))
                detailed_results <- list()

                for (j in seq_along(thresholds)) {
                    thresh <- thresholds[j]
                    nb_result <- private$.calculateNetBenefit(
                        predictions, outcomes, thresh, outcome_positive
                    )
                    net_benefits[j] <- nb_result$net_benefit
                    detailed_results[[j]] <- nb_result
                }

                # Store results
                dca_results[[model_name]] <- list(
                    net_benefits = net_benefits,
                    detailed_results = detailed_results,
                    thresholds = thresholds
                )

                # Add to plot data
                model_plot_data <- data.frame(
                    threshold = thresholds,
                    net_benefit = net_benefits,
                    model = model_name,
                    stringsAsFactors = FALSE
                )

                # Add confidence intervals if requested
                if (self$options$confidenceIntervals) {
                    ci_results <- private$.calculateBootstrapCI(
                        predictions, outcomes, thresholds, outcome_positive,
                        self$options$bootReps
                    )
                    model_plot_data$ci_lower <- ci_results$lower
                    model_plot_data$ci_upper <- ci_results$upper
                }

                plot_data <- rbind(plot_data, model_plot_data)
            }

            # Calculate net benefit for treat all strategy
            treat_all_nb <- numeric(length(thresholds))
            treat_none_nb <- numeric(length(thresholds))

            for (j in seq_along(thresholds)) {
                treat_all_nb[j] <- private$.calculateTreatAllNetBenefit(
                    outcomes, thresholds[j], outcome_positive
                )
                treat_none_nb[j] <- private$.calculateTreatNoneNetBenefit()
            }

            # Add reference strategies to plot data
            ref_data <- rbind(
                data.frame(
                    threshold = thresholds,
                    net_benefit = treat_all_nb,
                    model = "Treat All",
                    stringsAsFactors = FALSE
                ),
                data.frame(
                    threshold = thresholds,
                    net_benefit = treat_none_nb,
                    model = "Treat None",
                    stringsAsFactors = FALSE
                )
            )

            plot_data <- rbind(plot_data, ref_data)

            # Store results for plotting
            private$.dcaResults <- dca_results
            private$.plotData <- plot_data

            # Create procedure notes
            procedure_notes <- paste0(
                "<html><body>",
                "<h4>Decision Curve Analysis Summary</h4>",
                "<p><strong>Outcome Variable:</strong> ", outcome_var, " (", outcome_positive, " = positive)</p>",
                "<p><strong>Models Analyzed:</strong> ", paste(model_names, collapse = ", "), "</p>",
                "<p><strong>Sample Size:</strong> ", sum(complete_cases), " complete cases</p>",
                "<p><strong>Prevalence:</strong> ", round(mean(outcomes == outcome_positive) * 100, 1), "%</p>",
                "<p><strong>Threshold Range:</strong> ", round(min(thresholds) * 100, 1), "% to ",
                round(max(thresholds) * 100, 1), "%</p>",
                "</body></html>"
            )

            self$results$procedureNotes$setContent(procedure_notes)

            # Populate results table
            if (self$options$showTable) {
                private$.populateResultsTable(treat_all_nb, treat_none_nb)
            }

            # Populate optimal thresholds table
            if (self$options$showOptimalThreshold) {
                private$.populateOptimalTable()
            }

            # Calculate clinical impact if requested
            if (self$options$calculateClinicalImpact) {
                private$.calculateClinicalImpactMetrics(outcomes, outcome_positive)
            }

            # Calculate weighted AUC if requested
            if (self$options$weightedAUC) {
                private$.populateWeightedAUCTable()
            }

            # Model comparison if requested
            if (self$options$compareModels && length(model_vars) > 1) {
                private$.performModelComparison()
            }

            # Generate clinical interpretation
            private$.generateClinicalInterpretation()
        },

        .populateResultsTable = function(treat_all_nb, treat_none_nb) {
            selected_thresholds <- private$.parseSelectedThresholds()
            results_table <- self$results$resultsTable

            # Clear existing rows
            results_table$deleteRows()

            # Add columns for each model dynamically
            model_names <- names(private$.dcaResults)

            for (model_name in model_names) {
                results_table$addColumn(
                    name = paste0("model_", gsub("[^A-Za-z0-9]", "_", model_name)),
                    title = model_name,
                    type = "number",
                    format = "zto"
                )
            }

            # Populate table
            for (i in seq_along(selected_thresholds)) {
                thresh <- selected_thresholds[i]

                # Find closest threshold in our analysis
                closest_idx <- which.min(abs(private$.dcaResults[[1]]$thresholds - thresh))
                actual_thresh <- private$.dcaResults[[1]]$thresholds[closest_idx]

                # Create row values
                row_values <- list(
                    threshold = thresh,
                    treat_all = treat_all_nb[closest_idx],
                    treat_none = treat_none_nb[closest_idx]
                )

                # Add model values
                for (model_name in model_names) {
                    col_name <- paste0("model_", gsub("[^A-Za-z0-9]", "_", model_name))
                    row_values[[col_name]] <- private$.dcaResults[[model_name]]$net_benefits[closest_idx]
                }

                results_table$addRow(rowKey = i, values = row_values)
            }
        },

        .populateOptimalTable = function() {
            optimal_table <- self$results$optimalTable
            optimal_table$deleteRows()

            model_names <- names(private$.dcaResults)

            for (i in seq_along(model_names)) {
                model_name <- model_names[i]
                model_results <- private$.dcaResults[[model_name]]

                optimal_info <- private$.findOptimalThreshold(
                    model_results$net_benefits,
                    model_results$thresholds
                )

                optimal_table$addRow(rowKey = i, values = list(
                    model = model_name,
                    optimal_threshold = optimal_info$optimal_threshold,
                    max_net_benefit = optimal_info$max_net_benefit,
                    threshold_range_start = optimal_info$range_start,
                    threshold_range_end = optimal_info$range_end
                ))
            }
        },

        .calculateClinicalImpactMetrics = function(outcomes, outcome_positive) {
            clinical_impact_table <- self$results$clinicalImpactTable
            clinical_impact_table$deleteRows()

            selected_thresholds <- private$.parseSelectedThresholds()
            model_names <- names(private$.dcaResults)
            pop_size <- self$options$populationSize

            # Calculate for each model at each selected threshold
            row_counter <- 1
            for (model_name in model_names) {
                model_results <- private$.dcaResults[[model_name]]

                for (thresh in selected_thresholds) {
                    # Find closest threshold
                    closest_idx <- which.min(abs(model_results$thresholds - thresh))
                    detailed_result <- model_results$detailed_results[[closest_idx]]

                    # Calculate interventions avoided compared to treat all
                    treat_all_interventions <- pop_size  # Treat all = 100% get intervention
                    model_interventions <- detailed_result$interventions_per_100 * pop_size / 100
                    interventions_avoided <- treat_all_interventions - model_interventions

                    # Number needed to screen (simplified calculation)
                    if (detailed_result$true_positives_per_100 > 0) {
                        nns <- 100 / detailed_result$true_positives_per_100
                    } else {
                        nns <- Inf
                    }

                    clinical_impact_table$addRow(rowKey = row_counter, values = list(
                        model = model_name,
                        threshold = thresh,
                        interventions_per_100 = detailed_result$interventions_per_100,
                        true_positives_per_100 = detailed_result$true_positives_per_100,
                        false_positives_per_100 = detailed_result$false_positives_per_100,
                        interventions_avoided = interventions_avoided,
                        number_needed_to_screen = if(is.finite(nns)) nns else NA
                    ))

                    row_counter <- row_counter + 1
                }
            }
        },

        .populateWeightedAUCTable = function() {
            weighted_auc_table <- self$results$weightedAUCTable
            weighted_auc_table$deleteRows()

            model_names <- names(private$.dcaResults)
            thresholds <- private$.dcaResults[[1]]$thresholds

            # Calculate treat all weighted AUC for comparison
            outcomes <- self$data[[self$options$outcome]]
            outcome_positive <- self$options$outcomePositive
            treat_all_nb <- numeric(length(thresholds))
            for (j in seq_along(thresholds)) {
                treat_all_nb[j] <- private$.calculateTreatAllNetBenefit(
                    outcomes, thresholds[j], outcome_positive
                )
            }
            treat_all_wauc <- private$.calculateWeightedAUC(treat_all_nb, thresholds)

            for (i in seq_along(model_names)) {
                model_name <- model_names[i]
                model_results <- private$.dcaResults[[model_name]]

                # Calculate weighted AUC
                wauc <- private$.calculateWeightedAUC(
                    model_results$net_benefits,
                    model_results$thresholds
                )

                # Calculate relative benefit vs treat all
                if (!is.na(wauc) && !is.na(treat_all_wauc) && treat_all_wauc != 0) {
                    relative_benefit <- (wauc - treat_all_wauc) / abs(treat_all_wauc)
                } else {
                    relative_benefit <- NA
                }

                weighted_auc_table$addRow(rowKey = i, values = list(
                    model = model_name,
                    weighted_auc = wauc,
                    auc_range = paste0(round(min(thresholds) * 100, 1), "% - ",
                                       round(max(thresholds) * 100, 1), "%"),
                    relative_benefit = relative_benefit
                ))
            }
        },

        .performModelComparison = function() {
            # This would implement statistical tests for comparing models
            # For now, implement a simple comparison based on weighted AUC
            comparison_table <- self$results$comparisonTable
            comparison_table$deleteRows()

            model_names <- names(private$.dcaResults)

            # Compare each pair of models
            row_counter <- 1
            for (i in 1:(length(model_names) - 1)) {
                for (j in (i + 1):length(model_names)) {
                    model1 <- model_names[i]
                    model2 <- model_names[j]

                    # Calculate weighted AUC difference
                    wauc1 <- private$.calculateWeightedAUC(
                        private$.dcaResults[[model1]]$net_benefits,
                        private$.dcaResults[[model1]]$thresholds
                    )
                    wauc2 <- private$.calculateWeightedAUC(
                        private$.dcaResults[[model2]]$net_benefits,
                        private$.dcaResults[[model2]]$thresholds
                    )

                    wauc_diff <- wauc1 - wauc2

                    # For now, set placeholder values for CI and p-value
                    # In a full implementation, these would come from bootstrap testing
                    comparison_table$addRow(rowKey = row_counter, values = list(
                        comparison = paste(model1, "vs", model2),
                        weighted_auc_diff = wauc_diff,
                        ci_lower = NA,  # Would implement bootstrap CI
                        ci_upper = NA,  # Would implement bootstrap CI
                        p_value = NA    # Would implement statistical test
                    ))

                    row_counter <- row_counter + 1
                }
            }
        },

        .generateClinicalInterpretation = function() {
            model_names <- names(private$.dcaResults)

            # Find the best performing model (highest weighted AUC)
            best_wauc <- -Inf
            best_model <- NULL

            for (model_name in model_names) {
                wauc <- private$.calculateWeightedAUC(
                    private$.dcaResults[[model_name]]$net_benefits,
                    private$.dcaResults[[model_name]]$thresholds
                )
                if (!is.na(wauc) && wauc > best_wauc) {
                    best_wauc <- wauc
                    best_model <- model_name
                }
            }

            # Generate interpretation text
            interpretation <- paste0(
                "<html><body>",
                "<h4>Clinical Interpretation</h4>"
            )

            if (!is.null(best_model)) {
                interpretation <- paste0(
                    interpretation,
                    "<p><strong>Best Performing Model:</strong> ", best_model, "</p>"
                )

                # Get optimal threshold for best model
                best_results <- private$.dcaResults[[best_model]]
                optimal_info <- private$.findOptimalThreshold(
                    best_results$net_benefits,
                    best_results$thresholds
                )

                if (!is.na(optimal_info$optimal_threshold)) {
                    interpretation <- paste0(
                        interpretation,
                        "<p><strong>Optimal Threshold:</strong> ",
                        round(optimal_info$optimal_threshold * 100, 1),
                        "% (Net Benefit = ", round(optimal_info$max_net_benefit, 3), ")</p>"
                    )
                }

                if (!is.na(optimal_info$range_start) && !is.na(optimal_info$range_end)) {
                    interpretation <- paste0(
                        interpretation,
                        "<p><strong>Beneficial Range:</strong> ",
                        round(optimal_info$range_start * 100, 1), "% to ",
                        round(optimal_info$range_end * 100, 1), "%</p>"
                    )
                }
            }

            interpretation <- paste0(
                interpretation,
                "<p><strong>Interpretation Guidelines:</strong></p>",
                "<ul>",
                "<li>Models above both reference lines provide clinical benefit</li>",
                "<li>Higher net benefit indicates greater clinical utility</li>",
                "<li>Consider the threshold range relevant to your clinical context</li>",
                "<li>Net benefit can be interpreted as additional true positives per 100 patients screened</li>",
                "</ul>",
                "</body></html>"
            )

            self$results$summaryText$setContent(interpretation)
        },

        # Plotting functions
        .plotDCA = function(image, ggtheme, theme, ...) {
            if (is.null(private$.plotData) || nrow(private$.plotData) == 0) {
                return(FALSE)
            }

            plot_data <- private$.plotData

            # Create base plot
            p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = threshold, y = net_benefit, color = model)) +
                ggplot2::geom_line(size = 1) +
                ggplot2::labs(
                    title = "Decision Curve Analysis",
                    x = "Threshold Probability",
                    y = "Net Benefit",
                    color = "Strategy"
                ) +
                ggplot2::scale_x_continuous(labels = function(x) paste0(round(x * 100), "%")) +
                ggtheme

            # Add confidence intervals if calculated
            if ("ci_lower" %in% names(plot_data)) {
                model_data <- plot_data[!plot_data$model %in% c("Treat All", "Treat None"), ]
                if (nrow(model_data) > 0) {
                    p <- p + ggplot2::geom_ribbon(
                        data = model_data,
                        ggplot2::aes(ymin = ci_lower, ymax = ci_upper, fill = model),
                        alpha = 0.2, color = NA
                    )
                }
            }

            # Highlight clinical range if requested
            if (self$options$highlightRange) {
                p <- p + ggplot2::annotate(
                    "rect",
                    xmin = self$options$highlightMin,
                    xmax = self$options$highlightMax,
                    ymin = -Inf, ymax = Inf,
                    alpha = 0.1, fill = "yellow"
                )
            }

            # Style reference lines differently
            if (self$options$plotStyle == "standard" || self$options$plotStyle == "detailed") {
                # Make treat all/none lines dashed
                treat_lines <- plot_data[plot_data$model %in% c("Treat All", "Treat None"), ]
                if (nrow(treat_lines) > 0) {
                    p <- p + ggplot2::geom_line(
                        data = treat_lines,
                        linetype = "dashed", size = 0.8
                    )
                }
            }

            # Add annotations for detailed style
            if (self$options$plotStyle == "detailed") {
                # Add horizontal line at 0
                p <- p + ggplot2::geom_hline(yintercept = 0, linetype = "dotted", alpha = 0.5)

                # Add labels if requested
                if (self$options$showReferenceLinesLabels) {
                    # This would add text annotations for reference lines
                }
            }

            print(p)
            return(TRUE)
        },

        .plotClinicalImpact = function(image, ggtheme, theme, ...) {
            if (is.null(private$.dcaResults) || !self$options$calculateClinicalImpact) {
                return(FALSE)
            }

            # Get selected thresholds and models
            selected_thresholds <- private$.parseSelectedThresholds()
            model_names <- names(private$.dcaResults)
            pop_size <- self$options$populationSize

            # Prepare data for clinical impact plot
            impact_data <- data.frame()

            for (model_name in model_names) {
                model_results <- private$.dcaResults[[model_name]]

                for (thresh in selected_thresholds) {
                    # Find closest threshold
                    closest_idx <- which.min(abs(model_results$thresholds - thresh))
                    detailed_result <- model_results$detailed_results[[closest_idx]]

                    # Add to plot data
                    impact_data <- rbind(impact_data, data.frame(
                        threshold = thresh,
                        model = model_name,
                        interventions_per_100 = detailed_result$interventions_per_100,
                        true_positives_per_100 = detailed_result$true_positives_per_100,
                        false_positives_per_100 = detailed_result$false_positives_per_100,
                        stringsAsFactors = FALSE
                    ))
                }
            }

            if (nrow(impact_data) == 0) return(FALSE)

            # Reshape data for stacked bar chart
            library(tidyr)
            plot_data <- impact_data %>%
                tidyr::gather(key = "outcome_type", value = "count",
                              true_positives_per_100, false_positives_per_100) %>%
                dplyr::mutate(
                    outcome_type = factor(outcome_type,
                                          levels = c("true_positives_per_100", "false_positives_per_100"),
                                          labels = c("True Positives", "False Positives"))
                )

            # Create stacked bar chart showing clinical impact
            p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = factor(threshold), y = count, fill = outcome_type)) +
                ggplot2::geom_bar(stat = "identity", position = "stack") +
                ggplot2::facet_wrap(~ model, scales = "free_y") +
                ggplot2::labs(
                    title = "Clinical Impact: Interventions per 100 Patients",
                    x = "Threshold Probability",
                    y = "Patients per 100",
                    fill = "Outcome Type"
                ) +
                ggplot2::scale_x_discrete(labels = function(x) paste0(as.numeric(x) * 100, "%")) +
                ggplot2::scale_fill_manual(values = c("True Positives" = "darkgreen", "False Positives" = "darkred")) +
                ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
                ggtheme

            print(p)
            return(TRUE)
        },

        .plotInterventionsAvoided = function(image, ggtheme, theme, ...) {
            if (is.null(private$.dcaResults)) {
                return(FALSE)
            }

            # Calculate interventions avoided compared to "treat all" strategy
            thresholds <- private$.dcaResults[[1]]$thresholds
            model_names <- names(private$.dcaResults)

            # Prepare data
            avoided_data <- data.frame()

            for (model_name in model_names) {
                model_results <- private$.dcaResults[[model_name]]
                interventions_avoided <- numeric(length(thresholds))

                for (j in seq_along(thresholds)) {
                    detailed_result <- model_results$detailed_results[[j]]
                    # Treat all = 100% get intervention, model = actual intervention rate
                    interventions_avoided[j] <- 100 - detailed_result$interventions_per_100
                }

                avoided_data <- rbind(avoided_data, data.frame(
                    threshold = thresholds,
                    interventions_avoided = interventions_avoided,
                    model = model_name,
                    stringsAsFactors = FALSE
                ))
            }

            if (nrow(avoided_data) == 0) return(FALSE)

            # Create line plot showing interventions avoided
            p <- ggplot2::ggplot(avoided_data, ggplot2::aes(x = threshold, y = interventions_avoided, color = model)) +
                ggplot2::geom_line(size = 1) +
                ggplot2::geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
                ggplot2::labs(
                    title = "Interventions Avoided vs. Treat All Strategy",
                    subtitle = "Number of unnecessary interventions prevented per 100 patients",
                    x = "Threshold Probability",
                    y = "Interventions Avoided per 100 Patients",
                    color = "Model"
                ) +
                ggplot2::scale_x_continuous(labels = function(x) paste0(round(x * 100), "%")) +
                ggplot2::ylim(0, 100) +
                ggtheme

            # Add annotation explaining the interpretation
            p <- p + ggplot2::annotate(
                "text",
                x = max(thresholds) * 0.7,
                y = max(avoided_data$interventions_avoided, na.rm = TRUE) * 0.9,
                label = "Higher values = more\nunnecessary treatments avoided",
                hjust = 0.5,
                alpha = 0.7,
                size = 3
            )

            print(p)
            return(TRUE)
        }
    )
)
