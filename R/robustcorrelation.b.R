#' @title Robust Correlation Methods
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import glue
#'

robustcorrelationClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "robustcorrelationClass",
    inherit = robustcorrelationBase,
    private = list(

        # Cache for processed data and results
        .processedData = NULL,
        .correlationResults = NULL,
        .outlierResults = NULL,
        .bootstrapResults = NULL,

        # Initialize the analysis
        .init = function() {

            # Set plot dimensions
            plot_width <- if (!is.null(self$options$plot_width)) self$options$plot_width else 600
            plot_height <- if (!is.null(self$options$plot_height)) self$options$plot_height else 450

            self$results$correlation_heatmap$setSize(plot_width, plot_height)
            self$results$outlier_plot$setSize(plot_width, plot_height)
            self$results$diagnostic_plots$setSize(plot_width + 200, plot_height + 150)
            self$results$scatterplot_matrix$setSize(800, 800)

            # Initialize correlation table
            private$.initCorrelationTable()

            # Initialize bootstrap table if needed
            if (self$options$bootstrap_ci) {
                private$.initBootstrapTable()
            }

            # Initialize outlier table if needed
            if (self$options$outlier_detection) {
                private$.initOutlierTable()
            }
        },

        # Initialize correlation table columns
        .initCorrelationTable = function() {
            table <- self$results$correlation_table

            if (length(self$options$dep) < 2) return()

            vars <- self$options$dep
            n_vars <- length(vars)

            # Create columns for correlation table
            columns <- list()

            # Variable 1 column
            columns[["var1"]] <- list(
                name = "var1",
                title = "Variable 1",
                type = "text"
            )

            # Variable 2 column
            columns[["var2"]] <- list(
                name = "var2",
                title = "Variable 2",
                type = "text"
            )

            # Correlation coefficient
            columns[["correlation"]] <- list(
                name = "correlation",
                title = "Correlation",
                type = "number",
                format = paste0("zto,pvalue,dp:", self$options$decimal_places)
            )

            # P-value column if requested
            if (self$options$show_pvalues) {
                columns[["pvalue"]] <- list(
                    name = "pvalue",
                    title = "p-value",
                    type = "number",
                    format = "zto,pvalue"
                )

                # Adjusted p-value if correction applied
                if (self$options$p_adjust_method != "none") {
                    columns[["pvalue_adj"]] <- list(
                        name = "pvalue_adj",
                        title = "Adjusted p-value",
                        type = "number",
                        format = "zto,pvalue"
                    )
                }
            }

            # Sample size
            columns[["n"]] <- list(
                name = "n",
                title = "N",
                type = "integer"
            )

            table$set(columns = columns)
        },

        # Initialize bootstrap confidence interval table
        .initBootstrapTable = function() {
            table <- self$results$bootstrap_table

            if (length(self$options$dep) < 2) return()

            columns <- list(
                "var1" = list(name = "var1", title = "Variable 1", type = "text"),
                "var2" = list(name = "var2", title = "Variable 2", type = "text"),
                "correlation" = list(name = "correlation", title = "Correlation", type = "number", format = paste0("zto,pvalue,dp:", self$options$decimal_places)),
                "ci_lower" = list(name = "ci_lower", title = "CI Lower", type = "number", format = paste0("zto,pvalue,dp:", self$options$decimal_places)),
                "ci_upper" = list(name = "ci_upper", title = "CI Upper", type = "number", format = paste0("zto,pvalue,dp:", self$options$decimal_places)),
                "bias" = list(name = "bias", title = "Bootstrap Bias", type = "number", format = paste0("zto,pvalue,dp:", self$options$decimal_places)),
                "se" = list(name = "se", title = "Bootstrap SE", type = "number", format = paste0("zto,pvalue,dp:", self$options$decimal_places))
            )

            table$set(columns = columns)
        },

        # Initialize outlier detection table
        .initOutlierTable = function() {
            table <- self$results$outlier_table

            columns <- list(
                "observation" = list(name = "observation", title = "Observation", type = "integer"),
                "distance" = list(name = "distance", title = "Distance/Score", type = "number", format = "zto,pvalue,dp:3"),
                "threshold" = list(name = "threshold", title = "Threshold", type = "number", format = "zto,pvalue,dp:3"),
                "outlier" = list(name = "outlier", title = "Outlier", type = "text"),
                "variables" = list(name = "variables", title = "Extreme Variables", type = "text")
            )

            table$set(columns = columns)
        },

        # Prepare and clean data
        .prepareData = function() {
            if (!is.null(private$.processedData)) {
                return(private$.processedData)
            }

            # Get data and selected variables
            data <- self$data
            vars <- self$options$dep

            if (length(vars) < 2) {
                return(NULL)
            }

            # Extract and clean data
            data_subset <- data[vars]

            # Convert to numeric and remove missing values
            for (var in vars) {
                data_subset[[var]] <- jmvcore::toNumeric(data_subset[[var]])
            }

            # Remove rows with any missing values
            data_clean <- jmvcore::naOmit(data_subset)

            # Check for sufficient data
            if (nrow(data_clean) < 3) {
                stop("Insufficient data for correlation analysis. Need at least 3 complete observations.")
            }

            # Check for sufficient variation in each variable
            for (var in vars) {
                if (length(unique(data_clean[[var]])) < 2) {
                    stop(paste("Variable", var, "has insufficient variation for correlation analysis."))
                }
            }

            private$.processedData <- data_clean
            return(data_clean)
        },

        # Compute correlations using specified method
        .computeCorrelations = function(data) {
            if (!is.null(private$.correlationResults)) {
                return(private$.correlationResults)
            }

            method <- self$options$method
            vars <- self$options$dep
            n_vars <- length(vars)

            # Initialize results storage
            results <- list()
            results$correlations <- matrix(NA, nrow = n_vars, ncol = n_vars, dimnames = list(vars, vars))
            results$pvalues <- matrix(NA, nrow = n_vars, ncol = n_vars, dimnames = list(vars, vars))
            results$n <- matrix(NA, nrow = n_vars, ncol = n_vars, dimnames = list(vars, vars))

            # Compute correlations based on method
            for (i in 1:n_vars) {
                for (j in i:n_vars) {
                    if (i == j) {
                        results$correlations[i, j] <- 1.0
                        results$pvalues[i, j] <- NA
                        results$n[i, j] <- nrow(data)
                    } else {
                        x <- data[[vars[i]]]
                        y <- data[[vars[j]]]

                        # Remove any remaining NAs
                        complete_cases <- complete.cases(x, y)
                        x_clean <- x[complete_cases]
                        y_clean <- y[complete_cases]
                        n_obs <- length(x_clean)

                        if (n_obs >= 3) {
                            cor_result <- private$.computePairwiseCorrelation(x_clean, y_clean, method)
                            results$correlations[i, j] <- cor_result$estimate
                            results$correlations[j, i] <- cor_result$estimate
                            results$pvalues[i, j] <- cor_result$pvalue
                            results$pvalues[j, i] <- cor_result$pvalue
                            results$n[i, j] <- n_obs
                            results$n[j, i] <- n_obs
                        }
                    }
                }
            }

            # Apply multiple testing correction if requested
            if (self$options$p_adjust_method != "none" && self$options$show_pvalues) {
                p_vals <- results$pvalues[upper.tri(results$pvalues)]
                p_vals_adj <- p.adjust(p_vals, method = self$options$p_adjust_method)
                results$pvalues_adj <- results$pvalues
                results$pvalues_adj[upper.tri(results$pvalues_adj)] <- p_vals_adj
                results$pvalues_adj[lower.tri(results$pvalues_adj)] <- t(results$pvalues_adj)[lower.tri(results$pvalues_adj)]
            }

            private$.correlationResults <- results
            return(results)
        },

        # Compute individual pairwise correlation
        .computePairwiseCorrelation = function(x, y, method) {
            result <- list(estimate = NA, pvalue = NA)

            tryCatch({
                if (method == "pearson") {
                    test_result <- cor.test(x, y, method = "pearson")
                    result$estimate <- test_result$estimate
                    result$pvalue <- test_result$p.value
                } else if (method == "spearman") {
                    test_result <- cor.test(x, y, method = "spearman")
                    result$estimate <- test_result$estimate
                    result$pvalue <- test_result$p.value
                } else if (method == "kendall") {
                    test_result <- cor.test(x, y, method = "kendall")
                    result$estimate <- test_result$estimate
                    result$pvalue <- test_result$p.value
                } else if (method == "percentage_bend") {
                    if (requireNamespace("WRS2", quietly = TRUE)) {
                        pb_result <- WRS2::pbcor(x, y, beta = 0.1)
                        result$estimate <- pb_result$cor
                        result$pvalue <- pb_result$p.value
                    } else {
                        # Fallback to Spearman if WRS2 not available
                        test_result <- cor.test(x, y, method = "spearman")
                        result$estimate <- test_result$estimate
                        result$pvalue <- test_result$p.value
                    }
                } else if (method == "biweight") {
                    if (requireNamespace("WGCNA", quietly = TRUE)) {
                        bw_cor <- WGCNA::bicor(x, y, use = "complete.obs")
                        result$estimate <- bw_cor
                        # Approximate p-value using Fisher's z-transform
                        n <- length(x)
                        z <- 0.5 * log((1 + bw_cor) / (1 - bw_cor))
                        se <- 1 / sqrt(n - 3)
                        result$pvalue <- 2 * (1 - pnorm(abs(z) / se))
                    } else {
                        # Fallback to Spearman
                        test_result <- cor.test(x, y, method = "spearman")
                        result$estimate <- test_result$estimate
                        result$pvalue <- test_result$p.value
                    }
                } else if (method %in% c("mve", "mcd")) {
                    if (requireNamespace("robustbase", quietly = TRUE)) {
                        data_mat <- cbind(x, y)
                        if (method == "mve") {
                            cov_robust <- robustbase::covMve(data_mat)
                        } else {
                            cov_robust <- robustbase::covMcd(data_mat)
                        }
                        cor_mat <- cov2cor(cov_robust$cov)
                        result$estimate <- cor_mat[1, 2]
                        # Approximate p-value
                        n <- nrow(data_mat)
                        z <- 0.5 * log((1 + cor_mat[1, 2]) / (1 - cor_mat[1, 2]))
                        se <- 1 / sqrt(n - 3)
                        result$pvalue <- 2 * (1 - pnorm(abs(z) / se))
                    } else {
                        # Fallback to Spearman
                        test_result <- cor.test(x, y, method = "spearman")
                        result$estimate <- test_result$estimate
                        result$pvalue <- test_result$p.value
                    }
                }
            }, error = function(e) {
                # If computation fails, use Spearman as fallback
                test_result <- cor.test(x, y, method = "spearman")
                result$estimate <<- test_result$estimate
                result$pvalue <<- test_result$p.value
            })

            return(result)
        },

        # Detect outliers
        .detectOutliers = function(data) {
            if (!is.null(private$.outlierResults) || !self$options$outlier_detection) {
                return(private$.outlierResults)
            }

            method <- self$options$outlier_method
            threshold <- self$options$outlier_threshold

            outliers <- list()
            outliers$indices <- c()
            outliers$distances <- c()
            outliers$is_outlier <- c()
            outliers$extreme_vars <- c()

            tryCatch({
                if (method == "mahalanobis") {
                    # Mahalanobis distance
                    data_mat <- as.matrix(data)
                    center <- colMeans(data_mat, na.rm = TRUE)
                    cov_mat <- cov(data_mat, use = "complete.obs")

                    if (det(cov_mat) > 0) {
                        distances <- mahalanobis(data_mat, center, cov_mat)
                        cutoff <- qchisq(0.975, df = ncol(data_mat))
                        is_outlier <- distances > cutoff * threshold
                    } else {
                        distances <- rep(NA, nrow(data_mat))
                        is_outlier <- rep(FALSE, nrow(data_mat))
                    }

                } else if (method == "mcd") {
                    if (requireNamespace("robustbase", quietly = TRUE)) {
                        data_mat <- as.matrix(data)
                        mcd_result <- robustbase::covMcd(data_mat)
                        distances <- sqrt(mahalanobis(data_mat, mcd_result$center, mcd_result$cov))
                        cutoff <- sqrt(qchisq(0.975, df = ncol(data_mat)))
                        is_outlier <- distances > cutoff * threshold
                    } else {
                        # Fallback to standard mahalanobis
                        data_mat <- as.matrix(data)
                        center <- colMeans(data_mat, na.rm = TRUE)
                        cov_mat <- cov(data_mat, use = "complete.obs")
                        distances <- mahalanobis(data_mat, center, cov_mat)
                        cutoff <- qchisq(0.975, df = ncol(data_mat))
                        is_outlier <- distances > cutoff * threshold
                    }

                } else if (method == "zscore") {
                    # Z-score method
                    data_scaled <- scale(data)
                    max_z <- apply(abs(data_scaled), 1, max, na.rm = TRUE)
                    distances <- max_z
                    is_outlier <- max_z > threshold

                } else if (method == "iqr") {
                    # IQR method
                    outlier_indicators <- matrix(FALSE, nrow = nrow(data), ncol = ncol(data))
                    distances <- rep(0, nrow(data))

                    for (j in 1:ncol(data)) {
                        Q1 <- quantile(data[[j]], 0.25, na.rm = TRUE)
                        Q3 <- quantile(data[[j]], 0.75, na.rm = TRUE)
                        IQR <- Q3 - Q1
                        lower_bound <- Q1 - threshold * IQR
                        upper_bound <- Q3 + threshold * IQR

                        outlier_indicators[, j] <- data[[j]] < lower_bound | data[[j]] > upper_bound
                        distances <- pmax(distances,
                                        pmax((lower_bound - data[[j]]) / IQR,
                                             (data[[j]] - upper_bound) / IQR, 0))
                    }
                    is_outlier <- rowSums(outlier_indicators) > 0
                }

                # Find extreme variables for each outlier
                extreme_vars <- rep("", nrow(data))
                if (method %in% c("zscore", "iqr")) {
                    for (i in which(is_outlier)) {
                        if (method == "zscore") {
                            data_scaled <- scale(data)
                            extreme_idx <- which(abs(data_scaled[i, ]) > threshold)
                        } else {
                            outlier_indicators <- matrix(FALSE, nrow = nrow(data), ncol = ncol(data))
                            for (j in 1:ncol(data)) {
                                Q1 <- quantile(data[[j]], 0.25, na.rm = TRUE)
                                Q3 <- quantile(data[[j]], 0.75, na.rm = TRUE)
                                IQR <- Q3 - Q1
                                lower_bound <- Q1 - threshold * IQR
                                upper_bound <- Q3 + threshold * IQR
                                outlier_indicators[, j] <- data[[j]] < lower_bound | data[[j]] > upper_bound
                            }
                            extreme_idx <- which(outlier_indicators[i, ])
                        }
                        if (length(extreme_idx) > 0) {
                            extreme_vars[i] <- paste(names(data)[extreme_idx], collapse = ", ")
                        }
                    }
                }

                outliers$indices <- 1:nrow(data)
                outliers$distances <- distances
                outliers$is_outlier <- is_outlier
                outliers$extreme_vars <- extreme_vars

            }, error = function(e) {
                outliers$indices <<- 1:nrow(data)
                outliers$distances <<- rep(NA, nrow(data))
                outliers$is_outlier <<- rep(FALSE, nrow(data))
                outliers$extreme_vars <<- rep("", nrow(data))
            })

            private$.outlierResults <- outliers
            return(outliers)
        },

        # Bootstrap confidence intervals
        .computeBootstrapCI = function(data) {
            if (!is.null(private$.bootstrapResults) || !self$options$bootstrap_ci) {
                return(private$.bootstrapResults)
            }

            vars <- self$options$dep
            n_vars <- length(vars)
            method <- self$options$method
            n_bootstrap <- self$options$n_bootstrap
            conf_level <- self$options$confidence_level

            bootstrap_results <- list()
            bootstrap_results$correlations <- list()
            bootstrap_results$ci_lower <- list()
            bootstrap_results$ci_upper <- list()
            bootstrap_results$bias <- list()
            bootstrap_results$se <- list()

            # Bootstrap function
            boot_cor <- function(data, indices) {
                boot_data <- data[indices, ]
                cors <- c()
                for (i in 1:(n_vars-1)) {
                    for (j in (i+1):n_vars) {
                        x <- boot_data[[vars[i]]]
                        y <- boot_data[[vars[j]]]

                        complete_cases <- complete.cases(x, y)
                        if (sum(complete_cases) >= 3) {
                            cor_result <- private$.computePairwiseCorrelation(
                                x[complete_cases], y[complete_cases], method
                            )
                            cors <- c(cors, cor_result$estimate)
                        } else {
                            cors <- c(cors, NA)
                        }
                    }
                }
                return(cors)
            }

            # Perform bootstrap
            if (requireNamespace("boot", quietly = TRUE)) {
                tryCatch({
                    boot_result <- boot::boot(data, boot_cor, R = n_bootstrap)

                    pair_idx <- 1
                    alpha <- 1 - conf_level

                    for (i in 1:(n_vars-1)) {
                        for (j in (i+1):n_vars) {
                            pair_name <- paste(vars[i], vars[j], sep = "-")

                            if (!is.null(boot_result$t[, pair_idx]) &&
                                sum(!is.na(boot_result$t[, pair_idx])) > 10) {

                                # Basic bootstrap CI
                                boot_vals <- boot_result$t[, pair_idx]
                                boot_vals <- boot_vals[!is.na(boot_vals)]

                                if (length(boot_vals) > 0) {
                                    original_cor <- boot_result$t0[pair_idx]
                                    bias <- mean(boot_vals) - original_cor
                                    se <- sd(boot_vals)

                                    ci_lower <- quantile(boot_vals, alpha/2, na.rm = TRUE)
                                    ci_upper <- quantile(boot_vals, 1 - alpha/2, na.rm = TRUE)

                                    bootstrap_results$correlations[[pair_name]] <- original_cor
                                    bootstrap_results$ci_lower[[pair_name]] <- ci_lower
                                    bootstrap_results$ci_upper[[pair_name]] <- ci_upper
                                    bootstrap_results$bias[[pair_name]] <- bias
                                    bootstrap_results$se[[pair_name]] <- se
                                }
                            }
                            pair_idx <- pair_idx + 1
                        }
                    }
                }, error = function(e) {
                    # Bootstrap failed, create empty results
                })
            }

            private$.bootstrapResults <- bootstrap_results
            return(bootstrap_results)
        }
    ),

    # Public methods
    public = list(

        # Main run method
        .run = function() {

            # Initial instructions
            if (length(self$options$dep) < 2) {
                instructions <- glue::glue(
                    "<br>Welcome to ClinicoPath Robust Correlation Analysis
                    <br><br>
                    This tool provides comprehensive robust correlation analysis including:
                    <br>• Multiple robust correlation methods (Spearman, Kendall, Percentage Bend, etc.)
                    <br>• Outlier detection and diagnostics
                    <br>• Bootstrap confidence intervals
                    <br>• Correlation heatmaps and diagnostic plots
                    <br><br>
                    Please select at least 2 continuous variables to begin.
                    <br><hr>"
                )
                self$results$instructions$setContent(instructions)
                return()
            }

            # Validate data
            data <- private$.prepareData()
            if (is.null(data)) return()

            instructions <- glue::glue(
                "<br>Robust correlation analysis using {toupper(self$options$method)} method
                <br>Variables: {paste(self$options$dep, collapse = ', ')}
                <br>Sample size: {nrow(data)} complete observations
                <br><hr>"
            )
            self$results$instructions$setContent(instructions)

            # Compute correlations
            cor_results <- private$.computeCorrelations(data)
            private$.populateCorrelationTable(cor_results)

            # Outlier detection
            if (self$options$outlier_detection) {
                outlier_results <- private$.detectOutliers(data)
                private$.populateOutlierTable(outlier_results)
            }

            # Bootstrap confidence intervals
            if (self$options$bootstrap_ci) {
                bootstrap_results <- private$.computeBootstrapCI(data)
                private$.populateBootstrapTable(bootstrap_results)
            }
        },

        # Populate correlation table
        .populateCorrelationTable = function(results) {
            table <- self$results$correlation_table
            vars <- self$options$dep
            n_vars <- length(vars)

            rows <- list()
            row_idx <- 1

            matrix_type <- self$options$matrix_type

            for (i in 1:n_vars) {
                for (j in 1:n_vars) {
                    include_pair <- FALSE

                    if (matrix_type == "full") {
                        include_pair <- (i != j)
                    } else if (matrix_type == "upper") {
                        include_pair <- (i < j)
                    } else if (matrix_type == "lower") {
                        include_pair <- (i > j)
                    }

                    if (include_pair) {
                        row <- list()
                        row[["var1"]] <- vars[i]
                        row[["var2"]] <- vars[j]
                        row[["correlation"]] <- results$correlations[i, j]
                        row[["n"]] <- results$n[i, j]

                        if (self$options$show_pvalues) {
                            row[["pvalue"]] <- results$pvalues[i, j]
                            if (!is.null(results$pvalues_adj)) {
                                row[["pvalue_adj"]] <- results$pvalues_adj[i, j]
                            }
                        }

                        rows[[row_idx]] <- row
                        row_idx <- row_idx + 1
                    }
                }
            }

            table$setRows(rows)
        },

        # Populate outlier table
        .populateOutlierTable = function(outlier_results) {
            table <- self$results$outlier_table

            # Show only detected outliers
            outlier_indices <- which(outlier_results$is_outlier)

            rows <- list()
            for (i in seq_along(outlier_indices)) {
                idx <- outlier_indices[i]
                row <- list(
                    observation = idx,
                    distance = outlier_results$distances[idx],
                    threshold = self$options$outlier_threshold,
                    outlier = "Yes",
                    variables = outlier_results$extreme_vars[idx]
                )
                rows[[i]] <- row
            }

            table$setRows(rows)
        },

        # Populate bootstrap table
        .populateBootstrapTable = function(bootstrap_results) {
            table <- self$results$bootstrap_table

            rows <- list()
            row_idx <- 1

            for (pair_name in names(bootstrap_results$correlations)) {
                vars_split <- strsplit(pair_name, "-")[[1]]

                row <- list(
                    var1 = vars_split[1],
                    var2 = vars_split[2],
                    correlation = bootstrap_results$correlations[[pair_name]],
                    ci_lower = bootstrap_results$ci_lower[[pair_name]],
                    ci_upper = bootstrap_results$ci_upper[[pair_name]],
                    bias = bootstrap_results$bias[[pair_name]],
                    se = bootstrap_results$se[[pair_name]]
                )

                rows[[row_idx]] <- row
                row_idx <- row_idx + 1
            }

            table$setRows(rows)
        },

        # Plot correlation heatmap
        .plot_heatmap = function(image, ggtheme, theme, ...) {
            if (length(self$options$dep) < 2) return()

            data <- private$.prepareData()
            if (is.null(data)) return()

            cor_results <- private$.computeCorrelations(data)

            # Create correlation matrix for plotting
            cor_matrix <- cor_results$correlations

            # Apply matrix type filter
            if (self$options$matrix_type == "upper") {
                cor_matrix[lower.tri(cor_matrix)] <- NA
            } else if (self$options$matrix_type == "lower") {
                cor_matrix[upper.tri(cor_matrix)] <- NA
            }

            # Set diagonal to NA to avoid showing self-correlations
            diag(cor_matrix) <- NA

            # Create heatmap
            if (requireNamespace("ggcorrplot", quietly = TRUE)) {

                # Determine colors
                if (self$options$heatmap_colors == "custom") {
                    colors <- c(self$options$low_color, self$options$mid_color, self$options$high_color)
                } else if (self$options$heatmap_colors == "viridis") {
                    colors <- c("#440154", "#21908C", "#FDE725")
                } else if (self$options$heatmap_colors == "plasma") {
                    colors <- c("#0D0887", "#CC4678", "#F0F921")
                } else if (self$options$heatmap_colors == "red_yellow_blue") {
                    colors <- c("#D73027", "#FFFFBF", "#1A9850")
                } else {
                    colors <- c("#3B82C5", "white", "#E74C3C")  # blue_white_red
                }

                plot <- ggcorrplot::ggcorrplot(
                    cor_matrix,
                    method = "square",
                    type = ifelse(self$options$matrix_type == "full", "full", self$options$matrix_type),
                    show.diag = FALSE,
                    colors = colors,
                    lab = TRUE,
                    lab_size = 3,
                    digits = self$options$decimal_places
                ) +
                ggplot2::labs(
                    title = paste("Robust Correlation Matrix -", toupper(self$options$method)),
                    subtitle = paste("Method:", self$options$method, "| N =", nrow(data))
                ) +
                ggtheme

                print(plot)

            } else {
                # Fallback basic heatmap
                plot <- ggplot2::ggplot(data = reshape2::melt(cor_matrix, na.rm = TRUE),
                                       ggplot2::aes(x = Var1, y = Var2, fill = value)) +
                    ggplot2::geom_tile() +
                    ggplot2::scale_fill_gradient2(low = colors[1], mid = colors[2], high = colors[3],
                                                 midpoint = 0, limit = c(-1, 1)) +
                    ggplot2::labs(title = paste("Robust Correlation Matrix -", toupper(self$options$method)),
                                 x = "", y = "", fill = "Correlation") +
                    ggplot2::theme_minimal() +
                    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

                print(plot)
            }

            TRUE
        },

        # Plot outliers
        .plot_outliers = function(image, ggtheme, theme, ...) {
            if (!self$options$outlier_detection || length(self$options$dep) < 2) return()

            data <- private$.prepareData()
            if (is.null(data)) return()

            outlier_results <- private$.detectOutliers(data)

            # Create scatter plot with outliers highlighted
            if (ncol(data) >= 2) {
                # Use first two variables for plotting
                var1 <- names(data)[1]
                var2 <- names(data)[2]

                plot_data <- data.frame(
                    x = data[[var1]],
                    y = data[[var2]],
                    outlier = outlier_results$is_outlier,
                    distance = outlier_results$distances
                )

                plot <- ggplot2::ggplot(plot_data, ggplot2::aes(x = x, y = y)) +
                    ggplot2::geom_point(ggplot2::aes(color = outlier, size = distance), alpha = 0.7) +
                    ggplot2::scale_color_manual(values = c("FALSE" = "blue", "TRUE" = "red"),
                                              labels = c("Normal", "Outlier")) +
                    ggplot2::labs(
                        title = paste("Outlier Detection -", toupper(self$options$outlier_method)),
                        subtitle = paste("Threshold:", self$options$outlier_threshold, "| Outliers:", sum(outlier_results$is_outlier)),
                        x = var1,
                        y = var2,
                        color = "Status",
                        size = "Distance"
                    ) +
                    ggtheme

                print(plot)
            }

            TRUE
        },

        # Plot diagnostics
        .plot_diagnostics = function(image, ggtheme, theme, ...) {
            if (!self$options$show_diagnostics || length(self$options$dep) < 2) return()

            data <- private$.prepareData()
            if (is.null(data)) return()

            # Create diagnostic plots (QQ plots, histograms, etc.)
            if (requireNamespace("gridExtra", quietly = TRUE)) {
                plots <- list()

                # QQ plots for normality check
                for (i in seq_along(self$options$dep)) {
                    var <- self$options$dep[i]
                    qq_plot <- ggplot2::ggplot(data, ggplot2::aes(sample = .data[[var]])) +
                        ggplot2::stat_qq() +
                        ggplot2::stat_qq_line() +
                        ggplot2::labs(title = paste("QQ Plot -", var)) +
                        ggtheme

                    plots[[paste0("qq_", i)]] <- qq_plot
                }

                # Combine plots
                combined_plot <- do.call(gridExtra::grid.arrange, c(plots, ncol = 2))
                print(combined_plot)
            } else {
                # Simple diagnostic plot
                var1 <- self$options$dep[1]
                plot <- ggplot2::ggplot(data, ggplot2::aes(sample = .data[[var1]])) +
                    ggplot2::stat_qq() +
                    ggplot2::stat_qq_line() +
                    ggplot2::labs(title = paste("Diagnostic Plot -", var1)) +
                    ggtheme

                print(plot)
            }

            TRUE
        },

        # Scatterplot matrix
        .plot_scatter_matrix = function(image, ggtheme, theme, ...) {
            if (length(self$options$dep) < 2) return()

            data <- private$.prepareData()
            if (is.null(data)) return()

            # Create pairs plot
            if (requireNamespace("GGally", quietly = TRUE)) {
                plot <- GGally::ggpairs(data,
                                       title = "Scatterplot Matrix",
                                       upper = list(continuous = "cor"),
                                       lower = list(continuous = "points")) +
                    ggtheme

                print(plot)
            } else {
                # Fallback simple scatter
                if (ncol(data) >= 2) {
                    plot <- ggplot2::ggplot(data, ggplot2::aes(x = .data[[names(data)[1]]],
                                                              y = .data[[names(data)[2]]])) +
                        ggplot2::geom_point(alpha = 0.6) +
                        ggplot2::geom_smooth(method = "loess", se = TRUE) +
                        ggplot2::labs(title = "Scatterplot Matrix (Subset)") +
                        ggtheme

                    print(plot)
                }
            }

            TRUE
        }
    )
)
