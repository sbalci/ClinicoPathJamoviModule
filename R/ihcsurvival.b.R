#' @title IHC Survival Analysis
#' @importFrom R6 R6Class
#' @import jmvcore
#' @importFrom survival Surv survfit coxph survdiff cox.zph
#' @importFrom cluster daisy
#' @import ggplot2

# Define null coalescing operator
`%||%` <- function(x, y) if (!is.null(x)) x else y

ihcsurvivalClass <- if (requireNamespace('jmvcore', quietly = TRUE)) R6::R6Class(
    "ihcsurvivalClass",
    inherit = ihcsurvivalBase,
    private = list(
        .ihc_matrix = NULL,
        .survival_object = NULL,
        .risk_groups = NULL,
        .cox_model = NULL,
        .prog_clusters = NULL,
        .cache = list(),

        .init = function() {
            if (is.null(self$data) || length(self$options$markers) == 0 ||
                is.null(self$options$survivalTime) || self$options$survivalTime == "" ||
                is.null(self$options$survivalEvent) || self$options$survivalEvent == "") {
                self$results$instructions$setContent(
                    "<h3>IHC Survival & Prognostic Analysis</h3>
                    <p>This analysis performs survival analysis using IHC markers for prognostic assessment and risk stratification.</p>

                    <h4>Required Input:</h4>
                    <ul>
                        <li><b>IHC Markers:</b> Biomarker expression data</li>
                        <li><b>Survival Time:</b> Time to event (months, years, days)</li>
                        <li><b>Survival Event:</b> Event indicator (1=event, 0=censored)</li>
                        <li><b>Minimum samples:</b> At least 20 cases with events for meaningful analysis</li>
                    </ul>

                    <h4>Analysis Features:</h4>
                    <ul>
                        <li><b>Prognostic Clustering:</b> Survival-optimized patient grouping</li>
                        <li><b>Cox Regression:</b> Hazard ratio estimation with confidence intervals</li>
                        <li><b>Risk Stratification:</b> Low/intermediate/high risk group classification</li>
                        <li><b>Kaplan-Meier:</b> Survival curve visualization and statistics</li>
                        <li><b>Multi-region Analysis:</b> Central vs invasive tumor comparison</li>
                        <li><b>Landmark Analysis:</b> Time-specific survival probabilities</li>
                    </ul>

                    <p><i>Select your markers and survival variables to begin.</i></p>"
                )
            }

            # Validate multi-region analysis setup
            if (self$options$multiRegionAnalysis) {
                private$.validateMultiRegionSetup()
            }
        },

        .run = function() {
            # Validate inputs
            if (is.null(self$data) || length(self$options$markers) == 0 ||
                is.null(self$options$survivalTime) || self$options$survivalTime == "" ||
                is.null(self$options$survivalEvent) || self$options$survivalEvent == "") {
                return()
            }

            if (!requireNamespace("survival", quietly = TRUE)) {
                self$results$instructions$setContent(
                    "<p style='color: red;'><b>Error:</b> The 'survival' package is required for this analysis. Please install it.</p>"
                )
                return()
            }

            validation <- private$.validateData()
            if (!validation$valid) {
                self$results$instructions$setContent(
                    paste0("<p style='color: red;'><b>Error:</b> ", validation$message, "</p>")
                )
                return()
            }

            # Prepare data
            private$.prepareData()

            # Perform prognostic clustering
            if (self$options$prognosticClustering) {
                private$.performPrognosticClustering()
            }

            # Cox regression analysis
            if (self$options$coxRegression) {
                private$.performCoxRegression()
            }

            # Multi-region analysis
            if (self$options$multiRegionAnalysis) {
                private$.performMultiRegionAnalysis()
            }

            # Landmark analysis
            if (self$options$landmarkAnalysis) {
                private$.performLandmarkAnalysis()
            }

            # Update summary and panels
            private$.updateInstructions()
            private$.updateAssumptionsPanel()

            # Generate report if prognostic clustering was performed
            if (self$options$prognosticClustering && !is.null(private$.risk_groups)) {
                private$.updateReportPanel()
            }
        },

        .validateData = function() {
            # Check survival variables
            time_data <- self$data[[self$options$survivalTime]]
            event_data <- self$data[[self$options$survivalEvent]]

            if (any(is.na(time_data)) || any(is.na(event_data))) {
                return(list(valid = FALSE, message = "Missing values in survival variables not allowed"))
            }

            if (any(time_data <= 0)) {
                return(list(valid = FALSE, message = "Survival times must be positive"))
            }

            # Check number of events
            n_events <- sum(event_data == 1, na.rm = TRUE)
            if (n_events < 10) {
                return(list(valid = FALSE, message = sprintf("Insufficient events for analysis (n=%d, need ≥10)", n_events)))
            }

            # Check sample size
            if (nrow(self$data) < 20) {
                return(list(valid = FALSE, message = "At least 20 samples required for survival analysis"))
            }

            return(list(valid = TRUE))
        },

        .prepareData = function() {
            # Convert markers to numeric
            markers <- self$options$markers
            private$.ihc_matrix <- matrix(0, nrow = nrow(self$data), ncol = length(markers))
            colnames(private$.ihc_matrix) <- markers

            for (i in seq_along(markers)) {
                marker_data <- self$data[[markers[i]]]
                if (is.factor(marker_data) || is.character(marker_data)) {
                    levels <- sort(unique(marker_data[!is.na(marker_data)]))
                    numeric_values <- as.numeric(factor(marker_data, levels = levels)) - 1
                } else {
                    numeric_values <- as.numeric(marker_data)
                }
                private$.ihc_matrix[, i] <- numeric_values
            }

            # Create survival object
            time_data <- self$data[[self$options$survivalTime]]
            event_data <- self$data[[self$options$survivalEvent]]
            private$.survival_object <- survival::Surv(time_data, event_data)
        },

        .performPrognosticClustering = function() {
            # Perform survival-guided clustering
            # Use Cox regression to weight markers by prognostic value
            marker_weights <- numeric(ncol(private$.ihc_matrix))

            for (i in seq_len(ncol(private$.ihc_matrix))) {
                marker_data <- private$.ihc_matrix[, i]
                tryCatch({
                    cox_temp <- survival::coxph(private$.survival_object ~ marker_data)
                    marker_weights[i] <- abs(coef(cox_temp)[1])
                }, error = function(e) {
                    marker_weights[i] <- 0
                })
            }

            # Weight the distance matrix by prognostic importance
            weighted_matrix <- sweep(private$.ihc_matrix, 2, marker_weights, "*")
            dist_matrix <- cluster::daisy(weighted_matrix, metric = "gower")

            # Determine optimal number of risk groups
            n_groups <- switch(self$options$riskStratification,
                              "binary" = 2,
                              "tertiles" = 3,
                              "quartiles" = 4,
                              "optimal" = private$.findOptimalRiskGroups())

            # Perform hierarchical clustering optimized for survival
            hc <- hclust(dist_matrix, method = "ward.D2")
            private$.prog_clusters <- cutree(hc, k = n_groups)

            # Create risk groups based on median survival
            private$.createRiskGroups()
            private$.populatePrognosticGroups()
        },

        .findOptimalRiskGroups = function() {
            # Test different numbers of groups and select based on log-rank test
            max_groups <- min(5, floor(nrow(self$data) / 20))  # At least 20 per group
            best_p <- 1
            best_k <- 2

            for (k in 2:max_groups) {
                hc <- hclust(cluster::daisy(private$.ihc_matrix, metric = "gower"), method = "ward.D2")
                temp_clusters <- cutree(hc, k = k)

                tryCatch({
                    logrank_test <- survival::survdiff(private$.survival_object ~ temp_clusters)
                    p_value <- 1 - pchisq(logrank_test$chisq, df = k - 1)

                    if (p_value < best_p) {
                        best_p <- p_value
                        best_k <- k
                    }
                }, error = function(e) {
                    # Skip this k if error
                })
            }

            return(best_k)
        },

        .createRiskGroups = function() {
            # Assign risk group labels based on median survival
            n_groups <- length(unique(private$.prog_clusters))
            group_survival <- numeric(n_groups)

            for (i in 1:n_groups) {
                group_idx <- private$.prog_clusters == i
                group_surv <- private$.survival_object[group_idx]

                tryCatch({
                    km_fit <- survival::survfit(group_surv ~ 1)
                    group_survival[i] <- summary(km_fit)$table["median"]
                }, error = function(e) {
                    group_survival[i] <- NA
                })
            }

            # Order groups by survival (worst to best)
            group_order <- order(group_survival, na.last = TRUE)

            # Create risk group labels
            risk_labels <- switch(as.character(n_groups),
                                "2" = c("High Risk", "Low Risk"),
                                "3" = c("High Risk", "Intermediate Risk", "Low Risk"),
                                "4" = c("High Risk", "Intermediate-High Risk", "Intermediate-Low Risk", "Low Risk"))

            # Map clusters to risk groups
            private$.risk_groups <- factor(private$.prog_clusters, levels = group_order, labels = risk_labels)
        },

        .populatePrognosticGroups = function() {
            risk_levels <- levels(private$.risk_groups)

            for (risk_group in risk_levels) {
                group_idx <- private$.risk_groups == risk_group
                group_surv <- private$.survival_object[group_idx]
                group_markers <- private$.ihc_matrix[group_idx, , drop = FALSE]

                # Calculate survival statistics
                n <- sum(group_idx)
                # Use proper survival object extraction
                events <- sum(group_surv[, "status"] == 1)  # More robust event extraction

                tryCatch({
                    km_fit <- survival::survfit(group_surv ~ 1)
                    median_surv <- summary(km_fit)$table["median"]
                    ci_lower <- summary(km_fit)$table["0.95LCL"]
                    ci_upper <- summary(km_fit)$table["0.95UCL"]
                    ci_text <- sprintf("%.1f - %.1f", ci_lower, ci_upper)
                }, error = function(e) {
                    median_surv <- NA
                    ci_text <- "Not estimable"
                })

                # Describe IHC pattern
                pattern <- private$.describeGroupPattern(group_markers)

                self$results$prognosticGroups$addRow(
                    rowKey = risk_group,
                    values = list(
                        group = risk_group,
                        n = n,
                        events = events,
                        median_survival = median_surv,
                        survival_95ci = ci_text,
                        pattern = pattern
                    )
                )
            }
        },

        .describeGroupPattern = function(group_markers) {
            # Describe the predominant IHC pattern for this group
            if (nrow(group_markers) == 0) return("No data")

            mean_expression <- colMeans(group_markers, na.rm = TRUE)
            marker_names <- colnames(group_markers)

            # Find high and low markers
            high_markers <- marker_names[mean_expression > quantile(mean_expression, 0.75)]
            low_markers <- marker_names[mean_expression < quantile(mean_expression, 0.25)]

            if (length(high_markers) > 0) {
                return(paste0(paste(high_markers, collapse = "+"), " high"))
            } else if (length(low_markers) > 0) {
                return(paste0(paste(low_markers, collapse = "+"), " low"))
            } else {
                return("Mixed expression")
            }
        },

        .performCoxRegression = function() {
            # Univariate Cox regression for each marker
            markers <- colnames(private$.ihc_matrix)

            for (marker in markers) {
                marker_data <- private$.ihc_matrix[, marker]

                # Use refactored Cox calculation
                cox_result <- private$.calculateMarkerHR(marker_data, self$options$confidenceLevel)

                if (!is.na(cox_result$hr)) {
                    ci_text <- sprintf("%.3f - %.3f", cox_result$ci_lower, cox_result$ci_upper)
                    significance <- private$.getSignificance(cox_result$p_value)

                    # Get clinical interpretation
                    interpretation <- private$.interpretHazardRatio(cox_result$hr, cox_result$p_value, marker)

                    self$results$coxResults$addRow(
                        rowKey = marker,
                        values = list(
                            variable = marker,
                            hazard_ratio = cox_result$hr,
                            hr_95ci = ci_text,
                            p_value = cox_result$p_value,
                            significance = significance,
                            clinical_interpretation = interpretation$clinical
                        )
                    )
                } else {
                    self$results$coxResults$addRow(
                        rowKey = marker,
                        values = list(
                            variable = marker,
                            hazard_ratio = NA,
                            hr_95ci = "Not estimable",
                            p_value = NA,
                            significance = "",
                            clinical_interpretation = "Unable to determine clinical significance"
                        )
                    )
                }
            }

            # Risk group analysis
            if (!is.null(private$.risk_groups)) {
                tryCatch({
                    # Use lowest risk as reference
                    risk_levels <- levels(private$.risk_groups)
                    risk_groups_numeric <- as.numeric(private$.risk_groups)

                    cox_risk <- survival::coxph(private$.survival_object ~ risk_groups_numeric)

                    hr_risk <- exp(coef(cox_risk)[1])
                    ci_risk <- exp(confint(cox_risk, level = self$options$confidenceLevel))
                    p_val_risk <- summary(cox_risk)$coefficients[, "Pr(>|z|)"][1]

                    ci_text_risk <- sprintf("%.3f - %.3f", ci_risk[1], ci_risk[2])
                    significance_risk <- private$.getSignificance(p_val_risk)

                    self$results$coxResults$addRow(
                        rowKey = "risk_group",
                        values = list(
                            variable = "Risk Group (ordinal)",
                            hazard_ratio = hr_risk,
                            hr_95ci = ci_text_risk,
                            p_value = p_val_risk,
                            significance = significance_risk
                        )
                    )
                }, error = function(e) {
                    # Skip if error
                })
            }

            # Multivariate analysis if adjustment variables provided
            if (length(self$options$multivariateAdjustment) > 0) {
                private$.performMultivariateAnalysis()
            }

            # Model fit statistics
            private$.calculateModelFit()
        },

        .performMultivariateAnalysis = function() {
            # Combine IHC markers with adjustment variables
            adjustment_vars <- self$options$multivariateAdjustment

            # Create data frame for multivariate model
            model_data <- data.frame(private$.ihc_matrix)

            for (var in adjustment_vars) {
                model_data[[var]] <- self$data[[var]]
            }

            # Remove rows with missing data
            complete_idx <- complete.cases(model_data)
            model_data <- model_data[complete_idx, ]
            surv_complete <- private$.survival_object[complete_idx]

            # Fit multivariate model
            tryCatch({
                formula_str <- paste("surv_complete ~", paste(names(model_data), collapse = " + "))
                multivar_model <- survival::coxph(as.formula(formula_str), data = model_data)

                # Extract results
                coeffs <- summary(multivar_model)$coefficients
                conf_ints <- exp(confint(multivar_model, level = self$options$confidenceLevel))

                for (i in seq_len(nrow(coeffs))) {
                    var_name <- rownames(coeffs)[i]
                    hr <- exp(coeffs[i, "coef"])
                    p_val <- coeffs[i, "Pr(>|z|)"]
                    ci_text <- sprintf("%.3f - %.3f", conf_ints[i, 1], conf_ints[i, 2])
                    significance <- private$.getSignificance(p_val)

                    self$results$multivariateResults$addRow(
                        rowKey = var_name,
                        values = list(
                            variable = var_name,
                            hazard_ratio = hr,
                            hr_95ci = ci_text,
                            p_value = p_val,
                            significance = significance
                        )
                    )
                }
            }, error = function(e) {
                # Add error row
                self$results$multivariateResults$addRow(
                    rowKey = "error",
                    values = list(
                        variable = "Multivariate model",
                        hazard_ratio = NA,
                        hr_95ci = "Model failed",
                        p_value = NA,
                        significance = ""
                    )
                )
            })
        },

        .calculateModelFit = function() {
            if (is.null(private$.risk_groups)) return()

            tryCatch({
                # Concordance index
                cox_risk <- survival::coxph(private$.survival_object ~ as.numeric(private$.risk_groups))
                concordance <- summary(cox_risk)$concordance["C"]

                # Log-rank test
                logrank_test <- survival::survdiff(private$.survival_object ~ private$.risk_groups)
                logrank_p <- 1 - pchisq(logrank_test$chisq, df = length(levels(private$.risk_groups)) - 1)

                # AIC
                aic_value <- AIC(cox_risk)

                # Add results
                metrics <- list(
                    list(metric = "C-index", value = concordance, interpretation =
                         if (concordance > 0.7) "Good discrimination"
                         else if (concordance > 0.6) "Moderate discrimination"
                         else "Poor discrimination"),
                    list(metric = "Log-rank p-value", value = logrank_p, interpretation =
                         if (logrank_p < 0.001) "Highly significant"
                         else if (logrank_p < 0.05) "Significant"
                         else "Not significant"),
                    list(metric = "AIC", value = aic_value, interpretation = "Lower is better")
                )

                for (metric in metrics) {
                    self$results$modelFit$addRow(
                        rowKey = metric$metric,
                        values = metric
                    )
                }
            }, error = function(e) {
                # Skip if error
            })
        },

        .performMultiRegionAnalysis = function() {
            # Compare central vs invasive regions
            if (length(self$options$centralRegion) == 0 || length(self$options$invasiveRegion) == 0) {
                return()
            }

            central_markers <- self$options$centralRegion
            invasive_markers <- self$options$invasiveRegion

            # Find common markers
            common_markers <- intersect(central_markers, invasive_markers)

            for (marker in common_markers) {
                central_data <- self$data[[paste0(marker, "_central")]]  # Assuming naming convention
                invasive_data <- self$data[[paste0(marker, "_invasive")]]

                if (is.null(central_data) || is.null(invasive_data)) {
                    # Try exact marker names
                    central_data <- self$data[[marker]]
                    invasive_data <- self$data[[marker]]
                }

                tryCatch({
                    # Cox models for each region
                    cox_central <- survival::coxph(private$.survival_object ~ central_data)
                    cox_invasive <- survival::coxph(private$.survival_object ~ invasive_data)

                    hr_central <- exp(coef(cox_central)[1])
                    hr_invasive <- exp(coef(cox_invasive)[1])

                    # Concordance comparison
                    conc_central <- summary(cox_central)$concordance["C"]
                    conc_invasive <- summary(cox_invasive)$concordance["C"]

                    better_region <- if (conc_central > conc_invasive) "Central" else "Invasive"

                    self$results$regionalComparison$addRow(
                        rowKey = marker,
                        values = list(
                            marker = marker,
                            central_hr = hr_central,
                            invasive_hr = hr_invasive,
                            concordance = max(conc_central, conc_invasive),
                            better_region = better_region
                        )
                    )
                }, error = function(e) {
                    # Skip this marker if error
                })
            }
        },

        .performLandmarkAnalysis = function() {
            if (is.null(private$.risk_groups)) return()

            landmark_time <- self$options$landmarkTime

            # Filter to patients alive at landmark time
            time_data <- self$data[[self$options$survivalTime]]
            event_data <- self$data[[self$options$survivalEvent]]

            landmark_idx <- time_data >= landmark_time

            if (sum(landmark_idx) < 10) {
                return()  # Not enough patients at landmark
            }

            # Subset data to landmark survivors
            landmark_surv <- survival::Surv(time_data[landmark_idx] - landmark_time,
                                          event_data[landmark_idx])
            landmark_groups <- private$.risk_groups[landmark_idx]

            # Calculate survival probabilities from landmark
            for (group in levels(landmark_groups)) {
                group_idx <- landmark_groups == group
                group_surv <- landmark_surv[group_idx]

                if (sum(group_idx) < 5) next  # Skip small groups

                tryCatch({
                    km_fit <- survival::survfit(group_surv ~ 1)

                    # Get survival at specific time points (configurable)
                    time_points <- if (!is.null(self$options$landmarkTimePoints) && self$options$landmarkTimePoints != "") {
                        tryCatch({
                            as.numeric(strsplit(self$options$landmarkTimePoints, ",")[[1]])
                        }, error = function(e) {
                            seq(12, 60, 12)  # Default fallback
                        })
                    } else {
                        seq(12, 60, 12)  # Default: 1-5 years
                    }

                    for (t in time_points) {
                        if (max(km_fit$time) >= t) {
                            surv_prob <- summary(km_fit, times = t)$surv
                            surv_ci_lower <- summary(km_fit, times = t)$lower
                            surv_ci_upper <- summary(km_fit, times = t)$upper

                            ci_text <- sprintf("%.3f - %.3f", surv_ci_lower, surv_ci_upper)

                            self$results$landmarkResults$addRow(
                                rowKey = paste(group, t, sep = "_"),
                                values = list(
                                    group = paste(group, "at", t, "months"),
                                    at_risk = sum(group_idx),
                                    survival_prob = surv_prob,
                                    survival_95ci = ci_text
                                )
                            )
                        }
                    }
                }, error = function(e) {
                    # Skip this group if error
                })
            }
        },

        .updateInstructions = function() {
            n_samples <- nrow(self$data)
            n_markers <- length(self$options$markers)
            n_events <- sum(self$data[[self$options$survivalEvent]] == 1, na.rm = TRUE)

            follow_up_time <- max(self$data[[self$options$survivalTime]], na.rm = TRUE)

            summary_html <- sprintf(
                "<h3>Survival Analysis Complete</h3>
                <p><b>Data Summary:</b></p>
                <ul>
                    <li>Total cases: %d</li>
                    <li>Events observed: %d (%.1f%%)</li>
                    <li>IHC markers: %d</li>
                    <li>Maximum follow-up: %.1f</li>
                </ul>
                <p><i>Review the survival analysis results below.</i></p>",
                n_samples, n_events, 100 * n_events / n_samples,
                n_markers, follow_up_time
            )

            self$results$instructions$setContent(summary_html)
        },

        .plotKaplanMeier = function(image, ...) {
            if (is.null(private$.risk_groups)) {
                return()
            }

            # Create Kaplan-Meier plot
            km_fit <- survival::survfit(private$.survival_object ~ private$.risk_groups)

            # Colorblind-safe palette
            cb_colors <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", "#8c564b")
            n_groups <- length(levels(private$.risk_groups))
            plot_colors <- cb_colors[1:min(n_groups, length(cb_colors))]

            # Enhanced plot with better styling
            plot(km_fit,
                 main = "Kaplan-Meier Survival Curves by Risk Group",
                 xlab = paste("Time (", attr(private$.survival_object, "time.label", exact = TRUE) %||% "units", ")", sep = ""),
                 ylab = "Survival Probability",
                 col = plot_colors,
                 lwd = 3,
                 cex.main = 1.2,
                 cex.lab = 1.1)

            # Add confidence intervals if space allows
            if (n_groups <= 3) {
                plot(km_fit, col = plot_colors, lwd = 3, conf.int = TRUE, add = TRUE)
            }

            # Enhanced legend
            legend("topright",
                   legend = levels(private$.risk_groups),
                   col = plot_colors,
                   lwd = 3,
                   bty = "n",
                   cex = 0.9)

            # Add p-value from log-rank test
            tryCatch({
                logrank_test <- survival::survdiff(private$.survival_object ~ private$.risk_groups)
                p_val <- 1 - pchisq(logrank_test$chisq, df = n_groups - 1)
                p_text <- if (p_val < 0.001) "p < 0.001"
                         else if (p_val < 0.01) sprintf("p = %.3f", p_val)
                         else sprintf("p = %.2f", p_val)
                mtext(paste("Log-rank test:", p_text), side = 3, adj = 0, cex = 0.8)
            }, error = function(e) {
                # Skip p-value if calculation fails
            })

            TRUE
        },

        .plotHazardRatios = function(image, ...) {
            # Create forest plot of hazard ratios
            if (is.null(private$.ihc_matrix)) {
                return()
            }

            markers <- colnames(private$.ihc_matrix)
            hrs <- numeric(length(markers))
            ci_lower <- numeric(length(markers))
            ci_upper <- numeric(length(markers))
            p_values <- numeric(length(markers))

            # Calculate hazard ratios for each marker
            for (i in seq_along(markers)) {
                marker_data <- private$.ihc_matrix[, i]
                tryCatch({
                    cox_model <- survival::coxph(private$.survival_object ~ marker_data)
                    hrs[i] <- exp(coef(cox_model)[1])
                    ci <- exp(confint(cox_model, level = self$options$confidenceLevel))
                    ci_lower[i] <- ci[1]
                    ci_upper[i] <- ci[2]
                    p_values[i] <- summary(cox_model)$coefficients[, "Pr(>|z|)"][1]
                }, error = function(e) {
                    hrs[i] <- NA
                    ci_lower[i] <- NA
                    ci_upper[i] <- NA
                    p_values[i] <- NA
                })
            }

            # Remove NA values
            valid_idx <- !is.na(hrs)
            if (sum(valid_idx) == 0) {
                plot(1, 1, main = "No Valid Hazard Ratios", type = "n")
                return(TRUE)
            }

            markers <- markers[valid_idx]
            hrs <- hrs[valid_idx]
            ci_lower <- ci_lower[valid_idx]
            ci_upper <- ci_upper[valid_idx]
            p_values <- p_values[valid_idx]

            # Set up plot
            y_pos <- seq_len(length(markers))
            plot_range <- range(c(ci_lower, ci_upper, 1), na.rm = TRUE)
            plot_range[1] <- max(0.1, plot_range[1] * 0.9)
            plot_range[2] <- plot_range[2] * 1.1

            # Create forest plot
            plot(hrs, y_pos,
                 xlim = plot_range, ylim = c(0.5, length(markers) + 0.5),
                 main = "Hazard Ratio Forest Plot",
                 xlab = "Hazard Ratio", ylab = "",
                 pch = 18, cex = 1.5, col = "#1f77b4",
                 log = "x", yaxt = "n")

            # Add confidence intervals
            for (i in seq_along(y_pos)) {
                lines(c(ci_lower[i], ci_upper[i]), c(y_pos[i], y_pos[i]),
                      col = "#1f77b4", lwd = 2)
            }

            # Add reference line at HR = 1
            abline(v = 1, col = "red", lty = 2, lwd = 2)

            # Add marker labels
            axis(2, at = y_pos, labels = markers, las = 2, cex.axis = 0.8)

            # Add significance indicators
            for (i in seq_along(y_pos)) {
                if (p_values[i] < 0.05) {
                    text(max(plot_range), y_pos[i],
                         if (p_values[i] < 0.001) "***"
                         else if (p_values[i] < 0.01) "**"
                         else "*",
                         col = "red", cex = 1.2)
                }
            }

            TRUE
        },

        .plotRiskScores = function(image, ...) {
            # Enhanced risk score distribution plot
            if (is.null(private$.risk_groups)) return()

            # Colorblind-safe palette
            cb_colors <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", "#8c564b")
            n_groups <- length(levels(private$.risk_groups))
            plot_colors <- cb_colors[1:min(n_groups, length(cb_colors))]

            # Create enhanced bar plot
            risk_table <- table(private$.risk_groups)
            bp <- barplot(risk_table,
                         main = "Risk Group Distribution",
                         ylab = "Number of Patients",
                         xlab = "Risk Groups",
                         col = plot_colors,
                         border = "white",
                         cex.main = 1.2,
                         cex.lab = 1.1)

            # Add count labels on bars
            text(bp, risk_table + max(risk_table) * 0.02,
                 labels = as.character(risk_table),
                 cex = 1.1, font = 2)

            # Add percentage labels
            percentages <- round(100 * risk_table / sum(risk_table), 1)
            text(bp, risk_table / 2,
                 labels = paste0(percentages, "%"),
                 cex = 0.9, col = "white", font = 2)

            # Add grid for better readability
            grid(nx = NA, ny = NULL, col = "gray90", lty = 1)

            # Redraw bars to overlay grid
            barplot(risk_table,
                   col = plot_colors,
                   border = "white",
                   add = TRUE)

            TRUE
        },

        .getSignificance = function(p) {
            if (is.na(p)) return("")
            if (p < 0.001) return("***")
            if (p < 0.01) return("**")
            if (p < 0.05) return("*")
            return("ns")
        },

        # Reusable Cox regression calculation
        .calculateMarkerHR = function(marker_data, confidence_level = 0.95) {
            tryCatch({
                cox_model <- survival::coxph(private$.survival_object ~ marker_data)
                hr <- exp(coef(cox_model)[1])
                ci <- exp(confint(cox_model, level = confidence_level))
                p_value <- summary(cox_model)$coefficients[, "Pr(>|z|)"][1]

                list(
                    hr = hr,
                    ci_lower = ci[1],
                    ci_upper = ci[2],
                    p_value = p_value,
                    model = cox_model
                )
            }, error = function(e) {
                list(hr = NA, ci_lower = NA, ci_upper = NA, p_value = NA, model = NULL)
            })
        },

        # Clinical interpretation helper
        .interpretHazardRatio = function(hr, p_value, marker_name) {
            if (is.na(hr)) return(list(interpretation = "Not estimable", clinical = ""))

            direction <- if (hr > 1) "increased" else "decreased"
            magnitude <- if (hr > 2 || hr < 0.5) "substantially"
                        else if (hr > 1.5 || hr < 0.67) "moderately"
                        else "slightly"

            significance <- if (is.na(p_value)) "unknown significance"
                           else if (p_value < 0.001) "highly significant"
                           else if (p_value < 0.05) "statistically significant"
                           else "not statistically significant"

            interpretation <- sprintf(
                "Higher %s expression is associated with %s %s risk of event (HR = %.2f). This finding is %s%s.",
                marker_name, magnitude, direction, hr, significance,
                if (!is.na(p_value)) sprintf(" (p = %.3f)", p_value) else ""
            )

            clinical_meaning <- if (!is.na(hr) && !is.na(p_value)) {
                if (hr > 1.5 && p_value < 0.05) {
                    "Consider this marker for risk stratification."
                } else if (hr < 0.67 && p_value < 0.05) {
                    "This marker may indicate favorable prognosis."
                } else {
                    "Clinical utility requires further validation."
                }
            } else {
                "Unable to determine clinical significance."
            }

            return(list(interpretation = interpretation, clinical = clinical_meaning))
        },

        # Generate copy-ready report sentence
        .generateReportSentence = function() {
            if (is.null(private$.risk_groups)) return("")

            n_samples <- nrow(self$data)
            n_events <- sum(self$data[[self$options$survivalEvent]] == 1, na.rm = TRUE)
            median_followup <- median(self$data[[self$options$survivalTime]], na.rm = TRUE)

            # Get log-rank p-value
            tryCatch({
                logrank_test <- survival::survdiff(private$.survival_object ~ private$.risk_groups)
                p_val <- 1 - pchisq(logrank_test$chisq, df = length(levels(private$.risk_groups)) - 1)

                # Get group survival summary
                group_summary <- private$.getGroupSurvivalSummary()

                report <- sprintf(
                    paste0("In this cohort of %d patients with %d events (%.1f%%) and median follow-up of %.1f months, ",
                           "IHC-based risk stratification using %s identified %d prognostic groups with %s ",
                           "different survival outcomes (log-rank p = %.3f). %s"),
                    n_samples, n_events, 100*n_events/n_samples, median_followup,
                    paste(self$options$markers, collapse = ", "),
                    length(levels(private$.risk_groups)),
                    if (p_val < 0.05) "significantly" else "non-significantly",
                    p_val,
                    group_summary
                )

                return(report)
            }, error = function(e) {
                return("Unable to generate report summary due to insufficient data.")
            })
        },

        # Get survival summary for groups
        .getGroupSurvivalSummary = function() {
            if (is.null(private$.risk_groups)) return("")

            group_summaries <- character(0)
            for (group in levels(private$.risk_groups)) {
                group_idx <- private$.risk_groups == group
                group_surv <- private$.survival_object[group_idx]

                tryCatch({
                    km_fit <- survival::survfit(group_surv ~ 1)
                    median_surv <- summary(km_fit)$table["median"]
                    if (!is.na(median_surv)) {
                        group_summaries <- c(group_summaries, sprintf("%s: %.1f months", group, median_surv))
                    }
                }, error = function(e) {
                    # Skip this group if error
                })
            }

            if (length(group_summaries) > 0) {
                return(paste("Median survival times were:", paste(group_summaries, collapse = "; "), "."))
            } else {
                return("Median survival times could not be estimated for all groups.")
            }
        },

        # Generate assumptions and caveats panel
        .generateAssumptionsPanel = function() {
            html <- "<h4>Analysis Assumptions & Caveats</h4><ul>"

            # Check proportional hazards
            if (self$options$coxRegression) {
                ph_warning <- ""
                # Simple PH check for risk groups if available
                if (!is.null(private$.risk_groups)) {
                    tryCatch({
                        cox_test <- survival::coxph(private$.survival_object ~ private$.risk_groups)
                        ph_test <- survival::cox.zph(cox_test)
                        if (ph_test$table["GLOBAL", "p"] < 0.05) {
                            ph_warning <- " <span style='color: orange;'>Warning: Proportional hazards assumption may be violated (p = %.3f).</span>"
                            ph_warning <- sprintf(ph_warning, ph_test$table["GLOBAL", "p"])
                        }
                    }, error = function(e) {
                        # Skip PH test if error
                    })
                }
                html <- paste0(html, "<li><b>Proportional Hazards:</b> Cox regression assumes hazards are proportional over time.", ph_warning, " Consider stratification if this assumption is violated.</li>")
            }

            # Check sample size
            n_events <- sum(self$data[[self$options$survivalEvent]] == 1, na.rm = TRUE)
            events_per_variable <- n_events / max(1, length(self$options$markers))
            if (events_per_variable < 10) {
                html <- paste0(html, sprintf(
                    "<li style='color: orange;'><b>Sample Size Warning:</b> Only %.1f events per variable (recommend ≥10). Results may be unstable.</li>",
                    events_per_variable
                ))
            }

            # Check censoring pattern
            censoring_rate <- 1 - (n_events / nrow(self$data))
            if (censoring_rate > 0.5) {
                html <- paste0(html, sprintf(
                    "<li><b>High Censoring:</b> %.1f%% of cases are censored. Consider competing risks analysis if appropriate.</li>",
                    100 * censoring_rate
                ))
            }

            # Check follow-up time
            max_time <- max(self$data[[self$options$survivalTime]], na.rm = TRUE)
            min_time <- min(self$data[[self$options$survivalTime]], na.rm = TRUE)
            if (max_time / min_time > 10) {
                html <- paste0(html, "<li><b>Variable Follow-up:</b> Wide range in follow-up times may affect interpretation.</li>")
            }

            # Multi-region specific assumptions
            if (self$options$multiRegionAnalysis && length(self$options$centralRegion) > 0) {
                html <- paste0(html, "<li><b>Multi-region Analysis:</b> Assumes central and invasive regions represent biologically distinct tumor compartments.</li>")
            }

            html <- paste0(html, "</ul>")
            return(html)
        },

        # Update assumptions panel
        .updateAssumptionsPanel = function() {
            assumptions_html <- private$.generateAssumptionsPanel()
            self$results$assumptions$setContent(assumptions_html)
        },

        # Update report panel
        .updateReportPanel = function() {
            report_text <- private$.generateReportSentence()
            report_html <- sprintf(
                "<h4>Clinical Report Summary</h4><div style='background-color: #f8f9fa; padding: 15px; border-left: 4px solid #007bff; margin: 10px 0;'>%s</div><p><small><i>Click to select and copy the text above for use in your reports.</i></small></p>",
                report_text
            )
            self$results$reportSentence$setContent(report_html)
        },

        .validateMultiRegionSetup = function() {
            if (length(self$options$centralRegion) == 0 || length(self$options$invasiveRegion) == 0) {
                self$results$instructions$setContent(
                    "<p style='color: orange;'><b>Multi-region Analysis:</b> Please select markers for both central and invasive regions.</p>"
                )
                return()
            }

            # Check if marker counts match
            if (length(self$options$centralRegion) != length(self$options$invasiveRegion)) {
                self$results$instructions$setContent(sprintf(
                    "<p style='color: orange;'><b>Multi-region Analysis Warning:</b> Different number of markers selected (Central: %d, Invasive: %d). For optimal comparison, consider selecting the same markers for both regions.</p>",
                    length(self$options$centralRegion), length(self$options$invasiveRegion)
                ))
            }

            # Check for data availability with expected naming conventions
            central_markers <- self$options$centralRegion
            invasive_markers <- self$options$invasiveRegion
            missing_markers <- character(0)

            for (marker in c(central_markers, invasive_markers)) {
                # Check if marker exists as-is or with regional suffixes
                marker_exists <- any(c(
                    marker %in% names(self$data),
                    paste0(marker, "_central") %in% names(self$data),
                    paste0(marker, "_invasive") %in% names(self$data)
                ))

                if (!marker_exists) {
                    missing_markers <- c(missing_markers, marker)
                }
            }

            if (length(missing_markers) > 0) {
                self$results$instructions$setContent(sprintf(
                    "<p style='color: red;'><b>Multi-region Analysis Error:</b> Missing marker data: %s. Expected naming: 'marker_central' and 'marker_invasive' or exact marker names.</p>",
                    paste(missing_markers, collapse = ", ")
                ))
            }
        }
    )
)