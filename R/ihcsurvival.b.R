#' @title IHC Survival Analysis
#' @importFrom R6 R6Class
#' @import jmvcore
#' @importFrom survival Surv survfit coxph
#' @importFrom cluster daisy
#' @import ggplot2

ihcsurvivalClass <- if (requireNamespace('jmvcore', quietly = TRUE)) R6::R6Class(
    "ihcsurvivalClass",
    inherit = ihcsurvivalBase,
    private = list(
        .ihc_matrix = NULL,
        .survival_object = NULL,
        .risk_groups = NULL,
        .cox_model = NULL,
        .prog_clusters = NULL,

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

            # Update summary
            private$.updateInstructions()
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
                events <- sum(as.numeric(group_surv)[seq(1, 2*n, 2)] == 1)  # Event indicator

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

                tryCatch({
                    cox_model <- survival::coxph(private$.survival_object ~ marker_data)

                    hr <- exp(coef(cox_model)[1])
                    ci <- exp(confint(cox_model))
                    p_val <- summary(cox_model)$coefficients[, "Pr(>|z|)"][1]

                    ci_text <- sprintf("%.3f - %.3f", ci[1], ci[2])
                    significance <- private$.getSignificance(p_val)

                    self$results$coxResults$addRow(
                        rowKey = marker,
                        values = list(
                            variable = marker,
                            hazard_ratio = hr,
                            hr_95ci = ci_text,
                            p_value = p_val,
                            significance = significance
                        )
                    )
                }, error = function(e) {
                    self$results$coxResults$addRow(
                        rowKey = marker,
                        values = list(
                            variable = marker,
                            hazard_ratio = NA,
                            hr_95ci = "Not estimable",
                            p_value = NA,
                            significance = ""
                        )
                    )
                })
            }

            # Risk group analysis
            if (!is.null(private$.risk_groups)) {
                tryCatch({
                    # Use lowest risk as reference
                    risk_levels <- levels(private$.risk_groups)
                    risk_groups_numeric <- as.numeric(private$.risk_groups)

                    cox_risk <- survival::coxph(private$.survival_object ~ risk_groups_numeric)

                    hr_risk <- exp(coef(cox_risk)[1])
                    ci_risk <- exp(confint(cox_risk))
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
                conf_ints <- exp(confint(multivar_model))

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

                    # Get survival at specific time points (e.g., 1 year from landmark)
                    time_points <- seq(12, 60, 12)  # 1-5 years

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

            # Basic plot (would be enhanced with ggplot2 in full implementation)
            plot(km_fit,
                 main = "Kaplan-Meier Survival Curves by Risk Group",
                 xlab = "Time",
                 ylab = "Survival Probability",
                 col = 1:length(levels(private$.risk_groups)),
                 lwd = 2)

            legend("topright",
                   legend = levels(private$.risk_groups),
                   col = 1:length(levels(private$.risk_groups)),
                   lwd = 2)

            TRUE
        },

        .plotHazardRatios = function(image, ...) {
            # Placeholder for forest plot
            plot(1, 1, main = "Hazard Ratio Forest Plot",
                 xlab = "Hazard Ratio", ylab = "Variables")
            TRUE
        },

        .plotRiskScores = function(image, ...) {
            # Placeholder for risk score distribution
            if (is.null(private$.risk_groups)) return()

            barplot(table(private$.risk_groups),
                   main = "Risk Group Distribution",
                   ylab = "Number of Patients",
                   col = rainbow(length(levels(private$.risk_groups))))
            TRUE
        },

        .getSignificance = function(p) {
            if (is.na(p)) return("")
            if (p < 0.001) return("***")
            if (p < 0.01) return("**")
            if (p < 0.05) return("*")
            return("ns")
        }
    )
)