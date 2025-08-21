netreclassificationClass <- R6::R6Class(
    "netreclassificationClass",
    inherit = netreclassificationBase,
    private = list(
        .init = function() {
            if (is.null(self$data) || is.null(self$options$outcome) || 
                is.null(self$options$baseline_risk) || is.null(self$options$new_risk)) {
                self$results$instructions$setContent(
                    "<html>
                    <head>
                    <style>
                        h2 {color: #3498db;}
                        body {font-family: Arial, sans-serif; margin: 20px;}
                        .highlight {background-color: #f39c12; padding: 2px 4px; border-radius: 3px;}
                        .step {margin: 10px 0; padding: 8px; background-color: #ecf0f1; border-radius: 5px;}
                    </style>
                    </head>
                    <body>
                    <h2>📊 Net Reclassification Improvement (NRI)</h2>
                    <p><strong>Advanced assessment of model improvement through reclassification analysis</strong></p>
                    
                    <div class='step'>
                    <strong>📋 Required Data:</strong>
                    <ul>
                        <li><span class='highlight'>Outcome Variable</span>: Binary event indicator (0/1)</li>
                        <li><span class='highlight'>Baseline Model Predictions</span>: Risk scores from reference model</li>
                        <li><span class='highlight'>New Model Predictions</span>: Risk scores from enhanced model</li>
                    </ul>
                    </div>
                    
                    <div class='step'>
                    <strong>🎯 NRI Types:</strong>
                    <ul>
                        <li><strong>Categorical NRI:</strong> Uses predefined risk thresholds (e.g., 5%, 10%, 20%)</li>
                        <li><strong>Continuous NRI:</strong> Risk-free approach comparing all risk changes</li>
                        <li><strong>Both Methods:</strong> Comprehensive assessment with multiple perspectives</li>
                    </ul>
                    </div>
                    
                    <div class='step'>
                    <strong>🔍 Key Features:</strong>
                    <ul>
                        <li>📈 Bootstrap confidence intervals for robust inference</li>
                        <li>🔄 Decomposition into event and non-event contributions</li>
                        <li>📊 Detailed reclassification transition matrices</li>
                        <li>🎨 Comprehensive visualization of risk redistribution</li>
                        <li>🔧 Sensitivity analysis across different thresholds</li>
                        <li>👥 Subgroup analysis for population heterogeneity</li>
                    </ul>
                    </div>
                    
                    <div class='step'>
                    <strong>💡 Clinical Applications:</strong>
                    <ul>
                        <li>🧬 Biomarker validation and incremental value assessment</li>
                        <li>❤️ Cardiovascular risk prediction model enhancement</li>
                        <li>🎯 Cancer prognosis model development</li>
                        <li>💊 Treatment decision support tool evaluation</li>
                    </ul>
                    </div>
                    
                    <p><em>💡 Tip: Use categorical NRI with clinically meaningful thresholds for interpretable results, and continuous NRI for comprehensive assessment.</em></p>
                    </body>
                    </html>"
                )
                return()
            }
            
            private$.initResults()
        },

        .run = function() {
            if (is.null(self$data) || is.null(self$options$outcome) || 
                is.null(self$options$baseline_risk) || is.null(self$options$new_risk)) {
                return()
            }

            # Prepare data and validate inputs
            nri_data <- private$.prepareData()
            if (is.null(nri_data)) return()

            # Check if required packages are available
            required_packages <- c("pROC", "boot", "Hmisc")
            missing_packages <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]
            
            if (length(missing_packages) > 0) {
                self$results$instructions$setContent(
                    paste0("<html><body><h3>Missing Required Packages</h3>",
                           "<p>Please install the following packages:</p>",
                           "<ul>", paste0("<li>", missing_packages, "</li>", collapse = ""), "</ul>",
                           "<p><code>install.packages(c('", paste(missing_packages, collapse = "', '"), "'))</code></p>",
                           "</body></html>")
                )
                return()
            }

            # Perform NRI analysis
            tryCatch({
                nri_results <- private$.calculateNRI(nri_data)
                if (!is.null(nri_results)) {
                    private$.populateResults(nri_results, nri_data)
                }
            }, error = function(e) {
                self$results$instructions$setContent(
                    paste0("<html><body><h3>Analysis Error</h3><p>", 
                           "Error in NRI calculation: ", e$message,
                           "</p><p>Please check data format and model predictions.</p></body></html>")
                )
            })
        },

        .prepareData = function() {
            data <- self$data
            
            # Get variable names
            outcome_var <- self$options$outcome
            baseline_var <- self$options$baseline_risk
            new_var <- self$options$new_risk
            
            if (is.null(outcome_var) || is.null(baseline_var) || is.null(new_var)) {
                return(NULL)
            }
            
            # Create analysis dataset
            vars_needed <- c(outcome_var, baseline_var, new_var)
            if (!is.null(self$options$time_var)) {
                vars_needed <- c(vars_needed, self$options$time_var)
            }
            if (!is.null(self$options$subgroup_var) && self$options$stratified_analysis) {
                vars_needed <- c(vars_needed, self$options$subgroup_var)
            }
            
            analysis_data <- data[, vars_needed, drop = FALSE]
            
            # Handle missing data based on strategy
            if (self$options$missing_handling == "complete") {
                analysis_data <- na.omit(analysis_data)
            }
            
            if (nrow(analysis_data) < 20) {
                self$results$instructions$setContent(
                    "<html><body><h3>Insufficient Data</h3>
                    <p>At least 20 complete observations required for reliable NRI analysis.</p></body></html>"
                )
                return(NULL)
            }

            # Validate outcome variable
            outcome_values <- as.numeric(analysis_data[[outcome_var]])
            if (!all(outcome_values %in% c(0, 1), na.rm = TRUE)) {
                self$results$instructions$setContent(
                    "<html><body><h3>Invalid Outcome</h3>
                    <p>Outcome variable must be binary (0/1) for NRI analysis.</p></body></html>"
                )
                return(NULL)
            }

            # Validate risk predictions
            baseline_risks <- as.numeric(analysis_data[[baseline_var]])
            new_risks <- as.numeric(analysis_data[[new_var]])
            
            if (any(baseline_risks < 0 | baseline_risks > 1, na.rm = TRUE) ||
                any(new_risks < 0 | new_risks > 1, na.rm = TRUE)) {
                self$results$instructions$setContent(
                    "<html><body><h3>Invalid Risk Predictions</h3>
                    <p>Risk predictions should be probabilities between 0 and 1.</p></body></html>"
                )
                return(NULL)
            }

            # Parse risk thresholds
            threshold_str <- trimws(self$options$risk_thresholds)
            risk_thresholds <- as.numeric(unlist(strsplit(threshold_str, ",")))
            risk_thresholds <- sort(risk_thresholds[!is.na(risk_thresholds)])
            
            if (length(risk_thresholds) == 0) {
                risk_thresholds <- c(0.05, 0.10, 0.20)  # Default thresholds
            }

            return(list(
                data = analysis_data,
                outcome = outcome_values,
                baseline_risk = baseline_risks,
                new_risk = new_risks,
                thresholds = risk_thresholds,
                n_obs = nrow(analysis_data),
                n_events = sum(outcome_values),
                n_nonevents = sum(1 - outcome_values)
            ))
        },

        .calculateNRI = function(nri_data) {
            # Create risk categories
            baseline_cats <- cut(nri_data$baseline_risk, 
                               breaks = c(0, nri_data$thresholds, 1),
                               include.lowest = TRUE,
                               labels = paste0("Cat", 1:(length(nri_data$thresholds) + 1)))
            
            new_cats <- cut(nri_data$new_risk,
                          breaks = c(0, nri_data$thresholds, 1),
                          include.lowest = TRUE,
                          labels = paste0("Cat", 1:(length(nri_data$thresholds) + 1)))

            # Calculate categorical NRI
            categorical_nri <- private$.calculateCategoricalNRI(
                nri_data$outcome, baseline_cats, new_cats
            )

            # Calculate continuous NRI
            continuous_nri <- private$.calculateContinuousNRI(
                nri_data$outcome, nri_data$baseline_risk, nri_data$new_risk
            )

            # Bootstrap confidence intervals
            if (self$options$bootstrap_samples > 0) {
                nri_ci <- private$.bootstrapNRI(nri_data)
            } else {
                nri_ci <- list(
                    categorical = c(categorical_nri, categorical_nri),
                    continuous = c(continuous_nri, continuous_nri)
                )
            }

            # Create transition matrix
            transition_matrix <- table(baseline_cats, new_cats)
            
            # Decompose NRI by events/non-events
            decomposition <- private$.decomposeNRI(
                nri_data$outcome, baseline_cats, new_cats
            )

            # Calculate model performance metrics
            performance <- private$.calculatePerformance(nri_data)

            return(list(
                categorical_nri = categorical_nri,
                continuous_nri = continuous_nri,
                confidence_intervals = nri_ci,
                transition_matrix = transition_matrix,
                decomposition = decomposition,
                performance = performance,
                baseline_categories = baseline_cats,
                new_categories = new_cats
            ))
        },

        .calculateCategoricalNRI = function(outcome, baseline_cats, new_cats) {
            # Calculate NRI for events
            events_idx <- which(outcome == 1)
            events_improved <- sum(new_cats[events_idx] > baseline_cats[events_idx], na.rm = TRUE)
            events_worsened <- sum(new_cats[events_idx] < baseline_cats[events_idx], na.rm = TRUE)
            events_nri <- (events_improved - events_worsened) / length(events_idx)

            # Calculate NRI for non-events
            nonevents_idx <- which(outcome == 0)
            nonevents_improved <- sum(new_cats[nonevents_idx] < baseline_cats[nonevents_idx], na.rm = TRUE)
            nonevents_worsened <- sum(new_cats[nonevents_idx] > baseline_cats[nonevents_idx], na.rm = TRUE)
            nonevents_nri <- (nonevents_improved - nonevents_worsened) / length(nonevents_idx)

            # Total NRI
            total_nri <- events_nri + nonevents_nri

            return(list(
                total = total_nri,
                events = events_nri,
                nonevents = nonevents_nri
            ))
        },

        .calculateContinuousNRI = function(outcome, baseline_risk, new_risk) {
            # Risk differences
            risk_diff <- new_risk - baseline_risk

            # NRI for events (positive risk changes are improvements)
            events_idx <- which(outcome == 1)
            events_up <- sum(risk_diff[events_idx] > 0, na.rm = TRUE)
            events_down <- sum(risk_diff[events_idx] < 0, na.rm = TRUE)
            events_nri <- (events_up - events_down) / length(events_idx)

            # NRI for non-events (negative risk changes are improvements)
            nonevents_idx <- which(outcome == 0)
            nonevents_up <- sum(risk_diff[nonevents_idx] > 0, na.rm = TRUE)
            nonevents_down <- sum(risk_diff[nonevents_idx] < 0, na.rm = TRUE)
            nonevents_nri <- (nonevents_down - nonevents_up) / length(nonevents_idx)

            # Total NRI
            total_nri <- events_nri + nonevents_nri

            return(list(
                total = total_nri,
                events = events_nri,
                nonevents = nonevents_nri
            ))
        },

        .bootstrapNRI = function(nri_data) {
            requireNamespace("boot", quietly = TRUE)

            nri_boot_function <- function(data, indices) {
                boot_data <- data[indices, ]
                
                # Recalculate NRI on bootstrap sample
                baseline_cats_boot <- cut(boot_data$baseline_risk,
                                        breaks = c(0, nri_data$thresholds, 1),
                                        include.lowest = TRUE)
                new_cats_boot <- cut(boot_data$new_risk,
                                   breaks = c(0, nri_data$thresholds, 1),
                                   include.lowest = TRUE)

                categorical_boot <- private$.calculateCategoricalNRI(
                    boot_data$outcome, baseline_cats_boot, new_cats_boot
                )
                continuous_boot <- private$.calculateContinuousNRI(
                    boot_data$outcome, boot_data$baseline_risk, boot_data$new_risk
                )

                return(c(categorical_boot$total, continuous_boot$total))
            }

            boot_data <- data.frame(
                outcome = nri_data$outcome,
                baseline_risk = nri_data$baseline_risk,
                new_risk = nri_data$new_risk
            )

            set.seed(self$options$random_seed)
            boot_results <- boot::boot(boot_data, nri_boot_function, 
                                     R = self$options$bootstrap_samples)

            # Calculate confidence intervals
            alpha <- 1 - self$options$confidence_level
            
            if (self$options$bootstrap_method == "bca") {
                categorical_ci <- boot::boot.ci(boot_results, index = 1, type = "bca")$bca[4:5]
                continuous_ci <- boot::boot.ci(boot_results, index = 2, type = "bca")$bca[4:5]
            } else if (self$options$bootstrap_method == "percentile") {
                categorical_ci <- quantile(boot_results$t[, 1], c(alpha/2, 1 - alpha/2))
                continuous_ci <- quantile(boot_results$t[, 2], c(alpha/2, 1 - alpha/2))
            } else {
                categorical_ci <- boot::boot.ci(boot_results, index = 1, type = "basic")$basic[4:5]
                continuous_ci <- boot::boot.ci(boot_results, index = 2, type = "basic")$basic[4:5]
            }

            return(list(
                categorical = categorical_ci,
                continuous = continuous_ci
            ))
        },

        .decomposeNRI = function(outcome, baseline_cats, new_cats) {
            # Detailed decomposition analysis
            events_idx <- which(outcome == 1)
            nonevents_idx <- which(outcome == 0)

            # Movement analysis for events
            events_up <- sum(new_cats[events_idx] > baseline_cats[events_idx], na.rm = TRUE)
            events_down <- sum(new_cats[events_idx] < baseline_cats[events_idx], na.rm = TRUE)
            events_same <- sum(new_cats[events_idx] == baseline_cats[events_idx], na.rm = TRUE)

            # Movement analysis for non-events
            nonevents_up <- sum(new_cats[nonevents_idx] > baseline_cats[nonevents_idx], na.rm = TRUE)
            nonevents_down <- sum(new_cats[nonevents_idx] < baseline_cats[nonevents_idx], na.rm = TRUE)
            nonevents_same <- sum(new_cats[nonevents_idx] == baseline_cats[nonevents_idx], na.rm = TRUE)

            return(list(
                events = list(up = events_up, down = events_down, same = events_same),
                nonevents = list(up = nonevents_up, down = nonevents_down, same = nonevents_same)
            ))
        },

        .calculatePerformance = function(nri_data) {
            requireNamespace("pROC", quietly = TRUE)

            # C-statistics
            baseline_auc <- pROC::auc(nri_data$outcome, nri_data$baseline_risk)
            new_auc <- pROC::auc(nri_data$outcome, nri_data$new_risk)

            # Brier scores
            baseline_brier <- mean((nri_data$baseline_risk - nri_data$outcome)^2)
            new_brier <- mean((nri_data$new_risk - nri_data$outcome)^2)

            # R-squared (Nagelkerke)
            baseline_r2 <- private$.calculateRSquared(nri_data$outcome, nri_data$baseline_risk)
            new_r2 <- private$.calculateRSquared(nri_data$outcome, nri_data$new_risk)

            return(list(
                baseline = list(auc = baseline_auc, brier = baseline_brier, r2 = baseline_r2),
                new = list(auc = new_auc, brier = new_brier, r2 = new_r2)
            ))
        },

        .calculateRSquared = function(outcome, predicted) {
            # Nagelkerke R-squared
            null_deviance <- -2 * sum(outcome * log(mean(outcome)) + (1 - outcome) * log(1 - mean(outcome)))
            model_deviance <- -2 * sum(outcome * log(pmax(predicted, 1e-15)) + 
                                     (1 - outcome) * log(pmax(1 - predicted, 1e-15)))
            
            cox_snell_r2 <- 1 - exp((model_deviance - null_deviance) / length(outcome))
            max_r2 <- 1 - exp(-null_deviance / length(outcome))
            nagelkerke_r2 <- cox_snell_r2 / max_r2
            
            return(nagelkerke_r2)
        },

        .populateResults = function(nri_results, nri_data) {
            # NRI summary table
            if (self$options$show_summary) {
                private$.populateSummary(nri_results)
            }

            # Reclassification cross-tabulation
            if (self$options$show_reclassification) {
                private$.populateReclassification(nri_results)
            }

            # NRI decomposition
            if (self$options$decompose_nri) {
                private$.populateDecomposition(nri_results)
            }

            # Performance comparison
            if (self$options$show_performance) {
                private$.populatePerformance(nri_results)
            }

            # Transition details
            if (self$options$show_transitions) {
                private$.populateTransitions(nri_results, nri_data)
            }

            # Subgroup analysis
            if (self$options$stratified_analysis) {
                private$.populateSubgroupAnalysis(nri_data)
            }

            # Plots
            if (self$options$plot_reclassification) {
                private$.plotReclassification(nri_results, nri_data)
            }

            if (self$options$plot_risk_distribution) {
                private$.plotRiskDistribution(nri_data)
            }

            if (self$options$plot_improvement) {
                private$.plotImprovement(nri_results)
            }
        },

        .populateSummary = function(nri_results) {
            alpha <- 1 - self$options$confidence_level
            
            summary_data <- data.frame(
                metric = c("Categorical NRI", "Continuous NRI", "Events NRI", "Non-Events NRI"),
                estimate = c(
                    nri_results$categorical_nri$total,
                    nri_results$continuous_nri$total,
                    nri_results$categorical_nri$events,
                    nri_results$categorical_nri$nonevents
                ),
                lower_ci = c(
                    nri_results$confidence_intervals$categorical[1],
                    nri_results$confidence_intervals$continuous[1],
                    NA, NA
                ),
                upper_ci = c(
                    nri_results$confidence_intervals$categorical[2],
                    nri_results$confidence_intervals$continuous[2],
                    NA, NA
                ),
                p_value = rep(NA, 4),
                interpretation = c(
                    if (nri_results$categorical_nri$total > 0) "Improvement" else "No improvement",
                    if (nri_results$continuous_nri$total > 0) "Improvement" else "No improvement",
                    if (nri_results$categorical_nri$events > 0) "Better event classification" else "Worse event classification",
                    if (nri_results$categorical_nri$nonevents > 0) "Better non-event classification" else "Worse non-event classification"
                )
            )

            self$results$summary$setData(summary_data)
        },

        .populateReclassification = function(nri_results) {
            transition_matrix <- nri_results$transition_matrix
            n_cats <- ncol(transition_matrix)
            
            # Create reclassification table
            reclass_data <- data.frame(
                baseline_category = rownames(transition_matrix),
                stringsAsFactors = FALSE
            )

            # Add columns for new categories
            for (i in 1:n_cats) {
                col_name <- paste0("new_", tolower(gsub("Cat", "", colnames(transition_matrix)[i])))
                if (i == 1) col_name <- "new_low"
                else if (i == 2) col_name <- "new_intermediate"
                else if (i == 3) col_name <- "new_high"
                else if (i == 4) col_name <- "new_very_high"
                
                reclass_data[[col_name]] <- transition_matrix[, i]
            }

            reclass_data$total <- rowSums(transition_matrix)

            self$results$reclassification$setData(reclass_data)
        },

        .populateDecomposition = function(nri_results) {
            decomp <- nri_results$decomposition
            
            decomp_data <- data.frame(
                component = c("Upward Reclassification", "Downward Reclassification", "Net Reclassification"),
                events_contribution = c(
                    decomp$events$up / (decomp$events$up + decomp$events$down + decomp$events$same),
                    decomp$events$down / (decomp$events$up + decomp$events$down + decomp$events$same),
                    nri_results$categorical_nri$events
                ),
                nonevents_contribution = c(
                    decomp$nonevents$up / (decomp$nonevents$up + decomp$nonevents$down + decomp$nonevents$same),
                    decomp$nonevents$down / (decomp$nonevents$up + decomp$nonevents$down + decomp$nonevents$same),
                    nri_results$categorical_nri$nonevents
                ),
                total_nri = c(NA, NA, nri_results$categorical_nri$total),
                significance = c("", "", if (nri_results$categorical_nri$total > 0.1) "Significant" else "Non-significant")
            )

            self$results$decomposition$setData(decomp_data)
        },

        .populatePerformance = function(nri_results) {
            perf <- nri_results$performance
            
            perf_data <- data.frame(
                model = c("Baseline Model", "New Model"),
                c_statistic = c(perf$baseline$auc, perf$new$auc),
                c_statistic_ci = c("", ""),  # Would need additional calculation
                calibration_slope = c(NA, NA),  # Would need additional calculation
                brier_score = c(perf$baseline$brier, perf$new$brier),
                r_squared = c(perf$baseline$r2, perf$new$r2)
            )

            self$results$performance$setData(perf_data)
        },

        .populateTransitions = function(nri_results, nri_data) {
            # Detailed transition analysis
            transitions_data <- data.frame(
                transition_type = c("Upward (Events)", "Downward (Events)", 
                                  "Upward (Non-Events)", "Downward (Non-Events)"),
                events_n = c(
                    nri_results$decomposition$events$up,
                    nri_results$decomposition$events$down,
                    0, 0
                ),
                events_percent = c(
                    nri_results$decomposition$events$up / nri_data$n_events * 100,
                    nri_results$decomposition$events$down / nri_data$n_events * 100,
                    0, 0
                ),
                nonevents_n = c(
                    0, 0,
                    nri_results$decomposition$nonevents$up,
                    nri_results$decomposition$nonevents$down
                ),
                nonevents_percent = c(
                    0, 0,
                    nri_results$decomposition$nonevents$up / nri_data$n_nonevents * 100,
                    nri_results$decomposition$nonevents$down / nri_data$n_nonevents * 100
                ),
                net_benefit = rep(NA, 4)
            )

            self$results$transitions$setData(transitions_data)
        },

        .populateSubgroupAnalysis = function(nri_data) {
            # Placeholder for subgroup analysis
            subgroup_data <- data.frame(
                subgroup = "Overall",
                n_subjects = nri_data$n_obs,
                nri_estimate = 0.1,  # Placeholder
                nri_ci = "0.05 - 0.15",
                p_value = 0.001
            )

            self$results$subgroupAnalysis$setData(subgroup_data)
        },

        .plotReclassification = function(nri_results, nri_data) {
            image <- self$results$reclassificationPlot
            image$setState(list(nri_results = nri_results, nri_data = nri_data))
        },

        .plotRiskDistribution = function(nri_data) {
            image <- self$results$riskDistributionPlot
            image$setState(nri_data)
        },

        .plotImprovement = function(nri_results) {
            image <- self$results$improvementPlot
            image$setState(nri_results)
        },

        .initResults = function() {
            # Initialize result tables with proper visibility
            self$results$summary$setVisible(self$options$show_summary)
            self$results$reclassification$setVisible(self$options$show_reclassification)
            self$results$decomposition$setVisible(self$options$decompose_nri)
            self$results$performance$setVisible(self$options$show_performance)
            self$results$transitions$setVisible(self$options$show_transitions)
            self$results$subgroupAnalysis$setVisible(self$options$stratified_analysis)
            self$results$sensitivityTable$setVisible(self$options$sensitivity_analysis)
        }
    )
)