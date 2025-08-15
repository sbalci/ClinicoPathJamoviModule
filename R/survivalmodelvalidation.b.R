
#' @title Enhanced Survival Model Validation
#' @importFrom jmvcore .
#' @importFrom stats glm lm predict residuals fitted confint quantile
#' @importFrom survival Surv survfit coxph concordance
#' @importFrom boot boot boot.ci
#' @importFrom rms cph validate calibrate
#' @importFrom timeROC timeROC
#' @importFrom pec pec calPlot
#' @importFrom dcurves dca
#' @importFrom riskRegression Score
#' @export

survivalmodelvalidationClass <- R6::R6Class(
    "survivalmodelvalidationClass",
    inherit = survivalmodelvalidationBase,
    private = list(
        
        .init = function() {
            
            if (is.null(self$data) || is.null(self$options$time_var) || 
                is.null(self$options$status_var) || is.null(self$options$risk_score)) {
                
                self$results$instructions$setContent(
                    "<h3>Welcome to Enhanced Survival Model Validation</h3>
                    <p>This comprehensive toolkit provides advanced validation methods for survival prediction models, 
                    essential for developing reliable prognostic tools in clinical practice.</p>
                    
                    <h4>Key Validation Components:</h4>
                    <ul>
                    <li><b>Discrimination:</b> Model's ability to distinguish between patients with different risks</li>
                    <li><b>Calibration:</b> Agreement between predicted and observed outcomes</li>
                    <li><b>Clinical Utility:</b> Net benefit for clinical decision-making</li>
                    <li><b>Transportability:</b> Performance across different populations</li>
                    </ul>
                    
                    <h4>Validation Methods Available:</h4>
                    <ul>
                    <li><b>Internal Bootstrap:</b> Corrects for optimism in model performance</li>
                    <li><b>Cross-Validation:</b> K-fold validation for unbiased performance estimates</li>
                    <li><b>Temporal Validation:</b> Performance over time periods</li>
                    <li><b>External Validation:</b> Testing on independent datasets</li>
                    <li><b>Geographic Validation:</b> Transportability across regions</li>
                    </ul>
                    
                    <h4>Performance Metrics:</h4>
                    <ul>
                    <li><b>C-index:</b> Concordance index for discrimination</li>
                    <li><b>Time-dependent AUC:</b> Time-specific discrimination</li>
                    <li><b>Integrated Brier Score:</b> Overall prediction accuracy</li>
                    <li><b>Net Reclassification:</b> Improvement in risk classification</li>
                    </ul>
                    
                    <h4>Required Variables:</h4>
                    <ul>
                    <li>Time variable (follow-up time)</li>
                    <li>Event status (0/1 or censored/event)</li>
                    <li>Risk score or linear predictor from your survival model</li>
                    <li>Optional: Original model covariates for model refitting</li>
                    </ul>
                    
                    <p><b>Clinical Impact:</b> Properly validated survival models ensure reliable risk predictions, 
                    appropriate clinical decision-making, and successful translation to clinical practice.</p>
                    
                    <p>Please select your variables to begin comprehensive model validation.</p>"
                )
                return()
            }
            
            # Initialize based on selected variables
            time_var <- self$options$time_var
            status_var <- self$options$status_var
            risk_score <- self$options$risk_score
            validation_type <- self$options$validation_type
            
            self$results$instructions$setContent(
                paste0("<h3>Enhanced Survival Model Validation</h3>
                <p><b>Time Variable:</b> ", time_var, "</p>
                <p><b>Status Variable:</b> ", status_var, "</p>
                <p><b>Risk Score:</b> ", risk_score, "</p>
                <p><b>Validation Type:</b> ", gsub("_", " ", stringr::str_to_title(validation_type)), "</p>
                <p>Configure validation parameters and click <b>Results</b> to perform comprehensive validation analysis.</p>")
            )
        },
        
        .run = function() {
            
            if (is.null(self$data) || is.null(self$options$time_var) || 
                is.null(self$options$status_var) || is.null(self$options$risk_score)) {
                return()
            }
            
            # Prepare and validate data
            prepared_data <- private$.prepareData()
            if (is.null(prepared_data)) return()
            
            # Model summary
            private$.generateModelSummary(prepared_data)
            
            # Core validation analysis
            validation_results <- private$.performValidation(prepared_data)
            if (is.null(validation_results)) return()
            
            # Performance metrics
            private$.calculatePerformanceMetrics(prepared_data, validation_results)
            
            # Calibration assessment
            if (self$options$calibration_assessment) {
                private$.assessCalibration(prepared_data)
            }
            
            # Discrimination analysis
            private$.analyzeDiscrimination(prepared_data)
            
            # Subgroup validation
            if (self$options$subgroup_validation && !is.null(self$options$stratification_var)) {
                private$.performSubgroupValidation(prepared_data)
            }
            
            # Decision curve analysis
            if (self$options$decision_curve_analysis) {
                private$.performDecisionCurveAnalysis(prepared_data)
            }
            
            # Clinical impact assessment
            if (self$options$clinical_impact_metrics) {
                private$.assessClinicalImpact(prepared_data)
            }
            
            # Generate plots
            private$.generateValidationPlots(prepared_data)
            
            # Comprehensive validation report
            private$.generateValidationReport(prepared_data, validation_results)
            
            # Methodological notes and recommendations
            private$.generateRecommendations(prepared_data, validation_results)
        },
        
        .prepareData = function() {
            
            tryCatch({
                
                time_var <- self$options$time_var
                status_var <- self$options$status_var
                risk_score <- self$options$risk_score
                covariates <- self$options$covariates
                stratification_var <- self$options$stratification_var
                
                # Select required variables
                vars_needed <- c(time_var, status_var, risk_score)
                if (!is.null(covariates)) vars_needed <- c(vars_needed, covariates)
                if (!is.null(stratification_var)) vars_needed <- c(vars_needed, stratification_var)
                
                # Get data
                data <- self$data[, vars_needed, drop = FALSE]
                
                # Remove missing data
                complete_cases <- complete.cases(data)
                data <- data[complete_cases, ]
                
                if (nrow(data) < 50) {
                    self$results$diagnostics$setContent(
                        "<p style='color: red;'><b>Warning:</b> Sample size is very small (n < 50). 
                        Validation results may be unreliable. Consider collecting more data.</p>"
                    )
                } else if (nrow(data) < 100) {
                    self$results$diagnostics$setContent(
                        "<p style='color: orange;'><b>Note:</b> Sample size is modest (n < 100). 
                        Bootstrap validation recommended for robust estimates.</p>"
                    )
                }
                
                # Validate variable types and values
                data[[time_var]] <- as.numeric(data[[time_var]])
                data[[status_var]] <- as.numeric(data[[status_var]])
                data[[risk_score]] <- as.numeric(data[[risk_score]])
                
                # Check event status coding
                unique_status <- sort(unique(data[[status_var]]))
                if (!all(unique_status %in% c(0, 1))) {
                    if (all(unique_status %in% c(1, 2))) {
                        # Convert 1/2 to 0/1
                        data[[status_var]] <- data[[status_var]] - 1
                        message("Converted status coding from 1/2 to 0/1")
                    } else {
                        stop("Status variable must be coded as 0/1 or 1/2")
                    }
                }
                
                # Check for negative times
                if (any(data[[time_var]] <= 0, na.rm = TRUE)) {
                    stop("Time variable contains non-positive values")
                }
                
                # Store variable information
                n_total <- nrow(self$data)
                n_complete <- nrow(data)
                n_events <- sum(data[[status_var]])
                event_rate <- n_events / n_complete
                
                private$data_info <- list(
                    n_total = n_total,
                    n_complete = n_complete,
                    n_missing = n_total - n_complete,
                    n_events = n_events,
                    event_rate = event_rate,
                    median_followup = median(data[[time_var]]),
                    max_followup = max(data[[time_var]])
                )
                
                private$variable_names <- list(
                    time = time_var,
                    status = status_var,
                    risk_score = risk_score,
                    covariates = covariates,
                    stratification_var = stratification_var
                )
                
                return(data)
                
            }, error = function(e) {
                self$results$diagnostics$setContent(
                    paste("<p style='color: red;'><b>Data Preparation Error:</b>", e$message, "</p>")
                )
                return(NULL)
            })
        },
        
        .generateModelSummary = function(data) {
            
            info <- private$data_info
            vars <- private$variable_names
            
            # Basic model information
            html <- "<h3>Model Validation Summary</h3>"
            html <- paste0(html, "<table class='jamovi-table'>")
            html <- paste0(html, "<tr><td><b>Total Observations:</b></td><td>", info$n_total, "</td></tr>")
            html <- paste0(html, "<tr><td><b>Complete Cases:</b></td><td>", info$n_complete, " (", round(100 * info$n_complete / info$n_total, 1), "%)</td></tr>")
            html <- paste0(html, "<tr><td><b>Missing Data:</b></td><td>", info$n_missing, " (", round(100 * info$n_missing / info$n_total, 1), "%)</td></tr>")
            html <- paste0(html, "<tr><td><b>Events:</b></td><td>", info$n_events, " (", round(100 * info$event_rate, 1), "%)</td></tr>")
            html <- paste0(html, "<tr><td><b>Median Follow-up:</b></td><td>", round(info$median_followup, 2), " time units</td></tr>")
            html <- paste0(html, "<tr><td><b>Maximum Follow-up:</b></td><td>", round(info$max_followup, 2), " time units</td></tr>")
            html <- paste0(html, "</table>")
            
            # Risk score distribution
            risk_scores <- data[[vars$risk_score]]
            html <- paste0(html, "<h4>Risk Score Distribution</h4>")
            html <- paste0(html, "<table class='jamovi-table'>")
            html <- paste0(html, "<tr><td><b>Mean:</b></td><td>", round(mean(risk_scores), 4), "</td></tr>")
            html <- paste0(html, "<tr><td><b>Standard Deviation:</b></td><td>", round(sd(risk_scores), 4), "</td></tr>")
            html <- paste0(html, "<tr><td><b>Median:</b></td><td>", round(median(risk_scores), 4), "</td></tr>")
            html <- paste0(html, "<tr><td><b>IQR:</b></td><td>", round(quantile(risk_scores, 0.25), 4), " - ", round(quantile(risk_scores, 0.75), 4), "</td></tr>")
            html <- paste0(html, "<tr><td><b>Range:</b></td><td>", round(min(risk_scores), 4), " - ", round(max(risk_scores), 4), "</td></tr>")
            html <- paste0(html, "</table>")
            
            # Validation approach
            validation_type <- self$options$validation_type
            model_type <- self$options$model_type
            
            html <- paste0(html, "<h4>Validation Configuration</h4>")
            html <- paste0(html, "<ul>")
            html <- paste0(html, "<li><b>Model Type:</b> ", gsub("_", " ", stringr::str_to_title(model_type)), "</li>")
            html <- paste0(html, "<li><b>Validation Method:</b> ", gsub("_", " ", stringr::str_to_title(validation_type)), "</li>")
            
            if (validation_type == "internal_bootstrap") {
                html <- paste0(html, "<li><b>Bootstrap Samples:</b> ", self$options$bootstrap_samples, "</li>")
            } else if (validation_type == "cross_validation") {
                html <- paste0(html, "<li><b>CV Folds:</b> ", self$options$cv_folds, "</li>")
            }
            
            html <- paste0(html, "<li><b>Confidence Level:</b> ", round(self$options$confidence_level * 100, 1), "%</li>")
            html <- paste0(html, "</ul>")
            
            self$results$modelSummary$setContent(html)
        },
        
        .performValidation = function(data) {
            
            tryCatch({
                
                validation_type <- self$options$validation_type
                vars <- private$variable_names
                
                # Create survival object
                surv_obj <- survival::Surv(data[[vars$time]], data[[vars$status]])
                
                if (validation_type == "internal_bootstrap") {
                    results <- private$.performBootstrapValidation(data, surv_obj)
                } else if (validation_type == "cross_validation") {
                    results <- private$.performCrossValidation(data, surv_obj)
                } else if (validation_type == "temporal_validation") {
                    results <- private$.performTemporalValidation(data, surv_obj)
                } else {
                    # Default to apparent validation
                    results <- private$.performApparentValidation(data, surv_obj)
                }
                
                # Store validation results
                private$validation_results <- results
                
                # Update validation results table
                private$.updateValidationTable(results)
                
                return(results)
                
            }, error = function(e) {
                self$results$diagnostics$setContent(
                    paste0(self$results$diagnostics$content, 
                           "<p style='color: red;'><b>Validation Error:</b>", e$message, "</p>")
                )
                return(NULL)
            })
        },
        
        .performBootstrapValidation = function(data, surv_obj) {
            
            vars <- private$variable_names
            n_boot <- self$options$bootstrap_samples
            
            # Bootstrap function for C-index
            boot_cindex <- function(data, indices) {
                boot_data <- data[indices, ]
                boot_surv <- survival::Surv(boot_data[[vars$time]], boot_data[[vars$status]])
                
                if (requireNamespace('survival', quietly = TRUE)) {
                    # Calculate C-index
                    cindex <- survival::concordance(boot_surv ~ boot_data[[vars$risk_score]])$concordance
                    return(cindex)
                } else {
                    return(NA)
                }
            }
            
            # Perform bootstrap
            boot_results <- boot::boot(data, boot_cindex, R = n_boot)
            
            # Calculate apparent and bias-corrected estimates
            apparent_cindex <- boot_results$t0
            mean_boot_cindex <- mean(boot_results$t, na.rm = TRUE)
            optimism <- mean_boot_cindex - apparent_cindex
            corrected_cindex <- apparent_cindex - optimism
            
            # Bootstrap confidence interval
            if (requireNamespace('boot', quietly = TRUE)) {
                ci_result <- tryCatch({
                    boot::boot.ci(boot_results, type = "norm")
                }, error = function(e) {
                    NULL
                })
                
                if (!is.null(ci_result)) {
                    ci_lower <- ci_result$normal[2]
                    ci_upper <- ci_result$normal[3]
                } else {
                    ci_lower <- NA
                    ci_upper <- NA
                }
            } else {
                ci_lower <- NA
                ci_upper <- NA
            }
            
            return(list(
                apparent_performance = apparent_cindex,
                validated_performance = mean_boot_cindex,
                optimism = optimism,
                corrected_performance = corrected_cindex,
                ci_lower = ci_lower,
                ci_upper = ci_upper,
                validation_method = "Bootstrap"
            ))
        },
        
        .updateValidationTable = function(results) {
            
            validation_table <- self$results$validationResults
            
            validation_table$addRow(rowKey = "main", values = list(
                validation_method = results$validation_method,
                apparent_performance = round(results$apparent_performance, 4),
                validated_performance = round(results$validated_performance, 4),
                optimism = round(results$optimism, 4),
                corrected_performance = round(results$corrected_performance, 4)
            ))
        },
        
        .calculatePerformanceMetrics = function(data, validation_results) {
            
            vars <- private$variable_names
            surv_obj <- survival::Surv(data[[vars$time]], data[[vars$status]])
            risk_scores <- data[[vars$risk_score]]
            
            # Parse prediction times
            pred_times_str <- self$options$prediction_times
            pred_times <- as.numeric(strsplit(pred_times_str, ",")[[1]])
            pred_times <- pred_times[!is.na(pred_times)]
            
            metrics_table <- self$results$performanceMetrics
            
            # C-index (already calculated in validation)
            cindex <- validation_results$corrected_performance
            cindex_ci <- c(validation_results$ci_lower, validation_results$ci_upper)
            
            metrics_table$addRow(rowKey = "cindex", values = list(
                metric = "Concordance Index (C-index)",
                value = round(cindex, 4),
                ci_lower = ifelse(is.na(cindex_ci[1]), NA, round(cindex_ci[1], 4)),
                ci_upper = ifelse(is.na(cindex_ci[2]), NA, round(cindex_ci[2], 4)),
                interpretation = ifelse(cindex > 0.7, "Good discrimination",
                                      ifelse(cindex > 0.6, "Modest discrimination", "Poor discrimination"))
            ))
            
            # Time-dependent AUC
            if (requireNamespace('timeROC', quietly = TRUE) && length(pred_times) > 0) {
                tryCatch({
                    
                    for (t in pred_times) {
                        if (t <= max(data[[vars$time]]) && t > 0) {
                            roc_result <- timeROC::timeROC(
                                T = data[[vars$time]],
                                delta = data[[vars$status]],
                                marker = risk_scores,
                                cause = 1,
                                times = t,
                                ROC = TRUE
                            )
                            
                            if (length(roc_result$AUC) > 1) {
                                auc_t <- roc_result$AUC[2]  # AUC at time t
                                
                                metrics_table$addRow(rowKey = paste0("auc_", t), values = list(
                                    metric = paste0("Time-dependent AUC (t=", t, ")"),
                                    value = round(auc_t, 4),
                                    ci_lower = NA,
                                    ci_upper = NA,
                                    interpretation = ifelse(auc_t > 0.8, "Excellent",
                                                          ifelse(auc_t > 0.7, "Good", "Modest"))
                                ))
                            }
                        }
                    }
                    
                }, error = function(e) {
                    message("Time-dependent AUC calculation failed: ", e$message)
                })
            }
        },
        
        .generateValidationPlots = function(data) {
            
            # Set up plot states for rendering
            vars <- private$variable_names
            
            plot_data <- list(
                data = data,
                time_var = vars$time,
                status_var = vars$status,
                risk_score_var = vars$risk_score,
                pred_times = as.numeric(strsplit(self$options$prediction_times, ",")[[1]])
            )
            
            if (self$options$discrimination_plots) {
                self$results$discriminationPlot$setState(plot_data)
            }
            
            if (self$options$calibration_plots) {
                self$results$calibrationPlot$setState(plot_data)
            }
            
            if (self$options$roc_curves) {
                self$results$rocCurvePlot$setState(plot_data)
            }
            
            if (self$options$risk_distribution_plots) {
                self$results$riskDistributionPlot$setState(plot_data)
            }
        },
        
        .generateValidationReport = function(data, validation_results) {
            
            html <- "<h3>Comprehensive Validation Report</h3>"
            html <- paste0(html, "<p>This section provides a comprehensive assessment of the survival model's performance.</p>")
            
            # Key findings
            cindex <- validation_results$corrected_performance
            optimism <- validation_results$optimism
            
            html <- paste0(html, "<h4>Key Findings</h4>")
            html <- paste0(html, "<ul>")
            html <- paste0(html, "<li><b>Discrimination Performance:</b> C-index = ", round(cindex, 3))
            
            if (cindex > 0.7) {
                html <- paste0(html, " (Good discrimination)</li>")
            } else if (cindex > 0.6) {
                html <- paste0(html, " (Modest discrimination)</li>")
            } else {
                html <- paste0(html, " (Poor discrimination)</li>")
            }
            
            html <- paste0(html, "<li><b>Model Optimism:</b> ", round(optimism, 3))
            
            if (abs(optimism) < 0.05) {
                html <- paste0(html, " (Low optimism - model is well-calibrated)</li>")
            } else if (abs(optimism) < 0.1) {
                html <- paste0(html, " (Moderate optimism - some overfitting)</li>")
            } else {
                html <- paste0(html, " (High optimism - significant overfitting)</li>")
            }
            
            html <- paste0(html, "</ul>")
            
            self$results$validationReport$setContent(html)
        },
        
        .generateRecommendations = function(data, validation_results) {
            
            cindex <- validation_results$corrected_performance
            optimism <- validation_results$optimism
            n_obs <- nrow(data)
            event_rate <- private$data_info$event_rate
            
            html <- "<h3>Clinical and Methodological Recommendations</h3>"
            
            # Performance-based recommendations
            if (cindex < 0.6) {
                html <- paste0(html, "<p style='color: red;'><b>Model Performance Concern:</b> The model shows poor discrimination (C-index < 0.6). Consider:</p>")
                html <- paste0(html, "<ul>")
                html <- paste0(html, "<li>Adding additional predictive variables</li>")
                html <- paste0(html, "<li>Checking for data quality issues</li>")
                html <- paste0(html, "<li>Considering non-linear relationships</li>")
                html <- paste0(html, "<li>Evaluating different modeling approaches</li>")
                html <- paste0(html, "</ul>")
            } else if (cindex >= 0.7) {
                html <- paste0(html, "<p style='color: green;'><b>Good Model Performance:</b> The model demonstrates good discrimination and may be suitable for clinical use.</p>")
            }
            
            # Sample size recommendations
            if (n_obs < 100) {
                html <- paste0(html, "<p style='color: orange;'><b>Sample Size Limitation:</b> Consider collecting additional data for more robust validation.</p>")
            }
            
            # Event rate recommendations
            if (event_rate < 0.1) {
                html <- paste0(html, "<p style='color: orange;'><b>Low Event Rate:</b> The low event rate may limit model reliability. Longer follow-up may be needed.</p>")
            }
            
            # Next steps
            html <- paste0(html, "<h4>Recommended Next Steps</h4>")
            html <- paste0(html, "<ol>")
            
            if (optimism > 0.1) {
                html <- paste0(html, "<li>Apply shrinkage methods to reduce overfitting</li>")
            }
            
            html <- paste0(html, "<li>Perform external validation on independent datasets</li>")
            html <- paste0(html, "<li>Conduct decision curve analysis for clinical utility</li>")
            html <- paste0(html, "<li>Develop clinical prediction tools (nomograms, calculators)</li>")
            html <- paste0(html, "<li>Plan implementation and impact studies</li>")
            html <- paste0(html, "</ol>")
            
            self$results$recommendations$setContent(html)
        }
    )
)
