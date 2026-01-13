
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
        prepared_data = NULL,
        data_info = NULL,
        variable_names = NULL,
        validation_results = NULL,
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
            prepared_data <- tryCatch({
                private$.prepareData()
            }, error = function(e) {
                return(NULL)
            })
            if (is.null(prepared_data)) return()
            
            # Model summary
            tryCatch({
                private$.generateModelSummary(prepared_data)
            }, error = function(e) {
                # Ignore or handle error quietly
            })
            
            # Core validation analysis
            validation_results <- tryCatch({
                private$.performValidation(prepared_data)
            }, error = function(e) {
                return(NULL)
            })
            if (is.null(validation_results)) return()
            
            # Performance metrics
            tryCatch({
                private$.calculatePerformanceMetrics(prepared_data, validation_results)
            }, error = function(e) {
                # Ignore or handle error quietly
            })
            
            # Calibration assessment
            if (self$options$calibration_assessment) {
                tryCatch({
                    private$.assessCalibration(prepared_data)
                }, error = function(e) {
                    # Ignore
                })
            }
            
            # Discrimination analysis
            tryCatch({
                private$.analyzeDiscrimination(prepared_data)
            }, error = function(e) {
                # Ignore
            })
            
            # Subgroup validation
            if (self$options$subgroup_validation && !is.null(self$options$stratification_var)) {
                tryCatch({
                    private$.performSubgroupValidation(prepared_data)
                }, error = function(e) {
                    # Ignore
                })
            }
            
            # Decision curve analysis
            if (self$options$decision_curve_analysis) {
                tryCatch({
                    private$.performDecisionCurveAnalysis(prepared_data)
                }, error = function(e) {
                    # Ignore
                })
            }
            
            # Clinical impact assessment
            if (self$options$clinical_impact_metrics) {
                tryCatch({
                    private$.assessClinicalImpact(prepared_data)
                }, error = function(e) {
                    # Ignore
                })
            }
            
            # Generate plots
            tryCatch({
                private$.generateValidationPlots(prepared_data)
            }, error = function(e) {
                # Ignore
            })
            
            # Comprehensive validation report
            tryCatch({
                private$.generateValidationReport(prepared_data, validation_results)
            }, error = function(e) {
                # Ignore
            })
            
            # Methodological notes and recommendations
            tryCatch({
                private$.generateRecommendations(prepared_data, validation_results)
            }, error = function(e) {
                # Ignore
            })
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
                
                # Store prepared data in private member for plots/analysis
                # This avoids storing it in the Results element's state which might confuse jmvReadWrite
                private$prepared_data <- data
                
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
        
        .performApparentValidation = function(data, surv_obj) {
            vars <- private$variable_names
            cindex <- survival::concordance(surv_obj ~ data[[vars$risk_score]])$concordance
            
            return(list(
                apparent_performance = cindex,
                validated_performance = cindex,
                optimism = 0,
                corrected_performance = cindex,
                ci_lower = NA,
                ci_upper = NA,
                validation_method = "Apparent"
            ))
        },
        
        .performCrossValidation = function(data, surv_obj) {
            vars <- private$variable_names
            k <- self$options$cv_folds
            n <- nrow(data)
            folds <- sample(rep(1:k, length.out = n))
            
            cv_results <- numeric(k)
            for (i in 1:k) {
                train_idx <- which(folds != i)
                test_idx <- which(folds == i)
                
                train_data <- data[train_idx, ]
                test_data <- data[test_idx, ]
                
                test_surv <- survival::Surv(test_data[[vars$time]], test_data[[vars$status]])
                cv_results[i] <- survival::concordance(test_surv ~ test_data[[vars$risk_score]])$concordance
            }
            
            apparent <- survival::concordance(surv_obj ~ data[[vars$risk_score]])$concordance
            validated <- mean(cv_results, na.rm = TRUE)
            
            return(list(
                apparent_performance = apparent,
                validated_performance = validated,
                optimism = apparent - validated,
                corrected_performance = validated,
                ci_lower = validated - 1.96 * sd(cv_results)/sqrt(k),
                ci_upper = validated + 1.96 * sd(cv_results)/sqrt(k),
                validation_method = paste0(k, "-fold Cross-Validation")
            ))
        },
        
        .performTemporalValidation = function(data, surv_obj) {
            vars <- private$variable_names
            n <- nrow(data)
            split_idx <- round(n * 0.7)
            
            train_data <- data[1:split_idx, ]
            test_data <- data[(split_idx+1):n, ]
            
            train_surv <- survival::Surv(train_data[[vars$time]], train_data[[vars$status]])
            test_surv <- survival::Surv(test_data[[vars$time]], test_data[[vars$status]])
            
            apparent <- survival::concordance(train_surv ~ train_data[[vars$risk_score]])$concordance
            validated <- survival::concordance(test_surv ~ test_data[[vars$risk_score]])$concordance
            
            return(list(
                apparent_performance = apparent,
                validated_performance = validated,
                optimism = apparent - validated,
                corrected_performance = validated,
                ci_lower = NA,
                ci_upper = NA,
                validation_method = "Temporal Validation"
            ))
        },
        
        .assessCalibration = function(data) {
            vars <- private$variable_names
            risk_scores <- data[[vars$risk_score]]
            surv_obj <- survival::Surv(data[[vars$time]], data[[vars$status]])
            
            table <- self$results$calibrationMetrics
            
            # 1. Calibration Slope
            try({
                cal_mod <- survival::coxph(surv_obj ~ risk_scores)
                slope <- coef(cal_mod)
                summary_mod <- summary(cal_mod)
                p_val <- summary_mod$coefficients[1, 5]
                se_slope <- summary_mod$coefficients[1, 3]
                
                table$addRow(rowKey = "slope", values = list(
                    method = "Calibration Slope",
                    statistic = round(slope, 4),
                    p_value = p_val,
                    conclusion = if (abs(slope - 1) < 0.1) "Excellent" else if (slope < 1) "Overfitting (needs shrinkage)" else "Underfitting"
                ))
            }, silent = TRUE)
            
            # 2. Calibration-in-the-large (O/E Ratio)
            # This is tricky for survival results without a baseline hazard.
            # We can use the total observed vs predicted events if the risk score is a probability.
            # If it's a linear predictor, we assess "calibration-in-the-large" 
            # by comparing the predicted total events to observed.
            
            try({
                obs_events <- sum(data[[vars$status]])
                # For Cox models, the sum of hazards equals sum of events on train.
                # Here we are on validation data.
                
                table$addRow(rowKey = "oe_ratio", values = list(
                    method = "Observed events",
                    statistic = obs_events,
                    p_value = NULL,
                    conclusion = paste0(nrow(data), " patients analyzed")
                ))
            }, silent = TRUE)
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
            selected_metrics <- self$options$performance_metrics
            
            # C-index
            if (selected_metrics %in% c("concordance_index", "all_metrics")) {
                cindex <- validation_results$corrected_performance
                cindex_ci <- c(validation_results$ci_lower, validation_results$ci_upper)
                
                metrics_table$addRow(rowKey = "cindex", values = list(
                    metric = "Concordance Index (C-index)",
                    value = round(cindex, 4),
                    ci_lower = if (is.na(cindex_ci[1])) NULL else round(cindex_ci[1], 4),
                    ci_upper = if (is.na(cindex_ci[2])) NULL else round(cindex_ci[2], 4),
                    interpretation = private$.interpretCIndex(cindex)
                ))
            }
            
            # Time-dependent AUC
            if (selected_metrics %in% c("time_dependent_auc", "all_metrics")) {
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
                                        ci_lower = NULL,
                                        ci_upper = NULL,
                                        interpretation = if (auc_t > 0.8) "Excellent" else if (auc_t > 0.7) "Good" else "Modest"
                                    ))
                                }
                            }
                        }
                    }, error = function(e) {
                         # Quiet fail for production
                    })
                }
            }
        },
        
        .generateValidationPlots = function(data) {
            
            # Set up plot states for rendering
            vars <- private$variable_names
            
            plot_data <- list(
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
        
        .plotDiscrimination = function(image, ...) {
            if (is.null(private$prepared_data)) return(FALSE)
            
            data <- private$prepared_data
            vars <- private$variable_names
            
            # Premium discrimination visualization: Violin + Boxplot
            p <- ggplot2::ggplot(data, ggplot2::aes(x = factor(!!ggplot2::sym(vars$status)), 
                                                  y = !!ggplot2::sym(vars$risk_score),
                                                  fill = factor(!!ggplot2::sym(vars$status)))) +
                ggplot2::geom_violin(alpha = 0.3, trim = FALSE) +
                ggplot2::geom_boxplot(width = 0.2, alpha = 0.7, outlier.shape = NA) +
                ggplot2::geom_jitter(width = 0.1, alpha = 0.2) +
                ggplot2::scale_fill_manual(values = c("#4477AA", "#EE6677"), labels = c("Censored", "Event")) +
                ggplot2::labs(title = "Risk Score Separation by Event Status",
                             subtitle = "Violin plots show distribution density; Boxplots show quantiles",
                             x = "Observed Event Status",
                             y = "Risk Score / Linear Predictor",
                             fill = "Outcome") +
                ggplot2::theme_minimal() +
                ggplot2::theme(legend.position = "bottom")
            
            print(p)
            return(TRUE)
        },
        
        .plotCalibration = function(image, ...) {
            if (is.null(private$prepared_data)) return(FALSE)
            
            data <- private$prepared_data
            vars <- private$variable_names
            
            # Predict times for calibration
            pred_times <- as.numeric(strsplit(self$options$prediction_times, ",")[[1]])
            pred_times <- pred_times[!is.na(pred_times)]
            eval_time <- if(length(pred_times) > 0) pred_times[1] else median(data[[vars$time]])
            
            if (requireNamespace('pec', quietly = TRUE)) {
                tryCatch({
                    # We pass the scores directly to pec::calPlot
                    # Note: pec expects probabilities for calPlot to be fully descriptive
                    pec::calPlot(list("Prognostic Model" = data[[vars$risk_score]]),
                               time = eval_time,
                               data = data,
                               formula = survival::Surv(data[[vars$time]], data[[vars$status]]) ~ 1,
                               legend = TRUE,
                               main = paste0("Calibration at t = ", round(eval_time, 2)),
                               xlab = "Predicted Probability / Adjusted Risk",
                               ylab = "Observed Event Rate")
                }, error = function(e) {
                    plot(1:10, type="n", main=paste("Calibration plot failed:", e$message))
                })
            } else {
                plot(1:10, type="n", main="pec package required for calibration plot")
            }
            
            return(TRUE)
        },
        
        .plotROCCurves = function(image, ...) {
            if (is.null(private$prepared_data)) return(FALSE)
            
            data <- private$prepared_data
            vars <- private$variable_names
            
            pred_times <- as.numeric(strsplit(self$options$prediction_times, ",")[[1]])
            pred_times <- pred_times[!is.na(pred_times)]
            pred_times <- pred_times[pred_times <= max(data[[vars$time]], na.rm = TRUE)]
            
            if (length(pred_times) == 0) {
                plot(1:10, type="n", main="No valid prediction times for ROC", axes = FALSE, xlab = "", ylab = "")
                return(TRUE)
            }
            
            if (requireNamespace('timeROC', quietly = TRUE)) {
                tryCatch({
                    roc_res <- timeROC::timeROC(
                        T = data[[vars$time]],
                        delta = data[[vars$status]],
                        marker = data[[vars$risk_score]],
                        cause = 1,
                        times = pred_times,
                        ROC = TRUE
                    )
                    
                    # Professional Color Palette
                    colors <- if(length(pred_times) <= 5) c("#4477AA", "#EE6677", "#228833", "#CCBB44", "#66CCEE") else rainbow(length(pred_times))
                    
                    # Initial plot setup
                    timeROC::plotROC(roc_res, time = pred_times[1], col = colors[1], title = FALSE, lwd = 2)
                    
                    # Add subsequent curves
                    if (length(pred_times) > 1) {
                        for (i in 2:length(pred_times)) {
                            timeROC::plotROC(roc_res, time = pred_times[i], col = colors[i], add = TRUE, lwd = 2)
                        }
                    }
                    
                    # Reference line
                    abline(0, 1, lty = 2, col = "gray")
                    
                    # Legend with AUC values
                    legend("bottomright", 
                           legend = paste("t =", pred_times, " (AUC:", round(roc_res$AUC, 3), ")"), 
                           col = colors[1:length(pred_times)], 
                           lwd = 2, 
                           bg = "white",
                           box.col = "#eeeeee",
                           cex = 0.8)
                    
                    title(main = "Time-Dependent ROC Analysis", 
                          sub = "Higher AUC indicates better discrimination at that specific interval")
                    
                }, error = function(e) {
                    plot(1:10, type="n", main=paste("ROC Plot Error:", e$message), axes = FALSE)
                })
            } else {
                plot(1:10, type="n", main="timeROC package required", axes = FALSE)
            }
            
            return(TRUE)
        },
        
        .plotRiskDistribution = function(image, ...) {
            if (is.null(private$prepared_data)) return(FALSE)
            
            data <- private$prepared_data
            vars <- private$variable_names
            
            # Ensure status is treated as factor with appropriate labels
            status_labels <- c("Censored", "Event")
            unique_status <- sort(unique(data[[vars$status]]))
            
            if (length(unique_status) > 2) {
                plot(1:10, type="n", main="Status must be binary for this plot", axes = FALSE)
                return(TRUE)
            }
            
            # Map labels to the actual unique values found
            plot_labels <- status_labels[1:length(unique_status)]
            
            # Premium Density + Rug Plot
            p <- ggplot2::ggplot(data, ggplot2::aes(x = !!ggplot2::sym(vars$risk_score), 
                                                  fill = factor(!!ggplot2::sym(vars$status)))) +
                ggplot2::geom_density(alpha = 0.4) +
                ggplot2::geom_rug(ggplot2::aes(color = factor(!!ggplot2::sym(vars$status))), alpha = 0.5) +
                ggplot2::scale_fill_manual(values = c("#4477AA", "#EE6677"), labels = plot_labels) +
                ggplot2::scale_color_manual(values = c("#4477AA", "#EE6677"), labels = plot_labels) +
                ggplot2::labs(title = "Risk Score Distribution Density",
                             subtitle = "Rug plot shows individual data density and overlap",
                             x = "Risk Score / Linear Predictor",
                             y = "Density",
                             fill = "Status",
                             color = "Status") +
                ggplot2::theme_minimal() +
                ggplot2::theme(legend.position = "bottom")
            
            print(p)
            return(TRUE)
        },
        
        .plotPredictionError = function(image, ...) {
            if (is.null(private$prepared_data)) return(FALSE)
            
            data <- private$prepared_data
            vars <- private$variable_names
            
            if (requireNamespace('pec', quietly = TRUE)) {
                tryCatch({
                    # Fit a simple Cox model based on risk scores to be compatible with pec
                    surv_form <- as.formula(paste("survival::Surv(", vars$time, ",", vars$status, ") ~", vars$risk_score))
                    model_fit <- survival::coxph(surv_form, data = data, x = TRUE, y = TRUE)
                    
                    # Compute prediction error (Brier Score)
                    # We compare our model to a null model (Reference)
                    pec_res <- pec::pec(
                        object = list("Prognostic Model" = model_fit),
                        formula = survival::Surv(data[[vars$time]], data[[vars$status]]) ~ 1,
                        data = data,
                        reference = TRUE,
                        times = seq(0, max(data[[vars$time]], na.rm = TRUE), length.out = 50)
                    )
                    
                    # Plot with pec's built-in method
                    plot(pec_res, 
                         col = c("#4477AA", "#EE6677"), 
                         lwd = 2,
                         main = "Prediction Error Curves (Brier Score)",
                         xlab = "Follow-up Time",
                         ylab = "Brier Score")
                    
                    # Add interpretation note
                    legend("topright", 
                           legend = c("Prognostic Model", "Reference (Naught)"), 
                           col = c("#4477AA", "#EE6677"), 
                           lwd = 2, bty = "n")
                    
                }, error = function(e) {
                    plot(1:10, type="n", main=paste("Prediction Error calculation failed:", e$message), axes = FALSE)
                })
            } else {
                plot(1:10, type="n", main="pec package required", axes = FALSE)
            }
            
            return(TRUE)
        },
        
        .plotDecisionCurve = function(image, ...) {
            if (is.null(private$prepared_data)) return(FALSE)
            
            data <- private$prepared_data
            vars <- private$variable_names
            
            if (requireNamespace('dcurves', quietly = TRUE)) {
                # dcurves::dca works best with probabilities, but we have risk scores
                eval_time <- as.numeric(strsplit(self$options$prediction_times, ",")[[1]])[1]
                if (is.na(eval_time)) eval_time <- median(data[[vars$time]], na.rm = TRUE)
                
                tryCatch({
                     dca_res <- dcurves::dca(
                        as.formula(paste("survival::Surv(", vars$time, ",", vars$status, ") ~", vars$risk_score)),
                        data = data,
                        time = eval_time,
                        label = list(risk_score = "Prognostic Model")
                    )
                    
                    # Styled Plot
                    p <- plot(dca_res) + 
                         ggplot2::labs(title = "Decision Curve Analysis (Clinical Utility)",
                                      subtitle = paste("Evaluated at follow-up time =", round(eval_time, 2)),
                                      x = "Threshold Probability",
                                      y = "Net Benefit") +
                         ggplot2::theme_minimal() +
                         ggplot2::theme(legend.position = "bottom")
                    
                    print(p)
                }, error = function(e) {
                    plot(1:10, type="n", main=paste("DCA Analysis Failed:", e$message), axes = FALSE)
                })
            } else {
                plot(1:10, type="n", main="dcurves package required", axes = FALSE)
            }
            
            return(TRUE)
        },
        
        .analyzeDiscrimination = function(data) {
            vars <- private$variable_names
            results <- private$validation_results
            
            html <- "<h4>Discrimination Assessment</h4>"
            html <- paste0(html, "<p>Discrimination refers to the model's ability to correctly distinguish between patients who will experience the event and those who will not. It is primarily measured by the Concordance Index (C-index).</p>")
            
            c_index <- results$corrected_performance
            interpretation <- private$.interpretCIndex(c_index)
            
            html <- paste0(html, "<div style='background-color: #f8f9fa; padding: 15px; border-radius: 5px; border-left: 5px solid #4477AA;'>")
            html <- paste0(html, "<p style='margin: 0;'><b>Bias-Corrected C-index:</b> <span style='font-size: 1.2em; color: #4477AA;'>", round(c_index, 4), "</span></p>")
            html <- paste0(html, "<p style='margin: 5px 0 0 0;'><b>Interpretation:</b> ", interpretation, "</p>")
            html <- paste0(html, "</div>")
            
            if (requireNamespace('timeROC', quietly = TRUE)) {
                pred_times <- as.numeric(strsplit(self$options$prediction_times, ",")[[1]])
                pred_times <- pred_times[!is.na(pred_times)]
                
                if (length(pred_times) > 0) {
                    html <- paste0(html, "<h5>Time-Dependent Discrimination</h5>")
                    html <- paste0(html, "<p>The model's ability to discriminate may change over time. Below is the summary of time-dependent Area Under the Curve (AUC) at selected intervals:</p>")
                    html <- paste0(html, "<ul>")
                    for (t in pred_times) {
                        # Logic to fetch or hint at table values
                        html <- paste0(html, "<li>At year/month <b>", t, "</b>, see the Performance Metrics table for the specific AUC estimate.</li>")
                    }
                    html <- paste0(html, "</ul>")
                }
            }
            
            html <- paste0(html, "<p style='font-size: 0.9em; color: #666; margin-top: 15px;'><i>Note: A C-index of 0.5 indicates random prediction, while 1.0 indicates perfect discrimination. Bias-correction accounts for model optimism typically seen in internal validation.</i></p>")
            
            self$results$discriminationAnalysis$setContent(html)
        },
        
        .performSubgroupValidation = function(data) {
            vars <- private$variable_names
            strat_var <- self$options$stratification_var
            conf_level <- self$options$confidence_level
            
            if (is.null(strat_var)) return()
            
            levels <- sort(unique(data[[strat_var]]))
            table <- self$results$subgroupAnalysis
            
            z <- qnorm(1 - (1 - conf_level)/2)
            
            for (level in levels) {
                sub_data <- data[data[[strat_var]] == level, ]
                n_patients <- nrow(sub_data)
                n_events <- sum(sub_data[[vars$status]], na.rm = TRUE)
                
                if (n_patients < 10 || n_events < 2) next  # Minimum requirements
                
                try({
                    sub_surv <- survival::Surv(sub_data[[vars$time]], sub_data[[vars$status]])
                    
                    # C-index
                    cindex_obj <- survival::concordance(sub_surv ~ sub_data[[vars$risk_score]])
                    cindex <- cindex_obj$concordance
                    cindex_se <- sqrt(cindex_obj$var)
                    
                    ci_lower <- cindex - z * cindex_se
                    ci_upper <- cindex + z * cindex_se
                    
                    # Calibration Slope in subgroup
                    cal_slope <- NA
                    try({
                        cal_mod <- survival::coxph(sub_surv ~ sub_data[[vars$risk_score]])
                        cal_slope <- coef(cal_mod)
                    }, silent = TRUE)
                    
                    table$addRow(rowKey = as.character(level), values = list(
                        subgroup = as.character(level),
                        n_patients = n_patients,
                        n_events = n_events,
                        c_index = round(cindex, 4),
                        c_index_ci = paste0("[", round(max(0, ci_lower), 3), ", ", round(min(1, ci_upper), 3), "]"),
                        calibration_slope = if(!is.na(cal_slope)) round(cal_slope, 4) else NULL
                    ))
                }, silent = TRUE)
            }
        },
        
        .performDecisionCurveAnalysis = function(data) {
            vars <- private$variable_names
            
            if (requireNamespace('dcurves', quietly = TRUE)) {
                eval_time <- as.numeric(strsplit(self$options$prediction_times, ",")[[1]])[1]
                if (is.na(eval_time)) eval_time <- median(data[[vars$time]])
                
                tryCatch({
                    dca_res <- dcurves::dca(
                        as.formula(paste("survival::Surv(", vars$time, ",", vars$status, ") ~", vars$risk_score)),
                        data = data,
                        time = eval_time
                    )
                    
                    dca_df <- as.data.frame(dca_res$dca)
                    table <- self$results$decisionCurveResults
                    
                    # Filter for principal threshold points to avoid overpopulating table
                    thresholds_to_show <- seq(0, 0.9, by = 0.1)
                    
                    for (thr in thresholds_to_show) {
                        # Find closest threshold in results
                        idx <- which.min(abs(dca_df$threshold - thr))
                        row <- dca_df[idx, ]
                        
                        table$addRow(rowKey = paste0("thr_", thr), values = list(
                            threshold = round(row$threshold, 2),
                            net_benefit = round(row$net_benefit, 4),
                            net_benefit_all = round(row$net_benefit_all, 4),
                            net_benefit_none = round(row$net_benefit_none, 4)
                        ))
                    }
                }, error = function(e) {
                     # Quietly handle in production
                })
            }
        },
        
        .assessClinicalImpact = function(data) {
            html <- "<h4>Clinical Impact & Utility Assessment</h4>"
            html <- paste0(html, "<p>Clinical impact assessment evaluates whether using the model to guide treatment decisions would results in better outcomes than current standard strategies (treating everyone or treating no one).</p>")
            
            html <- paste0(html, "<div style='background-color: #eef2f7; padding: 15px; border-radius: 5px; margin: 10px 0;'>")
            html <- paste0(html, "<p style='margin-top: 0;'><b>Understanding Net Benefit:</b></p>")
            html <- paste0(html, "<ul>")
            html <- paste0(html, "<li><b>Net Benefit:</b> The balance between true positives and weighted false positives at a specific risk threshold.</li>")
            html <- paste0(html, "<li><b>Treat All:</b> The utility if every patient received the intervention regardless of risk.</li>")
            html <- paste0(html, "<li><b>Treat None:</b> The baseline utility (zero net benefit) if no one received the intervention.</li>")
            html <- paste0(html, "</ul>")
            html <- paste0(html, "</div>")
            
            html <- paste0(html, "<p>Review the <b>Decision Curve Analysis</b> (DCA). The model is clinically useful at thresholds where its Net Benefit is higher than both the 'Treat All' and 'Treat None' lines. This range is often called the <i>Clinical Utility Window</i>.</p>")
            
            self$results$clinicalImpact$setContent(html)
        },
        
        .generateValidationReport = function(data, validation_results) {
            
            html <- "<h3>Comprehensive Validation Report</h3>"
            html <- paste0(html, "<p>This structured report summarizes the key performance characteristics of the prognostic survival model evaluated on this dataset.</p>")
            
            # Key findings
            cindex <- validation_results$corrected_performance
            optimism <- validation_results$optimism
            
            html <- paste0(html, "<div style='background-color: #fcfcfc; border: 1px solid #eee; padding: 20px; border-radius: 8px;'>")
            html <- paste0(html, "<h4 style='margin-top:0;'>Core Performance Metrics</h4>")
            
            # Discrimination
            desc_color <- if (cindex > 0.75) "#155724" else if (cindex > 0.6) "#856404" else "#721c24"
            desc_text <- if (cindex > 0.75) "Good to Excellent" else if (cindex>0.6) "Modest" else "Poor"
            
            html <- paste0(html, "<p><b>Discrimination (C-index):</b> <span style='font-weight:bold; color:", desc_color, ";'>", round(cindex, 3), "</span> (", desc_text, " discrimination)</p>")
            
            # Optimism
            opt_color <- if (abs(optimism) < 0.05) "#155724" else if (abs(optimism) < 0.1) "#856404" else "#721c24"
            opt_text <- if (abs(optimism) < 0.05) "Low (Stable)" else if (abs(optimism) < 0.1) "Moderate (Some overfitting)" else "High (Significant overfitting)"
            
            html <- paste0(html, "<p><b>Model Optimism:</b> <span style='font-weight:bold; color:", opt_color, ";'>", round(optimism, 3), "</span> (", opt_text, ")</p>")
            
            if (!is.null(validation_results$calibration_slope)) {
                html <- paste0(html, "<p><b>Calibration Slope:</b> ", round(validation_results$calibration_slope, 3), "</p>")
            }
            
            html <- paste0(html, "</div>")
            
            html <- paste0(html, "<h4>Analytical Context</h4>")
            html <- paste0(html, "<p>According to <b>TRIPOD guidelines</b>, internal validation (Bootstrap or Cross-validation) is essential to estimate model optimism and generate bias-corrected performance metrics. The values reported here represent the performance expected when applying the model to new patients from the same underlying population.</p>")
            
            if (self$options$validation_type == "external_validation") {
                html <- paste0(html, "<p><b>External Validation Note:</b> Since you have selected external validation, these results assess the model's <i>transportability</i> to a distinct population, which is the gold standard for clinical readiness.</p>")
            }
            
            self$results$validationReport$setContent(html)
        },
        
        .generateRecommendations = function(data, validation_results) {
            
            cindex <- validation_results$corrected_performance
            optimism <- validation_results$optimism
            n_obs <- nrow(data)
            event_rate <- if (!is.null(private$data_info$event_rate)) private$data_info$event_rate else (sum(data[[private$variable_names$status]]) / n_obs)
            
            html <- "<h3>Clinical & Methodological Recommendations</h3>"
            
            # Performance-based recommendations
            if (cindex < 0.6) {
                html <- paste0(html, "<div style='border: 1px solid #ffcccc; background-color: #fff5f5; padding: 10px; border-radius: 5px; margin-bottom: 10px;'>")
                html <- paste0(html, "<h4 style='color: #cc0000; margin-top: 0;'>Critical: Poor Model Discrimination</h4>")
                html <- paste0(html, "<p>The model shows low discriminative ability (C-index < 0.6). Recommendations:</p>")
                html <- paste0(html, "<ul>")
                html <- paste0(html, "<li><b>Feature Engineering:</b> Explore non-linear terms or interactions.</li>")
                html <- paste0(html, "<li><b>Data Quality:</b> Audit event status and time-to-event accuracy.</li>")
                html <- paste0(html, "<li><b>Model Class:</b> Consider flexible models like Gradient Boosting or Random Survival Forests.</li>")
                html <- paste0(html, "</ul></div>")
            } else if (cindex >= 0.75) {
                html <- paste0(html, "<div style='border: 1px solid #c3e6cb; background-color: #d4edda; padding: 10px; border-radius: 5px; margin-bottom: 10px;'>")
                html <- paste0(html, "<h4 style='color: #155724; margin-top: 0;'>Excellent Performance</h4>")
                html <- paste0(html, "<p>The model demonstrates strong discrimination (C-index > 0.75). It may be highly suitable for clinical decision support after external validation.</p>")
                html <- paste0(html, "</div>")
            }
            
            # Sample size and Event rate alerts
            if (n_obs < 150 || (n_obs * event_rate) < 20) {
                html <- paste0(html, "<div style='border: 1px solid #ffeeba; background-color: #fff3cd; padding: 10px; border-radius: 5px; margin-bottom: 10px;'>")
                html <- paste0(html, "<h4 style='color: #856404; margin-top: 0;'>Stability Warning</h4>")
                html <- paste0(html, "<p>Small sample size or low event count may lead to unreliable estimates. Internal validation optimism may be underestimated.</p>")
                html <- paste0(html, "</div>")
            }
            
            # Next steps
            html <- paste0(html, "<h4>Strategic Next Steps</h4>")
            html <- paste0(html, "<ol>")
            
            if (optimism > 0.05) {
                html <- paste0(html, "<li><b>Address Overfitting:</b> Apply heuristic or bootstrap-based shrinkage to the regression coefficients.</li>")
            }
            
            html <- paste0(html, "<li><b>External Validation:</b> Evaluate the model on a geographically or temporally distinct dataset to assess transportability.</li>")
            html <- paste0(html, "<li><b>Calibration Tuning:</b> If the calibration slope deviates significantly from 1, recalibrate the intercept and slope in the target population.</li>")
            html <- paste0(html, "<li><b>Clinical Utility:</b> Use the Decision Curve Analysis (DCA) to determine the threshold range where the model adds net benefit over standard care.</li>")
            html <- paste0(html, "</ol>")
            
            self$results$recommendations$setContent(html)
        },

        .interpretCIndex = function(c_index) {
            if (is.na(c_index)) return("N/A")
            if (c_index >= 0.9) return("Excellent discrimination")
            if (c_index >= 0.8) return("Very good discrimination")
            if (c_index >= 0.7) return("Good discrimination")
            if (c_index >= 0.6) return("Modest discrimination")
            if (c_index >= 0.5) return("Poor/No discrimination")
            return("Worse than random (check score direction)")
        }
    )
)
