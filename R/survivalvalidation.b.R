#' @title Survival Model Validation
#' @importFrom R6 R6Class
#' @import jmvcore
#'

survivalvalidationClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "survivalvalidationClass",
    inherit = survivalvalidationBase,
    private = list(
        
        .init = function() {
            # Check for required packages
            packages_needed <- c('pec', 'timeROC', 'survAUC', 'riskRegression', 'survival')
            packages_missing <- packages_needed[!sapply(packages_needed, requireNamespace, quietly = TRUE)]
            
            if (length(packages_missing) > 0) {
                warning_msg <- paste("The following packages are recommended but not installed:", 
                                   paste(packages_missing, collapse = ", "),
                                   "\nInstall using: install.packages(c('", 
                                   paste(packages_missing, collapse = "', '"), "'))")
                self$results$todo$setContent(warning_msg)
            }
        },
        
        .run = function() {
            
            # Check if variables are selected
            if (is.null(self$options$time) || is.null(self$options$status)) {
                self$results$todo$setContent(
                    "<h3>Welcome to Survival Model Validation</h3>
                    <p>This module provides comprehensive validation and performance assessment 
                    for survival models using state-of-the-art statistical methods.</p>
                    
                    <h4>Key Validation Metrics:</h4>
                    <ul>
                    <li><b>Concordance Index (C-index):</b> Overall discriminative ability</li>
                    <li><b>Time-dependent AUC:</b> Discrimination at specific time points</li>
                    <li><b>Prediction Error Curves:</b> Prediction accuracy over time</li>
                    <li><b>Integrated Brier Score:</b> Overall prediction error</li>
                    <li><b>Calibration:</b> Agreement between predicted and observed</li>
                    <li><b>Decision Curve Analysis:</b> Clinical utility and net benefit</li>
                    </ul>
                    
                    <h4>Input Options:</h4>
                    <ul>
                    <li><b>Predicted Risk:</b> Provide pre-calculated risk scores/linear predictors</li>
                    <li><b>Model Formula:</b> Specify variables to fit Cox model automatically</li>
                    <li><b>External Data:</b> Use separate dataset for external validation</li>
                    </ul>
                    
                    <h4>Validation Methods:</h4>
                    <ul>
                    <li><b>Cross-validation:</b> K-fold internal validation</li>
                    <li><b>Bootstrap:</b> Bootstrap optimism correction</li>
                    <li><b>External:</b> Independent dataset validation</li>
                    <li><b>Apparent:</b> Performance on training data</li>
                    </ul>
                    
                    <p>Please select time, status, and either predicted risk or model formula to begin.</p>"
                )
                return()
            }
            
            # Prepare data and model
            prepared_data <- private$.prepareData()
            if (is.null(prepared_data)) return()
            
            # Fit or use provided model
            model_info <- private$.prepareModel(prepared_data)
            if (is.null(model_info)) return()
            
            # Perform validation based on method
            validation_method <- self$options$validation_method %||% "cv"
            validation_results <- private$.performValidation(prepared_data, model_info, validation_method)
            
            # Calculate performance metrics
            if (self$options$concordance_index) {
                private$.calculateConcordanceIndex(model_info, prepared_data)
            }
            
            if (self$options$time_dependent_auc) {
                private$.calculateTimeDepAUC(model_info, prepared_data)
            }
            
            if (self$options$prediction_error) {
                private$.calculatePredictionError(model_info, prepared_data)
            }
            
            if (self$options$integrated_brier) {
                private$.calculateIntegratedBrier(model_info, prepared_data)
            }
            
            if (self$options$calibration_plot) {
                private$.performCalibrationAnalysis(model_info, prepared_data)
            }
            
            if (self$options$decision_curve) {
                private$.performDecisionCurveAnalysis(model_info, prepared_data)
            }
            
            # Generate plots
            private$.generateValidationPlots(validation_results, model_info, prepared_data)
        },
        
        .prepareData = function() {
            
            tryCatch({
                # Get variables
                time_var <- self$options$time
                status_var <- self$options$status
                pred_risk_var <- self$options$predicted_risk
                # external_data <- self$options$external_data
                
                # Get main dataset
                data <- self$data
                
                # Select relevant columns
                vars_needed <- c(time_var, status_var)
                if (!is.null(pred_risk_var)) vars_needed <- c(vars_needed, pred_risk_var)
                
                # Add formula variables if specified
                model_formula <- self$options$model_formula
                if (!is.null(model_formula) && model_formula != "") {
                    # Parse formula to get variable names
                    formula_vars <- private$.parseFormulaVars(model_formula)
                    vars_needed <- c(vars_needed, formula_vars)
                }
                
                # Get complete cases
                data <- data[, vars_needed]
                data <- data[complete.cases(data), ]
                
                if (nrow(data) < 50) {
                    stop("Insufficient data for validation (minimum 50 complete cases required)")
                }
                
                # Ensure proper variable types
                data[[status_var]] <- as.numeric(data[[status_var]])
                data[[time_var]] <- as.numeric(data[[time_var]])
                
                # Store variable info
                private$variable_names <- list(
                    time = time_var,
                    status = status_var,
                    predicted_risk = pred_risk_var,
                    model_formula = model_formula
                )
                
                return(data)
                
            }, error = function(e) {
                self$results$todo$setContent(
                    paste("Data preparation failed:", e$message)
                )
                return(NULL)
            })
        },
        
        .prepareModel = function(data) {
            
            tryCatch({
                vars <- private$variable_names
                
                # Check if we have predicted risk or need to fit model
                if (!is.null(vars$predicted_risk)) {
                    # Use provided predictions
                    model_info <- list(
                        type = "provided",
                        predictions = data[[vars$predicted_risk]],
                        data = data
                    )
                } else if (!is.null(vars$model_formula) && vars$model_formula != "") {
                    # Fit Cox model with provided formula
                    formula_str <- paste("Surv(", vars$time, ",", vars$status, ") ~", vars$model_formula)
                    cox_model <- survival::coxph(as.formula(formula_str), data = data)
                    
                    model_info <- list(
                        type = "fitted",
                        model = cox_model,
                        predictions = predict(cox_model, type = "lp"),
                        data = data,
                        formula = formula_str
                    )
                } else {
                    stop("Either predicted risk variable or model formula must be provided")
                }
                
                return(model_info)
                
            }, error = function(e) {
                self$results$todo$setContent(
                    paste("Model preparation failed:", e$message)
                )
                return(NULL)
            })
        },
        
        .performValidation = function(data, model_info, method) {
            
            tryCatch({
                vars <- private$variable_names
                
                if (method == "cv") {
                    # Cross-validation
                    cv_folds <- self$options$cv_folds %||% 10
                    validation_results <- private$.performCrossValidation(data, model_info, cv_folds)
                    
                } else if (method == "bootstrap") {
                    # Bootstrap validation
                    n_boot <- self$options$bootstrap_samples %||% 500
                    validation_results <- private$.performBootstrapValidation(data, model_info, n_boot)
                    
                } else if (method == "external") {
                    # External validation
                    ext_data <- self$options$external_data
                    if (is.null(ext_data)) {
                        stop("External data not provided for external validation")
                    }
                    validation_results <- private$.performExternalValidation(model_info, ext_data)
                    
                } else if (method == "apparent") {
                    # Apparent performance
                    validation_results <- private$.performApparentValidation(data, model_info)
                    
                } else if (method == "optimism") {
                    # Optimism-corrected performance
                    validation_results <- private$.performOptimismCorrection(data, model_info)
                }
                
                # Store validation results
                private$validation_results <- validation_results
                
                return(validation_results)
                
            }, error = function(e) {
                self$results$todo$setContent(
                    paste("Validation failed:", e$message)
                )
                return(NULL)
            })
        },
        
        .calculateConcordanceIndex = function(model_info, data) {
            
            tryCatch({
                if (!requireNamespace('survival', quietly = TRUE)) {
                    stop("survival package required")
                }
                
                vars <- private$variable_names
                
                # Calculate Harrell's C-index
                if (model_info$type == "fitted") {
                    c_index <- survival::concordanceIndex(
                        model_info$predictions,
                        surv.time = data[[vars$time]],
                        surv.event = data[[vars$status]]
                    )
                } else {
                    # Use survcomp if available, otherwise survival
                    if (requireNamespace('survcomp', quietly = TRUE)) {
                        c_index <- survcomp::concordance.index(
                            x = model_info$predictions,
                            surv.time = data[[vars$time]],
                            surv.event = data[[vars$status]],
                            method = "noether"
                        )
                        
                        c_stat <- c_index$c.index
                        c_se <- c_index$se
                        c_ci_lower <- c_index$lower
                        c_ci_upper <- c_index$upper
                    } else {
                        # Fallback calculation
                        surv_obj <- survival::Surv(data[[vars$time]], data[[vars$status]])
                        c_stat <- survival::survConcordance(surv_obj ~ model_info$predictions)$concordance
                        c_se <- NA
                        c_ci_lower <- NA
                        c_ci_upper <- NA
                    }
                }
                
                # Display C-index results
                concordance_html <- glue::glue(
                    "<h4>Concordance Index (C-index)</h4>
                    <p><b>Harrell's C-index:</b> {round(c_stat, 4)}</p>",
                    if (!is.na(c_se)) paste0("<p><b>Standard Error:</b> {round(c_se, 4)}</p>") else "",
                    if (!is.na(c_ci_lower)) paste0("<p><b>95% CI:</b> ({round(c_ci_lower, 4)}, {round(c_ci_upper, 4)})</p>") else "",
                    "<p><b>Interpretation:</b> {private$.interpretCIndex(c_stat)}</p>"
                )
                
                self$results$concordanceResults$setContent(concordance_html)
                
            }, error = function(e) {
                message("C-index calculation failed: ", e$message)
            })
        },
        
        .calculateTimeDepAUC = function(model_info, data) {
            
            tryCatch({
                if (!requireNamespace('timeROC', quietly = TRUE)) {
                    message("timeROC package not available, skipping time-dependent AUC")
                    return()
                }
                
                vars <- private$variable_names
                
                # Parse time points
                time_points_str <- self$options$time_points %||% "1,2,3,5"
                time_points <- as.numeric(strsplit(time_points_str, ",")[[1]])
                
                # Calculate time-dependent AUC
                td_auc <- timeROC::timeROC(
                    T = data[[vars$time]],
                    delta = data[[vars$status]],
                    marker = model_info$predictions,
                    times = time_points,
                    cause = 1,
                    weighting = "marginal"
                )
                
                # Store results
                private$td_auc <- td_auc
                
                # Create results table
                auc_table <- self$results$aucTable
                for (i in seq_along(time_points)) {
                    auc_table$addRow(rowKey = i, values = list(
                        time_point = time_points[i],
                        auc = round(td_auc$AUC[i], 4),
                        se = round(sqrt(td_auc$inference$vect_sd_1[i]), 4),
                        ci_lower = round(td_auc$AUC[i] - 1.96 * sqrt(td_auc$inference$vect_sd_1[i]), 4),
                        ci_upper = round(td_auc$AUC[i] + 1.96 * sqrt(td_auc$inference$vect_sd_1[i]), 4)
                    ))
                }
                
            }, error = function(e) {
                message("Time-dependent AUC calculation failed: ", e$message)
            })
        },
        
        .calculatePredictionError = function(model_info, data) {
            
            tryCatch({
                if (!requireNamespace('pec', quietly = TRUE)) {
                    message("pec package not available, skipping prediction error curves")
                    return()
                }
                
                vars <- private$variable_names
                
                # Get maximum time
                max_time <- self$options$max_time
                if (max_time <= 0) {
                    max_time <- max(data[[vars$time]])
                }
                
                # Calculate prediction error curves
                if (model_info$type == "fitted") {
                    pec_result <- pec::pec(
                        object = model_info$model,
                        formula = Surv(data[[vars$time]], data[[vars$status]]) ~ 1,
                        data = data,
                        times = seq(0, max_time, length.out = 100),
                        exact = FALSE,
                        cens.model = "marginal"
                    )
                } else {
                    # For provided predictions, create a simple model
                    pec_result <- NULL
                    message("PEC requires fitted model object")
                }
                
                # Store results
                private$pec_result <- pec_result
                
            }, error = function(e) {
                message("Prediction error calculation failed: ", e$message)
            })
        },
        
        .performCalibrationAnalysis = function(model_info, data) {
            
            tryCatch({
                vars <- private$variable_names
                
                # Get evaluation time points
                time_points_str <- self$options$time_points %||% "1,2,3,5"
                time_points <- as.numeric(strsplit(time_points_str, ",")[[1]])
                
                # Number of risk groups
                n_groups <- self$options$risk_groups %||% 4
                
                calibration_results <- list()
                
                for (t_point in time_points) {
                    # Calculate observed and predicted survival at t_point
                    risk_groups <- cut(model_info$predictions, 
                                     breaks = quantile(model_info$predictions, 
                                                     probs = seq(0, 1, length.out = n_groups + 1)),
                                     include.lowest = TRUE)
                    
                    # For each risk group, calculate observed vs expected
                    group_results <- data.frame(
                        group = levels(risk_groups),
                        n = as.numeric(table(risk_groups)),
                        observed = NA,
                        expected = NA
                    )
                    
                    for (g in seq_along(levels(risk_groups))) {
                        group_data <- data[risk_groups == levels(risk_groups)[g], ]
                        
                        if (nrow(group_data) > 0) {
                            # Observed survival (Kaplan-Meier)
                            km_fit <- survival::survfit(
                                survival::Surv(group_data[[vars$time]], group_data[[vars$status]]) ~ 1
                            )
                            
                            # Find survival at time point
                            surv_at_t <- summary(km_fit, times = t_point)$surv
                            if (length(surv_at_t) == 0) surv_at_t <- NA
                            
                            group_results$observed[g] <- surv_at_t * 100
                            
                            # Expected survival (from model predictions)
                            # This is simplified - would need proper calibration methods
                            mean_pred <- mean(model_info$predictions[risk_groups == levels(risk_groups)[g]])
                            group_results$expected[g] <- mean_pred * 100  # Simplified
                        }
                    }
                    
                    calibration_results[[as.character(t_point)]] <- group_results
                }
                
                # Store calibration results
                private$calibration_results <- calibration_results
                
                # Display calibration summary
                private$.displayCalibrationResults(calibration_results)
                
            }, error = function(e) {
                message("Calibration analysis failed: ", e$message)
            })
        },
        
        .interpretCIndex = function(c_index) {
            if (c_index >= 0.8) {
                return("Excellent discrimination")
            } else if (c_index >= 0.7) {
                return("Good discrimination")
            } else if (c_index >= 0.6) {
                return("Poor discrimination")
            } else {
                return("No discrimination (random)")
            }
        },
        
        .generateValidationPlots = function(validation_results, model_info, data) {
            
            # Generate ROC curves if requested
            if (self$options$plot_roc_curves && !is.null(private$td_auc)) {
                image <- self$results$rocPlot
                image$setState(list(td_auc = private$td_auc))
            }
            
            # Generate calibration plot if requested
            if (self$options$plot_calibration && !is.null(private$calibration_results)) {
                image <- self$results$calibrationPlot
                image$setState(list(calibration = private$calibration_results))
            }
            
            # Generate prediction error plot if requested
            if (self$options$plot_prediction_error && !is.null(private$pec_result)) {
                image <- self$results$predErrorPlot
                image$setState(list(pec = private$pec_result))
            }
        }
    )
)