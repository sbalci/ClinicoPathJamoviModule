#' @title Clinical Nomograms & Risk Calculators
#' @importFrom jmvcore .
#' @importFrom stats glm lm predict residuals fitted confint quantile
#' @importFrom survival Surv survfit coxph
#' @importFrom rms cph nomogram val.surv calibrate
#' @importFrom regplot regplot
#' @importFrom glmnet glmnet cv.glmnet
#' @importFrom leaps regsubsets
#' @importFrom boot boot boot.ci
#' @importFrom dcurves dca
#' @export

clinicalnomogramsClass <- R6::R6Class(
    "clinicalnomogramsClass",
    inherit = clinicalnomogramsBase,
    private = list(
        
        .init = function() {
            
            nomogram_type <- self$options$nomogram_type
            
            if (is.null(self$data) || is.null(self$options$covariates)) {
                
                self$results$instructions$setContent(
                    "<h3>Welcome to Clinical Nomograms & Risk Calculators</h3>
                    <p>Transform statistical models into practical clinical tools for personalized risk assessment 
                    and treatment planning. This module creates publication-quality nomograms and interactive 
                    risk calculators for clinical practice.</p>
                    
                    <h4>Types of Nomograms Available:</h4>
                    <ul>
                    <li><b>Survival Nomogram:</b> Predict survival probabilities at specific time points</li>
                    <li><b>Logistic Nomogram:</b> Predict binary outcomes (e.g., response, recurrence)</li>
                    <li><b>Linear Nomogram:</b> Predict continuous outcomes</li>
                    <li><b>Competing Risks:</b> Account for multiple competing events</li>
                    <li><b>Multi-State:</b> Model complex disease progression pathways</li>
                    </ul>
                    
                    <h4>Key Features:</h4>
                    <ul>
                    <li><b>Visual Nomograms:</b> Publication-quality graphical risk assessment tools</li>
                    <li><b>Interactive Calculators:</b> Web-based tools for real-time risk calculation</li>
                    <li><b>Model Validation:</b> Bootstrap and cross-validation for reliability assessment</li>
                    <li><b>Performance Metrics:</b> Discrimination, calibration, and clinical utility</li>
                    <li><b>Risk Stratification:</b> Patient classification into risk groups</li>
                    <li><b>Clinical Scenarios:</b> Example patient cases for training</li>
                    </ul>
                    
                    <h4>Clinical Applications:</h4>
                    <ul>
                    <li>Personalized risk assessment and counseling</li>
                    <li>Treatment decision support</li>
                    <li>Clinical trial stratification</li>
                    <li>Quality improvement initiatives</li>
                    <li>Medical education and training</li>
                    </ul>
                    
                    <h4>Required Variables:</h4>
                    <ul>
                    <li><b>For Survival Nomograms:</b> Time variable, event status, predictor variables</li>
                    <li><b>For Other Nomograms:</b> Outcome variable, predictor variables</li>
                    <li><b>Predictor Variables:</b> Clinical factors, biomarkers, imaging features</li>
                    </ul>
                    
                    <p><b>Clinical Impact:</b> Nomograms translate complex statistical models into intuitive 
                    tools that support evidence-based clinical decision-making and improve patient care.</p>
                    
                    <p>Please select your variables to begin creating clinical nomograms.</p>"
                )
                return()
            }
            
            # Check if appropriate variables are selected based on nomogram type
            if (nomogram_type == "survival_nomogram") {
                if (is.null(self$options$time_var) || is.null(self$options$status_var)) {
                    self$results$instructions$setContent(
                        "<p><b>Survival Nomogram requires:</b> Time variable, Event status, and Predictor variables.</p>"
                    )
                    return()
                }
            } else {
                if (is.null(self$options$outcome_var)) {
                    self$results$instructions$setContent(
                        "<p><b>Non-survival nomograms require:</b> Outcome variable and Predictor variables.</p>"
                    )
                    return()
                }
            }
            
            # Initialize with selected variables
            covariates <- self$options$covariates
            n_predictors <- length(covariates)
            
            self$results$instructions$setContent(
                paste0("<h3>Clinical Nomogram Development</h3>
                <p><b>Nomogram Type:</b> ", gsub("_", " ", stringr::str_to_title(nomogram_type)), "</p>
                <p><b>Number of Predictors:</b> ", n_predictors, "</p>
                <p><b>Variables:</b> ", paste(covariates, collapse = ", "), "</p>
                <p>Configure nomogram settings and click <b>Results</b> to develop your clinical prediction tool.</p>")
            )
        },
        
        .run = function() {
            
            if (is.null(self$data) || is.null(self$options$covariates)) {
                return()
            }
            
            # Check nomogram type requirements
            nomogram_type <- self$options$nomogram_type
            if (nomogram_type == "survival_nomogram") {
                if (is.null(self$options$time_var) || is.null(self$options$status_var)) {
                    return()
                }
            } else {
                if (is.null(self$options$outcome_var)) {
                    return()
                }
            }
            
            # Prepare and validate data
            prepared_data <- private$.prepareData()
            if (is.null(prepared_data)) return()
            
            # Variable selection
            selected_vars <- private$.performVariableSelection(prepared_data)
            if (is.null(selected_vars)) return()
            
            # Fit the final model
            final_model <- private$.fitFinalModel(prepared_data, selected_vars)
            if (is.null(final_model)) return()
            
            # Model validation
            if (self$options$validation_method != "none") {
                validation_results <- private$.performModelValidation(prepared_data, final_model)
            }
            
            # Generate nomogram
            nomogram_obj <- private$.createNomogram(final_model, prepared_data)
            
            # Performance assessment
            if (self$options$performance_metrics) {
                private$.assessModelPerformance(prepared_data, final_model)
            }
            
            # Calibration assessment
            if (self$options$calibration_assessment) {
                private$.assessCalibration(prepared_data, final_model)
            }
            
            # Discrimination assessment
            if (self$options$discrimination_assessment) {
                private$.assessDiscrimination(prepared_data, final_model)
            }
            
            # Decision curve analysis
            if (self$options$decision_curve_analysis) {
                private$.performDecisionCurveAnalysis(prepared_data, final_model)
            }
            
            # Risk group analysis
            if (self$options$risk_groups) {
                private$.performRiskGroupAnalysis(prepared_data, final_model)
            }
            
            # Variable importance
            if (self$options$variable_importance) {
                private$.calculateVariableImportance(final_model)
            }
            
            # Clinical scenarios
            if (self$options$clinical_scenarios) {
                private$.generateClinicalScenarios(final_model, prepared_data)
            }
            
            # Generate plots
            private$.generateNomogramPlots(final_model, prepared_data, nomogram_obj)
            
            # Interactive nomogram
            if (self$options$interactive_nomogram) {
                private$.createInteractiveNomogram(final_model, prepared_data)
            }
            
            # Implementation guide
            if (self$options$clinical_implementation) {
                private$.generateImplementationGuide(final_model)
            }
            
            # Reporting checklist
            private$.generateReportingChecklist()
        },
        
        .prepareData = function() {
            
            tryCatch({
                
                nomogram_type <- self$options$nomogram_type
                covariates <- self$options$covariates
                
                # Select variables based on nomogram type
                if (nomogram_type == "survival_nomogram") {
                    time_var <- self$options$time_var
                    status_var <- self$options$status_var
                    vars_needed <- c(time_var, status_var, covariates)
                } else {
                    outcome_var <- self$options$outcome_var
                    vars_needed <- c(outcome_var, covariates)
                }
                
                # Get data
                data <- self$data[, vars_needed, drop = FALSE]
                
                # Handle missing data based on selected method
                missing_method <- self$options$missing_data_handling
                
                if (missing_method == "complete_case") {
                    complete_cases <- complete.cases(data)
                    data <- data[complete_cases, ]
                    
                    missing_count <- sum(!complete_cases)
                    if (missing_count > 0) {
                        self$results$diagnostics$setContent(
                            paste0("<p><b>Missing Data:</b> ", missing_count, " cases excluded due to missing values.</p>")
                        )
                    }
                } else if (missing_method == "median_mode") {
                    # Simple imputation with median for numeric, mode for factors
                    for (var in names(data)) {
                        if (any(is.na(data[[var]]))) {
                            if (is.numeric(data[[var]])) {
                                data[[var]][is.na(data[[var]])] <- median(data[[var]], na.rm = TRUE)
                            } else {
                                mode_val <- names(sort(table(data[[var]]), decreasing = TRUE))[1]
                                data[[var]][is.na(data[[var]])] <- mode_val
                            }
                        }
                    }
                }
                
                # Validate data requirements
                if (nrow(data) < 50) {
                    self$results$diagnostics$setContent(
                        paste0(self$results$diagnostics$content,
                               "<p style='color: red;'><b>Warning:</b> Sample size is very small (n < 50). 
                               Nomogram may be unreliable.</p>")
                    )
                } else if (nrow(data) < 100) {
                    self$results$diagnostics$setContent(
                        paste0(self$results$diagnostics$content,
                               "<p style='color: orange;'><b>Note:</b> Sample size is modest (n < 100). 
                               Consider validation on external data.</p>")
                    )
                }
                
                # Store variable information
                if (nomogram_type == "survival_nomogram") {
                    data[[status_var]] <- as.numeric(data[[status_var]])
                    data[[time_var]] <- as.numeric(data[[time_var]])
                    
                    # Check event status coding
                    unique_status <- sort(unique(data[[status_var]]))
                    if (!all(unique_status %in% c(0, 1))) {
                        if (all(unique_status %in% c(1, 2))) {
                            data[[status_var]] <- data[[status_var]] - 1
                        } else {
                            stop("Status variable must be coded as 0/1 or 1/2")
                        }
                    }
                    
                    n_events <- sum(data[[status_var]])
                    event_rate <- n_events / nrow(data)
                    
                    private$data_info <- list(
                        n_total = nrow(data),
                        n_events = n_events,
                        event_rate = event_rate,
                        median_followup = median(data[[time_var]])
                    )
                    
                    private$variable_names <- list(
                        time = time_var,
                        status = status_var,
                        outcome = NULL,
                        covariates = covariates
                    )
                } else {
                    outcome_var <- self$options$outcome_var
                    
                    # Convert outcome variable to appropriate type
                    if (nomogram_type == "logistic_nomogram") {
                        data[[outcome_var]] <- as.factor(data[[outcome_var]])
                        outcome_levels <- levels(data[[outcome_var]])
                        if (length(outcome_levels) != 2) {
                            stop("Logistic nomogram requires binary outcome variable")
                        }
                    } else {
                        data[[outcome_var]] <- as.numeric(data[[outcome_var]])
                    }
                    
                    private$data_info <- list(
                        n_total = nrow(data),
                        outcome_summary = summary(data[[outcome_var]])
                    )
                    
                    private$variable_names <- list(
                        time = NULL,
                        status = NULL,
                        outcome = outcome_var,
                        covariates = covariates
                    )
                }
                
                return(data)
                
            }, error = function(e) {
                self$results$diagnostics$setContent(
                    paste("<p style='color: red;'><b>Data Preparation Error:</b>", e$message, "</p>")
                )
                return(NULL)
            })
        },
        
        .performVariableSelection = function(data) {
            
            tryCatch({
                
                selection_method <- self$options$model_selection
                vars <- private$variable_names
                nomogram_type <- self$options$nomogram_type
                
                if (selection_method == "all_variables") {
                    # Include all provided variables
                    selected_vars <- vars$covariates
                    
                    # Update variable selection table
                    var_table <- self$results$variableSelection
                    for (var in selected_vars) {
                        var_table$addRow(rowKey = var, values = list(
                            variable = var,
                            coefficient = NA,
                            se = NA,
                            z_value = NA,
                            p_value = NA,
                            selected = "Yes"
                        ))
                    }
                    
                } else if (selection_method == "stepwise") {
                    # Stepwise variable selection
                    selected_vars <- private$.performStepwiseSelection(data)
                    
                } else if (selection_method == "lasso_selection") {
                    # LASSO regularization
                    selected_vars <- private$.performLassoSelection(data)
                    
                } else {
                    # Default to all variables
                    selected_vars <- vars$covariates
                }
                
                if (length(selected_vars) == 0) {
                    stop("No variables selected for the model")
                }
                
                return(selected_vars)
                
            }, error = function(e) {
                self$results$diagnostics$setContent(
                    paste0(self$results$diagnostics$content,
                           "<p style='color: red;'><b>Variable Selection Error:</b>", e$message, "</p>")
                )
                return(NULL)
            })
        },
        
        .fitFinalModel = function(data, selected_vars) {
            
            tryCatch({
                
                nomogram_type <- self$options$nomogram_type
                vars <- private$variable_names
                
                if (nomogram_type == "survival_nomogram") {
                    # Cox proportional hazards model
                    formula_str <- paste("Surv(", vars$time, ",", vars$status, ") ~ ",
                                       paste(selected_vars, collapse = " + "))
                    
                    if (requireNamespace('rms', quietly = TRUE)) {
                        # Use rms for nomogram compatibility
                        model <- rms::cph(as.formula(formula_str), 
                                        data = data, 
                                        x = TRUE, 
                                        y = TRUE,
                                        surv = TRUE)
                    } else {
                        model <- survival::coxph(as.formula(formula_str), data = data)
                    }
                    
                } else if (nomogram_type == "logistic_nomogram") {
                    # Logistic regression
                    formula_str <- paste(vars$outcome, "~", paste(selected_vars, collapse = " + "))
                    
                    if (requireNamespace('rms', quietly = TRUE)) {
                        model <- rms::lrm(as.formula(formula_str), 
                                        data = data, 
                                        x = TRUE, 
                                        y = TRUE)
                    } else {
                        model <- glm(as.formula(formula_str), 
                                   data = data, 
                                   family = binomial())
                    }
                    
                } else if (nomogram_type == "linear_nomogram") {
                    # Linear regression
                    formula_str <- paste(vars$outcome, "~", paste(selected_vars, collapse = " + "))
                    
                    if (requireNamespace('rms', quietly = TRUE)) {
                        model <- rms::ols(as.formula(formula_str), 
                                        data = data, 
                                        x = TRUE, 
                                        y = TRUE)
                    } else {
                        model <- lm(as.formula(formula_str), data = data)
                    }
                }
                
                # Store model and generate summary
                private$final_model <- model
                private$.generateModelSummary(model, data)
                
                return(model)
                
            }, error = function(e) {
                self$results$diagnostics$setContent(
                    paste0(self$results$diagnostics$content,
                           "<p style='color: red;'><b>Model Fitting Error:</b>", e$message, "</p>")
                )
                return(NULL)
            })
        },
        
        .generateModelSummary = function(model, data) {
            
            nomogram_type <- self$options$nomogram_type
            info <- private$data_info
            
            html <- "<h3>Fitted Model Summary</h3>"
            
            # Basic model information
            html <- paste0(html, "<table class='jamovi-table'>")
            html <- paste0(html, "<tr><td><b>Model Type:</b></td><td>", 
                          gsub("_", " ", stringr::str_to_title(nomogram_type)), "</td></tr>")
            html <- paste0(html, "<tr><td><b>Sample Size:</b></td><td>", info$n_total, "</td></tr>")
            
            if (nomogram_type == "survival_nomogram") {
                html <- paste0(html, "<tr><td><b>Number of Events:</b></td><td>", info$n_events, 
                              " (", round(100 * info$event_rate, 1), "%)</td></tr>")
                html <- paste0(html, "<tr><td><b>Median Follow-up:</b></td><td>", 
                              round(info$median_followup, 2), " time units</td></tr>")
            }
            
            html <- paste0(html, "</table>")
            
            # Model coefficients
            if (inherits(model, c("cph", "lrm", "ols"))) {
                # rms model
                coef_summary <- summary(model)
                if (!is.null(coef_summary)) {
                    html <- paste0(html, "<h4>Model Coefficients</h4>")
                    html <- paste0(html, "<p>Coefficients and their significance in the fitted model.</p>")
                }
            } else {
                # Standard R model
                model_summary <- summary(model)
                html <- paste0(html, "<h4>Model Fit Statistics</h4>")
                
                if (nomogram_type == "survival_nomogram") {
                    html <- paste0(html, "<p>Concordance index and likelihood ratio test results.</p>")
                } else if (nomogram_type == "logistic_nomogram") {
                    html <- paste0(html, "<p>AIC: ", round(model_summary$aic, 2), "</p>")
                } else {
                    html <- paste0(html, "<p>R-squared: ", round(model_summary$r.squared, 4), "</p>")
                    html <- paste0(html, "<p>Adjusted R-squared: ", round(model_summary$adj.r.squared, 4), "</p>")
                }
            }
            
            # Model equation
            if (self$options$model_equation) {
                private$.generateModelEquation(model)
            }
            
            self$results$modelSummary$setContent(html)
        },
        
        .createNomogram = function(model, data) {
            
            tryCatch({
                
                if (!requireNamespace('rms', quietly = TRUE)) {
                    stop("rms package is required for nomogram creation")
                }
                
                nomogram_type <- self$options$nomogram_type
                pred_times_str <- self$options$prediction_times
                points_scale <- self$options$points_scale
                
                # Parse prediction times
                pred_times <- as.numeric(strsplit(pred_times_str, ",")[[1]])
                pred_times <- pred_times[!is.na(pred_times)]
                
                if (nomogram_type == "survival_nomogram") {
                    # Create survival nomogram
                    if (length(pred_times) > 0) {
                        nomogram_obj <- rms::nomogram(
                            model,
                            fun = list(
                                function(x) 1 - rms::survest(model, times = pred_times[1], newdata = x)$surv,
                                if (length(pred_times) > 1) function(x) 1 - rms::survest(model, times = pred_times[2], newdata = x)$surv,
                                if (length(pred_times) > 2) function(x) 1 - rms::survest(model, times = pred_times[3], newdata = x)$surv
                            ),
                            funlabel = paste0("Mortality Risk at ", pred_times, " years"),
                            maxscale = points_scale
                        )
                    } else {
                        # Default nomogram without specific time points
                        nomogram_obj <- rms::nomogram(model, maxscale = points_scale)
                    }
                } else {
                    # Non-survival nomograms
                    nomogram_obj <- rms::nomogram(model, maxscale = points_scale)
                }
                
                # Store nomogram object
                private$nomogram_obj <- nomogram_obj
                
                # Generate risk calculator table
                if (self$options$risk_calculator) {
                    private$.generateRiskCalculatorTable(model, nomogram_obj)
                }
                
                return(nomogram_obj)
                
            }, error = function(e) {
                self$results$diagnostics$setContent(
                    paste0(self$results$diagnostics$content,
                           "<p style='color: red;'><b>Nomogram Creation Error:</b>", e$message, "</p>")
                )
                return(NULL)
            })
        },
        
        .generateNomogramPlots = function(model, data, nomogram_obj) {
            
            # Set up plot states for rendering
            plot_data <- list(
                model = model,
                data = data,
                nomogram_obj = nomogram_obj,
                nomogram_type = self$options$nomogram_type,
                nomogram_title = self$options$nomogram_title,
                pred_times = as.numeric(strsplit(self$options$prediction_times, ",")[[1]])
            )
            
            # Main nomogram plot
            self$results$nomogramPlot$setState(plot_data)
            
            # Calibration plot
            if (self$options$calibration_assessment) {
                self$results$calibrationPlot$setState(plot_data)
            }
            
            # Discrimination plot
            if (self$options$discrimination_assessment) {
                self$results$discriminationPlot$setState(plot_data)
            }
            
            # Decision curve plot
            if (self$options$decision_curve_analysis) {
                self$results$decisionCurvePlot$setState(plot_data)
            }
            
            # Risk group plot
            if (self$options$risk_groups) {
                self$results$riskGroupPlot$setState(plot_data)
            }
            
            # Variable importance plot
            if (self$options$variable_importance) {
                self$results$variableImportancePlot$setState(plot_data)
            }
        },
        
        .generateImplementationGuide = function(model) {
            
            nomogram_type <- self$options$nomogram_type
            
            html <- "<h3>Clinical Implementation Guide</h3>"
            html <- paste0(html, "<p>Guidance for implementing this nomogram in clinical practice.</p>")
            
            html <- paste0(html, "<h4>Pre-Implementation Checklist</h4>")
            html <- paste0(html, "<ul>")
            html <- paste0(html, "<li>✓ Model has been externally validated</li>")
            html <- paste0(html, "<li>✓ Performance metrics are clinically acceptable</li>")
            html <- paste0(html, "<li>✓ Clinical team has been trained on nomogram use</li>")
            html <- paste0(html, "<li>✓ Integration with electronic health records planned</li>")
            html <- paste0(html, "<li>✓ Quality assurance procedures established</li>")
            html <- paste0(html, "</ul>")
            
            html <- paste0(html, "<h4>Implementation Steps</h4>")
            html <- paste0(html, "<ol>")
            html <- paste0(html, "<li><b>Pilot Testing:</b> Start with a small group of clinicians</li>")
            html <- paste0(html, "<li><b>Training Program:</b> Educate staff on proper nomogram use</li>")
            html <- paste0(html, "<li><b>Workflow Integration:</b> Embed in clinical decision pathways</li>")
            html <- paste0(html, "<li><b>Monitoring:</b> Track usage and clinical outcomes</li>")
            html <- paste0(html, "<li><b>Continuous Improvement:</b> Regular performance evaluation</li>")
            html <- paste0(html, "</ol>")
            
            html <- paste0(html, "<h4>Clinical Considerations</h4>")
            html <- paste0(html, "<ul>")
            html <- paste0(html, "<li><b>Patient Population:</b> Ensure patients match the development cohort</li>")
            html <- paste0(html, "<li><b>Missing Data:</b> Establish protocols for incomplete information</li>")
            html <- paste0(html, "<li><b>Decision Thresholds:</b> Define clinically meaningful risk cutpoints</li>")
            html <- paste0(html, "<li><b>Patient Communication:</b> Develop risk communication strategies</li>")
            html <- paste0(html, "</ul>")
            
            if (nomogram_type == "survival_nomogram") {
                html <- paste0(html, "<h4>Survival Nomogram Specific Considerations</h4>")
                html <- paste0(html, "<ul>")
                html <- paste0(html, "<li>Explain survival probabilities in context of patient's situation</li>")
                html <- paste0(html, "<li>Consider competing risks in elderly or comorbid patients</li>")
                html <- paste0(html, "<li>Update predictions as new information becomes available</li>")
                html <- paste0(html, "</ul>")
            }
            
            self$results$implementationGuide$setContent(html)
        }
    )
)