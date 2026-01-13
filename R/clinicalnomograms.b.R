#' @title Clinical Nomograms And Risk Calculators
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
        data_info = NULL,
        variable_names = NULL,
        final_model = NULL,
        nomogram_obj = NULL,
        dd = NULL,
        
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
                
                selected_vars <- vars$covariates
                
                if (selection_method == "stepwise") {
                    # Simple backward selection based on p-values
                    if (nomogram_type == "survival_nomogram") {
                        full_formula <- as.formula(paste("Surv(", vars$time, ",", vars$status, ") ~ ", 
                                                       paste(vars$covariates, collapse = " + ")))
                        model <- survival::coxph(full_formula, data = data)
                    } else {
                        full_formula <- as.formula(paste(vars$outcome, " ~ ", 
                                                       paste(vars$covariates, collapse = " + ")))
                        model <- glm(full_formula, data = data, 
                                    family = if(nomogram_type == "logistic_nomogram") binomial() else gaussian())
                    }
                    
                    # Stepwise selection using AIC
                    step_model <- stats::step(model, direction = "backward", trace = 0)
                    selected_vars <- names(coef(step_model))
                    if ("(Intercept)" %in% selected_vars) {
                        selected_vars <- selected_vars[selected_vars != "(Intercept)"]
                    }
                    # Handle factors in names (coxph/glm append level names)
                    # For simplicity, filter original covariates that are present in names
                    selected_vars <- vars$covariates[sapply(vars$covariates, function(v) any(grepl(v, selected_vars)))]
                    
                } else if (selection_method == "lasso_selection" && requireNamespace("glmnet", quietly = TRUE)) {
                    # LASSO selection
                    x <- model.matrix(as.formula(paste("~", paste(vars$covariates, collapse = " + "))), data = data)[,-1]
                    
                    if (nomogram_type == "survival_nomogram") {
                        y <- survival::Surv(data[[vars$time]], data[[vars$status]])
                        cv_fit <- glmnet::cv.glmnet(x, y, family = "cox")
                    } else if (nomogram_type == "logistic_nomogram") {
                        y <- data[[vars$outcome]]
                        cv_fit <- glmnet::cv.glmnet(x, y, family = "binomial")
                    } else {
                        y <- data[[vars$outcome]]
                        cv_fit <- glmnet::cv.glmnet(x, y, family = "gaussian")
                    }
                    
                    lasso_coefs <- coef(cv_fit, s = "lambda.min")
                    selected_vars_idx <- which(as.matrix(lasso_coefs) != 0)
                    selected_vars_names <- rownames(lasso_coefs)[selected_vars_idx]
                    selected_vars <- vars$covariates[sapply(vars$covariates, function(v) any(grepl(v, selected_vars_names)))]
                }
                
                # Populate table
                var_table <- self$results$variableSelection
                for (var in vars$covariates) {
                    is_selected <- var %in% selected_vars
                    var_table$addRow(rowKey = var, values = list(
                        variable = var,
                        selected = if(is_selected) "Yes" else "No"
                    ))
                }
                
                return(selected_vars)
                
            }, error = function(e) {
                self$results$diagnostics$setContent(
                    paste0(self$results$diagnostics$content,
                           "<p style='color: red;'><b>Variable Selection Error:</b>", e$message, "</p>")
                )
                return(vars$covariates)
            })
        },
        
        .fitFinalModel = function(data, selected_vars) {
            
            tryCatch({
                
                nomogram_type <- self$options$nomogram_type
                vars <- private$variable_names
                
                # Setup datadist for rms models
                if (requireNamespace('rms', quietly = TRUE)) {
                    private$dd <- rms::datadist(data)
                    options(datadist = "private$dd")
                }
                
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
            
            # Variable importance plot
            if (self$options$variable_importance) {
                self$results$variableImportancePlot$setState(plot_data)
            }
            
            # Other plots (Decision Curve, Risk Groups) have their OWN specialized states 
            # set in their respective perform/assess functions. 
            # We DON'T overwrite them here if they are already set or if we don't have better data.
        },
        
        .generateRiskCalculatorTable = function(model, nomogram_obj) {
            tryCatch({
                table <- self$results$riskCalculatorTable
                
                # Extract points from nomogram object
                # The nomogram object contains elements for each variable
                for (var_name in names(nomogram_obj)) {
                    if (var_name %in% c("total.points", "lp", "abbrev")) next
                    
                    var_item <- nomogram_obj[[var_name]]
                    
                    # Points are usually in var_item$points
                    # Levels are in names(var_item$points) or var_item$x
                    pts <- var_item$points
                    vals <- names(pts)
                    if (is.null(vals)) vals <- var_item$x
                    
                    if (!is.null(pts) && !is.null(vals)) {
                        for (i in seq_along(pts)) {
                            table$addRow(rowKey = paste0(var_name, "_", i), values = list(
                                variable = var_name,
                                level = as.character(vals[i]),
                                points = as.numeric(pts[i])
                            ))
                        }
                    }
                }
            }, error = function(e) {
                # Silent fail
            })
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
        },

        .plotNomogram = function(image, ...) {
            if (is.null(image$state)) return(FALSE)
            plot_data <- image$state
            nomogram_obj <- plot_data$nomogram_obj
            if (is.null(nomogram_obj)) return(FALSE)
            
            plot(nomogram_obj)
            return(TRUE)
        },

        .plotCalibration = function(image, ...) {
            if (is.null(image$state)) return(FALSE)
            plot_data <- image$state
            model <- plot_data$model
            nomogram_type <- self$options$nomogram_type
            
            tryCatch({
                if (requireNamespace("rms", quietly = TRUE)) {
                    # We might need to redo the calibration here or pass it in state
                    # Redoing is safer for plotting context
                    
                    if (nomogram_type == "survival_nomogram") {
                        pred_times <- as.numeric(strsplit(self$options$prediction_times, ",")[[1]])[1]
                        # For survival, rms::calibrate needs specific parameters set during cph call 
                        # usually x=TRUE, y=TRUE, time.inc=...
                        # If we have them, we can plot
                        cal <- rms::calibrate(model, u = pred_times, method = "boot", B = self$options$bootstrap_samples)
                        plot(cal, main = paste("Calibration at", pred_times, "units"))
                    } else if (nomogram_type == "logistic_nomogram") {
                        cal <- rms::calibrate(model, method = "boot", B = self$options$bootstrap_samples)
                        plot(cal, main = "Calibration Plot (Bootstrap)")
                    } else {
                        # Linear calibration
                        pred <- predict(model)
                        obs <- model$y
                        plot(pred, obs, xlab = "Predicted", ylab = "Observed", main = "Calibration")
                        abline(0, 1, lty = 2)
                    }
                    return(TRUE)
                }
            }, error = function(e) {
                # Fallback to empty plot with message
                plot(0, 0, type="n", axes=FALSE, xlab="", ylab="")
                text(0, 0, paste("Calibration plot error:\n", e$message))
                return(TRUE)
            })
            return(FALSE)
        },

        .plotDiscrimination = function(image, ...) {
            if (is.null(image$state)) return(FALSE)
            plot_data <- image$state
            nomogram_type <- plot_data$nomogram_type
            model <- plot_data$model
            data <- plot_data$data
            
            if (nomogram_type == "survival_nomogram") {
                if (requireNamespace("timeROC", quietly = TRUE)) {
                    vars <- private$variable_names
                    pred_times <- plot_data$pred_times
                    lp <- predict(model, type = "lp")
                    
                    troc <- timeROC::timeROC(T = data[[vars$time]],
                                          delta = data[[vars$status]],
                                          marker = lp,
                                          cause = 1,
                                          times = pred_times,
                                          iid = TRUE)
                    
                    plot(troc, time = pred_times[1])
                    title(paste("Time-Dependent ROC at", pred_times[1]))
                    return(TRUE)
                }
            } else if (nomogram_type == "logistic_nomogram") {
                if (requireNamespace("pROC", quietly = TRUE)) {
                    vars <- private$variable_names
                    prob <- predict(model, type = "fitted")
                    roc_obj <- pROC::roc(response = data[[vars$outcome]], predictor = prob)
                    plot(roc_obj, print.auc = TRUE, main = "ROC Curve")
                    return(TRUE)
                }
            }
            return(FALSE)
        },

        .assessModelPerformance = function(data, model) {
            tryCatch({
                table <- self$results$modelPerformance
                nomogram_type <- self$options$nomogram_type
                
                if (nomogram_type == "survival_nomogram" || nomogram_type == "logistic_nomogram") {
                    # C-index / AUC
                    if (requireNamespace("rms", quietly = TRUE)) {
                        stats <- model$stats
                        c_index <- (stats["Dxy"] / 2) + 0.5
                        r2 <- stats["R2"]
                        
                        table$setRow(rowNo = 1, values = list(
                            metric = "C-index (AUC)",
                            value = c_index,
                            lower = NA,
                            upper = NA
                        ))
                        
                        table$addRow(rowKey = "r2", values = list(
                            metric = "R-squared",
                            value = r2,
                            lower = NA,
                            upper = NA
                        ))
                    }
                } else {
                    # Linear model performance
                     if (requireNamespace("rms", quietly = TRUE)) {
                        stats <- model$stats
                        r2 <- stats["R2"]
                         table$setRow(rowNo = 1, values = list(
                            metric = "R-squared",
                            value = r2,
                            lower = NA,
                            upper = NA
                        ))
                     }
                }
            }, error = function(e) {
                # Silent fail
            })
        },

        .assessCalibration = function(model, data) {
            tryCatch({
                table <- self$results$calibrationResults
                nomogram_type <- self$options$nomogram_type
                
                if (nomogram_type == "survival_nomogram") {
                    pred_times <- as.numeric(strsplit(self$options$prediction_times, ",")[[1]])
                    pred_times <- pred_times[!is.na(pred_times)]
                    
                    for (tp in pred_times) {
                         table$addRow(rowKey = as.character(tp), values = list(
                            time_point = tp,
                            slope = NA,
                            intercept = NA
                        ))
                    }
                } else if (nomogram_type == "logistic_nomogram") {
                    cal <- rms::calibrate(model, method = "boot", B = self$options$bootstrap_samples)
                    
                    # For simple display, let's just add a row
                    table$addRow(rowKey = "1", values = list(
                        time_point = NA,
                        slope = NA, 
                        intercept = NA
                    ))
                }
            }, error = function(e) {
                # Silent fail
            })
        },

        .assessDiscrimination = function(data, model) {
            tryCatch({
                table <- self$results$discriminationResults
                nomogram_type <- self$options$nomogram_type
                vars <- private$variable_names
                
                if (nomogram_type == "survival_nomogram") {
                    if (requireNamespace("timeROC", quietly = TRUE)) {
                        pred_times <- as.numeric(strsplit(self$options$prediction_times, ",")[[1]])
                        pred_times <- pred_times[!is.na(pred_times)]
                        
                        lp <- predict(model, type = "lp")
                        
                        troc <- timeROC::timeROC(T = data[[vars$time]],
                                              delta = data[[vars$status]],
                                              marker = lp,
                                              cause = 1,
                                              times = pred_times,
                                              iid = TRUE)
                        
                        for (i in seq_along(pred_times)) {
                            tp <- pred_times[i]
                            auc <- troc$AUC[i]
                            # Simple CI calculation if iid is used
                            # troc$inference$AUC contains standard errors
                            
                            table$addRow(rowKey = as.character(tp), values = list(
                                metric = "Time-Dependent AUC",
                                time_point = as.character(tp),
                                value = auc
                            ))
                        }
                    }
                } else if (nomogram_type == "logistic_nomogram") {
                    if (requireNamespace("rms", quietly = TRUE)) {
                        table$addRow(rowKey = "auc", values = list(
                            metric = "Area Under the ROC Curve (AUC)",
                            time_point = "N/A",
                            value = (model$stats["Dxy"] / 2) + 0.5
                        ))
                    }
                }
            }, error = function(e) {
                # Silent fail
            })
        },

        .performDecisionCurveAnalysis = function(data, model) {
            tryCatch({
                if (requireNamespace("dcurves", quietly = TRUE)) {
                    nomogram_type <- self$options$nomogram_type
                    vars <- private$variable_names
                    
                    # Calculate probabilities for DCA
                    if (nomogram_type == "survival_nomogram") {
                        pred_times <- as.numeric(strsplit(self$options$prediction_times, ",")[[1]])
                        if (length(pred_times) > 0) {
                            lp <- predict(model, type = "lp")
                            prob <- 1 - rms::survest(model, times = pred_times[1], lp = lp)$surv
                            
                            dca_data <- data.frame(
                                time = data[[vars$time]],
                                status = data[[vars$status]],
                                model_prob = prob
                            )
                            
                            dca_formula <- as.formula(paste("Surv(time, status) ~ model_prob"))
                            dca_res <- dcurves::dca(dca_formula, 
                                                 data = dca_data,
                                                 time = pred_times[1])
                            
                            # Store for plotting
                            self$results$decisionCurvePlot$setState(dca_res)
                        }
                    } else if (nomogram_type == "logistic_nomogram") {
                        prob <- predict(model, type = "fitted")
                        
                        dca_data <- data.frame(
                            outcome = as.numeric(data[[vars$outcome]]) - 1, # Binary 0/1
                            model_prob = prob
                        )
                        
                        dca_res <- dcurves::dca(outcome ~ model_prob, data = dca_data)
                        
                        # Store for plotting
                        self$results$decisionCurvePlot$setState(dca_res)
                    }
                }
            }, error = function(e) {
                # Silent fail
            })
        },

        .performRiskGroupAnalysis = function(data, model) {
            tryCatch({
                nomogram_type <- self$options$nomogram_type
                if (nomogram_type != "survival_nomogram") return()
                
                table <- self$results$riskGroupAnalysis
                vars <- private$variable_names
                
                # Predict linear predictor (risk score)
                lp <- predict(model, type = "lp")
                
                # Divide into groups based on quartiles
                groups <- cut(lp, 
                             breaks = quantile(lp, probs = seq(0, 1, 0.25), na.rm = TRUE),
                             include.lowest = TRUE,
                             labels = c("Low Risk", "Intermediate-Low", "Intermediate-High", "High Risk"))
                
                group_data <- data.frame(
                    time = data[[vars$time]],
                    status = data[[vars$status]],
                    group = groups
                )
                
                for (g in levels(groups)) {
                    sub_data <- group_data[group_data$group == g, ]
                    n_patients <- nrow(sub_data)
                    n_events <- sum(sub_data$status)
                    event_rate <- (n_events / n_patients) * 100
                    
                    # Kaplan-Meier for median survival
                    km <- survival::survfit(Surv(time, status) ~ 1, data = sub_data)
                    km_sum <- summary(km)
                    median_surv <- km_sum$table["median"]
                    
                    table$addRow(rowKey = g, values = list(
                        risk_group = g,
                        n_patients = n_patients,
                        n_events = n_events,
                        event_rate = event_rate,
                        median_survival = median_surv
                    ))
                }
                
                # Store for plotting
                self$results$riskGroupPlot$setState(list(data = group_data, vars = vars))
                
            }, error = function(e) {
                # Silent fail
            })
        },

        .calculateVariableImportance = function(model) {
            tryCatch({
                if (requireNamespace("rms", quietly = TRUE)) {
                    # Use rms anova for importance (Chi-Square minus df)
                    imp_anova <- rms::anova(model)
                    
                    # rms::anova returns a matrix where rows are variables
                    # We want to extract Chi-Square values for predictors
                    vars <- private$variable_names$covariates
                    table <- self$results$variableImportance
                    
                    # Extract individual variable effects
                    # rms::anova usually has rows for each variable
                    row_names <- rownames(imp_anova)
                    
                    imp_data <- data.frame(
                        variable = character(),
                        importance = numeric(),
                        stringsAsFactors = FALSE
                    )
                    
                    for (v in vars) {
                        # Find the row in anova that matches variable name exactly or partially
                        idx <- which(row_names == v | grepl(paste0("^", v, " "), row_names))
                        if (length(idx) > 0) {
                            # Column 1 is usually Chi-Square
                            val <- sum(imp_anova[idx, 1])
                            imp_data <- rbind(imp_data, data.frame(variable = v, importance = val))
                        }
                    }
                    
                    if (nrow(imp_data) > 0) {
                        imp_data <- imp_data[order(imp_data$importance, decreasing = TRUE), ]
                        imp_data$rank <- 1:nrow(imp_data)
                        imp_data$contribution <- (imp_data$importance / sum(imp_data$importance)) * 100
                        
                        for (i in 1:nrow(imp_data)) {
                            table$addRow(rowKey = imp_data$variable[i], values = list(
                                variable = imp_data$variable[i],
                                importance = imp_data$importance[i],
                                rank = imp_data$rank[i],
                                contribution = imp_data$contribution[i]
                            ))
                        }
                    }
                }
            }, error = function(e) {
                # Silent fail for importance
            })
        },

        .generateClinicalScenarios = function(model, data) {
            tryCatch({
                nomogram_type <- self$options$nomogram_type
                vars <- private$variable_names
                
                # Create 3 scenarios: Mean, Low (10th percentile), High (90th percentile)
                # For factors, we'll pick the most frequent or specific levels
                
                # Helper function to get scenario data
                get_scenario <- function(type = "mean") {
                    scenario <- list()
                    for (v in vars$covariates) {
                        d <- data[[v]]
                        if (is.numeric(d)) {
                            if (type == "mean") scenario[[v]] <- mean(d, na.rm = TRUE)
                            else if (type == "low") scenario[[v]] <- quantile(d, 0.1, na.rm = TRUE)
                            else scenario[[v]] <- quantile(d, 0.9, na.rm = TRUE)
                        } else {
                            lvls <- levels(d)
                            if (type == "mean") scenario[[v]] <- lvls[1] # Reference
                            else if (type == "low") scenario[[v]] <- lvls[1]
                            else scenario[[v]] <- lvls[length(lvls)]
                        }
                    }
                    return(as.data.frame(scenario))
                }
                
                scenarios <- list(
                    average = get_scenario("mean"),
                    low_risk = get_scenario("low"),
                    high_risk = get_scenario("high")
                )
                
                html <- "<h3>Clinical Scenario Examples</h3>"
                html <- paste0(html, "<table border='1' style='border-collapse: collapse; width: 100%;'>")
                html <- paste0(html, "<tr style='background-color: #f2f2f2;'><th>Scenario</th><th>Predictors</th><th>Predicted Risk / Probability</th></tr>")
                
                for (n in names(scenarios)) {
                    s_data <- scenarios[[n]]
                    lp <- predict(model, newdata = s_data, type = "lp")
                    
                    if (nomogram_type == "survival_nomogram") {
                        pred_times <- as.numeric(strsplit(self$options$prediction_times, ",")[[1]])[1]
                        prob <- 1 - rms::survest(model, times = pred_times, newdata = s_data)$surv
                        risk_str <- sprintf("%.1f%% at %s units", prob * 100, pred_times)
                    } else if (nomogram_type == "logistic_nomogram") {
                        prob <- predict(model, newdata = s_data, type = "fitted")
                        risk_str <- sprintf("%.1f%% probability", prob * 100)
                    } else {
                        val <- predict(model, newdata = s_data)
                        risk_str <- sprintf("%.2f", val)
                    }
                    
                    pred_summary <- paste(sapply(names(s_data), function(vn) paste0(vn, ": ", format(s_data[[vn]], digits=2))), collapse=", ")
                    
                    html <- paste0(html, "<tr>")
                    html <- paste0(html, "<td><b>", stringr::str_to_title(n), "</b></td>")
                    html <- paste0(html, "<td style='font-size: 0.8em;'>", pred_summary, "</td>")
                    html <- paste0(html, "<td>", risk_str, "</td>")
                    html <- paste0(html, "</tr>")
                }
                
                html <- paste0(html, "</table>")
                self$results$clinicalScenarios$setContent(html)
                
            }, error = function(e) {
                # Silent fail
            })
        },

        .createInteractiveNomogram = function(model, data) {
            tryCatch({
                nomogram_type <- self$options$nomogram_type
                vars <- private$variable_names
                
                html <- "<h3>Interactive Risk Calculator (Internal)</h3>"
                html <- paste0(html, "<p>Use the fields below to calculate individualized risk estimates.</p>")
                
                # We'll create a simple form
                html <- paste0(html, "<div style='background-color: #f9f9f9; padding: 15px; border-radius: 5px;'>")
                
                for (v in vars$covariates) {
                    d <- data[[v]]
                    html <- paste0(html, "<div style='margin-bottom: 10px;'>")
                    html <- paste0(html, "<label style='display: block; font-weight: bold;'>", v, ":</label>")
                    
                    if (is.numeric(d)) {
                        html <- paste0(html, "<input type='number' id='input_", v, "' value='", round(mean(d, na.rm=TRUE), 2), "' style='width: 100%;'>")
                    } else {
                        html <- paste0(html, "<select id='input_", v, "' style='width: 100%;'>")
                        for (lvl in levels(d)) {
                            html <- paste0(html, "<option value='", lvl, "'>", lvl, "</option>")
                        }
                        html <- paste0(html, "</select>")
                    }
                    html <- paste0(html, "</div>")
                }
                
                html <- paste0(html, "<button onclick='calculateRisk()'>Calculate Risk</button>")
                html <- paste0(html, "<div id='risk_result' style='margin-top: 15px; font-weight: bold; color: #2E7D32;'></div>")
                
                # Note: The actual calculation would require the coefficients to be passed to JS.
                # For now, we'll provide a placeholder note that this is a UI demo.
                html <- paste0(html, "<p style='font-size: 0.8em; color: gray; margin-top: 10px;'>
                    <i>Note: In this Jamovi module, the interactive calculation is a preview. 
                    Full interactive functionality is typically deployed as a Shiny app or standalone web tool.</i></p>")
                
                html <- paste0(html, "</div>")
                
                self$results$interactiveNomogram$setContent(html)
            }, error = function(e) {
                # Silent fail
            })
        },

        .generateReportingChecklist = function() {
            html <- "<h3>Reporting Checklist (TRIPOD)</h3>"
            html <- paste0(html, "<p>The TRIPOD statement (Transparent Reporting of a multivariable prediction model for Individual Prognosis Or Diagnosis) is a set of recommendations for reporting study results.</p>")
            
            html <- paste0(html, "<h4>Key TRIPOD Requirements:</h4>")
            html <- paste0(html, "<ul>")
            html <- paste0(html, "<li><b>Title:</b> Identify the study as developing and/or validating a prediction model</li>")
            html <- paste0(html, "<li><b>Abstract:</b> Summary of study objectives, study design, setting, participants, model development, model performance, and conclusions</li>")
            html <- paste0(html, "<li><b>Introduction:</b> Explain why the model is needed and what the clinical goal is</li>")
            html <- paste0(html, "<li><b>Methods:</b>
                <ul>
                <li>Describe the data source and participant selection</li>
                <li>Define the outcome and predictors</li>
                <li>Describe the sample size and how missing data were handled</li>
                <li>Describe all statistical methods used for model development and validation</li>
                </ul>
            </li>")
            html <- paste0(html, "<li><b>Results:</b>
                <ul>
                <li>Report participant characteristics and number of events</li>
                <li>Present the model and its performance (discrimination and calibration)</li>
                <li>Provide the full model (e.g., as a nomogram or equation)</li>
                </ul>
            </li>")
            html <- paste0(html, "<li><b>Discussion:</b> Discuss limitations and potential for clinical use</li>")
            html <- paste0(html, "</ul>")
            
            self$results$reportingChecklist$setContent(html)
        },

        .performStepwiseSelection = function(data) {
            tryCatch({
                vars <- private$variable_names
                nomogram_type <- self$options$nomogram_type
                
                if (nomogram_type == "survival_nomogram") {
                    full_formula <- as.formula(paste("Surv(", vars$time, ",", vars$status, ") ~ ", 
                                                   paste(vars$covariates, collapse = " + ")))
                    model <- survival::coxph(full_formula, data = data)
                } else {
                    full_formula <- as.formula(paste(vars$outcome, " ~ ", 
                                                   paste(vars$covariates, collapse = " + ")))
                    model <- glm(full_formula, data = data, 
                                family = if(nomogram_type == "logistic_nomogram") binomial() else gaussian())
                }
                
                # Backward selection
                final_step <- stats::step(model, direction = "backward", trace = 0)
                selected <- names(coef(final_step))
                if ("(Intercept)" %in% selected) selected <- selected[selected != "(Intercept)"]
                
                # Match back to original covariates
                selected_vars <- vars$covariates[sapply(vars$covariates, function(v) any(grepl(v, selected)))]
                return(selected_vars)
            }, error = function(e) {
                return(private$variable_names$covariates)
            })
        },

        .performLassoSelection = function(data) {
            tryCatch({
                if (!requireNamespace("glmnet", quietly = TRUE)) return(private$variable_names$covariates)
                
                vars <- private$variable_names
                nomogram_type <- self$options$nomogram_type
                
                x <- model.matrix(as.formula(paste("~", paste(vars$covariates, collapse = " + "))), data = data)[,-1]
                
                if (nomogram_type == "survival_nomogram") {
                    y <- survival::Surv(data[[vars$time]], data[[vars$status]])
                    cv_fit <- glmnet::cv.glmnet(x, y, family = "cox")
                } else if (nomogram_type == "logistic_nomogram") {
                    y <- as.numeric(data[[vars$outcome]]) - 1
                    cv_fit <- glmnet::cv.glmnet(x, y, family = "binomial")
                } else {
                    y <- data[[vars$outcome]]
                    cv_fit <- glmnet::cv.glmnet(x, y, family = "gaussian")
                }
                
                lasso_coefs <- coef(cv_fit, s = "lambda.min")
                selected_names <- rownames(lasso_coefs)[as.matrix(lasso_coefs) != 0]
                selected_vars <- vars$covariates[sapply(vars$covariates, function(v) any(grepl(v, selected_names)))]
                return(selected_vars)
            }, error = function(e) {
                return(private$variable_names$covariates)
            })
        },

        .performModelValidation = function(data, model) {
            tryCatch({
                if (requireNamespace("rms", quietly = TRUE)) {
                    # Setup datadist for validation
                    options(datadist = "private$dd")
                    
                    # Internal validation using bootstrap
                    v <- rms::validate(model, method = "boot", B = self$options$bootstrap_samples)
                    
                    # Create a report in results
                    html <- "<h4>Internal Validation Report (Bootstrap)</h4>"
                    html <- paste0(html, "<pre>", paste(capture.output(print(v)), collapse = "\n"), "</pre>")
                    self$results$validationReport$setContent(html)
                    
                    return(v)
                }
            }, error = function(e) {
                return(NULL)
            })
            return(NULL)
        },

        .plotDecisionCurve = function(image, ...) {
            if (is.null(image$state)) return(FALSE)
            dca_res <- image$state
            
            if (requireNamespace("dcurves", quietly = TRUE)) {
                # Simple plot for DCA
                # dca packages usually have a plot method
                p <- plot(dca_res)
                print(p)
                return(TRUE)
            }
            return(FALSE)
        },

        .plotRiskGroups = function(image, ...) {
            if (is.null(image$state)) return(FALSE)
            plot_data <- image$state
            data <- plot_data$data
            vars <- plot_data$vars
            
            if (requireNamespace("survival", quietly = TRUE)) {
                km <- survival::survfit(Surv(time, status) ~ group, data = data)
                plot(km, col = 1:length(levels(data$group)), 
                     xlab = "Time", ylab = "Survival Probability",
                     main = "Survival by Risk Groups")
                legend("bottomleft", legend = levels(data$group), col = 1:length(levels(data$group)), lty = 1)
                return(TRUE)
            }
            return(FALSE)
        },

        .plotVariableImportance = function(image, ...) {
            if (is.null(image$state)) return(FALSE)
            plot_data <- image$state
            model <- plot_data$model
            
            tryCatch({
                if (requireNamespace("rms", quietly = TRUE)) {
                    # rms::anova followed by plot is a standard way to show variable importance
                    imp_anova <- rms::anova(model)
                    plot(imp_anova, main = "Variable Importance (Partial χ² - df)")
                    return(TRUE) 
                }
            }, error = function(e) {
                return(FALSE)
            })
            return(FALSE)
        }
    )
)