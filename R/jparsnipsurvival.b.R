
#' @title Parsnip Survival Model Wrappers
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import parsnip
#' @import censored
#' @import workflows
#' @import dials
#' @import tune
#' @import rsample
#' @import yardstick
#' @import survival
#' @export

jparsnipsurvivalClass <- R6::R6Class(
    "jparsnipsurvivalClass",
    inherit = jparsnipsurvivalBase,
    private = list(
        .model = NULL,
        .workflow = NULL,
        .predictions = NULL,
        .splits = NULL,
        .cv_results = NULL,
        
        .init = function() {
            if (is.null(self$options$time_var) || is.null(self$options$event_var)) {
                self$results$instructions$setContent(
                    "<html>
                    <head>
                    <style>
                        .main {
                            margin: 20px;
                            font-family: -apple-system,BlinkMacSystemFont,'Segoe UI',Roboto,Arial,sans-serif;
                        }
                        .header {
                            color: #3f51b5;
                            font-size: 18px;
                            font-weight: bold;
                            margin-bottom: 10px;
                        }
                        .description {
                            margin-bottom: 20px;
                            line-height: 1.5;
                        }
                        .requirements {
                            background-color: #f5f5f5;
                            padding: 15px;
                            border-left: 4px solid #2196f3;
                            margin-bottom: 20px;
                        }
                        .steps {
                            counter-reset: step-counter;
                        }
                        .step {
                            counter-increment: step-counter;
                            margin-bottom: 10px;
                            padding-left: 30px;
                            position: relative;
                        }
                        .step::before {
                            content: counter(step-counter);
                            position: absolute;
                            left: 0;
                            background-color: #2196f3;
                            color: white;
                            width: 20px;
                            height: 20px;
                            border-radius: 50%;
                            display: flex;
                            align-items: center;
                            justify-content: center;
                            font-size: 12px;
                            font-weight: bold;
                        }
                    </style>
                    </head>
                    <body>
                    <div class='main'>
                        <div class='header'>📊 Parsnip Survival Model Wrappers</div>
                        <div class='description'>
                            This module provides a unified interface to survival models using the parsnip package 
                            and tidymodels framework. It supports multiple survival model engines with standardized 
                            syntax for model fitting, prediction, and evaluation.
                        </div>
                        <div class='requirements'>
                            <strong>Required Variables:</strong><br>
                            • Time Variable: Survival time or time-to-event variable<br>
                            • Event Variable: Event indicator (0=censored, 1=event)<br>
                            • At least one predictor variable for modeling
                        </div>
                        <div class='steps'>
                            <div class='step'>Select your time and event variables</div>
                            <div class='step'>Choose predictor variables to include in the model</div>
                            <div class='step'>Select your preferred model engine (Cox, parametric, random forest)</div>
                            <div class='step'>Configure model-specific parameters as needed</div>
                            <div class='step'>Choose prediction type and output options</div>
                        </div>
                        <div style='margin-top: 20px; font-style: italic; color: #666;'>
                            This module leverages the tidymodels ecosystem for consistent, high-performance survival modeling.
                        </div>
                    </div>
                    </body>
                    </html>"
                )
            }
        },
        
        .run = function() {
            # Check required variables
            if (is.null(self$options$time_var) || is.null(self$options$event_var)) {
                return()
            }
            
            if (length(self$options$predictors) == 0) {
                self$results$instructions$setContent(
                    "<div style='padding: 20px; color: #d32f2f;'>
                    <strong>⚠️ Missing Predictor Variables</strong><br>
                    Please select at least one predictor variable to build the survival model.
                    </div>"
                )
                return()
            }
            
            # Get data
            data <- private$.prepareData()
            if (is.null(data)) return()
            
            # Split data if requested
            if (self$options$split_data) {
                private$.splitData(data)
                train_data <- rsample::training(private$.splits)
                test_data <- rsample::testing(private$.splits)
            } else {
                train_data <- data
                test_data <- data
            }
            
            # Create and fit model
            private$.createWorkflow(train_data)
            private$.fitModel(train_data)
            
            # Generate predictions
            private$.generatePredictions(test_data)
            
            # Cross validation if requested
            if (self$options$cross_validation) {
                private$.performCrossValidation(data)
            }
            
            # Populate results
            private$.populateModelInfo()
            private$.populateCoefficients()
            private$.populatePredictions()
            private$.populateMetrics()
            private$.populateVariableImportance()
            private$.populateSummary()
            private$.populateInterpretation()
        },
        
        .prepareData = function() {
            # Get variables
            time_var <- self$options$time_var
            event_var <- self$options$event_var
            predictors <- self$options$predictors
            
            # Extract data
            data <- self$data
            
            # Check if variables exist
            if (!time_var %in% names(data) || !event_var %in% names(data)) {
                return(NULL)
            }
            
            # Get clean dataset
            vars_to_use <- c(time_var, event_var, predictors)
            clean_data <- data[vars_to_use]
            
            # Remove missing values
            clean_data <- clean_data[complete.cases(clean_data), ]
            
            if (nrow(clean_data) == 0) {
                return(NULL)
            }
            
            # Ensure event variable is numeric (0/1)
            clean_data[[event_var]] <- as.numeric(as.character(clean_data[[event_var]]))
            
            # Ensure time variable is numeric
            clean_data[[time_var]] <- as.numeric(clean_data[[time_var]])
            
            return(clean_data)
        },
        
        .splitData = function(data) {
            set.seed(123)  # For reproducibility
            private$.splits <- rsample::initial_split(
                data, 
                prop = self$options$train_prop, 
                strata = self$options$event_var
            )
        },
        
        .createWorkflow = function(data) {
            # Create formula
            time_var <- self$options$time_var
            event_var <- self$options$event_var
            predictors <- self$options$predictors
            
            formula_string <- paste0("Surv(", time_var, ", ", event_var, ") ~ ", 
                                   paste(predictors, collapse = " + "))
            model_formula <- as.formula(formula_string)
            
            # Create model specification based on engine
            engine <- self$options$model_engine
            
            if (engine == "cox_survival") {
                model_spec <- parsnip::proportional_hazards(
                    mode = "censored regression"
                ) %>%
                parsnip::set_engine("survival")
                
            } else if (engine == "cox_glmnet") {
                model_spec <- parsnip::proportional_hazards(
                    mode = "censored regression",
                    penalty = self$options$penalty,
                    mixture = self$options$mixture
                ) %>%
                parsnip::set_engine("glmnet")
                
            } else if (engine == "parametric_survival") {
                model_spec <- parsnip::survival_reg(
                    mode = "censored regression",
                    dist = self$options$parametric_dist
                ) %>%
                parsnip::set_engine("survival")
                
            } else if (engine == "parametric_flexsurv") {
                model_spec <- parsnip::survival_reg(
                    mode = "censored regression",
                    dist = self$options$parametric_dist
                ) %>%
                parsnip::set_engine("flexsurv")
                
            } else if (engine == "random_forest") {
                model_spec <- parsnip::rand_forest(
                    mode = "censored regression",
                    trees = self$options$trees,
                    min_n = self$options$min_n
                ) %>%
                parsnip::set_engine("randomForestSRC")
            }
            
            # Create workflow
            private$.workflow <- workflows::workflow() %>%
                workflows::add_model(model_spec) %>%
                workflows::add_formula(model_formula)
        },
        
        .fitModel = function(data) {
            tryCatch({
                private$.model <- workflows::fit(private$.workflow, data = data)
            }, error = function(e) {
                self$results$instructions$setContent(
                    paste0("<div style='padding: 20px; color: #d32f2f;'>
                    <strong>❌ Model Fitting Error</strong><br>
                    ", e$message, "
                    </div>")
                )
                private$.model <- NULL
            })
        },
        
        .generatePredictions = function(data) {
            if (is.null(private$.model)) return()
            
            tryCatch({
                pred_type <- self$options$prediction_type
                private$.predictions <- predict(
                    private$.model, 
                    new_data = data,
                    type = pred_type
                )
                private$.predictions <- bind_cols(data, private$.predictions)
            }, error = function(e) {
                private$.predictions <- NULL
            })
        },
        
        .performCrossValidation = function(data) {
            if (is.null(private$.workflow)) return()
            
            tryCatch({
                set.seed(123)
                cv_folds <- rsample::vfold_cv(data, v = self$options$cv_folds)
                
                private$.cv_results <- tune::fit_resamples(
                    private$.workflow,
                    resamples = cv_folds,
                    metrics = yardstick::metric_set(yardstick::concordance_survival)
                )
            }, error = function(e) {
                private$.cv_results <- NULL
            })
        },
        
        .populateModelInfo = function() {
            if (!self$options$show_coefficients || is.null(private$.model)) return()
            
            table <- self$results$model_info
            
            info_data <- data.frame(
                Attribute = c("Model Engine", "Mode", "Formula", "Observations"),
                Value = c(
                    self$options$model_engine,
                    "Censored Regression",
                    private$.getFormulaString(),
                    nrow(private$.prepareData())
                ),
                stringsAsFactors = FALSE
            )
            
            for (i in seq_len(nrow(info_data))) {
                table$addRow(rowKey = i, values = info_data[i, ])
            }
        },
        
        .populateCoefficients = function() {
            if (!self$options$show_coefficients || is.null(private$.model)) return()
            
            table <- self$results$coefficients
            
            tryCatch({
                # Extract model coefficients
                model_fit <- private$.model$fit
                
                if (self$options$model_engine %in% c("cox_survival", "cox_glmnet")) {
                    # Cox model coefficients
                    coef_summary <- summary(model_fit)
                    coef_data <- as.data.frame(coef_summary$coefficients)
                    coef_data$Variable <- rownames(coef_data)
                    
                    if (ncol(coef_data) >= 5) {
                        names(coef_data) <- c("coef", "exp_coef", "se_coef", "z", "p", "Variable")
                        coef_data <- coef_data[c("Variable", "coef", "exp_coef", "se_coef", "z", "p")]
                    }
                    
                } else if (self$options$model_engine %in% c("parametric_survival", "parametric_flexsurv")) {
                    # Parametric model coefficients
                    coef_data <- data.frame(
                        Variable = names(coef(model_fit)),
                        Coefficient = coef(model_fit),
                        stringsAsFactors = FALSE
                    )
                    
                } else {
                    # Default coefficient extraction
                    coef_data <- data.frame(
                        Variable = c("Model coefficients not available for this engine"),
                        Value = c("Use variable importance instead"),
                        stringsAsFactors = FALSE
                    )
                }
                
                for (i in seq_len(nrow(coef_data))) {
                    table$addRow(rowKey = i, values = coef_data[i, ])
                }
                
            }, error = function(e) {
                table$addRow(rowKey = 1, values = list(
                    Variable = "Error extracting coefficients",
                    Value = e$message
                ))
            })
        },
        
        .populatePredictions = function() {
            if (!self$options$show_predictions || is.null(private$.predictions)) return()
            
            table <- self$results$predictions
            
            # Show first 50 predictions
            pred_data <- head(private$.predictions, 50)
            
            for (i in seq_len(nrow(pred_data))) {
                row_data <- as.list(pred_data[i, ])
                table$addRow(rowKey = i, values = row_data)
            }
        },
        
        .populateMetrics = function() {
            if (!self$options$show_metrics || is.null(private$.model)) return()
            
            table <- self$results$performance_metrics
            
            tryCatch({
                if (!is.null(private$.predictions)) {
                    # Calculate C-index if possible
                    time_var <- self$options$time_var
                    event_var <- self$options$event_var
                    
                    surv_obj <- Surv(private$.predictions[[time_var]], 
                                   private$.predictions[[event_var]])
                    
                    if (".pred_linear_pred" %in% names(private$.predictions)) {
                        pred_values <- private$.predictions$.pred_linear_pred
                        c_index <- survival::concordance(surv_obj ~ pred_values)$concordance
                        
                        table$addRow(rowKey = 1, values = list(
                            Metric = "Concordance Index (C-index)",
                            Value = round(c_index, 4)
                        ))
                    }
                }
                
                # Add cross-validation results if available
                if (!is.null(private$.cv_results) && self$options$cross_validation) {
                    cv_metrics <- tune::collect_metrics(private$.cv_results)
                    
                    for (i in seq_len(nrow(cv_metrics))) {
                        table$addRow(rowKey = i + 1, values = list(
                            Metric = paste("CV", cv_metrics$.metric[i]),
                            Value = round(cv_metrics$mean[i], 4)
                        ))
                    }
                }
                
            }, error = function(e) {
                table$addRow(rowKey = 1, values = list(
                    Metric = "Error calculating metrics",
                    Value = e$message
                ))
            })
        },
        
        .populateVariableImportance = function() {
            if (self$options$model_engine != "random_forest" || 
                !self$options$show_coefficients || 
                is.null(private$.model)) return()
            
            table <- self$results$variable_importance
            
            tryCatch({
                # Extract variable importance for random forest
                rf_fit <- private$.model$fit
                importance_scores <- rf_fit$importance
                
                if (!is.null(importance_scores)) {
                    importance_data <- data.frame(
                        Variable = names(importance_scores),
                        Importance = as.numeric(importance_scores),
                        stringsAsFactors = FALSE
                    )
                    
                    # Sort by importance
                    importance_data <- importance_data[order(-importance_data$Importance), ]
                    
                    for (i in seq_len(nrow(importance_data))) {
                        table$addRow(rowKey = i, values = importance_data[i, ])
                    }
                }
                
            }, error = function(e) {
                table$addRow(rowKey = 1, values = list(
                    Variable = "Error extracting variable importance",
                    Importance = e$message
                ))
            })
        },
        
        .populateSummary = function() {
            summary_html <- private$.createSummaryHTML()
            self$results$model_summary$setContent(summary_html)
        },
        
        .populateInterpretation = function() {
            interpretation_html <- private$.createInterpretationHTML()
            self$results$interpretation$setContent(interpretation_html)
        },
        
        .createSummaryHTML = function() {
            if (is.null(private$.model)) {
                return("<div style='padding: 20px; color: #666;'>No model fitted yet.</div>")
            }
            
            data <- private$.prepareData()
            n_obs <- nrow(data)
            n_events <- sum(data[[self$options$event_var]])
            n_censored <- n_obs - n_events
            
            engine_name <- switch(self$options$model_engine,
                "cox_survival" = "Cox Proportional Hazards (survival)",
                "cox_glmnet" = "Regularized Cox Model (glmnet)",
                "parametric_survival" = "Parametric Survival Model (survival)",
                "parametric_flexsurv" = "Parametric Survival Model (flexsurv)",
                "random_forest" = "Random Survival Forest",
                self$options$model_engine
            )
            
            paste0("
            <div style='font-family: -apple-system,BlinkMacSystemFont,Segoe UI,Roboto,Arial,sans-serif; padding: 20px;'>
                <h3 style='color: #3f51b5; margin-bottom: 15px;'>📈 Model Summary</h3>
                <div style='background-color: #f8f9fa; padding: 15px; border-radius: 8px; margin-bottom: 15px;'>
                    <strong>Model Type:</strong> ", engine_name, "<br>
                    <strong>Total Observations:</strong> ", n_obs, "<br>
                    <strong>Events:</strong> ", n_events, " (", round(n_events/n_obs*100, 1), "%)<br>
                    <strong>Censored:</strong> ", n_censored, " (", round(n_censored/n_obs*100, 1), "%)<br>
                    <strong>Predictors:</strong> ", length(self$options$predictors), "
                </div>
                
                <div style='margin-top: 15px;'>
                    <strong>Formula:</strong><br>
                    <code style='background-color: #f1f3f4; padding: 8px; border-radius: 4px; display: block; margin-top: 5px;'>
                        ", private$.getFormulaString(), "
                    </code>
                </div>
            </div>")
        },
        
        .createInterpretationHTML = function() {
            if (is.null(private$.model)) {
                return("<div style='padding: 20px; color: #666;'>No model available for interpretation.</div>")
            }
            
            engine <- self$options$model_engine
            interpretation <- switch(engine,
                "cox_survival" = "The Cox proportional hazards model estimates the hazard ratio for each predictor, representing the multiplicative effect on the baseline hazard. Hazard ratios > 1 indicate increased risk, while < 1 indicate decreased risk.",
                "cox_glmnet" = "The regularized Cox model uses L1/L2 penalties to prevent overfitting and perform variable selection. This is especially useful with many predictors or multicollinearity.",
                "parametric_survival" = "Parametric survival models assume a specific distribution for survival times, providing more precise estimates when the distributional assumption is correct.",
                "parametric_flexsurv" = "FlexSurv provides flexible parametric survival models with advanced distributional options and smooth hazard functions.",
                "random_forest" = "Random survival forests provide non-parametric estimates without distributional assumptions, capturing complex non-linear relationships and interactions.",
                "This survival model provides estimates for the relationship between predictors and survival outcomes."
            )
            
            paste0("
            <div style='font-family: -apple-system,BlinkMacSystemFont,Segoe UI,Roboto,Arial,sans-serif; padding: 20px;'>
                <h3 style='color: #3f51b5; margin-bottom: 15px;'>🔍 Clinical Interpretation</h3>
                <div style='background-color: #e8f5e8; padding: 15px; border-radius: 8px; border-left: 4px solid #4caf50;'>
                    ", interpretation, "
                </div>
                
                <div style='margin-top: 20px; padding: 15px; background-color: #fff3e0; border-radius: 8px; border-left: 4px solid #ff9800;'>
                    <strong>⚠️ Important Considerations:</strong><br>
                    • Verify model assumptions are met for your data<br>
                    • Consider external validation of results<br>
                    • Interpret results in clinical context<br>
                    • Check for potential confounders not included in the model
                </div>
            </div>")
        },
        
        .getFormulaString = function() {
            time_var <- self$options$time_var
            event_var <- self$options$event_var
            predictors <- self$options$predictors
            
            paste0("Surv(", time_var, ", ", event_var, ") ~ ", 
                   paste(predictors, collapse = " + "))
        },
        
        .plot_survival_curves = function(image, ggtheme, theme, ...) {
            if (is.null(private$.model) || self$options$prediction_type != "survival") return()
            
            # This would create survival curves visualization
            # Implementation depends on specific model type and prediction format
            # Placeholder for now
            plot <- ggplot2::ggplot() + 
                ggplot2::geom_text(ggplot2::aes(x = 0.5, y = 0.5, label = "Survival curves plot\nwould be generated here"), 
                                 size = 5) +
                ggplot2::xlim(0, 1) + ggplot2::ylim(0, 1) +
                ggplot2::theme_void()
            
            print(plot)
            TRUE
        },
        
        .plot_residuals = function(image, ggtheme, theme, ...) {
            if (is.null(private$.model)) return()
            
            # This would create residual analysis plots
            # Implementation depends on specific model type
            # Placeholder for now
            plot <- ggplot2::ggplot() + 
                ggplot2::geom_text(ggplot2::aes(x = 0.5, y = 0.5, label = "Residual analysis plots\nwould be generated here"), 
                                 size = 5) +
                ggplot2::xlim(0, 1) + ggplot2::ylim(0, 1) +
                ggplot2::theme_void()
            
            print(plot)
            TRUE
        }
    )
)
