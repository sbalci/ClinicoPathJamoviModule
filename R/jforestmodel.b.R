
#' @title Forest Plot Visualization
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import ggplot2
#' @export

jforestmodelClass <- R6::R6Class(
    "jforestmodelClass",
    inherit = jforestmodelBase,
    private = list(
        .model = NULL,
        .model_data = NULL,
        .coefficients = NULL,
        
        .init = function() {
            if (is.null(self$options$dependent_var) || length(self$options$predictor_vars) == 0) {
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
                        <div class='header'>🌲 Forest Plot Visualization</div>
                        <div class='description'>
                            Create professional forest plots from regression models including linear,
                            logistic, and survival models. Visualize coefficients, confidence intervals,
                            and effect sizes with customizable formatting and layout options.
                        </div>
                        <div class='requirements'>
                            <strong>Required Variables:</strong><br>
                            • Dependent Variable: Outcome variable for modeling<br>
                            • Predictor Variables: Independent variables for the model<br>
                            • For Cox models: Time and Event variables
                        </div>
                        <div class='steps'>
                            <div class='step'>Select your dependent variable</div>
                            <div class='step'>Choose predictor variables for the model</div>
                            <div class='step'>Select appropriate model type (linear, logistic, Cox)</div>
                            <div class='step'>Customize plot appearance and options</div>
                            <div class='step'>Review the forest plot and model results</div>
                        </div>
                        <div style='margin-top: 20px; font-style: italic; color: #666;'>
                            Uses the forestmodel package for professional publication-quality plots.
                        </div>
                    </div>
                    </body>
                    </html>"
                )
            }
        },
        
        .run = function() {
            # Check required variables
            if (is.null(self$options$dependent_var) || length(self$options$predictor_vars) == 0) {
                return()
            }
            
            # Get data
            data <- private$.prepareData()
            if (is.null(data)) return()
            
            # Fit model
            private$.fitModel(data)
            
            # Populate results
            private$.populateModelSummary()
            private$.populateCoefficientsTable()
            private$.populateDiagnostics()
            private$.populateInterpretation()
        },
        
        .prepareData = function() {
            # Get variables
            dependent_var <- self$options$dependent_var
            predictor_vars <- self$options$predictor_vars
            
            # Extract data
            data <- self$data
            
            # Get clean dataset
            vars_to_use <- c(dependent_var, predictor_vars)
            
            # Add time and event variables for Cox model
            if (self$options$model_type == "coxph") {
                if (!is.null(self$options$time_var) && !is.null(self$options$event_var)) {
                    vars_to_use <- c(vars_to_use, self$options$time_var, self$options$event_var)
                } else {
                    self$results$instructions$setContent(
                        "<div style='padding: 20px; color: #d32f2f;'>
                        <strong>⚠️ Missing Cox Model Variables</strong><br>
                        Cox proportional hazards model requires both Time and Event variables.
                        </div>"
                    )
                    return(NULL)
                }
            }
            
            # Check if variables exist
            missing_vars <- vars_to_use[!vars_to_use %in% names(data)]
            if (length(missing_vars) > 0) {
                return(NULL)
            }
            
            clean_data <- data[vars_to_use]
            
            # Remove missing values
            clean_data <- clean_data[complete.cases(clean_data), ]
            
            if (nrow(clean_data) == 0) {
                return(NULL)
            }
            
            return(clean_data)
        },
        
        .fitModel = function(data) {
            model_type <- self$options$model_type
            dependent_var <- self$options$dependent_var
            predictor_vars <- self$options$predictor_vars
            
            # Create formula
            formula_string <- paste(dependent_var, "~", paste(predictor_vars, collapse = " + "))
            model_formula <- as.formula(formula_string)
            
            tryCatch({
                if (model_type == "lm") {
                    private$.model <- lm(model_formula, data = data)
                    
                } else if (model_type == "glm") {
                    family_name <- self$options$family
                    family_obj <- switch(family_name,
                        "binomial" = binomial(),
                        "poisson" = poisson(),
                        "gaussian" = gaussian(),
                        "gamma" = Gamma(),
                        binomial()
                    )
                    private$.model <- glm(model_formula, data = data, family = family_obj)
                    
                } else if (model_type == "coxph") {
                    # Cox model requires survival formula
                    time_var <- self$options$time_var
                    event_var <- self$options$event_var
                    cox_formula_string <- paste0("Surv(", time_var, ", ", event_var, ") ~ ", 
                                                paste(predictor_vars, collapse = " + "))
                    cox_formula <- as.formula(cox_formula_string)
                    
                    if (requireNamespace("survival", quietly = TRUE)) {
                        private$.model <- survival::coxph(cox_formula, data = data)
                    } else {
                        self$results$instructions$setContent(
                            "<div style='padding: 20px; color: #d32f2f;'>
                            <strong>❌ Package Required</strong><br>
                            The 'survival' package is required for Cox regression models.
                            </div>"
                        )
                        return()
                    }
                }
                
                private$.model_data <- data
                
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
        
        .populateModelSummary = function() {
            if (!self$options$show_summary || is.null(private$.model)) return()
            
            table <- self$results$model_summary
            
            model_type <- self$options$model_type
            n_obs <- nrow(private$.model_data)
            n_predictors <- length(self$options$predictor_vars)
            
            # Model type description
            model_desc <- switch(model_type,
                "lm" = "Linear Regression",
                "glm" = paste("Generalized Linear Model (", self$options$family, ")", sep=""),
                "coxph" = "Cox Proportional Hazards",
                model_type
            )
            
            table$addRow(rowKey = 1, values = list(
                Attribute = "Model Type",
                Value = model_desc
            ))
            
            table$addRow(rowKey = 2, values = list(
                Attribute = "Observations",
                Value = as.character(n_obs)
            ))
            
            table$addRow(rowKey = 3, values = list(
                Attribute = "Predictors",
                Value = as.character(n_predictors)
            ))
            
            # Add model-specific statistics
            if (model_type %in% c("lm", "glm")) {
                model_summary <- summary(private$.model)
                
                if (model_type == "lm") {
                    table$addRow(rowKey = 4, values = list(
                        Attribute = "R-squared",
                        Value = sprintf("%.4f", model_summary$r.squared)
                    ))
                    
                    table$addRow(rowKey = 5, values = list(
                        Attribute = "Adjusted R-squared",
                        Value = sprintf("%.4f", model_summary$adj.r.squared)
                    ))
                }
                
                if ("deviance" %in% names(model_summary)) {
                    table$addRow(rowKey = 6, values = list(
                        Attribute = "Residual Deviance",
                        Value = sprintf("%.2f", model_summary$deviance)
                    ))
                }
            }
        },
        
        .populateCoefficientsTable = function() {
            if (is.null(private$.model)) return()
            
            table <- self$results$coefficients_table
            
            tryCatch({
                # Extract coefficients
                model_summary <- summary(private$.model)
                coef_matrix <- model_summary$coefficients
                
                # Calculate confidence intervals
                conf_level <- self$options$confidence_level
                ci <- confint(private$.model, level = conf_level)
                
                # Handle exponentiation
                exponentiate <- self$options$exponentiate
                
                for (i in 1:nrow(coef_matrix)) {
                    var_name <- rownames(coef_matrix)[i]
                    coef_val <- coef_matrix[i, 1]
                    se_val <- coef_matrix[i, 2]
                    p_val <- coef_matrix[i, ncol(coef_matrix)]
                    
                    ci_lower <- ci[i, 1]
                    ci_upper <- ci[i, 2]
                    
                    if (exponentiate) {
                        coef_val <- exp(coef_val)
                        ci_lower <- exp(ci_lower)
                        ci_upper <- exp(ci_upper)
                    }
                    
                    table$addRow(rowKey = i, values = list(
                        Variable = var_name,
                        Coefficient = coef_val,
                        SE = se_val,
                        CI_Lower = ci_lower,
                        CI_Upper = ci_upper,
                        P_Value = p_val
                    ))
                }
                
            }, error = function(e) {
                table$addRow(rowKey = 1, values = list(
                    Variable = "Error extracting coefficients",
                    Coefficient = NA,
                    SE = NA,
                    CI_Lower = NA,
                    CI_Upper = NA,
                    P_Value = NA
                ))
            })
        },
        
        .populateDiagnostics = function() {
            if (!self$options$model_type %in% c("lm", "glm") || is.null(private$.model)) return()
            
            table <- self$results$model_diagnostics
            
            tryCatch({
                model_summary <- summary(private$.model)
                
                if (self$options$model_type == "lm") {
                    # F-statistic
                    f_stat <- model_summary$fstatistic
                    f_p_value <- pf(f_stat[1], f_stat[2], f_stat[3], lower.tail = FALSE)
                    
                    table$addRow(rowKey = 1, values = list(
                        Diagnostic = "F-statistic",
                        Value = sprintf("%.3f (p = %.4f)", f_stat[1], f_p_value),
                        Interpretation = ifelse(f_p_value < 0.05, "Significant", "Not significant")
                    ))
                    
                    # Residual standard error
                    table$addRow(rowKey = 2, values = list(
                        Diagnostic = "Residual Std. Error",
                        Value = sprintf("%.4f", model_summary$sigma),
                        Interpretation = "Lower is better"
                    ))
                }
                
                # AIC for both lm and glm
                aic_val <- AIC(private$.model)
                table$addRow(rowKey = 3, values = list(
                    Diagnostic = "AIC",
                    Value = sprintf("%.2f", aic_val),
                    Interpretation = "Lower indicates better fit"
                ))
                
            }, error = function(e) {
                table$addRow(rowKey = 1, values = list(
                    Diagnostic = "Error",
                    Value = e$message,
                    Interpretation = "Check model specification"
                ))
            })
        },
        
        .populateInterpretation = function() {
            if (!self$options$show_interpretation) return()
            
            model_type <- self$options$model_type
            exponentiate <- self$options$exponentiate
            
            interpretation <- switch(model_type,
                "lm" = "Linear regression coefficients represent the change in the dependent variable for a one-unit increase in each predictor, holding other variables constant.",
                "glm" = ifelse(exponentiate,
                    "Exponentiated coefficients represent odds ratios (for logistic) or rate ratios (for Poisson). Values > 1 indicate positive associations, < 1 indicate negative associations.",
                    "GLM coefficients are on the link function scale. Consider exponentiating for easier interpretation."
                ),
                "coxph" = ifelse(exponentiate,
                    "Exponentiated coefficients represent hazard ratios. Values > 1 indicate increased hazard (shorter survival), < 1 indicate decreased hazard (longer survival).",
                    "Cox regression coefficients are log hazard ratios. Consider exponentiating for hazard ratio interpretation."
                ),
                "General regression model coefficients and their confidence intervals."
            )
            
            confidence_level <- self$options$confidence_level
            conf_percent <- confidence_level * 100
            
            interpretation_html <- paste0("
            <div style='font-family: -apple-system,BlinkMacSystemFont,Segoe UI,Roboto,Arial,sans-serif; padding: 20px;'>
                <h3 style='color: #3f51b5; margin-bottom: 15px;'>🔍 Results Interpretation</h3>
                <div style='background-color: #e8f5e8; padding: 15px; border-radius: 8px; border-left: 4px solid #4caf50;'>
                    <strong>Coefficient Interpretation:</strong><br>
                    ", interpretation, "
                </div>
                
                <div style='margin-top: 15px; background-color: #f3e5f5; padding: 15px; border-radius: 8px; border-left: 4px solid #9c27b0;'>
                    <strong>Confidence Intervals (", conf_percent, "%):</strong><br>
                    The confidence intervals indicate the range of plausible values for each coefficient. 
                    If the interval includes ", ifelse(exponentiate, "1", "0"), " (null effect), 
                    the association may not be statistically significant.
                </div>
                
                <div style='margin-top: 15px; padding: 15px; background-color: #fff3e0; border-radius: 8px; border-left: 4px solid #ff9800;'>
                    <strong>⚠️ Important Considerations:</strong><br>
                    • Check model assumptions before interpreting results<br>
                    • Consider potential confounders not included in the model<br>
                    • Correlation does not imply causation<br>
                    • Clinical significance may differ from statistical significance
                </div>
            </div>")
            
            self$results$interpretation$setContent(interpretation_html)
        },
        
        .plot_forest = function(image, ggtheme, theme, ...) {
            if (is.null(private$.model)) return()
            
            # Check if forestmodel package is available
            if (!requireNamespace("forestmodel", quietly = TRUE)) {
                # Create placeholder plot
                plot <- ggplot2::ggplot() + 
                    ggplot2::geom_text(ggplot2::aes(x = 0.5, y = 0.5, 
                        label = "forestmodel package required\nfor forest plot visualization"), 
                        size = 5, color = "#666") +
                    ggplot2::xlim(0, 1) + ggplot2::ylim(0, 1) +
                    ggplot2::theme_void() +
                    ggplot2::ggtitle("Forest Plot") +
                    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
                
                print(plot)
                return(TRUE)
            }
            
            tryCatch({
                # Create forest plot using forestmodel
                plot <- forestmodel::forest_model(
                    model = private$.model,
                    exponentiate = self$options$exponentiate,
                    factor_separate_line = self$options$factor_separate_line
                )
                
                # Customize plot appearance
                if (self$options$plot_title != "Forest Plot") {
                    plot <- plot + ggplot2::ggtitle(self$options$plot_title)
                }
                
                if (self$options$x_axis_label != "") {
                    plot <- plot + ggplot2::xlab(self$options$x_axis_label)
                }
                
                # Apply color scheme
                color_scheme <- self$options$color_scheme
                if (color_scheme != "default") {
                    color_value <- switch(color_scheme,
                        "blue" = "#2196F3",
                        "red" = "#F44336",
                        "green" = "#4CAF50",
                        "purple" = "#9C27B0",
                        "custom" = self$options$custom_color,
                        "#2196F3"
                    )
                    # Note: forestmodel has limited customization options
                    # Additional styling would require manual ggplot2 modifications
                }
                
                print(plot)
                TRUE
                
            }, error = function(e) {
                # Create error plot
                plot <- ggplot2::ggplot() + 
                    ggplot2::geom_text(ggplot2::aes(x = 0.5, y = 0.5, 
                        label = paste("Error creating forest plot:\n", e$message)), 
                        size = 4, color = "#d32f2f") +
                    ggplot2::xlim(0, 1) + ggplot2::ylim(0, 1) +
                    ggplot2::theme_void() +
                    ggplot2::ggtitle("Forest Plot Error") +
                    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
                
                print(plot)
                TRUE
            })
        }
    )
)
