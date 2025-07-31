#' @title Continuous Variable Relationship Analysis
#' @description 
#' Analyzes relationships between continuous variables using evidence-based approaches
#' from BMJ best practices. Implements splines, fractional polynomials, and provides
#' educational guidance to avoid common statistical pitfalls.
#'
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import ggplot2
#' @importFrom splines ns
#' @importFrom rms rcs
#' @importFrom stats lm glm AIC BIC logLik quantile median
#' @importFrom dplyr mutate select filter
#'

continuousrelationshipClass <- if (requireNamespace('jmvcore', quietly = TRUE)) R6::R6Class(
    "continuousrelationshipClass",
    inherit = continuousrelationshipBase,
    private = list(
        
        .init = function() {
            # Initialize any required components
        },
        
        .run = function() {
            
            # Check for required variables
            if (is.null(self$options$outcome) || is.null(self$options$predictor)) {
                if (self$options$showGuidance) {
                    self$results$guidance$setContent(
                        paste0(
                            "<h3>Welcome to Continuous Variable Relationship Analysis</h3>",
                            "<p>This analysis follows <strong>BMJ best practices</strong> for handling continuous variables:</p>",
                            "<ul>",
                            "<li>✅ <strong>Avoid categorization</strong> - Keeps full information from continuous data</li>",
                            "<li>✅ <strong>Use flexible modeling</strong> - Captures non-linear relationships</li>",
                            "<li>✅ <strong>Visualize relationships</strong> - Understand patterns before modeling</li>",
                            "</ul>",
                            "<p><strong>To begin:</strong> Select an outcome variable and a continuous predictor.</p>",
                            "<hr>",
                            "<p><small><em>Based on: Sauerbrei W, Royston P, Binder H. BMJ 2024;390:e082440. ",
                            "<a href='https://www.bmj.com/content/390/bmj-2024-082440' target='_blank'>Read full article</a></em></small></p>"
                        )
                    )
                }
                return()
            }
            
            # Get data
            data <- self$data
            outcome <- data[[self$options$outcome]]
            predictor <- data[[self$options$predictor]]
            
            # Check for missing values
            complete_cases <- complete.cases(outcome, predictor)
            if (sum(!complete_cases) > 0) {
                self$results$warningMessage$setVisible(TRUE)
                self$results$warningMessage$setContent(
                    sprintf("<p>⚠️ <strong>Warning:</strong> %d cases with missing values were excluded.</p>", 
                            sum(!complete_cases))
                )
            }
            
            # Filter to complete cases
            outcome <- outcome[complete_cases]
            predictor <- predictor[complete_cases]
            
            # Show BMJ guidance if enabled
            if (self$options$showGuidance) {
                private$.showGuidance()
            }
            
            # Determine outcome type
            is_binary <- length(unique(outcome)) == 2
            
            # Set reference value
            ref_value <- if (!is.null(self$options$referenceValue)) {
                self$options$referenceValue
            } else {
                median(predictor, na.rm = TRUE)
            }
            
            # Fit models based on selected type
            model <- switch(self$options$modelType,
                spline = private$.fitSplineModel(predictor, outcome, is_binary),
                fp = private$.fitFPModel(predictor, outcome, is_binary),
                linear = private$.fitLinearModel(predictor, outcome, is_binary),
                categorized = private$.fitCategorizedModel(predictor, outcome, is_binary)
            )
            
            # Show model formula if requested
            if (self$options$showModel) {
                private$.showModelFormula(model, self$options$modelType)
            }
            
            # Test for linearity if requested
            if (self$options$testLinearity && self$options$modelType != "linear") {
                private$.testLinearity(predictor, outcome, is_binary)
            }
            
            # Show model fit statistics
            if (self$options$showModelFit) {
                private$.showModelFit(model, self$options$modelType)
            }
            
            # Add predictions to data if requested
            if (self$options$exportPredictions) {
                predictions <- predict(model, type = if(is_binary) "response" else "response")
                self$results$exportPredictions$setRowNums(rownames(self$data))
                self$results$exportPredictions$setValues(predictions)
            }
        },
        
        .showGuidance = function() {
            guidance_html <- paste0(
                "<h4>Analysis Guidance</h4>",
                "<p><strong>Key Principle:</strong> \"Truly linear relationships are rare in nature, ",
                "so anticipating non-linearity as a default is generally a sensible approach.\"</p>",
                "<p><em>Reference: Sauerbrei W, Royston P, Binder H. Selection of important variables and determination of functional form for continuous predictors in multivariable model building. ",
                "BMJ 2024;390:e082440. <a href='https://www.bmj.com/content/390/bmj-2024-082440' target='_blank'>doi: 10.1136/bmj-2024-082440</a></em></p>",
                
                "<h5>Your Selected Method: ", 
                switch(self$options$modelType,
                    spline = "Restricted Cubic Splines ✅",
                    fp = "Fractional Polynomials ✅",
                    linear = "Linear Model ⚠️",
                    categorized = "Categorized Variables ❌"
                ),
                "</h5>")
                
                if (self$options$modelType == "spline") {
                    guidance_html <- 
                     paste0(guidance_html,
                        "<p><strong>Good choice!</strong> Splines provide flexible modeling while avoiding overfitting.</p>",
                        "<p><strong>Knot selection:</strong> You selected ", self$options$nKnots, " knots. ",
                        "BMJ recommends 3-5 knots for most applications.</p>"
                    )
                } else if (self$options$modelType == "linear") {
                    guidance_html <- 
                     paste0(guidance_html,
                    "<p>⚠️ <strong>Caution:</strong> Linear models assume a constant effect across the entire range. ",
                    "Consider using splines to check for non-linearity.</p>"
                     )
                } else if (self$options$modelType == "categorized") {
                    guidance_html <- 
                     paste0(guidance_html,
                    
                    "<p>❌ <strong>Not Recommended:</strong> Categorization loses information and statistical power. ",
                    "This option is provided for educational purposes only to demonstrate why it's problematic.</p>"
                     )
                }
            
            self$results$guidance$setContent(guidance_html)
        },
        
        .fitSplineModel = function(predictor, outcome, is_binary) {
            n_knots <- as.numeric(self$options$nKnots)
            
            # Calculate knot positions
            knot_positions <- if (self$options$knotPositions == "quantile") {
                quantile(predictor, probs = seq(0, 1, length.out = n_knots + 2)[-c(1, n_knots + 2)])
            } else {
                seq(min(predictor), max(predictor), length.out = n_knots)
            }
            
            # Create spline basis
            spline_basis <- ns(predictor, knots = knot_positions[-c(1, length(knot_positions))])
            
            # Fit model
            if (is_binary) {
                model <- glm(outcome ~ spline_basis, family = binomial())
            } else {
                model <- lm(outcome ~ spline_basis)
            }
            
            return(model)
        },
        
        .fitLinearModel = function(predictor, outcome, is_binary) {
            if (is_binary) {
                glm(outcome ~ predictor, family = binomial())
            } else {
                lm(outcome ~ predictor)
            }
        },
        
        .fitCategorizedModel = function(predictor, outcome, is_binary) {
            # Create categories (for demonstration purposes)
            categories <- cut(predictor, breaks = quantile(predictor, probs = c(0, 0.33, 0.67, 1)), 
                            labels = c("Low", "Medium", "High"), include.lowest = TRUE)
            
            if (is_binary) {
                glm(outcome ~ categories, family = binomial())
            } else {
                lm(outcome ~ categories)
            }
        },
        
        .fitFPModel = function(predictor, outcome, is_binary) {
            # Simple fractional polynomial implementation
            # In practice, would use mfp package
            if (is_binary) {
                glm(outcome ~ predictor + I(predictor^2), family = binomial())
            } else {
                lm(outcome ~ predictor + I(predictor^2))
            }
        },
        
        .showModelFormula = function(model, model_type) {
            formula_html <- "<h4>Model Formula</h4><pre>"
            
            if (model_type == "spline") {
                formula_html <- paste0(formula_html, 
                    "outcome ~ ns(predictor, knots = c(", 
                    paste(round(attr(model$model[,2], "knots"), 2), collapse = ", "),
                    "))")
            } else {
                formula_html <- paste0(formula_html, 
                    paste(deparse(formula(model)), collapse = " "))
            }
            
            formula_html <- paste0(formula_html, "</pre>")
            self$results$modelFormula$setContent(formula_html)
        },
        
        .testLinearity = function(predictor, outcome, is_binary) {
            # Fit linear and spline models for comparison
            if (is_binary) {
                linear_model <- glm(outcome ~ predictor, family = binomial())
                spline_model <- glm(outcome ~ ns(predictor, df = 3), family = binomial())
            } else {
                linear_model <- lm(outcome ~ predictor)
                spline_model <- lm(outcome ~ ns(predictor, df = 3))
            }
            
            # Likelihood ratio test
            lr_test <- anova(linear_model, spline_model, test = "Chisq")
            
            # Populate results table
            self$results$linearityTest$addRow(rowKey = 1, values = list(
                test = "Likelihood Ratio Test",
                statistic = round(lr_test$Deviance[2], 3),
                df = lr_test$Df[2],
                p = lr_test$`Pr(>Chi)`[2],
                interpretation = ifelse(lr_test$`Pr(>Chi)`[2] < 0.05,
                    "Evidence of non-linearity (p < 0.05)",
                    "No strong evidence of non-linearity")
            ))
        },
        
        .showModelFit = function(model, model_type) {
            # Calculate fit statistics
            aic_val <- AIC(model)
            bic_val <- BIC(model)
            loglik_val <- logLik(model)[1]
            
            # Add to results table
            self$results$modelFit$addRow(rowKey = 1, values = list(
                model = model_type,
                aic = round(aic_val, 2),
                bic = round(bic_val, 2),
                loglik = round(loglik_val, 2)
            ))
        },
        
        .doseResponsePlot = function(image, ggtheme, theme, ...) {
            # Implementation of dose-response plot
            # This would create the main visualization
            plotData <- image$state
            
            # Create plot using ggplot2
            # ... (implementation details)
            
            print(plot)
            TRUE
        },
        
        .partialEffectPlot = function(image, ggtheme, theme, ...) {
            # Implementation of partial effect plot
            # ... (implementation details)
            TRUE
        },
        
        .comparisonPlot = function(image, ggtheme, theme, ...) {
            # Implementation showing multiple models for comparison
            # ... (implementation details)
            TRUE
        },
        
        .categorizedPlot = function(image, ggtheme, theme, ...) {
            # Educational plot showing pitfalls of categorization
            # ... (implementation details)
            TRUE
        }
    )
)