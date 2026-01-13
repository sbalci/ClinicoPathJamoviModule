
# This file is a generated template, your changes will not be overwritten

nonparametricregressionClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "nonparametricregressionClass",
    inherit = nonparametricregressionBase,
    private = list(
        .run = function() {

            # 1. Provide instructions if inputs are missing
            if (is.null(self$options$outcome) || is.null(self$options$predictors)) {
                
                todo <- "
                    <br>Welcome to Non-parametric Regression
                    <br><br>
                    This tool provides flexible regression modeling when parametric assumptions (like linearity) are not met.
                    <br><br>
                    To get started:
                    <br>1. Select the <b>Outcome Variable</b> (Continuous)
                    <br>2. Select one or more <b>Predictor Variables</b>
                    <br>3. Choose the <b>Non-parametric Regression Method</b> (e.g., LOESS, Spline)
                    <br>4. (Optional) Select a <b>Grouping Variable</b> for stratified analysis
                "
                self$results$methodsExplanation$setContent(todo)
                return()
            }

            # 2. Get and clean data
            mydata <- self$data
            outcomeVar <- self$options$outcome
            predVars <- self$options$predictors
            groupVar <- self$options$grouping_variable
            
            vars <- c(outcomeVar, predVars, groupVar)
            mydata <- jmvcore::naOmit(mydata[vars])
            
            if (nrow(mydata) == 0) {
                self$results$methodsExplanation$setContent("No valid data rows found after removing missing values.")
                return()
            }

            # 3. Model Summary Table
            tableSummary <- self$results$modelSummary
            tableSummary$addRow(rowKey=1, values=list(parameter="Method", value=self$options$regression_type, interpretation="Smoothing technique"))
            tableSummary$addRow(rowKey=2, values=list(parameter="Outcome", value=outcomeVar, interpretation="Response variable"))
            tableSummary$addRow(rowKey=3, values=list(parameter="Observations", value=as.character(nrow(mydata)), interpretation="Total valid data points"))

            # 4. Perform Analysis
            regType <- self$options$regression_type
            
            # Simple implementation for LOESS (supports multiple predictors)
            if (regType == "loess") {
                
                formula <- as.formula(paste(jmvcore::composeTerm(outcomeVar), "~", paste(jmvcore::composeTerms(predVars), collapse = " + ")))
                
                model <- try(loess(formula, data = mydata, span = self$options$loess_span, degree = if(self$options$loess_degree == "linear") 1 else 2), silent = TRUE)
                
                if (!inherits(model, "try-error")) {
                    
                    # 5. Model Fit Statistics
                    tableFit <- self$results$modelFit
                    # LOESS doesn't provide R2 directly easily, but we can compute it
                    res <- residuals(model)
                    r2 <- 1 - (sum(res^2) / sum((mydata[[outcomeVar]] - mean(mydata[[outcomeVar]]))^2))
                    
                    tableFit$addRow(rowKey=1, values=list(statistic="R-squared (approx)", value=r2, interpretation="Proportion of variance explained"))
                    tableFit$addRow(rowKey=2, values=list(statistic="Residual Standard Error", value=model$s, interpretation="Standard deviation of residuals"))
                    tableFit$addRow(rowKey=3, values=list(statistic="Effective df", value=model$enp, interpretation="Complexity of the smooth"))
                    
                } else {
                    self$results$methodsExplanation$setContent(paste("LOESS Error:", attr(model, "condition")$message))
                }
            } else if (regType == "spline" && length(predVars) == 1) {
                
                # Smoothing Spline (Univariate)
                model <- try(smooth.spline(x = mydata[[predVars[1]]], y = mydata[[outcomeVar]], df = self$options$spline_df), silent = TRUE)
                
                if (!inherits(model, "try-error")) {
                    tableFit <- self$results$modelFit
                    tableFit$addRow(rowKey=1, values=list(statistic="Smoothing Parameter (λ)", value=model$lambda, interpretation="Penalty applied to curvature"))
                    tableFit$addRow(rowKey=2, values=list(statistic="Generalized CV Score", value=model$cv.crit, interpretation="Estimate of prediction error"))
                    
                    # Bandwidth Table
                    tableBW <- self$results$bandwidthSelection
                    tableBW$addRow(rowKey=1, values=list(
                        predictor = predVars[1],
                        selected_bandwidth = model$spar,
                        gcv_score = model$cv.crit,
                        selection_method = "GCV"
                    ))
                }
            } else {
                 self$results$methodsExplanation$setContent(paste("Method", regType, "with", length(predVars), "predictors is not yet fully implemented in this preview. Try LOESS or univariate Spline."))
            }

            # 6. Methods Explanation
            explanation <- paste0("
                <h3>Non-parametric Regression</h3>
                <p>Non-parametric regression does not assume a specific functional form (like a straight line) for the relationship between variables. 
                Instead, it lets the data 'speak for itself' to find the best fit.</p>
                <ul>
                    <li><b>LOESS:</b> Fits multiple local regressions in a sliding window across the data.</li>
                    <li><b>Splines:</b> Uses piecewise polynomials with a penalty for complexity (smoothing splines).</li>
                </ul>
                <p>These methods are especially useful for biomarkers, growth curves, and identifying non-linear patterns in clinical data.</p>
            ")
            self$results$methodsExplanation$setContent(explanation)

        })
)
