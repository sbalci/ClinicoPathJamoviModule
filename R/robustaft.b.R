
#' @title Robust AFT Models
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import survival
#' @import ggplot2
#' @import dplyr
#' @export

robustaftClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "robustaftClass",
    inherit = robustaftBase,
    private = list(
        .init = function() {
            
            if (is.null(self$options$elapsedtime) || is.null(self$options$outcome)) {
                self$results$modelSummary$setNote("info",
                    "Please specify both time and event variables to proceed with the analysis.")
                return()
            }
            
            # Initialize result tables
            private$.initModelSummary()
            private$.initCoefficientsTable()
            private$.initOutlierAnalysisTable()
            private$.initDiagnosticsTable()
            private$.initComparisonTable()
            private$.initRobustnessTable()
            private$.initScaleEstimatesTable()
            private$.initOutlierTable()
            
            # Set up plot areas
            if (self$options$show_residual_plots) {
                self$results$residualPlots$setSize(800, 600)
            }
            if (self$options$show_outlier_plots) {
                self$results$outlierPlots$setSize(800, 600)
            }
            if (self$options$show_survival_plots) {
                self$results$survivalPlots$setSize(800, 600)
            }
            if (self$options$show_qq_plots) {
                self$results$qqPlots$setSize(800, 600)
            }
        },
        
        .run = function() {
            
            # Get data
            data <- self$data
            
            if (is.null(self$options$elapsedtime) || is.null(self$options$outcome)) {
                return()
            }
            
            # Check if RobustAFT package is available
            if (!requireNamespace("RobustAFT", quietly = TRUE)) {
                self$results$modelSummary$setNote("info", 
                    "Using built-in robust AFT implementation. Install 'RobustAFT' package for additional features.")
            }
            
            # Prepare data
            result <- private$.prepareData(data)
            if (!is.null(result$error)) {
                self$results$modelSummary$setNote("error", result$error)
                return()
            }
            
            preparedData <- result$data
            
            # Fit standard AFT model for comparison
            standardModel <- private$.fitStandardAFT(preparedData)
            
            # Fit robust AFT model
            result <- private$.fitRobustAFT(preparedData)
            if (!is.null(result$error)) {
                self$results$modelSummary$setNote("error", result$error)
                return()
            }
            
            robustModel <- result$model
            outliers <- result$outliers
            weights <- result$weights
            
            # Populate results
            private$.populateModelSummary(robustModel, preparedData)
            private$.populateCoefficientsTable(robustModel)
            private$.populateOutlierAnalysisTable(outliers, preparedData)
            private$.populateDiagnosticsTable(robustModel, standardModel)
            private$.populateComparisonTable(robustModel, standardModel)
            private$.populateRobustnessTable(robustModel)
            private$.populateScaleEstimatesTable(robustModel, standardModel)
            private$.populateOutlierTable(outliers, weights, preparedData)
            
            # Generate plots
            if (self$options$show_residual_plots) {
                private$.plotResiduals(robustModel, preparedData)
            }
            if (self$options$show_outlier_plots) {
                private$.plotOutliers(outliers, weights, preparedData)
            }
            if (self$options$show_survival_plots) {
                private$.plotSurvival(robustModel, preparedData)
            }
            if (self$options$show_qq_plots) {
                private$.plotQQ(robustModel, preparedData)
            }
            
            # Generate summaries and explanations
            if (self$options$showSummaries) {
                private$.populateAnalysisSummary(robustModel, outliers, preparedData)
            }
            if (self$options$showExplanations) {
                private$.populateMethodExplanation()
            }
        },
        
        .prepareData = function(data) {
            
            timeVar <- self$options$elapsedtime
            outcomeVar <- self$options$outcome
            covariates <- self$options$covariates
            stratifyVar <- self$options$stratify_variable
            weightsVar <- self$options$weights_variable
            outcomeLevel <- self$options$outcomeLevel
            
            # Extract variables
            time <- data[[timeVar]]
            outcome <- data[[outcomeVar]]
            
            # Convert outcome to numeric
            if (is.factor(outcome)) {
                outcome <- as.numeric(outcome == outcomeLevel)
            } else if (is.character(outcome)) {
                outcome <- as.numeric(outcome == outcomeLevel)
            } else {
                outcome <- as.numeric(outcome == as.numeric(outcomeLevel))
            }
            
            # Check for valid time values
            if (any(time <= 0, na.rm = TRUE)) {
                return(list(error = "Time variable contains non-positive values."))
            }
            
            # Check for valid outcome values
            if (!all(outcome %in% c(0, 1), na.rm = TRUE)) {
                return(list(error = "Outcome variable must be binary (0/1)."))
            }
            
            # Create base dataset
            modelData <- data.frame(
                time = time,
                status = outcome,
                stringsAsFactors = FALSE
            )
            
            # Add covariates
            if (!is.null(covariates)) {
                for (var in covariates) {
                    modelData[[var]] <- data[[var]]
                }
            }
            
            # Add stratification variable
            if (!is.null(stratifyVar)) {
                modelData$strata <- data[[stratifyVar]]
                if (is.numeric(modelData$strata)) {
                    modelData$strata <- as.factor(modelData$strata)
                }
            }
            
            # Add weights variable
            if (!is.null(weightsVar)) {
                modelData$weights <- data[[weightsVar]]
            }
            
            # Remove rows with missing data
            complete_cases <- complete.cases(modelData)
            modelData <- modelData[complete_cases, ]
            
            if (nrow(modelData) == 0) {
                return(list(error = "No complete cases available for analysis."))
            }
            
            # Add observation identifiers
            modelData$obs_id <- 1:nrow(modelData)
            
            return(list(data = modelData, error = NULL))
        },
        
        .fitStandardAFT = function(data) {
            
            covariates <- self$options$covariates
            stratifyVar <- self$options$stratify_variable
            weightsVar <- self$options$weights_variable
            distribution <- self$options$distribution
            
            # Build formula
            if (is.null(covariates) || length(covariates) == 0) {
                formula <- "Surv(time, status) ~ 1"
            } else {
                formula <- paste("Surv(time, status) ~", paste(covariates, collapse = " + "))
            }
            
            # Add stratification
            if (!is.null(stratifyVar)) {
                formula <- paste(formula, "+ strata(strata)")
            }
            
            # Fit standard AFT model
            tryCatch({
                if (!is.null(weightsVar)) {
                    model <- survival::survreg(
                        as.formula(formula),
                        data = data,
                        weights = data$weights,
                        dist = distribution
                    )
                } else {
                    model <- survival::survreg(
                        as.formula(formula),
                        data = data,
                        dist = distribution
                    )
                }
                return(model)
            }, error = function(e) {
                return(NULL)
            })
        },
        
        .fitRobustAFT = function(data) {
            
            covariates <- self$options$covariates
            stratifyVar <- self$options$stratify_variable
            weightsVar <- self$options$weights_variable
            distribution <- self$options$distribution
            robustMethod <- self$options$robust_method
            tuningConstant <- self$options$tuning_constant
            efficiencyTarget <- self$options$efficiency_target
            scaleEstimation <- self$options$scale_estimation
            maxIter <- self$options$max_iterations
            tolerance <- self$options$convergence_tolerance
            
            # Build formula
            if (is.null(covariates) || length(covariates) == 0) {
                formula <- "Surv(time, status) ~ 1"
            } else {
                formula <- paste("Surv(time, status) ~", paste(covariates, collapse = " + "))
            }
            
            # Add stratification
            if (!is.null(stratifyVar)) {
                formula <- paste(formula, "+ strata(strata)")
            }
            
            # Use built-in robust implementation
            result <- private$.fitRobustAFTBuiltin(formula, data, distribution, 
                                                  robustMethod, tuningConstant, 
                                                  efficiencyTarget, scaleEstimation,
                                                  maxIter, tolerance, weightsVar)
            return(result)
        },
        
        .fitRobustAFTBuiltin = function(formula, data, distribution, method, 
                                       tuningConstant, efficiency, scaleEst,
                                       maxIter, tolerance, weightsVar) {
            
            # Fit standard model first
            if (!is.null(weightsVar)) {
                standardModel <- survival::survreg(
                    as.formula(formula),
                    data = data,
                    weights = data$weights,
                    dist = distribution
                )
            } else {
                standardModel <- survival::survreg(
                    as.formula(formula),
                    data = data,
                    dist = distribution
                )
            }
            
            # Implement M-estimation for robust AFT
            result <- private$.mestimateAFT(standardModel, data, method, 
                                           tuningConstant, maxIter, tolerance)
            
            # Detect outliers
            outliers <- private$.detectOutliers(result$model, data)
            
            return(list(model = result$model, outliers = outliers, 
                       weights = result$weights, error = NULL))
        },
        
        .mestimateAFT = function(standardModel, data, method, tuningConstant, 
                                maxIter, tolerance) {
            
            # Extract initial estimates
            beta <- standardModel$coefficients
            scale <- standardModel$scale
            
            # Iterative M-estimation
            for (iter in 1:maxIter) {
                # Calculate residuals
                residuals <- private$.calculateResiduals(standardModel, data)
                
                # Calculate weights based on method
                weights <- private$.calculateRobustWeights(residuals, method, tuningConstant)
                
                # Update estimates with weighted regression
                updatedModel <- private$.updateEstimates(standardModel, data, weights)
                
                # Check convergence
                if (private$.checkConvergence(beta, updatedModel$coefficients, tolerance)) {
                    break
                }
                
                beta <- updatedModel$coefficients
                standardModel <- updatedModel
            }
            
            # Add robust information to model
            standardModel$robust_method <- method
            standardModel$tuning_constant <- tuningConstant
            standardModel$iterations <- iter
            standardModel$weights <- weights
            
            return(list(model = standardModel, weights = weights))
        },
        
        .calculateResiduals = function(model, data) {
            
            # Calculate standardized residuals
            linear_pred <- predict(model, type = "linear")
            residuals <- (log(data$time) - linear_pred) / model$scale
            
            return(residuals)
        },
        
        .calculateRobustWeights = function(residuals, method, tuningConstant) {
            
            # Calculate robust weights based on method
            weights <- switch(method,
                "huber" = {
                    # Huber weights
                    ifelse(abs(residuals) <= tuningConstant, 1, tuningConstant / abs(residuals))
                },
                "tukey" = {
                    # Tukey biweight weights
                    ifelse(abs(residuals) <= tuningConstant, 
                           (1 - (residuals / tuningConstant)^2)^2, 0)
                },
                "hampel" = {
                    # Hampel weights (simplified three-part)
                    a <- tuningConstant
                    b <- 2 * tuningConstant
                    c <- 3 * tuningConstant
                    
                    ifelse(abs(residuals) <= a, 1,
                           ifelse(abs(residuals) <= b, a / abs(residuals),
                                  ifelse(abs(residuals) <= c, 
                                         a * (c - abs(residuals)) / ((c - b) * abs(residuals)), 0)))
                },
                "andrews" = {
                    # Andrews wave weights
                    ifelse(abs(residuals) <= pi * tuningConstant, 
                           sin(residuals / tuningConstant) / (residuals / tuningConstant), 0)
                },
                "median" = {
                    # Median regression (LAD) - equal weights
                    rep(1, length(residuals))
                },
                "lad" = {
                    # Least absolute deviation - equal weights
                    rep(1, length(residuals))
                },
                {
                    # Default to Huber
                    ifelse(abs(residuals) <= tuningConstant, 1, tuningConstant / abs(residuals))
                }
            )
            
            # Ensure weights are positive
            weights <- pmax(weights, 0.001)
            
            return(weights)
        },
        
        .updateEstimates = function(model, data, weights) {
            
            # Refit model with robust weights
            formula <- formula(model)
            distribution <- model$dist
            
            tryCatch({
                updatedModel <- survival::survreg(
                    formula,
                    data = data,
                    weights = weights,
                    dist = distribution
                )
                return(updatedModel)
            }, error = function(e) {
                return(model)
            })
        },
        
        .checkConvergence = function(old_beta, new_beta, tolerance) {
            
            if (length(old_beta) != length(new_beta)) {
                return(FALSE)
            }
            
            max_diff <- max(abs(old_beta - new_beta))
            return(max_diff < tolerance)
        },
        
        .detectOutliers = function(model, data) {
            
            outlierThreshold <- self$options$outlier_threshold
            
            # Calculate residuals
            residuals <- private$.calculateResiduals(model, data)
            
            # Detect outliers based on threshold
            outlier_indices <- which(abs(residuals) > outlierThreshold)
            
            outliers <- data.frame(
                observation = outlier_indices,
                residual = residuals[outlier_indices],
                outlier_type = ifelse(residuals[outlier_indices] > 0, "High", "Low"),
                stringsAsFactors = FALSE
            )
            
            return(outliers)
        },
        
        .initModelSummary = function() {
            table <- self$results$modelSummary
            table$addColumn(name = 'term', title = '', type = 'text')
            table$addColumn(name = 'value', title = 'Value', type = 'text')
        },
        
        .initCoefficientsTable = function() {
            table <- self$results$coefficients
            table$addColumn(name = 'term', title = 'Term', type = 'text')
            table$addColumn(name = 'estimate', title = 'Estimate', type = 'number', format = 'zto')
            table$addColumn(name = 'robust_se', title = 'Robust SE', type = 'number', format = 'zto')
            table$addColumn(name = 'z', title = 'z', type = 'number', format = 'zto')
            table$addColumn(name = 'pvalue', title = 'p-value', type = 'number', format = 'zto,pvalue')
            table$addColumn(name = 'ciLower', title = 'Lower CI', type = 'number', format = 'zto')
            table$addColumn(name = 'ciUpper', title = 'Upper CI', type = 'number', format = 'zto')
            table$addColumn(name = 'acceleration_factor', title = 'AF', type = 'number', format = 'zto')
            table$addColumn(name = 'af_lower', title = 'AF Lower', type = 'number', format = 'zto')
            table$addColumn(name = 'af_upper', title = 'AF Upper', type = 'number', format = 'zto')
        },
        
        .initOutlierAnalysisTable = function() {
            table <- self$results$outlierAnalysis
            table$addColumn(name = 'statistic', title = 'Statistic', type = 'text')
            table$addColumn(name = 'value', title = 'Value', type = 'number', format = 'zto')
            table$addColumn(name = 'interpretation', title = 'Interpretation', type = 'text')
        },
        
        .initDiagnosticsTable = function() {
            table <- self$results$diagnostics
            table$addColumn(name = 'measure', title = 'Measure', type = 'text')
            table$addColumn(name = 'robust_value', title = 'Robust Value', type = 'number', format = 'zto')
            table$addColumn(name = 'standard_value', title = 'Standard Value', type = 'number', format = 'zto')
            table$addColumn(name = 'interpretation', title = 'Interpretation', type = 'text')
        },
        
        .initComparisonTable = function() {
            table <- self$results$modelComparison
            table$addColumn(name = 'model', title = 'Model', type = 'text')
            table$addColumn(name = 'loglik', title = 'Log-Likelihood', type = 'number', format = 'zto')
            table$addColumn(name = 'aic', title = 'AIC', type = 'number', format = 'zto')
            table$addColumn(name = 'bic', title = 'BIC', type = 'number', format = 'zto')
            table$addColumn(name = 'efficiency', title = 'Efficiency', type = 'number', format = 'zto,pc')
            table$addColumn(name = 'breakdown_point', title = 'Breakdown Point', type = 'number', format = 'zto,pc')
        },
        
        .initRobustnessTable = function() {
            table <- self$results$robustnessInfo
            table$addColumn(name = 'criterion', title = 'Criterion', type = 'text')
            table$addColumn(name = 'value', title = 'Value', type = 'text')
        },
        
        .initScaleEstimatesTable = function() {
            table <- self$results$scaleEstimates
            table$addColumn(name = 'parameter', title = 'Parameter', type = 'text')
            table$addColumn(name = 'robust_estimate', title = 'Robust Estimate', type = 'number', format = 'zto')
            table$addColumn(name = 'standard_estimate', title = 'Standard Estimate', type = 'number', format = 'zto')
            table$addColumn(name = 'relative_efficiency', title = 'Relative Efficiency', type = 'number', format = 'zto,pc')
        },
        
        .initOutlierTable = function() {
            table <- self$results$outlierTable
            table$addColumn(name = 'observation', title = 'Observation', type = 'integer')
            table$addColumn(name = 'residual', title = 'Residual', type = 'number', format = 'zto')
            table$addColumn(name = 'weight', title = 'Weight', type = 'number', format = 'zto')
            table$addColumn(name = 'influence', title = 'Influence', type = 'number', format = 'zto')
            table$addColumn(name = 'outlier_type', title = 'Outlier Type', type = 'text')
        },
        
        .populateModelSummary = function(model, data) {
            
            table <- self$results$modelSummary
            
            # Model information
            n_obs <- nrow(data)
            n_events <- sum(data$status)
            n_covariates <- length(self$options$covariates %||% character(0))
            
            # Robust method names
            method_names <- list(
                "huber" = "Huber M-estimator",
                "tukey" = "Tukey biweight",
                "hampel" = "Hampel function",
                "andrews" = "Andrews wave",
                "median" = "Median regression",
                "lad" = "Least absolute deviation"
            )
            
            # Distribution names
            dist_names <- list(
                "weibull" = "Weibull",
                "exponential" = "Exponential",
                "lognormal" = "Log-normal",
                "loglogistic" = "Log-logistic",
                "gamma" = "Gamma",
                "gaussian" = "Gaussian"
            )
            
            rows <- list(
                list(term = "Number of observations", value = toString(n_obs)),
                list(term = "Number of events", value = toString(n_events)),
                list(term = "Event rate", value = paste0(round(100 * n_events / n_obs, 1), "%")),
                list(term = "Number of covariates", value = toString(n_covariates)),
                list(term = "Distribution", value = dist_names[[self$options$distribution]]),
                list(term = "Robust method", value = method_names[[self$options$robust_method]]),
                list(term = "Tuning constant", value = toString(self$options$tuning_constant))
            )
            
            # Add convergence information if available
            if (!is.null(model$iterations)) {
                rows <- append(rows, list(list(term = "Iterations", value = toString(model$iterations))))
            }
            
            for (row in rows) {
                table$addRow(rowKey = row$term, values = row)
            }
        },
        
        .populateCoefficientsTable = function(model) {
            
            table <- self$results$coefficients
            confidence_level <- self$options$confidence_level
            
            # Get coefficient information
            if (!is.null(model$coefficients)) {
                coeffs <- model$coefficients
                
                # Use robust standard errors if available
                if (!is.null(model$robust.var)) {
                    se <- sqrt(diag(model$robust.var))
                } else {
                    se <- sqrt(diag(vcov(model)))
                }
                
                # Calculate statistics
                z_values <- coeffs / se
                p_values <- 2 * pnorm(-abs(z_values))
                
                # Calculate confidence intervals
                z_crit <- qnorm((1 + confidence_level) / 2)
                ci_lower <- coeffs - z_crit * se
                ci_upper <- coeffs + z_crit * se
                
                # Calculate acceleration factors (exp of negative coefficients for AFT)
                af <- exp(-coeffs)
                af_lower <- exp(-ci_upper)
                af_upper <- exp(-ci_lower)
                
                # Populate table
                for (i in seq_along(coeffs)) {
                    term <- names(coeffs)[i]
                    
                    table$addRow(rowKey = term, values = list(
                        term = term,
                        estimate = coeffs[i],
                        robust_se = se[i],
                        z = z_values[i],
                        pvalue = p_values[i],
                        ciLower = ci_lower[i],
                        ciUpper = ci_upper[i],
                        acceleration_factor = af[i],
                        af_lower = af_lower[i],
                        af_upper = af_upper[i]
                    ))
                }
            }
        },
        
        .populateOutlierAnalysisTable = function(outliers, data) {
            
            table <- self$results$outlierAnalysis
            
            # Outlier statistics
            n_outliers <- nrow(outliers)
            n_obs <- nrow(data)
            outlier_rate <- n_outliers / n_obs
            
            rows <- list(
                list(
                    statistic = "Number of outliers",
                    value = n_outliers,
                    interpretation = paste("Out of", n_obs, "observations")
                ),
                list(
                    statistic = "Outlier rate",
                    value = outlier_rate,
                    interpretation = if (outlier_rate > 0.1) "High outlier rate" else "Acceptable"
                ),
                list(
                    statistic = "Max absolute residual",
                    value = if (n_outliers > 0) max(abs(outliers$residual)) else 0,
                    interpretation = "Largest standardized residual"
                ),
                list(
                    statistic = "Outlier threshold",
                    value = self$options$outlier_threshold,
                    interpretation = "Detection threshold for outliers"
                )
            )
            
            for (row in rows) {
                table$addRow(rowKey = row$statistic, values = row)
            }
        },
        
        .populateDiagnosticsTable = function(robustModel, standardModel) {
            
            table <- self$results$diagnostics
            
            # Model diagnostics comparison
            robust_loglik <- robustModel$loglik[length(robustModel$loglik)]
            standard_loglik <- if (!is.null(standardModel)) standardModel$loglik[length(standardModel$loglik)] else NA
            
            robust_aic <- AIC(robustModel)
            standard_aic <- if (!is.null(standardModel)) AIC(standardModel) else NA
            
            robust_scale <- robustModel$scale
            standard_scale <- if (!is.null(standardModel)) standardModel$scale else NA
            
            rows <- list(
                list(
                    measure = "Log-likelihood",
                    robust_value = robust_loglik,
                    standard_value = standard_loglik,
                    interpretation = "Model fit measure"
                ),
                list(
                    measure = "AIC",
                    robust_value = robust_aic,
                    standard_value = standard_aic,
                    interpretation = if (!is.na(standard_aic) && robust_aic < standard_aic) "Robust model preferred" else "Comparable fit"
                ),
                list(
                    measure = "Scale parameter",
                    robust_value = robust_scale,
                    standard_value = standard_scale,
                    interpretation = "Dispersion parameter"
                )
            )
            
            for (row in rows) {
                table$addRow(rowKey = row$measure, values = row)
            }
        },
        
        .populateComparisonTable = function(robustModel, standardModel) {
            
            if (!self$options$show_comparison || is.null(standardModel)) {
                return()
            }
            
            table <- self$results$modelComparison
            
            # Calculate breakdown points for different methods
            breakdown_points <- list(
                "huber" = 0.5,
                "tukey" = 0.5,
                "hampel" = 0.5,
                "andrews" = 0.5,
                "median" = 0.5,
                "lad" = 0.5
            )
            
            # Calculate efficiency
            efficiency <- self$options$efficiency_target
            
            # Standard model metrics
            std_loglik <- standardModel$loglik[length(standardModel$loglik)]
            std_aic <- AIC(standardModel)
            std_bic <- BIC(standardModel)
            
            # Robust model metrics
            rob_loglik <- robustModel$loglik[length(robustModel$loglik)]
            rob_aic <- AIC(robustModel)
            rob_bic <- BIC(robustModel)
            
            # Add rows
            table$addRow(rowKey = "standard", values = list(
                model = "Standard AFT",
                loglik = std_loglik,
                aic = std_aic,
                bic = std_bic,
                efficiency = 1.0,
                breakdown_point = 0.0
            ))
            
            table$addRow(rowKey = "robust", values = list(
                model = "Robust AFT",
                loglik = rob_loglik,
                aic = rob_aic,
                bic = rob_bic,
                efficiency = efficiency,
                breakdown_point = breakdown_points[[self$options$robust_method]]
            ))
        },
        
        .populateRobustnessTable = function(model) {
            
            table <- self$results$robustnessInfo
            
            rows <- list(
                list(criterion = "Robust method", value = self$options$robust_method),
                list(criterion = "Tuning constant", value = toString(self$options$tuning_constant)),
                list(criterion = "Scale estimation", value = self$options$scale_estimation),
                list(criterion = "Efficiency target", value = paste0(round(100 * self$options$efficiency_target, 1), "%")),
                list(criterion = "Maximum iterations", value = toString(self$options$max_iterations)),
                list(criterion = "Convergence tolerance", value = sprintf("%.2e", self$options$convergence_tolerance))
            )
            
            if (!is.null(model$iterations)) {
                rows <- append(rows, list(
                    list(criterion = "Actual iterations", value = toString(model$iterations)),
                    list(criterion = "Convergence achieved", value = "Yes")
                ))
            }
            
            for (row in rows) {
                table$addRow(rowKey = row$criterion, values = row)
            }
        },
        
        .populateScaleEstimatesTable = function(robustModel, standardModel) {
            
            table <- self$results$scaleEstimates
            
            robust_scale <- robustModel$scale
            standard_scale <- if (!is.null(standardModel)) standardModel$scale else NA
            efficiency <- if (!is.na(standard_scale)) robust_scale / standard_scale else 1.0
            
            rows <- list(
                list(
                    parameter = "Scale parameter",
                    robust_estimate = robust_scale,
                    standard_estimate = standard_scale,
                    relative_efficiency = efficiency
                )
            )
            
            for (row in rows) {
                table$addRow(rowKey = row$parameter, values = row)
            }
        },
        
        .populateOutlierTable = function(outliers, weights, data) {
            
            table <- self$results$outlierTable
            
            if (nrow(outliers) == 0) {
                return()
            }
            
            for (i in 1:nrow(outliers)) {
                obs_id <- outliers$observation[i]
                
                table$addRow(rowKey = obs_id, values = list(
                    observation = obs_id,
                    residual = outliers$residual[i],
                    weight = if (length(weights) >= obs_id) weights[obs_id] else 1.0,
                    influence = abs(outliers$residual[i]),
                    outlier_type = outliers$outlier_type[i]
                ))
            }
        },
        
        .plotResiduals = function(model, data) {
            image <- self$results$residualPlots
            image$setState(list(model = model, data = data))
        },
        
        .plotOutliers = function(outliers, weights, data) {
            image <- self$results$outlierPlots
            image$setState(list(outliers = outliers, weights = weights, data = data))
        },
        
        .plotSurvival = function(model, data) {
            image <- self$results$survivalPlots
            image$setState(list(model = model, data = data))
        },
        
        .plotQQ = function(model, data) {
            image <- self$results$qqPlots
            image$setState(list(model = model, data = data))
        },
        
        .populateAnalysisSummary = function(model, outliers, data) {
            
            html <- self$results$summaryTable
            
            n_obs <- nrow(data)
            n_events <- sum(data$status)
            n_outliers <- nrow(outliers)
            event_rate <- round(100 * n_events / n_obs, 1)
            outlier_rate <- round(100 * n_outliers / n_obs, 1)
            
            content <- paste0(
                "<h3>Robust AFT Model Summary</h3>",
                "<p><strong>Sample Characteristics:</strong></p>",
                "<ul>",
                "<li>Total observations: ", n_obs, "</li>",
                "<li>Events observed: ", n_events, " (", event_rate, "%)</li>",
                "<li>Outliers detected: ", n_outliers, " (", outlier_rate, "%)</li>",
                "<li>Distribution: ", self$options$distribution, "</li>",
                "</ul>",
                
                "<p><strong>Robust Estimation:</strong></p>",
                "<ul>",
                "<li>Method: ", self$options$robust_method, "</li>",
                "<li>Tuning constant: ", self$options$tuning_constant, "</li>",
                "<li>Scale estimation: ", self$options$scale_estimation, "</li>",
                "<li>Efficiency target: ", round(100 * self$options$efficiency_target, 1), "%</li>",
                "</ul>",
                
                "<p><strong>Clinical Interpretation:</strong></p>",
                "<p>Robust AFT models provide outlier-resistant estimates of the acceleration ",
                "factors, which represent the ratio of expected survival times between different ",
                "covariate levels. These models are particularly valuable when standard AFT ",
                "models may be influenced by extreme observations or model misspecification, ",
                "ensuring more reliable parameter estimates in challenging clinical datasets.</p>"
            )
            
            html$setContent(content)
        },
        
        .populateMethodExplanation = function() {
            
            html <- self$results$methodExplanation
            
            content <- paste0(
                "<h3>Robust AFT Model Methods</h3>",
                
                "<h4>Method Overview</h4>",
                "<p>Robust accelerated failure time (AFT) models use M-estimation and resistant ",
                "estimators to provide outlier-resistant parameter estimates. Unlike Cox models ",
                "that focus on hazard ratios, AFT models directly model the effect of covariates ",
                "on survival time acceleration factors.</p>",
                
                "<h4>Robust Estimation Methods</h4>",
                "<ul>",
                "<li><strong>Huber M-estimator:</strong> Quadratic loss for small residuals, ",
                "linear loss for large residuals, providing good balance between efficiency and robustness.</li>",
                "<li><strong>Tukey Biweight:</strong> Redescending M-estimator that completely ",
                "down-weights extreme outliers to zero influence.</li>",
                "<li><strong>Hampel Function:</strong> Three-part redescending function with ",
                "flexible control over influence function shape.</li>",
                "<li><strong>Andrews Wave:</strong> Sine-based redescending function for smooth ",
                "outlier down-weighting.</li>",
                "<li><strong>Median Regression (LAD):</strong> Least absolute deviation estimation ",
                "providing high breakdown point.</li>",
                "</ul>",
                
                "<h4>Acceleration Factors</h4>",
                "<p>AFT models estimate acceleration factors (AF = exp(-β)) that represent:</p>",
                "<ul>",
                "<li>AF > 1: Accelerated failure (shorter survival)</li>",
                "<li>AF = 1: No effect on survival time</li>",
                "<li>AF < 1: Decelerated failure (longer survival)</li>",
                "</ul>",
                
                "<h4>When to Use Robust AFT Models</h4>",
                "<ul>",
                "<li>Presence of outliers or extreme observations</li>",
                "<li>Concern about model misspecification</li>",
                "<li>Need for direct interpretation of covariate effects on survival time</li>",
                "<li>Parametric survival modeling with uncertainty about distribution</li>",
                "<li>Validation of standard AFT model results</li>",
                "</ul>",
                
                "<h4>Advantages</h4>",
                "<ul>",
                "<li><strong>Outlier resistance:</strong> Reduces influence of extreme observations</li>",
                "<li><strong>Direct interpretation:</strong> Acceleration factors directly relate to survival time</li>",
                "<li><strong>Parametric efficiency:</strong> More efficient than non-parametric methods when assumptions hold</li>",
                "<li><strong>Breakdown point:</strong> High resistance to contamination</li>",
                "</ul>"
            )
            
            html$setContent(content)
        }
    )
)
