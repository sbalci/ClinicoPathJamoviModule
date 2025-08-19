#' @title Robust Cox Regression
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import survival
#' @import ggplot2
#' @import dplyr
#' @export

coxrobustClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "coxrobustClass",
    inherit = coxrobustBase,
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
            private$.initOutlierTable()
            private$.initInfluenceTable()
            private$.initComparisonTable()
            private$.initConvergenceTable()
            
            # Set up plot areas
            if (self$options$show_residual_plots) {
                self$results$residualPlots$setSize(800, 600)
            }
            if (self$options$show_influence_plots) {
                self$results$influencePlots$setSize(800, 600)
            }
            if (self$options$show_weight_plots) {
                self$results$weightPlots$setSize(800, 600)
            }
            if (self$options$show_survival_plots) {
                self$results$survivalPlots$setSize(800, 600)
            }
            if (self$options$show_outliers) {
                self$results$outlierPlots$setSize(800, 600)
            }
            if (self$options$show_comparison) {
                self$results$comparisonPlots$setSize(800, 600)
            }
        },
        
        .run = function() {
            
            # Get data
            data <- self$data
            
            if (is.null(self$options$elapsedtime) || is.null(self$options$outcome)) {
                return()
            }
            
            # Check if required packages are available
            if (!requireNamespace("coxrobust", quietly = TRUE)) {
                # If coxrobust package is not available, use alternative implementation
                self$results$modelSummary$setNote("info", 
                    "Using built-in robust Cox implementation. Install 'coxrobust' package for additional features.")
            }
            
            # Prepare data
            result <- private$.prepareData(data)
            if (!is.null(result$error)) {
                self$results$modelSummary$setNote("error", result$error)
                return()
            }
            
            preparedData <- result$data
            
            # Fit standard Cox model for comparison
            standardModel <- private$.fitStandardCox(preparedData)
            
            # Fit robust Cox model
            result <- private$.fitRobustCox(preparedData)
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
            private$.populateOutlierTable(outliers, preparedData)
            private$.populateInfluenceTable(robustModel, preparedData)
            private$.populateComparisonTable(robustModel, standardModel)
            private$.populateConvergenceTable(robustModel)
            
            # Generate plots
            if (self$options$show_residual_plots) {
                private$.plotResiduals(robustModel, preparedData)
            }
            if (self$options$show_influence_plots) {
                private$.plotInfluence(robustModel, preparedData)
            }
            if (self$options$show_weight_plots) {
                private$.plotWeights(weights, preparedData)
            }
            if (self$options$show_survival_plots) {
                private$.plotSurvival(robustModel, preparedData)
            }
            if (self$options$show_outliers) {
                private$.plotOutliers(outliers, preparedData)
            }
            if (self$options$show_comparison) {
                private$.plotComparison(robustModel, standardModel, preparedData)
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
            
            # Add weights
            if (!is.null(weightsVar)) {
                modelData$weights <- data[[weightsVar]]
                if (any(modelData$weights <= 0, na.rm = TRUE)) {
                    return(list(error = "Weights variable contains non-positive values."))
                }
            } else {
                modelData$weights <- rep(1, nrow(modelData))
            }
            
            # Remove rows with missing data
            complete_cases <- complete.cases(modelData)
            modelData <- modelData[complete_cases, ]
            
            if (nrow(modelData) == 0) {
                return(list(error = "No complete cases available for analysis."))
            }
            
            # Add row identifiers for outlier tracking
            modelData$obs_id <- 1:nrow(modelData)
            
            return(list(data = modelData, error = NULL))
        },
        
        .fitStandardCox = function(data) {
            
            covariates <- self$options$covariates
            stratifyVar <- self$options$stratify_variable
            
            # Build formula
            if (is.null(covariates) || length(covariates) == 0) {
                formula <- as.formula("Surv(time, status) ~ 1")
            } else {
                covar_formula <- paste(covariates, collapse = " + ")
                if (!is.null(stratifyVar)) {
                    formula <- as.formula(paste("Surv(time, status) ~", covar_formula, "+ strata(strata)"))
                } else {
                    formula <- as.formula(paste("Surv(time, status) ~", covar_formula))
                }
            }
            
            # Fit standard Cox model
            tryCatch({
                model <- survival::coxph(
                    formula = formula,
                    data = data,
                    weights = data$weights,
                    model = TRUE,
                    x = TRUE,
                    y = TRUE
                )
                return(model)
            }, error = function(e) {
                return(NULL)
            })
        },
        
        .fitRobustCox = function(data) {
            
            covariates <- self$options$covariates
            stratifyVar <- self$options$stratify_variable
            robustMethod <- self$options$robust_method
            tuningConstant <- self$options$tuning_constant
            efficiencyTarget <- self$options$efficiency_target
            maxIter <- self$options$max_iterations
            tolerance <- self$options$convergence_tolerance
            outlierDetection <- self$options$outlier_detection
            outlierThreshold <- self$options$outlier_threshold
            
            # Build formula
            if (is.null(covariates) || length(covariates) == 0) {
                formula <- as.formula("Surv(time, status) ~ 1")
            } else {
                covar_formula <- paste(covariates, collapse = " + ")
                if (!is.null(stratifyVar)) {
                    formula <- as.formula(paste("Surv(time, status) ~", covar_formula, "+ strata(strata)"))
                } else {
                    formula <- as.formula(paste("Surv(time, status) ~", covar_formula))
                }
            }
            
            # Check if coxrobust package is available
            if (requireNamespace("coxrobust", quietly = TRUE)) {
                # Use coxrobust package implementation
                tryCatch({
                    model <- coxrobust::coxr(
                        formula = formula,
                        data = data,
                        subset = NULL,
                        na.action = na.omit,
                        trunc = tuningConstant,
                        f.weight = robustMethod,
                        singular.ok = TRUE,
                        model = TRUE
                    )
                    
                    # Detect outliers if requested
                    outliers <- NULL
                    weights <- NULL
                    if (outlierDetection) {
                        result <- private$.detectOutliers(model, data, outlierThreshold)
                        outliers <- result$outliers
                        weights <- result$weights
                    }
                    
                    return(list(model = model, outliers = outliers, weights = weights, error = NULL))
                    
                }, error = function(e) {
                    error_msg <- paste("Robust Cox model fitting failed:", e$message)
                    return(list(error = error_msg))
                })
                
            } else {
                # Use built-in robust implementation
                result <- private$.fitRobustCoxBuiltin(formula, data, robustMethod, 
                                                       tuningConstant, maxIter, tolerance,
                                                       outlierDetection, outlierThreshold)
                return(result)
            }
        },
        
        .fitRobustCoxBuiltin = function(formula, data, method, tuning, maxIter, tolerance,
                                       outlierDetection, outlierThreshold) {
            
            # Fit standard Cox model first
            standardModel <- survival::coxph(
                formula = formula,
                data = data,
                weights = data$weights,
                model = TRUE,
                x = TRUE,
                y = TRUE
            )
            
            # Calculate residuals
            residuals <- residuals(standardModel, type = "deviance")
            
            # Calculate robust weights based on method
            weights <- private$.calculateRobustWeights(residuals, method, tuning)
            
            # Refit model with robust weights
            data$robust_weights <- data$weights * weights
            
            robustModel <- survival::coxph(
                formula = formula,
                data = data,
                weights = data$robust_weights,
                model = TRUE,
                x = TRUE,
                y = TRUE
            )
            
            # Add robust variance estimation
            robustModel$robust.se <- sqrt(diag(vcov(robustModel)))
            
            # Detect outliers if requested
            outliers <- NULL
            if (outlierDetection) {
                outliers <- data.frame(
                    obs_id = data$obs_id,
                    residual = residuals,
                    weight = weights,
                    outlier = abs(residuals) > outlierThreshold
                )
            }
            
            return(list(model = robustModel, outliers = outliers, weights = weights, error = NULL))
        },
        
        .calculateRobustWeights = function(residuals, method, tuning) {
            
            # Calculate MAD (Median Absolute Deviation) for scale
            mad_scale <- median(abs(residuals - median(residuals, na.rm = TRUE)), na.rm = TRUE) * 1.4826
            
            # Standardize residuals
            std_residuals <- residuals / mad_scale
            
            # Calculate weights based on method
            weights <- switch(method,
                "huber" = {
                    # Huber weights
                    ifelse(abs(std_residuals) <= tuning, 1, tuning / abs(std_residuals))
                },
                "tukey" = {
                    # Tukey's biweight
                    ifelse(abs(std_residuals) <= tuning,
                          (1 - (std_residuals / tuning)^2)^2, 0)
                },
                "hampel" = {
                    # Hampel's three-part redescending function
                    a <- tuning
                    b <- 2 * tuning
                    c <- 3 * tuning
                    abs_res <- abs(std_residuals)
                    
                    ifelse(abs_res <= a, 1,
                          ifelse(abs_res <= b, a / abs_res,
                                ifelse(abs_res <= c, 
                                      a * (c - abs_res) / (abs_res * (c - b)), 0)))
                },
                "bounded" = {
                    # Bounded influence function
                    pmin(1, tuning / abs(std_residuals))
                },
                "weighted" = {
                    # Simple weighted likelihood
                    exp(-abs(std_residuals) / tuning)
                },
                {
                    # Default to Huber
                    ifelse(abs(std_residuals) <= tuning, 1, tuning / abs(std_residuals))
                }
            )
            
            # Ensure weights are between 0 and 1
            weights <- pmax(0, pmin(1, weights))
            
            return(weights)
        },
        
        .detectOutliers = function(model, data, threshold) {
            
            # Calculate various residuals
            deviance_res <- residuals(model, type = "deviance")
            martingale_res <- residuals(model, type = "martingale")
            schoenfeld_res <- residuals(model, type = "schoenfeld")
            
            # Calculate influence measures
            dfbetas <- residuals(model, type = "dfbetas")
            
            # Identify outliers based on threshold
            outliers <- data.frame(
                obs_id = data$obs_id,
                time = data$time,
                status = data$status,
                deviance_residual = deviance_res,
                martingale_residual = martingale_res,
                outlier_flag = abs(deviance_res) > threshold
            )
            
            # Calculate robust weights (for display)
            mad_scale <- median(abs(deviance_res - median(deviance_res, na.rm = TRUE)), na.rm = TRUE) * 1.4826
            std_residuals <- deviance_res / mad_scale
            weights <- ifelse(abs(std_residuals) <= self$options$tuning_constant, 
                            1, self$options$tuning_constant / abs(std_residuals))
            
            return(list(outliers = outliers, weights = weights))
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
            table$addColumn(name = 'hazard_ratio', title = 'HR', type = 'number', format = 'zto')
            table$addColumn(name = 'hr_lower', title = 'HR Lower', type = 'number', format = 'zto')
            table$addColumn(name = 'hr_upper', title = 'HR Upper', type = 'number', format = 'zto')
        },
        
        .initOutlierTable = function() {
            table <- self$results$outlierAnalysis
            table$addColumn(name = 'observation', title = 'Observation', type = 'text')
            table$addColumn(name = 'time', title = 'Time', type = 'number', format = 'zto')
            table$addColumn(name = 'status', title = 'Status', type = 'text')
            table$addColumn(name = 'residual', title = 'Residual', type = 'number', format = 'zto')
            table$addColumn(name = 'weight', title = 'Robust Weight', type = 'number', format = 'zto')
            table$addColumn(name = 'influence', title = 'Influence', type = 'number', format = 'zto')
            table$addColumn(name = 'outlier_flag', title = 'Outlier', type = 'text')
        },
        
        .initInfluenceTable = function() {
            table <- self$results$influenceMeasures
            table$addColumn(name = 'measure', title = 'Measure', type = 'text')
            table$addColumn(name = 'value', title = 'Value', type = 'number', format = 'zto')
            table$addColumn(name = 'threshold', title = 'Threshold', type = 'number', format = 'zto')
            table$addColumn(name = 'interpretation', title = 'Interpretation', type = 'text')
        },
        
        .initComparisonTable = function() {
            table <- self$results$modelComparison
            table$addColumn(name = 'model', title = 'Model', type = 'text')
            table$addColumn(name = 'loglik', title = 'Log-Likelihood', type = 'number', format = 'zto')
            table$addColumn(name = 'aic', title = 'AIC', type = 'number', format = 'zto')
            table$addColumn(name = 'bic', title = 'BIC', type = 'number', format = 'zto')
            table$addColumn(name = 'concordance', title = 'C-Index', type = 'number', format = 'zto')
            table$addColumn(name = 'robust_measure', title = 'Robustness', type = 'number', format = 'zto')
        },
        
        .initConvergenceTable = function() {
            table <- self$results$convergenceInfo
            table$addColumn(name = 'criterion', title = 'Criterion', type = 'text')
            table$addColumn(name = 'value', title = 'Value', type = 'text')
        },
        
        .populateModelSummary = function(model, data) {
            
            table <- self$results$modelSummary
            
            # Model information
            n_obs <- nrow(data)
            n_events <- sum(data$status)
            n_covariates <- length(self$options$covariates %||% character(0))
            
            # Robust method name
            method_names <- list(
                "huber" = "Huber M-estimation",
                "tukey" = "Tukey's biweight",
                "hampel" = "Hampel's function",
                "bounded" = "Bounded influence",
                "weighted" = "Weighted likelihood"
            )
            
            rows <- list(
                list(term = "Number of observations", value = toString(n_obs)),
                list(term = "Number of events", value = toString(n_events)),
                list(term = "Number of covariates", value = toString(n_covariates)),
                list(term = "Robust method", value = method_names[[self$options$robust_method]]),
                list(term = "Tuning constant", value = toString(self$options$tuning_constant)),
                list(term = "Efficiency target", value = paste0(self$options$efficiency_target * 100, "%"))
            )
            
            # Add convergence information if available
            if (!is.null(model$iter)) {
                rows <- append(rows, list(list(term = "Iterations", value = toString(model$iter))))
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
                if (!is.null(model$robust.se)) {
                    se <- model$robust.se
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
                
                # Calculate hazard ratios
                hr <- exp(coeffs)
                hr_lower <- exp(ci_lower)
                hr_upper <- exp(ci_upper)
                
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
                        hazard_ratio = hr[i],
                        hr_lower = hr_lower[i],
                        hr_upper = hr_upper[i]
                    ))
                }
            }
        },
        
        .populateOutlierTable = function(outliers, data) {
            
            if (!self$options$show_outliers || is.null(outliers)) {
                return()
            }
            
            table <- self$results$outlierAnalysis
            
            # Sort by absolute residual value
            if ("deviance_residual" %in% names(outliers)) {
                outliers <- outliers[order(abs(outliers$deviance_residual), decreasing = TRUE), ]
                
                # Show top outliers (max 20)
                n_show <- min(20, sum(outliers$outlier_flag, na.rm = TRUE))
                
                if (n_show > 0) {
                    outliers_to_show <- outliers[outliers$outlier_flag, ][1:n_show, ]
                    
                    for (i in 1:nrow(outliers_to_show)) {
                        obs <- outliers_to_show[i, ]
                        
                        table$addRow(rowKey = obs$obs_id, values = list(
                            observation = paste("Obs", obs$obs_id),
                            time = obs$time,
                            status = if(obs$status == 1) "Event" else "Censored",
                            residual = obs$deviance_residual,
                            weight = NA,  # Will be populated if weights available
                            influence = obs$martingale_residual,
                            outlier_flag = "Yes"
                        ))
                    }
                }
            }
        },
        
        .populateInfluenceTable = function(model, data) {
            
            if (!self$options$show_influence) {
                return()
            }
            
            table <- self$results$influenceMeasures
            
            # Calculate influence measures
            n_obs <- nrow(data)
            n_params <- length(model$coefficients)
            
            # Common thresholds
            cook_threshold <- 4 / n_obs
            dfbetas_threshold <- 2 / sqrt(n_obs)
            
            rows <- list(
                list(
                    measure = "Cook's distance threshold",
                    value = cook_threshold,
                    threshold = cook_threshold,
                    interpretation = "Values above indicate high influence"
                ),
                list(
                    measure = "DFBETAS threshold",
                    value = dfbetas_threshold,
                    threshold = dfbetas_threshold,
                    interpretation = "Values above indicate parameter influence"
                ),
                list(
                    measure = "Leverage threshold",
                    value = 2 * n_params / n_obs,
                    threshold = 2 * n_params / n_obs,
                    interpretation = "Values above indicate high leverage"
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
            
            # Standard model metrics
            std_loglik <- standardModel$loglik[2]
            std_aic <- AIC(standardModel)
            std_bic <- BIC(standardModel)
            std_concordance <- if (!is.null(standardModel$concordance)) standardModel$concordance[1] else NA
            
            # Robust model metrics
            rob_loglik <- robustModel$loglik[2]
            rob_aic <- AIC(robustModel)
            rob_bic <- BIC(robustModel)
            rob_concordance <- if (!is.null(robustModel$concordance)) robustModel$concordance[1] else NA
            
            # Add rows
            table$addRow(rowKey = "standard", values = list(
                model = "Standard Cox",
                loglik = std_loglik,
                aic = std_aic,
                bic = std_bic,
                concordance = std_concordance,
                robust_measure = 0
            ))
            
            table$addRow(rowKey = "robust", values = list(
                model = "Robust Cox",
                loglik = rob_loglik,
                aic = rob_aic,
                bic = rob_bic,
                concordance = rob_concordance,
                robust_measure = 1
            ))
        },
        
        .populateConvergenceTable = function(model) {
            
            table <- self$results$convergenceInfo
            
            rows <- list(
                list(criterion = "Method", value = self$options$robust_method),
                list(criterion = "Maximum iterations", value = toString(self$options$max_iterations)),
                list(criterion = "Convergence tolerance", value = sprintf("%.2e", self$options$convergence_tolerance))
            )
            
            if (!is.null(model$iter)) {
                rows <- append(rows, list(
                    list(criterion = "Actual iterations", value = toString(model$iter)),
                    list(criterion = "Convergence achieved", value = "Yes")
                ))
            }
            
            for (row in rows) {
                table$addRow(rowKey = row$criterion, values = row)
            }
        },
        
        .plotResiduals = function(model, data) {
            image <- self$results$residualPlots
            image$setState(list(model = model, data = data))
        },
        
        .plotInfluence = function(model, data) {
            image <- self$results$influencePlots
            image$setState(list(model = model, data = data))
        },
        
        .plotWeights = function(weights, data) {
            image <- self$results$weightPlots
            image$setState(list(weights = weights, data = data))
        },
        
        .plotSurvival = function(model, data) {
            image <- self$results$survivalPlots
            image$setState(list(model = model, data = data))
        },
        
        .plotOutliers = function(outliers, data) {
            image <- self$results$outlierPlots
            image$setState(list(outliers = outliers, data = data))
        },
        
        .plotComparison = function(robustModel, standardModel, data) {
            image <- self$results$comparisonPlots
            image$setState(list(robust = robustModel, standard = standardModel, data = data))
        },
        
        .populateAnalysisSummary = function(model, outliers, data) {
            
            html <- self$results$summaryTable
            
            n_obs <- nrow(data)
            n_events <- sum(data$status)
            n_outliers <- if (!is.null(outliers)) sum(outliers$outlier_flag, na.rm = TRUE) else 0
            
            content <- paste0(
                "<h3>Robust Cox Regression Summary</h3>",
                "<p><strong>Sample Characteristics:</strong></p>",
                "<ul>",
                "<li>Total observations: ", n_obs, "</li>",
                "<li>Events observed: ", n_events, " (", round(100 * n_events / n_obs, 1), "%)</li>",
                "<li>Outliers detected: ", n_outliers, " (", round(100 * n_outliers / n_obs, 1), "%)</li>",
                "</ul>",
                
                "<p><strong>Robust Estimation:</strong></p>",
                "<ul>",
                "<li>Method: ", self$options$robust_method, "</li>",
                "<li>Tuning constant: ", self$options$tuning_constant, "</li>",
                "<li>Efficiency target: ", self$options$efficiency_target * 100, "%</li>",
                "</ul>",
                
                "<p><strong>Clinical Interpretation:</strong></p>",
                "<p>Robust Cox regression provides parameter estimates that are less sensitive to outliers ",
                "and model misspecification compared to standard Cox regression. The robust standard errors ",
                "provide more reliable inference when the proportional hazards assumption may be violated ",
                "or when influential observations are present. Hazard ratios should be interpreted with ",
                "consideration of the down-weighting applied to outlying observations.</p>"
            )
            
            html$setContent(content)
        },
        
        .populateMethodExplanation = function() {
            
            html <- self$results$methodExplanation
            
            content <- paste0(
                "<h3>Robust Cox Regression Methods</h3>",
                
                "<h4>Method Overview</h4>",
                "<p>Robust Cox regression methods modify the standard partial likelihood approach to reduce ",
                "the influence of outlying observations and provide more stable parameter estimates in the ",
                "presence of model misspecification.</p>",
                
                "<h4>Robust Estimation Methods</h4>",
                "<ul>",
                "<li><strong>Huber M-estimation:</strong> Uses a quadratic loss for small residuals and ",
                "linear loss for large residuals, providing a balance between efficiency and robustness.</li>",
                "<li><strong>Tukey's biweight:</strong> A redescending M-estimator that completely ",
                "downweights extreme outliers to zero.</li>",
                "<li><strong>Hampel's function:</strong> Three-part redescending function offering ",
                "flexible control over the influence function.</li>",
                "<li><strong>Bounded influence:</strong> Limits the maximum influence any single ",
                "observation can have on parameter estimates.</li>",
                "<li><strong>Weighted likelihood:</strong> Applies data-driven weights to the partial ",
                "likelihood based on residual magnitudes.</li>",
                "</ul>",
                
                "<h4>Key Advantages</h4>",
                "<ul>",
                "<li><strong>Outlier resistance:</strong> Automatically downweights influential observations</li>",
                "<li><strong>Stable estimates:</strong> Less sensitive to model misspecification</li>",
                "<li><strong>Robust inference:</strong> Sandwich variance estimators for valid standard errors</li>",
                "<li><strong>Efficiency control:</strong> Tunable trade-off between robustness and efficiency</li>",
                "</ul>",
                
                "<h4>Tuning Constants</h4>",
                "<p>The tuning constant controls the trade-off between robustness and efficiency:</p>",
                "<ul>",
                "<li>Smaller values: More robust but less efficient</li>",
                "<li>Larger values: Less robust but more efficient</li>",
                "<li>Common defaults: Huber (1.345), Tukey (4.685), Hampel (1.7, 3.4, 8.5)</li>",
                "</ul>",
                
                "<h4>When to Use Robust Cox Regression</h4>",
                "<ul>",
                "<li>Presence of outliers or influential observations</li>",
                "<li>Concerns about model misspecification</li>",
                "<li>Heavy-tailed error distributions</li>",
                "<li>Need for stable estimates across data perturbations</li>",
                "<li>Validation of standard Cox model results</li>",
                "</ul>"
            )
            
            html$setContent(content)
        }
    )
)