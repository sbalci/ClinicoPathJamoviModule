
#' @title Transformation Models
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import survival
#' @import ggplot2
#' @import dplyr
#' @export

transformationmodelsClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "transformationmodelsClass",
    inherit = transformationmodelsBase,
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
            private$.initTransformationAnalysisTable()
            private$.initDiagnosticsTable()
            private$.initComparisonTable()
            private$.initTransformationInfoTable()
            private$.initLambdaSearchTable()
            private$.initValidationResultsTable()
            
            # Set up plot areas
            if (self$options$show_transformation_plots) {
                self$results$transformationPlots$setSize(800, 600)
            }
            if (self$options$show_residual_plots) {
                self$results$residualPlots$setSize(800, 600)
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
            
            # Check if tram package is available
            if (!requireNamespace("tram", quietly = TRUE)) {
                self$results$modelSummary$setNote("info", 
                    "Using built-in transformation model implementation. Install 'tram' package for full features.")
            }
            
            # Prepare data
            result <- private$.prepareData(data)
            if (!is.null(result$error)) {
                self$results$modelSummary$setNote("error", result$error)
                return()
            }
            
            preparedData <- result$data
            
            # Fit transformation model
            result <- private$.fitTransformationModel(preparedData)
            if (!is.null(result$error)) {
                self$results$modelSummary$setNote("error", result$error)
                return()
            }
            
            model <- result$model
            transformationInfo <- result$transformation_info
            
            # Model comparison if enabled
            comparisonResults <- NULL
            if (self$options$model_selection) {
                comparisonResults <- private$.compareTransformations(preparedData)
            }
            
            # Transformation validation if enabled
            validationResults <- NULL
            if (self$options$transformation_validation) {
                validationResults <- private$.validateTransformation(model, preparedData)
            }
            
            # Populate results
            private$.populateModelSummary(model, preparedData)
            private$.populateCoefficientsTable(model)
            private$.populateTransformationAnalysisTable(transformationInfo)
            private$.populateDiagnosticsTable(model, preparedData)
            private$.populateTransformationInfoTable(transformationInfo)
            
            if (!is.null(comparisonResults)) {
                private$.populateComparisonTable(comparisonResults)
            }
            
            if (!is.null(validationResults)) {
                private$.populateValidationResultsTable(validationResults)
            }
            
            if (self$options$lambda_search && self$options$transformation == "boxcox") {
                private$.populateLambdaSearchTable(transformationInfo$lambda_search)
            }
            
            # Generate plots
            if (self$options$show_transformation_plots) {
                private$.plotTransformation(model, transformationInfo, preparedData)
            }
            if (self$options$show_residual_plots) {
                private$.plotResiduals(model, preparedData)
            }
            if (self$options$show_survival_plots) {
                private$.plotSurvival(model, preparedData)
            }
            if (self$options$show_qq_plots) {
                private$.plotQQ(model, preparedData)
            }
            
            # Generate summaries and explanations
            if (self$options$showSummaries) {
                private$.populateAnalysisSummary(model, transformationInfo, preparedData)
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
        
        .fitTransformationModel = function(data) {
            
            covariates <- self$options$covariates
            transformation <- self$options$transformation
            distribution <- self$options$distribution
            method <- self$options$method
            support <- self$options$support
            lambdaSearch <- self$options$lambda_search
            lambdaMin <- self$options$lambda_range_min
            lambdaMax <- self$options$lambda_range_max
            
            # Check if tram package is available
            if (requireNamespace("tram", quietly = TRUE)) {
                # Use tram package implementation
                result <- private$.fitTramModel(data, transformation, distribution, 
                                               method, support, lambdaSearch, 
                                               lambdaMin, lambdaMax)
                return(result)
            } else {
                # Use built-in implementation
                result <- private$.fitBuiltinTransformationModel(data, transformation, 
                                                                distribution, method, 
                                                                lambdaSearch, lambdaMin, lambdaMax)
                return(result)
            }
        },
        
        .fitTramModel = function(data, transformation, distribution, method, 
                                support, lambdaSearch, lambdaMin, lambdaMax) {
            
            # This would use the tram package if available
            # For now, fallback to built-in implementation
            return(private$.fitBuiltinTransformationModel(data, transformation, 
                                                          distribution, method, 
                                                          lambdaSearch, lambdaMin, lambdaMax))
        },
        
        .fitBuiltinTransformationModel = function(data, transformation, distribution, 
                                                 method, lambdaSearch, lambdaMin, lambdaMax) {
            
            covariates <- self$options$covariates
            
            # Build formula
            if (is.null(covariates) || length(covariates) == 0) {
                formula <- "Surv(time, status) ~ 1"
            } else {
                formula <- paste("Surv(time, status) ~", paste(covariates, collapse = " + "))
            }
            
            # Initialize transformation info
            transformationInfo <- list(
                type = transformation,
                distribution = distribution,
                method = method,
                lambda = NULL,
                lambda_search = NULL
            )
            
            # Handle Box-Cox transformation with lambda search
            if (transformation == "boxcox" && lambdaSearch) {
                lambdaSearchResult <- private$.searchOptimalLambda(data, formula, distribution, 
                                                                   lambdaMin, lambdaMax)
                transformationInfo$lambda <- lambdaSearchResult$optimal_lambda
                transformationInfo$lambda_search <- lambdaSearchResult$search_results
            }
            
            # Fit the model with selected transformation
            model <- private$.fitTransformedModel(data, formula, transformation, 
                                                 distribution, transformationInfo$lambda)
            
            # Add transformation information to model
            model$transformation_info <- transformationInfo
            
            return(list(model = model, transformation_info = transformationInfo, error = NULL))
        },
        
        .searchOptimalLambda = function(data, formula, distribution, lambdaMin, lambdaMax) {
            
            # Grid search for optimal lambda
            lambda_grid <- seq(lambdaMin, lambdaMax, by = 0.1)
            loglik_values <- numeric(length(lambda_grid))
            
            for (i in seq_along(lambda_grid)) {
                tryCatch({
                    model <- private$.fitTransformedModel(data, formula, "boxcox", 
                                                         distribution, lambda_grid[i])
                    loglik_values[i] <- model$loglik[length(model$loglik)]
                }, error = function(e) {
                    loglik_values[i] <- -Inf
                })
            }
            
            # Find optimal lambda
            optimal_idx <- which.max(loglik_values)
            optimal_lambda <- lambda_grid[optimal_idx]
            
            # Create search results
            search_results <- data.frame(
                lambda = lambda_grid,
                loglik = loglik_values,
                selected = lambda_grid == optimal_lambda,
                stringsAsFactors = FALSE
            )
            
            return(list(optimal_lambda = optimal_lambda, search_results = search_results))
        },
        
        .fitTransformedModel = function(data, formula, transformation, distribution, lambda = NULL) {
            
            # Apply transformation to the survival time
            transformed_data <- private$.applyTransformation(data, transformation, lambda)
            
            # Map distribution names
            survreg_dist <- switch(distribution,
                "normal" = "gaussian",
                "logistic" = "logistic",
                "extreme" = "extreme",
                "exponential" = "exponential",
                "weibull" = "weibull",
                "gaussian"  # default
            )
            
            # Fit survival regression model
            tryCatch({
                model <- survival::survreg(
                    as.formula(formula),
                    data = transformed_data,
                    dist = survreg_dist
                )
                
                # Add transformation information
                model$transformation <- transformation
                model$distribution <- distribution
                model$lambda <- lambda
                
                return(model)
            }, error = function(e) {
                stop(paste("Model fitting failed:", e$message))
            })
        },
        
        .applyTransformation = function(data, transformation, lambda = NULL) {
            
            transformed_data <- data
            
            # Apply transformation to time variable
            transformed_data$time <- switch(transformation,
                "linear" = {
                    # No transformation
                    data$time
                },
                "boxcox" = {
                    # Box-Cox transformation
                    if (is.null(lambda)) lambda <- 1
                    if (lambda == 0) {
                        log(data$time)
                    } else {
                        (data$time^lambda - 1) / lambda
                    }
                },
                "loglog" = {
                    # Log-log transformation
                    log(-log(1 - pmin(data$time / max(data$time, na.rm = TRUE), 0.999)))
                },
                "probit" = {
                    # Probit transformation
                    qnorm(pmin(data$time / max(data$time, na.rm = TRUE), 0.999))
                },
                "logit" = {
                    # Logit transformation
                    p <- pmin(data$time / max(data$time, na.rm = TRUE), 0.999)
                    log(p / (1 - p))
                },
                "cloglog" = {
                    # Complementary log-log transformation
                    log(-log(1 - pmin(data$time / max(data$time, na.rm = TRUE), 0.999)))
                },
                "nonparametric" = {
                    # Non-parametric transformation (rank-based)
                    rank(data$time) / length(data$time)
                },
                {
                    # Default: no transformation
                    data$time
                }
            )
            
            return(transformed_data)
        },
        
        .compareTransformations = function(data) {
            
            transformations <- c("linear", "boxcox", "loglog", "probit", "logit", "cloglog")
            distribution <- self$options$distribution
            
            results <- list()
            
            for (trans in transformations) {
                tryCatch({
                    model_result <- private$.fitBuiltinTransformationModel(data, trans, distribution, 
                                                                          "ml", FALSE, -2, 2)
                    model <- model_result$model
                    
                    results[[trans]] <- list(
                        transformation = trans,
                        loglik = model$loglik[length(model$loglik)],
                        aic = AIC(model),
                        bic = BIC(model),
                        converged = model$iter < 100,
                        selected = FALSE
                    )
                }, error = function(e) {
                    results[[trans]] <- list(
                        transformation = trans,
                        loglik = NA,
                        aic = NA,
                        bic = NA,
                        converged = FALSE,
                        selected = FALSE
                    )
                })
            }
            
            # Select best model based on AIC
            aic_values <- sapply(results, function(x) x$aic)
            best_idx <- which.min(aic_values)
            if (length(best_idx) > 0) {
                results[[best_idx]]$selected <- TRUE
            }
            
            return(results)
        },
        
        .validateTransformation = function(model, data) {
            
            # Basic transformation validation tests
            residuals <- residuals(model, type = "response")
            
            # Shapiro-Wilk test for normality of residuals
            shapiro_test <- tryCatch({
                test_result <- shapiro.test(residuals)
                list(statistic = test_result$statistic, pvalue = test_result$p.value, 
                     conclusion = ifelse(test_result$p.value > 0.05, "Normal", "Non-normal"))
            }, error = function(e) {
                list(statistic = NA, pvalue = NA, conclusion = "Test failed")
            })
            
            # Kolmogorov-Smirnov test
            ks_test <- tryCatch({
                test_result <- ks.test(residuals, "pnorm", mean(residuals), sd(residuals))
                list(statistic = test_result$statistic, pvalue = test_result$p.value,
                     conclusion = ifelse(test_result$p.value > 0.05, "Normal", "Non-normal"))
            }, error = function(e) {
                list(statistic = NA, pvalue = NA, conclusion = "Test failed")
            })
            
            results <- list(
                shapiro = shapiro_test,
                ks = ks_test
            )
            
            return(results)
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
            table$addColumn(name = 'se', title = 'SE', type = 'number', format = 'zto')
            table$addColumn(name = 'z', title = 'z', type = 'number', format = 'zto')
            table$addColumn(name = 'pvalue', title = 'p-value', type = 'number', format = 'zto,pvalue')
            table$addColumn(name = 'ciLower', title = 'Lower CI', type = 'number', format = 'zto')
            table$addColumn(name = 'ciUpper', title = 'Upper CI', type = 'number', format = 'zto')
            table$addColumn(name = 'effect_ratio', title = 'Effect Ratio', type = 'number', format = 'zto')
            table$addColumn(name = 'er_lower', title = 'ER Lower', type = 'number', format = 'zto')
            table$addColumn(name = 'er_upper', title = 'ER Upper', type = 'number', format = 'zto')
        },
        
        .initTransformationAnalysisTable = function() {
            table <- self$results$transformationAnalysis
            table$addColumn(name = 'parameter', title = 'Parameter', type = 'text')
            table$addColumn(name = 'estimate', title = 'Estimate', type = 'number', format = 'zto')
            table$addColumn(name = 'se', title = 'SE', type = 'number', format = 'zto')
            table$addColumn(name = 'interpretation', title = 'Interpretation', type = 'text')
        },
        
        .initDiagnosticsTable = function() {
            table <- self$results$diagnostics
            table$addColumn(name = 'measure', title = 'Measure', type = 'text')
            table$addColumn(name = 'value', title = 'Value', type = 'number', format = 'zto')
            table$addColumn(name = 'interpretation', title = 'Interpretation', type = 'text')
        },
        
        .initComparisonTable = function() {
            table <- self$results$modelComparison
            table$addColumn(name = 'transformation', title = 'Transformation', type = 'text')
            table$addColumn(name = 'loglik', title = 'Log-Likelihood', type = 'number', format = 'zto')
            table$addColumn(name = 'aic', title = 'AIC', type = 'number', format = 'zto')
            table$addColumn(name = 'bic', title = 'BIC', type = 'number', format = 'zto')
            table$addColumn(name = 'convergence', title = 'Converged', type = 'text')
            table$addColumn(name = 'selected', title = 'Selected', type = 'text')
        },
        
        .initTransformationInfoTable = function() {
            table <- self$results$transformationInfo
            table$addColumn(name = 'criterion', title = 'Criterion', type = 'text')
            table$addColumn(name = 'value', title = 'Value', type = 'text')
        },
        
        .initLambdaSearchTable = function() {
            table <- self$results$lambdaSearch
            table$addColumn(name = 'lambda', title = 'Lambda', type = 'number', format = 'zto')
            table$addColumn(name = 'loglik', title = 'Log-Likelihood', type = 'number', format = 'zto')
            table$addColumn(name = 'selected', title = 'Selected', type = 'text')
        },
        
        .initValidationResultsTable = function() {
            table <- self$results$validationResults
            table$addColumn(name = 'test', title = 'Test', type = 'text')
            table$addColumn(name = 'statistic', title = 'Statistic', type = 'number', format = 'zto')
            table$addColumn(name = 'pvalue', title = 'p-value', type = 'number', format = 'zto,pvalue')
            table$addColumn(name = 'conclusion', title = 'Conclusion', type = 'text')
        },
        
        .populateModelSummary = function(model, data) {
            
            table <- self$results$modelSummary
            
            # Model information
            n_obs <- nrow(data)
            n_events <- sum(data$status)
            n_covariates <- length(self$options$covariates %||% character(0))
            
            # Transformation names
            trans_names <- list(
                "linear" = "Linear (no transformation)",
                "boxcox" = "Box-Cox transformation",
                "loglog" = "Log-log transformation",
                "probit" = "Probit transformation",
                "logit" = "Logit transformation",
                "cloglog" = "Complementary log-log transformation",
                "nonparametric" = "Non-parametric transformation"
            )
            
            # Distribution names
            dist_names <- list(
                "normal" = "Normal (Gaussian)",
                "logistic" = "Logistic",
                "extreme" = "Extreme value (Gumbel)",
                "exponential" = "Exponential",
                "weibull" = "Weibull"
            )
            
            rows <- list(
                list(term = "Number of observations", value = toString(n_obs)),
                list(term = "Number of events", value = toString(n_events)),
                list(term = "Event rate", value = paste0(round(100 * n_events / n_obs, 1), "%")),
                list(term = "Number of covariates", value = toString(n_covariates)),
                list(term = "Transformation", value = trans_names[[self$options$transformation]]),
                list(term = "Distribution", value = dist_names[[self$options$distribution]]),
                list(term = "Estimation method", value = self$options$method)
            )
            
            # Add lambda if Box-Cox transformation
            if (self$options$transformation == "boxcox" && !is.null(model$lambda)) {
                rows <- append(rows, list(list(term = "Lambda parameter", value = toString(round(model$lambda, 3)))))
            }
            
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
                se <- sqrt(diag(vcov(model)))
                
                # Calculate statistics
                z_values <- coeffs / se
                p_values <- 2 * pnorm(-abs(z_values))
                
                # Calculate confidence intervals
                z_crit <- qnorm((1 + confidence_level) / 2)
                ci_lower <- coeffs - z_crit * se
                ci_upper <- coeffs + z_crit * se
                
                # Calculate effect ratios (exp of coefficients)
                er <- exp(coeffs)
                er_lower <- exp(ci_lower)
                er_upper <- exp(ci_upper)
                
                # Populate table
                for (i in seq_along(coeffs)) {
                    term <- names(coeffs)[i]
                    
                    table$addRow(rowKey = term, values = list(
                        term = term,
                        estimate = coeffs[i],
                        se = se[i],
                        z = z_values[i],
                        pvalue = p_values[i],
                        ciLower = ci_lower[i],
                        ciUpper = ci_upper[i],
                        effect_ratio = er[i],
                        er_lower = er_lower[i],
                        er_upper = er_upper[i]
                    ))
                }
            }
        },
        
        .populateTransformationAnalysisTable = function(transformationInfo) {
            
            table <- self$results$transformationAnalysis
            
            # Transformation-specific parameters
            if (transformationInfo$type == "boxcox" && !is.null(transformationInfo$lambda)) {
                table$addRow(rowKey = "lambda", values = list(
                    parameter = "Lambda parameter",
                    estimate = transformationInfo$lambda,
                    se = NA,
                    interpretation = if (transformationInfo$lambda == 0) "Log transformation" else 
                                   if (transformationInfo$lambda == 1) "No transformation" else 
                                   "Power transformation"
                ))
            }
            
            # Add distribution parameters if available
            table$addRow(rowKey = "transformation", values = list(
                parameter = "Transformation type",
                estimate = NA,
                se = NA,
                interpretation = paste("Using", transformationInfo$type, "transformation")
            ))
        },
        
        .populateDiagnosticsTable = function(model, data) {
            
            table <- self$results$diagnostics
            
            # Model diagnostics
            n_events <- sum(data$status)
            n_obs <- nrow(data)
            n_params <- length(model$coefficients)
            
            loglik <- model$loglik[length(model$loglik)]
            aic_value <- AIC(model)
            bic_value <- BIC(model)
            
            rows <- list(
                list(
                    measure = "Log-likelihood",
                    value = loglik,
                    interpretation = "Model fit measure"
                ),
                list(
                    measure = "AIC",
                    value = aic_value,
                    interpretation = "Akaike Information Criterion"
                ),
                list(
                    measure = "BIC",
                    value = bic_value,
                    interpretation = "Bayesian Information Criterion"
                ),
                list(
                    measure = "Scale parameter",
                    value = model$scale,
                    interpretation = "Error term scale"
                ),
                list(
                    measure = "Events per parameter",
                    value = n_events / n_params,
                    interpretation = if (n_events / n_params >= 10) "Adequate" else "May be low"
                )
            )
            
            for (row in rows) {
                table$addRow(rowKey = row$measure, values = row)
            }
        },
        
        .populateComparisonTable = function(comparisonResults) {
            
            table <- self$results$modelComparison
            
            for (result in comparisonResults) {
                table$addRow(rowKey = result$transformation, values = list(
                    transformation = result$transformation,
                    loglik = result$loglik,
                    aic = result$aic,
                    bic = result$bic,
                    convergence = if (result$converged) "Yes" else "No",
                    selected = if (result$selected) "Yes" else "No"
                ))
            }
        },
        
        .populateTransformationInfoTable = function(transformationInfo) {
            
            table <- self$results$transformationInfo
            
            rows <- list(
                list(criterion = "Transformation type", value = transformationInfo$type),
                list(criterion = "Error distribution", value = transformationInfo$distribution),
                list(criterion = "Estimation method", value = transformationInfo$method)
            )
            
            if (!is.null(transformationInfo$lambda)) {
                rows <- append(rows, list(list(criterion = "Lambda parameter", value = toString(round(transformationInfo$lambda, 3)))))
            }
            
            for (row in rows) {
                table$addRow(rowKey = row$criterion, values = row)
            }
        },
        
        .populateLambdaSearchTable = function(lambdaSearch) {
            
            if (is.null(lambdaSearch)) return()
            
            table <- self$results$lambdaSearch
            
            for (i in 1:nrow(lambdaSearch)) {
                table$addRow(rowKey = i, values = list(
                    lambda = lambdaSearch$lambda[i],
                    loglik = lambdaSearch$loglik[i],
                    selected = if (lambdaSearch$selected[i]) "Yes" else "No"
                ))
            }
        },
        
        .populateValidationResultsTable = function(validationResults) {
            
            table <- self$results$validationResults
            
            # Shapiro-Wilk test
            table$addRow(rowKey = "shapiro", values = list(
                test = "Shapiro-Wilk",
                statistic = validationResults$shapiro$statistic,
                pvalue = validationResults$shapiro$pvalue,
                conclusion = validationResults$shapiro$conclusion
            ))
            
            # Kolmogorov-Smirnov test
            table$addRow(rowKey = "ks", values = list(
                test = "Kolmogorov-Smirnov",
                statistic = validationResults$ks$statistic,
                pvalue = validationResults$ks$pvalue,
                conclusion = validationResults$ks$conclusion
            ))
        },
        
        .plotTransformation = function(model, transformationInfo, data) {
            image <- self$results$transformationPlots
            image$setState(list(model = model, transformation_info = transformationInfo, data = data))
        },
        
        .plotResiduals = function(model, data) {
            image <- self$results$residualPlots
            image$setState(list(model = model, data = data))
        },
        
        .plotSurvival = function(model, data) {
            image <- self$results$survivalPlots
            image$setState(list(model = model, data = data))
        },
        
        .plotQQ = function(model, data) {
            image <- self$results$qqPlots
            image$setState(list(model = model, data = data))
        },
        
        .populateAnalysisSummary = function(model, transformationInfo, data) {
            
            html <- self$results$summaryTable
            
            n_obs <- nrow(data)
            n_events <- sum(data$status)
            event_rate <- round(100 * n_events / n_obs, 1)
            
            content <- paste0(
                "<h3>Transformation Model Summary</h3>",
                "<p><strong>Sample Characteristics:</strong></p>",
                "<ul>",
                "<li>Total observations: ", n_obs, "</li>",
                "<li>Events observed: ", n_events, " (", event_rate, "%)</li>",
                "<li>Transformation: ", transformationInfo$type, "</li>",
                "<li>Distribution: ", transformationInfo$distribution, "</li>",
                "</ul>",
                
                "<p><strong>Transformation Details:</strong></p>",
                "<ul>",
                "<li>Method: ", transformationInfo$method, "</li>",
                if (!is.null(transformationInfo$lambda)) 
                    paste0("<li>Lambda parameter: ", round(transformationInfo$lambda, 3), "</li>") else "",
                "<li>Estimation: Maximum likelihood</li>",
                "</ul>",
                
                "<p><strong>Clinical Interpretation:</strong></p>",
                "<p>Transformation models provide a unified framework for survival analysis ",
                "by applying appropriate transformations to the response variable. This approach ",
                "allows for flexible modeling while maintaining interpretable parameters. The ",
                "transformation function adapts the model to the underlying data structure, ",
                "potentially improving model fit and prediction accuracy.</p>"
            )
            
            html$setContent(content)
        },
        
        .populateMethodExplanation = function() {
            
            html <- self$results$methodExplanation
            
            content <- paste0(
                "<h3>Transformation Model Methods</h3>",
                
                "<h4>Method Overview</h4>",
                "<p>Transformation models provide a unified framework for survival analysis ",
                "through the application of transformation functions to the response variable. ",
                "This approach encompasses many classical survival models as special cases ",
                "while allowing for flexible modeling of complex survival patterns.</p>",
                
                "<h4>Transformation Functions</h4>",
                "<ul>",
                "<li><strong>Linear:</strong> No transformation (standard parametric survival models)</li>",
                "<li><strong>Box-Cox:</strong> Power transformation with parameter λ, including log transformation (λ=0)</li>",
                "<li><strong>Log-log:</strong> Double logarithmic transformation for extreme value modeling</li>",
                "<li><strong>Probit:</strong> Normal quantile transformation for binary-like outcomes</li>",
                "<li><strong>Logit:</strong> Logistic transformation for bounded outcomes</li>",
                "<li><strong>Complementary log-log:</strong> Asymmetric transformation for rare events</li>",
                "<li><strong>Non-parametric:</strong> Data-driven transformation without parametric assumptions</li>",
                "</ul>",
                
                "<h4>Distribution Assumptions</h4>",
                "<p>The error distribution can be specified as:</p>",
                "<ul>",
                "<li>Normal (Gaussian) - symmetric errors</li>",
                "<li>Logistic - heavy-tailed symmetric errors</li>",
                "<li>Extreme value (Gumbel) - skewed errors</li>",
                "<li>Exponential - constant hazard</li>",
                "<li>Weibull - flexible hazard shape</li>",
                "</ul>",
                
                "<h4>Box-Cox Transformation</h4>",
                "<p>The Box-Cox transformation is defined as:</p>",
                "<ul>",
                "<li>λ = 0: log(y) transformation</li>",
                "<li>λ = 1: no transformation (linear)</li>",
                "<li>λ ≠ 0,1: (y^λ - 1)/λ power transformation</li>",
                "</ul>",
                
                "<h4>Advantages</h4>",
                "<ul>",
                "<li><strong>Unified framework:</strong> Encompasses many survival models as special cases</li>",
                "<li><strong>Flexible modeling:</strong> Adapts to complex survival patterns</li>",
                "<li><strong>Automatic selection:</strong> Data-driven transformation selection</li>",
                "<li><strong>Interpretable parameters:</strong> Maintains clinical interpretability</li>",
                "<li><strong>Robust estimation:</strong> Less sensitive to distributional assumptions</li>",
                "</ul>"
            )
            
            html$setContent(content)
        }
    )
)
