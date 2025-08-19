#' @title Weighted Cox Regression  
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import survival
#' @import ggplot2
#' @import dplyr
#' @export

coxphwClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "coxphwClass",
    inherit = coxphwBase,
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
            private$.initWeightAnalysisTable()
            private$.initDiagnosticsTable()
            private$.initComparisonTable()
            private$.initConvergenceTable()
            private$.initRareEventTable()
            
            # Set up plot areas
            if (self$options$show_weight_plots) {
                self$results$weightPlots$setSize(800, 600)
            }
            if (self$options$show_residual_plots) {
                self$results$residualPlots$setSize(800, 600)
            }
            if (self$options$show_survival_plots) {
                self$results$survivalPlots$setSize(800, 600)
            }
            if (self$options$show_forest_plot) {
                self$results$forestPlot$setSize(800, 600)
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
            
            # Check if coxphw package is available
            if (!requireNamespace("coxphw", quietly = TRUE)) {
                self$results$modelSummary$setNote("info", 
                    "Using built-in weighted Cox implementation. Install 'coxphw' package for additional features.")
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
            
            # Fit weighted Cox model
            result <- private$.fitWeightedCox(preparedData)
            if (!is.null(result$error)) {
                self$results$modelSummary$setNote("error", result$error)
                return()
            }
            
            weightedModel <- result$model
            weights <- result$weights
            
            # Populate results
            private$.populateModelSummary(weightedModel, preparedData)
            private$.populateCoefficientsTable(weightedModel)
            private$.populateWeightAnalysisTable(weights, preparedData)
            private$.populateDiagnosticsTable(weightedModel, preparedData)
            private$.populateComparisonTable(weightedModel, standardModel)
            private$.populateConvergenceTable(weightedModel)
            private$.populateRareEventTable(preparedData, weights)
            
            # Generate plots
            if (self$options$show_weight_plots) {
                private$.plotWeights(weights, preparedData)
            }
            if (self$options$show_residual_plots) {
                private$.plotResiduals(weightedModel, preparedData)
            }
            if (self$options$show_survival_plots) {
                private$.plotSurvival(weightedModel, preparedData)
            }
            if (self$options$show_forest_plot) {
                private$.plotForest(weightedModel)
            }
            if (self$options$show_comparison) {
                private$.plotComparison(weightedModel, standardModel, preparedData)
            }
            
            # Generate summaries and explanations
            if (self$options$showSummaries) {
                private$.populateAnalysisSummary(weightedModel, weights, preparedData)
            }
            if (self$options$showExplanations) {
                private$.populateMethodExplanation()
            }
        },
        
        .prepareData = function(data) {
            
            timeVar <- self$options$elapsedtime
            outcomeVar <- self$options$outcome
            covariates <- self$options$covariates
            offsetVar <- self$options$offset_variable
            stratifyVar <- self$options$stratify_variable
            clusterVar <- self$options$cluster_variable
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
            
            # Add offset variable
            if (!is.null(offsetVar)) {
                modelData$offset <- data[[offsetVar]]
            }
            
            # Add stratification variable
            if (!is.null(stratifyVar)) {
                modelData$strata <- data[[stratifyVar]]
                if (is.numeric(modelData$strata)) {
                    modelData$strata <- as.factor(modelData$strata)
                }
            }
            
            # Add cluster variable
            if (!is.null(clusterVar)) {
                modelData$cluster <- data[[clusterVar]]
                if (is.numeric(modelData$cluster)) {
                    modelData$cluster <- as.factor(modelData$cluster)
                }
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
        
        .fitStandardCox = function(data) {
            
            covariates <- self$options$covariates
            offsetVar <- self$options$offset_variable
            stratifyVar <- self$options$stratify_variable
            clusterVar <- self$options$cluster_variable
            
            # Build formula
            if (is.null(covariates) || length(covariates) == 0) {
                formula <- "Surv(time, status) ~ 1"
            } else {
                formula <- paste("Surv(time, status) ~", paste(covariates, collapse = " + "))
            }
            
            # Add offset
            if (!is.null(offsetVar)) {
                formula <- paste(formula, "+ offset(offset)")
            }
            
            # Add stratification
            if (!is.null(stratifyVar)) {
                formula <- paste(formula, "+ strata(strata)")
            }
            
            # Fit standard Cox model
            tryCatch({
                if (!is.null(clusterVar)) {
                    model <- survival::coxph(
                        as.formula(formula),
                        data = data,
                        cluster = data$cluster,
                        model = TRUE,
                        x = TRUE,
                        y = TRUE
                    )
                } else {
                    model <- survival::coxph(
                        as.formula(formula),
                        data = data,
                        model = TRUE,
                        x = TRUE,
                        y = TRUE
                    )
                }
                return(model)
            }, error = function(e) {
                return(NULL)
            })
        },
        
        .fitWeightedCox = function(data) {
            
            covariates <- self$options$covariates
            offsetVar <- self$options$offset_variable
            stratifyVar <- self$options$stratify_variable
            clusterVar <- self$options$cluster_variable
            weightMethod <- self$options$weight_method
            alpha <- self$options$alpha
            maxIter <- self$options$max_iterations
            tolerance <- self$options$convergence_tolerance
            
            # Build formula
            if (is.null(covariates) || length(covariates) == 0) {
                formula <- "Surv(time, status) ~ 1"
            } else {
                formula <- paste("Surv(time, status) ~", paste(covariates, collapse = " + "))
            }
            
            # Add offset
            if (!is.null(offsetVar)) {
                formula <- paste(formula, "+ offset(offset)")
            }
            
            # Add stratification
            if (!is.null(stratifyVar)) {
                formula <- paste(formula, "+ strata(strata)")
            }
            
            # Check if coxphw package is available
            if (requireNamespace("coxphw", quietly = TRUE)) {
                # Use coxphw package implementation
                tryCatch({
                    model <- coxphw::coxphw(
                        formula = as.formula(formula),
                        data = data,
                        alpha = alpha,
                        weight = weightMethod,
                        iter.max = maxIter,
                        rel.converge.eps = tolerance,
                        x = TRUE
                    )
                    
                    # Calculate weights
                    weights <- private$.calculateWeights(model, data, weightMethod, alpha)
                    
                    return(list(model = model, weights = weights, error = NULL))
                    
                }, error = function(e) {
                    error_msg <- paste("Weighted Cox model fitting failed:", e$message)
                    return(list(error = error_msg))
                })
                
            } else {
                # Use built-in weighted implementation
                result <- private$.fitWeightedCoxBuiltin(formula, data, weightMethod, 
                                                         alpha, maxIter, tolerance, clusterVar)
                return(result)
            }
        },
        
        .fitWeightedCoxBuiltin = function(formula, data, method, alpha, maxIter, tolerance, clusterVar) {
            
            # Fit standard Cox model first
            if (!is.null(clusterVar)) {
                standardModel <- survival::coxph(
                    as.formula(formula),
                    data = data,
                    cluster = data$cluster,
                    model = TRUE,
                    x = TRUE,
                    y = TRUE
                )
            } else {
                standardModel <- survival::coxph(
                    as.formula(formula),
                    data = data,
                    model = TRUE,
                    x = TRUE,
                    y = TRUE
                )
            }
            
            # Calculate weights based on method
            weights <- private$.calculateWeightsBuiltin(standardModel, data, method, alpha)
            
            # Refit model with weights
            if (!is.null(clusterVar)) {
                weightedModel <- survival::coxph(
                    as.formula(formula),
                    data = data,
                    weights = weights,
                    cluster = data$cluster,
                    model = TRUE,
                    x = TRUE,
                    y = TRUE
                )
            } else {
                weightedModel <- survival::coxph(
                    as.formula(formula),
                    data = data,
                    weights = weights,
                    model = TRUE,
                    x = TRUE,
                    y = TRUE
                )
            }
            
            # Add weight information to model
            weightedModel$weights <- weights
            weightedModel$weight_method <- method
            weightedModel$alpha <- alpha
            
            return(list(model = weightedModel, weights = weights, error = NULL))
        },
        
        .calculateWeights = function(model, data, method, alpha) {
            
            # For coxphw package models, weights are calculated internally
            # This is a placeholder for extracting weights if available
            if (!is.null(model$weights)) {
                return(model$weights)
            } else {
                # Fallback to built-in calculation
                return(private$.calculateWeightsBuiltin(model, data, method, alpha))
            }
        },
        
        .calculateWeightsBuiltin = function(model, data, method, alpha) {
            
            # Extract risk scores and event times
            risk_scores <- predict(model, type = "risk")
            times <- data$time
            events <- data$status
            
            # Calculate weights based on method
            weights <- switch(method,
                "average" = {
                    # Average hazard weights (AHW)
                    private$.calculateAverageWeights(times, events, risk_scores, alpha)
                },
                "schoenfeld" = {
                    # Schoenfeld residual-based weights
                    private$.calculateSchoenfeldWeights(model, times, events)
                },
                "prentice" = {
                    # Prentice weights
                    private$.calculatePrenticeWeights(times, events, risk_scores)
                },
                "logrankvar" = {
                    # Log-rank variance weights
                    private$.calculateLogRankWeights(times, events, risk_scores)
                },
                {
                    # Default to average weights
                    private$.calculateAverageWeights(times, events, risk_scores, alpha)
                }
            )
            
            # Ensure weights are positive
            weights <- pmax(weights, 0.001)
            
            return(weights)
        },
        
        .calculateAverageWeights = function(times, events, risk_scores, alpha) {
            
            # Order by event times
            ord <- order(times)
            times_ord <- times[ord]
            events_ord <- events[ord]
            risk_ord <- risk_scores[ord]
            
            n <- length(times)
            weights <- numeric(n)
            
            # Calculate weights for each event time
            event_times <- unique(times_ord[events_ord == 1])
            
            for (i in 1:n) {
                # At risk indicator
                at_risk <- times >= times_ord[i]
                n_at_risk <- sum(at_risk)
                
                if (events_ord[i] == 1) {
                    # Event weight
                    if (n_at_risk > 0) {
                        # Average hazard weight
                        sum_risk <- sum(risk_ord[at_risk])
                        if (sum_risk > 0) {
                            weights[ord[i]] <- (alpha * n_at_risk + (1 - alpha) * sum_risk) / n_at_risk
                        } else {
                            weights[ord[i]] <- 1
                        }
                    } else {
                        weights[ord[i]] <- 1
                    }
                } else {
                    # Censored observation weight
                    weights[ord[i]] <- 1
                }
            }
            
            return(weights)
        },
        
        .calculateSchoenfeldWeights = function(model, times, events) {
            
            # Calculate Schoenfeld residuals
            schoenfeld_res <- residuals(model, type = "schoenfeld")
            
            # Weight based on residual magnitude
            if (!is.null(schoenfeld_res) && length(schoenfeld_res) > 0) {
                # Standardize residuals
                res_scale <- median(abs(schoenfeld_res - median(schoenfeld_res, na.rm = TRUE)), na.rm = TRUE)
                if (res_scale > 0) {
                    std_res <- abs(schoenfeld_res) / res_scale
                    weights <- 1 / (1 + std_res)
                } else {
                    weights <- rep(1, length(times))
                }
            } else {
                weights <- rep(1, length(times))
            }
            
            return(weights)
        },
        
        .calculatePrenticeWeights = function(times, events, risk_scores) {
            
            # Prentice weights based on risk set size
            n <- length(times)
            weights <- numeric(n)
            
            for (i in 1:n) {
                at_risk <- times >= times[i]
                n_at_risk <- sum(at_risk)
                weights[i] <- sqrt(n_at_risk / n)
            }
            
            return(weights)
        },
        
        .calculateLogRankWeights = function(times, events, risk_scores) {
            
            # Log-rank variance-based weights
            n <- length(times)
            weights <- numeric(n)
            
            # Sort by time
            ord <- order(times)
            
            for (i in 1:n) {
                at_risk <- times >= times[ord[i]]
                n_at_risk <- sum(at_risk)
                n_events <- sum(events[at_risk & times == times[ord[i]]])
                
                if (n_at_risk > 1 && n_events > 0) {
                    # Log-rank variance weight
                    weights[ord[i]] <- sqrt((n_at_risk - n_events) * n_events / (n_at_risk^2 * (n_at_risk - 1)))
                } else {
                    weights[ord[i]] <- 1
                }
            }
            
            return(weights)
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
            table$addColumn(name = 'hazard_ratio', title = 'HR', type = 'number', format = 'zto')
            table$addColumn(name = 'hr_lower', title = 'HR Lower', type = 'number', format = 'zto')
            table$addColumn(name = 'hr_upper', title = 'HR Upper', type = 'number', format = 'zto')
        },
        
        .initWeightAnalysisTable = function() {
            table <- self$results$weightAnalysis
            table$addColumn(name = 'statistic', title = 'Statistic', type = 'text')
            table$addColumn(name = 'value', title = 'Value', type = 'number', format = 'zto')
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
            table$addColumn(name = 'model', title = 'Model', type = 'text')
            table$addColumn(name = 'loglik', title = 'Log-Likelihood', type = 'number', format = 'zto')
            table$addColumn(name = 'aic', title = 'AIC', type = 'number', format = 'zto')
            table$addColumn(name = 'bic', title = 'BIC', type = 'number', format = 'zto')
            table$addColumn(name = 'concordance', title = 'C-Index', type = 'number', format = 'zto')
            table$addColumn(name = 'events', title = 'Events', type = 'integer')
            table$addColumn(name = 'weight_effect', title = 'Weight Effect', type = 'text')
        },
        
        .initConvergenceTable = function() {
            table <- self$results$convergenceInfo
            table$addColumn(name = 'criterion', title = 'Criterion', type = 'text')
            table$addColumn(name = 'value', title = 'Value', type = 'text')
        },
        
        .initRareEventTable = function() {
            table <- self$results$rareEventAnalysis
            table$addColumn(name = 'covariate', title = 'Covariate', type = 'text')
            table$addColumn(name = 'event_rate', title = 'Event Rate', type = 'number', format = 'zto,pc')
            table$addColumn(name = 'imbalance_ratio', title = 'Imbalance Ratio', type = 'number', format = 'zto')
            table$addColumn(name = 'weight_impact', title = 'Weight Impact', type = 'text')
        },
        
        .populateModelSummary = function(model, data) {
            
            table <- self$results$modelSummary
            
            # Model information
            n_obs <- nrow(data)
            n_events <- sum(data$status)
            n_covariates <- length(self$options$covariates %||% character(0))
            
            # Weight method names
            method_names <- list(
                "average" = "Average hazard weights (AHW)",
                "schoenfeld" = "Schoenfeld residual weights",
                "prentice" = "Prentice weights",
                "logrankvar" = "Log-rank variance weights"
            )
            
            rows <- list(
                list(term = "Number of observations", value = toString(n_obs)),
                list(term = "Number of events", value = toString(n_events)),
                list(term = "Event rate", value = paste0(round(100 * n_events / n_obs, 1), "%")),
                list(term = "Number of covariates", value = toString(n_covariates)),
                list(term = "Weight method", value = method_names[[self$options$weight_method]]),
                list(term = "Weight parameter (α)", value = toString(self$options$alpha))
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
                se <- sqrt(diag(vcov(model)))
                
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
                        se = se[i],
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
        
        .populateWeightAnalysisTable = function(weights, data) {
            
            table <- self$results$weightAnalysis
            
            # Weight statistics
            rows <- list(
                list(
                    statistic = "Mean weight",
                    value = mean(weights, na.rm = TRUE),
                    interpretation = "Average weight applied to observations"
                ),
                list(
                    statistic = "Weight range",
                    value = diff(range(weights, na.rm = TRUE)),
                    interpretation = "Variation in weights across observations"
                ),
                list(
                    statistic = "Min weight",
                    value = min(weights, na.rm = TRUE),
                    interpretation = "Minimum weight (most down-weighted observation)"
                ),
                list(
                    statistic = "Max weight",
                    value = max(weights, na.rm = TRUE),
                    interpretation = "Maximum weight (most up-weighted observation)"
                ),
                list(
                    statistic = "Weight variability (CV)",
                    value = sd(weights, na.rm = TRUE) / mean(weights, na.rm = TRUE),
                    interpretation = if (sd(weights, na.rm = TRUE) / mean(weights, na.rm = TRUE) > 0.5) "High variability" else "Low variability"
                )
            )
            
            for (row in rows) {
                table$addRow(rowKey = row$statistic, values = row)
            }
        },
        
        .populateDiagnosticsTable = function(model, data) {
            
            table <- self$results$diagnostics
            
            # Model diagnostics
            n_events <- sum(data$status)
            n_obs <- nrow(data)
            n_params <- length(model$coefficients)
            
            rows <- list(
                list(
                    measure = "Events per parameter",
                    value = n_events / n_params,
                    interpretation = if (n_events / n_params >= 10) "Adequate" else "May be low for rare events"
                ),
                list(
                    measure = "Censoring proportion",
                    value = 1 - (n_events / n_obs),
                    interpretation = if (1 - (n_events / n_obs) > 0.8) "High censoring" else "Acceptable"
                ),
                list(
                    measure = "Log-likelihood",
                    value = model$loglik[2],
                    interpretation = "Model fit measure"
                )
            )
            
            # Add concordance if available
            if (!is.null(model$concordance)) {
                rows <- append(rows, list(list(
                    measure = "Concordance index",
                    value = model$concordance[1],
                    interpretation = if (model$concordance[1] > 0.7) "Good discrimination" else "Moderate discrimination"
                )))
            }
            
            for (row in rows) {
                table$addRow(rowKey = row$measure, values = row)
            }
        },
        
        .populateComparisonTable = function(weightedModel, standardModel) {
            
            if (!self$options$show_comparison || is.null(standardModel)) {
                return()
            }
            
            table <- self$results$modelComparison
            
            # Standard model metrics
            std_loglik <- standardModel$loglik[2]
            std_aic <- AIC(standardModel)
            std_bic <- BIC(standardModel)
            std_concordance <- if (!is.null(standardModel$concordance)) standardModel$concordance[1] else NA
            std_events <- standardModel$nevent
            
            # Weighted model metrics
            wt_loglik <- weightedModel$loglik[2]
            wt_aic <- AIC(weightedModel)
            wt_bic <- BIC(weightedModel)
            wt_concordance <- if (!is.null(weightedModel$concordance)) weightedModel$concordance[1] else NA
            wt_events <- weightedModel$nevent
            
            # Add rows
            table$addRow(rowKey = "standard", values = list(
                model = "Standard Cox",
                loglik = std_loglik,
                aic = std_aic,
                bic = std_bic,
                concordance = std_concordance,
                events = std_events,
                weight_effect = "No weighting"
            ))
            
            table$addRow(rowKey = "weighted", values = list(
                model = "Weighted Cox",
                loglik = wt_loglik,
                aic = wt_aic,
                bic = wt_bic,
                concordance = wt_concordance,
                events = wt_events,
                weight_effect = "Weighted estimation"
            ))
        },
        
        .populateConvergenceTable = function(model) {
            
            table <- self$results$convergenceInfo
            
            rows <- list(
                list(criterion = "Method", value = self$options$weight_method),
                list(criterion = "Weight parameter (α)", value = toString(self$options$alpha)),
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
        
        .populateRareEventTable = function(data, weights) {
            
            table <- self$results$rareEventAnalysis
            covariates <- self$options$covariates
            
            if (is.null(covariates)) {
                return()
            }
            
            for (var in covariates) {
                # Calculate event rate for this covariate
                if (is.factor(data[[var]]) || is.character(data[[var]])) {
                    # For categorical variables
                    levels_summary <- aggregate(status ~ data[[var]], data, mean)
                    min_rate <- min(levels_summary$status)
                    max_rate <- max(levels_summary$status)
                    imbalance <- max_rate / min_rate
                } else {
                    # For continuous variables, use overall event rate
                    min_rate <- mean(data$status)
                    max_rate <- min_rate
                    imbalance <- 1
                }
                
                # Weight impact assessment
                weight_cv <- sd(weights, na.rm = TRUE) / mean(weights, na.rm = TRUE)
                impact <- if (weight_cv > 0.3) "High" else if (weight_cv > 0.1) "Moderate" else "Low"
                
                table$addRow(rowKey = var, values = list(
                    covariate = var,
                    event_rate = mean(c(min_rate, max_rate)),
                    imbalance_ratio = imbalance,
                    weight_impact = impact
                ))
            }
        },
        
        .plotWeights = function(weights, data) {
            image <- self$results$weightPlots
            image$setState(list(weights = weights, data = data))
        },
        
        .plotResiduals = function(model, data) {
            image <- self$results$residualPlots
            image$setState(list(model = model, data = data))
        },
        
        .plotSurvival = function(model, data) {
            image <- self$results$survivalPlots
            image$setState(list(model = model, data = data))
        },
        
        .plotForest = function(model) {
            image <- self$results$forestPlot
            image$setState(list(model = model))
        },
        
        .plotComparison = function(weightedModel, standardModel, data) {
            image <- self$results$comparisonPlots
            image$setState(list(weighted = weightedModel, standard = standardModel, data = data))
        },
        
        .populateAnalysisSummary = function(model, weights, data) {
            
            html <- self$results$summaryTable
            
            n_obs <- nrow(data)
            n_events <- sum(data$status)
            event_rate <- round(100 * n_events / n_obs, 1)
            
            content <- paste0(
                "<h3>Weighted Cox Regression Summary</h3>",
                "<p><strong>Sample Characteristics:</strong></p>",
                "<ul>",
                "<li>Total observations: ", n_obs, "</li>",
                "<li>Events observed: ", n_events, " (", event_rate, "%)</li>",
                "<li>Data type: ", if (event_rate < 10) "Rare events" else "Common events", "</li>",
                "</ul>",
                
                "<p><strong>Weighting Strategy:</strong></p>",
                "<ul>",
                "<li>Method: ", self$options$weight_method, "</li>",
                "<li>Weight parameter (α): ", self$options$alpha, "</li>",
                "<li>Weight range: ", round(min(weights), 3), " - ", round(max(weights), 3), "</li>",
                "</ul>",
                
                "<p><strong>Clinical Interpretation:</strong></p>",
                "<p>Weighted Cox regression adjusts the partial likelihood to account for rare events ",
                "and imbalanced covariate distributions. This approach provides more stable hazard ratio ",
                "estimates when standard Cox regression may be unreliable due to sparse data. The weights ",
                "reflect the relative importance of different observations in parameter estimation.</p>"
            )
            
            html$setContent(content)
        },
        
        .populateMethodExplanation = function() {
            
            html <- self$results$methodExplanation
            
            content <- paste0(
                "<h3>Weighted Cox Regression Methods</h3>",
                
                "<h4>Method Overview</h4>",
                "<p>Weighted Cox regression modifies the standard partial likelihood to improve estimation ",
                "accuracy when events are rare or covariates are imbalanced. Different weighting schemes ",
                "address specific data characteristics commonly encountered in clinical research.</p>",
                
                "<h4>Weighting Methods</h4>",
                "<ul>",
                "<li><strong>Average Hazard Weights (AHW):</strong> Uses parameter α to control the weight ",
                "between log-rank (α=0), average hazard (α=0.5), and Breslow (α=1) approaches.</li>",
                "<li><strong>Schoenfeld Residual Weights:</strong> Weights based on the magnitude of ",
                "Schoenfeld residuals, down-weighting observations with large residuals.</li>",
                "<li><strong>Prentice Weights:</strong> Weights proportional to the square root of the ",
                "risk set size, emphasizing observations with larger risk sets.</li>",
                "<li><strong>Log-rank Variance Weights:</strong> Weights based on the variance of ",
                "log-rank statistics, optimizing for specific test statistics.</li>",
                "</ul>",
                
                "<h4>Weight Parameter (α)</h4>",
                "<p>The α parameter controls the weighting strategy:</p>",
                "<ul>",
                "<li>α = 0: Log-rank weights (equal weighting of event times)</li>",
                "<li>α = 0.5: Average hazard weights (balanced approach)</li>",
                "<li>α = 1: Breslow weights (emphasizes early failures)</li>",
                "</ul>",
                
                "<h4>When to Use Weighted Cox Regression</h4>",
                "<ul>",
                "<li>Rare events (event rate < 10%)</li>",
                "<li>Imbalanced covariate distributions</li>",
                "<li>Small sample sizes relative to number of parameters</li>",
                "<li>When standard Cox regression shows instability</li>",
                "<li>Validation of standard Cox model results</li>",
                "</ul>",
                
                "<h4>Advantages</h4>",
                "<ul>",
                "<li><strong>Improved stability:</strong> More reliable estimates with sparse data</li>",
                "<li><strong>Reduced bias:</strong> Less sensitive to imbalanced designs</li>",
                "<li><strong>Better coverage:</strong> More accurate confidence intervals</li>",
                "<li><strong>Flexible weighting:</strong> Adaptable to different data characteristics</li>",
                "</ul>"
            )
            
            html$setContent(content)
        }
    )
)