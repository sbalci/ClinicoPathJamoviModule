#' @title Rank-based AFT Estimation with GEE
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import survival
#' @import ggplot2
#' @import dplyr
#' @export

raftgeeClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "raftgeeClass",
    inherit = raftgeeBase,
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
            private$.initDiagnosticsTable() 
            private$.initGEEDiagnosticsTable()
            private$.initConvergenceTable()
            private$.initModelComparisonTable()
            
            # Set up plot areas
            if (self$options$show_residual_plots) {
                self$results$residualPlots$setSize(800, 600)
            }
            if (self$options$show_qq_plots) {
                self$results$qqPlots$setSize(800, 600) 
            }
            if (self$options$show_survival_plots) {
                self$results$survivalPlots$setSize(800, 600)
            }
            if (self$options$show_acceleration_plots) {
                self$results$accelerationPlots$setSize(800, 600)
            }
            if (self$options$show_comparison_plots) {
                self$results$comparisonPlots$setSize(800, 600)
            }
            if (self$options$show_diagnostics) {
                self$results$correlationPlots$setSize(600, 600)
            }
        },
        
        .run = function() {
            
            # Get data
            data <- self$data
            
            if (is.null(self$options$elapsedtime) || is.null(self$options$outcome)) {
                return()
            }
            
            # Check if required packages are available
            if (!requireNamespace("aftgee", quietly = TRUE)) {
                self$results$modelSummary$setNote("error", 
                    "The 'aftgee' package is required for rank-based AFT estimation but is not installed.")
                return()
            }
            
            if (!requireNamespace("geepack", quietly = TRUE)) {
                self$results$modelSummary$setNote("error",
                    "The 'geepack' package is required for GEE estimation but is not installed.")
                return()
            }
            
            # Prepare data
            result <- private$.prepareData(data)
            if (!is.null(result$error)) {
                self$results$modelSummary$setNote("error", result$error)
                return()
            }
            
            preparedData <- result$data
            
            # Fit AFT-GEE model
            result <- private$.fitAFTGEEModel(preparedData)
            if (!is.null(result$error)) {
                self$results$modelSummary$setNote("error", result$error)
                return()
            }
            
            aftModel <- result$model
            coxModel <- result$cox_model
            
            # Populate results
            private$.populateModelSummary(aftModel, preparedData)
            private$.populateCoefficientsTable(aftModel)
            private$.populateDiagnosticsTable(aftModel, preparedData)
            private$.populateGEEDiagnosticsTable(aftModel)
            private$.populateConvergenceTable(aftModel)
            private$.populateModelComparisonTable(aftModel, coxModel)
            
            # Generate plots
            if (self$options$show_residual_plots) {
                private$.plotResiduals(aftModel, preparedData)
            }
            if (self$options$show_qq_plots) {
                private$.plotQQ(aftModel, preparedData)
            }
            if (self$options$show_survival_plots) {
                private$.plotSurvivalCurves(aftModel, preparedData)
            }
            if (self$options$show_acceleration_plots) {
                private$.plotAccelerationFactors(aftModel)
            }
            if (self$options$show_comparison_plots) {
                private$.plotModelComparison(aftModel, coxModel, preparedData)
            }
            if (self$options$show_diagnostics) {
                private$.plotCorrelationStructure(aftModel)
            }
            
            # Generate summaries and explanations
            if (self$options$showSummaries) {
                private$.populateAnalysisSummary(aftModel, preparedData)
            }
            if (self$options$showExplanations) {
                private$.populateMethodExplanation()
            }
        },
        
        .prepareData = function(data) {
            
            timeVar <- self$options$elapsedtime
            outcomeVar <- self$options$outcome
            covariates <- self$options$covariates
            clusterVar <- self$options$cluster_variable
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
            
            # Add cluster variable
            if (!is.null(clusterVar)) {
                modelData$cluster <- data[[clusterVar]]
                # Convert to factor if numeric
                if (is.numeric(modelData$cluster)) {
                    modelData$cluster <- as.factor(modelData$cluster)
                }
            } else {
                # If no cluster specified, each observation is its own cluster
                modelData$cluster <- 1:nrow(modelData)
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
            
            return(list(data = modelData, error = NULL))
        },
        
        .fitAFTGEEModel = function(data) {
            
            covariates <- self$options$covariates
            rankMethod <- self$options$rank_method
            corrStructure <- self$options$correlation_structure
            maxIter <- self$options$max_iterations
            tolerance <- self$options$convergence_tolerance
            robustVar <- self$options$robust_variance
            
            # Build formula
            if (is.null(covariates) || length(covariates) == 0) {
                formula <- as.formula("Surv(time, status) ~ 1")
            } else {
                formula <- as.formula(paste("Surv(time, status) ~", paste(covariates, collapse = " + ")))
            }
            
            # Map rank method
            rank_methods <- list(
                "logrank" = "logrank",
                "gehan" = "gehan",
                "normal" = "logrank",  # aftgee uses logrank for normal scores
                "wilcoxon" = "gehan"   # aftgee uses gehan for Wilcoxon-type
            )
            aft_rank_method <- rank_methods[[rankMethod]]
            
            # Map correlation structure
            corr_structures <- list(
                "independence" = "independence", 
                "exchangeable" = "exchangeable",
                "ar1" = "AR-M",
                "unstructured" = "unstructured"
            )
            aft_corr_structure <- corr_structures[[corrStructure]]
            
            tryCatch({
                # Fit AFT-GEE model using aftgee
                aftModel <- aftgee::aftgee(
                    formula = formula,
                    data = data,
                    id = data$cluster,
                    weights = data$weights,
                    corstr = aft_corr_structure,
                    rankWeights = aft_rank_method,
                    control = aftgee::aftgee.control(
                        maxIter = maxIter,
                        tol = tolerance
                    )
                )
                
                # Fit comparison Cox model
                coxModel <- tryCatch({
                    survival::coxph(formula, data = data, weights = data$weights)
                }, error = function(e) NULL)
                
                return(list(model = aftModel, cox_model = coxModel, error = NULL))
                
            }, error = function(e) {
                error_msg <- paste("AFT-GEE model fitting failed:", e$message)
                return(list(error = error_msg))
            })
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
            table$addColumn(name = 'acceleration', title = 'Accel. Factor', type = 'number', format = 'zto')
            table$addColumn(name = 'accel_lower', title = 'AF Lower', type = 'number', format = 'zto')
            table$addColumn(name = 'accel_upper', title = 'AF Upper', type = 'number', format = 'zto')
        },
        
        .initDiagnosticsTable = function() {
            table <- self$results$diagnostics
            table$addColumn(name = 'statistic', title = 'Statistic', type = 'text')
            table$addColumn(name = 'value', title = 'Value', type = 'number', format = 'zto')
            table$addColumn(name = 'interpretation', title = 'Interpretation', type = 'text')
        },
        
        .initGEEDiagnosticsTable = function() {
            table <- self$results$geeDiagnostics
            table$addColumn(name = 'parameter', title = 'Parameter', type = 'text')
            table$addColumn(name = 'estimate', title = 'Estimate', type = 'number', format = 'zto')
            table$addColumn(name = 'se', title = 'SE', type = 'number', format = 'zto')
            table$addColumn(name = 'interpretation', title = 'Interpretation', type = 'text')
        },
        
        .initConvergenceTable = function() {
            table <- self$results$convergenceInfo
            table$addColumn(name = 'criterion', title = 'Criterion', type = 'text')
            table$addColumn(name = 'value', title = 'Value', type = 'text')
        },
        
        .initModelComparisonTable = function() {
            table <- self$results$modelComparison
            table$addColumn(name = 'model', title = 'Model', type = 'text')
            table$addColumn(name = 'aic', title = 'AIC', type = 'number', format = 'zto')
            table$addColumn(name = 'loglik', title = 'Log-Likelihood', type = 'number', format = 'zto')
            table$addColumn(name = 'concordance', title = 'C-Index', type = 'number', format = 'zto')
            table$addColumn(name = 'interpretation', title = 'Interpretation', type = 'text')
        },
        
        .populateModelSummary = function(model, data) {
            
            table <- self$results$modelSummary
            
            # Model information
            rank_method_names <- list(
                "logrank" = "Log-rank",
                "gehan" = "Gehan"
            )
            
            corr_names <- list(
                "independence" = "Independence",
                "exchangeable" = "Exchangeable", 
                "AR-M" = "AR(1)",
                "unstructured" = "Unstructured"
            )
            
            # Get model details
            n_obs <- nrow(data)
            n_events <- sum(data$status)
            n_clusters <- length(unique(data$cluster))
            
            rows <- list(
                list(term = "Number of observations", value = toString(n_obs)),
                list(term = "Number of events", value = toString(n_events)),
                list(term = "Number of clusters", value = toString(n_clusters)),
                list(term = "Rank method", value = rank_method_names[[self$options$rank_method]] %||% self$options$rank_method),
                list(term = "Correlation structure", value = corr_names[[model$corstr]] %||% model$corstr),
                list(term = "Robust variance", value = if(self$options$robust_variance) "Yes" else "No")
            )
            
            # Add convergence information if available
            if (!is.null(model$converged)) {
                rows <- append(rows, list(list(term = "Converged", value = if(model$converged) "Yes" else "No")))
            }
            
            for (row in rows) {
                table$addRow(rowKey = row$term, values = row)
            }
        },
        
        .populateCoefficientsTable = function(model) {
            
            table <- self$results$coefficients
            confidence_level <- self$options$confidence_level
            
            # Get coefficient summary
            if (!is.null(model$coefficients)) {
                coef_summary <- summary(model)
                
                # Extract coefficient information
                coeffs <- coef_summary$coefficient
                
                if (!is.null(coeffs) && nrow(coeffs) > 0) {
                    for (i in 1:nrow(coeffs)) {
                        term <- rownames(coeffs)[i]
                        estimate <- coeffs[i, "Estimate"]
                        se <- coeffs[i, "SE"]
                        z_value <- coeffs[i, "z"]
                        p_value <- coeffs[i, "Pr(>|z|)"]
                        
                        # Calculate confidence intervals
                        alpha <- 1 - confidence_level
                        z_crit <- qnorm(1 - alpha/2)
                        ci_lower <- estimate - z_crit * se
                        ci_upper <- estimate + z_crit * se
                        
                        # Calculate acceleration factors (exp of coefficient)
                        accel_factor <- exp(estimate)
                        accel_lower <- exp(ci_lower)
                        accel_upper <- exp(ci_upper)
                        
                        table$addRow(rowKey = term, values = list(
                            term = term,
                            estimate = estimate,
                            se = se,
                            z = z_value,
                            pvalue = p_value,
                            ciLower = ci_lower,
                            ciUpper = ci_upper,
                            acceleration = accel_factor,
                            accel_lower = accel_lower,
                            accel_upper = accel_upper
                        ))
                    }
                }
            }
        },
        
        .populateDiagnosticsTable = function(model, data) {
            
            table <- self$results$diagnostics
            
            # Model fit diagnostics
            rows <- list()
            
            # Sample size adequacy
            n_events <- sum(data$status)
            n_covariates <- length(self$options$covariates %||% character(0))
            events_per_covariate <- if (n_covariates > 0) n_events / n_covariates else n_events
            
            rows <- append(rows, list(list(
                statistic = "Events per covariate",
                value = events_per_covariate,
                interpretation = if (events_per_covariate >= 10) "Adequate" else "May be inadequate"
            )))
            
            # Censoring proportion
            censoring_prop <- 1 - (n_events / nrow(data))
            rows <- append(rows, list(list(
                statistic = "Censoring proportion",
                value = censoring_prop,
                interpretation = if (censoring_prop < 0.8) "Acceptable" else "High censoring"
            )))
            
            # Median follow-up time
            median_time <- median(data$time, na.rm = TRUE)
            rows <- append(rows, list(list(
                statistic = "Median follow-up time",
                value = median_time,
                interpretation = "Time units as specified"
            )))
            
            for (row in rows) {
                table$addRow(rowKey = row$statistic, values = row)
            }
        },
        
        .populateGEEDiagnosticsTable = function(model) {
            
            table <- self$results$geeDiagnostics
            
            # GEE-specific diagnostics
            if (!is.null(model$working.correlation)) {
                # Working correlation parameters
                if (is.matrix(model$working.correlation)) {
                    # For exchangeable structure, extract correlation
                    if (self$options$correlation_structure == "exchangeable") {
                        if (nrow(model$working.correlation) >= 2) {
                            corr_estimate <- model$working.correlation[1, 2]
                            table$addRow(rowKey = "working_correlation", values = list(
                                parameter = "Working correlation",
                                estimate = corr_estimate,
                                se = NA,
                                interpretation = if (abs(corr_estimate) < 0.3) "Weak correlation" else 
                                               if (abs(corr_estimate) < 0.7) "Moderate correlation" else "Strong correlation"
                            ))
                        }
                    }
                }
            }
            
            # Number of iterations
            if (!is.null(model$iterations)) {
                table$addRow(rowKey = "iterations", values = list(
                    parameter = "Iterations to convergence",
                    estimate = model$iterations,
                    se = NA,
                    interpretation = if (model$iterations < self$options$max_iterations) "Converged normally" else "Maximum iterations reached"
                ))
            }
        },
        
        .populateConvergenceTable = function(model) {
            
            table <- self$results$convergenceInfo
            
            rows <- list(
                list(criterion = "Algorithm", value = "aftgee"),
                list(criterion = "Maximum iterations", value = toString(self$options$max_iterations)),
                list(criterion = "Tolerance", value = sprintf("%.2e", self$options$convergence_tolerance))
            )
            
            if (!is.null(model$converged)) {
                rows <- append(rows, list(list(criterion = "Convergence status", 
                                              value = if(model$converged) "Converged" else "Not converged")))
            }
            
            if (!is.null(model$iterations)) {
                rows <- append(rows, list(list(criterion = "Actual iterations", value = toString(model$iterations))))
            }
            
            for (row in rows) {
                table$addRow(rowKey = row$criterion, values = row)
            }
        },
        
        .populateModelComparisonTable = function(aft_model, cox_model) {
            
            table <- self$results$modelComparison
            
            # AFT model row
            aft_loglik <- if (!is.null(aft_model$loglik)) aft_model$loglik else NA
            aft_aic <- if (!is.null(aft_loglik)) -2 * aft_loglik + 2 * length(aft_model$coefficients) else NA
            
            table$addRow(rowKey = "aft", values = list(
                model = "AFT-GEE",
                aic = aft_aic,
                loglik = aft_loglik,
                concordance = NA,  # Not directly available from aftgee
                interpretation = "Rank-based accelerated failure time with GEE"
            ))
            
            # Cox model row (if available)
            if (!is.null(cox_model)) {
                cox_aic <- AIC(cox_model)
                cox_loglik <- cox_model$loglik[2]  # Final log-likelihood
                cox_concordance <- if (!is.null(cox_model$concordance)) cox_model$concordance[1] else NA
                
                table$addRow(rowKey = "cox", values = list(
                    model = "Cox PH",
                    aic = cox_aic,
                    loglik = cox_loglik,
                    concordance = cox_concordance,
                    interpretation = "Proportional hazards model for comparison"
                ))
            }
        },
        
        .plotResiduals = function(model, data) {
            
            # Generate residual plots for AFT model diagnostics
            image <- self$results$residualPlots
            image$setState(list(model = model, data = data))
        },
        
        .plotQQ = function(model, data) {
            
            # Generate Q-Q plots for model diagnostics  
            image <- self$results$qqPlots
            image$setState(list(model = model, data = data))
        },
        
        .plotSurvivalCurves = function(model, data) {
            
            # Generate AFT-based survival curves
            image <- self$results$survivalPlots
            image$setState(list(model = model, data = data))
        },
        
        .plotAccelerationFactors = function(model) {
            
            # Plot acceleration factors with confidence intervals
            image <- self$results$accelerationPlots
            image$setState(list(model = model))
        },
        
        .plotModelComparison = function(aft_model, cox_model, data) {
            
            # Compare AFT and Cox model predictions
            image <- self$results$comparisonPlots
            image$setState(list(aft_model = aft_model, cox_model = cox_model, data = data))
        },
        
        .plotCorrelationStructure = function(model) {
            
            # Visualize working correlation structure
            image <- self$results$correlationPlots
            image$setState(list(model = model))
        },
        
        .populateAnalysisSummary = function(model, data) {
            
            html <- self$results$summaryTable
            
            # Generate comprehensive analysis summary
            n_obs <- nrow(data)
            n_events <- sum(data$status)
            n_clusters <- length(unique(data$cluster))
            
            content <- paste0(
                "<h3>Rank-based AFT Analysis Summary</h3>",
                "<p><strong>Sample Characteristics:</strong></p>",
                "<ul>",
                "<li>Total observations: ", n_obs, "</li>",
                "<li>Events observed: ", n_events, " (", round(100 * n_events / n_obs, 1), "%)</li>",
                "<li>Clusters: ", n_clusters, "</li>",
                "</ul>",
                
                "<p><strong>Model Specification:</strong></p>",
                "<ul>",
                "<li>Rank method: ", self$options$rank_method, "</li>",
                "<li>Correlation structure: ", self$options$correlation_structure, "</li>",
                "<li>Robust variance: ", if(self$options$robust_variance) "Yes" else "No", "</li>",
                "</ul>",
                
                "<p><strong>Clinical Interpretation:</strong></p>",
                "<p>This accelerated failure time (AFT) model estimates the effects of covariates on the ",
                "acceleration or deceleration of the failure process. Coefficients represent log acceleration factors: ",
                "positive values indicate acceleration (shorter survival times) while negative values indicate ",
                "deceleration (longer survival times). The acceleration factors are the exponentials of the coefficients.</p>"
            )
            
            html$setContent(content)
        },
        
        .populateMethodExplanation = function() {
            
            html <- self$results$methodExplanation
            
            content <- paste0(
                "<h3>Rank-based Accelerated Failure Time Models</h3>",
                
                "<h4>Method Overview</h4>",
                "<p>Rank-based accelerated failure time (AFT) models provide an alternative to Cox proportional hazards models ",
                "for analyzing survival data. Unlike Cox models which assume proportional hazards, AFT models assume that ",
                "covariates act multiplicatively on the failure time, effectively accelerating or decelerating the survival process.</p>",
                
                "<h4>Key Advantages</h4>",
                "<ul>",
                "<li><strong>Distribution-free:</strong> No assumptions about the underlying survival distribution</li>",
                "<li><strong>Direct time interpretation:</strong> Effects are expressed as acceleration factors on survival time</li>",
                "<li><strong>Robust estimation:</strong> Based on ranks rather than likelihood</li>",
                "<li><strong>Handles correlation:</strong> GEE framework accommodates clustered or correlated observations</li>",
                "</ul>",
                
                "<h4>Model Interpretation</h4>",
                "<p><strong>Coefficients (β):</strong> Log acceleration factors. A coefficient of β means:</p>",
                "<ul>",
                "<li>β > 0: Covariate accelerates failure (decreases survival time)</li>",
                "<li>β < 0: Covariate decelerates failure (increases survival time)</li>",
                "<li>β = 0: No effect on failure time</li>",
                "</ul>",
                
                "<p><strong>Acceleration Factors (exp(β)):</strong> Multiplicative effects on survival time:</p>",
                "<ul>",
                "<li>AF > 1: Survival time is multiplied by AF (if AF=2, survival time doubles)</li>",
                "<li>AF < 1: Survival time is reduced by factor AF (if AF=0.5, survival time halves)</li>",
                "<li>AF = 1: No effect on survival time</li>",
                "</ul>",
                
                "<h4>Rank Methods</h4>",
                "<p><strong>Log-rank:</strong> Gives equal weight to all failure times</p>",
                "<p><strong>Gehan:</strong> Weights observations by the number at risk (emphasizes early failures)</p>",
                
                "<h4>Correlation Structures</h4>",
                "<p><strong>Independence:</strong> Assumes uncorrelated observations</p>",
                "<p><strong>Exchangeable:</strong> Constant correlation within clusters</p>",
                "<p><strong>AR(1):</strong> First-order autoregressive correlation</p>",
                "<p><strong>Unstructured:</strong> General correlation matrix</p>",
                
                "<h4>When to Use AFT Models</h4>",
                "<ul>",
                "<li>Proportional hazards assumption is violated</li>",
                "<li>Interest in median survival time effects</li>",
                "<li>Clustered or longitudinal survival data</li>",
                "<li>Want robust, distribution-free estimates</li>",
                "</ul>"
            )
            
            html$setContent(content)
        }
    )
)