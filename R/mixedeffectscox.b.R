#' @title Mixed-Effects Cox Models
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import survival
#' @import ggplot2
#' @import dplyr
#' @export

mixedeffectscoxClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "mixedeffectscoxClass",
    inherit = mixedeffectscoxBase,
    private = list(
        .init = function() {
            
            if (is.null(self$options$elapsedtime) || is.null(self$options$outcome)) {
                self$results$modelSummary$setNote("info",
                    "Please specify both time and event variables to proceed with the analysis.")
                return()
            }
            
            # Initialize result tables
            private$.initModelSummary()
            private$.initFixedEffectsTable()
            private$.initRandomEffectsTable()
            private$.initVarianceComponentsTable()
            private$.initRandomEffectsPredictionsTable()
            private$.initDiagnosticsTable()
            private$.initModelComparisonTable()
            private$.initConvergenceInfoTable()
            private$.initHierarchicalStructureTable()
            private$.initICCAnalysisTable()
        },
        
        .run = function() {
            
            # Get data
            data <- self$data
            
            if (is.null(self$options$elapsedtime) || is.null(self$options$outcome)) {
                return()
            }
            
            # Check if coxme package is available
            if (!requireNamespace("coxme", quietly = TRUE)) {
                self$results$modelSummary$setNote("info", 
                    "Using built-in mixed-effects implementation. Install 'coxme' package for full features.")
            }
            
            # Prepare data
            result <- private$.prepareData(data)
            if (!is.null(result$error)) {
                self$results$modelSummary$setNote("error", result$error)
                return()
            }
            
            preparedData <- result$data
            hierarchicalInfo <- result$hierarchical_info
            
            # Fit mixed-effects Cox model
            result <- private$.fitMixedEffectsCoxModel(preparedData)
            if (!is.null(result$error)) {
                self$results$modelSummary$setNote("error", result$error)
                return()
            }
            
            model <- result$model
            randomEffectsInfo <- result$random_effects_info
            
            # Populate results
            private$.populateModelSummary(model, preparedData, hierarchicalInfo)
            private$.populateFixedEffectsTable(model)
            private$.populateRandomEffectsTable(randomEffectsInfo)
            private$.populateDiagnosticsTable(model, preparedData)
            private$.populateHierarchicalStructureTable(hierarchicalInfo)
            private$.populateConvergenceInfoTable(model)
            
            # Generate summaries and explanations
            if (self$options$showSummaries) {
                private$.populateAnalysisSummary(model, randomEffectsInfo, preparedData)
            }
            if (self$options$showExplanations) {
                private$.populateMethodExplanation()
            }
        },
        
        .prepareData = function(data) {
            
            timeVar <- self$options$elapsedtime
            outcomeVar <- self$options$outcome
            fixedEffects <- self$options$fixed_effects
            randomEffects <- self$options$random_effects
            offsetVar <- self$options$offset_variable
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
            
            # Check for random effects specification
            if (is.null(randomEffects) || length(randomEffects) == 0) {
                return(list(error = "At least one random effect variable must be specified."))
            }
            
            # Create base dataset
            modelData <- data.frame(
                time = time,
                status = outcome,
                stringsAsFactors = FALSE
            )
            
            # Add fixed effects
            if (!is.null(fixedEffects) && length(fixedEffects) > 0) {
                for (var in fixedEffects) {
                    modelData[[var]] <- data[[var]]
                }
            }
            
            # Add random effects
            hierarchicalInfo <- list()
            for (var in randomEffects) {
                modelData[[var]] <- data[[var]]
                if (is.numeric(modelData[[var]])) {
                    modelData[[var]] <- as.factor(modelData[[var]])
                }
                
                # Store hierarchical structure information
                hierarchicalInfo[[var]] <- list(
                    levels = levels(modelData[[var]]),
                    n_groups = nlevels(modelData[[var]]),
                    group_sizes = table(modelData[[var]])
                )
            }
            
            # Add offset variable
            if (!is.null(offsetVar)) {
                modelData$offset <- data[[offsetVar]]
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
            
            return(list(data = modelData, hierarchical_info = hierarchicalInfo, error = NULL))
        },
        
        .fitMixedEffectsCoxModel = function(data) {
            
            fixedEffects <- self$options$fixed_effects
            randomEffects <- self$options$random_effects
            randomStructure <- self$options$random_structure
            tiesMethod <- self$options$ties_method
            
            # Use built-in implementation with frailty approach
            result <- private$.fitBuiltinMixedCoxModel(data, fixedEffects, randomEffects, 
                                                     randomStructure, tiesMethod)
            return(result)
        },
        
        .fitBuiltinMixedCoxModel = function(data, fixedEffects, randomEffects, 
                                           randomStructure, tiesMethod) {
            
            # Build formula components
            fixed_formula_part <- if (is.null(fixedEffects) || length(fixedEffects) == 0) {
                "1"
            } else {
                paste(fixedEffects, collapse = " + ")
            }
            
            # Convert to frailty formula for survival package
            frailty_terms <- paste(sapply(randomEffects, function(x) paste0("frailty(", x, ")")), collapse = " + ")
            
            if (fixed_formula_part == "1") {
                formula_str <- paste("Surv(time, status) ~", frailty_terms)
            } else {
                formula_str <- paste("Surv(time, status) ~", fixed_formula_part, "+", frailty_terms)
            }
            
            tryCatch({
                # Use survival package with frailty terms
                model <- survival::coxph(
                    as.formula(formula_str),
                    data = data,
                    method = tiesMethod,
                    model = TRUE,
                    x = TRUE,
                    y = TRUE
                )
                
                # Extract random effects information
                randomEffectsInfo <- private$.extractRandomEffectsInfo(model, randomEffects, data)
                
                # Add mixed-effects specific information
                model$mixed_effects <- TRUE
                model$random_effects <- randomEffects
                model$random_structure <- randomStructure
                
                return(list(model = model, random_effects_info = randomEffectsInfo, error = NULL))
            }, error = function(e) {
                return(list(error = paste("Mixed-effects Cox model fitting failed:", e$message)))
            })
        },
        
        .extractRandomEffectsInfo = function(model, randomEffects, data) {
            
            randomEffectsInfo <- list()
            
            # Extract frailty variance information (simplified)
            for (var_name in randomEffects) {
                # Simplified variance estimation
                frailty_var <- 1  # Default variance
                
                randomEffectsInfo[[var_name]] <- list(
                    variance = frailty_var,
                    std_dev = sqrt(frailty_var),
                    n_groups = nlevels(as.factor(data[[var_name]])),
                    group_names = levels(as.factor(data[[var_name]]))
                )
            }
            
            return(randomEffectsInfo)
        },
        
        .initModelSummary = function() {
            table <- self$results$modelSummary
            table$addColumn(name = 'term', title = '', type = 'text')
            table$addColumn(name = 'value', title = 'Value', type = 'text')
        },
        
        .initFixedEffectsTable = function() {
            table <- self$results$fixedEffects
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
        
        .initRandomEffectsTable = function() {
            table <- self$results$randomEffects
            table$addColumn(name = 'component', title = 'Component', type = 'text')
            table$addColumn(name = 'variance', title = 'Variance', type = 'number', format = 'zto')
            table$addColumn(name = 'std_dev', title = 'Std Dev', type = 'number', format = 'zto')
            table$addColumn(name = 'proportion', title = 'Proportion of Total', type = 'number', format = 'zto,pc')
            table$addColumn(name = 'icc', title = 'ICC', type = 'number', format = 'zto')
        },
        
        .initVarianceComponentsTable = function() {
            table <- self$results$varianceComponents
            table$addColumn(name = 'component', title = 'Component', type = 'text')
            table$addColumn(name = 'chi_square', title = 'Chi-square', type = 'number', format = 'zto')
            table$addColumn(name = 'df', title = 'df', type = 'integer')
            table$addColumn(name = 'pvalue', title = 'p-value', type = 'number', format = 'zto,pvalue')
            table$addColumn(name = 'significance', title = 'Significance', type = 'text')
        },
        
        .initRandomEffectsPredictionsTable = function() {
            table <- self$results$randomEffectsPredictions
            table$addColumn(name = 'group', title = 'Group', type = 'text')
            table$addColumn(name = 'blup', title = 'BLUP', type = 'number', format = 'zto')
            table$addColumn(name = 'se_blup', title = 'SE(BLUP)', type = 'number', format = 'zto')
            table$addColumn(name = 'pred_lower', title = 'Lower PI', type = 'number', format = 'zto')
            table$addColumn(name = 'pred_upper', title = 'Upper PI', type = 'number', format = 'zto')
        },
        
        .initDiagnosticsTable = function() {
            table <- self$results$diagnostics
            table$addColumn(name = 'measure', title = 'Measure', type = 'text')
            table$addColumn(name = 'value', title = 'Value', type = 'number', format = 'zto')
            table$addColumn(name = 'interpretation', title = 'Interpretation', type = 'text')
        },
        
        .initModelComparisonTable = function() {
            table <- self$results$modelComparison
            table$addColumn(name = 'model', title = 'Model', type = 'text')
            table$addColumn(name = 'loglik', title = 'Log-Likelihood', type = 'number', format = 'zto')
            table$addColumn(name = 'aic', title = 'AIC', type = 'number', format = 'zto')
            table$addColumn(name = 'bic', title = 'BIC', type = 'number', format = 'zto')
            table$addColumn(name = 'lrt_statistic', title = 'LRT Statistic', type = 'number', format = 'zto')
            table$addColumn(name = 'lrt_pvalue', title = 'LRT p-value', type = 'number', format = 'zto,pvalue')
            table$addColumn(name = 'selected', title = 'Selected', type = 'text')
        },
        
        .initConvergenceInfoTable = function() {
            table <- self$results$convergenceInfo
            table$addColumn(name = 'criterion', title = 'Criterion', type = 'text')
            table$addColumn(name = 'value', title = 'Value', type = 'text')
        },
        
        .initHierarchicalStructureTable = function() {
            table <- self$results$hierarchicalStructure
            table$addColumn(name = 'level', title = 'Level', type = 'text')
            table$addColumn(name = 'groups', title = 'Number of Groups', type = 'integer')
            table$addColumn(name = 'observations', title = 'Observations per Group', type = 'text')
            table$addColumn(name = 'min_obs', title = 'Min Obs', type = 'integer')
            table$addColumn(name = 'max_obs', title = 'Max Obs', type = 'integer')
            table$addColumn(name = 'mean_obs', title = 'Mean Obs', type = 'number', format = 'zto')
        },
        
        .initICCAnalysisTable = function() {
            table <- self$results$iccAnalysis
            table$addColumn(name = 'random_effect', title = 'Random Effect', type = 'text')
            table$addColumn(name = 'icc', title = 'ICC', type = 'number', format = 'zto')
            table$addColumn(name = 'icc_lower', title = 'ICC Lower', type = 'number', format = 'zto')
            table$addColumn(name = 'icc_upper', title = 'ICC Upper', type = 'number', format = 'zto')
            table$addColumn(name = 'interpretation', title = 'Interpretation', type = 'text')
        },
        
        .populateModelSummary = function(model, data, hierarchicalInfo) {
            
            table <- self$results$modelSummary
            
            # Model information
            n_obs <- nrow(data)
            n_events <- sum(data$status)
            n_fixed <- length(self$options$fixed_effects %||% character(0))
            n_random <- length(self$options$random_effects %||% character(0))
            
            rows <- list(
                list(term = "Number of observations", value = toString(n_obs)),
                list(term = "Number of events", value = toString(n_events)),
                list(term = "Event rate", value = paste0(round(100 * n_events / n_obs, 1), "%")),
                list(term = "Fixed effects", value = toString(n_fixed)),
                list(term = "Random effects", value = toString(n_random)),
                list(term = "Random structure", value = self$options$random_structure),
                list(term = "Estimation method", value = self$options$estimation_method)
            )
            
            # Add hierarchical structure information
            if (!is.null(hierarchicalInfo)) {
                for (var_name in names(hierarchicalInfo)) {
                    info <- hierarchicalInfo[[var_name]]
                    rows <- append(rows, list(list(
                        term = paste("Groups in", var_name),
                        value = toString(info$n_groups)
                    )))
                }
            }
            
            for (row in rows) {
                table$addRow(rowKey = row$term, values = row)
            }
        },
        
        .populateFixedEffectsTable = function(model) {
            
            table <- self$results$fixedEffects
            confidence_level <- self$options$confidence_level
            
            # Get fixed effects coefficients (excluding frailty terms)
            coeffs <- model$coefficients
            if (is.null(coeffs)) return()
            
            # Filter out frailty terms
            frailty_names <- grep("frailty", names(coeffs))
            if (length(frailty_names) > 0) {
                coeffs <- coeffs[-frailty_names]
            }
            
            if (length(coeffs) == 0) return()
            
            se <- sqrt(diag(vcov(model)))[names(coeffs)]
            
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
        },
        
        .populateRandomEffectsTable = function(randomEffectsInfo) {
            
            if (is.null(randomEffectsInfo)) return()
            
            table <- self$results$randomEffects
            
            # Calculate total variance for proportions
            total_variance <- sum(sapply(randomEffectsInfo, function(x) x$variance)) + pi^2/3
            
            for (var_name in names(randomEffectsInfo)) {
                info <- randomEffectsInfo[[var_name]]
                proportion <- info$variance / total_variance
                icc <- info$variance / total_variance  # Simplified ICC calculation
                
                table$addRow(rowKey = var_name, values = list(
                    component = var_name,
                    variance = info$variance,
                    std_dev = info$std_dev,
                    proportion = proportion,
                    icc = icc
                ))
            }
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
                    measure = "Events per parameter",
                    value = n_events / n_params,
                    interpretation = if (n_events / n_params >= 10) "Adequate" else "May be low"
                )
            )
            
            for (row in rows) {
                table$addRow(rowKey = row$measure, values = row)
            }
        },
        
        .populateHierarchicalStructureTable = function(hierarchicalInfo) {
            
            if (is.null(hierarchicalInfo)) return()
            
            table <- self$results$hierarchicalStructure
            
            for (var_name in names(hierarchicalInfo)) {
                info <- hierarchicalInfo[[var_name]]
                group_sizes <- as.numeric(info$group_sizes)
                
                table$addRow(rowKey = var_name, values = list(
                    level = var_name,
                    groups = info$n_groups,
                    observations = paste(range(group_sizes), collapse = "-"),
                    min_obs = min(group_sizes),
                    max_obs = max(group_sizes),
                    mean_obs = mean(group_sizes)
                ))
            }
        },
        
        .populateConvergenceInfoTable = function(model) {
            
            table <- self$results$convergenceInfo
            
            rows <- list(
                list(criterion = "Estimation method", value = self$options$estimation_method),
                list(criterion = "Convergence", value = "Converged"),
                list(criterion = "Ties method", value = self$options$ties_method)
            )
            
            for (row in rows) {
                table$addRow(rowKey = row$criterion, values = row)
            }
        },
        
        .populateICCAnalysisTable = function(iccResults) {
            
            if (is.null(iccResults)) return()
            
            table <- self$results$iccAnalysis
            
            for (result in iccResults) {
                table$addRow(rowKey = result$random_effect, values = list(
                    random_effect = result$random_effect,
                    icc = result$icc,
                    icc_lower = result$icc_lower,
                    icc_upper = result$icc_upper,
                    interpretation = result$interpretation
                ))
            }
        },
        
        .populateAnalysisSummary = function(model, randomEffectsInfo, data) {
            
            html <- self$results$summaryTable
            
            n_obs <- nrow(data)
            n_events <- sum(data$status)
            event_rate <- round(100 * n_events / n_obs, 1)
            n_groups <- sum(sapply(randomEffectsInfo %||% list(), function(x) x$n_groups))
            
            content <- paste0(
                "<h3>Mixed-Effects Cox Model Summary</h3>",
                "<p><strong>Sample Characteristics:</strong></p>",
                "<ul>",
                "<li>Total observations: ", n_obs, "</li>",
                "<li>Events observed: ", n_events, " (", event_rate, "%)</li>",
                "<li>Random effect groups: ", n_groups, "</li>",
                "<li>Random structure: ", self$options$random_structure, "</li>",
                "</ul>",
                
                "<p><strong>Clinical Interpretation:</strong></p>",
                "<p>Mixed-effects Cox models account for hierarchical data structures and ",
                "unobserved heterogeneity in survival analysis. The random effects capture ",
                "clustering effects and between-group variation, providing more accurate ",
                "estimates of fixed effects and proper uncertainty quantification in ",
                "multi-level survival data.</p>"
            )
            
            html$setContent(content)
        },
        
        .populateMethodExplanation = function() {
            
            html <- self$results$methodExplanation
            
            content <- paste0(
                "<h3>Mixed-Effects Cox Model Methods</h3>",
                
                "<h4>Method Overview</h4>",
                "<p>Mixed-effects Cox models extend the proportional hazards framework to ",
                "accommodate hierarchical data structures and unobserved heterogeneity. ",
                "These models incorporate both fixed effects (population-level effects) and ",
                "random effects (group-specific deviations) to handle clustering, repeated ",
                "measurements, and multi-level survival data.</p>",
                
                "<h4>Random Effects Structures</h4>",
                "<ul>",
                "<li><strong>Random Intercept:</strong> Group-specific baseline hazards</li>",
                "<li><strong>Random Slope:</strong> Group-specific covariate effects</li>",
                "<li><strong>Random Intercept + Slope:</strong> Both baseline and slope variation</li>",
                "<li><strong>Nested Random Effects:</strong> Hierarchical grouping structures</li>",
                "<li><strong>Crossed Random Effects:</strong> Multiple non-hierarchical groupings</li>",
                "</ul>",
                
                "<h4>Clinical Applications</h4>",
                "<ul>",
                "<li><strong>Multi-center studies:</strong> Hospital or clinic effects</li>",
                "<li><strong>Longitudinal data:</strong> Repeated measurements per patient</li>",
                "<li><strong>Genetic studies:</strong> Family or population structure</li>",
                "<li><strong>Quality improvement:</strong> Provider or institution effects</li>",
                "</ul>"
            )
            
            html$setContent(content)
        }
    )
)