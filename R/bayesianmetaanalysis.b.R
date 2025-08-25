# This file is a generated template, your changes will not be overwritten

bayesianmetaanalysisClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "bayesianmetaanalysisClass",
    inherit = bayesianmetaanalysisBase,
    private = list(
        .init = function() {
            if (is.null(self$options$effectSize) || 
                is.null(self$options$standardError)) {
                return()
            }
            
            # Set default instructions
            self$results$instructions$setContent(
                "<html>
                <head>
                <style>
                .instructions {font-family: 'Segoe UI', Tahoma, sans-serif; color: #333;}
                .instructions h3 {color: #2c5282; margin-bottom: 10px;}
                .instructions p {margin: 5px 0;}
                .instructions ul {margin-left: 20px;}
                .instructions li {margin: 5px 0;}
                .note {background-color: #e7f3ff; padding: 10px; border-left: 3px solid #2196F3; margin: 10px 0;}
                </style>
                </head>
                <body>
                <div class='instructions'>
                <h3>Bayesian Meta-Analysis</h3>
                <p>This analysis performs Bayesian meta-analysis for evidence synthesis using hierarchical models.</p>
                <div class='note'>
                <strong>Getting Started:</strong>
                <ul>
                <li>Select the effect size variable (e.g., log odds ratio, mean difference)</li>
                <li>Select the standard error variable</li>
                <li>Optionally specify study identifiers and covariates for meta-regression</li>
                <li>Choose the model type and configure MCMC settings</li>
                </ul>
                </div>
                </div>
                </body>
                </html>"
            )
        },
        
        .run = function() {
            # Check if required variables are specified
            if (is.null(self$options$effectSize) || 
                is.null(self$options$standardError)) {
                return()
            }
            
            # Get the data
            data <- self$data
            
            # Extract variables
            effectSize <- jmvcore::toNumeric(data[[self$options$effectSize]])
            standardError <- jmvcore::toNumeric(data[[self$options$standardError]])
            
            # Check for missing values
            if (any(is.na(effectSize)) || any(is.na(standardError))) {
                jmvcore::reject("Missing values detected in effect size or standard error")
                return()
            }
            
            # Get study IDs if specified
            studyId <- NULL
            if (!is.null(self$options$studyId)) {
                studyId <- data[[self$options$studyId]]
            } else {
                studyId <- paste0("Study_", seq_len(length(effectSize)))
            }
            
            # Get covariates if specified
            covariates <- NULL
            if (length(self$options$covariates) > 0) {
                covariates <- data[self$options$covariates]
            }
            
            # Perform the Bayesian meta-analysis based on model type
            tryCatch({
                if (self$options$modelType == "fixed") {
                    results <- private$.runFixedEffectsModel(
                        effectSize, standardError, studyId, covariates
                    )
                } else if (self$options$modelType == "random") {
                    results <- private$.runRandomEffectsModel(
                        effectSize, standardError, studyId, covariates
                    )
                } else if (self$options$modelType == "hierarchical") {
                    results <- private$.runHierarchicalModel(
                        effectSize, standardError, studyId, covariates
                    )
                }
                
                # Populate results
                private$.populateModelSummary(results)
                private$.populateStudyEffects(results)
                private$.populateMCMCDiagnostics(results)
                
                if (!is.null(covariates)) {
                    private$.populateMetaRegression(results)
                }
                
                if (self$options$publicationBias) {
                    private$.assessPublicationBias(effectSize, standardError)
                }
                
                if (self$options$posteriorPredictive) {
                    private$.performPosteriorPredictiveCheck(results)
                }
                
                # Create plots if requested
                if (self$options$plotForest) {
                    private$.forestPlot(results)
                }
                
                if (self$options$plotPosterior) {
                    private$.posteriorPlot(results)
                }
                
            }, error = function(e) {
                jmvcore::reject(paste("Analysis failed:", e$message))
            })
        },
        
        .runFixedEffectsModel = function(effectSize, standardError, studyId, covariates) {
            # Check for required package
            if (!requireNamespace("metaBMA", quietly = TRUE)) {
                jmvcore::reject("Package 'metaBMA' is required for Bayesian meta-analysis. Please install it.")
                return(NULL)
            }
            
            # Prepare data for analysis
            weights <- 1 / (standardError^2)
            
            # Simple weighted average for fixed effects
            pooledEffect <- sum(effectSize * weights) / sum(weights)
            pooledSE <- sqrt(1 / sum(weights))
            
            # Calculate credible intervals
            ci_level <- self$options$credibleInterval / 100
            ci_lower <- pooledEffect - qnorm((1 + ci_level) / 2) * pooledSE
            ci_upper <- pooledEffect + qnorm((1 + ci_level) / 2) * pooledSE
            
            # Create results object
            results <- list(
                model_type = "Fixed Effects",
                pooled_effect = pooledEffect,
                pooled_se = pooledSE,
                ci_lower = ci_lower,
                ci_upper = ci_upper,
                tau2 = 0,  # No heterogeneity in fixed effects
                I2 = 0,
                Q_statistic = sum(weights * (effectSize - pooledEffect)^2),
                Q_df = length(effectSize) - 1,
                study_effects = data.frame(
                    study = studyId,
                    effect = effectSize,
                    se = standardError,
                    weight = weights / sum(weights) * 100
                ),
                n_studies = length(effectSize),
                n_observations = length(effectSize),
                DIC = NA,
                WAIC = NA,
                convergence = list(
                    Rhat = NA,
                    n_eff = NA,
                    converged = TRUE
                )
            )
            
            # Calculate Q test p-value
            results$Q_pvalue <- pchisq(results$Q_statistic, results$Q_df, lower.tail = FALSE)
            
            return(results)
        },
        
        .runRandomEffectsModel = function(effectSize, standardError, studyId, covariates) {
            # Try to use brms if available, otherwise fall back to metafor
            if (requireNamespace("brms", quietly = TRUE)) {
                return(private$.runBRMSModel(effectSize, standardError, studyId, covariates, "random"))
            } else if (requireNamespace("metafor", quietly = TRUE)) {
                return(private$.runMetaforModel(effectSize, standardError, studyId, covariates))
            } else {
                # Fallback to simple DerSimonian-Laird estimator
                return(private$.runSimpleRandomEffects(effectSize, standardError, studyId))
            }
        },
        
        .runHierarchicalModel = function(effectSize, standardError, studyId, covariates) {
            # Try to use brms for hierarchical model
            if (requireNamespace("brms", quietly = TRUE)) {
                return(private$.runBRMSModel(effectSize, standardError, studyId, covariates, "hierarchical"))
            } else {
                # Fall back to random effects if hierarchical not available
                return(private$.runRandomEffectsModel(effectSize, standardError, studyId, covariates))
            }
        },
        
        .runBRMSModel = function(effectSize, standardError, studyId, covariates, type) {
            # Prepare data for brms
            meta_data <- data.frame(
                yi = effectSize,
                sei = standardError,
                study = studyId
            )
            
            if (!is.null(covariates)) {
                meta_data <- cbind(meta_data, covariates)
            }
            
            # Set up priors based on user selection
            priors <- private$.setupPriors()
            
            # Build formula
            if (type == "hierarchical" && !is.null(covariates)) {
                formula_str <- paste0("yi | se(sei) ~ 1 + ", 
                                    paste(names(covariates), collapse = " + "),
                                    " + (1 | study)")
            } else if (!is.null(covariates)) {
                formula_str <- paste0("yi | se(sei) ~ 1 + ", 
                                    paste(names(covariates), collapse = " + "))
            } else {
                formula_str <- "yi | se(sei) ~ 1"
            }
            
            # Run the model
            model <- brms::brm(
                formula = as.formula(formula_str),
                data = meta_data,
                prior = priors,
                chains = self$options$mcmcChains,
                iter = self$options$mcmcIterations,
                warmup = self$options$warmupIterations,
                cores = parallel::detectCores() - 1,
                silent = 2,
                refresh = 0
            )
            
            # Extract results
            summary_model <- summary(model)
            posterior <- brms::posterior_samples(model)
            
            # Calculate pooled effect and credible interval
            ci_level <- self$options$credibleInterval / 100
            pooled_effect <- median(posterior$b_Intercept)
            ci_bounds <- quantile(posterior$b_Intercept, 
                                 probs = c((1 - ci_level) / 2, (1 + ci_level) / 2))
            
            # Calculate heterogeneity if random effects
            tau2 <- if ("sd_study__Intercept" %in% names(posterior)) {
                mean(posterior$sd_study__Intercept^2)
            } else {
                0
            }
            
            # Create results object
            results <- list(
                model_type = paste(toupper(substring(type, 1, 1)), 
                                 substring(type, 2), " Effects", sep = ""),
                pooled_effect = pooled_effect,
                pooled_se = sd(posterior$b_Intercept),
                ci_lower = ci_bounds[1],
                ci_upper = ci_bounds[2],
                tau2 = tau2,
                I2 = private$.calculateI2(effectSize, standardError, tau2),
                Q_statistic = NA,
                Q_df = NA,
                Q_pvalue = NA,
                study_effects = data.frame(
                    study = studyId,
                    effect = effectSize,
                    se = standardError,
                    weight = NA
                ),
                n_studies = length(unique(studyId)),
                n_observations = length(effectSize),
                DIC = NA,
                WAIC = brms::waic(model)$estimates["waic", "Estimate"],
                model = model,
                posterior = posterior,
                convergence = list(
                    Rhat = max(summary_model$fixed$Rhat, na.rm = TRUE),
                    n_eff = min(summary_model$fixed$Bulk_ESS, na.rm = TRUE),
                    converged = all(summary_model$fixed$Rhat < 1.01, na.rm = TRUE)
                )
            )
            
            # Add covariate effects if present
            if (!is.null(covariates)) {
                coef_names <- paste0("b_", names(covariates))
                results$covariate_effects <- data.frame(
                    covariate = names(covariates),
                    estimate = sapply(coef_names, function(x) median(posterior[[x]])),
                    se = sapply(coef_names, function(x) sd(posterior[[x]])),
                    ci_lower = sapply(coef_names, function(x) 
                        quantile(posterior[[x]], (1 - ci_level) / 2)),
                    ci_upper = sapply(coef_names, function(x) 
                        quantile(posterior[[x]], (1 + ci_level) / 2))
                )
            }
            
            return(results)
        },
        
        .runMetaforModel = function(effectSize, standardError, studyId, covariates) {
            # Use metafor for frequentist random effects as fallback
            if (!is.null(covariates)) {
                model <- metafor::rma(
                    yi = effectSize,
                    sei = standardError,
                    mods = ~ as.matrix(covariates),
                    method = "REML"
                )
            } else {
                model <- metafor::rma(
                    yi = effectSize,
                    sei = standardError,
                    method = "REML"
                )
            }
            
            # Convert to Bayesian-like results
            ci_level <- self$options$credibleInterval / 100
            z_crit <- qnorm((1 + ci_level) / 2)
            
            results <- list(
                model_type = "Random Effects (REML)",
                pooled_effect = model$beta[1],
                pooled_se = model$se[1],
                ci_lower = model$beta[1] - z_crit * model$se[1],
                ci_upper = model$beta[1] + z_crit * model$se[1],
                tau2 = model$tau2,
                I2 = model$I2,
                Q_statistic = model$QE,
                Q_df = model$k - model$p,
                Q_pvalue = model$QEp,
                study_effects = data.frame(
                    study = studyId,
                    effect = effectSize,
                    se = standardError,
                    weight = weights(model)
                ),
                n_studies = length(unique(studyId)),
                n_observations = model$k,
                DIC = NA,
                WAIC = NA,
                model = model,
                convergence = list(
                    Rhat = NA,
                    n_eff = NA,
                    converged = TRUE
                )
            )
            
            return(results)
        },
        
        .runSimpleRandomEffects = function(effectSize, standardError, studyId) {
            # DerSimonian-Laird estimator
            weights <- 1 / (standardError^2)
            pooled_fixed <- sum(effectSize * weights) / sum(weights)
            Q <- sum(weights * (effectSize - pooled_fixed)^2)
            df <- length(effectSize) - 1
            
            # Calculate tau2
            C <- sum(weights) - sum(weights^2) / sum(weights)
            tau2 <- max(0, (Q - df) / C)
            
            # Random effects weights
            weights_re <- 1 / (standardError^2 + tau2)
            pooled_random <- sum(effectSize * weights_re) / sum(weights_re)
            pooled_se <- sqrt(1 / sum(weights_re))
            
            # Calculate I2
            I2 <- max(0, (Q - df) / Q * 100)
            
            # Credible intervals
            ci_level <- self$options$credibleInterval / 100
            z_crit <- qnorm((1 + ci_level) / 2)
            
            results <- list(
                model_type = "Random Effects (DL)",
                pooled_effect = pooled_random,
                pooled_se = pooled_se,
                ci_lower = pooled_random - z_crit * pooled_se,
                ci_upper = pooled_random + z_crit * pooled_se,
                tau2 = tau2,
                I2 = I2,
                Q_statistic = Q,
                Q_df = df,
                Q_pvalue = pchisq(Q, df, lower.tail = FALSE),
                study_effects = data.frame(
                    study = studyId,
                    effect = effectSize,
                    se = standardError,
                    weight = weights_re / sum(weights_re) * 100
                ),
                n_studies = length(unique(studyId)),
                n_observations = length(effectSize),
                DIC = NA,
                WAIC = NA,
                convergence = list(
                    Rhat = NA,
                    n_eff = NA,
                    converged = TRUE
                )
            )
            
            return(results)
        },
        
        .setupPriors = function() {
            # Set up priors based on user selection
            if (self$options$priorType == "noninformative") {
                priors <- brms::prior(normal(0, 100), class = Intercept)
            } else if (self$options$priorType == "weakly_informative") {
                priors <- brms::prior(normal(0, 1), class = Intercept)
            } else if (self$options$priorType == "informative") {
                priors <- brms::prior(normal(0, 0.5), class = Intercept)
            } else {  # empirical_bayes
                priors <- NULL  # Let brms use default priors
            }
            return(priors)
        },
        
        .calculateI2 = function(effectSize, standardError, tau2) {
            # Calculate I2 statistic
            weights <- 1 / (standardError^2)
            pooled <- sum(effectSize * weights) / sum(weights)
            Q <- sum(weights * (effectSize - pooled)^2)
            df <- length(effectSize) - 1
            I2 <- max(0, (Q - df) / Q * 100)
            return(I2)
        },
        
        .populateModelSummary = function(results) {
            if (is.null(results)) return()
            
            table <- self$results$modelSummary
            
            table$setRow(rowNo = 1, values = list(
                model = results$model_type,
                pooledEffect = results$pooled_effect,
                se = results$pooled_se,
                ciLower = results$ci_lower,
                ciUpper = results$ci_upper,
                tau2 = results$tau2,
                I2 = results$I2,
                Q = results$Q_statistic,
                Qp = results$Q_pvalue,
                nStudies = results$n_studies
            ))
        },
        
        .populateStudyEffects = function(results) {
            if (is.null(results)) return()
            
            table <- self$results$studyEffects
            
            for (i in seq_len(nrow(results$study_effects))) {
                table$addRow(rowKey = i, values = list(
                    study = results$study_effects$study[i],
                    effect = results$study_effects$effect[i],
                    se = results$study_effects$se[i],
                    weight = results$study_effects$weight[i],
                    ciLower = results$study_effects$effect[i] - 
                              qnorm((1 + self$options$credibleInterval/100) / 2) * 
                              results$study_effects$se[i],
                    ciUpper = results$study_effects$effect[i] + 
                              qnorm((1 + self$options$credibleInterval/100) / 2) * 
                              results$study_effects$se[i]
                ))
            }
        },
        
        .populateMCMCDiagnostics = function(results) {
            if (is.null(results)) return()
            
            table <- self$results$mcmcDiagnostics
            
            table$setRow(rowNo = 1, values = list(
                parameter = "Pooled Effect",
                Rhat = results$convergence$Rhat,
                nEff = results$convergence$n_eff,
                converged = results$convergence$converged
            ))
            
            # Add model fit statistics
            if (!is.null(results$model)) {
                table <- self$results$modelComparison
                table$setRow(rowNo = 1, values = list(
                    model = results$model_type,
                    DIC = results$DIC,
                    WAIC = results$WAIC,
                    effectiveParams = NA
                ))
            }
        },
        
        .populateMetaRegression = function(results) {
            if (is.null(results$covariate_effects)) return()
            
            # Populate meta-regression results
            # This would need to be added to the results definition
        },
        
        .assessPublicationBias = function(effectSize, standardError) {
            table <- self$results$publicationBias
            
            # Simple Egger's test
            precision <- 1 / standardError
            model <- lm(effectSize ~ precision, weights = 1/standardError^2)
            
            table$setRow(rowNo = 1, values = list(
                test = "Egger's Test",
                statistic = coef(model)[1] / summary(model)$coefficients[1, 2],
                p = summary(model)$coefficients[1, 4]
            ))
        },
        
        .performPosteriorPredictiveCheck = function(results) {
            # Perform posterior predictive checks if model available
            if (!is.null(results$model) && requireNamespace("brms", quietly = TRUE)) {
                # This would involve generating replicated data and comparing to observed
                # Implementation would depend on specific requirements
            }
        },
        
        .forestPlot = function(results) {
            if (is.null(results)) return()
            
            plot <- self$results$forestPlot
            
            # Prepare data for forest plot
            plotData <- results$study_effects
            plotData$lower <- plotData$effect - 
                            qnorm((1 + self$options$credibleInterval/100) / 2) * plotData$se
            plotData$upper <- plotData$effect + 
                            qnorm((1 + self$options$credibleInterval/100) / 2) * plotData$se
            
            # Add pooled effect
            pooledData <- data.frame(
                study = "Pooled Effect",
                effect = results$pooled_effect,
                se = results$pooled_se,
                weight = NA,
                lower = results$ci_lower,
                upper = results$ci_upper
            )
            
            plotData <- rbind(plotData, pooledData)
            
            # Create the plot
            image <- jmvcore::Image$new(
                options = self$options,
                name = "forestPlot",
                renderFun = ".forestPlotRender",
                clearWith = list("effectSize", "standardError"),
                width = 600,
                height = 400 + nrow(plotData) * 20
            )
            
            # Store data for rendering
            image$setState(plotData)
            plot$setContent(image)
        },
        
        .forestPlotRender = function(image, ...) {
            if (!requireNamespace("ggplot2", quietly = TRUE))
                return(FALSE)
            
            plotData <- image$state
            if (is.null(plotData))
                return(FALSE)
            
            # Create forest plot
            p <- ggplot2::ggplot(plotData, ggplot2::aes(y = factor(study, levels = rev(study)))) +
                ggplot2::geom_point(ggplot2::aes(x = effect), size = 3) +
                ggplot2::geom_errorbarh(ggplot2::aes(xmin = lower, xmax = upper), height = 0.2) +
                ggplot2::geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.5) +
                ggplot2::labs(
                    x = "Effect Size",
                    y = "Study",
                    title = "Forest Plot"
                ) +
                ggplot2::theme_minimal() +
                ggplot2::theme(
                    panel.grid.major.y = ggplot2::element_blank(),
                    panel.grid.minor = ggplot2::element_blank()
                )
            
            # Highlight pooled effect
            if ("Pooled Effect" %in% plotData$study) {
                p <- p + ggplot2::geom_point(
                    data = plotData[plotData$study == "Pooled Effect", ],
                    ggplot2::aes(x = effect),
                    shape = 18,
                    size = 4,
                    color = "red"
                )
            }
            
            print(p)
            return(TRUE)
        },
        
        .posteriorPlot = function(results) {
            if (is.null(results)) return()
            
            plot <- self$results$posteriorPlot
            
            # Create the plot
            image <- jmvcore::Image$new(
                options = self$options,
                name = "posteriorPlot",
                renderFun = ".posteriorPlotRender",
                clearWith = list("effectSize", "standardError"),
                width = 600,
                height = 400
            )
            
            # Store results for rendering
            image$setState(results)
            plot$setContent(image)
        },
        
        .posteriorPlotRender = function(image, ...) {
            if (!requireNamespace("ggplot2", quietly = TRUE))
                return(FALSE)
            
            results <- image$state
            if (is.null(results))
                return(FALSE)
            
            # Create posterior distribution plot
            if (!is.null(results$posterior) && "b_Intercept" %in% names(results$posterior)) {
                # Use actual posterior samples if available
                plotData <- data.frame(effect = results$posterior$b_Intercept)
            } else {
                # Simulate from normal distribution as approximation
                plotData <- data.frame(
                    effect = rnorm(4000, 
                                 mean = results$pooled_effect,
                                 sd = results$pooled_se)
                )
            }
            
            ci_level <- self$options$credibleInterval / 100
            ci_bounds <- quantile(plotData$effect, 
                                probs = c((1 - ci_level) / 2, (1 + ci_level) / 2))
            
            p <- ggplot2::ggplot(plotData, ggplot2::aes(x = effect)) +
                ggplot2::geom_histogram(
                    ggplot2::aes(y = ggplot2::after_stat(density)),
                    bins = 50,
                    fill = "skyblue",
                    color = "black",
                    alpha = 0.7
                ) +
                ggplot2::geom_density(color = "darkblue", size = 1) +
                ggplot2::geom_vline(
                    xintercept = results$pooled_effect,
                    color = "red",
                    linetype = "solid",
                    size = 1
                ) +
                ggplot2::geom_vline(
                    xintercept = ci_bounds,
                    color = "red",
                    linetype = "dashed",
                    alpha = 0.7
                ) +
                ggplot2::labs(
                    x = "Effect Size",
                    y = "Density",
                    title = "Posterior Distribution",
                    subtitle = paste0(self$options$credibleInterval, "% Credible Interval: [",
                                    round(ci_bounds[1], 3), ", ",
                                    round(ci_bounds[2], 3), "]")
                ) +
                ggplot2::theme_minimal()
            
            print(p)
            return(TRUE)
        }
    )
)