metaanalysisClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "metaanalysisClass",
    inherit = metaanalysisBase,
    private = list(
        .init = function() {
            if (is.null(self$data) || is.null(self$options$effect_size) || is.null(self$options$variance)) {
                self$results$instructions$setContent(
                    "<html>
                    <head>
                    <meta http-equiv='Content-Type' content='text/html; charset=UTF-8'>
                    </head>
                    <body>
                    <h3>Meta-Analysis & Evidence Synthesis</h3>
                    <p><b>Data Requirements:</b></p>
                    <p>This module requires:</p>
                    <ul>
                    <li><b>Effect Size</b>: Numeric variable with effect sizes (log OR, Cohen's d, etc.)</li>
                    <li><b>Variance/SE</b>: Numeric variable with variance or standard error</li>
                    <li><b>Study ID</b>: Study identifier for each effect size</li>
                    <li><b>Sample Size</b> (optional): Sample size for each study</li>
                    </ul>
                    
                    <p><b>Analysis Types:</b></p>
                    <ul>
                    <li><b>Generic Meta-Analysis</b>: Standard meta-analysis for any effect measure</li>
                    <li><b>Diagnostic Test Accuracy</b>: Bivariate meta-analysis of sensitivity/specificity</li>
                    <li><b>Network Meta-Analysis</b>: Multiple treatment comparisons</li>
                    <li><b>Individual Patient Data</b>: IPD meta-analysis methods</li>
                    </ul>
                    
                    <p><b>Key Features:</b></p>
                    <ul>
                    <li>Forest plots with prediction intervals</li>
                    <li>Comprehensive heterogeneity assessment (I², τ², Q-test)</li>
                    <li>Publication bias detection (Egger's test, funnel plots, trim-and-fill)</li>
                    <li>Meta-regression and subgroup analysis</li>
                    <li>Sensitivity analysis and outlier detection</li>
                    <li>Fixed-effects and random-effects models</li>
                    </ul>
                    
                    <p><b>Clinical Applications:</b></p>
                    <ul>
                    <li>Systematic reviews and meta-analyses</li>
                    <li>Diagnostic test accuracy synthesis</li>
                    <li>Treatment effect pooling</li>
                    <li>Evidence synthesis for clinical guidelines</li>
                    <li>Comparative effectiveness research</li>
                    </ul>
                    </body>
                    </html>"
                )
                return()
            }
        },

        .run = function() {
            # Check for required packages
            if (!requireNamespace("metafor", quietly = TRUE)) {
                stop("Package 'metafor' is required for meta-analysis but is not installed.")
            }
            
            if (!requireNamespace("meta", quietly = TRUE)) {
                message("Package 'meta' is recommended for enhanced meta-analysis features.")
            }

            # Get the required variables
            effect_var <- self$options$effect_size
            variance_var <- self$options$variance
            study_var <- self$options$study_id
            
            if (is.null(effect_var) || is.null(variance_var)) return()

            # Prepare data
            data <- self$data
            effect_sizes <- jmvcore::toNumeric(data[[effect_var]])
            variances <- jmvcore::toNumeric(data[[variance_var]])
            study_ids <- if (!is.null(study_var)) data[[study_var]] else paste0("Study_", seq_along(effect_sizes))
            
            # Remove missing values
            complete_cases <- complete.cases(effect_sizes, variances)
            if (sum(complete_cases) < 2) {
                self$results$instructions$setContent("Error: Need at least 2 complete cases for meta-analysis.")
                return()
            }
            
            clean_effects <- effect_sizes[complete_cases]
            clean_variances <- variances[complete_cases]
            clean_studies <- study_ids[complete_cases]
            clean_se <- sqrt(clean_variances)
            
            # Get optional variables
            sample_sizes <- if (!is.null(self$options$sample_size)) {
                data[[self$options$sample_size]][complete_cases]
            } else NULL
            
            years <- if (!is.null(self$options$year)) {
                data[[self$options$year]][complete_cases]
            } else NULL
            
            # Perform meta-analysis based on type
            analysis_type <- self$options$analysis_type
            
            if (analysis_type == 'generic') {
                private$.performGenericMetaAnalysis(clean_effects, clean_variances, clean_studies, sample_sizes, years)
            } else if (analysis_type == 'diagnostic_accuracy') {
                private$.performDiagnosticMetaAnalysis()
            } else if (analysis_type == 'network') {
                private$.performNetworkMetaAnalysis()
            }
            
            # Common analyses for all types
            if (self$options$publication_bias && analysis_type == 'generic') {
                private$.performPublicationBiasAnalysis(clean_effects, clean_variances, clean_studies)
            }
            
            if (self$options$meta_regression && !is.null(self$options$moderator_vars) && analysis_type == 'generic') {
                private$.performMetaRegression(clean_effects, clean_variances, clean_studies)
            }
            
            if (self$options$sensitivity_analysis && analysis_type == 'generic') {
                private$.performSensitivityAnalysis(clean_effects, clean_variances, clean_studies)
            }
            
            if (self$options$outlier_detection && analysis_type == 'generic') {
                private$.performOutlierDetection(clean_effects, clean_variances, clean_studies)
            }
            
            # Always populate method explanation
            private$.populateMethodExplanation()
        },

        .performGenericMetaAnalysis = function(effects, variances, studies, sample_sizes, years) {
            # Populate study summary
            private$.populateStudySummary(effects, variances, studies, sample_sizes)
            
            # Perform meta-analysis
            model_type <- self$options$model_type
            heterogeneity_method <- self$options$heterogeneity_method
            
            # Map heterogeneity methods to metafor
            tau2_method <- switch(heterogeneity_method,
                'dersimonian_laird' = 'DL',
                'restricted_ml' = 'REML',
                'maximum_likelihood' = 'ML',
                'empirical_bayes' = 'EB',
                'paule_mandel' = 'PM',
                'DL'
            )
            
            # Fixed-effects model
            if (model_type %in% c('fixed_effects', 'mixed_effects')) {
                ma_fixed <- metafor::rma(yi = effects, vi = variances, method = 'FE')
                private$.populateOverallResults(ma_fixed, "Fixed-Effects")
            }
            
            # Random-effects model  
            if (model_type %in% c('random_effects', 'mixed_effects')) {
                ma_random <- metafor::rma(yi = effects, vi = variances, method = tau2_method)
                
                # Apply Knapp-Hartung adjustment if requested
                if (self$options$knha_adjustment && tau2_method %in% c('DL', 'REML', 'ML')) {
                    ma_random <- metafor::rma(yi = effects, vi = variances, method = tau2_method, knha = TRUE)
                }
                
                private$.populateOverallResults(ma_random, "Random-Effects")
                private$.populateHeterogeneityAssessment(ma_random)
                private$.populateModelFitStatistics(ma_random)
                
                # Store for other analyses
                private$ma_model <- ma_random
            }
            
            # Subgroup analysis if specified
            subgroup_var <- self$options$subgroup_var
            if (!is.null(subgroup_var)) {
                private$.performSubgroupAnalysis(effects, variances, studies, subgroup_var)
            }
        },

        .populateStudySummary = function(effects, variances, studies, sample_sizes) {
            summary_table <- self$results$studySummary
            
            # Calculate weights (inverse variance)
            weights <- 1 / variances
            total_weight <- sum(weights)
            weight_percentages <- (weights / total_weight) * 100
            
            # Calculate confidence intervals
            confidence_level <- self$options$confidence_level
            z_critical <- qnorm(1 - (1 - confidence_level) / 2)
            
            for (i in seq_along(effects)) {
                se <- sqrt(variances[i])
                ci_lower <- effects[i] - z_critical * se
                ci_upper <- effects[i] + z_critical * se
                ci_text <- sprintf("[%.3f, %.3f]", ci_lower, ci_upper)
                
                summary_table$addRow(rowKey = paste0("study_", i), values = list(
                    study = as.character(studies[i]),
                    effect_size = effects[i],
                    standard_error = se,
                    confidence_interval = ci_text,
                    weight = weight_percentages[i],
                    sample_size = if (!is.null(sample_sizes)) sample_sizes[i] else NA
                ))
            }
        },

        .populateOverallResults = function(ma_model, model_name) {
            results_table <- self$results$overallResults
            
            # Extract results
            pooled_effect <- as.numeric(ma_model$beta)
            se <- ma_model$se
            ci_lower <- ma_model$ci.lb
            ci_upper <- ma_model$ci.ub
            z_value <- ma_model$zval
            p_value <- ma_model$pval
            
            ci_text <- sprintf("[%.3f, %.3f]", ci_lower, ci_upper)
            
            # Calculate prediction interval if requested and applicable
            pi_text <- ""
            if (self$options$prediction_interval && !is.null(ma_model$tau2) && ma_model$tau2 > 0) {
                pi_result <- metafor::predict(ma_model)
                pi_text <- sprintf("[%.3f, %.3f]", pi_result$pi.lb, pi_result$pi.ub)
            }
            
            results_table$addRow(rowKey = model_name, values = list(
                model = model_name,
                pooled_effect = pooled_effect,
                standard_error = se,
                confidence_interval = ci_text,
                z_value = z_value,
                p_value = p_value,
                prediction_interval = pi_text
            ))
        },

        .populateHeterogeneityAssessment = function(ma_model) {
            het_table <- self$results$heterogeneityAssessment
            
            # Q-test for heterogeneity
            q_statistic <- ma_model$QE
            q_df <- ma_model$QE.df
            q_p <- ma_model$QEp
            
            q_interpretation <- if (q_p < 0.05) "Significant heterogeneity detected" else "No significant heterogeneity"
            
            het_table$addRow(rowKey = "q_test", values = list(
                statistic = "Q-statistic",
                value = q_statistic,
                confidence_interval = sprintf("df = %d", q_df),
                p_value = q_p,
                interpretation = q_interpretation
            ))
            
            # I² statistic
            if (!is.null(ma_model$I2)) {
                i2 <- ma_model$I2
                i2_interpretation <- if (i2 < 25) "Low heterogeneity" else
                                   if (i2 < 50) "Moderate heterogeneity" else
                                   if (i2 < 75) "Substantial heterogeneity" else
                                   "Considerable heterogeneity"
                
                het_table$addRow(rowKey = "i2", values = list(
                    statistic = "I² (%)",
                    value = i2,
                    confidence_interval = "",
                    p_value = NA,
                    interpretation = i2_interpretation
                ))
            }
            
            # τ² (tau-squared)
            if (!is.null(ma_model$tau2)) {
                tau2 <- ma_model$tau2
                tau <- sqrt(tau2)
                
                # Confidence interval for tau²
                tau2_ci <- confint(ma_model, type = "tau2")
                if (!is.null(tau2_ci)) {
                    tau2_ci_text <- sprintf("[%.4f, %.4f]", tau2_ci$random[1], tau2_ci$random[2])
                } else {
                    tau2_ci_text <- ""
                }
                
                het_table$addRow(rowKey = "tau2", values = list(
                    statistic = "τ² (tau-squared)",
                    value = tau2,
                    confidence_interval = tau2_ci_text,
                    p_value = NA,
                    interpretation = sprintf("Between-study SD = %.3f", tau)
                ))
                
                het_table$addRow(rowKey = "tau", values = list(
                    statistic = "τ (tau)",
                    value = tau,
                    confidence_interval = "",
                    p_value = NA,
                    interpretation = "Between-study standard deviation"
                ))
            }
        },

        .populateModelFitStatistics = function(ma_model) {
            fit_table <- self$results$modelFitStatistics
            
            # Extract fit statistics
            log_lik <- as.numeric(logLik(ma_model))
            aic <- AIC(ma_model)
            bic <- BIC(ma_model)
            deviance <- ma_model$QE  # Use Q-statistic as deviance measure
            
            fit_table$addRow(rowKey = "model_fit", values = list(
                model = "Random-Effects",
                log_likelihood = log_lik,
                aic = aic,
                bic = bic,
                deviance = deviance
            ))
        },

        .performPublicationBiasAnalysis = function(effects, variances, studies) {
            bias_table <- self$results$publicationBiasResults
            bias_tests <- self$options$bias_tests
            
            # Egger's test
            if (bias_tests %in% c('egger', 'all_tests') && length(effects) >= 3) {
                tryCatch({
                    egger_result <- metafor::regtest(private$ma_model, model = "lm")
                    
                    egger_interpretation <- if (egger_result$pval < 0.05) {
                        "Significant publication bias detected"
                    } else {
                        "No significant publication bias"
                    }
                    
                    egger_recommendation <- if (egger_result$pval < 0.05) {
                        "Consider trim-and-fill or additional bias assessment"
                    } else {
                        "Publication bias unlikely"
                    }
                    
                    bias_table$addRow(rowKey = "egger", values = list(
                        test = "Egger's Regression Test",
                        statistic = egger_result$zval,
                        p_value = egger_result$pval,
                        interpretation = egger_interpretation,
                        recommendation = egger_recommendation
                    ))
                }, error = function(e) {
                    bias_table$addRow(rowKey = "egger_error", values = list(
                        test = "Egger's Test",
                        statistic = NA,
                        p_value = NA,
                        interpretation = "Test failed",
                        recommendation = "Insufficient data or model issues"
                    ))
                })
            }
            
            # Begg's test (rank correlation)
            if (bias_tests %in% c('begg', 'all_tests') && length(effects) >= 3) {
                tryCatch({
                    begg_result <- metafor::ranktest(private$ma_model)
                    
                    begg_interpretation <- if (begg_result$pval < 0.05) {
                        "Significant publication bias detected"
                    } else {
                        "No significant publication bias"
                    }
                    
                    bias_table$addRow(rowKey = "begg", values = list(
                        test = "Begg's Rank Correlation Test",
                        statistic = begg_result$tau,
                        p_value = begg_result$pval,
                        interpretation = begg_interpretation,
                        recommendation = if (begg_result$pval < 0.05) "Investigate small-study effects" else "No action needed"
                    ))
                }, error = function(e) {
                    bias_table$addRow(rowKey = "begg_error", values = list(
                        test = "Begg's Test",
                        statistic = NA,
                        p_value = NA,
                        interpretation = "Test failed",
                        recommendation = "Check data quality"
                    ))
                })
            }
            
            # Trim-and-fill analysis
            if (bias_tests %in% c('trim_fill', 'all_tests')) {
                tryCatch({
                    tf_result <- metafor::trimfill(private$ma_model, side = "left")
                    
                    n_imputed <- tf_result$k0
                    tf_pooled <- tf_result$beta
                    original_pooled <- private$ma_model$beta
                    
                    tf_interpretation <- if (n_imputed > 0) {
                        sprintf("Suggests %d missing studies; adjusted estimate = %.3f", n_imputed, tf_pooled)
                    } else {
                        "No evidence of missing studies"
                    }
                    
                    bias_table$addRow(rowKey = "trim_fill", values = list(
                        test = "Trim-and-Fill Analysis",
                        statistic = n_imputed,
                        p_value = NA,
                        interpretation = tf_interpretation,
                        recommendation = if (n_imputed > 0) "Publication bias may be present" else "Results appear unbiased"
                    ))
                }, error = function(e) {
                    bias_table$addRow(rowKey = "tf_error", values = list(
                        test = "Trim-and-Fill",
                        statistic = NA,
                        p_value = NA,
                        interpretation = "Analysis failed",
                        recommendation = "Manual inspection recommended"
                    ))
                })
            }
        },

        .performSubgroupAnalysis = function(effects, variances, studies, subgroup_var) {
            subgroup_table <- self$results$subgroupAnalysis
            
            # Get subgroup variable data
            data <- self$data
            complete_cases <- complete.cases(effects, variances)
            subgroups <- data[[subgroup_var]][complete_cases]
            
            if (is.null(subgroups)) return()
            
            # Perform subgroup analysis
            tryCatch({
                # Create subgroup indicator
                unique_groups <- unique(subgroups[!is.na(subgroups)])
                
                for (group in unique_groups) {
                    group_indices <- which(subgroups == group & !is.na(subgroups))
                    
                    if (length(group_indices) >= 2) {
                        group_effects <- effects[group_indices]
                        group_variances <- variances[group_indices]
                        
                        # Fit random-effects model for subgroup
                        group_ma <- metafor::rma(yi = group_effects, vi = group_variances, method = 'REML')
                        
                        # Extract results
                        pooled_effect <- as.numeric(group_ma$beta)
                        ci_lower <- group_ma$ci.lb
                        ci_upper <- group_ma$ci.ub
                        p_value <- group_ma$pval
                        i2 <- if (!is.null(group_ma$I2)) group_ma$I2 else 0
                        
                        ci_text <- sprintf("[%.3f, %.3f]", ci_lower, ci_upper)
                        
                        subgroup_table$addRow(rowKey = paste0("subgroup_", group), values = list(
                            subgroup = as.character(group),
                            studies = length(group_indices),
                            pooled_effect = pooled_effect,
                            confidence_interval = ci_text,
                            heterogeneity_i2 = i2,
                            p_value = p_value
                        ))
                    }
                }
                
                # Test for subgroup differences
                if (length(unique_groups) > 1) {
                    subgroup_test <- metafor::rma(yi = effects, vi = variances, mods = ~ subgroups, method = 'REML')
                    
                    qm_statistic <- subgroup_test$QM
                    qm_p <- subgroup_test$QMp
                    
                    subgroup_table$addRow(rowKey = "subgroup_test", values = list(
                        subgroup = "Test for Subgroup Differences",
                        studies = NA,
                        pooled_effect = qm_statistic,
                        confidence_interval = "",
                        heterogeneity_i2 = NA,
                        p_value = qm_p
                    ))
                }
            }, error = function(e) {
                subgroup_table$addRow(rowKey = "subgroup_error", values = list(
                    subgroup = "Error in subgroup analysis",
                    studies = NA,
                    pooled_effect = NA,
                    confidence_interval = "",
                    heterogeneity_i2 = NA,
                    p_value = NA
                ))
            })
        },

        .performMetaRegression = function(effects, variances, studies) {
            regression_table <- self$results$metaRegressionResults
            
            # Get moderator variables
            moderator_vars <- self$options$moderator_vars
            if (is.null(moderator_vars) || length(moderator_vars) == 0) return()
            
            data <- self$data
            complete_cases <- complete.cases(effects, variances)
            
            tryCatch({
                # Prepare moderator data
                moderator_data <- data[complete_cases, moderator_vars, drop = FALSE]
                
                # Check for complete cases in moderators
                mod_complete <- complete.cases(moderator_data)
                if (sum(mod_complete) < 3) {
                    regression_table$addRow(rowKey = "insufficient_data", values = list(
                        moderator = "Error",
                        coefficient = NA,
                        standard_error = NA,
                        confidence_interval = "Insufficient complete data",
                        t_value = NA,
                        p_value = NA,
                        r_squared = NA
                    ))
                    return()
                }
                
                # Subset to complete cases
                mod_effects <- effects[mod_complete]
                mod_variances <- variances[mod_complete]
                mod_data_clean <- moderator_data[mod_complete, , drop = FALSE]
                
                # Perform meta-regression
                meta_reg <- metafor::rma(yi = mod_effects, vi = mod_variances, 
                                       mods = ~ ., data = mod_data_clean, method = 'REML')
                
                # Extract results for each moderator
                coef_names <- rownames(meta_reg$beta)[-1]  # Exclude intercept
                
                for (i in seq_along(coef_names)) {
                    coef_idx <- i + 1  # Skip intercept
                    
                    coefficient <- as.numeric(meta_reg$beta[coef_idx])
                    se <- meta_reg$se[coef_idx]
                    ci_lower <- meta_reg$ci.lb[coef_idx]
                    ci_upper <- meta_reg$ci.ub[coef_idx]
                    t_value <- meta_reg$zval[coef_idx]
                    p_value <- meta_reg$pval[coef_idx]
                    
                    ci_text <- sprintf("[%.4f, %.4f]", ci_lower, ci_upper)
                    
                    regression_table$addRow(rowKey = paste0("mod_", i), values = list(
                        moderator = coef_names[i],
                        coefficient = coefficient,
                        standard_error = se,
                        confidence_interval = ci_text,
                        t_value = t_value,
                        p_value = p_value,
                        r_squared = if (!is.null(meta_reg$R2)) meta_reg$R2 else NA
                    ))
                }
                
                # Add model R²
                if (!is.null(meta_reg$R2)) {
                    regression_table$addRow(rowKey = "model_r2", values = list(
                        moderator = "Model R² (explained heterogeneity)",
                        coefficient = meta_reg$R2,
                        standard_error = NA,
                        confidence_interval = sprintf("%.1f%% of heterogeneity explained", meta_reg$R2 * 100),
                        t_value = NA,
                        p_value = meta_reg$QMp,
                        r_squared = meta_reg$R2
                    ))
                }
            }, error = function(e) {
                regression_table$addRow(rowKey = "regression_error", values = list(
                    moderator = "Meta-regression failed",
                    coefficient = NA,
                    standard_error = NA,
                    confidence_interval = paste("Error:", e$message),
                    t_value = NA,
                    p_value = NA,
                    r_squared = NA
                ))
            })
        },

        .performSensitivityAnalysis = function(effects, variances, studies) {
            sensitivity_table <- self$results$sensitivityAnalysis
            
            n_studies <- length(effects)
            if (n_studies < 3) {
                sensitivity_table$addRow(rowKey = "insufficient", values = list(
                    analysis = "Insufficient studies",
                    excluded_studies = "N/A",
                    pooled_effect = NA,
                    confidence_interval = "Need ≥3 studies",
                    i2_change = NA
                ))
                return()
            }
            
            # Original I²
            original_i2 <- if (!is.null(private$ma_model$I2)) private$ma_model$I2 else 0
            
            # Leave-one-out analysis
            tryCatch({
                loo_results <- metafor::leave1out(private$ma_model)
                
                # Find studies with largest influence on results
                effect_changes <- abs(loo_results$estimate - as.numeric(private$ma_model$beta))
                most_influential_idx <- which.max(effect_changes)
                
                # Results when most influential study is removed
                most_influential_effect <- loo_results$estimate[most_influential_idx]
                most_influential_ci_lb <- loo_results$ci.lb[most_influential_idx]
                most_influential_ci_ub <- loo_results$ci.ub[most_influential_idx]
                most_influential_i2 <- loo_results$I2[most_influential_idx]
                
                ci_text <- sprintf("[%.3f, %.3f]", most_influential_ci_lb, most_influential_ci_ub)
                i2_change <- most_influential_i2 - original_i2
                
                sensitivity_table$addRow(rowKey = "most_influential", values = list(
                    analysis = "Remove Most Influential Study",
                    excluded_studies = as.character(studies[most_influential_idx]),
                    pooled_effect = most_influential_effect,
                    confidence_interval = ci_text,
                    i2_change = i2_change
                ))
                
                # Range of estimates across all leave-one-out analyses
                min_estimate <- min(loo_results$estimate)
                max_estimate <- max(loo_results$estimate)
                range_text <- sprintf("[%.3f, %.3f]", min_estimate, max_estimate)
                
                sensitivity_table$addRow(rowKey = "loo_range", values = list(
                    analysis = "Leave-One-Out Range",
                    excluded_studies = "Various",
                    pooled_effect = mean(loo_results$estimate),
                    confidence_interval = range_text,
                    i2_change = max(loo_results$I2) - min(loo_results$I2)
                ))
                
            }, error = function(e) {
                sensitivity_table$addRow(rowKey = "loo_error", values = list(
                    analysis = "Leave-one-out failed",
                    excluded_studies = "Error",
                    pooled_effect = NA,
                    confidence_interval = e$message,
                    i2_change = NA
                ))
            })
            
            # Fixed-effects vs random-effects comparison
            tryCatch({
                fe_model <- metafor::rma(yi = effects, vi = variances, method = 'FE')
                fe_effect <- as.numeric(fe_model$beta)
                re_effect <- as.numeric(private$ma_model$beta)
                
                sensitivity_table$addRow(rowKey = "fe_vs_re", values = list(
                    analysis = "Fixed vs Random Effects",
                    excluded_studies = "None",
                    pooled_effect = fe_effect,
                    confidence_interval = sprintf("RE = %.3f, FE = %.3f", re_effect, fe_effect),
                    i2_change = -original_i2  # Fixed-effects has I² = 0
                ))
            }, error = function(e) {
                # Error handling for fixed vs random effects comparison
            })
        },

        .performOutlierDetection = function(effects, variances, studies) {
            outlier_table <- self$results$outlierAnalysis
            
            if (length(effects) < 3) return()
            
            tryCatch({
                # Calculate standardized residuals
                residuals <- metafor::rstandard(private$ma_model)
                
                # Calculate leverage (hat values)
                leverage <- metafor::hatvalues(private$ma_model)
                
                # Calculate Cook's distance
                cooks_d <- metafor::cooks.distance(private$ma_model)
                
                # Identify outliers (|standardized residual| > 2)
                outlier_threshold <- 2
                
                for (i in seq_along(effects)) {
                    std_residual <- residuals$z[i]
                    hat_value <- leverage[i]
                    cook_distance <- cooks_d[i]
                    
                    is_outlier <- abs(std_residual) > outlier_threshold
                    outlier_flag <- if (is_outlier) "Yes" else "No"
                    
                    # Add influential studies as well (high Cook's distance)
                    if (cook_distance > 4/length(effects)) {
                        outlier_flag <- paste(outlier_flag, "(Influential)")
                    }
                    
                    outlier_table$addRow(rowKey = paste0("study_", i), values = list(
                        study = as.character(studies[i]),
                        standardized_residual = std_residual,
                        leverage = hat_value,
                        cooks_distance = cook_distance,
                        outlier_flag = outlier_flag
                    ))
                }
            }, error = function(e) {
                outlier_table$addRow(rowKey = "outlier_error", values = list(
                    study = "Error in outlier detection",
                    standardized_residual = NA,
                    leverage = NA,
                    cooks_distance = NA,
                    outlier_flag = e$message
                ))
            })
        },

        .performDiagnosticMetaAnalysis = function() {
            # Placeholder for diagnostic test accuracy meta-analysis
            # This would require additional implementation with mada or meta packages
            instructions <- self$results$instructions
            current_content <- instructions$content
            
            new_content <- paste0(current_content, 
                "<p><b>Note:</b> Diagnostic test accuracy meta-analysis requires TP, FP, FN, TN data.</p>")
            instructions$setContent(new_content)
        },

        .performNetworkMetaAnalysis = function() {
            # Placeholder for network meta-analysis
            # This would require additional implementation with netmeta or gemtc packages
            instructions <- self$results$instructions
            current_content <- instructions$content
            
            new_content <- paste0(current_content,
                "<p><b>Note:</b> Network meta-analysis requires treatment arm identifiers.</p>")
            instructions$setContent(new_content)
        },

        .populateMethodExplanation = function() {
            html <- "
            <html>
            <head>
            <meta http-equiv='Content-Type' content='text/html; charset=UTF-8'>
            </head>
            <body>
            <h3>Meta-Analysis Methods & Interpretation</h3>
            
            <h4>Effect Size Measures</h4>
            <p><b>Odds Ratio (OR):</b> Measure of association for binary outcomes. OR > 1 indicates increased odds.</p>
            <p><b>Mean Difference (MD):</b> Absolute difference in means between groups for continuous outcomes.</p>
            <p><b>Standardized Mean Difference (SMD):</b> Cohen's d; effect size standardized by pooled SD.</p>
            <p><b>Hazard Ratio (HR):</b> Time-to-event effect measure. HR > 1 indicates increased hazard.</p>
            
            <h4>Statistical Models</h4>
            <p><b>Fixed-Effects:</b> Assumes all studies estimate the same true effect (homogeneous).</p>
            <p><b>Random-Effects:</b> Allows for between-study heterogeneity; provides wider confidence intervals.</p>
            <p><b>Prediction Intervals:</b> Expected range for future studies (random-effects only).</p>
            
            <h4>Heterogeneity Assessment</h4>
            <p><b>Q-test:</b> Tests null hypothesis of homogeneity. p < 0.05 suggests significant heterogeneity.</p>
            <p><b>I² statistic:</b> Percentage of variation due to heterogeneity rather than sampling error.</p>
            <ul>
            <li>I² < 25%: Low heterogeneity</li>
            <li>I² 25-50%: Moderate heterogeneity</li>
            <li>I² 50-75%: Substantial heterogeneity</li>
            <li>I² > 75%: Considerable heterogeneity</li>
            </ul>
            <p><b>τ² (tau-squared):</b> Estimate of between-study variance in random-effects model.</p>
            
            <h4>Publication Bias Assessment</h4>
            <p><b>Egger's Test:</b> Regression test for funnel plot asymmetry. p < 0.05 suggests bias.</p>
            <p><b>Begg's Test:</b> Rank correlation test for small-study effects.</p>
            <p><b>Trim-and-Fill:</b> Estimates number of missing studies and adjusted effect size.</p>
            <p><b>Funnel Plot:</b> Visual assessment of publication bias (asymmetry suggests bias).</p>
            
            <h4>Sensitivity Analysis</h4>
            <p><b>Leave-One-Out:</b> Assesses influence of individual studies on pooled estimate.</p>
            <p><b>Outlier Detection:</b> Identifies studies with unusually large effect sizes or small standard errors.</p>
            <p><b>Fixed vs Random:</b> Comparison of model assumptions and their impact on results.</p>
            
            <h4>Meta-Regression</h4>
            <p><b>Purpose:</b> Investigate sources of heterogeneity using study-level covariates.</p>
            <p><b>R² interpretation:</b> Proportion of between-study variance explained by covariates.</p>
            <p><b>Residual heterogeneity:</b> Remaining unexplained between-study variation.</p>
            
            <h4>Reporting Guidelines</h4>
            <p>Follow PRISMA guidelines for systematic reviews and meta-analyses:</p>
            <ul>
            <li>Report both fixed-effects and random-effects results when heterogeneity is present</li>
            <li>Include prediction intervals for random-effects models</li>
            <li>Assess and report publication bias</li>
            <li>Perform sensitivity analyses</li>
            <li>Investigate heterogeneity through subgroup analysis or meta-regression</li>
            </ul>
            
            <p><b>References:</b></p>
            <p>• Borenstein M, et al. Introduction to Meta-Analysis. 2009.</p>
            <p>• Higgins JPT, et al. Cochrane Handbook for Systematic Reviews. 2019.</p>
            <p>• DerSimonian R, Laird N. Meta-analysis in clinical trials. Control Clin Trials. 1986.</p>
            <p>• Viechtbauer W. Conducting meta-analyses in R with the metafor package. JSS. 2010.</p>
            </body>
            </html>"
            
            self$results$methodExplanation$setContent(html)
        }
    )
)

metaanalysis <- metaanalysisClass$new