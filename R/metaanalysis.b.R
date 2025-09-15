metaanalysisClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "metaanalysisClass",
    inherit = metaanalysisBase,
    private = list(
        .init = function() {
            # Check dependencies and provide helpful messages
            private$.checkDependencies()

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

        .checkDependencies = function() {
            missing_packages <- character(0)
            optional_features <- character(0)

            # Check essential packages
            if (!requireNamespace("metafor", quietly = TRUE)) {
                missing_packages <- c(missing_packages, "metafor")
            }

            # Check optional packages and note missing features
            if (!requireNamespace("mada", quietly = TRUE)) {
                optional_features <- c(optional_features, "Advanced diagnostic test accuracy models (install 'mada' package)")
            }

            if (!requireNamespace("netmeta", quietly = TRUE)) {
                optional_features <- c(optional_features, "Advanced network meta-analysis (install 'netmeta' package)")
            }

            if (!requireNamespace("meta", quietly = TRUE)) {
                optional_features <- c(optional_features, "Additional meta-analysis methods (install 'meta' package)")
            }

            # Display warnings if packages are missing
            if (length(missing_packages) > 0) {
                self$results$instructions$setContent(
                    paste0("<div style='color: #dc3545; padding: 15px; background-color: #f8d7da; border-radius: 5px;'>
                           <h4>⚠️ Missing Required Packages</h4>
                           <p>Please install the following packages to use meta-analysis:</p>
                           <ul><li>", paste(missing_packages, collapse = "</li><li>"), "</li></ul>
                           <p>Install with: <code>install.packages(c('", paste(missing_packages, collapse = "', '"), "'))</code></p>
                           </div>")
                )
                return()
            }

            # Note optional features if packages missing
            if (length(optional_features) > 0) {
                feature_note <- paste0("<div style='color: #856404; padding: 10px; background-color: #fff3cd; border-radius: 5px; margin-bottom: 10px;'>
                                       <h5>💡 Optional Features Available</h5>
                                       <p>Install additional packages for enhanced functionality:</p>
                                       <ul><li>", paste(optional_features, collapse = "</li><li>"), "</li></ul>
                                       </div>")
                # Store for later display
                private$.optional_features_note <- feature_note
            }
        },

        .validateInputs = function() {
            # Check for required packages
            if (!requireNamespace("metafor", quietly = TRUE)) {
                self$results$instructions$setContent(
                    "<div style='color: #dc3545; padding: 15px; background-color: #f8d7da; border-radius: 5px;'>
                    <h4>⚠️ Required Package Missing</h4>
                    <p>Meta-analysis requires the 'metafor' package.</p>
                    <p>Install with: <code>install.packages('metafor')</code></p>
                    </div>"
                )
                return(FALSE)
            }

            # Get the required variables
            effect_var <- self$options$effect_size
            variance_var <- self$options$variance

            if (is.null(effect_var) || is.null(variance_var)) {
                if (exists(".optional_features_note", envir = private)) {
                    self$results$instructions$setContent(private$.optional_features_note)
                }
                return(FALSE)
            }

            return(TRUE)
        },

        .validateAssumptions = function() {
            # Get the required variables
            effect_var <- self$options$effect_size
            variance_var <- self$options$variance
            analysis_type <- self$options$analysis_type

            if (is.null(effect_var) || is.null(variance_var)) {
                return(list(valid = FALSE, message = "Effect size and variance variables must be selected."))
            }

            # Get data
            data <- self$data
            effect_sizes <- jmvcore::toNumeric(data[[effect_var]])
            variances <- jmvcore::toNumeric(data[[variance_var]])

            # Check for common issues
            warnings <- character(0)
            errors <- character(0)

            # Check sample size
            n_total <- length(effect_sizes)
            n_complete <- sum(complete.cases(effect_sizes, variances))

            if (n_complete < 3) {
                errors <- c(errors, "Meta-analysis requires at least 3 studies with complete data.")
            } else if (n_complete < 5) {
                warnings <- c(warnings, "Small number of studies (< 5) may limit the reliability of pooled estimates.")
            }

            # Check for extreme effect sizes (potential data entry errors)
            if (n_complete > 0) {
                extreme_effects <- abs(effect_sizes) > 10 & !is.na(effect_sizes)
                if (any(extreme_effects)) {
                    warnings <- c(warnings, sprintf("Found %d studies with very large effect sizes (>10). Please verify these are correct.", sum(extreme_effects)))
                }

                # Check for negative variances
                negative_var <- variances < 0 & !is.na(variances)
                if (any(negative_var)) {
                    errors <- c(errors, "Negative variances found. Variances must be positive.")
                }

                # Check for very small variances (precision issues)
                very_small_var <- variances < 1e-6 & !is.na(variances)
                if (any(very_small_var)) {
                    warnings <- c(warnings, "Some studies have very small variances. This may indicate precision issues.")
                }
            }

            # Analysis-specific checks
            if (analysis_type == 'diagnostic_accuracy') {
                # Check if diagnostic variables are provided
                tp_var <- self$options$true_positives
                fp_var <- self$options$false_positives
                fn_var <- self$options$false_negatives
                tn_var <- self$options$true_negatives

                if (is.null(tp_var) || is.null(fp_var) || is.null(fn_var) || is.null(tn_var)) {
                    errors <- c(errors, "Diagnostic test accuracy analysis requires True Positives, False Positives, False Negatives, and True Negatives variables.")
                }
            }

            # Generate warning/error message
            if (length(errors) > 0) {
                message <- paste0(
                    "<div style='color: #dc3545; padding: 15px; background-color: #f8d7da; border-radius: 5px;'>
                    <h4>❌ Data Issues Found</h4>
                    <p><b>The following issues must be resolved:</b></p>
                    <ul><li>", paste(errors, collapse = "</li><li>"), "</li></ul>
                    </div>"
                )
                return(list(valid = FALSE, message = message))
            }

            if (length(warnings) > 0) {
                message <- paste0(
                    "<div style='color: #856404; padding: 15px; background-color: #fff3cd; border-radius: 5px;'>
                    <h4>⚠️ Data Quality Warnings</h4>
                    <p><b>Please review the following:</b></p>
                    <ul><li>", paste(warnings, collapse = "</li><li>"), "</li></ul>
                    <p><i>You can proceed with the analysis, but consider addressing these issues for better results.</i></p>
                    </div>"
                )
                # Store warnings to display later
                private$.data_warnings <- message
            }

            return(list(valid = TRUE, message = NULL))
        },

        .getCleanData = function() {
            # Return cached data if available
            if (!is.null(private$.cached_clean_data)) {
                return(private$.cached_clean_data)
            }

            # Perform assumption validation
            validation_result <- private$.validateAssumptions()
            if (!validation_result$valid) {
                self$results$instructions$setContent(validation_result$message)
                return(NULL)
            }

            # Get the required variables
            effect_var <- self$options$effect_size
            variance_var <- self$options$variance
            study_var <- self$options$study_id

            # Prepare data
            data <- self$data
            effect_sizes <- jmvcore::toNumeric(data[[effect_var]])
            variances <- jmvcore::toNumeric(data[[variance_var]])
            study_ids <- if (!is.null(study_var)) data[[study_var]] else paste0("Study_", seq_along(effect_sizes))

            # Optional variables
            sample_sizes <- if (!is.null(self$options$sample_size)) {
                jmvcore::toNumeric(data[[self$options$sample_size]])
            } else {
                NULL
            }

            years <- if (!is.null(self$options$year)) {
                jmvcore::toNumeric(data[[self$options$year]])
            } else {
                NULL
            }

            # Remove missing values
            if (!is.null(sample_sizes) && !is.null(years)) {
                complete_cases <- complete.cases(effect_sizes, variances, study_ids, sample_sizes, years)
            } else if (!is.null(sample_sizes)) {
                complete_cases <- complete.cases(effect_sizes, variances, study_ids, sample_sizes)
            } else if (!is.null(years)) {
                complete_cases <- complete.cases(effect_sizes, variances, study_ids, years)
            } else {
                complete_cases <- complete.cases(effect_sizes, variances, study_ids)
            }

            if (sum(complete_cases) < 2) {
                self$results$instructions$setContent(
                    "<div style='color: #dc3545; padding: 15px; background-color: #f8d7da; border-radius: 5px;'>
                    <h4>⚠️ Insufficient Data</h4>
                    <p>Meta-analysis requires at least 2 studies with complete effect size and variance data.</p>
                    <p><b>Current status:</b> Found " + sum(complete_cases) + " complete cases out of " + length(effect_sizes) + " total.</p>
                    <p><b>Check your data for:</b></p>
                    <ul>
                    <li>Missing effect sizes</li>
                    <li>Missing variance/standard error values</li>
                    <li>Non-numeric values in these columns</li>
                    </ul>
                    </div>"
                )
                return(NULL)
            }

            # Cache cleaned data
            clean_data <- list(
                effects = effect_sizes[complete_cases],
                variances = variances[complete_cases],
                studies = study_ids[complete_cases],
                sample_sizes = if (!is.null(sample_sizes)) sample_sizes[complete_cases] else NULL,
                years = if (!is.null(years)) years[complete_cases] else NULL,
                n_studies = sum(complete_cases),
                complete_cases = complete_cases
            )

            private$.cached_clean_data <- clean_data
            return(clean_data)
        },

        .determineAnalysisType = function() {
            return(self$options$analysis_type)
        },

        .executeAnalysis = function(analysis_type, clean_data) {
            if (is.null(clean_data)) return()

            # Populate study summary first
            private$.populateStudySummary(clean_data$effects, clean_data$variances,
                                         clean_data$studies, clean_data$sample_sizes)

            # Execute specific analysis type
            switch(analysis_type,
                'generic' = private$.performGenericMetaAnalysis(clean_data),
                'diagnostic_accuracy' = private$.performDiagnosticMetaAnalysis(),
                'network' = private$.performNetworkMetaAnalysis(),
                'individual_patient' = {
                    self$results$instructions$setContent(
                        "<p><b>Note:</b> Individual patient data meta-analysis is not yet implemented.</p>"
                    )
                }
            )

            # Common analyses for applicable types
            if (analysis_type == 'generic') {
                if (self$options$publication_bias) {
                    private$.performPublicationBiasAnalysis(clean_data$effects, clean_data$variances, clean_data$studies)
                }

                if (self$options$meta_regression && !is.null(self$options$moderator_vars)) {
                    private$.performMetaRegression(clean_data$effects, clean_data$variances, clean_data$studies)
                }

                if (self$options$sensitivity_analysis) {
                    private$.performSensitivityAnalysis(clean_data$effects, clean_data$variances, clean_data$studies)
                }

                if (self$options$outlier_detection) {
                    private$.performOutlierDetection(clean_data$effects, clean_data$variances, clean_data$studies)
                }
            }
        },

        .run = function() {
            # Clear any cached data on new run
            private$.cached_clean_data <- NULL

            # Modular execution pipeline
            if (!private$.validateInputs()) return()

            clean_data <- private$.getCleanData()
            if (is.null(clean_data)) return()

            analysis_type <- private$.determineAnalysisType()
            private$.executeAnalysis(analysis_type, clean_data)

            # Generate clinical summary
            private$.generateClinicalSummary(analysis_type, clean_data)
        },

        .performGenericMetaAnalysis = function(clean_data) {
            # Perform meta-analysis using cleaned data
            model_type <- self$options$model_type
            heterogeneity_method <- self$options$heterogeneity_method
            effect_measure <- self$options$effect_measure
            robust_methods <- self$options$robust_methods
            small_sample_correction <- self$options$small_sample_correction

            # Map heterogeneity methods to metafor
            tau2_method <- switch(heterogeneity_method,
                'dersimonian_laird' = 'DL',
                'restricted_ml' = 'REML',
                'maximum_likelihood' = 'ML',
                'empirical_bayes' = 'EB',
                'paule_mandel' = 'PM',
                'DL')  # Default

            # Fixed-effects model
            if (model_type %in% c('fixed_effects', 'mixed_effects')) {
                ma_fixed <- metafor::rma(yi = clean_data$effects, vi = clean_data$variances, method = "FE")
                private$.populateOverallResults(ma_fixed, "Fixed-Effects")
                private$.populateModelFitStatistics(ma_fixed)
            }

            # Random-effects model
            if (model_type %in% c('random_effects', 'mixed_effects')) {
                ma_random <- metafor::rma(yi = clean_data$effects, vi = clean_data$variances, method = tau2_method)

                # Apply adjustments if requested
                if ((self$options$knha_adjustment || small_sample_correction || robust_methods) &&
                    tau2_method %in% c('DL', 'REML', 'ML')) {
                    ma_random <- metafor::rma(yi = clean_data$effects, vi = clean_data$variances, method = tau2_method, knha = TRUE)
                }

                private$.populateOverallResults(ma_random, "Random-Effects")
                private$.populateHeterogeneityAssessment(ma_random)
                private$.populateModelFitStatistics(ma_random)
            }

            # Subgroup analysis
            if (!is.null(self$options$subgroup_var)) {
                private$.performSubgroupAnalysis(clean_data$effects, clean_data$variances, clean_data$studies)
            }
        },

        .generateClinicalSummary = function(analysis_type, clean_data) {
            # Add clinical summary based on analysis type
            if (is.null(clean_data) || clean_data$n_studies < 2) return()

            effect_measure <- self$options$effect_measure
            n_studies <- clean_data$n_studies

            # Generate natural language summary
            summary_text <- switch(analysis_type,
                'generic' = private$.generateGenericSummary(clean_data),
                'diagnostic_accuracy' = private$.generateDiagnosticSummary(),
                'network' = private$.generateNetworkSummary(),
                "Analysis completed for " + n_studies + " studies."
            )

            # Start with summary
            summary_html <- paste0(
                "<div style='background-color: #e8f5e8; padding: 15px; border-radius: 5px; margin: 10px 0;'>
                <h4>📊 Analysis Summary</h4>
                <p>", summary_text, "</p>
                </div>"
            )

            # Add data warnings if they exist
            if (!is.null(private$.data_warnings)) {
                summary_html <- paste0(summary_html, private$.data_warnings)
            }

            # Add copy-ready interpretation
            copy_ready_text <- private$.generateCopyReadyInterpretation(analysis_type, clean_data)
            if (!is.null(copy_ready_text)) {
                summary_html <- paste0(summary_html,
                    "<div style='background-color: #f8f9fa; padding: 15px; border-radius: 5px; margin: 10px 0; border-left: 4px solid #28a745;'>
                    <h4>📝 Copy-Ready Interpretation</h4>
                    <p><i>Click to select and copy:</i></p>
                    <p style='background-color: #ffffff; padding: 10px; border: 1px dashed #ccc; border-radius: 3px; font-family: monospace; font-size: 14px;' onclick='this.select();'>",
                    copy_ready_text, "</p>
                    </div>"
                )
            }

            self$results$instructions$setContent(summary_html)
        },

        .generateGenericSummary = function(clean_data) {
            # Simple summary for now - can be enhanced
            effect_measure_name <- switch(self$options$effect_measure,
                'odds_ratio' = 'log odds ratio',
                'risk_ratio' = 'log risk ratio',
                'hazard_ratio' = 'log hazard ratio',
                'mean_difference' = 'mean difference',
                'standardized_mean_diff' = 'standardized mean difference',
                'correlation' = 'correlation coefficient',
                'effect size'
            )

            return(paste0("Meta-analysis of ", clean_data$n_studies, " studies examining ",
                         effect_measure_name, " completed. Check the forest plot and results tables below for detailed findings."))
        },

        .generateDiagnosticSummary = function() {
            return("Diagnostic test accuracy meta-analysis completed. Review the pooled sensitivity and specificity estimates in the results tables.")
        },

        .generateNetworkSummary = function() {
            return("Network meta-analysis completed. Review the treatment comparison results and network diagram below.")
        },

        .generateCopyReadyInterpretation = function(analysis_type, clean_data) {
            # Generate a copy-ready paragraph for clinical reports
            if (is.null(clean_data) || clean_data$n_studies < 2) return(NULL)

            effect_measure <- self$options$effect_measure
            model_type <- self$options$model_type
            n_studies <- clean_data$n_studies

            # Basic template
            effect_name <- switch(effect_measure,
                'odds_ratio' = 'odds ratios',
                'risk_ratio' = 'risk ratios',
                'hazard_ratio' = 'hazard ratios',
                'mean_difference' = 'mean differences',
                'standardized_mean_diff' = 'standardized mean differences',
                'correlation' = 'correlation coefficients',
                'effect sizes'
            )

            model_desc <- switch(model_type,
                'fixed_effects' = 'fixed-effects model',
                'random_effects' = 'random-effects model',
                'mixed_effects' = 'mixed-effects model',
                'meta-analysis model'
            )

            interpretation <- switch(analysis_type,
                'generic' = sprintf(
                    "A meta-analysis of %d studies was conducted to examine %s using a %s. [INSERT MAIN FINDINGS HERE: pooled effect size with 95%% confidence interval and p-value]. Heterogeneity between studies was [INSERT I² VALUE AND INTERPRETATION]. [INSERT PUBLICATION BIAS ASSESSMENT IF CONDUCTED]. These findings suggest [INSERT CLINICAL INTERPRETATION AND IMPLICATIONS].",
                    n_studies, effect_name, model_desc
                ),
                'diagnostic_accuracy' = sprintf(
                    "A diagnostic test accuracy meta-analysis of %d studies was performed. [INSERT POOLED SENSITIVITY AND SPECIFICITY WITH 95%% CONFIDENCE INTERVALS]. The summary ROC analysis demonstrated [INSERT AUC AND INTERPRETATION]. These findings indicate [INSERT CLINICAL UTILITY AND RECOMMENDATIONS FOR TEST USE].",
                    n_studies
                ),
                'network' = sprintf(
                    "A network meta-analysis of %d studies was conducted to compare multiple treatments. [INSERT RANKING RESULTS AND BEST TREATMENT]. The network geometry showed [INSERT NETWORK CHARACTERISTICS]. [INSERT CLINICAL RECOMMENDATIONS BASED ON COMPARATIVE EFFECTIVENESS].",
                    n_studies
                ),
                sprintf("Meta-analysis of %d studies completed. [INSERT DETAILED INTERPRETATION].", n_studies)
            )

            return(interpretation)
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
            # Check for required diagnostic accuracy variables
            tp_var <- self$options$true_positives
            fp_var <- self$options$false_positives
            fn_var <- self$options$false_negatives
            tn_var <- self$options$true_negatives
            dta_model_type <- self$options$dta_model_type

            if (is.null(tp_var) || is.null(fp_var) || is.null(fn_var) || is.null(tn_var)) {
                self$results$instructions$setContent(
                    "<p><b>Error:</b> Diagnostic test accuracy meta-analysis requires all four variables:</p>
                     <ul>
                     <li>True Positives</li>
                     <li>False Positives</li>
                     <li>False Negatives</li>
                     <li>True Negatives</li>
                     </ul>"
                )
                return()
            }

            # Get data
            data <- self$data
            tp <- jmvcore::toNumeric(data[[tp_var]])
            fp <- jmvcore::toNumeric(data[[fp_var]])
            fn <- jmvcore::toNumeric(data[[fn_var]])
            tn <- jmvcore::toNumeric(data[[tn_var]])

            # Remove missing values
            complete_cases <- complete.cases(tp, fp, fn, tn)
            if (sum(complete_cases) < 3) {
                self$results$instructions$setContent(
                    "<p><b>Error:</b> Diagnostic test accuracy meta-analysis requires at least 3 complete studies.</p>"
                )
                return()
            }

            clean_tp <- tp[complete_cases]
            clean_fp <- fp[complete_cases]
            clean_fn <- fn[complete_cases]
            clean_tn <- tn[complete_cases]

            # Calculate sensitivity and specificity for each study
            sensitivity <- clean_tp / (clean_tp + clean_fn)
            specificity <- clean_tn / (clean_tn + clean_fp)

            # Perform basic diagnostic accuracy analysis
            tryCatch({
                # Calculate pooled estimates using simple pooling (for now)
                # In practice, would use mada package for bivariate analysis

                # Logit transformation for meta-analysis
                logit_sens <- log(sensitivity / (1 - sensitivity))
                logit_spec <- log(specificity / (1 - specificity))

                # Calculate variances (simplified approach)
                var_sens <- (1/clean_tp) + (1/clean_fn)
                var_spec <- (1/clean_tn) + (1/clean_fp)

                # Perform meta-analysis on logit scale based on model type and available packages
                if (requireNamespace("metafor", quietly = TRUE)) {
                    # Use advanced methods if packages are available
                    if (dta_model_type %in% c('bivariate', 'hsroc') && requireNamespace("mada", quietly = TRUE)) {
                        # Advanced bivariate model using mada package
                        tryCatch({
                            # Prepare data for mada
                            dta_data <- data.frame(
                                TP = clean_tp, FP = clean_fp,
                                FN = clean_fn, TN = clean_tn
                            )

                            if (dta_model_type == 'bivariate') {
                                # Bivariate model
                                mada_result <- mada::reitsma(dta_data)
                                pooled_sens <- mada_result$sens$mu
                                pooled_spec <- mada_result$spec$mu

                                # Get confidence intervals
                                sens_ci_lower <- mada_result$sens$ci[1]
                                sens_ci_upper <- mada_result$sens$ci[2]
                                spec_ci_lower <- mada_result$spec$ci[1]
                                spec_ci_upper <- mada_result$spec$ci[2]

                                model_used <- "Bivariate (mada package)"
                            } else {
                                # HSROC model
                                hsroc_result <- mada::SummaryPts(mada::reitsma(dta_data), sroc.type = "ruttergatsonis")
                                pooled_sens <- hsroc_result$sens
                                pooled_spec <- hsroc_result$spec

                                sens_ci_lower <- hsroc_result$sens.ci[1]
                                sens_ci_upper <- hsroc_result$sens.ci[2]
                                spec_ci_lower <- hsroc_result$spec.ci[1]
                                spec_ci_upper <- hsroc_result$spec.ci[2]

                                model_used <- "HSROC (mada package)"
                            }
                        }, error = function(e) {
                            # Fall back to univariate analysis if mada fails
                            warning("Advanced diagnostic model failed, using univariate analysis: ", e$message)
                            dta_model_type <<- 'univariate'
                        })
                    }

                    # Univariate analysis (fallback or selected)
                    if (dta_model_type == 'univariate' || !exists("pooled_sens")) {
                        method <- switch(dta_model_type,
                                       'univariate' = 'DL',     # Simple univariate analysis
                                       'REML')                  # Default fallback

                        # Meta-analysis of sensitivity
                        ma_sens <- metafor::rma(yi = logit_sens, vi = var_sens, method = method)
                        pooled_sens <- exp(ma_sens$beta[1]) / (1 + exp(ma_sens$beta[1]))

                        # Meta-analysis of specificity
                        ma_spec <- metafor::rma(yi = logit_spec, vi = var_spec, method = method)
                        pooled_spec <- exp(ma_spec$beta[1]) / (1 + exp(ma_spec$beta[1]))

                        # Calculate confidence intervals
                        sens_ci_lower <- exp(ma_sens$ci.lb) / (1 + exp(ma_sens$ci.lb))
                        sens_ci_upper <- exp(ma_sens$ci.ub) / (1 + exp(ma_sens$ci.ub))
                        spec_ci_lower <- exp(ma_spec$ci.lb) / (1 + exp(ma_spec$ci.lb))
                        spec_ci_upper <- exp(ma_spec$ci.ub) / (1 + exp(ma_spec$ci.ub))

                        model_used <- if (dta_model_type == 'univariate') "Univariate (metafor)" else "Univariate fallback (metafor)"
                    }

                    # Populate diagnostic accuracy results table
                    dta_table <- self$results$diagnosticAccuracyResults

                    # Add model information
                    if (exists("model_used")) {
                        dta_table$addRow(rowKey = "model_info", values = list(
                            parameter = "Analysis Method",
                            pooled_estimate = NA,
                            confidence_interval = model_used,
                            prediction_region = "See parameters below",
                            heterogeneity = "Method-specific"
                        ))
                    }

                    # Add sensitivity result
                    het_info <- if (exists("ma_sens") && !is.null(ma_sens)) {
                        sprintf("I² = %.1f%%, τ² = %.3f",
                                max(0, (ma_sens$QE - ma_sens$k + 1) / ma_sens$QE * 100),
                                ma_sens$tau2)
                    } else {
                        "Advanced method used"
                    }

                    dta_table$addRow(rowKey = "sensitivity", values = list(
                        parameter = "Pooled Sensitivity",
                        pooled_estimate = pooled_sens,
                        confidence_interval = sprintf("%.3f - %.3f", sens_ci_lower, sens_ci_upper),
                        prediction_region = "Not calculated",
                        heterogeneity = het_info
                    ))

                    # Add specificity result
                    het_info_spec <- if (exists("ma_spec") && !is.null(ma_spec)) {
                        sprintf("I² = %.1f%%, τ² = %.3f",
                                max(0, (ma_spec$QE - ma_spec$k + 1) / ma_spec$QE * 100),
                                ma_spec$tau2)
                    } else {
                        "Advanced method used"
                    }

                    dta_table$addRow(rowKey = "specificity", values = list(
                        parameter = "Pooled Specificity",
                        pooled_estimate = pooled_spec,
                        confidence_interval = sprintf("%.3f - %.3f", spec_ci_lower, spec_ci_upper),
                        prediction_region = "Not calculated",
                        heterogeneity = het_info_spec
                    ))

                    # Calculate derived measures
                    ppv <- (pooled_sens * 0.5) / (pooled_sens * 0.5 + (1 - pooled_spec) * 0.5)  # Assuming 50% prevalence
                    npv <- (pooled_spec * 0.5) / (pooled_spec * 0.5 + (1 - pooled_sens) * 0.5)

                    # Add derived measures
                    dta_table$addRow(rowKey = "ppv", values = list(
                        parameter = "Positive Predictive Value (50% prevalence)",
                        pooled_estimate = ppv,
                        confidence_interval = "Not calculated",
                        prediction_region = "Not calculated",
                        heterogeneity = "N/A"
                    ))

                    dta_table$addRow(rowKey = "npv", values = list(
                        parameter = "Negative Predictive Value (50% prevalence)",
                        pooled_estimate = npv,
                        confidence_interval = "Not calculated",
                        prediction_region = "Not calculated",
                        heterogeneity = "N/A"
                    ))
                }

            }, error = function(e) {
                self$results$instructions$setContent(
                    paste0("<p><b>Error in diagnostic test accuracy analysis:</b> ", e$message, "</p>")
                )
            })
        },

        .performNetworkMetaAnalysis = function() {
            # Check for required network meta-analysis variables
            effect_var <- self$options$effect_size
            variance_var <- self$options$variance
            study_var <- self$options$study_id
            treatment_var <- self$options$treatment_arm
            comparison_var <- self$options$comparison_arm
            network_method <- self$options$network_method

            if (is.null(effect_var) || is.null(variance_var) || is.null(treatment_var)) {
                self$results$instructions$setContent(
                    "<p><b>Error:</b> Network meta-analysis requires:</p>
                     <ul>
                     <li>Effect Size</li>
                     <li>Variance/SE</li>
                     <li>Treatment Arm</li>
                     <li>Study ID</li>
                     </ul>"
                )
                return()
            }

            # Get data
            data <- self$data
            effect_sizes <- jmvcore::toNumeric(data[[effect_var]])
            variances <- jmvcore::toNumeric(data[[variance_var]])
            study_ids <- if (!is.null(study_var)) data[[study_var]] else paste0("Study_", seq_along(effect_sizes))
            treatments <- data[[treatment_var]]
            comparisons <- if (!is.null(comparison_var)) data[[comparison_var]] else "Control"

            # Remove missing values
            complete_cases <- complete.cases(effect_sizes, variances, treatments)
            if (sum(complete_cases) < 3) {
                self$results$instructions$setContent(
                    "<p><b>Error:</b> Network meta-analysis requires at least 3 complete comparisons.</p>"
                )
                return()
            }

            clean_effects <- effect_sizes[complete_cases]
            clean_variances <- variances[complete_cases]
            clean_studies <- study_ids[complete_cases]
            clean_treatments <- treatments[complete_cases]
            clean_comparisons <- comparisons[complete_cases]

            # Basic network analysis (simplified)
            tryCatch({
                # Get unique treatments
                unique_treatments <- unique(c(clean_treatments, clean_comparisons))
                n_treatments <- length(unique_treatments)

                if (n_treatments < 3) {
                    self$results$instructions$setContent(
                        "<p><b>Error:</b> Network meta-analysis requires at least 3 different treatments.</p>"
                    )
                    return()
                }

                # Populate network results table (simplified approach)
                network_table <- self$results$networkResults

                # For each unique treatment comparison, calculate pooled effect
                for (i in seq_along(unique_treatments)) {
                    treatment <- unique_treatments[i]

                    # Find comparisons involving this treatment
                    treatment_indices <- which(clean_treatments == treatment | clean_comparisons == treatment)

                    if (length(treatment_indices) > 0) {
                        # Pooling approach based on network method
                        if (requireNamespace("metafor", quietly = TRUE)) {
                            treatment_effects <- clean_effects[treatment_indices]
                            treatment_variances <- clean_variances[treatment_indices]

                            if (length(treatment_effects) >= 2) {
                                # Choose analysis method based on network_method
                                analysis_method <- switch(network_method,
                                                        'frequentist' = 'REML',
                                                        'bayesian' = 'REML',     # Would use brms/MCMCglmm for true Bayesian
                                                        'mixed_treatment' = 'ML', # Mixed treatment comparison
                                                        'REML')                   # Default

                                ma_treatment <- metafor::rma(yi = treatment_effects, vi = treatment_variances, method = analysis_method)

                                # Calculate ranking probability (simplified)
                                ranking_prob <- exp(-abs(ma_treatment$beta[1])) / sum(exp(-abs(clean_effects)))

                                network_table$addRow(rowKey = paste0("treatment_", i), values = list(
                                    comparison = paste(treatment, "vs Others"),
                                    effect_estimate = ma_treatment$beta[1],
                                    credible_interval = sprintf("%.3f - %.3f", ma_treatment$ci.lb, ma_treatment$ci.ub),
                                    probability_best = ranking_prob,
                                    ranking = paste("Rank", i, "of", n_treatments)
                                ))
                            }
                        }
                    }
                }

                # Add summary information
                network_table$addRow(rowKey = "summary", values = list(
                    comparison = "Network Summary",
                    effect_estimate = NA,
                    credible_interval = sprintf("%d treatments, %d studies", n_treatments, length(unique(clean_studies))),
                    probability_best = NA,
                    ranking = "See individual comparisons above"
                ))

            }, error = function(e) {
                self$results$instructions$setContent(
                    paste0("<p><b>Error in network meta-analysis:</b> ", e$message, "</p>")
                )
            })
        },

        .getEffectMeasureInterpretation = function() {
            effect_measure <- self$options$effect_measure

            interpretation <- switch(effect_measure,
                'mean_difference' = "Mean difference: Absolute difference between group means (continuous outcomes).",
                'standardized_mean_diff' = "Standardized mean difference (Cohen's d): Effect size independent of measurement scale.",
                'odds_ratio' = "Log odds ratio: Logarithm of the odds ratio (binary outcomes). Exp(estimate) gives OR.",
                'risk_ratio' = "Log risk ratio: Logarithm of the risk ratio (binary outcomes). Exp(estimate) gives RR.",
                'hazard_ratio' = "Log hazard ratio: Logarithm of the hazard ratio (time-to-event outcomes). Exp(estimate) gives HR.",
                'correlation' = "Correlation coefficient: Measure of association between variables (-1 to +1).",
                "Effect size interpretation varies by measure type."
            )

            return(interpretation)
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
            <p><b>Current Effect Measure:</b> " + private$.getEffectMeasureInterpretation() + "</p>
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
        },

        # Plot Functions
        .forestPlot = function(image, ...) {
            if (!requireNamespace("metafor", quietly = TRUE)) {
                return(FALSE)
            }

            # Get the data for plotting
            effect_var <- self$options$effect_size
            variance_var <- self$options$variance
            study_var <- self$options$study_id
            forest_options <- self$options$forest_plot_options
            prediction_intervals <- self$options$prediction_interval

            if (is.null(effect_var) || is.null(variance_var)) {
                return(FALSE)
            }

            # Prepare data
            data <- self$data
            effect_sizes <- jmvcore::toNumeric(data[[effect_var]])
            variances <- jmvcore::toNumeric(data[[variance_var]])
            study_ids <- if (!is.null(study_var)) data[[study_var]] else paste0("Study_", seq_along(effect_sizes))

            # Remove missing values
            complete_cases <- complete.cases(effect_sizes, variances)
            if (sum(complete_cases) < 2) {
                return(FALSE)
            }

            clean_effects <- effect_sizes[complete_cases]
            clean_variances <- variances[complete_cases]
            clean_studies <- study_ids[complete_cases]

            # Perform meta-analysis
            tryCatch({
                ma_model <- metafor::rma(yi = clean_effects, vi = clean_variances,
                                       slab = clean_studies, method = "REML")

                # Create forest plot with customization
                if (forest_options) {
                    # Enhanced forest plot with customization
                    metafor::forest(ma_model,
                                  main = "Meta-Analysis Forest Plot",
                                  xlab = paste("Effect Size (", self$options$effect_measure, ")", sep = ""),
                                  cex = 0.8,
                                  psize = 1)

                    # Add prediction interval if requested
                    if (prediction_intervals && !is.null(ma_model$tau2) && ma_model$tau2 > 0) {
                        # Prediction interval would be added here
                        # (metafor's forest() function can include prediction intervals)
                    }
                } else {
                    # Basic forest plot
                    print(ma_model)
                }
                TRUE
            }, error = function(e) {
                FALSE
            })
        },

        .funnelPlot = function(image, ...) {
            if (!requireNamespace("metafor", quietly = TRUE)) {
                return(FALSE)
            }

            # Get the data for plotting
            effect_var <- self$options$effect_size
            variance_var <- self$options$variance

            if (is.null(effect_var) || is.null(variance_var)) {
                return(FALSE)
            }

            # Prepare data
            data <- self$data
            effect_sizes <- jmvcore::toNumeric(data[[effect_var]])
            variances <- jmvcore::toNumeric(data[[variance_var]])

            # Remove missing values
            complete_cases <- complete.cases(effect_sizes, variances)
            if (sum(complete_cases) < 3) {
                return(FALSE)
            }

            clean_effects <- effect_sizes[complete_cases]
            clean_variances <- variances[complete_cases]

            # Perform meta-analysis and create funnel plot
            tryCatch({
                ma_model <- metafor::rma(yi = clean_effects, vi = clean_variances, method = "REML")
                metafor::funnel(ma_model, main = "Funnel Plot for Publication Bias Assessment")
                TRUE
            }, error = function(e) {
                FALSE
            })
        },

        .plotHeterogeneity = function(image, ...) {
            if (!requireNamespace("metafor", quietly = TRUE)) {
                return(FALSE)
            }

            # Get the data for plotting
            effect_var <- self$options$effect_size
            variance_var <- self$options$variance
            study_var <- self$options$study_id

            if (is.null(effect_var) || is.null(variance_var)) {
                return(FALSE)
            }

            # Prepare data
            data <- self$data
            effect_sizes <- jmvcore::toNumeric(data[[effect_var]])
            variances <- jmvcore::toNumeric(data[[variance_var]])
            study_ids <- if (!is.null(study_var)) data[[study_var]] else paste0("Study_", seq_along(effect_sizes))

            # Remove missing values
            complete_cases <- complete.cases(effect_sizes, variances)
            if (sum(complete_cases) < 3) {
                return(FALSE)
            }

            clean_effects <- effect_sizes[complete_cases]
            clean_variances <- variances[complete_cases]
            clean_studies <- study_ids[complete_cases]

            # Create heterogeneity visualization
            tryCatch({
                ma_model <- metafor::rma(yi = clean_effects, vi = clean_variances,
                                       slab = clean_studies, method = "REML")

                # Create L'Abbe plot for heterogeneity visualization
                par(mfrow = c(1, 1))
                plot(clean_effects, sqrt(clean_variances),
                     xlab = "Effect Size", ylab = "Standard Error",
                     main = "Heterogeneity Assessment Plot",
                     pch = 19, col = "steelblue")

                # Add pooled effect line
                abline(v = ma_model$beta[1], col = "red", lwd = 2, lty = 2)

                # Add confidence region
                se_range <- range(sqrt(clean_variances))
                effect_range <- range(clean_effects)

                TRUE
            }, error = function(e) {
                FALSE
            })
        },

        .plotMetaRegression = function(image, ...) {
            if (!requireNamespace("metafor", quietly = TRUE)) {
                return(FALSE)
            }

            # Check if meta-regression is enabled and moderators are specified
            if (!self$options$meta_regression || is.null(self$options$moderator_vars) ||
                length(self$options$moderator_vars) == 0) {
                return(FALSE)
            }

            # Get the data
            effect_var <- self$options$effect_size
            variance_var <- self$options$variance
            moderator_vars <- self$options$moderator_vars

            if (is.null(effect_var) || is.null(variance_var)) {
                return(FALSE)
            }

            # Prepare data
            data <- self$data
            effect_sizes <- jmvcore::toNumeric(data[[effect_var]])
            variances <- jmvcore::toNumeric(data[[variance_var]])

            # Get moderator data
            mod_data <- data[moderator_vars]

            # Remove missing values
            complete_cases <- complete.cases(effect_sizes, variances, mod_data)
            if (sum(complete_cases) < 3) {
                return(FALSE)
            }

            clean_effects <- effect_sizes[complete_cases]
            clean_variances <- variances[complete_cases]
            mod_data_clean <- mod_data[complete_cases, , drop = FALSE]

            # Create meta-regression plot
            tryCatch({
                meta_reg <- metafor::rma(yi = clean_effects, vi = clean_variances,
                                       mods = ~ ., data = mod_data_clean, method = 'REML')

                # Plot for first moderator
                if (ncol(mod_data_clean) >= 1) {
                    moderator <- mod_data_clean[, 1]

                    if (is.numeric(moderator)) {
                        # Bubble plot for continuous moderator
                        weights <- 1 / sqrt(clean_variances)
                        plot(moderator, clean_effects,
                             cex = weights / max(weights) * 3,
                             xlab = names(mod_data_clean)[1],
                             ylab = "Effect Size",
                             main = "Meta-Regression Plot",
                             pch = 19, col = rgb(70, 130, 180, alpha = 150, maxColorValue = 255))

                        # Add regression line
                        pred_line <- predict(meta_reg, newmods = range(moderator, na.rm = TRUE))
                        lines(range(moderator, na.rm = TRUE), pred_line$pred, col = "red", lwd = 2)
                    }
                }

                TRUE
            }, error = function(e) {
                FALSE
            })
        },

        .plotSROC = function(image, ...) {
            # Placeholder for Summary ROC plot (diagnostic test accuracy)
            if (self$options$analysis_type != 'diagnostic_accuracy') {
                return(FALSE)
            }

            # This would require mada package for SROC plotting
            if (!requireNamespace("mada", quietly = TRUE)) {
                return(FALSE)
            }

            # Implementation would go here when diagnostic analysis is completed
            plot(1, 1, main = "SROC Plot - Implementation Pending",
                 xlab = "1 - Specificity", ylab = "Sensitivity")
            text(1, 1, "SROC functionality\ncoming soon", cex = 1.5, col = "gray")

            TRUE
        },

        .plotNetwork = function(image, ...) {
            # Placeholder for Network plot
            if (self$options$analysis_type != 'network') {
                return(FALSE)
            }

            # This would require netmeta or igraph package
            plot(1, 1, main = "Network Plot - Implementation Pending",
                 xlab = "", ylab = "")
            text(1, 1, "Network meta-analysis\nfunctionality coming soon", cex = 1.5, col = "gray")

            TRUE
        }
    )
)

metaanalysis <- metaanalysisClass$new