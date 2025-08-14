#' @title Diagnostic Test Meta-Analysis for Pathology
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import mada
#' @import meta
#' @import metafor
#' @importFrom stats qnorm pnorm qt pt
#' @export


diagnosticmetaClass <- R6::R6Class(
    "diagnosticmetaClass",
    inherit = diagnosticmetaBase,
    private = list(
        .init = function() {
            
            # Initialize instructions
            private$.populateInstructions()
            
            # Initialize results tables with proper columns
            private$.initializeResultsTables()
            
        },
        
        .run = function() {
            
            # Check if data is ready
            if (is.null(self$data) || nrow(self$data) == 0) {
                self$results$instructions$setContent(
                    "<p>Welcome to Diagnostic Test Meta-Analysis for Pathology!</p>
                     <p>This module performs comprehensive meta-analysis of diagnostic test accuracy studies.</p>
                     <p>Please provide your data to begin the analysis.</p>"
                )
                return()
            }
            
            # Get variables
            study_var <- self$options$study
            tp_var <- self$options$true_positives
            fp_var <- self$options$false_positives
            fn_var <- self$options$false_negatives
            tn_var <- self$options$true_negatives
            
            # Validate variables
            if (is.null(study_var) || is.null(tp_var) || is.null(fp_var) || 
                is.null(fn_var) || is.null(tn_var)) {
                return()
            }
            
            # Extract data
            data <- self$data
            
            # Validate data completeness
            missing_data <- any(is.na(data[[tp_var]]) | is.na(data[[fp_var]]) | 
                              is.na(data[[fn_var]]) | is.na(data[[tn_var]]))
            
            if (missing_data) {
                self$results$instructions$setContent(
                    "<p><strong>Data Warning:</strong> Missing values detected in diagnostic test data.</p>
                     <p>Please ensure all true positives, false positives, false negatives, and true negatives are complete.</p>"
                )
                return()
            }
            
            # Create diagnostic test data structure
            meta_data <- data.frame(
                study = data[[study_var]],
                tp = as.numeric(data[[tp_var]]),
                fp = as.numeric(data[[fp_var]]),
                fn = as.numeric(data[[fn_var]]),
                tn = as.numeric(data[[tn_var]])
            )
            
            # Remove any rows with zero or negative counts
            meta_data <- meta_data[meta_data$tp >= 0 & meta_data$fp >= 0 & 
                                  meta_data$fn >= 0 & meta_data$tn >= 0, ]
            
            if (nrow(meta_data) < 2) {
                self$results$instructions$setContent(
                    "<p><strong>Error:</strong> Insufficient valid studies for meta-analysis.</p>
                     <p>At least 2 studies with complete diagnostic test data are required.</p>"
                )
                return()
            }
            
            # Perform bivariate meta-analysis
            tryCatch({
                private$.performBivariateMetaAnalysis(meta_data)
            }, error = function(e) {
                self$results$instructions$setContent(
                    paste0("<p><strong>Analysis Error:</strong> ", e$message, "</p>")
                )
            })
            
            # Perform HSROC analysis if requested
            if (self$options$hsroc_analysis) {
                tryCatch({
                    private$.performHSROCAnalysis(meta_data)
                }, error = function(e) {
                    self$results$instructions$setContent(
                        paste0("<p><strong>HSROC Analysis Error:</strong> ", e$message, "</p>")
                    )
                })
            }
            
            # Perform heterogeneity analysis
            if (self$options$heterogeneity_analysis) {
                tryCatch({
                    private$.performHeterogeneityAnalysis(meta_data)
                }, error = function(e) {
                    self$results$instructions$setContent(
                        paste0("<p><strong>Heterogeneity Analysis Error:</strong> ", e$message, "</p>")
                    )
                })
            }
            
            # Perform meta-regression if covariate specified
            if (self$options$meta_regression && !is.null(self$options$covariate)) {
                tryCatch({
                    private$.performMetaRegression(meta_data)
                }, error = function(e) {
                    self$results$instructions$setContent(
                        paste0("<p><strong>Meta-Regression Error:</strong> ", e$message, "</p>")
                    )
                })
            }
            
            # Perform publication bias assessment
            if (self$options$publication_bias) {
                tryCatch({
                    private$.performPublicationBiasAssessment(meta_data)
                }, error = function(e) {
                    self$results$instructions$setContent(
                        paste0("<p><strong>Publication Bias Analysis Error:</strong> ", e$message, "</p>")
                    )
                })
            }
            
            # Generate plots
            if (self$options$forest_plot) {
                private$.populateForestPlot(meta_data)
            }
            
            if (self$options$sroc_plot) {
                private$.populateSROCPlot(meta_data)
            }
            
            if (self$options$funnel_plot && self$options$publication_bias) {
                private$.populateFunnelPlot(meta_data)
            }
            
            # Provide clinical interpretation
            private$.populateInterpretation()
        },
        
        .performBivariateMetaAnalysis = function(meta_data) {
            
            # Calculate sensitivity and specificity for each study
            meta_data$sens <- meta_data$tp / (meta_data$tp + meta_data$fn)
            meta_data$spec <- meta_data$tn / (meta_data$tn + meta_data$fp)
            meta_data$fpr <- 1 - meta_data$spec
            
            # Perform bivariate random-effects meta-analysis using mada package
            if (requireNamespace("mada", quietly = TRUE)) {
                
                # Fit bivariate model
                biv_model <- mada::reitsma(meta_data, correction.control = "single")
                
                # Extract results
                summary_results <- summary(biv_model)
                
                # Populate bivariate results table
                bivariate_table <- self$results$bivariateresults
                
                # Pooled sensitivity
                sens_estimate <- summary_results$coefficients[1, "Estimate"]
                sens_se <- summary_results$coefficients[1, "Std. Error"]
                sens_ci_lower <- sens_estimate - qnorm(0.975) * sens_se
                sens_ci_upper <- sens_estimate + qnorm(0.975) * sens_se
                
                # Transform from logit scale
                pooled_sens <- plogis(sens_estimate)
                pooled_sens_ci_lower <- plogis(sens_ci_lower)
                pooled_sens_ci_upper <- plogis(sens_ci_upper)
                
                # Pooled specificity  
                spec_estimate <- summary_results$coefficients[2, "Estimate"]
                spec_se <- summary_results$coefficients[2, "Std. Error"]
                spec_ci_lower <- spec_estimate - qnorm(0.975) * spec_se
                spec_ci_upper <- spec_estimate + qnorm(0.975) * spec_se
                
                # Transform from logit scale
                pooled_spec <- plogis(spec_estimate)
                pooled_spec_ci_lower <- plogis(spec_ci_lower)
                pooled_spec_ci_upper <- plogis(spec_ci_upper)
                
                # Add rows to table
                bivariate_table$addRow(rowKey = "sensitivity", values = list(
                    parameter = "Pooled Sensitivity",
                    estimate = pooled_sens,
                    ci_lower = pooled_sens_ci_lower,
                    ci_upper = pooled_sens_ci_upper,
                    i_squared = NA_real_,
                    p_value = summary_results$coefficients[1, "Pr(>|z|)"]
                ))
                
                bivariate_table$addRow(rowKey = "specificity", values = list(
                    parameter = "Pooled Specificity", 
                    estimate = pooled_spec,
                    ci_lower = pooled_spec_ci_lower,
                    ci_upper = pooled_spec_ci_upper,
                    i_squared = NA_real_,
                    p_value = summary_results$coefficients[2, "Pr(>|z|)"]
                ))
                
                # Calculate additional diagnostic accuracy measures
                pooled_plr <- pooled_sens / (1 - pooled_spec)
                pooled_nlr <- (1 - pooled_sens) / pooled_spec
                pooled_dor <- pooled_plr / pooled_nlr
                
                bivariate_table$addRow(rowKey = "plr", values = list(
                    parameter = "Positive Likelihood Ratio",
                    estimate = pooled_plr,
                    ci_lower = NA_real_,
                    ci_upper = NA_real_,
                    i_squared = NA_real_,
                    p_value = NA_real_
                ))
                
                bivariate_table$addRow(rowKey = "nlr", values = list(
                    parameter = "Negative Likelihood Ratio",
                    estimate = pooled_nlr,
                    ci_lower = NA_real_,
                    ci_upper = NA_real_,
                    i_squared = NA_real_,
                    p_value = NA_real_
                ))
                
                bivariate_table$addRow(rowKey = "dor", values = list(
                    parameter = "Diagnostic Odds Ratio",
                    estimate = pooled_dor,
                    ci_lower = NA_real_,
                    ci_upper = NA_real_,
                    i_squared = NA_real_,
                    p_value = NA_real_
                ))
            }
        },
        
        .performHSROCAnalysis = function(meta_data) {
            
            if (requireNamespace("mada", quietly = TRUE)) {
                
                # Fit HSROC model
                hsroc_model <- mada::phm(meta_data)
                hsroc_summary <- summary(hsroc_model)
                
                # Populate HSROC results table
                hsroc_table <- self$results$hsrocresults
                
                # Extract HSROC parameters
                coefficients <- hsroc_summary$coefficients
                
                for (i in 1:nrow(coefficients)) {
                    param_name <- rownames(coefficients)[i]
                    
                    hsroc_table$addRow(rowKey = param_name, values = list(
                        parameter = param_name,
                        estimate = coefficients[i, "Estimate"],
                        std_error = coefficients[i, "Std. Error"],
                        z_value = coefficients[i, "z value"],
                        p_value = coefficients[i, "Pr(>|z|)"]
                    ))
                }
            }
        },
        
        .performHeterogeneityAnalysis = function(meta_data) {
            
            # Perform separate meta-analyses for sensitivity and specificity
            if (requireNamespace("metafor", quietly = TRUE)) {
                
                # Calculate logit transformed sensitivity and specificity
                meta_data$logit_sens <- qlogis(meta_data$sens)
                meta_data$logit_spec <- qlogis(meta_data$spec)
                
                # Calculate sampling variances
                meta_data$var_logit_sens <- 1/(meta_data$tp) + 1/(meta_data$fn)
                meta_data$var_logit_spec <- 1/(meta_data$tn) + 1/(meta_data$fp)
                
                # Meta-analysis for sensitivity
                sens_meta <- metafor::rma(yi = logit_sens, vi = var_logit_sens, 
                                        data = meta_data, method = "REML")
                
                # Meta-analysis for specificity
                spec_meta <- metafor::rma(yi = logit_spec, vi = var_logit_spec, 
                                        data = meta_data, method = "REML")
                
                # Populate heterogeneity table
                het_table <- self$results$heterogeneity
                
                het_table$addRow(rowKey = "sensitivity", values = list(
                    measure = "Sensitivity",
                    q_statistic = sens_meta$QE,
                    df = sens_meta$k - 1,
                    p_value = sens_meta$QEp,
                    i_squared = max(0, (sens_meta$QE - (sens_meta$k - 1)) / sens_meta$QE * 100),
                    tau_squared = sens_meta$tau2
                ))
                
                het_table$addRow(rowKey = "specificity", values = list(
                    measure = "Specificity",
                    q_statistic = spec_meta$QE,
                    df = spec_meta$k - 1,
                    p_value = spec_meta$QEp,
                    i_squared = max(0, (spec_meta$QE - (spec_meta$k - 1)) / spec_meta$QE * 100),
                    tau_squared = spec_meta$tau2
                ))
            }
        },
        
        .performMetaRegression = function(meta_data) {
            
            covariate_var <- self$options$covariate
            if (is.null(covariate_var)) return()
            
            # Add covariate to meta_data
            meta_data$covariate <- self$data[[covariate_var]]
            
            if (requireNamespace("metafor", quietly = TRUE)) {
                
                # Meta-regression for sensitivity
                sens_metareg <- metafor::rma(yi = logit_sens, vi = var_logit_sens, 
                                           mods = ~ covariate, data = meta_data, method = "REML")
                
                # Meta-regression for specificity
                spec_metareg <- metafor::rma(yi = logit_spec, vi = var_logit_spec, 
                                           mods = ~ covariate, data = meta_data, method = "REML")
                
                # Populate meta-regression table
                metareg_table <- self$results$metaregression
                
                metareg_table$addRow(rowKey = "sens_intercept", values = list(
                    measure = "Sensitivity",
                    parameter = "Intercept",
                    estimate = sens_metareg$beta[1],
                    std_error = sens_metareg$se[1],
                    z_value = sens_metareg$zval[1],
                    p_value = sens_metareg$pval[1]
                ))
                
                metareg_table$addRow(rowKey = "sens_covariate", values = list(
                    measure = "Sensitivity",
                    parameter = covariate_var,
                    estimate = sens_metareg$beta[2],
                    std_error = sens_metareg$se[2],
                    z_value = sens_metareg$zval[2],
                    p_value = sens_metareg$pval[2]
                ))
                
                metareg_table$addRow(rowKey = "spec_intercept", values = list(
                    measure = "Specificity",
                    parameter = "Intercept",
                    estimate = spec_metareg$beta[1],
                    std_error = spec_metareg$se[1],
                    z_value = spec_metareg$zval[1],
                    p_value = spec_metareg$pval[1]
                ))
                
                metareg_table$addRow(rowKey = "spec_covariate", values = list(
                    measure = "Specificity",
                    parameter = covariate_var,
                    estimate = spec_metareg$beta[2],
                    std_error = spec_metareg$se[2],
                    z_value = spec_metareg$zval[2],
                    p_value = spec_metareg$pval[2]
                ))
            }
        },
        
        .performPublicationBiasAssessment = function(meta_data) {
            
            if (requireNamespace("metafor", quietly = TRUE)) {
                
                # Calculate effective sample sizes
                meta_data$n_total <- meta_data$tp + meta_data$fp + meta_data$fn + meta_data$tn
                
                # Deeks' funnel plot asymmetry test
                meta_data$log_dor <- log((meta_data$tp * meta_data$tn) / (meta_data$fp * meta_data$fn))
                meta_data$se_log_dor <- sqrt(1/meta_data$tp + 1/meta_data$fp + 1/meta_data$fn + 1/meta_data$tn)
                
                # Regression test for asymmetry
                deeks_test <- metafor::rma(yi = log_dor, vi = se_log_dor^2, 
                                         mods = ~ I(1/sqrt(n_total)), data = meta_data, method = "FE")
                
                # Populate publication bias table
                bias_table <- self$results$publicationbias
                
                bias_table$addRow(rowKey = "deeks_test", values = list(
                    test = "Deeks' Funnel Plot Asymmetry Test",
                    statistic = deeks_test$zval[2],
                    p_value = deeks_test$pval[2],
                    interpretation = ifelse(deeks_test$pval[2] < 0.05, 
                                          "Significant asymmetry detected", 
                                          "No significant asymmetry")
                ))
            }
        },
        
        .populateForestPlot = function(meta_data) {
            
            image <- self$results$forestplot
            image$setState(meta_data)
        },
        
        .plotForestPlot = function(image, ggtheme, theme, ...) {
            
            meta_data <- image$state
            
            if (requireNamespace("mada", quietly = TRUE) && 
                requireNamespace("ggplot2", quietly = TRUE)) {
                
                # Create forest plot for sensitivity
                meta_data$sens <- meta_data$tp / (meta_data$tp + meta_data$fn)
                meta_data$spec <- meta_data$tn / (meta_data$tn + meta_data$fp)
                
                # Calculate confidence intervals for sensitivity
                meta_data$sens_ci_lower <- with(meta_data, 
                    pmax(0, sens - qnorm(0.975) * sqrt(sens * (1-sens) / (tp + fn))))
                meta_data$sens_ci_upper <- with(meta_data, 
                    pmin(1, sens + qnorm(0.975) * sqrt(sens * (1-sens) / (tp + fn))))
                
                # Create forest plot
                p <- ggplot2::ggplot(meta_data, ggplot2::aes(x = sens, y = reorder(study, sens))) +
                    ggplot2::geom_point(size = 3, color = "darkblue") +
                    ggplot2::geom_errorbarh(ggplot2::aes(xmin = sens_ci_lower, xmax = sens_ci_upper), 
                                          height = 0.2, color = "darkblue") +
                    ggplot2::labs(
                        title = "Forest Plot: Sensitivity by Study",
                        x = "Sensitivity",
                        y = "Study"
                    ) +
                    ggplot2::xlim(0, 1) +
                    ggtheme +
                    ggplot2::theme(
                        plot.title = ggplot2::element_text(size = 14, face = "bold"),
                        axis.title = ggplot2::element_text(size = 12),
                        axis.text = ggplot2::element_text(size = 10)
                    )
                
                print(p)
                TRUE
            } else {
                FALSE
            }
        },
        
        .populateSROCPlot = function(meta_data) {
            
            image <- self$results$srocplot
            image$setState(meta_data)
        },
        
        .plotSROCPlot = function(image, ggtheme, theme, ...) {
            
            meta_data <- image$state
            
            if (requireNamespace("mada", quietly = TRUE) && 
                requireNamespace("ggplot2", quietly = TRUE)) {
                
                # Calculate sensitivity and specificity
                meta_data$sens <- meta_data$tp / (meta_data$tp + meta_data$fn)
                meta_data$fpr <- meta_data$fp / (meta_data$fp + meta_data$tn)
                
                # Create SROC plot
                p <- ggplot2::ggplot(meta_data, ggplot2::aes(x = fpr, y = sens)) +
                    ggplot2::geom_point(size = 3, alpha = 0.7, color = "darkred") +
                    ggplot2::geom_abline(intercept = 1, slope = -1, linetype = "dashed", 
                                       color = "gray50", alpha = 0.5) +
                    ggplot2::labs(
                        title = "Summary ROC Plot",
                        x = "False Positive Rate (1 - Specificity)",
                        y = "True Positive Rate (Sensitivity)"
                    ) +
                    ggplot2::xlim(0, 1) +
                    ggplot2::ylim(0, 1) +
                    ggtheme +
                    ggplot2::theme(
                        plot.title = ggplot2::element_text(size = 14, face = "bold"),
                        axis.title = ggplot2::element_text(size = 12),
                        axis.text = ggplot2::element_text(size = 10)
                    )
                
                print(p)
                TRUE
            } else {
                FALSE
            }
        },
        
        .populateFunnelPlot = function(meta_data) {
            
            image <- self$results$funnelplot
            image$setState(meta_data)
        },
        
        .plotFunnelPlot = function(image, ggtheme, theme, ...) {
            
            meta_data <- image$state
            
            if (requireNamespace("ggplot2", quietly = TRUE)) {
                
                # Calculate diagnostic odds ratio and standard error
                meta_data$log_dor <- log((meta_data$tp * meta_data$tn) / (meta_data$fp * meta_data$fn))
                meta_data$se_log_dor <- sqrt(1/meta_data$tp + 1/meta_data$fp + 1/meta_data$fn + 1/meta_data$tn)
                
                # Create funnel plot
                p <- ggplot2::ggplot(meta_data, ggplot2::aes(x = log_dor, y = 1/se_log_dor)) +
                    ggplot2::geom_point(size = 3, alpha = 0.7, color = "darkgreen") +
                    ggplot2::labs(
                        title = "Funnel Plot: Publication Bias Assessment",
                        x = "Log Diagnostic Odds Ratio",
                        y = "Precision (1/SE)"
                    ) +
                    ggtheme +
                    ggplot2::theme(
                        plot.title = ggplot2::element_text(size = 14, face = "bold"),
                        axis.title = ggplot2::element_text(size = 12),
                        axis.text = ggplot2::element_text(size = 10)
                    )
                
                print(p)
                TRUE
            } else {
                FALSE
            }
        },
        
        .populateInstructions = function() {
            
            html <- "
            <h2>Diagnostic Test Meta-Analysis for Pathology</h2>
            
            <h3>Purpose</h3>
            <p>This module performs comprehensive meta-analysis of diagnostic test accuracy studies, specifically designed for pathology research including AI algorithm validation and biomarker diagnostic accuracy synthesis.</p>
            
            <h3>Required Data Structure</h3>
            <p>Your data should contain the following variables:</p>
            <ul>
                <li><strong>Study identifier:</strong> Unique name or ID for each study</li>
                <li><strong>True positives (TP):</strong> Number correctly identified as positive</li>
                <li><strong>False positives (FP):</strong> Number incorrectly identified as positive</li>
                <li><strong>False negatives (FN):</strong> Number incorrectly identified as negative</li>
                <li><strong>True negatives (TN):</strong> Number correctly identified as negative</li>
            </ul>
            
            <h3>Analysis Methods</h3>
            <ul>
                <li><strong>Bivariate Random-Effects Model:</strong> Jointly analyzes sensitivity and specificity accounting for correlation</li>
                <li><strong>HSROC Analysis:</strong> Hierarchical summary ROC curve modeling</li>
                <li><strong>Meta-Regression:</strong> Investigates sources of heterogeneity using study-level covariates</li>
                <li><strong>Publication Bias Assessment:</strong> Deeks' funnel plot asymmetry test</li>
            </ul>
            
            <h3>Clinical Applications</h3>
            <ul>
                <li>AI algorithm performance meta-analysis across multiple validation studies</li>
                <li>Biomarker diagnostic accuracy synthesis for clinical implementation</li>
                <li>Comparison of diagnostic methods across different populations</li>
                <li>Assessment of test performance heterogeneity and sources of variation</li>
            </ul>
            
            <h3>Interpretation Guidelines</h3>
            <p>Focus on:</p>
            <ul>
                <li>Pooled sensitivity and specificity with confidence intervals</li>
                <li>Likelihood ratios for clinical decision-making</li>
                <li>Heterogeneity assessment (I² > 50% indicates substantial heterogeneity)</li>
                <li>Publication bias evaluation for evidence quality</li>
            </ul>
            "
            
            self$results$instructions$setContent(html)
        },
        
        .initializeResultsTables = function() {
            
            # Bivariate results table
            bivariate_table <- self$results$bivariateresults
            bivariate_table$addColumn(name = "parameter", title = "Parameter", type = "text")
            bivariate_table$addColumn(name = "estimate", title = "Estimate", type = "number", format = "zto,pvalue")
            bivariate_table$addColumn(name = "ci_lower", title = "CI Lower", type = "number", format = "zto,pvalue")
            bivariate_table$addColumn(name = "ci_upper", title = "CI Upper", type = "number", format = "zto,pvalue")
            bivariate_table$addColumn(name = "i_squared", title = "I²", type = "number", format = "pc")
            bivariate_table$addColumn(name = "p_value", title = "p", type = "number", format = "zto,pvalue")
            
            # HSROC results table
            hsroc_table <- self$results$hsrocresults
            hsroc_table$addColumn(name = "parameter", title = "Parameter", type = "text")
            hsroc_table$addColumn(name = "estimate", title = "Estimate", type = "number")
            hsroc_table$addColumn(name = "std_error", title = "SE", type = "number")
            hsroc_table$addColumn(name = "z_value", title = "z", type = "number")
            hsroc_table$addColumn(name = "p_value", title = "p", type = "number", format = "zto,pvalue")
            
            # Heterogeneity table
            het_table <- self$results$heterogeneity
            het_table$addColumn(name = "measure", title = "Measure", type = "text")
            het_table$addColumn(name = "q_statistic", title = "Q", type = "number")
            het_table$addColumn(name = "df", title = "df", type = "integer")
            het_table$addColumn(name = "p_value", title = "p", type = "number", format = "zto,pvalue")
            het_table$addColumn(name = "i_squared", title = "I²", type = "number", format = "pc")
            het_table$addColumn(name = "tau_squared", title = "τ²", type = "number")
            
            # Meta-regression table
            metareg_table <- self$results$metaregression
            metareg_table$addColumn(name = "measure", title = "Measure", type = "text")
            metareg_table$addColumn(name = "parameter", title = "Parameter", type = "text")
            metareg_table$addColumn(name = "estimate", title = "Estimate", type = "number")
            metareg_table$addColumn(name = "std_error", title = "SE", type = "number")
            metareg_table$addColumn(name = "z_value", title = "z", type = "number")
            metareg_table$addColumn(name = "p_value", title = "p", type = "number", format = "zto,pvalue")
            
            # Publication bias table
            bias_table <- self$results$publicationbias
            bias_table$addColumn(name = "test", title = "Test", type = "text")
            bias_table$addColumn(name = "statistic", title = "Statistic", type = "number")
            bias_table$addColumn(name = "p_value", title = "p", type = "number", format = "zto,pvalue")
            bias_table$addColumn(name = "interpretation", title = "Interpretation", type = "text")
        },
        
        .populateInterpretation = function() {
            
            html <- "
            <h2>Clinical Interpretation Guidelines</h2>
            
            <h3>Meta-Analysis Results Interpretation</h3>
            <ul>
                <li><strong>Pooled Sensitivity:</strong> Proportion of true positives correctly identified (higher is better for screening)</li>
                <li><strong>Pooled Specificity:</strong> Proportion of true negatives correctly identified (higher is better for confirmation)</li>
                <li><strong>Positive Likelihood Ratio (PLR):</strong> How much the odds of disease increase with a positive test (>10 = strong evidence)</li>
                <li><strong>Negative Likelihood Ratio (NLR):</strong> How much the odds of disease decrease with a negative test (<0.1 = strong evidence)</li>
            </ul>
            
            <h3>Heterogeneity Assessment</h3>
            <ul>
                <li><strong>I² < 25%:</strong> Low heterogeneity - results can be pooled</li>
                <li><strong>I² 25-50%:</strong> Moderate heterogeneity - investigate sources</li>
                <li><strong>I² > 50%:</strong> Substantial heterogeneity - pooling may not be appropriate</li>
            </ul>
            
            <h3>Publication Bias Evaluation</h3>
            <ul>
                <li><strong>Deeks' Test p < 0.05:</strong> Significant asymmetry suggests publication bias</li>
                <li><strong>Funnel Plot:</strong> Visual assessment of study distribution symmetry</li>
            </ul>
            
            <h3>Clinical Decision Making</h3>
            <ul>
                <li>Consider both sensitivity and specificity for clinical utility</li>
                <li>High sensitivity tests are preferred for screening populations</li>
                <li>High specificity tests are preferred for confirming diagnoses</li>
                <li>Likelihood ratios help quantify the clinical impact of test results</li>
            </ul>
            
            <h3>AI Algorithm Validation Context</h3>
            <ul>
                <li>Meta-analysis provides robust evidence for algorithm performance</li>
                <li>Heterogeneity may indicate variability across different datasets or populations</li>
                <li>Publication bias assessment is crucial for unbiased performance estimates</li>
            </ul>
            "
            
            self$results$interpretation$setContent(html)
        }
    )
)