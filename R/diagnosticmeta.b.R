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
            <p><strong>Essential Variables (Required):</strong></p>
            <ul>
                <li><strong>Study identifier:</strong> Unique name or ID for each study (e.g., 'Smith_2020', 'Study_1')</li>
                <li><strong>True positives (TP):</strong> Number correctly identified as positive</li>
                <li><strong>False positives (FP):</strong> Number incorrectly identified as positive</li>
                <li><strong>False negatives (FN):</strong> Number incorrectly identified as negative</li>
                <li><strong>True negatives (TN):</strong> Number correctly identified as negative</li>
            </ul>
            
            <p><strong>Optional Variables for Meta-Regression:</strong></p>
            <ul>
                <li><strong>Patient population:</strong> Disease stage, demographics (e.g., 'early_stage', 'advanced', 'mixed')</li>
                <li><strong>Technical method:</strong> Staining protocol (e.g., 'automated', 'manual')</li>
                <li><strong>Geographic region:</strong> Study location for population analysis</li>
                <li><strong>Publication year:</strong> For temporal trend investigation</li>
            </ul>
            
            <h3>Data Preparation Checklist</h3>
            <div style='background-color: #f8f9fa; padding: 15px; border-left: 4px solid #007bff; margin: 10px 0;'>
                <p><strong>Before running analysis, verify:</strong></p>
                <ul>
                    <li>✅ No missing values in TP, FP, FN, TN columns</li>
                    <li>✅ All values are non-negative integers</li>
                    <li>✅ At least 2 studies with complete data</li>
                    <li>✅ Study identifiers are unique</li>
                    <li>✅ Sample sizes are realistic (TP+FP+FN+TN = total cases per study)</li>
                </ul>
            </div>
            
            <h3>Example Data Format</h3>
            <table style='border-collapse: collapse; width: 100%; margin: 10px 0;'>
                <tr style='background-color: #f1f1f1;'>
                    <th style='border: 1px solid #ddd; padding: 8px;'>study_name</th>
                    <th style='border: 1px solid #ddd; padding: 8px;'>true_positives</th>
                    <th style='border: 1px solid #ddd; padding: 8px;'>false_positives</th>
                    <th style='border: 1px solid #ddd; padding: 8px;'>false_negatives</th>
                    <th style='border: 1px solid #ddd; padding: 8px;'>true_negatives</th>
                    <th style='border: 1px solid #ddd; padding: 8px;'>population</th>
                </tr>
                <tr>
                    <td style='border: 1px solid #ddd; padding: 8px;'>Smith_2020</td>
                    <td style='border: 1px solid #ddd; padding: 8px;'>47</td>
                    <td style='border: 1px solid #ddd; padding: 8px;'>101</td>
                    <td style='border: 1px solid #ddd; padding: 8px;'>9</td>
                    <td style='border: 1px solid #ddd; padding: 8px;'>738</td>
                    <td style='border: 1px solid #ddd; padding: 8px;'>mixed</td>
                </tr>
                <tr>
                    <td style='border: 1px solid #ddd; padding: 8px;'>Johnson_2021</td>
                    <td style='border: 1px solid #ddd; padding: 8px;'>126</td>
                    <td style='border: 1px solid #ddd; padding: 8px;'>272</td>
                    <td style='border: 1px solid #ddd; padding: 8px;'>51</td>
                    <td style='border: 1px solid #ddd; padding: 8px;'>1543</td>
                    <td style='border: 1px solid #ddd; padding: 8px;'>early_stage</td>
                </tr>
            </table>
            
            <h3>Analysis Methods</h3>
            <ul>
                <li><strong>Bivariate Random-Effects Model (Recommended):</strong> Jointly analyzes sensitivity and specificity accounting for correlation</li>
                <li><strong>HSROC Analysis:</strong> Hierarchical summary ROC curve modeling</li>
                <li><strong>Meta-Regression:</strong> Investigates sources of heterogeneity using study-level covariates</li>
                <li><strong>Publication Bias Assessment:</strong> Deeks' funnel plot asymmetry test</li>
            </ul>
            
            <h3>Clinical Applications</h3>
            <ul>
                <li>IHC marker validation across multiple pathology studies</li>
                <li>AI algorithm performance meta-analysis for clinical implementation</li>
                <li>Biomarker diagnostic accuracy synthesis for guideline development</li>
                <li>Cross-population comparison of diagnostic test performance</li>
                <li>Assessment of test performance heterogeneity and variation sources</li>
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
            
            <h3>📊 Primary Results Interpretation</h3>
            
            <h4>Pooled Sensitivity and Specificity</h4>
            <ul>
                <li><strong>Pooled Sensitivity:</strong> Proportion of diseased cases correctly identified
                    <ul>
                        <li>High sensitivity (>90%): Excellent for screening - few diseased cases missed</li>
                        <li>Moderate sensitivity (80-90%): Good for screening with acceptable miss rate</li>
                        <li>Low sensitivity (<80%): Limited screening utility - many cases missed</li>
                    </ul>
                </li>
                <li><strong>Pooled Specificity:</strong> Proportion of non-diseased cases correctly identified
                    <ul>
                        <li>High specificity (>90%): Excellent for confirmation - few false alarms</li>
                        <li>Moderate specificity (80-90%): Good confirmatory value with some false positives</li>
                        <li>Low specificity (<80%): Limited confirmatory utility - many false alarms</li>
                    </ul>
                </li>
            </ul>
            
            <h4>Likelihood Ratios for Clinical Decision-Making</h4>
            <table style='border-collapse: collapse; width: 100%; margin: 10px 0;'>
                <tr style='background-color: #f1f1f1;'>
                    <th style='border: 1px solid #ddd; padding: 8px;'>Likelihood Ratio</th>
                    <th style='border: 1px solid #ddd; padding: 8px;'>Value Range</th>
                    <th style='border: 1px solid #ddd; padding: 8px;'>Clinical Interpretation</th>
                </tr>
                <tr>
                    <td style='border: 1px solid #ddd; padding: 8px;'>Positive LR</td>
                    <td style='border: 1px solid #ddd; padding: 8px;'>>10</td>
                    <td style='border: 1px solid #ddd; padding: 8px;'>Strong evidence FOR disease when test positive</td>
                </tr>
                <tr>
                    <td style='border: 1px solid #ddd; padding: 8px;'>Positive LR</td>
                    <td style='border: 1px solid #ddd; padding: 8px;'>5-10</td>
                    <td style='border: 1px solid #ddd; padding: 8px;'>Moderate evidence for disease</td>
                </tr>
                <tr>
                    <td style='border: 1px solid #ddd; padding: 8px;'>Positive LR</td>
                    <td style='border: 1px solid #ddd; padding: 8px;'>2-5</td>
                    <td style='border: 1px solid #ddd; padding: 8px;'>Weak evidence for disease</td>
                </tr>
                <tr>
                    <td style='border: 1px solid #ddd; padding: 8px;'>Negative LR</td>
                    <td style='border: 1px solid #ddd; padding: 8px;'><0.1</td>
                    <td style='border: 1px solid #ddd; padding: 8px;'>Strong evidence AGAINST disease when test negative</td>
                </tr>
                <tr>
                    <td style='border: 1px solid #ddd; padding: 8px;'>Negative LR</td>
                    <td style='border: 1px solid #ddd; padding: 8px;'>0.1-0.2</td>
                    <td style='border: 1px solid #ddd; padding: 8px;'>Moderate evidence against disease</td>
                </tr>
                <tr>
                    <td style='border: 1px solid #ddd; padding: 8px;'>Negative LR</td>
                    <td style='border: 1px solid #ddd; padding: 8px;'>0.2-0.5</td>
                    <td style='border: 1px solid #ddd; padding: 8px;'>Weak evidence against disease</td>
                </tr>
            </table>
            
            <h4>Diagnostic Odds Ratio (DOR)</h4>
            <ul>
                <li><strong>DOR > 25:</strong> Excellent overall discriminative ability</li>
                <li><strong>DOR 10-25:</strong> Good discriminative ability</li>
                <li><strong>DOR 5-10:</strong> Moderate discriminative ability</li>
                <li><strong>DOR < 5:</strong> Limited discriminative ability</li>
            </ul>
            
            <h3>🔍 Heterogeneity Assessment</h3>
            
            <div style='background-color: #fff3cd; padding: 15px; border-left: 4px solid #ffc107; margin: 10px 0;'>
                <h4>I² Statistic Interpretation:</h4>
                <ul>
                    <li><strong>I² < 25%:</strong> Low heterogeneity - results can be reliably pooled</li>
                    <li><strong>I² 25-50%:</strong> Moderate heterogeneity - investigate potential sources</li>
                    <li><strong>I² 50-75%:</strong> Substantial heterogeneity - pooling questionable</li>
                    <li><strong>I² > 75%:</strong> Considerable heterogeneity - avoid pooling, use subgroup analysis</li>
                </ul>
            </div>
            
            <h4>Common Sources of Heterogeneity:</h4>
            <ul>
                <li><strong>Patient Population:</strong> Disease stage, severity, demographics</li>
                <li><strong>Technical Factors:</strong> Staining protocols, antibody sources, automation</li>
                <li><strong>Methodological:</strong> Reference standards, blinding, cut-off thresholds</li>
                <li><strong>Geographic/Temporal:</strong> Population differences, technology evolution</li>
            </ul>
            
            <h3>📈 Publication Bias Assessment</h3>
            
            <h4>Deeks' Funnel Plot Test:</h4>
            <ul>
                <li><strong>p ≥ 0.05:</strong> No significant asymmetry - low risk of publication bias</li>
                <li><strong>p < 0.05:</strong> Significant asymmetry - potential publication bias detected</li>
            </ul>
            
            <div style='background-color: #f8d7da; padding: 15px; border-left: 4px solid #dc3545; margin: 10px 0;'>
                <p><strong>⚠️ When Publication Bias is Detected:</strong></p>
                <ul>
                    <li>Pooled estimates may be overoptimistic</li>
                    <li>Search for unpublished studies or negative results</li>
                    <li>Consider contacting study authors for additional data</li>
                    <li>Report limitations and interpret results cautiously</li>
                </ul>
            </div>
            
            <h3>🏥 Clinical Application Guidance</h3>
            
            <h4>IHC Marker Validation:</h4>
            <ul>
                <li><strong>Screening Applications:</strong> Prioritize high sensitivity (≥90%)</li>
                <li><strong>Confirmatory Testing:</strong> Prioritize high specificity (≥90%)</li>
                <li><strong>Balanced Performance:</strong> Consider clinical costs of false positives vs false negatives</li>
            </ul>
            
            <h4>AI Algorithm Implementation:</h4>
            <ul>
                <li><strong>Consistent Performance:</strong> Low heterogeneity supports broad implementation</li>
                <li><strong>Variable Performance:</strong> High heterogeneity suggests population-specific validation needed</li>
                <li><strong>External Validation:</strong> Meta-analysis provides evidence for regulatory approval</li>
            </ul>
            
            <h4>Predictive Values in Clinical Practice:</h4>
            <p><strong>Important:</strong> Sensitivity and specificity are test characteristics, but clinicians need predictive values that depend on disease prevalence in their population.</p>
            
            <table style='border-collapse: collapse; width: 100%; margin: 10px 0;'>
                <tr style='background-color: #f1f1f1;'>
                    <th style='border: 1px solid #ddd; padding: 8px;'>Disease Prevalence</th>
                    <th style='border: 1px solid #ddd; padding: 8px;'>PPV (Sen=90%, Spe=80%)</th>
                    <th style='border: 1px solid #ddd; padding: 8px;'>NPV (Sen=90%, Spe=80%)</th>
                </tr>
                <tr>
                    <td style='border: 1px solid #ddd; padding: 8px;'>5%</td>
                    <td style='border: 1px solid #ddd; padding: 8px;'>19%</td>
                    <td style='border: 1px solid #ddd; padding: 8px;'>99%</td>
                </tr>
                <tr>
                    <td style='border: 1px solid #ddd; padding: 8px;'>20%</td>
                    <td style='border: 1px solid #ddd; padding: 8px;'>53%</td>
                    <td style='border: 1px solid #ddd; padding: 8px;'>97%</td>
                </tr>
                <tr>
                    <td style='border: 1px solid #ddd; padding: 8px;'>50%</td>
                    <td style='border: 1px solid #ddd; padding: 8px;'>82%</td>
                    <td style='border: 1px solid #ddd; padding: 8px;'>89%</td>
                </tr>
            </table>
            
            <h3>📋 Reporting Recommendations</h3>
            
            <p>When reporting your meta-analysis results, include:</p>
            <ul>
                <li>✅ <strong>Study Selection:</strong> Number of studies included and excluded</li>
                <li>✅ <strong>Pooled Estimates:</strong> Sensitivity and specificity with 95% confidence intervals</li>
                <li>✅ <strong>Likelihood Ratios:</strong> For clinical decision-making context</li>
                <li>✅ <strong>Heterogeneity:</strong> I² values and potential sources investigated</li>
                <li>✅ <strong>Publication Bias:</strong> Deeks' test results and visual assessment</li>
                <li>✅ <strong>Clinical Implications:</strong> Population-specific predictive values</li>
                <li>✅ <strong>Limitations:</strong> Study quality, missing data, generalizability</li>
            </ul>
            
            <div style='background-color: #d1ecf1; padding: 15px; border-left: 4px solid #17a2b8; margin: 10px 0;'>
                <p><strong>💡 Pro Tip:</strong> Always interpret meta-analysis results in the context of your specific clinical population and intended use. A test excellent for one application may be inappropriate for another.</p>
            </div>
            "
            
            self$results$interpretation$setContent(html)
        }
    )
)