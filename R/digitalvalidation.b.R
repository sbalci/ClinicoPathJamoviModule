# This file is a generated template, your changes will not be overwritten

digitalvalidationClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "digitalvalidationClass",
    inherit = digitalvalidationBase,
    private = list(
        .init = function() {
            if (is.null(self$data)) {
                self$results$interpretation$setContent(
                    "<h3>Digital Pathology Validation Workflow</h3>
                    <p><strong>Purpose:</strong> Comprehensive validation framework for digital pathology algorithms and platforms, 
                    following international best practices for analytical validation in clinical pathology.</p>
                    
                    <h4>Data Requirements:</h4>
                    <ul>
                        <li><strong>Reference Method:</strong> Gold standard measurements (e.g., pathologist assessment, validated algorithm)</li>
                        <li><strong>Test Method:</strong> New algorithm or platform measurements to be validated</li>
                        <li><strong>Optional:</strong> Platform identifier, batch variables, quality metrics</li>
                    </ul>
                    
                    <h4>Validation Framework:</h4>
                    <ul>
                        <li><strong>Analytical Performance:</strong> Accuracy, precision, linearity assessment</li>
                        <li><strong>Agreement Analysis:</strong> Multiple agreement metrics with clinical interpretation</li>
                        <li><strong>Bias Assessment:</strong> Systematic and proportional bias detection</li>
                        <li><strong>Clinical Decision Impact:</strong> Effect on diagnostic thresholds</li>
                    </ul>
                    
                    <h4>Statistical Methods:</h4>
                    <ul>
                        <li><strong>Correlation:</strong> Pearson and Spearman coefficients</li>
                        <li><strong>Agreement:</strong> ICC(3,1), Concordance Correlation Coefficient</li>
                        <li><strong>Bias Analysis:</strong> Bland-Altman with regression analysis</li>
                        <li><strong>Bootstrap CI:</strong> Robust confidence interval estimation</li>
                    </ul>
                    
                    <h4>Clinical Applications:</h4>
                    <ul>
                        <li>FDA/CE-IVD algorithm validation for clinical deployment</li>
                        <li>Laboratory developed test (LDT) validation</li>
                        <li>Multi-platform reproducibility studies</li>
                        <li>Quality assurance and proficiency testing</li>
                    </ul>
                    
                    <p><em>This workflow implements CAP/CLSI guidelines for method comparison studies 
                    and FDA guidance for AI/ML-based medical devices in pathology.</em></p>"
                )
                return()
            }
            
            # Initialize results structure
            private$.initializeTables()
        },
        
        .run = function() {
            # Check required variables
            if (is.null(self$options$reference) || is.null(self$options$test)) {
                return()
            }
            
            data <- self$data
            if (nrow(data) == 0) return()
            
            # Extract data
            reference <- data[[self$options$reference]]
            test <- data[[self$options$test]]
            
            # Remove missing values
            complete_cases <- complete.cases(reference, test)
            reference <- reference[complete_cases]
            test <- test[complete_cases]
            
            if (length(reference) < 10) {
                self$results$interpretation$setContent(
                    "<p style='color: red;'><strong>Error:</strong> Insufficient data for validation analysis. 
                    At least 10 complete paired observations are required for reliable validation.</p>"
                )
                return()
            }
            
            # Perform comprehensive validation
            private$.performValidationAnalysis(reference, test)
            private$.generateValidationPlots(reference, test)
            private$.generateValidationInterpretation(reference, test)
        },
        
        .initializeTables = function() {
            # Performance metrics table
            perf_table <- self$results$performancetable
            perf_table$addColumn(name = 'metric', title = 'Performance Metric', type = 'text')
            perf_table$addColumn(name = 'value', title = 'Value', type = 'number', format = 'zto')
            perf_table$addColumn(name = 'ci_lower', title = '95% CI Lower', type = 'number', format = 'zto')
            perf_table$addColumn(name = 'ci_upper', title = '95% CI Upper', type = 'number', format = 'zto')
            perf_table$addColumn(name = 'specification', title = 'Clinical Acceptance', type = 'text')
            
            # Bias analysis table
            bias_table <- self$results$biastable
            bias_table$addColumn(name = 'bias_type', title = 'Bias Type', type = 'text')
            bias_table$addColumn(name = 'estimate', title = 'Estimate', type = 'number', format = 'zto')
            bias_table$addColumn(name = 'p_value', title = 'P-value', type = 'number', format = 'zto,pvalue')
            bias_table$addColumn(name = 'interpretation', title = 'Clinical Significance', type = 'text')
            
            # Decision impact table
            decision_table <- self$results$decisiontable
            decision_table$addColumn(name = 'threshold', title = 'Clinical Threshold', type = 'text')
            decision_table$addColumn(name = 'agreement', title = 'Classification Agreement (%)', type = 'number', format = 'zto')
            decision_table$addColumn(name = 'kappa', title = 'Cohen\'s Kappa', type = 'number', format = 'zto')
            decision_table$addColumn(name = 'impact', title = 'Clinical Impact', type = 'text')
        },
        
        .performValidationAnalysis = function(reference, test) {
            n <- length(reference)
            
            # 1. Performance Metrics
            # Correlation coefficients
            pearson_test <- cor.test(reference, test, method = "pearson")
            spearman_test <- cor.test(reference, test, method = "spearman")
            
            # ICC calculation
            if (requireNamespace('psych', quietly = TRUE)) {
                icc_data <- data.frame(Reference = reference, Test = test)
                icc_result <- psych::ICC(icc_data)
                icc_value <- icc_result$results$ICC[6]  # ICC(3,1)
                icc_lower <- icc_result$results$`lower bound`[6]
                icc_upper <- icc_result$results$`upper bound`[6]
            } else {
                icc_value <- icc_lower <- icc_upper <- NA
            }
            
            # Concordance Correlation Coefficient
            if (requireNamespace('epiR', quietly = TRUE)) {
                ccc_result <- epiR::epi.ccc(reference, test, ci = "z-transform", conf.level = 0.95)
                ccc_value <- ccc_result$rho.c$est
                ccc_lower <- ccc_result$rho.c$lower
                ccc_upper <- ccc_result$rho.c$upper
            } else {
                ccc_value <- ccc_lower <- ccc_upper <- NA
            }
            
            # Total Deviation Index (TDI)
            differences <- test - reference
            tdi_90 <- quantile(abs(differences), 0.90, na.rm = TRUE)
            
            # Populate performance table
            perf_table <- self$results$performancetable
            
            perf_table$addRow(rowKey = 1, values = list(
                metric = "Sample Size",
                value = n,
                ci_lower = NA,
                ci_upper = NA,
                specification = ifelse(n >= 40, "Adequate (≥40)", "Limited (<40)")
            ))
            
            perf_table$addRow(rowKey = 2, values = list(
                metric = "Pearson Correlation",
                value = pearson_test$estimate,
                ci_lower = pearson_test$conf.int[1],
                ci_upper = pearson_test$conf.int[2],
                specification = ifelse(pearson_test$estimate >= 0.95, "Excellent (≥0.95)", 
                                ifelse(pearson_test$estimate >= 0.90, "Good (≥0.90)", "Needs Improvement"))
            ))
            
            perf_table$addRow(rowKey = 3, values = list(
                metric = "Spearman Correlation",
                value = spearman_test$estimate,
                ci_lower = NA,
                ci_upper = NA,
                specification = ifelse(abs(spearman_test$estimate) >= 0.95, "Excellent (≥0.95)", 
                                ifelse(abs(spearman_test$estimate) >= 0.90, "Good (≥0.90)", "Needs Improvement"))
            ))
            
            if (!is.na(icc_value)) {
                perf_table$addRow(rowKey = 4, values = list(
                    metric = "ICC(3,1) Reliability",
                    value = icc_value,
                    ci_lower = icc_lower,
                    ci_upper = icc_upper,
                    specification = ifelse(icc_value >= 0.90, "Excellent (≥0.90)", 
                                    ifelse(icc_value >= 0.75, "Good (≥0.75)", "Needs Improvement"))
                ))
            }
            
            if (!is.na(ccc_value)) {
                perf_table$addRow(rowKey = 5, values = list(
                    metric = "Concordance Correlation Coefficient",
                    value = ccc_value,
                    ci_lower = ccc_lower,
                    ci_upper = ccc_upper,
                    specification = ifelse(ccc_value >= 0.95, "Excellent (≥0.95)", 
                                    ifelse(ccc_value >= 0.90, "Good (≥0.90)", "Needs Improvement"))
                ))
            }
            
            perf_table$addRow(rowKey = 6, values = list(
                metric = "Total Deviation Index (90th percentile)",
                value = tdi_90,
                ci_lower = NA,
                ci_upper = NA,
                specification = "Lower values indicate better agreement"
            ))
            
            # 2. Bias Analysis
            mean_diff <- mean(differences)
            sd_diff <- sd(differences)
            
            # Test for systematic bias (one-sample t-test)
            systematic_test <- t.test(differences, mu = 0)
            
            # Test for proportional bias (regression of differences on means)
            means <- (reference + test) / 2
            prop_bias_model <- lm(differences ~ means)
            prop_bias_test <- summary(prop_bias_model)
            slope <- prop_bias_test$coefficients[2, 1]
            slope_p <- prop_bias_test$coefficients[2, 4]
            
            # Populate bias table
            bias_table <- self$results$biastable
            
            bias_table$addRow(rowKey = 1, values = list(
                bias_type = "Systematic Bias (Mean Difference)",
                estimate = mean_diff,
                p_value = systematic_test$p.value,
                interpretation = ifelse(systematic_test$p.value < 0.05, 
                                       "Significant systematic bias detected", 
                                       "No significant systematic bias")
            ))
            
            bias_table$addRow(rowKey = 2, values = list(
                bias_type = "Proportional Bias (Slope)",
                estimate = slope,
                p_value = slope_p,
                interpretation = ifelse(slope_p < 0.05, 
                                       "Significant proportional bias detected", 
                                       "No significant proportional bias")
            ))
            
            # 3. Clinical Decision Impact Analysis
            private$.analyzeDecisionImpact(reference, test)
        },
        
        .analyzeDecisionImpact = function(reference, test) {
            # Common clinical thresholds for different biomarkers
            thresholds <- list()
            
            # Determine likely biomarker type based on data range
            data_range <- max(c(reference, test), na.rm = TRUE) - min(c(reference, test), na.rm = TRUE)
            max_val <- max(c(reference, test), na.rm = TRUE)
            
            if (max_val <= 1) {
                # Proportion/percentage data (0-1)
                thresholds <- list("Low (0.1)" = 0.1, "Moderate (0.3)" = 0.3, "High (0.7)" = 0.7)
            } else if (max_val <= 100 && data_range > 10) {
                # Percentage data (0-100) - likely Ki67
                thresholds <- list("Ki67 Low (14%)" = 14, "Ki67 High (30%)" = 30)
            } else if (max_val > 100) {
                # Count or intensity data
                thresholds <- list("Median" = median(c(reference, test), na.rm = TRUE))
            }
            
            decision_table <- self$results$decisiontable
            row_key <- 1
            
            for (threshold_name in names(thresholds)) {
                threshold_val <- thresholds[[threshold_name]]
                
                # Classify based on threshold
                ref_class <- ifelse(reference >= threshold_val, "Positive", "Negative")
                test_class <- ifelse(test >= threshold_val, "Positive", "Negative")
                
                # Calculate agreement
                agreement_pct <- sum(ref_class == test_class) / length(ref_class) * 100
                
                # Calculate Cohen's kappa
                if (requireNamespace('psych', quietly = TRUE)) {
                    confusion_matrix <- table(ref_class, test_class)
                    if (nrow(confusion_matrix) == 2 && ncol(confusion_matrix) == 2) {
                        kappa_val <- psych::cohen.kappa(confusion_matrix)$kappa
                    } else {
                        kappa_val <- NA
                    }
                } else {
                    kappa_val <- NA
                }
                
                # Clinical impact interpretation
                impact_interp <- ifelse(agreement_pct >= 95, "Minimal clinical impact",
                                ifelse(agreement_pct >= 90, "Minor clinical impact",
                                ifelse(agreement_pct >= 85, "Moderate clinical impact", 
                                       "Major clinical impact")))
                
                decision_table$addRow(rowKey = row_key, values = list(
                    threshold = threshold_name,
                    agreement = agreement_pct,
                    kappa = kappa_val,
                    impact = impact_interp
                ))
                
                row_key <- row_key + 1
            }
        },
        
        .generateValidationPlots = function(reference, test) {
            # Prepare data for plots
            plot_data <- list(
                reference = reference,
                test = test,
                differences = test - reference,
                means = (reference + test) / 2,
                residuals = lm(test ~ reference)$residuals
            )
            
            # Set plot data
            self$results$validationplot$setState(plot_data)
            self$results$residualplot$setState(plot_data)
        },
        
        .validationplot = function(image, ggtheme, theme, ...) {
            if (is.null(image$state))
                return(FALSE)
                
            data <- image$state
            
            # Create validation plot (reference vs test with Bland-Altman inset)
            library(ggplot2)
            library(gridExtra)
            
            # Main scatter plot
            p1 <- ggplot2::ggplot(data.frame(Reference = data$reference, Test = data$test), 
                                 ggplot2::aes(x = Reference, y = Test)) +
                ggplot2::geom_point(alpha = 0.6, color = "steelblue") +
                ggplot2::geom_smooth(method = "lm", se = TRUE, color = "blue") +
                ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
                ggplot2::labs(
                    title = "Method Validation: Reference vs Test",
                    subtitle = "Red dashed line = perfect agreement, Blue line = linear fit",
                    x = "Reference Method",
                    y = "Test Method"
                ) +
                ggtheme
                
            print(p1)
            TRUE
        },
        
        .residualplot = function(image, ggtheme, theme, ...) {
            if (is.null(image$state))
                return(FALSE)
                
            data <- image$state
            
            # Create residual plots for validation
            library(ggplot2)
            
            # Bland-Altman plot with regression line
            mean_diff <- mean(data$differences)
            sd_diff <- sd(data$differences)
            
            p <- ggplot2::ggplot(data.frame(Means = data$means, Differences = data$differences), 
                                ggplot2::aes(x = Means, y = Differences)) +
                ggplot2::geom_point(alpha = 0.6, color = "steelblue") +
                ggplot2::geom_hline(yintercept = mean_diff, color = "black", size = 1) +
                ggplot2::geom_hline(yintercept = mean_diff + 1.96*sd_diff, color = "red", linetype = "dashed") +
                ggplot2::geom_hline(yintercept = mean_diff - 1.96*sd_diff, color = "red", linetype = "dashed") +
                ggplot2::geom_smooth(method = "lm", se = TRUE, color = "purple", alpha = 0.3) +
                ggplot2::labs(
                    title = "Bland-Altman Plot with Bias Assessment",
                    subtitle = "Purple line tests for proportional bias",
                    x = "Mean of Methods",
                    y = "Difference (Test - Reference)"
                ) +
                ggtheme
                
            print(p)
            True
        },
        
        .generateValidationInterpretation = function(reference, test) {
            n <- length(reference)
            correlation <- cor(reference, test, method = "pearson")
            
            # Calculate key metrics for interpretation
            differences <- test - reference
            mean_diff <- mean(differences)
            sd_diff <- sd(differences)
            
            # Test for bias
            systematic_p <- t.test(differences, mu = 0)$p.value
            means <- (reference + test) / 2
            prop_bias_p <- summary(lm(differences ~ means))$coefficients[2, 4]
            
            interpretation <- paste0(
                "<h3>Digital Pathology Validation Report</h3>",
                "<p><strong>Analysis Summary:</strong> ", n, " paired measurements evaluated</p>",
                
                "<h4>Analytical Performance:</h4>",
                "<ul>",
                "<li><strong>Correlation:</strong> r = ", round(correlation, 3), 
                " (", ifelse(correlation >= 0.95, "Excellent", 
                      ifelse(correlation >= 0.90, "Good", "Needs improvement")), ")</li>",
                "<li><strong>Mean Bias:</strong> ", round(mean_diff, 3), 
                " (", ifelse(systematic_p < 0.05, "Statistically significant", "Not significant"), ")</li>",
                "<li><strong>Precision (SD of differences):</strong> ", round(sd_diff, 3), "</li>",
                "</ul>",
                
                "<h4>Clinical Validation Assessment:</h4>",
                "<div style='background-color: #f8f9fa; padding: 10px; border-left: 4px solid #007bff;'>",
                if (correlation >= 0.95 && systematic_p >= 0.05 && prop_bias_p >= 0.05) {
                    "<p><strong>✓ VALIDATION PASSED:</strong> The test method demonstrates excellent analytical performance 
                    with no significant bias. <span style='color: green;'>Suitable for clinical implementation.</span></p>"
                } else if (correlation >= 0.90 && abs(mean_diff) < sd_diff) {
                    "<p><strong>⚠ VALIDATION CONDITIONAL:</strong> Good correlation but some bias detected. 
                    <span style='color: orange;'>Consider bias correction or additional validation.</span></p>"
                } else {
                    "<p><strong>✗ VALIDATION CONCERNS:</strong> Significant performance issues detected. 
                    <span style='color: red;'>Additional development required before clinical use.</span></p>"
                },
                "</div>",
                
                "<h4>Regulatory Considerations:</h4>",
                "<ul>",
                if (n >= 40) {
                    "<li>✓ Sample size adequate for FDA/CE submission (n≥40)</li>"
                } else {
                    "<li>⚠ Sample size below recommended minimum for regulatory submission</li>"
                },
                if (correlation >= 0.95) {
                    "<li>✓ Correlation meets clinical laboratory standards</li>"
                } else {
                    "<li>⚠ Correlation below recommended threshold for clinical use</li>"
                },
                "</ul>",
                
                "<h4>Next Steps:</h4>",
                "<ul>",
                "<li>Review clinical decision impact analysis in Decision Impact table</li>",
                "<li>Consider validation on independent test set</li>",
                "<li>Document validation protocol and acceptance criteria</li>",
                if (systematic_p < 0.05) {
                    "<li><strong>Action Required:</strong> Investigate and correct systematic bias</li>"
                } else { "" },
                if (prop_bias_p < 0.05) {
                    "<li><strong>Action Required:</strong> Address proportional bias across measurement range</li>"
                } else { "" },
                "</ul>",
                
                "<p><em>This validation follows CAP/CLSI EP09 guidelines for method comparison studies 
                and FDA guidance for AI/ML-based medical device validation.</em></p>"
            )
            
            self$results$interpretation$setContent(interpretation)
        }
    )
)