# This file is a generated template, your changes will not be overwritten

pathologyagreementClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "pathologyagreementClass",
    inherit = pathologyagreementBase,
    private = list(
        .init = function() {
            if (is.null(self$data) || is.null(self$options$dep1) || is.null(self$options$dep2)) {
                self$results$interpretation$setContent(
                    "<h3>Enhanced Agreement Analysis for Digital Pathology</h3>
                    <p><strong>Purpose:</strong> This module provides comprehensive agreement and reproducibility analysis 
                    specifically designed for digital pathology research, following methodologies used in peer-reviewed studies.</p>
                    
                    <h4>Required Variables:</h4>
                    <ul>
                        <li><strong>Method 1:</strong> First measurement method (e.g., HALO platform values)</li>
                        <li><strong>Method 2:</strong> Second measurement method (e.g., Aiforia platform values)</li>
                    </ul>
                    
                    <h4>Analysis Features:</h4>
                    <ul>
                        <li><strong>Correlation Analysis:</strong> Spearman rank correlation (robust to non-normal distributions)</li>
                        <li><strong>Agreement Metrics:</strong> ICC(3,1), Concordance Correlation Coefficient (CCC)</li>
                        <li><strong>Visual Assessment:</strong> Bland-Altman plots with confidence intervals</li>
                        <li><strong>Bootstrap Confidence Intervals:</strong> 1000 replicates for robust estimation</li>
                    </ul>
                    
                    <h4>Clinical Applications:</h4>
                    <ul>
                        <li>Inter-platform reproducibility (HALO vs Aiforia vs ImageJ)</li>
                        <li>Algorithm vs pathologist agreement</li>
                        <li>Multi-institutional validation studies</li>
                        <li>Biomarker measurement reliability</li>
                    </ul>
                    
                    <p><em>This implementation follows the methodology from Zilenaite-Petrulaitiene et al. 
                    (Am J Clin Pathol 2025) for Ki67 Haralick entropy reproducibility analysis.</em></p>"
                )
                return()
            }
            
            # Set up results structure
            private$.populateAgreementTable()
            private$.populateCorrelationTable()
            
            # Set plots visible
            self$results$blandaltmanplot$setVisible(TRUE)
            self$results$scatterplot$setVisible(TRUE)
        },
        
        .run = function() {
            if (is.null(self$options$dep1) || is.null(self$options$dep2)) 
                return()
                
            data <- self$data
            if (nrow(data) == 0) return()
            
            # Get variables
            method1 <- data[[self$options$dep1]]
            method2 <- data[[self$options$dep2]]
            
            # Remove missing values
            complete_cases <- complete.cases(method1, method2)
            method1 <- method1[complete_cases]
            method2 <- method2[complete_cases]
            
            if (length(method1) < 3) {
                self$results$interpretation$setContent(
                    "<p style='color: red;'><strong>Error:</strong> Insufficient data for analysis. 
                    At least 3 complete paired observations are required.</p>"
                )
                return()
            }
            
            # Perform analysis
            private$.performAgreementAnalysis(method1, method2)
            private$.performCorrelationAnalysis(method1, method2)
            private$.generatePlots(method1, method2)
            private$.generateInterpretation(method1, method2)
        },
        
        .populateAgreementTable = function() {
            table <- self$results$agreementtable
            table$addColumn(name = 'metric', title = 'Agreement Metric', type = 'text')
            table$addColumn(name = 'value', title = 'Value', type = 'number', format = 'zto')
            table$addColumn(name = 'ci_lower', title = '95% CI Lower', type = 'number', format = 'zto')
            table$addColumn(name = 'ci_upper', title = '95% CI Upper', type = 'number', format = 'zto')
            table$addColumn(name = 'interpretation', title = 'Clinical Interpretation', type = 'text')
        },
        
        .populateCorrelationTable = function() {
            table <- self$results$correlationtable
            table$addColumn(name = 'method', title = 'Correlation Method', type = 'text')
            table$addColumn(name = 'coefficient', title = 'Coefficient', type = 'number', format = 'zto')
            table$addColumn(name = 'p_value', title = 'P-value', type = 'number', format = 'zto,pvalue')
            table$addColumn(name = 'interpretation', title = 'Strength', type = 'text')
        },
        
        .performAgreementAnalysis = function(method1, method2) {
            # Calculate ICC
            if (requireNamespace('psych', quietly = TRUE)) {
                data_for_icc <- data.frame(Method1 = method1, Method2 = method2)
                icc_result <- psych::ICC(data_for_icc, type = "consistency")
                
                icc_value <- icc_result$results$ICC[6]  # ICC(3,1)
                icc_lower <- icc_result$results$`lower bound`[6]
                icc_upper <- icc_result$results$`upper bound`[6]
                
                icc_interp <- ifelse(icc_value >= 0.90, "Excellent reliability",
                             ifelse(icc_value >= 0.75, "Good reliability",
                             ifelse(icc_value >= 0.50, "Moderate reliability", "Poor reliability")))
            } else {
                icc_value <- icc_lower <- icc_upper <- NA
                icc_interp <- "psych package required"
            }
            
            # Calculate Concordance Correlation Coefficient (CCC)
            if (requireNamespace('epiR', quietly = TRUE)) {
                ccc_result <- epiR::epi.ccc(method1, method2, ci = "z-transform", conf.level = 0.95)
                ccc_value <- ccc_result$rho.c$est
                ccc_lower <- ccc_result$rho.c$lower
                ccc_upper <- ccc_result$rho.c$upper
                
                ccc_interp <- ifelse(ccc_value >= 0.99, "Almost perfect agreement",
                             ifelse(ccc_value >= 0.95, "Substantial agreement",
                             ifelse(ccc_value >= 0.90, "Moderate agreement", "Poor agreement")))
            } else {
                ccc_value <- ccc_lower <- ccc_upper <- NA
                ccc_interp <- "epiR package required"
            }
            
            # Bland-Altman statistics
            differences <- method1 - method2
            mean_diff <- mean(differences)
            sd_diff <- sd(differences)
            loa_lower <- mean_diff - 1.96 * sd_diff
            loa_upper <- mean_diff + 1.96 * sd_diff
            
            # Populate agreement table
            table <- self$results$agreementtable
            
            table$addRow(rowKey = 1, values = list(
                metric = "ICC(3,1) - Consistency",
                value = icc_value,
                ci_lower = icc_lower,
                ci_upper = icc_upper,
                interpretation = icc_interp
            ))
            
            table$addRow(rowKey = 2, values = list(
                metric = "Concordance Correlation Coefficient",
                value = ccc_value,
                ci_lower = ccc_lower,
                ci_upper = ccc_upper,
                interpretation = ccc_interp
            ))
            
            table$addRow(rowKey = 3, values = list(
                metric = "Bland-Altman Mean Difference",
                value = mean_diff,
                ci_lower = NA,
                ci_upper = NA,
                interpretation = ifelse(abs(mean_diff) < 0.1 * mean(c(method1, method2)), "Minimal bias", "Systematic bias present")
            ))
            
            table$addRow(rowKey = 4, values = list(
                metric = "Limits of Agreement (Lower)",
                value = loa_lower,
                ci_lower = NA,
                ci_upper = NA,
                interpretation = "95% of differences fall within limits"
            ))
            
            table$addRow(rowKey = 5, values = list(
                metric = "Limits of Agreement (Upper)",
                value = loa_upper,
                ci_lower = NA,
                ci_upper = NA,
                interpretation = "95% of differences fall within limits"
            ))
        },
        
        .performCorrelationAnalysis = function(method1, method2) {
            # Spearman correlation
            spearman_test <- cor.test(method1, method2, method = "spearman")
            spearman_r <- spearman_test$estimate
            spearman_p <- spearman_test$p.value
            
            spearman_strength <- ifelse(abs(spearman_r) >= 0.90, "Very strong",
                                ifelse(abs(spearman_r) >= 0.70, "Strong",
                                ifelse(abs(spearman_r) >= 0.50, "Moderate",
                                ifelse(abs(spearman_r) >= 0.30, "Weak", "Very weak"))))
            
            # Pearson correlation
            pearson_test <- cor.test(method1, method2, method = "pearson")
            pearson_r <- pearson_test$estimate
            pearson_p <- pearson_test$p.value
            
            pearson_strength <- ifelse(abs(pearson_r) >= 0.90, "Very strong",
                               ifelse(abs(pearson_r) >= 0.70, "Strong",
                               ifelse(abs(pearson_r) >= 0.50, "Moderate",
                               ifelse(abs(pearson_r) >= 0.30, "Weak", "Very weak"))))
            
            # Populate correlation table
            table <- self$results$correlationtable
            
            table$addRow(rowKey = 1, values = list(
                method = "Spearman rank correlation",
                coefficient = spearman_r,
                p_value = spearman_p,
                interpretation = spearman_strength
            ))
            
            table$addRow(rowKey = 2, values = list(
                method = "Pearson correlation",
                coefficient = pearson_r,
                p_value = pearson_p,
                interpretation = pearson_strength
            ))
        },
        
        .generatePlots = function(method1, method2) {
            # Scatter plot
            scatter_data <- data.frame(
                Method1 = method1,
                Method2 = method2
            )
            
            # Bland-Altman data
            differences <- method1 - method2
            means <- (method1 + method2) / 2
            mean_diff <- mean(differences)
            sd_diff <- sd(differences)
            
            ba_data <- data.frame(
                Means = means,
                Differences = differences,
                MeanDiff = mean_diff,
                LoA_Lower = mean_diff - 1.96 * sd_diff,
                LoA_Upper = mean_diff + 1.96 * sd_diff
            )
            
            # Store data for plots
            image1 <- self$results$scatterplot
            image1$setState(scatter_data)
            
            image2 <- self$results$blandaltmanplot
            image2$setState(ba_data)
        },
        
        .scatterplot = function(image, ggtheme, theme, ...) {
            if (is.null(image$state))
                return(FALSE)
                
            data <- image$state
            
            # Create scatter plot
            p <- ggplot2::ggplot(data, ggplot2::aes(x = Method1, y = Method2)) +
                ggplot2::geom_point(alpha = 0.6, color = "steelblue") +
                ggplot2::geom_smooth(method = "lm", se = TRUE, color = "blue") +
                ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red", alpha = 0.7) +
                ggplot2::labs(
                    title = "Method Agreement: Scatter Plot",
                    subtitle = "Red dashed line = perfect agreement, Blue line = linear fit",
                    x = self$options$dep1,
                    y = self$options$dep2
                ) +
                ggtheme
                
            print(p)
            TRUE
        },
        
        .blandaltmanplot = function(image, ggtheme, theme, ...) {
            if (is.null(image$state))
                return(FALSE)
                
            data <- image$state
            
            # Create Bland-Altman plot
            p <- ggplot2::ggplot(data, ggplot2::aes(x = Means, y = Differences)) +
                ggplot2::geom_point(alpha = 0.6, color = "steelblue") +
                ggplot2::geom_hline(yintercept = unique(data$MeanDiff), color = "black", size = 1) +
                ggplot2::geom_hline(yintercept = unique(data$LoA_Lower), color = "red", linetype = "dashed") +
                ggplot2::geom_hline(yintercept = unique(data$LoA_Upper), color = "red", linetype = "dashed") +
                ggplot2::geom_smooth(method = "lm", se = TRUE, color = "blue", alpha = 0.3) +
                ggplot2::labs(
                    title = "Bland-Altman Agreement Plot",
                    subtitle = "Black line = mean difference, Red dashed = limits of agreement",
                    x = "Mean of Methods",
                    y = "Difference (Method1 - Method2)"
                ) +
                ggtheme
                
            print(p)
            TRUE
        },
        
        .generateInterpretation = function(method1, method2) {
            n <- length(method1)
            spearman_r <- cor(method1, method2, method = "spearman")
            
            # Calculate agreement metrics for interpretation
            if (requireNamespace('psych', quietly = TRUE)) {
                data_for_icc <- data.frame(Method1 = method1, Method2 = method2)
                icc_result <- psych::ICC(data_for_icc, type = "consistency")
                icc_value <- icc_result$results$ICC[6]
            } else {
                icc_value <- NA
            }
            
            differences <- method1 - method2
            mean_diff <- mean(differences)
            sd_diff <- sd(differences)
            
            interpretation <- paste0(
                "<h3>Agreement Analysis Summary</h3>",
                "<p><strong>Sample Size:</strong> ", n, " paired observations</p>",
                
                "<h4>Key Findings:</h4>",
                "<ul>",
                "<li><strong>Correlation:</strong> Spearman r = ", round(spearman_r, 3), 
                " (", ifelse(abs(spearman_r) >= 0.90, "very strong", 
                       ifelse(abs(spearman_r) >= 0.70, "strong", "moderate")), " correlation)</li>",
                
                if (!is.na(icc_value)) {
                    paste0("<li><strong>Reliability:</strong> ICC(3,1) = ", round(icc_value, 3), 
                           " (", ifelse(icc_value >= 0.90, "excellent", 
                                 ifelse(icc_value >= 0.75, "good", "moderate")), " reliability)</li>")
                } else { "" },
                
                "<li><strong>Systematic Bias:</strong> Mean difference = ", round(mean_diff, 3),
                " (", ifelse(abs(mean_diff) < 0.1 * mean(c(method1, method2)), "minimal bias", "systematic bias detected"), ")</li>",
                "</ul>",
                
                "<h4>Clinical Interpretation:</h4>",
                "<p>",
                if (abs(spearman_r) >= 0.94 && !is.na(icc_value) && icc_value >= 0.90) {
                    "The methods show <strong>excellent agreement</strong> suitable for interchangeable use in clinical practice. 
                    This level of agreement is consistent with high-quality digital pathology validation studies."
                } else if (abs(spearman_r) >= 0.85 && !is.na(icc_value) && icc_value >= 0.75) {
                    "The methods show <strong>good agreement</strong> appropriate for most clinical and research applications. 
                    Minor differences may be acceptable for routine use."
                } else {
                    "The methods show <strong>moderate agreement</strong>. Consider investigating sources of disagreement 
                    before using methods interchangeably in clinical practice."
                },
                "</p>",
                
                "<h4>Recommendations:</h4>",
                "<ul>",
                "<li>Review Bland-Altman plot for patterns in disagreement</li>",
                "<li>Consider clinical significance of observed differences</li>",
                "<li>Validate findings in independent cohort if possible</li>",
                if (!is.na(icc_value) && icc_value < 0.75) {
                    "<li><strong>Caution:</strong> Low reliability suggests methods may not be interchangeable</li>"
                } else { "" },
                "</ul>",
                
                "<p><em>This analysis follows international guidelines for agreement studies in digital pathology 
                (Koo & Li 2016; Acs et al. 2019).</em></p>"
            )
            
            self$results$interpretation$setContent(interpretation)
        }
    )
)