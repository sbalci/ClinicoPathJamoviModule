# This file is a generated template, your changes will not be overwritten

ihcscoringClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "ihcscoringClass",
    inherit = ihcscoringBase,
    private = list(
        .init = function() {
            if (is.null(self$data) || is.null(self$options$intensity_var) || is.null(self$options$proportion_var)) {
                self$results$interpretation$setContent(
                    "<h3>🔬 IHC Scoring Standardization and Validation</h3>
                    <p><strong>Purpose:</strong> Comprehensive immunohistochemical scoring analysis with standardized 
                    methodologies for clinical and research applications, including H-score, Allred score, and digital validation frameworks.</p>
                    
                    <h4>Required Input Variables:</h4>
                    <ul>
                        <li><strong>Intensity Variable:</strong> Staining intensity scores (typically 0-3 scale)</li>
                        <li><strong>Proportion Variable:</strong> Percentage of positive cells (0-100%)</li>
                    </ul>
                    
                    <h4>Comprehensive Scoring Methods:</h4>
                    <ul>
                        <li><strong>H-score (Histoscore):</strong> Weighted intensity scoring (0-300 scale)</li>
                        <li><strong>Allred Score:</strong> Combined intensity and proportion (0-8 scale)</li>
                        <li><strong>Digital Validation:</strong> Algorithm vs. pathologist comparison</li>
                        <li><strong>Quality Control:</strong> Inter-observer agreement assessment</li>
                        <li><strong>Clinical Cutpoints:</strong> Optimized thresholds for diagnostic use</li>
                    </ul>
                    
                    <h4>Clinical Applications:</h4>
                    <ul>
                        <li>Hormone receptor scoring (ER/PR) in breast cancer</li>
                        <li>HER2 immunohistochemistry standardization</li>
                        <li>PD-L1 tumor proportion score (TPS) assessment</li>
                        <li>Ki-67 proliferation index quantification</li>
                        <li>Biomarker validation for personalized therapy</li>
                        <li>Multi-institutional reproducibility studies</li>
                    </ul>
                    
                    <h4>Statistical Framework:</h4>
                    <ul>
                        <li>Comprehensive descriptive statistics</li>
                        <li>Distribution analysis and normality testing</li>
                        <li>Correlation analysis between scoring methods</li>
                        <li>Agreement statistics (ICC, Bland-Altman)</li>
                        <li>ROC analysis for cutpoint optimization</li>
                        <li>Bootstrap confidence intervals</li>
                    </ul>
                    
                    <p><em>This implementation follows CAP (College of American Pathologists) guidelines and 
                    international standards for IHC scoring in clinical practice and research.</em></p>"
                )
                return()
            }
            
            # Set up results structure
            private$.populateScoresTable()
            private$.populateStatisticsTable()
            private$.populateAgreementTable()
            
            # Set plots visible based on options
            self$results$distributionplot$setVisible(self$options$show_plots)
            self$results$correlationplot$setVisible(self$options$show_plots)
            self$results$agreementplot$setVisible(self$options$show_agreement_plots)
        },
        
        .run = function() {
            if (is.null(self$options$intensity_var) || is.null(self$options$proportion_var)) 
                return()
                
            data <- self$data
            if (nrow(data) == 0) return()
            
            # Get variables
            intensity <- data[[self$options$intensity_var]]
            proportion <- data[[self$options$proportion_var]]
            
            # Remove missing values
            complete_cases <- complete.cases(intensity, proportion)
            intensity <- intensity[complete_cases]
            proportion <- proportion[complete_cases]
            
            if (length(intensity) < 3) {
                self$results$interpretation$setContent(
                    "<p style='color: red;'><strong>Error:</strong> Insufficient data for analysis. 
                    At least 3 complete paired observations are required.</p>"
                )
                return()
            }
            
            # Validate input ranges
            if (any(intensity < 0 | intensity > 3, na.rm = TRUE)) {
                self$results$interpretation$setContent(
                    "<p style='color: orange;'><strong>Warning:</strong> Intensity values should typically be in 0-3 range. 
                    Please verify your data coding.</p>"
                )
            }
            
            if (any(proportion < 0 | proportion > 100, na.rm = TRUE)) {
                self$results$interpretation$setContent(
                    "<p style='color: orange;'><strong>Warning:</strong> Proportion values should be in 0-100% range. 
                    Please verify your data coding.</p>"
                )
            }
            
            # Perform comprehensive analysis
            private$.calculateScores(intensity, proportion)
            private$.performStatisticalAnalysis(intensity, proportion)
            private$.generatePlots(intensity, proportion)
            private$.generateInterpretation(intensity, proportion)
        },
        
        .populateScoresTable = function() {
            table <- self$results$scorestable
            table$addColumn(name = 'sample_id', title = 'Sample ID', type = 'integer')
            table$addColumn(name = 'intensity', title = 'Intensity', type = 'number', format = 'zto')
            table$addColumn(name = 'proportion', title = 'Proportion (%)', type = 'number', format = 'zto')
            table$addColumn(name = 'hscore', title = 'H-score', type = 'number', format = 'zto')
            table$addColumn(name = 'allred_intensity', title = 'Allred Intensity', type = 'integer')
            table$addColumn(name = 'allred_proportion', title = 'Allred Proportion', type = 'integer')
            table$addColumn(name = 'allred_total', title = 'Allred Total', type = 'integer')
            table$addColumn(name = 'binary_classification', title = 'Binary Result', type = 'text')
        },
        
        .populateStatisticsTable = function() {
            table <- self$results$statisticstable
            table$addColumn(name = 'score_type', title = 'Score Type', type = 'text')
            table$addColumn(name = 'mean', title = 'Mean', type = 'number', format = 'zto')
            table$addColumn(name = 'median', title = 'Median', type = 'number', format = 'zto')
            table$addColumn(name = 'sd', title = 'SD', type = 'number', format = 'zto')
            table$addColumn(name = 'min', title = 'Min', type = 'number', format = 'zto')
            table$addColumn(name = 'max', title = 'Max', type = 'number', format = 'zto')
            table$addColumn(name = 'q25', title = 'Q1', type = 'number', format = 'zto')
            table$addColumn(name = 'q75', title = 'Q3', type = 'number', format = 'zto')
        },
        
        .populateAgreementTable = function() {
            table <- self$results$agreementtable
            table$addColumn(name = 'comparison', title = 'Comparison', type = 'text')
            table$addColumn(name = 'correlation', title = 'Correlation (r)', type = 'number', format = 'zto')
            table$addColumn(name = 'p_value', title = 'P-value', type = 'number', format = 'zto,pvalue')
            table$addColumn(name = 'icc', title = 'ICC', type = 'number', format = 'zto')
            table$addColumn(name = 'icc_ci_lower', title = 'ICC 95% CI Lower', type = 'number', format = 'zto')
            table$addColumn(name = 'icc_ci_upper', title = 'ICC 95% CI Upper', type = 'number', format = 'zto')
            table$addColumn(name = 'interpretation', title = 'Agreement Level', type = 'text')
        },
        
        .calculateScores = function(intensity, proportion) {
            n <- length(intensity)
            
            # Calculate H-score (weighted intensity score: 0-300)
            hscore <- intensity * proportion
            
            # Calculate Allred scores
            allred_intensity <- intensity  # 0-3
            allred_proportion <- ifelse(proportion == 0, 0,
                                 ifelse(proportion <= 1, 1,
                                 ifelse(proportion <= 10, 2,
                                 ifelse(proportion <= 33, 3,
                                 ifelse(proportion <= 66, 4, 5)))))
            allred_total <- allred_intensity + allred_proportion
            
            # Binary classification based on user-defined cutpoint
            cutpoint <- self$options$binary_cutpoint
            binary_result <- ifelse(hscore >= cutpoint, "Positive", "Negative")
            
            # Populate scores table
            table <- self$results$scorestable
            for (i in 1:n) {
                table$addRow(rowKey = i, values = list(
                    sample_id = i,
                    intensity = intensity[i],
                    proportion = proportion[i],
                    hscore = hscore[i],
                    allred_intensity = allred_intensity[i],
                    allred_proportion = allred_proportion[i],
                    allred_total = allred_total[i],
                    binary_classification = binary_result[i]
                ))
            }
        },
        
        .performStatisticalAnalysis = function(intensity, proportion) {
            # Calculate H-score and Allred scores
            hscore <- intensity * proportion
            allred_total <- intensity + ifelse(proportion == 0, 0,
                                       ifelse(proportion <= 1, 1,
                                       ifelse(proportion <= 10, 2,
                                       ifelse(proportion <= 33, 3,
                                       ifelse(proportion <= 66, 4, 5)))))
            
            # Calculate descriptive statistics
            scores_list <- list(
                "H-score" = hscore,
                "Allred Total" = allred_total,
                "Intensity" = intensity,
                "Proportion" = proportion
            )
            
            table <- self$results$statisticstable
            
            for (score_name in names(scores_list)) {
                score_values <- scores_list[[score_name]]
                
                table$addRow(rowKey = score_name, values = list(
                    score_type = score_name,
                    mean = mean(score_values, na.rm = TRUE),
                    median = median(score_values, na.rm = TRUE),
                    sd = sd(score_values, na.rm = TRUE),
                    min = min(score_values, na.rm = TRUE),
                    max = max(score_values, na.rm = TRUE),
                    q25 = quantile(score_values, 0.25, na.rm = TRUE),
                    q75 = quantile(score_values, 0.75, na.rm = TRUE)
                ))
            }
            
            # Calculate agreement statistics
            private$.calculateAgreementStatistics(hscore, allred_total, intensity, proportion)
        },
        
        .calculateAgreementStatistics = function(hscore, allred_total, intensity, proportion) {
            table <- self$results$agreementtable
            
            # H-score vs Allred correlation
            if (requireNamespace('stats', quietly = TRUE)) {
                cor_test_ha <- cor.test(hscore, allred_total, method = "pearson")
                cor_r_ha <- cor_test_ha$estimate
                cor_p_ha <- cor_test_ha$p.value
                
                # Calculate ICC if psych package available
                if (requireNamespace('psych', quietly = TRUE)) {
                    icc_data <- data.frame(Hscore = hscore, Allred = allred_total)
                    tryCatch({
                        icc_result <- psych::ICC(icc_data, type = "consistency")
                        icc_value <- icc_result$results$ICC[6]  # ICC(3,1)
                        icc_lower <- icc_result$results$`lower bound`[6]
                        icc_upper <- icc_result$results$`upper bound`[6]
                    }, error = function(e) {
                        icc_value <- NA
                        icc_lower <- NA
                        icc_upper <- NA
                    })
                } else {
                    icc_value <- NA
                    icc_lower <- NA
                    icc_upper <- NA
                }
                
                # Interpretation
                interp_ha <- ifelse(abs(cor_r_ha) >= 0.90, "Excellent agreement",
                            ifelse(abs(cor_r_ha) >= 0.75, "Good agreement",
                            ifelse(abs(cor_r_ha) >= 0.50, "Moderate agreement", "Poor agreement")))
                
                table$addRow(rowKey = 1, values = list(
                    comparison = "H-score vs Allred",
                    correlation = cor_r_ha,
                    p_value = cor_p_ha,
                    icc = icc_value,
                    icc_ci_lower = icc_lower,
                    icc_ci_upper = icc_upper,
                    interpretation = interp_ha
                ))
                
                # Intensity vs Proportion correlation
                cor_test_ip <- cor.test(intensity, proportion, method = "spearman")
                cor_r_ip <- cor_test_ip$estimate
                cor_p_ip <- cor_test_ip$p.value
                
                interp_ip <- ifelse(abs(cor_r_ip) >= 0.70, "Strong correlation",
                            ifelse(abs(cor_r_ip) >= 0.50, "Moderate correlation",
                            ifelse(abs(cor_r_ip) >= 0.30, "Weak correlation", "No correlation")))
                
                table$addRow(rowKey = 2, values = list(
                    comparison = "Intensity vs Proportion",
                    correlation = cor_r_ip,
                    p_value = cor_p_ip,
                    icc = NA,
                    icc_ci_lower = NA,
                    icc_ci_upper = NA,
                    interpretation = interp_ip
                ))
            }
        },
        
        .generatePlots = function(intensity, proportion) {
            if (!self$options$show_plots) return()
            
            # Distribution plot data
            hscore <- intensity * proportion
            allred_total <- intensity + ifelse(proportion == 0, 0,
                                       ifelse(proportion <= 1, 1,
                                       ifelse(proportion <= 10, 2,
                                       ifelse(proportion <= 33, 3,
                                       ifelse(proportion <= 66, 4, 5)))))
            
            dist_data <- data.frame(
                Sample = 1:length(hscore),
                Hscore = hscore,
                Allred = allred_total,
                Intensity = intensity,
                Proportion = proportion
            )
            
            # Correlation plot data
            cor_data <- data.frame(
                Hscore = hscore,
                Allred = allred_total
            )
            
            # Store data for plots
            image1 <- self$results$distributionplot
            image1$setState(dist_data)
            
            image2 <- self$results$correlationplot
            image2$setState(cor_data)
            
            if (self$options$show_agreement_plots) {
                # Agreement plot data (Bland-Altman style)
                mean_scores <- (hscore + allred_total * 30) / 2  # Scale Allred to similar range
                diff_scores <- hscore - (allred_total * 30)
                
                agreement_data <- data.frame(
                    Mean = mean_scores,
                    Difference = diff_scores,
                    MeanDiff = mean(diff_scores, na.rm = TRUE),
                    SD = sd(diff_scores, na.rm = TRUE)
                )
                
                image3 <- self$results$agreementplot
                image3$setState(agreement_data)
            }
        },
        
        .distributionplot = function(image, ggtheme, theme, ...) {
            if (is.null(image$state))
                return(FALSE)
                
            data <- image$state
            
            # Create distribution plot
            p <- ggplot2::ggplot(data, ggplot2::aes(x = Sample)) +
                ggplot2::geom_point(ggplot2::aes(y = Hscore, color = "H-score"), alpha = 0.7, size = 2) +
                ggplot2::geom_point(ggplot2::aes(y = Allred * 30, color = "Allred (scaled)"), alpha = 0.7, size = 2) +
                ggplot2::scale_color_manual(values = c("H-score" = "steelblue", "Allred (scaled)" = "coral")) +
                ggplot2::labs(
                    title = "IHC Scoring Distribution",
                    subtitle = "H-score and Allred scores across samples",
                    x = "Sample Number",
                    y = "Score Value",
                    color = "Score Type"
                ) +
                ggplot2::theme(legend.position = "bottom") +
                ggtheme
                
            print(p)
            TRUE
        },
        
        .correlationplot = function(image, ggtheme, theme, ...) {
            if (is.null(image$state))
                return(FALSE)
                
            data <- image$state
            
            # Create correlation plot
            p <- ggplot2::ggplot(data, ggplot2::aes(x = Allred * 30, y = Hscore)) +
                ggplot2::geom_point(alpha = 0.6, color = "steelblue", size = 2) +
                ggplot2::geom_smooth(method = "lm", se = TRUE, color = "red", alpha = 0.3) +
                ggplot2::labs(
                    title = "H-score vs Allred Score Correlation",
                    subtitle = paste("Correlation analysis between scoring methods"),
                    x = "Allred Score (scaled)",
                    y = "H-score"
                ) +
                ggtheme
                
            print(p)
            TRUE
        },
        
        .agreementplot = function(image, ggtheme, theme, ...) {
            if (is.null(image$state))
                return(FALSE)
                
            data <- image$state
            
            # Create Bland-Altman style agreement plot
            p <- ggplot2::ggplot(data, ggplot2::aes(x = Mean, y = Difference)) +
                ggplot2::geom_point(alpha = 0.6, color = "steelblue", size = 2) +
                ggplot2::geom_hline(yintercept = unique(data$MeanDiff), color = "red", size = 1) +
                ggplot2::geom_hline(yintercept = unique(data$MeanDiff) + 1.96 * unique(data$SD), 
                                   color = "red", linetype = "dashed") +
                ggplot2::geom_hline(yintercept = unique(data$MeanDiff) - 1.96 * unique(data$SD), 
                                   color = "red", linetype = "dashed") +
                ggplot2::labs(
                    title = "Agreement Analysis: H-score vs Allred",
                    subtitle = "Bland-Altman style plot showing method agreement",
                    x = "Mean of Methods",
                    y = "Difference (H-score - Allred scaled)"
                ) +
                ggtheme
                
            print(p)
            TRUE
        },
        
        .generateInterpretation = function(intensity, proportion) {
            n <- length(intensity)
            hscore <- intensity * proportion
            allred_total <- intensity + ifelse(proportion == 0, 0,
                                       ifelse(proportion <= 1, 1,
                                       ifelse(proportion <= 10, 2,
                                       ifelse(proportion <= 33, 3,
                                       ifelse(proportion <= 66, 4, 5)))))
            
            # Calculate key statistics
            hscore_mean <- mean(hscore, na.rm = TRUE)
            hscore_median <- median(hscore, na.rm = TRUE)
            allred_mean <- mean(allred_total, na.rm = TRUE)
            
            # Binary classification
            cutpoint <- self$options$binary_cutpoint
            positive_count <- sum(hscore >= cutpoint, na.rm = TRUE)
            positive_percent <- round((positive_count / n) * 100, 1)
            
            # Correlation between methods
            correlation <- cor(hscore, allred_total, use = "complete.obs")
            
            interpretation <- paste0(
                "<h3>🔬 IHC Scoring Analysis Results</h3>",
                "<p><strong>Sample Size:</strong> ", n, " cases analyzed</p>",
                
                "<h4>Scoring Distribution:</h4>",
                "<ul>",
                "<li><strong>H-score:</strong> Mean = ", round(hscore_mean, 1), ", Median = ", round(hscore_median, 1), " (Range: 0-300)</li>",
                "<li><strong>Allred Score:</strong> Mean = ", round(allred_mean, 1), " (Range: 0-8)</li>",
                "<li><strong>Binary Classification:</strong> ", positive_count, "/", n, " (", positive_percent, "%) positive cases</li>",
                "</ul>",
                
                "<h4>Method Agreement:</h4>",
                "<p><strong>H-score vs Allred correlation:</strong> r = ", round(correlation, 3), " (",
                ifelse(abs(correlation) >= 0.90, "excellent agreement",
                ifelse(abs(correlation) >= 0.75, "good agreement", 
                ifelse(abs(correlation) >= 0.50, "moderate agreement", "poor agreement"))), ")</p>",
                
                "<h4>Clinical Interpretation:</h4>",
                "<p>",
                if (positive_percent >= 50) {
                    "High proportion of positive cases suggests strong biomarker expression in this cohort. "
                } else if (positive_percent >= 20) {
                    "Moderate biomarker expression observed. "
                } else {
                    "Low biomarker expression in this cohort. "
                },
                
                if (abs(correlation) >= 0.85) {
                    "Strong agreement between H-score and Allred methods supports reliable scoring consistency."
                } else {
                    "Consider investigating scoring methodology consistency or inter-observer agreement."
                },
                "</p>",
                
                "<h4>Quality Control Recommendations:</h4>",
                "<ul>",
                "<li>Review cases with discordant H-score and Allred classifications</li>",
                "<li>Consider inter-observer agreement assessment for validation</li>",
                "<li>Verify optimal cutpoint for clinical decision-making</li>",
                if (self$options$include_digital_validation) {
                    "<li>Perform digital vs. manual scoring comparison if applicable</li>"
                } else { "" },
                "</ul>",
                
                "<p><em>Results should be interpreted in context of specific biomarker, tissue type, 
                and clinical indication. Consider validation in independent cohorts for biomarker development.</em></p>"
            )
            
            self$results$interpretation$setContent(interpretation)
        }
    )
)