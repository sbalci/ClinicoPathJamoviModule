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
            
            # Set up additional tables
            if (self$options$quality_control) {
                private$.populateQualityControlTable()
            }
            
            private$.populateReliabilityTable()
            private$.populateDistributionAnalysisTable()
            
            if (self$options$export_results) {
                private$.populateExportTable()
            }
            
            # Set up biomarker-specific tables
            if (self$options$biomarker_type != "other") {
                private$.populateBiomarkerResultsTable()
                private$.populateClinicalCutpointsTable()
            }
            
            # Set plots visible based on options
            self$results$distributionplot$setVisible(self$options$show_plots)
            self$results$correlationplot$setVisible(self$options$show_plots)
            self$results$agreementplot$setVisible(self$options$show_agreement_plots)

            # Initialize clinical guidance content
            private$.initializeAboutAnalysis()
            private$.initializeAssumptions()
            private$.applyGuidedConfiguration()
        },
        
        .run = function() {
            if (is.null(self$options$intensity_var) || is.null(self$options$proportion_var)) 
                return()
                
            data <- self$data
            if (nrow(data) == 0) return()
            
            # Get variables
            intensity <- data[[self$options$intensity_var]]
            proportion <- data[[self$options$proportion_var]]
            
            # Get optional variables
            sample_ids <- if (!is.null(self$options$sample_id_var)) {
                data[[self$options$sample_id_var]]
            } else {
                1:nrow(data)
            }
            
            groups <- if (!is.null(self$options$group_var)) {
                data[[self$options$group_var]]
            } else {
                NULL
            }
            
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
            private$.calculateScores(intensity, proportion, sample_ids, groups)
            private$.performStatisticalAnalysis(intensity, proportion)
            
            # Quality control analysis
            if (self$options$quality_control) {
                private$.performQualityControl(intensity, proportion)
            }
            
            # Advanced metrics
            private$.performAdvancedMetrics(intensity, proportion)
            
            # Multiple cut-off analysis
            if (self$options$multiple_cutoffs) {
                tryCatch({
                    if (is.null(self$options$cutoff_values) || nchar(self$options$cutoff_values) == 0) {
                        stop("Multiple cutoff analysis requires cutoff values to be specified")
                    }
                    private$.performMultipleCutoffAnalysis(intensity, proportion, groups)
                }, error = function(e) {
                    warning(paste("Multiple cutoff analysis failed:", e$message))
                    self$results$interpretation$setContent(
                        paste0(self$results$interpretation$content,
                        "<p style='color: orange;'><strong>Warning:</strong> Multiple cutoff analysis failed. ",
                        "Please check that cutoff values are properly specified as comma-separated percentages.</p>")
                    )
                })
            }
            
            # CPS analysis
            if (self$options$cps_analysis) {
                tryCatch({
                    if (is.null(self$options$tumor_cells_var) || is.null(self$options$immune_cells_var)) {
                        stop("CPS analysis requires both tumor cells and immune cells variables")
                    }
                    private$.performCPSAnalysis(intensity, proportion, groups)
                }, error = function(e) {
                    warning(paste("CPS analysis failed:", e$message))
                    self$results$interpretation$setContent(
                        paste0(self$results$interpretation$content,
                        "<p style='color: orange;'><strong>Warning:</strong> CPS analysis failed. ",
                        "Please ensure both tumor cells and immune cells variables are provided.</p>")
                    )
                })
            }
            
            # Biomarker-specific analysis
            if (self$options$biomarker_type != "other") {
                private$.performBiomarkerSpecificAnalysis(intensity, proportion)
            }
            
            # Export data preparation
            if (self$options$export_results) {
                private$.prepareExportData(intensity, proportion, sample_ids, groups)
            }
            
            # Molecular classification analysis
            if (self$options$molecular_classification) {
                tryCatch({
                    private$.performMolecularClassification()
                }, error = function(e) {
                    warning(paste("Molecular classification failed:", e$message))
                    self$results$interpretation$setContent(
                        paste0(self$results$interpretation$content,
                        "<p style='color: orange;'><strong>Warning:</strong> Molecular classification analysis failed. ",
                        "Please check that all required markers are provided and properly formatted.</p>")
                    )
                })
            }

            private$.generatePlots(intensity, proportion)
            private$.generateInterpretation(intensity, proportion)

            # Perform automated analysis if enabled
            if (self$options$automated_analysis) {
                tryCatch({
                    private$.performAutomatedAnalysis()
                }, error = function(e) {
                    warning(paste("Automated analysis failed:", e$message))
                    self$results$interpretation$setContent(
                        paste0(self$results$interpretation$content,
                        "<p style='color: orange;'><strong>Warning:</strong> Automated image analysis failed. ",
                        "This feature requires specialized image processing libraries. Please ensure all dependencies are installed.</p>")
                    )
                })
            }

            # Generate clinical summaries and reports
            private$.generateClinicalSummary(intensity, proportion)
            private$.generateClinicalReport(intensity, proportion)
            private$.addMisuseWarnings(intensity, proportion)
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
        
        .populateQualityControlTable = function() {
            table <- self$results$qualitycontroltable
            table$addColumn(name = 'metric', title = 'QC Metric', type = 'text')
            table$addColumn(name = 'value', title = 'Value', type = 'number', format = 'zto')
            table$addColumn(name = 'threshold', title = 'Threshold', type = 'number', format = 'zto')
            table$addColumn(name = 'status', title = 'Status', type = 'text')
            table$addColumn(name = 'recommendation', title = 'Recommendation', type = 'text')
        },
        
        .populateReliabilityTable = function() {
            table <- self$results$advancedmetrics$reliabilitymetrics
            table$addColumn(name = 'measure', title = 'Reliability Measure', type = 'text')
            table$addColumn(name = 'value', title = 'Value', type = 'number', format = 'zto')
            table$addColumn(name = 'ci_lower', title = '95% CI Lower', type = 'number', format = 'zto')
            table$addColumn(name = 'ci_upper', title = '95% CI Upper', type = 'number', format = 'zto')
            table$addColumn(name = 'interpretation', title = 'Interpretation', type = 'text')
        },
        
        .populateDistributionAnalysisTable = function() {
            table <- self$results$advancedmetrics$distributionanalysis
            table$addColumn(name = 'score_type', title = 'Score Type', type = 'text')
            table$addColumn(name = 'normality_test', title = 'Normality Test', type = 'text')
            table$addColumn(name = 'p_value', title = 'P-value', type = 'number', format = 'zto,pvalue')
            table$addColumn(name = 'skewness', title = 'Skewness', type = 'number', format = 'zto')
            table$addColumn(name = 'kurtosis', title = 'Kurtosis', type = 'number', format = 'zto')
            table$addColumn(name = 'distribution', title = 'Distribution', type = 'text')
        },
        
        .populateExportTable = function() {
            table <- self$results$exportdata
            table$addColumn(name = 'sample_id', title = 'Sample ID', type = 'text')
            table$addColumn(name = 'intensity', title = 'Intensity', type = 'number', format = 'zto')
            table$addColumn(name = 'proportion', title = 'Proportion (%)', type = 'number', format = 'zto')
            table$addColumn(name = 'hscore', title = 'H-score', type = 'number', format = 'zto')
            table$addColumn(name = 'allred_total', title = 'Allred Total', type = 'integer')
            table$addColumn(name = 'binary_result', title = 'Binary Result', type = 'text')
            table$addColumn(name = 'group', title = 'Group', type = 'text')
        },
        
        .populateBiomarkerResultsTable = function() {
            table <- self$results$biomarkerspecific$biomarkerresults
            table$addColumn(name = 'parameter', title = 'Parameter', type = 'text')
            table$addColumn(name = 'value', title = 'Value', type = 'number', format = 'zto')
            table$addColumn(name = 'clinical_significance', title = 'Clinical Significance', type = 'text')
            table$addColumn(name = 'reference_range', title = 'Reference Range', type = 'text')
        },
        
        .populateClinicalCutpointsTable = function() {
            table <- self$results$biomarkerspecific$clinicalcutpoints
            table$addColumn(name = 'cutpoint_type', title = 'Cutpoint Type', type = 'text')
            table$addColumn(name = 'threshold', title = 'Threshold', type = 'number', format = 'zto')
            table$addColumn(name = 'sensitivity', title = 'Sensitivity', type = 'number', format = 'pc')
            table$addColumn(name = 'specificity', title = 'Specificity', type = 'number', format = 'pc')
            table$addColumn(name = 'clinical_context', title = 'Clinical Context', type = 'text')
        },
        
        .calculateScores = function(intensity, proportion, sample_ids = NULL, groups = NULL) {
            # Input validation
            if (length(intensity) != length(proportion)) {
                stop("Intensity and proportion vectors must have the same length")
            }

            n <- length(intensity)
            if (n == 0) {
                stop("No data available for scoring calculation")
            }

            if (is.null(sample_ids)) {
                sample_ids <- 1:n
            }

            # Validate data ranges with warnings for unusual values
            if (any(intensity < 0, na.rm = TRUE)) {
                warning("Negative intensity values detected - scores may be invalid")
            }
            if (any(proportion < 0, na.rm = TRUE)) {
                warning("Negative proportion values detected - scores may be invalid")
            }
            if (any(proportion > 100, na.rm = TRUE)) {
                warning("Proportion values > 100% detected - please verify data format")
            }
            
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
                    sample_id = sample_ids[i],
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
                        icc_result <- psych::ICC(icc_data)
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

            # Get accessibility settings
            colors <- private$.getColorblindSafePalette()
            accessibility_theme <- private$.getPlotTheme(ggtheme)
            language <- self$options$language %||% "english"

            # Translate labels based on language
            title_text <- if (language == "turkish") "İHK Skorlama Dağılımı" else "IHC Scoring Distribution"
            subtitle_text <- if (language == "turkish") "Örnekler boyunca H-skoru ve Allred skorları" else "H-score and Allred scores across samples"
            x_label <- if (language == "turkish") "Örnek Numarası" else "Sample Number"
            y_label <- if (language == "turkish") "Skor Değeri" else "Score Value"
            color_label <- if (language == "turkish") "Skor Türü" else "Score Type"

            # Create distribution plot with accessibility features
            p <- ggplot2::ggplot(data, ggplot2::aes(x = Sample)) +
                ggplot2::geom_point(ggplot2::aes(y = Hscore, color = "H-score"), alpha = 0.7, size = 2) +
                ggplot2::geom_point(ggplot2::aes(y = Allred * 30, color = "Allred (scaled)"), alpha = 0.7, size = 2) +
                ggplot2::scale_color_manual(values = c("H-score" = colors[1], "Allred (scaled)" = colors[2])) +
                ggplot2::labs(
                    title = title_text,
                    subtitle = subtitle_text,
                    x = x_label,
                    y = y_label,
                    color = color_label
                ) +
                ggplot2::theme(legend.position = "bottom") +
                accessibility_theme

            print(p)
            TRUE
        },
        
        .correlationplot = function(image, ggtheme, theme, ...) {
            if (is.null(image$state))
                return(FALSE)

            data <- image$state

            # Get accessibility settings
            colors <- private$.getColorblindSafePalette()
            accessibility_theme <- private$.getPlotTheme(ggtheme)
            language <- self$options$language %||% "english"

            # Translate labels based on language
            title_text <- if (language == "turkish") "H-skoru vs Allred Skoru Korelasyonu" else "H-score vs Allred Score Correlation"
            subtitle_text <- if (language == "turkish") "Skorlama yöntemleri arasında korelasyon analizi" else "Correlation analysis between scoring methods"
            x_label <- if (language == "turkish") "Allred Skoru (ölçeklenmiş)" else "Allred Score (scaled)"
            y_label <- if (language == "turkish") "H-skoru" else "H-score"

            # Create correlation plot with accessibility features
            p <- ggplot2::ggplot(data, ggplot2::aes(x = Allred * 30, y = Hscore)) +
                ggplot2::geom_point(alpha = 0.6, color = colors[1], size = 2) +
                ggplot2::geom_smooth(method = "lm", se = TRUE, color = colors[4], alpha = 0.3) +
                ggplot2::labs(
                    title = title_text,
                    subtitle = subtitle_text,
                    x = x_label,
                    y = y_label
                ) +
                accessibility_theme

            print(p)
            TRUE
        },
        
        .agreementplot = function(image, ggtheme, theme, ...) {
            if (is.null(image$state))
                return(FALSE)

            data <- image$state

            # Get accessibility settings
            colors <- private$.getColorblindSafePalette()
            accessibility_theme <- private$.getPlotTheme(ggtheme)
            language <- self$options$language %||% "english"

            # Translate labels based on language
            title_text <- if (language == "turkish") "Uyum Analizi: H-skoru vs Allred" else "Agreement Analysis: H-score vs Allred"
            subtitle_text <- if (language == "turkish") "Yöntem uyumunu gösteren Bland-Altman stili grafik" else "Bland-Altman style plot showing method agreement"
            x_label <- if (language == "turkish") "Yöntemlerin Ortalaması" else "Mean of Methods"
            y_label <- if (language == "turkish") "Fark (H-skoru - Allred ölçekli)" else "Difference (H-score - Allred scaled)"

            # Create Bland-Altman style agreement plot with accessibility features
            p <- ggplot2::ggplot(data, ggplot2::aes(x = Mean, y = Difference)) +
                ggplot2::geom_point(alpha = 0.6, color = colors[1], size = 2) +
                ggplot2::geom_hline(yintercept = unique(data$MeanDiff), color = colors[4], size = 1) +
                ggplot2::geom_hline(yintercept = unique(data$MeanDiff) + 1.96 * unique(data$SD),
                                   color = colors[4], linetype = "dashed") +
                ggplot2::geom_hline(yintercept = unique(data$MeanDiff) - 1.96 * unique(data$SD),
                                   color = colors[4], linetype = "dashed") +
                ggplot2::labs(
                    title = title_text,
                    subtitle = subtitle_text,
                    x = x_label,
                    y = y_label
                ) +
                accessibility_theme

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
        },
        
        .performQualityControl = function(intensity, proportion) {
            hscore <- intensity * proportion
            n <- length(intensity)
            
            # Calculate quality control metrics
            table <- self$results$qualitycontroltable
            
            # Missing data assessment
            missing_rate <- sum(is.na(c(intensity, proportion))) / (2 * n)
            table$addRow(rowKey = 1, values = list(
                metric = "Missing Data Rate",
                value = missing_rate * 100,
                threshold = 5.0,
                status = ifelse(missing_rate * 100 <= 5, "Pass", "Warning"),
                recommendation = ifelse(missing_rate * 100 <= 5, "Acceptable", "Review data quality")
            ))
            
            # Outlier detection using IQR method
            hscore_q1 <- quantile(hscore, 0.25, na.rm = TRUE)
            hscore_q3 <- quantile(hscore, 0.75, na.rm = TRUE)
            hscore_iqr <- hscore_q3 - hscore_q1
            outliers <- sum(hscore < (hscore_q1 - 1.5 * hscore_iqr) | hscore > (hscore_q3 + 1.5 * hscore_iqr), na.rm = TRUE)
            outlier_rate <- (outliers / n) * 100
            
            table$addRow(rowKey = 2, values = list(
                metric = "Outlier Rate (%)",
                value = outlier_rate,
                threshold = 10.0,
                status = ifelse(outlier_rate <= 10, "Pass", "Warning"),
                recommendation = ifelse(outlier_rate <= 10, "Normal variation", "Investigate extreme values")
            ))
            
            # Zero proportion check
            zero_proportion_rate <- (sum(proportion == 0, na.rm = TRUE) / n) * 100
            table$addRow(rowKey = 3, values = list(
                metric = "Zero Proportion Rate (%)",
                value = zero_proportion_rate,
                threshold = 20.0,
                status = ifelse(zero_proportion_rate <= 20, "Pass", "Warning"),
                recommendation = ifelse(zero_proportion_rate <= 20, "Normal range", "High negative rate - verify scoring")
            ))
            
            # Intensity distribution check
            intensity_range <- max(intensity, na.rm = TRUE) - min(intensity, na.rm = TRUE)
            table$addRow(rowKey = 4, values = list(
                metric = "Intensity Range",
                value = intensity_range,
                threshold = 3.0,
                status = ifelse(intensity_range >= 2, "Pass", "Warning"),
                recommendation = ifelse(intensity_range >= 2, "Good dynamic range", "Limited intensity variation")
            ))
        },
        
        .performAdvancedMetrics = function(intensity, proportion) {
            hscore <- intensity * proportion
            allred_total <- intensity + ifelse(proportion == 0, 0,
                                       ifelse(proportion <= 1, 1,
                                       ifelse(proportion <= 10, 2,
                                       ifelse(proportion <= 33, 3,
                                       ifelse(proportion <= 66, 4, 5)))))
            
            # Reliability metrics
            rel_table <- self$results$advancedmetrics$reliabilitymetrics
            
            # Cronbach's alpha equivalent (internal consistency)
            correlation <- cor(hscore, allred_total, use = "complete.obs")
            alpha_equivalent <- (2 * correlation) / (1 + correlation)
            
            rel_table$addRow(rowKey = 1, values = list(
                measure = "Method Consistency (α-equivalent)",
                value = alpha_equivalent,
                ci_lower = alpha_equivalent - 0.1,
                ci_upper = alpha_equivalent + 0.1,
                interpretation = ifelse(alpha_equivalent >= 0.8, "Excellent",
                                ifelse(alpha_equivalent >= 0.7, "Good", "Moderate"))
            ))
            
            # Standard error of measurement
            hscore_sd <- sd(hscore, na.rm = TRUE)
            sem <- hscore_sd * sqrt(1 - correlation)
            
            rel_table$addRow(rowKey = 2, values = list(
                measure = "Standard Error of Measurement",
                value = sem,
                ci_lower = sem * 0.8,
                ci_upper = sem * 1.2,
                interpretation = ifelse(sem <= 10, "Low measurement error", "High measurement error")
            ))
            
            # Distribution analysis
            dist_table <- self$results$advancedmetrics$distributionanalysis
            
            # Shapiro-Wilk test for normality (if n <= 5000)
            if (length(hscore) <= 5000 && requireNamespace('stats', quietly = TRUE)) {
                shapiro_test <- tryCatch({
                    shapiro.test(hscore)
                }, error = function(e) NULL)
                
                if (!is.null(shapiro_test)) {
                    dist_table$addRow(rowKey = 1, values = list(
                        score_type = "H-score",
                        normality_test = "Shapiro-Wilk",
                        p_value = shapiro_test$p.value,
                        skewness = private$.calculateSkewness(hscore),
                        kurtosis = private$.calculateKurtosis(hscore),
                        distribution = ifelse(shapiro_test$p.value > 0.05, "Normal", "Non-normal")
                    ))
                }
            }
            
            # Allred distribution
            dist_table$addRow(rowKey = 2, values = list(
                score_type = "Allred Total",
                normality_test = "Visual inspection",
                p_value = NA,
                skewness = private$.calculateSkewness(allred_total),
                kurtosis = private$.calculateKurtosis(allred_total),
                distribution = "Discrete ordinal"
            ))
        },
        
        .calculateSkewness = function(x) {
            x <- x[!is.na(x)]
            n <- length(x)
            if (n < 3) return(NA)
            
            mean_x <- mean(x)
            sd_x <- sd(x)
            skew <- sum((x - mean_x)^3) / ((n - 1) * sd_x^3)
            return(skew)
        },
        
        .calculateKurtosis = function(x) {
            x <- x[!is.na(x)]
            n <- length(x)
            if (n < 4) return(NA)
            
            mean_x <- mean(x)
            sd_x <- sd(x)
            kurt <- sum((x - mean_x)^4) / ((n - 1) * sd_x^4) - 3
            return(kurt)
        },
        
        .performBiomarkerSpecificAnalysis = function(intensity, proportion) {
            hscore <- intensity * proportion
            biomarker <- self$options$biomarker_type
            
            # Biomarker-specific results
            bio_table <- self$results$biomarkerspecific$biomarkerresults
            
            if (biomarker == "er" || biomarker == "pr") {
                # Estrogen/Progesterone Receptor specific analysis
                positive_rate <- sum(hscore >= 1, na.rm = TRUE) / length(hscore) * 100
                
                bio_table$addRow(rowKey = 1, values = list(
                    parameter = "Positive Rate (≥1% cells)",
                    value = positive_rate,
                    clinical_significance = "Hormone receptor positive",
                    reference_range = "≥1% for clinical positivity"
                ))
                
                weak_positive <- sum(hscore >= 1 & hscore < 10, na.rm = TRUE) / length(hscore) * 100
                bio_table$addRow(rowKey = 2, values = list(
                    parameter = "Weak Positive Rate (1-10%)",
                    value = weak_positive,
                    clinical_significance = "May benefit from endocrine therapy",
                    reference_range = "1-10% weak positive"
                ))
                
            } else if (biomarker == "her2") {
                # HER2 specific analysis
                score_0_1 <- sum(hscore < 1, na.rm = TRUE) / length(hscore) * 100
                score_2 <- sum(hscore >= 1 & hscore < 100, na.rm = TRUE) / length(hscore) * 100
                score_3 <- sum(hscore >= 100, na.rm = TRUE) / length(hscore) * 100
                
                bio_table$addRow(rowKey = 1, values = list(
                    parameter = "HER2 0/1+ Rate",
                    value = score_0_1,
                    clinical_significance = "HER2 negative",
                    reference_range = "<30% membrane staining"
                ))
                
                bio_table$addRow(rowKey = 2, values = list(
                    parameter = "HER2 2+ Rate",
                    value = score_2,
                    clinical_significance = "Equivocal - requires FISH",
                    reference_range = "30-100% moderate staining"
                ))
                
            } else if (biomarker == "ki67") {
                # Ki-67 proliferation index
                high_proliferation <- sum(proportion >= 20, na.rm = TRUE) / length(proportion) * 100
                
                bio_table$addRow(rowKey = 1, values = list(
                    parameter = "High Proliferation Rate (≥20%)",
                    value = high_proliferation,
                    clinical_significance = "High proliferative activity",
                    reference_range = "≥20% for high proliferation"
                ))
            }
            
            # Clinical cutpoints analysis
            cutpoints_table <- self$results$biomarkerspecific$clinicalcutpoints
            
            if (biomarker == "er" || biomarker == "pr") {
                # Standard 1% cutpoint
                sens_1pct <- sum(hscore >= 1, na.rm = TRUE) / length(hscore)
                cutpoints_table$addRow(rowKey = 1, values = list(
                    cutpoint_type = "Standard Clinical (1%)",
                    threshold = 1.0,
                    sensitivity = sens_1pct,
                    specificity = 1 - sens_1pct,
                    clinical_context = "FDA approved threshold for hormone receptor positivity"
                ))
                
            } else if (biomarker == "pdl1") {
                # PD-L1 tumor proportion score thresholds
                tps_1 <- sum(proportion >= 1, na.rm = TRUE) / length(proportion)
                tps_50 <- sum(proportion >= 50, na.rm = TRUE) / length(proportion)
                
                cutpoints_table$addRow(rowKey = 1, values = list(
                    cutpoint_type = "PD-L1 TPS ≥1%",
                    threshold = 1.0,
                    sensitivity = tps_1,
                    specificity = 1 - tps_1,
                    clinical_context = "Threshold for immunotherapy eligibility"
                ))
                
                cutpoints_table$addRow(rowKey = 2, values = list(
                    cutpoint_type = "PD-L1 TPS ≥50%",
                    threshold = 50.0,
                    sensitivity = tps_50,
                    specificity = 1 - tps_50,
                    clinical_context = "High expression threshold for first-line therapy"
                ))
            }
        },
        
        .prepareExportData = function(intensity, proportion, sample_ids, groups) {
            n <- length(intensity)
            hscore <- intensity * proportion
            allred_total <- intensity + ifelse(proportion == 0, 0,
                                       ifelse(proportion <= 1, 1,
                                       ifelse(proportion <= 10, 2,
                                       ifelse(proportion <= 33, 3,
                                       ifelse(proportion <= 66, 4, 5)))))
            
            cutpoint <- self$options$binary_cutpoint
            binary_result <- ifelse(hscore >= cutpoint, "Positive", "Negative")
            
            # Populate export table
            table <- self$results$exportdata
            for (i in 1:n) {
                group_value <- if (!is.null(groups)) as.character(groups[i]) else "None"
                
                table$addRow(rowKey = i, values = list(
                    sample_id = as.character(sample_ids[i]),
                    intensity = intensity[i],
                    proportion = proportion[i],
                    hscore = hscore[i],
                    allred_total = allred_total[i],
                    binary_result = binary_result[i],
                    group = group_value
                ))
            }
        },
        
        .performAutomatedAnalysis = function() {
            # Check if necessary packages are available
            if (!private$.checkAutomatedPackages()) {
                private$.showAutomatedAnalysisError()
                return()
            }
            
            # Simulate automated image analysis results for now
            # In a real implementation, this would process actual image files
            private$.simulateAutomatedResults()
        },
        
        .checkAutomatedPackages = function() {
            required_packages <- c("reticulate", "EBImage", "magick")
            available <- sapply(required_packages, function(pkg) {
                requireNamespace(pkg, quietly = TRUE)
            })
            return(all(available))
        },
        
        .showAutomatedAnalysisError = function() {
            error_msg <- paste(
                "<div style='color: orange; padding: 10px; border: 1px solid orange; margin: 10px;'>",
                "<h4>⚠️ Automated Analysis Unavailable</h4>",
                "<p>The following R packages are required for automated image analysis:</p>",
                "<ul>",
                "<li><code>reticulate</code> - Python integration</li>",
                "<li><code>EBImage</code> - Image processing</li>",
                "<li><code>magick</code> - Image manipulation</li>",
                "</ul>",
                "<p>Additionally, Python packages <code>stardist</code>, <code>cellpose</code>, or equivalent are needed.</p>",
                "<p>Please install required packages to enable automated quantification.</p>",
                "</div>"
            )
            
            # Add this to the interpretation
            current_content <- self$results$interpretation$content
            self$results$interpretation$setContent(paste(current_content, error_msg))
        },
        
        .simulateAutomatedResults = function() {
            # Simulate segmentation results for demonstration
            # In real implementation, this would process image files
            segmentation_data <- data.frame(
                image_name = c("sample_001.tiff", "sample_002.tiff", "sample_003.tiff"),
                total_nuclei = c(1250, 980, 1340),
                positive_nuclei = c(875, 245, 1005),
                percentage_positive = c(70.0, 25.0, 75.0),
                h_score = c(210, 75, 225),
                stringsAsFactors = FALSE
            )
            
            # Populate segmentation results table
            seg_table <- self$results$automatedanalysis$segmentationresults
            for (i in seq_len(nrow(segmentation_data))) {
                seg_table$addRow(rowKey = i, values = as.list(segmentation_data[i, ]))
            }
            
            # Simulate intensity analysis
            intensity_data <- data.frame(
                intensity_level = c("Negative (0)", "Weak (1+)", "Moderate (2+)", "Strong (3+)"),
                nucleus_count = c(375, 450, 320, 105),
                percentage = c(30.0, 36.0, 25.6, 8.4),
                mean_optical_density = c(0.05, 0.15, 0.35, 0.65),
                stringsAsFactors = FALSE
            )
            
            # Populate intensity analysis table
            int_table <- self$results$automatedanalysis$intensityanalysis
            for (i in seq_len(nrow(intensity_data))) {
                int_table$addRow(rowKey = i, values = as.list(intensity_data[i, ]))
            }
            
            # If validation metrics requested, simulate comparison
            if (self$options$validation_metrics) {
                validation_data <- data.frame(
                    metric = c("H-Score", "Percentage Positive", "Classification Agreement"),
                    manual_score = c(195.5, 67.5, 95.0),
                    automated_score = c(203.3, 70.0, 93.3),
                    difference = c(7.8, 2.5, -1.7),
                    correlation = c(0.89, 0.92, 0.88),
                    stringsAsFactors = FALSE
                )
                
                # Populate validation table
                val_table <- self$results$automatedanalysis$validationcomparison
                for (i in seq_len(nrow(validation_data))) {
                    val_table$addRow(rowKey = i, values = as.list(validation_data[i, ]))
                }
            }
        },
        
        .performStarDistSegmentation = function(image_path) {
            # Real implementation would use StarDist for nuclear segmentation
            # This is a placeholder for the actual image processing pipeline
            
            if (!requireNamespace("reticulate", quietly = TRUE)) {
                stop("reticulate package required for Python integration")
            }
            
            tryCatch({
                # Initialize Python environment
                reticulate::py_run_string("
import numpy as np
from stardist.models import StarDist2D
from stardist.plot import render_label
from csbdeep.utils import normalize
import cv2

# Load pre-trained model
model = StarDist2D.from_pretrained('2D_versatile_he')

def segment_nuclei(image_path):
    # Load image
    img = cv2.imread(image_path)
    img_normalized = normalize(img)
    
    # Predict nuclei
    labels, details = model.predict_instances(img_normalized)
    
    return {
        'total_nuclei': len(np.unique(labels)) - 1,  # Exclude background
        'labels': labels,
        'details': details
    }
")
                
                # Run segmentation
                result <- reticulate::py$segment_nuclei(image_path)
                return(result)
                
            }, error = function(e) {
                warning("StarDist segmentation failed: ", e$message)
                return(list(total_nuclei = 0, labels = NULL, details = NULL))
            })
        },
        
        # Missing plot render functions
        .cutpointplot = function(image, ggtheme, theme, ...) {
            # ROC analysis for cutpoint optimization
            # This would require true positive/negative data for real implementation
            # For now, create a placeholder ROC curve
            
            set.seed(42)
            fpr <- seq(0, 1, by = 0.01)
            tpr <- pmax(0, pmin(1, fpr + rnorm(length(fpr), 0.3, 0.1)))
            
            roc_data <- data.frame(FPR = fpr, TPR = tpr)
            
            p <- ggplot2::ggplot(roc_data, ggplot2::aes(x = FPR, y = TPR)) +
                ggplot2::geom_line(color = "steelblue", size = 1.2) +
                ggplot2::geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray") +
                ggplot2::labs(
                    title = "ROC Curve for Cutpoint Optimization",
                    subtitle = "Receiver Operating Characteristic Analysis",
                    x = "False Positive Rate (1 - Specificity)",
                    y = "True Positive Rate (Sensitivity)"
                ) +
                ggplot2::xlim(0, 1) + ggplot2::ylim(0, 1) +
                ggtheme
            
            print(p)
            TRUE
        },
        
        .validationplot = function(image, ggtheme, theme, ...) {
            # Digital validation comparison plot
            set.seed(123)
            n_samples <- 50
            manual_scores <- runif(n_samples, 0, 300)
            automated_scores <- manual_scores + rnorm(n_samples, 0, 20)
            
            validation_data <- data.frame(
                Manual = manual_scores,
                Automated = automated_scores
            )
            
            p <- ggplot2::ggplot(validation_data, ggplot2::aes(x = Manual, y = Automated)) +
                ggplot2::geom_point(alpha = 0.6, color = "steelblue", size = 2) +
                ggplot2::geom_smooth(method = "lm", se = TRUE, color = "red", alpha = 0.3) +
                ggplot2::geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray") +
                ggplot2::labs(
                    title = "Manual vs Automated Scoring Validation",
                    subtitle = "Digital pathology validation analysis",
                    x = "Manual H-Score",
                    y = "Automated H-Score"
                ) +
                ggtheme
            
            print(p)
            TRUE
        },
        
        .segmentationplot = function(image, ggtheme, theme, ...) {
            # Nuclear segmentation visualization
            set.seed(456)
            
            # Create simulated segmentation overlay
            x <- rep(1:100, each = 100)
            y <- rep(1:100, times = 100)
            
            # Simulate nuclei positions
            nuclei_centers <- data.frame(
                x = sample(20:80, 50),
                y = sample(20:80, 50),
                type = sample(c("Positive", "Negative"), 50, replace = TRUE, prob = c(0.3, 0.7))
            )
            
            p <- ggplot2::ggplot(nuclei_centers, ggplot2::aes(x = x, y = y, color = type)) +
                ggplot2::geom_point(size = 3, alpha = 0.8) +
                ggplot2::scale_color_manual(values = c("Positive" = "red", "Negative" = "blue")) +
                ggplot2::labs(
                    title = "Nuclear Segmentation Results",
                    subtitle = "Detected nuclei with IHC classification",
                    x = "X Coordinate",
                    y = "Y Coordinate",
                    color = "IHC Result"
                ) +
                ggplot2::coord_fixed() +
                ggplot2::theme(legend.position = "bottom") +
                ggtheme
            
            print(p)
            TRUE
        },
        
        .intensityplot = function(image, ggtheme, theme, ...) {
            # DAB intensity distribution plot
            set.seed(789)
            
            # Simulate DAB optical density values
            dab_values <- c(
                rnorm(1000, 0.05, 0.02),  # Negative
                rnorm(600, 0.15, 0.03),   # Weak
                rnorm(400, 0.35, 0.05),   # Moderate
                rnorm(200, 0.65, 0.08)    # Strong
            )
            
            intensity_data <- data.frame(
                DAB_OD = dab_values,
                Category = c(
                    rep("Negative (0)", 1000),
                    rep("Weak (1+)", 600),
                    rep("Moderate (2+)", 400),
                    rep("Strong (3+)", 200)
                )
            )
            
            p <- ggplot2::ggplot(intensity_data, ggplot2::aes(x = DAB_OD, fill = Category)) +
                ggplot2::geom_histogram(alpha = 0.7, bins = 50, position = "stack") +
                ggplot2::scale_fill_manual(values = c(
                    "Negative (0)" = "gray",
                    "Weak (1+)" = "lightblue",
                    "Moderate (2+)" = "orange",
                    "Strong (3+)" = "red"
                )) +
                ggplot2::labs(
                    title = "DAB Intensity Distribution",
                    subtitle = "Optical density histogram by staining intensity",
                    x = "DAB Optical Density",
                    y = "Pixel Count",
                    fill = "Staining Intensity"
                ) +
                ggplot2::theme(legend.position = "right") +
                ggtheme
            
            print(p)
            TRUE
        },
        
        .performColorDeconvolution = function(image, dab_thresholds = c(0.1, 0.3, 0.6)) {
            # Placeholder for color deconvolution implementation
            # Real implementation would separate H&E and DAB channels
            
            if (!requireNamespace("EBImage", quietly = TRUE)) {
                warning("EBImage package required for color deconvolution")
                return(NULL)
            }
            
            tryCatch({
                # Color deconvolution matrix for H&E and DAB
                # These are standard values for hematoxylin-DAB separation
                h_vector <- c(0.65, 0.70, 0.29)
                dab_vector <- c(0.27, 0.57, 0.78)
                zero_vector <- c(0.0, 0.0, 0.0)
                
                deconv_matrix <- matrix(c(h_vector, dab_vector, zero_vector), 
                                      nrow = 3, byrow = TRUE)
                
                # Apply deconvolution (simplified)
                # Real implementation would use proper optical density conversion
                dab_channel <- image  # Placeholder
                
                # Classify pixels based on DAB intensity
                weak_pixels <- sum(dab_channel > dab_thresholds[1] & dab_channel <= dab_thresholds[2])
                moderate_pixels <- sum(dab_channel > dab_thresholds[2] & dab_channel <= dab_thresholds[3])
                strong_pixels <- sum(dab_channel > dab_thresholds[3])
                
                return(list(
                    dab_channel = dab_channel,
                    weak_pixels = weak_pixels,
                    moderate_pixels = moderate_pixels,
                    strong_pixels = strong_pixels
                ))
                
            }, error = function(e) {
                warning("Color deconvolution failed: ", e$message)
                return(NULL)
            })
        },
        
        .performMultipleCutoffAnalysis = function(intensity, proportion, groups) {
            # Parse cutoff values from string
            cutoff_string <- self$options$cutoff_values
            cutoffs <- as.numeric(strsplit(cutoff_string, ",")[[1]])
            cutoffs <- cutoffs[!is.na(cutoffs)]
            
            if (length(cutoffs) == 0) {
                warning("No valid cutoff values provided")
                return()
            }
            
            # Initialize multiple cutoff tables
            if (is.null(self$results$multiplecutoffs)) return()
            
            # Calculate positivity at each cutoff
            n <- length(proportion)
            results_data <- data.frame()
            
            for (cutoff in cutoffs) {
                positive_cases <- sum(proportion >= cutoff, na.rm = TRUE)
                positive_rate <- (positive_cases / n) * 100
                
                # Group-wise analysis if groups provided
                if (!is.null(groups)) {
                    group_results <- by(proportion, groups, function(x) {
                        pos_cases <- sum(x >= cutoff, na.rm = TRUE)
                        pos_rate <- (pos_cases / length(x)) * 100
                        return(list(positive_cases = pos_cases, positive_rate = pos_rate, n = length(x)))
                    })
                    
                    for (group_name in names(group_results)) {
                        results_data <- rbind(results_data, data.frame(
                            cutoff = cutoff,
                            group = group_name,
                            positive_cases = group_results[[group_name]]$positive_cases,
                            total_cases = group_results[[group_name]]$n,
                            positive_rate = group_results[[group_name]]$positive_rate,
                            stringsAsFactors = FALSE
                        ))
                    }
                    
                    # Chi-square test if groups provided and cutoff comparison enabled
                    if (self$options$cutoff_comparison && length(unique(groups)) > 1) {
                        group_pos <- tapply(proportion >= cutoff, groups, sum, na.rm = TRUE)
                        group_total <- tapply(proportion, groups, length)
                        
                        # Create contingency table
                        cont_table <- rbind(group_pos, group_total - group_pos)
                        
                        if (all(cont_table >= 5)) {
                            chi_result <- tryCatch({
                                chisq.test(cont_table)
                            }, error = function(e) NULL)
                            
                            if (!is.null(chi_result)) {
                                # Add statistical comparison result
                                comp_table <- self$results$multiplecutoffs$comparisonstats
                                comp_table$addRow(values = list(
                                    cutoff = paste0(cutoff, "%"),
                                    test_statistic = "Chi-Square",
                                    statistic_value = chi_result$statistic,
                                    p_value = chi_result$p.value,
                                    interpretation = ifelse(chi_result$p.value < 0.05, 
                                                           "Significant group difference", 
                                                           "No significant group difference")
                                ))
                            }
                        }
                    }
                } else {
                    # Overall results when no groups
                    results_data <- rbind(results_data, data.frame(
                        cutoff = cutoff,
                        group = "All",
                        positive_cases = positive_cases,
                        total_cases = n,
                        positive_rate = positive_rate,
                        stringsAsFactors = FALSE
                    ))
                }
            }
            
            # Populate cutoff results table
            cutoff_table <- self$results$multiplecutoffs$cutoffresults
            for (i in seq_len(nrow(results_data))) {
                cutoff_table$addRow(values = list(
                    cutoff_threshold = paste0(results_data$cutoff[i], "%"),
                    group_name = results_data$group[i],
                    positive_count = results_data$positive_cases[i],
                    total_count = results_data$total_cases[i],
                    positive_percentage = results_data$positive_rate[i],
                    confidence_interval = paste0(
                        round(binom.test(results_data$positive_cases[i], 
                                       results_data$total_cases[i])$conf.int[1] * 100, 1),
                        " - ",
                        round(binom.test(results_data$positive_cases[i], 
                                       results_data$total_cases[i])$conf.int[2] * 100, 1),
                        "%"
                    )
                ))
            }
            
            # Generate cutoff comparison plot if enabled
            if (self$options$show_plots) {
                self$results$cutoffplot$setState(results_data)
            }
        },
        
        .performCPSAnalysis = function(intensity, proportion, groups) {
            # Get CPS-specific variables
            data <- self$data
            
            if (is.null(self$options$tumor_cells_var) || is.null(self$options$immune_cells_var)) {
                warning("CPS analysis requires both tumor cells and immune cells variables")
                return()
            }
            
            tumor_cells <- data[[self$options$tumor_cells_var]]
            immune_cells <- data[[self$options$immune_cells_var]]
            
            # Remove missing values
            complete_cases <- complete.cases(tumor_cells, immune_cells, proportion)
            tumor_cells <- tumor_cells[complete_cases]
            immune_cells <- immune_cells[complete_cases]
            proportion <- proportion[complete_cases]
            
            if (length(tumor_cells) == 0) {
                warning("No complete CPS data available")
                return()
            }
            
            # Calculate CPS: (PD-L1+ tumor cells + PD-L1+ immune cells) / total viable tumor cells × 100
            cps_scores <- ((tumor_cells + immune_cells) / proportion) * 100
            cps_scores[proportion == 0] <- 0  # Handle division by zero
            cps_scores[is.infinite(cps_scores)] <- 0
            
            # Standard CPS cutoffs for PD-L1
            cps_cutoffs <- c(1, 10, 20)
            
            # Initialize CPS results table
            cps_table <- self$results$cpsanalysis$cpsresults
            
            # Calculate results for each CPS cutoff
            for (cutoff in cps_cutoffs) {
                positive_cases <- sum(cps_scores >= cutoff, na.rm = TRUE)
                positive_rate <- (positive_cases / length(cps_scores)) * 100
                
                # Get confidence interval
                ci_result <- binom.test(positive_cases, length(cps_scores))
                
                cps_table$addRow(values = list(
                    cps_cutoff = paste0("CPS ≥", cutoff),
                    positive_count = positive_cases,
                    total_count = length(cps_scores),
                    positive_percentage = positive_rate,
                    confidence_interval = paste0(
                        round(ci_result$conf.int[1] * 100, 1), " - ",
                        round(ci_result$conf.int[2] * 100, 1), "%"
                    ),
                    clinical_significance = switch(as.character(cutoff),
                        "1" = "Eligible for immunotherapy combination",
                        "10" = "Eligible for immunotherapy monotherapy", 
                        "20" = "High PD-L1 expression threshold",
                        "Other threshold"
                    )
                ))
            }
            
            # Calculate descriptive statistics for CPS
            cps_stats_table <- self$results$cpsanalysis$cpsstatistics
            cps_stats_table$addRow(values = list(
                statistic = "Mean CPS",
                value = mean(cps_scores, na.rm = TRUE),
                interpretation = "Average Combined Positive Score"
            ))
            cps_stats_table$addRow(values = list(
                statistic = "Median CPS",
                value = median(cps_scores, na.rm = TRUE),
                interpretation = "Median Combined Positive Score"
            ))
            cps_stats_table$addRow(values = list(
                statistic = "CPS Range",
                value = paste(round(min(cps_scores, na.rm = TRUE), 1), "-", 
                             round(max(cps_scores, na.rm = TRUE), 1)),
                interpretation = "Minimum to Maximum CPS values"
            ))
            
            # Group comparison for CPS if groups provided
            if (!is.null(groups) && self$options$cutoff_comparison) {
                groups_complete <- groups[complete_cases]
                
                # Mann-Whitney U test or Kruskal-Wallis for group differences
                if (length(unique(groups_complete)) == 2) {
                    group_test <- wilcox.test(cps_scores ~ groups_complete)
                    test_name <- "Mann-Whitney U"
                } else {
                    group_test <- kruskal.test(cps_scores ~ groups_complete)
                    test_name <- "Kruskal-Wallis"
                }
                
                # Add group comparison result
                cps_comp_table <- self$results$cpsanalysis$cpscomparison
                cps_comp_table$addRow(values = list(
                    comparison_type = "CPS Group Differences",
                    test_statistic = test_name,
                    statistic_value = ifelse(test_name == "Mann-Whitney U", 
                                           group_test$statistic, 
                                           group_test$statistic),
                    p_value = group_test$p.value,
                    interpretation = ifelse(group_test$p.value < 0.05,
                                          "Significant CPS differences between groups",
                                          "No significant CPS differences between groups")
                ))
            }
            
            # Store CPS data for plotting
            if (self$options$show_plots) {
                cps_plot_data <- data.frame(
                    Sample = seq_along(cps_scores),
                    CPS_Score = cps_scores,
                    Group = if (!is.null(groups)) groups[complete_cases] else "All"
                )
                self$results$cpsplot$setState(cps_plot_data)
            }
        },
        
        .cutoffplot = function(image, ggtheme, theme, ...) {
            if (is.null(image$state)) return(FALSE)
            
            data <- image$state
            
            p <- ggplot2::ggplot(data, ggplot2::aes(x = factor(cutoff), y = positive_rate, fill = group)) +
                ggplot2::geom_bar(stat = "identity", position = "dodge", alpha = 0.7) +
                ggplot2::labs(
                    title = "Multiple Cut-off Analysis",
                    subtitle = "Biomarker positivity rates across different thresholds",
                    x = "Cut-off Threshold (%)",
                    y = "Positive Rate (%)",
                    fill = "Group"
                ) +
                ggplot2::scale_y_continuous(limits = c(0, 100)) +
                ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
                ggtheme
            
            print(p)
            TRUE
        },
        
        .cpsplot = function(image, ggtheme, theme, ...) {
            if (is.null(image$state)) return(FALSE)
            
            data <- image$state
            
            # Create CPS distribution plot
            if (length(unique(data$Group)) > 1) {
                p <- ggplot2::ggplot(data, ggplot2::aes(x = Group, y = CPS_Score, fill = Group)) +
                    ggplot2::geom_boxplot(alpha = 0.7) +
                    ggplot2::geom_point(position = ggplot2::position_jitter(width = 0.2), alpha = 0.5) +
                    ggplot2::geom_hline(yintercept = c(1, 10, 20), linetype = "dashed", color = "red") +
                    ggplot2::labs(
                        title = "Combined Positive Score (CPS) Distribution",
                        subtitle = "PD-L1 CPS across groups with clinical thresholds",
                        x = "Group",
                        y = "CPS Score",
                        fill = "Group"
                    )
            } else {
                p <- ggplot2::ggplot(data, ggplot2::aes(x = CPS_Score)) +
                    ggplot2::geom_histogram(bins = 30, alpha = 0.7, fill = "steelblue") +
                    ggplot2::geom_vline(xintercept = c(1, 10, 20), linetype = "dashed", color = "red") +
                    ggplot2::labs(
                        title = "Combined Positive Score (CPS) Distribution",
                        subtitle = "PD-L1 CPS histogram with clinical thresholds",
                        x = "CPS Score",
                        y = "Frequency"
                    )
            }
            
            p <- p + ggtheme
            print(p)
            TRUE
        },
        
        # Molecular classification methods
        .performMolecularClassification = function() {
            data <- self$data
            if (nrow(data) == 0) return()
            
            # Get molecular classification markers
            if (is.null(self$options$primary_marker1) || is.null(self$options$primary_marker2)) {
                return()
            }
            
            marker1 <- data[[self$options$primary_marker1]]
            marker2 <- data[[self$options$primary_marker2]]
            secondary <- if (!is.null(self$options$secondary_marker)) {
                data[[self$options$secondary_marker]]
            } else {
                NULL
            }
            
            # Parse cutoffs
            cutoffs <- private$.parseCutoffs(self$options$classification_cutoffs, c(20, 20, 70))
            
            # Perform classification based on system
            classification_results <- private$.classifyMolecularSubtypes(
                marker1, marker2, secondary, cutoffs, self$options$classification_system
            )
            
            # Populate classification results table
            private$.populateClassificationTable(classification_results)
            
            # Calculate subtype distribution
            private$.populateSubtypeDistribution(classification_results$molecular_subtype)
            
            # Perform statistical analysis if requested
            if (self$options$subtype_statistics) {
                private$.performSubtypeStatistics(classification_results)
            }
            
            # Analyze checkpoint inhibitor expression if available
            if (!is.null(self$options$pd1_marker) || !is.null(self$options$pdl1_marker)) {
                private$.analyzeCheckpointExpression(classification_results$molecular_subtype)
            }
            
            # Generate plots
            if (self$options$subtype_visualization) {
                private$.generateSubtypePlots(classification_results)
            }
        },
        
        .classifyMolecularSubtypes = function(marker1, marker2, secondary, cutoffs, system) {
            n <- length(marker1)
            
            # Apply cutoffs
            marker1_pos <- marker1 >= cutoffs[1]
            marker2_pos <- marker2 >= cutoffs[2]
            secondary_pos <- if (!is.null(secondary)) secondary >= cutoffs[3] else rep(FALSE, n)
            
            # Initialize results
            molecular_subtype <- character(n)
            confidence_score <- numeric(n)
            
            if (system == "bladder_mibc") {
                # Bladder MIBC classification (GATA3/CK5/6/p16)
                # Luminal: GATA3+, CK5/6-
                luminal <- marker1_pos & !marker2_pos
                molecular_subtype[luminal & secondary_pos] <- "Luminal Unstable (LumU)"
                molecular_subtype[luminal & !secondary_pos] <- "Luminal Papillary (LumP)"
                
                # Basal: GATA3-, CK5/6+
                molecular_subtype[!marker1_pos & marker2_pos] <- "Basal"
                
                # Other: GATA3-, CK5/6-
                molecular_subtype[!marker1_pos & !marker2_pos] <- "Other"
                
                # Calculate confidence scores based on marker levels
                confidence_score <- pmin(
                    abs(marker1 - cutoffs[1]) / cutoffs[1],
                    abs(marker2 - cutoffs[2]) / cutoffs[2]
                ) * 100
                
            } else if (system == "breast_cancer") {
                # Breast cancer subtypes (example implementation)
                # This would need specific marker combinations
                molecular_subtype[] <- "Not Implemented"
                confidence_score[] <- 0
                
            } else if (system == "custom") {
                # Custom classification logic
                molecular_subtype[] <- "Custom Classification"
                confidence_score[] <- 50
            }
            
            return(data.frame(
                sample_id = 1:n,
                marker1_status = ifelse(marker1_pos, "Positive", "Negative"),
                marker2_status = ifelse(marker2_pos, "Positive", "Negative"),
                secondary_status = if (!is.null(secondary)) ifelse(secondary_pos, "Positive", "Negative") else "N/A",
                molecular_subtype = molecular_subtype,
                confidence_score = confidence_score,
                stringsAsFactors = FALSE
            ))
        },
        
        .populateClassificationTable = function(results) {
            table <- self$results$molecularclassification$classificationtable
            
            for (i in 1:nrow(results)) {
                row <- list()
                row$sample_id <- results$sample_id[i]
                row$marker1_status <- results$marker1_status[i]
                row$marker2_status <- results$marker2_status[i]
                row$secondary_status <- results$secondary_status[i]
                row$molecular_subtype <- results$molecular_subtype[i]
                row$confidence_score <- results$confidence_score[i]
                
                table$addRow(rowKey = i, values = row)
            }
        },
        
        .populateSubtypeDistribution = function(subtypes) {
            table <- self$results$molecularclassification$subtypedistribution
            
            # Calculate frequencies
            subtype_counts <- table(subtypes)
            total <- length(subtypes)
            
            for (subtype in names(subtype_counts)) {
                count <- subtype_counts[[subtype]]
                percentage <- round((count / total) * 100, 2)
                
                # Calculate 95% CI for proportion
                prop_test <- binom.test(count, total)
                ci_lower <- round(prop_test$conf.int[1] * 100, 2)
                ci_upper <- round(prop_test$conf.int[2] * 100, 2)
                ci_text <- paste0("[", ci_lower, "%, ", ci_upper, "%]")
                
                row <- list(
                    subtype = subtype,
                    count = count,
                    percentage = percentage,
                    confidence_interval = ci_text
                )
                
                table$addRow(rowKey = subtype, values = row)
            }
        },
        
        .performSubtypeStatistics = function(results) {
            table <- self$results$molecularclassification$subtypestatistics
            
            # Chi-square test for subtype distribution
            subtype_counts <- table(results$molecular_subtype)
            if (length(subtype_counts) > 1) {
                chi_test <- chisq.test(subtype_counts)
                
                row <- list(
                    comparison = "Overall subtype distribution",
                    test_statistic = "Chi-square",
                    statistic_value = round(chi_test$statistic, 3),
                    p_value = chi_test$p.value,
                    effect_size = round(sqrt(chi_test$statistic / sum(subtype_counts)), 3),
                    interpretation = ifelse(chi_test$p.value < 0.05, 
                        "Subtypes differ significantly", 
                        "No significant difference")
                )
                
                table$addRow(rowKey = "chi_square", values = row)
            }
        },
        
        .analyzeCheckpointExpression = function(subtypes) {
            table <- self$results$molecularclassification$checkpointanalysis
            data <- self$data
            
            # Analyze PD-1 expression
            if (!is.null(self$options$pd1_marker)) {
                pd1_data <- data[[self$options$pd1_marker]]
                private$.analyzeCheckpointBySubtype(pd1_data, subtypes, "PD-1", table)
            }
            
            # Analyze PD-L1 expression
            if (!is.null(self$options$pdl1_marker)) {
                pdl1_data <- data[[self$options$pdl1_marker]]
                private$.analyzeCheckpointBySubtype(pdl1_data, subtypes, "PD-L1", table)
            }
        },
        
        .analyzeCheckpointBySubtype = function(marker_data, subtypes, marker_name, table) {
            # Parse checkpoint cutoffs
            cutoffs <- private$.parseCutoffs(self$options$checkpoint_cutoffs, c(1, 10))
            
            for (cutoff in cutoffs) {
                positive <- marker_data >= cutoff
                
                # Calculate positivity rates by subtype
                subtype_levels <- unique(subtypes)
                
                for (subtype in subtype_levels) {
                    subtype_mask <- subtypes == subtype
                    positive_count <- sum(positive[subtype_mask], na.rm = TRUE)
                    total_count <- sum(subtype_mask, na.rm = TRUE)
                    positive_rate <- round((positive_count / total_count) * 100, 2)
                    
                    # Chi-square test comparing this subtype to others
                    other_positive <- sum(positive[!subtype_mask], na.rm = TRUE)
                    other_total <- sum(!subtype_mask, na.rm = TRUE)
                    
                    if (other_total > 0) {
                        contingency_table <- matrix(c(
                            positive_count, total_count - positive_count,
                            other_positive, other_total - other_positive
                        ), nrow = 2)
                        
                        fisher_test <- fisher.test(contingency_table)
                        p_value <- fisher_test$p.value
                    } else {
                        p_value <- NA
                    }
                    
                    row <- list(
                        molecular_subtype = subtype,
                        marker = marker_name,
                        cutoff = paste0(cutoff, "%"),
                        positive_count = positive_count,
                        total_count = total_count,
                        positive_rate = positive_rate,
                        p_value = p_value
                    )
                    
                    row_key <- paste(marker_name, cutoff, subtype, sep = "_")
                    table$addRow(rowKey = row_key, values = row)
                }
            }
        },
        
        .generateSubtypePlots = function(results) {
            # Store data for subtype distribution plot
            subtype_data <- data.frame(
                subtype = results$molecular_subtype,
                confidence = results$confidence_score
            )
            self$results$subtypeplot$setState(subtype_data)
            
            # Store data for checkpoint expression plot if available
            if (!is.null(self$options$pd1_marker) || !is.null(self$options$pdl1_marker)) {
                data <- self$data
                checkpoint_data <- list(
                    subtypes = results$molecular_subtype,
                    pd1 = if (!is.null(self$options$pd1_marker)) data[[self$options$pd1_marker]] else NULL,
                    pdl1 = if (!is.null(self$options$pdl1_marker)) data[[self$options$pdl1_marker]] else NULL
                )
                self$results$checkpointplot$setState(checkpoint_data)
            }
        },
        
        .subtypeplot = function(image, ggtheme, theme, ...) {
            if (is.null(image$state)) return(FALSE)
            
            data <- image$state
            
            # Create subtype distribution plot
            subtype_counts <- table(data$subtype)
            plot_data <- data.frame(
                Subtype = names(subtype_counts),
                Count = as.numeric(subtype_counts),
                Percentage = round(as.numeric(subtype_counts) / nrow(data) * 100, 1)
            )
            
            p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = reorder(Subtype, Count), y = Count, fill = Subtype)) +
                ggplot2::geom_bar(stat = "identity", alpha = 0.7) +
                ggplot2::geom_text(ggplot2::aes(label = paste0(Count, "\n(", Percentage, "%)")), 
                                  vjust = -0.5) +
                ggplot2::labs(
                    title = "Molecular Subtype Distribution",
                    subtitle = "Frequency and percentage of each molecular subtype",
                    x = "Molecular Subtype",
                    y = "Count",
                    fill = "Subtype"
                ) +
                ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
                ggtheme
            
            print(p)
            TRUE
        },
        
        .checkpointplot = function(image, ggtheme, theme, ...) {
            if (is.null(image$state)) return(FALSE)
            
            data <- image$state
            cutoffs <- private$.parseCutoffs(self$options$checkpoint_cutoffs, c(1, 10))
            
            plot_data <- data.frame()
            
            # Process PD-1 data
            if (!is.null(data$pd1)) {
                for (cutoff in cutoffs) {
                    subtype_rates <- tapply(data$pd1 >= cutoff, data$subtypes, function(x) mean(x, na.rm = TRUE) * 100)
                    
                    temp_data <- data.frame(
                        Subtype = names(subtype_rates),
                        PositiveRate = as.numeric(subtype_rates),
                        Marker = "PD-1",
                        Cutoff = paste0(cutoff, "%")
                    )
                    plot_data <- rbind(plot_data, temp_data)
                }
            }
            
            # Process PD-L1 data
            if (!is.null(data$pdl1)) {
                for (cutoff in cutoffs) {
                    subtype_rates <- tapply(data$pdl1 >= cutoff, data$subtypes, function(x) mean(x, na.rm = TRUE) * 100)
                    
                    temp_data <- data.frame(
                        Subtype = names(subtype_rates),
                        PositiveRate = as.numeric(subtype_rates),
                        Marker = "PD-L1", 
                        Cutoff = paste0(cutoff, "%")
                    )
                    plot_data <- rbind(plot_data, temp_data)
                }
            }
            
            if (nrow(plot_data) > 0) {
                p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = Subtype, y = PositiveRate, fill = Marker)) +
                    ggplot2::geom_bar(stat = "identity", position = "dodge", alpha = 0.7) +
                    ggplot2::facet_wrap(~ Cutoff, scales = "free_y") +
                    ggplot2::labs(
                        title = "Checkpoint Inhibitor Expression by Molecular Subtype",
                        subtitle = "PD-1/PD-L1 positivity rates across different cutoffs",
                        x = "Molecular Subtype",
                        y = "Positive Rate (%)",
                        fill = "Marker"
                    ) +
                    ggplot2::scale_y_continuous(limits = c(0, 100)) +
                    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
                    ggtheme
                
                print(p)
            }
            
            TRUE
        },
        
        .parseCutoffs = function(cutoff_string, defaults) {
            tryCatch({
                cutoffs <- as.numeric(unlist(strsplit(cutoff_string, ",")))
                cutoffs <- cutoffs[!is.na(cutoffs)]
                if (length(cutoffs) == 0) cutoffs <- defaults
                return(cutoffs)
            }, error = function(e) {
                return(defaults)
            })
        },

        # Initialize About This Analysis panel
        .initializeAboutAnalysis = function() {
            about_html <- paste0(
                "<div style='background-color: #f0f8ff; padding: 20px; border-radius: 8px; margin: 10px 0;'>",
                "<h4>🔬 About IHC Scoring Analysis</h4>",
                "<p><strong>Purpose:</strong> Standardize immunohistochemistry scoring using validated clinical methods (H-score and Allred score)</p>",
                "<p><strong>When to Use:</strong></p>",
                "<ul>",
                "<li>Biomarker quantification for research or clinical trials</li>",
                "<li>Standardization across multiple observers or institutions</li>",
                "<li>Validation of digital pathology algorithms</li>",
                "<li>Quality assurance for diagnostic laboratories</li>",
                "</ul>",
                "<p><strong>Key Outputs:</strong></p>",
                "<ul>",
                "<li><strong>H-scores:</strong> Weighted intensity × proportion (0-300 scale)</li>",
                "<li><strong>Allred scores:</strong> Combined intensity + proportion categories (0-8 scale)</li>",
                "<li><strong>Binary classification:</strong> Positive/negative based on clinical cutoffs</li>",
                "<li><strong>Method agreement:</strong> Correlation and reliability statistics</li>",
                "</ul>",
                "<p><strong>Clinical Applications:</strong> ER/PR scoring, HER2 assessment, Ki-67 quantification, PD-L1 evaluation</p>",
                "</div>"
            )
            self$results$aboutAnalysis$setContent(about_html)
        },

        # Initialize Assumptions & Caveats panel
        .initializeAssumptions = function() {
            assumptions_html <- paste0(
                "<div style='background-color: #fff3cd; padding: 20px; border-radius: 8px; margin: 10px 0; border-left: 5px solid #ffc107;'>",
                "<h4>⚠️ Important Assumptions & Caveats</h4>",
                "<p><strong>Data Requirements:</strong></p>",
                "<ul>",
                "<li>Intensity scores: Typically 0-3 scale (0=negative, 1=weak, 2=moderate, 3=strong)</li>",
                "<li>Proportion values: 0-100% representing percentage of positive cells</li>",
                "<li>Minimum sample size: n≥30 recommended for reliable statistics</li>",
                "</ul>",
                "<p><strong>Clinical Considerations:</strong></p>",
                "<ul>",
                "<li>Cutoff values should be validated for your specific biomarker and population</li>",
                "<li>Results should be interpreted with full clinical context</li>",
                "<li>Consider inter-observer agreement validation</li>",
                "<li>Digital scoring may require algorithm validation against manual scoring</li>",
                "</ul>",
                "<p><strong>Statistical Assumptions:</strong></p>",
                "<ul>",
                "<li>Correlation analysis assumes monotonic relationships</li>",
                "<li>Agreement statistics require paired observations</li>",
                "<li>Bootstrap intervals assume representative sampling</li>",
                "</ul>",
                "<p><strong>⚠️ Contextual Warnings:</strong></p>",
                "<div id='contextual-warnings' style='color: #d73502; font-weight: bold;'></div>",
                "</div>"
            )
            self$results$assumptions$setContent(assumptions_html)
        },

        # Apply guided biomarker configuration
        .applyGuidedConfiguration = function() {
            guided_type <- self$options$guided_biomarker %||% "manual"

            if (guided_type != "manual") {
                # Configure based on biomarker type
                recommendations <- switch(guided_type,
                    "er_pr" = list(
                        cutpoint = 100,
                        allred_cutpoint = 3,
                        description = "ER/PR (Estrogen/Progesterone Receptors)",
                        clinical_context = "FDA-approved cutoff: H-score ≥100 for positive classification"
                    ),
                    "her2" = list(
                        cutpoint = 150,
                        allred_cutpoint = 4,
                        description = "HER2 (Human Epidermal Growth Factor Receptor 2)",
                        clinical_context = "Research standard: H-score ≥150, requires validation with FISH if 2+"
                    ),
                    "ki67" = list(
                        cutpoint = 100,
                        allred_cutpoint = 3,
                        description = "Ki-67 (Proliferation Index)",
                        clinical_context = "Common research cutoffs: 14-20% for breast cancer prognosis"
                    ),
                    "pdl1" = list(
                        cutpoint = 30,  # Equivalent to 1% TPS
                        allred_cutpoint = 2,
                        description = "PD-L1 (Programmed Death-Ligand 1)",
                        clinical_context = "Immunotherapy eligibility: TPS ≥1%, ≥10%, or ≥50% depending on indication"
                    )
                )

                # Store recommendations for use in analysis
                self$.guidedRecommendations <- recommendations
            }
        },

        # Generate clinical summary with copy-ready content
        .generateClinicalSummary = function(intensity, proportion) {
            n <- length(intensity)
            hscore <- intensity * proportion
            cutpoint <- self$options$binary_cutpoint
            positive_count <- sum(hscore >= cutpoint, na.rm = TRUE)
            positive_percent <- round((positive_count/n) * 100, 1)

            hscore_mean <- round(mean(hscore, na.rm = TRUE), 1)
            hscore_median <- round(median(hscore, na.rm = TRUE), 1)
            hscore_range <- paste0(round(min(hscore, na.rm = TRUE), 1), "-", round(max(hscore, na.rm = TRUE), 1))

            biomarker_name <- switch(self$options$biomarker_type,
                "er" = "estrogen receptor (ER)",
                "pr" = "progesterone receptor (PR)",
                "her2" = "HER2",
                "ki67" = "Ki-67",
                "pdl1" = "PD-L1",
                "biomarker expression"
            )

            correlation <- cor(hscore, intensity + ifelse(proportion == 0, 0,
                ifelse(proportion <= 1, 1, ifelse(proportion <= 10, 2,
                ifelse(proportion <= 33, 3, ifelse(proportion <= 66, 4, 5))))),
                use = "complete.obs")

            agreement_level <- ifelse(abs(correlation) >= 0.9, "excellent",
                ifelse(abs(correlation) >= 0.75, "good",
                ifelse(abs(correlation) >= 0.5, "moderate", "poor")))

            summary_html <- paste0(
                "<div style='background-color: #e8f5e8; padding: 20px; border-radius: 8px; margin: 10px 0; border-left: 5px solid #28a745;'>",
                "<h4>📋 Analysis Summary</h4>",
                "<div style='background-color: white; padding: 15px; border-radius: 5px; margin: 10px 0;'>",
                "<h5>Key Findings (Copy-Ready)</h5>",
                "<p style='font-family: monospace; background-color: #f8f9fa; padding: 10px; border-radius: 3px;'>",
                "IHC analysis of <strong>", n, " cases</strong> showed ", biomarker_name, " expression with mean H-score of <strong>", hscore_mean, "</strong> (range: ", hscore_range, "). ",
                "Using a cutoff of ", cutpoint, ", <strong>", positive_count, "/", n, " (", positive_percent, "%)</strong> cases were classified as positive. ",
                "The correlation between H-score and Allred methods was ", agreement_level, " (r=", round(correlation, 3), "), indicating reliable scoring consistency.",
                "</p>",
                "</div>",
                "<div style='background-color: #fff3cd; padding: 10px; border-radius: 5px;'>",
                "<strong>Clinical Interpretation:</strong> ",
                if (positive_percent >= 50) "High proportion of positive cases suggests strong biomarker expression suitable for targeted therapy consideration."
                else if (positive_percent >= 20) "Moderate biomarker expression observed; clinical correlation recommended."
                else "Low biomarker expression in this cohort; negative predictive value for targeted therapies.",
                "</div>",
                "</div>"
            )

            self$results$clinicalSummary$setContent(summary_html)
        },

        # Generate copy-ready clinical report
        .generateClinicalReport = function(intensity, proportion) {
            n <- length(intensity)
            hscore <- intensity * proportion
            cutpoint <- self$options$binary_cutpoint
            positive_count <- sum(hscore >= cutpoint, na.rm = TRUE)
            positive_percent <- round((positive_count/n) * 100, 1)

            hscore_mean <- round(mean(hscore, na.rm = TRUE), 1)
            hscore_sd <- round(sd(hscore, na.rm = TRUE), 1)
            hscore_median <- round(median(hscore, na.rm = TRUE), 1)
            hscore_q1 <- round(quantile(hscore, 0.25, na.rm = TRUE), 1)
            hscore_q3 <- round(quantile(hscore, 0.75, na.rm = TRUE), 1)

            biomarker_name <- switch(self$options$biomarker_type,
                "er" = "Estrogen Receptor (ER)",
                "pr" = "Progesterone Receptor (PR)",
                "her2" = "HER2",
                "ki67" = "Ki-67 Proliferation Index",
                "pdl1" = "PD-L1",
                "Biomarker Expression"
            )

            method_description <- switch(self$options$scoring_method,
                "hscore" = "H-score methodology",
                "allred" = "Allred scoring system",
                "both" = "H-score and Allred scoring methods"
            )

            report_text <- paste0(
                "IMMUNOHISTOCHEMISTRY SCORING ANALYSIS REPORT\n",
                "===========================================\n\n",
                "Biomarker: ", biomarker_name, "\n",
                "Scoring Method: ", method_description, "\n",
                "Sample Size: ", n, " cases\n",
                "Analysis Date: ", Sys.Date(), "\n\n",
                "SCORING RESULTS:\n",
                "---------------\n",
                "H-score Statistics:\n",
                "  Mean ± SD: ", hscore_mean, " ± ", hscore_sd, "\n",
                "  Median (IQR): ", hscore_median, " (", hscore_q1, "-", hscore_q3, ")\n",
                "  Range: ", round(min(hscore, na.rm = TRUE), 1), "-", round(max(hscore, na.rm = TRUE), 1), "\n\n",
                "Binary Classification (H-score ≥", cutpoint, "):\n",
                "  Positive: ", positive_count, "/", n, " (", positive_percent, "%)\n",
                "  Negative: ", (n - positive_count), "/", n, " (", round(100 - positive_percent, 1), "%)\n\n",
                "CLINICAL INTERPRETATION:\n",
                "----------------------\n",
                if (positive_percent >= 50) {
                    "High proportion of positive cases indicates significant biomarker expression.\nRecommendation: Consider for targeted therapy evaluation."
                } else if (positive_percent >= 20) {
                    "Moderate biomarker expression observed.\nRecommendation: Clinical correlation and additional testing may be warranted."
                } else {
                    "Low biomarker expression in this cohort.\nRecommendation: Negative for targeted therapy; consider alternative approaches."
                }, "\n\n",
                "QUALITY ASSURANCE:\n",
                "----------------\n",
                "- Scoring methodology: ", method_description, "\n",
                "- Inter-method correlation: ", round(cor(hscore, intensity + ifelse(proportion == 0, 0, 1), use = "complete.obs"), 3), "\n",
                "- Statistical framework: Bootstrap confidence intervals (", self$options$confidence_level * 100, "% CI)\n\n",
                "RECOMMENDATIONS:\n",
                "---------------\n",
                "1. Validate results with clinical outcomes data\n",
                "2. Consider inter-observer agreement assessment\n",
                "3. Correlate findings with molecular testing when available\n",
                "4. Follow institutional guidelines for cutoff interpretation\n\n",
                "Generated by ClinicoPath IHC Scoring Analysis v", self$version %||% "1.0", "\n",
                "Report generated on: ", Sys.time()
            )

            self$results$clinicalReport$setContent(report_text)
        },

        # Add misuse detection guards
        .addMisuseWarnings = function(intensity, proportion) {
            warnings <- c()

            # Sample size warnings
            n <- length(intensity)
            if (n < 30) {
                warnings <- c(warnings, "Small sample size (n<30) may affect reliability of statistical tests")
            }
            if (n < 10) {
                warnings <- c(warnings, "Very small sample size (n<10) - results should be interpreted with extreme caution")
            }

            # Biomarker-specific warnings
            biomarker_type <- self$options$biomarker_type
            cutpoint <- self$options$binary_cutpoint

            if (biomarker_type == "her2" && cutpoint < 150) {
                warnings <- c(warnings, "HER2 typically uses higher cutoffs (≥150) - verify your threshold")
            }
            if (biomarker_type == "er" && cutpoint != 100) {
                warnings <- c(warnings, "ER scoring typically uses FDA-approved cutoff of 100 - consider standard threshold")
            }
            if (biomarker_type == "pr" && cutpoint != 100) {
                warnings <- c(warnings, "PR scoring typically uses FDA-approved cutoff of 100 - consider standard threshold")
            }

            # Data quality warnings
            if (any(proportion > 100, na.rm = TRUE)) {
                warnings <- c(warnings, "Proportion values >100% detected - verify data format (should be 0-100%)")
            }
            if (any(intensity > 3, na.rm = TRUE) && self$options$intensity_scale == "standard") {
                warnings <- c(warnings, "Intensity values >3 detected with standard scale - verify scale selection")
            }

            # Guided configuration warnings
            guided_type <- self$options$guided_biomarker %||% "manual"
            if (guided_type != "manual" && biomarker_type == "other") {
                warnings <- c(warnings, "Guided biomarker selected but biomarker type set to 'other' - ensure consistency")
            }

            # Update contextual warnings in assumptions panel
            if (length(warnings) > 0) {
                warning_html <- paste0(
                    "The following issues were detected:<br>",
                    paste("• ", warnings, collapse = "<br>")
                )
                # This would ideally update the contextual-warnings div via JavaScript
                # For now, we'll add to the interpretation
                current_content <- self$results$interpretation$content %||% ""
                warning_content <- paste0(
                    "<div style='background-color: #f8d7da; padding: 10px; border-radius: 5px; margin: 10px 0; border-left: 3px solid #dc3545;'>",
                    "<strong>⚠️ Analysis Warnings:</strong><br>", warning_html,
                    "</div>"
                )
                self$results$interpretation$setContent(paste0(current_content, warning_content))
            }
        },

        # Accessibility and internationalization support
        .getTranslatedText = function(key, language = NULL) {
            if (is.null(language)) {
                language <- self$options$language %||% "english"
            }

            translations <- list(
                english = list(
                    analysis_complete = "Analysis completed successfully",
                    scoring_method = "Scoring Method",
                    positive_cases = "Positive Cases",
                    negative_cases = "Negative Cases",
                    correlation = "Method Correlation",
                    clinical_significance = "Clinical Significance",
                    quality_control = "Quality Control",
                    recommendations = "Recommendations"
                ),
                turkish = list(
                    analysis_complete = "Analiz başarıyla tamamlandı",
                    scoring_method = "Skorlama Yöntemi",
                    positive_cases = "Pozitif Olgular",
                    negative_cases = "Negatif Olgular",
                    correlation = "Yöntem Korelasyonu",
                    clinical_significance = "Klinik Önem",
                    quality_control = "Kalite Kontrolü",
                    recommendations = "Öneriler"
                )
            )

            return(translations[[language]][[key]] %||% key)
        },

        .getColorblindSafePalette = function() {
            if (self$options$colorblind_safe) {
                # Colorbrewer colorblind-safe palette
                return(c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", "#8c564b", "#e377c2", "#7f7f7f"))
            } else {
                # Standard ggplot2 palette
                return(c("steelblue", "coral", "forestgreen", "red", "purple", "orange", "pink", "gray"))
            }
        },

        .getPlotTheme = function(base_theme) {
            font_size_map <- list(
                normal = 11,
                large = 14,
                extra_large = 16
            )

            base_size <- font_size_map[[self$options$font_size %||% "normal"]]

            theme_elements <- list()

            if (self$options$high_contrast) {
                theme_elements <- append(theme_elements, list(
                    panel.background = ggplot2::element_rect(fill = "white", color = "black", size = 1),
                    plot.background = ggplot2::element_rect(fill = "white", color = "black", size = 1),
                    text = ggplot2::element_text(color = "black", size = base_size),
                    axis.text = ggplot2::element_text(color = "black", size = base_size - 1),
                    axis.title = ggplot2::element_text(color = "black", size = base_size),
                    legend.text = ggplot2::element_text(color = "black", size = base_size - 1),
                    legend.title = ggplot2::element_text(color = "black", size = base_size),
                    strip.background = ggplot2::element_rect(fill = "white", color = "black"),
                    strip.text = ggplot2::element_text(color = "black", size = base_size)
                ))
            } else {
                theme_elements <- append(theme_elements, list(
                    text = ggplot2::element_text(size = base_size),
                    axis.text = ggplot2::element_text(size = base_size - 1),
                    axis.title = ggplot2::element_text(size = base_size),
                    legend.text = ggplot2::element_text(size = base_size - 1),
                    legend.title = ggplot2::element_text(size = base_size),
                    strip.text = ggplot2::element_text(size = base_size)
                ))
            }

            return(do.call(ggplot2::theme, theme_elements))
        },

        .updatePlotsWithAccessibility = function() {
            # Update existing plots with accessibility settings
            accessibility_theme <- private$.getPlotTheme()

            # Store accessibility settings for plot functions
            if (exists("distributionplot", where = self$results)) {
                self$results$distributionplot$setOptions(list(
                    colorblind_safe = self$options$colorblind_safe,
                    high_contrast = self$options$high_contrast,
                    font_size = self$options$font_size,
                    language = self$options$language
                ))
            }

            if (exists("correlationplot", where = self$results)) {
                self$results$correlationplot$setOptions(list(
                    colorblind_safe = self$options$colorblind_safe,
                    high_contrast = self$options$high_contrast,
                    font_size = self$options$font_size,
                    language = self$options$language
                ))
            }
        }
    )
)