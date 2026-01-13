#' @title Pathology Agreement Analysis Class
#'
#' @description R6 class for performing pathology agreement analysis.
#' @name pathologyagreementClass
#' @importFrom R6 R6Class
pathologyagreementClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "pathologyagreementClass",
    inherit = pathologyagreementBase,
    private = list(
        # Clinical constants based on established guidelines
        .CLINICAL_CONSTANTS = list(
            ICC_EXCELLENT = 0.90,           # Landis & Koch 1977
            ICC_GOOD = 0.75,                # Cicchetti 1994
            ICC_MODERATE = 0.50,
            CORRELATION_VERY_STRONG = 0.90,
            CORRELATION_STRONG = 0.70,
            CORRELATION_MODERATE = 0.50,
            CORRELATION_WEAK = 0.30,
            MIN_SAMPLE_PATHOLOGY = 30,      # Minimum for pathology validation studies
            BIOMARKER_MIN_RANGE = 0,        # Biomarker percentage range
            BIOMARKER_MAX_RANGE = 100,
            BOOTSTRAP_RECOMMENDED = 2000    # FDA guidance for high-stakes validation
        ),

        # Collect warnings/notices as HTML instead of dynamic Notice objects
        .warnings = list(),

        .escapeVar = function(x) {
            # Safely escape variable names for data.frame access
            # Handles variables with spaces, special characters, etc.
            if (is.null(x) || length(x) == 0) return(NULL)
            gsub("[^A-Za-z0-9_]+", "_", make.names(x))
        },

        .addWarning = function(type, message) {
            # Add warning to list instead of using Notice objects
            # type: "ERROR", "STRONG_WARNING", "WARNING", or "INFO"

            style <- switch(type,
                "ERROR" = "background-color: #f8d7da; border-left: 4px solid #dc3545; color: #721c24;",
                "STRONG_WARNING" = "background-color: #fff3cd; border-left: 4px solid #ff6b6b; color: #856404;",
                "WARNING" = "background-color: #fff3cd; border-left: 4px solid #ffc107; color: #856404;",
                "INFO" = "background-color: #d1ecf1; border-left: 4px solid #17a2b8; color: #0c5460;",
                "background-color: #f8f9fa; border-left: 4px solid #6c757d; color: #383d41;"
            )

            icon <- switch(type,
                "ERROR" = "❌",
                "STRONG_WARNING" = "⚠️",
                "WARNING" = "⚠️",
                "INFO" = "ℹ️",
                "•"
            )

            label <- switch(type,
                "ERROR" = "ERROR",
                "STRONG_WARNING" = "STRONG WARNING",
                "WARNING" = "WARNING",
                "INFO" = "INFO",
                "NOTICE"
            )

            html <- sprintf(
                "<div style='padding: 12px; margin: 8px 0; border-radius: 4px; %s'>
                <strong>%s %s:</strong> %s
                </div>",
                style, icon, label, message
            )

            private$.warnings[[length(private$.warnings) + 1]] <- list(
                type = type,
                message = message,
                html = html
            )
        },

        .renderWarnings = function() {
            # Render all collected warnings as HTML
            if (length(private$.warnings) == 0) {
                return("")
            }

            html_parts <- sapply(private$.warnings, function(w) w$html)
            paste(html_parts, collapse = "\n")
        },
        
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

            # Set up multi-method tables if additional methods are provided
            if (!is.null(self$options$additional_methods) && length(self$options$additional_methods) > 0) {
                private$.populateCorrelationMatrixTable()
                private$.populateOverallICCTable()
            }

            # Set plots visible based on user option
            self$results$blandaltmanplot$setVisible(self$options$show_plots)
            self$results$scatterplot$setVisible(self$options$show_plots)
        },
        
        .run = function() {
            # Initialize warnings list
            private$.warnings <- list()

            # ====================================================================
            # CRITICAL ERRORS
            # ====================================================================

            # ERROR: Missing required variables
            if (is.null(self$options$dep1) || is.null(self$options$dep2)) {
                private$.addWarning("ERROR",
                    "Method 1 and Method 2 variables are required. Please select both variables to begin analysis.")
                self$results$warnings$setContent(private$.renderWarnings())
                return()
            }

            data <- self$data

            # ERROR: Empty dataset
            if (nrow(data) == 0) {
                private$.addWarning("ERROR",
                    "Dataset is empty. Please provide data with observations.")
                self$results$warnings$setContent(private$.renderWarnings())
                return()
            }

            # ERROR: Check package availability EARLY
            if (!requireNamespace('psych', quietly = TRUE)) {
                private$.addWarning("ERROR",
                    'Required R package "psych" not installed. Install via: install.packages("psych")')
                self$results$warnings$setContent(private$.renderWarnings())
                return()
            }

            if (!requireNamespace('epiR', quietly = TRUE)) {
                private$.addWarning("ERROR",
                    'Required R package "epiR" not installed. Install via: install.packages("epiR")')
                self$results$warnings$setContent(private$.renderWarnings())
                return()
            }

            # Get variables with safe conversion (handles factors/labelled data)
            method1 <- jmvcore::toNumeric(data[[self$options$dep1]])
            method2 <- jmvcore::toNumeric(data[[self$options$dep2]])

            # Handle missing values based on option
            n_removed <- 0
            if (self$options$missing_data == "listwise") {
                complete_cases <- complete.cases(method1, method2)
                n_total <- length(method1)
                method1 <- method1[complete_cases]
                method2 <- method2[complete_cases]
                n_removed <- n_total - length(method1)
            }

            # ERROR: Insufficient data after cleaning
            if (length(method1) < 3) {
                private$.addWarning("ERROR",
                    sprintf('Insufficient complete observations (n=%d). At least 3 paired observations required for agreement analysis.', length(method1)))
                self$results$warnings$setContent(private$.renderWarnings())
                return()
            }

            # ====================================================================
            # STRONG WARNINGS
            # ====================================================================

            n <- length(method1)

            # STRONG_WARNING: Very small sample (clinical threshold)
            if (n < 10) {
                private$.addWarning("STRONG_WARNING",
                    sprintf('Very small sample (n=%d). Results may be unreliable. Minimum n=30 recommended for pathology validation studies.', n))
            }

            # ====================================================================
            # WARNINGS
            # ====================================================================

            # WARNING: Sample size below recommended minimum
            if (n >= 10 && n < private$.CLINICAL_CONSTANTS$MIN_SAMPLE_PATHOLOGY) {
                private$.addWarning("WARNING",
                    sprintf('Sample size (n=%d) below recommended minimum (n=30) for pathology validation studies. Consider increasing sample size for robust estimates.', n))
            }

            # WARNING: Biomarker range validation
            if (self$options$clinical_preset == "biomarker_platforms") {
                range_check1 <- any(method1 < private$.CLINICAL_CONSTANTS$BIOMARKER_MIN_RANGE |
                                   method1 > private$.CLINICAL_CONSTANTS$BIOMARKER_MAX_RANGE, na.rm = TRUE)
                range_check2 <- any(method2 < private$.CLINICAL_CONSTANTS$BIOMARKER_MIN_RANGE |
                                   method2 > private$.CLINICAL_CONSTANTS$BIOMARKER_MAX_RANGE, na.rm = TRUE)

                if (range_check1 || range_check2) {
                    private$.addWarning("WARNING",
                        'Some biomarker values outside typical 0-100% range. Please verify data scaling (e.g., ensure percentages not decimals).')
                }
            }

            # WARNING: Bootstrap recommendation for high-stakes studies
            if (self$options$bootstrap_n < private$.CLINICAL_CONSTANTS$BOOTSTRAP_RECOMMENDED &&
                self$options$clinical_preset %in% c("multisite_validation", "ai_pathologist")) {
                private$.addWarning("WARNING",
                    sprintf('Bootstrap replicates (n=%d) below FDA-recommended threshold (n=2000) for high-stakes validation studies. Consider increasing for regulatory submissions.', self$options$bootstrap_n))
            }

            # ====================================================================
            # PERFORM ANALYSIS
            # ====================================================================

            # Perform standard 2-method analysis
            private$.performAgreementAnalysis(method1, method2)
            private$.performCorrelationAnalysis(method1, method2)
            private$.generatePlots(method1, method2)

            # Perform multi-method analysis if additional methods provided
            if (!is.null(self$options$additional_methods) && length(self$options$additional_methods) > 0) {
                if (length(self$options$additional_methods) >= 1) {
                    all_methods <- private$.extractAllMethods(data)
                    if (ncol(all_methods) >= 3) {
                        private$.performMultiMethodAnalysis(all_methods)
                    }
                }
            }

            private$.generateInterpretation(method1, method2)

            # Generate copy-ready report, glossary, and ICC guide (if interpretation enabled)
            if (self$options$show_interpretation) {
                private$.generateReportSentences(method1, method2)
                private$.generateStatisticalGlossary()
                private$.generateICCSelectionGuide()
            }

            # Generate plain-language summary (if enabled)
            if (self$options$show_summary) {
                private$.generatePlainLanguageSummary(method1, method2)
            }

            # Generate educational explanations (if enabled)
            if (self$options$show_explanations) {
                private$.generateEducationalExplanations()
            }

            # ====================================================================
            # INFO NOTICES
            # ====================================================================

            # INFO: Missing data summary (if any removed)
            if (n_removed > 0) {
                private$.addWarning("INFO",
                    sprintf('Listwise deletion removed %d cases (%.1f%%) with missing values. Analysis based on %d complete observations.', n_removed, 100*n_removed/(n+n_removed), n))
            }

            # INFO: Analysis completion summary
            n_methods <- if (!is.null(self$options$additional_methods)) length(self$options$additional_methods) + 2 else 2
            private$.addWarning("INFO",
                sprintf('Agreement analysis completed: %d methods compared, %d complete observations analyzed.', n_methods, n))

            # Render all collected warnings to HTML
            self$results$warnings$setContent(private$.renderWarnings())
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
            table$addColumn(name = 'ci_lower', title = 'CI Lower', type = 'number', format = 'zto')
            table$addColumn(name = 'ci_upper', title = 'CI Upper', type = 'number', format = 'zto')
            table$addColumn(name = 'interpretation', title = 'Strength', type = 'text')
        },
        
        .performAgreementAnalysis = function(method1, method2) {
            # Calculate ICC using user-selected type
            if (requireNamespace('psych', quietly = TRUE)) {
                data_for_icc <- data.frame(Method1 = method1, Method2 = method2)
                icc_result <- psych::ICC(data_for_icc)

                # Select the appropriate ICC based on user choice
                # ICC(3,1) for consistency (relative agreement), ICC(2,1) for absolute agreement
                if (self$options$icc_type == "consistency") {
                    icc_index <- 6  # ICC(3,1) - Consistency (fixed raters, relative agreement)
                    icc_label <- "ICC(3,1) - Consistency"
                    icc_interpretation <- "relative agreement between methods"
                } else { # absolute
                    icc_index <- 4  # ICC(2,1) - Absolute Agreement (random raters, absolute values)
                    icc_label <- "ICC(2,1) - Absolute Agreement"
                    icc_interpretation <- "absolute value concordance between methods"
                }

                icc_value <- icc_result$results$ICC[icc_index]
                icc_lower <- icc_result$results$`lower bound`[icc_index]
                icc_upper <- icc_result$results$`upper bound`[icc_index]

                # STRONG_WARNING: Negative ICC indicates severe reliability issues
                if (!is.na(icc_value) && icc_value < 0) {
                    private$.addWarning("STRONG_WARNING",
                        sprintf('Negative ICC (%.3f) indicates severe reliability issues, often due to model assumption errors or greater within-subject than between-subject variance.', icc_value))
                }

                icc_interp <- ifelse(icc_value >= private$.CLINICAL_CONSTANTS$ICC_EXCELLENT, "Excellent reliability",
                             ifelse(icc_value >= private$.CLINICAL_CONSTANTS$ICC_GOOD, "Good reliability",
                             ifelse(icc_value >= private$.CLINICAL_CONSTANTS$ICC_MODERATE, "Moderate reliability", "Poor reliability")))
            } else {
                icc_value <- icc_lower <- icc_upper <- NA
                icc_interp <- "psych package required"
            }

            # Calculate Concordance Correlation Coefficient (CCC)
            if (requireNamespace('epiR', quietly = TRUE)) {
                ccc_result <- epiR::epi.ccc(method1, method2, ci = "z-transform", conf.level = self$options$conf_level)
                ccc_value <- ccc_result$rho.c$est
                ccc_lower <- ccc_result$rho.c$lower
                ccc_upper <- ccc_result$rho.c$upper

                # McBride 2005 interpretation
                ccc_interp <- ifelse(ccc_value >= 0.99, "Almost perfect",
                             ifelse(ccc_value >= 0.95, "Substantial",
                             ifelse(ccc_value >= 0.90, "Moderate", "Poor")))
            } else {
                ccc_value <- ccc_lower <- ccc_upper <- NA
                ccc_interp <- "epiR package required"
            }

            # Bland-Altman statistics
            differences <- method1 - method2
            means <- (method1 + method2) / 2
            mean_diff <- mean(differences)
            sd_diff <- sd(differences)
            n_obs <- length(differences)
            se_diff <- sd_diff / sqrt(n_obs)
            ci_factor <- qt(1 - (1 - self$options$conf_level)/2, df = n_obs - 1)

            mean_diff_lower <- mean_diff - ci_factor * se_diff
            mean_diff_upper <- mean_diff + ci_factor * se_diff

            # FIX 1: Limits of Agreement with 95% Confidence Intervals (Bland & Altman 1999)
            loa_lower <- mean_diff - 1.96 * sd_diff
            loa_upper <- mean_diff + 1.96 * sd_diff

            # Calculate CIs for LoA: SE(LoA) = SD × sqrt(3/n)
            se_loa <- sd_diff * sqrt(3 / n_obs)
            ci_factor_loa <- qt(1 - (1 - self$options$conf_level)/2, df = n_obs - 1)

            loa_lower_ci_low <- loa_lower - ci_factor_loa * se_loa
            loa_lower_ci_high <- loa_lower + ci_factor_loa * se_loa

            loa_upper_ci_low <- loa_upper - ci_factor_loa * se_loa
            loa_upper_ci_high <- loa_upper + ci_factor_loa * se_loa

            # FIX 2: Check for proportional bias (Bland-Altman regression)
            ba_regression <- lm(differences ~ means)
            prop_bias_slope <- coef(ba_regression)[2]
            prop_bias_p <- summary(ba_regression)$coefficients[2, 4]

            if (prop_bias_p < 0.05) {
                private$.addWarning("WARNING",
                    sprintf('Proportional bias detected (slope=%.3f, p=%.3f). Disagreement increases at higher values. Consider non-linear transformation or stratified analysis.', prop_bias_slope, prop_bias_p))
            }

            # Bias significance check
            bias_present <- (mean_diff_lower > 0) || (mean_diff_upper < 0)
            bias_msg <- if(bias_present) "Systematic bias present (CI excludes 0)" else "No significant bias"

            # Populate agreement table
            table <- self$results$agreementtable

            table$addRow(rowKey = 1, values = list(
                metric = icc_label,
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
                metric = "Bland-Altman Mean Difference (Bias)",
                value = mean_diff,
                ci_lower = mean_diff_lower,
                ci_upper = mean_diff_upper,
                interpretation = bias_msg
            ))

            table$addRow(rowKey = 4, values = list(
                metric = "Limits of Agreement (Lower)",
                value = loa_lower,
                ci_lower = loa_lower_ci_low,
                ci_upper = loa_lower_ci_high,
                interpretation = "95% of differences fall within limits"
            ))

            table$addRow(rowKey = 5, values = list(
                metric = "Limits of Agreement (Upper)",
                value = loa_upper,
                ci_lower = loa_upper_ci_low,
                ci_upper = loa_upper_ci_high,
                interpretation = "95% of differences fall within limits"
            ))
        },
        
        .performCorrelationAnalysis = function(method1, method2) {
            table <- self$results$correlationtable
            method_option <- self$options$correlation_method
            row_key <- 1
            
            # Spearman correlation (if requested)
            if (method_option %in% c("both", "spearman")) {
                spearman_test <- cor.test(method1, method2, method = "spearman")
                spearman_r <- spearman_test$estimate
                spearman_p <- spearman_test$p.value
                
                # Bootstrap CI for Spearman
                spearman_boot_ci <- private$.performBootstrapCI(method1, method2, "spearman")
                
                spearman_strength <- ifelse(abs(spearman_r) >= private$.CLINICAL_CONSTANTS$CORRELATION_VERY_STRONG, "Very strong",
                                    ifelse(abs(spearman_r) >= private$.CLINICAL_CONSTANTS$CORRELATION_STRONG, "Strong",
                                    ifelse(abs(spearman_r) >= private$.CLINICAL_CONSTANTS$CORRELATION_MODERATE, "Moderate",
                                    ifelse(abs(spearman_r) >= private$.CLINICAL_CONSTANTS$CORRELATION_WEAK, "Weak", "Very weak"))))
                
                table$addRow(rowKey = row_key, values = list(
                    method = "Spearman rank correlation",
                    coefficient = spearman_r,
                    p_value = spearman_p,
                    ci_lower = spearman_boot_ci$lower,
                    ci_upper = spearman_boot_ci$upper,
                    interpretation = spearman_strength
                ))
                row_key <- row_key + 1
            }
            
            # Pearson correlation (if requested)
            if (method_option %in% c("both", "pearson")) {
                # WARNING: Normality check (Shapiro-Wilk)
                if (length(method1) >= 3 && length(method1) <= 5000) {
                     sw1 <- shapiro.test(method1)
                     sw2 <- shapiro.test(method2)
                     if (sw1$p.value < 0.05 || sw2$p.value < 0.05) {
                        private$.addWarning("WARNING",
                            'Normality assumption violated (Shapiro-Wilk p<0.05). Spearman rank correlation recommended over Pearson for non-normal data.')
                     }
                }

                pearson_test <- cor.test(method1, method2, method = "pearson", conf.level = self$options$conf_level)
                pearson_r <- pearson_test$estimate
                pearson_p <- pearson_test$p.value
                pearson_ci_lower <- pearson_test$conf.int[1]
                pearson_ci_upper <- pearson_test$conf.int[2]
                
                pearson_strength <- ifelse(abs(pearson_r) >= private$.CLINICAL_CONSTANTS$CORRELATION_VERY_STRONG, "Very strong",
                                   ifelse(abs(pearson_r) >= private$.CLINICAL_CONSTANTS$CORRELATION_STRONG, "Strong",
                                   ifelse(abs(pearson_r) >= private$.CLINICAL_CONSTANTS$CORRELATION_MODERATE, "Moderate",
                                   ifelse(abs(pearson_r) >= private$.CLINICAL_CONSTANTS$CORRELATION_WEAK, "Weak", "Very weak"))))
                
                table$addRow(rowKey = row_key, values = list(
                    method = "Pearson correlation",
                    coefficient = pearson_r,
                    p_value = pearson_p,
                    ci_lower = pearson_ci_lower,
                    ci_upper = pearson_ci_upper,
                    interpretation = pearson_strength
                ))
            }
        },
        
        .performBootstrapCI = function(method1, method2, method_type = "spearman") {
            n_bootstrap <- self$options$bootstrap_n
            conf_level <- self$options$conf_level
            
            bootstrap_results <- replicate(n_bootstrap, {
                indices <- sample(length(method1), replace = TRUE)
                boot_method1 <- method1[indices]
                boot_method2 <- method2[indices]
                cor(boot_method1, boot_method2, method = method_type, use = "complete.obs")
            })
            
            alpha <- 1 - conf_level
            list(
                lower = quantile(bootstrap_results, alpha / 2, na.rm = TRUE),
                upper = quantile(bootstrap_results, 1 - alpha / 2, na.rm = TRUE)
            )
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
                ggplot2::geom_hline(yintercept = unique(data$MeanDiff), color = "black", linewidth = 1) +
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
        
        .generateInterpretation = function(method1, method2, validation_warnings = "") {
            n <- length(method1)
            spearman_r <- cor(method1, method2, method = "spearman")
            
            # Calculate agreement metrics for interpretation
            if (requireNamespace('psych', quietly = TRUE)) {
                data_for_icc <- data.frame(Method1 = method1, Method2 = method2)
                icc_result <- psych::ICC(data_for_icc)

                # Use same ICC type as selected by user
                if (self$options$icc_type == "consistency") {
                    icc_index <- 6  # ICC(3,1) - Consistency
                } else { # absolute
                    icc_index <- 4  # ICC(2,1) - Absolute Agreement
                }

                icc_value <- icc_result$results$ICC[icc_index]
            } else {
                icc_value <- NA
            }
            
            differences <- method1 - method2
            mean_diff <- mean(differences)
            sd_diff <- sd(differences)
            
            interpretation <- paste0(
                validation_warnings,
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
            
            if (self$options$show_interpretation) {
                self$results$interpretation$setContent(interpretation)
            }
        },

        # Multi-method analysis functions
        .populateCorrelationMatrixTable = function() {
            table <- self$results$correlationmatrix
            table$addColumn(name = 'method', title = 'Method', type = 'text')
            table$addColumn(name = 'method1_corr', title = 'Method 1', type = 'number', format = 'zto')
            table$addColumn(name = 'method2_corr', title = 'Method 2', type = 'number', format = 'zto')
            table$addColumn(name = 'additional_corrs', title = 'Additional Methods', type = 'text')
        },

        .populateOverallICCTable = function() {
            table <- self$results$overall_icc_table
            table$addColumn(name = 'icc_type', title = 'ICC Type', type = 'text')
            table$addColumn(name = 'icc_value', title = 'ICC Value', type = 'number', format = 'zto')
            table$addColumn(name = 'ci_lower', title = '95% CI Lower', type = 'number', format = 'zto')
            table$addColumn(name = 'ci_upper', title = '95% CI Upper', type = 'number', format = 'zto')
            table$addColumn(name = 'interpretation', title = 'Interpretation', type = 'text')
        },

        .extractAllMethods = function(data) {
            # Extract all methods data (dep1, dep2, and additional_methods)
            # Use jmvcore::toNumeric() for safe conversion
            method1 <- jmvcore::toNumeric(data[[self$options$dep1]])
            method2 <- jmvcore::toNumeric(data[[self$options$dep2]])

            # Combine with additional methods
            all_data <- data.frame(Method1 = method1, Method2 = method2)

            if (!is.null(self$options$additional_methods) && length(self$options$additional_methods) > 0) {
                for (i in seq_along(self$options$additional_methods)) {
                    method_name <- paste0("Method", i + 2)
                    all_data[[method_name]] <- jmvcore::toNumeric(data[[self$options$additional_methods[i]]])
                }
            }

            # Remove rows with missing data
            complete_cases <- complete.cases(all_data)
            all_data[complete_cases, ]
        },

        .performMultiMethodAnalysis = function(all_methods) {
            n_methods <- ncol(all_methods)

            # 1. Generate correlation matrix
            private$.generateCorrelationMatrix(all_methods)

            # 2. Calculate overall ICC for all methods
            private$.calculateOverallICC(all_methods)

            # 3. Generate multi-method summary
            private$.generateMultiMethodSummary(all_methods)
        },

        .generateCorrelationMatrix = function(all_methods) {
            # Calculate correlation matrix (use spearman as primary method for multi-method)
            method_choice <- if (self$options$correlation_method == "pearson") "pearson" else "spearman"
            cor_matrix <- cor(all_methods, method = method_choice, use = "complete.obs")
            n_methods <- ncol(all_methods)

            table <- self$results$correlationmatrix

            # Populate correlation matrix table
            for (i in 1:n_methods) {
                method_name <- colnames(all_methods)[i]

                # Format correlations for display
                # Note: Currently table columns are fixed: method, method1, method2, additional
                # Ideally we would dynamic columns but Jamovi tables are static defined in yaml.
                # So we map:
                # Method 1 column -> Correlation with Method 1
                # Method 2 column -> Correlation with Method 2
                # Additional -> String of others
                
                method1_corr <- if (i == 1) 1.000 else cor_matrix[i, 1]
                method2_corr <- if (i == 2) 1.000 else cor_matrix[i, 2]

                # Additional correlations as text
                additional_corrs <- ""
                if (n_methods > 2) {
                    additional_vals <- sapply(3:n_methods, function(j) {
                        if (i == j) "1.000" else sprintf("%.3f", cor_matrix[i, j])
                    })
                    additional_corrs <- paste(additional_vals, collapse = ", ")
                }

                table$addRow(rowKey = i, values = list(
                    method = method_name,
                    method1_corr = method1_corr,
                    method2_corr = method2_corr,
                    additional_corrs = additional_corrs
                ))
            }
        },

        .calculateOverallICC = function(all_methods) {
            if (!requireNamespace('psych', quietly = TRUE)) {
                return()
            }

            # Calculate ICC for all methods
            icc_result <- psych::ICC(all_methods)

            table <- self$results$overall_icc_table

            # Add relevant ICC types for multi-method analysis
            icc_types <- list(
                list(index = 1, name = "ICC(1,1) - Single rater, random effects"),
                list(index = 4, name = "ICC(2,1) - Single rater, mixed effects"),
                list(index = 6, name = "ICC(3,1) - Single rater, fixed effects")
            )

            row_key <- 1
            for (icc_type in icc_types) {
                icc_value <- icc_result$results$ICC[icc_type$index]
                icc_lower <- icc_result$results$`lower bound`[icc_type$index]
                icc_upper <- icc_result$results$`upper bound`[icc_type$index]

                interpretation <- ifelse(icc_value >= 0.90, "Excellent reliability",
                                 ifelse(icc_value >= 0.75, "Good reliability",
                                 ifelse(icc_value >= 0.50, "Moderate reliability", "Poor reliability")))

                table$addRow(rowKey = row_key, values = list(
                    icc_type = icc_type$name,
                    icc_value = icc_value,
                    ci_lower = icc_lower,
                    ci_upper = icc_upper,
                    interpretation = interpretation
                ))
                row_key <- row_key + 1
            }
        },

        .generateMultiMethodSummary = function(all_methods) {
            n_methods <- ncol(all_methods)
            n_cases <- nrow(all_methods)

            # Calculate mean correlation
            cor_matrix <- cor(all_methods, use = "complete.obs")
            upper_tri <- cor_matrix[upper.tri(cor_matrix)]
            mean_correlation <- mean(upper_tri, na.rm = TRUE)

            summary_html <- paste0(
                "<h3>Multi-Method Analysis Summary</h3>",
                "<p><strong>Number of Methods:</strong> ", n_methods, "</p>",
                "<p><strong>Complete Cases:</strong> ", n_cases, "</p>",
                "<p><strong>Mean Inter-Method Correlation:</strong> ", sprintf("%.3f", mean_correlation), "</p>",

                "<h4>Analysis Overview:</h4>",
                "<ul>",
                "<li><strong>Standard Pairwise Analysis:</strong> Method 1 vs Method 2 (above)</li>",
                "<li><strong>Correlation Matrix:</strong> All pairwise correlations between methods</li>",
                "<li><strong>Overall ICC:</strong> Reliability across all methods simultaneously</li>",
                "</ul>",

                "<h4>Multi-Method Interpretation:</h4>",
                "<p>",
                if (mean_correlation >= 0.90) {
                    "All methods show <strong>excellent agreement</strong> with each other. Methods can be considered interchangeable."
                } else if (mean_correlation >= 0.75) {
                    "Methods show <strong>good overall agreement</strong>. Minor differences between methods may be acceptable."
                } else {
                    "Methods show <strong>moderate agreement</strong>. Investigate sources of disagreement between specific method pairs."
                },
                "</p>",
                
                "<div class='alert alert-info'>",
                "<strong>Note on Multiple Comparisons:</strong> When comparing >2 methods, the risk of finding random significant correlations increases. ",
                "Focus on the magnitude of correlation (effect size) rather than just p-values.",
                "</div>",

                "<p><em>Note: The standard 2-method analysis (above) remains the primary comparison. Multi-method results provide additional insights for overall method reliability assessment.</em></p>"
            )

            self$results$multimethod_summary$setContent(summary_html)
        },


        .generatePlainLanguageSummary = function(method1, method2) {
            # FIX 3: Generate plain-language summary suitable for copy-pasting into reports
            n <- length(method1)
            spearman_r <- cor(method1, method2, method = "spearman")

            # Get ICC if available
            icc_value <- NA
            if (requireNamespace('psych', quietly = TRUE)) {
                data_for_icc <- data.frame(Method1 = method1, Method2 = method2)
                icc_result <- psych::ICC(data_for_icc)
                icc_index <- if (self$options$icc_type == "consistency") 6 else 4
                icc_value <- icc_result$results$ICC[icc_index]
            }

            # Bland-Altman
            differences <- method1 - method2
            mean_diff <- mean(differences)

            # Generate plain-language summary paragraph
            agreement_level <- if (!is.na(icc_value) && icc_value >= 0.90 && abs(spearman_r) >= 0.94) {
                "excellent agreement"
            } else if (!is.na(icc_value) && icc_value >= 0.75 && abs(spearman_r) >= 0.85) {
                "good agreement"
            } else if (!is.na(icc_value) && icc_value >= 0.50) {
                "moderate agreement"
            } else {
                "limited agreement"
            }

            bias_description <- if (abs(mean_diff) < 0.1 * mean(c(method1, method2))) {
                "minimal systematic bias"
            } else {
                sprintf("systematic bias of %.2f units", mean_diff)
            }

            summary_text <- sprintf(
                "Agreement analysis between %s and %s was performed using %d paired observations. The methods showed %s (ICC = %.2f, Spearman r = %.2f) with %s. These results suggest that the methods are %s for clinical use in this context.",
                self$options$dep1, self$options$dep2, n,
                agreement_level,
                if (!is.na(icc_value)) icc_value else 0,
                spearman_r,
                bias_description,
                if (agreement_level == "excellent agreement") "highly suitable for interchangeable"
                else if (agreement_level == "good agreement") "generally suitable for interchangeable"
                else if (agreement_level == "moderate agreement") "potentially suitable with caution for"
                else "not recommended for interchangeable"
            )

            summary_html <- paste0(
                "<div style='background-color: #e8f5e9; padding: 20px; border-radius: 8px; border-left: 5px solid #4caf50;'>",
                "<h4>📝 Plain Language Summary</h4>",
                "<p style='font-size: 14px; line-height: 1.6;'>", summary_text, "</p>",
                "<p style='font-size: 12px; color: #666; margin-top: 15px;'><em>This summary is suitable for copying into pathology reports, research manuscripts, or presentations. It provides key findings in accessible language without technical jargon.</em></p>",
                "</div>"
            )

            self$results$summary$setContent(summary_html)
        },

        .generateEducationalExplanations = function() {
            # FIX 4: Generate educational content explaining the analysis
            explanations_html <- paste0(
                "<div style='background-color: #f0f4ff; padding: 20px; border-radius: 8px; border-left: 5px solid #2196f3;'>",
                "<h3>📚 About This Analysis: Agreement Analysis for Digital Pathology</h3>",

                "<h4>What This Analysis Does:</h4>",
                "<p>Agreement analysis evaluates whether two (or more) measurement methods produce consistent results on the same samples. ",
                "Unlike correlation, which only measures linear association, agreement analysis determines if methods can be used <strong>interchangeably</strong> in clinical practice.</p>",

                "<h4>When to Use This Analysis:</h4>",
                "<ul>",
                "<li><strong>Platform Comparison:</strong> Comparing digital pathology platforms (HALO vs Aiforia vs ImageJ) for biomarker quantification</li>",
                "<li><strong>Algorithm Validation:</strong> Validating AI algorithms against pathologist assessments</li>",
                "<li><strong>Multi-site Studies:</strong> Assessing reproducibility across institutions or laboratories</li>",
                "<li><strong>Method Substitution:</strong> Determining if a new method can replace an established reference method</li>",
                "<li><strong>Quality Control:</strong> Evaluating inter-observer or intra-observer reliability in diagnostic scoring</li>",
                "</ul>",

                "<h4>Key Assumptions:</h4>",
                "<ol>",
                "<li><strong>Paired Measurements:</strong> Each sample must be measured by both methods</li>",
                "<li><strong>Representative Sample:</strong> Data should represent the full range of values expected in practice</li>",
                "<li><strong>Independent Errors:</strong> Measurement errors should be independent (e.g., avoid pseudo-replication)</li>",
                "<li><strong>Appropriate Sample Size:</strong> Minimum n=30 recommended for reliable agreement estimates in pathology validation studies</li>",
                "</ol>",

                "<h4>How to Interpret Results:</h4>",

                "<p><strong>🔵 ICC (Intraclass Correlation Coefficient):</strong></p>",
                "<ul>",
                "<li><strong>Excellent (≥0.90):</strong> Methods can be used interchangeably without concern</li>",
                "<li><strong>Good (0.75-0.89):</strong> Methods are suitable for most clinical applications</li>",
                "<li><strong>Moderate (0.50-0.74):</strong> Use with caution; investigate sources of disagreement</li>",
                "<li><strong>Poor (<0.50):</strong> Methods should not be used interchangeably</li>",
                "</ul>",

                "<p><strong>🟢 Bland-Altman Plot:</strong></p>",
                "<ul>",
                "<li><strong>Mean Difference (Bias):</strong> Average difference between methods. Zero indicates no systematic bias.</li>",
                "<li><strong>Limits of Agreement (LoA):</strong> 95% of differences fall within these limits. Narrow limits indicate good agreement.</li>",
                "<li><strong>Proportional Bias:</strong> If differences increase with magnitude (sloped pattern), disagreement varies across measurement range</li>",
                "</ul>",

                "<p><strong>🟣 Correlation vs Agreement:</strong></p>",
                "<ul>",
                "<li><strong>Correlation (Spearman/Pearson):</strong> Measures strength of linear relationship. Can be high even if methods systematically differ by a constant.</li>",
                "<li><strong>Agreement (ICC/CCC):</strong> Measures actual concordance. Requires methods to give similar <em>absolute</em> values, not just similar ranking.</li>",
                "<li><strong>Clinical Relevance:</strong> For method substitution, agreement matters more than correlation.</li>",
                "</ul>",

                "<h4>Common Pitfalls to Avoid:</h4>",
                "<ul>",
                "<li>❌ Using correlation alone to assess agreement (correlation ≠ agreement)</li>",
                "<li>❌ Ignoring Bland-Altman plots (visual inspection reveals patterns missed by statistics)</li>",
                "<li>❌ Relying on small samples (n<30) for validation studies</li>",
                "<li>❌ Comparing methods across different ranges (e.g., one method only in high values)</li>",
                "<li>❌ Assuming statistical significance equals clinical significance (always consider clinical context)</li>",
                "</ul>",

                "<h4>Recommended Reading:</h4>",
                "<ul style='font-size: 13px;'>",
                "<li>Bland JM, Altman DG (1999). <em>Measuring agreement in method comparison studies.</em> Stat Methods Med Res 8(2):135-60.</li>",
                "<li>Koo TK, Li MY (2016). <em>A guideline of selecting and reporting intraclass correlation coefficients for reliability research.</em> J Chiropr Med 15(2):155-63.</li>",
                "<li>Landis JR, Koch GG (1977). <em>The measurement of observer agreement for categorical data.</em> Biometrics 33(1):159-74.</li>",
                "<li>Cicchetti DV (1994). <em>Guidelines, criteria, and rules of thumb for evaluating normed and standardized assessment instruments in psychology.</em> Psychol Assess 6(4):284-90.</li>",
                "</ul>",

                "<p style='margin-top: 20px; padding: 15px; background-color: white; border-radius: 5px;'>",
                "<strong>💡 Pro Tip:</strong> When reporting agreement analysis, always include: (1) ICC with 95% CI, (2) Bland-Altman mean difference and LoA, ",
                "(3) visual Bland-Altman plot, and (4) clinical interpretation of agreement magnitude in context of your application.",
                "</p>",

                "</div>"
            )

            self$results$explanations$setContent(explanations_html)
        },

        .generateReportSentences = function(method1, method2) {
            # Calculate key metrics for report
            spearman_test <- cor.test(method1, method2, method = "spearman")
            spearman_r <- spearman_test$estimate
            spearman_p <- spearman_test$p.value

            # Get ICC if available
            icc_value <- NA
            icc_ci_text <- ""
            if (requireNamespace('psych', quietly = TRUE)) {
                data_for_icc <- data.frame(Method1 = method1, Method2 = method2)
                icc_result <- psych::ICC(data_for_icc)
                icc_index <- if (self$options$icc_type == "consistency") 6 else 4  # absolute
                icc_value <- icc_result$results$ICC[icc_index]
                icc_lower <- icc_result$results$`lower bound`[icc_index]
                icc_upper <- icc_result$results$`upper bound`[icc_index]
                icc_ci_text <- sprintf("95%% CI %.3f-%.3f", icc_lower, icc_upper)
            }

            # Bland-Altman metrics
            differences <- method1 - method2
            mean_diff <- mean(differences)

            # Clinical interpretation based on preset
            preset_context <- switch(self$options$clinical_preset,
                "biomarker_platforms" = "biomarker platform comparison",
                "ai_pathologist" = "AI algorithm versus pathologist assessment",
                "multisite_validation" = "multi-institutional validation",
                "general" = "measurement method comparison"
            )

            # Generate report sentences
            methods_sentence <- sprintf(
                "Agreement analysis between %s and %s was performed using %s.",
                self$options$dep1, self$options$dep2, preset_context
            )

            correlation_sentence <- sprintf(
                "Spearman rank correlation showed %s agreement (ρ = %.3f, p = %.3f).",
                if (abs(spearman_r) >= private$.CLINICAL_CONSTANTS$CORRELATION_STRONG) "strong"
                else if (abs(spearman_r) >= private$.CLINICAL_CONSTANTS$CORRELATION_MODERATE) "moderate"
                else "weak",
                spearman_r, spearman_p
            )

            icc_sentence <- if (!is.na(icc_value)) {
                sprintf(
                    "Intraclass correlation coefficient demonstrated %s reliability (ICC = %.3f, %s).",
                    if (icc_value >= private$.CLINICAL_CONSTANTS$ICC_EXCELLENT) "excellent"
                    else if (icc_value >= private$.CLINICAL_CONSTANTS$ICC_GOOD) "good"
                    else "moderate",
                    icc_value, icc_ci_text
                )
            } else {
                "Intraclass correlation coefficient could not be calculated."
            }

            bias_sentence <- sprintf(
                "Bland-Altman analysis revealed %s systematic bias (mean difference = %.3f).",
                if (abs(mean_diff) < 0.1 * mean(c(method1, method2))) "minimal" else "notable",
                mean_diff
            )

            # Clinical conclusion
            conclusion_sentence <- sprintf(
                "The methods demonstrated %s for %s applications.",
                if (!is.na(icc_value) && icc_value >= private$.CLINICAL_CONSTANTS$ICC_GOOD &&
                   abs(spearman_r) >= private$.CLINICAL_CONSTANTS$CORRELATION_STRONG)
                    "acceptable agreement suitable for interchangeable use"
                else if (!is.na(icc_value) && icc_value >= private$.CLINICAL_CONSTANTS$ICC_MODERATE)
                    "moderate agreement requiring caution for interchangeable use"
                else "limited agreement not recommended for interchangeable use",
                preset_context
            )

            report_html <- paste0(
                "<div style='background-color: #f8f9fa; padding: 15px; border-radius: 5px; margin: 10px 0;'>",
                "<h4>📋 Copy-Ready Report Sentences</h4>",
                "<div style='font-family: monospace; background-color: white; padding: 12px; border: 1px solid #ddd; border-radius: 3px; margin: 8px 0;'>",
                "<strong>Methods:</strong><br>", methods_sentence,
                "</div>",
                "<div style='font-family: monospace; background-color: white; padding: 12px; border: 1px solid #ddd; border-radius: 3px; margin: 8px 0;'>",
                "<strong>Results:</strong><br>", correlation_sentence, " ", icc_sentence, " ", bias_sentence,
                "</div>",
                "<div style='font-family: monospace; background-color: white; padding: 12px; border: 1px solid #ddd; border-radius: 3px; margin: 8px 0;'>",
                "<strong>Conclusion:</strong><br>", conclusion_sentence,
                "</div>",
                "<p><em>Tip: Select text above and copy (Ctrl+C/Cmd+C) for use in manuscripts or reports.</em></p>",
                "</div>"
            )

            self$results$report_sentences$setContent(report_html)
        },

        .generateStatisticalGlossary = function() {
            glossary_html <- paste0(
                "<div style='background-color: #f0f8ff; padding: 15px; border-radius: 5px;'>",
                "<h4>📚 Statistical Terms Glossary</h4>",
                "<div style='margin: 10px 0;'>",

                "<p><strong>ICC (Intraclass Correlation Coefficient):</strong> Measures reliability and agreement between measurement methods. ",
                "Values: 0.90+ excellent, 0.75+ good, 0.50+ moderate reliability.</p>",

                "<p><strong>Spearman Correlation (ρ):</strong> Measures strength of monotonic relationship between methods, robust to outliers. ",
                "Clinical interpretation: 0.90+ very strong, 0.70+ strong, 0.50+ moderate association.</p>",

                "<p><strong>Concordance Correlation Coefficient (CCC):</strong> Measures agreement combining precision and accuracy. ",
                "Values: 0.99+ almost perfect, 0.95+ substantial, 0.90+ moderate agreement.</p>",

                "<p><strong>Bland-Altman Plot:</strong> Visualizes agreement by plotting differences vs. means. ",
                "Shows systematic bias (mean difference) and limits of agreement (±1.96 SD).</p>",

                "<p><strong>Bootstrap Confidence Intervals:</strong> Robust estimation method using resampling. ",
                "1000+ samples recommended for reliable intervals in clinical studies.</p>",

                "<p><strong>Clinical Presets:</strong></p>",
                "<ul>",
                "<li><strong>Biomarker Platforms:</strong> Optimized for Ki67, ER, PR, HER2 scoring across digital systems</li>",
                "<li><strong>AI vs Pathologist:</strong> Specialized for algorithm validation studies</li>",
                "<li><strong>Multi-site Validation:</strong> Enhanced for multi-institutional reproducibility studies</li>",
                "</ul>",

                "<p><em>Guidelines based on: Landis & Koch (1977), Cicchetti (1994), Koo & Li (2016), FDA Biomarker Qualification Guidance</em></p>",
                "</div>",
                "</div>"
            )

            self$results$statistical_glossary$setContent(glossary_html)
        },

        .generateICCSelectionGuide = function() {
            # Dynamic guidance based on current selection
            current_choice <- self$options$icc_type
            current_preset <- self$options$clinical_preset

            # Preset-specific recommendations
            preset_recommendation <- switch(current_preset,
                "biomarker_platforms" = "For biomarker platform comparison studies (Ki67, ER, PR, HER2), <strong>Consistency</strong> is typically recommended as relative ranking is more clinically relevant than exact numerical agreement.",
                "ai_pathologist" = "For AI algorithm vs pathologist studies, choose based on application: <strong>Consistency</strong> for diagnostic ranking, <strong>Absolute Agreement</strong> for exact score matching.",
                "multisite_validation" = "For multi-institutional validation, <strong>Consistency</strong> is usually preferred as it focuses on reproducible ranking across institutions despite potential systematic differences.",
                "For general agreement studies, choose based on your research question: ranking reproducibility vs exact value concordance."
            )

            guide_html <- paste0(
                "<div style='background-color: #e8f5e8; padding: 15px; border-radius: 5px; border-left: 4px solid #28a745;'>",
                "<h4>🎯 ICC Selection Guide</h4>",

                "<div style='margin: 15px 0;'>",
                "<h5>Current Selection: <span style='color: #007bff; font-weight: bold;'>",
                if (current_choice == "consistency") "Consistency (ICC 3,1)" else "Absolute Agreement (ICC 2,1)", "</span></h5>",
                "</div>",

                "<div style='background-color: white; padding: 12px; border-radius: 3px; margin: 10px 0;'>",
                "<h5>📊 When to Use Each Type:</h5>",

                "<p><strong>🟢 Consistency (ICC 3,1) - Recommended for most studies:</strong></p>",
                "<ul>",
                "<li><strong>Biomarker Scoring:</strong> Ki67 proliferation index, ER/PR scoring, HER2 assessment</li>",
                "<li><strong>Platform Comparison:</strong> HALO vs Aiforia vs ImageJ measurements</li>",
                "<li><strong>Diagnostic Ranking:</strong> When relative ordering matters more than exact values</li>",
                "<li><strong>Research Question:</strong> 'Do methods rank samples similarly?'</li>",
                "</ul>",

                "<p><strong>🟡 Absolute Agreement (ICC 2,1) - Use for exact concordance:</strong></p>",
                "<ul>",
                "<li><strong>Reference Range Validation:</strong> Establishing interchangeable cutoffs</li>",
                "<li><strong>Method Substitution:</strong> When exact numerical agreement is critical</li>",
                "<li><strong>Calibration Studies:</strong> Standardizing measurements across instruments</li>",
                "<li><strong>Research Question:</strong> 'Do methods give identical values?'</li>",
                "</ul>",
                "</div>",

                "<div style='background-color: #f8f9fa; padding: 12px; border-radius: 3px; margin: 10px 0;'>",
                "<h5>🏥 Clinical Preset Guidance:</h5>",
                "<p>", preset_recommendation, "</p>",
                "</div>",

                "<div style='background-color: #fff3cd; padding: 12px; border-radius: 3px; margin: 10px 0;'>",
                "<h5>⚠️ Key Differences:</h5>",
                "<ul>",
                "<li><strong>Consistency:</strong> Focuses on relative agreement - methods may differ by a constant but rank similarly</li>",
                "<li><strong>Absolute Agreement:</strong> Requires identical values - stricter criterion for interchangeability</li>",
                "<li><strong>Clinical Impact:</strong> Consistency typically gives higher ICC values than Absolute Agreement</li>",
                "</ul>",
                "</div>",

                "<p><em>💡 Tip: Most digital pathology studies use Consistency because systematic platform differences are common but don't affect clinical interpretation if ranking is preserved.</em></p>",
                "</div>"
            )

            self$results$icc_selection_guide$setContent(guide_html)
        }
    ), # End of private list
    public = list(
        #' @description
        #' Generate R source code for pathologyagreement analysis
        #' @return Character string with R syntax for reproducible analysis
        asSource = function() {
            dep1 <- self$options$dep1
            dep2 <- self$options$dep2

            if (is.null(dep1) || is.null(dep2))
                return('')

            # Escape dep1 variable
            dep1_escaped <- if (!is.null(dep1) && !identical(make.names(dep1), dep1)) {
                paste0('`', dep1, '`')
            } else {
                dep1
            }

            # Escape dep2 variable
            dep2_escaped <- if (!is.null(dep2) && !identical(make.names(dep2), dep2)) {
                paste0('`', dep2, '`')
            } else {
                dep2
            }

            # Build arguments
            dep1_arg <- paste0('dep1 = "', dep1_escaped, '"')
            dep2_arg <- paste0('dep2 = "', dep2_escaped, '"')

            # Get other arguments using base helper (if available)
            args <- ''
            if (!is.null(private$.asArgs)) {
                args <- private$.asArgs(incData = FALSE)
            }
            if (args != '')
                args <- paste0(',\n    ', args)

            # Get package name dynamically
            pkg_name <- utils::packageName()
            if (is.null(pkg_name)) pkg_name <- "ClinicoPath"  # fallback

            # Build complete function call
            paste0(pkg_name, '::pathologyagreement(\n    data = data,\n    ',
                   dep1_arg, ',\n    ', dep2_arg, args, ')')
        }
    ) # End of public list
)