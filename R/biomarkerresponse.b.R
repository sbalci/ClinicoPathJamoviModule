#' @title Biomarker Response Association
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import ggplot2
#' @importFrom dplyr group_by summarise mutate
#' @importFrom pROC roc auc ci.auc coords
#' @description Analyzes and visualizes relationships between biomarker levels and treatment responses

biomarkerresponseClass <- if(requireNamespace("jmvcore")) R6::R6Class(
    "biomarkerresponseClass",
    inherit = biomarkerresponseBase,
    private = list(
        
        # Determine optimal threshold using ROC analysis
        .calculateOptimalThreshold = function(biomarker_values, response_binary) {
            tryCatch({
                roc_obj <- pROC::roc(response_binary, biomarker_values)
                coords_obj <- pROC::coords(roc_obj, "best", ret = c("threshold", "sensitivity", "specificity"))
                
                return(list(
                    threshold = coords_obj$threshold,
                    sensitivity = coords_obj$sensitivity,
                    specificity = coords_obj$specificity,
                    auc = as.numeric(pROC::auc(roc_obj))
                ))
            }, error = function(e) {
                return(list(threshold = median(biomarker_values, na.rm = TRUE), 
                          sensitivity = NA, specificity = NA, auc = NA))
            })
        },
        
        # Calculate threshold performance metrics
        .calculateThresholdMetrics = function(biomarker_values, response_binary, threshold) {
            biomarker_positive <- biomarker_values >= threshold
            
            tp <- sum(biomarker_positive & response_binary, na.rm = TRUE)
            tn <- sum(!biomarker_positive & !response_binary, na.rm = TRUE)
            fp <- sum(biomarker_positive & !response_binary, na.rm = TRUE)
            fn <- sum(!biomarker_positive & response_binary, na.rm = TRUE)
            
            sensitivity <- tp / (tp + fn)
            specificity <- tn / (tn + fp)
            ppv <- tp / (tp + fp)
            npv <- tn / (tn + fn)
            
            # Calculate AUC
            tryCatch({
                roc_obj <- pROC::roc(response_binary, biomarker_values)
                auc_val <- as.numeric(pROC::auc(roc_obj))
            }, error = function(e) {
                auc_val <- NA
            })
            
            return(list(
                threshold = threshold,
                sensitivity = sensitivity,
                specificity = specificity,
                ppv = ppv,
                npv = npv,
                auc = auc_val
            ))
        },
        
        # Perform statistical tests based on response type
        .performStatisticalTests = function(biomarker_values, response_values, response_type, conf_level) {
            tests <- list()
            
            if (response_type == "binary" || response_type == "categorical") {
                # Convert to factor if needed
                response_factor <- as.factor(response_values)
                
                if (length(levels(response_factor)) == 2) {
                    # T-test for binary response
                    tryCatch({
                        t_test <- t.test(biomarker_values ~ response_factor, conf.level = conf_level)
                        tests[["t_test"]] <- list(
                            test = "Two-sample t-test",
                            statistic = t_test$statistic,
                            pvalue = t_test$p.value,
                            interpretation = ifelse(t_test$p.value < 0.05, 
                                                   "Significant difference between groups", 
                                                   "No significant difference")
                        )
                    }, error = function(e) {})
                    
                    # Wilcoxon test as non-parametric alternative with effect sizes
                    tryCatch({
                        wilcox_test <- wilcox.test(biomarker_values ~ response_factor, conf.level = conf_level, conf.int = TRUE)
                        
                        # Calculate effect sizes for biomarker analysis
                        group_levels <- levels(response_factor)
                        group1_data <- biomarker_values[response_factor == group_levels[1]]
                        group2_data <- biomarker_values[response_factor == group_levels[2]]
                        group1_data <- group1_data[!is.na(group1_data)]
                        group2_data <- group2_data[!is.na(group2_data)]
                        
                        # Calculate Cliff's Delta
                        cliff_delta <- private$.calculateCliffsDelta(group1_data, group2_data)
                        
                        # Calculate Hodges-Lehmann shift
                        hodges_lehmann <- private$.calculateHodgesLehmann(group1_data, group2_data)
                        
                        tests[["wilcox_test"]] <- list(
                            test = "Wilcoxon rank-sum test",
                            statistic = wilcox_test$statistic,
                            pvalue = wilcox_test$p.value,
                            cliff_delta = cliff_delta,
                            hodges_lehmann = hodges_lehmann,
                            interpretation = private$.interpretBiomarkerEffects(wilcox_test$p.value, cliff_delta, hodges_lehmann, group_levels)
                        )
                    }, error = function(e) {})
                    
                } else if (length(levels(response_factor)) > 2) {
                    # ANOVA for multiple groups
                    tryCatch({
                        anova_test <- aov(biomarker_values ~ response_factor)
                        anova_summary <- summary(anova_test)
                        tests[["anova"]] <- list(
                            test = "One-way ANOVA",
                            statistic = anova_summary[[1]][["F value"]][1],
                            pvalue = anova_summary[[1]][["Pr(>F)"]][1],
                            interpretation = ifelse(anova_summary[[1]][["Pr(>F)"]][1] < 0.05, 
                                                   "Significant difference between groups", 
                                                   "No significant difference")
                        )
                    }, error = function(e) {})
                    
                    # Kruskal-Wallis test
                    tryCatch({
                        kw_test <- kruskal.test(biomarker_values ~ response_factor)
                        tests[["kruskal"]] <- list(
                            test = "Kruskal-Wallis test",
                            statistic = kw_test$statistic,
                            pvalue = kw_test$p.value,
                            interpretation = ifelse(kw_test$p.value < 0.05, 
                                                   "Significant difference between groups", 
                                                   "No significant difference")
                        )
                    }, error = function(e) {})
                }
                
            } else if (response_type == "continuous") {
                # Correlation tests for continuous response
                tryCatch({
                    pearson_test <- cor.test(biomarker_values, response_values, method = "pearson", conf.level = conf_level)
                    tests[["pearson"]] <- list(
                        test = "Pearson correlation",
                        statistic = pearson_test$statistic,
                        pvalue = pearson_test$p.value,
                        interpretation = ifelse(pearson_test$p.value < 0.05, 
                                               paste("Significant correlation (r =", round(pearson_test$estimate, 3), ")"), 
                                               "No significant correlation")
                    )
                }, error = function(e) {})
                
                tryCatch({
                    spearman_test <- cor.test(biomarker_values, response_values, method = "spearman", conf.level = conf_level)
                    tests[["spearman"]] <- list(
                        test = "Spearman correlation",
                        statistic = spearman_test$statistic,
                        pvalue = spearman_test$p.value,
                        interpretation = ifelse(spearman_test$p.value < 0.05, 
                                               paste("Significant correlation (rho =", round(spearman_test$estimate, 3), ")"), 
                                               "No significant correlation")
                    )
                }, error = function(e) {})
            }
            
            return(tests)
        },
        
        .run = function() {
            # Enhanced guidance and documentation
            if (is.null(self$options$biomarker) || is.null(self$options$response)) {
                todo <- "
                <h3>🧬 ClinicoPath Biomarker Response Analysis</h3>
                <p><strong>Purpose:</strong> Analyze biomarker-response relationships for precision medicine and clinical decision support.</p>
                
                <h4>📋 Required Variables:</h4>
                <ul>
                    <li><strong>Biomarker Variable:</strong> Continuous measurement (expression, concentration, score)</li>
                    <li><strong>Response Variable:</strong> Treatment outcome (binary, categorical, or continuous)</li>
                </ul>
                
                <h4>⚙️ Optional Variables:</h4>
                <ul>
                    <li><strong>Grouping Variable:</strong> Stratification factor (treatment arm, disease stage)</li>
                </ul>
                
                <h4>🔬 Analysis Features:</h4>
                <ul>
                    <li><strong>ROC Analysis:</strong> Optimal threshold determination and performance metrics</li>
                    <li><strong>Statistical Testing:</strong> Appropriate tests for different response types</li>
                    <li><strong>Visualization:</strong> Box plots, scatter plots, violin plots with trend lines</li>
                    <li><strong>Clinical Validation:</strong> Sensitivity, specificity, PPV, NPV calculations</li>
                    <li><strong>Data Quality:</strong> Outlier handling and transformation options</li>
                </ul>
                
                <h4>🏥 Clinical Applications:</h4>
                <ul>
                    <li><strong>Predictive Biomarkers:</strong> Treatment selection (HER2, PD-L1, EGFR)</li>
                    <li><strong>Prognostic Biomarkers:</strong> Outcome prediction (Ki-67, p53)</li>
                    <li><strong>Pharmacogenomics:</strong> Drug metabolism (CYP2D6, TPMT)</li>
                    <li><strong>Companion Diagnostics:</strong> Regulatory-grade biomarker validation</li>
                    <li><strong>Treatment Monitoring:</strong> Response assessment (PSA, HbA1c)</li>
                </ul>
                
                <h4>📊 Interpretation Guide:</h4>
                <ul>
                    <li><strong>AUC > 0.8:</strong> Excellent biomarker performance</li>
                    <li><strong>AUC 0.7-0.8:</strong> Good biomarker performance</li>
                    <li><strong>AUC 0.6-0.7:</strong> Fair biomarker performance</li>
                    <li><strong>p < 0.05:</strong> Statistically significant association</li>
                </ul>
                
                <p><strong>Resources:</strong></p>
                <ul>
                    <li><a href='https://clinicopath.github.io/ClinicoPathJamoviModule/' target='_blank'>User Guide</a></li>
                    <li><a href='https://www.fda.gov/drugs/cder-biomarker-qualification-program' target='_blank'>FDA Biomarker Guidance</a></li>
                </ul>
                "
                self$results$todo$setContent(todo)
                return()
            }
            
            # Enhanced error checking and validation
            if (nrow(self$data) == 0) {
                stop("Error: Dataset contains no rows. Please provide data for biomarker analysis.")
            }
            
            # Enhanced data validation and preprocessing
            data <- self$data
            biomarker_var <- self$options$biomarker
            response_var <- self$options$response
            response_type <- self$options$responseType
            conf_level <- as.numeric(self$options$confidenceLevel)
            
            # Comprehensive data validation
            raw_biomarker_values <- data[[biomarker_var]]
            raw_response_values <- data[[response_var]]
            
            # Initial data validation
            if (is.null(raw_biomarker_values)) {
                stop("Error: Biomarker variable could not be found in the dataset.")
            }
            
            if (is.null(raw_response_values)) {
                stop("Error: Response variable could not be found in the dataset.")
            }
            
            if (all(is.na(raw_biomarker_values))) {
                stop("Error: Biomarker variable contains only missing values.")
            }
            
            if (all(is.na(raw_response_values))) {
                stop("Error: Response variable contains only missing values.")
            }
            
            # Convert biomarker to numeric if possible
            if (!is.numeric(raw_biomarker_values)) {
                tryCatch({
                    raw_biomarker_values <- as.numeric(raw_biomarker_values)
                }, error = function(e) {
                    stop("Error: Biomarker variable must be numeric or convertible to numeric.")
                })
            }
            
            # Validate response variable based on type
            if (response_type %in% c("binary", "categorical")) {
                raw_response_values <- as.factor(raw_response_values)
                if (response_type == "binary" && length(levels(raw_response_values)) != 2) {
                    warning(paste("Binary response specified but", length(levels(raw_response_values)), 
                                "levels found. Using first two levels for analysis."))
                }
            } else if (response_type == "continuous") {
                if (!is.numeric(raw_response_values)) {
                    tryCatch({
                        raw_response_values <- as.numeric(raw_response_values)
                    }, error = function(e) {
                        stop("Error: Continuous response variable must be numeric or convertible to numeric.")
                    })
                }
            }
            
            # Enhanced data preprocessing
            biomarker_values <- private$.preprocessBiomarkerData(raw_biomarker_values)
            response_values <- raw_response_values
            
            # Handle outliers if requested
            if (self$options$outlierHandling == "remove") {
                outlier_indices <- private$.detectOutliers(biomarker_values)
                if (length(outlier_indices) > 0) {
                    biomarker_values[outlier_indices] <- NA
                    response_values[outlier_indices] <- NA
                    
                    outlier_pct <- round(length(outlier_indices) / length(biomarker_values) * 100, 1)
                    warning(paste("Removed", length(outlier_indices), "outliers (", outlier_pct, "% of data)."))
                }
            }
            
            # Remove rows with missing values
            complete_cases <- !is.na(biomarker_values) & !is.na(response_values)
            biomarker_values <- biomarker_values[complete_cases]
            response_values <- response_values[complete_cases]
            
            # Final data validation
            if (length(biomarker_values) == 0) {
                stop("Error: No valid data points remain after preprocessing. Check for missing values and outliers.")
            }
            
            if (length(biomarker_values) < 10) {
                warning(paste("Small sample size (n =", length(biomarker_values), 
                            "). Results may be unreliable. Recommend n ≥ 30 for robust analysis."))
            }
            
            if (response_type %in% c("binary", "categorical")) {
                response_factor <- as.factor(response_values)
                min_group_size <- min(table(response_factor))
                if (min_group_size < 5) {
                    warning(paste("Small group size detected (n =", min_group_size, 
                                "). Results may be unreliable for statistical testing."))
                }
            }
            
            # Check biomarker data quality
            biomarker_range <- max(biomarker_values) - min(biomarker_values)
            if (biomarker_range == 0) {
                stop("Error: Biomarker values are constant. Cannot perform analysis.")
            }
            
            # Data quality assessment
            private$.assessDataQuality(biomarker_values, response_values, response_type)
            
            # Determine threshold
            threshold_value <- NULL
            if (self$options$showThreshold) {
                if (self$options$thresholdMethod == "manual" && !is.null(self$options$thresholdValue)) {
                    threshold_value <- self$options$thresholdValue
                } else if (self$options$thresholdMethod == "median") {
                    threshold_value <- median(biomarker_values, na.rm = TRUE)
                } else if (self$options$thresholdMethod == "q75") {
                    threshold_value <- quantile(biomarker_values, 0.75, na.rm = TRUE)
                } else if (self$options$thresholdMethod == "optimal" && response_type == "binary") {
                    # Convert response to binary if needed
                    response_binary <- as.numeric(as.factor(response_values)) - 1
                    optimal_result <- private$.calculateOptimalThreshold(biomarker_values, response_binary)
                    threshold_value <- optimal_result$threshold
                }
                
                # Calculate threshold metrics if binary response
                if (!is.null(threshold_value) && response_type == "binary") {
                    response_binary <- as.numeric(as.factor(response_values)) - 1
                    threshold_metrics <- private$.calculateThresholdMetrics(biomarker_values, response_binary, threshold_value)
                    
                    self$results$threshold$addRow(rowKey = 1, values = list(
                        threshold = threshold_metrics$threshold,
                        sensitivity = threshold_metrics$sensitivity,
                        specificity = threshold_metrics$specificity,
                        ppv = threshold_metrics$ppv,
                        npv = threshold_metrics$npv,
                        auc = threshold_metrics$auc
                    ))
                }
            }
            
            # Group comparison statistics
            if (response_type %in% c("binary", "categorical")) {
                response_factor <- as.factor(response_values)
                group_stats <- data.frame(
                    response_group = levels(response_factor),
                    n = as.numeric(table(response_factor)),
                    mean = tapply(biomarker_values, response_factor, mean, na.rm = TRUE),
                    sd = tapply(biomarker_values, response_factor, sd, na.rm = TRUE),
                    median = tapply(biomarker_values, response_factor, median, na.rm = TRUE),
                    q1 = tapply(biomarker_values, response_factor, quantile, 0.25, na.rm = TRUE),
                    q3 = tapply(biomarker_values, response_factor, quantile, 0.75, na.rm = TRUE)
                )
                
                for (i in 1:nrow(group_stats)) {
                    self$results$groupComparison$addRow(rowKey = i, values = list(
                        response_group = group_stats$response_group[i],
                        n = group_stats$n[i],
                        mean = group_stats$mean[i],
                        sd = group_stats$sd[i],
                        median = group_stats$median[i],
                        iqr = paste0(round(group_stats$q1[i], 2), " - ", round(group_stats$q3[i], 2))
                    ))
                }
            }
            
            # Correlation analysis for continuous response
            if (self$options$showCorrelation && response_type == "continuous") {
                tryCatch({
                    pearson_cor <- cor.test(biomarker_values, response_values, method = "pearson", conf.level = conf_level)
                    self$results$correlation$addRow(rowKey = 1, values = list(
                        method = "Pearson",
                        correlation = pearson_cor$estimate,
                        pvalue = pearson_cor$p.value,
                        ci_lower = pearson_cor$conf.int[1],
                        ci_upper = pearson_cor$conf.int[2]
                    ))
                }, error = function(e) {})
                
                tryCatch({
                    spearman_cor <- cor.test(biomarker_values, response_values, method = "spearman", conf.level = conf_level)
                    self$results$correlation$addRow(rowKey = 2, values = list(
                        method = "Spearman",
                        correlation = spearman_cor$estimate,
                        pvalue = spearman_cor$p.value,
                        ci_lower = NA,  # Spearman CI not always available
                        ci_upper = NA
                    ))
                }, error = function(e) {})
            }
            
            # Statistical tests
            if (self$options$performTests) {
                test_results <- private$.performStatisticalTests(biomarker_values, response_values, response_type, conf_level)
                
                for (test_name in names(test_results)) {
                    test <- test_results[[test_name]]
                    self$results$statisticalTests$addRow(rowKey = test_name, values = list(
                        test = test$test,
                        statistic = test$statistic,
                        pvalue = test$pvalue,
                        interpretation = test$interpretation
                    ))
                }
            }
            
            # Prepare plot data
            plot_data <- list(
                biomarker = biomarker_values,
                response = response_values,
                response_type = response_type,
                plot_type = self$options$plotType,
                threshold = threshold_value,
                show_threshold = self$options$showThreshold,
                add_trend_line = self$options$addTrendLine,
                trend_method = self$options$trendMethod,
                outlier_handling = self$options$outlierHandling,
                group_variable = if (!is.null(self$options$groupVariable)) data[[self$options$groupVariable]][complete_cases] else NULL,
                biomarker_name = biomarker_var,
                response_name = response_var
            )
            
            self$results$plot$setState(plot_data)
        },
        
        .preprocessBiomarkerData = function(raw_data) {
            # Enhanced biomarker data preprocessing
            
            # Apply log transformation if requested
            if (self$options$logTransform) {
                # Log(x+1) transformation to handle zeros
                processed_data <- log(raw_data + 1)
                
                # Check for infinite values after transformation
                if (any(is.infinite(processed_data), na.rm = TRUE)) {
                    warning("Log transformation resulted in infinite values. Consider data cleaning.")
                }
            } else {
                processed_data <- raw_data
            }
            
            return(processed_data)
        },
        
        .detectOutliers = function(data) {
            # IQR-based outlier detection
            Q1 <- quantile(data, 0.25, na.rm = TRUE)
            Q3 <- quantile(data, 0.75, na.rm = TRUE)
            IQR <- Q3 - Q1
            
            lower_bound <- Q1 - 1.5 * IQR
            upper_bound <- Q3 + 1.5 * IQR
            
            outlier_indices <- which(data < lower_bound | data > upper_bound)
            return(outlier_indices)
        },
        
        .assessDataQuality = function(biomarker_values, response_values, response_type) {
            # Comprehensive data quality assessment
            
            # Biomarker data quality
            biomarker_missing_pct <- sum(is.na(biomarker_values)) / length(biomarker_values) * 100
            biomarker_zeros_pct <- sum(biomarker_values == 0, na.rm = TRUE) / sum(!is.na(biomarker_values)) * 100
            biomarker_cv <- sd(biomarker_values, na.rm = TRUE) / mean(biomarker_values, na.rm = TRUE)
            
            # Response data quality
            if (response_type %in% c("binary", "categorical")) {
                response_factor <- as.factor(response_values)
                group_sizes <- table(response_factor)
                min_group_size <- min(group_sizes)
                group_balance <- min(group_sizes) / max(group_sizes)
            }
            
            # Quality warnings
            if (biomarker_missing_pct > 20) {
                warning(paste("High missing data rate for biomarker (", round(biomarker_missing_pct, 1), "%)."))
            }
            
            if (biomarker_zeros_pct > 30) {
                warning(paste("High proportion of zero values in biomarker (", round(biomarker_zeros_pct, 1), "%)."))
            }
            
            if (biomarker_cv > 2) {
                warning("High coefficient of variation in biomarker data. Consider transformation.")
            }
            
            if (response_type %in% c("binary", "categorical") && exists("group_balance") && group_balance < 0.2) {
                warning("Severely imbalanced response groups detected. Results may be biased.")
            }
            
            private$.checkpoint()
        },
        
        .enhanceThresholdAnalysis = function(benford_obj, cleaned_data, threshold_value) {
            # Enhanced threshold analysis with confidence intervals
            
            if (is.null(threshold_value)) {
                return(NULL)
            }
            
            tryCatch({
                # Calculate confidence intervals for threshold metrics
                n_bootstrap <- 1000
                bootstrap_results <- replicate(n_bootstrap, {
                    # Bootstrap sampling
                    boot_indices <- sample(length(cleaned_data), replace = TRUE)
                    boot_biomarker <- cleaned_data[boot_indices]
                    boot_response <- response_values[boot_indices]
                    
                    # Calculate metrics for bootstrap sample
                    boot_metrics <- private$.calculateThresholdMetrics(boot_biomarker, boot_response, threshold_value)
                    return(c(boot_metrics$sensitivity, boot_metrics$specificity, 
                            boot_metrics$ppv, boot_metrics$npv, boot_metrics$auc))
                }, simplify = TRUE)
                
                # Calculate confidence intervals
                ci_level <- as.numeric(self$options$confidenceLevel)
                alpha <- 1 - ci_level
                
                ci_lower <- apply(bootstrap_results, 1, quantile, alpha/2, na.rm = TRUE)
                ci_upper <- apply(bootstrap_results, 1, quantile, 1-alpha/2, na.rm = TRUE)
                
                return(list(
                    sensitivity_ci = c(ci_lower[1], ci_upper[1]),
                    specificity_ci = c(ci_lower[2], ci_upper[2]),
                    ppv_ci = c(ci_lower[3], ci_upper[3]),
                    npv_ci = c(ci_lower[4], ci_upper[4]),
                    auc_ci = c(ci_lower[5], ci_upper[5])
                ))
                
            }, error = function(e) {
                warning("Could not calculate confidence intervals for threshold metrics.")
                return(NULL)
            })
        },

        .plot = function(image, ggtheme, theme, ...) {
            plot_data <- image$state
            if (is.null(plot_data)) return()
            
            biomarker <- plot_data$biomarker
            response <- plot_data$response
            response_type <- plot_data$response_type
            plot_type <- plot_data$plot_type
            
            # Create base data frame
            df <- data.frame(
                biomarker = biomarker,
                response = response
            )
            
            # Add grouping variable if present
            if (!is.null(plot_data$group_variable)) {
                df$group <- plot_data$group_variable
            }
            
            # Create different plot types
            if (plot_type == "boxplot" && response_type %in% c("binary", "categorical")) {
                p <- ggplot2::ggplot(df, ggplot2::aes(x = factor(response), y = biomarker, fill = factor(response))) +
                    ggplot2::geom_boxplot() +
                    ggplot2::geom_jitter(width = 0.2, alpha = 0.6)
                
            } else if (plot_type == "violin" && response_type %in% c("binary", "categorical")) {
                p <- ggplot2::ggplot(df, ggplot2::aes(x = factor(response), y = biomarker, fill = factor(response))) +
                    ggplot2::geom_violin() +
                    ggplot2::geom_boxplot(width = 0.1, fill = "white", alpha = 0.7)
                
            } else if (plot_type == "scatter" || response_type == "continuous") {
                p <- ggplot2::ggplot(df, ggplot2::aes(x = biomarker, y = response)) +
                    ggplot2::geom_point(alpha = 0.6)
                
                # Add trend line if requested
                if (plot_data$add_trend_line) {
                    if (plot_data$trend_method == "lm") {
                        p <- p + ggplot2::geom_smooth(method = "lm", se = TRUE)
                    } else if (plot_data$trend_method == "loess") {
                        p <- p + ggplot2::geom_smooth(method = "loess", se = TRUE)
                    } else if (plot_data$trend_method == "gam") {
                        p <- p + ggplot2::geom_smooth(method = "gam", se = TRUE)
                    }
                }
                
            } else {
                # Default to scatter plot
                p <- ggplot2::ggplot(df, ggplot2::aes(x = biomarker, y = response)) +
                    ggplot2::geom_point(alpha = 0.6)
            }
            
            # Add threshold line if requested
            if (plot_data$show_threshold && !is.null(plot_data$threshold)) {
                if (plot_type == "scatter" || response_type == "continuous") {
                    p <- p + ggplot2::geom_vline(xintercept = plot_data$threshold, 
                                                linetype = "dashed", color = "red")
                } else {
                    p <- p + ggplot2::geom_hline(yintercept = plot_data$threshold, 
                                                linetype = "dashed", color = "red")
                }
            }
            
            # Add grouping colors if group variable is present
            if (!is.null(plot_data$group_variable)) {
                p <- p + ggplot2::aes(color = group)
            }
            
            # Labels and theme
            p <- p + ggplot2::labs(
                x = plot_data$biomarker_name,
                y = plot_data$response_name,
                title = "Biomarker-Response Association",
                subtitle = paste("Analysis type:", response_type)
            ) +
                ggtheme +
                ggplot2::theme(
                    legend.position = "right"
                )
            
            print(p)
            TRUE
        },
        
        .calculateCliffsDelta = function(x, y) {
            # Cliff's Delta for biomarker comparison between response groups
            n1 <- length(x)
            n2 <- length(y)
            
            greater <- 0
            less <- 0
            
            for (xi in x) {
                for (yj in y) {
                    if (xi > yj) greater <- greater + 1
                    else if (xi < yj) less <- less + 1
                }
            }
            
            delta <- (greater - less) / (n1 * n2)
            return(delta)
        },
        
        .calculateHodgesLehmann = function(x, y) {
            # Hodges-Lehmann shift for biomarker level difference
            differences <- c()
            
            for (xi in x) {
                for (yj in y) {
                    differences <- c(differences, xi - yj)
                }
            }
            
            # Return median of all pairwise differences
            return(median(differences))
        },
        
        .interpretBiomarkerEffects = function(p_value, cliff_delta, hodges_lehmann, group_levels) {
            # Clinical interpretation for biomarker-response associations
            significance <- ifelse(p_value < 0.05, "Significant", "Non-significant")
            abs_delta <- abs(cliff_delta)
            abs_shift <- abs(hodges_lehmann)
            
            # Probability interpretation
            prob <- round((cliff_delta + 1) / 2 * 100, 1)
            direction <- ifelse(cliff_delta > 0, "higher", "lower")
            better_group <- ifelse(cliff_delta > 0, group_levels[1], group_levels[2])
            
            # Effect magnitude
            if (abs_delta < 0.147) {
                effect_size <- "negligible"
            } else if (abs_delta < 0.33) {
                effect_size <- "small"
            } else if (abs_delta < 0.474) {
                effect_size <- "medium"
            } else {
                effect_size <- "large"
            }
            
            return(paste0(
                significance, " difference (p = ", round(p_value, 4), "). ",
                effect_size, " effect size (δ = ", round(cliff_delta, 3), "). ",
                "Probability that ", group_levels[1], " has ", direction, " biomarker levels: ", prob, "%. ",
                "Typical difference: ", round(abs_shift, 2), " units (",
                ifelse(hodges_lehmann > 0, paste(group_levels[1], "typically higher"), 
                       paste(group_levels[2], "typically higher")), "). ",
                if (abs_delta >= 0.33) "Clinically meaningful biomarker difference." else "Limited clinical significance."
            ))
        }
    )
)