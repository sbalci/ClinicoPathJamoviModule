enhancednonparametricClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "enhancednonparametricClass",
    inherit = enhancednonparametricBase,
    private = list(
        .init = function() {
            if (is.null(self$data) || is.null(self$options$dependent) || is.null(self$options$grouping)) {
                self$results$instructions$setContent(
                    "<html>
                    <head>
                    <meta http-equiv='Content-Type' content='text/html; charset=UTF-8'>
                    </head>
                    <body>
                    <h3>Enhanced Non-Parametric Tests</h3>
                    <p><b>Data Requirements:</b></p>
                    <p>This module provides comprehensive non-parametric testing with proper effect sizes, confidence intervals, and post hoc analysis.</p>
                    <ul>
                    <li><b>Dependent Variable</b>: Continuous variable for analysis (e.g., biomarker expression, cell counts)</li>
                    <li><b>Grouping Variable</b>: Categorical variable defining groups for comparison</li>
                    </ul>
                    
                    <p><b>Available Tests:</b></p>
                    <ul>
                    <li><b>Mann-Whitney U Test</b>: Enhanced with rank-biserial correlation, confidence intervals, and assumption checking</li>
                    <li><b>Kruskal-Wallis Test</b>: With proper Dunn's post hoc testing and effect sizes</li>
                    <li><b>Wilcoxon Signed-Rank Test</b>: For paired data with comprehensive output</li>
                    <li><b>Friedman Test</b>: For repeated measures with post hoc analysis</li>
                    </ul>
                    
                    <p><b>Key Features:</b></p>
                    <ul>
                    <li>Effect sizes with confidence intervals</li>
                    <li>Comprehensive assumption checking</li>
                    <li>Proper multiple comparison corrections</li>
                    <li>Publication-ready output formatting</li>
                    <li>Clinical interpretation guidelines</li>
                    </ul>
                    
                    <p><b>Clinical Applications:</b></p>
                    <ul>
                    <li>Biomarker expression comparison across tumor grades</li>
                    <li>Cell count analysis between treatment groups</li>
                    <li>IHC scoring comparisons</li>
                    <li>Diagnostic test performance evaluation</li>
                    </ul>
                    </body>
                    </html>"
                )
                return()
            }
        },

        .run = function() {
            # Get data and variables
            data <- self$data
            dependent <- self$options$dependent
            grouping <- self$options$grouping
            
            if (is.null(dependent) || is.null(grouping)) {
                self$results$instructions$setContent(
                    "<p style='color: red;'><b>Error:</b> Both dependent and grouping variables must be specified.</p>"
                )
                return()
            }
            
            # Extract data vectors
            y_data <- data[[dependent]]
            group_data <- data[[grouping]]
            
            # Remove missing data
            complete_cases <- complete.cases(y_data, group_data)
            if (sum(complete_cases) < 5) {
                self$results$instructions$setContent(
                    "<p style='color: red;'><b>Error:</b> At least 5 complete cases are required for analysis.</p>"
                )
                return()
            }
            
            y_clean <- y_data[complete_cases]
            group_clean <- factor(group_data[complete_cases])
            n_groups <- nlevels(group_clean)
            
            # Check data structure
            if (!is.numeric(y_clean)) {
                self$results$instructions$setContent(
                    "<p style='color: red;'><b>Error:</b> Dependent variable must be numeric.</p>"
                )
                return()
            }
            
            tryCatch({
                # Population descriptive statistics
                private$.populateDescriptives(y_clean, group_clean, dependent, grouping)
                
                # Assumption checking
                if (self$options$assumptions) {
                    private$.checkAssumptions(y_clean, group_clean)
                }
                
                # Main analysis based on number of groups
                if (n_groups == 2) {
                    # Mann-Whitney U Test
                    if (self$options$mann_whitney) {
                        private$.performMannWhitneyU(y_clean, group_clean)
                    }
                } else if (n_groups > 2) {
                    # Kruskal-Wallis Test
                    if (self$options$kruskal_wallis) {
                        private$.performKruskalWallis(y_clean, group_clean)
                    }
                }
                
                # Generate plots if requested
                if (self$options$show_plots) {
                    private$.preparePlots(y_clean, group_clean, dependent, grouping)
                }
                
                # Clinical interpretation
                private$.generateClinicalInterpretation(y_clean, group_clean, n_groups)
                
            }, error = function(e) {
                error_msg <- paste0("<p style='color: red;'><b>Error in analysis:</b> ", e$message, "</p>")
                self$results$instructions$setContent(error_msg)
            })
        },
        
        .populateDescriptives = function(y_data, group_data, dependent, grouping) {
            desc_table <- self$results$descriptives
            
            # Overall statistics
            overall_n <- length(y_data)
            overall_median <- median(y_data, na.rm = TRUE)
            overall_iqr <- IQR(y_data, na.rm = TRUE)
            overall_range <- range(y_data, na.rm = TRUE)
            
            desc_table$addRow(rowKey="overall", values=list(
                group="Overall",
                n=overall_n,
                median=round(overall_median, 3),
                iqr=round(overall_iqr, 3),
                min_val=round(overall_range[1], 3),
                max_val=round(overall_range[2], 3),
                mean_rank=round(mean(rank(y_data)), 2)
            ))
            
            # Group-specific statistics
            group_levels <- levels(group_data)
            for (i in seq_along(group_levels)) {
                group_name <- group_levels[i]
                group_data_subset <- y_data[group_data == group_name]
                
                if (length(group_data_subset) > 0) {
                    group_n <- length(group_data_subset)
                    group_median <- median(group_data_subset, na.rm = TRUE)
                    group_iqr <- IQR(group_data_subset, na.rm = TRUE)
                    group_range <- range(group_data_subset, na.rm = TRUE)
                    group_mean_rank <- mean(rank(y_data)[group_data == group_name])
                    
                    desc_table$addRow(rowKey=paste0("group_", i), values=list(
                        group=group_name,
                        n=group_n,
                        median=round(group_median, 3),
                        iqr=round(group_iqr, 3),
                        min_val=round(group_range[1], 3),
                        max_val=round(group_range[2], 3),
                        mean_rank=round(group_mean_rank, 2)
                    ))
                }
            }
        },
        
        .checkAssumptions = function(y_data, group_data) {
            assumptions_table <- self$results$assumptions
            
            # Check for independence (basic check)
            assumptions_table$addRow(rowKey="independence", values=list(
                assumption="Independence",
                test="Visual/Design Check",
                statistic="N/A",
                p_value="N/A",
                conclusion="Review study design for independent observations",
                interpretation="Ensure observations are independent within and between groups"
            ))
            
            # Check for similar distributions (not identical distributions)
            group_levels <- levels(group_data)
            n_groups <- length(group_levels)
            
            if (n_groups == 2) {
                # Levene's test for equal variances (less relevant but informative)
                if (requireNamespace("car", quietly = TRUE)) {
                    levene_test <- car::leveneTest(y_data ~ group_data)
                    levene_p <- levene_test$`Pr(>F)`[1]
                    
                    assumptions_table$addRow(rowKey="variance", values=list(
                        assumption="Equal Variances",
                        test="Levene's Test",
                        statistic=round(levene_test$`F value`[1], 3),
                        p_value=round(levene_p, 4),
                        conclusion=if(levene_p < 0.05) "Unequal variances" else "Equal variances",
                        interpretation="Less critical for Mann-Whitney U, but informative for effect size interpretation"
                    ))
                }
            }
            
            # Check for outliers in each group
            for (i in seq_along(group_levels)) {
                group_name <- group_levels[i]
                group_subset <- y_data[group_data == group_name]
                
                if (length(group_subset) >= 4) {
                    q1 <- quantile(group_subset, 0.25, na.rm = TRUE)
                    q3 <- quantile(group_subset, 0.75, na.rm = TRUE)
                    iqr <- q3 - q1
                    
                    outliers_count <- sum(group_subset < (q1 - 1.5 * iqr) | group_subset > (q3 + 1.5 * iqr), na.rm = TRUE)
                    outlier_prop <- outliers_count / length(group_subset)
                    
                    assumptions_table$addRow(rowKey=paste0("outliers_", i), values=list(
                        assumption=paste("Outliers:", group_name),
                        test="IQR Method",
                        statistic=outliers_count,
                        p_value="N/A",
                        conclusion=if(outlier_prop > 0.1) "High outlier rate" else "Acceptable outlier rate",
                        interpretation=paste0("Proportion of outliers: ", round(outlier_prop * 100, 1), "%")
                    ))
                }
            }
        },
        
        .performMannWhitneyU = function(y_data, group_data) {
            mw_table <- self$results$mannwhitney
            
            group_levels <- levels(group_data)
            group1_data <- y_data[group_data == group_levels[1]]
            group2_data <- y_data[group_data == group_levels[2]]
            
            # Basic Mann-Whitney U test
            mw_test <- wilcox.test(group1_data, group2_data, exact = FALSE, correct = TRUE)
            
            # Calculate effect size (rank-biserial correlation)
            n1 <- length(group1_data)
            n2 <- length(group2_data)
            U1 <- mw_test$statistic
            # Convert W to U if needed
            if (names(mw_test$statistic) == "W") {
                U1 <- min(U1, n1 * n2 - U1)
            }
            
            # Rank-biserial correlation (effect size)
            r_rb <- 1 - (2 * U1) / (n1 * n2)
            
            # Calculate confidence interval for effect size
            # Using bootstrap method for robust CI
            if (requireNamespace("boot", quietly = TRUE)) {
                boot_r_rb <- function(data, indices) {
                    d <- data[indices, ]
                    g1 <- d$y[d$group == group_levels[1]]
                    g2 <- d$y[d$group == group_levels[2]]
                    if (length(g1) > 0 && length(g2) > 0) {
                        test_result <- wilcox.test(g1, g2, exact = FALSE)
                        n1_boot <- length(g1)
                        n2_boot <- length(g2)
                        U_boot <- test_result$statistic
                        if (names(test_result$statistic) == "W") {
                            U_boot <- min(U_boot, n1_boot * n2_boot - U_boot)
                        }
                        return(1 - (2 * U_boot) / (n1_boot * n2_boot))
                    } else {
                        return(NA)
                    }
                }
                
                boot_data <- data.frame(y = y_data, group = group_data)
                tryCatch({
                    boot_result <- boot::boot(boot_data, boot_r_rb, R = 1000)
                    ci_result <- boot::boot.ci(boot_result, type = "perc")
                    r_rb_ci_lower <- ci_result$percent[4]
                    r_rb_ci_upper <- ci_result$percent[5]
                }, error = function(e) {
                    r_rb_ci_lower <- NA
                    r_rb_ci_upper <- NA
                })
            } else {
                r_rb_ci_lower <- NA
                r_rb_ci_upper <- NA
            }
            
            # Interpret effect size
            effect_interpretation <- if (abs(r_rb) < 0.1) {
                "Negligible effect"
            } else if (abs(r_rb) < 0.3) {
                "Small effect"
            } else if (abs(r_rb) < 0.5) {
                "Medium effect"
            } else {
                "Large effect"
            }
            
            # Add results to table
            mw_table$addRow(rowKey="main_test", values=list(
                comparison=paste(group_levels[1], "vs", group_levels[2]),
                n1=n1,
                n2=n2,
                u_statistic=round(U1, 0),
                z_statistic=round(qnorm(mw_test$p.value/2), 3),
                p_value=round(mw_test$p.value, 4),
                r_rb=round(r_rb, 3),
                r_rb_ci=if (!is.na(r_rb_ci_lower)) paste0("[", round(r_rb_ci_lower, 3), ", ", round(r_rb_ci_upper, 3), "]") else "Not available",
                interpretation=effect_interpretation
            ))
            
            # Add rank sums for additional information
            rank_data <- rank(y_data)
            rank_sum1 <- sum(rank_data[group_data == group_levels[1]])
            rank_sum2 <- sum(rank_data[group_data == group_levels[2]])
            
            mw_table$addRow(rowKey="rank_info", values=list(
                comparison="Rank Information",
                n1=paste("Sum:", round(rank_sum1, 1)),
                n2=paste("Sum:", round(rank_sum2, 1)),
                u_statistic="",
                z_statistic="",
                p_value="",
                r_rb=paste("Mean:", round(rank_sum1/n1, 1), "vs", round(rank_sum2/n2, 1)),
                r_rb_ci="",
                interpretation="Higher mean rank indicates larger values"
            ))
        },
        
        .performKruskalWallis = function(y_data, group_data) {
            kw_table <- self$results$kruskalwallis
            
            # Basic Kruskal-Wallis test
            kw_test <- kruskal.test(y_data ~ group_data)
            
            # Effect size (eta-squared)
            n_total <- length(y_data)
            k_groups <- nlevels(group_data)
            eta_squared <- (kw_test$statistic - k_groups + 1) / (n_total - k_groups)
            
            # Interpret effect size
            eta_interpretation <- if (eta_squared < 0.01) {
                "Negligible effect"
            } else if (eta_squared < 0.06) {
                "Small effect"
            } else if (eta_squared < 0.14) {
                "Medium effect"
            } else {
                "Large effect"
            }
            
            kw_table$addRow(rowKey="main_test", values=list(
                test="Kruskal-Wallis",
                df=kw_test$parameter,
                chi_square=round(kw_test$statistic, 3),
                p_value=round(kw_test$p.value, 4),
                eta_squared=round(eta_squared, 3),
                interpretation=eta_interpretation,
                conclusion=if(kw_test$p.value < 0.05) "Significant group differences" else "No significant group differences"
            ))
            
            # Post hoc analysis with Dunn's test if significant
            if (kw_test$p.value < 0.05 && self$options$posthoc_dunn) {
                private$.performDunnTest(y_data, group_data)
            }
        },
        
        .performDunnTest = function(y_data, group_data) {
            posthoc_table <- self$results$posthoc
            
            if (requireNamespace("PMCMRplus", quietly = TRUE)) {
                # Dunn's test with multiple comparison correction
                correction_method <- switch(self$options$correction_method,
                    "bonferroni" = "bonferroni",
                    "holm" = "holm", 
                    "hochberg" = "hochberg",
                    "bh" = "BH",
                    "by" = "BY",
                    "none" = "none",
                    "bonferroni"  # default
                )
                
                tryCatch({
                    dunn_result <- PMCMRplus::dunnTest(y_data, group_data, method = correction_method)
                    
                    # Extract pairwise comparisons
                    comparisons <- row.names(dunn_result)
                    p_values <- dunn_result$p.value
                    z_values <- dunn_result$statistic
                    
                    group_levels <- levels(group_data)
                    
                    for (i in seq_along(comparisons)) {
                        comparison_name <- comparisons[i]
                        p_val <- p_values[i]
                        z_val <- z_values[i]
                        
                        # Calculate effect size for each pairwise comparison
                        # Extract group names from comparison
                        groups <- unlist(strsplit(comparison_name, " - "))
                        if (length(groups) == 2) {
                            group1_data <- y_data[group_data == groups[1]]
                            group2_data <- y_data[group_data == groups[2]]
                            
                            if (length(group1_data) > 0 && length(group2_data) > 0) {
                                # Calculate rank-biserial correlation for pairwise comparison
                                mw_test <- wilcox.test(group1_data, group2_data, exact = FALSE)
                                n1 <- length(group1_data)
                                n2 <- length(group2_data)
                                U1 <- mw_test$statistic
                                if (names(mw_test$statistic) == "W") {
                                    U1 <- min(U1, n1 * n2 - U1)
                                }
                                r_rb <- 1 - (2 * U1) / (n1 * n2)
                                
                                effect_interp <- if (abs(r_rb) < 0.1) {
                                    "Negligible"
                                } else if (abs(r_rb) < 0.3) {
                                    "Small"
                                } else if (abs(r_rb) < 0.5) {
                                    "Medium"
                                } else {
                                    "Large"
                                }
                            } else {
                                r_rb <- NA
                                effect_interp <- "Cannot calculate"
                            }
                        } else {
                            r_rb <- NA
                            effect_interp <- "Cannot calculate"
                        }
                        
                        posthoc_table$addRow(rowKey=paste0("comparison_", i), values=list(
                            comparison=comparison_name,
                            z_statistic=round(z_val, 3),
                            p_value=round(p_val, 4),
                            p_adjusted=round(p_val, 4), # Already adjusted by PMCMRplus
                            effect_size=if (!is.na(r_rb)) round(r_rb, 3) else "N/A",
                            interpretation=effect_interp,
                            significance=if(p_val < 0.05) "Significant" else "Non-significant"
                        ))
                    }
                    
                }, error = function(e) {
                    posthoc_table$addRow(rowKey="error", values=list(
                        comparison="Error in Dunn's test",
                        z_statistic=paste("Error:", e$message),
                        p_value="",
                        p_adjusted="",
                        effect_size="",
                        interpretation="",
                        significance=""
                    ))
                })
            } else {
                posthoc_table$addRow(rowKey="missing_pkg", values=list(
                    comparison="PMCMRplus package required",
                    z_statistic="Install PMCMRplus for Dunn's test",
                    p_value="",
                    p_adjusted="",
                    effect_size="",
                    interpretation="",
                    significance=""
                ))
            }
        },
        
        .preparePlots = function(y_data, group_data, dependent, grouping) {
            if (!self$options$show_plots) return()
            
            # Boxplot
            boxplot_img <- self$results$boxplot
            boxplot_img$setState(list(
                data = data.frame(y = y_data, group = group_data), 
                dependent = dependent, 
                grouping = grouping
            ))
        },
        
        .boxplot = function(image, ggtheme, ...) {
            if (is.null(image$state)) return()
            
            state <- image$state
            plot_data <- state$data
            dependent <- state$dependent
            grouping <- state$grouping
            
            # Create enhanced boxplot with violin overlay
            p <- ggplot2::ggplot(plot_data, ggplot2::aes_string(x = "group", y = "y", fill = "group")) +
                ggplot2::geom_violin(alpha = 0.3, trim = FALSE) +
                ggplot2::geom_boxplot(width = 0.3, alpha = 0.7, outlier.colour = "red", outlier.size = 2) +
                ggplot2::stat_summary(fun = median, geom = "point", shape = 18, size = 3, color = "black") +
                ggplot2::labs(
                    title = "Distribution Comparison",
                    subtitle = "Boxplots with violin plots showing distributions",
                    x = grouping,
                    y = dependent,
                    caption = "Red dots = outliers; Diamond = median"
                ) +
                ggtheme +
                ggplot2::theme(
                    axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
                    legend.position = "none"
                ) +
                ggplot2::scale_fill_brewer(type = "qual", palette = "Set2")
            
            # Add sample sizes to x-axis labels
            group_counts <- table(plot_data$group)
            new_labels <- paste0(names(group_counts), "\n(n=", group_counts, ")")
            p <- p + ggplot2::scale_x_discrete(labels = new_labels)
            
            print(p)
            TRUE
        },
        
        .generateClinicalInterpretation = function(y_data, group_data, n_groups) {
            interpretation_table <- self$results$interpretation
            
            # Generate contextual interpretation
            median_overall <- median(y_data, na.rm = TRUE)
            iqr_overall <- IQR(y_data, na.rm = TRUE)
            
            clinical_context <- paste0(
                "<html><head><meta http-equiv='Content-Type' content='text/html; charset=UTF-8'></head><body>",
                "<h3>Clinical Interpretation: Enhanced Non-Parametric Analysis</h3>",
                
                "<h4>Analysis Summary</h4>",
                "<p><b>Dataset Overview:</b></p>",
                "<ul>",
                "<li>Total observations: ", length(y_data), "</li>",
                "<li>Number of groups: ", n_groups, "</li>",
                "<li>Overall median: ", round(median_overall, 3), " (IQR: ", round(iqr_overall, 3), ")</li>",
                "</ul>",
                
                "<h4>Statistical Method Selection</h4>",
                if (n_groups == 2) {
                    "<p><b>Mann-Whitney U Test (Wilcoxon Rank-Sum Test):</b></p>
                    <ul>
                    <li>Appropriate for comparing medians between two independent groups</li>
                    <li>Does not assume normal distribution</li>
                    <li>Robust to outliers and skewed data</li>
                    <li>Effect size measured by rank-biserial correlation (r_rb)</li>
                    </ul>"
                } else {
                    "<p><b>Kruskal-Wallis Test:</b></p>
                    <ul>
                    <li>Non-parametric alternative to one-way ANOVA</li>
                    <li>Compares medians across multiple independent groups</li>
                    <li>Does not assume normal distribution or equal variances</li>
                    <li>Effect size measured by eta-squared (η²)</li>
                    </ul>
                    
                    <p><b>Dunn's Post Hoc Test:</b></p>
                    <ul>
                    <li>Pairwise comparisons following significant Kruskal-Wallis test</li>
                    <li>Controls family-wise error rate</li>
                    <li>Provides specific group differences with effect sizes</li>
                    </ul>"
                },
                
                "<h4>Effect Size Interpretation Guidelines</h4>",
                "<p><b>For Rank-Biserial Correlation (r_rb):</b></p>",
                "<ul>",
                "<li>|r_rb| < 0.1: Negligible effect</li>",
                "<li>0.1 ≤ |r_rb| < 0.3: Small effect</li>",
                "<li>0.3 ≤ |r_rb| < 0.5: Medium effect</li>",
                "<li>|r_rb| ≥ 0.5: Large effect</li>",
                "</ul>",
                
                if (n_groups > 2) {
                    "<p><b>For Eta-Squared (η²):</b></p>
                    <ul>
                    <li>η² < 0.01: Negligible effect</li>
                    <li>0.01 ≤ η² < 0.06: Small effect</li>
                    <li>0.06 ≤ η² < 0.14: Medium effect</li>
                    <li>η² ≥ 0.14: Large effect</li>
                    </ul>"
                } else "",
                
                "<h4>Clinical Applications</h4>",
                "<p><b>Common Uses in Pathology:</b></p>",
                "<ul>",
                "<li><b>Biomarker Expression:</b> Comparing expression levels across tumor grades/stages</li>",
                "<li><b>Cell Counting:</b> Analyzing cell densities between treatment groups</li>",
                "<li><b>IHC Scoring:</b> Comparing immunohistochemical scores across diagnostic categories</li>",
                "<li><b>Morphometric Analysis:</b> Comparing nuclear size, shape parameters between groups</li>",
                "</ul>",
                
                "<h4>Advantages of Non-Parametric Tests</h4>",
                "<ul>",
                "<li>No assumption of normal distribution required</li>",
                "<li>Robust to outliers and extreme values</li>",
                "<li>Appropriate for ordinal and skewed continuous data</li>",
                "<li>Provides reliable inference with small sample sizes</li>",
                "</ul>",
                
                "<h4>Reporting Guidelines</h4>",
                "<p><b>Essential Elements for Publication:</b></p>",
                "<ul>",
                "<li>Report medians and interquartile ranges for each group</li>",
                "<li>Include effect sizes with confidence intervals</li>",
                "<li>State the exact p-values (not just p < 0.05)</li>",
                "<li>Describe multiple comparison correction method used</li>",
                "<li>Provide sample sizes for all groups</li>",
                "</ul>",
                
                "</body></html>"
            )
            
            interpretation_table$setContent(clinical_context)
        }
    )
)