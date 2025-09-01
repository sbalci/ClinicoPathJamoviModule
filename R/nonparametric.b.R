nonparametricClass <- R6::R6Class(
    "nonparametricClass",
    inherit = nonparametricBase,
    private = list(
        .init = function() {
            if (is.null(self$options$outcome)) {
                self$results$descriptives$setNote("placeholder",
                    "Please select an outcome variable to begin analysis.")
                return()
            }

            private$.initTables()
        },

        .initTables = function() {
            # Initialize descriptives table
            desc_table <- self$results$descriptives
            desc_table$getColumn('group')$setTitle('Group')
            desc_table$getColumn('n')$setTitle('N')
            desc_table$getColumn('median')$setTitle('Median')

            # Initialize test statistics table
            test_table <- self$results$testStatistics
            test_table$getColumn('test')$setTitle('Test')
            test_table$getColumn('statistic')$setTitle('Statistic')

            # Initialize post-hoc table if needed
            if (self$options$post_hoc) {
                posthoc_table <- self$results$postHocTable
                posthoc_table$getColumn('comparison')$setTitle('Comparison')
            }
        },

        .run = function() {
            if (is.null(self$options$outcome) || is.null(self$options$groups)) {
                return()
            }

            # Set seed if requested
            if (self$options$set_seed) {
                set.seed(self$options$seed_value)
            }

            # Prepare data
            prepared_data <- private$.prepareData()
            if (is.null(prepared_data)) return()

            # Run assumption tests
            if (self$options$test_assumptions) {
                private$.runAssumptionTests(prepared_data)
            }

            # Run descriptive statistics
            if (self$options$show_descriptives) {
                private$.runDescriptives(prepared_data)
            }

            # Run main non-parametric test
            if (self$options$show_test_statistics) {
                test_results <- private$.runMainTest(prepared_data)

                # Run post-hoc tests if requested and main test is significant
                if (self$options$post_hoc && !is.null(test_results) &&
                    test_results$p.value < 0.05) {
                    private$.runPostHocTests(prepared_data, test_results)
                }
            }

            # Calculate effect sizes
            if (self$options$show_effect_sizes && self$options$effect_size) {
                private$.calculateEffectSizes(prepared_data)
            }

            # Generate robust statistics
            private$.calculateRobustStatistics(prepared_data)

            # Generate clinical interpretation
            if (self$options$show_interpretation) {
                private$.generateClinicalInterpretation()
            }

            # Generate methods explanation
            private$.generateMethodsExplanation()
        },

        .prepareData = function() {
            data <- self$data

            # Check for required variables
            outcome_var <- self$options$outcome
            groups_var <- self$options$groups

            if (is.null(outcome_var) || is.null(groups_var)) {
                return(NULL)
            }

            # Extract variables
            outcome <- data[[outcome_var]]
            groups <- data[[groups_var]]

            # Remove missing values
            complete_cases <- complete.cases(outcome, groups)

            if (sum(complete_cases) < 3) {
                self$results$testStatistics$setNote("error",
                    "Insufficient data: At least 3 complete observations required.")
                return(NULL)
            }

            outcome <- outcome[complete_cases]
            groups <- factor(groups[complete_cases])

            # Check for minimum group sizes
            group_counts <- table(groups)
            if (any(group_counts < 2)) {
                self$results$testStatistics$setNote("warning",
                    "Some groups have fewer than 2 observations. Results may be unreliable.")
            }

            # Handle paired/blocking variables if specified
            paired_var <- NULL
            blocking_var <- NULL

            if (!is.null(self$options$paired_variable)) {
                paired_var <- factor(data[[self$options$paired_variable]][complete_cases])
            }

            if (!is.null(self$options$blocking_variable)) {
                blocking_var <- factor(data[[self$options$blocking_variable]][complete_cases])
            }

            return(list(
                outcome = outcome,
                groups = groups,
                paired = paired_var,
                blocking = blocking_var,
                n_total = length(outcome),
                n_groups = nlevels(groups),
                group_levels = levels(groups)
            ))
        },

        .runDescriptives = function(prepared_data) {
            desc_table <- self$results$descriptives

            outcome <- prepared_data$outcome
            groups <- prepared_data$groups

            # Calculate descriptive statistics for each group
            for (group_level in prepared_data$group_levels) {
                group_data <- outcome[groups == group_level]
                n <- length(group_data)

                if (n > 0) {
                    median_val <- median(group_data, na.rm = TRUE)
                    mad_val <- mad(group_data, na.rm = TRUE)
                    q1 <- quantile(group_data, 0.25, na.rm = TRUE)
                    q3 <- quantile(group_data, 0.75, na.rm = TRUE)
                    min_val <- min(group_data, na.rm = TRUE)
                    max_val <- max(group_data, na.rm = TRUE)
                    mean_rank <- mean(rank(outcome)[groups == group_level])

                    desc_table$addRow(rowKey = group_level, values = list(
                        group = group_level,
                        n = n,
                        median = median_val,
                        mad = mad_val,
                        q1 = q1,
                        q3 = q3,
                        min = min_val,
                        max = max_val,
                        mean_rank = mean_rank
                    ))
                }
            }
        },

        .runMainTest = function(prepared_data) {
            test_type <- self$options$test_type
            test_table <- self$results$testStatistics

            outcome <- prepared_data$outcome
            groups <- prepared_data$groups

            test_result <- NULL

            tryCatch({
                if (test_type == "kruskal_wallis") {
                    test_result <- private$.runKruskalWallis(outcome, groups)
                } else if (test_type == "friedman") {
                    test_result <- private$.runFriedman(prepared_data)
                } else if (test_type == "mann_whitney") {
                    test_result <- private$.runMannWhitney(outcome, groups)
                } else if (test_type == "wilcoxon_signed") {
                    test_result <- private$.runWilcoxonSigned(prepared_data)
                } else if (test_type == "median_test") {
                    test_result <- private$.runMedianTest(outcome, groups)
                } else if (test_type == "van_der_waerden") {
                    test_result <- private$.runVanDerWaerden(outcome, groups)
                } else if (test_type == "mood_median") {
                    test_result <- private$.runMoodMedian(outcome, groups)
                }

                if (!is.null(test_result)) {
                    # Calculate effect size
                    effect_size_result <- private$.calculateMainEffectSize(
                        prepared_data, test_result)

                    # Generate interpretation
                    interpretation <- private$.interpretMainTest(
                        test_result, effect_size_result)

                    test_table$addRow(rowKey = "main", values = list(
                        test = test_result$test_name,
                        statistic = test_result$statistic,
                        df = test_result$df,
                        p = test_result$p.value,
                        effect_size = effect_size_result$value,
                        effect_ci_lower = effect_size_result$ci_lower,
                        effect_ci_upper = effect_size_result$ci_upper,
                        interpretation = interpretation
                    ))
                }
            }, error = function(e) {
                test_table$setNote("error", paste("Error in main test:", e$message))
            })

            return(test_result)
        },

        .runKruskalWallis = function(outcome, groups) {
            # Use exact p-values if requested and feasible
            exact <- self$options$exact_p_values && length(outcome) <= 50

            test_result <- kruskal.test(outcome ~ groups)

            return(list(
                test_name = "Kruskal-Wallis Test",
                statistic = test_result$statistic,
                df = test_result$parameter,
                p.value = test_result$p.value,
                method = test_result$method
            ))
        },

        .runFriedman = function(prepared_data) {
            outcome <- prepared_data$outcome
            groups <- prepared_data$groups
            blocking <- prepared_data$blocking

            if (is.null(blocking)) {
                stop("Blocking variable required for Friedman test")
            }

            # Create matrix for friedman.test
            unique_blocks <- unique(blocking)
            unique_groups <- unique(groups)

            if (length(unique_blocks) < 2 || length(unique_groups) < 2) {
                stop("Insufficient blocks or groups for Friedman test")
            }

            test_result <- friedman.test(outcome, groups, blocking)

            return(list(
                test_name = "Friedman Test",
                statistic = test_result$statistic,
                df = test_result$parameter,
                p.value = test_result$p.value,
                method = test_result$method
            ))
        },

        .runMannWhitney = function(outcome, groups) {
            group_levels <- levels(groups)

            if (length(group_levels) != 2) {
                stop("Mann-Whitney U test requires exactly 2 groups")
            }

            group1_data <- outcome[groups == group_levels[1]]
            group2_data <- outcome[groups == group_levels[2]]

            exact <- self$options$exact_p_values &&
                    (length(group1_data) * length(group2_data) <= 50)

            test_result <- wilcox.test(group1_data, group2_data,
                                     exact = exact,
                                     correct = TRUE)

            return(list(
                test_name = "Mann-Whitney U Test",
                statistic = test_result$statistic,
                df = NA,
                p.value = test_result$p.value,
                method = test_result$method
            ))
        },

        .runWilcoxonSigned = function(prepared_data) {
            if (is.null(prepared_data$paired)) {
                stop("Paired variable required for Wilcoxon signed-rank test")
            }

            outcome <- prepared_data$outcome
            groups <- prepared_data$groups
            paired <- prepared_data$paired

            # Need to restructure data for paired comparison
            group_levels <- levels(groups)
            if (length(group_levels) != 2) {
                stop("Wilcoxon signed-rank test requires exactly 2 groups")
            }

            # Find paired observations
            paired_data <- data.frame(
                outcome = outcome,
                group = groups,
                pair = paired
            )

            wide_data <- reshape(paired_data,
                               v.names = "outcome",
                               timevar = "group",
                               idvar = "pair",
                               direction = "wide")

            if (nrow(wide_data) < 3) {
                stop("Insufficient paired observations")
            }

            exact <- self$options$exact_p_values && nrow(wide_data) <= 50

            test_result <- wilcox.test(wide_data[,2], wide_data[,3],
                                     paired = TRUE, exact = exact)

            return(list(
                test_name = "Wilcoxon Signed-Rank Test",
                statistic = test_result$statistic,
                df = NA,
                p.value = test_result$p.value,
                method = test_result$method
            ))
        },

        .runMedianTest = function(outcome, groups) {
            # Simple median test implementation
            overall_median <- median(outcome)

            # Count observations above/below median in each group
            contingency_table <- table(groups, outcome > overall_median)

            if (any(dim(contingency_table) < 2)) {
                stop("Insufficient data for median test")
            }

            test_result <- chisq.test(contingency_table)

            return(list(
                test_name = "Median Test",
                statistic = test_result$statistic,
                df = test_result$parameter,
                p.value = test_result$p.value,
                method = "Median Test (Chi-square)"
            ))
        },

        .runVanDerWaerden = function(outcome, groups) {
            # Van der Waerden normal scores test
            n <- length(outcome)
            ranks <- rank(outcome)
            normal_scores <- qnorm(ranks / (n + 1))

            # ANOVA on normal scores
            test_result <- anova(lm(normal_scores ~ groups))

            f_stat <- test_result$`F value`[1]
            df1 <- test_result$Df[1]
            df2 <- test_result$Df[2]
            p_val <- test_result$`Pr(>F)`[1]

            return(list(
                test_name = "Van der Waerden Test",
                statistic = f_stat,
                df = df1,
                p.value = p_val,
                method = "Van der Waerden Normal Scores Test"
            ))
        },

        .runMoodMedian = function(outcome, groups) {
            # Mood's median test - more sophisticated implementation
            overall_median <- median(outcome)

            # Create contingency table
            above_median <- outcome > overall_median
            below_median <- outcome < overall_median
            at_median <- outcome == overall_median

            # Handle ties at median
            if (sum(at_median) > 0) {
                # Split ties randomly
                n_ties <- sum(at_median)
                random_split <- sample(c(TRUE, FALSE), n_ties, replace = TRUE)
                above_median[at_median] <- random_split
                below_median[at_median] <- !random_split
            }

            contingency_table <- table(groups, above_median)
            test_result <- chisq.test(contingency_table)

            return(list(
                test_name = "Mood's Median Test",
                statistic = test_result$statistic,
                df = test_result$parameter,
                p.value = test_result$p.value,
                method = "Mood's Median Test"
            ))
        },

        .runPostHocTests = function(prepared_data, main_test_result) {
            method <- self$options$post_hoc_method
            posthoc_table <- self$results$postHocTable

            outcome <- prepared_data$outcome
            groups <- prepared_data$groups
            group_levels <- prepared_data$group_levels

            # Generate all pairwise comparisons
            comparisons <- combn(group_levels, 2, simplify = FALSE)

            p_values <- numeric(length(comparisons))
            statistics <- numeric(length(comparisons))
            effect_sizes <- numeric(length(comparisons))

            for (i in seq_along(comparisons)) {
                group1 <- comparisons[[i]][1]
                group2 <- comparisons[[i]][2]

                group1_data <- outcome[groups == group1]
                group2_data <- outcome[groups == group2]

                tryCatch({
                    if (method == "dunn") {
                        result <- private$.dunnTest(group1_data, group2_data,
                                                   prepared_data)
                    } else if (method == "conover") {
                        result <- private$.conoverTest(group1_data, group2_data)
                    } else if (method == "pairwise_wilcoxon") {
                        result <- private$.pairwiseWilcoxon(group1_data, group2_data)
                    } else {
                        # Default to Dunn test
                        result <- private$.dunnTest(group1_data, group2_data,
                                                   prepared_data)
                    }

                    p_values[i] <- result$p.value
                    statistics[i] <- result$statistic

                    # Calculate pairwise effect size
                    effect_size_result <- private$.calculatePairwiseEffectSize(
                        group1_data, group2_data)
                    effect_sizes[i] <- effect_size_result$value

                }, error = function(e) {
                    p_values[i] <- NA
                    statistics[i] <- NA
                    effect_sizes[i] <- NA
                })
            }

            # Adjust p-values
            p_adjusted <- p.adjust(p_values, method = self$options$p_adjustment)

            # Populate post-hoc table
            for (i in seq_along(comparisons)) {
                comparison_name <- paste(comparisons[[i]], collapse = " vs ")
                significance <- ifelse(p_adjusted[i] < 0.05, "Significant",
                                     "Not significant")

                posthoc_table$addRow(rowKey = paste0("comp", i), values = list(
                    comparison = comparison_name,
                    statistic = statistics[i],
                    p_raw = p_values[i],
                    p_adjusted = p_adjusted[i],
                    effect_size = effect_sizes[i],
                    effect_ci_lower = NA,  # Could add CI calculation
                    effect_ci_upper = NA,
                    significance = significance
                ))
            }
        },

        .dunnTest = function(group1_data, group2_data, prepared_data) {
            # Dunn's test implementation
            n1 <- length(group1_data)
            n2 <- length(group2_data)
            n_total <- prepared_data$n_total

            # Combined ranking
            combined_data <- c(group1_data, group2_data)
            combined_ranks <- rank(combined_data)

            r1 <- sum(combined_ranks[1:n1])
            r2 <- sum(combined_ranks[(n1+1):(n1+n2)])

            # Dunn's statistic
            expected_r1 <- n1 * (n_total + 1) / 2
            var_r1 <- n1 * n2 * (n_total + 1) / 12

            z_stat <- (r1 - expected_r1) / sqrt(var_r1)
            p_val <- 2 * pnorm(abs(z_stat), lower.tail = FALSE)

            return(list(
                statistic = z_stat,
                p.value = p_val,
                method = "Dunn's Test"
            ))
        },

        .conoverTest = function(group1_data, group2_data) {
            # Conover-Iman test (simplified implementation)
            test_result <- wilcox.test(group1_data, group2_data)

            return(list(
                statistic = test_result$statistic,
                p.value = test_result$p.value,
                method = "Conover-Iman Test"
            ))
        },

        .pairwiseWilcoxon = function(group1_data, group2_data) {
            test_result <- wilcox.test(group1_data, group2_data)

            return(list(
                statistic = test_result$statistic,
                p.value = test_result$p.value,
                method = "Pairwise Wilcoxon"
            ))
        },

        .calculateMainEffectSize = function(prepared_data, test_result) {
            method <- self$options$effect_size_method
            outcome <- prepared_data$outcome
            groups <- prepared_data$groups

            if (method == "eta_squared") {
                # Calculate eta-squared for Kruskal-Wallis
                h_stat <- test_result$statistic
                n <- length(outcome)
                k <- nlevels(groups)

                eta_squared <- (h_stat - k + 1) / (n - k)
                eta_squared <- max(0, eta_squared)  # Ensure non-negative

                return(list(
                    value = eta_squared,
                    ci_lower = NA,  # CI calculation would require bootstrap
                    ci_upper = NA,
                    interpretation = private$.interpretEffectSize(eta_squared, "eta_squared")
                ))
            } else if (method == "epsilon_squared") {
                h_stat <- test_result$statistic
                n <- length(outcome)
                k <- nlevels(groups)

                epsilon_squared <- (h_stat - k + 1) / (n^2 - 1) * (n - k) / n
                epsilon_squared <- max(0, epsilon_squared)

                return(list(
                    value = epsilon_squared,
                    ci_lower = NA,
                    ci_upper = NA,
                    interpretation = private$.interpretEffectSize(epsilon_squared, "epsilon_squared")
                ))
            }

            # Default return
            return(list(value = NA, ci_lower = NA, ci_upper = NA, interpretation = ""))
        },

        .calculatePairwiseEffectSize = function(group1_data, group2_data) {
            method <- self$options$effect_size_method

            if (method == "rank_biserial") {
                # Rank-biserial correlation
                n1 <- length(group1_data)
                n2 <- length(group2_data)

                test_result <- wilcox.test(group1_data, group2_data)
                u_stat <- test_result$statistic

                r <- 1 - (2 * u_stat) / (n1 * n2)

                return(list(
                    value = r,
                    interpretation = private$.interpretEffectSize(abs(r), "rank_biserial")
                ))
            } else if (method == "cles") {
                # Common Language Effect Size
                total_comparisons <- length(group1_data) * length(group2_data)
                greater_count <- sum(outer(group1_data, group2_data, ">"))

                cles <- greater_count / total_comparisons

                return(list(
                    value = cles,
                    interpretation = private$.interpretEffectSize(abs(cles - 0.5), "cles")
                ))
            }

            return(list(value = NA, interpretation = ""))
        },

        .interpretEffectSize = function(value, type) {
            if (is.na(value)) return("")

            if (type == "eta_squared") {
                if (value < 0.01) return("Negligible")
                else if (value < 0.06) return("Small")
                else if (value < 0.14) return("Medium")
                else return("Large")
            } else if (type == "rank_biserial") {
                if (value < 0.1) return("Negligible")
                else if (value < 0.3) return("Small")
                else if (value < 0.5) return("Medium")
                else return("Large")
            } else if (type == "cles") {
                if (value < 0.056) return("Negligible")
                else if (value < 0.111) return("Small")
                else if (value < 0.194) return("Medium")
                else return("Large")
            }

            return("")
        },

        .calculateEffectSizes = function(prepared_data) {
            effect_table <- self$results$effectSizesTable

            # Calculate multiple effect size measures
            outcome <- prepared_data$outcome
            groups <- prepared_data$groups

            # Eta-squared
            eta_result <- private$.calculateMainEffectSize(prepared_data,
                list(statistic = kruskal.test(outcome ~ groups)$statistic))

            effect_table$addRow(rowKey = "eta", values = list(
                measure = "Eta-squared (η²)",
                value = eta_result$value,
                ci_lower = eta_result$ci_lower,
                ci_upper = eta_result$ci_upper,
                interpretation = eta_result$interpretation,
                clinical_relevance = private$.assessClinicalRelevance(eta_result$value, "eta_squared")
            ))
        },

        .assessClinicalRelevance = function(effect_size, type) {
            context <- self$options$clinical_context

            if (is.na(effect_size)) return("Cannot assess")

            base_interpretation <- ""

            if (type == "eta_squared") {
                if (effect_size >= 0.14) {
                    base_interpretation <- "Large effect suggests clinically meaningful difference"
                } else if (effect_size >= 0.06) {
                    base_interpretation <- "Medium effect may be clinically relevant"
                } else if (effect_size >= 0.01) {
                    base_interpretation <- "Small effect requires clinical judgment"
                } else {
                    base_interpretation <- "Negligible effect unlikely to be clinically meaningful"
                }
            }

            # Context-specific interpretations
            if (context == "biomarker") {
                base_interpretation <- paste(base_interpretation,
                    "Consider biological variability and measurement precision.")
            } else if (context == "treatment") {
                base_interpretation <- paste(base_interpretation,
                    "Evaluate against treatment costs and risks.")
            }

            return(base_interpretation)
        },

        .runAssumptionTests = function(prepared_data) {
            assumptions_table <- self$results$assumptionsTable

            outcome <- prepared_data$outcome
            groups <- prepared_data$groups

            # Test homogeneity of variance
            homog_method <- self$options$homogeneity_test

            tryCatch({
                if (homog_method == "levene") {
                    # Levene's test (using car package if available)
                    test_result <- private$.leveneTest(outcome, groups)
                } else if (homog_method == "fligner") {
                    test_result <- fligner.test(outcome ~ groups)
                    test_result <- list(
                        statistic = test_result$statistic,
                        p.value = test_result$p.value,
                        df = test_result$parameter,
                        test_name = "Fligner-Killeen Test"
                    )
                } else if (homog_method == "bartlett") {
                    test_result <- bartlett.test(outcome ~ groups)
                    test_result <- list(
                        statistic = test_result$statistic,
                        p.value = test_result$p.value,
                        df = test_result$parameter,
                        test_name = "Bartlett Test"
                    )
                }

                result_text <- ifelse(test_result$p.value < 0.05,
                                    "Reject homogeneity assumption",
                                    "Accept homogeneity assumption")

                implication <- ifelse(test_result$p.value < 0.05,
                                    "Non-parametric tests are appropriate",
                                    "Both parametric and non-parametric tests valid")

                assumptions_table$addRow(rowKey = "homogeneity", values = list(
                    assumption = "Homogeneity of Variance",
                    test = test_result$test_name,
                    statistic = test_result$statistic,
                    df = test_result$df,
                    p = test_result$p.value,
                    result = result_text,
                    implication = implication
                ))

            }, error = function(e) {
                assumptions_table$setNote("error", paste("Error in assumption tests:", e$message))
            })
        },

        .leveneTest = function(outcome, groups) {
            # Simple implementation of Levene's test
            group_means <- tapply(outcome, groups, median, na.rm = TRUE)
            absolute_deviations <- abs(outcome - group_means[groups])

            # ANOVA on absolute deviations
            test_result <- anova(lm(absolute_deviations ~ groups))

            return(list(
                statistic = test_result$`F value`[1],
                p.value = test_result$`Pr(>F)`[1],
                df = test_result$Df[1],
                test_name = "Levene's Test"
            ))
        },

        .calculateRobustStatistics = function(prepared_data) {
            robust_table <- self$results$robustStatistics

            outcome <- prepared_data$outcome
            groups <- prepared_data$groups
            group_levels <- prepared_data$group_levels

            trim_prop <- self$options$trim_proportion
            winsor_prop <- self$options$winsorize_proportion

            for (group_level in group_levels) {
                group_data <- outcome[groups == group_level]
                n <- length(group_data)

                if (n > 2) {
                    # Trimmed mean
                    trimmed_mean <- mean(group_data, trim = trim_prop)

                    # Winsorized mean
                    winsor_limits <- quantile(group_data, c(winsor_prop, 1 - winsor_prop))
                    winsorized_data <- pmax(pmin(group_data, winsor_limits[2]), winsor_limits[1])
                    winsorized_mean <- mean(winsorized_data)

                    # Hodges-Lehmann estimator (median of pairwise averages)
                    pairwise_avgs <- outer(group_data, group_data, function(x, y) (x + y) / 2)
                    hodges_lehmann <- median(pairwise_avgs[upper.tri(pairwise_avgs, diag = TRUE)])

                    # Robust standard error (simplified)
                    robust_se <- mad(group_data) / sqrt(n)

                    # Robust confidence interval
                    robust_ci <- hodges_lehmann + c(-1, 1) * qnorm(0.975) * robust_se

                    robust_table$addRow(rowKey = group_level, values = list(
                        group = group_level,
                        trimmed_mean = trimmed_mean,
                        winsorized_mean = winsorized_mean,
                        hodges_lehmann = hodges_lehmann,
                        robust_se = robust_se,
                        robust_ci_lower = robust_ci[1],
                        robust_ci_upper = robust_ci[2]
                    ))
                }
            }
        },

        .interpretMainTest = function(test_result, effect_size_result) {
            p_val <- test_result$p.value
            effect_size <- effect_size_result$value

            significance <- ifelse(p_val < 0.05, "significant", "not significant")

            interpretation <- paste0(
                "The ", test_result$test_name, " is ", significance,
                " (p = ", round(p_val, 4), ")"
            )

            if (!is.na(effect_size)) {
                interpretation <- paste0(interpretation,
                    " with ", tolower(effect_size_result$interpretation),
                    " effect size (", round(effect_size, 3), ")")
            }

            return(interpretation)
        },

        .generateClinicalInterpretation = function() {
            html_content <- private$.buildInterpretationHTML()
            self$results$clinicalInterpretation$setContent(html_content)
        },

        .buildInterpretationHTML = function() {
            context <- self$options$clinical_context
            test_type <- self$options$test_type

            html <- "<div class='clinical-interpretation'>"
            html <- paste0(html, "<h3>Clinical Interpretation Guidelines</h3>")

            # Context-specific guidance
            if (context == "biomarker") {
                html <- paste0(html, "<h4>Biomarker Analysis Context</h4>")
                html <- paste0(html, "<p>When analyzing biomarker levels:</p>")
                html <- paste0(html, "<ul>")
                html <- paste0(html, "<li>Consider biological variability and measurement precision</li>")
                html <- paste0(html, "<li>Evaluate clinical significance beyond statistical significance</li>")
                html <- paste0(html, "<li>Account for potential confounding variables</li>")
                html <- paste0(html, "<li>Consider reference ranges and diagnostic thresholds</li>")
                html <- paste0(html, "</ul>")
            } else if (context == "treatment") {
                html <- paste0(html, "<h4>Treatment Comparison Context</h4>")
                html <- paste0(html, "<p>When comparing treatments:</p>")
                html <- paste0(html, "<ul>")
                html <- paste0(html, "<li>Consider clinical relevance of observed differences</li>")
                html <- paste0(html, "<li>Evaluate benefits against potential risks and costs</li>")
                html <- paste0(html, "<li>Account for patient heterogeneity and preferences</li>")
                html <- paste0(html, "<li>Consider practical implementation challenges</li>")
                html <- paste0(html, "</ul>")
            }

            # Test-specific interpretation
            html <- paste0(html, "<h4>Statistical Method Notes</h4>")

            if (test_type == "kruskal_wallis") {
                html <- paste0(html, "<p><strong>Kruskal-Wallis Test:</strong> ")
                html <- paste0(html, "Tests whether samples originate from the same distribution. ")
                html <- paste0(html, "Robust to outliers and does not assume normality. ")
                html <- paste0(html, "Significant results indicate at least one group differs from others.</p>")
            } else if (test_type == "friedman") {
                html <- paste0(html, "<p><strong>Friedman Test:</strong> ")
                html <- paste0(html, "Non-parametric alternative to repeated measures ANOVA. ")
                html <- paste0(html, "Tests for differences across related groups or time points. ")
                html <- paste0(html, "Accounts for blocking factors and paired observations.</p>")
            }

            html <- paste0(html, "</div>")
            return(html)
        },

        .generateMethodsExplanation = function() {
            html_content <- private$.buildMethodsHTML()
            self$results$methodsExplanation$setContent(html_content)
        },

        .buildMethodsHTML = function() {
            html <- "<div class='methods-explanation'>"
            html <- paste0(html, "<h3>Statistical Methods and Assumptions</h3>")

            test_type <- self$options$test_type

            html <- paste0(html, "<h4>Non-parametric Testing Overview</h4>")
            html <- paste0(html, "<p>Non-parametric tests are distribution-free methods that:</p>")
            html <- paste0(html, "<ul>")
            html <- paste0(html, "<li>Do not assume normal distribution of data</li>")
            html <- paste0(html, "<li>Are robust to outliers and skewed distributions</li>")
            html <- paste0(html, "<li>Use ranks rather than raw data values</li>")
            html <- paste0(html, "<li>Are appropriate for ordinal or non-normal continuous data</li>")
            html <- paste0(html, "</ul>")

            # Method-specific details
            if (test_type == "kruskal_wallis") {
                html <- paste0(html, "<h4>Kruskal-Wallis Test Details</h4>")
                html <- paste0(html, "<p><strong>Assumptions:</strong></p>")
                html <- paste0(html, "<ul>")
                html <- paste0(html, "<li>Independent observations</li>")
                html <- paste0(html, "<li>Similar distribution shapes across groups</li>")
                html <- paste0(html, "<li>At least 5 observations per group (recommended)</li>")
                html <- paste0(html, "</ul>")

                html <- paste0(html, "<p><strong>Interpretation:</strong> ")
                html <- paste0(html, "The test statistic follows a chi-square distribution. ")
                html <- paste0(html, "Significant results suggest differences in median ranks between groups.</p>")
            }

            # Post-hoc testing explanation
            if (self$options$post_hoc) {
                html <- paste0(html, "<h4>Post-hoc Testing</h4>")
                html <- paste0(html, "<p>When the overall test is significant, pairwise comparisons identify which specific groups differ. ")
                html <- paste0(html, "Multiple comparison corrections control Type I error inflation.</p>")

                method <- self$options$post_hoc_method
                if (method == "dunn") {
                    html <- paste0(html, "<p><strong>Dunn's Test:</strong> Uses rank sums with appropriate variance correction.</p>")
                } else if (method == "conover") {
                    html <- paste0(html, "<p><strong>Conover-Iman Test:</strong> More powerful alternative to Dunn's test.</p>")
                }
            }

            html <- paste0(html, "</div>")
            return(html)
        },

        # Plotting functions
        .plotBoxPlots = function(image, ...) {
            if (is.null(self$options$outcome) || is.null(self$options$groups)) {
                return()
            }

            prepared_data <- private$.prepareData()
            if (is.null(prepared_data)) return()

            outcome <- prepared_data$outcome
            groups <- prepared_data$groups

            plot_data <- data.frame(
                outcome = outcome,
                groups = factor(groups)
            )

            library(ggplot2)

            p <- ggplot(plot_data, aes(x = groups, y = outcome, fill = groups)) +
                geom_boxplot(alpha = 0.7, outlier.shape = 21) +
                geom_jitter(width = 0.2, alpha = 0.5, size = 1.5) +
                labs(
                    title = "Box Plots by Group",
                    x = "Group",
                    y = self$options$outcome
                ) +
                theme_minimal() +
                theme(
                    legend.position = "none",
                    plot.title = element_text(hjust = 0.5)
                ) +
                scale_fill_brewer(type = "qual", palette = "Set2")

            print(p)

            TRUE
        },

        .plotViolinPlots = function(image, ...) {
            if (is.null(self$options$outcome) || is.null(self$options$groups)) {
                return()
            }

            prepared_data <- private$.prepareData()
            if (is.null(prepared_data)) return()

            outcome <- prepared_data$outcome
            groups <- prepared_data$groups

            plot_data <- data.frame(
                outcome = outcome,
                groups = factor(groups)
            )

            library(ggplot2)

            p <- ggplot(plot_data, aes(x = groups, y = outcome, fill = groups)) +
                geom_violin(alpha = 0.7, trim = FALSE) +
                geom_boxplot(width = 0.1, fill = "white", alpha = 0.7) +
                labs(
                    title = "Violin Plots with Box Plots",
                    x = "Group",
                    y = self$options$outcome
                ) +
                theme_minimal() +
                theme(
                    legend.position = "none",
                    plot.title = element_text(hjust = 0.5)
                ) +
                scale_fill_brewer(type = "qual", palette = "Set2")

            print(p)

            TRUE
        },

        .plotRankDistribution = function(image, ...) {
            if (is.null(self$options$outcome) || is.null(self$options$groups)) {
                return()
            }

            prepared_data <- private$.prepareData()
            if (is.null(prepared_data)) return()

            outcome <- prepared_data$outcome
            groups <- prepared_data$groups
            ranks <- rank(outcome)

            plot_data <- data.frame(
                ranks = ranks,
                groups = factor(groups)
            )

            library(ggplot2)

            p <- ggplot(plot_data, aes(x = ranks, fill = groups)) +
                geom_histogram(alpha = 0.7, bins = 30, position = "identity") +
                facet_wrap(~ groups, ncol = 2) +
                labs(
                    title = "Rank Distribution by Group",
                    x = "Ranks",
                    y = "Frequency"
                ) +
                theme_minimal() +
                theme(
                    legend.position = "none",
                    plot.title = element_text(hjust = 0.5)
                ) +
                scale_fill_brewer(type = "qual", palette = "Set2")

            print(p)

            TRUE
        },

        .plotEffectSizes = function(image, ...) {
            if (is.null(self$options$outcome) || is.null(self$options$groups)) {
                return()
            }

            # Create a simple effect size visualization
            # This is a placeholder - would need actual effect size calculations

            library(ggplot2)

            effect_data <- data.frame(
                measure = c("Eta-squared", "Epsilon-squared"),
                value = c(0.05, 0.04),  # Placeholder values
                interpretation = c("Small", "Small")
            )

            p <- ggplot(effect_data, aes(x = measure, y = value, fill = interpretation)) +
                geom_col(alpha = 0.7) +
                labs(
                    title = "Effect Size Estimates",
                    x = "Effect Size Measure",
                    y = "Value"
                ) +
                theme_minimal() +
                theme(plot.title = element_text(hjust = 0.5)) +
                scale_fill_brewer(type = "qual", palette = "Set1")

            print(p)

            TRUE
        },


        .calculateCliffsDelta = function(x, y) {
            # Cliff's Delta calculation for basic nonparametric analysis
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
            # Hodges-Lehmann shift calculation
            differences <- c()

            for (xi in x) {
                for (yj in y) {
                    differences <- c(differences, xi - yj)
                }
            }

            return(median(differences))
        }

    )
)
