# Enhanced Non-Parametric Statistical Methods Module
# Comprehensive non-parametric analysis combining features from multiple implementations
# Supports all major non-parametric tests, effect sizes, and robust methods

nonparametricClass <- R6::R6Class(
    "nonparametricClass",
    inherit = nonparametricBase,
    private = list(

        # Escape variable names for safe use in keys and lookups
        .escapeVar = function(x) gsub("[^A-Za-z0-9_]+", "_", make.names(x)),

        .init = function() {

            # Initialize instructions if requested
            if (self$options$show_instructions) {
                private$.populateInstructions()
            }

            if (!private$.hasRequiredVars()) {
                self$results$descriptives$setNote("welcome",
                    .("Please select dependent variable(s) and grouping variable to begin analysis."))
                return()
            }

            # Initialize all tables
            private$.initializeTables()
        },
        
        .hasRequiredVars = function() {
            if (!is.null(self$options$deps) && length(self$options$deps) > 0 && !is.null(self$options$groups)) return(TRUE)
            if (!is.null(self$options$outcome) && !is.null(self$options$groups)) return(TRUE)
            return(FALSE)
        },

        .run = function() {

            # Set seed if requested
            if (self$options$set_seed) {
                set.seed(self$options$seed_value)
            }
            
            if (!private$.hasRequiredVars()) return()

            dep_vars <- if (!is.null(self$options$deps) && length(self$options$deps) > 0) {
                self$options$deps
            } else {
                self$options$outcome
            }
            
            # Process each dependent variable
            for (dep_var in dep_vars) {
                
                # Checkpoint before processing each variable
                private$.checkpoint()
                
                # Get and validate data for this variable
                data <- private$.cleanData(dep_var)
                if (is.null(data)) next
                
                # Run descriptive statistics
                if (self$options$show_descriptives) {
                    private$.populateDescriptives(data, dep_var)
                }
                
                # Run normality tests
                if (self$options$normality_tests) {
                    private$.populateNormality(data, dep_var)
                }
                
                # Run assumption checking
                if (self$options$assumption_checks || self$options$test_assumptions) {
                    private$.populateAssumptions(data, dep_var)
                }
                
                # Run main non-parametric tests
                private$.checkpoint()
                private$.performMainTests(data, dep_var)
                
                # Calculate effect sizes
                if (self$options$effect_size) {
                    private$.checkpoint()
                    private$.calculateEffectSizes(data, dep_var)
                }
                
                # Perform post-hoc tests
                if (self$options$post_hoc) {
                    private$.checkpoint()
                    private$.performPostHocTests(data, dep_var)
                }
                
                # Calculate robust statistics
                if (self$options$show_robust_statistics) {
                    private$.checkpoint()
                    private$.calculateRobustStatistics(data, dep_var)
                }
                
                # Perform power analysis
                if (self$options$show_power_analysis) {
                    private$.checkpoint()
                    private$.performPowerAnalysis(data, dep_var)
                }
            }
            
            # Generate plots
            private$.checkpoint()
            private$.generatePlots()
            
            # Create interpretations and explanations
            private$.checkpoint()
            private$.createInterpretations()

            # Completion notice
            test_name <- switch(self$options$test_type %||% "mann_whitney",
                "mann_whitney" = "Mann-Whitney U",
                "kruskal_wallis" = "Kruskal-Wallis",
                "wilcoxon_signed" = "Wilcoxon Signed-Rank",
                "friedman" = "Friedman",
                "jonckheere_terpstra" = "Jonckheere-Terpstra",
                "sign_test" = "Sign Test",
                self$options$test_type)
            ok_notice <- jmvcore::Notice$new(
                options = self$options, name = 'analysisComplete',
                type = jmvcore::NoticeType$INFO)
            ok_notice$setContent(
                sprintf('%s test completed using %d observations across %s variable(s).',
                        test_name, nrow(self$data), length(dep_vars)))
            self$results$insert(999, ok_notice)
        },
        
        .initializeTables = function() {
            
            # Initialize descriptives table
            if (self$options$show_descriptives) {
                desc_table <- self$results$descriptives
                desc_table$getColumn('variable')$setTitle(.('Variable'))
                desc_table$getColumn('group')$setTitle(.('Group'))
                desc_table$getColumn('n')$setTitle(.('N'))
                desc_table$getColumn('median')$setTitle(.('Median'))
            }
            
            # Initialize test results table
            if (self$options$show_test_statistics) {
                test_table <- self$results$tests
                test_table$getColumn('variable')$setTitle(.('Variable'))
                test_table$getColumn('test')$setTitle(.('Test'))
                test_table$getColumn('statistic')$setTitle(.('Statistic'))
            }
            
            # Initialize effect sizes table
            if (self$options$effect_size && self$options$show_effect_sizes) {
                effect_table <- self$results$effectsizes
                effect_table$getColumn('variable')$setTitle(.('Variable'))
                effect_table$getColumn('measure')$setTitle(.('Effect Size Measure'))
                effect_table$getColumn('value')$setTitle(.('Value'))
            }
            
            # Initialize post-hoc table
            if (self$options$post_hoc && self$options$show_post_hoc_table) {
                posthoc_table <- self$results$posthoc
                posthoc_table$getColumn('variable')$setTitle(.('Variable'))
                posthoc_table$getColumn('comparison')$setTitle(.('Comparison'))
            }
        },
        
        .cleanData = function(dep_var) {
            
            # Get data
            data <- self$data
            if (is.null(data)) return(NULL)
            
            # Enhanced validation and misuse detection
            private$.validateTestConfiguration()
            
            # Extract variables and convert to numeric
            outcome <- jmvcore::toNumeric(data[[dep_var]])
            groups <- data[[self$options$groups]]
            
            if (is.null(outcome) || is.null(groups)) {
                return(NULL)
            }
            
            # Extract paired/blocking variables BEFORE filtering so they align
            paired_raw <- NULL
            blocking_raw <- NULL

            if (!is.null(self$options$paired_variable) && nzchar(self$options$paired_variable)) {
                paired_raw <- data[[self$options$paired_variable]]
            }
            if (!is.null(self$options$blocking_variable) && nzchar(self$options$blocking_variable)) {
                blocking_raw <- data[[self$options$blocking_variable]]
            }

            # Handle missing data based on option
            missing_method <- self$options$missing_data_handling %||% "listwise"

            if (missing_method == "listwise") {
                # Include paired/blocking in complete-case filter
                if (!is.null(paired_raw)) {
                    complete_cases <- complete.cases(outcome, groups, paired_raw)
                } else if (!is.null(blocking_raw)) {
                    complete_cases <- complete.cases(outcome, groups, blocking_raw)
                } else {
                    complete_cases <- complete.cases(outcome, groups)
                }
                outcome <- outcome[complete_cases]
                groups <- groups[complete_cases]
                if (!is.null(paired_raw)) paired_raw <- paired_raw[complete_cases]
                if (!is.null(blocking_raw)) blocking_raw <- blocking_raw[complete_cases]
            } else if (missing_method == "pairwise") {
                valid_indices <- !is.na(outcome) & !is.na(groups)
                if (!is.null(paired_raw)) valid_indices <- valid_indices & !is.na(paired_raw)
                if (!is.null(blocking_raw)) valid_indices <- valid_indices & !is.na(blocking_raw)
                outcome <- outcome[valid_indices]
                groups <- groups[valid_indices]
                if (!is.null(paired_raw)) paired_raw <- paired_raw[valid_indices]
                if (!is.null(blocking_raw)) blocking_raw <- blocking_raw[valid_indices]
            } else if (missing_method == "median_imputation") {
                outcome[is.na(outcome)] <- median(outcome, na.rm = TRUE)
                valid_groups <- !is.na(groups)
                if (!is.null(paired_raw)) valid_groups <- valid_groups & !is.na(paired_raw)
                if (!is.null(blocking_raw)) valid_groups <- valid_groups & !is.na(blocking_raw)
                outcome <- outcome[valid_groups]
                groups <- groups[valid_groups]
                if (!is.null(paired_raw)) paired_raw <- paired_raw[valid_groups]
                if (!is.null(blocking_raw)) blocking_raw <- blocking_raw[valid_groups]
            } else if (missing_method == "mean_imputation") {
                outcome[is.na(outcome)] <- mean(outcome, na.rm = TRUE)
                valid_groups <- !is.na(groups)
                if (!is.null(paired_raw)) valid_groups <- valid_groups & !is.na(paired_raw)
                if (!is.null(blocking_raw)) valid_groups <- valid_groups & !is.na(blocking_raw)
                outcome <- outcome[valid_groups]
                groups <- groups[valid_groups]
                if (!is.null(paired_raw)) paired_raw <- paired_raw[valid_groups]
                if (!is.null(blocking_raw)) blocking_raw <- blocking_raw[valid_groups]
            }

            # Convert to factors after filtering
            paired_var <- if (!is.null(paired_raw)) factor(paired_raw) else NULL
            blocking_var <- if (!is.null(blocking_raw)) factor(blocking_raw) else NULL

            # Check for minimum sample size
            min_n <- self$options$minimum_sample_size %||% 5
            if (length(outcome) < min_n) {
                notice <- jmvcore::Notice$new(
                    options = self$options, name = 'insufficientData',
                    type = jmvcore::NoticeType$ERROR)
                notice$setContent(
                    sprintf('Insufficient data: only %d complete observations available (minimum %d required).', length(outcome), min_n))
                self$results$insert(1, notice)
                return(NULL)
            }

            # Convert groups to factor (drop unused levels from filtering)
            groups <- factor(groups)

            # Check for minimum group sizes
            group_counts <- table(groups)
            if (any(group_counts < 2)) {
                small_groups <- names(group_counts[group_counts < 2])
                notice <- jmvcore::Notice$new(
                    options = self$options, name = 'smallGroups',
                    type = jmvcore::NoticeType$STRONG_WARNING)
                notice$setContent(
                    sprintf('Groups with fewer than 2 observations: %s. Results may be unreliable.',
                            paste(htmltools::htmlEscape(small_groups), collapse = ', ')))
                self$results$insert(1, notice)
            }
            
            # Outlier detection and handling
            if (!is.null(self$options$outlier_method) && self$options$outlier_method != "none") {
                outliers <- private$.detectOutliers(outcome, self$options$outlier_method)
                # Just mark for now, don't remove
            }
            
            return(list(
                variable = dep_var,
                outcome = outcome,
                groups = groups,
                paired = paired_var,
                blocking = blocking_var,
                n_total = length(outcome),
                n_groups = nlevels(groups),
                group_levels = levels(groups),
                complete_cases = length(outcome)
            ))
        },
        
        .populateDescriptives = function(data, dep_var) {
            
            desc_table <- self$results$descriptives
            
            outcome <- data$outcome
            groups <- data$groups
            
            # Calculate descriptive statistics for each group
            for (group in levels(groups)) {
                private$.checkpoint(flush = FALSE)
                group_data <- outcome[groups == group]
                
                if (length(group_data) > 0) {
                    
                    # Calculate descriptives
                    n <- length(group_data)
                    missing <- sum(is.na(group_data))
                    median_val <- median(group_data, na.rm = TRUE)
                    q1 <- quantile(group_data, 0.25, na.rm = TRUE)
                    q3 <- quantile(group_data, 0.75, na.rm = TRUE)
                    iqr <- IQR(group_data, na.rm = TRUE)
                    mad_val <- mad(group_data, na.rm = TRUE)
                    mean_val <- mean(group_data, na.rm = TRUE)
                    sd_val <- sd(group_data, na.rm = TRUE)
                    min_val <- min(group_data, na.rm = TRUE)
                    max_val <- max(group_data, na.rm = TRUE)
                    ties_method <- self$options$ties_method %||% "average"
                    mean_rank <- mean(rank(outcome, ties.method = ties_method)[groups == group])
                    
                    # Count outliers
                    outliers <- private$.countOutliers(group_data)
                    
                    desc_table$addRow(rowKey = paste(dep_var, group, sep = "_"), values = list(
                        variable = dep_var,
                        group = group,
                        n = n,
                        missing = missing,
                        median = median_val,
                        q1 = q1,
                        q3 = q3,
                        iqr = iqr,
                        mad = mad_val,
                        mean = mean_val,
                        sd = sd_val,
                        min = min_val,
                        max = max_val,
                        mean_rank = mean_rank,
                        outliers = outliers
                    ))
                }
            }
        },
        
        .populateNormality = function(data, dep_var) {
            
            norm_table <- self$results$normality
            
            outcome <- data$outcome
            groups <- data$groups
            
            # Test normality for each group
            for (group in levels(groups)) {
                private$.checkpoint(flush = FALSE)
                group_data <- outcome[groups == group]
                
                if (length(group_data) >= 3) {
                    
                    # Choose test based on sample size
                    if (length(group_data) < 50) {
                        # Shapiro-Wilk test
                        test_result <- shapiro.test(group_data)
                        test_name <- .("Shapiro-Wilk")
                        statistic <- test_result$statistic
                        p_value <- test_result$p.value
                    } else {
                        # Anderson-Darling test (if available) or Kolmogorov-Smirnov
                        if (requireNamespace("nortest", quietly = TRUE)) {
                            test_result <- nortest::ad.test(group_data)
                            test_name <- .("Anderson-Darling")
                            statistic <- test_result$statistic
                            p_value <- test_result$p.value
                        } else {
                            test_result <- ks.test(group_data, "pnorm", mean(group_data), sd(group_data))
                            test_name <- .("Kolmogorov-Smirnov")
                            statistic <- test_result$statistic
                            p_value <- test_result$p.value
                        }
                    }
                    
                    # Interpret results
                    alpha <- self$options$alpha_level %||% 0.05
                    if (p_value < alpha) {
                        conclusion <- .("Non-normal")
                        recommendation <- .("Use non-parametric methods")
                    } else {
                        conclusion <- .("Normal")
                        recommendation <- .("Parametric methods acceptable")
                    }
                    
                    # df: Shapiro-Wilk uses n; K-S and A-D don't have traditional df
                    df_val <- if (grepl("Shapiro", test_name)) length(group_data) else NA

                    norm_table$addRow(rowKey = paste(dep_var, group, sep = "_"), values = list(
                        variable = dep_var,
                        group = group,
                        test = test_name,
                        statistic = statistic,
                        df = df_val,
                        p = p_value,
                        conclusion = conclusion,
                        recommendation = recommendation
                    ))
                }
            }
        },
        
        .populateAssumptions = function(data, dep_var) {
            
            assump_table <- self$results$assumptions
            
            outcome <- data$outcome
            groups <- data$groups
            
            # Test independence (simplified — not testable statistically)
            assump_table$addRow(rowKey = paste(dep_var, "independence", sep = "_"), values = list(
                variable = dep_var,
                assumption = .("Independence"),
                test = NA,
                statistic = NA,
                df = NA,
                p = NA,
                result = NA,
                assessment = .("Cannot be tested statistically"),
                implication = .("Verify study design ensures independent observations"),
                recommendation = .("Ensure random sampling and no clustering")
            ))
            
            # Test homogeneity of variance
            if (data$n_groups > 1) {
                homog_test <- self$options$homogeneity_test %||% "levene"
                
                stat_val <- NA
                df_val <- NA
                if (homog_test == "levene" && requireNamespace("car", quietly = TRUE)) {
                    test_result <- car::leveneTest(outcome ~ groups)
                    test_name <- "Levene's Test"
                    stat_val <- test_result$`F value`[1]
                    df_val <- test_result$Df[1]
                    p_value <- test_result$`Pr(>F)`[1]
                } else if (homog_test == "fligner") {
                    test_result <- fligner.test(outcome ~ groups)
                    test_name <- "Fligner-Killeen Test"
                    stat_val <- test_result$statistic
                    df_val <- test_result$parameter
                    p_value <- test_result$p.value
                } else {
                    test_result <- bartlett.test(outcome ~ groups)
                    test_name <- "Bartlett's Test"
                    stat_val <- test_result$statistic
                    df_val <- test_result$parameter
                    p_value <- test_result$p.value
                }

                alpha <- self$options$alpha_level %||% 0.05
                if (p_value < alpha) {
                    assessment <- "Unequal variances detected"
                    result_text <- "Significant"
                    recommendation <- "Consider robust methods or Welch's test"
                } else {
                    assessment <- "Equal variances assumption met"
                    result_text <- "Not significant"
                    recommendation <- "Standard methods appropriate"
                }

                assump_table$addRow(rowKey = paste(dep_var, "homogeneity", sep = "_"), values = list(
                    variable = dep_var,
                    assumption = "Homogeneity of Variance",
                    test = test_name,
                    statistic = stat_val,
                    df = df_val,
                    p = p_value,
                    result = result_text,
                    assessment = assessment,
                    implication = paste(test_name, "p =", round(p_value, 4)),
                    recommendation = recommendation
                ))
            }
            
            # Sample size adequacy
            min_n <- self$options$minimum_sample_size %||% 5
            group_counts <- table(groups)
            if (any(group_counts < min_n)) {
                assessment <- paste("Some groups have < ", min_n, " observations")
                recommendation <- "Consider combining groups or collecting more data"
            } else {
                assessment <- "Adequate sample sizes"
                recommendation <- "Sample sizes sufficient for analysis"
            }
            
            assump_table$addRow(rowKey = paste(dep_var, "sample_size", sep = "_"), values = list(
                variable = dep_var,
                assumption = "Sample Size Adequacy",
                test = NA,
                statistic = NA,
                df = NA,
                p = NA,
                result = NA,
                assessment = assessment,
                implication = sprintf(.("Smallest group: n=%d"), min(group_counts)),
                recommendation = recommendation
            ))
        },
        
        .performMainTests = function(data, dep_var) {
            
            test_table <- self$results$tests
            
            outcome <- data$outcome
            groups <- data$groups
            test_type <- self$options$test_type
            
            # Perform the selected test
            test_result <- private$.runNonparametricTest(outcome, groups, test_type, data)
            
            if (!is.null(test_result)) {
                
                # Calculate effect size
                effect_size_result <- private$.calculateMainEffectSize(outcome, groups, test_type)
                
                # Create interpretation
                alpha <- self$options$alpha_level %||% 0.05
                interpretation <- private$.interpretTestResult(test_result$p_value, alpha, test_type)
                
                # Apply global test count correction
                reported_p <- test_result$p_value
                globalN <- self$options$globalTestCount
                if (!is.null(globalN) && !is.na(globalN) && globalN > 1) {
                    reported_p <- min(reported_p * globalN, 1)
                }

                # Compute standardized statistic (Z-score) if requested
                std_stat <- NA
                if (isTRUE(self$options$report_standardized_statistics) && !is.na(reported_p)) {
                    # Convert p-value to Z-score (two-sided)
                    std_stat <- tryCatch({
                        sign_dir <- ifelse(!is.na(test_result$statistic) && test_result$statistic < 0, -1, 1)
                        sign_dir * abs(qnorm(reported_p / 2))
                    }, error = function(e) NA)
                }

                test_table$addRow(rowKey = paste(dep_var, test_type, sep = "_"), values = list(
                    variable = dep_var,
                    test = test_result$test_name,
                    statistic = test_result$statistic,
                    df = test_result$df,
                    p = reported_p,
                    effect_size = effect_size_result$value,
                    effect_measure = effect_size_result$measure,
                    effect_ci_lower = effect_size_result$ci_lower,
                    effect_ci_upper = effect_size_result$ci_upper,
                    interpretation = interpretation
                ))

                # Add standardized statistic as a note if computed
                if (!is.na(std_stat) && isTRUE(self$options$report_standardized_statistics)) {
                    test_table$setNote("std_stat",
                        sprintf(.("Standardized test statistic (Z): %.3f"), std_stat))
                }
            }
        },
        
        .runNonparametricTest = function(outcome, groups, test_type, data) {

            # Force exact tests for small samples if option is set
            use_exact <- self$options$exact_test ||
                        (self$options$small_sample_exact && data$n_total < 20)
            # Also honour exact_p_values option
            if (isTRUE(self$options$exact_p_values) && data$n_total <= 50) {
                use_exact <- TRUE
            }

            continuity_corr <- self$options$continuity_correction %||% TRUE

            result <- tryCatch({
                switch(test_type,
                    "mann_whitney" = {
                        if (nlevels(groups) != 2) {
                            jmvcore::reject("Mann-Whitney U test requires exactly 2 groups")
                        }
                        test_result <- wilcox.test(outcome ~ groups,
                                                 exact = use_exact,
                                                 correct = continuity_corr)
                        list(
                            test_name = "Mann-Whitney U Test",
                            statistic = test_result$statistic,
                            df = NA,
                            p_value = test_result$p.value
                        )
                    },

                    "kruskal_wallis" = {
                        test_result <- kruskal.test(outcome ~ groups)
                        list(
                            test_name = "Kruskal-Wallis Test",
                            statistic = test_result$statistic,
                            df = test_result$parameter,
                            p_value = test_result$p.value
                        )
                    },

                    "wilcoxon_signed" = {
                        if (nlevels(groups) == 2 && !is.null(data$paired)) {
                            # Proper paired test: join on paired/subject variable
                            group_levels <- levels(groups)
                            pair_ids <- as.character(data$paired)
                            df_long <- data.frame(
                                id = pair_ids, outcome = outcome,
                                group = as.character(groups),
                                stringsAsFactors = FALSE)
                            g1_df <- df_long[df_long$group == group_levels[1], c("id", "outcome")]
                            g2_df <- df_long[df_long$group == group_levels[2], c("id", "outcome")]

                            # Check for duplicate IDs within a group (would break pairing)
                            if (anyDuplicated(g1_df$id) || anyDuplicated(g2_df$id)) {
                                notice <- jmvcore::Notice$new(
                                    options = self$options, name = 'duplicatePairIds',
                                    type = jmvcore::NoticeType$STRONG_WARNING)
                                notice$setContent(
                                    .('Duplicate subject IDs found within a group. Each subject should appear exactly once per group for a paired test. Results may be incorrect.'))
                                self$results$insert(1, notice)
                                # Deduplicate: keep first occurrence
                                g1_df <- g1_df[!duplicated(g1_df$id), ]
                                g2_df <- g2_df[!duplicated(g2_df$id), ]
                            }

                            merged <- merge(g1_df, g2_df, by = "id", suffixes = c("_1", "_2"))
                            n_unmatched <- nrow(g1_df) + nrow(g2_df) - 2 * nrow(merged)
                            if (n_unmatched > 0) {
                                self$results$tests$setNote("unmatched_pairs",
                                    sprintf(.("%d observations could not be matched and were excluded."), n_unmatched))
                            }
                            if (nrow(merged) < 2) {
                                jmvcore::reject(.("Fewer than 2 matched pairs found. Check your paired/subject variable."))
                            }
                            test_result <- wilcox.test(merged$outcome_1, merged$outcome_2,
                                                     paired = TRUE,
                                                     exact = use_exact,
                                                     correct = continuity_corr)
                        } else if (nlevels(groups) == 2) {
                            # No pairing variable provided — warn and use positional fallback
                            notice <- jmvcore::Notice$new(
                                options = self$options, name = 'noPairingVar',
                                type = jmvcore::NoticeType$WARNING)
                            notice$setContent(
                                .('No paired/subject variable specified. Using positional pairing (row order). Add a subject ID variable for correct paired analysis.'))
                            self$results$insert(1, notice)

                            group_levels <- levels(groups)
                            g1 <- outcome[groups == group_levels[1]]
                            g2 <- outcome[groups == group_levels[2]]
                            n_pairs <- min(length(g1), length(g2))
                            test_result <- wilcox.test(g1[seq_len(n_pairs)],
                                                     g2[seq_len(n_pairs)],
                                                     paired = TRUE,
                                                     exact = use_exact,
                                                     correct = continuity_corr)
                        } else {
                            # One-sample signed-rank test against median = 0
                            test_result <- wilcox.test(outcome,
                                                     exact = use_exact,
                                                     correct = continuity_corr)
                        }
                        list(
                            test_name = "Wilcoxon Signed-Rank Test",
                            statistic = test_result$statistic,
                            df = NA,
                            p_value = test_result$p.value
                        )
                    },

                    "friedman" = {
                        if (is.null(data$blocking)) {
                            jmvcore::reject("Blocking variable required for Friedman test")
                        }
                        test_result <- friedman.test(outcome, groups, data$blocking)
                        list(
                            test_name = "Friedman Test",
                            statistic = test_result$statistic,
                            df = test_result$parameter,
                            p_value = test_result$p.value
                        )
                    },

                    "median_test" = {
                        private$.medianTest(outcome, groups)
                    },

                    "van_der_waerden" = {
                        private$.vanDerWaerdenTest(outcome, groups)
                    },

                    "mood_median" = {
                        private$.moodMedianTest(outcome, groups)
                    },

                    "cochran_q" = {
                        private$.cochranQTest(outcome, groups, data)
                    },

                    "page_trend" = {
                        private$.pageTrendTest(outcome, groups, data)
                    },

                    "mcnemar" = {
                        private$.mcnemarTest(outcome, groups, data)
                    },

                    "sign_test" = {
                        private$.signTest(outcome, groups, data)
                    },

                    "jonckheere_terpstra" = {
                        private$.jonckheereTerpstraTest(outcome, groups)
                    },

                    {
                        stop(paste("Unknown test type:", test_type))
                    }
                )
            }, error = function(e) {
                notice <- jmvcore::Notice$new(
                    options = self$options, name = 'testError',
                    type = jmvcore::NoticeType$ERROR)
                notice$setContent(
                    sprintf('Test error: %s', htmltools::htmlEscape(conditionMessage(e))))
                self$results$insert(1, notice)
                return(NULL)
            })
            return(result)
        },
        
        .calculateMainEffectSize = function(outcome, groups, test_type) {

            effect_method <- self$options$effect_size_method %||% "eta_squared"

            tryCatch({
                switch(effect_method,
                    "eta_squared" = private$.calculateEtaSquared(outcome, groups),
                    "epsilon_squared" = private$.calculateEpsilonSquared(outcome, groups),
                    "rank_biserial" = private$.calculateRankBiserial(outcome, groups),
                    "cliff_delta" = private$.calculateCliffDelta(outcome, groups),
                    "cles" = private$.calculateCLES(outcome, groups),
                    "vargha_delaney" = private$.calculateVarghaDelaney(outcome, groups),
                    "kendall_w" = private$.calculateKendallW(outcome, groups),
                    "glass_rank_biserial" = private$.calculateGlassRankBiserial(outcome, groups),
                    "somers_d" = private$.calculateSomersD(outcome, groups),
                    {
                        list(value = NA, measure = effect_method, ci_lower = NA, ci_upper = NA)
                    }
                )
            }, error = function(e) {
                list(value = NA, measure = effect_method, ci_lower = NA, ci_upper = NA)
            })
        },
        
        .calculateEffectSizes = function(data, dep_var) {
            
            if (!self$options$show_effect_sizes) return()
            
            effect_table <- self$results$effectsizes
            outcome <- data$outcome
            groups <- data$groups
            
            # Calculate all requested effect sizes
            effect_methods <- c("eta_squared", "cliff_delta", "rank_biserial", "cles")
            
            for (method in effect_methods) {
                private$.checkpoint(flush = FALSE)
                effect_result <- switch(method,
                    "eta_squared" = private$.calculateEtaSquared(outcome, groups),
                    "cliff_delta" = private$.calculateCliffDelta(outcome, groups),
                    "rank_biserial" = private$.calculateRankBiserial(outcome, groups),
                    "cles" = private$.calculateCLES(outcome, groups)
                )
                
                if (!is.null(effect_result)) {
                    
                    # Interpret magnitude
                    magnitude <- private$.interpretEffectSizeMagnitude(effect_result$value, method)
                    interpretation <- private$.interpretEffectSize(effect_result$value, method)
                    
                    effect_table$addRow(rowKey = paste(dep_var, method, sep = "_"), values = list(
                        variable = dep_var,
                        measure = effect_result$measure,
                        value = effect_result$value,
                        ci_lower = effect_result$ci_lower,
                        ci_upper = effect_result$ci_upper,
                        magnitude = magnitude,
                        interpretation = interpretation
                    ))
                }
            }
        },
        
        .performPostHocTests = function(data, dep_var) {
            
            if (!self$options$show_post_hoc_table || data$n_groups < 3) return()
            
            posthoc_table <- self$results$posthoc
            outcome <- data$outcome
            groups <- data$groups
            
            posthoc_method <- self$options$post_hoc_method %||% "dunn"
            p_adjust <- self$options$p_adjustment %||% "holm"
            
            # Perform post-hoc test
            posthoc_results <- private$.runPostHocTest(outcome, groups, posthoc_method, p_adjust)
            
            if (!is.null(posthoc_results)) {
                for (i in 1:nrow(posthoc_results)) {
                    private$.checkpoint(flush = FALSE)
                    
                    comparison <- posthoc_results$comparison[i]
                    statistic <- posthoc_results$statistic[i]
                    p_raw <- posthoc_results$p_raw[i]
                    p_adjusted <- posthoc_results$p_adjusted[i]
                    effect_size <- posthoc_results$effect_size[i]
                    
                    # Determine significance
                    alpha <- self$options$alpha_level %||% 0.05
                    significance <- ifelse(p_adjusted < alpha, "Significant", "Non-significant")
                    interpretation <- private$.interpretPostHocResult(p_adjusted, alpha)
                    
                    posthoc_table$addRow(rowKey = paste(dep_var, i, sep = "_"), values = list(
                        variable = dep_var,
                        comparison = comparison,
                        statistic = statistic,
                        p_raw = p_raw,
                        p_adjusted = p_adjusted,
                        effect_size = effect_size,
                        effect_ci_lower = NA,
                        effect_ci_upper = NA,
                        significance = significance,
                        interpretation = interpretation
                    ))
                }
            }
        },
        
        .calculateRobustStatistics = function(data, dep_var) {
            
            robust_table <- self$results$robustStatistics
            outcome <- data$outcome
            groups <- data$groups
            
            robust_method <- self$options$robust_method %||% "standard"
            trim_prop <- self$options$trim_proportion %||% 0.1
            wins_prop <- self$options$winsorize_proportion %||% 0.1
            
            # Calculate robust statistics for each group
            for (group in levels(groups)) {
                private$.checkpoint(flush = FALSE)
                group_data <- outcome[groups == group]
                
                if (length(group_data) > 0) {
                    
                    # Calculate different robust estimates
                    trimmed_mean <- mean(group_data, trim = trim_prop, na.rm = TRUE)
                    winsorized_mean <- private$.winsorizedMean(group_data, wins_prop)
                    hodges_lehmann <- private$.hodgesLehmannEstimate(group_data)
                    robust_se <- private$.robustStandardError(group_data, robust_method)
                    
                    # Confidence intervals
                    conf_level <- self$options$confidence_level %||% 0.95
                    ci_result <- private$.robustConfidenceInterval(group_data, robust_method, conf_level)
                    
                    robust_table$addRow(rowKey = paste(dep_var, group, sep = "_"), values = list(
                        variable = dep_var,
                        group = group,
                        trimmed_mean = trimmed_mean,
                        winsorized_mean = winsorized_mean,
                        hodges_lehmann = hodges_lehmann,
                        robust_se = robust_se,
                        robust_ci_lower = ci_result$lower,
                        robust_ci_upper = ci_result$upper
                    ))
                }
            }
        },
        
        .performPowerAnalysis = function(data, dep_var) {
            
            power_table <- self$results$powerAnalysis
            outcome <- data$outcome
            groups <- data$groups
            
            # Calculate observed effect size
            effect_result <- private$.calculateMainEffectSize(outcome, groups, self$options$test_type)
            
            if (!is.null(effect_result) && !is.na(effect_result$value)) {
                
                # Estimate achieved power
                power_result <- private$.calculatePower(data$n_total, effect_result$value, 
                                                      self$options$alpha_level %||% 0.05)
                
                # Calculate required sample size for 80% power
                required_n <- private$.calculateRequiredN(effect_result$value, 0.8, 
                                                        self$options$alpha_level %||% 0.05)
                
                power_interpretation <- private$.interpretPower(power_result$power)
                
                power_table$addRow(rowKey = dep_var, values = list(
                    variable = dep_var,
                    comparison = "Overall",
                    observed_effect = effect_result$value,
                    power = power_result$power,
                    required_n = required_n,
                    power_interpretation = power_interpretation
                ))
            }
        },
        
        .generatePlots = function() {
            
            # Plot generation would be implemented here
            # Each plot function would check its visibility option and generate accordingly
            
            if (self$options$descriptive_plots || self$options$show_boxplots) {
                private$.generateBoxPlots()
            }
            
            if (self$options$descriptive_plots || self$options$show_violin_plots) {
                private$.generateViolinPlots()
            }
            
            if (self$options$show_rank_plots) {
                private$.generateRankPlots()
            }
            
            if (self$options$show_effect_plots && self$options$effect_size) {
                private$.generateEffectSizePlots()
            }
            
            if (self$options$show_qqplots || (self$options$normality_tests && self$options$descriptive_plots)) {
                private$.generateQQPlots()
            }
            
            if (self$options$descriptive_plots) {
                private$.generateDistributionPlots()
            }
        },
        
        
        .populateInstructions = function() {
            
            instructions_text <- paste(
                "<h3>Enhanced Non-Parametric Statistical Methods</h3>",
                "<p><strong>Getting Started:</strong></p>",
                "<ol>",
                "<li><strong>Select Variables:</strong> Choose your dependent variable(s) and grouping variable</li>",
                "<li><strong>Choose Test:</strong> Select appropriate non-parametric test based on your design</li>",
                "<li><strong>Configure Options:</strong> Adjust effect sizes, post-hoc methods, and assumptions testing</li>",
                "<li><strong>Review Results:</strong> Examine test statistics, effect sizes, and interpretations</li>",
                "</ol>",
                "<p><strong>Test Selection Guide:</strong></p>",
                "<ul>",
                "<li><strong>Mann-Whitney U:</strong> Compare 2 independent groups</li>",
                "<li><strong>Kruskal-Wallis:</strong> Compare 3+ independent groups</li>",
                "<li><strong>Wilcoxon Signed-Rank:</strong> Compare paired samples</li>",
                "<li><strong>Friedman:</strong> Compare repeated measures (3+ time points)</li>",
                "<li><strong>McNemar:</strong> Compare paired categorical data</li>",
                "<li><strong>Cochran's Q:</strong> Compare binary outcomes across 3+ matched groups</li>",
                "</ul>",
                "<p><strong>Effect Sizes Available:</strong></p>",
                "<ul>",
                "<li><strong>Cliff's Delta:</strong> Recommended for Mann-Whitney U tests</li>",
                "<li><strong>Eta-squared:</strong> For Kruskal-Wallis tests</li>",
                "<li><strong>Rank-biserial:</strong> For Wilcoxon tests</li>",
                "<li><strong>Common Language Effect Size:</strong> Probability-based interpretation</li>",
                "</ul>",
                sep = ""
            )
            
            self$results$instructions$setContent(instructions_text)
        },
        
        # Helper methods for specific tests and calculations
        .calculateCliffDelta = function(outcome, groups) {
            
            if (nlevels(groups) != 2) return(NULL)
            
            group_levels <- levels(groups)
            group1 <- outcome[groups == group_levels[1]]
            group2 <- outcome[groups == group_levels[2]]
            
            # Calculate Cliff's Delta
            n1 <- length(group1)
            n2 <- length(group2)
            
            # Count comparisons
            greater <- sum(outer(group1, group2, ">"))
            less <- sum(outer(group1, group2, "<"))
            
            cliff_delta <- (greater - less) / (n1 * n2)
            
            # Bootstrap confidence interval if requested
            ci_lower <- NA
            ci_upper <- NA
            
            if (self$options$confidence_intervals || self$options$bootstrap_ci) {
                private$.checkpoint(flush = FALSE)
                ci_result <- private$.bootstrapCliffDelta(group1, group2,
                                                        self$options$confidence_level %||% 0.95)
                ci_lower <- ci_result$lower
                ci_upper <- ci_result$upper
            }
            
            list(
                value = cliff_delta,
                measure = "Cliff's Delta",
                ci_lower = ci_lower,
                ci_upper = ci_upper
            )
        },
        
        .calculateEtaSquared = function(outcome, groups) {
            
            # Calculate eta-squared for Kruskal-Wallis
            n <- length(outcome)
            k <- nlevels(groups)
            
            # Calculate H statistic
            ranks <- rank(outcome)
            group_ranks <- tapply(ranks, groups, sum)
            group_sizes <- table(groups)
            
            H <- (12 / (n * (n + 1))) * sum(group_ranks^2 / group_sizes) - 3 * (n + 1)
            
            # Eta-squared
            eta_sq <- (H - k + 1) / (n - k)
            
            list(
                value = eta_sq,
                measure = "Eta-squared",
                ci_lower = NA,
                ci_upper = NA
            )
        },
        
        .hodgesLehmannEstimate = function(data) {
            
            # Calculate Hodges-Lehmann estimator - vectorized for performance
            n <- length(data)
            if (n < 2) return(NA)
            
            # Use outer() for vectorized calculation - much faster than nested loops
            pairwise_sums <- outer(data, data, "+") / 2
            
            # Extract upper triangle including diagonal (each pair counted once)
            pairwise_avgs <- pairwise_sums[upper.tri(pairwise_sums, diag = TRUE)]
            
            median(pairwise_avgs, na.rm = TRUE)
        },
        
        .winsorizedMean = function(data, prop = 0.1) {
            
            if (length(data) < 2) return(mean(data, na.rm = TRUE))
            
            # Winsorize data
            sorted_data <- sort(data)
            n <- length(sorted_data)
            k <- floor(n * prop)
            
            if (k > 0) {
                # Replace extreme values
                sorted_data[1:k] <- sorted_data[k + 1]
                sorted_data[(n - k + 1):n] <- sorted_data[n - k]
            }
            
            mean(sorted_data, na.rm = TRUE)
        },
        
        .detectOutliers = function(data, method) {

            outliers <- c()

            switch(method,
                "iqr" = {
                    Q1 <- quantile(data, 0.25, na.rm = TRUE)
                    Q3 <- quantile(data, 0.75, na.rm = TRUE)
                    IQR <- Q3 - Q1
                    outliers <- which(data < (Q1 - 1.5 * IQR) | data > (Q3 + 1.5 * IQR))
                },
                "modified_zscore" = {
                    median_val <- median(data, na.rm = TRUE)
                    mad_val <- mad(data, na.rm = TRUE)
                    if (!is.na(mad_val) && mad_val > 0) {
                        modified_z <- 0.6745 * (data - median_val) / mad_val
                        outliers <- which(abs(modified_z) > 3.5)
                    }
                },
                "tukey" = {
                    Q1 <- quantile(data, 0.25, na.rm = TRUE)
                    Q3 <- quantile(data, 0.75, na.rm = TRUE)
                    IQR <- Q3 - Q1
                    outliers <- which(data < (Q1 - 3 * IQR) | data > (Q3 + 3 * IQR))
                },
                "hampel" = {
                    # Hampel filter: based on MAD, flags points > 3*MAD from median
                    median_val <- median(data, na.rm = TRUE)
                    mad_val <- mad(data, constant = 1.4826, na.rm = TRUE)
                    if (!is.na(mad_val) && mad_val > 0) {
                        outliers <- which(abs(data - median_val) > 3 * mad_val)
                    }
                }
            )

            return(outliers)
        },
        
        .countOutliers = function(data) {
            
            outlier_method <- self$options$outlier_method %||% "iqr"
            if (outlier_method == "none") return(0)
            
            outliers <- private$.detectOutliers(data, outlier_method)
            length(outliers)
        },
        
        # ── Implemented test methods ──────────────────────────────────────

        .medianTest = function(outcome, groups) {
            # Median test: chi-square test on counts above/below grand median
            grand_median <- median(outcome, na.rm = TRUE)
            above <- outcome > grand_median
            tab <- table(groups, above)
            if (ncol(tab) < 2) {
                return(list(test_name = "Median Test", statistic = NA, df = NA, p_value = NA))
            }
            res <- suppressWarnings(chisq.test(tab))
            list(
                test_name = "Median Test",
                statistic = res$statistic,
                df = res$parameter,
                p_value = res$p.value
            )
        },

        .vanDerWaerdenTest = function(outcome, groups) {
            # Van der Waerden (normal scores) test
            # Reference: Conover (1999), Practical Nonparametric Statistics, 3rd ed.
            # Statistic: A = sum(n_i * mean_score_i^2) / S^2
            # where scores = qnorm(R_i / (n+1)), S^2 = sum(scores^2) / (n-1)
            # A ~ chi-squared with k-1 df under H0
            n <- length(outcome)
            ranks <- rank(outcome)
            scores <- qnorm(ranks / (n + 1))
            k <- nlevels(groups)
            ns <- table(groups)
            # E[score] = 0 under H0, so grand mean reference is 0
            group_means <- tapply(scores, groups, mean)
            ss_between <- sum(ns * group_means^2)
            s_squared <- sum(scores^2) / (n - 1)
            if (s_squared == 0) {
                return(list(test_name = "Van der Waerden Test", statistic = NA, df = NA, p_value = NA))
            }
            stat <- ss_between / s_squared
            df_val <- k - 1
            p_val <- pchisq(stat, df = df_val, lower.tail = FALSE)
            list(
                test_name = "Van der Waerden (Normal Scores) Test",
                statistic = stat,
                df = df_val,
                p_value = p_val
            )
        },

        .moodMedianTest = function(outcome, groups) {
            # Mood's median test (same as median test but with Fisher exact for small samples)
            grand_median <- median(outcome, na.rm = TRUE)
            above <- outcome > grand_median
            tab <- table(groups, above)
            if (ncol(tab) < 2) {
                return(list(test_name = "Mood's Median Test", statistic = NA, df = NA, p_value = NA))
            }
            min_expected <- min(suppressWarnings(chisq.test(tab)$expected))
            if (min_expected < 5) {
                res <- fisher.test(tab, simulate.p.value = (nrow(tab) > 2 || ncol(tab) > 2))
                list(
                    test_name = "Mood's Median Test (Fisher's exact)",
                    statistic = NA,
                    df = NA,
                    p_value = res$p.value
                )
            } else {
                res <- suppressWarnings(chisq.test(tab))
                list(
                    test_name = "Mood's Median Test",
                    statistic = res$statistic,
                    df = res$parameter,
                    p_value = res$p.value
                )
            }
        },

        .cochranQTest = function(outcome, groups, data) {
            # Cochran's Q test for binary outcomes in repeated measures
            tryCatch({
                blocking <- data$blocking %||% data$paired
                if (is.null(blocking)) {
                    return(list(test_name = "Cochran's Q Test", statistic = NA, df = NA,
                                p_value = NA))
                }
                res <- DescTools::CochranQTest(outcome ~ groups | blocking)
                list(
                    test_name = "Cochran's Q Test",
                    statistic = res$statistic,
                    df = res$parameter,
                    p_value = res$p.value
                )
            }, error = function(e) {
                list(test_name = "Cochran's Q Test", statistic = NA, df = NA,
                     p_value = NA)
            })
        },

        .pageTrendTest = function(outcome, groups, data) {
            # Page's test for ordered alternatives in repeated measures
            tryCatch({
                blocking <- data$blocking %||% data$paired
                if (is.null(blocking)) {
                    return(list(test_name = "Page's Trend Test", statistic = NA, df = NA,
                                p_value = NA))
                }
                res <- DescTools::PageTest(outcome ~ groups | blocking)
                list(
                    test_name = "Page's Trend Test",
                    statistic = res$statistic,
                    df = NA,
                    p_value = res$p.value
                )
            }, error = function(e) {
                list(test_name = "Page's Trend Test", statistic = NA, df = NA,
                     p_value = NA)
            })
        },

        .mcnemarTest = function(outcome, groups, data) {
            # McNemar's test for paired categorical data
            tryCatch({
                tab <- table(outcome, groups)
                if (nrow(tab) != 2 || ncol(tab) != 2) {
                    return(list(test_name = "McNemar Test", statistic = NA, df = NA,
                                p_value = NA))
                }
                res <- mcnemar.test(tab)
                list(
                    test_name = "McNemar Test",
                    statistic = res$statistic,
                    df = res$parameter,
                    p_value = res$p.value
                )
            }, error = function(e) {
                list(test_name = "McNemar Test", statistic = NA, df = NA,
                     p_value = NA)
            })
        },

        .signTest = function(outcome, groups, data) {
            # Sign test for paired or one-sample data
            tryCatch({
                if (nlevels(groups) == 2) {
                    # Paired sign test: test median of differences = 0
                    group_levels <- levels(groups)
                    g1 <- outcome[groups == group_levels[1]]
                    g2 <- outcome[groups == group_levels[2]]
                    n_pairs <- min(length(g1), length(g2))
                    diffs <- g1[seq_len(n_pairs)] - g2[seq_len(n_pairs)]
                    res <- DescTools::SignTest(diffs)
                } else {
                    # One-sample sign test against median = 0
                    res <- DescTools::SignTest(outcome)
                }
                list(
                    test_name = "Sign Test",
                    statistic = res$statistic,
                    df = NA,
                    p_value = res$p.value
                )
            }, error = function(e) {
                list(test_name = "Sign Test", statistic = NA, df = NA,
                     p_value = NA)
            })
        },

        .jonckheereTerpstraTest = function(outcome, groups) {
            # Jonckheere-Terpstra test for ordered alternatives
            # Tests H1: theta_1 <= theta_2 <= ... <= theta_k (at least one strict)
            tryCatch({
                # Ensure groups is an ordered factor for the trend direction
                if (!is.ordered(groups)) {
                    groups <- factor(groups, levels = levels(groups), ordered = TRUE)
                }
                res <- DescTools::JonckheereTerpstraTest(
                    outcome ~ groups,
                    alternative = "increasing"
                )
                list(
                    test_name = "Jonckheere-Terpstra Test",
                    statistic = res$statistic,
                    df = NA,
                    p_value = res$p.value
                )
            }, error = function(e) {
                list(test_name = "Jonckheere-Terpstra Test", statistic = NA, df = NA,
                     p_value = NA)
            })
        },
        
        # Placeholder methods for other calculations
        .calculateEpsilonSquared = function(outcome, groups) {
            # Epsilon-squared: alternative to eta-squared for Kruskal-Wallis
            # eps^2 = H / ((n^2 - 1) / (n + 1))
            n <- length(outcome)
            k <- nlevels(groups)
            ranks <- rank(outcome)
            group_ranks <- tapply(ranks, groups, sum)
            group_sizes <- table(groups)
            H <- (12 / (n * (n + 1))) * sum(group_ranks^2 / group_sizes) - 3 * (n + 1)
            eps_sq <- H / ((n^2 - 1) / (n + 1))
            list(value = eps_sq, measure = "Epsilon-squared", ci_lower = NA, ci_upper = NA)
        },

        .calculateRankBiserial = function(outcome, groups) {
            # Rank-biserial correlation for Mann-Whitney U (2-group)
            # r = 1 - (2U) / (n1 * n2)
            if (nlevels(groups) != 2) return(list(value = NA, measure = "Rank-biserial correlation", ci_lower = NA, ci_upper = NA))
            group_levels <- levels(groups)
            g1 <- outcome[groups == group_levels[1]]
            g2 <- outcome[groups == group_levels[2]]
            n1 <- length(g1)
            n2 <- length(g2)
            wt <- wilcox.test(g1, g2, exact = FALSE)
            U <- wt$statistic
            r_rb <- 1 - (2 * U) / (n1 * n2)
            list(value = r_rb, measure = "Rank-biserial correlation", ci_lower = NA, ci_upper = NA)
        },

        .calculateCLES = function(outcome, groups) {
            # Common Language Effect Size: P(X > Y)
            if (nlevels(groups) != 2) return(list(value = NA, measure = "Common Language Effect Size", ci_lower = NA, ci_upper = NA))
            group_levels <- levels(groups)
            g1 <- outcome[groups == group_levels[1]]
            g2 <- outcome[groups == group_levels[2]]
            n1 <- length(g1)
            n2 <- length(g2)
            greater <- sum(outer(g1, g2, ">"))
            ties <- sum(outer(g1, g2, "=="))
            cles <- (greater + 0.5 * ties) / (n1 * n2)
            list(value = cles, measure = "Common Language Effect Size", ci_lower = NA, ci_upper = NA)
        },

        .calculateVarghaDelaney = function(outcome, groups) {
            # Vargha-Delaney A: equivalent to CLES (probability of superiority)
            if (nlevels(groups) != 2) return(list(value = NA, measure = "Vargha-Delaney A", ci_lower = NA, ci_upper = NA))
            group_levels <- levels(groups)
            g1 <- outcome[groups == group_levels[1]]
            g2 <- outcome[groups == group_levels[2]]
            n1 <- length(g1)
            n2 <- length(g2)
            wt <- wilcox.test(g1, g2, exact = FALSE)
            U <- wt$statistic
            A <- U / (n1 * n2)
            list(value = A, measure = "Vargha-Delaney A", ci_lower = NA, ci_upper = NA)
        },

        .calculateKendallW = function(outcome, groups) {
            # Kendall's W for Friedman-type designs (concordance coefficient)
            # W = chi2_r / (n * (k - 1))
            # For between-subjects: approximate using Kruskal-Wallis H
            n <- length(outcome)
            k <- nlevels(groups)
            if (k < 2) return(list(value = NA, measure = "Kendall's W", ci_lower = NA, ci_upper = NA))
            ranks <- rank(outcome)
            group_ranks <- tapply(ranks, groups, sum)
            group_sizes <- table(groups)
            H <- (12 / (n * (n + 1))) * sum(group_ranks^2 / group_sizes) - 3 * (n + 1)
            # For between-subjects approximation
            W <- H / (n - 1)
            W <- max(0, min(1, W))
            list(value = W, measure = "Kendall's W", ci_lower = NA, ci_upper = NA)
        },

        .calculateGlassRankBiserial = function(outcome, groups) {
            # Glass rank-biserial: correlation between binary grouping and ranks
            if (nlevels(groups) != 2) return(list(value = NA, measure = "Glass's Rank Biserial", ci_lower = NA, ci_upper = NA))
            group_levels <- levels(groups)
            ranks <- rank(outcome)
            r1 <- mean(ranks[groups == group_levels[1]])
            r2 <- mean(ranks[groups == group_levels[2]])
            n <- length(outcome)
            r_glass <- 2 * (r1 - r2) / n
            list(value = r_glass, measure = "Glass's Rank Biserial", ci_lower = NA, ci_upper = NA)
        },

        .calculateSomersD = function(outcome, groups) {
            # Somers' D: asymmetric measure of ordinal association
            # D = (concordant - discordant) / (concordant + discordant + tied_on_y)
            n <- length(outcome)
            if (n < 2) return(list(value = NA, measure = "Somers' D", ci_lower = NA, ci_upper = NA))
            x_num <- as.numeric(groups)
            y_num <- as.numeric(outcome)
            concordant <- 0
            discordant <- 0
            tied_y <- 0
            for (i in 1:(n - 1)) {
                for (j in (i + 1):n) {
                    dx <- sign(x_num[j] - x_num[i])
                    dy <- sign(y_num[j] - y_num[i])
                    if (dx != 0) {
                        if (dy != 0) {
                            if (dx * dy > 0) concordant <- concordant + 1
                            else discordant <- discordant + 1
                        } else {
                            tied_y <- tied_y + 1
                        }
                    }
                }
            }
            denom <- concordant + discordant + tied_y
            D <- if (denom > 0) (concordant - discordant) / denom else NA
            list(value = D, measure = "Somers' D", ci_lower = NA, ci_upper = NA)
        },
        
        # Plot generation stubs (actual rendering via .r.yaml renderFun callbacks)
        .generateBoxPlots = function() {},
        .generateViolinPlots = function() {},
        .generateRankPlots = function() {},
        .generateEffectSizePlots = function() {},
        .generateQQPlots = function() {},
        .generateDistributionPlots = function() {},

        # ── Shared plot data extraction ──────────────────────────────────

        .getPlotData = function() {
            dep_vars <- self$options$deps
            if (is.null(dep_vars) || length(dep_vars) == 0) dep_vars <- self$options$outcome
            if (is.null(dep_vars) || is.null(self$options$groups)) return(NULL)

            dep_var <- dep_vars[1]
            outcome <- self$data[[dep_var]]
            groups <- self$data[[self$options$groups]]
            valid <- complete.cases(outcome, groups)
            list(
                dep_var = dep_var,
                df = data.frame(outcome = outcome[valid], groups = factor(groups[valid]))
            )
        },

        # ── Plot render functions (called by jamovi via renderFun) ──────────

        .boxplot = function(image, ggtheme, theme, ...) {
            pd <- private$.getPlotData()
            if (is.null(pd)) return(FALSE)

            plot <- ggplot2::ggplot(pd$df, ggplot2::aes(x = groups, y = outcome, fill = groups)) +
                ggplot2::geom_boxplot(alpha = 0.7, outlier.shape = NA) +
                ggplot2::geom_jitter(width = 0.15, alpha = 0.4, size = 1.5) +
                ggplot2::labs(x = self$options$groups, y = pd$dep_var,
                             title = paste(.("Box Plot:"), pd$dep_var, .("by"), self$options$groups)) +
                ggtheme +
                ggplot2::theme(legend.position = "none")
            print(plot)
            return(TRUE)
        },

        .violinplot = function(image, ggtheme, theme, ...) {
            pd <- private$.getPlotData()
            if (is.null(pd)) return(FALSE)

            plot <- ggplot2::ggplot(pd$df, ggplot2::aes(x = groups, y = outcome, fill = groups)) +
                ggplot2::geom_violin(alpha = 0.6, trim = FALSE) +
                ggplot2::geom_boxplot(width = 0.15, alpha = 0.8) +
                ggplot2::labs(x = self$options$groups, y = pd$dep_var,
                             title = paste(.("Violin Plot:"), pd$dep_var, .("by"), self$options$groups)) +
                ggtheme +
                ggplot2::theme(legend.position = "none")
            print(plot)
            return(TRUE)
        },

        .plotRankDistribution = function(image, ggtheme, theme, ...) {
            pd <- private$.getPlotData()
            if (is.null(pd)) return(FALSE)

            ranks <- rank(pd$df$outcome)
            df <- data.frame(ranks = ranks, groups = pd$df$groups)

            plot <- ggplot2::ggplot(df, ggplot2::aes(x = groups, y = ranks, fill = groups)) +
                ggplot2::geom_boxplot(alpha = 0.7) +
                ggplot2::labs(x = self$options$groups, y = .("Rank"),
                             title = paste(.("Rank Distribution:"), pd$dep_var)) +
                ggtheme +
                ggplot2::theme(legend.position = "none")
            print(plot)
            return(TRUE)
        },

        .effectsizeplot = function(image, ggtheme, theme, ...) {
            es_table <- self$results$effectsizes
            if (es_table$rowCount == 0) return(FALSE)

            measures <- character(0)
            values <- numeric(0)
            ci_lo <- numeric(0)
            ci_hi <- numeric(0)
            for (i in seq_len(es_table$rowCount)) {
                row <- es_table$getRow(i)
                measures <- c(measures, row[["measure"]])
                values <- c(values, row[["value"]])
                ci_lo <- c(ci_lo, ifelse(is.na(row[["ci_lower"]]), row[["value"]], row[["ci_lower"]]))
                ci_hi <- c(ci_hi, ifelse(is.na(row[["ci_upper"]]), row[["value"]], row[["ci_upper"]]))
            }
            df <- data.frame(measure = measures, value = values, ci_lower = ci_lo, ci_upper = ci_hi)
            df$measure <- factor(df$measure, levels = rev(df$measure))

            plot <- ggplot2::ggplot(df, ggplot2::aes(x = value, y = measure)) +
                ggplot2::geom_point(size = 3) +
                ggplot2::geom_errorbarh(ggplot2::aes(xmin = ci_lower, xmax = ci_upper), height = 0.2) +
                ggplot2::geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
                ggplot2::labs(x = .("Effect Size"), y = "",
                             title = .("Effect Size Estimates with Confidence Intervals")) +
                ggtheme
            print(plot)
            return(TRUE)
        },

        .qqplot = function(image, ggtheme, theme, ...) {
            pd <- private$.getPlotData()
            if (is.null(pd)) return(FALSE)

            plot <- ggplot2::ggplot(pd$df, ggplot2::aes(sample = outcome)) +
                ggplot2::stat_qq(alpha = 0.5) +
                ggplot2::stat_qq_line(colour = "red") +
                ggplot2::facet_wrap(~ groups) +
                ggplot2::labs(title = paste(.("Q-Q Plot:"), pd$dep_var, .("by"), self$options$groups)) +
                ggtheme
            print(plot)
            return(TRUE)
        },

        .distributionplot = function(image, ggtheme, theme, ...) {
            pd <- private$.getPlotData()
            if (is.null(pd)) return(FALSE)

            plot <- ggplot2::ggplot(pd$df, ggplot2::aes(x = outcome, fill = groups)) +
                ggplot2::geom_density(alpha = 0.5) +
                ggplot2::labs(x = pd$dep_var, y = .("Density"), fill = self$options$groups,
                             title = paste(.("Distribution Comparison:"), pd$dep_var, .("by"), self$options$groups)) +
                ggtheme
            print(plot)
            return(TRUE)
        },
        
        # ── Interpretation methods ──────────────────────────────────────

        .interpretTestResult = function(p_value, alpha, test_type) {
            if (is.na(p_value)) return(.("Test result not available"))

            context <- switch(test_type,
                "mann_whitney" = .("difference between the two groups"),
                "kruskal_wallis" = .("difference among the groups"),
                "wilcoxon_signed" = .("difference between paired observations"),
                "friedman" = .("difference across repeated measures"),
                "jonckheere_terpstra" = .("ordered trend across groups"),
                "sign_test" = .("difference between paired observations"),
                "mcnemar" = .("change in proportions between conditions"),
                "cochran_q" = .("difference across matched groups"),
                "page_trend" = .("ordered trend across repeated measures"),
                "median_test" = .("difference in medians"),
                "van_der_waerden" = .("difference in locations"),
                "mood_median" = .("difference in medians"),
                .("difference between groups"))

            if (p_value < alpha) {
                sprintf(.("Statistically significant %s (p = %s, alpha = %s)"),
                        context, format.pval(p_value, digits = 3), alpha)
            } else {
                sprintf(.("No statistically significant %s (p = %s, alpha = %s)"),
                        context, format.pval(p_value, digits = 3), alpha)
            }
        },

        .interpretEffectSizeMagnitude = function(value, method) {
            if (is.na(value)) return(.("Cannot determine"))

            abs_value <- abs(value)

            # Method-specific thresholds (Cohen 1988; Vargha & Delaney 2000)
            switch(method,
                "eta_squared" = , "epsilon_squared" = {
                    # Eta-squared: 0.01 small, 0.06 medium, 0.14 large
                    if (abs_value < 0.01) .("Negligible")
                    else if (abs_value < 0.06) .("Small")
                    else if (abs_value < 0.14) .("Medium")
                    else .("Large")
                },
                "rank_biserial" = , "glass_rank_biserial" = {
                    # r: 0.1 small, 0.3 medium, 0.5 large
                    if (abs_value < 0.1) .("Negligible")
                    else if (abs_value < 0.3) .("Small")
                    else if (abs_value < 0.5) .("Medium")
                    else .("Large")
                },
                "cliff_delta" = {
                    # Cliff's delta: 0.147 small, 0.33 medium, 0.474 large (Romano et al. 2006)
                    if (abs_value < 0.147) .("Negligible")
                    else if (abs_value < 0.33) .("Small")
                    else if (abs_value < 0.474) .("Medium")
                    else .("Large")
                },
                "cles" = , "vargha_delaney" = {
                    # CLES/VDA: 0.56 small, 0.64 medium, 0.71 large (Vargha & Delaney 2000)
                    deviance <- abs(abs_value - 0.5)
                    if (deviance < 0.06) .("Negligible")
                    else if (deviance < 0.14) .("Small")
                    else if (deviance < 0.21) .("Medium")
                    else .("Large")
                },
                "kendall_w" = {
                    # Kendall's W: 0.1 small, 0.3 medium, 0.5 large
                    if (abs_value < 0.1) .("Negligible")
                    else if (abs_value < 0.3) .("Small")
                    else if (abs_value < 0.5) .("Medium")
                    else .("Large")
                },
                {
                    # Default Cohen's d scale
                    if (abs_value < 0.2) .("Small")
                    else if (abs_value < 0.5) .("Medium")
                    else .("Large")
                }
            )
        },

        .interpretEffectSize = function(value, method) {
            if (is.na(value)) return(.("Effect size not available"))
            magnitude <- private$.interpretEffectSizeMagnitude(value, method)
            sprintf(.("Effect size = %.3f (%s); %s"), value, method, magnitude)
        },
        
        # Post-hoc test implementation
        .runPostHocTest = function(outcome, groups, method, p_adjust) {
            tryCatch({
                group_levels <- levels(groups)
                k <- length(group_levels)
                if (k < 2) return(NULL)

                pairs <- combn(group_levels, 2)
                n_pairs <- ncol(pairs)
                comparisons <- character(n_pairs)
                statistics <- numeric(n_pairs)
                p_raw <- numeric(n_pairs)
                effect_sizes <- numeric(n_pairs)

                if (method == "dunn" && requireNamespace("dunn.test", quietly = TRUE)) {
                    # Dunn's test — method must be lowercase for dunn.test package
                    dunn_method <- tolower(p_adjust)
                    res <- dunn.test::dunn.test(outcome, groups, method = dunn_method, alpha = 0.05, kw = FALSE)
                    comparisons <- res$comparisons
                    statistics <- res$Z
                    p_raw <- res$P
                    p_adjusted <- res$P.adjusted
                    effect_sizes <- rep(NA, length(comparisons))
                    return(data.frame(
                        comparison = comparisons,
                        statistic = statistics,
                        p_raw = p_raw,
                        p_adjusted = p_adjusted,
                        effect_size = effect_sizes,
                        stringsAsFactors = FALSE
                    ))
                }

                # Fallback: pairwise Wilcoxon
                for (i in seq_len(n_pairs)) {
                    g1_name <- pairs[1, i]
                    g2_name <- pairs[2, i]
                    g1 <- outcome[groups == g1_name]
                    g2 <- outcome[groups == g2_name]
                    comparisons[i] <- paste(g1_name, "vs", g2_name)
                    wt <- wilcox.test(g1, g2, exact = FALSE)
                    statistics[i] <- wt$statistic
                    p_raw[i] <- wt$p.value
                    # Cliff's delta as pairwise effect size
                    greater <- sum(outer(g1, g2, ">"))
                    less <- sum(outer(g1, g2, "<"))
                    effect_sizes[i] <- (greater - less) / (length(g1) * length(g2))
                }

                p_adjusted <- p.adjust(p_raw, method = p_adjust)

                data.frame(
                    comparison = comparisons,
                    statistic = statistics,
                    p_raw = p_raw,
                    p_adjusted = p_adjusted,
                    effect_size = effect_sizes,
                    stringsAsFactors = FALSE
                )
            }, error = function(e) {
                NULL
            })
        },
        .bootstrapCliffDelta = function(group1, group2, conf_level = 0.95) {
            
            # Bootstrap implementation for Cliff's Delta confidence intervals
            n_boot <- self$options$bootstrap_samples %||% 1000
            n1 <- length(group1)
            n2 <- length(group2)
            
            # Bootstrap replicates
            boot_deltas <- numeric(n_boot)
            
            for (i in 1:n_boot) {
                # Resample with replacement
                boot_g1 <- sample(group1, n1, replace = TRUE)
                boot_g2 <- sample(group2, n2, replace = TRUE)
                
                # Calculate Cliff's Delta for bootstrap sample
                greater <- sum(outer(boot_g1, boot_g2, ">"))
                less <- sum(outer(boot_g1, boot_g2, "<"))
                boot_deltas[i] <- (greater - less) / (n1 * n2)
            }
            
            # Calculate confidence interval
            alpha <- 1 - conf_level
            ci_lower <- quantile(boot_deltas, alpha/2, na.rm = TRUE)
            ci_upper <- quantile(boot_deltas, 1 - alpha/2, na.rm = TRUE)
            
            list(
                lower = as.numeric(ci_lower),
                upper = as.numeric(ci_upper)
            )
        },
        
        # Create interpretations and explanations
        .createInterpretations = function() {

            # Generate method explanations if requested
            if (self$options$show_explanations) {
                private$.populateMethodExplanations()
            }

            # Generate clinical interpretations if requested
            if (self$options$show_interpretation) {
                private$.populateInterpretations()
            }

            # Generate recommendations if requested
            if (self$options$show_recommendations) {
                rec_html <- private$.createStatisticalRecommendations()
                self$results$statisticalRecommendations$setContent(rec_html)
            }

            # Generate natural language summary
            private$.generateResultSummary()
        },
        
        .generateResultSummary = function() {
            
            # Generate a plain-language summary of results
            test_type <- self$options$test_type %||% "mann_whitney"
            
            # Get the most recent test results from the table
            test_table <- self$results$tests
            if (test_table$rowCount == 0) return()
            
            summary_parts <- c()
            
            # Build summary based on test type
            test_name <- switch(test_type,
                "mann_whitney" = .("Mann-Whitney U test"),
                "kruskal_wallis" = .("Kruskal-Wallis test"), 
                "wilcoxon_signed" = .("Wilcoxon signed-rank test"),
                "friedman" = .("Friedman test"),
                .("Non-parametric test")
            )
            
            # Add basic summary
            summary_parts <- c(summary_parts, 
                jmvcore::format(.("The {} was conducted to compare groups."), test_name)
            )
            
            # Add effect size information if available
            if (self$options$effect_size && self$results$effectsizes$rowCount > 0) {
                effect_method <- self$options$effect_size_method %||% "eta_squared"
                effect_name <- switch(effect_method,
                    "cliff_delta" = .("Cliff's Delta"),
                    "eta_squared" = .("Eta-squared"),
                    "rank_biserial" = .("Rank-biserial correlation"),
                    .("Effect size")
                )
                
                summary_parts <- c(summary_parts,
                    jmvcore::format(.("Effect sizes were calculated using {}."), effect_name)
                )
            }
            
            # Add post-hoc information if available
            if (self$options$post_hoc && self$results$posthoc$rowCount > 0) {
                posthoc_method <- self$options$post_hoc_method %||% "dunn"
                posthoc_name <- switch(posthoc_method,
                    "dunn" = .("Dunn's test"),
                    "conover" = .("Conover-Iman test"),
                    .("Post-hoc comparisons")
                )
                
                summary_parts <- c(summary_parts,
                    jmvcore::format(.("Pairwise comparisons were conducted using {}."), posthoc_name)
                )
            }
            
            # Combine into final summary
            summary_text <- paste(summary_parts, collapse = " ")
            
            # Add copy-ready paragraph
            copy_ready <- paste(
                "<div style='background-color: #f8f9fa; padding: 15px; border-left: 4px solid #007bff; margin: 10px 0;'>",
                "<h4 style='margin-top: 0;'>", .("Analysis Summary"), "</h4>",
                "<p>", summary_text, "</p>",
                "<small style='color: #6c757d;'>", .("Copy-ready summary for reports"), "</small>",
                "</div>"
            )
            
            # Set content to clinical interpretation section
            if (self$options$show_interpretation) {
                self$results$clinicalInterpretation$setContent(copy_ready)
            }
        },
        
        .populateMethodExplanations = function() {
            test_type <- self$options$test_type %||% "mann_whitney"

            # Method explanation
            method_html <- private$.createMethodExplanation()
            self$results$methodExplanation$setContent(method_html)
            self$results$methodsExplanation$setContent(method_html)

            # Effect size explanation
            if (self$options$effect_size) {
                es_html <- private$.createEffectSizeExplanation()
                self$results$effectSizeExplanation$setContent(es_html)
            }

            # Post-hoc explanation
            if (self$options$post_hoc) {
                ph_html <- private$.createPostHocExplanation()
                self$results$postHocExplanation$setContent(ph_html)
            }

            # Assumption explanation
            if (self$options$test_assumptions || self$options$assumption_checks) {
                assump_html <- private$.createAssumptionExplanation()
                self$results$assumptionExplanation$setContent(assump_html)
            }

            # Robust methods explanation
            if (self$options$show_robust_statistics) {
                robust_html <- paste(
                    "<h4>Robust Statistical Methods</h4>",
                    "<p>Robust methods provide estimates that are less sensitive to outliers ",
                    "and departures from distributional assumptions.</p>",
                    "<ul>",
                    "<li><strong>Trimmed mean:</strong> Removes a proportion of extreme values before averaging.</li>",
                    "<li><strong>Winsorized mean:</strong> Replaces extreme values with boundary values.</li>",
                    "<li><strong>Hodges-Lehmann estimator:</strong> Median of all pairwise averages; robust location estimate.</li>",
                    "</ul>"
                )
                self$results$robustMethodsExplanation$setContent(robust_html)
            }

            # Recommendations
            if (self$options$show_recommendations) {
                rec_html <- private$.createStatisticalRecommendations()
                self$results$statisticalRecommendations$setContent(rec_html)
            }
        },

        .populateInterpretations = function() {
            interpretation <- private$.createResultInterpretation()
            self$results$resultInterpretation$setContent(interpretation)
        },
        
        .validateTestConfiguration = function() {
            
            # Enhanced misuse detection and warnings
            test_type <- self$options$test_type %||% "mann_whitney"
            groups_var <- self$options$groups
            paired_var <- self$options$paired_variable
            
            if (is.null(groups_var)) return()
            
            # Get group data for validation
            groups <- self$data[[groups_var]]
            if (is.null(groups)) return()
            
            n_groups <- nlevels(as.factor(groups))
            
            # Mann-Whitney U test validation
            if (test_type == "mann_whitney" && n_groups != 2) {
                self$results$tests$setNote("warning",
                    jmvcore::format(.("Mann-Whitney U test requires exactly 2 groups, but {} groups found. Consider using Kruskal-Wallis test for 3+ groups."), n_groups))
            }
            
            # Kruskal-Wallis test validation  
            if (test_type == "kruskal_wallis" && n_groups < 3) {
                self$results$tests$setNote("warning",
                    jmvcore::format(.("Kruskal-Wallis test is designed for 3+ groups, but only {} groups found. Consider using Mann-Whitney U test for 2 groups."), n_groups))
            }
            
            # Paired test validation
            if (test_type == "wilcoxon_signed" && is.null(paired_var)) {
                self$results$tests$setNote("warning",
                    .("Wilcoxon signed-rank test selected but no pairing variable provided. This test requires paired data."))
            }
            
            # Friedman test validation
            if (test_type == "friedman") {
                if (is.null(self$options$blocking_variable)) {
                    self$results$tests$setNote("warning",
                        .("Friedman test requires a blocking variable for repeated measures design."))
                }
                if (n_groups < 3) {
                    self$results$tests$setNote("warning",
                        jmvcore::format(.("Friedman test is designed for 3+ related groups, but only {} groups found."), n_groups))
                }
            }
            
            # McNemar test validation
            if (test_type == "mcnemar" && is.null(paired_var)) {
                self$results$tests$setNote("warning", 
                    .("McNemar test requires paired categorical data. Please specify a pairing variable."))
            }
            
            # Sample size warnings
            group_sizes <- table(groups)
            small_groups <- sum(group_sizes < 5)
            if (small_groups > 0) {
                self$results$tests$setNote("warning",
                    jmvcore::format(.("{} group(s) have fewer than 5 observations. Results may be unreliable for small samples."), small_groups))
            }
            
            # Effect size appropriateness warnings
            if (self$options$effect_size) {
                effect_method <- self$options$effect_size_method %||% "eta_squared"
                
                # Cliff's Delta is best for 2-group comparisons
                if (effect_method == "cliff_delta" && n_groups != 2) {
                    self$results$effectsizes$setNote("warning",
                        jmvcore::format(.("Cliff's Delta is designed for 2-group comparisons, but {} groups found. Consider using Eta-squared for multi-group comparisons."), n_groups))
                }
                
                # Eta-squared is best for multi-group comparisons  
                if (effect_method == "eta_squared" && n_groups == 2) {
                    self$results$effectsizes$setNote("info",
                        .("Eta-squared can be used for 2 groups, but Cliff's Delta may provide more intuitive interpretation."))
                }
            }
        },
        
        .robustStandardError = function(data, method) {
            n <- length(data)
            if (n < 3) return(NA)
            if (method == "trimmed") {
                trim_prop <- self$options$trim_proportion %||% 0.1
                k <- floor(n * trim_prop)
                trimmed <- sort(data)[(k + 1):(n - k)]
                return(sd(trimmed) / sqrt(length(trimmed)))
            }
            # Default: MAD-based SE
            mad_val <- mad(data, na.rm = TRUE)
            if (mad_val == 0) return(NA)
            return(mad_val / sqrt(n))
        },

        .robustConfidenceInterval = function(data, method, conf_level) {
            se <- private$.robustStandardError(data, method)
            if (is.na(se) || se == 0) return(list(lower = NA, upper = NA))
            z <- qnorm((1 + conf_level) / 2)
            center <- median(data, na.rm = TRUE)
            list(lower = center - z * se, upper = center + z * se)
        },

        .calculatePower = function(n, effect_size, alpha) {
            # Approximate power using normal approximation
            if (is.na(effect_size) || effect_size == 0) return(list(power = NA))
            z_alpha <- qnorm(1 - alpha / 2)
            z_power <- abs(effect_size) * sqrt(n) - z_alpha
            power <- pnorm(z_power)
            list(power = max(0, min(1, power)))
        },

        .calculateRequiredN = function(effect_size, power, alpha) {
            if (is.na(effect_size) || abs(effect_size) < 0.001) return(NA)
            z_alpha <- qnorm(1 - alpha / 2)
            z_power <- qnorm(power)
            n <- ((z_alpha + z_power) / abs(effect_size))^2
            as.integer(ceiling(n))
        },

        .interpretPower = function(power) {
            if (is.na(power)) return("Power calculation not available")
            if (power >= 0.8) return("Adequate power (>= 80%)")
            if (power >= 0.5) return("Moderate power; consider larger sample")
            return("Low power; results may be unreliable")
        },

        .interpretPostHocResult = function(p_value, alpha) {
            if (is.na(p_value)) return("Cannot interpret")
            if (p_value < alpha) return("Statistically significant difference")
            return("No statistically significant difference")
        },

        .createMethodExplanation = function() {
            test_type <- self$options$test_type %||% "mann_whitney"
            desc <- switch(test_type,
                "mann_whitney" = "<p>The <strong>Mann-Whitney U test</strong> (Wilcoxon rank-sum) compares two independent groups by ranking all observations and testing whether ranks differ systematically between groups.</p>",
                "kruskal_wallis" = "<p>The <strong>Kruskal-Wallis test</strong> is a non-parametric one-way ANOVA comparing the distribution of three or more independent groups using ranks.</p>",
                "wilcoxon_signed" = "<p>The <strong>Wilcoxon signed-rank test</strong> compares two related samples by examining the ranks of the absolute differences between paired observations.</p>",
                "friedman" = "<p>The <strong>Friedman test</strong> is a non-parametric repeated-measures ANOVA that tests for differences across multiple related groups.</p>",
                "median_test" = "<p>The <strong>Median test</strong> compares groups by dichotomising observations above and below the grand median and applying a chi-square test.</p>",
                "van_der_waerden" = "<p>The <strong>Van der Waerden test</strong> uses normal-score transformations of ranks, giving an ANOVA-like test that is sensitive to location differences.</p>",
                "mood_median" = "<p><strong>Mood's median test</strong> is similar to the median test but may use Fisher's exact test for small expected counts.</p>",
                "cochran_q" = "<p><strong>Cochran's Q test</strong> extends McNemar's test to three or more matched groups with binary outcomes.</p>",
                "page_trend" = "<p><strong>Page's trend test</strong> detects monotonic ordered alternatives across repeated measures.</p>",
                "mcnemar" = "<p>The <strong>McNemar test</strong> tests whether marginal proportions differ in paired 2x2 tables.</p>",
                "sign_test" = "<p>The <strong>Sign test</strong> is a simple paired test that counts positive and negative differences without using magnitudes.</p>",
                "jonckheere_terpstra" = "<p>The <strong>Jonckheere-Terpstra test</strong> tests for an ordered trend across groups (H1: theta_1 <= theta_2 <= ... <= theta_k).</p>",
                "<p>Non-parametric statistical test selected.</p>"
            )
            paste("<h4>Method Explanation</h4>", desc)
        },

        .createEffectSizeExplanation = function() {
            method <- self$options$effect_size_method %||% "eta_squared"
            desc <- switch(method,
                "eta_squared" = "Eta-squared represents the proportion of variance in ranks explained by group membership (analogous to R-squared).",
                "cliff_delta" = "Cliff's Delta measures the probability that a randomly chosen value from one group is larger than one from another, minus the reverse probability. Range: [-1, 1].",
                "rank_biserial" = "Rank-biserial correlation quantifies the relationship between a binary grouping variable and a continuous outcome using ranks.",
                "cles" = "Common Language Effect Size is the probability that a randomly selected observation from one group exceeds one from the other group.",
                paste("Effect size:", method)
            )
            paste("<h4>Effect Size Explanation</h4><p>", desc, "</p>")
        },

        .createPostHocExplanation = function() {
            method <- self$options$post_hoc_method %||% "dunn"
            p_adj <- self$options$p_adjustment %||% "holm"
            paste(
                "<h4>Post-Hoc Comparisons</h4>",
                "<p>Post-hoc tests identify which specific groups differ after a significant omnibus test.</p>",
                "<p>Method used: <strong>", method, "</strong>. P-value adjustment: <strong>", p_adj, "</strong>.</p>"
            )
        },

        .createAssumptionExplanation = function() {
            paste(
                "<h4>Assumptions for Non-Parametric Tests</h4>",
                "<ul>",
                "<li><strong>Independence:</strong> Observations must be independent (except for paired/repeated designs).</li>",
                "<li><strong>Ordinal or continuous:</strong> The dependent variable should be at least ordinal.</li>",
                "<li><strong>Similar shape:</strong> For Mann-Whitney/Kruskal-Wallis to compare medians, distributions should have similar shapes.</li>",
                "</ul>"
            )
        },

        .createResultInterpretation = function() {
            test_type <- self$options$test_type %||% "mann_whitney"
            test_table <- self$results$tests
            if (test_table$rowCount == 0) return("<p>No test results available yet.</p>")

            row <- test_table$getRow(1)
            p_val <- row[["p"]]
            stat_val <- row[["statistic"]]
            alpha <- self$options$alpha_level %||% 0.05

            if (is.na(p_val)) return("<p>Test result not available.</p>")

            sig <- if (p_val < alpha) "statistically significant" else "not statistically significant"

            paste(
                "<h4>Results Interpretation</h4>",
                "<p>The ", row[["test"]], " yielded a test statistic of ",
                round(stat_val, 3), " with p = ", format.pval(p_val, digits = 4), ".</p>",
                "<p>At alpha = ", alpha, ", the result is <strong>", sig, "</strong>.</p>"
            )
        },

        .createClinicalInterpretation = function() {
            context <- self$options$clinical_context %||% "general"
            context_desc <- switch(context,
                "general" = "general clinical research",
                "treatment" = "treatment comparison",
                "biomarker" = "biomarker analysis",
                "qol" = "quality of life assessment",
                "diagnostic" = "diagnostic performance evaluation",
                "pathological" = "pathological grading analysis",
                "clinical research"
            )
            paste("<h4>Clinical Context</h4>",
                  "<p>Results interpreted in the context of <strong>", context_desc, "</strong>.</p>")
        },

        .createStatisticalRecommendations = function() {
            recommendations <- c("<h4>Statistical Recommendations</h4><ul>")
            test_type <- self$options$test_type %||% "mann_whitney"

            if (test_type == "mann_whitney") {
                recommendations <- c(recommendations,
                    "<li>Report the U statistic, exact p-value, and an appropriate effect size (e.g., Cliff's Delta).</li>",
                    "<li>Consider whether a one-tailed test is justified by your hypothesis.</li>")
            } else if (test_type == "kruskal_wallis") {
                recommendations <- c(recommendations,
                    "<li>If the overall test is significant, proceed with post-hoc pairwise comparisons.</li>",
                    "<li>Report eta-squared or epsilon-squared as the effect size.</li>")
            }

            recommendations <- c(recommendations,
                "<li>Always report exact sample sizes per group alongside test statistics.</li>",
                "<li>Consider bootstrap confidence intervals for more precise effect size estimation.</li>",
                "</ul>")
            paste(recommendations, collapse = "")
        }
    )
)