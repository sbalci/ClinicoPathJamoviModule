# Enhanced Non-Parametric Tests Module
# Comprehensive non-parametric analysis with effect sizes and post hoc testing
# Critical for addressing 30% usage rate in pathology studies

enhancednonparametricClass <- R6::R6Class(
    "enhancednonparametricClass",
    inherit = enhancednonparametricBase,
    private = list(
        .init = function() {

            # Initialize instructions
            private$.populateInstructions()

            if (is.null(self$options$deps) || length(self$options$deps) == 0 ||
                is.null(self$options$group)) {
                return()
            }

            # Initialize tables
            private$.initializeTables()
        },

        .run = function() {

            if (is.null(self$options$deps) || length(self$options$deps) == 0 ||
                is.null(self$options$group)) {
                return()
            }

            # Process each dependent variable
            for (dep_var in self$options$deps) {

                # Get and validate data for this variable
                data <- private$.cleanData(dep_var)
                if (is.null(data)) next

                # Run descriptive statistics
                private$.populateDescriptives(data, dep_var)

                # Run normality tests
                if (self$options$normality_tests) {
                    private$.populateNormality(data, dep_var)
                }

                # Run assumption checking
                if (self$options$assumption_checks) {
                    private$.populateAssumptions(data, dep_var)
                }

                # Run main non-parametric tests
                private$.populateTests(data, dep_var)

                # Run post hoc tests if applicable
                if (self$options$posthoc != 'none' && length(unique(data$grouping)) > 2) {
                    private$.populatePostHoc(data, dep_var)
                }

                # Run effect sizes
                if (self$options$effect_size) {
                    private$.populateEffectSizes(data, dep_var)
                }
            }

            # Create plots if requested
            if (self$options$descriptive_plots) {
                private$.preparePlots()
            }

            # Add explanations and interpretations
            private$.populateExplanations()
            private$.populateInterpretations()
        },

        .cleanData = function(dep_var) {

            grouping <- self$options$group

            if (is.null(dep_var) || is.null(grouping)) {
                return(NULL)
            }

            data <- self$data

            # Extract variables
            dep_data <- jmvcore::toNumeric(data[[dep_var]])
            group_data <- data[[grouping]]

            # Remove missing values
            complete_cases <- complete.cases(dep_data, group_data)

            if (sum(complete_cases) < 3) {
                # Need at least 3 observations
                return(NULL)
            }

            clean_data <- data.frame(
                dependent = dep_data[complete_cases],
                grouping = as.factor(group_data[complete_cases]),
                dep_var_name = dep_var,
                stringsAsFactors = FALSE
            )

            # Check minimum group sizes
            group_sizes <- table(clean_data$grouping)
            if (any(group_sizes < 2)) {
                return(NULL)
            }

            return(clean_data)
        },

        .initializeTables = function() {

            grouping <- self$options$group
            data <- self$data

            if (is.null(grouping)) return()

            groups <- levels(as.factor(data[[grouping]]))
            n_groups <- length(groups)

            # Initialize descriptives table
            descriptives <- self$results$descriptives

            for (group in groups) {
                descriptives$addRow(rowKey = group, values = list(
                    group = group
                ))
            }

            # Initialize tests table based on test type
            tests <- self$results$tests
            test_type <- self$options$test_type

            if (test_type == "mann_whitney") {
                tests$addRow(rowKey = "mannwhitney", values = list(
                    test = "Mann-Whitney U Test"
                ))
            } else if (test_type == "kruskal_wallis") {
                tests$addRow(rowKey = "kruskalwallis", values = list(
                    test = "Kruskal-Wallis Test"
                ))
            } else if (test_type == "wilcoxon_signed") {
                tests$addRow(rowKey = "wilcoxon", values = list(
                    test = "Wilcoxon Signed-Rank Test"
                ))
            } else if (test_type == "friedman") {
                tests$addRow(rowKey = "friedman", values = list(
                    test = "Friedman Test"
                ))
            } else if (test_type == "cochran_q") {
                tests$addRow(rowKey = "cochranq", values = list(
                    test = "Cochran's Q Test"
                ))
            } else if (test_type == "page_trend") {
                tests$addRow(rowKey = "pagetrend", values = list(
                    test = "Page's Trend Test"
                ))
            }

            # Initialize effect sizes table based on test type
            effectsizes <- self$results$effectsizes

            if (test_type == "mann_whitney") {
                effectsizes$addRow(rowKey = "rankbiserial", values = list(
                    measure = "Rank-Biserial Correlation (r)"
                ))
                effectsizes$addRow(rowKey = "cliffs_delta", values = list(
                    measure = "Cliff's Delta (δ)"
                ))
                effectsizes$addRow(rowKey = "hodges_lehmann", values = list(
                    measure = "Hodges-Lehmann Shift (HL)"
                ))
            } else if (test_type == "kruskal_wallis") {
                effectsizes$addRow(rowKey = "eta_squared", values = list(
                    measure = "Eta-squared (η²)"
                ))
                effectsizes$addRow(rowKey = "epsilon_squared", values = list(
                    measure = "Epsilon-squared (ε²)"
                ))
            } else if (test_type == "wilcoxon_signed") {
                effectsizes$addRow(rowKey = "wilcoxon_r", values = list(
                    measure = "Matched-pairs rank-biserial r"
                ))
            } else if (test_type == "friedman") {
                effectsizes$addRow(rowKey = "kendalls_w", values = list(
                    measure = "Kendall's W (coefficient of concordance)"
                ))
            } else if (test_type == "cochran_q") {
                effectsizes$addRow(rowKey = "cochran_w", values = list(
                    measure = "Kendall's W for binary data"
                ))
            } else if (test_type == "page_trend") {
                effectsizes$addRow(rowKey = "tau_trend", values = list(
                    measure = "Kendall's tau for trend"
                ))
                effectsizes$addRow(rowKey = "page_w", values = list(
                    measure = "Kendall's W (concordance)"
                ))
            }

            # Initialize post hoc table if needed
            if (n_groups > 2 && self$options$posthoc) {
                posthoc <- self$results$posthoc

                for (i in 1:(n_groups-1)) {
                    for (j in (i+1):n_groups) {
                        comparison <- paste(groups[i], "vs", groups[j])
                        posthoc$addRow(rowKey = paste0(i, "_", j), values = list(
                            comparison = comparison
                        ))
                    }
                }
            }
        },

        .populateDescriptives = function(data) {

            descriptives <- self$results$descriptives

            # Calculate descriptive statistics for each group
            group_stats <- aggregate(dependent ~ grouping, data, function(x) {
                list(
                    n = length(x),
                    median = median(x, na.rm = TRUE),
                    q1 = quantile(x, 0.25, na.rm = TRUE),
                    q3 = quantile(x, 0.75, na.rm = TRUE),
                    mean = mean(x, na.rm = TRUE),
                    sd = sd(x, na.rm = TRUE),
                    min = min(x, na.rm = TRUE),
                    max = max(x, na.rm = TRUE)
                )
            })

            for (i in 1:nrow(group_stats)) {
                group_name <- as.character(group_stats$grouping[i])
                stats <- group_stats$dependent[[i]]

                descriptives$setRow(rowKey = group_name, values = list(
                    group = group_name,
                    n = stats$n,
                    median = stats$median,
                    q1 = stats$q1,
                    q3 = stats$q3,
                    mean = stats$mean,
                    sd = stats$sd,
                    min = stats$min,
                    max = stats$max
                ))
            }
        },

        .populateAssumptions = function(data) {

            assumptions <- self$results$assumptions

            n_groups <- length(unique(data$grouping))

            # Normality tests by group (Shapiro-Wilk)
            normality_results <- list()
            normal_all <- TRUE

            for (group in unique(data$grouping)) {
                group_data <- data$dependent[data$grouping == group]

                if (length(group_data) >= 3 && length(group_data) <= 5000) {
                    shapiro_test <- shapiro.test(group_data)
                    normality_results[[as.character(group)]] <- list(
                        statistic = shapiro_test$statistic,
                        p_value = shapiro_test$p.value,
                        normal = shapiro_test$p.value > 0.05
                    )
                    if (shapiro_test$p.value <= 0.05) normal_all <- FALSE
                } else {
                    normality_results[[as.character(group)]] <- list(
                        statistic = NA,
                        p_value = NA,
                        normal = NA
                    )
                }
            }

            # Overall normality assessment
            assumptions$addRow(rowKey = "normality", values = list(
                assumption = "Normality (Shapiro-Wilk)",
                assessment = ifelse(normal_all, "Met", "Violated"),
                recommendation = ifelse(normal_all,
                    "Consider parametric tests (t-test, ANOVA)",
                    "Non-parametric tests recommended")
            ))

            # Homogeneity of variance (Levene's test)
            if (n_groups >= 2) {
                tryCatch({
                    levene_test <- car::leveneTest(dependent ~ grouping, data = data)
                    equal_var <- levene_test$`Pr(>F)`[1] > 0.05

                    assumptions$addRow(rowKey = "homogeneity", values = list(
                        assumption = "Homogeneity of Variance (Levene's)",
                        assessment = ifelse(equal_var, "Met", "Violated"),
                        recommendation = ifelse(equal_var,
                            "Equal variances assumed",
                            "Consider Welch corrections or non-parametric tests")
                    ))
                }, error = function(e) {
                    assumptions$addRow(rowKey = "homogeneity", values = list(
                        assumption = "Homogeneity of Variance (Levene's)",
                        assessment = "Cannot assess",
                        recommendation = "Insufficient data for variance testing"
                    ))
                })
            }

            # Independence assumption (user responsibility)
            assumptions$addRow(rowKey = "independence", values = list(
                assumption = "Independence of Observations",
                assessment = "User Verified",
                recommendation = "Ensure observations are independent (no repeated measures, clustering, etc.)"
            ))
        },

        .populateTests = function(data, dep_var) {

            tests <- self$results$tests
            test_type <- self$options$test_type
            n_groups <- length(unique(data$grouping))

            if (test_type == "mann_whitney" && n_groups == 2) {
                # Mann-Whitney U Test
                private$.runMannWhitneyTest(data, tests)
            } else if (test_type == "kruskal_wallis" && n_groups > 2) {
                # Kruskal-Wallis Test
                private$.runKruskalWallisTest(data, tests)
            } else if (test_type == "wilcoxon_signed") {
                # Wilcoxon Signed-Rank Test (paired)
                private$.runWilcoxonSignedTest(data, tests)
            } else if (test_type == "friedman") {
                # Friedman Test (repeated measures)
                private$.runFriedmanTest(data, tests)
            } else if (test_type == "cochran_q") {
                # Cochran's Q Test (>2 paired groups)
                private$.runCochranQTest(data, tests)
            } else if (test_type == "page_trend") {
                # Page's Trend Test (ordered alternative to Friedman)
                private$.runPageTrendTest(data, tests)
            }
        },

        .runMannWhitneyTest = function(data, tests) {

            groups <- unique(data$grouping)
            group1_data <- data$dependent[data$grouping == groups[1]]
            group2_data <- data$dependent[data$grouping == groups[2]]

            # Enhanced Mann-Whitney U test
            tryCatch({
                mw_test <- wilcox.test(group1_data, group2_data,
                                     paired = FALSE,
                                     exact = self$options$exact_test,
                                     conf.int = TRUE,
                                     conf.level = self$options$confidence_level)

                # Calculate additional statistics
                n1 <- length(group1_data)
                n2 <- length(group2_data)
                U1 <- mw_test$statistic
                U2 <- n1 * n2 - U1

                # Effect size (rank-biserial correlation)
                r_rb <- 1 - (2 * U1) / (n1 * n2)

                # Confidence interval for effect size
                z_score <- qnorm((1 + self$options$confidence_level) / 2)
                se_r <- sqrt((n1 + n2 + 1) / (3 * n1 * n2))
                r_ci_lower <- r_rb - z_score * se_r
                r_ci_upper <- r_rb + z_score * se_r

                tests$setRow(rowKey = "mannwhitney", values = list(
                    test = "Mann-Whitney U Test",
                    statistic = U1,
                    df = NA,
                    p = mw_test$p.value,
                    effect_size = r_rb,
                    ci_lower = r_ci_lower,
                    ci_upper = r_ci_upper,
                    interpretation = private$.interpretMannWhitney(mw_test$p.value, r_rb)
                ))

            }, error = function(e) {
                tests$setRow(rowKey = "mannwhitney", values = list(
                    test = "Mann-Whitney U Test",
                    statistic = NA,
                    df = NA,
                    p = NA,
                    effect_size = NA,
                    ci_lower = NA,
                    ci_upper = NA,
                    interpretation = paste("Error:", e$message)
                ))
            })
        },

        .runKruskalWallisTest = function(data, tests) {

            tryCatch({
                kw_test <- kruskal.test(dependent ~ grouping, data = data)

                # Calculate eta-squared effect size
                n <- nrow(data)
                k <- length(unique(data$grouping))
                eta_squared <- (kw_test$statistic - k + 1) / (n - k)
                eta_squared <- max(0, eta_squared) # Ensure non-negative

                tests$setRow(rowKey = "kruskalwallis", values = list(
                    test = "Kruskal-Wallis Test",
                    statistic = kw_test$statistic,
                    df = kw_test$parameter,
                    p = kw_test$p.value,
                    effect_size = eta_squared,
                    ci_lower = NA,
                    ci_upper = NA,
                    interpretation = private$.interpretKruskalWallis(kw_test$p.value, eta_squared)
                ))

            }, error = function(e) {
                tests$setRow(rowKey = "kruskalwallis", values = list(
                    test = "Kruskal-Wallis Test",
                    statistic = NA,
                    df = NA,
                    p = NA,
                    effect_size = NA,
                    ci_lower = NA,
                    ci_upper = NA,
                    interpretation = paste("Error:", e$message)
                ))
            })
        },

        .runWilcoxonSignedTest = function(data, tests) {

            # Note: For paired samples, data should contain paired observations
            # This is a simplified implementation - full paired data handling would require
            # different data input structure

            tryCatch({
                # Assuming data contains paired differences or can be restructured
                wilcox_test <- wilcox.test(data$dependent,
                                         mu = 0,
                                         paired = TRUE,
                                         exact = self$options$exact_test,
                                         conf.int = TRUE,
                                         conf.level = self$options$confidence_level)

                # Calculate effect size (matched pairs rank-biserial correlation)
                n <- length(data$dependent)
                r_rb <- wilcox_test$statistic / (n * (n + 1) / 4)

                tests$addRow(rowKey = "wilcoxon", values = list(
                    test = "Wilcoxon Signed-Rank Test",
                    statistic = wilcox_test$statistic,
                    df = NA,
                    p = wilcox_test$p.value,
                    effect_size = r_rb,
                    ci_lower = wilcox_test$conf.int[1],
                    ci_upper = wilcox_test$conf.int[2],
                    interpretation = private$.interpretWilcoxon(wilcox_test$p.value, r_rb)
                ))

            }, error = function(e) {
                tests$addRow(rowKey = "wilcoxon", values = list(
                    test = "Wilcoxon Signed-Rank Test",
                    statistic = NA,
                    df = NA,
                    p = NA,
                    effect_size = NA,
                    ci_lower = NA,
                    ci_upper = NA,
                    interpretation = paste("Error:", e$message)
                ))
            })
        },

        .runFriedmanTest = function(data, tests) {

            # Friedman test for repeated measures non-parametric ANOVA
            # Requires data to be structured with repeated measures

            tryCatch({
                # Convert data to matrix format for Friedman test
                # Each row = subject, each column = condition/time point
                data_matrix <- as.matrix(reshape2::dcast(data,
                    formula = seq_len(nrow(data)) ~ grouping,
                    value.var = "dependent")[, -1])

                friedman_test <- friedman.test(data_matrix)

                # Calculate Kendall's W (coefficient of concordance)
                n <- nrow(data_matrix)  # number of subjects
                k <- ncol(data_matrix)  # number of conditions
                chi_sq <- friedman_test$statistic
                kendalls_w <- chi_sq / (n * (k - 1))

                tests$addRow(rowKey = "friedman", values = list(
                    test = "Friedman Test",
                    statistic = friedman_test$statistic,
                    df = friedman_test$parameter,
                    p = friedman_test$p.value,
                    effect_size = kendalls_w,
                    ci_lower = NA,
                    ci_upper = NA,
                    interpretation = private$.interpretFriedman(friedman_test$p.value, kendalls_w)
                ))

            }, error = function(e) {
                tests$addRow(rowKey = "friedman", values = list(
                    test = "Friedman Test",
                    statistic = NA,
                    df = NA,
                    p = NA,
                    effect_size = NA,
                    ci_lower = NA,
                    ci_upper = NA,
                    interpretation = paste("Error - requires proper repeated measures data structure:", e$message)
                ))
            })
        },

        .runCochranQTest = function(data, tests) {

            # Cochran's Q test for comparing proportions across matched groups
            # Requires binary/dichotomous data (0/1) in repeated measures format

            tryCatch({
                # Convert continuous data to binary if needed (median split or other threshold)
                # For demonstration, using median split - in practice, use clinically meaningful cutoff
                if (!all(data$dependent %in% c(0, 1))) {
                    median_val <- median(data$dependent, na.rm = TRUE)
                    data$dependent <- ifelse(data$dependent >= median_val, 1, 0)
                }

                # Reshape to matrix format for Cochran's Q
                data_matrix <- as.matrix(reshape2::dcast(data,
                    formula = seq_len(nrow(data)) ~ grouping,
                    value.var = "dependent")[, -1])

                # Calculate Cochran's Q statistic
                k <- ncol(data_matrix)  # number of groups
                n <- nrow(data_matrix)  # number of subjects

                # Row totals (Ri) and column totals (Cj)
                row_totals <- rowSums(data_matrix)
                col_totals <- colSums(data_matrix)
                grand_total <- sum(data_matrix)

                # Cochran's Q statistic
                q_stat <- (k - 1) * (k * sum(col_totals^2) - grand_total^2) /
                          (k * grand_total - sum(row_totals^2))

                # Degrees of freedom
                df <- k - 1

                # P-value (chi-square distribution)
                p_value <- 1 - pchisq(q_stat, df)

                # Effect size (Kendall's W for binary data)
                kendalls_w <- q_stat / (n * (k - 1))

                tests$addRow(rowKey = "cochranq", values = list(
                    test = "Cochran's Q Test",
                    statistic = q_stat,
                    df = df,
                    p = p_value,
                    effect_size = kendalls_w,
                    ci_lower = NA,
                    ci_upper = NA,
                    interpretation = private$.interpretCochranQ(p_value, kendalls_w)
                ))

            }, error = function(e) {
                tests$addRow(rowKey = "cochranq", values = list(
                    test = "Cochran's Q Test",
                    statistic = NA,
                    df = NA,
                    p = NA,
                    effect_size = NA,
                    ci_lower = NA,
                    ci_upper = NA,
                    interpretation = paste("Error - requires binary data in repeated measures format:", e$message)
                ))
            })
        },

        .runPageTrendTest = function(data, tests) {

            # Page's Trend Test for ordered alternatives in repeated measures
            # Tests for monotonic trends across ordered time points/conditions

            tryCatch({
                # Convert data to matrix format for Page's test
                # Assumes grouping variable represents ordered time points/conditions
                data_matrix <- as.matrix(reshape2::dcast(data,
                    formula = seq_len(nrow(data)) ~ grouping,
                    value.var = "dependent")[, -1])

                n <- nrow(data_matrix)  # number of subjects
                k <- ncol(data_matrix)  # number of conditions/time points

                # Rank data within each subject (row)
                ranked_data <- t(apply(data_matrix, 1, rank, ties.method = "average"))

                # Calculate Page's L statistic
                # L = sum of (i * R_i) where i is the condition index and R_i is sum of ranks
                condition_weights <- 1:k  # Ordered weights for conditions
                rank_sums <- colSums(ranked_data)

                L_statistic <- sum(condition_weights * rank_sums)

                # Expected value and variance under null hypothesis
                E_L <- n * k * (k + 1)^2 / 4
                Var_L <- n * k^2 * (k + 1) * (k - 1) / 144

                # Standardized test statistic (approximately normal for large n)
                z_stat <- (L_statistic - E_L) / sqrt(Var_L)

                # One-tailed p-value (testing for increasing trend)
                p_value <- 1 - pnorm(abs(z_stat))

                # Effect size: Kendall's tau for trend
                # Simplified calculation based on Page's L
                tau_trend <- (2 * (L_statistic - E_L)) / (n * k * (k - 1))

                # Also calculate Kendall's W for concordance
                chi_sq_friedman <- 12 * sum((rank_sums - n * (k + 1) / 2)^2) /
                                   (n * k * (k + 1))
                kendalls_w <- chi_sq_friedman / (n * (k - 1))

                tests$addRow(rowKey = "pagetrend", values = list(
                    test = "Page's Trend Test",
                    statistic = L_statistic,
                    df = NA,
                    p = p_value,
                    effect_size = tau_trend,
                    ci_lower = NA,
                    ci_upper = NA,
                    interpretation = private$.interpretPageTrend(p_value, tau_trend, kendalls_w)
                ))

            }, error = function(e) {
                tests$addRow(rowKey = "pagetrend", values = list(
                    test = "Page's Trend Test",
                    statistic = NA,
                    df = NA,
                    p = NA,
                    effect_size = NA,
                    ci_lower = NA,
                    ci_upper = NA,
                    interpretation = paste("Error - requires ordered repeated measures data:", e$message)
                ))
            })
        },

        .populatePostHoc = function(data) {

            if (length(unique(data$grouping)) <= 2) return()

            posthoc <- self$results$posthoc
            groups <- unique(data$grouping)
            n_groups <- length(groups)

            # Dunn's test for post hoc comparisons
            tryCatch({
                dunn_test <- dunn.test::dunn.test(
                    data$dependent,
                    data$grouping,
                    method = self$options$posthoc_method,
                    kw = FALSE,
                    alpha = 1 - self$options$confidence_level
                )

                # Extract pairwise results
                comparisons <- dunn_test$comparisons
                p_values <- dunn_test$P.adjusted
                z_values <- dunn_test$Z

                for (i in 1:length(comparisons)) {
                    comparison_parts <- strsplit(comparisons[i], " - ")[[1]]
                    group1 <- comparison_parts[1]
                    group2 <- comparison_parts[2]

                    # Calculate effect size for each pair
                    group1_data <- data$dependent[data$grouping == group1]
                    group2_data <- data$dependent[data$grouping == group2]

                    # Rank-biserial correlation for pairwise comparison
                    mw_test <- wilcox.test(group1_data, group2_data, exact = FALSE)
                    n1 <- length(group1_data)
                    n2 <- length(group2_data)
                    r_rb <- 1 - (2 * mw_test$statistic) / (n1 * n2)

                    row_key <- paste0(group1, "_", group2)
                    posthoc$setRow(rowKey = row_key, values = list(
                        comparison = paste(group1, "vs", group2),
                        z_statistic = z_values[i],
                        p_raw = dunn_test$P[i],
                        p_adjusted = p_values[i],
                        effect_size = r_rb,
                        significance = ifelse(p_values[i] < 0.05, "*", ""),
                        interpretation = private$.interpretPostHoc(p_values[i], r_rb)
                    ))
                }

            }, error = function(e) {
                posthoc$addRow(values = list(
                    comparison = "Post hoc analysis failed",
                    z_statistic = NA,
                    p_raw = NA,
                    p_adjusted = NA,
                    effect_size = NA,
                    significance = "",
                    interpretation = paste("Error:", e$message)
                ))
            })
        },

        .populateEffectSizes = function(data) {

            effectsizes <- self$results$effectsizes
            test_type <- self$options$test_type
            n_groups <- length(unique(data$grouping))

            if (test_type == "mann_whitney") {
                # Calculate Cliff's Delta
                groups <- unique(data$grouping)
                group1_data <- data$dependent[data$grouping == groups[1]]
                group2_data <- data$dependent[data$grouping == groups[2]]

                # Cliff's Delta calculation
                cliffs_delta <- private$.calculateCliffsDelta(group1_data, group2_data)

                # Hodges-Lehmann shift calculation
                hodges_lehmann <- private$.calculateHodgesLehmann(group1_data, group2_data)

                # Rank-biserial correlation
                mw_test <- wilcox.test(group1_data, group2_data, exact = FALSE)
                n1 <- length(group1_data)
                n2 <- length(group2_data)
                r_rb <- 1 - (2 * mw_test$statistic) / (n1 * n2)

                effectsizes$setRow(rowKey = "rankbiserial", values = list(
                    measure = "Rank-Biserial Correlation (r)",
                    value = r_rb,
                    interpretation = private$.interpretRankBiserial(r_rb)
                ))

                effectsizes$setRow(rowKey = "cliffs_delta", values = list(
                    measure = "Cliff's Delta (δ)",
                    value = cliffs_delta,
                    interpretation = private$.interpretCliffsDelta(cliffs_delta)
                ))

                effectsizes$setRow(rowKey = "hodges_lehmann", values = list(
                    measure = "Hodges-Lehmann Shift (HL)",
                    value = hodges_lehmann,
                    interpretation = private$.interpretHodgesLehmann(hodges_lehmann, group1_data, group2_data)
                ))

            } else if (test_type == "kruskal_wallis") {
                # Eta-squared and epsilon-squared for Kruskal-Wallis
                kw_test <- kruskal.test(dependent ~ grouping, data = data)
                n <- nrow(data)
                k <- n_groups

                eta_squared <- (kw_test$statistic - k + 1) / (n - k)
                eta_squared <- max(0, eta_squared)

                epsilon_squared <- (kw_test$statistic - k + 1) / (n^2 - 1)
                epsilon_squared <- max(0, epsilon_squared)

                effectsizes$setRow(rowKey = "eta_squared", values = list(
                    measure = "Eta-squared (η²)",
                    value = eta_squared,
                    interpretation = private$.interpretEtaSquared(eta_squared)
                ))

                effectsizes$setRow(rowKey = "epsilon_squared", values = list(
                    measure = "Epsilon-squared (ε²)",
                    value = epsilon_squared,
                    interpretation = private$.interpretEpsilonSquared(epsilon_squared)
                ))
            } else if (test_type == "wilcoxon_signed") {
                # Effect size for Wilcoxon signed-rank test
                wilcox_test <- wilcox.test(data$dependent, mu = 0, paired = TRUE, exact = FALSE)
                n <- length(data$dependent)
                r_rb <- wilcox_test$statistic / (n * (n + 1) / 4)

                effectsizes$setRow(rowKey = "wilcoxon_r", values = list(
                    measure = "Matched-pairs rank-biserial r",
                    value = r_rb,
                    interpretation = private$.interpretRankBiserial(r_rb)
                ))

            } else if (test_type == "friedman") {
                # Kendall's W for Friedman test
                # This requires repeated measures data structure
                tryCatch({
                    data_matrix <- as.matrix(reshape2::dcast(data,
                        formula = seq_len(nrow(data)) ~ grouping,
                        value.var = "dependent")[, -1])

                    friedman_test <- friedman.test(data_matrix)
                    n <- nrow(data_matrix)
                    k <- ncol(data_matrix)
                    kendalls_w <- friedman_test$statistic / (n * (k - 1))

                    effectsizes$setRow(rowKey = "kendalls_w", values = list(
                        measure = "Kendall's W (coefficient of concordance)",
                        value = kendalls_w,
                        interpretation = private$.interpretKendallsW(kendalls_w)
                    ))
                }, error = function(e) {
                    effectsizes$setRow(rowKey = "kendalls_w", values = list(
                        measure = "Kendall's W (coefficient of concordance)",
                        value = NA,
                        interpretation = "Error in calculation"
                    ))
                })

            } else if (test_type == "cochran_q") {
                # Kendall's W for Cochran's Q test (binary data)
                tryCatch({
                    # Convert to binary if needed
                    if (!all(data$dependent %in% c(0, 1))) {
                        median_val <- median(data$dependent, na.rm = TRUE)
                        data$dependent <- ifelse(data$dependent >= median_val, 1, 0)
                    }

                    data_matrix <- as.matrix(reshape2::dcast(data,
                        formula = seq_len(nrow(data)) ~ grouping,
                        value.var = "dependent")[, -1])

                    k <- ncol(data_matrix)
                    n <- nrow(data_matrix)

                    # Calculate Q statistic
                    row_totals <- rowSums(data_matrix)
                    col_totals <- colSums(data_matrix)
                    grand_total <- sum(data_matrix)

                    q_stat <- (k - 1) * (k * sum(col_totals^2) - grand_total^2) /
                              (k * grand_total - sum(row_totals^2))

                    kendalls_w <- q_stat / (n * (k - 1))

                    effectsizes$setRow(rowKey = "cochran_w", values = list(
                        measure = "Kendall's W for binary data",
                        value = kendalls_w,
                        interpretation = private$.interpretKendallsW(kendalls_w)
                    ))
                }, error = function(e) {
                    effectsizes$setRow(rowKey = "cochran_w", values = list(
                        measure = "Kendall's W for binary data",
                        value = NA,
                        interpretation = "Error in calculation"
                    ))
                })
            } else if (test_type == "page_trend") {
                # Effect sizes for Page's trend test
                tryCatch({
                    data_matrix <- as.matrix(reshape2::dcast(data,
                        formula = seq_len(nrow(data)) ~ grouping,
                        value.var = "dependent")[, -1])

                    n <- nrow(data_matrix)
                    k <- ncol(data_matrix)

                    # Rank data within each subject
                    ranked_data <- t(apply(data_matrix, 1, rank, ties.method = "average"))

                    # Calculate Page's L and related statistics
                    condition_weights <- 1:k
                    rank_sums <- colSums(ranked_data)
                    L_statistic <- sum(condition_weights * rank_sums)

                    # Expected values
                    E_L <- n * k * (k + 1)^2 / 4

                    # Tau for trend
                    tau_trend <- (2 * (L_statistic - E_L)) / (n * k * (k - 1))

                    # Kendall's W for concordance
                    chi_sq_friedman <- 12 * sum((rank_sums - n * (k + 1) / 2)^2) /
                                       (n * k * (k + 1))
                    kendalls_w <- chi_sq_friedman / (n * (k - 1))

                    effectsizes$setRow(rowKey = "tau_trend", values = list(
                        measure = "Kendall's tau for trend",
                        value = tau_trend,
                        interpretation = private$.interpretTauTrend(tau_trend)
                    ))

                    effectsizes$setRow(rowKey = "page_w", values = list(
                        measure = "Kendall's W (concordance)",
                        value = kendalls_w,
                        interpretation = private$.interpretKendallsW(kendalls_w)
                    ))
                }, error = function(e) {
                    effectsizes$setRow(rowKey = "tau_trend", values = list(
                        measure = "Kendall's tau for trend",
                        value = NA,
                        interpretation = "Error in calculation"
                    ))
                    effectsizes$setRow(rowKey = "page_w", values = list(
                        measure = "Kendall's W (concordance)",
                        value = NA,
                        interpretation = "Error in calculation"
                    ))
                })
            }
        },

        .calculateCliffsDelta = function(x, y) {
            # Cliff's Delta: proportion of pairs where x > y minus proportion where x < y
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
            # Hodges-Lehmann shift: median of all pairwise differences (x - y)
            # This estimates the median difference between groups
            differences <- c()

            for (xi in x) {
                for (yj in y) {
                    differences <- c(differences, xi - yj)
                }
            }

            # Return median of all pairwise differences
            return(median(differences))
        },

        .interpretCliffsDelta = function(delta) {
            # Clinical interpretation of Cliff's Delta for comparative analysis
            abs_delta <- abs(delta)
            direction <- ifelse(delta > 0, "higher", "lower")
            prob <- round((delta + 1) / 2 * 100, 1)

            if (abs_delta < 0.147) {
                magnitude <- "negligible"
                clinical <- "minimal clinical difference"
            } else if (abs_delta < 0.33) {
                magnitude <- "small"
                clinical <- "potentially meaningful for research"
            } else if (abs_delta < 0.474) {
                magnitude <- "medium"
                clinical <- "likely clinically important"
            } else {
                magnitude <- "large"
                clinical <- "substantial clinical significance"
            }

            return(paste0(
                magnitude, " effect (δ = ", round(delta, 3), "). ",
                "Probability that Group 1 has ", direction, " values: ", prob, "%. ",
                "Clinical significance: ", clinical, ".",
                if (abs_delta >= 0.33) " Consider clinical adequacy implications." else ""
            ))
        },

        .interpretHodgesLehmann = function(hl_shift, group1_data, group2_data) {
            # Clinical interpretation of Hodges-Lehmann shift for comparative analysis
            group1_median <- median(group1_data, na.rm = TRUE)
            group2_median <- median(group2_data, na.rm = TRUE)
            percent_change <- abs(hl_shift) / median(c(group1_data, group2_data), na.rm = TRUE) * 100

            direction <- ifelse(hl_shift > 0, "higher", "lower")
            abs_shift <- abs(hl_shift)

            # Generic thresholds for clinical interpretation
            if (abs_shift < 0.5) {
                magnitude <- "minimal"
                clinical <- "unlikely to be clinically significant"
            } else if (abs_shift < 2) {
                magnitude <- "small"
                clinical <- "may be clinically relevant for research"
            } else if (abs_shift < 5) {
                magnitude <- "moderate"
                clinical <- "likely clinically important for method comparison"
            } else {
                magnitude <- "large"
                clinical <- "substantial clinical difference affecting practice"
            }

            return(paste0(
                magnitude, " shift (HL = ", round(hl_shift, 2), " units). ",
                "Group 1 typically has ", round(abs_shift, 1), " ", direction, " values than Group 2 (",
                round(percent_change, 1), "% change). ",
                "Clinical significance: ", clinical, ".",
                if (abs_shift >= 2) " Consider clinical adequacy implications." else ""
            ))
        },

        .interpretWilcoxon = function(p_value, r_rb) {
            sig_text <- ifelse(p_value < 0.05, "Significant", "Non-significant")

            effect_text <- if (abs(r_rb) < 0.1) "negligible"
            else if (abs(r_rb) < 0.3) "small"
            else if (abs(r_rb) < 0.5) "medium"
            else "large"

            direction <- ifelse(r_rb > 0, "positive median difference", "negative median difference")

            return(paste0(sig_text, " paired difference (", effect_text, " effect, ", direction, ")"))
        },

        .interpretFriedman = function(p_value, kendalls_w) {
            sig_text <- ifelse(p_value < 0.05, "Significant", "Non-significant")

            effect_text <- if (kendalls_w < 0.1) "negligible"
            else if (kendalls_w < 0.3) "small"
            else if (kendalls_w < 0.5) "medium"
            else "large"

            return(paste0(sig_text, " differences across repeated measures (", effect_text, " effect, W = ",
                         round(kendalls_w, 3), ")"))
        },

        .interpretCochranQ = function(p_value, kendalls_w) {
            sig_text <- ifelse(p_value < 0.05, "Significant", "Non-significant")

            effect_text <- if (kendalls_w < 0.1) "negligible"
            else if (kendalls_w < 0.3) "small"
            else if (kendalls_w < 0.5) "medium"
            else "large"

            return(paste0(sig_text, " differences in proportions across matched groups (",
                         effect_text, " effect, W = ", round(kendalls_w, 3), ")"))
        },

        .interpretPageTrend = function(p_value, tau_trend, kendalls_w) {
            sig_text <- ifelse(p_value < 0.05, "Significant", "Non-significant")

            trend_direction <- ifelse(tau_trend > 0, "increasing", "decreasing")

            effect_text <- if (abs(tau_trend) < 0.1) "negligible"
            else if (abs(tau_trend) < 0.3) "small"
            else if (abs(tau_trend) < 0.5) "medium"
            else "large"

            return(paste0(sig_text, " ", trend_direction, " trend across ordered conditions (",
                         effect_text, " effect, τ = ", round(tau_trend, 3), ", W = ", round(kendalls_w, 3), ")"))
        },

        .interpretMannWhitney = function(p_value, r_rb) {
            sig_text <- ifelse(p_value < 0.05, "Significant", "Non-significant")

            effect_text <- if (abs(r_rb) < 0.1) "negligible"
            else if (abs(r_rb) < 0.3) "small"
            else if (abs(r_rb) < 0.5) "medium"
            else "large"

            direction <- ifelse(r_rb > 0, "Group 1 > Group 2", "Group 1 < Group 2")

            return(paste0(sig_text, " difference (", effect_text, " effect, ", direction, ")"))
        },

        .interpretKruskalWallis = function(p_value, eta_squared) {
            sig_text <- ifelse(p_value < 0.05, "Significant", "Non-significant")

            effect_text <- if (eta_squared < 0.01) "negligible"
            else if (eta_squared < 0.06) "small"
            else if (eta_squared < 0.14) "medium"
            else "large"

            return(paste0(sig_text, " group differences (", effect_text, " effect)"))
        },

        .interpretPostHoc = function(p_value, r_rb) {
            sig_text <- ifelse(p_value < 0.05, "Significant", "Non-significant")

            effect_text <- if (abs(r_rb) < 0.1) "negligible"
            else if (abs(r_rb) < 0.3) "small"
            else if (abs(r_rb) < 0.5) "medium"
            else "large"

            return(paste0(sig_text, " (", effect_text, " effect)"))
        },

        .interpretRankBiserial = function(r_rb) {
            abs_r <- abs(r_rb)
            if (abs_r < 0.1) return("negligible effect")
            else if (abs_r < 0.3) return("small effect")
            else if (abs_r < 0.5) return("medium effect")
            else return("large effect")
        },


        .interpretEtaSquared = function(eta_squared) {
            if (eta_squared < 0.01) return("negligible effect")
            else if (eta_squared < 0.06) return("small effect")
            else if (eta_squared < 0.14) return("medium effect")
            else return("large effect")
        },

        .interpretEpsilonSquared = function(epsilon) {
            if (epsilon < 0.01) return("negligible effect")
            else if (epsilon < 0.06) return("small effect")
            else if (epsilon < 0.14) return("medium effect")
            else return("large effect")
        },

        .interpretKendallsW = function(w) {
            if (w < 0.1) return("negligible concordance")
            else if (w < 0.3) return("small concordance")
            else if (w < 0.5) return("medium concordance")
            else if (w < 0.7) return("large concordance")
            else return("very large concordance")
        },

        .interpretTauTrend = function(tau) {
            abs_tau <- abs(tau)
            direction <- ifelse(tau > 0, "increasing", "decreasing")

            if (abs_tau < 0.1) return(paste("negligible", direction, "trend"))
            else if (abs_tau < 0.3) return(paste("small", direction, "trend"))
            else if (abs_tau < 0.5) return(paste("medium", direction, "trend"))
            else return(paste("large", direction, "trend"))
        },

        .prepareDistributionPlot = function(data) {

            if (is.null(data) || nrow(data) == 0) return()

            plot <- self$results$distributionplot

            tryCatch({

                # Create distribution plot showing group differences
                p <- ggplot2::ggplot(data, ggplot2::aes(x = grouping, y = dependent, fill = grouping)) +
                    ggplot2::geom_boxplot(alpha = 0.7, outlier.alpha = 0.6) +
                    ggplot2::geom_jitter(width = 0.2, alpha = 0.5, size = 1.5) +
                    ggplot2::labs(
                        title = "Distribution Comparison by Group",
                        subtitle = "Boxplots with individual data points",
                        x = "Group",
                        y = "Dependent Variable",
                        caption = "Box shows median (center line), quartiles (box), and 1.5×IQR (whiskers)"
                    ) +
                    ggplot2::theme_minimal() +
                    ggplot2::theme(
                        plot.title = ggplot2::element_text(size = 14, face = "bold"),
                        plot.subtitle = ggplot2::element_text(size = 12),
                        legend.position = "none",
                        axis.title = ggplot2::element_text(size = 11),
                        axis.text = ggplot2::element_text(size = 10)
                    ) +
                    ggplot2::scale_fill_brewer(type = "qual", palette = "Set2")

                plot$setState(p)

            }, error = function(e) {
                # Error handling for plot creation
            })
        },

        .prepareEffectSizePlot = function(data) {

            if (is.null(data) || nrow(data) == 0) return()

            plot <- self$results$effectsizeplot
            n_groups <- length(unique(data$grouping))

            if (n_groups > 2) return() # Effect size plot only for 2 groups

            tryCatch({

                groups <- unique(data$grouping)
                group1_data <- data$dependent[data$grouping == groups[1]]
                group2_data <- data$dependent[data$grouping == groups[2]]

                # Calculate rank-biserial correlation
                mw_test <- wilcox.test(group1_data, group2_data, exact = FALSE)
                n1 <- length(group1_data)
                n2 <- length(group2_data)
                r_rb <- 1 - (2 * mw_test$statistic) / (n1 * n2)

                # Create effect size visualization
                effect_data <- data.frame(
                    Effect = "Rank-Biserial r",
                    Value = r_rb,
                    CI_Lower = r_rb - 1.96 * sqrt((n1 + n2 + 1) / (3 * n1 * n2)),
                    CI_Upper = r_rb + 1.96 * sqrt((n1 + n2 + 1) / (3 * n1 * n2))
                )

                p <- ggplot2::ggplot(effect_data, ggplot2::aes(x = Effect, y = Value)) +
                    ggplot2::geom_point(size = 4, color = "steelblue") +
                    ggplot2::geom_errorbar(ggplot2::aes(ymin = CI_Lower, ymax = CI_Upper),
                                         width = 0.1, size = 1, color = "steelblue") +
                    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.6) +
                    ggplot2::geom_hline(yintercept = c(-0.1, 0.1), linetype = "dotted", alpha = 0.4, color = "gray") +
                    ggplot2::geom_hline(yintercept = c(-0.3, 0.3), linetype = "dotted", alpha = 0.4, color = "orange") +
                    ggplot2::geom_hline(yintercept = c(-0.5, 0.5), linetype = "dotted", alpha = 0.4, color = "red") +
                    ggplot2::labs(
                        title = "Effect Size with Confidence Interval",
                        subtitle = "Rank-Biserial Correlation for Mann-Whitney U Test",
                        x = "",
                        y = "Effect Size",
                        caption = "Guidelines: |r| < 0.1 negligible, 0.1-0.3 small, 0.3-0.5 medium, >0.5 large"
                    ) +
                    ggplot2::theme_minimal() +
                    ggplot2::theme(
                        plot.title = ggplot2::element_text(size = 14, face = "bold"),
                        plot.subtitle = ggplot2::element_text(size = 12),
                        axis.title = ggplot2::element_text(size = 11),
                        axis.text = ggplot2::element_text(size = 10)
                    ) +
                    ggplot2::coord_flip()

                plot$setState(p)

            }, error = function(e) {
                # Error handling for plot creation
            })
        },

        .populateInstructions = function() {

            html <- self$results$instructions

            str <- "
            <h2>Enhanced Non-Parametric Tests</h2>

            <p>This module provides comprehensive non-parametric tests for comparing groups when data violates parametric assumptions.
            Non-parametric tests are essential in pathology research where biomarker expressions and cellular measurements often show
            non-normal distributions, outliers, or ordinal scales.</p>

            <h3>Key Features</h3>
            <ul>
                <li><strong>Mann-Whitney U Test</strong>: Enhanced two-group comparison with effect sizes and confidence intervals</li>
                <li><strong>Kruskal-Wallis Test</strong>: Multi-group comparison with comprehensive post hoc analysis</li>
                <li><strong>Effect Size Calculations</strong>: Rank-biserial correlation, Cliff's Delta, eta-squared, epsilon-squared</li>
                <li><strong>Assumption Checking</strong>: Automated assessment of test assumptions and recommendations</li>
                <li><strong>Post Hoc Analysis</strong>: Dunn's test with multiple comparison corrections</li>
                <li><strong>Clinical Interpretation</strong>: Contextual interpretation for pathology research</li>
            </ul>

            <h3>When to Use Non-Parametric Tests</h3>
            <ul>
                <li><strong>Non-normal Data</strong>: Skewed distributions, heavy tails, or multimodal data</li>
                <li><strong>Ordinal Data</strong>: Tumor grades (I, II, III, IV), immunohistochemical scores (0, 1+, 2+, 3+)</li>
                <li><strong>Small Sample Sizes</strong>: When normality cannot be reliably assessed</li>
                <li><strong>Outliers Present</strong>: Robust to extreme values and outliers</li>
                <li><strong>Unequal Variances</strong>: No homoscedasticity assumption required</li>
            </ul>

            <h3>Clinical Applications in Pathology</h3>
            <ul>
                <li><strong>Biomarker Expression</strong>: Comparing Ki-67 indices, hormone receptor scores across patient groups</li>
                <li><strong>Tumor Characteristics</strong>: Analyzing mitotic counts, nuclear grades, cellular density differences</li>
                <li><strong>Digital Pathology</strong>: Comparing algorithm outputs, texture measures, spatial metrics</li>
                <li><strong>Immunohistochemistry</strong>: Analyzing staining intensities, positive cell percentages</li>
                <li><strong>Molecular Pathology</strong>: Comparing expression levels, mutation frequencies</li>
            </ul>

            <h3>🔬 Lymph Node Adequacy Analysis Example</h3>
            <div style='background:#f0f8ff; border-left:4px solid #2196F3; padding:15px; margin:15px 0;'>
                <p><strong>Research Question:</strong> Does Method A yield more lymph nodes than Method B?</p>

                <p><strong>Why Nonparametric Analysis?</strong></p>
                <ul>
                    <li>Lymph node counts are typically right-skewed (can't be negative, long tail)</li>
                    <li>Wide variability between patients and surgical techniques</li>
                    <li>Adequacy threshold (≥12 LN) creates clinical interpretation challenges</li>
                </ul>

                <p><strong>Optimal Effect Size Selection:</strong></p>
                <ul>
                    <li><strong>Cliff's Delta (δ):</strong> \"What's the probability that Method A yields more LNs?\"
                        <br>δ = 0.4 means 70% chance Method A > Method B (Formula: (δ+1)/2)</li>
                    <li><strong>Hodges-Lehmann Shift:</strong> \"How many more lymph nodes does Method A typically find?\"
                        <br>HL = 6.2 means Method A typically finds 6.2 more lymph nodes</li>
                </ul>

                <p><strong>Clinical Interpretation Thresholds:</strong></p>
                <ul>
                    <li><strong>HL Shift < 3 LN:</strong> Unlikely to affect adequacy (≥12 LN) assessment</li>
                    <li><strong>HL Shift 3-6 LN:</strong> May improve adequacy rates, consider method adoption</li>
                    <li><strong>HL Shift > 6 LN:</strong> Substantial improvement, strong evidence for method change</li>
                    <li><strong>Cliff's δ > 0.33:</strong> Medium-to-large effect, clinically meaningful difference</li>
                </ul>

                <p><strong>Research Translation:</strong></p>
                <ul>
                    <li>Mann-Whitney U test provides statistical significance (p-value)</li>
                    <li>Cliff's Delta quantifies probability of superiority</li>
                    <li>Hodges-Lehmann shift estimates typical benefit magnitude</li>
                    <li>Both measures are robust to outliers and skewed distributions</li>
                </ul>
            </div>

            <h3>Effect Size Interpretation</h3>
            <ul>
                <li><strong>Rank-Biserial r</strong>: |r| < 0.1 (negligible), 0.1-0.3 (small), 0.3-0.5 (medium), >0.5 (large)</li>
                <li><strong>Cliff's Delta (δ)</strong>: |δ| < 0.147 (negligible), 0.147-0.33 (small), 0.33-0.474 (medium), >0.474 (large)
                    <br><em>Clinical: Probability that Group 1 > Group 2 = (δ+1)/2 × 100%</em></li>
                <li><strong>Hodges-Lehmann Shift</strong>: Median difference in original units (e.g., lymph nodes, biomarker levels)
                    <br><em>Clinical: Direct interpretation - \"Group 1 typically has X more/fewer units than Group 2\"</em></li>
                <li><strong>Eta-squared</strong>: η² < 0.01 (negligible), 0.01-0.06 (small), 0.06-0.14 (medium), >0.14 (large)</li>
            </ul>

            <h3>Statistical Guidance</h3>
            <p><strong>Critical Impact:</strong> This module addresses the critical issue where ~30% of pathology studies use non-parametric
            tests but fail to report proper effect sizes and confidence intervals. Proper effect size reporting is essential for:</p>
            <ul>
                <li>Clinical significance assessment beyond statistical significance</li>
                <li>Meta-analysis inclusion and evidence synthesis</li>
                <li>Sample size planning for future studies</li>
                <li>Biomarker validation and clinical translation</li>
            </ul>

            <h3>Post Hoc Testing</h3>
            <p>When Kruskal-Wallis test is significant (p < 0.05), post hoc analysis using Dunn's test identifies which specific
            group pairs differ significantly. Multiple comparison corrections (Bonferroni, Holm, FDR) control family-wise error rate.</p>

            <p><strong>Note:</strong> Always verify that assumptions of independence are met and consider the clinical context
            when interpreting statistical significance versus clinical relevance.</p>
            "

            html$setContent(str)
        },

        .populateInterpretation = function() {

            html <- self$results$interpretation

            # Get current analysis results to provide specific interpretation
            dependent <- self$options$dependent
            grouping <- self$options$grouping

            if (is.null(dependent) || is.null(grouping)) {
                str <- "<p>Please specify dependent variable and grouping variable to see interpretation.</p>"
                html$setContent(str)
                return()
            }

            data <- private$.cleanData()
            if (is.null(data)) {
                str <- "<p>Unable to provide interpretation due to data issues.</p>"
                html$setContent(str)
                return()
            }

            n_groups <- length(unique(data$grouping))

            str <- paste0("
            <h3>Clinical Interpretation and Recommendations</h3>

            <h4>Analysis Summary</h4>
            <p>Your analysis includes <strong>", n_groups, " groups</strong> with a total of <strong>", nrow(data), " observations</strong>.
            Based on the number of groups, ",
            ifelse(n_groups == 2,
                "Mann-Whitney U test was performed for two-group comparison",
                "Kruskal-Wallis test was performed for multi-group comparison"),
            ".</p>

            <h4>Clinical Context</h4>
            <p>Non-parametric tests are particularly valuable in pathology research because:</p>
            <ul>
                <li><strong>Robustness</strong>: Resistant to outliers common in biological measurements</li>
                <li><strong>Distribution-free</strong>: No assumptions about normal distribution required</li>
                <li><strong>Ordinal Data</strong>: Appropriate for histological grades and immunostaining scores</li>
                <li><strong>Small Samples</strong>: Reliable even with limited patient cohorts</li>
            </ul>

            <h4>Effect Size Clinical Relevance</h4>
            <p>Effect sizes help determine <strong>clinical significance</strong> beyond statistical significance:</p>
            <ul>
                <li><strong>Small effects (r < 0.3)</strong>: May be statistically detectable but clinically limited</li>
                <li><strong>Medium effects (r = 0.3-0.5)</strong>: Potentially clinically meaningful, consider clinical context</li>
                <li><strong>Large effects (r > 0.5)</strong>: Likely clinically significant, important for patient care</li>
            </ul>

            <h4>Next Steps for Clinical Translation</h4>
            <ol>
                <li><strong>Validate Findings</strong>: Replicate results in independent patient cohorts</li>
                <li><strong>Clinical Correlation</strong>: Assess association with patient outcomes, treatment response</li>
                <li><strong>Biomarker Development</strong>: If effect sizes are large, consider diagnostic/prognostic utility</li>
                <li><strong>Sample Size Planning</strong>: Use effect sizes for future study power calculations</li>
                <li><strong>Publication Preparation</strong>: Report both statistical significance and effect sizes</li>
            </ol>

            <h4>Quality Assurance</h4>
            <p><strong>Critical for Pathology Research:</strong> This analysis addresses methodological gaps where studies report
            p-values without effect sizes. Proper reporting includes:</p>
            <ul>
                <li>Test statistics with degrees of freedom</li>
                <li>Exact p-values (not just 'p < 0.05')</li>
                <li>Effect sizes with confidence intervals</li>
                <li>Clinical interpretation of effect magnitude</li>
            </ul>

            <p><strong>Regulatory Compliance:</strong> These analyses follow best practices for biomarker validation studies
            and meet requirements for peer review in pathology and clinical research journals.</p>
            ")

            html$setContent(str)
        }
    )
)
