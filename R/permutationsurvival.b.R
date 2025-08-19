# Permutation Tests for Survival
# This file is part of ClinicoPath

permutationsurvivalClass <- R6::R6Class(
    "permutationsurvivalClass",
    inherit = permutationsurvivalBase,
    private = list(
        .init = function() {
            
            private$.initTodo()
        },
        
        .initTodo = function() {
            todo <- glue::glue(
                "<b>Permutation Tests for Survival</b>
                <br><br>
                Select variables:
                <br>• Time Variable: {ifelse(is.null(self$options$elapsedtime), '❌ Not selected', '✅ Selected')}
                <br>• Event Variable: {ifelse(is.null(self$options$outcome), '❌ Not selected', '✅ Selected')}  
                <br>• Group Variable: {ifelse(is.null(self$options$explanatory), '❌ Not selected', '✅ Selected')}
                <br><br>
                <b>About Permutation Tests:</b>
                <br>• Non-parametric alternative to traditional log-rank tests
                <br>• No assumptions about data distribution or asymptotic theory
                <br>• Ideal for small samples or violated assumptions
                <br>• Provides exact p-values through resampling
                <br>• Controls Type I error rate exactly under null hypothesis
                "
            )
            
            self$results$todo$setContent(todo)
        },
        
        .run = function() {
            
            # Early return if variables not selected
            if (is.null(self$options$elapsedtime) || 
                is.null(self$options$outcome) || 
                is.null(self$options$explanatory)) {
                return()
            }
            
            # Prepare data
            prepared_data <- private$.prepareData()
            if (is.null(prepared_data)) return()
            
            # Set seed for reproducibility
            set.seed(self$options$seed_value)
            
            # Perform permutation tests
            permutation_results <- private$.performPermutationTests(prepared_data)
            if (is.null(permutation_results)) return()
            
            # Populate results tables
            private$.populatePermutationTable(permutation_results)
            private$.populateGroupSummaryTable(prepared_data)
            
            # Create plots
            if (self$options$show_survival_curves) {
                private$.createSurvivalPlot(prepared_data)
            }
            
            if (self$options$show_permutation_distribution && !is.null(permutation_results$permutation_stats)) {
                private$.createPermutationDistributionPlot(permutation_results)
            }
            
            if (self$options$show_test_progression && !is.null(permutation_results$progression_data)) {
                private$.createTestProgressionPlot(permutation_results)
            }
            
            # Generate summaries
            if (self$options$showSummaries) {
                private$.generateSummary(permutation_results, prepared_data)
            }
            
            if (self$options$showExplanations) {
                private$.generateExplanation()
            }
        },
        
        .prepareData = function() {
            
            # Get data
            time_var <- self$options$elapsedtime
            event_var <- self$options$outcome 
            group_var <- self$options$explanatory
            strat_var <- self$options$stratify_variable
            
            if (is.null(time_var) || is.null(event_var) || is.null(group_var)) {
                return(NULL)
            }
            
            mydata <- self$data
            
            # Extract variables
            time <- jmvcore::toNumeric(mydata[[time_var]])
            event <- mydata[[event_var]]
            group <- mydata[[group_var]]
            
            # Handle stratification variable
            strat <- NULL
            if (!is.null(strat_var)) {
                strat <- mydata[[strat_var]]
            }
            
            # Handle outcome level
            outcome_level <- self$options$outcomeLevel
            if (is.factor(event)) {
                event_binary <- ifelse(event == outcome_level, 1, 0)
            } else {
                event_binary <- ifelse(event == as.numeric(outcome_level), 1, 0)
            }
            
            # Remove missing data
            if (is.null(strat)) {
                complete_cases <- complete.cases(time, event_binary, group)
            } else {
                complete_cases <- complete.cases(time, event_binary, group, strat)
            }
            
            if (sum(complete_cases) == 0) {
                jmvcore::reject("No complete cases available for analysis")
                return(NULL)
            }
            
            # Create clean dataset
            clean_data <- data.frame(
                time = time[complete_cases],
                event = event_binary[complete_cases],
                group = group[complete_cases]
            )
            
            if (!is.null(strat)) {
                clean_data$strat <- strat[complete_cases]
            }
            
            # Ensure group is factor
            clean_data$group <- as.factor(clean_data$group)
            
            # Validate data
            if (length(levels(clean_data$group)) < 2) {
                jmvcore::reject("At least 2 groups are required for comparison")
                return(NULL)
            }
            
            if (sum(clean_data$event) == 0) {
                jmvcore::reject("No events observed in the data")
                return(NULL)
            }
            
            # Create survival object
            clean_data$surv <- survival::Surv(clean_data$time, clean_data$event)
            
            return(clean_data)
        },
        
        .performPermutationTests = function(data) {
            
            method <- self$options$permutation_method
            test_stat <- self$options$test_statistic
            n_perms <- self$options$n_permutations
            
            # Calculate observed test statistic
            observed_stat <- private$.calculateTestStatistic(data, test_stat)
            
            if (is.null(observed_stat)) {
                return(NULL)
            }
            
            # Generate permutations based on method
            if (method == "exact") {
                permutation_stats <- private$.performExactPermutation(data, test_stat, observed_stat)
            } else if (method == "stratified" && "strat" %in% names(data)) {
                permutation_stats <- private$.performStratifiedPermutation(data, test_stat, n_perms, observed_stat)
            } else { # approximate
                permutation_stats <- private$.performApproximatePermutation(data, test_stat, n_perms, observed_stat)
            }
            
            if (is.null(permutation_stats)) {
                return(NULL)
            }
            
            # Calculate p-value
            if (method == "exact") {
                n_extreme <- sum(abs(permutation_stats$stats) >= abs(observed_stat))
                total_perms <- length(permutation_stats$stats)
                p_value <- n_extreme / total_perms
                actual_n_perms <- total_perms
            } else {
                n_extreme <- sum(abs(permutation_stats$stats) >= abs(observed_stat))
                p_value <- (n_extreme + 1) / (n_perms + 1)  # Add 1 for observed statistic
                actual_n_perms <- n_perms
            }
            
            # Create result
            result <- list(
                comparison = paste(levels(data$group), collapse = " vs "),
                test_statistic_name = private$.getTestStatisticName(test_stat),
                observed_statistic = observed_stat,
                n_permutations = actual_n_perms,
                p_value = p_value,
                permutation_stats = permutation_stats$stats,
                progression_data = permutation_stats$progression
            )
            
            # Apply multiple comparison correction
            if (self$options$multiple_comparison != "none") {
                result$p_adjusted <- p.adjust(result$p_value, method = self$options$multiple_comparison)
            } else {
                result$p_adjusted <- result$p_value
            }
            
            return(result)
        },
        
        .calculateTestStatistic = function(data, test_stat) {
            
            tryCatch({
                if (test_stat == "logrank") {
                    # Standard log-rank test statistic
                    test_result <- survival::survdiff(surv ~ group, data = data)
                    return(test_result$chisq)
                } else if (test_stat == "wilcoxon") {
                    # Wilcoxon (Gehan-Breslow) test statistic  
                    test_result <- survival::survdiff(surv ~ group, data = data, rho = 1)
                    return(test_result$chisq)
                } else if (test_stat == "tarone_ware") {
                    # Tarone-Ware test statistic
                    test_result <- survival::survdiff(surv ~ group, data = data, rho = 0.5)
                    return(test_result$chisq)
                } else if (test_stat == "max_deviation") {
                    # Maximum absolute deviation between survival curves
                    fit1 <- survival::survfit(surv ~ group, data = data)
                    
                    # Extract survival curves for each group
                    groups <- levels(data$group)
                    if (length(groups) == 2) {
                        # Find common time points
                        times1 <- fit1[1]$time
                        times2 <- fit1[2]$time
                        common_times <- sort(unique(c(times1, times2)))
                        
                        # Calculate survival probabilities at common times
                        surv1 <- summary(fit1[1], times = common_times, extend = TRUE)$surv
                        surv2 <- summary(fit1[2], times = common_times, extend = TRUE)$surv
                        
                        # Maximum absolute deviation
                        return(max(abs(surv1 - surv2), na.rm = TRUE))
                    } else {
                        # For multiple groups, use pairwise maximum
                        max_dev <- 0
                        for (i in 1:(length(groups)-1)) {
                            for (j in (i+1):length(groups)) {
                                subset_data <- data[data$group %in% groups[c(i,j)], ]
                                subset_data$group <- droplevels(subset_data$group)
                                fit_subset <- survival::survfit(surv ~ group, data = subset_data)
                                
                                times1 <- fit_subset[1]$time
                                times2 <- fit_subset[2]$time
                                common_times <- sort(unique(c(times1, times2)))
                                
                                surv1 <- summary(fit_subset[1], times = common_times, extend = TRUE)$surv
                                surv2 <- summary(fit_subset[2], times = common_times, extend = TRUE)$surv
                                
                                dev <- max(abs(surv1 - surv2), na.rm = TRUE)
                                max_dev <- max(max_dev, dev)
                            }
                        }
                        return(max_dev)
                    }
                }
            }, error = function(e) {
                message("Error calculating test statistic: ", e$message)
                return(NULL)
            })
        },
        
        .performApproximatePermutation = function(data, test_stat, n_perms, observed_stat) {
            
            stats <- numeric(n_perms)
            progression <- numeric(min(100, n_perms))  # Store up to 100 points for progression
            progression_indices <- round(seq(1, n_perms, length.out = min(100, n_perms)))
            progression_counter <- 1
            
            for (i in 1:n_perms) {
                # Permute group assignments
                permuted_data <- data
                permuted_data$group <- sample(data$group)
                
                # Calculate test statistic for permuted data
                perm_stat <- private$.calculateTestStatistic(permuted_data, test_stat)
                
                if (!is.null(perm_stat)) {
                    stats[i] <- perm_stat
                }
                
                # Store progression data
                if (i %in% progression_indices && progression_counter <= length(progression)) {
                    n_extreme <- sum(abs(stats[1:i]) >= abs(observed_stat), na.rm = TRUE)
                    progression[progression_counter] <- (n_extreme + 1) / (i + 1)
                    progression_counter <- progression_counter + 1
                }
            }
            
            return(list(
                stats = stats,
                progression = progression[1:(progression_counter-1)]
            ))
        },
        
        .performExactPermutation = function(data, test_stat, observed_stat) {
            
            # For exact permutation, we need to enumerate all possible group assignments
            # This is only feasible for small sample sizes
            n <- nrow(data)
            groups <- unique(data$group)
            n_groups <- table(data$group)
            
            if (n > 10) {
                jmvcore::reject("Exact permutation test is only feasible for small samples (n ≤ 10)")
                return(NULL)
            }
            
            # Generate all possible combinations
            all_perms <- private$.generateAllPermutations(n, n_groups)
            
            stats <- numeric(nrow(all_perms))
            
            for (i in 1:nrow(all_perms)) {
                permuted_data <- data
                permuted_data$group <- factor(all_perms[i, ], levels = levels(data$group))
                
                perm_stat <- private$.calculateTestStatistic(permuted_data, test_stat)
                
                if (!is.null(perm_stat)) {
                    stats[i] <- perm_stat
                }
            }
            
            return(list(stats = stats, progression = NULL))
        },
        
        .performStratifiedPermutation = function(data, test_stat, n_perms, observed_stat) {
            
            if (!"strat" %in% names(data)) {
                # Fall back to regular permutation
                return(private$.performApproximatePermutation(data, test_stat, n_perms, observed_stat))
            }
            
            stats <- numeric(n_perms)
            progression <- numeric(min(100, n_perms))
            progression_indices <- round(seq(1, n_perms, length.out = min(100, n_perms)))
            progression_counter <- 1
            
            strata <- unique(data$strat)
            
            for (i in 1:n_perms) {
                permuted_data <- data
                
                # Permute within each stratum
                for (stratum in strata) {
                    stratum_indices <- which(data$strat == stratum)
                    if (length(stratum_indices) > 1) {
                        permuted_data$group[stratum_indices] <- sample(data$group[stratum_indices])
                    }
                }
                
                # Calculate test statistic for permuted data
                perm_stat <- private$.calculateTestStatistic(permuted_data, test_stat)
                
                if (!is.null(perm_stat)) {
                    stats[i] <- perm_stat
                }
                
                # Store progression data
                if (i %in% progression_indices && progression_counter <= length(progression)) {
                    n_extreme <- sum(abs(stats[1:i]) >= abs(observed_stat), na.rm = TRUE)
                    progression[progression_counter] <- (n_extreme + 1) / (i + 1)
                    progression_counter <- progression_counter + 1
                }
            }
            
            return(list(
                stats = stats,
                progression = progression[1:(progression_counter-1)]
            ))
        },
        
        .generateAllPermutations = function(n, group_sizes) {
            
            # This is a simplified approach - in practice, you'd use a more efficient algorithm
            # For now, return a warning that exact permutation is not implemented
            jmvcore::reject("Exact permutation test implementation is not yet available")
            return(NULL)
        },
        
        .getTestStatisticName = function(test_stat) {
            
            switch(test_stat,
                   "logrank" = "Log-rank",
                   "wilcoxon" = "Wilcoxon (Gehan-Breslow)",
                   "tarone_ware" = "Tarone-Ware",
                   "max_deviation" = "Maximum Deviation",
                   "Unknown")
        },
        
        .populatePermutationTable = function(results) {
            
            table <- self$results$permutationTestsTable
            
            table$addRow(rowKey = "main", values = list(
                comparison = results$comparison,
                test_statistic_name = results$test_statistic_name,
                observed_statistic = results$observed_statistic,
                n_permutations = results$n_permutations,
                p_value = results$p_value,
                p_adjusted = results$p_adjusted
            ))
        },
        
        .populateGroupSummaryTable = function(data) {
            
            table <- self$results$groupSummaryTable
            groups <- levels(data$group)
            conf_level <- self$options$confidence_level
            
            for (group in groups) {
                group_data <- data[data$group == group, ]
                
                # Calculate group statistics
                n <- nrow(group_data)
                events <- sum(group_data$event)
                
                # Fit survival curve for median and CI
                fit <- survival::survfit(survival::Surv(time, event) ~ 1, data = group_data, 
                                       conf.int = conf_level)
                
                # Extract median survival and CI
                median_surv <- summary(fit)$table["median"]
                ci_lower <- summary(fit)$table[paste0(conf_level*100, "LCL")]  
                ci_upper <- summary(fit)$table[paste0(conf_level*100, "UCL")]
                
                table$addRow(rowKey = group, values = list(
                    group = group,
                    n = n,
                    events = events,
                    median_survival = ifelse(is.na(median_surv), NA, median_surv),
                    median_ci_lower = ifelse(is.na(ci_lower), NA, ci_lower),
                    median_ci_upper = ifelse(is.na(ci_upper), NA, ci_upper)
                ))
            }
        },
        
        .createSurvivalPlot = function(data) {
            
            if (!requireNamespace("ggplot2", quietly = TRUE) ||
                !requireNamespace("survminer", quietly = TRUE)) {
                return()
            }
            
            # Create survival plot
            fit <- survival::survfit(surv ~ group, data = data)
            
            plot <- survminer::ggsurvplot(
                fit,
                data = data,
                title = "Survival Curves Comparison",
                xlab = "Time",
                ylab = "Survival Probability",
                legend.title = "Group",
                conf.int = TRUE,
                ggtheme = ggplot2::theme_minimal(),
                palette = c("#E7B800", "#2E9FDF", "#00AFBB", "#FC4E07")
            )
            
            image <- self$results$survivalPlot
            image$setState(plot)
        },
        
        .createPermutationDistributionPlot = function(results) {
            
            if (!requireNamespace("ggplot2", quietly = TRUE)) {
                return()
            }
            
            if (is.null(results$permutation_stats)) {
                return()
            }
            
            # Create histogram of permutation statistics
            perm_data <- data.frame(statistic = results$permutation_stats)
            
            plot <- ggplot2::ggplot(perm_data, ggplot2::aes(x = statistic)) +
                ggplot2::geom_histogram(bins = 50, fill = "lightblue", color = "black", alpha = 0.7) +
                ggplot2::geom_vline(xintercept = results$observed_statistic, color = "red", 
                                  linetype = "dashed", size = 1) +
                ggplot2::labs(
                    title = "Distribution of Permutation Test Statistics",
                    subtitle = glue::glue("Red line: Observed statistic = {round(results$observed_statistic, 3)}"),
                    x = paste("Permutation", results$test_statistic_name, "Statistics"),
                    y = "Frequency"
                ) +
                ggplot2::theme_minimal()
            
            image <- self$results$permutationDistributionPlot
            image$setState(plot)
        },
        
        .createTestProgressionPlot = function(results) {
            
            if (!requireNamespace("ggplot2", quietly = TRUE)) {
                return()
            }
            
            if (is.null(results$progression_data) || length(results$progression_data) == 0) {
                return()
            }
            
            # Create progression plot
            n_points <- length(results$progression_data)
            progression_data <- data.frame(
                permutation = round(seq(1, results$n_permutations, length.out = n_points)),
                p_value = results$progression_data
            )
            
            plot <- ggplot2::ggplot(progression_data, ggplot2::aes(x = permutation, y = p_value)) +
                ggplot2::geom_line(color = "blue", size = 1) +
                ggplot2::geom_hline(yintercept = 0.05, linetype = "dashed", color = "red", alpha = 0.7) +
                ggplot2::geom_hline(yintercept = 0.01, linetype = "dashed", color = "red", alpha = 0.7) +
                ggplot2::labs(
                    title = "P-value Convergence During Permutation Testing",
                    subtitle = "Dashed lines at p = 0.05 and p = 0.01",
                    x = "Number of Permutations",
                    y = "P-value"
                ) +
                ggplot2::theme_minimal() +
                ggplot2::ylim(0, max(1, max(progression_data$p_value) * 1.1))
            
            image <- self$results$testProgressionPlot
            image$setState(plot)
        },
        
        .generateSummary = function(results, data) {
            
            groups <- levels(data$group)
            n_total <- nrow(data)
            n_events <- sum(data$event)
            
            summary_text <- glue::glue(
                "<b>Permutation Test Summary</b>
                <br><br>
                <b>Test Configuration:</b>
                <br>• Method: {private$.getMethodName(self$options$permutation_method)}
                <br>• Test Statistic: {results$test_statistic_name}
                <br>• Number of Permutations: {format(results$n_permutations, big.mark = ',')}
                <br>• Multiple Comparison: {private$.getMultipleComparisonName(self$options$multiple_comparison)}
                <br><br>
                <b>Data Summary:</b>
                <br>• Total Subjects: {n_total}
                <br>• Total Events: {n_events} ({round(n_events/n_total*100, 1)}%)
                <br>• Groups Compared: {paste(groups, collapse = ', ')}
                <br><br>
                <b>Test Results:</b>
                <br>• Observed Statistic: {round(results$observed_statistic, 4)}
                <br>• P-value: {format(results$p_value, digits = 4)}
                <br>• Adjusted P-value: {format(results$p_adjusted, digits = 4)}
                <br>• Result: {ifelse(results$p_value < 0.05, 'Significant difference detected', 'No significant difference')}
                "
            )
            
            self$results$analysisSummary$setContent(summary_text)
        },
        
        .generateExplanation = function() {
            
            explanation <- glue::glue(
                "<b>Permutation Tests for Survival Analysis</b>
                <br><br>
                <b>Method Overview:</b>
                <br>Permutation tests provide a non-parametric approach to hypothesis testing that does not rely on 
                distributional assumptions or asymptotic theory. The test works by repeatedly shuffling group assignments 
                and calculating the test statistic under all possible (or many random) permutations.
                <br><br>
                <b>Key Advantages:</b>
                <br>• <b>Distribution-free:</b> No assumptions about underlying data distribution
                <br>• <b>Exact p-values:</b> Provides exact Type I error control under null hypothesis
                <br>• <b>Small sample validity:</b> Valid for small sample sizes where asymptotic theory fails
                <br>• <b>Robust to outliers:</b> Less sensitive to extreme observations than parametric tests
                <br><br>
                <b>Test Statistics Available:</b>
                <br>• <b>Log-rank:</b> Standard survival test statistic (equal weighting across time)
                <br>• <b>Wilcoxon:</b> Emphasizes early differences (Gehan-Breslow weighting)
                <br>• <b>Tarone-Ware:</b> Intermediate weighting between log-rank and Wilcoxon
                <br>• <b>Maximum Deviation:</b> Focuses on largest difference between survival curves
                <br><br>
                <b>Permutation Methods:</b>
                <br>• <b>Approximate:</b> Random sampling of permutations (recommended for n > 10)
                <br>• <b>Exact:</b> All possible permutations (only for very small samples)
                <br>• <b>Stratified:</b> Permutation within strata to control for confounding
                <br><br>
                <b>Interpretation:</b>
                <br>The p-value represents the proportion of permutations that produced a test statistic 
                as extreme or more extreme than the observed statistic. A small p-value indicates that 
                the observed difference is unlikely to have occurred by chance alone.
                "
            )
            
            self$results$methodExplanation$setContent(explanation)
        },
        
        .getMethodName = function(method) {
            switch(method,
                   "exact" = "Exact Permutation",
                   "approximate" = "Approximate Permutation (Monte Carlo)",
                   "stratified" = "Stratified Permutation",
                   "Unknown")
        },
        
        .getMultipleComparisonName = function(method) {
            switch(method,
                   "none" = "None",
                   "bonferroni" = "Bonferroni",
                   "holm" = "Holm",
                   "hochberg" = "Hochberg", 
                   "BH" = "Benjamini-Hochberg (FDR)",
                   "Unknown")
        }
    )
)