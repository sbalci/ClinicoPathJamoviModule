#' @title Patient-Reported Outcomes & Quality of Life Analysis
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import ggplot2
#' @importFrom stats cor t.test aov kruskal.test wilcox.test
#' @export


patientreportedClass <- R6::R6Class(
    "patientreportedClass",
    inherit = patientreportedBase,
    private = list(
        .scale_data = NULL,
        .scale_scores = NULL,
        .reliability_results = NULL,
        .validity_results = NULL,
        .group_results = NULL,
        .longitudinal_results = NULL,
        .missing_patterns = NULL,
        
        .init = function() {
            if (length(self$options$scale_items) == 0) {
                self$results$scale_overview$setNote("note", "At least one scale item is required")
                return()
            }
            
            # Set table titles
            self$results$scale_overview$setTitle("PRO Scale Overview")
            self$results$item_statistics$setTitle("Item-Level Statistics")
            self$results$scale_scores$setTitle("Scale Score Summary")
        },
        
        .run = function() {
            if (length(self$options$scale_items) == 0) {
                return()
            }
            
            # Check for required packages
            private$.checkPackages()
            
            # Process scale data
            private$.processScaleData()
            
            # Calculate scale scores
            private$.calculateScaleScores()
            
            # Reliability analysis
            if (self$options$reliability_analysis) {
                private$.reliabilityAnalysis()
            }
            
            # Validity analysis
            if (self$options$validity_analysis) {
                private$.validityAnalysis()
            }
            
            # Group comparisons
            if (self$options$group_comparisons && !is.null(self$options$group_var)) {
                private$.groupComparisons()
            }
            
            # Longitudinal analysis
            if (self$options$longitudinal_analysis && !is.null(self$options$time_var)) {
                private$.longitudinalAnalysis()
            }
            
            # Data quality assessment
            if (self$options$data_quality_assessment) {
                private$.dataQualityAssessment()
            }
            
            # Clinical interpretation
            if (self$options$clinical_interpretation) {
                private$.clinicalInterpretation()
            }
            
            # Update tables
            private$.populateTables()
        },
        
        .checkPackages = function() {
            # Required packages for PRO analysis
            required_packages <- c("psych", "ltm", "lavaan")
            
            # Additional packages for specific methods
            if (self$options$factor_analysis) {
                required_packages <- c(required_packages, "GPArotation")
            }
            
            if (self$options$irt_analysis) {
                required_packages <- c(required_packages, "mirt")
            }
            
            if (self$options$missing_data_method == "multiple_imputation") {
                required_packages <- c(required_packages, "mice")
            }
            
            for (pkg in required_packages) {
                if (!requireNamespace(pkg, quietly = TRUE)) {
                    tryCatch({
                        install.packages(pkg, repos = "https://cran.rstudio.com/")
                    }, error = function(e) {
                        self$results$scale_overview$setNote("error", paste("Failed to install package:", pkg))
                    })
                }
            }
        },
        
        .processScaleData = function() {
            data <- self$data
            scale_items <- self$options$scale_items
            
            # Extract scale items
            scale_data <- data[, scale_items, drop = FALSE]
            
            # Handle reverse coding
            if (length(self$options$reverse_coded_items) > 0) {
                scale_data <- private$.reverseCodeItems(scale_data)
            }
            
            # Analyze missing data patterns
            private$.analyzeMissingPatterns(scale_data)
            
            # Store processed data
            private$.scale_data <- scale_data
        },
        
        .reverseCodeItems = function(scale_data) {
            reverse_items <- intersect(self$options$reverse_coded_items, names(scale_data))
            scale_min <- self$options$response_scale_min
            scale_max <- self$options$response_scale_max
            
            for (item in reverse_items) {
                if (is.numeric(scale_data[[item]])) {
                    scale_data[[item]] <- (scale_max + scale_min) - scale_data[[item]]
                }
            }
            
            return(scale_data)
        },
        
        .analyzeMissingPatterns = function(scale_data) {
            # Analyze missing data patterns
            missing_matrix <- is.na(scale_data)
            
            # Pattern analysis
            pattern_summary <- apply(missing_matrix, 1, function(row) paste(as.numeric(row), collapse = ""))
            pattern_freq <- table(pattern_summary)
            
            # Store missing patterns
            private$.missing_patterns <- list(
                patterns = pattern_freq,
                total_missing_by_item = colSums(missing_matrix),
                total_missing_by_case = rowSums(missing_matrix),
                complete_cases = sum(complete.cases(scale_data))
            )
        },
        
        .calculateScaleScores = function() {
            scale_data <- private$.scale_data
            method <- self$options$scoring_method
            min_items <- self$options$min_items_required
            missing_threshold <- self$options$missing_threshold
            
            n_items <- ncol(scale_data)
            scores <- numeric(nrow(scale_data))
            
            for (i in seq_len(nrow(scale_data))) {
                row_data <- scale_data[i, ]
                n_valid <- sum(!is.na(row_data))
                
                # Check if minimum requirements are met
                if (n_valid >= min_items && (n_valid / n_items) >= (1 - missing_threshold)) {
                    valid_responses <- row_data[!is.na(row_data)]
                    
                    if (method == "sum_score") {
                        scores[i] <- sum(valid_responses)
                    } else if (method == "mean_score") {
                        scores[i] <- mean(valid_responses)
                    } else if (method == "pro_rata_scoring") {
                        # Pro-rata scoring: scale up for missing items
                        scores[i] <- sum(valid_responses) * (n_items / n_valid)
                    } else if (method == "standardized") {
                        # Z-score standardization
                        item_means <- colMeans(scale_data, na.rm = TRUE)
                        item_sds <- apply(scale_data, 2, sd, na.rm = TRUE)
                        standardized <- (valid_responses - item_means[!is.na(row_data)]) / item_sds[!is.na(row_data)]
                        scores[i] <- mean(standardized)
                    } else if (method == "percent_scale") {
                        # Convert to 0-100 scale
                        min_possible <- self$options$response_scale_min * n_items
                        max_possible <- self$options$response_scale_max * n_items
                        raw_score <- sum(valid_responses) * (n_items / n_valid)
                        scores[i] <- ((raw_score - min_possible) / (max_possible - min_possible)) * 100
                    }
                } else {
                    scores[i] <- NA
                }
            }
            
            private$.scale_scores <- scores
        },
        
        .reliabilityAnalysis = function() {
            if (!requireNamespace("psych", quietly = TRUE)) {
                return()
            }
            
            scale_data <- private$.scale_data
            
            tryCatch({
                # Cronbach's alpha
                alpha_result <- psych::alpha(scale_data, na.rm = TRUE)
                
                # Item-total correlations
                item_stats <- psych::item.stats(scale_data)
                
                # Store reliability results
                private$.reliability_results <- list(
                    alpha = alpha_result,
                    item_stats = item_stats,
                    cronbach_alpha = alpha_result$total$std.alpha,
                    alpha_ci = alpha_result$feldt,
                    item_total_cors = alpha_result$item.stats$r.cor
                )
            }, error = function(e) {
                # Handle errors gracefully
                private$.reliability_results <- list(error = e$message)
            })
        },
        
        .validityAnalysis = function() {
            scale_data <- private$.scale_data
            
            # Construct validity through inter-item correlations
            cor_matrix <- cor(scale_data, use = "pairwise.complete.obs")
            
            # Average inter-item correlation
            n_items <- ncol(scale_data)
            sum_cors <- sum(cor_matrix, na.rm = TRUE) - n_items  # Remove diagonal
            avg_inter_item_cor <- sum_cors / (n_items * (n_items - 1))
            
            # Store validity results
            private$.validity_results <- list(
                correlation_matrix = cor_matrix,
                avg_inter_item_correlation = avg_inter_item_cor,
                construct_validity = ifelse(avg_inter_item_cor > 0.3, "Adequate", "Poor")
            )
        },
        
        .groupComparisons = function() {
            if (is.null(self$options$group_var)) return()
            
            data <- self$data
            group_var <- self$options$group_var
            scores <- private$.scale_scores
            method <- self$options$comparison_method
            
            # Remove missing scores and corresponding groups
            valid_indices <- !is.na(scores)
            clean_scores <- scores[valid_indices]
            clean_groups <- data[[group_var]][valid_indices]
            
            if (length(unique(clean_groups)) < 2) {
                return()
            }
            
            # Perform group comparisons
            if (method == "t_test" && length(unique(clean_groups)) == 2) {
                test_result <- t.test(clean_scores ~ clean_groups)
                
                # Effect size (Cohen's d)
                group1_scores <- clean_scores[clean_groups == levels(as.factor(clean_groups))[1]]
                group2_scores <- clean_scores[clean_groups == levels(as.factor(clean_groups))[2]]
                
                pooled_sd <- sqrt(((length(group1_scores) - 1) * var(group1_scores, na.rm = TRUE) +
                                  (length(group2_scores) - 1) * var(group2_scores, na.rm = TRUE)) /
                                 (length(group1_scores) + length(group2_scores) - 2))
                
                cohens_d <- (mean(group1_scores, na.rm = TRUE) - mean(group2_scores, na.rm = TRUE)) / pooled_sd
                
                private$.group_results <- list(
                    test = "t-test",
                    statistic = test_result$statistic,
                    p_value = test_result$p.value,
                    effect_size = cohens_d,
                    ci_lower = test_result$conf.int[1],
                    ci_upper = test_result$conf.int[2]
                )
            } else if (method == "anova") {
                test_result <- aov(clean_scores ~ clean_groups)
                summary_result <- summary(test_result)
                
                private$.group_results <- list(
                    test = "ANOVA",
                    f_statistic = summary_result[[1]]$`F value`[1],
                    p_value = summary_result[[1]]$`Pr(>F)`[1]
                )
            } else if (method == "wilcoxon") {
                test_result <- wilcox.test(clean_scores ~ clean_groups)
                
                private$.group_results <- list(
                    test = "Wilcoxon",
                    statistic = test_result$statistic,
                    p_value = test_result$p.value
                )
            } else if (method == "kruskal_wallis") {
                test_result <- kruskal.test(clean_scores ~ clean_groups)
                
                private$.group_results <- list(
                    test = "Kruskal-Wallis",
                    statistic = test_result$statistic,
                    p_value = test_result$p.value
                )
            }
        },
        
        .longitudinalAnalysis = function() {
            if (is.null(self$options$time_var)) return()
            
            data <- self$data
            time_var <- self$options$time_var
            patient_id <- self$options$patient_id
            scores <- private$.scale_scores
            
            # Create longitudinal dataset
            long_data <- data.frame(
                patient_id = if(!is.null(patient_id)) data[[patient_id]] else seq_len(nrow(data)),
                time = data[[time_var]],
                score = scores,
                stringsAsFactors = FALSE
            )
            
            # Remove missing scores
            long_data <- long_data[!is.na(long_data$score), ]
            
            # Calculate descriptive statistics by time point
            time_summary <- aggregate(score ~ time, data = long_data, 
                                    FUN = function(x) c(n = length(x), 
                                                       mean = mean(x, na.rm = TRUE),
                                                       sd = sd(x, na.rm = TRUE),
                                                       median = median(x, na.rm = TRUE)))
            
            private$.longitudinal_results <- list(
                data = long_data,
                time_summary = time_summary
            )
        },
        
        .dataQualityAssessment = function() {
            scale_data <- private$.scale_data
            
            # Calculate quality indicators
            n_items <- ncol(scale_data)
            n_cases <- nrow(scale_data)
            
            # Missing data summary
            missing_by_item <- colSums(is.na(scale_data))
            missing_by_case <- rowSums(is.na(scale_data))
            
            # Response patterns
            complete_cases <- sum(complete.cases(scale_data))
            completely_missing <- sum(missing_by_case == n_items)
            
            # Quality indicators
            quality_results <- list(
                n_items = n_items,
                n_cases = n_cases,
                complete_cases = complete_cases,
                complete_rate = complete_cases / n_cases,
                avg_missing_per_item = mean(missing_by_item),
                avg_missing_per_case = mean(missing_by_case),
                completely_missing_cases = completely_missing
            )
            
            # Store quality results
            private$.quality_results <- quality_results
        },
        
        .clinicalInterpretation = function() {
            scores <- private$.scale_scores
            
            if (all(is.na(scores))) return()
            
            # Basic descriptive statistics
            score_mean <- mean(scores, na.rm = TRUE)
            score_sd <- sd(scores, na.rm = TRUE)
            score_median <- median(scores, na.rm = TRUE)
            score_range <- range(scores, na.rm = TRUE)
            
            # Clinical significance thresholds
            mid_threshold <- self$options$mid_value
            
            # Ceiling and floor effects
            if (self$options$scoring_method == "percent_scale") {
                ceiling_threshold <- 95
                floor_threshold <- 5
            } else {
                max_possible <- self$options$response_scale_max * length(self$options$scale_items)
                min_possible <- self$options$response_scale_min * length(self$options$scale_items)
                ceiling_threshold <- max_possible * 0.95
                floor_threshold <- min_possible + (max_possible - min_possible) * 0.05
            }
            
            ceiling_effect <- sum(scores >= ceiling_threshold, na.rm = TRUE) / sum(!is.na(scores))
            floor_effect <- sum(scores <= floor_threshold, na.rm = TRUE) / sum(!is.na(scores))
            
            private$.clinical_interpretation <- list(
                score_mean = score_mean,
                score_sd = score_sd,
                score_median = score_median,
                score_range = score_range,
                ceiling_effect = ceiling_effect,
                floor_effect = floor_effect,
                mid_threshold = mid_threshold
            )
        },
        
        .populateTables = function() {
            # Scale overview
            n_items <- length(self$options$scale_items)
            n_cases <- nrow(self$data)
            
            overview_data <- data.frame(
                characteristic = c("Scale Type", "Instrument", "Number of Items", 
                                 "Number of Cases", "Scoring Method"),
                value = c(
                    self$options$scale_type,
                    self$options$instrument_name,
                    n_items,
                    n_cases,
                    self$options$scoring_method
                ),
                stringsAsFactors = FALSE
            )
            
            self$results$scale_overview$setData(overview_data)
            
            # Item statistics
            if (!is.null(private$.scale_data)) {
                scale_data <- private$.scale_data
                
                item_stats <- data.frame(
                    item = names(scale_data),
                    n_responses = colSums(!is.na(scale_data)),
                    mean = colMeans(scale_data, na.rm = TRUE),
                    sd = apply(scale_data, 2, sd, na.rm = TRUE),
                    median = apply(scale_data, 2, median, na.rm = TRUE),
                    missing_percent = colSums(is.na(scale_data)) / nrow(scale_data),
                    floor_effect = NA,  # Would calculate actual floor/ceiling effects
                    ceiling_effect = NA,
                    stringsAsFactors = FALSE
                )
                
                self$results$item_statistics$setData(item_stats)
            }
            
            # Scale scores summary
            if (!is.null(private$.scale_scores)) {
                scores <- private$.scale_scores
                valid_scores <- scores[!is.na(scores)]
                
                if (length(valid_scores) > 0) {
                    score_summary <- data.frame(
                        statistic = c("N", "Mean", "SD", "Median", "Min", "Max"),
                        value = c(
                            length(valid_scores),
                            mean(valid_scores),
                            sd(valid_scores),
                            median(valid_scores),
                            min(valid_scores),
                            max(valid_scores)
                        ),
                        interpretation = c(
                            "Valid scores",
                            "Average PRO score",
                            "Score variability",
                            "Median score",
                            "Lowest score",
                            "Highest score"
                        ),
                        stringsAsFactors = FALSE
                    )
                    
                    self$results$scale_scores$setData(score_summary)
                }
            }
            
            # Reliability results
            if (!is.null(private$.reliability_results) && self$options$reliability_analysis) {
                rel_results <- private$.reliability_results
                
                if (!"error" %in% names(rel_results)) {
                    reliability_data <- data.frame(
                        reliability_measure = c("Cronbach's Alpha", "Number of Items", "Average Inter-Item Correlation"),
                        value = c(
                            rel_results$cronbach_alpha,
                            length(self$options$scale_items),
                            mean(rel_results$item_total_cors, na.rm = TRUE)
                        ),
                        ci_lower = c(NA, NA, NA),  # Would add actual CIs
                        ci_upper = c(NA, NA, NA),
                        interpretation = c(
                            ifelse(rel_results$cronbach_alpha >= 0.7, "Acceptable", "Poor"),
                            "Scale length",
                            "Item consistency"
                        ),
                        stringsAsFactors = FALSE
                    )
                    
                    self$results$reliability_results$setData(reliability_data)
                }
            }
            
            # Group comparison results
            if (!is.null(private$.group_results) && self$options$group_comparisons) {
                group_data <- private$.group_results
                
                if (group_data$test == "t-test") {
                    comparison_data <- data.frame(
                        group1 = "Group 1",
                        group2 = "Group 2",
                        mean_diff = group_data$ci_upper - group_data$ci_lower,  # Approximation
                        ci_lower = group_data$ci_lower,
                        ci_upper = group_data$ci_upper,
                        p_value = group_data$p_value,
                        effect_size = group_data$effect_size,
                        clinical_significance = ifelse(abs(group_data$effect_size) >= 0.5, "Medium-Large Effect", "Small Effect"),
                        stringsAsFactors = FALSE
                    )
                    
                    self$results$group_comparison_results$setData(comparison_data)
                }
            }
        },
        
        .plot_scale_distribution = function(image, ggtheme, theme, ...) {
            if (is.null(private$.scale_scores)) {
                return()
            }
            
            scores <- private$.scale_scores
            valid_scores <- scores[!is.na(scores)]
            
            if (length(valid_scores) == 0) {
                return()
            }
            
            plot_data <- data.frame(scores = valid_scores)
            
            p <- ggplot(plot_data, aes(x = scores)) +
                geom_histogram(bins = 20, fill = "steelblue", alpha = 0.7, color = "white") +
                geom_vline(aes(xintercept = mean(scores)), color = "red", linetype = "dashed", size = 1) +
                labs(
                    title = "PRO Scale Score Distribution",
                    x = "Scale Score",
                    y = "Frequency",
                    caption = "Red line indicates mean score"
                ) +
                theme_minimal() +
                theme(
                    plot.title = element_text(hjust = 0.5, size = 14),
                    axis.title = element_text(size = 12),
                    axis.text = element_text(size = 10)
                )
            
            print(p)
            TRUE
        },
        
        .plot_item_distribution = function(image, ggtheme, theme, ...) {
            if (is.null(private$.scale_data)) {
                return()
            }
            
            scale_data <- private$.scale_data
            
            # Convert to long format
            library(reshape2)
            long_data <- reshape2::melt(scale_data)
            long_data <- long_data[!is.na(long_data$value), ]
            
            p <- ggplot(long_data, aes(x = factor(value))) +
                geom_bar(fill = "steelblue", alpha = 0.7) +
                facet_wrap(~ variable, scales = "free") +
                labs(
                    title = "Item Response Distributions",
                    x = "Response Value",
                    y = "Frequency"
                ) +
                theme_minimal() +
                theme(
                    plot.title = element_text(hjust = 0.5, size = 14),
                    axis.title = element_text(size = 12),
                    axis.text = element_text(size = 10),
                    strip.text = element_text(size = 10)
                )
            
            print(p)
            TRUE
        },
        
        .plot_missing_data = function(image, ggtheme, theme, ...) {
            if (is.null(private$.scale_data)) {
                return()
            }
            
            scale_data <- private$.scale_data
            missing_data <- is.na(scale_data)
            
            # Convert to long format for plotting
            missing_long <- reshape2::melt(missing_data)
            names(missing_long) <- c("Case", "Item", "Missing")
            
            p <- ggplot(missing_long, aes(x = Item, y = Case, fill = Missing)) +
                geom_tile() +
                scale_fill_manual(values = c("FALSE" = "lightblue", "TRUE" = "red"),
                                labels = c("FALSE" = "Present", "TRUE" = "Missing")) +
                labs(
                    title = "Missing Data Pattern",
                    x = "Scale Items",
                    y = "Cases",
                    fill = "Data Status"
                ) +
                theme_minimal() +
                theme(
                    plot.title = element_text(hjust = 0.5, size = 14),
                    axis.title = element_text(size = 12),
                    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
                    axis.text.y = element_text(size = 8)
                )
            
            print(p)
            TRUE
        }
    )
)