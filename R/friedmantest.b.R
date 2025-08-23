#' @title Friedman Test for Non-Parametric Repeated Measures
#' @importFrom R6 R6Class
#' @import jmvcore

friedmantestClass <- R6::R6Class(
    "friedmantestClass",
    inherit = friedmantestBase,
    private = list(
        
        # Initialize analysis
        .init = function() {
            
            if (is.null(self$data) || is.null(self$options$dependent) || 
                is.null(self$options$subject) || is.null(self$options$within)) {
                self$results$instructions$setContent(
                    "<html>
                    <head>
                    <style>
                        .main { margin: 2em 2em 2em 2em; color: #3E4053; }
                        .section { margin-bottom: 2em; }
                        .formula { font-family: monospace; background: #f5f5f5; padding: 10px; margin: 10px 0; }
                    </style>
                    </head>
                    <body>
                    <div class='main'>
                        <h2>Friedman Test for Non-Parametric Repeated Measures</h2>
                        <div class='section'>
                            <h3>Overview</h3>
                            <p>The Friedman test is the non-parametric alternative to repeated measures ANOVA. 
                            It tests whether the median values of a continuous or ordinal variable differ 
                            significantly across multiple related measurements (time points, conditions, or treatments). 
                            This test is appropriate when the assumptions of repeated measures ANOVA are violated.</p>
                        </div>
                        
                        <div class='section'>
                            <h3>Required Input</h3>
                            <ul>
                                <li><strong>Dependent Variable:</strong> Continuous or ordinal variable measured repeatedly</li>
                                <li><strong>Subject ID:</strong> Variable identifying subjects/cases for paired measurements</li>
                                <li><strong>Within-Subjects Factor:</strong> Factor indicating the repeated measure conditions</li>
                            </ul>
                        </div>
                        
                        <div class='section'>
                            <h3>Statistical Framework</h3>
                            <div class='formula'>
                                Friedman Test Statistic:<br/>
                                χ²r = 12/[nk(k+1)] * Σ(Rj)² - 3n(k+1)<br/><br/>
                                
                                Where:<br/>
                                • n = number of subjects<br/>
                                • k = number of conditions/time points<br/>
                                • Rj = sum of ranks for condition j<br/>
                                • Data ranked within each subject across conditions<br/><br/>
                                
                                Under H0: χ²r ~ χ²(k-1)
                            </div>
                        </div>
                        
                        <div class='section'>
                            <h3>Clinical Applications</h3>
                            <ul>
                                <li><strong>Treatment Response:</strong> Compare treatment efficacy across multiple time points</li>
                                <li><strong>Disease Progression:</strong> Monitor symptom severity or biomarker levels over time</li>
                                <li><strong>Diagnostic Comparison:</strong> Compare multiple diagnostic methods on same patients</li>
                                <li><strong>Quality of Life:</strong> Assess changes in patient-reported outcomes over follow-up</li>
                                <li><strong>Laboratory Values:</strong> Track laboratory parameters across treatment phases</li>
                            </ul>
                        </div>
                        
                        <div class='section'>
                            <h3>Advantages over Repeated Measures ANOVA</h3>
                            <ul>
                                <li>No normality assumption required</li>
                                <li>Robust to outliers and skewed distributions</li>
                                <li>Appropriate for ordinal data</li>
                                <li>Less sensitive to missing data patterns</li>
                                <li>Valid for small sample sizes</li>
                            </ul>
                        </div>
                        
                        <div class='section'>
                            <h3>Assumptions</h3>
                            <ul>
                                <li>Related samples (same subjects measured repeatedly)</li>
                                <li>Dependent variable is continuous or ordinal</li>
                                <li>Independence of subjects (but not of repeated measures within subjects)</li>
                                <li>Symmetric distribution of differences between conditions</li>
                            </ul>
                        </div>
                    </div>
                    </body>
                    </html>"
                )
                return()
            }
        },
        
        # Main analysis execution
        .run = function() {
            
            # Validate inputs
            if (!private$.validateInputs()) return()
            
            # Prepare data
            data_prepared <- private$.prepareData()
            if (is.null(data_prepared)) return()
            
            # Perform Friedman test
            friedman_result <- private$.performFriedmanTest(data_prepared)
            if (is.null(friedman_result)) return()
            
            # Populate results tables
            private$.populateDataInfo(data_prepared)
            private$.populateFriedmanTest(friedman_result)
            
            # Optional analyses
            if (self$options$show_descriptives) {
                private$.populateDescriptiveStats(data_prepared, friedman_result)
            }
            
            if (self$options$effect_size) {
                private$.populateEffectSize(friedman_result)
            }
            
            if (self$options$show_ranks) {
                private$.populateRankAnalysis(friedman_result)
            }
            
            if (self$options$posthoc && friedman_result$significant) {
                private$.populatePairwiseComparisons(data_prepared, friedman_result)
            }
            
            if (self$options$show_assumptions) {
                private$.populateAssumptionAssessment(data_prepared, friedman_result)
            }
            
            if (self$options$clinical_interpretation) {
                private$.populateClinicalInterpretation(friedman_result)
            }
            
            # Generate method explanation
            private$.generateMethodExplanation()
        },
        
        # Validate inputs
        .validateInputs = function() {
            
            if (is.null(self$options$dependent)) {
                self$results$instructions$setContent("Please specify a dependent variable.")
                return(FALSE)
            }
            
            if (is.null(self$options$subject)) {
                self$results$instructions$setContent("Please specify a subject ID variable.")
                return(FALSE)
            }
            
            if (is.null(self$options$within)) {
                self$results$instructions$setContent("Please specify a within-subjects factor.")
                return(FALSE)
            }
            
            return(TRUE)
        },
        
        # Prepare data for analysis
        .prepareData = function() {
            
            # Get variables from data
            dep_var <- self$options$dependent
            subj_var <- self$options$subject
            within_var <- self$options$within
            
            if (is.null(self$data[[dep_var]]) || is.null(self$data[[subj_var]]) || 
                is.null(self$data[[within_var]])) {
                return(NULL)
            }
            
            # Create analysis dataset
            analysis_data <- data.frame(
                dependent = as.numeric(self$data[[dep_var]]),
                subject = as.factor(self$data[[subj_var]]),
                within = as.factor(self$data[[within_var]]),
                stringsAsFactors = FALSE
            )
            
            # Remove missing cases
            analysis_data <- analysis_data[complete.cases(analysis_data), ]
            
            if (nrow(analysis_data) == 0) {
                return(NULL)
            }
            
            # Check for balanced design
            subject_counts <- table(analysis_data$subject, analysis_data$within)
            
            # Only include subjects with complete data across all conditions
            complete_subjects <- rownames(subject_counts)[apply(subject_counts > 0, 1, all)]
            analysis_data <- analysis_data[analysis_data$subject %in% complete_subjects, ]
            
            if (nrow(analysis_data) == 0) {
                return(NULL)
            }
            
            # Get final dimensions
            n_subjects <- length(unique(analysis_data$subject))
            n_conditions <- length(unique(analysis_data$within))
            condition_levels <- levels(analysis_data$within)
            
            # Create wide format for ranking
            wide_data <- reshape(analysis_data, 
                               idvar = "subject", 
                               timevar = "within", 
                               direction = "wide")
            
            if (is.null(wide_data)) {
                return(NULL)
            }
            
            result <- list(
                long_data = analysis_data,
                wide_data = wide_data,
                n_subjects = n_subjects,
                n_conditions = n_conditions,
                condition_levels = condition_levels,
                dep_var_name = dep_var,
                subj_var_name = subj_var,
                within_var_name = within_var
            )
            
            return(result)
        },
        
        # Perform Friedman test
        .performFriedmanTest = function(data_prepared) {
            
            tryCatch({
                
                long_data <- data_prepared$long_data
                n <- data_prepared$n_subjects
                k <- data_prepared$n_conditions
                
                # Calculate ranks within each subject
                long_data$ranks <- ave(long_data$dependent, 
                                     long_data$subject,
                                     FUN = function(x) rank(x, ties.method = "average"))
                
                # Calculate rank sums for each condition
                rank_sums <- aggregate(ranks ~ within, data = long_data, sum)
                colnames(rank_sums) <- c("condition", "rank_sum")
                rank_sums$mean_rank <- rank_sums$rank_sum / n
                
                # Calculate Friedman test statistic
                # χ²r = 12/[nk(k+1)] * Σ(Rj)² - 3n(k+1)
                sum_rank_squares <- sum(rank_sums$rank_sum^2)
                friedman_statistic <- (12 / (n * k * (k + 1))) * sum_rank_squares - 3 * n * (k + 1)
                
                df <- k - 1
                
                # Calculate p-value
                if (self$options$method == "asymptotic") {
                    p_value <- pchisq(friedman_statistic, df, lower.tail = FALSE)
                } else if (self$options$method == "exact") {
                    # Use built-in Friedman test for exact p-value (if available)
                    friedman_builtin <- friedman.test(dependent ~ within | subject, data = long_data)
                    p_value <- friedman_builtin$p.value
                    friedman_statistic <- friedman_builtin$statistic
                } else if (self$options$method == "monte_carlo") {
                    p_value <- private$.monteCarlofRiedman(long_data, friedman_statistic)
                } else {
                    p_value <- pchisq(friedman_statistic, df, lower.tail = FALSE)
                }
                
                # Effect size (Kendall's W - coefficient of concordance)
                kendalls_w <- friedman_statistic / (n * (k - 1))
                
                # Descriptive statistics by condition
                desc_stats <- aggregate(dependent ~ within, data = long_data, 
                                      function(x) c(median = median(x),
                                                   q1 = quantile(x, 0.25),
                                                   q3 = quantile(x, 0.75),
                                                   iqr = IQR(x),
                                                   mean = mean(x),
                                                   sd = sd(x)))
                
                # Convert matrix columns to separate columns
                desc_matrix <- desc_stats$dependent
                desc_stats$median <- desc_matrix[, "median"]
                desc_stats$q1 <- desc_matrix[, "q1"]
                desc_stats$q3 <- desc_matrix[, "q3"]
                desc_stats$iqr <- desc_matrix[, "iqr"]
                desc_stats$mean <- desc_matrix[, "mean"]
                desc_stats$sd <- desc_matrix[, "sd"]
                desc_stats$dependent <- NULL
                
                # Add mean ranks to descriptive stats
                desc_stats <- merge(desc_stats, rank_sums, by.x = "within", by.y = "condition")
                
                result <- list(
                    friedman_statistic = friedman_statistic,
                    df = df,
                    p_value = p_value,
                    significant = p_value < self$options$alpha,
                    kendalls_w = kendalls_w,
                    n_subjects = n,
                    n_conditions = k,
                    condition_levels = data_prepared$condition_levels,
                    rank_sums = rank_sums,
                    desc_stats = desc_stats,
                    ranked_data = long_data,
                    expected_rank = (k + 1) / 2
                )
                
                return(result)
                
            }, error = function(e) {
                self$results$instructions$setContent(paste("Error performing Friedman test:", e$message))
                return(NULL)
            })
        },
        
        # Monte Carlo p-value for Friedman test
        .monteCarlofRiedman = function(long_data, observed_statistic) {
            
            n_simulations <- 10000
            count_extreme <- 0
            
            subjects <- unique(long_data$subject)
            conditions <- unique(long_data$within)
            k <- length(conditions)
            
            for (i in 1:n_simulations) {
                # Simulate under null hypothesis (random rankings within subjects)
                sim_ranks <- numeric()
                
                for (subj in subjects) {
                    subj_data <- long_data[long_data$subject == subj, ]
                    n_obs <- nrow(subj_data)
                    sim_ranks <- c(sim_ranks, sample(1:n_obs, n_obs))
                }
                
                sim_data <- long_data
                sim_data$ranks <- sim_ranks
                
                # Calculate simulated test statistic
                sim_rank_sums <- aggregate(ranks ~ within, data = sim_data, sum)
                sum_sim_rank_squares <- sum(sim_rank_sums$ranks^2)
                n <- length(subjects)
                sim_statistic <- (12 / (n * k * (k + 1))) * sum_sim_rank_squares - 3 * n * (k + 1)
                
                if (sim_statistic >= observed_statistic) {
                    count_extreme <- count_extreme + 1
                }
            }
            
            return(count_extreme / n_simulations)
        },
        
        # Populate data info
        .populateDataInfo = function(data_prepared) {
            
            table <- self$results$dataInfo
            
            rows <- list(
                list(characteristic = "Number of Subjects", value = as.character(data_prepared$n_subjects)),
                list(characteristic = "Number of Conditions", value = as.character(data_prepared$n_conditions)),
                list(characteristic = "Total Observations", value = as.character(nrow(data_prepared$long_data))),
                list(characteristic = "Design", value = "Balanced Repeated Measures")
            )
            
            condition_names <- paste(data_prepared$condition_levels, collapse = ", ")
            rows <- append(rows, list(list(characteristic = "Conditions", value = condition_names)))
            
            for (i in seq_along(rows)) {
                table$addRow(rowKey = i, values = rows[[i]])
            }
        },
        
        # Populate Friedman test results
        .populateFriedmanTest = function(friedman_result) {
            
            table <- self$results$friedmanTest
            
            # Determine significance
            significance <- if (friedman_result$significant) {
                paste("Significant (p <", self$options$alpha, ")")
            } else {
                paste("Not significant (p ≥", self$options$alpha, ")")
            }
            
            # Conclusion
            conclusion <- if (friedman_result$significant) {
                "Significant differences exist between conditions"
            } else {
                "No significant differences between conditions"
            }
            
            table$addRow(rowKey = 1, values = list(
                statistic = friedman_result$friedman_statistic,
                df = friedman_result$df,
                p_value = friedman_result$p_value,
                significance = significance,
                conclusion = conclusion
            ))
        },
        
        # Populate descriptive statistics
        .populateDescriptiveStats = function(data_prepared, friedman_result) {
            
            table <- self$results$descriptiveStats
            desc_stats <- friedman_result$desc_stats
            
            for (i in 1:nrow(desc_stats)) {
                table$addRow(rowKey = i, values = list(
                    condition = as.character(desc_stats$within[i]),
                    n = friedman_result$n_subjects,
                    median = desc_stats$median[i],
                    q1 = desc_stats$q1[i],
                    q3 = desc_stats$q3[i],
                    iqr = desc_stats$iqr[i],
                    mean_rank = desc_stats$mean_rank[i]
                ))
            }
        },
        
        # Populate effect size measures
        .populateEffectSize = function(friedman_result) {
            
            table <- self$results$effectSize
            
            # Kendall's W (coefficient of concordance)
            w_value <- friedman_result$kendalls_w
            
            # Confidence interval for Kendall's W (approximate)
            ci_level <- self$options$confidence_level
            z_crit <- qnorm((1 + ci_level) / 2)
            se_w <- sqrt(2 * friedman_result$df / (friedman_result$n_subjects * friedman_result$df^2))
            ci_lower <- max(0, w_value - z_crit * se_w)
            ci_upper <- min(1, w_value + z_crit * se_w)
            ci_text <- sprintf("(%.3f, %.3f)", ci_lower, ci_upper)
            
            # Interpretation of Kendall's W
            if (w_value < 0.1) {
                interpretation <- "Very weak effect"
            } else if (w_value < 0.3) {
                interpretation <- "Small effect"
            } else if (w_value < 0.5) {
                interpretation <- "Medium effect"
            } else if (w_value < 0.7) {
                interpretation <- "Large effect"
            } else {
                interpretation <- "Very large effect"
            }
            
            table$addRow(rowKey = 1, values = list(
                measure = "Kendall's W",
                value = w_value,
                confidence_interval = ci_text,
                interpretation = interpretation
            ))
        },
        
        # Populate rank analysis
        .populateRankAnalysis = function(friedman_result) {
            
            table <- self$results$rankAnalysis
            rank_sums <- friedman_result$rank_sums
            expected_rank <- friedman_result$expected_rank
            
            for (i in 1:nrow(rank_sums)) {
                deviation <- rank_sums$mean_rank[i] - expected_rank
                
                table$addRow(rowKey = i, values = list(
                    condition = as.character(rank_sums$condition[i]),
                    mean_rank = rank_sums$mean_rank[i],
                    rank_sum = rank_sums$rank_sum[i],
                    expected_rank = expected_rank,
                    deviation = deviation
                ))
            }
        },
        
        # Generate method explanation
        .generateMethodExplanation = function() {
            
            method_desc <- switch(self$options$method,
                                 "asymptotic" = "asymptotic chi-square distribution",
                                 "exact" = "exact distribution",
                                 "monte_carlo" = "Monte Carlo simulation",
                                 "asymptotic chi-square distribution")
            
            content <- paste0(
                "<html><body>",
                "<h3>Method: Friedman Test for Non-Parametric Repeated Measures</h3>",
                "<p>This analysis uses the <strong>Friedman test</strong> with <strong>", method_desc, "</strong> ",
                "to test for differences across <strong>", length(self$options$variables), " conditions</strong> ",
                "in a repeated measures design.</p>",
                
                "<h4>Statistical Approach:</h4>",
                "<p>The Friedman test ranks observations within each subject across conditions, then compares ",
                "the sum of ranks for each condition. This approach is robust to outliers and non-normal distributions, ",
                "making it ideal when parametric assumptions are violated.</p>",
                
                "<h4>Clinical Applications:</h4>",
                "<ul>",
                "<li><strong>Treatment Monitoring:</strong> Compare treatment effectiveness across multiple time points</li>",
                "<li><strong>Disease Progression:</strong> Track changes in symptoms or biomarkers over follow-up period</li>",
                "<li><strong>Method Comparison:</strong> Compare multiple diagnostic or measurement methods on same patients</li>",
                "<li><strong>Quality Assessment:</strong> Evaluate patient-reported outcomes across treatment phases</li>",
                "</ul>",
                
                "<h4>Interpretation Guidelines:</h4>",
                "<ul>",
                "<li><strong>Significant Result:</strong> At least one condition differs significantly from others</li>",
                "<li><strong>Effect Size (Kendall's W):</strong> Measures strength of agreement/consistency across conditions</li>",
                "<li><strong>Mean Ranks:</strong> Higher mean ranks indicate higher values for that condition</li>",
                "<li><strong>Post-hoc Testing:</strong> When significant, pairwise tests identify specific differences</li>",
                "</ul>"
            )
            
            if (self$options$posthoc && self$options$correction != "none") {
                content <- paste0(content,
                    "<h4>Multiple Comparison Correction:</h4>",
                    "<p>Post-hoc pairwise comparisons use <strong>", 
                    switch(self$options$correction,
                           "bonferroni" = "Bonferroni",
                           "holm" = "Holm",
                           "fdr" = "False Discovery Rate (Benjamini-Hochberg)",
                           "none" = "no"),
                    "</strong> correction to control for multiple testing.</p>"
                )
            }
            
            content <- paste0(content, "</body></html>")
            
            self$results$methodExplanation$setContent(content)
        },
        
        # Placeholder methods for optional analyses
        .populatePairwiseComparisons = function(data_prepared, friedman_result) {
            # Implementation would perform pairwise post-hoc tests
        },
        
        .populateAssumptionAssessment = function(data_prepared, friedman_result) {
            # Implementation would assess assumptions
        },
        
        .populateClinicalInterpretation = function(friedman_result) {
            # Implementation would provide clinical interpretation
        },
        
        # Plot functions (placeholders)
        .plotBoxplotByCondition = function(image, ...) {
            # Implementation for box plot by condition
        },
        
        .plotMeanRanks = function(image, ...) {
            # Implementation for mean rank plot
        },
        
        .plotPairwiseComparisons = function(image, ...) {
            # Implementation for pairwise comparison matrix
        },
        
        .plotProfiles = function(image, ...) {
            # Implementation for individual profile plot
        }
    )
)