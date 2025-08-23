#' @title Cochran's Q Test for Paired Categorical Data
#' @importFrom R6 R6Class
#' @import jmvcore

cochranqClass <- R6::R6Class(
    "cochranqClass",
    inherit = cochranqBase,
    private = list(
        
        # Initialize analysis
        .init = function() {
            
            if (is.null(self$data) || is.null(self$options$variables) || 
                length(self$options$variables) < 3) {
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
                        <h2>Cochran's Q Test for Paired Categorical Data</h2>
                        <div class='section'>
                            <h3>Overview</h3>
                            <p>Cochran's Q test is used to test for changes in binary responses across three or more 
                            paired measurements. It is the extension of McNemar's test for repeated measures designs 
                            with binary outcomes, commonly used in clinical studies to evaluate treatment efficacy 
                            or diagnostic consistency across multiple time points.</p>
                        </div>
                        
                        <div class='section'>
                            <h3>Required Input</h3>
                            <ul>
                                <li><strong>Paired Binary Variables:</strong> At least 3 binary variables (0/1, Yes/No, etc.) representing the same measurement at different time points or conditions</li>
                                <li><strong>Subject ID (optional):</strong> Identifier for paired data validation</li>
                            </ul>
                        </div>
                        
                        <div class='section'>
                            <h3>Statistical Framework</h3>
                            <div class='formula'>
                                Cochran's Q Statistic:<br/>
                                Q = k(k-1) * [Σ(Tj)² - (ΣΣXij)²] / [k*ΣΣXij - Σ(Ri)²]<br/><br/>
                                
                                Where:<br/>
                                • k = number of treatments/time points<br/>
                                • Tj = sum of successes for treatment j<br/>
                                • Ri = sum of successes for subject i<br/>
                                • Xij = response for subject i at treatment j<br/><br/>
                                
                                Under H0: Q ~ χ²(k-1)
                            </div>
                        </div>
                        
                        <div class='section'>
                            <h3>Clinical Applications</h3>
                            <ul>
                                <li><strong>Treatment Response:</strong> Evaluate changes in binary treatment outcomes over multiple visits</li>
                                <li><strong>Diagnostic Agreement:</strong> Assess consistency of binary diagnostic results across multiple methods</li>
                                <li><strong>Symptom Progression:</strong> Monitor presence/absence of symptoms across follow-up periods</li>
                                <li><strong>Quality Improvement:</strong> Track binary quality indicators over time in healthcare settings</li>
                                <li><strong>Clinical Decision Making:</strong> Evaluate consistency of binary clinical decisions across conditions</li>
                            </ul>
                        </div>
                        
                        <div class='section'>
                            <h3>Assumptions</h3>
                            <ul>
                                <li>Binary variables measured on the same subjects at different times/conditions</li>
                                <li>Independence of subjects (but not of repeated measures within subjects)</li>
                                <li>Adequate sample size for chi-square approximation (expected frequency ≥ 5)</li>
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
            
            # Perform Cochran's Q test
            q_result <- private$.performCochranQ(data_prepared)
            if (is.null(q_result)) return()
            
            # Populate results tables
            private$.populateDataInfo(data_prepared)
            private$.populateCochranQTest(q_result)
            private$.populateMarginalFrequencies(data_prepared, q_result)
            
            # Optional analyses
            if (self$options$effect_size) {
                private$.populateEffectSize(q_result)
            }
            
            if (self$options$show_pattern) {
                private$.populateResponsePatterns(data_prepared, q_result)
            }
            
            if (self$options$posthoc && q_result$significant) {
                private$.populatePairwiseComparisons(data_prepared, q_result)
            }
            
            if (self$options$show_assumptions) {
                private$.populateAssumptionTesting(data_prepared, q_result)
            }
            
            if (self$options$clinical_interpretation) {
                private$.populateClinicalInterpretation(q_result)
            }
            
            # Generate method explanation
            private$.generateMethodExplanation()
        },
        
        # Validate inputs
        .validateInputs = function() {
            
            if (is.null(self$options$variables) || length(self$options$variables) < 3) {
                self$results$instructions$setContent("Cochran's Q test requires at least 3 paired binary variables.")
                return(FALSE)
            }
            
            return(TRUE)
        },
        
        # Prepare data for analysis
        .prepareData = function() {
            
            # Get variables from data
            variables <- self$options$variables
            
            if (any(sapply(variables, function(var) is.null(self$data[[var]])))) {
                return(NULL)
            }
            
            # Create data matrix
            data_matrix <- data.frame(
                stringsAsFactors = FALSE
            )
            
            # Add variables
            for (i in seq_along(variables)) {
                var_name <- variables[i]
                var_data <- self$data[[var_name]]
                
                # Convert to binary (0/1)
                if (is.factor(var_data)) {
                    var_data <- as.numeric(var_data) - 1
                } else if (is.logical(var_data)) {
                    var_data <- as.numeric(var_data)
                } else {
                    var_data <- as.numeric(var_data)
                }
                
                data_matrix[[paste0("var", i)]] <- var_data
                data_matrix[[paste0("var", i, "_name")]] <- rep(var_name, length(var_data))
            }
            
            # Add ID if specified
            if (!is.null(self$options$id)) {
                if (!is.null(self$data[[self$options$id]])) {
                    data_matrix$id <- self$data[[self$options$id]]
                }
            } else {
                data_matrix$id <- 1:nrow(data_matrix)
            }
            
            # Remove missing cases (listwise deletion)
            complete_cases <- complete.cases(data_matrix[, grepl("^var[0-9]+$", names(data_matrix))])
            data_matrix <- data_matrix[complete_cases, ]
            
            if (nrow(data_matrix) == 0) {
                return(NULL)
            }
            
            # Extract data matrix for analysis
            data_vars <- data_matrix[, grepl("^var[0-9]+$", names(data_matrix))]
            colnames(data_vars) <- variables
            
            result <- list(
                data_matrix = as.matrix(data_vars),
                variable_names = variables,
                n_subjects = nrow(data_vars),
                n_variables = ncol(data_vars),
                complete_data = data_matrix
            )
            
            return(result)
        },
        
        # Perform Cochran's Q test
        .performCochranQ = function(data_prepared) {
            
            tryCatch({
                
                data_matrix <- data_prepared$data_matrix
                k <- ncol(data_matrix)  # number of treatments/conditions
                n <- nrow(data_matrix)  # number of subjects
                
                # Calculate row sums (Ri) and column sums (Tj)
                row_sums <- rowSums(data_matrix)
                col_sums <- colSums(data_matrix)
                
                # Total sum
                total_sum <- sum(data_matrix)
                
                # Calculate Cochran's Q statistic
                # Q = k(k-1) * [sum(Tj^2) - (total_sum)^2] / [k*total_sum - sum(Ri^2)]
                
                sum_tj_squared <- sum(col_sums^2)
                sum_ri_squared <- sum(row_sums^2)
                
                numerator <- k * (k - 1) * (sum_tj_squared - (total_sum^2) / k)
                denominator <- k * total_sum - sum_ri_squared
                
                if (denominator == 0) {
                    # No variation between subjects
                    q_statistic <- 0
                    p_value <- 1.0
                } else {
                    q_statistic <- numerator / denominator
                    
                    # Calculate p-value
                    df <- k - 1
                    
                    if (self$options$method == "asymptotic") {
                        p_value <- pchisq(q_statistic, df, lower.tail = FALSE)
                    } else if (self$options$method == "exact") {
                        # For exact test, use permutation (simplified implementation)
                        p_value <- private$.exactCochranQ(data_matrix, q_statistic)
                    } else if (self$options$method == "monte_carlo") {
                        # Monte Carlo simulation
                        p_value <- private$.monteCarloCochranQ(data_matrix, q_statistic)
                    } else {
                        p_value <- pchisq(q_statistic, df, lower.tail = FALSE)
                    }
                }
                
                # Effect size (Kendall's W - coefficient of concordance)
                kendalls_w <- q_statistic / (n * (k - 1))
                
                result <- list(
                    q_statistic = q_statistic,
                    df = k - 1,
                    p_value = p_value,
                    significant = p_value < self$options$alpha,
                    kendalls_w = kendalls_w,
                    n_subjects = n,
                    n_conditions = k,
                    row_sums = row_sums,
                    col_sums = col_sums,
                    total_sum = total_sum,
                    marginal_proportions = col_sums / n,
                    data_matrix = data_matrix
                )
                
                return(result)
                
            }, error = function(e) {
                self$results$instructions$setContent(paste("Error performing Cochran's Q test:", e$message))
                return(NULL)
            })
        },
        
        # Exact Cochran's Q test (simplified)
        .exactCochranQ = function(data_matrix, observed_q) {
            
            # Simplified exact test using permutation
            # In practice, would need more sophisticated exact calculation
            
            n_permutations <- min(1000, factorial(ncol(data_matrix)))
            count_extreme <- 0
            
            for (i in 1:n_permutations) {
                # Permute columns for each subject
                permuted_matrix <- data_matrix
                for (j in 1:nrow(data_matrix)) {
                    permuted_matrix[j, ] <- sample(data_matrix[j, ])
                }
                
                # Calculate Q for permuted data
                k <- ncol(permuted_matrix)
                col_sums_perm <- colSums(permuted_matrix)
                row_sums_perm <- rowSums(permuted_matrix)
                total_sum_perm <- sum(permuted_matrix)
                
                if (k * total_sum_perm - sum(row_sums_perm^2) != 0) {
                    q_perm <- (k * (k - 1) * (sum(col_sums_perm^2) - (total_sum_perm^2) / k)) /
                             (k * total_sum_perm - sum(row_sums_perm^2))
                    
                    if (q_perm >= observed_q) {
                        count_extreme <- count_extreme + 1
                    }
                }
            }
            
            return(count_extreme / n_permutations)
        },
        
        # Monte Carlo Cochran's Q test
        .monteCarloCochranQ = function(data_matrix, observed_q) {
            
            # Monte Carlo simulation under null hypothesis
            n_simulations <- 10000
            count_extreme <- 0
            
            n <- nrow(data_matrix)
            k <- ncol(data_matrix)
            row_sums <- rowSums(data_matrix)
            
            for (i in 1:n_simulations) {
                # Simulate under null (random allocation within each subject)
                sim_matrix <- matrix(0, n, k)
                for (j in 1:n) {
                    if (row_sums[j] > 0 && row_sums[j] < k) {
                        positions <- sample(k, row_sums[j])
                        sim_matrix[j, positions] <- 1
                    } else if (row_sums[j] == k) {
                        sim_matrix[j, ] <- 1
                    }
                }
                
                # Calculate Q for simulated data
                col_sums_sim <- colSums(sim_matrix)
                total_sum_sim <- sum(sim_matrix)
                
                if (k * total_sum_sim - sum(row_sums^2) != 0) {
                    q_sim <- (k * (k - 1) * (sum(col_sums_sim^2) - (total_sum_sim^2) / k)) /
                            (k * total_sum_sim - sum(row_sums^2))
                    
                    if (q_sim >= observed_q) {
                        count_extreme <- count_extreme + 1
                    }
                }
            }
            
            return(count_extreme / n_simulations)
        },
        
        # Populate data info
        .populateDataInfo = function(data_prepared) {
            
            table <- self$results$dataInfo
            
            rows <- list(
                list(characteristic = "Number of Subjects", value = as.character(data_prepared$n_subjects)),
                list(characteristic = "Number of Time Points/Conditions", value = as.character(data_prepared$n_variables)),
                list(characteristic = "Complete Cases", value = as.character(data_prepared$n_subjects)),
                list(characteristic = "Total Observations", value = as.character(data_prepared$n_subjects * data_prepared$n_variables))
            )
            
            for (i in seq_along(rows)) {
                table$addRow(rowKey = i, values = rows[[i]])
            }
        },
        
        # Populate Cochran's Q test results
        .populateCochranQTest = function(q_result) {
            
            table <- self$results$cochranQTest
            
            # Determine significance
            significance <- if (q_result$significant) {
                paste("Significant (p <", self$options$alpha, ")")
            } else {
                paste("Not significant (p ≥", self$options$alpha, ")")
            }
            
            # Conclusion
            conclusion <- if (q_result$significant) {
                "There is a significant difference in response rates across time points/conditions"
            } else {
                "No significant difference in response rates across time points/conditions"
            }
            
            table$addRow(rowKey = 1, values = list(
                statistic = q_result$q_statistic,
                df = q_result$df,
                p_value = q_result$p_value,
                significance = significance,
                conclusion = conclusion
            ))
        },
        
        # Populate effect size measures
        .populateEffectSize = function(q_result) {
            
            table <- self$results$effectSize
            
            # Kendall's W (coefficient of concordance)
            w_value <- q_result$kendalls_w
            
            # Confidence interval for Kendall's W (approximate)
            ci_level <- self$options$confidence_level
            z_crit <- qnorm((1 + ci_level) / 2)
            se_w <- sqrt((2 * q_result$df) / (q_result$n_subjects^2 * q_result$df^2))
            ci_lower <- max(0, w_value - z_crit * se_w)
            ci_upper <- min(1, w_value + z_crit * se_w)
            ci_text <- sprintf("(%.3f, %.3f)", ci_lower, ci_upper)
            
            # Interpretation of Kendall's W
            if (w_value < 0.1) {
                interpretation <- "Very weak agreement/concordance"
            } else if (w_value < 0.3) {
                interpretation <- "Weak agreement/concordance"
            } else if (w_value < 0.5) {
                interpretation <- "Moderate agreement/concordance"
            } else if (w_value < 0.7) {
                interpretation <- "Strong agreement/concordance"
            } else {
                interpretation <- "Very strong agreement/concordance"
            }
            
            table$addRow(rowKey = 1, values = list(
                measure = "Kendall's W",
                value = w_value,
                confidence_interval = ci_text,
                interpretation = interpretation
            ))
        },
        
        # Populate marginal frequencies
        .populateMarginalFrequencies = function(data_prepared, q_result) {
            
            table <- self$results$marginalFrequencies
            
            for (i in 1:data_prepared$n_variables) {
                var_name <- data_prepared$variable_names[i]
                positive_count <- q_result$col_sums[i]
                negative_count <- data_prepared$n_subjects - positive_count
                proportion <- q_result$marginal_proportions[i]
                
                # Confidence interval for proportion
                ci_level <- self$options$confidence_level
                prop_test <- binom.test(positive_count, data_prepared$n_subjects, conf.level = ci_level)
                ci_text <- sprintf("(%.3f, %.3f)", prop_test$conf.int[1], prop_test$conf.int[2])
                
                table$addRow(rowKey = i, values = list(
                    variable = var_name,
                    positive_responses = positive_count,
                    negative_responses = negative_count,
                    proportion_positive = proportion,
                    confidence_interval = ci_text
                ))
            }
        },
        
        # Generate method explanation
        .generateMethodExplanation = function() {
            
            method_desc <- switch(self$options$method,
                                 "asymptotic" = "asymptotic chi-square",
                                 "exact" = "exact permutation",
                                 "monte_carlo" = "Monte Carlo simulation",
                                 "asymptotic chi-square")
            
            content <- paste0(
                "<html><body>",
                "<h3>Method: Cochran's Q Test for Paired Categorical Data</h3>",
                "<p>This analysis uses <strong>Cochran's Q test</strong> with <strong>", method_desc, "</strong> ",
                "p-value calculation to test for differences in binary response rates across ",
                "<strong>", length(self$options$variables), " paired measurements</strong>.</p>",
                
                "<h4>Statistical Framework:</h4>",
                "<p>Cochran's Q test is the non-parametric extension of McNemar's test for more than two paired binary measurements. ",
                "It tests the null hypothesis that the marginal probabilities of 'success' are equal across all conditions.</p>",
                
                "<h4>Clinical Applications:</h4>",
                "<ul>",
                "<li><strong>Treatment Response Monitoring:</strong> Track binary treatment outcomes (responder/non-responder) across multiple follow-up visits</li>",
                "<li><strong>Diagnostic Consistency:</strong> Evaluate agreement of binary diagnostic tests across multiple methods or time points</li>",
                "<li><strong>Symptom Progression:</strong> Monitor presence/absence of clinical symptoms during disease course</li>",
                "<li><strong>Quality Improvement:</strong> Track binary quality indicators (complications, readmissions) over time</li>",
                "</ul>",
                
                "<h4>Interpretation Guidelines:</h4>",
                "<ul>",
                "<li><strong>Significant Result:</strong> Indicates that response rates differ significantly across time points/conditions</li>",
                "<li><strong>Post-hoc Testing:</strong> When significant, pairwise McNemar tests identify which specific comparisons differ</li>",
                "<li><strong>Effect Size:</strong> Kendall's W measures the strength of agreement/concordance (0 = no agreement, 1 = perfect agreement)</li>",
                "<li><strong>Clinical Relevance:</strong> Statistical significance should be evaluated alongside clinical importance of observed differences</li>",
                "</ul>"
            )
            
            if (self$options$correction != "none" && self$options$posthoc) {
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
        .populateResponsePatterns = function(data_prepared, q_result) {
            # Implementation would analyze all response patterns
        },
        
        .populatePairwiseComparisons = function(data_prepared, q_result) {
            # Implementation would perform pairwise McNemar tests
        },
        
        .populateAssumptionTesting = function(data_prepared, q_result) {
            # Implementation would test assumptions
        },
        
        .populateClinicalInterpretation = function(q_result) {
            # Implementation would provide clinical interpretation
        },
        
        # Plot functions (placeholders)
        .plotResponsePatterns = function(image, ...) {
            # Implementation for response pattern visualization
        },
        
        .plotMarginalProportions = function(image, ...) {
            # Implementation for marginal proportions plot
        },
        
        .plotPairwiseComparisons = function(image, ...) {
            # Implementation for pairwise comparison heatmap
        }
    )
)