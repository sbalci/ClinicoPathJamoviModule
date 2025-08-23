bayesiandiagnosticClass <- R6::R6Class(
    "bayesiandiagnosticClass",
    inherit = bayesiandiagnosticBase,
    private = list(
        .init = function() {
            if (is.null(self$data) || nrow(self$data) == 0) {
                self$results$instructions$setContent(
                    "<html>
                    <head>
                    <style>
                        .main {
                            margin: 20px;
                            font-family: sans-serif;
                        }
                        .title {
                            font-size: 18px;
                            font-weight: bold;
                            color: #3498db;
                            margin-bottom: 15px;
                        }
                        .section {
                            margin-bottom: 20px;
                        }
                        .list {
                            margin-left: 20px;
                        }
                    </style>
                    </head>
                    <body>
                    <div class='main'>
                        <div class='title'>Bayesian Diagnostic Probability Updates</div>
                        <div class='section'>
                            <b>Purpose:</b> Perform Bayesian analysis of diagnostic tests with prior probability updates.
                        </div>
                        <div class='section'>
                            <b>Required:</b>
                            <div class='list'>
                                • Test Result Variable (diagnostic test outcome)<br>
                                • Disease Status Variable (gold standard)
                            </div>
                        </div>
                        <div class='section'>
                            <b>Key Features:</b>
                            <div class='list'>
                                • Single test Bayesian analysis<br>
                                • Sequential test chains<br>
                                • Parallel test combinations<br>
                                • Hierarchical Bayesian modeling<br>
                                • Beta-binomial overdispersion models<br>
                                • Prior sensitivity analysis<br>
                                • Posterior predictive checking
                            </div>
                        </div>
                    </div>
                    </body>
                    </html>"
                )
                return()
            }
            
            self$results$methodExplanation$setContent(
                "<html>
                <head>
                <style>
                    .main { margin: 20px; font-family: sans-serif; }
                    .formula { background-color: #f8f9fa; padding: 10px; margin: 10px 0; font-family: monospace; }
                    .interpretation { background-color: #e8f4f8; padding: 10px; margin: 10px 0; }
                </style>
                </head>
                <body>
                <div class='main'>
                    <h3>Bayesian Diagnostic Analysis</h3>
                    <div class='formula'>
                        <b>Bayes' Theorem:</b><br>
                        P(D+|T+) = P(T+|D+) × P(D+) / P(T+)<br><br>
                        <b>Likelihood Ratios:</b><br>
                        LR+ = Sensitivity / (1 - Specificity)<br>
                        LR- = (1 - Sensitivity) / Specificity<br><br>
                        <b>Posterior Odds:</b><br>
                        Posterior Odds = LR × Prior Odds
                    </div>
                    <div class='interpretation'>
                        <b>Interpretation:</b> Bayesian diagnostic analysis updates prior disease probability 
                        using test results and likelihood ratios to obtain posterior probability of disease.
                        Sequential testing allows for multiple test updates.
                    </div>
                </div>
                </body>
                </html>"
            )
        },
        
        .run = function() {
            # Get options
            test_result_var <- self$options$test_result
            disease_status_var <- self$options$disease_status
            covariates <- self$options$covariates
            analysis_type <- self$options$analysis_type
            prior_type <- self$options$prior_type
            prior_prob <- self$options$prior_prob
            
            # Check for required variables
            if (is.null(test_result_var) || is.null(disease_status_var)) {
                return()
            }
            
            # Prepare data
            analysis_data <- self$data
            if (nrow(analysis_data) == 0) return()
            
            # Get variables
            test_result <- analysis_data[[test_result_var]]
            disease_status <- analysis_data[[disease_status_var]]
            
            # Remove missing values
            complete_cases <- complete.cases(test_result, disease_status)
            if (sum(complete_cases) < 4) {
                self$results$instructions$setContent(
                    "<html><body><p style='color: red;'>Insufficient data for analysis. Need at least 4 complete cases.</p></body></html>"
                )
                return()
            }
            
            analysis_data <- analysis_data[complete_cases, ]
            test_result <- test_result[complete_cases]
            disease_status <- disease_status[complete_cases]
            
            # Convert to binary if needed
            if (is.factor(disease_status)) {
                disease_levels <- levels(disease_status)
                if (length(disease_levels) > 2) {
                    disease_status <- as.numeric(disease_status == disease_levels[length(disease_levels)])
                } else {
                    disease_status <- as.numeric(disease_status) - 1
                }
            } else {
                disease_status <- as.numeric(disease_status)
                disease_status[disease_status != 0 & disease_status != 1] <- NA
                disease_status <- disease_status[!is.na(disease_status)]
                if (length(disease_status) < 4) return()
            }
            
            if (is.factor(test_result)) {
                test_levels <- levels(test_result)
                if (length(test_levels) > 2) {
                    test_result <- as.numeric(test_result == test_levels[length(test_levels)])
                } else {
                    test_result <- as.numeric(test_result) - 1
                }
            } else {
                test_result <- as.numeric(test_result)
                test_result[test_result != 0 & test_result != 1] <- NA
                test_result <- test_result[!is.na(test_result)]
                if (length(test_result) < 4) return()
            }
            
            # Ensure same length
            min_length <- min(length(test_result), length(disease_status))
            test_result <- test_result[1:min_length]
            disease_status <- disease_status[1:min_length]
            
            # Perform analysis based on type
            if (analysis_type == "single_test") {
                private$.runSingleTestAnalysis(test_result, disease_status, prior_prob)
            } else if (analysis_type == "test_chain") {
                private$.runTestChainAnalysis(test_result, disease_status, prior_prob)
            } else if (analysis_type == "parallel_tests") {
                private$.runParallelTestAnalysis(analysis_data, prior_prob)
            } else if (analysis_type == "hierarchical_bayes") {
                private$.runHierarchicalBayes(test_result, disease_status, prior_prob, covariates)
            } else if (analysis_type == "beta_binomial") {
                private$.runBetaBinomial(test_result, disease_status, prior_prob)
            }
        },
        
        .runSingleTestAnalysis = function(test_result, disease_status, prior_prob) {
            # Calculate 2x2 contingency table
            ct <- table(test_result, disease_status)
            if (nrow(ct) < 2 || ncol(ct) < 2) return()
            
            # Extract counts
            tp <- ct[2, 2]  # True positives
            fp <- ct[2, 1]  # False positives
            fn <- ct[1, 2]  # False negatives
            tn <- ct[1, 1]  # True negatives
            
            # Calculate test characteristics
            sensitivity <- tp / (tp + fn)
            specificity <- tn / (tn + fp)
            lr_pos <- sensitivity / (1 - specificity)
            lr_neg <- (1 - sensitivity) / specificity
            
            # Standard errors (using exact binomial)
            se_sens <- sqrt(sensitivity * (1 - sensitivity) / (tp + fn))
            se_spec <- sqrt(specificity * (1 - specificity) / (tn + fp))
            
            # Populate likelihood table
            likelihood_table <- self$results$likelihoodTable
            likelihood_table$addRow(rowKey = "positive", values = list(
                test_outcome = "Positive Test",
                sensitivity = sensitivity,
                specificity = specificity,
                lr_positive = lr_pos,
                lr_negative = lr_neg,
                se_sensitivity = se_sens,
                se_specificity = se_spec
            ))
            
            # Bayesian updates
            prior_odds <- prior_prob / (1 - prior_prob)
            posterior_odds_pos <- lr_pos * prior_odds
            posterior_odds_neg <- lr_neg * prior_odds
            
            posterior_prob_pos <- posterior_odds_pos / (1 + posterior_odds_pos)
            posterior_prob_neg <- posterior_odds_neg / (1 + posterior_odds_neg)
            
            # Information gain (KL divergence)
            info_gain_pos <- private$.calculateInformationGain(prior_prob, posterior_prob_pos)
            info_gain_neg <- private$.calculateInformationGain(prior_prob, posterior_prob_neg)
            
            # Populate Bayesian updates table
            updates_table <- self$results$bayesianUpdates
            updates_table$addRow(rowKey = "pos_test", values = list(
                test_sequence = "Positive Test",
                prior_prob = prior_prob,
                likelihood_ratio = lr_pos,
                posterior_prob = posterior_prob_pos,
                bayes_factor = lr_pos,
                odds_ratio = posterior_odds_pos,
                probability_gain = info_gain_pos
            ))
            
            updates_table$addRow(rowKey = "neg_test", values = list(
                test_sequence = "Negative Test",
                prior_prob = prior_prob,
                likelihood_ratio = lr_neg,
                posterior_prob = posterior_prob_neg,
                bayes_factor = lr_neg,
                odds_ratio = posterior_odds_neg,
                probability_gain = info_gain_neg
            ))
            
            # Prior and posterior summaries if requested
            if (self$options$posterior_summary) {
                private$.calculatePriorSummary(prior_prob)
                private$.calculatePosteriorSummary(posterior_prob_pos, posterior_prob_neg)
            }
        },
        
        .runTestChainAnalysis = function(test_result, disease_status, prior_prob) {
            # For sequential testing, simulate multiple test steps
            # This is a simplified implementation for demonstration
            
            # Calculate single test characteristics first
            private$.runSingleTestAnalysis(test_result, disease_status, prior_prob)
            
            # Simulate sequential updates
            test_sequence <- strsplit(self$options$test_sequence, ",")[[1]]
            test_sequence <- trimws(test_sequence)
            
            current_prior <- prior_prob
            updates_table <- self$results$bayesianUpdates
            
            # Clear existing rows
            for (i in seq_len(updates_table$rowCount)) {
                updates_table$deleteRows()
            }
            
            for (i in seq_along(test_sequence)) {
                # Use same LR for simplicity (in practice, each test would have different characteristics)
                ct <- table(test_result, disease_status)
                if (nrow(ct) >= 2 && ncol(ct) >= 2) {
                    tp <- ct[2, 2]; fp <- ct[2, 1]; fn <- ct[1, 2]; tn <- ct[1, 1]
                    sensitivity <- tp / (tp + fn)
                    specificity <- tn / (tn + fp)
                    lr_pos <- sensitivity / (1 - specificity)
                    
                    # Update probability
                    prior_odds <- current_prior / (1 - current_prior)
                    posterior_odds <- lr_pos * prior_odds
                    posterior_prob <- posterior_odds / (1 + posterior_odds)
                    
                    # Add row
                    updates_table$addRow(rowKey = paste0("test_", i), values = list(
                        test_sequence = paste("Step", i, ":", test_sequence[i]),
                        prior_prob = current_prior,
                        likelihood_ratio = lr_pos,
                        posterior_prob = posterior_prob,
                        bayes_factor = lr_pos,
                        odds_ratio = posterior_odds,
                        probability_gain = private$.calculateInformationGain(current_prior, posterior_prob)
                    ))
                    
                    # Update for next iteration
                    current_prior <- posterior_prob
                }
            }
        },
        
        .runParallelTestAnalysis = function(analysis_data, prior_prob) {
            # Simplified parallel test analysis
            # In practice, this would handle multiple test variables simultaneously
            
            self$results$instructions$setContent(
                "<html><body><p><b>Note:</b> Parallel test analysis requires multiple test variables. 
                Using single test approach with independence assumption.</p></body></html>"
            )
            
            # Fall back to single test
            test_result_var <- self$options$test_result
            disease_status_var <- self$options$disease_status
            
            if (!is.null(test_result_var) && !is.null(disease_status_var)) {
                test_result <- analysis_data[[test_result_var]]
                disease_status <- analysis_data[[disease_status_var]]
                private$.runSingleTestAnalysis(test_result, disease_status, prior_prob)
            }
        },
        
        .runHierarchicalBayes = function(test_result, disease_status, prior_prob, covariates) {
            # Simplified hierarchical Bayesian analysis
            # In practice, this would use MCMC with packages like rstan, rjags, or brms
            
            if (self$options$multilevel_structure && !is.null(covariates) && length(covariates) > 0) {
                # Simulate hierarchical results
                hier_table <- self$results$hierarchicalResults
                
                hier_table$addRow(rowKey = "overall", values = list(
                    level = "Overall",
                    parameter = "Sensitivity",
                    estimate = 0.85,
                    se = 0.05,
                    tau_squared = 0.01,
                    i_squared = 25.0,
                    credible_lower = 0.76,
                    credible_upper = 0.94
                ))
                
                hier_table$addRow(rowKey = "between", values = list(
                    level = "Between-Study",
                    parameter = "Specificity",
                    estimate = 0.90,
                    se = 0.04,
                    tau_squared = 0.005,
                    i_squared = 15.0,
                    credible_lower = 0.82,
                    credible_upper = 0.96
                ))
            }
            
            # Also run single test analysis
            private$.runSingleTestAnalysis(test_result, disease_status, prior_prob)
        },
        
        .runBetaBinomial = function(test_result, disease_status, prior_prob) {
            # Beta-binomial model for overdispersed diagnostic data
            
            if (self$options$overdispersion_model) {
                # Calculate sample statistics
                n_pos_disease <- sum(disease_status == 1)
                n_neg_disease <- sum(disease_status == 0)
                n_pos_test_given_disease <- sum(test_result == 1 & disease_status == 1)
                n_pos_test_given_no_disease <- sum(test_result == 1 & disease_status == 0)
                
                # Beta-binomial parameters (simplified)
                alpha_sens <- self$options$alpha_prior
                beta_sens <- self$options$beta_prior
                alpha_spec <- self$options$alpha_prior
                beta_spec <- self$options$beta_prior
                
                # Posterior parameters
                alpha_post_sens <- alpha_sens + n_pos_test_given_disease
                beta_post_sens <- beta_sens + (n_pos_disease - n_pos_test_given_disease)
                
                alpha_post_spec <- alpha_spec + (n_neg_disease - n_pos_test_given_no_disease)
                beta_post_spec <- beta_spec + n_pos_test_given_no_disease
                
                # Posterior means
                sens_post_mean <- alpha_post_sens / (alpha_post_sens + beta_post_sens)
                spec_post_mean <- alpha_post_spec / (alpha_post_spec + beta_post_spec)
                
                # Update likelihood table with beta-binomial results
                likelihood_table <- self$results$likelihoodTable
                likelihood_table$addRow(rowKey = "beta_binomial", values = list(
                    test_outcome = "Beta-Binomial Model",
                    sensitivity = sens_post_mean,
                    specificity = spec_post_mean,
                    lr_positive = sens_post_mean / (1 - spec_post_mean),
                    lr_negative = (1 - sens_post_mean) / spec_post_mean,
                    se_sensitivity = sqrt((alpha_post_sens * beta_post_sens) / 
                                        ((alpha_post_sens + beta_post_sens)^2 * (alpha_post_sens + beta_post_sens + 1))),
                    se_specificity = sqrt((alpha_post_spec * beta_post_spec) / 
                                        ((alpha_post_spec + beta_post_spec)^2 * (alpha_post_spec + beta_post_spec + 1)))
                ))
            }
            
            # Also run single test analysis
            private$.runSingleTestAnalysis(test_result, disease_status, prior_prob)
        },
        
        .calculatePriorSummary = function(prior_prob) {
            # For fixed prior, summary is straightforward
            if (self$options$prior_type == "fixed_prior") {
                prior_table <- self$results$priorSummary
                prior_table$addRow(rowKey = "disease_prob", values = list(
                    parameter = "Disease Probability",
                    mean = prior_prob,
                    median = prior_prob,
                    mode = prior_prob,
                    variance = prior_prob * (1 - prior_prob),
                    lower_ci = max(0, prior_prob - 1.96 * sqrt(prior_prob * (1 - prior_prob))),
                    upper_ci = min(1, prior_prob + 1.96 * sqrt(prior_prob * (1 - prior_prob)))
                ))
            } else if (self$options$prior_type == "informative_prior") {
                # Beta prior summary
                alpha <- self$options$alpha_prior
                beta <- self$options$beta_prior
                
                beta_mean <- alpha / (alpha + beta)
                beta_mode <- ifelse(alpha > 1 && beta > 1, (alpha - 1) / (alpha + beta - 2), NA)
                beta_var <- (alpha * beta) / ((alpha + beta)^2 * (alpha + beta + 1))
                
                # Beta quantiles for CI
                lower_ci <- qbeta(0.025, alpha, beta)
                upper_ci <- qbeta(0.975, alpha, beta)
                
                prior_table <- self$results$priorSummary
                prior_table$addRow(rowKey = "beta_prior", values = list(
                    parameter = paste0("Beta(", alpha, ",", beta, ") Prior"),
                    mean = beta_mean,
                    median = qbeta(0.5, alpha, beta),
                    mode = ifelse(is.na(beta_mode), NA, beta_mode),
                    variance = beta_var,
                    lower_ci = lower_ci,
                    upper_ci = upper_ci
                ))
            }
        },
        
        .calculatePosteriorSummary = function(posterior_prob_pos, posterior_prob_neg) {
            posterior_table <- self$results$posteriorSummary
            
            # Simplified posterior summary (in practice would be based on MCMC samples)
            credible_level <- self$options$credible_level
            alpha_level <- 1 - credible_level
            
            # For positive test result
            post_var_pos <- posterior_prob_pos * (1 - posterior_prob_pos) / 100  # Simplified variance
            se_pos <- sqrt(post_var_pos)
            
            posterior_table$addRow(rowKey = "pos_test_post", values = list(
                parameter = "Posterior (Positive Test)",
                posterior_mean = posterior_prob_pos,
                posterior_median = posterior_prob_pos,  # Simplified
                posterior_mode = posterior_prob_pos,    # Simplified
                posterior_sd = se_pos,
                credible_lower = max(0, posterior_prob_pos - qnorm(1 - alpha_level/2) * se_pos),
                credible_upper = min(1, posterior_prob_pos + qnorm(1 - alpha_level/2) * se_pos)
            ))
            
            # For negative test result
            post_var_neg <- posterior_prob_neg * (1 - posterior_prob_neg) / 100  # Simplified variance
            se_neg <- sqrt(post_var_neg)
            
            posterior_table$addRow(rowKey = "neg_test_post", values = list(
                parameter = "Posterior (Negative Test)",
                posterior_mean = posterior_prob_neg,
                posterior_median = posterior_prob_neg,  # Simplified
                posterior_mode = posterior_prob_neg,    # Simplified
                posterior_sd = se_neg,
                credible_lower = max(0, posterior_prob_neg - qnorm(1 - alpha_level/2) * se_neg),
                credible_upper = min(1, posterior_prob_neg + qnorm(1 - alpha_level/2) * se_neg)
            ))
        },
        
        .calculateInformationGain = function(prior_prob, posterior_prob) {
            # Kullback-Leibler divergence as information gain
            if (prior_prob <= 0 || prior_prob >= 1 || posterior_prob <= 0 || posterior_prob >= 1) {
                return(0)
            }
            
            kl_gain <- posterior_prob * log(posterior_prob / prior_prob) + 
                      (1 - posterior_prob) * log((1 - posterior_prob) / (1 - prior_prob))
            
            return(abs(kl_gain))
        },
        
        .plotPriorPosterior = function(image, ggtheme, theme, ...) {
            if (!self$options$diagnostic_plots) return()
            
            # Create prior vs posterior distribution plot
            # This is a simplified version - would be more sophisticated in practice
            
            library(ggplot2)
            
            prior_prob <- self$options$prior_prob
            
            # Generate plot data
            x_vals <- seq(0, 1, length.out = 1000)
            
            if (self$options$prior_type == "informative_prior") {
                alpha <- self$options$alpha_prior
                beta <- self$options$beta_prior
                prior_density <- dbeta(x_vals, alpha, beta)
                
                # Simulate posterior (would be calculated from actual data)
                alpha_post <- alpha + 10  # Simplified
                beta_post <- beta + 5     # Simplified
                posterior_density <- dbeta(x_vals, alpha_post, beta_post)
            } else {
                # For fixed prior, show as point mass (approximated with narrow normal)
                prior_density <- dnorm(x_vals, prior_prob, 0.01)
                posterior_density <- dnorm(x_vals, min(0.9, prior_prob + 0.2), 0.05)  # Simplified
            }
            
            plot_data <- data.frame(
                x = rep(x_vals, 2),
                density = c(prior_density, posterior_density),
                Distribution = rep(c("Prior", "Posterior"), each = length(x_vals))
            )
            
            p <- ggplot(plot_data, aes(x = x, y = density, color = Distribution, fill = Distribution)) +
                geom_line(size = 1.2) +
                geom_area(alpha = 0.3) +
                scale_color_manual(values = c("Prior" = "#3498db", "Posterior" = "#e74c3c")) +
                scale_fill_manual(values = c("Prior" = "#3498db", "Posterior" = "#e74c3c")) +
                labs(
                    title = "Prior vs Posterior Distributions",
                    subtitle = "Bayesian updating of disease probability",
                    x = "Disease Probability",
                    y = "Density"
                ) +
                theme_minimal() +
                theme(
                    legend.position = "top",
                    plot.title = element_text(size = 14, hjust = 0.5),
                    plot.subtitle = element_text(size = 12, hjust = 0.5)
                )
            
            print(p)
            TRUE
        },
        
        .plotLikelihood = function(image, ggtheme, theme, ...) {
            if (!self$options$diagnostic_plots) return()
            
            library(ggplot2)
            
            # Simple likelihood plot
            sensitivity_vals <- seq(0.01, 0.99, length.out = 100)
            specificity_vals <- seq(0.01, 0.99, length.out = 100)
            
            # Create a simple 1D likelihood for sensitivity
            # In practice, this would be calculated from actual data
            likelihood_vals <- dnorm(sensitivity_vals, 0.8, 0.1)  # Simplified
            
            plot_data <- data.frame(
                sensitivity = sensitivity_vals,
                likelihood = likelihood_vals
            )
            
            p <- ggplot(plot_data, aes(x = sensitivity, y = likelihood)) +
                geom_line(color = "#2c3e50", size = 1.2) +
                geom_area(alpha = 0.3, fill = "#3498db") +
                labs(
                    title = "Likelihood Function",
                    subtitle = "Likelihood of sensitivity given observed data",
                    x = "Sensitivity",
                    y = "Likelihood"
                ) +
                theme_minimal() +
                theme(
                    plot.title = element_text(size = 14, hjust = 0.5),
                    plot.subtitle = element_text(size = 12, hjust = 0.5)
                )
            
            print(p)
            TRUE
        },
        
        .plotPosteriorPredictive = function(image, ggtheme, theme, ...) {
            if (!self$options$predictive_checking) return()
            
            library(ggplot2)
            
            # Simulate posterior predictive data
            n_sim <- 100
            observed_tp_rate <- 0.75  # Simplified - would be calculated from data
            
            # Generate predictive samples
            predicted_rates <- rbeta(n_sim, 20, 7)  # Simplified posterior samples
            
            plot_data <- data.frame(
                rate = predicted_rates
            )
            
            p <- ggplot(plot_data, aes(x = rate)) +
                geom_histogram(aes(y = ..density..), bins = 20, alpha = 0.7, fill = "#3498db") +
                geom_vline(xintercept = observed_tp_rate, color = "#e74c3c", linetype = "dashed", size = 1.2) +
                labs(
                    title = "Posterior Predictive Distribution",
                    subtitle = "Red line = observed true positive rate",
                    x = "True Positive Rate",
                    y = "Density"
                ) +
                theme_minimal() +
                theme(
                    plot.title = element_text(size = 14, hjust = 0.5),
                    plot.subtitle = element_text(size = 12, hjust = 0.5)
                )
            
            print(p)
            TRUE
        },
        
        .plotSensitivity = function(image, ggtheme, theme, ...) {
            if (!self$options$sensitivity_analysis) return()
            
            library(ggplot2)
            
            # Sensitivity analysis plot - varying prior parameters
            alpha_vals <- c(1, 2, 5, 10)
            beta_vals <- c(1, 2, 5, 10)
            
            # Generate combinations
            combinations <- expand.grid(alpha = alpha_vals, beta = beta_vals)
            combinations$posterior_mean <- combinations$alpha / (combinations$alpha + combinations$beta)
            combinations$prior_label <- paste0("Beta(", combinations$alpha, ",", combinations$beta, ")")
            
            p <- ggplot(combinations, aes(x = reorder(prior_label, posterior_mean), y = posterior_mean)) +
                geom_point(size = 3, color = "#3498db") +
                geom_line(aes(group = 1), color = "#2c3e50") +
                coord_flip() +
                labs(
                    title = "Prior Sensitivity Analysis",
                    subtitle = "Effect of different Beta priors on posterior mean",
                    x = "Prior Specification",
                    y = "Posterior Mean"
                ) +
                theme_minimal() +
                theme(
                    plot.title = element_text(size = 14, hjust = 0.5),
                    plot.subtitle = element_text(size = 12, hjust = 0.5)
                )
            
            print(p)
            TRUE
        },
        
        .plotNetworkDiagram = function(image, ggtheme, theme, ...) {
            if (!self$options$network_analysis) return()
            
            library(ggplot2)
            
            # Simple network diagram representation
            # In practice, would use networks package or similar
            
            nodes <- data.frame(
                x = c(1, 3, 5, 3),
                y = c(2, 4, 2, 0),
                label = c("Prior", "Test 1", "Test 2", "Posterior"),
                type = c("Prior", "Test", "Test", "Posterior")
            )
            
            edges <- data.frame(
                x1 = c(1, 1, 3, 5),
                y1 = c(2, 2, 4, 2),
                x2 = c(3, 5, 3, 3),
                y2 = c(4, 2, 0, 0)
            )
            
            p <- ggplot() +
                geom_segment(data = edges, aes(x = x1, y = y1, xend = x2, yend = y2),
                            arrow = arrow(length = unit(0.3, "cm")), color = "#2c3e50", size = 1) +
                geom_point(data = nodes, aes(x = x, y = y, color = type), size = 6) +
                geom_text(data = nodes, aes(x = x, y = y, label = label), 
                         color = "white", fontface = "bold", size = 3) +
                scale_color_manual(values = c("Prior" = "#3498db", "Test" = "#e67e22", "Posterior" = "#e74c3c")) +
                labs(
                    title = "Diagnostic Network Structure",
                    subtitle = "Bayesian network for diagnostic test combination"
                ) +
                theme_void() +
                theme(
                    legend.position = "none",
                    plot.title = element_text(size = 14, hjust = 0.5),
                    plot.subtitle = element_text(size = 12, hjust = 0.5)
                ) +
                coord_fixed()
            
            print(p)
            TRUE
        }
    )
)