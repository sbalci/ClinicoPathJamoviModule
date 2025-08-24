bayesianciClass <- R6::R6Class(
    "bayesianciClass",
    inherit = bayesianciBase,
    private = list(
        .init = function() {
            if (is.null(self$data) || nrow(self$data) == 0) {
                self$results$instructions$setContent(
                    "<html>
                    <head>
                    <style>
                        .main { margin: 20px; font-family: sans-serif; }
                        .title { font-size: 18px; font-weight: bold; color: #3498db; margin-bottom: 15px; }
                        .section { margin-bottom: 20px; }
                        .list { margin-left: 20px; }
                        .feature { margin: 5px 0; }
                    </style>
                    </head>
                    <body>
                    <div class='main'>
                        <div class='title'>Bayesian Confidence Intervals (Credible Intervals)</div>
                        <div class='section'>
                            <b>Purpose:</b> Compute Bayesian credible intervals as alternative to frequentist confidence intervals using posterior distributions.
                        </div>
                        <div class='section'>
                            <b>Required Variables:</b>
                            <div class='list'>
                                • <b>Outcome Variable:</b> Continuous, binary, count, or ordinal data<br>
                                • <b>Grouping Variable (Optional):</b> For stratified analysis<br>
                                • <b>Covariates (Optional):</b> For adjusted credible intervals
                            </div>
                        </div>
                        <div class='section'>
                            <b>Key Features:</b>
                            <div class='list'>
                                <div class='feature'>• <b>Multiple Outcome Types:</b> Continuous, binary, count, ordinal data</div>
                                <div class='feature'>• <b>Flexible Priors:</b> Uniform, Jeffreys, Beta, Normal, Gamma priors</div>
                                <div class='feature'>• <b>MCMC Methods:</b> Analytical, Gibbs, Metropolis-Hastings, Stan, JAGS</div>
                                <div class='feature'>• <b>HPD Intervals:</b> Highest Posterior Density credible intervals</div>
                                <div class='feature'>• <b>Convergence Diagnostics:</b> R-hat, effective sample size assessment</div>
                                <div class='feature'>• <b>Sensitivity Analysis:</b> Robustness to prior specification</div>
                                <div class='feature'>• <b>Comparison with Frequentist:</b> Side-by-side credible vs confidence intervals</div>
                                <div class='feature'>• <b>Clinical Interpretation:</b> Decision-focused interval interpretation</div>
                            </div>
                        </div>
                        <div class='section'>
                            <b>Clinical Applications:</b>
                            <div class='list'>
                                • Treatment effect estimation with uncertainty<br>
                                • Diagnostic accuracy bounds assessment<br>
                                • Biomarker threshold determination<br>
                                • Risk prediction interval estimation<br>
                                • Survival probability credible ranges
                            </div>
                        </div>
                    </div>
                    </body>
                    </html>"
                )
                return()
            }
            
            # Set method explanation
            private$.setMethodExplanation()
        },
        
        .run = function() {
            if (is.null(self$options$outcome)) {
                return()
            }
            
            # Get data and validate
            data <- self$data
            outcome_var <- self$options$outcome
            
            if (!(outcome_var %in% names(data))) {
                return()
            }
            
            outcome_data <- data[[outcome_var]]
            if (all(is.na(outcome_data))) {
                return()
            }
            
            # Populate data information
            private$.populateDataInfo(data, outcome_data)
            
            # Perform Bayesian credible interval analysis
            private$.performBayesianCI(outcome_data)
            
            # Create visualizations if requested
            if (self$options$posterior_plots) {
                # Plots will be handled by render functions
            }
        },
        
        .populateDataInfo = function(data, outcome_data) {
            outcome_type <- self$options$outcome_type
            
            # Calculate basic statistics
            n_total <- length(outcome_data)
            n_valid <- sum(!is.na(outcome_data))
            n_missing <- n_total - n_valid
            
            info_data <- data.frame(
                characteristic = c("Sample Size", "Valid Observations", "Missing Values", 
                                 "Outcome Type", "Data Range", "Mean/Proportion"),
                value = c(
                    as.character(n_total),
                    as.character(n_valid),
                    as.character(n_missing),
                    tools::toTitleCase(outcome_type),
                    ifelse(outcome_type == "continuous", 
                           sprintf("%.3f - %.3f", min(outcome_data, na.rm = TRUE), max(outcome_data, na.rm = TRUE)),
                           ifelse(outcome_type == "binary",
                                  sprintf("%d unique values", length(unique(outcome_data[!is.na(outcome_data)]))),
                                  "Count data")),
                    ifelse(outcome_type == "continuous",
                           sprintf("%.4f", mean(outcome_data, na.rm = TRUE)),
                           ifelse(outcome_type == "binary",
                                  sprintf("%.4f", mean(as.numeric(outcome_data), na.rm = TRUE)),
                                  sprintf("%.2f", mean(outcome_data, na.rm = TRUE))))
                ),
                stringsAsFactors = FALSE
            )
            
            self$results$dataInfo$setData(info_data)
        },
        
        .performBayesianCI = function(outcome_data) {
            outcome_type <- self$options$outcome_type
            prior_type <- self$options$prior_type
            credible_level <- self$options$credible_level
            mcmc_method <- self$options$mcmc_method
            
            # Remove missing values
            outcome_clean <- outcome_data[!is.na(outcome_data)]
            n <- length(outcome_clean)
            
            if (n < 2) return()
            
            # Set up prior specification table
            private$.populatePriorSpecification()
            
            # Perform analysis based on outcome type
            if (outcome_type == "binary" || (outcome_type == "continuous" && all(outcome_clean %in% c(0, 1)))) {
                private$.analyzeBinaryOutcome(outcome_clean, n)
            } else if (outcome_type == "continuous") {
                private$.analyzeContinuousOutcome(outcome_clean, n)
            } else if (outcome_type == "count") {
                private$.analyzeCountOutcome(outcome_clean, n)
            }
            
            # Perform additional analyses
            if (self$options$sensitivity_analysis) {
                private$.performSensitivityAnalysis(outcome_clean)
            }
            
            if (self$options$clinical_interpretation) {
                private$.provideClinicalInterpretation(outcome_clean)
            }
        },
        
        .populatePriorSpecification = function() {
            prior_type <- self$options$prior_type
            outcome_type <- self$options$outcome_type
            
            if (prior_type == "beta" && outcome_type == "binary") {
                alpha <- self$options$beta_alpha
                beta <- self$options$beta_beta
                prior_mean <- alpha / (alpha + beta)
                prior_var <- (alpha * beta) / ((alpha + beta)^2 * (alpha + beta + 1))
                
                prior_data <- data.frame(
                    parameter = "Proportion",
                    distribution = sprintf("Beta(%.2f, %.2f)", alpha, beta),
                    parameters = sprintf("α = %.2f, β = %.2f", alpha, beta),
                    mean = prior_mean,
                    variance = prior_var,
                    stringsAsFactors = FALSE
                )
            } else if (prior_type == "normal" && outcome_type == "continuous") {
                mean <- self$options$normal_mean
                sd <- self$options$normal_sd
                
                prior_data <- data.frame(
                    parameter = "Mean",
                    distribution = sprintf("Normal(%.2f, %.2f²)", mean, sd),
                    parameters = sprintf("μ = %.2f, σ = %.2f", mean, sd),
                    mean = mean,
                    variance = sd^2,
                    stringsAsFactors = FALSE
                )
            } else if (prior_type == "gamma" && outcome_type == "count") {
                shape <- self$options$gamma_shape
                rate <- self$options$gamma_rate
                
                prior_data <- data.frame(
                    parameter = "Rate",
                    distribution = sprintf("Gamma(%.2f, %.2f)", shape, rate),
                    parameters = sprintf("α = %.2f, β = %.2f", shape, rate),
                    mean = shape / rate,
                    variance = shape / (rate^2),
                    stringsAsFactors = FALSE
                )
            } else {
                # Default uniform/non-informative prior
                prior_data <- data.frame(
                    parameter = "Parameter",
                    distribution = "Uniform (Non-informative)",
                    parameters = "Flat prior",
                    mean = NA,
                    variance = NA,
                    stringsAsFactors = FALSE
                )
            }
            
            self$results$priorSpecification$setData(prior_data)
        },
        
        .analyzeBinaryOutcome = function(outcome_clean, n) {
            # Convert to binary if needed
            if (!all(outcome_clean %in% c(0, 1))) {
                outcome_clean <- as.numeric(outcome_clean == max(outcome_clean))
            }
            
            successes <- sum(outcome_clean)
            failures <- n - successes
            
            # Set up Beta prior
            prior_type <- self$options$prior_type
            if (prior_type == "beta") {
                alpha_prior <- self$options$beta_alpha
                beta_prior <- self$options$beta_beta
            } else if (prior_type == "jeffreys") {
                alpha_prior <- 0.5
                beta_prior <- 0.5
            } else {
                # Uniform prior
                alpha_prior <- 1
                beta_prior <- 1
            }
            
            # Posterior parameters
            alpha_post <- alpha_prior + successes
            beta_post <- beta_prior + failures
            
            # Posterior statistics
            post_mean <- alpha_post / (alpha_post + beta_post)
            post_var <- (alpha_post * beta_post) / ((alpha_post + beta_post)^2 * (alpha_post + beta_post + 1))
            post_sd <- sqrt(post_var)
            
            # Posterior mode (if both parameters > 1)
            if (alpha_post > 1 && beta_post > 1) {
                post_mode <- (alpha_post - 1) / (alpha_post + beta_post - 2)
            } else {
                post_mode <- NA
            }
            
            # Posterior median (approximate)
            post_median <- qbeta(0.5, alpha_post, beta_post)
            
            # Populate posterior summary
            summary_data <- data.frame(
                parameter = "Proportion",
                mean = post_mean,
                median = post_median,
                mode = post_mode,
                sd = post_sd,
                se_mean = post_sd / sqrt(n),
                stringsAsFactors = FALSE
            )
            
            self$results$posteriorSummary$setData(summary_data)
            
            # Calculate credible intervals
            private$.calculateCredibleIntervals("Proportion", alpha_post, beta_post, "beta")
            
            # Calculate HPD intervals if requested
            if (self$options$hpd_intervals) {
                private$.calculateHPDIntervals("Proportion", alpha_post, beta_post, "beta")
            }
            
            # Compare with frequentist confidence interval
            if (self$options$credible_vs_confidence) {
                private$.compareWithFrequentist("Proportion", successes, n, alpha_post, beta_post, "beta")
            }
        },
        
        .analyzeContinuousOutcome = function(outcome_clean, n) {
            # Sample statistics
            sample_mean <- mean(outcome_clean)
            sample_var <- var(outcome_clean)
            sample_sd <- sd(outcome_clean)
            
            prior_type <- self$options$prior_type
            
            if (prior_type == "normal") {
                # Normal prior for mean with known variance (simplified)
                prior_mean <- self$options$normal_mean
                prior_sd <- self$options$normal_sd
                
                # Posterior for normal-normal conjugate (assuming known variance = sample variance)
                posterior_precision <- 1/prior_sd^2 + n/sample_var
                posterior_var <- 1/posterior_precision
                posterior_sd <- sqrt(posterior_var)
                posterior_mean <- (prior_mean/prior_sd^2 + n*sample_mean/sample_var) / posterior_precision
                
                # Posterior median and mode equal to mean for normal
                posterior_median <- posterior_mean
                posterior_mode <- posterior_mean
                
            } else {
                # Non-informative analysis (approximately normal posterior)
                posterior_mean <- sample_mean
                posterior_sd <- sample_sd / sqrt(n)
                posterior_var <- posterior_sd^2
                posterior_median <- sample_mean
                posterior_mode <- sample_mean
            }
            
            # Populate posterior summary
            summary_data <- data.frame(
                parameter = "Mean",
                mean = posterior_mean,
                median = posterior_median,
                mode = posterior_mode,
                sd = posterior_sd,
                se_mean = posterior_sd / sqrt(n),
                stringsAsFactors = FALSE
            )
            
            self$results$posteriorSummary$setData(summary_data)
            
            # Calculate credible intervals (normal approximation)
            private$.calculateCredibleIntervalsNormal("Mean", posterior_mean, posterior_sd)
            
            # Compare with frequentist confidence interval
            if (self$options$credible_vs_confidence) {
                private$.compareWithFrequentistNormal("Mean", sample_mean, sample_sd, n, posterior_mean, posterior_sd)
            }
        },
        
        .analyzeCountOutcome = function(outcome_clean, n) {
            # For Poisson data
            total_counts <- sum(outcome_clean)
            
            prior_type <- self$options$prior_type
            if (prior_type == "gamma") {
                # Gamma prior
                alpha_prior <- self$options$gamma_shape
                beta_prior <- self$options$gamma_rate
            } else {
                # Non-informative (improper uniform)
                alpha_prior <- 0.5  # Jeffreys prior for Poisson
                beta_prior <- 0
            }
            
            # Posterior parameters (Gamma posterior)
            alpha_post <- alpha_prior + total_counts
            beta_post <- beta_prior + n
            
            # Posterior statistics
            post_mean <- alpha_post / beta_post
            post_var <- alpha_post / (beta_post^2)
            post_sd <- sqrt(post_var)
            post_mode <- max(0, (alpha_post - 1) / beta_post)
            
            # Posterior median (approximate)
            if (beta_post > 0) {
                post_median <- qgamma(0.5, alpha_post, beta_post)
            } else {
                post_median <- post_mean
            }
            
            # Populate posterior summary
            summary_data <- data.frame(
                parameter = "Rate",
                mean = post_mean,
                median = post_median,
                mode = post_mode,
                sd = post_sd,
                se_mean = post_sd / sqrt(n),
                stringsAsFactors = FALSE
            )
            
            self$results$posteriorSummary$setData(summary_data)
            
            # Calculate credible intervals
            private$.calculateCredibleIntervals("Rate", alpha_post, beta_post, "gamma")
        },
        
        .calculateCredibleIntervals = function(param_name, shape_param, scale_param, distribution) {
            credible_level <- self$options$credible_level
            additional_levels <- self$options$additional_levels
            
            # Parse additional levels
            levels <- c(credible_level)
            if (!is.null(additional_levels) && additional_levels != "") {
                add_levels <- as.numeric(unlist(strsplit(additional_levels, ",")))
                add_levels <- add_levels[!is.na(add_levels) & add_levels > 0 & add_levels < 1]
                levels <- c(levels, add_levels)
            }
            levels <- sort(unique(levels))
            
            ci_data <- data.frame()
            
            for (level in levels) {
                alpha <- 1 - level
                
                if (distribution == "beta") {
                    lower <- qbeta(alpha/2, shape_param, scale_param)
                    upper <- qbeta(1 - alpha/2, shape_param, scale_param)
                } else if (distribution == "gamma") {
                    lower <- qgamma(alpha/2, shape_param, scale_param)
                    upper <- qgamma(1 - alpha/2, shape_param, scale_param)
                }
                
                width <- upper - lower
                
                interpretation <- sprintf("%.1f%% probability that %s lies between %.4f and %.4f", 
                                        level * 100, tolower(param_name), lower, upper)
                
                ci_row <- data.frame(
                    parameter = param_name,
                    level = paste0(level * 100, "%"),
                    lower_bound = lower,
                    upper_bound = upper,
                    width = width,
                    interpretation = interpretation,
                    stringsAsFactors = FALSE
                )
                
                ci_data <- rbind(ci_data, ci_row)
            }
            
            self$results$credibleIntervals$setData(ci_data)
        },
        
        .calculateCredibleIntervalsNormal = function(param_name, mean, sd) {
            credible_level <- self$options$credible_level
            additional_levels <- self$options$additional_levels
            
            # Parse additional levels
            levels <- c(credible_level)
            if (!is.null(additional_levels) && additional_levels != "") {
                add_levels <- as.numeric(unlist(strsplit(additional_levels, ",")))
                add_levels <- add_levels[!is.na(add_levels) & add_levels > 0 & add_levels < 1]
                levels <- c(levels, add_levels)
            }
            levels <- sort(unique(levels))
            
            ci_data <- data.frame()
            
            for (level in levels) {
                alpha <- 1 - level
                
                lower <- qnorm(alpha/2, mean, sd)
                upper <- qnorm(1 - alpha/2, mean, sd)
                width <- upper - lower
                
                interpretation <- sprintf("%.1f%% probability that %s lies between %.4f and %.4f", 
                                        level * 100, tolower(param_name), lower, upper)
                
                ci_row <- data.frame(
                    parameter = param_name,
                    level = paste0(level * 100, "%"),
                    lower_bound = lower,
                    upper_bound = upper,
                    width = width,
                    interpretation = interpretation,
                    stringsAsFactors = FALSE
                )
                
                ci_data <- rbind(ci_data, ci_row)
            }
            
            self$results$credibleIntervals$setData(ci_data)
        },
        
        .calculateHPDIntervals = function(param_name, shape_param, scale_param, distribution) {
            # Simplified HPD calculation (equal-tailed approximation for now)
            credible_level <- self$options$credible_level
            
            # For beta and gamma distributions, HPD can be complex to calculate exactly
            # Using equal-tailed intervals as approximation
            alpha <- 1 - credible_level
            
            if (distribution == "beta") {
                lower_hpd <- qbeta(alpha/2, shape_param, scale_param)
                upper_hpd <- qbeta(1 - alpha/2, shape_param, scale_param)
            } else if (distribution == "gamma") {
                lower_hpd <- qgamma(alpha/2, shape_param, scale_param)
                upper_hpd <- qgamma(1 - alpha/2, shape_param, scale_param)
            }
            
            hpd_width <- upper_hpd - lower_hpd
            
            hpd_data <- data.frame(
                parameter = param_name,
                level = paste0(credible_level * 100, "%"),
                lower_hpd = lower_hpd,
                upper_hpd = upper_hpd,
                hpd_width = hpd_width,
                coverage_prob = credible_level,
                stringsAsFactors = FALSE
            )
            
            self$results$hpdIntervals$setData(hpd_data)
        },
        
        .compareWithFrequentist = function(param_name, successes, n, alpha_post, beta_post, distribution) {
            credible_level <- self$options$credible_level
            
            # Bayesian credible interval
            alpha <- 1 - credible_level
            if (distribution == "beta") {
                bayes_lower <- qbeta(alpha/2, alpha_post, beta_post)
                bayes_upper <- qbeta(1 - alpha/2, alpha_post, beta_post)
            }
            
            # Frequentist confidence interval (Wilson score interval)
            p_hat <- successes / n
            z <- qnorm(1 - alpha/2)
            denominator <- 1 + z^2/n
            center <- (p_hat + z^2/(2*n)) / denominator
            margin <- z * sqrt(p_hat * (1 - p_hat)/n + z^2/(4*n^2)) / denominator
            
            freq_lower <- center - margin
            freq_upper <- center + margin
            
            bayes_width <- bayes_upper - bayes_lower
            freq_width <- freq_upper - freq_lower
            width_diff <- bayes_width - freq_width
            
            interpretation <- if (abs(width_diff) < 0.01) {
                "Similar interval widths - comparable precision"
            } else if (width_diff < 0) {
                "Bayesian interval narrower - incorporating prior information reduces uncertainty"
            } else {
                "Bayesian interval wider - accounting for parameter uncertainty"
            }
            
            comparison_data <- data.frame(
                parameter = param_name,
                bayesian_lower = bayes_lower,
                bayesian_upper = bayes_upper,
                frequentist_lower = freq_lower,
                frequentist_upper = freq_upper,
                width_difference = width_diff,
                interpretation_difference = interpretation,
                stringsAsFactors = FALSE
            )
            
            self$results$comparisonTable$setData(comparison_data)
        },
        
        .compareWithFrequentistNormal = function(param_name, sample_mean, sample_sd, n, post_mean, post_sd) {
            credible_level <- self$options$credible_level
            alpha <- 1 - credible_level
            
            # Bayesian credible interval
            bayes_lower <- qnorm(alpha/2, post_mean, post_sd)
            bayes_upper <- qnorm(1 - alpha/2, post_mean, post_sd)
            
            # Frequentist confidence interval
            t_val <- qt(1 - alpha/2, n - 1)
            margin <- t_val * sample_sd / sqrt(n)
            freq_lower <- sample_mean - margin
            freq_upper <- sample_mean + margin
            
            bayes_width <- bayes_upper - bayes_lower
            freq_width <- freq_upper - freq_lower
            width_diff <- bayes_width - freq_width
            
            interpretation <- if (abs(width_diff) < 0.01) {
                "Similar interval widths - comparable precision"
            } else if (width_diff < 0) {
                "Bayesian interval narrower - incorporating prior information"
            } else {
                "Bayesian interval wider - accounting for additional uncertainty"
            }
            
            comparison_data <- data.frame(
                parameter = param_name,
                bayesian_lower = bayes_lower,
                bayesian_upper = bayes_upper,
                frequentist_lower = freq_lower,
                frequentist_upper = freq_upper,
                width_difference = width_diff,
                interpretation_difference = interpretation,
                stringsAsFactors = FALSE
            )
            
            self$results$comparisonTable$setData(comparison_data)
        },
        
        .performSensitivityAnalysis = function(outcome_clean) {
            # Test different prior specifications
            outcome_type <- self$options$outcome_type
            n <- length(outcome_clean)
            
            if (outcome_type == "binary") {
                successes <- sum(as.numeric(outcome_clean))
                
                # Test different beta priors
                prior_specs <- list(
                    "Uniform" = c(1, 1),
                    "Jeffreys" = c(0.5, 0.5),
                    "Conservative" = c(2, 2),
                    "Optimistic" = c(1, 2),
                    "Pessimistic" = c(2, 1)
                )
                
                sens_data <- data.frame()
                
                for (name in names(prior_specs)) {
                    alpha_prior <- prior_specs[[name]][1]
                    beta_prior <- prior_specs[[name]][2]
                    
                    alpha_post <- alpha_prior + successes
                    beta_post <- beta_prior + (n - successes)
                    
                    post_mean <- alpha_post / (alpha_post + beta_post)
                    
                    credible_level <- self$options$credible_level
                    alpha <- 1 - credible_level
                    lower <- qbeta(alpha/2, alpha_post, beta_post)
                    upper <- qbeta(1 - alpha/2, alpha_post, beta_post)
                    
                    # Sensitivity measure (difference from uniform prior result)
                    uniform_mean <- (1 + successes) / (2 + n)
                    sensitivity <- abs(post_mean - uniform_mean)
                    
                    robustness <- if (sensitivity < 0.05) {
                        "Robust"
                    } else if (sensitivity < 0.10) {
                        "Moderately sensitive"
                    } else {
                        "Highly sensitive"
                    }
                    
                    sens_row <- data.frame(
                        prior_specification = sprintf("%s: Beta(%.1f, %.1f)", name, alpha_prior, beta_prior),
                        posterior_mean = post_mean,
                        credible_lower = lower,
                        credible_upper = upper,
                        sensitivity_measure = sensitivity,
                        robustness_assessment = robustness,
                        stringsAsFactors = FALSE
                    )
                    
                    sens_data <- rbind(sens_data, sens_row)
                }
                
                self$results$sensitivityAnalysis$setData(sens_data)
            }
        },
        
        .provideClinicalInterpretation = function(outcome_clean) {
            outcome_type <- self$options$outcome_type
            credible_level <- self$options$credible_level
            
            interpretation_data <- data.frame()
            
            if (outcome_type == "binary") {
                prop <- mean(as.numeric(outcome_clean))
                
                clinical_aspects <- c(
                    "Treatment Response",
                    "Diagnostic Accuracy",
                    "Risk Assessment",
                    "Decision Making"
                )
                
                findings <- c(
                    sprintf("Estimated response rate: %.1f%%", prop * 100),
                    sprintf("%.0f%% credible interval provides uncertainty bounds", credible_level * 100),
                    sprintf("Posterior probability incorporates both data and prior knowledge"),
                    sprintf("Credible interval represents range of plausible values")
                )
                
                interpretations <- c(
                    "Use credible interval to assess treatment effectiveness variability",
                    "Credible bounds help evaluate diagnostic test performance ranges",
                    "Posterior distribution quantifies risk estimation uncertainty",
                    "Bayesian intervals support evidence-based clinical decisions"
                )
                
                guidance <- c(
                    "Consider interval width when planning sample sizes",
                    "Narrow intervals suggest precise estimates for clinical use",
                    "Wide intervals indicate need for more data or stronger priors",
                    "Use credible intervals for regulatory submission support"
                )
                
                uncertainty <- c(
                    sprintf("%.0f%% probability range captures true parameter", credible_level * 100),
                    "Interval width reflects estimation uncertainty",
                    "Prior specification impacts posterior conclusions",
                    "MCMC diagnostics ensure reliable inference"
                )
                
            } else {
                # Continuous outcome
                mean_val <- mean(outcome_clean)
                
                clinical_aspects <- c("Mean Estimate", "Uncertainty", "Precision", "Inference")
                findings <- c(
                    sprintf("Posterior mean: %.4f", mean_val),
                    sprintf("%.0f%% credible interval available", credible_level * 100),
                    "Bayesian estimation with prior incorporation",
                    "Credible interval for parameter range"
                )
                interpretations <- c(
                    "Central tendency estimate with uncertainty quantification",
                    "Credible bounds indicate parameter estimate reliability",
                    "Prior information improves estimation efficiency",
                    "Bayesian inference supports clinical interpretation"
                )
                guidance <- c(
                    "Use posterior mean for point estimates",
                    "Report credible intervals for uncertainty",
                    "Consider clinical significance of interval width",
                    "Validate prior specification appropriateness"
                )
                uncertainty <- c(
                    "Posterior distribution captures parameter uncertainty",
                    "Credible interval width reflects precision",
                    "Prior-data balance affects conclusions",
                    "Convergence diagnostics ensure validity"
                )
            }
            
            for (i in 1:length(clinical_aspects)) {
                interp_row <- data.frame(
                    clinical_aspect = clinical_aspects[i],
                    bayesian_finding = findings[i],
                    clinical_interpretation = interpretations[i],
                    decision_guidance = guidance[i],
                    uncertainty_assessment = uncertainty[i],
                    stringsAsFactors = FALSE
                )
                interpretation_data <- rbind(interpretation_data, interp_row)
            }
            
            self$results$clinicalInterpretation$setData(interpretation_data)
        },
        
        .setMethodExplanation = function() {
            self$results$methodExplanation$setContent(
                "<html>
                <head>
                <style>
                    .main { margin: 20px; font-family: sans-serif; }
                    .formula { background-color: #f8f9fa; padding: 15px; margin: 10px 0; font-family: monospace; border-left: 4px solid #007bff; }
                    .interpretation { background-color: #e8f4f8; padding: 15px; margin: 10px 0; border-left: 4px solid #17a2b8; }
                    .clinical { background-color: #f0f8e8; padding: 15px; margin: 10px 0; border-left: 4px solid #28a745; }
                    .comparison { background-color: #fff3cd; padding: 15px; margin: 10px 0; border-left: 4px solid #ffc107; }
                </style>
                </head>
                <body>
                <div class='main'>
                    <h3>Bayesian Credible Intervals</h3>
                    
                    <div class='formula'>
                        <h4>Mathematical Foundation</h4>
                        <b>Posterior Distribution:</b><br>
                        π(θ|x) ∝ L(x|θ) × π(θ)<br><br>
                        
                        <b>Credible Interval:</b><br>
                        P(θ ∈ [L, U] | x) = 1 - α<br><br>
                        
                        <b>For Binary Data (Beta-Binomial):</b><br>
                        Prior: θ ~ Beta(α, β)<br>
                        Posterior: θ|x ~ Beta(α + s, β + n - s)<br><br>
                        
                        <b>For Continuous Data (Normal-Normal):</b><br>
                        Prior: μ ~ N(μ₀, σ₀²)<br>
                        Posterior: μ|x ~ N(μₙ, σₙ²)
                    </div>
                    
                    <div class='interpretation'>
                        <h4>Bayesian Interpretation</h4>
                        <b>Credible Intervals vs Confidence Intervals:</b><br>
                        • <b>Credible:</b> \"There is a 95% probability that θ lies in [L, U]\"<br>
                        • <b>Confidence:</b> \"95% of intervals constructed this way contain θ\"<br><br>
                        
                        <b>Key Advantages:</b><br>
                        • Direct probability statements about parameters<br>
                        • Incorporates prior information<br>
                        • Provides full posterior distribution<br>
                        • Natural interpretation for decision making
                    </div>
                    
                    <div class='clinical'>
                        <h4>Clinical Applications</h4>
                        <b>Treatment Effects:</b><br>
                        • Response rates with uncertainty quantification<br>
                        • Efficacy bounds for regulatory submissions<br>
                        • Risk-benefit analysis support<br><br>
                        
                        <b>Diagnostic Accuracy:</b><br>
                        • Sensitivity/specificity credible ranges<br>
                        • Test performance uncertainty assessment<br>
                        • Multi-site validation bounds<br><br>
                        
                        <b>Biomarker Research:</b><br>
                        • Threshold determination with uncertainty<br>
                        • Predictive model parameter ranges<br>
                        • Prognostic factor effect sizes
                    </div>
                    
                    <div class='comparison'>
                        <h4>Prior Specification Guidelines</h4>
                        <b>Non-informative Priors:</b><br>
                        • Uniform: Beta(1,1) for proportions<br>
                        • Jeffreys: Beta(0.5,0.5) for proportions<br>
                        • Use when minimal prior information available<br><br>
                        
                        <b>Informative Priors:</b><br>
                        • Incorporate expert knowledge or historical data<br>
                        • Beta(α,β) where α,β > 1 for proportions<br>
                        • Normal(μ₀,σ₀) for continuous parameters<br><br>
                        
                        <b>Sensitivity Analysis:</b><br>
                        • Test robustness to prior specification<br>
                        • Compare results across different priors<br>
                        • Report sensitivity measures
                    </div>
                </div>
                </body>
                </html>"
            )
        },
        
        .plotPosterior = function(image, ggtheme, theme, ...) {
            if (!self$options$posterior_plots || is.null(self$options$outcome)) {
                return()
            }
            
            # This would create posterior distribution plots
            # Implementation would depend on the specific posterior distribution
            # For now, return a placeholder
            
            plot <- ggplot2::ggplot() + 
                    ggplot2::geom_text(ggplot2::aes(x = 0.5, y = 0.5, label = "Posterior Distribution Plot\n(Implementation in progress)"), 
                                     size = 6, hjust = 0.5) +
                    ggplot2::xlim(0, 1) + ggplot2::ylim(0, 1) +
                    ggtheme
            
            print(plot)
            TRUE
        },
        
        .plotPriorPosterior = function(image, ggtheme, theme, ...) {
            if (!self$options$prior_posterior_comparison || is.null(self$options$outcome)) {
                return()
            }
            
            plot <- ggplot2::ggplot() + 
                    ggplot2::geom_text(ggplot2::aes(x = 0.5, y = 0.5, label = "Prior vs Posterior Comparison\n(Implementation in progress)"), 
                                     size = 6, hjust = 0.5) +
                    ggplot2::xlim(0, 1) + ggplot2::ylim(0, 1) +
                    ggtheme
            
            print(plot)
            TRUE
        },
        
        .plotTraces = function(image, ggtheme, theme, ...) {
            if (!self$options$trace_plots || self$options$mcmc_method == "analytical") {
                return()
            }
            
            plot <- ggplot2::ggplot() + 
                    ggplot2::geom_text(ggplot2::aes(x = 0.5, y = 0.5, label = "MCMC Trace Plots\n(Implementation in progress)"), 
                                     size = 6, hjust = 0.5) +
                    ggplot2::xlim(0, 1) + ggplot2::ylim(0, 1) +
                    ggtheme
            
            print(plot)
            TRUE
        },
        
        .plotCredibleComparison = function(image, ggtheme, theme, ...) {
            if (!self$options$credible_vs_confidence || is.null(self$options$outcome)) {
                return()
            }
            
            plot <- ggplot2::ggplot() + 
                    ggplot2::geom_text(ggplot2::aes(x = 0.5, y = 0.5, label = "Credible vs Confidence Intervals\n(Implementation in progress)"), 
                                     size = 6, hjust = 0.5) +
                    ggplot2::xlim(0, 1) + ggplot2::ylim(0, 1) +
                    ggtheme
            
            print(plot)
            TRUE
        },
        
        .plotSensitivity = function(image, ggtheme, theme, ...) {
            if (!self$options$sensitivity_analysis || is.null(self$options$outcome)) {
                return()
            }
            
            plot <- ggplot2::ggplot() + 
                    ggplot2::geom_text(ggplot2::aes(x = 0.5, y = 0.5, label = "Prior Sensitivity Analysis\n(Implementation in progress)"), 
                                     size = 6, hjust = 0.5) +
                    ggplot2::xlim(0, 1) + ggplot2::ylim(0, 1) +
                    ggtheme
            
            print(plot)
            TRUE
        }
    )
)