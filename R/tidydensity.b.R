#' @title Statistical Distribution Generator and Analyzer
#' @return Interactive distribution analysis using TidyDensity package
#'
#' @importFrom R6 R6Class
#' @import jmvcore
#' @importFrom magrittr %>%
#' @importFrom TidyDensity tidy_normal tidy_beta tidy_gamma tidy_exponential tidy_uniform
#' @importFrom TidyDensity tidy_chisquare tidy_t tidy_f tidy_binomial tidy_poisson
#' @importFrom TidyDensity tidy_weibull tidy_lognormal tidy_logistic tidy_cauchy
#' @importFrom TidyDensity tidy_autoplot tidy_stat_tbl
#' @importFrom ggplot2 ggplot aes geom_histogram geom_density labs theme_minimal
#' @importFrom dplyr summarise group_by
#' @importFrom htmltools HTML
#' @importFrom stringr str_to_title

tidydensityClass <- if (requireNamespace("jmvcore")) R6::R6Class("tidydensityClass",
    inherit = tidydensityBase,
    private = list(

        .run = function() {

            # Show welcome message if no specific distribution configuration
            if (self$options$distribution_type == "normal" && 
                self$options$n_observations == 100 && 
                self$options$n_simulations == 1) {
                
                intro_msg <- "
                <div style='background-color: #e3f2fd; padding: 20px; border-radius: 8px; margin: 20px 0;'>
                <h3 style='color: #1976d2; margin-top: 0;'>📊 Welcome to Statistical Distribution Generator!</h3>
                <p><strong>Comprehensive distribution analysis using TidyDensity package</strong></p>
                <p>Generate random data from various statistical distributions with advanced visualization and analysis</p>
                
                <h4 style='color: #1976d2;'>Available Distributions:</h4>
                <ul>
                <li><strong>Continuous:</strong> Normal, Beta, Gamma, Exponential, Uniform, Chi-Square, t, F, Weibull, Log-Normal, Logistic, Cauchy</li>
                <li><strong>Discrete:</strong> Binomial, Poisson</li>
                </ul>
                
                <h4 style='color: #1976d2;'>Configuration Steps:</h4>
                <ol>
                <li><strong>Select Distribution Type:</strong> Choose from 13 available distributions</li>
                <li><strong>Configure Parameters:</strong> Set distribution-specific parameters</li>
                <li><strong>Set Sample Size:</strong> Number of observations (10-10,000)</li>
                <li><strong>Choose Simulations:</strong> Number of simulation runs (1-20)</li>
                <li><strong>Select Visualization:</strong> Density, Quantile, Probability, or Q-Q plots</li>
                </ol>
                
                <h4 style='color: #1976d2;'>Visualization Options:</h4>
                <ul>
                <li><strong>Density Plot:</strong> Shows probability density function</li>
                <li><strong>Quantile Plot:</strong> Displays quantile relationships</li>
                <li><strong>Probability Plot:</strong> Shows cumulative distribution</li>
                <li><strong>Q-Q Plot:</strong> Compares quantiles against theoretical distribution</li>
                </ul>
                
                <h4 style='color: #1976d2;'>Clinical Research Applications:</h4>
                <ul>
                <li><strong>Power Analysis:</strong> Simulate different scenarios for study design</li>
                <li><strong>Monte Carlo Studies:</strong> Test statistical methods under known conditions</li>
                <li><strong>Distribution Comparison:</strong> Compare empirical data to theoretical distributions</li>
                <li><strong>Statistical Education:</strong> Understand distribution properties and behavior</li>
                <li><strong>Simulation Studies:</strong> Generate realistic datasets for method validation</li>
                </ul>
                
                <h4 style='color: #1976d2;'>Output Features:</h4>
                <ul>
                <li><strong>Interactive Visualizations:</strong> Professional plots with multiple enhancement options</li>
                <li><strong>Comprehensive Statistics:</strong> Mean, variance, skewness, kurtosis, and more</li>
                <li><strong>Summary Tables:</strong> Detailed descriptive statistics by simulation</li>
                <li><strong>Parameter Information:</strong> Distribution-specific parameter explanations</li>
                </ul>
                
                <p style='font-size: 12px; color: #555; margin-top: 20px;'>
                💡 <em>Professional statistical distribution analysis for clinical research and education</em>
                </p>
                </div>"
                
                self$results$todo$setContent(intro_msg)
                return()
            } else {
                self$results$todo$setContent("")
            }

            # Check if TidyDensity package is available
            if (!requireNamespace("TidyDensity", quietly = TRUE)) {
                error_msg <- "
                <div style='color: red; background-color: #ffebee; padding: 20px; border-radius: 8px;'>
                <h4>TidyDensity Package Required</h4>
                <p>The TidyDensity package is required for distribution generation functionality.</p>
                <p>Please install it using: <code>install.packages('TidyDensity')</code></p>
                </div>"
                self$results$interpretation$setContent(error_msg)
                return()
            }

            # Generate distribution data
            distribution_data <- private$.generate_distribution_data()
            
            if (is.null(distribution_data)) {
                return()
            }

            # Prepare plot data
            plot_data <- list(
                data = distribution_data,
                plot_type = self$options$plot_type,
                enhancements = self$options$plot_enhancements,
                distribution_type = self$options$distribution_type,
                n_simulations = self$options$n_simulations
            )
            self$results$main_plot$setState(plot_data)

            # Generate outputs
            if (self$options$show_statistics) {
                stats_html <- private$.generate_distribution_statistics(distribution_data)
                self$results$distribution_statistics$setContent(stats_html)
            }
            
            if (self$options$show_summary_table) {
                summary_html <- private$.generate_summary_table(distribution_data)
                self$results$summary_table$setContent(summary_html)
            }
            
            if (self$options$show_parameter_info) {
                param_html <- private$.generate_parameter_info()
                self$results$parameter_info$setContent(param_html)
            }
            
            if (self$options$show_interpretation) {
                interpretation_html <- private$.generate_interpretation_guide()
                self$results$interpretation$setContent(interpretation_html)
            }

        },

        .plot = function(image, ggtheme, theme, ...) {
            
            plot_data <- image$state
            if (is.null(plot_data)) {
                return()
            }

            if (!requireNamespace("TidyDensity", quietly = TRUE)) {
                return()
            }

            # Create plot using TidyDensity autoplot
            plot <- tryCatch({
                p <- TidyDensity::tidy_autoplot(
                    .data = plot_data$data,
                    .plot_type = plot_data$plot_type,
                    .line_size = 0.7,
                    .geom_point = plot_data$enhancements,
                    .geom_rug = plot_data$enhancements,
                    .geom_smooth = plot_data$enhancements
                )
                
                # Add custom theme and title
                p <- p + 
                    ggplot2::labs(
                        title = paste("Distribution Visualization:", stringr::str_to_title(plot_data$distribution_type)),
                        subtitle = paste("Plot Type:", stringr::str_to_title(plot_data$plot_type)),
                        caption = paste("Number of simulations:", plot_data$n_simulations)
                    ) +
                    ggtheme
                
                p
            }, error = function(e) {
                # Fallback plot if TidyDensity plot fails
                ggplot2::ggplot() + 
                    ggplot2::geom_text(ggplot2::aes(x = 0.5, y = 0.5, label = "Plot generation failed"), 
                                       size = 6) +
                    ggplot2::xlim(0, 1) + ggplot2::ylim(0, 1) +
                    ggplot2::theme_void()
            })

            print(plot)
            TRUE
        },

        .generate_distribution_data = function() {
            
            dist_type <- self$options$distribution_type
            n <- self$options$n_observations
            num_sims <- self$options$n_simulations
            
            # Comprehensive parameter validation
            validation_result <- private$.validate_parameters()
            if (!validation_result$valid) {
                stop(validation_result$message)
            }
            
            tryCatch({
                switch(dist_type,
                    "normal" = TidyDensity::tidy_normal(
                        .n = n, 
                        .mean = self$options$normal_mean, 
                        .sd = self$options$normal_sd,
                        .num_sims = num_sims
                    ),
                    "beta" = TidyDensity::tidy_beta(
                        .n = n,
                        .shape1 = self$options$beta_shape1,
                        .shape2 = self$options$beta_shape2,
                        .num_sims = num_sims
                    ),
                    "gamma" = TidyDensity::tidy_gamma(
                        .n = n,
                        .shape = self$options$gamma_shape,
                        .scale = self$options$gamma_scale,
                        .num_sims = num_sims
                    ),
                    "exponential" = TidyDensity::tidy_exponential(
                        .n = n,
                        .rate = self$options$exponential_rate,
                        .num_sims = num_sims
                    ),
                    "uniform" = TidyDensity::tidy_uniform(
                        .n = n,
                        .min = self$options$uniform_min,
                        .max = self$options$uniform_max,
                        .num_sims = num_sims
                    ),
                    "chisquare" = TidyDensity::tidy_chisquare(
                        .n = n,
                        .df = self$options$chisq_df,
                        .num_sims = num_sims
                    ),
                    "t" = TidyDensity::tidy_t(
                        .n = n,
                        .df = self$options$t_df,
                        .num_sims = num_sims
                    ),
                    "f" = TidyDensity::tidy_f(
                        .n = n,
                        .df1 = self$options$f_df1,
                        .df2 = self$options$f_df2,
                        .num_sims = num_sims
                    ),
                    "binomial" = TidyDensity::tidy_binomial(
                        .n = n,
                        .size = self$options$binomial_size,
                        .prob = self$options$binomial_prob,
                        .num_sims = num_sims
                    ),
                    "poisson" = TidyDensity::tidy_poisson(
                        .n = n,
                        .lambda = self$options$poisson_lambda,
                        .num_sims = num_sims
                    ),
                    "weibull" = TidyDensity::tidy_weibull(
                        .n = n,
                        .shape = self$options$weibull_shape,
                        .scale = self$options$weibull_scale,
                        .num_sims = num_sims
                    ),
                    "lognormal" = TidyDensity::tidy_lognormal(
                        .n = n,
                        .meanlog = self$options$lognormal_meanlog,
                        .sdlog = self$options$lognormal_sdlog,
                        .num_sims = num_sims
                    ),
                    "logistic" = TidyDensity::tidy_logistic(
                        .n = n,
                        .location = self$options$logistic_location,
                        .scale = self$options$logistic_scale,
                        .num_sims = num_sims
                    ),
                    "cauchy" = TidyDensity::tidy_cauchy(
                        .n = n,
                        .location = self$options$cauchy_location,
                        .scale = self$options$cauchy_scale,
                        .num_sims = num_sims
                    ),
                    {
                        # Default to normal if unknown
                        TidyDensity::tidy_normal(.n = n, .num_sims = num_sims)
                    }
                )
            }, error = function(e) {
                NULL
            })
        },

        .generate_distribution_statistics = function(data) {
            
            if (is.null(data)) return("")
            
            tryCatch({
                # Generate statistics using TidyDensity
                stats_tbl <- TidyDensity::tidy_stat_tbl(data)
                
                # Convert to HTML table
                stats_html <- paste0(
                    "<div style='background-color: #f8f9fa; padding: 20px; border-radius: 8px;'>",
                    "<h3 style='color: #495057; margin-top: 0;'>📈 Distribution Statistics</h3>",
                    "<p><strong>Distribution:</strong> ", stringr::str_to_title(self$options$distribution_type), "</p>",
                    "<p><strong>Sample Size:</strong> ", self$options$n_observations, "</p>",
                    "<p><strong>Simulations:</strong> ", self$options$n_simulations, "</p>",
                    
                    "<table style='width: 100%; border-collapse: collapse; margin-top: 15px;'>",
                    "<thead><tr style='background-color: #6c757d; color: white;'>",
                    "<th style='padding: 8px; border: 1px solid #dee2e6;'>Simulation</th>",
                    "<th style='padding: 8px; border: 1px solid #dee2e6;'>Mean</th>",
                    "<th style='padding: 8px; border: 1px solid #dee2e6;'>Variance</th>",
                    "<th style='padding: 8px; border: 1px solid #dee2e6;'>Skewness</th>",
                    "<th style='padding: 8px; border: 1px solid #dee2e6;'>Kurtosis</th>",
                    "</tr></thead><tbody>"
                )
                
                # Add rows for each simulation
                for (i in 1:nrow(stats_tbl)) {
                    row_bg <- if (i %% 2 == 0) "#ffffff" else "#f8f9fa"
                    stats_html <- paste0(stats_html,
                        "<tr style='background-color: ", row_bg, ";'>",
                        "<td style='padding: 8px; border: 1px solid #dee2e6;'>", stats_tbl$sim_number[i], "</td>",
                        "<td style='padding: 8px; border: 1px solid #dee2e6;'>", round(stats_tbl$mean[i], 4), "</td>",
                        "<td style='padding: 8px; border: 1px solid #dee2e6;'>", round(stats_tbl$variance[i], 4), "</td>",
                        "<td style='padding: 8px; border: 1px solid #dee2e6;'>", round(stats_tbl$skewness[i], 4), "</td>",
                        "<td style='padding: 8px; border: 1px solid #dee2e6;'>", round(stats_tbl$kurtosis[i], 4), "</td>",
                        "</tr>"
                    )
                }
                
                stats_html <- paste0(stats_html, "</tbody></table></div>")
                
                return(stats_html)
                
            }, error = function(e) {
                return("<p>Error generating statistics table.</p>")
            })
        },

        .generate_summary_table = function(data) {
            
            if (is.null(data)) return("")
            
            tryCatch({
                # Calculate summary statistics by simulation
                summary_stats <- data %>%
                    dplyr::group_by(sim_number) %>%
                    dplyr::summarise(
                        n = dplyr::n(),
                        min = min(y, na.rm = TRUE),
                        q1 = quantile(y, 0.25, na.rm = TRUE),
                        median = median(y, na.rm = TRUE),
                        q3 = quantile(y, 0.75, na.rm = TRUE),
                        max = max(y, na.rm = TRUE),
                        mean = mean(y, na.rm = TRUE),
                        sd = sd(y, na.rm = TRUE),
                        .groups = "drop"
                    )
                
                summary_html <- paste0(
                    "<div style='background-color: #e8f5e8; padding: 20px; border-radius: 8px;'>",
                    "<h3 style='color: #2e7d32; margin-top: 0;'>📋 Summary Statistics Table</h3>",
                    
                    "<table style='width: 100%; border-collapse: collapse; font-size: 12px;'>",
                    "<thead><tr style='background-color: #388e3c; color: white;'>",
                    "<th style='padding: 6px; border: 1px solid #dee2e6;'>Sim</th>",
                    "<th style='padding: 6px; border: 1px solid #dee2e6;'>N</th>",
                    "<th style='padding: 6px; border: 1px solid #dee2e6;'>Min</th>",
                    "<th style='padding: 6px; border: 1px solid #dee2e6;'>Q1</th>",
                    "<th style='padding: 6px; border: 1px solid #dee2e6;'>Median</th>",
                    "<th style='padding: 6px; border: 1px solid #dee2e6;'>Q3</th>",
                    "<th style='padding: 6px; border: 1px solid #dee2e6;'>Max</th>",
                    "<th style='padding: 6px; border: 1px solid #dee2e6;'>Mean</th>",
                    "<th style='padding: 6px; border: 1px solid #dee2e6;'>SD</th>",
                    "</tr></thead><tbody>"
                )
                
                for (i in 1:nrow(summary_stats)) {
                    row_bg <- if (i %% 2 == 0) "#ffffff" else "#f1f8e9"
                    summary_html <- paste0(summary_html,
                        "<tr style='background-color: ", row_bg, ";'>",
                        "<td style='padding: 6px; border: 1px solid #dee2e6;'>", summary_stats$sim_number[i], "</td>",
                        "<td style='padding: 6px; border: 1px solid #dee2e6;'>", summary_stats$n[i], "</td>",
                        "<td style='padding: 6px; border: 1px solid #dee2e6;'>", round(summary_stats$min[i], 3), "</td>",
                        "<td style='padding: 6px; border: 1px solid #dee2e6;'>", round(summary_stats$q1[i], 3), "</td>",
                        "<td style='padding: 6px; border: 1px solid #dee2e6;'>", round(summary_stats$median[i], 3), "</td>",
                        "<td style='padding: 6px; border: 1px solid #dee2e6;'>", round(summary_stats$q3[i], 3), "</td>",
                        "<td style='padding: 6px; border: 1px solid #dee2e6;'>", round(summary_stats$max[i], 3), "</td>",
                        "<td style='padding: 6px; border: 1px solid #dee2e6;'>", round(summary_stats$mean[i], 3), "</td>",
                        "<td style='padding: 6px; border: 1px solid #dee2e6;'>", round(summary_stats$sd[i], 3), "</td>",
                        "</tr>"
                    )
                }
                
                summary_html <- paste0(summary_html, "</tbody></table></div>")
                return(summary_html)
                
            }, error = function(e) {
                return("<p>Error generating summary table.</p>")
            })
        },

        .generate_parameter_info = function() {
            
            dist_type <- self$options$distribution_type
            
            param_info <- switch(dist_type,
                "normal" = list(
                    title = "Normal (Gaussian) Distribution",
                    description = "The normal distribution is the most important probability distribution in statistics. It is symmetric, bell-shaped, and completely characterized by its mean and standard deviation.",
                    parameters = list(
                        "Mean (μ)" = paste("Location parameter, currently set to:", self$options$normal_mean),
                        "Standard Deviation (σ)" = paste("Scale parameter, currently set to:", self$options$normal_sd)
                    ),
                    applications = "Clinical measurements, biomarkers, measurement errors, many biological variables"
                ),
                "beta" = list(
                    title = "Beta Distribution",
                    description = "The beta distribution is defined on the interval [0,1] and is useful for modeling proportions, probabilities, and percentages.",
                    parameters = list(
                        "Shape 1 (α)" = paste("First shape parameter, currently set to:", self$options$beta_shape1),
                        "Shape 2 (β)" = paste("Second shape parameter, currently set to:", self$options$beta_shape2)
                    ),
                    applications = "Response rates, compliance rates, sensitivity/specificity modeling"
                ),
                "gamma" = list(
                    title = "Gamma Distribution",
                    description = "The gamma distribution is a two-parameter family of continuous probability distributions. It's often used to model waiting times and survival data.",
                    parameters = list(
                        "Shape (k)" = paste("Shape parameter, currently set to:", self$options$gamma_shape),
                        "Scale (θ)" = paste("Scale parameter, currently set to:", self$options$gamma_scale)
                    ),
                    applications = "Survival times, time between events, concentration measurements"
                ),
                "exponential" = list(
                    title = "Exponential Distribution",
                    description = "The exponential distribution models the time between events in a Poisson process. It has the memoryless property.",
                    parameters = list(
                        "Rate (λ)" = paste("Rate parameter (1/mean), currently set to:", self$options$exponential_rate)
                    ),
                    applications = "Time between hospital arrivals, survival analysis, reliability engineering"
                ),
                "uniform" = list(
                    title = "Uniform Distribution",
                    description = "The uniform distribution assigns equal probability to all values within a specified range. All outcomes are equally likely.",
                    parameters = list(
                        "Minimum" = paste("Lower bound, currently set to:", self$options$uniform_min),
                        "Maximum" = paste("Upper bound, currently set to:", self$options$uniform_max)
                    ),
                    applications = "Random sampling, simulation studies, quality control limits"
                ),
                "chisquare" = list(
                    title = "Chi-Square Distribution",
                    description = "The chi-square distribution is used in hypothesis testing and confidence intervals. It's the distribution of the sum of squares of independent standard normal variables.",
                    parameters = list(
                        "Degrees of Freedom (df)" = paste("Shape parameter, currently set to:", self$options$chisq_df)
                    ),
                    applications = "Goodness of fit tests, variance testing, categorical data analysis"
                ),
                "t" = list(
                    title = "Student's t Distribution",
                    description = "The t distribution is used for small sample hypothesis testing and confidence intervals when population variance is unknown.",
                    parameters = list(
                        "Degrees of Freedom (df)" = paste("Shape parameter, currently set to:", self$options$t_df)
                    ),
                    applications = "Small sample testing, confidence intervals, regression analysis"
                ),
                "f" = list(
                    title = "F Distribution",
                    description = "The F distribution is used for comparing variances and in ANOVA. It's the ratio of two chi-square distributions.",
                    parameters = list(
                        "Numerator DF" = paste("Degrees of freedom for numerator, currently set to:", self$options$f_df1),
                        "Denominator DF" = paste("Degrees of freedom for denominator, currently set to:", self$options$f_df2)
                    ),
                    applications = "ANOVA, variance comparison, regression model testing"
                ),
                "binomial" = list(
                    title = "Binomial Distribution",
                    description = "The binomial distribution models the number of successes in a fixed number of independent trials with constant success probability.",
                    parameters = list(
                        "Number of Trials (n)" = paste("Fixed number of trials, currently set to:", self$options$binomial_size),
                        "Success Probability (p)" = paste("Probability of success per trial, currently set to:", self$options$binomial_prob)
                    ),
                    applications = "Clinical trial success rates, diagnostic test performance, treatment response"
                ),
                "poisson" = list(
                    title = "Poisson Distribution",
                    description = "The Poisson distribution models the number of events occurring in a fixed interval of time or space.",
                    parameters = list(
                        "Rate (λ)" = paste("Average rate of occurrence, currently set to:", self$options$poisson_lambda)
                    ),
                    applications = "Adverse event counting, hospital arrivals, infection rates"
                ),
                "weibull" = list(
                    title = "Weibull Distribution",
                    description = "The Weibull distribution is widely used in survival analysis and reliability engineering. It can model various hazard rate patterns.",
                    parameters = list(
                        "Shape (k)" = paste("Shape parameter controlling distribution form, currently set to:", self$options$weibull_shape),
                        "Scale (λ)" = paste("Scale parameter affecting spread, currently set to:", self$options$weibull_scale)
                    ),
                    applications = "Survival analysis, time-to-failure, reliability studies"
                ),
                "lognormal" = list(
                    title = "Log-Normal Distribution",
                    description = "The log-normal distribution is the distribution of a variable whose logarithm is normally distributed. Common in biological and economic data.",
                    parameters = list(
                        "Mean of Log" = paste("Mean of the underlying normal distribution, currently set to:", self$options$lognormal_meanlog),
                        "SD of Log" = paste("Standard deviation of the underlying normal distribution, currently set to:", self$options$lognormal_sdlog)
                    ),
                    applications = "Drug concentrations, income distributions, biological measurements"
                ),
                "logistic" = list(
                    title = "Logistic Distribution",
                    description = "The logistic distribution is similar to the normal distribution but has heavier tails. Used in logistic regression and growth models.",
                    parameters = list(
                        "Location" = paste("Location parameter (similar to mean), currently set to:", self$options$logistic_location),
                        "Scale" = paste("Scale parameter affecting spread, currently set to:", self$options$logistic_scale)
                    ),
                    applications = "Logistic regression, growth models, dose-response relationships"
                ),
                "cauchy" = list(
                    title = "Cauchy Distribution",
                    description = "The Cauchy distribution has no defined mean or variance. It has very heavy tails and is used to model extreme events.",
                    parameters = list(
                        "Location" = paste("Location parameter (median), currently set to:", self$options$cauchy_location),
                        "Scale" = paste("Scale parameter affecting spread, currently set to:", self$options$cauchy_scale)
                    ),
                    applications = "Robust statistics, extreme value modeling, physics applications"
                ),
                # Default case
                list(
                    title = paste(stringr::str_to_title(dist_type), "Distribution"),
                    description = paste("Information about", dist_type, "distribution."),
                    parameters = list(),
                    applications = "Various statistical and clinical applications"
                )
            )
            
            html_content <- paste0(
                "<div style='background-color: #fff3e0; padding: 20px; border-radius: 8px;'>",
                "<h3 style='color: #ef6c00; margin-top: 0;'>📚 ", param_info$title, "</h3>",
                "<p><strong>Description:</strong> ", param_info$description, "</p>",
                
                "<h4 style='color: #ef6c00;'>Current Parameters:</h4>",
                "<ul>"
            )
            
            for (param_name in names(param_info$parameters)) {
                html_content <- paste0(html_content,
                    "<li><strong>", param_name, ":</strong> ", param_info$parameters[[param_name]], "</li>"
                )
            }
            
            html_content <- paste0(html_content,
                "</ul>",
                "<h4 style='color: #ef6c00;'>Clinical Applications:</h4>",
                "<p>", param_info$applications, "</p>",
                "</div>"
            )
            
            return(html_content)
        },

        .generate_interpretation_guide = function() {
            
            interpretation_html <- paste0(
                "<div style='background-color: #f3e5f5; padding: 20px; border-radius: 8px;'>",
                "<h3 style='color: #7b1fa2; margin-top: 0;'>📖 Statistical Distribution Guide</h3>",
                
                "<h4 style='color: #7b1fa2;'>Understanding Plot Types:</h4>",
                "<ul>",
                "<li><strong>Density Plot:</strong> Shows the probability density function - where values are most likely to occur</li>",
                "<li><strong>Quantile Plot:</strong> Displays the quantile function - useful for understanding percentiles</li>",
                "<li><strong>Probability Plot:</strong> Shows cumulative distribution function - probability that X ≤ x</li>",
                "<li><strong>Q-Q Plot:</strong> Compares sample quantiles to theoretical quantiles - tests distributional assumptions</li>",
                "</ul>",
                
                "<h4 style='color: #7b1fa2;'>Clinical Research Applications:</h4>",
                "<ul>",
                "<li><strong>Power Analysis:</strong> Simulate different effect sizes and sample sizes to determine study power</li>",
                "<li><strong>Monte Carlo Studies:</strong> Test robustness of statistical methods under various conditions</li>",
                "<li><strong>Biomarker Modeling:</strong> Model distributions of clinical biomarkers and laboratory values</li>",
                "<li><strong>Quality Control:</strong> Understand expected variation in laboratory measurements</li>",
                "<li><strong>Risk Assessment:</strong> Model probability distributions for clinical outcomes</li>",
                "</ul>",
                
                "<h4 style='color: #7b1fa2;'>Choosing the Right Distribution:</h4>",
                "<ul>",
                "<li><strong>Normal:</strong> Continuous measurements, many biological variables, measurement errors</li>",
                "<li><strong>Beta:</strong> Proportions, rates, probabilities (values between 0 and 1)</li>",
                "<li><strong>Gamma/Exponential:</strong> Waiting times, survival data, positive continuous variables</li>",
                "<li><strong>Binomial:</strong> Number of successes in fixed number of trials</li>",
                "<li><strong>Poisson:</strong> Count data, number of events in fixed time period</li>",
                "</ul>",
                
                "<h4 style='color: #7b1fa2;'>Simulation Best Practices:</h4>",
                "<ul>",
                "<li><strong>Sample Size:</strong> Use realistic sample sizes for your research context</li>",
                "<li><strong>Multiple Simulations:</strong> Run multiple simulations to understand sampling variability</li>",
                "<li><strong>Parameter Sensitivity:</strong> Test different parameter values to understand their impact</li>",
                "<li><strong>Validation:</strong> Compare simulated data characteristics to real data when available</li>",
                "</ul>",
                
                "<h4 style='color: #7b1fa2;'>Statistical Education Tips:</h4>",
                "<ul>",
                "<li>Observe how changing parameters affects distribution shape and location</li>",
                "<li>Compare different distributions with similar means and variances</li>",
                "<li>Use Q-Q plots to understand how real data deviates from theoretical distributions</li>",
                "<li>Practice interpreting statistical measures like skewness and kurtosis</li>",
                "</ul>",
                
                "<p style='font-size: 12px; color: #7b1fa2; margin-top: 15px;'>",
                "<em>📊 Professional statistical distribution analysis powered by TidyDensity</em>",
                "</p></div>"
            )
            
            return(interpretation_html)
        },
        
        .validate_parameters = function() {
            dist_type <- self$options$distribution_type
            
            # Validate based on distribution type
            switch(dist_type,
                "normal" = {
                    if (self$options$normal_sd <= 0) {
                        return(list(valid = FALSE, message = "Normal distribution: Standard deviation must be positive"))
                    }
                },
                "beta" = {
                    if (self$options$beta_shape1 <= 0 || self$options$beta_shape2 <= 0) {
                        return(list(valid = FALSE, message = "Beta distribution: Both shape parameters must be positive"))
                    }
                },
                "gamma" = {
                    if (self$options$gamma_shape <= 0 || self$options$gamma_scale <= 0) {
                        return(list(valid = FALSE, message = "Gamma distribution: Shape and scale parameters must be positive"))
                    }
                },
                "exponential" = {
                    if (self$options$exponential_rate <= 0) {
                        return(list(valid = FALSE, message = "Exponential distribution: Rate parameter must be positive"))
                    }
                },
                "uniform" = {
                    if (self$options$uniform_min >= self$options$uniform_max) {
                        return(list(valid = FALSE, message = "Uniform distribution: Maximum value must be greater than minimum value"))
                    }
                },
                "chisquare" = {
                    if (self$options$chisq_df <= 0) {
                        return(list(valid = FALSE, message = "Chi-square distribution: Degrees of freedom must be positive"))
                    }
                },
                "t" = {
                    if (self$options$t_df <= 0) {
                        return(list(valid = FALSE, message = "t distribution: Degrees of freedom must be positive"))
                    }
                },
                "f" = {
                    if (self$options$f_df1 <= 0 || self$options$f_df2 <= 0) {
                        return(list(valid = FALSE, message = "F distribution: Both degrees of freedom must be positive"))
                    }
                },
                "binomial" = {
                    if (self$options$binomial_size <= 0) {
                        return(list(valid = FALSE, message = "Binomial distribution: Number of trials must be positive"))
                    }
                    if (self$options$binomial_prob <= 0 || self$options$binomial_prob >= 1) {
                        return(list(valid = FALSE, message = "Binomial distribution: Probability must be between 0 and 1"))
                    }
                },
                "poisson" = {
                    if (self$options$poisson_lambda <= 0) {
                        return(list(valid = FALSE, message = "Poisson distribution: Lambda parameter must be positive"))
                    }
                },
                "weibull" = {
                    if (self$options$weibull_shape <= 0 || self$options$weibull_scale <= 0) {
                        return(list(valid = FALSE, message = "Weibull distribution: Shape and scale parameters must be positive"))
                    }
                },
                "lognormal" = {
                    if (self$options$lognormal_sdlog <= 0) {
                        return(list(valid = FALSE, message = "Log-normal distribution: Standard deviation of log must be positive"))
                    }
                },
                "logistic" = {
                    if (self$options$logistic_scale <= 0) {
                        return(list(valid = FALSE, message = "Logistic distribution: Scale parameter must be positive"))
                    }
                },
                "cauchy" = {
                    if (self$options$cauchy_scale <= 0) {
                        return(list(valid = FALSE, message = "Cauchy distribution: Scale parameter must be positive"))
                    }
                }
            )
            
            # General validations
            if (self$options$n_observations < 10 || self$options$n_observations > 10000) {
                return(list(valid = FALSE, message = "Number of observations must be between 10 and 10,000"))
            }
            
            if (self$options$n_simulations < 1 || self$options$n_simulations > 20) {
                return(list(valid = FALSE, message = "Number of simulations must be between 1 and 20"))
            }
            
            return(list(valid = TRUE, message = "Parameters are valid"))
        }

    )
)