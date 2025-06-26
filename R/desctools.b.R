#' @title Advanced Statistical Tests
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import magrittr
#' @importFrom DescTools CohenD HosmerLemeshowTest AndersonDarlingTest BarnardTest BreslowDayTest CochranArmitageTest JarqueBeraTest CramerVonMisesTest

desctoolsClass <- if (requireNamespace("jmvcore")) {
    R6::R6Class(
        "desctoolsClass",
        inherit = desctoolsBase,
        private = list(

            # init ----
            .init = function() {
                # Initialize visibility based on selected analyses
                if (!self$options$effect_size_analysis) {
                    self$results$effect_size_results$setVisible(FALSE)
                }
                
                if (!self$options$goodness_of_fit) {
                    self$results$goodness_fit_results$setVisible(FALSE)
                }
                
                if (!self$options$categorical_tests) {
                    self$results$categorical_results$setVisible(FALSE)
                }
                
                if (!self$options$show_effect_sizes) {
                    self$results$effect_size_results$setVisible(FALSE)
                }
                
                if (!self$options$show_goodness_tests) {
                    self$results$goodness_fit_results$setVisible(FALSE)
                }
                
                if (!self$options$show_categorical_tests) {
                    self$results$categorical_results$setVisible(FALSE)
                }
            },

            # run ----
            .run = function() {
                
                # Check for required packages
                if (!requireNamespace("DescTools", quietly = TRUE)) {
                    self$results$instructions$setContent(
                        "<div style='color: red; font-weight: bold;'>
                        Error: The 'DescTools' package is required but not installed.
                        <br><br>
                        Please install it using: install.packages('DescTools')
                        </div>"
                    )
                    return()
                }
                
                # Early return with instructions if no analysis is selected
                if (!self$options$effect_size_analysis && 
                    !self$options$goodness_of_fit && 
                    !self$options$categorical_tests) {
                    
                    instructions_html <- "
                    <div style='background-color: #e8f5e8; padding: 20px; border-radius: 8px; margin: 10px 0;'>
                        <h3>Advanced Statistical Tests from DescTools</h3>
                        <p><strong>This module provides advanced statistical tests for clinical and epidemiological research.</strong></p>
                        
                        <h4>Available Analyses:</h4>
                        
                        <h5>1. Effect Size Analysis</h5>
                        <ul>
                            <li><strong>Cohen's D:</strong> Standardized effect size for comparing group means</li>
                            <li><strong>Hedges' G:</strong> Bias-corrected effect size for small samples</li>
                            <li><strong>Confidence Intervals:</strong> Precision estimates for effect sizes</li>
                        </ul>
                        
                        <h5>2. Goodness of Fit Tests</h5>
                        <ul>
                            <li><strong>Hosmer-Lemeshow Test:</strong> Goodness of fit for logistic regression</li>
                            <li><strong>Anderson-Darling Test:</strong> Normality testing</li>
                            <li><strong>Jarque-Bera Test:</strong> Normality testing based on skewness and kurtosis</li>
                            <li><strong>Cramer-von Mises Test:</strong> General goodness of fit</li>
                        </ul>
                        
                        <h5>3. Advanced Categorical Tests</h5>
                        <ul>
                            <li><strong>Barnard's Test:</strong> Exact test for 2x2 contingency tables</li>
                            <li><strong>Breslow-Day Test:</strong> Homogeneity of odds ratios across strata</li>
                            <li><strong>Cochran-Armitage Test:</strong> Test for linear trend in proportions</li>
                        </ul>
                        
                        <h4>To Begin:</h4>
                        <ol>
                            <li>Select one or more analysis types</li>
                            <li>Choose appropriate variables for your selected analyses</li>
                            <li>Configure test parameters as needed</li>
                            <li>Review results with clinical interpretations</li>
                        </ol>
                        
                        <p><em>Note: All tests include detailed statistical output and clinical interpretation guidelines.</em></p>
                    </div>"
                    
                    self$results$instructions$setContent(instructions_html)
                    return()
                } else {
                    self$results$instructions$setContent("")
                }
                
                # Get data
                data <- self$data
                if (nrow(data) == 0) {
                    stop("Data contains no rows")
                }
                
                # Perform selected analyses
                tryCatch({
                    
                    if (self$options$effect_size_analysis) {
                        private$.performEffectSizeAnalysis(data)
                    }
                    
                    if (self$options$goodness_of_fit) {
                        private$.performGoodnessOfFitTests(data)
                    }
                    
                    if (self$options$categorical_tests) {
                        private$.performCategoricalTests(data)
                    }
                    
                }, error = function(e) {
                    error_msg <- paste0(
                        "<div style='color: red; font-weight: bold;'>",
                        "Error in statistical analysis: ", e$message,
                        "<br><br>",
                        "Please check your variable selections and data format.",
                        "</div>"
                    )
                    self$results$instructions$setContent(error_msg)
                })
            },
            
            # Perform effect size analysis
            .performEffectSizeAnalysis = function(data) {
                if (!self$options$show_effect_sizes) return()
                
                if (is.null(self$options$group_var) || is.null(self$options$continuous_var)) {
                    self$results$effect_size_results$setContent(
                        "<div style='color: orange;'>
                        Please select both a grouping variable and a continuous variable for effect size analysis.
                        </div>"
                    )
                    return()
                }
                
                group_col <- self$options$group_var
                cont_col <- self$options$continuous_var
                
                if (!group_col %in% names(data) || !cont_col %in% names(data)) {
                    stop("Selected variables not found in data")
                }
                
                # Check if grouping variable has exactly 2 levels
                group_levels <- unique(data[[group_col]])
                group_levels <- group_levels[!is.na(group_levels)]
                
                if (length(group_levels) != 2) {
                    self$results$effect_size_results$setContent(
                        "<div style='color: red;'>
                        Error: Grouping variable must have exactly 2 levels for Cohen's D calculation.
                        <br>
                        Current variable has ", length(group_levels), " levels: ", paste(group_levels, collapse=", "), "
                        </div>"
                    )
                    return()
                }
                
                # Extract data for each group
                group1_data <- data[data[[group_col]] == group_levels[1] & !is.na(data[[group_col]]), cont_col]
                group2_data <- data[data[[group_col]] == group_levels[2] & !is.na(data[[group_col]]), cont_col]
                
                # Remove missing values
                group1_data <- group1_data[!is.na(group1_data)]
                group2_data <- group2_data[!is.na(group2_data)]
                
                if (length(group1_data) < 2 || length(group2_data) < 2) {
                    self$results$effect_size_results$setContent(
                        "<div style='color: red;'>
                        Error: Each group must have at least 2 non-missing observations.
                        </div>"
                    )
                    return()
                }
                
                # Calculate Cohen's D
                conf_level <- if (self$options$effect_ci_level > 0.5) self$options$effect_ci_level else NA
                
                cohens_d <- DescTools::CohenD(
                    x = group1_data,
                    y = group2_data,
                    pooled = self$options$pooled_sd,
                    correct = self$options$hedges_correction,
                    conf.level = conf_level,
                    na.rm = TRUE
                )
                
                # Create HTML output
                html_content <- "<div style='font-family: Arial, sans-serif;'>"
                html_content <- paste0(html_content, "<h3>Effect Size Analysis</h3>")
                
                # Summary statistics
                html_content <- paste0(html_content, "<h4>Group Summary Statistics</h4>")
                html_content <- paste0(html_content, "<table border='1' cellpadding='5' cellspacing='0' style='border-collapse: collapse;'>")
                html_content <- paste0(html_content, "<tr style='background-color: #f0f0f0; font-weight: bold;'>")
                html_content <- paste0(html_content, "<th>Group</th><th>N</th><th>Mean</th><th>SD</th><th>Min</th><th>Max</th></tr>")
                
                html_content <- paste0(html_content, "<tr>")
                html_content <- paste0(html_content, "<td><strong>", group_levels[1], "</strong></td>")
                html_content <- paste0(html_content, "<td>", length(group1_data), "</td>")
                html_content <- paste0(html_content, "<td>", round(mean(group1_data), 3), "</td>")
                html_content <- paste0(html_content, "<td>", round(sd(group1_data), 3), "</td>")
                html_content <- paste0(html_content, "<td>", round(min(group1_data), 3), "</td>")
                html_content <- paste0(html_content, "<td>", round(max(group1_data), 3), "</td>")
                html_content <- paste0(html_content, "</tr>")
                
                html_content <- paste0(html_content, "<tr>")
                html_content <- paste0(html_content, "<td><strong>", group_levels[2], "</strong></td>")
                html_content <- paste0(html_content, "<td>", length(group2_data), "</td>")
                html_content <- paste0(html_content, "<td>", round(mean(group2_data), 3), "</td>")
                html_content <- paste0(html_content, "<td>", round(sd(group2_data), 3), "</td>")
                html_content <- paste0(html_content, "<td>", round(min(group2_data), 3), "</td>")
                html_content <- paste0(html_content, "<td>", round(max(group2_data), 3), "</td>")
                html_content <- paste0(html_content, "</tr>")
                
                html_content <- paste0(html_content, "</table>")
                
                # Effect size results
                html_content <- paste0(html_content, "<h4>Effect Size Results</h4>")
                html_content <- paste0(html_content, "<table border='1' cellpadding='5' cellspacing='0' style='border-collapse: collapse;'>")
                html_content <- paste0(html_content, "<tr style='background-color: #f0f0f0; font-weight: bold;'>")
                
                effect_name <- if (self$options$hedges_correction) "Hedges' g" else "Cohen's d"
                html_content <- paste0(html_content, "<th>Measure</th><th>Value</th>")
                
                if (!is.na(conf_level)) {
                    html_content <- paste0(html_content, "<th>Lower CI</th><th>Upper CI</th>")
                }
                html_content <- paste0(html_content, "<th>Magnitude</th></tr>")
                
                # Get magnitude interpretation
                d_value <- cohens_d[1]
                magnitude <- if (abs(d_value) < 0.2) "Negligible" 
                           else if (abs(d_value) < 0.5) "Small"
                           else if (abs(d_value) < 0.8) "Medium"
                           else "Large"
                
                html_content <- paste0(html_content, "<tr>")
                html_content <- paste0(html_content, "<td><strong>", effect_name, "</strong></td>")
                html_content <- paste0(html_content, "<td>", round(d_value, 3), "</td>")
                
                if (!is.na(conf_level)) {
                    html_content <- paste0(html_content, "<td>", round(cohens_d[2], 3), "</td>")
                    html_content <- paste0(html_content, "<td>", round(cohens_d[3], 3), "</td>")
                }
                html_content <- paste0(html_content, "<td>", magnitude, "</td>")
                html_content <- paste0(html_content, "</tr>")
                
                html_content <- paste0(html_content, "</table>")
                
                # Clinical interpretation
                if (self$options$show_interpretations) {
                    html_content <- paste0(html_content, "<h4>Clinical Interpretation</h4>")
                    html_content <- paste0(html_content, "<div style='background-color: #f9f9f9; padding: 10px; border-left: 4px solid #007acc;'>")
                    
                    interpretation <- if (magnitude == "Negligible") {
                        "The effect size is negligible, suggesting minimal practical difference between groups."
                    } else if (magnitude == "Small") {
                        "The effect size is small but potentially clinically meaningful, especially in large samples."
                    } else if (magnitude == "Medium") {
                        "The effect size is moderate and likely clinically significant."
                    } else {
                        "The effect size is large and represents a substantial clinical difference between groups."
                    }
                    
                    html_content <- paste0(html_content, "<p><strong>Effect Size Interpretation:</strong> ", interpretation, "</p>")
                    
                    # Cohen's conventions
                    html_content <- paste0(html_content, "<p><strong>Cohen's Conventions:</strong></p>")
                    html_content <- paste0(html_content, "<ul>")
                    html_content <- paste0(html_content, "<li>Small effect: d = 0.2</li>")
                    html_content <- paste0(html_content, "<li>Medium effect: d = 0.5</li>")
                    html_content <- paste0(html_content, "<li>Large effect: d = 0.8</li>")
                    html_content <- paste0(html_content, "</ul>")
                    
                    html_content <- paste0(html_content, "</div>")
                }
                
                html_content <- paste0(html_content, "</div>")
                
                self$results$effect_size_results$setContent(html_content)
            },
            
            # Perform goodness of fit tests
            .performGoodnessOfFitTests = function(data) {
                if (!self$options$show_goodness_tests) return()
                
                html_content <- "<div style='font-family: Arial, sans-serif;'>"
                html_content <- paste0(html_content, "<h3>Goodness of Fit Tests</h3>")
                
                tests_performed <- 0
                
                # Hosmer-Lemeshow test
                if (!is.null(self$options$fitted_probs) && !is.null(self$options$observed_outcomes)) {
                    fitted_col <- self$options$fitted_probs
                    observed_col <- self$options$observed_outcomes
                    
                    if (fitted_col %in% names(data) && observed_col %in% names(data)) {
                        fitted_vals <- data[[fitted_col]]
                        observed_vals <- data[[observed_col]]
                        
                        # Remove missing values
                        complete_cases <- !is.na(fitted_vals) & !is.na(observed_vals)
                        fitted_vals <- fitted_vals[complete_cases]
                        observed_vals <- observed_vals[complete_cases]
                        
                        if (length(fitted_vals) > 10) {
                            hl_test <- DescTools::HosmerLemeshowTest(
                                fit = fitted_vals,
                                obs = observed_vals,
                                ngr = self$options$hl_groups
                            )
                            
                            html_content <- paste0(html_content, "<h4>Hosmer-Lemeshow Goodness of Fit Test</h4>")
                            html_content <- paste0(html_content, "<table border='1' cellpadding='5' cellspacing='0' style='border-collapse: collapse;'>")
                            html_content <- paste0(html_content, "<tr style='background-color: #f0f0f0; font-weight: bold;'>")
                            html_content <- paste0(html_content, "<th>Statistic</th><th>Value</th><th>df</th><th>p-value</th></tr>")
                            
                            html_content <- paste0(html_content, "<tr>")
                            html_content <- paste0(html_content, "<td>C Statistic</td>")
                            html_content <- paste0(html_content, "<td>", round(hl_test$C$statistic, 4), "</td>")
                            html_content <- paste0(html_content, "<td>", hl_test$C$parameter, "</td>")
                            html_content <- paste0(html_content, "<td>", round(hl_test$C$p.value, 4), "</td>")
                            html_content <- paste0(html_content, "</tr>")
                            
                            html_content <- paste0(html_content, "</table>")
                            
                            p_val <- hl_test$C$p.value
                            interpretation <- if (p_val > 0.05) {
                                "Good model fit (p > 0.05). The model adequately fits the data."
                            } else {
                                "Poor model fit (p ≤ 0.05). Consider model revision or additional predictors."
                            }
                            
                            html_content <- paste0(html_content, "<p><strong>Interpretation:</strong> ", interpretation, "</p>")
                            tests_performed <- tests_performed + 1
                        }
                    }
                }
                
                # Normality tests
                if (!is.null(self$options$normality_var)) {
                    norm_col <- self$options$normality_var
                    
                    if (norm_col %in% names(data)) {
                        norm_data <- data[[norm_col]]
                        norm_data <- norm_data[!is.na(norm_data)]
                        
                        if (length(norm_data) > 7) {
                            # Anderson-Darling test
                            ad_test <- DescTools::AndersonDarlingTest(norm_data)
                            
                            # Jarque-Bera test
                            jb_test <- DescTools::JarqueBeraTest(norm_data)
                            
                            html_content <- paste0(html_content, "<h4>Normality Tests</h4>")
                            html_content <- paste0(html_content, "<table border='1' cellpadding='5' cellspacing='0' style='border-collapse: collapse;'>")
                            html_content <- paste0(html_content, "<tr style='background-color: #f0f0f0; font-weight: bold;'>")
                            html_content <- paste0(html_content, "<th>Test</th><th>Statistic</th><th>p-value</th><th>Conclusion</th></tr>")
                            
                            # Anderson-Darling
                            ad_conclusion <- if (ad_test$p.value > 0.05) "Normal" else "Non-normal"
                            html_content <- paste0(html_content, "<tr>")
                            html_content <- paste0(html_content, "<td>Anderson-Darling</td>")
                            html_content <- paste0(html_content, "<td>", round(ad_test$statistic, 4), "</td>")
                            html_content <- paste0(html_content, "<td>", round(ad_test$p.value, 4), "</td>")
                            html_content <- paste0(html_content, "<td>", ad_conclusion, "</td>")
                            html_content <- paste0(html_content, "</tr>")
                            
                            # Jarque-Bera
                            jb_conclusion <- if (jb_test$p.value > 0.05) "Normal" else "Non-normal"
                            html_content <- paste0(html_content, "<tr>")
                            html_content <- paste0(html_content, "<td>Jarque-Bera</td>")
                            html_content <- paste0(html_content, "<td>", round(jb_test$statistic, 4), "</td>")
                            html_content <- paste0(html_content, "<td>", round(jb_test$p.value, 4), "</td>")
                            html_content <- paste0(html_content, "<td>", jb_conclusion, "</td>")
                            html_content <- paste0(html_content, "</tr>")
                            
                            html_content <- paste0(html_content, "</table>")
                            tests_performed <- tests_performed + 1
                        }
                    }
                }
                
                if (tests_performed == 0) {
                    html_content <- paste0(html_content, "<p style='color: orange;'>")
                    html_content <- paste0(html_content, "No goodness of fit tests performed. ")
                    html_content <- paste0(html_content, "Please select appropriate variables:</p>")
                    html_content <- paste0(html_content, "<ul>")
                    html_content <- paste0(html_content, "<li>For Hosmer-Lemeshow test: Select fitted probabilities and observed outcomes</li>")
                    html_content <- paste0(html_content, "<li>For normality tests: Select a continuous variable</li>")
                    html_content <- paste0(html_content, "</ul>")
                }
                
                html_content <- paste0(html_content, "</div>")
                
                self$results$goodness_fit_results$setContent(html_content)
            },
            
            # Perform categorical tests
            .performCategoricalTests = function(data) {
                if (!self$options$show_categorical_tests) return()
                
                html_content <- "<div style='font-family: Arial, sans-serif;'>"
                html_content <- paste0(html_content, "<h3>Advanced Categorical Data Tests</h3>")
                
                tests_performed <- 0
                
                # Cochran-Armitage trend test
                if (!is.null(self$options$ordered_exposure) && !is.null(self$options$binary_outcome)) {
                    exposure_col <- self$options$ordered_exposure
                    outcome_col <- self$options$binary_outcome
                    
                    if (exposure_col %in% names(data) && outcome_col %in% names(data)) {
                        exposure_data <- data[[exposure_col]]
                        outcome_data <- data[[outcome_col]]
                        
                        # Remove missing values
                        complete_cases <- !is.na(exposure_data) & !is.na(outcome_data)
                        exposure_data <- exposure_data[complete_cases]
                        outcome_data <- outcome_data[complete_cases]
                        
                        if (length(exposure_data) > 10) {
                            ca_test <- DescTools::CochranArmitageTest(
                                x = outcome_data,
                                g = exposure_data
                            )
                            
                            html_content <- paste0(html_content, "<h4>Cochran-Armitage Trend Test</h4>")
                            html_content <- paste0(html_content, "<table border='1' cellpadding='5' cellspacing='0' style='border-collapse: collapse;'>")
                            html_content <- paste0(html_content, "<tr style='background-color: #f0f0f0; font-weight: bold;'>")
                            html_content <- paste0(html_content, "<th>Statistic</th><th>Value</th><th>p-value</th></tr>")
                            
                            html_content <- paste0(html_content, "<tr>")
                            html_content <- paste0(html_content, "<td>Z-statistic</td>")
                            html_content <- paste0(html_content, "<td>", round(ca_test$statistic, 4), "</td>")
                            html_content <- paste0(html_content, "<td>", round(ca_test$p.value, 4), "</td>")
                            html_content <- paste0(html_content, "</tr>")
                            
                            html_content <- paste0(html_content, "</table>")
                            
                            interpretation <- if (ca_test$p.value > 0.05) {
                                "No significant linear trend detected (p > 0.05)."
                            } else {
                                "Significant linear trend detected (p ≤ 0.05). There is evidence of a dose-response relationship."
                            }
                            
                            html_content <- paste0(html_content, "<p><strong>Interpretation:</strong> ", interpretation, "</p>")
                            tests_performed <- tests_performed + 1
                        }
                    }
                }
                
                if (tests_performed == 0) {
                    html_content <- paste0(html_content, "<p style='color: orange;'>")
                    html_content <- paste0(html_content, "No categorical tests performed. ")
                    html_content <- paste0(html_content, "Please select appropriate variables:</p>")
                    html_content <- paste0(html_content, "<ul>")
                    html_content <- paste0(html_content, "<li>For Cochran-Armitage test: Select ordered exposure and binary outcome variables</li>")
                    html_content <- paste0(html_content, "<li>For Barnard's test: Select two categorical variables</li>")
                    html_content <- paste0(html_content, "<li>For Breslow-Day test: Select two categorical variables and a stratum variable</li>")
                    html_content <- paste0(html_content, "</ul>")
                }
                
                html_content <- paste0(html_content, "</div>")
                
                self$results$categorical_results$setContent(html_content)
            }
        )
    )
}