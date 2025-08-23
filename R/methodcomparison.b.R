methodcomparisonClass <- R6::R6Class(
    "methodcomparisonClass",
    inherit = methodcomparisonBase,
    private = list(
        .init = function() {
            if (is.null(self$options$method1) || is.null(self$options$method2)) {
                self$results$todo$setContent(
                    "<html>
                    <head>
                    <style>
                        .todo {
                            color: #3498db;
                            font-weight: bold;
                            font-size: 16px;
                        }
                        .instructions {
                            margin-top: 15px;
                            color: #555;
                        }
                        .step {
                            margin: 8px 0;
                            padding: 8px;
                            background-color: #f8f9fa;
                            border-left: 3px solid #3498db;
                        }
                    </style>
                    </head>
                    <body>
                    <div class='todo'>Welcome to Method Comparison Analysis!</div>
                    <div class='instructions'>
                        <div class='step'><strong>Required:</strong> Assign continuous variables to 'Reference Method (X)' and 'Test Method (Y)'</div>
                        <div class='step'><strong>Optional:</strong> Add grouping variable for stratified analysis</div>
                        <div class='step'><strong>Choose Method:</strong> Select Bland-Altman, Passing-Bablok, Deming, or All Methods</div>
                        <div class='step'><strong>Clinical Assessment:</strong> Set clinical acceptability limits if needed</div>
                        <div class='step'><strong>Visualization:</strong> Enable plots for comprehensive analysis</div>
                    </div>
                    <p><em>Method comparison analysis validates new measurement techniques against reference standards using Bland-Altman plots, Passing-Bablok regression, and Deming regression.</em></p>
                    </body>
                    </html>"
                )
            }
        },

        .run = function() {
            if (is.null(self$options$method1) || is.null(self$options$method2))
                return()

            # Get data
            method1 <- self$data[[self$options$method1]]
            method2 <- self$data[[self$options$method2]]
            
            if (length(method1) == 0 || length(method2) == 0)
                return()

            # Handle missing data
            if (self$options$missing_treatment == "complete") {
                complete_cases <- complete.cases(method1, method2)
                method1 <- method1[complete_cases]
                method2 <- method2[complete_cases]
            }

            if (length(method1) < 10) {
                self$results$summary$setContent("<p>Insufficient data for analysis. Need at least 10 complete observations.</p>")
                return()
            }

            # Apply transformation if specified
            if (self$options$transformation == "log") {
                if (any(method1 <= 0) || any(method2 <= 0)) {
                    self$results$summary$setContent("<p>Log transformation requires all positive values.</p>")
                    return()
                }
                method1 <- log(method1)
                method2 <- log(method2)
            } else if (self$options$transformation == "sqrt") {
                if (any(method1 < 0) || any(method2 < 0)) {
                    self$results$summary$setContent("<p>Square root transformation requires non-negative values.</p>")
                    return()
                }
                method1 <- sqrt(method1)
                method2 <- sqrt(method2)
            }

            # Analysis summary
            self$results$summary$setContent(paste0(
                "<h3>Method Comparison Analysis Summary</h3>",
                "<p><strong>Sample size:</strong> ", length(method1), " paired observations</p>",
                "<p><strong>Method:</strong> ", switch(self$options$comparison_method,
                    "bland_altman" = "Bland-Altman Analysis",
                    "passing_bablok" = "Passing-Bablok Regression", 
                    "deming" = "Deming Regression",
                    "all" = "All Methods"), "</p>",
                if (self$options$transformation != "none") 
                    paste0("<p><strong>Transformation:</strong> ", self$options$transformation, "</p>") else "",
                "<p><strong>Confidence level:</strong> ", self$options$confidence_level * 100, "%</p>"
            ))

            # Descriptive statistics
            private$.populateDescriptiveStats(method1, method2)

            # Correlation analysis
            if (self$options$correlation_analysis) {
                private$.populateCorrelationTable(method1, method2)
            }
            
            # Concordance correlation coefficient
            if (self$options$concordance_correlation) {
                private$.populateConcordanceTable(method1, method2)
            }

            # Method-specific analyses
            if (self$options$comparison_method %in% c("bland_altman", "all")) {
                private$.populateBlandAltmanStats(method1, method2)
            }

            if (self$options$comparison_method %in% c("passing_bablok", "all")) {
                private$.populatePassingBablokResults(method1, method2)
            }

            if (self$options$comparison_method %in% c("deming", "all")) {
                private$.populateDemingResults(method1, method2)
            }

            # Regression comparison
            if (self$options$regression_comparison) {
                private$.populateRegressionComparison(method1, method2)
            }

            # Bias analysis
            if (self$options$proportional_bias) {
                private$.populateBiasAnalysis(method1, method2)
            }

            # Outlier detection
            if (self$options$outlier_detection) {
                private$.populateOutlierAnalysis(method1, method2)
            }

            # Clinical assessment
            if (self$options$clinical_limits) {
                private$.populateClinicalAssessment(method1, method2)
            }

            # Guidance content
            private$.populateMethodGuidance()
            private$.populateTechnicalNotes()
        },

        .populateDescriptiveStats = function(method1, method2) {
            table <- self$results$descriptiveStats

            # Method 1 statistics
            table$addRow(rowKey = "method1", values = list(
                method = "Reference Method",
                n = length(method1),
                mean = mean(method1, na.rm = TRUE),
                sd = sd(method1, na.rm = TRUE),
                median = median(method1, na.rm = TRUE),
                min = min(method1, na.rm = TRUE),
                max = max(method1, na.rm = TRUE)
            ))

            # Method 2 statistics  
            table$addRow(rowKey = "method2", values = list(
                method = "Test Method",
                n = length(method2),
                mean = mean(method2, na.rm = TRUE),
                sd = sd(method2, na.rm = TRUE),
                median = median(method2, na.rm = TRUE),
                min = min(method2, na.rm = TRUE),
                max = max(method2, na.rm = TRUE)
            ))

            # Difference statistics
            diff <- method2 - method1
            table$addRow(rowKey = "difference", values = list(
                method = "Difference (Test - Reference)",
                n = length(diff),
                mean = mean(diff, na.rm = TRUE),
                sd = sd(diff, na.rm = TRUE),
                median = median(diff, na.rm = TRUE),
                min = min(diff, na.rm = TRUE),
                max = max(diff, na.rm = TRUE)
            ))
        },

        .populateCorrelationTable = function(method1, method2) {
            table <- self$results$correlationTable
            conf_level <- self$options$confidence_level

            # Pearson correlation
            pearson_test <- cor.test(method1, method2, method = "pearson", conf.level = conf_level)
            table$addRow(rowKey = "pearson", values = list(
                correlation_type = "Pearson",
                r = pearson_test$estimate,
                r_squared = pearson_test$estimate^2,
                lower_ci = pearson_test$conf.int[1],
                upper_ci = pearson_test$conf.int[2],
                p_value = pearson_test$p.value
            ))

            # Spearman correlation
            spearman_test <- cor.test(method1, method2, method = "spearman", conf.level = conf_level)
            table$addRow(rowKey = "spearman", values = list(
                correlation_type = "Spearman",
                r = spearman_test$estimate,
                r_squared = spearman_test$estimate^2,
                lower_ci = spearman_test$conf.int[1],
                upper_ci = spearman_test$conf.int[2],
                p_value = spearman_test$p.value
            ))
        },

        .populateConcordanceTable = function(method1, method2) {
            if (!requireNamespace('DescTools', quietly = TRUE)) {
                # If DescTools not available, calculate CCC manually
                private$.calculateCCCManually(method1, method2)
                return()
            }
            
            table <- self$results$concordanceTable
            conf_level <- self$options$confidence_level
            
            tryCatch({
                # Calculate CCC using DescTools
                ccc_result <- DescTools::CCC(method1, method2, conf.level = conf_level)
                
                # Extract values
                ccc_value <- ccc_result$rho.c
                ccc_ci <- ccc_result$conf.int
                
                # Calculate precision and accuracy components
                r <- cor(method1, method2, use = "complete.obs")
                mean1 <- mean(method1, na.rm = TRUE)
                mean2 <- mean(method2, na.rm = TRUE)
                var1 <- var(method1, na.rm = TRUE)
                var2 <- var(method2, na.rm = TRUE)
                
                bias_correction <- 2 * r * sqrt(var1) * sqrt(var2) / (var1 + var2 + (mean1 - mean2)^2)
                precision <- r
                accuracy <- 2 / (1 + var1/var2 + var2/var1 + (mean1 - mean2)^2/(sqrt(var1) * sqrt(var2)))
                
                # Interpretation
                ccc_interp <- if (ccc_value >= 0.99) "Almost perfect agreement"
                              else if (ccc_value >= 0.95) "Substantial agreement"
                              else if (ccc_value >= 0.90) "Moderate agreement"
                              else if (ccc_value >= 0.75) "Fair agreement"
                              else "Poor agreement"
                
                # Add rows to table
                table$addRow(rowKey = "ccc", values = list(
                    measure = "Concordance Correlation Coefficient",
                    estimate = ccc_value,
                    lower_ci = ccc_ci[1],
                    upper_ci = ccc_ci[2],
                    interpretation = ccc_interp
                ))
                
                table$addRow(rowKey = "precision", values = list(
                    measure = "Precision (Correlation)",
                    estimate = precision,
                    lower_ci = NA,
                    upper_ci = NA,
                    interpretation = paste("r =", round(precision, 4))
                ))
                
                table$addRow(rowKey = "accuracy", values = list(
                    measure = "Accuracy (Bias correction)",
                    estimate = accuracy,
                    lower_ci = NA,
                    upper_ci = NA,
                    interpretation = paste("C_b =", round(bias_correction, 4))
                ))
                
            }, error = function(e) {
                # Fallback to manual calculation
                private$.calculateCCCManually(method1, method2)
            })
        },
        
        .calculateCCCManually = function(method1, method2) {
            table <- self$results$concordanceTable
            
            # Manual CCC calculation (Lin 1989)
            # Remove missing values
            complete_cases <- complete.cases(method1, method2)
            x <- method1[complete_cases]
            y <- method2[complete_cases]
            
            if (length(x) < 3) {
                table$addRow(rowKey = "error", values = list(
                    measure = "Error",
                    estimate = NA,
                    lower_ci = NA,
                    upper_ci = NA,
                    interpretation = "Insufficient data for CCC calculation"
                ))
                return()
            }
            
            # Calculate CCC components
            mean_x <- mean(x)
            mean_y <- mean(y)
            var_x <- var(x)
            var_y <- var(y)
            cov_xy <- cov(x, y)
            r <- cor(x, y)
            
            # Lin's CCC formula
            ccc <- (2 * cov_xy) / (var_x + var_y + (mean_x - mean_y)^2)
            
            # Approximate confidence interval (Fisher's Z transformation)
            n <- length(x)
            z_ccc <- 0.5 * log((1 + ccc)/(1 - ccc))
            se_z <- 1/sqrt(n - 3)
            alpha <- 1 - self$options$confidence_level
            z_critical <- qnorm(1 - alpha/2)
            
            z_lower <- z_ccc - z_critical * se_z
            z_upper <- z_ccc + z_critical * se_z
            
            ccc_lower <- (exp(2 * z_lower) - 1) / (exp(2 * z_lower) + 1)
            ccc_upper <- (exp(2 * z_upper) - 1) / (exp(2 * z_upper) + 1)
            
            # Interpretation
            ccc_interp <- if (ccc >= 0.99) "Almost perfect agreement"
                          else if (ccc >= 0.95) "Substantial agreement"
                          else if (ccc >= 0.90) "Moderate agreement"
                          else if (ccc >= 0.75) "Fair agreement"
                          else "Poor agreement"
            
            table$addRow(rowKey = "ccc_manual", values = list(
                measure = "Concordance Correlation Coefficient",
                estimate = ccc,
                lower_ci = ccc_lower,
                upper_ci = ccc_upper,
                interpretation = ccc_interp
            ))
            
            # Add precision and accuracy decomposition
            precision <- r
            accuracy <- 2 / (1 + var_x/var_y + var_y/var_x + (mean_x - mean_y)^2/(sqrt(var_x) * sqrt(var_y)))
            
            table$addRow(rowKey = "precision_manual", values = list(
                measure = "Precision (Correlation)",
                estimate = precision,
                lower_ci = NA,
                upper_ci = NA,
                interpretation = "Measures how far observations deviate from the best-fit line"
            ))
            
            table$addRow(rowKey = "accuracy_manual", values = list(
                measure = "Accuracy (Location shift)",
                estimate = accuracy,
                lower_ci = NA,
                upper_ci = NA,
                interpretation = "Measures how far the best-fit line deviates from 45° line"
            ))
        },

        .populateBlandAltmanStats = function(method1, method2) {
            table <- self$results$blandAltmanStats
            
            diff <- method2 - method1
            mean_diff <- mean(diff)
            sd_diff <- sd(diff)
            n <- length(diff)
            
            # Calculate limits of agreement based on method
            if (self$options$limits_method == "standard") {
                # Standard ±1.96 SD
                lower_loa <- mean_diff - 1.96 * sd_diff
                upper_loa <- mean_diff + 1.96 * sd_diff
                
                # Confidence intervals for limits
                se_loa <- sd_diff * sqrt(3/n)
                t_val <- qt((1 + self$options$confidence_level)/2, df = n-1)
                lower_loa_ci <- c(lower_loa - t_val * se_loa, lower_loa + t_val * se_loa)
                upper_loa_ci <- c(upper_loa - t_val * se_loa, upper_loa + t_val * se_loa)
                
            } else if (self$options$limits_method == "exact") {
                # Exact (t-distribution)
                t_val <- qt((1 + self$options$confidence_level)/2, df = n-1)
                lower_loa <- mean_diff - t_val * sd_diff
                upper_loa <- mean_diff + t_val * sd_diff
                
                se_loa <- sd_diff * sqrt((n-1)/n * (1 + t_val^2/(2*(n-1))))
                lower_loa_ci <- c(lower_loa - t_val * se_loa, lower_loa + t_val * se_loa)
                upper_loa_ci <- c(upper_loa - t_val * se_loa, upper_loa + t_val * se_loa)
            }

            # Populate table
            table$addRow(rowKey = "bias", values = list(
                statistic = "Mean Bias",
                value = mean_diff,
                lower_ci = mean_diff - qt((1 + self$options$confidence_level)/2, df = n-1) * sd_diff/sqrt(n),
                upper_ci = mean_diff + qt((1 + self$options$confidence_level)/2, df = n-1) * sd_diff/sqrt(n),
                interpretation = if (abs(mean_diff) < 0.1 * mean(c(mean(method1), mean(method2)))) "Negligible bias" else "Significant bias"
            ))

            table$addRow(rowKey = "lower_loa", values = list(
                statistic = "Lower Limit of Agreement",
                value = lower_loa,
                lower_ci = lower_loa_ci[1],
                upper_ci = lower_loa_ci[2],
                interpretation = "95% of differences below this value"
            ))

            table$addRow(rowKey = "upper_loa", values = list(
                statistic = "Upper Limit of Agreement", 
                value = upper_loa,
                lower_ci = upper_loa_ci[1],
                upper_ci = upper_loa_ci[2],
                interpretation = "95% of differences above this value"
            ))

            table$addRow(rowKey = "precision", values = list(
                statistic = "Precision (SD of differences)",
                value = sd_diff,
                lower_ci = sd_diff * sqrt((n-1)/qchisq((1 + self$options$confidence_level)/2, df = n-1)),
                upper_ci = sd_diff * sqrt((n-1)/qchisq((1 - self$options$confidence_level)/2, df = n-1)),
                interpretation = "Measure of random error"
            ))
        },

        .populatePassingBablokResults = function(method1, method2) {
            # Basic Passing-Bablok implementation
            # This is a simplified version - full implementation would use mcr package
            table <- self$results$passingBablokResults
            
            # Simplified slope and intercept calculation
            # In practice, use mcr::mcreg() for proper Passing-Bablok
            n <- length(method1)
            slopes <- numeric()
            
            # Calculate all possible slopes
            for (i in 1:(n-1)) {
                for (j in (i+1):n) {
                    if (method1[j] != method1[i]) {
                        slopes <- c(slopes, (method2[j] - method2[i])/(method1[j] - method1[i]))
                    }
                }
            }
            
            # Median slope
            beta <- median(slopes, na.rm = TRUE)
            
            # Intercept
            alpha <- median(method2 - beta * method1, na.rm = TRUE)
            
            # Confidence intervals (simplified)
            n_slopes <- length(slopes)
            k <- qnorm((1 + self$options$confidence_level)/2) * sqrt(n_slopes/3)
            lower_idx <- max(1, round(n_slopes/2 - k))
            upper_idx <- min(n_slopes, round(n_slopes/2 + k))
            
            sorted_slopes <- sort(slopes)
            beta_ci <- c(sorted_slopes[lower_idx], sorted_slopes[upper_idx])
            
            table$addRow(rowKey = "intercept", values = list(
                parameter = "Intercept (α)",
                estimate = alpha,
                lower_ci = NA, # Would need proper PB calculation
                upper_ci = NA,
                test_result = if (abs(alpha) < 1) "No significant intercept" else "Significant intercept"
            ))

            table$addRow(rowKey = "slope", values = list(
                parameter = "Slope (β)",
                estimate = beta,
                lower_ci = beta_ci[1],
                upper_ci = beta_ci[2],
                test_result = if (beta_ci[1] <= 1 && beta_ci[2] >= 1) "No significant slope difference" else "Significant slope difference"
            ))
        },

        .populateDemingResults = function(method1, method2) {
            table <- self$results$demingResults
            
            # Simplified Deming regression
            lambda <- self$options$error_ratio
            n <- length(method1)
            
            # Sample means
            x_mean <- mean(method1)
            y_mean <- mean(method2)
            
            # Sums of squares and cross products
            sxx <- sum((method1 - x_mean)^2)
            syy <- sum((method2 - y_mean)^2)
            sxy <- sum((method1 - x_mean) * (method2 - y_mean))
            
            # Deming regression slope
            u <- (syy - lambda * sxx) / (2 * sxy)
            beta <- u + sqrt(u^2 + lambda)
            
            # Intercept
            alpha <- y_mean - beta * x_mean
            
            # Standard errors (simplified)
            residuals <- method2 - (alpha + beta * method1)
            mse <- sum(residuals^2) / (n - 2)
            se_beta <- sqrt(mse * lambda / sxx)
            se_alpha <- sqrt(mse * (1/n + lambda * x_mean^2 / sxx))
            
            # Confidence intervals
            t_val <- qt((1 + self$options$confidence_level)/2, df = n-2)
            
            table$addRow(rowKey = "intercept", values = list(
                parameter = "Intercept (α)",
                estimate = alpha,
                se = se_alpha,
                lower_ci = alpha - t_val * se_alpha,
                upper_ci = alpha + t_val * se_alpha
            ))

            table$addRow(rowKey = "slope", values = list(
                parameter = "Slope (β)",
                estimate = beta,
                se = se_beta,
                lower_ci = beta - t_val * se_beta,
                upper_ci = beta + t_val * se_beta
            ))
        },

        .populateRegressionComparison = function(method1, method2) {
            table <- self$results$regressionComparison
            
            # Ordinary Least Squares
            ols_model <- lm(method2 ~ method1)
            ols_summary <- summary(ols_model)
            
            table$addRow(rowKey = "ols", values = list(
                method = "Ordinary Least Squares",
                intercept = coef(ols_model)[1],
                slope = coef(ols_model)[2], 
                r_squared = ols_summary$r.squared,
                rmse = sqrt(mean(residuals(ols_model)^2))
            ))
            
            # Add other regression methods if implemented
            # This would include results from Passing-Bablok and Deming
        },

        .populateBiasAnalysis = function(method1, method2) {
            table <- self$results$biasAnalysis
            
            diff <- method2 - method1
            average <- (method1 + method2) / 2
            
            # Test for proportional bias (correlation between difference and average)
            prop_bias_test <- cor.test(diff, average)
            
            table$addRow(rowKey = "proportional", values = list(
                bias_type = "Proportional Bias",
                statistic = prop_bias_test$statistic,
                p_value = prop_bias_test$p.value,
                interpretation = if (prop_bias_test$p.value < 0.05) 
                    "Significant proportional bias detected" else 
                    "No significant proportional bias"
            ))
            
            # Fixed bias (one-sample t-test on differences)
            fixed_bias_test <- t.test(diff)
            
            table$addRow(rowKey = "fixed", values = list(
                bias_type = "Fixed Bias",
                statistic = fixed_bias_test$statistic,
                p_value = fixed_bias_test$p.value,
                interpretation = if (fixed_bias_test$p.value < 0.05) 
                    "Significant fixed bias detected" else 
                    "No significant fixed bias"
            ))
        },

        .populateOutlierAnalysis = function(method1, method2) {
            table <- self$results$outlierAnalysis
            
            diff <- method2 - method1
            standardized_residuals <- abs(diff - mean(diff)) / sd(diff)
            outlier_threshold <- 2 # 2 SD threshold
            
            outliers <- which(standardized_residuals > outlier_threshold)
            
            if (length(outliers) > 0) {
                for (i in outliers) {
                    table$addRow(rowKey = paste0("outlier_", i), values = list(
                        observation = i,
                        method1_value = method1[i],
                        method2_value = method2[i],
                        difference = diff[i],
                        standardized_residual = standardized_residuals[i],
                        outlier_flag = "Yes"
                    ))
                }
            }
        },

        .populateClinicalAssessment = function(method1, method2) {
            table <- self$results$clinicalAssessment
            
            diff <- method2 - method1
            lower_limit <- self$options$clinical_lower
            upper_limit <- self$options$clinical_upper
            
            within_limits <- sum(diff >= lower_limit & diff <= upper_limit)
            total_obs <- length(diff)
            percentage_within <- (within_limits / total_obs) * 100
            
            table$addRow(rowKey = "clinical", values = list(
                criterion = paste0("Clinical Limits (", lower_limit, " to ", upper_limit, ")"),
                result = paste0(within_limits, " / ", total_obs, " observations"),
                percentage_within = percentage_within,
                clinical_interpretation = if (percentage_within >= 95) 
                    "Clinically acceptable agreement" else 
                    "Clinical agreement may be insufficient"
            ))
        },

        .populateMethodGuidance = function() {
            content <- "
            <h3>Method Comparison Guidelines</h3>
            <h4>Interpretation Guide:</h4>
            <ul>
                <li><strong>Bland-Altman Analysis:</strong> Assesses agreement between methods using mean bias and limits of agreement</li>
                <li><strong>Passing-Bablok Regression:</strong> Non-parametric regression robust to outliers, tests for proportional and systematic bias</li>
                <li><strong>Deming Regression:</strong> Accounts for measurement error in both methods</li>
            </ul>
            <h4>Decision Criteria:</h4>
            <ul>
                <li><strong>Good Agreement:</strong> Mean bias near zero, narrow limits of agreement, high correlation</li>
                <li><strong>Clinical Acceptability:</strong> Differences within predefined clinical limits for ≥95% of observations</li>
                <li><strong>Method Equivalence:</strong> Slope ≈ 1, intercept ≈ 0 in regression analysis</li>
            </ul>
            "
            self$results$methodGuidance$setContent(content)
        },

        .populateTechnicalNotes = function() {
            content <- paste0(
                "<h3>Technical Notes and Assumptions</h3>",
                "<h4>Assumptions:</h4>",
                "<ul>",
                "<li>Differences are approximately normally distributed (for Bland-Altman)</li>",
                "<li>Homoscedasticity (constant variance across measurement range)</li>",
                "<li>Independent observations</li>",
                "<li>Measurement errors follow assumed distribution patterns</li>",
                "</ul>",
                "<h4>Method Selection Guide:</h4>",
                "<ul>",
                "<li><strong>Bland-Altman:</strong> Standard approach for method comparison, assumes normal distribution of differences</li>",
                "<li><strong>Passing-Bablok:</strong> Robust to non-normal distributions and outliers, no assumptions about error distribution</li>",
                "<li><strong>Deming Regression:</strong> When both methods have measurement error, specify error variance ratio λ</li>",
                "</ul>",
                "<h4>Sample Size:</h4>",
                "<p>Minimum 30-50 observations recommended for reliable estimates. Larger samples (>100) preferred for robust confidence intervals.</p>"
            )
            self$results$technicalNotes$setContent(content)
        },

        .plotBlandAltman = function(image, ggtheme, theme, ...) {
            if (is.null(self$options$method1) || is.null(self$options$method2))
                return()

            # Get data
            method1 <- self$data[[self$options$method1]]
            method2 <- self$data[[self$options$method2]]

            # Handle missing data
            if (self$options$missing_treatment == "complete") {
                complete_cases <- complete.cases(method1, method2)
                method1 <- method1[complete_cases]
                method2 <- method2[complete_cases]
            }

            if (length(method1) < 10)
                return()

            # Apply transformation if specified
            if (self$options$transformation == "log") {
                method1 <- log(method1)
                method2 <- log(method2)
            } else if (self$options$transformation == "sqrt") {
                method1 <- sqrt(method1)
                method2 <- sqrt(method2)
            }

            # Calculate Bland-Altman statistics
            average <- (method1 + method2) / 2
            difference <- method2 - method1
            mean_diff <- mean(difference)
            sd_diff <- sd(difference)
            
            # Limits of agreement
            lower_loa <- mean_diff - 1.96 * sd_diff
            upper_loa <- mean_diff + 1.96 * sd_diff

            # Create plot
            plotData <- data.frame(
                Average = average,
                Difference = difference
            )

            p <- ggplot2::ggplot(plotData, ggplot2::aes(x = Average, y = Difference)) +
                ggplot2::geom_point(alpha = 0.6, size = 2) +
                ggplot2::geom_hline(yintercept = mean_diff, linetype = "solid", color = "blue", size = 1) +
                ggplot2::geom_hline(yintercept = lower_loa, linetype = "dashed", color = "red", size = 1) +
                ggplot2::geom_hline(yintercept = upper_loa, linetype = "dashed", color = "red", size = 1) +
                ggplot2::geom_hline(yintercept = 0, linetype = "dotted", color = "gray50") +
                ggplot2::labs(
                    title = "Bland-Altman Plot",
                    subtitle = paste0("Mean bias: ", round(mean_diff, 3), 
                                    " (95% LoA: ", round(lower_loa, 3), " to ", round(upper_loa, 3), ")"),
                    x = "Average of Methods",
                    y = "Difference (Test - Reference)"
                ) +
                ggtheme

            # Add clinical limits if specified
            if (self$options$clinical_limits) {
                p <- p + 
                    ggplot2::geom_hline(yintercept = self$options$clinical_lower, 
                                      linetype = "dotdash", color = "green", size = 1) +
                    ggplot2::geom_hline(yintercept = self$options$clinical_upper, 
                                      linetype = "dotdash", color = "green", size = 1)
            }

            print(p)
            TRUE
        },

        .plotScatter = function(image, ggtheme, theme, ...) {
            if (is.null(self$options$method1) || is.null(self$options$method2))
                return()

            # Get data
            method1 <- self$data[[self$options$method1]]
            method2 <- self$data[[self$options$method2]]

            # Handle missing data
            if (self$options$missing_treatment == "complete") {
                complete_cases <- complete.cases(method1, method2)
                method1 <- method1[complete_cases]
                method2 <- method2[complete_cases]
            }

            if (length(method1) < 10)
                return()

            # Apply transformation if specified
            if (self$options$transformation == "log") {
                method1 <- log(method1)
                method2 <- log(method2)
            } else if (self$options$transformation == "sqrt") {
                method1 <- sqrt(method1)
                method2 <- sqrt(method2)
            }

            # Create plot
            plotData <- data.frame(
                Reference = method1,
                Test = method2
            )

            # Calculate correlation
            corr <- cor(method1, method2, use = "complete.obs")

            p <- ggplot2::ggplot(plotData, ggplot2::aes(x = Reference, y = Test)) +
                ggplot2::geom_point(alpha = 0.6, size = 2) +
                ggplot2::geom_smooth(method = "lm", se = TRUE, color = "blue") +
                ggplot2::geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
                ggplot2::labs(
                    title = "Method Comparison Scatter Plot",
                    subtitle = paste0("Correlation: r = ", round(corr, 3)),
                    x = "Reference Method",
                    y = "Test Method"
                ) +
                ggtheme

            print(p)
            TRUE
        },

        .plotResiduals = function(image, ggtheme, theme, ...) {
            if (is.null(self$options$method1) || is.null(self$options$method2))
                return()

            # Get data
            method1 <- self$data[[self$options$method1]]
            method2 <- self$data[[self$options$method2]]

            # Handle missing data and transformation
            if (self$options$missing_treatment == "complete") {
                complete_cases <- complete.cases(method1, method2)
                method1 <- method1[complete_cases]
                method2 <- method2[complete_cases]
            }

            if (length(method1) < 10)
                return()

            # Apply transformation if specified
            if (self$options$transformation == "log") {
                method1 <- log(method1)
                method2 <- log(method2)
            } else if (self$options$transformation == "sqrt") {
                method1 <- sqrt(method1)
                method2 <- sqrt(method2)
            }

            # Fit regression model
            model <- lm(method2 ~ method1)
            residuals <- residuals(model)
            fitted_values <- fitted(model)

            # Create residual plot
            plotData <- data.frame(
                Fitted = fitted_values,
                Residuals = residuals
            )

            p <- ggplot2::ggplot(plotData, ggplot2::aes(x = Fitted, y = Residuals)) +
                ggplot2::geom_point(alpha = 0.6, size = 2) +
                ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
                ggplot2::geom_smooth(method = "loess", se = TRUE, color = "blue") +
                ggplot2::labs(
                    title = "Residual Analysis",
                    subtitle = "Residuals vs Fitted Values",
                    x = "Fitted Values",
                    y = "Residuals"
                ) +
                ggtheme

            print(p)
            TRUE
        },

        .plotMountain = function(image, ggtheme, theme, ...) {
            if (is.null(self$options$method1) || is.null(self$options$method2))
                return()

            # Get data
            method1 <- self$data[[self$options$method1]]
            method2 <- self$data[[self$options$method2]]

            # Handle missing data
            if (self$options$missing_treatment == "complete") {
                complete_cases <- complete.cases(method1, method2)
                method1 <- method1[complete_cases]
                method2 <- method2[complete_cases]
            }

            if (length(method1) < 10)
                return()

            # Apply transformation if specified
            if (self$options$transformation == "log") {
                method1 <- log(method1)
                method2 <- log(method2)
            } else if (self$options$transformation == "sqrt") {
                method1 <- sqrt(method1)
                method2 <- sqrt(method2)
            }

            # Calculate differences and percentiles
            difference <- method2 - method1
            n <- length(difference)
            ranks <- rank(difference)
            percentiles <- (ranks - 0.5) / n * 100

            # Create mountain plot
            plotData <- data.frame(
                Percentile = percentiles,
                Difference = difference
            )

            p <- ggplot2::ggplot(plotData, ggplot2::aes(x = Percentile, y = Difference)) +
                ggplot2::geom_line(size = 1, color = "blue") +
                ggplot2::geom_point(alpha = 0.6, size = 1.5) +
                ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
                ggplot2::geom_hline(yintercept = median(difference), linetype = "solid", color = "green") +
                ggplot2::labs(
                    title = "Mountain Plot",
                    subtitle = "Percentile-based Difference Distribution",
                    x = "Percentile",
                    y = "Difference (Test - Reference)"
                ) +
                ggtheme

            print(p)
            TRUE
        }
    )
)