#' @title Effect Size Analysis
#' @importFrom R6 R6Class
#' @import jmvcore
#' @importFrom stats t.test var sd
#' @export

effectsizeClass <- R6::R6Class(
    "effectsizeClass",
    inherit = effectsizeBase,
    private = list(
        .init = function() {
            if (is.null(self$data))
                return()
            
            # Set up instructions
            html <- self$results$instructions
            html$setContent(
                '<html>
                <head>
                </head>
                <body>
                <div class="instructions">
                <h3>Effect Size Analysis</h3>
                <p>This analysis calculates various effect size measures for comparing groups or testing against a reference value.</p>
                <ul>
                <li><b>Cohen\'s d:</b> Standardized mean difference using pooled standard deviation</li>
                <li><b>Hedges\' g:</b> Bias-corrected version of Cohen\'s d (recommended for small samples)</li>
                <li><b>Glass\' delta:</b> Standardized mean difference using control group standard deviation</li>
                </ul>
                <h4>Effect Size Interpretation (Cohen, 1988):</h4>
                <ul>
                <li>Small: d ≈ 0.2</li>
                <li>Medium: d ≈ 0.5</li>
                <li>Large: d ≈ 0.8</li>
                </ul>
                <p><b>Note:</b> Effect sizes are essential for clinical significance assessment and meta-analyses.</p>
                </div>
                </body>
                </html>'
            )
        },
        
        .run = function() {
            if (is.null(self$data) || is.null(self$options$dep))
                return()
            
            dep <- self$options$dep
            group <- self$options$group
            analysis_type <- self$options$analysisType
            measures <- self$options$measures
            ci_level <- self$options$ciWidth / 100
            test_value <- self$options$testValue
            group1_value <- self$options$group1Value
            group2_value <- self$options$group2Value
            
            # Get data
            data <- self$data
            data[[dep]] <- jmvcore::toNumeric(data[[dep]])
            
            # Remove missing values from dependent variable
            data <- data[!is.na(data[[dep]]), ]
            
            if (nrow(data) < 2) {
                self$results$instructions$setContent("Insufficient data for effect size analysis.")
                return()
            }
            
            if (analysis_type == "one_sample") {
                private$.runOneSample(data, dep, test_value, measures, ci_level)
            } else if (analysis_type == "two_sample" && !is.null(group)) {
                data[[group]] <- as.factor(data[[group]])
                private$.runTwoSample(data, dep, group, measures, ci_level, group1_value, group2_value)
            }
        },
        
        .runOneSample = function(data, dep, test_value, measures, ci_level) {
            values <- data[[dep]]
            n <- length(values)
            mean_val <- mean(values)
            sd_val <- sd(values)
            se_val <- sd_val / sqrt(n)
            
            # Calculate t-statistic for confidence intervals
            t_val <- (mean_val - test_value) / se_val
            df <- n - 1
            
            results_table <- self$results$effectSizes
            
            if ("cohens_d" %in% measures) {
                # Cohen's d for one sample
                cohens_d <- (mean_val - test_value) / sd_val
                
                # Cohen's d confidence interval (Hedges & Olkin, 1985)
                ncp <- cohens_d * sqrt(n)
                ci_lower <- private$.cohensD_CI_lower(ncp, df, ci_level)
                ci_upper <- private$.cohensD_CI_upper(ncp, df, ci_level)
                
                results_table$addRow(rowKey = "cohens_d", values = list(
                    measure = "Cohen's d",
                    estimate = cohens_d,
                    lower = ci_lower,
                    upper = ci_upper,
                    interpretation = private$.interpretEffect(abs(cohens_d))
                ))
            }
            
            if ("hedges_g" %in% measures) {
                # Hedges' g with bias correction
                cohens_d <- (mean_val - test_value) / sd_val
                correction_factor <- 1 - (3 / (4 * df - 1))
                hedges_g <- cohens_d * correction_factor
                
                # Hedges' g confidence interval
                ncp <- hedges_g * sqrt(n)
                ci_lower <- private$.hedgesG_CI_lower(ncp, df, ci_level)
                ci_upper <- private$.hedgesG_CI_upper(ncp, df, ci_level)
                
                results_table$addRow(rowKey = "hedges_g", values = list(
                    measure = "Hedges' g",
                    estimate = hedges_g,
                    lower = ci_lower,
                    upper = ci_upper,
                    interpretation = private$.interpretEffect(abs(hedges_g))
                ))
            }
            
            if ("glass_delta" %in% measures) {
                # Glass' delta (same as Cohen's d for one sample)
                glass_delta <- (mean_val - test_value) / sd_val
                
                # Glass' delta confidence interval
                ncp <- glass_delta * sqrt(n)
                ci_lower <- private$.glassD_CI_lower(ncp, df, ci_level)
                ci_upper <- private$.glassD_CI_upper(ncp, df, ci_level)
                
                results_table$addRow(rowKey = "glass_delta", values = list(
                    measure = "Glass' Δ",
                    estimate = glass_delta,
                    lower = ci_lower,
                    upper = ci_upper,
                    interpretation = private$.interpretEffect(abs(glass_delta))
                ))
            }
            
            # Add sample statistics
            stats_table <- self$results$descriptives
            stats_table$addRow(rowKey = "sample", values = list(
                group = "Sample",
                n = n,
                mean = mean_val,
                sd = sd_val,
                se = se_val
            ))
            
            # Add test details
            test_table <- self$results$testDetails
            test_table$addRow(rowKey = "details", values = list(
                testValue = test_value,
                meanDiff = mean_val - test_value,
                tStat = t_val,
                df = df,
                pValue = 2 * (1 - pt(abs(t_val), df))
            ))
        },
        
        .runTwoSample = function(data, dep, group, measures, ci_level, group1_value, group2_value) {
            # Filter data for specified groups if provided
            if (!is.null(group1_value) && !is.null(group2_value)) {
                group_levels <- c(group1_value, group2_value)
                data <- data[data[[group]] %in% group_levels, ]
                data[[group]] <- factor(data[[group]], levels = group_levels)
            }
            
            # Split data by group
            groups <- split(data[[dep]], data[[group]])
            group_names <- names(groups)
            
            if (length(groups) < 2) {
                self$results$instructions$setContent("Need at least two groups for comparison.")
                return()
            }
            
            if (length(groups) > 2) {
                groups <- groups[1:2]
                group_names <- group_names[1:2]
            }
            
            group1 <- groups[[1]]
            group2 <- groups[[2]]
            
            # Remove missing values
            group1 <- group1[!is.na(group1)]
            group2 <- group2[!is.na(group2)]
            
            if (length(group1) < 2 || length(group2) < 2) {
                self$results$instructions$setContent("Each group needs at least 2 observations.")
                return()
            }
            
            # Calculate group statistics
            n1 <- length(group1)
            n2 <- length(group2)
            mean1 <- mean(group1)
            mean2 <- mean(group2)
            sd1 <- sd(group1)
            sd2 <- sd(group2)
            
            # Pooled standard deviation
            pooled_sd <- sqrt(((n1 - 1) * sd1^2 + (n2 - 1) * sd2^2) / (n1 + n2 - 2))
            
            results_table <- self$results$effectSizes
            
            if ("cohens_d" %in% measures) {
                # Cohen's d using pooled SD
                cohens_d <- (mean1 - mean2) / pooled_sd
                
                # Confidence interval for Cohen's d
                se_d <- sqrt((n1 + n2) / (n1 * n2) + cohens_d^2 / (2 * (n1 + n2)))
                t_crit <- qt((1 + ci_level) / 2, n1 + n2 - 2)
                ci_lower <- cohens_d - t_crit * se_d
                ci_upper <- cohens_d + t_crit * se_d
                
                results_table$addRow(rowKey = "cohens_d", values = list(
                    measure = "Cohen's d",
                    estimate = cohens_d,
                    lower = ci_lower,
                    upper = ci_upper,
                    interpretation = private$.interpretEffect(abs(cohens_d))
                ))
            }
            
            if ("hedges_g" %in% measures) {
                # Hedges' g with bias correction
                cohens_d <- (mean1 - mean2) / pooled_sd
                df <- n1 + n2 - 2
                correction_factor <- 1 - (3 / (4 * df - 1))
                hedges_g <- cohens_d * correction_factor
                
                # Confidence interval for Hedges' g
                se_g <- sqrt((n1 + n2) / (n1 * n2) + hedges_g^2 / (2 * (n1 + n2)))
                t_crit <- qt((1 + ci_level) / 2, df)
                ci_lower <- hedges_g - t_crit * se_g
                ci_upper <- hedges_g + t_crit * se_g
                
                results_table$addRow(rowKey = "hedges_g", values = list(
                    measure = "Hedges' g",
                    estimate = hedges_g,
                    lower = ci_lower,
                    upper = ci_upper,
                    interpretation = private$.interpretEffect(abs(hedges_g))
                ))
            }
            
            if ("glass_delta" %in% measures) {
                # Glass' delta using control group SD (assume group2 is control)
                glass_delta <- (mean1 - mean2) / sd2
                
                # Confidence interval for Glass' delta
                se_glass <- sqrt(1/n1 + 1/n2 + glass_delta^2 / (2 * n2))
                t_crit <- qt((1 + ci_level) / 2, n2 - 1)
                ci_lower <- glass_delta - t_crit * se_glass
                ci_upper <- glass_delta + t_crit * se_glass
                
                results_table$addRow(rowKey = "glass_delta", values = list(
                    measure = "Glass' Δ",
                    estimate = glass_delta,
                    lower = ci_lower,
                    upper = ci_upper,
                    interpretation = private$.interpretEffect(abs(glass_delta))
                ))
            }
            
            # Add descriptive statistics
            stats_table <- self$results$descriptives
            stats_table$addRow(rowKey = "group1", values = list(
                group = group_names[1],
                n = n1,
                mean = mean1,
                sd = sd1,
                se = sd1 / sqrt(n1)
            ))
            
            stats_table$addRow(rowKey = "group2", values = list(
                group = group_names[2],
                n = n2,
                mean = mean2,
                sd = sd2,
                se = sd2 / sqrt(n2)
            ))
            
            # Add comparison details
            t_stat <- (mean1 - mean2) / sqrt(pooled_sd^2 * (1/n1 + 1/n2))
            df <- n1 + n2 - 2
            p_value <- 2 * (1 - pt(abs(t_stat), df))
            
            test_table <- self$results$testDetails
            test_table$addRow(rowKey = "details", values = list(
                testValue = NA,
                meanDiff = mean1 - mean2,
                tStat = t_stat,
                df = df,
                pValue = p_value
            ))
        },
        
        .interpretEffect = function(effect_size) {
            if (is.na(effect_size)) return("Cannot determine")
            if (effect_size < 0.2) return("Negligible")
            if (effect_size < 0.5) return("Small")
            if (effect_size < 0.8) return("Medium")
            return("Large")
        },
        
        # Simplified CI calculations (could be enhanced with more sophisticated methods)
        .cohensD_CI_lower = function(ncp, df, ci_level) {
            alpha <- 1 - ci_level
            t_crit <- qt(alpha / 2, df)
            se <- sqrt(1 + ncp^2 / (2 * (df + 1)))
            return((ncp / sqrt(df + 1)) + t_crit * se)
        },
        
        .cohensD_CI_upper = function(ncp, df, ci_level) {
            alpha <- 1 - ci_level
            t_crit <- qt(1 - alpha / 2, df)
            se <- sqrt(1 + ncp^2 / (2 * (df + 1)))
            return((ncp / sqrt(df + 1)) + t_crit * se)
        },
        
        .hedgesG_CI_lower = function(ncp, df, ci_level) {
            # Use similar approach as Cohen's d but with bias correction
            return(private$.cohensD_CI_lower(ncp, df, ci_level) * (1 - 3/(4*df - 1)))
        },
        
        .hedgesG_CI_upper = function(ncp, df, ci_level) {
            # Use similar approach as Cohen's d but with bias correction
            return(private$.cohensD_CI_upper(ncp, df, ci_level) * (1 - 3/(4*df - 1)))
        },
        
        .glassD_CI_lower = function(ncp, df, ci_level) {
            # Similar to Cohen's d for simplicity
            return(private$.cohensD_CI_lower(ncp, df, ci_level))
        },
        
        .glassD_CI_upper = function(ncp, df, ci_level) {
            # Similar to Cohen's d for simplicity
            return(private$.cohensD_CI_upper(ncp, df, ci_level))
        },
        
        .plotEffects = function(image, ...) {
            if (is.null(self$options$dep) || !self$options$plotEffects)
                return()
            
            # Get the effect size results
            effectSizes <- self$results$effectSizes
            
            if (effectSizes$rowCount == 0)
                return()
            
            # Extract data for plotting
            measures <- c()
            estimates <- c()
            lowers <- c()
            uppers <- c()
            
            for (i in 1:effectSizes$rowCount) {
                measures <- c(measures, effectSizes$getRow(rowNo = i)$measure)
                estimates <- c(estimates, effectSizes$getRow(rowNo = i)$estimate)
                lowers <- c(lowers, effectSizes$getRow(rowNo = i)$lower)
                uppers <- c(uppers, effectSizes$getRow(rowNo = i)$upper)
            }
            
            # Create data frame for plotting
            plot_data <- data.frame(
                measure = measures,
                estimate = estimates,
                lower = lowers,
                upper = uppers,
                stringsAsFactors = FALSE
            )
            
            # Remove any rows with missing values
            plot_data <- plot_data[complete.cases(plot_data), ]
            
            if (nrow(plot_data) == 0)
                return()
            
            # Create the plot based on plot type
            plot_type <- self$options$plotType
            
            if (plot_type == "forest") {
                p <- private$.createForestPlot(plot_data)
            } else if (plot_type == "bar") {
                p <- private$.createBarPlot(plot_data)
            } else {
                p <- private$.createDotPlot(plot_data)
            }
            
            print(p)
            TRUE
        },
        
        .createForestPlot = function(plot_data) {
            # Create forest plot
            library(ggplot2)
            
            p <- ggplot(plot_data, aes(y = measure, x = estimate)) +
                geom_point(size = 3, color = "blue") +
                geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0.2, color = "blue") +
                geom_vline(xintercept = 0, linetype = "dashed", color = "red", alpha = 0.7) +
                geom_vline(xintercept = c(-0.2, 0.2), linetype = "dotted", color = "gray", alpha = 0.7) +
                geom_vline(xintercept = c(-0.5, 0.5), linetype = "dotted", color = "gray", alpha = 0.7) +
                geom_vline(xintercept = c(-0.8, 0.8), linetype = "dotted", color = "gray", alpha = 0.7) +
                labs(
                    title = "Effect Size Forest Plot",
                    subtitle = "Effect sizes with 95% confidence intervals",
                    x = "Effect Size",
                    y = "Measure"
                ) +
                theme_minimal() +
                theme(
                    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
                    plot.subtitle = element_text(hjust = 0.5, size = 12),
                    axis.text = element_text(size = 10),
                    axis.title = element_text(size = 12),
                    panel.grid.minor = element_blank()
                ) +
                annotate("text", x = -0.2, y = length(plot_data$measure) + 0.5, label = "Small", size = 3, color = "gray60") +
                annotate("text", x = -0.5, y = length(plot_data$measure) + 0.5, label = "Medium", size = 3, color = "gray60") +
                annotate("text", x = -0.8, y = length(plot_data$measure) + 0.5, label = "Large", size = 3, color = "gray60")
            
            return(p)
        },
        
        .createBarPlot = function(plot_data) {
            library(ggplot2)
            
            p <- ggplot(plot_data, aes(x = measure, y = estimate)) +
                geom_col(fill = "steelblue", alpha = 0.7, width = 0.6) +
                geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, color = "black") +
                geom_hline(yintercept = 0, linetype = "dashed", color = "red", alpha = 0.7) +
                geom_hline(yintercept = c(-0.2, 0.2, -0.5, 0.5, -0.8, 0.8), 
                          linetype = "dotted", color = "gray", alpha = 0.5) +
                labs(
                    title = "Effect Size Bar Chart",
                    subtitle = "Effect sizes with 95% confidence intervals",
                    x = "Effect Size Measure",
                    y = "Effect Size"
                ) +
                theme_minimal() +
                theme(
                    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
                    plot.subtitle = element_text(hjust = 0.5, size = 12),
                    axis.text = element_text(size = 10),
                    axis.title = element_text(size = 12),
                    axis.text.x = element_text(angle = 45, hjust = 1),
                    panel.grid.minor = element_blank()
                )
            
            return(p)
        },
        
        .createDotPlot = function(plot_data) {
            library(ggplot2)
            
            p <- ggplot(plot_data, aes(x = measure, y = estimate)) +
                geom_point(size = 4, color = "darkblue") +
                geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, color = "darkblue") +
                geom_hline(yintercept = 0, linetype = "dashed", color = "red", alpha = 0.7) +
                geom_hline(yintercept = c(-0.2, 0.2, -0.5, 0.5, -0.8, 0.8), 
                          linetype = "dotted", color = "gray", alpha = 0.5) +
                labs(
                    title = "Effect Size Dot Plot",
                    subtitle = "Effect sizes with 95% confidence intervals",
                    x = "Effect Size Measure",
                    y = "Effect Size"
                ) +
                theme_minimal() +
                theme(
                    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
                    plot.subtitle = element_text(hjust = 0.5, size = 12),
                    axis.text = element_text(size = 10),
                    axis.title = element_text(size = 12),
                    axis.text.x = element_text(angle = 45, hjust = 1),
                    panel.grid.minor = element_blank()
                )
            
            return(p)
        }
    )
)