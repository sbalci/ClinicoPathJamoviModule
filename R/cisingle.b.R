# Enhanced confidence intervals for mean values
# @importFrom boot boot boot.ci
# @importFrom ggplot2 ggplot aes geom_point geom_errorbar labs theme_minimal

ciSingleClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "ciSingleClass",
    inherit = ciSingleBase,
    private = list(
        
        # Calculate confidence intervals using different methods
        .calculateCI = function(x, conf_level, method = "t", bootstrap_samples = 1000) {
            x_clean <- x[!is.na(x)]
            n <- length(x_clean)
            
            if (n < 2) {
                return(list(
                    mean = NA, sd = NA, se = NA, 
                    lb = NA, ub = NA, method = "Insufficient data"
                ))
            }
            
            mean_x <- mean(x_clean)
            sd_x <- sd(x_clean)
            se_x <- sd_x / sqrt(n)
            
            if (method == "t") {
                # t-distribution method (default)
                alpha <- 1 - conf_level
                t_crit <- qt(1 - alpha/2, df = n - 1)
                margin <- t_crit * se_x
                
                return(list(
                    mean = mean_x, sd = sd_x, se = se_x,
                    lb = mean_x - margin, ub = mean_x + margin,
                    method = "t-distribution"
                ))
                
            } else if (method == "normal") {
                # Normal approximation
                alpha <- 1 - conf_level
                z_crit <- qnorm(1 - alpha/2)
                margin <- z_crit * se_x
                
                return(list(
                    mean = mean_x, sd = sd_x, se = se_x,
                    lb = mean_x - margin, ub = mean_x + margin,
                    method = "Normal approximation"
                ))
                
            } else if (method == "bootstrap") {
                # Bootstrap method
                tryCatch({
                    boot_means <- function(data, indices) {
                        return(mean(data[indices]))
                    }
                    
                    boot_results <- boot::boot(x_clean, boot_means, R = bootstrap_samples)
                    boot_ci <- boot::boot.ci(boot_results, conf = conf_level, type = "perc")
                    
                    return(list(
                        mean = mean_x, sd = sd_x, se = se_x,
                        lb = boot_ci$percent[4], ub = boot_ci$percent[5],
                        method = paste("Bootstrap (", bootstrap_samples, " samples)")
                    ))
                }, error = function(e) {
                    # Fallback to t-method if bootstrap fails
                    return(private$.calculateCI(x, conf_level, method = "t"))
                })
            }
        },
        
        # Test normality assumptions
        .testNormality = function(x) {
            x_clean <- x[!is.na(x)]
            n <- length(x_clean)
            
            if (n < 3) {
                return(list(
                    shapiro_p = NA,
                    normality = "Cannot test",
                    assumption_met = "Unknown"
                ))
            }
            
            # Shapiro-Wilk test for normality
            if (n <= 5000) {  # Shapiro-Wilk has sample size limitations
                shapiro_result <- shapiro.test(x_clean)
                shapiro_p <- shapiro_result$p.value
                
                normality <- if (shapiro_p > 0.05) "Normal" else "Non-normal"
                assumption_met <- if (shapiro_p > 0.05) "Met" else "Violated"
                
            } else {
                # For large samples, use alternative tests or just note sample size
                shapiro_p <- NA
                normality <- "Large sample"
                assumption_met <- "Approximate"
            }
            
            return(list(
                shapiro_p = shapiro_p,
                normality = normality,
                assumption_met = assumption_met
            ))
        },
        
        .run = function() {
            # Input validation
            if (length(self$options$deps) == 0)
                return()
            
            if (nrow(self$data) == 0) 
                stop('Data contains no (complete) rows')

            # Get data and options
            splitBy <- self$options$splitBy
            data <- self$data
            splitByTrue <- !is.null(splitBy)
            ciLevel <- self$options$ciWidth / 100
            method <- self$options$method
            bootstrap_samples <- self$options$bootstrapSamples
            
            # Convert split variable to factor if present
            if (splitByTrue) {
                data[[splitBy]] <- as.factor(data[[splitBy]])
            }

            # Display method and confidence level information
            method_name <- switch(method,
                "t" = "t-distribution",
                "bootstrap" = paste("Bootstrap (", bootstrap_samples, " samples)"),
                "normal" = "Normal approximation"
            )
            
            disp_tx <- paste0(
                "Confidence Intervals (", self$options$ciWidth, "%) for Mean Values\n",
                "Method: ", method_name, "\n",
                if (method == "t") "Note: Results assume normally distributed data" else
                if (method == "normal") "Note: Using normal approximation (large sample)" else
                "Note: Bootstrap method (distribution-free)"
            )
            self$results$conflevel$setContent(disp_tx)

            # Prepare data for plotting
            plot_data <- list()

            # Calculate CI for each dependent variable
            for (dep in self$options$deps) {
                # Get numeric data
                x <- jmvcore::toNumeric(data[[dep]])
                
                # Calculate confidence interval
                ci_result <- private$.calculateCI(x, ciLevel, method, bootstrap_samples)
                
                # Test normality if diagnostics requested
                if (self$options$showDiagnostics) {
                    normality_result <- private$.testNormality(x)
                    
                    # Add to diagnostics table
                    self$results$diagnostics$setRow(rowKey = dep, values = list(
                        var = dep,
                        shapiro_p = normality_result$shapiro_p,
                        normality = normality_result$normality,
                        method_used = ci_result$method,
                        assumption_met = normality_result$assumption_met
                    ))
                }
                
                # Calculate CI width
                ci_width <- if (!is.na(ci_result$lb) && !is.na(ci_result$ub)) {
                    ci_result$ub - ci_result$lb
                } else {
                    NA
                }
                
                # Add to results table
                n_valid <- length(x[!is.na(x)])
                self$results$citable$setRow(rowKey = dep, values = list(
                    var = dep,
                    n = n_valid,
                    mean = ci_result$mean,
                    sd = ci_result$sd,
                    se = ci_result$se,
                    lb = ci_result$lb,
                    ub = ci_result$ub,
                    width = ci_width
                ))
                
                # Store data for plotting
                if (self$options$showPlot && !is.na(ci_result$mean)) {
                    plot_data[[dep]] <- list(
                        var = dep,
                        mean = ci_result$mean,
                        lb = ci_result$lb,
                        ub = ci_result$ub,
                        group = "Overall"
                    )
                }

                # Handle split by groups if specified
                if (splitByTrue) {
                    levels <- base::levels(data[[splitBy]])
                    for (level in levels) {
                        # Get data for this group
                        x_group <- x[data[[splitBy]] == level]
                        n_group <- length(x_group[!is.na(x_group)])
                        
                        if (n_group >= 2) {
                            ci_result_group <- private$.calculateCI(x_group, ciLevel, method, bootstrap_samples)
                            
                            # Calculate CI width for group
                            ci_width_group <- if (!is.na(ci_result_group$lb) && !is.na(ci_result_group$ub)) {
                                ci_result_group$ub - ci_result_group$lb
                            } else {
                                NA
                            }
                            
                            self$results$citable$addRow(rowKey = paste0(dep, level), values = list(
                                var = paste0(dep, " - ", level),
                                n = n_group,
                                mean = ci_result_group$mean,
                                sd = ci_result_group$sd,
                                se = ci_result_group$se,
                                lb = ci_result_group$lb,
                                ub = ci_result_group$ub,
                                width = ci_width_group
                            ))
                            
                            # Add group data for plotting
                            if (self$options$showPlot && !is.na(ci_result_group$mean)) {
                                plot_data[[paste0(dep, "_", level)]] <- list(
                                    var = paste0(dep, " (", level, ")"),
                                    mean = ci_result_group$mean,
                                    lb = ci_result_group$lb,
                                    ub = ci_result_group$ub,
                                    group = level
                                )
                            }
                            
                            # Add group diagnostics if requested
                            if (self$options$showDiagnostics) {
                                normality_result_group <- private$.testNormality(x_group)
                                
                                self$results$diagnostics$addRow(rowKey = paste0(dep, level), values = list(
                                    var = paste0(dep, " - ", level),
                                    shapiro_p = normality_result_group$shapiro_p,
                                    normality = normality_result_group$normality,
                                    method_used = ci_result_group$method,
                                    assumption_met = normality_result_group$assumption_met
                                ))
                            }
                            
                        } else {
                            # Insufficient data for this group
                            self$results$citable$addRow(rowKey = paste0(dep, level), values = list(
                                var = paste0(dep, " - ", level),
                                n = n_group,
                                mean = "Insufficient data",
                                sd = NA,
                                se = NA,
                                lb = NA,
                                ub = NA,
                                width = NA
                            ))
                        }
                    }
                }
            }
            
            # Set plot state
            if (self$options$showPlot && length(plot_data) > 0) {
                self$results$plot$setState(plot_data)
            }
        },
        
        .plot = function(image, ggtheme, theme, ...) {
            plot_data <- image$state
            if (is.null(plot_data) || length(plot_data) == 0) return()
            
            # Convert to data frame
            df <- do.call(rbind, lapply(plot_data, function(x) data.frame(x, stringsAsFactors = FALSE)))
            df$mean <- as.numeric(df$mean)
            df$lb <- as.numeric(df$lb)
            df$ub <- as.numeric(df$ub)
            
            # Create confidence interval plot
            p <- ggplot2::ggplot(df, ggplot2::aes(x = var, y = mean, color = group)) +
                ggplot2::geom_point(size = 3) +
                ggplot2::geom_errorbar(
                    ggplot2::aes(ymin = lb, ymax = ub),
                    width = 0.2,
                    size = 1
                ) +
                ggplot2::labs(
                    title = paste("Confidence Intervals (", self$options$ciWidth, "%)", sep = ""),
                    x = "Variable",
                    y = "Mean Value",
                    color = "Group"
                ) +
                ggplot2::theme_minimal() +
                ggplot2::theme(
                    axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
                    legend.position = "right"
                ) +
                ggtheme
            
            # Add horizontal line at zero if range includes it
            y_range <- range(c(df$lb, df$ub), na.rm = TRUE)
            if (y_range[1] <= 0 && y_range[2] >= 0) {
                p <- p + ggplot2::geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5)
            }
            
            print(p)
            TRUE
        }
    )
)
