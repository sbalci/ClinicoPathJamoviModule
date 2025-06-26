#' @title Economist-Style Distribution Plots
#' @description
#' Creates elegant Economist-style distribution plots using ggeconodist package
#' for professional publication-quality visualizations. Provides diminutive 
#' distribution charts that effectively communicate statistical distribution 
#' characteristics with The Economist's distinctive visual style.
#'
#' @details
#' This module integrates the ggeconodist R package functionality into jamovi,
#' providing access to The Economist's signature distribution visualization style.
#' Features include:
#' - Economist-style distribution plots with distinctive visual elements
#' - Custom percentile highlighting and color schemes
#' - Statistical annotations and effect size calculations
#' - Publication-ready formatting and themes
#' - Comprehensive distribution analysis and diagnostics
#'
#' @importFrom R6 R6Class
#' @import jmvcore
#' @importFrom ggeconodist geom_econodist theme_econodist add_econodist_legend left_align
#' @importFrom ggplot2 ggplot aes geom_point labs theme element_text coord_flip
#' @importFrom ggplot2 scale_y_continuous scale_x_discrete facet_wrap
#' @importFrom dplyr group_by summarise mutate arrange desc
#' @importFrom stats aov kruskal.test t.test wilcox.test
#' @importFrom broom tidy glance
#' @export

economistplotsClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "economistplotsClass",
    inherit = economistplotsBase,
    private = list(
        
        # Internal data storage
        .processed_data = NULL,
        .statistical_results = NULL,
        .summary_stats = NULL,
        
        .init = function() {
            # Initialize instructions
            instructions_html <- paste(
                "<div style='background-color: #f8f9fa; padding: 20px; border-radius: 8px; margin: 10px 0;'>",
                "<h3 style='color: #c7254e; margin-top: 0;'>📊 Economist-Style Distribution Plots</h3>",
                "<div style='margin: 10px 0;'>",
                "<p><strong>Create professional distribution visualizations inspired by The Economist magazine's distinctive style.</strong></p>",
                "<div style='background-color: #e8f4fd; padding: 15px; border-radius: 5px; margin: 10px 0;'>",
                "<h4 style='color: #1e6091; margin: 0 0 10px 0;'>Key Features:</h4>",
                "<ul style='margin: 5px 0; padding-left: 20px; line-height: 1.6;'>",
                "<li><strong>Diminutive Distribution Charts:</strong> Compact yet informative visualization style</li>",
                "<li><strong>Percentile Highlighting:</strong> Emphasize 10th, 50th, and 90th percentiles</li>",
                "<li><strong>Publication Quality:</strong> Professional formatting for reports and papers</li>",
                "<li><strong>Statistical Integration:</strong> Built-in statistical tests and effect sizes</li>",
                "<li><strong>Customizable Themes:</strong> Multiple visual styles from classic to enhanced</li>",
                "</ul>",
                "</div>",
                "</div>",
                "<div style='margin: 10px 0;'>",
                "<h4 style='color: #2c3e50; margin: 10px 0 5px 0;'>Quick Start:</h4>",
                "<ol style='margin: 5px 0; padding-left: 20px; line-height: 1.6;'>",
                "<li><strong>Distribution Variable:</strong> Select continuous variable to analyze</li>",
                "<li><strong>Grouping Variable:</strong> Choose categorical variable for comparison</li>",
                "<li><strong>Visual Style:</strong> Select from Classic Economist, Minimal, Enhanced, or Publication themes</li>",
                "<li><strong>Customization:</strong> Adjust colors, statistical tests, and annotations</li>",
                "</ol>",
                "</div>",
                "<div style='background-color: #fff3cd; padding: 10px; border-radius: 5px; margin: 10px 0;'>",
                "<p style='margin: 0; color: #856404;'><strong>💡 Pro Tip:</strong> The Economist style is perfect for showing price variations, outcome distributions, or any data where you want to emphasize distribution characteristics in a compact, elegant format.</p>",
                "</div>",
                "</div>"
            )
            
            self$results$instructions$setContent(instructions_html)
            
            # Set plot dimensions
            plot_width <- self$options$plot_width * 100
            plot_height <- self$options$plot_height * 100
            self$results$main_plot$setSize(plot_width, plot_height)
        },
        
        .run = function() {
            # Clear instructions if analysis is ready
            if (!is.null(self$options$y_var) && !is.null(self$options$x_var)) {
                self$results$instructions$setContent("")
            }
            
            # Early validation
            if (is.null(self$options$y_var)) {
                return()
            }
            
            if (is.null(self$options$x_var)) {
                return()
            }
            
            # Check for ggeconodist package
            if (!requireNamespace("ggeconodist", quietly = TRUE)) {
                
                # Check for fonts and provide guidance
                font_status <- self$.check_font_availability()
                
                # Create comprehensive error message with font guidance
                font_guidance <- if (!font_status$optimal_available) {
                    paste(
                        "<div style='background-color: #fff3cd; padding: 10px; border-radius: 5px; margin: 10px 0;'>",
                        "<h4 style='color: #856404; margin: 0 0 8px 0;'>📖 Font Installation Guide</h4>",
                        "<p style='margin: 5px 0; color: #856404;'><strong>Current font:</strong> ", font_status$best_font, "</p>",
                        if (!font_status$optimal_available) {
                            paste(
                                "<p style='margin: 5px 0; color: #856404;'><strong>For optimal Economist styling, install:</strong></p>",
                                "<ul style='margin: 5px 0; color: #856404;'>",
                                "<li>ITC Officina Sans (The Economist's primary font)</li>",
                                "<li>IBM Plex Sans Condensed (free alternative from Google Fonts)</li>",
                                "<li>Verdana (good substitute, usually pre-installed)</li>",
                                "</ul>",
                                "<p style='margin: 5px 0; color: #856404; font-size: 0.9em;'>",
                                "💡 See ECONOMIST_FONTS_INSTALLATION.md for detailed instructions",
                                "</p>"
                            )
                        } else "",
                        "</div>"
                    )
                } else ""
                
                error_msg <- paste(
                    "<div style='color: red; padding: 10px; border-radius: 5px; background-color: #f8d7da;'>",
                    "<strong>Package Required:</strong> The 'ggeconodist' package is required for Economist-style plots.",
                    "<br><br><strong>Installation Options:</strong>",
                    "<br>• CRAN: <code>install.packages('ggeconodist')</code>",
                    "<br>• GitHub: <code>remotes::install_github('hrbrmstr/ggeconodist')</code>",
                    "<br>• Alternative: <code>install.packages('ggeconodist', repos = 'https://cinc.rud.is')</code>",
                    "</div>",
                    font_guidance
                )
                self$results$main_plot$setContent(error_msg)
                return()
            }
            
            # Data validation
            if (nrow(self$data) == 0) {
                stop("Data contains no (complete) rows")
            }
            
            # Process data
            private$.processed_data <- private$.process_data()
            
            # Generate statistical analysis if requested
            if (self$options$add_statistics) {
                private$.generate_statistical_results()
            }
            
            # Generate summary statistics
            if (self$options$summary_statistics) {
                private$.generate_summary_statistics()
            }
            
            # Generate Economist legend information
            if (self$options$show_legend) {
                private$.generate_legend_info()
            }
            
            # Generate pairwise comparisons if requested
            if (self$options$comparison_annotations && self$options$add_statistics) {
                private$.generate_pairwise_comparisons()
            }
            
            # Generate R code output
            if (self$options$export_economist_code) {
                private$.generate_r_code()
            }
            
            # Generate interpretation guide
            private$.generate_interpretation_guide()
            
            # Set plot state for rendering
            self$results$main_plot$setState(private$.processed_data)
        },
        
        .process_data = function() {
            mydata <- self$data
            
            # Get variable names
            y_var <- self$options$y_var
            x_var <- self$options$x_var
            facet_var <- self$options$facet_var
            color_var <- self$options$color_var
            
            # Select relevant variables
            vars_to_select <- c(y_var, x_var)
            if (!is.null(facet_var)) vars_to_select <- c(vars_to_select, facet_var)
            if (!is.null(color_var)) vars_to_select <- c(vars_to_select, color_var)
            
            mydata <- mydata[vars_to_select]
            
            # Remove missing values
            mydata <- mydata[complete.cases(mydata), ]
            
            # Handle outlier treatment
            if (self$options$outlier_treatment == "hide_extreme") {
                # Remove extreme outliers (beyond 3 IQR)
                Q1 <- quantile(mydata[[y_var]], 0.25, na.rm = TRUE)
                Q3 <- quantile(mydata[[y_var]], 0.75, na.rm = TRUE)
                IQR <- Q3 - Q1
                lower_bound <- Q1 - 3 * IQR
                upper_bound <- Q3 + 3 * IQR
                mydata <- mydata[mydata[[y_var]] >= lower_bound & mydata[[y_var]] <= upper_bound, ]
            } else if (self$options$outlier_treatment == "transform") {
                # Log transform if highly skewed
                if (min(mydata[[y_var]], na.rm = TRUE) > 0) {
                    skewness <- moments::skewness(mydata[[y_var]], na.rm = TRUE)
                    if (abs(skewness) > 2) {
                        mydata[[paste0(y_var, "_log")]] <- log(mydata[[y_var]])
                    }
                }
            }
            
            # Mark outliers if requested
            if (self$options$outlier_treatment == "mark") {
                mydata <- mydata %>%
                    dplyr::group_by(.data[[x_var]]) %>%
                    dplyr::mutate(
                        is_outlier = abs(.data[[y_var]] - median(.data[[y_var]], na.rm = TRUE)) > 
                                    2 * mad(.data[[y_var]], na.rm = TRUE)
                    ) %>%
                    dplyr::ungroup()
            }
            
            return(mydata)
        },
        
        .check_font_availability = function() {
            # Check for Economist fonts and provide status
            available_fonts <- try({
                if (requireNamespace("systemfonts", quietly = TRUE)) {
                    systemfonts::system_fonts()$family
                } else {
                    # Fallback to basic check
                    c("Arial", "Helvetica", "Verdana", "sans")
                }
            }, silent = TRUE)
            
            if (inherits(available_fonts, "try-error")) {
                available_fonts <- c("Arial", "Helvetica", "Verdana", "sans")
            }
            
            # Check for Economist fonts (in order of preference)
            economist_fonts <- list(
                optimal = c("ITC Officina Sans", "EconSansCndReg", "Economist Sans"),
                good = c("IBM Plex Sans", "IBM Plex Sans Condensed", "Verdana"),
                acceptable = c("Arial", "Helvetica", "sans")
            )
            
            status <- list(
                optimal_available = any(economist_fonts$optimal %in% available_fonts),
                good_available = any(economist_fonts$good %in% available_fonts),
                best_font = "sans"  # default fallback
            )
            
            # Find the best available font
            for (font in c(economist_fonts$optimal, economist_fonts$good, economist_fonts$acceptable)) {
                if (font %in% available_fonts || font == "sans") {
                    status$best_font <- font
                    break
                }
            }
            
            return(status)
        },
        
        .generate_statistical_results = function() {
            if (!self$options$add_statistics) return()
            
            data <- private$.processed_data
            y_var <- self$options$y_var
            x_var <- self$options$x_var
            stat_method <- self$options$stat_method
            
            tryCatch({
                stat_html <- "<h4>Statistical Analysis Results</h4>"
                stat_html <- paste0(stat_html, "<div style='background-color: #e8f5e8; padding: 15px; border-radius: 5px; margin: 10px 0;'>")
                
                if (stat_method == "anova") {
                    # One-way ANOVA
                    formula_str <- paste(y_var, "~", x_var)
                    aov_result <- aov(as.formula(formula_str), data = data)
                    aov_summary <- summary(aov_result)
                    
                    f_stat <- round(aov_summary[[1]]$`F value`[1], 3)
                    p_value <- aov_summary[[1]]$`Pr(>F)`[1]
                    df1 <- aov_summary[[1]]$Df[1]
                    df2 <- aov_summary[[1]]$Df[2]
                    
                    stat_html <- paste0(stat_html, "<p><strong>One-way ANOVA Results:</strong></p>")
                    stat_html <- paste0(stat_html, "<p>F(", df1, ", ", df2, ") = ", f_stat, ", p = ", format.pval(p_value, digits = 3), "</p>")
                    
                    # Effect size (eta-squared)
                    if (self$options$effect_size) {
                        ss_total <- sum(aov_summary[[1]]$`Sum Sq`)
                        ss_between <- aov_summary[[1]]$`Sum Sq`[1]
                        eta_squared <- ss_between / ss_total
                        stat_html <- paste0(stat_html, "<p><strong>Effect Size (η²):</strong> ", round(eta_squared, 3), "</p>")
                        
                        # Interpretation
                        if (eta_squared < 0.01) {
                            effect_interp <- "Very small effect"
                        } else if (eta_squared < 0.06) {
                            effect_interp <- "Small effect"
                        } else if (eta_squared < 0.14) {
                            effect_interp <- "Medium effect"
                        } else {
                            effect_interp <- "Large effect"
                        }
                        stat_html <- paste0(stat_html, "<p><em>", effect_interp, "</em></p>")
                    }
                    
                } else if (stat_method == "kruskal") {
                    # Kruskal-Wallis test
                    formula_str <- paste(y_var, "~", x_var)
                    k_result <- kruskal.test(as.formula(formula_str), data = data)
                    
                    stat_html <- paste0(stat_html, "<p><strong>Kruskal-Wallis Test Results:</strong></p>")
                    stat_html <- paste0(stat_html, "<p>χ² = ", round(k_result$statistic, 3), ", df = ", k_result$parameter, ", p = ", format.pval(k_result$p.value, digits = 3), "</p>")
                    
                } else if (stat_method == "ttest") {
                    # T-test (assuming two groups)
                    groups <- unique(data[[x_var]])
                    if (length(groups) == 2) {
                        group1_data <- data[data[[x_var]] == groups[1], y_var]
                        group2_data <- data[data[[x_var]] == groups[2], y_var]
                        
                        t_result <- t.test(group1_data, group2_data)
                        
                        stat_html <- paste0(stat_html, "<p><strong>Independent t-test Results:</strong></p>")
                        stat_html <- paste0(stat_html, "<p>t(", round(t_result$parameter, 1), ") = ", round(t_result$statistic, 3), ", p = ", format.pval(t_result$p.value, digits = 3), "</p>")
                        stat_html <- paste0(stat_html, "<p>Mean difference: ", round(diff(t_result$estimate), 3), "</p>")
                        
                        # Cohen's d effect size
                        if (self$options$effect_size) {
                            pooled_sd <- sqrt(((length(group1_data) - 1) * var(group1_data) + 
                                             (length(group2_data) - 1) * var(group2_data)) / 
                                            (length(group1_data) + length(group2_data) - 2))
                            cohens_d <- abs(diff(c(mean(group1_data), mean(group2_data)))) / pooled_sd
                            stat_html <- paste0(stat_html, "<p><strong>Effect Size (Cohen's d):</strong> ", round(cohens_d, 3), "</p>")
                        }
                    } else {
                        stat_html <- paste0(stat_html, "<p style='color: orange;'>t-test requires exactly 2 groups. Found ", length(groups), " groups.</p>")
                    }
                }
                
                # Add interpretation
                if (exists("p_value") || (stat_method == "kruskal" && exists("k_result"))) {
                    p_val <- if (stat_method == "kruskal") k_result$p.value else p_value
                    
                    if (p_val < 0.001) {
                        interp <- "Highly significant difference (p < 0.001)"
                        color <- "green"
                    } else if (p_val < 0.01) {
                        interp <- "Very significant difference (p < 0.01)"
                        color <- "green"
                    } else if (p_val < 0.05) {
                        interp <- "Significant difference (p < 0.05)"
                        color <- "green"
                    } else {
                        interp <- "No significant difference (p ≥ 0.05)"
                        color <- "orange"
                    }
                    
                    stat_html <- paste0(stat_html, "<p style='color: ", color, "; font-weight: bold;'>", interp, "</p>")
                }
                
                stat_html <- paste0(stat_html, "</div>")
                
                self$results$statistical_results$setContent(stat_html)
                
            }, error = function(e) {
                error_msg <- paste("Error in statistical analysis:", e$message)
                self$results$statistical_results$setContent(paste("<p style='color: red;'>", error_msg, "</p>"))
            })
        },
        
        .generate_summary_statistics = function() {
            if (!self$options$summary_statistics) return()
            
            data <- private$.processed_data
            y_var <- self$options$y_var
            x_var <- self$options$x_var
            
            # Calculate comprehensive summary statistics
            summary_stats <- data %>%
                dplyr::group_by(.data[[x_var]]) %>%
                dplyr::summarise(
                    n = dplyr::n(),
                    mean = mean(.data[[y_var]], na.rm = TRUE),
                    median = median(.data[[y_var]], na.rm = TRUE),
                    sd = sd(.data[[y_var]], na.rm = TRUE),
                    min = min(.data[[y_var]], na.rm = TRUE),
                    max = max(.data[[y_var]], na.rm = TRUE),
                    q25 = quantile(.data[[y_var]], 0.25, na.rm = TRUE),
                    q75 = quantile(.data[[y_var]], 0.75, na.rm = TRUE),
                    q10 = quantile(.data[[y_var]], 0.10, na.rm = TRUE),
                    q90 = quantile(.data[[y_var]], 0.90, na.rm = TRUE),
                    .groups = "drop"
                )
            
            # Create HTML summary table
            summary_html <- "<h4>Distribution Summary Statistics</h4>"
            summary_html <- paste0(summary_html, "<div style='background-color: #f8f9fa; padding: 15px; border-radius: 5px; margin: 10px 0;'>")
            
            summary_html <- paste0(summary_html, "<table class='table table-striped' style='margin: 10px 0; max-width: 900px;'>")
            summary_html <- paste0(summary_html, "<thead><tr>")
            summary_html <- paste0(summary_html, "<th>Group</th><th>N</th><th>Mean</th><th>Median</th><th>SD</th>")
            summary_html <- paste0(summary_html, "<th>10th %ile</th><th>90th %ile</th><th>Range</th>")
            summary_html <- paste0(summary_html, "</tr></thead><tbody>")
            
            for (i in 1:nrow(summary_stats)) {
                group_name <- summary_stats[[x_var]][i]
                n <- summary_stats$n[i]
                mean_val <- round(summary_stats$mean[i], 2)
                median_val <- round(summary_stats$median[i], 2)
                sd_val <- round(summary_stats$sd[i], 2)
                q10_val <- round(summary_stats$q10[i], 2)
                q90_val <- round(summary_stats$q90[i], 2)
                range_val <- paste0(round(summary_stats$min[i], 2), " - ", round(summary_stats$max[i], 2))
                
                summary_html <- paste0(summary_html, "<tr>")
                summary_html <- paste0(summary_html, "<td><strong>", group_name, "</strong></td>")
                summary_html <- paste0(summary_html, "<td>", n, "</td>")
                summary_html <- paste0(summary_html, "<td>", mean_val, "</td>")
                summary_html <- paste0(summary_html, "<td>", median_val, "</td>")
                summary_html <- paste0(summary_html, "<td>", sd_val, "</td>")
                summary_html <- paste0(summary_html, "<td style='color: #c7254e; font-weight: bold;'>", q10_val, "</td>")
                summary_html <- paste0(summary_html, "<td style='color: #18bc9c; font-weight: bold;'>", q90_val, "</td>")
                summary_html <- paste0(summary_html, "<td>", range_val, "</td>")
                summary_html <- paste0(summary_html, "</tr>")
            }
            
            summary_html <- paste0(summary_html, "</tbody></table>")
            summary_html <- paste0(summary_html, "<p style='color: #666; font-size: 0.9em;'><strong>Note:</strong> 10th and 90th percentiles are highlighted in Economist-style colors.</p>")
            summary_html <- paste0(summary_html, "</div>")
            
            self$results$summary_statistics$setContent(summary_html)
        },
        
        .generate_legend_info = function() {
            legend_html <- "<h4>Economist Distribution Legend</h4>"
            legend_html <- paste0(legend_html, "<div style='background-color: #f3e5f5; padding: 15px; border-radius: 5px; margin: 10px 0;'>")
            
            legend_html <- paste0(legend_html, "<p><strong>Understanding the Economist-Style Distribution Plot:</strong></p>")
            legend_html <- paste0(legend_html, "<ul style='margin: 5px 0; padding-left: 20px; line-height: 1.8;'>")
            legend_html <- paste0(legend_html, "<li><span style='color: ", self$options$tenth_color, "; font-weight: bold;'>●</span> <strong>10th Percentile:</strong> 10% of values fall below this point</li>")
            legend_html <- paste0(legend_html, "<li><span style='color: ", self$options$median_color, "; font-weight: bold;'>●</span> <strong>Median (50th Percentile):</strong> Middle value of the distribution</li>")
            legend_html <- paste0(legend_html, "<li><span style='color: ", self$options$ninetieth_color, "; font-weight: bold;'>●</span> <strong>90th Percentile:</strong> 90% of values fall below this point</li>")
            legend_html <- paste0(legend_html, "<li><span style='color: ", self$options$distribution_fill, "; font-weight: bold;'>▓</span> <strong>Distribution Area:</strong> Shows the shape and spread of data</li>")
            legend_html <- paste0(legend_html, "</ul>")
            
            legend_html <- paste0(legend_html, "<h5>Visual Design Principles:</h5>")
            legend_html <- paste0(legend_html, "<ul style='margin: 5px 0; padding-left: 20px; line-height: 1.6;'>")
            legend_html <- paste0(legend_html, "<li><strong>Compact Design:</strong> Maximum information in minimal space</li>")
            legend_html <- paste0(legend_html, "<li><strong>Clear Hierarchy:</strong> Key percentiles emphasized through color</li>")
            legend_html <- paste0(legend_html, "<li><strong>Professional Typography:</strong> Clean, readable text styling</li>")
            legend_html <- paste0(legend_html, "<li><strong>Consistent Branding:</strong> Follows The Economist's visual standards</li>")
            legend_html <- paste0(legend_html, "</ul>")
            
            legend_html <- paste0(legend_html, "</div>")
            
            self$results$economist_legend_info$setContent(legend_html)
        },
        
        .generate_pairwise_comparisons = function() {
            # Implementation for pairwise comparisons would go here
            # This is a simplified version
            comp_html <- "<h4>Pairwise Comparisons</h4>"
            comp_html <- paste0(comp_html, "<div style='background-color: #fff3e0; padding: 15px; border-radius: 5px; margin: 10px 0;'>")
            comp_html <- paste0(comp_html, "<p>Pairwise comparison analysis would be displayed here when implemented.</p>")
            comp_html <- paste0(comp_html, "</div>")
            
            self$results$comparison_results$setContent(comp_html)
        },
        
        .generate_r_code = function() {
            y_var <- self$options$y_var
            x_var <- self$options$x_var
            
            code_template <- paste(
                "# Economist-Style Distribution Plot",
                "# Generated by ClinicoPath jamovi module",
                "",
                "library(ggeconodist)",
                "library(ggplot2)",
                "",
                "# Create the plot",
                "plot <- ggplot(data, aes(x = {x_var}, y = {y_var})) +",
                "  geom_econodist() +",
                "  theme_econodist()",
                "",
                "# Add Economist legend",
                "if (show_legend) {",
                "  plot <- add_econodist_legend(plot)",
                "}",
                "",
                "# Display the plot",
                "print(plot)",
                sep = "\n"
            )
            
            # Replace placeholders
            actual_code <- gsub("\\{x_var\\}", x_var %||% "x_variable", code_template)
            actual_code <- gsub("\\{y_var\\}", y_var %||% "y_variable", actual_code)
            
            code_html <- paste(
                "<h4>Reproducible R Code</h4>",
                "<div style='background-color: #f8f9fa; padding: 15px; border-radius: 5px; margin: 10px 0;'>",
                "<p><strong>Complete R code using ggeconodist package:</strong></p>",
                "<pre style='background-color: #2d3748; color: #e2e8f0; padding: 15px; border-radius: 5px; overflow-x: auto;'>",
                "<code>", actual_code, "</code>",
                "</pre>",
                "<p style='color: #666; font-size: 0.9em; margin-top: 10px;'>",
                "Copy this code to reproduce the Economist-style visualization in your R environment.",
                "</p>",
                "</div>"
            )
            
            self$results$r_code_output$setContent(code_html)
        },
        
        .generate_interpretation_guide = function() {
            style <- self$options$distribution_style
            
            interp_html <- "<h4>Plot Interpretation Guide</h4>"
            interp_html <- paste0(interp_html, "<div style='background-color: #e8f4fd; padding: 15px; border-radius: 5px; margin: 10px 0;'>")
            
            # Style-specific guidance
            if (style == "classic") {
                interp_html <- paste0(interp_html, "<p><strong>Classic Economist Style:</strong></p>")
                interp_html <- paste0(interp_html, "<p>This visualization follows The Economist's signature approach to data presentation, emphasizing clarity and elegance.</p>")
            } else if (style == "enhanced") {
                interp_html <- paste0(interp_html, "<p><strong>Enhanced Style:</strong></p>")
                interp_html <- paste0(interp_html, "<p>Enhanced version with additional visual elements for deeper data exploration.</p>")
            } else if (style == "publication") {
                interp_html <- paste0(interp_html, "<p><strong>Publication Style:</strong></p>")
                interp_html <- paste0(interp_html, "<p>Optimized for academic and professional publications with precise formatting.</p>")
            }
            
            # General interpretation guidelines
            interp_html <- paste0(interp_html, "<h5>Reading Distribution Plots:</h5>")
            interp_html <- paste0(interp_html, "<ul style='margin: 5px 0; padding-left: 20px; line-height: 1.6;'>")
            interp_html <- paste0(interp_html, "<li><strong>Width:</strong> Indicates frequency/density of values at that level</li>")
            interp_html <- paste0(interp_html, "<li><strong>Height:</strong> Shows the range of values in each group</li>")
            interp_html <- paste0(interp_html, "<li><strong>Percentiles:</strong> Key reference points for comparison</li>")
            interp_html <- paste0(interp_html, "<li><strong>Shape:</strong> Reveals distribution characteristics (skewness, modality)</li>")
            interp_html <- paste0(interp_html, "</ul>")
            
            # Clinical research applications
            interp_html <- paste0(interp_html, "<h5>Clinical Research Applications:</h5>")
            interp_html <- paste0(interp_html, "<ul style='margin: 5px 0; padding-left: 20px; line-height: 1.6;'>")
            interp_html <- paste0(interp_html, "<li><strong>Treatment Outcomes:</strong> Compare distributions across treatment groups</li>")
            interp_html <- paste0(interp_html, "<li><strong>Biomarker Analysis:</strong> Visualize biomarker distributions by patient subgroups</li>")
            interp_html <- paste0(interp_html, "<li><strong>Cost Analysis:</strong> Show variation in healthcare costs or resource utilization</li>")
            interp_html <- paste0(interp_html, "<li><strong>Quality Metrics:</strong> Display quality indicators across institutions or time periods</li>")
            interp_html <- paste0(interp_html, "</ul>")
            
            interp_html <- paste0(interp_html, "</div>")
            
            self$results$interpretation_guide$setContent(interp_html)
        },
        
        # Plot rendering function
        .plot_main = function(image, ggtheme, theme, ...) {
            data <- image$state
            if (is.null(data)) return()
            
            y_var <- self$options$y_var
            x_var <- self$options$x_var
            
            # Check font availability for optimal rendering
            font_status <- private$.check_font_availability()
            selected_font <- font_status$best_font
            
            tryCatch({
                # Check if ggeconodist is available
                if (!requireNamespace("ggeconodist", quietly = TRUE)) {
                    warning("ggeconodist package not available, using fallback plot")
                    return(private$.create_fallback_plot(data, x_var, y_var, ggtheme, selected_font))
                }
                
                # Create the base plot
                plot <- ggplot2::ggplot(data, ggplot2::aes(x = .data[[x_var]], y = .data[[y_var]]))
                
                # Add econodist geom
                plot <- plot + ggeconodist::geom_econodist()
                
                # Apply economist theme with font optimization
                if (self$options$economist_theme) {
                    if (selected_font != "sans" && selected_font %in% c("ITC Officina Sans", "EconSansCndReg", "Economist Sans", "IBM Plex Sans", "Verdana")) {
                        plot <- plot + ggeconodist::theme_econodist(base_family = selected_font)
                    } else {
                        plot <- plot + ggeconodist::theme_econodist()
                    }
                    
                    # Add additional font styling if using optimal fonts
                    if (selected_font != "sans") {
                        plot <- plot + ggplot2::theme(
                            text = ggplot2::element_text(family = selected_font),
                            plot.title = ggplot2::element_text(family = selected_font, face = "bold"),
                            axis.title = ggplot2::element_text(family = selected_font),
                            axis.text = ggplot2::element_text(family = selected_font),
                            strip.text = ggplot2::element_text(family = selected_font)
                        )
                    }
                } else {
                    plot <- plot + ggtheme
                }
                
                # Add faceting if specified
                if (!is.null(self$options$facet_var)) {
                    plot <- plot + ggplot2::facet_wrap(as.formula(paste("~", self$options$facet_var)))
                }
                
                # Apply orientation
                if (self$options$plot_orientation == "horizontal") {
                    plot <- plot + ggplot2::coord_flip()
                }
                
                # Add individual points if requested
                if (self$options$show_points) {
                    plot <- plot + ggplot2::geom_point(
                        position = ggplot2::position_jitter(width = self$options$point_jitter),
                        alpha = 0.5,
                        size = 1
                    )
                }
                
                # Apply custom titles
                titles <- private$.get_plot_titles()
                plot <- plot + ggplot2::labs(
                    title = titles$title,
                    x = titles$x,
                    y = titles$y,
                    caption = titles$caption
                )
                
                # Apply left alignment if requested
                if (self$options$left_align_title) {
                    plot <- ggeconodist::left_align(plot)
                }
                
                # Add economist legend if requested
                if (self$options$show_legend && requireNamespace("ggeconodist", quietly = TRUE)) {
                    plot <- ggeconodist::add_econodist_legend(plot)
                }
                
                print(plot)
                TRUE
                
            }, error = function(e) {
                warning(paste("Error creating econodist plot:", e$message))
                # Fallback to basic plot
                return(private$.create_fallback_plot(data, x_var, y_var, ggtheme))
            })
        },
        
        .create_fallback_plot = function(data, x_var, y_var, ggtheme, selected_font = "sans") {
            # Fallback plot when ggeconodist is not available
            plot <- ggplot2::ggplot(data, ggplot2::aes(x = .data[[x_var]], y = .data[[y_var]])) +
                ggplot2::geom_boxplot(fill = self$options$distribution_fill, alpha = self$options$alpha_level) +
                ggplot2::geom_point(position = ggplot2::position_jitter(width = 0.2), alpha = 0.6) +
                ggtheme +
                ggplot2::labs(
                    title = "Distribution Plot (Fallback)",
                    subtitle = "Install 'ggeconodist' package for Economist-style visualization",
                    x = x_var,
                    y = y_var
                )
            
            # Apply font styling if available
            if (selected_font != "sans") {
                plot <- plot + ggplot2::theme(
                    text = ggplot2::element_text(family = selected_font),
                    plot.title = ggplot2::element_text(family = selected_font, face = "bold"),
                    plot.subtitle = ggplot2::element_text(family = selected_font),
                    axis.title = ggplot2::element_text(family = selected_font),
                    axis.text = ggplot2::element_text(family = selected_font)
                )
            }
            
            print(plot)
            TRUE
        },
        
        .get_plot_titles = function() {
            list(
                title = if (self$options$plot_title != "") self$options$plot_title else NULL,
                x = if (self$options$x_title != "") self$options$x_title else self$options$x_var,
                y = if (self$options$y_title != "") self$options$y_title else self$options$y_var,
                caption = if (self$options$caption_text != "") self$options$caption_text else NULL
            )
        }
    )
)

# Helper function for NULL coalescing
`%||%` <- function(x, y) if (is.null(x)) y else x