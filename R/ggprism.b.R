#' @title GraphPad Prism Style Plots
#' @return Publication-ready plots with Prism styling using ggprism package
#'
#' @importFrom R6 R6Class
#' @import jmvcore
#' @importFrom magrittr %>%
#' @importFrom ggplot2 ggplot aes geom_violin geom_boxplot geom_point geom_col geom_line
#' @importFrom ggplot2 labs theme facet_wrap coord_flip position_dodge position_jitter
#' @importFrom ggplot2 stat_summary geom_errorbar scale_x_discrete scale_y_continuous
#' @importFrom ggprism theme_prism scale_color_prism scale_fill_prism add_pvalue
#' @importFrom dplyr group_by summarise mutate n
#' @importFrom stats t.test wilcox.test aov kruskal.test shapiro.test
#' @importFrom htmltools HTML

ggprismClass <- if (requireNamespace("jmvcore")) R6::R6Class("ggprismClass",
    inherit = ggprismBase,
    private = list(

        .run = function() {

            # Check if required variables have been selected
            if (is.null(self$options$x_var) || is.null(self$options$y_var)) {
                intro_msg <- "
                <div style='background-color: #e3f2fd; padding: 20px; border-radius: 8px; margin: 20px 0;'>
                <h3 style='color: #1976d2; margin-top: 0;'>🎨 Welcome to GraphPad Prism Style Plots!</h3>
                <p><strong>Publication-ready visualizations</strong> with authentic GraphPad Prism styling</p>
                <p>Based on the ggprism R package for creating professional scientific graphics</p>
                
                <h4 style='color: #1976d2;'>Required Variables:</h4>
                <ol>
                <li><strong>X-Axis Variable:</strong> Categorical or continuous variable</li>
                <li><strong>Y-Axis Variable:</strong> Continuous variable for analysis</li>
                </ol>
                
                <h4 style='color: #1976d2;'>What Prism Style Plots Offer:</h4>
                <ul>
                <li><strong>Authentic Styling:</strong> True GraphPad Prism look and feel</li>
                <li><strong>Multiple Plot Types:</strong> Violin, box, scatter, column, and line plots</li>
                <li><strong>Color Palettes:</strong> Prism-inspired color schemes (floral, candy, neon, etc.)</li>
                <li><strong>Statistical Annotations:</strong> Automated p-value displays</li>
                <li><strong>Publication Ready:</strong> Professional formatting for journals</li>
                </ul>
                
                <h4 style='color: #1976d2;'>Perfect For:</h4>
                <ul>
                <li><strong>Clinical Research:</strong> Professional biostatistics visualizations</li>
                <li><strong>Academic Publications:</strong> Journal-quality figure formatting</li>
                <li><strong>Presentations:</strong> Clear, impactful scientific graphics</li>
                <li><strong>Data Analysis:</strong> Statistical comparisons with visual appeal</li>
                </ul>
                
                <p style='font-size: 12px; color: #555; margin-top: 20px;'>
                💡 <em>Bring the professional look of GraphPad Prism to your R-based analysis workflows</em>
                </p>
                </div>"
                
                self$results$todo$setContent(intro_msg)
                return()
            } else {
                self$results$todo$setContent("")
            }

            # Validate dataset
            if (nrow(self$data) == 0) {
                stop("Error: The provided dataset contains no complete rows. Please check your data and try again.")
            }

            # Safely require ggprism
            if (!requireNamespace("ggprism", quietly = TRUE)) {
                error_msg <- "
                <div style='color: red; background-color: #ffebee; padding: 20px; border-radius: 8px;'>
                <h4>ggprism Package Required</h4>
                <p>The ggprism package is required for GraphPad Prism style plots.</p>
                <p>Please install it using: <code>install.packages('ggprism')</code></p>
                </div>"
                self$results$interpretation$setContent(error_msg)
                return()
            }

            # Get data and variables
            dataset <- self$data
            x_var <- self$options$x_var
            y_var <- self$options$y_var
            group_var <- self$options$group_var
            facet_var <- self$options$facet_var

            # Prepare analysis data
            required_vars <- c(x_var, y_var)
            if (!is.null(group_var) && group_var != "") {
                required_vars <- c(required_vars, group_var)
            }
            if (!is.null(facet_var) && facet_var != "") {
                required_vars <- c(required_vars, facet_var)
            }
            
            analysis_data <- dataset[required_vars]
            analysis_data <- analysis_data[complete.cases(analysis_data), ]
            
            if (nrow(analysis_data) == 0) {
                stop("Error: No complete cases found for the selected variables.")
            }

            # Convert variables to appropriate types
            analysis_data[[y_var]] <- as.numeric(analysis_data[[y_var]])
            
            # Handle x_var type conversion
            if (is.factor(analysis_data[[x_var]]) || is.character(analysis_data[[x_var]])) {
                analysis_data[[x_var]] <- as.factor(analysis_data[[x_var]])
            } else {
                analysis_data[[x_var]] <- as.numeric(analysis_data[[x_var]])
            }
            
            if (!is.null(group_var) && group_var != "") {
                analysis_data[[group_var]] <- as.factor(analysis_data[[group_var]])
            }
            if (!is.null(facet_var) && facet_var != "") {
                analysis_data[[facet_var]] <- as.factor(analysis_data[[facet_var]])
            }

            # Generate summary statistics if requested
            if (self$options$show_summary) {
                summary_html <- private$.generate_summary_statistics(analysis_data, x_var, y_var, group_var)
                self$results$summary$setContent(summary_html)
            }
            
            # Generate statistical tests if requested
            if (self$options$show_statistics) {
                stats_html <- private$.generate_statistical_tests(analysis_data, x_var, y_var, group_var)
                self$results$statistics$setContent(stats_html)
            }
            
            # Generate interpretation guide
            interpretation_html <- private$.generate_interpretation_guide(analysis_data, x_var, y_var, group_var)
            self$results$interpretation$setContent(interpretation_html)

            # Store data for plotting
            private$.analysis_data <- analysis_data

        },

        .plot = function(image, ggtheme, theme, ...) {
            
            # Check if analysis was performed
            if (is.null(private$.analysis_data)) {
                return()
            }
            
            analysis_data <- private$.analysis_data
            x_var <- self$options$x_var
            y_var <- self$options$y_var
            group_var <- self$options$group_var
            facet_var <- self$options$facet_var
            plot_type <- self$options$plot_type
            
            # Determine grouping variable for aesthetics
            group_mapping <- if (!is.null(group_var) && group_var != "") group_var else NULL
            
            # Create base plot
            if (!is.null(group_mapping)) {
                p <- ggplot2::ggplot(analysis_data, ggplot2::aes_string(x = x_var, y = y_var, 
                                                                       fill = group_mapping, color = group_mapping))
            } else {
                p <- ggplot2::ggplot(analysis_data, ggplot2::aes_string(x = x_var, y = y_var))
            }
            
            # Add plot type specific geoms
            if (plot_type == "violin") {
                p <- p + ggplot2::geom_violin(alpha = 0.7, trim = FALSE)
                if (self$options$error_bars != "none") {
                    p <- p + ggplot2::stat_summary(fun.data = private$.get_error_function(), 
                                                  geom = "errorbar", width = 0.2)
                }
                if (self$options$show_points) {
                    p <- p + ggplot2::geom_point(position = ggplot2::position_jitter(width = 0.2), 
                                                size = self$options$point_size, alpha = self$options$point_alpha)
                }
                
            } else if (plot_type == "boxplot") {
                p <- p + ggplot2::geom_boxplot(alpha = 0.7)
                if (self$options$show_points) {
                    p <- p + ggplot2::geom_point(position = ggplot2::position_jitter(width = 0.2), 
                                                size = self$options$point_size, alpha = self$options$point_alpha)
                }
                
            } else if (plot_type == "scatter") {
                p <- p + ggplot2::geom_point(size = self$options$point_size, alpha = self$options$point_alpha)
                
            } else if (plot_type == "column") {
                # Create summary data for column plot
                if (!is.null(group_mapping)) {
                    summary_data <- analysis_data %>%
                        dplyr::group_by(.data[[x_var]], .data[[group_mapping]]) %>%
                        dplyr::summarise(mean_y = mean(.data[[y_var]], na.rm = TRUE),
                                       se_y = sd(.data[[y_var]], na.rm = TRUE) / sqrt(dplyr::n()),
                                       .groups = 'drop')
                    p <- ggplot2::ggplot(summary_data, ggplot2::aes_string(x = x_var, y = "mean_y", 
                                                                          fill = group_mapping))
                    p <- p + ggplot2::geom_col(position = "dodge", alpha = 0.8)
                    if (self$options$error_bars != "none") {
                        p <- p + ggplot2::geom_errorbar(ggplot2::aes(ymin = mean_y - se_y, ymax = mean_y + se_y),
                                                       position = ggplot2::position_dodge(width = 0.9), width = 0.25)
                    }
                } else {
                    summary_data <- analysis_data %>%
                        dplyr::group_by(.data[[x_var]]) %>%
                        dplyr::summarise(mean_y = mean(.data[[y_var]], na.rm = TRUE),
                                       se_y = sd(.data[[y_var]], na.rm = TRUE) / sqrt(dplyr::n()),
                                       .groups = 'drop')
                    p <- ggplot2::ggplot(summary_data, ggplot2::aes_string(x = x_var, y = "mean_y"))
                    p <- p + ggplot2::geom_col(alpha = 0.8)
                    if (self$options$error_bars != "none") {
                        p <- p + ggplot2::geom_errorbar(ggplot2::aes(ymin = mean_y - se_y, ymax = mean_y + se_y),
                                                       width = 0.25)
                    }
                }
                
            } else if (plot_type == "line") {
                if (!is.null(group_mapping)) {
                    p <- p + ggplot2::geom_line(ggplot2::aes_string(group = group_mapping), size = 1.2)
                } else {
                    p <- p + ggplot2::geom_line(size = 1.2)
                }
                if (self$options$show_points) {
                    p <- p + ggplot2::geom_point(size = self$options$point_size, alpha = self$options$point_alpha)
                }
            }
            
            # Apply Prism theme
            p <- p + private$.get_prism_theme()
            
            # Apply Prism color palette
            if (!is.null(group_mapping)) {
                palette_name <- self$options$prism_palette
                p <- p + ggprism::scale_fill_prism(palette = palette_name)
                p <- p + ggprism::scale_color_prism(palette = palette_name)
            }
            
            # Add faceting if specified
            if (!is.null(facet_var) && facet_var != "") {
                p <- p + ggplot2::facet_wrap(as.formula(paste("~", facet_var)))
            }
            
            # Add labels
            x_label <- if (self$options$x_label != "") self$options$x_label else x_var
            y_label <- if (self$options$y_label != "") self$options$y_label else y_var
            plot_title <- self$options$plot_title
            
            p <- p + ggplot2::labs(
                title = plot_title,
                x = x_label,
                y = y_label
            )
            
            # Set legend position
            legend_pos <- self$options$legend_position
            if (legend_pos == "none") {
                p <- p + ggplot2::theme(legend.position = "none")
            } else {
                p <- p + ggplot2::theme(legend.position = legend_pos)
            }
            
            print(p)
            TRUE
        },

        .get_prism_theme = function() {
            theme_name <- self$options$prism_theme
            base_size <- self$options$base_size
            
            base_theme <- switch(theme_name,
                "default" = ggprism::theme_prism(base_size = base_size),
                "white" = ggprism::theme_prism(base_size = base_size, axis_text_angle = 0),
                "minimal" = ggprism::theme_prism(base_size = base_size) + 
                    ggplot2::theme(panel.grid.major = ggplot2::element_blank()),
                "publication" = ggprism::theme_prism(base_size = base_size) +
                    ggplot2::theme(
                        plot.title = ggplot2::element_text(size = base_size + 2, face = "bold", hjust = 0.5),
                        axis.title = ggplot2::element_text(size = base_size + 1, face = "bold")
                    ),
                ggprism::theme_prism(base_size = base_size)  # fallback
            )
            
            return(base_theme)
        },

        .get_error_function = function() {
            error_type <- self$options$error_bars
            
            if (error_type == "se") {
                return(function(x) {
                    data.frame(
                        y = mean(x, na.rm = TRUE),
                        ymin = mean(x, na.rm = TRUE) - sd(x, na.rm = TRUE) / sqrt(length(x)),
                        ymax = mean(x, na.rm = TRUE) + sd(x, na.rm = TRUE) / sqrt(length(x))
                    )
                })
            } else if (error_type == "sd") {
                return(function(x) {
                    data.frame(
                        y = mean(x, na.rm = TRUE),
                        ymin = mean(x, na.rm = TRUE) - sd(x, na.rm = TRUE),
                        ymax = mean(x, na.rm = TRUE) + sd(x, na.rm = TRUE)
                    )
                })
            } else if (error_type == "ci") {
                return(function(x) {
                    ci <- t.test(x)$conf.int
                    data.frame(
                        y = mean(x, na.rm = TRUE),
                        ymin = ci[1],
                        ymax = ci[2]
                    )
                })
            }
        },

        .generate_summary_statistics = function(data, x_var, y_var, group_var) {
            # Generate summary statistics table
            
            if (!is.null(group_var) && group_var != "") {
                stats_summary <- data %>%
                    dplyr::group_by(.data[[group_var]]) %>%
                    dplyr::summarise(
                        n = dplyr::n(),
                        mean = round(mean(.data[[y_var]], na.rm = TRUE), 3),
                        median = round(median(.data[[y_var]], na.rm = TRUE), 3),
                        sd = round(sd(.data[[y_var]], na.rm = TRUE), 3),
                        se = round(sd(.data[[y_var]], na.rm = TRUE) / sqrt(dplyr::n()), 3),
                        min_val = round(min(.data[[y_var]], na.rm = TRUE), 3),
                        max_val = round(max(.data[[y_var]], na.rm = TRUE), 3),
                        .groups = 'drop'
                    )
                group_col <- group_var
            } else {
                stats_summary <- data %>%
                    dplyr::summarise(
                        group = "All Data",
                        n = dplyr::n(),
                        mean = round(mean(.data[[y_var]], na.rm = TRUE), 3),
                        median = round(median(.data[[y_var]], na.rm = TRUE), 3),
                        sd = round(sd(.data[[y_var]], na.rm = TRUE), 3),
                        se = round(sd(.data[[y_var]], na.rm = TRUE) / sqrt(dplyr::n()), 3),
                        min_val = round(min(.data[[y_var]], na.rm = TRUE), 3),
                        max_val = round(max(.data[[y_var]], na.rm = TRUE), 3)
                    )
                group_col <- "group"
            }
            
            summary_html <- paste0(
                "<div style='background-color: #f8f9fa; padding: 20px; border-radius: 8px; margin-bottom: 20px;'>",
                "<h3 style='color: #495057; margin-top: 0;'>📊 Summary Statistics</h3>",
                "<table style='width: 100%; border-collapse: collapse; font-family: Arial, sans-serif;'>",
                "<thead><tr style='background-color: #6c757d; color: white;'>",
                "<th style='padding: 8px; border: 1px solid #dee2e6;'>Group</th>",
                "<th style='padding: 8px; border: 1px solid #dee2e6;'>N</th>",
                "<th style='padding: 8px; border: 1px solid #dee2e6;'>Mean</th>",
                "<th style='padding: 8px; border: 1px solid #dee2e6;'>Median</th>",
                "<th style='padding: 8px; border: 1px solid #dee2e6;'>SD</th>",
                "<th style='padding: 8px; border: 1px solid #dee2e6;'>SE</th>",
                "<th style='padding: 8px; border: 1px solid #dee2e6;'>Range</th>",
                "</tr></thead><tbody>"
            )
            
            for (i in 1:nrow(stats_summary)) {
                row_bg <- if (i %% 2 == 0) "#ffffff" else "#f8f9fa"
                summary_html <- paste0(summary_html,
                    "<tr style='background-color: ", row_bg, ";'>",
                    "<td style='padding: 8px; border: 1px solid #dee2e6;'><strong>", stats_summary[[group_col]][i], "</strong></td>",
                    "<td style='padding: 8px; border: 1px solid #dee2e6; text-align: center;'>", stats_summary$n[i], "</td>",
                    "<td style='padding: 8px; border: 1px solid #dee2e6; text-align: center;'>", stats_summary$mean[i], "</td>",
                    "<td style='padding: 8px; border: 1px solid #dee2e6; text-align: center;'>", stats_summary$median[i], "</td>",
                    "<td style='padding: 8px; border: 1px solid #dee2e6; text-align: center;'>", stats_summary$sd[i], "</td>",
                    "<td style='padding: 8px; border: 1px solid #dee2e6; text-align: center;'>", stats_summary$se[i], "</td>",
                    "<td style='padding: 8px; border: 1px solid #dee2e6; text-align: center;'>", stats_summary$min_val[i], " - ", stats_summary$max_val[i], "</td>",
                    "</tr>"
                )
            }
            
            summary_html <- paste0(summary_html, 
                "</tbody></table>",
                "<p style='font-size: 12px; color: #6c757d; margin-top: 15px;'>",
                "<em>SD = Standard Deviation, SE = Standard Error of the Mean.</em>",
                "</p></div>"
            )
            
            return(summary_html)
        },

        .generate_statistical_tests = function(data, x_var, y_var, group_var) {
            # Perform statistical tests
            
            if (is.null(group_var) || group_var == "") {
                stats_html <- paste0(
                    "<div style='background-color: #fff3cd; padding: 20px; border-radius: 8px;'>",
                    "<h3 style='color: #856404; margin-top: 0;'>📈 Statistical Analysis</h3>",
                    "<p>No grouping variable specified. Statistical comparisons require a grouping variable.</p>",
                    "</div>"
                )
                return(stats_html)
            }
            
            groups <- levels(as.factor(data[[group_var]]))
            n_groups <- length(groups)
            stats_method <- self$options$stats_method
            
            # Automatic method selection
            if (stats_method == "auto") {
                if (n_groups == 2) {
                    # Check normality for automatic test selection
                    group1_data <- data[data[[group_var]] == groups[1], y_var]
                    group2_data <- data[data[[group_var]] == groups[2], y_var]
                    
                    if (length(group1_data) >= 3 && length(group2_data) >= 3) {
                        sw1 <- shapiro.test(group1_data)$p.value
                        sw2 <- shapiro.test(group2_data)$p.value
                        use_parametric <- (sw1 > 0.05 && sw2 > 0.05)
                    } else {
                        use_parametric <- FALSE
                    }
                    
                    test_method <- if (use_parametric) "t-test" else "Wilcoxon"
                } else {
                    # Multiple groups - use ANOVA or Kruskal-Wallis
                    overall_normal <- TRUE
                    for (group in groups) {
                        group_data <- data[data[[group_var]] == group, y_var]
                        if (length(group_data) >= 3 && length(group_data) <= 5000) {
                            if (shapiro.test(group_data)$p.value <= 0.05) {
                                overall_normal <- FALSE
                                break
                            }
                        }
                    }
                    test_method <- if (overall_normal) "ANOVA" else "Kruskal-Wallis"
                }
            } else {
                test_method <- switch(stats_method,
                    "ttest" = "t-test",
                    "wilcoxon" = "Wilcoxon",
                    "anova" = "ANOVA",
                    "kruskal" = "Kruskal-Wallis"
                )
            }
            
            # Perform the selected test
            if (test_method == "t-test" && n_groups == 2) {
                group1_data <- data[data[[group_var]] == groups[1], y_var]
                group2_data <- data[data[[group_var]] == groups[2], y_var]
                
                test_result <- t.test(group1_data, group2_data)
                test_stat <- round(test_result$statistic, 4)
                p_value <- round(test_result$p.value, 4)
                test_details <- paste0("t = ", test_stat, ", df = ", round(test_result$parameter, 1))
                
            } else if (test_method == "Wilcoxon" && n_groups == 2) {
                group1_data <- data[data[[group_var]] == groups[1], y_var]
                group2_data <- data[data[[group_var]] == groups[2], y_var]
                
                test_result <- wilcox.test(group1_data, group2_data)
                test_stat <- round(test_result$statistic, 4)
                p_value <- round(test_result$p.value, 4)
                test_details <- paste0("W = ", test_stat)
                
            } else if (test_method == "ANOVA") {
                formula_str <- paste(y_var, "~", group_var)
                aov_result <- aov(as.formula(formula_str), data = data)
                summary_aov <- summary(aov_result)
                
                f_stat <- round(summary_aov[[1]]$`F value`[1], 4)
                p_value <- round(summary_aov[[1]]$`Pr(>F)`[1], 4)
                df1 <- summary_aov[[1]]$Df[1]
                df2 <- summary_aov[[1]]$Df[2]
                test_details <- paste0("F(", df1, ",", df2, ") = ", f_stat)
                
            } else if (test_method == "Kruskal-Wallis") {
                formula_str <- paste(y_var, "~", group_var)
                kw_result <- kruskal.test(as.formula(formula_str), data = data)
                
                test_stat <- round(kw_result$statistic, 4)
                p_value <- round(kw_result$p.value, 4)
                test_details <- paste0("χ² = ", test_stat, ", df = ", kw_result$parameter)
            }
            
            # Format p-value based on user preference
            pvalue_format <- self$options$pvalue_format
            formatted_p <- private$.format_pvalue(p_value, pvalue_format)
            
            # Create results HTML
            significance <- if (p_value < 0.001) "Highly significant (***)" else 
                          if (p_value < 0.01) "Very significant (**)" else
                          if (p_value < 0.05) "Significant (*)" else "Not significant"
            
            stats_html <- paste0(
                "<div style='background-color: #f3e5f5; padding: 20px; border-radius: 8px; margin-bottom: 20px;'>",
                "<h3 style='color: #7b1fa2; margin-top: 0;'>📊 Statistical Test Results</h3>",
                "<table style='width: 100%; border-collapse: collapse;'>",
                "<tr><td style='padding: 8px; border: 1px solid #ddd;'><strong>Test Method:</strong></td><td style='padding: 8px; border: 1px solid #ddd;'>", test_method, "</td></tr>",
                "<tr><td style='padding: 8px; border: 1px solid #ddd;'><strong>Test Statistic:</strong></td><td style='padding: 8px; border: 1px solid #ddd;'>", test_details, "</td></tr>",
                "<tr><td style='padding: 8px; border: 1px solid #ddd;'><strong>P-value:</strong></td><td style='padding: 8px; border: 1px solid #ddd;'>", formatted_p, "</td></tr>",
                "<tr><td style='padding: 8px; border: 1px solid #ddd;'><strong>Result:</strong></td><td style='padding: 8px; border: 1px solid #ddd;'>", significance, "</td></tr>",
                "</table>",
                "<p style='font-size: 12px; color: #7b1fa2; margin-top: 15px;'>",
                "<em>* p < 0.05, ** p < 0.01, *** p < 0.001.</em>",
                "</p></div>"
            )
            
            return(stats_html)
        },

        .format_pvalue = function(p_value, format_type) {
            if (format_type == "exact") {
                return(paste0("p = ", sprintf("%.3f", p_value)))
            } else if (format_type == "scientific") {
                if (p_value < 0.001) {
                    return("p < 0.001")
                } else if (p_value < 0.01) {
                    return("p < 0.01")
                } else if (p_value < 0.05) {
                    return("p < 0.05")
                } else {
                    return(paste0("p = ", sprintf("%.3f", p_value)))
                }
            } else if (format_type == "stars") {
                if (p_value < 0.001) {
                    return("***")
                } else if (p_value < 0.01) {
                    return("**")
                } else if (p_value < 0.05) {
                    return("*")
                } else {
                    return("")
                }
            } else if (format_type == "symbols") {
                if (p_value < 0.001) {
                    return("***")
                } else if (p_value < 0.01) {
                    return("**")
                } else if (p_value < 0.05) {
                    return("*")
                } else {
                    return("ns")
                }
            }
        },

        .generate_interpretation_guide = function(data, x_var, y_var, group_var) {
            # Generate interpretation guide
            
            n_total <- nrow(data)
            plot_type <- self$options$plot_type
            prism_theme <- self$options$prism_theme
            prism_palette <- self$options$prism_palette
            
            interpretation_html <- paste0(
                "<div style='background-color: #e8f5e8; padding: 20px; border-radius: 8px;'>",
                "<h3 style='color: #2e7d32; margin-top: 0;'>🎨 GraphPad Prism Style Plot Guide</h3>",
                
                "<h4 style='color: #2e7d32;'>Plot Configuration:</h4>",
                "<ul>",
                "<li><strong>Plot Type:</strong> ", stringr::str_to_title(plot_type), " plot with Prism styling</li>",
                "<li><strong>Theme:</strong> ", stringr::str_to_title(prism_theme), " Prism theme</li>",
                "<li><strong>Color Palette:</strong> ", stringr::str_to_title(prism_palette), " color scheme</li>",
                "<li><strong>Variables:</strong> ", y_var, " by ", x_var, 
                if (!is.null(group_var) && group_var != "") paste0(", grouped by ", group_var) else "", "</li>",
                "<li><strong>Observations:</strong> ", n_total, " data points</li>",
                "</ul>",
                
                "<h4 style='color: #2e7d32;'>GraphPad Prism Features:</h4>",
                "<ul>",
                "<li><strong>Authentic Styling:</strong> Professional GraphPad Prism look and feel</li>",
                "<li><strong>Publication Ready:</strong> Journal-quality formatting and typography</li>",
                "<li><strong>Color Schemes:</strong> Carefully designed palettes for scientific visualization</li>",
                "<li><strong>Statistical Integration:</strong> Seamless p-value annotations and error bars</li>",
                "</ul>",
                
                "<h4 style='color: #2e7d32;'>Clinical Research Applications:</h4>",
                "<ul>",
                "<li><strong>Biostatistics:</strong> Professional visualization for clinical data analysis</li>",
                "<li><strong>Publication Figures:</strong> High-quality graphics for manuscripts and journals</li>",
                "<li><strong>Presentations:</strong> Clear, impactful visuals for academic conferences</li>",
                "<li><strong>Data Communication:</strong> Effective statistical storytelling with visual appeal</li>",
                "</ul>",
                
                "<h4 style='color: #2e7d32;'>Plot Type Specific Information:</h4>",
                switch(plot_type,
                    "violin" = "<p><strong>Violin Plots:</strong> Show full distribution shape with density curves, ideal for revealing multimodality and distribution characteristics.</p>",
                    "boxplot" = "<p><strong>Box Plots:</strong> Display quartiles, median, and outliers with classic Prism formatting for clear statistical summaries.</p>",
                    "scatter" = "<p><strong>Scatter Plots:</strong> Reveal relationships between continuous variables with professional Prism styling.</p>",
                    "column" = "<p><strong>Column Plots:</strong> Present mean values with error bars using authentic Prism column chart aesthetics.</p>",
                    "line" = "<p><strong>Line Plots:</strong> Track changes over time or ordered categories with Prism line styling.</p>"
                ),
                
                "<p style='font-size: 12px; color: #2e7d32; margin-top: 15px;'>",
                "<em>🎨 Experience the professional quality of GraphPad Prism styling within R's powerful analysis environment</em>",
                "</p></div>"
            )
            
            return(interpretation_html)
        }

    )
)

# Store analysis data for plotting
.analysis_data <- NULL