#' @title Scientific Visualization with Grafify
#' @importFrom R6 R6Class
#' @import jmvcore
#' @importFrom grafify plot_scatterbar
#' @importFrom grafify plot_scatterbox
#' @importFrom grafify plot_scatterviolin
#' @importFrom grafify plot_dotbar
#' @importFrom grafify plot_dotbox
#' @importFrom grafify plot_dotviolin
#' @importFrom grafify plot_befafter_box
#' @importFrom grafify plot_befafter_colors
#' @importFrom grafify plot_3d_scatterbar
#' @importFrom grafify plot_4d_scatterbar
#' @importFrom grafify plot_density
#' @importFrom grafify plot_histogram
#' @importFrom grafify plot_xy_CatGroup
#' @importFrom grafify plot_xy_NumGroup
#' @importFrom grafify graf_palettes
#' @importFrom grafify theme_grafify
#' @importFrom grafify scale_fill_grafify
#' @importFrom grafify scale_color_grafify
#' @importFrom grafify posthoc_Pairwise
#' @importFrom grafify posthoc_Levelwise
#' @importFrom grafify posthoc_vsRef
#' @importFrom grafify posthoc_Trends_Levelwise
#' @importFrom dplyr select
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @importFrom dplyr mutate
#' @importFrom dplyr filter
#' @importFrom dplyr across
#' @importFrom dplyr everything
#' @importFrom stats aov
#' @importFrom stats lm
#' @importFrom stats t.test
#' @importFrom stats cor.test
#' @importFrom stats shapiro.test
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 ggsave
#' @export

grafifyClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "grafifyClass",
    inherit = grafifyBase,
    private = list(
        .results_cache = NULL,
        .processed_data = NULL,
        
        .init = function() {
            # Initialize visibility based on user options
            if (length(self$options$vars) == 0 && is.null(self$options$x_var) && is.null(self$options$y_var)) {
                self$results$main_plot$setVisible(FALSE)
                self$results$summary_stats$setVisible(FALSE)
                self$results$statistical_analysis$setVisible(FALSE)
                self$results$posthoc_results$setVisible(FALSE)
                self$results$diagnostic_plots$setVisible(FALSE)
                self$results$qqplot$setVisible(FALSE)
                self$results$export_info$setVisible(FALSE)
                self$results$todo$setVisible(TRUE)
            } else {
                self$results$todo$setVisible(FALSE)
            }
            
            # Set plot dimensions
            plot_width <- self$options$plot_width * 100
            plot_height <- self$options$plot_height * 100
            self$results$main_plot$setSize(plot_width, plot_height)
        },
        
        .run = function() {
            # Early exit with instructions if no variables selected
            if (length(self$options$vars) == 0 && is.null(self$options$x_var) && is.null(self$options$y_var)) {
                welcome_html <- private$.create_welcome_message()
                self$results$todo$setContent(welcome_html)
                return()
            }
            
            # Check for grafify package
            if (!requireNamespace("grafify", quietly = TRUE)) {
                error_msg <- paste(
                    "<div style='color: red; padding: 10px;'>",
                    "<strong>Error:</strong> The 'grafify' package is required for scientific visualization.",
                    "<br>Please install it using: install.packages('grafify')",
                    "</div>"
                )
                self$results$main_plot$setContent(error_msg)
                return()
            }
            
            # Validate data
            if (nrow(self$data) == 0) {
                stop("Data contains no (complete) rows")
            }
            
            # Process and validate data
            private$.processed_data <- private$.process_data(self$data)
            
            # Generate summary statistics
            if (self$options$show_summary_stats) {
                summary_html <- private$.create_summary_stats()
                self$results$summary_stats$setContent(summary_html)
            }
            
            # Perform statistical analysis
            if (self$options$add_statistics) {
                stat_results <- private$.perform_statistical_analysis()
                private$.generate_stat_outputs(stat_results)
            }
            
            # Generate interpretation guide
            interpretation_html <- private$.create_interpretation_guide()
            self$results$plot_interpretation$setContent(interpretation_html)
            
            # Generate export information
            if (self$options$export_data) {
                export_html <- private$.create_export_info()
                self$results$export_info$setContent(export_html)
            }
            
            # Prepare plot data for rendering
            plot_data <- private$.prepare_plot_data()
            self$results$main_plot$setState(plot_data)
        },
        
        .create_welcome_message = function() {
            paste(
                "<div style='background-color: #f8f9fa; padding: 20px; border-radius: 8px; margin: 10px;'>",
                "<h3 style='color: #2c3e50; margin-top: 0;'>Scientific Visualization with Grafify</h3>",
                "<p><strong>Create beautiful, color-blind friendly scientific plots with advanced statistical integration.</strong></p>",
                "<h4>Getting Started:</h4>",
                "<ol>",
                "<li><strong>Choose Variables:</strong> Select variables for plotting or specify X/Y variables</li>",
                "<li><strong>Select Plot Type:</strong> Choose from scatter plots, box plots, violin plots, and more</li>",
                "<li><strong>Configure Groups:</strong> Add grouping variables for categorical comparisons</li>",
                "<li><strong>Customize Appearance:</strong> Select color-blind friendly palettes and styling</li>",
                "<li><strong>Add Statistics:</strong> Enable statistical analysis and post-hoc comparisons</li>",
                "</ol>",
                "<h4>Key Features:</h4>",
                "<ul>",
                "<li>🎨 <strong>Color-Blind Friendly:</strong> 12 carefully designed color palettes</li>",
                "<li>📊 <strong>Scientific Plot Types:</strong> Specialized plots for experimental data</li>",
                "<li>📈 <strong>Integrated Statistics:</strong> ANOVA, t-tests, correlations with post-hoc tests</li>",
                "<li>🔬 <strong>Experimental Designs:</strong> Support for randomized blocks, repeated measures, factorials</li>",
                "<li>📋 <strong>Before-After Analysis:</strong> Paired comparisons and longitudinal data</li>",
                "<li>🎯 <strong>Publication Ready:</strong> Professional styling for scientific journals</li>",
                "</ul>",
                "<p><em>Select your variables to begin creating professional scientific visualizations.</em></p>",
                "</div>"
            )
        },
        
        .process_data = function(raw_data) {
            # Clean and prepare data
            processed_data <- raw_data
            
            # Clean variable names
            if (requireNamespace("janitor", quietly = TRUE)) {
                processed_data <- janitor::clean_names(processed_data)
            }
            
            # Validate and select variables based on plot requirements
            plot_vars <- c()
            
            if (!is.null(self$options$x_var)) {
                plot_vars <- c(plot_vars, self$options$x_var)
            }
            if (!is.null(self$options$y_var)) {
                plot_vars <- c(plot_vars, self$options$y_var)
            }
            if (length(self$options$vars) > 0) {
                plot_vars <- c(plot_vars, self$options$vars)
            }
            if (!is.null(self$options$groups)) {
                plot_vars <- c(plot_vars, self$options$groups)
            }
            if (!is.null(self$options$blocks)) {
                plot_vars <- c(plot_vars, self$options$blocks)
            }
            if (!is.null(self$options$facet_var)) {
                plot_vars <- c(plot_vars, self$options$facet_var)
            }
            if (!is.null(self$options$befafter_shape_var)) {
                plot_vars <- c(plot_vars, self$options$befafter_shape_var)
            }
            if (!is.null(self$options$befafter_id_var)) {
                plot_vars <- c(plot_vars, self$options$befafter_id_var)
            }
            if (length(self$options$random_effects) > 0) {
                plot_vars <- c(plot_vars, self$options$random_effects)
            }
            
            # Remove duplicates and filter data
            plot_vars <- unique(plot_vars)
            if (length(plot_vars) > 0) {
                processed_data <- processed_data[, plot_vars, drop = FALSE]
            }
            
            # Remove missing values for selected variables
            processed_data <- processed_data[complete.cases(processed_data), ]
            
            if (nrow(processed_data) == 0) {
                stop("No complete cases found for selected variables")
            }
            
            return(processed_data)
        },
        
        .create_summary_stats = function() {
            data <- private$.processed_data
            
            # Generate summary statistics based on plot configuration
            summary_html <- "<div style='padding: 15px;'>"
            summary_html <- paste(summary_html, "<h4>Summary Statistics</h4>")
            
            # Overall data summary
            summary_html <- paste(summary_html,
                "<p><strong>Dataset:</strong>", nrow(data), "observations,", ncol(data), "variables</p>")
            
            # Variable-specific summaries
            if (!is.null(self$options$x_var) && !is.null(self$options$y_var)) {
                x_var <- self$options$x_var
                y_var <- self$options$y_var
                
                if (is.numeric(data[[x_var]])) {
                    x_stats <- private$.get_numeric_summary(data[[x_var]], x_var)
                    summary_html <- paste(summary_html, x_stats)
                }
                
                if (is.numeric(data[[y_var]])) {
                    y_stats <- private$.get_numeric_summary(data[[y_var]], y_var)
                    summary_html <- paste(summary_html, y_stats)
                }
                
                # Group-wise summaries if grouping variable specified
                if (!is.null(self$options$groups)) {
                    group_stats <- private$.get_group_summary(data, y_var, self$options$groups)
                    summary_html <- paste(summary_html, group_stats)
                }
            }
            
            summary_html <- paste(summary_html, "</div>")
            return(summary_html)
        },
        
        .get_numeric_summary = function(x, var_name) {
            if (!is.numeric(x)) return("")
            
            summary_func <- switch(self$options$summary_function,
                "mean" = mean,
                "median" = median,
                "geomean" = function(x) exp(mean(log(x[x > 0]), na.rm = TRUE))
            )
            
            central_val <- summary_func(x, na.rm = TRUE)
            sd_val <- sd(x, na.rm = TRUE)
            n_val <- length(x[!is.na(x)])
            
            paste(
                "<p><strong>", var_name, ":</strong>",
                sprintf("%.3f ± %.3f", central_val, sd_val),
                "(n =", n_val, ")</p>"
            )
        },
        
        .get_group_summary = function(data, y_var, group_var) {
            if (!group_var %in% names(data) || !y_var %in% names(data)) {
                return("")
            }
            
            group_summary <- data %>%
                dplyr::group_by(!!rlang::sym(group_var)) %>%
                dplyr::summarise(
                    n = dplyr::n(),
                    mean = mean(!!rlang::sym(y_var), na.rm = TRUE),
                    sd = sd(!!rlang::sym(y_var), na.rm = TRUE),
                    median = median(!!rlang::sym(y_var), na.rm = TRUE),
                    .groups = 'drop'
                )
            
            html <- "<h5>Group-wise Summary:</h5><table style='border-collapse: collapse; width: 100%;'>"
            html <- paste(html, "<tr><th style='border: 1px solid #ddd; padding: 8px;'>Group</th>")
            html <- paste(html, "<th style='border: 1px solid #ddd; padding: 8px;'>N</th>")
            html <- paste(html, "<th style='border: 1px solid #ddd; padding: 8px;'>Mean ± SD</th>")
            html <- paste(html, "<th style='border: 1px solid #ddd; padding: 8px;'>Median</th></tr>")
            
            for (i in 1:nrow(group_summary)) {
                html <- paste(html, 
                    "<tr><td style='border: 1px solid #ddd; padding: 8px;'>", group_summary[[group_var]][i], "</td>",
                    "<td style='border: 1px solid #ddd; padding: 8px;'>", group_summary$n[i], "</td>",
                    "<td style='border: 1px solid #ddd; padding: 8px;'>", 
                    sprintf("%.3f ± %.3f", group_summary$mean[i], group_summary$sd[i]), "</td>",
                    "<td style='border: 1px solid #ddd; padding: 8px;'>", 
                    sprintf("%.3f", group_summary$median[i]), "</td></tr>"
                )
            }
            
            html <- paste(html, "</table>")
            return(html)
        },
        
        .perform_statistical_analysis = function() {
            data <- private$.processed_data
            
            if (is.null(self$options$x_var) || is.null(self$options$y_var)) {
                return(list(error = "X and Y variables required for statistical analysis"))
            }
            
            x_var <- self$options$x_var
            y_var <- self$options$y_var
            group_var <- self$options$groups
            
            results <- list()
            
            tryCatch({
                switch(self$options$stat_method,
                    "anova_1way" = {
                        if (!is.null(group_var)) {
                            formula_str <- paste(y_var, "~", group_var)
                            aov_result <- aov(as.formula(formula_str), data = data)
                            results$anova <- summary(aov_result)
                            results$model <- aov_result
                        }
                    },
                    "anova_2way" = {
                        if (!is.null(group_var) && !is.null(self$options$blocks)) {
                            formula_str <- paste(y_var, "~", group_var, "*", self$options$blocks)
                            aov_result <- aov(as.formula(formula_str), data = data)
                            results$anova <- summary(aov_result)
                            results$model <- aov_result
                        }
                    },
                    "ttest" = {
                        if (!is.null(group_var)) {
                            groups <- unique(data[[group_var]])
                            if (length(groups) == 2) {
                                group1_data <- data[data[[group_var]] == groups[1], y_var]
                                group2_data <- data[data[[group_var]] == groups[2], y_var]
                                results$ttest <- t.test(group1_data, group2_data)
                            }
                        }
                    },
                    "correlation" = {
                        if (is.numeric(data[[x_var]]) && is.numeric(data[[y_var]])) {
                            results$correlation <- cor.test(data[[x_var]], data[[y_var]])
                        }
                    }
                )
                
                # Normality tests
                if (is.numeric(data[[y_var]])) {
                    if (nrow(data) <= 5000) {  # Shapiro-Wilk limitation
                        results$normality <- shapiro.test(data[[y_var]])
                    }
                }
                
                results$success <- TRUE
                
            }, error = function(e) {
                results$error <- e$message
                results$success <- FALSE
            })
            
            return(results)
        },
        
        .generate_stat_outputs = function(stat_results) {
            if (!stat_results$success) {
                error_html <- paste(
                    "<div style='color: red; padding: 10px;'>",
                    "<strong>Statistical Analysis Error:</strong>", stat_results$error,
                    "</div>"
                )
                self$results$statistical_analysis$setContent(error_html)
                return()
            }
            
            # Main statistical results
            stat_html <- private$.format_statistical_results(stat_results)
            self$results$statistical_analysis$setContent(stat_html)
            
            # Post-hoc comparisons
            if (self$options$posthoc_comparisons && !is.null(stat_results$model)) {
                posthoc_html <- private$.perform_posthoc_analysis(stat_results$model)
                self$results$posthoc_results$setContent(posthoc_html)
            }
        },
        
        .format_statistical_results = function(results) {
            html <- "<div style='padding: 15px;'>"
            html <- paste(html, "<h4>Statistical Analysis Results</h4>")
            
            # ANOVA results
            if (!is.null(results$anova)) {
                html <- paste(html, "<h5>Analysis of Variance:</h5>")
                anova_table <- results$anova[[1]]
                html <- paste(html, "<pre>", paste(capture.output(print(anova_table)), collapse = "\n"), "</pre>")
            }
            
            # t-test results
            if (!is.null(results$ttest)) {
                html <- paste(html, "<h5>t-test Results:</h5>")
                html <- paste(html, "<p><strong>t-statistic:</strong>", round(results$ttest$statistic, 4), "</p>")
                html <- paste(html, "<p><strong>p-value:</strong>", format.pval(results$ttest$p.value), "</p>")
                html <- paste(html, "<p><strong>95% CI:</strong>", 
                             paste(round(results$ttest$conf.int, 4), collapse = " to "), "</p>")
            }
            
            # Correlation results
            if (!is.null(results$correlation)) {
                html <- paste(html, "<h5>Correlation Analysis:</h5>")
                html <- paste(html, "<p><strong>Correlation coefficient (r):</strong>", 
                             round(results$correlation$estimate, 4), "</p>")
                html <- paste(html, "<p><strong>p-value:</strong>", format.pval(results$correlation$p.value), "</p>")
                html <- paste(html, "<p><strong>95% CI:</strong>", 
                             paste(round(results$correlation$conf.int, 4), collapse = " to "), "</p>")
            }
            
            # Normality test results
            if (!is.null(results$normality)) {
                html <- paste(html, "<h5>Normality Test (Shapiro-Wilk):</h5>")
                html <- paste(html, "<p><strong>W-statistic:</strong>", round(results$normality$statistic, 4), "</p>")
                html <- paste(html, "<p><strong>p-value:</strong>", format.pval(results$normality$p.value), "</p>")
                
                if (results$normality$p.value < 0.05) {
                    html <- paste(html, "<p style='color: orange;'><em>Note: Data may not be normally distributed (p < 0.05)</em></p>")
                }
            }
            
            html <- paste(html, "</div>")
            return(html)
        },
        
        .perform_posthoc_analysis = function(model) {
            if (!requireNamespace("grafify", quietly = TRUE)) {
                return("<p>grafify package required for post-hoc analysis.</p>")
            }
            
            html <- "<div style='padding: 15px;'>"
            html <- paste(html, "<h4>Post-hoc Comparisons</h4>")
            
            tryCatch({
                posthoc_results <- switch(self$options$comparison_method,
                    "pairwise" = grafify::posthoc_Pairwise(model),
                    "vs_ref" = grafify::posthoc_vsRef(model),
                    "trends" = grafify::posthoc_Trends_Levelwise(model),
                    "levelwise" = grafify::posthoc_Levelwise(model)
                )
                
                html <- paste(html, "<pre>", paste(capture.output(print(posthoc_results)), collapse = "\n"), "</pre>")
                
            }, error = function(e) {
                html <- paste(html, "<p style='color: red;'>Error in post-hoc analysis:", e$message, "</p>")
            })
            
            html <- paste(html, "</div>")
            return(html)
        },
        
        .create_interpretation_guide = function() {
            plot_type <- self$options$plot_type
            experimental_design <- self$options$experimental_design
            
            html <- "<div style='background-color: #e8f5e8; padding: 15px; border-radius: 5px; margin: 10px;'>"
            html <- paste(html, "<h4>Plot Interpretation Guide</h4>")
            
            # Plot type specific guidance
            plot_guidance <- switch(plot_type,
                "scatterbar" = "Shows individual data points with error bars representing variability. Ideal for comparing group means while showing data distribution.",
                "scatterbox" = "Combines scatter plot with box plots showing median, quartiles, and outliers. Good for comparing distributions between groups.",
                "scatterviolin" = "Shows data density distribution along with individual points. Excellent for visualizing data shape and identifying multimodal distributions.",
                "dotbar" = "Simplified dot plot with error bars, focusing on summary statistics. Clean presentation for publication figures.",
                "befafter_box" = "Specialized for before-after comparisons with paired data. Shows changes over time or treatment.",
                "befafter_colors" = "Before-after visualization using colors to distinguish timepoints. Emphasizes individual subject changes.",
                "scatter_3d" = "Three-dimensional visualization with X, Y variables and grouping. Useful for exploring complex relationships.",
                "density" = "Shows probability density of continuous variables. Ideal for distribution comparisons and normality assessment.",
                "histogram" = "Frequency distribution of continuous variables. Useful for exploring data distribution and identifying patterns.",
                "Default plot interpretation available."
            )
            
            html <- paste(html, "<p><strong>Plot Type:</strong>", plot_guidance, "</p>")
            
            # Experimental design guidance
            design_guidance <- switch(experimental_design,
                "crd" = "Completely randomized design assumes independence between observations. Use for simple group comparisons.",
                "rbd" = "Randomized block design controls for blocking factors. Reduces experimental error by accounting for known sources of variation.",
                "repeated" = "Repeated measures design for longitudinal data. Accounts for correlation within subjects over time.",
                "factorial" = "Factorial design examines main effects and interactions between multiple factors.",
                "before_after" = "Paired comparison design for pre-post studies. Controls for individual differences.",
                "Consider your experimental design when interpreting statistical results."
            )
            
            html <- paste(html, "<p><strong>Experimental Design:</strong>", design_guidance, "</p>")
            
            # Color palette information
            palette_info <- paste(
                "<p><strong>Color Palette:</strong> All grafify palettes are designed to be color-blind friendly",
                "and suitable for scientific publication. The", self$options$color_palette, 
                "palette provides optimal contrast for your visualization.</p>"
            )
            html <- paste(html, palette_info)
            
            html <- paste(html, "</div>")
            return(html)
        },
        
        .create_export_info = function() {
            html <- "<div style='background-color: #fff3cd; padding: 15px; border-radius: 5px; margin: 10px;'>"
            html <- paste(html, "<h4>Data Export Information</h4>")
            html <- paste(html, "<p>Processed data and summary statistics are available for export.</p>")
            html <- paste(html, "<ul>")
            html <- paste(html, "<li><strong>Plot Data:</strong> Processed variables used for visualization</li>")
            html <- paste(html, "<li><strong>Summary Statistics:</strong> Group means, standard deviations, and sample sizes</li>")
            if (self$options$add_statistics) {
                html <- paste(html, "<li><strong>Statistical Results:</strong> ANOVA tables, test statistics, and p-values</li>")
            }
            if (self$options$posthoc_comparisons) {
                html <- paste(html, "<li><strong>Post-hoc Comparisons:</strong> Pairwise comparison results</li>")
            }
            html <- paste(html, "</ul>")
            html <- paste(html, "</div>")
            return(html)
        },
        
        .prepare_plot_data = function() {
            data <- private$.processed_data
            
            plot_data <- list(
                data = data,
                plot_type = self$options$plot_type,
                x_var = self$options$x_var,
                y_var = self$options$y_var,
                groups = self$options$groups,
                blocks = self$options$blocks,
                facet_var = self$options$facet_var,
                color_palette = self$options$color_palette,
                reverse_palette = self$options$reverse_palette,
                use_grafify_theme = self$options$use_grafify_theme,
                error_type = self$options$error_type,
                summary_function = self$options$summary_function,
                jitter_width = self$options$jitter_width,
                transparency = self$options$transparency,
                point_size = self$options$point_size,
                line_size = self$options$line_size,
                log_transform = self$options$log_transform,
                show_individual_points = self$options$show_individual_points,
                title_text = self$options$title_text,
                x_label = self$options$x_label,
                y_label = self$options$y_label,
                legend_position = self$options$legend_position,
                befafter_shape_var = self$options$befafter_shape_var,
                befafter_id_var = self$options$befafter_id_var
            )
            
            return(plot_data)
        },
        
        # Main plot rendering function
        .plot_main = function(image, ggtheme, theme, ...) {
            plot_data <- image$state
            if (is.null(plot_data) || is.null(plot_data$data)) {
                return()
            }
            
            if (!requireNamespace("grafify", quietly = TRUE)) {
                stop("grafify package is required for plotting")
            }
            
            # Generate the plot based on type
            plot <- private$.create_grafify_plot(plot_data)
            
            if (is.null(plot)) {
                return()
            }
            
            print(plot)
            TRUE
        },
        
        .create_grafify_plot = function(plot_data) {
            data <- plot_data$data
            plot_type <- plot_data$plot_type
            
            tryCatch({
                # Validate required variables for plot type
                if (is.null(plot_data$x_var) || is.null(plot_data$y_var)) {
                    if (!plot_type %in% c("density", "histogram")) {
                        stop("X and Y variables required for this plot type")
                    }
                }
                
                # Create the appropriate grafify plot
                plot <- switch(plot_type,
                    "scatterbar" = private$.create_scatterbar_plot(data, plot_data),
                    "scatterbox" = private$.create_scatterbox_plot(data, plot_data),
                    "scatterviolin" = private$.create_scatterviolin_plot(data, plot_data),
                    "dotbar" = private$.create_dotbar_plot(data, plot_data),
                    "dotbox" = private$.create_dotbox_plot(data, plot_data),
                    "dotviolin" = private$.create_dotviolin_plot(data, plot_data),
                    "befafter_box" = private$.create_befafter_box_plot(data, plot_data),
                    "befafter_colors" = private$.create_befafter_colors_plot(data, plot_data),
                    "scatter_3d" = private$.create_3d_scatter_plot(data, plot_data),
                    "scatter_4d" = private$.create_4d_scatter_plot(data, plot_data),
                    "density" = private$.create_density_plot(data, plot_data),
                    "histogram" = private$.create_histogram_plot(data, plot_data),
                    "xy_catgroup" = private$.create_xy_catgroup_plot(data, plot_data),
                    "xy_numgroup" = private$.create_xy_numgroup_plot(data, plot_data),
                    stop("Unknown plot type")
                )
                
                # Apply common styling and transformations
                plot <- private$.apply_common_styling(plot, plot_data)
                
                return(plot)
                
            }, error = function(e) {
                # Create error plot
                error_plot <- ggplot2::ggplot() + 
                    ggplot2::geom_text(
                        ggplot2::aes(x = 0.5, y = 0.5, 
                                   label = paste("Error creating plot:\n", e$message)),
                        size = 5, color = "red"
                    ) +
                    ggplot2::xlim(0, 1) + ggplot2::ylim(0, 1) +
                    ggplot2::theme_void()
                
                return(error_plot)
            })
        },
        
        .create_scatterbar_plot = function(data, plot_data) {
            x_var <- plot_data$x_var
            y_var <- plot_data$y_var
            group_var <- plot_data$groups
            
            if (!is.null(group_var)) {
                plot <- grafify::plot_scatterbar(
                    data = data,
                    xcol = x_var,
                    ycol = y_var,
                    facet = group_var,
                    ColPal = plot_data$color_palette,
                    ColSeq = !plot_data$reverse_palette,
                    alpha = plot_data$transparency,
                    size = plot_data$point_size,
                    width = plot_data$jitter_width,
                    ErrorType = plot_data$error_type,
                    symm = plot_data$summary_function == "mean"
                )
            } else {
                # Single group scatterbar
                plot <- ggplot2::ggplot(data, ggplot2::aes_string(x = x_var, y = y_var)) +
                    ggplot2::geom_point(alpha = plot_data$transparency, size = plot_data$point_size)
            }
            
            return(plot)
        },
        
        .create_scatterbox_plot = function(data, plot_data) {
            x_var <- plot_data$x_var
            y_var <- plot_data$y_var
            group_var <- plot_data$groups
            
            if (!is.null(group_var)) {
                plot <- grafify::plot_scatterbox(
                    data = data,
                    xcol = x_var,
                    ycol = y_var,
                    facet = group_var,
                    ColPal = plot_data$color_palette,
                    ColSeq = !plot_data$reverse_palette,
                    alpha = plot_data$transparency,
                    size = plot_data$point_size,
                    width = plot_data$jitter_width
                )
            } else {
                plot <- ggplot2::ggplot(data, ggplot2::aes_string(x = x_var, y = y_var)) +
                    ggplot2::geom_boxplot() +
                    ggplot2::geom_point(alpha = plot_data$transparency, size = plot_data$point_size)
            }
            
            return(plot)
        },
        
        .create_scatterviolin_plot = function(data, plot_data) {
            x_var <- plot_data$x_var
            y_var <- plot_data$y_var
            group_var <- plot_data$groups
            
            if (!is.null(group_var)) {
                plot <- grafify::plot_scatterviolin(
                    data = data,
                    xcol = x_var,
                    ycol = y_var,
                    facet = group_var,
                    ColPal = plot_data$color_palette,
                    ColSeq = !plot_data$reverse_palette,
                    alpha = plot_data$transparency,
                    size = plot_data$point_size,
                    width = plot_data$jitter_width
                )
            } else {
                plot <- ggplot2::ggplot(data, ggplot2::aes_string(x = x_var, y = y_var)) +
                    ggplot2::geom_violin() +
                    ggplot2::geom_point(alpha = plot_data$transparency, size = plot_data$point_size)
            }
            
            return(plot)
        },
        
        .create_dotbar_plot = function(data, plot_data) {
            x_var <- plot_data$x_var
            y_var <- plot_data$y_var
            group_var <- plot_data$groups
            
            if (!is.null(group_var)) {
                plot <- grafify::plot_dotbar(
                    data = data,
                    xcol = x_var,
                    ycol = y_var,
                    facet = group_var,
                    ColPal = plot_data$color_palette,
                    ColSeq = !plot_data$reverse_palette,
                    size = plot_data$point_size,
                    ErrorType = plot_data$error_type,
                    symm = plot_data$summary_function == "mean"
                )
            } else {
                # Create summary dot plot manually
                plot <- ggplot2::ggplot(data, ggplot2::aes_string(x = x_var, y = y_var)) +
                    ggplot2::stat_summary(fun = mean, geom = "point", size = plot_data$point_size)
            }
            
            return(plot)
        },
        
        .create_dotbox_plot = function(data, plot_data) {
            x_var <- plot_data$x_var
            y_var <- plot_data$y_var
            group_var <- plot_data$groups
            
            if (!is.null(group_var)) {
                plot <- grafify::plot_dotbox(
                    data = data,
                    xcol = x_var,
                    ycol = y_var,
                    facet = group_var,
                    ColPal = plot_data$color_palette,
                    ColSeq = !plot_data$reverse_palette,
                    size = plot_data$point_size
                )
            } else {
                plot <- ggplot2::ggplot(data, ggplot2::aes_string(x = x_var, y = y_var)) +
                    ggplot2::geom_boxplot() +
                    ggplot2::stat_summary(fun = mean, geom = "point", size = plot_data$point_size)
            }
            
            return(plot)
        },
        
        .create_dotviolin_plot = function(data, plot_data) {
            x_var <- plot_data$x_var
            y_var <- plot_data$y_var
            group_var <- plot_data$groups
            
            if (!is.null(group_var)) {
                plot <- grafify::plot_dotviolin(
                    data = data,
                    xcol = x_var,
                    ycol = y_var,
                    facet = group_var,
                    ColPal = plot_data$color_palette,
                    ColSeq = !plot_data$reverse_palette,
                    size = plot_data$point_size
                )
            } else {
                plot <- ggplot2::ggplot(data, ggplot2::aes_string(x = x_var, y = y_var)) +
                    ggplot2::geom_violin() +
                    ggplot2::stat_summary(fun = mean, geom = "point", size = plot_data$point_size)
            }
            
            return(plot)
        },
        
        .create_befafter_box_plot = function(data, plot_data) {
            if (is.null(plot_data$befafter_shape_var) || is.null(plot_data$befafter_id_var)) {
                stop("Before-after plots require shape and ID variables")
            }
            
            plot <- grafify::plot_befafter_box(
                data = data,
                xcol = plot_data$befafter_shape_var,
                ycol = plot_data$y_var,
                match = plot_data$befafter_id_var,
                facet = plot_data$groups,
                ColPal = plot_data$color_palette,
                ColSeq = !plot_data$reverse_palette
            )
            
            return(plot)
        },
        
        .create_befafter_colors_plot = function(data, plot_data) {
            if (is.null(plot_data$befafter_shape_var) || is.null(plot_data$befafter_id_var)) {
                stop("Before-after plots require shape and ID variables")
            }
            
            plot <- grafify::plot_befafter_colors(
                data = data,
                xcol = plot_data$befafter_shape_var,
                ycol = plot_data$y_var,
                match = plot_data$befafter_id_var,
                facet = plot_data$groups,
                ColPal = plot_data$color_palette,
                ColSeq = !plot_data$reverse_palette
            )
            
            return(plot)
        },
        
        .create_3d_scatter_plot = function(data, plot_data) {
            if (is.null(plot_data$groups)) {
                stop("3D scatter plot requires grouping variable")
            }
            
            plot <- grafify::plot_3d_scatterbar(
                data = data,
                xcol = plot_data$x_var,
                ycol = plot_data$y_var,
                zcol = plot_data$groups,
                ColPal = plot_data$color_palette,
                ColSeq = !plot_data$reverse_palette,
                alpha = plot_data$transparency,
                size = plot_data$point_size
            )
            
            return(plot)
        },
        
        .create_4d_scatter_plot = function(data, plot_data) {
            if (is.null(plot_data$groups) || is.null(plot_data$facet_var)) {
                stop("4D scatter plot requires grouping and faceting variables")
            }
            
            plot <- grafify::plot_4d_scatterbar(
                data = data,
                xcol = plot_data$x_var,
                ycol = plot_data$y_var,
                zcol = plot_data$groups,
                facet = plot_data$facet_var,
                ColPal = plot_data$color_palette,
                ColSeq = !plot_data$reverse_palette,
                alpha = plot_data$transparency,
                size = plot_data$point_size
            )
            
            return(plot)
        },
        
        .create_density_plot = function(data, plot_data) {
            var_name <- plot_data$y_var %||% plot_data$x_var %||% plot_data$vars[1]
            
            if (is.null(var_name)) {
                stop("Variable required for density plot")
            }
            
            plot <- grafify::plot_density(
                data = data,
                xcol = var_name,
                facet = plot_data$groups,
                ColPal = plot_data$color_palette,
                ColSeq = !plot_data$reverse_palette
            )
            
            return(plot)
        },
        
        .create_histogram_plot = function(data, plot_data) {
            var_name <- plot_data$y_var %||% plot_data$x_var %||% plot_data$vars[1]
            
            if (is.null(var_name)) {
                stop("Variable required for histogram")
            }
            
            plot <- grafify::plot_histogram(
                data = data,
                xcol = var_name,
                facet = plot_data$groups,
                ColPal = plot_data$color_palette,
                ColSeq = !plot_data$reverse_palette
            )
            
            return(plot)
        },
        
        .create_xy_catgroup_plot = function(data, plot_data) {
            plot <- grafify::plot_xy_CatGroup(
                data = data,
                xcol = plot_data$x_var,
                ycol = plot_data$y_var,
                CatGroup = plot_data$groups,
                ColPal = plot_data$color_palette,
                ColSeq = !plot_data$reverse_palette,
                alpha = plot_data$transparency,
                size = plot_data$point_size
            )
            
            return(plot)
        },
        
        .create_xy_numgroup_plot = function(data, plot_data) {
            plot <- grafify::plot_xy_NumGroup(
                data = data,
                xcol = plot_data$x_var,
                ycol = plot_data$y_var,
                NumGroup = plot_data$groups,
                ColPal = plot_data$color_palette,
                ColSeq = !plot_data$reverse_palette,
                alpha = plot_data$transparency,
                size = plot_data$point_size
            )
            
            return(plot)
        },
        
        .apply_common_styling = function(plot, plot_data) {
            # Apply grafify theme if requested
            if (plot_data$use_grafify_theme) {
                plot <- plot + grafify::theme_grafify()
            }
            
            # Apply log transformations
            if (plot_data$log_transform != "none") {
                if (plot_data$log_transform %in% c("log10_y", "log10_both")) {
                    plot <- plot + ggplot2::scale_y_log10()
                }
                if (plot_data$log_transform %in% c("log10_x", "log10_both")) {
                    plot <- plot + ggplot2::scale_x_log10()
                }
                if (plot_data$log_transform %in% c("ln_y", "ln_both")) {
                    plot <- plot + ggplot2::scale_y_continuous(trans = "log")
                }
                if (plot_data$log_transform %in% c("ln_x", "ln_both")) {
                    plot <- plot + ggplot2::scale_x_continuous(trans = "log")
                }
            }
            
            # Apply custom labels
            if (plot_data$title_text != "") {
                plot <- plot + ggplot2::ggtitle(plot_data$title_text)
            }
            if (plot_data$x_label != "") {
                plot <- plot + ggplot2::xlab(plot_data$x_label)
            }
            if (plot_data$y_label != "") {
                plot <- plot + ggplot2::ylab(plot_data$y_label)
            }
            
            # Set legend position
            if (plot_data$legend_position == "none") {
                plot <- plot + ggplot2::theme(legend.position = "none")
            } else {
                plot <- plot + ggplot2::theme(legend.position = plot_data$legend_position)
            }
            
            return(plot)
        },
        
        # Palette preview plot
        .plot_palette = function(image, ggtheme, theme, ...) {
            if (!requireNamespace("grafify", quietly = TRUE)) {
                return()
            }
            
            # Create palette preview
            palette_name <- self$options$color_palette
            reverse_pal <- self$options$reverse_palette
            
            # Get the color palette
            colors <- grafify::graf_palettes(palette_name, n = 8, direction = ifelse(reverse_pal, -1, 1))
            
            # Create a simple bar plot to show colors
            preview_data <- data.frame(
                x = 1:length(colors),
                y = rep(1, length(colors)),
                color = colors
            )
            
            plot <- ggplot2::ggplot(preview_data, ggplot2::aes(x = x, y = y, fill = color)) +
                ggplot2::geom_col() +
                ggplot2::scale_fill_identity() +
                ggplot2::labs(title = paste("Color Palette:", palette_name), 
                             subtitle = ifelse(reverse_pal, "Reversed", "Normal order")) +
                ggplot2::theme_void() +
                ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                              plot.subtitle = ggplot2::element_text(hjust = 0.5))
            
            print(plot)
            TRUE
        },
        
        # Diagnostic plots
        .plot_diagnostics = function(image, ggtheme, theme, ...) {
            if (!self$options$add_statistics) {
                return()
            }
            
            # Create diagnostic plots based on statistical method
            # This is a placeholder - would implement model diagnostics
            plot <- ggplot2::ggplot() + 
                ggplot2::geom_text(ggplot2::aes(x = 0.5, y = 0.5, 
                                               label = "Model Diagnostic Plots\n(Implementation in progress)"),
                                  size = 6) +
                ggplot2::xlim(0, 1) + ggplot2::ylim(0, 1) +
                ggplot2::theme_void()
            
            print(plot)
            TRUE
        },
        
        # Q-Q plot for normality
        .plot_qq = function(image, ggtheme, theme, ...) {
            if (!self$options$add_statistics) {
                return()
            }
            
            # Create Q-Q plot for normality assessment
            # This is a placeholder - would implement Q-Q plots
            plot <- ggplot2::ggplot() + 
                ggplot2::geom_text(ggplot2::aes(x = 0.5, y = 0.5, 
                                               label = "Q-Q Plot for Normality\n(Implementation in progress)"),
                                  size = 6) +
                ggplot2::xlim(0, 1) + ggplot2::ylim(0, 1) +
                ggplot2::theme_void()
            
            print(plot)
            TRUE
        }
    )
)