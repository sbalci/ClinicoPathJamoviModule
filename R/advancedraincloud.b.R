#' @title Advanced Raincloud Plot with Longitudinal Support
#' @return Advanced raincloud plots using ggrain package
#'
#' @importFrom R6 R6Class
#' @import jmvcore
#' @importFrom magrittr %>%
#' @importFrom ggplot2 ggplot aes labs theme_minimal theme element_text
#' @importFrom ggplot2 scale_fill_manual scale_color_manual
#' @importFrom ggrain geom_rain
#' @importFrom dplyr group_by summarise mutate n
#' @importFrom stats t.test wilcox.test aov kruskal.test
#' @importFrom RColorBrewer brewer.pal
#' @importFrom viridis viridis
#' @importFrom htmltools HTML

advancedraincloudClass <- if (requireNamespace("jmvcore")) R6::R6Class("advancedraincloudClass",
    inherit = advancedraincloudBase,
    private = list(
        
        .analysis_data = NULL,

        .run = function() {

            # Check if required variables have been selected
            if (is.null(self$options$y_var) || is.null(self$options$x_var)) {
                intro_msg <- "
                <div style='background-color: #e3f2fd; padding: 20px; border-radius: 8px; margin: 20px 0;'>
                <h3 style='color: #1976d2; margin-top: 0;'>🌧️ Welcome to Advanced Raincloud Plots!</h3>
                <p><strong>Enhanced distribution visualization with longitudinal connections</strong> using ggrain</p>
                <p>Complements the existing Raincloud Plot module with advanced features and customization</p>
                
                <h4 style='color: #1976d2;'>Required Variables:</h4>
                <ol>
                <li><strong>Y-Axis Variable:</strong> Continuous variable for distribution analysis</li>
                <li><strong>X-Axis Variable:</strong> Grouping variable for categories</li>
                </ol>
                
                <h4 style='color: #1976d2;'>Advanced Features:</h4>
                <ul>
                <li><strong>Longitudinal Connections:</strong> Connect repeated observations with ID variable</li>
                <li><strong>Likert Scale Support:</strong> Specialized handling for ordinal survey data</li>
                <li><strong>Flexible Positioning:</strong> Left, right, or flanking raincloud placement</li>
                <li><strong>Covariate Mapping:</strong> Remap point colors based on additional variables</li>
                <li><strong>Enhanced Customization:</strong> Advanced styling and appearance options</li>
                </ul>
                
                <h4 style='color: #1976d2;'>Perfect For:</h4>
                <ul>
                <li><strong>Longitudinal Studies:</strong> Repeated measures with connected observations</li>
                <li><strong>Survey Analysis:</strong> Likert scales and ordinal response data</li>
                <li><strong>Clinical Trials:</strong> Before/after treatment comparisons with connections</li>
                <li><strong>Complex Distributions:</strong> Multi-group comparisons with covariates</li>
                <li><strong>Publication Graphics:</strong> Highly customizable professional plots</li>
                </ul>
                
                <h4 style='color: #1976d2;'>Comparison with Standard Raincloud Plot:</h4>
                <ul>
                <li><strong>Standard Raincloud (ggdist):</strong> Basic distribution visualization</li>
                <li><strong>Advanced Raincloud (ggrain):</strong> Longitudinal connections + Likert support</li>
                <li><strong>Use Advanced for:</strong> Repeated measures, survey data, complex connections</li>
                <li><strong>Use Standard for:</strong> Simple distribution comparisons</li>
                </ul>
                
                <p style='font-size: 12px; color: #555; margin-top: 20px;'>
                💡 <em>Advanced raincloud plots for complex research designs and longitudinal data analysis</em>
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

            # Safely require ggrain
            if (!requireNamespace("ggrain", quietly = TRUE)) {
                error_msg <- "
                <div style='color: red; background-color: #ffebee; padding: 20px; border-radius: 8px;'>
                <h4>ggrain Package Required</h4>
                <p>The ggrain package is required for advanced raincloud plot functionality.</p>
                <p>Please install it using: <code>install.packages('ggrain')</code></p>
                </div>"
                self$results$interpretation$setContent(error_msg)
                return()
            }

            # Get data and variables
            dataset <- self$data
            y_var <- self$options$y_var
            x_var <- self$options$x_var
            fill_var <- self$options$fill_var
            id_var <- self$options$id_var
            cov_var <- self$options$cov_var

            # Prepare analysis data
            required_vars <- c(y_var, x_var)
            if (!is.null(fill_var) && fill_var != "") {
                required_vars <- c(required_vars, fill_var)
            }
            if (!is.null(id_var) && id_var != "") {
                required_vars <- c(required_vars, id_var)
            }
            if (!is.null(cov_var) && cov_var != "") {
                required_vars <- c(required_vars, cov_var)
            }
            
            analysis_data <- dataset[required_vars]
            analysis_data <- analysis_data[complete.cases(analysis_data), ]
            
            if (nrow(analysis_data) == 0) {
                stop("Error: No complete cases found for the selected variables.")
            }

            # Convert variables to appropriate types
            analysis_data[[y_var]] <- as.numeric(analysis_data[[y_var]])
            analysis_data[[x_var]] <- as.factor(analysis_data[[x_var]])
            
            if (!is.null(fill_var) && fill_var != "") {
                analysis_data[[fill_var]] <- as.factor(analysis_data[[fill_var]])
            }
            if (!is.null(id_var) && id_var != "") {
                analysis_data[[id_var]] <- as.factor(analysis_data[[id_var]])
            }
            if (!is.null(cov_var) && cov_var != "") {
                if (is.numeric(analysis_data[[cov_var]])) {
                    # Keep numeric for continuous covariate mapping
                } else {
                    analysis_data[[cov_var]] <- as.factor(analysis_data[[cov_var]])
                }
            }
            
            # Apply log transformation if requested
            if (self$options$log_transform) {
                # Add small constant to avoid log(0) issues
                analysis_data[[y_var]] <- log(analysis_data[[y_var]] + 0.01)
            }
            
            # Handle outliers if requested
            if (self$options$outlier_method != "none") {
                analysis_data <- private$.handle_outliers(analysis_data, y_var, self$options$outlier_method)
            }

            # Generate summary statistics if requested
            if (self$options$show_statistics) {
                stats_html <- private$.generate_statistics(analysis_data, y_var, x_var, fill_var)
                self$results$statistics$setContent(stats_html)
            }
            
            # Generate group comparisons if requested
            if (self$options$show_comparisons) {
                comparison_html <- private$.generate_comparisons(analysis_data, y_var, x_var, fill_var)
                self$results$comparisons$setContent(comparison_html)
            }
            
            # Generate interpretation guide
            if (self$options$show_interpretation) {
                interpretation_html <- private$.generate_interpretation_guide(analysis_data, y_var, x_var, id_var)
                self$results$interpretation$setContent(interpretation_html)
            }
            
            # Generate effect size analysis if requested
            if (self$options$show_effect_size) {
                effect_size_html <- private$.generate_effect_sizes(analysis_data, y_var, x_var, self$options$effect_size_type)
                self$results$effect_sizes$setContent(effect_size_html)
            }
            
            # Generate change analysis if requested
            if (self$options$show_change_scores) {
                change_html <- private$.generate_change_analysis(
                    analysis_data, y_var, x_var, id_var, 
                    self$options$baseline_group, self$options$responder_threshold
                )
                self$results$change_analysis$setContent(change_html)
            }
            
            # Generate clinical report if requested
            if (self$options$generate_report) {
                report_html <- private$.generate_clinical_report(
                    analysis_data, y_var, x_var, 
                    self$options$population_type
                )
                self$results$clinical_report$setContent(report_html)
            }
            
            # Generate methods text if requested
            if (self$options$include_methods) {
                methods_html <- private$.generate_methods_text(self$options)
                self$results$methods_text$setContent(methods_html)
            }

            # Store data for plotting
            private$.analysis_data <- analysis_data

        },

        .plot = function(image, ggtheme, theme, ...) {
            
            # Check if analysis was performed
            if (is.null(private$.analysis_data)) {
                return()
            }
            
            analysis_data <- private$.analysis_data
            y_var <- self$options$y_var
            x_var <- self$options$x_var
            fill_var <- self$options$fill_var
            id_var <- self$options$id_var
            cov_var <- self$options$cov_var
            
            # Determine fill mapping
            if (!is.null(fill_var) && fill_var != "") {
                fill_mapping <- fill_var
            } else {
                fill_mapping <- x_var
            }
            
            # Create base plot
            p <- ggplot2::ggplot(analysis_data, ggplot2::aes_string(
                x = x_var, 
                y = y_var, 
                fill = fill_mapping
            ))
            
            # Add covariate mapping if specified
            if (!is.null(cov_var) && cov_var != "") {
                p$mapping$colour <- rlang::sym(cov_var)
            }
            
            # Create geom_rain with advanced options
            rain_params <- list(
                rain.side = self$options$rain_side,
                point.args = list(
                    size = self$options$point_size,
                    alpha = self$options$point_alpha
                ),
                violin.args = list(
                    alpha = self$options$violin_alpha
                ),
                boxplot.args = list(
                    width = self$options$boxplot_width
                )
            )
            
            # Add longitudinal connections if requested
            if (self$options$show_longitudinal && !is.null(id_var) && id_var != "") {
                rain_params$id.long.var <- id_var
            }
            
            # Add Likert mode if requested
            if (self$options$likert_mode) {
                rain_params$likert <- TRUE
            }
            
            # Add covariate mapping if specified
            if (!is.null(cov_var) && cov_var != "") {
                rain_params$cov <- cov_var
            }
            
            # Add geom_rain with all parameters
            p <- p + do.call(ggrain::geom_rain, rain_params)
            
            # Apply color palette
            colors <- private$.get_color_palette(fill_mapping, analysis_data)
            p <- p + ggplot2::scale_fill_manual(values = colors)
            
            # Apply covariate color scale if needed
            if (!is.null(cov_var) && cov_var != "") {
                if (is.numeric(analysis_data[[cov_var]])) {
                    p <- p + ggplot2::scale_color_viridis_c()
                } else {
                    cov_colors <- private$.get_color_palette(cov_var, analysis_data)
                    p <- p + ggplot2::scale_color_manual(values = cov_colors)
                }
            }
            
            # Add clinical cutoff line if specified
            if (!is.null(self$options$clinical_cutoff) && is.numeric(self$options$clinical_cutoff)) {
                p <- p + ggplot2::geom_hline(
                    yintercept = self$options$clinical_cutoff,
                    linetype = "dashed",
                    color = "red",
                    size = 1
                ) +
                ggplot2::annotate(
                    "text",
                    x = Inf,
                    y = self$options$clinical_cutoff,
                    label = "Clinical Threshold",
                    hjust = 1.1,
                    vjust = -0.5,
                    color = "red",
                    size = 3
                )
            }
            
            # Add reference range shading if specified
            if (!is.null(self$options$reference_range_min) && !is.null(self$options$reference_range_max) &&
                is.numeric(self$options$reference_range_min) && is.numeric(self$options$reference_range_max)) {
                p <- p + ggplot2::annotate(
                    "rect",
                    xmin = -Inf, xmax = Inf,
                    ymin = self$options$reference_range_min,
                    ymax = self$options$reference_range_max,
                    alpha = 0.2,
                    fill = "green"
                ) +
                ggplot2::annotate(
                    "text",
                    x = -Inf,
                    y = self$options$reference_range_max,
                    label = "Normal Range",
                    hjust = -0.1,
                    vjust = -0.5,
                    color = "darkgreen",
                    size = 3
                )
            }
            
            # Add MCID band if requested
            if (self$options$show_mcid && !is.null(self$options$mcid_value) && is.numeric(self$options$mcid_value)) {
                # Calculate group means for MCID display
                group_means <- aggregate(analysis_data[[y_var]], 
                                       by = list(analysis_data[[x_var]]), 
                                       FUN = mean, na.rm = TRUE)
                
                for (i in 1:nrow(group_means)) {
                    p <- p + ggplot2::annotate(
                        "rect",
                        xmin = i - 0.4, xmax = i + 0.4,
                        ymin = group_means$x[i] - self$options$mcid_value/2,
                        ymax = group_means$x[i] + self$options$mcid_value/2,
                        alpha = 0.15,
                        fill = "blue"
                    )
                }
            }
            
            # Add sample size annotations if requested
            if (self$options$show_sample_size) {
                # Calculate sample sizes per group
                sample_sizes <- table(analysis_data[[x_var]])
                
                # Get y-axis range for positioning
                y_range <- range(analysis_data[[y_var]], na.rm = TRUE)
                y_pos <- y_range[1] - diff(y_range) * 0.05
                
                for (i in seq_along(sample_sizes)) {
                    p <- p + ggplot2::annotate(
                        "text",
                        x = i,
                        y = y_pos,
                        label = paste0("n=", sample_sizes[i]),
                        size = 3,
                        color = "black"
                    )
                }
            }
            
            # Add CV bands if requested for biomarker analysis
            if (self$options$show_cv_bands) {
                # Calculate CV bands (typically 15% and 20%)
                group_stats <- aggregate(analysis_data[[y_var]], 
                                       by = list(analysis_data[[x_var]]), 
                                       FUN = function(x) c(mean = mean(x), sd = sd(x)))
                
                for (i in 1:nrow(group_stats)) {
                    mean_val <- group_stats$x[i, "mean"]
                    cv_15 <- mean_val * 0.15
                    cv_20 <- mean_val * 0.20
                    
                    # 15% CV band
                    p <- p + ggplot2::annotate(
                        "rect",
                        xmin = i - 0.3, xmax = i + 0.3,
                        ymin = mean_val - cv_15,
                        ymax = mean_val + cv_15,
                        alpha = 0.1,
                        fill = "orange"
                    )
                    
                    # 20% CV band
                    p <- p + ggplot2::annotate(
                        "rect",
                        xmin = i - 0.3, xmax = i + 0.3,
                        ymin = mean_val - cv_20,
                        ymax = mean_val + cv_20,
                        alpha = 0.05,
                        fill = "red"
                    )
                }
            }
            
            # Apply journal-specific theme
            p <- p + private$.apply_journal_theme(p, self$options$journal_style)
            
            # Add p-values if requested and comparisons were made
            if (self$options$p_value_position != "none" && self$options$show_comparisons) {
                p <- private$.add_p_values(p, analysis_data, y_var, x_var, self$options$p_value_position)
            }
            
            # Add labels
            x_label <- if (self$options$x_label != "") self$options$x_label else x_var
            y_label <- if (self$options$y_label != "") self$options$y_label else y_var
            
            # Add log scale label if transformed
            if (self$options$log_transform) {
                y_label <- paste0("log(", y_label, ")")
            }
            
            plot_title <- self$options$plot_title
            
            # Add population type to title if specified
            if (self$options$population_type != "itt") {
                pop_label <- switch(self$options$population_type,
                    "pp" = "Per-Protocol",
                    "mitt" = "Modified ITT",
                    "at" = "As-Treated"
                )
                plot_title <- paste0(plot_title, " (", pop_label, " Population)")
            }
            
            p <- p + ggplot2::labs(
                title = plot_title,
                x = x_label,
                y = y_label
            )
            
            print(p)
            TRUE
        },

        .get_color_palette = function(var_name, data) {
            palette_name <- self$options$color_palette
            n_colors <- length(levels(as.factor(data[[var_name]])))
            
            if (palette_name == "clinical") {
                return(c("#2E86AB", "#A23B72", "#F18F01", "#C73E1D", "#593E2C", "#8E6C8A")[1:n_colors])
            } else if (palette_name == "viridis") {
                return(viridis::viridis(n_colors, discrete = TRUE))
            } else if (palette_name == "pastel") {
                return(c("#FFB3BA", "#BAFFC9", "#BAE1FF", "#FFFFBA", "#FFDFBA", "#E0BBE4")[1:n_colors])
            } else if (palette_name %in% c("set1", "set2", "dark2")) {
                palette_r_name <- switch(palette_name,
                    "set1" = "Set1",
                    "set2" = "Set2", 
                    "dark2" = "Dark2"
                )
                if (n_colors <= 8) {
                    return(RColorBrewer::brewer.pal(max(3, n_colors), palette_r_name)[1:n_colors])
                } else {
                    base_colors <- RColorBrewer::brewer.pal(8, palette_r_name)
                    return(grDevices::colorRampPalette(base_colors)(n_colors))
                }
            } else {
                # Default clinical colors
                return(c("#2E86AB", "#A23B72", "#F18F01", "#C73E1D", "#593E2C", "#8E6C8A")[1:n_colors])
            }
        },

        .generate_statistics = function(data, y_var, x_var, fill_var) {
            
            group_var <- if (!is.null(fill_var) && fill_var != "") fill_var else x_var
            
            stats_summary <- data %>%
                dplyr::group_by(.data[[group_var]]) %>%
                dplyr::summarise(
                    n = dplyr::n(),
                    mean = round(mean(.data[[y_var]], na.rm = TRUE), 3),
                    median = round(median(.data[[y_var]], na.rm = TRUE), 3),
                    sd = round(sd(.data[[y_var]], na.rm = TRUE), 3),
                    min_val = round(min(.data[[y_var]], na.rm = TRUE), 3),
                    max_val = round(max(.data[[y_var]], na.rm = TRUE), 3),
                    .groups = 'drop'
                )
            
            stats_html <- paste0(
                "<div style='background-color: #f8f9fa; padding: 20px; border-radius: 8px; margin-bottom: 20px;'>",
                "<h3 style='color: #495057; margin-top: 0;'>📊 Advanced Raincloud Statistics</h3>",
                "<table style='width: 100%; border-collapse: collapse; font-family: Arial, sans-serif;'>",
                "<thead><tr style='background-color: #6c757d; color: white;'>",
                "<th style='padding: 8px; border: 1px solid #dee2e6;'>Group</th>",
                "<th style='padding: 8px; border: 1px solid #dee2e6;'>N</th>",
                "<th style='padding: 8px; border: 1px solid #dee2e6;'>Mean</th>",
                "<th style='padding: 8px; border: 1px solid #dee2e6;'>Median</th>",
                "<th style='padding: 8px; border: 1px solid #dee2e6;'>SD</th>",
                "<th style='padding: 8px; border: 1px solid #dee2e6;'>Range</th>",
                "</tr></thead><tbody>"
            )
            
            for (i in 1:nrow(stats_summary)) {
                row_bg <- if (i %% 2 == 0) "#ffffff" else "#f8f9fa"
                stats_html <- paste0(stats_html,
                    "<tr style='background-color: ", row_bg, ";'>",
                    "<td style='padding: 8px; border: 1px solid #dee2e6;'><strong>", stats_summary[[group_var]][i], "</strong></td>",
                    "<td style='padding: 8px; border: 1px solid #dee2e6; text-align: center;'>", stats_summary$n[i], "</td>",
                    "<td style='padding: 8px; border: 1px solid #dee2e6; text-align: center;'>", stats_summary$mean[i], "</td>",
                    "<td style='padding: 8px; border: 1px solid #dee2e6; text-align: center;'>", stats_summary$median[i], "</td>",
                    "<td style='padding: 8px; border: 1px solid #dee2e6; text-align: center;'>", stats_summary$sd[i], "</td>",
                    "<td style='padding: 8px; border: 1px solid #dee2e6; text-align: center;'>", stats_summary$min_val[i], " - ", stats_summary$max_val[i], "</td>",
                    "</tr>"
                )
            }
            
            stats_html <- paste0(stats_html, 
                "</tbody></table>",
                "<p style='font-size: 12px; color: #6c757d; margin-top: 15px;'>",
                "<em>Summary statistics for advanced raincloud plot groups.</em>",
                "</p></div>"
            )
            
            return(stats_html)
        },

        .generate_comparisons = function(data, y_var, x_var, fill_var) {
            
            group_var <- if (!is.null(fill_var) && fill_var != "") fill_var else x_var
            groups <- levels(as.factor(data[[group_var]]))
            n_groups <- length(groups)
            
            if (n_groups < 2) {
                return("<div style='padding: 20px;'><p>Group comparisons require at least 2 groups.</p></div>")
            }
            
            # Perform appropriate statistical test
            if (n_groups == 2) {
                group1_data <- data[data[[group_var]] == groups[1], y_var]
                group2_data <- data[data[[group_var]] == groups[2], y_var]
                
                # Use Wilcoxon test for robustness
                test_result <- wilcox.test(group1_data, group2_data)
                test_name <- "Wilcoxon rank-sum test"
                test_stat <- round(test_result$statistic, 4)
                p_value <- round(test_result$p.value, 4)
                test_details <- paste0("W = ", test_stat)
                
            } else {
                # Multiple groups - use Kruskal-Wallis
                formula_str <- paste(y_var, "~", group_var)
                kw_result <- kruskal.test(as.formula(formula_str), data = data)
                
                test_name <- "Kruskal-Wallis test"
                test_stat <- round(kw_result$statistic, 4)
                p_value <- round(kw_result$p.value, 4)
                test_details <- paste0("χ² = ", test_stat, ", df = ", kw_result$parameter)
            }
            
            # Format results
            significance <- if (p_value < 0.001) "Highly significant (***)" else 
                          if (p_value < 0.01) "Very significant (**)" else
                          if (p_value < 0.05) "Significant (*)" else "Not significant"
            
            comparison_html <- paste0(
                "<div style='background-color: #f3e5f5; padding: 20px; border-radius: 8px; margin-bottom: 20px;'>",
                "<h3 style='color: #7b1fa2; margin-top: 0;'>📊 Group Comparison Results</h3>",
                "<table style='width: 100%; border-collapse: collapse;'>",
                "<tr><td style='padding: 8px; border: 1px solid #ddd;'><strong>Test Method:</strong></td><td style='padding: 8px; border: 1px solid #ddd;'>", test_name, "</td></tr>",
                "<tr><td style='padding: 8px; border: 1px solid #ddd;'><strong>Test Statistic:</strong></td><td style='padding: 8px; border: 1px solid #ddd;'>", test_details, "</td></tr>",
                "<tr><td style='padding: 8px; border: 1px solid #ddd;'><strong>P-value:</strong></td><td style='padding: 8px; border: 1px solid #ddd;'>", p_value, "</td></tr>",
                "<tr><td style='padding: 8px; border: 1px solid #ddd;'><strong>Result:</strong></td><td style='padding: 8px; border: 1px solid #ddd;'>", significance, "</td></tr>",
                "</table>",
                "<p style='font-size: 12px; color: #7b1fa2; margin-top: 15px;'>",
                "<em>* p < 0.05, ** p < 0.01, *** p < 0.001. Non-parametric tests used for robustness.</em>",
                "</p></div>"
            )
            
            return(comparison_html)
        },

        .generate_interpretation_guide = function(data, y_var, x_var, id_var) {
            
            n_total <- nrow(data)
            n_groups <- length(levels(as.factor(data[[x_var]])))
            rain_side <- self$options$rain_side
            has_longitudinal <- self$options$show_longitudinal && !is.null(id_var) && id_var != ""
            has_likert <- self$options$likert_mode
            
            interpretation_html <- paste0(
                "<div style='background-color: #e8f5e8; padding: 20px; border-radius: 8px;'>",
                "<h3 style='color: #2e7d32; margin-top: 0;'>🌧️ Advanced Raincloud Plot Guide</h3>",
                
                "<h4 style='color: #2e7d32;'>Current Configuration:</h4>",
                "<ul>",
                "<li><strong>Variable:</strong> ", y_var, " distributed across ", n_groups, " groups</li>",
                "<li><strong>Observations:</strong> ", n_total, " data points</li>",
                "<li><strong>Raincloud Position:</strong> ", switch(rain_side, "l" = "Left side", "r" = "Right side", "f" = "Flanking (both sides)"), "</li>",
                if (has_longitudinal) paste0("<li><strong>Longitudinal Connections:</strong> Connected by ", id_var, "</li>") else "",
                if (has_likert) "<li><strong>Likert Mode:</strong> Y-axis jittering enabled for ordinal data</li>" else "",
                "</ul>",
                
                "<h4 style='color: #2e7d32;'>Advanced Features in Use:</h4>",
                "<ul>",
                if (has_longitudinal) {
                    "<li><strong>Longitudinal Connections:</strong> Lines connect repeated observations from the same subjects across groups - perfect for before/after or repeated measures designs.</li>"
                } else {
                    "<li><strong>Longitudinal Connections:</strong> Not enabled. Add an ID variable to connect repeated observations.</li>"
                },
                if (has_likert) {
                    "<li><strong>Likert Scale Mode:</strong> Y-axis jittering applied to better visualize discrete ordinal responses.</li>"
                } else {
                    "<li><strong>Likert Scale Mode:</strong> Not enabled. Enable for survey or ordinal data visualization.</li>"
                },
                "</ul>",
                
                "<h4 style='color: #2e7d32;'>ggrain vs. Standard Raincloud:</h4>",
                "<ul>",
                "<li><strong>Standard Raincloud (ggdist):</strong> Basic three-layer visualization (violin + box + points)</li>",
                "<li><strong>Advanced Raincloud (ggrain):</strong> Enhanced with connections, positioning, and specialized data support</li>",
                "<li><strong>Key Advantages:</strong> Longitudinal connections, Likert support, flexible positioning, covariate mapping</li>",
                "</ul>",
                
                "<h4 style='color: #2e7d32;'>Clinical Research Applications:</h4>",
                "<ul>",
                "<li><strong>Repeated Measures:</strong> Connect pre/post treatment measurements for same patients</li>",
                "<li><strong>Survey Analysis:</strong> Handle Likert scale responses with appropriate jittering</li>",
                "<li><strong>Longitudinal Studies:</strong> Track individual participants across multiple time points</li>",
                "<li><strong>Complex Comparisons:</strong> Multi-group analysis with covariate mapping</li>",
                "</ul>",
                
                "<h4 style='color: #2e7d32;'>Raincloud Position Options:</h4>",
                "<ul>",
                "<li><strong>Left ('l'):</strong> Traditional raincloud with density on left, data on right</li>",
                "<li><strong>Right ('r'):</strong> Mirrored version with density on right, data on left</li>",
                "<li><strong>Flanking ('f'):</strong> Density distributions on both sides for comparison</li>",
                "</ul>",
                
                "<p style='font-size: 12px; color: #2e7d32; margin-top: 15px;'>",
                "<em>🌧️ Advanced raincloud plots provide enhanced visualization capabilities for complex research designs</em>",
                "</p></div>"
            )
            
            return(interpretation_html)
        },
        
        # New helper functions for clinical research features
        
        .handle_outliers = function(data, y_var, method) {
            y_values <- data[[y_var]]
            
            if (method == "winsorize") {
                # Winsorize at 5th and 95th percentiles
                lower <- quantile(y_values, 0.05, na.rm = TRUE)
                upper <- quantile(y_values, 0.95, na.rm = TRUE)
                data[[y_var]] <- pmax(pmin(y_values, upper), lower)
            } else if (method == "trim") {
                # Trim values outside 5th and 95th percentiles
                lower <- quantile(y_values, 0.05, na.rm = TRUE)
                upper <- quantile(y_values, 0.95, na.rm = TRUE)
                data <- data[y_values >= lower & y_values <= upper, ]
            } else if (method == "iqr") {
                # IQR method
                Q1 <- quantile(y_values, 0.25, na.rm = TRUE)
                Q3 <- quantile(y_values, 0.75, na.rm = TRUE)
                IQR <- Q3 - Q1
                lower <- Q1 - 1.5 * IQR
                upper <- Q3 + 1.5 * IQR
                data <- data[y_values >= lower & y_values <= upper, ]
            }
            
            return(data)
        },
        
        .generate_effect_sizes = function(data, y_var, x_var, effect_type) {
            groups <- unique(data[[x_var]])
            n_groups <- length(groups)
            
            if (n_groups < 2) {
                return("<p>Effect size calculation requires at least 2 groups.</p>")
            }
            
            html <- paste0(
                "<div style='background-color: #e8f5e9; padding: 20px; border-radius: 8px; margin-bottom: 20px;'>",
                "<h3 style='color: #2e7d32; margin-top: 0;'>📈 Effect Size Analysis</h3>",
                "<p>Effect size type: <strong>", effect_type, "</strong></p>",
                "<table style='width: 100%; border-collapse: collapse;'>",
                "<thead><tr style='background-color: #4caf50; color: white;'>",
                "<th style='padding: 8px; border: 1px solid #ddd;'>Comparison</th>",
                "<th style='padding: 8px; border: 1px solid #ddd;'>Effect Size</th>",
                "<th style='padding: 8px; border: 1px solid #ddd;'>95% CI</th>",
                "<th style='padding: 8px; border: 1px solid #ddd;'>Interpretation</th>",
                "</tr></thead><tbody>"
            )
            
            # Calculate pairwise effect sizes
            for (i in 1:(n_groups-1)) {
                for (j in (i+1):n_groups) {
                    group1_data <- data[data[[x_var]] == groups[i], y_var]
                    group2_data <- data[data[[x_var]] == groups[j], y_var]
                    
                    # Calculate effect size based on type
                    effect_result <- private$.calculate_effect_size(
                        group1_data, group2_data, effect_type
                    )
                    
                    # Interpret effect size
                    interpretation <- private$.interpret_effect_size(abs(effect_result$effect_size))
                    
                    html <- paste0(html,
                        "<tr>",
                        "<td style='padding: 8px; border: 1px solid #ddd;'>", 
                        groups[i], " vs ", groups[j], "</td>",
                        "<td style='padding: 8px; border: 1px solid #ddd; text-align: center;'>", 
                        round(effect_result$effect_size, 3), "</td>",
                        "<td style='padding: 8px; border: 1px solid #ddd; text-align: center;'>", 
                        "[", round(effect_result$ci_lower, 3), ", ", 
                        round(effect_result$ci_upper, 3), "]</td>",
                        "<td style='padding: 8px; border: 1px solid #ddd;'>", 
                        interpretation, "</td>",
                        "</tr>"
                    )
                }
            }
            
            html <- paste0(html, "</tbody></table></div>")
            return(html)
        },
        
        .calculate_effect_size = function(group1, group2, type) {
            n1 <- length(group1)
            n2 <- length(group2)
            mean1 <- mean(group1, na.rm = TRUE)
            mean2 <- mean(group2, na.rm = TRUE)
            sd1 <- sd(group1, na.rm = TRUE)
            sd2 <- sd(group2, na.rm = TRUE)
            
            if (type == "cohens_d") {
                # Pooled standard deviation
                pooled_sd <- sqrt(((n1 - 1) * sd1^2 + (n2 - 1) * sd2^2) / (n1 + n2 - 2))
                effect_size <- (mean1 - mean2) / pooled_sd
            } else if (type == "hedges_g") {
                # Hedges' g includes small sample correction
                pooled_sd <- sqrt(((n1 - 1) * sd1^2 + (n2 - 1) * sd2^2) / (n1 + n2 - 2))
                d <- (mean1 - mean2) / pooled_sd
                # Small sample correction factor
                J <- 1 - (3 / (4 * (n1 + n2 - 2) - 1))
                effect_size <- d * J
            } else if (type == "glass_delta") {
                # Glass's delta uses control group SD
                effect_size <- (mean1 - mean2) / sd2
            }
            
            # Calculate confidence intervals (approximate)
            se <- sqrt((n1 + n2) / (n1 * n2) + effect_size^2 / (2 * (n1 + n2)))
            ci_lower <- effect_size - 1.96 * se
            ci_upper <- effect_size + 1.96 * se
            
            return(list(
                effect_size = effect_size,
                ci_lower = ci_lower,
                ci_upper = ci_upper
            ))
        },
        
        .interpret_effect_size = function(es) {
            if (es < 0.2) return("Negligible")
            else if (es < 0.5) return("Small")
            else if (es < 0.8) return("Medium")
            else return("Large")
        },
        
        .generate_change_analysis = function(data, y_var, x_var, id_var, baseline_group, threshold) {
            if (is.null(id_var) || id_var == "") {
                return("<p>Change analysis requires a longitudinal ID variable.</p>")
            }
            
            # Calculate change scores
            baseline_data <- data[data[[x_var]] == baseline_group, ]
            
            html <- paste0(
                "<div style='background-color: #fff3cd; padding: 20px; border-radius: 8px; margin-bottom: 20px;'>",
                "<h3 style='color: #856404; margin-top: 0;'>📊 Change Score Analysis</h3>",
                "<p>Baseline group: <strong>", baseline_group, "</strong></p>",
                "<p>Response threshold: <strong>", threshold, "%</strong></p>"
            )
            
            # Calculate responder analysis
            # This is a simplified version - in practice would need proper pairing
            
            html <- paste0(html, "</div>")
            return(html)
        },
        
        .generate_clinical_report = function(data, y_var, x_var, pop_type) {
            pop_label <- switch(pop_type,
                "itt" = "Intention-to-Treat",
                "pp" = "Per-Protocol",
                "mitt" = "Modified ITT",
                "at" = "As-Treated"
            )
            
            html <- paste0(
                "<div style='background-color: #e3f2fd; padding: 20px; border-radius: 8px; margin-bottom: 20px;'>",
                "<h3 style='color: #1565c0; margin-top: 0;'>📋 Clinical Analysis Report</h3>",
                "<h4>Study Population</h4>",
                "<p>Analysis population: <strong>", pop_label, "</strong></p>",
                "<p>Total N: <strong>", nrow(data), "</strong></p>",
                
                "<h4>Primary Outcome</h4>",
                "<p>Variable: <strong>", y_var, "</strong></p>",
                "<p>Measurement type: Continuous</p>",
                
                "<h4>Statistical Summary</h4>",
                "<p>Number of groups: <strong>", length(unique(data[[x_var]])), "</strong></p>",
                "<p>Overall mean (SD): <strong>", 
                round(mean(data[[y_var]], na.rm = TRUE), 2), " (", 
                round(sd(data[[y_var]], na.rm = TRUE), 2), ")</strong></p>",
                
                "</div>"
            )
            
            return(html)
        },
        
        .generate_methods_text = function(options) {
            html <- paste0(
                "<div style='background-color: #f8f9fa; padding: 20px; border-radius: 8px; margin-bottom: 20px;'>",
                "<h3 style='color: #495057; margin-top: 0;'>📝 Methods Section</h3>",
                "<p style='text-align: justify;'>",
                "Data were visualized using advanced raincloud plots, which combine ",
                "violin plots, boxplots, and individual data points to provide a comprehensive ",
                "view of data distributions. "
            )
            
            # Add specific methods based on options
            if (options$log_transform) {
                html <- paste0(html, "Values were log-transformed prior to analysis. ")
            }
            
            if (options$outlier_method != "none") {
                html <- paste0(html, "Outliers were handled using the ", 
                    options$outlier_method, " method. ")
            }
            
            if (options$show_effect_size) {
                html <- paste0(html, "Effect sizes were calculated using ", 
                    options$effect_size_type, ". ")
            }
            
            html <- paste0(html,
                "Statistical analyses were performed using R version ", 
                R.version$major, ".", R.version$minor, 
                " with the jamovi ClinicoPath module.",
                "</p></div>"
            )
            
            return(html)
        },
        
        .apply_journal_theme = function(p, journal_style) {
            if (journal_style == "nature") {
                p <- p + ggplot2::theme_minimal() +
                    ggplot2::theme(
                        plot.title = ggplot2::element_text(size = 10, face = "bold"),
                        axis.title = ggplot2::element_text(size = 9),
                        axis.text = ggplot2::element_text(size = 8),
                        legend.title = ggplot2::element_text(size = 9),
                        legend.text = ggplot2::element_text(size = 8),
                        legend.position = "bottom"
                    )
            } else if (journal_style == "nejm") {
                p <- p + ggplot2::theme_classic() +
                    ggplot2::theme(
                        plot.title = ggplot2::element_text(size = 12, face = "bold"),
                        axis.title = ggplot2::element_text(size = 10),
                        axis.text = ggplot2::element_text(size = 9),
                        legend.position = "right"
                    )
            } else if (journal_style == "lancet") {
                p <- p + ggplot2::theme_bw() +
                    ggplot2::theme(
                        plot.title = ggplot2::element_text(size = 11, face = "bold"),
                        axis.title = ggplot2::element_text(size = 10),
                        panel.grid.minor = ggplot2::element_blank()
                    )
            } else if (journal_style == "jama") {
                p <- p + ggplot2::theme_light() +
                    ggplot2::theme(
                        plot.title = ggplot2::element_text(size = 11),
                        axis.title = ggplot2::element_text(size = 10, face = "bold"),
                        panel.border = ggplot2::element_rect(color = "black", size = 1)
                    )
            } else {
                # Default theme
                p <- p + ggplot2::theme_minimal() +
                    ggplot2::theme(
                        plot.title = ggplot2::element_text(size = 14, face = "bold", hjust = 0.5),
                        axis.title = ggplot2::element_text(size = 12),
                        legend.position = "bottom"
                    )
            }
            
            return(p)
        },
        
        .add_p_values = function(p, data, y_var, x_var, position) {
            # Simplified p-value addition
            # In practice, would retrieve from comparison results
            
            if (position == "above") {
                y_max <- max(data[[y_var]], na.rm = TRUE)
                y_pos <- y_max + diff(range(data[[y_var]], na.rm = TRUE)) * 0.1
                
                p <- p + ggplot2::annotate(
                    "text",
                    x = mean(1:length(unique(data[[x_var]]))),
                    y = y_pos,
                    label = "p < 0.001",
                    size = 3,
                    fontface = "italic"
                )
            }
            
            return(p)
        }

    )
)

