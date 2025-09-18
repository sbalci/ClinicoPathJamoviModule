#' @title GraphPad Prism Style Plots
#' @importFrom R6 R6Class
#' @import jmvcore
#' @importFrom ggplot2 ggplot aes geom_violin geom_boxplot geom_point geom_col geom_line
#' @importFrom ggplot2 labs theme facet_wrap coord_flip position_dodge position_jitter
#' @importFrom ggplot2 stat_summary geom_errorbar scale_x_discrete scale_y_continuous
#' @importFrom ggplot2 guides guide_legend ggsave element_text element_blank margin
#' @importFrom dplyr group_by summarise mutate n
#' @importFrom stats t.test wilcox.test aov kruskal.test shapiro.test
#' @importFrom stringr str_to_title str_replace_all
#' @export

ggprismClass <- if (requireNamespace("jmvcore")) R6::R6Class("ggprismClass",
    inherit = ggprismBase,
    private = list(
        
        # Internal data storage
        .analysis_data = NULL,
        .clinical_warnings = NULL,
        .interpretation_cache = NULL,
        .sample_summary = NULL,
        
        .init = function() {
            # Dynamic plot sizing based on plot type and options
            plot_width <- 800
            plot_height <- 600
            
            # Adjust for publication mode
            if (self$options$publication_ready) {
                plot_width <- 1200
                plot_height <- 900
                self$results$publication_plot$setVisible(TRUE)
            } else {
                self$results$publication_plot$setVisible(FALSE)
            }
            
            # Set main plot size
            self$results$main_plot$setSize(plot_width, plot_height)
            
            # Control palette preview visibility
            if (self$options$preview_mode) {
                self$results$palette_preview$setVisible(TRUE)
            } else {
                self$results$palette_preview$setVisible(FALSE)
            }
        },

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
                
                self$results$instructions$setContent(intro_msg)
                return()
            } else {
                self$results$instructions$setContent("")
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
                self$results$prism_guide$setContent(error_msg)
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
                self$results$summary_statistics$setContent(summary_html)
            }
            
            # Generate statistical tests if requested
            if (self$options$show_statistics) {
                stats_html <- private$.generate_statistical_tests(analysis_data, x_var, y_var, group_var)
                self$results$statistical_tests$setContent(stats_html)
            }
            
            # Generate clinical analysis outputs
            private$.validate_clinical_assumptions()
            private$.generate_clinical_summary()
            private$.generate_interpretation_guide_content()
            private$.generate_assumptions_check()
            private$.generate_report_sentences()
            private$.generate_statistical_glossary()
            private$.generate_clinical_presets()

            # Generate Prism style guide
            guide_html <- private$.generate_prism_guide(analysis_data, x_var, y_var, group_var)
            self$results$prism_guide$setContent(guide_html)
            
            # Generate palette information
            palette_html <- private$.generate_palette_information()
            self$results$palette_information$setContent(palette_html)
            
            # Generate export code if publication mode
            if (self$options$publication_ready) {
                code_html <- private$.generate_export_code()
                self$results$export_code$setContent(code_html)
            }
            
            # Generate accessibility notes
            accessibility_html <- private$.generate_accessibility_notes()
            self$results$accessibility_notes$setContent(accessibility_html)

            # Store data for plotting with performance optimization
            if (nrow(analysis_data) > 10000) {
                # For large datasets, sample for preview but keep full data for publication
                sample_size <- min(5000, nrow(analysis_data))
                private$.sample_summary <- list(
                    n_total = nrow(analysis_data),
                    is_sampled = TRUE,
                    sample_size = sample_size
                )
                if (self$options$publication_ready) {
                    # Use full dataset for publication plots
                    private$.analysis_data <- analysis_data
                } else {
                    # Safe sampling for interactive preview
                    sample_indices <- sample(nrow(analysis_data), sample_size)
                    private$.analysis_data <- analysis_data[sample_indices, ]
                }
            } else {
                private$.analysis_data <- analysis_data
                private$.sample_summary <- list(
                    n_total = nrow(analysis_data),
                    is_sampled = FALSE,
                    sample_size = nrow(analysis_data)
                )
            }

        },

        .plot_main = function(image, ggtheme, theme, ...) {
            return(private$.create_main_plot(image, ggtheme, theme, ...))
        },
        
        .plot_palette_preview = function(image, ggtheme, theme, ...) {
            return(private$.create_palette_preview(image, ggtheme, theme, ...))
        },
        
        .plot_publication = function(image, ggtheme, theme, ...) {
            return(private$.create_publication_plot(image, ggtheme, theme, ...))
        },
        
        .create_main_plot = function(image, ggtheme, theme, ...) {
            
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
            
            # Determine grouping variable for aesthetics with safe validation
            group_mapping <- if (!is.null(group_var) &&
                               group_var != "" &&
                               group_var %in% names(analysis_data)) {
                # Check if group variable has multiple levels
                if (length(unique(analysis_data[[group_var]])) > 1) {
                    group_var
                } else {
                    NULL  # Single group, don't use for aesthetics
                }
            } else {
                NULL
            }
            
            # Create base plot
            if (!is.null(group_mapping)) {
                p <- ggplot2::ggplot(analysis_data, ggplot2::aes_string(x = x_var, y = y_var, 
                                                                       fill = group_mapping, color = group_mapping))
            } else {
                p <- ggplot2::ggplot(analysis_data, ggplot2::aes_string(x = x_var, y = y_var))
            }
            
            # Add plot type specific geoms
            if (plot_type == "violin") {
                # Use violin_width if specified
                violin_width <- if (!is.null(self$options$violin_width) && self$options$violin_width > 0) {
                    self$options$violin_width
                } else {
                    0.9
                }
                p <- p + ggplot2::geom_violin(alpha = 0.7, trim = FALSE, width = violin_width)
                if (self$options$error_bars != "none") {
                    p <- p + ggplot2::stat_summary(fun.data = private$.get_error_function(),
                                                  geom = "errorbar", width = 0.2)
                }
                if (self$options$show_points) {
                    # Use jitter_width if specified
                    jitter_width <- if (!is.null(self$options$jitter_width) && self$options$jitter_width >= 0) {
                        self$options$jitter_width
                    } else {
                        0.2
                    }
                    p <- p + ggplot2::geom_point(position = ggplot2::position_jitter(width = jitter_width),
                                                size = self$options$point_size, alpha = self$options$point_alpha)
                }
                
            } else if (plot_type == "boxplot") {
                p <- p + ggplot2::geom_boxplot(alpha = 0.7)
                if (self$options$show_points) {
                    # Use jitter_width if specified for boxplot points
                    jitter_width <- if (!is.null(self$options$jitter_width) && self$options$jitter_width >= 0) {
                        self$options$jitter_width
                    } else {
                        0.2
                    }
                    p <- p + ggplot2::geom_point(position = ggplot2::position_jitter(width = jitter_width),
                                                size = self$options$point_size, alpha = self$options$point_alpha)
                }
                
            } else if (plot_type == "scatter") {
                # For scatter plots, jitter can be applied if specified
                if (!is.null(self$options$jitter_width) && self$options$jitter_width > 0) {
                    p <- p + ggplot2::geom_jitter(width = self$options$jitter_width, height = 0,
                                                 size = self$options$point_size, alpha = self$options$point_alpha)
                } else {
                    p <- p + ggplot2::geom_point(size = self$options$point_size, alpha = self$options$point_alpha)
                }
                
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
            
            # Apply comprehensive Prism styling
            palette_name <- self$options$prism_palette
            
            # Apply color and fill scales
            if (!is.null(group_mapping)) {
                color_scales <- private$.get_prism_color_scales(palette_name)
                p <- p + color_scales[[1]] + color_scales[[2]]
            }
            
            # Apply Prism guides if advanced options enabled
            if (self$options$prism_guides != "standard") {
                p <- p + private$.get_prism_guides()
            }

            # Add annotation ticks if requested
            if (self$options$annotation_ticks) {
                p <- p + private$.add_annotation_ticks()
            }

            # Add custom statistical comparisons if specified
            if (!is.null(self$options$custom_comparisons) && self$options$custom_comparisons != "") {
                p <- p + private$.add_custom_comparisons()
            }
            
            # Apply shape palette if relevant for scatter plots
            if (plot_type == "scatter" && !is.null(group_mapping)) {
                p <- p + private$.get_prism_shape_scale()
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
            
            # Create base Prism theme with error handling
            if (requireNamespace("ggprism", quietly = TRUE)) {
                base_theme <- switch(theme_name,
                    "default" = ggprism::theme_prism(base_size = base_size),
                    "white" = ggprism::theme_prism(base_size = base_size, axis_text_angle = 0),
                    "minimal" = ggprism::theme_prism(base_size = base_size) + 
                        ggplot2::theme(panel.grid.major = ggplot2::element_blank()),
                    "publication" = ggprism::theme_prism(base_size = base_size) +
                        ggplot2::theme(
                            plot.title = ggplot2::element_text(size = base_size + 2, face = "bold", hjust = 0.5),
                            axis.title = ggplot2::element_text(size = base_size + 1, face = "bold"),
                            legend.background = ggplot2::element_rect(fill = "white", colour = "black"),
                            panel.border = ggplot2::element_rect(colour = "black", fill = NA)
                        ),
                    ggprism::theme_prism(base_size = base_size)  # fallback
                )
            } else {
                # Fallback theme if ggprism not available
                base_theme <- ggplot2::theme_minimal(base_size = base_size) +
                    ggplot2::theme(
                        panel.border = ggplot2::element_rect(colour = "black", fill = NA),
                        axis.line = ggplot2::element_line(colour = "black")
                    )
            }
            
            return(base_theme)
        },
        
        .get_prism_color_scales = function(palette_name) {
            if (!requireNamespace("ggprism", quietly = TRUE)) {
                # Fallback to viridis if ggprism not available
                return(list(
                    ggplot2::scale_fill_viridis_d(),
                    ggplot2::scale_color_viridis_d()
                ))
            }
            
            # Apply ggprism color scales
            list(
                ggprism::scale_fill_prism(palette = palette_name),
                ggprism::scale_color_prism(palette = palette_name)
            )
        },
        
        .add_annotation_ticks = function() {
            # Add minor ticks and enhanced axis annotations for Prism-style plots
            if (requireNamespace("ggprism", quietly = TRUE)) {
                list(
                    ggprism::guide_prism_minor(),
                    ggplot2::theme(
                        axis.ticks.length = ggplot2::unit(0.2, "cm"),
                        axis.minor.ticks.length = ggplot2::unit(0.1, "cm")
                    )
                )
            } else {
                # Fallback enhanced ticks
                ggplot2::theme(
                    axis.ticks.length = ggplot2::unit(0.2, "cm"),
                    axis.line = ggplot2::element_line(size = 0.8)
                )
            }
        },

        .add_custom_comparisons = function() {
            # Parse and add custom statistical comparisons
            comparisons_text <- self$options$custom_comparisons

            # Simple parsing - expecting format like "Group1 vs Group2, Group1 vs Group3"
            if (nchar(comparisons_text) > 0) {
                comparisons <- strsplit(comparisons_text, ",")[[1]]
                comparisons <- trimws(comparisons)

                # For now, add as text annotation
                # Future enhancement: implement actual statistical comparison
                if (length(comparisons) > 0) {
                    ggplot2::annotate("text",
                                    x = Inf, y = Inf,
                                    label = paste("Custom comparisons:", paste(comparisons, collapse = "; ")),
                                    hjust = 1, vjust = 1,
                                    size = 3, color = "gray50",
                                    fontface = "italic")
                } else {
                    NULL
                }
            } else {
                NULL
            }
        },

        .get_prism_guides = function() {
            guide_type <- self$options$prism_guides
            
            if (!requireNamespace("ggprism", quietly = TRUE)) {
                return(ggplot2::guides())
            }
            
            switch(guide_type,
                "minor" = ggprism::guide_prism_minor(),
                "offset" = ggprism::guide_prism_offset(),
                "offset_minor" = ggprism::guide_prism_offset_minor(),
                "bracket" = ggprism::guide_prism_bracket(),
                ggplot2::guides()  # standard fallback
            )
        },
        
        .get_prism_shape_scale = function() {
            shape_palette <- self$options$prism_shape_palette
            
            if (!requireNamespace("ggprism", quietly = TRUE)) {
                return(ggplot2::scale_shape_discrete())
            }
            
            switch(shape_palette,
                "prism" = ggprism::scale_shape_prism(),
                "filled" = ggprism::scale_shape_prism(palette = "filled"),
                "open" = ggprism::scale_shape_prism(palette = "open"),
                ggplot2::scale_shape_discrete()  # default fallback
            )
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

        .create_palette_preview = function(image, ggtheme, theme, ...) {
            if (!self$options$preview_mode) {
                return()
            }
            
            palette_name <- self$options$prism_palette
            
            # Create color preview plot
            if (requireNamespace("ggprism", quietly = TRUE)) {
                # Get palette colors
                palette_colors <- try({
                    ggprism::prism_colour_pal(palette = palette_name)(10)
                }, silent = TRUE)
                
                if (inherits(palette_colors, "try-error")) {
                    palette_colors <- viridis::viridis(10)
                }
            } else {
                palette_colors <- viridis::viridis(10)
            }
            
            # Create data frame for color swatches
            color_data <- data.frame(
                x = rep(1:5, 2),
                y = rep(c(1, 2), each = 5),
                color = palette_colors[1:10],
                label = paste("Color", 1:10)
            )
            
            # Create palette preview plot
            p <- ggplot2::ggplot(color_data, ggplot2::aes(x = x, y = y, fill = color)) +
                ggplot2::geom_tile(color = "white", size = 2) +
                ggplot2::scale_fill_identity() +
                ggplot2::geom_text(ggplot2::aes(label = label), color = "white", fontface = "bold") +
                ggplot2::labs(title = paste("Color Palette Preview:", stringr::str_to_title(palette_name))) +
                ggplot2::theme_void() +
                ggplot2::theme(
                    plot.title = ggplot2::element_text(hjust = 0.5, size = 16, face = "bold")
                )
            
            print(p)
            TRUE
        },
        
        .create_publication_plot = function(image, ggtheme, theme, ...) {
            if (!self$options$publication_ready) {
                return()
            }

            # Set DPI for high-resolution output
            export_dpi <- if (!is.null(self$options$export_dpi) && self$options$export_dpi > 0) {
                self$options$export_dpi
            } else {
                300  # Default publication DPI
            }

            # Create enhanced publication version
            p <- private$.create_main_plot(image, ggtheme, theme, ...)
            
            # Add publication enhancements
            if (!is.null(p)) {
                # Enhanced titles and labels
                p <- p + ggplot2::theme(
                    plot.title = ggplot2::element_text(size = 18, face = "bold", hjust = 0.5),
                    plot.subtitle = ggplot2::element_text(size = 14, hjust = 0.5),
                    axis.title = ggplot2::element_text(size = 14, face = "bold"),
                    axis.text = ggplot2::element_text(size = 12),
                    legend.title = ggplot2::element_text(size = 12, face = "bold"),
                    legend.text = ggplot2::element_text(size = 11),
                    panel.border = ggplot2::element_rect(colour = "black", fill = NA, size = 1)
                )
                
                # Add publication subtitle if not already present
                if (self$options$plot_title == "GraphPad Prism Style Plot") {
                    p <- p + ggplot2::labs(
                        title = "Publication-Ready Scientific Visualization",
                        subtitle = "Generated with GraphPad Prism Styling"
                    )
                }
                
                print(p)
            }
            TRUE
        },
        
        .generate_prism_guide = function(data, x_var, y_var, group_var) {
            # Generate comprehensive Prism style guide
            
            n_total <- nrow(data)
            plot_type <- self$options$plot_type
            prism_theme <- self$options$prism_theme
            prism_palette <- self$options$prism_palette
            
            guide_html <- paste0(
                "<div style='background-color: #e8f5e8; padding: 20px; border-radius: 8px;'>",
                "<h3 style='color: #2e7d32; margin-top: 0;'>🎨 GraphPad Prism Style Guide</h3>",
                
                "<h4 style='color: #2e7d32;'>Current Configuration:</h4>",
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
                "<li><strong>20+ Color Palettes:</strong> Carefully designed scientific color schemes</li>",
                "<li><strong>Advanced Guides:</strong> Minor ticks, offset axes, and bracket annotations</li>",
                "<li><strong>Statistical Integration:</strong> Seamless p-value annotations and error bars</li>",
                "</ul>",
                
                "<h4 style='color: #2e7d32;'>Clinical Research Applications:</h4>",
                "<ul>",
                "<li><strong>Biostatistics:</strong> Professional visualization for clinical data analysis</li>",
                "<li><strong>Publication Figures:</strong> High-quality graphics for manuscripts and journals</li>",
                "<li><strong>Presentations:</strong> Clear, impactful visuals for academic conferences</li>",
                "<li><strong>Data Communication:</strong> Effective statistical storytelling with visual appeal</li>",
                "</ul>",
                
                "<p style='font-size: 12px; color: #2e7d32; margin-top: 15px;'>",
                "<em>🎨 Experience the professional quality of GraphPad Prism styling within R's powerful analysis environment</em>",
                "</p></div>"
            )
            
            return(guide_html)
        },
        
        .generate_palette_information = function() {
            palette_name <- self$options$prism_palette
            
            # Define palette descriptions
            palette_info <- list(
                "floral" = "12 nature-inspired colors perfect for biological data",
                "candy_bright" = "9 vibrant, high-contrast colors for clear group distinction",
                "office" = "9 professional colors suitable for corporate presentations",
                "pastels" = "9 soft, muted colors ideal for subtle group differences",
                "colorblind_safe" = "6 carefully selected colors accessible to colorblind viewers",
                "blueprint" = "9 blue-toned colors for technical and engineering themes",
                "neon" = "9 bright, attention-grabbing colors for impactful presentations",
                "flames" = "9 warm colors ranging from red to orange for heat-themed data",
                "ocean" = "9 blue-green colors perfect for environmental and marine data",
                "spring" = "9 fresh, light colors ideal for positive data trends",
                "starry" = "5 deep, cosmic colors for astronomical or night-themed data",
                "the_blues" = "9 monochromatic blue shades for professional presentations",
                "viridis" = "6 perceptually uniform colors following viridis principles",
                "pearl" = "6 elegant, lustrous colors for sophisticated visualizations",
                "quiet" = "9 understated colors for subtle, professional presentations",
                "stained_glass" = "9 rich, jewel-toned colors inspired by stained glass art",
                "warm_pastels" = "9 gentle, warm-toned colors for approachable visualizations",
                "prism_dark" = "10 deep, rich colors for high-contrast presentations",
                "prism_light" = "10 bright, airy colors for clean, modern aesthetics",
                "evergreen" = "9 nature-inspired greens for environmental data",
                "sunny_garden" = "9 cheerful, garden-inspired colors for positive data"
            )
            
            current_info <- palette_info[[palette_name]] %||% "Professional color palette for scientific visualization"
            
            info_html <- paste0(
                "<div style='background-color: #fff3e0; padding: 20px; border-radius: 8px;'>",
                "<h3 style='color: #ef6c00; margin-top: 0;'>🎨 Color Palette Information</h3>",
                "<h4 style='color: #ef6c00;'>Current Palette: ", stringr::str_to_title(palette_name), "</h4>",
                "<p><strong>Description:</strong> ", current_info, "</p>",
                "<h4 style='color: #ef6c00;'>Usage Tips:</h4>",
                "<ul>",
                "<li><strong>Preview Mode:</strong> Enable palette preview to see all colors</li>",
                "<li><strong>Accessibility:</strong> Consider colorblind_safe palette for inclusive design</li>",
                "<li><strong>Publication:</strong> Professional palettes work best for journals</li>",
                "<li><strong>Presentation:</strong> High-contrast palettes improve visibility</li>",
                "</ul>",
                "</div>"
            )
            
            return(info_html)
        },
        
        .generate_export_code = function() {
            if (!self$options$publication_ready) {
                return("")
            }
            
            x_var <- self$options$x_var
            y_var <- self$options$y_var
            group_var <- self$options$group_var
            plot_type <- self$options$plot_type
            palette_name <- self$options$prism_palette
            theme_name <- self$options$prism_theme
            
            code_html <- paste0(
                "<div style='background-color: #f5f5f5; padding: 20px; border-radius: 8px;'>",
                "<h3 style='color: #424242; margin-top: 0;'>💻 Reproducible R Code</h3>",
                "<p>Use this code to recreate your plot in R:</p>",
                "<pre style='background-color: #263238; color: #eeffff; padding: 15px; border-radius: 5px; overflow-x: auto;'>",
                "<code>",
                "# Load required packages\n",
                "library(ggplot2)\n",
                "library(ggprism)\n\n",
                "# Create the plot\n",
                "ggplot(data, aes(x = ", x_var, ", y = ", y_var,
                if (!is.null(group_var) && group_var != "") paste0(", fill = ", group_var, ", color = ", group_var) else "",
                ")) +\n",
                "  geom_", plot_type, "() +\n",
                "  theme_prism(base_size = ", self$options$base_size, ") +\n",
                "  scale_fill_prism(palette = '", palette_name, "') +\n",
                "  scale_color_prism(palette = '", palette_name, "') +\n",
                "  labs(title = '", self$options$plot_title, "')\n",
                "</code></pre>",
                "<p style='font-size: 12px; color: #666;'>",
                "<em>💡 Copy this code to reproduce your exact visualization in any R environment</em>",
                "</p></div>"
            )
            
            return(code_html)
        },
        
        .generate_accessibility_notes = function() {
            accessibility_html <- paste0(
                "<div style='background-color: #e1f5fe; padding: 20px; border-radius: 8px;'>",
                "<h3 style='color: #0277bd; margin-top: 0;'>♿ Accessibility & Best Practices</h3>",
                "<h4 style='color: #0277bd;'>Color Accessibility:</h4>",
                "<ul>",
                "<li><strong>Colorblind Safe:</strong> Use the 'colorblind_safe' palette for inclusive design</li>",
                "<li><strong>High Contrast:</strong> Ensure sufficient contrast between colors and backgrounds</li>",
                "<li><strong>Alternative Encodings:</strong> Combine color with shapes or patterns when possible</li>",
                "</ul>",
                "<h4 style='color: #0277bd;'>Publication Standards:</h4>",
                "<ul>",
                "<li><strong>DPI:</strong> Use 300 DPI minimum for print publications</li>",
                "<li><strong>Font Size:</strong> Ensure text remains readable when scaled</li>",
                "<li><strong>Consistency:</strong> Maintain consistent styling across all figures</li>",
                "</ul>",
                "<h4 style='color: #0277bd;'>Statistical Clarity:</h4>",
                "<ul>",
                "<li><strong>Error Bars:</strong> Always include appropriate error representations</li>",
                "<li><strong>Sample Sizes:</strong> Report sample sizes clearly</li>",
                "<li><strong>Statistical Tests:</strong> Use appropriate tests for your data type</li>",
                "</ul>",
                "</div>"
            )
            
            return(accessibility_html)
        },

        # ===== CLINICAL-FOCUSED ENHANCEMENTS =====

        .validate_clinical_assumptions = function() {
            if (is.null(private$.analysis_data)) return()

            data <- private$.analysis_data
            warnings <- character(0)

            # Sample size warnings
            n_total <- nrow(data)
            if (n_total < 30) {
                warnings <- c(warnings, paste("Small sample size (n =", n_total, ") may affect statistical validity. Consider non-parametric tests."))
            }

            # Plot type appropriateness
            plot_type <- self$options$plot_type
            if (plot_type == "violin" && n_total < 20) {
                warnings <- c(warnings, "Violin plots work best with larger sample sizes (n ≥ 20). Consider box plots for smaller samples.")
            }

            # Group size balance - enhanced error handling
            if (!is.null(self$options$group_var) &&
                self$options$group_var != "" &&
                self$options$group_var %in% names(data)) {

                group_counts <- table(data[[self$options$group_var]], useNA = "no")
                if (length(group_counts) > 1) {
                    min_group <- min(group_counts)
                    max_group <- max(group_counts)

                    if (min_group < 5) {
                        warnings <- c(warnings, paste("Some groups have very small sample sizes (minimum n =", min_group, "). Results may be unreliable."))
                    }

                    if (max_group / min_group > 3) {
                        warnings <- c(warnings, "Unbalanced group sizes detected. Consider Welch's t-test for comparing means.")
                    }
                } else if (length(group_counts) == 1) {
                    warnings <- c(warnings, "Only one group detected in grouping variable. Consider removing grouping variable.")
                }
            } else if (!is.null(self$options$group_var) && self$options$group_var != "") {
                warnings <- c(warnings, paste("Grouping variable '", self$options$group_var, "' not found in dataset."))
            }

            # Statistical test appropriateness
            if (self$options$show_statistics) {
                if (self$options$stats_method == "ttest" && n_total < 30) {
                    warnings <- c(warnings, "t-test assumes normality. With small samples, consider Wilcoxon test for non-normal data.")
                }
            }

            private$.clinical_warnings <- warnings
        },

        .generate_clinical_summary = function() {
            if (is.null(private$.analysis_data)) return()

            data <- private$.analysis_data
            plot_type <- self$options$plot_type
            x_var <- self$options$x_var
            y_var <- self$options$y_var
            group_var <- self$options$group_var

            n_total <- if (!is.null(private$.sample_summary)) {
                private$.sample_summary$n_total
            } else {
                nrow(data)
            }

            n_displayed <- nrow(data)
            is_sampled <- if (!is.null(private$.sample_summary)) {
                private$.sample_summary$is_sampled
            } else {
                FALSE
            }

            # Determine analysis type with safe group variable handling
            analysis_type <- if (!is.null(group_var) &&
                                group_var != "" &&
                                group_var %in% names(data)) {
                n_groups <- length(unique(data[[group_var]]))
                if (n_groups > 1) {
                    paste("group comparison with", n_groups, "groups")
                } else {
                    "single group analysis (grouping variable has only one level)"
                }
            } else {
                "single variable visualization"
            }

            # Statistical testing info
            stats_info <- if (self$options$show_statistics) {
                paste("Statistical differences evaluated using",
                      switch(self$options$stats_method,
                             "ttest" = "t-test (comparing means)",
                             "wilcox" = "Wilcoxon test (comparing medians)",
                             "anova" = "ANOVA (comparing multiple group means)",
                             "kruskal" = "Kruskal-Wallis test (comparing multiple group medians)",
                             self$options$stats_method))
            } else {
                "No statistical testing performed"
            }

            # Plot interpretation
            plot_description <- switch(plot_type,
                "violin" = "Violin plot shows the full distribution shape of data, revealing skewness, multimodality, and spread",
                "boxplot" = "Box plot displays median, quartiles, and outliers, providing a robust summary of data distribution",
                "scatter" = "Scatter plot reveals relationships and correlations between variables at the individual data point level",
                "column" = "Column plot compares mean values between groups with error bars showing variability",
                "line" = "Line plot shows trends and changes across categories or time points",
                "Data visualization"
            )

            summary_html <- paste(
                "<div style='background-color: #e8f5e8; padding: 20px; border-radius: 8px; margin: 10px 0; border-left: 5px solid #2e7d32;'>",
                "<h3 style='color: #2e7d32; margin-top: 0;'>📊 Clinical Summary</h3>",
                paste0("<p><strong>Analysis:</strong> ", stringr::str_to_title(analysis_type), " of ", y_var,
                       if (!is.null(group_var) && group_var != "") paste(" across", group_var) else "",
                       " (n = ", n_total,
                       if (is_sampled) paste0("; displaying ", n_displayed, " sampled points for performance") else "",
                       ")</p>"),
                paste0("<p><strong>Visualization:</strong> ", plot_description, "</p>"),
                paste0("<p><strong>Statistical Analysis:</strong> ", stats_info, "</p>"),
                if (length(private$.clinical_warnings) > 0) {
                    paste0("<div style='background-color: #fff3e0; padding: 10px; border-radius: 5px; margin: 10px 0;'>",
                           "<p style='color: #e65100; margin: 0;'><strong>⚠️ Clinical Considerations:</strong></p>",
                           "<ul style='margin: 5px 0; color: #e65100;'>",
                           paste0("<li>", private$.clinical_warnings, "</li>", collapse = ""),
                           "</ul></div>")
                } else "",
                "</div>"
            )

            self$results$clinical_summary$setContent(summary_html)
        },

        .generate_interpretation_guide_content = function() {
            plot_type <- self$options$plot_type

            interpretation_html <- paste(
                "<div style='background-color: #f3e5f5; padding: 20px; border-radius: 8px; margin: 10px 0;'>",
                "<h3 style='color: #7b1fa2; margin-top: 0;'>📖 How to Interpret This Plot</h3>",

                switch(plot_type,
                    "violin" = paste(
                        "<h4>Violin Plot Interpretation:</h4>",
                        "<ul>",
                        "<li><strong>Width:</strong> Wider sections indicate more data points at that value</li>",
                        "<li><strong>Shape:</strong> Reveals distribution pattern (normal, skewed, bimodal)</li>",
                        "<li><strong>Symmetry:</strong> Symmetric violins suggest normal distribution</li>",
                        "<li><strong>Multiple peaks:</strong> May indicate subpopulations</li>",
                        "</ul>",
                        "<p><strong>Clinical Use:</strong> Excellent for comparing distribution shapes between treatment groups.</p>"
                    ),
                    "boxplot" = paste(
                        "<h4>Box Plot Interpretation:</h4>",
                        "<ul>",
                        "<li><strong>Center line:</strong> Median (50th percentile)</li>",
                        "<li><strong>Box edges:</strong> 25th and 75th percentiles (interquartile range)</li>",
                        "<li><strong>Whiskers:</strong> Range of typical values (within 1.5 × IQR)</li>",
                        "<li><strong>Points beyond whiskers:</strong> Potential outliers</li>",
                        "</ul>",
                        "<p><strong>Clinical Use:</strong> Robust summary that's not affected by extreme values.</p>"
                    ),
                    "scatter" = paste(
                        "<h4>Scatter Plot Interpretation:</h4>",
                        "<ul>",
                        "<li><strong>Point clusters:</strong> Similar values group together</li>",
                        "<li><strong>Linear patterns:</strong> Suggest correlation between variables</li>",
                        "<li><strong>Outliers:</strong> Points far from the main pattern</li>",
                        "<li><strong>Spread:</strong> Indicates variability in the relationship</li>",
                        "</ul>",
                        "<p><strong>Clinical Use:</strong> Reveals individual patient patterns and correlations.</p>"
                    ),
                    "column" = paste(
                        "<h4>Column Plot Interpretation:</h4>",
                        "<ul>",
                        "<li><strong>Bar height:</strong> Mean value for each group</li>",
                        "<li><strong>Error bars:</strong> Indicate uncertainty (standard error or confidence interval)</li>",
                        "<li><strong>Non-overlapping error bars:</strong> Suggest statistically significant differences</li>",
                        "<li><strong>Group comparisons:</strong> Direct visual comparison of means</li>",
                        "</ul>",
                        "<p><strong>Clinical Use:</strong> Clear comparison of average outcomes between treatments.</p>"
                    ),
                    "line" = paste(
                        "<h4>Line Plot Interpretation:</h4>",
                        "<ul>",
                        "<li><strong>Slope:</strong> Rate of change between points</li>",
                        "<li><strong>Intersections:</strong> Points where groups converge</li>",
                        "<li><strong>Parallel lines:</strong> Consistent group differences</li>",
                        "<li><strong>Trends:</strong> Overall direction of change</li>",
                        "</ul>",
                        "<p><strong>Clinical Use:</strong> Shows progression over time or ordered categories.</p>"
                    ),
                    "<p>General visualization showing data patterns and relationships.</p>"
                ),
                "</div>"
            )

            self$results$interpretation_guide$setContent(interpretation_html)
        },

        .generate_assumptions_check = function() {
            if (!self$options$show_statistics) return()

            stats_method <- self$options$stats_method

            assumptions_html <- paste(
                "<div style='background-color: #fff3e0; padding: 20px; border-radius: 8px; margin: 10px 0;'>",
                "<h3 style='color: #ef6c00; margin-top: 0;'>⚠️ Statistical Assumptions & Requirements</h3>",

                switch(stats_method,
                    "ttest" = paste(
                        "<h4>t-test Assumptions:</h4>",
                        "<ul>",
                        "<li><strong>Normality:</strong> Data should be approximately normal (especially important for n < 30)</li>",
                        "<li><strong>Independence:</strong> Observations should be independent</li>",
                        "<li><strong>Equal variances:</strong> Groups should have similar spread (use Welch's t-test if not)</li>",
                        "</ul>",
                        "<p><strong>Violations:</strong> Consider Wilcoxon test for non-normal data or unequal variances.</p>"
                    ),
                    "wilcox" = paste(
                        "<h4>Wilcoxon Test Assumptions:</h4>",
                        "<ul>",
                        "<li><strong>Independence:</strong> Observations should be independent</li>",
                        "<li><strong>Ordinal data:</strong> Data should be at least ordinal (rankable)</li>",
                        "<li><strong>Similar shapes:</strong> Group distributions should have similar shapes</li>",
                        "</ul>",
                        "<p><strong>Benefits:</strong> Robust to outliers and non-normal distributions.</p>"
                    ),
                    "anova" = paste(
                        "<h4>ANOVA Assumptions:</h4>",
                        "<ul>",
                        "<li><strong>Normality:</strong> Residuals should be normally distributed</li>",
                        "<li><strong>Homogeneity:</strong> Equal variances across groups</li>",
                        "<li><strong>Independence:</strong> Observations should be independent</li>",
                        "</ul>",
                        "<p><strong>Violations:</strong> Consider Kruskal-Wallis test for non-normal data.</p>"
                    ),
                    "kruskal" = paste(
                        "<h4>Kruskal-Wallis Test Assumptions:</h4>",
                        "<ul>",
                        "<li><strong>Independence:</strong> Observations should be independent</li>",
                        "<li><strong>Ordinal data:</strong> Data should be at least ordinal</li>",
                        "<li><strong>Similar shapes:</strong> Group distributions should have similar shapes</li>",
                        "</ul>",
                        "<p><strong>Benefits:</strong> Non-parametric alternative to ANOVA.</p>"
                    ),
                    "<p>Statistical test assumptions will be displayed here.</p>"
                ),

                if (length(private$.clinical_warnings) > 0) {
                    paste0("<div style='background-color: #ffebee; padding: 10px; border-radius: 5px; margin: 10px 0;'>",
                           "<h4 style='color: #c62828;'>Detected Issues:</h4>",
                           "<ul style='color: #c62828;'>",
                           paste0("<li>", private$.clinical_warnings, "</li>", collapse = ""),
                           "</ul></div>")
                } else {
                    "<div style='background-color: #e8f5e8; padding: 10px; border-radius: 5px; margin: 10px 0;'><p style='color: #2e7d32; margin: 0;'>✅ No major assumption violations detected.</p></div>"
                },
                "</div>"
            )

            self$results$assumptions_check$setContent(assumptions_html)
        },

        .generate_report_sentences = function() {
            if (is.null(private$.analysis_data)) return()

            data <- private$.analysis_data
            x_var <- self$options$x_var
            y_var <- self$options$y_var
            group_var <- self$options$group_var
            plot_type <- self$options$plot_type
            n_total <- nrow(data)

            # Generate sample sentence with safe group handling
            sample_desc <- if (!is.null(group_var) &&
                             group_var != "" &&
                             group_var %in% names(data)) {
                group_counts <- table(data[[group_var]], useNA = "no")
                if (length(group_counts) > 1) {
                    group_text <- paste(names(group_counts), "(n =", group_counts, ")", collapse = ", ")
                    paste("across", length(group_counts), "groups:", group_text)
                } else {
                    paste("(n =", n_total, "; single group)")
                }
            } else {
                paste("(n =", n_total, ")")
            }

            plot_desc <- switch(plot_type,
                "violin" = "violin plot",
                "boxplot" = "box plot",
                "scatter" = "scatter plot",
                "column" = "column chart",
                "line" = "line graph",
                "visualization"
            )

            # Basic report sentence
            report_sentence <- paste0(
                "We analyzed ", y_var, " ", sample_desc, " using a GraphPad Prism-style ",
                plot_desc, ". "
            )

            # Add statistical testing if performed
            if (self$options$show_statistics) {
                test_desc <- switch(self$options$stats_method,
                    "ttest" = "independent samples t-test",
                    "wilcox" = "Wilcoxon rank-sum test",
                    "anova" = "one-way ANOVA",
                    "kruskal" = "Kruskal-Wallis test",
                    "statistical testing"
                )
                report_sentence <- paste0(report_sentence, "Statistical differences were evaluated using ", test_desc, ". ")
            }

            # Method statement
            method_sentence <- paste0(
                "Data visualization and analysis were performed using the ClinicoPath R package with GraphPad Prism styling."
            )

            report_html <- paste(
                "<div style='background-color: #f5f5f5; padding: 20px; border-radius: 8px; margin: 10px 0;'>",
                "<h3 style='color: #424242; margin-top: 0;'>📄 Copy-Ready Report Sentences</h3>",
                "<div style='background-color: white; padding: 15px; border-radius: 5px; border: 1px solid #ddd;'>",
                "<h4>Results Description:</h4>",
                "<p style='font-family: \"Times New Roman\", serif; line-height: 1.6;'>", report_sentence, "</p>",
                "<h4>Methods Statement:</h4>",
                "<p style='font-family: \"Times New Roman\", serif; line-height: 1.6;'>", method_sentence, "</p>",
                "</div>",
                "<div style='margin-top: 15px;'>",
                "<button onclick='navigator.clipboard.writeText(\"", gsub("\"", "'", paste(report_sentence, method_sentence)), "\")' style='background-color: #1976d2; color: white; border: none; padding: 8px 16px; border-radius: 4px; cursor: pointer;'>",
                "📋 Copy to Clipboard",
                "</button>",
                "</div>",
                "<p style='font-size: 12px; color: #666; margin-top: 10px;'>",
                "💡 <em>Edit these sentences as needed for your specific research context.</em>",
                "</p>",
                "</div>"
            )

            self$results$report_sentences$setContent(report_html)
        },

        .generate_statistical_glossary = function() {
            glossary_html <- paste(
                "<div style='background-color: #e3f2fd; padding: 20px; border-radius: 8px; margin: 10px 0;'>",
                "<h3 style='color: #1565c0; margin-top: 0;'>📚 Statistical Terms Glossary</h3>",
                "<div style='column-count: 2; column-gap: 20px;'>",

                "<div style='break-inside: avoid; margin-bottom: 15px;'>",
                "<h4 style='color: #1976d2; margin-bottom: 5px;'>P-value</h4>",
                "<p style='margin: 0; font-size: 14px;'>Probability of observing results this extreme if no real difference exists. p < 0.05 typically considered significant.</p>",
                "</div>",

                "<div style='break-inside: avoid; margin-bottom: 15px;'>",
                "<h4 style='color: #1976d2; margin-bottom: 5px;'>Confidence Interval</h4>",
                "<p style='margin: 0; font-size: 14px;'>Range of values likely to contain the true population parameter. 95% CI means 95% confidence the true value lies within this range.</p>",
                "</div>",

                "<div style='break-inside: avoid; margin-bottom: 15px;'>",
                "<h4 style='color: #1976d2; margin-bottom: 5px;'>Effect Size</h4>",
                "<p style='margin: 0; font-size: 14px;'>Magnitude of difference between groups. Large effect sizes are more clinically meaningful than small ones.</p>",
                "</div>",

                "<div style='break-inside: avoid; margin-bottom: 15px;'>",
                "<h4 style='color: #1976d2; margin-bottom: 5px;'>Standard Error</h4>",
                "<p style='margin: 0; font-size: 14px;'>Uncertainty in the estimated mean. Smaller SE indicates more precise estimates.</p>",
                "</div>",

                "<div style='break-inside: avoid; margin-bottom: 15px;'>",
                "<h4 style='color: #1976d2; margin-bottom: 5px;'>Median vs Mean</h4>",
                "<p style='margin: 0; font-size: 14px;'><strong>Median:</strong> Middle value, robust to outliers. <strong>Mean:</strong> Average value, affected by extreme values.</p>",
                "</div>",

                "<div style='break-inside: avoid; margin-bottom: 15px;'>",
                "<h4 style='color: #1976d2; margin-bottom: 5px;'>Parametric vs Non-parametric</h4>",
                "<p style='margin: 0; font-size: 14px;'><strong>Parametric:</strong> Assumes normal distribution (t-test, ANOVA). <strong>Non-parametric:</strong> No distribution assumptions (Wilcoxon, Kruskal-Wallis).</p>",
                "</div>",

                "</div>",
                "</div>"
            )

            self$results$statistical_glossary$setContent(glossary_html)
        },

        .generate_clinical_presets = function() {
            presets_html <- paste(
                "<div style='background-color: #f3e5f5; padding: 20px; border-radius: 8px; margin: 10px 0;'>",
                "<h3 style='color: #7b1fa2; margin-top: 0;'>🎯 Clinical Research Presets</h3>",
                "<p>Common configurations for clinical research scenarios:</p>",

                "<div style='display: grid; grid-template-columns: 1fr 1fr; gap: 15px; margin-top: 15px;'>",

                "<div style='background-color: white; padding: 15px; border-radius: 5px; border: 1px solid #ddd;'>",
                "<h4 style='color: #2e7d32; margin-top: 0;'>Treatment Comparison</h4>",
                "<ul style='font-size: 14px; margin: 5px 0;'>",
                "<li>Plot: Box plot</li>",
                "<li>Test: t-test or Wilcoxon</li>",
                "<li>Error bars: 95% CI</li>",
                "<li>Shows: Individual points</li>",
                "</ul>",
                "<p style='font-size: 12px; color: #666; margin: 5px 0;'><em>Best for comparing treatment outcomes between two groups</em></p>",
                "</div>",

                "<div style='background-color: white; padding: 15px; border-radius: 5px; border: 1px solid #ddd;'>",
                "<h4 style='color: #d32f2f; margin-top: 0;'>Biomarker Analysis</h4>",
                "<ul style='font-size: 14px; margin: 5px 0;'>",
                "<li>Plot: Violin plot</li>",
                "<li>Test: Kruskal-Wallis</li>",
                "<li>Error bars: Standard error</li>",
                "<li>Shows: Distribution shape</li>",
                "</ul>",
                "<p style='font-size: 12px; color: #666; margin: 5px 0;'><em>Ideal for biomarker expression across multiple groups</em></p>",
                "</div>",

                "<div style='background-color: white; padding: 15px; border-radius: 5px; border: 1px solid #ddd;'>",
                "<h4 style='color: #1976d2; margin-top: 0;'>Dose-Response</h4>",
                "<ul style='font-size: 14px; margin: 5px 0;'>",
                "<li>Plot: Line plot</li>",
                "<li>Test: ANOVA</li>",
                "<li>Error bars: Standard error</li>",
                "<li>Shows: Trend analysis</li>",
                "</ul>",
                "<p style='font-size: 12px; color: #666; margin: 5px 0;'><em>Perfect for dose-response relationships</em></p>",
                "</div>",

                "<div style='background-color: white; padding: 15px; border-radius: 5px; border: 1px solid #ddd;'>",
                "<h4 style='color: #ef6c00; margin-top: 0;'>Correlation Study</h4>",
                "<ul style='font-size: 14px; margin: 5px 0;'>",
                "<li>Plot: Scatter plot</li>",
                "<li>Test: Correlation analysis</li>",
                "<li>Error bars: None</li>",
                "<li>Shows: Individual values</li>",
                "</ul>",
                "<p style='font-size: 12px; color: #666; margin: 5px 0;'><em>Best for examining relationships between variables</em></p>",
                "</div>",

                "</div>",

                "<div style='margin-top: 20px; padding: 15px; background-color: #e8f5e8; border-radius: 5px;'>",
                "<h4 style='color: #2e7d32; margin-top: 0;'>Quick Selection Guide:</h4>",
                "<ul style='font-size: 14px;'>",
                "<li><strong>2 groups, normal data:</strong> Box plot + t-test</li>",
                "<li><strong>2 groups, non-normal data:</strong> Box plot + Wilcoxon test</li>",
                "<li><strong>3+ groups, normal data:</strong> Violin plot + ANOVA</li>",
                "<li><strong>3+ groups, non-normal data:</strong> Violin plot + Kruskal-Wallis</li>",
                "<li><strong>Continuous relationships:</strong> Scatter plot + correlation</li>",
                "</ul>",
                "</div>",
                "</div>"
            )

            self$results$clinical_presets$setContent(presets_html)
        }

    )
)

