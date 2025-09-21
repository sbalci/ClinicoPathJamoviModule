#' @title Hull Plot for Group Visualization
#' @return Hull plot using ggforce for cluster and group visualization
#'
#' @importFrom R6 R6Class
#' @import jmvcore
#' @importFrom magrittr %>%
#' @importFrom ggplot2 ggplot aes geom_point scale_color_manual scale_fill_manual
#' @importFrom ggplot2 theme_minimal theme_classic theme_light theme_dark labs
#' @importFrom ggplot2 scale_color_viridis_d scale_fill_viridis_d theme_bw stat_ellipse
#' @importFrom ggforce geom_mark_hull
#' @importFrom RColorBrewer brewer.pal
#' @importFrom scales alpha hue_pal
#' @importFrom htmltools HTML
#' @importFrom dplyr group_by summarise n
#' @importFrom viridis viridis

hullplotClass <- if (requireNamespace("jmvcore")) R6::R6Class("hullplotClass",
    inherit = hullplotBase,
    private = list(

        .run = function() {

            # Check if required variables have been selected
            if (is.null(self$options$x_var) || is.null(self$options$y_var) || is.null(self$options$group_var) ||
                self$options$x_var == "" || self$options$y_var == "" || self$options$group_var == "") {
                intro_msg <- "
                <div style='background-color: #e3f2fd; padding: 20px; border-radius: 8px; margin: 20px 0;'>
                <h3 style='color: #1976d2; margin-top: 0;'>🎯 Welcome to Hull Plot Visualization!</h3>
                <p><strong>Create stunning cluster visualizations</strong> using ggforce hull polygons</p>
                <p>Based on R-Bloggers tutorial: 'Make a Hull Plot to Visualize Clusters in ggplot2'</p>
                
                <h4 style='color: #1976d2;'>Quick Start:</h4>
                <ol>
                <li><strong>X-Axis Variable:</strong> Select a continuous variable for horizontal axis</li>
                <li><strong>Y-Axis Variable:</strong> Select a continuous variable for vertical axis</li>
                <li><strong>Grouping Variable:</strong> Choose categorical variable to define hull boundaries</li>
                <li><strong>Optional:</strong> Add color and size variables for enhanced visualization</li>
                <li><strong>Customize:</strong> Adjust hull appearance, colors, and themes</li>
                </ol>
                
                <h4 style='color: #1976d2;'>Perfect For:</h4>
                <ul>
                <li><strong>Customer Segmentation:</strong> Visualize customer groups and segments</li>
                <li><strong>Clinical Clusters:</strong> Show patient subgroups in clinical research</li>
                <li><strong>Data Exploration:</strong> Identify natural groupings in your data</li>
                <li><strong>Research Presentation:</strong> Professional publication-ready plots</li>
                </ul>
                
                <p style='font-size: 12px; color: #555; margin-top: 20px;'>
                💡 <em>Hull plots use ggforce::geom_mark_hull() to create polygonal boundaries around grouped data points</em>
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

            # Safely require ggforce and concaveman
            if (!requireNamespace("ggforce", quietly = TRUE)) {
                error_msg <- "
                <div style='color: red; background-color: #ffebee; padding: 20px; border-radius: 8px;'>
                <h4>ggforce Package Required</h4>
                <p>The ggforce package is required for hull plot functionality.</p>
                <p>Please install it using: <code>install.packages('ggforce')</code></p>
                </div>"
                self$results$interpretation$setContent(error_msg)
                return()
            }
            
            # Check for V8/concaveman availability and prepare note
            v8_available <- requireNamespace("V8", quietly = TRUE)
            concaveman_available <- requireNamespace("concaveman", quietly = TRUE)
            fallback_note <- NULL
            if (!(v8_available && concaveman_available)) {
                fallback_note <- paste0(
                    "<div style='color: #856404; background-color: #fff3cd; padding: 12px; border-radius: 6px; margin: 12px 0;'>",
                    "<strong>Concave hulls unavailable:</strong> V8/concaveman not installed. Showing convex hulls. ",
                    "Install with <code>install.packages('V8'); install.packages('concaveman')</code> for concave hulls.",
                    "</div>")
            }
            # Force convex hulls when concaveman is not available and user requested concavity < 2
            if (!concaveman_available && self$options$hull_concavity < 2)
                hull_concavity <- 2
            else
                hull_concavity <- self$options$hull_concavity

            # Get data and variables
            dataset <- self$data
            x_var <- self$options$x_var
            y_var <- self$options$y_var
            group_var <- self$options$group_var
            color_var <- self$options$color_var
            size_var <- self$options$size_var

            # Validate variable names exist in dataset
            required_vars <- c(x_var, y_var, group_var)
            missing_vars <- required_vars[!required_vars %in% names(dataset)]
            if (length(missing_vars) > 0) {
                stop(paste("Error: Variables not found in dataset:", paste(missing_vars, collapse = ", ")))
            }

            # Prepare data - create subset with required variables
            plot_data <- data.frame(
                x = dataset[[x_var]],
                y = dataset[[y_var]],
                group = dataset[[group_var]]
            )
            names(plot_data) <- c(x_var, y_var, group_var)

            # Add color variable if specified and different from group variable
            if (!is.null(color_var) && color_var != "" && color_var != group_var && color_var %in% names(dataset)) {
                plot_data[[paste0("color_", color_var)]] <- dataset[[color_var]]
            }

            # Add size variable if specified
            if (!is.null(size_var) && size_var != "" && size_var %in% names(dataset)) {
                plot_data[[paste0("size_", size_var)]] <- dataset[[size_var]]
            }

            # Remove rows with missing values in required variables
            required_cols <- c(x_var, y_var, group_var)
            plot_data <- plot_data[complete.cases(plot_data[required_cols]), ]
            
            if (nrow(plot_data) == 0) {
                stop("Error: No complete cases found for the selected variables.")
            }

            # Convert group variable to factor
            plot_data[[group_var]] <- as.factor(plot_data[[group_var]])
            
            # Generate group statistics if requested
            if (self$options$show_statistics) {
                stats_html <- private$.generate_group_statistics(plot_data, x_var, y_var, group_var)
                self$results$statistics$setContent(stats_html)
            }
            
            # Generate outlier analysis if requested
            if (self$options$outlier_detection) {
                outlier_html <- private$.generate_outlier_analysis(plot_data, x_var, y_var, group_var)
                self$results$outliers$setContent(outlier_html)
            }
            
            # Generate interpretation guide
            interpretation_html <- private$.generate_interpretation_guide(plot_data, x_var, y_var, group_var)
            if (!is.null(fallback_note))
                interpretation_html <- paste0(interpretation_html, fallback_note)
            self$results$interpretation$setContent(interpretation_html)

        },

        .plot = function(image, ggtheme, theme, ...) {
            
            # Check if required variables are selected
            if (is.null(self$options$x_var) || is.null(self$options$y_var) || is.null(self$options$group_var) ||
                self$options$x_var == "" || self$options$y_var == "" || self$options$group_var == "") {
                return()
            }

            # Get data and variables
            dataset <- self$data
            x_var <- self$options$x_var
            y_var <- self$options$y_var
            group_var <- self$options$group_var
            color_var <- self$options$color_var
            size_var <- self$options$size_var

            # Validate variable names exist in dataset
            required_vars <- c(x_var, y_var, group_var)
            missing_vars <- required_vars[!required_vars %in% names(dataset)]
            if (length(missing_vars) > 0) {
                return()  # Silently return for plot function
            }

            # Prepare data - create subset with required variables
            plot_data <- data.frame(
                x = dataset[[x_var]],
                y = dataset[[y_var]],
                group = dataset[[group_var]]
            )
            names(plot_data) <- c(x_var, y_var, group_var)

            # Add color variable if specified and different from group variable
            color_mapping <- group_var  # Default to group variable
            if (!is.null(color_var) && color_var != "" && color_var != group_var && color_var %in% names(dataset)) {
                plot_data[[paste0("color_", color_var)]] <- dataset[[color_var]]
                color_mapping <- paste0("color_", color_var)
            }

            # Add size variable if specified
            if (!is.null(size_var) && size_var != "" && size_var %in% names(dataset)) {
                plot_data[[paste0("size_", size_var)]] <- dataset[[size_var]]
            }

            # Remove missing values
            required_cols <- c(x_var, y_var, group_var)
            plot_data <- plot_data[complete.cases(plot_data[required_cols]), ]
            
            if (nrow(plot_data) == 0) {
                return()
            }

            # Convert factors
            plot_data[[group_var]] <- as.factor(plot_data[[group_var]])
            if (color_mapping != group_var && color_mapping %in% names(plot_data)) {
                plot_data[[color_mapping]] <- as.factor(plot_data[[color_mapping]])
            }

            # Check for concaveman package availability and adjust concavity
            concaveman_available <- requireNamespace("concaveman", quietly = TRUE)
            hull_concavity <- if (!concaveman_available && self$options$hull_concavity < 2) {
                2  # Force convex hulls when concaveman is not available
            } else {
                self$options$hull_concavity
            }

            # Create base plot
            p <- ggplot2::ggplot(plot_data, ggplot2::aes_string(x = x_var, y = y_var))

            # If V8/concaveman are unavailable, avoid geom_mark_hull and draw convex hulls directly
            v8_available <- requireNamespace("V8", quietly = TRUE)
            concaveman_available <- requireNamespace("concaveman", quietly = TRUE)
            use_fallback_hull <- !(v8_available && concaveman_available)

            if (use_fallback_hull) {
                # Build convex hull polygons per group using chull
                split_groups <- split(plot_data, plot_data[[group_var]])
                hull_list <- lapply(split_groups, function(df) {
                    if (nrow(df) < 3) return(df)
                    idx <- grDevices::chull(df[[x_var]], df[[y_var]])
                    df[idx, , drop = FALSE]
                })
                hull_df <- do.call(rbind, hull_list)

                p <- p + ggplot2::geom_polygon(
                    data = hull_df,
                    ggplot2::aes_string(x = x_var, y = y_var, fill = group_var, group = group_var),
                    alpha = self$options$hull_alpha,
                    color = NA
                )

                if (self$options$show_labels) {
                    # Label at group centroids
                    centroids <- stats::aggregate(
                        hull_df[c(x_var, y_var)],
                        list(group = hull_df[[group_var]]),
                        mean
                    )
                    names(centroids)[names(centroids) == "group"] <- group_var
                    p <- p + ggplot2::geom_text(
                        data = centroids,
                        ggplot2::aes_string(x = x_var, y = y_var, label = group_var),
                        fontface = "bold",
                        color = "black"
                    )
                }
            } else {
                # Add hull polygons via ggforce with proper concavity handling
                if (self$options$show_labels) {
                    p <- p + ggforce::geom_mark_hull(
                        ggplot2::aes_string(fill = group_var, label = group_var),
                        concavity = hull_concavity,
                        expand = grid::unit(self$options$hull_expand, "mm"),
                        alpha = self$options$hull_alpha,
                        show.legend = TRUE
                    )
                } else {
                    p <- p + ggforce::geom_mark_hull(
                        ggplot2::aes_string(fill = group_var),
                        concavity = hull_concavity,
                        expand = grid::unit(self$options$hull_expand, "mm"),
                        alpha = self$options$hull_alpha,
                        show.legend = TRUE
                    )
                }
            }
            
            # Add confidence ellipses if requested
            if (self$options$confidence_ellipses) {
                p <- p + ggplot2::stat_ellipse(
                    ggplot2::aes_string(color = group_var),
                    level = 0.95,
                    linetype = "dashed",
                    size = 0.8
                )
            }
            
            # Add points - fix aes construction
            if (!is.null(size_var) && size_var != "" && paste0("size_", size_var) %in% names(plot_data)) {
                p <- p + ggplot2::geom_point(
                    ggplot2::aes_string(color = color_mapping, size = paste0("size_", size_var)),
                    alpha = self$options$point_alpha
                )
            } else {
                p <- p + ggplot2::geom_point(
                    ggplot2::aes_string(color = color_mapping),
                    alpha = self$options$point_alpha,
                    size = self$options$point_size
                )
            }
            
            # Apply color palette - fix transparency application
            colors <- private$.get_color_palette(length(levels(plot_data[[group_var]])))
            p <- p + ggplot2::scale_fill_manual(values = colors)
            p <- p + ggplot2::scale_color_manual(values = colors)
            
            # Apply theme
            p <- p + private$.get_plot_theme()
            
            # Add labels
            x_label <- if (self$options$x_label != "") self$options$x_label else x_var
            y_label <- if (self$options$y_label != "") self$options$y_label else y_var
            plot_title <- if (self$options$plot_title != "") self$options$plot_title else "Hull Plot - Group Visualization"
            
            p <- p + ggplot2::labs(
                title = plot_title,
                x = x_label,
                y = y_label,
                fill = "Groups",
                color = if (color_mapping == group_var) "Groups" else color_var
            )

            # Add caption when falling back to convex hulls
            if (use_fallback_hull) {
                p <- p + ggplot2::labs(
                    caption = "Concave hulls unavailable (install V8 + concaveman); showing convex hulls"
                )
            }
            
            # Handle size legend
            if (!is.null(size_var)) {
                p <- p + ggplot2::labs(size = size_var)
            }
            
            print(p)
            TRUE
        },

        .get_color_palette = function(n_colors) {
            palette_name <- self$options$color_palette
            
            if (palette_name == "viridis") {
                return(viridis::viridis(n_colors, discrete = TRUE))
            } else if (palette_name == "clinical") {
                clinical_colors <- c("#2E86AB", "#A23B72", "#F18F01", "#C73E1D", "#593E2C", "#8E6C8A")
                return(rep(clinical_colors, length.out = n_colors))
            } else if (palette_name %in% c("set1", "set2", "dark2")) {
                palette_r_name <- switch(palette_name,
                    "set1" = "Set1",
                    "set2" = "Set2", 
                    "dark2" = "Dark2"
                )
                if (n_colors <= 8) {
                    return(RColorBrewer::brewer.pal(min(max(3, n_colors), 8), palette_r_name))
                } else {
                    base_colors <- RColorBrewer::brewer.pal(8, palette_r_name)
                    return(grDevices::colorRampPalette(base_colors)(n_colors))
                }
            } else {
                # Default ggplot2 colors
                return(scales::hue_pal()(n_colors))
            }
        },

        .get_plot_theme = function() {
            theme_name <- self$options$plot_theme
            
            switch(theme_name,
                "minimal" = ggplot2::theme_minimal(),
                "classic" = ggplot2::theme_classic(),
                "light" = ggplot2::theme_light(),
                "dark" = ggplot2::theme_dark(),
                "clinical" = ggplot2::theme_minimal() + 
                    ggplot2::theme(
                        panel.grid.minor = ggplot2::element_blank(),
                        panel.border = ggplot2::element_rect(fill = NA, color = "grey20"),
                        plot.title = ggplot2::element_text(size = 14, face = "bold"),
                        axis.title = ggplot2::element_text(size = 12),
                        legend.title = ggplot2::element_text(size = 11, face = "bold")
                    ),
                ggplot2::theme_minimal()  # fallback
            )
        },

        .generate_group_statistics = function(data, x_var, y_var, group_var) {
            # Calculate group statistics
            group_stats <- data %>%
                dplyr::group_by(rlang::.data[[group_var]]) %>%
                dplyr::summarise(
                    n = dplyr::n(),
                    x_mean = round(mean(rlang::.data[[x_var]], na.rm = TRUE), 2),
                    x_sd = round(sd(rlang::.data[[x_var]], na.rm = TRUE), 2),
                    y_mean = round(mean(rlang::.data[[y_var]], na.rm = TRUE), 2),
                    y_sd = round(sd(rlang::.data[[y_var]], na.rm = TRUE), 2),
                    .groups = 'drop'
                )
            
            # Create HTML table
            stats_html <- paste0(
                "<div style='background-color: #f8f9fa; padding: 20px; border-radius: 8px; margin-bottom: 20px;'>",
                "<h3 style='color: #495057; margin-top: 0;'>📊 Group Statistics Summary</h3>",
                "<table style='width: 100%; border-collapse: collapse; font-family: Arial, sans-serif;'>",
                "<thead><tr style='background-color: #6c757d; color: white;'>",
                "<th style='padding: 12px; border: 1px solid #dee2e6;'>Group</th>",
                "<th style='padding: 12px; border: 1px solid #dee2e6;'>N</th>",
                "<th style='padding: 12px; border: 1px solid #dee2e6;'>", x_var, " Mean ± SD</th>",
                "<th style='padding: 12px; border: 1px solid #dee2e6;'>", y_var, " Mean ± SD</th>",
                "</tr></thead><tbody>"
            )
            
            for (i in 1:nrow(group_stats)) {
                row_bg <- if (i %% 2 == 0) "#ffffff" else "#f8f9fa"
                stats_html <- paste0(stats_html,
                    "<tr style='background-color: ", row_bg, ";'>",
                    "<td style='padding: 10px; border: 1px solid #dee2e6;'><strong>", group_stats[[group_var]][i], "</strong></td>",
                    "<td style='padding: 10px; border: 1px solid #dee2e6; text-align: center;'>", group_stats$n[i], "</td>",
                    "<td style='padding: 10px; border: 1px solid #dee2e6; text-align: center;'>", group_stats$x_mean[i], " ± ", group_stats$x_sd[i], "</td>",
                    "<td style='padding: 10px; border: 1px solid #dee2e6; text-align: center;'>", group_stats$y_mean[i], " ± ", group_stats$y_sd[i], "</td>",
                    "</tr>"
                )
            }
            
            stats_html <- paste0(stats_html, 
                "</tbody></table>",
                "<p style='font-size: 12px; color: #6c757d; margin-top: 15px;'>",
                "<em>Statistics calculated for ", nrow(data), " complete observations across ", nrow(group_stats), " groups.</em>",
                "</p></div>"
            )
            
            return(stats_html)
        },

        .generate_outlier_analysis = function(data, x_var, y_var, group_var) {
            # Simple outlier detection using IQR method within groups
            outliers_list <- list()
            
            for (group in levels(data[[group_var]])) {
                group_data <- data[data[[group_var]] == group, ]
                
                # X variable outliers
                x_q1 <- quantile(group_data[[x_var]], 0.25, na.rm = TRUE)
                x_q3 <- quantile(group_data[[x_var]], 0.75, na.rm = TRUE)
                x_iqr <- x_q3 - x_q1
                x_outliers <- which(group_data[[x_var]] < (x_q1 - 1.5 * x_iqr) | group_data[[x_var]] > (x_q3 + 1.5 * x_iqr))
                
                # Y variable outliers
                y_q1 <- quantile(group_data[[y_var]], 0.25, na.rm = TRUE)
                y_q3 <- quantile(group_data[[y_var]], 0.75, na.rm = TRUE)
                y_iqr <- y_q3 - y_q1
                y_outliers <- which(group_data[[y_var]] < (y_q1 - 1.5 * y_iqr) | group_data[[y_var]] > (y_q3 + 1.5 * y_iqr))
                
                # Combined outliers
                all_outliers <- unique(c(x_outliers, y_outliers))
                outliers_list[[group]] <- length(all_outliers)
            }
            
            outlier_html <- paste0(
                "<div style='background-color: #fff3cd; padding: 20px; border-radius: 8px; margin-bottom: 20px;'>",
                "<h3 style='color: #856404; margin-top: 0;'>⚠️ Outlier Detection (IQR Method)</h3>",
                "<ul>"
            )
            
            total_outliers <- 0
            for (group in names(outliers_list)) {
                count <- outliers_list[[group]]
                total_outliers <- total_outliers + count
                outlier_html <- paste0(outlier_html,
                    "<li><strong>", group, ":</strong> ", count, " potential outliers detected</li>"
                )
            }
            
            outlier_html <- paste0(outlier_html,
                "</ul>",
                "<p><strong>Total outliers across all groups:</strong> ", total_outliers, "</p>",
                "<p style='font-size: 12px; color: #856404; margin-top: 15px;'>",
                "<em>Outliers defined as points beyond 1.5 × IQR from Q1/Q3. Consider investigating these points for data quality or interesting patterns.</em>",
                "</p></div>"
            )
            
            return(outlier_html)
        },

        .generate_interpretation_guide = function(data, x_var, y_var, group_var) {
            n_groups <- length(levels(data[[group_var]]))
            n_total <- nrow(data)
            
            interpretation_html <- paste0(
                "<div style='background-color: #d1ecf1; padding: 20px; border-radius: 8px;'>",
                "<h3 style='color: #0c5460; margin-top: 0;'>📋 Hull Plot Interpretation Guide</h3>",
                
                "<h4 style='color: #0c5460;'>Plot Summary:</h4>",
                "<ul>",
                "<li><strong>Variables:</strong> ", x_var, " (X-axis) vs ", y_var, " (Y-axis)</li>",
                "<li><strong>Groups:</strong> ", n_groups, " groups defined by ", group_var, "</li>",
                "<li><strong>Observations:</strong> ", n_total, " data points</li>",
                "</ul>",
                
                "<h4 style='color: #0c5460;'>How to Read Hull Plots:</h4>",
                "<ul>",
                "<li><strong>Hull Boundaries:</strong> Polygonal areas show the extent of each group</li>",
                "<li><strong>Overlap:</strong> Overlapping hulls indicate similar characteristics between groups</li>",
                "<li><strong>Separation:</strong> Distinct hulls suggest clear group differences</li>",
                "<li><strong>Point Density:</strong> Clustered points within hulls show group cohesion</li>",
                "</ul>",
                
                "<h4 style='color: #0c5460;'>Clinical/Research Applications:</h4>",
                "<ul>",
                "<li><strong>Patient Segmentation:</strong> Identify distinct patient subgroups</li>",
                "<li><strong>Treatment Response:</strong> Visualize how different treatments cluster</li>",
                "<li><strong>Biomarker Analysis:</strong> Show relationships between biomarkers and outcomes</li>",
                "<li><strong>Quality Control:</strong> Detect unusual patterns or outliers</li>",
                "</ul>",
                
                "<p style='font-size: 12px; color: #0c5460; margin-top: 15px;'>",
                "<em>💡 Hull plots are excellent for presentations and publications as they clearly show group boundaries and relationships.</em>",
                "</p></div>"
            )
            
            return(interpretation_html)
        }

    )
)
