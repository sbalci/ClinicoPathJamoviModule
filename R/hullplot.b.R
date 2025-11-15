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
#' @importFrom rlang .data
#' @importFrom grid unit

hullplotClass <- if (requireNamespace("jmvcore")) R6::R6Class("hullplotClass",
    inherit = hullplotBase,
    private = list(
        .prepared_data = NULL,
        .data_cache_key = NULL,

        .prepare_data = function() {
            # CRITICAL FIX: Create cache key including data CONTENT hash
            # This prevents stale data after filtering/editing
            data_size <- if (is.null(self$data)) 0 else nrow(self$data)

            # Calculate hash of relevant data columns to detect content changes
            data_hash <- ""
            if (!is.null(self$data) && data_size > 0) {
                # Select only the columns that will be used in the plot
                relevant_cols <- c(
                    self$options$x_var,
                    self$options$y_var,
                    self$options$group_var,
                    self$options$color_var,
                    self$options$size_var
                )
                relevant_cols <- relevant_cols[!sapply(relevant_cols, is.null)]
                relevant_cols <- relevant_cols[relevant_cols != ""]
                relevant_cols <- relevant_cols[relevant_cols %in% names(self$data)]

                if (length(relevant_cols) > 0) {
                    relevant_data <- self$data[, relevant_cols, drop = FALSE]
                    # Use digest for fast, reliable hashing
                    if (requireNamespace("digest", quietly = TRUE)) {
                        data_hash <- digest::digest(relevant_data, algo = "xxhash64")
                    } else {
                        # Fallback: use a simple hash based on first/last rows
                        # Not perfect but better than nothing
                        data_hash <- paste(
                            digest::digest(head(relevant_data, 10)),
                            digest::digest(tail(relevant_data, 10)),
                            sep = ":"
                        )
                    }
                }
            }

            cache_key <- paste(
                self$options$x_var,
                self$options$y_var,
                self$options$group_var,
                self$options$color_var,
                self$options$size_var,
                data_size,
                data_hash,  # ✅ Now includes actual data content
                sep = "|"
            )

            # Return cached data if key matches
            if (!is.null(private$.data_cache_key) && private$.data_cache_key == cache_key && !is.null(private$.prepared_data)) {
                return(private$.prepared_data)
            }

            # Check if required variables have been selected
            if (is.null(self$options$x_var) || is.null(self$options$y_var) || is.null(self$options$group_var) ||
                self$options$x_var == "" || self$options$y_var == "" || self$options$group_var == "") {
                return(NULL)
            }

            # Validate dataset exists and has data
            if (is.null(self$data)) {
                return(NULL)
            }

            if (nrow(self$data) == 0) {
                return(NULL)
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
            color_mapping <- group_var  # Default to group variable
            if (!is.null(color_var) && color_var != "" && color_var != group_var && color_var %in% names(dataset)) {
                plot_data[[paste0("color_", color_var)]] <- dataset[[color_var]]
                color_mapping <- paste0("color_", color_var)
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

            # Convert color mapping variable to factor if it's not the group variable
            if (color_mapping != group_var && color_mapping %in% names(plot_data)) {
                plot_data[[color_mapping]] <- as.factor(plot_data[[color_mapping]])
            }

            # Validate group sizes and add warnings
            group_counts <- table(plot_data[[group_var]])
            min_group_size <- 3
            small_groups <- names(group_counts[group_counts < min_group_size])

            # Store validation warnings for later display
            validation_warnings <- list()

            if (length(small_groups) > 0) {
                validation_warnings$small_groups <- sprintf(
                    "Groups with < %d points: %s. Hull boundaries may not be meaningful for these groups.",
                    min_group_size, paste(small_groups, collapse = ", ")
                )
            }

            if (length(levels(plot_data[[group_var]])) > 10) {
                validation_warnings$many_groups <- "More than 10 groups detected. Consider grouping similar categories for clearer visualization."
            }

            # Cache the prepared data
            prepared_data <- list(
                data = plot_data,
                x_var = x_var,
                y_var = y_var,
                group_var = group_var,
                color_mapping = color_mapping,
                size_var = size_var,
                validation_warnings = validation_warnings
            )

            private$.prepared_data <- prepared_data
            private$.data_cache_key <- cache_key

            return(prepared_data)
        },

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

            # Prepare data using cached method
            prepared <- private$.prepare_data()
            if (is.null(prepared)) {
                return()  # Variables not selected or data preparation failed
            }

            plot_data <- prepared$data
            x_var <- prepared$x_var
            y_var <- prepared$y_var
            group_var <- prepared$group_var

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

            # Generate natural language summary if requested
            if (self$options$show_summary) {
                summary_html <- private$.generate_natural_summary(plot_data, x_var, y_var, group_var)
                self$results$summary$setContent(summary_html)
            }

            # Generate assumptions guide if requested
            if (self$options$show_assumptions) {
                assumptions_html <- private$.generate_assumptions_guide()
                self$results$assumptions$setContent(assumptions_html)
            }

            # Generate interpretation guide with validation warnings
            interpretation_html <- private$.generate_interpretation_guide(plot_data, x_var, y_var, group_var, prepared$validation_warnings)
            if (!is.null(fallback_note))
                interpretation_html <- paste0(interpretation_html, fallback_note)
            self$results$interpretation$setContent(interpretation_html)

            # Set state for plot function
            self$results$plot$setState(prepared)

        },

        .plot = function(image, ggtheme, theme, ...) {

            # Get data from image state (set by .run() method)
            prepared <- image$state
            if (is.null(prepared)) {
                return(FALSE)  # No state data available
            }

            plot_data <- prepared$data
            x_var <- prepared$x_var
            y_var <- prepared$y_var
            group_var <- prepared$group_var
            color_mapping <- prepared$color_mapping
            size_var <- prepared$size_var

            # Check for concaveman package availability and adjust concavity
            concaveman_available <- requireNamespace("concaveman", quietly = TRUE)
            hull_concavity <- if (!concaveman_available && self$options$hull_concavity < 2) {
                2  # Force convex hulls when concaveman is not available
            } else {
                self$options$hull_concavity
            }

            # Create base plot
            p <- ggplot2::ggplot(plot_data, ggplot2::aes(.data[[x_var]], .data[[y_var]]))

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
                    ggplot2::aes(.data[[x_var]], .data[[y_var]], fill = .data[[group_var]], group = .data[[group_var]]),
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
                        ggplot2::aes(.data[[x_var]], .data[[y_var]], label = .data[[group_var]]),
                        fontface = "bold",
                        color = "black"
                    )
                }
            } else {
                # Add hull polygons via ggforce with proper concavity handling
                if (self$options$show_labels) {
                    p <- p + ggforce::geom_mark_hull(
                        ggplot2::aes(fill = .data[[group_var]], label = .data[[group_var]]),
                        concavity = hull_concavity,
                        expand = grid::unit(self$options$hull_expand, "mm"),
                        alpha = self$options$hull_alpha,
                        show.legend = TRUE
                    )
                } else {
                    p <- p + ggforce::geom_mark_hull(
                        ggplot2::aes(fill = .data[[group_var]]),
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
                    ggplot2::aes(color = .data[[group_var]]),
                    level = 0.95,
                    linetype = "dashed",
                    size = 0.8
                )
            }
            
            # Add points - fix aes construction
            if (!is.null(size_var) && size_var != "" && paste0("size_", size_var) %in% names(plot_data)) {
                p <- p + ggplot2::geom_point(
                    ggplot2::aes(color = .data[[color_mapping]], size = .data[[paste0("size_", size_var)]]),
                    alpha = self$options$point_alpha
                )
            } else {
                p <- p + ggplot2::geom_point(
                    ggplot2::aes(color = .data[[color_mapping]]),
                    alpha = self$options$point_alpha,
                    size = self$options$point_size
                )
            }
            
            # CRITICAL FIX: Calculate correct number of levels for each aesthetic
            # - Hulls (fill) use group_var levels
            # - Points (color) use color_mapping levels (could be different variable)
            n_groups <- length(levels(plot_data[[group_var]]))
            n_colors <- length(levels(plot_data[[color_mapping]]))

            # Generate color palettes with correct sizes
            fill_palette <- private$.get_color_palette(n_groups)
            color_palette <- if (color_mapping == group_var) {
                fill_palette  # Same variable - reuse palette
            } else {
                private$.get_color_palette(n_colors)  # Different variable - separate palette
            }

            # Apply fill scale for hulls (always based on group_var)
            p <- p + ggplot2::scale_fill_manual(
                values = fill_palette,
                name = group_var
            )

            # Apply colour scale for points (based on color_mapping)
            p <- p + ggplot2::scale_colour_manual(
                values = color_palette,
                name = if (color_mapping == group_var) group_var else self$options$color_var
            )
            
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
                color = if (color_mapping == group_var) "Groups" else self$options$color_var
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
                return(viridis::viridis(n_colors))
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
                dplyr::group_by(!!rlang::sym(group_var)) %>%
                dplyr::summarise(
                    n = dplyr::n(),
                    x_mean = round(mean(!!rlang::sym(x_var), na.rm = TRUE), 2),
                    x_sd = round(sd(!!rlang::sym(x_var), na.rm = TRUE), 2),
                    y_mean = round(mean(!!rlang::sym(y_var), na.rm = TRUE), 2),
                    y_sd = round(sd(!!rlang::sym(y_var), na.rm = TRUE), 2),
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

        .generate_interpretation_guide = function(data, x_var, y_var, group_var, validation_warnings = list()) {
            n_groups <- length(levels(data[[group_var]]))
            n_total <- nrow(data)

            # Add validation warnings if any exist
            warnings_html <- ""
            if (length(validation_warnings) > 0) {
                warnings_html <- paste0(
                    "<div style='background-color: #fff3cd; border-left: 4px solid #ffc107; padding: 15px; margin-bottom: 20px; border-radius: 4px;'>",
                    "<h4 style='color: #856404; margin-top: 0;'>⚠️ Data Quality Warnings</h4>"
                )
                for (warning in validation_warnings) {
                    warnings_html <- paste0(warnings_html, "<p style='color: #856404; margin: 5px 0;'>• ", warning, "</p>")
                }
                warnings_html <- paste0(warnings_html, "</div>")
            }

            interpretation_html <- paste0(
                warnings_html,
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
        },

        .generate_natural_summary = function(data, x_var, y_var, group_var) {
            n_groups <- length(levels(data[[group_var]]))
            n_total <- nrow(data)

            # Calculate overlap index (simplified measure)
            group_stats <- data %>%
                dplyr::group_by(!!rlang::sym(group_var)) %>%
                dplyr::summarise(
                    x_mean = mean(!!rlang::sym(x_var), na.rm = TRUE),
                    y_mean = mean(!!rlang::sym(y_var), na.rm = TRUE),
                    n = dplyr::n(),
                    .groups = 'drop'
                )

            # CRITICAL FIX: Determine separation quality (handle single-group case)
            # When only 1 group exists, dist() returns length-0 vector → mean() = NaN → crash
            if (n_groups < 2) {
                # Single group - no inter-group distances to calculate
                avg_distance <- NA
                separation_quality <- "single cohort (no comparison available)"
            } else {
                # Multiple groups - calculate distances between group means
                mean_distances <- stats::dist(group_stats[c("x_mean", "y_mean")])
                avg_distance <- mean(mean_distances)

                # Determine separation based on average distance
                separation_quality <- if (is.na(avg_distance)) {
                    "unable to calculate"
                } else if (avg_distance > 2) {
                    "well-separated"
                } else if (avg_distance > 1) {
                    "moderately separated"
                } else {
                    "overlapping"
                }
            }

            # Generate copy-ready summary
            summary_html <- paste0(
                "<div style='background-color: #e8f5e8; border-left: 4px solid #28a745; padding: 20px; margin-bottom: 20px; border-radius: 4px;'>",
                "<h3 style='color: #155724; margin-top: 0;'>📄 Natural Language Summary</h3>",

                "<div style='background-color: #ffffff; padding: 15px; border-radius: 6px; margin: 15px 0; border: 1px solid #c3e6cb;'>",
                "<h4 style='color: #155724; margin-top: 0;'>Copy-Ready Text:</h4>",
                "<p style='font-family: \"Times New Roman\", serif; line-height: 1.6; margin: 0;'>",
                "<strong>Hull plot analysis revealed ", n_groups, " distinct groups based on ", group_var, " classifications. ",
                "The visualization shows the relationship between ", x_var, " and ", y_var, " across ", n_total, " observations. ",
                "Groups appear ", separation_quality, " in the two-dimensional space, ",
                "with hull boundaries clearly delineating the extent of each group's distribution.</strong>",
                "</p>",
                "</div>",

                "<h4 style='color: #155724;'>Key Findings:</h4>",
                "<ul style='color: #155724;'>"
            )

            # Add specific findings for each group
            for (i in 1:nrow(group_stats)) {
                group_name <- group_stats[[group_var]][i]
                n_points <- group_stats$n[i]
                summary_html <- paste0(summary_html,
                    "<li><strong>", group_name, ":</strong> ", n_points, " observations (",
                    round(100 * n_points / n_total, 1), "% of total)</li>"
                )
            }

            summary_html <- paste0(summary_html,
                "</ul>",

                "<h4 style='color: #155724;'>Clinical Interpretation:</h4>",
                "<p style='color: #155724;'>",
                "Hull plots are particularly valuable for identifying patient subgroups, treatment response patterns, ",
                "and biomarker relationships. The ", separation_quality, " nature of these groups suggests ",
                if (separation_quality == "well-separated") "clear distinctions between categories that may indicate meaningful biological or clinical differences."
                else if (separation_quality == "moderately separated") "some overlap between categories, suggesting possible transitional states or shared characteristics."
                else "substantial overlap between categories, indicating either similar underlying characteristics or the need for additional discriminating variables.",
                "</p>",

                "<p style='font-size: 11px; color: #6c757d; margin-top: 20px; font-style: italic;'>",
                "This summary is generated automatically based on the hull plot visualization. ",
                "Copy the text above for use in reports, presentations, or publications.",
                "</p>",
                "</div>"
            )

            return(summary_html)
        },

        .generate_assumptions_guide = function() {
            assumptions_html <- paste0(
                "<div style='background-color: #fff8e1; border-left: 4px solid #ff9800; padding: 20px; margin-bottom: 20px; border-radius: 4px;'>",
                "<h3 style='color: #e65100; margin-top: 0;'>📋 Data Requirements & Assumptions</h3>",

                "<h4 style='color: #e65100;'>Data Requirements:</h4>",
                "<ul style='color: #e65100;'>",
                "<li><strong>X & Y Variables:</strong> Continuous numeric variables (measurements, scores, biomarker levels)</li>",
                "<li><strong>Grouping Variable:</strong> Categorical variable (treatment groups, patient types, disease stages)</li>",
                "<li><strong>Minimum Sample Size:</strong> At least 3 observations per group for meaningful hull boundaries</li>",
                "<li><strong>Complete Cases:</strong> Missing values in key variables will be excluded from analysis</li>",
                "</ul>",

                "<h4 style='color: #e65100;'>Key Assumptions:</h4>",
                "<ul style='color: #e65100;'>",
                "<li><strong>Meaningful Grouping:</strong> The grouping variable represents biologically or clinically relevant categories</li>",
                "<li><strong>Scale Appropriateness:</strong> X and Y variables are on appropriate scales for comparison</li>",
                "<li><strong>Data Independence:</strong> Observations should be independent (not repeated measures without appropriate handling)</li>",
                "<li><strong>Outlier Consideration:</strong> Extreme outliers may distort hull boundaries and interpretation</li>",
                "</ul>",

                "<h4 style='color: #e65100;'>Best Practices:</h4>",
                "<ul style='color: #e65100;'>",
                "<li><strong>Sample Size:</strong> Larger groups (n > 10) provide more stable hull boundaries</li>",
                "<li><strong>Variable Selection:</strong> Choose variables that are expected to differentiate between groups</li>",
                "<li><strong>Outlier Management:</strong> Review and investigate outliers before final interpretation</li>",
                "<li><strong>Clinical Context:</strong> Always interpret results within the specific clinical or research context</li>",
                "<li><strong>Validation:</strong> Consider complementing with statistical tests for group differences</li>",
                "</ul>",

                "<h4 style='color: #e65100;'>When Hull Plots Are Most Useful:</h4>",
                "<ul style='color: #e65100;'>",
                "<li><strong>Exploratory Analysis:</strong> Initial investigation of group patterns and relationships</li>",
                "<li><strong>Presentation:</strong> Clear visual communication of group boundaries to clinical audiences</li>",
                "<li><strong>Hypothesis Generation:</strong> Identifying potential subgroups or response patterns</li>",
                "<li><strong>Quality Control:</strong> Detecting unusual patterns or data quality issues</li>",
                "</ul>",

                "<div style='background-color: #ffe0b2; padding: 12px; border-radius: 6px; margin-top: 15px;'>",
                "<p style='color: #bf360c; margin: 0; font-weight: bold;'>",
                "⚠️ Remember: Hull plots are descriptive visualizations. For formal statistical inference about group differences, ",
                "use appropriate statistical tests (t-tests, ANOVA, MANOVA, etc.) in addition to visual exploration.",
                "</p>",
                "</div>",
                "</div>"
            )

            return(assumptions_html)
        }

    )
)
