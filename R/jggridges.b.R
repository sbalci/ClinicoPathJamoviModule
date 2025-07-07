
#' @title Ridge Plot Visualization
#' @importFrom jmvcore .
#' @export

jggridgesClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "jggridgesClass",
    inherit = jggridgesBase,
    private = list(
        .init = function() {
            if (is.null(self$data) || is.null(self$options$x_var) || is.null(self$options$y_var)) {
                self$results$instructions$setContent(
                    "<h3>Ridge Plot Instructions</h3>
                    <p>To create a ridge plot, you need to specify:</p>
                    <ul>
                        <li><strong>X Variable:</strong> A continuous variable for the distribution</li>
                        <li><strong>Y Variable:</strong> A grouping variable for separate ridges</li>
                    </ul>
                    <p>Optional variables:</p>
                    <ul>
                        <li><strong>Color Variable:</strong> Variable for color mapping</li>
                        <li><strong>Facet Variable:</strong> Variable for creating separate panels</li>
                    </ul>"
                )
                self$results$instructions$setVisible(visible = TRUE)
                self$results$plot$setVisible(visible = FALSE)
                self$results$statistics$setVisible(visible = FALSE)
                self$results$interpretation$setVisible(visible = FALSE)
                return()
            }
            
            private$.checkData()
            
            self$results$instructions$setVisible(visible = FALSE)
            self$results$plot$setVisible(visible = TRUE)
            self$results$statistics$setVisible(visible = self$options$show_statistics)
            self$results$interpretation$setVisible(visible = self$options$show_interpretation)
        },
        
        .run = function() {
            if (is.null(self$data) || is.null(self$options$x_var) || is.null(self$options$y_var)) {
                return()
            }
            
            # Check if ggridges is available
            if (!requireNamespace("ggridges", quietly = TRUE)) {
                stop("The ggridges package is required but not installed.")
            }
            
            # Prepare data
            data <- private$.prepareData()
            
            # Create ridge plot
            plot <- private$.createRidgePlot(data)
            
            # Set the plot
            self$results$plot$setState(plot)
            
            # Generate statistics
            if (self$options$show_statistics) {
                stats <- private$.generateStatistics(data)
                self$results$statistics$setContent(stats)
            }
            
            # Generate interpretation
            if (self$options$show_interpretation) {
                interpretation <- private$.generateInterpretation(data)
                self$results$interpretation$setContent(interpretation)
            }
        },
        
        .prepareData = function() {
            data <- self$data
            
            # Get required variables
            x_var <- self$options$x_var
            y_var <- self$options$y_var
            
            # Create working data frame
            plot_data <- data.frame(
                x = data[[x_var]],
                y = data[[y_var]],
                stringsAsFactors = FALSE
            )
            
            # Add color variable if specified
            if (!is.null(self$options$color_var)) {
                plot_data$color <- data[[self$options$color_var]]
            }
            
            # Add facet variable if specified
            if (!is.null(self$options$facet_var)) {
                plot_data$facet <- data[[self$options$facet_var]]
            }
            
            # Remove missing values
            plot_data <- plot_data[complete.cases(plot_data), ]
            
            # Convert y to factor if needed
            if (!is.factor(plot_data$y)) {
                plot_data$y <- as.factor(plot_data$y)
            }
            
            # Reverse order if requested
            if (self$options$reverse_order) {
                plot_data$y <- factor(plot_data$y, levels = rev(levels(plot_data$y)))
            }
            
            return(plot_data)
        },
        
        .createRidgePlot = function(data) {
            # Create base plot with appropriate aesthetics
            if (!is.null(self$options$color_var) && "color" %in% names(data)) {
                p <- ggplot2::ggplot(data, ggplot2::aes(x = x, y = y, fill = color))
            } else {
                p <- ggplot2::ggplot(data, ggplot2::aes(x = x, y = y))
            }
            
            # Add ridge layer based on plot type
            p <- private$.addRidgeLayer(p, data)
            
            # Add quantile lines if requested
            if (self$options$quantile_lines) {
                p <- private$.addQuantileLines(p, data)
            }
            
            # Add jittered points if requested
            if (self$options$jittered_points) {
                p <- private$.addJitteredPoints(p, data)
            }
            
            # Add mean lines if requested
            if (self$options$show_mean) {
                p <- private$.addMeanLines(p, data)
            }
            
            # Apply theme
            p <- private$.applyTheme(p)
            
            # Apply color palette
            p <- private$.applyColorPalette(p, data)
            
            # Add faceting if specified
            if (!is.null(self$options$facet_var) && "facet" %in% names(data)) {
                p <- p + ggplot2::facet_wrap(~ facet, scales = "free")
            }
            
            # Add labels
            p <- private$.addLabels(p)
            
            return(p)
        },
        
        .addRidgeLayer = function(plot, data) {
            plot_type <- self$options$plot_type
            scale <- self$options$scale
            alpha <- self$options$alpha
            rel_min_height <- self$options$rel_min_height
            
            if (plot_type == "ridgeline") {
                plot <- plot + ggridges::geom_ridgeline(
                    scale = scale,
                    alpha = alpha,
                    rel_min_height = rel_min_height
                )
            } else if (plot_type == "density_ridges") {
                plot <- plot + ggridges::geom_density_ridges(
                    scale = scale,
                    alpha = alpha,
                    rel_min_height = rel_min_height,
                    bandwidth = private$.getBandwidth()
                )
            } else if (plot_type == "density_ridges_gradient") {
                # For gradient density ridges, we need to adjust the aesthetics
                if (!is.null(self$options$color_var)) {
                    # With color variable, use group fill
                    plot <- plot + ggridges::geom_density_ridges_gradient(
                        scale = scale,
                        alpha = alpha,
                        rel_min_height = rel_min_height,
                        bandwidth = private$.getBandwidth()
                    )
                } else {
                    # Without color variable, map density to fill
                    plot <- ggplot2::ggplot(data, ggplot2::aes(x = x, y = y, fill = stat(x))) +
                        ggridges::geom_density_ridges_gradient(
                            scale = scale,
                            alpha = alpha,
                            rel_min_height = rel_min_height,
                            bandwidth = private$.getBandwidth()
                        )
                }
            } else if (plot_type == "violin_ridges") {
                plot <- plot + ggridges::geom_violin_ridges(
                    scale = scale,
                    alpha = alpha,
                    rel_min_height = rel_min_height
                )
            }
            
            return(plot)
        },
        
        .addQuantileLines = function(plot, data) {
            quantiles_str <- self$options$quantiles
            if (!is.null(quantiles_str) && nchar(trimws(quantiles_str)) > 0) {
                # Parse comma-separated quantiles string
                quantiles <- as.numeric(unlist(strsplit(trimws(quantiles_str), "[,\\s]+")))
                quantiles <- quantiles[!is.na(quantiles)]
                
                if (length(quantiles) > 0) {
                    plot <- plot + ggridges::stat_density_ridges(
                        quantile_lines = TRUE,
                        quantiles = quantiles,
                        alpha = 0
                    )
                }
            }
            return(plot)
        },
        
        .addJitteredPoints = function(plot, data) {
            point_alpha <- self$options$point_alpha
            plot <- plot + ggridges::geom_density_ridges(
                jittered_points = TRUE,
                position = "raincloud",
                alpha = 0.7,
                point_alpha = point_alpha
            )
            return(plot)
        },
        
        .addMeanLines = function(plot, data) {
            # Calculate means by group
            mean_data <- data %>%
                dplyr::group_by(y) %>%
                dplyr::summarise(mean_x = mean(x, na.rm = TRUE), .groups = 'drop')
            
            plot <- plot + ggplot2::geom_vline(
                data = mean_data,
                ggplot2::aes(xintercept = mean_x),
                color = "red",
                linetype = "dashed",
                alpha = 0.7
            )
            
            return(plot)
        },
        
        .applyTheme = function(plot) {
            theme_style <- self$options$theme_style
            
            if (theme_style == "theme_ridges") {
                plot <- plot + ggridges::theme_ridges()
            } else if (theme_style == "theme_minimal") {
                plot <- plot + ggplot2::theme_minimal()
            } else if (theme_style == "theme_classic") {
                plot <- plot + ggplot2::theme_classic()
            } else if (theme_style == "theme_gray") {
                plot <- plot + ggplot2::theme_gray()
            } else if (theme_style == "theme_bw") {
                plot <- plot + ggplot2::theme_bw()
            }
            
            # Apply panel expansion
            if (self$options$expand_panel) {
                plot <- plot + ggplot2::theme(
                    panel.spacing = ggplot2::unit(0, "lines"),
                    strip.text = ggplot2::element_text(size = 10),
                    axis.text.y = ggplot2::element_text(size = 9)
                )
            }
            
            return(plot)
        },
        
        .applyColorPalette = function(plot, data) {
            palette <- self$options$color_palette
            plot_type <- self$options$plot_type
            
            if (plot_type == "density_ridges_gradient" && is.null(self$options$color_var)) {
                # For gradient density ridges without color variable, use continuous scale
                if (palette %in% c("viridis", "plasma", "inferno", "magma")) {
                    plot <- plot + ggplot2::scale_fill_viridis_c(option = palette)
                } else {
                    # Use gradient for other palettes
                    plot <- plot + ggplot2::scale_fill_gradient(low = "darkblue", high = "darkred")
                }
            } else if (!is.null(self$options$color_var)) {
                # For categorical color variables
                if (palette %in% c("viridis", "plasma", "inferno", "magma")) {
                    plot <- plot + ggplot2::scale_fill_viridis_d(option = palette)
                } else {
                    plot <- plot + ggplot2::scale_fill_brewer(palette = palette)
                }
            }
            
            return(plot)
        },
        
        .addLabels = function(plot) {
            # Get variable names
            x_var <- self$options$x_var
            y_var <- self$options$y_var
            
            # Set labels
            x_label <- if (self$options$x_label != "") self$options$x_label else x_var
            y_label <- if (self$options$y_label != "") self$options$y_label else y_var
            
            plot <- plot + ggplot2::labs(
                x = x_label,
                y = y_label,
                title = self$options$plot_title,
                subtitle = self$options$plot_subtitle
            )
            
            return(plot)
        },
        
        .getBandwidth = function() {
            bandwidth <- self$options$bandwidth
            if (bandwidth == "nrd0") {
                return("nrd0")
            } else if (bandwidth == "nrd") {
                return("nrd")
            } else if (bandwidth == "ucv") {
                return("ucv")
            } else if (bandwidth == "bcv") {
                return("bcv")
            } else if (bandwidth == "SJ") {
                return("SJ-ste")
            }
            return("nrd0")
        },
        
        .generateStatistics = function(data) {
            # Calculate summary statistics by group
            stats_summary <- data %>%
                dplyr::group_by(y) %>%
                dplyr::summarise(
                    n = dplyr::n(),
                    mean = round(mean(x, na.rm = TRUE), 3),
                    sd = round(stats::sd(x, na.rm = TRUE), 3),
                    median = round(stats::median(x, na.rm = TRUE), 3),
                    q25 = round(stats::quantile(x, 0.25, na.rm = TRUE), 3),
                    q75 = round(stats::quantile(x, 0.75, na.rm = TRUE), 3),
                    min = round(min(x, na.rm = TRUE), 3),
                    max = round(max(x, na.rm = TRUE), 3),
                    .groups = 'drop'
                )
            
            # Create HTML table
            html <- "<h3>Summary Statistics by Group</h3>"
            html <- paste0(html, "<table class='table table-striped'>")
            html <- paste0(html, "<thead><tr>")
            html <- paste0(html, "<th>Group</th><th>N</th><th>Mean</th><th>SD</th><th>Median</th>")
            html <- paste0(html, "<th>Q1</th><th>Q3</th><th>Min</th><th>Max</th>")
            html <- paste0(html, "</tr></thead><tbody>")
            
            for (i in seq_len(nrow(stats_summary))) {
                row <- stats_summary[i, ]
                html <- paste0(html, "<tr>")
                html <- paste0(html, "<td>", row$y, "</td>")
                html <- paste0(html, "<td>", row$n, "</td>")
                html <- paste0(html, "<td>", row$mean, "</td>")
                html <- paste0(html, "<td>", row$sd, "</td>")
                html <- paste0(html, "<td>", row$median, "</td>")
                html <- paste0(html, "<td>", row$q25, "</td>")
                html <- paste0(html, "<td>", row$q75, "</td>")
                html <- paste0(html, "<td>", row$min, "</td>")
                html <- paste0(html, "<td>", row$max, "</td>")
                html <- paste0(html, "</tr>")
            }
            
            html <- paste0(html, "</tbody></table>")
            
            return(html)
        },
        
        .generateInterpretation = function(data) {
            x_var <- self$options$x_var
            y_var <- self$options$y_var
            n_groups <- length(unique(data$y))
            total_n <- nrow(data)
            
            # Calculate overall statistics
            overall_mean <- round(mean(data$x, na.rm = TRUE), 3)
            overall_sd <- round(stats::sd(data$x, na.rm = TRUE), 3)
            
            # Calculate group statistics
            group_stats <- data %>%
                dplyr::group_by(y) %>%
                dplyr::summarise(
                    mean = mean(x, na.rm = TRUE),
                    sd = stats::sd(x, na.rm = TRUE),
                    .groups = 'drop'
                )
            
            # Find groups with highest and lowest means
            highest_group <- group_stats$y[which.max(group_stats$mean)]
            lowest_group <- group_stats$y[which.min(group_stats$mean)]
            highest_mean <- round(max(group_stats$mean), 3)
            lowest_mean <- round(min(group_stats$mean), 3)
            
            # Create interpretation
            interpretation <- paste0(
                "<h3>Ridge Plot Interpretation</h3>",
                "<p><strong>Dataset Overview:</strong> The ridge plot displays the distribution of <em>", x_var, "</em> ",
                "across ", n_groups, " groups defined by <em>", y_var, "</em>, with a total of ", total_n, " observations.</p>",
                "<p><strong>Overall Distribution:</strong> The overall mean is ", overall_mean, " (SD = ", overall_sd, ").</p>",
                "<p><strong>Group Comparisons:</strong> The group <em>", highest_group, "</em> has the highest mean value (", highest_mean, "), ",
                "while <em>", lowest_group, "</em> has the lowest mean value (", lowest_mean, ").</p>",
                "<p><strong>Visual Insights:</strong> Ridge plots are particularly useful for comparing distribution shapes, ",
                "identifying multimodal distributions, and detecting outliers across groups. Each ridge represents the ",
                "probability density of the continuous variable within each group.</p>"
            )
            
            return(interpretation)
        },
        
        .checkData = function() {
            x_var <- self$options$x_var
            y_var <- self$options$y_var
            
            if (!is.null(x_var)) {
                x_data <- self$data[[x_var]]
                if (!is.numeric(x_data)) {
                    stop("X variable must be numeric for ridge plots")
                }
            }
            
            if (!is.null(y_var)) {
                y_data <- self$data[[y_var]]
                if (length(unique(y_data)) < 2) {
                    stop("Y variable must have at least 2 groups for ridge plots")
                }
            }
        },
        
        .plot = function(image, ...) {
            if (is.null(self$data) || is.null(self$options$x_var) || is.null(self$options$y_var)) {
                return()
            }
            
            plot <- self$results$plot$state
            if (is.null(plot)) {
                return()
            }
            
            print(plot)
        }
    )
)
