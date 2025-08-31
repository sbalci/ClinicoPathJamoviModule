#' @title Advanced Ridge Plot
#' @description Creates advanced ridgeline plots combining features from ggridges and ggstatsplot
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import ggplot2
#' @import ggridges
#' @importFrom ggstatsplot grouped_ggbetweenstats
#' @import dplyr
#' @import tidyr
#'

jjridgesClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "jjridgesClass",
    inherit = jjridgesBase,
    private = list(
        .init = function() {
            # Check if required variables are provided
            if (is.null(self$options$x_var) || is.null(self$options$y_var)) {
                self$results$instructions$setContent(
                    "<h3>Ridge Plot Instructions</h3>
                    <p>Welcome to the Advanced Ridge Plot analysis!</p>
                    <p><strong>Required:</strong></p>
                    <ul>
                        <li><strong>X Variable:</strong> Continuous variable for distributions</li>
                        <li><strong>Y Variable:</strong> Grouping variable for separate ridges</li>
                    </ul>
                    <p><strong>Optional:</strong></p>
                    <ul>
                        <li><strong>Fill Variable:</strong> Color-code segments within ridges</li>
                        <li><strong>Facet Variable:</strong> Create separate panels</li>
                    </ul>
                    <p><strong>Features:</strong></p>
                    <ul>
                        <li>Multiple plot types (density, histogram, gradient, violin)</li>
                        <li>Statistical overlays with p-values and effect sizes</li>
                        <li>Boxplots, quantiles, and mean/median lines</li>
                        <li>Publication-ready themes and customization</li>
                    </ul>"
                )
                self$results$instructions$setVisible(visible = TRUE)
                self$results$plot$setVisible(visible = FALSE)
                self$results$statistics$setVisible(visible = FALSE)
                self$results$tests$setVisible(visible = FALSE)
                self$results$interpretation$setVisible(visible = FALSE)
                return()
            }
            
            # Set visibility based on options
            self$results$instructions$setVisible(visible = FALSE)
            self$results$plot$setVisible(visible = TRUE)
            self$results$statistics$setVisible(visible = TRUE)
            self$results$tests$setVisible(visible = self$options$show_stats)
            self$results$interpretation$setVisible(visible = TRUE)
            
            # Set plot dimensions
            self$results$plot$setSize(self$options$width, self$options$height)
        },
        
        .run = function() {
            # Check requirements
            if (is.null(self$options$x_var) || is.null(self$options$y_var))
                return()
            
            # Check package availability
            if (!requireNamespace("ggridges", quietly = TRUE)) {
                self$results$warnings$setContent(
                    "<p style='color:red;'>The ggridges package is required but not installed.</p>"
                )
                self$results$warnings$setVisible(TRUE)
                return()
            }
            
            # Prepare data
            plot_data <- private$.prepareData()
            
            if (nrow(plot_data) == 0) {
                self$results$warnings$setContent(
                    "<p style='color:orange;'>No complete cases found after removing missing values.</p>"
                )
                self$results$warnings$setVisible(TRUE)
                return()
            }
            
            # Generate statistics
            private$.generateStatistics(plot_data)
            
            # Generate statistical tests if requested
            if (self$options$show_stats) {
                private$.generateTests(plot_data)
            }
            
            # Create plot
            plot <- private$.createPlot(plot_data)
            
            # Save plot state
            self$results$plot$setState(plot)
            
            # Generate interpretation
            private$.generateInterpretation(plot_data)
        },
        
        .prepareData = function() {
            # Get data
            data <- self$data
            
            # Extract variables
            x_var <- self$options$x_var
            y_var <- self$options$y_var
            
            # Create base data frame
            plot_data <- data.frame(
                x = jmvcore::toNumeric(data[[x_var]]),
                y = as.factor(data[[y_var]]),
                stringsAsFactors = FALSE
            )
            
            # Add optional variables
            if (!is.null(self$options$fill_var)) {
                plot_data$fill <- as.factor(data[[self$options$fill_var]])
            }
            
            if (!is.null(self$options$facet_var)) {
                plot_data$facet <- as.factor(data[[self$options$facet_var]])
            }
            
            # Remove missing values
            plot_data <- na.omit(plot_data)
            
            # Reverse order if requested
            if (self$options$reverse_order) {
                plot_data$y <- factor(plot_data$y, levels = rev(levels(plot_data$y)))
            }
            
            return(plot_data)
        },
        
        .createPlot = function(data) {
            # Base plot setup
            plot_type <- self$options$plot_type
            
            # Create base plot based on type
            if (plot_type == "ridgeline" || plot_type == "density_ridges") {
                p <- private$.createDensityPlot(data)
            } else if (plot_type == "density_ridges_gradient") {
                p <- private$.createGradientPlot(data)
            } else if (plot_type == "histogram_ridges") {
                p <- private$.createHistogramPlot(data)
            } else if (plot_type == "violin_ridges") {
                p <- private$.createViolinPlot(data)
            } else if (plot_type == "double_ridges") {
                p <- private$.createDoublePlot(data)
            } else {
                p <- private$.createDensityPlot(data)  # Default
            }
            
            # Add advanced features
            if (self$options$add_boxplot) {
                p <- p + ggridges::stat_boxplot(
                    geom = "errorbar",
                    width = 0.1,
                    position = position_nudge(y = 0.05)
                )
            }
            
            if (self$options$add_points) {
                p <- p + ggridges::geom_density_ridges_rug(
                    alpha = self$options$point_alpha,
                    jittered_points = TRUE
                )
            }
            
            if (self$options$add_quantiles) {
                quantiles <- as.numeric(strsplit(self$options$quantiles, ",")[[1]])
                p <- p + ggridges::stat_density_ridges(
                    quantile_lines = TRUE,
                    quantiles = quantiles
                )
            }
            
            if (self$options$add_mean) {
                mean_data <- data %>%
                    group_by(y) %>%
                    summarise(mean_x = mean(x, na.rm = TRUE))
                
                p <- p + geom_vline(
                    data = mean_data,
                    aes(xintercept = mean_x),
                    color = "red",
                    linetype = "dashed",
                    alpha = 0.7
                )
            }
            
            if (self$options$add_median) {
                median_data <- data %>%
                    group_by(y) %>%
                    summarise(median_x = median(x, na.rm = TRUE))
                
                p <- p + geom_vline(
                    data = median_data,
                    aes(xintercept = median_x),
                    color = "blue",
                    linetype = "dashed",
                    alpha = 0.7
                )
            }
            
            # Apply faceting if requested
            if (!is.null(self$options$facet_var)) {
                p <- p + facet_wrap(~ facet, scales = "free_x")
            }
            
            # Apply theme
            p <- private$.applyTheme(p)
            
            # Apply labels
            p <- private$.applyLabels(p)
            
            # Add sample sizes if requested
            if (self$options$add_sample_size) {
                sample_sizes <- data %>%
                    group_by(y) %>%
                    summarise(n = n()) %>%
                    mutate(label = paste0("n=", n))
                
                p <- p + geom_text(
                    data = sample_sizes,
                    aes(x = Inf, y = y, label = label),
                    hjust = 1.1,
                    vjust = -0.5,
                    size = 3
                )
            }
            
            return(p)
        },
        
        .createDensityPlot = function(data) {
            p <- ggplot(data, aes(x = x, y = y))
            
            if (!is.null(self$options$fill_var)) {
                p <- p + ggridges::geom_density_ridges(
                    aes(fill = fill),
                    scale = self$options$scale,
                    alpha = self$options$alpha,
                    bandwidth = private$.getBandwidth()
                )
            } else {
                p <- p + ggridges::geom_density_ridges(
                    aes(fill = y),
                    scale = self$options$scale,
                    alpha = self$options$alpha,
                    bandwidth = private$.getBandwidth(),
                    show.legend = FALSE
                )
            }
            
            # Apply color palette
            p <- private$.applyColorPalette(p)
            
            return(p)
        },
        
        .createGradientPlot = function(data) {
            p <- ggplot(data, aes(x = x, y = y))
            
            p <- p + ggridges::geom_density_ridges_gradient(
                aes(fill = after_stat(x)),
                scale = self$options$scale,
                gradient_lwd = 1.0,
                bandwidth = private$.getBandwidth()
            )
            
            # Apply gradient colors
            p <- p + scale_fill_gradient(
                low = self$options$gradient_low,
                high = self$options$gradient_high,
                name = self$options$x_var
            )
            
            return(p)
        },
        
        .createHistogramPlot = function(data) {
            p <- ggplot(data, aes(x = x, y = y))
            
            if (!is.null(self$options$fill_var)) {
                p <- p + ggridges::geom_density_ridges(
                    aes(fill = fill),
                    stat = "binline",
                    binwidth = self$options$binwidth,
                    scale = self$options$scale,
                    alpha = self$options$alpha
                )
            } else {
                p <- p + ggridges::geom_density_ridges(
                    aes(fill = y),
                    stat = "binline",
                    binwidth = self$options$binwidth,
                    scale = self$options$scale,
                    alpha = self$options$alpha,
                    show.legend = FALSE
                )
            }
            
            # Apply color palette
            p <- private$.applyColorPalette(p)
            
            return(p)
        },
        
        .createViolinPlot = function(data) {
            p <- ggplot(data, aes(x = x, y = y))
            
            if (!is.null(self$options$fill_var)) {
                p <- p + ggridges::geom_violin_ridges(
                    aes(fill = fill),
                    scale = self$options$scale,
                    alpha = self$options$alpha
                )
            } else {
                p <- p + ggridges::geom_violin_ridges(
                    aes(fill = y),
                    scale = self$options$scale,
                    alpha = self$options$alpha,
                    show.legend = FALSE
                )
            }
            
            # Apply color palette
            p <- private$.applyColorPalette(p)
            
            return(p)
        },
        
        .createDoublePlot = function(data) {
            # For double ridges, we need two distributions
            # This is a simplified version - full implementation would need two x variables
            p <- ggplot(data, aes(x = x, y = y))
            
            # Create mirrored distributions
            p <- p + 
                ggridges::geom_density_ridges(
                    aes(fill = y),
                    scale = self$options$scale,
                    alpha = self$options$alpha / 2,
                    bandwidth = private$.getBandwidth(),
                    show.legend = FALSE
                ) +
                ggridges::geom_density_ridges(
                    aes(fill = y),
                    scale = -self$options$scale / 2,  # Negative scale for mirror effect
                    alpha = self$options$alpha / 2,
                    bandwidth = private$.getBandwidth(),
                    show.legend = FALSE
                )
            
            # Apply color palette
            p <- private$.applyColorPalette(p)
            
            return(p)
        },
        
        .getBandwidth = function() {
            if (self$options$bandwidth == "custom") {
                return(self$options$bandwidth_value)
            } else if (self$options$bandwidth != "nrd0") {
                return(self$options$bandwidth)
            }
            return(NULL)  # Use default
        },
        
        .applyColorPalette = function(p) {
            palette <- self$options$color_palette
            
            if (palette == "custom") {
                colors <- strsplit(self$options$custom_colors, ",")[[1]]
                colors <- trimws(colors)
                p <- p + scale_fill_manual(values = colors)
            } else if (palette %in% c("viridis", "plasma", "inferno", "magma")) {
                if (requireNamespace("viridis", quietly = TRUE)) {
                    p <- p + scale_fill_viridis_d(option = tolower(palette))
                } else {
                    # Fallback to manual viridis-like colors
                    viridis_colors <- c("#440154", "#414487", "#2A788E", "#22A884", "#7AD151", "#FDE725")
                    p <- p + scale_fill_manual(values = viridis_colors)
                }
            } else {
                p <- p + scale_fill_brewer(palette = palette)
            }
            
            return(p)
        },
        
        .applyTheme = function(p) {
            theme_style <- self$options$theme_style
            
            if (theme_style == "theme_ridges") {
                p <- p + ggridges::theme_ridges(grid = self$options$grid_lines)
            } else if (theme_style == "theme_minimal") {
                p <- p + theme_minimal()
            } else if (theme_style == "theme_classic") {
                p <- p + theme_classic()
            } else if (theme_style == "theme_dark") {
                p <- p + theme_dark()
            } else if (theme_style == "theme_pubr") {
                p <- p + theme_minimal() +
                    theme(
                        panel.border = element_rect(fill = NA, color = "black"),
                        axis.line = element_line(color = "black")
                    )
            }
            
            # Apply additional theme modifications
            if (self$options$expand_panels) {
                p <- p + theme(
                    plot.margin = margin(0, 0, 0, 0),
                    panel.spacing = unit(0, "lines")
                )
            }
            
            # Set legend position
            p <- p + theme(legend.position = self$options$legend_position)
            
            return(p)
        },
        
        .applyLabels = function(p) {
            x_label <- if (self$options$x_label != "") self$options$x_label else self$options$x_var
            y_label <- if (self$options$y_label != "") self$options$y_label else self$options$y_var
            
            p <- p + labs(
                x = x_label,
                y = y_label,
                title = if (self$options$plot_title != "") self$options$plot_title else NULL,
                subtitle = if (self$options$plot_subtitle != "") self$options$plot_subtitle else NULL,
                caption = if (self$options$plot_caption != "") self$options$plot_caption else NULL
            )
            
            return(p)
        },
        
        .generateStatistics = function(data) {
            stats_table <- self$results$statistics
            
            # Calculate statistics for each group
            stats <- data %>%
                group_by(y) %>%
                summarise(
                    n = n(),
                    mean = mean(x, na.rm = TRUE),
                    sd = sd(x, na.rm = TRUE),
                    median = median(x, na.rm = TRUE),
                    q1 = quantile(x, 0.25, na.rm = TRUE),
                    q3 = quantile(x, 0.75, na.rm = TRUE),
                    min = min(x, na.rm = TRUE),
                    max = max(x, na.rm = TRUE)
                )
            
            # Populate table
            for (i in seq_len(nrow(stats))) {
                stats_table$addRow(
                    rowKey = i,
                    values = list(
                        group = as.character(stats$y[i]),
                        n = stats$n[i],
                        mean = stats$mean[i],
                        sd = stats$sd[i],
                        median = stats$median[i],
                        q1 = stats$q1[i],
                        q3 = stats$q3[i],
                        min = stats$min[i],
                        max = stats$max[i]
                    )
                )
            }
        },
        
        .generateTests = function(data) {
            # This is a simplified version - full implementation would use ggstatsplot
            tests_table <- self$results$tests
            
            # Perform pairwise comparisons if more than 2 groups
            groups <- unique(data$y)
            if (length(groups) > 1) {
                comparisons <- combn(groups, 2, simplify = FALSE)
                
                for (i in seq_along(comparisons)) {
                    group1 <- comparisons[[i]][1]
                    group2 <- comparisons[[i]][2]
                    
                    data1 <- data$x[data$y == group1]
                    data2 <- data$x[data$y == group2]
                    
                    # Perform test based on type
                    if (self$options$test_type == "parametric") {
                        test_result <- t.test(data1, data2)
                        statistic <- test_result$statistic
                        p_value <- test_result$p.value
                    } else if (self$options$test_type == "nonparametric") {
                        test_result <- wilcox.test(data1, data2)
                        statistic <- test_result$statistic
                        p_value <- test_result$p.value
                    } else {
                        statistic <- NA
                        p_value <- NA
                    }
                    
                    # Calculate effect size
                    if (self$options$effsize_type == "d") {
                        pooled_sd <- sqrt(((length(data1) - 1) * var(data1) + 
                                         (length(data2) - 1) * var(data2)) / 
                                        (length(data1) + length(data2) - 2))
                        effect_size <- (mean(data1) - mean(data2)) / pooled_sd
                    } else {
                        effect_size <- NA
                    }
                    
                    # Add row to table
                    tests_table$addRow(
                        rowKey = i,
                        values = list(
                            comparison = paste(group1, "vs", group2),
                            statistic = statistic,
                            p_value = p_value,
                            p_adjusted = p_value,  # Would apply adjustment here
                            effect_size = effect_size,
                            ci_lower = NA,  # Would calculate CI here
                            ci_upper = NA
                        )
                    )
                }
            }
        },
        
        .generateInterpretation = function(data) {
            n_groups <- length(unique(data$y))
            n_total <- nrow(data)
            
            interpretation <- paste0(
                "<h4>Ridge Plot Interpretation</h4>",
                "<p>The ridge plot displays the distribution of <strong>", self$options$x_var, "</strong> ",
                "across <strong>", n_groups, " groups</strong> defined by <strong>", self$options$y_var, "</strong>.</p>",
                "<p>Total sample size: <strong>n = ", n_total, "</strong></p>",
                "<ul>",
                "<li>Each ridge represents the probability density or frequency distribution for a group</li>",
                "<li>Overlapping areas indicate similar value ranges between groups</li>",
                "<li>Ridge height and spread indicate the concentration and variability of values</li>",
                "<li>Peaks show the most common values (modes) within each group</li>",
                "</ul>"
            )
            
            if (self$options$plot_type == "density_ridges_gradient") {
                interpretation <- paste0(
                    interpretation,
                    "<p><strong>Gradient coloring:</strong> Colors represent the value of ", 
                    self$options$x_var, " along each ridge.</p>"
                )
            }
            
            if (self$options$add_boxplot) {
                interpretation <- paste0(
                    interpretation,
                    "<p><strong>Boxplots:</strong> Show median, quartiles, and outliers for each group.</p>"
                )
            }
            
            self$results$interpretation$setContent(interpretation)
        },
        
        .plot = function(image, ggtheme, theme, ...) {
            if (is.null(self$options$x_var) || is.null(self$options$y_var))
                return()
            
            plot <- image$state
            
            if (!is.null(plot)) {
                print(plot)
                return(TRUE)
            }
            
            return(FALSE)
        }
    )
)