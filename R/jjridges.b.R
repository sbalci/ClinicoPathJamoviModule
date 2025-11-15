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
    
    # Constants for validation and defaults
    private = list(
        # Clinical constants
        .MIN_SAMPLE_SIZE = 10,
        .MIN_GROUP_SIZE = 3,
        .MAX_OUTLIER_PROP = 0.05,
        .VIRIDIS_FALLBACK = c("#440154", "#414487", "#2A788E", "#22A884", "#7AD151", "#FDE725"),
        .CLINICAL_CB_SAFE_COLORS = c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", "#8c564b"),
        .init = function() {
            # Check if required variables are provided
            if (is.null(self$options$x_var) || is.null(self$options$y_var)) {
                self$results$instructions$setContent(paste0(
                    "<h3>", .("Ridge Plot Instructions"), "</h3>",
                    "<p>", .("Welcome to the Advanced Ridge Plot analysis!"), "</p>",
                    
                    "<div style='background:#f0f8ff; border-left:4px solid #2196F3; padding:15px; margin:15px 0;'>",
                        "<h4 style='color:#2196F3; margin-top:0;'>📊 ", .("Clinical Guidance"), "</h4>",
                        "<p><strong>", .("When to Use Ridge Plots:"), "</strong></p>",
                        "<ul style='margin-bottom:10px;'>",
                            "<li>", .("Compare biomarker distributions between patient groups"), "</li>",
                            "<li>", .("Visualize treatment response patterns across disease stages"), "</li>",
                            "<li>", .("Show how continuous measures vary by pathological categories"), "</li>",
                            "<li>", .("Display age or tumor size distributions by clinical characteristics"), "</li>",
                        "</ul>",
                        "<p><strong>", .("Key Considerations:"), "</strong></p>",
                        "<ul style='margin-bottom:10px;'>",
                            "<li>", .("Minimum 3-5 observations per group for reliable curves"), "</li>",
                            "<li>", .("Wide ridges = high variability; narrow ridges = consistent values"), "</li>",
                            "<li>", .("Multiple peaks may indicate distinct clinical subgroups"), "</li>",
                            "<li>", .("Compare ridge positions (left/right shift) for group differences"), "</li>",
                        "</ul>",
                    "</div>",
                    
                    "<p><strong>", .("Required:"), "</strong></p>",
                    "<ul>",
                        "<li><strong>", .("X Variable:"), "</strong> ", .("Continuous variable for distributions"), "</li>",
                        "<li><strong>", .("Y Variable:"), "</strong> ", .("Grouping variable for separate ridges"), "</li>",
                    "</ul>",
                    "<p><strong>", .("Optional:"), "</strong></p>",
                    "<ul>",
                        "<li><strong>", .("Fill Variable:"), "</strong> ", .("Color-code segments within ridges"), "</li>",
                        "<li><strong>", .("Facet Variable:"), "</strong> ", .("Create separate panels"), "</li>",
                    "</ul>",
                    "<p><strong>", .("Features:"), "</strong></p>",
                    "<ul>",
                        "<li>", .("Multiple plot types (density, histogram, gradient, violin)"), "</li>",
                        "<li>", .("Statistical overlays with p-values and effect sizes"), "</li>",
                        "<li>", .("Boxplots, quantiles, and mean/median lines"), "</li>",
                        "<li>", .("Publication-ready themes and customization"), "</li>",
                    "</ul>",
                    "<p><strong>", .("Export Options:"), "</strong></p>",
                    "<ul>",
                        "<li>", .("Adjust plot width and height in Export Options"), "</li>",
                        "<li>", .("Set DPI for high-resolution output (300 DPI recommended for publications)"), "</li>",
                        "<li>", .("Use right-click → Save Image As for quick export"), "</li>",
                    "</ul>"
                ))
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
            
            # Set plot dimensions - increase size if legends are likely to be shown
            plot_width <- self$options$width
            plot_height <- self$options$height
            
            # Increase width if fill legend is shown and legend position is right/left
            if (!is.null(self$options$fill_var) && 
                (self$options$show_fill_legend %||% TRUE) &&
                self$options$legend_position %in% c("right", "left")) {
                plot_width <- max(plot_width, 900)  # Minimum 900px when right/left legend
            }
            
            # Increase height if fill legend is shown and legend position is top/bottom
            if (!is.null(self$options$fill_var) && 
                (self$options$show_fill_legend %||% TRUE) &&
                self$options$legend_position %in% c("top", "bottom")) {
                plot_height <- max(plot_height, 650)  # Minimum 650px when top/bottom legend
            }
            
            # Increase height if faceting is used
            if (!is.null(self$options$facet_var)) {
                plot_height <- max(plot_height, 700)  # Minimum 700px when faceting
            }
            
            self$results$plot$setSize(plot_width, plot_height)
        },
        
        .validateInputs = function() {
            warnings <- c()
            
            # Check variable selection
            if (is.null(self$options$x_var)) {
                stop(.("Please select a continuous variable for X (Distribution)"))
            }
            if (is.null(self$options$y_var)) {
                stop(.("Please select a grouping variable for Y (Groups)"))
            }
            
            # Check data availability
            if (is.null(self$data) || nrow(self$data) == 0) {
                stop(.("No data available for analysis"))
            }
            
            # Check minimum sample size
            if (nrow(self$data) < private$.MIN_SAMPLE_SIZE) {
                warnings <- c(warnings, paste0(.("Sample size (n="), nrow(self$data), 
                                               .(") is below recommended minimum of "), private$.MIN_SAMPLE_SIZE, 
                                               .(" for reliable ridge plot analysis")))
            }
            
            return(warnings)
        },
        
        .validateData = function(plot_data) {
            warnings <- c()
            
            # Check for minimum group count
            n_groups <- length(unique(plot_data$y))
            if (n_groups < 2) {
                stop(.("At least 2 groups required for ridge plot comparison"))
            }
            
            # Check group sizes with clinical context
            group_counts <- table(plot_data$y)
            small_groups <- group_counts[group_counts < private$.MIN_GROUP_SIZE]
            if (length(small_groups) > 0) {
                warnings <- c(warnings, paste0(.("Groups with fewer than "), private$.MIN_GROUP_SIZE, 
                                               .(" observations may show unreliable density estimates: "), 
                                               paste(names(small_groups), collapse=", ")))
            }
            
            # Check for extreme outliers that could affect interpretation
            if (length(plot_data$x) > 0) {
                z_scores <- abs(scale(plot_data$x))
                outlier_prop <- sum(z_scores > 3, na.rm = TRUE) / length(z_scores)
                if (outlier_prop > private$.MAX_OUTLIER_PROP) {
                    warnings <- c(warnings, paste0(.("High proportion of extreme outliers detected ("), 
                                                   round(outlier_prop * 100, 1), 
                                                   .("%). Consider reviewing data quality or using robust statistical options.")))
                }
            }
            
            # Check for highly skewed distributions
            if (length(plot_data$x) > 10) {
                if (requireNamespace("moments", quietly = TRUE)) {
                    skewness <- moments::skewness(plot_data$x, na.rm = TRUE)
                } else {
                    # Fallback calculation for skewness when moments package is not available
                    x <- plot_data$x[!is.na(plot_data$x)]
                    if (length(x) > 0) {
                        m <- mean(x)
                        s <- sd(x)
                        if (s > 0) {
                            skewness <- sum(((x - m) / s)^3) / length(x)
                        } else {
                            skewness <- 0
                        }
                    } else {
                        skewness <- 0
                    }
                }
                if (abs(skewness) > 2) {
                    warnings <- c(warnings, .("Data shows high skewness. Consider log transformation if appropriate for your clinical context."))
                }
            }
            
            return(warnings)
        },
        
        .validateQuantiles = function(quantile_string) {
            tryCatch({
                # Split and trim whitespace
                vals <- as.numeric(trimws(strsplit(quantile_string, ",")[[1]]))
                
                # Check for parsing errors
                if (any(is.na(vals))) {
                    stop(.("Non-numeric values detected in quantiles"))
                }
                
                # Check valid range
                if (any(vals < 0 | vals > 1)) {
                    stop(.("Quantile values must be between 0 and 1"))
                }
                
                # Check not empty
                if (length(vals) == 0) {
                    stop(.("No quantile values provided"))
                }
                
                # Sort quantiles
                sort(vals)
                
            }, error = function(e) {
                # Log warning and use defaults
                warning(.("Invalid quantiles specified: "), e$message, .(", using defaults"))
                c(0.25, 0.5, 0.75)
            })
        },
        
        .generateClinicalSummary = function(data, has_stats = FALSE) {
            n_groups <- length(unique(data$y))
            n_total <- nrow(data)
            
            # Basic summary
            summary <- paste0(
                "<div class='clinical-summary' style='background:#f8f9fa; border-left:4px solid #007bff; padding:15px; margin:10px 0;'>",
                "<h4 style='color:#007bff; margin-top:0;'>", .("Clinical Summary"), "</h4>",
                "<p><strong>", .("Analysis:"), "</strong> ", .("Ridge plot comparing the distribution of"), " <strong>", 
                self$options$x_var, "</strong> ", .("across"), " <strong>", n_groups, " ", .("groups"), "</strong> ", .("defined by"), " <strong>", 
                self$options$y_var, "</strong>.</p>",
                "<p><strong>", .("Sample:"), "</strong> n = ", n_total, " ", .("total observations across all groups"), ".</p>"
            )
            
            # Add interpretation guidance
            summary <- paste0(summary,
                "<p><strong>", .("Interpretation Guide:"), "</strong></p>",
                "<ul style='margin-bottom:10px;'>",
                "<li><strong>", .("Ridge Shape:"), "</strong> ", .("Wider ridges = more variability; Narrow ridges = consistent values"), "</li>",
                "<li><strong>", .("Ridge Position:"), "</strong> ", .("Left/right shift indicates lower/higher average values"), "</li>",
                "<li><strong>", .("Multiple Peaks:"), "</strong> ", .("May indicate subgroups or distinct clinical phenotypes"), "</li>",
                "<li><strong>", .("Overlap:"), "</strong> ", .("Similar distributions between groups; Separation = distinct patterns"), "</li>",
                "</ul></div>"
            )
            
            return(summary)
        },

        .applyClinicalPreset = function() {
            preset <- self$options$clinicalPreset
            if (preset == "custom") {
                return()
            }

            if (preset == "biomarker_distribution") {
                self$options$plot_type <- "density_ridges"
                self$options$add_boxplot <- TRUE
                self$options$add_quantiles <- TRUE
                self$options$quantiles <- "0.25, 0.5, 0.75"
                self$options$theme_style <- "theme_pubr"

            } else if (preset == "treatment_response") {
                self$options$plot_type <- "violin_ridges"
                self$options$show_stats <- TRUE
                self$options$test_type <- "nonparametric"
                self$options$effsize_type <- "cliff_delta"
                self$options$theme_style <- "theme_pubr"

            } else if (preset == "correlation_analysis") {
                # CRITICAL FIX: This preset is not yet fully implemented
                # The double_ridges plot type currently only mirrors the same variable
                # For true correlation analysis between two biomarkers, use scatter plots or correlation matrix

                # Show prominent warning
                warning_msg <- paste0(
                    "<div style='background:#fff3cd; border-left:4px solid #ff9800; padding:15px; margin:10px 0;'>",
                    "<h4 style='color:#ff6f00; margin-top:0;'>⚠️ Feature Not Yet Implemented</h4>",
                    "<p><strong>The 'Correlation Analysis' preset is not yet fully implemented.</strong></p>",
                    "<p>The current 'double_ridges' plot only shows a mirrored version of the same variable, ",
                    "not a true two-variable correlation analysis.</p>",
                    "<p><strong>For correlation analysis between two biomarkers:</strong></p>",
                    "<ul>",
                    "<li>Use the <strong>Correlation Matrix</strong> analysis instead (jjcorrmat)</li>",
                    "<li>Or use <strong>Scatter Plot with Stats</strong> (jjscatterstats)</li>",
                    "</ul>",
                    "<p>This preset will fall back to standard density ridges for now.</p>",
                    "</div>"
                )
                self$results$warnings$setContent(warning_msg)
                self$results$warnings$setVisible(TRUE)

                # Fallback to standard density plot instead of broken double_ridges
                self$options$plot_type <- "density_ridges"
                self$options$show_stats <- TRUE
                self$options$test_type <- "parametric"
                self$options$effsize_type <- "d"
                self$options$theme_style <- "theme_pubr"
            }
        },
        
        .run = function() {
            # Check requirements
            if (is.null(self$options$x_var) || is.null(self$options$y_var))
                return()

            # Apply clinical preset if selected (must be done before other processing)
            private$.applyClinicalPreset()

            # Check package availability
            if (!requireNamespace("ggridges", quietly = TRUE)) {
                self$results$warnings$setContent(
                    paste0("<p style='color:red;'>", .("The ggridges package is required but not installed."), "</p>")
                )
                self$results$warnings$setVisible(TRUE)
                return()
            }

            # Validate inputs and collect warnings
            input_warnings <- tryCatch({
                private$.validateInputs()
            }, error = function(e) {
                self$results$warnings$setContent(paste0("<p style='color:red;'>", e$message, "</p>"))
                self$results$warnings$setVisible(TRUE)
                return(NULL)
            })
            
            # Prepare data
            private$.checkpoint()
            plot_data <- private$.prepareData()
            
            if (nrow(plot_data) == 0) {
                self$results$warnings$setContent(
                    paste0("<p style='color:orange;'>", .("No complete cases found after removing missing values."), "</p>")
                )
                self$results$warnings$setVisible(TRUE)
                return()
            }
            
            # Validate data and collect additional warnings
            data_warnings <- tryCatch({
                private$.validateData(plot_data)
            }, error = function(e) {
                self$results$warnings$setContent(paste0("<p style='color:red;'>", e$message, "</p>"))
                self$results$warnings$setVisible(TRUE)
                return(NULL)
            })
            
            # Combine and display warnings
            all_warnings <- c(input_warnings, data_warnings)
            if (length(all_warnings) > 0) {
                warning_html <- paste0(
                    "<div style='background:#fff3cd; border:1px solid #ffeaa7; padding:10px; margin:10px 0; border-radius:4px;'>",
                    "<h5 style='color:#856404; margin-top:0;'>⚠️ ", .("Clinical Data Considerations:"), "</h5>",
                    paste0("<p style='color:#856404; margin:5px 0;'>• ", all_warnings, "</p>", collapse=""),
                    "</div>"
                )
                self$results$warnings$setContent(warning_html)
                self$results$warnings$setVisible(TRUE)
            }
            
            # Generate statistics
            private$.checkpoint()
            private$.generateStatistics(plot_data)
            
            # Generate statistical tests if requested
            if (self$options$show_stats) {
                private$.checkpoint()
                private$.generateTests(plot_data)
            }
            
            # Create plot
            private$.checkpoint()
            plot <- private$.createPlot(plot_data)
            
            # Save plot state
            self$results$plot$setState(plot)
            
            # Generate interpretation
            private$.generateInterpretation(plot_data)
            
            # Set clinical summary
            clinical_summary <- private$.generateClinicalSummary(plot_data, self$options$show_stats)
            self$results$clinicalSummary$setContent(clinical_summary)
            self$results$clinicalSummary$setVisible(TRUE)
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
                # Add boxplot elements using ggplot2 geoms
                p <- p + ggplot2::geom_boxplot(
                    width = 0.1,
                    position = ggplot2::position_nudge(y = 0.15),
                    outlier.shape = NA,
                    alpha = 0.7,
                    color = "black"
                )
            }
            
            if (self$options$add_points) {
                # Add jittered points using ggplot2
                p <- p + ggplot2::geom_point(
                    alpha = self$options$point_alpha,
                    position = ggplot2::position_jitter(height = 0.1, width = 0),
                    size = 0.5
                )
            }
            
            if (self$options$add_quantiles) {
                quantiles <- private$.validateQuantiles(self$options$quantiles)
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
                show_facet_legend <- self$options$show_facet_legend %||% TRUE
                if (show_facet_legend) {
                    p <- p + facet_wrap(~ facet, scales = "free_x")
                } else {
                    p <- p + facet_wrap(~ facet, scales = "free_x") +
                        theme(strip.text = element_blank())
                }
            }
            
            # Apply theme
            p <- private$.applyTheme(p)
            
            # Apply labels
            p <- private$.applyLabels(p)
            
            # Add sample sizes if requested
            if (self$options$add_sample_size) {
                # Calculate appropriate x position (90% of x range)
                x_max <- max(data$x, na.rm = TRUE)
                x_min <- min(data$x, na.rm = TRUE)
                x_pos <- x_min + 0.9 * (x_max - x_min)
                
                sample_sizes <- data %>%
                    group_by(y) %>%
                    summarise(n = n(), .groups = 'drop') %>%
                    mutate(
                        label = paste0("n=", n),
                        x_pos = x_pos
                    )
                
                p <- p + geom_text(
                    data = sample_sizes,
                    aes(x = x_pos, y = y, label = label),
                    hjust = 0.5,
                    vjust = -0.3,
                    size = 3,
                    fontface = "bold",
                    color = "black",
                    alpha = 0.8
                )
            }
            
            # Add density peak values if requested
            if (self$options$add_density_values) {
                # Calculate density peaks for each group
                if (!is.null(self$options$fill_var)) {
                    # When fill variable exists, calculate peaks by both y and fill
                    peak_data <- data %>%
                        group_by(y, fill) %>%
                        summarise(
                            peak_x = {
                                if(length(x) > 2) {
                                    dens <- density(x, na.rm = TRUE)
                                    dens$x[which.max(dens$y)]
                                } else {
                                    mean(x, na.rm = TRUE)
                                }
                            },
                            .groups = 'drop'
                        )
                } else {
                    # When no fill variable, calculate peaks by y only
                    peak_data <- data %>%
                        group_by(y) %>%
                        summarise(
                            peak_x = {
                                if(length(x) > 2) {
                                    dens <- density(x, na.rm = TRUE)
                                    dens$x[which.max(dens$y)]
                                } else {
                                    mean(x, na.rm = TRUE)
                                }
                            },
                            .groups = 'drop'
                        )
                }
                
                p <- p + geom_text(
                    data = peak_data,
                    aes(x = peak_x, y = y, label = round(peak_x, 1)),
                    vjust = 1.2,
                    hjust = 0.5,
                    size = 2.5,
                    color = "darkred",
                    fontface = "bold",
                    alpha = 0.8
                )
            }
            
            # Add custom annotations if provided
            if (!is.null(self$options$custom_annotations) && self$options$custom_annotations != "") {
                # Parse custom annotations: format "x,y,text;x2,y2,text2"
                annotations <- strsplit(self$options$custom_annotations, ";")[[1]]
                for (ann in annotations) {
                    parts <- trimws(strsplit(ann, ",")[[1]])
                    if (length(parts) == 3) {
                        # Try to parse x and y as numeric
                        x_val <- suppressWarnings(as.numeric(parts[1]))
                        y_val <- suppressWarnings(as.numeric(parts[2]))
                        
                        if (!is.na(x_val) && !is.na(y_val)) {
                            p <- p + annotate("text", 
                                x = x_val, 
                                y = y_val, 
                                label = parts[3],
                                size = 3.5,
                                color = "blue")
                        }
                    }
                }
            }
            
            return(p)
        },
        
        .createDensityPlot = function(data) {
            p <- ggplot(data, aes(x = x, y = y))
            
            # Handle fill_ridges option
            if (self$options$fill_ridges) {
                if (!is.null(self$options$fill_var)) {
                    # Show legend based on user preference when fill variable is used
                    show_fill_legend <- self$options$show_fill_legend %||% TRUE
                    p <- p + ggridges::geom_density_ridges(
                        aes(fill = fill),
                        scale = self$options$scale,
                        alpha = self$options$alpha,
                        bandwidth = private$.getBandwidth(),
                        color = "black",
                        size = 0.5,
                        show.legend = show_fill_legend
                    )
                } else {
                    # Hide legend for y-variable coloring (redundant with y-axis)
                    p <- p + ggridges::geom_density_ridges(
                        aes(fill = y),
                        scale = self$options$scale,
                        alpha = self$options$alpha,
                        bandwidth = private$.getBandwidth(),
                        color = "black",
                        size = 0.5,
                        show.legend = FALSE
                    )
                }
                # Apply color palette for filled ridges
                p <- private$.applyColorPalette(p)
            } else {
                # Outline only - no fill
                p <- p + ggridges::geom_density_ridges(
                    scale = self$options$scale,
                    fill = NA,
                    color = "black",
                    size = 0.75,
                    bandwidth = private$.getBandwidth()
                )
            }
            
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
            
            if (self$options$fill_ridges) {
                if (!is.null(self$options$fill_var)) {
                    # Show legend based on user preference when fill variable is used
                    show_fill_legend <- self$options$show_fill_legend %||% TRUE
                    p <- p + ggridges::geom_density_ridges(
                        aes(fill = fill),
                        stat = "binline",
                        binwidth = self$options$binwidth,
                        scale = self$options$scale,
                        alpha = self$options$alpha,
                        color = "black",
                        size = 0.5,
                        show.legend = show_fill_legend
                    )
                } else {
                    p <- p + ggridges::geom_density_ridges(
                        aes(fill = y),
                        stat = "binline",
                        binwidth = self$options$binwidth,
                        scale = self$options$scale,
                        alpha = self$options$alpha,
                        color = "black",
                        size = 0.5,
                        show.legend = FALSE
                    )
                }
                # Apply color palette
                p <- private$.applyColorPalette(p)
            } else {
                # Outline only
                p <- p + ggridges::geom_density_ridges(
                    stat = "binline",
                    binwidth = self$options$binwidth,
                    scale = self$options$scale,
                    fill = NA,
                    color = "black",
                    size = 0.75
                )
            }
            
            return(p)
        },
        
        .createViolinPlot = function(data) {
            p <- ggplot(data, aes(x = x, y = y))
            
            if (!is.null(self$options$fill_var)) {
                # Show legend based on user preference when fill variable is used
                show_fill_legend <- self$options$show_fill_legend %||% TRUE
                p <- p + ggridges::geom_density_ridges(
                    aes(fill = fill, height = stat(density)),
                    scale = self$options$scale,
                    alpha = self$options$alpha,
                    show.legend = show_fill_legend,
                    bandwidth = private$.getBandwidth()
                )
            } else {
                p <- p + ggridges::geom_density_ridges(
                    aes(fill = y, height = stat(density)),
                    scale = self$options$scale,
                    alpha = self$options$alpha,
                    show.legend = FALSE,
                    bandwidth = private$.getBandwidth()
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
            
            # Determine legend title
            legend_title <- if (!is.null(self$options$fill_var)) {
                self$options$fill_var
            } else {
                self$options$y_var
            }
            
            if (palette == "custom") {
                colors <- strsplit(self$options$custom_colors, ",")[[1]]
                colors <- trimws(colors)
                p <- p + scale_fill_manual(values = colors, name = legend_title)
            } else if (palette == "clinical_colorblind") {
                # Clinical colorblind-safe palette
                p <- p + scale_fill_manual(values = private$.CLINICAL_CB_SAFE_COLORS, name = legend_title)
            } else if (palette %in% c("viridis", "plasma", "inferno", "magma")) {
                if (requireNamespace("viridis", quietly = TRUE)) {
                    p <- p + scale_fill_viridis_d(option = tolower(palette), name = legend_title)
                } else {
                    # Use constant instead of hardcoded values
                    p <- p + scale_fill_manual(values = private$.VIRIDIS_FALLBACK, name = legend_title)
                }
            } else if (palette %in% c("Set1", "Set2", "Dark2", "Paired")) {
                # Use RColorBrewer palettes
                if (requireNamespace("RColorBrewer", quietly = TRUE)) {
                    p <- p + scale_fill_brewer(palette = palette, name = legend_title)
                } else {
                    # Fallback to clinical colors
                    p <- p + scale_fill_manual(values = private$.CLINICAL_CB_SAFE_COLORS, name = legend_title)
                }
            } else {
                # Default to clinical colorblind-safe palette
                p <- p + scale_fill_manual(values = private$.CLINICAL_CB_SAFE_COLORS, name = legend_title)
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
                # Create a dark theme
                p <- p + theme_minimal() +
                    theme(
                        plot.background = element_rect(fill = "#2E2E2E", color = NA),
                        panel.background = element_rect(fill = "#2E2E2E", color = NA),
                        panel.grid.major = element_line(color = "#555555", size = 0.3),
                        panel.grid.minor = element_line(color = "#444444", size = 0.2),
                        text = element_text(color = "#E0E0E0"),
                        axis.text = element_text(color = "#E0E0E0"),
                        axis.title = element_text(color = "#E0E0E0"),
                        legend.background = element_rect(fill = "#2E2E2E", color = NA),
                        legend.text = element_text(color = "#E0E0E0"),
                        legend.title = element_text(color = "#E0E0E0")
                    )
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

            # Build grouping variables: always include y, optionally add fill and facet
            grouping_vars <- "y"
            if (!is.null(self$options$fill_var) && "fill" %in% names(data)) {
                grouping_vars <- c(grouping_vars, "fill")
            }
            if (!is.null(self$options$facet_var) && "facet" %in% names(data)) {
                grouping_vars <- c(grouping_vars, "facet")
            }

            # Calculate statistics for each combination of grouping variables
            stats <- data %>%
                group_by(across(all_of(grouping_vars))) %>%
                summarise(
                    n = n(),
                    mean = mean(x, na.rm = TRUE),
                    sd = sd(x, na.rm = TRUE),
                    median = median(x, na.rm = TRUE),
                    q1 = quantile(x, 0.25, na.rm = TRUE),
                    q3 = quantile(x, 0.75, na.rm = TRUE),
                    min = min(x, na.rm = TRUE),
                    max = max(x, na.rm = TRUE),
                    .groups = "drop"
                )

            # Populate table
            for (i in seq_len(nrow(stats))) {
                private$.checkpoint(flush = FALSE)

                # Build group label from all grouping variables
                group_label <- as.character(stats$y[i])
                if ("fill" %in% names(stats)) {
                    group_label <- paste0(group_label, " [", stats$fill[i], "]")
                }
                if ("facet" %in% names(stats)) {
                    group_label <- paste0(group_label, " (", stats$facet[i], ")")
                }

                stats_table$addRow(
                    rowKey = i,
                    values = list(
                        group = group_label,
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

        # Helper method: Perform a single statistical test with proper warnings
        .performSingleTest = function(data1, data2, group1, group2, stratum_label = "") {
            test_type <- self$options$test_type
            statistic <- NA
            p_value <- NA
            ci_lower <- NA
            ci_upper <- NA
            test_method <- ""
            warning_msg <- NULL

            if (test_type == "parametric") {
                test_result <- t.test(data1, data2)
                statistic <- test_result$statistic
                p_value <- test_result$p.value
                ci_lower <- test_result$conf.int[1]
                ci_upper <- test_result$conf.int[2]
                test_method <- "t-test"

            } else if (test_type == "nonparametric") {
                test_result <- wilcox.test(data1, data2, conf.int = TRUE)
                statistic <- test_result$statistic
                p_value <- test_result$p.value
                ci_lower <- if(!is.null(test_result$conf.int)) test_result$conf.int[1] else NA
                ci_upper <- if(!is.null(test_result$conf.int)) test_result$conf.int[2] else NA
                test_method <- "Wilcoxon"

            } else if (test_type == "robust") {
                # CRITICAL FIX: Warn if WRS2 unavailable instead of silent fallback
                if (requireNamespace("WRS2", quietly = TRUE)) {
                    test_result <- WRS2::yuen(data1, data2)
                    statistic <- test_result$test
                    p_value <- test_result$p.value
                    ci_lower <- test_result$conf.int[1]
                    ci_upper <- test_result$conf.int[2]
                    test_method <- "Yuen (robust)"
                } else {
                    # Fallback with warning
                    test_result <- t.test(data1, data2)
                    statistic <- test_result$statistic
                    p_value <- test_result$p.value
                    ci_lower <- test_result$conf.int[1]
                    ci_upper <- test_result$conf.int[2]
                    test_method <- "t-test (WRS2 unavailable)"
                    warning_msg <- paste0("⚠️ WRS2 package not available for robust test. ",
                                         "Falling back to standard t-test for comparison: ",
                                         group1, " vs ", group2,
                                         if(stratum_label != "") paste0(" (", stratum_label, ")") else "")
                }

            } else if (test_type == "bayes") {
                # CRITICAL FIX: Implement Bayesian test or warn
                if (requireNamespace("BayesFactor", quietly = TRUE)) {
                    # Use BayesFactor for Bayesian t-test
                    bf_result <- tryCatch({
                        BayesFactor::ttestBF(x = data1, y = data2)
                    }, error = function(e) NULL)

                    if (!is.null(bf_result)) {
                        statistic <- exp(bf_result@bayesFactor$bf)  # Bayes Factor
                        p_value <- NA  # Bayesian inference doesn't use p-values
                        ci_lower <- NA
                        ci_upper <- NA
                        test_method <- "Bayesian t-test"
                    } else {
                        statistic <- NA
                        p_value <- NA
                        ci_lower <- NA
                        ci_upper <- NA
                        test_method <- "Bayesian (failed)"
                        warning_msg <- paste0("⚠️ Bayesian test failed for: ", group1, " vs ", group2)
                    }
                } else {
                    statistic <- NA
                    p_value <- NA
                    ci_lower <- NA
                    ci_upper <- NA
                    test_method <- "Bayesian (unavailable)"
                    warning_msg <- paste0("⚠️ BayesFactor package not available. ",
                                         "Cannot perform Bayesian test for: ", group1, " vs ", group2)
                }

            } else {
                warning_msg <- paste0("⚠️ Unknown test type '", test_type, "' for: ", group1, " vs ", group2)
            }

            # Calculate effect size with proper CIs
            effect_result <- private$.calculateEffectSizeWithCI(data1, data2)

            # Build comparison label
            comparison_label <- paste(group1, "vs", group2)
            if (stratum_label != "") {
                comparison_label <- paste0(comparison_label, " (", stratum_label, ")")
            }

            return(list(
                comparison = comparison_label,
                statistic = statistic,
                p_value = p_value,
                ci_lower = ci_lower,
                ci_upper = ci_upper,
                effect_size = effect_result$effect_size,
                effect_ci_lower = effect_result$ci_lower,
                effect_ci_upper = effect_result$ci_upper,
                test_method = test_method,
                warning = warning_msg
            ))
        },

        # Helper method: Calculate effect size with proper confidence intervals
        .calculateEffectSizeWithCI = function(data1, data2) {
            effsize_type <- self$options$effsize_type
            effect_size <- NA
            ci_lower <- NA
            ci_upper <- NA

            n1 <- length(data1)
            n2 <- length(data2)

            if (effsize_type == "d") {
                # Cohen's d with proper CI
                pooled_sd <- sqrt(((n1 - 1) * var(data1) + (n2 - 1) * var(data2)) / (n1 + n2 - 2))
                effect_size <- (mean(data1) - mean(data2)) / pooled_sd

                # CI for Cohen's d using approximate formula
                se_d <- sqrt(((n1 + n2) / (n1 * n2)) + (effect_size^2 / (2 * (n1 + n2))))
                ci_lower <- effect_size - 1.96 * se_d
                ci_upper <- effect_size + 1.96 * se_d

            } else if (effsize_type == "g") {
                # Hedge's g with CI
                pooled_sd <- sqrt(((n1 - 1) * var(data1) + (n2 - 1) * var(data2)) / (n1 + n2 - 2))
                d <- (mean(data1) - mean(data2)) / pooled_sd
                J <- 1 - (3 / (4 * (n1 + n2 - 2) - 1))
                effect_size <- d * J

                # CI for Hedge's g
                se_g <- sqrt(((n1 + n2) / (n1 * n2)) + (effect_size^2 / (2 * (n1 + n2)))) * J
                ci_lower <- effect_size - 1.96 * se_g
                ci_upper <- effect_size + 1.96 * se_g

            } else if (effsize_type == "eta") {
                # Eta squared
                ss_between <- sum(n1 * (mean(data1) - mean(c(data1, data2)))^2 +
                                n2 * (mean(data2) - mean(c(data1, data2)))^2)
                ss_total <- sum((c(data1, data2) - mean(c(data1, data2)))^2)
                effect_size <- ss_between / ss_total

                # CI for eta-squared (approximate)
                # Using Fisher's Z transformation
                if (effect_size > 0 && effect_size < 1) {
                    z <- 0.5 * log((1 + sqrt(effect_size)) / (1 - sqrt(effect_size)))
                    se_z <- 1 / sqrt(n1 + n2 - 3)
                    z_lower <- z - 1.96 * se_z
                    z_upper <- z + 1.96 * se_z
                    ci_lower <- tanh(z_lower)^2
                    ci_upper <- tanh(z_upper)^2
                }

            } else if (effsize_type == "cliff_delta") {
                # Cliff's Delta with CI using bootstrap
                effect_size <- private$.calculateCliffsDelta(data1, data2)

                # Bootstrap CI for Cliff's Delta
                if (requireNamespace("boot", quietly = TRUE)) {
                    boot_fn <- function(data, indices) {
                        d1 <- data[indices[indices <= n1]]
                        d2 <- data[indices[indices > n1]]
                        private$.calculateCliffsDelta(d1, d2)
                    }
                    boot_result <- tryCatch({
                        boot::boot(c(data1, data2), boot_fn, R = 1000)
                    }, error = function(e) NULL)

                    if (!is.null(boot_result)) {
                        ci_result <- boot::boot.ci(boot_result, type = "perc")
                        if (!is.null(ci_result$percent)) {
                            ci_lower <- ci_result$percent[4]
                            ci_upper <- ci_result$percent[5]
                        }
                    }
                }

            } else if (effsize_type == "hodges_lehmann") {
                # Hodges-Lehmann shift (median of pairwise differences)
                effect_size <- private$.calculateHodgesLehmann(data1, data2)

                # CI from Wilcoxon test
                wilcox_result <- wilcox.test(data1, data2, conf.int = TRUE)
                if (!is.null(wilcox_result$conf.int)) {
                    ci_lower <- wilcox_result$conf.int[1]
                    ci_upper <- wilcox_result$conf.int[2]
                }

            } else if (effsize_type == "omega") {
                # CRITICAL FIX: Implement omega-squared
                # Omega-squared for two groups
                grand_mean <- mean(c(data1, data2))
                ss_between <- n1 * (mean(data1) - grand_mean)^2 + n2 * (mean(data2) - grand_mean)^2
                ss_within <- sum((data1 - mean(data1))^2) + sum((data2 - mean(data2))^2)
                ms_within <- ss_within / (n1 + n2 - 2)

                effect_size <- (ss_between - ms_within) / (ss_between + ss_within + ms_within)
                effect_size <- max(0, effect_size)  # Omega-squared can't be negative

                # CI for omega-squared (approximate using eta-squared CI method)
                if (effect_size > 0 && effect_size < 1) {
                    z <- 0.5 * log((1 + sqrt(effect_size)) / (1 - sqrt(effect_size)))
                    se_z <- 1 / sqrt(n1 + n2 - 3)
                    z_lower <- z - 1.96 * se_z
                    z_upper <- z + 1.96 * se_z
                    ci_lower <- max(0, tanh(z_lower)^2)
                    ci_upper <- min(1, tanh(z_upper)^2)
                }
            }

            return(list(
                effect_size = effect_size,
                ci_lower = ci_lower,
                ci_upper = ci_upper
            ))
        },

        # Helper method: Adjust p-values handling NA correctly
        .adjustPValues = function(p_values) {
            if (self$options$p_adjust_method == "none") {
                return(p_values)
            }

            # CRITICAL FIX: Handle NA values correctly
            # Apply adjustment only to non-NA values, preserve NA positions
            non_na_indices <- which(!is.na(p_values))

            if (length(non_na_indices) == 0) {
                # All NA, return as is
                return(p_values)
            }

            # Adjust only non-NA p-values
            adjusted <- p_values
            adjusted[non_na_indices] <- p.adjust(p_values[non_na_indices],
                                                 method = self$options$p_adjust_method)

            return(adjusted)
        },

        # Helper method: Add test row to table
        .addTestRow = function(tests_table, test_result, adjusted_p, row_key) {
            # Display warning if present
            if (!is.null(test_result$warning)) {
                current_warnings <- self$results$warnings$state
                if (is.null(current_warnings)) {
                    current_warnings <- ""
                }
                new_warning <- paste0(
                    current_warnings,
                    "<p style='color:#856404;'>", test_result$warning, "</p>"
                )
                self$results$warnings$setContent(new_warning)
                self$results$warnings$setVisible(TRUE)
            }

            tests_table$addRow(
                rowKey = row_key,
                values = list(
                    comparison = test_result$comparison,
                    statistic = test_result$statistic,
                    p_value = test_result$p_value,
                    p_adjusted = adjusted_p,
                    effect_size = test_result$effect_size,
                    ci_lower = test_result$effect_ci_lower,  # Effect size CI, not test CI
                    ci_upper = test_result$effect_ci_upper   # Effect size CI, not test CI
                )
            )
        },

        .generateTests = function(data) {
            tests_table <- self$results$tests

            # Build stratification variables (facet and fill if present)
            strata_vars <- c()
            if (!is.null(self$options$facet_var) && "facet" %in% names(data)) {
                strata_vars <- c(strata_vars, "facet")
            }
            if (!is.null(self$options$fill_var) && "fill" %in% names(data)) {
                strata_vars <- c(strata_vars, "fill")
            }

            # If there are strata, perform comparisons within each stratum
            if (length(strata_vars) > 0) {
                # Get all unique combinations of strata
                strata_combinations <- data %>%
                    select(all_of(strata_vars)) %>%
                    distinct()

                all_test_results <- list()
                all_p_values <- numeric()

                for (s in seq_len(nrow(strata_combinations))) {
                    # Filter data for this stratum
                    stratum_data <- data
                    stratum_label <- ""
                    for (sv in strata_vars) {
                        stratum_val <- strata_combinations[[sv]][s]
                        stratum_data <- stratum_data[stratum_data[[sv]] == stratum_val, ]
                        stratum_label <- paste0(stratum_label, if(stratum_label != "") " / " else "", sv, "=", stratum_val)
                    }

                    # Perform pairwise comparisons within this stratum
                    groups <- unique(stratum_data$y)
                    if (length(groups) > 1) {
                        comparisons <- combn(groups, 2, simplify = FALSE)

                        for (i in seq_along(comparisons)) {
                            private$.checkpoint(flush = FALSE)
                            group1 <- comparisons[[i]][1]
                            group2 <- comparisons[[i]][2]

                            data1 <- stratum_data$x[stratum_data$y == group1]
                            data2 <- stratum_data$x[stratum_data$y == group2]

                            # Perform test and store results with stratum label
                            test_result <- private$.performSingleTest(data1, data2, group1, group2, stratum_label)
                            all_test_results <- c(all_test_results, list(test_result))
                            all_p_values <- c(all_p_values, test_result$p_value)
                        }
                    }
                }

                # Apply p-value adjustment across all tests
                adjusted_p <- private$.adjustPValues(all_p_values)

                # Add rows to table
                for (i in seq_along(all_test_results)) {
                    private$.addTestRow(tests_table, all_test_results[[i]], adjusted_p[i], i)
                }

            } else {
                # No strata: perform simple pairwise comparisons as before
                groups <- unique(data$y)
                if (length(groups) > 1) {
                    comparisons <- combn(groups, 2, simplify = FALSE)
                    p_values <- numeric(length(comparisons))
                    test_results <- list()

                    for (i in seq_along(comparisons)) {
                        private$.checkpoint(flush = FALSE)
                        group1 <- comparisons[[i]][1]
                        group2 <- comparisons[[i]][2]

                        data1 <- data$x[data$y == group1]
                        data2 <- data$x[data$y == group2]

                        # Use helper method for single test
                        test_result <- private$.performSingleTest(data1, data2, group1, group2, "")
                        test_results[[i]] <- test_result
                        p_values[i] <- test_result$p_value
                    }

                    # Apply p-value adjustment using helper method
                    adjusted_p <- private$.adjustPValues(p_values)

                    # Add rows to table using helper method
                    for (i in seq_along(test_results)) {
                        private$.addTestRow(tests_table, test_results[[i]], adjusted_p[i], i)
                    }
                }
            }
        },
        
        .generateInterpretation = function(data) {
            # Generate clinical summary first
            clinical_summary <- private$.generateClinicalSummary(data, self$options$show_stats)
            
            # Traditional interpretation
            n_groups <- length(unique(data$y))
            n_total <- nrow(data)
            
            interpretation <- paste0(
                clinical_summary,
                "<h4>", .("Technical Interpretation"), "</h4>",
                "<p>", .("The ridge plot displays the distribution of"), " <strong>", self$options$x_var, "</strong> ",
                .("across"), " <strong>", n_groups, " ", .("groups"), "</strong> ", .("defined by"), " <strong>", self$options$y_var, "</strong>.</p>",
                "<ul>",
                "<li><strong>", .("Each ridge:"), "</strong> ", .("Shows the probability density or frequency distribution for a group"), "</li>",
                "<li><strong>", .("Overlapping areas:"), "</strong> ", .("Indicate similar value ranges between groups"), "</li>",
                "<li><strong>", .("Ridge height and spread:"), "</strong> ", .("Indicate the concentration and variability of values"), "</li>",
                "<li><strong>", .("Peaks:"), "</strong> ", .("Show the most common values (modes) within each group"), "</li>",
                "</ul>"
            )
            
            # Add plot-specific interpretations
            if (self$options$plot_type == "density_ridges_gradient") {
                interpretation <- paste0(
                    interpretation,
                    "<div style='background:#e3f2fd; padding:10px; margin:10px 0; border-radius:4px;'>",
                    "<strong>🎨 ", .("Gradient Coloring:"), "</strong> ", .("Colors represent the value of"), " ", 
                    self$options$x_var, " ", .("along each ridge, helping visualize how values are distributed within groups."), "</div>"
                )
            }
            
            if (self$options$add_boxplot) {
                interpretation <- paste0(
                    interpretation,
                    "<div style='background:#e8f5e8; padding:10px; margin:10px 0; border-radius:4px;'>",
                    "<strong>📊 ", .("Boxplots:"), "</strong> ", .("Show median (center line), quartiles (box boundaries), and outliers for each group."), " ",
                    .("Compare medians and quartile ranges across groups for clinical significance."), "</div>"
                )
            }
            
            # Add statistical context if tests were run
            if (self$options$show_stats) {
                interpretation <- paste0(
                    interpretation,
                    "<div style='background:#fff3e0; padding:10px; margin:10px 0; border-radius:4px;'>",
                    "<strong>📈 ", .("Statistical Tests:"), "</strong> ", .("Pairwise comparisons test whether group differences are statistically significant."), " ",
                    .("Consider effect sizes alongside p-values for clinical importance. Adjust for multiple comparisons when appropriate."), "</div>"
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
        },
        
        .calculateCliffsDelta = function(x, y) {
            # Cliff's Delta: proportion of pairs where x > y minus proportion where x < y
            # Perfect for lymph node count comparisons (e.g., orange-peeling vs conventional)
            n1 <- length(x)
            n2 <- length(y)
            
            greater <- 0
            less <- 0
            
            for (xi in x) {
                for (yj in y) {
                    if (xi > yj) greater <- greater + 1
                    else if (xi < yj) less <- less + 1
                }
            }
            
            delta <- (greater - less) / (n1 * n2)
            return(delta)
        },
        
        .calculateHodgesLehmann = function(x, y) {
            # Hodges-Lehmann shift: median of all pairwise differences (x - y)
            # Shows typical difference in lymph node counts between techniques
            differences <- c()
            
            for (xi in x) {
                for (yj in y) {
                    differences <- c(differences, xi - yj)
                }
            }
            
            # Return median of all pairwise differences
            return(median(differences))
        }
    )
)