#' @title Comprehensive Tidy Plots with Advanced Statistical Visualization
#' 
#' @description 
#' Create publication-ready plots using the tidyplots framework with extensive
#' customization options, statistical features, and advanced visualization capabilities.
#' This implementation provides a jamovi interface to the tidyplots package for
#' streamlined scientific plotting.
#' 
#' @details
#' This function provides comprehensive plotting capabilities including:
#' \itemize{
#'   \item Multiple plot types: points, lines, bars, boxplots, violin plots, histograms
#'   \item Advanced statistical features: means, medians, error bars, confidence intervals
#'   \item Statistical testing: p-values, significance asterisks
#'   \item Extensive color schemes: discrete, continuous, and diverging palettes
#'   \item Grouping and faceting capabilities
#'   \item Publication-ready customization options
#' }
#' 
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import tidyplots
#' @import ggplot2
#' @import dplyr
#' @import rlang

tidyplotsClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "tidyplotsClass",
    inherit = tidyplotsBase,
    private = list(
        .init = function() {
            # Initialize plot size with responsive dimensions
            self$results$plot$setSize(800, 600)
            
            # Initialize instructions
            self$results$instructions$setContent(
                self$.createInstructions()
            )
        },

        .run = function() {
            # Comprehensive input validation
            if (is.null(self$options$xvar) || is.null(self$options$yvar)) {
                self$results$instructions$setVisible(TRUE)
                return()
            }
            
            # Hide instructions when variables are selected
            self$results$instructions$setVisible(FALSE)
            
            # Validate data
            if (nrow(self$data) == 0) {
                stop('Data contains no (complete) rows')
            }
            
            # Check for required packages
            if (!requireNamespace('tidyplots', quietly = TRUE)) {
                stop('tidyplots package is required but not installed. Please install it with: install.packages("tidyplots")')
            }
            
            # Validate variable types
            private$.validateVariables()
            
            # Set dynamic plot dimensions based on content
            private$.setPlotDimensions()
        },

        .plot = function(image, ...) {
            # Comprehensive plotting function with full tidyplots support
            
            # Get data and prepare variables
            plotData <- private$.prepareData()
            if (is.null(plotData)) return(FALSE)
            
            # Extract plotting variables
            xvar <- self$options$xvar
            yvar <- self$options$yvar
            colorvar <- self$options$color
            groupvar <- self$options$group
            facetvar <- self$options$facet
            
            # Initialize base plot with comprehensive error handling
            tryCatch({
                plot <- private$.initializePlot(plotData, xvar, yvar, colorvar, groupvar)
                
                # Add main plot elements
                plot <- private$.addMainPlotElements(plot)
                
                # Add statistical elements
                plot <- private$.addStatisticalElements(plot)
                
                # Add distribution elements
                plot <- private$.addDistributionElements(plot)
                
                # Apply styling and customization
                plot <- private$.applyCustomization(plot)
                
                # Add statistical tests if requested
                plot <- private$.addStatisticalTests(plot)
                
                # Apply faceting if specified
                if (!is.null(facetvar) && facetvar != "") {
                    plot <- private$.applyFaceting(plot, facetvar)
                }
                
                # Final plot adjustments
                plot <- private$.applyFinalAdjustments(plot)
                
                print(plot)
                return(TRUE)
                
            }, error = function(e) {
                stop(paste("Error creating plot:", e$message))
            })
        },
        
        # ===================================================================
        # COMPREHENSIVE HELPER FUNCTIONS
        # ===================================================================
        
        .createInstructions = function() {
            paste0(
                "<html>",
                "<head></head>",
                "<body>",
                "<div class='instructions'>",
                "<h3>Comprehensive Tidy Plots Analysis</h3>",
                "<p>Create publication-ready plots with advanced statistical visualization capabilities.</p>",
                "<h4>Getting Started:</h4>",
                "<ol>",
                "<li><b>Required:</b> Select X and Y variables</li>",
                "<li><b>Optional:</b> Add Color, Group, or Facet variables for advanced visualization</li>",
                "<li><b>Customize:</b> Choose plot types, statistical elements, and styling options</li>",
                "</ol>",
                "<h4>Available Features:</h4>",
                "<ul>",
                "<li>Multiple plot types: Points, Lines, Bars, Boxplots, Violin plots, Histograms</li>",
                "<li>Statistical elements: Means, medians, error bars, confidence intervals</li>",
                "<li>Statistical testing: P-values, significance indicators</li>",
                "<li>Advanced customization: Color schemes, themes, labels</li>",
                "</ul>",
                "</div>",
                "</body>",
                "</html>"
            )
        },
        
        .validateVariables = function() {
            data <- self$data
            
            # Check if variables exist in data
            if (!self$options$xvar %in% names(data)) {
                stop(paste("X variable", self$options$xvar, "not found in data"))
            }
            if (!self$options$yvar %in% names(data)) {
                stop(paste("Y variable", self$options$yvar, "not found in data"))
            }
            
            # Validate optional variables
            if (!is.null(self$options$color) && self$options$color != "" && 
                !self$options$color %in% names(data)) {
                warning(paste("Color variable", self$options$color, "not found in data"))
            }
            if (!is.null(self$options$group) && self$options$group != "" && 
                !self$options$group %in% names(data)) {
                warning(paste("Group variable", self$options$group, "not found in data"))
            }
        },
        
        .setPlotDimensions = function() {
            # Set responsive plot dimensions based on plot complexity
            base_width <- 800
            base_height <- 600
            
            # Adjust for faceting
            if (!is.null(self$options$facet) && self$options$facet != "") {
                base_width <- base_width * 1.2
                base_height <- base_height * 1.2
            }
            
            # Adjust for grouping
            if (!is.null(self$options$group) && self$options$group != "") {
                base_width <- base_width * 1.1
            }
            
            self$results$plot$setSize(base_width, base_height)
        },
        
        .prepareData = function() {
            data <- self$data
            
            # Remove rows with missing values in key variables
            key_vars <- c(self$options$xvar, self$options$yvar)
            if (!is.null(self$options$color) && self$options$color != "") {
                key_vars <- c(key_vars, self$options$color)
            }
            if (!is.null(self$options$group) && self$options$group != "") {
                key_vars <- c(key_vars, self$options$group)
            }
            if (!is.null(self$options$facet) && self$options$facet != "") {
                key_vars <- c(key_vars, self$options$facet)
            }
            
            # Filter complete cases
            complete_data <- data[complete.cases(data[, key_vars, drop = FALSE]), ]
            
            if (nrow(complete_data) == 0) {
                stop("No complete cases found for the selected variables")
            }
            
            return(complete_data)
        },
        
        .initializePlot = function(data, xvar, yvar, colorvar, groupvar) {
            # Initialize base tidyplot
            
            # Build aesthetic mapping
            aes_list <- list(
                x = rlang::sym(xvar),
                y = rlang::sym(yvar)
            )
            
            if (!is.null(colorvar) && colorvar != "") {
                aes_list$color <- rlang::sym(colorvar)
            }
            
            if (!is.null(groupvar) && groupvar != "") {
                aes_list$group <- rlang::sym(groupvar)
            }
            
            # Create base plot
            plot <- data %>%
                tidyplots::tidyplot(
                    x = aes_list$x,
                    y = aes_list$y,
                    color = aes_list$color,
                    group = aes_list$group
                )
            
            return(plot)
        },
        
        .addMainPlotElements = function(plot) {
            # Add main plot elements based on plot type
            
            plot_type <- self$options$plotType
            
            if (plot_type == 'points') {
                plot <- private$.addPointElements(plot)
            } else if (plot_type == 'line') {
                plot <- private$.addLineElements(plot)
            } else if (plot_type == 'bar') {
                plot <- private$.addBarElements(plot)
            } else if (plot_type == 'boxplot') {
                plot <- private$.addBoxplotElements(plot)
            } else if (plot_type == 'violin') {
                plot <- private$.addViolinElements(plot)
            } else if (plot_type == 'histogram') {
                plot <- private$.addHistogramElements(plot)
            } else if (plot_type == 'area') {
                plot <- private$.addAreaElements(plot)
            } else if (plot_type == 'density') {
                plot <- private$.addDensityElements(plot)
            }
            
            return(plot)
        },
        
        .addPointElements = function(plot) {
            point_style <- self$options$pointType
            
            if (point_style == 'basic') {
                plot <- plot %>% tidyplots::add_data_points()
            } else if (point_style == 'beeswarm') {
                plot <- plot %>% tidyplots::add_data_points_beeswarm()
            } else if (point_style == 'jitter') {
                plot <- plot %>% tidyplots::add_data_points_jitter()
            }
            
            return(plot)
        },
        
        .addLineElements = function(plot) {
            line_type <- self$options$lineType
            
            if (line_type == 'mean') {
                plot <- plot %>% tidyplots::add_mean_line()
            } else if (line_type == 'median') {
                plot <- plot %>% tidyplots::add_median_line()
            } else if (line_type == 'curve') {
                plot <- plot %>% tidyplots::add_curve_fit()
            } else {
                plot <- plot %>% tidyplots::add_line()
            }
            
            return(plot)
        },
        
        .addBarElements = function(plot) {
            bar_type <- self$options$barType
            
            if (bar_type == 'mean') {
                plot <- plot %>% tidyplots::add_mean_bar(alpha = 0.7)
            } else if (bar_type == 'median') {
                plot <- plot %>% tidyplots::add_median_bar(alpha = 0.7)
            } else if (bar_type == 'count') {
                plot <- plot %>% tidyplots::add_count_bar()
            }
            
            return(plot)
        },
        
        .addBoxplotElements = function(plot) {
            plot <- plot %>% tidyplots::add_boxplot()
            
            # Add optional outlier points
            if (self$options$showOutliers) {
                plot <- plot %>% tidyplots::add_data_points()
            }
            
            return(plot)
        },
        
        .addViolinElements = function(plot) {
            plot <- plot %>% tidyplots::add_violin()
            
            # Add optional data points
            if (self$options$violinPoints) {
                plot <- plot %>% tidyplots::add_data_points_beeswarm()
            }
            
            return(plot)
        },
        
        .addHistogramElements = function(plot) {
            bins <- ifelse(is.null(self$options$histogramBins) || self$options$histogramBins == 0, 
                          30, self$options$histogramBins)
            
            plot <- plot %>% tidyplots::add_histogram(bins = bins)
            
            return(plot)
        },
        
        .addAreaElements = function(plot) {
            area_type <- self$options$areaType
            
            if (area_type == 'absolute') {
                plot <- plot %>% tidyplots::add_area()
            } else if (area_type == 'relative') {
                plot <- plot %>% tidyplots::add_area_relative()
            }
            
            return(plot)
        },
        
        .addDensityElements = function(plot) {
            plot <- plot %>% tidyplots::add_density()
            return(plot)
        },
        
        .addStatisticalElements = function(plot) {
            # Add central tendency measures
            if (self$options$showMean) {
                if (self$options$meanType == 'dash') {
                    plot <- plot %>% tidyplots::add_mean_dash()
                } else if (self$options$meanType == 'dot') {
                    plot <- plot %>% tidyplots::add_mean_dot()
                } else if (self$options$meanType == 'value') {
                    plot <- plot %>% tidyplots::add_mean_value()
                }
            }
            
            if (self$options$showMedian) {
                if (self$options$medianType == 'dash') {
                    plot <- plot %>% tidyplots::add_median_dash()
                } else if (self$options$medianType == 'dot') {
                    plot <- plot %>% tidyplots::add_median_dot()
                } else if (self$options$medianType == 'value') {
                    plot <- plot %>% tidyplots::add_median_value()
                }
            }
            
            # Add error bars and confidence intervals
            if (self$options$showSEM) {
                plot <- plot %>% tidyplots::add_sem_errorbar()
            }
            
            if (self$options$showSD) {
                plot <- plot %>% tidyplots::add_sd_errorbar()
            }
            
            if (self$options$showCI) {
                if (self$options$ciType == 'errorbar') {
                    plot <- plot %>% tidyplots::add_ci95_errorbar()
                } else if (self$options$ciType == 'ribbon') {
                    plot <- plot %>% tidyplots::add_ci95_ribbon()
                }
            }
            
            if (self$options$showRange) {
                plot <- plot %>% tidyplots::add_range_errorbar()
            }
            
            return(plot)
        },
        
        .addDistributionElements = function(plot) {
            # Add distribution-related elements if requested
            
            if (self$options$showDistribution) {
                dist_type <- self$options$distributionType
                
                if (dist_type == 'density') {
                    plot <- plot %>% tidyplots::add_density()
                } else if (dist_type == 'rug') {
                    plot <- plot %>% tidyplots::add_rug()
                }
            }
            
            return(plot)
        },
        
        .applyCustomization = function(plot) {
            # Apply color schemes
            plot <- private$.applyColorScheme(plot)
            
            # Apply themes and styling
            plot <- private$.applyTheme(plot)
            
            # Apply labels and titles
            plot <- private$.applyLabels(plot)
            
            # Apply axis modifications
            plot <- private$.applyAxisModifications(plot)
            
            return(plot)
        },
        
        .applyColorScheme = function(plot) {
            color_scheme <- self$options$colorScheme
            
            # Discrete color schemes
            if (color_scheme == 'friendly') {
                plot <- plot %>% tidyplots::adjust_colors(tidyplots::colors_discrete_friendly)
            } else if (color_scheme == 'seaside') {
                plot <- plot %>% tidyplots::adjust_colors(tidyplots::colors_discrete_seaside)
            } else if (color_scheme == 'apple') {
                plot <- plot %>% tidyplots::adjust_colors(tidyplots::colors_discrete_apple)
            } else if (color_scheme == 'rainbow') {
                plot <- plot %>% tidyplots::adjust_colors(tidyplots::colors_discrete_rainbow)
            }
            # Continuous color schemes  
            else if (color_scheme == 'viridis') {
                plot <- plot %>% tidyplots::adjust_colors(tidyplots::colors_continuous_viridis)
            } else if (color_scheme == 'inferno') {
                plot <- plot %>% tidyplots::adjust_colors(tidyplots::colors_continuous_inferno)
            } else if (color_scheme == 'magma') {
                plot <- plot %>% tidyplots::adjust_colors(tidyplots::colors_continuous_magma)
            } else if (color_scheme == 'turbo') {
                plot <- plot %>% tidyplots::adjust_colors(tidyplots::colors_continuous_turbo)
            }
            # Diverging color schemes
            else if (color_scheme == 'blue2red') {
                plot <- plot %>% tidyplots::adjust_colors(tidyplots::colors_diverging_blue2red)
            } else if (color_scheme == 'blue2brown') {
                plot <- plot %>% tidyplots::adjust_colors(tidyplots::colors_diverging_blue2brown)
            }
            
            return(plot)
        },
        
        .applyTheme = function(plot) {
            # Apply theme modifications
            if (self$options$removeLegend) {
                plot <- plot %>% tidyplots::remove_legend()
            }
            
            if (self$options$removePadding) {
                plot <- plot %>% tidyplots::remove_padding()
            }
            
            # Apply font adjustments
            if (!is.null(self$options$fontSize) && self$options$fontSize > 0) {
                plot <- plot %>% tidyplots::adjust_font(size = self$options$fontSize)
            }
            
            return(plot)
        },
        
        .applyLabels = function(plot) {
            # Add title
            if (!is.null(self$options$plotTitle) && self$options$plotTitle != '') {
                plot <- plot %>% tidyplots::add_title(self$options$plotTitle)
            }
            
            # Adjust axis titles
            if (!is.null(self$options$xLabel) && self$options$xLabel != '') {
                plot <- plot %>% tidyplots::adjust_x_axis_title(self$options$xLabel)
            }
            
            if (!is.null(self$options$yLabel) && self$options$yLabel != '') {
                plot <- plot %>% tidyplots::adjust_y_axis_title(self$options$yLabel)
            }
            
            # Adjust legend title
            if (!is.null(self$options$legendTitle) && self$options$legendTitle != '') {
                plot <- plot %>% tidyplots::adjust_legend_title(self$options$legendTitle)
            }
            
            return(plot)
        },
        
        .applyAxisModifications = function(plot) {
            # X-axis modifications
            if (self$options$removeXAxis) {
                plot <- plot %>% tidyplots::remove_x_axis()
            } else {
                if (self$options$removeXAxisLabels) {
                    plot <- plot %>% tidyplots::remove_x_axis_labels()
                }
                if (self$options$removeXAxisTitle) {
                    plot <- plot %>% tidyplots::remove_x_axis_title()
                }
            }
            
            # Y-axis modifications
            if (self$options$removeYAxis) {
                plot <- plot %>% tidyplots::remove_y_axis()
            } else {
                if (self$options$removeYAxisLabels) {
                    plot <- plot %>% tidyplots::remove_y_axis_labels()
                }
                if (self$options$removeYAxisTitle) {
                    plot <- plot %>% tidyplots::remove_y_axis_title()
                }
            }
            
            return(plot)
        },
        
        .addStatisticalTests = function(plot) {
            # Add statistical testing if requested
            if (self$options$showPValue && !is.null(self$options$color) && self$options$color != "") {
                tryCatch({
                    plot <- plot %>% tidyplots::add_test_pvalue()
                }, error = function(e) {
                    warning(paste("Could not add p-value:", e$message))
                })
            }
            
            if (self$options$showSignificance && !is.null(self$options$color) && self$options$color != "") {
                tryCatch({
                    plot <- plot %>% tidyplots::add_test_asterisks()
                }, error = function(e) {
                    warning(paste("Could not add significance asterisks:", e$message))
                })
            }
            
            return(plot)
        },
        
        .applyFaceting = function(plot, facetvar) {
            # Apply faceting/splitting
            tryCatch({
                plot <- plot %>% tidyplots::split_plot(by = rlang::sym(facetvar))
            }, error = function(e) {
                warning(paste("Could not apply faceting:", e$message))
            })
            
            return(plot)
        },
        
        .applyFinalAdjustments = function(plot) {
            # Apply any final adjustments
            
            # Adjust transparency/alpha if specified
            if (!is.null(self$options$alpha) && self$options$alpha < 1) {
                # Note: This might need to be applied earlier in specific add_* functions
                # depending on the tidyplots implementation
            }
            
            # Any other final modifications can go here
            
            return(plot)
        }
    )
)
