#' @title Tidy Plots
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import tidyplots

tidyplotsClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "tidyplotsClass",
    inherit = tidyplotsBase,
    private = list(
        .init = function() {
            # Initialize plot size
            self$results$plot$setSize(600, 400)
        },

        .run = function() {
            # Error handling
            if (is.null(self$options$xvar) || is.null(self$options$yvar))
                return()

            if (nrow(self$data) == 0)
                stop('Data contains no (complete) rows')
        },

        .plot = function(image, ...) {
            # Get data and variables
            plotData <- self$data
            xvar <- self$options$xvar
            yvar <- self$options$yvar
            colorvar <- self$options$color

            # Start building plot
            plot <- plotData %>%
                tidyplot(
                    x = !!rlang::sym(xvar),
                    y = !!rlang::sym(yvar),
                    color = if (!is.null(colorvar)) !!rlang::sym(colorvar) else NULL
                )

            # Add plot elements based on options
            if (self$options$plotType == 'points') {
                if (self$options$pointType == 'basic') {
                    plot <- plot %>% add_data_points()
                } else if (self$options$pointType == 'beeswarm') {
                    plot <- plot %>% add_data_points_beeswarm()
                } else if (self$options$pointType == 'jitter') {
                    plot <- plot %>% add_data_points_jitter()
                }
            } else if (self$options$plotType == 'line') {
                plot <- plot %>% add_line()
            } else if (self$options$plotType == 'bar') {
                plot <- plot %>% add_mean_bar()
            } else if (self$options$plotType == 'boxplot') {
                plot <- plot %>% add_boxplot()
            }

            # Add statistical elements
            if (self$options$showMean) {
                plot <- plot %>% add_mean_dash()
                if (self$options$showSEM) {
                    plot <- plot %>% add_sem_errorbar()
                }
                if (self$options$showCI) {
                    plot <- plot %>% add_sem_ribbon()
                }
            }

            # Apply color scheme
            if (self$options$colorScheme == 'friendly') {
                plot <- plot %>% adjust_colors(colors_discrete_friendly)
            } else if (self$options$colorScheme == 'viridis') {
                plot <- plot %>% adjust_colors(colors_continuous_viridis)
            } else if (self$options$colorScheme == 'seaside') {
                plot <- plot %>% adjust_colors(colors_discrete_seaside)
            }

            # Add labels
            if (self$options$plotTitle != '') {
                plot <- plot %>% add_title(self$options$plotTitle)
            }
            if (self$options$xLabel != '') {
                plot <- plot %>% adjust_x_axis_title(self$options$xLabel)
            }
            if (self$options$yLabel != '') {
                plot <- plot %>% adjust_y_axis_title(self$options$yLabel)
            }

            print(plot)
            TRUE
        }
    )
)
