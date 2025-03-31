#' @title River Plots
#' @importFrom R6 R6Class
#' @import jmvcore
#'

riverplotClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "riverplotClass",
    inherit = riverplotBase,
    private = list(
        # init ----
        .init = function() {
            # Set the size of the plot
            self$results$plot$setSize(800, 600)
        },

        # run ----
        .run = function() {
            # Initial message ----
            if (is.null(self$options$time) ||
                is.null(self$options$strata) ||
                length(self$options$strata) < 1) {

                # todo ----
                todo <- glue::glue(
                    "<br>Welcome to River Plots
                <br><br>
                This tool will help you generate river plots (alluvial diagrams) to visualize flows and transitions.
                <br><br>
                You need to specify:
                <br> - A time or sequence variable to define the stages
                <br> - At least one strata variable containing categories that change over time
                <br> - Optionally, an ID variable to track individual entities
                <br> - Optionally, a weight variable to determine stream width
                <br><br>
                This function uses ggplot2 and ggalluvial/easyalluvial packages.
                <br>
                Please cite jamovi and the packages as given below.
                <br><hr>"
                )

                self$results$todo$setContent(todo)
                return()

            } else {
                # todo ----
                todo <- glue::glue(
                    "<br>You have selected to create a river plot to visualize transitions and flows.<br><hr>")

                self$results$todo$setContent(todo)

                if (nrow(self$data) == 0)
                    stop('Data contains no (complete) rows')
            }
        },

        .plot = function(image, ggtheme, theme, ...) {
            # Error messages ----
            if (is.null(self$options$time) ||
                is.null(self$options$strata) ||
                length(self$options$strata) < 1)
                return()

            if (nrow(self$data) == 0)
                stop('Data contains no (complete) rows')

            # Prepare data ----
            mydata <- self$data

            # Extract options
            time_var <- self$options$time
            strata_vars <- self$options$strata
            id_var <- self$options$id
            weight_var <- self$options$weight

            # Convert to factors if needed
            mydata[[time_var]] <- as.factor(mydata[[time_var]])
            for (var in strata_vars) {
                mydata[[var]] <- as.factor(mydata[[var]])
            }

            # Handle missing values
            mydata <- jmvcore::naOmit(mydata)

            # Process other options
            plot_type <- self$options$plotType
            fill_type <- self$options$fillType
            sort_streams <- self$options$sortStreams
            label_nodes <- self$options$labelNodes
            curve_type <- self$options$curveType
            show_counts <- self$options$showCounts
            show_legend <- self$options$showLegend

            # Title and labels
            mytitle <- self$options$mytitle
            if (mytitle == '') mytitle <- NULL

            xtitle <- self$options$xtitle
            if (xtitle == '') xtitle <- NULL

            ytitle <- self$options$ytitle
            if (ytitle == '') ytitle <- NULL

            # Create appropriate river plot based on the selected type
            if (plot_type == "alluvial") {
                # Create the plot using ggalluvial

                # Determine if we need to reshape the data
                if (length(strata_vars) == 1) {
                    # A single strata variable, likely in long format already

                    # Create frequency data with stats
                    if (is.null(weight_var)) {
                        # No weight variable, use counts
                        alluvial_data <- mydata %>%
                            dplyr::group_by_at(c(time_var, strata_vars)) %>%
                            dplyr::summarize(freq = dplyr::n(), .groups = 'drop')
                    } else {
                        # Use weight variable
                        alluvial_data <- mydata %>%
                            dplyr::group_by_at(c(time_var, strata_vars)) %>%
                            dplyr::summarize(freq = sum(!!rlang::sym(weight_var)), .groups = 'drop')
                    }

                    # Create the base plot
                    plot <- ggplot(alluvial_data,
                                   aes(x = !!rlang::sym(time_var),
                                       y = freq,
                                       alluvium = !!rlang::sym(id_var),
                                       stratum = !!rlang::sym(strata_vars),
                                       fill = !!rlang::sym(strata_vars),
                                       label = !!rlang::sym(strata_vars))) +
                        ggalluvial::geom_alluvium(aes(fill = !!rlang::sym(strata_vars)),
                                                  alpha = 0.8,
                                                  curve_type = curve_type) +
                        ggalluvial::geom_stratum(width = 1/3, alpha = 0.8) +
                        scale_x_discrete(position = "top")

                    # Add labels if requested
                    if (label_nodes) {
                        plot <- plot + ggalluvial::geom_text(stat = "stratum",
                                                             position = "center",
                                                             aes(label = !!rlang::sym(strata_vars)))
                    }

                } else {
                    # Multiple strata variables, likely in wide format
                    # Create the plot with multiple strata
                    var_names <- strata_vars

                    if (is.null(weight_var)) {
                        # Set up the aesthetics
                        plot <- ggplot(mydata,
                                       aes_string(axis1 = var_names[1],
                                                  axis2 = var_names[2]))

                        # Add more axes if needed
                        if (length(var_names) > 2) {
                            for (i in 3:length(var_names)) {
                                plot <- plot + ggalluvial::geom_alluvium(aes_string(axis = var_names[i]))
                            }
                        }

                        # Complete the plot
                        plot <- plot +
                            ggalluvial::geom_alluvium(aes_string(fill = if(fill_type == "first") var_names[1]
                                                                 else if(fill_type == "last") var_names[length(var_names)]
                                                                 else "after_stat(frequency)"),
                                                      alpha = 0.8,
                                                      curve_type = curve_type) +
                            ggalluvial::geom_stratum(width = 1/3, alpha = 0.8)

                        # Add labels if requested
                        if (label_nodes) {
                            plot <- plot + ggalluvial::geom_text(stat = "stratum",
                                                                 position = "center")
                        }
                    } else {
                        # With weight variable
                        plot <- ggplot(mydata,
                                       aes_string(axis1 = var_names[1],
                                                  axis2 = var_names[2],
                                                  weight = weight_var))

                        # Add more axes if needed
                        if (length(var_names) > 2) {
                            for (i in 3:length(var_names)) {
                                plot <- plot + ggalluvial::geom_alluvium(aes_string(axis = var_names[i]))
                            }
                        }

                        # Complete the plot
                        plot <- plot +
                            ggalluvial::geom_alluvium(aes_string(fill = if(fill_type == "first") var_names[1]
                                                                 else if(fill_type == "last") var_names[length(var_names)]
                                                                 else "after_stat(frequency)"),
                                                      alpha = 0.8,
                                                      curve_type = curve_type) +
                            ggalluvial::geom_stratum(width = 1/3, alpha = 0.8)

                        # Add labels if requested
                        if (label_nodes) {
                            plot <- plot + ggalluvial::geom_text(stat = "stratum",
                                                                 position = "center")
                        }
                    }
                }

            } else if (plot_type == "sankey") {
                # Use ggalluvial but with sankey-style layout

                # Similar to alluvial but with adjustments for Sankey style
                var_names <- strata_vars

                if (is.null(weight_var)) {
                    # Set up the aesthetics for Sankey-style
                    plot <- ggplot(mydata,
                                   aes_string(axis1 = var_names[1],
                                              axis2 = var_names[2]))

                    # Add more axes if needed
                    if (length(var_names) > 2) {
                        for (i in 3:length(var_names)) {
                            plot <- plot + ggalluvial::geom_alluvium(aes_string(axis = var_names[i]))
                        }
                    }

                    # Complete the plot with Sankey-style settings
                    plot <- plot +
                        ggalluvial::geom_alluvium(aes_string(fill = if(fill_type == "first") var_names[1]
                                                             else if(fill_type == "last") var_names[length(var_names)]
                                                             else "after_stat(frequency)"),
                                                  alpha = 0.8,
                                                  curve_type = "sigmoid",
                                                  width = 1/2) +
                        ggalluvial::geom_stratum(width = 1/8, alpha = 0.8)

                    # Add labels if requested
                    if (label_nodes) {
                        plot <- plot + ggalluvial::geom_text(stat = "stratum",
                                                             position = "center")
                    }
                } else {
                    # With weight variable
                    plot <- ggplot(mydata,
                                   aes_string(axis1 = var_names[1],
                                              axis2 = var_names[2],
                                              weight = weight_var))

                    # Add more axes if needed
                    if (length(var_names) > 2) {
                        for (i in 3:length(var_names)) {
                            plot <- plot + ggalluvial::geom_alluvium(aes_string(axis = var_names[i]))
                        }
                    }

                    # Complete the plot with Sankey-style settings
                    plot <- plot +
                        ggalluvial::geom_alluvium(aes_string(fill = if(fill_type == "first") var_names[1]
                                                             else if(fill_type == "last") var_names[length(var_names)]
                                                             else "after_stat(frequency)"),
                                                  alpha = 0.8,
                                                  curve_type = "sigmoid",
                                                  width = 1/2) +
                        ggalluvial::geom_stratum(width = 1/8, alpha = 0.8)

                    # Add labels if requested
                    if (label_nodes) {
                        plot <- plot + ggalluvial::geom_text(stat = "stratum",
                                                             position = "center")
                    }
                }

            } else if (plot_type == "stream") {
                # Create a streamgraph using ggplot2 or ggstream
                # First reshape the data if needed

                if (length(strata_vars) == 1) {
                    # Data is likely already in long format
                    strata_var <- strata_vars

                    if (is.null(weight_var)) {
                        # No weight variable, use counts
                        stream_data <- mydata %>%
                            dplyr::group_by_at(c(time_var, strata_var)) %>%
                            dplyr::summarize(freq = dplyr::n(), .groups = 'drop')
                    } else {
                        # Use weight variable
                        stream_data <- mydata %>%
                            dplyr::group_by_at(c(time_var, strata_var)) %>%
                            dplyr::summarize(freq = sum(!!rlang::sym(weight_var)), .groups = 'drop')
                    }

                    # Create the streamgraph
                    plot <- ggplot(stream_data,
                                   aes(x = !!rlang::sym(time_var),
                                       y = freq,
                                       fill = !!rlang::sym(strata_var),
                                       group = !!rlang::sym(strata_var))) +
                        geom_area(position = "stack", alpha = 0.8) +
                        theme_minimal() +
                        theme(axis.text.x = element_text(angle = 45, hjust = 1))

                } else {
                    # Multiple strata variables, need to reshape to long format first
                    message <- "Stream plot with multiple strata variables requires data reshaping. Please use the Restructure option in Jamovi to convert your data to long format first."
                    stop(message)
                }
            }

            # Add titles and customize appearance
            plot <- plot +
                labs(title = mytitle,
                     x = xtitle,
                     y = ytitle)

            # Handle legend display
            if (!show_legend) {
                plot <- plot + theme(legend.position = "none")
            }

            # Apply theme
            originaltheme <- self$options$originaltheme

            if (!originaltheme) {
                plot <- plot + ggtheme
            } else {
                plot <- plot + ggstatsplot::theme_ggstatsplot()
            }

            # Display counts if requested
            if (show_counts) {
                # Logic depends on the plot type
                if (plot_type == "alluvial" || plot_type == "sankey") {
                    # Add count labels to the stratum
                    plot <- plot + ggalluvial::geom_text(stat = "stratum",
                                                         aes(label = after_stat(count)),
                                                         position = "upper")
                } else if (plot_type == "stream") {
                    # For stream plots, adding text is more complex
                    # Would need to calculate positions carefully
                    # This is a simplified approach
                    plot <- plot + geom_text(aes(label = freq),
                                             position = position_stack(vjust = 0.5))
                }
            }

            # Print the plot
            print(plot)
            TRUE
        }
    )
)
