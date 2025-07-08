#' @title River Plots
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import ggplot2
#' @importFrom ggalluvial geom_alluvium geom_stratum
#' @importFrom dplyr group_by summarize n %>% mutate
#' @importFrom rlang sym !!
#' @export

jjriverplotClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "jjriverplotClass",
    inherit = jjriverplotBase,
    private = list(

        # Cache for processed data and options to avoid redundant computation
        .processedData = NULL,
        .processedOptions = NULL,
        # init ----
        .init = function() {
            # Set the size of the plot
            self$results$plot$setSize(800, 600)
        },

        # Optimized data preparation with caching
        .prepareData = function(force_refresh = FALSE) {
            if (!is.null(private$.processedData) && !force_refresh) {
                return(private$.processedData)
            }

            # Prepare data with progress feedback
            self$results$todo$setContent(
                glue::glue("<br>Processing data for river plot analysis...<br><hr>")
            )

            mydata <- self$data
            
            # Extract options for processing
            time_var <- self$options$time
            strata_vars <- self$options$strata
            
            # Convert to factors if needed
            if (!is.null(time_var)) {
                mydata[[time_var]] <- as.factor(mydata[[time_var]])
            }
            
            for (var in strata_vars) {
                if (!is.null(var)) {
                    mydata[[var]] <- as.factor(mydata[[var]])
                }
            }

            # Handle missing values with checkpoint
            private$.checkpoint()
            mydata <- jmvcore::naOmit(mydata)

            # Cache the processed data
            private$.processedData <- mydata
            return(mydata)
        },

        # Optimized options preparation with caching
        .prepareOptions = function(force_refresh = FALSE) {
            if (!is.null(private$.processedOptions) && !force_refresh) {
                return(private$.processedOptions)
            }

            # Prepare options with progress feedback
            self$results$todo$setContent(
                glue::glue("<br>Preparing river plot options...<br><hr>")
            )

            # Process all options
            time_var <- self$options$time
            strata_vars <- self$options$strata
            id_var <- self$options$id
            weight_var <- self$options$weight
            plot_type <- self$options$plotType
            fill_type <- self$options$fillType
            sort_streams <- self$options$sortStreams
            label_nodes <- self$options$labelNodes
            curve_type <- self$options$curveType
            show_counts <- self$options$showCounts
            show_legend <- self$options$showLegend
            originaltheme <- self$options$originaltheme
            
            # Process titles
            mytitle <- self$options$mytitle
            if (mytitle == '') mytitle <- NULL
            
            xtitle <- self$options$xtitle
            if (xtitle == '') xtitle <- NULL
            
            ytitle <- self$options$ytitle
            if (ytitle == '') ytitle <- NULL
            
            # Cache the processed options
            options_list <- list(
                time_var = time_var,
                strata_vars = strata_vars,
                id_var = id_var,
                weight_var = weight_var,
                plot_type = plot_type,
                fill_type = fill_type,
                sort_streams = sort_streams,
                label_nodes = label_nodes,
                curve_type = curve_type,
                show_counts = show_counts,
                show_legend = show_legend,
                originaltheme = originaltheme,
                mytitle = mytitle,
                xtitle = xtitle,
                ytitle = ytitle
            )
            private$.processedOptions <- options_list
            return(options_list)
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
                This function uses ggplot2 and ggalluvial packages.
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

                # Pre-process data and options for performance
                private$.prepareData()
                private$.prepareOptions()
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
            
            # Use cached data and options for performance ----
            mydata <- private$.prepareData()
            options_data <- private$.prepareOptions()
            
            # Extract cached options
            time_var <- options_data$time_var
            strata_vars <- options_data$strata_vars
            id_var <- options_data$id_var
            weight_var <- options_data$weight_var
            plot_type <- options_data$plot_type
            fill_type <- options_data$fill_type
            sort_streams <- options_data$sort_streams
            label_nodes <- options_data$label_nodes
            curve_type <- options_data$curve_type
            show_counts <- options_data$show_counts
            show_legend <- options_data$show_legend
            originaltheme <- options_data$originaltheme
            mytitle <- options_data$mytitle
            xtitle <- options_data$xtitle
            ytitle <- options_data$ytitle
            
            # Create appropriate river plot based on the selected type
            if (plot_type == "alluvial") {
                # Create the plot using ggalluvial
                
                # Determine if we have a single strata variable (long format) or multiple (wide format)
                if (length(strata_vars) == 1) {
                    # Single strata variable, likely in long format already
                    strata_var <- strata_vars[1]
                    
                    # Create frequency data
                    if (is.null(weight_var)) {
                        # No weight variable, use counts
                        alluvial_data <- mydata %>%
                            dplyr::group_by(!!rlang::sym(time_var), !!rlang::sym(strata_var)) %>%
                            dplyr::summarize(freq = dplyr::n(), .groups = 'drop')
                    } else {
                        # Use weight variable
                        alluvial_data <- mydata %>%
                            dplyr::group_by(!!rlang::sym(time_var), !!rlang::sym(strata_var)) %>%
                            dplyr::summarize(freq = sum(!!rlang::sym(weight_var), na.rm = TRUE), .groups = 'drop')
                    }
                    
                    # Create the base plot
                    plot <- ggplot2::ggplot(alluvial_data, 
                                   ggplot2::aes(x = !!rlang::sym(time_var), 
                                       stratum = !!rlang::sym(strata_var),
                                       alluvium = !!rlang::sym(strata_var),
                                       y = freq,
                                       fill = !!rlang::sym(strata_var),
                                       label = !!rlang::sym(strata_var))) +
                        ggalluvial::geom_alluvium(alpha = 0.8) +
                        ggalluvial::geom_stratum(width = 1/3, alpha = 0.8)
                    
                    # Add labels if requested
                    if (label_nodes) {
                        plot <- plot + ggalluvial::geom_text(stat = "stratum", 
                                                            ggplot2::aes(label = ggplot2::after_stat(stratum)))
                    }
                    
                } else {
                    # Multiple strata variables, likely in wide format
                    var_names <- strata_vars
                    
                    # Create basic aesthetics mapping for multiple axes
                    if (length(var_names) >= 2) {
                        if (is.null(weight_var)) {
                            # Without weight variable
                            plot <- ggplot2::ggplot(mydata,
                                           ggplot2::aes_string(axis1 = var_names[1],
                                                     axis2 = var_names[2]))
                            
                            # Add more axes if available
                            if (length(var_names) >= 3) {
                                plot$mapping$axis3 <- ggplot2::aes_string(axis3 = var_names[3])$axis3
                            }
                            if (length(var_names) >= 4) {
                                plot$mapping$axis4 <- ggplot2::aes_string(axis4 = var_names[4])$axis4
                            }
                        } else {
                            # With weight variable
                            plot <- ggplot2::ggplot(mydata,
                                          ggplot2::aes_string(axis1 = var_names[1],
                                                    axis2 = var_names[2],
                                                    weight = weight_var))
                            
                            # Add more axes if available
                            if (length(var_names) >= 3) {
                                plot$mapping$axis3 <- ggplot2::aes_string(axis3 = var_names[3])$axis3
                            }
                            if (length(var_names) >= 4) {
                                plot$mapping$axis4 <- ggplot2::aes_string(axis4 = var_names[4])$axis4
                            }
                        }
                        
                        # Determine fill aesthetic based on fill_type
                        fill_var <- if(fill_type == "first") var_names[1] 
                                   else if(fill_type == "last") var_names[length(var_names)]
                                   else var_names[1]  # default to first
                        
                        # Complete the plot
                        plot <- plot + 
                            ggalluvial::geom_alluvium(ggplot2::aes_string(fill = fill_var),
                                                     alpha = 0.8) +
                            ggalluvial::geom_stratum(width = 1/3, alpha = 0.8)
                        
                        # Add labels if requested
                        if (label_nodes) {
                            plot <- plot + ggalluvial::geom_text(stat = "stratum", 
                                                                ggplot2::aes(label = ggplot2::after_stat(stratum)))
                        }
                    } else {
                        stop("Need at least 2 strata variables for wide format river plot")
                    }
                }
                
            } else if (plot_type == "sankey") {
                # Use ggalluvial but with sankey-style layout
                var_names <- strata_vars
                
                if (length(var_names) >= 2) {
                    if (is.null(weight_var)) {
                        # Set up the aesthetics for Sankey-style 
                        plot <- ggplot2::ggplot(mydata,
                                       ggplot2::aes_string(axis1 = var_names[1],
                                                 axis2 = var_names[2]))
                        
                        # Add more axes if available
                        if (length(var_names) >= 3) {
                            plot$mapping$axis3 <- ggplot2::aes_string(axis3 = var_names[3])$axis3
                        }
                        if (length(var_names) >= 4) {
                            plot$mapping$axis4 <- ggplot2::aes_string(axis4 = var_names[4])$axis4
                        }
                    } else {
                        # With weight variable
                        plot <- ggplot2::ggplot(mydata,
                                      ggplot2::aes_string(axis1 = var_names[1],
                                                axis2 = var_names[2],
                                                weight = weight_var))
                        
                        # Add more axes if available
                        if (length(var_names) >= 3) {
                            plot$mapping$axis3 <- ggplot2::aes_string(axis3 = var_names[3])$axis3
                        }
                        if (length(var_names) >= 4) {
                            plot$mapping$axis4 <- ggplot2::aes_string(axis4 = var_names[4])$axis4
                        }
                    }
                    
                    # Determine fill aesthetic
                    fill_var <- if(fill_type == "first") var_names[1] 
                               else if(fill_type == "last") var_names[length(var_names)]
                               else var_names[1]
                    
                    # Complete the plot with Sankey-style settings
                    plot <- plot + 
                        ggalluvial::geom_alluvium(ggplot2::aes_string(fill = fill_var),
                                                 alpha = 0.8,
                                                 width = 1/2) +
                        ggalluvial::geom_stratum(width = 1/8, alpha = 0.8)
                    
                    # Add labels if requested
                    if (label_nodes) {
                        plot <- plot + ggalluvial::geom_text(stat = "stratum", 
                                                            ggplot2::aes(label = ggplot2::after_stat(stratum)))
                    }
                } else {
                    stop("Need at least 2 strata variables for Sankey plot")
                }
                
            } else if (plot_type == "stream") {
                # Create a streamgraph using ggplot2
                if (length(strata_vars) == 1) {
                    # Data is likely already in long format
                    strata_var <- strata_vars[1]
                    
                    if (is.null(weight_var)) {
                        # No weight variable, use counts
                        stream_data <- mydata %>%
                            dplyr::group_by(!!rlang::sym(time_var), !!rlang::sym(strata_var)) %>%
                            dplyr::summarize(freq = dplyr::n(), .groups = 'drop')
                    } else {
                        # Use weight variable
                        stream_data <- mydata %>%
                            dplyr::group_by(!!rlang::sym(time_var), !!rlang::sym(strata_var)) %>%
                            dplyr::summarize(freq = sum(!!rlang::sym(weight_var), na.rm = TRUE), .groups = 'drop')
                    }
                    
                    # Create the streamgraph
                    plot <- ggplot2::ggplot(stream_data, 
                                  ggplot2::aes(x = !!rlang::sym(time_var), 
                                      y = freq, 
                                      fill = !!rlang::sym(strata_var),
                                      group = !!rlang::sym(strata_var))) +
                        ggplot2::geom_area(position = "stack", alpha = 0.8) +
                        ggplot2::theme_minimal() +
                        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
                    
                } else {
                    # Multiple strata variables, need to reshape to long format first
                    stop("Stream plot with multiple strata variables requires data reshaping. Please use the Restructure option in Jamovi to convert your data to long format first.")
                }
            }
            
            # Add titles and customize appearance
            plot <- plot + 
                ggplot2::labs(title = mytitle,
                     x = xtitle,
                     y = ytitle)
            
            # Handle legend display
            if (!show_legend) {
                plot <- plot + ggplot2::theme(legend.position = "none")
            }
            
            # Apply theme
            if (!originaltheme) {
                plot <- plot + ggtheme
            } else {
                if (requireNamespace("ggstatsplot", quietly = TRUE)) {
                    plot <- plot + ggstatsplot::theme_ggstatsplot()
                } else {
                    plot <- plot + ggplot2::theme_minimal()
                }
            }
            
            # Display counts if requested
            if (show_counts) {
                # Logic depends on the plot type
                if (plot_type == "alluvial" || plot_type == "sankey") {
                    # Add count labels to the stratum
                    plot <- plot + ggalluvial::geom_text(stat = "stratum", 
                                                       ggplot2::aes(label = ggplot2::after_stat(count)),
                                                       position = "top", size = 3)
                } else if (plot_type == "stream") {
                    # For stream plots, adding text is more complex
                    # This is a simplified approach
                    plot <- plot + ggplot2::geom_text(ggplot2::aes(label = freq), 
                                           position = ggplot2::position_stack(vjust = 0.5), size = 3)
                }
            }
            
            # Print the plot
            print(plot)
            TRUE
        }
    )
)