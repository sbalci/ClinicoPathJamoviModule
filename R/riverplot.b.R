#' @title River Plots (Temporal Flows)
#' @description
#' Creates river plots to visualize how categorical variables change over time 
#' or sequential stages. Shows temporal flows of categories across time periods, 
#' ideal for tracking patient journeys, treatment progressions, disease stages, 
#' or any categorical changes over time. Supports both individual tracking and 
#' aggregate trend visualization.
#' @importFrom R6 R6Class
#' @import jmvcore
#' @importFrom dplyr group_by_at summarize n
#' @importFrom rlang sym
#' @importFrom ggplot2 ggplot aes geom_area theme_minimal theme element_text labs scale_x_discrete geom_text
#' @importFrom ggalluvial geom_alluvium geom_stratum StatStratum

riverplotClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "riverplotClass",
    inherit = riverplotBase,
    private = list(
        # init ----
        .init = function() {
            # Check for required packages
            missing_packages <- c()
            if (!requireNamespace("ggalluvial", quietly = TRUE)) {
                missing_packages <- c(missing_packages, "ggalluvial")
            }
            if (!requireNamespace("dplyr", quietly = TRUE)) {
                missing_packages <- c(missing_packages, "dplyr")
            }
            
            if (length(missing_packages) > 0) {
                error_msg <- paste0(
                    "Required packages missing: ",
                    paste(missing_packages, collapse = ", "),
                    "\n\nPlease install with:\n",
                    "install.packages(c(", paste0("'", missing_packages, "'", collapse = ", "), "))"
                )
                
                self$results$todo$setContent(paste0(
                    "<div class='alert alert-danger'>",
                    "<h4>Missing Dependencies</h4>",
                    "<p>", gsub("\n", "<br>", error_msg), "</p>",
                    "</div>"
                ))
                return()
            }
            
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
                todo <- paste0(
                    "<div class='alert alert-info'>",
                    "<h4>", .("Welcome to River Plots (Temporal Flows)"), "</h4>",
                    "<p>", .("Track how categorical variables change across time periods or sequential stages."), "</p>",
                    
                    "<h5>", .("Required inputs:"), "</h5>",
                    "<ul>",
                    "<li><strong>", .("Time/Sequence Variable"), "</strong>: ", .("Time points, follow-up dates, or sequential stages"), "</li>",
                    "<li><strong>", .("Categorical Variable"), "</strong>: ", .("Variable that changes over time (treatment response, disease stage)"), "</li>",
                    "</ul>",
                    
                    "<h5>", .("Optional inputs:"), "</h5>",
                    "<ul>",
                    "<li><strong>", .("ID Variable"), "</strong>: ", .("Track individual patients/entities through time"), "</li>",
                    "<li><strong>", .("Weight Variable"), "</strong>: ", .("Values to weight flows (patient counts, severity scores)"), "</li>",
                    "</ul>",
                    
                    "<h5>", .("Flow diagrams:"), "</h5>",
                    "<ul>",
                    "<li><strong>Purpose</strong>: Track individual changes over time periods</li>",
                    "<li><strong>Best for</strong>: Patient journeys, treatment progressions</li>",
                    "<li><strong>Output</strong>: Alluvial flows showing temporal transitions</li>",
                    "</ul>",
                    
                    "<h5>", .("Stream charts:"), "</h5>",
                    "<ul>",
                    "<li><strong>Purpose</strong>: Show aggregate trends over time</li>",
                    "<li><strong>Best for</strong>: Population-level changes, temporal trends</li>",
                    "<li><strong>Output</strong>: Stacked area chart showing category proportions</li>",
                    "</ul>",
                    
                    "<p><em>For category-to-category flows (non-temporal), use the dedicated <strong>Alluvial & Sankey Diagrams</strong> function.</em></p>",
                    "</div>"
                )

                self$results$todo$setContent(todo)
                return()

            } else {
                # Hide welcome message and validate data
                self$results$todo$setVisible(FALSE)
                
                # Validate data
                if (nrow(self$data) == 0) {
                    stop(.("Data contains no complete rows. Please check your data for missing values."))
                }
                
                # Validate variables exist in data
                all_vars <- c(self$options$time, self$options$strata, self$options$id, self$options$weight)
                all_vars <- all_vars[!sapply(all_vars, is.null)]
                missing_vars <- setdiff(all_vars, names(self$data))
                
                if (length(missing_vars) > 0) {
                    stop(paste(.("Variables not found in data:"), paste(missing_vars, collapse = ", ")))
                }
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

            # Process options for temporal flow plots
            plot_type <- self$options$plotType
            show_legend <- self$options$showLegend
            curve_type <- self$options$curveType
            label_nodes <- self$options$labelNodes
            show_counts <- self$options$showCounts
            fill_type <- self$options$fillType

            # Title and labels
            mytitle <- self$options$mytitle
            if (mytitle == '') mytitle <- NULL

            xtitle <- self$options$xtitle
            if (xtitle == '') xtitle <- NULL

            ytitle <- self$options$ytitle
            if (ytitle == '') ytitle <- NULL

            # Ensure we have required variables for temporal flows
            if (length(strata_vars) != 1) {
                stop(.("Temporal flow plots require exactly one categorical variable. Use the Alluvial function for multiple variables."))
            }
            
            strata_var <- strata_vars[1]

            # Create temporal categorical flow plots
            if (plot_type == "flow") {
                # Flow diagram - tracks individual changes over time
                
                # For flow diagrams, we need to handle individual tracking
                if (!is.null(id_var)) {
                    # Individual entity tracking through time periods
                    flow_data <- mydata
                    
                    # Convert to long format for ggalluvial
                    plot <- ggplot(flow_data,
                                   aes(axis1 = !!rlang::sym(time_var),
                                       axis2 = !!rlang::sym(strata_var))) +
                        ggalluvial::geom_alluvium(aes(fill = !!rlang::sym(
                            if (fill_type == "first") time_var else strata_var)),
                            alpha = 0.8,
                            curve_type = curve_type,
                            width = 1/3) +
                        ggalluvial::geom_stratum(width = 1/4, alpha = 0.8) +
                        ggplot2::theme_minimal()
                        
                } else {
                    # Aggregate flows without individual tracking
                    if (is.null(weight_var)) {
                        # Use counts
                        flow_data <- mydata %>%
                            dplyr::group_by_at(c(time_var, strata_var)) %>%
                            dplyr::summarize(freq = dplyr::n(), .groups = 'drop')
                    } else {
                        # Use weight variable
                        flow_data <- mydata %>%
                            dplyr::group_by_at(c(time_var, strata_var)) %>%
                            dplyr::summarize(freq = sum(!!rlang::sym(weight_var), na.rm = TRUE), .groups = 'drop')
                    }
                    
                    # Create alluvial plot for temporal flows
                    plot <- ggplot(flow_data,
                                   aes(axis1 = !!rlang::sym(time_var),
                                       axis2 = !!rlang::sym(strata_var),
                                       y = freq)) +
                        ggalluvial::geom_alluvium(aes(fill = !!rlang::sym(
                            if (fill_type == "first") time_var else strata_var)),
                            alpha = 0.8,
                            curve_type = curve_type,
                            width = 1/3) +
                        ggalluvial::geom_stratum(width = 1/4, alpha = 0.8) +
                        ggplot2::theme_minimal()
                }
                
                # Add labels if requested
                if (label_nodes) {
                    plot <- plot + ggplot2::geom_text(stat = ggalluvial::StatStratum,
                                                      aes(label = after_stat(stratum)),
                                                      size = 3)
                }
                
                # Add counts if requested
                if (show_counts) {
                    plot <- plot + ggplot2::geom_text(stat = ggalluvial::StatStratum,
                                                      aes(label = after_stat(count)),
                                                      size = 3, vjust = -0.5)
                }
                
            } else if (plot_type == "stream") {
                # Stream chart - shows aggregate trends over time
                
                # Prepare stream data
                if (is.null(weight_var)) {
                    # Use counts
                    stream_data <- mydata %>%
                        dplyr::group_by_at(c(time_var, strata_var)) %>%
                        dplyr::summarize(freq = dplyr::n(), .groups = 'drop')
                } else {
                    # Use weight variable
                    stream_data <- mydata %>%
                        dplyr::group_by_at(c(time_var, strata_var)) %>%
                        dplyr::summarize(freq = sum(!!rlang::sym(weight_var), na.rm = TRUE), .groups = 'drop')
                }
                
                # Create streamgraph for temporal trends
                plot <- ggplot(stream_data,
                               aes(x = !!rlang::sym(time_var),
                                   y = freq,
                                   fill = !!rlang::sym(strata_var),
                                   group = !!rlang::sym(strata_var))) +
                    ggplot2::geom_area(position = "stack", alpha = 0.8) +
                    ggplot2::theme_minimal() +
                    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
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

            # Apply theme (removed ggstatsplot dependency)
            originaltheme <- self$options$originaltheme

            if (!originaltheme) {
                plot <- plot + ggtheme
            } else {
                # Use ggplot2 theme instead of ggstatsplot
                plot <- plot + ggplot2::theme_minimal() +
                    ggplot2::theme(
                        plot.title = ggplot2::element_text(size = 14, face = "bold"),
                        axis.title = ggplot2::element_text(size = 12),
                        legend.position = "bottom"
                    )
            }

            # Note: Stream plots don't typically need count labels as they show proportions
            # The area size itself represents the values

            # Print the plot
            print(plot)
            TRUE
        }
    )
)
