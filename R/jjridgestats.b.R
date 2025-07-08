#' @title Enhanced Ridgeline Plot
#' @importFrom R6 R6Class
#' @import jmvcore
#'

jjridgestatsClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "jjridgestatsClass",
    inherit = jjridgestatsBase,
    private = list(

        # Cache for processed data and options to avoid redundant computation
        .processedData = NULL,
        .processedOptions = NULL,

        # init ----
        .init = function() {
            self$results$plot$setSize(800, 500)
        },

        # Optimized data preparation with caching
        .prepareData = function(force_refresh = FALSE) {
            if (!is.null(private$.processedData) && !force_refresh) {
                return(private$.processedData)
            }

            # Prepare data with progress feedback
            self$results$todo$setContent(
                glue::glue("<br>Processing data for ridgeline plot analysis...<br><hr>")
            )

            mydata <- self$data
            
            # Convert dependent variable to numeric
            dep <- self$options$dep
            if (!is.null(dep)) {
                mydata[[dep]] <- jmvcore::toNumeric(mydata[[dep]])
            }

            # Exclude NA with checkpoint
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
                glue::glue("<br>Preparing ridgeline plot options...<br><hr>")
            )

            # Process options
            dep <- self$options$dep
            group <- self$options$group
            plotStyle <- self$options$plotStyle
            scaling <- self$options$scaling
            bandwidth <- self$options$bandwidth
            binwidth <- self$options$binwidth
            fill <- self$options$fill
            colorscheme <- self$options$colorscheme
            customColor <- self$options$customColor
            themeChoice <- self$options$themeChoice
            legendPosition <- self$options$legendPosition
            
            # Process titles
            mytitle <- if(self$options$mytitle == '') NULL else self$options$mytitle
            xtitle <- if(self$options$xtitle == '') NULL else self$options$xtitle
            ytitle <- if(self$options$ytitle == '') NULL else self$options$ytitle
            
            # Cache the processed options
            options_list <- list(
                dep = dep,
                group = group,
                plotStyle = plotStyle,
                scaling = scaling,
                bandwidth = bandwidth,
                binwidth = binwidth,
                fill = fill,
                colorscheme = colorscheme,
                customColor = customColor,
                themeChoice = themeChoice,
                legendPosition = legendPosition,
                mytitle = mytitle,
                xtitle = xtitle,
                ytitle = ytitle
            )
            private$.processedOptions <- options_list
            return(options_list)
        },

        # run ----
        .run = function() {
            ## Initial Message ----
            if (is.null(self$options$dep) || is.null(self$options$group)) {

                ## todo ----
                todo <- glue::glue(
                "<br>
                Welcome to ClinicoPath
                <br><br>
                This tool will help you generate Ridgeline Plots.
                <br><br>
                This function uses ggplot2 and ggridges packages. See documentations <a href = 'https://wilkelab.org/ggridges/' target='_blank'>ggridges</a>.
                <br>
                Please cite jamovi and the packages as given below.
                <br><hr>"
                )

                self$results$todo$setContent(todo)

                return()

            } else {

                todo <- glue::glue("<br>You have selected to make a ridgeline plot.<br><hr>")

                self$results$todo$setContent(todo)

                if (nrow(self$data) == 0)
                    stop('Data contains no (complete) rows')

                # Pre-process data and options for performance
                private$.prepareData()
                private$.prepareOptions()

            }
        },

        .plot = function(image, ggtheme, theme, ...) {
            # Input validation
            if (is.null(self$options$dep) || is.null(self$options$group))
                return()
            if (nrow(self$data) == 0)
                stop('Data contains no (complete) rows')

            # Use cached data and options for performance ----
            mydata <- private$.prepareData()
            options_data <- private$.prepareOptions()
            
            dep <- options_data$dep
            group <- options_data$group
            plotStyle <- options_data$plotStyle
            scaling <- options_data$scaling
            bandwidth <- options_data$bandwidth
            binwidth <- options_data$binwidth
            fill <- options_data$fill
            colorscheme <- options_data$colorscheme
            customColor <- options_data$customColor
            themeChoice <- options_data$themeChoice
            legendPosition <- options_data$legendPosition
            mytitle <- options_data$mytitle
            xtitle <- options_data$xtitle
            ytitle <- options_data$ytitle

            # Base plot
            plot <- ggplot2::ggplot(mydata,
                                    ggplot2::aes(x = !!rlang::sym(dep),
                                                 y = !!rlang::sym(group)))

            # Add appropriate geom based on plot style
            if (plotStyle == 'density') {
                if (fill) {
                    plot <- plot +
                        ggplot2::aes(fill = !!rlang::sym(group)) +
                        ggridges::geom_density_ridges(
                            scale = scaling,
                            bandwidth = bandwidth,
                            alpha = 0.6,
                            color = "black"
                        )
                } else {
                    plot <- plot +
                        ggridges::geom_density_ridges(
                            scale = scaling,
                            bandwidth = bandwidth,
                            color = "black",
                            fill = NA
                        )
                }
            } else if (plotStyle == 'histogram') {
                plot <- plot +
                    ggridges::geom_density_ridges(
                        scale = scaling,
                        stat = "binline",
                        bins = round(1/binwidth),
                        alpha = if(fill) 0.6 else 0,
                        color = "black"
                    )
            } else if (plotStyle == 'gradient') {
                plot <- plot +
                    ggplot2::aes(fill = stat(x)) +
                    ggridges::geom_density_ridges_gradient(
                        scale = scaling,
                        bandwidth = bandwidth,
                        rel_min_height = 0.01,
                        color = "black"
                    )
            }

            # Apply color scheme - using ggplot2's integrated viridis scales
            if (fill || plotStyle == 'gradient') {
                if (colorscheme == 'viridis') {
                    plot <- plot + ggplot2::scale_fill_viridis_d()
                } else if (colorscheme == 'plasma') {
                    plot <- plot + ggplot2::scale_fill_viridis_d(option = "plasma")
                } else if (colorscheme == 'magma') {
                    plot <- plot + ggplot2::scale_fill_viridis_d(option = "magma")
                } else if (colorscheme == 'blues') {
                    plot <- plot + ggplot2::scale_fill_brewer(palette = "Blues")
                } else if (colorscheme == 'custom') {
                    plot <- plot + ggplot2::scale_fill_manual(values = customColor)
                }
            }

            # Apply theme
            if (themeChoice == 'minimal') {
                plot <- plot + ggridges::theme_ridges()
            } else if (themeChoice == 'classic') {
                plot <- plot + ggplot2::theme_classic()
            } else if (themeChoice == 'dark') {
                plot <- plot + ggplot2::theme_dark()
            }

            # Add labels
            plot <- plot + ggplot2::labs(
                title = mytitle,
                x = xtitle,
                y = ytitle
            )

            # Legend position
            if (legendPosition != 'none' && (fill || plotStyle == 'gradient')) {
                plot <- plot + ggplot2::theme(
                    legend.position = legendPosition
                )
            } else {
                plot <- plot + ggplot2::theme(
                    legend.position = "none"
                )
            }

            # Additional theme refinements
            plot <- plot + ggplot2::theme(
                panel.spacing = ggplot2::unit(0.1, "lines"),
                strip.text.x = ggplot2::element_text(size = 8)
            )

            # Print Plot
            print(plot)
            TRUE
        }
    )
)
