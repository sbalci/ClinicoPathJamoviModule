#' @title Enhanced Ridgeline Plot
#' @importFrom R6 R6Class
#' @import jmvcore
#'

jjridgestatsClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "jjridgestatsClass",
    inherit = jjridgestatsBase,
    private = list(
        # init and run functions remain the same...

        .plot = function(image, ggtheme, theme, ...) {
            # Input validation
            if (is.null(self$options$dep) || is.null(self$options$group))
                return()
            if (nrow(self$data) == 0)
                stop('Data contains no (complete) rows')

            # Prepare data
            mydata <- self$data
            dep <- self$options$dep
            mydata[[dep]] <- jmvcore::toNumeric(mydata[[dep]])
            mydata <- jmvcore::naOmit(mydata)

            # Get parameters
            group <- self$options$group
            scaling <- self$options$scaling
            bandwidth <- self$options$bandwidth
            plotStyle <- self$options$plotStyle
            colorscheme <- self$options$colorscheme
            binwidth <- self$options$binwidth
            fill <- self$options$fill

            # Get titles
            mytitle <- if(self$options$mytitle == '') NULL else self$options$mytitle
            xtitle <- if(self$options$xtitle == '') NULL else self$options$xtitle
            ytitle <- if(self$options$ytitle == '') NULL else self$options$ytitle

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
                    customColor <- self$options$customColor
                    plot <- plot + ggplot2::scale_fill_manual(values = customColor)
                }
            }

            # Apply theme
            themeChoice <- self$options$themeChoice
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
            legendPos <- self$options$legendPosition
            if (legendPos != 'none' && (fill || plotStyle == 'gradient')) {
                plot <- plot + ggplot2::theme(
                    legend.position = legendPos
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
