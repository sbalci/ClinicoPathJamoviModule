#' @title Waffle Charts
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import ggplot2
#' @import waffle

jjwafflestatsClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "jjwafflestatsClass",
    inherit = jjwafflestatsBase,
    private = list(
        .init = function() {
            self$results$plot$setSize(600, 450)

            if (!is.null(self$options$facet)) {
                mydata <- self$data
                facet_var <- self$options$facet
                if (!is.null(mydata[[facet_var]])) {
                    num_levels <- length(unique(mydata[[facet_var]]))
                    if (num_levels > 1)
                        self$results$plot$setSize(num_levels * 600, 450)
                }
            }
        },

        .run = function() {
            if (is.null(self$options$counts) || is.null(self$options$groups)) {
                todo <- glue::glue(
                    "<br>Welcome to ClinicoPath
                    <br><br>
                    This tool will help you create Waffle Charts to visualize distributions.
                    <br><br>
                    Please provide:
                    <br>1. A numeric variable for the counts
                    <br>2. A grouping variable to organize the squares
                    <br><br>
                    The waffle chart will show proportions using squares arranged in a grid.
                    <br><hr>"
                )
                self$results$todo$setContent(todo)
                return()
            }

            if (nrow(self$data) == 0)
                stop('Data contains no (complete) rows')

            todo <- glue::glue("<br>Creating waffle chart...<br><hr>")
            self$results$todo$setContent(todo)
        },

        .plot = function(image, ...) {
            if (is.null(self$options$counts) || is.null(self$options$groups))
                return()

            if (nrow(self$data) == 0)
                stop('Data contains no (complete) rows')

            # Prepare data
            mydata <- self$data
            mydata <- jmvcore::naOmit(mydata)

            counts_var <- self$options$counts
            groups_var <- self$options$groups
            facet_var <- self$options$facet

            # Build grouping expression based on whether faceting is used
            if (!is.null(facet_var)) {
                plotdata <- mydata %>%
                    dplyr::group_by(
                        !!rlang::sym(groups_var),
                        !!rlang::sym(facet_var)
                    ) %>%
                    dplyr::summarise(
                        count = sum(!!rlang::sym(counts_var))
                    ) %>%
                    dplyr::ungroup()
            } else {
                plotdata <- mydata %>%
                    dplyr::group_by(!!rlang::sym(groups_var)) %>%
                    dplyr::summarise(
                        count = sum(!!rlang::sym(counts_var))
                    ) %>%
                    dplyr::ungroup()
            }

            # Get number of unique groups
            n_groups <- length(unique(plotdata[[groups_var]]))

            # Generate color palettes based on number of groups
            palettes <- list(
                default = colorRampPalette(c("#4DA6FF", "#FFB84D"))(n_groups),
                colorblind = colorRampPalette(c("#999999", "#E69F00", "#56B4E9", "#009E73"))(n_groups),
                pastel = colorRampPalette(c("#69b3a2", "#404080", "#FFA07A"))(n_groups),
                dark = colorRampPalette(c("#1B9E77", "#D95F02", "#7570B3"))(n_groups)
            )

            sel_palette <- palettes[[self$options$color_palette]]
            if (is.null(sel_palette))
                sel_palette <- palettes$default

            # Create base plot
            p <- ggplot2::ggplot(
                plotdata,
                ggplot2::aes(
                    fill = !!rlang::sym(groups_var),
                    values = count
                )
            ) +
                waffle::geom_waffle(
                    n_rows = self$options$rows,
                    size = 0.5,
                    color = "white",
                    flip = self$options$flip,
                    make_proportional = TRUE
                ) +
                ggplot2::scale_fill_manual(
                    values = sel_palette,
                    name = if (self$options$legendtitle != '')
                        self$options$legendtitle
                    else
                        groups_var
                ) +
                ggplot2::coord_equal() +
                ggplot2::theme_minimal()

            # Add faceting if specified
            if (!is.null(facet_var)) {
                p <- p + ggplot2::facet_wrap(
                    as.formula(paste0("~", facet_var)),
                    nrow = 1,
                    strip.position = "bottom"
                )
            }

            # Add title if specified
            if (self$options$mytitle != '')
                p <- p + ggplot2::labs(title = self$options$mytitle)

            # Handle legend
            if (!self$options$show_legend) {
                p <- p + ggplot2::theme(legend.position = "none")
            }

            # Apply final theme adjustments
            p <- p + ggplot2::theme(
                plot.title = ggplot2::element_text(
                    hjust = 0.5,
                    size = 14,
                    margin = ggplot2::margin(b = 10)
                ),
                legend.position = if(self$options$show_legend) "right" else "none",
                legend.title = ggplot2::element_text(size = 10),
                legend.text = ggplot2::element_text(size = 8),
                strip.text = ggplot2::element_text(size = 10),
                panel.grid = ggplot2::element_blank(),
                axis.text = ggplot2::element_blank(),
                axis.title = ggplot2::element_blank()
            )

            print(p)
            TRUE
        }
    )
)
