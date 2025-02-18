#' @title Treemap
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import ggplot2

jjtreemapClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "jjtreemapClass",
    inherit = jjtreemapBase,
    private = list(
        # Init function to set up the plot size
        .init = function() {
            self$results$plot$setSize(800, 500)
        },

        # Run function - initial setup and checks
        .run = function() {
            if (is.null(self$options$group) || is.null(self$options$size)) {
                # Show welcome message if required variables aren't provided
                todo <- glue::glue(
                    "<br>Welcome to ClinicoPath
                    <br><br>
                    This tool will help you create treemap visualizations.
                    <br><br>
                    Please provide at least:
                    <br>- A group variable (for rectangles)
                    <br>- A size variable (for rectangle areas)
                    <br><hr>"
                )
                self$results$todo$setContent(todo)
                return()
            }

            if (nrow(self$data) == 0)
                stop('Data contains no (complete) rows')
        },

        # Main plot function
        .plot = function(image, ggtheme, theme, ...) {
            # Check for required variables
            if (is.null(self$options$group) || is.null(self$options$size))
                return()

            if (nrow(self$data) == 0)
                stop('Data contains no (complete) rows')

            # Prepare data
            mydata <- self$data
            mydata <- jmvcore::naOmit(mydata)

            # Get variables from options
            group_var <- self$options$group
            size_var <- self$options$size
            color_var <- self$options$color

            # Convert font face option to numeric
            font_face_map <- list(
                "normal" = 1,
                "bold" = 2,
                "italic" = 3,
                "bolditalic" = 4
            )
            font_face <- font_face_map[[self$options$labelFontFace]]

            # Create alignment list based on user options
            align_labels <- list(
                c(self$options$labelAlignH, self$options$labelAlignV)
            )
            if (!is.null(self$options$color)) {
                align_labels[[2]] <- c(self$options$labelAlignH, self$options$labelAlignV)
            }

            # Create the treemap data
            data_tree <- treemap::treemap(
                mydata,
                index = c(group_var),
                vSize = size_var,
                vColor = if (!is.null(color_var)) color_var else group_var,
                type = "categorical",
                algorithm = "pivotSize",

                # Border customization
                border.col = c(self$options$borderLevel1Color, self$options$borderLevel2Color),
                border.lwds = c(self$options$borderLevel1Width, self$options$borderLevel2Width),

                # Label customization
                fontsize.labels = c(self$options$labelLevel1Size, self$options$labelLevel2Size),
                fontcolor.labels = c(self$options$labelLevel1Color, self$options$labelLevel2Color),
                fontface.labels = rep(font_face, 2),
                bg.labels = self$options$labelBackground,
                align.labels = align_labels,
                overlap.labels = self$options$labelOverlap,

                aspRatio = self$options$aspectRatio,
                draw = FALSE
            )

            # Convert treemap data to ggplot-friendly format
            data_ggplot <- data_tree[["tm"]] %>%
                tibble::as_tibble() %>%
                dplyr::arrange(desc(vSize)) %>%
                dplyr::mutate(
                    xmax = x0 + w,
                    ymax = y0 + h,
                    label = sprintf("%s\n(%g)", get(group_var), vSize)
                )

            # Create the plot
            p <- ggplot(data_ggplot) +
                geom_rect(
                    aes(
                        xmin = x0,
                        ymin = y0,
                        xmax = xmax,
                        ymax = ymax,
                        fill = vColor
                    ),
                    colour = "black",
                    size = self$options$borderWidth
                )

            # Add labels if requested
            if (self$options$showLabels) {
                p <- p + ggfittext::geom_fit_text(
                    aes(
                        xmin = x0,
                        xmax = xmax,
                        ymin = y0,
                        ymax = ymax,
                        label = label
                    ),
                    min.size = self$options$labelSize
                )
            }

            # Add title and labels
            p <- p + labs(
                title = self$options$title,
                subtitle = self$options$subtitle,
                caption = self$options$caption
            )

            # Add theme
            p <- p +
                theme_void() +
                theme(
                    legend.position = "none",
                    plot.title = element_text(size = 14, hjust = 0.5),
                    plot.subtitle = element_text(size = 12, hjust = 0.5),
                    plot.caption = element_text(size = 9, hjust = 1)
                )

            # Print plot
            print(p)
            TRUE
        }
    )
)
