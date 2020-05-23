#' @title Violin Plot
#' @return Violin Plot
#' @importFrom R6 R6Class
#' @import jmvcore
#'

jjviolinClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "jjviolinClass",
    inherit = jjviolinBase,
    private = list(
        .run = function() {

            # Error Message ----

            if (nrow(self$data) == 0) stop("Data contains no (complete) rows")

            if ( is.null(self$options$dep) || is.null(self$options$group) || is.null(self$options$col) ) {

                # ToDo Message ----
                todo <- "
                <br>Welcome to ClinicoPath
                          <br><br>
                          This tool will help you form a Violin Plot.
                          "
                html <- self$results$todo
                html$setContent(todo)

            } else {
                todo <- ""
                html <- self$results$todo
                html$setContent(todo)

                # image <- self$results$plot
                # image$setState(plotData)

                }
        },


        .plot = function(image, ...) {

            # plotData <- image$state


            # Prepare Data ----

            mydata <- self$data

            # Exclude NA ----

            excl <- self$options$excl
            if (excl) {mydata <- jmvcore::naOmit(mydata)}

            # Define variables for arguments ----

            dep <- self$options$dep
            group <- self$options$group
            col <- self$options$col

            dep <- jmvcore::composeTerm(components = dep)

            group <- jmvcore::composeTerm(components = group)

            col <- jmvcore::composeTerm(components = col)


            # Plot function ----

            plot <- mydata %>%
                ggplot2::ggplot(.,
                                ggplot2::aes(x = .data[[group]],
                                             y = .data[[dep]]
                                             # , fill = .data[[col]]
                                            )
                                    ) +
                ggplot2::geom_violin()







            # ggplot( aes(x=text, y=value, fill=text, color=text)) +
            #     geom_violin(width=2.1, size=0.2) +
            #     scale_fill_viridis(discrete=TRUE) +
            #     scale_color_viridis(discrete=TRUE) +
            #     theme_ipsum() +
            #     theme(
            #         legend.position="none"
            #     ) +
            #     coord_flip() + # This switch X and Y axis and allows to get the horizontal version
            #     xlab("") +
            #     ylab("Assigned Probability (%)")






























            # Print plot

            print(plot)
            TRUE
        }
        )
)
