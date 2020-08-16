
# This file is a generated template, your changes will not be overwritten

linechartClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "linechartClass",
    inherit = linechartBase,
    private = list(
        .run = function() {

            if (nrow(self$data) == 0)
                stop("Data contains no (complete) rows")


            data <- self$data

            xvar <- self$options$xvar
            yvar <- self$options$yvar

            data <- jmvcore::select(df = data,
                                    columnNames = c(xvar, yvar)
                                    )

            data <- jmvcore::naOmit(data)

            plotData <- data

            # image <- self$results$plot
            # image$setState(plotData)


            self$results$text$setContent(head(plotData))


        }

        ,
        .plot=function(image, ...) {

            # plotData <- image$state


            mydata <- self$data

            xvar <- self$options$xvar
            yvar <- self$options$yvar


            xvar <- jmvcore::composeTerm(components = xvar)
            yvar <- jmvcore::composeTerm(components = yvar)

            # https://www.r-graph-gallery.com/line-chart-ggplot2.html


            plot <- mydata %>%
                ggplot2::ggplot(.,
                                ggplot2::aes(x = !!xvar,
                                             y = !!yvar
                                )
                ) +
                ggplot2::geom_line()




            # plot <- mydata %>%
            #     ggplot2::ggplot(.,
            #                     ggplot2::aes(x = .data[[xvar]],
            #                                  y = .data[[yvar]]
            #                                  )
            #                     ) +
            #     ggplot2::geom_line()


            print(plot)
            TRUE
        }







        )
)
