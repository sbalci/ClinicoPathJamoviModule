
# This file is a generated template, your changes will not be overwritten

statsplot2Class <- if (requireNamespace('jmvcore')) R6::R6Class(
    "statsplot2Class",
    inherit = statsplot2Base,
    private = list(
        .run = function() {

            mydata <- self$data

            mydep <- self$data[[self$options$dep]]
            mygroup <- self$data[[self$options$group]]


            plotData <- data.frame(gr = mygroup, dp = jmvcore::toNumeric(mydep))
            plotData <- jmvcore::naOmit(plotData)

            image <- self$results$plot
            image$setState(plotData)


            self$results$text1$setContent(plotData)



        },


        .plot=function(image, ...) {  # <-- the plot function


            plotData <- image$state


            # plot <- ggplot(plotData, aes(x = gr,
            #                              y = dp)) +
            #     geom_point()

            plot <- plotData %>%
                ggstatsplot::ggbetweenstats(
                    x = gr,
                    y = dp
                    )

            print(plot)
            TRUE


        })
)
