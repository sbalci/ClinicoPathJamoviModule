#' @importFrom R6 R6Class
#' @import jmvcore
#'
# This file is a generated template, your changes will not be overwritten

statsplot2Class <- if (requireNamespace('jmvcore')) R6::R6Class(
    "statsplot2Class",
    inherit = statsplot2Base,
    private = list(
        .run = function() {


            # TODO

            todo <- glue::glue(
                "This Module is still under development
                -
                -  "
            )

            self$results$todo$setContent(todo)



            if (length(self$options$dep) + length(self$options$group) < 2)
                return()

            # mydata <- self$data

            mydep <- self$data[[self$options$dep]]
            mygroup <- self$data[[self$options$group]]


            plotData <- data.frame(gr = mygroup, dp = jmvcore::toNumeric(mydep))
            plotData <- jmvcore::naOmit(plotData)

            image <- self$results$plot

            image$setState(plotData)


            # self$results$text1$setContent(plotData)


            # mydepType <- data.frame(vclass = class(mydep),
            #                         vtypeof = typeof(mydep),
            #                         vordered = is.ordered(mydep),
            #                         vfactor = is.factor(mydep),
            #                         vnumeric = is.numeric(mydep),
            #                         vdouble = is.double(mydep),
            #                         vcharacter = is.character(mydep),
            #                         vdate = lubridate::is.Date(mydep),
            #                         vdate2 = is.na.POSIXlt(mydep)
            #                         )
            # mygroupType <- class(mygroup)
            # variableTypes <- list(mydepType, mygroupType)
            # self$results$text1$setContent(variableTypes)



        },


        .plot=function(image, ...) {  # <-- the plot function

            if (length(self$options$dep) + length(self$options$group) < 2)
                return()


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
