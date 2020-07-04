#' @title Violin Plots to Compare Between Groups
#'
#'
#'
#'
#' @importFrom R6 R6Class
#' @import jmvcore
#'


jjbetweenstatsClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "jjbetweenstatsClass",
    inherit = jjbetweenstatsBase,
    private = list(

        .run = function() {

            # Initial Message ----
            if (is.null(self$options$dep) ||
                is.null(self$options$group)) {
                # TODO ----

                todo <- glue::glue(
                    "
                <br>Welcome to ClinicoPath
                <br><br>
                This tool will help you generate Box Violin Plots.
                <br><br>
                This function uses ggplot2 and ggstatsplot packages. See documentations <a href = 'https://indrajeetpatil.github.io/ggstatsplot/reference/ggbetweenstats.html' target='_blank'>here</a> and <a href = 'https://indrajeetpatil.github.io/ggstatsplot/reference/grouped_ggbetweenstats.html' target='_blank'>here</a>.
                <br>
                Please cite jamovi and the packages as given below.
                <br><hr>"
                )

                self$results$todo$setContent(todo)

                return()

            } else {

                todo <- glue::glue(
                    "<br>You have selected to use a Violin Plot to compare continious variables between groups.<br><hr>")

                self$results$todo$setContent(todo)

                if (nrow(self$data) == 0)
                    stop('Data contains no (complete) rows')

            }
        }


        ,
        .plot = function(image, ...) {

            # the plot function ----
            # Error messages ----

            if (is.null(self$options$dep) ||
                is.null(self$options$group))
                return()

            if (nrow(self$data) == 0)
                stop('Data contains no (complete) rows')


            # Prepare Data ----


            # # direction, paired ----
            #
            # direction <- self$options$direction
            #
            # if (direction == "repeated") {
            #
            #     paired <- TRUE
            #
            # } else if (direction == "independent") {
            #
            #     paired <- FALSE
            #
            # }

            # distribution <-
            #     jmvcore::constructFormula(terms = self$options$distribution)

            # pairw <- self$options$pairw


            # distribution ----


            # distribution <-
            #     jmvcore::constructFormula(terms = self$options$distribution)

            # pairw <- self$options$pairw




            # read data ----

            mydata <- self$data


            # Exclude NA ----

            excl <- self$options$excl

            if (excl) {mydata <- jmvcore::naOmit(mydata)}


            # read arguments ----

            dep <- self$options$dep

            group <- self$options$group


            dep <- jmvcore::composeTerm(components = dep)

            group <- jmvcore::composeTerm(components = group)


            # ggbetweenstats ----
            # https://indrajeetpatil.github.io/ggstatsplot/reference/ggbetweenstats.html


            plot <- ggstatsplot::ggbetweenstats(
                data = mydata,
                x = !!group,
                y = !!dep
                # ,
                # type = distribution
            )


            # Print Plot ----

            print(plot)
            TRUE

        }


        ,
        .plot2 = function(image, ...) {

            # the plot function ----
            # Error messages ----

            if (is.null(self$options$dep) ||
                is.null(self$options$group) || is.null(self$options$grvar))
                return()

            if (nrow(self$data) == 0)
                stop('Data contains no (complete) rows')


            # Prepare Data ----


            # # direction, paired ----
            #
            # direction <- self$options$direction
            #
            # if (direction == "repeated") {
            #
            #     paired <- TRUE
            #
            # } else if (direction == "independent") {
            #
            #     paired <- FALSE
            #
            # }

            # distribution <-
            #     jmvcore::constructFormula(terms = self$options$distribution)

            # pairw <- self$options$pairw


            # distribution ----


            # distribution <-
            #     jmvcore::constructFormula(terms = self$options$distribution)

            # pairw <- self$options$pairw




            # read data ----

            mydata <- self$data


            # Exclude NA ----

            excl <- self$options$excl

            if (excl) {mydata <- jmvcore::naOmit(mydata)}


            # read arguments ----

            dep <- self$options$dep

            group <- self$options$group


            dep <- jmvcore::composeTerm(components = dep)

            group <- jmvcore::composeTerm(components = group)



            # grouped_ggbetweenstats
            # https://indrajeetpatil.github.io/ggstatsplot/reference/grouped_ggbetweenstats.html


            if ( !is.null(self$options$grvar) ) {

                grvar <- self$options$grvar

                plot2 <- ggstatsplot::grouped_ggbetweenstats(
                    data = mydata,
                    x = !!group,
                    y = !! dep,
                    grouping.var = !!grvar)

            }

            # Print Plot ----

            print(plot2)
            TRUE

        }




    )
)
