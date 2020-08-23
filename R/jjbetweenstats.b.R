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

        # init ----

        .init = function() {
            deplen <- length(self$options$dep)

            self$results$plot$setSize(400, deplen * 300)

            self$results$plot2$setSize(800, deplen * 300)

        }
        ,


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
                    "<br>You have selected to use a Violin Plot to compare continuous variables between groups.<br><hr>")

                self$results$todo$setContent(todo)

                if (nrow(self$data) == 0)
                    stop('Data contains no (complete) rows')

            }
        }


        ,
        .plot = function(image, ggtheme, theme, ...) {

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


            vars <- self$options$dep


            for (var in vars)
                mydata[[var]] <- jmvcore::toNumeric(mydata[[var]])


            # Exclude NA ----

            excl <- self$options$excl

            if (excl) {mydata <- jmvcore::naOmit(mydata)}


            # read arguments ----

            dep <- self$options$dep

            group <- self$options$group

            originaltheme <- self$options$originaltheme


            dep <- jmvcore::composeTerm(components = dep)

            group <- jmvcore::composeTerm(components = group)

            # ggbetweenstats ----
            # https://indrajeetpatil.github.io/ggstatsplot/reference/ggbetweenstats.html

            # dep == 1 ----

            if (length(self$options$dep) == 1) {
            plot <- ggstatsplot::ggbetweenstats(
                data = mydata,
                x = !!group,
                y = !!dep
                # ,
                # type = distribution
                , ggtheme = ggtheme
                , ggstatsplot.layer = originaltheme

            )

            }



            # dep > 1 ----

            if (length(self$options$dep) > 1) {
                dep2 <- as.list(self$options$dep)

                plotlist <-
                    purrr::pmap(
                        .l = list(y = dep2,
                                  # title = list(dep),
                                  messages = FALSE),
                        .f = ggstatsplot::ggbetweenstats,
                        data = mydata,
                        x = !!group,
                        # ,
                        # type = distribution
                        , ggtheme = ggtheme
                        , ggstatsplot.layer = originaltheme

                    )

                plot <- ggstatsplot::combine_plots(plotlist = plotlist,
                                                   nrow = length(self$options$dep))



            }




            # Print Plot ----

            print(plot)
            TRUE

        }


        ,
        .plot2 = function(image, ggtheme, theme, ...) {

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


            vars <- self$options$dep


            for (var in vars)
                mydata[[var]] <- jmvcore::toNumeric(mydata[[var]])


            # Exclude NA ----

            excl <- self$options$excl

            if (excl) {mydata <- jmvcore::naOmit(mydata)}


            # read arguments ----

            dep <- self$options$dep

            group <- self$options$group

            originaltheme <- self$options$originaltheme


            dep <- jmvcore::composeTerm(components = dep)

            group <- jmvcore::composeTerm(components = group)




            # grouped_ggbetweenstats ----
            # https://indrajeetpatil.github.io/ggstatsplot/reference/grouped_ggbetweenstats.html


            # dep = 1 ----

            grvar <- self$options$grvar

            if (length(self$options$dep) == 1) {

                plot2 <- ggstatsplot::grouped_ggbetweenstats(
                    data = mydata,
                    x = !!group,
                    y = !! dep,
                    grouping.var = !!grvar
                    , ggtheme = ggtheme
                    , ggstatsplot.layer = originaltheme

                )

            }


            # dep > 1 ----

            if (length(self$options$dep) > 1) {
                dep2 <- as.list(self$options$dep)

                plotlist <-
                    purrr::pmap(
                        .l = list(y = dep2,
                                  # title = list(dep),
                                  messages = FALSE),
                        .f = ggstatsplot::grouped_ggbetweenstats,
                        data = mydata,
                        x = !!group,
                        grouping.var = !!grvar
                        , ggtheme = ggtheme
                        , ggstatsplot.layer = originaltheme

                    )

                plot2 <- ggstatsplot::combine_plots(plotlist = plotlist,
                                                    ncol = 1)

            }



            # Print Plot ----

            print(plot2)
            TRUE

        }




    )
)
