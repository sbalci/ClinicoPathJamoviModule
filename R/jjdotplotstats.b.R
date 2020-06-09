#' @title Dot Chart
#'
#'
#' @importFrom R6 R6Class
#' @import jmvcore
#'


jjdotplotstatsClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "jjdotplotstatsClass",
    inherit = jjdotplotstatsBase,
    private = list(

        .run = function() {

            # Initial Message ----
            if ( is.null(self$options$dep) || is.null(self$options$group)) {

                # TODO ----

                todo <- glue::glue(
                "<br>Welcome to ClinicoPath
                <br><br>
                This tool will help you generate Bar Charts.
                <br><br>
                This function uses ggplot2 and ggstatsplot packages. See documentations for <a href = 'https://indrajeetpatil.github.io/ggstatsplot/reference/ggdotplotstats.html' target='_blank'>ggdotplotstats</a> and <a href = 'https://indrajeetpatil.github.io/ggstatsplot/reference/grouped_ggdotplotstats.html' target='_blank'>grouped_ggdotplotstats</a>.
                <br>
                Please cite jamovi and the packages as given below.
                <br><hr>"
                )

                self$results$todo$setContent(todo)

                return()

            } else {

                # TODO ----
                todo <- glue::glue(
                    "<br>You have selected to use a Dot Plot to compare continious variables by groups.<br><hr>")

                self$results$todo$setContent(todo)

                if (nrow(self$data) == 0)
                    stop('Data contains no (complete) rows')

            }
        }


        ,
        .plot = function(image, ...) {
            # the plot function ----
            # Error messages ----

            if ( is.null(self$options$dep) || is.null(self$options$group))
                return()

            if (nrow(self$data) == 0)
                stop('Data contains no (complete) rows')


            # Prepare Data ----


            # direction, paired ----

            direction <- self$options$direction

            if (direction == "repeated") {

                paired <- TRUE

            } else if (direction == "independent") {

                paired <- FALSE

            }


            # distribution <-
            #     jmvcore::constructFormula(terms = self$options$distribution)

            # pairw <- self$options$pairw


            mydata <- self$data


            # Exclude NA ----

            excl <- self$options$excl

            if (excl) {mydata <- jmvcore::naOmit(mydata)}


            # define main arguments ----

            dep <- self$options$dep

            group <- self$options$group


            dep <- jmvcore::composeTerm(components = dep)

            group <- jmvcore::composeTerm(components = group)


            # ggdotplotstats ----
            # https://indrajeetpatil.github.io/ggstatsplot/reference/ggdotplotstats.html



            plot <-
                ggstatsplot::ggdotplotstats(
                    data = mydata,
                    x = !!dep,
                    y = !!group,
                    xlab = NULL,
                    ylab = NULL,
                    title = NULL,
                    subtitle = NULL,
                    caption = NULL,
                    type = "parametric",
                    test.value = 0,
                    bf.prior = 0.707,
                    bf.message = TRUE,
                    effsize.type = "g",
                    conf.level = 0.95,
                    nboot = 100,
                    k = 2,
                    results.subtitle = TRUE,
                    point.args = list(color = "black", size = 3, shape = 16),
                    test.k = 0,
                    test.value.line = FALSE,
                    test.value.line.args = list(size = 1),
                    test.value.label.args = list(size = 3),
                    centrality.parameter = "mean",
                    centrality.k = 2,
                    centrality.line.args = list(color = "blue", size = 1),
                    centrality.label.args = list(color = "blue", size = 3),
                    ggplot.component = NULL,
                    ggtheme = ggplot2::theme_bw(),
                    ggstatsplot.layer = TRUE,
                    output = "plot",
                    messages = TRUE
                )


            # Print Plot ----

            print(plot)
            TRUE

        }


        ,

        .plot2 = function(image, ...) {
            # the plot function ----
            # Error messages ----

            if ( is.null(self$options$dep) || is.null(self$options$group) || is.null(self$options$grvar))
                return()

            if (nrow(self$data) == 0)
                stop('Data contains no (complete) rows')


            # Prepare Data ----

            mydata <- self$data


            # direction, paired ----

            direction <- self$options$direction

            if (direction == "repeated") {

                paired <- TRUE

            } else if (direction == "independent") {

                paired <- FALSE

            }

            # Exclude NA ----

            excl <- self$options$excl

            if (excl) {mydata <- jmvcore::naOmit(mydata)}


            # define main arguments ----


            dep <- self$options$dep

            group <- self$options$group


            dep <- jmvcore::composeTerm(components = dep)

            group <- jmvcore::composeTerm(components = group)




            # grouped_ggdotplotstats ----
            # https://indrajeetpatil.github.io/ggstatsplot/reference/grouped_ggdotplotstats.html



            if ( !is.null(self$options$grvar) ) {
                grvar <- self$options$grvar

                plot2 <- ggstatsplot::grouped_ggdotplotstats(
                    data = mydata,
                    x = !!dep,
                    y = !!group,
                    grouping.var = !!grvar,
                    title.prefix = NULL,
                    output = "plot",
                    plotgrid.args = list(),
                    title.text = NULL,
                    title.args = list(size = 16, fontface = "bold"),
                    caption.text = NULL,
                    caption.args = list(size = 10),
                    sub.text = NULL,
                    sub.args = list(size = 12)
                )


            }

            # Print Plot ----

            print(plot2)
            TRUE

        }





    )
)








