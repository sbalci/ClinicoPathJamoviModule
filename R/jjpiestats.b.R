#' @title Pie Charts
#'
#'
#'
#'
#' @importFrom R6 R6Class
#' @import jmvcore
#'

jjpiestatsClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "jjpiestatsClass",
    inherit = jjpiestatsBase,
    private = list(

        .run = function() {

            # Initial Message ----
            if ( is.null(self$options$dep) ) {

                # TODO ----

                todo <- glue::glue(
                "<br>Welcome to ClinicoPath
                <br><br>
                This tool will help you generate Pie Charts.
                <br><br>
                This function uses ggplot2 and ggstatsplot packages. See documentations <a href = 'https://indrajeetpatil.github.io/ggstatsplot/reference/ggpiestats.html' target='_blank'>ggpiestats</a> and <a href = 'https://indrajeetpatil.github.io/ggstatsplot/reference/grouped_ggpiestats.html' target='_blank'>grouped_ggpiestats</a>.
                <br>
                Please cite jamovi and the packages as given below.
                <br><hr>"
                )

                self$results$todo$setContent(todo)

                return()

            } else {

                # TODO ----
                todo <- glue::glue(
                    "<br>You have selected to use Pie Charts.<br><hr>")

                self$results$todo$setContent(todo)

                if (nrow(self$data) == 0)
                    stop('Data contains no (complete) rows')

            }
        }


        # the plot1 function ----


        ,
        .plot1 = function(image, ggtheme, theme, ...) {

            # Error messages ----

            if ( is.null(self$options$dep) )
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


            mydata <- self$data


            # Exclude NA ----

            excl <- self$options$excl

            if (excl) {mydata <- jmvcore::naOmit(mydata)}



            # mydep <- mydata[[self$options$dep]]
            # mygroup <- mydata[[self$options$group]]


            dep <- self$options$dep

            # group <- self$options$group


            dep <- jmvcore::composeTerm(components = dep)


            # group <- jmvcore::composeTerm(components = group)


            originaltheme <- self$options$originaltheme




            # ggpiestats ----
            # https://indrajeetpatil.github.io/ggstatsplot/reference/ggpiestats.html



            plot1 <-
                ggstatsplot::ggpiestats(
                    data = mydata,
                    x = !!dep,
                    y = NULL,
                    counts = NULL,
                    ratio = NULL,
                    paired = FALSE,
                    results.subtitle = TRUE,
                    label = "percentage",
                    perc.k = 0,
                    label.args = list(alpha = 1, fill = "white"),
                    bf.message = TRUE,
                    sampling.plan = "indepMulti",
                    fixed.margin = "rows",
                    prior.concentration = 1,
                    title = NULL,
                    subtitle = NULL,
                    caption = NULL,
                    conf.level = 0.95,
                    nboot = 100,
                    legend.title = NULL,
                    k = 2,
                    proportion.test = TRUE,
                    ggtheme = ggtheme,

                    # ggtheme = ggplot2::theme_bw(),
                    ggstatsplot.layer = originaltheme,
                    package = "RColorBrewer",
                    palette = "Dark2",
                    ggplot.component = NULL,
                    output = "plot",
                    messages = TRUE
                    )



            # Print Plot1 ----

            print(plot1)
            TRUE

        }


        # the plot2 function ----


        , .plot2 = function(image, ggtheme, theme, ...) {

            # Error messages ----

            if ( is.null(self$options$dep) || is.null(self$options$group) )
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


            mydata <- self$data


            # Exclude NA ----

            excl <- self$options$excl

            if (excl) {mydata <- jmvcore::naOmit(mydata)}



            # mydep <- mydata[[self$options$dep]]
            # mygroup <- mydata[[self$options$group]]


            dep <- self$options$dep

            group <- self$options$group

            originaltheme <- self$options$originaltheme


            dep <- jmvcore::composeTerm(components = dep)

            group <- jmvcore::composeTerm(components = group)


            # ggpiestats ----
            # https://indrajeetpatil.github.io/ggstatsplot/reference/ggpiestats.html

            plot2 <-
                ggstatsplot::ggpiestats(
                    data = mydata,
                    x = !!dep,
                    y = !!group,
                    counts = NULL,
                    ratio = NULL,
                    paired = FALSE,
                    results.subtitle = TRUE,
                    label = "percentage",
                    perc.k = 0,
                    label.args = list(alpha = 1, fill = "white"),
                    bf.message = TRUE,
                    sampling.plan = "indepMulti",
                    fixed.margin = "rows",
                    prior.concentration = 1,
                    title = NULL,
                    subtitle = NULL,
                    caption = NULL,
                    conf.level = 0.95,
                    nboot = 100,
                    legend.title = NULL,
                    k = 2,
                    proportion.test = TRUE,
                    ggtheme = ggtheme,

                    # ggtheme = ggplot2::theme_bw(),
                    ggstatsplot.layer = originaltheme,
                    package = "RColorBrewer",
                    palette = "Dark2",
                    ggplot.component = NULL,
                    output = "plot",
                    messages = TRUE
                )


            # Print Plot2 ----
            print(plot2)
            TRUE
        }



        # the plot3 function ----



        , .plot3 = function(image, ggtheme, theme, ...) {

            # Error messages ----

            if ( is.null(self$options$dep) || is.null(self$options$grvar) )
                return()

            if (nrow(self$data) == 0)
                stop('Data contains no (complete) rows')


            # Prepare Data ----

            mydata <- self$data


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

            # Exclude NA ----

            excl <- self$options$excl

            if (excl) {mydata <- jmvcore::naOmit(mydata)}


            dep <- self$options$dep

            # group <- self$options$group


            dep <- jmvcore::composeTerm(components = dep)

            # group <- jmvcore::composeTerm(components = group)



            originaltheme <- self$options$originaltheme


            # grouped_ggpiestats ----
            # https://indrajeetpatil.github.io/ggstatsplot/reference/grouped_ggpiestats.html


            if ( !is.null(self$options$grvar) ) {

            grvar <- self$options$grvar

            grvar <- jmvcore::composeTerm(components = grvar)


                plot3 <- ggstatsplot::grouped_ggpiestats(
                    data = mydata,
                    x = !!dep,
                    y = NULL,
                    counts = NULL,
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
                    , ggtheme = ggtheme
                    , ggstatsplot.layer = originaltheme

                )
}


            # Print Plot3 ----
            print(plot3)
            TRUE
        }



# the plot4 function ----




        , .plot4 = function(image, ggtheme, theme, ...) {

            # Error messages ----

            if ( is.null(self$options$dep) || is.null(self$options$group) ||is.null(self$options$grvar) )
                return()

            if (nrow(self$data) == 0)
                stop('Data contains no (complete) rows')


            # Prepare Data ----

            mydata <- self$data


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

            # Exclude NA ----

            excl <- self$options$excl

            if (excl) {mydata <- jmvcore::naOmit(mydata)}



            dep <- self$options$dep

            group <- self$options$group

            originaltheme <- self$options$originaltheme


            dep <- jmvcore::composeTerm(components = dep)

            group <- jmvcore::composeTerm(components = group)


            # grouped_ggpiestats ----
            # https://indrajeetpatil.github.io/ggstatsplot/reference/grouped_ggpiestats.html

            if ( !is.null(self$options$grvar) ) {

                grvar <- self$options$grvar

                grvar <- jmvcore::composeTerm(components = grvar)


                plot4 <- ggstatsplot::grouped_ggpiestats(
                    data = mydata,
                    x = !!dep,
                    y = !!group,
                    counts = NULL,
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
                    , ggtheme = ggtheme
                    , ggstatsplot.layer = originaltheme

                )
            }

            # Print Plot4 ----
            print(plot4)
            TRUE
        }

    )
)
