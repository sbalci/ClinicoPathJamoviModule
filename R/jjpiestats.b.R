#' @title jjpiestats
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
            if (is.null(self$options$dep) ) {
                # TODO ----

                todo <- glue::glue(
                    "
                <br>Welcome to ClinicoPath
                <br><br>
                This tool will help you generate Bar Plots.
                <br><br>
                This function uses ggplot2 and ggstatsplot packages. See documentations <a href = 'https://indrajeetpatil.github.io/ggstatsplot/reference/ggpiestats.html' target='_blank'>here</a> and <a href = 'https://indrajeetpatil.github.io/ggstatsplot/reference/grouped_ggpiestats.html' target='_blank'>here</a>.
                <br>
                Please cite jamovi and the packages as given below.
                <br><hr>"
                )

                self$results$todo$setContent(todo)

                return()

            } else {

                todo <- glue::glue(
                    "<br>You have selected to use a Pie Chart to compare a categorical variable with another.<br><hr>")

                self$results$todo$setContent(todo)

                if (nrow(self$data) == 0)
                    stop('Data contains no (complete) rows')

            }
        }

            ,


            .plot = function(image, ...) {
                # the plot function ----
                # Error messages ----

                if (is.null(self$options$dep))
                    return()

                if (nrow(self$data) == 0)
                    stop('Data contains no (complete) rows')


                # Prepare Data ----

                # direction <- self$options$direction

                # distribution <-
                #     jmvcore::constructFormula(terms = self$options$distribution)

                # pairw <- self$options$pairw


                mydata <- self$data


                # Exclude NA ----

                excl <- self$options$excl

                if (excl) {mydata <- jmvcore::naOmit(mydata)}


                mydep <- mydata[[self$options$dep]]

                if ( !is.null(self$options$group) ) {
                mygroup <- mydata[[self$options$group]]
                }

                if ( !is.null(self$options$grvar) ) {
                    mygrvar <- mydata[[self$options$grvar]]
                }


                # plotData <- data.frame(gr = mygroup,
                #                        dp = mydep)

                # ggpiestats ----
                # https://indrajeetpatil.github.io/ggstatsplot/reference/ggpiestats.html

                # one variable ----


                plotData <- data.frame(dp = mydep)

                plot <- ggstatsplot::ggpiestats(
                    data = plotData,
                    main = dp,
                    condition = NULL,
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
                    ggtheme = ggplot2::theme_bw(),
                    ggstatsplot.layer = TRUE,
                    package = "RColorBrewer",
                    palette = "Dark2",
                    ggplot.component = NULL,
                    output = "plot",
                    messages = TRUE,
                    x = NULL,
                    y = NULL
                )



                # two variable ----

                if ( !is.null(self$options$group) ) {

                plotData <- data.frame(gr = mygroup,
                                       dp = mydep)

                plot <- ggstatsplot::ggpiestats(
                    data = plotData,
                    main = dp,
                    condition = gr
                )

                }










                # grouped_ggpiestats ----
                # https://indrajeetpatil.github.io/ggstatsplot/reference/grouped_ggpiestats.html



                if ( !is.null(self$options$group) && !is.null(self$options$grvar) ) {

                    plotData <- data.frame(gr = mygroup,
                                           dp = mydep,
                                           grvar = mygrvar )



                plot <- ggstatsplot::grouped_ggpiestats(
                    data = plotData,
                    main = dp,
                    condition = gr,
                    counts = NULL,
                    grouping.var = mygrvar,
                    title.prefix = NULL,
                    output = "plot",
                    x = NULL,
                    y = NULL,
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

                print(plot)
                TRUE

            }


            )
)

#' @title Bar Charts
#'
#'
#'
#' @importFrom R6 R6Class
#' @import jmvcore
#'


jjbarstatsClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "jjbarstatsClass",
    inherit = jjbarstatsBase,
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
                This function uses ggplot2 and ggstatsplot packages. See documentations <a href = 'https://indrajeetpatil.github.io/ggstatsplot/reference/ggbarstats.html' target='_blank'>here</a> and <a href = 'https://indrajeetpatil.github.io/ggstatsplot/reference/grouped_ggbarstats.html' target='_blank'>here</a>.
                <br>
                Please cite jamovi and the packages as given below.
                <br><hr>"
                )

                self$results$todo$setContent(todo)

                return()

            } else {

                # TODO ----
                todo <- glue::glue(
                    "<br>You have selected to use a barplot to compare a categorical variable with another.<br><hr>")

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



            # mydep <- mydata[[self$options$dep]]
            # mygroup <- mydata[[self$options$group]]


            dep <- self$options$dep

            group <- self$options$group


            dep <- jmvcore::composeTerm(components = dep)

            group <- jmvcore::composeTerm(components = group)


            # ggbarstats ----
            # bar charts for categorical data
            # https://indrajeetpatil.github.io/ggstatsplot/reference/ggbarstats.html



            plot <-
                ggstatsplot::ggbarstats(
                    data = mydata,
                    main = !!dep,
                    condition = !!group,

                    paired = paired,


                    counts = NULL,
                    ratio = NULL,
                    results.subtitle = TRUE,
                    sample.size.label = TRUE,
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
                    xlab = NULL,
                    ylab = NULL,
                    k = 2,
                    proportion.test = TRUE,
                    ggtheme = ggplot2::theme_bw(),
                    ggstatsplot.layer = TRUE,
                    package = "RColorBrewer",
                    palette = "Dark2",
                    ggplot.component = NULL,
                    output = "plot",
                    messages = TRUE,
                    x = NULL,
                    y = NULL
                )


            # Print Plot ----

            print(plot)
            TRUE

        }


        ,             .plot2 = function(image, ...) {
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



            dep <- self$options$dep

            group <- self$options$group


            dep <- jmvcore::composeTerm(components = dep)

            group <- jmvcore::composeTerm(components = group)




            # grouped_ggbarstats ----
            # https://indrajeetpatil.github.io/ggstatsplot/reference/grouped_ggbarstats.html



            if ( !is.null(self$options$grvar) ) {
                grvar <- self$options$grvar

                plot2 <- ggstatsplot::grouped_ggbarstats(
                    data = mydata,
                    main = !!dep,
                    condition = !!group,
                    grouping.var = !!grvar,

                    paired = paired,


                    counts = NULL,
                    title.prefix = NULL,
                    output = "plot",
                    x = NULL,
                    y = NULL,
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
