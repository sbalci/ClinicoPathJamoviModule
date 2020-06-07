#' @title jjscatterstats
#'
#'
#'
#'
#' @importFrom R6 R6Class
#' @import jmvcore
#'



jjscatterstatsClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "jjscatterstatsClass",
    inherit = jjscatterstatsBase,
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
                This tool will help you generate Bar Plots.
                <br><br>
                This function uses ggplot2 and ggstatsplot packages. See documentations <a href = 'https://indrajeetpatil.github.io/ggstatsplot/reference/ggbarstats.html' target='_blank'>here</a> and <a href = 'https://indrajeetpatil.github.io/ggstatsplot/reference/grouped_ggbarstats.html' target='_blank'>here</a>.
                <br>
                Please cite jamovi and the packages as given below.
                <br><hr>"
                )

                self$results$todo$setContent(todo)

                return()

            } else {

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

            if (is.null(self$options$dep) ||
                is.null(self$options$group))
                return()

            if (nrow(self$data) == 0)
                stop('Data contains no (complete) rows')


            # Prepare Data ----

            direction <- self$options$direction

            # distribution <-
            #     jmvcore::constructFormula(terms = self$options$distribution)

            # pairw <- self$options$pairw


            mydata <- self$data


            # Exclude NA ----

            excl <- self$options$excl

            if (excl) {mydata <- jmvcore::naOmit(mydata)}



            mydep <- mydata[[self$options$dep]]
            mygroup <- mydata[[self$options$group]]

            if ( !is.null(self$options$grvar) ) {
                mygrvar <- mydata[[self$options$grvar]]
            }


            # ggscatterstats ----
            # https://indrajeetpatil.github.io/ggstatsplot/reference/ggscatterstats.html




            ggscatterstats(
                data,
                x,
                y,
                type = "parametric",
                conf.level = 0.95,
                bf.prior = 0.707,
                bf.message = TRUE,
                label.var = NULL,
                label.expression = NULL,
                point.label.args = list(size = 3),
                formula = y ~ x,
                smooth.line.args = list(size = 1.5, color = "blue"),
                method = "lm",
                method.args = list(),
                point.args = list(size = 3, alpha = 0.4),
                point.width.jitter = 0,
                point.height.jitter = 0,
                marginal = TRUE,
                marginal.type = "histogram",
                margins = "both",
                marginal.size = 5,
                xfill = "#009E73",
                yfill = "#D55E00",
                xparams = list(fill = xfill),
                yparams = list(fill = yfill),
                centrality.parameter = "none",
                centrality.label.args = list(size = 3),
                vline.args = list(color = xfill, size = 1, linetype = "dashed"),
                hline.args = list(color = yfill, size = 1, linetype = "dashed"),
                results.subtitle = TRUE,
                xlab = NULL,
                ylab = NULL,
                title = NULL,
                subtitle = NULL,
                caption = NULL,
                nboot = 100,
                beta = 0.1,
                k = 2,
                ggtheme = ggplot2::theme_bw(),
                ggstatsplot.layer = TRUE,
                ggplot.component = NULL,
                output = "plot",
                messages = TRUE,
                ...
            )





            # grouped_ggscatterstats ----
            # https://indrajeetpatil.github.io/ggstatsplot/reference/grouped_ggscatterstats.html


            grouped_ggscatterstats(
                data,
                x,
                y,
                grouping.var,
                label.var = NULL,
                label.expression = NULL,
                title.prefix = NULL,
                output = "plot",
                ...,
                plotgrid.args = list(),
                title.text = NULL,
                title.args = list(size = 16, fontface = "bold"),
                caption.text = NULL,
                caption.args = list(size = 10),
                sub.text = NULL,
                sub.args = list(size = 12)
            )





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
