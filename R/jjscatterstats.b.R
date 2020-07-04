#' @title Scatter Plot
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
            if ( is.null(self$options$dep) || is.null(self$options$group)) {

                # TODO ----

                todo <- glue::glue(
                "<br>Welcome to ClinicoPath
                <br><br>
                This tool will help you generate Scatter Plot.
                <br><br>
                This function uses ggplot2 and ggstatsplot packages. See documentations <a href = 'https://indrajeetpatil.github.io/ggstatsplot/reference/ggscatterstats.html' target='_blank'>ggscatterstats</a> and <a href = 'https://indrajeetpatil.github.io/ggstatsplot/reference/grouped_ggscatterstats.html' target='_blank'>grouped_ggscatterstats</a>.
                <br>
                Please cite jamovi and the packages as given below.
                <br><hr>"
                )

                self$results$todo$setContent(todo)

                return()

            } else {

                # TODO ----
                todo <- glue::glue(
                    "<br>You have selected to use a scatter plot.<br><hr>")

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


            dep <- jmvcore::composeTerm(components = dep)

            group <- jmvcore::composeTerm(components = group)


            # ggscatterstats ----
            # https://indrajeetpatil.github.io/ggstatsplot/reference/ggscatterstats.html



            plot <-
                ggstatsplot::ggscatterstats(
                    data = mydata,
                    x = !!dep,
                    y = !!group,
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
                    # xparams = list(fill = xfill),
                    # yparams = list(fill = yfill),
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
                    beta = 0.1,
                    k = 2L,
                    ggtheme = ggplot2::theme_bw(),
                    ggstatsplot.layer = TRUE,
                    ggplot.component = NULL,
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


            dep <- jmvcore::composeTerm(components = dep)

            group <- jmvcore::composeTerm(components = group)




            # grouped_ggscatterstats ----
            # https://indrajeetpatil.github.io/ggstatsplot/reference/grouped_ggscatterstats.html



            if ( !is.null(self$options$grvar) ) {

                grvar <- self$options$grvar

                plot2 <- ggstatsplot::grouped_ggscatterstats(
                    data = mydata,
                    x = !!dep,
                    y = !!group,
                    grouping.var = !!grvar,
                    label.var = NULL,
                    label.expression = NULL,
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
