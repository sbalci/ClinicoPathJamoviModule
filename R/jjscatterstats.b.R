#' @title Scatter Plot
#' @importFrom R6 R6Class
#' @import jmvcore
#'



jjscatterstatsClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "jjscatterstatsClass",
    inherit = jjscatterstatsBase,
    private = list(


        # init ----

        .init = function() {

            self$results$plot$setSize(600, 450)


            if (!is.null(self$options$grvar)) {

                mydata <- self$data

                grvar <-  self$options$grvar

                num_levels <- nlevels(
                    as.factor(mydata[[grvar]])
                )

                self$results$plot2$setSize(num_levels * 600, 450)

            }

        }

        # run ----
        ,
        .run = function() {

            # Initial Message ----
            if ( is.null(self$options$dep) || is.null(self$options$group)) {

                # todo ----

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

                # todo ----
                todo <- glue::glue(
                    "<br>You have selected to use a scatter plot.<br><hr>")

                self$results$todo$setContent(todo)

                if (nrow(self$data) == 0)
                    stop('Data contains no (complete) rows')

            }
        }



        ,
        .plot = function(image, ggtheme, theme, ...) {
            # the plot function ----
            # Error messages ----

            if ( is.null(self$options$dep) || is.null(self$options$group))
                return()

            if (nrow(self$data) == 0)
                stop('Data contains no (complete) rows')


            # Prepare Data ----


            mydata <- self$data


            vars <- self$options$dep


            for (var in vars)
                mydata[[var]] <- jmvcore::toNumeric(mydata[[var]])


            # Exclude NA ----

            mydata <- jmvcore::naOmit(mydata)


            # type of statistics ----


            typestatistics <-
                jmvcore::constructFormula(terms = self$options$typestatistics)


            dep <- self$options$dep

            group <- self$options$group



            mytitle <- self$options$mytitle

            if (mytitle == '') {
                mytitle <- NULL
            }


            xtitle <- self$options$xtitle

            if (xtitle == '') {
                xtitle <- NULL
            }

            ytitle <- self$options$ytitle

            if (ytitle == '') {
                ytitle <- NULL
            }


            # ggscatterstats ----
            # https://indrajeetpatil.github.io/ggstatsplot/reference/ggscatterstats.html



            plot <-
                ggstatsplot::ggscatterstats(
                    data = mydata,
                    x = !!rlang::sym(dep),
                    y = !!rlang::sym(group)

                    , type = typestatistics

                    , title = mytitle
                    , xlab = xtitle
                    , ylab = ytitle

                    ,
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

                    subtitle = NULL,
                    caption = NULL,
                    beta = 0.1,
                    k = 2L,

                    ggplot.component = NULL,
                    output = "plot",
                    messages = TRUE
                )



            originaltheme <- self$options$originaltheme

            if (!originaltheme) {
                plot <- plot + ggtheme
            } else {
                plot <- plot + ggstatsplot::theme_ggstatsplot()
                # ggplot2::theme_bw()
            }

            # Print Plot ----

            print(plot)
            TRUE

        }


        ,
        .plot2 = function(image, ggtheme, theme, ...) {
            # the plot function ----
            # Error messages ----

            if ( is.null(self$options$dep) || is.null(self$options$group) || is.null(self$options$grvar))
                return()

            if (nrow(self$data) == 0)
                stop('Data contains no (complete) rows')


            # Prepare Data ----

            mydata <- self$data

            vars <- self$options$dep


            for (var in vars)
                mydata[[var]] <- jmvcore::toNumeric(mydata[[var]])


            # Exclude NA ----

            mydata <- jmvcore::naOmit(mydata)


            # type of statistics ----


            typestatistics <-
                jmvcore::constructFormula(terms = self$options$typestatistics)

            dep <- self$options$dep

            group <- self$options$group


            mytitle <- self$options$mytitle

            if (mytitle == '') {
                mytitle <- NULL
            }


            # grouped_ggscatterstats ----
            # https://indrajeetpatil.github.io/ggstatsplot/reference/grouped_ggscatterstats.html



            if ( !is.null(self$options$grvar) ) {

                grvar <- self$options$grvar

                plot2 <- ggstatsplot::grouped_ggscatterstats(
                    data = mydata,
                    x = !!rlang::sym(dep),
                    y = !!rlang::sym(group),
                    grouping.var = !!rlang::sym(grvar),
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
                    , ggtheme = ggtheme
                    , type = typestatistics
                    , title = mytitle


                )
            }


            originaltheme <- self$options$originaltheme

            if (!originaltheme) {
                plot2 <- plot2 + ggtheme
            } else {
                plot2 <- plot2 + ggstatsplot::theme_ggstatsplot()
                # ggplot2::theme_bw()
            }

            # Print Plot ----
            print(plot2)
            TRUE
        }

    )
)
