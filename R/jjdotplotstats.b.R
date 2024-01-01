#' @title Dot Chart
#' @importFrom R6 R6Class
#' @import jmvcore
#'


jjdotplotstatsClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "jjdotplotstatsClass",
    inherit = jjdotplotstatsBase,
    private = list(


        # init ----

        .init = function() {

            deplen <- length(self$options$dep)

            self$results$plot$setSize(600, deplen * 450)


            if (!is.null(self$options$grvar)) {

                mydata <- self$data

                grvar <-  self$options$grvar

                num_levels <- nlevels(
                    as.factor(mydata[[grvar]])
                )

                self$results$plot2$setSize(num_levels * 600, deplen * 450)

            }

        }

        # run ----
        ,
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
                    "<br>You have selected to use a Dot Plot to compare continuous variables by groups.<br><hr>")

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


            vars <- self$options$dep


            for (var in vars)
                mydata[[var]] <- jmvcore::toNumeric(mydata[[var]])


            # Exclude NA ----

            excl <- self$options$excl

            if (excl) {mydata <- jmvcore::naOmit(mydata)}


            # type of statistics ----


            typestatistics <-
                jmvcore::constructFormula(terms = self$options$typestatistics)



            # define main arguments ----

            dep <- self$options$dep

            group <- self$options$group

            originaltheme <- self$options$originaltheme


            # dep <- jmvcore::composeTerm(components = dep)

            # group <- jmvcore::composeTerm(components = group)


            # ggdotplotstats ----
            # https://indrajeetpatil.github.io/ggstatsplot/reference/ggdotplotstats.html



            plot <-
                ggstatsplot::ggdotplotstats(
                    data = mydata,
                    x = !!rlang::sym(dep),
                    y = !!rlang::sym(group),
                    xlab = NULL,
                    ylab = NULL,
                    title = NULL,
                    subtitle = NULL,
                    caption = NULL,

                    type = typestatistics,

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
                    ggtheme = ggtheme,
                    # ggtheme = ggplot2::theme_bw(),
                    ggstatsplot.layer = originaltheme,
                    output = "plot",
                    messages = TRUE
                )


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


            # type of statistics ----


            typestatistics <-
                jmvcore::constructFormula(terms = self$options$typestatistics)


            # define main arguments ----


            dep <- self$options$dep

            group <- self$options$group


            originaltheme <- self$options$originaltheme


            # dep <- jmvcore::composeTerm(components = dep)

            # group <- jmvcore::composeTerm(components = group)


            # grouped_ggdotplotstats ----
            # https://indrajeetpatil.github.io/ggstatsplot/reference/grouped_ggdotplotstats.html



            if ( !is.null(self$options$grvar) ) {
                grvar <- self$options$grvar

                plot2 <- ggstatsplot::grouped_ggdotplotstats(
                    data = mydata,
                    x = !!rlang::sym(dep),
                    y = !!rlang::sym(group),
                    grouping.var = !!rlang::sym(grvar),
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
                    , type = typestatistics


                )


            }

            # Print Plot ----

            print(plot2)
            TRUE

        }





    )
)








