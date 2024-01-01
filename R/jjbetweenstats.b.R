#' @title Violin Plots to Compare Between Groups
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


            # type of statistics ----


            typestatistics <-
                jmvcore::constructFormula(terms = self$options$typestatistics)


            plottype <-
                jmvcore::constructFormula(terms = self$options$plottype)

            originaltheme <- self$options$originaltheme

            pairwisecomparisons <- self$options$pairwisecomparisons

            pairwisedisplay <-
                jmvcore::constructFormula(terms = self$options$pairwisedisplay)

            padjustmethod <-
                jmvcore::constructFormula(terms = self$options$padjustmethod)


            # ADD HERE ----



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



            # # mydataview ----
            # self$results$mydataview$setContent(
            #     list(
            #         dep1 = dep,
            #         group = group,
            #         mydata = head(mydata)
            #         )
            # )

            # dep <- jmvcore::composeTerm(components = dep)

            # group <- jmvcore::composeTerm(components = group)

            # ggbetweenstats ----
            # https://indrajeetpatil.github.io/ggstatsplot/reference/ggbetweenstats.html

            # dep == 1 ----

            if (length(self$options$dep) == 1) {

            plot <- ggstatsplot::ggbetweenstats(
                data = mydata,
                x = !!rlang::sym(group),
                y = !!rlang::sym(dep)

                , type = typestatistics
                , ggtheme = ggtheme
                , ggstatsplot.layer = originaltheme
                , plot.type = plottype
                , pairwise.comparisons = pairwisecomparisons
                , pairwise.display = pairwisedisplay
                , p.adjust.method = padjustmethod

                ,
                effsize.type = "unbiased",
                bf.prior = 0.707,
                bf.message = TRUE,
                results.subtitle = TRUE,
                xlab = NULL,
                ylab = NULL,
                caption = NULL,
                title = NULL,
                subtitle = NULL,
                sample.size.label = TRUE,
                k = 2L,
                var.equal = FALSE,
                conf.level = 0.95,
                nboot = 100L,
                tr = 0.1,
                mean.plotting = TRUE,
                mean.ci = FALSE,
                mean.point.args = list(size = 5, color = "darkred"),
                mean.label.args = list(size = 3),
                notch = FALSE,
                notchwidth = 0.5,
                outlier.tagging = FALSE,
                outlier.label = NULL,
                outlier.coef = 1.5,
                outlier.shape = 19,
                outlier.color = "black",
                outlier.label.args = list(size = 3),
                outlier.point.args = list(),
                point.args = list(
                    position = ggplot2::position_jitterdodge(dodge.width = 0.6),
                    alpha = 0.4, size = 3, stroke = 0),
                violin.args = list(width = 0.5, alpha = 0.2),
                ggsignif.args = list(textsize = 3, tip_length = 0.01),

                package = "RColorBrewer",
                palette = "Dark2",
                ggplot.component = NULL,
                output = "plot"
            )

            }



            # dep > 1 ----

            if (length(self$options$dep) > 1) {
                dep2 <- as.list(self$options$dep)
                dep2_symbols <- purrr::map(dep2, rlang::sym)

                plotlist <-
                    purrr::pmap(
                        .l = list(
                            y = dep2_symbols,
                            messages = FALSE),

                        .f = function(y, messages) {
                            ggstatsplot::ggbetweenstats(
                                data = mydata,
                                y = !!y,
                                messages = messages,
                        x = !!rlang::sym(group)
                        , type = typestatistics
                        , ggtheme = ggtheme
                        , ggstatsplot.layer = originaltheme
                        , plot.type = plottype
                        , pairwise.comparisons = pairwisecomparisons
                        , pairwise.display = pairwisedisplay
                        , p.adjust.method = padjustmethod

                    )
                        }
                    )

                plot <- ggstatsplot::combine_plots(
                    plotlist = plotlist,
                            plotgrid.args = list(ncol = 1)
                            )

        }

            ## Print Plot ----

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

            # pairw <- self$options$pairw


            # type of statistics ----


            typestatistics <-
                jmvcore::constructFormula(terms = self$options$typestatistics)



            plottype <-
                jmvcore::constructFormula(terms = self$options$plottype)


            originaltheme <- self$options$originaltheme

            pairwisecomparisons <- self$options$pairwisecomparisons

            pairwisedisplay <-
                jmvcore::constructFormula(terms = self$options$pairwisedisplay)

            padjustmethod <-
                jmvcore::constructFormula(terms = self$options$padjustmethod)

            # ADD HERE ----



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



            # dep <- jmvcore::composeTerm(components = dep)

            # group <- jmvcore::composeTerm(components = group)




            # grouped_ggbetweenstats ----
            # https://indrajeetpatil.github.io/ggstatsplot/reference/grouped_ggbetweenstats.html


            # dep = 1 ----

            grvar <- self$options$grvar

            if (length(self$options$dep) == 1) {

                plot2 <- ggstatsplot::grouped_ggbetweenstats(
                    data = mydata,
                    x = !!rlang::sym(group),
                    y = !!rlang::sym(dep),
                    grouping.var = !!rlang::sym(grvar)
                    , type = typestatistics
                    , ggtheme = ggtheme
                    , ggstatsplot.layer = originaltheme
                    , plot.type = plottype
                    , pairwise.comparisons = pairwisecomparisons
                    , pairwise.display = pairwisedisplay
                    , p.adjust.method = padjustmethod

                )

            }


            # dep > 1 ----

            if (length(self$options$dep) > 1) {

                dep2 <- as.list(self$options$dep)
                dep2_symbols <- purrr::map(dep2, rlang::sym)

                plotlist <-
                    purrr::pmap(
                        .l = list(
                            y = dep2_symbols,
                            messages = FALSE
                            ),
                        .f = function(y, messages) {
                            ggstatsplot::grouped_ggbetweenstats(

                        data = mydata,
                        y = !!y,
                        messages = messages,



                        x = !!rlang::sym(group),
                        grouping.var = !!rlang::sym(grvar)
                        , type = typestatistics
                        , ggtheme = ggtheme
                        , ggstatsplot.layer = originaltheme
                        , plot.type = plottype
                        , pairwise.comparisons = pairwisecomparisons
                        , pairwise.display = pairwisedisplay
                        , p.adjust.method = padjustmethod


                    )
                    }
                    )


                plot2 <- ggstatsplot::combine_plots(
                    plotlist = plotlist,
                            plotgrid.args = list(ncol = 1)
                            )


            }

            ## Print Plot ----

            print(plot2)
            TRUE

        }

    )

)
