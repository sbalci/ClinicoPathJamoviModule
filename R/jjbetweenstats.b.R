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

            self$results$plot$setSize(650, deplen * 450)


            if (!is.null(self$options$grvar)) {

                mydata <- self$data

                grvar <-  self$options$grvar

                num_levels <- nlevels(
                    as.factor(mydata[[grvar]])
                )

                self$results$plot2$setSize(num_levels * 650, deplen * 450)

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


            # read data ----

            mydata <- self$data


            vars <- self$options$dep


            for (var in vars)
                mydata[[var]] <- jmvcore::toNumeric(mydata[[var]])


            # Exclude NA ----

            mydata <- jmvcore::naOmit(mydata)


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




            ## type of statistics ----

            typestatistics <-
                jmvcore::constructFormula(terms = self$options$typestatistics)


            pairwisecomparisons <- self$options$pairwisecomparisons

            pairwisedisplay <-
                jmvcore::constructFormula(terms = self$options$pairwisedisplay)

            padjustmethod <-
                jmvcore::constructFormula(terms = self$options$padjustmethod)

            # read arguments ----

            mytitle <- self$options$mytitle

            xtitle <- self$options$xtitle

            if (xtitle == '') {
                xtitle <- NULL
            }

            ytitle <- self$options$ytitle

            if (ytitle == '') {
                ytitle <- NULL
            }

            effsizetype <- self$options$effsizetype

            centralityplotting <- self$options$centralityplotting

            centralitytype <- self$options$centralitytype

            violin <- self$options$violin

            boxplot <- self$options$boxplot

            point <- self$options$point

            if (violin) {

                violinargs <- list(width = 0.5, alpha = 0.2, na.rm = TRUE)

                } else {

                violinargs <- list(width = 0)
            }


            if (boxplot) {
            boxplotargs <- list(width = 0.2, alpha = 0.5, na.rm = TRUE)
            } else {
            boxplotargs <- list(width = 0)
            }

            if (point) {
            pointargs <- list(alpha = 0.5, linetype = "dashed")
            } else {
            pointargs <- list(alpha = 0)
            }


            # ggbetweenstats ----
            # https://indrajeetpatil.github.io/ggstatsplot/reference/ggbetweenstats.html

            # dep == 1 ----

            if (length(self$options$dep) == 1) {

            plot <- ggstatsplot::ggbetweenstats(
                data = mydata,
                x = !!rlang::sym(group),
                y = !!rlang::sym(dep)

                    , title = mytitle
                    , xlab = xtitle
                    , ylab = ytitle

                , type = typestatistics
                    , pairwise.comparisons = pairwisecomparisons
                    , pairwise.display = pairwisedisplay
                    , p.adjust.method = padjustmethod
                    , effsize.type = effsizetype
                    , centrality.plotting = centralityplotting
                    , centrality.type = centralitytype
                    , violin.args = violinargs
                    , boxplot.args = boxplotargs
                    , point.args = pointargs


                )

            originaltheme <- self$options$originaltheme

            if (!originaltheme) {
                plot <- plot + ggtheme
            } else {
                plot <- plot + ggstatsplot::theme_ggstatsplot()
                # ggplot2::theme_bw()
            }


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
                    , title = mytitle
                    , xlab = xtitle
                    , ylab = ytitle
                    , pairwise.comparisons = pairwisecomparisons
                    , pairwise.display = pairwisedisplay
                    , p.adjust.method = padjustmethod
                    , effsize.type = effsizetype
                    , centrality.plotting = centralityplotting
                    , centrality.type = centralitytype
                    , violin.args = violinargs
                    , boxplot.args = boxplotargs
                    , point.args = pointargs


                    )
                        }
                    )

                originaltheme <- self$options$originaltheme


                # Assuming plotlist is a list of plots
                for (i in seq_along(plotlist)) {
                    if (!originaltheme) {
                        plotlist[[i]] <- plotlist[[i]] + ggtheme
                    } else {
                        plotlist[[i]] <- plotlist[[i]] + ggstatsplot::theme_ggstatsplot()
                    }
                }

                # Now combine the plots with the applied themes
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


            # read data ----

            mydata <- self$data


            vars <- self$options$dep


            for (var in vars)
                mydata[[var]] <- jmvcore::toNumeric(mydata[[var]])


            # Exclude NA ----

            mydata <- jmvcore::naOmit(mydata)



            # type of statistics ----


            typestatistics <-
                jmvcore::constructFormula(terms = self$options$typestatistics)


            pairwisecomparisons <- self$options$pairwisecomparisons

            pairwisedisplay <-
                jmvcore::constructFormula(terms = self$options$pairwisedisplay)

            padjustmethod <-
                jmvcore::constructFormula(terms = self$options$padjustmethod)


            # read arguments ----

            dep <- self$options$dep

            group <- self$options$group


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


            ## type of statistics ----

            typestatistics <-
                jmvcore::constructFormula(terms = self$options$typestatistics)


            pairwisecomparisons <- self$options$pairwisecomparisons

            pairwisedisplay <-
                jmvcore::constructFormula(terms = self$options$pairwisedisplay)

            padjustmethod <-
                jmvcore::constructFormula(terms = self$options$padjustmethod)

            # read arguments ----

            effsizetype <- self$options$effsizetype

            centralityplotting <- self$options$centralityplotting

            centralitytype <- self$options$centralitytype

            violin <- self$options$violin

            boxplot <- self$options$boxplot

            point <- self$options$point

            if (violin) {

                violinargs <- list(width = 0.5, alpha = 0.2, na.rm = TRUE)

            } else {

                violinargs <- list(width = 0)
            }


            if (boxplot) {
                boxplotargs <- list(width = 0.2, alpha = 0.5, na.rm = TRUE)
            } else {
                boxplotargs <- list(width = 0)
            }

            if (point) {
                pointargs <- list(alpha = 0.5, linetype = "dashed")
            } else {
                pointargs <- list(alpha = 0)
            }


            # grouped_ggbetweenstats ----
            # https://indrajeetpatil.github.io/ggstatsplot/reference/grouped_ggbetweenstats.html


            # dep = 1 ----

            grvar <- self$options$grvar

            originaltheme <- self$options$originaltheme

            if (length(self$options$dep) == 1) {

                selected_theme <- if (!originaltheme) ggtheme else ggstatsplot::theme_ggstatsplot()

                # Create plot2 with the selected theme
                plot2 <- ggstatsplot::grouped_ggbetweenstats(
                    data = mydata,
                    x = !!rlang::sym(group),
                    y = !!rlang::sym(dep),
                    grouping.var = !!rlang::sym(grvar),
                    type = typestatistics,
                    pairwise.comparisons = pairwisecomparisons,
                    pairwise.display = pairwisedisplay,
                    p.adjust.method = padjustmethod,
                    effsize.type = effsizetype,
                    centrality.plotting = centralityplotting,
                    centrality.type = centralitytype,
                    violin.args = violinargs,
                    boxplot.args = boxplotargs,
                    point.args = pointargs,
                    ggtheme = selected_theme

                )

            }


            # dep > 1 ----

            if (length(self$options$dep) > 1) {

                originaltheme <- self$options$originaltheme

                selected_theme <- if (!originaltheme) ggtheme else ggstatsplot::theme_ggstatsplot()

                # Convert 'dep' to a list and then to symbols
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
                        , pairwise.comparisons = pairwisecomparisons
                        , pairwise.display = pairwisedisplay
                        , p.adjust.method = padjustmethod
                        , effsize.type = effsizetype
                        , centrality.plotting = centralityplotting
                        , centrality.type = centralitytype
                        , violin.args = violinargs
                        , boxplot.args = boxplotargs
                        , point.args = pointargs
                        , ggtheme = selected_theme

                    )
                    }
                    )

                # Combine plots into a single plot
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
