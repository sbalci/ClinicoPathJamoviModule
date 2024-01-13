#' @title Bar Charts
#'
#' @importFrom R6 R6Class
#' @import jmvcore
#'

jjbarstatsClass <- if (requireNamespace('jmvcore'))
    R6::R6Class(
        "jjbarstatsClass",
        inherit = jjbarstatsBase,
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
                    # todo ----

                    todo <- glue::glue(
                        "<br>Welcome to ClinicoPath
                <br><br>
                This tool will help you generate Bar Charts.
                <br><br>
                This function uses ggplot2 and ggstatsplot packages. See documentations <a href = 'https://indrajeetpatil.github.io/ggstatsplot/reference/ggbarstats.html' target='_blank'>ggbarstats</a> and <a href = 'https://indrajeetpatil.github.io/ggstatsplot/reference/grouped_ggbarstats.html' target='_blank'>grouped_ggbarstats</a>.
                <br>
                Please cite jamovi and the packages as given below.
                <br><hr>"
                    )

                    self$results$todo$setContent(todo)

                    return()

                } else {
                    # todo ----
                    todo <- glue::glue(
                        "<br>You have selected to use a barplot to compare a categorical variable with another.<br><hr>"
                    )

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

                mydata <- self$data


                # Exclude NA ----

                mydata <- jmvcore::naOmit(mydata)

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


                # direction, paired ----

                direction <- self$options$direction

                if (direction == "repeated") {
                    paired <- TRUE
                } else if (direction == "independent") {
                    paired <- FALSE
                }

                # ggbarstats ----
                # bar charts for categorical data
                # https://indrajeetpatil.github.io/ggstatsplot/reference/ggbarstats.html


                # originaltheme <- self$options$originaltheme
                #
                # selected_theme <- if (!originaltheme) ggtheme else ggstatsplot::theme_ggstatsplot()

                # dep == 1 ----

                if (length(self$options$dep) == 1) {
                    plot <-
                        ggstatsplot::ggbarstats(
                            data = mydata,
                            x = !!rlang::sym(dep),
                            y = !!rlang::sym(group)

                            , title = mytitle
                            , xlab = xtitle
                            , ylab = ytitle
                            , paired = paired
                            , results.subtitle = self$options$resultssubtitle

                            # , ggtheme = selected_theme
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
                                x = dep2_symbols,
                                messages = FALSE
                                ),
                            .f = function(x, messages) {
                                 ggstatsplot::ggbarstats(
                                    data = mydata,
                                        x = !!x,
                                        messages = messages,
                            y = !!rlang::sym(group),

                            , title = mytitle
                            , paired = paired
                            , results.subtitle = self$options$resultssubtitle

                            # , ggtheme = selected_theme

                        )
                            }
                        )

                    originaltheme <- self$options$originaltheme


                    # Assuming plotlist is a list of plots
                    for (i in seq_along(plotlist)) {
                        if (!originaltheme) {
                            plotlist[[i]] <- plotlist[[i]] + ggtheme
                        } else {
                            plotlist[[i]] <- plotlist[[i]] +
                                ggstatsplot::theme_ggstatsplot()
                        }
                    }

                    plot <- ggstatsplot::combine_plots(
                        plotlist = plotlist,
                        plotgrid.args = list(ncol = 1)
                    )




                }

            #     originaltheme <- self$options$originaltheme
            #
            # if (!originaltheme) {
            #     plot <- plot + ggtheme
            # } else {
            #     plot <- plot + ggstatsplot::theme_ggstatsplot()
            #     # ggplot2::theme_bw()
            # }

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

                mydata <- self$data


                # Exclude NA ----

                mydata <- jmvcore::naOmit(mydata)

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


                # direction, paired ----

                direction <- self$options$direction

                if (direction == "repeated") {
                    paired <- TRUE
                } else if (direction == "independent") {
                    paired <- FALSE
                }


                # grouped_ggbarstats ----
                # https://indrajeetpatil.github.io/ggstatsplot/reference/grouped_ggbarstats.html


                dep1 <- self$options$dep

                grvar <- self$options$grvar

                # originaltheme <- self$options$originaltheme
                #
                # selected_theme <- if (!originaltheme) ggtheme else ggstatsplot::theme_ggstatsplot()


                # dep = 1 ----

                originaltheme <- self$options$originaltheme

                if (length(self$options$dep) == 1) {

                    selected_theme <- if (!originaltheme) ggtheme else ggstatsplot::theme_ggstatsplot()

                    plot2 <- ggstatsplot::grouped_ggbarstats(
                        data = mydata,
                        x = !!rlang::sym(dep1),
                        y = !!rlang::sym(group),
                        grouping.var = !!rlang::sym(grvar),

                        # , title = mytitle
                        # , xlab = xtitle
                        # , ylab = ytitle
                        , paired = paired
                        , ggtheme = selected_theme
                        , results.subtitle = self$options$resultssubtitle

                        )

                    # originaltheme <- self$options$originaltheme
                    #
                    # if (!originaltheme) {
                    #     plot2 <- plot2 + ggtheme
                    # } else {
                    #     plot2 <- plot2 + ggstatsplot::theme_ggstatsplot()
                    # }



                }


                # dep > 1 ----

                if (length(self$options$dep) > 1) {

                    originaltheme <- self$options$originaltheme

                    selected_theme <- if (!originaltheme) ggtheme else ggstatsplot::theme_ggstatsplot()



                    dep2 <- as.list(self$options$dep)
                    dep2_symbols <- purrr::map(dep2, rlang::sym)

                    plotlist <-
                        purrr::pmap(
                            .l = list(
                                x = dep2_symbols,
                                messages = FALSE
                                ),
                            .f = function(x, messages) {
                                ggstatsplot::grouped_ggbarstats(
                            data = mydata,
                            x = !!x,
                            messages = messages,

                            y = !!rlang::sym(group),
                            grouping.var = !!rlang::sym(grvar),


                            # , title = mytitle
                            # , xlab = xtitle
                            # , ylab = ytitle
                            , paired = paired
                            , ggtheme = selected_theme
                            , results.subtitle = self$options$resultssubtitle


                            )
                            }
                        )


                    # originaltheme <- self$options$originaltheme
                    #
                    #
                    # # Assuming plotlist is a list of plots
                    # for (i in seq_along(plotlist)) {
                    #     if (!originaltheme) {
                    #         plotlist[[i]] <- plotlist[[i]] + ggtheme
                    #     } else {
                    #         plotlist[[i]] <- plotlist[[i]] +
                    #             ggstatsplot::theme_ggstatsplot()
                    #     }
                    # }


                    plot2 <- ggstatsplot::combine_plots(
                        plotlist = plotlist,
                            plotgrid.args = list(ncol = 1)
                            )

                    }

                # Print Plot ----

                print(plot2)
                TRUE
            }

        )
    )
