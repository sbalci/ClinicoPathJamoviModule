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

                self$results$plot$setSize(400, deplen * 300)

                self$results$plot2$setSize(800, deplen * 300)

            }
            ,

            .run = function() {
                # Initial Message ----
                if (is.null(self$options$dep) ||
                    is.null(self$options$group)) {
                    # TODO ----

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
                    # TODO ----
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

                if (excl) {
                    mydata <- jmvcore::naOmit(mydata)
                }



                # mydep <- mydata[[self$options$dep]]
                # mygroup <- mydata[[self$options$group]]


                dep <- self$options$dep

                group <- self$options$group

                originaltheme <- self$options$originaltheme


                # dep1 <- jmvcore::composeTerms(listOfComponents = dep)

                dep1 <- jmvcore::composeTerm(components = dep)

                group <- jmvcore::composeTerm(components = group)


                # ggbarstats ----
                # bar charts for categorical data
                # https://indrajeetpatil.github.io/ggstatsplot/reference/ggbarstats.html


                # dep == 1 ----

                if (length(self$options$dep) == 1) {
                    plot <-
                        ggstatsplot::ggbarstats(
                            data = mydata,
                            x = !!dep1,
                            y = !!group,

                            # paired = paired,

                            paired = FALSE,

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
                            ggtheme = ggtheme,

                            # ggtheme = ggplot2::theme_bw(),
                            ggstatsplot.layer = originaltheme,
                            package = "RColorBrewer",
                            palette = "Dark2",
                            ggplot.component = NULL,
                            output = "plot",
                            messages = TRUE
                            # x = NULL,
                            # y = NULL
                        )

                }


                # dep > 1 ----

                if (length(self$options$dep) > 1) {
                    dep2 <- as.list(self$options$dep)

                    plotlist <-
                        purrr::pmap(
                            .l = list(x = dep2,
                                      # title = list(dep),
                                      messages = TRUE),
                            .f = ggstatsplot::ggbarstats,
                            data = mydata,
                            y = !!group,

                            # paired = paired,
                            paired = FALSE,

                            # ,
                            # counts = NULL,
                            # ratio = NULL,
                            # results.subtitle = TRUE,
                            # sample.size.label = TRUE,
                            # label = "percentage",
                            # perc.k = 0,
                            # label.args = list(alpha = 1, fill = "white"),
                            # bf.message = TRUE,
                            # sampling.plan = "indepMulti",
                            # fixed.margin = "rows",
                            # prior.concentration = 1,
                            # title = NULL,
                            # subtitle = NULL,
                            # caption = NULL,
                            # conf.level = 0.95,
                            # nboot = 100,
                            # legend.title = NULL,
                            # xlab = NULL,
                            # ylab = NULL,
                            # k = 2,
                            # proportion.test = TRUE,
                            ggtheme = ggtheme,

                            # ggtheme = ggplot2::theme_bw(),
                            ggstatsplot.layer = originaltheme
                            # ggstatsplot.layer = originaltheme,
                            # package = "RColorBrewer",
                            # palette = "Dark2",
                            # ggplot.component = NULL,
                            # output = "plot",
                            # messages = TRUE,
                            # x = NULL,
                            # y = NULL
                        )

                    plot <- ggstatsplot::combine_plots(plotlist = plotlist,
                                                       nrow = length(self$options$dep))



                }

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

                if (excl) {
                    mydata <- jmvcore::naOmit(mydata)
                }



                dep <- self$options$dep

                group <- self$options$group

                # originaltheme <- self$options$originaltheme

                dep1 <- jmvcore::composeTerm(components = dep)

                group <- jmvcore::composeTerm(components = group)



                # grouped_ggbarstats ----
                # https://indrajeetpatil.github.io/ggstatsplot/reference/grouped_ggbarstats.html



                grvar <- self$options$grvar

                # dep = 1 ----

                if (length(self$options$dep) == 1) {
                    plot2 <- ggstatsplot::grouped_ggbarstats(
                        data = mydata,
                        x = !!dep1,
                        y = !!group,
                        grouping.var = !!grvar,

                        # paired = paired,
                        paired = FALSE,
                        ggtheme = ggtheme,


                        counts = NULL,
                        title.prefix = NULL,
                        output = "plot",
                        # x = NULL,
                        # y = NULL,
                        plotgrid.args = list(),
                        title.text = NULL,
                        title.args = list(size = 16, fontface = "bold"),
                        caption.text = NULL,
                        caption.args = list(size = 10),
                        sub.text = NULL,
                        sub.args = list(size = 12)
                        # ,
                        # ggtheme = ggtheme,
                        # ggstatsplot.layer = originaltheme
                    )

                }


                # dep > 1 ----

                if (length(self$options$dep) > 1) {
                    dep2 <- as.list(self$options$dep)

                    plotlist <-
                        purrr::pmap(
                            .l = list(x = dep2,
                                      # title = list(dep),
                                      messages = TRUE),
                            .f = ggstatsplot::grouped_ggbarstats,
                            data = mydata,

                            y = !!group,
                            grouping.var = !!grvar,

                            # paired = paired,

                            paired = FALSE,


                            counts = NULL,
                            title.prefix = NULL,
                            output = "plot",
                            # x = NULL,
                            # y = NULL,
                            plotgrid.args = list(),
                            title.text = NULL,
                            title.args = list(size = 16, fontface = "bold"),
                            caption.text = NULL,
                            caption.args = list(size = 10),
                            sub.text = NULL,
                            sub.args = list(size = 12)
                            # ,
                            # ggtheme = ggtheme,
                            # ggstatsplot.layer = originaltheme
                        )

                    plot2 <- ggstatsplot::combine_plots(plotlist = plotlist,
                                                        ncol = 1)

                    }

                # Print Plot ----

                print(plot2)
                TRUE

            }





        )
    )
