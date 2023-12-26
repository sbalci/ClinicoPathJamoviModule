#' @title Histogram
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import ggplot2


jjhistostatsClass <- if (requireNamespace('jmvcore'))
    R6::R6Class(
        "jjhistostatsClass",
        inherit = jjhistostatsBase,
        private = list(

            # init ----
            .init = function() {
                deplen <- length(self$options$dep)

                self$results$plot$setSize(400, deplen * 300)

                self$results$plot2$setSize(800, deplen * 300)
            }

            # run ----
            ,
            .run = function() {
                ## Initial Message ----
                if (is.null(self$options$dep)) {

                    ## todo ----

                    todo <- glue::glue(
                    "<br>
                    Welcome to ClinicoPath
                <br><br>
                This tool will help you generate Histograms.
                <br><br>
                This function uses ggplot2 and ggstatsplot packages. See documentations <a href = 'https://indrajeetpatil.github.io/ggstatsplot/reference/gghistostats.html' target='_blank'>gghistostats</a> and <a href = 'https://indrajeetpatil.github.io/ggstatsplot/reference/grouped_gghistostats.html' target='_blank'>grouped_gghistostats</a>.
                <br>
                Please cite jamovi and the packages as given below.
                <br><hr>"
                    )

                    self$results$todo$setContent(todo)

                    return()

                } else {

                    todo <- glue::glue("<br>You have selected to make a histogram.<br><hr>")

                    self$results$todo$setContent(todo)

                    if (nrow(self$data) == 0)
                        stop('Data contains no (complete) rows')
                }
            }

            ,
            .plot = function(image, ggtheme, theme, ...) {
                # the plot function ----

                ## Error messages ----

                if (is.null(self$options$dep))
                    return()

                if (nrow(self$data) == 0)
                    stop('Data contains no (complete) rows')


                ## read data ----

                mydata <- self$data

                vars <- self$options$dep


                for (var in vars)
                    mydata[[var]] <- jmvcore::toNumeric(mydata[[var]])


                ## Exclude NA ----

                excl <- self$options$excl

                if (excl) {
                    mydata <- jmvcore::naOmit(mydata)
                }

                dep <- self$options$dep

                ## arguments ----

                    binwidth <- NULL

                    if(self$options$changebinwidth) {
                        binwidth <- self$options$binwidth
                    }


                    barmeasure <-
                        jmvcore::constructFormula(
                            terms = self$options$barmeasure)

                    centralityparameter <-
                        jmvcore::constructFormula(
                            terms = self$options$centralityparameter)



                ## direction, paired ----

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
                #     jmvcore::constructFormula(
                #     terms = self$options$distribution
                #     )

                # pairw <- self$options$pairw




                # gghistostats
                # https://indrajeetpatil.github.io/ggstatsplot/reference/gghistostats.html

                ## dep == 1 ----

                if (length(self$options$dep) == 1) {
                    plot <-
                        ggstatsplot::gghistostats(
                            data = mydata,
                            x = !!rlang::sym(dep)

                            # ,
                            #                 binwidth = binwidth,
                            #                 # bar.measure = barmeasure,
                            #                 # xlab = NULL,
                            #                 # title = NULL,
                            #                 # subtitle = NULL,
                            #                 # caption = NULL,
                            #
                            #                 type = typestatistics,
                            #
                            #
                            #                 # test.value = 0,
                            #                 # bf.prior = 0.707,
                            #                 # bf.message = TRUE,
                            #                 # effsize.type = "g",
                            #                 # conf.level = 0.95,
                            #                 # tr = 0.2,
                            #                 # nboot = 100,
                            #                 # k = 2L,
                            #
                            #                 ggtheme = ggplot2::theme_bw(),
                            #                 ggstatsplot.layer = TRUE,
                            #
                            #
                            #
                            #                 bar.fill = "grey50",
                            #                 results.subtitle = self$options$resultssubtitle,
                            #                 test.k = 0,
                            #                 test.value.line = FALSE,
                            #                 test.value.line.args = list(size = 1),
                            #                 test.value.label.args = list(size = 3),
                            #
                            #                 centrality.plotting = self$options$centralityline,
                            #
                            #                 centrality.parameter = centralityparameter,
                            #                 centrality.k = 2,
                            #                 centrality.line.args = list(size = 1, color = "blue"),
                            #                 centrality.label.args = list(color = "blue", size = 3),
                            #                 normal.curve = self$options$normalcurve,
                            #                 normal.curve.args = list(size = 3),
                            #                 ggplot.component = NULL,
                            #                 output = "plot"








                        )

extracted_stats <- ggstatsplot::extract_stats(plot)
extracted_subtitle <- ggstatsplot::extract_subtitle(plot)
extracted_caption <- ggstatsplot::extract_caption(plot)

self$results$e_stats$setContent(extracted_stats)
self$results$e_subtitle$setContent(extracted_subtitle)
self$results$e_caption$setContent(extracted_caption)



                }


                ## dep > 1 ----

                if (length(self$options$dep) > 1) {

                    dep2 <- as.list(self$options$dep)
                    dep2_symbols <- purrr::map(dep2, rlang::sym)

                    plotlist <-
                        purrr::pmap(
                            .l = list(
                                x = dep2_symbols,
                                messages = FALSE),
                            .f = function(x, messages) {
                                    ggstatsplot::gghistostats(
                                        data = mydata,
                                        x = !!x,
                                        messages = messages



                                        # ,
                                        #                 binwidth = binwidth,
                                        #                 bar.measure = barmeasure,
                                        #                 # xlab = NULL,
                                        #                 # title = NULL,
                                        #                 # subtitle = NULL,
                                        #                 # caption = NULL,
                                        #
                                        #                 type = typestatistics,
                                        #
                                        #                 # test.value = 0,
                                        #                 # bf.prior = 0.707,
                                        #                 # bf.message = TRUE,
                                        #                 # effsize.type = "g",
                                        #                 # conf.level = 0.95,
                                        #                 # nboot = 100,
                                        #                 # k = 2L,
                                        #
                                        #                 ggtheme = ggplot2::theme_bw(),
                                        #                 ggstatsplot.layer = TRUE,
                                        #
                                        #
                                        #                 bar.fill = "grey50",
                                        #                 results.subtitle = self$options$resultssubtitle,
                                        #
                                        #                 # test.k = 0,
                                        #                 # test.value.line = FALSE,
                                        #                 # test.value.line.args = list(size = 1),
                                        #                 # test.value.label.args = list(size = 3),
                                        #
                                        #                 centrality.plotting = self$options$centralityline,
                                        #
                                        #                 centrality.parameter = centralityparameter,
                                        #                 centrality.k = 2,
                                        #                 centrality.line.args = list(size = 1, color = "blue"),
                                        #                 centrality.label.args = list(color = "blue", size = 3),
                                        #                 normal.curve = self$options$normalcurve,
                                        #                 normal.curve.args = list(size = 3),
                                        #                 ggplot.component = NULL,
                                        #                 output = "plot"











                                    )
                            }
                        )

                    plot <-
                        ggstatsplot::combine_plots(
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
                # the plot2 function ----

                ## Error messages ----

                if (is.null(self$options$dep) ||
                    is.null(self$options$grvar))
                    return()

                if (nrow(self$data) == 0)
                    stop('Data contains no (complete) rows')

                ## read data ----

                mydata <- self$data

                vars <- self$options$dep

                for (var in vars)
                    mydata[[var]] <- jmvcore::toNumeric(mydata[[var]])



                ## direction, paired ----

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

                ## Exclude NA ----

                excl <- self$options$excl

                if (excl) {
                    mydata <- jmvcore::naOmit(mydata)
                }

                ## type of statistics ----

                typestatistics <-
                    jmvcore::constructFormula(
                        terms = self$options$typestatistics)

                dep <- self$options$dep

                ## arguments ----

                binwidth <- NULL

                if(self$options$changebinwidth) {
                    binwidth <- self$options$binwidth
                }

                barmeasure <-
                    jmvcore::constructFormula(
                        terms = self$options$barmeasure)

                centralityparameter <-
                    jmvcore::constructFormula(
                        terms = self$options$centralityparameter)


                # grouped_gghistostats
                # https://indrajeetpatil.github.io/ggstatsplot/reference/grouped_gghistostats.html


                grvar <- self$options$grvar

                ## dep = 1 ----

                if (length(self$options$dep) == 1) {
                    plot2 <- ggstatsplot::grouped_gghistostats(
                        data = mydata,
                        x = !!rlang::sym(dep),
                        grouping.var = !!rlang::sym(grvar)
                        # ,
                        # binwidth = binwidth,
                        # title.prefix = NULL,
                        # output = "plot",
                        # plotgrid.args = list(),
                        # title.text = NULL,
                        # title.args = list(size = 16,
                        #                   fontface = "bold"),
                        # caption.text = NULL,
                        # caption.args = list(size = 10),
                        # sub.text = NULL,
                        # sub.args = list(size = 12)
                        #
                        # , type = typestatistics
                        # , bar.measure = barmeasure
                        # , centrality.parameter = centralityparameter
                        # , results.subtitle = self$options$resultssubtitle
                        # , normal.curve = self$options$normalcurve
                        # , centrality.plotting = self$options$centralityline


                    )

                }

                ## dep > 1 ----

                if (length(self$options$dep) > 1) {
                    dep2 <- as.list(self$options$dep)
                    dep2_symbols <- purrr::map(dep2, rlang::sym)

                    plotlist <-
                        purrr::pmap(
                            .l = list(
                                x = dep2_symbols,
                                messages = FALSE),
                            .f = function(x, messages) {
                                ggstatsplot::grouped_gghistostats(
                                    data = mydata,
                                    x = !!x,
                                    messages = messages,
                                    grouping.var = !!rlang::sym(grvar)

                            # ,
                            # binwidth = binwidth,
                            # title.prefix = NULL,
                            # output = "plot",
                            # plotgrid.args = list(),
                            # title.text = NULL,
                            # title.args = list(size = 16,
                            #                   fontface = "bold"),
                            # caption.text = NULL,
                            # caption.args = list(size = 10),
                            # sub.text = NULL,
                            # sub.args = list(size = 12)
                            #
                            # , type = typestatistics
                            # , bar.measure = barmeasure
                            # , centrality.parameter = centralityparameter
                            # , results.subtitle = self$options$resultssubtitle
                            # , normal.curve = self$options$normalcurve
                            # , centrality.plotting = self$options$centralityline
)
                            }
)


                    plot2 <-
                        ggstatsplot::combine_plots(
            plotlist = plotlist,
                        plotgrid.args = list(ncol = 1)
                         )

                }

                ## Print Plot 2 ----

                print(plot2)
                TRUE

            }





        )
    )
