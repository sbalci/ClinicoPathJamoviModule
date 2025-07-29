#' @title Histogram
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import ggplot2


jjhistostatsClass <- if (requireNamespace('jmvcore'))
    R6::R6Class(
        "jjhistostatsClass",
        inherit = jjhistostatsBase,
        private = list(

            # Cache for processed data and options to avoid redundant computation
            .processedData = NULL,
            .processedOptions = NULL,

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



            },

            # Optimized data preparation with caching
            .prepareData = function(force_refresh = FALSE) {
                if (!is.null(private$.processedData) && !force_refresh) {
                    return(private$.processedData)
                }

                # Prepare data with progress feedback
                self$results$todo$setContent(
                    glue::glue("<br>Processing data for histogram analysis...<br><hr>")
                )

                mydata <- self$data
                
                # Convert variables to numeric
                vars <- self$options$dep
                if (!is.null(vars)) {
                    for (var in vars) {
                        mydata[[var]] <- jmvcore::toNumeric(mydata[[var]])
                    }
                }

                # Exclude NA
                mydata <- jmvcore::naOmit(mydata)

                # Cache the processed data
                private$.processedData <- mydata
                return(mydata)
            },

            # Optimized options preparation with caching
            .prepareOptions = function(force_refresh = FALSE) {
                if (!is.null(private$.processedOptions) && !force_refresh) {
                    return(private$.processedOptions)
                }

                # Prepare options with progress feedback
                self$results$todo$setContent(
                    glue::glue("<br>Preparing histogram analysis options...<br><hr>")
                )

                # Process options
                typestatistics <- self$options$typestatistics  # No need for constructFormula
                dep <- self$options$dep
                
                # Process binwidth
                binwidth <- NULL
                if (self$options$changebinwidth) {
                    binwidth <- self$options$binwidth
                }
                
                # Process text parameters
                xlab <- if (self$options$xlab != '') self$options$xlab else NULL
                title <- if (self$options$title != '') self$options$title else NULL
                subtitle <- if (self$options$subtitle != '') self$options$subtitle else NULL
                caption <- if (self$options$caption != '') self$options$caption else NULL
                
                # Process bin.args
                bin.args <- list(
                    fill = self$options$binfill,
                    color = self$options$bincolor,
                    alpha = self$options$binalpha
                )
                
                # Process centrality.line.args
                centrality.line.args <- list(
                    color = self$options$centralitylinecolor,
                    linewidth = self$options$centralitylinewidth,
                    linetype = self$options$centralitylinetype
                )
                
                # Process centrality.type
                centrality.type <- if (self$options$centralitytype != 'default') self$options$centralitytype else NULL
                
                # Cache the processed options
                options_list <- list(
                    typestatistics = typestatistics,
                    dep = dep,
                    binwidth = binwidth,
                    resultssubtitle = self$options$resultssubtitle,
                    centralityline = self$options$centralityline,
                    test.value = self$options$test.value,
                    conf.level = self$options$conf.level,
                    bf.message = self$options$bf.message,
                    digits = self$options$digits,
                    xlab = xlab,
                    title = title,
                    subtitle = subtitle,
                    caption = caption,
                    bin.args = bin.args,
                    centrality.line.args = centrality.line.args,
                    centrality.type = centrality.type
                )
                private$.processedOptions <- options_list
                return(options_list)
            },

            # run ----
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

                    # Pre-process data and options for performance
                    private$.prepareData()
                    private$.prepareOptions()

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

                # Use cached data and options for performance ----
                mydata <- private$.prepareData()
                options_data <- private$.prepareOptions()
                
                dep <- options_data$dep
                typestatistics <- options_data$typestatistics
                binwidth <- options_data$binwidth





                # gghistostats
                # https://indrajeetpatil.github.io/ggstatsplot/reference/gghistostats.html

                    # originaltheme <- self$options$originaltheme
                    #
                    # selected_theme <- if (!originaltheme) ggtheme else ggstatsplot::theme_ggstatsplot()

                ## dep == 1 ----

                if (length(self$options$dep) == 1) {
                    if (!is.null(options_data$centrality.type)) {
                        plot <- ggstatsplot::gghistostats(
                            data = mydata,
                            x = !!rlang::sym(dep),
                            type = typestatistics,
                            results.subtitle = options_data$resultssubtitle,
                            centrality.plotting = options_data$centralityline,
                            binwidth = binwidth,
                            test.value = options_data$test.value,
                            conf.level = options_data$conf.level,
                            bf.message = options_data$bf.message,
                            digits = options_data$digits,
                            xlab = options_data$xlab,
                            title = options_data$title,
                            subtitle = options_data$subtitle,
                            caption = options_data$caption,
                            bin.args = options_data$bin.args,
                            centrality.line.args = options_data$centrality.line.args,
                            centrality.type = options_data$centrality.type
                        )
                    } else {
                        plot <- ggstatsplot::gghistostats(
                            data = mydata,
                            x = !!rlang::sym(dep),
                            type = typestatistics,
                            results.subtitle = options_data$resultssubtitle,
                            centrality.plotting = options_data$centralityline,
                            binwidth = binwidth,
                            test.value = options_data$test.value,
                            conf.level = options_data$conf.level,
                            bf.message = options_data$bf.message,
                            digits = options_data$digits,
                            xlab = options_data$xlab,
                            title = options_data$title,
                            subtitle = options_data$subtitle,
                            caption = options_data$caption,
                            bin.args = options_data$bin.args,
                            centrality.line.args = options_data$centrality.line.args
                        )
                    }

# extracted_stats <- ggstatsplot::extract_stats(plot)
# extracted_subtitle <- ggstatsplot::extract_subtitle(plot)
# extracted_caption <- ggstatsplot::extract_caption(plot)
#
# self$results$e_stats$setContent(extracted_stats)
# self$results$e_subtitle$setContent(extracted_subtitle)
# self$results$e_caption$setContent(extracted_caption)


                    # originaltheme <- self$options$originaltheme
                    #
                    # if (!originaltheme) {
                    #     plot <- plot + ggtheme
                    # } else {
                    #     plot <- plot + ggstatsplot::theme_ggstatsplot()
                    # }

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
                                    if (!is.null(options_data$centrality.type)) {
                                        ggstatsplot::gghistostats(
                                            data = mydata,
                                            x = !!x,
                                            messages = messages,
                                            type = typestatistics,
                                            results.subtitle = options_data$resultssubtitle,
                                            centrality.plotting = options_data$centralityline,
                                            binwidth = binwidth,
                                            test.value = options_data$test.value,
                                            conf.level = options_data$conf.level,
                                            bf.message = options_data$bf.message,
                                            digits = options_data$digits,
                                            xlab = options_data$xlab,
                                            title = options_data$title,
                                            subtitle = options_data$subtitle,
                                            caption = options_data$caption,
                                            bin.args = options_data$bin.args,
                                            centrality.line.args = options_data$centrality.line.args,
                                            centrality.type = options_data$centrality.type
                                        )
                                    } else {
                                        ggstatsplot::gghistostats(
                                            data = mydata,
                                            x = !!x,
                                            messages = messages,
                                            type = typestatistics,
                                            results.subtitle = options_data$resultssubtitle,
                                            centrality.plotting = options_data$centralityline,
                                            binwidth = binwidth,
                                            test.value = options_data$test.value,
                                            conf.level = options_data$conf.level,
                                            bf.message = options_data$bf.message,
                                            digits = options_data$digits,
                                            xlab = options_data$xlab,
                                            title = options_data$title,
                                            subtitle = options_data$subtitle,
                                            caption = options_data$caption,
                                            bin.args = options_data$bin.args,
                                            centrality.line.args = options_data$centrality.line.args
                                        )
                                    }
                            }
                        )

                    plot <-
                        ggstatsplot::combine_plots(
                            plotlist = plotlist,
                            plotgrid.args = list(ncol = 1)
                            )
                }

            # originaltheme <- self$options$originaltheme
            #
            # if (!originaltheme) {
            #     plot <- plot + ggtheme
            # } else {
            #     plot <- plot + ggstatsplot::theme_ggstatsplot()
            #     # ggplot2::theme_bw()
            # }

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

                # Use cached data and options for performance ----
                mydata <- private$.prepareData()
                options_data <- private$.prepareOptions()
                
                dep <- options_data$dep
                typestatistics <- options_data$typestatistics
                binwidth <- options_data$binwidth



                # grouped_gghistostats
                # https://indrajeetpatil.github.io/ggstatsplot/reference/grouped_gghistostats.html


                grvar <- self$options$grvar

                ## dep = 1 ----

                if (length(self$options$dep) == 1) {
                    if (!is.null(options_data$centrality.type)) {
                        plot2 <- ggstatsplot::grouped_gghistostats(
                            data = mydata,
                            x = !!rlang::sym(dep),
                            grouping.var = !!rlang::sym(grvar),
                            type = typestatistics,
                            results.subtitle = options_data$resultssubtitle,
                            centrality.plotting = options_data$centralityline,
                            binwidth = binwidth,
                            test.value = options_data$test.value,
                            conf.level = options_data$conf.level,
                            bf.message = options_data$bf.message,
                            digits = options_data$digits,
                            xlab = options_data$xlab,
                            caption = options_data$caption,
                            bin.args = options_data$bin.args,
                            centrality.line.args = options_data$centrality.line.args,
                            centrality.type = options_data$centrality.type
                        )
                    } else {
                        plot2 <- ggstatsplot::grouped_gghistostats(
                            data = mydata,
                            x = !!rlang::sym(dep),
                            grouping.var = !!rlang::sym(grvar),
                            type = typestatistics,
                            results.subtitle = options_data$resultssubtitle,
                            centrality.plotting = options_data$centralityline,
                            binwidth = binwidth,
                            test.value = options_data$test.value,
                            conf.level = options_data$conf.level,
                            bf.message = options_data$bf.message,
                            digits = options_data$digits,
                            xlab = options_data$xlab,
                            caption = options_data$caption,
                            bin.args = options_data$bin.args,
                            centrality.line.args = options_data$centrality.line.args
                        )
                    }

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
                                if (!is.null(options_data$centrality.type)) {
                                    ggstatsplot::grouped_gghistostats(
                                        data = mydata,
                                        x = !!x,
                                        messages = messages,
                                        grouping.var = !!rlang::sym(grvar),
                                        type = typestatistics,
                                        results.subtitle = options_data$resultssubtitle,
                                        centrality.plotting = options_data$centralityline,
                                        binwidth = binwidth,
                                        test.value = options_data$test.value,
                                        conf.level = options_data$conf.level,
                                        bf.message = options_data$bf.message,
                                        digits = options_data$digits,
                                        xlab = options_data$xlab,
                                        caption = options_data$caption,
                                        bin.args = options_data$bin.args,
                                        centrality.line.args = options_data$centrality.line.args,
                                        centrality.type = options_data$centrality.type
                                    )
                                } else {
                                    ggstatsplot::grouped_gghistostats(
                                        data = mydata,
                                        x = !!x,
                                        messages = messages,
                                        grouping.var = !!rlang::sym(grvar),
                                        type = typestatistics,
                                        results.subtitle = options_data$resultssubtitle,
                                        centrality.plotting = options_data$centralityline,
                                        binwidth = binwidth,
                                        test.value = options_data$test.value,
                                        conf.level = options_data$conf.level,
                                        bf.message = options_data$bf.message,
                                        digits = options_data$digits,
                                        xlab = options_data$xlab,
                                        caption = options_data$caption,
                                        bin.args = options_data$bin.args,
                                        centrality.line.args = options_data$centrality.line.args
                                    )
                                }
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
