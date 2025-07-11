#' @title Pie Charts
#' @importFrom R6 R6Class
#' @import jmvcore
#'

jjpiestatsClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "jjpiestatsClass",
    inherit = jjpiestatsBase,
    private = list(

        # Cache for processed data and options to avoid redundant computation
        .processedData = NULL,
        .processedOptions = NULL,

        # init ----

        .init = function() {

            self$results$plot1$setSize(650, 450)

            self$results$plot2$setSize(650, 450)

            # if (!is.null(self$options$dep) && !is.null(self$options$grvar)) {
            #
            #     mydata <- self$data
            #
            #     grvar <-  self$options$grvar
            #
            #     num_levels <- nlevels(
            #         as.factor(mydata[[grvar]])
            #     )
            #
            #     self$results$plot3$setSize(num_levels * 600, 450)
            #
            # }

            if (!is.null(self$options$grvar) && !is.null(self$options$group)) {

                mydata <- self$data

                group <-  self$options$group

                num_levels_group <- nlevels(
                    as.factor(mydata[[group]])
                )

                self$results$plot4$setSize(num_levels_group * 600, 450)

            }




            if (!is.null(self$options$group) && !is.null(self$options$grvar)) {

                mydata <- self$data

                grvar <-  self$options$grvar

                num_levels <- nlevels(
                    as.factor(mydata[[grvar]])
                )

                group <-  self$options$group

                num_levels_group <- nlevels(
                    as.factor(mydata[[group]])
                )

                # self$results$plot3$setSize((num_levels + num_levels_group) * 600, 450)

                self$results$plot4$setSize((num_levels + num_levels_group) * 600, 450)

            }

        },

        # Optimized data preparation with caching
        .prepareData = function(force_refresh = FALSE) {
            if (!is.null(private$.processedData) && !force_refresh) {
                return(private$.processedData)
            }

            # Prepare data with progress feedback
            self$results$todo$setContent(
                glue::glue("<br>Processing data for pie chart analysis...<br><hr>")
            )

            mydata <- self$data

            # Exclude NA with checkpoint
            private$.checkpoint()
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
                glue::glue("<br>Preparing pie chart analysis options...<br><hr>")
            )

            # Process options
            dep <- self$options$dep
            group <- self$options$group
            grvar <- self$options$grvar
            typestatistics <- self$options$typestatistics
            
            # Cache the processed options
            options_list <- list(
                dep = dep,
                group = group,
                grvar = grvar,
                typestatistics = typestatistics,
                resultssubtitle = self$options$resultssubtitle,
                originaltheme = self$options$originaltheme
            )
            private$.processedOptions <- options_list
            return(options_list)
        }



        # run ----
        ,
        .run = function() {

            # Initial Message ----
            if ( is.null(self$options$dep) ) {

                # TODO ----

                todo <- glue::glue(
                "<br>Welcome to ClinicoPath
                <br><br>
                This tool will help you generate Pie Charts.
                <br><br>
                This function uses ggplot2 and ggstatsplot packages. See documentations <a href = 'https://indrajeetpatil.github.io/ggstatsplot/reference/ggpiestats.html' target='_blank'>ggpiestats</a> and <a href = 'https://indrajeetpatil.github.io/ggstatsplot/reference/grouped_ggpiestats.html' target='_blank'>grouped_ggpiestats</a>.
                <br>
                Please cite jamovi and the packages as given below.
                <br><hr>"
                )

                self$results$todo$setContent(todo)

                return()

            } else {

                # TODO ----
                todo <- glue::glue(
                    "<br>You have selected to use Pie Charts.<br><hr>")

                self$results$todo$setContent(todo)

                if (nrow(self$data) == 0)
                    stop('Data contains no (complete) rows')

                # Pre-process data and options for performance
                private$.prepareData()
                private$.prepareOptions()

            }
        }


        # the plot1 function ----


        ,
        .plot1 = function(image, ggtheme, theme, ...) {

            # Error messages ----

            if ( is.null(self$options$dep) )
                return()

            if (nrow(self$data) == 0)
                stop('Data contains no (complete) rows')

            # Use cached data and options for performance ----
            mydata <- private$.prepareData()
            options_data <- private$.prepareOptions()
            
            dep <- options_data$dep


            # ggpiestats ----
            # https://indrajeetpatil.github.io/ggstatsplot/reference/ggpiestats.html



            plot1 <-
                ggstatsplot::ggpiestats(
                    data = mydata,
                    x = !!rlang::sym(dep),
                    y = NULL,
                    counts = NULL,
                    ratio = NULL,
                    paired = FALSE,
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
                    k = 2,
                    proportion.test = TRUE,

                    package = "RColorBrewer",
                    palette = "Dark2",
                    ggplot.component = NULL,
                    output = "plot",
                    messages = TRUE
                    , results.subtitle = options_data$resultssubtitle

                    )



            originaltheme <- options_data$originaltheme

            if (!originaltheme) {
                plot1 <- plot1 + ggtheme
            } else {
                plot1 <- plot1 + ggstatsplot::theme_ggstatsplot()
                # ggplot2::theme_bw()
            }

            # Print Plot1 ----

            print(plot1)
            TRUE

        }


        # the plot2 function ----


        , .plot2 = function(image, ggtheme, theme, ...) {

            # Error messages ----

            if ( is.null(self$options$dep) || is.null(self$options$group) )
                return()

            if (nrow(self$data) == 0)
                stop('Data contains no (complete) rows')

            # Use cached data and options for performance ----
            mydata <- private$.prepareData()
            options_data <- private$.prepareOptions()
            
            dep <- options_data$dep
            group <- options_data$group

            # originaltheme <- self$options$originaltheme


            # dep <- jmvcore::composeTerm(components = dep)

            # group <- jmvcore::composeTerm(components = group)


            # ggpiestats ----
            # https://indrajeetpatil.github.io/ggstatsplot/reference/ggpiestats.html

            plot2 <-
                ggstatsplot::ggpiestats(
                    data = mydata,
                    x = !!rlang::sym(dep),
                    y = !!rlang::sym(group),
                    counts = NULL,
                    ratio = NULL,
                    paired = FALSE,
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
                    k = 2,
                    proportion.test = TRUE,

                    package = "RColorBrewer",
                    palette = "Dark2",
                    ggplot.component = NULL,
                    output = "plot",
                    messages = TRUE
                    , results.subtitle = options_data$resultssubtitle
                )


            originaltheme <- options_data$originaltheme

            if (!originaltheme) {
                plot2 <- plot2 + ggtheme
            } else {
                plot2 <- plot2 + ggstatsplot::theme_ggstatsplot()
                # ggplot2::theme_bw()
            }


            # Print Plot2 ----
            print(plot2)
            TRUE
        }



#         # the plot3 function ----
#
#
#
#         , .plot3 = function(image, ggtheme, theme, ...) {
#
#             # Error messages ----
#
#             if ( is.null(self$options$dep) || is.null(self$options$grvar) )
#                 return()
#
#             if (nrow(self$data) == 0)
#                 stop('Data contains no (complete) rows')
#
#
#             # Prepare Data ----
#
#             mydata <- self$data
#
#
#             # # direction, paired ----
#             #
#             # direction <- self$options$direction
#             #
#             # if (direction == "repeated") {
#             #
#             #     paired <- TRUE
#             #
#             # } else if (direction == "independent") {
#             #
#             #     paired <- FALSE
#             #
#             # }
#
#             # Exclude NA ----
#
#             excl <- self$options$excl
#
#             if (excl) {mydata <- jmvcore::naOmit(mydata)}
#
#
#             dep <- self$options$dep
#
#             # group <- self$options$group
#
#
#             # dep <- jmvcore::composeTerm(components = dep)
#
#             # group <- jmvcore::composeTerm(components = group)
#
#
#
#             # originaltheme <- self$options$originaltheme
#
#
#             # grouped_ggpiestats ----
#             # https://indrajeetpatil.github.io/ggstatsplot/reference/grouped_ggpiestats.html
#
#
#             if ( !is.null(self$options$grvar) ) {
#
#             grvar <- self$options$grvar
#
#             # grvar <- jmvcore::composeTerm(components = grvar)
#
#
#                 plot3 <- ggstatsplot::grouped_ggpiestats(
#                     data = mydata,
#                     x = !!rlang::sym(dep),
#                     y = NULL,
#                     counts = NULL,
#                     grouping.var = !!rlang::sym(grvar),
#                     title.prefix = NULL,
#                     output = "plot",
#                     plotgrid.args = list(),
#                     title.text = NULL,
#                     title.args = list(size = 16, fontface = "bold"),
#                     caption.text = NULL,
#                     caption.args = list(size = 10),
#                     sub.text = NULL,
#                     sub.args = list(size = 12)
#                     , ggtheme = ggtheme
#                     , ggstatsplot.layer = originaltheme
#
#                 )
# }
#
#
#             originaltheme <- self$options$originaltheme
#
#             if (!originaltheme) {
#                 plot3 <- plot3 + ggtheme
#             } else {
#                 plot3 <- plot3 + ggstatsplot::theme_ggstatsplot()
#                 # ggplot2::theme_bw()
#             }
#
#
#             # Print Plot3 ----
#             print(plot3)
#             TRUE
#         }
#
#

# the plot4 function ----




        , .plot4 = function(image, ggtheme, theme, ...) {

            # Error messages ----

            if ( is.null(self$options$dep) || is.null(self$options$group) || is.null(self$options$grvar) )
                return()

            if (nrow(self$data) == 0)
                stop('Data contains no (complete) rows')

            # Use cached data and options for performance ----
            mydata <- private$.prepareData()
            options_data <- private$.prepareOptions()
            
            dep <- options_data$dep
            group <- options_data$group
            grvar <- options_data$grvar

            # originaltheme <- self$options$originaltheme


            # dep <- jmvcore::composeTerm(components = dep)

            # group <- jmvcore::composeTerm(components = group)


            # grouped_ggpiestats ----
            # https://indrajeetpatil.github.io/ggstatsplot/reference/grouped_ggpiestats.html

            if ( !is.null(grvar) ) {

                originaltheme <- options_data$originaltheme

                selected_theme <- if (!originaltheme) ggtheme else ggstatsplot::theme_ggstatsplot()


                plot4 <- ggstatsplot::grouped_ggpiestats(
                    data = mydata,
                    x = !!rlang::sym(dep),
                    y = !!rlang::sym(group),
                    counts = NULL,
                    grouping.var = !!rlang::sym(grvar)

                    , ggtheme = selected_theme
                    , ggstatsplot.layer = originaltheme

                )
            }

            #  originaltheme <- self$options$originaltheme
            #
            # if (!originaltheme) {
            #     plot4 <- plot4 + ggtheme
            # } else {
            #     plot4 <- plot4 + ggstatsplot::theme_ggstatsplot()
            #     # ggplot2::theme_bw()
            # }

            # Print Plot4 ----
            print(plot4)
            TRUE
        }

    )
)
