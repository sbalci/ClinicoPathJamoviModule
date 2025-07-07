#' @title Dot Chart
#' @importFrom R6 R6Class
#' @import jmvcore
#'


jjdotplotstatsClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "jjdotplotstatsClass",
    inherit = jjdotplotstatsBase,
    private = list(

        # Cache for processed data and options to avoid redundant computation
        .processedData = NULL,
        .processedOptions = NULL,

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


,
        # Optimized data preparation with caching
        .prepareData = function(force_refresh = FALSE) {
            if (!is.null(private$.processedData) && !force_refresh) {
                return(private$.processedData)
            }

            # Prepare data with progress feedback
            self$results$todo$setContent(
                glue::glue("<br>Processing data for dot plot analysis...<br><hr>")
            )

            mydata <- self$data
            
            # Convert variables to numeric
            vars <- self$options$dep
            if (!is.null(vars)) {
                for (var in vars) {
                    mydata[[var]] <- jmvcore::toNumeric(mydata[[var]])
                }
            }

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
                glue::glue("<br>Preparing dot plot analysis options...<br><hr>")
            )

            # Process type of statistics
            typestatistics <- jmvcore::constructFormula(terms = self$options$typestatistics)

            # Process variables
            dep <- self$options$dep
            group <- self$options$group
            
            # Process titles
            mytitle <- self$options$mytitle
            if (mytitle == '') mytitle <- NULL
            
            xtitle <- self$options$xtitle
            if (xtitle == '') xtitle <- NULL
            
            ytitle <- self$options$ytitle
            if (ytitle == '') ytitle <- NULL
            
            # Cache the processed options
            options_list <- list(
                typestatistics = typestatistics,
                dep = dep,
                group = group,
                mytitle = mytitle,
                xtitle = xtitle,
                ytitle = ytitle,
                effsizetype = self$options$effsizetype,
                centralityplotting = self$options$centralityplotting,
                centralitytype = self$options$centralitytype
            )
            private$.processedOptions <- options_list
            return(options_list)
        },

        # run ----
        .run = function() {

            # Initial Message ----
            if ( is.null(self$options$dep) || is.null(self$options$group)) {

                # todo ----

                todo <- glue::glue(
                "<br>Welcome to ClinicoPath
                <br><br>
                This tool will help you generate Dot Charts.
                <br><br>
                This function uses ggplot2 and ggstatsplot packages. See documentations for <a href = 'https://indrajeetpatil.github.io/ggstatsplot/reference/ggdotplotstats.html' target='_blank'>ggdotplotstats</a> and <a href = 'https://indrajeetpatil.github.io/ggstatsplot/reference/grouped_ggdotplotstats.html' target='_blank'>grouped_ggdotplotstats</a>.
                <br>
                Please cite jamovi and the packages as given below.
                <br><hr>"
                )

                self$results$todo$setContent(todo)

                return()

            } else {

                # todo ----
                todo <- glue::glue(
                    "<br>You have selected to use a Dot Plot to compare continuous variables by groups.<br><hr>")

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
            # Error messages ----

            if ( is.null(self$options$dep) || is.null(self$options$group))
                return()

            if (nrow(self$data) == 0)
                stop('Data contains no (complete) rows')

            # Use cached data and options for performance ----
            mydata <- private$.prepareData()
            options_data <- private$.prepareOptions()
            
            typestatistics <- options_data$typestatistics
            dep <- options_data$dep
            group <- options_data$group
            mytitle <- options_data$mytitle
            xtitle <- options_data$xtitle
            ytitle <- options_data$ytitle
            effsizetype <- options_data$effsizetype
            centralityplotting <- options_data$centralityplotting
            centralitytype <- options_data$centralitytype


            # ggdotplotstats ----
            # https://indrajeetpatil.github.io/ggstatsplot/reference/ggdotplotstats.html



            plot <-
                ggstatsplot::ggdotplotstats(
                    data = mydata,
                    x = !!rlang::sym(dep),
                    y = !!rlang::sym(group)
                    , title = mytitle
                    , xlab = xtitle
                    , ylab = ytitle
                    , type = typestatistics
                    , effsize.type = effsizetype
                    , centrality.plotting = centralityplotting
                    , centrality.type = centralitytype
                    , results.subtitle = self$options$resultssubtitle


                )


            originaltheme <- self$options$originaltheme

            if (!originaltheme) {
                plot <- plot + ggtheme
            } else {
                plot <- plot + ggstatsplot::theme_ggstatsplot()
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

            # Use cached data and options for performance ----
            mydata <- private$.prepareData()
            options_data <- private$.prepareOptions()
            
            typestatistics <- options_data$typestatistics
            dep <- options_data$dep
            group <- options_data$group
            effsizetype <- options_data$effsizetype
            centralityplotting <- options_data$centralityplotting
            centralitytype <- options_data$centralitytype


            # grouped_ggdotplotstats ----
            # https://indrajeetpatil.github.io/ggstatsplot/reference/grouped_ggdotplotstats.html



            if ( !is.null(self$options$grvar) ) {

                originaltheme <- self$options$originaltheme

                selected_theme <- if (!originaltheme) ggtheme else ggstatsplot::theme_ggstatsplot()


                grvar <- self$options$grvar

                plot2 <- ggstatsplot::grouped_ggdotplotstats(
                    data = mydata,
                    x = !!rlang::sym(dep),
                    y = !!rlang::sym(group),
                    grouping.var = !!rlang::sym(grvar)

                    , type = typestatistics
                    , effsize.type = effsizetype
                    , centrality.plotting = centralityplotting
                    , centrality.type = centralitytype
                    , ggtheme = selected_theme
                    , results.subtitle = self$options$resultssubtitle



                )
            }


            # Print Plot ----

            print(plot2)
            TRUE

        }





    )
)








