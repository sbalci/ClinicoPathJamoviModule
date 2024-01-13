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
            if ( is.null(self$options$dep) || is.null(self$options$group)) {

                # todo ----

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

                # todo ----
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

            mydata <- self$data


            vars <- self$options$dep


            for (var in vars)
                mydata[[var]] <- jmvcore::toNumeric(mydata[[var]])


            # Exclude NA ----

            mydata <- jmvcore::naOmit(mydata)


            # type of statistics ----


            typestatistics <-
                jmvcore::constructFormula(terms = self$options$typestatistics)



            # define main arguments ----

            dep <- self$options$dep

            group <- self$options$group


            # read arguments ----

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

            effsizetype <- self$options$effsizetype

            centralityplotting <- self$options$centralityplotting

            centralitytype <- self$options$centralitytype


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


            # define main arguments ----


            dep <- self$options$dep

            group <- self$options$group


            effsizetype <- self$options$effsizetype

            centralityplotting <- self$options$centralityplotting

            centralitytype <- self$options$centralitytype


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








