#' @title Violin Plots to Compare Within Group
#' @importFrom R6 R6Class
#' @import jmvcore
#'


jjwithinstatsClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "jjwithinstatsClass",
    inherit = jjwithinstatsBase,
    private = list(

        # init ----

        .init = function() {

            self$results$plot$setSize(650, 450)

            if (!is.null(self$options$dep3) || !is.null(self$options$dep4))
                self$results$plot$setSize(900, 600)

            # if (!is.null(self$options$dep3) && !is.null(self$options$dep4))
            #     self$results$plot$setSize(800, 600)

        }

        # run ----
        ,
        .run = function() {

            ## Initial Message ----
            if ( is.null(self$options$dep1) || is.null(self$options$dep2)) {

                ### todo ----

                todo <- glue::glue(
                "<br>Welcome to ClinicoPath
                <br><br>
                This tool will help you generate Violin Plots for repeated measurements.
                <br><br>
                The data should be in wide format: Each row should have a unique case. Columns should have separate measurements. This function does not allow missing values.
                <br><br>
                This function uses ggplot2 and ggstatsplot packages. See documentations <a href = 'https://indrajeetpatil.github.io/ggstatsplot/reference/ggwithinstats.html' target='_blank'>ggwithinstats</a> and <a href = 'https://indrajeetpatil.github.io/ggstatsplot/reference/grouped_ggwithinstats.html' target='_blank'>grouped_ggwithinstats</a>.
                Please see above links for further information.
                <br>
                Please cite jamovi and the packages as given below.
                <br><hr>"
                )

                self$results$todo$setContent(todo)

                return()

            } else {

                ### todo ----
                todo <- glue::glue(
                "<br>You have selected to use a Violin Plot to Compare repeated measurements.<br><hr>")

                self$results$todo$setContent(todo)

                if (nrow(self$data) == 0)
                    stop('Data contains no (complete) rows')

            }
        }


        # the plot function ----
        ,
        .plot = function(image, ggtheme, theme, ...) {

            ## Error messages ----

            if (is.null(self$options$dep1) ||
                is.null(self$options$dep2))
                return()

            if (nrow(self$data) == 0)
                stop('Data contains no (complete) rows')


            ## Prepare Data ----

            mydata <- self$data

            mydata$rowid <- seq.int(nrow(mydata))

            dep1 <- self$options$dep1

            dep2 <- self$options$dep2

            dep3 <- self$options$dep3

            dep4 <- self$options$dep4

            vars <- c(dep1, dep2, dep3, dep4)
            vars <- vars[!sapply(vars, is.null)]

            for (var in vars)
                mydata[[var]] <- jmvcore::toNumeric(mydata[[var]])

            mydata <- jmvcore::naOmit(mydata)

            long_data <- tidyr::pivot_longer(
                mydata,
                cols = vars,
                names_to = "measurement",
                values_to = "value"
            )

            long_data$measurement <- factor(long_data$measurement,
                                            levels = vars)


            # mydataview ----
            # self$results$mydataview$setContent(
            #     list(
            #     dep1 = dep1,
            #     dep2 = dep2,
            #     dep3 = dep3,
            #     dep4 = dep4,
            #     mydata = head(mydata),
            #     long_data = head(long_data)
            # )
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

            pointpath <- self$options$pointpath

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

            pointpath <- self$options$pointpath

            centralityplotting <- self$options$centralityplotting

            centralitytype <- self$options$centralitytype

            centralitypath <- self$options$centralitypath

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


            # ggwithinstats ----
            # https://indrajeetpatil.github.io/ggstatsplot/reference/ggwithinstats.html


            plot <-
                ggstatsplot::ggwithinstats(
                    data = long_data,
                    x = measurement,
                    y = value,
                    paired = TRUE,
                    id = "rowid"


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
                    , point.path = pointpath
                    , centrality.path = centralitypath
                    , violin.args = violinargs
                    , boxplot.args = boxplotargs
                    , point.args = pointargs
                    , results.subtitle = self$options$resultssubtitle

                )


            # mydataview ----
            # extracted_stats <- ggstatsplot::extract_stats(plot)
            # extracted_subtitle <- ggstatsplot::extract_subtitle(plot)
            # extracted_caption <- ggstatsplot::extract_caption(plot)
            # self$results$e_plot$setContent(
            #     as.list(
            #         plot
            #     )
            # )
            # self$results$e_stats$setContent(as.list(extracted_stats))
            # self$results$e_subtitle$setContent(as.list(extracted_subtitle))
            # self$results$e_caption$setContent(as.list(extracted_caption))


            originaltheme <- self$options$originaltheme

            if (!originaltheme) {
                plot <- plot + ggtheme
            } else {
                plot <- plot + ggstatsplot::theme_ggstatsplot()
                # ggplot2::theme_bw()
            }


            # Print Plot ----

            print(plot)
            TRUE

        }

    )
)
