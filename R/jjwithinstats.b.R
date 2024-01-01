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

            self$results$plot$setSize(600, 450)

            if (!is.null(self$options$dep3) || !is.null(self$options$dep4))
                self$results$plot$setSize(800, 600)

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
                The data should be in wide format: Each row should have a unique case. Columns should have separate measurements.
                <br><br>
                This function uses ggplot2 and ggstatsplot packages. See documentations <a href = 'https://indrajeetpatil.github.io/ggstatsplot/reference/ggwithinstats.html' target='_blank'>ggwithinstats</a> and <a href = 'https://indrajeetpatil.github.io/ggstatsplot/reference/grouped_ggwithinstats.html' target='_blank'>grouped_ggwithinstats</a>.
This function does not allow missing values and works on long data format. Please see above links for further information.
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



            # read arguments ----

            pointpath <- self$options$pointpath

            meanpath <- self$options$meanpath




            ## type of statistics ----


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


            mytitle <- self$options$mytitle

            effsizetype <- self$options$effsizetype

            pointpath <- self$options$pointpath

            meanpath <- self$options$meanpath

            meanplotting <- self$options$meanplotting



            # ggwithinstats ----
            # https://indrajeetpatil.github.io/ggstatsplot/reference/ggwithinstats.html




            plot <-
                ggstatsplot::ggwithinstats(
                    data = long_data,
                    x = measurement,
                    y = value,
                    paired = TRUE,
                    id = rowid


                    , title = mytitle
                    , type = typestatistics
                    , ggtheme = ggtheme
                    , ggstatsplot.layer = originaltheme
                    , plot.type = plottype
                    , pairwise.comparisons = pairwisecomparisons
                    , pairwise.display = pairwisedisplay
                    , p.adjust.method = padjustmethod
                    , effsize.type = effsizetype

                    # partial = TRUE,
                    # bf.prior = 0.707,
                    # bf.message = TRUE,
                    # sphericity.correction = TRUE,
                    # results.subtitle = TRUE,
                    # xlab = NULL,
                    # ylab = NULL,


                    # subtitle = NULL,
                    # sample.size.label = TRUE,
                    # k = 2L,
                    # conf.level = 0.95,
                    # nboot = 100L,
                    # tr = 0.1,
                    , mean.plotting = meanplotting
                    # mean.ci = FALSE,
                    # mean.point.args = list(size = 5, color = "darkred"),
                    # mean.label.args = list(size = 3),
                    , point.path = pointpath
                    # point.path.args = list(alpha = 0.5, linetype = "dashed"),
                    , mean.path = meanpath
                    # mean.path.args = list(color = "red", size = 1, alpha = 0.5),
                    # notch = FALSE,
                    # notchwidth = 0.5,
                    # outlier.tagging = FALSE,
                    # outlier.label = NULL,
                    # outlier.coef = 1.5,
                    # outlier.label.args = list(),
                    # outlier.point.args = list(),
                    # violin.args = list(width = 0.5, alpha = 0.2),
                    # ggsignif.args = list(textsize = 3, tip_length = 0.01),
                    # ggtheme = ggtheme,
                    # # ggtheme = ggplot2::theme_bw(),

                    # package = "RColorBrewer",
                    # palette = "Dark2",
                    # ggplot.component = NULL,
                    # output = "plot",
                    # messages = TRUE
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






            # Print Plot ----

            print(plot)
            TRUE

        }

    )
)
