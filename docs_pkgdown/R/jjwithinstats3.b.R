#' @title Violin Plots to Compare Within Group  for Wide Data Format 3
#'
#'
#' @importFrom R6 R6Class
#' @import jmvcore
#'


jjwithinstats3Class <- if (requireNamespace('jmvcore'))
    R6::R6Class(
        "jjwithinstats3Class",
        inherit = jjwithinstats3Base,
        private = list(


            .run = function() {
                # Initial Message ----
                if (is.null(self$options$dep)) {
                    # TODO ----

                    todo <- glue::glue(
                        "<br>Welcome to ClinicoPath
                <br><br>
                This tool will help you generate Violin Plots for repeated measurements.
                <br><br>
                This function uses ggplot2 and ggstatsplot packages. See documentations <a href = 'https://indrajeetpatil.github.io/ggstatsplot/reference/ggwithinstats.html' target='_blank'>ggwithinstats</a> and <a href = 'https://indrajeetpatil.github.io/ggstatsplot/reference/grouped_ggwithinstats.html' target='_blank'>grouped_ggwithinstats</a>.
                <br>
                Please cite jamovi and the packages as given below.
                <br><hr>"
                    )

                    self$results$todo$setContent(todo)

                    return()

                } else {
                    # TODO ----
                    todo <- glue::glue(
                        "<br>You have selected to use a Violin Plots to Compare repeated measurements.<br><hr>"
                    )

                    self$results$todo$setContent(todo)

                    if (nrow(self$data) == 0)
                        stop('Data contains no (complete) rows')









                    # Error messages ----

                    if (is.null(self$options$dep))
                        return()

                    if (nrow(self$data) == 0)
                        stop('Data contains no (complete) rows')


                    # Prepare Data ----


                    mydata <- self$data


                    vars <- self$options$dep


                    for (var in vars)
                        mydata[[var]] <- jmvcore::toNumeric(mydata[[var]])


                        # mydata <-
                        #     data %>%
                        #     tibble::rowid_to_column(.data = .) %>%
                        #     tidyr::pivot_longer(cols = -rowid,
                        #                         names_to = "Measurement",
                        #                         values_to = "Values"
                        #     )

                        # mydata[["Measurement"]] <- ordered(
                        #     x = mydata[["Measurement"]],
                        #     levels = c(name1, name2)
                        # )


                    }


                    # Exclude NA ----

                    # excl <- self$options$excl

                    # if (excl) {
                    #     mydata <- jmvcore::naOmit(mydata)
                    # }



                    self$results$text1$setContent(
                        list(
                            mydata = head(mydata, n = 10)
                            # data = head(data, n = 10),
                            # levels = levels(mydata[["Measurement"]]),
                            # table = table(mydata[["Measurement"]]),
                            )
                    )



            }



            ,
            .plot = function(image, ggtheme, theme, ...) {

                # the plot function ----
                # Error messages ----

                if (is.null(self$options$dep))
                    return()

                if (nrow(self$data) == 0)
                    stop('Data contains no (complete) rows')


                # Prepare Data ----



                # Exclude NA ----

                # excl <- self$options$excl
                #
                # if (excl) {
                #     mydata <- jmvcore::naOmit(mydata)
                # }



                # originaltheme <- self$options$originaltheme

                # plot <-
                #     ggstatsplot::ggwithinstats(
                #         data = mydata,
                #         x = "Measurement",
                #         y = "Values",
                #         type = "parametric",
                #         pairwise.comparisons = FALSE,
                #         pairwise.display = "significant",
                #         p.adjust.method = "holm",
                #         effsize.type = "unbiased",
                #         partial = TRUE,
                #         bf.prior = 0.707,
                #         bf.message = TRUE,
                #         sphericity.correction = TRUE,
                #         results.subtitle = TRUE,
                #         xlab = NULL,
                #         ylab = NULL,
                #         caption = NULL,
                #         title = NULL,
                #         subtitle = NULL,
                #         sample.size.label = TRUE,
                #         k = 2,
                #         conf.level = 0.95,
                #         nboot = 100,
                #         tr = 0.1,
                #         mean.plotting = TRUE,
                #         mean.ci = FALSE,
                #         mean.point.args = list(size = 5, color = "darkred"),
                #         mean.label.args = list(size = 3),
                #         point.path = TRUE,
                #         point.path.args = list(alpha = 0.5, linetype = "dashed"),
                #         mean.path = TRUE,
                #         mean.path.args = list(
                #             color = "red",
                #             size = 1,
                #             alpha = 0.5
                #         ),
                #         notch = FALSE,
                #         notchwidth = 0.5,
                #         outlier.tagging = FALSE,
                #         outlier.label = NULL,
                #         outlier.coef = 1.5,
                #         outlier.label.args = list(),
                #         outlier.point.args = list(),
                #         violin.args = list(width = 0.5, alpha = 0.2),
                #         ggtheme = ggtheme,
                #         # ggtheme = ggplot2::theme_bw(),
                #         ggstatsplot.layer = originaltheme,
                #         package = "RColorBrewer",
                #         palette = "Dark2",
                #         ggplot.component = NULL,
                #         output = "plot",
                #         messages = TRUE
                #     )



                # Print Plot ----

                # print(plot)
                # TRUE

            }




        )
    )
