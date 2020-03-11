#' @importFrom R6 R6Class
#' @import jmvcore
#' @import ggplot2
#' @import ggstatsplot
#' @import ggalluvial
#' @importFrom rlang .data
#'


statsplot2Class <- if (requireNamespace('jmvcore')) R6::R6Class(
    "statsplot2Class",
    inherit = statsplot2Base,
    private = list(
        .run = function() {

            # TODO

            todo <- glue::glue(
                "This Module is still under development
                -
                -  "
            )

            self$results$todo$setContent(todo)

        },

        .plot=function(image, ...) {  # <-- the plot function

            if (length(self$options$dep) + length(self$options$group) < 2)
                return()

            mydata <- self$data

            mydep <- self$data[[self$options$dep]]
            mygroup <- self$data[[self$options$group]]

            direction <- self$data[[self$options$direction]]
            typex <- self$data[[self$options$typex]]
            typey <- self$data[[self$options$typey]]

            # https://indrajeetpatil.github.io/ggstatsplot/

            if (direction == "independent" & typex == "categorical" & typey == "continuous") {

                # ggbetweenstats 	violin plots 	for comparisons between groups/conditions

                plot <- ggstatsplot::ggbetweenstats(
                    data = mydata,
                    x = mygroup,
                    y = mydep
                    )

                } else if (direction == "independent" & typex == "continuous" & typey == "continuous") {

                    # ggscatterstats 	scatterplots 	for correlations between two variables

                    plot <- ggstatsplot::ggscatterstats(
                        data = mydata,
                        x = mygroup,
                        y = mydep
                    )


                } else if (direction == "independent" & typex == "categorical" & typey == "categorical") {

                # ggbarstats 	bar charts 	for categorical data

                    plot <- ggstatsplot::ggbarstats(
                        data = mydata,
                        x = mygroup,
                        y = mydep
                    )


                } else if (direction == "independent" & typex == "continuous" & typey == "categorical") {


                    plot <- "Not Available"




                } else if (direction == "dependent" & typex == "categorical" & typey == "continuous") {

                    # ggwithinstats 	violin plots 	for comparisons within groups/conditions

                    plot <- ggstatsplot::ggwithinstats(
                        data = mydata,
                        x = mygroup,
                        y = mydep
                    )

                } else if (direction == "dependent" & typex == "continuous" & typey == "continuous") {

                    plot <- "Not Available"


                } else if (direction == "dependent" & typex == "categorical" & typey == "categorical") {

                     # http://corybrunson.github.io/ggalluvial/


                    mydata_changes <- mydata %>%
                        dplyr::select(.data[[mygroup]], .data[[mygroup]]) %>%
                        dplyr::filter(complete.cases(.)) %>%
                        dplyr::group_by(.data[[mygroup]], .data[[mygroup]]) %>%
                        dplyr::tally()

                    # plot <- ggplot(data = mydata_changes,
                    #        aes(axis1 = TumorEcadherin, axis2 = TomurcukEcadherin,
                    #            y = n)) +
                    #     scale_x_discrete(limits = c("TumorEcadherin", "TomurcukEcadherin"),
                    #                      expand = c(.1, .05)
                    #     ) +
                    #     xlab("Tumor Tomurcuk") +
                    #     geom_alluvium(aes(fill = PeritumoralTomurcukGr4,
                    #                       colour = PeritumoralTomurcukGr4
                    #     )) +
                    #     geom_stratum() +
                    #     geom_text(stat = "stratum", label.strata = TRUE) +
                    #     theme_minimal() +
                    #     ggtitle("Changes in Ecadherin")



                    plot <- "Under Construction"

                } else if (direction == "dependent" & typex == "continuous" & typey == "categorical") {


                    plot <- "Not Available"




                }


            print(plot)
            TRUE



        })
)
