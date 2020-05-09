#' Plots and Graphs Based on Variable Types
#'


#'
#' 
#'
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import ggplot2
#' @import ggstatsplot
#' @import ggalluvial
#' @importFrom ggalluvial StatStratum
#'



statsplot2Class <- if (requireNamespace('jmvcore'))
    R6::R6Class(
        "statsplot2Class",
        inherit = statsplot2Base,
        private = list(
            .run = function() {

                StatStratum <- ggalluvial::StatStratum


                # If no variable selected Initial Message ----
                if (is.null(self$options$dep) ||
                    is.null(self$options$group)) {
                    # TODO ----

                    todo <- glue::glue(
                        "
                <br>Welcome to ClinicoPath
                <br><br>
                This tool will help you generate plots based on variable types.
                <br><br>
                This function uses ggstatsplot and ggalluvial packages. Please cite jamovi and the packages as given below.
                                   "
                    )

                    self$results$todo$setContent(todo)

                    return()

                } else {
                    todo <- ""
                    self$results$todo$setContent(todo)


                    if (nrow(self$data) == 0)
                        stop('Data contains no (complete) rows')


                    # mydata <- self$data

                    mydep <- self$data[[self$options$dep]]

                    mygroup <- self$data[[self$options$group]]

                    contin <- c("integer", "numeric", "double")
                    # categ <- c("factor")

                    distribution <- self$options$distribution
                    distribution <- jmvcore::composeTerm(distribution)

                    direction <- self$options$direction
                    direction <- jmvcore::composeTerm(direction)


                    # klass <- print(list(
                    #     "mydep" = c(typeof(mydep), class(mydep)),
                    #     "mydep2" = c(
                    #         inherits(mydep, "factor"),
                    #         inherits(mydep, "character"),
                    #         inherits(mydep, "integer"),
                    #         inherits(mydep, "numeric"),
                    #         inherits(mydep, contin)
                    #     ),
                    #     "mygroup" = c(typeof(mygroup), class(mygroup)),
                    #     "a" = c(distribution, direction)
                    # ))
                    #
                    #
                    # self$results$text1$setContent(klass)



                    # independent ----

                    if (direction == "independent") {
                        # independent, factor, continuous ----
                        if (inherits(mygroup, "factor") &&
                            inherits(mydep, contin)) {
                            # ggbetweenstats 	violin plots 	for comparisons between groups/conditions
                            # stat_exp <- c("independent", "factor", "continuous")
                            stat_exp <-
                                glue::glue(
                                    "You have selected to use a violin plot to compare a continuous variable between independent groups/conditions.
                                           "
                                )
                    # independent, continuous, continuous ----
                        } else if (inherits(mygroup, contin) &&
                                   inherits(mydep, contin)) {
                            # ggscatterstats 	scatterplots 	for correlations between two variables
                            # stat_exp <- c("independent", "continuous", "continuous")

                            stat_exp <-
                                glue::glue(
                                    "You have selected to use a scatter plot to compare a continuous variable to another.
                                           "
                                )


                    # independent, factor, factor ----
                        } else if (inherits(mygroup, "factor") &&
                                   inherits(mydep, "factor")) {
                            # stat_exp <- c("independent", "factor", "factor")

                            stat_exp <-
                                glue::glue(
                                    "You have selected to use a barplot to compare a group variable with another.
                                           "
                                )



                    # independent, continuous, factor ----
                        } else if (inherits(mygroup, contin) &&
                                   inherits(mydep, "factor")) {
stat_exp <- glue::glue("Please switch the variables to generate a plot.")
                        }
                        # repeated ----
                    } else if (direction == "repeated") {
                        # repeated, factor, continuous ----
                        if (inherits(mygroup, "factor") &&
                            inherits(mydep, contin)) {
                            # ggwithinstats 	violin plots 	for comparisons within groups/conditions
                            # stat_exp <-
                            #     c("repeated", "factor", "continuous")

                            stat_exp <- glue::glue(
                                "You have selected to use a violin plot to compare a continuous variable between repeated measurements.
                                           "
                            )


                            # repeated, continuous, continuous ----
                        } else if (inherits(mygroup, contin) &&
                                   inherits(mydep, contin)) {
                            # stat_exp <- c("repeated", "continuous", "continuous")

                            stat_exp <- glue::glue(
                                "Currently this tool does not support scatterplots for repeated measurements. You may refer to R-project rmcorr package."
                            )





                            # repeated, factor, factor ----
                        } else if (inherits(mygroup, "factor") &&
                                   inherits(mydep, "factor")) {
                            # stat_exp <- c("repeated", "factor", "factor")

                            stat_exp <-
                                glue::glue(
                                    "You have selected to compare 2 repeated factor measurements.")



                            # repeated, continuous, factor ----
                        } else if (inherits(mygroup, contin) &&
                                   inherits(mydep, "factor")) {


                            stat_exp <- glue::glue(
                                "Please switch the variables to generate a plot.
                                           "
                            )


                        }
                    }

                    self$results$text4$setContent(stat_exp)



                }
            },

            .plot = function(image, ...) {
                # the plot function ----


                if (is.null(self$options$dep) ||
                    is.null(self$options$group))
                    return()

                if (nrow(self$data) == 0)
                    stop('Data contains no (complete) rows')

                direction <- self$options$direction


                distribution <-
                    jmvcore::constructFormula(terms = self$options$distribution)

                mydata <- self$data

                mydep <- self$data[[self$options$dep]]

                mygroup <- self$data[[self$options$group]]

                contin <- c("integer", "numeric", "double")
                categ <- c("factor")


                # independent ----

                # independent, factor, continuous ----

                if (direction == "independent") {
                    if (inherits(mygroup, "factor") && inherits(mydep, contin)) {
                        # ggbetweenstats 	violin plots 	for comparisons between groups/conditions

                        plotData <- data.frame(gr = mygroup,
                                               dp = jmvcore::toNumeric(mydep))
                        plotData <- jmvcore::naOmit(plotData)

                        plot <- ggstatsplot::ggbetweenstats(
                            data = plotData,
                            x = gr,
                            y = dp,
                            type = distribution
                        )

                        # independent, continuous, continuous ----

                    } else if (inherits(mygroup, contin) &&
                               inherits(mydep, contin)) {
                        # ggscatterstats 	scatterplots 	for correlations between two variables

                        plotData <-
                            data.frame(gr = jmvcore::toNumeric(mygroup),
                                       dp = jmvcore::toNumeric(mydep))
                        plotData <- jmvcore::naOmit(plotData)

                        plot <- ggstatsplot::ggscatterstats(data = plotData,
                                                            x = gr,
                                                            y = dp)

                        # independent, factor, factor ----

                    } else if (inherits(mygroup, "factor") &&
                               inherits(mydep, "factor")) {
                        # ggbarstats 	bar charts 	for categorical data

                        plotData <- data.frame(gr = mygroup,
                                               dp = mydep)

                        plotData <- jmvcore::naOmit(plotData)

                        plot <- ggstatsplot::ggbarstats(data = plotData,
                                                        main = gr,
                                                        condition = dp)

                        # independent, continuous, factor ----

                    } else if (inherits(mygroup, contin) &&
                               inherits(mydep, "factor")) {
                        plot <- "Not Available"

                    }


                    # repeated ----


                } else if (direction == "repeated") {
                    # repeated, factor, continuous ----

                    if (inherits(mygroup, "factor") &&
                        inherits(mydep, contin)) {
                        # ggwithinstats 	violin plots 	for comparisons within groups/conditions

                        plotData <- data.frame(gr = mygroup,
                                               dp = jmvcore::toNumeric(mydep))
                        plotData <- jmvcore::naOmit(plotData)


                        plot <- ggstatsplot::ggwithinstats(
                            data = plotData,
                            x = gr,
                            y = dp,
                            type = distribution
                        )

                        # repeated, continuous, continuous ----


                    } else if (inherits(mygroup, contin) &&
                               inherits(mydep, contin)) {
                        plot <- c("Not Available")



                        # my.rmc <- rmcorr::rmcorr(participant = Subject,
                        #                          measure1 = PacO2,
                        #                          measure2 = pH,
                        #                          dataset = rmcorr::bland1995)
                        #
                        # plot(my.rmc, overall = TRUE)
                        #
                        # ggplot2::ggplot(rmcorr::bland1995,
                        #                 ggplot2::aes(x = PacO2,
                        #                              y = pH,
                        #                              group = factor(Subject),
                        #                              color = factor(Subject)
                        #                 )
                        # ) +
                        #     ggplot2::geom_point(ggplot2::aes(colour = factor(Subject))) +
                        #     ggplot2::geom_line(ggplot2::aes(y = my.rmc$model$fitted.values), linetype = 1)



                        # repeated, factor, factor ----



                    } else if (inherits(mygroup, "factor") &&
                               inherits(mydep, "factor")) {
                        # http://corybrunson.github.io/ggalluvial/


                        plotData <- data.frame(gr = mygroup,
                                               dp = mydep)

                        plotData <- jmvcore::naOmit(plotData)


                        mydata_changes <- plotData %>%
                            dplyr::group_by(gr, dp) %>%
                            dplyr::tally(x = .)


                        # head(as.data.frame(UCBAdmissions), n = 12)

                        # ggalluvial::is_alluvia_form(
                        #     as.data.frame(UCBAdmissions),
                        #     axes = 1:3, silent = TRUE)



                        # plot <- ggplot(as.data.frame(UCBAdmissions),
                        #        aes(y = Freq, axis1 = Gender, axis2 = Dept)) +
                        #     geom_alluvium(aes(fill = Admit), width = 1/12) +
                        #     geom_stratum(width = 1/12, fill = "black", color = "grey") +
                        #     geom_label(stat = "stratum", infer.label = TRUE) +
                        #     scale_x_discrete(limits = c("Gender", "Dept"), expand = c(.05, .05)) +
                        #     scale_fill_brewer(type = "qual", palette = "Set1") +
                        #     ggtitle("UC Berkeley admissions and rejections, by sex and department")





                        stratum <- ggalluvial::StatStratum

                        plot <- ggplot2::ggplot(data = mydata_changes,
                                                ggplot2::aes(
                                                    axis1 = gr,
                                                    axis2 = dp,
                                                    y = n
                                                )) +
                            ggplot2::scale_x_discrete(
                                limits = c(self$options$group, self$options$dep),
                                expand = c(.1, .05)
                            ) +
                            ggplot2::xlab(self$options$group) +
                            ggalluvial::geom_alluvium(ggplot2::aes(fill = gr,
                                                                   colour = gr)) +
                            ggalluvial::geom_stratum() +
                            ggalluvial::stat_stratum(geom = "stratum") +
                            ggplot2::geom_label(stat = stratum, infer.label = TRUE) +

                            # ggalluvial::geom_stratum(stat = "stratum", label.strata = TRUE) +
                            # ggplot2::geom_text(stat = "stratum", infer.label = TRUE) +
                            # ggplot2::geom_text(label.strata = TRUE) +
                            # ggalluvial::geom_stratum()
                            ggplot2::theme_minimal()
                        # ggplot2::ggtitle(paste0("Changes in ", self$options$group))
                        #
                        #
                        # nodes = data.frame("name" =
                        #                        c(self$options$group,
                        #                          self$options$dep))
                        #
                        # links <- mydata_changes
                        #
                        # names(links) = c("source", "target", "value")
                        #
                        # plot <- networkD3::sankeyNetwork(Links = links, Nodes = nodes,
                        #               Source = "source", Target = "target",
                        #               Value = "value", NodeID = "name",
                        #               fontSize= 12, nodeWidth = 30)

                        # library(networkD3)
                        # nodes = data.frame("name" =
                        #                        c("Node A", # Node 0
                        #                          "Node B", # Node 1
                        #                          "Node C", # Node 2
                        #                          "Node D"))# Node 3
                        # links = as.data.frame(matrix(c(
                        #     0, 1, 10, # Each row represents a link. The first number
                        #     0, 2, 20, # represents the node being conntected from.
                        #     1, 3, 30, # the second number represents the node connected to.
                        #     2, 3, 40),# The third number is the value of the node
                        #     byrow = TRUE, ncol = 3))
                        # names(links) = c("source", "target", "value")
                        # sankeyNetwork(Links = links, Nodes = nodes,
                        #               Source = "source", Target = "target",
                        #               Value = "value", NodeID = "name",
                        #               fontSize= 12, nodeWidth = 30)

                        # plot <- c("Under Construction")

                        # plot <- list(plot1,
                        #              plot2)

                        # repeated, continuous, factor ----


                    } else if (inherits(mygroup, contin) &&
                               inherits(mydep, "factor")) {
                        plot <- c("Not Available")

                    }

                }


                print(plot)
                TRUE

            }


        )
    )
