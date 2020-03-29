#' @importFrom R6 R6Class
#' @import jmvcore
#' @import networkD3

# This file is a generated template, your changes will not be overwritten

statsplotbetweenClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "statsplotbetweenClass",
    inherit = statsplotbetweenBase,
    private = list(
        .run = function() {



            if (is.null(self$options$dep) || is.null(self$options$group))
                return()

            if (nrow(self$data) == 0)
                stop('Data contains no (complete) rows')


            mydata <- self$data

            mydep <- self$data[[self$options$dep]]

            mygroup <- self$data[[self$options$group]]

            plotData <- data.frame(gr = mygroup,
                                   dp = mydep)

            plotData <- jmvcore::naOmit(plotData)


            mydata_changes <- plotData %>%
                dplyr::group_by(gr, dp) %>%
                dplyr::tally(x = .)


            nodes = data.frame("name" =
                                   c(self$options$group,
                                     self$options$dep))

            links <- mydata_changes

            names(links) = c("source", "target", "value")


            self$results$text1$setContent(nodes)

            self$results$text2$setContent(links)


            # plothtml <- networkD3::sankeyNetwork(Links = links, Nodes = nodes,
            #                                  Source = "source", Target = "target",
            #                                  Value = "value", NodeID = "name",
            #                                  fontSize= 12, nodeWidth = 30)


            # Load energy projection data
            # Load energy projection data
            URL <- paste0(
                "https://cdn.rawgit.com/christophergandrud/networkD3/",
                "master/JSONdata/energy.json")
            Energy <- jsonlite::fromJSON(URL)
            # Plot
            plothtml <- networkD3::sankeyNetwork(
                Links = Energy$links,
                Nodes = Energy$nodes,
                Source = "source",
                          Target = "target", Value = "value", NodeID = "name",
                          units = "TWh", fontSize = 12, nodeWidth = 30)


            self$results$plothtml$setContent(plothtml)


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

        }

        ,

        .plot = function(image, ...) {  # <-- the plot function ----


            if (is.null(self$options$dep) || is.null(self$options$group))
                return()

            if (nrow(self$data) == 0)
                stop('Data contains no (complete) rows')



            mydata <- self$data

            mydep <- self$data[[self$options$dep]]

            mygroup <- self$data[[self$options$group]]

            plotData <- data.frame(gr = mygroup,
                                   dp = mydep)

            plotData <- jmvcore::naOmit(plotData)


            mydata_changes <- plotData %>%
                dplyr::group_by(gr, dp) %>%
                dplyr::tally(x = .)


            nodes = data.frame("name" =
                                   c(self$options$group,
                                     self$options$dep))

            links <- mydata_changes

            names(links) = c("source", "target", "value")

        #     plot <- networkD3::sankeyNetwork(Links = links, Nodes = nodes,
        #                                      Source = "source", Target = "target",
        #                                      Value = "value", NodeID = "name",
        #                                      fontSize= 12, nodeWidth = 30)



        # Load energy projection data
        # Load energy projection data
        URL <- paste0(
            "https://cdn.rawgit.com/christophergandrud/networkD3/",
            "master/JSONdata/energy.json")
        Energy <- jsonlite::fromJSON(URL)
        # Plot
        plot <- networkD3::sankeyNetwork(
            Links = Energy$links,
            Nodes = Energy$nodes,
            Source = "source",
            Target = "target", Value = "value", NodeID = "name",
            units = "TWh", fontSize = 12, nodeWidth = 30)


            print(plot)
            TRUE


        }


            )
)
