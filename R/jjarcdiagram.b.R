#' @title Arc Diagram
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import arcdiagram


jjarcdiagramClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "jjarcdiagramClass",
    inherit = jjarcdiagramBase,
    private = list(
        # Init function
        .init = function() {
            if (is.null(self$options$source) || is.null(self$options$target))
                return()

            # Set plot size
            self$results$plot$setSize(800, 600)
        },

        # Run function
        .run = function() {
            if (is.null(self$options$source) || is.null(self$options$target)) {
                todo <- glue::glue(
                    "<br>Welcome to ClinicoPath
                    <br><br>
                    This tool will help you create Arc Diagrams for network visualization.
                    <br><br>
                    Please select source and target node variables to create the network.
                    <br><hr>"
                )
                self$results$todo$setContent(todo)
                return()
            }

            todo <- glue::glue(
                "<br>Creating arc diagram visualization...<br><hr>"
            )
            self$results$todo$setContent(todo)

            if (nrow(self$data) == 0)
                stop('Data contains no (complete) rows')
        },

        # Plot function
        .plot = function(image, ggtheme, theme, ...) {
            # Check for required variables
            if (is.null(self$options$source) || is.null(self$options$target))
                return()

            if (nrow(self$data) == 0)
                stop('Data contains no (complete) rows')

            # Prepare data
            mydata <- self$data
            mydata <- jmvcore::naOmit(mydata)

            # Create edgelist
            source <- self$options$source
            target <- self$options$target
            weight <- self$options$weight
            group <- self$options$group

            # Create edge list matrix
            edgelist <- as.matrix(mydata[c(source, target)])

            # Get edge weights if specified
            if (!is.null(weight)) {
                weights <- mydata[[weight]]
            } else {
                weights <- rep(1, nrow(edgelist))
            }

            # Create igraph object
            g <- igraph::graph_from_edgelist(edgelist, directed = TRUE)

            # Get vertex attributes
            vlabels <- unique(c(edgelist[,1], edgelist[,2]))
            degrees <- igraph::degree(g)

            # Node ordering
            ord <- switch(self$options$sortNodes,
                          "none" = seq_along(vlabels),
                          "name" = order(vlabels, decreasing = self$options$sortDecreasing),
                          "degree" = order(degrees, decreasing = self$options$sortDecreasing),
                          "group" = {
                              if (!is.null(group)) {
                                  vgroups <- mydata[[group]][match(vlabels, mydata[[source]])]
                                  order(vgroups, decreasing = self$options$sortDecreasing)
                              } else {
                                  seq_along(vlabels)
                              }
                          }
            )

            # Node sizes
            if (self$options$nodeSize == "fixed") {
                node_sizes <- rep(self$options$nodeSizeValue, length(vlabels))
            } else {
                node_sizes <- log(degrees + 1) + 1
            }

            # Arc widths
            if (self$options$arcWidth == "fixed") {
                arc_widths <- rep(self$options$arcWidthValue, length(weights))
            } else {
                arc_widths <- scales::rescale(weights, to = c(0.5, 3))
            }

            # Create the plot
            oldpar <- par(mar = c(1, 1, 1, 1))
            on.exit(par(oldpar))

            arcplot(
                edgelist,
                ordering = ord,
                horizontal = self$options$horizontal,
                labels = vlabels,
                show.nodes = self$options$showNodes,
                col.nodes = "black",
                bg.nodes = "lightgray",
                cex.nodes = node_sizes,
                pch.nodes = 21,
                lwd.nodes = 1,
                col.arcs = hsv(0, 0, 0.2, self$options$arcTransparency),
                lwd.arcs = arc_widths
            )

            TRUE
        }
    )
)
