#' @title Study Flowchart
#' @importFrom R6 R6Class
#' @import jmvcore
#'
flowchartClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "flowchartClass",
    inherit = flowchartBase,
    private = list(
        .run = function() {

            # Show initial help message if no variables selected
            if (is.null(self$options$nodes)) {
                todo <- "
                <br>Welcome to ClinicoPath Flowchart Creator
                <br><br>
                This tool helps create CONSORT-style flow diagrams showing participant flow through studies.
                <br><br>
                To create a flowchart:
                1. Add variables containing node descriptions to 'Node Data'
                2. Add corresponding count variables to 'Node Counts'
                3. Adjust direction and style options as needed
                <hr>
                "
                html <- self$results$todo
                html$setContent(todo)
                return()
            }

            if (nrow(self$data) == 0)
                stop('Data contains no (complete) rows')

            # Prepare data for plotting
            plotData <- list(
                "data" = self$data,
                "nodes" = self$options$nodes,
                "counts" = self$options$counts,
                "direction" = self$options$direction,
                "style" = self$options$style,
                "showCounts" = self$options$showCounts,
                "title" = self$options$title
            )

            # Save state for plot function
            image <- self$results$diagram
            image$setState(plotData)
        },

        .plot = function(image, ggtheme, theme, ...) {
            # Get plot data
            plotData <- image$state

            # Create nodes and edges dataframe
            nodes_df <- data.frame(
                id = seq_along(plotData$nodes),
                label = paste0(
                    plotData$data[[plotData$nodes[1]]],
                    ifelse(
                        plotData$showCounts,
                        paste0("\n(n=", plotData$data[[plotData$counts[1]]], ")"),
                        ""
                    )
                ),
                stringsAsFactors = FALSE
            )

            edges_df <- data.frame(
                from = head(nodes_df$id, -1),
                to = tail(nodes_df$id, -1),
                stringsAsFactors = FALSE
            )

            # Create graph
            graph <- DiagrammeR::create_graph()
            graph <- DiagrammeR::add_nodes_from_table(
                graph,
                nodes_df,
                label_col = "label"
            )
            graph <- DiagrammeR::add_edges_from_table(
                graph,
                edges_df,
                from_col = "from",
                to_col = "to"
            )

            # Set graph attributes based on style
            if (plotData$style == "detailed") {
                graph <- DiagrammeR::set_global_graph_attrs(
                    graph,
                    "layout", plotData$direction,
                    "rankdir", plotData$direction,
                    "splines", "ortho"
                )
            } else {
                graph <- DiagrammeR::set_global_graph_attrs(
                    graph,
                    "layout", plotData$direction,
                    "rankdir", plotData$direction
                )
            }

            # Add title
            graph <- DiagrammeR::add_global_graph_attrs(
                graph,
                "label", plotData$title,
                "labelloc", "t"
            )

            # Render graph
            print(graph)
            TRUE
        }
    )
)
