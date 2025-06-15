#' @title Study Flowchart
#' @importFrom R6 R6Class
#' @import jmvcore
#' @importFrom magrittr %>%
#' @importFrom DiagrammeR create_graph create_node_df create_edge_df add_nodes_from_table add_edges_from_table add_global_graph_attrs render_graph
#' @importFrom dplyr mutate case_when
#' @description Creates CONSORT-style flowcharts for clinical studies and research workflows

flowchartClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "flowchartClass",
    inherit = flowchartBase,
    private = list(
        .run = function() {

            if (is.null(self$options$nodes) || is.null(self$options$counts)) {
                todo <- "
                <div style='font-family: Arial, sans-serif; color: #2c3e50;'>
                  <h3>Welcome to ClinicoPath Flowchart Creator</h3>
                  <p>This tool creates <strong>CONSORT-style flowcharts</strong> for clinical studies and research workflows.</p>
                  
                  <h4>How to use:</h4>
                  <ol>
                    <li><strong>Node Data:</strong> Add variables containing step descriptions (e.g., 'Assessed for eligibility', 'Randomized')</li>
                    <li><strong>Node Counts:</strong> Add corresponding count variables (e.g., participant numbers at each step)</li>
                    <li><strong>Customize:</strong> Adjust layout, colors, and styling options</li>
                  </ol>
                  
                  <h4>Features:</h4>
                  <ul>
                    <li>CONSORT-compliant flowcharts</li>
                    <li>Multiple layout directions</li>
                    <li>Custom node styling and colors</li>
                    <li>Automatic data validation</li>
                    <li>Export-ready diagrams</li>
                  </ul>
                  
                  <p><em>Perfect for clinical trial reporting, study workflows, and research documentation.</em></p>
                  <hr>
                </div>"
                self$results$todo$setContent(todo)
                return()
            }

            # Validate data availability
            if (nrow(self$data) == 0) {
                stop("Error: The provided dataset contains no rows. Please check your data and try again.")
            }

            # Get data
            data <- self$data
            
            # Get node labels and counts
            nodeVars <- self$options$nodes
            countVars <- self$options$counts
            
            # Enhanced validation
            if (length(nodeVars) == 0 && length(countVars) == 0) {
                self$results$todo$setContent("Please select both Node Data and Node Counts variables.")
                return()
            }
            
            if (length(nodeVars) != length(countVars)) {
                stop("Error: Number of node variables must match number of count variables. Please ensure each node has a corresponding count.")
            }

            # Enhanced data processing
            nodeData <- data.frame(
                id = seq_along(nodeVars),
                label = character(length(nodeVars)),
                count = numeric(length(nodeVars)),
                valid = logical(length(nodeVars)),
                stringsAsFactors = FALSE
            )

            # Process each node with better error handling
            for (i in seq_along(nodeVars)) {
                tryCatch({
                    # Get node label - use first non-NA value or handle different data types
                    node_data <- data[[nodeVars[i]]]
                    if (is.factor(node_data) || is.character(node_data)) {
                        valid_labels <- node_data[!is.na(node_data) & node_data != ""]
                        nodeData$label[i] <- if (length(valid_labels) > 0) as.character(valid_labels[1]) else paste("Node", i)
                    } else {
                        nodeData$label[i] <- paste("Node", i)
                    }
                    
                    # Get count - use first non-NA numeric value
                    count_data <- data[[countVars[i]]]
                    if (is.numeric(count_data)) {
                        valid_counts <- count_data[!is.na(count_data)]
                        nodeData$count[i] <- if (length(valid_counts) > 0) valid_counts[1] else 0
                    } else {
                        # Try to convert to numeric
                        numeric_count <- as.numeric(as.character(count_data))
                        valid_counts <- numeric_count[!is.na(numeric_count)]
                        nodeData$count[i] <- if (length(valid_counts) > 0) valid_counts[1] else 0
                    }
                    
                    nodeData$valid[i] <- TRUE
                    
                }, error = function(e) {
                    warning(paste("Error processing node", i, ":", e$message))
                    nodeData$label[i] <- paste("Node", i, "(Error)")
                    nodeData$count[i] <- 0
                    nodeData$valid[i] <- FALSE
                })
            }

            # Filter out invalid nodes if requested
            if (any(!nodeData$valid)) {
                warning("Some nodes had processing errors and were given default values.")
            }

            # Add validation summary
            valid_nodes <- sum(nodeData$valid)
            total_nodes <- nrow(nodeData)
            
            if (valid_nodes < total_nodes) {
                message(paste("Processed", valid_nodes, "out of", total_nodes, "nodes successfully."))
            }

            # Add to results table with enhanced information
            for (i in seq_len(nrow(nodeData))) {
                self$results$nodeData$addRow(rowKey=i, values=list(
                    node = nodeData$label[i],
                    count = nodeData$count[i]
                ))
            }

            # Calculate flow statistics
            flow_stats <- list(
                total_nodes = nrow(nodeData),
                max_count = max(nodeData$count, na.rm = TRUE),
                min_count = min(nodeData$count, na.rm = TRUE),
                total_flow = sum(nodeData$count, na.rm = TRUE)
            )

            # Save enhanced data for plot
            image <- self$results$diagram
            image$setState(list(
                nodes = nodeData,
                direction = self$options$direction,
                nodeWidth = self$options$nodeWidth,
                nodeHeight = self$options$nodeHeight,
                fontSize = self$options$fontSize,
                showPercentages = self$options$showPercentages,
                showExclusions = self$options$showExclusions,
                nodeColor = self$options$nodeColor,
                includeTitle = self$options$includeTitle,
                flow_stats = flow_stats
            ))

        },

        .plot = function(image, ggtheme, theme, ...) {
            if (is.null(image$state)) return()
            
            plotData <- image$state
            nodeData <- plotData$nodes
            flow_stats <- plotData$flow_stats
            
            # Validate we have nodes to plot
            if (is.null(nodeData) || nrow(nodeData) == 0) {
                warning("No valid node data available for plotting")
                return(FALSE)
            }
            
            # Enhanced node styling based on count ranges
            n_nodes <- nrow(nodeData)
            max_count <- flow_stats$max_count
            min_count <- flow_stats$min_count
            
            # Calculate node colors based on selected scheme
            base_colors <- switch(plotData$nodeColor,
                "blue" = c("lightblue", "lightsteelblue", "skyblue", "cornflowerblue"),
                "gray" = c("lightgray", "silver", "darkgray", "gray"),
                "green" = c("lightgreen", "palegreen", "lightseagreen", "mediumseagreen"),
                c("lightblue", "lightsteelblue", "skyblue", "cornflowerblue")  # default
            )
            
            if (max_count > min_count) {
                # Scale node colors by count (higher counts = more intense)
                color_intensity <- (nodeData$count - min_count) / (max_count - min_count)
                color_indices <- as.integer(color_intensity * (length(base_colors) - 1)) + 1
                node_colors <- base_colors[color_indices]
            } else {
                node_colors <- rep(base_colors[1], n_nodes)
            }
            
            # Enhanced positioning for better layout
            if (plotData$direction == "TB") {
                # Top to bottom: center horizontally, space vertically
                x_pos <- rep(2, n_nodes)
                y_pos <- seq(n_nodes, 1, by = -1)  # Reverse order for top-down
            } else if (plotData$direction == "LR") {
                # Left to right: space horizontally, center vertically  
                x_pos <- seq(1, n_nodes)
                y_pos <- rep(2, n_nodes)
            } else {
                # Default positioning
                x_pos <- seq(1, n_nodes)
                y_pos <- rep(2, n_nodes)
            }
            
            # Enhanced label formatting for clinical context
            enhanced_labels <- character(n_nodes)
            for (i in 1:n_nodes) {
                count_text <- if (nodeData$count[i] > 0) {
                    paste0("n = ", nodeData$count[i])
                } else {
                    "n = 0"
                }
                
                # Add percentage if requested and not first node
                if (plotData$showPercentages && i > 1 && nodeData$count[1] > 0) {
                    pct <- round((nodeData$count[i] / nodeData$count[1]) * 100, 1)
                    count_text <- paste0(count_text, "\n(", pct, "%)")
                }
                
                enhanced_labels[i] <- paste0(nodeData$label[i], "\n", count_text)
            }
            
            # Create enhanced nodes dataframe for DiagrammeR
            nodes_df <- DiagrammeR::create_node_df(
                n = n_nodes,
                type = "flow_node",
                label = enhanced_labels,
                shape = "rectangle",
                width = plotData$nodeWidth,
                height = plotData$nodeHeight,
                fontsize = plotData$fontSize,
                fillcolor = node_colors,
                color = "black",
                fontcolor = "black",
                style = "filled,rounded",
                x = x_pos,
                y = y_pos
            )
            
            # Create base graph with enhanced attributes
            graph <- DiagrammeR::create_graph() %>%
                DiagrammeR::add_global_graph_attrs(
                    attr = "rankdir",
                    value = plotData$direction,
                    attr_type = "graph"
                ) %>%
                DiagrammeR::add_global_graph_attrs(
                    attr = "splines",
                    value = "ortho",  # Orthogonal edges for cleaner look
                    attr_type = "graph"
                ) %>%
                DiagrammeR::add_global_graph_attrs(
                    attr = "nodesep",
                    value = "0.8",
                    attr_type = "graph"
                ) %>%
                DiagrammeR::add_global_graph_attrs(
                    attr = "ranksep",
                    value = "1.2",
                    attr_type = "graph"
                )
            
            # Add nodes to graph
            graph <- graph %>%
                DiagrammeR::add_nodes_from_table(
                    nodes_df,
                    label_col = "label",
                    shape_col = "shape",
                    width_col = "width", 
                    height_col = "height",
                    fontsize_col = "fontsize",
                    fillcolor_col = "fillcolor",
                    color_col = "color",
                    fontcolor_col = "fontcolor",
                    style_col = "style",
                    x_col = "x",
                    y_col = "y"
                )
            
            # Create enhanced edges between consecutive nodes
            if (n_nodes > 1) {
                # Calculate exclusion counts for edge labels if requested
                edge_labels <- character(n_nodes - 1)
                if (plotData$showExclusions) {
                    for (i in 1:(n_nodes - 1)) {
                        excluded <- nodeData$count[i] - nodeData$count[i + 1]
                        if (excluded > 0) {
                            edge_labels[i] <- paste0("Excluded: ", excluded)
                        } else {
                            edge_labels[i] <- ""
                        }
                    }
                } else {
                    edge_labels <- rep("", n_nodes - 1)
                }
                
                edges_df <- DiagrammeR::create_edge_df(
                    from = seq_len(n_nodes-1),
                    to = seq(2, n_nodes),
                    label = edge_labels,
                    color = "darkgray",
                    arrowsize = "0.8",
                    fontsize = plotData$fontSize - 2
                )
                
                graph <- graph %>%
                    DiagrammeR::add_edges_from_table(
                        edges_df,
                        from_col = "from",
                        to_col = "to",
                        label_col = "label",
                        color_col = "color",
                        arrowsize_col = "arrowsize",
                        fontsize_col = "fontsize"
                    )
            }
            
            # Add title based on flow statistics if requested
            if (plotData$includeTitle) {
                title_text <- paste0(
                    "Study Flow Diagram\\n",
                    "Total Participants: ", flow_stats$max_count, "\\n",
                    "Final Sample: ", flow_stats$min_count,
                    " (", round((flow_stats$min_count / flow_stats$max_count) * 100, 1), "%)"
                )
                
                graph <- graph %>%
                    DiagrammeR::add_global_graph_attrs(
                        attr = "label",
                        value = title_text,
                        attr_type = "graph"
                    ) %>%
                    DiagrammeR::add_global_graph_attrs(
                        attr = "labelloc",
                        value = "t",  # Top
                        attr_type = "graph"
                    ) %>%
                    DiagrammeR::add_global_graph_attrs(
                        attr = "fontsize",
                        value = as.character(plotData$fontSize + 2),
                        attr_type = "graph"
                    )
            }
            
            # Render enhanced graph
            tryCatch({
                DiagrammeR::render_graph(graph)
            }, error = function(e) {
                warning(paste("Error rendering flowchart:", e$message))
                # Fallback to simpler rendering
                simple_graph <- DiagrammeR::create_graph() %>%
                    DiagrammeR::add_nodes_from_table(nodes_df) %>%
                    DiagrammeR::add_global_graph_attrs("rankdir", plotData$direction, "graph")
                
                if (n_nodes > 1) {
                    simple_edges <- DiagrammeR::create_edge_df(
                        from = seq_len(n_nodes-1),
                        to = seq(2, n_nodes)
                    )
                    simple_graph <- simple_graph %>%
                        DiagrammeR::add_edges_from_table(simple_edges)
                }
                
                DiagrammeR::render_graph(simple_graph)
            })
            
            TRUE
        }
    )
)