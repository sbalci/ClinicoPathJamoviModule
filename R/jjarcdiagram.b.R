#' @title Arc Diagram
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import arcdiagram
#' @import igraph
#' @import grDevices
#' @import RColorBrewer


jjarcdiagramClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "jjarcdiagramClass",
    inherit = jjarcdiagramBase,
    private = list(
        # Init function
        .init = function() {
            if (is.null(self$options$source) || is.null(self$options$target))
                return()

            # Set plot size based on layout
            plot_width <- if (self$options$horizontal) 800 else 600
            plot_height <- if (self$options$horizontal) 600 else 800
            self$results$plot$setSize(plot_width, plot_height)
            
            # Set up user instructions
            instructions <- private$.createInstructions()
            self$results$instructions$setContent(instructions)
        },

        # Run function
        .run = function() {
            # Check for required variables
            if (is.null(self$options$source) || is.null(self$options$target)) {
                todo <- glue::glue(
                    "<br>Welcome to ClinicoPath Arc Diagram
                    <br><br>
                    Create interactive network visualizations to explore relationships between entities.
                    <br><br>
                    <b>Required:</b>
                    <li>Source Node: Starting point of connections</li>
                    <li>Target Node: Endpoint of connections</li>
                    <br>
                    <b>Optional:</b>
                    <li>Edge Weight: Strength of connections</li>
                    <li>Node Groups: Categories for color coding</li>
                    <br><hr>"
                )
                self$results$todo$setContent(todo)
                return()
            }

            # Validate data
            if (nrow(self$data) == 0) {
                stop('Data contains no (complete) rows')
            }
            
            # Prepare and validate the network data
            network_data <- private$.prepareNetworkData()
            
            if (is.null(network_data)) {
                return()
            }
            
            # Generate network statistics
            if (self$options$showStats) {
                private$.generateNetworkStats(network_data)
            }

            # Update todo with processing status
            todo <- glue::glue(
                "<br>Network Analysis Complete
                <br>📊 Nodes: {network_data$n_nodes}
                <br>🔗 Edges: {network_data$n_edges}
                <br>📈 Density: {round(network_data$density, 3)}
                <br><hr>"
            )
            self$results$todo$setContent(todo)
        },

        # Plot function
        .plot = function(image, ggtheme, theme, ...) {
            # Check for required variables
            if (is.null(self$options$source) || is.null(self$options$target))
                return()

            if (nrow(self$data) == 0)
                stop('Data contains no (complete) rows')

            # Prepare network data
            network_data <- private$.prepareNetworkData()
            if (is.null(network_data)) return()

            # Create the plot with proper error handling
            tryCatch({
                private$.createArcPlot(network_data)
            }, error = function(e) {
                self$results$todo$setContent(
                    paste("<br><b>Error creating plot:</b>", e$message, "<br><hr>")
                )
                stop(e$message)
            })

            TRUE
        },
        
        # Helper method to create user instructions
        .createInstructions = function() {
            instructions <- "
            <h3>🌐 Arc Diagram Network Visualization</h3>
            
            <h4>📋 Getting Started:</h4>
            <ul>
              <li><strong>Source Node:</strong> Select the variable representing connection origins</li>
              <li><strong>Target Node:</strong> Select the variable representing connection destinations</li>
              <li><strong>Edge Weight:</strong> Optional numeric variable for connection strength</li>
              <li><strong>Node Groups:</strong> Optional categorical variable for color coding</li>
            </ul>
            
            <h4>🎨 Customization Options:</h4>
            <ul>
              <li><strong>Layout:</strong> Choose horizontal or vertical arrangement</li>
              <li><strong>Node Sizing:</strong> Fixed size or proportional to degree centrality</li>
              <li><strong>Node Sorting:</strong> Arrange by name, degree, or group</li>
              <li><strong>Arc Styling:</strong> Fixed width or proportional to weights</li>
              <li><strong>Color Schemes:</strong> Multiple palettes for different node groups</li>
            </ul>
            
            <h4>📊 Network Metrics:</h4>
            <ul>
              <li><strong>Density:</strong> Measure of network connectivity</li>
              <li><strong>Centrality:</strong> Node importance in the network</li>
              <li><strong>Communities:</strong> Group structure detection</li>
            </ul>
            
            <p><em>💡 Tip: Arc diagrams are ideal for visualizing hierarchical or sequential relationships with minimal visual clutter.</em></p>
            "
            return(instructions)
        },
        
        # Helper method to prepare network data
        .prepareNetworkData = function() {
            mydata <- self$data
            mydata <- jmvcore::naOmit(mydata)
            
            if (nrow(mydata) == 0) {
                self$results$todo$setContent("<br><b>Error:</b> No complete data rows available<br><hr>")
                return(NULL)
            }

            # Get variable names
            source_var <- self$options$source
            target_var <- self$options$target
            weight_var <- self$options$weight
            group_var <- self$options$group

            # Validate required columns exist
            if (!source_var %in% names(mydata) || !target_var %in% names(mydata)) {
                self$results$todo$setContent("<br><b>Error:</b> Source or target variables not found<br><hr>")
                return(NULL)
            }

            # Create edge list matrix
            edgelist <- as.matrix(mydata[c(source_var, target_var)])
            
            # Remove any rows with missing values
            complete_rows <- complete.cases(edgelist)
            edgelist <- edgelist[complete_rows, , drop = FALSE]
            
            if (nrow(edgelist) == 0) {
                self$results$todo$setContent("<br><b>Error:</b> No complete edge data available<br><hr>")
                return(NULL)
            }

            # Get edge weights if specified
            if (!is.null(weight_var) && weight_var %in% names(mydata)) {
                weights <- mydata[[weight_var]][complete_rows]
                weights[is.na(weights)] <- 1  # Replace missing weights with 1
            } else {
                weights <- rep(1, nrow(edgelist))
            }

            # Get group information if specified
            groups <- NULL
            if (!is.null(group_var) && group_var %in% names(mydata)) {
                groups <- mydata[[group_var]][complete_rows]
            }

            # Create igraph object for analysis
            g <- tryCatch({
                igraph::graph_from_edgelist(edgelist, directed = self$options$directed)
            }, error = function(e) {
                self$results$todo$setContent("<br><b>Error creating network:</b> Invalid edge structure<br><hr>")
                return(NULL)
            })
            
            if (is.null(g)) return(NULL)

            # Get vertex attributes
            vlabels <- unique(c(edgelist[,1], edgelist[,2]))
            degrees <- igraph::degree(g)
            
            # Calculate network metrics
            n_nodes <- length(vlabels)
            n_edges <- nrow(edgelist)
            density <- igraph::edge_density(g)

            return(list(
                edgelist = edgelist,
                weights = weights,
                groups = groups,
                g = g,
                vlabels = vlabels,
                degrees = degrees,
                n_nodes = n_nodes,
                n_edges = n_edges,
                density = density
            ))
        },
        
        # Helper method to create the arc plot
        .createArcPlot = function(network_data) {
            # Set up margins and plotting parameters
            oldpar <- par(mar = c(2, 2, 3, 2))
            on.exit(par(oldpar))

            # Calculate node ordering
            ord <- private$.calculateNodeOrdering(network_data)
            
            # Calculate node sizes
            node_sizes <- private$.calculateNodeSizes(network_data)
            
            # Calculate arc widths
            arc_widths <- private$.calculateArcWidths(network_data)
            
            # Get colors
            colors <- private$.getColors(network_data)

            # Create the plot with proper namespacing
            arcdiagram::arcplot(
                network_data$edgelist,
                ordering = ord,
                horizontal = self$options$horizontal,
                labels = network_data$vlabels,
                show.nodes = self$options$showNodes,
                col.nodes = colors$node_border,
                bg.nodes = colors$node_fill,
                cex.nodes = node_sizes,
                pch.nodes = 21,
                lwd.nodes = 1.5,
                col.arcs = colors$arc_color,
                lwd.arcs = arc_widths,
                cex.labels = self$options$labelSize
            )
            
            # Add title if specified
            if (self$options$plotTitle != "") {
                title(main = self$options$plotTitle, cex.main = 1.2)
            }
            
            # Add legend if groups are specified
            if (!is.null(network_data$groups) && self$options$showLegend) {
                private$.addLegend(colors)
            }
        },
        
        # Helper method to calculate node ordering
        .calculateNodeOrdering = function(network_data) {
            vlabels <- network_data$vlabels
            degrees <- network_data$degrees
            groups <- network_data$groups
            
            ord <- switch(self$options$sortNodes,
                "none" = seq_along(vlabels),
                "name" = order(vlabels, decreasing = self$options$sortDecreasing),
                "degree" = order(degrees, decreasing = self$options$sortDecreasing),
                "group" = {
                    if (!is.null(groups)) {
                        # Match groups to vertices
                        vertex_groups <- groups[match(vlabels, network_data$edgelist[,1])]
                        vertex_groups[is.na(vertex_groups)] <- groups[match(vlabels[is.na(vertex_groups)], 
                                                                          network_data$edgelist[,2])]
                        if (all(is.na(vertex_groups))) {
                            seq_along(vlabels)
                        } else {
                            order(vertex_groups, decreasing = self$options$sortDecreasing, na.last = TRUE)
                        }
                    } else {
                        seq_along(vlabels)
                    }
                }
            )
            
            return(ord)
        },
        
        # Helper method to calculate node sizes
        .calculateNodeSizes = function(network_data) {
            if (self$options$nodeSize == "fixed") {
                node_sizes <- rep(self$options$nodeSizeValue, length(network_data$vlabels))
            } else {
                # Size by degree centrality
                node_sizes <- log(network_data$degrees + 1) * self$options$nodeSizeValue / 2 + 0.5
                node_sizes <- pmax(node_sizes, 0.5)  # Minimum size
            }
            return(node_sizes)
        },
        
        # Helper method to calculate arc widths
        .calculateArcWidths = function(network_data) {
            if (self$options$arcWidth == "fixed") {
                arc_widths <- rep(self$options$arcWidthValue, length(network_data$weights))
            } else {
                # Scale by weight
                if (all(network_data$weights == network_data$weights[1])) {
                    # All weights are the same
                    arc_widths <- rep(self$options$arcWidthValue, length(network_data$weights))
                } else {
                    arc_widths <- scales::rescale(network_data$weights, 
                                                to = c(0.3, self$options$arcWidthValue * 2))
                }
            }
            return(arc_widths)
        },
        
        # Helper method to get color schemes
        .getColors = function(network_data) {
            groups <- network_data$groups
            vlabels <- network_data$vlabels
            
            if (!is.null(groups) && self$options$colorByGroup) {
                # Get unique groups
                unique_groups <- unique(groups)
                unique_groups <- unique_groups[!is.na(unique_groups)]
                n_groups <- length(unique_groups)
                
                # Get color palette
                if (n_groups <= 3) {
                    group_colors <- RColorBrewer::brewer.pal(max(3, n_groups), "Set1")[1:n_groups]
                } else if (n_groups <= 8) {
                    group_colors <- RColorBrewer::brewer.pal(n_groups, "Set1")
                } else {
                    group_colors <- rainbow(n_groups)
                }
                names(group_colors) <- unique_groups
                
                # Assign colors to vertices
                vertex_groups <- groups[match(vlabels, network_data$edgelist[,1])]
                vertex_groups[is.na(vertex_groups)] <- groups[match(vlabels[is.na(vertex_groups)], 
                                                                  network_data$edgelist[,2])]
                
                node_fill <- group_colors[vertex_groups]
                node_fill[is.na(node_fill)] <- "lightgray"
                node_border <- "black"
                
                # Arc colors with transparency
                arc_color <- hsv(0, 0, 0.2, self$options$arcTransparency)
                
            } else {
                # Default colors
                node_fill <- "lightblue"
                node_border <- "black"
                arc_color <- hsv(0, 0, 0.2, self$options$arcTransparency)
                group_colors <- NULL
            }
            
            return(list(
                node_fill = node_fill,
                node_border = node_border,
                arc_color = arc_color,
                group_colors = group_colors
            ))
        },
        
        # Helper method to add legend
        .addLegend = function(colors) {
            if (!is.null(colors$group_colors)) {
                legend("topright", 
                      legend = names(colors$group_colors),
                      fill = colors$group_colors,
                      cex = 0.8,
                      title = "Groups")
            }
        },
        
        # Helper method to generate network statistics
        .generateNetworkStats = function(network_data) {
            g <- network_data$g
            
            # Basic network metrics
            stats_text <- paste(
                "<h3>📊 Network Statistics</h3>",
                "<h4>Basic Metrics:</h4>",
                "<ul>",
                "<li><strong>Number of Nodes:</strong>", network_data$n_nodes, "</li>",
                "<li><strong>Number of Edges:</strong>", network_data$n_edges, "</li>",
                "<li><strong>Network Density:</strong>", round(network_data$density, 4), "</li>",
                "<li><strong>Is Connected:</strong>", ifelse(igraph::is_connected(g), "Yes", "No"), "</li>",
                "</ul>",
                sep = "\n"
            )
            
            # Centrality measures
            if (network_data$n_nodes > 1) {
                betweenness <- igraph::betweenness(g)
                closeness <- igraph::closeness(g)
                
                stats_text <- paste(stats_text,
                    "<h4>Centrality Measures:</h4>",
                    "<ul>",
                    "<li><strong>Highest Degree:</strong>", names(which.max(network_data$degrees)), 
                    " (", max(network_data$degrees), ")</li>",
                    "<li><strong>Highest Betweenness:</strong>", names(which.max(betweenness)), 
                    " (", round(max(betweenness), 2), ")</li>",
                    "</ul>",
                    sep = "\n"
                )
            }
            
            # Group statistics if available
            if (!is.null(network_data$groups)) {
                group_counts <- table(network_data$groups)
                stats_text <- paste(stats_text,
                    "<h4>Group Distribution:</h4>",
                    "<ul>",
                    paste("<li><strong>", names(group_counts), ":</strong>", group_counts, "</li>", collapse = ""),
                    "</ul>",
                    sep = "\n"
                )
            }
            
            self$results$networkStats$setContent(stats_text)
        }
    )
)
