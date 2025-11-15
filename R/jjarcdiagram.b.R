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
            
            # Set up assumptions panel conditionally
            private$.updateAssumptionsPanel()
        },

        # Run function
        .run = function() {
            
            # Check for required variables
            if (is.null(self$options$source) || is.null(self$options$target) || 
                length(self$options$source) == 0 || length(self$options$target) == 0) {
                todo <- glue::glue(
                    .("<br>Welcome to ClinicoPath Arc Diagram
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
                    <br><hr>")
                )
                self$results$todo$setContent(todo)
                # Clear report sentence when no variables selected
                self$results$reportSentence$setContent("")
                return()
            }


            if (is.null(self$data) || nrow(self$data) == 0)
                            return() 

            # Validate data
            if (nrow(self$data) == 0) {
                stop(.('Data contains no (complete) rows'))
            }
            
            private$.checkpoint()
            # Update assumptions panel when variables change
            private$.updateAssumptionsPanel()

            # Prepare and validate the network data
            private$.checkpoint()  # Before expensive network data processing
            network_data <- private$.prepareNetworkData()
            
            if (is.null(network_data)) {
                return()
            }
            
            # Generate network statistics
            if (self$options$showStats) {
                private$.checkpoint()  # Before statistical calculations
                private$.generateNetworkStats(network_data)
            }
            
            # Generate copy-ready report sentence with error handling
            tryCatch({
                private$.generateReportSentence(network_data)
            }, error = function(e) {
                warning(paste("Report generation failed:", e$message))
                # Set fallback content
                fallback_html <- paste(
                    .("<h3>📄 Analysis Summary</h3>"),
                    "<div style='background-color: #fff3cd; padding: 10px; border-left: 4px solid #ffc107;'>",
                    .("<p><strong>Status:</strong> Network analysis completed successfully.</p>"),
                    .("<p><strong>Note:</strong> Detailed summary generation encountered an issue. Basic network visualization is available above.</p>"),
                    "</div>",
                    sep = "\n"
                )
                self$results$reportSentence$setContent(fallback_html)
            })

            # Update todo with processing status
            todo <- glue::glue(
                .("<br>Network Analysis Complete
                <br>📊 Nodes: {network_data$n_nodes}
                <br>🔗 Edges: {network_data$n_edges}
                <br>📈 Density: {round(network_data$density, 3)}
                <br><hr>")
            )
            self$results$todo$setContent(todo)
        },

        # Plot function
        .plot = function(image, ggtheme, theme, ...) {
            # Check for required variables - don't attempt plot without them
            if (is.null(self$options$source) || is.null(self$options$target)  || 
                length(self$options$source) == 0 || length(self$options$target) == 0)
                return()

            # Check for data availability
            if (is.null(self$data) || nrow(self$data) == 0)
                return()  # Simply return without error when no data

            # Prepare network data
            private$.checkpoint()  # Before network data preparation in plot
            network_data <- private$.prepareNetworkData()
            if (is.null(network_data)) return()

            # Create the plot with proper error handling
            private$.checkpoint()  # Before expensive plot creation
            tryCatch({
                private$.createArcPlot(network_data)
            }, error = function(e) {
                self$results$todo$setContent(
                    paste(.("<br><b>Error creating plot:</b>"), e$message, "<br><hr>")
                )
                stop(e$message)
            })

            TRUE
        },
        
        # Helper method to create user instructions
        .createInstructions = function() {
            # Get preset-specific guidance
            preset_guidance <- private$.getPresetGuidance()
            
            instructions <- paste(
                .("<h3>🌐 Arc Diagram Network Visualization</h3>"),
                preset_guidance,
                
                .("<h4>📋 Getting Started:</h4>"),
                "<ul>",
                .("<li><strong>Source Node:</strong> Select the variable representing connection origins</li>"),
                .("<li><strong>Target Node:</strong> Select the variable representing connection destinations</li>"),
                .("<li><strong>Edge Weight:</strong> Optional numeric variable for connection strength</li>"),
                .("<li><strong>Node Groups:</strong> Optional categorical variable for color coding</li>"),
                "</ul>",
                
                .("<h4>🎨 Customization Options:</h4>"),
                "<ul>",
                .("<li><strong>Layout:</strong> Choose horizontal or vertical arrangement</li>"),
                .("<li><strong>Node Sizing:</strong> Fixed size or proportional to degree centrality</li>"),
                .("<li><strong>Node Sorting:</strong> Arrange by name, degree, or group</li>"),
                .("<li><strong>Arc Styling:</strong> Fixed width or proportional to weights</li>"),
                .("<li><strong>Color Schemes:</strong> Multiple palettes for different node groups</li>"),
                "</ul>",
                
                .("<h4>📊 Network Metrics:</h4>"),
                "<ul>",
                .("<li><strong>Density:</strong> Measure of network connectivity</li>"),
                .("<li><strong>Centrality:</strong> Node importance in the network</li>"),
                .("<li><strong>Communities:</strong> Group structure detection</li>"),
                "</ul>",
                
                .("<p><em>💡 Tip: Arc diagrams are ideal for visualizing hierarchical or sequential relationships with minimal visual clutter.</em></p>"),
                sep = "\n"
            )
            return(instructions)
        },
        
        # Helper method to get preset-specific guidance
        .getPresetGuidance = function() {
            preset <- self$options$analysisPreset
            
            guidance <- switch(preset,
                "gene_interaction" = paste(
                    "<div style='background-color: #e8f5e8; padding: 10px; margin: 10px 0; border-left: 4px solid #4CAF50;'>",
                    .("<h4>🧬 Gene Interaction Network Analysis</h4>"),
                    .("<p><strong>Optimized for:</strong> Gene regulatory relationships, protein interactions, pathway analysis</p>"),
                    .("<p><strong>Recommendations:</strong> Use gene symbols as nodes, interaction scores as weights, pathways/functions as groups</p>"),
                    .("<p><strong>Suggested Settings:</strong> Sort by group, enable statistics, use degree-based node sizing</p>"),
                    "</div>",
                    sep = "\n"
                ),
                "patient_network" = paste(
                    "<div style='background-color: #e3f2fd; padding: 10px; margin: 10px 0; border-left: 4px solid #2196F3;'>",
                    .("<h4>👥 Patient Similarity Network Analysis</h4>"),
                    .("<p><strong>Optimized for:</strong> Patient similarity, treatment response, clinical outcomes</p>"),
                    .("<p><strong>Recommendations:</strong> Use patient IDs as nodes, similarity scores as weights, clinical subtypes as groups</p>"),
                    .("<p><strong>Suggested Settings:</strong> Sort by group, enable statistics, use horizontal layout</p>"),
                    "</div>",
                    sep = "\n"
                ),
                "pathway_network" = paste(
                    "<div style='background-color: #fff3e0; padding: 10px; margin: 10px 0; border-left: 4px solid #ff9800;'>",
                    .("<h4>🔬 Biological Pathway Network Analysis</h4>"),
                    .("<p><strong>Optimized for:</strong> Pathway interactions, biological processes, functional modules</p>"),
                    .("<p><strong>Recommendations:</strong> Use pathway names as nodes, interaction strength as weights, functional categories as groups</p>"),
                    .("<p><strong>Suggested Settings:</strong> Sort by degree, enable statistics, use weight-based arc widths</p>"),
                    "</div>",
                    sep = "\n"
                ),
                "comorbidity_network" = paste(
                    "<div style='background-color: #fce4ec; padding: 10px; margin: 10px 0; border-left: 4px solid #e91e63;'>",
                    .("<h4>🏥 Disease Co-occurrence Network Analysis</h4>"),
                    .("<p><strong>Optimized for:</strong> Disease associations, comorbidity patterns, epidemiological analysis</p>"),
                    .("<p><strong>Recommendations:</strong> Use disease codes/names as nodes, co-occurrence frequency as weights, disease categories as groups</p>"),
                    .("<p><strong>Suggested Settings:</strong> Sort by degree, enable statistics, use weight-based arc widths</p>"),
                    "</div>",
                    sep = "\n"
                ),
                "" # Default: no specific guidance
            )
            
            return(guidance)
        },
        
        # Helper method to update assumptions panel conditionally
        .updateAssumptionsPanel = function() {
            if (!is.null(self$options$source) && !is.null(self$options$target) || 
                length(self$options$source) == 0 || length(self$options$target) == 0
            ) {
                # Configure preset-specific settings
                private$.configurePresets()
                
                # Set up assumptions and guidelines
                assumptions <- private$.createAssumptions()
                self$results$assumptions$setContent(assumptions)
            } else {
                # Clear assumptions panel when no variables selected
                self$results$assumptions$setContent("")
            }
        },
        
        # Helper method to configure analysis presets
        .configurePresets = function() {
            preset <- self$options$analysisPreset

            # Apply preset-specific optimizations
            switch(preset,
                "gene_interaction" = {
                    if (is.null(self$options$plotTitle) || self$options$plotTitle == "") {
                        self$options$plotTitle <- "Gene Interaction Network"
                    }
                },
                "patient_network" = {
                    self$options$horizontal <- TRUE
                },
                "pathway_network" = {
                    self$options$sortNodes <- "degree"
                },
                "comorbidity_network" = {
                    self$options$sortNodes <- "degree"
                }
            )
        },
        
        # Helper method to validate network structure
        .validateNetworkStructure = function(edgelist, network_data) {
            warnings <- c()
            
            # Check for self-loops
            self_loops <- sum(edgelist[,1] == edgelist[,2])
            if (self_loops > 0) {
                warnings <- c(warnings, sprintf(.("Warning: %d self-loop(s) detected and will be ignored"), self_loops))
                edgelist <- edgelist[edgelist[,1] != edgelist[,2], , drop = FALSE]
            }
            
            # Check for small networks
            if (network_data$n_nodes < 10) {
                warnings <- c(warnings, .("Warning: Small networks (< 10 nodes) may not provide reliable centrality measures"))
            }
            
            # Check for very large networks
            if (nrow(edgelist) > 10000) {
                warnings <- c(warnings, .("Note: Large network detected. Analysis may take longer"))
            }
            
            # Memory usage warning for massive datasets
            if (nrow(edgelist) > 50000) {
                stop(.("Network too large (> 50,000 edges). Consider filtering or sampling data first"))
            }
            
            return(list(edgelist = edgelist, warnings = warnings))
        },
        
        # Helper method to prepare network data
        .prepareNetworkData = function() {
            mydata <- self$data

            if (nrow(mydata) == 0) {
                self$results$todo$setContent(.("<br><b>Error:</b> No data available<br><hr>"))
                return(NULL)
            }

            # Get variable names
            source_var <- self$options$source
            target_var <- self$options$target
            weight_var <- self$options$weight
            group_var <- self$options$group

            # Validate required columns exist
            if (!source_var %in% names(mydata) || !target_var %in% names(mydata)) {
                self$results$todo$setContent(.("<br><b>Error:</b> Source or target variables not found<br><hr>"))
                return(NULL)
            }

            # Build list of columns to check for NAs (only those actually used)
            required_cols <- c(source_var, target_var)
            if (!is.null(weight_var) && weight_var %in% names(mydata)) {
                required_cols <- c(required_cols, weight_var)
            }
            if (!is.null(group_var) && group_var %in% names(mydata)) {
                required_cols <- c(required_cols, group_var)
            }

            # SELECTIVE NA OMISSION: Only drop rows with NAs in required columns
            complete_rows <- complete.cases(mydata[required_cols])
            mydata_clean <- mydata[complete_rows, , drop = FALSE]

            if (nrow(mydata_clean) == 0) {
                self$results$todo$setContent(.("<br><b>Error:</b> No complete rows in source/target/weight/group columns<br><hr>"))
                return(NULL)
            }

            # Create edge list matrix
            edgelist <- as.matrix(mydata_clean[c(source_var, target_var)])

            if (nrow(edgelist) == 0) {
                self$results$todo$setContent(.("<br><b>Error:</b> No complete edge data available<br><hr>"))
                return(NULL)
            }

            # Calculate initial network metrics for validation
            vlabels <- unique(c(edgelist[,1], edgelist[,2]))
            n_nodes <- length(vlabels)
            n_edges <- nrow(edgelist)

            # Validate network structure
            validation <- private$.validateNetworkStructure(edgelist, list(n_nodes = n_nodes, n_edges = n_edges))
            edgelist <- validation$edgelist

            # Update edge count after validation
            n_edges <- nrow(edgelist)

            # Display warnings if any
            if (length(validation$warnings) > 0) {
                warning_text <- paste(validation$warnings, collapse = "<br>")
                self$results$todo$setContent(paste0("<br>", warning_text, "<br><hr>"))
            }

            # Get edge weights if specified
            if (!is.null(weight_var) && weight_var %in% names(mydata_clean)) {
                weights <- as.numeric(mydata_clean[[weight_var]])
                # If any weights are still NA after filtering, replace with 1
                weights[is.na(weights)] <- 1
            } else {
                weights <- rep(1, nrow(edgelist))
            }

            # NODE-LEVEL GROUP HANDLING: Build proper node-to-group mapping
            # Groups should be node attributes, not edge attributes
            node_groups <- NULL
            if (!is.null(group_var) && group_var %in% names(mydata_clean)) {
                # Create a mapping from node labels to groups
                # For each node, find its group by looking at source or target columns
                node_groups <- setNames(rep(NA, length(vlabels)), vlabels)

                for (i in seq_along(vlabels)) {
                    node <- vlabels[i]
                    # Look for this node in source column
                    source_match <- which(mydata_clean[[source_var]] == node)
                    if (length(source_match) > 0) {
                        # Use the first occurrence's group
                        node_groups[node] <- as.character(mydata_clean[[group_var]][source_match[1]])
                    } else {
                        # Look in target column
                        target_match <- which(mydata_clean[[target_var]] == node)
                        if (length(target_match) > 0) {
                            node_groups[node] <- as.character(mydata_clean[[group_var]][target_match[1]])
                        }
                    }
                }
            }

            # Create igraph object for analysis
            private$.checkpoint(flush = FALSE)  # Before igraph creation
            g <- tryCatch({
                igraph::graph_from_edgelist(edgelist, directed = self$options$directed)
            }, error = function(e) {
                self$results$todo$setContent(.("<br><b>Error creating network:</b> Invalid edge structure<br><hr>"))
                return(NULL)
            })

            if (is.null(g)) return(NULL)

            # INJECT EDGE WEIGHTS into igraph object for centrality calculations
            igraph::E(g)$weight <- weights

            # INJECT NODE GROUPS into igraph object as vertex attribute
            if (!is.null(node_groups)) {
                igraph::V(g)$group <- node_groups[igraph::V(g)$name]
            }

            # Get vertex attributes with WEIGHTED degree if weights provided
            private$.checkpoint(flush = FALSE)  # Before degree calculation
            if (!is.null(weight_var) && weight_var %in% names(mydata_clean)) {
                # Use weighted degree (strength)
                degrees <- igraph::strength(g, weights = igraph::E(g)$weight)
            } else {
                # Use unweighted degree
                degrees <- igraph::degree(g)
            }

            # Calculate network density
            private$.checkpoint(flush = FALSE)  # Before density calculation
            density <- igraph::edge_density(g)

            return(list(
                edgelist = edgelist,
                weights = weights,
                node_groups = node_groups,  # Now node-level, not edge-level
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
            private$.checkpoint(flush = FALSE)  # Before node calculations
            ord <- private$.calculateNodeOrdering(network_data)
            
            # Calculate node sizes
            node_sizes <- private$.calculateNodeSizes(network_data)
            
            # Calculate arc widths
            arc_widths <- private$.calculateArcWidths(network_data)
            
            # Get colors
            colors <- private$.getColors(network_data)

            # Create the plot with proper namespacing
            private$.checkpoint(flush = FALSE)  # Before arcplot rendering
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
            
            # Add legend if groups are specified and color by group is enabled
            if (!is.null(network_data$groups) && self$options$colorByGroup && self$options$showLegend) {
                private$.addLegend(colors)
            }
        },
        
        # Helper method to calculate node ordering
        .calculateNodeOrdering = function(network_data) {
            vlabels <- network_data$vlabels
            degrees <- network_data$degrees
            node_groups <- network_data$node_groups  # Now using node-level groups

            ord <- switch(self$options$sortNodes,
                "none" = seq_along(vlabels),
                "name" = order(vlabels, decreasing = self$options$sortDecreasing),
                "degree" = order(degrees, decreasing = self$options$sortDecreasing),
                "group" = {
                    if (!is.null(node_groups)) {
                        # node_groups is already a named vector mapping node labels to groups
                        if (all(is.na(node_groups))) {
                            seq_along(vlabels)
                        } else {
                            order(node_groups, decreasing = self$options$sortDecreasing, na.last = TRUE)
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
            node_groups <- network_data$node_groups  # Now using node-level groups
            vlabels <- network_data$vlabels

            if (!is.null(node_groups) && self$options$colorByGroup) {
                # Get unique groups (node-level)
                unique_groups <- unique(node_groups)
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

                # Assign colors to vertices using node_groups directly
                # node_groups is already a named vector mapping node labels to groups
                node_fill <- group_colors[node_groups]
                node_fill[is.na(node_fill)] <- "lightgray"
                node_border <- "black"

                # Arc colors: Color edges based on SOURCE node's group
                edgelist <- network_data$edgelist
                source_groups <- node_groups[edgelist[, 1]]
                arc_colors <- group_colors[source_groups]
                arc_colors[is.na(arc_colors)] <- "gray50"

                # Apply transparency to arc colors
                arc_color <- adjustcolor(arc_colors, alpha.f = self$options$arcTransparency)

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
                      title = .("Groups"))
            }
        },
        
        # Helper method to generate network statistics
        .generateNetworkStats = function(network_data) {
            g <- network_data$g
            
            # Basic network metrics with clinical interpretations
            density_interp <- ifelse(network_data$density > 0.5, .("highly"), ifelse(network_data$density > 0.2, .("moderately"), .("sparsely")))
            connectivity_interp <- ifelse(igraph::is_connected(g), .("fully connected network"), .("network with isolated components"))
            
            stats_text <- paste(
                .("<h3>📊 Network Statistics</h3>"),
                .("<h4>Basic Metrics:</h4>"),
                "<ul>",
                paste(.("<li><strong>Number of Nodes:</strong>"), network_data$n_nodes, "</li>"),
                paste(.("<li><strong>Number of Edges:</strong>"), network_data$n_edges, "</li>"),
                paste(.("<li><strong>Network Density:</strong>"), round(network_data$density, 4), sprintf(.("(%s connected)"), density_interp), "</li>"),
                paste(.("<li><strong>Is Connected:</strong>"), ifelse(igraph::is_connected(g), .("Yes"), .("No")), sprintf(.("(%s)"), connectivity_interp), "</li>"),
                "</ul>",
                sep = "\n"
            )
            
            # Add clinical interpretation summary
            clinical_summary <- paste(
                .("<h4>📋 Clinical Interpretation:</h4>"),
                "<div style='background-color: #f0f8ff; padding: 10px; margin: 10px 0; border-left: 4px solid #4CAF50;'>",
                sprintf(.("<p><strong>Network Overview:</strong> This network contains %d entities with %d relationships, showing %s connectivity (density = %.3f).</p>"), 
                        network_data$n_nodes, network_data$n_edges, density_interp, network_data$density),
                ifelse(network_data$density < 0.1, 
                       .("<p><strong>💡 Insight:</strong> Sparse networks may indicate specialized or selective relationships between entities.</p>"),
                       ifelse(network_data$density > 0.5,
                              .("<p><strong>💡 Insight:</strong> Dense networks suggest strong interconnectedness, possibly indicating shared pathways or common mechanisms.</p>"),
                              .("<p><strong>💡 Insight:</strong> Moderate connectivity suggests a balanced network structure with both specialized and shared relationships.</p>"))),
                "</div>",
                sep = "\n"
            )
            
            stats_text <- paste(stats_text, clinical_summary, sep = "\n")
            
            # Centrality measures with clinical context
            if (network_data$n_nodes > 1) {
                private$.checkpoint(flush = FALSE)  # Before expensive centrality calculations

                # WEIGHTED CENTRALITY: Use edge weights if available
                if (!is.null(igraph::E(g)$weight) && any(igraph::E(g)$weight != 1)) {
                    # Network has meaningful weights - use weighted centrality
                    betweenness <- igraph::betweenness(g, weights = igraph::E(g)$weight)
                    closeness <- igraph::closeness(g, weights = igraph::E(g)$weight)
                    degree_label <- .("connections/strength")
                } else {
                    # Unweighted network
                    betweenness <- igraph::betweenness(g)
                    closeness <- igraph::closeness(g)
                    degree_label <- .("connections")
                }

                # Get key nodes for clinical interpretation
                highest_degree_node <- names(which.max(network_data$degrees))
                highest_betweenness_node <- names(which.max(betweenness))
                max_degree <- max(network_data$degrees)
                max_betweenness <- max(betweenness)

                stats_text <- paste(stats_text,
                    .("<h4>Centrality Measures:</h4>"),
                    "<ul>",
                    paste(.("<li><strong>Highest Degree:</strong>"), highest_degree_node, " (", round(max_degree, 2), " ", degree_label, ")</li>"),
                    paste(.("<li><strong>Highest Betweenness:</strong>"), highest_betweenness_node, " (", round(max_betweenness, 2), ")</li>"),
                    "</ul>",
                    sep = "\n"
                )

                # Add centrality interpretation
                centrality_interp <- paste(
                    "<div style='background-color: #fff5f5; padding: 8px; margin: 8px 0; border-left: 4px solid #FF6B6B;'>",
                    sprintf(.("<p><strong>🎯 Key Players:</strong> '%s' is the most connected entity (%.2f %s), suggesting it may be a hub or central player.</p>"),
                            highest_degree_node, max_degree, degree_label),
                    if (highest_betweenness_node != highest_degree_node) {
                        sprintf(.("<p><strong>🌉 Bridge Entity:</strong> '%s' has the highest betweenness centrality, indicating it serves as an important bridge between different network regions.</p>"),
                                highest_betweenness_node)
                    } else {
                        .("<p><strong>🌟 Central Hub:</strong> The same entity serves as both the most connected node and the main bridge in the network.</p>")
                    },
                    "</div>",
                    sep = "\n"
                )

                stats_text <- paste(stats_text, centrality_interp, sep = "\n")
            }

            # NODE-LEVEL Group statistics if available
            if (!is.null(network_data$node_groups)) {
                # Count nodes per group (not edges)
                group_counts <- table(network_data$node_groups, useNA = "no")
                stats_text <- paste(stats_text,
                    .("<h4>Group Distribution (Node Counts):</h4>"),
                    "<ul>",
                    paste("<li><strong>", names(group_counts), ":</strong>", group_counts, .(" nodes"), "</li>", collapse = ""),
                    "</ul>",
                    sep = "\n"
                )
            }
            
            self$results$networkStats$setContent(stats_text)
        },
        
        # Helper method to create assumptions and guidelines
        .createAssumptions = function() {
            assumptions <- paste(
                .("<h3>📋 Network Analysis Assumptions & Guidelines</h3>"),
                "<div style='background-color: #f9f9f9; padding: 15px; margin: 10px 0; border-radius: 5px;'>",
                
                .("<h4>📊 Data Requirements:</h4>"),
                "<ul style='margin-left: 20px;'>",
                .("<li><strong>Data Format:</strong> Each row represents one relationship/connection</li>"),
                .("<li><strong>Node Variables:</strong> Source and Target must be categorical (factors)</li>"),
                .("<li><strong>Edge Weights:</strong> Optional numeric values for connection strength</li>"),
                .("<li><strong>Missing Data:</strong> Rows with missing source/target values are automatically excluded</li>"),
                "</ul>",
                
                .("<h4>🎯 Network Analysis Guidelines:</h4>"),
                "<ul style='margin-left: 20px;'>",
                .("<li><strong>Network Size:</strong> Optimal range is 10-1000 nodes for meaningful analysis</li>"),
                .("<li><strong>Self-loops:</strong> Connections from a node to itself are detected and removed</li>"),
                .("<li><strong>Multiple Edges:</strong> Duplicate connections are preserved but may be aggregated</li>"),
                .("<li><strong>Isolated Nodes:</strong> Entities with no connections appear only if referenced</li>"),
                "</ul>",
                
                .("<h4>📈 Interpretation Notes:</h4>"),
                "<ul style='margin-left: 20px;'>",
                .("<li><strong>Network Density:</strong> 0-0.2 (sparse), 0.2-0.5 (moderate), >0.5 (dense)</li>"),
                .("<li><strong>Degree Centrality:</strong> Number of direct connections (hub identification)</li>"),
                .("<li><strong>Betweenness Centrality:</strong> Importance as a bridge between network regions</li>"),
                .("<li><strong>Connected Components:</strong> Isolated subnetworks may indicate distinct functional modules</li>"),
                "</ul>",
                
                .("<h4>⚠️ Common Pitfalls:</h4>"),
                "<ul style='margin-left: 20px;'>",
                .("<li><strong>Small Networks:</strong> Centrality measures less reliable with < 10 nodes</li>"),
                .("<li><strong>Threshold Effects:</strong> Edge weight filtering can dramatically alter network structure</li>"),
                .("<li><strong>Directed Networks:</strong> When 'Directed Network' is enabled, statistics respect edge direction but the arc diagram visualization does NOT display arrows. Consider this a limitation of the arc plot style.</li>"),
                .("<li><strong>Clinical Relevance:</strong> High connectivity doesn't always imply biological significance</li>"),
                "</ul>",
                
                "</div>",
                sep = "\n"
            )
            return(assumptions)
        },
        
        # Helper method to generate copy-ready report sentence
        .generateReportSentence = function(network_data) {
            # Get analysis type for context
            analysis_type <- switch(self$options$analysisPreset,
                "gene_interaction" = .("gene interaction"),
                "patient_network" = .("patient similarity"),
                "pathway_network" = .("biological pathway"),
                "comorbidity_network" = .("disease co-occurrence"),
                .("network")
            )
            
            # Generate density interpretation
            density_desc <- ifelse(network_data$density > 0.5, .("highly connected"), 
                                 ifelse(network_data$density > 0.2, .("moderately connected"), .("sparsely connected")))
            
            # Create the copy-ready sentence
            report_html <- paste(
                .("<h3>📄 Copy-Ready Analysis Summary</h3>"),
                "<div style='background-color: #e8f5e8; padding: 15px; margin: 10px 0; border-radius: 5px; border-left: 4px solid #4CAF50;'>",
                
                sprintf(.("<p><strong>Network Summary:</strong> Arc diagram analysis of a %s network revealed %d entities connected by %d relationships, with a network density of %.3f indicating %s structure.</p>"),
                        analysis_type, network_data$n_nodes, network_data$n_edges, network_data$density, density_desc),
                
                if (network_data$n_nodes > 1 && self$options$showStats) {
                    # Get centrality information if available
                    highest_degree_node <- names(which.max(network_data$degrees))
                    max_degree <- max(network_data$degrees)
                    # Use appropriate label for weighted vs unweighted
                    if (!is.null(igraph::E(network_data$g)$weight) && any(igraph::E(network_data$g)$weight != 1)) {
                        sprintf(.("<p><strong>Key Findings:</strong> The entity '%s' emerged as the most highly connected hub with %.2f weighted connections, suggesting its central role in the network.</p>"),
                                highest_degree_node, max_degree)
                    } else {
                        sprintf(.("<p><strong>Key Findings:</strong> The entity '%s' emerged as the most highly connected hub with %.0f direct connections, suggesting its central role in the network.</p>"),
                                highest_degree_node, max_degree)
                    }
                } else {
                    ""
                },

                if (!is.null(network_data$node_groups)) {
                    # Count unique NODE groups (not edge groups)
                    group_count <- length(unique(network_data$node_groups[!is.na(network_data$node_groups)]))
                    sprintf(.("<p><strong>Grouping:</strong> %d nodes were categorized into %d distinct groups, with color-coding revealing potential functional clusters or classifications.</p>"),
                            sum(!is.na(network_data$node_groups)), group_count)
                } else {
                    ""
                },
                
                .("<p><em>💡 Tip: Copy the text above for use in reports, presentations, or publications.</em></p>"),
                "</div>",
                sep = "\n"
            )
            
            self$results$reportSentence$setContent(report_html)
        }
    )
)
