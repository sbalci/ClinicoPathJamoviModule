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
            
            # Generate copy-ready report sentence with error handling (visible only when showSummary=TRUE)
            if (self$options$showSummary) {
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
            }

            # Generate glossary if requested (visible only when showGlossary=TRUE)
            if (self$options$showGlossary) {
                private$.generateGlossary()
            }

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
                notice <- jmvcore::Notice$new(
                    options = self$options,
                    name = 'plotCreationError',
                    type = jmvcore::NoticeType$ERROR
                )
                notice$setContent(paste('Error creating plot:', e$message))
                self$results$insert(1, notice)
                stop(e$message)
            })

            TRUE
        },

        # Helper method to escape variable names for safe igraph handling
        .escapeVar = function(x) {
            x_char <- as.character(x)
            safe <- make.names(x_char, unique = TRUE)
            safe <- gsub("\\.", "_", safe)
            return(safe)
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
        
        # Helper method to prepare network data
        .prepareNetworkData = function() {
            mydata <- self$data

            if (nrow(mydata) == 0) {
                notice <- jmvcore::Notice$new(
                    options = self$options,
                    name = 'noDataAvailable',
                    type = jmvcore::NoticeType$ERROR
                )
                notice$setContent('No data available. Please load a dataset and re-run the analysis.')
                self$results$insert(1, notice)
                return(NULL)
            }

            # Get variable names
            source_var <- self$options$source
            target_var <- self$options$target
            weight_var <- self$options$weight
            group_var <- self$options$group

            # Validate required columns exist
            if (!source_var %in% names(mydata) || !target_var %in% names(mydata)) {
                notice <- jmvcore::Notice$new(
                    options = self$options,
                    name = 'variablesNotFound',
                    type = jmvcore::NoticeType$ERROR
                )
                notice$setContent('Source or target variables not found in the dataset.')
                self$results$insert(1, notice)
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
                notice <- jmvcore::Notice$new(
                    options = self$options,
                    name = 'noCompleteRows',
                    type = jmvcore::NoticeType$ERROR
                )
                notice$setContent('No complete rows after removing missing values. Check your data for NA values in source, target, weight, or group variables.')
                self$results$insert(1, notice)
                return(NULL)
            }

            # Create edge data frame with escaped names + original labels
            edge_df <- data.frame(
                source = private$.escapeVar(mydata_clean[[source_var]]),
                target = private$.escapeVar(mydata_clean[[target_var]]),
                source_label = as.character(mydata_clean[[source_var]]),
                target_label = as.character(mydata_clean[[target_var]]),
                stringsAsFactors = FALSE
            )

            # Add weights if specified
            if (!is.null(weight_var) && weight_var %in% names(mydata_clean)) {
                edge_df$weight <- as.numeric(mydata_clean[[weight_var]])
                edge_df$weight[is.na(edge_df$weight)] <- 1
            } else {
                edge_df$weight <- 1
            }

            if (nrow(edge_df) == 0) {
                notice <- jmvcore::Notice$new(
                    options = self$options,
                    name = 'noEdgeData',
                    type = jmvcore::NoticeType$ERROR
                )
                notice$setContent('No valid edge data available after processing. Ensure source and target variables have overlapping values.')
                self$results$insert(1, notice)
                return(NULL)
            }

            # Calculate initial metrics before aggregation
            vlabels_initial <- unique(c(edge_df$source, edge_df$target))
            vlabels_display <- unique(c(edge_df$source_label, edge_df$target_label))
            n_edges_raw <- nrow(edge_df)

            # Remove self-loops before aggregation
            self_loops <- sum(edge_df$source == edge_df$target)
            if (self_loops > 0) {
                edge_df <- edge_df[edge_df$source != edge_df$target, , drop = FALSE]
            }

            # AGGREGATE DUPLICATE EDGES if option enabled
            aggregation_warnings <- c()
            if (self$options$aggregateEdges) {
                # Count duplicates before aggregation
                edge_pairs <- paste(edge_df$source, edge_df$target, sep = "->")
                if (!self$options$directed) {
                    # For undirected, normalize pairs (A-B same as B-A)
                    edge_pairs_sorted <- apply(edge_df[, c("source", "target")], 1, function(x) paste(sort(x), collapse = "->"))
                    duplicates <- sum(duplicated(edge_pairs_sorted))
                } else {
                    duplicates <- sum(duplicated(edge_pairs))
                }

                if (duplicates > 0) {
                    aggregation_warnings <- c(aggregation_warnings,
                        sprintf(.("Note: %d duplicate edge(s) aggregated by summing weights"), duplicates))
                }

                # Aggregate by summing weights for duplicate edges
                if (!self$options$directed) {
                    # For undirected networks, treat A->B and B->A as the same edge
                    edge_df$edge_key <- apply(edge_df[, c("source", "target")], 1, function(x) paste(sort(x), collapse = "~~~"))
                    edge_agg <- aggregate(weight ~ edge_key, data = edge_df, FUN = sum)

                    # Split edge_key back into source and target
                    edge_split <- strsplit(edge_agg$edge_key, "~~~")
                    edge_df <- data.frame(
                        source = sapply(edge_split, `[`, 1),
                        target = sapply(edge_split, `[`, 2),
                        weight = edge_agg$weight,
                        stringsAsFactors = FALSE
                    )
                } else {
                    # For directed networks, only aggregate identical source->target pairs
                    edge_agg <- aggregate(weight ~ source + target, data = edge_df, FUN = sum)
                    edge_df <- edge_agg
                }
            } else {
                # Warn if there are duplicates but aggregation is disabled
                edge_pairs <- paste(edge_df$source, edge_df$target, sep = "->")
                duplicates <- sum(duplicated(edge_pairs))
                if (duplicates > 0) {
                    aggregation_warnings <- c(aggregation_warnings,
                        sprintf(.("Warning: %d duplicate edge(s) detected but not aggregated. Network metrics may be inflated. Enable 'Aggregate Duplicate Edges' for accurate statistics."), duplicates))
                }
            }

            # Create edge list matrix from aggregated data
            edgelist <- as.matrix(edge_df[, c("source", "target")])
            weights <- edge_df$weight

            # Calculate network metrics after aggregation
            vlabels <- unique(c(edgelist[,1], edgelist[,2]))
            n_nodes <- length(vlabels)
            n_edges <- nrow(edgelist)

            # Display self-loops warning as Notice
            if (self_loops > 0) {
                notice <- jmvcore::Notice$new(
                    options = self$options,
                    name = 'selfLoopsDetected',
                    type = jmvcore::NoticeType$WARNING
                )
                notice$setContent(sprintf('%d self-loop(s) detected and removed from the network.', self_loops))
                self$results$insert(2, notice)
            }

            # Display aggregation warnings as Notices
            if (length(aggregation_warnings) > 0) {
                for (i in seq_along(aggregation_warnings)) {
                    notice <- jmvcore::Notice$new(
                        options = self$options,
                        name = paste0('aggregationWarning', i),
                        type = jmvcore::NoticeType$WARNING
                    )
                    # Remove the "Warning: " prefix from the message since NoticeType already indicates it's a warning
                    msg <- sub("^Warning: ", "", aggregation_warnings[i])
                    notice$setContent(msg)
                    self$results$insert(2, notice)
                }
            }

            # Check for small/large networks with tiered warnings
            if (n_nodes < 3) {
                notice <- jmvcore::Notice$new(
                    options = self$options,
                    name = 'trivialNetwork',
                    type = jmvcore::NoticeType$STRONG_WARNING
                )
                notice$setContent('Network has fewer than 3 nodes. Centrality measures are not meaningful for networks this small. Consider adding more entities or combining data.')
                self$results$insert(1, notice)
            } else if (n_nodes < 5) {
                notice <- jmvcore::Notice$new(
                    options = self$options,
                    name = 'verySmallNetwork',
                    type = jmvcore::NoticeType$STRONG_WARNING
                )
                notice$setContent('Network has fewer than 5 nodes. Centrality measures may be unreliable. Interpret with caution and consider qualitative assessment instead of quantitative metrics.')
                self$results$insert(1, notice)
            } else if (n_nodes < 10) {
                notice <- jmvcore::Notice$new(
                    options = self$options,
                    name = 'smallNetwork',
                    type = jmvcore::NoticeType$WARNING
                )
                notice$setContent('Small network (< 10 nodes). Centrality measures may show limited variation. Results should be interpreted cautiously.')
                self$results$insert(2, notice)
            }

            if (n_edges > 10000) {
                notice <- jmvcore::Notice$new(
                    options = self$options,
                    name = 'largeNetwork',
                    type = jmvcore::NoticeType$INFO
                )
                notice$setContent('Large network detected. Analysis may take longer.')
                self$results$insert(999, notice)
            }

            if (n_edges > 50000) {
                notice <- jmvcore::Notice$new(
                    options = self$options,
                    name = 'networkTooLarge',
                    type = jmvcore::NoticeType$ERROR
                )
                notice$setContent('Network too large (> 50,000 edges after aggregation). Consider filtering or sampling data first.')
                self$results$insert(1, notice)
                stop('Network too large (> 50,000 edges after aggregation). Consider filtering or sampling data first.')
            }

            # NODE-LEVEL GROUP HANDLING: Build proper node-to-group mapping with conflict detection
            # Groups should be node attributes, not edge-level
            # Create reverse mapping: escaped names -> original labels
            label_to_escaped <- setNames(vlabels, vlabels_display)
            escaped_to_label <- setNames(vlabels_display, vlabels)

            node_groups <- NULL
            group_conflicts <- c()
            if (!is.null(group_var) && group_var %in% names(mydata_clean)) {
                # Create a mapping from ESCAPED node labels to groups
                node_groups <- setNames(rep(NA, length(vlabels)), vlabels)

                for (i in seq_along(vlabels)) {
                    node_escaped <- vlabels[i]
                    node_original <- escaped_to_label[node_escaped]
                    # Look for this node's ORIGINAL name in both source and target columns
                    source_match <- which(as.character(mydata_clean[[source_var]]) == node_original)
                    target_match <- which(as.character(mydata_clean[[target_var]]) == node_original)
                    all_matches <- c(source_match, target_match)

                    if (length(all_matches) > 0) {
                        # Get all unique groups for this node
                        groups_for_node <- unique(as.character(mydata_clean[[group_var]][all_matches]))
                        groups_for_node <- groups_for_node[!is.na(groups_for_node)]

                        if (length(groups_for_node) > 1) {
                            # CONFLICT DETECTED: Node appears with multiple groups
                            group_conflicts <- c(group_conflicts,
                                sprintf(.("Warning: Node '%s' has conflicting groups (%s). Using first occurrence: '%s'"),
                                    node_original, paste(groups_for_node, collapse = ", "), groups_for_node[1]))
                            node_groups[node_escaped] <- groups_for_node[1]
                        } else if (length(groups_for_node) == 1) {
                            node_groups[node_escaped] <- groups_for_node[1]
                        }
                    }
                }

                # Display group conflict warnings as Notices
                if (length(group_conflicts) > 0) {
                    for (i in seq_along(group_conflicts)) {
                        notice <- jmvcore::Notice$new(
                            options = self$options,
                            name = paste0('groupConflict', i),
                            type = jmvcore::NoticeType$WARNING
                        )
                        # Remove the "Warning: " prefix from the message
                        msg <- sub("^Warning: ", "", group_conflicts[i])
                        notice$setContent(msg)
                        self$results$insert(2, notice)
                    }
                }
            }

            # Create igraph object for analysis
            private$.checkpoint(flush = FALSE)  # Before igraph creation
            g <- tryCatch({
                igraph::graph_from_edgelist(edgelist, directed = self$options$directed)
            }, error = function(e) {
                notice <- jmvcore::Notice$new(
                    options = self$options,
                    name = 'invalidEdgeStructure',
                    type = jmvcore::NoticeType$ERROR
                )
                notice$setContent('Invalid edge structure detected. Check for duplicate or self-referencing edges.')
                self$results$insert(1, notice)
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

            # Domain-specific network density warnings
            if (density > 0.7) {
                notice <- jmvcore::Notice$new(
                    options = self$options,
                    name = 'highNetworkDensity',
                    type = jmvcore::NoticeType$WARNING
                )
                notice$setContent('Network density is very high (>70%). Consider filtering weak connections or using different visualization methods for clearer interpretation.')
                self$results$insert(2, notice)
            }

            # Info for sparse networks (helpful guidance, not a problem)
            if (density < 0.05 && n_nodes > 20) {
                notice <- jmvcore::Notice$new(
                    options = self$options,
                    name = 'sparseNetwork',
                    type = jmvcore::NoticeType$INFO
                )
                notice$setContent('Network is sparse (density <5%). Arc diagram is well-suited for visualizing sparse network structures.')
                self$results$insert(999, notice)
            }

            return(list(
                edgelist = edgelist,
                weights = weights,
                node_groups = node_groups,
                g = g,
                vlabels = vlabels,              # Escaped names for igraph
                vlabels_display = vlabels_display,  # Original for plotting
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
                labels = network_data$vlabels_display,
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
            vlabels_display <- network_data$vlabels_display
            degrees <- network_data$degrees
            node_groups <- network_data$node_groups

            ord <- switch(self$options$sortNodes,
                "none" = seq_along(vlabels),
                "name" = order(vlabels_display, decreasing = self$options$sortDecreasing),
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

                # Get color palette (color-blind safe)
                # Use viridis-based palettes for better accessibility
                if (n_groups <= 8) {
                    # viridis "turbo" palette - excellent for categorical data, CB-safe
                    group_colors <- viridisLite::turbo(n_groups, begin = 0.1, end = 0.9)
                } else if (n_groups <= 12) {
                    # Extend to 12 colors with viridis
                    group_colors <- viridisLite::viridis(n_groups, option = "H")
                } else if (n_groups <= 24) {
                    # Use HCL-based palette for many groups
                    group_colors <- grDevices::hcl.colors(n_groups, palette = "Dark 3")
                } else {
                    # For very large number of groups, use polychrome approach
                    group_colors <- grDevices::hcl.colors(n_groups, palette = "Zissou 1")
                }
                names(group_colors) <- unique_groups

                # Assign colors to vertices using node_groups directly
                # node_groups is already a named vector mapping node labels to groups
                node_fill <- group_colors[node_groups]
                node_fill[is.na(node_fill)] <- "lightgray"
                node_border <- "black"

                # Arc colors: Support multiple coloring modes
                edgelist <- network_data$edgelist
                source_groups <- node_groups[edgelist[, 1]]
                target_groups <- node_groups[edgelist[, 2]]

                arc_color_mode <- self$options$arcColorMode

                if (arc_color_mode == "source") {
                    # Color arcs by SOURCE node's group
                    arc_colors <- group_colors[source_groups]
                    arc_colors[is.na(arc_colors)] <- "gray50"
                } else if (arc_color_mode == "target") {
                    # Color arcs by TARGET node's group
                    arc_colors <- group_colors[target_groups]
                    arc_colors[is.na(arc_colors)] <- "gray50"
                } else if (arc_color_mode == "gradient") {
                    # Gradient: Blend source and target colors
                    arc_colors <- character(nrow(edgelist))
                    for (i in seq_len(nrow(edgelist))) {
                        src_color <- group_colors[source_groups[i]]
                        tgt_color <- group_colors[target_groups[i]]

                        if (!is.na(src_color) && !is.na(tgt_color)) {
                            # Blend the two colors (simple average in RGB space)
                            src_rgb <- col2rgb(src_color) / 255
                            tgt_rgb <- col2rgb(tgt_color) / 255
                            blended_rgb <- (src_rgb + tgt_rgb) / 2
                            arc_colors[i] <- rgb(blended_rgb[1], blended_rgb[2], blended_rgb[3])
                        } else if (!is.na(src_color)) {
                            arc_colors[i] <- src_color
                        } else if (!is.na(tgt_color)) {
                            arc_colors[i] <- tgt_color
                        } else {
                            arc_colors[i] <- "gray50"
                        }
                    }
                } else {
                    # Default to source mode
                    arc_colors <- group_colors[source_groups]
                    arc_colors[is.na(arc_colors)] <- "gray50"
                }

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

                # WEIGHTED CENTRALITY: Handle weight semantics correctly
                has_weights <- !is.null(igraph::E(g)$weight) && any(igraph::E(g)$weight != 1)

                if (has_weights) {
                    # Determine weight interpretation
                    if (self$options$weightMode == "strength") {
                        # STRENGTH MODE: Higher weights = stronger connections
                        # Invert weights for centrality calculations (igraph treats weights as distances)
                        max_weight <- max(igraph::E(g)$weight, na.rm = TRUE)
                        min_weight <- min(igraph::E(g)$weight, na.rm = TRUE)

                        # Invert: strong connections (high weight) become short distances (low weight)
                        inv_weights <- max_weight - igraph::E(g)$weight + min_weight

                        betweenness <- igraph::betweenness(g, weights = inv_weights)
                        closeness <- igraph::closeness(g, weights = inv_weights)
                        degree_label <- .("weighted strength")
                    } else {
                        # DISTANCE MODE: Higher weights = longer paths (igraph default)
                        betweenness <- igraph::betweenness(g, weights = igraph::E(g)$weight)
                        closeness <- igraph::closeness(g, weights = igraph::E(g)$weight)
                        degree_label <- .("weighted distance")
                    }
                } else {
                    # Unweighted network
                    betweenness <- igraph::betweenness(g)
                    closeness <- igraph::closeness(g)
                    degree_label <- .("connections")
                }

                # HANDLE DISCONNECTED GRAPHS: Filter out Inf/NA values
                betweenness[is.infinite(betweenness) | is.na(betweenness)] <- 0
                closeness[is.infinite(closeness) | is.na(closeness)] <- 0

                # Check if graph is connected and warn if not
                is_connected <- igraph::is_connected(g)
                if (!is_connected) {
                    stats_text <- paste(stats_text,
                        "<div style='background-color: #fff3cd; padding: 8px; margin: 8px 0; border-left: 4px solid #ffc107;'>",
                        .("<p><strong>⚠️ Disconnected Network:</strong> This network contains isolated components. Centrality measures (betweenness, closeness) may be less meaningful. Nodes unreachable from others are assigned zero centrality.</p>"),
                        "</div>",
                        sep = "\n"
                    )
                }

                # Get key nodes for clinical interpretation (after filtering invalid values)
                valid_degrees <- network_data$degrees[is.finite(network_data$degrees) & !is.na(network_data$degrees)]
                valid_betweenness <- betweenness[is.finite(betweenness) & !is.na(betweenness)]

                if (length(valid_degrees) > 0 && length(valid_betweenness) > 0) {
                    highest_degree_node <- names(which.max(network_data$degrees))
                    highest_betweenness_node <- names(which.max(betweenness))
                    max_degree <- max(network_data$degrees)
                    max_betweenness <- max(betweenness)
                } else {
                    # Fallback if no valid centrality values
                    stats_text <- paste(stats_text,
                        "<div style='background-color: #f8d7da; padding: 8px; margin: 8px 0; border-left: 4px solid #dc3545;'>",
                        .("<p><strong>❌ Error:</strong> Could not calculate valid centrality measures. Network may be too disconnected or contain invalid data.</p>"),
                        "</div>",
                        sep = "\n"
                    )
                    self$results$networkStats$setContent(stats_text)
                    return()
                }

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
                .("<li><strong>Self-loops:</strong> Connections from a node to itself are automatically detected and removed</li>"),
                .("<li><strong>Duplicate Edges:</strong> When 'Aggregate Duplicate Edges' is enabled (recommended), parallel edges are combined by summing weights for accurate density/centrality calculations</li>"),
                .("<li><strong>Edge Weight Mode:</strong> 'Strength' mode (default) treats higher weights as stronger connections, suitable for correlations/scores. 'Distance' mode treats higher weights as longer paths, suitable for costs/distances</li>"),
                .("<li><strong>Isolated Nodes:</strong> Entities with no connections appear only if referenced; unreachable nodes get zero centrality</li>"),
                "</ul>",

                .("<h4>📈 Interpretation Notes:</h4>"),
                "<ul style='margin-left: 20px;'>",
                .("<li><strong>Network Density:</strong> 0-0.2 (sparse), 0.2-0.5 (moderate), >0.5 (dense). Aggregating duplicate edges ensures density ≤ 1.0</li>"),
                .("<li><strong>Degree Centrality:</strong> Number of direct connections (hub identification). For weighted networks, uses strength (sum of weights)</li>"),
                .("<li><strong>Betweenness Centrality:</strong> Importance as a bridge between network regions. Accounts for weight interpretation mode</li>"),
                .("<li><strong>Closeness Centrality:</strong> Average distance to all other nodes. May be Inf for disconnected graphs (auto-filtered)</li>"),
                .("<li><strong>Connected Components:</strong> Isolated subnetworks may indicate distinct functional modules. Warnings issued for disconnected graphs</li>"),
                "</ul>",

                .("<h4>⚠️ Common Pitfalls & Important Notes:</h4>"),
                "<ul style='margin-left: 20px;'>",
                .("<li><strong>Small Networks:</strong> Centrality measures less reliable with < 10 nodes (warning issued)</li>"),
                .("<li><strong>Duplicate Edges:</strong> If not aggregated, density can exceed 1.0 and centrality measures may be inflated. Enable 'Aggregate Duplicate Edges' to fix this</li>"),
                .("<li><strong>Weight Interpretation:</strong> Ensure 'Edge Weight Interpretation' matches your data. Most biological/clinical networks use 'Strength' mode</li>"),
                .("<li><strong>Group Conflicts:</strong> If a node appears with multiple group labels, warnings are issued and the first occurrence is used</li>"),
                .("<li><strong>Arc Coloring:</strong> Choose 'Source', 'Target', or 'Gradient' mode to control how arcs are colored when grouping is enabled</li>"),
                .("<li><strong>Directed Networks:</strong> When 'Directed Network' is enabled, statistics respect edge direction but the arc diagram visualization does NOT display arrows. This is a limitation of the arc plot style</li>"),
                .("<li><strong>Disconnected Graphs:</strong> Closeness/betweenness may be unreliable. Inf/NA values are auto-filtered and warnings are issued</li>"),
                .("<li><strong>Clinical Relevance:</strong> High connectivity doesn't always imply biological significance. Consider domain knowledge when interpreting results</li>"),
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
        },

        # Helper method to generate glossary panel
        .generateGlossary = function() {
            glossary_html <- paste(
                .("<h3>📖 Network Analysis Glossary</h3>"),
                "<div style='background-color: #f9f9f9; padding: 15px; margin: 10px 0; border-radius: 5px; border-left: 4px solid #2196F3;'>",

                "<dl style='margin: 0;'>",

                "<dt style='font-weight: bold; margin-top: 10px;'>", .("Node (Vertex)"), "</dt>",
                "<dd style='margin-left: 20px; margin-bottom: 8px;'>",
                .("An entity or element in the network (e.g., gene, patient, disease, protein). Nodes are the fundamental building blocks of networks, representing the items being studied."),
                "</dd>",

                "<dt style='font-weight: bold; margin-top: 10px;'>", .("Edge (Arc)"), "</dt>",
                "<dd style='margin-left: 20px; margin-bottom: 8px;'>",
                .("A connection or relationship between two nodes (e.g., regulatory link, similarity score, co-occurrence). In arc diagrams, edges are visualized as curved arcs connecting nodes."),
                "</dd>",

                "<dt style='font-weight: bold; margin-top: 10px;'>", .("Degree Centrality"), "</dt>",
                "<dd style='margin-left: 20px; margin-bottom: 8px;'>",
                .("Number of direct connections a node has. High-degree nodes are 'hubs' that interact with many other entities. <strong>Clinical interpretation:</strong> A gene with high degree might be a master regulator; a patient with high degree might share characteristics with many others."),
                "</dd>",

                "<dt style='font-weight: bold; margin-top: 10px;'>", .("Betweenness Centrality"), "</dt>",
                "<dd style='margin-left: 20px; margin-bottom: 8px;'>",
                .("Measures how often a node lies on shortest paths between other nodes. High betweenness indicates the node serves as a 'bridge' connecting different network regions. <strong>Clinical interpretation:</strong> Entities with high betweenness may be critical bottlenecks or gateways in biological pathways."),
                "</dd>",

                "<dt style='font-weight: bold; margin-top: 10px;'>", .("Closeness Centrality"), "</dt>",
                "<dd style='margin-left: 20px; margin-bottom: 8px;'>",
                .("Average distance from a node to all other nodes in the network. Higher closeness means the node can reach others more quickly. <strong>Note:</strong> May be infinite (Inf) for disconnected graphs."),
                "</dd>",

                "<dt style='font-weight: bold; margin-top: 10px;'>", .("Network Density"), "</dt>",
                "<dd style='margin-left: 20px; margin-bottom: 8px;'>",
                .("Proportion of actual connections vs. all possible connections (0-1 scale). <strong>Interpretation:</strong> 0-0.2 = sparse (selective relationships), 0.2-0.5 = moderate (balanced), >0.5 = dense (highly interconnected). Dense networks may indicate shared mechanisms or pathways."),
                "</dd>",

                "<dt style='font-weight: bold; margin-top: 10px;'>", .("Weighted Network"), "</dt>",
                "<dd style='margin-left: 20px; margin-bottom: 8px;'>",
                .("A network where edges have numerical values representing connection strength, correlation, distance, or other quantitative measures. Weights influence how centrality measures are calculated."),
                "</dd>",

                "<dt style='font-weight: bold; margin-top: 10px;'>", .("Directed vs. Undirected"), "</dt>",
                "<dd style='margin-left: 20px; margin-bottom: 8px;'>",
                .("<strong>Directed:</strong> Edges have direction (A→B is different from B→A), suitable for regulatory relationships or causal pathways. <strong>Undirected:</strong> Edges are bidirectional (A-B same as B-A), suitable for similarity or co-occurrence networks. <strong>Note:</strong> Arc diagrams do not display arrows even for directed networks."),
                "</dd>",

                "<dt style='font-weight: bold; margin-top: 10px;'>", .("Isolated Node"), "</dt>",
                "<dd style='margin-left: 20px; margin-bottom: 8px;'>",
                .("A node with degree = 0 (no connections). Isolated nodes appear in the diagram but have zero centrality measures. May indicate rare or independent entities."),
                "</dd>",

                "<dt style='font-weight: bold; margin-top: 10px;'>", .("Connected Component"), "</dt>",
                "<dd style='margin-left: 20px; margin-bottom: 8px;'>",
                .("A subgroup of nodes where every node can reach every other node in that subgroup. Disconnected networks have multiple separate components, which may represent distinct functional modules or pathways."),
                "</dd>",

                "</dl>",

                "<p style='margin-top: 15px; font-style: italic; color: #666;'>",
                .("💡 Tip: Use this glossary as a quick reference when interpreting network statistics and visualizations."),
                "</p>",

                "</div>",
                sep = "\n"
            )

            self$results$glossary$setContent(glossary_html)
        }
    )
)
