#' @title Study Flowchart (Enhanced)
#' @importFrom R6 R6Class
#' @import jmvcore
#' @importFrom magrittr %>%
#' @importFrom DiagrammeR create_graph create_node_df create_edge_df add_nodes_from_table add_edges_from_table add_global_graph_attrs render_graph
#' @importFrom DiagrammeRsvg export_svg
#' @importFrom dplyr mutate case_when select
#' @importFrom ggplot2 ggplot aes labs theme theme_minimal scale_fill_manual element_text
#' @importFrom RColorBrewer brewer.pal
#' @importFrom viridis viridis
#' @importFrom htmltools as.tags
#' @description Creates professional flowcharts using DiagrammeR, ggplot2, or flowchart package, supporting node-count, edge-list, and clinical trial flow formats

flowchartClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "flowchartClass",
    inherit = flowchartBase,
    private = list(
        .nodeData = NULL,
        .dataMode = NULL,
        .edgeData = NULL,
        .clinicalFlowData = NULL,
        
        .run = function() {
            cat("Run function called\n")
            
            # Get input mode
            input_mode <- self$options$input_mode
            cat("Input mode:", input_mode, "\n")
            
            # Check requirements based on input mode
            variables_missing <- FALSE
            
            if (input_mode == "node_count") {
                cat("nodes length:", length(self$options$nodes), "\n")
                cat("counts length:", length(self$options$counts), "\n")
                if (is.null(self$options$nodes) || is.null(self$options$counts) ||
                    length(self$options$nodes) == 0 || length(self$options$counts) == 0) {
                    variables_missing <- TRUE
                    cat("Variables missing: TRUE\n")
                }
            } else if (input_mode == "edge_list") {
                if (is.null(self$options$from_var) || is.null(self$options$to_var) ||
                    self$options$from_var == "" || self$options$to_var == "") {
                    variables_missing <- TRUE
                }
            }
            
            if (variables_missing) {
                cat("Variables missing - showing welcome message and returning early\n")
                welcome_msg <- private$.generateWelcomeMessage(input_mode)
                self$results$todo$setContent(welcome_msg)
                # Clear any existing table data and plot state
                # Clear nodeData table
                for (i in self$results$nodeData$rowKeys) {
                    self$results$nodeData$removeRow(rowKey = i)
                }
                # Clear plot state
                self$results$diagram$setContent("")
                cat("Cleared plot state and returning\n")
                return()
            } else {
                cat("Variables present - continuing with analysis\n")
                self$results$todo$setContent("")
            }

            # Validate data availability
            if (nrow(self$data) == 0) {
                stop("The provided dataset contains no rows. Please check your data and try again.")
            }

            # Process data based on input mode
            if (input_mode == "node_count") {
                private$.processNodeCountData()
            } else if (input_mode == "edge_list") {
                private$.processEdgeListData()
            }
            
            # Generate interpretation if requested
            if (self$options$show_interpretation) {
                interpretation_html <- private$.generateInterpretation(input_mode)
                self$results$interpretation$setContent(interpretation_html)
            }
        },
        
        .processNodeCountData = function() {
            # Original flowchart logic for node-count mode
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
                stop("Number of node variables must match number of count variables. Please ensure each node has a corresponding count.")
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
                    nodeData$label[i] <<- paste("Node", i, "(Error)")
                    nodeData$count[i] <<- 0
                    nodeData$valid[i] <<- FALSE
                })
            }

            # Filter out invalid nodes
            validNodes <- nodeData[nodeData$valid, ]
            
            if (nrow(validNodes) == 0) {
                stop("No valid node data found. Please check your variable selections and data.")
            }

            # Populate node data table
            for (i in 1:nrow(validNodes)) {
                self$results$nodeData$addRow(rowKey = i, values = list(
                    node = validNodes$label[i],
                    count = validNodes$count[i]
                ))
            }

            # Store processed data for plotting
            private$.nodeData <- validNodes
            private$.dataMode <- "node_count"
            
            # Set plot state for jamovi
            # Generate DiagrammeR HTML content
            html_content <- private$.generateDiagrammeRHtml("node_count", validNodes)
            self$results$diagram$setContent(html_content)
            cat("Set diagram HTML content with mode: node_count and", nrow(validNodes), "nodes\n")
        },
        
        .processEdgeListData = function() {
            # Edge-list mode processing
            dataset <- self$data
            from_var <- self$options$from_var
            to_var <- self$options$to_var
            group_var <- self$options$group_var

            # Prepare analysis data
            required_vars <- c(from_var, to_var)
            if (!is.null(group_var) && group_var != "") {
                required_vars <- c(required_vars, group_var)
            }
            
            analysis_data <- dataset[required_vars]
            analysis_data <- analysis_data[complete.cases(analysis_data), ]
            
            if (nrow(analysis_data) == 0) {
                stop("No complete cases found for the selected variables.")
            }

            # Convert variables to appropriate types
            analysis_data[[from_var]] <- as.character(analysis_data[[from_var]])
            analysis_data[[to_var]] <- as.character(analysis_data[[to_var]])
            
            if (!is.null(group_var) && group_var != "") {
                analysis_data[[group_var]] <- as.factor(analysis_data[[group_var]])
            }
            
            # Create node summary for the nodeData table
            all_nodes <- unique(c(analysis_data[[from_var]], analysis_data[[to_var]]))
            node_counts <- table(c(analysis_data[[from_var]], analysis_data[[to_var]]))
            
            # Populate node data table with edge counts
            for (i in seq_along(all_nodes)) {
                node_name <- all_nodes[i]
                node_count <- as.numeric(node_counts[node_name])
                
                self$results$nodeData$addRow(rowKey = i, values = list(
                    node = node_name,
                    count = node_count
                ))
            }

            # Store processed data for plotting
            private$.edgeData <- analysis_data
            private$.dataMode <- "edge_list"
            
            # Set plot state for jamovi
            # Generate DiagrammeR HTML content for edge list mode
            html_content <- "<p>Edge list mode not fully implemented yet.</p>"
            self$results$diagram$setContent(html_content)
        },
        
        .generateWelcomeMessage = function(input_mode) {
            if (input_mode == "node_count") {
                return(paste0(
                    "<div style='background-color: #e7f3ff; padding: 20px; border-radius: 8px; margin: 10px 0; border-left: 4px solid #007bff;'>",
                    "<h3 style='color: #007bff; margin-top: 0;'>🏥 Patient Flow Diagram</h3>",
                    "<p style='font-size: 16px; margin: 15px 0;'><strong>Create professional flowcharts for clinical studies</strong></p>",
                    
                    "<div style='background-color: white; padding: 15px; border-radius: 5px; margin: 15px 0; box-shadow: 0 2px 4px rgba(0,0,0,0.1);'>",
                    "<h4 style='color: #28a745; margin-top: 0;'>📋 Step-by-Step Setup:</h4>",
                    "<div style='margin: 12px 0; padding: 10px; background-color: #f8f9fa; border-radius: 4px; display: flex; align-items: center;'>",
                    "<span style='background-color: #007bff; color: white; border-radius: 50%; width: 24px; height: 24px; display: inline-flex; align-items: center; justify-content: center; margin-right: 12px; font-weight: bold; font-size: 12px;'>1</span>",
                    "<div><strong>Drag Step Names:</strong> From left panel, drag variable with step descriptions (like 'Screened', 'Enrolled') to '📝 Step Names' box</div>",
                    "</div>",
                    "<div style='margin: 12px 0; padding: 10px; background-color: #f8f9fa; border-radius: 4px; display: flex; align-items: center;'>",
                    "<span style='background-color: #28a745; color: white; border-radius: 50%; width: 24px; height: 24px; display: inline-flex; align-items: center; justify-content: center; margin-right: 12px; font-weight: bold; font-size: 12px;'>2</span>",
                    "<div><strong>Drag Participant Counts:</strong> From left panel, drag variable with numbers (like 500, 485, 450) to '🔢 Participant Counts' box</div>",
                    "</div>",
                    "<div style='margin: 12px 0; padding: 10px; background-color: #f8f9fa; border-radius: 4px; display: flex; align-items: center;'>",
                    "<span style='background-color: #6f42c1; color: white; border-radius: 50%; width: 24px; height: 24px; display: inline-flex; align-items: center; justify-content: center; margin-right: 12px; font-weight: bold; font-size: 12px;'>3</span>",
                    "<div><strong>Ignore Other Boxes:</strong> Skip the boxes labeled 'for Edge-List mode' - you only need the two above!</div>",
                    "</div>",
                    "</div>",
                    
                    "<div style='background-color: #d1ecf1; padding: 15px; border-radius: 5px; margin: 15px 0; border: 1px solid #bee5eb;'>",
                    "<h4 style='color: #0c5460; margin-top: 0;'>🎯 Perfect for Clinical Research:</h4>",
                    "<div style='display: grid; grid-template-columns: 1fr 1fr; gap: 10px;'>",
                    "<div>✓ CONSORT flowcharts</div><div>✓ Screening cascades</div>",
                    "<div>✓ Enrollment tracking</div><div>✓ Treatment pathways</div>",
                    "<div>✓ Quality metrics</div><div>✓ Patient journeys</div>",
                    "</div>",
                    "</div>",
                    
                    "<div style='background-color: #f8d7da; padding: 15px; border-radius: 5px; margin: 15px 0; border: 1px solid #f5c6cb;'>",
                    "<h4 style='color: #721c24; margin-top: 0;'>⚠️ Common Issues & Solutions:</h4>",
                    "<ul style='margin: 10px 0; padding-left: 20px; line-height: 1.6;'>",
                    "<li><strong>Numbers don't align?</strong> Ensure step names and counts have the same number of rows</li>",
                    "<li><strong>Missing percentages?</strong> Enable 'Show percentages' in Display Options</li>",
                    "<li><strong>Publication ready?</strong> Use 'Clinical Blue' colors and include title</li>",
                    "</ul>",
                    "</div>",
                    
                    "<div style='text-align: center; margin-top: 20px; padding: 15px; background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); color: white; border-radius: 5px;'>",
                    "<h4 style='margin: 0; font-size: 18px;'>🚀 Ready to start? Add your variables above!</h4>",
                    "</div>",
                    
                    "</div>"
                ))
            } else if (input_mode == "edge_list") {
                return("
                <div style='font-family: Arial, sans-serif; color: #2c3e50;'>
                  <h3>🔀 Enhanced Study Flowchart Creator</h3>
                  <p>Creates professional flowcharts with <strong>dual rendering engines</strong> and flexible data input modes.</p>
                  
                  <h4>Edge-List Mode (Selected):</h4>
                  <ol>
                    <li><strong>From Node:</strong> Variable with source node names</li>
                    <li><strong>To Node:</strong> Variable with target node names</li>
                    <li><strong>Node Grouping:</strong> Optional categories for color coding</li>
                  </ol>
                  
                  <h4>Perfect For:</h4>
                  <ul>
                    <li><strong>Process Workflows:</strong> Step-by-step procedures</li>
                    <li><strong>Decision Trees:</strong> Smart node typing (enable Node Typing option)</li>
                    <li><strong>Network Analysis:</strong> Connection visualizations</li>
                    <li><strong>Clinical Algorithms:</strong> Evidence-based decision support</li>
                  </ul>
                  
                  <p><em>💡 Try Node-Count mode for traditional clinical flowcharts!</em></p>
                  <hr>
                </div>")
            } else {
                return("
                <div style='font-family: Arial, sans-serif; color: #2c3e50;'>
                  <h3>🔀 Study Flowchart Creator</h3>
                  <p>Please select either <strong>Node-Count</strong> or <strong>Edge-List</strong> mode to begin.</p>
                  
                  <h4>Clinical Trial Flow Mode (Selected):</h4>
                  <ol>
                    <li><strong>Participant Identifier:</strong> Variable containing unique participant IDs</li>
                    <li><strong>Trial Group/Arm:</strong> Optional variable for treatment arms</li>
                    <li><strong>Exclusion Criteria:</strong> Optional exclusion reasons</li>
                    <li><strong>Filter Expressions:</strong> Custom filtering logic (e.g. '!is.na(group)')</li>
                    <li><strong>Filter Labels:</strong> Descriptive labels for each filter step</li>
                  </ol>
                  
                  <h4>Perfect For:</h4>
                  <ul>
                    <li><strong>Clinical Trial Flow:</strong> Participant tracking and analysis</li>
                    <li><strong>Participant Tracking:</strong> Enrollment to completion</li>
                    <li><strong>Exclusion Analysis:</strong> Automatic percentage calculations</li>
                    <li><strong>Multi-arm Trials:</strong> Split flows by treatment group</li>
                  </ul>
                  
                  <p><em>💡 Uses the flowchart R package for data-driven flow diagrams!</em></p>
                  <hr>
                </div>")
            }
        },
        
        .generateInterpretation = function(input_mode) {
            render_engine <- self$options$render_engine
            
            interpretation <- paste0(
                "<div style='background-color: #f8f9fa; padding: 20px; border-radius: 8px; margin: 10px 0; border-left: 4px solid #28a745;'>",
                "<h3 style='color: #28a745; margin-top: 0;'>📊 About This Analysis</h3>",
                
                "<div style='background-color: white; padding: 15px; border-radius: 5px; margin: 15px 0; box-shadow: 0 1px 3px rgba(0,0,0,0.1);'>",
                "<h4 style='color: #007bff; margin-top: 0;'>What does this tool do?</h4>",
                "<p>Creates professional flowcharts showing patient flow through clinical studies, research processes, or treatment pathways. Perfect for publications, presentations, and regulatory submissions.</p>",
                "</div>",
                
                "<div style='background-color: white; padding: 15px; border-radius: 5px; margin: 15px 0; box-shadow: 0 1px 3px rgba(0,0,0,0.1);'>",
                "<h4 style='color: #6f42c1; margin-top: 0;'>Current Settings:</h4>",
                "<ul style='line-height: 1.6;'>",
                "<li><strong>Mode:</strong> ", 
                if (input_mode == "node_count") {
                    "Patient Flow (Node-Count) - Shows sequential steps with counts"
                } else if (input_mode == "edge_list") {
                    "Process Flow (Edge-List) - Shows connected processes"
                } else {
                    "Clinical Trial (CONSORT-style) - Detailed participant tracking"
                },
                "</li>",
                "<li><strong>Visualization:</strong> ", 
                if (render_engine == "diagrammer") {
                    "Classic diagrams (DiagrammeR) - Traditional flowchart style"
                } else if (render_engine == "ggplot2") {
                    "Modern graphics (ggplot2) - Contemporary design with advanced styling"
                } else {
                    "Clinical Flow (Specialized) - CONSORT-compliant formatting"
                },
                "</li>",
                "</ul>",
                "</div>",
                
                "<div style='background-color: #e7f3ff; padding: 15px; border-radius: 5px; margin: 15px 0; border: 1px solid #b3d7ff;'>",
                "<h4 style='color: #0066cc; margin-top: 0;'>💡 Clinical Applications:</h4>",
                "<div style='display: grid; grid-template-columns: 1fr 1fr; gap: 15px;'>",
                if (input_mode == "node_count") {
                    paste0(
                        "<div><strong>👥 Patient Studies:</strong><br>Track enrollment, randomization, completion rates</div>",
                        "<div><strong>📋 Screening:</strong><br>Show eligibility assessment and selection process</div>",
                        "<div><strong>📊 Quality Metrics:</strong><br>Monitor process efficiency and bottlenecks</div>",
                        "<div><strong>📑 Publications:</strong><br>Create CONSORT-compliant flow diagrams</div>"
                    )
                } else if (input_mode == "edge_list") {
                    paste0(
                        "<div><strong>🔄 Process Mapping:</strong><br>Visualize treatment pathways and decision points</div>",
                        "<div><strong>⚕️ Clinical Workflows:</strong><br>Show care coordination and handoffs</div>",
                        "<div><strong>🎯 Decision Trees:</strong><br>Map diagnostic or treatment algorithms</div>",
                        "<div><strong>📈 Improvement:</strong><br>Identify process optimization opportunities</div>"
                    )
                } else {
                    paste0(
                        "<div><strong>🧪 Clinical Trials:</strong><br>Detailed participant flow with exclusions</div>",
                        "<div><strong>📊 Regulatory:</strong><br>FDA/EMA submission-ready diagrams</div>",
                        "<div><strong>👨‍⚕️ Multi-arm Studies:</strong><br>Complex randomization schemes</div>",
                        "<div><strong>📋 Protocol Deviations:</strong><br>Track and visualize study issues</div>"
                    )
                },
                "</div>",
                "</div>",
                
                "<div style='background-color: #fff3cd; padding: 15px; border-radius: 5px; margin: 15px 0; border: 1px solid #ffeaa7;'>",
                "<h4 style='color: #856404; margin-top: 0;'>📝 How to Interpret Results:</h4>",
                "<ul style='line-height: 1.6; margin: 10px 0; padding-left: 20px;'>",
                "<li><strong>Box Sizes:</strong> Larger boxes typically represent higher counts</li>",
                "<li><strong>Percentages:</strong> Show retention rates from baseline (first step)</li>",
                "<li><strong>Exclusion Numbers:</strong> Help identify major dropout points</li>",
                "<li><strong>Color Coding:</strong> Distinguishes different types of steps or outcomes</li>",
                "</ul>",
                "</div>",
                
                "</div>"
            )
            
            return(interpretation)
        },

        .generateDiagrammeRHtml = function(mode, nodeData) {
            cat("Generating DiagrammeR HTML for mode:", mode, "\n")
            
            if (mode == "node_count") {
                n_nodes <- nrow(nodeData)
                cat("NodeData rows:", n_nodes, "\n")
                
                # Calculate flow statistics
                flow_stats <- list(
                    total_nodes = n_nodes,
                    max_count = max(nodeData$count, na.rm = TRUE),
                    min_count = min(nodeData$count, na.rm = TRUE),
                    total_flow = sum(nodeData$count, na.rm = TRUE)
                )
                
                # Enhanced node styling based on count ranges
                max_count <- flow_stats$max_count
                min_count <- flow_stats$min_count
                
                # Calculate node colors based on selected scheme
                base_colors <- private$.getColorPalette()
                
                if (max_count > min_count) {
                    # Scale node colors by count (higher counts = more intense)
                    color_intensity <- (nodeData$count - min_count) / (max_count - min_count)
                    color_indices <- as.integer(color_intensity * (length(base_colors) - 1)) + 1
                    node_colors <- base_colors[color_indices]
                } else {
                    node_colors <- rep(base_colors[1], n_nodes)
                }
                
                # Enhanced positioning for better layout
                direction <- self$options$direction
                if (direction == "TB") {
                    # Top to bottom: center horizontally, space vertically
                    x_pos <- rep(2, n_nodes)
                    y_pos <- seq(n_nodes, 1, by = -1)  # Reverse order for top-down
                } else if (direction == "LR") {
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
                    if (self$options$showPercentages && i > 1 && nodeData$count[1] > 0) {
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
                    width = self$options$nodeWidth,
                    height = self$options$nodeHeight,
                    fontsize = self$options$fontSize,
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
                        value = self$options$direction,
                        attr_type = "graph"
                    ) %>%
                    DiagrammeR::add_global_graph_attrs(
                        attr = "splines",
                        value = "ortho",  # Orthogonal edges for cleaner look
                        attr_type = "graph"
                    )
                
                # Add nodes to graph
                graph <- graph %>%
                    DiagrammeR::add_nodes_from_table(
                        nodes_df,
                        label_col = "label"
                    )
                
                # Add edges for node_count mode
                if (n_nodes > 1) {
                    # Calculate exclusion counts for edge labels if requested
                    edge_labels <- character(n_nodes - 1)
                    if (self$options$showExclusions) {
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
                        label = edge_labels
                    )
                    
                    graph <- graph %>%
                        DiagrammeR::add_edges_from_table(edges_df)
                }
                
                # Add title if requested
                if (self$options$includeTitle) {
                    plot_title <- self$options$plot_title
                    
                    graph <- graph %>%
                        DiagrammeR::add_global_graph_attrs(
                            attr = "label",
                            value = plot_title,
                            attr_type = "graph"
                        ) %>%
                        DiagrammeR::add_global_graph_attrs(
                            attr = "labelloc",
                            value = "t",
                            attr_type = "graph"
                        )
                }
                
                # Debug graph structure before rendering
                cat("Graph created successfully\n")
                cat("Number of nodes in graph:", DiagrammeR::count_nodes(graph), "\n")
                cat("Number of edges in graph:", DiagrammeR::count_edges(graph), "\n")

                # Try DiagrammeR SVG export first (following vartree pattern)
                tryCatch({
                    cat("Attempting DiagrammeR SVG export...\n")
                    svg_content <- DiagrammeRsvg::export_svg(gv = graph)

                    if (!is.null(svg_content) && nchar(svg_content) > 0) {
                        cat("SVG export successful, length:", nchar(svg_content), "\n")

                        # Apply vartree's SVG processing pattern
                        # Adjust width for proper display
                        maxwidth <- 800  # Default width for flowcharts
                        svg_content <- base::sub('width=\"[[:digit:]]+pt\"',
                                               paste0('width=', maxwidth, 'pt '),
                                               svg_content)

                        # Wrap in vartree's HTML document structure
                        wrapped_svg <- paste0(
                            '<html><head><style>',
                            '#myDIV {width: 100%; max-width: 1000px; height: auto; min-height: 400px; overflow: auto; margin: 0 auto;}',
                            '</style></head><body><div id="myDIV">',
                            svg_content,
                            '</div></body></html>'
                        )

                        cat("Returning wrapped SVG content\n")
                        return(wrapped_svg)
                    } else {
                        cat("SVG export returned empty content, falling back to HTML/CSS\n")
                    }
                }, error = function(e) {
                    cat("SVG export failed with error:", e$message, "\n")
                    cat("Falling back to HTML/CSS flowchart\n")
                })

                # Fallback: Create professional HTML/CSS flowchart
                cat("Creating HTML/CSS flowchart fallback...\n")
                return(private$.createHtmlFlowchart(nodeData, n_nodes, node_colors))
            }
            
            return("<p>Mode not implemented yet.</p>")
        },
        
        .createHtmlFlowchart = function(nodeData, n_nodes, node_colors) {
            cat("Generating professional HTML/CSS flowchart\n")
            
            # Start with container and title
            html_content <- paste0(
                "<div style='font-family: Arial, Helvetica, sans-serif; max-width: 800px; margin: 0 auto; padding: 20px; background: #ffffff; border-radius: 8px; box-shadow: 0 4px 6px rgba(0,0,0,0.1);'>"
            )
            
            # Add title if requested
            if (self$options$includeTitle) {
                html_content <- paste0(html_content,
                    "<h2 style='text-align: center; color: #2c3e50; margin-bottom: 30px; font-weight: 600; border-bottom: 2px solid #007bff; padding-bottom: 10px;'>",
                    self$options$plot_title,
                    "</h2>"
                )
            }
            
            # Create flowchart container with CSS for proper layout
            html_content <- paste0(html_content,
                "<div style='display: flex; flex-direction: column; align-items: center; gap: 15px;'>"
            )
            
            # Generate nodes with enhanced styling
            for (i in 1:n_nodes) {
                # Node content
                node_label <- htmltools::htmlEscape(nodeData$label[i])
                node_count <- nodeData$count[i]
                
                # Calculate percentage if requested
                percentage_text <- ""
                if (self$options$showPercentages && i > 1 && nodeData$count[1] > 0) {
                    pct <- round((node_count / nodeData$count[1]) * 100, 1)
                    percentage_text <- paste0(" (", pct, "%)")
                }
                
                # Node styling based on position and type
                node_style <- if (i == 1) {
                    # First node (starting point)
                    paste0(
                        "background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); ",
                        "color: white; border: 3px solid #5a67d8;"
                    )
                } else if (i == n_nodes) {
                    # Last node (endpoint)  
                    paste0(
                        "background: linear-gradient(135deg, #11998e 0%, #38ef7d 100%); ",
                        "color: white; border: 3px solid #10b981;"
                    )
                } else {
                    # Middle nodes
                    paste0(
                        "background: linear-gradient(135deg, #667eea 20%, #764ba2 80%); ",
                        "color: white; border: 3px solid #007bff;"
                    )
                }
                
                # Create node
                html_content <- paste0(html_content,
                    "<div style='",
                    "min-width: 280px; max-width: 400px; padding: 20px; border-radius: 12px; ",
                    "text-align: center; box-shadow: 0 4px 12px rgba(0,0,0,0.15); ",
                    "transform: translateY(0); transition: transform 0.2s ease; ",
                    node_style,
                    "'>",
                    "<div style='font-size: 16px; font-weight: 600; margin-bottom: 8px; line-height: 1.4;'>",
                    node_label,
                    "</div>",
                    "<div style='font-size: 24px; font-weight: 700; margin-bottom: 4px;'>",
                    "n = ", format(node_count, big.mark = ","),
                    "</div>",
                    if (percentage_text != "") {
                        paste0("<div style='font-size: 14px; opacity: 0.9;'>", percentage_text, "</div>")
                    } else {
                        ""
                    },
                    "</div>"
                )
                
                # Add arrow and exclusion info (except after last node)
                if (i < n_nodes) {
                    # Calculate exclusions
                    excluded <- nodeData$count[i] - nodeData$count[i + 1]
                    
                    # Arrow with exclusion information
                    html_content <- paste0(html_content,
                        "<div style='display: flex; flex-direction: column; align-items: center; margin: 10px 0;'>",
                        
                        # Exclusion box (if any exclusions and option enabled)
                        if (self$options$showExclusions && excluded > 0) {
                            paste0(
                                "<div style='background: #fff3cd; border: 2px solid #ffc107; border-radius: 8px; padding: 8px 16px; margin-bottom: 8px; font-size: 12px; font-weight: 600; color: #856404;'>",
                                "Excluded: ", format(excluded, big.mark = ","),
                                "</div>"
                            )
                        } else {
                            ""
                        },
                        
                        # Arrow
                        "<div style='",
                        "width: 0; height: 0; ",
                        "border-left: 15px solid transparent; ",
                        "border-right: 15px solid transparent; ",
                        "border-top: 20px solid #007bff; ",
                        "filter: drop-shadow(0 2px 4px rgba(0,0,0,0.2));",
                        "'></div>",
                        "</div>"
                    )
                }
            }
            
            # Close flowchart container
            html_content <- paste0(html_content, "</div>")
            
            # Add summary statistics
            if (n_nodes > 1) {
                total_participants <- nodeData$count[1]
                final_participants <- nodeData$count[n_nodes]
                overall_retention <- round((final_participants / total_participants) * 100, 1)
                total_excluded <- total_participants - final_participants
                
                html_content <- paste0(html_content,
                    "<div style='margin-top: 30px; padding: 20px; background: #f8f9fa; border-radius: 8px; border-left: 4px solid #28a745;'>",
                    "<h4 style='color: #28a745; margin-top: 0; margin-bottom: 15px;'>📊 Flow Summary</h4>",
                    "<div style='display: grid; grid-template-columns: 1fr 1fr 1fr; gap: 15px; text-align: center;'>",
                    "<div>",
                    "<div style='font-size: 24px; font-weight: 700; color: #007bff;'>", format(total_participants, big.mark = ","), "</div>",
                    "<div style='font-size: 12px; color: #6c757d; font-weight: 600;'>TOTAL STARTED</div>",
                    "</div>",
                    "<div>",
                    "<div style='font-size: 24px; font-weight: 700; color: #28a745;'>", format(final_participants, big.mark = ","), "</div>",
                    "<div style='font-size: 12px; color: #6c757d; font-weight: 600;'>COMPLETED</div>",
                    "</div>",
                    "<div>",
                    "<div style='font-size: 24px; font-weight: 700; color: ", if (overall_retention >= 70) "#28a745" else if (overall_retention >= 50) "#ffc107" else "#dc3545", ";'>", overall_retention, "%</div>",
                    "<div style='font-size: 12px; color: #6c757d; font-weight: 600;'>RETENTION RATE</div>",
                    "</div>",
                    "</div>",
                    if (total_excluded > 0) {
                        paste0(
                            "<div style='text-align: center; margin-top: 15px; padding-top: 15px; border-top: 1px solid #dee2e6;'>",
                            "<span style='font-size: 14px; color: #6c757d;'>Total excluded: </span>",
                            "<span style='font-weight: 600; color: #dc3545;'>", format(total_excluded, big.mark = ","), "</span>",
                            "</div>"
                        )
                    } else {
                        ""
                    },
                    "</div>"
                )
            }
            
            # Close main container
            html_content <- paste0(html_content, "</div>")

            # Apply vartree's proven HTML document wrapper pattern
            # This prevents jamovi from escaping the HTML content
            wrapped_html <- paste0(
                '<html><head><style>',
                '#myDIV {width: 100%; max-width: 1000px; height: auto; min-height: 400px; overflow: auto; margin: 0 auto;}',
                '</style></head><body><div id="myDIV">',
                html_content,
                '</div></body></html>'
            )

            cat("Generated professional HTML flowchart, length:", nchar(wrapped_html), "\n")
            return(wrapped_html)
        },

        .plot = function(image, ggtheme, theme, ...) {
            # Debug information
            cat("Plot function called\n")
            cat("image$state is null:", is.null(image$state), "\n")
            
            # Check if we have plot state
            if (is.null(image$state)) {
                cat("No plot state available - analysis may not have run or variables missing\n")
                return(FALSE)
            }
            
            # Get data mode from state
            plot_mode <- image$state$mode
            cat("Plot mode from state:", plot_mode, "\n")
            
            # Render based on engine and data mode
            render_engine <- self$options$render_engine
            cat("Render engine:", render_engine, "\n")
            
            if (render_engine == "ggplot2") {
                result <- private$.plotGgplot2(image, ggtheme, theme, ...)
            } else if (render_engine == "flowchart_pkg") {
                result <- private$.plotFlowchartPkg(image, ggtheme, theme, ...)
            } else {
                result <- private$.plotDiagrammeR(image, ggtheme, theme, ...)
            }
            
            return(result)
        },

        .plotDiagrammeR = function(image, ggtheme, theme, ...) {
            cat("DiagrammeR plot function called\n")
            
            # Get plot mode from state instead of private variable
            plot_mode <- image$state$mode
            cat("Plot mode from state:", plot_mode, "\n")
            
            if (plot_mode == "node_count") {
                nodeData <- image$state$nodeData
                cat("NodeData rows:", nrow(nodeData), "\n")
                
                # Calculate flow statistics
                flow_stats <- list(
                    total_nodes = nrow(nodeData),
                    max_count = max(nodeData$count, na.rm = TRUE),
                    min_count = min(nodeData$count, na.rm = TRUE),
                    total_flow = sum(nodeData$count, na.rm = TRUE)
                )
                
                # Enhanced node styling based on count ranges
                n_nodes <- nrow(nodeData)
                max_count <- flow_stats$max_count
                min_count <- flow_stats$min_count
                
                # Calculate node colors based on selected scheme
                base_colors <- private$.getColorPalette()
                
                if (max_count > min_count) {
                    # Scale node colors by count (higher counts = more intense)
                    color_intensity <- (nodeData$count - min_count) / (max_count - min_count)
                    color_indices <- as.integer(color_intensity * (length(base_colors) - 1)) + 1
                    node_colors <- base_colors[color_indices]
                } else {
                    node_colors <- rep(base_colors[1], n_nodes)
                }
                
                # Enhanced positioning for better layout
                direction <- self$options$direction
                if (direction == "TB") {
                    # Top to bottom: center horizontally, space vertically
                    x_pos <- rep(2, n_nodes)
                    y_pos <- seq(n_nodes, 1, by = -1)  # Reverse order for top-down
                } else if (direction == "LR") {
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
                    if (self$options$showPercentages && i > 1 && nodeData$count[1] > 0) {
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
                    width = self$options$nodeWidth,
                    height = self$options$nodeHeight,
                    fontsize = self$options$fontSize,
                    fillcolor = node_colors,
                    color = "black",
                    fontcolor = "black",
                    style = "filled,rounded",
                    x = x_pos,
                    y = y_pos
                )
                
            } else {
                # Edge-list mode for DiagrammeR
                edgeData <- private$.edgeData
                from_var <- self$options$from_var
                to_var <- self$options$to_var
                
                # Create nodes from unique values
                all_nodes <- unique(c(edgeData[[from_var]], edgeData[[to_var]]))
                n_nodes <- length(all_nodes)
                
                # Create basic node data
                nodes_df <- DiagrammeR::create_node_df(
                    n = n_nodes,
                    type = "flow_node",
                    label = all_nodes,
                    shape = "rectangle",
                    width = self$options$nodeWidth,
                    height = self$options$nodeHeight,
                    fontsize = self$options$fontSize,
                    fillcolor = private$.getColorPalette()[1],
                    color = "black",
                    fontcolor = "black",
                    style = "filled,rounded"
                )
            }
            
            # Create base graph with enhanced attributes
            graph <- DiagrammeR::create_graph() %>%
                DiagrammeR::add_global_graph_attrs(
                    attr = "rankdir",
                    value = self$options$direction,
                    attr_type = "graph"
                ) %>%
                DiagrammeR::add_global_graph_attrs(
                    attr = "splines",
                    value = "ortho",  # Orthogonal edges for cleaner look
                    attr_type = "graph"
                )
            
            # Add nodes to graph
            graph <- graph %>%
                DiagrammeR::add_nodes_from_table(
                    nodes_df,
                    label_col = "label"
                )
            
            # Add edges based on mode
            if (plot_mode == "node_count") {
                nodeData <- image$state$nodeData
                n_nodes <- nrow(nodeData)
                
                if (n_nodes > 1) {
                    # Calculate exclusion counts for edge labels if requested
                    edge_labels <- character(n_nodes - 1)
                    if (self$options$showExclusions) {
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
                        label = edge_labels
                    )
                    
                    graph <- graph %>%
                        DiagrammeR::add_edges_from_table(edges_df)
                }
            } else {
                # Edge-list mode edges
                edgeData <- private$.edgeData
                from_var <- self$options$from_var
                to_var <- self$options$to_var
                
                all_nodes <- unique(c(edgeData[[from_var]], edgeData[[to_var]]))
                
                # Create edge dataframe
                from_indices <- match(edgeData[[from_var]], all_nodes)
                to_indices <- match(edgeData[[to_var]], all_nodes)
                
                edges_df <- DiagrammeR::create_edge_df(
                    from = from_indices,
                    to = to_indices
                )
                
                graph <- graph %>%
                    DiagrammeR::add_edges_from_table(edges_df)
            }
            
            # Add title if requested
            if (self$options$includeTitle) {
                plot_title <- self$options$plot_title
                
                graph <- graph %>%
                    DiagrammeR::add_global_graph_attrs(
                        attr = "label",
                        value = plot_title,
                        attr_type = "graph"
                    ) %>%
                    DiagrammeR::add_global_graph_attrs(
                        attr = "labelloc",
                        value = "t",
                        attr_type = "graph"
                    )
            }
            
            # Debug graph structure before rendering
            cat("Graph created successfully\n")
            cat("Number of nodes in graph:", DiagrammeR::count_nodes(graph), "\n")
            cat("Number of edges in graph:", DiagrammeR::count_edges(graph), "\n")
            
            # Render graph
            cat("Attempting to render graph...\n")
            plot_result <- DiagrammeR::render_graph(graph)
            cat("Graph rendered, result type:", class(plot_result), "\n")
            cat("Plot result is null:", is.null(plot_result), "\n")
            
            if (!is.null(plot_result)) {
                cat("Returning plot result\n")
                return(plot_result)
            } else {
                cat("Plot result is null, returning FALSE\n")
                return(FALSE)
            }
        },

        .plotGgplot2 = function(image, ggtheme, theme, ...) {
            
            # Require ggflowchart
            if (!requireNamespace("ggflowchart")) {
                stop("The ggflowchart package is required for ggplot2 rendering. Please install it using: install.packages('ggflowchart') or from GitHub: remotes::install_github('nrennie/ggflowchart')")
            }
            
            if (private$.dataMode == "edge_list") {
                return(private$.plotGgplot2EdgeList(image, ggtheme, theme, ...))
            } else {
                return(private$.plotGgplot2NodeCount(image, ggtheme, theme, ...))
            }
        },

        .plotGgplot2EdgeList = function(image, ggtheme, theme, ...) {
            edgeData <- private$.edgeData
            from_var <- self$options$from_var
            to_var <- self$options$to_var
            group_var <- self$options$group_var
            
            # Prepare flowchart data following minimal example pattern
            flowchart_data <- data.frame(
                from = as.character(edgeData[[from_var]]),
                to = as.character(edgeData[[to_var]]),
                stringsAsFactors = FALSE
            )
            
            # Add edge labels if there's a suitable column in the data
            # Look for columns that might contain edge labels or descriptions
            potential_label_cols <- names(edgeData)[!names(edgeData) %in% c(from_var, to_var, group_var)]
            if (length(potential_label_cols) > 0) {
                # Use the first non-grouping variable as potential labels
                label_col <- potential_label_cols[1]
                edge_labels <- as.character(edgeData[[label_col]])
                
                # Only add labels if they're not all the same and not all NA
                if (length(unique(edge_labels[!is.na(edge_labels)])) > 1) {
                    flowchart_data$label <- edge_labels
                }
            }
            
            # Remove any rows with missing values (as per documentation best practices)
            complete_rows <- complete.cases(flowchart_data)
            if (!all(complete_rows)) {
                warning(paste("Removed", sum(!complete_rows), "rows with missing values from flowchart data"))
                flowchart_data <- flowchart_data[complete_rows, ]
            }
            
            # Check for valid data
            if (nrow(flowchart_data) == 0) {
                warning("No valid connections found for flowchart")
                return(FALSE)
            }
            
            # Get styling options
            arrow_color <- private$.getArrowColor()
            font_family <- private$.getFontFamily()
            x_nudge <- self$options$nodeSpacing
            
            # Check if node typing is enabled for decision tree functionality
            node_typing_enabled <- self$options$nodeTyping
            node_alpha <- self$options$nodeAlpha
            
            # Apply intelligent layout selection if enabled
            layout_data <- private$.applyLayoutStrategy(flowchart_data, edgeData, group_var)
            
            if (node_typing_enabled) {
                # Create decision tree with typed nodes
                node_data <- private$.createDecisionTreeAesthetics(layout_data, edgeData, group_var)
                
                p <- ggflowchart::ggflowchart(
                    layout_data, 
                    node_data = node_data,
                    arrow_colour = arrow_color,
                    family = font_family,
                    x_nudge = x_nudge,
                    horizontal = private$.getLayoutOrientation()
                )
                
                # Add transparency if specified
                if (node_alpha < 1.0) {
                    p <- p + ggplot2::geom_rect(alpha = node_alpha)
                }
                
            } else if (!is.null(group_var) && group_var != "") {
                # Traditional grouping approach
                node_data <- private$.createNodeAesthetics(layout_data, edgeData, group_var)
                
                p <- ggflowchart::ggflowchart(
                    layout_data, 
                    node_data = node_data,
                    colour = node_data$fill[1],
                    arrow_colour = arrow_color,
                    family = font_family,
                    x_nudge = x_nudge,
                    horizontal = private$.getLayoutOrientation()
                )
            } else {
                # Basic flowchart with enhanced options
                colors <- private$.getColorPalette()
                node_color <- colors[1]
                
                p <- ggflowchart::ggflowchart(
                    layout_data,
                    colour = node_color,
                    text_colour = private$.getTextColor(node_color),
                    arrow_colour = arrow_color,
                    family = font_family,
                    x_nudge = x_nudge,
                    horizontal = private$.getLayoutOrientation()
                )
            }
            
            # Apply enhanced theming
            p <- private$.applyGgplot2Theme(p)
            
            print(p)
            return(TRUE)
        },

        .plotGgplot2NodeCount = function(image, ggtheme, theme, ...) {
            nodeData <- private$.nodeData
            n_nodes <- nrow(nodeData)
            
            if (n_nodes <= 1) {
                stop("ggplot2 rendering requires at least 2 nodes for edge connections")
            }
            
            # Create sequential edges for node-count mode
            flowchart_data <- data.frame(
                from = nodeData$label[1:(n_nodes-1)],
                to = nodeData$label[2:n_nodes],
                stringsAsFactors = FALSE
            )
            
            # Create enhanced node data with count information and colors
            all_nodes <- unique(c(flowchart_data$from, flowchart_data$to))
            colors <- private$.getColorPalette()
            
            # Create node aesthetics based on participant counts
            node_counts <- setNames(nodeData$count, nodeData$label)
            max_count <- max(nodeData$count, na.rm = TRUE)
            min_count <- min(nodeData$count, na.rm = TRUE)
            
            # Color intensity based on count (higher counts = more intense colors)
            if (max_count > min_count) {
                color_intensities <- (node_counts[all_nodes] - min_count) / (max_count - min_count)
                color_indices <- pmax(1, pmin(length(colors), round(color_intensities * length(colors))))
                node_colors <- colors[color_indices]
            } else {
                node_colors <- rep(colors[1], length(all_nodes))
            }
            
            # Create enhanced node data
            node_data <- data.frame(
                name = all_nodes,
                fill = node_colors,
                text_colour = "white",  # Better contrast for clinical data
                stringsAsFactors = FALSE
            )
            
            # Add count information to node labels for clinical context (if enabled)
            if (self$options$showCounts) {
                enhanced_labels <- character(length(all_nodes))
                for (i in seq_along(all_nodes)) {
                    node_name <- all_nodes[i]
                    count <- node_counts[node_name]
                    if (!is.na(count)) {
                        enhanced_labels[i] <- paste0(node_name, "\n(n=", count, ")")
                    } else {
                        enhanced_labels[i] <- node_name
                    }
                }
                node_data$name <- enhanced_labels
            } else {
                node_data$name <- all_nodes
            }
            
            # Get styling options
            arrow_color <- private$.getArrowColor()
            font_family <- private$.getFontFamily()
            x_nudge <- self$options$nodeSpacing
            node_typing_enabled <- self$options$nodeTyping
            node_alpha <- self$options$nodeAlpha
            
            # Apply decision tree styling to node-count mode if enabled
            if (node_typing_enabled) {
                # Detect node types based on labels and apply decision tree coloring
                node_types <- private$.detectNodeTypes(flowchart_data, node_data$name)
                
                # Apply type-based colors
                decision_color <- private$.getDecisionNodeColor()
                outcome_color <- private$.getOutcomeNodeColor()
                
                node_data$fill <- sapply(node_types, function(type) {
                    if (type == "decision") {
                        return(decision_color)
                    } else if (type == "outcome") {
                        return(outcome_color)
                    } else {
                        return(node_data$fill[1])  # Keep original color for intermediate
                    }
                })
                
                # Recalculate text colors
                node_data$text_colour <- sapply(node_data$fill, function(color) {
                    private$.getTextColor(color)
                })
            }
            
            # Apply layout strategy to node-count data
            layout_data <- private$.applyLayoutStrategy(flowchart_data, private$.edgeData, NULL)
            
            # Create flowchart with enhanced node data - following minimal example pattern
            p <- ggflowchart::ggflowchart(
                layout_data, 
                node_data = node_data,
                arrow_colour = arrow_color,
                family = font_family,
                x_nudge = x_nudge,
                horizontal = private$.getLayoutOrientation()
            )
            
            # Add transparency if specified
            if (node_alpha < 1.0) {
                p <- p + ggplot2::geom_rect(alpha = node_alpha)
            }
            
            # Apply enhanced theming
            p <- private$.applyGgplot2Theme(p)
            
            print(p)
            return(TRUE)
        },

        .createNodeAesthetics = function(flowchart_data, edgeData, group_var) {
            # Create comprehensive node aesthetics based on grouping
            all_nodes <- unique(c(flowchart_data$from, flowchart_data$to))
            
            # Initialize node groups
            node_groups <- data.frame(
                node = all_nodes,
                group = "Default",
                stringsAsFactors = FALSE
            )
            
            # Assign groups based on edge data
            for (i in 1:nrow(edgeData)) {
                from_node <- as.character(edgeData[[self$options$from_var]][i])
                to_node <- as.character(edgeData[[self$options$to_var]][i])
                group_val <- as.character(edgeData[[group_var]][i])
                
                # Update group for both nodes in the connection
                node_groups[node_groups$node == from_node, "group"] <- group_val
                node_groups[node_groups$node == to_node, "group"] <- group_val
            }
            
            # Create color mapping
            colors <- private$.getColorPalette()
            unique_groups <- unique(node_groups$group)
            n_groups <- length(unique_groups)
            
            # Ensure we have enough colors
            if (n_groups > length(colors)) {
                colors <- rep(colors, ceiling(n_groups / length(colors)))
            }
            
            group_color_map <- setNames(colors[1:n_groups], unique_groups)
            
            # Create enhanced node data with both fill and text aesthetics
            node_data <- data.frame(
                name = node_groups$node,
                fill = group_color_map[node_groups$group],
                text_colour = ifelse(
                    node_groups$group == "Default", 
                    "black", 
                    "white"  # Better contrast for colored nodes
                ),
                stringsAsFactors = FALSE
            )
            
            # Add advanced styling enhancements
            node_data <- private$.enhanceNodeData(node_data, flowchart_data, edgeData)
            
            return(node_data)
        },

        .createDecisionTreeAesthetics = function(flowchart_data, edgeData, group_var) {
            # Create decision tree aesthetics with automatic node typing
            all_nodes <- unique(c(flowchart_data$from, flowchart_data$to))
            
            # Detect node types based on patterns and connections
            node_types <- private$.detectNodeTypes(flowchart_data, all_nodes)
            
            # Create comprehensive node data structure following documentation
            node_data <- data.frame(
                name = all_nodes,
                type = node_types,
                stringsAsFactors = FALSE
            )
            
            # Apply type-based colors
            decision_color <- private$.getDecisionNodeColor()
            outcome_color <- private$.getOutcomeNodeColor()
            default_colors <- private$.getColorPalette()
            
            node_data$fill <- sapply(node_data$type, function(type) {
                if (type == "decision") {
                    return(decision_color)
                } else if (type == "outcome") {
                    return(outcome_color)
                } else {
                    return(default_colors[1])  # Default for intermediate nodes
                }
            })
            
            # Calculate appropriate text colors
            node_data$text_colour <- sapply(node_data$fill, function(color) {
                private$.getTextColor(color)
            })
            
            # Add advanced styling based on options
            node_data <- private$.enhanceNodeData(node_data, flowchart_data, edgeData)
            
            return(node_data)
        },

        .detectNodeTypes = function(flowchart_data, all_nodes) {
            # Automatic node type detection for decision trees
            node_types <- rep("intermediate", length(all_nodes))
            names(node_types) <- all_nodes
            
            # Count outgoing connections for each node
            outgoing_counts <- table(flowchart_data$from)
            incoming_counts <- table(flowchart_data$to)
            
            for (node in all_nodes) {
                outgoing <- ifelse(node %in% names(outgoing_counts), outgoing_counts[node], 0)
                incoming <- ifelse(node %in% names(incoming_counts), incoming_counts[node], 0)
                
                # Decision nodes: typically have multiple outgoing connections
                # Often contain question words or decision indicators
                if (outgoing >= 2 || private$.containsDecisionKeywords(node)) {
                    node_types[node] <- "decision"
                }
                # Outcome nodes: typically have no outgoing connections (terminal nodes)
                # Or contain outcome/result keywords
                else if (outgoing == 0 || private$.containsOutcomeKeywords(node)) {
                    node_types[node] <- "outcome"
                }
                # Otherwise, it's an intermediate processing node
            }
            
            return(node_types)
        },

        .containsDecisionKeywords = function(node_text) {
            # Check if node text contains decision-related keywords
            decision_keywords <- c("\\?", "decision", "choose", "if", "whether", "assess", "evaluate", "check", "test", "screen")
            node_lower <- tolower(as.character(node_text))
            return(any(sapply(decision_keywords, function(kw) grepl(kw, node_lower))))
        },

        .containsOutcomeKeywords = function(node_text) {
            # Check if node text contains outcome-related keywords
            outcome_keywords <- c("result", "outcome", "end", "final", "complete", "finished", "diagnosis", "treatment", "positive", "negative")
            node_lower <- tolower(as.character(node_text))
            return(any(sapply(outcome_keywords, function(kw) grepl(kw, node_lower))))
        },

        .enhanceNodeData = function(node_data, flowchart_data, edgeData) {
            # Comprehensive node enhancement following ggflowchart documentation
            
            # Add custom sizing if enabled
            if (self$options$customNodeSizing) {
                node_data <- private$.addCustomSizing(node_data, flowchart_data, edgeData)
            }
            
            # Add advanced positioning if enabled
            if (self$options$advancedPositioning) {
                node_data <- private$.addAdvancedPositioning(node_data, flowchart_data)
            }
            
            # Add text size scaling if enabled
            text_size_option <- self$options$nodeTextSize
            if (text_size_option != "fixed") {
                node_data <- private$.addTextSizeScaling(node_data, text_size_option, edgeData)
            }
            
            return(node_data)
        },

        .addCustomSizing = function(node_data, flowchart_data, edgeData) {
            # Add custom node sizing based on content length or values
            
            # Calculate base sizing factors
            text_lengths <- nchar(as.character(node_data$name))
            max_length <- max(text_lengths, na.rm = TRUE)
            min_length <- min(text_lengths, na.rm = TRUE)
            
            if (max_length > min_length) {
                # Scale nudging based on text length (following documentation pattern)
                x_scaling <- (text_lengths - min_length) / (max_length - min_length)
                y_scaling <- x_scaling * 0.5  # More subtle vertical scaling
                
                # Apply scaling with reasonable bounds
                node_data$x_nudge <- 0.2 + (x_scaling * 0.6)  # Range: 0.2 to 0.8
                node_data$y_nudge <- 0.1 + (y_scaling * 0.3)  # Range: 0.1 to 0.4
            } else {
                # Default values when all text is similar length
                node_data$x_nudge <- 0.5
                node_data$y_nudge <- 0.2
            }
            
            return(node_data)
        },

        .addAdvancedPositioning = function(node_data, flowchart_data) {
            # Add fine-tuned positioning based on flowchart structure
            
            # Analyze node connections for intelligent positioning
            all_nodes <- node_data$name
            
            # Count connections for each node
            outgoing_counts <- table(flowchart_data$from)
            incoming_counts <- table(flowchart_data$to)
            
            # Create positioning adjustments
            x_adjustments <- numeric(length(all_nodes))
            y_adjustments <- numeric(length(all_nodes))
            
            for (i in seq_along(all_nodes)) {
                node <- all_nodes[i]
                
                outgoing <- ifelse(node %in% names(outgoing_counts), outgoing_counts[node], 0)
                incoming <- ifelse(node %in% names(incoming_counts), incoming_counts[node], 0)
                
                # Nodes with more connections get more spacing
                connection_factor <- (outgoing + incoming) / max(4, max(outgoing_counts, incoming_counts))
                
                # Apply subtle positioning adjustments
                x_adjustments[i] <- connection_factor * 0.3
                y_adjustments[i] <- connection_factor * 0.2
            }
            
            # Add or update nudge columns
            if ("x_nudge" %in% names(node_data)) {
                node_data$x_nudge <- node_data$x_nudge + x_adjustments
                node_data$y_nudge <- node_data$y_nudge + y_adjustments
            } else {
                node_data$x_nudge <- x_adjustments
                node_data$y_nudge <- y_adjustments
            }
            
            return(node_data)
        },

        .addTextSizeScaling = function(node_data, scaling_option, edgeData) {
            # Add text size scaling based on content or values
            
            if (scaling_option == "content") {
                # Scale by text content length
                text_lengths <- nchar(as.character(node_data$name))
                max_length <- max(text_lengths, na.rm = TRUE)
                min_length <- min(text_lengths, na.rm = TRUE)
                
                if (max_length > min_length) {
                    # Normalize to reasonable text size range
                    size_factor <- (text_lengths - min_length) / (max_length - min_length)
                    node_data$text_size <- 10 + (size_factor * 4)  # Range: 10-14pt
                } else {
                    node_data$text_size <- 12  # Default size
                }
                
            } else if (scaling_option == "value" && !is.null(edgeData)) {
                # Scale by data values if available
                # Look for numeric columns that might represent importance/weight
                numeric_cols <- sapply(edgeData, is.numeric)
                
                if (any(numeric_cols)) {
                    # Use first numeric column for scaling
                    value_col <- names(edgeData)[which(numeric_cols)[1]]
                    values <- edgeData[[value_col]]
                    
                    # Map values to nodes (use mean if multiple values per node)
                    from_var <- self$options$from_var
                    node_values <- numeric(nrow(node_data))
                    
                    for (i in seq_along(node_data$name)) {
                        node <- node_data$name[i]
                        node_rows <- which(edgeData[[from_var]] == node)
                        if (length(node_rows) > 0) {
                            node_values[i] <- mean(values[node_rows], na.rm = TRUE)
                        }
                    }
                    
                    # Normalize and apply scaling
                    max_val <- max(node_values, na.rm = TRUE)
                    min_val <- min(node_values, na.rm = TRUE)
                    
                    if (max_val > min_val) {
                        size_factor <- (node_values - min_val) / (max_val - min_val)
                        node_data$text_size <- 10 + (size_factor * 6)  # Range: 10-16pt
                    } else {
                        node_data$text_size <- 12
                    }
                } else {
                    node_data$text_size <- 12  # Default when no numeric data
                }
            }
            
            return(node_data)
        },

        .applyGgplot2Theme = function(p) {
            plot_title <- self$options$plot_title
            enhanced_styling <- self$options$enhancedStyling
            ggplot_integration <- self$options$ggplotIntegration
            
            # Apply ggplot2 scales and aesthetics if full integration is enabled
            if (ggplot_integration) {
                p <- private$.addGgplotScales(p)
            }
            
            if (enhanced_styling) {
                # Apply comprehensive modern theme with enhanced styling
                p <- p + 
                    ggplot2::theme_minimal() +
                    ggplot2::theme(
                        plot.title = ggplot2::element_text(
                            size = 16, 
                            face = "bold", 
                            hjust = 0.5,
                            margin = ggplot2::margin(b = 20),
                            color = "#2c3e50"
                        ),
                        plot.subtitle = ggplot2::element_text(
                            size = 12, 
                            hjust = 0.5,
                            color = "gray60",
                            margin = ggplot2::margin(b = 20),
                            style = "italic"
                        ),
                        legend.position = "bottom",
                        legend.title = ggplot2::element_text(face = "bold", size = 11),
                        legend.text = ggplot2::element_text(size = 10),
                        legend.box = "horizontal",
                        panel.grid = ggplot2::element_blank(),
                        panel.background = ggplot2::element_blank(),
                        plot.background = ggplot2::element_rect(fill = "white", color = NA),
                        plot.margin = ggplot2::margin(25, 25, 25, 25),
                        panel.border = ggplot2::element_blank(),
                        axis.line = ggplot2::element_blank(),
                        axis.text = ggplot2::element_blank(),
                        axis.ticks = ggplot2::element_blank()
                    ) +
                    ggplot2::labs(
                        title = plot_title,
                        subtitle = paste(
                            "Generated with ggflowchart •",
                            if (private$.dataMode == "edge_list") "Edge-List Mode" else "Node-Count Mode",
                            "• Enhanced Styling"
                        ),
                        caption = "Created with ClinicoPath jamovi module"
                    )
            } else {
                # Apply basic clean theme
                p <- p + 
                    ggplot2::theme_minimal() +
                    ggplot2::theme(
                        plot.title = ggplot2::element_text(
                            size = 14, 
                            face = "bold", 
                            hjust = 0.5
                        ),
                        legend.position = "bottom",
                        panel.grid = ggplot2::element_blank()
                    ) +
                    ggplot2::labs(title = plot_title)
            }
            
            return(p)
        },

        .addGgplotScales = function(p) {
            # Add comprehensive ggplot2 scale integration following documentation
            
            # Check if we're using decision tree mode for special handling
            node_typing <- self$options$nodeTyping
            
            if (node_typing) {
                # Add discrete scales for decision tree categories
                p <- p + ggplot2::scale_fill_manual(
                    name = "Node Type",
                    values = c(
                        "decision" = private$.getDecisionNodeColor(),
                        "outcome" = private$.getOutcomeNodeColor(),
                        "intermediate" = private$.getColorPalette()[1]
                    ),
                    labels = c(
                        "decision" = "Decision Point",
                        "outcome" = "Outcome/Result", 
                        "intermediate" = "Process Step"
                    )
                )
            } else {
                # Use the current color palette for regular nodes
                colors <- private$.getColorPalette()
                palette_name <- self$options$nodeColor
                
                if (palette_name == "viridis") {
                    p <- p + ggplot2::scale_fill_viridis_d(name = "Node Group")
                } else if (palette_name == "set1") {
                    p <- p + ggplot2::scale_fill_brewer(
                        name = "Node Group", 
                        palette = "Set1"
                    )
                } else {
                    p <- p + ggplot2::scale_fill_manual(
                        name = "Node Group",
                        values = colors
                    )
                }
            }
            
            # Add text color scale if needed
            p <- p + ggplot2::scale_color_identity()
            
            return(p)
        },

        .getColorPalette = function() {
            palette_name <- self$options$nodeColor
            
            if (palette_name == "blue") {
                return(c("#2E86AB", "#A23B72", "#F18F01", "#C73E1D", "#593E2C", "#8E6C8A"))
            } else if (palette_name == "gray") {
                return(c("#E8E8E8", "#D3D3D3", "#B8B8B8", "#A0A0A0", "#888888", "#707070"))
            } else if (palette_name == "green") {
                return(c("#4CAF50", "#8BC34A", "#CDDC39", "#FFC107", "#FF9800", "#FF5722"))
            } else if (palette_name == "viridis") {
                return(viridis::viridis(8, discrete = TRUE))
            } else if (palette_name == "set1") {
                return(RColorBrewer::brewer.pal(min(8, max(3, 8)), "Set1"))
            } else if (palette_name == "pastel") {
                return(c("#FFB3BA", "#BAFFC9", "#BAE1FF", "#FFFFBA", "#FFDFBA", "#E0BBE4", "#DDA0DD", "#98FB98"))
            } else if (palette_name == "gradient_blue") {
                # Enhanced gradient blues for clinical context
                return(c("#E3F2FD", "#BBDEFB", "#90CAF9", "#64B5F6", "#42A5F5", "#2196F3", "#1976D2", "#0D47A1"))
            } else if (palette_name == "clinical_warm") {
                # Warm clinical colors
                return(c("#FFF3E0", "#FFE0B2", "#FFCC02", "#FF9800", "#F57C00", "#E65100", "#D84315", "#BF360C"))
            } else {
                # Default clinical blue
                return(c("#2E86AB", "#A23B72", "#F18F01", "#C73E1D", "#593E2C", "#8E6C8A"))
            }
        },

        .getArrowColor = function() {
            arrow_option <- self$options$arrowColor
            
            if (arrow_option == "darkgray") {
                return("#555555")
            } else if (arrow_option == "black") {
                return("#000000")
            } else if (arrow_option == "clinical_blue") {
                return("#2E86AB")
            } else if (arrow_option == "match_nodes") {
                # Return the first color from the current palette
                colors <- private$.getColorPalette()
                return(colors[1])
            } else {
                return("#555555")  # Default dark gray
            }
        },

        .getFontFamily = function() {
            font_option <- self$options$fontFamily
            
            if (font_option == "default") {
                return("")  # Use system default
            } else if (font_option %in% c("Arial", "Times", "Courier", "Helvetica")) {
                return(font_option)
            } else {
                return("")  # Fallback to default
            }
        },

        .getTextColor = function(node_color) {
            # Calculate appropriate text color based on node color brightness
            # Convert hex to RGB
            if (substr(node_color, 1, 1) == "#") {
                node_color <- substr(node_color, 2, 7)
            }
            
            # Convert hex to RGB values
            r <- strtoi(substr(node_color, 1, 2), 16)
            g <- strtoi(substr(node_color, 3, 4), 16)
            b <- strtoi(substr(node_color, 5, 6), 16)
            
            # Calculate perceived brightness
            brightness <- (r * 0.299 + g * 0.587 + b * 0.114)
            
            # Return white text for dark backgrounds, black for light
            return(if (brightness < 128) "white" else "black")
        },

        .getDecisionNodeColor = function() {
            decision_option <- self$options$decisionNodeColor
            
            if (decision_option == "clinical_red") {
                return("#DC3545")  # Bootstrap danger red
            } else if (decision_option == "warning_orange") {
                return("#FD7E14")  # Bootstrap warning orange
            } else if (decision_option == "decision_blue") {
                return("#0D6EFD")  # Bootstrap primary blue
            } else if (decision_option == "same_primary") {
                colors <- private$.getColorPalette()
                return(colors[1])
            } else {
                return("#DC3545")  # Default clinical red
            }
        },

        .getOutcomeNodeColor = function() {
            outcome_option <- self$options$outcomeNodeColor
            
            if (outcome_option == "success_green") {
                return("#198754")  # Bootstrap success green
            } else if (outcome_option == "neutral_gray") {
                return("#6C757D")  # Bootstrap secondary gray
            } else if (outcome_option == "result_blue") {
                return("#0DCAF0")  # Bootstrap info cyan
            } else if (outcome_option == "same_primary") {
                colors <- private$.getColorPalette()
                return(colors[1])
            } else {
                return("#198754")  # Default success green
            }
        },

        .applyLayoutStrategy = function(flowchart_data, edgeData, group_var) {
            # Comprehensive layout strategy application following documentation
            
            # Check if intelligent layout selection is enabled
            intelligent_layout <- self$options$intelligentLayout
            layout_algorithm <- self$options$layoutAlgorithm
            
            if (intelligent_layout) {
                # Analyze flowchart structure for optimal layout selection
                layout_algorithm <- private$.selectOptimalLayout(flowchart_data, edgeData)
            }
            
            # Apply the selected layout algorithm
            if (layout_algorithm == "custom") {
                return(private$.applyCustomLayout(flowchart_data, edgeData))
            } else if (layout_algorithm != "tree") {
                # Use igraph-based layouts for non-default algorithms
                return(private$.applyIgraphLayout(flowchart_data, layout_algorithm))
            } else {
                # Use default tree layout with spread factor adjustment
                return(private$.applyTreeLayout(flowchart_data))
            }
        },

        .selectOptimalLayout = function(flowchart_data, edgeData) {
            # Intelligent layout selection based on flowchart characteristics
            
            # Analyze structure characteristics
            unique_nodes <- unique(c(flowchart_data$from, flowchart_data$to))
            n_nodes <- length(unique_nodes)
            n_edges <- nrow(flowchart_data)
            
            # Count branching patterns
            outgoing_counts <- table(flowchart_data$from)
            max_branching <- max(outgoing_counts)
            avg_branching <- mean(outgoing_counts)
            
            # Decision logic for optimal layout
            if (n_nodes <= 5) {
                # Small flowcharts work well with star or circle
                return(if (max_branching >= 3) "star" else "circle")
            } else if (max_branching >= 4 && avg_branching >= 2) {
                # High branching suggests tree layout
                return("tree")
            } else if (n_edges / n_nodes > 1.5) {
                # Dense connections suggest nicely layout
                return("nicely")
            } else {
                # Linear or simple structures use tree
                return("tree")
            }
        },

        .applyIgraphLayout = function(flowchart_data, algorithm) {
            # Apply igraph-based layout algorithms
            
            # Check if igraph is available
            if (!requireNamespace("igraph", quietly = TRUE)) {
                warning("igraph package required for advanced layouts. Using default tree layout.")
                return(flowchart_data)
            }
            
            tryCatch({
                # Create igraph object
                g <- igraph::graph_from_data_frame(flowchart_data, directed = TRUE)
                
                # Apply the specified layout algorithm
                if (algorithm == "nicely") {
                    layout <- igraph::layout_nicely(g)
                } else if (algorithm == "star") {
                    layout <- igraph::layout_as_star(g)
                } else if (algorithm == "circle") {
                    layout <- igraph::layout_in_circle(g)
                } else {
                    # Fallback to nicely for unknown algorithms
                    layout <- igraph::layout_nicely(g)
                }
                
                # Apply spread factor
                spread_factor <- self$options$nodeSpreadFactor
                layout <- layout * spread_factor
                
                # Add coordinates to flowchart data
                node_names <- igraph::V(g)$name
                coords_df <- data.frame(
                    node = node_names,
                    x = layout[, 1],
                    y = layout[, 2],
                    stringsAsFactors = FALSE
                )
                
                # Merge coordinates back into flowchart data (optional enhancement)
                # For now, return original data as ggflowchart handles layout internally
                return(flowchart_data)
                
            }, error = function(e) {
                warning(paste("Error applying igraph layout:", e$message, "Using default layout."))
                return(flowchart_data)
            })
        },

        .applyCustomLayout = function(flowchart_data, edgeData) {
            # Apply custom layout based on clinical flowchart patterns
            
            # For clinical flowcharts, create structured positioning
            unique_nodes <- unique(c(flowchart_data$from, flowchart_data$to))
            n_nodes <- length(unique_nodes)
            
            # Detect if this is a clinical decision tree or linear process
            outgoing_counts <- table(flowchart_data$from)
            incoming_counts <- table(flowchart_data$to)
            
            # Create custom coordinates based on node roles
            coords <- data.frame(
                node = unique_nodes,
                x = numeric(n_nodes),
                y = numeric(n_nodes),
                stringsAsFactors = FALSE
            )
            
            # Apply clinical layout logic
            for (i in seq_along(unique_nodes)) {
                node <- unique_nodes[i]
                
                outgoing <- ifelse(node %in% names(outgoing_counts), outgoing_counts[node], 0)
                incoming <- ifelse(node %in% names(incoming_counts), incoming_counts[node], 0)
                
                # Decision nodes (high outgoing) get central positioning
                if (outgoing >= 2) {
                    coords$x[i] <- 0  # Center
                    coords$y[i] <- i * 0.5  # Vertical spacing
                } else if (incoming == 0) {
                    # Start nodes
                    coords$x[i] <- -1
                    coords$y[i] <- 0
                } else if (outgoing == 0) {
                    # End nodes
                    coords$x[i] <- 1
                    coords$y[i] <- i * 0.3
                } else {
                    # Intermediate nodes
                    coords$x[i] <- 0
                    coords$y[i] <- i * 0.4
                }
            }
            
            # Apply spread factor
            spread_factor <- self$options$nodeSpreadFactor
            coords$x <- coords$x * spread_factor
            coords$y <- coords$y * spread_factor
            
            # For now, return original data as coordinate specification needs integration
            return(flowchart_data)
        },

        .applyTreeLayout = function(flowchart_data) {
            # Apply enhanced tree layout with spread factor
            # Since ggflowchart handles tree layout internally, we just return the data
            # The spread factor will be applied via other parameters
            return(flowchart_data)
        },

        .getLayoutOrientation = function() {
            # Get layout orientation setting
            orientation <- self$options$layoutOrientation
            return(orientation == "horizontal")
        },

        .plotFlowchartPkg = function(image, ggtheme, theme, ...) {
            # Clinical trial flow mode using flowchart package
            
            # Check if flowchart package is available
            if (!requireNamespace("flowchart")) {
                stop("flowchart package is required for Clinical Flow mode. Please install it using: install.packages('flowchart')")
            }
            
            if (private$.dataMode == "clinical_flow") {
                    flow_data <- private$.clinicalFlowData
                    
                    # Convert data to flowchart format
                    fc_data <- flow_data$data
                    
                    # Standard clinical flow creation
                    initial_label <- paste("Participants assessed for eligibility (N =", flow_data$total_participants, ")")
                    fc_obj <- fc_data %>% flowchart::as_fc(label = initial_label)
                    
                    # Apply standard filters and splits
                    fc_obj <- private$.applyStandardFilters(fc_obj, flow_data)
                    
                    # Draw the flowchart
                    plot_obj <- fc_obj %>% flowchart::fc_draw()
                    
                    # Add title if requested
                    if (self$options$includeTitle && nchar(self$options$plot_title) > 0) {
                        plot_obj <- plot_obj + 
                            ggplot2::labs(title = self$options$plot_title)
                    }
                    
                    print(plot_obj)
                    
                } else {
                    # For other data modes, create basic flowchart from available data
                    warning("Clinical Flow mode works best with clinical_flow data mode.")
                    
                    # Create a simple flowchart from available data
                    if (private$.dataMode == "node_count") {
                        node_data <- private$.nodeData
                        total_count <- sum(node_data$count, na.rm = TRUE)
                        
                        # Create simple flowchart
                        simple_data <- data.frame(id = 1:total_count)
                        fc_obj <- simple_data %>% 
                            flowchart::as_fc(label = paste("Total Cases (N =", total_count, ")")) %>%
                            flowchart::fc_draw()
                        
                        print(fc_obj)
                    }
                }
                
                return(TRUE)
        },

        .applyStandardFilters = function(fc_obj, flow_data) {
            # Apply standard filters and splits
            filter_exprs <- flow_data$filter_expressions
            filter_labels <- flow_data$filter_labels
            
            if (nchar(filter_exprs) > 0 && nchar(filter_labels) > 0) {
                expr_list <- strsplit(filter_exprs, ",")[[1]]
                label_list <- strsplit(filter_labels, ",")[[1]]
                
                # Apply each filter
                for (i in seq_along(expr_list)) {
                    if (i <= length(label_list)) {
                        expr <- trimws(expr_list[i])
                        label <- trimws(label_list[i])
                        
                        # Parse and apply filter expression
                        if (nchar(expr) > 0) {
                            fc_obj <- fc_obj %>% 
                                flowchart::fc_filter(
                                    !!rlang::parse_expr(expr),
                                    label = label, 
                                    show_exc = self$options$show_exclusions_flow
                                )
                        }
                    }
                }
            }
            
            # Split by trial groups if specified
            if (!is.null(flow_data$trial_group_var) && flow_data$trial_group_var != "") {
                fc_obj <- fc_obj %>% 
                    flowchart::fc_split(!!rlang::sym(flow_data$trial_group_var))
            }
            
            return(fc_obj)
        }

    )
)