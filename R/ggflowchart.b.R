#' @title ggFlowchart - Modern Flowcharts with ggplot2
#' @return Modern flowcharts using ggflowchart package
#'
#' @importFrom R6 R6Class
#' @import jmvcore
#' @importFrom magrittr %>%
#' @importFrom ggplot2 ggplot aes labs theme theme_minimal
#' @importFrom ggplot2 scale_fill_manual element_text
#' @importFrom ggflowchart ggflowchart
#' @importFrom dplyr select
#' @importFrom RColorBrewer brewer.pal
#' @importFrom viridis viridis
#' @importFrom htmltools HTML

ggflowchartClass <- if (requireNamespace("jmvcore")) R6::R6Class("ggflowchartClass",
    inherit = ggflowchartBase,
    private = list(

        .run = function() {

            # Check if required variables have been selected
            if (is.null(self$options$from_var) || is.null(self$options$to_var)) {
                intro_msg <- "
                <div style='background-color: #e3f2fd; padding: 20px; border-radius: 8px; margin: 20px 0;'>
                <h3 style='color: #1976d2; margin-top: 0;'>🔀 Welcome to ggFlowchart!</h3>
                <p><strong>Modern flowcharts with ggplot2 styling</strong> using the ggflowchart package</p>
                <p>Alternative to DiagrammeR-based flowcharts with modern ggplot2 integration</p>
                
                <h4 style='color: #1976d2;'>Required Variables:</h4>
                <ol>
                <li><strong>From Node:</strong> Starting points for flowchart connections</li>
                <li><strong>To Node:</strong> Ending points for flowchart connections</li>
                </ol>
                
                <h4 style='color: #1976d2;'>What ggFlowchart Offers:</h4>
                <ul>
                <li><strong>ggplot2 Integration:</strong> Native ggplot2 aesthetics and themes</li>
                <li><strong>Modern Styling:</strong> Clean, contemporary flowchart appearance</li>
                <li><strong>Customizable Colors:</strong> Multiple color palette options</li>
                <li><strong>Simple Data Structure:</strong> Just 'from' and 'to' connections needed</li>
                <li><strong>Publication Ready:</strong> High-quality output for publications</li>
                </ul>
                
                <h4 style='color: #1976d2;'>Perfect For:</h4>
                <ul>
                <li><strong>Process Workflows:</strong> Simple process flow documentation</li>
                <li><strong>Decision Trees:</strong> Basic decision flow visualization</li>
                <li><strong>Study Design:</strong> Research workflow representation</li>
                <li><strong>Data Flow:</strong> Analysis pipeline visualization</li>
                </ul>
                
                <h4 style='color: #1976d2;'>Data Structure:</h4>
                <p>Your data should contain simple edge connections:</p>
                <ul>
                <li><strong>From Variable:</strong> Source node names</li>
                <li><strong>To Variable:</strong> Target node names</li>
                <li><strong>Optional Groups:</strong> Node categories for color coding</li>
                </ul>
                
                <p style='font-size: 12px; color: #555; margin-top: 20px;'>
                💡 <em>Modern alternative to traditional flowcharts with ggplot2 aesthetics</em>
                </p>
                </div>"
                
                self$results$todo$setContent(intro_msg)
                return()
            } else {
                self$results$todo$setContent("")
            }

            # Validate dataset
            if (nrow(self$data) == 0) {
                stop("Error: The provided dataset contains no complete rows. Please check your data and try again.")
            }

            # Safely require ggflowchart
            if (!requireNamespace("ggflowchart", quietly = TRUE)) {
                error_msg <- "
                <div style='color: red; background-color: #ffebee; padding: 20px; border-radius: 8px;'>
                <h4>ggflowchart Package Required</h4>
                <p>The ggflowchart package is required for modern flowchart functionality.</p>
                <p>Please install it using: <code>install.packages('ggflowchart')</code></p>
                </div>"
                self$results$interpretation$setContent(error_msg)
                return()
            }

            # Get data and variables
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
                stop("Error: No complete cases found for the selected variables.")
            }

            # Convert variables to appropriate types
            analysis_data[[from_var]] <- as.character(analysis_data[[from_var]])
            analysis_data[[to_var]] <- as.character(analysis_data[[to_var]])
            
            if (!is.null(group_var) && group_var != "") {
                analysis_data[[group_var]] <- as.factor(analysis_data[[group_var]])
            }
            
            # Generate interpretation guide
            if (self$options$show_interpretation) {
                interpretation_html <- private$.generate_interpretation_guide(analysis_data, from_var, to_var, group_var)
                self$results$interpretation$setContent(interpretation_html)
            }

            # Store data for plotting
            private$.analysis_data <- analysis_data

        },

        .plot = function(image, ggtheme, theme, ...) {
            
            # Check if analysis was performed
            if (is.null(private$.analysis_data)) {
                return()
            }
            
            analysis_data <- private$.analysis_data
            from_var <- self$options$from_var
            to_var <- self$options$to_var
            group_var <- self$options$group_var
            
            # Prepare flowchart data
            flowchart_data <- analysis_data %>%
                dplyr::select(from = !!from_var, to = !!to_var)
            
            # Add grouping if specified
            if (!is.null(group_var) && group_var != "") {
                # Create node grouping
                all_nodes <- unique(c(flowchart_data$from, flowchart_data$to))
                node_groups <- data.frame(
                    node = all_nodes,
                    group = "Default",
                    stringsAsFactors = FALSE
                )
                
                # Assign groups based on user data
                for (i in 1:nrow(analysis_data)) {
                    from_node <- analysis_data[[from_var]][i]
                    to_node <- analysis_data[[to_var]][i]
                    group_val <- as.character(analysis_data[[group_var]][i])
                    
                    node_groups[node_groups$node == from_node, "group"] <- group_val
                    node_groups[node_groups$node == to_node, "group"] <- group_val
                }
                
                flowchart_data$fill <- node_groups$group[match(flowchart_data$from, node_groups$node)]
            }
            
            # Create flowchart
            if (!is.null(group_var) && group_var != "" && "fill" %in% names(flowchart_data)) {
                # Create node_data for coloring based on groups
                all_nodes <- unique(c(flowchart_data$from, flowchart_data$to))
                node_colors <- data.frame(
                    name = all_nodes,
                    stringsAsFactors = FALSE
                )
                
                # Assign colors based on groups
                colors <- private$.get_color_palette()
                unique_groups <- unique(flowchart_data$fill)
                n_groups <- length(unique_groups)
                
                # Map groups to colors
                group_color_map <- setNames(colors[1:min(n_groups, length(colors))], unique_groups)
                
                # Assign colors to nodes based on their groups
                node_colors$fill <- "#FFFFFF"  # Default white
                for (i in 1:nrow(flowchart_data)) {
                    from_node <- flowchart_data$from[i]
                    to_node <- flowchart_data$to[i]
                    group_color <- group_color_map[flowchart_data$fill[i]]
                    
                    node_colors[node_colors$name == from_node, "fill"] <- group_color
                    node_colors[node_colors$name == to_node, "fill"] <- group_color
                }
                
                # Create flowchart with node_data
                p <- ggflowchart::ggflowchart(flowchart_data, node_data = node_colors)
            } else {
                # Create basic flowchart without grouping
                p <- ggflowchart::ggflowchart(flowchart_data)
            }
            
            # Apply modern theme
            p <- p + ggplot2::theme_minimal() +
                ggplot2::theme(
                    plot.title = ggplot2::element_text(size = 14, face = "bold", hjust = 0.5),
                    legend.position = "bottom"
                )
            
            # Add title
            plot_title <- self$options$plot_title
            p <- p + ggplot2::labs(title = plot_title)
            
            print(p)
            TRUE
        },

        .get_color_palette = function() {
            palette_name <- self$options$node_fill_palette
            
            if (palette_name == "clinical_blue") {
                return(c("#2E86AB", "#A23B72", "#F18F01", "#C73E1D", "#593E2C", "#8E6C8A"))
            } else if (palette_name == "modern_gray") {
                return(c("#E8E8E8", "#D3D3D3", "#B8B8B8", "#A0A0A0", "#888888", "#707070"))
            } else if (palette_name == "pastel") {
                return(c("#FFB3BA", "#BAFFC9", "#BAE1FF", "#FFFFBA", "#FFDFBA", "#E0BBE4"))
            } else if (palette_name == "viridis") {
                return(viridis::viridis(6, discrete = TRUE))
            } else if (palette_name == "set1") {
                return(RColorBrewer::brewer.pal(6, "Set1"))
            } else {
                # Default clinical blue
                return(c("#2E86AB", "#A23B72", "#F18F01", "#C73E1D", "#593E2C", "#8E6C8A"))
            }
        },

        .generate_interpretation_guide = function(data, from_var, to_var, group_var) {
            # Generate interpretation guide
            
            n_connections <- nrow(data)
            unique_nodes <- length(unique(c(data[[from_var]], data[[to_var]])))
            
            interpretation_html <- paste0(
                "<div style='background-color: #e8f5e8; padding: 20px; border-radius: 8px;'>",
                "<h3 style='color: #2e7d32; margin-top: 0;'>🔀 ggFlowchart Usage Guide</h3>",
                
                "<h4 style='color: #2e7d32;'>Current Flowchart:</h4>",
                "<ul>",
                "<li><strong>Connections:</strong> ", n_connections, " edges</li>",
                "<li><strong>Unique Nodes:</strong> ", unique_nodes, " nodes</li>",
                if (!is.null(group_var) && group_var != "") paste0("<li><strong>Grouping:</strong> Color-coded by ", group_var, "</li>") else "",
                "</ul>",
                
                "<h4 style='color: #2e7d32;'>ggFlowchart Features:</h4>",
                "<ul>",
                "<li><strong>ggplot2 Native:</strong> Full integration with ggplot2 ecosystem</li>",
                "<li><strong>Modern Aesthetics:</strong> Clean, contemporary styling</li>",
                "<li><strong>Simple Data:</strong> Just 'from' and 'to' connections needed</li>",
                "<li><strong>Customizable:</strong> Multiple color schemes and themes</li>",
                "</ul>",
                
                "<h4 style='color: #2e7d32;'>Best Use Cases:</h4>",
                "<ul>",
                "<li><strong>Process Flows:</strong> Simple workflow documentation</li>",
                "<li><strong>Decision Trees:</strong> Basic decision point visualization</li>",
                "<li><strong>Data Pipelines:</strong> Analysis workflow representation</li>",
                "<li><strong>Study Design:</strong> Research process mapping</li>",
                "</ul>",
                
                "<h4 style='color: #2e7d32;'>Comparison with DiagrammeR Flowchart:</h4>",
                "<ul>",
                "<li><strong>ggFlowchart:</strong> Modern ggplot2 styling, simpler data structure</li>",
                "<li><strong>DiagrammeR Flowchart:</strong> More complex layouts, detailed CONSORT features</li>",
                "<li><strong>Choose ggFlowchart for:</strong> Modern aesthetics and simple workflows</li>",
                "<li><strong>Choose DiagrammeR for:</strong> Complex CONSORT diagrams with counts</li>",
                "</ul>",
                
                "<p style='font-size: 12px; color: #2e7d32; margin-top: 15px;'>",
                "<em>🔀 Modern flowcharts with the power and flexibility of ggplot2</em>",
                "</p></div>"
            )
            
            return(interpretation_html)
        }

    )
)

# Store analysis data for plotting
.analysis_data <- NULL