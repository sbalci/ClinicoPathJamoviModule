
# This file is a generated template, your changes will not be overwritten

jggsankeyfierClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "jggsankeyfierClass",
    inherit = jggsankeyfierBase,
    private = list(
        .init = function() {
            if (is.null(self$data) || is.null(self$options$value_var)) {
                self$results$plot$setVisible(FALSE)
                return()
            }
        },
        
        .run = function() {
            # Early return if essential variables not specified
            if (is.null(self$options$value_var) || 
                (is.null(self$options$source_var) && is.null(self$options$node_vars))) {
                return()
            }
            
            # Prepare data
            data <- self$data
            if (nrow(data) == 0) return()
            
            # Clean variable names
            if (requireNamespace('janitor', quietly = TRUE)) {
                data <- janitor::clean_names(data)
            }
            
            # Generate plot
            plot <- self$.create_sankey_plot(data)
            
            # Set plot
            self$results$plot$setState(plot)
            
            # Generate data table if requested
            if (self$options$output_format %in% c("data_table", "both")) {
                self$.populate_data_table(data)
            }
            
            # Generate statistics if requested  
            if (self$options$show_statistics) {
                self$.populate_statistics_table(data)
            }
            
            # Generate interpretation if requested
            if (self$options$show_interpretation) {
                self$.generate_interpretation(data)
            }
        },
        
        .plot = function(image, ggtheme, theme, ...) {
            if (is.null(image$state))
                return(FALSE)
                
            plot <- image$state
            print(plot)
            
            TRUE
        },
        
        .create_sankey_plot = function(data) {
            # Load required packages
            packages <- c('ggplot2', 'ggsankey', 'dplyr', 'scales')
            for (pkg in packages) {
                if (!requireNamespace(pkg, quietly = TRUE)) {
                    stop(paste0("Package '", pkg, "' is required but not installed"))
                }
            }
            
            # Prepare flow data based on diagram type
            flow_data <- self$.prepare_flow_data(data)
            
            # Create base plot based on diagram type
            plot <- switch(self$options$diagram_type,
                "sankey" = self$.create_sankey_diagram(flow_data),
                "alluvial" = self$.create_alluvial_diagram(flow_data), 
                "parallel_sets" = self$.create_parallel_sets(flow_data),
                self$.create_sankey_diagram(flow_data)
            )
            
            # Apply customizations
            plot <- self$.apply_plot_customizations(plot)
            
            return(plot)
        },
        
        .prepare_flow_data = function(data) {
            value_var <- self$options$value_var
            
            if (!is.null(self$options$source_var) && !is.null(self$options$target_var)) {
                # Simple source-target flow
                flow_data <- data %>%
                    dplyr::select(
                        source = !!self$options$source_var,
                        target = !!self$options$target_var,
                        value = !!value_var
                    ) %>%
                    dplyr::filter(!is.na(source), !is.na(target), !is.na(value))
                    
            } else if (!is.null(self$options$node_vars) && length(self$options$node_vars) >= 2) {
                # Multi-level node flow (for alluvial)
                node_vars <- self$options$node_vars
                flow_data <- data %>%
                    dplyr::select(!!!node_vars, value = !!value_var) %>%
                    dplyr::filter(!is.na(value))
                    
                # Convert to long format for ggsankey
                flow_data <- flow_data %>%
                    ggsankey::make_long(!!!node_vars, value = value)
                    
            } else {
                stop("Please specify either Source & Target variables or at least 2 Node variables")
            }
            
            return(flow_data)
        },
        
        .create_sankey_diagram = function(flow_data) {
            if ('next_x' %in% names(flow_data)) {
                # Multi-level alluvial format
                plot <- ggplot2::ggplot(flow_data, ggplot2::aes(
                    x = x, next_x = next_x, node = node, next_node = next_node,
                    fill = factor(node), label = node
                )) +
                ggsankey::geom_sankey(
                    flow.alpha = self$options$edge_alpha,
                    node.color = "black",
                    show.legend = FALSE
                )
            } else {
                # Simple source-target format  
                plot <- ggplot2::ggplot(flow_data, ggplot2::aes(
                    x = 0, xend = 1, y = source, yend = target, size = value
                )) +
                ggplot2::geom_segment(alpha = self$options$edge_alpha)
            }
            
            return(plot)
        },
        
        .create_alluvial_diagram = function(flow_data) {
            if ('next_x' %in% names(flow_data)) {
                plot <- ggplot2::ggplot(flow_data, ggplot2::aes(
                    x = x, next_x = next_x, node = node, next_node = next_node,
                    fill = factor(node), label = node
                )) +
                ggsankey::geom_alluvial(
                    flow.alpha = self$options$edge_alpha,
                    node.color = "black"
                )
            } else {
                # Fall back to sankey for simple data
                plot <- self$.create_sankey_diagram(flow_data)
            }
            
            return(plot)
        },
        
        .create_parallel_sets = function(flow_data) {
            # For parallel sets, use geom_alluvial with specific styling
            if ('next_x' %in% names(flow_data)) {
                plot <- ggplot2::ggplot(flow_data, ggplot2::aes(
                    x = x, next_x = next_x, node = node, next_node = next_node,
                    fill = factor(node)
                )) +
                ggsankey::geom_alluvial_ts(
                    alpha = self$options$edge_alpha,
                    smooth = 8
                )
            } else {
                plot <- self$.create_sankey_diagram(flow_data)
            }
            
            return(plot)
        },
        
        .apply_plot_customizations = function(plot) {
            # Apply theme
            theme_func <- switch(self$options$theme_style,
                "minimal" = ggplot2::theme_minimal(),
                "classic" = ggplot2::theme_classic(),
                "void" = ggplot2::theme_void(),
                ggplot2::theme_gray()
            )
            
            plot <- plot + theme_func
            
            # Apply color palette
            if (self$options$color_palette != "default") {
                colors <- switch(self$options$color_palette,
                    "viridis" = scales::viridis_d(),
                    "plasma" = scales::viridis_d(option = "plasma"),
                    "set3" = ggplot2::scale_fill_brewer(type = "qual", palette = "Set3"),
                    "pastel1" = ggplot2::scale_fill_brewer(type = "qual", palette = "Pastel1"),
                    "dark2" = ggplot2::scale_fill_brewer(type = "qual", palette = "Dark2"),
                    scales::viridis_d()
                )
                plot <- plot + colors
            }
            
            # Add labels if requested
            if (self$options$show_labels) {
                if ('label' %in% names(plot$data)) {
                    plot <- plot + ggsankey::geom_sankey_label(
                        size = self$options$label_size / 3,
                        color = "black", 
                        fill = "white",
                        alpha = 0.8
                    )
                }
            }
            
            # Add titles
            if (nchar(self$options$plot_title) > 0) {
                plot <- plot + ggplot2::ggtitle(self$options$plot_title)
            }
            
            if (nchar(self$options$plot_subtitle) > 0) {
                plot <- plot + ggplot2::labs(subtitle = self$options$plot_subtitle)
            }
            
            return(plot)
        },
        
        .populate_data_table = function(data) {
            if (is.null(self$options$source_var) || is.null(self$options$target_var)) {
                return()
            }
            
            # Prepare data table
            table_data <- data %>%
                dplyr::select(
                    source = !!self$options$source_var,
                    target = !!self$options$target_var,
                    value = !!self$options$value_var
                ) %>%
                dplyr::filter(!is.na(source), !is.na(target), !is.na(value)) %>%
                dplyr::mutate(
                    percentage = value / sum(value, na.rm = TRUE)
                )
            
            # Populate results table
            for (i in seq_len(nrow(table_data))) {
                self$results$datatab$addRow(
                    rowKey = i,
                    values = list(
                        source = table_data$source[i],
                        target = table_data$target[i], 
                        value = table_data$value[i],
                        percentage = table_data$percentage[i]
                    )
                )
            }
        },
        
        .populate_statistics_table = function(data) {
            if (is.null(self$options$value_var)) return()
            
            value_data <- data[[self$options$value_var]]
            value_data <- value_data[!is.na(value_data)]
            
            if (length(value_data) == 0) return()
            
            # Calculate statistics
            stats <- list(
                list(metric = "Total Flow", value = sum(value_data)),
                list(metric = "Mean Flow", value = mean(value_data)),
                list(metric = "Median Flow", value = median(value_data)),
                list(metric = "Number of Flows", value = length(value_data)),
                list(metric = "Max Flow", value = max(value_data)),
                list(metric = "Min Flow", value = min(value_data))
            )
            
            # Populate results table
            for (i in seq_along(stats)) {
                self$results$stats$addRow(
                    rowKey = i,
                    values = stats[[i]]
                )
            }
        },
        
        .generate_interpretation = function(data) {
            if (is.null(self$options$value_var)) return()
            
            value_data <- data[[self$options$value_var]]
            value_data <- value_data[!is.na(value_data)]
            
            if (length(value_data) == 0) return()
            
            # Generate interpretation
            total_flow <- sum(value_data)
            num_flows <- length(value_data)
            avg_flow <- mean(value_data)
            
            diagram_name <- switch(self$options$diagram_type,
                "sankey" = "Sankey diagram",
                "alluvial" = "Alluvial diagram", 
                "parallel_sets" = "Parallel sets diagram",
                "diagram"
            )
            
            interpretation <- paste0(
                "<h3>Flow Analysis Summary</h3>",
                "<p>This ", diagram_name, " visualizes ", num_flows, " flow connections ",
                "with a total flow volume of ", round(total_flow, 2), ".</p>",
                "<p>The average flow size is ", round(avg_flow, 2), 
                ", indicating ", 
                if (avg_flow > median(value_data)) "right-skewed" else "fairly distributed",
                " flow patterns.</p>",
                if (self$options$diagram_type == "sankey") {
                    "<p>Sankey diagrams are ideal for showing how quantities flow between categories, 
                     with line thickness proportional to flow magnitude.</p>"
                } else if (self$options$diagram_type == "alluvial") {
                    "<p>Alluvial diagrams excel at showing how categorical variables change 
                     across multiple dimensions or time points.</p>"
                } else {
                    "<p>Parallel sets provide an alternative view of categorical relationships 
                     across multiple variables.</p>"
                }
            )
            
            self$results$interpretation$setContent(interpretation)
        })
)
