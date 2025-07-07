
# This file is a generated template, your changes will not be overwritten

jggsankeyfierClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "jggsankeyfierClass",
    inherit = jggsankeyfierBase,
    private = list(
        .init = function() {
            if (is.null(self$data) || is.null(self$options$value_var)) {
                # Set default instructions
                self$results$interpretation$setContent(
                    "<h3>Sankey & Alluvial Diagram Instructions</h3>
                    <p>To create a flow diagram, you need to specify:</p>
                    <ul>
                        <li><strong>Value Variable:</strong> A numeric variable representing flow weights/sizes</li>
                    </ul>
                    <p>Then choose one of these data formats:</p>
                    <ul>
                        <li><strong>Source-Target Format:</strong> Specify Source and Target variables for simple flows</li>
                        <li><strong>Multi-Node Format:</strong> Specify 2+ Node variables for complex alluvial diagrams</li>
                    </ul>
                    <p>Optional variables:</p>
                    <ul>
                        <li><strong>Grouping Variable:</strong> To group flows by categories</li>
                        <li><strong>Time Variable:</strong> For temporal flow analysis</li>
                    </ul>"
                )
                self$results$plot$setVisible(FALSE)
                return()
            }
            
            # Validate required packages
            if (!requireNamespace("ggalluvial", quietly = TRUE)) {
                self$results$interpretation$setContent(
                    "<p style='color: red;'><strong>Error:</strong> The 'ggalluvial' package is required but not installed. 
                    Please install it using: <code>install.packages('ggalluvial')</code></p>"
                )
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
            packages <- c('ggplot2', 'ggalluvial', 'dplyr', 'scales', 'tidyr')
            for (pkg in packages) {
                if (!requireNamespace(pkg, quietly = TRUE)) {
                    stop(paste0("Package '", pkg, "' is required but not installed"))
                }
            }
            
            # Prepare flow data based on diagram type
            flow_data <- self$.prepare_flow_data(data)
            
            # Apply data transformations
            flow_data <- self$.apply_data_transformations(flow_data)
            
            # Create base plot based on diagram type
            plot <- switch(self$options$diagram_type,
                "sankey" = self$.create_sankey_diagram(flow_data),
                "alluvial" = self$.create_alluvial_diagram(flow_data), 
                "parallel_sets" = self$.create_parallel_sets(flow_data),
                self$.create_alluvial_diagram(flow_data)
            )
            
            # Apply customizations
            plot <- self$.apply_plot_customizations(plot)
            
            return(plot)
        },
        
        .prepare_flow_data = function(data) {
            value_var <- self$options$value_var
            
            if (!is.null(self$options$source_var) && !is.null(self$options$target_var)) {
                # Simple source-target flow
                cols_to_select <- list(
                    source = self$options$source_var,
                    target = self$options$target_var,
                    value = value_var
                )
                
                # Add grouping variable if specified
                if (!is.null(self$options$grouping_var)) {
                    cols_to_select$group <- self$options$grouping_var
                }
                
                # Add time variable if specified
                if (!is.null(self$options$time_var)) {
                    cols_to_select$time <- self$options$time_var
                }
                
                flow_data <- data %>%
                    dplyr::select(!!!cols_to_select) %>%
                    dplyr::filter(!is.na(source), !is.na(target), !is.na(value))
                    
            } else if (!is.null(self$options$node_vars) && length(self$options$node_vars) >= 2) {
                # Multi-level node flow (for alluvial)
                node_vars <- self$options$node_vars
                cols_to_select <- c(node_vars, value_var)
                
                # Add grouping variable if specified
                if (!is.null(self$options$grouping_var)) {
                    cols_to_select <- c(cols_to_select, self$options$grouping_var)
                }
                
                # Add time variable if specified
                if (!is.null(self$options$time_var)) {
                    cols_to_select <- c(cols_to_select, self$options$time_var)
                }
                
                flow_data <- data %>%
                    dplyr::select(!!!cols_to_select) %>%
                    dplyr::filter(!is.na(!!sym(value_var)))
                    
                # Convert to long format for ggalluvial
                flow_data <- flow_data %>%
                    tidyr::pivot_longer(
                        cols = all_of(node_vars),
                        names_to = "variable",
                        values_to = "value_cat"
                    ) %>%
                    dplyr::mutate(
                        x = match(variable, node_vars),
                        node = value_cat
                    )
                    
            } else {
                stop("Please specify either Source & Target variables or at least 2 Node variables")
            }
            
            return(flow_data)
        },
        
        .apply_data_transformations = function(flow_data) {
            # Apply sorting if requested
            if (self$options$sort_nodes == "alphabetical") {
                if ("source" %in% names(flow_data)) {
                    flow_data <- flow_data %>%
                        dplyr::arrange(source, target)
                } else if ("node" %in% names(flow_data)) {
                    flow_data <- flow_data %>%
                        dplyr::arrange(node)
                }
            } else if (self$options$sort_nodes == "by_value") {
                if ("value" %in% names(flow_data)) {
                    flow_data <- flow_data %>%
                        dplyr::arrange(desc(!!sym(self$options$value_var)))
                }
            }
            
            # Apply value formatting if needed
            if (self$options$value_format == "percent") {
                if ("value" %in% names(flow_data)) {
                    total_value <- sum(flow_data[[self$options$value_var]], na.rm = TRUE)
                    flow_data <- flow_data %>%
                        dplyr::mutate(value_formatted = (!!sym(self$options$value_var)) / total_value * 100)
                }
            } else if (self$options$value_format == "rounded") {
                if ("value" %in% names(flow_data)) {
                    flow_data <- flow_data %>%
                        dplyr::mutate(value_formatted = round(!!sym(self$options$value_var), 2))
                }
            }
            
            return(flow_data)
        },
        
        .create_sankey_diagram = function(flow_data) {
            if ('x' %in% names(flow_data) && 'node' %in% names(flow_data)) {
                # Multi-level alluvial format using ggalluvial
                plot <- ggplot2::ggplot(flow_data, ggplot2::aes(
                    x = x, stratum = node, alluvium = !!sym(self$options$value_var),
                    fill = factor(node), label = node
                )) +
                ggalluvial::stat_alluvium(
                    alpha = self$options$edge_alpha,
                    width = self$options$node_width,
                    knot.pos = 0.5
                ) +
                ggalluvial::stat_stratum(
                    width = self$options$node_width,
                    color = "black",
                    size = 0.5
                )
            } else if ('source' %in% names(flow_data) && 'target' %in% names(flow_data)) {
                # Simple source-target format using network-style visualization
                # Convert to alluvial format
                alluvial_data <- flow_data %>%
                    tidyr::pivot_longer(cols = c(source, target), names_to = "x", values_to = "stratum") %>%
                    dplyr::mutate(
                        x = ifelse(x == "source", 1, 2),
                        alluvium = row_number()
                    )
                
                plot <- ggplot2::ggplot(alluvial_data, ggplot2::aes(
                    x = x, stratum = stratum, alluvium = alluvium,
                    fill = factor(stratum), y = value
                )) +
                ggalluvial::stat_alluvium(
                    alpha = self$options$edge_alpha,
                    width = self$options$node_width,
                    knot.pos = 0.5
                ) +
                ggalluvial::stat_stratum(
                    width = self$options$node_width,
                    color = "black",
                    size = 0.5
                )
            } else {
                stop("Invalid data format for sankey diagram")
            }
            
            return(plot)
        },
        
        .create_alluvial_diagram = function(flow_data) {
            if ('x' %in% names(flow_data) && 'node' %in% names(flow_data)) {
                # Multi-node alluvial
                plot <- ggplot2::ggplot(flow_data, ggplot2::aes(
                    x = x, stratum = node, alluvium = !!sym(self$options$value_var),
                    fill = factor(node)
                )) +
                ggalluvial::stat_alluvium(
                    alpha = self$options$edge_alpha,
                    width = self$options$node_width,
                    curve_type = "sigmoid"
                ) +
                ggalluvial::stat_stratum(
                    width = self$options$node_width,
                    color = "white",
                    size = 0.3
                )
            } else {
                # Fall back to sankey for simple data
                plot <- self$.create_sankey_diagram(flow_data)
            }
            
            return(plot)
        },
        
        .create_parallel_sets = function(flow_data) {
            # For parallel sets, use straight lines instead of curves
            if ('x' %in% names(flow_data) && 'node' %in% names(flow_data)) {
                plot <- ggplot2::ggplot(flow_data, ggplot2::aes(
                    x = x, stratum = node, alluvium = !!sym(self$options$value_var),
                    fill = factor(node)
                )) +
                ggalluvial::stat_alluvium(
                    alpha = self$options$edge_alpha,
                    width = self$options$node_width,
                    curve_type = "linear"  # Straight lines for parallel sets
                ) +
                ggalluvial::stat_stratum(
                    width = self$options$node_width,
                    color = "black",
                    size = 0.8
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
                    "viridis" = ggplot2::scale_fill_viridis_d(),
                    "plasma" = ggplot2::scale_fill_viridis_d(option = "plasma"),
                    "set3" = ggplot2::scale_fill_brewer(type = "qual", palette = "Set3"),
                    "pastel1" = ggplot2::scale_fill_brewer(type = "qual", palette = "Pastel1"),
                    "dark2" = ggplot2::scale_fill_brewer(type = "qual", palette = "Dark2"),
                    ggplot2::scale_fill_viridis_d()
                )
                plot <- plot + colors
            }
            
            # Add stratum labels if requested
            if (self$options$show_labels) {
                plot <- plot + ggalluvial::geom_stratum_label(
                    size = self$options$label_size / 3,
                    color = "black",
                    fontface = "bold"
                )
            }
            
            # Add flow values if requested
            if (self$options$show_values) {
                # Add text showing flow values
                plot <- plot + ggalluvial::geom_alluvium_label(
                    aes(label = !!sym(self$options$value_var)),
                    size = self$options$label_size / 4,
                    color = "white",
                    fontface = "bold"
                )
            }
            
            # Apply flow direction
            if (self$options$flow_direction == "top_bottom") {
                plot <- plot + ggplot2::coord_flip()
            } else if (self$options$flow_direction == "right_left") {
                plot <- plot + ggplot2::scale_x_reverse()
            } else if (self$options$flow_direction == "bottom_top") {
                plot <- plot + ggplot2::coord_flip() + ggplot2::scale_y_reverse()
            }
            
            # Add titles
            if (nchar(self$options$plot_title) > 0) {
                plot <- plot + ggplot2::ggtitle(self$options$plot_title)
            }
            
            if (nchar(self$options$plot_subtitle) > 0) {
                plot <- plot + ggplot2::labs(subtitle = self$options$plot_subtitle)
            }
            
            # Remove axis labels and ticks for cleaner look
            plot <- plot + ggplot2::theme(
                axis.text = ggplot2::element_blank(),
                axis.ticks = ggplot2::element_blank(),
                axis.title = ggplot2::element_blank(),
                panel.grid = ggplot2::element_blank(),
                legend.position = "bottom"
            )
            
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
