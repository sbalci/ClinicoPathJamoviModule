#' @title Segmented Total Bar Charts
#' @description
#' Create segmented total bar charts (100% stacked bars) that show proportional 
#' breakdowns within categories. Perfect for displaying composition data where 
#' each bar represents 100% and segments show relative proportions.
#'
#' @details
#' This module creates segmented total bar charts using ggplot2, where:
#' - Each bar represents 100% of the total for that category
#' - Segments within bars show relative proportions
#' - Colors distinguish different segments
#' - Labels can show percentages and/or raw counts
#'
#' @importFrom R6 R6Class
#' @import jmvcore
#' @importFrom ggplot2 ggplot aes geom_col position_fill coord_flip
#' @importFrom ggplot2 theme_minimal theme_classic theme_bw labs scale_fill_brewer
#' @importFrom ggplot2 scale_fill_viridis_d geom_text element_text theme element_rect
#' @importFrom ggplot2 element_line margin scale_y_continuous
#' @importFrom dplyr group_by summarise mutate arrange count ungroup
#' @importFrom scales percent
#' @importFrom RColorBrewer brewer.pal
#' @importFrom stringr str_to_title
#' @importFrom rlang sym
#' @export

jjsegmentedtotalbarClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "jjsegmentedtotalbarClass",
    inherit = jjsegmentedtotalbarBase,
    private = list(
        
        # Internal data storage
        .processed_data = NULL,
        .composition_data = NULL,
        
        .init = function() {
            # Initialize instructions
            instructions_html <- paste(
                "<div style='background-color: #e8f5e8; padding: 20px; border-radius: 8px; margin: 10px 0;'>",
                "<h3 style='color: #2e7d32; margin-top: 0;'>Segmented Total Bar Charts</h3>",
                "<div style='margin: 10px 0;'>",
                "<p><strong>Create 100% stacked bar charts showing proportional composition:</strong></p>",
                "<ul style='margin: 5px 0; padding-left: 20px; line-height: 1.6;'>",
                "<li><strong>Category Variable (X-axis):</strong> Main grouping variable</li>",
                "<li><strong>Value Variable (Y-axis):</strong> Numeric values for segments</li>",
                "<li><strong>Segment Variable (Fill):</strong> Variable defining bar segments</li>",
                "<li><strong>Panel Variable:</strong> Optional faceting variable</li>",
                "</ul>",
                "<p style='margin: 10px 0; padding: 10px; background-color: #f3e5f5; border-left: 4px solid #9c27b0;'>",
                "<strong>Perfect for:</strong> Survey responses, patient demographics, market composition, treatment outcomes</p>",
                "</div>",
                "</div>"
            )
            
            self$results$instructions$setContent(instructions_html)
            
            # Set default plot size
            self$results$plot$setSize(650, 450)
        },
        
        .run = function() {
            
            # Check if required variables are specified
            if (is.null(self$options$x_var) || 
                is.null(self$options$y_var) || 
                is.null(self$options$fill_var)) {
                return()
            }
            
            # Get data
            data <- self$data
            
            # Extract variable names
            x_var <- self$options$x_var
            y_var <- self$options$y_var
            fill_var <- self$options$fill_var
            facet_var <- self$options$facet_var
            
            # Validate data
            if (nrow(data) == 0) {
                self$results$instructions$setContent("No data available for analysis.")
                return()
            }
            
            # Prepare data
            private$.processData(data, x_var, y_var, fill_var, facet_var)
            
            # Create the plot
            private$.createPlot()
            
            # Update summary table
            private$.updateSummary()
            
            # Update composition table
            private$.updateComposition()
            
            # Update detailed statistics
            private$.updateDetailedStats()
            
            # Create interpretation
            private$.createInterpretation()
        },
        
        .processData = function(data, x_var, y_var, fill_var, facet_var) {
            
            # Validate variables exist in data
            required_vars <- c(x_var, y_var, fill_var)
            missing_vars <- setdiff(required_vars, names(data))
            if (length(missing_vars) > 0) {
                stop(paste("Missing variables in data:", paste(missing_vars, collapse = ", ")))
            }
            
            # Check for NA values in key variables
            if (sum(is.na(data[[x_var]])) > 0) {
                warning(paste("Found", sum(is.na(data[[x_var]])), "NA values in", x_var, "- these will be removed"))
                data <- data[!is.na(data[[x_var]]), ]
            }
            
            if (sum(is.na(data[[fill_var]])) > 0) {
                warning(paste("Found", sum(is.na(data[[fill_var]])), "NA values in", fill_var, "- these will be removed"))
                data <- data[!is.na(data[[fill_var]]), ]
            }
            
            # Check if y_var is numeric
            if (!is.numeric(data[[y_var]])) {
                stop(paste(y_var, "must be a numeric variable"))
            }
            
            # Convert variables to factors if needed
            data[[x_var]] <- as.factor(data[[x_var]])
            data[[fill_var]] <- as.factor(data[[fill_var]])
            
            # Check for sufficient levels
            if (length(levels(data[[x_var]])) < 1) {
                stop(paste(x_var, "must have at least one category"))
            }
            
            if (length(levels(data[[fill_var]])) < 1) {
                stop(paste(fill_var, "must have at least one segment"))
            }
            
            if (!is.null(facet_var)) {
                if (!facet_var %in% names(data)) {
                    stop(paste("Facet variable", facet_var, "not found in data"))
                }
                data[[facet_var]] <- as.factor(data[[facet_var]])
            }
            
            # Group and summarize data
            if (!is.null(facet_var)) {
                processed_data <- data %>%
                    dplyr::group_by(!!rlang::sym(x_var), !!rlang::sym(fill_var), !!rlang::sym(facet_var)) %>%
                    dplyr::summarise(
                        value = sum(!!rlang::sym(y_var), na.rm = TRUE),
                        count = dplyr::n(),
                        .groups = 'drop'
                    )
            } else {
                processed_data <- data %>%
                    dplyr::group_by(!!rlang::sym(x_var), !!rlang::sym(fill_var)) %>%
                    dplyr::summarise(
                        value = sum(!!rlang::sym(y_var), na.rm = TRUE),
                        count = dplyr::n(),
                        .groups = 'drop'
                    )
            }
            
            # Calculate percentages within each category
            if (!is.null(facet_var)) {
                processed_data <- processed_data %>%
                    dplyr::group_by(!!rlang::sym(x_var), !!rlang::sym(facet_var)) %>%
                    dplyr::mutate(
                        percentage = value / sum(value) * 100,
                        total_in_category = sum(value)
                    ) %>%
                    dplyr::ungroup()
            } else {
                processed_data <- processed_data %>%
                    dplyr::group_by(!!rlang::sym(x_var)) %>%
                    dplyr::mutate(
                        percentage = value / sum(value) * 100,
                        total_in_category = sum(value)
                    ) %>%
                    dplyr::ungroup()
            }
            
            # Check for empty groups
            if (nrow(processed_data) == 0) {
                stop("No valid data after processing. Please check your input variables.")
            }
            
            # Check for zero totals
            zero_totals <- processed_data %>%
                dplyr::group_by(!!rlang::sym(x_var)) %>%
                dplyr::summarise(total = sum(value), .groups = 'drop') %>%
                dplyr::filter(total == 0)
            
            if (nrow(zero_totals) > 0) {
                warning(paste("Categories with zero totals found:", 
                            paste(zero_totals[[x_var]], collapse = ", "),
                            "- these may appear empty in the plot"))
            }
            
            # Apply sorting if requested
            if (self$options$sort_categories != "none") {
                processed_data <- private$.applySorting(processed_data, x_var, fill_var, facet_var)
            }
            
            private$.processed_data <- processed_data
            
            # Create composition data for table
            composition_data <- processed_data %>%
                dplyr::select(!!rlang::sym(x_var), !!rlang::sym(fill_var), count, percentage, total_in_category)
            
            names(composition_data) <- c("category", "segment", "count", "percentage", "total_in_category")
            
            private$.composition_data <- composition_data
        },
        
        .applySorting = function(data, x_var, fill_var, facet_var) {
            
            sort_type <- self$options$sort_categories
            
            if (sort_type == "total") {
                # Sort by total value in each category
                if (!is.null(facet_var)) {
                    category_totals <- data %>%
                        dplyr::group_by(!!rlang::sym(x_var), !!rlang::sym(facet_var)) %>%
                        dplyr::summarise(total = sum(value), .groups = 'drop') %>%
                        dplyr::arrange(desc(total))
                } else {
                    category_totals <- data %>%
                        dplyr::group_by(!!rlang::sym(x_var)) %>%
                        dplyr::summarise(total = sum(value), .groups = 'drop') %>%
                        dplyr::arrange(desc(total))
                }
                
                data[[x_var]] <- factor(data[[x_var]], levels = category_totals[[x_var]])
                
            } else if (sort_type == "largest_segment") {
                # Sort by the size of the largest segment in each category
                if (!is.null(facet_var)) {
                    largest_segments <- data %>%
                        dplyr::group_by(!!rlang::sym(x_var), !!rlang::sym(facet_var)) %>%
                        dplyr::summarise(max_segment = max(value), .groups = 'drop') %>%
                        dplyr::arrange(desc(max_segment))
                } else {
                    largest_segments <- data %>%
                        dplyr::group_by(!!rlang::sym(x_var)) %>%
                        dplyr::summarise(max_segment = max(value), .groups = 'drop') %>%
                        dplyr::arrange(desc(max_segment))
                }
                
                data[[x_var]] <- factor(data[[x_var]], levels = largest_segments[[x_var]])
                
            } else if (sort_type == "alpha") {
                # Alphabetical sorting
                data[[x_var]] <- factor(data[[x_var]], levels = sort(levels(data[[x_var]])))
            }
            
            return(data)
        },
        
        .createPlot = function() {
            
            data <- private$.processed_data
            
            if (is.null(data) || nrow(data) == 0) {
                self$results$instructions$setContent(
                    "<div style='background-color: #ffebee; padding: 15px; border-radius: 6px; margin: 10px 0;'>
                    <p style='color: #c62828;'><strong>Error:</strong> No data available for plotting. 
                    Please check your variable selections and ensure data is available.</p>
                    </div>"
                )
                return()
            }
            
            x_var <- self$options$x_var
            y_var <- "value"
            fill_var <- self$options$fill_var
            facet_var <- self$options$facet_var
            
            # Create base plot
            p <- ggplot2::ggplot(data, ggplot2::aes(x = !!rlang::sym(x_var), 
                                                   y = !!rlang::sym(y_var), 
                                                   fill = !!rlang::sym(fill_var)))
            
            # Add bars with position_fill for 100% stacking
            if (self$options$add_outline && self$options$outline_color != "none") {
                outline_color <- switch(self$options$outline_color,
                                      "white" = "white",
                                      "black" = "black", 
                                      "gray" = "gray50",
                                      "white")
                
                p <- p + ggplot2::geom_col(position = "fill", 
                                         width = self$options$bar_width,
                                         color = outline_color,
                                         size = 0.5)
            } else {
                p <- p + ggplot2::geom_col(position = "fill", 
                                         width = self$options$bar_width)
            }
            
            # Add percentage labels if requested
            if (self$options$show_percentages) {
                
                # Filter data for labels based on threshold
                label_data <- data %>%
                    dplyr::filter(percentage >= self$options$label_threshold)
                
                if (nrow(label_data) > 0) {
                    
                    # Calculate label positions
                    label_data <- label_data %>%
                        dplyr::group_by(!!rlang::sym(x_var)) %>%
                        dplyr::arrange(!!rlang::sym(fill_var)) %>%
                        dplyr::mutate(
                            cumulative = cumsum(value),
                            label_position = cumulative - value/2,
                            total = sum(value),
                            label_position_prop = label_position / total
                        ) %>%
                        dplyr::ungroup()
                    
                    # Format percentage labels
                    if (self$options$percentage_format == "integer") {
                        label_data$label <- paste0(round(label_data$percentage), "%")
                    } else if (self$options$percentage_format == "decimal1") {
                        label_data$label <- paste0(round(label_data$percentage, 1), "%")
                    } else if (self$options$percentage_format == "decimal2") {
                        label_data$label <- paste0(round(label_data$percentage, 2), "%")
                    }
                    
                    # Add count if requested
                    if (self$options$show_counts) {
                        label_data$label <- paste0(label_data$label, "\n(n=", label_data$count, ")")
                    }
                    
                    p <- p + ggplot2::geom_text(
                        data = label_data,
                        ggplot2::aes(x = !!rlang::sym(x_var), 
                                    y = label_position_prop, 
                                    label = label),
                        position = ggplot2::position_identity(),
                        size = 3,
                        color = "white",
                        fontface = "bold"
                    )
                }
            }
            
            # Apply color palette
            p <- private$.applyColorPalette(p, data, fill_var)
            
            # Apply orientation
            if (self$options$orientation == "horizontal") {
                p <- p + ggplot2::coord_flip()
            }
            
            # Add faceting if specified
            if (!is.null(facet_var)) {
                p <- p + ggplot2::facet_wrap(ggplot2::vars(!!rlang::sym(facet_var)))
            }
            
            # Apply theme and styling
            p <- private$.applyTheme(p)
            
            # Add titles and labels
            p <- private$.addTitles(p)
            
            # Set y-axis to percentage scale
            p <- p + ggplot2::scale_y_continuous(labels = scales::percent_format())
            
            # Apply legend settings
            p <- private$.applyLegend(p)
            
            # Apply export-ready settings if requested
            if (self$options$export_ready) {
                p <- p + ggplot2::theme(
                    text = ggplot2::element_text(family = "Arial"),
                    plot.background = ggplot2::element_rect(fill = "white", color = NA),
                    panel.background = ggplot2::element_rect(fill = "white", color = NA),
                    legend.background = ggplot2::element_rect(fill = "white", color = NA)
                )
            }
            
            # Print plot
            print(p)
            TRUE
        },
        
        .applyColorPalette = function(p, data, fill_var) {
            
            palette <- self$options$color_palette
            n_colors <- length(unique(data[[fill_var]]))
            
            if (palette == "viridis") {
                p <- p + ggplot2::scale_fill_viridis_d()
            } else if (palette == "set1") {
                p <- p + ggplot2::scale_fill_brewer(type = "qual", palette = "Set1")
            } else if (palette == "dark2") {
                p <- p + ggplot2::scale_fill_brewer(type = "qual", palette = "Dark2")
            } else if (palette == "paired") {
                p <- p + ggplot2::scale_fill_brewer(type = "qual", palette = "Paired")
            } else if (palette == "clinical") {
                clinical_colors <- c("#2E86AB", "#A23B72", "#F18F01", "#C73E1D", "#6A994E", "#7209B7", "#F72585", "#4361EE")
                if (n_colors <= length(clinical_colors)) {
                    p <- p + ggplot2::scale_fill_manual(values = clinical_colors[1:n_colors])
                } else {
                    p <- p + ggplot2::scale_fill_viridis_d()
                }
            } else if (palette == "colorblind") {
                cb_colors <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#000000")
                if (n_colors <= length(cb_colors)) {
                    p <- p + ggplot2::scale_fill_manual(values = cb_colors[1:n_colors])
                } else {
                    p <- p + ggplot2::scale_fill_viridis_d()
                }
            } else if (palette == "bbc_multi") {
                bbc_colors <- c("#1380A1", "#FAAB18", "#990000", "#588300", "#990099", "#FF6600", "#809900", "#bcc4c1")
                if (n_colors <= length(bbc_colors)) {
                    p <- p + ggplot2::scale_fill_manual(values = bbc_colors[1:n_colors])
                } else {
                    p <- p + ggplot2::scale_fill_viridis_d()
                }
            } else if (palette == "prism_colorblind_safe") {
                prism_colors <- c("#0000FF", "#FF0000", "#00BF00", "#FF00FF", "#FFFF00", "#00FFFF", "#800080", "#FFA500")
                if (n_colors <= length(prism_colors)) {
                    p <- p + ggplot2::scale_fill_manual(values = prism_colors[1:n_colors])
                } else {
                    p <- p + ggplot2::scale_fill_viridis_d()
                }
            } else if (palette == "nature") {
                nature_colors <- c("#E64B35", "#4DBBD5", "#00A087", "#3C5488", "#F39B7F", "#8491B4", "#91D1C2", "#DC0000")
                if (n_colors <= length(nature_colors)) {
                    p <- p + ggplot2::scale_fill_manual(values = nature_colors[1:n_colors])
                } else {
                    p <- p + ggplot2::scale_fill_viridis_d()
                }
            } else if (palette == "science") {
                science_colors <- c("#0C5BB0", "#FFA042", "#15983D", "#EC0000", "#8b7355", "#149EF3", "#FA6B09", "#A149FA")
                if (n_colors <= length(science_colors)) {
                    p <- p + ggplot2::scale_fill_manual(values = science_colors[1:n_colors])
                } else {
                    p <- p + ggplot2::scale_fill_viridis_d()
                }
            }
            # Default fallback to viridis if palette not recognized
            
            return(p)
        },
        
        .applyTheme = function(p) {
            
            style <- self$options$chart_style
            
            if (style == "clean") {
                p <- p + ggplot2::theme_minimal() +
                    ggplot2::theme(
                        panel.grid.major.x = ggplot2::element_blank(),
                        panel.grid.minor = ggplot2::element_blank(),
                        text = ggplot2::element_text(size = 11),
                        axis.title = ggplot2::element_text(size = 12, face = "bold"),
                        plot.title = ggplot2::element_text(size = 14, face = "bold", hjust = 0.5)
                    )
            } else if (style == "publication") {
                p <- p + ggplot2::theme_classic() +
                    ggplot2::theme(
                        text = ggplot2::element_text(size = 10, family = "serif"),
                        axis.title = ggplot2::element_text(size = 11, face = "bold"),
                        plot.title = ggplot2::element_text(size = 12, face = "bold", hjust = 0.5),
                        legend.text = ggplot2::element_text(size = 9),
                        axis.line = ggplot2::element_line(color = "black", size = 0.5)
                    )
            } else if (style == "presentation") {
                p <- p + ggplot2::theme_minimal() +
                    ggplot2::theme(
                        text = ggplot2::element_text(size = 14),
                        axis.title = ggplot2::element_text(size = 16, face = "bold"),
                        plot.title = ggplot2::element_text(size = 18, face = "bold", hjust = 0.5),
                        legend.text = ggplot2::element_text(size = 12),
                        panel.grid.major.x = ggplot2::element_blank(),
                        panel.grid.minor = ggplot2::element_blank()
                    )
            } else if (style == "clinical") {
                p <- p + ggplot2::theme_bw() +
                    ggplot2::theme(
                        text = ggplot2::element_text(size = 11, family = "sans"),
                        axis.title = ggplot2::element_text(size = 12, face = "bold"),
                        plot.title = ggplot2::element_text(size = 14, face = "bold", hjust = 0.5),
                        panel.grid.major.x = ggplot2::element_blank(),
                        panel.grid.minor = ggplot2::element_blank(),
                        legend.position = "right",
                        legend.background = ggplot2::element_rect(fill = "white", color = "gray80"),
                        panel.border = ggplot2::element_rect(color = "black", fill = NA, linewidth = 1)
                    )
            } else if (style == "bbc_style") {
                p <- p + ggplot2::theme_minimal() +
                    ggplot2::theme(
                        text = ggplot2::element_text(size = 11, family = "sans"),
                        plot.title = ggplot2::element_text(size = 18, face = "bold", hjust = 0),
                        plot.subtitle = ggplot2::element_text(size = 14, hjust = 0),
                        axis.title = ggplot2::element_blank(),
                        axis.text = ggplot2::element_text(size = 10),
                        panel.grid.major.x = ggplot2::element_blank(),
                        panel.grid.minor = ggplot2::element_blank(),
                        panel.background = ggplot2::element_blank(),
                        legend.position = "top",
                        legend.justification = "left",
                        legend.text = ggplot2::element_text(size = 10),
                        legend.title = ggplot2::element_blank()
                    )
            } else if (style == "prism_style") {
                p <- p + ggplot2::theme_classic() +
                    ggplot2::theme(
                        text = ggplot2::element_text(size = 10, family = "sans"),
                        axis.title = ggplot2::element_text(size = 11),
                        plot.title = ggplot2::element_text(size = 12, face = "bold", hjust = 0.5),
                        axis.line = ggplot2::element_line(color = "black", linewidth = 0.5),
                        axis.ticks = ggplot2::element_line(color = "black", linewidth = 0.5),
                        legend.text = ggplot2::element_text(size = 9),
                        legend.key.size = ggplot2::unit(0.8, "cm"),
                        panel.background = ggplot2::element_rect(fill = "white")
                    )
            }
            
            return(p)
        },
        
        .addTitles = function(p) {
            
            # Set titles
            plot_title <- if (self$options$plot_title != "") self$options$plot_title else "Segmented Total Bar Chart"
            x_title <- if (self$options$x_title != "") self$options$x_title else self$options$x_var
            y_title <- if (self$options$y_title != "") self$options$y_title else "Percentage"
            legend_title <- if (self$options$legend_title != "") self$options$legend_title else self$options$fill_var
            
            p <- p + ggplot2::labs(
                title = plot_title,
                x = x_title,
                y = y_title,
                fill = legend_title
            )
            
            return(p)
        },
        
        .applyLegend = function(p) {
            
            position <- self$options$legend_position
            
            if (position == "none") {
                p <- p + ggplot2::theme(legend.position = "none")
            } else {
                p <- p + ggplot2::theme(legend.position = position)
            }
            
            return(p)
        },
        
        .updateSummary = function() {
            
            data <- private$.processed_data
            
            if (is.null(data)) {
                return()
            }
            
            n_categories <- length(unique(data[[self$options$x_var]]))
            n_segments <- length(unique(data[[self$options$fill_var]]))
            total_obs <- sum(data$count, na.rm = TRUE)
            
            summary_row <- list(
                categories = n_categories,
                segments = n_segments,
                total_observations = total_obs,
                chart_type = "Segmented Total Bar (100% Stacked)"
            )
            
            self$results$summary$setRow(rowNo = 1, values = summary_row)
        },
        
        .updateComposition = function() {
            
            data <- private$.composition_data
            
            if (is.null(data)) {
                return()
            }
            
            for (i in 1:nrow(data)) {
                row <- list(
                    category = as.character(data$category[i]),
                    segment = as.character(data$segment[i]),
                    count = data$count[i],
                    percentage = data$percentage[i] / 100,  # Convert to proportion for percentage format
                    total_in_category = data$total_in_category[i]
                )
                
                self$results$composition_table$addRow(rowKey = i, values = row)
            }
        },
        
        .createInterpretation = function() {
            
            data <- private$.processed_data
            composition_data <- private$.composition_data
            
            if (is.null(data) || is.null(composition_data)) {
                return()
            }
            
            n_categories <- length(unique(data[[self$options$x_var]]))
            n_segments <- length(unique(data[[self$options$fill_var]]))
            
            # Find largest segment overall
            largest_segment <- composition_data %>%
                dplyr::arrange(desc(percentage)) %>%
                dplyr::slice(1)
            
            # Find most balanced category
            balanced_category <- composition_data %>%
                dplyr::group_by(category) %>%
                dplyr::summarise(
                    cv = sd(percentage) / mean(percentage),
                    .groups = 'drop'
                ) %>%
                dplyr::arrange(cv) %>%
                dplyr::slice(1)
            
            interpretation_html <- paste(
                "<div style='background-color: #f8f9fa; padding: 15px; border-radius: 6px; margin: 10px 0;'>",
                "<h4 style='color: #495057; margin-top: 0;'>Chart Interpretation</h4>",
                "<p><strong>Data Overview:</strong></p>",
                "<ul>",
                paste0("<li>", n_categories, " categories with ", n_segments, " segments each</li>"),
                paste0("<li>Total observations: ", sum(composition_data$count), "</li>"),
                "</ul>",
                "<p><strong>Key Findings:</strong></p>",
                "<ul>",
                paste0("<li><strong>Largest segment:</strong> ", largest_segment$segment[1], 
                       " in ", largest_segment$category[1], 
                       " (", round(largest_segment$percentage[1], 1), "%)</li>"),
                paste0("<li><strong>Most balanced category:</strong> ", balanced_category$category[1], 
                       " shows the most even distribution across segments</li>"),
                "</ul>",
                "</div>"
            )
            
            self$results$interpretation$setContent(interpretation_html)
        },
        
        .updateDetailedStats = function() {
            
            data <- private$.processed_data
            composition_data <- private$.composition_data
            
            if (is.null(data) || is.null(composition_data)) {
                return()
            }
            
            # Calculate detailed statistics
            n_categories <- length(unique(data[[self$options$x_var]]))
            n_segments <- length(unique(data[[self$options$fill_var]]))
            total_obs <- sum(data$count, na.rm = TRUE)
            
            # Calculate min and max percentages
            min_pct <- round(min(composition_data$percentage, na.rm = TRUE), 1)
            max_pct <- round(max(composition_data$percentage, na.rm = TRUE), 1)
            mean_pct <- round(mean(composition_data$percentage, na.rm = TRUE), 1)
            
            # Calculate segment with highest variation
            segment_variation <- composition_data %>%
                dplyr::group_by(segment) %>%
                dplyr::summarise(
                    cv = sd(percentage, na.rm = TRUE) / mean(percentage, na.rm = TRUE),
                    .groups = 'drop'
                ) %>%
                dplyr::arrange(desc(cv)) %>%
                dplyr::slice(1)
            
            # Create statistics rows
            stats_rows <- list(
                list(measure = "Total Categories", value = as.character(n_categories)),
                list(measure = "Total Segments", value = as.character(n_segments)),
                list(measure = "Total Observations", value = as.character(total_obs)),
                list(measure = "Min Percentage", value = paste0(min_pct, "%")),
                list(measure = "Max Percentage", value = paste0(max_pct, "%")),
                list(measure = "Mean Percentage", value = paste0(mean_pct, "%")),
                list(measure = "Most Variable Segment", value = as.character(segment_variation$segment[1]))
            )
            
            # Add rows to table
            for (i in seq_along(stats_rows)) {
                self$results$detailed_stats$addRow(rowKey = i, values = stats_rows[[i]])
            }
            
            # Make the table visible if we have data
            self$results$detailed_stats$setVisible(TRUE)
        }
    )
)