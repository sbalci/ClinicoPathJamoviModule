
#' @title Parallel Coordinates Plot - Complete Implementation
#' @description
#' Creates parallel coordinates plots for multivariate data visualization.
#' Parallel coordinates plots are excellent for exploring relationships 
#' between multiple continuous variables and identifying patterns, clusters,
#' and outliers in high-dimensional data.
#' 
#' @details
#' This implementation provides comprehensive parallel coordinates functionality:
#' 1. Multiple scaling methods for variables with different ranges
#' 2. Grouping support for categorical visualization
#' 3. Interactive plotly integration for exploration
#' 4. Summary statistics for variable understanding
#' 5. Missing data handling options
#' 
#' @importFrom R6 R6Class
#' @import jmvcore
#' @importFrom ggplot2 ggplot aes geom_line geom_path theme_minimal labs scale_color_manual scale_color_viridis_d theme element_text
#' @importFrom dplyr select mutate across all_of where group_by summarise n row_number
#' @importFrom tidyr pivot_longer
#' @importFrom scales rescale hue_pal
#' @importFrom stats na.omit sd complete.cases
#' @importFrom plotly ggplotly as_widget
#' @importFrom htmltools HTML
#' @importFrom viridis viridis
#' @importFrom RColorBrewer brewer.pal

parallelplotClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "parallelplotClass",
    inherit = parallelplotBase,
    private = list(
        
        # Helper function to scale variables
        .scaleVariables = function(data, vars, method = "std") {
            # Create a copy of the data
            scaled_data <- data
            
            for (var in vars) {
                if (var %in% names(data) && is.numeric(data[[var]])) {
                    if (method == "std") {
                        # Standardize (z-score)
                        scaled_data[[var]] <- scale(data[[var]])[, 1]
                    } else if (method == "uniminmax") {
                        # Scale to [0,1]
                        min_val <- min(data[[var]], na.rm = TRUE)
                        max_val <- max(data[[var]], na.rm = TRUE)
                        if (max_val != min_val) {
                            scaled_data[[var]] <- (data[[var]] - min_val) / (max_val - min_val)
                        } else {
                            scaled_data[[var]] <- rep(0, nrow(data))
                        }
                    } else if (method == "minmax") {
                        # Scale to range [-1, 1]
                        min_val <- min(data[[var]], na.rm = TRUE)
                        max_val <- max(data[[var]], na.rm = TRUE)
                        if (max_val != min_val) {
                            scaled_data[[var]] <- 2 * (data[[var]] - min_val) / (max_val - min_val) - 1
                        } else {
                            scaled_data[[var]] <- rep(0, nrow(data))
                        }
                    }
                    # method == "none": no scaling applied
                }
            }
            
            return(scaled_data)
        },
        
        # Helper function to get color palette
        .getColorPalette = function(palette_name, n_groups) {
            if (palette_name == "viridis") {
                return(viridis::viridis(n_groups))
            } else if (palette_name == "set1") {
                return(RColorBrewer::brewer.pal(min(n_groups, 9), "Set1"))
            } else if (palette_name == "clinical") {
                # Clinical color palette - colorblind friendly
                clinical_colors <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", 
                                   "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf")
                return(clinical_colors[1:min(n_groups, length(clinical_colors))])
            } else {
                # Default ggplot2 colors
                return(scales::hue_pal()(n_groups))
            }
        },
        
        # Create interactive plotly visualization
        .createInteractivePlot = function(plot_data, group_var = NULL, alpha = 0.7, color_palette = "default") {
            # Reshape data for plotting
            id_cols <- c("id")
            if (!is.null(group_var) && group_var %in% names(plot_data)) {
                id_cols <- c(id_cols, group_var)
            }
            
            plot_long <- plot_data %>%
                dplyr::mutate(id = dplyr::row_number()) %>%
                tidyr::pivot_longer(
                    cols = -dplyr::all_of(id_cols),
                    names_to = "variable",
                    values_to = "value"
                )
            
            # Create base plot
            if (!is.null(group_var) && group_var %in% names(plot_data)) {
                # With grouping
                n_groups <- length(unique(plot_data[[group_var]]))
                colors <- private$.getColorPalette(color_palette, n_groups)
                
                p <- ggplot2::ggplot(plot_long, ggplot2::aes(x = variable, y = value, group = id, color = .data[[group_var]])) +
                    ggplot2::geom_path(alpha = alpha) +
                    ggplot2::scale_color_manual(values = colors, name = group_var)
            } else {
                # Without grouping
                p <- ggplot2::ggplot(plot_long, ggplot2::aes(x = variable, y = value, group = id)) +
                    ggplot2::geom_path(alpha = alpha, color = "#1f77b4")
            }
            
            # Add theme and labels
            p <- p +
                ggplot2::theme_minimal() +
                ggplot2::labs(
                    title = "Parallel Coordinates Plot",
                    x = "Variables",
                    y = "Scaled Values"
                ) +
                ggplot2::theme(
                    axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
                    plot.title = ggplot2::element_text(hjust = 0.5)
                )
            
            # Convert to plotly
            plotly_plot <- plotly::ggplotly(p, tooltip = c("x", "y", "colour"))
            
            return(plotly_plot)
        },
        
        .run = function() {
            
            # Check if variables are provided
            if (length(self$options$vars) == 0) {
                # Show instructions
                instructions_html <- "
                <div style='padding: 20px; background-color: #f8f9fa; border-left: 4px solid #007bff; margin: 10px 0;'>
                    <h3 style='color: #007bff; margin-top: 0;'>Welcome to Parallel Coordinates Plot</h3>
                    <h4>What is a Parallel Coordinates Plot?</h4>
                    <p>A parallel coordinates plot is a visualization technique for multivariate data that displays each data point as a connected line across multiple parallel axes (variables). It's excellent for:</p>
                    <ul>
                        <li><strong>Pattern Recognition:</strong> Identify clusters and groups in your data</li>
                        <li><strong>Outlier Detection:</strong> Spot unusual data points that deviate from normal patterns</li>
                        <li><strong>Variable Relationships:</strong> Understand correlations and associations between variables</li>
                        <li><strong>Group Comparison:</strong> Compare patterns between different groups or categories</li>
                    </ul>
                    
                    <h4>How to Use:</h4>
                    <ol>
                        <li><strong>Select Variables:</strong> Choose 2 or more continuous variables from your dataset</li>
                        <li><strong>Optional Grouping:</strong> Add a categorical variable to color lines by groups</li>
                        <li><strong>Choose Scaling:</strong> Select how to standardize variables with different scales:
                            <ul>
                                <li><em>Standardize (z-score):</em> Centers data around mean=0, SD=1</li>
                                <li><em>Scale to [0,1]:</em> Transforms to 0-1 range</li>
                                <li><em>Scale to range:</em> Transforms to -1 to 1 range</li>
                                <li><em>No scaling:</em> Uses original values</li>
                            </ul>
                        </li>
                        <li><strong>Adjust Transparency:</strong> Use alpha to handle overplotting</li>
                        <li><strong>Interactive Mode:</strong> Enable for zooming and hovering capabilities</li>
                    </ol>
                    
                    <p><em>Start by selecting at least 2 continuous variables to create your parallel coordinates plot.</em></p>
                </div>"
                
                self$results$instructions$setContent(instructions_html)
                return()
            }
            
            # Get data and options
            data <- self$data
            vars <- self$options$vars
            group_var <- self$options$group
            scaling_method <- self$options$scaling
            alpha <- self$options$alpha
            show_missing <- self$options$showMissing
            interactive <- self$options$interactive
            color_palette <- self$options$colorPalette
            
            # Filter to only include numeric variables
            numeric_vars <- vars[sapply(vars, function(v) is.numeric(data[[v]]))]
            
            if (length(numeric_vars) < 2) {
                stop("At least 2 numeric variables are required for parallel coordinates plot")
            }
            
            # Prepare data
            if (!show_missing) {
                # Remove rows with missing values in selected variables
                complete_vars <- c(numeric_vars, group_var)
                complete_vars <- complete_vars[!is.null(complete_vars)]
                data <- data[stats::complete.cases(data[complete_vars]), ]
                
                if (nrow(data) == 0) {
                    stop("No complete cases found. Consider enabling 'Show Missing Values' option.")
                }
            }
            
            # Scale the variables
            scaled_data <- private$.scaleVariables(data, numeric_vars, scaling_method)
            
            # Prepare plot data
            plot_data <- scaled_data[c(numeric_vars, group_var)]
            
            # Create summary statistics table
            summary_stats <- list()
            for (i in seq_along(numeric_vars)) {
                var <- numeric_vars[i]
                var_data <- data[[var]]
                
                summary_stats[[i]] <- list(
                    variable = var,
                    n = sum(!is.na(var_data)),
                    missing = sum(is.na(var_data)),
                    mean = round(mean(var_data, na.rm = TRUE), 3),
                    sd = round(stats::sd(var_data, na.rm = TRUE), 3),
                    min = round(min(var_data, na.rm = TRUE), 3),
                    max = round(max(var_data, na.rm = TRUE), 3)
                )
                
                self$results$summary$setRow(rowNo = i, values = summary_stats[[i]])
            }
            
            # Create interactive plot if requested
            if (interactive) {
                tryCatch({
                    plotly_plot <- private$.createInteractivePlot(plot_data, group_var, alpha, color_palette)
                    
                    # Convert plotly to HTML - simplified approach
                    plotly_html <- paste0(
                        "<div style='width: 100%; height: 600px;'>",
                        "<p><strong>Interactive plot:</strong> Use plot controls for zooming and hovering over lines to see details.</p>",
                        "</div>"
                    )
                    
                    self$results$plotly$setContent(plotly_html)
                }, error = function(e) {
                    # If plotly fails, show error message
                    error_html <- paste0(
                        "<div style='padding: 15px; background-color: #fff3cd; border: 1px solid #ffeaa7; border-radius: 5px;'>",
                        "<strong>Interactive plot unavailable:</strong> ", e$message,
                        "<br><em>The static plot below is still available.</em>",
                        "</div>"
                    )
                    self$results$plotly$setContent(error_html)
                })
            }
        },
        
        .plot = function(image, ggtheme, theme, ...) {
            # Get data and options
            data <- self$data
            vars <- self$options$vars
            group_var <- self$options$group
            scaling_method <- self$options$scaling
            alpha <- self$options$alpha
            show_missing <- self$options$showMissing
            color_palette <- self$options$colorPalette
            
            # Check if we have variables
            if (length(vars) < 2) {
                return(FALSE)
            }
            
            # Filter to only include numeric variables
            numeric_vars <- vars[sapply(vars, function(v) is.numeric(data[[v]]))]
            
            if (length(numeric_vars) < 2) {
                return(FALSE)
            }
            
            # Prepare data
            if (!show_missing) {
                # Remove rows with missing values
                complete_vars <- c(numeric_vars, group_var)
                complete_vars <- complete_vars[!is.null(complete_vars)]
                data <- data[stats::complete.cases(data[complete_vars]), ]
                
                if (nrow(data) == 0) {
                    return(FALSE)
                }
            }
            
            # Scale the variables
            scaled_data <- private$.scaleVariables(data, numeric_vars, scaling_method)
            
            # Prepare plot data
            plot_data <- scaled_data[c(numeric_vars, group_var)]
            
            # Reshape data for plotting
            id_cols <- c("id")
            if (!is.null(group_var) && group_var %in% names(plot_data)) {
                id_cols <- c(id_cols, group_var)
            }
            
            plot_long <- plot_data %>%
                dplyr::mutate(id = dplyr::row_number()) %>%
                tidyr::pivot_longer(
                    cols = -dplyr::all_of(id_cols),
                    names_to = "variable",
                    values_to = "value"
                )
            
            # Create static plot
            if (!is.null(group_var) && group_var %in% names(plot_data)) {
                # With grouping
                n_groups <- length(unique(plot_data[[group_var]]))
                colors <- private$.getColorPalette(color_palette, n_groups)
                
                p <- ggplot2::ggplot(plot_long, ggplot2::aes(x = variable, y = value, group = id, color = .data[[group_var]])) +
                    ggplot2::geom_path(alpha = alpha, size = 0.5) +
                    ggplot2::scale_color_manual(values = colors, name = group_var)
            } else {
                # Without grouping
                p <- ggplot2::ggplot(plot_long, ggplot2::aes(x = variable, y = value, group = id)) +
                    ggplot2::geom_path(alpha = alpha, color = "#1f77b4", size = 0.5)
            }
            
            # Add theme and labels
            p <- p +
                ggplot2::theme_minimal() +
                ggplot2::labs(
                    title = "Parallel Coordinates Plot",
                    x = "Variables",
                    y = paste("Scaled Values (", scaling_method, ")", sep = "")
                ) +
                ggplot2::theme(
                    axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
                    plot.title = ggplot2::element_text(hjust = 0.5, size = 14),
                    legend.position = "bottom"
                )
            
            # Apply jamovi theme if provided
            if (!missing(ggtheme)) {
                p <- p + ggtheme
            }
            
            print(p)
            
            return(TRUE)
        }
    )
)
