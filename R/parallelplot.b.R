#' @title Parallel Coordinates Plot - Simplified Implementation
#' @description
#' Creates parallel coordinates plots for multivariate data visualization.
#' Parallel coordinates plots are excellent for exploring relationships
#' between multiple continuous variables and identifying patterns, clusters,
#' and outliers in high-dimensional data.
#'
#' @importFrom R6 R6Class
#' @import jmvcore
#' @importFrom ggplot2 ggplot aes geom_line theme_minimal labs scale_color_manual theme element_text
#' @importFrom dplyr mutate row_number
#' @importFrom tidyr pivot_longer
#' @importFrom scales hue_pal
#' @importFrom stats na.omit sd complete.cases
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
                        mean_val <- mean(data[[var]], na.rm = TRUE)
                        sd_val <- sd(data[[var]], na.rm = TRUE)
                        if (!is.na(sd_val) && sd_val > 0) {
                            scaled_data[[var]] <- (data[[var]] - mean_val) / sd_val
                        } else {
                            scaled_data[[var]] <- rep(0, nrow(data))
                        }
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
                if (n_groups <= 3) {
                    return(RColorBrewer::brewer.pal(3, "Set1")[1:n_groups])
                } else if (n_groups <= 9) {
                    return(RColorBrewer::brewer.pal(n_groups, "Set1"))
                } else {
                    # Extend Set1 palette for more groups
                    base_colors <- RColorBrewer::brewer.pal(9, "Set1")
                    return(grDevices::colorRampPalette(base_colors)(n_groups))
                }
            } else if (palette_name == "clinical") {
                # Clinical color palette - colorblind friendly
                clinical_colors <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd",
                                   "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf")
                return(rep(clinical_colors, length.out = n_groups))
            } else {
                # Default ggplot2 colors
                return(scales::hue_pal()(n_groups))
            }
        },

        .run = function() {

            # Check if variables are provided
            if (is.null(self$options$vars) || length(self$options$vars) == 0) {
                # Show instructions
                instructions_html <- "
                <div style='padding: 20px; background-color: #f8f9fa; border-left: 4px solid #007bff; margin: 10px 0;'>
                    <h3 style='color: #007bff; margin-top: 0;'>Welcome to Parallel Coordinates Plot</h3>
                    <h4>What is a Parallel Coordinates Plot?</h4>
                    <p>A parallel coordinates plot displays each data point as a connected line across multiple parallel axes (variables). It's excellent for:</p>
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
                        <li><strong>Choose Scaling:</strong> Select how to standardize variables with different scales</li>
                        <li><strong>Adjust Transparency:</strong> Use alpha to handle overplotting</li>
                    </ol>

                    <p><em>Start by selecting at least 2 continuous variables to create your parallel coordinates plot.</em></p>
                </div>"

                self$results$instructions$setContent(instructions_html)
                self$results$instructions$setVisible(TRUE)
                return()
            } else {
                self$results$instructions$setVisible(FALSE)
            }

            # Get data and options
            data <- self$data
            vars <- self$options$vars
            group_var <- self$options$group
            scaling_method <- self$options$scaling
            alpha <- self$options$alpha
            show_missing <- self$options$showMissing
            color_palette <- self$options$colorPalette

            # Filter to only include numeric variables
            numeric_vars <- character()
            for (v in vars) {
                if (v %in% names(data) && is.numeric(data[[v]])) {
                    numeric_vars <- c(numeric_vars, v)
                }
            }

            if (length(numeric_vars) < 2) {
                stop("At least 2 numeric variables are required for parallel coordinates plot")
            }

            # Prepare data
            if (!show_missing) {
                # Remove rows with missing values in selected variables
                complete_vars <- numeric_vars
                if (!is.null(group_var) && group_var != "" && group_var %in% names(data)) {
                    complete_vars <- c(complete_vars, group_var)
                }
                keep_rows <- complete.cases(data[complete_vars])
                data <- data[keep_rows, ]

                if (nrow(data) == 0) {
                    stop("No complete cases found. Consider enabling 'Show Missing Values' option.")
                }
            }

            # Scale the variables
            scaled_data <- private$.scaleVariables(data, numeric_vars, scaling_method)

            # Prepare plot data - handle NULL group_var
            if (!is.null(group_var) && group_var != "" && group_var %in% names(data)) {
                plot_data <- scaled_data[c(numeric_vars, group_var)]
            } else {
                plot_data <- scaled_data[numeric_vars]
                group_var <- NULL  # Ensure it's NULL for downstream logic
            }

            # Create summary statistics table
            for (i in seq_along(numeric_vars)) {
                var <- numeric_vars[i]
                var_data <- data[[var]]

                summary_stats <- list(
                    variable = var,
                    n = sum(!is.na(var_data)),
                    missing = sum(is.na(var_data)),
                    mean = round(mean(var_data, na.rm = TRUE), 3),
                    sd = round(stats::sd(var_data, na.rm = TRUE), 3),
                    min = round(min(var_data, na.rm = TRUE), 3),
                    max = round(max(var_data, na.rm = TRUE), 3)
                )

                self$results$summary$setRow(rowNo = i, values = summary_stats)
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
            if (is.null(vars) || length(vars) < 2) {
                return(FALSE)
            }

            # Filter to only include numeric variables
            numeric_vars <- character()
            for (v in vars) {
                if (v %in% names(data) && is.numeric(data[[v]])) {
                    numeric_vars <- c(numeric_vars, v)
                }
            }

            if (length(numeric_vars) < 2) {
                return(FALSE)
            }

            # Prepare data
            if (!show_missing) {
                # Remove rows with missing values
                complete_vars <- numeric_vars
                if (!is.null(group_var) && group_var != "" && group_var %in% names(data)) {
                    complete_vars <- c(complete_vars, group_var)
                }
                keep_rows <- complete.cases(data[complete_vars])
                data <- data[keep_rows, ]

                if (nrow(data) == 0) {
                    return(FALSE)
                }
            }

            # Scale the variables
            scaled_data <- private$.scaleVariables(data, numeric_vars, scaling_method)

            # Prepare plot data - handle NULL group_var
            if (!is.null(group_var) && group_var != "" && group_var %in% names(data)) {
                plot_data <- scaled_data[c(numeric_vars, group_var)]
            } else {
                plot_data <- scaled_data[numeric_vars]
                group_var <- NULL  # Ensure it's NULL for downstream logic
            }

            # Add row ID
            plot_data$._id <- seq_len(nrow(plot_data))

            # Reshape data for plotting
            if (!is.null(group_var)) {
                # Store group information before reshaping
                group_info <- plot_data[[group_var]]
                id_info <- plot_data$._id

                # Reshape only numeric columns
                plot_long <- data.frame()
                for (i in seq_len(nrow(plot_data))) {
                    for (var in numeric_vars) {
                        plot_long <- rbind(plot_long, data.frame(
                            id = id_info[i],
                            group = group_info[i],
                            variable = var,
                            value = plot_data[i, var],
                            stringsAsFactors = FALSE
                        ))
                    }
                }
                names(plot_long)[2] <- group_var

                # Ensure variable order is preserved
                plot_long$variable <- factor(plot_long$variable, levels = numeric_vars)

            } else {
                # Without grouping - simpler reshape
                plot_long <- data.frame()
                id_info <- plot_data$._id

                for (i in seq_len(nrow(plot_data))) {
                    for (var in numeric_vars) {
                        plot_long <- rbind(plot_long, data.frame(
                            id = id_info[i],
                            variable = var,
                            value = plot_data[i, var],
                            stringsAsFactors = FALSE
                        ))
                    }
                }

                # Ensure variable order is preserved
                plot_long$variable <- factor(plot_long$variable, levels = numeric_vars)
            }

            # Create static plot
            if (!is.null(group_var)) {
                # With grouping
                plot_long[[group_var]] <- as.factor(plot_long[[group_var]])
                n_groups <- length(unique(plot_long[[group_var]]))
                colors <- private$.getColorPalette(color_palette, n_groups)

                p <- ggplot2::ggplot(plot_long, ggplot2::aes(x = variable, y = value, group = id)) +
                    ggplot2::geom_line(ggplot2::aes_string(color = group_var), alpha = alpha, size = 0.5) +
                    ggplot2::scale_color_manual(values = colors, name = group_var)
            } else {
                # Without grouping
                p <- ggplot2::ggplot(plot_long, ggplot2::aes(x = variable, y = value, group = id)) +
                    ggplot2::geom_line(alpha = alpha, color = "#1f77b4", size = 0.5)
            }

            # Add theme and labels
            scaling_label <- switch(scaling_method,
                "std" = "Standardized (z-score)",
                "uniminmax" = "Scaled to [0,1]",
                "minmax" = "Scaled to [-1,1]",
                "none" = "Original Values"
            )

            p <- p +
                ggplot2::theme_minimal() +
                ggplot2::labs(
                    title = "Parallel Coordinates Plot",
                    x = "Variables",
                    y = scaling_label
                ) +
                ggplot2::theme(
                    axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
                    plot.title = ggplot2::element_text(hjust = 0.5, size = 14),
                    legend.position = if (!is.null(group_var)) "bottom" else "none"
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