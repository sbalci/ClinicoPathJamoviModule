
#' @title Base Graphics Visualization
#' @description
#' Base R graphics visualization module providing fast, blazing fast, and extremely 
#' customizable data visualization solutions using pure base R graphics.
#' This module showcases the power and flexibility of base R plotting functions
#' without external dependencies.
#'
#' @details
#' This module implements the functionality requested in GitHub Issue #75,
#' providing comprehensive base R graphics visualization capabilities. Base R
#' graphics offer exceptional performance and unlimited customization potential
#' for clinical research and data visualization.
#'
#' Supported plot types:
#' - Scatter plots: Visualize relationships between continuous variables
#' - Line plots: Show trends and time series data
#' - Histograms: Display distribution of continuous variables
#' - Box plots: Compare distributions across groups
#' - Bar plots: Visualize categorical data frequencies
#' - Density plots: Smooth distribution visualization
#' - Pairs plots: Multiple variable relationships
#' - Matrix plots: Multiple series on same plot
#'
#' @param data A data frame containing the variables to plot.
#' @param plot_type Type of base R plot to generate.
#' @param x_var Variable for x-axis.
#' @param y_var Variable for y-axis (continuous plots).
#' @param group_var Optional grouping variable for stratified plots.
#' @param main_title Main title for the plot.
#' @param x_label Label for x-axis.
#' @param y_label Label for y-axis.
#'
#' @return Base R graphics plots with customizable styling and options.
#'
#' @importFrom R6 R6Class
#' @import jmvcore
#' @importFrom dplyr select all_of
#' @importFrom janitor clean_names
#' @importFrom labelled set_variable_labels var_label
#' @import magrittr

basegraphicsClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "basegraphicsClass",
    inherit = basegraphicsBase,
    private = list(
        
        # Internal data storage
        .processed_data = NULL,
        .plot_data = NULL,
        
        .init = function() {
            # Initialize instructions
            instructions_html <- paste(
                "<div style='background-color: #e8f5e8; padding: 20px; border-radius: 8px; margin: 10px 0;'>",
                "<h3 style='color: #2e7d32; margin-top: 0;'>Base Graphics Visualization</h3>",
                "<div style='margin: 10px 0;'>",
                "<h4 style='color: #388e3c; margin: 10px 0 5px 0;'>Fast & Customizable Base R Plots:</h4>",
                "<ul style='margin: 5px 0; padding-left: 20px; line-height: 1.6;'>",
                "<li><strong>Scatter Plots:</strong> Visualize relationships between continuous variables</li>",
                "<li><strong>Line Plots:</strong> Show trends and time series data</li>",
                "<li><strong>Histograms:</strong> Display distribution of continuous variables</li>",
                "<li><strong>Box Plots:</strong> Compare distributions across groups</li>",
                "<li><strong>Bar Plots:</strong> Visualize categorical data frequencies</li>",
                "<li><strong>Density Plots:</strong> Smooth distribution visualization</li>",
                "<li><strong>Pairs Plots:</strong> Multiple variable relationships</li>",
                "<li><strong>Matrix Plots:</strong> Multiple series on same plot</li>",
                "</ul>",
                "</div>",
                "<div style='margin: 10px 0;'>",
                "<h4 style='color: #388e3c; margin: 10px 0 5px 0;'>Quick Start:</h4>",
                "<ol style='margin: 5px 0; padding-left: 20px; line-height: 1.6;'>",
                "<li><strong>Choose Plot Type:</strong> Select from 8 base R plot types</li>",
                "<li><strong>Select Variables:</strong> Choose X and Y variables as appropriate</li>",
                "<li><strong>Optional Grouping:</strong> Add grouping variable for colored/stratified plots</li>",
                "<li><strong>Customize Appearance:</strong> Adjust titles, labels, colors, and styling</li>",
                "<li><strong>View Results:</strong> Generate fast, customizable base R graphics</li>",
                "</ol>",
                "</div>",
                "<div style='background-color: #fff8e1; padding: 10px; border-radius: 5px; margin: 10px 0;'>",
                "<p style='margin: 0; color: #f57c00;'><strong>Performance Note:</strong> Base R graphics are blazing fast and require no external dependencies, making them ideal for large datasets and rapid exploration.</p>",
                "</div>",
                "<p style='margin: 10px 0 0 0; color: #666; font-style: italic;'>💡 This module implements GitHub Issue #75 showcasing the power of base R graphics.</p>",
                "</div>"
            )
            
            self$results$instructions$setContent(instructions_html)
        },
        
        .run = function() {
            # Clear instructions if analysis is ready
            if (!is.null(self$options$x_var) && self$options$plot_type != "") {
                self$results$instructions$setContent("")
            }
            
            # Early validation
            if (is.null(self$options$x_var)) {
                return()
            }
            
            # Data validation
            if (nrow(self$data) == 0) {
                stop("Data contains no (complete) rows")
            }
            
            # Process data
            private$.processed_data <- private$.process_data()
            
            # Prepare plot data
            private$.prepare_plot_data()
            
            # Set plot state for rendering
            self$results$base_plot$setState(list(
                data = private$.plot_data,
                options = self$options
            ))
            
            # Generate plot description
            private$.generate_description()
        },
        
        .process_data = function() {
            mydata <- self$data
            
            # Store original names and labels
            original_names <- names(mydata)
            labels <- setNames(original_names, original_names)
            
            # Clean variable names
            mydata <- mydata %>% janitor::clean_names()
            
            # Restore labels to cleaned names
            corrected_labels <- setNames(original_names, names(mydata))
            mydata <- labelled::set_variable_labels(.data = mydata, .labels = corrected_labels)
            
            # Remove missing values for selected variables
            required_vars <- c(self$options$x_var)
            if (!is.null(self$options$y_var)) {
                required_vars <- c(required_vars, self$options$y_var)
            }
            if (!is.null(self$options$group_var)) {
                required_vars <- c(required_vars, self$options$group_var)
            }
            
            # Convert to cleaned names
            required_vars_clean <- janitor::make_clean_names(required_vars)
            mydata <- mydata[complete.cases(mydata[required_vars_clean]), ]
            
            return(mydata)
        },
        
        .prepare_plot_data = function() {
            mydata <- private$.processed_data
            
            # Convert variable names to cleaned versions
            x_var_clean <- janitor::make_clean_names(self$options$x_var)
            
            plot_data <- list(
                x = mydata[[x_var_clean]],
                x_name = self$options$x_var
            )
            
            # Add Y variable if specified
            if (!is.null(self$options$y_var)) {
                y_var_clean <- janitor::make_clean_names(self$options$y_var)
                plot_data$y <- mydata[[y_var_clean]]
                plot_data$y_name <- self$options$y_var
            }
            
            # Add grouping variable if specified
            if (!is.null(self$options$group_var)) {
                group_var_clean <- janitor::make_clean_names(self$options$group_var)
                plot_data$group <- as.factor(mydata[[group_var_clean]])
                plot_data$group_name <- self$options$group_var
            }
            
            private$.plot_data <- plot_data
        },
        
        .generate_description = function() {
            plot_type <- self$options$plot_type
            x_var <- self$options$x_var
            y_var <- self$options$y_var
            group_var <- self$options$group_var
            
            # Create description based on plot type
            desc_html <- "<h4>Base R Graphics Plot Description</h4>"
            desc_html <- paste0(desc_html, "<div style='background-color: #f5f5f5; padding: 15px; border-radius: 5px; margin: 10px 0;'>")
            
            if (plot_type == "scatter") {
                desc_html <- paste0(desc_html, "<p><strong>Scatter Plot:</strong> Shows the relationship between ", x_var)
                if (!is.null(y_var)) {
                    desc_html <- paste0(desc_html, " and ", y_var)
                }
                desc_html <- paste0(desc_html, ".</p>")
            } else if (plot_type == "histogram") {
                desc_html <- paste0(desc_html, "<p><strong>Histogram:</strong> Shows the distribution of ", x_var, ".</p>")
            } else if (plot_type == "boxplot") {
                desc_html <- paste0(desc_html, "<p><strong>Box Plot:</strong> Shows the distribution of ", x_var)
                if (!is.null(group_var)) {
                    desc_html <- paste0(desc_html, " grouped by ", group_var)
                }
                desc_html <- paste0(desc_html, ".</p>")
            } else if (plot_type == "barplot") {
                desc_html <- paste0(desc_html, "<p><strong>Bar Plot:</strong> Shows frequencies/counts for ", x_var, ".</p>")
            } else if (plot_type == "density") {
                desc_html <- paste0(desc_html, "<p><strong>Density Plot:</strong> Shows smooth density estimation for ", x_var, ".</p>")
            } else if (plot_type == "line") {
                desc_html <- paste0(desc_html, "<p><strong>Line Plot:</strong> Shows trend lines for the data.</p>")
            } else if (plot_type == "pairs") {
                desc_html <- paste0(desc_html, "<p><strong>Pairs Plot:</strong> Shows pairwise relationships between variables.</p>")
            } else if (plot_type == "matplot") {
                desc_html <- paste0(desc_html, "<p><strong>Matrix Plot:</strong> Shows multiple data series on the same plot.</p>")
            }
            
            if (!is.null(group_var)) {
                desc_html <- paste0(desc_html, "<p><strong>Grouping:</strong> Data is grouped by ", group_var, " with different colors/symbols.</p>")
            }
            
            desc_html <- paste0(desc_html, "<p><strong>Graphics Engine:</strong> Pure base R graphics (no external dependencies)</p>")
            desc_html <- paste0(desc_html, "<p><strong>Performance:</strong> Blazing fast rendering for large datasets</p>")
            desc_html <- paste0(desc_html, "</div>")
            
            self$results$plot_description$setContent(desc_html)
        },
        
        .add_statistics_to_plot = function(data, options) {
            # Add statistical information overlay to plots
            
            if (options$plot_type %in% c("scatter", "line") && !is.null(data$y)) {
                # For bivariate plots, add correlation
                cor_value <- cor(data$x, data$y, use = "complete.obs")
                stats_text <- paste0("r = ", round(cor_value, 3))
                
                # Add regression line for scatter plots
                if (options$plot_type == "scatter") {
                    abline(lm(data$y ~ data$x), col = "red", lty = 2)
                    lm_fit <- lm(data$y ~ data$x)
                    r_squared <- round(summary(lm_fit)$r.squared, 3)
                    stats_text <- paste0(stats_text, "\nR² = ", r_squared)
                }
                
                # Position text in upper-left corner
                mtext(stats_text, side = 3, line = -2, adj = 0.05, cex = 0.8, col = "blue")
                
            } else if (options$plot_type == "histogram") {
                # For histograms, add summary statistics
                mean_val <- round(mean(data$x, na.rm = TRUE), 2)
                median_val <- round(median(data$x, na.rm = TRUE), 2)
                sd_val <- round(sd(data$x, na.rm = TRUE), 2)
                
                stats_text <- paste0("Mean: ", mean_val, "\nMedian: ", median_val, "\nSD: ", sd_val)
                mtext(stats_text, side = 3, line = -4, adj = 0.95, cex = 0.8, col = "blue")
                
                # Add vertical lines for mean and median
                abline(v = mean_val, col = "red", lty = 2, lwd = 2)
                abline(v = median_val, col = "blue", lty = 3, lwd = 2)
                
            } else if (options$plot_type == "boxplot") {
                # For boxplots, add sample sizes
                if (!is.null(data$group)) {
                    group_counts <- table(data$group)
                    stats_text <- paste0("n = ", paste(group_counts, collapse = ", "))
                } else {
                    stats_text <- paste0("n = ", length(data$x))
                }
                mtext(stats_text, side = 3, line = -2, adj = 0.05, cex = 0.8, col = "blue")
                
            } else if (options$plot_type == "density") {
                # For density plots, add summary statistics
                mean_val <- round(mean(data$x, na.rm = TRUE), 2)
                median_val <- round(median(data$x, na.rm = TRUE), 2)
                
                stats_text <- paste0("Mean: ", mean_val, "\nMedian: ", median_val)
                mtext(stats_text, side = 3, line = -3, adj = 0.95, cex = 0.8, col = "blue")
                
                # Add vertical lines for mean and median
                abline(v = mean_val, col = "red", lty = 2)
                abline(v = median_val, col = "blue", lty = 3)
            }
        },
        
        # Plot rendering function
        .plot_base = function(image, ggtheme, theme, ...) {
            plot_state <- image$state
            if (is.null(plot_state)) return()
            
            data <- plot_state$data
            options <- plot_state$options
            
            # Set up plot parameters
            main_title <- if (options$main_title != "") options$main_title else paste(options$plot_type, "Plot")
            x_label <- if (options$x_label != "") options$x_label else data$x_name
            y_label <- if (options$y_label != "") options$y_label else if (!is.null(data$y_name)) data$y_name else "Values"
            
            # Set up colors
            if (!is.null(data$group)) {
                n_groups <- length(levels(data$group))
                if (options$color_scheme == "rainbow") {
                    colors <- rainbow(n_groups)
                } else if (options$color_scheme == "heat") {
                    colors <- heat.colors(n_groups)
                } else if (options$color_scheme == "terrain") {
                    colors <- terrain.colors(n_groups)
                } else if (options$color_scheme == "topo") {
                    colors <- topo.colors(n_groups)
                } else if (options$color_scheme == "cm") {
                    colors <- cm.colors(n_groups)
                } else {
                    colors <- 1:n_groups
                }
            } else {
                colors <- "black"
            }
            
            tryCatch({
                # Prepare custom axis limits
                xlim <- if (options$custom_limits && !is.null(options$x_min) && !is.null(options$x_max)) {
                    c(options$x_min, options$x_max)
                } else NULL
                ylim <- if (options$custom_limits && !is.null(options$y_min) && !is.null(options$y_max)) {
                    c(options$y_min, options$y_max)
                } else NULL
                
                # Generate plot based on type
                if (options$plot_type == "scatter") {
                    if (!is.null(data$y)) {
                        if (!is.null(data$group)) {
                            plot(data$x, data$y, 
                                col = colors[data$group], 
                                pch = as.numeric(options$point_type), 
                                cex = options$point_size,
                                main = main_title, 
                                xlab = x_label, 
                                ylab = y_label,
                                xlim = xlim, ylim = ylim)
                            if (options$add_legend) {
                                legend("topright", legend = levels(data$group), col = colors, pch = as.numeric(options$point_type))
                            }
                        } else {
                            plot(data$x, data$y, 
                                col = colors, 
                                pch = as.numeric(options$point_type), 
                                cex = options$point_size,
                                main = main_title, 
                                xlab = x_label, 
                                ylab = y_label,
                                xlim = xlim, ylim = ylim)
                        }
                    } else {
                        plot(data$x, 
                            col = colors, 
                            pch = as.numeric(options$point_type), 
                            cex = options$point_size,
                            main = main_title, 
                            xlab = x_label, 
                            ylab = "Index",
                            xlim = xlim, ylim = ylim)
                    }
                } else if (options$plot_type == "histogram") {
                    hist(data$x, 
                        breaks = options$bins,
                        main = main_title, 
                        xlab = x_label, 
                        ylab = "Frequency",
                        col = if (length(colors) == 1) colors else colors[1],
                        xlim = xlim, ylim = ylim)
                } else if (options$plot_type == "boxplot") {
                    if (!is.null(data$group)) {
                        boxplot(data$x ~ data$group, 
                               main = main_title, 
                               xlab = data$group_name, 
                               ylab = x_label,
                               col = colors,
                               ylim = ylim)  # xlim not applicable to boxplot with groups
                    } else {
                        boxplot(data$x, 
                               main = main_title, 
                               ylab = x_label,
                               col = if (length(colors) == 1) colors else colors[1],
                               ylim = ylim)
                    }
                } else if (options$plot_type == "barplot") {
                    if (is.factor(data$x) || is.character(data$x)) {
                        tab <- table(data$x)
                        barplot(tab, 
                               main = main_title, 
                               xlab = x_label, 
                               ylab = "Frequency",
                               col = colors,
                               ylim = ylim)  # xlim not meaningful for barplot
                    } else {
                        barplot(data$x, 
                               main = main_title, 
                               xlab = "Index", 
                               ylab = x_label,
                               col = colors,
                               ylim = ylim)
                    }
                } else if (options$plot_type == "density") {
                    if (!is.null(data$group)) {
                        groups <- levels(data$group)
                        first_group <- TRUE
                        for (i in seq_along(groups)) {
                            group_data <- data$x[data$group == groups[i]]
                            if (length(group_data) > 1) {
                                d <- density(group_data, na.rm = TRUE)
                                if (first_group) {
                                    plot(d, main = main_title, xlab = x_label, ylab = "Density", 
                                         col = colors[i], xlim = xlim, ylim = ylim)
                                    first_group <- FALSE
                                } else {
                                    lines(d, col = colors[i])
                                }
                            }
                        }
                        if (options$add_legend) {
                            legend("topright", legend = groups, col = colors, lty = 1)
                        }
                    } else {
                        d <- density(data$x, na.rm = TRUE)
                        plot(d, main = main_title, xlab = x_label, ylab = "Density", 
                             col = colors, xlim = xlim, ylim = ylim)
                    }
                } else if (options$plot_type == "line") {
                    if (!is.null(data$y)) {
                        plot(data$x, data$y, type = "l", 
                            main = main_title, 
                            xlab = x_label, 
                            ylab = y_label,
                            col = colors, xlim = xlim, ylim = ylim)
                    } else {
                        plot(data$x, type = "l", 
                            main = main_title, 
                            xlab = "Index", 
                            ylab = x_label,
                            col = colors, xlim = xlim, ylim = ylim)
                    }
                } else if (options$plot_type == "pairs") {
                    # Pairs plot for multiple variable relationships
                    mydata <- private$.processed_data
                    
                    # Select numeric variables for pairs plot
                    numeric_vars <- sapply(mydata, is.numeric)
                    if (sum(numeric_vars) < 2) {
                        plot(1, 1, type = "n", axes = FALSE, xlab = "", ylab = "")
                        text(1, 1, "Pairs plot requires at least 2 numeric variables\nPlease add more numeric variables to your dataset", 
                             cex = 1.2, col = "red")
                    } else {
                        numeric_data <- mydata[, numeric_vars, drop = FALSE]
                        
                        # Create pairs plot with customization
                        if (!is.null(data$group)) {
                            # Color by group
                            pairs(numeric_data, 
                                 main = main_title,
                                 col = colors[data$group],
                                 pch = as.numeric(options$point_type),
                                 cex = options$point_size)
                            
                            # Add legend in corner
                            if (options$add_legend) {
                                # Get plot coordinates for legend placement
                                par(xpd = TRUE)
                                legend(0.85, 0.95, legend = levels(data$group), 
                                      col = colors, pch = as.numeric(options$point_type),
                                      bty = "n", cex = 0.8)
                                par(xpd = FALSE)
                            }
                        } else {
                            pairs(numeric_data, 
                                 main = main_title,
                                 col = colors,
                                 pch = as.numeric(options$point_type),
                                 cex = options$point_size)
                        }
                    }
                } else if (options$plot_type == "matplot") {
                    # Matrix plot for multiple data series
                    mydata <- private$.processed_data
                    
                    # Select numeric variables for matrix plot
                    numeric_vars <- sapply(mydata, is.numeric)
                    if (sum(numeric_vars) < 2) {
                        plot(1, 1, type = "n", axes = FALSE, xlab = "", ylab = "")
                        text(1, 1, "Matrix plot requires at least 2 numeric variables\nPlease add more numeric variables to your dataset", 
                             cex = 1.2, col = "red")
                    } else {
                        numeric_data <- as.matrix(mydata[, numeric_vars, drop = FALSE])
                        
                        # Apply custom limits if specified
                        xlim <- if (options$custom_limits && !is.null(options$x_min) && !is.null(options$x_max)) {
                            c(options$x_min, options$x_max)
                        } else NULL
                        ylim <- if (options$custom_limits && !is.null(options$y_min) && !is.null(options$y_max)) {
                            c(options$y_min, options$y_max)
                        } else NULL
                        
                        # Create matrix plot
                        matplot(numeric_data, 
                               type = "l",
                               main = main_title,
                               xlab = x_label,
                               ylab = y_label,
                               col = colors,
                               lty = 1:ncol(numeric_data),
                               xlim = xlim, ylim = ylim)
                        
                        # Add legend
                        if (options$add_legend) {
                            legend("topright", legend = colnames(numeric_data), 
                                  col = colors, lty = 1:ncol(numeric_data), bty = "n")
                        }
                    }
                }
                
                # Add grid if requested
                if (options$add_grid) {
                    grid()
                }
                
                # Add statistics if requested
                if (options$show_statistics) {
                    private$.add_statistics_to_plot(data, options)
                }
                
                TRUE
            }, error = function(e) {
                plot(1, 1, type = "n", axes = FALSE, xlab = "", ylab = "")
                text(1, 1, paste("Error generating plot:\n", e$message), cex = 1.2, col = "red")
                TRUE
            })
        }
    )
)
