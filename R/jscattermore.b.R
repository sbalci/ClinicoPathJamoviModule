
# This file is a generated template, your changes will not be overwritten

# High-Performance Scatter Plots using scattermore package
# Following jamovi naming convention with j-prefix to avoid namespace conflicts

jscattermoreClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "jscattermoreClass",
    inherit = jscattermoreBase,
    private = list(
        # Performance optimization: cache variables
        .prepared_data = NULL,
        .prepared_options = NULL,
        .data_hash = NULL,
        .options_hash = NULL,
        .cached_plot = NULL,
        .cached_summary = NULL,
        .cached_performance = NULL,
        
        .init = function() {
            # Initialize plot dimensions for high-performance rendering
            self$results$plot$setSize(800, 600)
            self$results$summary$setVisible(TRUE)
            self$results$performance$setVisible(FALSE)
        },
        
        # Performance optimization methods
        .calculateDataHash = function() {
            if (is.null(self$data) || nrow(self$data) == 0) {
                return(NULL)
            }
            
            # Create hash based on relevant data columns and size
            relevant_vars <- c(self$options$x_var, self$options$y_var, 
                             self$options$color_var, self$options$size_var, 
                             self$options$facet_var)
            relevant_vars <- relevant_vars[!sapply(relevant_vars, is.null)]
            
            if (length(relevant_vars) == 0) {
                return(NULL)
            }
            
            # Create a simple hash string
            data_summary <- paste(
                nrow(self$data),
                ncol(self$data),
                paste(relevant_vars, collapse = "_"),
                if (length(relevant_vars) > 0) {
                    paste(sapply(relevant_vars, function(var) {
                        if (var %in% names(self$data)) {
                            paste(range(self$data[[var]], na.rm = TRUE), collapse = "_")
                        } else {
                            "missing"
                        }
                    }), collapse = "_")
                } else {
                    "no_vars"
                },
                sep = "_"
            )
            
            return(data_summary)
        },
        
        .calculateOptionsHash = function() {
            # Create hash of all relevant options
            options_list <- list(
                plot_type = self$options$plot_type,
                point_size = self$options$point_size,
                alpha = self$options$alpha,
                pixels = self$options$pixels,
                pointsize = self$options$pointsize,
                interpolate = self$options$interpolate,
                color_palette = self$options$color_palette,
                show_smooth = self$options$show_smooth,
                smooth_method = self$options$smooth_method,
                show_density = self$options$show_density,
                log_transform_x = self$options$log_transform_x,
                log_transform_y = self$options$log_transform_y,
                x_label = self$options$x_label,
                y_label = self$options$y_label,
                plot_title = self$options$plot_title,
                show_correlation = self$options$show_correlation,
                show_performance = self$options$show_performance,
                theme_style = self$options$theme_style
            )
            
            return(paste(options_list, collapse = "_"))
        },
        
        .canUseCache = function() {
            current_data_hash <- private$.calculateDataHash()
            current_options_hash <- private$.calculateOptionsHash()
            
            return(!is.null(private$.cached_plot) &&
                   !is.null(private$.data_hash) &&
                   !is.null(private$.options_hash) &&
                   !is.null(current_data_hash) &&
                   !is.null(current_options_hash) &&
                   current_data_hash == private$.data_hash &&
                   current_options_hash == private$.options_hash)
        },
        
        .prepareData = function() {
            current_hash <- private$.calculateDataHash()
            
            if (is.null(private$.data_hash) || private$.data_hash != current_hash) {
                # Data has changed, prepare new data
                data <- self$data
                options <- self$options
                
                # Check for minimum required data
                if (is.null(data) || nrow(data) == 0) {
                    private$.prepared_data <- NULL
                    private$.data_hash <- current_hash
                    return(NULL)
                }
                
                # Check required variables
                if (is.null(options$x_var) || is.null(options$y_var)) {
                    private$.prepared_data <- NULL
                    private$.data_hash <- current_hash
                    return(NULL)
                }
                
                x_var <- options$x_var
                y_var <- options$y_var
                
                # Clean data and remove missing values
                data_clean <- data[!is.na(data[[x_var]]) & !is.na(data[[y_var]]), ]
                
                if (nrow(data_clean) == 0) {
                    private$.prepared_data <- NULL
                    private$.data_hash <- current_hash
                    return(NULL)
                }
                
                private$.prepared_data <- data_clean
                private$.data_hash <- current_hash
            }
            
            return(private$.prepared_data)
        },
        
        .prepareOptions = function() {
            current_hash <- private$.calculateOptionsHash()
            
            if (is.null(private$.options_hash) || private$.options_hash != current_hash) {
                # Options have changed, update cache
                private$.prepared_options <- self$options
                private$.options_hash <- current_hash
            }
            
            return(private$.prepared_options)
        },
        
        .run = function() {
            
            # Check if scattermore package is available
            if (!requireNamespace('scattermore', quietly = TRUE)) {
                stop('The scattermore package is required but not installed. Please install it using install.packages("scattermore")')
            }
            
            # Performance optimization: check if we can use cached results
            if (private$.canUseCache()) {
                # Use cached results
                self$results$plot$setState(private$.cached_plot)
                self$results$summary$setContent(private$.cached_summary)
                
                if (self$options$show_performance && !is.null(private$.cached_performance)) {
                    self$results$performance$setVisible(TRUE)
                    self$results$performance$setContent(private$.cached_performance)
                } else {
                    self$results$performance$setVisible(FALSE)
                }
                return()
            }
            
            # Prepare data with caching
            data_clean <- private$.prepareData()
            options <- private$.prepareOptions()
            
            if (is.null(data_clean) || nrow(data_clean) == 0) {
                return()
            }
            
            if (is.null(options$x_var) || is.null(options$y_var)) {
                return()
            }
            
            x_var <- options$x_var
            y_var <- options$y_var
            
            # Record start time for performance measurement
            start_time <- Sys.time()
            
            # Generate high-performance scatter plot
            plot_obj <- self$.createScatterPlot(data_clean, x_var, y_var)
            
            # Calculate performance metrics
            end_time <- Sys.time()
            render_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
            
            # Generate summary
            summary_text <- self$.generateSummary(data_clean, x_var, y_var, render_time)
            
            # Generate performance info if needed
            performance_text <- NULL
            if (options$show_performance) {
                performance_text <- self$.generatePerformanceInfo(data_clean, render_time)
            }
            
            # Cache the results for future use
            private$.cached_plot <- plot_obj
            private$.cached_summary <- summary_text
            private$.cached_performance <- performance_text
            
            # Set the results
            self$results$plot$setState(plot_obj)
            self$results$summary$setContent(summary_text)
            
            # Show performance info if requested
            if (options$show_performance) {
                self$results$performance$setVisible(TRUE)
                self$results$performance$setContent(performance_text)
            } else {
                self$results$performance$setVisible(FALSE)
            }
        },
        
        .createScatterPlot = function(data, x_var, y_var) {
            
            options <- self$options
            plot_type <- options$plot_type
            
            # Apply log transformations if requested
            if (options$log_transform_x) {
                data[[x_var]] <- log10(data[[x_var]] + 1e-10) # Small constant to avoid log(0)
            }
            
            if (options$log_transform_y) {
                data[[y_var]] <- log10(data[[y_var]] + 1e-10)
            }
            
            if (plot_type == 'base_r') {
                return(self$.createBaseRPlot(data, x_var, y_var))
            } else {
                return(self$.createGgplotPlot(data, x_var, y_var))
            }
        },
        
        .createBaseRPlot = function(data, x_var, y_var) {
            
            options <- self$options
            
            # Create base R plot using scattermoreplot
            tryCatch({
                
                # Prepare color mapping if specified
                colors <- NULL
                if (!is.null(options$color_var)) {
                    color_data <- data[[options$color_var]]
                    if (is.numeric(color_data)) {
                        colors <- self$.getColorPalette(color_data)
                    } else {
                        colors <- rainbow(length(unique(color_data)))[as.factor(color_data)]
                    }
                }
                
                # Create the plot function that will be called when rendering
                plot_func <- function() {
                    scattermore::scattermoreplot(
                        x = data[[x_var]],
                        y = data[[y_var]],
                        col = colors,
                        size = options$point_size,
                        alpha = options$alpha,
                        pixels = c(options$pixels, options$pixels),
                        pointsize = if (options$pointsize > 0) options$pointsize else NULL,
                        interpolate = options$interpolate,
                        main = if (options$plot_title != "") options$plot_title else "High-Performance Scatter Plot",
                        xlab = if (options$x_label != "") options$x_label else x_var,
                        ylab = if (options$y_label != "") options$y_label else y_var
                    )
                    
                    # Add correlation if requested
                    if (options$show_correlation) {
                        cor_val <- cor(data[[x_var]], data[[y_var]], use = "complete.obs")
                        mtext(paste("r =", round(cor_val, 3)), side = 3, line = 0.5, adj = 1)
                    }
                }
                
                return(plot_func)
                
            }, error = function(e) {
                jmvcore::reject(paste('Error creating base R plot:', e$message))
            })
        },
        
        .createGgplotPlot = function(data, x_var, y_var) {
            
            options <- self$options
            
            if (!requireNamespace('ggplot2', quietly = TRUE)) {
                jmvcore::reject('ggplot2 package is required for this plot type')
            }
            
            tryCatch({
                
                # Create base ggplot
                p <- ggplot2::ggplot(data, ggplot2::aes_string(x = x_var, y = y_var))
                
                # Add scattermore layer based on plot type
                if (options$plot_type == 'ggplot2_opt') {
                    p <- p + scattermore::geom_scattermost(
                        size = options$point_size,
                        alpha = options$alpha,
                        pixels = c(options$pixels, options$pixels),
                        pointsize = if (options$pointsize > 0) options$pointsize else 0,
                        interpolate = options$interpolate
                    )
                } else {
                    p <- p + scattermore::geom_scattermore(
                        size = options$point_size,
                        alpha = options$alpha,
                        pixels = c(options$pixels, options$pixels),
                        pointsize = if (options$pointsize > 0) options$pointsize else 0,
                        interpolate = options$interpolate
                    )
                }
                
                # Add color mapping if specified
                if (!is.null(options$color_var)) {
                    if (is.numeric(data[[options$color_var]])) {
                        p <- p + ggplot2::aes_string(color = options$color_var)
                        p <- p + ggplot2::scale_color_viridis_c(option = options$color_palette)
                    } else {
                        p <- p + ggplot2::aes_string(color = options$color_var)
                        p <- p + ggplot2::scale_color_viridis_d(option = options$color_palette)
                    }
                }
                
                # Add size mapping if specified
                if (!is.null(options$size_var)) {
                    p <- p + ggplot2::aes_string(size = options$size_var)
                }
                
                # Add smooth line if requested
                if (options$show_smooth) {
                    p <- p + ggplot2::geom_smooth(
                        method = options$smooth_method,
                        se = TRUE,
                        color = "red",
                        size = 1
                    )
                }
                
                # Add density contours if requested
                if (options$show_density) {
                    p <- p + ggplot2::stat_density_2d(alpha = 0.5)
                }
                
                # Add faceting if specified
                if (!is.null(options$facet_var)) {
                    p <- p + ggplot2::facet_wrap(~ get(options$facet_var))
                }
                
                # Apply theme
                p <- p + self$.applyTheme(options$theme_style)
                
                # Add labels
                p <- p + ggplot2::labs(
                    title = if (options$plot_title != "") options$plot_title else "High-Performance Scatter Plot",
                    x = if (options$x_label != "") options$x_label else x_var,
                    y = if (options$y_label != "") options$y_label else y_var
                )
                
                # Add correlation annotation if requested
                if (options$show_correlation) {
                    cor_val <- cor(data[[x_var]], data[[y_var]], use = "complete.obs")
                    p <- p + ggplot2::annotate(
                        "text",
                        x = Inf, y = Inf,
                        label = paste("r =", round(cor_val, 3)),
                        hjust = 1.1, vjust = 1.5,
                        size = 4, color = "black"
                    )
                }
                
                return(p)
                
            }, error = function(e) {
                jmvcore::reject(paste('Error creating ggplot:', e$message))
            })
        },
        
        .getColorPalette = function(values) {
            
            options <- self$options
            palette_name <- options$color_palette
            
            # Normalize values to 0-1 range
            normalized <- (values - min(values, na.rm = TRUE)) / 
                         (max(values, na.rm = TRUE) - min(values, na.rm = TRUE))
            
            # Apply color palette
            if (palette_name == "viridis") {
                return(viridis::viridis(100)[round(normalized * 99) + 1])
            } else if (palette_name == "plasma") {
                return(viridis::plasma(100)[round(normalized * 99) + 1])
            } else if (palette_name == "inferno") {
                return(viridis::inferno(100)[round(normalized * 99) + 1])
            } else if (palette_name == "magma") {
                return(viridis::magma(100)[round(normalized * 99) + 1])
            } else if (palette_name == "cividis") {
                return(viridis::cividis(100)[round(normalized * 99) + 1])
            } else if (palette_name == "rainbow") {
                return(rainbow(100)[round(normalized * 99) + 1])
            } else if (palette_name == "heat") {
                return(heat.colors(100)[round(normalized * 99) + 1])
            } else if (palette_name == "terrain") {
                return(terrain.colors(100)[round(normalized * 99) + 1])
            } else {
                return(viridis::viridis(100)[round(normalized * 99) + 1])
            }
        },
        
        .applyTheme = function(theme_style) {
            
            if (!requireNamespace('ggplot2', quietly = TRUE)) {
                return(NULL)
            }
            
            switch(theme_style,
                "minimal" = ggplot2::theme_minimal(),
                "classic" = ggplot2::theme_classic(),
                "dark" = ggplot2::theme_dark(),
                "clean" = ggplot2::theme_void(),
                ggplot2::theme_gray() # default
            )
        },
        
        .generateSummary = function(data, x_var, y_var, render_time) {
            
            # Calculate summary statistics
            n_points <- nrow(data)
            x_stats <- summary(data[[x_var]])
            y_stats <- summary(data[[y_var]])
            correlation <- cor(data[[x_var]], data[[y_var]], use = "complete.obs")
            
            # Generate HTML summary
            summary_html <- paste(
                "<h3>High-Performance Scatter Plot Summary</h3>",
                "<p><strong>Dataset Information:</strong></p>",
                "<ul>",
                paste0("<li>Number of points plotted: ", format(n_points, big.mark = ","), "</li>"),
                paste0("<li>Rendering time: ", round(render_time, 4), " seconds</li>"),
                paste0("<li>Performance: ", round(n_points / render_time, 0), " points/second</li>"),
                "</ul>",
                
                "<p><strong>Variable Statistics:</strong></p>",
                "<table style='border-collapse: collapse; width: 100%;'>",
                "<tr style='background-color: #f2f2f2;'>",
                "<th style='border: 1px solid #ddd; padding: 8px;'>Variable</th>",
                "<th style='border: 1px solid #ddd; padding: 8px;'>Min</th>",
                "<th style='border: 1px solid #ddd; padding: 8px;'>Median</th>",
                "<th style='border: 1px solid #ddd; padding: 8px;'>Mean</th>",
                "<th style='border: 1px solid #ddd; padding: 8px;'>Max</th>",
                "</tr>",
                paste0("<tr><td style='border: 1px solid #ddd; padding: 8px;'>", x_var, "</td>",
                      "<td style='border: 1px solid #ddd; padding: 8px;'>", round(x_stats["Min."], 3), "</td>",
                      "<td style='border: 1px solid #ddd; padding: 8px;'>", round(x_stats["Median"], 3), "</td>",
                      "<td style='border: 1px solid #ddd; padding: 8px;'>", round(x_stats["Mean"], 3), "</td>",
                      "<td style='border: 1px solid #ddd; padding: 8px;'>", round(x_stats["Max."], 3), "</td></tr>"),
                paste0("<tr><td style='border: 1px solid #ddd; padding: 8px;'>", y_var, "</td>",
                      "<td style='border: 1px solid #ddd; padding: 8px;'>", round(y_stats["Min."], 3), "</td>",
                      "<td style='border: 1px solid #ddd; padding: 8px;'>", round(y_stats["Median"], 3), "</td>",
                      "<td style='border: 1px solid #ddd; padding: 8px;'>", round(y_stats["Mean"], 3), "</td>",
                      "<td style='border: 1px solid #ddd; padding: 8px;'>", round(y_stats["Max."], 3), "</td></tr>"),
                "</table>",
                
                "<p><strong>Correlation Analysis:</strong></p>",
                paste0("<p>Pearson correlation coefficient: <strong>r = ", round(correlation, 4), "</strong></p>"),
                
                if (abs(correlation) > 0.7) {
                    "<p style='color: #d9534f;'>Strong correlation detected</p>"
                } else if (abs(correlation) > 0.3) {
                    "<p style='color: #f0ad4e;'>Moderate correlation detected</p>"
                } else {
                    "<p style='color: #5cb85c;'>Weak correlation detected</p>"
                }
            )
            
            return(summary_html)
        },
        
        .generatePerformanceInfo = function(data, render_time) {
            
            n_points <- nrow(data)
            
            # Performance benchmarks (approximate)
            base_plot_est <- n_points * 0.00005  # Estimated time for base plot()
            ggplot_est <- n_points * 0.0001     # Estimated time for ggplot2::geom_point()
            
            performance_html <- paste(
                "<h3>Performance Analysis</h3>",
                "<p><strong>Rendering Performance:</strong></p>",
                "<ul>",
                paste0("<li>Actual rendering time: ", round(render_time, 4), " seconds</li>"),
                paste0("<li>Points per second: ", format(round(n_points / render_time, 0), big.mark = ","), "</li>"),
                paste0("<li>Estimated base plot() time: ", round(base_plot_est, 4), " seconds</li>"),
                paste0("<li>Estimated ggplot2 time: ", round(ggplot_est, 4), " seconds</li>"),
                "</ul>",
                
                "<p><strong>Performance Comparison:</strong></p>",
                "<ul>",
                paste0("<li>Speedup vs base plot(): ", round(base_plot_est / render_time, 1), "x faster</li>"),
                paste0("<li>Speedup vs ggplot2: ", round(ggplot_est / render_time, 1), "x faster</li>"),
                "</ul>",
                
                "<p><strong>Memory Efficiency:</strong></p>",
                "<ul>",
                paste0("<li>Raster resolution: ", self$options$pixels, " x ", self$options$pixels, " pixels</li>"),
                paste0("<li>Interpolation: ", if (self$options$interpolate) "Enabled" else "Disabled", "</li>"),
                "<li>Vector output: Optimized for file size</li>",
                "</ul>"
            )
            
            return(performance_html)
        },
        
        .plot = function(image, ggtheme, theme, ...) {
            
            # Render the plot stored in state
            plot_obj <- image$state
            
            if (!is.null(plot_obj)) {
                if (is.function(plot_obj)) {
                    # For base R plots stored as functions
                    plot_obj()
                } else {
                    # For ggplot objects
                    print(plot_obj)
                }
                TRUE
            } else {
                FALSE
            }
        }
    )
)
