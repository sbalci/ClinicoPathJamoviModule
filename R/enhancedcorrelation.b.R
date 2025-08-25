#' @title Enhanced Correlation Analysis
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import ggplot2
#' @importFrom stats cor.test complete.cases cor
#' @importFrom psych corr.test
#' @export

enhancedcorrelationClass <- R6::R6Class(
    "enhancedcorrelationClass",
    inherit = enhancedcorrelationBase,
    private = list(
        .init = function() {
            if (is.null(self$data) || is.null(self$options$vars) || length(self$options$vars) < 2)
                return()
            
            # Set up instructions
            html <- self$results$instructions
            html$setContent(
                '<html>
                <head>
                </head>
                <body>
                <div class="instructions">
                <h3>Enhanced Correlation Analysis</h3>
                <p>This analysis provides correlation coefficients with confidence intervals and significance tests.</p>
                <ul>
                <li><b>Pearson:</b> Linear correlation for continuous variables</li>
                <li><b>Spearman:</b> Non-parametric correlation based on ranks</li>
                <li><b>Kendall:</b> Non-parametric correlation based on concordance</li>
                </ul>
                <p><b>Note:</b> Spearman and Kendall correlations are robust to outliers and non-linear relationships.</p>
                </div>
                </body>
                </html>'
            )
            
            # Initialize details table
            details <- self$results$text
            n_vars <- length(vars)
            n_pairs <- n_vars * (n_vars - 1) / 2
            
            if (n_pairs > 0) {
                details$addRows(n_pairs)
            }
            
        },
        
        .run = function() {
            if (is.null(self$data) || is.null(self$options$vars) || length(self$options$vars) < 2)
                return()
            
            vars <- self$options$vars
            method <- self$options$method
            ci_level <- self$options$ciWidth / 100
            alpha_level <- self$options$sigLevel
            pvalue_adjustment <- self$options$pValueAdjustment
            
            # Get data
            data <- self$data
            for (var in vars) {
                data[[var]] <- jmvcore::toNumeric(data[[var]])
            }
            
            # Remove incomplete cases
            data <- data[complete.cases(data[vars]), vars, drop = FALSE]
            
            if (nrow(data) < 3) {
                self$results$instructions$setContent("Insufficient data for correlation analysis (need at least 3 complete cases).")
                return()
            }
            
            # Use psych package for comprehensive correlation analysis
            if (requireNamespace("psych", quietly = TRUE)) {
                private$.runComprehensiveCorrelations(data, vars, method, ci_level, pvalue_adjustment)
            } else {
                private$.runBasicCorrelations(data, vars, method, ci_level, alpha_level)
            }
        },
        
        .runComprehensiveCorrelations = function(data, vars, method, ci_level, pvalue_adjustment) {
            # Use psych::corr.test for comprehensive correlation analysis
            cor_result <- tryCatch({
                psych::corr.test(data[vars], method = method, alpha = 1 - ci_level, adjust = pvalue_adjustment)
            }, error = function(e) {
                warning("Failed to use psych package, falling back to basic correlations")
                return(NULL)
            })
            
            if (is.null(cor_result)) {
                private$.runBasicCorrelations(data, vars, method, ci_level, 1 - ci_level)
                return()
            }
            
            # Extract correlation matrix, p-values, and confidence intervals
            cor_matrix <- cor_result$r
            p_matrix <- if (pvalue_adjustment != "none") cor_result$p.adj else cor_result$p
            ci_lower <- cor_result$ci$lower
            ci_upper <- cor_result$ci$upper
            n_matrix <- cor_result$n
            
            # Populate correlation matrix table
            if (self$options$showMatrix) {
                matrix_table <- self$results$correlationMatrix
                matrix_table$addRows(length(vars))
                
                for (i in 1:length(vars)) {
                    row_values <- list(variable = vars[i])
                    
                    for (j in 1:length(vars)) {
                        if (i == j) {
                            row_values[[paste0("cor", j)]] <- 1.0
                        } else {
                            cor_val <- cor_matrix[i, j]
                            p_val <- p_matrix[i, j]
                            
                            # Format correlation with significance stars
                            if (self$options$significanceStars) {
                                stars <- ""
                                if (p_val < 0.001) stars <- "***"
                                else if (p_val < 0.01) stars <- "**"
                                else if (p_val < 0.05) stars <- "*"
                                
                                cor_text <- sprintf("%.3f%s", cor_val, stars)
                            } else {
                                cor_text <- sprintf("%.3f", cor_val)
                            }
                            
                            row_values[[paste0("cor", j)]] <- cor_text
                        }
                    }
                    
                    matrix_table$setRow(rowNo = i, values = row_values)
                }
            }
            
            # Populate detailed pairwise results
            details <- self$results$text
            row_idx <- 1
            
            for (i in 1:(length(vars) - 1)) {
                for (j in (i + 1):length(vars)) {
                    var1 <- vars[i]
                    var2 <- vars[j]
                    
                    cor_val <- cor_matrix[i, j]
                    p_val <- p_matrix[i, j]
                    n_val <- n_matrix[i, j]
                    
                    # Format confidence interval
                    ci_text <- ""
                    if (self$options$ci && !is.null(ci_lower) && !is.null(ci_upper)) {
                        ci_text <- sprintf("[%.3f, %.3f]", ci_lower[i, j], ci_upper[i, j])
                    }
                    
                    # Set row values
                    row_values <- list(
                        var1 = var1,
                        var2 = var2,
                        r = cor_val,
                        rCI = ci_text,
                        p = p_val,
                        n = n_val
                    )
                    
                    details$setRow(rowNo = row_idx, values = row_values)
                    row_idx <- row_idx + 1
                }
            }
        },
        
        .runBasicCorrelations = function(data, vars, method, ci_level, alpha_level) {
            # Fallback to basic correlation analysis
            details <- self$results$text
            row_idx <- 1
            
            p_values <- c()
            
            for (i in 1:(length(vars) - 1)) {
                for (j in (i + 1):length(vars)) {
                    var1 <- vars[i]
                    var2 <- vars[j]
                    
                    # Perform correlation test
                    test_result <- tryCatch({
                        cor.test(data[[var1]], data[[var2]], 
                                method = method, 
                                conf.level = ci_level)
                    }, error = function(e) NULL)
                    
                    if (!is.null(test_result)) {
                        p_values <- c(p_values, test_result$p.value)
                        
                        # Format confidence interval
                        ci_text <- ""
                        if (self$options$ci && !is.null(test_result$conf.int)) {
                            ci_text <- sprintf("[%.3f, %.3f]", 
                                             test_result$conf.int[1], 
                                             test_result$conf.int[2])
                        }
                        
                        # Set row values
                        row_values <- list(
                            var1 = var1,
                            var2 = var2,
                            r = test_result$estimate,
                            rCI = ci_text,
                            p = test_result$p.value,
                            n = sum(complete.cases(data[[var1]], data[[var2]]))
                        )
                        
                        details$setRow(rowNo = row_idx, values = row_values)
                    }
                    
                    row_idx <- row_idx + 1
                }
            }
            
            # Apply multiple comparison correction if requested
            if (self$options$pValueAdjustment != "none" && length(p_values) > 1) {
                adjusted_p <- p.adjust(p_values, method = self$options$pValueAdjustment)
                
                # Update the table with adjusted p-values
                for (i in 1:length(adjusted_p)) {
                    current_row <- details$getRow(rowNo = i)
                    current_row$p <- adjusted_p[i]
                    details$setRow(rowNo = i, values = current_row)
                }
            }
        },
        
        .plot = function(image, ggtheme, theme, ...) {
            if (is.null(self$data) || is.null(self$options$vars) || length(self$options$vars) < 2)
                return()
            
            vars <- self$options$vars
            data <- self$data
            
            # Convert to numeric
            for (var in vars) {
                data[[var]] <- jmvcore::toNumeric(data[[var]])
            }
            
            # Remove incomplete cases
            data <- data[complete.cases(data[vars]), vars, drop = FALSE]
            
            if (nrow(data) < 3)
                return()
            
            plot_type <- self$options$plotType
            
            if (plot_type == "matrix") {
                private$.createMatrixPlot(data, vars)
            } else if (plot_type == "web") {
                private$.createWebPlot(data, vars)
            } else if (plot_type == "network") {
                private$.createNetworkPlot(data, vars)
            } else if (plot_type == "scatter") {
                private$.createScatterPlot(data, vars)
            } else {
                FALSE
            }
        },
        
        .createMatrixPlot = function(data, vars) {
            # Enhanced correlation heatmap with better styling
            cor_matrix <- cor(data[vars], method = self$options$method, use = "complete.obs")
            
            # Convert to long format for ggplot
            cor_long <- expand.grid(Var1 = vars, Var2 = vars)
            cor_long$Correlation <- as.vector(cor_matrix)
            
            # Create significance matrix if p-values are available
            if (requireNamespace("psych", quietly = TRUE)) {
                cor_test <- psych::corr.test(data[vars], method = self$options$method)
                p_matrix <- cor_test$p
                p_long <- as.vector(p_matrix)
                cor_long$Significant <- p_long < 0.05
            } else {
                cor_long$Significant <- TRUE
            }
            
            p <- ggplot(cor_long, aes(x = Var1, y = Var2, fill = Correlation)) +
                geom_tile(aes(alpha = ifelse(Significant, 1, 0.5)), color = "white", size = 0.5) +
                geom_text(aes(label = sprintf("%.3f", Correlation)), 
                         color = ifelse(abs(cor_long$Correlation) > 0.5, "white", "black"), 
                         size = 3.5, fontface = "bold") +
                scale_fill_gradient2(
                    low = "#2166AC", high = "#762A83", mid = "white", 
                    midpoint = 0, limit = c(-1, 1),
                    guide = guide_colorbar(title = "Correlation")
                ) +
                scale_alpha_identity() +
                labs(
                    title = paste("Correlation Matrix (", toupper(self$options$method), ")", sep = ""),
                    subtitle = "Non-significant correlations (p ≥ 0.05) shown with reduced opacity",
                    x = "", y = ""
                ) +
                theme_minimal() +
                theme(
                    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
                    axis.text.y = element_text(size = 10),
                    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
                    plot.subtitle = element_text(hjust = 0.5, size = 10),
                    panel.grid = element_blank(),
                    legend.position = "right"
                )
            
            print(p)
            TRUE
        },
        
        .createWebPlot = function(data, vars) {
            # Create web/radar plot inspired by BlueSky's PlotWeb
            cor_matrix <- cor(data[vars], method = self$options$method, use = "complete.obs")
            
            # Create a network-like visualization using ggplot
            n_vars <- length(vars)
            
            # Create coordinates for variables in a circle
            angles <- seq(0, 2*pi, length.out = n_vars + 1)[1:n_vars]
            coords <- data.frame(
                var = vars,
                x = cos(angles),
                y = sin(angles)
            )
            
            # Create edges data frame for correlations
            edges <- data.frame()
            for (i in 1:(n_vars-1)) {
                for (j in (i+1):n_vars) {
                    cor_val <- cor_matrix[i, j]
                    if (abs(cor_val) > 0.1) {  # Only show correlations > 0.1
                        edges <- rbind(edges, data.frame(
                            var1 = vars[i],
                            var2 = vars[j],
                            correlation = cor_val,
                            x1 = coords$x[i],
                            y1 = coords$y[i],
                            x2 = coords$x[j],
                            y2 = coords$y[j]
                        ))
                    }
                }
            }
            
            p <- ggplot() +
                # Draw correlation lines
                geom_segment(data = edges, 
                           aes(x = x1, y = y1, xend = x2, yend = y2,
                               color = correlation, size = abs(correlation)),
                           alpha = 0.7) +
                # Draw variable points
                geom_point(data = coords, aes(x = x, y = y), 
                          size = 8, color = "darkblue", alpha = 0.8) +
                # Add variable labels
                geom_text(data = coords, aes(x = x * 1.15, y = y * 1.15, label = var),
                         size = 3.5, fontface = "bold") +
                scale_color_gradient2(
                    low = "#D73027", high = "#1A9850", mid = "white",
                    midpoint = 0, limit = c(-1, 1),
                    guide = guide_colorbar(title = "Correlation")
                ) +
                scale_size_continuous(range = c(0.5, 3), guide = "none") +
                labs(
                    title = "Correlation Web Plot",
                    subtitle = paste("Method:", toupper(self$options$method))
                ) +
                theme_void() +
                theme(
                    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
                    plot.subtitle = element_text(hjust = 0.5, size = 12),
                    legend.position = "right"
                ) +
                coord_equal()
            
            print(p)
            TRUE
        },
        
        .createNetworkPlot = function(data, vars) {
            # Create network plot showing correlation structure
            cor_matrix <- cor(data[vars], method = self$options$method, use = "complete.obs")
            
            # Transform correlation matrix to distance matrix
            dist_matrix <- 1 - abs(cor_matrix)
            
            # Use multidimensional scaling for layout
            mds_result <- cmdscale(dist_matrix, k = 2)
            
            coords <- data.frame(
                var = vars,
                x = mds_result[, 1],
                y = mds_result[, 2]
            )
            
            # Create edges for strong correlations
            edges <- data.frame()
            threshold <- ifelse(is.null(self$options$correlationThreshold), 0.3, self$options$correlationThreshold)
            
            for (i in 1:(length(vars)-1)) {
                for (j in (i+1):length(vars)) {
                    cor_val <- cor_matrix[i, j]
                    if (abs(cor_val) >= threshold) {
                        edges <- rbind(edges, data.frame(
                            var1 = vars[i],
                            var2 = vars[j],
                            correlation = cor_val,
                            x1 = coords$x[i],
                            y1 = coords$y[i],
                            x2 = coords$x[j],
                            y2 = coords$y[j]
                        ))
                    }
                }
            }
            
            p <- ggplot() +
                # Draw correlation edges
                geom_segment(data = edges,
                           aes(x = x1, y = y1, xend = x2, yend = y2,
                               color = correlation, size = abs(correlation)),
                           alpha = 0.6) +
                # Draw variable nodes
                geom_point(data = coords, aes(x = x, y = y),
                          size = 10, color = "steelblue", alpha = 0.8) +
                # Add variable labels
                geom_text(data = coords, aes(x = x, y = y, label = var),
                         color = "white", size = 3, fontface = "bold") +
                scale_color_gradient2(
                    low = "#E31A1C", high = "#1F78B4", mid = "lightgray",
                    midpoint = 0, limit = c(-1, 1),
                    name = "Correlation"
                ) +
                scale_size_continuous(range = c(1, 4), guide = "none") +
                labs(
                    title = "Correlation Network",
                    subtitle = paste("Showing correlations ≥", threshold, "| Method:", toupper(self$options$method))
                ) +
                theme_void() +
                theme(
                    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
                    plot.subtitle = element_text(hjust = 0.5, size = 12),
                    legend.position = "right"
                )
            
            print(p)
            TRUE
        },
        
        .createScatterPlot = function(data, vars) {
            # Enhanced scatter plot matrix or pairwise plots
            if (length(vars) == 2) {
                # Simple scatter plot for 2 variables
                cor_val <- cor(data[[vars[1]]], data[[vars[2]]], method = self$options$method)
                
                p <- ggplot(data, aes_string(x = vars[1], y = vars[2])) +
                    geom_point(alpha = 0.6, size = 2, color = "steelblue") +
                    geom_smooth(method = "lm", se = TRUE, color = "red", size = 1) +
                    labs(
                        title = paste("Scatterplot:", vars[1], "vs", vars[2]),
                        subtitle = paste("Correlation (", self$options$method, ") =", sprintf("%.3f", cor_val))
                    ) +
                    theme_minimal() +
                    theme(
                        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
                        plot.subtitle = element_text(hjust = 0.5, size = 12)
                    )
                
                print(p)
                TRUE
            } else {
                # For multiple variables, create pairs plot
                if (requireNamespace("GGally", quietly = TRUE)) {
                    p <- GGally::ggpairs(data[vars], 
                                       title = paste("Pairwise Correlations (", toupper(self$options$method), ")", sep = ""))
                    print(p)
                    TRUE
                } else {
                    # Fallback to first pair
                    return(private$.createScatterPlot(data, vars[1:2]))
                }
            }
        }
    )
)