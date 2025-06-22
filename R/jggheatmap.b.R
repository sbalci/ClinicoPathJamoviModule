
# This file is a generated template, your changes will not be overwritten

jggheatmapClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "jggheatmapClass",
    inherit = jggheatmapBase,
    private = list(
        .init = function() {
            if (is.null(self$data) || 
                (is.null(self$options$matrix_vars) && is.null(self$options$value_var))) {
                self$results$plot$setVisible(FALSE)
                return()
            }
        },
        
        .run = function() {
            # Early return if essential variables not specified
            if (is.null(self$options$matrix_vars) && is.null(self$options$value_var)) {
                return()
            }
            
            # Prepare data
            data <- self$data
            if (nrow(data) == 0) return()
            
            # Clean variable names
            if (requireNamespace('janitor', quietly = TRUE)) {
                data <- janitor::clean_names(data)
            }
            
            # Generate heatmap
            plot <- self$.create_heatmap(data)
            
            # Set plot with custom dimensions
            self$results$plot$setState(plot)
            self$results$plot$setSize(self$options$plot_width, self$options$plot_height)
            
            # Generate data matrix if requested
            if (self$options$output_format %in% c("data_matrix", "both")) {
                self$.populate_matrix_table(data)
            }
            
            # Generate clustering results if clustering is enabled
            if (self$options$cluster_rows || self$options$cluster_cols) {
                self$.populate_cluster_table(data)
            }
            
            # Generate interpretation
            self$.generate_interpretation(data)
        },
        
        .plot = function(image, ggtheme, theme, ...) {
            if (is.null(image$state))
                return(FALSE)
                
            plot <- image$state
            print(plot)
            
            TRUE
        },
        
        .create_heatmap = function(data) {
            # Load required packages
            packages <- c('ggplot2', 'ggheatmap', 'dplyr', 'stats')
            for (pkg in packages) {
                if (!requireNamespace(pkg, quietly = TRUE)) {
                    stop(paste0("Package '", pkg, "' is required but not installed"))
                }
            }
            
            # Prepare matrix data
            matrix_data <- self$.prepare_matrix_data(data)
            
            # Create ggheatmap plot
            plot <- self$.build_ggheatmap(matrix_data)
            
            return(plot)
        },
        
        .prepare_matrix_data = function(data) {
            if (!is.null(self$options$matrix_vars) && length(self$options$matrix_vars) > 0) {
                # Direct matrix from selected variables
                matrix_data <- data %>%
                    dplyr::select(!!!self$options$matrix_vars) %>%
                    as.matrix()
                
                # Remove non-numeric columns
                numeric_cols <- sapply(data[self$options$matrix_vars], is.numeric)
                if (!all(numeric_cols)) {
                    warning("Non-numeric variables removed from matrix")
                    matrix_data <- matrix_data[, numeric_cols, drop = FALSE]
                }
                
            } else if (!is.null(self$options$value_var) && 
                      !is.null(self$options$row_var) && 
                      !is.null(self$options$col_var)) {
                # Pivot data to matrix format
                matrix_data <- data %>%
                    dplyr::select(
                        row = !!self$options$row_var,
                        col = !!self$options$col_var,
                        value = !!self$options$value_var
                    ) %>%
                    tidyr::pivot_wider(names_from = col, values_from = value) %>%
                    tibble::column_to_rownames("row") %>%
                    as.matrix()
                    
            } else {
                stop("Please specify either Matrix Variables or Row/Column/Value variables")
            }
            
            # Apply scaling
            matrix_data <- self$.apply_scaling(matrix_data)
            
            return(matrix_data)
        },
        
        .apply_scaling = function(matrix_data) {
            if (self$options$scaling == "none") {
                return(matrix_data)
            }
            
            if (self$options$scaling == "row") {
                # Z-score scaling by rows
                matrix_data <- t(scale(t(matrix_data)))
            } else if (self$options$scaling == "column") {
                # Z-score scaling by columns
                matrix_data <- scale(matrix_data)
            } else if (self$options$scaling == "global") {
                # Global scaling
                matrix_data <- (matrix_data - mean(matrix_data, na.rm = TRUE)) / 
                              sd(matrix_data, na.rm = TRUE)
            }
            
            return(matrix_data)
        },
        
        .build_ggheatmap = function(matrix_data) {
            # Convert matrix to data frame for ggheatmap
            heatmap_data <- expand.grid(
                Row = rownames(matrix_data),
                Column = colnames(matrix_data)
            )
            heatmap_data$Value <- as.vector(matrix_data)
            
            # Create base plot
            plot <- ggplot2::ggplot(heatmap_data, ggplot2::aes(x = Column, y = Row, fill = Value)) +
                ggplot2::geom_tile()
            
            # Apply color scheme
            plot <- self$.apply_color_scheme(plot)
            
            # Apply clustering if requested
            if (self$options$cluster_rows || self$options$cluster_cols) {
                plot <- self$.apply_clustering(plot, matrix_data)
            }
            
            # Add cell values if requested
            if (self$options$show_values) {
                plot <- plot + ggplot2::geom_text(
                    ggplot2::aes(label = self$.format_values(Value)),
                    size = self$options$text_size / 3,
                    color = "black"
                )
            }
            
            # Apply styling
            plot <- self$.apply_styling(plot)
            
            return(plot)
        },
        
        .apply_color_scheme = function(plot) {
            color_scale <- switch(self$options$color_scheme,
                "blue_red" = ggplot2::scale_fill_gradient2(
                    low = "blue", mid = "white", high = "red",
                    name = self$options$colorbar_title
                ),
                "viridis" = ggplot2::scale_fill_viridis_c(
                    name = self$options$colorbar_title
                ),
                "plasma" = ggplot2::scale_fill_viridis_c(
                    option = "plasma",
                    name = self$options$colorbar_title
                ),
                "rdylbu" = ggplot2::scale_fill_distiller(
                    palette = "RdYlBu",
                    name = self$options$colorbar_title
                ),
                "spectral" = ggplot2::scale_fill_distiller(
                    palette = "Spectral",
                    name = self$options$colorbar_title
                ),
                ggplot2::scale_fill_gradient2(
                    low = "blue", mid = "white", high = "red",
                    name = self$options$colorbar_title
                )
            )
            
            plot <- plot + color_scale
            
            return(plot)
        },
        
        .apply_clustering = function(plot, matrix_data) {
            # Perform hierarchical clustering
            if (self$options$cluster_rows) {
                row_dist <- self$.calculate_distance(matrix_data, "row")
                row_clust <- stats::hclust(row_dist, method = self$options$clustering_method)
                plot <- plot + ggplot2::scale_y_discrete(limits = rownames(matrix_data)[row_clust$order])
            }
            
            if (self$options$cluster_cols) {
                col_dist <- self$.calculate_distance(matrix_data, "column")
                col_clust <- stats::hclust(col_dist, method = self$options$clustering_method)
                plot <- plot + ggplot2::scale_x_discrete(limits = colnames(matrix_data)[col_clust$order])
            }
            
            return(plot)
        },
        
        .calculate_distance = function(matrix_data, dimension) {
            if (dimension == "row") {
                data_for_dist <- matrix_data
            } else {
                data_for_dist <- t(matrix_data)
            }
            
            dist_method <- switch(self$options$distance_method,
                "euclidean" = "euclidean",
                "manhattan" = "manhattan",
                "maximum" = "maximum",
                "pearson" = {
                    return(as.dist(1 - cor(t(data_for_dist), use = "complete.obs")))
                },
                "spearman" = {
                    return(as.dist(1 - cor(t(data_for_dist), method = "spearman", use = "complete.obs")))
                },
                "euclidean"
            )
            
            if (dist_method %in% c("pearson", "spearman")) {
                return(dist_method)  # Already calculated above
            } else {
                return(stats::dist(data_for_dist, method = dist_method))
            }
        },
        
        .format_values = function(values) {
            format_type <- self$options$value_format
            
            formatted <- switch(format_type,
                "auto" = format(values, digits = 2),
                "integer" = as.character(round(values)),
                "decimal1" = sprintf("%.1f", values),
                "decimal2" = sprintf("%.2f", values),
                "scientific" = format(values, scientific = TRUE, digits = 2),
                format(values, digits = 2)
            )
            
            return(formatted)
        },
        
        .apply_styling = function(plot) {
            plot <- plot + 
                ggplot2::theme_minimal() +
                ggplot2::theme(
                    axis.text.x = ggplot2::element_text(
                        angle = 45, hjust = 1, 
                        size = self$options$col_label_size
                    ),
                    axis.text.y = ggplot2::element_text(
                        size = self$options$row_label_size
                    ),
                    axis.title = ggplot2::element_blank(),
                    panel.grid = ggplot2::element_blank()
                )
            
            # Add title if specified
            if (nchar(self$options$plot_title) > 0) {
                plot <- plot + ggplot2::ggtitle(self$options$plot_title)
            }
            
            # Hide color bar if requested
            if (!self$options$show_colorbar) {
                plot <- plot + ggplot2::theme(legend.position = "none")
            }
            
            # Hide labels if requested
            if (!self$options$show_row_labels) {
                plot <- plot + ggplot2::theme(axis.text.y = ggplot2::element_blank())
            }
            
            if (!self$options$show_col_labels) {
                plot <- plot + ggplot2::theme(axis.text.x = ggplot2::element_blank())
            }
            
            return(plot)
        },
        
        .populate_matrix_table = function(data) {
            matrix_data <- self$.prepare_matrix_data(data)
            
            # Convert matrix to long format for table
            matrix_long <- expand.grid(
                Row = rownames(matrix_data),
                Column = colnames(matrix_data)
            )
            matrix_long$Value <- as.vector(matrix_data)
            
            # Populate results table
            for (i in seq_len(nrow(matrix_long))) {
                self$results$matrixtab$addRow(
                    rowKey = i,
                    values = list(
                        row_name = as.character(matrix_long$Row[i]),
                        col_name = as.character(matrix_long$Column[i]),
                        value = matrix_long$Value[i]
                    )
                )
            }
        },
        
        .populate_cluster_table = function(data) {
            matrix_data <- self$.prepare_matrix_data(data)
            cluster_results <- list()
            
            if (self$options$cluster_rows) {
                row_dist <- self$.calculate_distance(matrix_data, "row")
                row_clust <- stats::hclust(row_dist, method = self$options$clustering_method)
                
                for (i in seq_along(row_clust$labels)) {
                    cluster_results <- append(cluster_results, list(
                        list(
                            item = rownames(matrix_data)[i],
                            cluster = stats::cutree(row_clust, k = min(3, nrow(matrix_data)))[i],
                            height = row_clust$height[i]
                        )
                    ))
                }
            }
            
            if (self$options$cluster_cols) {
                col_dist <- self$.calculate_distance(matrix_data, "column")
                col_clust <- stats::hclust(col_dist, method = self$options$clustering_method)
                
                for (i in seq_along(col_clust$labels)) {
                    cluster_results <- append(cluster_results, list(
                        list(
                            item = paste0("Col: ", colnames(matrix_data)[i]),
                            cluster = stats::cutree(col_clust, k = min(3, ncol(matrix_data)))[i],
                            height = col_clust$height[i]
                        )
                    ))
                }
            }
            
            # Populate results table
            for (i in seq_along(cluster_results)) {
                self$results$clustertab$addRow(
                    rowKey = i,
                    values = cluster_results[[i]]
                )
            }
        },
        
        .generate_interpretation = function(data) {
            matrix_data <- self$.prepare_matrix_data(data)
            
            n_rows <- nrow(matrix_data)
            n_cols <- ncol(matrix_data)
            n_values <- sum(!is.na(matrix_data))
            na_percent <- round(sum(is.na(matrix_data)) / length(matrix_data) * 100, 1)
            
            value_range <- range(matrix_data, na.rm = TRUE)
            
            interpretation <- paste0(
                "<h3>Heatmap Analysis Summary</h3>",
                "<p>This heatmap visualizes a ", n_rows, " × ", n_cols, " data matrix with ",
                n_values, " non-missing values (", 100 - na_percent, "% complete).</p>",
                
                "<p><strong>Data characteristics:</strong></p>",
                "<ul>",
                "<li>Value range: ", round(value_range[1], 3), " to ", round(value_range[2], 3), "</li>",
                "<li>Scaling method: ", self$options$scaling, "</li>",
                "<li>Color scheme: ", self$options$color_scheme, "</li>",
                "</ul>"
            )
            
            if (self$options$cluster_rows || self$options$cluster_cols) {
                interpretation <- paste0(interpretation,
                    "<p><strong>Clustering:</strong></p>",
                    "<ul>",
                    if (self$options$cluster_rows) "<li>Rows clustered using hierarchical clustering</li>" else "",
                    if (self$options$cluster_cols) "<li>Columns clustered using hierarchical clustering</li>" else "",
                    "<li>Distance method: ", self$options$distance_method, "</li>",
                    "<li>Linkage method: ", self$options$clustering_method, "</li>",
                    "</ul>"
                )
            }
            
            interpretation <- paste0(interpretation,
                "<p>The ggheatmap package provides advanced heatmap visualization capabilities ",
                "with flexible clustering, color schemes, and annotation options, making it ideal ",
                "for clinical research data analysis and visualization.</p>"
            )
            
            self$results$interpretation$setContent(interpretation)
        })
)
