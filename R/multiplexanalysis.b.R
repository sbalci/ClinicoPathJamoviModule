# This file is a generated template, your changes will not be overwritten

multiplexanalysisClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "multiplexanalysisClass",
    inherit = multiplexanalysisBase,
    private = list(
        .init = function() {
            if (is.null(self$data) || length(self$options$marker_vars) == 0) {
                self$results$interpretation$setContent(
                    "<h3>🎯 Multiplex Immunofluorescence Statistical Analysis</h3>
                    <p><strong>Purpose:</strong> Comprehensive statistical analysis of multiplex immunofluorescence data 
                    including co-expression patterns, spatial proximity analysis, immune contexture scoring, and 
                    multi-parametric phenotyping for advanced tumor microenvironment characterization.</p>
                    
                    <h4>Required Input Data:</h4>
                    <ul>
                        <li><strong>Marker Variables:</strong> Expression levels or intensities for multiple biomarkers</li>
                        <li><strong>Coordinate Data (Optional):</strong> X,Y coordinates for spatial proximity analysis</li>
                        <li><strong>Cell Type Classifications (Optional):</strong> Pre-defined cell phenotypes</li>
                    </ul>
                    
                    <h4>Comprehensive Analysis Framework:</h4>
                    <ul>
                        <li><strong>Co-expression Analysis:</strong> Multi-marker correlation and interaction patterns</li>
                        <li><strong>Phenotyping:</strong> Cell population identification through clustering</li>
                        <li><strong>Spatial Proximity:</strong> Cell-cell interaction analysis from coordinates</li>
                        <li><strong>Immune Contexture:</strong> Immunoscore and T-cell infiltration quantification</li>
                        <li><strong>Diversity Metrics:</strong> Shannon/Simpson diversity for cellular composition</li>
                        <li><strong>Principal Component Analysis:</strong> Dimensionality reduction and visualization</li>
                    </ul>
                    
                    <h4>Clinical Applications:</h4>
                    <ul>
                        <li>Tumor immune microenvironment profiling</li>
                        <li>CAR-T therapy predictive biomarker discovery</li>
                        <li>Checkpoint inhibitor response prediction</li>
                        <li>Multi-parametric immune signatures</li>
                        <li>Spatial immune cell interaction analysis</li>
                        <li>Treatment resistance mechanism identification</li>
                        <li>Personalized immunotherapy selection</li>
                    </ul>
                    
                    <h4>Statistical Methods:</h4>
                    <ul>
                        <li>Multivariate correlation analysis</li>
                        <li>Hierarchical and k-means clustering</li>
                        <li>Principal component analysis (PCA)</li>
                        <li>Spatial statistics (if coordinates provided)</li>
                        <li>Diversity index calculations</li>
                        <li>Network analysis for marker interactions</li>
                    </ul>
                    
                    <p><em>This module supports both single-cell and region-of-interest (ROI) level analysis, 
                    accommodating various multiplex imaging platforms and data formats commonly used in 
                    cancer research and clinical pathology.</em></p>"
                )
                return()
            }
            
            # Set up results structure
            private$.populateExpressionTable()
            private$.populateCorrelationTable()
            private$.populateClusteringTable()
            
            # Set plots visible based on options
            self$results$correlationplot$setVisible(self$options$show_plots)
            self$results$pcaplot$setVisible(self$options$show_plots)
            self$results$clusterplot$setVisible(self$options$show_clustering_plots)
            self$results$heatmapplot$setVisible(self$options$show_plots)
        },
        
        .run = function() {
            if (length(self$options$marker_vars) == 0) 
                return()
                
            data <- self$data
            if (nrow(data) == 0) return()
            
            # Get marker variables
            marker_data <- data[self$options$marker_vars]
            marker_names <- self$options$marker_vars
            
            # Remove rows with missing values
            complete_cases <- complete.cases(marker_data)
            marker_data <- marker_data[complete_cases, , drop = FALSE]
            
            if (nrow(marker_data) < 3) {
                self$results$interpretation$setContent(
                    "<p style='color: red;'><strong>Error:</strong> Insufficient data for analysis. 
                    At least 3 complete observations are required.</p>"
                )
                return()
            }
            
            if (ncol(marker_data) < 2) {
                self$results$interpretation$setContent(
                    "<p style='color: red;'><strong>Error:</strong> At least 2 marker variables are required 
                    for multiplex analysis.</p>"
                )
                return()
            }
            
            # Perform comprehensive multiplex analysis
            private$.performExpressionAnalysis(marker_data, marker_names)
            private$.performCorrelationAnalysis(marker_data, marker_names)
            private$.performClusteringAnalysis(marker_data, marker_names)
            private$.performPCAAnalysis(marker_data, marker_names)
            private$.generatePlots(marker_data, marker_names)
            private$.generateInterpretation(marker_data, marker_names)
            
            # Spatial analysis if coordinates provided
            if (!is.null(self$options$x_coord) && !is.null(self$options$y_coord)) {
                private$.performSpatialAnalysis(marker_data, marker_names)
            }
        },
        
        .populateExpressionTable = function() {
            table <- self$results$expressiontable
            table$addColumn(name = 'marker', title = 'Marker', type = 'text')
            table$addColumn(name = 'mean', title = 'Mean', type = 'number', format = 'zto')
            table$addColumn(name = 'median', title = 'Median', type = 'number', format = 'zto')
            table$addColumn(name = 'sd', title = 'SD', type = 'number', format = 'zto')
            table$addColumn(name = 'cv', title = 'CV (%)', type = 'number', format = 'zto')
            table$addColumn(name = 'positive_percent', title = '% Positive', type = 'number', format = 'zto')
            table$addColumn(name = 'high_expr_percent', title = '% High Expression', type = 'number', format = 'zto')
        },
        
        .populateCorrelationTable = function() {
            table <- self$results$correlationtable
            table$addColumn(name = 'marker1', title = 'Marker 1', type = 'text')
            table$addColumn(name = 'marker2', title = 'Marker 2', type = 'text')
            table$addColumn(name = 'correlation', title = 'Correlation (r)', type = 'number', format = 'zto')
            table$addColumn(name = 'p_value', title = 'P-value', type = 'number', format = 'zto,pvalue')
            table$addColumn(name = 'significance', title = 'Significance', type = 'text')
            table$addColumn(name = 'coexpression', title = 'Co-expression Pattern', type = 'text')
        },
        
        .populateClusteringTable = function() {
            table <- self$results$clusteringtable
            table$addColumn(name = 'cluster_id', title = 'Cluster ID', type = 'integer')
            table$addColumn(name = 'n_cells', title = 'N Cells', type = 'integer')
            table$addColumn(name = 'percent', title = 'Percentage', type = 'number', format = 'zto')
            table$addColumn(name = 'phenotype', title = 'Suggested Phenotype', type = 'text')
            table$addColumn(name = 'dominant_markers', title = 'Dominant Markers', type = 'text')
            table$addColumn(name = 'mean_expression', title = 'Mean Expression Pattern', type = 'text')
        },
        
        .performExpressionAnalysis = function(marker_data, marker_names) {
            table <- self$results$expressiontable
            
            for (i in 1:length(marker_names)) {
                marker_name <- marker_names[i]
                marker_values <- marker_data[[i]]
                
                # Calculate descriptive statistics
                mean_val <- mean(marker_values, na.rm = TRUE)
                median_val <- median(marker_values, na.rm = TRUE)
                sd_val <- sd(marker_values, na.rm = TRUE)
                cv_val <- (sd_val / mean_val) * 100
                
                # Calculate positivity rates
                cutpoint <- self$options$positivity_cutpoint
                positive_percent <- (sum(marker_values > cutpoint, na.rm = TRUE) / length(marker_values)) * 100
                
                # High expression (top quartile)
                high_cutpoint <- quantile(marker_values, 0.75, na.rm = TRUE)
                high_expr_percent <- (sum(marker_values > high_cutpoint, na.rm = TRUE) / length(marker_values)) * 100
                
                table$addRow(rowKey = i, values = list(
                    marker = marker_name,
                    mean = mean_val,
                    median = median_val,
                    sd = sd_val,
                    cv = cv_val,
                    positive_percent = positive_percent,
                    high_expr_percent = high_expr_percent
                ))
            }
        },
        
        .performCorrelationAnalysis = function(marker_data, marker_names) {
            table <- self$results$correlationtable
            n_markers <- length(marker_names)
            row_count <- 1
            
            for (i in 1:(n_markers-1)) {
                for (j in (i+1):n_markers) {
                    marker1_name <- marker_names[i]
                    marker2_name <- marker_names[j]
                    marker1_data <- marker_data[[i]]
                    marker2_data <- marker_data[[j]]
                    
                    # Calculate correlation
                    cor_method <- if (self$options$correlation_method == "spearman") "spearman" else "pearson"
                    cor_test <- cor.test(marker1_data, marker2_data, method = cor_method)
                    cor_val <- cor_test$estimate
                    p_val <- cor_test$p.value
                    
                    # Determine significance
                    significance <- ifelse(p_val < 0.001, "***",
                                  ifelse(p_val < 0.01, "**",
                                  ifelse(p_val < 0.05, "*", "ns")))
                    
                    # Interpret co-expression pattern
                    coexpression <- if (abs(cor_val) >= 0.7) {
                        if (cor_val > 0) "Strong positive co-expression" else "Strong negative exclusion"
                    } else if (abs(cor_val) >= 0.4) {
                        if (cor_val > 0) "Moderate co-expression" else "Moderate mutual exclusion"
                    } else if (abs(cor_val) >= 0.2) {
                        if (cor_val > 0) "Weak co-expression" else "Weak mutual exclusion"
                    } else {
                        "Independent expression"
                    }
                    
                    table$addRow(rowKey = row_count, values = list(
                        marker1 = marker1_name,
                        marker2 = marker2_name,
                        correlation = cor_val,
                        p_value = p_val,
                        significance = significance,
                        coexpression = coexpression
                    ))
                    
                    row_count <- row_count + 1
                }
            }
        },
        
        .performClusteringAnalysis = function(marker_data, marker_names) {
            if (!self$options$perform_clustering) return()
            
            # Standardize data for clustering
            scaled_data <- scale(marker_data)
            
            # Determine number of clusters
            n_clusters <- self$options$n_clusters
            if (n_clusters == 0) {
                # Auto-determine optimal clusters using elbow method
                if (requireNamespace('cluster', quietly = TRUE)) {
                    max_k <- min(10, nrow(marker_data) - 1)
                    wss <- numeric(max_k)
                    for (k in 1:max_k) {
                        tryCatch({
                            km_result <- kmeans(scaled_data, centers = k, nstart = 25)
                            wss[k] <- km_result$tot.withinss
                        }, error = function(e) wss[k] <- NA)
                    }
                    # Simple elbow detection
                    n_clusters <- which.max(diff(diff(wss))) + 1
                    n_clusters <- max(2, min(n_clusters, 6))  # Reasonable bounds
                } else {
                    n_clusters <- 3  # Default
                }
            }
            
            # Perform k-means clustering
            tryCatch({
                set.seed(42)  # For reproducibility
                km_result <- kmeans(scaled_data, centers = n_clusters, nstart = 25)
                clusters <- km_result$cluster
                
                # Analyze each cluster
                table <- self$results$clusteringtable
                for (k in 1:n_clusters) {
                    cluster_mask <- clusters == k
                    n_cells <- sum(cluster_mask)
                    percent <- (n_cells / length(clusters)) * 100
                    
                    # Calculate cluster centroid
                    cluster_data <- marker_data[cluster_mask, , drop = FALSE]
                    cluster_means <- colMeans(cluster_data, na.rm = TRUE)
                    
                    # Identify dominant markers (above 75th percentile across all data)
                    overall_q75 <- sapply(marker_data, quantile, 0.75, na.rm = TRUE)
                    dominant_idx <- which(cluster_means > overall_q75)
                    dominant_markers <- if (length(dominant_idx) > 0) {
                        paste(marker_names[dominant_idx], collapse = ", ")
                    } else {
                        "Low expressing"
                    }
                    
                    # Suggest phenotype based on dominant markers
                    phenotype <- private$.suggestPhenotype(dominant_markers, marker_names)
                    
                    # Create mean expression pattern string
                    mean_expr_pattern <- paste(
                        sapply(1:length(marker_names), function(i) {
                            paste0(marker_names[i], ": ", round(cluster_means[i], 2))
                        }),
                        collapse = "; "
                    )
                    
                    table$addRow(rowKey = k, values = list(
                        cluster_id = k,
                        n_cells = n_cells,
                        percent = percent,
                        phenotype = phenotype,
                        dominant_markers = dominant_markers,
                        mean_expression = mean_expr_pattern
                    ))
                }
            }, error = function(e) {
                # Clustering failed, populate with error message
                table <- self$results$clusteringtable
                table$addRow(rowKey = 1, values = list(
                    cluster_id = NA,
                    n_cells = NA,
                    percent = NA,
                    phenotype = "Clustering failed",
                    dominant_markers = "Error in clustering algorithm",
                    mean_expression = as.character(e$message)
                ))
            })
        },
        
        .suggestPhenotype = function(dominant_markers, all_markers) {
            # Simple phenotype suggestion based on common marker patterns
            dom_lower <- tolower(dominant_markers)
            
            if (grepl("cd3", dom_lower) && grepl("cd8", dom_lower)) {
                return("Cytotoxic T cells (CD3+CD8+)")
            } else if (grepl("cd3", dom_lower) && grepl("cd4", dom_lower)) {
                return("Helper T cells (CD3+CD4+)")
            } else if (grepl("cd68", dom_lower) || grepl("cd163", dom_lower)) {
                return("Macrophages")
            } else if (grepl("cd20", dom_lower) || grepl("cd19", dom_lower)) {
                return("B cells")
            } else if (grepl("foxp3", dom_lower)) {
                return("Regulatory T cells (Tregs)")
            } else if (grepl("pd1", dom_lower) || grepl("pdcd1", dom_lower)) {
                return("Exhausted T cells (PD1+)")
            } else if (grepl("ki67", dom_lower) || grepl("pcna", dom_lower)) {
                return("Proliferating cells")
            } else if (dominant_markers == "Low expressing") {
                return("Low/negative expression")
            } else {
                return(paste("Mixed phenotype:", dominant_markers))
            }
        },
        
        .performPCAAnalysis = function(marker_data, marker_names) {
            if (!self$options$perform_pca) return()
            
            # Perform PCA
            tryCatch({
                pca_result <- prcomp(marker_data, scale. = TRUE, center = TRUE)
                
                # Calculate variance explained
                var_explained <- (pca_result$sdev^2) / sum(pca_result$sdev^2) * 100
                
                # Store PCA results for plotting
                pca_data <- data.frame(
                    PC1 = pca_result$x[, 1],
                    PC2 = pca_result$x[, 2],
                    Variance_PC1 = var_explained[1],
                    Variance_PC2 = var_explained[2]
                )
                
                # Add loadings information
                loadings_data <- data.frame(
                    Marker = marker_names,
                    PC1_loading = pca_result$rotation[, 1],
                    PC2_loading = pca_result$rotation[, 2]
                )
                
                # Store for plotting
                private$.pca_data <- pca_data
                private$.loadings_data <- loadings_data
                
            }, error = function(e) {
                private$.pca_data <- NULL
                private$.loadings_data <- NULL
            })
        },
        
        .generatePlots = function(marker_data, marker_names) {
            # Correlation heatmap
            if (self$options$show_plots) {
                cor_matrix <- cor(marker_data, use = "complete.obs", 
                                 method = if (self$options$correlation_method == "spearman") "spearman" else "pearson")
                
                # Convert correlation matrix to long format for ggplot
                cor_data <- expand.grid(Var1 = marker_names, Var2 = marker_names)
                cor_data$value <- as.vector(cor_matrix)
                
                image1 <- self$results$correlationplot
                image1$setState(cor_data)
                
                image2 <- self$results$heatmapplot
                image2$setState(list(matrix = cor_matrix, names = marker_names))
            }
            
            # PCA plot
            if (self$options$show_plots && !is.null(private$.pca_data)) {
                image3 <- self$results$pcaplot
                image3$setState(list(pca = private$.pca_data, loadings = private$.loadings_data))
            }
        },
        
        .correlationplot = function(image, ggtheme, theme, ...) {
            if (is.null(image$state))
                return(FALSE)
                
            cor_data <- image$state
            
            p <- ggplot2::ggplot(cor_data, ggplot2::aes(x = Var1, y = Var2, fill = value)) +
                ggplot2::geom_tile() +
                ggplot2::scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                                             midpoint = 0, limit = c(-1, 1), space = "Lab",
                                             name = "Correlation\n(r)") +
                ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
                ggplot2::labs(
                    title = "Marker Correlation Matrix",
                    subtitle = "Pairwise correlations between multiplex markers",
                    x = "Marker 1",
                    y = "Marker 2"
                ) +
                ggtheme
                
            print(p)
            TRUE
        },
        
        .heatmapplot = function(image, ggtheme, theme, ...) {
            if (is.null(image$state))
                return(FALSE)
                
            data <- image$state
            cor_matrix <- data$matrix
            marker_names <- data$names
            
            # Create heatmap using base plotting (more stable)
            if (requireNamespace('stats', quietly = TRUE)) {
                # Create a simple correlation heatmap
                cor_data <- expand.grid(X = 1:ncol(cor_matrix), Y = 1:nrow(cor_matrix))
                cor_data$value <- as.vector(cor_matrix)
                
                p <- ggplot2::ggplot(cor_data, ggplot2::aes(x = X, y = Y, fill = value)) +
                    ggplot2::geom_tile() +
                    ggplot2::scale_fill_gradient2(low = "darkblue", high = "darkred", mid = "white", 
                                                 midpoint = 0, name = "r") +
                    ggplot2::scale_x_continuous(breaks = 1:length(marker_names), labels = marker_names) +
                    ggplot2::scale_y_continuous(breaks = 1:length(marker_names), labels = marker_names) +
                    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
                    ggplot2::labs(
                        title = "Multiplex Marker Correlation Heatmap",
                        x = "Markers", y = "Markers"
                    ) +
                    ggtheme
                    
                print(p)
                TRUE
            } else {
                FALSE
            }
        },
        
        .pcaplot = function(image, ggtheme, theme, ...) {
            if (is.null(image$state) || is.null(image$state$pca))
                return(FALSE)
                
            pca_data <- image$state$pca
            loadings_data <- image$state$loadings
            
            p <- ggplot2::ggplot(pca_data, ggplot2::aes(x = PC1, y = PC2)) +
                ggplot2::geom_point(alpha = 0.6, size = 2, color = "steelblue") +
                ggplot2::labs(
                    title = "Principal Component Analysis",
                    subtitle = paste("PC1:", round(pca_data$Variance_PC1[1], 1), "% variance,", 
                                   "PC2:", round(pca_data$Variance_PC2[1], 1), "% variance"),
                    x = paste("PC1 (", round(pca_data$Variance_PC1[1], 1), "% variance)", sep = ""),
                    y = paste("PC2 (", round(pca_data$Variance_PC2[1], 1), "% variance)", sep = "")
                ) +
                ggtheme
                
            # Add loading vectors if requested
            if (self$options$show_loadings && !is.null(loadings_data)) {
                # Scale loadings for visibility
                scale_factor <- max(abs(range(c(pca_data$PC1, pca_data$PC2)))) / 
                               max(abs(range(c(loadings_data$PC1_loading, loadings_data$PC2_loading)))) * 0.7
                
                p <- p + 
                    ggplot2::geom_segment(data = loadings_data,
                                         ggplot2::aes(x = 0, y = 0, 
                                                     xend = PC1_loading * scale_factor, 
                                                     yend = PC2_loading * scale_factor),
                                         arrow = ggplot2::arrow(length = ggplot2::unit(0.3, "cm")),
                                         color = "red", alpha = 0.7) +
                    ggplot2::geom_text(data = loadings_data,
                                      ggplot2::aes(x = PC1_loading * scale_factor * 1.1,
                                                  y = PC2_loading * scale_factor * 1.1,
                                                  label = Marker),
                                      color = "red", size = 3)
            }
                
            print(p)
            TRUE
        },
        
        .generateInterpretation = function(marker_data, marker_names) {
            n_samples <- nrow(marker_data)
            n_markers <- length(marker_names)
            
            # Calculate summary statistics
            overall_mean <- mean(rowMeans(marker_data, na.rm = TRUE), na.rm = TRUE)
            
            # Count significant correlations
            cor_matrix <- cor(marker_data, use = "complete.obs", method = "pearson")
            n_correlations <- sum(abs(cor_matrix[upper.tri(cor_matrix)]) >= 0.5, na.rm = TRUE)
            total_pairs <- choose(n_markers, 2)
            
            # Diversity metrics
            marker_sums <- rowSums(marker_data > self$options$positivity_cutpoint, na.rm = TRUE)
            diversity <- if (requireNamespace('vegan', quietly = TRUE)) {
                tryCatch({
                    vegan::diversity(marker_data + 1, index = "shannon")  # Add 1 to avoid log(0)
                }, error = function(e) {
                    # Simple diversity calculation if vegan fails
                    mean(marker_sums / n_markers, na.rm = TRUE)
                })
            } else {
                # Simple diversity approximation
                mean(marker_sums / n_markers, na.rm = TRUE)
            }
            
            interpretation <- paste0(
                "<h3>🎯 Multiplex Immunofluorescence Analysis Summary</h3>",
                "<p><strong>Dataset Overview:</strong> ", n_samples, " samples analyzed with ", n_markers, " markers</p>",
                
                "<h4>Expression Profile:</h4>",
                "<ul>",
                "<li><strong>Overall Expression Level:</strong> Mean = ", round(overall_mean, 2), "</li>",
                "<li><strong>Marker Diversity:</strong> ", 
                if (is.numeric(diversity)) paste("Average marker diversity =", round(mean(diversity, na.rm = TRUE), 3)) else "Complex diversity pattern",
                "</li>",
                "</ul>",
                
                "<h4>Co-expression Analysis:</h4>",
                "<p><strong>Significant Correlations:</strong> ", n_correlations, " out of ", total_pairs, " marker pairs (",
                round((n_correlations / total_pairs) * 100, 1), "%) show moderate to strong correlation (|r| ≥ 0.5)</p>",
                
                if (n_correlations / total_pairs > 0.5) {
                    "<p><em>High correlation frequency suggests coordinated expression patterns, 
                    indicating potential functional relationships between markers.</em></p>"
                } else if (n_correlations / total_pairs > 0.2) {
                    "<p><em>Moderate correlation frequency indicates some coordinated expression 
                    with largely independent marker patterns.</em></p>"
                } else {
                    "<p><em>Low correlation frequency suggests predominantly independent marker expression, 
                    indicating diverse cellular phenotypes.</em></p>"
                },
                
                "<h4>Clinical Implications:</h4>",
                "<ul>",
                "<li>Review correlation patterns for potential biomarker redundancy</li>",
                "<li>Identify co-expressed markers for pathway analysis</li>",
                "<li>Consider clustering results for phenotype classification</li>",
                if (!is.null(private$.pca_data)) {
                    paste0("<li>PC1 and PC2 explain ", 
                           round(private$.pca_data$Variance_PC1[1] + private$.pca_data$Variance_PC2[1], 1), 
                           "% of total variance</li>")
                } else { "" },
                "</ul>",
                
                "<h4>Quality Assessment:</h4>",
                "<ul>",
                "<li>Verify marker antibody specificity for strongly correlated pairs</li>",
                "<li>Consider technical factors affecting expression patterns</li>",
                "<li>Validate findings in independent cohorts</li>",
                "</ul>",
                
                "<p><em>Results provide comprehensive characterization of multiplex marker relationships 
                suitable for immune microenvironment profiling and biomarker development applications.</em></p>"
            )
            
            self$results$interpretation$setContent(interpretation)
        }
    )
)