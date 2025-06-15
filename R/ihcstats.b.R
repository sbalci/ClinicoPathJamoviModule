#' @title IHC Expression Analysis
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import ggplot2

ihcstatsClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "ihcstatsClass",
    inherit = ihcstatsBase,
    private = list(
        # Private data storage for visualizations and analysis
        .ihc_matrix = NULL,  # Stores numeric converted IHC values
        .clusters = NULL,    # Stores cluster assignments
        .hc = NULL,          # Stores hierarchical clustering results
        .pam = NULL,         # Stores PAM clustering results if used
        .silhouette = NULL,  # Stores silhouette analysis results
        .cluster_profiles = NULL,  # Stores detailed cluster profiles
        
        # Initialization function - called when the analysis is created
        .init = function() {
            # Display welcome message when no data or markers selected
            if (is.null(self$data) || length(self$options$markers) == 0) {
                todo <- "
                    <br>Welcome to IHC Expression Analysis
                    <br><br>
                    This tool analyzes immunohistochemical (IHC) expression patterns and identifies clusters of similar cases.
                    <br><br>
                    To begin:
                    <ul>
                        <li>Select categorical IHC marker variables</li>
                        <li>Optionally provide sample IDs and/or grouping variables</li>
                        <li>Adjust clustering and visualization options as needed</li>
                    </ul>
                    <br>
                    The analysis will:
                    <ul>
                        <li>Calculate H-scores from categorical markers</li>
                        <li>Perform cluster analysis with distance metrics optimized for IHC data</li>
                        <li>Visualize expression patterns with dendrograms and heatmaps</li>
                        <li>Identify characteristic patterns for each cluster</li>
                    </ul>
                    "
                html <- self$results$todo
                html$setContent(todo)
            }
        },

        # Main analysis function - called when the analysis is run
        .run = function() {
            # Check if markers have been selected
            if (is.null(self$options$markers))
                return()

            # Check if data has rows
            if (nrow(self$data) == 0)
                stop('Data contains no (complete) rows')

            # Get the data for selected markers
            markers <- self$options$markers
            data <- self$data[markers]
            
            # Compute H-scores if requested
            if (self$options$computeHScore) {
                private$.computeHScores(data)
            }

            # Perform clustering based on selected method
            if (self$options$clusterMethod == "hierarchical") {
                private$.performHierarchicalClustering(data)
            } else if (self$options$clusterMethod == "pam") {
                private$.performPAMClustering(data)
            }
        },

        # Calculate custom Jaccard distance for IHC data
        .calculateJaccardDistance = function(ihc_matrix) {
            n <- nrow(ihc_matrix)
            dist_matrix <- matrix(0, n, n)
            
            # Calculate distance between each pair of cases
            for (i in 1:(n-1)) {
                for (j in (i+1):n) {
                    # Calculate weighted Jaccard similarity
                    shared_weights <- 0
                    total_weights <- 0
                    
                    for (k in 1:ncol(ihc_matrix)) {
                        val_i <- ihc_matrix[i, k]
                        val_j <- ihc_matrix[j, k]
                        
                        # Skip if both are NA
                        if (is.na(val_i) && is.na(val_j)) next
                        
                        # Handle missing values
                        if (is.na(val_i) || is.na(val_j)) {
                            total_weights <- total_weights + 1
                            next
                        }
                        
                        # Calculate similarity based on IHC ordinal values
                        # For standard IHC (0-3 scale), closer values are more similar
                        if (val_i == val_j) {
                            # Exact match gets full weight (1.0)
                            shared_weights <- shared_weights + 1
                        } else {
                            # Partial similarity based on distance between ordinal values
                            max_distance <- 3  # Maximum possible distance in standard IHC
                            actual_distance <- abs(val_i - val_j)
                            
                            # Linear scaling of similarity based on distance
                            partial_similarity <- 1 - (actual_distance / max_distance)
                            shared_weights <- shared_weights + partial_similarity
                        }
                        
                        total_weights <- total_weights + 1
                    }
                    
                    # Calculate Jaccard distance
                    if (total_weights > 0) {
                        jaccard_similarity <- shared_weights / total_weights
                        dist_matrix[i, j] <- dist_matrix[j, i] <- 1 - jaccard_similarity
                    } else {
                        dist_matrix[i, j] <- dist_matrix[j, i] <- 1  # Maximum distance if no shared data
                    }
                }
            }
            
            # Convert to dist object
            return(as.dist(dist_matrix))
        },
        
        # Create text summary of expression patterns in a cluster
        .summarizeClusterPattern = function(data, cluster_members) {
            # Prepare results
            pattern_elements <- character()
            
            # For each marker
            for (i in 1:ncol(data)) {
                col_name <- colnames(data)[i]
                col_data <- data[cluster_members, i]
                
                # Skip if all NA
                if (all(is.na(col_data))) next
                
                # Calculate mode for categorical data
                if (is.factor(col_data)) {
                    # Get frequencies
                    freq_table <- table(col_data)
                    mode_val <- names(freq_table)[which.max(freq_table)]
                    freq <- max(freq_table) / sum(freq_table) * 100
                    
                    # Add to pattern
                    pattern_elements <- c(pattern_elements, 
                                          sprintf("%s: %s (%.0f%%)", col_name, mode_val, freq))
                } else if (is.numeric(col_data)) {
                    # For H-score or other numeric data
                    mean_val <- mean(col_data, na.rm = TRUE)
                    pattern_elements <- c(pattern_elements,
                                         sprintf("%s: %.1f", col_name, mean_val))
                }
            }
            
            # Combine into single string
            return(paste(pattern_elements, collapse = "; "))
        },
        
        # Convert IHC data to numeric matrix
        .prepareIHCMatrix = function(data) {
            # Create empty matrix with proper dimensions
            ihc_matrix <- matrix(0, nrow = nrow(data), ncol = ncol(data))
            colnames(ihc_matrix) <- colnames(data)
            rownames(ihc_matrix) <- rownames(data)
            
            # Create row names if missing
            if (is.null(rownames(ihc_matrix))) {
                # Use ID variable if provided
                if (!is.null(self$options$id) && self$options$id != "" && 
                    !is.null(self$data[[self$options$id]])) {
                    rownames(ihc_matrix) <- as.character(self$data[[self$options$id]])
                } else {
                    # Otherwise use Case_N
                    rownames(ihc_matrix) <- paste0("Case_", 1:nrow(ihc_matrix))
                }
            }
            
            # Convert each column to numeric
            for (i in 1:ncol(data)) {
                if (is.factor(data[,i])) {
                    # Get levels count to determine scoring system
                    level_count <- length(levels(data[,i]))
                    
                    # For categorical IHC data, map to 0-(n-1) scale
                    if (level_count <= 5) {  # Standard IHC scoring has 2-4 levels typically
                        # Convert factor to numeric, preserving order
                        # This maps the first level to 1, second to 2, etc.
                        # Then subtract 1 to get 0-based indexing
                        ihc_matrix[,i] <- as.numeric(data[,i]) - 1
                    } else {
                        # For other categorical variables with many levels
                        # Just use numeric conversion without offset
                        ihc_matrix[,i] <- as.numeric(data[,i])
                    }
                } else if (is.numeric(data[,i])) {
                    # For already numeric data (like H-scores)
                    ihc_matrix[,i] <- data[,i]
                } else {
                    # For unsupported types, set to NA
                    ihc_matrix[,i] <- NA
                }
            }
            
            return(ihc_matrix)
        },
        
        # Perform hierarchical clustering
        .performHierarchicalClustering = function(data) {
            # Check required libraries
            if (!requireNamespace("pheatmap", quietly = TRUE) || 
                !requireNamespace("dendextend", quietly = TRUE) ||
                !requireNamespace("RColorBrewer", quietly = TRUE) ||
                !requireNamespace("cluster", quietly = TRUE)) {
                jmvcore::reject("This analysis requires the 'pheatmap', 'dendextend', 'RColorBrewer', and 'cluster' packages")
                return()
            }
            
            # Convert categorical IHC data to numeric matrix for analysis
            ihc_matrix <- private$.prepareIHCMatrix(data)
            
            # Calculate distance matrix based on selected metric
            if (self$options$distanceMetric == "jaccard") {
                # Custom Jaccard distance optimized for IHC categorical data
                dist_matrix <- private$.calculateJaccardDistance(ihc_matrix)
            } else {
                # Gower distance (handles mixed data types)
                dist_matrix <- cluster::daisy(ihc_matrix, metric = "gower")
            }
            
            # Perform hierarchical clustering
            hc <- hclust(dist_matrix, method = self$options$linkageMethod)
            
            # Cut tree to get cluster assignments
            clusters <- cutree(hc, k = self$options$nClusters)
            
            # Store results for visualizations
            private$.clusters <- clusters
            private$.hc <- hc
            private$.ihc_matrix <- ihc_matrix
            
            # Generate cluster summaries for results table
            private$.generateClusterSummaries(data, clusters)
            
            # Perform silhouette analysis if requested
            if (self$options$silhouetteAnalysis) {
                private$.performSilhouetteAnalysis(dist_matrix, clusters)
            }
        },
        
        # Perform PAM (Partitioning Around Medoids) clustering
        .performPAMClustering = function(data) {
            # Check required library
            if (!requireNamespace("cluster", quietly = TRUE)) {
                jmvcore::reject("This analysis requires the 'cluster' package")
                return()
            }
            
            # Convert categorical IHC data to numeric matrix
            ihc_matrix <- private$.prepareIHCMatrix(data)
            
            # Calculate distance matrix based on selected metric
            if (self$options$distanceMetric == "jaccard") {
                dist_matrix <- private$.calculateJaccardDistance(ihc_matrix)
            } else {
                dist_matrix <- cluster::daisy(ihc_matrix, metric = "gower")
            }
            
            # Perform PAM clustering
            pam_result <- cluster::pam(dist_matrix, k = self$options$nClusters, diss = TRUE)
            
            # Store results for visualizations
            private$.clusters <- pam_result$clustering
            private$.pam <- pam_result
            private$.ihc_matrix <- ihc_matrix
            
            # Generate cluster summaries
            private$.generateClusterSummaries(data, pam_result$clustering)
            
            # Perform silhouette analysis if requested
            if (self$options$silhouetteAnalysis) {
                private$.performSilhouetteAnalysis(dist_matrix, pam_result$clustering)
            }
        },
        
        # Generate summaries for each cluster
        .generateClusterSummaries = function(data, clusters) {
            # Clear existing results
            self$results$clusterSummary$clear()
            
            # For each cluster
            for (i in 1:max(clusters)) {
                # Get cluster members
                cluster_members <- which(clusters == i)
                cluster_size <- length(cluster_members)
                
                # Skip empty clusters
                if (cluster_size == 0) next
                
                # Calculate expression pattern for this cluster
                pattern_text <- private$.summarizeClusterPattern(data, cluster_members)
                
                # Add to results table
                self$results$clusterSummary$addRow(rowKey = i, values = list(
                    cluster = i,
                    size = cluster_size,
                    pattern = pattern_text
                ))
            }
        },
        
        # Perform silhouette analysis to assess clustering quality
        .performSilhouetteAnalysis = function(dist_matrix, clusters) {
            # Clear existing results
            self$results$silhouetteTable$clear()
            
            # Calculate silhouette values
            sil <- cluster::silhouette(clusters, dist_matrix)
            
            # Get average silhouette width per cluster
            avg_sil_by_cluster <- aggregate(sil[, "sil_width"], 
                                           by = list(Cluster = sil[, "cluster"]), 
                                           FUN = mean)
            
            # Get overall average silhouette width
            avg_sil <- summary(sil)$avg.width
            
            # Add overall silhouette info
            self$results$silhouetteTable$addRow(rowKey = "overall", values = list(
                method = self$options$clusterMethod,
                clusters = self$options$nClusters,
                avg_silhouette = round(avg_sil, 3)
            ))
            
            # Store for later use
            private$.silhouette <- sil
        },
        
        # Calculate H-scores from categorical IHC markers
        .computeHScores = function(data) {
            # Clear existing results
            self$results$hscoreTable$clear()
            
            # Process each marker
            for (marker in names(data)) {
                # Skip if not a factor
                if (!is.factor(data[[marker]])) next
                
                # Get levels and their counts
                levels <- levels(data[[marker]])
                level_count <- length(levels)
                
                # Only process if we have 2-4 levels (standard IHC scoring)
                if (level_count >= 2 && level_count <= 4) {
                    # Get distribution for reporting
                    dist <- table(data[[marker]])
                    dist_text <- paste(names(dist), dist, sep = ": ", collapse = ", ")
                    
                    # Calculate proportions for each intensity level
                    props <- prop.table(table(data[[marker]]))
                    
                    # Map intensity values to 0-(n-1) scale
                    intensity_values <- seq(0, level_count - 1)
                    names(intensity_values) <- levels
                    
                    # Calculate H-score:
                    # H-score = (% of 1+ cells × 1) + (% of 2+ cells × 2) + (% of 3+ cells × 3)
                    # with percentages expressed as whole numbers (0-100)
                    h_score <- sum(props * intensity_values[names(props)] * 100, na.rm = TRUE)
                    
                    # Add to results table
                    self$results$hscoreTable$addRow(rowKey = marker, values = list(
                        marker = marker,
                        hscore = round(h_score, 1),
                        dist = dist_text
                    ))
                }
            }
        },
        
        # Create dendrogram visualization
        .visualizeDendrogram = function(image, ggtheme, theme, ...) {
            # Skip if not requested or no data
            if (!self$options$showDendrogram || is.null(private$.hc))
                return()
            
            # Get hierarchical clustering result
            hc <- private$.hc
            clusters <- private$.clusters
            
            # Convert to dendrogram
            dend <- as.dendrogram(hc)
            
            # Color branches by cluster
            dend <- dendextend::color_branches(dend, k = self$options$nClusters)
            
            # Color labels by cluster if showing labels
            if (self$options$showSampleLabels) {
                dend <- dendextend::color_labels(dend, k = self$options$nClusters)
            } else {
                # Hide labels when not showing them
                dend <- dendextend::set(dend, "labels", rep("", length(private$.clusters)))
            }
            
            # Plot dendrogram
            plot(dend, main = "IHC Expression Pattern Clustering",
                 ylab = "Distance", xlab = "Cases",
                 horiz = FALSE)
            
            # Add cluster rectangles if requested
            if (self$options$showClusterBoxes) {
                # Get colors for clusters - using RColorBrewer for consistent colors
                cluster_cols <- RColorBrewer::brewer.pal(min(self$options$nClusters, 8), "Set1")
                if (self$options$nClusters > 8) {
                    # Extend colors if needed
                    cluster_cols <- colorRampPalette(cluster_cols)(self$options$nClusters)
                }
                
                # Add rectangles
                dendextend::rect.dendrogram(dend, k = self$options$nClusters, 
                                           border = cluster_cols)
            }
            
            return(TRUE)
        },
        
        # Create heatmap visualization
        .visualizeClusterHeatmap = function(image, ggtheme, theme, ...) {
            # Skip if not requested or no data
            if (!self$options$showHeatmap || is.null(private$.clusters) || is.null(private$.ihc_matrix))
                return()
            
            # Get stored data
            ihc_matrix <- private$.ihc_matrix
            clusters <- private$.clusters
            
            # Try to get grouping information if provided
            has_groups <- FALSE
            if (!is.null(self$options$group) && self$options$group != "" && 
                !is.null(self$data[[self$options$group]])) {
                groups <- self$data[[self$options$group]]
                has_groups <- TRUE
            }
            
            # Create annotation data frame
            if (has_groups) {
                annotation_row <- data.frame(
                    Cluster = factor(clusters),
                    Group = factor(groups)
                )
            } else {
                annotation_row <- data.frame(
                    Cluster = factor(clusters)
                )
            }
            rownames(annotation_row) <- rownames(ihc_matrix)
            
            # Create color palettes
            # For IHC markers - yellow to red gradient similar to leiomyosarcoma paper
            color_palette <- colorRampPalette(c("#F7F7F7", "#FFC000", "#FF0000"))(50)
            
            # For clusters - Set1 has good distinguishable colors
            n_clusters <- length(unique(clusters))
            cluster_colors <- RColorBrewer::brewer.pal(min(n_clusters, 8), "Set1")
            if (n_clusters > 8) {
                cluster_colors <- colorRampPalette(cluster_colors)(n_clusters)
            }
            names(cluster_colors) <- levels(factor(clusters))
            
            # For groups if available
            annotation_colors <- list(
                Cluster = setNames(cluster_colors, levels(factor(clusters)))
            )
            
            if (has_groups) {
                n_groups <- length(unique(groups))
                group_colors <- RColorBrewer::brewer.pal(min(n_groups, 8), "Set2")
                if (n_groups > 8) {
                    group_colors <- colorRampPalette(group_colors)(n_groups)
                }
                names(group_colors) <- levels(factor(groups))
                annotation_colors$Group <- group_colors
            }
            
            # Create marker type annotations if requested
            annotation_col <- NULL
            if (self$options$annotateMarkers) {
                # Determine marker types based on levels
                marker_types <- sapply(self$data[self$options$markers], function(x) {
                    if (is.factor(x)) {
                        paste0(length(levels(x)), "-level")
                    } else if (is.numeric(x)) {
                        "H-score"
                    } else {
                        "Other"
                    }
                })
                
                annotation_col <- data.frame(
                    MarkerType = factor(marker_types)
                )
                rownames(annotation_col) <- colnames(ihc_matrix)
                
                # Add marker type colors
                marker_type_colors <- RColorBrewer::brewer.pal(
                    min(length(unique(marker_types)), 8), "Pastel1")
                names(marker_type_colors) <- unique(marker_types)
                annotation_colors$MarkerType <- marker_type_colors
            }
            
            # Generate heatmap with settings similar to the leiomyosarcoma paper
            heatmap_plot <- pheatmap::pheatmap(
                mat = ihc_matrix,
                color = color_palette,
                cluster_rows = TRUE,
                cluster_cols = TRUE,
                clustering_distance_rows = "euclidean",
                clustering_distance_cols = "euclidean",
                clustering_method = self$options$linkageMethod,
                annotation_row = annotation_row,
                annotation_col = annotation_col,
                annotation_colors = annotation_colors,
                fontsize = 10,
                fontsize_row = 8,
                show_rownames = self$options$showSampleLabels,
                main = "IHC Expression Pattern Clusters",
                silent = TRUE
            )
            
            # Print the plot
            print(heatmap_plot)
            return(TRUE)
        }
    )
)