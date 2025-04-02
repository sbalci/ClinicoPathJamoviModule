#  @title IHC Expression Analysis
#  @importFrom R6 R6Class
#  @import jmvcore
#  @import ggplot2

ihcstatsClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "ihcstatsClass",
    inherit = ihcstatsBase,
    private = list(

        .clusters = NULL,
        .hc = NULL,



        .init = function() {
            # Initialize any required packages
            if (is.null(self$data) || length(self$options$markers) == 0) {
                todo <- "
                    <br>Welcome to IHC Expression Analysis
                    <br><br>
                    To begin:
                    <ul>
                        <li>Select categorical IHC marker variables</li>
                        <li>Choose analysis options</li>
                        <li>Select visualization preferences</li>
                    </ul>
                    "
                html <- self$results$todo
                html$setContent(todo)
            }
        },

        .run = function() {
            if (is.null(self$options$markers))
                return()

            if (nrow(self$data) == 0)
                stop('Data contains no (complete) rows')

            # Get the data
            markers <- self$options$markers
            data <- self$data[markers]

            # Compute H-scores if requested
            if (self$options$computeHScore)
                private$.computeHScores(data)

            # Perform clustering
            private$.performClustering(data)

            # Create visualizations
            if (self$options$showDendrogram ||
                self$options$showHeatmap ||
                self$options$showScoreDist) {
                private$.createVisualizations(data)
            }
        },


        .computeHScores = function(data) {
            for (marker in names(data)) {
                # Get raw factor levels
                levels <- levels(data[[marker]])

                # Get counts for distribution
                dist <- table(data[[marker]])
                dist_text <- paste(names(dist), dist, sep=": ", collapse=", ")

                # Convert scoring to numeric based on IHC conventions
                scores_map <- switch(length(levels),
                                     "2" = c(0, 1),  # Binary scoring (-, +)
                                     "3" = c(0, 1, 2),  # 3-level scoring (-, 1+, 2+)
                                     "4" = c(0, 1, 2, 3),  # 4-level scoring (-, 1+, 2+, 3+)
                                     NULL
                )

                if (!is.null(scores_map)) {
                    # Calculate proportions
                    props <- prop.table(table(data[[marker]]))

                    # Calculate H-score (weighted sum of scores)
                    h_score <- sum(scores_map * (props * 100))

                    # Add to results table
                    self$results$hscoreTable$addRow(rowKey=marker, values=list(
                        marker = marker,
                        hscore = round(h_score, 1),
                        dist = dist_text
                    ))
                } else {
                    # Handle unexpected number of levels
                    self$results$hscoreTable$addRow(rowKey=marker, values=list(
                        marker = marker,
                        hscore = NA,
                        dist = "Invalid scoring levels"
                    ))
                }
            }
        },

        .calculateDistances = function(data, method = "mixed") {
            # Helper function to determine variable type
            .getVarType <- function(x) {
                if (is.factor(x)) {
                    if (is.ordered(x)) return("ordinal")
                    return("nominal")
                }
                if (is.numeric(x)) return("numeric")
                return("other")
            }

            # Get variable types
            var_types <- sapply(data, .getVarType)

            # Calculate distances based on data type
            if (method == "mixed") {
                # Gower distance for mixed data types
                # Automatically handles different variable types appropriately
                dist_matrix <- cluster::daisy(data, metric = "gower",
                                              type = list(asymm = which(var_types == "nominal"),
                                                          symm = which(var_types == "ordinal"),
                                                          logratio = which(var_types == "numeric")))

            } else if (method == "categorical") {
                # For purely categorical data
                n <- nrow(data)
                dist_matrix <- matrix(0, n, n)

                for (i in 1:(n-1)) {
                    for (j in (i+1):n) {
                        # Different weights for ordinal vs nominal variables
                        matches <- mapply(function(x, y, type) {
                            if (type == "ordinal") {
                                # For ordinal, consider distance between levels
                                abs(as.numeric(x) - as.numeric(y)) / (length(levels(x)) - 1)
                            } else {
                                # For nominal, simple match/mismatch
                                as.numeric(x != y)
                            }
                        }, data[i,], data[j,], var_types)

                        dist_matrix[i,j] <- dist_matrix[j,i] <- mean(matches)
                    }
                }
                dist_matrix <- as.dist(dist_matrix)

            } else if (method == "hscore") {
                # For H-score data (0-300 scale)
                dist_matrix <- dist(data, method = "euclidean")
                # Normalize to 0-1 range
                dist_matrix <- dist_matrix / 300
            }

            return(dist_matrix)
        },

        .calculateIHCDistance = function(data) {
            n <- nrow(data)
            dist_matrix <- matrix(0, n, n)

            for (i in 1:(n-1)) {
                for (j in (i+1):n) {
                    # Calculate weighted disagreement for each marker
                    marker_diffs <- sapply(data[i,], function(x, y) {
                        if (is.na(x) || is.na(y)) return(NA)

                        # Convert factor levels to numeric scores
                        x_score <- as.numeric(x)
                        y_score <- as.numeric(y)

                        # Custom weighting based on IHC interpretation
                        weight <- case_when(
                            abs(x_score - y_score) == 1 ~ 0.5,  # Adjacent categories
                            abs(x_score - y_score) == 2 ~ 0.8,  # Two steps apart
                            abs(x_score - y_score) == 3 ~ 1.0,  # Maximum difference
                            TRUE ~ 0  # Same category
                        )

                        # Special weighting for clinically significant differences
                        # e.g., negative vs positive threshold
                        if ((x_score == 1 && y_score > 1) || (y_score == 1 && x_score > 1)) {
                            weight <- weight * 1.2  # Increase weight for clinical threshold
                        }

                        return(weight)
                    }, data[j,])

                    # Average weighted differences across markers
                    dist_matrix[i,j] <- dist_matrix[j,i] <- mean(marker_diffs, na.rm = TRUE)
                }
            }

            return(as.dist(dist_matrix))
        },





        .performClustering = function(data) {
            # First determine if we have H-scores or categorical data
            has_hscores <- any(grepl("hscore", names(data), ignore.case = TRUE))

            # Calculate appropriate distance matrix
            if (has_hscores) {
                dist_matrix <- private$.calculateDistances(data, method = "hscore")
            } else if (all(sapply(data, is.factor))) {
                dist_matrix <- private$.calculateDistances(data, method = "categorical")
            } else {
                dist_matrix <- private$.calculateDistances(data, method = "mixed")
            }

            # Perform clustering
            method <- self$options$clusterMethod
            n_clusters <- self$options$nClusters

            if (method == "hierarchical") {
                hc <- hclust(dist_matrix, method = "complete")
                clusters <- cutree(hc, k = n_clusters)
                private$.clusters <- clusters
                private$.hc <- hc

                # Calculate silhouette if requested
                if (self$options$silhouetteAnalysis) {
                    sil <- cluster::silhouette(clusters, dist_matrix)
                    private$.silhouette <- sil
                }

            } else if (method == "pam") {
                pam_result <- cluster::pam(dist_matrix, k = n_clusters, diss = TRUE)
                clusters <- pam_result$clustering
                private$.clusters <- clusters
                private$.pam <- pam_result
            }

            # Generate cluster summary
            cluster_summary <- private$.summarizeClusters(data, clusters)

            # Store results
            private$.cluster_summary <- cluster_summary
        }

#
#         .performClustering = function(data) {
#             # Calculate IHC-specific distances
#             dist_matrix <- private$.calculateIHCDistance(data)
#
#             # Perform hierarchical clustering
#             if (self$options$clusterMethod == "hierarchical") {
#                 # Use Ward's method as it tends to work well with IHC patterns
#                 hc <- hclust(dist_matrix, method = "ward.D2")
#                 clusters <- cutree(hc, k = self$options$nClusters)
#
#                 # Store for plotting
#                 private$.clusters <- clusters
#                 private$.hc <- hc
#
#                 # Calculate cluster stability
#                 if (self$options$stabilityAnalysis) {
#                     boot_results <- pvclust::pvclust(data,
#                                                      method.hclust = "ward.D2",
#                                                      method.dist = function(x) private$.calculateIHCDistance(x),
#                                                      nboot = 100)
#                     private$.stability <- boot_results
#                 }
#             } else if (self$options$clusterMethod == "pam") {
#                 # PAM clustering with custom distance
#                 pam_result <- cluster::pam(dist_matrix,
#                                            k = self$options$nClusters,
#                                            diss = TRUE)
#                 private$.clusters <- pam_result$clustering
#             }
#
#             # Generate interpretable cluster profiles
#             cluster_profiles <- lapply(1:self$options$nClusters, function(k) {
#                 cluster_data <- data[private$.clusters == k, , drop = FALSE]
#                 # Get modal pattern for each marker
#                 sapply(cluster_data, function(x) {
#                     tab <- table(x)
#                     names(which.max(tab))
#                 })
#             })
#
#             private$.cluster_profiles <- cluster_profiles
#         }
#
#
#
#
#
#         .performClustering = function(data) {
#             # Check if we have data
#             if (ncol(data) < 2)
#                 return()
#
#             # Convert categorical data to distance matrix
#             dist_method <- self$options$distanceMetric
#
#             # Create distance matrix based on selected method
#             dist_matrix <- switch(dist_method,
#                                   "gower" = {
#                                       # Gower distance handles mixed data types
#                                       cluster::daisy(data, metric = "gower", weights = rep(1, ncol(data)))
#                                   },
#                                   "jaccard" = {
#                                       # Custom Jaccard distance for categorical data
#                                       n <- nrow(data)
#                                       d <- matrix(0, n, n)
#                                       for(i in 1:(n-1)) {
#                                           for(j in (i+1):n) {
#                                               shared <- sum(data[i,] == data[j,])
#                                               total <- ncol(data)
#                                               d[i,j] <- d[j,i] <- 1 - (shared/total)
#                                           }
#                                       }
#                                       as.dist(d)
#                                   }
#             )
#
#             # Perform clustering based on method
#             clusterMethod <- self$options$clusterMethod
#             n_clusters <- self$options$nClusters
#
#             if (clusterMethod == "hierarchical") {
#                 # Perform hierarchical clustering
#                 hc <- hclust(dist_matrix, method = "complete")
#
#                 # Cut tree to get cluster assignments
#                 clusters <- cutree(hc, k = n_clusters)
#
#                 # Save for plotting
#                 private$.clusters <- clusters
#                 private$.hc <- hc
#
#                 # Calculate cophenetic correlation
#                 coph_corr <- cor(dist_matrix, cophenetic(hc))
#
#                 # Calculate cluster stability using bootstrap
#                 if (requireNamespace("fpc", quietly = TRUE)) {
#                     stab <- fpc::clusterboot(dist_matrix,
#                                              B = 100,
#                                              distances = TRUE,
#                                              bootmethod = "boot",
#                                              clustermethod = fpc::hclustCBI,
#                                              k = n_clusters,
#                                              seed = 123)
#                     cluster_stab <- stab$bootmean
#                 } else {
#                     cluster_stab <- NA
#                 }
#
#             } else if (clusterMethod == "pam") {
#                 # Partitioning Around Medoids
#                 pam_result <- cluster::pam(dist_matrix, k = n_clusters, diss = TRUE)
#                 clusters <- pam_result$clustering
#                 private$.clusters <- clusters
#
#                 # Get silhouette information
#                 sil <- cluster::silhouette(clusters, dist_matrix)
#                 avg_sil <- summary(sil)$avg.width
#             }
#
#             # Generate detailed cluster summaries
#             cluster_summaries <- list()
#             for (i in 1:n_clusters) {
#                 cluster_data <- data[clusters == i, ]
#
#                 # Calculate modal pattern for cluster
#                 pattern <- character()
#                 distinctiveness <- numeric()
#
#                 for (col in names(cluster_data)) {
#                     # Get mode for this marker in cluster
#                     mode_val <- names(sort(table(cluster_data[[col]]), decreasing = TRUE))[1]
#
#                     # Calculate proportion of mode in cluster vs overall
#                     cluster_prop <- mean(cluster_data[[col]] == mode_val)
#                     overall_prop <- mean(data[[col]] == mode_val)
#
#                     # Measure distinctiveness
#                     distinct_score <- cluster_prop / overall_prop
#
#                     pattern <- c(pattern, paste0(col, ": ", mode_val))
#                     distinctiveness <- c(distinctiveness, distinct_score)
#                 }
#
#                 # Format pattern highlighting distinctive features
#                 distinctive_features <- which(distinctiveness > 1.5)
#                 pattern_text <- paste(pattern, collapse = "; ")
#                 if (length(distinctive_features) > 0) {
#                     pattern_text <- paste0(pattern_text,
#                                            " (Distinctive: ",
#                                            paste(names(data)[distinctive_features],
#                                                  collapse = ", "),
#                                            ")")
#                 }
#
#                 # Add cluster summary
#                 self$results$clusterSummary$addRow(rowKey = i, values = list(
#                     cluster = i,
#                     size = sum(clusters == i),
#                     pattern = pattern_text
#                 ))
#
#                 # Store full summary
#                 cluster_summaries[[i]] <- list(
#                     size = sum(clusters == i),
#                     pattern = pattern,
#                     distinctiveness = distinctiveness,
#                     stability = if (exists("cluster_stab")) cluster_stab[i] else NA,
#                     silhouette = if (exists("avg_sil")) avg_sil else NA
#                 )
#             }
#
#             # Store summaries for plotting
#             private$.cluster_summaries <- cluster_summaries
#         }

        ,
        .summarizePattern = function(cluster_data) {
            # Generate readable pattern description
            pattern <- character()
            for (col in names(cluster_data)) {
                mode_val <- names(sort(table(cluster_data[[col]]), decreasing=TRUE))[1]
                pattern <- c(pattern, paste0(col, ": ", mode_val))
            }
            paste(pattern, collapse="; ")
        },

        .clusterPlot = function(image, ggtheme, theme, ...) {
            if (!self$options$showDendrogram)
                return()

            if (self$options$clusterMethod == "hierarchical") {  # Changed from clusterOptions$method
                dend <- as.dendrogram(private$.hc)
                plot <- ggdendro::ggdendrogram(dend, theme_dendro=FALSE) +
                    ggtheme +
                    labs(title="IHC Expression Pattern Clustering")
                print(plot)
                TRUE
            }
        },


        .heatmapPlot = function(image, ggtheme, theme, ...) {
            if (!self$options$showHeatmap)
                return()

            # Add color handling for H-scores
            if (has_hscores) {
                color_palette <- colorRampPalette(c("#FFFFFF", "#FF0000"))(100)
            } else {
                # Categorical color scheme as before
                color_palette <- colorRampPalette(c("#FEE0D2", "#DE2D26"))(100)
            }


            data <- self$data[self$options$markers]

            # Convert categorical data to numeric matrix while preserving ordinal nature
            heatmap_data <- as.matrix(sapply(data, function(x) {
                # Maintain original ordering of factor levels
                as.numeric(factor(x, ordered = TRUE))
            }))

            # Handle case labels
            case_labels <- rownames(data)
            if (is.null(case_labels)) {
                case_labels <- paste0("Case ", 1:nrow(data))
            }

            # Create annotation dataframe for clusters
            if (!is.null(private$.clusters)) {
                annotation_row <- data.frame(
                    Cluster = factor(private$.clusters)
                )
                rownames(annotation_row) <- case_labels
            } else {
                annotation_row <- NULL
            }

            # Create annotation for marker scoring systems
            marker_levels <- sapply(data, function(x) length(levels(x)))
            annotation_col <- data.frame(
                ScoringSystem = factor(paste0(marker_levels, "-level"))
            )
            rownames(annotation_col) <- colnames(data)

            # Custom color palettes
            cluster_colors <- RColorBrewer::brewer.pal(
                n = max(private$.clusters),
                name = "Set1"
            )
            names(cluster_colors) <- levels(factor(private$.clusters))

            scoring_colors <- RColorBrewer::brewer.pal(
                n = length(unique(marker_levels)),
                name = "Set2"
            )
            names(scoring_colors) <- levels(factor(annotation_col$ScoringSystem))

            # Generate dendrogram and heatmap
            pheatmap::pheatmap(
                mat = heatmap_data,
                annotation_row = annotation_row,
                annotation_col = annotation_col,
                clustering_method = "complete",
                clustering_distance_rows = "euclidean",
                clustering_distance_cols = "euclidean",
                show_rownames = FALSE,
                show_colnames = TRUE,
                main = "IHC Expression Patterns",
                fontsize = 10,
                annotation_colors = list(
                    Cluster = cluster_colors,
                    ScoringSystem = scoring_colors
                ),
                color = colorRampPalette(c("#FEE0D2", "#DE2D26"))(100)
            )

            TRUE
        },



        .scoreDistPlot = function(image, ggtheme, theme, ...) {
            if (!self$options$showScoreDist)
                return()

            data <- self$data[self$options$markers]

            # Reshape data for plotting
            plot_data <- tidyr::gather(data, key="Marker", value="Score")

            # Create distribution plot
            plot <- ggplot(plot_data, aes(x=Score, fill=Marker)) +
                geom_bar(position="dodge") +
                ggtheme +
                labs(title="IHC Score Distribution",
                     x="Expression Level",
                     y="Count")

            print(plot)
            TRUE
        }


#
#
#          #  @title IHC-specific Clustering and Visualization
#          #  @importFrom R6 R6Class
#          #  @import jmvcore
#          #  @import ggplot2
#
#          # Enhanced IHC clustering function with dendogram visualization like in the leiomyosarcoma paper
#          .performCategoricalClustering = function(data) {
#              # Check required libraries
#              if (!requireNamespace("pheatmap", quietly = TRUE) ||
#                  !requireNamespace("dendextend", quietly = TRUE) ||
#                  !requireNamespace("RColorBrewer", quietly = TRUE)) {
#                  jmvcore::reject("This analysis requires the 'pheatmap', 'dendextend', and 'RColorBrewer' packages")
#                  return()
#              }
#
#              # Convert categorical IHC data to numeric matrix
#              ihc_matrix <- matrix(0, nrow = nrow(data), ncol = ncol(data))
#              colnames(ihc_matrix) <- colnames(data)
#              rownames(ihc_matrix) <- rownames(data)
#              if (is.null(rownames(ihc_matrix))) {
#                  rownames(ihc_matrix) <- paste0("Case_", 1:nrow(ihc_matrix))
#              }
#
#              # Convert IHC categorical scores to numeric values, preserving ordinal nature
#              for (i in 1:ncol(data)) {
#                  if (is.factor(data[,i])) {
#                      # Get levels count to determine scoring system
#                      level_count <- length(levels(data[,i]))
#
#                      # Create appropriate scoring map - typically 0 to 3 for IHC
#                      if (level_count <= 4) {
#                          # For categorical IHC data, map to 0-3 scale
#                          # Typically: (-) = 0, (+) = 1, (++) = 2, (+++) = 3
#                          ihc_matrix[,i] <- as.numeric(data[,i]) - 1
#                      } else {
#                          # For other categorical variables, just use as.numeric
#                          ihc_matrix[,i] <- as.numeric(data[,i])
#                      }
#                  } else if (is.numeric(data[,i])) {
#                      # For already numeric data (like H-scores)
#                      ihc_matrix[,i] <- data[,i]
#                  }
#              }
#
#              # Calculate IHC-specific distance matrix
#              if (self$options$distanceMetric == "jaccard") {
#                  # Custom Jaccard distance for IHC categorical data
#                  dist_matrix <- private$.calculateJaccardDistance(ihc_matrix)
#              } else {
#                  # Gower distance as default - handles mixed data
#                  dist_matrix <- cluster::daisy(ihc_matrix, metric = "gower")
#              }
#
#              # Perform hierarchical clustering
#              hc <- hclust(dist_matrix, method = self$options$linkageMethod)
#
#              # Determine clusters
#              clusters <- cutree(hc, k = self$options$nClusters)
#
#              # Store for later use in visualizations
#              private$.clusters <- clusters
#              private$.hc <- hc
#              private$.ihc_matrix <- ihc_matrix
#
#              # Generate cluster summaries
#              for (i in 1:self$options$nClusters) {
#                  # Get cluster members
#                  cluster_members <- which(clusters == i)
#                  cluster_size <- length(cluster_members)
#
#                  # Skip empty clusters
#                  if (cluster_size == 0) next
#
#                  # Calculate expression pattern for this cluster
#                  pattern_text <- private$.summarizeClusterPattern(data, cluster_members)
#
#                  # Add to results table
#                  self$results$clusterSummary$addRow(rowKey = i, values = list(
#                      cluster = i,
#                      size = cluster_size,
#                      pattern = pattern_text
#                  ))
#              }
#
#              # If requested, calculate silhouette width
#              if (self$options$silhouetteAnalysis) {
#                  sil <- cluster::silhouette(clusters, dist_matrix)
#                  avg_sil <- summary(sil)$avg.width
#
#                  # Add silhouette info to results
#                  self$results$silhouetteTable$addRow(rowKey = 1, values = list(
#                      method = self$options$clusterMethod,
#                      clusters = self$options$nClusters,
#                      avg_silhouette = round(avg_sil, 3)
#                  ))
#              }
#          }
#
#          # Function to create Jaccard distance matrix optimized for IHC data
#          .calculateJaccardDistance = function(ihc_matrix) {
#              n <- nrow(ihc_matrix)
#              dist_matrix <- matrix(0, n, n)
#
#              # For each pair of cases
#              for (i in 1:(n-1)) {
#                  for (j in (i+1):n) {
#                      # Calculate weighted Jaccard similarity
#                      shared_weights <- 0
#                      total_weights <- 0
#
#                      for (k in 1:ncol(ihc_matrix)) {
#                          val_i <- ihc_matrix[i, k]
#                          val_j <- ihc_matrix[j, k]
#
#                          # Skip if both are NA
#                          if (is.na(val_i) && is.na(val_j)) next
#
#                          # Handle missing values
#                          if (is.na(val_i) || is.na(val_j)) {
#                              total_weights <- total_weights + 1
#                              next
#                          }
#
#                          # Calculate similarity based on IHC ordinal values
#                          # IHC values are typically 0-3 (negative to strong positive)
#                          if (val_i == val_j) {
#                              # Exact match gets full weight
#                              shared_weights <- shared_weights + 1
#                          } else {
#                              # Partial similarity based on distance between ordinal values
#                              max_distance <- 3  # Maximum possible distance in standard IHC
#                              actual_distance <- abs(val_i - val_j)
#                              partial_similarity <- 1 - (actual_distance / max_distance)
#                              shared_weights <- shared_weights + partial_similarity
#                          }
#
#                          total_weights <- total_weights + 1
#                      }
#
#                      # Calculate Jaccard distance
#                      if (total_weights > 0) {
#                          jaccard_similarity <- shared_weights / total_weights
#                          dist_matrix[i, j] <- dist_matrix[j, i] <- 1 - jaccard_similarity
#                      } else {
#                          dist_matrix[i, j] <- dist_matrix[j, i] <- 1  # Maximum distance if no shared data
#                      }
#                  }
#              }
#
#              # Convert to dist object
#              return(as.dist(dist_matrix))
#          }
#
#          # Function to summarize expression pattern in a cluster
#          .summarizeClusterPattern = function(data, cluster_members) {
#              # Prepare results
#              pattern_elements <- character()
#
#              # For each marker
#              for (i in 1:ncol(data)) {
#                  col_name <- colnames(data)[i]
#                  col_data <- data[cluster_members, i]
#
#                  # Skip if all NA
#                  if (all(is.na(col_data))) next
#
#                  # Calculate mode for categorical data
#                  if (is.factor(col_data)) {
#                      # Get frequencies
#                      freq_table <- table(col_data)
#                      mode_val <- names(freq_table)[which.max(freq_table)]
#                      freq <- max(freq_table) / sum(freq_table) * 100
#
#                      # Add to pattern
#                      pattern_elements <- c(pattern_elements,
#                                            sprintf("%s: %s (%.0f%%)", col_name, mode_val, freq))
#                  } else if (is.numeric(col_data)) {
#                      # For H-score or other numeric data
#                      mean_val <- mean(col_data, na.rm = TRUE)
#                      pattern_elements <- c(pattern_elements,
#                                            sprintf("%s: %.1f", col_name, mean_val))
#                  }
#              }
#
#              # Combine into single string
#              return(paste(pattern_elements, collapse = "; "))
#          }
#
#          # Enhanced heatmap visualization with dendrogram like in the example paper
#          .clusterHeatmapWithDendrogram = function(image, ggtheme, theme, ...) {
#              if (!self$options$showDendrogram && !self$options$showHeatmap)
#                  return()
#
#              # Get data
#              ihc_matrix <- private$.ihc_matrix
#              clusters <- private$.clusters
#              hc <- private$.hc
#
#              # Check if we have data
#              if (is.null(ihc_matrix) || is.null(clusters) || is.null(hc))
#                  return()
#
#              # Create annotation data frame
#              annotation_row <- data.frame(
#                  Cluster = factor(clusters)
#              )
#              rownames(annotation_row) <- rownames(ihc_matrix)
#
#              # Color schemes
#              # For IHC staining intensity (typically 0-3)
#              color_palette <- colorRampPalette(c("#FFFFFF", "#FFF7BC", "#FEC44F", "#D95F0E"))(4)
#
#              # For clusters
#              cluster_colors <- RColorBrewer::brewer.pal(n = min(self$options$nClusters, 8), name = "Set1")
#              names(cluster_colors) <- levels(factor(clusters))
#
#              # Create annotation colors
#              annotation_colors <- list(
#                  Cluster = cluster_colors
#              )
#
#              # Generate heatmap with dendrogram
#              heatmap_plot <- pheatmap::pheatmap(
#                  mat = ihc_matrix,
#                  color = color_palette,
#                  cluster_rows = TRUE,
#                  cluster_cols = TRUE,
#                  clustering_distance_rows = "euclidean",
#                  clustering_distance_cols = "euclidean",
#                  clustering_method = self$options$linkageMethod,
#                  annotation_row = annotation_row,
#                  annotation_colors = annotation_colors,
#                  fontsize = 10,
#                  fontsize_row = 8,
#                  show_rownames = self$options$showSampleLabels,
#                  main = "IHC Expression Patterns",
#                  silent = TRUE  # Return the plot object
#              )
#
#              # Print the plot
#              print(heatmap_plot)
#              return(TRUE)
#          }
#
#          # Calculation of H-scores from categorical IHC data
#          .computeHScores = function(data) {
#              for (marker in names(data)) {
#                  # Skip if not a factor
#                  if (!is.factor(data[[marker]])) next
#
#                  # Get levels and their counts
#                  levels <- levels(data[[marker]])
#                  level_count <- length(levels)
#
#                  # Only process if we have 2-4 levels (standard IHC scoring)
#                  if (level_count >= 2 && level_count <= 4) {
#                      # Get distribution
#                      dist <- table(data[[marker]])
#                      dist_text <- paste(names(dist), dist, sep = ": ", collapse = ", ")
#
#                      # Calculate H-score based on intensity distribution
#                      # H-score = (% of 1+ cells × 1) + (% of 2+ cells × 2) + (% of 3+ cells × 3)
#                      # with percentages expressed as whole numbers (0-100)
#                      props <- prop.table(dist)
#
#                      # Map intensity values to 0-3 scale
#                      intensity_values <- seq(0, level_count - 1)
#                      names(intensity_values) <- levels
#
#                      # Calculate H-score
#                      h_score <- sum(props * intensity_values * 100, na.rm = TRUE)
#
#                      # Add to results table
#                      self$results$hscoreTable$addRow(rowKey = marker, values = list(
#                          marker = marker,
#                          hscore = round(h_score, 1),
#                          dist = dist_text
#                      ))
#                  }
#              }
#          }
#
#          # Enhanced heatmap visualization for IHC data with dendrograms
#          .visualizeClusterHeatmap = function(image, ggtheme, theme, ...) {
#              # If visualization is disabled, return
#              if (!self$options$showHeatmap)
#                  return()
#
#              # Check for required data
#              if (is.null(private$.clusters) || is.null(private$.ihc_matrix))
#                  return()
#
#              # Get stored data
#              ihc_matrix <- private$.ihc_matrix
#              clusters <- private$.clusters
#
#              # Try to get sample IDs
#              if (self$options$id && !is.null(self$data[[self$options$id]])) {
#                  sample_ids <- self$data[[self$options$id]]
#              } else {
#                  sample_ids <- paste0("Case_", 1:nrow(ihc_matrix))
#              }
#              rownames(ihc_matrix) <- sample_ids
#
#              # Try to get group information if provided
#              has_groups <- FALSE
#              if (self$options$group && !is.null(self$data[[self$options$group]])) {
#                  groups <- self$data[[self$options$group]]
#                  has_groups <- TRUE
#              }
#
#              # Create annotation data frame
#              if (has_groups) {
#                  annotation_row <- data.frame(
#                      Cluster = factor(clusters),
#                      Group = factor(groups)
#                  )
#              } else {
#                  annotation_row <- data.frame(
#                      Cluster = factor(clusters)
#                  )
#              }
#              rownames(annotation_row) <- rownames(ihc_matrix)
#
#              # Create color palettes
#              # For IHC markers - based on staining intensity
#              # Yellow to red gradient similar to Figure 3 in the leiomyosarcoma paper
#              color_palette <- colorRampPalette(c("#F7F7F7", "#FFC000", "#FF0000"))(4)
#
#              # For clusters
#              n_clusters <- length(unique(clusters))
#              cluster_colors <- RColorBrewer::brewer.pal(min(n_clusters, 8), "Set1")
#              names(cluster_colors) <- levels(factor(clusters))
#
#              # For groups if available
#              if (has_groups) {
#                  n_groups <- length(unique(groups))
#                  group_colors <- RColorBrewer::brewer.pal(min(n_groups, 8), "Set2")
#                  names(group_colors) <- levels(factor(groups))
#
#                  annotation_colors <- list(
#                      Cluster = setNames(cluster_colors, levels(factor(clusters))),
#                      Group = setNames(group_colors, levels(factor(groups)))
#                  )
#              } else {
#                  annotation_colors <- list(
#                      Cluster = setNames(cluster_colors, levels(factor(clusters)))
#                  )
#              }
#
#              # Create more helpful annotations for marker types
#              if (self$options$annotateMarkers) {
#                  # Determine marker types based on levels
#                  marker_types <- sapply(self$data[self$options$markers], function(x) {
#                      if (is.factor(x)) {
#                          paste0(length(levels(x)), "-level")
#                      } else if (is.numeric(x)) {
#                          "H-score"
#                      } else {
#                          "Other"
#                      }
#                  })
#
#                  annotation_col <- data.frame(
#                      MarkerType = factor(marker_types)
#                  )
#                  rownames(annotation_col) <- colnames(ihc_matrix)
#
#                  # Add marker type colors
#                  marker_type_colors <- RColorBrewer::brewer.pal(length(unique(marker_types)), "Pastel1")
#                  names(marker_type_colors) <- unique(marker_types)
#                  annotation_colors$MarkerType <- marker_type_colors
#              } else {
#                  annotation_col <- NULL
#              }
#
#              # Generate heatmap with settings similar to the leiomyosarcoma paper
#              heatmap_plot <- pheatmap::pheatmap(
#                  mat = ihc_matrix,
#                  color = color_palette,
#                  cluster_rows = TRUE,
#                  cluster_cols = TRUE,
#                  clustering_distance_rows = "euclidean",
#                  clustering_distance_cols = "euclidean",
#                  clustering_method = self$options$linkageMethod,
#                  annotation_row = annotation_row,
#                  annotation_col = annotation_col,
#                  annotation_colors = annotation_colors,
#                  fontsize = 10,
#                  fontsize_row = 8,
#                  show_rownames = self$options$showSampleLabels,
#                  main = "IHC Expression Pattern Clusters",
#                  silent = TRUE
#              )
#
#              # Print the plot
#              print(heatmap_plot)
#              return(TRUE)
#          }
#
#          # Function to create a separate dendrogram visualization
#          .visualizeDendrogram = function(image, ggtheme, theme, ...) {
#              # If visualization is disabled, return
#              if (!self$options$showDendrogram)
#                  return()
#
#              # Check for required data
#              if (is.null(private$.hc))
#                  return()
#
#              # Get hierarchical clustering result
#              hc <- private$.hc
#              clusters <- private$.clusters
#
#              # Convert to dendrogram
#              dend <- as.dendrogram(hc)
#
#              # Color branches by cluster
#              dend <- dendextend::color_branches(dend, k = self$options$nClusters)
#
#              # Color labels by cluster
#              if (self$options$showSampleLabels) {
#                  dend <- dendextend::color_labels(dend, k = self$options$nClusters)
#              }
#
#              # Plot dendrogram
#              plot(dend, main = "IHC Expression Pattern Clustering",
#                   ylab = "Distance", xlab = "Cases")
#
#              # Add cluster rectangles
#              if (self$options$showClusterBoxes) {
#                  dendextend::rect.dendrogram(dend, k = self$options$nClusters,
#                                              border = rainbow(self$options$nClusters))
#              }
#
#              return(TRUE)
#          }
#
#
#
    )
)
