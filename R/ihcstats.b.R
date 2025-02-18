#' @title IHC Expression Analysis
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import ggplot2

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

        .performClustering = function(data) {
            # Convert categorical data to distance matrix
            dist_method <- self$options$distanceMetric
            dist_matrix <- switch(dist_method,
                                  "gower" = cluster::daisy(data, metric="gower"),
                                  "jaccard" = {
                                      # Custom Jaccard implementation for categorical data
                                      n <- nrow(data)
                                      d <- matrix(0, n, n)
                                      for(i in 1:(n-1)) {
                                          for(j in (i+1):n) {
                                              matches <- sum(data[i,] == data[j,])
                                              total <- ncol(data)
                                              d[i,j] <- d[j,i] <- 1 - (matches/total)
                                          }
                                      }
                                      as.dist(d)
                                  }
            )

            # Perform clustering using clusterMethod instead of method
            clusterMethod <- self$options$clusterMethod  # Changed from method to clusterMethod
            n_clusters <- self$options$nClusters

            if (clusterMethod == "hierarchical") {
                hc <- hclust(dist_matrix, method="complete")
                clusters <- cutree(hc, k=n_clusters)

                # Save clustering info for plots
                private$.clusters <- clusters
                private$.hc <- hc

            } else if (clusterMethod == "pam") {
                pam_result <- cluster::pam(dist_matrix, k=n_clusters)
                clusters <- pam_result$clustering
                private$.clusters <- clusters
            }

            # Generate cluster summary
            for (i in 1:n_clusters) {
                cluster_data <- data[clusters == i,]
                pattern <- private$.summarizePattern(cluster_data)

                self$results$clusterSummary$addRow(rowKey=i, values=list(
                    cluster = i,
                    size = sum(clusters == i),
                    pattern = pattern
                ))
            }
        },

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

            data <- self$data[self$options$markers]

            # Create heatmap matrix while preserving categorical nature
            heatmap_data <- as.matrix(sapply(data, as.numeric))

            # Create annotation
            annotation <- data.frame(
                Cluster=factor(private$.clusters)
            )
            rownames(annotation) <- rownames(data)

            # Generate heatmap
            pheatmap::pheatmap(heatmap_data,
                               annotation_row=annotation,
                               clustering_method="complete",
                               show_rownames=FALSE,
                               main="IHC Expression Patterns"
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
    )
)
