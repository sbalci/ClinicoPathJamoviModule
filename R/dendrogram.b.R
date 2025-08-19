
# This file is a generated template, your changes will not be overwritten

dendrogramClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "dendrogramClass",
    inherit = dendrogramBase,
    private = list(
        .run = function() {
            
            # Get options
            vars <- self$options$vars
            clusterMethod <- self$options$clusterMethod
            distanceMethod <- self$options$distanceMethod
            showLabels <- self$options$showLabels
            colorGroups <- self$options$colorGroups
            group <- self$options$group
            plotType <- self$options$plotType
            edgeType <- self$options$edgeType
            colorScheme <- self$options$colorScheme
            highlightClusters <- self$options$highlightClusters
            nClusters <- self$options$nClusters
            maxLabels <- self$options$maxLabels
            plotWidth <- self$options$plotWidth
            plotHeight <- self$options$plotHeight
            
            # Check if we have variables selected
            if (length(vars) == 0) {
                self$results$clusterInfo$setContent(
                    "<p>Please select variables for clustering analysis.</p>"
                )
                return()
            }
            
            # Get data
            data <- self$data
            
            # Prepare data for clustering
            clusterData <- data[vars]
            
            # Remove rows with missing values
            clusterData <- na.omit(clusterData)
            
            if (nrow(clusterData) < 2) {
                self$results$clusterInfo$setContent(
                    "<p>Insufficient data for clustering. Need at least 2 complete observations.</p>"
                )
                return()
            }
            
            # Validate group variable if specified
            groupData <- NULL
            if (colorGroups && !is.null(group) && group != "") {
                if (!group %in% names(data)) {
                    self$results$clusterInfo$setContent(
                        paste0("<p>Error: Grouping variable '", group, "' not found in data.</p>")
                    )
                    return()
                }
                groupData <- data[[group]]
                if (length(groupData) != nrow(data)) {
                    self$results$clusterInfo$setContent(
                        "<p>Error: Grouping variable length does not match data dimensions.</p>"
                    )
                    return()
                }
            }
            
            # Fill summary table
            summaryTable <- self$results$summary
            for (var in vars) {
                varData <- clusterData[[var]]
                if (is.numeric(varData)) {
                    summaryTable$addRow(rowKey = var, values = list(
                        variable = var,
                        n = length(varData),
                        mean = mean(varData, na.rm = TRUE),
                        sd = sd(varData, na.rm = TRUE),
                        missing = sum(is.na(data[[var]]))
                    ))
                }
            }
            
            # Compute distance matrix
            distMatrix <- dist(clusterData, method = distanceMethod)
            
            # Perform hierarchical clustering
            hclustResult <- hclust(distMatrix, method = clusterMethod)
            
            # Create cluster information
            nObs <- nrow(clusterData)
            clusterInfoText <- paste0(
                "<h3>Hierarchical Clustering Results</h3>",
                "<p><strong>Number of observations:</strong> ", nObs, "</p>",
                "<p><strong>Number of variables:</strong> ", length(vars), "</p>",
                "<p><strong>Variables used:</strong> ", paste(vars, collapse = ", "), "</p>",
                "<p><strong>Distance method:</strong> ", distanceMethod, "</p>",
                "<p><strong>Clustering method:</strong> ", clusterMethod, "</p>",
                "<p><strong>Plot type:</strong> ", plotType, "</p>"
            )
            
            if (highlightClusters) {
                clusterInfoText <- paste0(clusterInfoText,
                    "<p><strong>Highlighted clusters:</strong> ", nClusters, "</p>"
                )
            }
            
            self$results$clusterInfo$setContent(clusterInfoText)
            
            # Store clustering result for plotting
            image <- self$results$plot
            image$setState(list(
                hclustResult = hclustResult,
                data = clusterData,
                originalData = data,
                showLabels = showLabels,
                colorGroups = colorGroups,
                group = group,
                groupData = groupData,
                vars = vars,
                plotType = plotType,
                edgeType = edgeType,
                colorScheme = colorScheme,
                highlightClusters = highlightClusters,
                nClusters = nClusters,
                maxLabels = maxLabels,
                plotWidth = plotWidth,
                plotHeight = plotHeight
            ))
        },
        
        .plot = function(image, ggtheme, theme, ...) {
            
            state <- image$state
            
            if (is.null(state))
                return(FALSE)
            
            hclustResult <- state$hclustResult
            data <- state$data
            originalData <- state$originalData
            showLabels <- state$showLabels
            colorGroups <- state$colorGroups
            group <- state$group
            groupData <- state$groupData
            vars <- state$vars
            plotType <- state$plotType
            edgeType <- state$edgeType
            colorScheme <- state$colorScheme
            highlightClusters <- state$highlightClusters
            nClusters <- state$nClusters
            maxLabels <- state$maxLabels
            
            # Convert to dendrogram
            dendro <- as.dendrogram(hclustResult)
            
            if (plotType == "base") {
                # Basic R dendrogram plot
                return(private$.plotBaseDendrogram(hclustResult, dendro, data, showLabels, 
                                                 colorGroups, group, originalData, highlightClusters, 
                                                 nClusters, colorScheme, maxLabels))
            } else if (plotType == "linear" || plotType == "circular") {
                # ggraph-based dendrograms
                return(private$.plotGgraphDendrogram(hclustResult, dendro, data, showLabels, 
                                                   colorGroups, group, originalData, plotType, 
                                                   edgeType, colorScheme, highlightClusters, 
                                                   nClusters, maxLabels, ggtheme))
            }
            
            return(FALSE)
        },
        
        .plotBaseDendrogram = function(hclustResult, dendro, data, showLabels, colorGroups, 
                                     group, originalData, highlightClusters, nClusters, colorScheme, maxLabels) {
            
            if (highlightClusters && requireNamespace('dendextend', quietly = TRUE)) {
                # Enhanced dendrogram with dendextend
                tryCatch({
                    # Set colors based on clusters
                    colors <- private$.getColors(nClusters, colorScheme)
                    
                    dendro <- dendro %>%
                        dendextend::set("labels_col", value = colors, k = nClusters) %>%
                        dendextend::set("branches_k_color", value = colors, k = nClusters) %>%
                        dendextend::set("branches_lwd", 2) %>%
                        dendextend::set("labels_cex", 0.8)
                    
                    plot(dendro, main = "Hierarchical Clustering Dendrogram with Highlighted Clusters")
                }, error = function(e) {
                    warning("Error using dendextend package, falling back to basic plot: ", e$message)
                    # Fall back to basic plot with rectangles
                    plot(hclustResult, 
                         main = "Hierarchical Clustering Dendrogram",
                         xlab = "Observations", 
                         ylab = "Distance",
                         cex = 0.8)
                    colors <- private$.getColors(nClusters, colorScheme)
                    rect.hclust(hclustResult, k = nClusters, border = colors)
                })
                
            } else {
                # Basic dendrogram plot
                plot(hclustResult, 
                     main = "Hierarchical Clustering Dendrogram",
                     xlab = "Observations", 
                     ylab = "Distance",
                     cex = 0.8)
                
                if (highlightClusters) {
                    # Add colored rectangles around clusters
                    colors <- private$.getColors(nClusters, colorScheme)
                    rect.hclust(hclustResult, k = nClusters, border = colors)
                }
            }
            
            return(TRUE)
        },
        
        .plotGgraphDendrogram = function(hclustResult, dendro, data, showLabels, colorGroups, 
                                       group, originalData, plotType, edgeType, colorScheme, 
                                       highlightClusters, nClusters, maxLabels, ggtheme) {
            
            if (!requireNamespace('ggraph', quietly = TRUE) || !requireNamespace('igraph', quietly = TRUE)) {
                # Fall back to base plot instead of showing error message
                warning("ggraph/igraph packages not available, falling back to base plot")
                return(private$.plotBaseDendrogram(hclustResult, dendro, data, showLabels, 
                                                 colorGroups, group, originalData, highlightClusters, 
                                                 nClusters, colorScheme, maxLabels))
            }
            
            # Convert dendrogram to graph format for ggraph
            edges_df <- private$.dendrogramToEdges(dendro)
            vertices_df <- private$.createVertices(edges_df, data, colorGroups, group, groupData)
            
            # Create graph object
            mygraph <- igraph::graph_from_data_frame(edges_df, vertices = vertices_df)
            
            # Determine if circular layout
            circular <- (plotType == "circular")
            
            # Create base plot
            p <- ggraph::ggraph(mygraph, layout = 'dendrogram', circular = circular)
            
            # Add edges based on edge type
            if (edgeType == "diagonal") {
                p <- p + ggraph::geom_edge_diagonal(colour = "grey", alpha = 0.7)
            } else if (edgeType == "link") {
                p <- p + ggraph::geom_edge_link(colour = "grey", alpha = 0.7)
            } else {
                p <- p + ggraph::geom_edge_elbow(colour = "grey", alpha = 0.7)
            }
            
            # Add cluster coloring if requested
            if (highlightClusters) {
                cluster_colors <- private$.getClusterColors(hclustResult, nClusters, colorScheme)
                vertices_df$cluster <- cluster_colors[match(vertices_df$name, names(cluster_colors))]
                
                p <- p + ggraph::geom_node_point(ggplot2::aes(color = cluster, filter = leaf), 
                                               size = 2, alpha = 0.8)
            } else {
                p <- p + ggraph::geom_node_point(ggplot2::aes(filter = leaf), 
                                               size = 1.5, alpha = 0.8)
            }
            
            # Add labels if requested
            if (showLabels && nrow(data) <= maxLabels) {  # Use configurable label limit
                if (circular) {
                    # Special handling for circular layout
                    vertices_df <- private$.calculateCircularAngles(vertices_df)
                    p <- p + ggraph::geom_node_text(
                        ggplot2::aes(x = x*1.1, y = y*1.1, 
                                   label = name, 
                                   angle = angle, 
                                   hjust = hjust,
                                   filter = leaf), 
                        size = 3, alpha = 0.9
                    )
                } else {
                    p <- p + ggraph::geom_node_text(
                        ggplot2::aes(label = name, filter = leaf), 
                        angle = 90, hjust = 1, size = 3, alpha = 0.9
                    )
                }
            }
            
            # Apply color scheme
            if (highlightClusters) {
                colors <- private$.getColors(nClusters, colorScheme)
                p <- p + ggplot2::scale_color_manual(values = colors)
            }
            
            # Add title and theme
            title <- if (circular) "Circular Hierarchical Clustering Dendrogram" else "Hierarchical Clustering Dendrogram"
            p <- p + 
                ggplot2::labs(title = title) +
                ggplot2::theme_void() +
                ggplot2::theme(
                    plot.title = ggplot2::element_text(hjust = 0.5, size = 14),
                    legend.position = if (highlightClusters) "bottom" else "none"
                )
            
            print(p)
            return(TRUE)
        },
        
        .dendrogramToEdges = function(dendro) {
            # Convert dendrogram to edge list for ggraph
            edges <- data.frame(from = character(0), to = character(0), stringsAsFactors = FALSE)
            
            # Recursive function to extract edges
            extractEdges <- function(node, parent_name = NULL) {
                if (is.leaf(node)) {
                    leaf_name <- attr(node, "label")
                    if (!is.null(parent_name)) {
                        edges <<- rbind(edges, data.frame(from = parent_name, to = leaf_name, stringsAsFactors = FALSE))
                    }
                } else {
                    node_name <- paste0("node_", length(edges) + 1)
                    if (!is.null(parent_name)) {
                        edges <<- rbind(edges, data.frame(from = parent_name, to = node_name, stringsAsFactors = FALSE))
                    }
                    
                    for (i in 1:length(node)) {
                        extractEdges(node[[i]], node_name)
                    }
                }
            }
            
            # Start extraction from root
            extractEdges(dendro, "root")
            return(edges)
        },
        
        .createVertices = function(edges_df, data, colorGroups, group, groupData) {
            # Create vertices dataframe
            all_nodes <- unique(c(edges_df$from, edges_df$to))
            vertices_df <- data.frame(
                name = all_nodes,
                leaf = !all_nodes %in% edges_df$from,
                stringsAsFactors = FALSE
            )
            
            # Add group information if available and validated
            if (colorGroups && !is.null(groupData)) {
                # Match vertices to group data using more robust matching
                vertices_df$group <- NA
                leaf_indices <- which(vertices_df$leaf)
                
                # Get the original row indices that correspond to the clustering data
                original_indices <- as.numeric(rownames(data))
                if (is.null(original_indices)) {
                    original_indices <- seq_len(nrow(data))
                }
                
                for (i in leaf_indices) {
                    vertex_name <- vertices_df$name[i]
                    # Try to match by vertex name to row names or index
                    if (vertex_name %in% rownames(data)) {
                        data_row_idx <- which(rownames(data) == vertex_name)
                        if (length(data_row_idx) > 0) {
                            original_idx <- original_indices[data_row_idx[1]]
                            if (original_idx <= length(groupData)) {
                                vertices_df$group[i] <- as.character(groupData[original_idx])
                            }
                        }
                    }
                }
            }
            
            return(vertices_df)
        },
        
        .calculateCircularAngles = function(vertices_df) {
            # Calculate angles for circular dendrogram labels
            leaves <- which(vertices_df$leaf)
            n_leaves <- length(leaves)
            
            vertices_df$angle <- NA
            vertices_df$hjust <- NA
            
            for (i in seq_along(leaves)) {
                angle <- 90 - 360 * (i - 1) / n_leaves
                vertices_df$angle[leaves[i]] <- ifelse(angle < -90, angle + 180, angle)
                vertices_df$hjust[leaves[i]] <- ifelse(angle < -90, 1, 0)
            }
            
            return(vertices_df)
        },
        
        .getColors = function(n, colorScheme) {
            # Get color palette based on scheme
            if (colorScheme == "viridis" && requireNamespace('viridis', quietly = TRUE)) {
                return(viridis::viridis(n))
            } else if (colorScheme == "RdYlBu") {
                return(RColorBrewer::brewer.pal(min(n, 11), "RdYlBu"))
            } else if (colorScheme == "Set1") {
                return(RColorBrewer::brewer.pal(min(n, 9), "Set1"))
            } else if (colorScheme == "Dark2") {
                return(RColorBrewer::brewer.pal(min(n, 8), "Dark2"))
            } else {
                # Default rainbow colors
                return(rainbow(n))
            }
        },
        
        .getClusterColors = function(hclustResult, nClusters, colorScheme) {
            # Get cluster assignments and colors
            clusters <- cutree(hclustResult, k = nClusters)
            colors <- private$.getColors(nClusters, colorScheme)
            cluster_colors <- colors[clusters]
            names(cluster_colors) <- names(clusters)
            return(cluster_colors)
        })
)
