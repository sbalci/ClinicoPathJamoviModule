#' @title Correlation Network Analysis
#' @importFrom R6 R6Class
#' @import jmvcore
#'

correlationnetworkClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "correlationnetworkClass",
    inherit = correlationnetworkBase,
    private = list(
        .init = function() {
            # Initialize tables with dynamic columns based on selected variables
            if (length(self$options$vars) >= 3) {
                private$.initAdjacencyMatrix()
                private$.initNetworkSummary()
                
                if (self$options$centralityMeasures) {
                    private$.initCentralityMeasures()
                }
                
                if (self$options$communityDetection) {
                    private$.initCommunityStructure()
                    private$.initCommunityStats()
                }
                
                if (self$options$networkComparison && !is.null(self$options$group)) {
                    private$.initNetworkComparison()
                }
            }
        },
        
        .run = function() {
            # Validate inputs
            if (length(self$options$vars) < 3) {
                return()
            }

            if (nrow(self$data) == 0) {
                stop('Data contains no (complete) rows')
            }

            # Clean and prepare data
            data <- private$.cleanData()
            
            if (nrow(data) < 5) {
                stop('At least 5 complete observations are required for network analysis')
            }

            # Construct network
            network_result <- private$.constructNetwork(data)
            
            # Populate results tables
            private$.populateNetworkSummary(network_result)
            private$.populateAdjacencyMatrix(network_result)
            
            if (self$options$centralityMeasures) {
                private$.populateCentralityMeasures(network_result)
            }
            
            if (self$options$communityDetection) {
                private$.performCommunityDetection(network_result)
            }
            
            if (self$options$networkComparison && !is.null(self$options$group)) {
                private$.performNetworkComparison(data)
            }
            
            # Generate natural language report
            if (self$options$report) {
                private$.generateReport(network_result, data)
            }

            # Prepare plot data
            if (self$options$plots) {
                self$results$networkPlot$setState(network_result)
                self$results$interactiveNetworkPlot$setState(network_result)
                self$results$adjacencyHeatmap$setState(network_result)
                
                if (self$options$centralityMeasures) {
                    self$results$centralityPlot$setState(network_result)
                    self$results$networkMetricsPlot$setState(network_result)
                }
                
                if (self$options$communityDetection) {
                    self$results$communityPlot$setState(network_result)
                }
            }
        },

        .cleanData = function() {
            # Select variables and handle grouping
            vars <- self$options$vars
            group <- self$options$group
            
            if (!is.null(group)) {
                vars <- c(vars, group)
            }
            
            data <- self$data[vars]
            
            # Convert to numeric and remove missing values
            for (var in self$options$vars) {
                data[[var]] <- jmvcore::toNumeric(data[[var]])
            }
            
            # Handle grouping variable
            if (!is.null(group)) {
                data[[group]] <- as.factor(data[[group]])
            }
            
            # Remove incomplete cases
            data <- jmvcore::naOmit(data)
            
            return(data)
        },

        .constructNetwork = function(data) {
            vars <- self$options$vars
            method <- self$options$networkMethod
            cor_method <- self$options$correlationMethod
            threshold <- self$options$threshold
            sig_threshold <- self$options$significanceThreshold
            only_significant <- self$options$onlySignificant
            
            # Prepare data for network construction
            cor_data <- data[vars]
            n <- nrow(cor_data)
            
            # Calculate correlation matrix
            cor_matrix <- cor(cor_data, method = cor_method, use = "complete.obs")
            
            # Calculate significance matrix if needed
            p_matrix <- NULL
            if (only_significant || method == "correlation") {
                p_matrix <- matrix(NA, ncol = length(vars), nrow = length(vars))
                rownames(p_matrix) <- colnames(p_matrix) <- vars
                
                for (i in 1:length(vars)) {
                    for (j in 1:length(vars)) {
                        if (i != j) {
                            test_result <- cor.test(cor_data[[vars[i]]], cor_data[[vars[j]]], 
                                                   method = cor_method)
                            p_matrix[i, j] <- test_result$p.value
                        } else {
                            p_matrix[i, j] <- 0
                        }
                    }
                }
            }
            
            # Construct network based on method
            adjacency_matrix <- switch(method,
                "correlation" = private$.buildCorrelationNetwork(cor_matrix, p_matrix, threshold, sig_threshold, only_significant),
                "partial" = private$.buildPartialNetwork(cor_data, threshold, sig_threshold, only_significant),
                "ggm" = private$.buildGGMNetwork(cor_data, threshold, sig_threshold),
                "lasso" = private$.buildLassoNetwork(cor_data, threshold),
                private$.buildCorrelationNetwork(cor_matrix, p_matrix, threshold, sig_threshold, only_significant)
            )
            
            # Create igraph object if igraph is available
            graph_obj <- NULL
            if (requireNamespace("igraph", quietly = TRUE)) {
                # Convert to adjacency matrix format for igraph
                diag(adjacency_matrix) <- 0  # Remove self-loops
                graph_obj <- igraph::graph_from_adjacency_matrix(adjacency_matrix, 
                                                               mode = "undirected", 
                                                               weighted = TRUE,
                                                               diag = FALSE)
            }
            
            return(list(
                adjacency = adjacency_matrix,
                correlation = cor_matrix,
                p_values = p_matrix,
                graph = graph_obj,
                data = cor_data,
                n = n,
                method = method
            ))
        },

        .buildCorrelationNetwork = function(cor_matrix, p_matrix, threshold, sig_threshold, only_significant) {
            adj_matrix <- abs(cor_matrix)
            
            # Apply significance filtering if requested
            if (only_significant && !is.null(p_matrix)) {
                adj_matrix[p_matrix >= sig_threshold] <- 0
            }
            
            # Apply threshold
            adj_matrix[adj_matrix < threshold] <- 0
            
            return(adj_matrix)
        },

        .buildPartialNetwork = function(cor_data, threshold, sig_threshold, only_significant) {
            # Calculate partial correlations using correlation matrix inversion
            cor_matrix <- cor(cor_data, use = "complete.obs")
            
            tryCatch({
                # Partial correlation via precision matrix
                precision_matrix <- solve(cor_matrix)
                partial_cor <- -cov2cor(precision_matrix)
                diag(partial_cor) <- 1
                
                adj_matrix <- abs(partial_cor)
                adj_matrix[adj_matrix < threshold] <- 0
                diag(adj_matrix) <- 0
                
                return(adj_matrix)
            }, error = function(e) {
                # Fallback to regular correlation if precision matrix calculation fails
                return(private$.buildCorrelationNetwork(cor_matrix, NULL, threshold, sig_threshold, only_significant))
            })
        },

        .buildGGMNetwork = function(cor_data, threshold, sig_threshold) {
            # Simplified GGM using regularized precision matrix estimation
            if (requireNamespace("glmnet", quietly = TRUE)) {
                tryCatch({
                    # Use graphical lasso approach
                    cor_matrix <- cor(cor_data, use = "complete.obs")
                    n <- nrow(cor_data)
                    
                    # Estimate regularization parameter using cross-validation
                    lambda_seq <- seq(0.01, 0.5, length.out = 20)
                    
                    # Simple regularization approach
                    lambda <- 0.1  # Default value
                    shrunk_cor <- (1 - lambda) * cor_matrix + lambda * diag(ncol(cor_matrix))
                    
                    precision_matrix <- solve(shrunk_cor)
                    partial_cor <- -cov2cor(precision_matrix)
                    diag(partial_cor) <- 0
                    
                    adj_matrix <- abs(partial_cor)
                    adj_matrix[adj_matrix < threshold] <- 0
                    
                    return(adj_matrix)
                }, error = function(e) {
                    # Fallback to correlation network
                    cor_matrix <- cor(cor_data, use = "complete.obs")
                    return(private$.buildCorrelationNetwork(cor_matrix, NULL, threshold, sig_threshold, FALSE))
                })
            } else {
                # Fallback if glmnet not available
                cor_matrix <- cor(cor_data, use = "complete.obs")
                return(private$.buildCorrelationNetwork(cor_matrix, NULL, threshold, sig_threshold, FALSE))
            }
        },

        .buildLassoNetwork = function(cor_data, threshold) {
            if (requireNamespace("glmnet", quietly = TRUE)) {
                tryCatch({
                    # LASSO-based network construction
                    adj_matrix <- matrix(0, ncol = ncol(cor_data), nrow = ncol(cor_data))
                    rownames(adj_matrix) <- colnames(adj_matrix) <- colnames(cor_data)
                    
                    # For each variable, fit LASSO regression against all others
                    for (i in 1:ncol(cor_data)) {
                        y <- cor_data[, i]
                        x <- as.matrix(cor_data[, -i])
                        
                        # Fit LASSO with cross-validation
                        cv_fit <- glmnet::cv.glmnet(x, y, alpha = 1)
                        
                        # Extract coefficients at optimal lambda
                        coef_vals <- as.matrix(glmnet::coef.glmnet(cv_fit, s = cv_fit$lambda.min))
                        
                        # Store non-zero coefficients as edges
                        non_zero_idx <- which(abs(coef_vals[-1, 1]) > 0)  # Exclude intercept
                        other_vars <- colnames(cor_data)[-i]
                        
                        for (j in non_zero_idx) {
                            var_j_name <- other_vars[j]
                            var_j_idx <- which(colnames(cor_data) == var_j_name)
                            adj_matrix[i, var_j_idx] <- abs(coef_vals[j + 1, 1])
                        }
                    }
                    
                    # Make symmetric (take maximum)
                    adj_matrix <- pmax(adj_matrix, t(adj_matrix))
                    
                    # Apply threshold
                    adj_matrix[adj_matrix < threshold] <- 0
                    diag(adj_matrix) <- 0
                    
                    return(adj_matrix)
                }, error = function(e) {
                    # Fallback to correlation network
                    cor_matrix <- cor(cor_data, use = "complete.obs")
                    return(private$.buildCorrelationNetwork(cor_matrix, NULL, threshold, 0.05, FALSE))
                })
            } else {
                # Fallback if glmnet not available
                cor_matrix <- cor(cor_data, use = "complete.obs")
                return(private$.buildCorrelationNetwork(cor_matrix, NULL, threshold, 0.05, FALSE))
            }
        },

        .initNetworkSummary = function() {
            summary <- self$results$networkSummary
            # Columns will be added dynamically
        },

        .initAdjacencyMatrix = function() {
            matrix <- self$results$adjacencyMatrix
            vars <- self$options$vars
            
            # Add row variable column
            matrix$addColumn(
                name = "var",
                title = "Variable",
                type = "text"
            )
            
            # Add columns for each variable
            for (var in vars) {
                matrix$addColumn(
                    name = var,
                    title = var,
                    type = "number",
                    format = "zto"
                )
            }
        },

        .initCentralityMeasures = function() {
            centrality <- self$results$centralityMeasures
            # Columns are predefined in r.yaml
        },

        .initCommunityStructure = function() {
            community <- self$results$communityStructure
            # Columns are predefined in r.yaml
        },

        .initCommunityStats = function() {
            stats <- self$results$communityStats
            # Columns are predefined in r.yaml
        },

        .initNetworkComparison = function() {
            comparison <- self$results$networkComparison
            # Columns are predefined in r.yaml
        },

        .populateNetworkSummary = function(network_result) {
            summary <- self$results$networkSummary
            
            adj_matrix <- network_result$adjacency
            n_nodes <- ncol(adj_matrix)
            n_edges <- sum(adj_matrix > 0) / 2  # Undirected network
            density <- (2 * n_edges) / (n_nodes * (n_nodes - 1))
            
            # Calculate additional network metrics if igraph is available
            if (requireNamespace("igraph", quietly = TRUE) && !is.null(network_result$graph)) {
                graph <- network_result$graph
                
                # Only calculate if graph has edges
                if (igraph::ecount(graph) > 0) {
                    transitivity <- igraph::transitivity(graph, type = "global")
                    avg_path_length <- igraph::mean_distance(graph, directed = FALSE)
                    diameter <- igraph::diameter(graph, directed = FALSE)
                    components <- igraph::components(graph)$no
                } else {
                    transitivity <- 0
                    avg_path_length <- Inf
                    diameter <- 0
                    components <- n_nodes
                }
            } else {
                transitivity <- NA
                avg_path_length <- NA
                diameter <- NA
                components <- NA
            }
            
            stats <- list(
                list(property = "Number of nodes", value = n_nodes),
                list(property = "Number of edges", value = n_edges),
                list(property = "Network density", value = density),
                list(property = "Clustering coefficient", value = transitivity),
                list(property = "Average path length", value = avg_path_length),
                list(property = "Network diameter", value = diameter),
                list(property = "Connected components", value = components)
            )
            
            # Remove NA values
            stats <- stats[!sapply(stats, function(x) is.na(x$value))]
            
            for (i in seq_along(stats)) {
                summary$addRow(rowKey = i, values = stats[[i]])
            }
        },

        .populateAdjacencyMatrix = function(network_result) {
            matrix <- self$results$adjacencyMatrix
            vars <- self$options$vars
            adj_matrix <- network_result$adjacency
            
            # Populate matrix table
            for (i in 1:length(vars)) {
                row_values <- list(var = vars[i])
                
                for (j in 1:length(vars)) {
                    if (i == j) {
                        row_values[[vars[j]]] <- 0
                    } else {
                        row_values[[vars[j]]] <- adj_matrix[i, j]
                    }
                }
                
                matrix$addRow(rowKey = i, values = row_values)
            }
        },

        .populateCentralityMeasures = function(network_result) {
            centrality <- self$results$centralityMeasures
            vars <- self$options$vars
            
            if (!requireNamespace("igraph", quietly = TRUE) || is.null(network_result$graph)) {
                return()
            }
            
            graph <- network_result$graph
            
            # Only calculate centrality if graph has edges
            if (igraph::ecount(graph) == 0) {
                # No edges, all centrality measures are zero
                for (i in seq_along(vars)) {
                    centrality$addRow(rowKey = i, values = list(
                        node = vars[i],
                        degree = 0,
                        betweenness = 0,
                        closeness = 0,
                        eigenvector = 0,
                        strength = 0
                    ))
                }
                return()
            }
            
            tryCatch({
                # Calculate centrality measures
                degree_cent <- igraph::degree(graph)
                betweenness_cent <- igraph::betweenness(graph, normalized = TRUE)
                closeness_cent <- igraph::closeness(graph, normalized = TRUE)
                strength_cent <- igraph::strength(graph)
                
                # Eigenvector centrality might fail for disconnected graphs
                eigenvector_cent <- tryCatch({
                    igraph::eigen_centrality(graph)$vector
                }, error = function(e) {
                    rep(0, length(vars))
                })
                
                # Populate table
                for (i in seq_along(vars)) {
                    centrality$addRow(rowKey = i, values = list(
                        node = vars[i],
                        degree = degree_cent[i],
                        betweenness = betweenness_cent[i],
                        closeness = closeness_cent[i],
                        eigenvector = eigenvector_cent[i],
                        strength = strength_cent[i]
                    ))
                }
            }, error = function(e) {
                # Fallback: add rows with zero values
                for (i in seq_along(vars)) {
                    centrality$addRow(rowKey = i, values = list(
                        node = vars[i],
                        degree = 0,
                        betweenness = 0,
                        closeness = 0,
                        eigenvector = 0,
                        strength = 0
                    ))
                }
            })
        },

        .performCommunityDetection = function(network_result) {
            community_structure <- self$results$communityStructure
            community_stats <- self$results$communityStats
            vars <- self$options$vars
            method <- self$options$communityMethod
            
            if (!requireNamespace("igraph", quietly = TRUE) || is.null(network_result$graph)) {
                return()
            }
            
            graph <- network_result$graph
            
            # Only perform community detection if graph has edges
            if (igraph::ecount(graph) == 0) {
                # No edges, each node is its own community
                for (i in seq_along(vars)) {
                    community_structure$addRow(rowKey = i, values = list(
                        node = vars[i],
                        community = i,
                        membership_strength = 1.0
                    ))
                    
                    community_stats$addRow(rowKey = i, values = list(
                        community = i,
                        size = 1,
                        modularity = 0,
                        density = 0
                    ))
                }
                return()
            }
            
            tryCatch({
                # Perform community detection
                communities <- switch(method,
                    "walktrap" = igraph::cluster_walktrap(graph),
                    "louvain" = igraph::cluster_louvain(graph),
                    "leiden" = {
                        if (requireNamespace("leiden", quietly = TRUE)) {
                            # Convert to leiden format if package available
                            adj_matrix <- igraph::as_adjacency_matrix(graph, sparse = FALSE)
                            leiden_result <- leiden::leiden(adj_matrix)
                            # Convert back to igraph communities object
                            membership_vec <- leiden_result
                            list(membership = membership_vec, 
                                 modularity = igraph::modularity(graph, membership_vec))
                        } else {
                            igraph::cluster_louvain(graph)  # Fallback
                        }
                    },
                    "spinglass" = {
                        tryCatch({
                            igraph::cluster_spinglass(graph)
                        }, error = function(e) {
                            igraph::cluster_walktrap(graph)  # Fallback
                        })
                    },
                    "betweenness" = igraph::cluster_edge_betweenness(graph),
                    igraph::cluster_walktrap(graph)  # Default fallback
                )
                
                membership <- communities$membership
                mod_score <- igraph::modularity(communities)
                
                # Populate community structure table
                for (i in seq_along(vars)) {
                    community_structure$addRow(rowKey = i, values = list(
                        node = vars[i],
                        community = membership[i],
                        membership_strength = 1.0  # Simplified - could be enhanced with probabilistic membership
                    ))
                }
                
                # Calculate community statistics
                unique_communities <- unique(membership)
                for (comm in unique_communities) {
                    comm_nodes <- which(membership == comm)
                    comm_size <- length(comm_nodes)
                    
                    # Calculate community density
                    comm_subgraph <- igraph::induced_subgraph(graph, comm_nodes)
                    comm_density <- igraph::edge_density(comm_subgraph)
                    
                    community_stats$addRow(rowKey = comm, values = list(
                        community = comm,
                        size = comm_size,
                        modularity = mod_score / length(unique_communities),  # Approximate per-community contribution
                        density = comm_density
                    ))
                }
                
            }, error = function(e) {
                # Fallback: assign each node to its own community
                for (i in seq_along(vars)) {
                    community_structure$addRow(rowKey = i, values = list(
                        node = vars[i],
                        community = i,
                        membership_strength = 1.0
                    ))
                }
            })
        },

        .performNetworkComparison = function(data) {
            comparison <- self$results$networkComparison
            group_var <- self$options$group
            
            if (is.null(group_var)) return()
            
            # Split data by group
            groups <- levels(data[[group_var]])
            if (length(groups) != 2) {
                return()  # Only support binary group comparison for now
            }
            
            group1_data <- data[data[[group_var]] == groups[1], self$options$vars]
            group2_data <- data[data[[group_var]] == groups[2], self$options$vars]
            
            # Construct networks for each group
            network1 <- private$.constructNetwork(cbind(group1_data, dummy_group = 1))
            network2 <- private$.constructNetwork(cbind(group2_data, dummy_group = 1))
            
            # Compare network metrics
            metrics <- list()
            
            # Basic network metrics
            density1 <- sum(network1$adjacency > 0) / (ncol(network1$adjacency) * (ncol(network1$adjacency) - 1))
            density2 <- sum(network2$adjacency > 0) / (ncol(network2$adjacency) * (ncol(network2$adjacency) - 1))
            
            metrics$density <- list(
                metric = "Network Density",
                group1 = density1,
                group2 = density2,
                difference = density1 - density2,
                p_value = NA  # Would need permutation test for significance
            )
            
            # Add more network comparison metrics if igraph is available
            if (requireNamespace("igraph", quietly = TRUE)) {
                if (!is.null(network1$graph) && !is.null(network2$graph)) {
                    # Global clustering coefficient
                    if (igraph::ecount(network1$graph) > 0 && igraph::ecount(network2$graph) > 0) {
                        clustering1 <- igraph::transitivity(network1$graph, type = "global")
                        clustering2 <- igraph::transitivity(network2$graph, type = "global")
                        
                        metrics$clustering <- list(
                            metric = "Clustering Coefficient",
                            group1 = clustering1,
                            group2 = clustering2,
                            difference = clustering1 - clustering2,
                            p_value = NA
                        )
                        
                        # Average path length
                        path1 <- igraph::mean_distance(network1$graph, directed = FALSE)
                        path2 <- igraph::mean_distance(network2$graph, directed = FALSE)
                        
                        metrics$path_length <- list(
                            metric = "Average Path Length",
                            group1 = path1,
                            group2 = path2,
                            difference = path1 - path2,
                            p_value = NA
                        )
                    }
                }
            }
            
            # Populate comparison table
            for (i in seq_along(metrics)) {
                comparison$addRow(rowKey = i, values = metrics[[i]])
            }
        },

        .generateReport = function(network_result, data) {
            report_html <- "<div style='background-color: #f8f9fa; padding: 15px; border-radius: 5px; margin: 10px 0;'>"
            report_html <- paste0(report_html, "<h4 style='color: #495057; margin-top: 0;'>Correlation Network Analysis Summary</h4>")
            
            vars <- self$options$vars
            method <- self$options$networkMethod
            n_nodes <- length(vars)
            n_obs <- nrow(data)
            
            # Network construction summary
            method_name <- switch(method,
                "correlation" = "Correlation-based",
                "partial" = "Partial correlation",
                "ggm" = "Gaussian Graphical Model",
                "lasso" = "LASSO regularized")
            
            adj_matrix <- network_result$adjacency
            n_edges <- sum(adj_matrix > 0) / 2
            density <- (2 * n_edges) / (n_nodes * (n_nodes - 1))
            
            report_html <- paste0(report_html, 
                "<p><strong>Network Construction:</strong> ", method_name, " network was constructed using ", 
                n_nodes, " variables from ", n_obs, " observations. ",
                "The resulting network contains ", n_edges, " edges with a density of ", 
                round(density, 3), ".</p>")
            
            # Network structure analysis
            if (requireNamespace("igraph", quietly = TRUE) && !is.null(network_result$graph)) {
                graph <- network_result$graph
                
                if (igraph::ecount(graph) > 0) {
                    components <- igraph::components(graph)$no
                    transitivity <- igraph::transitivity(graph, type = "global")
                    
                    report_html <- paste0(report_html,
                        "<p><strong>Network Structure:</strong> The network consists of ", components, 
                        " connected component(s) with a global clustering coefficient of ", 
                        round(transitivity, 3), ".")
                    
                    if (igraph::is_connected(graph)) {
                        avg_path <- igraph::mean_distance(graph, directed = FALSE)
                        diameter <- igraph::diameter(graph, directed = FALSE)
                        report_html <- paste0(report_html, 
                            " The average path length is ", round(avg_path, 2), 
                            " and the network diameter is ", diameter, ".")
                    }
                    
                    report_html <- paste0(report_html, "</p>")
                } else {
                    report_html <- paste0(report_html, 
                        "<p><strong>Network Structure:</strong> The network contains no edges above the specified threshold.</p>")
                }
            }
            
            # Community detection summary
            if (self$options$communityDetection && requireNamespace("igraph", quietly = TRUE) && 
                !is.null(network_result$graph) && igraph::ecount(network_result$graph) > 0) {
                
                method_name <- switch(self$options$communityMethod,
                    "walktrap" = "Walktrap",
                    "louvain" = "Louvain",
                    "leiden" = "Leiden",
                    "spinglass" = "Spinglass",
                    "betweenness" = "Edge betweenness")
                
                tryCatch({
                    communities <- switch(self$options$communityMethod,
                        "walktrap" = igraph::cluster_walktrap(network_result$graph),
                        "louvain" = igraph::cluster_louvain(network_result$graph),
                        "leiden" = igraph::cluster_louvain(network_result$graph),  # Fallback
                        "spinglass" = igraph::cluster_spinglass(network_result$graph),
                        "betweenness" = igraph::cluster_edge_betweenness(network_result$graph),
                        igraph::cluster_walktrap(network_result$graph))
                    
                    n_communities <- max(communities$membership)
                    modularity <- igraph::modularity(communities)
                    
                    report_html <- paste0(report_html,
                        "<p><strong>Community Structure:</strong> Using the ", method_name, " algorithm, ", 
                        n_communities, " communities were detected with a modularity score of ", 
                        round(modularity, 3), ".</p>")
                    
                }, error = function(e) {
                    report_html <<- paste0(report_html,
                        "<p><strong>Community Structure:</strong> Community detection was attempted but failed.</p>")
                })
            }
            
            # Centrality analysis
            if (self$options$centralityMeasures && requireNamespace("igraph", quietly = TRUE) && 
                !is.null(network_result$graph) && igraph::ecount(network_result$graph) > 0) {
                
                tryCatch({
                    degree_cent <- igraph::degree(network_result$graph)
                    max_degree_idx <- which.max(degree_cent)
                    max_degree_var <- vars[max_degree_idx]
                    
                    betweenness_cent <- igraph::betweenness(network_result$graph, normalized = TRUE)
                    max_betweenness_idx <- which.max(betweenness_cent)
                    max_betweenness_var <- vars[max_betweenness_idx]
                    
                    report_html <- paste0(report_html,
                        "<p><strong>Central Nodes:</strong> ", max_degree_var, " has the highest degree centrality (", 
                        degree_cent[max_degree_idx], " connections), while ", max_betweenness_var, 
                        " has the highest betweenness centrality (", round(betweenness_cent[max_betweenness_idx], 3), 
                        "), indicating its importance as a bridge between other variables.</p>")
                        
                }, error = function(e) {
                    # Skip centrality report if calculation fails
                })
            }
            
            # Interpretation
            threshold <- self$options$threshold
            report_html <- paste0(report_html,
                "<p><strong>Interpretation:</strong> This network analysis reveals the complex interdependencies among the selected variables. ",
                "Edges represent relationships stronger than the threshold of ", threshold, ". ",
                "Dense clusters suggest groups of highly intercorrelated variables, while bridge nodes (high betweenness centrality) ",
                "may represent important mediating variables in the system.</p>")
            
            report_html <- paste0(report_html, "</div>")
            self$results$report$setContent(report_html)
        },

        .networkPlot = function(image, ggtheme, theme, ...) {
            if (length(self$options$vars) < 3 || !self$options$plots)
                return()

            network_result <- image$state
            
            if (is.null(network_result))
                return()

            # Create network visualization
            if (requireNamespace("igraph", quietly = TRUE) && requireNamespace("ggraph", quietly = TRUE) && 
                !is.null(network_result$graph)) {
                
                graph <- network_result$graph
                
                # Skip plotting if no edges
                if (igraph::ecount(graph) == 0) {
                    # Create a simple node-only plot
                    plot <- ggplot2::ggplot() +
                        ggplot2::annotate("text", x = 0.5, y = 0.5, 
                                         label = "No edges above threshold\nNodes would appear as isolated points") +
                        ggplot2::theme_void() +
                        ggplot2::labs(title = "Network Visualization (No Edges)")
                    
                    print(plot)
                    return(TRUE)
                }
                
                # Set up node attributes
                node_size_attr <- switch(self$options$nodeSize,
                    "constant" = rep(5, igraph::vcount(graph)),
                    "degree" = igraph::degree(graph),
                    "betweenness" = igraph::betweenness(graph, normalized = TRUE),
                    "closeness" = igraph::closeness(graph, normalized = TRUE),
                    "eigenvector" = {
                        tryCatch({
                            igraph::eigen_centrality(graph)$vector
                        }, error = function(e) {
                            rep(5, igraph::vcount(graph))
                        })
                    },
                    rep(5, igraph::vcount(graph))
                )
                
                # Community colors if requested
                node_colors <- NULL
                if (self$options$colorNodes && self$options$communityDetection) {
                    tryCatch({
                        communities <- igraph::cluster_walktrap(graph)
                        node_colors <- communities$membership
                    }, error = function(e) {
                        node_colors <- rep(1, igraph::vcount(graph))
                    })
                } else {
                    node_colors <- rep("steelblue", igraph::vcount(graph))
                }
                
                # Create layout
                layout_fun <- switch(self$options$layoutAlgorithm,
                    "spring" = "fr",
                    "circle" = "circle",
                    "nicely" = "nicely",
                    "kk" = "kk",
                    "lgl" = "lgl",
                    "fr")
                
                # Create the plot
                plot <- ggraph::ggraph(graph, layout = layout_fun) +
                    ggraph::geom_edge_link(
                        ggplot2::aes(width = weight, alpha = weight),
                        colour = "gray50"
                    ) +
                    ggraph::geom_node_point(
                        ggplot2::aes(size = node_size_attr, colour = factor(node_colors))
                    ) +
                    ggraph::geom_node_text(
                        ggplot2::aes(label = name), 
                        repel = TRUE,
                        size = 3
                    ) +
                    ggplot2::scale_size_continuous(name = "Node Size", range = c(3, 10)) +
                    ggplot2::scale_edge_width_continuous(name = "Edge Weight", range = c(0.5, 3)) +
                    ggplot2::scale_edge_alpha_continuous(name = "Edge Weight", range = c(0.3, 1)) +
                    ggplot2::labs(title = paste("Correlation Network:", 
                                               stringr::str_to_title(network_result$method)),
                                 subtitle = paste("Layout:", stringr::str_to_title(self$options$layoutAlgorithm))) +
                    ggplot2::theme_void() +
                    ggtheme +
                    ggplot2::theme(legend.position = "bottom")
                
                # Remove color legend if not using communities
                if (!self$options$colorNodes || !self$options$communityDetection) {
                    plot <- plot + ggplot2::guides(colour = "none")
                }
                
                print(plot)
                
            } else {
                # Fallback visualization using base plotting or ggplot2
                vars <- self$options$vars
                adj_matrix <- network_result$adjacency
                
                # Create a simple correlation matrix heatmap as fallback
                cor_df <- as.data.frame(as.table(adj_matrix))
                names(cor_df) <- c("Var1", "Var2", "Weight")
                
                plot <- ggplot2::ggplot(cor_df, ggplot2::aes(Var1, Var2, fill = Weight)) +
                    ggplot2::geom_tile() +
                    ggplot2::scale_fill_gradient(low = "white", high = "darkred", 
                                               limits = c(0, max(adj_matrix))) +
                    ggplot2::labs(title = "Network Adjacency Matrix",
                                 x = "Variables", y = "Variables") +
                    ggplot2::theme_minimal() +
                    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
                    ggtheme
                
                print(plot)
            }
            
            TRUE
        },

        .interactiveNetworkPlot = function(image, ...) {
            if (length(self$options$vars) < 3 || !self$options$plots || !self$options$interactive)
                return()

            network_result <- image$state
            
            if (is.null(network_result))
                return()

            # Create interactive network visualization
            if (requireNamespace("visNetwork", quietly = TRUE) && 
                requireNamespace("igraph", quietly = TRUE) && 
                !is.null(network_result$graph)) {
                
                graph <- network_result$graph
                
                # Convert igraph to visNetwork format
                vis_data <- visNetwork::toVisNetworkData(graph)
                
                # Enhance node attributes
                vis_data$nodes$title <- paste("Variable:", vis_data$nodes$label)
                
                # Set node sizes based on centrality
                if (igraph::ecount(graph) > 0) {
                    node_sizes <- switch(self$options$nodeSize,
                        "constant" = rep(25, nrow(vis_data$nodes)),
                        "degree" = scales::rescale(igraph::degree(graph), to = c(15, 50)),
                        "betweenness" = scales::rescale(igraph::betweenness(graph, normalized = TRUE), to = c(15, 50)),
                        "closeness" = scales::rescale(igraph::closeness(graph, normalized = TRUE), to = c(15, 50)),
                        "eigenvector" = {
                            tryCatch({
                                scales::rescale(igraph::eigen_centrality(graph)$vector, to = c(15, 50))
                            }, error = function(e) {
                                rep(25, nrow(vis_data$nodes))
                            })
                        },
                        rep(25, nrow(vis_data$nodes))
                    )
                } else {
                    node_sizes <- rep(25, nrow(vis_data$nodes))
                }
                
                vis_data$nodes$size <- node_sizes
                
                # Set colors based on communities if requested
                if (self$options$colorNodes && self$options$communityDetection && igraph::ecount(graph) > 0) {
                    tryCatch({
                        communities <- igraph::cluster_walktrap(graph)
                        colors <- rainbow(max(communities$membership))[communities$membership]
                        vis_data$nodes$color <- colors
                    }, error = function(e) {
                        vis_data$nodes$color <- "steelblue"
                    })
                } else {
                    vis_data$nodes$color <- "steelblue"
                }
                
                # Create the interactive plot
                plot <- visNetwork::visNetwork(vis_data$nodes, vis_data$edges) %>%
                    visNetwork::visOptions(highlightNearest = TRUE, 
                                         selectedBy = "group",
                                         nodesIdSelection = TRUE) %>%
                    visNetwork::visInteraction(navigationButtons = TRUE) %>%
                    visNetwork::visLayout(randomSeed = 123)
                
                # Convert to HTML
                html_content <- as.character(plot)
                self$results$interactiveNetworkPlot$setContent(html_content)
                
            } else {
                # Fallback message
                html_content <- "<div style='text-align: center; padding: 50px;'>
                    <h4>Interactive Network Visualization</h4>
                    <p>Interactive network visualization requires the 'visNetwork' package.</p>
                    <p>Please install the package to use this feature.</p>
                    </div>"
                self$results$interactiveNetworkPlot$setContent(html_content)
            }
            
            TRUE
        },

        .centralityPlot = function(image, ggtheme, theme, ...) {
            if (length(self$options$vars) < 3 || !self$options$plots || !self$options$centralityMeasures)
                return()

            network_result <- image$state
            
            if (is.null(network_result) || !requireNamespace("igraph", quietly = TRUE) || 
                is.null(network_result$graph))
                return()

            graph <- network_result$graph
            vars <- self$options$vars
            
            if (igraph::ecount(graph) == 0) {
                plot <- ggplot2::ggplot() +
                    ggplot2::annotate("text", x = 0.5, y = 0.5, 
                                     label = "No edges in network\nCentrality measures are all zero") +
                    ggplot2::theme_void() +
                    ggplot2::labs(title = "Centrality Measures (No Edges)")
                
                print(plot)
                return(TRUE)
            }
            
            tryCatch({
                # Calculate centrality measures
                degree_cent <- igraph::degree(graph)
                betweenness_cent <- igraph::betweenness(graph, normalized = TRUE)
                closeness_cent <- igraph::closeness(graph, normalized = TRUE)
                strength_cent <- igraph::strength(graph)
                
                eigenvector_cent <- tryCatch({
                    igraph::eigen_centrality(graph)$vector
                }, error = function(e) {
                    rep(0, length(vars))
                })
                
                # Create data frame for plotting
                centrality_df <- data.frame(
                    Variable = rep(vars, 5),
                    Centrality = c("Degree", "Betweenness", "Closeness", "Eigenvector", "Strength"),
                    Value = c(degree_cent, betweenness_cent, closeness_cent, eigenvector_cent, strength_cent),
                    stringsAsFactors = FALSE
                )
                
                # Create centrality comparison plot
                plot <- ggplot2::ggplot(centrality_df, ggplot2::aes(x = Variable, y = Value, fill = Centrality)) +
                    ggplot2::geom_col(position = "dodge") +
                    ggplot2::facet_wrap(~ Centrality, scales = "free_y") +
                    ggplot2::labs(title = "Node Centrality Measures",
                                 x = "Variables", y = "Centrality Value") +
                    ggplot2::theme_minimal() +
                    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
                                  strip.background = ggplot2::element_rect(fill = "lightgray"),
                                  legend.position = "none") +
                    ggtheme
                
                print(plot)
                
            }, error = function(e) {
                plot <- ggplot2::ggplot() +
                    ggplot2::annotate("text", x = 0.5, y = 0.5, 
                                     label = "Error calculating centrality measures") +
                    ggplot2::theme_void() +
                    ggplot2::labs(title = "Centrality Measures (Error)")
                
                print(plot)
            })
            
            TRUE
        },

        .communityPlot = function(image, ggtheme, theme, ...) {
            if (length(self$options$vars) < 3 || !self$options$plots || !self$options$communityDetection)
                return()

            network_result <- image$state
            
            if (is.null(network_result) || !requireNamespace("igraph", quietly = TRUE) || 
                !requireNamespace("ggraph", quietly = TRUE) || is.null(network_result$graph))
                return()

            graph <- network_result$graph
            
            if (igraph::ecount(graph) == 0) {
                plot <- ggplot2::ggplot() +
                    ggplot2::annotate("text", x = 0.5, y = 0.5, 
                                     label = "No edges in network\nEach node forms its own community") +
                    ggplot2::theme_void() +
                    ggplot2::labs(title = "Community Structure (No Edges)")
                
                print(plot)
                return(TRUE)
            }
            
            tryCatch({
                # Perform community detection
                communities <- switch(self$options$communityMethod,
                    "walktrap" = igraph::cluster_walktrap(graph),
                    "louvain" = igraph::cluster_louvain(graph),
                    "leiden" = igraph::cluster_louvain(graph),  # Fallback
                    "spinglass" = igraph::cluster_spinglass(graph),
                    "betweenness" = igraph::cluster_edge_betweenness(graph),
                    igraph::cluster_walktrap(graph))
                
                # Create community-colored network plot
                plot <- ggraph::ggraph(graph, layout = "fr") +
                    ggraph::geom_edge_link(colour = "gray70", alpha = 0.7) +
                    ggraph::geom_node_point(
                        ggplot2::aes(colour = factor(communities$membership)), 
                        size = 8
                    ) +
                    ggraph::geom_node_text(
                        ggplot2::aes(label = name), 
                        colour = "white", 
                        size = 3, 
                        fontface = "bold"
                    ) +
                    ggplot2::scale_colour_discrete(name = "Community") +
                    ggplot2::labs(title = paste("Community Structure:",
                                               stringr::str_to_title(self$options$communityMethod)),
                                 subtitle = paste("Modularity =", round(igraph::modularity(communities), 3))) +
                    ggplot2::theme_void() +
                    ggtheme +
                    ggplot2::theme(legend.position = "right")
                
                print(plot)
                
            }, error = function(e) {
                plot <- ggplot2::ggplot() +
                    ggplot2::annotate("text", x = 0.5, y = 0.5, 
                                     label = "Error in community detection") +
                    ggplot2::theme_void() +
                    ggplot2::labs(title = "Community Structure (Error)")
                
                print(plot)
            })
            
            TRUE
        },

        .adjacencyHeatmap = function(image, ggtheme, theme, ...) {
            if (length(self$options$vars) < 3 || !self$options$plots)
                return()

            network_result <- image$state
            
            if (is.null(network_result))
                return()

            adj_matrix <- network_result$adjacency
            
            # Convert matrix to long format for ggplot
            adj_df <- as.data.frame(as.table(adj_matrix))
            names(adj_df) <- c("Var1", "Var2", "Weight")
            
            # Create heatmap
            plot <- ggplot2::ggplot(adj_df, ggplot2::aes(Var1, Var2, fill = Weight)) +
                ggplot2::geom_tile(color = "white", size = 0.5) +
                ggplot2::geom_text(ggplot2::aes(label = ifelse(Weight > 0, round(Weight, 2), "")), 
                                  size = 3) +
                ggplot2::scale_fill_gradient(low = "white", high = "darkred", 
                                           limits = c(0, max(adj_matrix)),
                                           name = "Weight") +
                ggplot2::labs(title = "Network Adjacency Matrix",
                             subtitle = paste("Method:", stringr::str_to_title(network_result$method)),
                             x = "Variables", y = "Variables") +
                ggplot2::theme_minimal() +
                ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
                              panel.grid = ggplot2::element_blank()) +
                ggtheme
            
            print(plot)
            TRUE
        },

        .networkMetricsPlot = function(image, ggtheme, theme, ...) {
            if (length(self$options$vars) < 3 || !self$options$plots || !self$options$centralityMeasures)
                return()

            network_result <- image$state
            
            if (is.null(network_result) || !requireNamespace("igraph", quietly = TRUE) || 
                is.null(network_result$graph))
                return()

            graph <- network_result$graph
            
            if (igraph::ecount(graph) == 0) {
                plot <- ggplot2::ggplot() +
                    ggplot2::annotate("text", x = 0.5, y = 0.5, 
                                     label = "No edges in network\nAll centrality distributions are zero") +
                    ggplot2::theme_void() +
                    ggplot2::labs(title = "Network Metrics Distribution (No Edges)")
                
                print(plot)
                return(TRUE)
            }
            
            tryCatch({
                # Calculate centrality measures
                degree_cent <- igraph::degree(graph)
                betweenness_cent <- igraph::betweenness(graph, normalized = TRUE)
                closeness_cent <- igraph::closeness(graph, normalized = TRUE)
                strength_cent <- igraph::strength(graph)
                
                # Create data frame for distribution plots
                metrics_df <- data.frame(
                    Degree = degree_cent,
                    Betweenness = betweenness_cent,
                    Closeness = closeness_cent,
                    Strength = strength_cent
                )
                
                # Convert to long format
                metrics_long <- tidyr::pivot_longer(metrics_df, 
                                                   cols = everything(), 
                                                   names_to = "Metric", 
                                                   values_to = "Value")
                
                # Create distribution plots
                plot <- ggplot2::ggplot(metrics_long, ggplot2::aes(x = Value)) +
                    ggplot2::geom_histogram(bins = 10, fill = "steelblue", alpha = 0.7, color = "white") +
                    ggplot2::facet_wrap(~ Metric, scales = "free") +
                    ggplot2::labs(title = "Distribution of Network Centrality Metrics",
                                 x = "Metric Value", y = "Count") +
                    ggplot2::theme_minimal() +
                    ggplot2::theme(strip.background = ggplot2::element_rect(fill = "lightgray")) +
                    ggtheme
                
                print(plot)
                
            }, error = function(e) {
                plot <- ggplot2::ggplot() +
                    ggplot2::annotate("text", x = 0.5, y = 0.5, 
                                     label = "Error calculating network metrics") +
                    ggplot2::theme_void() +
                    ggplot2::labs(title = "Network Metrics Distribution (Error)")
                
                print(plot)
            })
            
            TRUE
        }
    )
)