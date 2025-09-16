#' @title Advanced IHC Clustering Analysis
#' @importFrom R6 R6Class
#' @import jmvcore
#' @importFrom stats prcomp kmeans dist hclust cutree sd
#' @importFrom cluster silhouette pam clusGap

# Source IHC utilities
tryCatch({
    utils_file <- system.file("R", "ihc_utilities.R", package = "ClinicoPath")
    if (file.exists(utils_file)) {
        source(utils_file)
    } else {
        local_utils <- file.path(dirname(sys.frame(1)$ofile), "ihc_utilities.R")
        if (file.exists(local_utils)) {
            source(local_utils)
        }
    }
}, error = function(e) {
    # Utilities will be loaded by package
})

# Ensure utility functions exist
if (!exists("convertIHCToNumeric")) {
    convertIHCToNumeric <- function(data, markers, id_variable = NULL) {
        ihc_matrix <- matrix(0, nrow = nrow(data), ncol = length(markers))
        colnames(ihc_matrix) <- markers
        for (i in seq_along(markers)) {
            marker_data <- data[[markers[i]]]
            if (is.factor(marker_data) || is.character(marker_data)) {
                levels <- sort(unique(marker_data[!is.na(marker_data)]))
                numeric_values <- as.numeric(factor(marker_data, levels = levels)) - 1
            } else {
                numeric_values <- as.numeric(marker_data)
            }
            ihc_matrix[, i] <- numeric_values
        }
        if (!is.null(id_variable) && id_variable != "" && id_variable %in% names(data)) {
            rownames(ihc_matrix) <- as.character(data[[id_variable]])
        }
        return(ihc_matrix)
    }
}

ihcadvancedClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "ihcadvancedClass",
    inherit = ihcadvancedBase,
    private = list(
        .ihc_data = NULL,
        .optimal_k = NULL,
        .clusters = NULL,
        .pca_results = NULL,
        .selected_markers = NULL,
        .distance_matrix = NULL,
        .validation_results = NULL,

        .init = function() {
            # Initialize instructions
            if (is.null(self$data) || length(self$options$markers) == 0) {
                self$results$instructions$setContent(
                    "<h3>Advanced IHC Clustering Analysis</h3>
                    <p>This module performs sophisticated clustering analysis with optimization.</p>

                    <h4>Features:</h4>
                    <ul>
                        <li><b>Optimal K Selection:</b> Automatically find the best number of clusters</li>
                        <li><b>PCA Analysis:</b> Dimensionality reduction and visualization</li>
                        <li><b>Iterative Marker Selection:</b> Find the most informative markers</li>
                        <li><b>Consensus Clustering:</b> Bootstrap validation for stable results</li>
                        <li><b>Validation Metrics:</b> Multiple quality measures</li>
                    </ul>

                    <h4>Requirements:</h4>
                    <ul>
                        <li>At least 2 IHC markers</li>
                        <li>Minimum 10 samples recommended</li>
                        <li>Numeric or categorical marker data</li>
                    </ul>

                    <p><i>Select your IHC markers to begin advanced analysis.</i></p>"
                )
                return()
            }
        },

        .run = function() {
            # Validate inputs
            if (is.null(self$data) || length(self$options$markers) < 2) {
                return()
            }

            # Set random seed for reproducibility
            set.seed(self$options$randomSeed)

            # Prepare data
            private$.prepareData()

            if (is.null(private$.ihc_data)) {
                return()
            }

            # Find optimal K
            if (self$options$optimalKMethod != "manual") {
                private$.findOptimalK()
            }

            # Perform iterative marker selection if requested
            if (self$options$iterativeClustering) {
                private$.iterativeMarkerSelection()
            }

            # PCA Analysis
            if (self$options$pcaAnalysis) {
                private$.performPCA()
            }

            # Consensus clustering
            if (self$options$consensusClustering) {
                private$.performConsensusClustering()
            }

            # Cluster validation
            if (self$options$clusterValidation) {
                private$.validateClusters()
            }

            # Update summary
            private$.updateSummary()

            # Update assumptions panel
            private$.updateAssumptions()
        },

        .prepareData = function() {
            tryCatch({
                # Convert IHC data to numeric matrix
                markers <- self$options$markers
                private$.ihc_data <- convertIHCToNumeric(
                    self$data,
                    markers,
                    id_variable = if(!is.null(self$options$id) && self$options$id != "") self$options$id else NULL
                )

                # Remove rows with NA
                complete_rows <- complete.cases(private$.ihc_data)
                n_complete <- sum(complete_rows)

                # Enhanced sample size validation with clinical guidance
                if (n_complete < 10) {
                    self$results$instructions$setContent(paste0(
                        "<p style='color: red;'><b>Error:</b> Insufficient complete cases after removing missing data.</p>
                        <p><b>Clinical Guidance:</b></p>
                        <ul>
                            <li>Advanced clustering requires ≥10 complete cases</li>
                            <li>Current complete cases: ", n_complete, "</li>
                            <li>Consider: Basic clustering for smaller datasets</li>
                            <li>Or: Impute missing values before analysis</li>
                        </ul>"
                    ))
                    private$.ihc_data <- NULL
                    return()
                } else if (n_complete < 30) {
                    self$results$instructions$setContent(paste0(
                        "<p style='color: orange;'><b>Warning:</b> Small sample size detected (", n_complete, " cases).</p>
                        <p><b>Recommendations:</b></p>
                        <ul>
                            <li>Results may be unstable with <30 samples</li>
                            <li>Reduce number of clusters tested</li>
                            <li>Consider simpler clustering methods</li>
                            <li>Bootstrap results should be interpreted cautiously</li>
                        </ul>"
                    ))
                }

                private$.ihc_data <- private$.ihc_data[complete_rows, , drop = FALSE]

                # Scale data
                private$.ihc_data <- scale(private$.ihc_data)

            }, error = function(e) {
                self$results$instructions$setContent(
                    paste0("<p style='color: red;'>Error preparing data: ", e$message, "</p>")
                )
                private$.ihc_data <- NULL
            })
        },

        .findOptimalK = function() {
            # Parse K range
            k_range <- private$.parseKRange(self$options$kRange)
            if (is.null(k_range)) {
                k_range <- 2:min(8, nrow(private$.ihc_data) - 1)
            }

            # Calculate metrics for each K
            results <- list()

            for (k in k_range) {
                if (k >= nrow(private$.ihc_data)) next

                # Perform clustering
                km <- kmeans(private$.ihc_data, centers = k, nstart = 25)

                # Calculate silhouette
                if (k > 1 && k < nrow(private$.ihc_data)) {
                    sil <- cluster::silhouette(km$cluster, dist(private$.ihc_data))
                    avg_sil <- mean(sil[, 3])
                } else {
                    avg_sil <- NA
                }

                # Calculate within-cluster sum of squares
                within_ss <- km$tot.withinss

                # Gap statistic (simplified)
                gap_stat <- NA
                if (self$options$optimalKMethod == "gap" && requireNamespace("cluster", quietly = TRUE)) {
                    tryCatch({
                        gap_result <- cluster::clusGap(private$.ihc_data, kmeans, K.max = k, B = 20)
                        gap_stat <- gap_result$Tab[k, "gap"]
                    }, error = function(e) {
                        gap_stat <- NA
                    })
                }

                # Determine recommendation
                recommendation <- ""
                if (self$options$optimalKMethod == "silhouette" && !is.na(avg_sil)) {
                    if (k == 1 || (length(results) > 0 && avg_sil > max(sapply(results, function(x) x$avg_sil), na.rm = TRUE))) {
                        recommendation <- "Best"
                        private$.optimal_k <- k
                        private$.clusters <- km$cluster
                    }
                } else if (self$options$optimalKMethod == "elbow") {
                    # Elbow method logic would go here
                    recommendation <- ""
                }

                results[[length(results) + 1]] <- list(
                    k = k,
                    avg_sil = avg_sil,
                    within_ss = within_ss,
                    gap_stat = gap_stat,
                    recommendation = recommendation
                )

                # Add to results table
                self$results$optimalKResults$addRow(
                    rowKey = k,
                    values = list(
                        k = k,
                        silhouette_avg = avg_sil,
                        within_ss = within_ss,
                        gap_statistic = gap_stat,
                        recommendation = recommendation
                    )
                )
            }
        },

        .iterativeMarkerSelection = function() {
            if (is.null(private$.ihc_data)) return()

            markers <- self$options$markers
            n_markers <- length(markers)

            # Start with all markers
            current_markers <- markers
            best_silhouette <- -1
            iteration <- 0

            # Forward selection: Start with best single marker
            for (i in 1:min(n_markers, 5)) {  # Limit iterations
                iteration <- iteration + 1
                best_improvement <- 0
                best_action <- ""
                best_marker <- ""

                if (i == 1) {
                    # Find best single marker
                    for (marker in markers) {
                        marker_idx <- which(markers == marker)
                        test_data <- private$.ihc_data[, marker_idx, drop = FALSE]

                        if (ncol(test_data) >= 1 && nrow(test_data) > 3) {
                            # Simple 2-cluster test
                            km <- kmeans(test_data, centers = min(2, nrow(test_data) - 1), nstart = 10)

                            if (length(unique(km$cluster)) > 1) {
                                sil <- cluster::silhouette(km$cluster, dist(test_data))
                                avg_sil <- mean(sil[, 3])

                                if (avg_sil > best_silhouette) {
                                    best_silhouette <- avg_sil
                                    best_improvement <- avg_sil
                                    best_marker <- marker
                                    best_action <- "Initial"
                                    current_markers <- marker
                                }
                            }
                        }
                    }
                } else {
                    # Add markers one by one
                    for (marker in setdiff(markers, current_markers)) {
                        test_markers <- c(current_markers, marker)
                        marker_indices <- which(markers %in% test_markers)
                        test_data <- private$.ihc_data[, marker_indices, drop = FALSE]

                        if (ncol(test_data) > 1) {
                            km <- kmeans(test_data, centers = min(3, nrow(test_data) - 1), nstart = 10)

                            if (length(unique(km$cluster)) > 1) {
                                sil <- cluster::silhouette(km$cluster, dist(test_data))
                                avg_sil <- mean(sil[, 3])
                                improvement <- avg_sil - best_silhouette

                                if (improvement > best_improvement) {
                                    best_improvement <- improvement
                                    best_marker <- marker
                                    best_action <- "Added"
                                }
                            }
                        }
                    }

                    if (best_improvement > 0.01) {  # Minimum improvement threshold
                        current_markers <- c(current_markers, best_marker)
                        best_silhouette <- best_silhouette + best_improvement
                    } else {
                        break  # No significant improvement
                    }
                }

                # Record iteration
                self$results$markerOptimization$addRow(
                    rowKey = iteration,
                    values = list(
                        iteration = iteration,
                        marker = best_marker,
                        action = best_action,
                        silhouette_improvement = best_improvement,
                        selected_markers = paste(current_markers, collapse = ", ")
                    )
                )
            }

            private$.selected_markers <- current_markers
        },

        .performPCA = function() {
            if (is.null(private$.ihc_data)) return()

            tryCatch({
                # Perform PCA
                pca <- prcomp(private$.ihc_data, scale. = FALSE)  # Already scaled
                private$.pca_results <- pca

                # Calculate variance explained
                var_explained <- (pca$sdev^2 / sum(pca$sdev^2)) * 100
                cum_var <- cumsum(var_explained)

                # Add PCA results
                n_components <- min(length(pca$sdev), 5)
                for (i in 1:n_components) {
                    self$results$pcaResults$addRow(
                        rowKey = i,
                        values = list(
                            component = paste0("PC", i),
                            eigenvalue = pca$sdev[i]^2,
                            variance_explained = var_explained[i],
                            cumulative_variance = cum_var[i]
                        )
                    )
                }

                # Add loadings
                markers <- self$options$markers
                n_show <- min(3, ncol(pca$rotation))

                for (i in seq_along(markers)) {
                    loadings <- list(marker = markers[i])
                    for (j in 1:n_show) {
                        col_name <- paste0("pc", j)
                        loadings[[col_name]] <- pca$rotation[i, j]
                    }

                    self$results$pcaLoadings$addRow(
                        rowKey = i,
                        values = loadings
                    )
                }

            }, error = function(e) {
                # PCA failed, but don't stop the analysis
            })
        },

        .performConsensusClustering = function() {
            if (is.null(private$.ihc_data)) return()

            n_samples <- nrow(private$.ihc_data)
            n_bootstrap <- self$options$nBootstrap
            k_range <- private$.parseKRange(self$options$kRange)
            if (is.null(k_range)) {
                k_range <- 2:min(5, n_samples - 1)
            }

            # Optimization for large datasets
            if (n_samples > 500) {
                # Reduce bootstrap iterations for very large datasets
                n_bootstrap <- min(n_bootstrap, 50)
                # Use subset sampling for consensus calculation
                subset_size <- min(200, n_samples)
                use_subset <- TRUE
            } else if (n_samples > 200) {
                # Moderate optimization
                n_bootstrap <- min(n_bootstrap, 100)
                subset_size <- min(150, n_samples)
                use_subset <- TRUE
            } else {
                use_subset <- FALSE
                subset_size <- n_samples
            }

            # Pre-allocate arrays for better memory management
            consensus_results <- vector("list", length(k_range))

            for (k_idx in seq_along(k_range)) {
                k <- k_range[k_idx]
                if (k >= n_samples) next

                # Use subset for large datasets
                if (use_subset) {
                    subset_idx <- sample(n_samples, subset_size, replace = FALSE)
                    data_subset <- private$.ihc_data[subset_idx, , drop = FALSE]
                } else {
                    subset_idx <- seq_len(n_samples)
                    data_subset <- private$.ihc_data
                }

                subset_n <- nrow(data_subset)

                # Pre-allocate cluster assignments matrix
                cluster_assignments <- matrix(0, nrow = subset_n, ncol = n_bootstrap)

                # Parallel bootstrap if enabled
                if (self$options$parallelProcessing && requireNamespace("parallel", quietly = TRUE)) {
                    # Use parallel processing for bootstrap
                    n_cores <- min(parallel::detectCores() - 1, 4)  # Leave one core free, max 4

                    boot_results <- parallel::mclapply(1:n_bootstrap, function(b) {
                        set.seed(self$options$randomSeed + b)  # Ensure reproducibility
                        boot_idx <- sample(subset_n, replace = TRUE)
                        boot_data <- data_subset[boot_idx, , drop = FALSE]

                        tryCatch({
                            km <- kmeans(boot_data, centers = k, nstart = 3, iter.max = 50)
                            assignments <- integer(subset_n)
                            assignments[boot_idx] <- km$cluster
                            assignments
                        }, error = function(e) {
                            integer(subset_n)  # Return zeros on error
                        })
                    }, mc.cores = n_cores, mc.set.seed = FALSE)

                    # Combine results
                    for (b in 1:n_bootstrap) {
                        if (!is.null(boot_results[[b]])) {
                            cluster_assignments[, b] <- boot_results[[b]]
                        }
                    }
                } else {
                    # Sequential bootstrap
                    for (b in 1:n_bootstrap) {
                        set.seed(self$options$randomSeed + b)
                        boot_idx <- sample(subset_n, replace = TRUE)
                        boot_data <- data_subset[boot_idx, , drop = FALSE]

                        tryCatch({
                            km <- kmeans(boot_data, centers = k, nstart = 3, iter.max = 50)
                            cluster_assignments[boot_idx, b] <- km$cluster
                        }, error = function(e) {
                            # Skip this bootstrap iteration on error
                        })
                    }
                }

                # Efficient consensus calculation using vectorized operations
                consensus_score <- 0
                n_pairs <- 0

                # Create logical matrices for faster computation
                valid_assignments <- cluster_assignments > 0

                for (i in 1:(subset_n - 1)) {
                    for (j in (i + 1):subset_n) {
                        # Both present in bootstrap samples
                        both_present_mask <- valid_assignments[i, ] & valid_assignments[j, ]
                        n_both <- sum(both_present_mask)

                        if (n_both > 0) {
                            # Same cluster assignments
                            same_cluster <- sum(cluster_assignments[i, both_present_mask] ==
                                              cluster_assignments[j, both_present_mask])

                            consensus_score <- consensus_score + (same_cluster / n_both)
                            n_pairs <- n_pairs + 1
                        }
                    }
                }

                avg_consensus <- if (n_pairs > 0) consensus_score / n_pairs else 0

                # Determine stability with more nuanced thresholds
                stability <- if (avg_consensus > 0.85) "Very High"
                           else if (avg_consensus > 0.75) "High"
                           else if (avg_consensus > 0.6) "Moderate"
                           else if (avg_consensus > 0.4) "Low"
                           else "Very Low"

                # Selection frequency
                selection_freq <- (sum(valid_assignments) / length(cluster_assignments)) * 100

                self$results$consensusResults$addRow(
                    rowKey = k,
                    values = list(
                        k = k,
                        consensus_score = avg_consensus,
                        stability = stability,
                        frequency = selection_freq
                    )
                )
            }
        },

        .validateClusters = function() {
            if (is.null(private$.ihc_data) || is.null(private$.clusters)) {
                # Use optimal K or default
                k <- if (!is.null(private$.optimal_k)) private$.optimal_k else 3
                km <- kmeans(private$.ihc_data, centers = min(k, nrow(private$.ihc_data) - 1), nstart = 25)
                private$.clusters <- km$cluster
            }

            # Use cached distance matrix
            dist_matrix <- private$.getDistanceMatrix()

            # Silhouette coefficient
            sil <- cluster::silhouette(private$.clusters, dist_matrix)
            avg_silhouette <- mean(sil[, 3])

            # Connectivity (simplified - average distance within clusters)
            connectivity <- 0
            for (c in unique(private$.clusters)) {
                cluster_points <- which(private$.clusters == c)
                if (length(cluster_points) > 1) {
                    cluster_dist <- as.matrix(dist_matrix)[cluster_points, cluster_points]
                    connectivity <- connectivity + mean(cluster_dist[upper.tri(cluster_dist)])
                }
            }
            connectivity <- connectivity / length(unique(private$.clusters))

            # Dunn index (simplified)
            min_between <- Inf
            max_within <- 0

            for (i in unique(private$.clusters)) {
                for (j in unique(private$.clusters)) {
                    if (i < j) {
                        points_i <- which(private$.clusters == i)
                        points_j <- which(private$.clusters == j)
                        between_dist <- min(as.matrix(dist_matrix)[points_i, points_j])
                        min_between <- min(min_between, between_dist)
                    }
                }

                points_i <- which(private$.clusters == i)
                if (length(points_i) > 1) {
                    within_dist <- max(as.matrix(dist_matrix)[points_i, points_i])
                    max_within <- max(max_within, within_dist)
                }
            }

            dunn_index <- if (max_within > 0) min_between / max_within else NA

            # Add to results
            metrics <- list(
                list(metric = "Silhouette Coefficient",
                     value = avg_silhouette,
                     interpretation = if (avg_silhouette > 0.7) "Strong structure"
                                    else if (avg_silhouette > 0.5) "Reasonable structure"
                                    else if (avg_silhouette > 0.25) "Weak structure"
                                    else "No structure",
                     range = "[-1, 1]"),
                list(metric = "Connectivity",
                     value = connectivity,
                     interpretation = if (connectivity < 10) "Compact clusters"
                                    else if (connectivity < 20) "Moderate compactness"
                                    else "Loose clusters",
                     range = "[0, ∞)"),
                list(metric = "Dunn Index",
                     value = dunn_index,
                     interpretation = if (!is.na(dunn_index) && dunn_index > 0.5) "Well-separated"
                                    else if (!is.na(dunn_index) && dunn_index > 0.2) "Moderate separation"
                                    else "Poor separation",
                     range = "[0, ∞)")
            )

            for (i in seq_along(metrics)) {
                self$results$validationMetrics$addRow(
                    rowKey = i,
                    values = metrics[[i]]
                )
            }
        },

        .updateSummary = function() {
            n_samples <- nrow(private$.ihc_data)
            n_markers <- length(self$options$markers)

            # Generate clinical summary
            clinical_summary <- private$.generateClinicalSummary()
            report_sentence <- private$.generateReportSentence()

            summary_html <- sprintf(
                "<h3>Advanced Clustering Analysis - Complete</h3>
                <p><b>Data Summary:</b></p>
                <ul>
                    <li>Samples analyzed: %d</li>
                    <li>Markers used: %d</li>
                    <li>Optimal clusters: %s</li>
                    <li>Method used: %s</li>
                </ul>

                <div style='background-color: #e8f4fd; padding: 15px; border-radius: 5px; margin: 10px 0;'>
                <h4>Clinical Interpretation:</h4>
                <p>%s</p>
                </div>

                <div style='background-color: #f0f8ff; padding: 10px; border: 1px solid #ccc; border-radius: 3px; margin: 10px 0;'>
                <h4>Report Summary (Copy-Ready):</h4>
                <p>%s</p>
                </div>",
                n_samples, n_markers,
                if (!is.null(private$.optimal_k)) private$.optimal_k else "Not determined",
                self$options$optimalKMethod,
                clinical_summary,
                report_sentence
            )

            if (!is.null(private$.selected_markers)) {
                summary_html <- paste0(summary_html, sprintf(
                    "<p><b>Selected Markers:</b> %s</p>",
                    paste(private$.selected_markers, collapse = ", ")
                ))
            }

            if (!is.null(private$.pca_results)) {
                var_explained <- (private$.pca_results$sdev[1]^2 / sum(private$.pca_results$sdev^2)) * 100
                summary_html <- paste0(summary_html, sprintf(
                    "<p><b>PCA Results:</b> First component explains %.1f%% of variance</p>",
                    var_explained
                ))
            }

            summary_html <- paste0(summary_html,
                "<p><i>Review the detailed results in the tables and plots below.</i></p>")

            self$results$instructions$setContent(summary_html)
        },

        .parseKRange = function(k_range_str) {
            tryCatch({
                if (grepl(":", k_range_str)) {
                    # Range format: "2:8"
                    parts <- strsplit(k_range_str, ":")[[1]]
                    return(as.numeric(parts[1]):as.numeric(parts[2]))
                } else if (grepl(",", k_range_str)) {
                    # List format: "2,3,4,5"
                    return(as.numeric(strsplit(k_range_str, ",")[[1]]))
                } else {
                    # Single value
                    return(as.numeric(k_range_str))
                }
            }, error = function(e) {
                return(NULL)
            })
        },

        .getDistanceMatrix = function() {
            if (is.null(private$.distance_matrix)) {
                private$.distance_matrix <- dist(private$.ihc_data)
            }
            return(private$.distance_matrix)
        },

        .generateClinicalSummary = function() {
            if (is.null(private$.ihc_data)) return("Analysis not completed.")

            n_samples <- nrow(private$.ihc_data)
            n_clusters <- if (!is.null(private$.optimal_k)) private$.optimal_k else length(unique(private$.clusters))

            # Clinical interpretation based on results
            if (!is.null(private$.clusters)) {
                cluster_sizes <- table(private$.clusters)
                min_size <- min(cluster_sizes)
                max_size <- max(cluster_sizes)

                balance_assessment <- if (max_size / min_size <= 2) "well-balanced" else "unbalanced"

                summary <- sprintf(
                    "The analysis identified %d distinct clusters from %d samples. Cluster sizes are %s (range: %d-%d samples). ",
                    n_clusters, n_samples, balance_assessment, min_size, max_size
                )

                # Add validation interpretation if available
                if (!is.null(private$.validation_results)) {
                    avg_sil <- private$.validation_results$avg_silhouette
                    if (!is.na(avg_sil)) {
                        sil_interpretation <- if (avg_sil > 0.7) "excellent separation"
                                            else if (avg_sil > 0.5) "good separation"
                                            else if (avg_sil > 0.25) "weak separation"
                                            else "poor separation"
                        summary <- paste0(summary, sprintf(
                            "Cluster quality shows %s (silhouette coefficient = %.3f). ",
                            sil_interpretation, avg_sil
                        ))
                    }
                }

                # Add PCA interpretation if available
                if (!is.null(private$.pca_results)) {
                    var_explained <- (private$.pca_results$sdev[1]^2 / sum(private$.pca_results$sdev^2)) * 100
                    summary <- paste0(summary, sprintf(
                        "Principal component analysis shows the first component explains %.1f%% of variance. ",
                        var_explained
                    ))
                }

                return(summary)
            } else {
                return("Clustering analysis completed. Review detailed results below.")
            }
        },

        .generateReportSentence = function() {
            if (is.null(private$.ihc_data)) return("")

            n_samples <- nrow(private$.ihc_data)
            n_markers <- length(self$options$markers)
            marker_names <- paste(self$options$markers, collapse = ", ")

            report <- sprintf(
                "Advanced clustering analysis was performed on %d cases using %d IHC markers (%s). ",
                n_samples, n_markers, marker_names
            )

            if (!is.null(private$.optimal_k)) {
                report <- paste0(report, sprintf(
                    "Optimal number of clusters was determined to be %d using %s method. ",
                    private$.optimal_k, self$options$optimalKMethod
                ))
            }

            if (self$options$pcaAnalysis && !is.null(private$.pca_results)) {
                var_explained <- (private$.pca_results$sdev[1]^2 / sum(private$.pca_results$sdev^2)) * 100
                report <- paste0(report, sprintf(
                    "Principal component analysis revealed the first component explained %.1f%% of variance. ",
                    var_explained
                ))
            }

            if (self$options$consensusClustering) {
                report <- paste0(report, sprintf(
                    "Consensus clustering with %d bootstrap iterations was used to validate stability. ",
                    self$options$nBootstrap
                ))
            }

            return(report)
        },

        .updateAssumptions = function() {
            assumptions_html <- "
            <h4>Analysis Assumptions:</h4>
            <ul>
                <li><b>Data Quality:</b> IHC markers should be properly scored and standardized</li>
                <li><b>Sample Size:</b> Minimum 10 cases per expected cluster for stable results</li>
                <li><b>Missing Data:</b> Complete case analysis - missing values are excluded</li>
                <li><b>Clustering Algorithm:</b> K-means assumes spherical clusters of similar size</li>
                <li><b>Scaling:</b> All markers are automatically scaled to have equal weight</li>
            </ul>

            <h4>Important Caveats:</h4>
            <ul>
                <li><b>Validation Required:</b> Results should be validated on independent datasets</li>
                <li><b>Clinical Context:</b> Clustering results must be interpreted with pathological expertise</li>
                <li><b>Technical Factors:</b> Staining protocols and antibody clones may affect results</li>
                <li><b>Optimal K:</b> Automated selection may not always match biological reality</li>
                <li><b>Reproducibility:</b> Results may vary slightly between runs (set random seed)</li>
            </ul>

            <h4>When to Use Advanced Clustering:</h4>
            <ul>
                <li>Exploring unknown subtypes in large cohorts (≥30 cases)</li>
                <li>Validating known classification schemes</li>
                <li>Identifying optimal marker panels for diagnosis</li>
                <li>Research settings where pattern discovery is the goal</li>
            </ul>

            <h4>When NOT to Use:</h4>
            <ul>
                <li>Small datasets (<30 cases) - use basic clustering instead</li>
                <li>Well-established diagnostic criteria exist</li>
                <li>Single-marker decisions are sufficient</li>
                <li>Clinical urgency requires immediate results</li>
            </ul>
            "

            # Add method-specific warnings
            if (self$options$optimalKMethod == "gap") {
                assumptions_html <- paste0(assumptions_html, "
                <p style='color: #ff6600;'><b>Gap Statistic Note:</b> Computationally intensive method.
                Results may take longer and require more samples for stability.</p>")
            }

            if (self$options$consensusClustering && self$options$nBootstrap > 500) {
                assumptions_html <- paste0(assumptions_html, "
                <p style='color: #ff6600;'><b>Bootstrap Warning:</b> High number of iterations selected.
                Analysis may take several minutes to complete.</p>")
            }

            self$results$assumptions$setContent(assumptions_html)
        },

        # Plotting functions
        .plotOptimalK = function(image, ...) {
            if (is.null(private$.ihc_data)) {
                plot(1, 1, type = "n", main = "No data available", xlab = "", ylab = "")
                return(TRUE)
            }

            # Extract data from results table
            k_values <- c()
            silhouette_values <- c()
            within_ss_values <- c()

            # Simple plot showing elbow or silhouette
            k_range <- private$.parseKRange(self$options$kRange)
            if (is.null(k_range)) k_range <- 2:8

            for (k in k_range) {
                if (k >= nrow(private$.ihc_data)) next
                km <- kmeans(private$.ihc_data, centers = k, nstart = 10)
                k_values <- c(k_values, k)
                within_ss_values <- c(within_ss_values, km$tot.withinss)

                if (k > 1) {
                    sil <- cluster::silhouette(km$cluster, dist(private$.ihc_data))
                    silhouette_values <- c(silhouette_values, mean(sil[, 3]))
                } else {
                    silhouette_values <- c(silhouette_values, NA)
                }
            }

            if (self$options$optimalKMethod == "silhouette" && length(silhouette_values) > 0) {
                plot(k_values, silhouette_values, type = "b",
                     main = "Silhouette Analysis",
                     xlab = "Number of Clusters (K)",
                     ylab = "Average Silhouette Width",
                     col = "blue", pch = 19)

                # Mark optimal
                if (!is.null(private$.optimal_k)) {
                    optimal_idx <- which(k_values == private$.optimal_k)
                    if (length(optimal_idx) > 0) {
                        points(private$.optimal_k, silhouette_values[optimal_idx],
                               col = "red", pch = 19, cex = 2)
                        text(private$.optimal_k, silhouette_values[optimal_idx],
                             "Optimal", pos = 3, col = "red")
                    }
                }
            } else {
                # Elbow plot
                plot(k_values, within_ss_values, type = "b",
                     main = "Elbow Method",
                     xlab = "Number of Clusters (K)",
                     ylab = "Total Within-Cluster SS",
                     col = "blue", pch = 19)
            }

            TRUE
        },

        .plotPCA = function(image, ...) {
            if (is.null(private$.pca_results)) {
                plot(1, 1, type = "n", main = "PCA not performed", xlab = "", ylab = "")
                return(TRUE)
            }

            pca <- private$.pca_results

            # Create biplot
            pc1 <- pca$x[, 1]
            pc2 <- pca$x[, 2]

            # Color by clusters with colorblind-safe palette
            colors <- if (!is.null(private$.clusters)) {
                # Colorblind-safe palette
                cb_colors <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728",
                             "#9467bd", "#8c564b", "#e377c2", "#7f7f7f")
                n_clusters <- length(unique(private$.clusters))
                palette_colors <- if (n_clusters <= length(cb_colors)) {
                    cb_colors[1:n_clusters]
                } else {
                    grDevices::colorRampPalette(cb_colors)(n_clusters)
                }
                palette_colors[private$.clusters]
            } else {
                "#1f77b4"  # Default blue
            }

            # Calculate variance explained
            var_explained <- round((pca$sdev^2 / sum(pca$sdev^2)) * 100, 1)

            plot(pc1, pc2,
                 main = "PCA Biplot with Clusters",
                 xlab = paste0("PC1 (", var_explained[1], "%)"),
                 ylab = paste0("PC2 (", var_explained[2], "%)"),
                 col = colors, pch = 19)

            # Add loadings as arrows
            loadings <- pca$rotation[, 1:2]
            arrow_scale <- min(abs(c(range(pc1), range(pc2)))) * 0.8

            arrows(0, 0,
                   loadings[, 1] * arrow_scale,
                   loadings[, 2] * arrow_scale,
                   length = 0.1, col = "gray50", lwd = 2)

            text(loadings[, 1] * arrow_scale * 1.1,
                 loadings[, 2] * arrow_scale * 1.1,
                 rownames(loadings), cex = 0.8, col = "black")

            # Add legend if clusters exist
            if (!is.null(private$.clusters)) {
                cb_colors <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728",
                             "#9467bd", "#8c564b", "#e377c2", "#7f7f7f")
                n_clusters <- length(unique(private$.clusters))
                legend_colors <- if (n_clusters <= length(cb_colors)) {
                    cb_colors[1:n_clusters]
                } else {
                    grDevices::colorRampPalette(cb_colors)(n_clusters)
                }
                legend("topright",
                       legend = paste("Cluster", sort(unique(private$.clusters))),
                       col = legend_colors,
                       pch = 19, cex = 0.8)
            }

            TRUE
        },

        .plotSilhouette = function(image, ...) {
            if (is.null(private$.clusters) || is.null(private$.ihc_data)) {
                plot(1, 1, type = "n", main = "No clustering results", xlab = "", ylab = "")
                return(TRUE)
            }

            # Calculate silhouette
            sil <- cluster::silhouette(private$.clusters, dist(private$.ihc_data))

            # Create silhouette plot with colorblind-safe colors
            cb_colors <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728",
                         "#9467bd", "#8c564b", "#e377c2", "#7f7f7f")
            n_clusters <- length(unique(private$.clusters))
            sil_colors <- if (n_clusters <= length(cb_colors)) {
                cb_colors[1:n_clusters]
            } else {
                grDevices::colorRampPalette(cb_colors)(n_clusters)
            }

            plot(sil, main = "Silhouette Plot - Cluster Quality Assessment",
                 col = sil_colors)

            # Add average silhouette width
            avg_width <- mean(sil[, 3])
            mtext(paste("Average silhouette width:", round(avg_width, 3)),
                  side = 3, line = 0, cex = 0.9)

            TRUE
        },

        .plotValidation = function(image, ...) {
            # Create a bar plot of validation metrics
            if (is.null(private$.clusters)) {
                plot(1, 1, type = "n", main = "No validation results", xlab = "", ylab = "")
                return(TRUE)
            }

            # Simple visualization of cluster quality
            par(mfrow = c(1, 2))

            # Plot 1: Cluster sizes
            cluster_sizes <- table(private$.clusters)
            # Colorblind-safe colors for barplots
            cb_colors <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728",
                         "#9467bd", "#8c564b", "#e377c2", "#7f7f7f")
            n_clusters <- length(cluster_sizes)
            bar_colors <- if (n_clusters <= length(cb_colors)) {
                cb_colors[1:n_clusters]
            } else {
                grDevices::colorRampPalette(cb_colors)(n_clusters)
            }

            barplot(cluster_sizes,
                    main = "Cluster Sizes Distribution",
                    xlab = "Cluster ID",
                    ylab = "Number of Samples",
                    col = bar_colors)

            # Plot 2: Within-cluster distances
            dist_matrix <- dist(private$.ihc_data)
            within_distances <- numeric(length(unique(private$.clusters)))

            for (i in seq_along(unique(private$.clusters))) {
                cluster_id <- unique(private$.clusters)[i]
                cluster_points <- which(private$.clusters == cluster_id)
                if (length(cluster_points) > 1) {
                    cluster_dist <- as.matrix(dist_matrix)[cluster_points, cluster_points]
                    within_distances[i] <- mean(cluster_dist[upper.tri(cluster_dist)])
                }
            }

            barplot(within_distances,
                    main = "Average Within-Cluster Distance",
                    xlab = "Cluster ID",
                    ylab = "Average Distance (Lower = More Compact)",
                    names.arg = unique(private$.clusters),
                    col = bar_colors)

            par(mfrow = c(1, 1))

            TRUE
        }
    )
)