#' @title Basic IHC Analysis
#' @importFrom R6 R6Class
#' @import jmvcore
#' @importFrom cluster daisy pam silhouette
#' @importFrom stats hclust cutree kmeans dist kruskal.test
#' @import ggplot2

ihcbasicClass <- if (requireNamespace('jmvcore', quietly = TRUE)) R6::R6Class(
    "ihcbasicClass",
    inherit = ihcbasicBase,
    private = list(
        .ihc_matrix = NULL,
        .clusters = NULL,
        .dist_matrix = NULL,
        .hc = NULL,

        .init = function() {
            # Display initial instructions
            if (is.null(self$data) || length(self$options$markers) == 0) {
                self$results$instructions$setContent(
                    "<h3>Basic IHC Expression Analysis</h3>
                    <p>This analysis performs clustering of samples based on IHC marker expression patterns.</p>

                    <h4>Required Input:</h4>
                    <ul>
                        <li><b>IHC Markers:</b> Select at least 2 marker variables</li>
                        <li><b>Data format:</b> Categorical (0/1/2/3) or continuous (H-scores)</li>
                        <li><b>Minimum samples:</b> At least 3 cases required</li>
                    </ul>

                    <h4>Analysis Steps:</h4>
                    <ol>
                        <li>Data validation and conversion</li>
                        <li>Distance matrix calculation</li>
                        <li>Clustering analysis</li>
                        <li>H-score calculation (if requested)</li>
                        <li>Cluster quality assessment</li>
                    </ol>

                    <p><i>Select your IHC markers to begin analysis.</i></p>"
                )
            }
        },

        .run = function() {
            # Step 1: Validate data
            if (is.null(self$data) || length(self$options$markers) == 0) {
                return()
            }

            validation <- private$.validateData()
            if (!validation$valid) {
                self$results$instructions$setContent(
                    paste0("<p style='color: red;'><b>Error:</b> ", validation$message, "</p>")
                )
                return()
            }

            # Step 2: Prepare data
            private$.prepareData()

            # Step 3: Perform clustering
            private$.performClustering()

            # Step 4: Calculate summaries
            private$.calculateSummaries()

            # Step 5: H-score analysis if requested
            if (self$options$computeHScore) {
                private$.calculateHScores()
            }

            # Step 6: Silhouette analysis if requested
            if (self$options$silhouetteAnalysis) {
                private$.performSilhouetteAnalysis()
            }

            # Step 7: Generate clinical interpretations
            private$.generateClinicalInterpretation()

            # Step 8: Update instructions with results summary
            private$.updateInstructions()
        },

        .validateData = function() {
            # Use existing IHC utility function for base validation
            validation <- validateIHCData(self$data, self$options$markers)
            if (!validation$valid) {
                return(validation)
            }

            # Additional ihcbasic-specific validation
            if (length(self$options$markers) < 2) {
                return(list(valid = FALSE, message = "At least 2 IHC markers are required for clustering"))
            }

            # Clinical significance warnings
            warnings <- private$.assessClinicalSignificance()
            if (length(warnings) > 0) {
                warning_html <- paste0(
                    "<div style='background-color: #fff3cd; border: 1px solid #ffeaa7; padding: 10px; margin: 10px 0;'>",
                    "<h4 style='color: #856404; margin-top: 0;'>⚠️ Clinical Considerations:</h4>",
                    "<ul>",
                    paste0("<li>", warnings, "</li>", collapse = ""),
                    "</ul></div>"
                )

                self$results$instructions$setContent(paste0(
                    "<h3>Basic IHC Expression Analysis</h3>",
                    warning_html,
                    "<p>This analysis performs clustering of samples based on IHC marker expression patterns.</p>
                    <p><i>Review the warnings above before interpreting results.</i></p>"
                ))
            }

            return(list(valid = TRUE))
        },

        .assessClinicalSignificance = function() {
            warnings <- character(0)
            n_samples <- nrow(self$data)
            n_clusters <- self$options$nClusters

            # Warning for too many clusters relative to sample size
            if (n_clusters > n_samples / 3) {
                warnings <- c(warnings,
                    sprintf("Too many clusters (%d) for sample size (%d). Consider using %d or fewer clusters for reliable results.",
                           n_clusters, n_samples, max(2, floor(n_samples / 3))))
            }

            # Warning for small sample size
            if (n_samples < 10) {
                warnings <- c(warnings,
                    "Small sample size (<10) may produce unstable clustering results. Interpret with caution.")
            }

            # Warning for inappropriate clustering method
            if (self$options$clusterMethod == "kmeans" && n_samples < 30) {
                warnings <- c(warnings,
                    "K-means clustering may not be optimal for small datasets. Consider hierarchical clustering instead.")
            }

            # Warning for standardization with binary data
            if (self$options$standardizeData && self$options$scoringScale == "binary") {
                warnings <- c(warnings,
                    "Standardization is not recommended for binary data. Consider disabling 'Standardize Data' option.")
            }

            return(warnings)
        },

        .prepareData = function() {
            # Use IHC utility function for basic data conversion
            private$.ihc_matrix <- convertIHCToNumeric(
                self$data,
                self$options$markers,
                id_variable = if(!is.null(self$options$id) && self$options$id != "") self$options$id else NULL
            )

            # Apply scoring scale transformations
            private$.applyScoring()

            # Apply standardization if requested
            if (self$options$standardizeData && self$options$scoringScale != "binary") {
                private$.ihc_matrix <- scale(private$.ihc_matrix)
            }

            # Calculate distance matrix
            private$.calculateDistanceMatrix()
        },

        .applyScoring = function() {
            if (self$options$scoringScale == "binary") {
                private$.ihc_matrix <- ifelse(private$.ihc_matrix > 0, 1, 0)
            } else if (self$options$scoringScale == "standard") {
                private$.ihc_matrix <- pmin(pmax(private$.ihc_matrix, 0), 3)
            }
            # hscore scaling is handled in separate H-score calculation
        },

        .convertToHScore = function(matrix_data) {
            # Convert to H-score scale (0-300)
            hscore_matrix <- matrix_data * 100
            hscore_matrix <- pmin(hscore_matrix, 300)
            return(hscore_matrix)
        },

        .calculateDistanceMatrix = function() {
            if (self$options$distanceMetric == "gower") {
                private$.dist_matrix <- cluster::daisy(private$.ihc_matrix, metric = "gower")
            } else if (self$options$distanceMetric == "jaccard") {
                binary_data <- private$.ihc_matrix > 0
                private$.dist_matrix <- dist(binary_data, method = "binary")
            }
        },

        .performClustering = function() {
            method <- self$options$clusterMethod
            k <- self$options$nClusters

            if (method == "hierarchical") {
                private$.hc <- hclust(private$.dist_matrix, method = self$options$linkageMethod)
                private$.clusters <- cutree(private$.hc, k = k)
            } else if (method == "kmeans") {
                set.seed(123)  # For reproducibility
                km <- kmeans(private$.ihc_matrix, centers = k, nstart = 25)
                private$.clusters <- km$cluster
            } else if (method == "pam") {
                pam_result <- cluster::pam(private$.dist_matrix, k = k, diss = TRUE)
                private$.clusters <- pam_result$clustering
            }
        },

        .calculateSummaries = function() {
            # Clear existing rows
            self$results$clusterSummary$setNote("clusterNote", NULL)

            # Calculate cluster summaries
            clusters <- private$.clusters
            n_clusters <- length(unique(clusters))

            for (i in 1:n_clusters) {
                cluster_idx <- clusters == i
                n <- sum(cluster_idx)
                percentage <- 100 * n / length(clusters)

                # Calculate predominant pattern
                cluster_data <- private$.ihc_matrix[cluster_idx, , drop = FALSE]
                pattern <- private$.describePattern(cluster_data)

                self$results$clusterSummary$addRow(
                    rowKey = i,
                    values = list(
                        cluster = i,
                        n = n,
                        percentage = percentage,
                        pattern = pattern
                    )
                )
            }

            # Calculate marker expression by cluster
            private$.calculateMarkerSummary()
        },

        .describePattern = function(cluster_data) {
            # Describe the predominant expression pattern
            mean_expression <- colMeans(cluster_data, na.rm = TRUE)

            # Get configurable thresholds based on scoring scale
            thresholds <- private$.getClassificationThresholds()

            # Classify as high/medium/low
            classification <- ifelse(mean_expression > thresholds$high, "High",
                                    ifelse(mean_expression > thresholds$moderate, "Moderate", "Low"))

            # Create pattern description
            markers <- colnames(cluster_data)
            high_markers <- markers[classification == "High"]
            low_markers <- markers[classification == "Low"]

            if (length(high_markers) > 0) {
                pattern <- paste0(paste(high_markers, collapse = "+"), " high")
            } else if (length(low_markers) > 0) {
                pattern <- paste0(paste(low_markers, collapse = "+"), " low")
            } else {
                pattern <- "Mixed expression"
            }

            return(pattern)
        },

        .getClassificationThresholds = function() {
            # Return configurable thresholds based on scoring scale
            if (self$options$scoringScale == "binary") {
                return(list(high = 0.75, moderate = 0.25))
            } else if (self$options$scoringScale == "hscore") {
                return(list(high = 200, moderate = 100))
            } else {  # standard 0-3 scale
                return(list(high = 2, moderate = 1))
            }
        },

        .calculateMarkerSummary = function() {
            markers <- self$options$markers
            clusters <- private$.clusters
            n_clusters <- length(unique(clusters))

            # First, add dynamic cluster columns to the table
            for (i in 1:n_clusters) {
                col_name <- paste0("cluster", i)
                col_title <- paste("Cluster", i)

                # Add column if it doesn't exist
                existing_columns <- sapply(self$results$markerSummary$columns, function(x) x$name)
                if (!col_name %in% existing_columns) {
                    self$results$markerSummary$addColumn(
                        name = col_name,
                        title = col_title,
                        type = "text",
                        index = i + 1  # After marker column
                    )
                }
            }

            # Now populate the data
            for (marker in markers) {
                marker_data <- private$.ihc_matrix[, marker]
                row_values <- list(marker = marker)

                # Calculate mean expression per cluster
                for (i in 1:n_clusters) {
                    cluster_data <- marker_data[clusters == i]
                    mean_val <- mean(cluster_data, na.rm = TRUE)
                    sd_val <- sd(cluster_data, na.rm = TRUE)
                    row_values[[paste0("cluster", i)]] <- sprintf("%.2f ± %.2f", mean_val, sd_val)
                }

                # Kruskal-Wallis test for differences
                if (n_clusters > 1) {
                    kw_test <- kruskal.test(marker_data ~ clusters)
                    row_values$p_value <- kw_test$p.value
                    row_values$significance <- private$.getSignificance(kw_test$p.value)
                } else {
                    row_values$p_value <- NA
                    row_values$significance <- ""
                }

                self$results$markerSummary$addRow(
                    rowKey = marker,
                    values = row_values
                )
            }
        },

        .calculateHScores = function() {
            # Use utility function for basic conversion
            base_matrix <- convertIHCToNumeric(
                self$data,
                self$options$markers
            )

            # Convert to H-score scale manually
            hscore_matrix <- private$.convertToHScore(base_matrix)

            markers <- self$options$markers
            for (i in seq_along(markers)) {
                marker <- markers[i]
                hscore_values <- hscore_matrix[, i]

                mean_hscore <- mean(hscore_values, na.rm = TRUE)
                sd_hscore <- sd(hscore_values, na.rm = TRUE)
                median_hscore <- median(hscore_values, na.rm = TRUE)
                range_hscore <- sprintf("%.0f - %.0f",
                                       min(hscore_values, na.rm = TRUE),
                                       max(hscore_values, na.rm = TRUE))

                self$results$hscoreTable$addRow(
                    rowKey = marker,
                    values = list(
                        marker = marker,
                        mean = mean_hscore,
                        sd = sd_hscore,
                        median = median_hscore,
                        range = range_hscore
                    )
                )
            }
        },

        .performSilhouetteAnalysis = function() {
            if (length(unique(private$.clusters)) < 2) {
                return()
            }

            sil <- cluster::silhouette(private$.clusters, private$.dist_matrix)
            avg_width <- mean(sil[, 3])

            # Overall silhouette
            interpretation <- if (avg_width > 0.7) "Strong structure"
                            else if (avg_width > 0.5) "Reasonable structure"
                            else if (avg_width > 0.25) "Weak structure"
                            else "No substantial structure"

            self$results$silhouetteTable$addRow(
                rowKey = "overall",
                values = list(
                    cluster = "Overall",
                    avg_width = avg_width,
                    interpretation = interpretation
                )
            )

            # Per-cluster silhouette
            for (i in unique(private$.clusters)) {
                cluster_sil <- sil[private$.clusters == i, 3]
                cluster_avg <- mean(cluster_sil)

                self$results$silhouetteTable$addRow(
                    rowKey = paste0("cluster", i),
                    values = list(
                        cluster = paste("Cluster", i),
                        avg_width = cluster_avg,
                        interpretation = if (cluster_avg > 0.5) "Well-separated"
                                       else if (cluster_avg > 0.25) "Moderate"
                                       else "Overlapping"
                    )
                )
            }
        },

        .generateClinicalInterpretation = function() {
            n_samples <- nrow(self$data)
            n_markers <- length(self$options$markers)
            n_clusters <- length(unique(private$.clusters))

            # Generate cluster profiles for interpretation
            cluster_profiles <- private$.generateClusterProfiles()
            dominant_pattern <- cluster_profiles$dominant

            # Clinical summary with plain language
            clinical_html <- sprintf(
                "<div style='background-color: #f8f9fa; padding: 15px; border-left: 4px solid #007bff; margin-bottom: 15px;'>
                <h4 style='margin-top: 0; color: #007bff;'>🔬 Clinical Interpretation</h4>
                <p><strong>Key Finding:</strong> Analysis of %d patient samples identified <strong>%d distinct IHC expression patterns</strong> using %d biomarkers.</p>

                <p><strong>Dominant Pattern:</strong> Pattern %d represents the most common phenotype,
                observed in %d patients (%.1f%% of cohort). This pattern is characterized by <strong>%s</strong>.</p>

                <p><strong>Clinical Relevance:</strong> %s</p>

                <p><strong>Quality Assessment:</strong> %s</p>
                </div>",
                n_samples, n_clusters, n_markers,
                dominant_pattern$cluster_id, dominant_pattern$count, dominant_pattern$percentage,
                dominant_pattern$description,
                private$.getClinicalRelevance(cluster_profiles),
                private$.getQualityAssessment()
            )

            self$results$clinicalSummary$setContent(clinical_html)

            # Generate copy-ready report text
            report_html <- private$.generateReportText(cluster_profiles)
            self$results$reportText$setContent(report_html)
        },

        .generateClusterProfiles = function() {
            clusters <- private$.clusters
            n_clusters <- length(unique(clusters))

            profiles <- list()
            cluster_sizes <- table(clusters)
            dominant_idx <- which.max(cluster_sizes)

            # Analyze each cluster
            for (i in 1:n_clusters) {
                cluster_data <- private$.ihc_matrix[clusters == i, , drop = FALSE]
                mean_expr <- colMeans(cluster_data, na.rm = TRUE)

                # Get thresholds for interpretation
                thresholds <- private$.getClassificationThresholds()
                high_markers <- names(mean_expr)[mean_expr > thresholds$high]
                low_markers <- names(mean_expr)[mean_expr <= thresholds$moderate]

                # Generate description
                if (length(high_markers) > 0) {
                    description <- paste0(paste(high_markers, collapse = ", "), " high expression")
                } else if (length(low_markers) == length(mean_expr)) {
                    description <- "low expression across all markers"
                } else {
                    description <- "mixed expression pattern"
                }

                profiles[[i]] <- list(
                    cluster_id = i,
                    count = cluster_sizes[i],
                    percentage = 100 * cluster_sizes[i] / sum(cluster_sizes),
                    description = description,
                    high_markers = high_markers,
                    low_markers = low_markers
                )
            }

            return(list(
                profiles = profiles,
                dominant = profiles[[dominant_idx]]
            ))
        },

        .getClinicalRelevance = function(cluster_profiles) {
            n_patterns <- length(cluster_profiles$profiles)

            if (n_patterns == 2) {
                return("Two distinct expression patterns suggest potential for stratified treatment approaches.")
            } else if (n_patterns == 3) {
                return("Three expression patterns may correspond to different prognostic groups or treatment responses.")
            } else if (n_patterns >= 4) {
                return("Multiple expression patterns indicate significant tumor heterogeneity, which may impact treatment selection.")
            } else {
                return("Uniform expression pattern suggests homogeneous tumor characteristics.")
            }
        },

        .getQualityAssessment = function() {
            if (self$options$silhouetteAnalysis && !is.null(private$.clusters)) {
                sil <- cluster::silhouette(private$.clusters, private$.dist_matrix)
                avg_width <- mean(sil[, 3])

                if (avg_width > 0.5) {
                    return("Clustering quality is good - patterns are well-separated and reliable for clinical interpretation.")
                } else if (avg_width > 0.25) {
                    return("Clustering quality is moderate - patterns show some overlap, interpret with caution.")
                } else {
                    return("Clustering quality is poor - patterns are not well-defined, results may not be clinically meaningful.")
                }
            } else {
                return("Enable cluster quality assessment for reliability evaluation.")
            }
        },

        .generateReportText = function(cluster_profiles) {
            n_samples <- nrow(self$data)
            n_markers <- length(self$options$markers)
            n_clusters <- length(cluster_profiles$profiles)
            dominant <- cluster_profiles$dominant
            method <- self$options$clusterMethod

            # Generate formatted report text
            report_html <- sprintf(
                "<div style='background-color: #fff8dc; padding: 15px; border: 1px solid #ffd700; margin-bottom: 15px;'>
                <h4 style='margin-top: 0; color: #b8860b;'>📋 Copy-Ready Report Text</h4>

                <div style='background-color: white; padding: 10px; border: 1px dashed #ccc; font-family: monospace; font-size: 13px;'>
                <p><strong>IHC Expression Pattern Analysis:</strong></p>

                <p>Immunohistochemical analysis of %d tumor samples was performed using %d biomarkers (%s).
                %s clustering analysis identified %d distinct expression patterns.</p>

                <p>The predominant expression pattern (Pattern %d) was observed in %d/%d cases (%.1f%%),
                characterized by %s. %s</p>

                <p><em>Statistical Methods:</em> Clustering was performed using %s method with %s distance metric.
                %s</p>
                </div>

                <p><small><em>💡 Tip: Copy the text above for your pathology report or manuscript.</em></small></p>
                </div>",
                n_samples, n_markers, paste(self$options$markers, collapse = ", "),
                tools::toTitleCase(method), n_clusters,
                dominant$cluster_id, dominant$count, n_samples, dominant$percentage, dominant$description,
                private$.getClinicalRelevance(cluster_profiles),
                method, self$options$distanceMetric,
                if(self$options$silhouetteAnalysis) private$.getQualityAssessment() else "Cluster quality assessment was not performed."
            )

            return(report_html)
        },

        .updateInstructions = function() {
            n_samples <- nrow(self$data)
            n_markers <- length(self$options$markers)
            n_clusters <- length(unique(private$.clusters))
            method <- self$options$clusterMethod

            summary_html <- sprintf(
                "<h3>✅ Analysis Complete</h3>
                <p><b>Data Summary:</b></p>
                <ul>
                    <li>Samples analyzed: %d</li>
                    <li>IHC markers: %d</li>
                    <li>Expression patterns identified: %d</li>
                    <li>Method: %s clustering</li>
                </ul>
                <p><strong>📊 Review Results:</strong> Check the Clinical Interpretation and Copy-Ready Report sections above,
                then examine the detailed tables and visualizations below.</p>",
                n_samples, n_markers, n_clusters, tools::toTitleCase(method)
            )

            self$results$instructions$setContent(summary_html)
        },

        .plotDendrogram = function(image, ...) {
            if (is.null(private$.hc) || self$options$clusterMethod != "hierarchical") {
                return()
            }

            # Use base R plot for dendrogram (most appropriate for tree structures)
            plot(private$.hc,
                 main = "IHC Expression Pattern Clustering",
                 xlab = "Samples",
                 ylab = "Distance",
                 sub = sprintf("Method: %s linkage, %s distance",
                              self$options$linkageMethod,
                              self$options$distanceMetric),
                 cex.main = 1.2,
                 cex.lab = 1.1)

            # Add cluster rectangles with colorblind-safe colors
            cluster_colors <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728",
                               "#9467bd", "#8c564b", "#e377c2", "#7f7f7f",
                               "#bcbd22", "#17becf")
            rect.hclust(private$.hc, k = self$options$nClusters,
                       border = cluster_colors[1:self$options$nClusters])

            TRUE
        },

        .plotHeatmap = function(image, ...) {
            if (is.null(private$.ihc_matrix) || is.null(private$.clusters)) {
                return()
            }

            # Order samples by cluster
            order_idx <- order(private$.clusters)
            heatmap_data <- private$.ihc_matrix[order_idx, ]
            clusters_ordered <- private$.clusters[order_idx]

            # Create colorblind-safe expression palette (blue-white-red)
            colors <- grDevices::colorRampPalette(c("#2166ac", "white", "#762a83"))(100)

            # Create colorblind-safe cluster colors
            cluster_colors <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728",
                               "#9467bd", "#8c564b", "#e377c2", "#7f7f7f",
                               "#bcbd22", "#17becf")
            cluster_side_colors <- cluster_colors[clusters_ordered]

            # Create heatmap with clinical-friendly labeling
            heatmap(heatmap_data,
                   Rowv = NA,
                   Colv = NA,
                   scale = "column",
                   col = colors,
                   main = "IHC Expression Patterns by Sample",
                   xlab = "IHC Markers",
                   ylab = "Patient Samples (grouped by expression pattern)",
                   RowSideColors = cluster_side_colors,
                   cex.main = 1.2,
                   cex.lab = 1.1)

            # Add legend for clusters
            n_clusters <- length(unique(clusters_ordered))
            legend("topright",
                   legend = paste("Pattern", 1:n_clusters),
                   fill = cluster_colors[1:n_clusters],
                   cex = 0.8,
                   title = "Expression Patterns")

            TRUE
        },

        .getSignificance = function(p) {
            if (is.na(p)) return("")
            if (p < 0.001) return("***")
            if (p < 0.01) return("**")
            if (p < 0.05) return("*")
            return("ns")
        }
    )
)