#' @title Patient Similarity Clustering - Discover Patient Subgroups
#' @description
#' Visualizes patient similarity using dimensionality reduction techniques (PCA, t-SNE, UMAP, MDS).
#' Projects high-dimensional patient data into 2D or 3D space to reveal natural patient groupings.
#' Inspired by Orange Data Mining's interactive projection widgets, adapted for jamovi with comprehensive
#' cluster analysis and statistical validation.
#'
#' @details
#' This analysis performs dimensionality reduction to visualize patient similarity:
#' \itemize{
#'   \item **PCA**: Linear method preserving global variance structure
#'   \item **t-SNE**: Non-linear method excellent for visualization, preserves local neighborhoods
#'   \item **UMAP**: Non-linear method preserving both local and global structure, faster than t-SNE
#'   \item **MDS**: Classical scaling method preserving pairwise distances
#' }
#'
#' Optional cluster analysis identifies patient subgroups using k-means, hierarchical clustering, or DBSCAN.
#' Survival analysis can compare outcomes across discovered clusters.
#'
#' @section Use Cases:
#' \itemize{
#'   \item Discover unexpected patient subtypes based on clinicopathological features
#'   \item Identify which variables drive patient groupings
#'   \item Validate if known outcomes correspond to natural patient clusters
#'   \item Find prognostic patient subgroups for stratified treatment
#' }
#'
#' @examples
#' \dontrun{
#' # Discover patient subgroups
#' patientsimilarity(
#'   data = clinical_data,
#'   vars = c("age", "tumor_size", "grade", "ki67"),
#'   method = "tsne",
#'   colorBy = "survival_status",
#'   performClustering = TRUE
#' )
#' }
#'
#' @references
#' Orange Data Mining: https://orangedatamining.com/widget-catalog/unsupervised/
#'
#' @author ClinicoPath Development Team
#' @importFrom R6 R6Class
#' @import jmvcore

patientsimilarityClass <- if (requireNamespace('jmvcore', quietly = TRUE)) R6::R6Class(
    "patientsimilarityClass",
    inherit = patientsimilarityBase,
    private = list(

        # Store projection results
        .projectionData = NULL,

        .escapeVar = function(x) {
            # Safely escape variable names for data.frame access
            if (is.null(x) || length(x) == 0) return(NULL)
            gsub("[^A-Za-z0-9_]+", "_", make.names(x))
        },

        # Initialize ----
        .init = function() {

            if (!is.null(self$options$vars) && length(self$options$vars) > 0) {
                html <- "<p><b>Patient Similarity Clustering</b></p>
                        <p>This analysis projects high-dimensional patient data into 2D/3D space
                        to reveal natural patient groupings.</p>
                        <ul>
                        <li><b>PCA:</b> Linear method, good for understanding overall variance</li>
                        <li><b>t-SNE:</b> Non-linear, excellent visualization, preserves local structure</li>
                        <li><b>UMAP:</b> Non-linear, preserves local and global structure, faster</li>
                        <li><b>MDS:</b> Classical scaling, preserves pairwise distances</li>
                        </ul>"
                self$results$instructions$setContent(html)
            }
        },

        # Main analysis ----
        .run = function() {

            # Check for required inputs
            if (is.null(self$options$vars) || length(self$options$vars) == 0) {
                notice <- jmvcore::Notice$new(
                    options = self$options,
                    name = 'missingVariables',
                    type = jmvcore::NoticeType$ERROR
                )
                notice$setContent('Please select at least 2 variables for similarity analysis.')
                self$results$insert(999, notice)
                return()
            }

            # Set seed for reproducibility
            if (!is.null(self$options$seed)) {
                message(paste("Setting seed to:", self$options$seed))
                set.seed(self$options$seed)
            }

            # Prepare data
            prep_data <- private$.prepareData()
            if (is.null(prep_data)) return()

            # Perform dimensionality reduction
            projection <- private$.performProjection(prep_data$scaled_data)
            if (is.null(projection)) return()

            # Store for plotting
            private$.projectionData <- projection

            # Generate summary
            private$.generateSummary(projection, prep_data)

            # Clustering if requested
            if (self$options$performClustering) {
                projection$clusters <- private$.performClustering(projection, prep_data$original_data)
            }

            # Survival analysis if requested
            if (self$options$survivalAnalysis &&
                !is.null(self$options$survivalTime) &&
                !is.null(self$options$survivalEvent)) {
                private$.performSurvivalAnalysis(projection, prep_data$original_data)
            }

            # Export if requested
            if (self$options$exportCoordinates &&
                self$results$exportCoordinates$isNotFilled()) {
                private$.exportCoordinates(projection)
            }

            # Interpretation
            private$.generateInterpretation(projection)
        },

        # Data preparation ----
        .prepareData = function() {

            tryCatch({

                # Get variables
                data <- self$data[, self$options$vars, drop = FALSE]

                # Convert to numeric if needed
                data <- as.data.frame(lapply(data, jmvcore::toNumeric))

                # Remove rows with missing values
                complete_idx <- complete.cases(data)
                data <- data[complete_idx, ]

                # Check sample size
                if (nrow(data) < 5) {
                    notice <- jmvcore::Notice$new(
                        options = self$options,
                        name = 'insufficientData',
                        type = jmvcore::NoticeType$ERROR
                    )
                    notice$setContent(sprintf('Insufficient data for analysis (n=%d after removing missing values). At least 5 complete observations required.', nrow(data)))
                    self$results$insert(999, notice)
                    return(NULL)
                } else if (nrow(data) < 10) {
                    notice <- jmvcore::Notice$new(
                        options = self$options,
                        name = 'verySmallSample',
                        type = jmvcore::NoticeType$STRONG_WARNING
                    )
                    notice$setContent(sprintf('Sample size is very small (n=%d). Results may be unreliable. Consider collecting more data for robust analysis.', nrow(data)))
                    self$results$insert(6, notice)
                } else if (nrow(data) < 30) {
                    notice <- jmvcore::Notice$new(
                        options = self$options,
                        name = 'smallSample',
                        type = jmvcore::NoticeType$WARNING
                    )
                    notice$setContent(sprintf('Sample size is small (n=%d). Results should be interpreted with caution.', nrow(data)))
                    self$results$insert(21, notice)
                }

                # Remove outliers if requested
                if (self$options$removeOutliers) {
                    data <- private$.removeOutliers(data)
                }

                # Scale if requested
                if (self$options$scaleVars) {
                    scaled_data <- scale(data)
                } else {
                    scaled_data <- as.matrix(data)
                }

                # Store original data with complete rows
                original_data <- self$data[complete_idx, ]

                # Debug
                # message(paste("PrepareData: Original data info"))
                # message(colnames(original_data))
                # if ("time" %in% names(original_data)) message(paste("NAs in time:", sum(is.na(original_data$time))))

                list(
                    scaled_data = scaled_data,
                    original_data = original_data,
                    complete_idx = complete_idx
                )

            }, error = function(e) {
                notice <- jmvcore::Notice$new(
                    options = self$options,
                    name = 'dataPreparationError',
                    type = jmvcore::NoticeType$ERROR
                )
                notice$setContent(paste('Error preparing data:', e$message, 'Please check that all selected variables contain valid numeric values.'))
                self$results$insert(999, notice)
                return(NULL)
            })
        },

        .removeOutliers = function(data) {
            # Simple IQR-based outlier removal
            for (col in names(data)) {
                Q1 <- quantile(data[[col]], 0.25, na.rm = TRUE)
                Q3 <- quantile(data[[col]], 0.75, na.rm = TRUE)
                IQR_val <- Q3 - Q1
                lower <- Q1 - 1.5 * IQR_val
                upper <- Q3 + 1.5 * IQR_val
                data[[col]][data[[col]] < lower | data[[col]] > upper] <- NA
            }
            data[complete.cases(data), ]
        },

        # Projection methods ----
        .performProjection = function(scaled_data) {

            method <- self$options$method
            n_dims <- as.numeric(self$options$dimensions)

            tryCatch({

                if (method == "pca") {
                    result <- private$.runPCA(scaled_data, n_dims)
                } else if (method == "tsne") {
                    result <- private$.runTSNE(scaled_data, n_dims)
                } else if (method == "umap") {
                    result <- private$.runUMAP(scaled_data, n_dims)
                } else if (method == "mds") {
                    result <- private$.runMDS(scaled_data, n_dims)
                }

                result

            }, error = function(e) {
                notice <- jmvcore::Notice$new(
                    options = self$options,
                    name = 'projectionError',
                    type = jmvcore::NoticeType$ERROR
                )
                notice$setContent(paste('Error in projection:', e$message, 'This may be due to insufficient data or numerical issues. Try a different method or check your input data.'))
                self$results$insert(999, notice)
                return(NULL)
            })
        },

        .runPCA = function(data, n_dims) {
            pca_result <- prcomp(data, center = FALSE, scale. = FALSE)

            # Extract coordinates
            coords <- pca_result$x[, 1:n_dims, drop = FALSE]

            # Variance explained
            variance <- summary(pca_result)$importance[2, 1:n_dims]
            cumulative <- summary(pca_result)$importance[3, 1:n_dims]

            # Populate variance table
            var_table <- self$results$varianceTable
            for (i in 1:n_dims) {
                var_table$addRow(rowKey = i, values = list(
                    component = paste0("PC", i),
                    variance = variance[i],
                    cumulative = cumulative[i]
                ))
            }

            # Loadings if requested
            if (self$options$showLoadings) {
                private$.populateLoadings(pca_result$rotation[, 1:n_dims, drop = FALSE])
            }

            list(
                coords = coords,
                method = "PCA",
                obj = pca_result
            )
        },

        .runTSNE = function(data, n_dims) {
            if (!requireNamespace("Rtsne", quietly = TRUE)) {
                notice <- jmvcore::Notice$new(
                    options = self$options,
                    name = 'missingRtsne',
                    type = jmvcore::NoticeType$ERROR
                )
                notice$setContent('Package "Rtsne" is required for t-SNE analysis. Install with: install.packages("Rtsne")')
                self$results$insert(999, notice)
                return(NULL)
            }

            # Perplexity validation
            if (nrow(data) < 3 * self$options$perplexity) {
                new_perp <- floor((nrow(data) - 1) / 3)
                if (new_perp < 1) new_perp <- 1

                notice <- jmvcore::Notice$new(
                    options = self$options,
                    name = 'perplexityAdjusted',
                    type = jmvcore::NoticeType$WARNING
                )
                notice$setContent(sprintf('t-SNE Perplexity (%d) is too high for sample size (n=%d). Automatically adjusted to %d. For optimal results, use perplexity < n/3.',
                    self$options$perplexity, nrow(data), new_perp))
                self$results$insert(21, notice)

                perplexity <- new_perp
            } else {
                perplexity <- self$options$perplexity
            }

            tsne_result <- Rtsne::Rtsne(
                data,
                dims = n_dims,
                perplexity = perplexity,
                max_iter = self$options$iterations,
                check_duplicates = FALSE
            )

            list(
                coords = tsne_result$Y,
                method = "t-SNE",
                obj = tsne_result
            )
        },

        .runUMAP = function(data, n_dims) {
            if (!requireNamespace("umap", quietly = TRUE)) {
                notice <- jmvcore::Notice$new(
                    options = self$options,
                    name = 'missingUmap',
                    type = jmvcore::NoticeType$ERROR
                )
                notice$setContent('Package "umap" is required for UMAP analysis. Install with: install.packages("umap")')
                self$results$insert(999, notice)
                return(NULL)
            }

            umap_config <- umap::umap.defaults
            umap_config$n_components <- n_dims
            umap_config$n_neighbors <- self$options$umapNeighbors
            umap_config$min_dist <- self$options$umapMinDist
            if (!is.null(self$options$seed)) {
                umap_config$random_state <- self$options$seed
            }

            umap_result <- umap::umap(data, config = umap_config)

            list(
                coords = umap_result$layout,
                method = "UMAP",
                obj = umap_result
            )
        },

        .runMDS = function(data, n_dims) {
            dist_matrix <- dist(data)
            mds_result <- cmdscale(dist_matrix, k = n_dims, eig = TRUE)

            # Loadings for MDS
            if (self$options$showLoadings) {
                # Approximate loadings from correlation with original variables
                loadings <- cor(data, mds_result$points)
                private$.populateLoadings(loadings)
            }

            list(
                coords = mds_result$points,
                method = "MDS",
                obj = mds_result
            )
        },

        .populateLoadings = function(loadings) {
            load_table <- self$results$loadingsTable

            for (i in seq_len(nrow(loadings))) {
                values <- list(
                    variable = rownames(loadings)[i],
                    dim1 = loadings[i, 1],
                    dim2 = loadings[i, 2]
                )

                if (ncol(loadings) >= 3) {
                    values$dim3 <- loadings[i, 3]
                }

                load_table$addRow(rowKey = i, values = values)
            }
        },

        # Clustering ----
        .performClustering = function(projection, original_data) {

            coords <- projection$coords
            method <- self$options$clusterMethod
            clusters <- NULL # Initialize clusters to NULL

            tryCatch({

                if (method == "kmeans") {
                    clusters <- kmeans(coords, centers = self$options$nClusters)$cluster
                } else if (method == "hclust") {
                    hc <- hclust(dist(coords))
                    clusters <- cutree(hc, k = self$options$nClusters)
                } else if (method == "dbscan") {
                    if (!requireNamespace("dbscan", quietly = TRUE)) {
                        notice <- jmvcore::Notice$new(
                            options = self$options,
                            name = 'missingDbscan',
                            type = jmvcore::NoticeType$WARNING
                        )
                        notice$setContent('Package "dbscan" not available. Using k-means clustering instead. Install dbscan with: install.packages("dbscan")')
                        self$results$insert(21, notice)
                        clusters <- kmeans(coords, centers = self$options$nClusters)$cluster
                    } else {
                        clusters <- dbscan::dbscan(coords, eps = self$options$dbscan_eps, minPts = self$options$dbscan_minpts)$cluster
                        # Check if only noise/one cluster found
                        n_found <- length(unique(clusters[clusters != 0]))
                        if (n_found < 2) {
                            notice <- jmvcore::Notice$new(
                                options = self$options,
                                name = 'dbscanFewClusters',
                                type = jmvcore::NoticeType$WARNING
                            )
                            notice$setContent(sprintf('DBSCAN found only %d cluster(s). Try adjusting epsilon (eps) or minimum points (minPts) parameters to identify more clusters.', n_found))
                            self$results$insert(21, notice)
                        }
                    }
                }

                # Add to projection data (this is now done in .run)
                # projection$clusters <- clusters

                # Populate cluster heading
                self$results$clusterHeading$setContent(
                    "<h3>Cluster Analysis Results</h3><p>Automatic clustering identified distinct patient subgroups based on projection coordinates.</p>"
                )

                # Summary table
                cluster_counts <- table(clusters)
                cluster_summary <- self$results$clusterSummary

                for (cl in sort(unique(clusters))) {
                    cluster_summary$addRow(rowKey = cl, values = list(
                        cluster = paste0("Cluster ", cl),
                        n = sum(clusters == cl),
                        percentage = sum(clusters == cl) / length(clusters)
                    ))
                }

                # Characteristics if requested
                if (self$options$showClusterStats) {
                    private$.clusterCharacteristics(clusters, original_data)
                }

                # Outcome distribution if colorBy is set
                if (!is.null(self$options$colorBy)) {
                    private$.clusterOutcomes(clusters, original_data)
                }

                # Quality metrics
                if (method %in% c("kmeans", "hclust")) {
                    # Silhouette score
                    if (requireNamespace("cluster", quietly = TRUE)) {
                        sil <- cluster::silhouette(clusters, dist(coords))
                        sil_score <- mean(sil[, 3])

                        self$results$clusterQuality$addRow(rowKey = 1, values = list(
                            metric = "Silhouette Score",
                            value = sil_score
                        ))
                    }
                }

                # Export if requested
                if (self$options$exportClusters &&
                    self$results$exportClusters$isNotFilled()) {
                    row_nums <- seq_len(self$data$rowCount)
                    cluster_export <- rep(NA, length(row_nums))
                    # Use the correct complete_idx from prep_data (stored in private) or recalculate
                    # We need to access the complete_idx used for the protection.
                    # It was returned by .prepareData. We should pass it or store it.
                    # Or just recalculate here for safety as shown in existing code but using consistent logic.
                    complete_idx <- complete.cases(self$data[, self$options$vars, drop = FALSE])
                    
                    # Ensure clusters length matches complete cases
                    if(length(clusters) == sum(complete_idx)) {
                        cluster_export[complete_idx] <- clusters
                        self$results$exportClusters$setRowNums(row_nums)
                        self$results$exportClusters$setValues(cluster_export)
                    }
                }

            }, error = function(e) {
                notice <- jmvcore::Notice$new(
                    options = self$options,
                    name = 'clusteringError',
                    type = jmvcore::NoticeType$WARNING
                )
                notice$setContent(paste('Error in clustering:', e$message, 'Clustering skipped. Try different parameters or method.'))
                self$results$insert(21, notice)
            })

            return(clusters)
        },

        .clusterCharacteristics = function(clusters, original_data) {
            char_table <- self$results$clusterCharacteristics

            # Get mean values for each cluster
            for (var in self$options$vars) {
                var_data <- jmvcore::toNumeric(original_data[[var]])

                values <- list(variable = var)

                for (cl in sort(unique(clusters))) {
                    cluster_mean <- mean(var_data[clusters == cl], na.rm = TRUE)
                    values[[paste0("cluster", cl)]] <- sprintf("%.2f", cluster_mean)
                }

                # Dynamically add columns if needed
                for (cl in sort(unique(clusters))) {
                    col_name <- paste0("cluster", cl)
                    if (!col_name %in% names(char_table$columns)) {
                        char_table$addColumn(
                            name = col_name,
                            title = paste0("Cluster ", cl),
                            type = "text"
                        )
                    }
                }

                char_table$addRow(rowKey = var, values = values)
            }
        },


        .clusterOutcomes = function(clusters, original_data) {
            outcome_table <- self$results$clusterOutcomes
            outcome_var <- original_data[[self$options$colorBy]]

            for (cl in sort(unique(clusters))) {
                cluster_outcomes <- outcome_var[clusters == cl]

                if (is.factor(outcome_var) || is.character(outcome_var)) {
                    outcome_counts <- table(cluster_outcomes)
                    summary_text <- paste(names(outcome_counts), ": ", outcome_counts,
                                        " (", round(100*outcome_counts/sum(outcome_counts), 1), "%)",
                                        sep = "", collapse = "; ")
                } else {
                    summary_text <- sprintf("Mean: %.2f (SD: %.2f)",
                                          mean(cluster_outcomes, na.rm = TRUE),
                                          sd(cluster_outcomes, na.rm = TRUE))
                }

                outcome_table$addRow(rowKey = cl, values = list(
                    cluster = paste0("Cluster ", cl),
                    outcome_summary = summary_text
                ))
            }
        },

        # Survival analysis ----
        .performSurvivalAnalysis = function(projection, original_data) {

            if (is.null(projection$clusters)) {
                return()
            }

            if (is.null(self$options$survivalTime) || is.null(self$options$survivalEvent)) {
                notice <- jmvcore::Notice$new(
                    options = self$options,
                    name = 'missingSurvivalVariables',
                    type = jmvcore::NoticeType$ERROR
                )
                notice$setContent('Survival analysis requires both time and event variables. Please select survival time and event in the Survival Analysis panel.')
                self$results$insert(999, notice)
                return()
            }

            tryCatch({

                survtime <- jmvcore::toNumeric(original_data[[self$options$survivalTime]])
                event_var <- original_data[[self$options$survivalEvent]]
                event_level <- self$options$survivalEventLevel

                if (is.factor(event_var)) {
                    event <- as.numeric(event_var == event_level)
                } else {
                    event <- jmvcore::toNumeric(event_var)
                }

                # Check for missing survival data
                if (any(is.na(survtime)) || any(is.na(event))) {
                    notice <- jmvcore::Notice$new(
                        options = self$options,
                        name = 'survivalMissingValues',
                        type = jmvcore::NoticeType$WARNING
                    )
                    n_missing <- sum(is.na(survtime) | is.na(event))
                    notice$setContent(sprintf('Survival analysis: %d observation(s) with missing time or event values were excluded from the analysis.', n_missing))
                    self$results$insert(21, notice)
                }

                clusters <- projection$clusters

                # Populate survival heading
                self$results$survivalHeading$setContent(
                    "<h3>Survival Analysis by Cluster</h3><p>Comparing survival outcomes across discovered patient subgroups.</p>"
                )

                # Survival summary
                surv_table <- self$results$survivalTable

                for (cl in sort(unique(clusters))) {
                    cluster_surv <- survtime[clusters == cl]
                    cluster_event <- event[clusters == cl]

                    surv_table$addRow(rowKey = cl, values = list(
                        cluster = paste0("Cluster ", cl),
                        n = length(cluster_surv),
                        events = sum(cluster_event, na.rm = TRUE),
                        median_survival = median(cluster_surv[cluster_event == 1], na.rm = TRUE)
                    ))
                }

                # Log-rank test
                surv_obj <- survival::Surv(survtime, event)
                logrank <- survival::survdiff(surv_obj ~ clusters)

                self$results$survivalComparison$addRow(rowKey = 1, values = list(
                    chisq = logrank$chisq,
                    df = length(logrank$n) - 1,
                    pvalue = 1 - pchisq(logrank$chisq, length(logrank$n) - 1)
                ))

            }, error = function(e) {
                message(paste("Error in survival analysis:", e$message))
            })
        },

        # Plotting ----
        .projectionPlot = function(image, ggtheme, theme, ...) {

            if (is.null(private$.projectionData)) {
                return()
            }

            library(ggplot2)

            projection <- private$.projectionData
            coords <- as.data.frame(projection$coords)

            n_dims <- ncol(coords)

            if (n_dims == 2) {
                colnames(coords) <- c("Dim1", "Dim2")
            } else {
                colnames(coords) <- c("Dim1", "Dim2", "Dim3")
            }

            # Add color variable if specified
            if (!is.null(self$options$colorBy)) {
                # Get complete cases
                complete_idx <- complete.cases(self$data[, self$options$vars, drop = FALSE])
                color_var <- self$data[[self$options$colorBy]][complete_idx]
                coords$Color <- color_var
            }

            # Add clusters if available
            if (!is.null(projection$clusters)) {
                coords$Cluster <- factor(projection$clusters)
            }

            # Create plot
            if (n_dims == 2) {
                if (!is.null(self$options$colorBy)) {
                    p <- ggplot(coords, aes(x = Dim1, y = Dim2, color = Color)) +
                        geom_point(size = 2, alpha = 0.6)
                } else if (!is.null(projection$clusters)) {
                    p <- ggplot(coords, aes(x = Dim1, y = Dim2, color = Cluster)) +
                        geom_point(size = 2, alpha = 0.6)
                } else {
                    p <- ggplot(coords, aes(x = Dim1, y = Dim2)) +
                        geom_point(size = 2, alpha = 0.6)
                }

                p <- p +
                    labs(
                        title = paste0("Patient Similarity - ", projection$method),
                        x = "Dimension 1",
                        y = "Dimension 2"
                    ) +
                    theme_minimal() +
                    theme(plot.title = element_text(hjust = 0.5, face = "bold"))

                print(p)
                TRUE
            }
        },

        .projection3D = function(image, ggtheme, theme, ...) {

            if (is.null(private$.projectionData)) {
                return(FALSE)
            }

            if (as.numeric(self$options$dimensions) != 3) {
                return(FALSE)
            }

            if (!requireNamespace("plotly", quietly = TRUE)) {
                message("Package 'plotly' required for 3D plots")
                return(FALSE)
            }

            projection <- private$.projectionData
            coords <- as.data.frame(projection$coords)
            colnames(coords) <- c("Dim1", "Dim2", "Dim3")

            # Add color
            if (!is.null(self$options$colorBy)) {
                complete_idx <- complete.cases(self$data[, self$options$vars, drop = FALSE])
                coords$Color <- self$data[[self$options$colorBy]][complete_idx]
            }

            # Create 3D plot
            p <- plotly::plot_ly(coords, x = ~Dim1, y = ~Dim2, z = ~Dim3,
                               color = if (!is.null(self$options$colorBy)) ~Color else NULL,
                               type = "scatter3d", mode = "markers")

            p <- plotly::layout(p, title = paste0("3D Projection - ", projection$method))

            print(p)
            TRUE
        },

        .survivalPlot = function(image, ggtheme, theme, ...) {

            if (!self$options$survivalAnalysis) {
                return(FALSE)
            }

            if (is.null(private$.projectionData$clusters)) {
                return(FALSE)
            }

            library(survival)
            library(survminer)

            # Get data
            complete_idx <- complete.cases(self$data[, self$options$vars, drop = FALSE])
            original_data <- self$data[complete_idx, ]

            survtime <- jmvcore::toNumeric(original_data[[self$options$survivalTime]])
            event_var <- original_data[[self$options$survivalEvent]]
            event_level <- self$options$survivalEventLevel

            if (is.factor(event_var)) {
                event <- as.numeric(event_var == event_level)
            } else {
                event <- jmvcore::toNumeric(event_var)
            }

            clusters <- private$.projectionData$clusters

            # Create survival object
            surv_formula <- as.formula("survival::Surv(survtime, event) ~ clusters")
            fit <- survival::survfit(surv_formula)

            # Plot
            plot <- survminer::ggsurvplot(
                fit,
                data = data.frame(survtime, event, clusters),
                pval = TRUE,
                risk.table = TRUE,
                title = "Survival by Discovered Cluster",
                ggtheme = theme_minimal()
            )

            print(plot)
            TRUE
        },

        # Summary and interpretation ----
        .generateSummary = function(projection, prep_data) {

            n_samples <- nrow(projection$coords)
            n_vars <- length(self$options$vars)
            method <- projection$method

            summary_text <- sprintf(
                paste0("%s Projection Summary\n\n",
                "Method: %s\n",
                "Samples analyzed: %d\n",
                "Variables used: %d\n",
                "Dimensions: %s\n"),
                method, method, n_samples, n_vars, self$options$dimensions
            )

            if (method == "PCA") {
                variance <- summary(projection$obj)$importance[3, as.numeric(self$options$dimensions)]
                summary_text <- paste0(summary_text,
                    sprintf("Variance explained: %.1f%%\n", variance * 100))
            }

            if (self$options$scaleVars) {
                summary_text <- paste0(summary_text, "\nVariables were standardized before analysis.\n")
            }

            self$results$summaryText$setContent(summary_text)
        },

        .generateInterpretation = function(projection) {

            html <- "<h3>Interpretation Guide</h3>"

            html <- paste0(html,
                "<p><b>What does this plot show?</b><br>",
                "Each point represents a patient. Patients close together have similar ",
                "characteristics based on the variables you selected.</p>")

            if (!is.null(self$options$colorBy)) {
                html <- paste0(html,
                    "<p><b>Color coding:</b> Points are colored by the outcome variable. ",
                    "If colors cluster together, the outcome corresponds to natural patient groupings.</p>")
            }

            if (self$options$performClustering) {
                html <- paste0(html,
                    "<p><b>Cluster Analysis:</b> Automatic clustering identified distinct patient subgroups. ",
                    "Review cluster characteristics to understand what defines each group.</p>")
            }

            html <- paste0(html,
                "<p><b>Method-specific notes:</b></p><ul>")

            if (self$options$method == "tsne") {
                html <- paste0(html,
                    "<li><b>t-SNE:</b> Distances between clusters are NOT meaningful. ",
                    "Focus on separation between groups, not their relative positions.</li>")
            } else if (self$options$method == "umap") {
                html <- paste0(html,
                    "<li><b>UMAP:</b> Better preserves global structure than t-SNE. ",
                    "Both cluster separation and relative positions are meaningful.</li>")
            } else if (self$options$method == "pca") {
                html <- paste0(html,
                    "<li><b>PCA:</b> Linear method. If you see clear separation, ",
                    "it means variables differ systematically between groups.</li>")
            }

            html <- paste0(html, "</ul>")

            html <- paste0(html,
                "<p><i>This analysis was inspired by Orange Data Mining's projection widgets, ",
                "adapted for jamovi with clustering and survival analysis integration.</i></p>")

            self$results$interpretation$setContent(html)
        },

        .exportCoordinates = function(projection) {
            coords <- projection$coords
            n_dims <- ncol(coords)

            # Create export data for all rows
            row_nums <- seq_len(self$data$rowCount)
            complete_idx <- complete.cases(self$data[, self$options$vars, drop = FALSE])

            # Export first dimension only (jamovi Output supports single column)
            # For multi-column export, use Output objects for each dimension separately in .a.yaml/.r.yaml
            coord_export <- rep(NA, length(row_nums))
            coord_export[complete_idx] <- coords[, 1]  # Dim 1 only

            self$results$exportCoordinates$setRowNums(row_nums)
            self$results$exportCoordinates$setValues(coord_export)

            # Inform user if >1 dimension
            if (n_dims > 1) {
                notice <- jmvcore::Notice$new(
                    options = self$options,
                    name = 'coordinatesExportLimited',
                    type = jmvcore::NoticeType$INFO
                )
                notice$setContent(sprintf('Note: Only Dimension 1 exported to dataset. Total dimensions available: %d. For all dimensions, use external tools or save the full projection results.', n_dims))
                self$results$insert(999, notice)
            }
        }
    )
)
