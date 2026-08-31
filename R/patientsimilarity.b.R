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
#' @return An \code{R6} class generator object for the \code{patientsimilarityClass} backend; used internally by the jamovi analysis wrapper and not called directly.

patientsimilarityClass <- if (requireNamespace('jmvcore', quietly = TRUE)) R6::R6Class(
    "patientsimilarityClass",
    inherit = patientsimilarityBase,
    private = list(

        # Store projection results
        .projectionData = NULL,

        # Row indices of self$data that survived complete-case filtering AND outlier
        # removal. Every downstream consumer (plots, exports, cluster tables) reads this
        # one index; recomputing complete.cases() locally silently misaligns rows against
        # the projection whenever removeOutliers dropped further rows.
        .keepIdx = NULL,

        # Survival frame assembled once in .performSurvivalAnalysis and reused by
        # .survivalPlot - renderers must not re-derive it from self$data.
        .survData = NULL,

        # Notice collection helpers. A single Preformatted (plain-text) output item:
        # avoids BOTH the jmvcore::Notice serialization error from
        # self$results$insert(999, Notice) AND any HTML in notices (project convention:
        # notice content must be plain text). ====
        .noticeList = list(),

        .addNotice = function(type, title, content) {
            private$.noticeList[[length(private$.noticeList) + 1]] <- list(
                type = type,
                title = title,
                content = content
            )
            # Render immediately so early-return validation aborts still display the notice
            private$.renderNotices()
        },

        .renderNotices = function() {
            if (length(private$.noticeList) == 0) {
                self$results$notices$setContent("")
                return()
            }

            # Plain text only - notices avoid HTML by project convention; the Preformatted
            # output item renders this literally (no markup, no injection surface).
            blocks <- vapply(private$.noticeList, function(notice) {
                prefix <- switch(notice$type,
                    ERROR          = "ERROR: ",
                    STRONG_WARNING = "WARNING: ",
                    WARNING        = "WARNING: ",
                    "")
                paste0(prefix, notice$title, "\n", notice$content)
            }, character(1))

            self$results$notices$setContent(paste(blocks, collapse = "\n\n"))
        },

        # Initialize ----
        .init = function() {

            # The element is unconditionally visible (no `visible:` in .r.yaml): a
            # leading-`!` visible expression such as `(!vars)` is SILENTLY always-visible
            # (jmvcore's Options$eval routing regex is "^\\([\\$A-Za-z].*\\)$", so `(!vars)`
            # never matches, is returned as a raw string, and length(string) > 0 == TRUE).
            # So the backend picks the content instead: welcome block before any variable
            # is chosen, method guide afterwards.
            if (is.null(self$options$vars) || length(self$options$vars) == 0) {
                html <- "<div style='background-color: rgba(33, 149, 188, 0.1); color: inherit; padding: 12px; border-radius: 5px;'>
                        <p><b>Welcome to Patient Similarity Clustering</b></p>
                        <p>This analysis projects high-dimensional patient data into 2D/3D space
                        to reveal natural patient groupings.</p>
                        <p><b>To get started:</b></p>
                        <ol>
                        <li>Assign at least <b>2 continuous or ordinal variables</b> to
                        <b>Variables for Similarity Analysis</b> (e.g. age, tumor size, grade, Ki-67).</li>
                        <li>Pick a <b>Dimensionality Reduction Method</b> (PCA, t-SNE, UMAP or MDS).</li>
                        <li>Optionally set <b>Color Points By</b> to a grouping variable to see whether a
                        known label lines up with the discovered structure.</li>
                        <li>Optionally tick <b>Perform Cluster Analysis</b>, and
                        <b>Compare Survival Across Clusters</b> if you have follow-up time and an event variable.</li>
                        </ol>
                        <p>Missing values are handled by complete-case filtering, so variables with heavy
                        missingness will shrink the analysed sample.</p>
                        </div>"
                self$results$instructions$setContent(html)
            } else {
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

            # Row sets of varianceTable and loadingsTable are fixed by the options alone,
            # so create them here and fill them with setRow() in .run(): the tables render
            # at their final size instead of appearing empty and restructuring every run.
            # The guard mirrors .run()'s validation exactly - an invalid option combination
            # produces no projection, hence no rows.
            vars <- self$options$vars
            n_dims <- as.numeric(self$options$dimensions)

            if (length(vars) >= 2 && length(vars) >= n_dims) {

                if (self$options$method == "pca") {
                    for (i in seq_len(n_dims))
                        self$results$varianceTable$addRow(
                            rowKey = i, values = list(component = paste0("PC", i)))
                }

                # Only PCA and MDS produce loadings.
                if (self$options$showLoadings && self$options$method %in% c("pca", "mds")) {
                    for (i in seq_along(vars))
                        self$results$loadingsTable$addRow(
                            rowKey = vars[[i]], values = list(variable = vars[[i]]))
                }
            }
        },

        # Main analysis ----
        .run = function() {

            private$.noticeList <- list()

            # Reset projection cache so early-return paths (e.g. missing vars) do not leave
            # `.projectionPlot`/`.projection3D`/`.survivalPlot` rendering a prior run's stale
            # projection alongside fresh notice text.
            private$.projectionData <- NULL
            private$.keepIdx <- NULL
            private$.survData <- NULL

            # Check for required inputs
            n_dims <- as.numeric(self$options$dimensions)

            if (is.null(self$options$vars) || length(self$options$vars) < 2) {
                private$.addNotice('ERROR', 'Variables Required', 'Please select at least 2 variables for similarity analysis.')
                return()
            }

            # A p-variable dataset yields at most p dimensions; asking for more indexes
            # past the end of the PCA/MDS coordinate matrix and aborts with a cryptic
            # subscript error.
            if (length(self$options$vars) < n_dims) {
                private$.addNotice('ERROR', 'Too Few Variables for Requested Dimensions', sprintf('A %d-dimensional projection needs at least %d variables; %d selected. Select more variables or switch to a 2D projection.', n_dims, n_dims, length(self$options$vars)))
                return()
            }

            # Set seed for reproducibility - save and restore global RNG state so subsequent
            # random draws elsewhere in the user's session are not affected by our seed.
            # Rtsne/umap/kmeans/dbscan all consume the RNG, so the seed effectively propagates
            # downstream without the restore; H4 hygiene fix mirrors optimalcutpoint.b.R:765-772.
            if (!is.null(self$options$seed)) {
                old_seed <- if (exists(".Random.seed", envir = .GlobalEnv))
                                get(".Random.seed", envir = .GlobalEnv)
                            else NULL
                on.exit({
                    if (!is.null(old_seed))
                        assign(".Random.seed", old_seed, envir = .GlobalEnv)
                }, add = TRUE)
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
                # Re-store. R copied `projection` at the assignment above, so without this
                # the cluster vector never reaches .projectionPlot / .survivalPlot and the
                # KM plot silently renders nothing.
                private$.projectionData <- projection
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

                # Remove rows with missing values. `keep` carries the surviving row numbers
                # of self$data all the way through outlier removal so every consumer can
                # realign to the original dataset.
                keep <- which(complete.cases(data))
                data <- data[keep, , drop = FALSE]

                # Check sample size
                if (nrow(data) < 5) {
                    private$.addNotice('ERROR', 'Insufficient Data', sprintf('Insufficient data for analysis (n=%d after removing missing values). At least 5 complete observations required.', nrow(data)))
                    return(NULL)
                } else if (nrow(data) < 10) {
                    private$.addNotice('STRONG_WARNING', 'Very Small Sample', sprintf('Sample size is very small (n=%d). Results may be unreliable. Consider collecting more data for robust analysis.', nrow(data)))
                } else if (nrow(data) < 30) {
                    private$.addNotice('WARNING', 'Small Sample', sprintf('Sample size is small (n=%d). Results should be interpreted with caution.', nrow(data)))
                }

                # Remove outliers if requested
                if (self$options$removeOutliers) {
                    ok <- private$.outlierKeep(data)
                    n_dropped <- sum(!ok)
                    keep <- keep[ok]
                    data <- data[ok, , drop = FALSE]

                    if (n_dropped > 0) {
                        private$.addNotice('WARNING', 'Outliers Removed', sprintf('%d observation(s) outside 1.5 x IQR on at least one variable were removed before projection.', n_dropped))
                    }

                    if (nrow(data) < 5) {
                        private$.addNotice('ERROR', 'Insufficient Data After Outlier Removal', sprintf('Only %d observation(s) remain after outlier removal. At least 5 are required. Turn off outlier removal or widen your selection.', nrow(data)))
                        return(NULL)
                    }
                }

                # A constant column makes scale() emit NaN and every projection method fail
                # with an opaque numerical error, so drop it explicitly and say so.
                col_sd <- vapply(data, stats::sd, numeric(1), na.rm = TRUE)
                constant <- !is.finite(col_sd) | col_sd == 0
                if (any(constant)) {
                    if (sum(!constant) < 2) {
                        private$.addNotice('ERROR', 'No Variation in Data', 'Fewer than 2 of the selected variables vary across the retained observations, so patient similarity cannot be computed.')
                        return(NULL)
                    }
                    private$.addNotice('WARNING', 'Constant Variables Dropped', sprintf('Variable(s) with no variation were excluded from the projection: %s.', paste(names(data)[constant], collapse = ', ')))
                    data <- data[, !constant, drop = FALSE]
                }

                # Scale if requested
                if (self$options$scaleVars) {
                    scaled_data <- scale(data)
                } else {
                    scaled_data <- as.matrix(data)
                }

                # Store original data for exactly the rows that reached the projection
                original_data <- self$data[keep, , drop = FALSE]
                private$.keepIdx <- keep

                list(
                    scaled_data = scaled_data,
                    original_data = original_data,
                    keep = keep
                )

            }, error = function(e) {
                private$.addNotice('ERROR', 'Data Preparation Error', paste('Error preparing data:', conditionMessage(e), 'Please check that all selected variables contain valid numeric values.'))
                return(NULL)
            })
        },

        # Returns a logical mask over the rows of `data` (TRUE = keep) rather than the
        # subset itself, so the caller can carry the original row numbers forward.
        .outlierKeep = function(data) {
            ok <- rep(TRUE, nrow(data))
            for (col in names(data)) {
                Q1 <- stats::quantile(data[[col]], 0.25, na.rm = TRUE)
                Q3 <- stats::quantile(data[[col]], 0.75, na.rm = TRUE)
                IQR_val <- Q3 - Q1
                lower <- Q1 - 1.5 * IQR_val
                upper <- Q3 + 1.5 * IQR_val
                ok <- ok & !is.na(data[[col]]) & data[[col]] >= lower & data[[col]] <= upper
            }
            ok
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
                private$.addNotice('ERROR', 'Projection Error', paste('Error in projection:', conditionMessage(e), 'This may be due to insufficient data or numerical issues. Try a different method or check your input data.'))
                return(NULL)
            })
        },

        .runPCA = function(data, n_dims) {
            # Center is required for valid PCA (components about the mean, not the origin);
            # harmless no-op when scaleVars already standardized the data.
            pca_result <- prcomp(data, center = TRUE, scale. = FALSE)

            # Extract coordinates
            coords <- pca_result$x[, 1:n_dims, drop = FALSE]

            # Variance explained
            variance <- summary(pca_result)$importance[2, 1:n_dims]
            cumulative <- summary(pca_result)$importance[3, 1:n_dims]

            # Fill the rows created in .init() - by position, so the rowKey type can never
            # drift. addRow() here would stack a duplicate set on every re-run.
            var_table <- self$results$varianceTable
            for (i in 1:n_dims) {
                var_table$setRow(rowNo = i, values = list(
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
                private$.addNotice('ERROR', 'Rtsne Package Required', 'Package "Rtsne" is required for t-SNE analysis. Install with: install.packages("Rtsne")')
                return(NULL)
            }

            # Perplexity validation
            if (nrow(data) < 3 * self$options$perplexity) {
                new_perp <- floor((nrow(data) - 1) / 3)
                if (new_perp < 1) new_perp <- 1

                private$.addNotice('WARNING', 'Perplexity Adjusted', sprintf('t-SNE Perplexity (%g) is too high for sample size (n=%d). Automatically adjusted to %g. For optimal results, use perplexity < n/3.',
                    self$options$perplexity, nrow(data), new_perp))

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
                private$.addNotice('ERROR', 'umap Package Required', 'Package "umap" is required for UMAP analysis. Install with: install.packages("umap")')
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
            # Rows were created in .init(), one per selected variable, keyed by variable
            # name. .prepareData() may have dropped constant variables, so iterate over the
            # option list (never over `loadings`) - every setRow() then hits a key that
            # exists, and a dropped variable keeps an explicitly empty row instead of
            # silently vanishing; the "Constant Variables Dropped" notice names it.
            load_table <- self$results$loadingsTable
            vars <- self$options$vars
            idx <- match(vars, rownames(loadings))

            for (i in seq_along(vars)) {
                j <- idx[i]
                values <- list(
                    dim1 = if (is.na(j)) NA_real_ else loadings[j, 1],
                    dim2 = if (is.na(j)) NA_real_ else loadings[j, 2]
                )

                if (ncol(loadings) >= 3) {
                    values$dim3 <- if (is.na(j)) NA_real_ else loadings[j, 3]
                }

                load_table$setRow(rowKey = vars[[i]], values = values)
            }
        },

        # Clustering ----

        # DBSCAN reports unassigned points as cluster 0; they are noise, not a subgroup.
        .clusterLabel = function(cl) {
            if (self$options$clusterMethod == "dbscan" && as.numeric(cl) == 0)
                "Noise (unassigned)"
            else
                paste0("Cluster ", cl)
        },

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
                        private$.addNotice('WARNING', 'dbscan Package Missing', 'Package "dbscan" not available. Using k-means clustering instead. Install dbscan with: install.packages("dbscan")')
                        clusters <- kmeans(coords, centers = self$options$nClusters)$cluster
                    } else {
                        clusters <- dbscan::dbscan(coords, eps = self$options$dbscan_eps, minPts = self$options$dbscan_minpts)$cluster
                        # Check if only noise/one cluster found
                        n_found <- length(unique(clusters[clusters != 0]))
                        if (n_found < 2) {
                            private$.addNotice('WARNING', 'DBSCAN Few Clusters', sprintf('DBSCAN found only %d cluster(s). Try adjusting epsilon (eps) or minimum points (minPts) parameters to identify more clusters.', n_found))
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
                cluster_summary <- self$results$clusterSummary
                cluster_summary$deleteRows()

                for (cl in sort(unique(clusters))) {
                    cluster_summary$addRow(rowKey = cl, values = list(
                        cluster = private$.clusterLabel(cl),
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

                        # rows: 1 in the .r.yaml already created row "1"; addRow(rowKey = 1)
                        # would append a second, blank-topped row (rowKeys are type-strict).
                        self$results$clusterQuality$setRow(rowNo = 1, values = list(
                            metric = "Silhouette Score",
                            value = sil_score
                        ))
                    }
                }

                # Export if requested
                if (self$options$exportClusters &&
                    self$results$exportClusters$isNotFilled() &&
                    length(clusters) == length(private$.keepIdx)) {
                    # self$data is a plain data.frame - it has no $rowCount. Row identity is
                    # carried by rownames(), which is what every other export in this module uses.
                    cluster_export <- rep(NA_integer_, nrow(self$data))
                    cluster_export[private$.keepIdx] <- as.integer(clusters)
                    self$results$exportClusters$setRowNums(rownames(self$data))
                    self$results$exportClusters$setValues(cluster_export)
                }

            }, error = function(e) {
                private$.addNotice('WARNING', 'Clustering Error', paste('Error in clustering:', conditionMessage(e), 'Clustering skipped. Try different parameters or method.'))
            })

            return(clusters)
        },

        .clusterCharacteristics = function(clusters, original_data) {
            char_table <- self$results$clusterCharacteristics
            char_table$deleteRows()

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
                            title = private$.clusterLabel(cl),
                            type = "text"
                        )
                    }
                }

                char_table$addRow(rowKey = var, values = values)
            }
        },


        .clusterOutcomes = function(clusters, original_data) {
            outcome_table <- self$results$clusterOutcomes
            outcome_table$deleteRows()
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
                    cluster = private$.clusterLabel(cl),
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
                private$.addNotice('ERROR', 'Survival Variables Required', 'Survival analysis requires both time and event variables. Please select survival time and event in the Survival Analysis panel.')
                return()
            }

            event_var <- original_data[[self$options$survivalEvent]]
            event_level <- self$options$survivalEventLevel

            # A factor event with no level chosen makes `event_var == event_level` return
            # logical(0), which propagates as a size-0 recycling error rather than advice.
            if (is.factor(event_var) && (is.null(event_level) || !nzchar(event_level))) {
                private$.addNotice('ERROR', 'Event Level Required', 'Select which level of the survival event variable represents the event (for example "1" or "Dead") in the Survival Analysis panel.')
                return()
            }

            tryCatch({

                survtime <- jmvcore::toNumeric(original_data[[self$options$survivalTime]])

                if (is.factor(event_var)) {
                    event <- as.numeric(event_var == event_level)
                } else {
                    event <- jmvcore::toNumeric(event_var)
                }

                clusters <- projection$clusters

                # Drop incomplete survival rows explicitly; survfit/survdiff would drop them
                # silently and the per-cluster Ns would then disagree with the KM curves.
                usable <- !is.na(survtime) & !is.na(event) & !is.na(clusters)
                if (sum(!usable) > 0) {
                    private$.addNotice('WARNING', 'Survival Missing Values', sprintf('Survival analysis: %d observation(s) with missing time or event values were excluded from the analysis.', sum(!usable)))
                }

                if (sum(usable) < 2 || length(unique(clusters[usable])) < 2) {
                    private$.addNotice('WARNING', 'Survival Comparison Not Possible', 'At least two clusters with complete survival data are needed to compare survival. Adjust the clustering settings or check the survival variables.')
                    return()
                }

                surv_df <- data.frame(
                    survtime = survtime[usable],
                    event    = event[usable],
                    clusters = factor(clusters[usable])
                )

                # Cached for .survivalPlot - the renderer must not re-derive these rows.
                private$.survData <- surv_df

                # Populate survival heading
                self$results$survivalHeading$setContent(
                    "<h3>Survival Analysis by Cluster</h3><p>Comparing survival outcomes across discovered patient subgroups.</p>"
                )

                # Kaplan-Meier fit: the median must come from the KM estimator, not from the
                # median observed time among patients who had the event (which ignores
                # censoring entirely and is biased downwards).
                fit <- survival::survfit(
                    survival::Surv(survtime, event) ~ clusters, data = surv_df)
                # Always a matrix here: the guard above ensures >= 2 retained clusters.
                fit_table <- summary(fit)$table

                surv_table <- self$results$survivalTable
                surv_table$deleteRows()

                for (cl in levels(surv_df$clusters)) {
                    key <- paste0("clusters=", cl)
                    idx <- match(key, rownames(fit_table))

                    surv_table$addRow(rowKey = cl, values = list(
                        cluster = private$.clusterLabel(cl),
                        n = sum(surv_df$clusters == cl),
                        events = sum(surv_df$event[surv_df$clusters == cl]),
                        median_survival = if (is.na(idx)) NA_real_
                                          else unname(fit_table[idx, "median"])
                    ))
                }

                # Log-rank test
                logrank <- survival::survdiff(
                    survival::Surv(survtime, event) ~ clusters, data = surv_df)

                lr_df <- length(logrank$n) - 1
                self$results$survivalComparison$setRow(rowNo = 1, values = list(
                    chisq = logrank$chisq,
                    df = lr_df,
                    pvalue = stats::pchisq(logrank$chisq, lr_df, lower.tail = FALSE)
                ))

            }, error = function(e) {
                # message() never reaches a jamovi user; route the failure to the notice pane.
                private$.addNotice('WARNING', 'Survival Analysis Error', paste('Survival analysis could not be completed:', conditionMessage(e), 'Check that the time variable is numeric and the event variable is coded consistently.'))
            })
        },

        # Plotting ----

        # Builds the coordinate frame every renderer shares: dimension columns plus the
        # optional Color / Cluster aesthetics, all indexed by private$.keepIdx so the
        # rows line up with the projection even after outlier removal.
        .plotFrame = function() {

            projection <- private$.projectionData
            if (is.null(projection) || is.null(projection$coords))
                return(NULL)

            coords <- as.data.frame(projection$coords)
            if (ncol(coords) < 2)
                return(NULL)

            colnames(coords) <- paste0("Dim", seq_len(ncol(coords)))

            keep <- private$.keepIdx
            if (!is.null(self$options$colorBy) &&
                !is.null(keep) && length(keep) == nrow(coords)) {
                coords$Color <- self$data[[self$options$colorBy]][keep]
            }

            if (!is.null(projection$clusters) &&
                length(projection$clusters) == nrow(coords)) {
                coords$Cluster <- factor(vapply(projection$clusters,
                                                private$.clusterLabel, character(1)))
            }

            coords
        },

        .projectionPlot = function(image, ggtheme, theme, ...) {

            coords <- private$.plotFrame()
            if (is.null(coords))
                return(FALSE)

            projection <- private$.projectionData

            # A 3D projection still gets a 2D main plot of the first two dimensions;
            # returning early here left the panel blank whenever dimensions = 3.
            if ("Color" %in% names(coords)) {
                p <- ggplot(coords, aes(x = Dim1, y = Dim2, color = Color)) +
                    labs(color = self$options$colorBy)
            } else if ("Cluster" %in% names(coords)) {
                p <- ggplot(coords, aes(x = Dim1, y = Dim2, color = Cluster))
            } else {
                p <- ggplot(coords, aes(x = Dim1, y = Dim2))
            }

            if ("Color" %in% names(coords) && "Cluster" %in% names(coords))
                p <- p + geom_point(aes(shape = Cluster), size = 2, alpha = 0.6)
            else
                p <- p + geom_point(size = 2, alpha = 0.6)

            p <- p +
                labs(
                    title = paste0("Patient Similarity - ", projection$method),
                    x = "Dimension 1",
                    y = "Dimension 2"
                ) +
                ggtheme +
                theme(plot.title = element_text(hjust = 0.5, face = "bold"))

            print(p)
            TRUE
        },

        .projection3D = function(image, ggtheme, theme, ...) {

            if (as.numeric(self$options$dimensions) != 3)
                return(FALSE)

            coords <- private$.plotFrame()
            if (is.null(coords) || !"Dim3" %in% names(coords))
                return(FALSE)

            # jamovi Image items paint onto a graphics device; a plotly htmlwidget writes
            # nothing to one (and print() on it tries to open a browser), so the three
            # pairwise views of the 3D projection are drawn as a faceted ggplot instead.
            views <- list(
                c("Dim1", "Dim2"),
                c("Dim1", "Dim3"),
                c("Dim2", "Dim3")
            )

            panels <- lapply(views, function(v) {
                d <- data.frame(
                    x = coords[[v[1]]],
                    y = coords[[v[2]]],
                    panel = paste0(sub("Dim", "Dimension ", v[1]), " vs ",
                                   sub("Dim", "Dimension ", v[2])),
                    stringsAsFactors = FALSE
                )
                if ("Color" %in% names(coords))   d$Color <- coords$Color
                if ("Cluster" %in% names(coords)) d$Cluster <- coords$Cluster
                d
            })
            pairs_df <- do.call(rbind, panels)

            if ("Color" %in% names(pairs_df)) {
                p <- ggplot(pairs_df, aes(x = x, y = y, color = Color)) +
                    labs(color = self$options$colorBy)
            } else if ("Cluster" %in% names(pairs_df)) {
                p <- ggplot(pairs_df, aes(x = x, y = y, color = Cluster))
            } else {
                p <- ggplot(pairs_df, aes(x = x, y = y))
            }

            p <- p +
                geom_point(size = 1.8, alpha = 0.6) +
                facet_wrap(~ panel, scales = "free", nrow = 2) +
                labs(
                    title = paste0("3D Projection - ", private$.projectionData$method),
                    x = NULL,
                    y = NULL
                ) +
                ggtheme +
                theme(plot.title = element_text(hjust = 0.5, face = "bold"))

            print(p)
            TRUE
        },

        .survivalPlot = function(image, ggtheme, theme, ...) {

            # .survData is populated only when .performSurvivalAnalysis succeeded, so this
            # single guard also covers "clustering off", "survival off" and early aborts.
            surv_df <- private$.survData
            if (is.null(surv_df) || nrow(surv_df) == 0)
                return(FALSE)

            # Bare `Surv` (not `survival::Surv`) is globally allow-listed by jmvcore::asFormula.
            # `.asSurvivalFormula` is the project wrapper around jmvcore::asFormula with the
            # survival helper allow-list applied. `survtime`/`event`/`clusters` are internal
            # column names of surv_df, not user column names - composeTerm not needed.
            surv_formula <- .asSurvivalFormula("Surv(survtime, event) ~ clusters")
            fit <- survival::survfit(surv_formula, data = surv_df)
            # survminer re-parses fit$call$formula; passing the formula through a variable
            # leaves a bare symbol there and ggsurvplot dies with
            # "object of type 'symbol' is not subsettable".
            fit$call$formula <- surv_formula

            plot <- survminer::ggsurvplot(
                fit,
                data = surv_df,
                pval = TRUE,
                risk.table = TRUE,
                legend.labs = vapply(levels(surv_df$clusters), private$.clusterLabel, character(1)),
                title = "Survival by Discovered Cluster",
                ggtheme = ggtheme,
                # Without this the risk table inherits jamovi's 16pt ggtheme and renders
                # as an empty strip at the default image size.
                tables.theme = survminer::theme_cleantable()
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
            # Export first dimension only (jamovi Output supports single column)
            # For multi-column export, use Output objects for each dimension separately in .a.yaml/.r.yaml
            coord_export <- rep(NA_real_, nrow(self$data))
            coord_export[private$.keepIdx] <- coords[, 1]  # Dim 1 only

            self$results$exportCoordinates$setRowNums(rownames(self$data))
            self$results$exportCoordinates$setValues(coord_export)

            # Inform user if >1 dimension
            if (n_dims > 1) {
                private$.addNotice('INFO', 'Coordinates Export Limited', sprintf('Note: Only Dimension 1 exported to dataset. Total dimensions available: %d. For all dimensions, use external tools or save the full projection results.', n_dims))
            }
        }
    )
)
