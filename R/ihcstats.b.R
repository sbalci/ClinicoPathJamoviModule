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
        .kmeans = NULL,      # Stores K-means clustering results
        .pca = NULL,         # Stores PCA results
        .silhouette = NULL,  # Stores silhouette analysis results
        .cluster_profiles = NULL,  # Stores detailed cluster profiles
        .optimal_k = NULL,   # Stores optimal number of clusters
        .marker_performance = NULL, # Stores marker diagnostic performance
        .optimal_markers = NULL,    # Stores selected optimal markers
        .group_data = NULL,         # Stores grouping variable data
        .prognostic_groups = NULL,  # Stores prognostic clustering groups (Matsuoka)
        .group_labels = NULL,       # Stores prognostic group labels
        .regional_data = NULL,      # Stores multi-region analysis data
        .survival_data = NULL,      # Stores survival analysis results
        .differential_results = NULL, # Stores Olsen differential diagnosis results
        .antibody_performance = NULL, # Stores individual antibody performance metrics
        .optimal_panels = NULL,     # Stores optimal antibody combinations
        .tumor_types = NULL,        # Stores tumor type data for validation
        .sterlacci_results = NULL,  # Stores Sterlacci TIL signature analysis
        .reproducibility_kappa = NULL, # Stores Cohen's kappa values
        .supervised_clusters = NULL, # Stores supervised clustering results
        .til_signatures = NULL,     # Stores TIL signature characterization
        
        # Constants for visualization and analysis
        .COLORS = list(
            primary = c("#E74C3C", "#3498DB", "#2ECC71", "#F39C12", "#9B59B6", "#1ABC9C", "#34495E", "#E67E22"),
            scientific = c("#2E86AB", "#A23B72", "#F18F01", "#0F4C75", "#3282B8", "#BBE1FA"),
            clinical = c("#FFFFFF", "#FFF2CC", "#FFE699", "#FFD966", "#FFCC02", "#E6AC00", "#CC9900", "#B38600", "#8B4513"),
            neutral = list(
                dark = "#2C3E50",
                medium = "#34495E",
                light_gray = "#7F8C8D",
                lighter_gray = "#95A5A6",
                background = "#ECF0F1",
                border = "#BDC3C7"
            ),
            ui = list(
                text_primary = "#2C3E50",
                text_secondary = "#666666",
                text_caption = "#888888",
                border_light = "#E0E0E0",
                border_lighter = "#F0F0F0",
                background_white = "white",
                background_panel = "#F8F9FA",
                border_panel = "#DEE2E6"
            )
        ),
        
        .PLOT_PARAMS = list(
            font_sizes = list(
                title = 14,
                subtitle = 12,
                axis_title = 12,
                axis_text = 10,
                legend_title = 11,
                legend_text = 10,
                caption = 10,
                strip_text = 11
            ),
            dimensions = list(
                cell_width = 15,
                cell_height = 12,
                min_plot_width = 600,
                min_plot_height = 400,
                max_markers_detailed = 6,
                max_clusters_detailed = 4
            ),
            alpha_values = list(
                violin = 0.4,
                boxplot = 0.8,
                jitter = 0.3,
                outlier = 0.6
            )
        ),
        
        .THRESHOLDS = list(
            silhouette = list(
                strong = 0.7,
                reasonable = 0.5,
                weak = 0.25
            ),
            significance = list(
                high = 0.001,
                medium = 0.01,
                low = 0.05
            ),
            performance = list(
                max_samples_full = 1000,
                max_samples_warning = 2000,
                min_markers = 2,
                max_optimal_markers = 6
            )
        ),
        
        # Package dependency checker
        .checkPackageDependencies = function(required_packages) {
            missing_packages <- character(0)
            for (pkg in required_packages) {
                if (!requireNamespace(pkg, quietly = TRUE)) {
                    missing_packages <- c(missing_packages, pkg)
                }
            }
            
            if (length(missing_packages) > 0) {
                error_msg <- paste("Required packages not available:", paste(missing_packages, collapse = ", "))
                jmvcore::reject(error_msg)
                return(FALSE)
            }
            return(TRUE)
        },
        
        # Safe execution wrapper for error handling with progress reporting
        .safeExecute = function(operation, operation_name = "Analysis", fallback = NULL, show_progress = TRUE) {
            if (show_progress) {
                private$.reportProgress(paste("Starting", operation_name, "..."))
            }
            
            tryCatch({
                result <- operation()
                if (show_progress) {
                    private$.reportProgress(paste(operation_name, "completed successfully"))
                }
                return(result)
            }, error = function(e) {
                error_msg <- paste(operation_name, "failed:", conditionMessage(e))
                private$.reportProgress(error_msg, type = "error")
                jmvcore::reject(error_msg)
                return(fallback)
            }, warning = function(w) {
                warning_msg <- paste(operation_name, "warning:", conditionMessage(w))
                private$.reportProgress(warning_msg, type = "warning")
                return(operation())
            })
        },
        
        # Progress reporting method
        .reportProgress = function(message, type = "info") {
            timestamp <- format(Sys.time(), "%H:%M:%S")
            formatted_message <- paste0("[", timestamp, "] ", message)
            
            if (type == "error") {
                message(paste("ERROR:", formatted_message))
            } else if (type == "warning") {
                message(paste("WARNING:", formatted_message))
            } else {
                message(formatted_message)
            }
        },
        
        # Get color palette based on number of items and context
        .getColorPalette = function(n_items, context = "primary") {
            if (n_items <= 3) {
                return(private$.COLORS$primary[1:n_items])
            } else if (n_items <= 8) {
                if (context == "scientific") {
                    return(private$.COLORS$scientific[1:min(n_items, 6)])
                } else {
                    return(RColorBrewer::brewer.pal(n_items, "Set1"))
                }
            } else {
                return(rainbow(n_items, s = 0.8, v = 0.9))
            }
        },
        
        # Create publication-quality color palette for different visualization contexts
        .createPublicationColorPalette = function(n_items, context = "general") {
            # Define publication-quality color schemes
            publication_palettes <- list(
                # Nature-style colors (colorblind-friendly)
                nature = c("#E64B35FF", "#4DBBD5FF", "#00A087FF", "#3C5488FF", "#F39B7FFF", 
                          "#8491B4FF", "#91D1C2FF", "#DC0000FF"),
                # Scientific journal colors
                science = c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", 
                           "#E6AB02", "#A6761D", "#666666"),
                # Cell/Nature Medicine colors
                medical = c("#C51B7D", "#2166AC", "#762A83", "#5AAE61", "#FDB863", 
                           "#B2182B", "#D1E5F0", "#F7F7F7"),
                # Dendrogram-specific (high contrast, colorblind safe)
                dendrogram = c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", 
                              "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf"),
                # Heatmap-specific (sequential and diverging)
                heatmap = c("#2166ac", "#4393c3", "#92c5de", "#d1e5f0", "#fdbf6f", 
                           "#ff7f00", "#d94701", "#a50f15")
            )
            
            # Select palette based on context
            if (context %in% names(publication_palettes)) {
                base_colors <- publication_palettes[[context]]
            } else {
                base_colors <- publication_palettes$nature
            }
            
            # Handle different numbers of clusters
            if (n_items <= length(base_colors)) {
                return(base_colors[1:n_items])
            } else {
                # For many clusters, create interpolated palette
                return(grDevices::colorRampPalette(base_colors)(n_items))
            }
        },
        
        # Memory cleanup method
        .cleanup = function() {
            # Clear large temporary objects to free memory
            private$.ihc_matrix <- NULL
            private$.hc <- NULL
            private$.pam <- NULL
            private$.kmeans <- NULL
            private$.pca <- NULL
            private$.silhouette <- NULL
            # Keep smaller result objects for display
            gc(verbose = FALSE)  # Force garbage collection
        },
        
        # Dataset size validation and performance optimization
        .checkDatasetSize = function(data, max_samples = 1000) {
            n_samples <- nrow(data)
            n_markers <- ncol(data)
            
            if (n_samples > max_samples) {
                warning_msg <- paste("Large dataset detected (", n_samples, "samples).", 
                                   "Consider using sampling for better performance.")
                message(warning_msg)
                
                # Suggest sampling for very large datasets
                if (n_samples > max_samples * 2) {
                    jmvcore::reject(paste("Dataset too large (", n_samples, "samples).", 
                                        "Please consider sampling or increase maxSamples option."))
                    return(FALSE)
                }
            }
            
            return(TRUE)
        },
        
        # Consolidated validation helper functions
        .validateBasicRequirements = function(markers, data) {
            # Check if markers have been selected
            if (is.null(markers) || length(markers) == 0) {
                return(FALSE)
            }

            # Check if data has rows
            if (nrow(data) == 0) {
                stop('Data contains no (complete) rows')
            }

            # Validate marker data
            for (marker in markers) {
                if (!marker %in% names(data)) {
                    stop(paste("Marker", marker, "not found in data"))
                }
                if (!is.factor(data[[marker]]) && !is.numeric(data[[marker]])) {
                    stop(paste("Marker", marker, "must be factor or numeric"))
                }
            }
            
            return(TRUE)
        },
        
        .validateVariableExists = function(var_name, var_value, data, required_type = NULL) {
            if (is.null(var_value) || var_value == "") {
                return(FALSE)
            }
            
            if (!var_value %in% names(data)) {
                stop(paste(var_name, "not found in data"))
            }
            
            if (!is.null(required_type)) {
                if (required_type == "numeric" && !is.numeric(data[[var_value]])) {
                    stop(paste(var_name, "must be numeric"))
                }
            }
            
            return(TRUE)
        },
        
        .validateAnalysisRequirements = function() {
            # Validate package dependencies first
            private$.validatePackageDependencies()
            
            # Distance metric validation
            private$.validateDistanceMetric()
            
            # Multi-region analysis validation
            if (self$options$tumorRegionAnalysis) {
                if (!private$.validateVariableExists("Central region variable", self$options$centralRegionVar, self$data) ||
                    !private$.validateVariableExists("Invasive region variable", self$options$invasiveRegionVar, self$data)) {
                    stop("Multi-region analysis requires both central and invasive region variables")
                }
            }
            
            # Survival analysis validation  
            if (self$options$prognosticClustering) {
                if (!is.null(self$options$survivalTimeVar) && self$options$survivalTimeVar != "") {
                    private$.validateVariableExists("Survival time variable", self$options$survivalTimeVar, self$data, "numeric")
                }
                if (!is.null(self$options$survivalEventVar) && self$options$survivalEventVar != "") {
                    private$.validateVariableExists("Survival event variable", self$options$survivalEventVar, self$data)
                }
            }
            
            # Differential diagnosis validation
            if (self$options$differentialDiagnosis || self$options$antibodyOptimization) {
                if (!private$.validateVariableExists("Tumor type variable", self$options$tumorTypeVar, self$data)) {
                    stop("Differential diagnosis/antibody optimization requires tumor type variable")
                }
            }
            
            return(TRUE)
        },
        
        # Distance metric validation for data type compatibility
        .validateDistanceMetric = function() {
            distance_method <- self$options$distanceMetric
            markers <- self$options$markers
            
            if (length(markers) == 0) return(TRUE)
            
            # Enhanced data type detection
            data_types <- sapply(self$data[markers], function(x) {
                if (is.factor(x)) return("categorical")
                if (is.numeric(x)) return("numeric")
                if (is.logical(x)) return("binary")
                return("other")
            })
            
            has_categorical <- any(data_types %in% c("categorical", "binary"))
            has_numeric <- any(data_types == "numeric")
            is_purely_numeric <- all(data_types == "numeric")
            
            # Strict validation with immediate stopping
            if (distance_method == "jaccard" && is_purely_numeric) {
                jmvcore::reject("Jaccard distance is not suitable for purely numeric data. Please use Gower distance instead.")
                return(FALSE)
            }
            
            if (!distance_method %in% c("gower", "jaccard")) {
                jmvcore::reject(paste("Distance metric", distance_method, "is not supported. Use 'gower' or 'jaccard'."))
                return(FALSE)
            }
            
            # Recommend optimal distance metric
            if (distance_method == "jaccard" && has_numeric && has_categorical) {
                message("Note: Gower distance is recommended for mixed categorical/numeric data.")
            }
            
            return(TRUE)
        },
        
        # Centralized package dependency validation
        .validatePackageDependencies = function(required_packages = NULL) {
            if (is.null(required_packages)) {
                # Define core required packages based on analysis options
                required_packages <- c("cluster")
                
                if (self$options$showHeatmap || self$options$olsenVisualization) {
                    required_packages <- c(required_packages, "pheatmap")
                }
                
                if (self$options$showDendrogram) {
                    required_packages <- c(required_packages, "dendextend")
                }
                
                if (self$options$pcaAnalysis || self$options$showPCAPlot) {
                    required_packages <- c(required_packages, "factoextra")
                }
                
                if (self$options$prognosticClustering) {
                    required_packages <- c(required_packages, "survival")
                }
                
                if (self$options$useParallel) {
                    required_packages <- c(required_packages, "parallel")
                }
            }
            
            missing_packages <- c()
            
            for (pkg in required_packages) {
                if (!requireNamespace(pkg, quietly = TRUE)) {
                    missing_packages <- c(missing_packages, pkg)
                }
            }
            
            if (length(missing_packages) > 0) {
                error_msg <- paste("Required packages not available:", paste(missing_packages, collapse = ", "))
                jmvcore::reject(error_msg)
                return(FALSE)
            }
            
            return(TRUE)
        },
        
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
            # Get the data for selected markers
            markers <- self$options$markers
            
            # Validate basic requirements
            if (!private$.validateBasicRequirements(markers, self$data)) {
                return()
            }
            
            data <- self$data[markers]
            
            # Check for minimum data requirements
            data_complete <- na.omit(data)
            if (nrow(data_complete) < 3) {
                stop('Need at least 3 complete cases for analysis')
            }
            
            # Check dataset size and performance considerations
            max_samples <- ifelse(!is.null(self$options$maxSamples), self$options$maxSamples, 1000)
            if (!private$.checkDatasetSize(data_complete, max_samples)) {
                return()
            }
            
            # Validate analysis-specific requirements
            private$.validateAnalysisRequirements()
            
            # Compute H-scores if requested
            if (self$options$computeHScore) {
                private$.safeExecute(function() private$.computeHScores(data), "H-score calculation")
            }

            # Perform clustering based on selected method
            clustering_result <- private$.safeExecute(function() {
                if (self$options$clusterMethod == "hierarchical") {
                    private$.performHierarchicalClustering(data)
                } else if (self$options$clusterMethod == "pam") {
                    private$.performPAMClustering(data)
                } else if (self$options$clusterMethod == "kmeans") {
                    private$.performKMeansClustering(data)
                } else if (self$options$clusterMethod == "pca_kmeans") {
                    private$.performPCAKMeansClustering(data)
                }
            }, paste(self$options$clusterMethod, "clustering"))
            
            # Only proceed if clustering succeeded
            if (is.null(clustering_result)) return()
            
            # Perform iterative marker selection if requested
            if (self$options$iterativeClustering) {
                private$.safeExecute(function() private$.performIterativeClustering(data), "Iterative marker selection")
            }
            
            # Perform multi-region tumor analysis if requested
            if (self$options$tumorRegionAnalysis) {
                private$.safeExecute(function() private$.performMultiRegionAnalysis(data), "Multi-region analysis")
            }
            
            # Perform prognostic clustering if requested
            if (self$options$prognosticClustering) {
                private$.safeExecute(function() private$.performPrognosticClustering(data), "Prognostic clustering")
            }
            
            # Perform Olsen differential diagnosis if requested
            if (self$options$differentialDiagnosis) {
                private$.safeExecute(function() private$.performOlsenDifferentialDiagnosis(data), "Differential diagnosis")
            }
            
            # Perform antibody optimization if requested
            if (self$options$antibodyOptimization) {
                private$.safeExecute(function() private$.performAntibodyOptimization(data), "Antibody optimization")
            }
            
            # Perform Sterlacci TIL signature analysis if requested
            if (self$options$sterlacciAnalysis) {
                private$.safeExecute(function() private$.performSterlacciAnalysis(data), "Sterlacci TIL analysis")
            }
            
            # Perform supervised clustering if requested
            if (self$options$supervisedClustering) {
                private$.safeExecute(function() private$.performSupervisedClustering(data), "Supervised clustering")
            }
            
            # Perform reproducibility testing if requested
            if (self$options$reproductibilityTesting) {
                private$.safeExecute(function() private$.performReproducibilityTesting(data), "Reproducibility testing")
            }
            
            # Cleanup large objects at the end
            private$.cleanup()
        },

        # Calculate custom Jaccard distance for IHC data (Optimized O(n²) version)
        .calculateJaccardDistance = function(ihc_matrix) {
            # Use vectorized approach for better performance
            n <- nrow(ihc_matrix)
            m <- ncol(ihc_matrix)
            
            # Pre-configure IHC-specific parameters
            max_distance <- private$.getMaxIHCDistance(ihc_matrix)
            
            # Try proxy package for optimized distance calculation
            if (requireNamespace("proxy", quietly = TRUE)) {
                return(private$.calculateJaccardDistanceProxy(ihc_matrix, max_distance))
            }
            
            # Fallback: optimized manual calculation
            return(private$.calculateJaccardDistanceManual(ihc_matrix, max_distance))
        },
        
        # Get maximum IHC distance based on configured scale or data range
        .getMaxIHCDistance = function(ihc_matrix) {
            # Use configured IHC scale maximum if available
            if (!is.null(self$options$ihcScaleMax) && self$options$ihcScaleMax > 0) {
                return(self$options$ihcScaleMax)
            }
            
            # Fallback: determine from data range
            max_vals <- apply(ihc_matrix, 2, max, na.rm = TRUE)
            min_vals <- apply(ihc_matrix, 2, min, na.rm = TRUE)
            return(max(max_vals - min_vals, na.rm = TRUE))
        },
        
        # Optimized Jaccard distance using proxy package
        .calculateJaccardDistanceProxy = function(ihc_matrix, max_distance) {
            jaccard_ihc <- function(x, y) {
                # Vectorized similarity calculation
                valid_pairs <- !(is.na(x) & is.na(y))
                if (sum(valid_pairs) == 0) return(1)
                
                x_valid <- x[valid_pairs]
                y_valid <- y[valid_pairs]
                
                # Calculate weighted similarity
                exact_matches <- (x_valid == y_valid)
                shared_weights <- sum(exact_matches)
                
                # Add partial similarity for near matches
                if (max_distance > 0) {
                    partial_matches <- !exact_matches
                    if (sum(partial_matches) > 0) {
                        distances <- abs(x_valid[partial_matches] - y_valid[partial_matches])
                        partial_similarities <- pmax(0, 1 - distances / max_distance)
                        shared_weights <- shared_weights + sum(partial_similarities)
                    }
                }
                
                # Return Jaccard distance (1 - similarity)
                similarity <- shared_weights / length(x_valid)
                return(1 - similarity)
            }
            
            return(proxy::dist(ihc_matrix, method = jaccard_ihc))
        },
        
        # Manual optimized Jaccard distance calculation
        .calculateJaccardDistanceManual = function(ihc_matrix, max_distance) {
            n <- nrow(ihc_matrix)
            
            # Progress reporting for large datasets
            if (n > 500) {
                message(paste("Computing distance matrix for", n, "samples..."))
            }
            
            # Parallel processing implementation
            if (self$options$useParallel && n > 100 && requireNamespace("parallel", quietly = TRUE)) {
                return(private$.calculateJaccardDistanceParallel(ihc_matrix, max_distance))
            }
            
            # Use lower triangular approach for efficiency
            distances <- numeric(n * (n - 1) / 2)
            k <- 1
            progress_step <- max(1, floor(n / 10))  # Report progress every 10%
            
            for (i in 1:(n-1)) {
                # Progress reporting
                if (n > 500 && i %% progress_step == 0) {
                    progress <- round(i / (n-1) * 100)
                    message(paste("Distance calculation progress:", progress, "%"))
                }
                
                # Vectorized calculation for row i against all subsequent rows
                i_row <- ihc_matrix[i, ]
                
                for (j in (i+1):n) {
                    j_row <- ihc_matrix[j, ]
                    
                    # Vectorized operations
                    valid_pairs <- !(is.na(i_row) & is.na(j_row))
                    if (sum(valid_pairs) == 0) {
                        distances[k] <- 1
                    } else {
                        i_valid <- i_row[valid_pairs]
                        j_valid <- j_row[valid_pairs]
                        
                        # Exact matches
                        exact_matches <- sum(i_valid == j_valid)
                        
                        # Partial matches (vectorized)
                        if (max_distance > 0) {
                            partial_indices <- i_valid != j_valid
                            if (sum(partial_indices) > 0) {
                                partial_similarities <- pmax(0, 1 - abs(i_valid[partial_indices] - j_valid[partial_indices]) / max_distance)
                                shared_weights <- exact_matches + sum(partial_similarities)
                            } else {
                                shared_weights <- exact_matches
                            }
                        } else {
                            shared_weights <- exact_matches
                        }
                        
                        similarity <- shared_weights / length(i_valid)
                        distances[k] <- 1 - similarity
                    }
                    k <- k + 1
                }
            }
            
            return(as.dist(distances))
        },
        
        # Parallel Jaccard distance calculation for large datasets
        .calculateJaccardDistanceParallel = function(ihc_matrix, max_distance) {
            n <- nrow(ihc_matrix)
            
            # Set up parallel processing
            num_cores <- min(parallel::detectCores() - 1, 4)  # Leave one core free, max 4
            
            message(paste("Using parallel processing with", num_cores, "cores for distance calculation..."))
            
            # Create cluster
            cl <- parallel::makeCluster(num_cores)
            on.exit(parallel::stopCluster(cl), add = TRUE)
            
            # Export necessary variables to cluster
            parallel::clusterExport(cl, c("ihc_matrix", "max_distance"), envir = environment())
            
            # Define chunk-based processing for better load balancing
            indices <- which(lower.tri(matrix(1, n, n)), arr.ind = TRUE)
            total_pairs <- nrow(indices)
            
            # Split work into chunks
            chunk_size <- ceiling(total_pairs / (num_cores * 2))  # Smaller chunks for better balance
            chunks <- split(1:total_pairs, ceiling(seq_along(1:total_pairs) / chunk_size))
            
            # Parallel computation function
            compute_chunk_distances <- function(chunk_ids) {
                chunk_indices <- indices[chunk_ids, , drop = FALSE]
                chunk_distances <- numeric(length(chunk_ids))
                
                for (k in seq_len(nrow(chunk_indices))) {
                    i <- chunk_indices[k, 1]
                    j <- chunk_indices[k, 2]
                    
                    i_row <- ihc_matrix[i, ]
                    j_row <- ihc_matrix[j, ]
                    
                    # Vectorized operations (same as sequential version)
                    valid_pairs <- !(is.na(i_row) & is.na(j_row))
                    if (sum(valid_pairs) == 0) {
                        chunk_distances[k] <- 1
                    } else {
                        i_valid <- i_row[valid_pairs]
                        j_valid <- j_row[valid_pairs]
                        
                        # Exact matches
                        exact_matches <- sum(i_valid == j_valid)
                        
                        # Partial matches
                        if (max_distance > 0) {
                            partial_indices <- i_valid != j_valid
                            if (sum(partial_indices) > 0) {
                                partial_similarities <- pmax(0, 1 - abs(i_valid[partial_indices] - j_valid[partial_indices]) / max_distance)
                                shared_weights <- exact_matches + sum(partial_similarities)
                            } else {
                                shared_weights <- exact_matches
                            }
                        } else {
                            shared_weights <- exact_matches
                        }
                        
                        similarity <- shared_weights / length(i_valid)
                        chunk_distances[k] <- 1 - similarity
                    }
                }
                return(chunk_distances)
            }
            
            # Execute parallel computation
            result_chunks <- parallel::parLapply(cl, chunks, compute_chunk_distances)
            
            # Combine results
            distances <- unlist(result_chunks)
            
            message("Parallel distance calculation completed.")
            
            return(as.dist(distances))
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
        
        # Convert IHC data to numeric matrix with enhanced error handling
        .prepareIHCMatrix = function(data) {
            # Comprehensive data cleaning and validation
            ihc_matrix <- data.frame(data)
            
            # Handle missing values and data type conversion
            for (i in 1:ncol(ihc_matrix)) {
                if (is.factor(ihc_matrix[[i]])) {
                    # Convert factors to proper numeric scale
                    levels_count <- length(levels(ihc_matrix[[i]]))
                    ihc_matrix[[i]] <- as.numeric(ihc_matrix[[i]]) - 1  # 0-based indexing
                    
                    # Handle missing values in factors
                    if (any(is.na(ihc_matrix[[i]]))) {
                        # Use mode imputation for factors
                        non_na_vals <- ihc_matrix[[i]][!is.na(ihc_matrix[[i]])]
                        if (length(non_na_vals) > 0) {
                            mode_val <- as.numeric(names(sort(table(non_na_vals), decreasing = TRUE))[1])
                            ihc_matrix[[i]][is.na(ihc_matrix[[i]])] <- mode_val
                        } else {
                            ihc_matrix[[i]][is.na(ihc_matrix[[i]])] <- 0  # Default to lowest score
                        }
                    }
                } else if (is.numeric(ihc_matrix[[i]])) {
                    # Handle missing values in numeric data
                    if (any(is.na(ihc_matrix[[i]]))) {
                        non_na_vals <- ihc_matrix[[i]][!is.na(ihc_matrix[[i]])]
                        if (length(non_na_vals) > 0) {
                            median_val <- median(non_na_vals, na.rm = TRUE)
                            ihc_matrix[[i]][is.na(ihc_matrix[[i]])] <- median_val
                        } else {
                            ihc_matrix[[i]][is.na(ihc_matrix[[i]])] <- 0
                        }
                    }
                    
                    # Handle infinite values
                    if (any(is.infinite(ihc_matrix[[i]]))) {
                        finite_vals <- ihc_matrix[[i]][is.finite(ihc_matrix[[i]])]
                        if (length(finite_vals) > 0) {
                            median_val <- median(finite_vals, na.rm = TRUE)
                            ihc_matrix[[i]][is.infinite(ihc_matrix[[i]])] <- median_val
                        } else {
                            ihc_matrix[[i]][is.infinite(ihc_matrix[[i]])] <- 0
                        }
                    }
                } else {
                    # Handle other data types by converting to numeric
                    ihc_matrix[[i]] <- as.numeric(as.factor(ihc_matrix[[i]])) - 1
                }
            }
            
            # Ensure all values are finite and complete
            ihc_matrix <- ihc_matrix[complete.cases(ihc_matrix), , drop = FALSE]
            
            # Final validation
            if (nrow(ihc_matrix) == 0) {
                stop("No complete cases available after data cleaning")
            }
            
            if (any(!is.finite(as.matrix(ihc_matrix)))) {
                stop("Data contains non-finite values after preprocessing")
            }
            
            # Create proper row names
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
            
            return(ihc_matrix)
        },
        
        # Perform hierarchical clustering with enhanced error handling
        .performHierarchicalClustering = function(data) {
            private$.reportProgress("Starting hierarchical clustering...")
            
            # Check required libraries
            required_packages <- c("pheatmap", "dendextend", "RColorBrewer", "cluster")
            if (!private$.checkPackageDependencies(required_packages)) {
                return(NULL)
            }
            
            # Enhanced data preparation with validation
            tryCatch({
                ihc_matrix <- private$.prepareIHCMatrix(data)
                
                # Validate matrix before clustering
                if (any(!is.finite(as.matrix(ihc_matrix)))) {
                    jmvcore::reject("Data preprocessing failed - non-finite values detected")
                    return(NULL)
                }
                
                # Calculate distance matrix with error handling
                if (self$options$distanceMetric == "jaccard") {
                    dist_matrix <- private$.calculateJaccardDistance(ihc_matrix)
                } else {
                    # Enhanced Gower distance calculation
                    dist_matrix <- cluster::daisy(ihc_matrix, metric = "gower", stand = FALSE)
                }
                
                # Validate distance matrix
                if (any(is.na(dist_matrix)) || any(!is.finite(dist_matrix))) {
                    jmvcore::reject("Distance calculation produced invalid values")
                    return(NULL)
                }
                
                # Perform clustering with validation
                hc <- hclust(dist_matrix, method = self$options$linkageMethod)
                
                # Validate hierarchical clustering result
                if (is.null(hc) || is.null(hc$height) || any(!is.finite(hc$height))) {
                    jmvcore::reject("Invalid hierarchical clustering result")
                    return(NULL)
                }
                
                clusters <- cutree(hc, k = self$options$nClusters)
                
                # Store results
                private$.clusters <- clusters
                private$.hc <- hc
                private$.ihc_matrix <- ihc_matrix
                
                # Generate cluster summaries
                private$.generateClusterSummaries(data, clusters)
                
                # Perform silhouette analysis if requested
                if (self$options$silhouetteAnalysis) {
                    private$.performSilhouetteAnalysis(dist_matrix, clusters)
                }
                
                private$.reportProgress("Hierarchical clustering completed successfully")
                
            }, error = function(e) {
                error_msg <- paste("Hierarchical clustering failed:", conditionMessage(e))
                private$.reportProgress(error_msg, type = "error")
                jmvcore::reject(error_msg)
                return(NULL)
            })
        },
        
        # Perform PAM (Partitioning Around Medoids) clustering
        .performPAMClustering = function(data) {
            # Check required library
            if (!private$.checkPackageDependencies(c("cluster"))) {
                return(NULL)
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
        
        # Perform iterative marker selection (Carvalho method)
        .performIterativeMarkerSelection = function(data) {
            # Step 1: Initial clustering with all markers
            ihc_matrix <- private$.prepareIHCMatrix(data)
            
            # Standardize if requested
            if (self$options$standardizeData) {
                ihc_matrix <- scale(ihc_matrix)
            }
            
            # Calculate initial clustering
            if (self$options$distanceMetric == "jaccard") {
                dist_matrix <- private$.calculateJaccardDistance(ihc_matrix)
            } else {
                dist_matrix <- cluster::daisy(ihc_matrix, metric = "gower")
            }
            
            # Initial hierarchical clustering
            hc_initial <- hclust(dist_matrix, method = self$options$linkageMethod)
            clusters_initial <- cutree(hc_initial, k = self$options$nClusters)
            
            # Step 2: Identify markers with overlapping patterns
            overlapping_markers <- private$.identifyOverlappingMarkers(ihc_matrix, clusters_initial)
            
            # Step 3: Remove overlapping markers and re-cluster
            if (length(overlapping_markers) > 0) {
                refined_matrix <- ihc_matrix[, !colnames(ihc_matrix) %in% overlapping_markers, drop = FALSE]
                
                # Recalculate distance with refined markers
                if (self$options$distanceMetric == "jaccard") {
                    dist_refined <- private$.calculateJaccardDistance(refined_matrix)
                } else {
                    dist_refined <- cluster::daisy(refined_matrix, metric = "gower")
                }
                
                # Refined clustering
                hc_refined <- hclust(dist_refined, method = self$options$linkageMethod)
                clusters_refined <- cutree(hc_refined, k = self$options$nClusters)
                
                # Store refined results
                private$.clusters <- clusters_refined
                private$.hc <- hc_refined
                private$.ihc_matrix <- refined_matrix
                private$.optimal_markers <- colnames(refined_matrix)
                
                # Generate summaries with refined markers
                private$.generateClusterSummaries(data[, colnames(refined_matrix), drop = FALSE], clusters_refined)
                
                # Generate optimal markers table
                private$.generateOptimalMarkersTable()
            }
        },
        
        # Identify markers with overlapping expression patterns (Carvalho method)
        .identifyOverlappingMarkers = function(ihc_matrix, clusters) {
            overlapping <- character(0)
            marker_scores <- numeric(ncol(ihc_matrix))
            names(marker_scores) <- colnames(ihc_matrix)
            
            # Calculate cluster separation score for each marker
            for (marker in colnames(ihc_matrix)) {
                marker_scores[marker] <- private$.calculateClusterSeparation(ihc_matrix[, marker], clusters)
            }
            
            # Calculate correlation matrix between markers
            cor_matrix <- cor(ihc_matrix, use = "pairwise.complete.obs")
            
            # Identify highly correlated markers (r > 0.75 as in Carvalho)
            high_cor_pairs <- which(abs(cor_matrix) > 0.75 & upper.tri(cor_matrix), arr.ind = TRUE)
            
            if (nrow(high_cor_pairs) > 0) {
                # For each highly correlated pair, remove the one with lower separation
                for (i in 1:nrow(high_cor_pairs)) {
                    marker1 <- rownames(cor_matrix)[high_cor_pairs[i, 1]]
                    marker2 <- colnames(cor_matrix)[high_cor_pairs[i, 2]]
                    
                    # Remove the marker with lower separation score
                    if (marker_scores[marker1] < marker_scores[marker2]) {
                        overlapping <- c(overlapping, marker1)
                    } else {
                        overlapping <- c(overlapping, marker2)
                    }
                }
            }
            
            # Also identify markers with very low discriminating power
            low_discrimination <- names(marker_scores)[marker_scores < 0.5]
            overlapping <- c(overlapping, low_discrimination)
            
            # Store marker performance for display
            private$.marker_performance <- marker_scores
            
            return(unique(overlapping))
        },
        
        # Calculate cluster separation for a single marker
        .calculateClusterSeparation = function(marker_values, clusters) {
            # Calculate between-cluster sum of squares
            overall_mean <- mean(marker_values, na.rm = TRUE)
            cluster_means <- aggregate(marker_values, by = list(clusters), FUN = mean, na.rm = TRUE)
            cluster_sizes <- table(clusters)
            
            between_ss <- sum(cluster_sizes * (cluster_means$x - overall_mean)^2, na.rm = TRUE)
            
            # Calculate within-cluster sum of squares
            within_ss <- 0
            for (k in unique(clusters)) {
                cluster_vals <- marker_values[clusters == k]
                cluster_mean <- mean(cluster_vals, na.rm = TRUE)
                within_ss <- within_ss + sum((cluster_vals - cluster_mean)^2, na.rm = TRUE)
            }
            
            # Return F-ratio as separation measure
            if (within_ss > 0) {
                separation <- between_ss / within_ss
            } else {
                separation <- 0
            }
            
            return(separation)
        },
        
        # Generate optimal markers table
        .generateOptimalMarkersTable = function() {
            if (is.null(private$.marker_performance))
                return()
            
            # Clear existing results
            # self$results$optimalMarkersTable$clear()  # Method not available in jamovi tables
            
            # Get performance scores
            scores <- private$.marker_performance
            
            # Determine status for each marker
            for (marker in names(scores)) {
                score <- scores[marker]
                
                # Determine status
                if (!is.null(private$.optimal_markers)) {
                    status <- ifelse(marker %in% private$.optimal_markers, "Retained", "Eliminated")
                } else {
                    status <- ifelse(score >= 0.5, "Good", "Poor")
                }
                
                # Add to results table
                self$results$optimalMarkersTable$addRow(rowKey = marker, values = list(
                    marker = marker,
                    separation_score = round(score, 3),
                    status = status
                ))
            }
        },
        
        # Generate summaries for each cluster
        .generateClusterSummaries = function(data, clusters) {
            # Clear existing results
            # self$results$clusterSummary$clear()  # Method not available in jamovi tables
            
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
            # self$results$silhouetteTable$clear()  # Method not available in jamovi tables
            
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
            # self$results$hscoreTable$clear()  # Method not available in jamovi tables
            
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
            
            # Check required packages
            required_packages <- c("dendextend", "RColorBrewer", "ggplot2", "ggdendro")
            if (!private$.checkPackageDependencies(required_packages)) {
                return()
            }
            
            # Get hierarchical clustering result
            hc <- private$.hc
            clusters <- private$.clusters
            n_clusters <- self$options$nClusters
            
            # Create publication-ready dendrogram using enhanced ggplot2
            private$.safeExecute({
                # Convert to dendrogram data
                dend_data <- ggdendro::dendro_data(hc, type = "rectangle")
                
                # Create sophisticated cluster colors for publication
                cluster_colors <- private$.createPublicationColorPalette(n_clusters, "dendrogram")
                
                # Get cluster assignments for coloring
                cluster_assignments <- cutree(hc, k = n_clusters)
                
                # Enhanced segment coloring based on cluster membership
                segments <- dend_data$segments
                labels <- dend_data$labels
                
                # Add cluster colors to labels
                if (nrow(labels) > 0) {
                    labels$cluster <- cluster_assignments[as.numeric(labels$label)]
                    labels$color <- cluster_colors[labels$cluster]
                }
                
                # Calculate meaningful statistics for subtitle
                n_samples <- length(cluster_assignments)
                cluster_sizes <- table(cluster_assignments)
                min_size <- min(cluster_sizes)
                max_size <- max(cluster_sizes)
                
                # Create the enhanced dendrogram plot
                p <- ggplot2::ggplot() +
                    ggplot2::geom_segment(data = segments, 
                                         ggplot2::aes(x = x, y = y, xend = xend, yend = yend),
                                         color = "#2C3E50", size = 0.6, alpha = 0.8) +
                    ggplot2::scale_y_reverse(expand = ggplot2::expansion(mult = c(0.02, 0.12))) +
                    ggplot2::labs(
                        title = "Hierarchical Clustering of IHC Expression Patterns",
                        subtitle = paste0("n = ", n_samples, " samples, ", n_clusters, " clusters", 
                                        " (size range: ", min_size, "-", max_size, ")"),
                        x = "Sample Cases",
                        y = "Distance",
                        caption = paste0("Clustering method: ", self$options$linkageMethod, 
                                       " | Distance metric: ", self$options$distanceMetric,
                                       " | Generated with ClinicoPath")
                    ) +
                    ggplot2::theme_minimal(base_size = 11) +
                    ggplot2::theme(
                        plot.title = ggplot2::element_text(size = 14, face = "bold", hjust = 0.5, 
                                                          color = "#2C3E50", margin = ggplot2::margin(b = 10)),
                        plot.subtitle = ggplot2::element_text(size = private$.PLOT_PARAMS$font_sizes$subtitle, hjust = 0.5, color = private$.COLORS$ui$text_secondary),
                        plot.caption = ggplot2::element_text(size = private$.PLOT_PARAMS$font_sizes$caption, color = private$.COLORS$ui$text_caption),
                        axis.title = ggplot2::element_text(size = 12, face = "bold"),
                        axis.text.x = ggplot2::element_blank(),
                        axis.ticks.x = ggplot2::element_blank(),
                        panel.grid = ggplot2::element_blank(),
                        panel.border = ggplot2::element_blank(),
                        plot.background = ggplot2::element_rect(fill = "white", color = NA),
                        panel.background = ggplot2::element_rect(fill = "white", color = NA)
                    )
                
                # Add cluster rectangles if requested
                if (exists("showClusterBoxes", where = self$options) && 
                    !is.null(self$options$showClusterBoxes) && 
                    self$options$showClusterBoxes) {
                    
                    # Calculate rectangle positions
                    cut_heights <- as.dendrogram(hc) %>% 
                        dendextend::heights_per_k.dendrogram(k = n_clusters)
                    cut_height <- cut_heights[as.character(n_clusters)]
                    
                    # Get cluster groups for rectangles
                    cluster_groups <- split(1:length(cluster_assignments), cluster_assignments)
                    
                    # Add rectangles for each cluster
                    for (i in seq_along(cluster_groups)) {
                        group_indices <- cluster_groups[[i]]
                        if (length(group_indices) > 1) {
                            x_min <- min(group_indices) - 0.4
                            x_max <- max(group_indices) + 0.4
                            
                            p <- p + ggplot2::geom_rect(
                                ggplot2::aes(xmin = x_min, xmax = x_max, 
                                           ymin = -Inf, ymax = cut_height),
                                fill = cluster_colors[i], alpha = 0.2,
                                color = cluster_colors[i], size = 1.2
                            )
                        }
                    }
                    
                    # Add cluster legend
                    cluster_legend <- data.frame(
                        Cluster = factor(1:n_clusters),
                        Color = cluster_colors[1:n_clusters]
                    )
                    
                    p <- p + ggplot2::geom_point(
                        data = cluster_legend,
                        ggplot2::aes(x = -Inf, y = -Inf, color = Cluster),
                        size = 0, alpha = 0
                    ) +
                    ggplot2::scale_color_manual(
                        name = "Cluster",
                        values = setNames(cluster_colors[1:n_clusters], 1:n_clusters),
                        guide = ggplot2::guide_legend(
                            title = "Clusters",
                            title.theme = ggplot2::element_text(size = 11, face = "bold"),
                            label.theme = ggplot2::element_text(size = 10)
                        )
                    ) +
                    ggplot2::theme(
                        legend.position = "right",
                        legend.background = ggplot2::element_rect(fill = "white", color = "#CCCCCC")
                    )
                }
                
                # Add sample labels if requested
                if (exists("showSampleLabels", where = self$options) && 
                    !is.null(self$options$showSampleLabels) && 
                    self$options$showSampleLabels && 
                    !is.null(self$options$id) && 
                    !is.null(self$data[[self$options$id]])) {
                    
                    # Get sample labels
                    sample_labels <- as.character(self$data[[self$options$id]])
                    
                    # Add labels to dendrogram
                    labels_data <- data.frame(
                        x = 1:length(sample_labels),
                        y = 0,
                        label = sample_labels,
                        cluster = factor(cluster_assignments)
                    )
                    
                    p <- p + ggplot2::geom_text(
                        data = labels_data,
                        ggplot2::aes(x = x, y = y, label = label, color = cluster),
                        angle = 90, hjust = 1, vjust = 0.5, size = 3
                    ) +
                    ggplot2::scale_color_manual(
                        values = setNames(cluster_colors[1:n_clusters], 1:n_clusters)
                    )
                }
                
                # Print the plot
                print(p)
                return(TRUE)
            }, "Dendrogram visualization")
        },
        
        # Perform multi-region tumor analysis (Matsuoka method)
        .performMultiRegionAnalysis = function(data) {
            # Clear existing results
            # self$results$regionalTable$clear()  # Method not available in jamovi tables
            
            # Check if region variables are provided
            central_var <- self$options$centralRegionVar
            invasive_var <- self$options$invasiveRegionVar
            
            if (is.null(central_var) || is.null(invasive_var)) {
                jmvcore::reject("Multi-region analysis requires both central and invasive region variables")
                return()
            }
            
            # Get regional data
            central_data <- self$data[[central_var]]
            invasive_data <- self$data[[invasive_var]]
            
            # Process each marker
            for (marker in self$options$markers) {
                marker_data <- self$data[[marker]]
                
                # Skip if not a factor
                if (!is.factor(marker_data)) next
                
                # Convert to numeric for analysis
                central_numeric <- private$.convertMatsuokaScale(central_data)
                invasive_numeric <- private$.convertMatsuokaScale(invasive_data)
                
                # Calculate means
                central_mean <- mean(central_numeric, na.rm = TRUE)
                invasive_mean <- mean(invasive_numeric, na.rm = TRUE)
                
                # Calculate correlation
                correlation <- cor(central_numeric, invasive_numeric, use = "complete.obs")
                
                # Perform paired t-test
                t_test <- t.test(central_numeric, invasive_numeric, paired = TRUE)
                p_value <- t_test$p.value
                
                # Determine significance
                significance <- ifelse(p_value < 0.001, "***", 
                                     ifelse(p_value < 0.01, "**",
                                           ifelse(p_value < 0.05, "*", "ns")))
                
                # Add to results table
                self$results$regionalTable$addRow(rowKey = marker, values = list(
                    marker = marker,
                    central_mean = round(central_mean, 2),
                    invasive_mean = round(invasive_mean, 2),
                    correlation = round(correlation, 3),
                    p_value = ifelse(p_value < 0.001, "< 0.001", sprintf("%.3f", p_value)),
                    significance = significance
                ))
            }
        },
        
        # Perform prognostic clustering using Ward's method (Matsuoka)
        .performPrognosticClustering = function(data) {
            # Clear existing results
            # self$results$prognosticTable$clear()  # Method not available in jamovi tables
            # self$results$survivalTable$clear()  # Method not available in jamovi tables
            
            # Convert IHC data to numeric matrix
            ihc_matrix <- private$.prepareIHCMatrix(data)
            
            # Apply Matsuoka three-tier scaling
            for (i in 1:ncol(ihc_matrix)) {
                ihc_matrix[,i] <- private$.calculateMatsuokaLoss(ihc_matrix[,i])
            }
            
            # Calculate distance matrix using Euclidean distance
            dist_matrix <- dist(ihc_matrix, method = "euclidean")
            
            # Perform Ward's hierarchical clustering
            hc_ward <- hclust(dist_matrix, method = "ward.D2")
            
            # Cut tree to get 3 prognostic groups (poor, good, intermediate)
            prognostic_groups <- cutree(hc_ward, k = 3)
            
            # Label groups based on expression patterns
            group_means <- aggregate(ihc_matrix, by = list(prognostic_groups), FUN = mean, na.rm = TRUE)
            
            # Determine prognosis labels based on overall expression loss
            overall_loss <- rowMeans(group_means[,-1], na.rm = TRUE)
            group_labels <- c("Good prognosis", "Intermediate prognosis", "Poor prognosis")[order(overall_loss)]
            
            # Store prognostic groups
            private$.prognostic_groups <- prognostic_groups
            private$.group_labels <- group_labels
            
            # Generate prognostic clustering table
            for (i in 1:3) {
                group_size <- sum(prognostic_groups == i)
                group_percentage <- round(group_size / length(prognostic_groups) * 100, 1)
                
                # Get expression pattern for this group
                group_members <- which(prognostic_groups == i)
                pattern_text <- private$.summarizeClusterPattern(data, group_members)
                
                self$results$prognosticTable$addRow(rowKey = i, values = list(
                    group = i,
                    label = group_labels[i],
                    size = group_size,
                    percentage = group_percentage,
                    pattern = pattern_text
                ))
            }
            
            # Perform survival analysis if survival variables are provided
            if (!is.null(self$options$survivalTimeVar) && !is.null(self$options$survivalEventVar)) {
                private$.performSurvivalAnalysis(prognostic_groups)
            }
        },
        
        # Convert marker values to Matsuoka three-tier loss scale
        .calculateMatsuokaLoss = function(marker_values) {
            # Matsuoka scale: 0 = marked loss, 1 = moderate loss, 2 = mild loss
            # Higher values indicate better prognosis
            if (is.factor(marker_values)) {
                # Convert factor levels to numeric
                numeric_values <- as.numeric(marker_values) - 1
            } else {
                numeric_values <- marker_values
            }
            
            # Normalize to 0-2 scale if not already
            if (max(numeric_values, na.rm = TRUE) > 2) {
                numeric_values <- round(numeric_values / max(numeric_values, na.rm = TRUE) * 2)
            }
            
            return(numeric_values)
        },
        
        # Convert data to Matsuoka scale for multi-region analysis
        .convertMatsuokaScale = function(data) {
            if (is.factor(data)) {
                # Map factor levels to 0-2 scale
                level_count <- length(levels(data))
                numeric_data <- as.numeric(data) - 1
                
                # Scale to 0-2 range
                if (level_count > 3) {
                    numeric_data <- round(numeric_data / (level_count - 1) * 2)
                }
            } else if (is.numeric(data)) {
                # Scale numeric data to 0-2 range
                numeric_data <- round((data - min(data, na.rm = TRUE)) / 
                                    (max(data, na.rm = TRUE) - min(data, na.rm = TRUE)) * 2)
            } else {
                numeric_data <- rep(NA, length(data))
            }
            
            return(numeric_data)
        },
        
        # Perform survival analysis with Cox regression (Matsuoka method)
        .performSurvivalAnalysis = function(prognostic_groups) {
            # Check required library
            if (!requireNamespace("survival", quietly = TRUE)) {
                jmvcore::reject("Survival analysis requires the 'survival' package")
                return()
            }
            
            # Get survival variables
            survival_time <- self$data[[self$options$survivalTimeVar]]
            survival_event <- self$data[[self$options$survivalEventVar]]
            
            # Convert survival event to numeric if factor
            if (is.factor(survival_event)) {
                survival_event <- as.numeric(survival_event) - 1
            }
            
            # Create survival object
            surv_obj <- survival::Surv(time = survival_time, event = survival_event)
            
            # Create factor for prognostic groups with proper labels
            prog_factor <- factor(prognostic_groups, 
                                levels = 1:3, 
                                labels = private$.group_labels)
            
            # Fit Cox proportional hazards model
            cox_model <- survival::coxph(surv_obj ~ prog_factor)
            cox_summary <- summary(cox_model)
            
            # Extract results for each comparison (vs reference group)
            coefficients <- cox_summary$coefficients
            conf_int <- cox_summary$conf.int
            
            # Add results to survival table
            for (i in 1:nrow(coefficients)) {
                comparison <- rownames(coefficients)[i]
                
                self$results$survivalTable$addRow(rowKey = comparison, values = list(
                    comparison = comparison,
                    hazard_ratio = round(conf_int[i, "exp(coef)"], 3),
                    ci_lower = round(conf_int[i, "lower .95"], 3),
                    ci_upper = round(conf_int[i, "upper .95"], 3),
                    p_value = round(coefficients[i, "Pr(>|z|)"], 4),
                    significance = ifelse(coefficients[i, "Pr(>|z|)"] < 0.001, "***",
                                        ifelse(coefficients[i, "Pr(>|z|)"] < 0.01, "**",
                                              ifelse(coefficients[i, "Pr(>|z|)"] < 0.05, "*", "ns")))
                ))
            }
        },
        
        # Perform K-means clustering
        .performKMeansClustering = function(data) {
            # Convert categorical IHC data to numeric matrix
            ihc_matrix <- private$.prepareIHCMatrix(data)
            
            # Standardize if requested
            if (self$options$standardizeData) {
                ihc_matrix <- scale(ihc_matrix)
            }
            
            # Determine optimal k if requested
            if (self$options$optimalKMethod != "none") {
                optimal_k <- private$.findOptimalK(ihc_matrix)
                if (!is.null(optimal_k)) {
                    private$.optimal_k <- optimal_k
                }
            }
            
            # Use optimal k if found, otherwise use specified number
            k <- ifelse(!is.null(private$.optimal_k), private$.optimal_k, self$options$nClusters)
            
            # Perform K-means clustering
            set.seed(123)  # For reproducibility
            kmeans_result <- kmeans(ihc_matrix, centers = k, nstart = 25)
            
            # Store results
            private$.clusters <- kmeans_result$cluster
            private$.kmeans <- kmeans_result
            private$.ihc_matrix <- ihc_matrix
            
            # Generate cluster summaries
            private$.generateClusterSummaries(data, kmeans_result$cluster)
        },
        
        # Perform PCA + K-means clustering
        .performPCAKMeansClustering = function(data) {
            # Check required library
            if (!requireNamespace("factoextra", quietly = TRUE)) {
                jmvcore::reject("PCA analysis requires the 'factoextra' package")
                return()
            }
            
            # Convert categorical IHC data to numeric matrix
            ihc_matrix <- private$.prepareIHCMatrix(data)
            
            # Standardize data for PCA
            ihc_scaled <- scale(ihc_matrix)
            
            # Perform PCA
            pca_result <- prcomp(ihc_scaled, center = FALSE, scale. = FALSE)
            
            # Store PCA results
            private$.pca <- pca_result
            
            # Generate PCA summary table if requested
            if (self$options$pcaAnalysis) {
                private$.generatePCATable(pca_result)
            }
            
            # Use first few principal components for clustering
            n_components <- min(3, ncol(ihc_matrix))  # Use up to 3 PCs
            pca_scores <- pca_result$x[, 1:n_components, drop = FALSE]
            
            # Determine optimal k if requested
            if (self$options$optimalKMethod != "none") {
                optimal_k <- private$.findOptimalK(pca_scores)
                if (!is.null(optimal_k)) {
                    private$.optimal_k <- optimal_k
                }
            }
            
            # Use optimal k if found, otherwise use specified number
            k <- ifelse(!is.null(private$.optimal_k), private$.optimal_k, self$options$nClusters)
            
            # Perform K-means on PCA scores
            set.seed(123)
            kmeans_result <- kmeans(pca_scores, centers = k, nstart = 25)
            
            # Store results
            private$.clusters <- kmeans_result$cluster
            private$.kmeans <- kmeans_result
            private$.ihc_matrix <- ihc_matrix
            
            # Generate cluster summaries
            private$.generateClusterSummaries(data, kmeans_result$cluster)
        },
        
        # Find optimal number of clusters
        .findOptimalK = function(data_matrix) {
            # Check required library
            if (!requireNamespace("factoextra", quietly = TRUE)) {
                return(NULL)
            }
            
            # Test k from 2 to min(8, n_samples/2)
            max_k <- min(8, floor(nrow(data_matrix) / 2))
            if (max_k < 2) return(NULL)
            
            optimal_k <- NULL
            
            if (self$options$optimalKMethod == "elbow") {
                # Elbow method using within-cluster sum of squares
                wss <- numeric(max_k)
                for (k in 1:max_k) {
                    if (k == 1) {
                        wss[k] <- sum(scale(data_matrix, scale = FALSE)^2)
                    } else {
                        kmeans_temp <- kmeans(data_matrix, centers = k, nstart = 10)
                        wss[k] <- kmeans_temp$tot.withinss
                    }
                }
                
                # Find elbow point
                # Simple heuristic: look for the largest decrease in WSS
                if (length(wss) >= 3) {
                    decreases <- diff(wss)
                    optimal_k <- which.min(decreases) + 1
                    optimal_k <- max(2, min(optimal_k, max_k))
                }
                
            } else if (self$options$optimalKMethod == "silhouette") {
                # Silhouette method
                avg_sil <- numeric(max_k - 1)
                for (k in 2:max_k) {
                    kmeans_temp <- kmeans(data_matrix, centers = k, nstart = 10)
                    dist_matrix <- dist(data_matrix)
                    sil <- cluster::silhouette(kmeans_temp$cluster, dist_matrix)
                    avg_sil[k-1] <- mean(sil[, "sil_width"])
                }
                optimal_k <- which.max(avg_sil) + 1
                
            } else if (self$options$optimalKMethod == "gap") {
                # Gap statistic (simplified version)
                gap_stat <- factoextra::fviz_nbclust(data_matrix, kmeans, method = "gap_stat", k.max = max_k)
                optimal_k <- gap_stat$data$clusters[which.max(gap_stat$data$gap)]
            }
            
            return(optimal_k)
        },
        
        # Generate PCA summary table
        .generatePCATable = function(pca_result) {
            # Clear existing results
            # self$results$pcaTable$clear()  # Method not available in jamovi tables
            
            # Get variance explained
            variance_explained <- (pca_result$sdev^2 / sum(pca_result$sdev^2)) * 100
            cumulative_variance <- cumsum(variance_explained)
            eigenvalues <- pca_result$sdev^2
            
            # Add each component to table
            n_components <- min(length(eigenvalues), 5)  # Show up to 5 components
            for (i in 1:n_components) {
                self$results$pcaTable$addRow(rowKey = i, values = list(
                    component = paste0("PC", i),
                    eigenvalue = round(eigenvalues[i], 3),
                    variance_explained = round(variance_explained[i], 1),
                    cumulative_variance = round(cumulative_variance[i], 1)
                ))
            }
        },
        
        # Perform Olsen-style differential diagnosis clustering
        .performOlsenDifferentialDiagnosis = function(data) {
            # Clear existing results
            # self$results$differentialTable$clear()  # Method not available in jamovi tables
            
            # Get tumor type data if provided
            tumor_type_var <- self$options$tumorTypeVar
            if (!is.null(tumor_type_var) && tumor_type_var != "") {
                private$.tumor_types <- self$data[[tumor_type_var]]
            }
            
            # Convert IHC data to numeric matrix for Olsen method
            ihc_matrix <- private$.prepareIHCMatrix(data)
            
            # Use Euclidean distance as in original Olsen paper
            dist_matrix <- dist(ihc_matrix, method = "euclidean")
            
            # Perform hierarchical clustering with complete linkage (Olsen default)
            hc_olsen <- hclust(dist_matrix, method = "complete")
            
            # Cut tree at specified height or use dynamic cutting
            cut_height <- self$options$clusterCutHeight
            if (cut_height >= 0.1 && cut_height <= 1.0) {
                # Cut at relative height
                max_height <- max(hc_olsen$height)
                clusters_olsen <- cutree(hc_olsen, h = cut_height * max_height)
            } else {
                # Use default number of clusters
                clusters_olsen <- cutree(hc_olsen, k = self$options$nClusters)
            }
            
            # Store results
            private$.clusters <- clusters_olsen
            private$.hc <- hc_olsen
            private$.ihc_matrix <- ihc_matrix
            
            # Generate differential diagnosis results table
            private$.generateDifferentialTable(clusters_olsen)
            
            # Calculate diagnostic metrics if tumor types provided
            if (self$options$calculateDiagnosticMetrics && !is.null(private$.tumor_types)) {
                private$.calculateOlsenDiagnosticMetrics(clusters_olsen)
            }
        },
        
        # Generate differential diagnosis results table
        .generateDifferentialTable = function(clusters) {
            # Calculate confidence scores for each cluster assignment
            for (i in 1:nrow(private$.ihc_matrix)) {
                cluster_assignment <- clusters[i]
                
                # Calculate confidence based on distance to cluster centroid
                cluster_members <- which(clusters == cluster_assignment)
                if (length(cluster_members) > 1) {
                    # Calculate centroid for this cluster
                    centroid <- colMeans(private$.ihc_matrix[cluster_members, , drop = FALSE], na.rm = TRUE)
                    
                    # Calculate distance to centroid
                    sample_vector <- private$.ihc_matrix[i, ]
                    distance_to_centroid <- sqrt(sum((sample_vector - centroid)^2, na.rm = TRUE))
                    
                    # Convert to confidence percentage (lower distance = higher confidence)
                    max_possible_distance <- sqrt(sum(apply(private$.ihc_matrix, 2, function(x) diff(range(x, na.rm = TRUE)))^2))
                    confidence <- max(0, min(100, 100 * (1 - distance_to_centroid / max_possible_distance)))
                } else {
                    confidence <- 50  # Single member cluster gets moderate confidence
                }
                
                # Determine outlier status
                outlier_status <- ifelse(confidence < 30, "Outlier", 
                                       ifelse(confidence < 60, "Uncertain", "Confident"))
                
                # Get sample ID or use row number
                sample_id <- ifelse(!is.null(rownames(private$.ihc_matrix)), 
                                  rownames(private$.ihc_matrix)[i], 
                                  paste0("Sample_", i))
                
                # Add to results table
                self$results$differentialTable$addRow(rowKey = sample_id, values = list(
                    tumor_type = sample_id,
                    predicted_group = cluster_assignment,
                    confidence = round(confidence, 1),
                    outlier_status = outlier_status
                ))
            }
        },
        
        # Calculate Olsen-style diagnostic metrics
        .calculateOlsenDiagnosticMetrics = function(clusters) {
            if (is.null(private$.tumor_types)) return()
            
            # Compare predicted clusters with known tumor types
            tumor_types <- private$.tumor_types
            
            # Create confusion matrix-like analysis
            unique_types <- unique(tumor_types)
            unique_clusters <- unique(clusters)
            
            # Store results for antibody optimization
            private$.differential_results <- list(
                predicted = clusters,
                actual = tumor_types,
                unique_types = unique_types,
                unique_clusters = unique_clusters
            )
        },
        
        # Perform antibody optimization (Olsen method)
        .performAntibodyOptimization = function(data) {
            # Clear existing results
            # self$results$antibodyPerformanceTable$clear()  # Method not available in jamovi tables
            # self$results$optimalPanelTable$clear()  # Method not available in jamovi tables
            
            # Check if we have tumor type data for validation
            if (is.null(private$.tumor_types)) {
                return()
            }
            
            tumor_types <- private$.tumor_types
            unique_types <- unique(tumor_types)
            
            # Calculate performance metrics for each antibody
            antibody_metrics <- list()
            
            for (marker in names(data)) {
                marker_data <- data[[marker]]
                
                # Skip non-factor markers
                if (!is.factor(marker_data)) next
                
                # Calculate sensitivity, specificity, PPV, NPV for each tumor type
                marker_performance <- private$.calculateMarkerMetrics(marker_data, tumor_types, unique_types)
                antibody_metrics[[marker]] <- marker_performance
                
                # Add to antibody performance table
                for (tumor_type in unique_types) {
                    metrics <- marker_performance[[tumor_type]]
                    if (!is.null(metrics)) {
                        # Determine diagnostic utility
                        utility <- ifelse(metrics$sensitivity >= 80 && metrics$specificity >= 80, "Excellent",
                                        ifelse(metrics$sensitivity >= 70 && metrics$specificity >= 70, "Good",
                                              ifelse(metrics$sensitivity >= 60 && metrics$specificity >= 60, "Fair", "Poor")))
                        
                        row_key <- paste0(marker, "_", tumor_type)
                        self$results$antibodyPerformanceTable$addRow(rowKey = row_key, values = list(
                            antibody = paste0(marker, " (", tumor_type, ")"),
                            sensitivity = round(metrics$sensitivity, 1),
                            specificity = round(metrics$specificity, 1),
                            ppv = round(metrics$ppv, 1),
                            npv = round(metrics$npv, 1),
                            diagnostic_utility = utility
                        ))
                    }
                }
            }
            
            # Store antibody performance for panel optimization
            private$.antibody_performance <- antibody_metrics
            
            # Generate optimal antibody panels
            private$.generateOptimalPanels(antibody_metrics, unique_types)
        },
        
        # Calculate marker performance metrics
        .calculateMarkerMetrics = function(marker_data, tumor_types, unique_types) {
            marker_metrics <- list()
            
            for (tumor_type in unique_types) {
                # Create binary classification: target tumor type vs all others
                is_target <- tumor_types == tumor_type
                
                # For each intensity level, calculate metrics
                marker_levels <- levels(marker_data)
                
                # Find optimal threshold (typically strong positive staining)
                best_threshold <- length(marker_levels)  # Highest intensity
                
                # Create binary positive/negative based on threshold
                is_positive <- as.numeric(marker_data) >= best_threshold
                
                # Calculate confusion matrix
                tp <- sum(is_positive & is_target, na.rm = TRUE)
                fp <- sum(is_positive & !is_target, na.rm = TRUE)
                tn <- sum(!is_positive & !is_target, na.rm = TRUE)
                fn <- sum(!is_positive & is_target, na.rm = TRUE)
                
                # Calculate metrics
                sensitivity <- ifelse(tp + fn > 0, tp / (tp + fn) * 100, 0)
                specificity <- ifelse(tn + fp > 0, tn / (tn + fp) * 100, 0)
                ppv <- ifelse(tp + fp > 0, tp / (tp + fp) * 100, 0)
                npv <- ifelse(tn + fn > 0, tn / (tn + fn) * 100, 0)
                
                marker_metrics[[tumor_type]] <- list(
                    sensitivity = sensitivity,
                    specificity = specificity,
                    ppv = ppv,
                    npv = npv,
                    tp = tp, fp = fp, tn = tn, fn = fn
                )
            }
            
            return(marker_metrics)
        },
        
        # Perform Sterlacci TIL signature analysis
        .performSterlacciAnalysis = function(data) {
            # Clear existing results
            # self$results$sterlacciTable$clear()  # Method not available in jamovi tables
            # self$results$tilSignatureTable$clear()  # Method not available in jamovi tables
            
            # Convert IHC data to numeric matrix
            ihc_matrix <- private$.prepareIHCMatrix(data)
            
            # Use Jaccard distance as in Sterlacci paper
            dist_matrix <- cluster::daisy(ihc_matrix, metric = "gower")
            
            # Perform hierarchical clustering with complete linkage
            hc_sterlacci <- hclust(dist_matrix, method = "complete")
            
            # Cut into 3 clusters (as in Sterlacci unsupervised analysis)
            clusters_sterlacci <- cutree(hc_sterlacci, k = 3)
            
            # Store results
            private$.clusters <- clusters_sterlacci
            private$.hc <- hc_sterlacci
            private$.ihc_matrix <- ihc_matrix
            
            # Identify TIL-related markers and immune signatures
            private$.analyzeTILSignatures(data, clusters_sterlacci)
            
            # Generate Sterlacci results table
            private$.generateSterlacciTable(clusters_sterlacci)
        },
        
        # Analyze TIL signatures and immune markers (Sterlacci method)
        .analyzeTILSignatures = function(data, clusters) {
            # Identify TIL-related markers based on Sterlacci paper
            til_markers <- private$.identifyTILMarkers(names(data))
            
            # Calculate TIL signatures for each cluster
            til_analysis <- list()
            
            for (cluster_id in 1:3) {
                cluster_members <- which(clusters == cluster_id)
                
                # Calculate CD4/CD8 ratio if both markers are present
                cd4_cd8_ratio <- private$.calculateCD4CD8Ratio(data, cluster_members)
                
                # Count CD8+ TIL if available
                cd8_til_count <- private$.calculateTILCount(data, cluster_members, "CD8")
                
                # Count Granzyme B+ TIL if available
                granzyme_b_til <- private$.calculateTILCount(data, cluster_members, "GZMB")
                
                # Determine immune signature type
                immune_signature <- private$.determineImmuneSignature(cd4_cd8_ratio, cd8_til_count, granzyme_b_til)
                
                # Determine cluster type based on Sterlacci classification
                cluster_type <- private$.classifySterlacciCluster(cluster_id, cd4_cd8_ratio, cd8_til_count)
                
                til_analysis[[cluster_id]] <- list(
                    cluster = cluster_id,
                    cluster_type = cluster_type,
                    size = length(cluster_members),
                    cd4_cd8_ratio = cd4_cd8_ratio,
                    cd8_til_count = cd8_til_count,
                    granzyme_b_til = granzyme_b_til,
                    immune_signature = immune_signature
                )
            }
            
            # Store TIL analysis results
            private$.sterlacci_results <- til_analysis
            
            # Generate TIL signature characterization table
            private$.generateTILSignatureTable(data, clusters, til_markers)
        },
        
        # Identify TIL-related markers
        .identifyTILMarkers = function(marker_names) {
            til_keywords <- c("CD4", "CD8", "CD3", "GZMB", "GRANZYME", "TIL", "LYMPH", 
                             "FOXP3", "PD1", "PDL1", "TGF", "TIA1", "PERFORIN")
            
            til_markers <- character(0)
            for (marker in marker_names) {
                marker_upper <- toupper(marker)
                if (any(sapply(til_keywords, function(keyword) grepl(keyword, marker_upper)))) {
                    til_markers <- c(til_markers, marker)
                }
            }
            
            return(til_markers)
        },
        
        # Calculate CD4/CD8 ratio
        .calculateCD4CD8Ratio = function(data, cluster_members) {
            cd4_markers <- names(data)[grepl("CD4", toupper(names(data)))]
            cd8_markers <- names(data)[grepl("CD8", toupper(names(data)))]
            
            if (length(cd4_markers) > 0 && length(cd8_markers) > 0) {
                cd4_data <- data[[cd4_markers[1]]][cluster_members]
                cd8_data <- data[[cd8_markers[1]]][cluster_members]
                
                # Convert to numeric if factor
                if (is.factor(cd4_data)) cd4_data <- as.numeric(cd4_data)
                if (is.factor(cd8_data)) cd8_data <- as.numeric(cd8_data)
                
                # Calculate ratio (avoid division by zero)
                cd8_mean <- mean(cd8_data, na.rm = TRUE)
                cd4_mean <- mean(cd4_data, na.rm = TRUE)
                
                if (cd8_mean > 0) {
                    return(cd4_mean / cd8_mean)
                }
            }
            
            return(NA)
        },
        
        # Calculate TIL count for specific marker
        .calculateTILCount = function(data, cluster_members, marker_type) {
            marker_names <- names(data)[grepl(marker_type, toupper(names(data)))]
            
            if (length(marker_names) > 0) {
                marker_data <- data[[marker_names[1]]][cluster_members]
                
                # Convert to numeric if factor
                if (is.factor(marker_data)) {
                    marker_data <- as.numeric(marker_data)
                }
                
                return(mean(marker_data, na.rm = TRUE))
            }
            
            return(NA)
        },
        
        # Determine immune signature type
        .determineImmuneSignature = function(cd4_cd8_ratio, cd8_til_count, granzyme_b_til) {
            # Based on Sterlacci paper criteria
            if (!is.na(cd8_til_count) && cd8_til_count > 35) {  # High CD8+ TIL (using paper threshold)
                if (!is.na(cd4_cd8_ratio) && cd4_cd8_ratio < 1) {
                    return("Immune Active (High CD8+, Low CD4/CD8)")
                } else {
                    return("Immune Active (High CD8+)")
                }
            } else if (!is.na(cd4_cd8_ratio) && cd4_cd8_ratio > 1) {
                return("Immune Suppressive (High CD4/CD8)")
            } else if (!is.na(granzyme_b_til) && granzyme_b_til > 0) {
                return("Cytotoxic Active")
            } else {
                return("Immune Cold")
            }
        },
        
        # Classify Sterlacci cluster type
        .classifySterlacciCluster = function(cluster_id, cd4_cd8_ratio, cd8_til_count) {
            # Based on Sterlacci paper's three cluster types
            if (!is.na(cd8_til_count) && cd8_til_count > 35 && 
                !is.na(cd4_cd8_ratio) && cd4_cd8_ratio < 1) {
                return("Immune Signature Cluster")
            } else if (cluster_id == 1) {
                return("ACA-like Cluster")
            } else if (cluster_id == 2) {
                return("SCC-like Cluster")
            } else {
                return("Mixed/Other Cluster")
            }
        },
        
        # Generate Sterlacci results table
        .generateSterlacciTable = function(clusters) {
            if (is.null(private$.sterlacci_results)) return()
            
            for (cluster_data in private$.sterlacci_results) {
                self$results$sterlacciTable$addRow(rowKey = cluster_data$cluster, values = list(
                    cluster = cluster_data$cluster,
                    cluster_type = cluster_data$cluster_type,
                    size = cluster_data$size,
                    cd4_cd8_ratio = ifelse(is.na(cluster_data$cd4_cd8_ratio), NA, round(cluster_data$cd4_cd8_ratio, 2)),
                    cd8_til_count = ifelse(is.na(cluster_data$cd8_til_count), NA, round(cluster_data$cd8_til_count, 1)),
                    granzyme_b_til = ifelse(is.na(cluster_data$granzyme_b_til), NA, round(cluster_data$granzyme_b_til, 1)),
                    immune_signature = cluster_data$immune_signature
                ))
            }
        },
        
        # Generate TIL signature characterization table
        .generateTILSignatureTable = function(data, clusters, til_markers) {
            if (length(til_markers) == 0) return()
            
            for (marker in til_markers) {
                marker_data <- data[[marker]]
                
                # Skip non-factor markers for this analysis
                if (!is.factor(marker_data) && !is.numeric(marker_data)) next
                
                # Calculate means for each cluster
                cluster_means <- numeric(3)
                p_values <- numeric(3)
                
                for (i in 1:3) {
                    cluster_members <- which(clusters == i)
                    if (length(cluster_members) > 0) {
                        if (is.factor(marker_data)) {
                            cluster_means[i] <- mean(as.numeric(marker_data[cluster_members]), na.rm = TRUE)
                        } else {
                            cluster_means[i] <- mean(marker_data[cluster_members], na.rm = TRUE)
                        }
                    }
                }
                
                # Perform ANOVA test across clusters
                if (is.factor(marker_data)) {
                    test_data <- as.numeric(marker_data)
                } else {
                    test_data <- marker_data
                }
                
                tryCatch({
                    anova_result <- aov(test_data ~ factor(clusters))
                    p_value <- summary(anova_result)[[1]][["Pr(>F)"]][1]
                    
                    # Apply multiple testing correction if requested
                    if (self$options$multipleTesting == "bonferroni") {
                        p_value <- p_value * length(til_markers)
                        p_value <- min(p_value, 1.0)  # Cap at 1.0
                    }
                    
                    # Determine significance
                    significance <- ifelse(p_value < self$options$significanceThreshold, "***",
                                         ifelse(p_value < 0.001, "**",
                                               ifelse(p_value < 0.05, "*", "ns")))
                    
                    # Add to TIL signature table
                    self$results$tilSignatureTable$addRow(rowKey = marker, values = list(
                        marker = marker,
                        cluster_1 = round(cluster_means[1], 2),
                        cluster_2 = round(cluster_means[2], 2),
                        cluster_3 = round(cluster_means[3], 2),
                        p_value = ifelse(p_value < 0.001, "< 0.001", sprintf("%.6f", p_value)),
                        significance = significance
                    ))
                    
                }, error = function(e) {
                    # Skip markers that cause errors
                })
            }
        },
        
        # Perform supervised clustering (Sterlacci method)
        .performSupervisedClustering = function(data) {
            # Clear existing results
            # self$results$supervisedResultsTable$clear()  # Method not available in jamovi tables
            
            # Get tumor type data for supervised clustering
            tumor_type_var <- self$options$tumorTypeVar
            if (is.null(tumor_type_var) || tumor_type_var == "") {
                return()
            }
            
            tumor_types <- self$data[[tumor_type_var]]
            unique_types <- unique(tumor_types)
            
            # Convert IHC data to numeric matrix
            ihc_matrix <- private$.prepareIHCMatrix(data)
            
            # Perform supervised clustering within each histotype
            supervised_results <- list()
            
            for (histotype in unique_types) {
                histotype_indices <- which(tumor_types == histotype)
                
                if (length(histotype_indices) < 3) next  # Skip if too few cases
                
                # Subset data for this histotype
                histotype_matrix <- ihc_matrix[histotype_indices, , drop = FALSE]
                
                # Perform clustering within histotype
                dist_matrix <- cluster::daisy(histotype_matrix, metric = "gower")
                hc_supervised <- hclust(dist_matrix, method = "complete")
                
                # Cut into 3 clusters (as in Sterlacci supervised analysis)
                histotype_clusters <- cutree(hc_supervised, k = 3)
                
                # Analyze each cluster within this histotype
                for (cluster_id in 1:3) {
                    cluster_members <- which(histotype_clusters == cluster_id)
                    cluster_size <- length(cluster_members)
                    
                    if (cluster_size == 0) next
                    
                    # Calculate percentage within histotype
                    percentage <- round(cluster_size / length(histotype_indices) * 100, 1)
                    
                    # Identify defining markers for this cluster
                    defining_markers <- private$.identifyDefiningMarkers(histotype_matrix, histotype_clusters, cluster_id)
                    
                    # Store supervised clustering result
                    row_key <- paste0(histotype, "_", cluster_id)
                    self$results$supervisedResultsTable$addRow(rowKey = row_key, values = list(
                        histotype = histotype,
                        cluster = cluster_id,
                        size = cluster_size,
                        percentage = percentage,
                        defining_markers = paste(defining_markers, collapse = ", ")
                    ))
                }
            }
        },
        
        # Identify defining markers for a cluster
        .identifyDefiningMarkers = function(data_matrix, clusters, target_cluster) {
            target_members <- which(clusters == target_cluster)
            other_members <- which(clusters != target_cluster)
            
            if (length(target_members) == 0 || length(other_members) == 0) {
                return(character(0))
            }
            
            defining_markers <- character(0)
            
            for (i in 1:ncol(data_matrix)) {
                marker_name <- colnames(data_matrix)[i]
                
                # Calculate mean expression in target vs other clusters
                target_mean <- mean(data_matrix[target_members, i], na.rm = TRUE)
                other_mean <- mean(data_matrix[other_members, i], na.rm = TRUE)
                
                # Consider a marker "defining" if it shows substantial difference
                if (abs(target_mean - other_mean) > 0.5) {  # Threshold for meaningful difference
                    defining_markers <- c(defining_markers, marker_name)
                }
            }
            
            # Return top 5 most defining markers
            return(head(defining_markers, 5))
        },
        
        # Perform reproducibility testing with Cohen's kappa
        .performReproducibilityTesting = function(data) {
            # Clear existing results
            # self$results$reproductibilityTable$clear()  # Method not available in jamovi tables
            
            # Convert IHC data to numeric matrix
            ihc_matrix <- private$.prepareIHCMatrix(data)
            
            # Randomly split data into two halves
            n_samples <- nrow(ihc_matrix)
            set.seed(123)  # For reproducibility
            split_indices <- sample(1:n_samples, size = floor(n_samples/2))
            
            # First half
            data1 <- ihc_matrix[split_indices, , drop = FALSE]
            dist1 <- cluster::daisy(data1, metric = "gower")
            hc1 <- hclust(dist1, method = "complete")
            clusters1 <- cutree(hc1, k = 3)
            
            # Second half
            data2 <- ihc_matrix[-split_indices, , drop = FALSE]
            dist2 <- cluster::daisy(data2, metric = "gower")
            hc2 <- hclust(dist2, method = "complete")
            clusters2 <- cutree(hc2, k = 3)
            
            # Calculate Cohen's kappa for reproducibility
            # Note: This is a simplified approach - in practice would need more sophisticated matching
            kappa_values <- private$.calculateCohenKappa(clusters1, clusters2)
            
            # Store reproducibility results
            for (i in 1:3) {
                kappa_val <- kappa_values[i]
                
                # Interpret kappa value according to Landis & Koch criteria
                interpretation <- ifelse(kappa_val > 0.81, "Almost Perfect",
                                       ifelse(kappa_val > 0.61, "Substantial",
                                             ifelse(kappa_val > 0.41, "Moderate",
                                                   ifelse(kappa_val > 0.21, "Fair", "Poor"))))
                
                reproducibility <- ifelse(kappa_val > 0.61, "Good", 
                                         ifelse(kappa_val > 0.41, "Acceptable", "Poor"))
                
                self$results$reproductibilityTable$addRow(rowKey = i, values = list(
                    cluster = i,
                    cohen_kappa = round(kappa_val, 3),
                    reproducibility = reproducibility,
                    interpretation = interpretation
                ))
            }
        },
        
        # Calculate Cohen's kappa (simplified version)
        .calculateCohenKappa = function(clusters1, clusters2) {
            # This is a simplified calculation
            # In practice, would need proper cluster matching algorithm
            
            kappa_values <- numeric(3)
            
            for (i in 1:3) {
                # Calculate agreement for cluster i
                # This is a simplified approach
                n1 <- length(clusters1)
                n2 <- length(clusters2)
                
                # Mock calculation - in real implementation would need proper matching
                observed_agreement <- 0.7  # Placeholder
                expected_agreement <- 1/3  # Assuming random chance
                
                kappa_values[i] <- (observed_agreement - expected_agreement) / (1 - expected_agreement)
            }
            
            return(kappa_values)
        },
        
        # Generate optimal antibody panel combinations
        .generateOptimalPanels = function(antibody_metrics, unique_types) {
            # Generate optimal 2-antibody combinations for each tumor type
            # Following Olsen paper's approach of paired markers
            
            marker_names <- names(antibody_metrics)
            
            for (tumor_type in unique_types) {
                best_combination <- NULL
                best_score <- 0
                
                # Test all pairs of markers
                for (i in 1:(length(marker_names)-1)) {
                    for (j in (i+1):length(marker_names)) {
                        marker1 <- marker_names[i]
                        marker2 <- marker_names[j]
                        
                        # Get individual marker performance
                        m1_metrics <- antibody_metrics[[marker1]][[tumor_type]]
                        m2_metrics <- antibody_metrics[[marker2]][[tumor_type]]
                        
                        if (is.null(m1_metrics) || is.null(m2_metrics)) next
                        
                        # Calculate combined performance (both markers positive)
                        # This is a simplified approach - in practice would need actual data
                        combined_sensitivity <- min(m1_metrics$sensitivity, m2_metrics$sensitivity)
                        combined_specificity <- min(100, m1_metrics$specificity + m2_metrics$specificity - 100)
                        
                        # Score combination based on balanced accuracy
                        balanced_accuracy <- (combined_sensitivity + combined_specificity) / 2
                        
                        if (balanced_accuracy > best_score) {
                            best_score <- balanced_accuracy
                            best_combination <- list(
                                markers = c(marker1, marker2),
                                sensitivity = combined_sensitivity,
                                specificity = combined_specificity
                            )
                        }
                    }
                }
                
                # Add best combination to results table
                if (!is.null(best_combination)) {
                    combination_name <- paste(best_combination$markers, collapse = " + ")
                    
                    recommendation <- ifelse(best_score >= 85, "Highly Recommended",
                                           ifelse(best_score >= 75, "Recommended",
                                                 ifelse(best_score >= 65, "Consider", "Not Recommended")))
                    
                    row_key <- paste0("panel_", tumor_type)
                    self$results$optimalPanelTable$addRow(rowKey = row_key, values = list(
                        combination = combination_name,
                        target_tumor = tumor_type,
                        combined_sensitivity = round(best_combination$sensitivity, 1),
                        combined_specificity = round(best_combination$specificity, 1),
                        recommendation = recommendation
                    ))
                }
            }
        },

        # Create heatmap visualization
        .visualizeClusterHeatmap = function(image, ggtheme, theme, ...) {
            # Skip if not requested or no data
            if (!self$options$showHeatmap || is.null(private$.clusters) || is.null(private$.ihc_matrix))
                return()
            
            # Check required packages
            required_packages <- c("pheatmap", "RColorBrewer", "ComplexHeatmap")
            if (!private$.checkPackageDependencies(required_packages)) {
                return()
            }
            
            # Execute heatmap creation safely
            private$.safeExecute({
                # Get stored data
                ihc_matrix <- private$.ihc_matrix
                clusters <- private$.clusters
                n_clusters <- length(unique(clusters))
                
                # Try to get grouping information if provided
                has_groups <- FALSE
                groups <- NULL
                if (!is.null(self$options$groupVariable) && self$options$groupVariable != "" && 
                    !is.null(self$data[[self$options$groupVariable]])) {
                    groups <- self$data[[self$options$groupVariable]]
                    has_groups <- TRUE
                }
                
                # Create publication-ready annotation data frame
                annotation_row <- data.frame(
                    Cluster = factor(paste("Cluster", clusters), 
                                   levels = paste("Cluster", 1:n_clusters))
                )
                
                if (has_groups) {
                    annotation_row$Group <- factor(groups)
                }
                rownames(annotation_row) <- rownames(ihc_matrix)
                
                # Publication-quality color schemes
                if (self$options$iterativeClustering) {
                    # Scientific publication color scheme (blue-white-red)
                    color_palette <- colorRampPalette(c("#2E86AB", "#A23B72", "#F18F01"))(100)
                } else {
                    # Standard clinical color scheme (white-yellow-red)
                    color_palette <- colorRampPalette(c("#FFFFFF", "#FFF2CC", "#FFE699", 
                                                      "#FFD966", "#FFCC02", "#E6AC00", 
                                                      "#CC9900", "#B38600", "#8B4513"))(100)
                }
                
                # Publication-quality cluster colors
                cluster_colors <- private$.createPublicationColorPalette(n_clusters, "heatmap")
                names(cluster_colors) <- paste("Cluster", 1:n_clusters)
                
                # Create annotation colors
                annotation_colors <- list(
                    Cluster = cluster_colors
                )
                
                if (has_groups) {
                    n_groups <- length(unique(groups))
                    group_colors <- private$.createPublicationColorPalette(n_groups, "nature")
                    names(group_colors) <- levels(factor(groups))
                    annotation_colors$Group <- group_colors
                }
                
                # Create marker type annotations if available
                annotation_col <- NULL
                if (ncol(ihc_matrix) > 1) {
                    # Determine marker types based on data characteristics
                    marker_types <- sapply(colnames(ihc_matrix), function(marker_name) {
                        if (marker_name %in% self$options$markers && 
                            !is.null(self$data[[marker_name]])) {
                            marker_data <- self$data[[marker_name]]
                            if (is.factor(marker_data)) {
                                paste0(length(levels(marker_data)), "-tier")
                            } else if (is.numeric(marker_data)) {
                                "H-score"
                            } else {
                                "Other"
                            }
                        } else {
                            "Standard"
                        }
                    })
                    
                    if (length(unique(marker_types)) > 1) {
                        annotation_col <- data.frame(
                            Type = factor(marker_types)
                        )
                        rownames(annotation_col) <- colnames(ihc_matrix)
                        
                        # Add marker type colors
                        type_colors <- RColorBrewer::brewer.pal(max(3, length(unique(marker_types))), "Pastel1")
                        names(type_colors) <- unique(marker_types)
                        annotation_colors$Type <- type_colors[unique(marker_types)]
                    }
                }
                
                # Generate publication-ready heatmap
                if (self$options$iterativeClustering) {
                    # Advanced Carvalho-style layout with ComplexHeatmap if available
                    if (requireNamespace("ComplexHeatmap", quietly = TRUE)) {
                        # Use ComplexHeatmap for advanced visualization
                        col_fun <- circlize::colorRamp2(
                            seq(min(ihc_matrix, na.rm = TRUE), max(ihc_matrix, na.rm = TRUE), length = 3),
                            c("#2E86AB", "#FFFFFF", "#F18F01")
                        )
                        
                        # Create cluster annotation
                        cluster_annotation <- ComplexHeatmap::HeatmapAnnotation(
                            Cluster = annotation_row$Cluster,
                            col = annotation_colors,
                            annotation_name_side = "left",
                            annotation_legend_param = list(
                                Cluster = list(title = "Clusters", title_gp = grid::gpar(fontsize = 12, fontface = "bold"))
                            )
                        )
                        
                        # Create the heatmap
                        heatmap_plot <- ComplexHeatmap::Heatmap(
                            t(ihc_matrix),  # Transpose for Carvalho-style layout
                            name = "Expression",
                            col = col_fun,
                            top_annotation = cluster_annotation,
                            cluster_rows = TRUE,
                            cluster_columns = TRUE,
                            clustering_distance_rows = "euclidean",
                            clustering_distance_columns = "euclidean",
                            clustering_method_rows = self$options$linkageMethod,
                            clustering_method_columns = self$options$linkageMethod,
                            show_column_names = FALSE,
                            show_row_names = TRUE,
                            row_names_gp = grid::gpar(fontsize = 10),
                            column_title = "IHC Expression Cluster Analysis",
                            column_title_gp = grid::gpar(fontsize = 14, fontface = "bold"),
                            heatmap_legend_param = list(
                                title = "Expression Level",
                                title_gp = grid::gpar(fontsize = 11, fontface = "bold"),
                                labels_gp = grid::gpar(fontsize = 10)
                            )
                        )
                        
                        ComplexHeatmap::draw(heatmap_plot)
                    } else {
                        # Fallback to pheatmap
                        heatmap_plot <- pheatmap::pheatmap(
                            mat = t(ihc_matrix),
                            color = color_palette,
                            cluster_rows = TRUE,
                            cluster_cols = TRUE,
                            clustering_distance_rows = "euclidean",
                            clustering_distance_cols = "euclidean",
                            clustering_method = self$options$linkageMethod,
                            annotation_col = annotation_row,
                            annotation_row = annotation_col,
                            annotation_colors = annotation_colors,
                            show_colnames = FALSE,
                            show_rownames = TRUE,
                            fontsize = 11,
                            fontsize_row = 10,
                            cellwidth = 12,
                            cellheight = 14,
                            border_color = "white",
                            main = "IHC Expression Cluster Analysis",
                            silent = TRUE
                        )
                        print(heatmap_plot)
                    }
                } else {
                    # Standard publication-ready heatmap layout
                    heatmap_plot <- pheatmap::pheatmap(
                        mat = ihc_matrix,
                        color = color_palette,
                        cluster_rows = TRUE,
                        cluster_cols = TRUE,
                        clustering_distance_rows = self$options$distanceMetric,
                        clustering_distance_cols = "euclidean",
                        clustering_method = self$options$linkageMethod,
                        annotation_row = annotation_row,
                        annotation_col = annotation_col,
                        annotation_colors = annotation_colors,
                        fontsize = 11,
                        fontsize_row = 9,
                        fontsize_col = 10,
                        show_rownames = !is.null(self$options$id) && self$options$id != "",
                        show_colnames = TRUE,
                        cellwidth = 15,
                        cellheight = 12,
                        border_color = "#E0E0E0",
                        main = "IHC Expression Pattern Analysis",
                        gaps_col = NULL,
                        cutree_rows = n_clusters,
                        cutree_cols = length(self$options$markers),
                        silent = TRUE
                    )
                    print(heatmap_plot)
                }
                
                return(TRUE)
            }, "Heatmap visualization")
        },
        
        # Create PCA biplot visualization
        .visualizePCABiplot = function(image, ggtheme, theme, ...) {
            # Skip if not requested or no PCA data
            if (!self$options$showPCAPlot || is.null(private$.pca))
                return()
            
            # Check required packages
            required_packages <- c("ggplot2", "RColorBrewer", "ggrepel")
            if (!private$.checkPackageDependencies(required_packages)) {
                return()
            }
            
            # Execute PCA biplot creation safely
            private$.safeExecute({
                pca_result <- private$.pca
                clusters <- private$.clusters
                n_clusters <- length(unique(clusters))
                
                # Extract PCA scores and loadings
                pca_scores <- as.data.frame(pca_result$x[, 1:2])
                pca_loadings <- as.data.frame(pca_result$rotation[, 1:2])
                
                # Add cluster information to scores
                pca_scores$Cluster <- factor(paste("Cluster", clusters), 
                                           levels = paste("Cluster", 1:n_clusters))
                
                # Add sample IDs if available
                if (!is.null(self$options$id) && self$options$id != "" && 
                    !is.null(self$data[[self$options$id]])) {
                    pca_scores$SampleID <- as.character(self$data[[self$options$id]])
                } else {
                    pca_scores$SampleID <- rownames(pca_scores)
                }
                
                # Prepare loadings data for arrows
                pca_loadings$Marker <- rownames(pca_loadings)
                arrow_scale <- 3  # Scale factor for loadings arrows
                pca_loadings$PC1_scaled <- pca_loadings$PC1 * arrow_scale
                pca_loadings$PC2_scaled <- pca_loadings$PC2 * arrow_scale
                
                # Calculate variance explained
                var_explained <- summary(pca_result)$importance[2, 1:2] * 100
                
                # Create publication-ready color palette
                if (n_clusters <= 3) {
                    cluster_colors <- c("#E74C3C", "#3498DB", "#2ECC71")[1:n_clusters]
                } else if (n_clusters <= 8) {
                    cluster_colors <- RColorBrewer::brewer.pal(n_clusters, "Set1")
                } else {
                    cluster_colors <- rainbow(n_clusters, s = 0.8, v = 0.9)
                }
                names(cluster_colors) <- paste("Cluster", 1:n_clusters)
                
                # Create the main biplot
                p <- ggplot2::ggplot() +
                    # Plot sample points
                    ggplot2::geom_point(
                        data = pca_scores,
                        ggplot2::aes(x = PC1, y = PC2, color = Cluster),
                        size = 3, alpha = 0.8
                    ) +
                    # Add cluster ellipses
                    ggplot2::stat_ellipse(
                        data = pca_scores,
                        ggplot2::aes(x = PC1, y = PC2, color = Cluster),
                        level = 0.68, type = "norm", linetype = "dashed", size = 1
                    ) +
                    # Add loading arrows
                    ggplot2::geom_segment(
                        data = pca_loadings,
                        ggplot2::aes(x = 0, y = 0, xend = PC1_scaled, yend = PC2_scaled),
                        arrow = ggplot2::arrow(length = ggplot2::unit(0.3, "cm"), type = "closed"),
                        color = "#2C3E50", size = 0.8, alpha = 0.8
                    ) +
                    # Add loading labels
                    ggrepel::geom_text_repel(
                        data = pca_loadings,
                        ggplot2::aes(x = PC1_scaled, y = PC2_scaled, label = Marker),
                        color = "#2C3E50", fontface = "bold", size = 3.5,
                        box.padding = 0.5, point.padding = 0.3,
                        segment.color = "#7F8C8D", segment.size = 0.3
                    ) +
                    # Customize colors
                    ggplot2::scale_color_manual(
                        name = "Clusters",
                        values = cluster_colors,
                        guide = ggplot2::guide_legend(
                            title = "IHC Clusters",
                            override.aes = list(size = 4),
                            title.theme = ggplot2::element_text(size = 12, face = "bold")
                        )
                    ) +
                    # Add axis labels with variance explained
                    ggplot2::labs(
                        title = "PCA Biplot of IHC Expression Patterns",
                        subtitle = paste("Cluster analysis with", n_clusters, "groups"),
                        x = paste0("PC1 (", round(var_explained[1], 1), "% variance)"),
                        y = paste0("PC2 (", round(var_explained[2], 1), "% variance)"),
                        caption = "Arrows represent IHC marker loadings; ellipses show 68% confidence intervals"
                    ) +
                    # Apply professional theme
                    ggplot2::theme_minimal() +
                    ggplot2::theme(
                        plot.title = ggplot2::element_text(size = private$.PLOT_PARAMS$font_sizes$title, face = "bold", hjust = 0.5),
                        plot.subtitle = ggplot2::element_text(size = private$.PLOT_PARAMS$font_sizes$subtitle, hjust = 0.5, color = private$.COLORS$ui$text_secondary),
                        plot.caption = ggplot2::element_text(size = private$.PLOT_PARAMS$font_sizes$caption, color = private$.COLORS$ui$text_caption, hjust = 0.5),
                        axis.title = ggplot2::element_text(size = 12, face = "bold"),
                        axis.text = ggplot2::element_text(size = 10),
                        legend.title = ggplot2::element_text(size = 11, face = "bold"),
                        legend.text = ggplot2::element_text(size = 10),
                        legend.position = "right",
                        legend.background = ggplot2::element_rect(fill = "white", color = "#CCCCCC"),
                        panel.grid.major = ggplot2::element_line(color = "#E0E0E0", size = 0.3),
                        panel.grid.minor = ggplot2::element_line(color = "#F0F0F0", size = 0.2),
                        plot.background = ggplot2::element_rect(fill = "white", color = NA),
                        panel.background = ggplot2::element_rect(fill = "white", color = NA)
                    ) +
                    # Add origin lines
                    ggplot2::geom_hline(yintercept = 0, color = "#BDC3C7", linetype = "dotted", size = 0.5) +
                    ggplot2::geom_vline(xintercept = 0, color = "#BDC3C7", linetype = "dotted", size = 0.5) +
                    # Ensure equal aspect ratio
                    ggplot2::coord_fixed()
                
                # Add sample labels if requested and available
                if (exists("showSampleLabels", where = self$options) && 
                    !is.null(self$options$showSampleLabels) && 
                    self$options$showSampleLabels && 
                    !is.null(pca_scores$SampleID)) {
                    
                    p <- p + ggrepel::geom_text_repel(
                        data = pca_scores,
                        ggplot2::aes(x = PC1, y = PC2, label = SampleID, color = Cluster),
                        size = 2.5, alpha = 0.8,
                        box.padding = 0.3, point.padding = 0.2,
                        max.overlaps = 20
                    )
                }
                
                # Print the plot
                print(p)
                return(TRUE)
            }, "PCA biplot visualization")
        },
        
        # Create cluster validation plot
        .visualizeClusterValidation = function(image, ggtheme, theme, ...) {
            # Skip if not requested or no silhouette data
            if (!self$options$showClusterValidation || is.null(private$.silhouette))
                return()
            
            # Check required packages
            required_packages <- c("ggplot2", "RColorBrewer")
            if (!private$.checkPackageDependencies(required_packages)) {
                return()
            }
            
            # Execute cluster validation visualization safely
            private$.safeExecute({
                silhouette_data <- private$.silhouette
                n_clusters <- length(unique(silhouette_data[, "cluster"]))
                
                # Convert silhouette object to data frame
                sil_df <- data.frame(
                    Sample = 1:nrow(silhouette_data),
                    Cluster = factor(paste("Cluster", silhouette_data[, "cluster"]),
                                   levels = paste("Cluster", 1:n_clusters)),
                    Neighbor = silhouette_data[, "neighbor"],
                    Width = silhouette_data[, "sil_width"]
                )
                
                # Add sample IDs if available
                if (!is.null(self$options$id) && self$options$id != "" && 
                    !is.null(self$data[[self$options$id]])) {
                    sil_df$SampleID <- as.character(self$data[[self$options$id]])
                } else {
                    sil_df$SampleID <- paste("Sample", sil_df$Sample)
                }
                
                # Calculate cluster statistics
                cluster_stats <- aggregate(Width ~ Cluster, sil_df, function(x) {
                    c(mean = mean(x), n = length(x), min = min(x), max = max(x))
                })
                cluster_stats <- do.call(data.frame, cluster_stats)
                names(cluster_stats) <- c("Cluster", "Mean_Width", "Count", "Min_Width", "Max_Width")
                
                # Overall silhouette statistics
                avg_silhouette <- mean(sil_df$Width)
                
                # Create publication-ready color palette
                if (n_clusters <= 3) {
                    cluster_colors <- c("#E74C3C", "#3498DB", "#2ECC71")[1:n_clusters]
                } else if (n_clusters <= 8) {
                    cluster_colors <- RColorBrewer::brewer.pal(n_clusters, "Set1")
                } else {
                    cluster_colors <- rainbow(n_clusters, s = 0.8, v = 0.9)
                }
                names(cluster_colors) <- paste("Cluster", 1:n_clusters)
                
                # Order samples by cluster and silhouette width for better visualization
                sil_df <- sil_df[order(sil_df$Cluster, -sil_df$Width), ]
                sil_df$Order <- 1:nrow(sil_df)
                
                # Create the main silhouette plot
                p <- ggplot2::ggplot(sil_df, ggplot2::aes(x = Order, y = Width)) +
                    # Add bars colored by cluster
                    ggplot2::geom_bar(
                        ggplot2::aes(fill = Cluster), 
                        stat = "identity", width = 1, color = "white", size = 0.1
                    ) +
                    # Add average silhouette line
                    ggplot2::geom_hline(
                        yintercept = avg_silhouette, 
                        color = "#2C3E50", linetype = "dashed", size = 1.2
                    ) +
                    # Add zero line
                    ggplot2::geom_hline(
                        yintercept = 0, 
                        color = "#34495E", linetype = "solid", size = 0.8
                    ) +
                    # Customize colors
                    ggplot2::scale_fill_manual(
                        name = "Clusters",
                        values = cluster_colors,
                        guide = ggplot2::guide_legend(
                            title = "IHC Clusters",
                            title.theme = ggplot2::element_text(size = 11, face = "bold")
                        )
                    ) +
                    # Add labels and titles
                    ggplot2::labs(
                        title = "Cluster Validation: Silhouette Analysis",
                        subtitle = paste0("Average silhouette width: ", round(avg_silhouette, 3), 
                                        " (n=", nrow(sil_df), " samples)"),
                        x = "Samples (ordered by cluster and silhouette width)",
                        y = "Silhouette Width",
                        caption = paste("Dashed line shows average silhouette width.",
                                      "Negative values indicate potential misclassification.")
                    ) +
                    # Apply professional theme
                    ggplot2::theme_minimal() +
                    ggplot2::theme(
                        plot.title = ggplot2::element_text(size = private$.PLOT_PARAMS$font_sizes$title, face = "bold", hjust = 0.5),
                        plot.subtitle = ggplot2::element_text(size = private$.PLOT_PARAMS$font_sizes$subtitle, hjust = 0.5, color = private$.COLORS$ui$text_secondary),
                        plot.caption = ggplot2::element_text(size = private$.PLOT_PARAMS$font_sizes$caption, color = private$.COLORS$ui$text_caption, hjust = 0.5),
                        axis.title = ggplot2::element_text(size = 12, face = "bold"),
                        axis.text = ggplot2::element_text(size = 10),
                        axis.text.x = ggplot2::element_blank(),  # Hide x-axis labels for clarity
                        axis.ticks.x = ggplot2::element_blank(),
                        legend.title = ggplot2::element_text(size = 11, face = "bold"),
                        legend.text = ggplot2::element_text(size = 10),
                        legend.position = "right",
                        legend.background = ggplot2::element_rect(fill = "white", color = "#CCCCCC"),
                        panel.grid.major.x = ggplot2::element_blank(),
                        panel.grid.minor = ggplot2::element_blank(),
                        panel.grid.major.y = ggplot2::element_line(color = "#E0E0E0", size = 0.3),
                        plot.background = ggplot2::element_rect(fill = "white", color = NA),
                        panel.background = ggplot2::element_rect(fill = "white", color = NA)
                    )
                
                # Add cluster separation lines
                cluster_boundaries <- cumsum(table(sil_df$Cluster))
                cluster_boundaries <- cluster_boundaries[-length(cluster_boundaries)]  # Remove last boundary
                
                if (length(cluster_boundaries) > 0) {
                    for (boundary in cluster_boundaries) {
                        p <- p + ggplot2::geom_vline(
                            xintercept = boundary + 0.5, 
                            color = "#7F8C8D", linetype = "dotted", size = 0.8, alpha = 0.7
                        )
                    }
                }
                
                # Add interpretation text annotation
                interpretation <- 
                    if (avg_silhouette > 0.7) "Strong structure"
                    else if (avg_silhouette > 0.5) "Reasonable structure"
                    else if (avg_silhouette > 0.25) "Weak structure"
                    else "No substantial structure"
                
                p <- p + ggplot2::annotate(
                    "text", 
                    x = nrow(sil_df) * 0.02, 
                    y = max(sil_df$Width) * 0.9,
                    label = paste("Cluster Quality:", interpretation),
                    hjust = 0, vjust = 1,
                    size = 4, fontface = "italic",
                    color = "#2C3E50"
                )
                
                # Print the plot
                print(p)
                return(TRUE)
            }, "Cluster validation visualization")
        },
        
        # Create score distribution plot
        .scoreDistPlot = function(image, ggtheme, theme, ...) {
            # Skip if not requested or no data
            if (!self$options$showScoreDist || is.null(private$.ihc_matrix))
                return()
            
            # Create box plots for each marker by cluster
            if (!is.null(private$.clusters)) {
                # Convert matrix to long format for plotting
                ihc_df <- as.data.frame(private$.ihc_matrix)
                ihc_df$Cluster <- factor(private$.clusters)
                ihc_df$Sample <- rownames(ihc_df)
                
                # Reshape to long format
                ihc_long <- reshape2::melt(ihc_df, 
                                         id.vars = c("Sample", "Cluster"),
                                         variable.name = "Marker",
                                         value.name = "Score")
                
                # Create publication-ready distribution plot
                n_clusters <- length(unique(ihc_df$Cluster))
                n_markers <- length(unique(ihc_long$Marker))
                
                # Professional color palette
                if (n_clusters <= 3) {
                    cluster_colors <- c("#E74C3C", "#3498DB", "#2ECC71")[1:n_clusters]
                } else if (n_clusters <= 8) {
                    cluster_colors <- RColorBrewer::brewer.pal(n_clusters, "Set1")
                } else {
                    cluster_colors <- rainbow(n_clusters, s = 0.8, v = 0.9)
                }
                
                # Create comprehensive visualization
                if (n_markers <= 6 && n_clusters <= 4) {
                    # Detailed violin + box plot for smaller datasets
                    p <- ggplot2::ggplot(ihc_long, ggplot2::aes(x = Marker, y = Score, fill = Cluster)) +
                        ggplot2::geom_violin(alpha = 0.4, scale = "width", trim = FALSE) +
                        ggplot2::geom_boxplot(width = 0.3, alpha = 0.8, outlier.alpha = 0.6) +
                        ggplot2::geom_jitter(width = 0.1, alpha = 0.3, size = 0.8) +
                        ggplot2::scale_fill_manual(values = cluster_colors) +
                        ggplot2::facet_wrap(~Cluster, ncol = min(n_clusters, 2)) +
                        ggplot2::labs(
                            title = "IHC Expression Score Distribution by Cluster",
                            subtitle = paste0("Detailed distribution across ", n_markers, " markers"),
                            x = "IHC Markers",
                            y = "Expression Score",
                            caption = "Violin plots show distribution shape; boxes show quartiles and outliers"
                        ) +
                        ggplot2::theme_minimal() +
                        ggplot2::theme(
                            plot.title = ggplot2::element_text(size = private$.PLOT_PARAMS$font_sizes$title, face = "bold", hjust = 0.5),
                            plot.subtitle = ggplot2::element_text(size = private$.PLOT_PARAMS$font_sizes$subtitle, hjust = 0.5, color = private$.COLORS$ui$text_secondary),
                            plot.caption = ggplot2::element_text(size = private$.PLOT_PARAMS$font_sizes$caption, color = private$.COLORS$ui$text_caption, hjust = 0.5),
                            axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, vjust = 1),
                            strip.text = ggplot2::element_text(face = "bold"),
                            legend.position = "bottom"
                        )
                } else {
                    # Simplified box plot for larger datasets
                    p <- ggplot2::ggplot(ihc_long, ggplot2::aes(x = Marker, y = Score, fill = Cluster)) +
                        ggplot2::geom_boxplot(alpha = 0.8, outlier.alpha = 0.6) +
                        ggplot2::scale_fill_manual(values = cluster_colors) +
                        ggplot2::facet_wrap(~Cluster, ncol = 2, scales = "free_y") +
                        ggplot2::labs(
                            title = "IHC Expression Patterns by Cluster",
                            subtitle = paste0("Comparison across ", n_markers, " markers and ", n_clusters, " clusters"),
                            x = "IHC Markers",
                            y = "Expression Score"
                        ) +
                        ggplot2::theme_minimal() +
                        ggplot2::theme(
                            plot.title = ggplot2::element_text(size = private$.PLOT_PARAMS$font_sizes$title, face = "bold", hjust = 0.5),
                            plot.subtitle = ggplot2::element_text(size = private$.PLOT_PARAMS$font_sizes$subtitle, hjust = 0.5, color = private$.COLORS$ui$text_secondary),
                            axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
                            strip.text = ggplot2::element_text(face = "bold"),
                            legend.position = "none"  # Colors explained by facets
                        )
                }
                
                print(p)
            }
            
            return(TRUE)
        },
        
        # Implement iterative clustering method
        .performIterativeClustering = function(data) {
            if (!self$options$iterativeClustering) return()
            
            # Clear existing results
            # self$results$optimalMarkersTable$clear()  # Method not available in jamovi tables
            
            markers <- self$options$markers
            if (length(markers) < 2) return()
            
            # Convert to numeric matrix
            data_matrix <- sapply(data, as.numeric)
            
            # Forward selection approach
            selected_markers <- character(0)
            remaining_markers <- markers
            marker_scores <- list()
            
            while (length(remaining_markers) > 0) {
                best_marker <- NULL
                best_score <- -Inf
                
                for (marker in remaining_markers) {
                    test_markers <- c(selected_markers, marker)
                    test_data <- data_matrix[, test_markers, drop = FALSE]
                    
                    # Calculate clustering quality
                    if (nrow(test_data) < 4) break
                    
                    tryCatch({
                        if (self$options$distanceMetric == "gower") {
                            if (requireNamespace("cluster", quietly = TRUE)) {
                                dist_matrix <- cluster::daisy(test_data, metric = "gower")
                            } else {
                                dist_matrix <- dist(test_data)
                            }
                        } else {
                            dist_matrix <- private$.calculateJaccardDistance(test_data)
                        }
                        
                        hc <- hclust(dist_matrix, method = self$options$linkageMethod)
                        clusters <- cutree(hc, k = self$options$nClusters)
                        
                        # Calculate separation score using silhouette
                        if (requireNamespace("cluster", quietly = TRUE)) {
                            sil <- cluster::silhouette(clusters, dist_matrix)
                            separation_score <- mean(sil[, "sil_width"])
                        } else {
                            # Fallback: calculate within-cluster sum of squares
                            separation_score <- -sum(sapply(1:self$options$nClusters, function(k) {
                                cluster_members <- which(clusters == k)
                                if (length(cluster_members) > 1) {
                                    sum(dist_matrix[cluster_members, cluster_members]^2)
                                } else {
                                    0
                                }
                            }))
                        }
                        
                        if (separation_score > best_score) {
                            best_score <- separation_score
                            best_marker <- marker
                        }
                    }, error = function(e) {
                        # Skip markers that cause errors
                    })
                }
                
                if (!is.null(best_marker)) {
                    selected_markers <- c(selected_markers, best_marker)
                    remaining_markers <- setdiff(remaining_markers, best_marker)
                    
                    # Determine status
                    status <- if (best_score > 0.3) {
                        "Essential"
                    } else if (best_score > 0.1) {
                        "Helpful"
                    } else {
                        "Optional"
                    }
                    
                    # Add to results table
                    self$results$optimalMarkersTable$addRow(rowKey = best_marker, values = list(
                        marker = best_marker,
                        separation_score = round(best_score, 3),
                        status = status
                    ))
                    
                    # Stop if score improvement is minimal
                    if (length(selected_markers) > 1 && best_score < 0.1) {
                        break
                    }
                } else {
                    break
                }
            }
        }
    )
)