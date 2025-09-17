#' @title IHC Diagnostic Analysis
#' @name ihcdiagnostic
#' @importFrom R6 R6Class
#' @import jmvcore
#' @importFrom cluster daisy pam
#' @importFrom stats hclust cutree
#' @import ggplot2

# Source IHC utilities
tryCatch({
    utils_file <- system.file("R", "ihc_utilities.R", package = "ClinicoPath")
    if (file.exists(utils_file)) {
        source(utils_file)
    } else {
        # Try local file for development
        local_utils <- file.path(dirname(sys.frame(1)$ofile), "ihc_utilities.R")
        if (file.exists(local_utils)) {
            source(local_utils)
        }
    }
}, error = function(e) {
    # Utilities will be loaded by package
})

# Ensure functions exist
if (!exists("convertIHCToNumeric")) {
    # Define fallback implementation
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

if (!exists("validateIHCData")) {
    # Define fallback implementation
    validateIHCData <- function(data, markers) {
        if (is.null(data) || nrow(data) < 3) {
            return(list(valid = FALSE, message = "At least 3 samples are required for IHC analysis"))
        }
        if (length(markers) < 2) {
            return(list(valid = FALSE, message = "At least 2 IHC markers are required for clustering analysis"))
        }
        missing_markers <- markers[!markers %in% names(data)]
        if (length(missing_markers) > 0) {
            return(list(valid = FALSE, message = paste("Missing marker columns:", paste(missing_markers, collapse = ", "))))
        }
        return(list(valid = TRUE, message = NULL))
    }
}

ihcdiagnosticClass <- if (requireNamespace('jmvcore', quietly = TRUE)) R6::R6Class(
    "ihcdiagnosticClass",
    inherit = ihcdiagnosticBase,
    private = list(
        .ihc_matrix = NULL,
        .diagnosis_groups = NULL,
        .clusters = NULL,
        .performance_results = NULL,
        .roc_data = NULL,

        .init = function() {
            if (is.null(self$data) || length(self$options$markers) == 0 ||
                is.null(self$options$diagnosis) || self$options$diagnosis == "") {
                self$results$instructions$setContent(
                    "<h3>IHC Diagnostic Performance Analysis</h3>
                    <p>This analysis evaluates the diagnostic performance of IHC markers and performs differential diagnosis.</p>

                    <h4>Required Input:</h4>
                    <ul>
                        <li><b>IHC Markers:</b> Expression data for antibodies/biomarkers (e.g., ER, PR, HER2, Ki67)</li>
                        <li><b>Known Diagnosis:</b> Reference standard diagnosis (e.g., tumor type, grade)</li>
                        <li><b>Minimum samples:</b> At least 10 cases per diagnostic group</li>
                    </ul>

                    <h4>Analysis Options:</h4>
                    <ul>
                        <li><b>Diagnostic Metrics:</b> Sensitivity, specificity, PPV, NPV, AUC</li>
                        <li><b>Differential Diagnosis:</b> Cluster-based diagnostic prediction</li>
                        <li><b>Panel Optimization:</b> Find optimal antibody combinations</li>
                        <li><b>ROC Analysis:</b> Performance visualization and comparison</li>
                    </ul>

                    <h4>Clinical Applications:</h4>
                    <ul>
                        <li>Breast cancer subtyping (Luminal A/B, HER2+, Triple Negative)</li>
                        <li>Lymphoma classification using immunophenotyping</li>
                        <li>Carcinoma of unknown primary workup</li>
                        <li>Sarcoma differential diagnosis</li>
                    </ul>

                    <p><i>Select your markers and diagnosis variable to begin.</i></p>"
                )
            }

            # Initialize results sections with safe access
            tryCatch({
                if (!is.null(self$results) && !is.null(self$results[["clinicalSummary"]])) {
                    self$results$clinicalSummary$setVisible(FALSE)
                }
            }, error = function(e) {
                # Result not available yet, ignore
            })

            tryCatch({
                if (!is.null(self$results) && !is.null(self$results[["interpretationGuide"]])) {
                    self$results$interpretationGuide$setVisible(FALSE)
                }
            }, error = function(e) {
                # Result not available yet, ignore
            })
        },

        .run = function() {
            # Validate inputs
            if (is.null(self$data) || length(self$options$markers) == 0 ||
                is.null(self$options$diagnosis) || self$options$diagnosis == "") {
                return()
            }

            validation <- private$.validateData()
            if (!validation$valid) {
                self$results$instructions$setContent(
                    paste0("<p style='color: red;'><b>Error:</b> ", validation$message, "</p>")
                )
                return()
            }

            # Prepare data
            private$.prepareData()

            # Calculate diagnostic performance
            if (self$options$calculateDiagnosticMetrics) {
                private$.calculateDiagnosticPerformance()
            }

            # Perform differential diagnosis
            if (self$options$differentialDiagnosis) {
                private$.performDifferentialDiagnosis()
            }

            # Optimize antibody panels
            if (self$options$antibodyOptimization) {
                private$.optimizeAntibodyPanel()
            }

            # Update summary
            private$.updateInstructions()

            # Generate clinical summary
            private$.generateClinicalSummary()

            # Show interpretation guide
            private$.showInterpretationGuide()
        },

        .validateData = function() {
            # Check diagnosis variable
            diagnosis_data <- self$data[[self$options$diagnosis]]
            if (is.null(diagnosis_data)) {
                return(list(valid = FALSE, message = "Diagnosis variable not found"))
            }

            # Check group sizes
            group_counts <- table(diagnosis_data)
            min_size <- self$options$minimumGroupSize
            small_groups <- group_counts < min_size

            if (any(small_groups)) {
                small_names <- names(group_counts)[small_groups]
                # Add clinical warning for small sample sizes
                warning_msg <- sprintf("Groups with insufficient samples: %s (need ≥%d for reliable analysis).\n\nClinical Note: Small sample sizes may lead to unreliable diagnostic performance estimates. Consider:\n• Combining similar diagnostic groups\n• Collecting additional cases\n• Using bootstrap resampling for confidence intervals",
                                     paste(small_names, collapse = ", "), min_size)
                return(list(valid = FALSE, message = warning_msg))
            }

            # Check number of groups
            if (length(group_counts) < 2) {
                return(list(valid = FALSE, message = "At least 2 diagnostic groups required"))
            }

            return(list(valid = TRUE))
        },

        .prepareData = function() {
            # Use IHC utility function for consistent data conversion
            tryCatch({
                private$.ihc_matrix <- convertIHCToNumeric(
                    self$data,
                    self$options$markers,
                    id_variable = if(!is.null(self$options$id) && self$options$id != "") self$options$id else NULL
                )
            }, error = function(e) {
                stop(paste("Error converting IHC data:", e$message))
            })

            # Store diagnosis groups
            diagnosis_data <- self$data[[self$options$diagnosis]]
            if (is.null(diagnosis_data)) {
                stop("Diagnosis variable not found in data")
            }
            private$.diagnosis_groups <- factor(diagnosis_data)
        },

        .calculateDiagnosticPerformance = function() {
            markers <- self$options$markers
            groups <- private$.diagnosis_groups

            # Store performance results for plotting
            private$.performance_results <- list()

            # For binary classification, convert to binary problem
            if (length(levels(groups)) == 2) {
                binary_outcome <- as.numeric(groups) - 1

                for (marker in markers) {
                    marker_data <- private$.ihc_matrix[, marker]

                    if (self$options$crossValidation) {
                        perf <- private$.calculateBinaryPerformanceCV(marker_data, binary_outcome, marker)
                    } else {
                        perf <- private$.calculateBinaryPerformance(marker_data, binary_outcome, marker)
                    }

                    # Store for plotting
                    private$.performance_results[[marker]] <- perf

                    self$results$diagnosticPerformance$addRow(
                        rowKey = marker,
                        values = perf
                    )
                }
            } else {
                # Multi-class: calculate one-vs-rest for each group
                for (i in seq_along(levels(groups))) {
                    target_group <- levels(groups)[i]
                    binary_outcome <- ifelse(groups == target_group, 1, 0)

                    for (marker in markers) {
                        marker_data <- private$.ihc_matrix[, marker]

                        if (self$options$crossValidation) {
                            perf <- private$.calculateBinaryPerformanceCV(marker_data, binary_outcome,
                                                                        paste(marker, "vs", target_group))
                        } else {
                            perf <- private$.calculateBinaryPerformance(marker_data, binary_outcome,
                                                                        paste(marker, "vs", target_group))
                        }

                        # Store for plotting
                        key <- paste(marker, target_group, sep = "_")
                        private$.performance_results[[key]] <- perf

                        self$results$diagnosticPerformance$addRow(
                            rowKey = key,
                            values = perf
                        )
                    }
                }
            }
        },

        .calculateBinaryPerformance = function(marker_data, outcome, marker_name) {
            # Remove missing data
            complete_idx <- !is.na(marker_data) & !is.na(outcome)
            marker_clean <- marker_data[complete_idx]
            outcome_clean <- outcome[complete_idx]

            if (length(unique(outcome_clean)) < 2) {
                return(list(
                    marker = marker_name,
                    cutpoint = NA,
                    sensitivity = NA,
                    specificity = NA,
                    ppv = NA,
                    npv = NA,
                    accuracy = NA,
                    auc = NA
                ))
            }

            # Calculate ROC and find optimal cutpoint
            roc_result <- private$.calculateROC(marker_clean, outcome_clean)
            optimal_cutpoint <- private$.findOptimalCutpoint(marker_clean, outcome_clean)

            # Calculate performance at optimal cutpoint
            predicted <- ifelse(marker_clean >= optimal_cutpoint, 1, 0)
            tp <- sum(predicted == 1 & outcome_clean == 1)
            tn <- sum(predicted == 0 & outcome_clean == 0)
            fp <- sum(predicted == 1 & outcome_clean == 0)
            fn <- sum(predicted == 0 & outcome_clean == 1)

            # Calculate confidence intervals if requested
            confidence_level <- self$options$confidenceLevel
            alpha <- 1 - confidence_level
            z_score <- qnorm(1 - alpha/2)

            n_pos <- tp + fn
            n_neg <- tn + fp

            sensitivity <- if (n_pos > 0) tp / n_pos * 100 else NA
            specificity <- if (n_neg > 0) tn / n_neg * 100 else NA
            ppv <- if (tp + fp > 0) tp / (tp + fp) * 100 else NA
            npv <- if (tn + fn > 0) tn / (tn + fn) * 100 else NA
            accuracy <- (tp + tn) / length(outcome_clean) * 100

            # Store ROC data for plotting
            if (is.null(private$.roc_data)) {
                private$.roc_data <- list()
            }
            private$.roc_data[[marker_name]] <- roc_result

            return(list(
                marker = marker_name,
                cutpoint = optimal_cutpoint,
                sensitivity = sensitivity,
                specificity = specificity,
                ppv = ppv,
                npv = npv,
                accuracy = accuracy,
                auc = roc_result$auc
            ))
        },

        .calculateROC = function(marker_data, outcome) {
            # Simple ROC calculation
            thresholds <- sort(unique(marker_data))
            sensitivities <- numeric(length(thresholds))
            specificities <- numeric(length(thresholds))

            for (i in seq_along(thresholds)) {
                predicted <- ifelse(marker_data >= thresholds[i], 1, 0)
                tp <- sum(predicted == 1 & outcome == 1)
                tn <- sum(predicted == 0 & outcome == 0)
                fp <- sum(predicted == 1 & outcome == 0)
                fn <- sum(predicted == 0 & outcome == 1)

                sensitivities[i] <- if (tp + fn > 0) tp / (tp + fn) else 0
                specificities[i] <- if (tn + fp > 0) tn / (tn + fp) else 0
            }

            # Calculate AUC using trapezoidal rule
            fpr <- 1 - specificities
            auc <- private$.calculateAUC(fpr, sensitivities)

            return(list(
                thresholds = thresholds,
                sensitivities = sensitivities,
                specificities = specificities,
                fpr = fpr,
                auc = auc
            ))
        },

        .calculateAUC = function(fpr, tpr) {
            # Sort by FPR
            order_idx <- order(fpr)
            fpr_sorted <- fpr[order_idx]
            tpr_sorted <- tpr[order_idx]

            # Trapezoidal integration
            auc <- 0
            for (i in 2:length(fpr_sorted)) {
                auc <- auc + (fpr_sorted[i] - fpr_sorted[i-1]) * (tpr_sorted[i] + tpr_sorted[i-1]) / 2
            }

            return(auc)
        },

        .findOptimalCutpoint = function(marker_data, outcome, marker_name = NULL) {
            if (self$options$cutpointMethod == "median") {
                return(median(marker_data, na.rm = TRUE))
            } else if (self$options$cutpointMethod == "clinical") {
                # Use clinical guidelines based on marker type
                cutpoint <- private$.getClinicalCutpoint(marker_name, marker_data)
                return(cutpoint)
            } else {
                # Youden index (optimal method)
                thresholds <- sort(unique(marker_data))
                youden_indices <- numeric(length(thresholds))

                for (i in seq_along(thresholds)) {
                    predicted <- ifelse(marker_data >= thresholds[i], 1, 0)
                    tp <- sum(predicted == 1 & outcome == 1)
                    tn <- sum(predicted == 0 & outcome == 0)
                    fp <- sum(predicted == 1 & outcome == 0)
                    fn <- sum(predicted == 0 & outcome == 1)

                    sensitivity <- if (tp + fn > 0) tp / (tp + fn) else 0
                    specificity <- if (tn + fp > 0) tn / (tn + fp) else 0
                    youden_indices[i] <- sensitivity + specificity - 1
                }

                optimal_idx <- which.max(youden_indices)
                return(thresholds[optimal_idx])
            }
        },

        .performDifferentialDiagnosis = function() {
            # Perform clustering
            dist_matrix <- cluster::daisy(private$.ihc_matrix, metric = "gower")

            n_groups <- length(levels(private$.diagnosis_groups))

            if (self$options$clusterMethod == "hierarchical") {
                hc <- hclust(dist_matrix, method = "ward.D2")
                private$.clusters <- cutree(hc, k = n_groups)
            } else {
                pam_result <- cluster::pam(dist_matrix, k = n_groups, diss = TRUE)
                private$.clusters <- pam_result$clustering
            }

            # Map clusters to diagnoses (find best matching)
            cluster_mapping <- private$.mapClustersToGroups()

            # Populate results table
            for (i in seq_len(nrow(self$data))) {
                case_id <- if (!is.null(self$options$id) && self$options$id != "") {
                    as.character(self$data[[self$options$id]][i])
                } else {
                    paste("Case", i)
                }

                predicted_group <- cluster_mapping[private$.clusters[i]]
                actual_group <- as.character(private$.diagnosis_groups[i])
                confidence <- private$.calculateConfidence(i)
                correct <- ifelse(predicted_group == actual_group, "✓", "✗")

                self$results$differentialResults$addRow(
                    rowKey = i,
                    values = list(
                        case_id = case_id,
                        predicted = predicted_group,
                        actual = actual_group,
                        confidence = confidence,
                        cluster = private$.clusters[i],
                        correct = correct
                    )
                )
            }

            # Create confusion matrix
            private$.createConfusionMatrix(cluster_mapping)
        },

        .mapClustersToGroups = function() {
            # Find the best mapping between clusters and diagnosis groups
            groups <- levels(private$.diagnosis_groups)
            clusters <- unique(private$.clusters)

            mapping <- character(max(clusters))

            for (cluster in clusters) {
                cluster_indices <- which(private$.clusters == cluster)
                cluster_groups <- private$.diagnosis_groups[cluster_indices]

                # Find most common group in this cluster
                group_counts <- table(cluster_groups)
                most_common <- names(group_counts)[which.max(group_counts)]
                mapping[cluster] <- most_common
            }

            return(mapping)
        },

        .calculateConfidence = function(case_idx) {
            # Real confidence calculation based on distance to cluster centroid
            cluster <- private$.clusters[case_idx]
            cluster_indices <- which(private$.clusters == cluster)

            if (length(cluster_indices) < 2) return(50.0)

            # Calculate distance-based confidence
            cluster_data <- private$.ihc_matrix[cluster_indices, , drop = FALSE]
            case_data <- private$.ihc_matrix[case_idx, , drop = FALSE]

            # Distance to own cluster centroid
            centroid <- colMeans(cluster_data, na.rm = TRUE)
            own_dist <- sqrt(sum((case_data - centroid)^2, na.rm = TRUE))

            # Distance to other cluster centroids
            other_clusters <- unique(private$.clusters)[unique(private$.clusters) != cluster]
            min_other_dist <- Inf

            for (other_cluster in other_clusters) {
                other_indices <- which(private$.clusters == other_cluster)
                other_centroid <- colMeans(private$.ihc_matrix[other_indices, , drop = FALSE], na.rm = TRUE)
                other_dist <- sqrt(sum((case_data - other_centroid)^2, na.rm = TRUE))
                min_other_dist <- min(min_other_dist, other_dist)
            }

            # Confidence based on relative distances
            if (min_other_dist == Inf || own_dist + min_other_dist == 0) {
                confidence <- 50.0
            } else {
                confidence <- min_other_dist / (own_dist + min_other_dist) * 100
                confidence <- max(50, min(95, confidence))
            }

            return(confidence)
        },

        .createConfusionMatrix = function(cluster_mapping) {
            # Create dynamic confusion matrix with proper column creation
            predicted_labels <- cluster_mapping[private$.clusters]
            actual_labels <- as.character(private$.diagnosis_groups)
            groups <- levels(private$.diagnosis_groups)

            # Add dynamic columns for each predicted group
            for (i in seq_along(groups)) {
                col_name <- paste0("predicted_", i)
                col_title <- groups[i]

                existing_columns <- sapply(self$results$confusionMatrix$columns, function(x) x$name)
                if (!col_name %in% existing_columns) {
                    self$results$confusionMatrix$addColumn(
                        name = col_name,
                        title = col_title,
                        type = "integer",
                        index = i + 1  # After actual column
                    )
                }
            }

            # Populate data
            for (actual in groups) {
                row_values <- list(actual = actual)
                actual_indices <- which(actual_labels == actual)

                for (i in seq_along(groups)) {
                    predicted <- groups[i]
                    count <- sum(predicted_labels[actual_indices] == predicted)
                    row_values[[paste0("predicted_", i)]] <- count
                }

                row_values$total <- length(actual_indices)

                self$results$confusionMatrix$addRow(
                    rowKey = actual,
                    values = row_values
                )
            }
        },

        .optimizeAntibodyPanel = function() {
            markers <- self$options$markers
            n_markers <- length(markers)

            # Limit combinations to prevent excessive computation
            max_panel_size <- min(n_markers, 4)

            # Test single markers first
            single_results <- list()
            for (marker in markers) {
                perf <- private$.evaluatePanel(marker)
                single_results[[marker]] <- perf

                self$results$panelOptimization$addRow(
                    rowKey = paste("single", marker),
                    values = list(
                        panel_size = 1,
                        markers_included = marker,
                        sensitivity = perf$sensitivity,
                        specificity = perf$specificity,
                        accuracy = perf$accuracy,
                        recommendation = perf$recommendation
                    )
                )
            }

            # Find best performing single markers for combinations
            single_accuracies <- sapply(single_results, function(x) x$accuracy)
            best_markers <- names(sort(single_accuracies, decreasing = TRUE))

            # Test meaningful combinations
            if (n_markers >= 2) {
                # Test pairs of best performers
                for (i in 1:min(3, n_markers-1)) {
                    for (j in (i+1):min(i+2, n_markers)) {
                        if (j <= length(best_markers)) {
                            combo_markers <- best_markers[c(i, j)]
                            perf <- private$.evaluatePanel(combo_markers)

                            self$results$panelOptimization$addRow(
                                rowKey = paste("pair", i, j),
                                values = list(
                                    panel_size = 2,
                                    markers_included = paste(combo_markers, collapse = ", "),
                                    sensitivity = perf$sensitivity,
                                    specificity = perf$specificity,
                                    accuracy = perf$accuracy,
                                    recommendation = perf$recommendation
                                )
                            )
                        }
                    }
                }
            }

            # Test triple combinations if we have enough markers
            if (n_markers >= 3) {
                # Test top 3 performers together
                if (length(best_markers) >= 3) {
                    combo_markers <- best_markers[1:3]
                    perf <- private$.evaluatePanel(combo_markers)

                    self$results$panelOptimization$addRow(
                        rowKey = "triple_best",
                        values = list(
                            panel_size = 3,
                            markers_included = paste(combo_markers, collapse = ", "),
                            sensitivity = perf$sensitivity,
                            specificity = perf$specificity,
                            accuracy = perf$accuracy,
                            recommendation = perf$recommendation
                        )
                    )
                }
            }

            # Test all markers together if reasonable number
            if (n_markers >= 4 && n_markers <= max_panel_size) {
                perf <- private$.evaluatePanel(markers)

                self$results$panelOptimization$addRow(
                    rowKey = "all_markers",
                    values = list(
                        panel_size = n_markers,
                        markers_included = paste(markers, collapse = ", "),
                        sensitivity = perf$sensitivity,
                        specificity = perf$specificity,
                        accuracy = perf$accuracy,
                        recommendation = perf$recommendation
                    )
                )
            }
        },

        .evaluatePanel = function(markers) {
            # Enhanced panel evaluation using weighted ensemble
            if (length(markers) == 1) {
                marker_data <- private$.ihc_matrix[, markers]
            } else {
                # Use weighted ensemble based on individual marker performance
                weights <- numeric(length(markers))
                for (i in seq_along(markers)) {
                    # Get individual marker AUC if available
                    if (!is.null(private$.performance_results[[markers[i]]])) {
                        weights[i] <- private$.performance_results[[markers[i]]]$auc
                    } else {
                        weights[i] <- 0.5  # Default weight
                    }
                }

                # Normalize weights
                weights <- weights / sum(weights)

                # Weighted combination
                marker_data <- numeric(nrow(private$.ihc_matrix))
                for (i in seq_along(markers)) {
                    marker_data <- marker_data + weights[i] * private$.ihc_matrix[, markers[i]]
                }
            }

            # Convert to binary for evaluation
            binary_outcome <- as.numeric(private$.diagnosis_groups) - 1
            if (length(unique(binary_outcome)) > 2) {
                # Multi-class to binary (first class vs rest)
                binary_outcome <- ifelse(binary_outcome == 0, 1, 0)
            }

            # Calculate performance
            optimal_cutpoint <- private$.findOptimalCutpoint(marker_data, binary_outcome)
            predicted <- ifelse(marker_data >= optimal_cutpoint, 1, 0)

            tp <- sum(predicted == 1 & binary_outcome == 1)
            tn <- sum(predicted == 0 & binary_outcome == 0)
            fp <- sum(predicted == 1 & binary_outcome == 0)
            fn <- sum(predicted == 0 & binary_outcome == 1)

            sensitivity <- if (tp + fn > 0) tp / (tp + fn) * 100 else 0
            specificity <- if (tn + fp > 0) tn / (tn + fp) * 100 else 0
            accuracy <- (tp + tn) / length(binary_outcome) * 100

            # Generate recommendation
            recommendation <- if (accuracy > 90) "Excellent"
                            else if (accuracy > 80) "Good"
                            else if (accuracy > 70) "Acceptable"
                            else "Consider alternatives"

            return(list(
                sensitivity = sensitivity,
                specificity = specificity,
                accuracy = accuracy,
                recommendation = recommendation
            ))
        },

        .updateInstructions = function() {
            n_samples <- nrow(self$data)
            n_markers <- length(self$options$markers)
            n_groups <- length(levels(private$.diagnosis_groups))

            summary_html <- sprintf(
                "<h3>Diagnostic Analysis Complete</h3>
                <p><b>Analysis Summary:</b></p>
                <ul>
                    <li>Cases analyzed: %d</li>
                    <li>IHC markers: %d</li>
                    <li>Diagnostic groups: %d</li>
                </ul>
                <p><i>Review the diagnostic performance tables and plots below.</i></p>",
                n_samples, n_markers, n_groups
            )

            self$results$instructions$setContent(summary_html)
        },

        .plotROC = function(image, ...) {
            if (is.null(private$.roc_data) || length(private$.roc_data) == 0) {
                plot(c(0, 1), c(0, 1), type = "l", lty = 2,
                     xlab = "False Positive Rate", ylab = "True Positive Rate",
                     main = "ROC Curves - No Data Available")
                return(TRUE)
            }

            # Set up plot
            plot(c(0, 1), c(0, 1), type = "l", lty = 2, col = "gray",
                 xlab = "False Positive Rate (1 - Specificity)",
                 ylab = "True Positive Rate (Sensitivity)",
                 main = "ROC Curves for IHC Markers")

            # Define colors for different markers
            colors <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728",
                       "#9467bd", "#8c564b", "#e377c2", "#7f7f7f")

            # Plot ROC curves for each marker
            legend_labels <- character(0)
            legend_colors <- character(0)

            for (i in seq_along(private$.roc_data)) {
                marker_name <- names(private$.roc_data)[i]
                roc_data <- private$.roc_data[[marker_name]]

                if (!is.null(roc_data$fpr) && !is.null(roc_data$sensitivities)) {
                    color <- colors[((i - 1) %% length(colors)) + 1]
                    lines(roc_data$fpr, roc_data$sensitivities, col = color, lwd = 2)

                    legend_labels <- c(legend_labels,
                                     sprintf("%s (AUC = %.3f)", marker_name, roc_data$auc))
                    legend_colors <- c(legend_colors, color)
                }
            }

            # Add legend
            if (length(legend_labels) > 0) {
                legend("bottomright", legend = legend_labels, col = legend_colors,
                       lwd = 2, cex = 0.8)
            }

            # Add reference line label
            text(0.6, 0.4, "Random Classifier", col = "gray", cex = 0.8)

            TRUE
        },

        .plotDiagnosticPerformance = function(image, ...) {
            if (is.null(private$.performance_results) || length(private$.performance_results) == 0) {
                plot(1, 1, type = "n", main = "No performance data available",
                     sub = "Run diagnostic metrics calculation first")
                return(TRUE)
            }

            # Extract actual performance data from results
            markers <- self$options$markers
            n_markers <- length(markers)

            if (n_markers == 0) {
                plot(1, 1, type = "n", main = "No markers selected")
                return(TRUE)
            }

            # Create matrix for grouped barplot
            perf_metrics <- c("Sensitivity", "Specificity", "Accuracy")
            perf_matrix <- matrix(NA, nrow = length(perf_metrics), ncol = n_markers)
            rownames(perf_matrix) <- perf_metrics
            colnames(perf_matrix) <- markers

            # Get actual performance data from stored results
            for (i in seq_along(markers)) {
                marker <- markers[i]
                if (!is.null(private$.performance_results[[marker]])) {
                    perf <- private$.performance_results[[marker]]
                    perf_matrix["Sensitivity", i] <- perf$sensitivity
                    perf_matrix["Specificity", i] <- perf$specificity
                    perf_matrix["Accuracy", i] <- perf$accuracy
                } else {
                    # Handle multi-class case - take average
                    marker_perfs <- grep(paste0("^", marker, "_"), names(private$.performance_results), value = TRUE)
                    if (length(marker_perfs) > 0) {
                        sens_values <- sapply(marker_perfs, function(x) private$.performance_results[[x]]$sensitivity)
                        spec_values <- sapply(marker_perfs, function(x) private$.performance_results[[x]]$specificity)
                        acc_values <- sapply(marker_perfs, function(x) private$.performance_results[[x]]$accuracy)

                        perf_matrix["Sensitivity", i] <- mean(sens_values, na.rm = TRUE)
                        perf_matrix["Specificity", i] <- mean(spec_values, na.rm = TRUE)
                        perf_matrix["Accuracy", i] <- mean(acc_values, na.rm = TRUE)
                    }
                }
            }

            # Create grouped barplot with colorblind-safe colors
            colors <- c("#1f77b4", "#ff7f0e", "#2ca02c")  # Colorblind-safe palette
            bp <- barplot(perf_matrix, beside = TRUE, col = colors,
                         main = "Diagnostic Performance by Marker",
                         ylab = "Performance (%)",
                         xlab = "IHC Markers",
                         ylim = c(0, 100),
                         legend = TRUE,
                         args.legend = list(x = "topright", cex = 0.8))

            # Add clinical reference lines
            abline(h = 90, col = "darkgreen", lty = 2, lwd = 1)
            text(1, 92, "Excellent (90%)", col = "darkgreen", cex = 0.7)
            abline(h = 80, col = "orange", lty = 2, lwd = 1)
            text(1, 82, "Good (80%)", col = "orange", cex = 0.7)
            abline(h = 70, col = "red", lty = 2, lwd = 1)
            text(1, 72, "Acceptable (70%)", col = "red", cex = 0.7)

            # Add value labels on bars
            for (i in 1:ncol(perf_matrix)) {
                for (j in 1:nrow(perf_matrix)) {
                    if (!is.na(perf_matrix[j, i])) {
                        text(bp[j, i], perf_matrix[j, i] + 2,
                             sprintf("%.1f", perf_matrix[j, i]),
                             cex = 0.7)
                    }
                }
            }

            TRUE
        },

        .calculateBinaryPerformanceCV = function(marker_data, outcome, marker_name) {
            # Cross-validation implementation
            n_folds <- 5
            n_samples <- length(outcome)
            fold_size <- floor(n_samples / n_folds)

            # Initialize metrics storage
            cv_sensitivities <- numeric(n_folds)
            cv_specificities <- numeric(n_folds)
            cv_ppvs <- numeric(n_folds)
            cv_npvs <- numeric(n_folds)
            cv_accuracies <- numeric(n_folds)
            cv_aucs <- numeric(n_folds)

            # Shuffle indices
            set.seed(123)  # For reproducibility
            shuffled_idx <- sample(n_samples)

            for (fold in 1:n_folds) {
                # Define fold indices
                test_start <- (fold - 1) * fold_size + 1
                test_end <- min(fold * fold_size, n_samples)
                test_idx <- shuffled_idx[test_start:test_end]
                train_idx <- shuffled_idx[-c(test_start:test_end)]

                # Split data
                train_marker <- marker_data[train_idx]
                train_outcome <- outcome[train_idx]
                test_marker <- marker_data[test_idx]
                test_outcome <- outcome[test_idx]

                # Find optimal cutpoint on training set
                cutpoint <- private$.findOptimalCutpoint(train_marker, train_outcome, marker_name)

                # Evaluate on test set
                predicted <- ifelse(test_marker >= cutpoint, 1, 0)
                tp <- sum(predicted == 1 & test_outcome == 1)
                tn <- sum(predicted == 0 & test_outcome == 0)
                fp <- sum(predicted == 1 & test_outcome == 0)
                fn <- sum(predicted == 0 & test_outcome == 1)

                n_pos <- tp + fn
                n_neg <- tn + fp

                cv_sensitivities[fold] <- if (n_pos > 0) tp / n_pos * 100 else NA
                cv_specificities[fold] <- if (n_neg > 0) tn / n_neg * 100 else NA
                cv_ppvs[fold] <- if (tp + fp > 0) tp / (tp + fp) * 100 else NA
                cv_npvs[fold] <- if (tn + fn > 0) tn / (tn + fn) * 100 else NA
                cv_accuracies[fold] <- (tp + tn) / length(test_outcome) * 100

                # Calculate AUC for this fold
                roc_result <- private$.calculateROC(test_marker, test_outcome)
                cv_aucs[fold] <- roc_result$auc
            }

            # Calculate overall metrics (mean across folds)
            sensitivity <- mean(cv_sensitivities, na.rm = TRUE)
            specificity <- mean(cv_specificities, na.rm = TRUE)
            ppv <- mean(cv_ppvs, na.rm = TRUE)
            npv <- mean(cv_npvs, na.rm = TRUE)
            accuracy <- mean(cv_accuracies, na.rm = TRUE)
            auc <- mean(cv_aucs, na.rm = TRUE)

            # Calculate final cutpoint on full dataset
            final_cutpoint <- private$.findOptimalCutpoint(marker_data, outcome, marker_name)

            # Store ROC data for plotting (using full dataset)
            roc_result <- private$.calculateROC(marker_data, outcome)
            if (is.null(private$.roc_data)) {
                private$.roc_data <- list()
            }
            private$.roc_data[[marker_name]] <- roc_result

            return(list(
                marker = marker_name,
                cutpoint = final_cutpoint,
                sensitivity = sensitivity,
                specificity = specificity,
                ppv = ppv,
                npv = npv,
                accuracy = accuracy,
                auc = auc
            ))
        },

        .getClinicalCutpoint = function(marker_name, marker_data) {
            # Clinical cutpoints based on common IHC markers
            # These are examples - should be customized based on clinical guidelines

            if (!is.null(marker_name)) {
                marker_lower <- tolower(marker_name)

                # Breast cancer markers
                if (grepl("er|estrogen", marker_lower)) {
                    # Allred score ≥3 or >1% positive cells
                    return(quantile(marker_data, 0.1, na.rm = TRUE))  # Low threshold for ER
                } else if (grepl("pr|progesterone", marker_lower)) {
                    # Allred score ≥3 or >1% positive cells
                    return(quantile(marker_data, 0.1, na.rm = TRUE))  # Low threshold for PR
                } else if (grepl("her2", marker_lower)) {
                    # IHC 3+ considered positive
                    return(quantile(marker_data, 0.9, na.rm = TRUE))  # High threshold for HER2
                } else if (grepl("ki67|ki-67|mib1", marker_lower)) {
                    # Common cutoffs: 14% for luminal A/B, 20% for high proliferation
                    return(quantile(marker_data, 0.5, na.rm = TRUE))  # Median for Ki67
                }

                # Lymphoma markers
                else if (grepl("cd20", marker_lower)) {
                    return(quantile(marker_data, 0.8, na.rm = TRUE))  # B-cell marker
                } else if (grepl("cd3", marker_lower)) {
                    return(quantile(marker_data, 0.8, na.rm = TRUE))  # T-cell marker
                } else if (grepl("cd30", marker_lower)) {
                    return(quantile(marker_data, 0.75, na.rm = TRUE))  # Hodgkin marker
                }

                # Carcinoma markers
                else if (grepl("ck7|cytokeratin.7", marker_lower)) {
                    return(quantile(marker_data, 0.8, na.rm = TRUE))
                } else if (grepl("ck20|cytokeratin.20", marker_lower)) {
                    return(quantile(marker_data, 0.8, na.rm = TRUE))
                } else if (grepl("ttf1|ttf-1", marker_lower)) {
                    return(quantile(marker_data, 0.75, na.rm = TRUE))  # Lung/thyroid
                }

                # Neuroendocrine markers
                else if (grepl("chromogranin|synaptophysin", marker_lower)) {
                    return(quantile(marker_data, 0.7, na.rm = TRUE))
                }
            }

            # Default: use 75th percentile for high expression
            return(quantile(marker_data, 0.75, na.rm = TRUE))
        },

        .generateClinicalSummary = function() {
            tryCatch({
                if (is.null(self$results) || is.null(self$results[["clinicalSummary"]])) {
                    return()
                }
                n_samples <- nrow(self$data)
                n_markers <- length(self$options$markers)
                n_groups <- length(levels(private$.diagnosis_groups))

                # Find best performing marker
                best_marker <- NULL
                best_auc <- 0
                best_accuracy <- 0

                if (length(private$.performance_results) > 0) {
                    for (name in names(private$.performance_results)) {
                        perf <- private$.performance_results[[name]]
                        if (!is.na(perf$auc) && perf$auc > best_auc) {
                            best_auc <- perf$auc
                            best_accuracy <- perf$accuracy
                            best_marker <- perf$marker
                        }
                    }
                }

                # Calculate differential diagnosis accuracy if available
                correct_predictions <- 0
                total_predictions <- 0

                if (!is.null(private$.clusters)) {
                    cluster_mapping <- private$.mapClustersToGroups()
                    predicted_labels <- cluster_mapping[private$.clusters]
                    actual_labels <- as.character(private$.diagnosis_groups)
                    correct_predictions <- sum(predicted_labels == actual_labels)
                    total_predictions <- length(actual_labels)
                }

                # Generate clinical interpretation
                summary_text <- sprintf(
                    "<h3>Clinical Summary</h3>
                    <div style='background-color: #f0f8ff; padding: 15px; border-radius: 5px;'>
                    <p><b>Analysis Overview:</b></p>
                    <ul>
                        <li>Analyzed %d cases with %d IHC markers across %d diagnostic groups</li>
                        <li>Method: %s with %s cutpoint selection</li>
                        <li>Confidence level: %.0f%%</li>
                    </ul>",
                    n_samples, n_markers, n_groups,
                    ifelse(self$options$crossValidation, "5-fold cross-validation", "Single evaluation"),
                    self$options$cutpointMethod,
                    self$options$confidenceLevel * 100
                )

                if (!is.null(best_marker)) {
                    summary_text <- paste0(summary_text, sprintf(
                        "<p><b>Top Performing Marker:</b></p>
                        <ul>
                            <li>%s achieved the best diagnostic performance</li>
                            <li>AUC: %.3f (%.1f%% accuracy)</li>
                            <li>Clinical interpretation: %s</li>
                        </ul>",
                        best_marker, best_auc, best_accuracy,
                        if (best_auc > 0.9) "Excellent diagnostic value"
                        else if (best_auc > 0.8) "Good diagnostic value"
                        else if (best_auc > 0.7) "Fair diagnostic value"
                        else "Limited diagnostic value"
                    ))
                }

                if (total_predictions > 0) {
                    diff_accuracy <- (correct_predictions / total_predictions) * 100
                    summary_text <- paste0(summary_text, sprintf(
                        "<p><b>Differential Diagnosis Performance:</b></p>
                        <ul>
                            <li>Overall accuracy: %.1f%% (%d/%d cases correctly classified)</li>
                            <li>Clustering method: %s</li>
                            <li>Clinical utility: %s</li>
                        </ul>",
                        diff_accuracy, correct_predictions, total_predictions,
                        self$options$clusterMethod,
                        if (diff_accuracy > 85) "Ready for clinical validation"
                        else if (diff_accuracy > 70) "Promising, needs optimization"
                        else "Requires additional markers or refinement"
                    ))
                }

                # Add copy-ready report sentence
                report_sentence <- private$.generateReportSentence()
                summary_text <- paste0(summary_text, sprintf(
                    "<p><b>Report Summary (copy-ready):</b></p>
                    <div style='background-color: white; padding: 10px; border: 1px solid #ccc; border-radius: 3px;'>
                    <p>%s</p>
                    </div>
                    </div>",
                    report_sentence
                ))

                self$results$clinicalSummary$setContent(summary_text)
                self$results$clinicalSummary$setVisible(TRUE)
            }, error = function(e) {
                # Handle any errors in summary generation
                if (!is.null(self$results[["clinicalSummary"]])) {
                    self$results$clinicalSummary$setContent(
                        "<p>Error generating clinical summary. Please check your data and settings.</p>"
                    )
                    self$results$clinicalSummary$setVisible(TRUE)
                }
            })
        },

        .generateReportSentence = function() {
            n_samples <- nrow(self$data)
            n_markers <- length(self$options$markers)
            n_groups <- length(levels(private$.diagnosis_groups))

            # Find best marker
            best_marker <- NULL
            best_auc <- 0

            if (length(private$.performance_results) > 0) {
                for (name in names(private$.performance_results)) {
                    perf <- private$.performance_results[[name]]
                    if (!is.na(perf$auc) && perf$auc > best_auc) {
                        best_auc <- perf$auc
                        best_marker <- perf$marker
                    }
                }
            }

            cv_text <- ifelse(self$options$crossValidation, "with 5-fold cross-validation", "")

            sentence <- sprintf(
                "Immunohistochemical diagnostic analysis was performed on %d cases using %d markers %s. ",
                n_samples, n_markers, cv_text
            )

            if (!is.null(best_marker)) {
                sentence <- paste0(sentence, sprintf(
                    "%s demonstrated the highest diagnostic performance (AUC = %.3f), ",
                    best_marker, best_auc
                ))

                if (best_auc > 0.8) {
                    sentence <- paste0(sentence, "indicating good discriminatory ability for the diagnostic groups evaluated. ")
                } else {
                    sentence <- paste0(sentence, "suggesting moderate discriminatory ability that may benefit from additional markers. ")
                }
            }

            if (!is.null(private$.clusters)) {
                cluster_mapping <- private$.mapClustersToGroups()
                predicted_labels <- cluster_mapping[private$.clusters]
                actual_labels <- as.character(private$.diagnosis_groups)
                accuracy <- mean(predicted_labels == actual_labels) * 100

                sentence <- paste0(sentence, sprintf(
                    "Differential diagnosis using %s clustering achieved %.1f%% accuracy. ",
                    self$options$clusterMethod, accuracy
                ))
            }

            return(sentence)
        },

        .showInterpretationGuide = function() {
            tryCatch({
                if (is.null(self$results) || is.null(self$results[["interpretationGuide"]])) {
                    return()
                }
                guide_html <- "
                <h3>Interpretation Guide</h3>
                <div style='background-color: #fffacd; padding: 15px; border-radius: 5px;'>

                <h4>Understanding Diagnostic Metrics:</h4>
                <ul>
                    <li><b>Sensitivity:</b> % of true positive cases correctly identified (ability to detect disease)</li>
                    <li><b>Specificity:</b> % of true negative cases correctly identified (ability to exclude disease)</li>
                    <li><b>PPV:</b> Positive Predictive Value - probability that positive test = true disease</li>
                    <li><b>NPV:</b> Negative Predictive Value - probability that negative test = no disease</li>
                    <li><b>AUC:</b> Area Under ROC Curve (0.5 = random, 1.0 = perfect)</li>
                </ul>

                <h4>Clinical Interpretation Guidelines:</h4>
                <table style='width: 100%; border-collapse: collapse;'>
                    <tr style='background-color: #e6e6e6;'>
                        <th style='padding: 8px; border: 1px solid #ddd;'>AUC Range</th>
                        <th style='padding: 8px; border: 1px solid #ddd;'>Diagnostic Value</th>
                        <th style='padding: 8px; border: 1px solid #ddd;'>Clinical Application</th>
                    </tr>
                    <tr>
                        <td style='padding: 8px; border: 1px solid #ddd;'>0.90-1.00</td>
                        <td style='padding: 8px; border: 1px solid #ddd;'>Excellent</td>
                        <td style='padding: 8px; border: 1px solid #ddd;'>Standalone diagnostic test</td>
                    </tr>
                    <tr>
                        <td style='padding: 8px; border: 1px solid #ddd;'>0.80-0.89</td>
                        <td style='padding: 8px; border: 1px solid #ddd;'>Good</td>
                        <td style='padding: 8px; border: 1px solid #ddd;'>Useful in diagnostic panel</td>
                    </tr>
                    <tr>
                        <td style='padding: 8px; border: 1px solid #ddd;'>0.70-0.79</td>
                        <td style='padding: 8px; border: 1px solid #ddd;'>Fair</td>
                        <td style='padding: 8px; border: 1px solid #ddd;'>Supportive with other markers</td>
                    </tr>
                    <tr>
                        <td style='padding: 8px; border: 1px solid #ddd;'>0.60-0.69</td>
                        <td style='padding: 8px; border: 1px solid #ddd;'>Poor</td>
                        <td style='padding: 8px; border: 1px solid #ddd;'>Limited diagnostic value</td>
                    </tr>
                    <tr>
                        <td style='padding: 8px; border: 1px solid #ddd;'>&lt;0.60</td>
                        <td style='padding: 8px; border: 1px solid #ddd;'>Fail</td>
                        <td style='padding: 8px; border: 1px solid #ddd;'>Not diagnostically useful</td>
                    </tr>
                </table>

                <h4>Common Clinical Scenarios:</h4>
                <ul>
                    <li><b>Screening test:</b> Prioritize high sensitivity (catch all disease)</li>
                    <li><b>Confirmatory test:</b> Prioritize high specificity (avoid false positives)</li>
                    <li><b>Balanced diagnostic:</b> Optimize Youden index (sensitivity + specificity - 1)</li>
                </ul>

                <h4>Important Considerations:</h4>
                <ul>
                    <li>Performance metrics depend on disease prevalence in your population</li>
                    <li>Consider pre-test probability when interpreting results</li>
                    <li>Validate cutpoints on independent cohort before clinical implementation</li>
                    <li>Account for technical variations (antibody clones, staining protocols)</li>
                </ul>
                </div>"

                self$results$interpretationGuide$setContent(guide_html)
                self$results$interpretationGuide$setVisible(TRUE)
            }, error = function(e) {
                # Handle any errors in guide generation
                if (!is.null(self$results[["interpretationGuide"]])) {
                    self$results$interpretationGuide$setContent(
                        "<p>Error generating interpretation guide.</p>"
                    )
                    self$results$interpretationGuide$setVisible(TRUE)
                }
            })
        }
    )
)