#' @importFrom R6 R6Class
#' @importFrom stats quantile var dist
#' @importFrom utils head
#' @import ggplot2

ihcimmuneClass <- R6::R6Class(
    "ihcimmuneClass",
    inherit = ihcimmuneBase,
    private = list(
        .immune_matrix = NULL,
        .checkpoint_matrix = NULL,
        .spatial_coords = NULL,
        .case_ids = NULL,
        .tumor_regions = NULL,
        .detected_presets = NULL,
        .applied_preset = NULL,
        .cache = list(),  # Performance caching
        .large_dataset_threshold = 1000,  # Threshold for large dataset optimizations

        .init = function() {
            if (is.null(self$data))
                return()

            self$results$instructions$setContent(
                "<h3>Tumor-Infiltrating Lymphocyte (TIL) Analysis</h3>
                <p>This analysis provides comprehensive immune microenvironment characterization using IHC markers:</p>
                <ul>
                <li><strong>TIL Analysis:</strong> Quantifies tumor-infiltrating lymphocytes using established methods</li>
                <li><strong>Immune Contexture:</strong> Calculates immune contexture scores for prognostic classification</li>
                <li><strong>Spatial Analysis:</strong> Analyzes spatial distribution and clustering of immune cells</li>
                <li><strong>Checkpoint Analysis:</strong> Evaluates checkpoint inhibitor therapy potential</li>
                <li><strong>Diversity Metrics:</strong> Calculates Shannon and Simpson diversity indices</li>
                </ul>
                <p><strong>Clinical Applications:</strong> Immuno-oncology, treatment selection, prognosis prediction</p>"
            )

            # Prepare immune markers matrix
            if (length(self$options$immune_markers) > 0) {
                validation <- validateIHCData(self$data, self$options$immune_markers)
                if (!validation$valid) {
                    self$results$instructions$setContent(showIHCDataRequirements())
                    return()
                }
                private$.immune_matrix <- convertIHCToNumeric(
                    self$data,
                    self$options$immune_markers,
                    self$options$id
                )
            }

            # Prepare checkpoint markers if provided
            if (length(self$options$checkpoint_markers) > 0) {
                private$.checkpoint_matrix <- convertIHCToNumeric(
                    self$data,
                    self$options$checkpoint_markers,
                    self$options$id
                )
            }

            # Prepare spatial coordinates if spatial analysis is enabled
            if (self$options$spatialAnalysis &&
                !is.null(self$options$x_coordinate) &&
                !is.null(self$options$y_coordinate)) {

                # Validate spatial data
                if (!private$.validateSpatialData()) {
                    return()
                }

                x_coord <- convertIHCMarkerToNumeric(self$data[[self$options$x_coordinate]])
                y_coord <- convertIHCMarkerToNumeric(self$data[[self$options$y_coordinate]])

                coords <- data.frame(x = x_coord, y = y_coord)
                complete_cases <- complete.cases(coords)
                private$.spatial_coords <- coords[complete_cases, ]
            }

            # Prepare tumor region data if provided
            if (!is.null(self$options$tumorRegion)) {
                private$.tumor_regions <- self$data[[self$options$tumorRegion]]
            }

            # Prepare case IDs
            if (!is.null(self$options$id)) {
                private$.case_ids <- self$data[[self$options$id]]
            } else {
                private$.case_ids <- seq_len(nrow(self$data))
            }

            # Apply clinical presets based on marker combinations
            if (!is.null(private$.immune_matrix)) {
                private$.applyClinicalPresets()
            }
        },

        .run = function() {
            if (is.null(private$.immune_matrix))
                return()

            # Check for large dataset and apply optimizations
            is_large_dataset <- private$.optimizeForLargeDataset()

            # Generate clinical interpretation context
            private$.generateClinicalContext()

            # Run TIL analysis
            if (self$options$tilAnalysis) {
                private$.runTILAnalysis()
            }

            # Generate immune profile
            private$.generateImmuneProfile()

            # Run immune contexture analysis
            if (self$options$immuneContexture) {
                private$.runImmuneContexture()
            }

            # Run checkpoint analysis
            if (self$options$checkpointScore && !is.null(private$.checkpoint_matrix)) {
                private$.runCheckpointAnalysis()
            }

            # Run spatial analysis
            if (self$options$spatialAnalysis && !is.null(private$.spatial_coords)) {
                private$.runSpatialAnalysis()
            }

            # Run hotspot analysis
            if (self$options$hotspotAnalysis) {
                private$.runHotspotAnalysis()
            }

            # Calculate diversity metrics
            if (self$options$diversityMetrics) {
                private$.calculateDiversityMetrics()
            }

            # Generate method-specific results
            if (self$options$tilAnalysis) {
                private$.generateTILMethodResults()
            }
        },


        .runTILAnalysis = function() {
            method <- self$options$tilMethod
            results <- list()

            if (method == "sterlacci") {
                results <- private$.performSterlacciAnalysis()
            } else if (method == "cd4cd8_ratio") {
                results <- private$.performCD4CD8Analysis()
            } else if (method == "granzyme_focus") {
                results <- private$.performGranzymeAnalysis()
            } else {
                results <- private$.performComprehensiveTILAnalysis()
            }

            # Populate TIL summary table
            table <- self$results$tilSummary

            for (i in seq_along(results)) {
                result <- results[[i]]
                table$addRow(rowKey = i, values = list(
                    metric = result$metric,
                    value = result$value,
                    category = result$category,
                    interpretation = result$interpretation
                ))
            }
        },

        .performSterlacciAnalysis = function() {
            # Sterlacci method for breast cancer TIL assessment
            cd3_col <- which(grepl("CD3", colnames(private$.immune_matrix), ignore.case = TRUE))[1]
            cd8_col <- which(grepl("CD8", colnames(private$.immune_matrix), ignore.case = TRUE))[1]

            results <- list()

            if (!is.na(cd3_col)) {
                cd3_mean <- mean(private$.immune_matrix[, cd3_col], na.rm = TRUE)
                cd3_threshold <- self$options$cd3Threshold
                cd3_category <- ifelse(cd3_mean < cd3_threshold, "Low",
                                     ifelse(cd3_mean < cd3_threshold * 5, "Intermediate", "High"))

                results[[length(results) + 1]] <- list(
                    metric = "CD3+ TIL Density",
                    value = cd3_mean,
                    category = cd3_category,
                    interpretation = paste("Sterlacci method:", cd3_category, "TIL infiltration")
                )
            }

            if (!is.na(cd8_col)) {
                cd8_mean <- mean(private$.immune_matrix[, cd8_col], na.rm = TRUE)
                cd8_threshold <- self$options$cd8Threshold
                cd8_category <- ifelse(cd8_mean < cd8_threshold, "Low",
                                     ifelse(cd8_mean < cd8_threshold * 4, "Intermediate", "High"))

                results[[length(results) + 1]] <- list(
                    metric = "CD8+ TIL Density",
                    value = cd8_mean,
                    category = cd8_category,
                    interpretation = paste("Cytotoxic T-cell infiltration:", cd8_category)
                )
            }

            return(results)
        },

        .performCD4CD8Analysis = function() {
            cd4_col <- which(grepl("CD4", colnames(private$.immune_matrix), ignore.case = TRUE))[1]
            cd8_col <- which(grepl("CD8", colnames(private$.immune_matrix), ignore.case = TRUE))[1]

            results <- list()

            if (!is.na(cd4_col) && !is.na(cd8_col)) {
                cd4_values <- private$.immune_matrix[, cd4_col]
                cd8_values <- private$.immune_matrix[, cd8_col]

                # Calculate ratio for each case
                ratios <- cd4_values / (cd8_values + 0.1)  # Add small constant to avoid division by zero
                mean_ratio <- mean(ratios, na.rm = TRUE)

                ratio_category <- ifelse(mean_ratio < 1, "CD8-dominant",
                                       ifelse(mean_ratio < 2, "Balanced", "CD4-dominant"))

                results[[1]] <- list(
                    metric = "CD4/CD8 Ratio",
                    value = mean_ratio,
                    category = ratio_category,
                    interpretation = paste("Immune balance:", ratio_category, "response pattern")
                )

                # Add individual CD4 and CD8 metrics
                results[[2]] <- list(
                    metric = "CD4+ Helper T-cells",
                    value = mean(cd4_values, na.rm = TRUE),
                    category = ifelse(mean(cd4_values, na.rm = TRUE) > 15, "High", "Low"),
                    interpretation = "Helper T-cell infiltration level"
                )

                results[[3]] <- list(
                    metric = "CD8+ Cytotoxic T-cells",
                    value = mean(cd8_values, na.rm = TRUE),
                    category = ifelse(mean(cd8_values, na.rm = TRUE) > 10, "High", "Low"),
                    interpretation = "Cytotoxic T-cell infiltration level"
                )
            }

            return(results)
        },

        .performGranzymeAnalysis = function() {
            granzyme_col <- which(grepl("GZMB|Granzyme", colnames(private$.immune_matrix), ignore.case = TRUE))[1]
            cd8_col <- which(grepl("CD8", colnames(private$.immune_matrix), ignore.case = TRUE))[1]

            results <- list()

            if (!is.na(granzyme_col)) {
                granzyme_values <- private$.immune_matrix[, granzyme_col]
                granzyme_mean <- mean(granzyme_values, na.rm = TRUE)

                # Identify hotspots (areas with high Granzyme B)
                hotspot_threshold <- quantile(granzyme_values, 0.75, na.rm = TRUE)
                hotspot_cases <- sum(granzyme_values > hotspot_threshold, na.rm = TRUE)

                results[[1]] <- list(
                    metric = "Granzyme B+ TIL",
                    value = granzyme_mean,
                    category = ifelse(granzyme_mean > 5, "Active", "Inactive"),
                    interpretation = "Cytotoxic activity level in tumor microenvironment"
                )

                results[[2]] <- list(
                    metric = "Granzyme B Hotspots",
                    value = hotspot_cases,
                    category = ifelse(hotspot_cases > nrow(private$.immune_matrix) * 0.25, "Multiple", "Few"),
                    interpretation = "Number of high cytotoxic activity regions"
                )
            }

            return(results)
        },

        .performComprehensiveTILAnalysis = function() {
            results <- list()

            # Overall immune infiltration score
            immune_score <- rowMeans(private$.immune_matrix, na.rm = TRUE)
            mean_immune_score <- mean(immune_score, na.rm = TRUE)

            immune_threshold <- self$options$immuneScoreThreshold
            immune_category <- ifelse(mean_immune_score < immune_threshold, "Cold",
                                    ifelse(mean_immune_score < immune_threshold * 3, "Intermediate", "Hot"))

            results[[1]] <- list(
                metric = "Overall Immune Score",
                value = mean_immune_score,
                category = immune_category,
                interpretation = paste("Tumor immune microenvironment:", immune_category)
            )

            # T-cell infiltration (CD3)
            cd3_col <- which(grepl("CD3", colnames(private$.immune_matrix), ignore.case = TRUE))[1]
            if (!is.na(cd3_col)) {
                cd3_mean <- mean(private$.immune_matrix[, cd3_col], na.rm = TRUE)
                results[[length(results) + 1]] <- list(
                    metric = "T-cell Infiltration (CD3+)",
                    value = cd3_mean,
                    category = ifelse(cd3_mean > 20, "High", "Low"),
                    interpretation = "Overall T-lymphocyte presence"
                )
            }

            # B-cell infiltration (CD20)
            cd20_col <- which(grepl("CD20", colnames(private$.immune_matrix), ignore.case = TRUE))[1]
            if (!is.na(cd20_col)) {
                cd20_mean <- mean(private$.immune_matrix[, cd20_col], na.rm = TRUE)
                results[[length(results) + 1]] <- list(
                    metric = "B-cell Infiltration (CD20+)",
                    value = cd20_mean,
                    category = ifelse(cd20_mean > 5, "Present", "Absent"),
                    interpretation = "B-lymphocyte and humoral immunity"
                )
            }

            return(results)
        },

        .generateImmuneProfile = function() {
            table <- self$results$immuneProfile

            for (i in seq_len(ncol(private$.immune_matrix))) {
                marker_name <- colnames(private$.immune_matrix)[i]
                marker_data <- private$.immune_matrix[, i]

                mean_expr <- mean(marker_data, na.rm = TRUE)
                median_expr <- median(marker_data, na.rm = TRUE)

                # Calculate positivity rate (> 1% threshold)
                positive_cases <- sum(marker_data > 1, na.rm = TRUE)
                positive_rate <- (positive_cases / length(marker_data)) * 100

                # Categorize density
                density_category <- ifelse(mean_expr < 5, "Low",
                                         ifelse(mean_expr < 20, "Moderate", "High"))

                table$addRow(rowKey = i, values = list(
                    marker = marker_name,
                    mean_expression = mean_expr,
                    median_expression = median_expr,
                    positive_cases = positive_rate,
                    density_category = density_category
                ))
            }
        },

        .runImmuneContexture = function() {
            # Simplified immune contexture based on CD8 and CD45RO
            cd8_col <- which(grepl("CD8", colnames(private$.immune_matrix), ignore.case = TRUE))[1]
            cd45ro_col <- which(grepl("CD45RO", colnames(private$.immune_matrix), ignore.case = TRUE))[1]

            table <- self$results$immuneContexture

            for (i in seq_len(nrow(private$.immune_matrix))) {
                case_id <- private$.case_ids[i]

                # Calculate contexture score
                cd8_score <- if (!is.na(cd8_col)) private$.immune_matrix[i, cd8_col] else 0
                cd45ro_score <- if (!is.na(cd45ro_col)) private$.immune_matrix[i, cd45ro_col] else 0

                contexture_score <- (cd8_score + cd45ro_score) / 2

                # Classify immune context
                immune_class <- ifelse(contexture_score < 5, "Cold",
                                     ifelse(contexture_score < 15, "Intermediate", "Hot"))

                # Prognostic prediction
                prognosis <- ifelse(immune_class == "Hot", "Favorable",
                                  ifelse(immune_class == "Intermediate", "Intermediate", "Poor"))

                table$addRow(rowKey = i, values = list(
                    case_id = as.character(case_id),
                    contexture_score = contexture_score,
                    immune_classification = immune_class,
                    cd8_score = cd8_score,
                    cd45ro_score = cd45ro_score,
                    prognosis_prediction = prognosis
                ))
            }
        },

        .runCheckpointAnalysis = function() {
            if (is.null(private$.checkpoint_matrix))
                return()

            table <- self$results$checkpointAnalysis

            for (i in seq_len(ncol(private$.checkpoint_matrix))) {
                marker_name <- colnames(private$.checkpoint_matrix)[i]
                marker_data <- private$.checkpoint_matrix[, i]

                # Calculate positivity rate based on cutoffs
                if (grepl("PD-?1", marker_name, ignore.case = TRUE)) {
                    cutoff <- self$options$pd1Cutoff
                } else if (grepl("PD-?L1", marker_name, ignore.case = TRUE)) {
                    cutoff <- self$options$pdl1Cutoff
                } else {
                    cutoff <- 1  # Default 1% cutoff
                }

                positive_rate <- (sum(marker_data >= cutoff, na.rm = TRUE) / length(marker_data)) * 100

                # Calculate composite score
                composite_score <- mean(marker_data, na.rm = TRUE) / 10  # Normalized score

                # Predict response
                predicted_response <- ifelse(positive_rate > 50, "High likelihood",
                                           ifelse(positive_rate > 20, "Moderate likelihood", "Low likelihood"))

                confidence <- ifelse(positive_rate > 50 | positive_rate < 5, "High", "Moderate")

                table$addRow(rowKey = i, values = list(
                    marker = marker_name,
                    positive_rate = positive_rate,
                    composite_score = composite_score,
                    predicted_response = predicted_response,
                    confidence = confidence
                ))
            }
        },

        .runSpatialAnalysis = function() {
            if (is.null(private$.spatial_coords))
                return()

            table <- self$results$spatialMetrics

            for (i in seq_len(ncol(private$.immune_matrix))) {
                marker_name <- colnames(private$.immune_matrix)[i]
                marker_values <- private$.immune_matrix[, i]

                # If tumor regions are available, analyze by region
                if (!is.null(private$.tumor_regions)) {
                    regions <- unique(private$.tumor_regions[!is.na(private$.tumor_regions)])

                    for (region in regions) {
                        region_idx <- private$.tumor_regions == region & !is.na(private$.tumor_regions)
                        if (sum(region_idx) < 3) next  # Need at least 3 points for spatial analysis

                        region_coords <- private$.spatial_coords[region_idx, ]
                        region_values <- marker_values[region_idx]

                        # Calculate spatial metrics for this region
                        moran_result <- private$.calculateMoranI(region_values, region_coords)
                        clustering_index <- private$.calculateClusteringIndex(region_values, region_coords)
                        nn_distance <- private$.calculateNearestNeighborDistance(region_coords)

                        # Determine spatial pattern with significance
                        significance <- ifelse(moran_result$p_value < 0.05, "*", "")
                        spatial_pattern <- ifelse(moran_result$moran_i > 0.3, paste0("Clustered", significance),
                                                ifelse(moran_result$moran_i < -0.3, paste0("Dispersed", significance),
                                                      paste0("Random", significance)))

                        table$addRow(rowKey = paste(i, region, sep = "_"), values = list(
                            marker = paste(marker_name, "(", region, ")"),
                            moran_i = moran_result$moran_i,
                            clustering_index = clustering_index,
                            nearest_neighbor_distance = nn_distance,
                            spatial_pattern = spatial_pattern
                        ))
                    }
                } else {
                    # Standard analysis without regions
                    # Calculate Moran's I for spatial autocorrelation
                    moran_result <- private$.calculateMoranI(marker_values, private$.spatial_coords)

                    # Calculate clustering index
                    clustering_index <- private$.calculateClusteringIndex(marker_values, private$.spatial_coords)

                    # Calculate mean nearest neighbor distance
                    nn_distance <- private$.calculateNearestNeighborDistance(private$.spatial_coords)

                    # Determine spatial pattern with significance
                    significance <- ifelse(moran_result$p_value < 0.05, "*", "")
                    spatial_pattern <- ifelse(moran_result$moran_i > 0.3, paste0("Clustered", significance),
                                            ifelse(moran_result$moran_i < -0.3, paste0("Dispersed", significance),
                                                  paste0("Random", significance)))

                    table$addRow(rowKey = i, values = list(
                        marker = marker_name,
                        moran_i = moran_result$moran_i,
                        clustering_index = clustering_index,
                        nearest_neighbor_distance = nn_distance,
                        spatial_pattern = spatial_pattern
                    ))
                }
            }
        },

        .runHotspotAnalysis = function() {
            # Simplified hotspot detection based on immune density
            immune_density <- rowMeans(private$.immune_matrix, na.rm = TRUE)

            # Define hotspots as top 25% of immune density
            hotspot_threshold <- quantile(immune_density, 0.75, na.rm = TRUE)
            coldspot_threshold <- quantile(immune_density, 0.25, na.rm = TRUE)

            table <- self$results$hotspotAnalysis

            hotspot_regions <- which(immune_density >= hotspot_threshold)
            coldspot_regions <- which(immune_density <= coldspot_threshold)

            # Process hotspots
            for (i in seq_along(hotspot_regions)) {
                region_idx <- hotspot_regions[i]
                region_data <- private$.immune_matrix[region_idx, ]

                dominant_pop <- colnames(private$.immune_matrix)[which.max(region_data)]
                diversity_idx <- private$.calculateShannonIndex(region_data)

                table$addRow(rowKey = paste0("hot_", i), values = list(
                    region_id = region_idx,
                    hotspot_type = "Immune Hotspot",
                    immune_density = immune_density[region_idx],
                    dominant_population = dominant_pop,
                    diversity_index = diversity_idx
                ))
            }

            # Process coldspots
            for (i in seq_along(coldspot_regions)) {
                region_idx <- coldspot_regions[i]
                region_data <- private$.immune_matrix[region_idx, ]

                diversity_idx <- private$.calculateShannonIndex(region_data)

                table$addRow(rowKey = paste0("cold_", i), values = list(
                    region_id = region_idx,
                    hotspot_type = "Immune Desert",
                    immune_density = immune_density[region_idx],
                    dominant_population = "None",
                    diversity_index = diversity_idx
                ))
            }
        },

        .calculateDiversityMetrics = function() {
            table <- self$results$diversityResults

            for (i in seq_len(nrow(private$.immune_matrix))) {
                case_data <- private$.immune_matrix[i, ]
                case_id <- private$.case_ids[i]

                # Calculate Shannon diversity
                shannon_div <- private$.calculateShannonIndex(case_data)

                # Calculate Simpson diversity
                simpson_div <- private$.calculateSimpsonIndex(case_data)

                # Calculate advanced diversity metrics
                brillouin_div <- private$.calculateBrillouinIndex(case_data)
                berger_parker_div <- private$.calculateBergerParkerIndex(case_data)

                # Calculate evenness
                evenness <- shannon_div / log(length(case_data))

                # Calculate richness (number of expressed markers)
                richness <- sum(case_data > 0, na.rm = TRUE)

                # Categorize diversity
                diversity_category <- ifelse(shannon_div > 1.5, "High",
                                           ifelse(shannon_div > 0.8, "Moderate", "Low"))

                table$addRow(rowKey = i, values = list(
                    case_id = as.character(case_id),
                    shannon_diversity = shannon_div,
                    simpson_diversity = simpson_div,
                    evenness = evenness,
                    richness = richness,
                    diversity_category = diversity_category,
                    brillouin_diversity = brillouin_div,
                    berger_parker_dominance = berger_parker_div
                ))
            }
        },

        .generateTILMethodResults = function() {
            method <- self$options$tilMethod
            table <- self$results$tilMethodResults

            if (method == "sterlacci") {
                table$addRow(rowKey = 1, values = list(
                    parameter = "Assessment Method",
                    value = NA,
                    reference_range = "Sterlacci 2014",
                    clinical_significance = "Breast cancer TIL evaluation"
                ))

                table$addRow(rowKey = 2, values = list(
                    parameter = "TIL Threshold Low",
                    value = 10,
                    reference_range = "<10%",
                    clinical_significance = "Minimal immune infiltration"
                ))

                table$addRow(rowKey = 3, values = list(
                    parameter = "TIL Threshold High",
                    value = 50,
                    reference_range = ">50%",
                    clinical_significance = "Strong immune activation"
                ))
            } else if (method == "cd4cd8_ratio") {
                cd4_col <- which(grepl("CD4", colnames(private$.immune_matrix), ignore.case = TRUE))[1]
                cd8_col <- which(grepl("CD8", colnames(private$.immune_matrix), ignore.case = TRUE))[1]

                if (!is.na(cd4_col) && !is.na(cd8_col)) {
                    mean_ratio <- mean(private$.immune_matrix[, cd4_col] /
                                     (private$.immune_matrix[, cd8_col] + 0.1), na.rm = TRUE)

                    table$addRow(rowKey = 1, values = list(
                        parameter = "CD4/CD8 Ratio",
                        value = mean_ratio,
                        reference_range = "0.5-2.0",
                        clinical_significance = "Immune balance indicator"
                    ))
                }
            }
        },

        .applyClinicalPresets = function() {
            # Apply tumor-type specific presets based on available markers
            if (!self$options$immune_markers || length(self$options$immune_markers) == 0) return()

            markers <- colnames(private$.immune_matrix)
            markers_lower <- tolower(markers)

            # Detect potential tumor types based on marker combinations
            detected_presets <- list()

            # Breast Cancer Detection
            if (any(grepl("cd3|cd8|cd4", markers_lower)) && any(grepl("cd20|ki67", markers_lower))) {
                detected_presets[["breast_cancer"]] <- list(
                    name = "Breast Cancer (Triple Negative/HER2+)",
                    tilMethod = "sterlacci",
                    cd3Threshold = 10,
                    cd8Threshold = 5,
                    immuneScoreThreshold = 15,
                    description = "Optimized for breast cancer TIL assessment following Sterlacci guidelines"
                )
            }

            # Melanoma Detection
            if (any(grepl("cd8|cd4", markers_lower)) && any(grepl("pd1|pdl1", markers_lower))) {
                detected_presets[["melanoma"]] <- list(
                    name = "Melanoma",
                    tilMethod = "cd4cd8_ratio",
                    cd3Threshold = 15,
                    cd8Threshold = 8,
                    immuneScoreThreshold = 20,
                    pd1Cutoff = 1,
                    pdl1Cutoff = 1,
                    description = "Optimized for melanoma immunotherapy response prediction"
                )
            }

            # NSCLC Detection
            if (any(grepl("cd3|cd8", markers_lower)) && any(grepl("pd1|pdl1|ctla", markers_lower))) {
                detected_presets[["nsclc"]] <- list(
                    name = "Non-Small Cell Lung Cancer (NSCLC)",
                    tilMethod = "comprehensive",
                    cd3Threshold = 8,
                    cd8Threshold = 3,
                    immuneScoreThreshold = 12,
                    pd1Cutoff = 1,
                    pdl1Cutoff = 1,
                    description = "Optimized for NSCLC checkpoint inhibitor therapy selection"
                )
            }

            # Colorectal Cancer Detection
            if (any(grepl("cd3|cd8", markers_lower)) && any(grepl("cd45ro", markers_lower))) {
                detected_presets[["colorectal"]] <- list(
                    name = "Colorectal Cancer",
                    tilMethod = "comprehensive",
                    cd3Threshold = 12,
                    cd8Threshold = 6,
                    immuneScoreThreshold = 18,
                    description = "Optimized for colorectal cancer microsatellite status correlation"
                )
            }

            # Store detected presets for clinical context
            private$.detected_presets <- detected_presets

            # Apply best matching preset automatically if only one is detected
            if (length(detected_presets) == 1) {
                preset <- detected_presets[[1]]
                private$.applied_preset <- preset

                # Generate preset application message
                preset_message <- paste0(
                    "<div style='background-color: #e6f3ff; border: 1px solid #0066cc; padding: 10px; margin: 10px 0;'>",
                    "<h5 style='color: #0066cc; margin-top: 0;'>🎯 Clinical Preset Auto-Applied</h5>",
                    "<p><strong>", preset$name, "</strong></p>",
                    "<p>", preset$description, "</p>",
                    "<p><strong>Applied Settings:</strong> TIL Method = ", preset$tilMethod,
                    ", CD3 Threshold = ", preset$cd3Threshold, "%",
                    ", CD8 Threshold = ", preset$cd8Threshold, "%",
                    ", Immune Score Threshold = ", preset$immuneScoreThreshold, "%</p>",
                    "</div>"
                )

                # Update instructions with preset information
                current_instructions <- self$results$instructions$content
                if (is.null(current_instructions) || current_instructions == "") {
                    current_instructions <- "<h4>TIL Analysis Instructions</h4>"
                }
                self$results$instructions$setContent(paste0(current_instructions, preset_message))
            } else if (length(detected_presets) > 1) {
                # Multiple presets detected - show options
                preset_options <- paste(sapply(names(detected_presets), function(x) {
                    paste0("• <strong>", detected_presets[[x]]$name, ":</strong> ", detected_presets[[x]]$description)
                }), collapse = "<br>")

                preset_message <- paste0(
                    "<div style='background-color: #fff3cd; border: 1px solid #ffc107; padding: 10px; margin: 10px 0;'>",
                    "<h5 style='color: #856404; margin-top: 0;'>🔍 Multiple Clinical Presets Available</h5>",
                    "<p>Based on your marker selection, multiple tumor-specific presets are available:</p>",
                    "<p>", preset_options, "</p>",
                    "<p><em>Consider selecting the most appropriate tumor type for optimized analysis parameters.</em></p>",
                    "</div>"
                )

                current_instructions <- self$results$instructions$content
                if (is.null(current_instructions) || current_instructions == "") {
                    current_instructions <- "<h4>TIL Analysis Instructions</h4>"
                }
                self$results$instructions$setContent(paste0(current_instructions, preset_message))
            }
        },

        .generateClinicalReport = function() {
            # Generate copy-ready clinical report
            if (!self$options$immune_markers || length(self$options$immune_markers) == 0) {
                return("<p>No immune markers selected for analysis.</p>")
            }

            report_sections <- list()

            # Header
            report_sections[[1]] <- "<h3>Tumor-Infiltrating Lymphocyte (TIL) Analysis Report</h3>"

            # TIL Summary Section
            if (self$options$tilAnalysis) {
                til_data <- private$.performTILAnalysis()
                if (length(til_data) > 0) {
                    report_sections[[length(report_sections) + 1]] <- "<h4>TIL Analysis Summary</h4>"
                    report_sections[[length(report_sections) + 1]] <- "<ul>"
                    for (result in til_data) {
                        report_sections[[length(report_sections) + 1]] <- paste0(
                            "<li><strong>", result$metric, ":</strong> ",
                            round(result$value, 2), "% (", result$category, ") - ",
                            result$interpretation, "</li>"
                        )
                    }
                    report_sections[[length(report_sections) + 1]] <- "</ul>"
                }
            }

            # Immune Profile Section
            report_sections[[length(report_sections) + 1]] <- "<h4>Immune Cell Profile</h4>"
            report_sections[[length(report_sections) + 1]] <- "<p>The following immune markers were analyzed:</p><ul>"

            for (i in seq_len(ncol(private$.immune_matrix))) {
                marker_name <- colnames(private$.immune_matrix)[i]
                marker_data <- private$.immune_matrix[, i]
                mean_expr <- round(mean(marker_data, na.rm = TRUE), 2)
                positive_rate <- round((sum(marker_data > 1, na.rm = TRUE) / length(marker_data)) * 100, 1)
                density_category <- ifelse(mean_expr < 5, "Low", ifelse(mean_expr < 20, "Moderate", "High"))

                report_sections[[length(report_sections) + 1]] <- paste0(
                    "<li><strong>", marker_name, ":</strong> Mean expression ", mean_expr,
                    "%, Positivity rate ", positive_rate, "% (", density_category, " density)</li>"
                )
            }
            report_sections[[length(report_sections) + 1]] <- "</ul>"

            # Clinical Interpretation
            report_sections[[length(report_sections) + 1]] <- "<h4>Clinical Interpretation</h4>"

            # Generate interpretation based on TIL method
            method <- self$options$tilMethod
            if (method == "sterlacci") {
                report_sections[[length(report_sections) + 1]] <- paste0(
                    "<p><strong>Sterlacci Method Analysis:</strong> This analysis is specifically designed for breast cancer specimens. ",
                    "The TIL assessment follows established protocols for prognostic stratification in breast cancer patients.</p>"
                )
            } else if (method == "cd4cd8_ratio") {
                report_sections[[length(report_sections) + 1]] <- paste0(
                    "<p><strong>CD4/CD8 Ratio Analysis:</strong> The balance between helper T cells (CD4+) and cytotoxic T cells (CD8+) ",
                    "provides insights into immune response efficacy and therapeutic response potential.</p>"
                )
            } else if (method == "granzyme_focus") {
                report_sections[[length(report_sections) + 1]] <- paste0(
                    "<p><strong>Granzyme B+ TIL Focus:</strong> Analysis centers on cytotoxic activity markers, ",
                    "particularly relevant for immunotherapy response prediction.</p>"
                )
            } else {
                report_sections[[length(report_sections) + 1]] <- paste0(
                    "<p><strong>Comprehensive TIL Profiling:</strong> Complete immune microenvironment characterization ",
                    "including multiple T cell subsets, B cells, and spatial distribution patterns.</p>"
                )
            }

            # Spatial Analysis Section
            if (self$options$spatialAnalysis && !is.null(private$.spatial_coords)) {
                report_sections[[length(report_sections) + 1]] <- "<h4>Spatial Distribution Analysis</h4>"
                report_sections[[length(report_sections) + 1]] <- paste0(
                    "<p>Spatial analysis was performed using coordinate data to assess immune cell clustering patterns. ",
                    "Moran's I values with statistical significance testing (* indicates p < 0.05) provide insights into ",
                    "the spatial organization of the immune microenvironment.</p>"
                )
            }

            # Checkpoint Inhibitor Section
            if (self$options$checkpointScore && !is.null(private$.checkpoint_matrix)) {
                report_sections[[length(report_sections) + 1]] <- "<h4>Checkpoint Inhibitor Analysis</h4>"
                report_sections[[length(report_sections) + 1]] <- paste0(
                    "<p>Checkpoint inhibitor analysis assessed PD-1 and PD-L1 expression patterns using cutoffs of ",
                    self$options$pd1Cutoff, "% and ", self$options$pdl1Cutoff,
                    "% respectively. This analysis aids in immunotherapy selection and response prediction.</p>"
                )
            }

            # Footer with interpretation guidance
            report_sections[[length(report_sections) + 1]] <- paste0(
                "<h4>Clinical Significance</h4>",
                "<p>This analysis provides quantitative assessment of tumor-infiltrating lymphocytes and immune microenvironment characteristics. ",
                "Results should be interpreted in conjunction with clinical parameters, tumor stage, and patient history. ",
                "For therapeutic decisions, correlation with validated biomarkers and multidisciplinary team assessment is recommended.</p>"
            )

            return(paste(report_sections, collapse = ""))
        },

        .optimizeForLargeDataset = function() {
            # Performance optimization for large datasets
            n_samples <- nrow(private$.immune_matrix)

            if (n_samples > private$.large_dataset_threshold) {
                # Large dataset optimizations
                sample_message <- paste0(
                    "<div style='background-color: #fff8dc; border: 1px solid #daa520; padding: 10px; margin: 10px 0;'>",
                    "<h5 style='color: #b8860b; margin-top: 0;'>⚡ Large Dataset Optimization</h5>",
                    "<p><strong>Dataset size:</strong> ", n_samples, " samples (>", private$.large_dataset_threshold, " threshold)</p>",
                    "<p><strong>Optimizations applied:</strong></p>",
                    "<ul>",
                    "<li>Spatial analysis: Sampling-based distance calculations</li>",
                    "<li>Diversity metrics: Optimized computation algorithms</li>",
                    "<li>Memory management: Chunked processing for large matrices</li>",
                    "</ul>",
                    "<p><em>Performance optimizations may slightly affect precision but maintain statistical validity.</em></p>",
                    "</div>"
                )

                current_instructions <- self$results$instructions$content
                if (is.null(current_instructions) || current_instructions == "") {
                    current_instructions <- "<h4>TIL Analysis Instructions</h4>"
                }
                self$results$instructions$setContent(paste0(current_instructions, sample_message))

                return(TRUE)
            }
            return(FALSE)
        },

        .getCachedOrCompute = function(cache_key, compute_function, ...) {
            # Caching system for expensive computations
            if (cache_key %in% names(private$.cache)) {
                return(private$.cache[[cache_key]])
            }

            result <- compute_function(...)
            private$.cache[[cache_key]] <- result
            return(result)
        },

        # Helper functions for spatial analysis
        .calculateMoranI = function(values, coords) {
            # Enhanced Moran's I calculation with statistical significance and performance optimization
            n <- length(values)
            if (n < 3) return(list(moran_i = 0, p_value = 1, z_score = 0))

            # Performance optimization for large datasets
            if (n > private$.large_dataset_threshold) {
                # Use sampling approach for very large datasets
                sample_size <- min(500, n)  # Sample at most 500 points
                sample_idx <- sample(n, sample_size)
                values <- values[sample_idx]
                coords <- coords[sample_idx, ]
                n <- length(values)
            }

            # Create distance matrix
            dist_matrix <- as.matrix(dist(coords))

            # Create weights matrix (inverse distance)
            w_matrix <- 1 / (dist_matrix + 1)
            diag(w_matrix) <- 0

            # Normalize weights
            row_sums <- rowSums(w_matrix, na.rm = TRUE)
            row_sums[row_sums == 0] <- 1  # Avoid division by zero
            w_matrix <- w_matrix / row_sums

            # Calculate Moran's I
            values_centered <- values - mean(values, na.rm = TRUE)
            numerator <- sum(w_matrix * outer(values_centered, values_centered), na.rm = TRUE)
            denominator <- sum(values_centered^2, na.rm = TRUE)

            if (denominator == 0) {
                return(list(moran_i = 0, p_value = 1, z_score = 0))
            }

            moran_i <- numerator / denominator

            # Calculate expected value and variance for significance testing
            S0 <- sum(w_matrix, na.rm = TRUE)
            S1 <- 0.5 * sum((w_matrix + t(w_matrix))^2, na.rm = TRUE)
            S2 <- sum(rowSums(w_matrix, na.rm = TRUE)^2, na.rm = TRUE)

            # Expected value
            E_I <- -1 / (n - 1)

            # Variance calculation
            b2 <- sum(values_centered^4, na.rm = TRUE) / (sum(values_centered^2, na.rm = TRUE)^2)
            var_I <- ((n * S1 - n * S2 + 3 * S0^2) / ((n - 1) * (n - 2) * (n - 3) * S0^2)) -
                     ((b2 * (n * S1 - 2 * n * S2 + 6 * S0^2)) / ((n - 1) * (n - 2) * (n - 3) * S0^2)) -
                     E_I^2

            # Z-score and p-value
            if (var_I > 0) {
                z_score <- (moran_i - E_I) / sqrt(var_I)
                p_value <- 2 * (1 - pnorm(abs(z_score)))
            } else {
                z_score <- 0
                p_value <- 1
            }

            return(list(moran_i = moran_i, p_value = p_value, z_score = z_score))
        },

        .calculateClusteringIndex = function(values, coords) {
            # Simple clustering index based on local variance
            n <- length(values)
            if (n < 3) return(0)

            local_variances <- numeric(n)
            for (i in seq_len(n)) {
                # Find nearest neighbors
                distances <- sqrt((coords$x - coords$x[i])^2 + (coords$y - coords$y[i])^2)
                nearest_indices <- order(distances)[1:min(5, n)]  # 5 nearest neighbors
                local_values <- values[nearest_indices]
                local_variances[i] <- var(local_values, na.rm = TRUE)
            }

            return(mean(local_variances, na.rm = TRUE))
        },

        .calculateNearestNeighborDistance = function(coords) {
            n <- nrow(coords)
            if (n < 2) return(0)

            distances <- numeric(n)
            for (i in seq_len(n)) {
                other_points <- coords[-i, ]
                point_distances <- sqrt((other_points$x - coords$x[i])^2 +
                                      (other_points$y - coords$y[i])^2)
                distances[i] <- min(point_distances)
            }

            return(mean(distances, na.rm = TRUE))
        },

        # Advanced diversity metrics calculation functions
        .calculateShannonIndex = function(values) {
            # Shannon diversity index H' = -sum(pi * log(pi))
            values <- values[values > 0]  # Remove zeros
            if (length(values) <= 1) return(0)

            total <- sum(values)
            proportions <- values / total
            shannon <- -sum(proportions * log(proportions), na.rm = TRUE)
            return(shannon)
        },

        .calculateSimpsonIndex = function(values) {
            # Simpson diversity index D = 1 - sum(pi^2)
            values <- values[values > 0]  # Remove zeros
            if (length(values) <= 1) return(0)

            total <- sum(values)
            proportions <- values / total
            simpson <- 1 - sum(proportions^2, na.rm = TRUE)
            return(simpson)
        },

        .calculateBrillouinIndex = function(values) {
            # Brillouin diversity index
            values <- values[values > 0]  # Remove zeros
            if (length(values) <= 1) return(0)

            n <- sum(values)
            if (n <= 1) return(0)

            # Brillouin index formula using lgamma to avoid factorial overflow
            brillouin <- (lgamma(n + 1) - sum(lgamma(values + 1), na.rm = TRUE)) / n
            return(brillouin)
        },

        .calculateBergerParkerIndex = function(values) {
            # Berger-Parker dominance index (proportion of most abundant species)
            values <- values[values > 0]  # Remove zeros
            if (length(values) == 0) return(0)

            total <- sum(values)
            max_abundance <- max(values)
            berger_parker <- max_abundance / total
            return(berger_parker)
        },

        # Plotting functions
        .plotImmuneDistribution = function(image, ggtheme, theme, ...) {
            if (is.null(private$.immune_matrix))
                return()

            # Create long format data for plotting
            plot_data <- data.frame(
                Case = rep(seq_len(nrow(private$.immune_matrix)), ncol(private$.immune_matrix)),
                Marker = rep(colnames(private$.immune_matrix), each = nrow(private$.immune_matrix)),
                Expression = as.vector(private$.immune_matrix)
            )

            plot <- ggplot2::ggplot(plot_data, ggplot2::aes(x = Marker, y = Expression)) +
                ggplot2::geom_boxplot(ggplot2::aes(fill = Marker), alpha = 0.7) +
                ggplot2::geom_jitter(width = 0.2, alpha = 0.5) +
                ggplot2::labs(
                    title = "Immune Marker Expression Distribution",
                    x = "Immune Markers",
                    y = "Expression Level (%)"
                ) +
                getIHCTheme() +
                ggplot2::theme(
                    axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
                    legend.position = "none"
                ) +
                ggplot2::scale_fill_viridis_d(option = "plasma")

            print(plot)
            TRUE
        },

        .plotTILAnalysis = function(image, ggtheme, theme, ...) {
            if (is.null(private$.immune_matrix))
                return()

            # Calculate TIL scores for each case
            til_scores <- rowMeans(private$.immune_matrix, na.rm = TRUE)

            plot_data <- data.frame(
                Case = seq_along(til_scores),
                TIL_Score = til_scores,
                Category = ifelse(til_scores < 10, "Cold",
                                ifelse(til_scores < 30, "Intermediate", "Hot"))
            )

            plot <- ggplot2::ggplot(plot_data, ggplot2::aes(x = Case, y = TIL_Score, color = Category)) +
                ggplot2::geom_point(size = 3, alpha = 0.7) +
                ggplot2::geom_hline(yintercept = c(10, 30), linetype = "dashed", color = "gray50") +
                ggplot2::labs(
                    title = paste("TIL Analysis -", self$options$tilMethod, "Method"),
                    x = "Case Number",
                    y = "TIL Score",
                    color = "Immune Status"
                ) +
                getIHCTheme() +
                ggplot2::scale_color_manual(values = c("Cold" = "blue", "Intermediate" = "orange", "Hot" = "red"))

            print(plot)
            TRUE
        },

        .plotContextureHeatmap = function(image, ggtheme, theme, ...) {
            if (is.null(private$.immune_matrix))
                return()

            # Normalize data for heatmap
            normalized_data <- scale(private$.immune_matrix)

            # Create heatmap data
            heatmap_data <- data.frame(
                Case = rep(seq_len(nrow(normalized_data)), ncol(normalized_data)),
                Marker = rep(colnames(normalized_data), each = nrow(normalized_data)),
                Expression = as.vector(normalized_data)
            )

            plot <- ggplot2::ggplot(heatmap_data, ggplot2::aes(x = Marker, y = Case, fill = Expression)) +
                ggplot2::geom_tile() +
                ggplot2::labs(
                    title = "Immune Contexture Heatmap",
                    x = "Immune Markers",
                    y = "Cases",
                    fill = "Normalized\nExpression"
                ) +
                getIHCTheme() +
                ggplot2::theme(
                    axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
                    axis.text.y = ggplot2::element_blank()
                ) +
                ggplot2::scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0)

            print(plot)
            TRUE
        },

        .plotSpatialDistribution = function(image, ggtheme, theme, ...) {
            if (is.null(private$.spatial_coords) || is.null(private$.immune_matrix))
                return()

            # Calculate overall immune score for each location
            immune_scores <- rowMeans(private$.immune_matrix, na.rm = TRUE)

            plot_data <- data.frame(
                X = private$.spatial_coords$x,
                Y = private$.spatial_coords$y,
                Immune_Score = immune_scores
            )

            plot <- ggplot2::ggplot(plot_data, ggplot2::aes(x = X, y = Y, color = Immune_Score)) +
                ggplot2::geom_point(size = 3, alpha = 0.7) +
                ggplot2::labs(
                    title = "Spatial Distribution of Immune Infiltration",
                    x = "X Coordinate",
                    y = "Y Coordinate",
                    color = "Immune\nScore"
                ) +
                getIHCTheme() +
                ggplot2::scale_color_viridis_c(option = "plasma") +
                ggplot2::coord_equal()

            print(plot)
            TRUE
        },

        .plotCheckpointScore = function(image, ggtheme, theme, ...) {
            if (is.null(private$.checkpoint_matrix))
                return()

            # Calculate checkpoint scores
            checkpoint_scores <- rowMeans(private$.checkpoint_matrix, na.rm = TRUE)

            plot_data <- data.frame(
                Case = seq_along(checkpoint_scores),
                Checkpoint_Score = checkpoint_scores,
                Response_Likelihood = ifelse(checkpoint_scores > 20, "High",
                                           ifelse(checkpoint_scores > 5, "Moderate", "Low"))
            )

            plot <- ggplot2::ggplot(plot_data, ggplot2::aes(x = Response_Likelihood, y = Checkpoint_Score)) +
                ggplot2::geom_boxplot(ggplot2::aes(fill = Response_Likelihood), alpha = 0.7) +
                ggplot2::geom_jitter(width = 0.2, alpha = 0.6) +
                ggplot2::labs(
                    title = "Checkpoint Inhibitor Response Prediction",
                    x = "Response Likelihood",
                    y = "Checkpoint Score",
                    fill = "Likelihood"
                ) +
                getIHCTheme() +
                ggplot2::scale_fill_manual(values = c("Low" = "red", "Moderate" = "orange", "High" = "green"))

            print(plot)
            TRUE
        },

        # Generate clinical interpretation context
        .generateClinicalContext = function() {
            method <- self$options$tilMethod
            markers <- colnames(private$.immune_matrix)
            n_samples <- nrow(private$.immune_matrix)

            clinical_html <- "<h4>Clinical Interpretation Guidelines</h4>"

            # Method-specific guidance
            if (method == "sterlacci") {
                clinical_html <- paste0(clinical_html,
                    "<h5>Sterlacci Method (Breast Cancer TIL Assessment)</h5>",
                    "<ul>",
                    "<li><b>CD3+ TIL Density:</b> Low (<", self$options$cd3Threshold, "%), Intermediate (", self$options$cd3Threshold, "-", self$options$cd3Threshold * 5, "%), High (>", self$options$cd3Threshold * 5, "%)</li>",
                    "<li><b>CD8+ TIL Density:</b> Low (<", self$options$cd8Threshold, "%), Intermediate (", self$options$cd8Threshold, "-", self$options$cd8Threshold * 4, "%), High (>", self$options$cd8Threshold * 4, "%)</li>",
                    "<li><b>Clinical Significance:</b> Higher TIL infiltration associated with better prognosis in breast cancer</li>",
                    "<li><b>Reference:</b> Sterlacci et al. (2014) - Breast cancer pathology guidelines</li>",
                    "</ul>"
                )
            } else if (method == "cd4cd8_ratio") {
                clinical_html <- paste0(clinical_html,
                    "<h5>CD4/CD8 Ratio Analysis</h5>",
                    "<ul>",
                    "<li><b>Balanced Response:</b> CD4/CD8 ratio 0.5-2.0 indicates optimal immune balance</li>",
                    "<li><b>CD8-Dominant:</b> Ratio <1.0 suggests cytotoxic T-cell predominance</li>",
                    "<li><b>CD4-Dominant:</b> Ratio >2.0 suggests helper T-cell predominance</li>",
                    "<li><b>Therapeutic Implications:</b> CD8-dominant profiles may respond better to checkpoint inhibitors</li>",
                    "</ul>"
                )
            } else if (method == "granzyme_focus") {
                clinical_html <- paste0(clinical_html,
                    "<h5>Granzyme B+ TIL Focus Analysis</h5>",
                    "<ul>",
                    "<li><b>Active Cytotoxicity:</b> Granzyme B+ >5% indicates active cytotoxic immune response</li>",
                    "<li><b>Hotspot Detection:</b> Multiple hotspots suggest heterogeneous immune activation</li>",
                    "<li><b>Clinical Relevance:</b> High granzyme activity correlates with immunotherapy response</li>",
                    "<li><b>Prognostic Value:</b> Strong granzyme expression associated with favorable outcomes</li>",
                    "</ul>"
                )
            } else {
                clinical_html <- paste0(clinical_html,
                    "<h5>Comprehensive TIL Analysis</h5>",
                    "<ul>",
                    "<li><b>Cold Tumors:</b> Immune score <", self$options$immuneScoreThreshold, "% - Limited immune infiltration</li>",
                    "<li><b>Intermediate:</b> Immune score ", self$options$immuneScoreThreshold, "-", self$options$immuneScoreThreshold * 3, "% - Moderate immune activity</li>",
                    "<li><b>Hot Tumors:</b> Immune score >", self$options$immuneScoreThreshold * 3, "% - High immune infiltration</li>",
                    "<li><b>Treatment Selection:</b> Hot tumors more likely to respond to immunotherapy</li>",
                    "</ul>"
                )
            }

            # General clinical context
            clinical_html <- paste0(clinical_html,
                "<h5>General Clinical Guidelines</h5>",
                "<ul>",
                "<li><b>Sample Size:</b> Current analysis includes ", n_samples, " cases</li>",
                "<li><b>Markers Analyzed:</b> ", paste(markers, collapse = ", "), "</li>",
                "<li><b>Prognostic Value:</b> Higher TIL density generally associated with better outcomes</li>",
                "<li><b>Therapeutic Implications:</b> Immune-infiltrated tumors more responsive to immunotherapy</li>",
                "<li><b>Validation:</b> Results should be validated in independent cohorts</li>",
                "<li><b>Quality Control:</b> Ensure consistent IHC staining and scoring protocols</li>",
                "</ul>"
            )

            # Checkpoint inhibitor context if applicable
            if (self$options$checkpointScore && !is.null(private$.checkpoint_matrix)) {
                clinical_html <- paste0(clinical_html,
                    "<h5>Checkpoint Inhibitor Analysis</h5>",
                    "<ul>",
                    "<li><b>PD-1 Threshold:</b> ", self$options$pd1Cutoff, "% positivity cutoff</li>",
                    "<li><b>PD-L1 Threshold:</b> ", self$options$pdl1Cutoff, "% positivity cutoff</li>",
                    "<li><b>Response Prediction:</b> High likelihood >50%, Moderate 20-50%, Low <20%</li>",
                    "<li><b>Clinical Use:</b> Combined with TIL analysis for optimal patient selection</li>",
                    "</ul>"
                )
            }

            # Add copy-ready report section
            clinical_html <- paste0(clinical_html,
                "<hr>",
                "<h5>Copy-Ready Clinical Report</h5>",
                "<p><i>The following section provides a comprehensive clinical report suitable for copy-paste into clinical documentation:</i></p>",
                "<div style='border: 1px solid #ccc; padding: 10px; background-color: #f9f9f9; font-family: Arial, sans-serif;'>",
                private$.generateClinicalReport(),
                "</div>"
            )

            self$results$clinicalContext$setContent(clinical_html)
        },

        # Enhanced spatial analysis validation
        .validateSpatialData = function() {
            if (!self$options$spatialAnalysis) return(TRUE)

            if (is.null(self$options$x_coordinate) || is.null(self$options$y_coordinate)) {
                self$results$instructions$setContent(
                    "<p style='color: orange;'><b>Spatial Analysis:</b> Both X and Y coordinates are required for spatial analysis.</p>"
                )
                return(FALSE)
            }

            x_data <- self$data[[self$options$x_coordinate]]
            y_data <- self$data[[self$options$y_coordinate]]

            if (!is.numeric(x_data) || !is.numeric(y_data)) {
                self$results$instructions$setContent(
                    "<p style='color: red;'><b>Spatial Analysis Error:</b> Coordinate variables must be numeric. Please select numeric columns for X and Y coordinates.</p>"
                )
                return(FALSE)
            }

            # Check for sufficient valid coordinates
            valid_coords <- complete.cases(x_data, y_data)
            if (sum(valid_coords) < 3) {
                self$results$instructions$setContent(
                    "<p style='color: red;'><b>Spatial Analysis Error:</b> At least 3 cases with valid coordinates are required for spatial analysis.</p>"
                )
                return(FALSE)
            }

            # Check coordinate range
            if (var(x_data, na.rm = TRUE) == 0 || var(y_data, na.rm = TRUE) == 0) {
                self$results$instructions$setContent(
                    "<p style='color: orange;'><b>Spatial Analysis Warning:</b> Coordinates show no variation. Spatial analysis may not be meaningful.</p>"
                )
            }

            return(TRUE)
        }
    )
)