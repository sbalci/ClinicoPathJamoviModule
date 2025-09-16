ihcimmuneClass <- R6::R6Class(
    "ihcimmuneClass",
    inherit = ihcimmuneBase,
    private = list(
        .immune_matrix = NULL,
        .checkpoint_matrix = NULL,
        .spatial_coords = NULL,
        .case_ids = NULL,

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
                x_coord <- convertIHCMarkerToNumeric(self$data[[self$options$x_coordinate]])
                y_coord <- convertIHCMarkerToNumeric(self$data[[self$options$y_coordinate]])

                coords <- data.frame(x = x_coord, y = y_coord)
                complete_cases <- complete.cases(coords)
                private$.spatial_coords <- coords[complete_cases, ]
            }

            # Prepare case IDs
            if (!is.null(self$options$id)) {
                private$.case_ids <- self$data[[self$options$id]]
            } else {
                private$.case_ids <- seq_len(nrow(self$data))
            }
        },

        .run = function() {
            if (is.null(private$.immune_matrix))
                return()

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
                cd3_category <- ifelse(cd3_mean < 10, "Low", ifelse(cd3_mean < 50, "Intermediate", "High"))

                results[[length(results) + 1]] <- list(
                    metric = "CD3+ TIL Density",
                    value = cd3_mean,
                    category = cd3_category,
                    interpretation = paste("Sterlacci method:", cd3_category, "TIL infiltration")
                )
            }

            if (!is.na(cd8_col)) {
                cd8_mean <- mean(private$.immune_matrix[, cd8_col], na.rm = TRUE)
                cd8_category <- ifelse(cd8_mean < 5, "Low", ifelse(cd8_mean < 20, "Intermediate", "High"))

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

            immune_category <- ifelse(mean_immune_score < 10, "Cold",
                                    ifelse(mean_immune_score < 30, "Intermediate", "Hot"))

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

                # Calculate Moran's I for spatial autocorrelation
                moran_i <- private$.calculateMoranI(marker_values, private$.spatial_coords)

                # Calculate clustering index
                clustering_index <- private$.calculateClusteringIndex(marker_values, private$.spatial_coords)

                # Calculate mean nearest neighbor distance
                nn_distance <- private$.calculateNearestNeighborDistance(private$.spatial_coords)

                # Determine spatial pattern
                spatial_pattern <- ifelse(moran_i > 0.3, "Clustered",
                                        ifelse(moran_i < -0.3, "Dispersed", "Random"))

                table$addRow(rowKey = i, values = list(
                    marker = marker_name,
                    moran_i = moran_i,
                    clustering_index = clustering_index,
                    nearest_neighbor_distance = nn_distance,
                    spatial_pattern = spatial_pattern
                ))
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
                    diversity_category = diversity_category
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

        # Helper functions for spatial analysis
        .calculateMoranI = function(values, coords) {
            # Simplified Moran's I calculation
            n <- length(values)
            if (n < 3) return(0)

            # Create distance matrix
            dist_matrix <- as.matrix(dist(coords))

            # Create weights matrix (inverse distance)
            w_matrix <- 1 / (dist_matrix + 1)
            diag(w_matrix) <- 0

            # Normalize weights
            w_matrix <- w_matrix / rowSums(w_matrix, na.rm = TRUE)

            # Calculate Moran's I
            values_centered <- values - mean(values, na.rm = TRUE)
            numerator <- sum(w_matrix * outer(values_centered, values_centered), na.rm = TRUE)
            denominator <- sum(values_centered^2, na.rm = TRUE)

            moran_i <- numerator / denominator
            return(moran_i)
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

        .calculateShannonIndex = function(values) {
            # Remove zeros and normalize
            values <- values[values > 0]
            if (length(values) == 0) return(0)

            proportions <- values / sum(values)
            shannon <- -sum(proportions * log(proportions))
            return(shannon)
        },

        .calculateSimpsonIndex = function(values) {
            # Remove zeros and normalize
            values <- values[values > 0]
            if (length(values) == 0) return(0)

            proportions <- values / sum(values)
            simpson <- 1 - sum(proportions^2)
            return(simpson)
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
        }
    )
)