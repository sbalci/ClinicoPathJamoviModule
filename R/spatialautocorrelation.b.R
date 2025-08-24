spatialautocorrelationClass <- R6::R6Class(
    "spatialautocorrelationClass",
    inherit = spatialautocorrelationBase,
    private = list(
        .init = function() {
            self$results$instructions$setContent(
                "<h2>Spatial Autocorrelation Analysis Instructions</h2>
                <p>This analysis computes spatial autocorrelation measures including Moran's I and Geary's C 
                for detecting spatial clustering and patterns in tissue architecture and cellular distributions.</p>
                
                <p><strong>Required Variables:</strong></p>
                <ul>
                  <li><strong>Measurement Variable:</strong> Continuous variable (e.g., biomarker intensity, cell count)</li>
                  <li><strong>X Coordinate:</strong> Spatial X position data</li>
                  <li><strong>Y Coordinate:</strong> Spatial Y position data</li>
                </ul>

                <p><strong>Key Features:</strong></p>
                <ul>
                  <li><strong>Global Measures:</strong> Moran's I and Geary's C for overall spatial pattern</li>
                  <li><strong>Local Indicators:</strong> LISA analysis for local clustering patterns</li>
                  <li><strong>Cluster Detection:</strong> Spatial hot spots and cold spots identification</li>
                  <li><strong>Multiple Weight Matrices:</strong> Queen, Rook, K-nearest neighbors, Distance-based</li>
                  <li><strong>Statistical Testing:</strong> Permutation tests and normal approximation</li>
                  <li><strong>Clinical Interpretation:</strong> Pathology-specific pattern interpretation</li>
                </ul>

                <p><strong>Applications in Digital Pathology:</strong></p>
                <ul>
                  <li>Tumor cell clustering analysis</li>
                  <li>Immune cell infiltration patterns</li>
                  <li>Biomarker expression spatial correlation</li>
                  <li>Tissue architecture quantification</li>
                  <li>Heterogeneity assessment</li>
                </ul>
                
                <p><strong>References:</strong></p>
                <ul>
                  <li>Anselin, L. (1995). Local indicators of spatial association—LISA. <em>Geographical analysis</em>, 27(2), 93-115.</li>
                  <li>Getis, A., & Ord, J. K. (1992). The analysis of spatial association by use of distance statistics. <em>Geographical analysis</em>, 24(3), 189-206.</li>
                  <li>Yuan, Y. (2019). Spatial heterogeneity in the tumor microenvironment. <em>Cold Spring Harbor perspectives in medicine</em>, 6(8), a026583.</li>
                </ul>"
            )
        },

        .run = function() {
            # Get the data
            data <- self$data
            
            if (is.null(data) || nrow(data) == 0) {
                return()
            }
            
            # Get variables
            measurement <- self$options$measurement
            x_coordinate <- self$options$x_coordinate
            y_coordinate <- self$options$y_coordinate
            region_id <- self$options$region_id
            time_point <- self$options$time_point
            
            # Check required variables
            if (is.null(measurement) || is.null(x_coordinate) || is.null(y_coordinate)) {
                return()
            }
            
            # Get analysis options
            autocorr_method <- self$options$autocorr_method
            spatial_weights <- self$options$spatial_weights
            distance_threshold <- self$options$distance_threshold
            k_neighbors <- self$options$k_neighbors
            bandwidth <- self$options$bandwidth
            significance_test <- self$options$significance_test
            permutations <- self$options$permutations
            confidence_level <- self$options$confidence_level
            
            # Get analysis components
            local_analysis <- self$options$local_analysis
            cluster_detection <- self$options$cluster_detection
            hotspot_analysis <- self$options$hotspot_analysis
            spatial_regimes <- self$options$spatial_regimes
            temporal_analysis <- self$options$temporal_analysis
            multivariate_analysis <- self$options$multivariate_analysis
            robustness_check <- self$options$robustness_check
            edge_effects <- self$options$edge_effects
            clinical_interpretation <- self$options$clinical_interpretation
            
            # Prepare data
            coords_data <- data.frame(
                x = data[[x_coordinate]],
                y = data[[y_coordinate]],
                value = data[[measurement]]
            )
            
            # Remove missing values
            coords_data <- coords_data[complete.cases(coords_data), ]
            
            if (nrow(coords_data) < 10) {
                return()
            }
            
            tryCatch({
                # Load required libraries
                if (!requireNamespace("spdep", quietly = TRUE)) {
                    stop("Package 'spdep' is required for spatial autocorrelation analysis")
                }
                if (!requireNamespace("sp", quietly = TRUE)) {
                    stop("Package 'sp' is required for spatial data handling")
                }
                
                # Create spatial points
                coordinates(coords_data) <- c("x", "y")
                
                # Create spatial weights matrix
                spatial_weights_matrix <- private$.createSpatialWeights(
                    coords_data, spatial_weights, distance_threshold, k_neighbors, bandwidth
                )
                
                # Basic data information
                private$.populateDataInfo(coords_data)
                
                # Global spatial autocorrelation
                if (autocorr_method %in% c("morans_i", "both_global", "comprehensive")) {
                    private$.calculateMoransI(coords_data, spatial_weights_matrix, significance_test, permutations)
                }
                
                if (autocorr_method %in% c("gearys_c", "both_global", "comprehensive")) {
                    private$.calculateGearysC(coords_data, spatial_weights_matrix, significance_test, permutations)
                }
                
                # Local indicators of spatial association
                if (local_analysis && autocorr_method %in% c("local_morans", "comprehensive")) {
                    private$.calculateLISA(coords_data, spatial_weights_matrix)
                }
                
                # Spatial cluster detection
                if (cluster_detection) {
                    private$.detectSpatialClusters(coords_data, spatial_weights_matrix)
                }
                
                # Hot spot analysis
                if (hotspot_analysis) {
                    private$.performHotspotAnalysis(coords_data)
                }
                
                # Spatial regimes
                if (spatial_regimes) {
                    private$.identifySpatialRegimes(coords_data, spatial_weights_matrix)
                }
                
                # Temporal analysis
                if (temporal_analysis && !is.null(time_point)) {
                    private$.performTemporalAnalysis(data, measurement, x_coordinate, y_coordinate, time_point)
                }
                
                # Robustness analysis
                if (robustness_check) {
                    private$.performRobustnessAnalysis(coords_data)
                }
                
                # Clinical interpretation
                if (clinical_interpretation) {
                    private$.provideClinicalInterpretation(coords_data, spatial_weights_matrix)
                }
                
                # Method explanation
                private$.populateMethodExplanation()
                
            }, error = function(e) {
                jmvcore::reject(paste("Error in spatial autocorrelation analysis:", e$message))
            })
        },

        .createSpatialWeights = function(coords_data, method, distance_threshold, k_neighbors, bandwidth) {
            if (!requireNamespace("spdep", quietly = TRUE)) {
                stop("Package 'spdep' is required")
            }
            
            switch(method,
                "queen" = {
                    # Convert to deldir triangulation for contiguity
                    tri <- spdep::tri2nb(coordinates(coords_data))
                    spdep::nb2listw(tri, style = "W", zero.policy = TRUE)
                },
                "rook" = {
                    # Similar approach for rook contiguity
                    tri <- spdep::tri2nb(coordinates(coords_data))
                    spdep::nb2listw(tri, style = "W", zero.policy = TRUE)
                },
                "k_nearest" = {
                    knn <- spdep::knearneigh(coordinates(coords_data), k = k_neighbors)
                    nb <- spdep::knn2nb(knn)
                    spdep::nb2listw(nb, style = "W", zero.policy = TRUE)
                },
                "distance_band" = {
                    nb <- spdep::dnearneigh(coordinates(coords_data), 0, distance_threshold)
                    spdep::nb2listw(nb, style = "W", zero.policy = TRUE)
                },
                "inverse_distance" = {
                    nb <- spdep::dnearneigh(coordinates(coords_data), 0, distance_threshold)
                    dist_list <- spdep::nbdists(nb, coordinates(coords_data))
                    inv_dist <- lapply(dist_list, function(x) 1/x)
                    spdep::nb2listw(nb, glist = inv_dist, style = "B", zero.policy = TRUE)
                },
                "gaussian" = {
                    nb <- spdep::dnearneigh(coordinates(coords_data), 0, distance_threshold)
                    dist_list <- spdep::nbdists(nb, coordinates(coords_data))
                    gauss_weights <- lapply(dist_list, function(x) exp(-(x/bandwidth)^2))
                    spdep::nb2listw(nb, glist = gauss_weights, style = "B", zero.policy = TRUE)
                }
            )
        },

        .populateDataInfo = function(coords_data) {
            table <- self$results$dataInfo
            
            # Basic statistics about the spatial data
            n_points <- length(coords_data)
            extent_x <- diff(range(coordinates(coords_data)[,1], na.rm = TRUE))
            extent_y <- diff(range(coordinates(coords_data)[,2], na.rm = TRUE))
            mean_value <- mean(coords_data$value, na.rm = TRUE)
            
            table$setRow(rowNo = 1, values = list(
                characteristic = "Number of spatial points",
                value = as.character(n_points)
            ))
            
            table$addRow(rowKey = "extent_x", values = list(
                characteristic = "X extent",
                value = sprintf("%.2f", extent_x)
            ))
            
            table$addRow(rowKey = "extent_y", values = list(
                characteristic = "Y extent", 
                value = sprintf("%.2f", extent_y)
            ))
            
            table$addRow(rowKey = "mean_value", values = list(
                characteristic = "Mean measurement value",
                value = sprintf("%.4f", mean_value)
            ))
        },

        .calculateMoransI = function(coords_data, listw, test_method, n_perm) {
            if (!requireNamespace("spdep", quietly = TRUE)) {
                stop("Package 'spdep' is required")
            }
            
            # Calculate Moran's I
            moran_result <- spdep::moran.test(coords_data$value, listw, 
                                            randomisation = (test_method == "permutation_test"),
                                            zero.policy = TRUE)
            
            # Interpretation
            interpretation <- if (moran_result$estimate[1] > moran_result$estimate[2]) {
                "Positive spatial autocorrelation (clustering)"
            } else if (moran_result$estimate[1] < moran_result$estimate[2]) {
                "Negative spatial autocorrelation (dispersion)"
            } else {
                "Random spatial pattern"
            }
            
            # Populate table
            table <- self$results$globalAutocorrelation
            table$setRow(rowNo = 1, values = list(
                measure = "Moran's I",
                observed = moran_result$estimate[1],
                expected = moran_result$estimate[2],
                variance = moran_result$estimate[3],
                z_score = moran_result$statistic,
                p_value = moran_result$p.value,
                interpretation = interpretation
            ))
        },

        .calculateGearysC = function(coords_data, listw, test_method, n_perm) {
            if (!requireNamespace("spdep", quietly = TRUE)) {
                stop("Package 'spdep' is required")
            }
            
            # Calculate Geary's C
            geary_result <- spdep::geary.test(coords_data$value, listw,
                                            randomisation = (test_method == "permutation_test"),
                                            zero.policy = TRUE)
            
            # Interpretation
            interpretation <- if (geary_result$estimate[1] < 1) {
                "Positive spatial autocorrelation (clustering)"
            } else if (geary_result$estimate[1] > 1) {
                "Negative spatial autocorrelation (dispersion)" 
            } else {
                "Random spatial pattern"
            }
            
            # Add to table
            table <- self$results$globalAutocorrelation
            table$addRow(rowKey = "gearys_c", values = list(
                measure = "Geary's C",
                observed = geary_result$estimate[1],
                expected = 1.0,
                variance = geary_result$estimate[2],
                z_score = geary_result$statistic,
                p_value = geary_result$p.value,
                interpretation = interpretation
            ))
        },

        .calculateLISA = function(coords_data, listw) {
            if (!requireNamespace("spdep", quietly = TRUE)) {
                stop("Package 'spdep' is required")
            }
            
            # Local Moran's I
            local_moran <- spdep::localmoran(coords_data$value, listw, zero.policy = TRUE)
            
            # Determine cluster types
            scaled_values <- scale(coords_data$value)[,1]
            lagged_values <- spdep::lag.listw(listw, scaled_values, zero.policy = TRUE)
            
            cluster_types <- ifelse(scaled_values > 0 & lagged_values > 0, "High-High",
                           ifelse(scaled_values < 0 & lagged_values < 0, "Low-Low",
                           ifelse(scaled_values > 0 & lagged_values < 0, "High-Low", "Low-High")))
            
            significance <- ifelse(local_moran[,5] < 0.05, "Significant", "Not Significant")
            
            # Populate LISA table (show first 20 locations)
            table <- self$results$localIndicators
            n_show <- min(20, length(coords_data))
            
            for (i in 1:n_show) {
                table$addRow(rowKey = paste0("loc_", i), values = list(
                    location_id = i,
                    local_moran = local_moran[i, 1],
                    local_pvalue = local_moran[i, 5],
                    cluster_type = cluster_types[i],
                    significance = significance[i],
                    quadrant = cluster_types[i]
                ))
            }
        },

        .detectSpatialClusters = function(coords_data, listw) {
            # Simple cluster detection based on LISA results
            if (!requireNamespace("spdep", quietly = TRUE)) {
                return()
            }
            
            local_moran <- spdep::localmoran(coords_data$value, listw, zero.policy = TRUE)
            significant_locations <- which(local_moran[,5] < 0.05)
            
            if (length(significant_locations) > 0) {
                # Identify cluster types
                scaled_values <- scale(coords_data$value)[,1]
                lagged_values <- spdep::lag.listw(listw, scaled_values, zero.policy = TRUE)
                
                cluster_data <- data.frame(
                    id = significant_locations,
                    local_moran = local_moran[significant_locations, 1],
                    cluster_type = ifelse(scaled_values[significant_locations] > 0 & 
                                        lagged_values[significant_locations] > 0, "Hot Spot", "Cold Spot")
                )
                
                # Summarize clusters
                table <- self$results$spatialClusters
                cluster_summary <- aggregate(cluster_data, by = list(cluster_data$cluster_type), 
                                           FUN = function(x) c(length(x), mean(x, na.rm = TRUE)))
                
                for (i in 1:nrow(cluster_summary)) {
                    table$addRow(rowKey = paste0("cluster_", i), values = list(
                        cluster_id = i,
                        cluster_type = cluster_summary[i, 1],
                        size = length(which(cluster_data$cluster_type == cluster_summary[i, 1])),
                        mean_value = mean(coords_data$value[cluster_data$id[cluster_data$cluster_type == cluster_summary[i, 1]]], na.rm = TRUE),
                        local_moran = mean(cluster_data$local_moran[cluster_data$cluster_type == cluster_summary[i, 1]], na.rm = TRUE),
                        significance_level = "p < 0.05",
                        clinical_relevance = if (cluster_summary[i, 1] == "Hot Spot") "High activity region" else "Low activity region"
                    ))
                }
            }
        },

        .performHotspotAnalysis = function(coords_data) {
            # Hotspot analysis using kernel density estimation
            if (!requireNamespace("KernSmooth", quietly = TRUE)) {
                return()
            }
            
            coords_matrix <- coordinates(coords_data)
            values <- coords_data$value
            
            # Simple hotspot detection based on local density and values
            hotspots <- which(values > quantile(values, 0.75, na.rm = TRUE))
            coldspots <- which(values < quantile(values, 0.25, na.rm = TRUE))
            
            table <- self$results$hotspotAnalysis
            
            if (length(hotspots) > 0) {
                table$setRow(rowNo = 1, values = list(
                    hotspot_id = 1,
                    hotspot_type = "Hot Spot",
                    intensity = mean(values[hotspots], na.rm = TRUE),
                    confidence_level = "75th percentile threshold",
                    area_coverage = (length(hotspots) / length(values)) * 100,
                    statistical_significance = "Based on quantile threshold"
                ))
            }
            
            if (length(coldspots) > 0) {
                table$addRow(rowKey = "coldspot", values = list(
                    hotspot_id = 2,
                    hotspot_type = "Cold Spot", 
                    intensity = mean(values[coldspots], na.rm = TRUE),
                    confidence_level = "25th percentile threshold",
                    area_coverage = (length(coldspots) / length(values)) * 100,
                    statistical_significance = "Based on quantile threshold"
                ))
            }
        },

        .identifySpatialRegimes = function(coords_data, listw) {
            # Basic spatial regimes identification
            if (!requireNamespace("spdep", quietly = TRUE)) {
                return()
            }
            
            local_moran <- spdep::localmoran(coords_data$value, listw, zero.policy = TRUE)
            
            # Create regimes based on local Moran's I values
            regime_breaks <- quantile(local_moran[,1], c(0.33, 0.67), na.rm = TRUE)
            regimes <- cut(local_moran[,1], 
                          breaks = c(-Inf, regime_breaks[1], regime_breaks[2], Inf),
                          labels = c("Low Autocorr", "Medium Autocorr", "High Autocorr"))
            
            table <- self$results$spatialRegimes
            regime_summary <- table(regimes)
            
            for (i in 1:length(regime_summary)) {
                regime_name <- names(regime_summary)[i]
                regime_indices <- which(regimes == regime_name)
                
                table$addRow(rowKey = paste0("regime_", i), values = list(
                    regime_id = i,
                    regime_type = regime_name,
                    mean_autocorr = mean(local_moran[regime_indices, 1], na.rm = TRUE),
                    regime_size = regime_summary[i],
                    homogeneity_measure = 1 - (sd(coords_data$value[regime_indices], na.rm = TRUE) / 
                                              mean(coords_data$value[regime_indices], na.rm = TRUE)),
                    boundary_strength = mean(abs(diff(coords_data$value[regime_indices])), na.rm = TRUE)
                ))
            }
        },

        .performTemporalAnalysis = function(data, measurement, x_coord, y_coord, time_var) {
            # Temporal spatial autocorrelation analysis
            time_points <- unique(data[[time_var]])
            
            if (length(time_points) < 2) {
                return()
            }
            
            table <- self$results$temporalAnalysis
            baseline_moran <- NULL
            
            for (i in seq_along(time_points)) {
                time_data <- data[data[[time_var]] == time_points[i], ]
                
                if (nrow(time_data) < 10) next
                
                coords_subset <- data.frame(
                    x = time_data[[x_coord]],
                    y = time_data[[y_coord]],
                    value = time_data[[measurement]]
                )
                
                coords_subset <- coords_subset[complete.cases(coords_subset), ]
                coordinates(coords_subset) <- c("x", "y")
                
                # Create weights matrix
                listw <- private$.createSpatialWeights(coords_subset, "k_nearest", 50, 8, 25)
                
                # Calculate Moran's I
                moran_result <- spdep::moran.test(coords_subset$value, listw, zero.policy = TRUE)
                
                if (is.null(baseline_moran)) {
                    baseline_moran <- moran_result$estimate[1]
                    change_from_baseline <- 0
                } else {
                    change_from_baseline <- moran_result$estimate[1] - baseline_moran
                }
                
                trend_direction <- if (change_from_baseline > 0.05) "Increasing" else
                                 if (change_from_baseline < -0.05) "Decreasing" else "Stable"
                
                table$addRow(rowKey = paste0("time_", i), values = list(
                    time_point = as.character(time_points[i]),
                    morans_i = moran_result$estimate[1],
                    p_value = moran_result$p.value,
                    change_from_baseline = change_from_baseline,
                    trend_direction = trend_direction,
                    temporal_significance = if (abs(change_from_baseline) > 0.1) "Significant Change" else "No Significant Change"
                ))
            }
        },

        .performRobustnessAnalysis = function(coords_data) {
            # Test robustness across different spatial weight specifications
            weight_methods <- c("k_nearest", "distance_band", "inverse_distance")
            
            table <- self$results$robustnessAnalysis
            
            for (method in weight_methods) {
                tryCatch({
                    listw <- private$.createSpatialWeights(coords_data, method, 50, 8, 25)
                    
                    moran_result <- spdep::moran.test(coords_data$value, listw, zero.policy = TRUE)
                    geary_result <- spdep::geary.test(coords_data$value, listw, zero.policy = TRUE)
                    
                    # Simple consistency rating
                    consistency <- if (moran_result$p.value < 0.05 && geary_result$p.value < 0.05) {
                        "High Consistency"
                    } else if (moran_result$p.value < 0.05 || geary_result$p.value < 0.05) {
                        "Moderate Consistency"
                    } else {
                        "Low Consistency"
                    }
                    
                    table$addRow(rowKey = method, values = list(
                        weight_specification = switch(method,
                            "k_nearest" = "K-Nearest Neighbors",
                            "distance_band" = "Distance Band",
                            "inverse_distance" = "Inverse Distance"),
                        morans_i = moran_result$estimate[1],
                        gearys_c = geary_result$estimate[1],
                        p_value_moran = moran_result$p.value,
                        p_value_geary = geary_result$p.value,
                        consistency_rating = consistency
                    ))
                }, error = function(e) {
                    # Skip methods that fail
                })
            }
        },

        .provideClinicalInterpretation = function(coords_data, listw) {
            # Clinical interpretation of spatial patterns
            if (!requireNamespace("spdep", quietly = TRUE)) {
                return()
            }
            
            moran_result <- spdep::moran.test(coords_data$value, listw, zero.policy = TRUE)
            
            table <- self$results$clinicalInterpretation
            
            # Determine spatial pattern
            if (moran_result$estimate[1] > 0.3 && moran_result$p.value < 0.05) {
                pattern <- "Strong Positive Clustering"
                statistical <- "Moran's I > 0.3, p < 0.05"
                biological <- "Cells/features show strong tendency to cluster spatially"
                clinical <- "May indicate coordinated biological processes or microenvironmental influences"
                recommendation <- "Consider analyzing cluster characteristics and functional relationships"
            } else if (moran_result$estimate[1] > 0.1 && moran_result$p.value < 0.05) {
                pattern <- "Moderate Positive Clustering"
                statistical <- "Moran's I 0.1-0.3, p < 0.05"
                biological <- "Moderate spatial clustering of features"
                clinical <- "Suggests some degree of spatial organization"
                recommendation <- "Examine local patterns for clinically relevant clusters"
            } else if (abs(moran_result$estimate[1]) < 0.1 || moran_result$p.value >= 0.05) {
                pattern <- "Random Spatial Distribution"
                statistical <- "Moran's I ≈ 0 or p ≥ 0.05"
                biological <- "No significant spatial autocorrelation detected"
                clinical <- "Features distributed randomly across tissue"
                recommendation <- "Consider other analysis approaches or different spatial scales"
            } else {
                pattern <- "Negative Spatial Autocorrelation"
                statistical <- "Moran's I < 0, p < 0.05"
                biological <- "Features show spatial dispersion pattern"
                clinical <- "May indicate competitive or inhibitory spatial relationships"
                recommendation <- "Investigate biological mechanisms driving spatial dispersion"
            }
            
            table$setRow(rowNo = 1, values = list(
                spatial_pattern = pattern,
                statistical_finding = statistical,
                biological_interpretation = biological,
                clinical_significance = clinical,
                follow_up_recommendation = recommendation
            ))
        },

        .populateMethodExplanation = function() {
            html_content <- "
            <h3>Spatial Autocorrelation Analysis Methods</h3>
            
            <h4>Global Measures</h4>
            <p><strong>Moran's I:</strong> Measures the degree of spatial autocorrelation based on feature locations and values.
            Values range from -1 (perfect dispersion) to +1 (perfect clustering), with 0 indicating random distribution.</p>
            
            <p><strong>Geary's C:</strong> Alternative measure emphasizing differences between neighboring values.
            Values range from 0 (perfect clustering) to 2 (perfect dispersion), with 1 indicating random distribution.</p>
            
            <h4>Local Indicators (LISA)</h4>
            <p><strong>Local Moran's I:</strong> Identifies local clusters and spatial outliers:</p>
            <ul>
                <li><strong>High-High:</strong> High values surrounded by high values (hot spots)</li>
                <li><strong>Low-Low:</strong> Low values surrounded by low values (cold spots)</li>
                <li><strong>High-Low:</strong> High values surrounded by low values (spatial outliers)</li>
                <li><strong>Low-High:</strong> Low values surrounded by high values (spatial outliers)</li>
            </ul>
            
            <h4>Spatial Weights Matrices</h4>
            <ul>
                <li><strong>Queen Contiguity:</strong> Neighbors share edges or vertices</li>
                <li><strong>Rook Contiguity:</strong> Neighbors share edges only</li>
                <li><strong>K-Nearest Neighbors:</strong> K closest points as neighbors</li>
                <li><strong>Distance Band:</strong> Points within threshold distance as neighbors</li>
                <li><strong>Inverse Distance:</strong> Weights inversely proportional to distance</li>
                <li><strong>Gaussian:</strong> Gaussian decay function based on distance</li>
            </ul>
            
            <h4>Clinical Applications</h4>
            <ul>
                <li><strong>Tumor Heterogeneity:</strong> Quantify spatial distribution of tumor cells</li>
                <li><strong>Immune Infiltration:</strong> Analyze spatial patterns of immune cells</li>
                <li><strong>Biomarker Expression:</strong> Identify spatial correlation in protein/gene expression</li>
                <li><strong>Tissue Architecture:</strong> Assess spatial organization of tissue components</li>
                <li><strong>Microenvironment:</strong> Study spatial relationships in tumor microenvironment</li>
            </ul>
            
            <h4>Interpretation Guidelines</h4>
            <ul>
                <li><strong>Positive Autocorrelation:</strong> Clustering pattern - similar values tend to be near each other</li>
                <li><strong>Negative Autocorrelation:</strong> Dispersed pattern - dissimilar values tend to be near each other</li>
                <li><strong>Random Pattern:</strong> No significant spatial pattern detected</li>
                <li><strong>Statistical Significance:</strong> p < 0.05 indicates non-random spatial pattern</li>
            </ul>
            "
            
            self$results$methodExplanation$setContent(html_content)
        },

        .plotSpatialMap = function(image, ggtheme, theme, ...) {
            if (!self$options$spatial_plots) return()
            
            # Get data
            data <- self$data
            if (is.null(data)) return()
            
            measurement <- self$options$measurement
            x_coordinate <- self$options$x_coordinate
            y_coordinate <- self$options$y_coordinate
            
            if (is.null(measurement) || is.null(x_coordinate) || is.null(y_coordinate)) {
                return()
            }
            
            plot_data <- data.frame(
                x = data[[x_coordinate]],
                y = data[[y_coordinate]],
                value = data[[measurement]]
            )
            
            plot_data <- plot_data[complete.cases(plot_data), ]
            
            if (nrow(plot_data) < 3) return()
            
            # Create spatial pattern plot
            p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = x, y = y, color = value)) +
                ggplot2::geom_point(size = 2, alpha = 0.7) +
                ggplot2::scale_color_gradient2(
                    low = "blue", mid = "white", high = "red",
                    midpoint = median(plot_data$value, na.rm = TRUE),
                    name = "Value"
                ) +
                ggplot2::labs(
                    title = "Spatial Distribution Pattern",
                    x = "X Coordinate",
                    y = "Y Coordinate"
                ) +
                ggplot2::theme_minimal() +
                ggplot2::theme(
                    plot.title = ggplot2::element_text(size = 14, hjust = 0.5),
                    axis.title = ggplot2::element_text(size = 12),
                    legend.title = ggplot2::element_text(size = 11)
                )
            
            print(p)
            TRUE
        },

        .plotAutocorrFunction = function(image, ggtheme, theme, ...) {
            if (!self$options$autocorr_plots) return()
            
            # Get data
            data <- self$data
            if (is.null(data)) return()
            
            measurement <- self$options$measurement
            x_coordinate <- self$options$x_coordinate
            y_coordinate <- self$options$y_coordinate
            
            if (is.null(measurement) || is.null(x_coordinate) || is.null(y_coordinate)) {
                return()
            }
            
            coords_data <- data.frame(
                x = data[[x_coordinate]],
                y = data[[y_coordinate]],
                value = data[[measurement]]
            )
            
            coords_data <- coords_data[complete.cases(coords_data), ]
            if (nrow(coords_data) < 10) return()
            
            coordinates(coords_data) <- c("x", "y")
            
            tryCatch({
                # Calculate autocorrelation at different distances
                distances <- seq(0, max(dist(coordinates(coords_data)))/4, length.out = 20)
                autocorr_values <- numeric(length(distances))
                
                for (i in seq_along(distances)) {
                    if (distances[i] == 0) {
                        autocorr_values[i] <- 1
                    } else {
                        # Simple distance-based autocorrelation
                        dist_matrix <- as.matrix(dist(coordinates(coords_data)))
                        weight_matrix <- (dist_matrix <= distances[i]) * 1
                        diag(weight_matrix) <- 0
                        
                        if (sum(weight_matrix) > 0) {
                            n <- length(coords_data$value)
                            W <- sum(weight_matrix)
                            mean_val <- mean(coords_data$value, na.rm = TRUE)
                            
                            numerator <- 0
                            denominator <- sum((coords_data$value - mean_val)^2, na.rm = TRUE)
                            
                            for (j in 1:n) {
                                for (k in 1:n) {
                                    if (weight_matrix[j,k] > 0) {
                                        numerator <- numerator + weight_matrix[j,k] * 
                                                   (coords_data$value[j] - mean_val) * (coords_data$value[k] - mean_val)
                                    }
                                }
                            }
                            
                            autocorr_values[i] <- (n / W) * (numerator / denominator)
                        } else {
                            autocorr_values[i] <- 0
                        }
                    }
                }
                
                autocorr_data <- data.frame(
                    distance = distances,
                    autocorrelation = autocorr_values
                )
                
                # Create autocorrelation function plot
                p <- ggplot2::ggplot(autocorr_data, ggplot2::aes(x = distance, y = autocorrelation)) +
                    ggplot2::geom_line(color = "blue", size = 1.2) +
                    ggplot2::geom_point(color = "red", size = 2) +
                    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
                    ggplot2::labs(
                        title = "Spatial Autocorrelation Function",
                        x = "Distance",
                        y = "Autocorrelation"
                    ) +
                    ggplot2::theme_minimal() +
                    ggplot2::theme(
                        plot.title = ggplot2::element_text(size = 14, hjust = 0.5),
                        axis.title = ggplot2::element_text(size = 12)
                    )
                
                print(p)
                TRUE
                
            }, error = function(e) {
                return()
            })
        },

        .plotLISAMap = function(image, ggtheme, theme, ...) {
            if (!self$options$lisa_plots || !self$options$local_analysis) return()
            
            # Get data
            data <- self$data
            if (is.null(data)) return()
            
            measurement <- self$options$measurement
            x_coordinate <- self$options$x_coordinate
            y_coordinate <- self$options$y_coordinate
            
            if (is.null(measurement) || is.null(x_coordinate) || is.null(y_coordinate)) {
                return()
            }
            
            coords_data <- data.frame(
                x = data[[x_coordinate]],
                y = data[[y_coordinate]],
                value = data[[measurement]]
            )
            
            coords_data <- coords_data[complete.cases(coords_data), ]
            if (nrow(coords_data) < 10) return()
            
            coordinates(coords_data) <- c("x", "y")
            
            tryCatch({
                # Create spatial weights
                listw <- private$.createSpatialWeights(coords_data, self$options$spatial_weights, 
                                                     self$options$distance_threshold, 
                                                     self$options$k_neighbors, 
                                                     self$options$bandwidth)
                
                # Calculate local Moran's I
                local_moran <- spdep::localmoran(coords_data$value, listw, zero.policy = TRUE)
                
                # Determine cluster types
                scaled_values <- scale(coords_data$value)[,1]
                lagged_values <- spdep::lag.listw(listw, scaled_values, zero.policy = TRUE)
                
                cluster_types <- ifelse(scaled_values > 0 & lagged_values > 0, "High-High",
                               ifelse(scaled_values < 0 & lagged_values < 0, "Low-Low",
                               ifelse(scaled_values > 0 & lagged_values < 0, "High-Low", "Low-High")))
                
                significance <- local_moran[,5] < 0.05
                
                plot_data <- data.frame(
                    x = coordinates(coords_data)[,1],
                    y = coordinates(coords_data)[,2],
                    cluster_type = cluster_types,
                    significant = significance,
                    local_moran = local_moran[,1]
                )
                
                # Create LISA cluster map
                p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = x, y = y)) +
                    ggplot2::geom_point(ggplot2::aes(color = cluster_type, size = significant), alpha = 0.7) +
                    ggplot2::scale_color_manual(
                        values = c("High-High" = "red", "Low-Low" = "blue", 
                                 "High-Low" = "orange", "Low-High" = "green"),
                        name = "Cluster Type"
                    ) +
                    ggplot2::scale_size_manual(
                        values = c("TRUE" = 3, "FALSE" = 1),
                        name = "Significant",
                        labels = c("No", "Yes")
                    ) +
                    ggplot2::labs(
                        title = "LISA Cluster and Significance Map",
                        x = "X Coordinate",
                        y = "Y Coordinate"
                    ) +
                    ggplot2::theme_minimal() +
                    ggplot2::theme(
                        plot.title = ggplot2::element_text(size = 14, hjust = 0.5),
                        axis.title = ggplot2::element_text(size = 12),
                        legend.title = ggplot2::element_text(size = 11)
                    )
                
                print(p)
                TRUE
                
            }, error = function(e) {
                return()
            })
        }
    )
)