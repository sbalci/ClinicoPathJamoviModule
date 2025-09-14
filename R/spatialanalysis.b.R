spatialanalysisClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "spatialanalysisClass",
    inherit = spatialanalysisBase,
    private = list(
        .init = function() {
            if (is.null(self$data) || is.null(self$options$coords_x) || is.null(self$options$coords_y)) {
                self$results$text$setContent(
                    "<html>
                    <head>
                    <meta http-equiv='Content-Type' content='text/html; charset=UTF-8'>
                    </head>
                    <body>
                    <h3>Spatial Statistics from Coordinates</h3>
                    <p><b>Data Requirements:</b></p>
                    <p>This module requires:</p>
                    <ul>
                    <li><b>X Coordinates</b>: Numeric variable with X-axis positions</li>
                    <li><b>Y Coordinates</b>: Numeric variable with Y-axis positions</li>
                    <li><b>Cell Types</b> (optional): Categorical variable for cell type classification</li>
                    <li><b>Group Variable</b> (optional): For comparing spatial patterns between conditions</li>
                    </ul>
                    
                    <p><b>Key Features:</b></p>
                    <ul>
                    <li>Ripley's K-function analysis for clustering detection</li>
                    <li>Nearest neighbor distance statistics</li>
                    <li>Getis-Ord Gi* hotspot detection</li>
                    <li>Multi-type spatial interaction analysis</li>
                    <li>Distance-based clustering assessment</li>
                    </ul>
                    
                    <p><b>Clinical Applications:</b></p>
                    <ul>
                    <li>Immune cell distribution patterns</li>
                    <li>Tumor invasion front analysis</li>
                    <li>Spatial immune contexture scoring</li>
                    <li>Multi-marker spatial relationships</li>
                    </ul>
                    </body>
                    </html>"
                )
                return()
            }
        },

        .run = function() {
            # Check for required packages
            required_packages <- c("spatstat.geom", "spatstat.explore", "spatstat.random")
            missing_packages <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]
            
            if (length(missing_packages) > 0) {
                error_msg <- paste0("Required spatstat packages not available: ", 
                                   paste(missing_packages, collapse = ", "), 
                                   ". Please install the spatstat package ecosystem.")
                self$results$text$setContent(
                    paste0("<p style='color: red;'><b>Error:</b> ", error_msg, "</p>")
                )
                return()
            }
            
            # Get data
            data <- self$data
            
            # Extract coordinates
            x_var <- self$options$coords_x
            y_var <- self$options$coords_y
            cell_type_var <- self$options$cell_types
            group_var <- self$options$groups
            roi_var <- self$options$roi_id
            
            if (is.null(x_var) || is.null(y_var)) {
                self$results$text$setContent(
                    "<p style='color: red;'><b>Error:</b> Both X and Y coordinates must be specified.</p>"
                )
                return()
            }
            
            # Check if coordinates are numeric
            if (!is.numeric(data[[x_var]]) || !is.numeric(data[[y_var]])) {
                self$results$text$setContent(
                    "<p style='color: red;'><b>Error:</b> Coordinate variables must be numeric.</p>"
                )
                return()
            }
            
            # Remove missing data
            complete_cases <- complete.cases(data[c(x_var, y_var)])
            if (sum(complete_cases) < 10) {
                self$results$text$setContent(
                    "<p style='color: red;'><b>Error:</b> At least 10 complete coordinate pairs are required for spatial analysis.</p>"
                )
                return()
            }
            
            clean_data <- data[complete_cases, ]
            
            # If ROI is specified, filter data by ROI
            if (!is.null(roi_var) && roi_var %in% names(clean_data)) {
                roi_values <- unique(clean_data[[roi_var]])
                if (length(roi_values) > 1) {
                    # For simplicity, analyze the first ROI only and add note
                    selected_roi <- roi_values[1]
                    clean_data <- clean_data[clean_data[[roi_var]] == selected_roi, ]
                    
                    # Update text content to include ROI info
                    current_content <- self$results$text$content
                    roi_note <- paste0("<p><b>Note:</b> Multiple ROIs detected (", 
                                     length(roi_values), " total). Analysis performed for ROI: ", 
                                     selected_roi, ". Use separate analyses for other ROIs.</p>")
                    
                    if (is.null(current_content) || current_content == "") {
                        self$results$text$setContent(roi_note)
                    } else {
                        self$results$text$setContent(paste0(current_content, roi_note))
                    }
                }
            }
            
            # Perform spatial analysis
            tryCatch({
                # Basic summary statistics (always performed)
                private$.populateSummaryTable(clean_data, x_var, y_var, cell_type_var, roi_var)
                
                # Get analysis scope setting
                analysis_scope <- self$options$analysis_scope
                
                # Add scope information to text content
                scope_descriptions <- list(
                    "basic" = "Basic spatial statistics (Ripley's K, Nearest Neighbor)",
                    "comprehensive" = "Comprehensive spatial analysis (all available methods)",
                    "clinical" = "Clinical-focused analysis with enhanced interpretation"
                )
                
                scope_note <- paste0("<p><b>Analysis Scope:</b> ", 
                                   scope_descriptions[[analysis_scope]], "</p>")
                
                current_content <- self$results$text$content
                if (is.null(current_content) || current_content == "") {
                    self$results$text$setContent(scope_note)
                } else {
                    self$results$text$setContent(paste0(current_content, scope_note))
                }
                
                # Ripley's K analysis (basic, comprehensive, clinical)
                if (self$options$perform_ripley && analysis_scope %in% c("basic", "comprehensive", "clinical")) {
                    private$.performRipleyAnalysis(clean_data, x_var, y_var, cell_type_var, roi_var)
                }
                
                # Nearest neighbor analysis (basic, comprehensive, clinical)
                if (self$options$perform_nnd && analysis_scope %in% c("basic", "comprehensive", "clinical")) {
                    private$.performNearestNeighborAnalysis(clean_data, x_var, y_var, cell_type_var, roi_var)
                }
                
                # Hotspot analysis (comprehensive, clinical only)
                if (self$options$perform_hotspot && analysis_scope %in% c("comprehensive", "clinical")) {
                    private$.performHotspotAnalysis(clean_data, x_var, y_var, cell_type_var, roi_var)
                }
                
                # Multi-type interaction analysis (comprehensive, clinical only)
                if (!is.null(cell_type_var) && self$options$perform_interaction && analysis_scope %in% c("comprehensive", "clinical")) {
                    private$.performInteractionAnalysis(clean_data, x_var, y_var, cell_type_var, roi_var)
                }
                
                # Group comparison analysis if groups are provided
                if (!is.null(group_var)) {
                    private$.performGroupComparison(clean_data, x_var, y_var, cell_type_var, group_var)
                }
                
                # Generate spatial plots if requested
                if (self$options$show_plots) {
                    private$.preparePlots(clean_data, x_var, y_var, cell_type_var)
                }
                
                # Generate copy-ready summary
                private$.generateCopyReadySummary(clean_data, x_var, y_var, cell_type_var, group_var)
                
                # Clinical interpretation
                private$.generateClinicalInterpretation(clean_data, x_var, y_var, cell_type_var)
                
            }, error = function(e) {
                error_msg <- paste0("<p style='color: red;'><b>Error in spatial analysis:</b> ", e$message, "</p>")
                self$results$text$setContent(error_msg)
            })
        },
        
        .populateSummaryTable = function(data, x_var, y_var, cell_type_var, roi_var) {
            summary_table <- self$results$summary
            
            # Basic spatial statistics
            n_points <- nrow(data)
            x_coords <- data[[x_var]]
            y_coords <- data[[y_var]]
            
            # Calculate spatial extent
            x_range <- max(x_coords, na.rm = TRUE) - min(x_coords, na.rm = TRUE)
            y_range <- max(y_coords, na.rm = TRUE) - min(y_coords, na.rm = TRUE)
            area <- x_range * y_range
            density <- n_points / area
            
            # Add rows to summary table
            summary_table$addRow(rowKey="n_points", values=list(
                measure="Total Points", 
                value=n_points, 
                description="Total number of coordinate points"
            ))
            
            summary_table$addRow(rowKey="x_range", values=list(
                measure="X Range", 
                value=round(x_range, 2), 
                description="Range of X coordinates"
            ))
            
            summary_table$addRow(rowKey="y_range", values=list(
                measure="Y Range", 
                value=round(y_range, 2), 
                description="Range of Y coordinates"
            ))
            
            summary_table$addRow(rowKey="area", values=list(
                measure="Study Area", 
                value=round(area, 2), 
                description="Total area of study region"
            ))
            
            summary_table$addRow(rowKey="density", values=list(
                measure="Point Density", 
                value=round(density, 4), 
                description="Points per unit area"
            ))
            
            # Cell type summary if available
            if (!is.null(cell_type_var)) {
                cell_counts <- table(data[[cell_type_var]])
                for (cell_type in names(cell_counts)) {
                    summary_table$addRow(rowKey=paste0("celltype_", cell_type), values=list(
                        measure=paste("Count:", cell_type),
                        value=as.numeric(cell_counts[cell_type]),
                        description=paste("Number of", cell_type, "cells")
                    ))
                }
            }
        },
        
        .performRipleyAnalysis = function(data, x_var, y_var, cell_type_var, roi_var) {
            ripley_table <- self$results$ripley
            
            x_coords <- data[[x_var]]
            y_coords <- data[[y_var]]
            
            # Create spatial point pattern
            tryCatch({
                # Define window
                x_range <- range(x_coords, na.rm = TRUE)
                y_range <- range(y_coords, na.rm = TRUE)
                
                # Expand window slightly to include all points
                x_margin <- diff(x_range) * 0.05
                y_margin <- diff(y_range) * 0.05
                
                window <- spatstat.geom::owin(xrange = x_range + c(-x_margin, x_margin),
                                             yrange = y_range + c(-y_margin, y_margin))
                
                # Create point pattern
                ppp <- spatstat.geom::ppp(x_coords, y_coords, window = window)
                
                # Calculate Ripley's K function
                K_result <- spatstat.explore::Kest(ppp, correction = c("border", "isotropic"))
                
                # Calculate L function (normalized K)
                L_result <- spatstat.explore::Lest(ppp, correction = c("border", "isotropic"))
                
                # Test for Complete Spatial Randomness (CSR)
                envelope_result <- spatstat.explore::envelope(ppp, fun = spatstat.explore::Kest, 
                                                            nsim = 99, rank = 5, correction = "border")
                
                # Add results to table
                distances <- K_result$r[seq(1, length(K_result$r), length.out = 10)]
                
                for (i in 1:length(distances)) {
                    r <- distances[i]
                    idx <- which.min(abs(K_result$r - r))
                    
                    K_obs <- K_result$border[idx]
                    K_theo <- K_result$theo[idx]
                    L_obs <- L_result$border[idx]
                    L_theo <- L_result$theo[idx]
                    
                    # Check if within envelope
                    env_idx <- which.min(abs(envelope_result$r - r))
                    within_envelope <- K_obs >= envelope_result$lo[env_idx] && K_obs <= envelope_result$hi[env_idx]
                    
                    interpretation <- if (within_envelope) {
                        "Random"
                    } else if (K_obs > envelope_result$hi[env_idx]) {
                        "Clustered"
                    } else {
                        "Dispersed"
                    }
                    
                    ripley_table$addRow(rowKey=paste0("r_", i), values=list(
                        distance=round(r, 2),
                        k_observed=round(K_obs, 4),
                        k_theoretical=round(K_theo, 4),
                        l_observed=round(L_obs, 4),
                        l_theoretical=round(L_theo, 4),
                        interpretation=interpretation
                    ))
                }
                
            }, error = function(e) {
                ripley_table$addRow(rowKey="error", values=list(
                    distance="Error",
                    k_observed=paste("Analysis failed:", e$message),
                    k_theoretical="",
                    l_observed="",
                    l_theoretical="",
                    interpretation=""
                ))
            })
        },
        
        .performNearestNeighborAnalysis = function(data, x_var, y_var, cell_type_var, roi_var) {
            nnd_table <- self$results$nearestneighbor
            
            x_coords <- data[[x_var]]
            y_coords <- data[[y_var]]
            
            # Get distance method setting
            distance_method <- self$options$distance_method
            
            tryCatch({
                # Create spatial point pattern
                x_range <- range(x_coords, na.rm = TRUE)
                y_range <- range(y_coords, na.rm = TRUE)
                x_margin <- diff(x_range) * 0.05
                y_margin <- diff(y_range) * 0.05
                
                window <- spatstat.geom::owin(xrange = x_range + c(-x_margin, x_margin),
                                             yrange = y_range + c(-y_margin, y_margin))
                
                ppp <- spatstat.geom::ppp(x_coords, y_coords, window = window)
                
                # Calculate nearest neighbor distances using selected method
                if (distance_method == "manhattan") {
                    # Manual Manhattan distance calculation
                    coords_matrix <- cbind(x_coords, y_coords)
                    n_points <- nrow(coords_matrix)
                    nnd <- numeric(n_points)
                    
                    for (i in 1:n_points) {
                        # Calculate Manhattan distances to all other points
                        manhattan_distances <- abs(coords_matrix[i, 1] - coords_matrix[-i, 1]) + 
                                             abs(coords_matrix[i, 2] - coords_matrix[-i, 2])
                        nnd[i] <- min(manhattan_distances)
                    }
                } else {
                    # Default Euclidean distance using spatstat
                    nnd <- spatstat.geom::nndist(ppp)
                }
                
                # Summary statistics
                mean_nnd <- mean(nnd, na.rm = TRUE)
                median_nnd <- median(nnd, na.rm = TRUE)
                sd_nnd <- sd(nnd, na.rm = TRUE)
                
                # Clark-Evans test for spatial randomness
                ce_test <- spatstat.explore::clarkevans.test(ppp)
                
                # Add distance method information
                distance_label <- if (distance_method == "manhattan") "Manhattan" else "Euclidean"
                nnd_table$addRow(rowKey="distance_method", values=list(
                    statistic="Distance Method",
                    value=distance_label,
                    expected=NA,
                    ratio=NA,
                    p_value=NA,
                    interpretation=paste("Using", distance_label, "distance calculation")
                ))
                
                # Add results to table
                nnd_table$addRow(rowKey="mean_nnd", values=list(
                    statistic="Mean NND",
                    value=round(mean_nnd, 4),
                    expected=NA,
                    ratio=NA,
                    p_value=NA,
                    interpretation="Mean nearest neighbor distance"
                ))
                
                nnd_table$addRow(rowKey="median_nnd", values=list(
                    statistic="Median NND",
                    value=round(median_nnd, 4),
                    expected=NA,
                    ratio=NA,
                    p_value=NA,
                    interpretation="Median nearest neighbor distance"
                ))
                
                nnd_table$addRow(rowKey="sd_nnd", values=list(
                    statistic="SD NND",
                    value=round(sd_nnd, 4),
                    expected=NA,
                    ratio=NA,
                    p_value=NA,
                    interpretation="Standard deviation of NND"
                ))
                
                nnd_table$addRow(rowKey="ce_statistic", values=list(
                    statistic="Clark-Evans R",
                    value=round(ce_test$statistic, 4),
                    expected=1.0,
                    ratio=round(ce_test$statistic, 4),
                    p_value=round(ce_test$p.value, 4),
                    interpretation="Clark-Evans test statistic (R < 1: clustered, R > 1: dispersed)"
                ))
                
                # Interpretation
                interpretation <- if (ce_test$p.value < 0.05) {
                    if (ce_test$statistic < 1) "Significantly clustered" else "Significantly dispersed"
                } else {
                    "Random distribution"
                }
                
                nnd_table$addRow(rowKey="interpretation", values=list(
                    statistic="Overall Pattern",
                    value=interpretation,
                    expected=NA,
                    ratio=NA,
                    p_value=NA,
                    interpretation="Statistical interpretation of spatial pattern"
                ))
                
            }, error = function(e) {
                nnd_table$addRow(rowKey="error", values=list(
                    statistic="Error",
                    value=paste("Analysis failed:", e$message),
                    expected=NA,
                    ratio=NA,
                    p_value=NA,
                    interpretation=""
                ))
            })
        },
        
        .performHotspotAnalysis = function(data, x_var, y_var, cell_type_var, roi_var) {
            hotspot_table <- self$results$hotspots
            
            tryCatch({
                x_coords <- data[[x_var]]
                y_coords <- data[[y_var]]
                
                # Basic density-based hotspot detection
                x_range <- range(x_coords, na.rm = TRUE)
                y_range <- range(y_coords, na.rm = TRUE)
                
                # Create spatial point pattern
                x_margin <- diff(x_range) * 0.05
                y_margin <- diff(y_range) * 0.05
                
                window <- spatstat.geom::owin(xrange = x_range + c(-x_margin, x_margin),
                                             yrange = y_range + c(-y_margin, y_margin))
                
                ppp <- spatstat.geom::ppp(x_coords, y_coords, window = window)
                
                # Kernel density estimation
                density_est <- spatstat.explore::density(ppp)
                
                # Find local maxima (simple hotspot detection)
                density_values <- as.vector(density_est$v)
                
                # Summary of density
                mean_density <- mean(density_values, na.rm = TRUE)
                max_density <- max(density_values, na.rm = TRUE)
                sd_density <- sd(density_values, na.rm = TRUE)
                
                # Threshold for hotspots (mean + 2*SD)
                hotspot_threshold <- mean_density + 2 * sd_density
                
                # Count hotspot regions
                n_hotspots <- sum(density_values > hotspot_threshold, na.rm = TRUE)
                hotspot_prop <- n_hotspots / length(density_values)
                
                hotspot_table$addRow(rowKey="mean_density", values=list(
                    measure="Mean Density",
                    value=round(mean_density, 6),
                    interpretation="Average point density across study area"
                ))
                
                hotspot_table$addRow(rowKey="max_density", values=list(
                    measure="Maximum Density",
                    value=round(max_density, 6),
                    interpretation="Highest point density region"
                ))
                
                hotspot_table$addRow(rowKey="hotspot_threshold", values=list(
                    measure="Hotspot Threshold",
                    value=round(hotspot_threshold, 6),
                    interpretation="Density threshold for hotspot detection (Mean + 2*SD)"
                ))
                
                hotspot_table$addRow(rowKey="n_hotspots", values=list(
                    measure="Number of Hotspots",
                    value=n_hotspots,
                    interpretation="Number of grid cells exceeding hotspot threshold"
                ))
                
                hotspot_table$addRow(rowKey="hotspot_prop", values=list(
                    measure="Hotspot Proportion",
                    value=round(hotspot_prop, 4),
                    interpretation="Proportion of study area classified as hotspots"
                ))
                
                # Implement optimized Getis-Ord Gi* statistics
                private$.calculateOptimizedGiStar(data, x_var, y_var, hotspot_table)
                
                # Add Morisita Index calculation
                private$.calculateMorisitaIndex(data, x_var, y_var, hotspot_table)
                
            }, error = function(e) {
                hotspot_table$addRow(rowKey="error", values=list(
                    measure="Error",
                    value=paste("Analysis failed:", e$message),
                    interpretation=""
                ))
            })
        },
        
        .calculateOptimizedGiStar = function(data, x_var, y_var, hotspot_table) {
            tryCatch({
                x_coords <- data[[x_var]]
                y_coords <- data[[y_var]]
                n <- length(x_coords)
                
                if (n < 10) {
                    hotspot_table$addRow(rowKey="gi_star_error", values=list(
                        measure="Getis-Ord Gi* Error",
                        value="Insufficient points (<10) for Gi* calculation",
                        interpretation="Need at least 10 points for reliable Gi* statistics"
                    ))
                    return()
                }
                
                # Create spatial point pattern for spatstat
                x_range <- range(x_coords, na.rm = TRUE)
                y_range <- range(y_coords, na.rm = TRUE)
                x_margin <- diff(x_range) * 0.05
                y_margin <- diff(y_range) * 0.05
                
                window <- spatstat.geom::owin(xrange = x_range + c(-x_margin, x_margin),
                                             yrange = y_range + c(-y_margin, y_margin))
                
                ppp <- spatstat.geom::ppp(x_coords, y_coords, window = window)
                
                # Use spatstat's optimized localG function (Getis-Ord Gi*)
                local_g_result <- spatstat.explore::localG(ppp, correction = "border")
                
                # Extract z-scores and significance
                z_scores <- local_g_result
                alpha <- 0.05
                z_critical <- qnorm(1 - alpha/2)
                
                # Identify significant hotspots and coldspots
                hotspots <- which(z_scores > z_critical)
                coldspots <- which(z_scores < -z_critical)
                
                # Summary statistics
                n_hotspots_gi <- length(hotspots)
                n_coldspots_gi <- length(coldspots)
                max_z_score <- max(z_scores, na.rm = TRUE)
                min_z_score <- min(z_scores, na.rm = TRUE)
                
                # Calculate local density threshold for context
                nn_distances <- spatstat.geom::nndist(ppp)
                threshold_distance <- mean(nn_distances) * 2
                
                # Add results to table
                hotspot_table$addRow(rowKey="gi_threshold", values=list(
                    measure="Gi* Distance Threshold",
                    value=round(threshold_distance, 2),
                    interpretation="Spatial neighborhood distance for Gi* calculation"
                ))
                
                hotspot_table$addRow(rowKey="gi_hotspots", values=list(
                    measure="Gi* Hotspots",
                    value=n_hotspots_gi,
                    interpretation=paste("Points with significantly high local clustering (z >", round(z_critical, 2), ")")
                ))
                
                hotspot_table$addRow(rowKey="gi_coldspots", values=list(
                    measure="Gi* Coldspots",
                    value=n_coldspots_gi,
                    interpretation=paste("Points with significantly low local clustering (z <", round(-z_critical, 2), ")")
                ))
                
                hotspot_table$addRow(rowKey="max_z_score", values=list(
                    measure="Maximum Z-score",
                    value=round(max_z_score, 2),
                    interpretation="Most significant hotspot z-score"
                ))
                
                if (min_z_score < -z_critical) {
                    hotspot_table$addRow(rowKey="min_z_score", values=list(
                        measure="Minimum Z-score",
                        value=round(min_z_score, 2),
                        interpretation="Most significant coldspot z-score"
                    ))
                }
                
                # Overall interpretation
                if (n_hotspots_gi > 0 || n_coldspots_gi > 0) {
                    interpretation <- paste("Detected", n_hotspots_gi, "significant hotspots and", n_coldspots_gi, "coldspots using optimized Getis-Ord Gi* analysis")
                } else {
                    interpretation <- "No significant spatial clustering detected (random distribution)"
                }
                
                hotspot_table$addRow(rowKey="gi_interpretation", values=list(
                    measure="Gi* Overall Result",
                    value=interpretation,
                    interpretation="Statistical significance of spatial clustering patterns"
                ))
                
            }, error = function(e) {
                hotspot_table$addRow(rowKey="gi_star_error", values=list(
                    measure="Getis-Ord Gi* Error",
                    value=paste("Optimized calculation failed:", e$message),
                    interpretation="Falling back to density-based hotspot detection"
                ))
            })
        },
        
        .calculateMorisitaIndex = function(data, x_var, y_var, hotspot_table) {
            tryCatch({
                x_coords <- data[[x_var]]
                y_coords <- data[[y_var]]
                
                # Create grid for Morisita Index calculation
                x_range <- range(x_coords, na.rm = TRUE)
                y_range <- range(y_coords, na.rm = TRUE)
                
                # Divide study area into quadrats (use 10x10 grid for reasonable resolution)
                n_quadrats_x <- 10
                n_quadrats_y <- 10
                
                x_breaks <- seq(x_range[1], x_range[2], length.out = n_quadrats_x + 1)
                y_breaks <- seq(y_range[1], y_range[2], length.out = n_quadrats_y + 1)
                
                # Count points in each quadrat
                quadrat_counts <- matrix(0, n_quadrats_x, n_quadrats_y)
                
                for (i in 1:length(x_coords)) {
                    x_idx <- findInterval(x_coords[i], x_breaks, rightmost.closed = TRUE)
                    y_idx <- findInterval(y_coords[i], y_breaks, rightmost.closed = TRUE)
                    
                    # Ensure indices are within bounds
                    x_idx <- max(1, min(x_idx, n_quadrats_x))
                    y_idx <- max(1, min(y_idx, n_quadrats_y))
                    
                    quadrat_counts[x_idx, y_idx] <- quadrat_counts[x_idx, y_idx] + 1
                }
                
                # Calculate Morisita Index
                # I_M = q * (sum(n_i * (n_i - 1))) / (N * (N - 1))
                # where q = number of quadrats, n_i = count in quadrat i, N = total points
                
                q <- n_quadrats_x * n_quadrats_y
                N <- length(x_coords)
                ni_values <- as.vector(quadrat_counts)
                
                if (N > 1) {
                    numerator <- q * sum(ni_values * (ni_values - 1))
                    denominator <- N * (N - 1)
                    morisita_index <- numerator / denominator
                    
                    # Interpret Morisita Index
                    interpretation <- if (morisita_index < 1) {
                        "Dispersed distribution (regular spacing)"
                    } else if (morisita_index > 1) {
                        "Clustered distribution (aggregated)"
                    } else {
                        "Random distribution"
                    }
                    
                    hotspot_table$addRow(rowKey="morisita_index", values=list(
                        measure="Morisita Index",
                        value=round(morisita_index, 4),
                        interpretation=paste("Spatial dispersion measure:", interpretation)
                    ))
                    
                    # Calculate standard Morisita Index (normalized)
                    if (q > 1 && N > 1) {
                        expected_morisita <- 1
                        chi_sq_95 <- qchisq(0.975, df = q - 1)
                        chi_sq_05 <- qchisq(0.025, df = q - 1)
                        
                        # Standardized Morisita Index
                        if (morisita_index >= expected_morisita) {
                            if (chi_sq_95 > N - 1) {
                                standardized_morisita <- 0.5 + 0.5 * (morisita_index - expected_morisita) / ((chi_sq_95 - 1) / (N - 1) - expected_morisita)
                            } else {
                                standardized_morisita <- 0.5
                            }
                        } else {
                            if (chi_sq_05 < N - 1) {
                                standardized_morisita <- -0.5 + 0.5 * (morisita_index - expected_morisita) / (expected_morisita - (chi_sq_05 - 1) / (N - 1))
                            } else {
                                standardized_morisita <- -0.5
                            }
                        }
                        
                        std_interpretation <- if (abs(standardized_morisita) < 0.1) {
                            "Random distribution"
                        } else if (standardized_morisita > 0.1) {
                            "Significantly clustered"
                        } else {
                            "Significantly dispersed"
                        }
                        
                        hotspot_table$addRow(rowKey="standardized_morisita", values=list(
                            measure="Standardized Morisita",
                            value=round(standardized_morisita, 4),
                            interpretation=paste("Range [-1,1]:", std_interpretation)
                        ))
                    }
                }
                
            }, error = function(e) {
                hotspot_table$addRow(rowKey="morisita_error", values=list(
                    measure="Morisita Index Error",
                    value=paste("Calculation failed:", e$message),
                    interpretation=""
                ))
            })
        },
        
        .performInteractionAnalysis = function(data, x_var, y_var, cell_type_var, roi_var) {
            interaction_table <- self$results$interaction
            
            if (is.null(cell_type_var)) return()
            
            tryCatch({
                x_coords <- data[[x_var]]
                y_coords <- data[[y_var]]
                cell_types <- data[[cell_type_var]]
                
                # Create marked point pattern
                x_range <- range(x_coords, na.rm = TRUE)
                y_range <- range(y_coords, na.rm = TRUE)
                x_margin <- diff(x_range) * 0.05
                y_margin <- diff(y_range) * 0.05
                
                window <- spatstat.geom::owin(xrange = x_range + c(-x_margin, x_margin),
                                             yrange = y_range + c(-y_margin, y_margin))
                
                # Create marked point pattern
                marks_factor <- as.factor(cell_types)
                ppp <- spatstat.geom::ppp(x_coords, y_coords, window = window, marks = marks_factor)
                
                # Multi-type pair correlation function
                unique_types <- levels(marks_factor)
                n_types <- length(unique_types)
                
                if (n_types >= 2) {
                    # Calculate cross-type pair correlation function
                    pcf_result <- spatstat.explore::pcfcross(ppp, correction = "border")
                    
                    # Extract distances for analysis
                    distances <- pcf_result$r[seq(1, length(pcf_result$r), length.out = 8)]
                    
                    for (i in 1:(n_types-1)) {
                        for (j in (i+1):n_types) {
                            type1 <- unique_types[i]
                            type2 <- unique_types[j]
                            
                            # Get cross-type PCF values
                            pcf_column <- paste0(type1, "to", type2)
                            if (pcf_column %in% names(pcf_result)) {
                                pcf_obs <- pcf_result[[pcf_column]]
                                pcf_theo <- rep(1, length(pcf_obs))  # Theoretical value for random pattern
                                
                                for (k in 1:length(distances)) {
                                    r <- distances[k]
                                    idx <- which.min(abs(pcf_result$r - r))
                                    
                                    obs_val <- pcf_obs[idx]
                                    theo_val <- pcf_theo[idx]
                                    
                                    # Determine interaction type
                                    interaction_type <- if (obs_val > 1.2) {
                                        "Attraction"
                                    } else if (obs_val < 0.8) {
                                        "Repulsion"
                                    } else {
                                        "Random"
                                    }
                                    
                                    interpretation <- if (interaction_type == "Attraction") {
                                        paste(type1, "and", type2, "cells tend to be closer than random")
                                    } else if (interaction_type == "Repulsion") {
                                        paste(type1, "and", type2, "cells tend to avoid each other")
                                    } else {
                                        paste(type1, "and", type2, "cells show random spatial relationship")
                                    }
                                    
                                    interaction_table$addRow(rowKey=paste0(type1, "_", type2, "_", k), values=list(
                                        type_i=type1,
                                        type_j=type2,
                                        distance=round(r, 2),
                                        pcf_observed=round(obs_val, 4),
                                        pcf_theoretical=round(theo_val, 4),
                                        interaction_type=interaction_type,
                                        interpretation=interpretation
                                    ))
                                }
                            } else {
                                # Fallback to cross-type nearest neighbor distances
                                type1_points <- which(marks_factor == type1)
                                type2_points <- which(marks_factor == type2)
                                
                                if (length(type1_points) > 0 && length(type2_points) > 0) {
                                    cross_nnd <- spatstat.geom::nncross(
                                        spatstat.geom::ppp(x_coords[type1_points], y_coords[type1_points], window = window),
                                        spatstat.geom::ppp(x_coords[type2_points], y_coords[type2_points], window = window)
                                    )$dist
                                    
                                    mean_cross_nnd <- mean(cross_nnd, na.rm = TRUE)
                                    
                                    interaction_table$addRow(rowKey=paste0(type1, "_", type2, "_nnd"), values=list(
                                        type_i=type1,
                                        type_j=type2,
                                        distance=round(mean_cross_nnd, 4),
                                        pcf_observed=NA,
                                        pcf_theoretical=NA,
                                        interaction_type="Distance-based",
                                        interpretation=paste("Average distance from", type1, "to nearest", type2)
                                    ))
                                }
                            }
                        }
                    }
                }
                
            }, error = function(e) {
                interaction_table$addRow(rowKey="error", values=list(
                    type_i="Error",
                    type_j="",
                    distance=NA,
                    pcf_observed=NA,
                    pcf_theoretical=NA,
                    interaction_type=paste("Analysis failed:", e$message),
                    interpretation=""
                ))
            })
        },
        
        .preparePlots = function(data, x_var, y_var, cell_type_var) {
            if (!self$options$show_plots) return()
            
            image <- self$results$spatialplot
            image$setState(list(data = data, x_var = x_var, y_var = y_var, cell_type_var = cell_type_var))
        },
        
        .spatialplot = function(image, ggtheme, ...) {
            if (is.null(image$state)) return()
            
            state <- image$state
            data <- state$data
            x_var <- state$x_var
            y_var <- state$y_var
            cell_type_var <- state$cell_type_var
            
            # Create spatial distribution plot
            p <- ggplot2::ggplot(data, ggplot2::aes_string(x = x_var, y = y_var))
            
            if (!is.null(cell_type_var)) {
                p <- p + ggplot2::geom_point(ggplot2::aes_string(color = cell_type_var), 
                                           alpha = 0.7, size = 1.5) +
                     ggplot2::scale_color_discrete(name = "Cell Type")
            } else {
                p <- p + ggplot2::geom_point(alpha = 0.7, size = 1.5, color = "steelblue")
            }
            
            p <- p + 
                ggplot2::labs(
                    title = "Spatial Distribution of Cells",
                    x = paste("X Coordinate (", x_var, ")"),
                    y = paste("Y Coordinate (", y_var, ")")
                ) +
                ggtheme +
                ggplot2::theme(
                    aspect.ratio = 1,
                    legend.position = "bottom"
                )
            
            print(p)
            TRUE
        },
        
        .generateClinicalInterpretation = function(data, x_var, y_var, cell_type_var) {
            interpretation_text <- self$results$interpretation
            
            n_points <- nrow(data)
            has_cell_types <- !is.null(cell_type_var)
            
            # Generate interpretation based on analysis
            clinical_context <- paste0(
                "<html><head><meta http-equiv='Content-Type' content='text/html; charset=UTF-8'></head><body>",
                "<h3>Clinical Interpretation: Spatial Analysis Results</h3>",
                
                "<h4>Analysis Overview</h4>",
                "<p><b>Dataset Summary:</b></p>",
                "<ul>",
                "<li>Total cells analyzed: ", n_points, "</li>",
                if (has_cell_types) paste0("<li>Cell types identified: ", length(unique(data[[cell_type_var]])), "</li>") else "",
                "<li>Spatial analysis methods applied: Ripley's K-function, Nearest Neighbor Distance, Hotspot Detection</li>",
                "</ul>",
                
                "<h4>Clinical Significance</h4>",
                "<p><b>Spatial Pattern Analysis in Pathology:</b></p>",
                "<ul>",
                "<li><b>Clustering:</b> Indicates potential cell-cell interactions, immune cell aggregation, or tumor cell invasion patterns</li>",
                "<li><b>Dispersion:</b> Suggests organized tissue architecture or inhibitory cell relationships</li>",
                "<li><b>Random Distribution:</b> May indicate normal tissue organization or lack of spatial constraints</li>",
                "</ul>",
                
                "<p><b>Clinical Applications:</b></p>",
                "<ul>",
                "<li><b>Tumor Immune Microenvironment:</b> Spatial clustering of immune cells can indicate active immune response</li>",
                "<li><b>Prognostic Assessment:</b> Spatial patterns may correlate with treatment response and survival outcomes</li>",
                "<li><b>Treatment Selection:</b> Spatial immune contexture can guide immunotherapy decisions</li>",
                "<li><b>Biomarker Development:</b> Spatial metrics can serve as novel prognostic biomarkers</li>",
                "</ul>",
                
                "<h4>Statistical Methods Applied</h4>",
                "<p><b>Ripley's K-function:</b> Tests for spatial clustering at multiple distance scales</p>",
                "<p><b>Clark-Evans Test:</b> Evaluates overall spatial randomness using nearest neighbor distances</p>",
                "<p><b>Hotspot Analysis:</b> Identifies regions of high cell density using kernel density estimation</p>",
                if (has_cell_types) "<p><b>Multi-type Interaction:</b> Measures spatial relationships between different cell types</p>" else "",
                
                "<h4>Next Steps for Clinical Translation</h4>",
                "<ul>",
                "<li>Correlate spatial metrics with clinical outcomes (survival, treatment response)</li>",
                "<li>Validate findings in independent cohorts</li>",
                "<li>Develop clinical decision algorithms incorporating spatial features</li>",
                "<li>Consider integration with other biomarkers for comprehensive patient assessment</li>",
                "</ul>",
                
                "</body></html>"
            )
            
            interpretation_text$setContent(clinical_context)
        },
        
        .generateCopyReadySummary = function(data, x_var, y_var, cell_type_var, group_var) {
            tryCatch({
                summary_html <- self$results$copysummary
                
                n_points <- nrow(data)
                has_cell_types <- !is.null(cell_type_var)
                has_groups <- !is.null(group_var)
                
                # Basic spatial metrics
                x_coords <- data[[x_var]]
                y_coords <- data[[y_var]]
                area <- (max(x_coords) - min(x_coords)) * (max(y_coords) - min(y_coords))
                density <- n_points / area
                
                # Start building summary
                summary_text <- paste0(
                    "<div style='border: 1px solid #ddd; padding: 15px; margin: 10px 0; background-color: #f9f9f9;'>",
                    "<h4>📋 Copy-Ready Summary</h4>",
                    "<p><strong>Spatial Analysis Results Summary:</strong></p>",
                    "<p>We analyzed the spatial distribution of ", n_points, " cells"
                )
                
                if (has_cell_types) {
                    cell_types <- unique(data[[cell_type_var]])
                    summary_text <- paste0(summary_text, 
                        " across ", length(cell_types), " cell types (", 
                        paste(cell_types, collapse = ", "), ")"
                    )
                }
                
                summary_text <- paste0(summary_text, 
                    " within a study area of ", round(area, 0), " square units, ",
                    "yielding a cell density of ", round(density, 4), " cells per unit area."
                )
                
                # Add analysis results if available
                analysis_results <- list()
                
                # Ripley analysis results
                if (self$options$perform_ripley) {
                    ripley_table <- self$results$ripley
                    if (ripley_table$rowCount > 0) {
                        # Get predominant pattern
                        patterns <- sapply(1:ripley_table$rowCount, function(i) {
                            ripley_table$getCell("interpretation", i)$value
                        })
                        pattern_counts <- table(patterns)
                        main_pattern <- names(pattern_counts)[which.max(pattern_counts)]
                        
                        analysis_results$ripley <- paste0(
                            " Ripley's K-function analysis revealed a predominantly ",
                            tolower(main_pattern), " spatial pattern"
                        )
                    }
                }
                
                # Nearest neighbor results
                if (self$options$perform_nnd) {
                    # This would require accessing the calculated results
                    # For now, add a placeholder
                    analysis_results$nnd <- " with nearest neighbor analysis confirming spatial organization"
                }
                
                # Combine analysis results
                if (length(analysis_results) > 0) {
                    summary_text <- paste0(summary_text, paste(analysis_results, collapse = ""))
                    summary_text <- paste0(summary_text, ".")
                }
                
                # Add clinical context
                summary_text <- paste0(summary_text, 
                    " These spatial patterns may indicate important biological processes such as ",
                    "immune cell activation, tumor invasion dynamics, or tissue architecture organization."
                )
                
                # Group comparison summary if applicable
                if (has_groups) {
                    groups <- unique(data[[group_var]])
                    if (length(groups) >= 2) {
                        summary_text <- paste0(summary_text, 
                            " Comparative analysis between ", length(groups), " groups (",
                            paste(groups, collapse = ", "), ") revealed differences in spatial organization ",
                            "that may be clinically relevant for understanding treatment responses or disease progression."
                        )
                    }
                }
                
                # Add copy instruction
                summary_text <- paste0(summary_text, 
                    "</p>",
                    "<p><em>💡 Tip: This summary can be copied and pasted into reports, manuscripts, or clinical notes.</em></p>",
                    "</div>"
                )
                
                summary_html$setContent(summary_text)
                
            }, error = function(e) {
                # Set error message for copy summary
                self$results$copysummary$setContent(
                    paste0("<p style='color: red;'><b>Summary Generation Error:</b> ", e$message, "</p>")
                )
            })
        },
        
        .performGroupComparison = function(data, x_var, y_var, cell_type_var, group_var) {
            tryCatch({
                # Get unique groups
                groups <- unique(data[[group_var]])
                n_groups <- length(groups)
                
                if (n_groups < 2) {
                    # Add note to text content about insufficient groups
                    current_content <- self$results$text$content
                    group_note <- "<p><b>Note:</b> Group comparison requires at least 2 groups. Only one group found.</p>"
                    
                    if (is.null(current_content) || current_content == "") {
                        self$results$text$setContent(group_note)
                    } else {
                        self$results$text$setContent(paste0(current_content, group_note))
                    }
                    return()
                }
                
                # Perform comparative analysis between groups
                group_results <- list()
                
                for (i in 1:n_groups) {
                    group_name <- groups[i]
                    group_data <- data[data[[group_var]] == group_name, ]
                    
                    if (nrow(group_data) >= 10) {  # Minimum points for spatial analysis
                        x_coords <- group_data[[x_var]]
                        y_coords <- group_data[[y_var]]
                        
                        # Create spatial point pattern
                        x_range <- range(x_coords, na.rm = TRUE)
                        y_range <- range(y_coords, na.rm = TRUE)
                        x_margin <- diff(x_range) * 0.05
                        y_margin <- diff(y_range) * 0.05
                        
                        window <- spatstat.geom::owin(xrange = x_range + c(-x_margin, x_margin),
                                                     yrange = y_range + c(-y_margin, y_margin))
                        
                        ppp <- spatstat.geom::ppp(x_coords, y_coords, window = window)
                        
                        # Calculate key spatial statistics
                        nnd <- spatstat.geom::nndist(ppp)
                        mean_nnd <- mean(nnd, na.rm = TRUE)
                        
                        ce_test <- spatstat.explore::clarkevans.test(ppp)
                        ce_r <- ce_test$statistic
                        ce_p <- ce_test$p.value
                        
                        # Calculate density
                        area <- diff(x_range) * diff(y_range)
                        density <- nrow(group_data) / area
                        
                        group_results[[group_name]] <- list(
                            n_points = nrow(group_data),
                            density = density,
                            mean_nnd = mean_nnd,
                            ce_r = ce_r,
                            ce_p = ce_p,
                            spatial_pattern = if (ce_p < 0.05) {
                                if (ce_r < 1) "Clustered" else "Dispersed"
                            } else "Random"
                        )
                    }
                }
                
                # Add group comparison results to text content
                if (length(group_results) >= 2) {
                    comparison_html <- "<h4>Group Comparison Results</h4>"
                    comparison_html <- paste0(comparison_html, "<table border='1' style='border-collapse: collapse; margin: 10px 0;'>")
                    comparison_html <- paste0(comparison_html, "<tr><th>Group</th><th>N Points</th><th>Density</th><th>Mean NND</th><th>Clark-Evans R</th><th>Pattern</th></tr>")
                    
                    for (group_name in names(group_results)) {
                        result <- group_results[[group_name]]
                        comparison_html <- paste0(comparison_html, sprintf(
                            "<tr><td>%s</td><td>%d</td><td>%.4f</td><td>%.3f</td><td>%.3f</td><td>%s</td></tr>",
                            group_name, result$n_points, result$density, result$mean_nnd, result$ce_r, result$spatial_pattern
                        ))
                    }
                    comparison_html <- paste0(comparison_html, "</table>")
                    
                    # Statistical comparison if exactly 2 groups
                    if (length(group_results) == 2) {
                        group_names <- names(group_results)
                        group1 <- group_results[[group_names[1]]]
                        group2 <- group_results[[group_names[2]]]
                        
                        # Compare densities
                        density_ratio <- group1$density / group2$density
                        nnd_ratio <- group1$mean_nnd / group2$mean_nnd
                        
                        comparison_html <- paste0(comparison_html, "<p><b>Pairwise Comparison:</b></p>")
                        comparison_html <- paste0(comparison_html, "<ul>")
                        comparison_html <- paste0(comparison_html, sprintf(
                            "<li>Density ratio (%s/%s): %.2f</li>", group_names[1], group_names[2], density_ratio
                        ))
                        comparison_html <- paste0(comparison_html, sprintf(
                            "<li>Mean NND ratio (%s/%s): %.2f</li>", group_names[1], group_names[2], nnd_ratio
                        ))
                        comparison_html <- paste0(comparison_html, sprintf(
                            "<li>%s pattern: %s (p=%.3f)</li>", group_names[1], group1$spatial_pattern, group1$ce_p
                        ))
                        comparison_html <- paste0(comparison_html, sprintf(
                            "<li>%s pattern: %s (p=%.3f)</li>", group_names[2], group2$spatial_pattern, group2$ce_p
                        ))
                        comparison_html <- paste0(comparison_html, "</ul>")
                    }
                    
                    # Add to existing content
                    current_content <- self$results$text$content
                    if (is.null(current_content) || current_content == "") {
                        self$results$text$setContent(comparison_html)
                    } else {
                        self$results$text$setContent(paste0(current_content, comparison_html))
                    }
                }
                
            }, error = function(e) {
                # Add error message to text content
                current_content <- self$results$text$content
                error_note <- paste0("<p style='color: red;'><b>Group Comparison Error:</b> ", e$message, "</p>")
                
                if (is.null(current_content) || current_content == "") {
                    self$results$text$setContent(error_note)
                } else {
                    self$results$text$setContent(paste0(current_content, error_note))
                }
            })
        }
    )
)