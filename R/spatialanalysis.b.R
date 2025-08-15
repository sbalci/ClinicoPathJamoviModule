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
            if (!requireNamespace("spatstat", quietly = TRUE)) {
                stop("Package 'spatstat' is required for spatial analysis but is not installed.")
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
            
            # Perform spatial analysis
            tryCatch({
                # Basic summary statistics
                private$.populateSummaryTable(clean_data, x_var, y_var, cell_type_var, roi_var)
                
                # Ripley's K analysis if requested
                if (self$options$perform_ripley) {
                    private$.performRipleyAnalysis(clean_data, x_var, y_var, cell_type_var, roi_var)
                }
                
                # Nearest neighbor analysis if requested
                if (self$options$perform_nnd) {
                    private$.performNearestNeighborAnalysis(clean_data, x_var, y_var, cell_type_var, roi_var)
                }
                
                # Hotspot analysis if requested
                if (self$options$perform_hotspot) {
                    private$.performHotspotAnalysis(clean_data, x_var, y_var, cell_type_var, roi_var)
                }
                
                # Multi-type interaction analysis if cell types are provided
                if (!is.null(cell_type_var) && self$options$perform_interaction) {
                    private$.performInteractionAnalysis(clean_data, x_var, y_var, cell_type_var, roi_var)
                }
                
                # Generate spatial plots if requested
                if (self$options$show_plots) {
                    private$.preparePlots(clean_data, x_var, y_var, cell_type_var)
                }
                
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
                        l_function=round(L_obs, 4),
                        interpretation=interpretation
                    ))
                }
                
            }, error = function(e) {
                ripley_table$addRow(rowKey="error", values=list(
                    distance="Error",
                    k_observed=paste("Analysis failed:", e$message),
                    k_theoretical="",
                    l_function="",
                    interpretation=""
                ))
            })
        },
        
        .performNearestNeighborAnalysis = function(data, x_var, y_var, cell_type_var, roi_var) {
            nnd_table <- self$results$nearestneighbor
            
            x_coords <- data[[x_var]]
            y_coords <- data[[y_var]]
            
            tryCatch({
                # Create spatial point pattern
                x_range <- range(x_coords, na.rm = TRUE)
                y_range <- range(y_coords, na.rm = TRUE)
                x_margin <- diff(x_range) * 0.05
                y_margin <- diff(y_range) * 0.05
                
                window <- spatstat.geom::owin(xrange = x_range + c(-x_margin, x_margin),
                                             yrange = y_range + c(-y_margin, y_margin))
                
                ppp <- spatstat.geom::ppp(x_coords, y_coords, window = window)
                
                # Calculate nearest neighbor distances
                nnd <- spatstat.geom::nndist(ppp)
                
                # Summary statistics
                mean_nnd <- mean(nnd, na.rm = TRUE)
                median_nnd <- median(nnd, na.rm = TRUE)
                sd_nnd <- sd(nnd, na.rm = TRUE)
                
                # Clark-Evans test for spatial randomness
                ce_test <- spatstat.explore::clarkevans.test(ppp)
                
                # Add results to table
                nnd_table$addRow(rowKey="mean_nnd", values=list(
                    statistic="Mean NND",
                    value=round(mean_nnd, 4),
                    description="Mean nearest neighbor distance"
                ))
                
                nnd_table$addRow(rowKey="median_nnd", values=list(
                    statistic="Median NND",
                    value=round(median_nnd, 4),
                    description="Median nearest neighbor distance"
                ))
                
                nnd_table$addRow(rowKey="sd_nnd", values=list(
                    statistic="SD NND",
                    value=round(sd_nnd, 4),
                    description="Standard deviation of NND"
                ))
                
                nnd_table$addRow(rowKey="ce_statistic", values=list(
                    statistic="Clark-Evans R",
                    value=round(ce_test$statistic, 4),
                    description="Clark-Evans test statistic (R < 1: clustered, R > 1: dispersed)"
                ))
                
                nnd_table$addRow(rowKey="ce_pvalue", values=list(
                    statistic="Clark-Evans p-value",
                    value=round(ce_test$p.value, 4),
                    description="P-value for test of spatial randomness"
                ))
                
                # Interpretation
                interpretation <- if (ce_test$p.value < 0.05) {
                    if (ce_test$statistic < 1) "Significantly clustered" else "Significantly dispersed"
                } else {
                    "Random distribution"
                }
                
                nnd_table$addRow(rowKey="interpretation", values=list(
                    statistic="Interpretation",
                    value=interpretation,
                    description="Statistical interpretation of spatial pattern"
                ))
                
            }, error = function(e) {
                nnd_table$addRow(rowKey="error", values=list(
                    statistic="Error",
                    value=paste("Analysis failed:", e$message),
                    description=""
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
                
                # Implement Getis-Ord Gi* statistics
                private$.calculateGetisOrdGiStar(data, x_var, y_var, hotspot_table)
                
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
        
        .calculateGetisOrdGiStar = function(data, x_var, y_var, hotspot_table) {
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
                
                # Create distance matrix
                coords_matrix <- cbind(x_coords, y_coords)
                dist_matrix <- as.matrix(dist(coords_matrix))
                
                # Define distance threshold (use average nearest neighbor distance)
                nn_distances <- apply(dist_matrix + diag(Inf, n), 1, min)
                threshold_distance <- mean(nn_distances) * 2  # Use 2x average NN distance
                
                # Create spatial weights matrix (binary)
                W <- ifelse(dist_matrix <= threshold_distance & dist_matrix > 0, 1, 0)
                
                # For point pattern data, use local density as the attribute value
                # Create grid for local density calculation
                x_range <- range(x_coords, na.rm = TRUE)
                y_range <- range(y_coords, na.rm = TRUE)
                
                # Calculate local density for each point using kernel density
                local_densities <- numeric(n)
                
                for (i in 1:n) {
                    # Count neighbors within threshold distance
                    neighbors <- which(dist_matrix[i, ] <= threshold_distance & dist_matrix[i, ] > 0)
                    local_densities[i] <- length(neighbors) + 1  # Include the point itself
                }
                
                # Calculate Getis-Ord Gi* for each point
                gi_star_values <- numeric(n)
                z_scores <- numeric(n)
                
                # Global statistics
                x_bar <- mean(local_densities)
                s_squared <- var(local_densities)
                
                for (i in 1:n) {
                    # Include the point itself in Gi* calculation
                    Wi_sum <- sum(W[i, ]) + 1  # Add 1 for the point itself
                    Wij_xj_sum <- sum(W[i, ] * local_densities) + local_densities[i]  # Include point itself
                    
                    if (Wi_sum > 0) {
                        gi_star_values[i] <- Wij_xj_sum / sum(local_densities)
                        
                        # Calculate z-score for significance testing
                        if (s_squared > 0 && n > 1) {
                            numerator <- Wij_xj_sum - x_bar * Wi_sum
                            variance_gi <- s_squared * (n * Wi_sum - Wi_sum^2) / (n - 1)
                            
                            if (variance_gi > 0) {
                                z_scores[i] <- numerator / sqrt(variance_gi)
                            }
                        }
                    }
                }
                
                # Identify significant hotspots and coldspots
                alpha <- 0.05
                z_critical <- qnorm(1 - alpha/2)
                
                hotspots <- which(z_scores > z_critical)
                coldspots <- which(z_scores < -z_critical)
                
                # Summary statistics
                n_hotspots_gi <- length(hotspots)
                n_coldspots_gi <- length(coldspots)
                max_gi_star <- max(gi_star_values, na.rm = TRUE)
                min_gi_star <- min(gi_star_values, na.rm = TRUE)
                max_z_score <- max(z_scores, na.rm = TRUE)
                min_z_score <- min(z_scores, na.rm = TRUE)
                
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
                
                hotspot_table$addRow(rowKey="max_gi_star", values=list(
                    measure="Maximum Gi*",
                    value=round(max_gi_star, 4),
                    interpretation="Highest local clustering statistic"
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
                    interpretation <- paste("Detected", n_hotspots_gi, "significant hotspots and", n_coldspots_gi, "coldspots using Getis-Ord Gi* analysis")
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
                    value=paste("Calculation failed:", e$message),
                    interpretation=""
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
                    # Cross-type nearest neighbor distances
                    for (i in 1:(n_types-1)) {
                        for (j in (i+1):n_types) {
                            type1 <- unique_types[i]
                            type2 <- unique_types[j]
                            
                            # Get points of each type
                            type1_points <- which(marks_factor == type1)
                            type2_points <- which(marks_factor == type2)
                            
                            if (length(type1_points) > 0 && length(type2_points) > 0) {
                                # Calculate cross-type nearest neighbor distances
                                cross_nnd <- spatstat.geom::nncross(
                                    spatstat.geom::ppp(x_coords[type1_points], y_coords[type1_points], window = window),
                                    spatstat.geom::ppp(x_coords[type2_points], y_coords[type2_points], window = window)
                                )$dist
                                
                                mean_cross_nnd <- mean(cross_nnd, na.rm = TRUE)
                                median_cross_nnd <- median(cross_nnd, na.rm = TRUE)
                                
                                interaction_table$addRow(rowKey=paste0(type1, "_", type2, "_mean"), values=list(
                                    type1=type1,
                                    type2=type2,
                                    measure="Mean Cross-NND",
                                    value=round(mean_cross_nnd, 4),
                                    interpretation=paste("Average distance from", type1, "to nearest", type2)
                                ))
                                
                                interaction_table$addRow(rowKey=paste0(type1, "_", type2, "_median"), values=list(
                                    type1=type1,
                                    type2=type2,
                                    measure="Median Cross-NND",
                                    value=round(median_cross_nnd, 4),
                                    interpretation=paste("Median distance from", type1, "to nearest", type2)
                                ))
                            }
                        }
                    }
                }
                
            }, error = function(e) {
                interaction_table$addRow(rowKey="error", values=list(
                    type1="Error",
                    type2="",
                    measure=paste("Analysis failed:", e$message),
                    value="",
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
        }
    )
)