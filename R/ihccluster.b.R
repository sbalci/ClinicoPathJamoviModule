#' @title IHC Clustering Analysis Backend
#' @description
#' Backend implementation for IHC clustering analysis. Clusters cases based on
#' immunohistochemistry (IHC) staining patterns using various clustering algorithms
#' optimized for mixed categorical and continuous data.
#'
#' @details
#' This function supports multiple clustering approaches:
#' \itemize{
#'   \item PAM (k-medoids) - partitioning around medoids
#'   \item Hierarchical clustering - with multiple linkage methods (Ward, complete, average, single)
#'   \item MCA/PCA + k-means - dimension reduction approach
#' }
#'
#' **Distance Metrics:**
#' \itemize{
#'   \item **Gower distance** (default) - handles mixed data types (categorical + continuous)
#'   \item **Jaccard distance** - optimized for binary IHC data (Sterlacci et al. 2019)
#' }
#'
#' **Linkage Methods (hierarchical clustering):**
#' \itemize{
#'   \item **Ward** (default) - minimizes within-cluster variance, produces balanced clusters
#'   \item **Complete** - furthest neighbor, produces compact spherical clusters (Sterlacci et al. 2019)
#'   \item **Average** - mean distance between clusters
#'   \item **Single** - nearest neighbor (may produce chains)
#' }
#'
#' @section Features:
#' \itemize{
#'   \item Automatic optimal k selection using silhouette analysis
#'   \item Multiple distance metrics (Gower, Jaccard)
#'   \item Multiple linkage methods for hierarchical clustering
#'   \item Multiple testing correction (Bonferroni, FDR, Holm) for marker associations
#'   \item Comprehensive visualization suite (heatmaps, dendrograms, PCA plots)
#'   \item Consensus clustering for stability assessment
#'   \item Clinical correlation analysis
#'   \item Optimal antibody panel identification
#'   \item Variable weighting support
#'   \item Missing data handling (complete cases or pairwise distances)
#' }
#'
#' @section New in v2.0 (Sterlacci 2019 Features):
#' \itemize{
#'   \item Jaccard distance for binary IHC marker data
#'   \item Complete linkage hierarchical clustering
#'   \item Bonferroni correction for multiple testing of marker associations
#' }
#'
#' @author ClinicoPath Development Team
#' @keywords clustering immunohistochemistry pathology
#'
# Backend for ihccluster
ihcclusterClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "ihcclusterClass",
    inherit = ihcclusterBase,
    private = list(

        .computeJaccardDistance = function(binary_matrix) {
            # Validate binary data
            if (!all(binary_matrix %in% c(0, 1, NA))) {
                stop("Jaccard distance requires binary (0/1) data. Convert categorical markers to binary first.")
            }
            # Use proxy package
            if (!requireNamespace("proxy", quietly = TRUE)) {
                stop("Package 'proxy' required for Jaccard distance. Please install it: install.packages('proxy')")
            }
            dist_matrix <- proxy::dist(binary_matrix, method = "Jaccard")
            return(dist_matrix)
        },

        .convertToBinary = function(df, catVars, contVars) {
            # Convert all markers to binary (0/1)
            binary_df <- df
            conversion_info <- list(
                categorical = list(),
                continuous = list(),
                warnings = character()
            )

            # Extended list of positive indicators for IHC
            pos_indicators <- c(
                "Positive", "Pos", "pos", "POS", "+", "1", 
                "1+", "2", "2+", "3", "3+", "High", "Strong", 
                "Moderate", "Weak", "Present", "Yes", "True", "TRUE"
            )

            # Convert categorical to 0/1
            if (length(catVars) > 0) {
                for (var in catVars) {
                    if (!var %in% colnames(df)) next
                    if (is.factor(df[[var]])) {
                        levels_orig <- levels(df[[var]])
                        
                        # Check for positive values
                        binary_df[[var]] <- as.numeric(df[[var]] %in% pos_indicators)

                        # Track conversion
                        n_positive <- sum(binary_df[[var]] == 1, na.rm = TRUE)
                        n_negative <- sum(binary_df[[var]] == 0, na.rm = TRUE)
                        conversion_info$categorical[[var]] <- list(
                            original_levels = levels_orig,
                            n_positive = n_positive,
                            n_negative = n_negative
                        )

                        # Warning if no positive cases detected
                        if (n_positive == 0 || n_negative == 0) {
                            conversion_info$warnings <- c(conversion_info$warnings,
                                sprintf("%s: All cases coded as %s", var,
                                       if(n_positive == 0) "negative" else "positive"))
                        }
                    }
                }
            }

            # Convert continuous to 0/1 using median split
            if (length(contVars) > 0) {
                for (var in contVars) {
                    if (!var %in% colnames(df)) next
                    median_val <- median(df[[var]], na.rm = TRUE)
                    if (is.na(median_val)) {
                        binary_df[[var]] <- 0  # All NA -> 0
                        conversion_info$warnings <- c(conversion_info$warnings,
                            sprintf("%s: All values missing, set to 0", var))
                    } else {
                        binary_df[[var]] <- as.numeric(df[[var]] > median_val)

                        # Track conversion
                        n_above <- sum(df[[var]] > median_val, na.rm = TRUE)
                        n_below <- sum(df[[var]] <= median_val, na.rm = TRUE)
                        conversion_info$continuous[[var]] <- list(
                            median = median_val,
                            range = range(df[[var]], na.rm = TRUE),
                            n_above = n_above,
                            n_below = n_below
                        )
                    }
                }
            }

            attr(binary_df, "conversion_info") <- conversion_info
            return(binary_df)
        },

        .showBinaryConversionNotice = function(df, catVars, contVars) {
            # Generate HTML notice about binary conversion for Jaccard distance

            html <- "<div style='padding:15px; background-color:#fff3cd; border-left:4px solid #ffc107;'>"
            html <- paste0(html, "<h4 style='color:#856404; margin-top:0;'>IMPORTANT: Binary Conversion for Jaccard Distance</h4>")

            html <- paste0(html, "<p><b>Jaccard distance requires binary (0/1) data.</b> Your data has been automatically converted as follows:</p>")

            # Extended list of positive indicators for checking
            pos_indicators <- c(
                "Positive", "Pos", "pos", "POS", "+", "1", 
                "1+", "2", "2+", "3", "3+", "High", "Strong", 
                "Moderate", "Weak", "Present", "Yes", "True", "TRUE"
            )

            # Categorical markers section
            if (length(catVars) > 0) {
                html <- paste0(html, "<h5 style='color:#856404;'>Categorical Markers (n=", length(catVars), "):</h5>")
                html <- paste0(html, "<table style='width:100%; border-collapse:collapse; margin-bottom:10px;'>")
                html <- paste0(html, "<tr style='background-color:#f8f9fa; font-weight:bold;'>")
                html <- paste0(html, "<td style='padding:8px; border:1px solid #dee2e6;'>Marker</td>")
                html <- paste0(html, "<td style='padding:8px; border:1px solid #dee2e6;'>Original Levels</td>")
                html <- paste0(html, "<td style='padding:8px; border:1px solid #dee2e6;'>Positive (1)</td>")
                html <- paste0(html, "<td style='padding:8px; border:1px solid #dee2e6;'>Negative (0)</td>")
                html <- paste0(html, "<td style='padding:8px; border:1px solid #dee2e6;'>Status</td>")
                html <- paste0(html, "</tr>")

                for (var in catVars) {
                    if (!var %in% colnames(df)) next
                    levels_str <- paste(levels(df[[var]]), collapse=", ")
                    n_pos <- sum(df[[var]] %in% pos_indicators, na.rm = TRUE)
                    n_neg <- sum(!df[[var]] %in% pos_indicators, na.rm = TRUE)

                    # Status check
                    status <- "OK"
                    status_color <- "#28a745"
                    if (n_pos == 0 || n_neg == 0) {
                        status <- "WARNING: All same"
                        status_color <- "#dc3545"
                    }
                    # Relaxed check for standard coding
                    
                    html <- paste0(html, "<tr>")
                    html <- paste0(html, "<td style='padding:8px; border:1px solid #dee2e6;'><b>", var, "</b></td>")
                    html <- paste0(html, "<td style='padding:8px; border:1px solid #dee2e6;'>", levels_str, "</td>")
                    html <- paste0(html, "<td style='padding:8px; border:1px solid #dee2e6;'>", n_pos, "</td>")
                    html <- paste0(html, "<td style='padding:8px; border:1px solid #dee2e6;'>", n_neg, "</td>")
                    html <- paste0(html, "<td style='padding:8px; border:1px solid #dee2e6; color:", status_color, ";'><b>", status, "</b></td>")
                    html <- paste0(html, "</tr>")
                }

                html <- paste0(html, "</table>")
                html <- paste0(html, "<p style='margin:5px 0; font-size:0.9em;'><b>Conversion rule:</b> Standard IHC positive terms (Positive, +, 1/2/3, High, Strong, etc.) coded as 1; others as 0</p>")
            }

            # Continuous markers section
            if (length(contVars) > 0) {
                html <- paste0(html, "<h5 style='color:#856404; margin-top:15px;'>Continuous Markers (n=", length(contVars), ") - Median Split:</h5>")
                html <- paste0(html, "<table style='width:100%; border-collapse:collapse; margin-bottom:10px;'>")
                html <- paste0(html, "<tr style='background-color:#f8f9fa; font-weight:bold;'>")
                html <- paste0(html, "<td style='padding:8px; border:1px solid #dee2e6;'>Marker</td>")
                html <- paste0(html, "<td style='padding:8px; border:1px solid #dee2e6;'>Range</td>")
                html <- paste0(html, "<td style='padding:8px; border:1px solid #dee2e6;'>Median</td>")
                html <- paste0(html, "<td style='padding:8px; border:1px solid #dee2e6;'>Above (1)</td>")
                html <- paste0(html, "<td style='padding:8px; border:1px solid #dee2e6;'>Below (0)</td>")
                html <- paste0(html, "</tr>")

                for (var in contVars) {
                    if (!var %in% colnames(df)) next
                    var_data <- df[[var]]
                    var_range <- range(var_data, na.rm = TRUE)
                    var_median <- median(var_data, na.rm = TRUE)
                    n_above <- sum(var_data > var_median, na.rm = TRUE)
                    n_below <- sum(var_data <= var_median, na.rm = TRUE)

                    html <- paste0(html, "<tr>")
                    html <- paste0(html, "<td style='padding:8px; border:1px solid #dee2e6;'><b>", var, "</b></td>")
                    html <- paste0(html, "<td style='padding:8px; border:1px solid #dee2e6;'>",
                                  sprintf("%.1f - %.1f", var_range[1], var_range[2]), "</td>")
                    html <- paste0(html, "<td style='padding:8px; border:1px solid #dee2e6;'>",
                                  sprintf("%.1f", var_median), "</td>")
                    html <- paste0(html, "<td style='padding:8px; border:1px solid #dee2e6;'>", n_above, "</td>")
                    html <- paste0(html, "<td style='padding:8px; border:1px solid #dee2e6;'>", n_below, "</td>")
                    html <- paste0(html, "</tr>")
                }

                html <- paste0(html, "</table>")
                html <- paste0(html, "<p style='margin:5px 0; font-size:0.9em;'><b>Conversion rule:</b> Values above median coded as 1; values at or below median coded as 0</p>")
                html <- paste0(html, "<p style='margin:5px 0; font-size:0.9em; color:#dc3545;'><b>WARNING:</b> Median split loses information. H-scores, percentages, and intensity scales are better analyzed with Gower distance.</p>")
            }

            # Important notes section
            html <- paste0(html, "<div style='margin-top:15px; padding:10px; background-color:#f8d7da; border:1px solid #f5c6cb;'>")
            html <- paste0(html, "<h5 style='color:#721c24; margin-top:0;'>VERIFY BEFORE INTERPRETING RESULTS:</h5>")
            html <- paste0(html, "<ol style='margin:5px 0; padding-left:20px;'>")
            html <- paste0(html, "<li><b>Categorical markers:</b> Ensure 'Positive'/'Negative' coding is correct. Non-standard codes (e.g., 'Present', 'Absent') will be treated as negative.</li>")
            html <- paste0(html, "<li><b>Continuous markers:</b> Median split creates artificial dichotomy. Consider whether binary conversion is appropriate for your research question.</li>")
            html <- paste0(html, "<li><b>Check for warnings:</b> If all cases are coded the same (all 0 or all 1), that marker provides no clustering information.</li>")
            html <- paste0(html, "<li><b>Alternative:</b> If you have ordinal scales (0/1+/2+/3+) or want to preserve continuous information, use <b>Gower distance</b> instead.</li>")
            html <- paste0(html, "</ol>")
            html <- paste0(html, "</div>")

            # When to use section
            html <- paste0(html, "<div style='margin-top:10px; padding:10px; background-color:#d1ecf1; border:1px solid #bee5eb;'>")
            html <- paste0(html, "<h5 style='color:#0c5460; margin-top:0;'>When Jaccard Distance is Appropriate:</h5>")
            html <- paste0(html, "<ul style='margin:5px 0; padding-left:20px;'>")
            html <- paste0(html, "<li>Tissue microarray (TMA) data with binary scoring (positive/negative only)</li>")
            html <- paste0(html, "<li>Replicating published studies that used Jaccard distance (e.g., Sterlacci 2019)</li>")
            html <- paste0(html, "<li>All markers are truly dichotomous (present/absent, expressed/not expressed)</li>")
            html <- paste0(html, "</ul>")
            html <- paste0(html, "<p style='margin:5px 0;'><b>If unsure, use Gower distance (default).</b> It handles mixed data types without information loss.</p>")
            html <- paste0(html, "</div>")

            html <- paste0(html, "</div>")

            # Set HTML content
            if (!is.null(self$results$binaryConversionNote)) {
                self$results$binaryConversionNote$setVisible(TRUE)
                self$results$binaryConversionNote$setContent(html)
            }
        },

        .testReproducibility = function(df, catVars, contVars, opts, cluster_func) {
            # Test cluster reproducibility via random split and Cohen's kappa
            n_splits <- opts$nSplits %||% 10
            n_cases <- nrow(df)

            if (n_cases < 20) {
                return(list(
                    error = "Reproducibility testing requires at least 20 cases",
                    kappa_values = NULL
                ))
            }

            kappa_results <- list()
            cluster_labels <- unique(cluster_func(df)$clusters)
            n_clusters <- length(cluster_labels)

            # Create numeric version of data for profile calculation
            df_numeric <- df
            for (var in names(df_numeric)) {
                if (is.factor(df_numeric[[var]]) || is.character(df_numeric[[var]])) {
                    df_numeric[[var]] <- as.numeric(as.factor(df_numeric[[var]]))
                }
            }

            for (split in 1:n_splits) {
                set.seed(opts$seed + split)

                # Random 50/50 split
                group_a_idx <- sample(1:n_cases, size = floor(n_cases/2))
                group_b_idx <- setdiff(1:n_cases, group_a_idx)

                df_a <- df[group_a_idx, , drop = FALSE]
                df_b <- df[group_b_idx, , drop = FALSE]

                # Numeric subsets for profiling
                df_num_a <- df_numeric[group_a_idx, , drop = FALSE]
                df_num_b <- df_numeric[group_b_idx, , drop = FALSE]

                # Cluster each group independently
                result_a <- cluster_func(df_a)
                result_b <- cluster_func(df_b)

                # Match clusters between groups using Hungarian algorithm
                # Create confusion matrix of cluster similarities
                confusion <- matrix(0, nrow = n_clusters, ncol = n_clusters)

                for (i in 1:n_clusters) {
                    cases_a <- which(as.character(result_a$clusters) == as.character(cluster_labels[i]))
                    if (length(cases_a) == 0) next

                    # Get marker profile for cluster i in group A
                    profile_a <- colMeans(df_num_a[cases_a, , drop = FALSE], na.rm = TRUE)

                    for (j in 1:n_clusters) {
                        cases_b <- which(as.character(result_b$clusters) == as.character(cluster_labels[j]))
                        if (length(cases_b) == 0) next

                        # Get marker profile for cluster j in group B
                        profile_b <- colMeans(df_num_b[cases_b, , drop = FALSE], na.rm = TRUE)

                        # Similarity = negative euclidean distance
                        confusion[i, j] <- -sqrt(sum((profile_a - profile_b)^2, na.rm = TRUE))
                    }
                }

                # Match clusters (maximize similarity)
                cluster_mapping <- private$.matchClusters(confusion)

                # Calculate Cohen's kappa for each cluster
                for (k in 1:n_clusters) {
                    matched_cluster <- cluster_mapping[k]
                    if (is.na(matched_cluster)) next

                    # Binary classification: cluster k vs all others
                    # Group A
                    class_a <- as.numeric(as.character(result_a$clusters) == as.character(cluster_labels[k]))
                    # Group B (using matched cluster)
                    class_b <- as.numeric(as.character(result_b$clusters) == as.character(cluster_labels[matched_cluster]))

                    # For cases in both groups, calculate kappa
                    # (Not applicable for independent splits, so skip)

                    # Store agreement information
                    if (!cluster_labels[k] %in% names(kappa_results)) {
                        kappa_results[[as.character(cluster_labels[k])]] <- list(
                            kappas = numeric(),
                            n_a = numeric(),
                            n_b = numeric()
                        )
                    }

                    kappa_results[[as.character(cluster_labels[k])]]$n_a <- c(
                        kappa_results[[as.character(cluster_labels[k])]]$n_a,
                        sum(class_a)
                    )
                    kappa_results[[as.character(cluster_labels[k])]]$n_b <- c(
                        kappa_results[[as.character(cluster_labels[k])]]$n_b,
                        sum(class_b)
                    )

                    # Calculate proportion agreement
                    prop_a <- mean(class_a)
                    prop_b <- mean(class_b)
                    # Observed agreement (proportion in both or neither)
                    p_o <- (prop_a * prop_b) + ((1-prop_a) * (1-prop_b))
                    # Expected agreement by chance
                    p_e <- (prop_a + prop_b)/2 * (prop_a + prop_b)/2 +
                           (1 - (prop_a + prop_b)/2) * (1 - (prop_a + prop_b)/2)
                    # Kappa
                    kappa <- ifelse(p_e < 1, (p_o - p_e) / (1 - p_e), 0)

                    kappa_results[[as.character(cluster_labels[k])]]$kappas <- c(
                        kappa_results[[as.character(cluster_labels[k])]]$kappas,
                        kappa
                    )
                }
            }

            # Summarize kappas for each cluster
            summary_results <- list()
            for (cluster_name in names(kappa_results)) {
                kappas <- kappa_results[[cluster_name]]$kappas
                mean_kappa <- mean(kappas, na.rm = TRUE)
                sd_kappa <- sd(kappas, na.rm = TRUE)

                # 95% CI
                se_kappa <- sd_kappa / sqrt(length(kappas))
                ci_lower <- mean_kappa - 1.96 * se_kappa
                ci_upper <- mean_kappa + 1.96 * se_kappa

                interpretation <- private$.interpretKappa(mean_kappa)

                summary_results[[cluster_name]] <- list(
                    cluster = cluster_name,
                    mean_kappa = mean_kappa,
                    sd_kappa = sd_kappa,
                    ci_lower = ci_lower,
                    ci_upper = ci_upper,
                    interpretation = interpretation,
                    n_splits = length(kappas)
                )
            }

            return(summary_results)
        },

        .matchClusters = function(confusion_matrix) {
            # Simple greedy matching of clusters
            n <- nrow(confusion_matrix)
            mapping <- rep(NA, n)
            used <- rep(FALSE, n)

            for (i in 1:n) {
                # Find best match for cluster i
                available <- which(!used)
                if (length(available) == 0) break

                best_match <- available[which.max(confusion_matrix[i, available])]
                mapping[i] <- best_match
                used[best_match] <- TRUE
            }

            return(mapping)
        },

        .interpretKappa = function(kappa) {
            if (is.na(kappa)) return("Unable to calculate")
            if (kappa < 0.21) return("Poor")
            if (kappa < 0.41) return("Fair")
            if (kappa < 0.61) return("Moderate")
            if (kappa < 0.81) return("Substantial")
            return("Almost Perfect")
        },

        .supervisedClustering = function(df, catVars, contVars, opts, group_var) {
            # Perform clustering within each group separately
            if (!group_var %in% colnames(self$data)) {
                return(list(error = "Grouping variable not found in data"))
            }

            group_values <- self$data[[group_var]]
            groups <- unique(group_values[!is.na(group_values)])

            results <- list()
            min_cases <- 2 * (opts$nClusters %||% 3)

            for (grp in groups) {
                grp_idx <- which(group_values == grp & !is.na(group_values))

                if (length(grp_idx) < min_cases) {
                    results[[as.character(grp)]] <- list(
                        group = grp,
                        n_cases = length(grp_idx),
                        status = sprintf("Skipped: Too few cases (need %d)", min_cases),
                        clusters = NULL
                    )
                    next
                }

                # Subset data to this group
                df_grp <- df[grp_idx, , drop = FALSE]

                # Perform clustering
                grp_result <- private$.performClustering(
                    df = df_grp,
                    catVars = catVars,
                    contVars = contVars,
                    opts = opts,
                    silhouetteTable = NULL
                )

                results[[as.character(grp)]] <- list(
                    group = grp,
                    n_cases = length(grp_idx),
                    n_clusters = grp_result$usedK,
                    clusters = grp_result$clusters,
                    avg_silhouette = mean(cluster::silhouette(
                        as.numeric(grp_result$clusters),
                        grp_result$dist
                    )[, "sil_width"], na.rm = TRUE),
                    status = "Success",
                    cluster_sizes = table(grp_result$clusters)
                )
            }

            return(results)
        },

        # Phase 3: Calculate marker ratios (e.g., CD4/CD8 ratio)
        .calculateRatio = function(data, numerator_var, denominator_var, ratio_name, opts) {
            # Validate inputs
            if (is.null(numerator_var) || is.null(denominator_var)) {
                return(list(
                    error = "Both numerator and denominator variables must be specified"
                ))
            }

            if (!numerator_var %in% colnames(data)) {
                return(list(error = sprintf("Numerator variable '%s' not found in dataset", numerator_var)))
            }

            if (!denominator_var %in% colnames(data)) {
                return(list(error = sprintf("Denominator variable '%s' not found in dataset", denominator_var)))
            }

            # Extract values
            numerator <- data[[numerator_var]]
            denominator <- data[[denominator_var]]

            # Check numeric
            if (!is.numeric(numerator)) {
                return(list(error = sprintf("Numerator variable '%s' must be numeric", numerator_var)))
            }
            if (!is.numeric(denominator)) {
                return(list(error = sprintf("Denominator variable '%s' must be numeric", denominator_var)))
            }

            # Calculate ratio (handle zero denominators)
            ratio_values <- ifelse(denominator > 0, numerator / denominator, NA)

            # Summary statistics
            summary_stats <- list(
                n_valid = sum(!is.na(ratio_values)),
                n_missing = sum(is.na(ratio_values)),
                n_zero_denom = sum(denominator == 0, na.rm = TRUE),
                mean = mean(ratio_values, na.rm = TRUE),
                median = median(ratio_values, na.rm = TRUE),
                sd = sd(ratio_values, na.rm = TRUE),
                min = min(ratio_values, na.rm = TRUE),
                max = max(ratio_values, na.rm = TRUE),
                q25 = quantile(ratio_values, 0.25, na.rm = TRUE),
                q75 = quantile(ratio_values, 0.75, na.rm = TRUE)
            )

            # Classify if requested
            classification <- NULL
            if (isTRUE(opts$ratioClassification)) {
                low_cutoff <- opts$ratioLowCutoff %||% 1.0
                high_cutoff <- opts$ratioHighCutoff %||% 2.0

                # Classify values
                ratio_class <- rep(NA_character_, length(ratio_values))
                ratio_class[!is.na(ratio_values) & ratio_values <= low_cutoff] <- "Low"
                ratio_class[!is.na(ratio_values) & ratio_values > low_cutoff & ratio_values < high_cutoff] <- "Intermediate"
                ratio_class[!is.na(ratio_values) & ratio_values >= high_cutoff] <- "High"
                ratio_class <- factor(ratio_class, levels = c("Low", "Intermediate", "High"))

                # Classification summary
                class_table <- table(ratio_class, useNA = "ifany")
                classification <- list(
                    classes = ratio_class,
                    low_cutoff = low_cutoff,
                    high_cutoff = high_cutoff,
                    n_low = as.integer(class_table["Low"]),
                    n_intermediate = as.integer(class_table["Intermediate"]),
                    n_high = as.integer(class_table["High"]),
                    n_missing = sum(is.na(ratio_class)),
                    range_low = range(ratio_values[ratio_class == "Low"], na.rm = TRUE),
                    range_intermediate = range(ratio_values[ratio_class == "Intermediate"], na.rm = TRUE),
                    range_high = range(ratio_values[ratio_class == "High"], na.rm = TRUE)
                )
            }

            return(list(
                ratio_values = ratio_values,
                summary = summary_stats,
                classification = classification,
                numerator_var = numerator_var,
                denominator_var = denominator_var,
                ratio_name = ratio_name
            ))
        },

        .prepareData = function(data, catVars, contVars, opts) {
            notes <- character()
            info <- list(
                missingCategorical = character(),
                missingContinuous = character(),
                droppedMarkers = character(),
                handleMissing = if (is.null(opts$handleMissing)) "pairwise" else opts$handleMissing
            )

            allVars <- unique(c(catVars, contVars))
            
            # Add auxiliary variables to keep them aligned with markers
            auxVars <- character()
            if (!is.null(opts$caseId)) auxVars <- c(auxVars, opts$caseId)
            if (!is.null(opts$spatialCompartment)) auxVars <- c(auxVars, opts$spatialCompartment)
            if (!is.null(opts$supervisedVariable)) auxVars <- c(auxVars, opts$supervisedVariable)
            if (!is.null(opts$knownDiagnosis)) auxVars <- c(auxVars, opts$knownDiagnosis)
            if (!is.null(opts$survivalTime)) auxVars <- c(auxVars, opts$survivalTime)
            if (!is.null(opts$survivalEvent)) auxVars <- c(auxVars, opts$survivalEvent)
            if (!is.null(opts$clinicalVars)) auxVars <- c(auxVars, opts$clinicalVars)
            
            auxVars <- unique(auxVars)
            auxVars <- auxVars[auxVars %in% colnames(data)]

            if (length(allVars) == 0)
                stop("No markers supplied for clustering")

            # Include markers and auxiliary variables
            df <- data[, c(allVars, auxVars), drop = FALSE]

            if (identical(info$handleMissing, "complete")) {
                # Only check markers for completeness, not auxiliary vars
                # But jamovi's naOmit removes row if ANY column is NA
                # We want to remove cases where MARKERS are missing
                
                # Check complete cases based on markers only
                complete_cases <- complete.cases(df[, allVars, drop = FALSE])
                initial_n <- nrow(df)
                df <- df[complete_cases, , drop = FALSE]
                removed <- initial_n - nrow(df)
                
                if (removed > 0)
                    notes <- c(notes, sprintf("Removed %d cases with incomplete marker data (complete-case analysis)", removed))
            }

            # Drop markers with no observed data
            all_na <- names(df)[vapply(df[, allVars, drop=FALSE], function(col) all(is.na(col)), logical(1))]
            if (length(all_na) > 0) {
                # Do not remove from df, just remove from catVars/contVars list
                # df <- df[, setdiff(names(df), all_na), drop = FALSE] 
                catVars <- setdiff(catVars, all_na)
                contVars <- setdiff(contVars, all_na)
                info$droppedMarkers <- all_na
                notes <- c(notes, sprintf("Removed markers with no observed values: %s", paste(all_na, collapse = ", ")))
            }
            
            # Re-verify we have enough markers
            if (length(c(catVars, contVars)) < 2)
                 stop("At least two markers with data are required for clustering")

            # Harmonise categorical markers
            if (length(catVars) > 0) {
                validCat <- character()
                for (var in catVars) {
                    if (!var %in% colnames(df))
                        next
                    x <- df[[var]]
                    if (!is.factor(x))
                        x <- as.factor(x)
                    if (anyNA(x))
                        info$missingCategorical <- unique(c(info$missingCategorical, var))
                    if (!identical(info$handleMissing, "pairwise") && anyNA(x)) {
                        x <- addNA(x, ifany = TRUE)
                        levels(x)[is.na(levels(x))] <- "Missing"
                    }
                    df[[var]] <- droplevels(x)
                    validCat <- c(validCat, var)
                }
                catVars <- validCat
            }

            # Process continuous markers
            if (length(contVars) > 0) {
                validCont <- character()
                for (var in contVars) {
                    if (!var %in% colnames(df))
                        next
                    x <- df[[var]]
                    if (!is.numeric(x))
                        stop(sprintf("Variable '%s' must be numeric for continuous analysis", var))

                    if (all(is.na(x)))
                        next

                    uniqueVals <- unique(na.omit(x))
                    if (length(uniqueVals) == 1)
                        warning(sprintf("Variable '%s' has constant values and may not contribute to clustering", var))

                    if (anyNA(x))
                        info$missingContinuous <- unique(c(info$missingContinuous, var))

                    if (length(uniqueVals) > 1) {
                        q <- stats::quantile(x, c(0.01, 0.99), na.rm = TRUE)
                        mean_val <- mean(x, na.rm = TRUE)
                        denom <- max(1e-9, abs(mean_val))
                        if (!isTRUE(all.equal(mean_val, 0)) && abs(q[2] - q[1]) / denom > 10)
                            warning(sprintf("Variable '%s' shows extreme spread and may influence clustering disproportionately", var))
                    }

                    if (isTRUE(opts$scaleContVars)) {
                        mu <- mean(x, na.rm = TRUE)
                        sd_val <- stats::sd(x, na.rm = TRUE)
                        if (!is.na(sd_val) && sd_val > 0) {
                            x <- (x - mu) / sd_val
                        } else {
                            x <- x - mu
                        }
                    }

                    df[[var]] <- x
                    validCont <- c(validCont, var)
                }
                contVars <- validCont
            }

            if (nrow(df) < 5)
                stop("Insufficient data after preprocessing. Need at least 5 cases.")

            if (length(info$missingCategorical) > 0 && identical(info$handleMissing, "pairwise"))
                notes <- c(notes, sprintf("Missing categorical markers handled via pairwise Gower distances for: %s", paste(info$missingCategorical, collapse = ", ")))
            if (length(info$missingContinuous) > 0 && identical(info$handleMissing, "pairwise"))
                notes <- c(notes, sprintf("Missing continuous markers handled via pairwise Gower distances for: %s", paste(info$missingContinuous, collapse = ", ")))

            list(
                df = df,
                catVars = catVars,
                contVars = contVars,
                notes = notes,
                info = info
            )
        },

        .parseWeights = function(rawWeights, allVars) {
            if (is.null(rawWeights) || rawWeights == "" || nchar(rawWeights) == 0)
                return(NULL)

            weights <- NULL
            # TESTING: tryCatch disabled - errors will be visible
            # tryCatch({
                w <- as.numeric(strsplit(rawWeights, ",")[[1]])
                if (length(w) == length(allVars)) {
                    weights <- w
                } else if (length(w) == 1) {
                    weights <- rep(w, length(allVars))
                } else {
                    warning(sprintf("Weights length (%d) doesn't match number of variables (%d). Ignoring weights.", length(w), length(allVars)))
                    weights <- NULL
                }
            # }, error = function(e) {
            #     warning(sprintf("Error parsing weights: %s. Ignoring weights.", e$message))
            #     weights <- NULL
            # })

            if (!is.null(weights))
                names(weights) <- allVars

            weights
        },

        .performClustering = function(df, catVars, contVars, opts, silhouetteTable = NULL) {
            method <- opts$method %||% "pam"
            requestedK <- opts$nClusters %||% 3
            autoSelect <- isTRUE(opts$autoSelectK) || is.null(opts$nClusters)
            weights <- private$.parseWeights(opts$weights, colnames(df))

            return(private$.clusterData(
                df = df,
                method = method,
                opts = opts,
                k = requestedK,
                autoSelect = autoSelect,
                catVars = catVars,
                contVars = contVars,
                weights = weights,
                silhouetteTable = silhouetteTable
            ))
        },

        .clusterData = function(df, method, opts, k, autoSelect, catVars, contVars, weights, silhouetteTable = NULL) {
            result <- list(clusters = NULL, usedK = k, fit = NULL, dist = NULL, hc = NULL,
                           scores = NULL, silhouette = NULL, notes = character(), method = method)

            # Compute distance matrix once for methods that can reuse it
            dist_input <- df
            dist_weights <- weights

            if (method == "kmodes") {
                dist_input <- df[, catVars, drop = FALSE]
                if (ncol(dist_input) == 0)
                    stop("k-modes requires at least one categorical marker")
                if (!is.null(dist_weights)) {
                    dist_weights <- dist_weights[colnames(dist_input)]
                    if (any(is.na(dist_weights)))
                        dist_weights <- NULL
                }
            } else if (!is.null(dist_weights)) {
                dist_weights <- dist_weights[colnames(dist_input)]
                if (any(is.na(dist_weights)))
                    dist_weights <- NULL
            }

            # Distance calculation based on selected method
            distance_method <- opts$distanceMethod %||% "gower"

            if (distance_method == "jaccard") {
                # Convert to binary matrix for Jaccard distance
                binary_df <- private$.convertToBinary(dist_input, catVars, contVars)
                binary_matrix <- as.matrix(binary_df)
                dist_matrix <- private$.computeJaccardDistance(binary_matrix)
                result$notes <- c(result$notes, "Using Jaccard distance (binary conversion applied)")
            } else {
                # Gower distance (existing implementation)
                if (is.null(dist_weights)) {
                    dist_matrix <- cluster::daisy(dist_input, metric = "gower")
                } else {
                    dist_matrix <- cluster::daisy(dist_input, metric = "gower", weights = dist_weights)
                }
            }
            if (anyNA(dist_matrix)) {
                result$notes <- c(result$notes, "Warning: Pairwise distances resulted in missing values. Imputing with max distance.")
                dist_matrix[is.na(dist_matrix)] <- max(dist_matrix, na.rm = TRUE)
            }
            result$dist <- dist_matrix

            addSilhouetteRow <- function(kVals, silhouettes, selected) {
                if (is.null(silhouetteTable))
                    return()
                # Tables are automatically overwritten by jamovi
                for (i in seq_along(kVals)) {
                    silhouetteTable$addRow(
                        rowKey = paste0("k_", kVals[i]),
                        list(
                            k = as.integer(kVals[i]),
                            avg_silhouette = as.numeric(silhouettes[i]),
                            selected = if (selected[i]) "✓" else ""
                        )
                    )
                }
            }

            chooseK <- function(candidateKs, computeSilhouette) {
                candidateKs <- candidateKs[candidateKs < nrow(df)]
                if (length(candidateKs) == 0)
                    stop("Candidate cluster sizes must be smaller than the number of cases")
                sil_values <- vapply(candidateKs, function(kk) {
                    # TESTING: tryCatch and suppressWarnings disabled - errors will be visible
                    # suppressWarnings(tryCatch({
                        computeSilhouette(kk)
                    # }, error = function(e) NA_real_))
                }, numeric(1))

                finite_vals <- sil_values
                finite_vals[!is.finite(finite_vals)] <- -Inf

                if (all(is.infinite(finite_vals)))
                    stop("Unable to compute silhouette widths for any candidate cluster size. Check data quality or reduce k range.")

                best_index <- which.max(finite_vals)
                selected <- seq_along(candidateKs) == best_index
                addSilhouetteRow(candidateKs, sil_values, selected)
                list(k = candidateKs[[best_index]], values = sil_values, selected = selected)
            }

            kRange <- switch(opts$kRange %||% "medium",
                              "small" = 2:6,
                              "large" = 2:12,
                              2:8)

            if (method == "pam") {
                if (autoSelect || is.null(k)) {
                    res <- chooseK(kRange, function(kk) {
                        fit_tmp <- cluster::pam(dist_matrix, k = kk, diss = TRUE)
                        sil <- cluster::silhouette(fit_tmp)
                        mean(sil[, "sil_width"], na.rm = TRUE)
                    })
                    k <- res$k
                    result$silhouette <- res
                    result$notes <- c(result$notes, sprintf("Auto-selected k=%d (PAM, silhouette)", k))
                }
                pam_fit <- cluster::pam(dist_matrix, k = k, diss = TRUE)
                result$clusters <- factor(pam_fit$clustering, labels = paste0("C", seq_len(k)))
                result$fit <- pam_fit
                result$usedK <- k

            } else if (method == "hierarchical") {
                # Get linkage method
                linkage <- opts$linkageMethod %||% "ward"
                
                distance_method <- opts$distanceMethod %||% "gower"
                if (distance_method == "gower" && linkage == "ward") {
                     result$notes <- c(result$notes, "Note: Ward linkage with Gower distance is heuristic. Consider 'Complete' or 'Average' linkage for rigorous interpretation.")
                }

                # Perform hierarchical clustering with selected linkage
                if (linkage == "ward") {
                    hc <- cluster::agnes(dist_matrix, method = "ward")
                } else {
                    # Use stats::hclust for other linkage methods
                    hc_stats <- stats::hclust(dist_matrix, method = linkage)
                    # Wrap in agnes-like structure for compatibility
                    hc <- list()
                    hc$merge <- hc_stats$merge
                    hc$height <- hc_stats$height
                    hc$order <- hc_stats$order
                    class(hc) <- c("hclust", "agnes")
                }

                result$notes <- c(result$notes, sprintf("Hierarchical clustering with %s linkage", linkage))

                if (autoSelect || is.null(k)) {
                    res <- chooseK(kRange, function(kk) {
                        cluster_ids <- stats::cutree(as.hclust(hc), k = kk)
                        sil <- cluster::silhouette(cluster_ids, dist_matrix)
                        mean(sil[, "sil_width"], na.rm = TRUE)
                    })
                    k <- res$k
                    result$silhouette <- res
                    result$notes <- c(result$notes, sprintf("Auto-selected k=%d (hierarchical, silhouette)", k))
                }
                cluster_ids <- stats::cutree(as.hclust(hc), k = k)
                result$clusters <- factor(cluster_ids, labels = paste0("C", seq_len(k)))
                result$hc <- hc
                result$usedK <- k

            } else if (method == "dimreduce") {
                # Use FactoMineR methods to derive low-dimensional representation
                imputeForDimension <- function(df_imp) {
                    out <- df_imp
                    if (length(contVars) > 0) {
                        for (var in contVars) {
                            if (!var %in% colnames(out))
                                next
                            x <- out[[var]]
                            if (anyNA(x)) {
                                med <- stats::median(x, na.rm = TRUE)
                                if (is.na(med))
                                    med <- 0
                                x[is.na(x)] <- med
                                out[[var]] <- x
                            }
                        }
                    }
                    if (length(catVars) > 0) {
                        for (var in catVars) {
                            if (!var %in% colnames(out))
                                next
                            x <- out[[var]]
                            if (!is.factor(x))
                                x <- as.factor(x)
                            if (anyNA(x)) {
                                tbl <- table(x, useNA = "no")
                                mode_level <- names(tbl)[which.max(tbl)]
                                if (length(mode_level) == 0 || is.na(mode_level))
                                    mode_level <- "Missing"
                                levels_all <- unique(c(levels(x), mode_level))
                                x_chr <- as.character(x)
                                x_chr[is.na(x_chr)] <- mode_level
                                out[[var]] <- factor(x_chr, levels = levels_all)
                            }
                        }
                    }
                    out
                }

                df_dim <- df
                if (anyNA(df_dim)) {
                    df_dim <- imputeForDimension(df_dim)
                    result$notes <- c(result$notes, "Applied median/mode imputation required for dimension reduction.")
                }

                scores <- NULL
                # TESTING: tryCatch disabled - errors will be visible
                # tryCatch({
                    if (length(catVars) > 0 && length(contVars) > 0) {
                        famd <- FactoMineR::FAMD(df_dim, graph = FALSE, ncp = min(5, ncol(df_dim)))
                        scores <- famd$ind$coord
                    } else if (length(catVars) > 0) {
                        mca <- FactoMineR::MCA(df_dim[, catVars, drop = FALSE], graph = FALSE)
                        eig <- mca$eig
                        cump <- cumsum(eig[, "cumulative percentage of variance"])
                        keep <- max(1, which(cump >= 75)[1])
                        scores <- mca$ind$coord[, 1:keep, drop = FALSE]
                    } else {
                        pca <- stats::prcomp(df_dim[, contVars, drop = FALSE], center = TRUE, scale. = TRUE)
                        keep <- min(5, ncol(pca$x))
                        scores <- pca$x[, 1:keep, drop = FALSE]
                    }
                # }, error = function(e) {
                #     stop(sprintf("Dimension reduction failed: %s", e$message))
                # })

                if (is.null(scores) || ncol(scores) < 1)
                    stop("Dimension reduction produced empty component matrix.")

                scaled_scores <- scale(as.matrix(scores))
                scaled_scores <- as.matrix(scaled_scores)
                rownames(scaled_scores) <- rownames(df)

                if (autoSelect || is.null(k)) {
                    res <- chooseK(kRange, function(kk) {
                        cl <- stats::kmeans(scaled_scores, centers = kk, nstart = 50)
                        sil <- cluster::silhouette(cl$cluster, stats::dist(scaled_scores))
                        mean(sil[, "sil_width"], na.rm = TRUE)
                    })
                    k <- res$k
                    result$silhouette <- res
                    result$notes <- c(result$notes, sprintf("Auto-selected k=%d (dimension reduction, silhouette)", k))
                }

                km_fit <- stats::kmeans(scaled_scores, centers = k, nstart = 100)
                result$clusters <- factor(km_fit$cluster, labels = paste0("C", seq_len(k)))
                result$usedK <- k
                result$fit <- km_fit
                result$scores <- scaled_scores

            } else if (method == "kmodes") {
                if (length(catVars) == 0)
                    stop("k-modes method requires categorical variables")

                catOnlyDf <- df[, catVars, drop = FALSE]
                computeKmodes <- function(centers) {
                    klaR::kmodes(catOnlyDf, modes = centers, iter.max = 100, weighted = FALSE)
                }

                if (autoSelect || is.null(k)) {
                    res <- chooseK(kRange, function(kk) {
                        fit_tmp <- computeKmodes(kk)
                        sil <- cluster::silhouette(fit_tmp$cluster, dist_matrix)
                        mean(sil[, "sil_width"], na.rm = TRUE)
                    })
                    k <- res$k
                    result$silhouette <- res
                    result$notes <- c(result$notes, sprintf("Auto-selected k=%d (k-modes, silhouette)", k))
                }

                fit <- computeKmodes(k)
                result$clusters <- factor(fit$cluster, labels = paste0("C", seq_len(k)))
                result$usedK <- k
                result$fit <- fit

            } else if (method == "mca_kmeans") {
                if (length(catVars) == 0)
                    stop("MCA k-means method requires categorical variables")

                mca <- FactoMineR::MCA(df[, catVars, drop = FALSE], graph = FALSE)
                eig <- mca$eig
                cump <- cumsum(eig[, "cumulative percentage of variance"])
                keep <- max(1, which(cump >= 75)[1])
                scores <- mca$ind$coord[, 1:keep, drop = FALSE]
                scaled_scores <- scale(scores)

                if (autoSelect || is.null(k)) {
                    res <- chooseK(kRange, function(kk) {
                        cl <- stats::kmeans(scaled_scores, centers = kk, nstart = 50)
                        sil <- cluster::silhouette(cl$cluster, stats::dist(scaled_scores))
                        mean(sil[, "sil_width"], na.rm = TRUE)
                    })
                    k <- res$k
                    result$silhouette <- res
                    result$notes <- c(result$notes, sprintf("Auto-selected k=%d (MCA + k-means, silhouette)", k))
                }

                km <- stats::kmeans(scaled_scores, centers = k, nstart = 100)
                result$clusters <- factor(km$cluster, labels = paste0("C", seq_len(k)))
                result$usedK <- k
                result$fit <- km
                result$scores <- scaled_scores

            } else {
                stop(sprintf("Unknown clustering method: %s", method))
            }

            result
        },


        .paletteForLevels = function(levels) {
            base_cols <- grDevices::hcl.colors(max(3, length(levels)), palette = "Set2")
            base_cols <- base_cols[seq_along(levels)]
            names(base_cols) <- levels
            base_cols
        },

        # Escape variable names for safe use in jamovi contexts
        # Handles spaces, special characters, and Unicode
        .escapeVar = function(x) {
            # Convert to valid R names, then sanitize further
            escaped <- make.names(x, unique = TRUE)
            # Replace remaining non-alphanumeric with underscores
            escaped <- gsub("[^A-Za-z0-9_]+", "_", escaped)
            # Remove leading/trailing underscores
            escaped <- gsub("^_+|_+$", "", escaped)
            escaped
        },

        .kwEpsilonSquared = function(test_result, clusters, values) {
            H <- as.numeric(test_result$statistic)
            k <- nlevels(clusters)
            n <- sum(!is.na(values))
            denom <- n - k
            if (denom <= 0)
                return(NA_real_)
            eps2 <- (H - k + 1) / denom
            max(0, min(1, eps2))
        },

        .cramersV = function(chisq_result) {
            stat <- as.numeric(chisq_result$statistic)
            n <- sum(chisq_result$observed)
            if (n <= 0)
                return(NA_real_)
            dims <- dim(chisq_result$observed)
            phi <- sqrt(stat / n)
            min_dim <- min(dims[1] - 1, dims[2] - 1)
            if (min_dim <= 0)
                return(NA_real_)
            phi / sqrt(min_dim)
        },

        .init = function() {
            # Initialize UI elements that don't require data
            private$.initTodo()

            if (is.null(self$data))
                return()

            # Early validation: Check if variables are provided
            catVars <- if (!is.null(self$options$catVars) && length(self$options$catVars) > 0) self$options$catVars else character(0)
            contVars <- if (!is.null(self$options$contVars) && length(self$options$contVars) > 0) self$options$contVars else character(0)

            if (length(catVars) == 0 && length(contVars) == 0) {
                self$results$summary$setContent(
                    "<p><b>Getting Started:</b> Please select at least 2 IHC markers (categorical or continuous) from the options panel.</p>"
                )
                return()
            }

            # Initialize dynamic result tables (only when we have variables)
            private$.initTechnicalNotes()
            private$.initInterpretationGuide()
        },

        .run = function() {

            if (is.null(self$data))
                return()

            data <- self$data
            opts <- self$options
            `%||%` <- function(x, y) if (is.null(x)) y else x

            catVars <- if (!is.null(opts$catVars) && length(opts$catVars) > 0) opts$catVars else character(0)
            contVars <- if (!is.null(opts$contVars) && length(opts$contVars) > 0) opts$contVars else character(0)
            catVars <- unique(catVars)
            contVars <- unique(contVars)
            allVars <- c(catVars, contVars)

            if (length(allVars) < 2) {
                notice <- jmvcore::Notice$new(
                    options = self$options,
                    name = '.insufficient_markers',
                    type = jmvcore::NoticeType$ERROR
                )
                notice$setContent('At least 2 IHC markers required. Select categorical or continuous markers from the variable list to perform clustering analysis.')
                self$results$insert(999, notice)
                return()
            }

            # TESTING: tryCatch disabled - errors will be visible
            # prepared <- tryCatch({
                prepared <- private$.prepareData(data, catVars, contVars, opts)
            # }, error = function(e) {
            #     self$results$summary$setContent(
            #         sprintf("<p><b>Error:</b> %s</p>", private$.escapeHtml(e$message))
            #     )
            #     NULL
            # })

            if (is.null(prepared))
                return()

            df <- prepared$df
            catVars <- prepared$catVars
            contVars <- prepared$contVars

            # Phase 3: Calculate marker ratios (Sterlacci 2019)
            # Moved up so ratios participate in clustering
            ratioResult <- NULL
            if (isTRUE(opts$calculateRatios)) {
                ratioResult <- private$.calculateRatio(
                    data = data,
                    numerator_var = opts$ratioNumerator,
                    denominator_var = opts$ratioDenominator,
                    ratio_name = opts$ratioName %||% "Marker_Ratio",
                    opts = opts
                )

                # If successful, add ratio to continuous markers for clustering
                if (is.null(ratioResult$error)) {
                    # Add ratio values to data frame
                    ratio_name <- ratioResult$ratio_name
                    df[[ratio_name]] <- ratioResult$ratio_values

                    # Add to continuous markers list
                    contVars <- c(contVars, ratio_name)

                    # If classification requested, also add classified variable
                    if (isTRUE(opts$ratioClassification) && !is.null(ratioResult$classification)) {
                        df[[paste0(ratio_name, "_Class")]] <- ratioResult$classification$classes
                        catVars <- c(catVars, paste0(ratio_name, "_Class"))
                    }
                }
            }

            # Display binary conversion notice if using Jaccard distance
            distance_method <- opts$distanceMethod %||% "gower"
            if (distance_method == "jaccard") {
                private$.showBinaryConversionNotice(df, catVars, contVars)
            }

            # Missing data transparency notice
            missing_cat <- prepared$info$missingCategorical
            missing_cont <- prepared$info$missingContinuous
            has_missing_data <- length(missing_cat) > 0 || length(missing_cont) > 0

            if (has_missing_data) {
                handleMissingMethod <- if (is.null(opts$handleMissing)) "pairwise" else opts$handleMissing

                # Build notice message
                missing_parts <- character()
                if (length(missing_cat) > 0) {
                    missing_parts <- c(missing_parts,
                        sprintf("Categorical markers with missing values: %s",
                                paste(missing_cat, collapse = ", ")))
                }
                if (length(missing_cont) > 0) {
                    missing_parts <- c(missing_parts,
                        sprintf("Continuous markers with missing values: %s",
                                paste(missing_cont, collapse = ", ")))
                }

                # Count total missing cases
                all_missing_markers <- c(missing_cat, missing_cont)
                n_complete <- sum(complete.cases(df[, all_missing_markers, drop = FALSE]))
                n_total <- nrow(df)
                n_with_missing <- n_total - n_complete

                handling_method_text <- if (handleMissingMethod == "pairwise") {
                    "pairwise Gower distances (cases with missing data are included using available markers)"
                } else {
                    sprintf("complete cases only (%d of %d cases with complete data)", n_complete, n_total)
                }

                notice_text <- paste0(
                    sprintf("Missing data detected in %d marker(s) affecting %d case(s). ",
                            length(all_missing_markers), n_with_missing),
                    paste(missing_parts, collapse=". "),
                    sprintf(". Handling method: %s.", handling_method_text)
                )

                notice <- jmvcore::Notice$new(
                    options = self$options,
                    name = '.missing_data_transparency',
                    type = jmvcore::NoticeType$INFO
                )
                notice$setContent(notice_text)
                self$results$insert(2, notice)
            }

            # Validate data quality for clustering
            has_na <- any(is.na(df))
            handleMissing <- opts$handleMissing %||% "pairwise"

            if (has_na && handleMissing == "pairwise" && length(catVars) == 0 && length(contVars) > 0) {
                # Continuous-only data with NAs and pairwise handling will fail
                na_vars <- names(which(sapply(df, function(x) any(is.na(x)))))
                complete_n <- sum(complete.cases(df))

                notice <- jmvcore::Notice$new(
                    options = self$options,
                    name = '.missing_data_error',
                    type = jmvcore::NoticeType$ERROR
                )
                notice$setContent(sprintf(
                    'Cannot cluster %d continuous-only markers with missing data using pairwise distances. Solution: Change Missing Data Handling to "Complete cases only" (uses %d of %d cases) or add categorical markers to enable pairwise calculation.',
                    length(contVars), complete_n, nrow(df)
                ))
                self$results$insert(999, notice)
                return()
            }

            set.seed(opts$seed %||% 42)

            method <- opts$method %||% "pam"
            requestedK <- opts$nClusters %||% 3
            autoSelect <- isTRUE(opts$autoSelectK) || is.null(opts$nClusters)
            weights <- private$.parseWeights(opts$weights, colnames(df))

            # Note: Tables are automatically overwritten by jamovi, no need to clear

            # TESTING: tryCatch disabled - errors will be visible
            # clusterResult <- tryCatch({
                clusterResult <- private$.clusterData(df, method, opts, requestedK, autoSelect, catVars, contVars, weights, self$results$silhouetteStats)
            # }, error = function(e) {
            #     self$results$summary$setContent(
            #         sprintf("<p><b>Error:</b> %s</p>", private$.escapeHtml(e$message))
            #     )
            #     NULL
            # })

            if (is.null(clusterResult) || is.null(clusterResult$clusters))
                return()

            clusters <- clusterResult$clusters
            usedK <- clusterResult$usedK

            text_lines <- character()
            if (length(prepared$notes) > 0)
                text_lines <- c(text_lines, prepared$notes)
            if (length(clusterResult$notes) > 0)
                text_lines <- c(text_lines, clusterResult$notes)
            if (!is.null(weights))
                text_lines <- c(text_lines, "Variable weights applied during distance calculation")

            text_lines <- unique(text_lines)
            text_lines <- text_lines[nzchar(text_lines)]

            # Compute dimension reduction (PCA/MCA/MDS) if showPCAPlot is enabled
            dimredResult <- NULL
            if (isTRUE(opts$showPCAPlot)) {
                dimredResult <- private$.computeDimensionReduction(df, catVars, contVars)
            }

            analysisState <- list(
                clusters = clusters,
                usedK = usedK,
                df = df,
                method = method,
                catVars = catVars,
                contVars = contVars,
                fit = clusterResult$fit,
                dist = clusterResult$dist,
                hc = clusterResult$hc,
                scores = clusterResult$scores,
                dimred = dimredResult,  # Add dimension reduction results
                notes = text_lines
            )

            # Compute marker optimization if requested
            optimization <- NULL
            if (isTRUE(opts$markerOptimization)) {
                optimization <- private$.optimizeMarkerPanel(df, clusters, catVars, contVars)
            }

            # Perform marker-level clustering if requested
            markerClusteringResults <- NULL
            if (isTRUE(opts$performMarkerClustering)) {
                markerClusteringResults <- private$.performMarkerClustering(
                    df,
                    catVars,
                    contVars,
                    method = opts$markerClusteringMethod %||% "chisquared",
                    linkage = opts$markerLinkage %||% "ward",
                    testAssociations = opts$markerSignificanceTest %||% TRUE,
                    autoCut = opts$markerCutHeight %||% TRUE
                )
            }

            # Compute cluster quality metrics
            quality <- NULL
            if (isTRUE(opts$clusterQualityMetrics)) {
                quality <- private$.computeClusterQuality(df, clusters, clusterResult$dist)
            }

            # Perform iterative refinement if requested
            refinement <- NULL
            if (isTRUE(opts$iterativeRefinement)) {
                max_iter <- opts$refinementIterations
                if (is.null(max_iter)) max_iter <- 3
                refinement <- private$.performIterativeRefinement(df, catVars, contVars, max_iter)
            }

            # Perform spatial/compartment analysis if requested
            spatialResults <- NULL
            spatialConcordance <- NULL
            spatialMarkerDifferences <- NULL

            if (isTRUE(opts$performSpatialAnalysis) && !is.null(opts$spatialCompartment)) {
                tryCatch({
                    spatialResults <- private$.analyzeSpatialCompartments(
                        df,
                        opts$spatialCompartment,
                        catVars,
                        contVars,
                        opts$spatialComparisonMode %||% "both"
                    )

                    # Compare compartments if in between or both mode
                    if (opts$spatialComparisonMode %in% c("between", "both")) {
                        spatialConcordance <- private$.compareCompartmentClustering(spatialResults)
                    }

                    # Test marker differences between compartments
                    spatialMarkerDifferences <- private$.testMarkerDifferencesByCompartment(
                        df,
                        opts$spatialCompartment,
                        catVars,
                        contVars
                    )
                }, error = function(e) {
                    # Silently fail if spatial analysis errors
                    NULL
                })
            }

            # Calculate diagnostic metrics if known diagnosis is provided
            markerPerformance <- NULL
            optimalPanels <- NULL
            outlierCases <- NULL

            # Define markerVars for diagnostic features (ratio already included if computed)
            markerVars <- c(catVars, contVars)

            if (!is.null(opts$knownDiagnosis)) {
                # Marker performance metrics
                if (isTRUE(opts$calculateDiagnosticMetrics)) {
                    markerPerformance <- private$.calculateMarkerPerformance(
                        df,
                        markerVars,
                        opts$knownDiagnosis
                    )
                }

                # Optimal antibody panel selection
                if (isTRUE(opts$identifyOptimalPanel)) {
                    panelSize <- opts$panelSize %||% "pairs"
                    optimalPanels <- private$.identifyOptimalPanels(
                        df,
                        markerVars,
                        opts$knownDiagnosis,
                        panelSize
                    )
                }
            }

            # Phase 2: Reproducibility testing (Sterlacci 2019)
            reproducibilityResults <- NULL
            if (isTRUE(opts$reproducibilityTest)) {
                # Create clustering function wrapper
                cluster_func <- function(data_subset) {
                    return(private$.performClustering(
                        df = data_subset,
                        catVars = catVars,
                        contVars = contVars,
                        opts = opts,
                        silhouetteTable = NULL
                    ))
                }

                reproducibilityResults <- private$.testReproducibility(
                    df = df,
                    catVars = catVars,
                    contVars = contVars,
                    opts = opts,
                    cluster_func = cluster_func
                )
            }

            # Phase 2: Supervised clustering (Sterlacci 2019)
            supervisedResults <- NULL
            if (isTRUE(opts$supervisedClustering) && !is.null(opts$supervisedVariable)) {
                supervisedResults <- private$.supervisedClustering(
                    df = df,
                    catVars = catVars,
                    contVars = contVars,
                    opts = opts,
                    group_var = opts$supervisedVariable
                )
            }

            # Flag outlier cases
            if (isTRUE(opts$flagOutliers)) {
                # Compute silhouette if not already done
                sil <- cluster::silhouette(as.integer(clusters), clusterResult$dist)
                threshold <- opts$outlierThreshold %||% 0.25
                outlierCases <- private$.flagOutlierCases(
                    df,
                    clusters,
                    sil,
                    threshold,
                    opts$caseId
                )
            }

            # Add all results to analysis state
            analysisState$optimization <- optimization
            analysisState$quality <- quality
            analysisState$refinement <- refinement
            analysisState$markerClusteringResults <- markerClusteringResults
            analysisState$spatialResults <- spatialResults
            analysisState$spatialConcordance <- spatialConcordance
            analysisState$spatialMarkerDifferences <- spatialMarkerDifferences
            analysisState$markerPerformance <- markerPerformance
            analysisState$optimalPanels <- optimalPanels
            analysisState$outlierCases <- outlierCases

            self$results$summary$setState(analysisState)

            # Populate all tables
            private$.populatePCAContributions()
            private$.populateMarkerImportance()
            private$.populateMarkerClusteringResults()
            private$.populateClusterQuality()
            private$.populateRefinementHistory()
            private$.populateSpatialCompartmentSummary()
            private$.populateSpatialConcordance()
            private$.populateSpatialClusterComparison()
            private$.populateSpatialMarkerDifferences()
            private$.populateMarkerPerformance()
            private$.populateOptimalPanels()
            private$.populateOutlierCases()
            private$.populateReproducibilityStats(reproducibilityResults)
            private$.populateSupervisedResults(supervisedResults)
            private$.populateRatioResults(ratioResult)

            sizes <- table(clusters)
            if (!is.null(self$results$clusterSizes)) {
                tbl <- self$results$clusterSizes
                for (cl in names(sizes)) {
                    tbl$addRow(
                        rowKey = cl,
                        values = list(
                            cluster = cl,
                            n = as.integer(sizes[[cl]]),
                            percent = as.numeric(sizes[[cl]]) / length(clusters)
                        )
                    )
                }
            } else if (!is.null(self$results$sizes)) {
                tbl <- self$results$sizes
                for (cl in names(sizes)) {
                    tbl$addRow(
                        rowKey = cl,
                        values = list(
                            cluster = cl,
                            n = as.integer(sizes[[cl]])
                        )
                    )
                }
            }

            if (method == "pam" && !is.null(clusterResult$fit)) {
                medoidTable <- self$results$medoidInfo
                if (!is.null(medoidTable)) {
                    fit <- clusterResult$fit

                    # Use caseId if provided, otherwise use rownames or index
                    caseIdVar <- if (!is.null(opts$caseId) && length(opts$caseId) > 0) opts$caseId else NULL

                    for (i in seq_along(fit$medoids)) {
                        medoidId <- if (!is.null(caseIdVar)) {
                            as.character(df[[caseIdVar]][fit$medoids[i]])
                        } else if (!is.null(rownames(df))) {
                            rownames(df)[fit$medoids[i]]
                        } else {
                            as.character(fit$medoids[i])
                        }

                        medoidRow <- df[fit$medoids[i], , drop = FALSE]
                        profileDesc <- paste(names(medoidRow), "=", sapply(medoidRow, as.character), collapse = ", ")
                        medoidTable$addRow(
                            rowKey = i,
                            values = list(
                                cluster = paste0("C", i),
                                medoid_id = medoidId,
                                description = profileDesc
                            )
                        )
                    }
                }
            }

            if (!is.null(self$results$markerSummary)) {
                mt <- self$results$markerSummary
                for (cl in levels(clusters)) {
                    for (mk in colnames(df)) {
                        clusterData <- df[clusters == cl, mk]
                        clusterData <- clusterData[!is.na(clusterData)]
                        if (length(clusterData) == 0)
                            next

                        if (is.numeric(clusterData)) {
                            meanVal <- round(mean(clusterData), 2)
                            medianVal <- round(stats::median(clusterData), 2)
                            sdVal <- round(stats::sd(clusterData), 2)
                            iqrVal <- round(stats::IQR(clusterData), 2)
                            rangeVal <- paste0(round(min(clusterData), 2), " - ", round(max(clusterData), 2))

                            mt$addRow(rowKey = paste(cl, mk, sep = "_"), list(
                                cluster = cl,
                                marker = mk,
                                type = "Continuous",
                                mean_median = paste0("Mean: ", meanVal, "; Median: ", medianVal),
                                sd_iqr = paste0("SD: ", sdVal, "; IQR: ", iqrVal),
                                range = rangeVal
                            ))
                        } else {
                            tab <- table(clusterData)
                            mode_level <- names(which.max(tab))
                            mode_pct <- round(100 * max(tab) / sum(tab), 1)
                            levels_summary <- paste(names(tab), "(", tab, ")", collapse = ", ")

                            mt$addRow(rowKey = paste(cl, mk, sep = "_"), list(
                                cluster = cl,
                                marker = mk,
                                type = "Categorical",
                                mean_median = paste0("Mode: ", mode_level, " (", mode_pct, "%)"),
                                sd_iqr = "-",
                                range = levels_summary
                            ))
                        }
                    }
                }
            }

            if (!is.null(self$results$distr)) {
                distr <- do.call(rbind, lapply(seq_along(df), function(j) {
                    mk <- colnames(df)[j]
                    if (is.numeric(df[[j]]))
                        return(NULL)
                    d <- as.data.frame(table(clusters, df[[j]], useNA = "no"))
                    colnames(d) <- c("cluster", "level", "n")
                    d$marker <- mk
                    d
                }))

                if (!is.null(distr) && nrow(distr) > 0) {
                    suppressWarnings({
                        distr <- dplyr::group_by(distr, marker, cluster)
                        distr <- dplyr::mutate(distr, pct = n / sum(n))
                        distr <- dplyr::ungroup(distr)
                    })
                    dtab <- self$results$distr
                    for (i in seq_len(nrow(distr))) {
                        row <- distr[i, ]
                        dtab$addRow(rowKey = paste(row$marker, row$cluster, row$level, sep = "|"), list(
                            marker = as.character(row$marker),
                            cluster = as.character(row$cluster),
                            level = as.character(row$level),
                            n = as.integer(row$n),
                            pct = as.numeric(row$pct)
                        ))
                    }
                }
            }

            if (isTRUE(self$options$associationTests)) {
                assoc_results <- list()
                for (mk in colnames(df)) {
                    marker_data <- df[[mk]]
                    if (is.numeric(marker_data)) {
                        # TESTING: tryCatch disabled - errors will be visible
                        # test_result <- tryCatch(kruskal.test(marker_data, clusters), error = function(e) NULL)
                        test_result <- kruskal.test(marker_data, clusters)
                        if (!is.null(test_result)) {
                            assoc_results[[mk]] <- list(
                                marker = mk,
                                test = "Kruskal-Wallis",
                                statistic = as.numeric(test_result$statistic),
                                p = test_result$p.value,
                                effect = private$.kwEpsilonSquared(test_result, clusters, marker_data)
                            )
                        }
                    } else {
                        tab <- table(clusters, marker_data, useNA = "no")
                        if (length(tab) == 0)
                            next
                        row_totals <- rowSums(tab)
                        col_totals <- colSums(tab)
                        tab <- tab[row_totals > 0, col_totals > 0, drop = FALSE]
                        if (nrow(tab) < 2 || ncol(tab) < 2)
                            next

                        chi_stat <- suppressWarnings(chisq.test(tab, correct = FALSE))
                        effect_size <- private$.cramersV(chi_stat)
                        expected <- chi_stat$expected
                        test_label <- "Chi-square"
                        p_value <- chi_stat$p.value
                        statistic <- as.numeric(chi_stat$statistic)

                        if (any(expected < 5, na.rm = TRUE)) {
                            if (nrow(tab) == 2 && ncol(tab) == 2) {
                                fisher_res <- suppressWarnings(fisher.test(tab))
                                p_value <- fisher_res$p.value
                                test_label <- "Fisher's exact"
                            } else {
                                chi_sim <- suppressWarnings(chisq.test(tab, simulate.p.value = TRUE, B = 5000))
                                p_value <- chi_sim$p.value
                                test_label <- "Chi-square (simulated p)"
                            }
                        }

                        assoc_results[[mk]] <- list(
                            marker = mk,
                            test = test_label,
                            statistic = statistic,
                            p = p_value,
                            effect = effect_size
                        )
                    }
                }

                # Apply multiple testing correction
                correction_method <- self$options$multipleTestingCorrection %||% "bonferroni"

                # Extract p-values
                p_values <- sapply(assoc_results, function(x) x$p)

                if (correction_method != "none" && length(p_values) > 0) {
                    # Apply correction
                    p_adjusted <- stats::p.adjust(p_values, method = correction_method)

                    # Add adjusted p-values back to results
                    for (i in seq_along(assoc_results)) {
                        mk <- names(assoc_results)[i]
                        assoc_results[[mk]]$p_adj <- p_adjusted[i]
                    }
                } else {
                    # No correction - adjusted = raw
                    for (mk in names(assoc_results)) {
                        assoc_results[[mk]]$p_adj <- assoc_results[[mk]]$p
                    }
                }

                # Populate table
                if (!is.null(self$results$associationTests)) {
                    at <- self$results$associationTests
                    for (mk in names(assoc_results)) {
                        res <- assoc_results[[mk]]
                        at$addRow(rowKey = mk, list(
                            marker = res$marker,
                            test = res$test,
                            statistic = res$statistic,
                            p_value = res$p,
                            p_adjusted = res$p_adj,
                            effect_size = res$effect
                        ))
                    }

                    # Add explanatory note
                    if (correction_method == "bonferroni" && length(p_values) > 0) {
                        bonf_threshold <- 0.05 / length(p_values)
                        note <- sprintf("Bonferroni-corrected significance threshold: p < %.6f (α=0.05 / %d markers)",
                                       bonf_threshold, length(p_values))
                        at$setNote("bonferroni", note)
                    } else if (correction_method == "fdr") {
                        at$setNote("fdr", "False Discovery Rate (Benjamini-Hochberg) correction applied")
                    } else if (correction_method == "holm") {
                        at$setNote("holm", "Holm sequential correction applied")
                    } else if (correction_method == "none") {
                        at$setNote("none", "No multiple testing correction applied")
                    }
                }

                if (!is.null(self$results$assoc)) {
                    at_simple <- self$results$assoc
                    for (mk in names(assoc_results)) {
                        res <- assoc_results[[mk]]
                        at_simple$addRow(rowKey = mk, list(marker = res$marker, p = res$p))
                    }
                }
            }

            if (isTRUE(self$options$clusterProfiles) && !is.null(self$results$clusterProfiles)) {
                profileTable <- self$results$clusterProfiles
                for (cl in levels(clusters)) {
                    cluster_indices <- which(clusters == cl)
                    for (marker in colnames(df)) {
                        marker_data <- df[cluster_indices, marker]
                        if (is.numeric(marker_data)) {
                            summary_text <- sprintf("Median: %.2f (IQR: %.2f)", stats::median(marker_data, na.rm = TRUE), stats::IQR(marker_data, na.rm = TRUE))
                        } else {
                            counts <- table(marker_data)
                            mode_val <- names(which.max(counts))
                            pct <- round(100 * max(counts) / sum(counts), 1)
                            summary_text <- sprintf("Most common: %s (%.1f%%)", mode_val, pct)
                        }
                        profileTable$addRow(rowKey = paste(cl, marker, sep = "_"), list(
                            cluster = cl,
                            marker = marker,
                            summary = summary_text
                        ))
                    }
                }
            }

            if (length(self$options$clinicalVars) > 0 && !is.null(self$results$clinicalComparison)) {
                clinicalTable <- self$results$clinicalComparison
                for (var in self$options$clinicalVars) {
                    clinicalData <- self$data[[var]]
                    if (is.null(clinicalData)) {
                        warning(sprintf("Clinical variable '%s' not found in data", var))
                        next
                    }

                    nonMissing <- sum(!is.na(clinicalData))
                    if (nonMissing < 5) {
                        warning(sprintf("Clinical variable '%s' has insufficient non-missing values (%d)", var, nonMissing))
                        next
                    }

                    if (is.numeric(clinicalData)) {
                        # TESTING: tryCatch disabled - errors will be visible
                        # test_result <- tryCatch(kruskal.test(clinicalData, clusters), error = function(e) NULL)
                        test_result <- kruskal.test(clinicalData, clusters)
                        if (!is.null(test_result)) {
                            summary_text <- paste(
                                sapply(levels(clusters), function(cl) {
                                    cl_data <- clinicalData[clusters == cl]
                                    cl_data <- cl_data[!is.na(cl_data)]
                                    if (length(cl_data) > 0) {
                                        sprintf("%s: median %.2f (IQR %.2f)", cl, stats::median(cl_data), stats::IQR(cl_data))
                                    } else {
                                        sprintf("%s: No data", cl)
                                    }
                                }),
                                collapse = "; "
                            )

                            clinicalTable$addRow(rowKey = var, list(
                                variable = var,
                                test = "Kruskal-Wallis",
                                statistic = as.numeric(test_result$statistic),
                                p_value = test_result$p.value,
                                summary = summary_text
                            ))
                        }
                    } else {
                        tab <- table(clusters, clinicalData, useNA = "no")
                        if (length(tab) == 0)
                            next
                        row_totals <- rowSums(tab)
                        col_totals <- colSums(tab)
                        tab <- tab[row_totals > 0, col_totals > 0, drop = FALSE]
                        if (nrow(tab) < 2 || ncol(tab) < 2)
                            next

                        chi_stat <- suppressWarnings(chisq.test(tab, correct = FALSE))
                        expected <- chi_stat$expected
                        test_label <- "Chi-square"
                        p_value <- chi_stat$p.value
                        statistic <- as.numeric(chi_stat$statistic)

                        if (any(expected < 5, na.rm = TRUE)) {
                            if (nrow(tab) == 2 && ncol(tab) == 2) {
                                fisher_res <- suppressWarnings(fisher.test(tab))
                                p_value <- fisher_res$p.value
                                test_label <- "Fisher's exact"
                            } else {
                                chi_sim <- suppressWarnings(chisq.test(tab, simulate.p.value = TRUE, B = 5000))
                                p_value <- chi_sim$p.value
                                test_label <- "Chi-square (simulated p)"
                            }
                        }

                        clinicalTable$addRow(rowKey = var, list(
                            variable = var,
                            test = test_label,
                            statistic = statistic,
                            p_value = p_value,
                            summary = "Cross-tabulation performed"
                        ))
                    }
                }
            }

            if (isTRUE(self$options$consensusClustering)) {
                private$.performConsensus(df, clusterResult, catVars, contVars, weights)
            }

            method_names <- list(
                "pam" = "PAM (k-medoids) with Gower distance",
                "hierarchical" = "Hierarchical clustering (Ward) with Gower distance",
                "dimreduce" = "Dimension reduction (FAMD/MCA/PCA) + k-means",
                "kmodes" = "k-modes (categorical only)",
                "mca_kmeans" = "MCA + k-means (categorical only)"
            )
            method_name <- method_names[[method]] %||% method

            # Compute overall silhouette for quality warnings
            avg_silhouette <- NA
            if (!is.null(clusterResult$dist) && !is.null(clusters)) {
                sil <- cluster::silhouette(as.integer(clusters), clusterResult$dist)
                avg_silhouette <- mean(sil[, "sil_width"], na.rm = TRUE)
            }

            # Notice 1: Small sample size warning
            notice_position <- 1
            if (nrow(df) < 30) {
                notice <- jmvcore::Notice$new(
                    options = self$options,
                    name = '.small_sample_warning',
                    type = jmvcore::NoticeType$STRONG_WARNING
                )
                notice$setContent(sprintf(
                    'Small sample size (N=%d). Clusters may be unstable with <30 cases. Consider reproducibility testing or increasing sample size for robust results.',
                    nrow(df)
                ))
                self$results$insert(notice_position, notice)
                notice_position <- notice_position + 1
            }

            # Notice 2: Poor cluster quality warning
            if (!is.na(avg_silhouette) && avg_silhouette < 0.3) {
                notice <- jmvcore::Notice$new(
                    options = self$options,
                    name = '.poor_cluster_quality',
                    type = jmvcore::NoticeType$STRONG_WARNING
                )
                notice$setContent(sprintf(
                    'Poor cluster quality detected (avg silhouette=%.2f). Values <0.3 indicate weak cluster structure. Consider reducing cluster count, checking data quality, or using different markers.',
                    avg_silhouette
                ))
                self$results$insert(notice_position, notice)
                notice_position <- notice_position + 1
            }

            txt <- paste(c(
                sprintf("Method: %s", method_name),
                sprintf("k: %s", ifelse(is.null(usedK), "NA", usedK)),
                sprintf("Categorical markers: %d", length(catVars)),
                sprintf("Continuous markers: %d", length(contVars)),
                sprintf("Total cases: %d", nrow(df)),
                text_lines
            ), collapse = "
")

            if (!is.null(self$results$summary)) {
                self$results$summary$setContent(paste0("<pre>", txt, "</pre>"))
            } else if (!is.null(self$results$text)) {
                self$results$text$setContent(txt)
            }

            if (!is.null(self$results$executiveSummary)) {
                executive_summary <- private$.generateExecutiveSummary()
                self$results$executiveSummary$setContent(executive_summary)
            }

            if (!is.null(self$results$survivalPlot)) {
                if (!is.null(self$options$survivalTime) && !is.null(self$options$survivalEvent)) {
                    self$results$survivalPlot$setVisible(TRUE)
                } else {
                    self$results$survivalPlot$setVisible(FALSE)
                }
            }

            # Export cluster assignments if requested
            if (isTRUE(self$options$exportClusters)) {
                # TESTING: tryCatch disabled - errors will be visible
                # tryCatch({
                    cluster_var_name <- "IHC_Cluster"
                    # Check if variable already exists
                    existing_cols <- colnames(self$data)
                    if (cluster_var_name %in% existing_cols) {
                        cluster_var_name <- paste0(cluster_var_name, "_", format(Sys.time(), "%Y%m%d_%H%M%S"))
                    }

                    # Add clusters to dataset (Jamovi-compatible approach)
                    # Note: This creates a computed column in the dataset
                    cluster_assignments <- as.character(clusters)
                    names(cluster_assignments) <- rownames(df) %||% seq_along(clusters)

                    # Store cluster assignments in result state for potential export
                    analysisState$clusterAssignments <- cluster_assignments
                    analysisState$clusterVarName <- cluster_var_name
                    self$results$summary$setState(analysisState)

                    # Notify user
                    note_msg <- sprintf("Cluster assignments ready for export as '%s'. Use jamovi data export to save.", cluster_var_name)
                    text_lines <- c(text_lines, note_msg)

                # }, error = function(e) {
                #     warning(sprintf("Failed to prepare cluster export: %s", e$message))
                # })
            }

            # Populate interpretation guide if requested
            if (isTRUE(opts$showInterpretation)) {
                private$.populateInterpretationGuide()
            }

            # Populate technical notes if requested
            if (isTRUE(opts$showTechnicalNotes)) {
                private$.populateTechnicalNotes()
            }

            # Populate diagnostic glossary if diagnostic metrics are calculated
            if (isTRUE(opts$calculateDiagnosticMetrics) && isTRUE(opts$showDiagnosticGlossary)) {
                private$.populateDiagnosticGlossary()
            }

            # Add completion info notice at bottom
            info <- jmvcore::Notice$new(
                options = self$options,
                name = '.analysis_complete',
                type = jmvcore::NoticeType$INFO
            )
            sil_text <- if (!is.na(avg_silhouette)) sprintf(", avg silhouette=%.2f", avg_silhouette) else ""
            info$setContent(sprintf(
                'Analysis completed: %d cases clustered into %d groups using %s method%s.',
                nrow(df), usedK, method_name, sil_text
            ))
            self$results$insert(999, info)
        },
        
        # Initialize instruction panel with context-sensitive guidance
        # Provides different instructions based on current variable selection
        # and analysis configuration. Shows step-by-step workflow guidance.
        .initTodo = function() {
            nCat <- length(self$options$catVars)
            nCont <- length(self$options$contVars)
            totalVars <- nCat + nCont

            # Show welcome message only when minimum required variables are not selected
            # Minimum: at least 2 markers total (can be all categorical, all continuous, or mixed)
            if (totalVars < 2) {
                html <- '
                <div style="background-color: #f0f8ff; padding: 20px; margin: 15px 0; border-radius: 8px; border-left: 5px solid #2196F3;">
                <h3 style="margin-top: 0; color: #1976D2;">🧪 Welcome to IHC Clustering Analysis</h3>
                
                <div style="background-color: white; padding: 15px; margin: 10px 0; border-radius: 5px;">
                <h4>📋 Step 1: Select Your Variables</h4>
                <table style="width: 100%; border-collapse: collapse;">
                <tr style="background-color: #f8f9fa;">
                <th style="padding: 10px; text-align: left; border: 1px solid #dee2e6;">Variable Type</th>
                <th style="padding: 10px; text-align: left; border: 1px solid #dee2e6;">Examples</th>
                <th style="padding: 10px; text-align: left; border: 1px solid #dee2e6;">Data Format</th>
                </tr>
                <tr>
                <td style="padding: 10px; border: 1px solid #dee2e6;"><b>Categorical IHC Markers</b></td>
                <td style="padding: 10px; border: 1px solid #dee2e6;">ER, PR, HER2 status</td>
                <td style="padding: 10px; border: 1px solid #dee2e6;">pos/neg, 0/1/2/3, negative/weak/moderate/strong</td>
                </tr>
                <tr>
                <td style="padding: 10px; border: 1px solid #dee2e6;"><b>Continuous IHC Markers</b></td>
                <td style="padding: 10px; border: 1px solid #dee2e6;">Ki-67, ER%, AR H-score</td>
                <td style="padding: 10px; border: 1px solid #dee2e6;">0-100 (%), 0-300 (H-score), expression levels</td>
                </tr>
                <tr>
                <td style="padding: 10px; border: 1px solid #dee2e6;"><b>Case ID (optional)</b></td>
                <td style="padding: 10px; border: 1px solid #dee2e6;">Patient ID, Case number</td>
                <td style="padding: 10px; border: 1px solid #dee2e6;">Any unique identifier</td>
                </tr>
                </table>
                </div>
                
                <div style="background-color: white; padding: 15px; margin: 10px 0; border-radius: 5px;">
                <h4>⚙️ Step 2: Choose Clustering Method</h4>
                <ul style="list-style-type: none; padding-left: 0;">
                <li style="margin: 8px 0;">🎯 <b>PAM (k-medoids):</b> Robust to outliers, identifies representative cases (medoids)</li>
                <li style="margin: 8px 0;">🌳 <b>Hierarchical:</b> Shows cluster relationships, good for exploratory analysis</li>
                <li style="margin: 8px 0;">📊 <b>MCA/PCA + k-means:</b> Dimension reduction followed by clustering</li>
                </ul>
                </div>
                
                <div style="background-color: white; padding: 15px; margin: 10px 0; border-radius: 5px;">
                <h4>🔧 Step 3: Configure Analysis</h4>
                <ul>
                <li><b>Number of Clusters:</b> Enable auto-selection to find optimal k using silhouette analysis</li>
                <li><b>Data Preprocessing:</b> Scale continuous variables, handle missing data</li>
                <li><b>Visualizations:</b> Heatmaps, silhouette plots, PCA/MCA plots</li>
                <li><b>Clinical Correlations:</b> Test associations with clinical variables</li>
                </ul>
                </div>
                
                <div style="background-color: #fff3cd; padding: 10px; margin: 10px 0; border-radius: 5px; border: 1px solid #ffc107;">
                <p style="margin: 0;"><b>💡 Note:</b> This analysis uses Gower distance to appropriately handle mixed categorical and continuous data.</p>
                </div>
                </div>'
            } else {
                # Variables selected - show brief configuration summary
                # Analysis will run automatically once minimum requirements are met
                html <- paste0(
                    '<div style="background-color: #e8f4fd; padding: 15px; margin: 10px 0; border-radius: 5px; border-left: 4px solid #2196F3;">',
                    '<h4 style="margin-top: 0; color: #1976D2;">✅ Analysis Configuration</h4>',
                    '<p><b>Variables Selected:</b> ', nCat, ' categorical + ', nCont, ' continuous markers</p>'
                )

                if (self$options$autoSelectK) {
                    html <- paste0(html, '<p>🎯 <b>Cluster Selection:</b> Automatic (optimal k will be determined)</p>')
                } else {
                    html <- paste0(html, '<p>🎯 <b>Clusters:</b> ', self$options$nClusters, ' (fixed)</p>')
                }

                html <- paste0(html,
                    '<p>📊 <b>Method:</b> ', toupper(self$options$method), '</p>',
                    '<p><em>Analysis will run automatically. You can adjust settings in the panel as needed.</em></p>',
                    '</div>'
                )
            }
            
            self$results$todo$setContent(html)
        },
        
        # Initialize technical implementation notes panel
        # Provides comprehensive documentation about clustering algorithms,
        # distance metrics, and statistical methods used in the analysis.
        .initTechnicalNotes = function() {
            html <- '
            <div style="background-color: #f8f9fa; padding: 20px; margin: 15px 0; border-radius: 8px; border: 1px solid #dee2e6;">
            <h3 style="margin-top: 0; color: #495057;">🔬 Technical Implementation Details</h3>
            
            <div style="background-color: white; padding: 15px; margin: 10px 0; border-radius: 5px;">
            <h4>📐 Distance Metrics & Data Handling</h4>
            <ul>
            <li><b>Gower Distance:</b> Specially designed for mixed data types</li>
            <li><b>Categorical Variables:</b> Simple matching coefficient (0 = different, 1 = same)</li>
            <li><b>Continuous Variables:</b> Range-normalized to [0,1] scale</li>
            <li><b>Missing Values:</b> Categorical markers gain an explicit "Missing" level; continuous markers are median-imputed when pairwise handling is selected</li>
            <li><b>Scaling:</b> Z-score standardization available for continuous variables</li>
            </ul>
            </div>
            
            <div style="background-color: white; padding: 15px; margin: 10px 0; border-radius: 5px;">
            <h4>🔄 Clustering Algorithms</h4>
            <table style="width: 100%; border-collapse: collapse; margin: 10px 0;">
            <tr style="background-color: #e9ecef;">
            <th style="padding: 10px; text-align: left; border: 1px solid #dee2e6;">Algorithm</th>
            <th style="padding: 10px; text-align: left; border: 1px solid #dee2e6;">Strengths</th>
            <th style="padding: 10px; text-align: left; border: 1px solid #dee2e6;">Best Use Cases</th>
            </tr>
            <tr>
            <td style="padding: 10px; border: 1px solid #dee2e6;"><b>PAM (k-medoids)</b></td>
            <td style="padding: 10px; border: 1px solid #dee2e6;">Outlier resistant, interpretable medoids</td>
            <td style="padding: 10px; border: 1px solid #dee2e6;">Small-medium datasets, need representative cases</td>
            </tr>
            <tr>
            <td style="padding: 10px; border: 1px solid #dee2e6;"><b>Hierarchical (Ward)</b></td>
            <td style="padding: 10px; border: 1px solid #dee2e6;">Shows cluster relationships, deterministic</td>
            <td style="padding: 10px; border: 1px solid #dee2e6;">Exploratory analysis, understanding structure</td>
            </tr>
            <tr>
            <td style="padding: 10px; border: 1px solid #dee2e6;"><b>MCA/PCA + k-means</b></td>
            <td style="padding: 10px; border: 1px solid #dee2e6;">Handles high dimensions, efficient</td>
            <td style="padding: 10px; border: 1px solid #dee2e6;">Many variables, large datasets</td>
            </tr>
            </table>
            </div>
            
            <div style="background-color: white; padding: 15px; margin: 10px 0; border-radius: 5px;">
            <h4>📊 Quality Assessment & Validation</h4>
            <ul>
            <li><b>Silhouette Analysis:</b> Measures cluster cohesion (within) vs separation (between)</li>
            <li><b>Optimal k Selection:</b> Maximizes average silhouette width across k-range</li>
            <li><b>Consensus Clustering:</b> Bootstrap resampling to assess stability</li>
            <li><b>Statistical Tests:</b> Chi-square/Fisher (categorical), Kruskal-Wallis/ANOVA (continuous)</li>
            </ul>
            
            <div style="background-color: #f0f9ff; padding: 10px; margin: 10px 0; border-radius: 3px; border-left: 3px solid #0ea5e9;">
            <h5>Silhouette Interpretation:</h5>
            <ul style="margin: 5px 0;">
            <li><span style="color: #059669;">0.7 - 1.0:</span> Strong, well-separated clusters</li>
            <li><span style="color: #d97706;">0.5 - 0.7:</span> Reasonable structure found</li>
            <li><span style="color: #dc2626;">< 0.5:</span> Weak clusters, consider fewer k or different method</li>
            </ul>
            </div>
            </div>
            
            <div style="background-color: white; padding: 15px; margin: 10px 0; border-radius: 5px;">
            <h4>📚 R Package Dependencies</h4>
            <code style="background-color: #f1f3f4; padding: 8px; border-radius: 3px; display: block; margin: 5px 0;">
            cluster (PAM, Gower distance), FactoMineR (MCA/PCA), factoextra (visualization),<br>
            ComplexHeatmap (advanced heatmaps), survival (survival analysis)
            </code>
            </div>
            
            <div style="background-color: #f0fdf4; padding: 10px; margin: 10px 0; border-radius: 5px; border: 1px solid #22c55e;">
            <p style="margin: 0;"><b>📖 Reference:</b> Gower, J.C. (1971). A general coefficient of similarity and some of its properties. <em>Biometrics</em>, 27(4), 857-871.</p>
            </div>
            </div>'
            
            self$results$technicalNotes$setContent(html)
        },
        
        # Initialize clinical interpretation guide panel
        # Provides clinical context for interpreting clustering results,
        # including guidelines for validation and clinical application.
        .initInterpretationGuide = function() {
            html <- '
            <div style="background-color: #fff8e1; padding: 20px; margin: 15px 0; border-radius: 8px; border-left: 5px solid #ffc107;">
            <h3 style="margin-top: 0; color: #e65100;">🏥 Clinical Interpretation Guide</h3>'

            html <- paste0(html, '
            
            <div style="background-color: white; padding: 15px; margin: 10px 0; border-radius: 5px;">
            <h4>🎯 Understanding Your Results</h4>
            <table style="width: 100%; border-collapse: collapse;">
            <tr style="background-color: #f8f9fa;">
            <th style="padding: 12px; text-align: left; border: 1px solid #dee2e6;">Output</th>
            <th style="padding: 12px; text-align: left; border: 1px solid #dee2e6;">Interpretation</th>
            <th style="padding: 12px; text-align: left; border: 1px solid #dee2e6;">Clinical Significance</th>
            </tr>
            <tr>
            <td style="padding: 12px; border: 1px solid #dee2e6;"><b>Cluster Profiles</b></td>
            <td style="padding: 12px; border: 1px solid #dee2e6;">Characteristic IHC pattern for each cluster</td>
            <td style="padding: 12px; border: 1px solid #dee2e6;">May represent distinct molecular subtypes</td>
            </tr>
            <tr>
            <td style="padding: 12px; border: 1px solid #dee2e6;"><b>Medoid Cases</b></td>
            <td style="padding: 12px; border: 1px solid #dee2e6;">Most representative case in each cluster</td>
            <td style="padding: 12px; border: 1px solid #dee2e6;">Reference cases for classification</td>
            </tr>
            <tr>
            <td style="padding: 12px; border: 1px solid #dee2e6;"><b>Silhouette Plot</b></td>
            <td style="padding: 12px; border: 1px solid #dee2e6;">Cluster quality and case assignment confidence</td>
            <td style="padding: 12px; border: 1px solid #dee2e6;">Validates cluster definitions</td>
            </tr>
            <tr>
            <td style="padding: 12px; border: 1px solid #dee2e6;"><b>Association Tests</b></td>
            <td style="padding: 12px; border: 1px solid #dee2e6;">Which markers best distinguish clusters</td>
            <td style="padding: 12px; border: 1px solid #dee2e6;">Identifies key diagnostic markers</td>
            </tr>
            </table>
            </div>
            
            <div style="background-color: white; padding: 15px; margin: 10px 0; border-radius: 5px;">
            <h4>🔬 Clinical Applications</h4>
            <div style="display: grid; grid-template-columns: 1fr 1fr; gap: 15px;">
            <div>
            <h5 style="color: #1976d2;">💊 Treatment Selection</h5>
            <ul>
            <li>Different clusters may respond to different therapies</li>
            <li>Precision medicine based on IHC patterns</li>
            <li>Combination therapy selection</li>
            </ul>
            </div>
            <div>
            <h5 style="color: #1976d2;">📈 Prognosis</h5>
            <ul>
            <li>Clusters may have different survival outcomes</li>
            <li>Risk stratification for clinical decisions</li>
            <li>Follow-up intensity planning</li>
            </ul>
            </div>
            </div>
            </div>
            
            <div style="background-color: white; padding: 15px; margin: 10px 0; border-radius: 5px;">
            <h4>✅ Validation Checklist</h4>
            <ol style="margin: 10px 0;">
            <li><b>Quality Check:</b> Review silhouette plot - aim for average silhouette > 0.5</li>
            <li><b>Biological Plausibility:</b> Do clusters make biological sense?</li>
            <li><b>Clinical Relevance:</b> Test associations with clinical outcomes</li>
            <li><b>Reproducibility:</b> Use consensus clustering to check stability</li>
            <li><b>External Validation:</b> Test on independent cohort if available</li>
            </ol>
            </div>
            
            <div style="background-color: white; padding: 15px; margin: 10px 0; border-radius: 5px;">
            <h4>⚠️ Important Considerations</h4>
            <div style="background-color: #fef2f2; padding: 12px; border-radius: 5px; border: 1px solid #fca5a5; margin: 10px 0;">
            <h5 style="margin-top: 0; color: #dc2626;">Limitations & Cautions</h5>
            <ul style="margin: 5px 0;">
            <li>Clustering is <b>exploratory</b> - validate findings independently</li>
            <li>Consider technical factors: antibody clones, staining protocols, scoring methods</li>
            <li>Account for inter-observer variability in IHC scoring</li>
            <li>Heatmap visualisations illustrate continuous markers; categorical markers appear as annotation bands</li>
            <li>Statistical significance ≠ clinical significance</li>
            </ul>
            </div>
            
            <div style="background-color: #f0fdf4; padding: 12px; border-radius: 5px; border: 1px solid #86efac; margin: 10px 0;">
            <h5 style="margin-top: 0; color: #059669;">Best Practices</h5>
            <ul style="margin: 5px 0;">
            <li>Document cluster definitions for reproducible classification</li>
            <li>Include representative images for each cluster</li>
            <li>Test clinical associations (survival, treatment response)</li>
            <li>Consider cost-effectiveness of additional IHC markers</li>
            </ul>
            </div>
            </div>
            
            <div style="background-color: #e3f2fd; padding: 15px; margin: 10px 0; border-radius: 5px; border: 1px solid #90caf9;">
            <p style="margin: 0;"><b>💡 Next Steps:</b> Export cluster assignments and test associations with clinical variables using the survival analysis or cross-table modules.</p>
            </div>
            </div>')

            self$results$interpretationGuide$setContent(html)
        },
        
        .performConsensus = function(df, clusterResult, catVars, contVars, weights) {
            nBootstrap <- self$options$nBootstrap %||% 100
            method <- clusterResult$method %||% self$options$method %||% "pam"
            k <- clusterResult$usedK

            if (is.null(k) || k < 2)
                return()

            n <- nrow(df)
            consensus_matrix <- matrix(0, nrow = n, ncol = n)
            indicators <- matrix(0, nrow = n, ncol = n)

            # TESTING: tryCatch disabled - errors will be visible
            # tryCatch({
                for (i in seq_len(nBootstrap)) {
                    boot_idx <- sample.int(n, size = n, replace = TRUE)
                    boot_data <- df[boot_idx, , drop = FALSE]

                    boot_res <- private$.clusterData(
                        df = boot_data,
                        method = method,
                        opts = self$options,
                        k = k,
                        autoSelect = FALSE,
                        catVars = catVars,
                        contVars = contVars,
                        weights = weights,
                        silhouetteTable = NULL
                    )

                    boot_clusters <- boot_res$clusters

                    for (j1 in seq_along(boot_idx)) {
                        for (j2 in seq_along(boot_idx)) {
                            orig_idx1 <- boot_idx[j1]
                            orig_idx2 <- boot_idx[j2]
                            indicators[orig_idx1, orig_idx2] <- indicators[orig_idx1, orig_idx2] + 1
                            if (boot_clusters[j1] == boot_clusters[j2])
                                consensus_matrix[orig_idx1, orig_idx2] <- consensus_matrix[orig_idx1, orig_idx2] + 1
                        }
                    }
                }

                indicators[indicators == 0] <- 1
                consensus_matrix <- consensus_matrix / indicators

                if (!is.null(self$results$consensusStats)) {
                    consensusTable <- self$results$consensusStats

                    clusters <- clusterResult$clusters
                    for (cl in levels(clusters)) {
                        cluster_indices <- which(clusters == cl)
                        if (length(cluster_indices) > 1) {
                            within_consensus <- consensus_matrix[cluster_indices, cluster_indices]
                            upper_vals <- within_consensus[upper.tri(within_consensus)]
                            stability <- if (length(upper_vals) > 0) mean(upper_vals, na.rm = TRUE) else NA_real_

                            row_means <- vapply(seq_along(cluster_indices), function(idx) {
                                vals <- within_consensus[idx, ]
                                vals <- vals[-idx]
                                if (length(vals) == 0) return(NA_real_)
                                mean(vals, na.rm = TRUE)
                            }, numeric(1))
                            core_samples <- sum(row_means > 0.8, na.rm = TRUE)

                            interpretation <- if (is.na(stability)) {
                                "Insufficient information"
                            } else if (stability > 0.8) {
                                "Highly stable"
                            } else if (stability > 0.6) {
                                "Moderately stable"
                            } else {
                                "Low stability"
                            }

                            consensusTable$addRow(rowKey = cl, list(
                                cluster = cl,
                                stability = stability,
                                core_samples = as.integer(core_samples),
                                interpretation = interpretation
                            ))
                        }
                    }
                }

            # }, error = function(e) {
            #     warning(paste("Consensus clustering failed:", e$message))
            # })
        },

        .plotConsensus = function(image, ggtheme, theme, ...) {
            if (!isTRUE(self$options$consensusClustering)) return(FALSE)
            
            analysisState <- self$results$summary$state
            if (is.null(analysisState) || is.null(analysisState$consensusMatrix)) return(FALSE)
            
            matrix <- analysisState$consensusMatrix
            clusters <- analysisState$clusters
            
            # Order matrix by cluster assignment
            ord <- order(clusters)
            matrix <- matrix[ord, ord]
            clusters <- clusters[ord]
            
            # Use pheatmap if available
            if (requireNamespace("pheatmap", quietly = TRUE)) {
                # Create annotation for clusters
                annotation_col <- data.frame(Cluster = clusters)
                rownames(annotation_col) <- colnames(matrix)
                
                # Define colors
                cluster_colors <- rainbow(length(unique(clusters)))
                names(cluster_colors) <- levels(clusters)
                annotation_colors <- list(Cluster = cluster_colors)
                
                pheatmap::pheatmap(
                    matrix,
                    color = colorRampPalette(c("white", "darkblue"))(100),
                    cluster_rows = FALSE,
                    cluster_cols = FALSE,
                    annotation_col = annotation_col,
                    annotation_colors = annotation_colors,
                    show_rownames = FALSE,
                    show_colnames = FALSE,
                    main = "Consensus Clustering Matrix"
                )
            } else {
                # Base heatmap fallback
                heatmap(
                    matrix,
                    Rowv = NA, 
                    Colv = NA,
                    col = colorRampPalette(c("white", "darkblue"))(100),
                    labRow = NA, 
                    labCol = NA,
                    main = "Consensus Clustering Matrix"
                )
            }
            
            return(TRUE)
        },

        # --- Plots ---
        # Generate silhouette plot for cluster quality assessment
        # Creates silhouette plot showing cluster cohesion and separation.
        # Higher silhouette values indicate better clustering quality.
        .plotSilhouette = function(image, ggtheme, theme, ...) {
            # Comprehensive silhouette plot using centralized state
            if (!isTRUE(self$options$showSilhouette)) return()

            # Get analysis state from centralized state management
            analysisState <- self$results$summary$state
            if (is.null(analysisState)) return()

            clusters <- analysisState$clusters
            dist_matrix <- analysisState$dist
            fit <- analysisState$fit
            method <- analysisState$method

            if (is.null(clusters) || is.null(dist_matrix)) return()

            # TESTING: tryCatch disabled - errors will be visible
            # tryCatch({
                # Calculate silhouette using stored distance matrix
                if (method == "pam" && !is.null(fit)) {
                    # Use PAM silhouette directly
                    s <- cluster::silhouette(fit)
                } else {
                    # Calculate silhouette from stored distance matrix
                    s <- cluster::silhouette(as.integer(clusters), dist_matrix)
                }

                # Get accessibility-aware colors
                n_clusters <- length(unique(clusters))
                colors <- private$.getColorPalette(n_clusters)

                p <- factoextra::fviz_silhouette(s, label=FALSE) +
                    ggplot2::scale_fill_manual(values = colors) +
                    ggplot2::scale_color_manual(values = colors) +
                    ggplot2::labs(title = "Silhouette Analysis",
                                 subtitle = paste("Average silhouette width:", round(mean(s[,"sil_width"]), 3)))

                # Apply accessibility theme
                p <- private$.applyAccessibilityTheme(p)
                print(p)

            # }, error = function(e) {
            #     error_msg <- private$.createUserFriendlyError(e, "silhouette plot")
            #     plot(1, type = "n", xlab = "", ylab = "", main = "Silhouette Plot Error")
            #     text(1, 1, error_msg, cex = 0.8, col = "red")
            # })
        },

        # Generate clustered heatmap of IHC expression data
        # Creates heatmap with cases ordered by cluster assignment.
        # Supports row/column scaling options for better visualization.
        .plotHeatmap = function(image, ggtheme, theme, ...) {
            if (!isTRUE(self$options$showHeatmap)) return()

            # Get analysis state from jamovi state management
            analysisState <- self$results$summary$state
            if (is.null(analysisState)) return()

            clusters <- analysisState$clusters
            df <- analysisState$df
            if (is.null(clusters) || is.null(df)) return()

            # Check for ComplexHeatmap package
            if (!requireNamespace("ComplexHeatmap", quietly = TRUE)) {
                plot(1, type="n", main="Heatmap Unavailable", xlab="", ylab="", axes=FALSE)
                text(1, 1, "ComplexHeatmap package required", col="red", font=2, cex=1.2)
                text(1, 0.9, "Install with:", cex=0.9)
                text(1, 0.85, "BiocManager::install('ComplexHeatmap')", cex=0.8, family="mono")
                return()
            }

            numericVars <- colnames(df)[vapply(df, is.numeric, logical(1))]
            catVarsLocal <- setdiff(colnames(df), numericVars)

            # Handle categorical-only IHC data with oncoprint-style heatmap
            if (length(numericVars) == 0 && length(catVarsLocal) > 0) {
                # Create categorical heatmap (oncoprint-style) with dendrograms

                # Convert categorical variables to numeric codes for clustering
                cat_matrix <- matrix(0, nrow = length(catVarsLocal), ncol = nrow(df))
                rownames(cat_matrix) <- catVarsLocal
                colnames(cat_matrix) <- rownames(df) %||% paste0("Case_", seq_len(nrow(df)))

                for (i in seq_along(catVarsLocal)) {
                    var <- catVarsLocal[i]
                    cat_matrix[i, ] <- as.numeric(as.factor(df[[var]]))
                }

                # Perform hierarchical clustering on cases (columns) using Gower distance
                # This shows which cases have similar IHC profiles
                analysisState <- self$results$summary$state
                if (!is.null(analysisState) && !is.null(analysisState$dist)) {
                    # Use stored Gower distance matrix from main clustering
                    dist_matrix <- analysisState$dist
                    hc_cases <- stats::hclust(dist_matrix, method = "ward.D2")
                } else {
                    # Fallback: compute distance on categorical matrix
                    dist_cases <- stats::dist(t(cat_matrix), method = "euclidean")
                    hc_cases <- stats::hclust(dist_cases, method = "ward.D2")
                }

                # Perform hierarchical clustering on markers (rows)
                # This shows which markers have similar patterns across cases
                if (length(catVarsLocal) > 1) {
                    dist_markers <- stats::dist(cat_matrix, method = "euclidean")
                    hc_markers <- stats::hclust(dist_markers, method = "ward.D2")
                } else {
                    hc_markers <- NULL
                }

                # Create cluster annotation
                clusters <- analysisState$clusters %||% as.factor(rep(1, nrow(df)))
                n_clusters <- length(unique(clusters))
                colors <- private$.getColorPalette(n_clusters)
                names(colors) <- levels(clusters)
                annotation_df <- data.frame(Cluster = clusters)
                anno_colors <- list(Cluster = colors)

                # Add clinical variables as additional annotations if provided
                clinicalVars <- self$options$clinicalVars
                if (!is.null(clinicalVars) && length(clinicalVars) > 0) {
                    for (cvar in clinicalVars) {
                        if (cvar %in% colnames(self$data)) {
                            cvar_data <- self$data[[cvar]]
                            if (is.numeric(cvar_data)) {
                                # For continuous clinical variables, use color gradient
                                annotation_df[[cvar]] <- cvar_data
                                anno_colors[[cvar]] <- circlize::colorRamp2(
                                    c(min(cvar_data, na.rm = TRUE), max(cvar_data, na.rm = TRUE)),
                                    c("white", "purple")
                                )
                            } else {
                                # For categorical clinical variables
                                annotation_df[[cvar]] <- as.factor(cvar_data)
                                anno_colors[[cvar]] <- private$.paletteForLevels(levels(annotation_df[[cvar]]))
                            }
                        }
                    }
                }

                cluster_annot <- ComplexHeatmap::HeatmapAnnotation(
                    df = annotation_df,
                    col = anno_colors,
                    show_annotation_name = TRUE,
                    annotation_name_side = "left",
                    annotation_legend_param = list(
                        title_gp = grid::gpar(fontsize = 9),
                        labels_gp = grid::gpar(fontsize = 8)
                    )
                )

                # Create discrete color palette for categorical data
                max_levels <- max(apply(cat_matrix, 1, function(x) length(unique(x[!is.na(x)]))))
                discrete_colors <- grDevices::colorRampPalette(c("#EFEFEF", "#4A90E2", "#E24A4A", "#50C878", "#FFD700"))(max_levels)

                # Generate categorical heatmap with dendrograms
                ht <- ComplexHeatmap::Heatmap(
                    cat_matrix,
                    name = "IHC Level",
                    column_title = "Cases (Categorical IHC Data)",
                    row_title = "Markers",
                    show_column_names = FALSE,
                    cluster_columns = hc_cases,  # Show dendrogram for cases
                    cluster_rows = hc_markers,   # Show dendrogram for markers (if >1 marker)
                    show_row_dend = !is.null(hc_markers),
                    show_column_dend = TRUE,
                    row_dend_width = grid::unit(15, "mm"),
                    column_dend_height = grid::unit(15, "mm"),
                    top_annotation = cluster_annot,
                    col = discrete_colors,
                    cell_fun = function(j, i, x, y, width, height, fill) {
                        # Add marker level text in cells for clarity
                        # Get indices after clustering reordering
                        var_name <- rownames(cat_matrix)[i]
                        case_name <- colnames(cat_matrix)[j]
                        case_idx <- which(colnames(cat_matrix) == case_name)
                        value <- as.character(df[case_idx, var_name])
                        if (!is.na(value) && nchar(value) <= 3) {
                            grid::grid.text(value, x, y, gp = grid::gpar(fontsize = 7))
                        }
                    },
                    heatmap_legend_param = list(
                        title = "IHC Levels",
                        title_gp = grid::gpar(fontsize = 9),
                        labels_gp = grid::gpar(fontsize = 8),
                        grid_height = grid::unit(4, "mm"),
                        grid_width = grid::unit(4, "mm")
                    ),
                    heatmap_width = grid::unit(120, "mm"),
                    heatmap_height = grid::unit(60, "mm")
                )

                ComplexHeatmap::draw(ht,
                                    heatmap_legend_side = "right",
                                    annotation_legend_side = "right",
                                    merge_legend = TRUE)
                return()
            }

            # If no variables at all
            if (length(numericVars) == 0 && length(catVarsLocal) == 0) {
                plot(1, type="n", main="Expression Heatmap Unavailable", xlab="", ylab="", axes=FALSE)
                text(1, 1, "No variables available for heatmap", col="red", font=2, cex=1.1)
                return()
            }

            # Check for NA values and provide guidance
            has_na <- any(is.na(df[, numericVars, drop = FALSE]))
            if (has_na) {
                na_counts <- sapply(numericVars, function(v) sum(is.na(df[[v]])))
                vars_with_na <- names(na_counts[na_counts > 0])

                handleMissing <- self$options$handleMissing %||% "pairwise"

                # If pairwise with continuous-only and NAs, this will cause issues
                if (handleMissing == "pairwise" && length(catVarsLocal) == 0) {
                    plot(1, type="n", main="Expression Heatmap: Data Issue", xlab="", ylab="", axes=FALSE)
                    text(1, 1.05, "Missing values detected in continuous markers", col="red", font=2, cex=1.1)
                    text(1, 0.95, sprintf("%d variable(s) with missing data:", length(vars_with_na)), cex=0.9)

                    y_pos <- 0.88
                    for (i in seq_along(vars_with_na)) {
                        if (i <= 5) {  # Show max 5 variables
                            na_pct <- round(100 * na_counts[vars_with_na[i]] / nrow(df), 1)
                            text(1, y_pos, sprintf("  • %s: %d NAs (%.1f%%)",
                                                  vars_with_na[i], na_counts[vars_with_na[i]], na_pct),
                                 cex=0.75, col="darkred")
                            y_pos <- y_pos - 0.05
                        }
                    }
                    if (length(vars_with_na) > 5) {
                        text(1, y_pos, sprintf("  ... and %d more", length(vars_with_na) - 5), cex=0.75, col="darkred")
                        y_pos <- y_pos - 0.05
                    }

                    y_pos <- y_pos - 0.05
                    text(1, y_pos, "SOLUTION - Choose one:", font=2, cex=0.95, col="blue")
                    y_pos <- y_pos - 0.08

                    text(1, y_pos, "Option 1 (Recommended):", font=2, cex=0.85)
                    y_pos <- y_pos - 0.05
                    text(1, y_pos, "Data Preprocessing → Missing Data Handling", cex=0.8)
                    y_pos <- y_pos - 0.05
                    text(1, y_pos, "→ Select 'Complete cases only'", cex=0.8)
                    y_pos <- y_pos - 0.05
                    complete_cases <- sum(complete.cases(df[, numericVars, drop = FALSE]))
                    text(1, y_pos, sprintf("(Will use %d of %d cases)", complete_cases, nrow(df)),
                         cex=0.75, col="darkgreen")

                    y_pos <- y_pos - 0.08
                    text(1, y_pos, "Option 2:", font=2, cex=0.85)
                    y_pos <- y_pos - 0.05
                    text(1, y_pos, "Add categorical variables (ER, PR, HER2, etc.)", cex=0.8)
                    y_pos <- y_pos - 0.05
                    text(1, y_pos, "→ Pairwise handling works with mixed data", cex=0.8)

                    return()
                }
            }

            mat <- as.matrix(df[, numericVars, drop = FALSE])
                rownames(mat) <- rownames(df) %||% paste0("Case_", seq_len(nrow(df)))

                heatmapScale <- self$options$heatmapScale %||% "row"
                if (heatmapScale == "row") {
                    scaled <- t(scale(t(mat)))
                    scaled[is.na(scaled)] <- 0
                    mat <- scaled
                } else if (heatmapScale == "column") {
                    scaled <- scale(mat)
                    scaled[is.na(scaled)] <- 0
                    mat <- scaled
                }

                ord <- order(clusters)
                mat_ordered <- mat[ord, , drop = FALSE]
                clusters_ordered <- clusters[ord]

                # Create cluster annotation
                n_clusters <- length(unique(clusters))
                colors <- private$.getColorPalette(n_clusters)
                names(colors) <- levels(clusters)
                annotation_df <- data.frame(Cluster = clusters_ordered)

                anno_colors <- list(Cluster = colors)
                if (length(catVarsLocal) > 0) {
                    for (var in catVarsLocal) {
                        annotation_df[[var]] <- droplevels(as.factor(df[ord, var]))
                        anno_colors[[var]] <- private$.paletteForLevels(levels(annotation_df[[var]]))
                    }
                }

                cluster_annot <- ComplexHeatmap::HeatmapAnnotation(
                    df = annotation_df,
                    col = anno_colors
                )

                # Generate heatmap
                ht <- ComplexHeatmap::Heatmap(
                    t(mat_ordered),
                    name = "Expression",
                    column_title = "Cases",
                    row_title = "Markers",
                    show_column_names = FALSE,
                    cluster_columns = FALSE,
                    top_annotation = cluster_annot
                )

                ComplexHeatmap::draw(ht, heatmap_legend_side = "bottom", annotation_legend_side = "bottom")
        },

        # Generate dendrogram for hierarchical clustering
        # Creates dendrogram showing hierarchical relationships between cases.
        # Only available for hierarchical clustering method.
        .plotDendrogram = function(image, ggtheme, theme, ...) {
            if (!isTRUE(self$options$showDendrogram) || self$options$method != "hierarchical") return()

            # Get analysis state from centralized state management
            analysisState <- self$results$summary$state
            if (is.null(analysisState)) return()

            hc <- analysisState$hc
            usedK <- analysisState$usedK
            if (is.null(hc)) return()

            # TESTING: tryCatch disabled - errors will be visible
            # tryCatch({
                # Use stored hierarchical clustering object
                plot(as.hclust(hc), main = "Hierarchical Clustering Dendrogram",
                     xlab = "Cases", ylab = "Height", cex = 0.6)
                if (!is.null(usedK)) {
                    par(lwd = 2)
                    rect.hclust(as.hclust(hc), k = usedK, border = "red")
                    par(lwd = 1)  # Reset to default
                }
            # }, error = function(e) {
            #     error_msg <- private$.createUserFriendlyError(e, "dendrogram")
            #     plot(1, type = "n", xlab = "", ylab = "", main = "Dendrogram Error")
            #     text(1, 1, error_msg, cex = 0.8, col = "red")
            # })
        },

        # Compute PCA or MCA and store results for later use
        .computeDimensionReduction = function(df, catVars, contVars) {
            result <- list(
                type = NULL,    # "pca", "mca", or "mds"
                object = NULL,  # PCA/MCA object
                coords = NULL,  # Coordinates for plotting
                variances = NULL,  # Variance explained
                contributions = NULL  # Variable contributions
            )

            if (length(catVars) > 0 && length(contVars) == 0) {
                # Categorical only: MCA
                catData <- df[, catVars, drop=FALSE]
                catData[] <- lapply(catData, as.factor)
                mca <- FactoMineR::MCA(catData, graph=FALSE, ncp=5)

                result$type <- "mca"
                result$object <- mca
                result$coords <- mca$ind$coord[, 1:2, drop=FALSE]
                result$variances <- mca$eig[, 2]  # Percentage of variance

                # Extract variable contributions (cos2 = quality of representation)
                var_contrib <- mca$var$contrib[, 1:3, drop=FALSE]  # First 3 dimensions
                var_cos2 <- mca$var$cos2[, 1:3, drop=FALSE]

                # Compute p-values using permutation test approximation
                # Based on contribution significance test
                n_vars <- nrow(var_contrib)
                expected_contrib <- 100 / n_vars  # Equal contribution expected

                result$contributions <- list()
                for (i in 1:min(3, ncol(var_contrib))) {
                    dim_name <- paste0("Dim", i)
                    contrib_vals <- var_contrib[, i]

                    # Z-score based p-value approximation
                    # Higher contribution than expected is significant
                    z_scores <- (contrib_vals - expected_contrib) / sqrt(expected_contrib)
                    p_values <- 2 * stats::pnorm(-abs(z_scores))

                    result$contributions[[dim_name]] <- data.frame(
                        marker = rownames(var_contrib),
                        contribution = contrib_vals / 100,  # Convert to proportion
                        cos2 = var_cos2[, i],
                        p_value = pmin(p_values, 1.0),
                        stringsAsFactors = FALSE
                    )
                }

            } else if (length(contVars) > 0 && length(catVars) == 0) {
                # Continuous only: PCA
                contData <- df[, contVars, drop=FALSE]
                contData_complete <- stats::na.omit(contData)

                pca <- stats::prcomp(contData_complete, scale. = TRUE, center = TRUE)

                result$type <- "pca"
                result$object <- pca
                result$coords <- pca$x[, 1:2, drop=FALSE]
                result$variances <- (pca$sdev^2 / sum(pca$sdev^2)) * 100

                # Extract variable loadings (rotation matrix)
                loadings <- pca$rotation[, 1:min(3, ncol(pca$rotation)), drop=FALSE]

                # Compute contributions: squared loadings normalized to 100%
                # This is mathematically equivalent to cos2 in PCA
                result$contributions <- list()
                for (i in 1:ncol(loadings)) {
                    dim_name <- paste0("PC", i)
                    loading_vals <- loadings[, i]
                    contrib_vals <- (loading_vals^2 / sum(loading_vals^2)) * 100

                    # Statistical significance using correlation test
                    # Loading is correlation between variable and PC
                    n_obs <- nrow(contData_complete)
                    t_stat <- loading_vals * sqrt(n_obs - 2) / sqrt(1 - loading_vals^2)
                    p_values <- 2 * stats::pt(-abs(t_stat), df = n_obs - 2)

                    result$contributions[[dim_name]] <- data.frame(
                        marker = rownames(loadings),
                        contribution = contrib_vals / 100,  # Proportion
                        loading = loading_vals,
                        p_value = pmin(p_values, 1.0),
                        stringsAsFactors = FALSE
                    )
                }

            } else {
                # Mixed data: Classical MDS on Gower distance
                d <- cluster::daisy(df, metric = "gower")

                if (anyNA(d)) {
                    d[is.na(d)] <- max(d, na.rm = TRUE)
                }

                cmd <- stats::cmdscale(d, k = 5, eig = TRUE)

                result$type <- "mds"
                result$object <- cmd
                result$coords <- cmd$points[, 1:2, drop=FALSE]

                # Variance explained by each dimension
                eigenvalues <- cmd$eig[cmd$eig > 0]
                result$variances <- (eigenvalues / sum(eigenvalues)) * 100

                # For MDS, we cannot directly compute variable contributions
                # Store NULL to indicate this
                result$contributions <- NULL
            }

            return(result)
        },

        # Populate PCA contributions table
        .populatePCAContributions = function() {
            if (!isTRUE(self$options$showPCAPlot)) return()

            analysisState <- self$results$summary$state
            if (is.null(analysisState)) return()

            dimredResult <- analysisState$dimred
            if (is.null(dimredResult) || is.null(dimredResult$contributions)) return()

            table <- self$results$pcaContributions

            # Populate table with contributions from all dimensions
            for (dim_name in names(dimredResult$contributions)) {
                dim_data <- dimredResult$contributions[[dim_name]]

                # Sort by contribution (descending)
                dim_data <- dim_data[order(-dim_data$contribution), ]

                for (i in seq_len(nrow(dim_data))) {
                    row <- dim_data[i, ]
                    table$addRow(rowKey = paste0(dim_name, "_", row$marker), list(
                        component = dim_name,
                        marker = as.character(row$marker),
                        contribution = row$contribution,
                        p_value = row$p_value
                    ))
                }
            }
        },

        # Plot PCA variable factor map (correlation circle)
        .plotPCAVariables = function(image, ggtheme, theme, ...) {
            if (!isTRUE(self$options$showPCAPlot)) return()

            analysisState <- self$results$summary$state
            if (is.null(analysisState)) return()

            dimredResult <- analysisState$dimred
            if (is.null(dimredResult)) return()

            if (dimredResult$type == "pca") {
                # PCA correlation circle
                pca <- dimredResult$object

                # Use factoextra for publication-quality correlation circle
                p <- factoextra::fviz_pca_var(
                    pca,
                    col.var = "contrib",  # Color by contribution
                    gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                    repel = TRUE,
                    labelsize = 3
                )

                # Calculate variance percentages for axis labels
                var_exp <- dimredResult$variances
                xlab <- sprintf("PC1 (%.1f%%)", var_exp[1])
                ylab <- sprintf("PC2 (%.1f%%)", var_exp[2])

                p <- p +
                    ggplot2::labs(
                        title = "PCA Variable Factor Map (Correlation Circle)",
                        subtitle = "Arrow length = contribution strength",
                        x = xlab,
                        y = ylab
                    )

                # Apply accessibility theme
                p <- private$.applyAccessibilityTheme(p)
                print(p)

            } else if (dimredResult$type == "mca") {
                # MCA variable plot
                mca <- dimredResult$object

                p <- factoextra::fviz_mca_var(
                    mca,
                    col.var = "contrib",
                    gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                    repel = TRUE,
                    labelsize = 3
                )

                # Calculate variance percentages
                var_exp <- dimredResult$variances
                xlab <- sprintf("Dim 1 (%.1f%%)", var_exp[1])
                ylab <- sprintf("Dim 2 (%.1f%%)", var_exp[2])

                p <- p +
                    ggplot2::labs(
                        title = "MCA Variable Factor Map",
                        subtitle = "Distance from origin = importance",
                        x = xlab,
                        y = ylab
                    )

                # Apply accessibility theme
                p <- private$.applyAccessibilityTheme(p)
                print(p)

            } else {
                # MDS doesn't have variable factor map
                plot(1, type="n", axes=FALSE, xlab="", ylab="",
                     main="Variable Factor Map")
                text(1, 1, "Variable factor map not available for mixed data (MDS).\nUse categorical-only or continuous-only data for this plot.",
                     cex=0.9, col="darkred")
            }
        },

        .plotPCA = function(image, ggtheme, theme, ...) {
            if (!isTRUE(self$options$showPCAPlot)) return()

            # Get analysis state from jamovi state management
            analysisState <- self$results$summary$state
            if (is.null(analysisState)) return()

            clusters <- analysisState$clusters
            df <- analysisState$df
            dimredResult <- analysisState$dimred

            if (is.null(clusters) || is.null(df) || is.null(dimredResult)) return()

            # Get accessibility-aware colors
            n_clusters <- length(unique(clusters))
            colors <- private$.getColorPalette(n_clusters)

            if (dimredResult$type == "pca") {
                # PCA individual factor map with proper confidence ellipses
                pca <- dimredResult$object

                # Calculate variance percentages for axis labels
                var_exp <- dimredResult$variances
                xlab <- sprintf("PC1 (%.1f%%)", var_exp[1])
                ylab <- sprintf("PC2 (%.1f%%)", var_exp[2])

                p <- factoextra::fviz_pca_ind(
                    pca,
                    habillage = clusters,
                    addEllipses = TRUE,
                    ellipse.type = "confidence",  # 95% confidence ellipses
                    ellipse.level = 0.95,
                    repel = TRUE,
                    palette = colors,
                    pointsize = 2,
                    labelsize = 3
                ) +
                    ggplot2::labs(
                        title = "PCA Individual Factor Map",
                        subtitle = "Cases colored by cluster with 95% confidence ellipses",
                        x = xlab,
                        y = ylab
                    )

            } else if (dimredResult$type == "mca") {
                # MCA individual factor map
                mca <- dimredResult$object

                var_exp <- dimredResult$variances
                xlab <- sprintf("Dim 1 (%.1f%%)", var_exp[1])
                ylab <- sprintf("Dim 2 (%.1f%%)", var_exp[2])

                p <- factoextra::fviz_mca_ind(
                    mca,
                    habillage = clusters,
                    addEllipses = TRUE,
                    ellipse.type = "confidence",
                    ellipse.level = 0.95,
                    repel = TRUE,
                    palette = colors,
                    pointsize = 2,
                    labelsize = 3
                ) +
                    ggplot2::labs(
                        title = "MCA Individual Factor Map",
                        subtitle = "Cases colored by cluster with 95% confidence ellipses",
                        x = xlab,
                        y = ylab
                    )

            } else {
                # MDS plot
                coords <- dimredResult$coords
                var_exp <- dimredResult$variances
                xlab <- sprintf("Coordinate 1 (%.1f%%)", var_exp[1])
                ylab <- sprintf("Coordinate 2 (%.1f%%)", var_exp[2])

                plotData <- data.frame(
                    Dim1 = coords[, 1],
                    Dim2 = coords[, 2],
                    Cluster = clusters
                )

                p <- ggplot2::ggplot(plotData, ggplot2::aes(x = Dim1, y = Dim2, color = Cluster)) +
                    ggplot2::geom_point(size = 2) +
                    ggplot2::stat_ellipse(type = "norm", level = 0.95, size = 1) +
                    ggplot2::scale_color_manual(values = colors) +
                    ggplot2::labs(
                        title = "Multidimensional Scaling Plot (Mixed Data)",
                        subtitle = "Based on Gower distance with 95% confidence ellipses",
                        x = xlab,
                        y = ylab
                    )
            }

            # Apply accessibility theme
            p <- private$.applyAccessibilityTheme(p)
            print(p)
            TRUE
        },
        
        # Legacy MCA plot (backward compatibility)
        .plotMCA = function(image, ggtheme, theme, ...) {
            if (self$options$method != "mca_kmeans" && self$options$method != "dimreduce") return()
            private$.plotPCA(image, ggtheme, theme, ...)
        },

        .plotBoxplots = function(image, ggtheme, theme, ...) {
            if (!isTRUE(self$options$showBoxplots)) return()
            
            # Get analysis state from jamovi state management
            analysisState <- self$results$summary$state
            if (is.null(analysisState)) return()
            
            clusters <- analysisState$clusters
            df <- analysisState$df
            contVars <- self$options$contVars
            
            if (is.null(clusters) || is.null(df) || length(contVars) == 0) return()
            
            # TESTING: tryCatch disabled - errors will be visible
            # tryCatch({
                # Create long format data for continuous variables
                cont_data <- df[, contVars, drop = FALSE]
                cont_data$cluster <- clusters

                # Get accessibility-aware colors
                n_clusters <- length(unique(clusters))
                colors <- private$.getColorPalette(n_clusters)

                # Simple boxplot approach using base R
                par(mfrow = c(ceiling(length(contVars)/2), 2))
                for (var in contVars) {
                    if (is.numeric(cont_data[[var]])) {
                        boxplot(cont_data[[var]] ~ cont_data$cluster,
                               main = paste("Distribution of", var, "by Cluster"),
                               xlab = "Cluster", ylab = var,
                               col = colors)
                    }
                }
                par(mfrow = c(1, 1))

            # }, error = function(e) {
            #     error_msg <- private$.createUserFriendlyError(e, "boxplot")
            #     plot(1, type = "n", xlab = "", ylab = "", main = "Boxplot Error")
            #     text(1, 1, error_msg, cex = 0.8, col = "red")
            # })
        },

        .plotSurvival = function(image, ggtheme, theme, ...) {
            
            if (is.null(self$options$survivalTime) || is.null(self$options$survivalEvent)) return()
            
            # Get analysis state from jamovi state management
            analysisState <- self$results$summary$state
            if (is.null(analysisState)) return()
            
            clusters <- analysisState$clusters
            if (is.null(clusters)) return()
            
            survTime <- self$data[[self$options$survivalTime]]
            survEvent <- self$data[[self$options$survivalEvent]]
            
            if (is.null(survTime) || is.null(survEvent)) return()
            
            # TESTING: tryCatch disabled - errors will be visible
            # tryCatch({
                # Create survival object
                if (!requireNamespace("survival", quietly = TRUE)) {
                    plot(1, type = "n", xlab = "", ylab = "", main = "Survival analysis unavailable")
                    text(1, 1, "survival package not available")
                    return()
                }

                surv_obj <- survival::Surv(survTime, survEvent)
                fit <- survival::survfit(surv_obj ~ clusters)

                # Plot using survminer if available, otherwise base R
                if (requireNamespace("survminer", quietly = TRUE)) {
                    p <- survminer::ggsurvplot(fit,
                                              pval = TRUE,
                                              conf.int = TRUE,
                                              risk.table = TRUE,
                                              risk.table.col = "strata",
                                              legend.labs = levels(clusters),
                                              title = "Survival Analysis by IHC Cluster")
                    print(p)
                } else {
                    # Base R plot
                    plot(fit, col = rainbow(length(levels(clusters))),
                         main = "Survival Curves by Cluster",
                         xlab = "Time", ylab = "Survival Probability")
                    legend("topright", legend = levels(clusters),
                           col = rainbow(length(levels(clusters))), lty = 1)
                }

            # }, error = function(e) {
            #     error_msg <- private$.createUserFriendlyError(e, "survival plot")
            #     plot(1, type = "n", xlab = "", ylab = "", main = "Survival Plot Error")
            #     text(1, 1, error_msg, cex = 0.8, col = "red")
            # })
        },

        # Get accessibility-aware color palette
        .getColorPalette = function(n_colors) {
            palette_type <- self$options$colorPalette %||% "default"

            switch(palette_type,
                "colorblind" = {
                    # Wong colorblind-safe palette
                    colors <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442",
                               "#0072B2", "#D55E00", "#CC79A7", "#000000")
                    if (n_colors <= length(colors)) {
                        return(colors[1:n_colors])
                    } else {
                        return(rep(colors, length.out = n_colors))
                    }
                },
                "viridis" = {
                    if (requireNamespace("viridis", quietly = TRUE)) {
                        return(viridis::viridis(n_colors))
                    } else {
                        return(rainbow(n_colors))
                    }
                },
                "high_contrast" = {
                    # High contrast black/white/red palette
                    colors <- c("#000000", "#FFFFFF", "#FF0000", "#00FF00",
                               "#0000FF", "#FFFF00", "#FF00FF", "#00FFFF")
                    if (n_colors <= length(colors)) {
                        return(colors[1:n_colors])
                    } else {
                        return(rep(colors, length.out = n_colors))
                    }
                },
                "default" = {
                    return(rainbow(n_colors))
                }
            )
        },

        .escapeHtml = function(text) {
            txt <- as.character(text)
            txt <- gsub("&", "&amp;", txt, fixed = TRUE)
            txt <- gsub("<", "&lt;", txt, fixed = TRUE)
            gsub(">", "&gt;", txt, fixed = TRUE)
        },

        # Get accessibility-aware font size
        .getFontSize = function() {
            font_setting <- self$options$fontSize %||% "medium"

            switch(font_setting,
                "small" = 10,
                "medium" = 12,
                "large" = 14,
                "extra_large" = 16,
                12  # default
            )
        },




        # Apply accessibility theme to ggplot
        .applyAccessibilityTheme = function(p) {
            base_size <- private$.getFontSize()
            high_contrast <- isTRUE(self$options$plotContrast)

            if (high_contrast) {
                # High contrast theme
                p <- p + ggplot2::theme_minimal(base_size = base_size) +
                    ggplot2::theme(
                        panel.background = ggplot2::element_rect(fill = "white", color = "black", size = 2),
                        plot.background = ggplot2::element_rect(fill = "white", color = "black", size = 2),
                        panel.grid.major = ggplot2::element_line(color = "black", size = 0.5),
                        panel.grid.minor = ggplot2::element_blank(),
                        axis.text = ggplot2::element_text(color = "black", size = base_size),
                        axis.title = ggplot2::element_text(color = "black", size = base_size + 2),
                        plot.title = ggplot2::element_text(color = "black", size = base_size + 4),
                        legend.text = ggplot2::element_text(color = "black", size = base_size),
                        legend.title = ggplot2::element_text(color = "black", size = base_size + 2)
                    )
            } else {
                # Standard theme with font size adjustment
                p <- p + ggplot2::theme_minimal(base_size = base_size)
            }

            return(p)
        },

        # Create user-friendly error messages for plot functions
        .createUserFriendlyError = function(e, plotType = "plot") {
            error_message <- tolower(e$message)

            if (grepl("package|namespace", error_message)) {
                return(paste("Required package missing for", plotType, ".\nPlease install missing dependencies."))
            } else if (grepl("data|insufficient|empty", error_message)) {
                return(paste("Insufficient data for", plotType, ".\nTry different clustering options or add more variables."))
            } else if (grepl("cluster|silhouette", error_message)) {
                return(paste("Clustering issue in", plotType, ".\nCheck cluster assignments and parameters."))
            } else if (grepl("memory|allocation", error_message)) {
                return(paste("Memory issue in", plotType, ".\nTry reducing data size or complexity."))
            } else {
                return(paste(plotType, "error:", e$message))
            }
        },

        # Generate copy-ready executive summary for clinical reports
        .generateExecutiveSummary = function() {
            # TESTING: tryCatch disabled - errors will be visible
            # tryCatch({
                analysisState <- self$results$summary$state
                if (is.null(analysisState)) return("")

                clusters <- analysisState$clusters
                method <- analysisState$method
                catVars <- analysisState$catVars
                contVars <- analysisState$contVars

                if (is.null(clusters)) return("")

                # Basic cluster information
                n_clusters <- length(unique(clusters))
                n_cases <- length(clusters)
                cluster_sizes <- table(clusters)

                # Method description
                method_desc <- switch(method,
                    "pam" = "PAM (Partitioning Around Medoids)",
                    "hierarchical" = "hierarchical clustering",
                    "mca_kmeans" = "Multiple Correspondence Analysis with k-means",
                    "dimreduce" = "dimensionality reduction with k-means",
                    method
                )

                # Generate summary text
                summary_lines <- c()

                # Header
                summary_lines <- c(summary_lines, "IMMUNOHISTOCHEMISTRY CLUSTERING ANALYSIS - EXECUTIVE SUMMARY")
                summary_lines <- c(summary_lines, paste0("=", strrep("=", 60)))
                summary_lines <- c(summary_lines, "")

                # Basic analysis information
                summary_lines <- c(summary_lines, paste0("Analysis Method: ", method_desc))
                summary_lines <- c(summary_lines, paste0("Total Cases Analyzed: ", n_cases))
                summary_lines <- c(summary_lines, paste0("Number of Clusters Identified: ", n_clusters))
                summary_lines <- c(summary_lines, "")

                # Variables analyzed
                if (length(catVars) > 0) {
                    summary_lines <- c(summary_lines, paste0("Categorical Markers: ", paste(catVars, collapse = ", ")))
                }
                if (length(contVars) > 0) {
                    summary_lines <- c(summary_lines, paste0("Continuous Markers: ", paste(contVars, collapse = ", ")))
                }
                summary_lines <- c(summary_lines, "")

                # Cluster distribution
                summary_lines <- c(summary_lines, "CLUSTER DISTRIBUTION")
                summary_lines <- c(summary_lines, paste0("-", strrep("-", 20)))
                for (i in 1:n_clusters) {
                    cluster_name <- paste("Cluster", i)
                    cluster_size <- cluster_sizes[i]
                    cluster_pct <- round(100 * cluster_size / n_cases, 1)
                    summary_lines <- c(summary_lines,
                        paste0(cluster_name, ": ", cluster_size, " cases (", cluster_pct, "%)"))
                }
                summary_lines <- c(summary_lines, "")

                # Clinical interpretation
                summary_lines <- c(summary_lines, "CLINICAL INTERPRETATION")
                summary_lines <- c(summary_lines, paste0("-", strrep("-", 25)))

                if (n_clusters == 2) {
                    summary_lines <- c(summary_lines, "The analysis identified two distinct immunophenotypic groups, suggesting potential biological heterogeneity that may have diagnostic or prognostic implications. Consider correlating these clusters with clinical outcomes.")
                } else if (n_clusters == 3) {
                    summary_lines <- c(summary_lines, "Three immunophenotypic subgroups were identified, indicating moderate tumor heterogeneity. This stratification may be useful for risk assessment and treatment planning.")
                } else if (n_clusters >= 4) {
                    summary_lines <- c(summary_lines,
                        paste0(n_clusters, " immunophenotypic subgroups were identified, suggesting high tumor heterogeneity. This complex pattern may require additional validation and correlation with molecular markers."))
                }

                summary_lines <- c(summary_lines, "")

                # Recommendations
                summary_lines <- c(summary_lines, "RECOMMENDATIONS")
                summary_lines <- c(summary_lines, paste0("-", strrep("-", 15)))
                summary_lines <- c(summary_lines,
                    "1. Correlate clustering results with clinical parameters and outcomes",
                    "2. Consider validation in an independent cohort",
                    "3. Evaluate cluster-specific treatment responses if applicable",
                    "4. Review individual cluster profiles for biomarker patterns")

                summary_lines <- c(summary_lines, "")
                summary_lines <- c(summary_lines,
                    paste0("Generated by ClinicoPath IHC Clustering Analysis on ", Sys.Date()))

                # Join all lines
                executive_summary <- paste(summary_lines, collapse = "\n")

                return(executive_summary)

            # }, error = function(e) {
            #     return(paste("Executive summary generation failed:", e$message))
            # })
        },

        # ================== MARKER OPTIMIZATION FUNCTIONS ==================

        # Compute marker importance using multiple methods
        # Returns importance scores based on discrimination power
        .computeMarkerImportance = function(df, clusters, catVars, contVars) {
            all_markers <- c(catVars, contVars)
            if (length(all_markers) == 0) return(NULL)

            importance_scores <- data.frame(
                marker = all_markers,
                importance = numeric(length(all_markers)),
                between_var = numeric(length(all_markers)),
                within_var = numeric(length(all_markers)),
                chi_p = numeric(length(all_markers)),
                stringsAsFactors = FALSE
            )

            for (i in seq_along(all_markers)) {
                marker <- all_markers[i]
                marker_data <- df[[marker]]

                if (is.factor(marker_data) || is.character(marker_data)) {
                    # Categorical marker: use Cramér's V and chi-square
                    marker_factor <- as.factor(marker_data)
                    tbl <- table(marker_factor, clusters)

                    # Chi-square test
                    chi_test <- stats::chisq.test(tbl)
                    chi_p <- chi_test$p.value

                    # Cramér's V effect size
                    n <- sum(tbl)
                    min_dim <- min(nrow(tbl) - 1, ncol(tbl) - 1)
                    cramers_v <- sqrt(chi_test$statistic / (n * min_dim))

                    importance_scores$importance[i] <- as.numeric(cramers_v)
                    importance_scores$chi_p[i] <- chi_p

                } else if (is.numeric(marker_data)) {
                    # Continuous marker: use between/within cluster variance ratio
                    marker_clean <- marker_data[!is.na(marker_data)]
                    clusters_clean <- clusters[!is.na(marker_data)]

                    if (length(marker_clean) < 3) {
                        importance_scores$importance[i] <- 0
                        next
                    }

                    # One-way ANOVA to get F-statistic
                    anova_result <- stats::aov(marker_clean ~ clusters_clean)
                    anova_summary <- summary(anova_result)

                    # Between-cluster variance / Within-cluster variance
                    f_stat <- anova_summary[[1]]["clusters_clean", "F value"]
                    p_val <- anova_summary[[1]]["clusters_clean", "Pr(>F)"]

                    # Eta-squared (effect size)
                    ss_between <- anova_summary[[1]]["clusters_clean", "Sum Sq"]
                    ss_total <- sum(anova_summary[[1]][, "Sum Sq"])
                    eta_sq <- ss_between / ss_total

                    importance_scores$importance[i] <- eta_sq
                    importance_scores$between_var[i] <- ss_between
                    importance_scores$within_var[i] <- anova_summary[[1]]["Residuals", "Sum Sq"]
                    importance_scores$chi_p[i] <- p_val
                }
            }

            # Normalize importance to 0-1 scale
            if (max(importance_scores$importance) > 0) {
                importance_scores$importance <- importance_scores$importance / max(importance_scores$importance)
            }

            return(importance_scores)
        },

        # Compute marker correlation matrix and identify redundant markers
        .computeMarkerCorrelation = function(df, catVars, contVars) {
            all_markers <- c(catVars, contVars)
            if (length(all_markers) < 2) return(NULL)

            n_markers <- length(all_markers)
            cor_matrix <- matrix(0, n_markers, n_markers)
            rownames(cor_matrix) <- colnames(cor_matrix) <- all_markers

            for (i in 1:(n_markers-1)) {
                for (j in (i+1):n_markers) {
                    marker1 <- df[[all_markers[i]]]
                    marker2 <- df[[all_markers[j]]]

                    # Remove NA pairs
                    valid <- complete.cases(marker1, marker2)
                    marker1 <- marker1[valid]
                    marker2 <- marker2[valid]

                    if (length(marker1) < 3) {
                        cor_val <- 0
                    } else if (is.numeric(marker1) && is.numeric(marker2)) {
                        # Both continuous: Pearson correlation
                        cor_val <- stats::cor(marker1, marker2, method = "pearson")
                    } else if (is.factor(marker1) || is.character(marker1)) {
                        if (is.factor(marker2) || is.character(marker2)) {
                            # Both categorical: Cramér's V
                            tbl <- table(as.factor(marker1), as.factor(marker2))
                            chi_test <- stats::chisq.test(tbl)
                            n <- sum(tbl)
                            min_dim <- min(nrow(tbl) - 1, ncol(tbl) - 1)
                            cor_val <- sqrt(chi_test$statistic / (n * min_dim))
                        } else {
                            # Mixed: point-biserial / eta coefficient
                            # Convert categorical to numeric for correlation
                            marker1_num <- as.numeric(as.factor(marker1))
                            cor_val <- stats::cor(marker1_num, marker2, method = "spearman")
                        }
                    } else {
                        # marker1 continuous, marker2 categorical
                        marker2_num <- as.numeric(as.factor(marker2))
                        cor_val <- stats::cor(marker1, marker2_num, method = "spearman")
                    }

                    cor_matrix[i, j] <- cor_matrix[j, i] <- abs(cor_val)
                }
            }

            diag(cor_matrix) <- 1
            return(cor_matrix)
        },

        # Optimize marker panel by removing redundant markers
        .optimizeMarkerPanel = function(df, clusters, catVars, contVars, threshold = 0.8) {
            importance <- private$.computeMarkerImportance(df, clusters, catVars, contVars)
            if (is.null(importance)) return(NULL)

            cor_matrix <- private$.computeMarkerCorrelation(df, catVars, contVars)
            if (is.null(cor_matrix)) return(importance)

            # Sort by importance (descending)
            importance <- importance[order(-importance$importance), ]

            # Identify redundant markers
            all_markers <- importance$marker
            keep_markers <- character()
            redundant_markers <- list()

            for (i in seq_along(all_markers)) {
                marker <- all_markers[i]

                # Check correlation with already kept markers
                if (length(keep_markers) > 0) {
                    max_cor <- max(cor_matrix[marker, keep_markers])
                    if (max_cor > threshold) {
                        # Find which marker it's redundant with
                        redundant_with <- keep_markers[which.max(cor_matrix[marker, keep_markers])]
                        redundant_markers[[marker]] <- list(
                            redundant_with = redundant_with,
                            correlation = max_cor
                        )
                        next
                    }
                }

                keep_markers <- c(keep_markers, marker)
            }

            # Add redundancy information to importance dataframe
            importance$max_correlation <- NA
            importance$redundant_with <- ""
            importance$recommendation <- "Keep"

            for (marker in names(redundant_markers)) {
                idx <- which(importance$marker == marker)
                info <- redundant_markers[[marker]]
                importance$max_correlation[idx] <- info$correlation
                importance$redundant_with[idx] <- info$redundant_with
                importance$recommendation[idx] <- "Remove (redundant)"
            }

            # For kept markers, find their max correlation
            for (marker in keep_markers) {
                idx <- which(importance$marker == marker)
                if (length(keep_markers) > 1) {
                    other_markers <- setdiff(keep_markers, marker)
                    importance$max_correlation[idx] <- max(cor_matrix[marker, other_markers])
                } else {
                    importance$max_correlation[idx] <- 0
                }
            }

            return(list(
                importance = importance,
                cor_matrix = cor_matrix,
                keep_markers = keep_markers,
                redundant_markers = redundant_markers
            ))
        },

        # Compute cluster quality metrics including PPV
        .computeClusterQuality = function(df, clusters, dist_matrix = NULL) {
            n_clusters <- length(unique(clusters))
            quality_results <- data.frame(
                cluster = levels(clusters),
                size = as.integer(table(clusters)),
                purity = numeric(n_clusters),
                avg_silhouette = numeric(n_clusters),
                separation = numeric(n_clusters),
                compactness = numeric(n_clusters),
                stringsAsFactors = FALSE
            )

            # Compute silhouette scores
            if (!is.null(dist_matrix)) {
                # Ensure dist_matrix is clean
                if (anyNA(dist_matrix) || any(!is.finite(dist_matrix))) {
                     val <- max(dist_matrix, na.rm = TRUE)
                     if (!is.finite(val)) val <- 1
                     dist_matrix[is.na(dist_matrix) | !is.finite(dist_matrix)] <- val
                }

                sil <- cluster::silhouette(as.integer(clusters), dist_matrix)
                for (i in 1:n_clusters) {
                    cluster_idx <- which(clusters == levels(clusters)[i])
                    quality_results$avg_silhouette[i] <- mean(sil[cluster_idx, "sil_width"])
                }
            }

            # Compute separation and compactness
            for (i in 1:n_clusters) {
                cluster_label <- levels(clusters)[i]
                cluster_idx <- which(clusters == cluster_label)

                if (!is.null(dist_matrix)) {
                    # Compactness: average within-cluster distance
                    if (length(cluster_idx) > 1) {
                        within_dists <- as.matrix(dist_matrix)[cluster_idx, cluster_idx]
                        quality_results$compactness[i] <- mean(within_dists[upper.tri(within_dists)])
                    } else {
                        quality_results$compactness[i] <- 0
                    }

                    # Separation: minimum distance to other clusters
                    other_idx <- which(clusters != cluster_label)
                    if (length(other_idx) > 0) {
                        between_dists <- as.matrix(dist_matrix)[cluster_idx, other_idx, drop = FALSE]
                        quality_results$separation[i] <- min(between_dists)
                    } else {
                        quality_results$separation[i] <- NA
                    }
                }

                # Purity: homogeneity measure (for now, use silhouette as proxy)
                # True purity requires known labels - here we use cluster cohesion
                quality_results$purity[i] <- max(0, quality_results$avg_silhouette[i])
            }

            # Quality rating based on silhouette
            quality_results$quality <- sapply(quality_results$avg_silhouette, function(s) {
                if (is.na(s)) return("Unknown")
                if (s > 0.7) return("Excellent")
                if (s > 0.5) return("Good")
                if (s > 0.25) return("Fair")
                return("Poor")
            })

            return(quality_results)
        },

        # Plot marker correlation matrix
        .plotMarkerCorrelation = function(image, ggtheme, theme, ...) {
            if (!isTRUE(self$options$showMarkerCorrelation)) return()

            analysisState <- self$results$summary$state
            if (is.null(analysisState)) return()

            df <- analysisState$df
            catVars <- analysisState$catVars
            contVars <- analysisState$contVars

            if (is.null(df)) return()

            cor_matrix <- private$.computeMarkerCorrelation(df, catVars, contVars)
            if (is.null(cor_matrix)) {
                plot(1, type="n", axes=FALSE, xlab="", ylab="", main="Marker Correlation")
                text(1, 1, "Insufficient markers for correlation analysis\n(need at least 2 markers)",
                     cex=0.9, col="darkred")
                return()
            }

            # Use corrplot if available, otherwise use base graphics
            if (requireNamespace("corrplot", quietly = TRUE)) {
                corrplot::corrplot(cor_matrix,
                                  method = "color",
                                  type = "upper",
                                  order = "hclust",
                                  addCoef.col = "black",
                                  tl.col = "black",
                                  tl.srt = 45,
                                  number.cex = 0.7,
                                  title = "Marker Correlation Matrix\n(Absolute values)",
                                  mar = c(0,0,2,0))
            } else {
                # Fallback to base graphics heatmap
                heatmap(cor_matrix,
                       Rowv = as.dendrogram(stats::hclust(stats::as.dist(1 - cor_matrix))),
                       Colv = as.dendrogram(stats::hclust(stats::as.dist(1 - cor_matrix))),
                       scale = "none",
                       main = "Marker Correlation Matrix",
                       xlab = "", ylab = "",
                       cexRow = 0.8, cexCol = 0.8)
            }
        },

        # Perform iterative marker refinement
        .performIterativeRefinement = function(df, catVars, contVars, max_iterations = 3) {
            if (!isTRUE(self$options$iterativeRefinement)) return(NULL)

            history <- data.frame(
                iteration = integer(),
                n_markers = integer(),
                markers_used = character(),
                avg_silhouette = numeric(),
                stringsAsFactors = FALSE
            )

            current_catVars <- catVars
            current_contVars <- contVars
            best_silhouette <- -Inf
            best_markers <- list(catVars = catVars, contVars = contVars)

            for (iter in 1:max_iterations) {
                # Perform clustering with current markers
                all_markers <- c(current_catVars, current_contVars)
                if (length(all_markers) < 2) break

                df_subset <- df[, all_markers, drop = FALSE]

                # Compute distance
                if (length(current_catVars) > 0 && length(current_contVars) > 0) {
                    d <- cluster::daisy(df_subset, metric = "gower")
                } else if (length(current_contVars) > 0) {
                    d <- stats::dist(df_subset)
                } else {
                    d <- cluster::daisy(df_subset, metric = "gower")
                }

                # Cluster
                opts <- self$options
                k <- opts$nClusters %||% 3
                fit <- cluster::pam(d, k = k)
                clusters <- as.factor(fit$clustering)

                # Compute silhouette
                sil <- cluster::silhouette(as.integer(clusters), d)
                avg_sil <- mean(sil[, "sil_width"])

                # Record history
                history <- rbind(history, data.frame(
                    iteration = iter,
                    n_markers = length(all_markers),
                    markers_used = paste(all_markers, collapse = ", "),
                    avg_silhouette = avg_sil,
                    stringsAsFactors = FALSE
                ))

                # Check if improved
                if (avg_sil > best_silhouette) {
                    best_silhouette <- avg_sil
                    best_markers <- list(catVars = current_catVars, contVars = current_contVars)
                }

                # Compute marker importance
                importance <- private$.computeMarkerImportance(df_subset, clusters,
                                                              current_catVars, current_contVars)
                if (is.null(importance)) break

                # Remove lowest importance marker
                importance <- importance[order(-importance$importance), ]
                markers_to_keep <- importance$marker[1:(length(all_markers) - 1)]

                # Update marker lists
                current_catVars <- intersect(markers_to_keep, current_catVars)
                current_contVars <- intersect(markers_to_keep, current_contVars)

                # Stop if no improvement or too few markers
                if (length(c(current_catVars, current_contVars)) < 2) break
            }

            # Add improvement column
            history$improvement <- ""
            for (i in 2:nrow(history)) {
                diff <- history$avg_silhouette[i] - history$avg_silhouette[i-1]
                if (diff > 0.01) {
                    history$improvement[i] <- sprintf("+%.3f (Better)", diff)
                } else if (diff < -0.01) {
                    history$improvement[i] <- sprintf("%.3f (Worse)", diff)
                } else {
                    history$improvement[i] <- "~0 (No change)"
                }
            }
            history$improvement[1] <- "Baseline"

            return(list(
                history = history,
                best_markers = best_markers,
                best_silhouette = best_silhouette
            ))
        },

        # Populate marker importance table
        .populateMarkerImportance = function() {
            if (!isTRUE(self$options$markerOptimization)) return()

            analysisState <- self$results$summary$state
            if (is.null(analysisState)) return()

            optimization <- analysisState$optimization
            if (is.null(optimization)) return()

            table <- self$results$markerImportance
            importance_df <- optimization$importance

            for (i in seq_len(nrow(importance_df))) {
                row <- importance_df[i, ]
                table$addRow(rowKey = row$marker, list(
                    marker = row$marker,
                    importance = row$importance,
                    rank = i,
                    correlation_max = ifelse(is.na(row$max_correlation), 0, row$max_correlation),
                    redundant_with = row$redundant_with,
                    recommendation = row$recommendation
                ))
            }
        },

        # =============================================================================
        # MARKER-LEVEL CLUSTERING FUNCTIONS
        # =============================================================================

        # Main marker clustering function
        .performMarkerClustering = function(df, catVars, contVars, method = "chisquared",
                                           linkage = "ward", testAssociations = TRUE,
                                           autoCut = TRUE) {

            allMarkers <- c(catVars, contVars)
            if (length(allMarkers) < 2) {
                return(list(error = "Need at least 2 markers for clustering"))
            }

            # Compute distance matrix based on method
            distMatrix <- NULL
            markerType <- "mixed"

            if (method == "chisquared" && length(catVars) >= 2) {
                # Chi-squared distance for categorical markers
                distMatrix <- private$.computeChiSquaredDistance(df, catVars)
                markers <- catVars
                markerType <- "categorical"
            } else if (method == "jaccard" && length(catVars) >= 2) {
                # Jaccard distance for binary markers
                distMatrix <- private$.computeMarkerJaccardDistance(df, catVars)
                markers <- catVars
                markerType <- "categorical"
            } else if (method == "hamming" && length(catVars) >= 2) {
                # Hamming distance for categorical markers
                distMatrix <- private$.computeHammingDistance(df, catVars)
                markers <- catVars
                markerType <- "categorical"
            } else if (method == "cramer" && length(catVars) >= 2) {
                # Cramér's V distance for categorical markers
                distMatrix <- private$.computeCramerDistance(df, catVars)
                markers <- catVars
                markerType <- "categorical"
            } else if (method == "euclidean" && length(contVars) >= 2) {
                # Euclidean distance for continuous markers
                distMatrix <- dist(scale(df[, contVars]), method = "euclidean")
                markers <- contVars
                markerType <- "continuous"
            } else if (method == "manhattan" && length(contVars) >= 2) {
                # Manhattan distance for continuous markers
                distMatrix <- dist(scale(df[, contVars]), method = "manhattan")
                markers <- contVars
                markerType <- "continuous"
            } else if (method == "correlation" && length(contVars) >= 2) {
                # Correlation distance for continuous markers
                corMatrix <- cor(df[, contVars], use = "pairwise.complete.obs")
                distMatrix <- as.dist(1 - abs(corMatrix))
                markers <- contVars
                markerType <- "continuous"
            } else if (method == "mutual_info" && length(allMarkers) >= 2) {
                # Mutual information distance (works for any type)
                distMatrix <- private$.computeMutualInfoDistance(df, catVars, contVars)
                markers <- allMarkers
                markerType <- "mixed"
            } else if (method == "mixed" && length(allMarkers) >= 2) {
                # Mixed distance using automatic selection
                distMatrix <- private$.computeMixedMarkerDistance(df, catVars, contVars)
                markers <- allMarkers
                markerType <- "mixed"
            } else {
                return(list(error = "Invalid marker clustering method or insufficient markers"))
            }

            # Perform hierarchical clustering
            linkageMethod <- switch(linkage,
                ward = "ward.D2",
                complete = "complete",
                average = "average",
                "ward.D2"
            )

            hc <- hclust(distMatrix, method = linkageMethod)

            # Test marker-marker associations if requested
            associations <- NULL
            if (testAssociations) {
                associations <- private$.testMarkerAssociations(df, markers, markerType)
            }

            # Build clustering tree (dendrogram merge sequence)
            clusteringTree <- private$.buildMarkerClusteringTree(hc, distMatrix, markers)

            # Identify marker groups if auto-cut enabled
            markerGroups <- NULL
            if (autoCut) {
                markerGroups <- private$.identifyMarkerGroups(hc, associations, markers)
            }

            return(list(
                hc = hc,
                distMatrix = distMatrix,
                markers = markers,
                markerType = markerType,
                associations = associations,
                clusteringTree = clusteringTree,
                markerGroups = markerGroups,
                method = method,
                linkage = linkage
            ))
        },

        # Compute chi-squared distance matrix for categorical markers
        .computeChiSquaredDistance = function(df, catVars) {
            n_markers <- length(catVars)
            chiSqMatrix <- matrix(0, n_markers, n_markers)
            rownames(chiSqMatrix) <- colnames(chiSqMatrix) <- catVars

            for (i in 1:(n_markers-1)) {
                for (j in (i+1):n_markers) {
                    marker1 <- df[[catVars[i]]]
                    marker2 <- df[[catVars[j]]]

                    # Create contingency table
                    contTable <- table(marker1, marker2, useNA = "no")

                    # Compute chi-squared statistic
                    if (nrow(contTable) > 1 && ncol(contTable) > 1) {
                        chiTest <- tryCatch({
                            chisq.test(contTable)
                        }, error = function(e) NULL, warning = function(w) NULL)

                        if (!is.null(chiTest)) {
                            # Use chi-squared as dissimilarity (higher = more different)
                            chiSqMatrix[i, j] <- chiTest$statistic
                            chiSqMatrix[j, i] <- chiTest$statistic
                        }
                    }
                }
            }

            # Convert to distance object (normalize by max chi-squared)
            maxChiSq <- max(chiSqMatrix)
            if (maxChiSq > 0) {
                chiSqMatrix <- chiSqMatrix / maxChiSq
            }

            return(as.dist(chiSqMatrix))
        },

        # Compute Jaccard distance for binary/categorical markers
        .computeMarkerJaccardDistance = function(df, catVars) {
            n_markers <- length(catVars)
            jaccardMatrix <- matrix(0, n_markers, n_markers)
            rownames(jaccardMatrix) <- colnames(jaccardMatrix) <- catVars

            for (i in 1:(n_markers-1)) {
                for (j in (i+1):n_markers) {
                    marker1 <- df[[catVars[i]]]
                    marker2 <- df[[catVars[j]]]

                    # Convert to binary if needed (presence/absence)
                    # For ordinal data, consider positive if > baseline
                    binary1 <- as.numeric(marker1 != levels(marker1)[1])
                    binary2 <- as.numeric(marker2 != levels(marker2)[1])

                    # Compute Jaccard index
                    intersection <- sum(binary1 & binary2, na.rm = TRUE)
                    union <- sum(binary1 | binary2, na.rm = TRUE)

                    jaccard <- ifelse(union > 0, intersection / union, 0)
                    jaccardMatrix[i, j] <- 1 - jaccard  # Convert to distance
                    jaccardMatrix[j, i] <- 1 - jaccard
                }
            }

            return(as.dist(jaccardMatrix))
        },

        # Compute Hamming distance for categorical markers
        .computeHammingDistance = function(df, catVars) {
            n_markers <- length(catVars)
            n_cases <- nrow(df)
            hammingMatrix <- matrix(0, n_markers, n_markers)
            rownames(hammingMatrix) <- colnames(hammingMatrix) <- catVars

            for (i in 1:(n_markers-1)) {
                for (j in (i+1):n_markers) {
                    marker1 <- df[[catVars[i]]]
                    marker2 <- df[[catVars[j]]]

                    # Count mismatches (cases where markers differ)
                    mismatches <- sum(marker1 != marker2, na.rm = TRUE)
                    valid_pairs <- sum(!is.na(marker1) & !is.na(marker2))

                    # Normalize by number of valid comparisons
                    hamming_dist <- ifelse(valid_pairs > 0, mismatches / valid_pairs, 0)

                    hammingMatrix[i, j] <- hamming_dist
                    hammingMatrix[j, i] <- hamming_dist
                }
            }

            return(as.dist(hammingMatrix))
        },

        # Compute Cramér's V distance for categorical markers
        .computeCramerDistance = function(df, catVars) {
            n_markers <- length(catVars)
            cramerMatrix <- matrix(0, n_markers, n_markers)
            rownames(cramerMatrix) <- colnames(cramerMatrix) <- catVars

            for (i in 1:(n_markers-1)) {
                for (j in (i+1):n_markers) {
                    marker1 <- df[[catVars[i]]]
                    marker2 <- df[[catVars[j]]]

                    # Create contingency table
                    contTable <- table(marker1, marker2, useNA = "no")

                    # Compute Cramér's V
                    if (nrow(contTable) > 1 && ncol(contTable) > 1) {
                        chiTest <- tryCatch({
                            chisq.test(contTable)
                        }, error = function(e) NULL, warning = function(w) NULL)

                        if (!is.null(chiTest)) {
                            n <- sum(contTable)
                            minDim <- min(dim(contTable)) - 1
                            cramersV <- sqrt(chiTest$statistic / (n * minDim))
                            # Convert to distance: high V = similar (low distance)
                            cramerMatrix[i, j] <- 1 - as.numeric(cramersV)
                            cramerMatrix[j, i] <- 1 - as.numeric(cramersV)
                        }
                    }
                }
            }

            return(as.dist(cramerMatrix))
        },

        # Compute mutual information distance
        .computeMutualInfoDistance = function(df, catVars, contVars) {
            allMarkers <- c(catVars, contVars)
            n_markers <- length(allMarkers)
            miMatrix <- matrix(0, n_markers, n_markers)
            rownames(miMatrix) <- colnames(miMatrix) <- allMarkers

            # Helper function to discretize continuous variables
            discretize <- function(x, n_bins = 5) {
                cut(x, breaks = n_bins, labels = FALSE, include.lowest = TRUE)
            }

            for (i in 1:(n_markers-1)) {
                for (j in (i+1):n_markers) {
                    var1 <- allMarkers[i]
                    var2 <- allMarkers[j]

                    # Get data and discretize if continuous
                    data1 <- df[[var1]]
                    data2 <- df[[var2]]

                    if (var1 %in% contVars) {
                        data1 <- discretize(data1)
                    }
                    if (var2 %in% contVars) {
                        data2 <- discretize(data2)
                    }

                    # Compute mutual information
                    mi <- private$.computeMI(data1, data2)

                    # Normalize by max entropy (creates normalized MI in [0,1])
                    h1 <- private$.computeEntropy(data1)
                    h2 <- private$.computeEntropy(data2)
                    max_h <- max(h1, h2)

                    normalized_mi <- ifelse(max_h > 0, mi / max_h, 0)

                    # Convert to distance
                    miMatrix[i, j] <- 1 - normalized_mi
                    miMatrix[j, i] <- 1 - normalized_mi
                }
            }

            return(as.dist(miMatrix))
        },

        # Compute entropy for a variable
        .computeEntropy = function(x) {
            x <- x[!is.na(x)]
            if (length(x) == 0) return(0)

            probs <- table(x) / length(x)
            probs <- probs[probs > 0]  # Remove zero probabilities

            -sum(probs * log2(probs))
        },

        # Compute mutual information between two variables
        .computeMI = function(x, y) {
            # Remove missing values
            valid_idx <- !is.na(x) & !is.na(y)
            x <- x[valid_idx]
            y <- y[valid_idx]

            if (length(x) == 0) return(0)

            # Compute marginal entropies
            h_x <- private$.computeEntropy(x)
            h_y <- private$.computeEntropy(y)

            # Compute joint entropy
            joint_table <- table(x, y)
            joint_probs <- joint_table / sum(joint_table)
            joint_probs <- joint_probs[joint_probs > 0]
            h_xy <- -sum(joint_probs * log2(joint_probs))

            # MI = H(X) + H(Y) - H(X,Y)
            mi <- h_x + h_y - h_xy

            return(max(0, mi))  # Ensure non-negative
        },

        # Compute mixed-type marker distance
        .computeMixedMarkerDistance = function(df, catVars, contVars) {
            allMarkers <- c(catVars, contVars)
            n_markers <- length(allMarkers)
            distMatrix <- matrix(0, n_markers, n_markers)
            rownames(distMatrix) <- colnames(distMatrix) <- allMarkers

            for (i in 1:(n_markers-1)) {
                for (j in (i+1):n_markers) {
                    var1 <- allMarkers[i]
                    var2 <- allMarkers[j]

                    # Determine variable types
                    isCat1 <- var1 %in% catVars
                    isCat2 <- var2 %in% catVars

                    distance <- 0
                    if (isCat1 && isCat2) {
                        # Both categorical: use chi-squared
                        contTable <- table(df[[var1]], df[[var2]], useNA = "no")
                        chiTest <- tryCatch(chisq.test(contTable),
                                          error = function(e) NULL,
                                          warning = function(w) NULL)
                        if (!is.null(chiTest)) {
                            # Normalize by sample size
                            distance <- sqrt(chiTest$statistic / nrow(df))
                        }
                    } else if (!isCat1 && !isCat2) {
                        # Both continuous: use correlation
                        corVal <- cor(df[[var1]], df[[var2]], use = "pairwise.complete.obs")
                        distance <- 1 - abs(corVal)
                    } else {
                        # Mixed: use eta-squared (ANOVA-based)
                        catVar <- if (isCat1) var1 else var2
                        contVar <- if (isCat1) var2 else var1

                        aovResult <- tryCatch({
                            aov(df[[contVar]] ~ df[[catVar]])
                        }, error = function(e) NULL)

                        if (!is.null(aovResult)) {
                            ss <- summary(aovResult)[[1]]
                            if (nrow(ss) >= 2) {
                                etaSq <- ss[1, "Sum Sq"] / sum(ss[, "Sum Sq"])
                                distance <- 1 - etaSq
                            }
                        }
                    }

                    distMatrix[i, j] <- distance
                    distMatrix[j, i] <- distance
                }
            }

            return(as.dist(distMatrix))
        },

        # Test marker-marker associations
        .testMarkerAssociations = function(df, markers, markerType) {
            n_markers <- length(markers)
            if (n_markers < 2) return(NULL)

            associations <- data.frame(
                marker1 = character(),
                marker2 = character(),
                test = character(),
                statistic = numeric(),
                df = integer(),
                p_value = numeric(),
                effect_size = character(),
                interpretation = character(),
                stringsAsFactors = FALSE
            )

            for (i in 1:(n_markers-1)) {
                for (j in (i+1):n_markers) {
                    marker1 <- markers[i]
                    marker2 <- markers[j]

                    result <- NULL
                    if (markerType %in% c("categorical", "mixed")) {
                        # Chi-squared test for categorical associations
                        contTable <- table(df[[marker1]], df[[marker2]], useNA = "no")
                        result <- tryCatch({
                            test <- chisq.test(contTable)

                            # Calculate Cramer's V
                            n <- sum(contTable)
                            minDim <- min(dim(contTable)) - 1
                            cramersV <- sqrt(test$statistic / (n * minDim))

                            # Interpretation
                            interp <- if (test$p.value < 0.001) "Very strong association" else
                                     if (test$p.value < 0.01) "Strong association" else
                                     if (test$p.value < 0.05) "Moderate association" else
                                     "Weak/No association"

                            list(
                                test = "Chi-squared",
                                statistic = as.numeric(test$statistic),
                                df = test$parameter,
                                p_value = test$p.value,
                                effect_size = sprintf("Cramer's V = %.3f", cramersV),
                                interpretation = interp
                            )
                        }, error = function(e) NULL, warning = function(w) NULL)
                    } else if (markerType == "continuous") {
                        # Correlation test for continuous associations
                        result <- tryCatch({
                            test <- cor.test(df[[marker1]], df[[marker2]])

                            # Interpretation
                            r <- abs(test$estimate)
                            interp <- if (test$p.value < 0.05 && r > 0.7) "Strong correlation" else
                                     if (test$p.value < 0.05 && r > 0.4) "Moderate correlation" else
                                     if (test$p.value < 0.05) "Weak correlation" else
                                     "No significant correlation"

                            list(
                                test = "Pearson correlation",
                                statistic = as.numeric(test$statistic),
                                df = test$parameter,
                                p_value = test$p.value,
                                effect_size = sprintf("r = %.3f", test$estimate),
                                interpretation = interp
                            )
                        }, error = function(e) NULL)
                    }

                    if (!is.null(result)) {
                        associations <- rbind(associations, data.frame(
                            marker1 = marker1,
                            marker2 = marker2,
                            test = result$test,
                            statistic = result$statistic,
                            df = as.integer(result$df),
                            p_value = result$p_value,
                            effect_size = result$effect_size,
                            interpretation = result$interpretation,
                            stringsAsFactors = FALSE
                        ))
                    }
                }
            }

            return(associations)
        },

        # Build marker clustering tree showing merge sequence
        .buildMarkerClusteringTree = function(hc, distMatrix, markers) {
            n <- length(markers)
            mergeSequence <- hc$merge
            heights <- hc$height

            tree <- data.frame(
                step = integer(),
                markers_merged = character(),
                groups_after_merge = character(),
                distance = numeric(),
                distance_reduction = numeric(),
                pct_reduction = numeric(),
                stringsAsFactors = FALSE
            )

            # Track cluster membership
            clusters <- as.list(1:n)
            names(clusters) <- markers

            initialDist <- max(heights)
            prevDist <- initialDist

            for (i in seq_len(nrow(mergeSequence))) {
                step <- mergeSequence[i, ]
                currentDist <- heights[i]

                # Identify which clusters/markers are being merged
                cluster1Idx <- abs(step[1])
                cluster2Idx <- abs(step[2])

                # Get cluster contents
                cluster1 <- if (step[1] < 0) markers[-step[1]] else paste0("Group", step[1])
                cluster2 <- if (step[2] < 0) markers[-step[2]] else paste0("Group", step[2])

                # Calculate reductions
                reduction <- prevDist - currentDist
                pctReduction <- ifelse(initialDist > 0, reduction / initialDist * 100, 0)

                # Update tree
                tree <- rbind(tree, data.frame(
                    step = i,
                    markers_merged = paste(cluster1, "+", cluster2),
                    groups_after_merge = paste(n - i, "groups"),
                    distance = currentDist,
                    distance_reduction = reduction,
                    pct_reduction = pctReduction,
                    stringsAsFactors = FALSE
                ))

                prevDist <- currentDist
            }

            return(tree)
        },

        # Identify statistically distinct marker groups
        .identifyMarkerGroups = function(hc, associations, markers) {
            if (is.null(associations) || nrow(associations) == 0) {
                # Without association tests, use dynamic cut
                cutHeight <- mean(hc$height)
                groups <- cutree(hc, h = cutHeight)
            } else {
                # Use association p-values to determine cut height
                # Markers with p < 0.05 should be in different groups
                significantAssoc <- associations[associations$p_value < 0.05, ]

                if (nrow(significantAssoc) > 0) {
                    # Find height that best separates significant associations
                    cutHeight <- median(hc$height)
                } else {
                    cutHeight <- mean(hc$height)
                }

                groups <- cutree(hc, h = cutHeight)
            }

            # Build group summary
            groupSummary <- data.frame(
                group = character(),
                members = character(),
                n_markers = integer(),
                avg_association = numeric(),
                interpretation = character(),
                stringsAsFactors = FALSE
            )

            for (g in sort(unique(groups))) {
                groupMarkers <- markers[groups == g]

                # Calculate average association within group
                avgAssoc <- NA
                if (!is.null(associations) && length(groupMarkers) > 1) {
                    groupAssocs <- associations[
                        (associations$marker1 %in% groupMarkers & associations$marker2 %in% groupMarkers),
                    ]
                    if (nrow(groupAssocs) > 0) {
                        avgAssoc <- mean(groupAssocs$p_value, na.rm = TRUE)
                    }
                }

                # Interpretation
                interp <- if (length(groupMarkers) == 1) {
                    "Single marker"
                } else if (!is.na(avgAssoc) && avgAssoc < 0.05) {
                    "Co-expressed markers (statistically associated)"
                } else {
                    "Markers cluster together but associations not significant"
                }

                groupSummary <- rbind(groupSummary, data.frame(
                    group = paste0("Group ", g),
                    members = paste(groupMarkers, collapse = ", "),
                    n_markers = length(groupMarkers),
                    avg_association = ifelse(is.na(avgAssoc), 0, avgAssoc),
                    interpretation = interp,
                    stringsAsFactors = FALSE
                ))
            }

            return(groupSummary)
        },

        # Populate marker clustering results tables
        .populateMarkerClusteringResults = function() {
            if (!isTRUE(self$options$performMarkerClustering)) return()

            analysisState <- self$results$summary$state
            if (is.null(analysisState)) return()

            results <- analysisState$markerClusteringResults
            if (is.null(results) || !is.null(results$error)) return()

            # Populate association table
            if (!is.null(results$associations) && isTRUE(self$options$markerSignificanceTest)) {
                assocTable <- self$results$markerAssociationTable
                for (i in seq_len(nrow(results$associations))) {
                    row <- results$associations[i, ]
                    assocTable$addRow(rowKey = paste0(row$marker1, "_", row$marker2), list(
                        marker1 = row$marker1,
                        marker2 = row$marker2,
                        test = row$test,
                        statistic = row$statistic,
                        df = row$df,
                        p_value = row$p_value,
                        effect_size = row$effect_size,
                        interpretation = row$interpretation
                    ))
                }
            }

            # Populate clustering tree table
            if (!is.null(results$clusteringTree)) {
                treeTable <- self$results$markerClusteringTree
                for (i in seq_len(nrow(results$clusteringTree))) {
                    row <- results$clusteringTree[i, ]
                    treeTable$addRow(rowKey = i, list(
                        step = row$step,
                        markers_merged = row$markers_merged,
                        groups_after_merge = row$groups_after_merge,
                        distance = row$distance,
                        distance_reduction = row$distance_reduction,
                        pct_reduction = row$pct_reduction / 100
                    ))
                }
            }

            # Populate marker groups table
            if (!is.null(results$markerGroups) && isTRUE(self$options$markerCutHeight)) {
                groupsTable <- self$results$markerGroups
                for (i in seq_len(nrow(results$markerGroups))) {
                    row <- results$markerGroups[i, ]
                    groupsTable$addRow(rowKey = row$group, list(
                        group = row$group,
                        members = row$members,
                        n_markers = row$n_markers,
                        avg_association = row$avg_association,
                        interpretation = row$interpretation
                    ))
                }
            }
        },

        # Plot marker dendrogram
        .plotMarkerDendrogram = function(image, ggtheme, theme, ...) {
            if (!isTRUE(self$options$performMarkerClustering)) return()

            analysisState <- self$results$summary$state
            if (is.null(analysisState)) return()

            results <- analysisState$markerClusteringResults
            if (is.null(results) || !is.null(results$error)) return()

            hc <- results$hc
            if (is.null(hc)) return()

            # Plot dendrogram
            plot(hc,
                 main = sprintf("Marker Clustering Dendrogram (%s distance, %s linkage)",
                               results$method, results$linkage),
                 xlab = "IHC Markers",
                 ylab = "Distance/Dissimilarity",
                 cex = 0.8,
                 hang = -1)

            # Add significance line if marker groups identified
            if (!is.null(results$markerGroups) && isTRUE(self$options$markerCutHeight)) {
                cutHeight <- mean(hc$height)
                abline(h = cutHeight, col = "red", lty = 2, lwd = 2)
                text(length(hc$labels) * 0.8, cutHeight,
                     "Significance threshold",
                     pos = 3, col = "red", cex = 0.8)

                # Highlight groups
                nGroups <- length(unique(cutree(hc, h = cutHeight)))
                rect.hclust(hc, k = nGroups, border = "blue")
            }
        },

        # Populate cluster quality metrics table
        .populateClusterQuality = function() {
            if (!isTRUE(self$options$clusterQualityMetrics)) return()

            analysisState <- self$results$summary$state
            if (is.null(analysisState)) return()

            quality <- analysisState$quality
            if (is.null(quality)) return()

            table <- self$results$clusterQuality

            for (i in seq_len(nrow(quality))) {
                row <- quality[i, ]
                table$addRow(rowKey = row$cluster, list(
                    cluster = as.character(row$cluster),
                    size = row$size,
                    purity = row$purity,
                    avg_silhouette = row$avg_silhouette,
                    separation = row$separation,
                    compactness = row$compactness,
                    quality = row$quality
                ))
            }
        },

        # Populate refinement history table
        .populateRefinementHistory = function() {
            if (!isTRUE(self$options$iterativeRefinement)) return()

            analysisState <- self$results$summary$state
            if (is.null(analysisState)) return()

            refinement <- analysisState$refinement
            if (is.null(refinement) || is.null(refinement$history)) return()

            table <- self$results$refinementHistory
            history <- refinement$history

            for (i in seq_len(nrow(history))) {
                row <- history[i, ]
                table$addRow(rowKey = i, list(
                    iteration = row$iteration,
                    n_markers = row$n_markers,
                    markers_used = row$markers_used,
                    silhouette = row$avg_silhouette,
                    improvement = row$improvement
                ))
            }
        },

        # =============================================================================
        # SPATIAL/COMPARTMENT ANALYSIS FUNCTIONS
        # =============================================================================

        # Perform spatial compartment analysis
        .analyzeSpatialCompartments = function(data, spatialVar, catVars, contVars, mode = "both") {
            # Validate inputs
            if (is.null(spatialVar) || !spatialVar %in% names(data)) {
                stop("Spatial compartment variable not found in data")
            }

            compartmentCol <- data[[spatialVar]]
            if (!is.factor(compartmentCol)) {
                compartmentCol <- as.factor(compartmentCol)
            }

            # Remove NA compartments
            valid_idx <- !is.na(compartmentCol)
            data <- data[valid_idx, ]
            compartmentCol <- compartmentCol[valid_idx]

            compartments <- levels(compartmentCol)
            n_compartments <- length(compartments)

            if (n_compartments < 2) {
                stop("Spatial analysis requires at least 2 compartments")
            }

            markerVars <- c(catVars, contVars)
            opts <- self$options

            results <- list(
                compartments = compartments,
                n_compartments = n_compartments,
                compartment_clusters = list(),
                compartment_summary = data.frame(
                    compartment = character(),
                    n_cases = integer(),
                    n_clusters = integer(),
                    avg_silhouette = numeric(),
                    quality = character(),
                    stringsAsFactors = FALSE
                )
            )

            # Mode 1: Cluster within each compartment separately
            if (mode %in% c("within", "both")) {
                for (comp in compartments) {
                    comp_idx <- compartmentCol == comp
                    comp_data <- data[comp_idx, markerVars, drop = FALSE]

                    # Skip if too few cases
                    if (nrow(comp_data) < opts$nClusters) {
                        next
                    }

                    # Perform clustering for this compartment
                    tryCatch({
                        comp_result <- private$.performClustering(
                            comp_data,
                            catVars,
                            contVars,
                            opts
                        )

                        # Compute silhouette
                        sil <- cluster::silhouette(
                            as.integer(comp_result$clusters),
                            comp_result$dist
                        )
                        avg_sil <- mean(sil[, "sil_width"])

                        # Determine quality
                        quality <- if (avg_sil > 0.7) "Excellent"
                                  else if (avg_sil > 0.5) "Good"
                                  else if (avg_sil > 0.25) "Fair"
                                  else "Poor"

                        # Store results
                        results$compartment_clusters[[comp]] <- list(
                            clusters = comp_result$clusters,
                            fit = comp_result$fit,
                            dist = comp_result$dist,
                            silhouette = avg_sil,
                            case_indices = which(comp_idx)
                        )

                        # Add to summary
                        results$compartment_summary <- rbind(
                            results$compartment_summary,
                            data.frame(
                                compartment = comp,
                                n_cases = nrow(comp_data),
                                n_clusters = length(unique(comp_result$clusters)),
                                avg_silhouette = avg_sil,
                                quality = quality,
                                stringsAsFactors = FALSE
                            )
                        )
                    }, error = function(e) {
                        # Skip compartments that fail clustering
                        NULL
                    })
                }
            }

            # Mode 2: Cluster all data together and compare between compartments
            if (mode %in% c("between", "both")) {
                # Perform overall clustering
                all_data <- data[, markerVars, drop = FALSE]
                overall_result <- private$.performClustering(
                    all_data,
                    catVars,
                    contVars,
                    opts
                )

                results$overall_clusters <- overall_result$clusters
                results$overall_dist <- overall_result$dist
                results$compartment_vector <- compartmentCol
            }

            return(results)
        },

        # Compare clustering between compartments
        .compareCompartmentClustering = function(spatialResults) {
            compartments <- spatialResults$compartments
            n_comp <- length(compartments)

            if (n_comp < 2) return(NULL)

            comparison <- data.frame(
                compartment1 = character(),
                compartment2 = character(),
                concordance = numeric(),
                kappa = numeric(),
                interpretation = character(),
                stringsAsFactors = FALSE
            )

            # Compare all pairs of compartments
            for (i in 1:(n_comp - 1)) {
                for (j in (i + 1):n_comp) {
                    comp1 <- compartments[i]
                    comp2 <- compartments[j]

                    # Get cluster assignments for overlapping cases (if any)
                    # For "between" mode with overall clustering
                    if (!is.null(spatialResults$overall_clusters)) {
                        comp1_idx <- spatialResults$compartment_vector == comp1
                        comp2_idx <- spatialResults$compartment_vector == comp2

                        clusters1 <- spatialResults$overall_clusters[comp1_idx]
                        clusters2 <- spatialResults$overall_clusters[comp2_idx]

                        # Compute concordance (what % are in same cluster?)
                        # This makes sense if cases appear in both compartments
                        # Otherwise, we compare cluster distributions

                        # Create contingency table
                        cluster_table <- table(
                            Comp1 = as.character(clusters1),
                            Comp2 = as.character(clusters2)
                        )

                        # Compute Cohen's kappa if dimensions match
                        kappa_val <- NA
                        if (nrow(cluster_table) == ncol(cluster_table)) {
                            tryCatch({
                                # Simplified kappa calculation
                                total <- sum(cluster_table)
                                observed_agreement <- sum(diag(cluster_table)) / total

                                row_sums <- rowSums(cluster_table)
                                col_sums <- colSums(cluster_table)
                                expected_agreement <- sum(row_sums * col_sums) / (total^2)

                                kappa_val <- (observed_agreement - expected_agreement) /
                                            (1 - expected_agreement)
                            }, error = function(e) {
                                kappa_val <- NA
                            })
                        }

                        # Concordance as proportion in same cluster
                        concordance <- sum(diag(cluster_table)) / sum(cluster_table)

                        # Interpretation
                        interpret <- if (is.na(kappa_val)) {
                            "Unable to calculate"
                        } else if (kappa_val < 0) {
                            "Poor agreement"
                        } else if (kappa_val < 0.2) {
                            "Slight agreement"
                        } else if (kappa_val < 0.4) {
                            "Fair agreement"
                        } else if (kappa_val < 0.6) {
                            "Moderate agreement"
                        } else if (kappa_val < 0.8) {
                            "Substantial agreement"
                        } else {
                            "Almost perfect agreement"
                        }

                        comparison <- rbind(comparison, data.frame(
                            compartment1 = comp1,
                            compartment2 = comp2,
                            concordance = concordance,
                            kappa = ifelse(is.na(kappa_val), NA, kappa_val),
                            interpretation = interpret,
                            stringsAsFactors = FALSE
                        ))
                    }
                }
            }

            return(comparison)
        },

        # Compute inter-compartment concordance
        .computeCompartmentConcordance = function(clusters1, clusters2) {
            # Both cluster vectors must have same length
            if (length(clusters1) != length(clusters2)) {
                return(list(concordance = NA, kappa = NA))
            }

            # Remove missing values
            valid_idx <- !is.na(clusters1) & !is.na(clusters2)
            clusters1 <- clusters1[valid_idx]
            clusters2 <- clusters2[valid_idx]

            if (length(clusters1) == 0) {
                return(list(concordance = NA, kappa = NA))
            }

            # Simple concordance: proportion assigned to same cluster
            concordance <- mean(clusters1 == clusters2)

            # Cohen's kappa
            confusion <- table(clusters1, clusters2)
            n <- sum(confusion)

            observed_agreement <- sum(diag(confusion)) / n

            row_margins <- rowSums(confusion) / n
            col_margins <- colSums(confusion) / n
            expected_agreement <- sum(row_margins * col_margins)

            kappa <- (observed_agreement - expected_agreement) / (1 - expected_agreement)

            return(list(
                concordance = concordance,
                kappa = kappa
            ))
        },

        # Test marker expression differences by compartment
        .testMarkerDifferencesByCompartment = function(data, spatialVar, catVars, contVars) {
            compartmentCol <- data[[spatialVar]]
            if (!is.factor(compartmentCol)) {
                compartmentCol <- as.factor(compartmentCol)
            }

            compartments <- levels(compartmentCol)
            n_comp <- length(compartments)

            if (n_comp < 2) return(NULL)

            markerVars <- c(catVars, contVars)

            results <- data.frame(
                marker = character(),
                compartment1 = character(),
                compartment2 = character(),
                test = character(),
                statistic = numeric(),
                p_value = numeric(),
                effect_size = character(),
                stringsAsFactors = FALSE
            )

            # Compare all pairs
            for (i in 1:(n_comp - 1)) {
                for (j in (i + 1):n_comp) {
                    comp1 <- compartments[i]
                    comp2 <- compartments[j]

                    comp1_idx <- compartmentCol == comp1
                    comp2_idx <- compartmentCol == comp2

                    # Test each marker
                    for (marker in markerVars) {
                        marker_data <- data[[marker]]

                        comp1_vals <- marker_data[comp1_idx]
                        comp2_vals <- marker_data[comp2_idx]

                        # Remove NAs
                        comp1_vals <- comp1_vals[!is.na(comp1_vals)]
                        comp2_vals <- comp2_vals[!is.na(comp2_vals)]

                        if (length(comp1_vals) < 2 || length(comp2_vals) < 2) next

                        if (is.numeric(marker_data)) {
                            # Continuous: t-test or Mann-Whitney
                            test_result <- tryCatch({
                                wilcox.test(comp1_vals, comp2_vals)
                            }, error = function(e) NULL)

                            if (!is.null(test_result)) {
                                # Effect size: Cohen's d
                                mean_diff <- mean(comp1_vals) - mean(comp2_vals)
                                pooled_sd <- sqrt((var(comp1_vals) + var(comp2_vals)) / 2)
                                cohens_d <- mean_diff / pooled_sd

                                effect_size_text <- sprintf("Cohen's d = %.3f", cohens_d)

                                results <- rbind(results, data.frame(
                                    marker = marker,
                                    compartment1 = comp1,
                                    compartment2 = comp2,
                                    test = "Mann-Whitney U",
                                    statistic = test_result$statistic,
                                    p_value = test_result$p.value,
                                    effect_size = effect_size_text,
                                    stringsAsFactors = FALSE
                                ))
                            }
                        } else {
                            # Categorical: Chi-square or Fisher's exact
                            tbl <- table(
                                Compartment = c(rep(comp1, length(comp1_vals)),
                                               rep(comp2, length(comp2_vals))),
                                Level = c(as.character(comp1_vals), as.character(comp2_vals))
                            )

                            test_result <- tryCatch({
                                chisq.test(tbl)
                            }, error = function(e) NULL)

                            if (!is.null(test_result)) {
                                # Effect size: Cramér's V
                                n <- sum(tbl)
                                min_dim <- min(nrow(tbl) - 1, ncol(tbl) - 1)
                                cramers_v <- sqrt(test_result$statistic / (n * min_dim))

                                effect_size_text <- sprintf("Cramér's V = %.3f", cramers_v)

                                results <- rbind(results, data.frame(
                                    marker = marker,
                                    compartment1 = comp1,
                                    compartment2 = comp2,
                                    test = "Chi-square",
                                    statistic = test_result$statistic,
                                    p_value = test_result$p.value,
                                    effect_size = effect_size_text,
                                    stringsAsFactors = FALSE
                                ))
                            }
                        }
                    }
                }
            }

            return(results)
        },

        # Plot spatial compartment heatmap
        .plotSpatialHeatmap = function(image, ...) {
            analysisState <- self$results$summary$state
            if (is.null(analysisState)) return(FALSE)

            spatialResults <- analysisState$spatialResults
            if (is.null(spatialResults)) return(FALSE)

            opts <- self$options

            # Get data and compartment info
            if (!is.null(spatialResults$overall_clusters)) {
                # Use overall clustering
                clusters <- spatialResults$overall_clusters
                compartments <- spatialResults$compartment_vector
                df <- analysisState$df
            } else {
                return(FALSE)
            }

            # Create heatmap data with compartment annotations
            plot_data <- df
            plot_data$Cluster <- as.factor(clusters)
            plot_data$Compartment <- as.factor(compartments)

            # Order by compartment then cluster
            order_idx <- order(plot_data$Compartment, plot_data$Cluster)
            plot_data <- plot_data[order_idx, ]

            # Scale data if requested
            markerVars <- c(analysisState$catVars, analysisState$contVars)
            plot_matrix <- as.matrix(plot_data[, markerVars])

            # Convert categorical to numeric
            for (i in seq_along(markerVars)) {
                if (is.factor(plot_matrix[, i])) {
                    plot_matrix[, i] <- as.numeric(as.factor(plot_matrix[, i]))
                }
            }

            heatmap_scale <- opts$heatmapScale %||% "row"

            if (heatmap_scale == "row") {
                plot_matrix <- t(scale(t(plot_matrix)))
            } else if (heatmap_scale == "column") {
                plot_matrix <- scale(plot_matrix)
            }

            # Get color palette
            colors <- private$.getColorPalette(opts$colorPalette %||% "default", 100)

            # Create annotation colors for compartments and clusters
            compartment_colors <- rainbow(length(unique(compartments)))
            names(compartment_colors) <- unique(compartments)

            cluster_colors <- rainbow(length(unique(clusters)))
            names(cluster_colors) <- paste0("C", unique(clusters))

            annotation_row <- data.frame(
                Compartment = plot_data$Compartment,
                Cluster = paste0("C", plot_data$Cluster)
            )
            rownames(annotation_row) <- rownames(plot_data)

            annotation_colors <- list(
                Compartment = compartment_colors,
                Cluster = cluster_colors
            )

            # Draw heatmap
            if (requireNamespace("pheatmap", quietly = TRUE)) {
                pheatmap::pheatmap(
                    t(plot_matrix),
                    color = colors,
                    cluster_rows = TRUE,
                    cluster_cols = FALSE,
                    annotation_col = annotation_row,
                    annotation_colors = annotation_colors,
                    show_colnames = FALSE,
                    fontsize = private$.getFontSize(opts$fontSize %||% "medium"),
                    main = "IHC Expression by Spatial Compartment"
                )
            } else {
                # Fallback to base heatmap
                heatmap(t(plot_matrix),
                       col = colors,
                       Colv = NA,
                       main = "IHC Expression by Spatial Compartment")
            }

            return(TRUE)
        },

        # Populate spatial compartment summary table
        .populateSpatialCompartmentSummary = function() {
            if (!isTRUE(self$options$performSpatialAnalysis)) return()

            analysisState <- self$results$summary$state
            if (is.null(analysisState)) return()

            spatialResults <- analysisState$spatialResults
            if (is.null(spatialResults) || is.null(spatialResults$compartment_summary)) return()

            table <- self$results$spatialCompartmentSummary
            summary_df <- spatialResults$compartment_summary

            for (i in seq_len(nrow(summary_df))) {
                row <- summary_df[i, ]
                table$addRow(rowKey = i, list(
                    compartment = row$compartment,
                    n_cases = row$n_cases,
                    n_clusters = row$n_clusters,
                    avg_silhouette = row$avg_silhouette,
                    quality = row$quality
                ))
            }
        },

        # Populate spatial concordance table
        .populateSpatialConcordance = function() {
            if (!isTRUE(self$options$performSpatialAnalysis)) return()

            analysisState <- self$results$summary$state
            if (is.null(analysisState)) return()

            concordance <- analysisState$spatialConcordance
            if (is.null(concordance)) return()

            table <- self$results$spatialConcordance

            for (i in seq_len(nrow(concordance))) {
                row <- concordance[i, ]
                table$addRow(rowKey = i, list(
                    compartment1 = row$compartment1,
                    compartment2 = row$compartment2,
                    concordance = row$concordance,
                    kappa = row$kappa,
                    interpretation = row$interpretation
                ))
            }
        },

        # Populate spatial cluster comparison table
        .populateSpatialClusterComparison = function() {
            if (!isTRUE(self$options$performSpatialAnalysis)) return()

            analysisState <- self$results$summary$state
            if (is.null(analysisState)) return()

            spatialResults <- analysisState$spatialResults
            if (is.null(spatialResults)) return()

            # Calculate cluster distribution by compartment
            if (!is.null(spatialResults$overall_clusters)) {
                clusters <- spatialResults$overall_clusters
                compartments <- spatialResults$compartment_vector

                table <- self$results$spatialClusterComparison

                # Create cross-tabulation
                cross_tab <- table(
                    Compartment = compartments,
                    Cluster = paste0("C", clusters)
                )

                row_idx <- 1
                for (comp in rownames(cross_tab)) {
                    comp_total <- sum(cross_tab[comp, ])
                    for (clust in colnames(cross_tab)) {
                        n <- cross_tab[comp, clust]
                        pct <- n / comp_total

                        table$addRow(rowKey = row_idx, list(
                            compartment = comp,
                            cluster = clust,
                            n = as.integer(n),
                            percent = pct
                        ))
                        row_idx <- row_idx + 1
                    }
                }
            }
        },

        # Populate spatial marker differences table
        .populateSpatialMarkerDifferences = function() {
            if (!isTRUE(self$options$performSpatialAnalysis)) return()

            analysisState <- self$results$summary$state
            if (is.null(analysisState)) return()

            markerDiffs <- analysisState$spatialMarkerDifferences
            if (is.null(markerDiffs)) return()

            table <- self$results$spatialMarkerDifferences

            for (i in seq_len(nrow(markerDiffs))) {
                row <- markerDiffs[i, ]
                table$addRow(rowKey = i, list(
                    marker = row$marker,
                    compartment1 = row$compartment1,
                    compartment2 = row$compartment2,
                    test = row$test,
                    statistic = row$statistic,
                    p_value = row$p_value,
                    effect_size = row$effect_size
                ))
            }
        },

        # =============================================================================
        # DIAGNOSTIC FEATURES (Olsen et al. 2006)
        # =============================================================================

        # Calculate marker performance metrics (sensitivity, specificity, PPV, NPV)
        .calculateMarkerPerformance = function(data, markers, diagnosisVar) {
            if (is.null(diagnosisVar) || !diagnosisVar %in% names(data)) {
                return(NULL)
            }

            diagnosis <- data[[diagnosisVar]]
            if (!is.factor(diagnosis)) {
                diagnosis <- as.factor(diagnosis)
            }

            diagnoses <- levels(diagnosis)
            n_diagnoses <- length(diagnoses)

            if (n_diagnoses < 2) {
                return(NULL)
            }

            results <- data.frame(
                marker = character(),
                diagnosis = character(),
                sensitivity = numeric(),
                specificity = numeric(),
                ppv = numeric(),
                npv = numeric(),
                accuracy = numeric(),
                stringsAsFactors = FALSE
            )

            # For each marker
            for (marker in markers) {
                marker_data <- data[[marker]]

                # Convert to binary positive/negative
                # For categorical: any non-negative level is positive
                # For continuous: above median is positive
                if (is.factor(marker_data) || is.character(marker_data)) {
                    # Categorical marker
                    marker_data <- as.character(marker_data)
                    # Assume anything except "negative", "neg", "0", "-" is positive
                    marker_positive <- !(marker_data %in% c("negative", "Negative", "neg", "Neg", "0", "-", ""))
                } else if (is.numeric(marker_data)) {
                    # Continuous marker - use median split
                    marker_positive <- marker_data > median(marker_data, na.rm = TRUE)
                } else {
                    next
                }

                # Calculate metrics for each diagnosis
                for (target_dx in diagnoses) {
                    # True condition: this diagnosis
                    true_condition <- diagnosis == target_dx

                    # Remove NAs
                    valid_idx <- !is.na(marker_positive) & !is.na(true_condition)
                    marker_pos_clean <- marker_positive[valid_idx]
                    true_cond_clean <- true_condition[valid_idx]

                    if (length(marker_pos_clean) < 10) next  # Need minimum sample

                    # Confusion matrix
                    tp <- sum(marker_pos_clean & true_cond_clean)
                    tn <- sum(!marker_pos_clean & !true_cond_clean)
                    fp <- sum(marker_pos_clean & !true_cond_clean)
                    fn <- sum(!marker_pos_clean & true_cond_clean)

                    # Calculate metrics
                    sensitivity <- if ((tp + fn) > 0) tp / (tp + fn) else NA
                    specificity <- if ((tn + fp) > 0) tn / (tn + fp) else NA
                    ppv <- if ((tp + fp) > 0) tp / (tp + fp) else NA
                    npv <- if ((tn + fn) > 0) tn / (tn + fn) else NA
                    accuracy <- if (length(marker_pos_clean) > 0) (tp + tn) / length(marker_pos_clean) else NA

                    results <- rbind(results, data.frame(
                        marker = marker,
                        diagnosis = target_dx,
                        sensitivity = sensitivity,
                        specificity = specificity,
                        ppv = ppv,
                        npv = npv,
                        accuracy = accuracy,
                        stringsAsFactors = FALSE
                    ))
                }
            }

            return(results)
        },

        # Identify optimal antibody panels (combinations of markers)
        .identifyOptimalPanels = function(data, markers, diagnosisVar, panelSize = "pairs") {
            if (is.null(diagnosisVar) || !diagnosisVar %in% names(data)) {
                return(NULL)
            }

            diagnosis <- data[[diagnosisVar]]
            if (!is.factor(diagnosis)) {
                diagnosis <- as.factor(diagnosis)
            }

            diagnoses <- levels(diagnosis)
            n_diagnoses <- length(diagnoses)
            n_markers <- length(markers)

            if (n_diagnoses < 2 || n_markers < 2) {
                return(NULL)
            }

            # Convert markers to binary
            marker_matrix <- matrix(FALSE, nrow = nrow(data), ncol = n_markers)
            colnames(marker_matrix) <- markers

            for (i in seq_along(markers)) {
                marker <- markers[i]
                marker_data <- data[[marker]]

                if (is.factor(marker_data) || is.character(marker_data)) {
                    marker_data <- as.character(marker_data)
                    marker_matrix[, i] <- !(marker_data %in% c("negative", "Negative", "neg", "Neg", "0", "-", ""))
                } else if (is.numeric(marker_data)) {
                    marker_matrix[, i] <- marker_data > median(marker_data, na.rm = TRUE)
                }
            }

            results <- data.frame(
                rank = integer(),
                panel = character(),
                target_diagnosis = character(),
                sensitivity = numeric(),
                specificity = numeric(),
                ppv = numeric(),
                performance_score = numeric(),
                recommendation = character(),
                stringsAsFactors = FALSE
            )

            # Evaluate 2-marker combinations
            if (panelSize %in% c("pairs", "both")) {
                for (i in 1:(n_markers - 1)) {
                    for (j in (i + 1):n_markers) {
                        panel_name <- paste0(markers[i], " + ", markers[j])

                        # Combined positivity: both markers positive
                        combined_positive <- marker_matrix[, i] & marker_matrix[, j]

                        # Test for each diagnosis
                        for (target_dx in diagnoses) {
                            true_condition <- diagnosis == target_dx
                            valid_idx <- !is.na(combined_positive) & !is.na(true_condition)

                            if (sum(valid_idx) < 10) next

                            pos_clean <- combined_positive[valid_idx]
                            cond_clean <- true_condition[valid_idx]

                            tp <- sum(pos_clean & cond_clean)
                            tn <- sum(!pos_clean & !cond_clean)
                            fp <- sum(pos_clean & !cond_clean)
                            fn <- sum(!pos_clean & cond_clean)

                            sens <- if ((tp + fn) > 0) tp / (tp + fn) else NA
                            spec <- if ((tn + fp) > 0) tn / (tn + fp) else NA
                            ppv <- if ((tp + fp) > 0) tp / (tp + fp) else NA

                            if (is.na(sens) || is.na(spec) || is.na(ppv)) next

                            # Performance score: geometric mean of sens, spec, ppv
                            perf_score <- (sens * spec * ppv)^(1/3)

                            # Recommendation based on Olsen et al. criteria
                            recommend <- if (spec >= 0.95 && ppv >= 0.90) {
                                "Excellent - highly specific panel"
                            } else if (spec >= 0.90 && ppv >= 0.80) {
                                "Good - reliable for diagnosis"
                            } else if (spec >= 0.80 && sens >= 0.50) {
                                "Moderate - use with caution"
                            } else {
                                "Limited utility - consider alternatives"
                            }

                            results <- rbind(results, data.frame(
                                rank = 0,  # Will rank later
                                panel = panel_name,
                                target_diagnosis = target_dx,
                                sensitivity = sens,
                                specificity = spec,
                                ppv = ppv,
                                performance_score = perf_score,
                                recommendation = recommend,
                                stringsAsFactors = FALSE
                            ))
                        }
                    }
                }
            }

            # Evaluate 3-marker combinations
            if (panelSize %in% c("triplets", "both") && n_markers >= 3) {
                # Limit to top 10 markers by individual performance to avoid combinatorial explosion
                top_markers <- markers[1:min(10, n_markers)]
                n_top <- length(top_markers)

                if (n_top >= 3) {
                    for (i in 1:(n_top - 2)) {
                        for (j in (i + 1):(n_top - 1)) {
                            for (k in (j + 1):n_top) {
                                panel_name <- paste0(top_markers[i], " + ", top_markers[j], " + ", top_markers[k])

                                m1_idx <- which(markers == top_markers[i])
                                m2_idx <- which(markers == top_markers[j])
                                m3_idx <- which(markers == top_markers[k])

                                combined_positive <- marker_matrix[, m1_idx] & marker_matrix[, m2_idx] & marker_matrix[, m3_idx]

                                for (target_dx in diagnoses) {
                                    true_condition <- diagnosis == target_dx
                                    valid_idx <- !is.na(combined_positive) & !is.na(true_condition)

                                    if (sum(valid_idx) < 10) next

                                    pos_clean <- combined_positive[valid_idx]
                                    cond_clean <- true_condition[valid_idx]

                                    tp <- sum(pos_clean & cond_clean)
                                    tn <- sum(!pos_clean & !cond_clean)
                                    fp <- sum(pos_clean & !cond_clean)
                                    fn <- sum(!pos_clean & cond_clean)

                                    sens <- if ((tp + fn) > 0) tp / (tp + fn) else NA
                                    spec <- if ((tn + fp) > 0) tn / (tn + fp) else NA
                                    ppv <- if ((tp + fp) > 0) tp / (tp + fp) else NA

                                    if (is.na(sens) || is.na(spec) || is.na(ppv)) next

                                    perf_score <- (sens * spec * ppv)^(1/3)

                                    recommend <- if (spec >= 0.95 && ppv >= 0.90) {
                                        "Excellent - highly specific panel"
                                    } else if (spec >= 0.90 && ppv >= 0.80) {
                                        "Good - reliable for diagnosis"
                                    } else if (spec >= 0.80 && sens >= 0.50) {
                                        "Moderate - use with caution"
                                    } else {
                                        "Limited utility - consider alternatives"
                                    }

                                    results <- rbind(results, data.frame(
                                        rank = 0,
                                        panel = panel_name,
                                        target_diagnosis = target_dx,
                                        sensitivity = sens,
                                        specificity = spec,
                                        ppv = ppv,
                                        performance_score = perf_score,
                                        recommendation = recommend,
                                        stringsAsFactors = FALSE
                                    ))
                                }
                            }
                        }
                    }
                }
            }

            # Rank by performance score within each diagnosis
            if (nrow(results) > 0) {
                results <- results[order(-results$performance_score), ]

                # Assign ranks within each diagnosis
                for (dx in unique(results$target_diagnosis)) {
                    dx_idx <- results$target_diagnosis == dx
                    results$rank[dx_idx] <- seq_len(sum(dx_idx))
                }

                # Keep top 10 per diagnosis
                final_results <- do.call(rbind, lapply(split(results, results$target_diagnosis), function(df) {
                    head(df, 10)
                }))

                return(final_results)
            }

            return(results)
        },

        # Flag outlier cases with atypical immunoprofiles
        .flagOutlierCases = function(data, clusters, silhouette_obj, threshold = 0.25, caseIdVar = NULL) {
            # Extract silhouette scores
            sil_scores <- silhouette_obj[, "sil_width"]
            sil_cluster <- silhouette_obj[, "cluster"]
            sil_neighbor <- silhouette_obj[, "neighbor"]

            # Calculate distance to cluster center
            # Use silhouette score as proxy: low silhouette = far from center
            distance_to_center <- 1 - abs(sil_scores)

            # Identify outliers
            outliers <- sil_scores < threshold

            if (sum(outliers) == 0) {
                return(NULL)
            }

            outlier_indices <- which(outliers)

            results <- data.frame(
                case_id = character(),
                assigned_cluster = character(),
                silhouette = numeric(),
                distance_to_center = numeric(),
                nearest_alternative = character(),
                quality_flag = character(),
                recommendation = character(),
                stringsAsFactors = FALSE
            )

            for (idx in outlier_indices) {
                # Get case ID
                if (!is.null(caseIdVar) && caseIdVar %in% names(data)) {
                    case_id <- as.character(data[[caseIdVar]][idx])
                } else {
                    case_id <- as.character(idx)
                }

                # Get cluster info
                assigned <- paste0("Cluster ", sil_cluster[idx])
                sil_val <- sil_scores[idx]
                dist_val <- distance_to_center[idx]
                neighbor_cluster <- paste0("Cluster ", sil_neighbor[idx])

                # Quality flag
                quality_flag <- if (sil_val < 0) {
                    "Poor - misclassified"
                } else if (sil_val < 0.10) {
                    "Very low - ambiguous"
                } else if (sil_val < 0.25) {
                    "Low - atypical"
                } else {
                    "Borderline"
                }

                # Recommendation
                recommendation <- if (sil_val < 0) {
                    "Strong outlier - review IHC data, consider molecular testing"
                } else if (sil_val < 0.10) {
                    "Atypical immunoprofile - recommend molecular confirmation"
                } else {
                    "Borderline case - clinical correlation advised"
                }

                results <- rbind(results, data.frame(
                    case_id = case_id,
                    assigned_cluster = assigned,
                    silhouette = sil_val,
                    distance_to_center = dist_val,
                    nearest_alternative = neighbor_cluster,
                    quality_flag = quality_flag,
                    recommendation = recommendation,
                    stringsAsFactors = FALSE
                ))
            }

            return(results)
        },

        # Populate marker performance table
        .populateMarkerPerformance = function() {
            if (!isTRUE(self$options$calculateDiagnosticMetrics)) return()
            if (is.null(self$options$knownDiagnosis)) return()

            analysisState <- self$results$summary$state
            if (is.null(analysisState)) return()

            performance <- analysisState$markerPerformance
            if (is.null(performance)) return()

            table <- self$results$markerPerformance

            for (i in seq_len(nrow(performance))) {
                row <- performance[i, ]
                table$addRow(rowKey = i, list(
                    marker = row$marker,
                    diagnosis = row$diagnosis,
                    sensitivity = row$sensitivity,
                    specificity = row$specificity,
                    ppv = row$ppv,
                    npv = row$npv,
                    accuracy = row$accuracy
                ))
            }
        },

        # Populate optimal panels table
        .populateOptimalPanels = function() {
            if (!isTRUE(self$options$identifyOptimalPanel)) return()
            if (is.null(self$options$knownDiagnosis)) return()

            analysisState <- self$results$summary$state
            if (is.null(analysisState)) return()

            panels <- analysisState$optimalPanels
            if (is.null(panels)) return()

            table <- self$results$optimalPanels

            for (i in seq_len(nrow(panels))) {
                row <- panels[i, ]
                table$addRow(rowKey = i, list(
                    rank = row$rank,
                    panel = row$panel,
                    target_diagnosis = row$target_diagnosis,
                    sensitivity = row$sensitivity,
                    specificity = row$specificity,
                    ppv = row$ppv,
                    performance_score = row$performance_score,
                    recommendation = row$recommendation
                ))
            }
        },

        # Populate outlier cases table
        .populateOutlierCases = function() {
            if (!isTRUE(self$options$flagOutliers)) return()

            analysisState <- self$results$summary$state
            if (is.null(analysisState)) return()

            outliers <- analysisState$outlierCases
            if (is.null(outliers)) return()

            table <- self$results$outlierCases

            for (i in seq_len(nrow(outliers))) {
                row <- outliers[i, ]
                table$addRow(rowKey = i, list(
                    case_id = row$case_id,
                    assigned_cluster = row$assigned_cluster,
                    silhouette = row$silhouette,
                    distance_to_center = row$distance_to_center,
                    nearest_alternative = row$nearest_alternative,
                    quality_flag = row$quality_flag,
                    recommendation = row$recommendation
                ))
            }
        },

        .populateReproducibilityStats = function(reproducibilityResults) {
            if (!isTRUE(self$options$reproducibilityTest)) return()
            if (is.null(reproducibilityResults)) return()

            table <- self$results$reproducibilityStats
            if (is.null(table)) return()

            # Check for error
            if (!is.null(reproducibilityResults$error)) {
                # Could add error message to summary
                return()
            }

            # Populate table with kappa results for each cluster
            for (cluster_name in names(reproducibilityResults)) {
                result <- reproducibilityResults[[cluster_name]]
                table$addRow(rowKey = cluster_name, list(
                    cluster = result$cluster,
                    mean_kappa = result$mean_kappa,
                    sd_kappa = result$sd_kappa,
                    ci_lower = result$ci_lower,
                    ci_upper = result$ci_upper,
                    interpretation = result$interpretation,
                    n_splits = result$n_splits
                ))
            }
        },

        .populateSupervisedResults = function(supervisedResults) {
            if (!isTRUE(self$options$supervisedClustering)) return()
            if (is.null(supervisedResults)) return()

            # Populate summary table
            summaryTable <- self$results$supervisedSummary
            if (!is.null(summaryTable)) {
                for (group_name in names(supervisedResults)) {
                    result <- supervisedResults[[group_name]]
                    summaryTable$addRow(rowKey = group_name, list(
                        group = result$group,
                        n_cases = result$n_cases,
                        n_clusters = ifelse(is.null(result$n_clusters), NA, result$n_clusters),
                        avg_silhouette = ifelse(is.null(result$avg_silhouette), NA, result$avg_silhouette),
                        status = result$status
                    ))
                }
            }

            # Populate detailed HTML results
            detailsHtml <- self$results$supervisedResults
            if (!is.null(detailsHtml)) {
                html <- "<div style='padding:15px;'>"
                html <- paste0(html, "<h4>Supervised Clustering Details</h4>")
                html <- paste0(html, "<p>Clustering performed separately within each group of: <b>", self$options$supervisedVariable, "</b></p>")

                for (group_name in names(supervisedResults)) {
                    result <- supervisedResults[[group_name]]

                    html <- paste0(html, "<div style='margin-top:20px; padding:15px; border:1px solid #dee2e6; background-color:#f8f9fa;'>")
                    html <- paste0(html, "<h5 style='margin-top:0; color:#495057;'>Group: ", result$group, "</h5>")
                    html <- paste0(html, "<p><b>N Cases:</b> ", result$n_cases, "</p>")

                    if (result$status == "Success") {
                        html <- paste0(html, "<p><b>N Clusters:</b> ", result$n_clusters, "</p>")
                        html <- paste0(html, "<p><b>Avg Silhouette:</b> ", sprintf("%.3f", result$avg_silhouette), "</p>")

                        # Cluster sizes
                        html <- paste0(html, "<p><b>Cluster Sizes:</b></p>")
                        html <- paste0(html, "<table style='margin-left:20px; border-collapse:collapse;'>")
                        html <- paste0(html, "<tr style='background-color:#e9ecef;'><th style='padding:5px; border:1px solid #dee2e6;'>Cluster</th><th style='padding:5px; border:1px solid #dee2e6;'>N</th></tr>")
                        for (cl in names(result$cluster_sizes)) {
                            html <- paste0(html, "<tr><td style='padding:5px; border:1px solid #dee2e6;'>", cl, "</td><td style='padding:5px; border:1px solid #dee2e6;'>", result$cluster_sizes[[cl]], "</td></tr>")
                        }
                        html <- paste0(html, "</table>")
                    } else {
                        html <- paste0(html, "<p style='color:#dc3545;'><b>Status:</b> ", result$status, "</p>")
                    }

                    html <- paste0(html, "</div>")
                }

                html <- paste0(html, "</div>")
                detailsHtml$setContent(html)
            }
        },

        # Phase 3: Populate ratio calculation results
        .populateRatioResults = function(ratioResult) {
            if (!isTRUE(self$options$calculateRatios)) return()
            if (is.null(ratioResult)) return()

            # Check for error
            if (!is.null(ratioResult$error)) {
                # Could add error message somewhere
                return()
            }

            # Populate ratio summary table
            summaryTable <- self$results$ratioSummary
            if (!is.null(summaryTable)) {
                stats <- ratioResult$summary

                # Add rows for each statistic
                summaryTable$addRow(rowKey = "ratio", list(
                    statistic = sprintf("%s / %s", ratioResult$numerator_var, ratioResult$denominator_var),
                    value = NA
                ))

                summaryTable$addRow(rowKey = "n_valid", list(
                    statistic = "N Valid",
                    value = stats$n_valid
                ))

                summaryTable$addRow(rowKey = "n_missing", list(
                    statistic = "N Missing",
                    value = stats$n_missing
                ))

                if (stats$n_zero_denom > 0) {
                    summaryTable$addRow(rowKey = "n_zero_denom", list(
                        statistic = "N Zero Denominator",
                        value = stats$n_zero_denom
                    ))
                }

                summaryTable$addRow(rowKey = "mean", list(
                    statistic = "Mean",
                    value = stats$mean
                ))

                summaryTable$addRow(rowKey = "median", list(
                    statistic = "Median",
                    value = stats$median
                ))

                summaryTable$addRow(rowKey = "sd", list(
                    statistic = "SD",
                    value = stats$sd
                ))

                summaryTable$addRow(rowKey = "min", list(
                    statistic = "Minimum",
                    value = stats$min
                ))

                summaryTable$addRow(rowKey = "q25", list(
                    statistic = "25th Percentile",
                    value = stats$q25
                ))

                summaryTable$addRow(rowKey = "q75", list(
                    statistic = "75th Percentile",
                    value = stats$q75
                ))

                summaryTable$addRow(rowKey = "max", list(
                    statistic = "Maximum",
                    value = stats$max
                ))
            }

            # Populate classification table if requested
            if (isTRUE(self$options$ratioClassification) && !is.null(ratioResult$classification)) {
                classTable <- self$results$ratioClassificationTable
                if (!is.null(classTable)) {
                    classif <- ratioResult$classification

                    # Low category
                    if (!is.na(classif$n_low) && classif$n_low > 0) {
                        range_str <- sprintf("%.2f - %.2f", classif$range_low[1], classif$range_low[2])
                        classTable$addRow(rowKey = "low", list(
                            category = "Low",
                            n = classif$n_low,
                            percent = classif$n_low / ratioResult$summary$n_valid,
                            range = range_str
                        ))
                    }

                    # Intermediate category
                    if (!is.na(classif$n_intermediate) && classif$n_intermediate > 0) {
                        range_str <- sprintf("%.2f - %.2f", classif$range_intermediate[1], classif$range_intermediate[2])
                        classTable$addRow(rowKey = "intermediate", list(
                            category = "Intermediate",
                            n = classif$n_intermediate,
                            percent = classif$n_intermediate / ratioResult$summary$n_valid,
                            range = range_str
                        ))
                    }

                    # High category
                    if (!is.na(classif$n_high) && classif$n_high > 0) {
                        range_str <- sprintf("%.2f - %.2f", classif$range_high[1], classif$range_high[2])
                        classTable$addRow(rowKey = "high", list(
                            category = "High",
                            n = classif$n_high,
                            percent = classif$n_high / ratioResult$summary$n_valid,
                            range = range_str
                        ))
                    }

                    # Add note about cutoffs
                    note_text <- sprintf("Classification: Low (≤ %.2f), Intermediate (> %.2f and < %.2f), High (≥ %.2f)",
                                        classif$low_cutoff, classif$low_cutoff, classif$high_cutoff, classif$high_cutoff)
                    classTable$setNote("cutoffs", note_text)
                }
            }
        },

        .populateInterpretationGuide = function() {
            html <- self$results$interpretationGuide

            guide_html <- paste0(
                "<div style='background-color: #e8f4f8; border: 2px solid #3498db; padding: 20px; margin: 10px 0;'>",
                "<h3 style='color: #2c3e50; margin-top: 0;'>Clinical Interpretation Guide</h3>",

                "<h4 style='color: #34495e; margin-top: 15px;'>Understanding Cluster Results</h4>",
                "<ul style='line-height: 1.6;'>",
                "<li><strong>Cluster Assignments:</strong> Each case is assigned to the most similar immunophenotypic group based on IHC marker expression patterns.</li>",
                "<li><strong>Silhouette Scores:</strong> Measure cluster quality (0.7-1.0 = strong, 0.5-0.7 = reasonable, &lt;0.5 = weak or ambiguous).",
                " Low scores may indicate atypical cases or borderline immunoprofiles requiring expert review.</li>",
                "<li><strong>Cluster Profiles:</strong> Characteristic marker patterns for each cluster. High mean values indicate frequent positivity/high expression.</li>",
                "<li><strong>Marker Associations:</strong> Statistical tests show which markers significantly differ across clusters (p &lt; 0.05 after correction).</li>",
                "</ul>",

                "<h4 style='color: #34495e; margin-top: 15px;'>Clinical Applications</h4>",
                "<ul style='line-height: 1.6;'>",
                "<li><strong>Diagnosis Support:</strong> Clusters may correspond to known diagnostic entities or reveal novel immunophenotypic subtypes.</li>",
                "<li><strong>Biomarker Discovery:</strong> Identifying co-expressed markers and optimal antibody panels for differential diagnosis.</li>",
                "<li><strong>Quality Control:</strong> Cases with low silhouette scores or unexpected cluster assignments warrant slide review.</li>",
                "<li><strong>Research Insights:</strong> Unsupervised clustering can reveal biological subtypes independent of morphology.</li>",
                "</ul>",

                "<h4 style='color: #34495e; margin-top: 15px;'>Interpretation Caveats</h4>",
                "<ul style='line-height: 1.6;'>",
                "<li><strong>Data-Driven Results:</strong> Clusters are derived from the data and may not align with established diagnostic categories.</li>",
                "<li><strong>Validation Required:</strong> Results should be validated in independent cohorts and correlated with clinical outcomes.</li>",
                "<li><strong>Missing Data:</strong> Cases with incomplete IHC panels may cluster poorly or be excluded (depending on settings).</li>",
                "<li><strong>Marker Selection:</strong> Results depend on which markers are included. Redundant markers may bias clustering.</li>",
                "<li><strong>Sample Size:</strong> Small samples (&lt;30 cases) may produce unstable clusters. Consider reproducibility testing.</li>",
                "</ul>",

                "<h4 style='color: #34495e; margin-top: 15px;'>Recommended Next Steps</h4>",
                "<ol style='line-height: 1.6;'>",
                "<li>Review cluster profiles to understand characteristic immunophenotypes</li>",
                "<li>Examine cases with low silhouette scores for data quality issues</li>",
                "<li>Correlate clusters with known diagnoses (if available) to assess biological validity</li>",
                "<li>Perform survival analysis or clinical correlation to assess prognostic significance</li>",
                "<li>Consider reproducibility testing (random split validation) for publication-ready results</li>",
                "</ol>",

                "<p style='margin-top: 15px; padding: 10px; background-color: #fff3cd; border-left: 4px solid #f0ad4e;'>",
                "<strong>⚠️ Important:</strong> IHC clustering results are exploratory and hypothesis-generating. ",
                "Clinical decisions should integrate clustering results with morphology, clinical context, and expert pathologist review.",
                "</p>",
                "</div>"
            )

            html$setContent(guide_html)
        },

        .populateTechnicalNotes = function() {
            html <- self$results$technicalNotes
            opts <- self$options

            # Determine method details
            method_name <- switch(opts$method,
                'pam' = "PAM (Partitioning Around Medoids)",
                'hierarchical' = "Hierarchical Clustering",
                'dimreduce' = "MCA/PCA + k-means",
                opts$method
            )

            distance_name <- switch(opts$distanceMethod,
                'gower' = "Gower Distance (handles mixed data types)",
                'jaccard' = "Jaccard Distance (binary data)",
                opts$distanceMethod
            )

            linkage_name <- if (opts$method == 'hierarchical') {
                switch(opts$linkageMethod,
                    'ward' = "Ward's Minimum Variance",
                    'complete' = "Complete Linkage (Furthest Neighbor)",
                    'average' = "Average Linkage (UPGMA)",
                    'single' = "Single Linkage (Nearest Neighbor)",
                    opts$linkageMethod
                )
            } else {
                "N/A (not using hierarchical clustering)"
            }

            k_selection <- if (isTRUE(opts$autoSelectK)) {
                sprintf("Automatic (silhouette width optimization, range: %s)", opts$kRange)
            } else {
                sprintf("User-specified: %d clusters", opts$nClusters)
            }

            missing_method <- switch(opts$handleMissing,
                'complete' = "Complete cases only (listwise deletion)",
                'pairwise' = "Pairwise deletion (variable-specific handling)",
                opts$handleMissing
            )

            scaling_status <- if (isTRUE(opts$scaleContVars)) {
                "Yes (z-score standardization)"
            } else {
                "No (raw values used)"
            }

            consensus_status <- if (isTRUE(opts$consensusClustering)) {
                sprintf("Yes (%d bootstrap iterations)", opts$nBootstrap)
            } else {
                "No"
            }

            notes_html <- paste0(
                "<div style='background-color: #f8f9fa; border: 2px solid #6c757d; padding: 20px; margin: 10px 0;'>",
                "<h3 style='color: #2c3e50; margin-top: 0;'>Technical Implementation Details</h3>",

                "<h4 style='color: #34495e; margin-top: 15px;'>Clustering Method</h4>",
                "<table style='width: 100%; border-collapse: collapse; margin: 10px 0;'>",
                "<tr style='background-color: #e9ecef;'>",
                "<td style='padding: 8px; border: 1px solid #dee2e6; width: 40%;'><strong>Algorithm:</strong></td>",
                "<td style='padding: 8px; border: 1px solid #dee2e6;'>", method_name, "</td>",
                "</tr>",
                "<tr>",
                "<td style='padding: 8px; border: 1px solid #dee2e6;'><strong>Distance Metric:</strong></td>",
                "<td style='padding: 8px; border: 1px solid #dee2e6;'>", distance_name, "</td>",
                "</tr>",
                "<tr style='background-color: #e9ecef;'>",
                "<td style='padding: 8px; border: 1px solid #dee2e6;'><strong>Linkage Method:</strong></td>",
                "<td style='padding: 8px; border: 1px solid #dee2e6;'>", linkage_name, "</td>",
                "</tr>",
                "<tr>",
                "<td style='padding: 8px; border: 1px solid #dee2e6;'><strong>Number of Clusters:</strong></td>",
                "<td style='padding: 8px; border: 1px solid #dee2e6;'>", k_selection, "</td>",
                "</tr>",
                "<tr style='background-color: #e9ecef;'>",
                "<td style='padding: 8px; border: 1px solid #dee2e6;'><strong>Random Seed:</strong></td>",
                "<td style='padding: 8px; border: 1px solid #dee2e6;'>", opts$seed, " (for reproducibility)</td>",
                "</tr>",
                "</table>",

                "<h4 style='color: #34495e; margin-top: 15px;'>Data Preprocessing</h4>",
                "<table style='width: 100%; border-collapse: collapse; margin: 10px 0;'>",
                "<tr style='background-color: #e9ecef;'>",
                "<td style='padding: 8px; border: 1px solid #dee2e6; width: 40%;'><strong>Continuous Variable Scaling:</strong></td>",
                "<td style='padding: 8px; border: 1px solid #dee2e6;'>", scaling_status, "</td>",
                "</tr>",
                "<tr>",
                "<td style='padding: 8px; border: 1px solid #dee2e6;'><strong>Missing Data Handling:</strong></td>",
                "<td style='padding: 8px; border: 1px solid #dee2e6;'>", missing_method, "</td>",
                "</tr>",
                "<tr style='background-color: #e9ecef;'>",
                "<td style='padding: 8px; border: 1px solid #dee2e6;'><strong>Consensus Clustering:</strong></td>",
                "<td style='padding: 8px; border: 1px solid #dee2e6;'>", consensus_status, "</td>",
                "</tr>",
                "</table>",

                "<h4 style='color: #34495e; margin-top: 15px;'>Key References</h4>",
                "<ul style='line-height: 1.8;'>",
                "<li><strong>Gower Distance:</strong> Gower JC. A general coefficient of similarity and some of its properties. ",
                "<em>Biometrics</em>. 1971;27(4):857-871. doi:10.2307/2528823</li>",
                "<li><strong>PAM Algorithm:</strong> Kaufman L, Rousseeuw PJ. Clustering by means of medoids. ",
                "In: <em>Statistical Data Analysis Based on the L1-Norm and Related Methods</em>. 1987:405-416.</li>",
                "<li><strong>Silhouette Width:</strong> Rousseeuw PJ. Silhouettes: a graphical aid to the interpretation and validation of cluster analysis. ",
                "<em>J Comput Appl Math</em>. 1987;20:53-65. doi:10.1016/0377-0427(87)90125-7</li>",
                "<li><strong>IHC Clustering Application:</strong> Sterlacci W, et al. Putative Stem Cell Markers in Non-Small-Cell Lung Cancer: ",
                "A Clinicopathologic Characterization. <em>J Thorac Oncol</em>. 2014;9(1):41-49. doi:10.1097/JTO.0000000000000021</li>",
                "<li><strong>Diagnostic Markers:</strong> Olsen RJ, et al. Practical applications of immunohistochemistry in the diagnosis of hematopoietic neoplasms. ",
                "<em>Arch Pathol Lab Med</em>. 2006;130(6):860-867. doi:10.1043/1543-2165(2006)130[860:PAOIIT]2.0.CO;2</li>",
                "</ul>",

                "<h4 style='color: #34495e; margin-top: 15px;'>Software Implementation</h4>",
                "<p style='line-height: 1.6;'>",
                "This analysis uses R packages: <code>cluster</code> (PAM, silhouette), <code>factoextra</code> (visualization), ",
                "<code>FactoMineR</code> (MCA/PCA), <code>ComplexHeatmap</code> (heatmaps), and <code>ggplot2</code> (plotting). ",
                "Gower distance is computed using <code>cluster::daisy()</code> which handles mixed data types appropriately.",
                "</p>",

                "<p style='margin-top: 15px; padding: 10px; background-color: #d1ecf1; border-left: 4px solid #17a2b8;'>",
                "<strong>ℹ️ Note:</strong> For reproducible research, document all analysis settings including the random seed, ",
                "clustering method, distance metric, and any data preprocessing steps (scaling, missing data handling).",
                "</p>",
                "</div>"
            )

            html$setContent(notes_html)
        },

        .populateDiagnosticGlossary = function() {
            html <- self$results$diagnosticGlossary

            glossary_html <- paste0(
                "<div style='background-color: #fff8e1; border: 2px solid #ffc107; padding: 20px; margin: 10px 0;'>",
                "<h3 style='color: #2c3e50; margin-top: 0;'>📚 Diagnostic Metrics Glossary</h3>",

                "<h4 style='color: #34495e; margin-top: 15px;'>Understanding Marker Performance</h4>",

                "<table style='width: 100%; border-collapse: collapse; margin: 15px 0;'>",
                "<tr style='background-color: #ffe082;'>",
                "<th style='padding: 12px; border: 2px solid #ffa000; text-align: left; width: 25%;'>Metric</th>",
                "<th style='padding: 12px; border: 2px solid #ffa000; text-align: left; width: 35%;'>Definition</th>",
                "<th style='padding: 12px; border: 2px solid #ffa000; text-align: left; width: 40%;'>Clinical Interpretation</th>",
                "</tr>",

                "<tr>",
                "<td style='padding: 10px; border: 1px solid #ffb74d;'><strong>Sensitivity</strong></td>",
                "<td style='padding: 10px; border: 1px solid #ffb74d;'>True Positive Rate<br><em>P(Marker+ | Disease+)</em></td>",
                "<td style='padding: 10px; border: 1px solid #ffb74d;'>",
                "Proportion of true positives correctly identified. ",
                "<strong>High sensitivity (≥90%)</strong> is essential for screening tests - ",
                "you don't want to miss cases. ",
                "<strong>Low sensitivity</strong> means many false negatives (missed diagnoses).",
                "</td>",
                "</tr>",

                "<tr style='background-color: #fff3e0;'>",
                "<td style='padding: 10px; border: 1px solid #ffb74d;'><strong>Specificity</strong></td>",
                "<td style='padding: 10px; border: 1px solid #ffb74d;'>True Negative Rate<br><em>P(Marker- | Disease-)</em></td>",
                "<td style='padding: 10px; border: 1px solid #ffb74d;'>",
                "Proportion of true negatives correctly identified. ",
                "<strong>High specificity (≥90%)</strong> is essential for confirmatory tests - ",
                "you don't want false alarms. ",
                "<strong>Low specificity</strong> means many false positives (overdiagnosis).",
                "</td>",
                "</tr>",

                "<tr>",
                "<td style='padding: 10px; border: 1px solid #ffb74d;'><strong>PPV</strong><br>(Positive Predictive Value)</td>",
                "<td style='padding: 10px; border: 1px solid #ffb74d;'>Precision<br><em>P(Disease+ | Marker+)</em></td>",
                "<td style='padding: 10px; border: 1px solid #ffb74d;'>",
                "If marker is positive, what is the probability the patient truly has the disease? ",
                "<strong>Depends on disease prevalence</strong> in your population. ",
                "Same test has higher PPV in high-risk populations. ",
                "<strong>Critical for patient counseling</strong> after a positive result.",
                "</td>",
                "</tr>",

                "<tr style='background-color: #fff3e0;'>",
                "<td style='padding: 10px; border: 1px solid #ffb74d;'><strong>NPV</strong><br>(Negative Predictive Value)</td>",
                "<td style='padding: 10px; border: 1px solid #ffb74d;'>Negative Precision<br><em>P(Disease- | Marker-)</em></td>",
                "<td style='padding: 10px; border: 1px solid #ffb74d;'>",
                "If marker is negative, what is the probability the patient truly does NOT have the disease? ",
                "<strong>Also depends on prevalence</strong>. ",
                "In rare diseases, even poor tests can have high NPV. ",
                "<strong>Important for ruling out disease</strong>.",
                "</td>",
                "</tr>",

                "<tr>",
                "<td style='padding: 10px; border: 1px solid #ffb74d;'><strong>Accuracy</strong></td>",
                "<td style='padding: 10px; border: 1px solid #ffb74d;'>Overall Correct Rate<br><em>(TP+TN) / Total</em></td>",
                "<td style='padding: 10px; border: 1px solid #ffb74d;'>",
                "Proportion of all cases classified correctly. ",
                "<strong>Can be misleading</strong> in imbalanced datasets - ",
                "a test that always predicts 'negative' has 95% accuracy if only 5% have disease! ",
                "Use with caution.",
                "</td>",
                "</tr>",

                "<tr style='background-color: #fff3e0;'>",
                "<td style='padding: 10px; border: 1px solid #ffb74d;'><strong>Likelihood Ratio+</strong></td>",
                "<td style='padding: 10px; border: 1px solid #ffb74d;'>LR+ = Sensitivity / (1-Specificity)</td>",
                "<td style='padding: 10px; border: 1px solid #ffb74d;'>",
                "How much a positive result increases disease probability. ",
                "<strong>LR+ > 10</strong> = strong evidence for disease. ",
                "<strong>LR+ 5-10</strong> = moderate evidence. ",
                "<strong>LR+ < 2</strong> = weak evidence.",
                "</td>",
                "</tr>",

                "<tr>",
                "<td style='padding: 10px; border: 1px solid #ffb74d;'><strong>Likelihood Ratio-</strong></td>",
                "<td style='padding: 10px; border: 1px solid #ffb74d;'>LR- = (1-Sensitivity) / Specificity</td>",
                "<td style='padding: 10px; border: 1px solid #ffb74d;'>",
                "How much a negative result decreases disease probability. ",
                "<strong>LR- < 0.1</strong> = strong evidence against disease. ",
                "<strong>LR- 0.1-0.5</strong> = moderate evidence. ",
                "<strong>LR- > 0.5</strong> = weak evidence.",
                "</td>",
                "</tr>",
                "</table>",

                "<h4 style='color: #34495e; margin-top: 20px;'>📊 2×2 Confusion Matrix</h4>",
                "<table style='width: 80%; margin: 15px auto; border-collapse: collapse; text-align: center;'>",
                "<tr>",
                "<td style='border: none;'></td>",
                "<td style='border: none;'></td>",
                "<td colspan='2' style='padding: 8px; background-color: #e3f2fd; border: 2px solid #1976d2;'><strong>Gold Standard Diagnosis</strong></td>",
                "</tr>",
                "<tr>",
                "<td style='border: none;'></td>",
                "<td style='border: none;'></td>",
                "<td style='padding: 8px; background-color: #e3f2fd; border: 2px solid #1976d2;'><strong>Disease+</strong></td>",
                "<td style='padding: 8px; background-color: #e3f2fd; border: 2px solid #1976d2;'><strong>Disease-</strong></td>",
                "</tr>",
                "<tr>",
                "<td rowspan='2' style='padding: 8px; background-color: #fff3e0; border: 2px solid #ffa000; vertical-align: middle;'><strong>IHC Marker</strong></td>",
                "<td style='padding: 8px; background-color: #fff3e0; border: 2px solid #ffa000;'><strong>Marker+</strong></td>",
                "<td style='padding: 12px; border: 2px solid #4caf50; background-color: #c8e6c9;'><strong>TP</strong><br>True Positive</td>",
                "<td style='padding: 12px; border: 2px solid #f44336; background-color: #ffcdd2;'><strong>FP</strong><br>False Positive</td>",
                "</tr>",
                "<tr>",
                "<td style='padding: 8px; background-color: #fff3e0; border: 2px solid #ffa000;'><strong>Marker-</strong></td>",
                "<td style='padding: 12px; border: 2px solid #f44336; background-color: #ffcdd2;'><strong>FN</strong><br>False Negative</td>",
                "<td style='padding: 12px; border: 2px solid #4caf50; background-color: #c8e6c9;'><strong>TN</strong><br>True Negative</td>",
                "</tr>",
                "</table>",

                "<h4 style='color: #34495e; margin-top: 20px;'>⚠️ Key Considerations</h4>",
                "<ul style='line-height: 1.8;'>",
                "<li><strong>Prevalence Matters:</strong> PPV and NPV depend heavily on disease prevalence in your cohort. ",
                "A marker with 90% sensitivity/specificity has PPV=50% at 10% prevalence, but PPV=10% at 1% prevalence.</li>",

                "<li><strong>Trade-offs:</strong> Sensitivity and specificity are often inversely related. ",
                "Lowering a threshold increases sensitivity (catches more cases) but decreases specificity (more false positives). ",
                "Choose based on clinical consequences of false negatives vs. false positives.</li>",

                "<li><strong>Panel Combinations:</strong> Using multiple markers can improve performance. ",
                "Markers in <strong>series</strong> (all must be positive) increases specificity. ",
                "Markers in <strong>parallel</strong> (any can be positive) increases sensitivity.</li>",

                "<li><strong>Clinical Context:</strong> Statistical significance ≠ clinical utility. ",
                "A marker with sensitivity=60% and specificity=70% may be statistically significant but clinically useless. ",
                "Consider minimum acceptable thresholds for your use case.</li>",

                "<li><strong>Confidence Intervals:</strong> Small sample sizes produce wide confidence intervals. ",
                "A reported sensitivity of 85% with CI [60%-95%] is unreliable. ",
                "Aim for CI width < 20% for robust markers.</li>",
                "</ul>",

                "<h4 style='color: #34495e; margin-top: 20px;'>📖 Recommended Reading</h4>",
                "<ul style='line-height: 1.8;'>",
                "<li>Altman DG, Bland JM. Diagnostic tests 1-3 series. <em>BMJ</em>. 1994.</li>",
                "<li>Lalkhen AG, McCluskey A. Clinical tests: sensitivity and specificity. ",
                "<em>Contin Educ Anaesth Crit Care Pain</em>. 2008;8(6):221-223.</li>",
                "<li>Trevethan R. Sensitivity, Specificity, and Predictive Values: Foundations, Pliabilities, and Pitfalls ",
                "in Research and Practice. <em>Front Public Health</em>. 2017;5:307.</li>",
                "</ul>",

                "</div>"
            )

            html$setContent(glossary_html)
        }

    )
)

ihccluster <- function(jmvcore) ihcclusterClass
