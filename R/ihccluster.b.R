#' @title IHC Clustering Analysis Backend
#' @description
#' Backend implementation for IHC clustering analysis. Clusters cases based on
#' immunohistochemistry (IHC) staining patterns using various clustering algorithms
#' optimized for mixed categorical and continuous data.
#'
#' @details
#' This function supports multiple clustering approaches:
#' \itemize{
#'   \item PAM (k-medoids) on Gower distance - best for mixed data types
#'   \item Hierarchical clustering (Ward) on Gower distance - shows relationships
#'   \item MCA/PCA + k-means - dimension reduction approach
#' }
#'
#' The analysis automatically handles mixed data types using Gower distance,
#' which appropriately weights categorical and continuous variables.
#'
#' @section Features:
#' \itemize{
#'   \item Automatic optimal k selection using silhouette analysis
#'   \item Comprehensive visualization suite (heatmaps, dendrograms, PCA plots)
#'   \item Consensus clustering for stability assessment
#'   \item Clinical correlation analysis
#'   \item Variable weighting support
#'   \item Missing data handling (complete cases or pairwise distances)
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
        
        .init = function() {
            if (is.null(self$data))
                return()
                
            # Initialize dynamic result tables
            private$.initTodo()
            private$.initTechnicalNotes()
            private$.initInterpretationGuide()
        },

        .run = function() {
            
            if (is.null(self$data))
                return()
                
            data <- self$data
            opts <- self$options
            `%||%` <- function(x, y) if (is.null(x)) y else x

            # Unified variable handling - merge all categorical variables and treat separately from continuous
            catVars <- character(0)
            contVars <- character(0)
            
            # Collect all categorical variables
            if (!is.null(opts$catVars) && length(opts$catVars) > 0) {
                catVars <- c(catVars, opts$catVars)
            }
            
            # Collect continuous variables
            if (!is.null(opts$contVars) && length(opts$contVars) > 0) {
                contVars <- opts$contVars
            }
            
            # Remove duplicates and create final variable list
            catVars <- unique(catVars)
            contVars <- unique(contVars)
            allVars <- c(catVars, contVars)
                
            # Check for minimum variables
            if (length(allVars) < 2) {
                self$results$summary$setContent(
                    "<p><b>Error:</b> Please specify at least 2 IHC markers (categorical or continuous).</p>"
                )
                return()
            }
                
            # Handle missing data
            if (opts$handleMissing %||% "pairwise" == "complete") {
                df <- jmvcore::naOmit(data[, allVars, drop=FALSE])
            } else {
                df <- data[, allVars, drop=FALSE]
            }
                
            # Process categorical variables as factors
            if (length(catVars) > 0) {
                df[catVars] <- lapply(df[catVars], function(x) as.factor(x))
            }
                
            # Process continuous variables (scale if requested)  
            if (length(contVars) > 0) {
                # Validate continuous variables
                for (var in contVars) {
                    if (!is.numeric(df[[var]])) {
                        stop(sprintf("Variable '%s' must be numeric for continuous analysis", var))
                    }
                    # Check for constant values
                    uniqueVals <- unique(na.omit(df[[var]]))
                    if (length(uniqueVals) == 1) {
                        warning(sprintf("Variable '%s' has constant values and may not contribute to clustering", var))
                    }
                    # Check for extreme outliers
                    if (length(uniqueVals) > 1) {
                        q <- quantile(df[[var]], c(0.01, 0.99), na.rm = TRUE)
                        if ((q[2] - q[1]) / mean(df[[var]], na.rm = TRUE) > 10) {
                            warning(sprintf("Variable '%s' has extreme outliers that may affect clustering", var))
                        }
                    }
                }
                
                if (isTRUE(opts$scaleContVars)) {
                    df[contVars] <- lapply(df[contVars], function(x) {
                        if (is.numeric(x) && sd(x, na.rm = TRUE) > 0) {
                            as.numeric(scale(x))
                        } else x
                    })
                }
            }
            
            # Additional validation
            if (nrow(df) < 5) {
                self$results$summary$setContent(
                    "<p><b>Error:</b> Insufficient data after handling missing values. Need at least 5 cases.</p>"
                )
                return()
            }

            set.seed(opts$seed %||% 42)

            k <- opts$nClusters %||% 3
            method <- opts$method %||% "pam"

            text_lines <- list()
            clusters <- NULL
            usedK <- NULL

            # Parse weights if provided
            weights <- NULL
            if (!is.null(opts$weights) && opts$weights != "" && nchar(opts$weights) > 0) {
                tryCatch({
                    w <- as.numeric(strsplit(opts$weights, ",")[[1]])
                    if (length(w) == length(allVars)) {
                        weights <- w
                        names(weights) <- allVars
                    } else if (length(w) == 1) {
                        # Single weight value - use for all variables
                        weights <- rep(w, length(allVars))
                        names(weights) <- allVars
                    } else {
                        warning(sprintf("Weights length (%d) doesn't match number of variables (%d). Ignoring weights.", length(w), length(allVars)))
                        weights <- NULL
                    }
                }, error = function(e) {
                    warning(sprintf("Error parsing weights: %s. Ignoring weights.", e$message))
                    weights <- NULL
                })
            }

            if (method == "pam") {
                if (is.null(weights)) {
                    d <- cluster::daisy(df, metric="gower")
                } else {
                    d <- cluster::daisy(df, metric="gower", weights=weights)
                }
                
                # Auto k selection with new range options
                if (isTRUE(opts$autoSelectK) || is.null(k)) {
                    kRange <- switch(opts$kRange %||% "medium",
                                   "small" = 2:6,
                                   "medium" = 2:8, 
                                   "large" = 2:12,
                                   2:8)
                    
                    # Calculate silhouette for each k and populate table directly
                    sils <- sapply(kRange, function(kk) {
                        fit <- cluster::pam(d, k=kk, diss=TRUE)
                        mean(cluster::silhouette(fit)[, "sil_width"])
                    })
                    
                    usedK <- kRange[which.max(sils)]
                    text_lines <- c(text_lines, sprintf("Auto k (silhouette): %d", usedK))
                    
                    # Populate silhouette statistics table immediately
                    if (!is.null(self$results$silhouetteStats)) {
                        silTable <- self$results$silhouetteStats
                        
                        for (i in seq_along(kRange)) {
                            selected_marker <- if (kRange[i] == usedK) "✓" else ""
                            silTable$addRow(rowKey = paste0("k_", kRange[i]), list(
                                k = as.integer(kRange[i]),
                                avg_silhouette = as.numeric(sils[i]),
                                selected = selected_marker
                            ))
                        }
                    }
                } else usedK <- k
                fit <- cluster::pam(d, k=usedK, diss=TRUE)
                clusters <- factor(fit$clustering, labels = paste0("C", seq_len(usedK)))
                # self$state$fit <- fit
                # self$state$dist <- d
                
                # Fit object now stored in central state management

            } else if (method == "hierarchical") {
                # New hierarchical clustering method
                if (is.null(weights)) {
                    d <- cluster::daisy(df, metric="gower")
                } else {
                    d <- cluster::daisy(df, metric="gower", weights=weights)
                }
                
                if (isTRUE(opts$autoSelectK) || is.null(k)) {
                    kRange <- switch(opts$kRange %||% "medium", "small" = 2:6, "medium" = 2:8, "large" = 2:12, 2:8)
                    
                    hc <- cluster::agnes(d, method="ward")
                    sils <- sapply(kRange, function(kk) {
                        clusterIds <- cutree(as.hclust(hc), k=kk)
                        sil <- cluster::silhouette(clusterIds, d)
                        mean(sil[, "sil_width"])
                    })
                    usedK <- kRange[which.max(sils)]
                    text_lines <- c(text_lines, sprintf("Auto k (hierarchical silhouette): %d", usedK))
                    
                    # Populate silhouette statistics table
                    if (!is.null(self$results$silhouetteStats)) {
                        silTable <- self$results$silhouetteStats
                        for (i in seq_along(kRange)) {
                            selected_marker <- if (kRange[i] == usedK) "✓" else ""
                            silTable$addRow(rowKey = paste0("k_", kRange[i]), list(
                                k = as.integer(kRange[i]),
                                avg_silhouette = as.numeric(sils[i]),
                                selected = selected_marker
                            ))
                        }
                    }
                } else usedK <- k
                
                hc <- cluster::agnes(d, method="ward")
                clusterIds <- cutree(as.hclust(hc), k=usedK)
                clusters <- factor(clusterIds, labels = paste0("C", seq_len(usedK)))
                # self$state$fit <- hc
                # self$state$dist <- d

            } else if (method == "dimreduce") {
                # New dimension reduction + k-means method
                if (length(contVars) > 0 && length(catVars) > 0) {
                    # Mixed data: use MCA for categorical + PCA for continuous
                    scores <- NULL
                    
                    if (length(catVars) > 0) {
                        catData <- df[, catVars, drop=FALSE]
                        mca <- FactoMineR::MCA(catData, graph=FALSE)
                        mcaScores <- mca$ind$coord[, 1:min(3, ncol(mca$ind$coord)), drop=FALSE]
                        scores <- mcaScores
                        # Store mca locally - self$state$mca <- mca
                    }
                    
                    if (length(contVars) > 0) {
                        contData <- df[, contVars, drop=FALSE]
                        contData <- contData[complete.cases(contData), , drop=FALSE]
                        if (nrow(contData) > 0) {
                            pca <- prcomp(contData, scale.=TRUE)
                            pcaScores <- pca$x[, 1:min(3, ncol(pca$x)), drop=FALSE]
                            if (is.null(scores)) {
                                scores <- pcaScores
                            } else if (nrow(scores) == nrow(pcaScores)) {
                                scores <- cbind(scores, pcaScores)
                            }
                            # self$state$pca <- pca
                        }
                    }
                    
                } else if (length(catVars) > 0) {
                    # Categorical only: use MCA
                    mca <- FactoMineR::MCA(df, graph=FALSE)
                    eig <- mca$eig
                    cump <- cumsum(eig[, "cumulative percentage of variance"])
                    keep <- which(cump >= 75)[1]
                    scores <- mca$ind$coord[, 1:keep, drop=FALSE]
                    # self$state$mca <- mca
                } else {
                    # Continuous only: use PCA
                    pca <- prcomp(df, scale.=TRUE)
                    scores <- pca$x[, 1:min(ncol(pca$x), 5), drop=FALSE]
                    # self$state$pca <- pca
                }

                if (isTRUE(opts$autoSelectK) || is.null(k)) {
                    kRange <- switch(opts$kRange %||% "medium", "small" = 2:6, "medium" = 2:8, "large" = 2:12, 2:8)
                    sils <- sapply(kRange, function(kk) {
                        cl <- stats::kmeans(scale(scores), centers=kk, nstart=50)
                        sil <- cluster::silhouette(cl$cluster, stats::dist(scale(scores)))
                        mean(sil[, "sil_width"])
                    })
                    usedK <- kRange[which.max(sils)]
                    text_lines <- c(text_lines, sprintf("Auto k (dimension reduction silhouette): %d", usedK))
                } else usedK <- k

                km <- stats::kmeans(scale(scores), centers=usedK, nstart=100)
                clusters <- factor(km$cluster, labels = paste0("C", seq_len(usedK)))
                # self$state$km <- km
                # self$state$scores <- scores

            # Keep existing methods for backward compatibility
            } else if (method == "kmodes") {
                # Only works with categorical data
                catOnlyDf <- df[, catVars, drop=FALSE]
                if (length(catVars) == 0) {
                    stop("k-modes method requires categorical variables")
                }
                
                if (isTRUE(opts$autoSelectK) || is.null(k)) {
                    kRange <- switch(opts$kRange %||% "medium", "small" = 2:6, "medium" = 2:8, "large" = 2:12, 2:8)
                    costs <- sapply(kRange, function(kk) {
                        fit <- klaR::kmodes(catOnlyDf, modes = kk, iter.max = 50, weighted = FALSE)
                        fit$withindiff
                    })
                    usedK <- kRange[which.min(costs)]
                    text_lines <- c(text_lines, sprintf("Auto k (k-modes min cost): %d", usedK))
                } else usedK <- k
                fit <- klaR::kmodes(catOnlyDf, modes = usedK, iter.max = 100, weighted = FALSE)
                clusters <- factor(fit$cluster, labels = paste0("C", seq_len(usedK)))
                # self$state$fit <- fit

            } else if (method == "mca_kmeans") {
                # Legacy MCA method - only works with categorical data
                catOnlyDf <- df[, catVars, drop=FALSE]
                if (length(catVars) == 0) {
                    stop("MCA k-means method requires categorical variables")
                }
                
                mca <- FactoMineR::MCA(catOnlyDf, graph=FALSE)
                eig <- mca$eig
                cump <- cumsum(eig[, "cumulative percentage of variance"])
                keep <- which(cump >= 75)[1]
                scores <- mca$ind$coord[, 1:keep, drop=FALSE]

                if (isTRUE(opts$autoSelectK) || is.null(k)) {
                    kRange <- switch(opts$kRange %||% "medium", "small" = 2:6, "medium" = 2:8, "large" = 2:12, 2:8)
                    sils <- sapply(kRange, function(kk) {
                        cl <- stats::kmeans(scale(scores), centers=kk, nstart=50)
                        sil <- cluster::silhouette(cl$cluster, stats::dist(scale(scores)))
                        mean(sil[, "sil_width"])
                    })
                    usedK <- kRange[which.max(sils)]
                    text_lines <- c(text_lines, sprintf("Auto k (MCA silhouette): %d", usedK))
                } else usedK <- k

                km <- stats::kmeans(scale(scores), centers=usedK, nstart=100)
                clusters <- factor(km$cluster, labels = paste0("C", seq_len(usedK)))
                # self$state$mca <- mca
                # self$state$km <- km
                # self$state$scores <- scores
            }

            # Store analysis results using jamovi state management
            analysisState <- list(
                clusters = clusters,
                usedK = usedK,
                df = df,
                method = method,
                catVars = catVars,
                contVars = contVars,
                fit = NULL,  # Will store clustering fit object
                dist = NULL, # Will store distance matrix
                hc = NULL    # Will store hierarchical clustering object
            )

            # Store method-specific objects for plots
            if (method == "pam") {
                # Recreate fit object for medoid info and silhouette plots
                d <- if (is.null(weights)) {
                    cluster::daisy(df, metric="gower")
                } else {
                    cluster::daisy(df, metric="gower", weights=weights)
                }
                fit <- cluster::pam(d, k=usedK, diss=TRUE)
                analysisState$fit <- fit
                analysisState$dist <- d
            } else if (method == "hierarchical") {
                # Store hierarchical clustering object for dendrogram
                d <- if (is.null(weights)) {
                    cluster::daisy(df, metric="gower")
                } else {
                    cluster::daisy(df, metric="gower", weights=weights)
                }
                hc <- cluster::agnes(d, method="ward")
                analysisState$hc <- hc
                analysisState$dist <- d
            }

            self$results$summary$setState(analysisState)

            # --- Populate results tables ---
            
            # Cluster sizes table (backward compatible)
            sizes <- table(clusters)
            if (!is.null(self$results$clusterSizes)) {
                tbl <- self$results$clusterSizes
                for (cl in names(sizes))
                    tbl$addRow(rowKey=cl, list(cluster=cl, n=as.integer(sizes[[cl]]), 
                                              percent=as.numeric(sizes[[cl]])/length(clusters)))
            } else if (!is.null(self$results$sizes)) {
                tbl <- self$results$sizes
                for (cl in names(sizes))
                    tbl$addRow(rowKey=cl, list(cluster=cl, n=as.integer(sizes[[cl]])))
            }
            
            # Medoid information for PAM clustering using centralized state
            if (method == "pam" && !is.null(analysisState$fit)) {
                fit <- analysisState$fit
                medoidTable <- self$results$medoidInfo
                if (!is.null(medoidTable)) {
                    for (i in seq_along(fit$medoids)) {
                        medoidId <- if (!is.null(rownames(df))) rownames(df)[fit$medoids[i]] else as.character(fit$medoids[i])
                        # Create profile description
                        medoidRow <- df[fit$medoids[i], , drop = FALSE]
                        profileDesc <- paste(names(medoidRow), "=", sapply(medoidRow, as.character), collapse = ", ")

                        medoidTable$addRow(list(
                            cluster = paste0("C", i),
                            medoid_id = medoidId,
                            description = profileDesc
                        ))
                    }
                }
            }

            # Silhouette statistics are now populated directly during k selection above

            # Marker summary by cluster (new comprehensive format)
            if (!is.null(self$results$markerSummary)) {
                mt <- self$results$markerSummary
                for (cl in levels(clusters)) {
                    for (mk in colnames(df)) {
                        clusterData <- df[clusters == cl, mk]
                        clusterData <- clusterData[!is.na(clusterData)]
                        
                        if (length(clusterData) == 0) next
                        
                        if (is.numeric(clusterData)) {
                            # Continuous marker
                            meanVal <- round(mean(clusterData), 2)
                            medianVal <- round(median(clusterData), 2)
                            sdVal <- round(sd(clusterData), 2)
                            iqrVal <- round(IQR(clusterData), 2)
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
                            # Categorical marker
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

            # Legacy mode per stain per cluster (backward compatibility)
            # NOTE: Skipping modes table as it requires dynamic columns which jamovi doesn't support
            # The same information is available in markerSummary table with better formatting
            # if (!is.null(self$results$modes)) {
            #     # Cannot dynamically add columns in jamovi tables
            #     # This legacy table is marked as visible: false anyway
            # }

            # Legacy distributions by cluster (backward compatibility, categorical only)
            if (!is.null(self$results$distr)) {
                distr <- do.call(rbind, lapply(seq_along(df), function(j) {
                    mk <- colnames(df)[j]
                    if (is.numeric(df[[j]])) return(NULL)  # Skip continuous for legacy table
                    d <- as.data.frame(table(clusters, df[[j]], useNA="no"))
                    colnames(d) <- c("cluster","level","n")
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
                        dtab$addRow(rowKey = paste(row$marker, row$cluster, row$level, sep="|"),
                            list(marker=as.character(row$marker),
                                 cluster=as.character(row$cluster),
                                 level=as.character(row$level),
                                 n=as.integer(row$n),
                                 pct=as.numeric(row$pct)))
                    }
                }
            }

            # Association tests (enhanced for mixed data)
            if (isTRUE(self$options$associationTests)) {
                # New comprehensive association tests
                if (!is.null(self$results$associationTests)) {
                    at <- self$results$associationTests
                    for (mk in colnames(df)) {
                        if (is.numeric(df[[mk]])) {
                            # Continuous marker - use Kruskal-Wallis
                            test_result <- tryCatch({
                                kruskal.test(df[[mk]], clusters)
                            }, error = function(e) NULL)
                            
                            if (!is.null(test_result)) {
                                at$addRow(rowKey = mk, list(
                                    marker = mk,
                                    test = "Kruskal-Wallis",
                                    statistic = test_result$statistic,
                                    p_value = test_result$p.value,
                                    effect_size = "Eta-squared"
                                ))
                            }
                        } else {
                            # Categorical marker - use Chi-square
                            test_result <- tryCatch({
                                chisq.test(table(clusters, df[[mk]]))
                            }, error = function(e) NULL)
                            
                            if (!is.null(test_result)) {
                                at$addRow(rowKey = mk, list(
                                    marker = mk,
                                    test = "Chi-square",
                                    statistic = test_result$statistic,
                                    p_value = test_result$p.value,
                                    effect_size = "Cramér's V"
                                ))
                            }
                        }
                    }
                }
                
                # Legacy association tests (backward compatibility)
                if (!is.null(self$results$assoc)) {
                    at <- self$results$assoc
                    for (mk in colnames(df)) {
                        if (is.numeric(df[[mk]])) {
                            p <- tryCatch(kruskal.test(df[[mk]], clusters)$p.value,
                                          error=function(e) NA_real_)
                        } else {
                            p <- tryCatch(stats::chisq.test(table(clusters, df[[mk]]))$p.value,
                                          error=function(e) NA_real_)
                        }
                        at$addRow(rowKey = mk, list(marker=mk, p=p))
                    }
                }
            }
            
            # Cluster profiles (characteristic features)
            if (isTRUE(self$options$clusterProfiles) && !is.null(self$results$clusterProfiles)) {
                profileTable <- self$results$clusterProfiles
                
                for (cl in levels(clusters)) {
                    cluster_indices <- which(clusters == cl)
                    
                    for (marker in colnames(df)) {
                        marker_data <- df[cluster_indices, marker]
                        
                        if (is.numeric(marker_data)) {
                            # Continuous marker
                            mean_val <- mean(marker_data, na.rm = TRUE)
                            sd_val <- sd(marker_data, na.rm = TRUE)
                            summary_text <- sprintf("Mean: %.2f (SD: %.2f)", mean_val, sd_val)
                        } else {
                            # Categorical marker
                            mode_val <- names(sort(table(marker_data), decreasing = TRUE))[1]
                            pct <- round(100 * max(table(marker_data)) / length(marker_data), 1)
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

            # Clinical variable comparisons
            if (length(self$options$clinicalVars) > 0 && !is.null(self$results$clinicalComparison)) {
                clinicalTable <- self$results$clinicalComparison
                
                for (var in self$options$clinicalVars) {
                    clinicalData <- self$data[[var]]
                    if (is.null(clinicalData)) {
                        warning(sprintf("Clinical variable '%s' not found in data", var))
                        next
                    }
                    
                    # Check for sufficient non-missing values
                    nonMissing <- sum(!is.na(clinicalData))
                    if (nonMissing < 5) {
                        warning(sprintf("Clinical variable '%s' has insufficient non-missing values (%d)", var, nonMissing))
                        next
                    }
                    
                    if (is.numeric(clinicalData)) {
                        # Continuous clinical variable
                        test_result <- tryCatch({
                            kruskal.test(clinicalData, clusters)
                        }, error = function(e) NULL)
                        
                        if (!is.null(test_result)) {
                            # Summary by cluster
                            summary_text <- paste(
                                sapply(levels(clusters), function(cl) {
                                    cl_data <- clinicalData[clusters == cl]
                                    cl_data <- cl_data[!is.na(cl_data)]
                                    if (length(cl_data) > 0) {
                                        paste0(cl, ": ", round(mean(cl_data), 2), "±", round(sd(cl_data), 2))
                                    } else {
                                        paste0(cl, ": No data")
                                    }
                                }),
                                collapse = "; "
                            )
                            
                            clinicalTable$addRow(rowKey = var, list(
                                variable = var,
                                test = "Kruskal-Wallis",
                                statistic = test_result$statistic,
                                p_value = test_result$p.value,
                                summary = summary_text
                            ))
                        }
                    } else {
                        # Categorical clinical variable
                        test_result <- tryCatch({
                            chisq.test(table(clusters, clinicalData))
                        }, error = function(e) NULL)
                        
                        if (!is.null(test_result)) {
                            summary_text <- "Cross-tabulation performed"
                            
                            clinicalTable$addRow(rowKey = var, list(
                                variable = var,
                                test = "Chi-square",
                                statistic = test_result$statistic,
                                p_value = test_result$p.value,
                                summary = summary_text
                            ))
                        }
                    }
                }
            }

            # Consensus clustering if requested
            if (isTRUE(self$options$consensusClustering)) {
                private$.performConsensus(df, usedK, weights, clusters)
            }

            # Notes (enhanced)
            method_names <- list(
                "pam" = "PAM (k-medoids) with Gower distance",
                "hierarchical" = "Hierarchical clustering (Ward) with Gower distance",
                "dimreduce" = "Dimension reduction (MCA/PCA) + k-means",
                "kmodes" = "k-modes (categorical only)",
                "mca_kmeans" = "MCA + k-means (categorical only)"
            )
            method_name <- method_names[[method]] %||% method
            
            txt <- paste(c(
                sprintf("Method: %s", method_name),
                sprintf("k: %s", ifelse(is.null(usedK), "NA", usedK)),
                sprintf("Categorical markers: %d", length(catVars)),
                sprintf("Continuous markers: %d", length(contVars)),
                sprintf("Total cases: %d", nrow(df)),
                text_lines
            ), collapse="\n")
            
            if (!is.null(self$results$summary)) {
                self$results$summary$setContent(paste0("<pre>", txt, "</pre>"))
            } else if (!is.null(self$results$text)) {
                self$results$text$setContent(txt)
            }

            # Generate and populate executive summary
            if (!is.null(self$results$executiveSummary)) {
                executive_summary <- private$.generateExecutiveSummary()
                self$results$executiveSummary$setContent(executive_summary)
            }
            
            # Control survival plot visibility dynamically
            if (!is.null(self$results$survivalPlot)) {
                if (!is.null(self$options$survivalTime) && !is.null(self$options$survivalEvent)) {
                    # Both survival variables are provided, make plot visible
                    self$results$survivalPlot$setVisible(TRUE)
                } else {
                    # Either variable missing, hide plot
                    self$results$survivalPlot$setVisible(FALSE)
                }
            }
        },
        
        # Initialize instruction panel with context-sensitive guidance
        # Provides different instructions based on current variable selection
        # and analysis configuration. Shows step-by-step workflow guidance.
        .initTodo = function() {
            if (is.null(self$options$catVars) && is.null(self$options$contVars)) {
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
                # Provide context-specific guidance based on selected variables
                nCat <- length(self$options$catVars)
                nCont <- length(self$options$contVars)
                
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
                    '<p><em>Click "Run Analysis" to proceed or adjust settings in the panel.</em></p>',
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
            <li><b>Missing Values:</b> Handled through pairwise deletion or complete case analysis</li>
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
            # Get preset-specific interpretation if available
            preset_interpretation <- private$.getPresetInterpretation()

            html <- '
            <div style="background-color: #fff8e1; padding: 20px; margin: 15px 0; border-radius: 8px; border-left: 5px solid #ffc107;">
            <h3 style="margin-top: 0; color: #e65100;">🏥 Clinical Interpretation Guide</h3>'

            # Add preset-specific guidance if available
            if (nchar(preset_interpretation) > 0) {
                html <- paste0(html, '
                <div style="background-color: #e8f5e8; padding: 15px; margin: 10px 0; border-radius: 5px; border-left: 3px solid #4caf50;">
                <h4 style="color: #2e7d32;">🧬 Tumor-Specific Classification</h4>
                <pre style="white-space: pre-wrap; font-family: monospace; color: #2e7d32;">', preset_interpretation, '</pre>
                </div>')
            }

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
        
        .performConsensus = function(df, k, weights, clusters) {
            # Simple consensus clustering implementation using bootstrap resampling
            nBootstrap <- self$options$nBootstrap %||% 100
            method <- self$options$method %||% "pam"
            
            # Consensus matrix initialization
            n <- nrow(df)
            consensus_matrix <- matrix(0, nrow = n, ncol = n)
            indicators <- matrix(0, nrow = n, ncol = n)  # Track co-occurrence
            
            tryCatch({
                for (i in seq_len(nBootstrap)) {
                    # Bootstrap sample
                    boot_idx <- sample(n, size = n, replace = TRUE)
                    boot_data <- df[boot_idx, , drop = FALSE]
                    
                    # Perform clustering on bootstrap sample
                    if (method == "pam") {
                        if (is.null(weights)) {
                            d <- cluster::daisy(boot_data, metric = "gower")
                        } else {
                            d <- cluster::daisy(boot_data, metric = "gower", weights = weights)
                        }
                        fit <- cluster::pam(d, k = k, diss = TRUE)
                        boot_clusters <- fit$clustering
                    } else {
                        # Simplified - use same method as main analysis
                        if (is.null(weights)) {
                            d <- cluster::daisy(boot_data, metric = "gower")
                        } else {
                            d <- cluster::daisy(boot_data, metric = "gower", weights = weights)
                        }
                        fit <- cluster::pam(d, k = k, diss = TRUE)  # Fallback to PAM
                        boot_clusters <- fit$clustering
                    }
                    
                    # Update consensus matrix for bootstrap indices
                    for (j1 in seq_len(length(boot_idx))) {
                        for (j2 in seq_len(length(boot_idx))) {
                            orig_idx1 <- boot_idx[j1]
                            orig_idx2 <- boot_idx[j2]
                            
                            # Update co-occurrence matrix
                            indicators[orig_idx1, orig_idx2] <- indicators[orig_idx1, orig_idx2] + 1
                            
                            # If same cluster, update consensus
                            if (boot_clusters[j1] == boot_clusters[j2]) {
                                consensus_matrix[orig_idx1, orig_idx2] <- consensus_matrix[orig_idx1, orig_idx2] + 1
                            }
                        }
                    }
                }
                
                # Calculate final consensus proportions
                consensus_matrix <- ifelse(indicators > 0, consensus_matrix / indicators, 0)
                
                # Calculate stability metrics per cluster
                if (!is.null(self$results$consensusStats)) {
                    consensusTable <- self$results$consensusStats
                    
                    for (cl in levels(clusters)) {
                        cluster_indices <- which(clusters == cl)
                        if (length(cluster_indices) > 1) {
                            # Calculate within-cluster consensus
                            within_consensus <- consensus_matrix[cluster_indices, cluster_indices]
                            stability <- mean(within_consensus[upper.tri(within_consensus)])
                            
                            # Count core samples (high consensus)
                            core_samples <- sum(apply(within_consensus, 1, function(x) mean(x) > 0.8))
                            
                            interpretation <- if (stability > 0.8) {
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
                
            }, error = function(e) {
                # If consensus clustering fails, don't populate the table
                warning(paste("Consensus clustering failed:", e$message))
            })
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

            tryCatch({
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
                
            }, error = function(e) {
                error_msg <- private$.createUserFriendlyError(e, "silhouette plot")
                plot(1, type = "n", xlab = "", ylab = "", main = "Silhouette Plot Error")
                text(1, 1, error_msg, cex = 0.8, col = "red")
            })
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
            
            tryCatch({
                # Convert to numeric matrix for heatmap
                mat <- matrix(NA, nrow = nrow(df), ncol = ncol(df))
                colnames(mat) <- colnames(df)
                rownames(mat) <- rownames(df) %||% paste0("Case_", seq_len(nrow(df)))
                
                for (i in seq_along(colnames(df))) {
                    var <- colnames(df)[i]
                    if (is.numeric(df[[var]])) {
                        mat[, i] <- df[[var]]
                    } else {
                        # Convert factor to numeric
                        mat[, i] <- as.numeric(df[[var]])
                    }
                }
                
                # Scale if requested
                heatmapScale <- self$options$heatmapScale %||% "row"
                if (heatmapScale == "row") {
                    mat <- t(scale(t(mat)))
                } else if (heatmapScale == "column") {
                    mat <- scale(mat)
                }
                
                # Order by clusters
                ord <- order(clusters)
                mat_ordered <- mat[ord, ]
                
                # Simple heatmap using base graphics
                heatmap(t(mat_ordered), 
                       Colv = NA, 
                       main = "IHC Expression Heatmap",
                       xlab = "Cases", ylab = "Markers",
                       col = colorRampPalette(c("blue", "white", "red"))(50))
                
            }, error = function(e) {
                error_msg <- private$.createUserFriendlyError(e, "heatmap")
                plot(1, type = "n", xlab = "", ylab = "", main = "Heatmap Error")
                text(1, 1, error_msg, cex = 0.8, col = "red")
            })
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

            tryCatch({
                # Use stored hierarchical clustering object
                plot(as.hclust(hc), main = "Hierarchical Clustering Dendrogram",
                     xlab = "Cases", ylab = "Height", cex = 0.6)
                if (!is.null(usedK)) {
                    rect.hclust(as.hclust(hc), k = usedK, border = "red", lwd = 2)
                }
            }, error = function(e) {
                error_msg <- private$.createUserFriendlyError(e, "dendrogram")
                plot(1, type = "n", xlab = "", ylab = "", main = "Dendrogram Error")
                text(1, 1, error_msg, cex = 0.8, col = "red")
            })
        },

        .plotPCA = function(image, ggtheme, theme, ...) {
            if (!isTRUE(self$options$showPCAPlot)) return()
            
            # Get analysis state from jamovi state management
            analysisState <- self$results$summary$state
            if (is.null(analysisState)) return()
            
            clusters <- analysisState$clusters
            df <- analysisState$df
            if (is.null(clusters) || is.null(df)) return()
            
            tryCatch({
                opts <- self$options
                catVars <- character(0)
                contVars <- character(0)
                
                # Collect variables
                if (!is.null(opts$catVars) && length(opts$catVars) > 0) catVars <- c(catVars, opts$catVars)
                if (!is.null(opts$contVars) && length(opts$contVars) > 0) contVars <- opts$contVars
                
                catVars <- unique(catVars)
                contVars <- unique(contVars)
                
                if (length(catVars) > 0 && length(contVars) == 0) {
                    # Categorical only: MCA
                    catData <- df[, catVars, drop=FALSE]
                    catData[] <- lapply(catData, as.factor)
                    mca <- FactoMineR::MCA(catData, graph=FALSE)

                    # Get accessibility-aware colors
                    n_clusters <- length(unique(clusters))
                    colors <- private$.getColorPalette(n_clusters)

                    p <- factoextra::fviz_mca_ind(mca,
                                                 habillage = clusters,
                                                 addEllipses = TRUE,
                                                 repel = TRUE,
                                                 palette = colors) +
                        ggplot2::labs(title = "MCA Plot with Clusters")
                } else if (length(contVars) > 0 && length(catVars) == 0) {
                    # Continuous only: PCA
                    contData <- df[, contVars, drop=FALSE]
                    pca <- stats::prcomp(contData, scale. = TRUE, center = TRUE)

                    # Get accessibility-aware colors
                    n_clusters <- length(unique(clusters))
                    colors <- private$.getColorPalette(n_clusters)

                    p <- factoextra::fviz_pca_ind(pca,
                                                 habillage = clusters,
                                                 addEllipses = TRUE,
                                                 repel = TRUE,
                                                 palette = colors) +
                        ggplot2::labs(title = "PCA Plot with Clusters")
                } else {
                    # Mixed data: use first two principal coordinates from distance matrix
                    # Use cached distance matrix if available
                    d <- analysisState$dist
                    if (is.null(d)) {
                        # Fallback calculation if not cached
                        d <- cluster::daisy(df, metric = "gower")
                    }
                    cmd <- stats::cmdscale(d, k = 2)
                    plotData <- data.frame(
                        PC1 = cmd[, 1],
                        PC2 = cmd[, 2],
                        Cluster = clusters
                    )

                    # Get accessibility-aware colors
                    n_clusters <- length(unique(clusters))
                    colors <- private$.getColorPalette(n_clusters)

                    p <- ggplot2::ggplot(plotData, ggplot2::aes(x = PC1, y = PC2, color = Cluster)) +
                        ggplot2::geom_point(size = 2) +
                        ggplot2::stat_ellipse() +
                        ggplot2::scale_color_manual(values = colors) +
                        ggplot2::labs(title = "Multidimensional Scaling Plot with Clusters",
                                      x = "Coordinate 1", y = "Coordinate 2")
                }

                # Apply accessibility theme
                p <- private$.applyAccessibilityTheme(p)
                print(p)
                TRUE
            }, error = function(e) {
                error_msg <- private$.createUserFriendlyError(e, "PCA/MCA plot")
                plot(1, type = "n", axes = FALSE, xlab = "", ylab = "",
                     main = "PCA/MCA Plot Error")
                text(1, 1, error_msg, cex = 0.8, col = "red")
            })
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
            
            tryCatch({
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
                
            }, error = function(e) {
                error_msg <- private$.createUserFriendlyError(e, "boxplot")
                plot(1, type = "n", xlab = "", ylab = "", main = "Boxplot Error")
                text(1, 1, error_msg, cex = 0.8, col = "red")
            })
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
            
            tryCatch({
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
                
            }, error = function(e) {
                error_msg <- private$.createUserFriendlyError(e, "survival plot")
                plot(1, type = "n", xlab = "", ylab = "", main = "Survival Plot Error")
                text(1, 1, error_msg, cex = 0.8, col = "red")
            })
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

        # Apply tumor-specific preset configurations
        .applyTumorPreset = function() {
            if (!isTRUE(self$options$applyPreset)) return()

            preset <- self$options$tumorPreset %||% "none"
            if (preset == "none") return()

            # Define preset configurations
            preset_configs <- list(
                "breast_luminal" = list(
                    method = "pam",
                    nClusters = 4,
                    recommendedMarkers = c("ER", "PR", "HER2", "Ki67", "CK5/6", "EGFR"),
                    description = "Breast cancer luminal subtype classification (ER+/PR+/HER2-, ER+/PR+/HER2+, ER-/PR-/HER2+, Triple negative)"
                ),
                "breast_triple_negative" = list(
                    method = "hierarchical",
                    nClusters = 3,
                    recommendedMarkers = c("CK5/6", "EGFR", "AR", "LAR", "BL1", "BL2"),
                    description = "Triple-negative breast cancer molecular subtypes (Basal-like 1, Basal-like 2, LAR)"
                ),
                "lung_nsclc" = list(
                    method = "pam",
                    nClusters = 3,
                    recommendedMarkers = c("TTF1", "Napsin A", "p40", "CK5/6", "ALK", "ROS1", "PD-L1"),
                    description = "NSCLC histologic and molecular classification (Adenocarcinoma, Squamous, Large cell)"
                ),
                "prostate_standard" = list(
                    method = "pam",
                    nClusters = 3,
                    recommendedMarkers = c("PSA", "PSAP", "AR", "AMACR", "p63", "CK903"),
                    description = "Prostate cancer Gleason grade groups and molecular subtypes"
                ),
                "colon_standard" = list(
                    method = "hierarchical",
                    nClusters = 4,
                    recommendedMarkers = c("MLH1", "MSH2", "MSH6", "PMS2", "BRAF", "KRAS"),
                    description = "Colorectal cancer molecular classification (MSI-H, MSS, CIN, POLE)"
                ),
                "melanoma_immune" = list(
                    method = "pam",
                    nClusters = 4,
                    recommendedMarkers = c("PD-1", "PD-L1", "CD8", "CD4", "FOXP3", "LAG3", "TIM3"),
                    description = "Melanoma immune microenvironment classification"
                ),
                "lymphoma_panel" = list(
                    method = "pam",
                    nClusters = 6,
                    recommendedMarkers = c("CD20", "CD3", "CD10", "BCL6", "MUM1", "CD30", "ALK"),
                    description = "Lymphoma subtype classification (DLBCL, FL, HL, ALCL, etc.)"
                )
            )

            config <- preset_configs[[preset]]
            if (is.null(config)) return()

            # Update options based on preset (note: this is for guidance only)
            # In jamovi, options are typically set by user interaction
            # This function serves to provide recommendations

            return(config)
        },

        # Translation framework for Turkish/English support
        .translate = function(key, lang = NULL) {
            if (is.null(lang)) {
                lang <- self$options$language %||% "english"
            }

            translations <- list(
                "english" = list(
                    "analysis_method" = "Analysis Method",
                    "total_cases" = "Total Cases Analyzed",
                    "clusters_identified" = "Number of Clusters Identified",
                    "categorical_markers" = "Categorical Markers",
                    "continuous_markers" = "Continuous Markers",
                    "cluster_distribution" = "CLUSTER DISTRIBUTION",
                    "clinical_interpretation" = "CLINICAL INTERPRETATION",
                    "recommendations" = "RECOMMENDATIONS",
                    "executive_summary" = "IMMUNOHISTOCHEMISTRY CLUSTERING ANALYSIS - EXECUTIVE SUMMARY",
                    "generated_by" = "Generated by ClinicoPath IHC Clustering Analysis on",
                    "cluster" = "Cluster",
                    "cases" = "cases",
                    "two_groups_interpretation" = "The analysis identified two distinct immunophenotypic groups, suggesting potential biological heterogeneity that may have diagnostic or prognostic implications. Consider correlating these clusters with clinical outcomes.",
                    "three_groups_interpretation" = "Three immunophenotypic subgroups were identified, indicating moderate tumor heterogeneity. This stratification may be useful for risk assessment and treatment planning.",
                    "multiple_groups_interpretation" = "immunophenotypic subgroups were identified, suggesting high tumor heterogeneity. This complex pattern may require additional validation and correlation with molecular markers.",
                    "recommendation_1" = "1. Correlate clustering results with clinical parameters and outcomes",
                    "recommendation_2" = "2. Consider validation in an independent cohort",
                    "recommendation_3" = "3. Evaluate cluster-specific treatment responses if applicable",
                    "recommendation_4" = "4. Review individual cluster profiles for biomarker patterns"
                ),
                "turkish" = list(
                    "analysis_method" = "Analiz Yöntemi",
                    "total_cases" = "Analiz Edilen Toplam Vaka",
                    "clusters_identified" = "Tanımlanan Küme Sayısı",
                    "categorical_markers" = "Kategorik Belirteçler",
                    "continuous_markers" = "Sürekli Belirteçler",
                    "cluster_distribution" = "KÜME DAĞILIMI",
                    "clinical_interpretation" = "KLİNİK YORUM",
                    "recommendations" = "ÖNERİLER",
                    "executive_summary" = "İMMÜNOHİSTOKİMYA KÜMELEME ANALİZİ - YÖNETİCİ ÖZETİ",
                    "generated_by" = "ClinicoPath İHK Kümeleme Analizi tarafından oluşturuldu",
                    "cluster" = "Küme",
                    "cases" = "vaka",
                    "two_groups_interpretation" = "Analiz, potansiyel biyolojik heterojenite öneren iki farklı immünofenotipik grup tanımladı. Bu kümelerin tanısal veya prognostik etkileri olabilir. Bu kümeleri klinik sonuçlarla korelasyon halinde değerlendirmeyi düşünün.",
                    "three_groups_interpretation" = "Üç immünofenotipik alt grup tanımlandı, bu orta düzeyde tümör heterojenitesini göstermektedir. Bu katmanlaşma risk değerlendirmesi ve tedavi planlaması için yararlı olabilir.",
                    "multiple_groups_interpretation" = "immünofenotipik alt grup tanımlandı, bu yüksek tümör heterojenitesini göstermektedir. Bu karmaşık desen ek doğrulama ve moleküler belirteçlerle korelasyon gerektirebilir.",
                    "recommendation_1" = "1. Kümeleme sonuçlarını klinik parametreler ve sonuçlarla korelasyon halinde değerlendirin",
                    "recommendation_2" = "2. Bağımsız bir kohortta doğrulamayı düşünün",
                    "recommendation_3" = "3. Uygunsa küme-özgü tedavi yanıtlarını değerlendirin",
                    "recommendation_4" = "4. Biyobelirteç kalıpları için bireysel küme profillerini gözden geçirin"
                )
            )

            lang_dict <- translations[[lang]]
            if (is.null(lang_dict) || is.null(lang_dict[[key]])) {
                return(translations[["english"]][[key]] %||% key)
            }
            return(lang_dict[[key]])
        },

        # Get preset-specific interpretation guidance
        .getPresetInterpretation = function() {
            preset <- self$options$tumorPreset %||% "none"
            if (preset == "none" || !isTRUE(self$options$applyPreset)) return("")

            interpretations <- list(
                "breast_luminal" = paste(
                    "BREAST CANCER LUMINAL CLASSIFICATION:",
                    "- Cluster 1: Luminal A (ER+/PR+/HER2-, low Ki67)",
                    "- Cluster 2: Luminal B (ER+/PR+/HER2-, high Ki67 or HER2+)",
                    "- Cluster 3: HER2-enriched (ER-/PR-/HER2+)",
                    "- Cluster 4: Triple-negative (ER-/PR-/HER2-)",
                    "Clinical relevance: Guides treatment selection and prognosis.",
                    sep = "\n"
                ),
                "breast_triple_negative" = paste(
                    "TRIPLE-NEGATIVE BREAST CANCER SUBTYPES:",
                    "- BL1: Basal-like 1 (immune-activated, better prognosis)",
                    "- BL2: Basal-like 2 (glycolysis, worse prognosis)",
                    "- LAR: Luminal androgen receptor (AR+, potential AR-targeted therapy)",
                    "Clinical relevance: Predicts response to chemotherapy and immunotherapy.",
                    sep = "\n"
                ),
                "lung_nsclc" = paste(
                    "NON-SMALL CELL LUNG CANCER CLASSIFICATION:",
                    "- Adenocarcinoma: TTF1+/Napsin A+",
                    "- Squamous cell: p40+/CK5/6+",
                    "- Large cell: TTF1-/p40-",
                    "Molecular targets: ALK, ROS1, EGFR, PD-L1 expression",
                    "Clinical relevance: Determines targeted therapy eligibility.",
                    sep = "\n"
                ),
                "prostate_standard" = paste(
                    "PROSTATE CANCER MOLECULAR CLASSIFICATION:",
                    "- Low risk: PSA+/AR+/low AMACR",
                    "- Intermediate risk: Mixed expression pattern",
                    "- High risk: p63+/altered AR signaling",
                    "Clinical relevance: Correlates with Gleason score and prognosis.",
                    sep = "\n"
                ),
                "colon_standard" = paste(
                    "COLORECTAL CANCER MOLECULAR SUBTYPES:",
                    "- MSI-H: MLH1/MSH2/MSH6/PMS2 loss (immunotherapy responsive)",
                    "- MSS: Intact mismatch repair",
                    "- CIN: Chromosomal instability",
                    "- POLE: Ultra-mutated, excellent prognosis",
                    "Clinical relevance: Guides immunotherapy and chemotherapy decisions.",
                    sep = "\n"
                ),
                "melanoma_immune" = paste(
                    "MELANOMA IMMUNE MICROENVIRONMENT:",
                    "- Hot: High CD8+/PD-L1+/immune infiltrate",
                    "- Cold: Low immune infiltrate",
                    "- Excluded: Immune cells at tumor periphery",
                    "- Exhausted: High checkpoint receptors",
                    "Clinical relevance: Predicts immunotherapy response.",
                    sep = "\n"
                ),
                "lymphoma_panel" = paste(
                    "LYMPHOMA CLASSIFICATION:",
                    "- DLBCL: CD20+/variable CD10/BCL6/MUM1",
                    "- FL: CD20+/CD10+/BCL6+",
                    "- HL: CD30+/variable CD20",
                    "- ALCL: CD30+/ALK+/-",
                    "Clinical relevance: Determines treatment protocol and prognosis.",
                    sep = "\n"
                )
            )

            return(interpretations[[preset]] %||% "")
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
            tryCatch({
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

                # Header (translated)
                summary_lines <- c(summary_lines, private$.translate("executive_summary"))
                summary_lines <- c(summary_lines, paste0("=", strrep("=", 60)))
                summary_lines <- c(summary_lines, "")

                # Basic analysis information (translated)
                summary_lines <- c(summary_lines, paste0(private$.translate("analysis_method"), ": ", method_desc))
                summary_lines <- c(summary_lines, paste0(private$.translate("total_cases"), ": ", n_cases))
                summary_lines <- c(summary_lines, paste0(private$.translate("clusters_identified"), ": ", n_clusters))
                summary_lines <- c(summary_lines, "")

                # Variables analyzed (translated)
                if (length(catVars) > 0) {
                    summary_lines <- c(summary_lines, paste0(private$.translate("categorical_markers"), ": ", paste(catVars, collapse = ", ")))
                }
                if (length(contVars) > 0) {
                    summary_lines <- c(summary_lines, paste0(private$.translate("continuous_markers"), ": ", paste(contVars, collapse = ", ")))
                }
                summary_lines <- c(summary_lines, "")

                # Cluster distribution (translated)
                summary_lines <- c(summary_lines, private$.translate("cluster_distribution"))
                summary_lines <- c(summary_lines, paste0("-", strrep("-", 20)))
                for (i in 1:n_clusters) {
                    cluster_name <- paste(private$.translate("cluster"), i)
                    cluster_size <- cluster_sizes[i]
                    cluster_pct <- round(100 * cluster_size / n_cases, 1)
                    summary_lines <- c(summary_lines,
                        paste0(cluster_name, ": ", cluster_size, " ", private$.translate("cases"), " (", cluster_pct, "%)"))
                }
                summary_lines <- c(summary_lines, "")

                # Clinical interpretation (translated)
                summary_lines <- c(summary_lines, private$.translate("clinical_interpretation"))
                summary_lines <- c(summary_lines, paste0("-", strrep("-", 25)))

                if (n_clusters == 2) {
                    summary_lines <- c(summary_lines, private$.translate("two_groups_interpretation"))
                } else if (n_clusters == 3) {
                    summary_lines <- c(summary_lines, private$.translate("three_groups_interpretation"))
                } else if (n_clusters >= 4) {
                    summary_lines <- c(summary_lines,
                        paste0(n_clusters, " ", private$.translate("multiple_groups_interpretation")))
                }

                summary_lines <- c(summary_lines, "")

                # Recommendations (translated)
                summary_lines <- c(summary_lines, private$.translate("recommendations"))
                summary_lines <- c(summary_lines, paste0("-", strrep("-", 15)))
                summary_lines <- c(summary_lines,
                    private$.translate("recommendation_1"),
                    private$.translate("recommendation_2"),
                    private$.translate("recommendation_3"),
                    private$.translate("recommendation_4"))

                summary_lines <- c(summary_lines, "")
                summary_lines <- c(summary_lines,
                    paste0(private$.translate("generated_by"), " ", Sys.Date()))

                # Join all lines
                executive_summary <- paste(summary_lines, collapse = "\n")

                return(executive_summary)

            }, error = function(e) {
                return(paste("Executive summary generation failed:", e$message))
            })
        }
    )
)

ihccluster <- function(jmvcore) ihcclusterClass
