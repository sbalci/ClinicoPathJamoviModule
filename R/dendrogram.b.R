
# This file is a generated template, your changes will not be overwritten

dendrogramClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "dendrogramClass",
    inherit = dendrogramBase,
    private = list(

        # Helper method to escape variable names for safe handling
        .escapeVar = function(varName) {
            gsub("[^A-Za-z0-9]", "_", varName)
        },

        .run = function() {

            # Check required packages for selected plot type
            plotType <- self$options$plotType

            if (plotType == "heatmap") {
                missing_heatmap <- character(0)
                if (!requireNamespace("tidyHeatmap", quietly = TRUE)) missing_heatmap <- c(missing_heatmap, "tidyHeatmap")
                if (!requireNamespace("ComplexHeatmap", quietly = TRUE)) missing_heatmap <- c(missing_heatmap, "ComplexHeatmap")
                if (!requireNamespace("tibble", quietly = TRUE)) missing_heatmap <- c(missing_heatmap, "tibble")

                if (length(missing_heatmap) > 0) {
                    notice <- jmvcore::Notice$new(
                        options = self$options,
                        name = 'missingHeatmapPackages',
                        type = jmvcore::NoticeType$ERROR
                    )

                    install_cmd <- if ("ComplexHeatmap" %in% missing_heatmap) {
                        'install.packages("tidyHeatmap"); BiocManager::install("ComplexHeatmap")'
                    } else {
                        paste0('install.packages(c("', paste(missing_heatmap, collapse = '", "'), '"))')
                    }

                    notice$setContent(jmvcore::format(
                        'Heatmap plot requires missing packages: {pkgs}. • Install with {install_cmd}. • Alternatively, select Linear, Circular, or Base plot type to continue.',
                        pkgs = paste(missing_heatmap, collapse = ", "),
                        install_cmd = install_cmd
                    ))
                    self$results$insert(999, notice)
                    return()
                }
            }

            if (plotType %in% c("linear", "circular")) {
                if (!requireNamespace("ggraph", quietly = TRUE) || !requireNamespace("igraph", quietly = TRUE)) {
                    notice <- jmvcore::Notice$new(
                        options = self$options,
                        name = 'missingGgraphPackages',
                        type = jmvcore::NoticeType$WARNING
                    )
                    notice$setContent(jmvcore::format(
                        'ggraph and igraph packages recommended for {plot_type} plots. • Install with install.packages(c("ggraph", "igraph")). • Falling back to base plot.',
                        plot_type = if (plotType == "linear") "linear" else "circular"
                    ))
                    self$results$insert(999, notice)
                }
            }

            # Get options
            vars <- self$options$vars
            clusterMethod <- self$options$clusterMethod
            distanceMethod <- self$options$distanceMethod
            showLabels <- self$options$showLabels
            colorGroups <- self$options$colorGroups
            group <- self$options$group
            plotType <- self$options$plotType
            edgeType <- self$options$edgeType
            colorScheme <- self$options$colorScheme
            highlightClusters <- self$options$highlightClusters
            nClusters <- self$options$nClusters
            maxLabels <- self$options$maxLabels
            plotWidth <- self$options$plotWidth
            plotHeight <- self$options$plotHeight
            standardize <- self$options$standardize
            
            # Check if we have variables selected
            if (length(vars) == 0) {
                self$results$welcome$setVisible(TRUE)
                self$results$welcome$setContent(
                    "<div class='jmv-welcome' style='margin: 2em; padding: 2em; background: #f8f9fa; border-left: 4px solid #007bff;'>
                    <h3 style='margin-top: 0; color: #007bff;'>Hierarchical Clustering Dendrogram</h3>
                    <p style='margin-bottom: 1em;'>Select <strong>2 or more numeric variables</strong> to begin clustering analysis.</p>
                    <p style='font-size: 0.9em; color: #6c757d; margin-bottom: 0;'>
                    This module performs hierarchical clustering and visualizes the results as a dendrogram tree structure.
                    Configure clustering method, distance metric, and visualization options in the left panel.
                    </p>
                    </div>"
                )
                return()
            }
            self$results$welcome$setVisible(FALSE)
            
            # Get data
            data <- self$data
            
            # Prepare numeric data for clustering
            clusterData <- jmvcore::select(data, vars)
            clusterData <- as.data.frame(lapply(clusterData, jmvcore::toNumeric), stringsAsFactors = FALSE)

            if (!is.null(rownames(data)) && is.null(rownames(clusterData)))
                rownames(clusterData) <- rownames(data)

            # Remove rows with missing values but keep bookkeeping
            completeCases <- stats::complete.cases(clusterData)
            removedRows <- sum(!completeCases)

            if (sum(completeCases) < 2) {
                notice <- jmvcore::Notice$new(
                    options = self$options,
                    name = 'insufficientCases',
                    type = jmvcore::NoticeType$ERROR
                )
                notice$setContent(jmvcore::format(
                    'Insufficient complete observations for clustering (n={n}, minimum 2 required). • Current dataset has {total} observations with {removed} removed due to missing values. • Remove missing values or collect additional complete cases.',
                    n = sum(completeCases),
                    total = nrow(data),
                    removed = removedRows
                ))
                self$results$insert(999, notice)
                return()
            }

            clusterData <- clusterData[completeCases, , drop = FALSE]
            
            # Validate group variable if specified
            groupData <- NULL
            groupLevels <- NULL
            if (colorGroups) {
                if (is.null(group) || group == "") {
                    notice <- jmvcore::Notice$new(
                        options = self$options,
                        name = 'missingGroupVariable',
                        type = jmvcore::NoticeType$ERROR
                    )
                    notice$setContent(jmvcore::format(
                        'Grouping variable required when "Color by groups" is enabled. • Please select a grouping variable in the "Grouping & Colors" panel. • Alternatively, disable "Color by groups" to continue without group coloring.'
                    ))
                    self$results$insert(999, notice)
                    return()
                }

                if (!group %in% names(data)) {
                    notice <- jmvcore::Notice$new(
                        options = self$options,
                        name = 'groupVariableNotFound',
                        type = jmvcore::NoticeType$ERROR
                    )
                    notice$setContent(jmvcore::format(
                        'Grouping variable "{group}" not found in dataset. • Available variables: {vars}. • Please select a valid grouping variable or disable "Color by groups".',
                        group = group,
                        vars = paste(names(data)[1:min(10, length(names(data)))], collapse = ", ")
                    ))
                    self$results$insert(999, notice)
                    return()
                }

                groupData <- data[[group]]
                groupData <- droplevels(as.factor(groupData))
                groupLevels <- levels(groupData)
                groupData <- groupData[completeCases]
                names(groupData) <- rownames(clusterData)
            }
            
            # Fill summary table
            summaryTable <- self$results$summary
            if ("clearRows" %in% names(summaryTable))
                summaryTable$clearRows()
            for (var in vars) {
                varData <- clusterData[[var]]
                summaryTable$addRow(rowKey = var, values = list(
                    variable = var,
                    n = length(varData),
                    mean = mean(varData, na.rm = TRUE),
                    sd = stats::sd(varData, na.rm = TRUE),
                    missing = sum(is.na(data[[var]]))
                ))
            }
            
            prep <- private$.prepareClusterData(clusterData, standardize, distanceMethod)
            if (!prep$ok) {
                notice <- jmvcore::Notice$new(
                    options = self$options,
                    name = 'dataPreparationError',
                    type = jmvcore::NoticeType$ERROR
                )
                notice$setContent(prep$message)
                self$results$insert(999, notice)
                return()
            }

            clusteringMatrix <- prep$matrix
            distMatrix <- stats::dist(clusteringMatrix, method = distanceMethod)

            # CRITICAL: Validate distance/linkage compatibility
            validationResult <- private$.validateDistanceLinkage(distanceMethod, clusterMethod)
            if (!validationResult$valid) {
                notice <- jmvcore::Notice$new(
                    options = self$options,
                    name = 'invalidDistanceLinkage',
                    type = jmvcore::NoticeType$ERROR
                )
                notice$setContent(validationResult$message)
                self$results$insert(999, notice)
                return()
            }
            if (!is.null(validationResult$warning)) {
                # Store warning to display in cluster info
                compatibilityWarning <- validationResult$warning
            } else {
                compatibilityWarning <- NULL
            }

            # Perform hierarchical clustering
            hclustResult <- stats::hclust(distMatrix, method = clusterMethod)

            maxClusters <- nrow(clusterData)
            effectiveClusters <- max(1, min(nClusters, maxClusters))
            clusterMembership <- stats::cutree(hclustResult, k = effectiveClusters)

            # Save cluster membership to dataset if requested
            if (self$results$clustOutput$isNotFilled()) {
                # Create full length vector with NAs
                fullClusterMembership <- rep(NA, nrow(data))
                # Assign clusters to complete cases
                fullClusterMembership[completeCases] <- clusterMembership
                # Set values in output variable
                self$results$clustOutput$setValues(fullClusterMembership)
            }

            clusterSummary <- self$results$clusterSummary
            if ("clearRows" %in% names(clusterSummary))
                clusterSummary$clearRows()

            clusterCounts <- as.data.frame(table(clusterMembership), stringsAsFactors = FALSE)
            names(clusterCounts) <- c("cluster", "n")
            clusterCounts$cluster <- as.integer(as.character(clusterCounts$cluster))
            clusterCounts <- clusterCounts[order(clusterCounts$cluster), , drop = FALSE]
            totalAssigned <- sum(clusterCounts$n)
            clusterCounts$percent <- if (totalAssigned > 0) round(clusterCounts$n / totalAssigned * 100, 1) else NA_real_

            for (i in seq_len(nrow(clusterCounts))) {
                clusterSummary$addRow(
                    rowKey = as.character(clusterCounts$cluster[i]),
                    values = list(
                        cluster = clusterCounts$cluster[i],
                        size = clusterCounts$n[i],
                        percent = clusterCounts$percent[i]
                    )
                )
            }

            # Clinical Profile Notice: Sample Size Adequacy
            n_clustered <- nrow(clusterData)
            if (n_clustered < 30) {
                notice_type <- if (n_clustered < 10) jmvcore::NoticeType$STRONG_WARNING else jmvcore::NoticeType$WARNING

                notice <- jmvcore::Notice$new(
                    options = self$options,
                    name = 'smallSampleClustering',
                    type = notice_type
                )

                severity <- if (n_clustered < 10) "very small" else "small"
                notice$setContent(jmvcore::format(
                    '{severity_cap} sample size (n={n}) for hierarchical clustering. • Clusters may be unstable with fewer than 30 observations. • Dendrogram structure and cluster assignments should be interpreted cautiously. • Consider collecting additional data or using this as exploratory analysis only.',
                    severity_cap = tools::toTitleCase(severity),
                    n = n_clustered
                ))
                self$results$insert(999, notice)
            }

            highlightClusters <- highlightClusters && effectiveClusters > 1

            infoBullets <- list(
                paste0("<strong>Number of observations used:</strong> ", nrow(clusterData)),
                paste0("<strong>Variables used:</strong> ", paste(vars, collapse = ", ")),
                paste0("<strong>Distance method:</strong> ", distanceMethod),
                paste0("<strong>Clustering method:</strong> ", clusterMethod),
                paste0("<strong>Plot type:</strong> ", plotType)
            )

            if (removedRows > 0)
                infoBullets <- c(infoBullets, paste0("<strong>Rows removed due to missing values:</strong> ", removedRows))

            if (prep$standardizeApplied)
                infoBullets <- c(infoBullets, "<strong>Scaling:</strong> Variables standardized to mean 0 and SD 1.")
            else if (standardize && distanceMethod == "binary")
                infoBullets <- c(infoBullets, "<strong>Scaling:</strong> Not applied; binary distances require raw 0/1 data.")

            if (length(prep$zeroVariance) > 0)
                infoBullets <- c(infoBullets, paste0("<strong>Zero-variance variables:</strong> ", paste(prep$zeroVariance, collapse = ", "), " (no contribution to distances)."))

            if (highlightClusters) {
                highlightText <- paste0("<strong>Highlighted clusters:</strong> ", effectiveClusters)
                if (nClusters != effectiveClusters)
                    highlightText <- paste0(highlightText, " (requested ", nClusters, ")")
                infoBullets <- c(infoBullets, highlightText)
            } else {
                summaryText <- paste0("<strong>Cluster summary:</strong> ", effectiveClusters, " cluster", if (effectiveClusters == 1) "" else "s")
                if (nClusters != effectiveClusters)
                    summaryText <- paste0(summaryText, " (requested ", nClusters, ")")
                infoBullets <- c(infoBullets, summaryText)
            }

            if (colorGroups)
                infoBullets <- c(infoBullets, paste0("<strong>Grouping variable:</strong> ", group))

            if (!is.null(compatibilityWarning))
                infoBullets <- c(infoBullets, paste0("<strong style='color: #dc3545;'>⚠️ Warning:</strong> ", compatibilityWarning))

            if (length(prep$messages) > 0)
                infoBullets <- c(infoBullets, prep$messages)

            clusterInfoText <- paste0(
                "<h3>Hierarchical Clustering Results</h3>",
                "<ul><li>",
                paste(infoBullets, collapse = "</li><li>"),
                "</li></ul>",
                "<div style='margin-top: 10px;'>",
                "<p><strong>Clinical interpretation (pathologist-friendly):</strong></p>",
                "<ul style='margin-top: 4px;'>",
                "<li><strong>What the tree shows:</strong> Samples that join at short branch heights are more similar; long branches indicate greater separation.</li>",
                if (highlightClusters && effectiveClusters > 1) {
                    "<li><strong>Colored clusters:</strong> Highlighted colors mark the requested clusters. Look for clusters that merge at low heights for the tightest groups.</li>"
                } else {
                    "<li><strong>Clusters:</strong> You can highlight clusters to see where natural groupings occur; low merge heights = stronger similarity.</li>"
                },
                if (prep$standardizeApplied) {
                    "<li><strong>Scaling:</strong> Variables were z-scored (mean 0, SD 1) so each marker contributes equally.</li>"
                } else {
                    "<li><strong>Scaling:</strong> Distances are on the original scale; high-variance markers can dominate clustering.</li>"
                },
                if (!is.null(groupData) && colorGroups) {
                    "<li><strong>Group colours:</strong> Leaf colours reflect the selected grouping variable; grey leaves mean the group was missing/unmatched.</li>"
                } else {
                    "<li><strong>Group colours:</strong> You can colour leaves by a grouping variable to see whether clinical categories align with clusters.</li>"
                },
                "<li><strong>Height axis:</strong> The y-axis shows the clustering distance; focus on where clusters fuse to judge separation.</li>",
                "</ul></div>"
            )
            self$results$clusterInfo$setContent(clusterInfoText)

            # Success Completion Notice
            notice <- jmvcore::Notice$new(
                options = self$options,
                name = 'analysisComplete',
                type = jmvcore::NoticeType$INFO
            )
            notice$setContent(jmvcore::format(
                'Hierarchical clustering completed successfully. • {n_obs} observations clustered using {n_vars} variables. • {n_clust} cluster{s} identified. • Distance: {dist}, Linkage: {link}. • Review dendrogram and cluster membership below.',
                n_obs = nrow(clusterData),
                n_vars = length(vars),
                n_clust = effectiveClusters,
                s = if (effectiveClusters == 1) "" else "s",
                dist = distanceMethod,
                link = clusterMethod
            ))
            self$results$insert(999, notice)

            # Store clustering result for plotting
            image <- self$results$plot
            image$setSize(plotWidth, plotHeight)
            image$setState(list(
                hclustResult = hclustResult,
                clusteringMatrix = clusteringMatrix,
                data = clusterData,
                originalData = data,
                showLabels = showLabels,
                colorGroups = colorGroups,
                group = group,
                groupData = groupData,
                groupLevels = groupLevels,
                vars = vars,
                plotType = plotType,
                edgeType = edgeType,
                colorScheme = colorScheme,
                highlightClusters = highlightClusters,
                requestedClusters = nClusters,
                effectiveClusters = effectiveClusters,
                clusterMembership = clusterMembership,
                clusterSummary = clusterCounts,
                maxLabels = maxLabels,
                standardizeApplied = prep$standardizeApplied,
                distanceMethod = distanceMethod,
                clusterMethod = clusterMethod
            ))
        },
        
        .plot = function(image, ggtheme, theme, ...) {
            
            state <- image$state
            
            if (is.null(state))
                return(FALSE)
            
            hclustResult <- state$hclustResult
            data <- state$data
            clusteringMatrix <- state$clusteringMatrix
            showLabels <- state$showLabels
            colorGroups <- state$colorGroups
            groupData <- state$groupData
            plotType <- state$plotType
            edgeType <- state$edgeType
            colorScheme <- state$colorScheme
            highlightClusters <- state$highlightClusters
            maxLabels <- state$maxLabels
            clusterMembership <- state$clusterMembership
            effectiveClusters <- state$effectiveClusters
            groupLevels <- state$groupLevels
            distanceMethod <- state$distanceMethod
            clusterMethod <- state$clusterMethod

            # Convert to dendrogram
            dendro <- as.dendrogram(hclustResult)

            if (plotType == "heatmap") {
                # tidyHeatmap-based heatmap with dendrograms
                # CRITICAL FIX: Pass precomputed hclustResult to avoid reclustering
                return(private$.plotTidyHeatmap(
                    dataMatrix = clusteringMatrix,
                    hclustResult = hclustResult,
                    showRowDendro = self$options$showRowDendro,
                    showColDendro = self$options$showColDendro,
                    heatmapScale = self$options$heatmapScale,
                    heatmapPalette = self$options$heatmapPalette,
                    showCellBorders = self$options$showCellBorders,
                    showLabels = showLabels,
                    maxLabels = maxLabels,
                    colorGroups = colorGroups,
                    groupData = groupData,
                    groupLevels = groupLevels,
                    colorScheme = colorScheme,
                    distanceMethod = distanceMethod,
                    clusterMethod = clusterMethod
                ))
            } else if (plotType == "base") {
                # Basic R dendrogram plot
                return(private$.plotBaseDendrogram(
                    hclustResult = hclustResult,
                    dendro = dendro,
                    showLabels = showLabels,
                    colorGroups = colorGroups,
                    groupData = groupData,
                    highlightClusters = highlightClusters,
                    effectiveClusters = effectiveClusters,
                    colorScheme = colorScheme,
                    maxLabels = maxLabels,
                    clusterMembership = clusterMembership
                ))
            } else if (plotType == "linear" || plotType == "circular") {
                # ggraph-based dendrograms
                return(private$.plotGgraphDendrogram(
                    hclustResult = hclustResult,
                    dendro = dendro,
                    showLabels = showLabels,
                    colorGroups = colorGroups,
                    groupData = groupData,
                    groupLevels = groupLevels,
                    plotType = plotType,
                    edgeType = edgeType,
                    colorScheme = colorScheme,
                    highlightClusters = highlightClusters,
                    effectiveClusters = effectiveClusters,
                    maxLabels = maxLabels,
                    ggtheme = ggtheme,
                    clusterMembership = clusterMembership
                ))
            }
            
            return(FALSE)
        },
        
        .plotTidyHeatmap = function(dataMatrix, hclustResult, showRowDendro, showColDendro,
                                   heatmapScale, heatmapPalette, showCellBorders, showLabels, maxLabels,
                                   colorGroups, groupData, groupLevels, colorScheme, distanceMethod, clusterMethod) {

            tidyHeatmapAvailable <- requireNamespace('tidyHeatmap', quietly = TRUE) &&
                requireNamespace('ComplexHeatmap', quietly = TRUE) &&
                requireNamespace('tibble', quietly = TRUE)

            if (!tidyHeatmapAvailable) {
                private$.checkpoint()  # Track progress for jamovi
                return(FALSE)
            }

            # CRITICAL FIX: Use precomputed hclustResult for column clustering
            # The hclustResult clusters samples (rows), which become columns after transpose
            # This ensures heatmap displays the SAME clustering reported in summary tables

            # Ensure matrix with dimnames for consistency
            dataMatrix <- as.matrix(dataMatrix)
            # Transpose: features as rows, samples as columns
            dataMatrix <- t(dataMatrix)

            # Convert hclustResult to dendrogram for column clustering
            columnDendro <- as.dendrogram(hclustResult)

            # Determine whether to show labels based on count
            sampleCount <- ncol(dataMatrix)
            featureCount <- nrow(dataMatrix)
            showColLabels <- showLabels && sampleCount <= maxLabels
            showRowLabels <- TRUE  # Always show feature names (typically small number)

            tidyData <- tibble::as_tibble(dataMatrix, rownames = "feature") %>%
                tidyr::pivot_longer(
                    cols = -feature,
                    names_to = "sample",
                    values_to = "value"
                )

            # Set up color palette
            palette <- private$.getHeatmapPalette(heatmapPalette)

            # Scale data if requested
            scaleOption <- if (heatmapScale == "none") "none"
                          else if (heatmapScale == "row") "row"
                          else "column"

            # CRITICAL FIX: Build heatmap parameters to honor user options
            heatmapParams <- list(
                .row = quote(feature),
                .column = quote(sample),
                .value = quote(value),
                scale = scaleOption,
                palette_value = palette,
                cluster_rows = if (!is.null(rowDendro)) rowDendro else showRowDendro,  # Feature clustering
                cluster_columns = columnDendro,     # Use precomputed sample clustering
                show_row_names = showRowLabels,
                show_column_names = showColLabels,
                column_names_gp = grid::gpar(fontsize = 10),
                row_names_gp = grid::gpar(fontsize = 10)
            )

            # CRITICAL FIX: Add cell borders BEFORE annotation to ensure they persist
            if (showCellBorders) {
                heatmapParams$rect_gp <- grid::gpar(col = "white", lwd = 0.5)
            }

            # Optional row dendrogram using user-selected distance/method
            rowDendro <- NULL
            if (showRowDendro) {
                # Validate compatibility for row clustering
                rowValid <- private$.validateDistanceLinkage(distanceMethod, clusterMethod)
                if (!rowValid$valid) {
                    private$.checkpoint()  # Track progress for jamovi
                } else {
                    rowDendro <- tryCatch({
                        stats::as.dendrogram(stats::hclust(
                            stats::dist(t(dataMatrix), method = distanceMethod),
                            method = clusterMethod
                        ))
                    }, error = function(e) {
                        private$.checkpoint()  # Track progress for jamovi
                        NULL
                    })
                }
            }

            # Create base heatmap with all parameters
            hm <- do.call(tidyHeatmap::heatmap, c(list(tidyData), heatmapParams))

            # Add group annotation if requested
            if (colorGroups && !is.null(groupData) && !is.null(groupLevels)) {
                # Use centralized group coloring logic
                colorInfo <- private$.prepareGroupColors(groupData, groupLevels, colorScheme)
                groupColors <- colorInfo$palette

                # Add annotation for samples (columns)
                # Convert groupData to character, preserving NA
                annotationData <- tibble::tibble(
                    sample = names(groupData),
                    group = as.character(groupData)
                )

                # Mark unmatched as NA_character_ instead of reassigning
                annotationData$group[is.na(annotationData$group)] <- NA_character_

                tidyData <- tidyData %>%
                    dplyr::left_join(annotationData, by = "sample")

                # groupColors already includes "Unmatched" = "grey70" if needed
                # Map NA to "Unmatched" for display
                if (colorInfo$hasUnmatched) {
                    groupColors["NA"] <- "grey70"
                }

                hm <- hm %>%
                    tidyHeatmap::annotation_tile(group, palette = groupColors)
            }

            # Print the heatmap
            ComplexHeatmap::draw(hm)
            return(TRUE)
        },

        .plotBaseDendrogram = function(hclustResult, dendro, showLabels, colorGroups,
                                     groupData, highlightClusters, effectiveClusters,
                                     colorScheme, maxLabels, clusterMembership) {

            labelCount <- length(labels(dendro))
            showLeafLabels <- showLabels && labelCount <= maxLabels
            leaflab <- if (showLeafLabels) "perpendicular" else "none"

            dendextendAvailable <- requireNamespace('dendextend', quietly = TRUE)

            if (dendextendAvailable && (highlightClusters || (colorGroups && !is.null(groupData)))) {
                style <- private$.decorateDendrogram(
                    dendro = dendro,
                    highlightClusters = highlightClusters,
                    effectiveClusters = effectiveClusters,
                    colorScheme = colorScheme,
                    clusterMembership = clusterMembership,
                    colorGroups = colorGroups,
                    groupData = groupData
                )

                plot(style$dendro,
                     main = "Hierarchical Clustering Dendrogram",
                     ylab = "Height",
                     leaflab = leaflab,
                     cex = 0.8)

                if (!is.null(style$legend)) {
                    legend(
                        "topright",
                        title = style$legend$title,
                        legend = style$legend$labels,
                        fill = style$legend$colours,
                        border = NA,
                        bty = "n",
                        cex = 0.8
                    )
                }
            } else {
                plot(hclustResult,
                     main = "Hierarchical Clustering Dendrogram",
                     xlab = "Observations",
                     ylab = "Distance",
                     labels = if (showLeafLabels) NULL else FALSE,
                     cex = 0.8)

                if (highlightClusters && effectiveClusters > 1) {
                    colors <- private$.getColors(effectiveClusters, colorScheme)
                    stats::rect.hclust(hclustResult, k = effectiveClusters, border = colors)
                }

                if (colorGroups && !is.null(groupData) && !dendextendAvailable)
                    private$.checkpoint()  # Track progress for jamovi
            }

            return(TRUE)
        },

        .plotGgraphDendrogram = function(hclustResult, dendro, showLabels, colorGroups,
                                       groupData, groupLevels, plotType, edgeType, colorScheme,
                                       highlightClusters, effectiveClusters, maxLabels, ggtheme,
                                       clusterMembership) {

            packagesAvailable <- requireNamespace('ggraph', quietly = TRUE) &&
                requireNamespace('igraph', quietly = TRUE) &&
                requireNamespace('ggplot2', quietly = TRUE)

            if (!packagesAvailable) {
                private$.checkpoint()  # Track progress for jamovi
                return(private$.plotBaseDendrogram(
                    hclustResult = hclustResult,
                    dendro = dendro,
                    showLabels = showLabels,
                    colorGroups = colorGroups,
                    groupData = groupData,
                    highlightClusters = highlightClusters,
                    effectiveClusters = effectiveClusters,
                    colorScheme = colorScheme,
                    maxLabels = maxLabels,
                    clusterMembership = clusterMembership
                ))
            }

            edges_df <- private$.dendrogramToEdges(dendro)
            vertices_df <- private$.createVertices(
                edges_df = edges_df,
                clusterMembership = clusterMembership,
                groupData = groupData,
                colorGroups = colorGroups
            )

            circular <- (plotType == "circular")
            if (circular)
                vertices_df <- private$.calculateCircularAngles(vertices_df)

            mygraph <- igraph::graph_from_data_frame(edges_df, vertices = vertices_df)

            p <- ggraph::ggraph(mygraph, layout = "dendrogram", circular = circular)

            if (edgeType == "diagonal") {
                p <- p + ggraph::geom_edge_diagonal(colour = "grey70", alpha = 0.7)
            } else if (edgeType == "link") {
                p <- p + ggraph::geom_edge_link(colour = "grey70", alpha = 0.7)
            } else {
                p <- p + ggraph::geom_edge_elbow(colour = "grey70", alpha = 0.7)
            }

            colourScale <- NULL

            if (colorGroups && !is.null(groupData)) {
                # Use centralized group coloring logic
                colorInfo <- private$.prepareGroupColors(groupData, groupLevels, colorScheme)
                palette <- colorInfo$palette
                levelsToUse <- colorInfo$levels

                # CRITICAL FIX: Handle NA group assignments explicitly
                # Replace NA with "Unmatched" for plotting if needed
                if (colorInfo$hasUnmatched) {
                    vertices_df$group[is.na(vertices_df$group)] <- "Unmatched"
                }

                vertices_df$group <- factor(vertices_df$group, levels = levelsToUse)
                p <- p + ggraph::geom_node_point(
                    ggplot2::aes(color = group, filter = leaf),
                    size = 2.2,
                    alpha = 0.85
                )
                colourScale <- ggplot2::scale_color_manual(
                    values = palette,
                    na.value = "grey70",
                    name = "Group"
                )
            } else if (highlightClusters && effectiveClusters > 1) {
                palette <- private$.getColors(effectiveClusters, colorScheme)
                clusterLevels <- sort(unique(clusterMembership))
                vertices_df$cluster <- factor(vertices_df$cluster, levels = clusterLevels)
                p <- p + ggraph::geom_node_point(
                    ggplot2::aes(color = cluster, filter = leaf),
                    size = 2.2,
                    alpha = 0.85
                )
                colourScale <- ggplot2::scale_color_manual(
                    values = palette,
                    name = "Cluster"
                )
            } else {
                p <- p + ggraph::geom_node_point(
                    ggplot2::aes(filter = leaf),
                    size = 1.8,
                    alpha = 0.8,
                    colour = "steelblue"
                )
            }

            leafCount <- length(clusterMembership)
            if (showLabels && leafCount <= maxLabels) {
                if (circular) {
                    p <- p + ggraph::geom_node_text(
                        ggplot2::aes(
                            x = x * 1.1,
                            y = y * 1.1,
                            label = name,
                            angle = angle,
                            hjust = hjust,
                            filter = leaf
                        ),
                        size = 3,
                        alpha = 0.9
                    )
                } else {
                    p <- p + ggraph::geom_node_text(
                        ggplot2::aes(label = name, filter = leaf),
                        angle = 90,
                        hjust = 1,
                        size = 3,
                        alpha = 0.9
                    )
                }
            }

            if (!is.null(colourScale))
                p <- p + colourScale

            title <- if (circular) "Circular Hierarchical Clustering Dendrogram" else "Hierarchical Clustering Dendrogram"
            p <- p +
                ggplot2::labs(title = title) +
                ggplot2::theme_void() +
                ggplot2::theme(
                    plot.title = ggplot2::element_text(hjust = 0.5, size = 14),
                    legend.position = if (!is.null(colourScale)) "bottom" else "none"
                )

            if (!is.null(ggtheme))
                p <- p + ggtheme

            print(p)
            return(TRUE)
        },

        .prepareClusterData = function(clusterData, standardize, distanceMethod) {
            result <- list(
                ok = TRUE,
                matrix = NULL,
                standardizeApplied = FALSE,
                zeroVariance = character(0),
                messages = character(0),
                message = NULL
            )

            if (distanceMethod == "binary") {
                nonBinary <- vapply(clusterData, function(column) {
                    any(!column %in% c(0, 1), na.rm = TRUE)
                }, logical(1))

                if (any(nonBinary)) {
                    badVars <- names(clusterData)[nonBinary]
                    result$ok <- FALSE
                    result$message <- jmvcore::format(
                        'Binary distance requires variables coded as 0/1 only. • Non-binary values detected in: {vars}. • Please recode variables to binary (0/1) or select a different distance method (e.g., Euclidean, Manhattan).',
                        vars = paste(badVars, collapse = ", ")
                    )
                    return(result)
                }

                result$matrix <- as.matrix(clusterData)
                return(result)
            }

            zeroVariance <- names(clusterData)[vapply(clusterData, function(column) {
                sd_val <- stats::sd(column, na.rm = TRUE)
                is.na(sd_val) || sd_val == 0
            }, logical(1))]

            result$zeroVariance <- zeroVariance

            if (standardize) {
                scaled <- lapply(clusterData, private$.scaleColumn)
                scaled <- as.data.frame(scaled, stringsAsFactors = FALSE)
                rownames(scaled) <- rownames(clusterData)
                result$matrix <- as.matrix(scaled)
                result$standardizeApplied <- TRUE
            } else {
                result$matrix <- as.matrix(clusterData)
            }

            if (length(zeroVariance) > 0) {
                result$messages <- c(
                    result$messages,
                    paste0(
                        "<strong>Note:</strong> Zero-variance variables (",
                        paste(zeroVariance, collapse = ", "),
                        ") were present and contribute no separation."
                    )
                )
            }

            return(result)
        },

        .scaleColumn = function(column) {
            sd_val <- stats::sd(column, na.rm = TRUE)
            if (is.na(sd_val) || sd_val == 0) {
                scaled <- rep(0, length(column))
            } else {
                mean_val <- mean(column, na.rm = TRUE)
                scaled <- (column - mean_val) / sd_val
            }
            names(scaled) <- names(column)
            return(scaled)
        },

        .dendrogramToEdges = function(dendro) {
            # Convert dendrogram to edge list for ggraph
            edges <- data.frame(from = character(0), to = character(0), stringsAsFactors = FALSE)
            
            # Recursive function to extract edges
            extractEdges <- function(node, parent_name = NULL) {
                if (is.leaf(node)) {
                    leaf_name <- attr(node, "label")
                    if (!is.null(parent_name)) {
                        edges <<- rbind(edges, data.frame(from = parent_name, to = leaf_name, stringsAsFactors = FALSE))
                    }
                } else {
                    node_name <- paste0("node_", length(edges) + 1)
                    if (!is.null(parent_name)) {
                        edges <<- rbind(edges, data.frame(from = parent_name, to = node_name, stringsAsFactors = FALSE))
                    }
                    
                    for (i in 1:length(node)) {
                        extractEdges(node[[i]], node_name)
                    }
                }
            }
            
            # Start extraction from root
            extractEdges(dendro, "root")
            return(edges)
        },

        .decorateDendrogram = function(dendro, highlightClusters, effectiveClusters,
                                      colorScheme, clusterMembership, colorGroups, groupData) {

            if (!requireNamespace('dendextend', quietly = TRUE))
                return(list(dendro = dendro, legend = NULL))

            styled <- dendro
            legendInfo <- NULL

            # Color by groups takes precedence over cluster highlighting
            if (colorGroups && !is.null(groupData)) {
                # Use centralized group coloring logic
                colorInfo <- private$.prepareGroupColors(groupData, levels(groupData), colorScheme)
                palette <- colorInfo$palette
                groupLevels <- colorInfo$levels

                # Get leaf labels from dendrogram
                leafLabels <- labels(styled)

                # Match leaf labels to group data
                # groupData is a named factor where names are row names
                groupAssignments <- as.character(groupData[leafLabels])

                # CRITICAL FIX: Keep NA for unmatched samples, render with neutral color
                # DO NOT silently reassign to first group level
                if (colorInfo$hasUnmatched) {
                    private$.checkpoint()  # Track progress for jamovi
                }

                # Build color vector: use palette for matched groups, grey70 for NA
                labelColours <- ifelse(
                    is.na(groupAssignments),
                    "grey70",  # Neutral color for unmatched
                    palette[groupAssignments]
                )
                # Handle any remaining NAs (shouldn't happen but be safe)
                labelColours[is.na(labelColours)] <- "grey70"

                dendextend::labels_colors(styled) <- labelColours
                styled <- dendextend::set(styled, "labels_cex", 0.8)

                # Legend uses palette and levels from colorInfo
                legendLabels <- groupLevels
                legendColours <- palette

                legendInfo <- list(
                    title = "Group",
                    labels = legendLabels,
                    colours = legendColours
                )
            } else if (highlightClusters && effectiveClusters > 1 && !is.null(clusterMembership)) {
                palette <- private$.getColors(effectiveClusters, colorScheme)
                styled <- dendextend::color_branches(styled, k = effectiveClusters, col = palette)
                styled <- dendextend::set(styled, "branches_lwd", 1.5)

                leafLabels <- labels(styled)
                membership <- clusterMembership[match(leafLabels, names(clusterMembership))]
                labelColours <- palette[membership]
                if (any(is.na(labelColours)))
                    labelColours[is.na(labelColours)] <- palette[1]

                dendextend::labels_colors(styled) <- labelColours
                styled <- dendextend::set(styled, "labels_cex", 0.8)

                legendInfo <- list(
                    title = "Cluster",
                    labels = paste0("Cluster ", seq_len(effectiveClusters)),
                    colours = palette
                )
            }

            return(list(dendro = styled, legend = legendInfo))
        },
        
        .createVertices = function(edges_df, clusterMembership, groupData, colorGroups) {
            all_nodes <- unique(c(edges_df$from, edges_df$to))
            vertices_df <- data.frame(
                name = all_nodes,
                leaf = !all_nodes %in% edges_df$from,
                stringsAsFactors = FALSE
            )

            if (!is.null(clusterMembership)) {
                vertices_df$cluster <- clusterMembership[match(vertices_df$name, names(clusterMembership))]
            } else {
                vertices_df$cluster <- NA_integer_
            }

            if (colorGroups && !is.null(groupData)) {
                # CRITICAL FIX: Keep NA for unmatched samples instead of silent reassignment
                # groupData is a named factor, preserve names when converting
                groupVec <- as.character(groupData)
                names(groupVec) <- names(groupData)
                # Match vertex names to group data names
                vertices_df$group <- groupVec[match(vertices_df$name, names(groupVec))]

                # Warn about unmatched samples
                numUnmatched <- sum(is.na(vertices_df$group) & vertices_df$leaf)
                if (numUnmatched > 0) {
                    private$.checkpoint()  # Track progress for jamovi
                }
            } else {
                vertices_df$group <- NA_character_
            }

            return(vertices_df)
        },
        
        .calculateCircularAngles = function(vertices_df) {
            # Calculate angles for circular dendrogram labels
            leaves <- which(vertices_df$leaf)
            n_leaves <- length(leaves)
            
            vertices_df$angle <- NA
            vertices_df$hjust <- NA
            
            for (i in seq_along(leaves)) {
                angle <- 90 - 360 * (i - 1) / n_leaves
                vertices_df$angle[leaves[i]] <- ifelse(angle < -90, angle + 180, angle)
                vertices_df$hjust[leaves[i]] <- ifelse(angle < -90, 1, 0)
            }
            
            return(vertices_df)
        },
        
        .getColors = function(n, colorScheme) {
            n <- max(1, n)

            if (colorScheme == "viridis") {
                if (requireNamespace('viridisLite', quietly = TRUE))
                    return(viridisLite::viridis(n))
                if (requireNamespace('viridis', quietly = TRUE))
                    return(viridis::viridis(n))
            }

            if (colorScheme %in% c("RdYlBu", "Set1", "Dark2") && requireNamespace('RColorBrewer', quietly = TRUE)) {
                palInfo <- RColorBrewer::brewer.pal_info[colorScheme, ]
                maxCols <- palInfo$maxcolors
                minCols <- palInfo$mincolors
                baseCols <- RColorBrewer::brewer.pal(max(minCols, min(maxCols, n)), colorScheme)
                if (n <= length(baseCols))
                    return(baseCols[seq_len(n)])
                return(grDevices::colorRampPalette(baseCols)(n))
            }

            if (n == 1)
                return("#1f77b4")

            return(grDevices::rainbow(n))
        },

        .prepareGroupColors = function(groupData, groupLevels, colorScheme) {
            # Standardize group levels
            if (is.null(groupLevels) || length(groupLevels) == 0)
                groupLevels <- sort(unique(as.character(groupData)))

            # Get color palette
            palette <- private$.getColors(max(1, length(groupLevels)), colorScheme)
            palette <- palette[seq_len(max(1, length(groupLevels)))]
            names(palette) <- groupLevels

            # Check for unmatched samples (NA in groupData)
            hasUnmatched <- any(is.na(groupData))

            if (hasUnmatched) {
                palette <- c(palette, "Unmatched" = "grey70")
                groupLevels <- c(groupLevels, "Unmatched")
            }

            return(list(
                palette = palette,
                levels = groupLevels,
                hasUnmatched = hasUnmatched
            ))
        },

        .getHeatmapPalette = function(paletteName) {
            if (paletteName == "viridis") {
                if (requireNamespace('viridisLite', quietly = TRUE))
                    return(viridisLite::viridis(100))
                if (requireNamespace('viridis', quietly = TRUE))
                    return(viridis::viridis(100))
            }

            if (paletteName == "RdYlBu" && requireNamespace('RColorBrewer', quietly = TRUE)) {
                return(grDevices::colorRampPalette(
                    RColorBrewer::brewer.pal(11, "RdYlBu")
                )(100))
            }

            if (paletteName == "spectral" && requireNamespace('RColorBrewer', quietly = TRUE)) {
                return(grDevices::colorRampPalette(
                    RColorBrewer::brewer.pal(11, "Spectral")
                )(100))
            }

            # Default blue-red palette
            return(grDevices::colorRampPalette(c("blue", "white", "red"))(100))
        },

        .validateDistanceLinkage = function(distanceMethod, clusterMethod) {
            # CRITICAL: Ward, centroid, median methods require Euclidean distances
            # Using these methods with non-Euclidean distances produces mathematically invalid results

            result <- list(valid = TRUE, warning = NULL, message = NULL)

            # Methods that REQUIRE Euclidean distance
            euclideanOnly <- c("ward.D", "ward.D2", "centroid", "median")

            if (clusterMethod %in% euclideanOnly && distanceMethod != "euclidean") {
                result$valid <- FALSE

                reason <- if (clusterMethod %in% c("ward.D", "ward.D2")) {
                    "Ward methods minimize within-cluster sum of squares, which is only defined for Euclidean distances. Using other distances produces tree heights that don't reflect Ward's criterion."
                } else {
                    "Centroid/median methods compute cluster centers as geometric centroids, which requires Euclidean geometry. Non-Euclidean distances cause inversions (negative heights)."
                }

                result$message <- jmvcore::format(
                    'Invalid distance/linkage combination: {method} clustering requires Euclidean distance. • Current distance: {dist}. • Why this matters: {reason} • Solutions: Change distance to Euclidean, OR change clustering to Complete/Average/Single linkage.',
                    method = clusterMethod,
                    dist = distanceMethod,
                    reason = reason
                )
            }

            return(result)
        })
)
