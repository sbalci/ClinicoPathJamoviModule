#' @title Stage Migration Analysis
#' @importFrom R6 R6Class
#' @import jmvcore
#'
#' @description
#' This function analyzes stage migration between different staging systems and assesses
#' its impact on prognostic value and survival outcomes. It provides tools for quantifying
#' the Will Rogers phenomenon and evaluating staging system performance.
#'
#' @return A results object containing migration tables, survival comparisons,
#' and visualization of migration patterns.

stagemigrationClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "stagemigrationClass",
    inherit = stagemigrationBase,
    private = list(
        .init = function() {
            # Set up dynamic table and plot layouts based on variables
            if (is.null(self$options$oldStage) || is.null(self$options$newStage)) {
                self$results$todo$setVisible(TRUE)
                self$results$migrationSummary$setVisible(FALSE)
                self$results$stageDistribution$setVisible(FALSE)
                self$results$migrationTable$setVisible(FALSE)
                self$results$survivalComparison$setVisible(FALSE)
                self$results$migrationPlot$setVisible(FALSE)
                self$results$survivalPlot$setVisible(FALSE)
                self$results$concordancePlot$setVisible(FALSE)
                self$results$stagingPerformance$setVisible(FALSE)
            } else {
                self$results$todo$setVisible(FALSE)
            }
        },

        .todo = function() {
            # Display initial instructions when no variables are selected
            todo <- glue::glue(
                "
                <br>Welcome to Stage Migration Analysis
                <br><br>
                This tool will help you analyze the impact of changes in staging systems.
                <br><br>
                Please select:
                <br>- The original staging variable
                <br>- The new staging variable
                <br>- Time and event variables for survival analysis
                <br><br>
                The analysis will:
                <br>- Quantify migration between staging systems
                <br>- Compare prognostic performance
                <br>- Visualize stage migration patterns
                <br>- Analyze the Will Rogers phenomenon
                <br><br>
                This function uses survival, survminer, and ggalluvial packages.
                "
            )

            html <- self$results$todo
            html$setContent(todo)
        },

        .getData = function() {
            # Get the data with proper cleaning
            if (is.null(self$data) || nrow(self$data) == 0)
                return(NULL)

            if (is.null(self$options$oldStage) ||
                is.null(self$options$newStage) ||
                is.null(self$options$survivalTime) ||
                is.null(self$options$event))
                return(NULL)

            # Get variables
            data <- self$data

            oldStage <- self$options$oldStage
            newStage <- self$options$newStage
            timeVar <- self$options$survivalTime
            eventVar <- self$options$event
            eventLevel <- self$options$eventLevel

            # Convert event variable to numeric if factor
            if (is.factor(data[[eventVar]])) {
                data[["event_numeric"]] <- ifelse(data[[eventVar]] == eventLevel, 1, 0)
                eventVar <- "event_numeric"
            }

            # Clean data - remove missing values
            cols <- c(oldStage, newStage, timeVar, eventVar)
            data <- data[, cols, drop = FALSE]
            data <- stats::na.omit(data)

            # Check if enough data remains
            if (nrow(data) < 10) {
                stop("Too few complete cases for analysis (at least 10 required)")
            }

            # Ensure stages are factors with the same levels
            allStages <- unique(c(levels(as.factor(data[[oldStage]])),
                                  levels(as.factor(data[[newStage]]))))
            data[[oldStage]] <- factor(data[[oldStage]], levels = allStages)
            data[[newStage]] <- factor(data[[newStage]], levels = allStages)

            # Return the processed data
            return(list(
                data = data,
                oldStage = oldStage,
                newStage = newStage,
                timeVar = timeVar,
                eventVar = eventVar
            ))
        },

        .analyzeMigration = function(dataList) {
            data <- dataList$data
            oldStage <- dataList$oldStage
            newStage <- dataList$newStage

            # Create cross-tabulation of stages
            crossTab <- table(Old = data[[oldStage]], New = data[[newStage]])

            # Calculate percentages
            rowPct <- prop.table(crossTab, margin = 1) * 100  # % within old stage
            colPct <- prop.table(crossTab, margin = 2) * 100  # % within new stage

            # Calculate stage distribution before and after
            oldDist <- table(data[[oldStage]])
            newDist <- table(data[[newStage]])

            # Calculate summary statistics
            unchangedCount <- sum(diag(crossTab))
            totalCount <- sum(crossTab)
            migrationPct <- (1 - unchangedCount/totalCount) * 100

            # Calculate upstaging and downstaging
            stages <- as.numeric(gsub("[^0-9]", "", levels(as.factor(data[[oldStage]]))))
            if (length(stages) > 0 && !any(is.na(stages))) {
                stageMatrix <- matrix(0, nrow = nrow(crossTab), ncol = ncol(crossTab))
                for (i in 1:nrow(crossTab)) {
                    for (j in 1:ncol(crossTab)) {
                        if (j > i) stageMatrix[i,j] <- crossTab[i,j]  # upstaging
                        if (j < i) stageMatrix[i,j] <- crossTab[i,j]  # downstaging
                    }
                }
                upstaging <- sum(stageMatrix[lower.tri(stageMatrix, diag = FALSE)])
                downstaging <- sum(stageMatrix[upper.tri(stageMatrix, diag = FALSE)])

                upstagingPct <- upstaging / totalCount * 100
                downstagingPct <- downstaging / totalCount * 100
            } else {
                upstaging <- NA
                downstaging <- NA
                upstagingPct <- NA
                downstagingPct <- NA
            }

            # Chi-square test for association
            chiTest <- chisq.test(crossTab)

            # Return results
            return(list(
                crossTab = crossTab,
                rowPct = rowPct,
                colPct = colPct,
                oldDist = oldDist,
                newDist = newDist,
                migrationPct = migrationPct,
                upstaging = upstaging,
                downstaging = downstaging,
                upstagingPct = upstagingPct,
                downstagingPct = downstagingPct,
                chiTest = chiTest,
                unchangedCount = unchangedCount,
                totalCount = totalCount
            ))
        },

        .analyzeSurvival = function(dataList) {
            data <- dataList$data
            oldStage <- dataList$oldStage
            newStage <- dataList$newStage
            timeVar <- dataList$timeVar
            eventVar <- dataList$eventVar

            # Fit survival models
            oldFit <- try(
                survival::survfit(
                    survival::Surv(data[[timeVar]], data[[eventVar]]) ~ data[[oldStage]]
                )
            )

            newFit <- try(
                survival::survfit(
                    survival::Surv(data[[timeVar]], data[[eventVar]]) ~ data[[newStage]]
                )
            )

            # Log-rank tests
            oldLR <- try(
                survival::survdiff(
                    survival::Surv(data[[timeVar]], data[[eventVar]]) ~ data[[oldStage]]
                )
            )

            newLR <- try(
                survival::survdiff(
                    survival::Surv(data[[timeVar]], data[[eventVar]]) ~ data[[newStage]]
                )
            )

            # Cox proportional hazards models
            oldCox <- try(
                survival::coxph(
                    survival::Surv(data[[timeVar]], data[[eventVar]]) ~ data[[oldStage]]
                )
            )

            newCox <- try(
                survival::coxph(
                    survival::Surv(data[[timeVar]], data[[eventVar]]) ~ data[[newStage]]
                )
            )

            # Calculate C-index for both staging systems
            if (!inherits(oldCox, "try-error") && !inherits(newCox, "try-error")) {
                oldC <- survival::concordance(oldCox)$concordance
                newC <- survival::concordance(newCox)$concordance
                cImprovement <- newC - oldC
                cPctImprovement <- (cImprovement / oldC) * 100
            } else {
                oldC <- NA
                newC <- NA
                cImprovement <- NA
                cPctImprovement <- NA
            }

            # AIC comparison
            if (!inherits(oldCox, "try-error") && !inherits(newCox, "try-error")) {
                oldAIC <- AIC(oldCox)
                newAIC <- AIC(newCox)
                aicImprovement <- oldAIC - newAIC
            } else {
                oldAIC <- NA
                newAIC <- NA
                aicImprovement <- NA
            }

            # Likelihood ratio test comparison
            if (!inherits(oldLR, "try-error") && !inherits(newLR, "try-error")) {
                oldLRchi <- oldLR$chisq
                oldLRdf <- length(oldLR$n) - 1
                oldLRp <- 1 - pchisq(oldLRchi, oldLRdf)

                newLRchi <- newLR$chisq
                newLRdf <- length(newLR$n) - 1
                newLRp <- 1 - pchisq(newLRchi, newLRdf)

                lrImprovement <- newLRchi - oldLRchi
            } else {
                oldLRchi <- NA; oldLRdf <- NA; oldLRp <- NA
                newLRchi <- NA; newLRdf <- NA; newLRp <- NA
                lrImprovement <- NA
            }

            # Will Rogers analysis for stages that have migration
            willRogers <- list()

            if (!inherits(oldFit, "try-error") && !inherits(newFit, "try-error")) {
                # Get all stage pairs with migration
                crossTab <- table(Old = data[[oldStage]], New = data[[newStage]])
                for (i in 1:nrow(crossTab)) {
                    oldStageLevel <- rownames(crossTab)[i]

                    # Find cases that migrated from this stage
                    oldStageData <- data[data[[oldStage]] == oldStageLevel, ]
                    stayed <- oldStageData[oldStageData[[newStage]] == oldStageLevel, ]
                    migrated <- oldStageData[oldStageData[[newStage]] != oldStageLevel, ]

                    if (nrow(migrated) >= 5 && nrow(stayed) >= 5) {
                        # Calculate survival for stayed and migrated groups
                        stayedFit <- try(
                            survival::survfit(
                                survival::Surv(stayed[[timeVar]], stayed[[eventVar]]) ~ 1
                            )
                        )

                        migratedFit <- try(
                            survival::survfit(
                                survival::Surv(migrated[[timeVar]], migrated[[eventVar]]) ~ 1
                            )
                        )

                        if (!inherits(stayedFit, "try-error") && !inherits(migratedFit, "try-error")) {
                            # Calculate median survival times
                            stayedMedian <- summary(stayedFit)$table["median"]
                            migratedMedian <- summary(migratedFit)$table["median"]

                            # Compare survival between stayed and migrated
                            compFit <- survival::survdiff(
                                survival::Surv(oldStageData[[timeVar]], oldStageData[[eventVar]]) ~
                                    (oldStageData[[newStage]] == oldStageLevel)
                            )

                            compP <- 1 - pchisq(compFit$chisq, df = 1)

                            willRogers[[oldStageLevel]] <- list(
                                oldStage = oldStageLevel,
                                stayedN = nrow(stayed),
                                migratedN = nrow(migrated),
                                stayedMedian = stayedMedian,
                                migratedMedian = migratedMedian,
                                pValue = compP
                            )
                        }
                    }
                }
            }

            return(list(
                oldFit = oldFit,
                newFit = newFit,
                oldLR = oldLR,
                newLR = newLR,
                oldCox = oldCox,
                newCox = newCox,
                oldC = oldC,
                newC = newC,
                cImprovement = cImprovement,
                cPctImprovement = cPctImprovement,
                oldAIC = oldAIC,
                newAIC = newAIC,
                aicImprovement = aicImprovement,
                oldLRchi = oldLRchi,
                oldLRdf = oldLRdf,
                oldLRp = oldLRp,
                newLRchi = newLRchi,
                newLRdf = newLRdf,
                newLRp = newLRp,
                lrImprovement = lrImprovement,
                willRogers = willRogers
            ))
        },

        .populateMigrationTable = function(dataList, migrationResults) {
            # Fill migration summary table
            summaryTable <- self$results$migrationSummary

            summaryTable$addRow(rowKey = "total", values = list(
                statistic = "Total patients",
                value = migrationResults$totalCount
            ))

            summaryTable$addRow(rowKey = "unchanged", values = list(
                statistic = "Unchanged stage",
                value = migrationResults$unchangedCount
            ))

            summaryTable$addRow(rowKey = "migration", values = list(
                statistic = "Stage migration (%)",
                value = sprintf("%.1f%%", migrationResults$migrationPct)
            ))

            if (!is.na(migrationResults$upstagingPct)) {
                summaryTable$addRow(rowKey = "upstaging", values = list(
                    statistic = "Upstaging (%)",
                    value = sprintf("%.1f%%", migrationResults$upstagingPct)
                ))

                summaryTable$addRow(rowKey = "downstaging", values = list(
                    statistic = "Downstaging (%)",
                    value = sprintf("%.1f%%", migrationResults$downstagingPct)
                ))
            }

            summaryTable$addRow(rowKey = "chi", values = list(
                statistic = "Chi-square",
                value = sprintf("%.2f (p < %.4f)",
                                migrationResults$chiTest$statistic,
                                migrationResults$chiTest$p.value)
            ))

            # Fill stage distribution table
            distTable <- self$results$stageDistribution
            oldStage <- dataList$oldStage
            newStage <- dataList$newStage

            stages <- unique(c(names(migrationResults$oldDist), names(migrationResults$newDist)))
            for (stage in stages) {
                oldCount <- migrationResults$oldDist[stage]
                if (is.na(oldCount)) oldCount <- 0

                newCount <- migrationResults$newDist[stage]
                if (is.na(newCount)) newCount <- 0

                oldPct <- oldCount / sum(migrationResults$oldDist) * 100
                newPct <- newCount / sum(migrationResults$newDist) * 100

                distTable$addRow(rowKey = stage, values = list(
                    stage = stage,
                    oldCount = oldCount,
                    oldPct = sprintf("%.1f%%", oldPct),
                    newCount = newCount,
                    newPct = sprintf("%.1f%%", newPct),
                    change = sprintf("%+.1f%%", newPct - oldPct)
                ))
            }

            # Fill migration matrix table
            migTable <- self$results$migrationTable

            # Add header for oldStage
            migTable$addColumn(
                name = "oldStage",
                title = dataList$oldStage,
                type = "text",
                combineBelow = FALSE
            )

            # Add columns for each new stage
            for (newStage in colnames(migrationResults$crossTab)) {
                colName <- paste0("newstage_", gsub("[^a-zA-Z0-9]", "_", newStage))
                migTable$addColumn(
                    name = colName,
                    title = newStage,
                    type = "number",
                    format = "%.0f (%s)"
                )
            }

            # Add rows for each old stage
            for (i in 1:nrow(migrationResults$crossTab)) {
                oldStageVal <- rownames(migrationResults$crossTab)[i]
                rowVals <- list(oldStage = oldStageVal)

                for (j in 1:ncol(migrationResults$crossTab)) {
                    newStageVal <- colnames(migrationResults$crossTab)[j]
                    colName <- paste0("newstage_", gsub("[^a-zA-Z0-9]", "_", newStageVal))

                    count <- migrationResults$crossTab[i, j]
                    pct <- sprintf("%.1f%%", migrationResults$rowPct[i, j])

                    rowVals[[colName]] <- list(count, pct)
                }

                migTable$addRow(rowKey = oldStageVal, values = rowVals)
            }
        },

        .populateSurvivalTable = function(dataList, survResults) {
            table <- self$results$survivalComparison

            # Add C-index row
            table$addRow(rowKey = "cindex", values = list(
                metric = "C-index (Harrell's)",
                oldValue = sprintf("%.3f", survResults$oldC),
                newValue = sprintf("%.3f", survResults$newC),
                change = sprintf("%+.3f (%+.1f%%)",
                                 survResults$cImprovement,
                                 survResults$cPctImprovement)
            ))

            # Add Log-rank test row
            table$addRow(rowKey = "logrank", values = list(
                metric = "Log-rank test",
                oldValue = sprintf("χ²=%.2f, p=%s",
                                   survResults$oldLRchi,
                                   format.pval(survResults$oldLRp, digits=4)),
                newValue = sprintf("χ²=%.2f, p=%s",
                                   survResults$newLRchi,
                                   format.pval(survResults$newLRp, digits=4)),
                change = sprintf("%+.2f", survResults$lrImprovement)
            ))

            # Add AIC row
            table$addRow(rowKey = "aic", values = list(
                metric = "AIC",
                oldValue = sprintf("%.2f", survResults$oldAIC),
                newValue = sprintf("%.2f", survResults$newAIC),
                change = sprintf("%+.2f", survResults$aicImprovement)
            ))

            # Will Rogers phenomenon table
            if (length(survResults$willRogers) > 0) {
                wrTable <- self$results$stagingPerformance

                for (stage in names(survResults$willRogers)) {
                    wr <- survResults$willRogers[[stage]]

                    wrTable$addRow(rowKey = stage, values = list(
                        stage = wr$oldStage,
                        stayedN = wr$stayedN,
                        stayedMedian = sprintf("%.1f", wr$stayedMedian),
                        migratedN = wr$migratedN,
                        migratedMedian = sprintf("%.1f", wr$migratedMedian),
                        pValue = format.pval(wr$pValue, digits=4)
                    ))
                }

                # Add explanation note
                if (nrow(wrTable) > 0) {
                    note <- paste(
                        "This table shows evidence of the Will Rogers phenomenon: cases migrated",
                        "to different stages show different survival characteristics than those",
                        "that remained in the same stage category."
                    )
                    wrTable$setNote("note", note)
                }
            }
        },

        .migrationPlot = function(image, ...) {
            if (is.null(image$state))
                return(FALSE)

            dataList <- image$state$dataList

            data <- dataList$data
            oldStage <- dataList$oldStage
            newStage <- dataList$newStage

            # Create alluvial/sankey data
            plotData <- data.frame(
                old = data[[oldStage]],
                new = data[[newStage]]
            )

            # Handle staging systems with many levels
            if (length(unique(c(plotData$old, plotData$new))) > 10) {
                # Consolidate less frequent levels
                oldTable <- table(plotData$old)
                newTable <- table(plotData$new)

                oldFreq <- names(oldTable)[oldTable >= (sum(oldTable) * 0.03)]
                newFreq <- names(newTable)[newTable >= (sum(newTable) * 0.03)]

                plotData$old <- as.character(plotData$old)
                plotData$new <- as.character(plotData$new)

                plotData$old[!(plotData$old %in% oldFreq)] <- "Other"
                plotData$new[!(plotData$new %in% newFreq)] <- "Other"

                plotData$old <- factor(plotData$old)
                plotData$new <- factor(plotData$new)
            }

            # Generate frequency table for the plot
            freqTable <- as.data.frame(table(plotData$old, plotData$new))
            names(freqTable) <- c("old", "new", "Freq")

            # Create the alluvial plot
            p <- ggplot2::ggplot(
                freqTable,
                ggplot2::aes(
                    y = Freq,
                    axis1 = old,
                    axis2 = new
                )
            ) +
                ggalluvial::geom_alluvium(
                    ggplot2::aes(fill = old),
                    width = 1/3
                ) +
                ggalluvial::geom_stratum(width = 1/3, fill = "lightgrey", color = "black") +
                ggplot2::geom_text(
                    stat = "stratum",
                    ggplot2::aes(label = ggplot2::after_stat(stratum))
                ) +
                ggplot2::scale_x_discrete(
                    limits = c(dataList$oldStage, dataList$newStage),
                    expand = c(0.05, 0.05)
                ) +
                ggplot2::theme_minimal() +
                ggplot2::labs(
                    title = paste("Stage Migration from", dataList$oldStage, "to", dataList$newStage),
                    y = "Number of Patients",
                    fill = dataList$oldStage
                ) +
                ggplot2::theme(
                    legend.position = "bottom",
                    panel.grid.major.x = ggplot2::element_blank()
                )

            print(p)
            return(TRUE)
        },

        .survivalPlot = function(image, ...) {
            if (is.null(image$state))
                return(FALSE)

            dataList <- image$state$dataList
            survResults <- image$state$survResults

            if (inherits(survResults$oldFit, "try-error") || inherits(survResults$newFit, "try-error"))
                return(FALSE)

            # Create survival plot comparison
            oldFit <- survResults$oldFit
            newFit <- survResults$newFit

            # Customize based on option for comparing plots
            if (self$options$survivalPlotType == "separate") {
                # Create separate plots
                p1 <- survminer::ggsurvplot(
                    oldFit,
                    data = dataList$data,
                    risk.table = TRUE,
                    pval = TRUE,
                    conf.int = self$options$showCI,
                    title = paste("Survival by", dataList$oldStage),
                    legend.title = dataList$oldStage,
                    ggtheme = ggplot2::theme_minimal()
                )

                p2 <- survminer::ggsurvplot(
                    newFit,
                    data = dataList$data,
                    risk.table = TRUE,
                    pval = TRUE,
                    conf.int = self$options$showCI,
                    title = paste("Survival by", dataList$newStage),
                    legend.title = dataList$newStage,
                    ggtheme = ggplot2::theme_minimal()
                )

                # Arrange in grid
                gridExtra::grid.arrange(
                    p1$plot, p2$plot,
                    p1$table, p2$table,
                    ncol = 2,
                    heights = c(2, 0.5)
                )

            } else {
                # Create a plot comparing specific stages
                if (length(levels(as.factor(dataList$data[[dataList$oldStage]]))) > 6 ||
                    length(levels(as.factor(dataList$data[[dataList$newStage]]))) > 6) {
                    # Too many groups - simplify by combining stages for visual clarity
                    # Focus on 3-5 representative stages that show the migration effect
                    message <- "Too many stage groups for a clear comparison. Simplifying to key stages."
                    grid::grid.text(message, x = 0.5, y = 0.95, just = "center", gp = grid::gpar(fontsize = 12))

                    # Identify key stages with most migration
                    crossTab <- table(dataList$data[[dataList$oldStage]], dataList$data[[dataList$newStage]])
                    offDiag <- crossTab
                    diag(offDiag) <- 0

                    # Find top stages with migration
                    stageSums <- rowSums(offDiag)
                    keyStages <- names(sort(stageSums, decreasing = TRUE)[1:min(4, length(stageSums))])

                    # Subset data to these stages
                    oldSubset <- dataList$data[dataList$data[[dataList$oldStage]] %in% keyStages, ]
                    newSubset <- dataList$data[dataList$data[[dataList$newStage]] %in% keyStages, ]

                    # Create survival fits for subsets
                    oldSubFit <- survival::survfit(
                        survival::Surv(oldSubset[[dataList$timeVar]], oldSubset[[dataList$eventVar]]) ~
                            oldSubset[[dataList$oldStage]]
                    )

                    newSubFit <- survival::survfit(
                        survival::Surv(newSubset[[dataList$timeVar]], newSubset[[dataList$eventVar]]) ~
                            newSubset[[dataList$newStage]]
                    )

                    # Create side-by-side plots
                    p1 <- survminer::ggsurvplot(
                        oldSubFit,
                        data = oldSubset,
                        title = paste("Key Stages in", dataList$oldStage),
                        legend.title = dataList$oldStage,
                        ggtheme = ggplot2::theme_minimal()
                    )

                    p2 <- survminer::ggsurvplot(
                        newSubFit,
                        data = newSubset,
                        title = paste("Key Stages in", dataList$newStage),
                        legend.title = dataList$newStage,
                        ggtheme = ggplot2::theme_minimal()
                    )

                    gridExtra::grid.arrange(p1$plot, p2$plot, ncol = 2)

                } else {
                    # Reasonable number of groups - show side by side comparison
                    p1 <- survminer::ggsurvplot(
                        oldFit,
                        data = dataList$data,
                        title = paste("Survival by", dataList$oldStage),
                        legend.title = dataList$oldStage,
                        ggtheme = ggplot2::theme_minimal()
                    )

                    p2 <- survminer::ggsurvplot(
                        newFit,
                        data = dataList$data,
                        title = paste("Survival by", dataList$newStage),
                        legend.title = dataList$newStage,
                        ggtheme = ggplot2::theme_minimal()
                    )

                    gridExtra::grid.arrange(p1$plot, p2$plot, ncol = 2)
                }
            }

            return(TRUE)
        },

        .concordancePlot = function(image, ...) {
            if (is.null(image$state))
                return(FALSE)

            survResults <- image$state$survResults

            if (is.na(survResults$oldC) || is.na(survResults$newC))
                return(FALSE)

            # Create data for the plot
            df <- data.frame(
                System = c("Original Staging", "New Staging"),
                Concordance = c(survResults$oldC, survResults$newC)
            )

            p <- ggplot2::ggplot(df, ggplot2::aes(x = System, y = Concordance, fill = System)) +
                ggplot2::geom_bar(stat = "identity", width = 0.6) +
                ggplot2::geom_text(ggplot2::aes(label = sprintf("%.3f", Concordance)),
                                   vjust = -0.5, size = 5) +
                ggplot2::scale_fill_manual(values = c("lightblue", "steelblue")) +
                ggplot2::ylim(0, max(c(survResults$oldC, survResults$newC)) * 1.1) +
                ggplot2::theme_minimal() +
                ggplot2::labs(
                    title = "Comparison of Concordance Index (C-index)",
                    subtitle = sprintf("Improvement: +%.3f (%+.1f%%)",
                                       survResults$cImprovement,
                                       survResults$cPctImprovement),
                    y = "Harrell's C-index",
                    x = ""
                ) +
                ggplot2::theme(
                    legend.position = "none",
                    axis.text.x = ggplot2::element_text(size = 12),
                    axis.title.y = ggplot2::element_text(size = 12),
                    plot.title = ggplot2::element_text(size = 14, face = "bold"),
                    plot.subtitle = ggplot2::element_text(size = 12)
                )

            print(p)
            return(TRUE)
        },

        .createPlots = function(dataList, migrationResults, survResults) {
            # Set plot states
            plotState <- list(
                dataList = dataList,
                migrationResults = migrationResults,
                survResults = survResults
            )

            self$results$migrationPlot$setState(plotState)
            self$results$survivalPlot$setState(plotState)
            self$results$concordancePlot$setState(plotState)
        },

        .run = function() {
            # Check if required variables are selected
            if (is.null(self$options$oldStage) ||
                is.null(self$options$newStage) ||
                is.null(self$options$survivalTime) ||
                is.null(self$options$event)) {

                private$.todo()
                return()
            }

            # Get and process data
            dataList <- private$.getData()
            if (is.null(dataList)) {
                private$.todo()
                return()
            }

            # Analyze stage migration
            migrationResults <- private$.analyzeMigration(dataList)

            # Analyze survival differences
            survResults <- private$.analyzeSurvival(dataList)

            # Populate migration tables
            private$.populateMigrationTable(dataList, migrationResults)

            # Populate survival comparison tables
            private$.populateSurvivalTable(dataList, survResults)

            # Set data for plots
            private$.createPlots(dataList, migrationResults, survResults)
        }
    )
)
