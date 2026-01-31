
groomecompareClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "groomecompareClass",
    inherit = groomecompareBase,
    private = list(
        # Initialize notice list
        .noticeList = list(),

        # Add a notice
        .addNotice = function(type, title, content) {
            notice <- list(type = type, title = title, content = content)
            private$.noticeList[[length(private$.noticeList) + 1]] <- notice
        },

        # Render all notices as HTML
        .renderNotices = function() {
            if (length(private$.noticeList) == 0) {
                self$results$notices$setContent("")
                return()
            }

            html <- '<div style="font-family: Arial, sans-serif; margin: 10px;">'

            for (notice in private$.noticeList) {
                color <- switch(notice$type,
                    "ERROR" = "#d9534f",
                    "WARNING" = "#f0ad4e",
                    "INFO" = "#5bc0de",
                    "#777"
                )

                icon <- switch(notice$type,
                    "ERROR" = "&#x2716;",
                    "WARNING" = "&#x26A0;",
                    "INFO" = "&#x2139;",
                    "&#x2022;"
                )

                html <- paste0(html,
                    '<div style="margin: 8px 0; padding: 10px; background-color: ', color, '22; border-left: 4px solid ', color, '; border-radius: 3px;">',
                    '<strong style="color: ', color, ';">', icon, ' ', notice$title, '</strong><br/>',
                    '<span style="color: #333;">', notice$content, '</span>',
                    '</div>'
                )
            }

            html <- paste0(html, '</div>')
            self$results$notices$setContent(html)
        },

        # Initialize
        .init = function() {
            # Set instructions
            instructions <- '<div style="font-family: Arial; padding: 15px; background-color: #f8f9fa; border-radius: 5px; margin: 10px 0;">
                <h3 style="color: #0066cc; margin-top: 0;">Groome Staging System Comparison</h3>
                <p><strong>Purpose:</strong> Compare two staging systems using well-accepted criteria (Groome et al., 2001).</p>
                <p><strong>Criteria Explained:</strong></p>
                <ul>
                    <li><strong>Hazard Consistency:</strong> max|log(HR_i/HR_j)| for adjacent stages. Smaller = more consistent hazard progression.</li>
                    <li><strong>Hazard Discrimination:</strong> Range of log(HR) across stages. Larger = better separation of risk groups.</li>
                    <li><strong>Sample Balance:</strong> max(n_i/n_j) across stages. Smaller = more balanced stage distribution.</li>
                    <li><strong>Outcome Prediction:</strong> Weighted combination of above metrics.</li>
                    <li><strong>Overall Rank:</strong> Sum of 4 scores. <span style="color: red; font-weight: bold;">Smaller is better.</span></li>
                </ul>
                <p><strong>Interpretation:</strong> The system with lower overall rank has superior prognostic performance.</p>
                <p><strong>Reference:</strong> Groome PA, et al. Head Neck. 2001;23:613-24.</p>
                <p><strong>Application:</strong> Compare ypTNM vs. RPA staging (Liu et al., Br J Cancer 2026).</p>
            </div>'

            self$results$instructions$setContent(instructions)
        },

        # Main run function
        .run = function() {
            # Check inputs
            if (is.null(self$options$time) || is.null(self$options$event) ||
                is.null(self$options$stage1) || is.null(self$options$stage2)) {
                private$.addNotice("INFO", "Awaiting Input",
                    "Please select Survival Time, Event Status, and both Staging Systems.")
                private$.renderNotices()
                return()
            }

            # Get data
            timeVar <- jmvcore::toNumeric(self$data[[self$options$time]])
            eventVar <- self$data[[self$options$event]]

            # Handle event value
            if (is.factor(eventVar)) {
                eventVar <- as.character(eventVar)
            }

            if (self$options$eventValue == 'TRUE') {
                eventNumeric <- as.numeric(eventVar == 'TRUE' | eventVar == 'true' | eventVar == '1')
            } else {
                eventNumeric <- as.numeric(eventVar == self$options$eventValue)
            }

            stage1 <- self$data[[self$options$stage1]]
            stage2 <- self$data[[self$options$stage2]]

            # Convert to factors if not already
            if (!is.factor(stage1)) stage1 <- as.factor(stage1)
            if (!is.factor(stage2)) stage2 <- as.factor(stage2)

            # Check for adequate stages
            if (nlevels(stage1) < 2 || nlevels(stage2) < 2) {
                private$.addNotice("ERROR", "Insufficient Stages",
                    "Each staging system must have at least 2 stages.")
                private$.renderNotices()
                return()
            }

            # Complete cases
            completeIdx <- complete.cases(timeVar, eventNumeric, stage1, stage2)
            if (sum(!completeIdx) > 0) {
                private$.addNotice("WARNING", "Missing Data",
                    paste0(sum(!completeIdx), " rows excluded due to missing data."))
            }

            timeVar <- timeVar[completeIdx]
            eventNumeric <- eventNumeric[completeIdx]
            stage1 <- droplevels(stage1[completeIdx])
            stage2 <- droplevels(stage2[completeIdx])

            # Check sample size
            nTotal <- length(timeVar)
            nEvents <- sum(eventNumeric)

            if (nTotal < 50) {
                private$.addNotice("WARNING", "Small Sample Size",
                    paste0("Only ", nTotal, " observations. Groome criteria work best with n ≥ 100."))
            }

            if (nEvents < 10) {
                private$.addNotice("ERROR", "Insufficient Events",
                    "Need at least 10 events for meaningful comparison.")
                private$.renderNotices()
                return()
            }

            # Check events per stage
            events_per_stage1 <- tapply(eventNumeric, stage1, sum)
            events_per_stage2 <- tapply(eventNumeric, stage2, sum)

            if (any(events_per_stage1 < 5) || any(events_per_stage2 < 5)) {
                private$.addNotice("WARNING", "Sparse Stage Events",
                    "Some stages have <5 events. Hazard ratios may be unstable.")
            }

            # Calculate Groome metrics for both systems
            metrics1 <- private$.groomeMetrics(timeVar, eventNumeric, stage1, self$options$stage1name)
            metrics2 <- private$.groomeMetrics(timeVar, eventNumeric, stage2, self$options$stage2name)

            # Populate summary table
            summaryData <- data.frame(
                criterion = c("Hazard Consistency", "Hazard Discrimination",
                              "Sample Balance", "Outcome Prediction", "Overall Rank"),
                system1 = c(metrics1$consistency, metrics1$discrimination,
                           metrics1$balance, metrics1$prediction, metrics1$overall),
                system2 = c(metrics2$consistency, metrics2$discrimination,
                           metrics2$balance, metrics2$prediction, metrics2$overall),
                stringsAsFactors = FALSE
            )

            # Determine better system for each criterion
            summaryData$better <- c(
                ifelse(metrics1$consistency < metrics2$consistency, self$options$stage1name, self$options$stage2name),
                ifelse(metrics1$discrimination > metrics2$discrimination, self$options$stage1name, self$options$stage2name),
                ifelse(metrics1$balance < metrics2$balance, self$options$stage1name, self$options$stage2name),
                ifelse(metrics1$prediction < metrics2$prediction, self$options$stage1name, self$options$stage2name),
                ifelse(metrics1$overall < metrics2$overall, self$options$stage1name, self$options$stage2name)
            )

            # Populate summary table using addRow (following jamovi guide pattern)
            summaryTable <- self$results$summary
            for (i in seq_len(nrow(summaryData))) {
                summaryTable$addRow(
                    rowKey = i,
                    values = list(
                        criterion = summaryData$criterion[i],
                        system1 = summaryData$system1[i],
                        system2 = summaryData$system2[i],
                        better = summaryData$better[i]
                    )
                )
            }

            # Populate detailed metrics if requested
            if (self$options$detailedmetrics) {
                # Consistency details
                consistencyDetails <- private$.getConsistencyDetails(metrics1, metrics2)
                if (nrow(consistencyDetails) > 0) {
                    consistencyTable <- self$results$detailedmetrics$consistency
                    for (i in seq_len(nrow(consistencyDetails))) {
                        consistencyTable$addRow(
                            rowKey = i,
                            values = as.list(consistencyDetails[i, ])
                        )
                    }
                }

                # Discrimination details
                discriminationDetails <- private$.getDiscriminationDetails(metrics1, metrics2)
                if (nrow(discriminationDetails) > 0) {
                    discriminationTable <- self$results$detailedmetrics$discrimination
                    for (i in seq_len(nrow(discriminationDetails))) {
                        discriminationTable$addRow(
                            rowKey = i,
                            values = as.list(discriminationDetails[i, ])
                        )
                    }
                }
            }

            # Overall winner
            winner <- ifelse(metrics1$overall < metrics2$overall,
                           self$options$stage1name, self$options$stage2name)

            improvementPct <- abs(metrics1$overall - metrics2$overall) /
                            max(metrics1$overall, metrics2$overall) * 100

            private$.addNotice("INFO", paste0("Winner: ", winner),
                paste0(winner, " demonstrates superior performance (",
                      round(improvementPct, 1), "% better overall rank)."))

            # Detailed hazard ratio tables
            if (self$options$hazardratios) {
                # System 1
                hrs1 <- private$.getHazardRatios(timeVar, eventNumeric, stage1)
                if (nrow(hrs1) > 0) {
                    hrs1Table <- self$results$hazardratios$hrs1
                    for (i in seq_len(nrow(hrs1))) {
                        hrs1Table$addRow(
                            rowKey = i,
                            values = as.list(hrs1[i, ])
                        )
                    }
                }

                # System 2
                hrs2 <- private$.getHazardRatios(timeVar, eventNumeric, stage2)
                if (nrow(hrs2) > 0) {
                    hrs2Table <- self$results$hazardratios$hrs2
                    for (i in seq_len(nrow(hrs2))) {
                        hrs2Table$addRow(
                            rowKey = i,
                            values = as.list(hrs2[i, ])
                        )
                    }
                }
            }

            # Sample size distribution
            if (self$options$samplesize) {
                sampleDist <- rbind(
                    private$.getSampleDistribution(stage1, self$options$stage1name),
                    private$.getSampleDistribution(stage2, self$options$stage2name)
                )
                if (nrow(sampleDist) > 0) {
                    sampleTable <- self$results$samplesize
                    for (i in seq_len(nrow(sampleDist))) {
                        sampleTable$addRow(
                            rowKey = i,
                            values = as.list(sampleDist[i, ])
                        )
                    }
                }
            }

            # C-index comparison
            if (self$options$cindexcompare) {
                cindex1 <- tryCatch({
                    cox1 <- survival::coxph(survival::Surv(timeVar, eventNumeric) ~ stage1)
                    lp1 <- stats::predict(cox1, type = "lp")
                    survival::concordance(survival::Surv(timeVar, eventNumeric) ~ lp1)
                }, error = function(e) NULL)

                cindex2 <- tryCatch({
                    cox2 <- survival::coxph(survival::Surv(timeVar, eventNumeric) ~ stage2)
                    lp2 <- stats::predict(cox2, type = "lp")
                    survival::concordance(survival::Surv(timeVar, eventNumeric) ~ lp2)
                }, error = function(e) NULL)

                if (is.null(cindex1) || is.null(cindex2)) {
                    private$.addNotice("WARNING", "C-Index Skipped",
                        "C-index comparison failed due to model fitting issues.")
                } else {
                    cindexData <- data.frame(
                        system = c(self$options$stage1name, self$options$stage2name),
                        cindex = c(cindex1$concordance, cindex2$concordance),
                        se = c(sqrt(cindex1$var), sqrt(cindex2$var)),
                        ci95 = c(
                            paste0("(", round(cindex1$concordance - 1.96 * sqrt(cindex1$var), 3),
                                  "-", round(cindex1$concordance + 1.96 * sqrt(cindex1$var), 3), ")"),
                            paste0("(", round(cindex2$concordance - 1.96 * sqrt(cindex2$var), 3),
                                  "-", round(cindex2$concordance + 1.96 * sqrt(cindex2$var), 3), ")")
                        ),
                        stringsAsFactors = FALSE
                    )

                    # Populate C-index table using addRow
                    cindexTable <- self$results$cindexcompare
                    for (i in seq_len(nrow(cindexData))) {
                        cindexTable$addRow(
                            rowKey = i,
                            values = list(
                                system = cindexData$system[i],
                                cindex = cindexData$cindex[i],
                                se = cindexData$se[i],
                                ci95 = cindexData$ci95[i]
                            )
                        )
                    }

                    # Compare C-indices
                    cindex_diff <- abs(cindex1$concordance - cindex2$concordance)
                    if (cindex_diff > 0.05) {
                        private$.addNotice("INFO", "C-Index Difference",
                            paste0("C-index differs by ", round(cindex_diff, 3),
                                  " (>0.05 = clinically meaningful)."))
                    }
                }
            }

            # Bootstrap validation if requested
            if (self$options$bootstrap) {
                set.seed(self$options$seed)
                bootstrapResults <- private$.bootstrapValidation(
                    timeVar, eventNumeric, stage1, stage2,
                    self$options$stage1name, self$options$stage2name,
                    self$options$nboot
                )

                # Populate bootstrap table using addRow
                if (nrow(bootstrapResults) > 0) {
                    bootstrapTable <- self$results$bootstrap
                    for (i in seq_len(nrow(bootstrapResults))) {
                        bootstrapTable$addRow(
                            rowKey = i,
                            values = as.list(bootstrapResults[i, ])
                        )
                    }
                }
            }

            # Store for plots
            self$results$radarplot$setState(list(
                metrics1 = metrics1,
                metrics2 = metrics2,
                name1 = self$options$stage1name,
                name2 = self$options$stage2name
            ))

            self$results$barplot$setState(list(
                summary = summaryData,
                name1 = self$options$stage1name,
                name2 = self$options$stage2name
            ))

            # KM plots
            kmData1 <- data.frame(
                time = timeVar,
                event = eventNumeric,
                stage = stage1,
                stringsAsFactors = FALSE
            )
            kmData2 <- data.frame(
                time = timeVar,
                event = eventNumeric,
                stage = stage2,
                stringsAsFactors = FALSE
            )

            kmFit1 <- survival::survfit(survival::Surv(time, event) ~ stage, data = kmData1)
            kmFit2 <- survival::survfit(survival::Surv(time, event) ~ stage, data = kmData2)

            self$results$kmplot1$setState(list(
                fit = kmFit1,
                name = self$options$stage1name,
                stages = levels(stage1),
                data = kmData1
            ))

            self$results$kmplot2$setState(list(
                fit = kmFit2,
                name = self$options$stage2name,
                stages = levels(stage2),
                data = kmData2
            ))

            # Add success summary
            private$.addNotice("INFO", "Comparison Complete",
                paste0("Compared ", self$options$stage1name, " (", nlevels(stage1), " stages) vs. ",
                      self$options$stage2name, " (", nlevels(stage2), " stages). ",
                      "Winner: ", winner, ". Review Groome metrics and plots for interpretation."))

            # Render notices
            private$.renderNotices()
        },

        # Calculate Groome metrics
        .groomeMetrics = function(time, event, stage, systemName) {
            # Fit Cox model
            coxFit <- survival::coxph(survival::Surv(time, event) ~ stage)

            # Extract HRs (reference = first level)
            hrs <- c(1, exp(coef(coxFit)))
            logHRs <- log(hrs)

            # 1. Hazard Consistency: max|log(HR_i / HR_j)| for adjacent stages
            consistency <- max(abs(diff(logHRs)))

            # 2. Hazard Discrimination: range of log(HR)
            discrimination <- max(logHRs) - min(logHRs)

            # 3. Sample Balance: max(n_i / n_j)
            stageCounts <- table(stage)
            balance <- max(stageCounts) / min(stageCounts)

            # 4. Outcome Prediction: weighted combination (Groome formula)
            # Simplified version: equal weights
            prediction <- consistency + (1/discrimination) + balance

            # Overall rank: sum of 4 scores (smaller = better)
            overall <- consistency + (1/discrimination) + balance + prediction

            return(list(
                systemName = systemName,
                consistency = consistency,
                discrimination = discrimination,
                balance = balance,
                prediction = prediction,
                overall = overall,
                hrs = hrs,
                logHRs = logHRs,
                stageCounts = stageCounts
            ))
        },

        # Get hazard ratios table
        .getHazardRatios = function(time, event, stage) {
            coxFit <- survival::coxph(survival::Surv(time, event) ~ stage)
            coxSummary <- summary(coxFit)

            stageTable <- table(stage)
            eventTable <- tapply(event, stage, sum)

            # Reference stage
            refStage <- levels(stage)[1]
            refRow <- data.frame(
                stage = refStage,
                n = as.numeric(stageTable[1]),
                events = as.numeric(eventTable[1]),
                hr = 1.0,
                ci95 = "Reference",
                pvalue = NA,
                stringsAsFactors = FALSE
            )

            # Other stages
            otherRows <- data.frame(
                stage = levels(stage)[-1],
                n = as.numeric(stageTable[-1]),
                events = as.numeric(eventTable[-1]),
                hr = exp(coef(coxFit)),
                ci95 = paste0("(", round(coxSummary$conf.int[, 3], 2), "-",
                             round(coxSummary$conf.int[, 4], 2), ")"),
                pvalue = coxSummary$coefficients[, 5],
                stringsAsFactors = FALSE
            )

            rbind(refRow, otherRows)
        },

        # Get sample distribution
        .getSampleDistribution = function(stage, systemName) {
            stageCounts <- table(stage)
            totalN <- sum(stageCounts)

            data.frame(
                system = rep(systemName, length(stageCounts)),
                stage = names(stageCounts),
                n = as.numeric(stageCounts),
                percent = round(100 * as.numeric(stageCounts) / totalN, 1),
                ratio = max(stageCounts) / min(stageCounts),
                stringsAsFactors = FALSE
            )
        },

        # Get consistency details
        .getConsistencyDetails = function(metrics1, metrics2) {
            rows <- list()

            # System 1
            hrs1 <- metrics1$hrs
            if (length(hrs1) > 1) {
                for (i in 1:(length(hrs1)-1)) {
                    rows[[length(rows)+1]] <- data.frame(
                        system = metrics1$systemName,
                        comparison = paste0("Stage ", i, " vs ", i+1),
                        hr1 = hrs1[i],
                        hr2 = hrs1[i+1],
                        logdiff = abs(log(hrs1[i+1]/hrs1[i])),
                        stringsAsFactors = FALSE
                    )
                }
            }

            # System 2
            hrs2 <- metrics2$hrs
            if (length(hrs2) > 1) {
                for (i in 1:(length(hrs2)-1)) {
                    rows[[length(rows)+1]] <- data.frame(
                        system = metrics2$systemName,
                        comparison = paste0("Stage ", i, " vs ", i+1),
                        hr1 = hrs2[i],
                        hr2 = hrs2[i+1],
                        logdiff = abs(log(hrs2[i+1]/hrs2[i])),
                        stringsAsFactors = FALSE
                    )
                }
            }

            if (length(rows) > 0) {
                do.call(rbind, rows)
            } else {
                data.frame()
            }
        },

        # Get discrimination details
        .getDiscriminationDetails = function(metrics1, metrics2) {
            rows <- list()

            # System 1
            hrs1 <- metrics1$hrs
            logHRs1 <- metrics1$logHRs
            for (i in seq_along(hrs1)) {
                rows[[length(rows)+1]] <- data.frame(
                    system = metrics1$systemName,
                    stage = paste0("Stage ", i),
                    hr = hrs1[i],
                    loghr = logHRs1[i],
                    stringsAsFactors = FALSE
                )
            }

            # System 2
            hrs2 <- metrics2$hrs
            logHRs2 <- metrics2$logHRs
            for (i in seq_along(hrs2)) {
                rows[[length(rows)+1]] <- data.frame(
                    system = metrics2$systemName,
                    stage = paste0("Stage ", i),
                    hr = hrs2[i],
                    loghr = logHRs2[i],
                    stringsAsFactors = FALSE
                )
            }

            if (length(rows) > 0) {
                do.call(rbind, rows)
            } else {
                data.frame()
            }
        },

        # Bootstrap validation
        .bootstrapValidation = function(time, event, stage1, stage2, name1, name2, nboot) {
            n <- length(time)

            # Apparent performance
            apparent1 <- private$.groomeMetrics(time, event, stage1, name1)
            apparent2 <- private$.groomeMetrics(time, event, stage2, name2)

            # Bootstrap loop
            boot_consistency1 <- boot_consistency2 <- numeric(nboot)
            boot_discrimination1 <- boot_discrimination2 <- numeric(nboot)
            boot_balance1 <- boot_balance2 <- numeric(nboot)
            boot_overall1 <- boot_overall2 <- numeric(nboot)

            for (b in 1:nboot) {
                # Sample with replacement
                idx <- sample(1:n, n, replace = TRUE)

                # Calculate metrics on bootstrap sample
                metrics1_boot <- tryCatch({
                    private$.groomeMetrics(time[idx], event[idx], stage1[idx], name1)
                }, error = function(e) NULL)

                metrics2_boot <- tryCatch({
                    private$.groomeMetrics(time[idx], event[idx], stage2[idx], name2)
                }, error = function(e) NULL)

                if (!is.null(metrics1_boot)) {
                    boot_consistency1[b] <- metrics1_boot$consistency
                    boot_discrimination1[b] <- metrics1_boot$discrimination
                    boot_balance1[b] <- metrics1_boot$balance
                    boot_overall1[b] <- metrics1_boot$overall
                }

                if (!is.null(metrics2_boot)) {
                    boot_consistency2[b] <- metrics2_boot$consistency
                    boot_discrimination2[b] <- metrics2_boot$discrimination
                    boot_balance2[b] <- metrics2_boot$balance
                    boot_overall2[b] <- metrics2_boot$overall
                }
            }

            # Optimism = Bootstrap mean - Apparent
            results <- data.frame(
                system = c(rep(name1, 4), rep(name2, 4)),
                metric = rep(c("Consistency", "Discrimination", "Balance", "Overall Rank"), 2),
                apparent = c(
                    apparent1$consistency, apparent1$discrimination,
                    apparent1$balance, apparent1$overall,
                    apparent2$consistency, apparent2$discrimination,
                    apparent2$balance, apparent2$overall
                ),
                bootstrap_mean = c(
                    mean(boot_consistency1, na.rm = TRUE),
                    mean(boot_discrimination1, na.rm = TRUE),
                    mean(boot_balance1, na.rm = TRUE),
                    mean(boot_overall1, na.rm = TRUE),
                    mean(boot_consistency2, na.rm = TRUE),
                    mean(boot_discrimination2, na.rm = TRUE),
                    mean(boot_balance2, na.rm = TRUE),
                    mean(boot_overall2, na.rm = TRUE)
                ),
                stringsAsFactors = FALSE
            )

            results$optimism <- results$bootstrap_mean - results$apparent
            results$corrected <- results$apparent - results$optimism

            return(results)
        },

        # Plot radar chart
        .plotRadar = function(image, ...) {
            if (is.null(image$state)) return(FALSE)

            state <- image$state
            disc1 <- state$metrics1$discrimination
            disc2 <- state$metrics2$discrimination
            inv_disc1 <- if (!is.na(disc1) && is.finite(disc1) && disc1 > 0) 1 / disc1 else 0
            inv_disc2 <- if (!is.na(disc2) && is.finite(disc2) && disc2 > 0) 1 / disc2 else 0

            default_max <- c(3, 3, 10, 10)
            values1 <- c(state$metrics1$consistency, inv_disc1,
                        state$metrics1$balance, state$metrics1$prediction)
            values2 <- c(state$metrics2$consistency, inv_disc2,
                        state$metrics2$balance, state$metrics2$prediction)
            values1[!is.finite(values1)] <- 0
            values2[!is.finite(values2)] <- 0
            values1 <- pmax(values1, 0)
            values2 <- pmax(values2, 0)
            data_max <- pmax(values1, values2, na.rm = TRUE)
            data_max[!is.finite(data_max)] <- 0
            max_vals <- pmax(default_max, data_max * 1.1)
            max_vals[max_vals <= 0] <- 1
            scale_changed <- any(abs(max_vals - default_max) > .Machine$double.eps)

            # Prepare data for radar chart
            # Invert discrimination (so smaller = better for all metrics)
            values1 <- pmin(values1, max_vals)
            values2 <- pmin(values2, max_vals)
            data <- data.frame(
                max = max_vals,  # Max values (data-informed)
                min = c(0, 0, 0, 0),    # Min values
                System1 = values1,
                System2 = values2,
                stringsAsFactors = FALSE
            )
            colnames(data) <- c("Consistency", "1/Discrimination", "Balance", "Prediction")

            # Plot
            plotted <- tryCatch({
                fmsb::radarchart(
                    data,
                    axistype = 1,
                    pcol = c("#0066CC", "#CC6600"),
                    plwd = 3,
                    plty = 1,
                    cglcol = "grey70",
                    cglty = 1,
                    axislabcol = "grey30",
                    cglwd = 0.8,
                    vlcex = 1.0,
                    title = "Groome Criteria Comparison\n(Smaller values = Better performance)"
                )

                graphics::legend("topright",
                                 legend = c(state$name1, state$name2),
                                 col = c("#0066CC", "#CC6600"),
                                 lty = 1, lwd = 3, cex = 0.9)

                if (scale_changed && is.null(private$.radarScaleNotified)) {
                    private$.radarScaleNotified <- TRUE
                    private$.addNotice("INFO", "Radar Plot Scaling",
                        "Radar chart scale adjusted to fit observed values.")
                }
                TRUE
            }, error = function(e) {
                private$.addNotice("WARNING", "Radar Plot Skipped",
                    paste0("Radar chart could not be rendered: ", conditionMessage(e)))
                graphics::plot.new()
                graphics::text(0.5, 0.5, "Radar chart unavailable", cex = 1.0)
                TRUE
            })

            return(plotted)
        },

        # Plot bar chart
        .plotBar = function(image, ...) {
            if (is.null(image$state)) return(FALSE)

            state <- image$state
            df <- state$summary[1:4, ]  # Exclude overall rank

            # Reshape for plotting
            df_long <- tidyr::pivot_longer(df,
                                          cols = c("system1", "system2"),
                                          names_to = "System",
                                          values_to = "Value")

            df_long$System <- factor(df_long$System,
                                    levels = c("system1", "system2"),
                                    labels = c(state$name1, state$name2))

            p <- ggplot(df_long, aes(x = criterion, y = Value, fill = System)) +
                geom_bar(stat = "identity", position = "dodge") +
                scale_fill_manual(values = c("#0066CC", "#CC6600")) +
                theme_minimal() +
                labs(title = "Groome Criteria Comparison",
                     x = "Criterion",
                     y = "Score (lower = better, except Discrimination)") +
                theme(axis.text.x = element_text(angle = 45, hjust = 1))

            print(p)

            return(TRUE)
        },

        # Plot KM1
        .plotKM1 = function(image, ...) {
            private$.plotKM(image, ...)
        },

        # Plot KM2
        .plotKM2 = function(image, ...) {
            private$.plotKM(image, ...)
        },

        # Plot KM helper
        .plotKM = function(image, ...) {
            if (is.null(image$state)) return(FALSE)

            state <- image$state

            p <- survminer::ggsurvplot(
                state$fit,
                data = state$data,
                risk.table = TRUE,
                pval = TRUE,
                conf.int = FALSE,
                palette = "jco",
                ggtheme = ggplot2::theme_minimal(),
                title = paste0("Kaplan-Meier Curves: ", state$name),
                xlab = "Time (months)",
                ylab = "Overall Survival Probability",
                legend.title = "Stage",
                legend.labs = state$stages,
                risk.table.height = 0.25,
                fontsize = 3.5
            )

            print(p)

            return(TRUE)
        }
    )
)
