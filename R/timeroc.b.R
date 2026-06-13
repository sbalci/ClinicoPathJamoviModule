#' @title Enhanced ROC Analysis for Clinical Research
#'
#' @description
#' Comprehensive evaluation of biomarker predictive performance using both
#' time-dependent and general binary ROC analysis. Supports multiple ROC estimation
#' methods, statistical comparisons, and provides clinical interpretation of results.
#'
#' @details
#' This analysis provides both time-dependent and binary ROC curve analysis with:
#' \itemize{
#'   \item Time-dependent ROC: Cumulative/dynamic ROC with IPCW weighting options
#'   \item Binary ROC: General diagnostic performance evaluation
#'   \item DeLong test for comparing multiple ROC curves (binary mode)
#'   \item Bootstrap and Venkatraman tests for ROC comparison
#'   \item Asymptotic confidence intervals via influence functions
#'   \item Optimal cutoff calculation using Youden index
#'   \item Comprehensive visualization (ROC curves and AUC over time)
#'   \item Clinical interpretation and performance assessment
#'   \item Model comparison capabilities with statistical testing
#' }
#'
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import timeROC
#' @import ggplot2
#' @import glue
#' @importFrom pROC roc auc ci.auc roc.test

timerocClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "timerocClass",
    inherit = timerocBase,
    private = list(
        .data = NULL,
        .fit = NULL,
        .timepoints = NULL,
        .primary_roc = NULL,
        .complete_rows = NULL,

        # ── Notice infrastructure ──────────────────────────────
        .noticeList = list(),

        .addNotice = function(type, title, content) {
            private$.noticeList[[length(private$.noticeList) + 1]] <- list(
                type = type, title = title, content = content
            )
        },

        .renderNotices = function() {
            if (length(private$.noticeList) == 0) return()

            typeStyles <- list(
                ERROR = list(color = "#dc2626", bgcolor = "#fef2f2",
                             border = "#fca5a5", icon = "\u26d4"),
                STRONG_WARNING = list(color = "#ea580c", bgcolor = "#fff7ed",
                                      border = "#fdba74", icon = "\u26a0\ufe0f"),
                WARNING = list(color = "#ca8a04", bgcolor = "#fefce8",
                               border = "#fde047", icon = "\u26a0"),
                INFO = list(color = "#2563eb", bgcolor = "#eff6ff",
                            border = "#93c5fd", icon = "\u2139\ufe0f")
            )

            html <- "<div style='margin: 10px 0;'>"
            for (notice in private$.noticeList) {
                style <- typeStyles[[notice$type]]
                if (is.null(style)) style <- typeStyles$INFO
                html <- paste0(html,
                    "<div style='background-color: ", style$bgcolor, "; ",
                    "border-left: 4px solid ", style$border, "; ",
                    "padding: 12px; margin: 8px 0; border-radius: 4px;'>",
                    "<strong style='color: ", style$color, ";'>",
                    htmltools::htmlEscape(notice$title), "</strong><br>",
                    "<span style='color: #374151;'>", htmltools::htmlEscape(notice$content), "</span>",
                    "</div>"
                )
            }
            html <- paste0(html, "</div>")
            self$results$notices$setContent(html)
        },

        # ── Initialization ─────────────────────────────────────
        .init = function() {
            # Core variables always required
            if (is.null(self$options$marker) || is.null(self$options$outcome)) {

                welcome <- glue::glue("
                    <h3>Enhanced ROC Analysis</h3>
                    <p>This analysis evaluates how well a continuous biomarker predicts outcomes
                    using time-dependent or general binary ROC analysis.</p>

                    <h4>Required Variables:</h4>
                    <ul>
                        <li><b>Outcome Variable:</b> Event status (factor or 0/1)</li>
                        <li><b>Marker Variable:</b> Continuous predictor to evaluate</li>
                        <li><b>Time Variable:</b> Follow-up duration (time-dependent mode only)</li>
                    </ul>

                    <h4>Analysis Modes:</h4>
                    <ul>
                        <li><b>Time-Dependent ROC:</b> Evaluates performance at specific timepoints</li>
                        <li><b>Binary ROC:</b> Standard diagnostic ROC (no time variable needed)</li>
                    </ul>
                ")

                self$results$text$setContent(welcome)
                return()
            }

            # Time variable required only for time-dependent mode
            if (self$options$analysisType == "timedep" && is.null(self$options$elapsedtime)) {
                self$results$text$setContent(
                    "<h3>Time Variable Required</h3>
                    <p>Please select a <b>Time Elapsed</b> variable for time-dependent ROC analysis,
                    or switch to <b>General Binary ROC</b> mode under Analysis Type.</p>")
                return()
            }

            private$.cleanData()
        },

        # ── Data cleaning ──────────────────────────────────────
        # TODO(m1): Add variable name escaping for columns with spaces/special
        # chars. Use jmvcore::composeTerm() or backtick-quoting when building
        # formulas or accessing columns programmatically.
        .cleanData = function() {
            data <- self$data

            if (nrow(data) == 0) {
                jmvcore::reject('Data contains no rows')
            }

            time_var <- self$options$elapsedtime
            outcome_var <- self$options$outcome
            marker_var <- self$options$marker
            outcome_level <- self$options$outcomeLevel

            # Input validation
            if (!is.null(time_var) && !is.numeric(data[[time_var]])) {
                jmvcore::reject("Time variable must be numeric")
            }

            if (!is.numeric(data[[marker_var]])) {
                jmvcore::reject("Marker variable must be numeric")
            }

            # Convert outcome to 0/1
            if (is.factor(data[[outcome_var]])) {
                if (is.null(outcome_level)) {
                    jmvcore::reject("Please specify the event level for the outcome variable")
                }
                data$status <- ifelse(data[[outcome_var]] == outcome_level, 1, 0)
            } else {
                if (!all(data[[outcome_var]] %in% c(0, 1, NA))) {
                    jmvcore::reject("Numeric outcome must contain only 0s and 1s")
                }
                data$status <- data[[outcome_var]]
            }

            # Clean time and marker
            if (!is.null(time_var)) {
                data$time <- jmvcore::toNumeric(data[[time_var]])
            } else {
                data$time <- rep(1, nrow(data))
            }
            data$marker <- jmvcore::toNumeric(data[[marker_var]])

            # Remove missing values
            complete_cases <- complete.cases(data[c("time", "status", "marker")])
            private$.complete_rows <- which(complete_cases)
            data <- data[complete_cases, ]

            # Validate final dataset
            if (nrow(data) == 0) {
                jmvcore::reject("No complete cases remaining after removing missing values")
            }

            if (sum(data$status) == 0) {
                private$.addNotice("ERROR", "No Events Found",
                    "The outcome variable contains no events (all 0). ROC analysis requires events to evaluate marker performance.")
                jmvcore::reject("No events found in the outcome variable")
            }

            if (length(unique(data$marker)) < 5) {
                private$.addNotice("WARNING", "Few Unique Marker Values",
                    sprintf("Marker variable has only %d unique values. ROC analysis may be unreliable with limited marker discrimination.",
                        length(unique(data$marker))))
            }

            private$.data <- data[c("time", "status", "marker")]
        },

        # ── Timepoints parsing ─────────────────────────────────
        .parseTimepoints = function() {
            timepoints <- tryCatch({
                pts <- as.numeric(trimws(unlist(strsplit(self$options$timepoints, ","))))
                pts <- sort(unique(pts[!is.na(pts) & pts > 0]))
                if (length(pts) == 0) c(12, 36, 60) else pts
            }, error = function(e) {
                private$.addNotice("WARNING", "Invalid Timepoints",
                    "Could not parse specified timepoints. Using defaults: 12, 36, 60.")
                c(12, 36, 60)
            })

            max_time <- max(private$.data$time, na.rm = TRUE)
            valid_timepoints <- timepoints[timepoints <= max_time]

            if (length(valid_timepoints) == 0) {
                private$.addNotice("WARNING", "Timepoints Adjusted",
                    sprintf("All specified timepoints exceed maximum follow-up (%.1f). Using quartiles of follow-up time instead.",
                        max_time))
                valid_timepoints <- quantile(private$.data$time, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
            }

            private$.timepoints <- valid_timepoints
            return(valid_timepoints)
        },

        # ── Marker statistics ──────────────────────────────────
        .calculateMarkerStats = function() {
            if (!self$options$showMarkerStats) return()

            marker <- private$.data$marker

            stats <- data.frame(
                statistic = c("N", "Mean", "Median", "SD", "IQR", "Min", "Max", "Events", "Event Rate"),
                value = c(
                    length(marker),
                    round(mean(marker, na.rm = TRUE), 3),
                    round(median(marker, na.rm = TRUE), 3),
                    round(sd(marker, na.rm = TRUE), 3),
                    round(IQR(marker, na.rm = TRUE), 3),
                    round(min(marker, na.rm = TRUE), 3),
                    round(max(marker, na.rm = TRUE), 3),
                    sum(private$.data$status),
                    paste0(round(100 * mean(private$.data$status), 1), "%")
                )
            )

            table <- self$results$markerStats
            for (i in 1:nrow(stats)) {
                table$addRow(rowKey = i, values = list(
                    statistic = stats$statistic[i],
                    value = stats$value[i]
                ))
            }
        },

        # ── Optimal cutoffs ────────────────────────────────────
        .calculateOptimalCutoffs = function() {
            if (!self$options$showOptimalCutoff || is.null(private$.fit)) return()

            timepoints <- private$.timepoints
            table <- self$results$cutoffTable

            for (i in seq_along(timepoints)) {
                tryCatch({
                    tp <- timepoints[i]
                    time_idx <- which.min(abs(private$.fit$times - tp))

                    if (length(time_idx) > 0 && !is.null(private$.fit$TP) && !is.null(private$.fit$FP)) {
                        sens <- private$.fit$TP[, time_idx]
                        fpr <- private$.fit$FP[, time_idx]
                        spec <- 1 - fpr

                        valid_idx <- !is.na(sens) & !is.na(spec) & !is.na(private$.fit$marker)
                        if (sum(valid_idx) > 0) {
                            sens_clean <- sens[valid_idx]
                            spec_clean <- spec[valid_idx]
                            marker_clean <- private$.fit$marker[valid_idx]

                            youden <- sens_clean + spec_clean - 1
                            optimal_idx <- which.max(youden)

                            if (length(optimal_idx) > 0 && optimal_idx <= length(marker_clean)) {
                                table$addRow(rowKey = i, values = list(
                                    timepoint = tp,
                                    cutoff = round(marker_clean[optimal_idx], 3),
                                    sensitivity = round(sens_clean[optimal_idx], 3),
                                    specificity = round(spec_clean[optimal_idx], 3),
                                    youden = round(youden[optimal_idx], 3)
                                ))
                            }
                        }
                    }
                }, error = function(e) {
                    private$.addNotice("WARNING", "Cutoff Calculation Failed",
                        sprintf("Could not calculate optimal cutoff for timepoint %s: %s",
                            timepoints[i], e$message))
                })
            }
        },

        # ── Main dispatcher ────────────────────────────────────
        .run = function() {
            if (is.null(private$.data))
                return()

            # Reset notices for fresh run
            private$.noticeList <- list()

            if (self$options$analysisType == "binary") {
                private$.runBinaryROC()
            } else {
                private$.runTimeROC()
            }
        },

        # ── Binary ROC analysis ────────────────────────────────
        .runBinaryROC = function() {
            tryCatch({
                data <- private$.data

                # direction = "auto" lets pROC determine whether higher or
                # lower marker values indicate cases (important for markers
                # like albumin/eGFR where lower = worse)
                primary_roc <- pROC::roc(
                    response = data$status,
                    predictor = data$marker,
                    ci = TRUE,
                    levels = c(0, 1),
                    direction = "auto"
                )

                # Calculate optimal cutoff using Youden index
                if (self$options$youdenIndex) {
                    youden_index <- primary_roc$sensitivities + primary_roc$specificities - 1
                    optimal_idx <- which.max(youden_index)
                    optimal_cutoff <- primary_roc$thresholds[optimal_idx]
                    optimal_sens <- primary_roc$sensitivities[optimal_idx]
                    optimal_spec <- primary_roc$specificities[optimal_idx]
                } else {
                    optimal_cutoff <- NA
                    optimal_sens <- NA
                    optimal_spec <- NA
                }

                # Compute SE from CI (safer than pROC::var which relies on S3 dispatch)
                auc_val <- as.numeric(primary_roc$auc)
                ci_lo <- as.numeric(primary_roc$ci[1])
                ci_hi <- as.numeric(primary_roc$ci[3])
                se_val <- (ci_hi - ci_lo) / (2 * 1.96)

                # Populate binary ROC table
                table <- self$results$binaryROCTable
                table$addRow(rowKey = "primary", values = list(
                    marker = self$options$marker,
                    auc = round(auc_val, 3),
                    se = round(se_val, 3),
                    ci_lower = round(ci_lo, 3),
                    ci_upper = round(ci_hi, 3),
                    sensitivity = round(optimal_sens, 3),
                    specificity = round(optimal_spec, 3),
                    optimal_cutoff = round(optimal_cutoff, 3)
                ))

                private$.primary_roc <- primary_roc

                # Clinical notices for binary ROC
                if (auc_val < 0.5) {
                    private$.addNotice("STRONG_WARNING", "AUC Below 0.5 (Reversed Marker?)",
                        sprintf("AUC = %.3f is below random chance (0.5). The marker direction may be reversed. Consider negating the marker values or changing the direction parameter.",
                            auc_val))
                } else if (auc_val < 0.7) {
                    private$.addNotice("WARNING", "Limited Clinical Utility",
                        sprintf("AUC = %.3f is below 0.70. The marker shows limited discriminative ability for clinical decision-making.",
                            auc_val))
                }

                # Handle comparison if requested
                if (self$options$compareROCs && !is.null(self$options$markers)) {
                    private$.runROCComparison()
                }

                # Generate diagnostic performance summary
                private$.generateDiagnosticSummary()

                # Analysis complete notice
                private$.addNotice("INFO", "Analysis Complete",
                    sprintf("Binary ROC analysis completed. %d observations, %d events (%.1f%% event rate).",
                        nrow(data), sum(data$status), 100 * mean(data$status)))

                private$.renderNotices()

            }, error = function(e) {
                private$.addNotice("ERROR", "Binary ROC Analysis Failed", e$message)
                private$.renderNotices()
                stop(paste("Binary ROC analysis failed:", e$message))
            })
        },

        # ── ROC comparison ─────────────────────────────────────
        .runROCComparison = function() {
            data <- private$.data
            primary_roc <- private$.primary_roc
            markers <- self$options$markers

            if (is.null(markers) || length(markers) == 0) return()

            comparison_table <- self$results$rocComparisonTable

            for (marker in markers) {
                if (marker == self$options$marker) next

                tryCatch({
                    # Use aligned row indices from cleanData
                    raw_marker <- jmvcore::toNumeric(self$data[[marker]])
                    marker_data <- raw_marker[private$.complete_rows]
                    complete_idx <- complete.cases(data$status, marker_data)

                    if (sum(complete_idx) < 10) {
                        private$.addNotice("WARNING", "Insufficient Data",
                            sprintf("Fewer than 10 complete cases for marker: %s. Skipped.", marker))
                        next
                    }

                    comparison_roc <- pROC::roc(
                        response = data$status[complete_idx],
                        predictor = marker_data[complete_idx],
                        levels = c(0, 1),
                        direction = "auto"
                    )

                    test_result <- switch(self$options$rocComparison,
                        "delong" = pROC::roc.test(primary_roc, comparison_roc, method = "delong"),
                        "bootstrap" = pROC::roc.test(primary_roc, comparison_roc, method = "bootstrap", boot.n = 1000),
                        "venkatraman" = pROC::roc.test(primary_roc, comparison_roc, method = "venkatraman")
                    )

                    interpretation <- if (test_result$p.value < 0.05) {
                        "Significantly different"
                    } else {
                        "Not significantly different"
                    }

                    comparison_name <- paste(self$options$marker, "vs", marker)
                    comparison_table$addRow(rowKey = marker, values = list(
                        comparison = comparison_name,
                        method = switch(self$options$rocComparison,
                            "delong" = "DeLong",
                            "bootstrap" = "Bootstrap",
                            "venkatraman" = "Venkatraman"
                        ),
                        test_statistic = round(as.numeric(test_result$statistic), 4),
                        p_value = round(test_result$p.value, 4),
                        interpretation = interpretation
                    ))

                }, error = function(e) {
                    private$.addNotice("WARNING", "Comparison Failed",
                        sprintf("ROC comparison failed for marker %s: %s", marker, e$message))
                })
            }
        },

        # ── Diagnostic summary ─────────────────────────────────
        .generateDiagnosticSummary = function() {
            primary_roc <- private$.primary_roc
            auc_value <- as.numeric(primary_roc$auc)

            auc_interpretation <- if (auc_value >= 0.9) {
                "Excellent discrimination"
            } else if (auc_value >= 0.8) {
                "Good discrimination"
            } else if (auc_value >= 0.7) {
                "Fair discrimination"
            } else if (auc_value >= 0.6) {
                "Poor discrimination"
            } else {
                "No discrimination (equivalent to random chance)"
            }

            summary_html <- glue::glue("
                <h4>Diagnostic Performance Summary</h4>
                <p><strong>AUC:</strong> {round(auc_value, 3)} ({auc_interpretation})</p>
                <p><strong>95% CI:</strong> [{round(primary_roc$ci[1], 3)}, {round(primary_roc$ci[3], 3)}]</p>

                <h4>Clinical Interpretation:</h4>
                <ul>
                    <li>An AUC of {round(auc_value, 3)} indicates correct discrimination in {round(auc_value * 100, 1)}% of randomly selected case-control pairs.</li>
                    <li>The 95% CI suggests the true AUC lies between {round(primary_roc$ci[1], 3)} and {round(primary_roc$ci[3], 3)}.</li>
                </ul>
            ")

            self$results$diagnosticPerformance$setContent(summary_html)
        },

        # ── Time-dependent ROC ─────────────────────────────────
        .runTimeROC = function() {
            timepoints <- private$.parseTimepoints()

            # Handle legacy method values from saved analyses
            method_val <- self$options$method
            if (method_val %in% c("incident", "cumulative", "static")) {
                method_val <- "marginal"
            }

            tryCatch({
                if (length(unique(private$.data$status)) < 2) {
                    stop("Outcome variable must have both event (1) and non-event (0) cases")
                }

                if (max(timepoints) > max(private$.data$time, na.rm = TRUE)) {
                    private$.addNotice("WARNING", "Timepoint Warning",
                        sprintf("Some timepoints exceed maximum follow-up (%.1f %s). Results at those timepoints may be unreliable.",
                            max(private$.data$time, na.rm = TRUE), self$options$timetypeoutput))
                }

                # Check for sufficient events at each timepoint
                for (tp in timepoints) {
                    events_by_time <- sum(private$.data$time <= tp & private$.data$status == 1, na.rm = TRUE)
                    if (events_by_time < 5) {
                        private$.addNotice("WARNING", "Few Events at Timepoint",
                            sprintf("Only %d events observed by timepoint %s %s. ROC estimates may be unreliable (recommend >= 5).",
                                events_by_time, tp, self$options$timetypeoutput))
                    }
                }

                # Compute time-dependent ROC with the selected weighting method
                fit <- timeROC::timeROC(
                    T = private$.data$time,
                    delta = private$.data$status,
                    marker = private$.data$marker,
                    cause = 1,
                    weighting = method_val,
                    times = timepoints,
                    iid = self$options$bootstrapCI,
                    ROC = TRUE
                )

                if (is.null(fit) || is.null(fit$AUC)) {
                    stop("timeROC analysis failed to produce valid results")
                }

                private$.fit <- fit

                # Calculate marker statistics
                private$.calculateMarkerStats()

                # Fill AUC table
                table <- self$results$aucTable
                for (i in seq_along(timepoints)) {
                    tp <- timepoints[i]
                    idx <- which.min(abs(fit$times - tp))

                    if (length(idx) > 0 && !is.na(fit$AUC[idx])) {
                        auc <- fit$AUC[idx]

                        se <- if (is.list(fit$inference) && !is.null(fit$inference$vect_sd_1)) {
                            round(fit$inference$vect_sd_1[idx], 3)
                        } else {
                            NaN
                        }

                        if (!is.nan(se)) {
                            se_val <- as.numeric(se)
                            ci_lower <- round(pmax(0, auc - 1.96 * se_val), 3)
                            ci_upper <- round(pmin(1, auc + 1.96 * se_val), 3)
                        } else {
                            ci_lower <- NaN
                            ci_upper <- NaN
                        }

                        table$addRow(rowKey = i, values = list(
                            timepoint = tp,
                            auc = round(auc, 3),
                            se = se,
                            ci_lower = ci_lower,
                            ci_upper = ci_upper
                        ))
                    }
                }

                # Clinical AUC notices
                auc_values <- fit$AUC[!is.na(fit$AUC)]
                if (length(auc_values) > 0) {
                    if (any(auc_values < 0.5)) {
                        bad_tp <- timepoints[which(fit$AUC < 0.5)[1]]
                        bad_auc <- fit$AUC[which(fit$AUC < 0.5)[1]]
                        private$.addNotice("STRONG_WARNING", "AUC Below 0.5 (Reversed Marker?)",
                            sprintf("At timepoint %s %s, AUC = %.3f is below random chance (0.5). The marker direction may be reversed. Consider negating the marker values.",
                                bad_tp, self$options$timetypeoutput, bad_auc))
                    } else if (mean(auc_values) < 0.7) {
                        private$.addNotice("WARNING", "Limited Clinical Utility",
                            sprintf("Mean AUC = %.3f is below the 0.70 threshold for clinically useful discrimination.",
                                mean(auc_values)))
                    }
                }

                # Calculate optimal cutoffs
                private$.calculateOptimalCutoffs()

                # Create interpretation text
                private$.createInterpretation()

                # Create clinical interpretation
                private$.createClinicalInterpretation()

                # Model comparison if requested
                if (self$options$compareBaseline) {
                    private$.compareToBaseline()
                }

                # Analysis complete notice
                private$.addNotice("INFO", "Analysis Complete",
                    sprintf("Time-dependent ROC analysis completed. %d observations, %d events (%.1f%%), weighting: %s.",
                        nrow(private$.data), sum(private$.data$status),
                        100 * mean(private$.data$status), method_val))

                private$.renderNotices()

            }, error = function(e) {
                error_msg <- sprintf("
                    <h3>Time-Dependent ROC Analysis Error</h3>
                    <p><b>Error:</b> %s</p>
                    <h4>Common Issues:</h4>
                    <ul>
                        <li><b>Time Variable:</b> Must be positive numeric</li>
                        <li><b>Outcome:</b> Must be binary (0/1) or factor with event level</li>
                        <li><b>Marker:</b> Must be continuous with sufficient variation</li>
                        <li><b>Events:</b> Need at least 5-10 events for reliable analysis</li>
                        <li><b>Timepoints:</b> Should be within follow-up period</li>
                    </ul>
                    <h4>Data Summary:</h4>
                    <ul>
                        <li>N: %d, Events: %d (%.1f%%)</li>
                        <li>Follow-up: %.1f to %.1f %s</li>
                        <li>Timepoints: %s</li>
                    </ul>
                ",
                htmltools::htmlEscape(e$message),
                ifelse(is.null(private$.data), 0, nrow(private$.data)),
                ifelse(is.null(private$.data), 0, sum(private$.data$status, na.rm = TRUE)),
                ifelse(is.null(private$.data), 0, 100 * mean(private$.data$status, na.rm = TRUE)),
                ifelse(is.null(private$.data), 0, min(private$.data$time, na.rm = TRUE)),
                ifelse(is.null(private$.data), 0, max(private$.data$time, na.rm = TRUE)),
                self$options$timetypeoutput,
                paste(timepoints, collapse = ", ")
                )

                self$results$text$setContent(error_msg)
                private$.addNotice("ERROR", "Analysis Failed", e$message)
                private$.renderNotices()
            })
        },

        # ── Interpretation text ────────────────────────────────
        .createInterpretation = function() {
            timepoints <- private$.timepoints
            fit <- private$.fit

            method_label <- switch(self$options$method,
                "marginal" = "Cumulative/Dynamic (Kaplan-Meier weighting)",
                "cox" = "Cumulative/Dynamic (Cox model weighting)",
                "aalen" = "Cumulative/Dynamic (Aalen model weighting)",
                "Cumulative/Dynamic"  # fallback for legacy values
            )

            text <- sprintf(
                "<h3>Time-Dependent ROC Analysis Results</h3>
                <p><b>Marker Variable:</b> %s</p>
                <p><b>Analysis Method:</b> %s</p>
                <p><b>Sample Size:</b> %d observations, %d events (%.1f%%)</p>",
                htmltools::htmlEscape(self$options$marker),
                method_label,
                nrow(private$.data),
                sum(private$.data$status),
                100 * mean(private$.data$status)
            )

            if (self$options$bootstrapCI) {
                text <- paste0(text,
                    "<p><b>Confidence Intervals:</b> Asymptotic (influence function-based)</p>"
                )
            }

            text <- paste0(text, "<h4>AUC Interpretation by Timepoint:</h4>")

            # Add interpretation for each timepoint
            for (i in seq_along(timepoints)) {
                tp <- timepoints[i]
                idx <- which.min(abs(fit$times - tp))

                auc <- fit$AUC[idx]

                # SE from influence function (only available when iid=TRUE)
                if (is.list(fit$inference) && !is.null(fit$inference$vect_sd_1)) {
                    se <- fit$inference$vect_sd_1[idx]
                } else {
                    se <- NA
                }

                if (!is.na(se)) {
                    ci_lower <- auc - 1.96 * se
                    ci_upper <- auc + 1.96 * se

                    p_value <- 2 * (1 - pnorm(abs((auc - 0.5) / se)))
                    significance <- ifelse(p_value < 0.05,
                        sprintf(" (p = %.3f, significantly better than chance)", p_value),
                        sprintf(" (p = %.3f, not significantly different from chance)", p_value))
                } else {
                    ci_lower <- NA
                    ci_upper <- NA
                    significance <- " (CI not available: enable Confidence Intervals option)"
                }

                performance <- if (auc >= 0.9) "excellent"
                    else if (auc >= 0.8) "good (0.80-0.89)"
                    else if (auc >= 0.7) "fair (0.70-0.79)"
                    else if (auc >= 0.6) "poor (0.60-0.69)"
                    else "failed (<0.60)"

                text <- paste0(text, sprintf(
                    "<p><b>At %d %s:</b><br>
                    AUC = %.3f (95%% CI: %.3f - %.3f)<br>
                    Performance: %s%s</p>",
                    timepoints[i],
                    self$options$timetypeoutput,
                    auc, ci_lower, ci_upper,
                    performance,
                    significance
                ))
            }

            self$results$text$setContent(text)
        },

        # ── Clinical interpretation ────────────────────────────
        .createClinicalInterpretation = function() {
            auc_values <- private$.fit$AUC
            timepoints <- private$.timepoints

            mean_auc <- mean(auc_values)
            trend <- ifelse(length(auc_values) > 1,
                ifelse(auc_values[length(auc_values)] > auc_values[1], "improving", "declining"),
                "stable")

            method_interp <- switch(self$options$method,
                "marginal" = "This analysis uses Kaplan-Meier (marginal) weighting for censoring adjustment, appropriate when censoring is independent of marker values.",
                "cox" = "This analysis uses Cox model-based IPCW weighting, which adjusts for covariates that may affect censoring.",
                "aalen" = "This analysis uses Aalen additive model for IPCW weighting, providing flexible adjustment for censoring.",
                "Cumulative/Dynamic ROC analysis with IPCW weighting."
            )

            clinical_text <- sprintf("
                <h3>Clinical Interpretation</h3>
                <h4>Overall Performance:</h4>
                <p>The %s shows %s discriminative ability (mean AUC = %.3f).</p>
                <h4>Time Trend:</h4>
                <p>Performance shows a %s trend over time.</p>
                <h4>Clinical Utility:</h4>
                <ul>
                    <li><b>Best Performance:</b> At %d %s (AUC = %.3f)</li>
                    <li><b>Threshold:</b> AUC >= 0.70 is generally considered clinically useful</li>
                    <li><b>Assessment:</b> %s</li>
                </ul>
                <p><b>Method:</b> %s</p>",
                htmltools::htmlEscape(self$options$marker),
                if (mean_auc >= 0.8) "good to excellent"
                    else if (mean_auc >= 0.7) "fair to good"
                    else if (mean_auc >= 0.6) "poor to fair"
                    else "poor",
                mean_auc,
                trend,
                timepoints[which.max(auc_values)],
                self$options$timetypeoutput,
                max(auc_values),
                ifelse(max(auc_values) >= 0.7,
                    "The marker shows clinically relevant predictive ability",
                    "The marker may have limited clinical utility for prediction"),
                method_interp
            )

            self$results$clinicalInterpretation$setContent(clinical_text)
        },

        # ── Model comparison ───────────────────────────────────
        .compareToBaseline = function() {
            # timeROC stores SEs in $inference$vect_sd_1 (only when iid=TRUE)
            if (!self$options$bootstrapCI ||
                !is.list(private$.fit$inference) ||
                is.null(private$.fit$inference$vect_sd_1)) {
                self$results$modelComparison$setContent(
                    "<p>Enable 'Confidence Intervals (Asymptotic)' to compute model comparison (variance estimates are required).</p>")
                return()
            }
            auc_values <- private$.fit$AUC
            se_values <- private$.fit$inference$vect_sd_1
            timepoints <- private$.timepoints

            comparison_text <- "<h3>Model Performance Comparison</h3>"
            comparison_text <- paste0(comparison_text, "<h4>Comparison to Baseline (AUC = 0.5):</h4>")

            for (i in seq_along(timepoints)) {
                auc <- auc_values[i]
                se <- se_values[i]

                z_score <- (auc - 0.5) / se
                p_value <- 2 * (1 - pnorm(abs(z_score)))
                improvement <- round((auc - 0.5) * 100, 1)

                comparison_text <- paste0(comparison_text, sprintf(
                    "<p><b>At %d %s:</b><br>
                    Improvement: +%.1f%% (AUC: %.3f vs 0.50)<br>
                    p = %.4f %s</p>",
                    timepoints[i], self$options$timetypeoutput,
                    improvement, auc, p_value,
                    ifelse(p_value < 0.05, "(significant)", "(not significant)")
                ))
            }

            self$results$modelComparison$setContent(comparison_text)
        },

        # ── Plot: Time-dependent ROC curves ────────────────────
        # TODO(m2): .plotROC uses base graphics — consider ggplot2 for theme consistency
        .plotROC = function(image, ggtheme, theme, ...) {
            if (!self$options$plotROC || is.null(private$.fit))
                return(FALSE)

            timepoints <- private$.timepoints
            fit <- private$.fit

            colors <- c("blue", "red", "green", "purple", "orange", "brown")[1:length(timepoints)]

            plot(0, 0, type = "n", xlim = c(0, 1), ylim = c(0, 1),
                 xlab = "1 - Specificity (False Positive Rate)",
                 ylab = "Sensitivity (True Positive Rate)",
                 main = sprintf("Time-Dependent ROC Curves\nMarker: %s", self$options$marker))

            abline(0, 1, lty = 2, col = "gray", lwd = 2)

            legend_text <- character(length(timepoints))

            for (i in seq_along(timepoints)) {
                if (!is.null(fit$FP) && !is.null(fit$TP)) {
                    fpr <- fit$FP[, i]
                    tpr <- fit$TP[, i]

                    valid_idx <- !is.na(fpr) & !is.na(tpr)
                    if (sum(valid_idx) > 1) {
                        lines(fpr[valid_idx], tpr[valid_idx],
                              col = colors[i], lwd = 2, type = "l")
                    }
                }

                legend_text[i] <- sprintf("%d %s (AUC=%.3f)",
                    timepoints[i], self$options$timetypeoutput, fit$AUC[i])
            }

            legend("bottomright", legend = legend_text,
                   col = colors, lwd = 2, bty = "n", cex = 0.9)
            grid(col = "lightgray", lty = 1, lwd = 0.5)

            TRUE
        },

        # ── Plot: AUC over time ────────────────────────────────
        .plotAUC = function(image, ggtheme, theme, ...) {
            if (!self$options$plotAUC || is.null(private$.fit))
                return(FALSE)

            timepoints <- private$.timepoints
            auc <- private$.fit$AUC

            # timeROC stores SEs in $inference$vect_sd_1 (only when iid=TRUE)
            if (is.list(private$.fit$inference) &&
                !is.null(private$.fit$inference$vect_sd_1) &&
                length(private$.fit$inference$vect_sd_1) >= length(auc)) {
                se <- private$.fit$inference$vect_sd_1[1:length(auc)]
            } else {
                se <- rep(0, length(auc))
            }

            valid_data <- !is.na(auc) & !is.na(timepoints)
            if (sum(valid_data) == 0) return(FALSE)

            plot_data <- data.frame(
                time = timepoints[valid_data],
                auc = auc[valid_data],
                lower = pmax(0, auc[valid_data] - 1.96 * se[valid_data]),
                upper = pmin(1, auc[valid_data] + 1.96 * se[valid_data])
            )

            p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = time, y = auc)) +
                ggplot2::geom_hline(yintercept = 0.5, linetype = "dashed",
                                   color = "red", alpha = 0.7, linewidth = 1) +
                ggplot2::geom_hline(yintercept = 0.7, linetype = "dotted",
                                   color = "orange", alpha = 0.7, linewidth = 0.8) +
                ggplot2::geom_hline(yintercept = 0.8, linetype = "dotdash",
                                   color = "green", alpha = 0.5, linewidth = 0.6) +
                ggplot2::geom_ribbon(ggplot2::aes(ymin = lower, ymax = upper),
                                    alpha = 0.2, fill = "blue") +
                ggplot2::geom_point(color = "blue", size = 3) +
                ggplot2::scale_y_continuous(
                    limits = c(0.4, 1),
                    breaks = seq(0.4, 1, by = 0.1),
                    labels = scales::number_format(accuracy = 0.1)
                ) +
                ggplot2::scale_x_continuous(
                    breaks = plot_data$time,
                    labels = plot_data$time
                ) +
                ggplot2::labs(
                    x = sprintf("Time (%s)", self$options$timetypeoutput),
                    y = "Area Under ROC Curve (AUC)",
                    title = sprintf("Time-Dependent AUC: %s", self$options$marker),
                    subtitle = sprintf("Dashed: Random (0.5) | Dotted: Fair (0.7) | Dot-dash: Good (0.8)"),
                    caption = ifelse(self$options$bootstrapCI,
                        "Error bars: 95% CI (asymptotic, influence function-based)",
                        "No CI (enable Confidence Intervals option for error bars)")
                ) +
                ggtheme +
                ggplot2::theme(
                    plot.title = ggplot2::element_text(face = "bold"),
                    plot.caption = ggplot2::element_text(face = "italic")
                )

            # Add smoothing or line — fixed scope bug
            if (self$options$smoothAUC && nrow(plot_data) > 2) {
                smooth_layer <- tryCatch({
                    ggplot2::geom_smooth(method = "loess", se = FALSE,
                                       color = "darkblue", linewidth = 1.2)
                }, error = function(e) {
                    ggplot2::geom_line(color = "blue", linewidth = 1)
                })
                p <- p + smooth_layer
            } else {
                p <- p + ggplot2::geom_line(color = "blue", linewidth = 1)
            }

            print(p)
            TRUE
        },

        # ── Plot: Binary ROC ───────────────────────────────────
        .plotBinaryROC = function(image, ggtheme, theme, ...) {
            if (!self$options$plotROC || is.null(private$.primary_roc))
                return(FALSE)

            primary_roc <- private$.primary_roc
            auc_val <- round(as.numeric(primary_roc$auc), 3)

            roc_list <- list(primary_roc)
            roc_labels <- sprintf("%s (AUC = %.3f)", self$options$marker, auc_val)
            roc_colors <- c("blue")

            if (self$options$compareROCs && !is.null(self$options$markers)) {
                palette <- c("red", "green4", "purple", "orange", "brown", "cyan4")
                color_idx <- 1
                data <- private$.data
                for (m in self$options$markers) {
                    if (m == self$options$marker) next
                    tryCatch({
                        # Use aligned row indices
                        m_data <- jmvcore::toNumeric(self$data[[m]])
                        m_data <- m_data[private$.complete_rows]
                        ok <- complete.cases(data$status, m_data)
                        if (sum(ok) < 10) next
                        comp_roc <- pROC::roc(
                            response  = data$status[ok],
                            predictor = m_data[ok],
                            levels    = c(0, 1),
                            direction = "auto",
                            quiet     = TRUE
                        )
                        roc_list   <- c(roc_list, list(comp_roc))
                        roc_labels <- c(roc_labels,
                            sprintf("%s (AUC = %.3f)", m, round(as.numeric(comp_roc$auc), 3)))
                        roc_colors <- c(roc_colors,
                            palette[((color_idx - 1) %% length(palette)) + 1])
                        color_idx  <- color_idx + 1
                    }, error = function(e) NULL)
                }
            }

            pROC::plot.roc(roc_list[[1]],
                col = roc_colors[1], lwd = 2,
                main = sprintf("Binary ROC Curve: %s", self$options$marker),
                legacy.axes = TRUE, print.auc = FALSE)

            abline(0, 1, lty = 2, col = "gray60", lwd = 1.5)

            if (length(roc_list) > 1) {
                for (k in 2:length(roc_list)) {
                    pROC::plot.roc(roc_list[[k]],
                        col = roc_colors[k], lwd = 2,
                        add = TRUE, print.auc = FALSE)
                }
            }

            legend("bottomright", legend = roc_labels,
                col = roc_colors, lwd = 2, bty = "n", cex = 0.85)
            grid(col = "lightgray", lty = 1, lwd = 0.5)

            TRUE
        }
    )
)
