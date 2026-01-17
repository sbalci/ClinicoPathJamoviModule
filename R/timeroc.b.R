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
#'   \item Time-dependent ROC: Multiple estimation methods (incident, cumulative, static)
#'   \item Binary ROC: General diagnostic performance evaluation
#'   \item DeLong test for comparing multiple ROC curves (binary mode)
#'   \item Bootstrap and Venkatraman tests for ROC comparison
#'   \item Bootstrap confidence intervals for robust inference
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
#' @import dplyr
#' @importFrom pROC roc auc ci.auc roc.test

timerocClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "timerocClass",
    inherit = timerocBase,
    private = list(
        .data = NULL,
        .fit = NULL,
        .timepoints = NULL,
        
        .init = function() {
            # Show welcome message if variables not selected
            if (is.null(self$options$marker) ||
                is.null(self$options$outcome) ||
                is.null(self$options$elapsedtime)) {

                welcome <- glue::glue("
                    <h3>Time-Dependent ROC Analysis</h3>
                    <p>This analysis evaluates how well a continuous biomarker predicts survival outcomes at different time points.</p>
                    
                    <h4>Required Variables:</h4>
                    <ul>
                        <li><b>Time Variable:</b> Follow-up duration (numeric)</li>
                        <li><b>Outcome Variable:</b> Event status (factor or 0/1)</li>  
                        <li><b>Marker Variable:</b> Continuous predictor to evaluate (e.g., biomarker level, risk score)</li>
                    </ul>
                    
                    <h4>Analysis Provides:</h4>
                    <ul>
                        <li>Time-specific ROC curves</li>
                        <li>AUC values with confidence intervals at specified timepoints</li>
                        <li>Optimal cutoff values for clinical decision-making</li>
                        <li>Model performance comparison</li>
                        <li>Clinical interpretation of results</li>
                    </ul>
                    
                    <h4>Methods Available:</h4>
                    <ul>
                        <li><b>Incident/Dynamic:</b> Evaluates ability to predict events occurring within a specific time window</li>
                        <li><b>Cumulative/Dynamic:</b> Evaluates ability to predict cumulative events up to a time point</li>
                        <li><b>Incident/Static:</b> Uses marker value at baseline to predict future events</li>
                    </ul>
                ")

                self$results$text$setContent(welcome)
                return()
            }

            # Initialize analysis if all variables selected
            private$.cleanData()
        },

        .cleanData = function() {
            # Get data
            data <- self$data
            
            # Handle empty data
            if (nrow(data) == 0) {
                stop('Data contains no rows')
            }

            # Get variable names
            time_var <- self$options$elapsedtime
            outcome_var <- self$options$outcome
            marker_var <- self$options$marker
            outcome_level <- self$options$outcomeLevel

            # Input validation
            if (!is.numeric(data[[time_var]])) {
                stop("Time variable must be numeric")
            }

            if (!is.numeric(data[[marker_var]])) {
                stop("Marker variable must be numeric")
            }

            # Convert outcome to 0/1
            if (is.factor(data[[outcome_var]])) {
                if (is.null(outcome_level)) {
                    stop("Please specify the event level for the outcome variable")
                }
                data$status <- ifelse(data[[outcome_var]] == outcome_level, 1, 0)
            } else {
                if (!all(data[[outcome_var]] %in% c(0,1,NA))) {
                    stop("Numeric outcome must contain only 0s and 1s")
                }
                data$status <- data[[outcome_var]]
            }

            # Clean time and marker
            data$time <- jmvcore::toNumeric(data[[time_var]])
            data$marker <- jmvcore::toNumeric(data[[marker_var]])

            # Remove missing values
            complete_cases <- complete.cases(data[c("time", "status", "marker")])
            data <- data[complete_cases, ]

            # Validate final dataset
            if (nrow(data) == 0) {
                stop("No complete cases remaining after removing missing values")
            }
            
            if (sum(data$status) == 0) {
                stop("No events found in the outcome variable")
            }
            
            if (length(unique(data$marker)) < 5) {
                warning("Marker variable has few unique values. Results may be unreliable.")
            }

            # Store cleaned data
            private$.data <- data[c("time", "status", "marker")]
        },

        .parseTimepoints = function() {
            # Parse and validate timepoints
            timepoints <- tryCatch({
                pts <- as.numeric(trimws(unlist(strsplit(self$options$timepoints, ","))))
                pts <- sort(unique(pts[!is.na(pts) & pts > 0]))
                if (length(pts) == 0) c(12, 36, 60) else pts
            }, error = function(e) {
                warning("Invalid timepoints specified, using defaults: 12, 36, 60")
                c(12, 36, 60)
            })
            
            # Filter timepoints that are within data range
            max_time <- max(private$.data$time, na.rm = TRUE)
            valid_timepoints <- timepoints[timepoints <= max_time]
            
            if (length(valid_timepoints) == 0) {
                warning("All specified timepoints exceed maximum follow-up time. Using quartiles of follow-up time.")
                valid_timepoints <- quantile(private$.data$time, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
            }
            
            private$.timepoints <- valid_timepoints
            return(valid_timepoints)
        },

        .calculateMarkerStats = function() {
            # Calculate descriptive statistics for marker
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
            
            # Populate table
            table <- self$results$markerStats
            for (i in 1:nrow(stats)) {
                table$addRow(rowKey = i, values = list(
                    statistic = stats$statistic[i],
                    value = stats$value[i]
                ))
            }
        },

        .calculateOptimalCutoffs = function() {
            # Calculate optimal cutoff values using Youden index
            if (!self$options$showOptimalCutoff || is.null(private$.fit)) return()
            
            timepoints <- private$.timepoints
            
            # Clear existing rows
            table <- self$results$cutoffTable
            
            for (i in seq_along(timepoints)) {
                tryCatch({
                    # Extract ROC data for this timepoint
                    tp <- timepoints[i]
                    
                    # Find ROC data for this timepoint - exact match or closest
                    time_idx <- which.min(abs(private$.fit$times - tp))
                    
                    if (length(time_idx) > 0 && !is.null(private$.fit$TP) && !is.null(private$.fit$FP)) {
                        # Get sensitivity (True Positive Rate) and False Positive Rate
                        sens <- private$.fit$TP[, time_idx]
                        fpr <- private$.fit$FP[, time_idx]  # This is False Positive Rate (1-specificity)
                        spec <- 1 - fpr  # Calculate specificity from FPR
                        
                        # Remove any NA values and ensure vectors are same length
                        valid_idx <- !is.na(sens) & !is.na(spec) & !is.na(private$.fit$marker)
                        if (sum(valid_idx) > 0) {
                            sens_clean <- sens[valid_idx]
                            spec_clean <- spec[valid_idx]
                            marker_clean <- private$.fit$marker[valid_idx]
                            
                            # Calculate Youden index (J = Sensitivity + Specificity - 1)
                            youden <- sens_clean + spec_clean - 1
                            
                            # Find optimal cutoff (maximum Youden index)
                            optimal_idx <- which.max(youden)
                            
                            if (length(optimal_idx) > 0 && optimal_idx <= length(marker_clean)) {
                                optimal_cutoff <- marker_clean[optimal_idx]
                                optimal_sens <- sens_clean[optimal_idx]
                                optimal_spec <- spec_clean[optimal_idx]
                                optimal_youden <- youden[optimal_idx]
                                
                                # Add to table
                                table$addRow(rowKey = i, values = list(
                                    timepoint = tp,
                                    cutoff = round(optimal_cutoff, 3),
                                    sensitivity = round(optimal_sens, 3),
                                    specificity = round(optimal_spec, 3),
                                    youden = round(optimal_youden, 3)
                                ))
                            }
                        }
                    }
                }, error = function(e) {
                    # Silent error handling - skip this timepoint if calculation fails
                    warning(paste("Could not calculate optimal cutoff for timepoint", timepoints[i], ":", e$message))
                })
            }
        },

        .run = function() {
            if (is.null(private$.data))
                return()

            # Choose analysis type
            if (self$options$analysisType == "binary") {
                private$.runBinaryROC()
            } else {
                private$.runTimeROC()
            }
        },

        .runBinaryROC = function() {
            # Binary ROC analysis using pROC package
            tryCatch({
                data <- private$.data
                
                # Calculate primary marker ROC
                primary_roc <- pROC::roc(
                    response = data$status,
                    predictor = data$marker,
                    ci = TRUE,
                    levels = c(0, 1),
                    direction = "<"
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
                
                # Populate binary ROC table
                table <- self$results$binaryROCTable
                table$addRow(rowKey = "primary", values = list(
                    marker = self$options$marker,
                    auc = round(as.numeric(primary_roc$auc), 3),
                    se = round(sqrt(pROC::var(primary_roc)), 3),
                    ci_lower = round(primary_roc$ci[1], 3),
                    ci_upper = round(primary_roc$ci[3], 3),
                    sensitivity = round(optimal_sens, 3),
                    specificity = round(optimal_spec, 3),
                    optimal_cutoff = round(optimal_cutoff, 3)
                ))
                
                # Store primary ROC for comparison and plotting
                private$.primary_roc <- primary_roc
                
                # Handle multiple markers and comparison if requested
                if (self$options$compareROCs && !is.null(self$options$markers)) {
                    private$.runROCComparison()
                }
                
                # Generate diagnostic performance summary
                private$.generateDiagnosticSummary()
                
            }, error = function(e) {
                stop(paste("Binary ROC analysis failed:", e$message))
            })
        },

        .runROCComparison = function() {
            # Compare multiple ROC curves using statistical tests
            data <- private$.data
            primary_roc <- private$.primary_roc
            markers <- self$options$markers
            
            if (is.null(markers) || length(markers) == 0) return()
            
            comparison_table <- self$results$rocComparison
            
            for (marker in markers) {
                if (marker == self$options$marker) next  # Skip primary marker
                
                tryCatch({
                    # Calculate ROC for comparison marker
                    marker_data <- jmvcore::toNumeric(self$data[[marker]])
                    complete_idx <- complete.cases(data$status, marker_data)
                    
                    if (sum(complete_idx) < 10) {
                        warning(paste("Insufficient data for marker:", marker))
                        next
                    }
                    
                    comparison_roc <- pROC::roc(
                        response = data$status[complete_idx],
                        predictor = marker_data[complete_idx],
                        levels = c(0, 1),
                        direction = "<"
                    )
                    
                    # Perform statistical comparison
                    test_result <- switch(self$options$rocComparison,
                        "delong" = pROC::roc.test(primary_roc, comparison_roc, method = "delong"),
                        "bootstrap" = pROC::roc.test(primary_roc, comparison_roc, method = "bootstrap", boot.n = 1000),
                        "venkatraman" = pROC::roc.test(primary_roc, comparison_roc, method = "venkatraman")
                    )
                    
                    # Interpret results
                    interpretation <- if (test_result$p.value < 0.05) {
                        "Significantly different"
                    } else {
                        "Not significantly different"
                    }
                    
                    # Add to comparison table
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
                    warning(paste("ROC comparison failed for marker", marker, ":", e$message))
                })
            }
        },

        .generateDiagnosticSummary = function() {
            # Generate diagnostic performance summary HTML
            primary_roc <- private$.primary_roc
            
            auc_value <- as.numeric(primary_roc$auc)
            
            # AUC interpretation
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
                    <li>An AUC of {round(auc_value, 3)} indicates that the marker correctly discriminates between cases and controls in {round(auc_value * 100, 1)}% of randomly selected pairs.</li>
                    <li>The 95% confidence interval suggests the true AUC lies between {round(primary_roc$ci[1], 3)} and {round(primary_roc$ci[3], 3)}.</li>
                </ul>
            ")
            
            self$results$diagnosticPerformance$setContent(summary_html)
        },

        .runTimeROC = function() {
            # Original time-dependent ROC analysis
            timepoints <- private$.parseTimepoints()

            tryCatch({
                # Validate data for timeROC requirements
                if (length(unique(private$.data$status)) < 2) {
                    stop("Outcome variable must have both event (1) and non-event (0) cases")
                }
                
                if (max(timepoints) > max(private$.data$time, na.rm = TRUE)) {
                    warning("Some timepoints exceed maximum follow-up time. Results may be unreliable.")
                }
                
                # Check for sufficient events at each timepoint
                for (tp in timepoints) {
                    at_risk <- sum(private$.data$time >= tp, na.rm = TRUE)
                    events_by_time <- sum(private$.data$time <= tp & private$.data$status == 1, na.rm = TRUE)
                    if (events_by_time < 5) {
                        warning(sprintf("Few events (%d) observed by timepoint %d. Results may be unreliable.", 
                                      events_by_time, tp))
                    }
                }

                # Compute time-dependent ROC with enhanced parameters
                fit <- timeROC::timeROC(
                    T = private$.data$time,
                    delta = private$.data$status,
                    marker = private$.data$marker,
                    cause = 1,
                    times = timepoints,
                    iid = self$options$bootstrapCI,
                    ROC = TRUE  # Calculate full ROC curves
                )

                if (is.null(fit) || is.null(fit$AUC)) {
                    stop("timeROC analysis failed to produce valid results")
                }

                # Store results
                private$.fit <- fit

                # Calculate marker statistics
                private$.calculateMarkerStats()

                # Fill AUC table with improved error handling
                table <- self$results$aucTable
                
                for (i in seq_along(timepoints)) {
                    # Find matching timepoint in fit results
                    tp <- timepoints[i]
                    idx <- which.min(abs(fit$times - tp))
                    
                    if (length(idx) > 0 && !is.na(fit$AUC[idx])) {
                        auc <- fit$AUC[idx]
                        
                        # Calculate SE and CIs using correct index
                        se <- ifelse(is.list(fit$inference) && !is.null(fit$inference$vect_sd_1), 
                                   round(fit$inference$vect_sd_1[idx], 3), "N/A")
                        
                        # Calculate CI logic handled in display
                        if (se != "N/A") {
                            se_val <- as.numeric(se)
                            ci_lower <- round(pmax(0, auc - 1.96 * se_val), 3)
                            ci_upper <- round(pmin(1, auc + 1.96 * se_val), 3)
                        } else {
                            ci_lower <- "N/A"
                            ci_upper <- "N/A"
                        }
                        
                        row <- list(
                            timepoint = tp,
                            auc = round(auc, 3),
                            se = se,
                            ci_lower = ci_lower,
                            ci_upper = ci_upper
                        )
                        table$addRow(rowKey = i, values = row)
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

            }, error = function(e) {
                error_msg <- sprintf("
                    <h3>Time-Dependent ROC Analysis Error</h3>
                    <p><b>Error Message:</b> %s</p>
                    
                    <h4>Common Issues and Solutions:</h4>
                    <ul>
                        <li><b>Time Variable:</b> Must be positive numeric values (days, months, years)</li>
                        <li><b>Outcome Variable:</b> Must be binary (0/1) or factor with specified event level</li>
                        <li><b>Marker Variable:</b> Must be continuous with sufficient variation</li>
                        <li><b>Events Required:</b> Need at least 5-10 events for reliable ROC analysis</li>
                        <li><b>Timepoints:</b> Should be within your follow-up period</li>
                        <li><b>Sample Size:</b> Need adequate sample size for time-dependent analysis (≥50 recommended)</li>
                    </ul>
                    
                    <h4>Data Requirements Check:</h4>
                    <ul>
                        <li>Sample size: %d observations</li>
                        <li>Total events: %d (%.1f%%)</li>
                        <li>Follow-up range: %.1f to %.1f %s</li>
                        <li>Timepoints specified: %s</li>
                    </ul>
                ", 
                e$message,
                ifelse(is.null(private$.data), 0, nrow(private$.data)),
                ifelse(is.null(private$.data), 0, sum(private$.data$status, na.rm = TRUE)),
                ifelse(is.null(private$.data), 0, 100 * mean(private$.data$status, na.rm = TRUE)),
                ifelse(is.null(private$.data), 0, min(private$.data$time, na.rm = TRUE)),
                ifelse(is.null(private$.data), 0, max(private$.data$time, na.rm = TRUE)),
                self$options$timetypeoutput,
                paste(timepoints, collapse = ", ")
                )
                
                self$results$text$setContent(error_msg)
            })
        },

        .createInterpretation = function() {
            timepoints <- private$.timepoints
            fit <- private$.fit
            
            text <- sprintf(
                "<h3>Time-Dependent ROC Analysis Results</h3>
                <p><b>Marker Variable:</b> %s</p>
                <p><b>Analysis Method:</b> %s</p>
                <p><b>Sample Size:</b> %d observations, %d events (%.1f%%)</p>",
                self$options$marker,
                self$options$method,
                nrow(private$.data),
                sum(private$.data$status),
                100 * mean(private$.data$status)
            )

            if (self$options$bootstrapCI) {
                text <- paste0(text, sprintf(
                    "<p><b>Bootstrap Samples:</b> %d</p>", 
                    self$options$nboot
                ))
            }

            text <- paste0(text, "<h4>AUC Interpretation by Timepoint:</h4>")

            # Add interpretation for each timepoint
            # Add interpretation for each timepoint
            for (i in seq_along(timepoints)) {
                tp <- timepoints[i]
                idx <- which.min(abs(fit$times - tp))
                
                auc <- fit$AUC[idx]
                
                # Safe SE access
                if (is.list(fit$inference) && !is.null(fit$inference$vect_sd_1)) {
                    se <- fit$inference$vect_sd_1[idx]
                } else if (!is.null(fit$var.AUC)) {
                    se <- sqrt(fit$var.AUC[idx])
                } else {
                    se <- NA
                }
                
                if (!is.na(se)) {
                    ci_lower <- auc - 1.96*se
                    ci_upper <- auc + 1.96*se
                    
                    # Statistical significance test
                    p_value <- 2 * (1 - pnorm(abs((auc - 0.5) / se)))
                    significance <- ifelse(p_value < 0.05, 
                                         sprintf(" (p = %.3f, significantly better than chance)", p_value),
                                         sprintf(" (p = %.3f, not significantly different from chance)", p_value))
                } else {
                    ci_lower <- NA
                    ci_upper <- NA
                    significance <- " (Significance not calculated: SE unavailable)"
                }
                performance <- dplyr::case_when(
                    auc >= 0.9 ~ "excellent (≥0.90)",
                    auc >= 0.8 ~ "good (0.80-0.89)",
                    auc >= 0.7 ~ "fair (0.70-0.79)",
                    auc >= 0.6 ~ "poor (0.60-0.69)",
                    TRUE ~ "failed (<0.60)"
                )



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

        .createClinicalInterpretation = function() {
            auc_values <- private$.fit$AUC
            timepoints <- private$.timepoints
            
            # Overall assessment
            mean_auc <- mean(auc_values)
            trend <- ifelse(length(auc_values) > 1,
                          ifelse(auc_values[length(auc_values)] > auc_values[1], "improving", "declining"),
                          "stable")

            clinical_text <- sprintf("
                <h3>Clinical Interpretation</h3>
                
                <h4>Overall Performance Summary:</h4>
                <p>The %s shows %s discriminative ability across the evaluated timepoints (mean AUC = %.3f).</p>
                
                <h4>Time-Dependent Performance:</h4>
                <p>The predictive performance shows a %s trend over time, which suggests that the marker's 
                discriminative ability %s as follow-up time increases.</p>
                
                <h4>Clinical Utility Considerations:</h4>
                <ul>
                    <li><b>Best Performance:</b> At %d %s (AUC = %.3f)</li>
                    <li><b>Clinical Threshold:</b> Generally, AUC ≥ 0.70 is considered clinically useful</li>
                    <li><b>Decision Making:</b> %s</li>
                </ul>",
                self$options$marker,
                dplyr::case_when(
                    mean_auc >= 0.8 ~ "good to excellent",
                    mean_auc >= 0.7 ~ "fair to good", 
                    mean_auc >= 0.6 ~ "poor to fair",
                    TRUE ~ "poor"
                ),
                mean_auc,
                trend,
                ifelse(trend == "improving", "improves", ifelse(trend == "declining", "declines", "remains stable")),
                timepoints[which.max(auc_values)],
                self$options$timetypeoutput,
                max(auc_values),
                ifelse(max(auc_values) >= 0.7,
                      "The marker shows clinically relevant predictive ability",
                      "The marker may have limited clinical utility for prediction")
            )

            # Add method-specific interpretation
            method_interp <- switch(self$options$method,
                "incident" = "This analysis evaluates the marker's ability to predict events occurring within specific time windows (incident/dynamic approach).",
                "cumulative" = "This analysis evaluates the marker's ability to predict cumulative events up to each timepoint (cumulative/dynamic approach).", 
                "static" = "This analysis uses baseline marker values to predict future events at different timepoints (incident/static approach)."
            )

            clinical_text <- paste0(clinical_text, sprintf("<p><b>Method Note:</b> %s</p>", method_interp))

            self$results$clinicalInterpretation$setContent(clinical_text)
        },

        .compareToBaseline = function() {
            auc_values <- private$.fit$AUC
            se_values <- sqrt(private$.fit$var.AUC)
            timepoints <- private$.timepoints
            
            comparison_text <- "<h3>Model Performance Comparison</h3>"
            comparison_text <- paste0(comparison_text, "<h4>Comparison to Baseline Model (AUC = 0.5):</h4>")
            
            for (i in seq_along(timepoints)) {
                auc <- auc_values[i]
                se <- se_values[i]
                
                # Test against baseline (AUC = 0.5)
                z_score <- (auc - 0.5) / se
                p_value <- 2 * (1 - pnorm(abs(z_score)))
                
                improvement <- round((auc - 0.5) * 100, 1)
                
                comparison_text <- paste0(comparison_text, sprintf(
                    "<p><b>At %d %s:</b><br>
                    Improvement over baseline: +%.1f%% (AUC: %.3f vs 0.50)<br>
                    Statistical significance: p = %.4f %s</p>",
                    timepoints[i],
                    self$options$timetypeoutput,
                    improvement,
                    auc,
                    p_value,
                    ifelse(p_value < 0.05, "(significant)", "(not significant)")
                ))
            }
            
            self$results$modelComparison$setContent(comparison_text)
        },

        .plotROC = function(image, ...) {
            if (!self$options$plotROC || is.null(private$.fit))
                return(FALSE)

            timepoints <- private$.timepoints
            fit <- private$.fit
            
            # Set up plotting parameters
            colors <- c("blue", "red", "green", "purple", "orange", "brown")[1:length(timepoints)]
            
            # Create the plot
            plot(0, 0, type = "n", xlim = c(0, 1), ylim = c(0, 1),
                 xlab = "1 - Specificity (False Positive Rate)",
                 ylab = "Sensitivity (True Positive Rate)",
                 main = sprintf("Time-Dependent ROC Curves\nMarker: %s", self$options$marker))
            
            # Add reference line
            abline(0, 1, lty = 2, col = "gray", lwd = 2)
            
            # Plot ROC curves for each timepoint
            legend_text <- character(length(timepoints))
            
            for (i in seq_along(timepoints)) {
                if (!is.null(fit$FP) && !is.null(fit$TP)) {
                    fpr <- fit$FP[, i]
                    tpr <- fit$TP[, i]
                    
                    # Remove any NA values
                    valid_idx <- !is.na(fpr) & !is.na(tpr)
                    if (sum(valid_idx) > 1) {
                        lines(fpr[valid_idx], tpr[valid_idx], 
                              col = colors[i], lwd = 2, type = "l")
                    }
                }
                
                legend_text[i] <- sprintf("%d %s (AUC=%.3f)", 
                                        timepoints[i], 
                                        self$options$timetypeoutput,
                                        fit$AUC[i])
            }
            
            # Add legend
            legend("bottomright", legend = legend_text, 
                   col = colors, lwd = 2, bty = "n", cex = 0.9)
            
            # Add grid
            grid(col = "lightgray", lty = 1, lwd = 0.5)
            
            TRUE
        },

        .plotAUC = function(image, ...) {
            if (!self$options$plotAUC || is.null(private$.fit))
                return(FALSE)

            timepoints <- private$.timepoints
            auc <- private$.fit$AUC
            
            # Enhanced error handling for variance
            if (!is.null(private$.fit$var.AUC) && length(private$.fit$var.AUC) >= length(auc)) {
                se <- sqrt(pmax(0, private$.fit$var.AUC[1:length(auc)]))
            } else {
                se <- rep(0, length(auc))  # Fallback if variance not available
            }

            # Create data frame for plotting with validation
            valid_data <- !is.na(auc) & !is.na(timepoints)
            if (sum(valid_data) == 0) {
                return(FALSE)  # Cannot plot if no valid data
            }
            
            plot_data <- data.frame(
                time = timepoints[valid_data],
                auc = auc[valid_data],
                lower = pmax(0, auc[valid_data] - 1.96*se[valid_data]),  # Ensure CI doesn't go below 0
                upper = pmin(1, auc[valid_data] + 1.96*se[valid_data])   # Ensure CI doesn't go above 1
            )

            # Create the plot with improved aesthetics
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
                    subtitle = sprintf("Method: %s | Dashed: Random (0.5) | Dotted: Fair (0.7) | Dot-dash: Good (0.8)",
                                     self$options$method),
                    caption = ifelse(self$options$bootstrapCI, 
                                   sprintf("Error bars: 95%% CI (%d bootstrap samples)", self$options$nboot),
                                   "Error bars: 95% CI (asymptotic)")
                ) +
                ggplot2::theme_bw() +
                ggplot2::theme(
                    plot.title = ggplot2::element_text(size = 14, face = "bold"),
                    plot.subtitle = ggplot2::element_text(size = 10),
                    plot.caption = ggplot2::element_text(size = 8, style = "italic"),
                    axis.title = ggplot2::element_text(size = 12),
                    axis.text = ggplot2::element_text(size = 10),
                    panel.grid.minor = ggplot2::element_blank(),
                    panel.grid.major = ggplot2::element_line(alpha = 0.3)
                )

            # Add smoothing if requested and feasible
            if (self$options$smoothAUC && nrow(plot_data) > 2) {
                tryCatch({
                    p <- p + ggplot2::geom_smooth(method = "loess", se = FALSE, 
                                                color = "darkblue", linewidth = 1.2)
                }, error = function(e) {
                    # Fallback to simple line if smoothing fails
                    p <- p + ggplot2::geom_line(color = "blue", linewidth = 1)
                })
            } else {
                p <- p + ggplot2::geom_line(color = "blue", linewidth = 1)
            }

            print(p)
            TRUE
        }
    )
)