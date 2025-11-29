
#' @title Time-Dependent Decision Curve Analysis
#' @importFrom R6 R6Class
#' @import jmvcore
#' @importFrom stats qnorm quantile predict loess
#' @importFrom survival Surv survfit coxph
#' @importFrom graphics plot lines legend abline
#' @export


timedependentdcaClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "timedependentdcaClass",
    inherit = timedependentdcaBase,
    private = list(

        # Data storage
        .data_prepared = NULL,
        .time_points = NULL,
        .thresholds = NULL,
        .results_by_time = NULL,

        #---------------------------------------------
        # INIT
        #---------------------------------------------
        .init = function() {

            # Instructions
            html <- "<h3>Time-Dependent Decision Curve Analysis</h3>
            <p>Time-dependent DCA evaluates the clinical utility of prognostic models for time-to-event outcomes.</p>
            <h4>Net Benefit Formula (Time-Dependent):</h4>
            <p style='font-family: monospace;'>
            NB(t, p<sub>t</sub>) = [TP(t) / N] - [FP(t) / N] × [p<sub>t</sub> / (1 - p<sub>t</sub>)]
            </p>
            <p>Where:</p>
            <ul>
            <li><b>t:</b> Time point of interest (e.g., 1 year, 5 years)</li>
            <li><b>p<sub>t</sub>:</b> Threshold probability at time t</li>
            <li><b>TP(t):</b> True positives (events correctly predicted by time t)</li>
            <li><b>FP(t):</b> False positives (non-events incorrectly predicted)</li>
            </ul>
            <h4>Applications:</h4>
            <ul>
            <li><b>Recurrence prediction:</b> Serial biopsy surveillance decisions</li>
            <li><b>Survival models:</b> Treatment vs palliative care thresholds</li>
            <li><b>Competing risks:</b> Recurrence vs death decision making</li>
            </ul>"

            self$results$instructionsText$setContent(html)

            # Interpretation guide
            interp_html <- "<h3>Interpretation Guide</h3>
            <h4>Net Benefit Interpretation:</h4>
            <ul>
            <li><b>NB > 0:</b> Using the model provides benefit over default strategies</li>
            <li><b>NB = 0:</b> No benefit (equivalent to 'treat none')</li>
            <li><b>NB < 0:</b> Model causes net harm</li>
            </ul>
            <h4>Curve Comparison:</h4>
            <ul>
            <li><b>Model above 'Treat All':</b> Model reduces unnecessary interventions</li>
            <li><b>Model above 'Treat None':</b> Model identifies high-risk patients</li>
            <li><b>Wider range above references:</b> More clinically useful</li>
            </ul>
            <h4>Time-Specific Considerations:</h4>
            <ul>
            <li><b>Early time points:</b> Higher uncertainty, fewer events observed</li>
            <li><b>Late time points:</b> More events, but censoring reduces sample size</li>
            <li><b>Clinical relevance:</b> Choose time points matching decision windows</li>
            </ul>"

            self$results$interpretationText$setContent(interp_html)
        },

        #---------------------------------------------
        # RUN
        #---------------------------------------------
        .run = function() {

            # Check requirements
            if (is.null(self$options$time) || self$options$time == "") {
                return()
            }

            if (is.null(self$options$event) || self$options$event == "") {
                return()
            }

            if (is.null(self$options$predictor) || self$options$predictor == "") {
                return()
            }

            # Prepare data
            tryCatch({
                private$.prepareData()
            }, error = function(e) {
                stop(paste("Data preparation error:", e$message))
            })

            if (is.null(private$.data_prepared)) {
                return()
            }

            # Calculate time-dependent net benefit
            tryCatch({
                private$.calculateTimeDependentNB()
            }, error = function(e) {
                stop(paste("Net benefit calculation error:", e$message))
            })

            # Populate results
            private$.populateNetBenefitTable()
            private$.populateSummaryTable()
            private$.populateInterventionsTable()
        },

        #---------------------------------------------
        # PREPARE DATA
        #---------------------------------------------
        .prepareData = function() {

            data <- self$data
            time_var <- self$options$time
            event_var <- self$options$event
            predictor_var <- self$options$predictor

            # Extract data
            time <- as.numeric(data[[time_var]])
            event <- as.numeric(data[[event_var]])
            predictor <- as.numeric(data[[predictor_var]])

            # Check for missing values
            if (any(is.na(time)) || any(is.na(event)) || any(is.na(predictor))) {
                complete_cases <- !is.na(time) & !is.na(event) & !is.na(predictor)
                time <- time[complete_cases]
                event <- event[complete_cases]
                predictor <- predictor[complete_cases]
                warning(paste("Removed", sum(!complete_cases), "cases with missing values"))
            }

            # Parse time points
            time_points_str <- self$options$time_points
            time_points <- as.numeric(unlist(strsplit(time_points_str, ",")))
            time_points <- time_points[!is.na(time_points)]

            if (length(time_points) == 0) {
                stop("No valid time points specified")
            }

            # Generate threshold sequence
            threshold_min <- self$options$threshold_range_min
            threshold_max <- self$options$threshold_range_max
            n_steps <- self$options$threshold_steps

            thresholds <- seq(threshold_min, threshold_max, length.out = n_steps)

            private$.data_prepared <- list(
                time = time,
                event = event,
                predictor = predictor,
                n = length(time)
            )

            private$.time_points <- time_points
            private$.thresholds <- thresholds
        },

        #---------------------------------------------
        # CALCULATE TIME-DEPENDENT NET BENEFIT
        #---------------------------------------------
        .calculateTimeDependentNB = function() {

            time <- private$.data_prepared$time
            event <- private$.data_prepared$event
            predictor <- private$.data_prepared$predictor
            n <- private$.data_prepared$n
            time_points <- private$.time_points
            thresholds <- private$.thresholds

            set.seed(self$options$random_seed)

            estimate_method <- self$options$estimate_survival
            results_by_time <- list()

            # 1. Calculate risk scores (probabilities) for all patients
            # This part remains similar - we need P(T <= t | X) for each patient
            
            # Pre-calculate risk scores for all time points if possible, or loop
            # For simplicity and correctness with the existing structure, we loop by time point
            
            for (t_eval in time_points) {

                # --- Step A: Calculate Risk Scores (Probabilities) ---
                if (estimate_method == "direct") {
                    prob_event <- predictor
                } else if (estimate_method == "kaplan_meier") {
                    # Stratified KM
                    n_groups <- if (n < 100) 3 else if (n < 300) 5 else 10
                    predictor_breaks <- quantile(predictor, probs = seq(0, 1, length = n_groups + 1), na.rm = TRUE)
                    # Handle unique breaks issue if predictor has many ties
                    if (length(unique(predictor_breaks)) < length(predictor_breaks)) {
                         predictor_groups <- as.numeric(cut(predictor, breaks = unique(predictor_breaks), include.lowest = TRUE))
                    } else {
                         predictor_groups <- as.numeric(cut(predictor, breaks = predictor_breaks, include.lowest = TRUE, labels = FALSE))
                    }

                    surv_obj <- Surv(time, event)
                    prob_event <- numeric(n)
                    
                    tryCatch({
                        surv_by_group <- survfit(surv_obj ~ predictor_groups)
                        group_summary <- summary(surv_by_group, times = t_eval, extend = TRUE)
                        
                        for (g in unique(predictor_groups)) {
                            if (is.na(g)) next
                            group_mask <- predictor_groups == g
                            
                            # Find survival for this group
                            if (!is.null(group_summary$strata)) {
                                stratum_name <- paste0("predictor_groups=", g)
                                stratum_idx <- which(group_summary$strata == stratum_name)
                                if (length(stratum_idx) > 0) {
                                    surv_g <- group_summary$surv[stratum_idx[1]]
                                    prob_event[group_mask] <- 1 - surv_g
                                } else {
                                     # Fallback if stratum missing in summary (rare)
                                     prob_event[group_mask] <- mean(event[group_mask], na.rm=TRUE) 
                                }
                            } else {
                                # Single group case
                                prob_event[group_mask] <- 1 - group_summary$surv[1]
                            }
                        }
                    }, error = function(e) {
                        warning("KM stratification failed: ", e$message)
                        km_overall <- survfit(surv_obj ~ 1)
                        surv_t <- summary(km_overall, times = t_eval, extend = TRUE)$surv[1]
                        prob_event <<- rep(1 - surv_t, n)
                    })
                    
                } else { # cox
                    surv_obj <- Surv(time, event)
                    tryCatch({
                        cox_fit <- coxph(surv_obj ~ predictor)
                        newdata <- data.frame(predictor = predictor)
                        surv_fit <- survfit(cox_fit, newdata = newdata)
                        
                        # Extract survival at t_eval for each patient
                        surv_times <- surv_fit$time
                        surv_probs <- surv_fit$surv
                        
                        prob_event <- numeric(n)
                        
                        # Optimization: find time index once
                        time_idx <- which(surv_times <= t_eval)
                        last_time_idx <- if(length(time_idx) > 0) max(time_idx) else 0
                        
                        if (last_time_idx == 0) {
                            prob_event <- rep(0, n)
                        } else {
                            if (is.matrix(surv_probs)) {
                                prob_event <- 1 - surv_probs[last_time_idx, ]
                            } else {
                                prob_event <- rep(1 - surv_probs[last_time_idx], n)
                            }
                        }
                        
                    }, error = function(e) {
                        warning("Cox model failed: ", e$message)
                        km_overall <- survfit(surv_obj ~ 1)
                        surv_t <- summary(km_overall, times = t_eval, extend = TRUE)$surv[1]
                        prob_event <<- rep(1 - surv_t, n)
                    })
                }
                
                prob_event <- pmin(pmax(prob_event, 0), 1)

                # --- Step B: Calculate Net Benefit using KM in High/Low Risk Groups ---
                
                # Overall Event Rate (for Treat All)
                # S(t) for whole population
                km_overall <- survfit(Surv(time, event) ~ 1)
                surv_overall <- summary(km_overall, times = t_eval, extend = TRUE)$surv[1]
                event_rate <- 1 - surv_overall
                
                nb_model <- numeric(length(thresholds))
                nb_treat_all <- numeric(length(thresholds))
                nb_treat_none <- numeric(length(thresholds)) # Always 0
                interventions_avoided <- numeric(length(thresholds))
                events_detected <- numeric(length(thresholds))

                for (i in seq_along(thresholds)) {
                    pt <- thresholds[i]
                    
                    # Define High Risk Group
                    is_high_risk <- prob_event >= pt
                    n_high_risk <- sum(is_high_risk)
                    
                    if (n_high_risk == 0) {
                        tp_rate <- 0
                        fp_rate <- 0
                    } else if (n_high_risk == n) {
                        tp_rate <- event_rate
                        fp_rate <- 1 - event_rate
                    } else {
                        # Calculate Survival in High Risk Group
                        # We use the subset of data for high risk patients
                        surv_high_risk <- survfit(Surv(time[is_high_risk], event[is_high_risk]) ~ 1)
                        s_high_risk <- summary(surv_high_risk, times = t_eval, extend = TRUE)$surv[1]
                        
                        # TP Rate = P(T<=t | High Risk) * P(High Risk)
                        #         = (1 - S_high_risk(t)) * (n_high_risk / n)
                        tp_rate <- (1 - s_high_risk) * (n_high_risk / n)
                        
                        # FP Rate = P(T>t | High Risk) * P(High Risk)
                        #         = S_high_risk(t) * (n_high_risk / n)
                        fp_rate <- s_high_risk * (n_high_risk / n)
                    }
                    
                    # Net Benefit
                    nb_model[i] <- tp_rate - fp_rate * (pt / (1 - pt))
                    
                    # Treat All Net Benefit
                    # TP_all = event_rate, FP_all = 1 - event_rate
                    nb_treat_all[i] <- event_rate - (1 - event_rate) * (pt / (1 - pt))
                    
                    # Interventions Avoided
                    # (N_all - N_treated) / N * 100
                    interventions_avoided[i] <- (n - n_high_risk) / n * 100
                    
                    # Events Detected (Sensitivity)
                    # TP_model / TP_all
                    if (event_rate > 0) {
                        events_detected[i] <- tp_rate / event_rate * 100
                    } else {
                        events_detected[i] <- 0
                    }
                }

                # Apply smoothing if requested
                if (self$options$smoothing) {
                    smooth_span <- 0.2
                    nb_model <- suppressWarnings(predict(loess(nb_model ~ thresholds, span = smooth_span)))
                    nb_treat_all <- suppressWarnings(predict(loess(nb_treat_all ~ thresholds, span = smooth_span)))
                }

                # Find optimal threshold
                max_idx <- which.max(nb_model)
                optimal_threshold <- thresholds[max_idx]
                max_net_benefit <- nb_model[max_idx]

                results_by_time[[as.character(t_eval)]] <- list(
                    time_point = t_eval,
                    n_at_risk = n, # Total N is the denominator for rates
                    n_events = round(event_rate * n), # Estimated events
                    event_rate = event_rate,
                    thresholds = thresholds,
                    nb_model = nb_model,
                    nb_treat_all = nb_treat_all,
                    nb_treat_none = nb_treat_none,
                    interventions_avoided = interventions_avoided,
                    events_detected = events_detected,
                    optimal_threshold = optimal_threshold,
                    max_net_benefit = max_net_benefit
                )
            }

            private$.results_by_time <- results_by_time
        },

        #---------------------------------------------
        # POPULATE NET BENEFIT TABLE
        #---------------------------------------------
        .populateNetBenefitTable = function() {

            if (is.null(private$.results_by_time)) return()

            table <- self$results$netBenefitTable
            ref_strategy <- self$options$reference_strategy

            # Show representative thresholds
            threshold_indices <- seq(1, length(private$.thresholds), length.out = min(20, length(private$.thresholds)))

            for (tp_name in names(private$.results_by_time)) {
                result <- private$.results_by_time[[tp_name]]

                for (idx in threshold_indices) {
                    idx <- round(idx)
                    improvement <- result$nb_model[idx] - max(result$nb_treat_all[idx], result$nb_treat_none[idx])

                    row <- list(
                        time_point = result$time_point,
                        threshold = result$thresholds[idx],
                        net_benefit = result$nb_model[idx],
                        nb_treat_all = result$nb_treat_all[idx],
                        nb_treat_none = result$nb_treat_none[idx],
                        improvement = improvement
                    )

                    row_key <- paste(tp_name, idx, sep = "_")
                    table$addRow(rowKey = row_key, values = row)
                }
            }
        },

        #---------------------------------------------
        # POPULATE SUMMARY TABLE
        #---------------------------------------------
        .populateSummaryTable = function() {

            if (is.null(private$.results_by_time)) return()

            table <- self$results$summaryTable

            for (tp_name in names(private$.results_by_time)) {
                result <- private$.results_by_time[[tp_name]]

                row <- list(
                    time_point = result$time_point,
                    n_at_risk = result$n_at_risk,
                    n_events = result$n_events,
                    event_rate = result$event_rate,
                    max_net_benefit = result$max_net_benefit,
                    optimal_threshold = result$optimal_threshold
                )

                table$addRow(rowKey = tp_name, values = row)
            }
        },

        #---------------------------------------------
        # POPULATE INTERVENTIONS TABLE
        #---------------------------------------------
        .populateInterventionsTable = function() {

            if (is.null(private$.results_by_time)) return()

            table <- self$results$interventionsTable

            # Show representative thresholds
            threshold_indices <- seq(1, length(private$.thresholds), length.out = min(10, length(private$.thresholds)))

            for (tp_name in names(private$.results_by_time)) {
                result <- private$.results_by_time[[tp_name]]

                for (idx in threshold_indices) {
                    idx <- round(idx)

                    row <- list(
                        time_point = result$time_point,
                        threshold = result$thresholds[idx],
                        interventions_avoided = result$interventions_avoided[idx],
                        events_detected = result$events_detected[idx]
                    )

                    row_key <- paste(tp_name, "int", idx, sep = "_")
                    table$addRow(rowKey = row_key, values = row)
                }
            }
        },

        #---------------------------------------------
        # PLOT NET BENEFIT CURVES
        #---------------------------------------------
        .netBenefitPlot = function(image, ...) {

            if (is.null(private$.results_by_time)) return()

            ref_strategy <- self$options$reference_strategy
            plot_by_tp <- self$options$plot_by_timepoint

            # Determine y-axis range
            all_nb <- unlist(lapply(private$.results_by_time, function(r) {
                c(r$nb_model, r$nb_treat_all, r$nb_treat_none)
            }))
            y_min <- min(all_nb, -0.05, na.rm = TRUE)
            y_max <- max(all_nb, 0.05, na.rm = TRUE)

            thresholds <- private$.thresholds

            if (!plot_by_tp) {
                # Overlay all time points
                plot(NULL, xlim = c(min(thresholds), max(thresholds)),
                     ylim = c(y_min, y_max),
                     xlab = "Threshold Probability",
                     ylab = "Net Benefit",
                     main = "Time-Dependent Decision Curves")

                abline(h = 0, lty = 2, col = "gray")
                grid()

                colors <- rainbow(length(private$.results_by_time))
                i <- 1

                for (tp_name in names(private$.results_by_time)) {
                    result <- private$.results_by_time[[tp_name]]

                    lines(result$thresholds, result$nb_model, col = colors[i], lwd = 2)

                    if (ref_strategy %in% c("treat_all", "both")) {
                        lines(result$thresholds, result$nb_treat_all,
                              col = colors[i], lty = 3, lwd = 1)
                    }

                    i <- i + 1
                }

                legend("topright",
                       legend = paste("t =", names(private$.results_by_time)),
                       col = colors, lwd = 2, cex = 0.8)
            }

            TRUE
        },

        #---------------------------------------------
        # PLOT INTERVENTIONS AVOIDED
        #---------------------------------------------
        .interventionsPlot = function(image, ...) {

            if (is.null(private$.results_by_time)) return()

            thresholds <- private$.thresholds

            plot(NULL, xlim = c(min(thresholds), max(thresholds)),
                 ylim = c(0, 100),
                 xlab = "Threshold Probability",
                 ylab = "Interventions Avoided (per 100 patients)",
                 main = "Interventions Avoided")

            grid()

            colors <- rainbow(length(private$.results_by_time))
            i <- 1

            for (tp_name in names(private$.results_by_time)) {
                result <- private$.results_by_time[[tp_name]]

                lines(result$thresholds, result$interventions_avoided,
                      col = colors[i], lwd = 2)

                i <- i + 1
            }

            legend("topright",
                   legend = paste("t =", names(private$.results_by_time)),
                   col = colors, lwd = 2, cex = 0.8)

            TRUE
        }
    )
)
