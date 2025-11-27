
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

            for (t_eval in time_points) {

                # Estimate event probabilities at time t
                if (estimate_method == "direct") {
                    # Predictor is already a probability
                    prob_event <- predictor
                    
                } else if (estimate_method == "kaplan_meier") {
                    # Use KM with stratification by predictor quantiles
                    # This provides proper non-parametric estimation
                    
                    # Determine number of strata based on sample size
                    n_groups <- if (n < 100) 3 else if (n < 300) 5 else 10
                    
                    # Create predictor groups
                    predictor_breaks <- quantile(predictor, 
                                                 probs = seq(0, 1, length = n_groups + 1),
                                                 na.rm = TRUE)
                    predictor_groups <- cut(predictor, 
                                           breaks = predictor_breaks,
                                           include.lowest = TRUE,
                                           labels = FALSE)
                    
                    # Fit KM by group
                    surv_obj <- Surv(time, event)
                    
                    tryCatch({
                        surv_by_group <- survfit(surv_obj ~ predictor_groups)
                        
                        # Get survival at time t for each group
                        group_summary <- summary(surv_by_group, times = t_eval, extend = TRUE)
                        
                        # Initialize probability vector
                        prob_event <- numeric(n)
                        
                        # Map survival probabilities back to individuals
                        for (g in 1:n_groups) {
                            group_mask <- predictor_groups == g
                            if (sum(group_mask, na.rm = TRUE) > 0) {
                                # Find survival for this group
                                if (length(group_summary$strata) > 0) {
                                    stratum_name <- paste0("predictor_groups=", g)
                                    stratum_idx <- which(group_summary$strata == stratum_name)
                                    
                                    if (length(stratum_idx) > 0) {
                                        surv_g <- group_summary$surv[stratum_idx[1]]
                                        prob_event[group_mask] <- 1 - surv_g
                                    } else {
                                        # Fallback to overall event rate for this group
                                        prob_event[group_mask] <- mean(event[group_mask], na.rm = TRUE)
                                    }
                                } else {
                                    # Single group or no strata
                                    prob_event[group_mask] <- 1 - group_summary$surv[1]
                                }
                            }
                        }
                        
                    }, error = function(e) {
                        # Fallback: use overall Kaplan-Meier if stratification fails
                        warning("KM stratification failed, using overall estimate: ", e$message)
                        km_overall <- survfit(surv_obj ~ 1)
                        surv_t <- summary(km_overall, times = t_eval, extend = TRUE)$surv[1]
                        prob_event <<- rep(1 - surv_t, n)
                    })
                    
                    # Ensure valid probabilities
                    prob_event <- pmin(pmax(prob_event, 0), 1)

                } else {  # cox
                    # Fit Cox proportional hazards model
                    surv_obj <- Surv(time, event)
                    
                    tryCatch({
                        cox_fit <- coxph(surv_obj ~ predictor)
                        
                        # Create newdata for predictions
                        newdata <- data.frame(predictor = predictor)
                        
                        # Get survival predictions at time t using survfit
                        surv_fit <- survfit(cox_fit, newdata = newdata)
                        
                        # Extract survival probabilities at time t_eval
                        # survfit returns a list of survival curves (one per individual)
                        prob_event <- numeric(n)
                        
                        for (i in 1:n) {
                            # Get survival curve for individual i
                            surv_times <- surv_fit$time
                            surv_probs <- surv_fit$surv
                            
                            # Handle matrix vs vector format
                            if (is.matrix(surv_probs)) {
                                surv_i <- surv_probs[, i]
                            } else {
                                surv_i <- surv_probs
                            }
                            
                            # Find survival at t_eval (use last observed if t_eval > max time)
                            if (length(surv_times) == 0) {
                                prob_event[i] <- 0  # No events observed
                            } else {
                                time_idx <- which(surv_times <= t_eval)
                                if (length(time_idx) > 0) {
                                    surv_at_t <- surv_i[max(time_idx)]
                                } else {
                                    surv_at_t <- 1  # Before first event time
                                }
                                prob_event[i] <- 1 - surv_at_t
                            }
                        }
                        
                    }, error = function(e) {
                        warning("Cox model failed: ", e$message, ". Using KM fallback.")
                        # Fallback to overall KM
                        km_overall <- survfit(surv_obj ~ 1)
                        surv_t <- summary(km_overall, times = t_eval, extend = TRUE)$surv[1]
                        prob_event <<- rep(1 - surv_t, n)
                    })
                    
                    # Ensure valid probabilities
                    prob_event <- pmin(pmax(prob_event, 0), 1)
                }

                # Determine who has event by time t
                has_event_by_t <- (time <= t_eval & event == 1)
                
                # CORRECTED: Define at-risk population properly
                # Include only those who:
                # 1. Were censored after t_eval (time > t_eval & event == 0), OR
                # 2. Had event at or after t_eval (time >= t_eval & event == 1)
                at_risk_at_t <- (time > t_eval) | (time >= t_eval & event == 1)


                n_at_risk <- sum(at_risk_at_t)
                n_events <- sum(has_event_by_t)

                # Calculate net benefit at each threshold
                nb_model <- numeric(length(thresholds))
                nb_treat_all <- numeric(length(thresholds))
                nb_treat_none <- numeric(length(thresholds))
                interventions_avoided <- numeric(length(thresholds))
                events_detected <- numeric(length(thresholds))

                for (i in seq_along(thresholds)) {
                    pt <- thresholds[i]

                    # Model strategy: treat if prob > threshold
                    treat <- prob_event >= pt

                    tp <- sum(treat & has_event_by_t & at_risk_at_t)
                    fp <- sum(treat & !has_event_by_t & at_risk_at_t)
                    tn <- sum(!treat & !has_event_by_t & at_risk_at_t)
                    fn <- sum(!treat & has_event_by_t & at_risk_at_t)

                    # Net benefit for model
                    if (n_at_risk > 0) {
                        nb_model[i] <- (tp / n_at_risk) - (fp / n_at_risk) * (pt / (1 - pt))
                    } else {
                        nb_model[i] <- 0
                    }

                    # Treat all
                    nb_treat_all[i] <- (n_events / n_at_risk) - ((n_at_risk - n_events) / n_at_risk) * (pt / (1 - pt))

                    # Treat none
                    nb_treat_none[i] <- 0

                    # Interventions avoided
                    n_treated_all <- n_at_risk
                    n_treated_model <- sum(treat)
                    interventions_avoided[i] <- (n_treated_all - n_treated_model) / n_at_risk * 100

                    # Events detected
                    events_detected[i] <- if (n_events > 0) tp / n_events * 100 else 0
                }

                # Apply smoothing if requested
                if (self$options$smoothing) {
                    smooth_span <- 0.2
                    nb_model <- suppressWarnings(predict(loess(nb_model ~ thresholds, span = smooth_span)))
                    nb_treat_all <- suppressWarnings(predict(loess(nb_treat_all ~ thresholds, span = smooth_span)))
                }

                # Find optimal threshold (maximum net benefit)
                max_idx <- which.max(nb_model)
                optimal_threshold <- thresholds[max_idx]
                max_net_benefit <- nb_model[max_idx]

                results_by_time[[as.character(t_eval)]] <- list(
                    time_point = t_eval,
                    n_at_risk = n_at_risk,
                    n_events = n_events,
                    event_rate = n_events / n_at_risk,
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
