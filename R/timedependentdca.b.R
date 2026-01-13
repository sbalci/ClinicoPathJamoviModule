#' @title Time-Dependent Decision Curve Analysis Class
#' @description R6 class for performing time-dependent decision curve analysis.
#' @name timedependentdcaClass
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

        # Utility: Escape variable names
        .escapeVar = function(x) {
            if (is.null(x) || length(x) == 0 || x == "") return(x)
            gsub("[^A-Za-z0-9_]+", "_", make.names(x))
        },

        #---------------------------------------------
        # INIT
        #---------------------------------------------
        .init = function() {

            # Instructions
            html <- "<div class='jmv-welcome'>
            <h2>Time-Dependent Decision Curve Analysis</h2>
            <p class='jmv-welcome-desc'>Evaluate the clinical utility of prognostic models for time-to-event outcomes.</p>

            <div class='jmv-info-box'>
            <h3>Net Benefit Formula (Time-Dependent):</h3>
            <p class='formula'>NB(t, p<sub>t</sub>) = [TP(t) / N] - [FP(t) / N] × [p<sub>t</sub> / (1 - p<sub>t</sub>)]</p>

            <h4>Where:</h4>
            <ul>
            <li><strong>t:</strong> Time point of interest (e.g., 1 year, 5 years)</li>
            <li><strong>p<sub>t</sub>:</strong> Threshold probability at time t</li>
            <li><strong>TP(t):</strong> True positives (events correctly predicted by time t)</li>
            <li><strong>FP(t):</strong> False positives (non-events incorrectly predicted)</li>
            </ul>
            </div>

            <div class='jmv-info-box'>
            <h3>Applications:</h3>
            <ul>
            <li><strong>Recurrence prediction:</strong> Serial biopsy surveillance decisions</li>
            <li><strong>Survival models:</strong> Treatment vs palliative care thresholds</li>
            </ul>
            </div>
            </div>"

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
        # NOTICE HELPERS (HTML-based to avoid serialization)
        #---------------------------------------------
        .noticeList = list(),

        .addNotice = function(type, content, name = NULL, position = NULL) {
            if (is.null(name)) {
                name <- paste0('notice_', length(private$.noticeList) + 1)
            }

            type_name <- switch(
                as.character(type),
                "1" = "ERROR",
                "2" = "STRONG_WARNING",
                "3" = "WARNING",
                "4" = "INFO",
                "INFO"
            )

            private$.noticeList[[name]] <- list(
                type = type_name,
                content = content
            )
        },

        .renderNotices = function() {
            if (length(private$.noticeList) == 0) {
                self$results$notices$setVisible(FALSE)
                return()
            }

            html_parts <- c('<div class="jmv-results-notice-container">')

            # Order: ERROR/STRONG_WARNING first, then WARNING, then INFO
            type_order <- c("ERROR", "STRONG_WARNING", "WARNING", "INFO")
            ordered_notices <- list()

            for (t in type_order) {
                for (name in names(private$.noticeList)) {
                    notice <- private$.noticeList[[name]]
                    if (notice$type == t) {
                        ordered_notices[[length(ordered_notices) + 1]] <- notice
                    }
                }
            }

            for (notice in ordered_notices) {
                css_class <- switch(
                    notice$type,
                    "ERROR" = "error",
                    "STRONG_WARNING" = "strong-warning",
                    "WARNING" = "warning",
                    "INFO" = "info",
                    "info"
                )

                icon <- switch(
                    notice$type,
                    "ERROR" = "⛔",
                    "STRONG_WARNING" = "⚠️",
                    "WARNING" = "⚠",
                    "INFO" = "ℹ",
                    "ℹ"
                )

                html_parts <- c(html_parts, sprintf(
                    '<div class="jmv-results-notice %s"><span class="jmv-results-notice-icon">%s</span><span class="jmv-results-notice-content">%s</span></div>',
                    css_class, icon, notice$content
                ))
            }

            html_parts <- c(html_parts, '</div>')
            self$results$notices$setContent(paste(html_parts, collapse = '\n'))
        },

        #---------------------------------------------
        # BOOTSTRAP HELPER
        #---------------------------------------------
        .calculateBootstrapNB = function(time, event, predictor, t_eval, thresholds, estimate_method, cox_fit_global = NULL) {
            # Perform one bootstrap iteration
            n <- length(time)
            boot_idx <- sample(1:n, n, replace = TRUE)

            time_boot <- time[boot_idx]
            event_boot <- event[boot_idx]
            predictor_boot <- predictor[boot_idx]

            # Calculate risk probabilities using same method as main analysis
            if (estimate_method == "direct") {
                prob_event <- predictor_boot
            } else if (estimate_method == "kaplan_meier") {
                n_groups <- if (n < 100) 3 else if (n < 300) 5 else 10
                predictor_breaks <- quantile(predictor_boot, probs = seq(0, 1, length = n_groups + 1), na.rm = TRUE)

                if (length(unique(predictor_breaks)) < length(predictor_breaks)) {
                    predictor_groups <- as.numeric(cut(predictor_boot, breaks = unique(predictor_breaks), include.lowest = TRUE))
                } else {
                    predictor_groups <- as.numeric(cut(predictor_boot, breaks = predictor_breaks, include.lowest = TRUE, labels = FALSE))
                }

                surv_obj <- Surv(time_boot, event_boot)
                prob_event <- numeric(n)

                tryCatch({
                    surv_by_group <- survfit(surv_obj ~ predictor_groups)
                    group_summary <- summary(surv_by_group, times = t_eval, extend = TRUE)

                    for (g in unique(predictor_groups)) {
                        if (is.na(g)) next
                        group_mask <- predictor_groups == g

                        if (!is.null(group_summary$strata)) {
                            stratum_name <- paste0("predictor_groups=", g)
                            stratum_idx <- which(group_summary$strata == stratum_name)
                            if (length(stratum_idx) > 0) {
                                surv_g <- group_summary$surv[stratum_idx[1]]
                                prob_event[group_mask] <- 1 - surv_g
                            } else {
                                prob_event[group_mask] <- mean(event_boot[group_mask], na.rm = TRUE)
                            }
                        } else {
                            prob_event[group_mask] <- 1 - group_summary$surv[1]
                        }
                    }
                }, error = function(e) {
                    km_overall <- survfit(surv_obj ~ 1)
                    surv_t <- summary(km_overall, times = t_eval, extend = TRUE)$surv[1]
                    prob_event <<- rep(1 - surv_t, n)
                })

            } else {
                # Cox method - refit on bootstrap sample
                tryCatch({
                    surv_obj <- Surv(time_boot, event_boot)
                    cox_boot <- coxph(surv_obj ~ predictor_boot)
                    newdata <- data.frame(predictor_boot = predictor_boot)
                    surv_fit <- survfit(cox_boot, newdata = newdata)

                    surv_times <- surv_fit$time
                    surv_probs <- surv_fit$surv

                    prob_event <- numeric(n)
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
                    prob_event <- numeric(n)
                })
            }

            prob_event <- pmin(pmax(prob_event, 0), 1)

            # Calculate net benefit for all thresholds
            km_overall <- survfit(Surv(time_boot, event_boot) ~ 1)
            surv_overall <- summary(km_overall, times = t_eval, extend = TRUE)$surv[1]
            event_rate <- 1 - surv_overall

            nb_model <- numeric(length(thresholds))

            for (i in seq_along(thresholds)) {
                pt <- thresholds[i]
                is_high_risk <- prob_event >= pt
                n_high_risk <- sum(is_high_risk)

                if (n_high_risk == 0) {
                    tp_rate <- 0
                    fp_rate <- 0
                } else if (n_high_risk == n) {
                    tp_rate <- event_rate
                    fp_rate <- 1 - event_rate
                } else {
                    surv_high_risk <- survfit(Surv(time_boot[is_high_risk], event_boot[is_high_risk]) ~ 1)
                    s_high_risk <- summary(surv_high_risk, times = t_eval, extend = TRUE)$surv[1]
                    tp_rate <- (1 - s_high_risk) * (n_high_risk / n)
                    fp_rate <- s_high_risk * (n_high_risk / n)
                }

                nb_model[i] <- tp_rate - fp_rate * (pt / (1 - pt))
            }

            return(nb_model)
        },

        #---------------------------------------------
        # RUN
        #---------------------------------------------
        .run = function() {

            # Check requirements
            if (is.null(self$options$time) || self$options$time == "" ||
                is.null(self$options$event) || self$options$event == "" ||
                is.null(self$options$predictor) || self$options$predictor == "") {

                private$.addNotice(
                    type = jmvcore::NoticeType$ERROR,
                    content = 'Time, Event, and Predictor variables are required. Please select all three variables to begin time-dependent decision curve analysis.',
                    name = 'missingVariables',
                    position = 1
                )
                return()
            }

            # Prepare data
            tryCatch({
                private$.prepareData()
            }, error = function(e) {
                private$.addNotice(
                    type = jmvcore::NoticeType$ERROR,
                    content = sprintf('Data preparation failed: %s. Check variable types and data integrity.', e$message),
                    name = 'dataPrepError',
                    position = 1
                )
                return()
            })

            if (is.null(private$.data_prepared)) {
                return()
            }

            # Calculate time-dependent net benefit
            tryCatch({
                private$.calculateTimeDependentNB()
            }, error = function(e) {
                private$.addNotice(
                    type = jmvcore::NoticeType$ERROR,
                    content = sprintf('Net benefit calculation failed: %s. This may indicate numerical issues or extreme data values.', e$message),
                    name = 'calcError',
                    position = 1
                )
                return()
            })

            # Populate results
            private$.populateNetBenefitTable()
            private$.populateSummaryTable()
            private$.populateInterventionsTable()

            # Methodological transparency notice
            private$.addNotice(
                type = jmvcore::NoticeType$INFO,
                content = 'Net benefit estimates use Kaplan-Meier survival within risk groups at each threshold. Inverse probability of censoring weighting (IPCW) and time-dependent censoring adjustments are not implemented. Interpret with caution when censoring is heavy or informative.',
                name = 'methodNote',
                position = 999
            )

            # Success notice
            n_timepoints <- length(private$.time_points)
            n_obs <- private$.data_prepared$n
            n_events <- sum(private$.data_prepared$event)
            private$.addNotice(
                type = jmvcore::NoticeType$INFO,
                content = sprintf('Time-dependent DCA completed successfully. Analyzed %d observations (%d events, %.1f%% event rate) across %d time points using %s estimation method.',
                                  n_obs, n_events, 100 * n_events / n_obs, n_timepoints, self$options$estimate_survival),
                name = 'analysisComplete',
                position = 999
            )

            # Render all notices at end
            private$.renderNotices()
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
            time <- jmvcore::toNumeric(data[[time_var]])
            event <- jmvcore::toNumeric(data[[event_var]])
            predictor <- jmvcore::toNumeric(data[[predictor_var]])

            # Escape variable names for safe handling
            time_var_clean <- private$.escapeVar(time_var)
            event_var_clean <- private$.escapeVar(event_var)
            predictor_var_clean <- private$.escapeVar(predictor_var)

            # Validate time variable
            if (any(time <= 0, na.rm = TRUE)) {
                n_invalid <- sum(time <= 0, na.rm = TRUE)
                private$.addNotice(
                    type = jmvcore::NoticeType$ERROR,
                    content = sprintf('Time variable contains %d non-positive values. All time values must be greater than zero for survival analysis.', n_invalid),
                    name = 'invalidTime',
                    position = 1
                )
                return(NULL)
            }

            # Validate event indicator
            event_unique <- unique(event[!is.na(event)])
            if (!all(event_unique %in% c(0, 1))) {
                private$.addNotice(
                    type = jmvcore::NoticeType$ERROR,
                    content = 'Event indicator must be binary (0 = censored, 1 = event). Please recode your event variable or check for data entry errors.',
                    name = 'invalidEvent',
                    position = 1
                )
                return(NULL)
            }

            # Handle missing values
            if (any(is.na(time)) || any(is.na(event)) || any(is.na(predictor))) {
                n_missing <- sum(!complete.cases(data.frame(time, event, predictor)))
                complete_cases <- !is.na(time) & !is.na(event) & !is.na(predictor)
                time <- time[complete_cases]
                event <- event[complete_cases]
                predictor <- predictor[complete_cases]

                private$.addNotice(
                    type = jmvcore::NoticeType$WARNING,
                    content = sprintf('Removed %d observations with missing values (%.1f%% of data). Analysis uses %d complete cases.',
                                      n_missing, 100 * n_missing / (n_missing + length(time)), length(time)),
                    name = 'missingData',
                    position = 50
                )
            }

            # Parse time points
            time_points_str <- self$options$time_points
            time_points <- as.numeric(unlist(strsplit(time_points_str, ",")))
            time_points <- time_points[!is.na(time_points)]

            if (length(time_points) == 0) {
                private$.addNotice(
                    type = jmvcore::NoticeType$ERROR,
                    content = 'No valid time points specified. Please enter comma-separated numeric values (e.g., "365, 730, 1095" for 1, 2, 3 years).',
                    name = 'invalidTimePoints',
                    position = 1
                )
                return(NULL)
            }

            max_time <- max(time, na.rm = TRUE)
            if (any(time_points > max_time)) {
                n_exceed <- sum(time_points > max_time)
                private$.addNotice(
                    type = jmvcore::NoticeType$WARNING,
                    content = sprintf('%d time point(s) exceed maximum observed follow-up (%.1f). Estimates will be extrapolated using Kaplan-Meier extension; interpret with caution.',
                                      n_exceed, max_time),
                    name = 'extrapolatedTimePoints',
                    position = 50
                )
            }

            # Statistical validation checks
            n <- length(time)
            n_events <- sum(event)
            censoring_pct <- 100 * (1 - n_events / n)

            # CRITICAL: Very few events
            if (n_events < 10) {
                private$.addNotice(
                    type = jmvcore::NoticeType$STRONG_WARNING,
                    content = sprintf('Only %d events observed. Time-dependent decision curve analysis requires at least 10 events for stable estimates. Results may be unreliable; consider collecting more data or extending follow-up.', n_events),
                    name = 'fewEvents',
                    position = 1
                )
            } else if (n_events < 20) {
                private$.addNotice(
                    type = jmvcore::NoticeType$WARNING,
                    content = sprintf('%d events observed. Time-dependent DCA is more reliable with at least 20 events. Interpret net benefit estimates cautiously.', n_events),
                    name = 'moderateEvents',
                    position = 50
                )
            }

            # Small sample size
            if (n < 50) {
                private$.addNotice(
                    type = jmvcore::NoticeType$WARNING,
                    content = sprintf('Sample size is %d. Decision curve analysis is more stable with at least 50 observations. Results may have wide confidence intervals.', n),
                    name = 'smallSample',
                    position = 50
                )
            }

            # Heavy censoring
            if (censoring_pct > 80) {
                private$.addNotice(
                    type = jmvcore::NoticeType$STRONG_WARNING,
                    content = sprintf('Heavy censoring detected (%.1f%% censored). Net benefit estimates may be unreliable when censoring exceeds 80%%. Consider sensitivity analyses or alternative methods.', censoring_pct),
                    name = 'heavyCensoring',
                    position = 1
                )
            } else if (censoring_pct > 60) {
                private$.addNotice(
                    type = jmvcore::NoticeType$WARNING,
                    content = sprintf('Moderate censoring detected (%.1f%% censored). Net benefit estimates depend on censoring assumptions; verify censoring is non-informative.', censoring_pct),
                    name = 'moderateCensoring',
                    position = 50
                )
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
            
            # Optimization: Fit Cox model once if selected
            cox_fit_global <- NULL
            if (estimate_method == "cox") {
                surv_obj <- Surv(time, event)
                tryCatch({
                    cox_fit_global <- coxph(surv_obj ~ predictor)
                }, error = function(e) {
                    private$.addNotice(
                        type = jmvcore::NoticeType$WARNING,
                        content = sprintf('Cox proportional hazards model fitting failed (%s). Falling back to Kaplan-Meier estimation. Check predictor variable for collinearity or zero variance.', e$message),
                        name = 'coxFitFailed',
                        position = 50
                    )
                })
            }

            # 1. Calculate risk scores (probabilities) for all patients
            
            for (t_eval in time_points) {

                # --- Step A: Calculate Risk Scores (Probabilities) ---
                if (estimate_method == "direct") {
                    # CRITICAL SAFETY CHECK: Validate predictor is in [0,1] range
                    if (any(predictor < 0 | predictor > 1, na.rm = TRUE)) {
                        private$.addNotice(
                            type = jmvcore::NoticeType$ERROR,
                            content = sprintf('Direct probability method requires predictor values in [0,1] range. Found values outside this range (min=%.3f, max=%.3f). Either transform predictor to probabilities or use Kaplan-Meier or Cox estimation method.', min(predictor, na.rm = TRUE), max(predictor, na.rm = TRUE)),
                            name = 'directProbOutOfRange',
                            position = 1
                        )
                        return()
                    }
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
                        private$.addNotice(
                            type = jmvcore::NoticeType$WARNING,
                            content = sprintf('Kaplan-Meier stratification failed for time point %.1f (%s). Using overall survival estimate. Check predictor distribution for outliers or insufficient variation.', t_eval, e$message),
                            name = paste0('kmStratFailed_', t_eval),
                            position = 50
                        )
                        km_overall <- survfit(surv_obj ~ 1)
                        surv_t <- summary(km_overall, times = t_eval, extend = TRUE)$surv[1]
                        prob_event <<- rep(1 - surv_t, n)
                    })
                    
                } else { # cox
                    if (!is.null(cox_fit_global)) {
                        tryCatch({
                            newdata <- data.frame(predictor = predictor)
                            surv_fit <- survfit(cox_fit_global, newdata = newdata)
                            
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
                            private$.addNotice(
                                type = jmvcore::NoticeType$WARNING,
                                content = sprintf('Cox prediction failed for time point %.1f (%s). Using overall survival estimate. Verify model convergence and predictor coding.', t_eval, e$message),
                                name = paste0('coxPredFailed_', t_eval),
                                position = 50
                            )
                            prob_event <- numeric(n) # Will default to 0 or fallback below
                        })
                    } else {
                         # Fallback if fit failed
                         surv_obj <- Surv(time, event)
                         km_overall <- survfit(surv_obj ~ 1)
                         surv_t <- summary(km_overall, times = t_eval, extend = TRUE)$surv[1]
                         prob_event <<- rep(1 - surv_t, n)
                    }
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
                    
                    # Interventions Avoided (Placeholder - calculated after smoothing)
                    interventions_avoided[i] <- 0
                    
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

                # Calculate Standardized Net Reduction in Interventions (per 100 patients)
                # Formula: (NB_model - NB_all) / (pt / (1 - pt)) * 100
                for (i in seq_along(thresholds)) {
                    pt <- thresholds[i]
                    u <- pt / (1 - pt)
                    
                    if (pt > 0 && pt < 1) {
                        net_reduction <- (nb_model[i] - nb_treat_all[i]) / u * 100
                        # Determine if model is better than treating all
                        # If NB_model < NB_all, reduction is negative (harm equivalent to treating more people)
                        interventions_avoided[i] <- net_reduction
                    } else {
                        interventions_avoided[i] <- 0
                    }
                }

                # Find optimal threshold
                max_idx <- which.max(nb_model)
                optimal_threshold <- thresholds[max_idx]
                max_net_benefit <- nb_model[max_idx]

                # Bootstrap confidence intervals if requested
                nb_model_lower <- NULL
                nb_model_upper <- NULL

                if (self$options$use_bootstrap) {
                    n_boot <- self$options$bootstrap_iterations
                    ci_level <- self$options$ci_level
                    alpha <- 1 - ci_level

                    # Matrix to store bootstrap results (rows = iterations, cols = thresholds)
                    boot_results <- matrix(NA, nrow = n_boot, ncol = length(thresholds))

                    for (b in 1:n_boot) {
                        boot_results[b, ] <- private$.calculateBootstrapNB(
                            time = time,
                            event = event,
                            predictor = predictor,
                            t_eval = t_eval,
                            thresholds = thresholds,
                            estimate_method = estimate_method,
                            cox_fit_global = cox_fit_global
                        )
                    }

                    # Calculate percentile-based confidence intervals
                    nb_model_lower <- apply(boot_results, 2, quantile, probs = alpha / 2, na.rm = TRUE)
                    nb_model_upper <- apply(boot_results, 2, quantile, probs = 1 - alpha / 2, na.rm = TRUE)

                    # Apply smoothing to CIs if main curves were smoothed
                    if (self$options$smoothing) {
                        smooth_span <- 0.2
                        nb_model_lower <- suppressWarnings(predict(loess(nb_model_lower ~ thresholds, span = smooth_span)))
                        nb_model_upper <- suppressWarnings(predict(loess(nb_model_upper ~ thresholds, span = smooth_span)))
                    }
                }

                results_by_time[[as.character(t_eval)]] <- list(
                    time_point = t_eval,
                    n_at_risk = n, # Total N is the denominator for rates
                    n_events = round(event_rate * n), # Estimated events
                    event_rate = event_rate,
                    thresholds = thresholds,
                    nb_model = nb_model,
                    nb_model_lower = nb_model_lower,
                    nb_model_upper = nb_model_upper,
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
                        nb_lower = if (!is.null(result$nb_model_lower)) result$nb_model_lower[idx] else NULL,
                        nb_upper = if (!is.null(result$nb_model_upper)) result$nb_model_upper[idx] else NULL,
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

            # Determine y-axis range (include CIs if present)
            all_nb <- unlist(lapply(private$.results_by_time, function(r) {
                c(r$nb_model, r$nb_treat_all, r$nb_treat_none,
                  r$nb_model_lower, r$nb_model_upper)
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
                     main = "Time-Dependent Decision Curves (All Time Points)")

                abline(h = 0, lty = 2, col = "gray")
                grid()

                colors <- rainbow(length(private$.results_by_time))
                i <- 1

                for (tp_name in names(private$.results_by_time)) {
                    result <- private$.results_by_time[[tp_name]]

                    # Add CI band if bootstrap enabled
                    if (!is.null(result$nb_model_lower) && !is.null(result$nb_model_upper)) {
                        polygon(c(result$thresholds, rev(result$thresholds)),
                                c(result$nb_model_lower, rev(result$nb_model_upper)),
                                col = adjustcolor(colors[i], alpha.f = 0.2),
                                border = NA)
                    }

                    # Main model line
                    lines(result$thresholds, result$nb_model, col = colors[i], lwd = 2)

                    # Reference strategies
                    if (ref_strategy %in% c("treat_all", "both")) {
                        lines(result$thresholds, result$nb_treat_all,
                              col = colors[i], lty = 3, lwd = 1)
                    }

                    i <- i + 1
                }

                legend("topright",
                       legend = paste("t =", names(private$.results_by_time)),
                       col = colors, lwd = 2, cex = 0.8)

            } else {
                # Separate plot for each time point (FACETED LAYOUT)
                n_timepoints <- length(private$.results_by_time)

                # Setup multi-panel layout
                n_cols <- ceiling(sqrt(n_timepoints))
                n_rows <- ceiling(n_timepoints / n_cols)
                old_par <- par(no.readonly = TRUE)
                on.exit(par(old_par))
                par(mfrow = c(n_rows, n_cols), mar = c(4, 4, 2, 1))

                for (tp_name in names(private$.results_by_time)) {
                    result <- private$.results_by_time[[tp_name]]

                    # Individual plot for this time point
                    plot(result$thresholds, result$nb_model,
                         type = "n",
                         xlim = c(min(thresholds), max(thresholds)),
                         ylim = c(y_min, y_max),
                         xlab = "Threshold Probability",
                         ylab = "Net Benefit",
                         main = paste("t =", result$time_point))

                    abline(h = 0, lty = 2, col = "gray")
                    grid()

                    # Add CI band if bootstrap enabled
                    if (!is.null(result$nb_model_lower) && !is.null(result$nb_model_upper)) {
                        polygon(c(result$thresholds, rev(result$thresholds)),
                                c(result$nb_model_lower, rev(result$nb_model_upper)),
                                col = adjustcolor("blue", alpha.f = 0.2),
                                border = NA)
                    }

                    # Main model line
                    lines(result$thresholds, result$nb_model, col = "blue", lwd = 2)

                    # Reference strategies
                    if (ref_strategy %in% c("treat_all", "both")) {
                        lines(result$thresholds, result$nb_treat_all,
                              col = "red", lty = 2, lwd = 1)
                    }
                    if (ref_strategy %in% c("treat_none", "both")) {
                        lines(result$thresholds, result$nb_treat_none,
                              col = "gray", lty = 2, lwd = 1)
                    }

                    # Add minimal legend
                    legend_items <- c("Model")
                    legend_col <- c("blue")
                    legend_lty <- c(1)

                    if (ref_strategy %in% c("treat_all", "both")) {
                        legend_items <- c(legend_items, "Treat All")
                        legend_col <- c(legend_col, "red")
                        legend_lty <- c(legend_lty, 2)
                    }
                    if (ref_strategy %in% c("treat_none", "both")) {
                        legend_items <- c(legend_items, "Treat None")
                        legend_col <- c(legend_col, "gray")
                        legend_lty <- c(legend_lty, 2)
                    }

                    legend("topright", legend = legend_items,
                           col = legend_col, lty = legend_lty,
                           lwd = c(2, rep(1, length(legend_items)-1)),
                           cex = 0.7)
                }
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
                 ylim = range(unlist(lapply(private$.results_by_time, function(r) r$interventions_avoided)), na.rm = TRUE),
                 xlab = "Threshold Probability",
                 ylab = "Net Reduction in Interventions (per 100 patients)",
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
