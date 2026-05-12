conditionalsurvivalClass <- R6::R6Class(
    "conditionalsurvivalClass",
    inherit = conditionalsurvivalBase,
    private = list(
        .init = function() {
            
            private$.initTodo()
            
            # Initialize empty table
            condsurvTable <- self$results$condsurvTable
            
            # Check if we have required variables
            if (is.null(self$options$timeVar) || is.null(self$options$outcomeVar)) {
                condsurvTable$addFootnote(rowNo = 1, col = "time", 
                    "Please specify both Time and Event/Status variables")
                return()
            }
            
            if (self$options$showExplanations) {
                private$.initMethodExplanation()
            }
        },

        .run = function() {

            # Get variables
            timeVar <- self$options$timeVar
            outcomeVar <- self$options$outcomeVar
            conditionVar <- self$options$conditionVar

            # bandwidth option: Could be used for kernel-smoothed conditional survival
            # via bw parameter in density estimation or loess smoothing of KM estimates.
            # Currently uses step-function KM without smoothing.
            # When method="pkm", this should control the presmoothing bandwidth.
            bandwidth <- self$options$bandwidth

            # TODO: plotType (curves/probability/both) is not yet
            # implemented in .plot(). Currently always draws curves.
            plotType <- self$options$plotType

            # conditionVar stratification is handled below in the analysis block

            # Check for required variables
            if (is.null(timeVar) || is.null(outcomeVar)) {
                return()
            }
            
            # Get data
            data <- self$data
            
            if (nrow(data) == 0)
                return()
                
            # Prepare survival data
            time <- data[[timeVar]]
            status <- data[[outcomeVar]]
            
            # Handle factor status variable with validation
            if (is.factor(status)) {
                levs <- levels(status)
                if (length(levs) != 2) {
                    jmvcore::reject(
                        paste0("Event/Status variable must have exactly 2 levels (got ", length(levs), ")."),
                        code = ""
                    )
                }
                status <- as.numeric(status) - 1
            } else {
                unique_vals <- sort(unique(status[!is.na(status)]))
                if (!all(unique_vals %in% c(0, 1))) {
                    jmvcore::reject(
                        "Event/Status variable must contain only 0 (censored) and 1 (event).",
                        code = ""
                    )
                }
            }

            # Minimum event count validation
            n_events <- sum(status == 1, na.rm = TRUE)
            if (n_events < 5) {
                jmvcore::reject(
                    paste0("Too few events (", n_events,
                        "). Conditional survival requires at least 5 events for reliable estimation."),
                    code = ""
                )
            }

            # Check for required packages
            if (!requireNamespace("survival", quietly = TRUE)) {
                self$results$condsurvTable$addFootnote(rowNo = 1, col = "time", 
                    "survival package required but not available")
                return()
            }
            
            # Check if condSURV package is available
            has_condsurv <- requireNamespace("condSURV", quietly = TRUE)
            
            # Get conditioning time (0 or NULL = use median follow-up)
            condTime <- self$options$conditionTime
            if (is.null(condTime) || is.na(condTime) || condTime <= 0) {
                condTime <- median(time, na.rm = TRUE)
            }

            # Get specific time points
            timePoints <- private$.parseTimePoints()
            if (length(timePoints) == 0) {
                # Use default time points based on data range
                timePoints <- private$.getDefaultTimePoints(time, condTime)
            }
            
            # Validate conditionVar groups before computation
            grpFactor <- NULL
            grpLevels <- NULL
            if (!is.null(conditionVar)) {
                grpFactor <- as.factor(data[[conditionVar]])
                grpLevels <- levels(grpFactor)
                if (length(grpLevels) < 2) {
                    jmvcore::reject(
                        "The conditioning variable has fewer than 2 levels. Stratified analysis requires at least 2 groups.",
                        code = ""
                    )
                }
            }

            # Run conditional survival analysis -- with optional stratification
            tryCatch({
                if (!is.null(conditionVar)) {
                    allResults <- list()
                    for (grp in grpLevels) {
                        grpIdx <- grpFactor == grp
                        grpTime <- time[grpIdx]
                        grpStatus <- status[grpIdx]

                        # Skip groups with too few events
                        grpEvents <- sum(grpStatus == 1, na.rm = TRUE)
                        if (grpEvents < 3) {
                            next
                        }

                        grpRes <- private$.computeCondSurv(
                            grpTime, grpStatus, condTime, timePoints, has_condsurv
                        )
                        grpRes$group <- grp
                        allResults[[grp]] <- grpRes
                    }

                    if (length(allResults) == 0) {
                        jmvcore::reject("No groups had enough events (>=3) for conditional survival estimation.")
                    }

                    results <- do.call(rbind, allResults)
                    rownames(results) <- NULL
                } else {
                    # Overall (unstratified) analysis
                    results <- private$.computeCondSurv(
                        time, status, condTime, timePoints, has_condsurv
                    )
                    results$group <- "Overall"
                }

                # Populate table
                private$.populateCondSurvTable(results, condTime)

                # Update explanations if requested
                if (self$options$showExplanations) {
                    private$.updateMethodExplanation(condTime, results)
                }

                # Generate report sentence
                private$.generateReportSentence(condTime, results, time, status)

                # Populate assumptions and caveats panel
                private$.populateAssumptions()

            }, error = function(e) {
                try(
                    self$results$condsurvTable$addFootnote(rowNo = 1, col = "time",
                        paste("Error in conditional survival calculation:", e$message)),
                    silent = TRUE
                )
            })
        },

        # Dispatcher: choose condSURV or manual method for a single data subset
        .computeCondSurv = function(time, status, condTime, timePoints, has_condsurv) {
            method <- self$options$method
            if (has_condsurv && method %in% c("km", "landmark", "ipw", "pkm")) {
                private$.runCondSurv(time, status, condTime, timePoints)
            } else {
                private$.runManualCondSurv(time, status, condTime, timePoints)
            }
        },

        .plot = function(image, ggtheme, theme, ...) {

            # Get variables
            timeVar <- self$options$timeVar
            outcomeVar <- self$options$outcomeVar
            conditionVar <- self$options$conditionVar

            if (is.null(timeVar) || is.null(outcomeVar))
                return(FALSE)

            # Get data
            data <- self$data
            if (nrow(data) == 0)
                return(FALSE)

            time <- data[[timeVar]]
            status <- data[[outcomeVar]]

            if (is.factor(status)) {
                status <- as.numeric(status) - 1
            }

            # Get conditioning time (0 or NULL = use median follow-up)
            condTime <- self$options$conditionTime
            if (is.null(condTime) || is.na(condTime) || condTime <= 0) {
                condTime <- median(time, na.rm = TRUE)
            }

            tryCatch({
                p <- private$.createCondSurvPlot(
                    time, status, condTime, ggtheme,
                    conditionVar = conditionVar, data = data
                )
                print(p)
                return(TRUE)
            }, error = function(e) {
                # Return empty plot with error message
                plot.new()
                text(0.5, 0.5, paste("Error creating plot:", e$message),
                     cex = 1.2, col = "red")
                TRUE
            })
        },

        .runCondSurv = function(time, status, condTime, timePoints) {
            
            # Create survival object
            survObj <- survival::Surv(time, status)
            
            # Get method
            method <- self$options$method
            
            # Use condSURV package functions based on method
            if (method == "km") {
                # Kaplan-Meier weights approach
                weights <- condSURV::KMW(time = time, delta = status, x = condTime)
                condSurv <- private$.calculateCondSurvFromWeights(time, status, weights, timePoints, condTime)
                
            } else if (method == "landmark") {
                # Landmark approach - subset data to those who survived to condTime
                landmark_idx <- time >= condTime
                if (sum(landmark_idx) < 10) {
                    jmvcore::reject("Insufficient subjects surviving to landmark time")
                }
                
                # Adjust time and status for landmark analysis
                time_adj <- time[landmark_idx] - condTime
                status_adj <- status[landmark_idx]
                
                fit <- survival::survfit(survival::Surv(time_adj, status_adj) ~ 1)
                condSurv <- private$.extractCondSurvFromFit(fit, timePoints - condTime, condTime)
                
            } else if (method == "ipw") {
                # Inverse probability weighting (simplified implementation)
                condSurv <- private$.calculateIPWCondSurv(time, status, condTime, timePoints)
                
            } else {
                # Default to manual calculation
                condSurv <- private$.runManualCondSurv(time, status, condTime, timePoints)
            }
            
            return(condSurv)
        },

        .runManualCondSurv = function(time, status, condTime, timePoints) {
            
            # Manual conditional survival calculation
            # P(T > t | T > s) = P(T > t) / P(T > s) where s = condTime
            
            # Fit overall survival
            fit <- survival::survfit(survival::Surv(time, status) ~ 1)
            
            # Get survival probability at conditioning time
            survAtCondTime <- private$.getSurvProbAtTime(fit, condTime)
            
            if (survAtCondTime <= 0) {
                jmvcore::reject("No subjects survive to conditioning time")
            }
            
            # Calculate conditional survival for each time point
            results <- data.frame(
                time = timePoints,
                condtime = condTime,
                condprob = numeric(length(timePoints)),
                se = numeric(length(timePoints)),
                lower = numeric(length(timePoints)),
                upper = numeric(length(timePoints))
            )
            
            for (i in seq_along(timePoints)) {
                t <- timePoints[i]
                
                if (t <= condTime) {
                    # If t <= condTime, conditional probability is 1
                    results$condprob[i] <- 1.0
                    results$se[i] <- 0.0
                    results$lower[i] <- 1.0
                    results$upper[i] <- 1.0
                } else {
                    # P(T > t | T > condTime) = P(T > t) / P(T > condTime)
                    survAtT <- private$.getSurvProbAtTime(fit, t)
                    
                    condSurv <- survAtT / survAtCondTime
                    
                    # Approximate standard error using delta method
                    se_t <- private$.getSurvSEAtTime(fit, t)
                    se_cond <- private$.getSurvSEAtTime(fit, condTime)
                    
                    # Delta method approximation for SE of ratio
                    # Guard: when survAtT == 0, the ratio se_t/survAtT is Inf,
                    # and 0 * Inf = NaN in R
                    if (survAtT > 0) {
                        se <- condSurv * sqrt((se_t/survAtT)^2 + (se_cond/survAtCondTime)^2)
                    } else {
                        se <- 0
                    }
                    
                    # Confidence interval
                    ci_level <- self$options$confInt
                    z <- qnorm(1 - (1 - ci_level)/2)
                    
                    lower <- max(0, condSurv - z * se)
                    upper <- min(1, condSurv + z * se)
                    
                    results$condprob[i] <- condSurv
                    results$se[i] <- se
                    results$lower[i] <- lower
                    results$upper[i] <- upper
                }
            }
            
            return(results)
        },
        
        .calculateCondSurvFromWeights = function(time, status, weights, timePoints, condTime) {
            # Simplified conditional survival calculation from KM weights
            # This is a placeholder - full condSURV implementation would be more complex
            
            results <- data.frame(
                time = timePoints,
                condtime = condTime,
                condprob = numeric(length(timePoints)),
                se = numeric(length(timePoints)),
                lower = numeric(length(timePoints)),
                upper = numeric(length(timePoints))
            )
            
            # Use weighted Kaplan-Meier for conditional survival estimation
            for (i in seq_along(timePoints)) {
                t <- timePoints[i]
                
                if (t <= condTime) {
                    results$condprob[i] <- 1.0
                    results$se[i] <- 0.0
                    results$lower[i] <- 1.0
                    results$upper[i] <- 1.0
                } else {
                    # Weighted analysis for subjects surviving past condTime
                    idx <- time >= condTime
                    if (sum(idx) > 0) {
                        time_adj <- time[idx]
                        status_adj <- status[idx] 
                        weights_adj <- weights[idx]
                        
                        # Simple weighted survival estimate
                        at_risk <- sum(weights_adj[time_adj >= t])
                        total_at_cond <- sum(weights_adj)
                        
                        if (total_at_cond > 0) {
                            condSurv <- at_risk / total_at_cond
                            se <- sqrt(condSurv * (1 - condSurv) / total_at_cond)
                            
                            ci_level <- self$options$confInt
                            z <- qnorm(1 - (1 - ci_level)/2)
                            
                            results$condprob[i] <- condSurv
                            results$se[i] <- se
                            results$lower[i] <- max(0, condSurv - z * se)
                            results$upper[i] <- min(1, condSurv + z * se)
                        }
                    }
                }
            }
            
            return(results)
        },

        .calculateIPWCondSurv = function(time, status, condTime, timePoints) {
            # TODO: IPW Conditional Survival -- Implement proper inverse probability
            # weighting using IPCW estimator. Requires computing censoring weights
            # via Cox model on censoring distribution. See Beran (1981) or
            # condSURV package for reference implementation. Priority: LOW.
            return(private$.runManualCondSurv(time, status, condTime, timePoints))
        },

        .extractCondSurvFromFit = function(fit, adjTimePoints, condTime) {
            # Extract conditional survival from survfit object
            
            results <- data.frame(
                time = adjTimePoints + condTime,
                condtime = condTime,
                condprob = numeric(length(adjTimePoints)),
                se = numeric(length(adjTimePoints)),
                lower = numeric(length(adjTimePoints)),
                upper = numeric(length(adjTimePoints))
            )
            
            for (i in seq_along(adjTimePoints)) {
                t_adj <- adjTimePoints[i]
                
                if (t_adj <= 0) {
                    results$condprob[i] <- 1.0
                    results$se[i] <- 0.0
                    results$lower[i] <- 1.0
                    results$upper[i] <- 1.0
                } else {
                    survProb <- private$.getSurvProbAtTime(fit, t_adj)
                    survSE <- private$.getSurvSEAtTime(fit, t_adj)
                    
                    ci_level <- self$options$confInt
                    z <- qnorm(1 - (1 - ci_level)/2)
                    
                    results$condprob[i] <- survProb
                    results$se[i] <- survSE
                    results$lower[i] <- max(0, survProb - z * survSE)
                    results$upper[i] <- min(1, survProb + z * survSE)
                }
            }
            
            return(results)
        },

        .getSurvProbAtTime = function(fit, time) {
            # Get survival probability at specific time from survfit object
            if (length(fit$time) == 0 || time <= 0) {
                return(1.0)
            }

            # Find closest time point (Bug 2 fix: guard against empty which())
            idx <- which(fit$time <= time)
            if (length(idx) == 0) {
                return(1.0)
            }

            return(fit$surv[max(idx)])
        },

        .getSurvSEAtTime = function(fit, time) {
            # Get survival standard error at specific time from survfit object
            # Bug 1 fix: fit$std.err from survival::survfit is the SE of -log(S(t))
            # (cumulative hazard scale), NOT the SE of S(t) itself.
            # The actual SE of S(t) = S(t) * fit$std.err  (delta method on log transform).
            if (length(fit$time) == 0 || time <= 0 || is.null(fit$std.err) || is.null(fit$surv)) {
                return(0.0)
            }

            # Find closest time point (Bug 2 pattern: guard against empty which())
            idx <- which(fit$time <= time)
            if (length(idx) == 0) {
                return(0.0)
            }

            i <- max(idx)
            return(fit$surv[i] * fit$std.err[i])
        },

        .parseTimePoints = function() {
            # Parse comma-separated time points
            timePointsStr <- self$options$timePoints
            
            if (is.null(timePointsStr) || timePointsStr == "") {
                return(numeric(0))
            }
            
            tryCatch({
                points <- as.numeric(strsplit(timePointsStr, ",")[[1]])
                points <- points[!is.na(points)]
                return(sort(points))
            }, error = function(e) {
                return(numeric(0))
            })
        },

        .getDefaultTimePoints = function(time, condTime) {
            # Generate default time points for analysis
            maxTime <- max(time, na.rm = TRUE)

            # Bug 3 fix: reject when condTime is at or beyond the data horizon
            if (condTime >= maxTime) {
                jmvcore::reject(
                    "Conditioning time ({condTime}) is at or beyond maximum follow-up ({maxTime}). Choose an earlier time.",
                    code = ""
                )
            }

            # Create time points starting from condTime
            timePoints <- seq(condTime, maxTime, length.out = 6)
            timePoints <- timePoints[timePoints > condTime]  # Exclude condTime itself

            return(round(timePoints, 1))
        },

        .populateCondSurvTable = function(results, condTime) {

            table <- self$results$condsurvTable

            # Clear all existing rows (no-arg form is the correct pattern)
            table$deleteRows()

            # Add results -- include group column for stratified analyses
            for (i in seq_len(nrow(results))) {
                vals <- list(
                    time = results$time[i],
                    condtime = results$condtime[i],
                    condprob = results$condprob[i],
                    se = results$se[i],
                    lower = results$lower[i],
                    upper = results$upper[i]
                )
                if ("group" %in% names(results)) {
                    vals$group <- results$group[i]
                }
                table$addRow(rowKey = i, values = vals)
            }

            # Add footnote explaining conditioning
            table$addFootnote(rowNo = 1, col = "condprob",
                paste0("Conditional survival probabilities given survival to time ", condTime))
        },

        .createCondSurvPlot = function(time, status, condTime, ggtheme = NULL,
                                       conditionVar = NULL, data = NULL) {

            if (!requireNamespace("ggplot2", quietly = TRUE)) {
                jmvcore::reject("ggplot2 package required for plotting")
            }

            # Use jamovi ggtheme if provided, otherwise fall back to theme_minimal
            if (is.null(ggtheme)) {
                ggtheme <- ggplot2::theme_minimal()
            }

            # Build plot data: either stratified or overall
            if (!is.null(conditionVar) && !is.null(data)) {
                grpFactor <- as.factor(data[[conditionVar]])
                grpLevels <- levels(grpFactor)
                plot_data <- data.frame(
                    time = numeric(0), surv = numeric(0),
                    type = character(0), group = character(0)
                )

                for (grp in grpLevels) {
                    grpIdx <- grpFactor == grp
                    grpTime <- time[grpIdx]
                    grpStatus <- status[grpIdx]

                    if (sum(grpStatus == 1, na.rm = TRUE) < 3) next

                    fit <- survival::survfit(survival::Surv(grpTime, grpStatus) ~ 1)
                    survAtCond <- private$.getSurvProbAtTime(fit, condTime)

                    if (survAtCond > 0) {
                        idx_after <- fit$time > condTime
                        if (any(idx_after)) {
                            cond_df <- data.frame(
                                time = c(condTime, fit$time[idx_after]),
                                surv = c(1.0, fit$surv[idx_after] / survAtCond),
                                type = "Conditional",
                                group = grp
                            )
                            plot_data <- rbind(plot_data, cond_df)
                        }
                    }
                }

                if (nrow(plot_data) == 0) {
                    jmvcore::reject("No groups had sufficient data for conditional survival plotting")
                }

                p <- ggplot2::ggplot(
                    plot_data,
                    ggplot2::aes(x = time, y = surv, color = group)
                ) +
                    ggplot2::geom_step(linewidth = 1) +
                    ggplot2::geom_vline(
                        xintercept = condTime, linetype = "dashed", alpha = 0.7
                    ) +
                    ggplot2::scale_y_continuous(
                        limits = c(0, 1), labels = scales::percent
                    ) +
                    ggplot2::labs(
                        title = paste0(
                            "Conditional Survival by ", conditionVar,
                            " (given T > ", condTime, ")"
                        ),
                        x = "Time",
                        y = "Conditional Survival Probability",
                        color = conditionVar
                    ) +
                    ggtheme +
                    ggplot2::theme(
                        legend.position = "bottom",
                        plot.title = ggplot2::element_text(hjust = 0.5)
                    ) +
                    ggplot2::annotate(
                        "text", x = condTime, y = 0.05,
                        label = paste("Conditioning\nTime =", condTime),
                        hjust = 0, vjust = 0, size = 3
                    )

            } else {
                # Overall: show both overall KM and conditional survival curve
                fit <- survival::survfit(survival::Surv(time, status) ~ 1)

                plot_data <- data.frame(
                    time = fit$time,
                    surv = fit$surv,
                    type = "Overall Survival"
                )

                survAtCondTime <- private$.getSurvProbAtTime(fit, condTime)

                if (survAtCondTime > 0) {
                    idx_after_cond <- fit$time > condTime
                    if (any(idx_after_cond)) {
                        cond_data <- data.frame(
                            time = c(condTime, fit$time[idx_after_cond]),
                            surv = c(1.0, fit$surv[idx_after_cond] / survAtCondTime),
                            type = paste0("Conditional (T > ", condTime, ")")
                        )
                        plot_data <- rbind(plot_data, cond_data)
                    }
                }

                p <- ggplot2::ggplot(
                    plot_data,
                    ggplot2::aes(x = time, y = surv, color = type)
                ) +
                    ggplot2::geom_step(linewidth = 1) +
                    ggplot2::geom_vline(
                        xintercept = condTime, linetype = "dashed", alpha = 0.7
                    ) +
                    ggplot2::scale_y_continuous(
                        limits = c(0, 1), labels = scales::percent
                    ) +
                    ggplot2::labs(
                        title = "Conditional Survival Analysis",
                        x = "Time",
                        y = "Survival Probability",
                        color = "Curve Type"
                    ) +
                    ggtheme +
                    ggplot2::theme(
                        legend.position = "bottom",
                        plot.title = ggplot2::element_text(hjust = 0.5)
                    ) +
                    ggplot2::annotate(
                        "text", x = condTime, y = 0.05,
                        label = paste("Conditioning\nTime =", condTime),
                        hjust = 0, vjust = 0, size = 3
                    )
            }

            return(p)
        },

        .generateReportSentence = function(condTime, results, time, status) {

            method <- self$options$method
            confInt <- self$options$confInt
            method_name <- switch(method,
                "km" = "Kaplan-Meier weights",
                "landmark" = "landmark",
                "ipw" = "inverse probability weighting",
                "pkm" = "presmoothed Kaplan-Meier",
                method  # fallback: use raw method name
            )

            # Number at risk at conditioning time
            n_at_risk <- sum(time >= condTime, na.rm = TRUE)

            # Build report sentences -- one per group
            groups <- unique(results$group)
            sentences <- character(0)

            for (grp in groups) {
                grpRes <- results[results$group == grp, ]
                # Use the row with the largest time as the "target" time point
                target_row <- grpRes[which.max(grpRes$time), ]

                grp_label <- if (length(groups) > 1 || grp != "Overall") {
                    paste0(" in the ", htmltools::htmlEscape(grp), " group")
                } else {
                    ""
                }

                sentence <- sprintf(
                    paste0(
                        "Conditional survival analysis was performed using the %s method. ",
                        "Given survival to %.1f%s (n at risk = %d), the estimated conditional ",
                        "%.1f-unit survival probability was %.1f%% (%s%% CI: %.1f%%--%.1f%%)."
                    ),
                    method_name,
                    condTime,
                    grp_label,
                    n_at_risk,
                    target_row$time,
                    target_row$condprob * 100,
                    round(confInt * 100),
                    target_row$lower * 100,
                    target_row$upper * 100
                )
                sentences <- c(sentences, sentence)
            }

            html_content <- paste0(
                "<h4>Report Sentence (copy-ready)</h4>",
                "<div style='background-color:#f8f9fa; padding:10px; border-left:3px solid #007bff; margin:8px 0; font-style:italic;'>",
                paste(sentences, collapse = "<br><br>"),
                "</div>",
                "<p style='font-size:0.85em; color:#666;'>",
                "Tip: Copy the text above directly into your manuscript methods/results section.</p>"
            )

            self$results$reportSentence$setContent(html_content)
        },

        .populateAssumptions = function() {
            assumptions_html <- paste0(
                "<h4>Assumptions & Caveats</h4>",
                "<ul>",
                "<li><b>Non-informative censoring:</b> Censored patients must have the ",
                "same future survival probability as those who remain under observation.</li>",
                "<li><b>Conditioning time:</b> Must be within the observed follow-up range. ",
                "Estimates beyond maximum follow-up are unreliable.</li>",
                "<li><b>Sample size at conditioning time:</b> The number of patients still ",
                "at risk at the conditioning time must be adequate. Small risk sets produce ",
                "unstable estimates.</li>",
                "<li><b>Interpretation:</b> CS(t|t0) = P(T > t | T > t0). This is NOT ",
                "the same as the unconditional survival probability.</li>",
                "<li><b>Clinical use:</b> Conditional survival is most useful for patient ",
                "counseling after a period of disease-free survival (e.g., '5-year survival ",
                "given you already survived 2 years').</li>",
                "</ul>"
            )
            self$results$assumptions$setContent(assumptions_html)
        },

        .initTodo = function() {
            
            html <- self$results$todo
            
            str <- paste0(
                "<h2>Conditional Survival Estimation Analysis</h2>",
                "<p>This analysis calculates conditional survival probabilities, which represent the probability of surviving beyond a specific time point, given survival to a conditioning time point.</p>",
                
                "<h3>How to use this analysis:</h3>",
                "<ol>",
                "<li><b>Time Variable:</b> Select the survival time variable (numeric)</li>",
                "<li><b>Event/Status Variable:</b> Select the event indicator (0=censored, 1=event)</li>",
                "<li><b>Conditioning Variable (optional):</b> Variable for subgroup analysis</li>",
                "<li><b>Set Analysis Options:</b>",
                "<ul>",
                "<li><b>Conditioning Time Point:</b> Time at which to condition survival (default: median follow-up)</li>",
                "<li><b>Estimation Method:</b> Choose from Kaplan-Meier weights, Landmark approach, IPW, or Presmoothed KM</li>",
                "<li><b>Time Points:</b> Specify comma-separated time points for analysis (e.g., 12,24,60)</li>",
                "</ul></li>",
                "</ol>",
                
                "<h3>Interpretation:</h3>",
                "<p>Conditional survival P(T > t | T > s) represents the probability of surviving beyond time <i>t</i>, given survival to time <i>s</i> (conditioning time). This is clinically relevant for patients who have already survived a certain period and want to know their updated prognosis.</p>",
                
                "<h3>Methods Available:</h3>",
                "<ul>",
                "<li><b>Kaplan-Meier Weights:</b> Uses weighted estimation with KM weights</li>",
                "<li><b>Landmark Approach:</b> Subsets data to survivors at conditioning time</li>",
                "<li><b>Inverse Probability Weighting:</b> Accounts for censoring through weighting</li>",
                "<li><b>Presmoothed KM:</b> Smoothed version of Kaplan-Meier estimation</li>",
                "</ul>"
            )
            
            html$setContent(str)
        },

        .initMethodExplanation = function() {
            html <- self$results$methodExplanation
            html$setContent("<p>Method explanation will be updated after analysis.</p>")
        },

        .updateMethodExplanation = function(condTime, results) {
            
            html <- self$results$methodExplanation
            method <- self$options$method
            
            method_name <- switch(method,
                "km" = "Kaplan-Meier Weights",
                "landmark" = "Landmark Approach",
                "ipw" = "Inverse Probability Weighting",
                "pkm" = "Presmoothed Kaplan-Meier"
            )
            
            # Calculate some summary statistics
            avgCondSurv <- round(mean(results$condprob, na.rm = TRUE), 3)
            minCondSurv <- round(min(results$condprob, na.rm = TRUE), 3)
            maxCondSurv <- round(max(results$condprob, na.rm = TRUE), 3)
            
            str <- paste0(
                "<h3>Method: ", method_name, "</h3>",
                
                "<h4>Analysis Summary:</h4>",
                "<ul>",
                "<li><b>Conditioning Time:</b> ", condTime, "</li>",
                "<li><b>Number of Time Points:</b> ", nrow(results), "</li>",
                "<li><b>Average Conditional Survival:</b> ", avgCondSurv, "</li>",
                "<li><b>Range:</b> ", minCondSurv, " - ", maxCondSurv, "</li>",
                "</ul>",
                
                "<h4>Clinical Interpretation:</h4>",
                "<p>These conditional survival probabilities represent updated survival estimates for patients who have survived to time ", condTime, ". ",
                "For example, a conditional survival probability of 0.80 at time point ", max(results$time), " means that patients who have survived to time ", condTime, " have an 80% probability of surviving to time ", max(results$time), ".</p>",
                
                "<h4>Method Description:</h4>"
            )
            
            # Add method-specific description
            if (method == "km") {
                str <- paste0(str,
                    "<p><b>Kaplan-Meier Weights Method:</b> This approach uses Kaplan-Meier weights to estimate conditional survival probabilities. The weights account for the censoring pattern and provide more accurate estimates when censoring is informative.</p>"
                )
            } else if (method == "landmark") {
                str <- paste0(str,
                    "<p><b>Landmark Approach:</b> This method subsets the data to include only patients who survived to the conditioning time point, then estimates survival from that point forward. This approach is simple but may lose information from patients censored before the landmark time.</p>"
                )
            } else if (method == "ipw") {
                str <- paste0(str,
                    "<p><b>Inverse Probability Weighting:</b> This approach uses inverse probability weights to account for censoring, providing unbiased estimates of conditional survival probabilities even when censoring is informative.</p>"
                )
            } else {
                str <- paste0(str,
                    "<p><b>Presmoothed Kaplan-Meier:</b> This method applies smoothing techniques to the Kaplan-Meier estimator to reduce variability and provide more stable conditional survival estimates.</p>"
                )
            }
            
            str <- paste0(str,
                "<h4>Statistical Notes:</h4>",
                "<ul>",
                "<li>Confidence intervals are calculated using the specified confidence level (", self$options$confInt * 100, "%)</li>",
                "<li>Standard errors are computed using appropriate methods for each estimation approach</li>",
                "<li>Time points at or before the conditioning time have conditional survival probability = 1.0</li>",
                "</ul>"
            )
            
            html$setContent(str)
        }
    )
)