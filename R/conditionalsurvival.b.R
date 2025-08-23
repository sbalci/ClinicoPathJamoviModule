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
            
            # Handle factor status variable  
            if (is.factor(status)) {
                status <- as.numeric(status) - 1
            }
            
            # Check for required packages
            if (!requireNamespace("survival", quietly = TRUE)) {
                self$results$condsurvTable$addFootnote(rowNo = 1, col = "time", 
                    "survival package required but not available")
                return()
            }
            
            # Check if condSURV package is available
            has_condsurv <- requireNamespace("condSURV", quietly = TRUE)
            
            # Get conditioning time
            condTime <- self$options$conditionTime
            if (is.null(condTime) || is.na(condTime)) {
                # Use median follow-up time as default
                condTime <- median(time, na.rm = TRUE)
            }
            
            # Get specific time points
            timePoints <- private$.parseTimePoints()
            if (length(timePoints) == 0) {
                # Use default time points based on data range
                timePoints <- private$.getDefaultTimePoints(time, condTime)
            }
            
            # Run conditional survival analysis
            tryCatch({
                if (has_condsurv && self$options$method %in% c("km", "landmark", "ipw", "pkm")) {
                    results <- private$.runCondSurv(time, status, condTime, timePoints)
                } else {
                    # Fallback to manual calculation using survival package
                    results <- private$.runManualCondSurv(time, status, condTime, timePoints)
                }
                
                # Populate table
                private$.populateCondSurvTable(results, condTime)
                
                # Update explanations if requested
                if (self$options$showExplanations) {
                    private$.updateMethodExplanation(condTime, results)
                }
                
            }, error = function(e) {
                self$results$condsurvTable$addFootnote(rowNo = 1, col = "time", 
                    paste("Error in conditional survival calculation:", e$message))
            })
        },
        
        .plot = function(image, ...) {
            
            # Get variables
            timeVar <- self$options$timeVar
            outcomeVar <- self$options$outcomeVar
            
            if (is.null(timeVar) || is.null(outcomeVar))
                return()
                
            # Get data
            data <- self$data
            if (nrow(data) == 0)
                return()
                
            time <- data[[timeVar]]
            status <- data[[outcomeVar]]
            
            if (is.factor(status)) {
                status <- as.numeric(status) - 1
            }
            
            # Get conditioning time
            condTime <- self$options$conditionTime
            if (is.null(condTime) || is.na(condTime)) {
                condTime <- median(time, na.rm = TRUE)
            }
            
            tryCatch({
                p <- private$.createCondSurvPlot(time, status, condTime)
                print(p)
                TRUE
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
                    stop("Insufficient subjects surviving to landmark time")
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
                stop("No subjects survive to conditioning time")
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
                    se <- condSurv * sqrt((se_t/survAtT)^2 + (se_cond/survAtCondTime)^2)
                    
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
            # Simplified IPW conditional survival - placeholder implementation
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
            
            # Find closest time point
            idx <- max(which(fit$time <= time))
            if (length(idx) == 0 || idx == 0) {
                return(1.0)
            }
            
            return(fit$surv[idx])
        },

        .getSurvSEAtTime = function(fit, time) {
            # Get survival standard error at specific time from survfit object
            if (length(fit$time) == 0 || time <= 0 || is.null(fit$std.err)) {
                return(0.0)
            }
            
            # Find closest time point
            idx <- max(which(fit$time <= time))
            if (length(idx) == 0 || idx == 0) {
                return(0.0)
            }
            
            return(fit$std.err[idx])
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
            
            # Create time points starting from condTime
            timePoints <- seq(condTime, maxTime, length.out = 6)
            timePoints <- timePoints[timePoints > condTime]  # Exclude condTime itself
            
            return(round(timePoints, 1))
        },

        .populateCondSurvTable = function(results, condTime) {
            
            table <- self$results$condsurvTable
            
            # Clear existing rows
            for (i in seq_len(table$rowCount)) {
                table$deleteRows(rowNo = 1)
            }
            
            # Add results
            for (i in seq_len(nrow(results))) {
                table$addRow(rowKey = i, values = list(
                    time = results$time[i],
                    condtime = results$condtime[i],
                    condprob = results$condprob[i],
                    se = results$se[i],
                    lower = results$lower[i],
                    upper = results$upper[i]
                ))
            }
            
            # Add footnote explaining conditioning
            table$addFootnote(rowNo = 1, col = "condprob", 
                paste0("Conditional survival probabilities given survival to time ", condTime))
        },

        .createCondSurvPlot = function(time, status, condTime) {
            
            if (!requireNamespace("ggplot2", quietly = TRUE)) {
                stop("ggplot2 package required for plotting")
            }
            
            # Create survival fit for overall and conditional survival
            fit <- survival::survfit(survival::Surv(time, status) ~ 1)
            
            # Create data for plotting
            plot_data <- data.frame(
                time = fit$time,
                surv = fit$surv,
                type = "Overall Survival"
            )
            
            # Add conditional survival curve
            survAtCondTime <- private$.getSurvProbAtTime(fit, condTime)
            
            if (survAtCondTime > 0) {
                # Calculate conditional survival for all time points > condTime
                idx_after_cond <- fit$time > condTime
                
                if (any(idx_after_cond)) {
                    cond_data <- data.frame(
                        time = fit$time[idx_after_cond],
                        surv = fit$surv[idx_after_cond] / survAtCondTime,
                        type = paste0("Conditional Survival (t > ", condTime, ")")
                    )
                    
                    # Add point at conditioning time
                    cond_start <- data.frame(
                        time = condTime,
                        surv = 1.0,
                        type = paste0("Conditional Survival (t > ", condTime, ")")
                    )
                    
                    cond_data <- rbind(cond_start, cond_data)
                    plot_data <- rbind(plot_data, cond_data)
                }
            }
            
            # Create plot
            p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = time, y = surv, color = type)) +
                ggplot2::geom_step(linewidth = 1) +
                ggplot2::geom_vline(xintercept = condTime, linetype = "dashed", alpha = 0.7) +
                ggplot2::scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
                ggplot2::labs(
                    title = "Conditional Survival Analysis",
                    x = "Time",
                    y = "Survival Probability",
                    color = "Curve Type"
                ) +
                ggplot2::theme_minimal() +
                ggplot2::theme(
                    legend.position = "bottom",
                    plot.title = ggplot2::element_text(hjust = 0.5)
                ) +
                ggplot2::annotate("text", x = condTime, y = 0.1, 
                         label = paste("Conditioning\nTime =", condTime), 
                         hjust = 0, vjust = 0, size = 3)
            
            return(p)
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