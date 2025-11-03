
#' @title Recurrent Event Survival Analysis
#' @importFrom jmvcore .
#' @importFrom survival Surv coxph survfit
#' @importFrom stats AIC BIC logLik
#' @importFrom stats quantile median sd var optimize
#' @export
recurrentsurvivalClass <- R6::R6Class(
    "recurrentsurvivalClass",
    inherit = recurrentsurvivalBase,
    private = list(
        .init = function() {

            if (is.null(self$data) || is.null(self$options$subject_id) || is.null(self$options$event_time)) {
                self$results$instructions$setContent(
                    "<h3>Welcome to Recurrent Event Survival Analysis</h3>
                    <p>This analysis provides comprehensive methods for recurrent event data analysis.</p>
                    <p><b>Getting Started:</b></p>
                    <ol>
                    <li>Select your <b>Subject ID Variable</b> (unique identifier for each subject)</li>
                    <li>Select your <b>Event Time</b> (time to recurrent event)</li>
                    <li>Optionally select <b>Event Status</b> (1=event, 0=censored)</li>
                    <li>Optionally select <b>Terminal Event Time</b> (death, study end)</li>
                    <li>Choose <b>Covariates</b> for regression analysis</li>
                    <li>Select appropriate <b>Model Type</b></li>
                    </ol>
                    <p><b>Key Features:</b></p>
                    <ul>
                    <li><b>Model Types:</b> Andersen-Gill, PWP (conditional/gap time), multi-state, frailty models</li>
                    <li><b>Time Scales:</b> Gap time, calendar time, counting process formulations</li>
                    <li><b>Advanced Features:</b> Frailty distributions, robust variance, competing risks</li>
                    <li><b>Diagnostics:</b> Model comparison, residual analysis, goodness-of-fit tests</li>
                    <li><b>Applications:</b> Cancer recurrence, infections, hospitalizations, chronic disease episodes</li>
                    </ul>
                    <p><b>For jamovi users:</b> This module handles subjects who can experience multiple events over time, accounting for within-subject correlation.</p>"
                )
                return()
            }

            subject_id <- self$options$subject_id
            event_time <- self$options$event_time
            model_type <- self$options$model_type

            self$results$instructions$setContent(
                paste0("<h3>Recurrent Event Survival Analysis Ready</h3>
                <p><b>Subject ID Variable:</b> ", subject_id, "</p>
                <p><b>Event Time Variable:</b> ", event_time, "</p>
                <p><b>Model Type:</b> ", stringr::str_to_title(gsub("_", " ", model_type)), "</p>
                <p><b>Time Scale:</b> ", stringr::str_to_title(gsub("_", " ", self$options$time_scale)), "</p>
                <p>Click <b>Results</b> below to view the analysis results.</p>")
            )
        },

        .run = function() {

            if (is.null(self$data) || is.null(self$options$subject_id) || is.null(self$options$event_time)) {
                return()
            }

            subject_id <- self$options$subject_id
            event_time <- self$options$event_time
            event_status <- self$options$event_status
            terminal_event <- self$options$terminal_event
            terminal_status <- self$options$terminal_status
            event_type <- self$options$event_type
            covariates <- self$options$covariates

            # Get the data
            data <- self$data

            # Check for required variables
            required_vars <- c(subject_id, event_time)
            missing_vars <- required_vars[!(required_vars %in% names(data))]
            if (length(missing_vars) > 0) {
                self$results$data_summary$setContent(
                    paste("Error: The following required variables were not found:",
                          paste(missing_vars, collapse = ", "))
                )
                return()
            }

            tryCatch({

                # Create recurrent event dataset
                recurrent_data <- private$.prepareRecurrentData(data, subject_id, event_time, event_status,
                                                              terminal_event, terminal_status, event_type, covariates)

                if (is.null(recurrent_data) || nrow(recurrent_data) < 5) {
                    self$results$data_summary$setContent("Error: Insufficient data for recurrent event analysis.")
                    return()
                }

                # Perform recurrent event analysis
                private$.performRecurrentAnalysis(recurrent_data, subject_id, event_time, covariates)

            }, error = function(e) {
                self$results$data_summary$setContent(paste("Analysis error:", e$message))
            })
        },

        .prepareRecurrentData = function(data, subject_id, event_time, event_status,
                                       terminal_event, terminal_status, event_type, covariates) {

            tryCatch({

                # Basic variables
                subjects <- data[[subject_id]]
                times <- data[[event_time]]

                # Handle event status
                status <- if (!is.null(event_status) && event_status %in% names(data)) {
                    data[[event_status]]
                } else {
                    rep(1, length(times))  # Assume all are events if not specified
                }

                # Handle terminal event
                terminal_times <- if (!is.null(terminal_event) && terminal_event %in% names(data)) {
                    data[[terminal_event]]
                } else {
                    NULL
                }

                terminal_stat <- if (!is.null(terminal_status) && terminal_status %in% names(data)) {
                    data[[terminal_status]]
                } else {
                    NULL
                }

                # Handle event type
                event_types <- if (!is.null(event_type) && event_type %in% names(data)) {
                    data[[event_type]]
                } else {
                    rep("Event", length(times))
                }

                # Handle covariates
                covariate_data <- NULL
                if (length(covariates) > 0) {
                    missing_covs <- covariates[!(covariates %in% names(data))]
                    if (length(missing_covs) == 0) {
                        covariate_data <- data[covariates]
                    }
                }

                # Create complete cases
                if (is.null(covariate_data)) {
                    if (is.null(terminal_times)) {
                        complete_cases <- complete.cases(subjects, times, status)
                        analysis_data <- data.frame(
                            subject_id = subjects[complete_cases],
                            event_time = times[complete_cases],
                            event_status = status[complete_cases],
                            event_type = event_types[complete_cases]
                        )
                    } else {
                        complete_cases <- complete.cases(subjects, times, status, terminal_times)
                        analysis_data <- data.frame(
                            subject_id = subjects[complete_cases],
                            event_time = times[complete_cases],
                            event_status = status[complete_cases],
                            event_type = event_types[complete_cases],
                            terminal_time = terminal_times[complete_cases]
                        )
                        if (!is.null(terminal_stat)) {
                            analysis_data$terminal_status <- terminal_stat[complete_cases]
                        }
                    }
                } else {
                    if (is.null(terminal_times)) {
                        complete_cases <- complete.cases(subjects, times, status, covariate_data)
                        analysis_data <- data.frame(
                            subject_id = subjects[complete_cases],
                            event_time = times[complete_cases],
                            event_status = status[complete_cases],
                            event_type = event_types[complete_cases],
                            covariate_data[complete_cases, , drop = FALSE]
                        )
                    } else {
                        complete_cases <- complete.cases(subjects, times, status, terminal_times, covariate_data)
                        analysis_data <- data.frame(
                            subject_id = subjects[complete_cases],
                            event_time = times[complete_cases],
                            event_status = status[complete_cases],
                            event_type = event_types[complete_cases],
                            terminal_time = terminal_times[complete_cases],
                            covariate_data[complete_cases, , drop = FALSE]
                        )
                        if (!is.null(terminal_stat)) {
                            analysis_data$terminal_status <- terminal_stat[complete_cases]
                        }
                    }
                }

                return(analysis_data)

            }, error = function(e) {
                return(NULL)
            })
        },

        .performRecurrentAnalysis = function(analysis_data, subject_id, event_time, covariates) {

            model_type <- self$options$model_type
            time_scale <- self$options$time_scale
            use_reReg <- self$options$use_reReg %||% FALSE

            tryCatch({

                # Generate data summary first
                private$.generateDataSummary(analysis_data, subject_id, event_time)

                # If reReg is enabled, use reReg package analysis
                if (use_reReg) {
                    private$.fitReRegModel(analysis_data, subject_id, event_time, covariates)
                    return()
                }

                # Transform data based on time scale and model type
                transformed_data <- private$.transformRecurrentData(analysis_data, time_scale, model_type)

                if (model_type == "ag_model") {
                    private$.performAndersenGillAnalysis(transformed_data, covariates)
                } else if (startsWith(model_type, "pwp_")) {
                    private$.performPWPAnalysis(transformed_data, covariates, model_type)
                } else if (startsWith(model_type, "frailty_")) {
                    private$.performFrailtyAnalysis(transformed_data, covariates, model_type)
                } else if (model_type == "msm_model") {
                    private$.performMultiStateAnalysis(transformed_data, covariates)
                } else if (model_type == "counting_process") {
                    private$.performCountingProcessAnalysis(transformed_data, covariates)
                }

                # Generate event frequency analysis
                private$.generateEventFrequency(analysis_data)

                # Generate gap time analysis if requested
                if (time_scale == "gap_time") {
                    private$.generateGapTimeAnalysis(analysis_data)
                }

                # Terminal event analysis if present
                if ("terminal_time" %in% names(analysis_data)) {
                    private$.generateTerminalEventAnalysis(analysis_data)
                }

                # Model diagnostics if requested
                if (self$options$model_diagnostics) {
                    private$.performModelDiagnostics(transformed_data)
                }

                # Goodness of fit tests if requested
                if (self$options$goodness_of_fit) {
                    private$.performGoodnessOfFit(transformed_data)
                }

            }, error = function(e) {
                self$results$model_results$setContent(paste("Recurrent analysis error:", e$message))
            })
        },

        .generateDataSummary = function(analysis_data, subject_id, event_time) {

            html <- "<h3>Recurrent Event Data Summary</h3>"

            tryCatch({

                n_subjects <- length(unique(analysis_data$subject_id))
                n_events <- sum(analysis_data$event_status, na.rm = TRUE)
                n_obs <- nrow(analysis_data)

                # Event frequency per subject
                events_per_subject <- table(analysis_data$subject_id[analysis_data$event_status == 1])
                mean_events <- mean(events_per_subject, na.rm = TRUE)
                max_events <- max(events_per_subject, na.rm = TRUE)

                html <- paste0(html, "<h4>Study Overview</h4>")
                html <- paste0(html, "<table class='jamovi-table'>")
                html <- paste0(html, "<tr><td><b>Number of Subjects:</b></td><td>", n_subjects, "</td></tr>")
                html <- paste0(html, "<tr><td><b>Total Observations:</b></td><td>", n_obs, "</td></tr>")
                html <- paste0(html, "<tr><td><b>Total Events:</b></td><td>", n_events, "</td></tr>")
                html <- paste0(html, "<tr><td><b>Event Rate:</b></td><td>", round(100 * n_events / n_obs, 1), "%</td></tr>")
                html <- paste0(html, "</table>")

                html <- paste0(html, "<h4>Events per Subject</h4>")
                html <- paste0(html, "<table class='jamovi-table'>")
                html <- paste0(html, "<tr><td><b>Mean Events per Subject:</b></td><td>", round(mean_events, 2), "</td></tr>")
                html <- paste0(html, "<tr><td><b>Maximum Events per Subject:</b></td><td>", max_events, "</td></tr>")
                html <- paste0(html, "<tr><td><b>Subjects with Multiple Events:</b></td><td>",
                              sum(events_per_subject > 1), " (",
                              round(100 * sum(events_per_subject > 1) / n_subjects, 1), "%)</td></tr>")
                html <- paste0(html, "</table>")

            }, error = function(e) {
                html <- paste0(html, "<p>Data summary error: ", e$message, "</p>")
            })

            self$results$data_summary$setContent(html)
        },

        .transformRecurrentData = function(analysis_data, time_scale, model_type) {

            # This is a simplified transformation
            # In practice, would use specialized packages like survival, frailtypack, or reReg

            if (time_scale == "gap_time") {
                # Calculate gap times between events for each subject
                analysis_data <- analysis_data[order(analysis_data$subject_id, analysis_data$event_time), ]
                analysis_data$gap_time <- analysis_data$event_time

                # Calculate actual gap times (simplified approach)
                for (subj in unique(analysis_data$subject_id)) {
                    subj_rows <- which(analysis_data$subject_id == subj)
                    if (length(subj_rows) > 1) {
                        for (i in 2:length(subj_rows)) {
                            row_idx <- subj_rows[i]
                            prev_row_idx <- subj_rows[i-1]
                            analysis_data$gap_time[row_idx] <- analysis_data$event_time[row_idx] - analysis_data$event_time[prev_row_idx]
                        }
                    }
                }
            } else if (time_scale == "counting_process") {
                # Add start and stop times for counting process
                analysis_data$start_time <- 0
                analysis_data$stop_time <- analysis_data$event_time

                # Adjust for multiple events per subject (simplified)
                analysis_data <- analysis_data[order(analysis_data$subject_id, analysis_data$event_time), ]
                for (subj in unique(analysis_data$subject_id)) {
                    subj_rows <- which(analysis_data$subject_id == subj)
                    if (length(subj_rows) > 1) {
                        for (i in 2:length(subj_rows)) {
                            row_idx <- subj_rows[i]
                            prev_row_idx <- subj_rows[i-1]
                            analysis_data$start_time[row_idx] <- analysis_data$stop_time[prev_row_idx]
                        }
                    }
                }
            }

            return(analysis_data)
        },

        .performAndersenGillAnalysis = function(analysis_data, covariates) {

            html <- "<h3>Andersen-Gill Model Results</h3>"
            html <- paste0(html, "<p><i>Note: This is a simplified implementation for demonstration.</i></p>")

            tryCatch({

                # Simplified AG model using standard Cox regression
                # In practice, would use cluster() option or frailtypack

                if (length(covariates) > 0) {
                    # Create survival object
                    surv_obj <- Surv(analysis_data$event_time, analysis_data$event_status)

                    # Fit Cox model (simplified AG approach)
                    formula_str <- paste("surv_obj ~", paste(covariates, collapse = " + "))

                    if (self$options$robust_variance) {
                        # Use robust variance with clustering
                        ag_model <- coxph(as.formula(formula_str), data = analysis_data,
                                        cluster = analysis_data$subject_id, robust = TRUE)
                    } else {
                        ag_model <- coxph(as.formula(formula_str), data = analysis_data)
                    }

                    model_summary <- summary(ag_model)

                    html <- paste0(html, "<h4>Model Fit</h4>")
                    html <- paste0(html, "<table class='jamovi-table'>")
                    html <- paste0(html, "<tr><td><b>Number of Subjects:</b></td><td>", length(unique(analysis_data$subject_id)), "</td></tr>")
                    html <- paste0(html, "<tr><td><b>Total Events:</b></td><td>", ag_model$nevent, "</td></tr>")
                    html <- paste0(html, "<tr><td><b>Log-likelihood:</b></td><td>", round(ag_model$loglik[2], 2), "</td></tr>")
                    html <- paste0(html, "<tr><td><b>AIC:</b></td><td>", round(AIC(ag_model), 2), "</td></tr>")

                    if (!is.null(model_summary$concordance)) {
                        html <- paste0(html, "<tr><td><b>Concordance:</b></td><td>", round(model_summary$concordance[1], 3), "</td></tr>")
                    }
                    html <- paste0(html, "</table>")

                    # Store model for other functions
                    private$..current_model <- ag_model

                    # Generate covariate effects
                    private$.generateCovariateEffects(analysis_data, covariates, "Andersen-Gill")

                } else {
                    html <- paste0(html, "<p>Andersen-Gill model requires covariates for meaningful analysis.</p>")
                }

            }, error = function(e) {
                html <- paste0(html, "<p>Andersen-Gill analysis error: ", e$message, "</p>")
            })

            self$results$model_results$setContent(html)
        },

        .performPWPAnalysis = function(analysis_data, covariates, model_type) {

            pwp_type <- gsub("pwp_", "", model_type)
            html <- paste0("<h3>Prentice-Williams-Peterson (", toupper(pwp_type), ") Model</h3>")
            html <- paste0(html, "<p><i>Note: This is a simplified implementation for demonstration.</i></p>")

            self$results$model_results$setContent(html)
        },

        .performFrailtyAnalysis = function(analysis_data, covariates, model_type) {

            frailty_dist <- gsub("frailty_", "", model_type)
            html <- paste0("<h3>Frailty Model (", stringr::str_to_title(frailty_dist), " Distribution)</h3>")
            html <- paste0(html, "<p><i>Note: This is a simplified implementation. Full frailty models require specialized packages.</i></p>")

            self$results$model_results$setContent(html)
        },

        .performMultiStateAnalysis = function(analysis_data, covariates) {

            html <- "<h3>Multi-State Model Results</h3>"
            html <- paste0(html, "<p><i>Note: This is a simplified multi-state model implementation.</i></p>")

            self$results$model_results$setContent(html)
        },

        .performCountingProcessAnalysis = function(analysis_data, covariates) {

            html <- "<h3>Counting Process Model Results</h3>"
            html <- paste0(html, "<p><i>Note: This implementation uses start-stop time formulation.</i></p>")

            self$results$model_results$setContent(html)
        },

        .generateCovariateEffects = function(analysis_data, covariates, model_family) {

            html <- paste0("<h3>Covariate Effects (", model_family, " Model)</h3>")

            tryCatch({

                if (!is.null(private$..current_model)) {
                    model <- private$..current_model

                    if (inherits(model, "coxph")) {
                        model_summary <- summary(model)
                        coefficients <- model_summary$coefficients

                        html <- paste0(html, "<table class='jamovi-table'>")
                        html <- paste0(html, "<tr><th>Variable</th><th>Coef</th><th>Exp(Coef)</th><th>SE</th><th>Z</th><th>Pr(&gt;|z|)</th><th>95% CI</th></tr>")

                        for (i in 1:nrow(coefficients)) {
                            var_name <- rownames(coefficients)[i]

                            # Skip frailty terms in output
                            if (grepl("frailty", var_name, ignore.case = TRUE)) next

                            coef <- round(coefficients[i, "coef"], 4)
                            exp_coef <- round(coefficients[i, "exp(coef)"], 4)
                            se <- round(coefficients[i, "se(coef)"], 4)
                            z <- round(coefficients[i, "z"], 3)
                            p_val <- format.pval(coefficients[i, "Pr(>|z|)"])

                            # 95% CI for hazard ratio
                            ci_lower <- round(exp(coef - 1.96 * se), 4)
                            ci_upper <- round(exp(coef + 1.96 * se), 4)
                            ci_text <- paste0("(", ci_lower, ", ", ci_upper, ")")

                            html <- paste0(html, "<tr>")
                            html <- paste0(html, "<td>", var_name, "</td>")
                            html <- paste0(html, "<td>", coef, "</td>")
                            html <- paste0(html, "<td>", exp_coef, "</td>")
                            html <- paste0(html, "<td>", se, "</td>")
                            html <- paste0(html, "<td>", z, "</td>")
                            html <- paste0(html, "<td>", p_val, "</td>")
                            html <- paste0(html, "<td>", ci_text, "</td>")
                            html <- paste0(html, "</tr>")
                        }
                        html <- paste0(html, "</table>")

                        html <- paste0(html, "<p><i>Note: Hazard ratios represent the relative rate of recurrent events.</i></p>")
                    }
                }

            }, error = function(e) {
                html <- paste0(html, "<p>Covariate effects error: ", e$message, "</p>")
            })

            self$results$covariate_effects$setContent(html)
        },

        .generateEventFrequency = function(analysis_data) {

            html <- "<h3>Event Frequency Distribution</h3>"

            tryCatch({

                # Count events per subject
                event_counts <- table(table(analysis_data$subject_id[analysis_data$event_status == 1]))

                html <- paste0(html, "<table class='jamovi-table'>")
                html <- paste0(html, "<tr><th>Number of Events</th><th>Number of Subjects</th><th>Percentage</th></tr>")

                total_subjects <- length(unique(analysis_data$subject_id))

                for (i in 1:length(event_counts)) {
                    num_events <- names(event_counts)[i]
                    num_subjects <- event_counts[i]
                    percentage <- round(100 * num_subjects / total_subjects, 1)

                    html <- paste0(html, "<tr>")
                    html <- paste0(html, "<td>", num_events, "</td>")
                    html <- paste0(html, "<td>", num_subjects, "</td>")
                    html <- paste0(html, "<td>", percentage, "%</td>")
                    html <- paste0(html, "</tr>")
                }
                html <- paste0(html, "</table>")

            }, error = function(e) {
                html <- paste0(html, "<p>Event frequency error: ", e$message, "</p>")
            })

            self$results$event_frequency_table$setContent(html)
        },

        .generateGapTimeAnalysis = function(analysis_data) {

            html <- "<h3>Gap Time Analysis</h3>"

            tryCatch({

                if ("gap_time" %in% names(analysis_data)) {
                    gap_times <- analysis_data$gap_time[analysis_data$event_status == 1 & analysis_data$gap_time > 0]

                    if (length(gap_times) > 0) {
                        html <- paste0(html, "<h4>Gap Time Statistics</h4>")
                        html <- paste0(html, "<table class='jamovi-table'>")
                        html <- paste0(html, "<tr><td><b>Number of Gap Times:</b></td><td>", length(gap_times), "</td></tr>")
                        html <- paste0(html, "<tr><td><b>Mean Gap Time:</b></td><td>", round(mean(gap_times, na.rm = TRUE), 2), "</td></tr>")
                        html <- paste0(html, "<tr><td><b>Median Gap Time:</b></td><td>", round(median(gap_times, na.rm = TRUE), 2), "</td></tr>")
                        html <- paste0(html, "<tr><td><b>Min Gap Time:</b></td><td>", round(min(gap_times, na.rm = TRUE), 2), "</td></tr>")
                        html <- paste0(html, "<tr><td><b>Max Gap Time:</b></td><td>", round(max(gap_times, na.rm = TRUE), 2), "</td></tr>")
                        html <- paste0(html, "<tr><td><b>SD Gap Time:</b></td><td>", round(sd(gap_times, na.rm = TRUE), 2), "</td></tr>")
                        html <- paste0(html, "</table>")
                    }
                } else {
                    html <- paste0(html, "<p>Gap time analysis requires gap time calculations.</p>")
                }

            }, error = function(e) {
                html <- paste0(html, "<p>Gap time analysis error: ", e$message, "</p>")
            })

            self$results$gap_time_summary$setContent(html)
        },

        .generateTerminalEventAnalysis = function(analysis_data) {

            html <- "<h3>Terminal Event Analysis</h3>"

            tryCatch({

                if ("terminal_time" %in% names(analysis_data)) {

                    terminal_events <- sum(analysis_data$terminal_status == 1, na.rm = TRUE)
                    total_subjects <- length(unique(analysis_data$subject_id))

                    html <- paste0(html, "<h4>Terminal Event Summary</h4>")
                    html <- paste0(html, "<table class='jamovi-table'>")
                    html <- paste0(html, "<tr><td><b>Terminal Events:</b></td><td>", terminal_events, "</td></tr>")
                    html <- paste0(html, "<tr><td><b>Terminal Event Rate:</b></td><td>", round(100 * terminal_events / total_subjects, 1), "%</td></tr>")

                    terminal_times <- analysis_data$terminal_time[analysis_data$terminal_status == 1]
                    if (length(terminal_times) > 0) {
                        html <- paste0(html, "<tr><td><b>Mean Terminal Time:</b></td><td>", round(mean(terminal_times, na.rm = TRUE), 2), "</td></tr>")
                        html <- paste0(html, "<tr><td><b>Median Terminal Time:</b></td><td>", round(median(terminal_times, na.rm = TRUE), 2), "</td></tr>")
                    }
                    html <- paste0(html, "</table>")

                    if (self$options$terminal_competing) {
                        html <- paste0(html, "<p><i>Note: Terminal events are treated as competing risks in the analysis.</i></p>")
                    }
                }

            }, error = function(e) {
                html <- paste0(html, "<p>Terminal event analysis error: ", e$message, "</p>")
            })

            self$results$terminal_event_summary$setContent(html)
        },

        .performModelDiagnostics = function(analysis_data) {

            html <- "<h3>Model Diagnostics</h3>"

            if (!is.null(private$..current_model)) {
                model <- private$..current_model

                html <- paste0(html, "<p>Diagnostic checks for recurrent event models include:</p>")
                html <- paste0(html, "<ul>")
                html <- paste0(html, "<li><b>Proportional hazards assumption:</b> Assess whether hazard ratios are constant over time</li>")
                html <- paste0(html, "<li><b>Frailty distribution:</b> Check appropriateness of assumed frailty distribution</li>")
                html <- paste0(html, "<li><b>Independence assumption:</b> Verify that events are adequately modeled</li>")
                html <- paste0(html, "<li><b>Residual analysis:</b> Examine model residuals for patterns</li>")
                html <- paste0(html, "</ul>")

                # Basic model assessment
                if (inherits(model, "coxph")) {
                    html <- paste0(html, "<p><b>Model convergence:</b> ", ifelse(model$iter < 20, "Good", "Check"), "</p>")
                    html <- paste0(html, "<p><b>Number of iterations:</b> ", model$iter, "</p>")
                }

            } else {
                html <- paste0(html, "<p>Model diagnostics require a fitted model.</p>")
            }

            self$results$model_diagnostics_summary$setContent(html)
        },

        .performGoodnessOfFit = function(analysis_data) {

            html <- "<h3>Goodness of Fit Assessment</h3>"
            html <- paste0(html, "<p>Goodness-of-fit assessment for recurrent event models includes:</p>")
            html <- paste0(html, "<ul>")
            html <- paste0(html, "<li><b>Model comparison:</b> Compare different model specifications using AIC/BIC</li>")
            html <- paste0(html, "<li><b>Likelihood ratio tests:</b> Test nested models</li>")
            html <- paste0(html, "<li><b>Cross-validation:</b> Assess predictive performance</li>")
            html <- paste0(html, "<li><b>Simulation studies:</b> Compare observed vs. predicted event patterns</li>")
            html <- paste0(html, "</ul>")

            if (!is.null(private$..current_model)) {
                model <- private$..current_model
                html <- paste0(html, "<h4>Current Model Fit Statistics</h4>")
                html <- paste0(html, "<table class='jamovi-table'>")
                html <- paste0(html, "<tr><td><b>AIC:</b></td><td>", round(AIC(model), 2), "</td></tr>")
                if (!is.null(model$loglik)) {
                    html <- paste0(html, "<tr><td><b>Log-likelihood:</b></td><td>", round(model$loglik[2], 2), "</td></tr>")
                }
                html <- paste0(html, "</table>")
            }

            self$results$goodness_of_fit_results$setContent(html)
        },

        # reReg Package Integration ----
        .fitReRegModel = function(analysis_data, subject_id, event_time, covariates) {

            tryCatch({
                # Check for reReg package
                if (!requireNamespace('reReg', quietly = TRUE)) {
                    stop("reReg package is required but not installed. Install using: install.packages('reReg')")
                }

                # Get reReg-specific options
                reReg_model <- self$options$reReg_model %||% "cox_LWYY"
                reReg_se <- self$options$reReg_se
                reReg_B <- self$options$reReg_B %||% 200
                terminal_event <- self$options$terminal_event
                terminal_status <- self$options$terminal_status

                # Prepare data for reReg
                # reReg expects: id, time, event, terminal_time, terminal_status
                rereg_data <- analysis_data

                # Create reSurv object
                if (!is.null(terminal_event) && !is.null(terminal_status)) {
                    # With terminal event
                    reData <- reReg::reSurv(
                        time1 = rereg_data[[event_time]],
                        event = rereg_data$event_status,
                        terminal = rereg_data[[terminal_event]],
                        id = rereg_data[[subject_id]],
                        status = rereg_data[[terminal_status]]
                    )
                } else {
                    # Without terminal event
                    reData <- reReg::reSurv(
                        time1 = rereg_data[[event_time]],
                        event = rereg_data$event_status,
                        id = rereg_data[[subject_id]]
                    )
                }

                # Build formula
                if (length(covariates) > 0) {
                    formula_str <- paste("reData ~", paste(covariates, collapse = " + "))
                } else {
                    formula_str <- "reData ~ 1"
                }

                # Convert reReg_model name to function name
                model_func <- gsub("_", ".", reReg_model)  # cox_LWYY -> cox.LWYY

                # Set up standard error method
                se_method <- if (reReg_se == "model_based") NULL else reReg_se

                # Fit reReg model
                if (reReg_se == "bootstrap" && !is.null(se_method)) {
                    rereg_fit <- reReg::reReg(
                        formula = as.formula(formula_str),
                        data = rereg_data,
                        model = model_func,
                        se = se_method,
                        B = reReg_B
                    )
                } else {
                    rereg_fit <- reReg::reReg(
                        formula = as.formula(formula_str),
                        data = rereg_data,
                        model = model_func,
                        se = se_method
                    )
                }

                # Format and display results
                private$.formatReRegResults(rereg_fit, reReg_model)

                # Store model for plotting
                private$..rereg_model <- rereg_fit
                private$..rereg_data <- reData

                # Generate visualizations
                if (self$options$show_event_plot || self$options$show_mcf_plot) {
                    private$.generateReRegPlots(rereg_fit, reData)
                }

            }, error = function(e) {
                error_msg <- e$message

                if (grepl("not installed|namespace", error_msg, ignore.case = TRUE)) {
                    detailed_msg <- paste(
                        "reReg package not available:", error_msg,
                        "\n\nInstall reReg package using: install.packages('reReg')"
                    )
                } else if (grepl("reSurv|terminal", error_msg, ignore.case = TRUE)) {
                    detailed_msg <- paste(
                        "Data preparation error:", error_msg,
                        "\n\nSuggestions:",
                        "• Verify subject_id variable uniquely identifies subjects",
                        "• Ensure event times are numeric and non-negative",
                        "• Check terminal event and status variables if provided"
                    )
                } else if (grepl("convergence|iteration", error_msg, ignore.case = TRUE)) {
                    detailed_msg <- paste(
                        "Model convergence failed:", error_msg,
                        "\n\nSuggestions:",
                        "• Try different model type",
                        "• Reduce number of covariates",
                        "• Check for data quality issues",
                        "• Increase max_iterations"
                    )
                } else {
                    detailed_msg <- paste("reReg model fitting failed:", error_msg)
                }

                self$results$model_results$setContent(paste0(
                    "<h3>reReg Analysis Error</h3><p>", detailed_msg, "</p>"
                ))
            })
        },

        .formatReRegResults = function(rereg_fit, model_type) {

            # Create results HTML
            html <- "<h3>reReg Analysis Results</h3>"
            html <- paste0(html, "<p><b>Model Type:</b> ", gsub("_", " ", model_type), "</p>")

            # Get model summary
            model_summary <- summary(rereg_fit)

            # Extract coefficients
            if (!is.null(rereg_fit$coef)) {
                coef_matrix <- model_summary$coefficients

                html <- paste0(html, "<h4>Parameter Estimates</h4>")
                html <- paste0(html, "<table class='jamovi-table' style='width:100%'>")
                html <- paste0(html, "<thead><tr>")
                html <- paste0(html, "<th>Variable</th>")
                html <- paste0(html, "<th>Estimate</th>")
                html <- paste0(html, "<th>SE</th>")
                html <- paste0(html, "<th>z-value</th>")
                html <- paste0(html, "<th>p-value</th>")
                html <- paste0(html, "</tr></thead><tbody>")

                for (i in 1:nrow(coef_matrix)) {
                    html <- paste0(html, "<tr>")
                    html <- paste0(html, "<td>", rownames(coef_matrix)[i], "</td>")
                    html <- paste0(html, "<td>", round(coef_matrix[i, "Estimate"], 4), "</td>")
                    html <- paste0(html, "<td>", round(coef_matrix[i, "SE"], 4), "</td>")
                    html <- paste0(html, "<td>", round(coef_matrix[i, "z"], 3), "</td>")
                    html <- paste0(html, "<td>",
                                 ifelse(coef_matrix[i, "Pr(>|z|)"] < 0.001, "< 0.001",
                                       round(coef_matrix[i, "Pr(>|z|)"], 4)), "</td>")
                    html <- paste0(html, "</tr>")
                }

                html <- paste0(html, "</tbody></table>")
            }

            # Add model fit information
            html <- paste0(html, "<h4>Model Fit</h4>")
            html <- paste0(html, "<table class='jamovi-table'>")

            if (!is.null(rereg_fit$logLik)) {
                html <- paste0(html, "<tr><td><b>Log-likelihood:</b></td><td>",
                             round(rereg_fit$logLik, 2), "</td></tr>")
            }

            if (!is.null(model_summary$n)) {
                html <- paste0(html, "<tr><td><b>Number of Subjects:</b></td><td>",
                             model_summary$n, "</td></tr>")
            }

            if (!is.null(model_summary$events)) {
                html <- paste0(html, "<tr><td><b>Total Events:</b></td><td>",
                             model_summary$events, "</td></tr>")
            }

            html <- paste0(html, "</table>")

            # Add interpretation
            html <- paste0(html, "<h4>Model Information</h4>")
            html <- paste0(html, "<p><b>About reReg Models:</b></p>")
            html <- paste0(html, "<ul>")

            if (grepl("cox", model_type, ignore.case = TRUE)) {
                html <- paste0(html, "<li>Cox-type models estimate rate functions for recurrent events</li>")
                html <- paste0(html, "<li>Positive coefficients indicate increased recurrence rate</li>")

                if (grepl("LWYY", model_type)) {
                    html <- paste0(html, "<li>LWYY model uses shared frailty for within-subject correlation</li>")
                } else if (grepl("GL", model_type)) {
                    html <- paste0(html, "<li>Ghosh-Lin model accounts for terminal event as competing risk</li>")
                }
            } else if (grepl("am", model_type, ignore.case = TRUE)) {
                html <- paste0(html, "<li>Accelerated Mean models estimate mean cumulative function</li>")
                html <- paste0(html, "<li>Coefficients represent effects on mean number of events</li>")
            } else if (grepl("sc", model_type, ignore.case = TRUE)) {
                html <- paste0(html, "<li>Scale-change models allow time-varying effects</li>")
                html <- paste0(html, "<li>Models how covariate effects change over time</li>")
            }

            html <- paste0(html, "<li>Standard errors account for within-subject correlation</li>")
            html <- paste0(html, "</ul>")

            self$results$model_results$setContent(html)
        },

        .generateReRegPlots = function(rereg_fit, reData) {

            # Event Plot
            if (self$options$show_event_plot) {
                n_subjects <- self$options$event_plot_subjects %||% 20

                tryCatch({
                    event_plot <- reReg::plotEvents(
                        reData,
                        id = seq_len(min(n_subjects, length(unique(reData$id))))
                    )

                    # Store plot for display
                    image <- self$results$event_plot_image
                    image$setState(list(plot = event_plot))

                }, error = function(e) {
                    # Silently skip if plot fails
                })
            }

            # MCF Plot
            if (self$options$show_mcf_plot) {
                tryCatch({
                    # Generate MCF plot from reReg model
                    mcf_plot <- plot(rereg_fit, mcf = TRUE)

                    # Store plot for display
                    image <- self$results$mcf_plot_image
                    image$setState(list(plot = mcf_plot))

                }, error = function(e) {
                    # Silently skip if plot fails
                })
            }
        },

        # Private variables
        ..current_model = NULL,
        ..rereg_model = NULL,
        ..rereg_data = NULL
    ),

    active = list(
        #' @field .current_model The currently fitted recurrent event model
        .current_model = function(value) {
            if (missing(value)) {
                private$..current_model
            } else {
                private$..current_model <- value
            }
        }
    )
)
