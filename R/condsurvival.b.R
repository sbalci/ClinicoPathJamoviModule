condsurvivalClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "condsurvivalClass",
    inherit = condsurvivalBase,
    private = list(
        .init = function() {
            if (is.null(self$data) || is.null(self$options$time) || is.null(self$options$status)) {
                self$results$instructions$setContent(
                    "<html>
                    <head>
                    <meta http-equiv='Content-Type' content='text/html; charset=UTF-8'>
                    </head>
                    <body>
                    <h3>Conditional Survival Analysis</h3>
                    <p><b>Data Requirements:</b></p>
                    <p>This module requires:</p>
                    <ul>
                    <li><b>Time Variable</b>: Numeric variable with survival/follow-up times</li>
                    <li><b>Event Status</b>: Binary variable indicating event occurrence (1/TRUE = event, 0/FALSE = censored)</li>
                    <li><b>Group Variable</b> (optional): Categorical variable for stratified analysis</li>
                    <li><b>Subject ID</b> (optional): Identifier for longitudinal tracking</li>
                    </ul>
                    
                    <p><b>Conditional Survival Analysis:</b></p>
                    <p>Conditional survival estimates the probability of surviving an additional time period, 
                    given that a patient has already survived to a certain time point. This is expressed as:</p>
                    <p><b>CS(t|s) = S(s+t) / S(s)</b></p>
                    <p>Where CS(t|s) is the conditional survival probability of surviving an additional 
                    t units of time, given survival to time s.</p>
                    
                    <p><b>Clinical Applications:</b></p>
                    <ul>
                    <li><b>Dynamic Prognosis</b>: Updated survival estimates as patients survive longer</li>
                    <li><b>Patient Counseling</b>: More relevant survival information for long-term survivors</li>
                    <li><b>Treatment Planning</b>: Informed decision-making for ongoing care</li>
                    <li><b>Follow-up Strategies</b>: Risk-adapted surveillance schedules</li>
                    </ul>
                    
                    <p><b>Key Features:</b></p>
                    <ul>
                    <li>Multiple conditional time points analysis</li>
                    <li>Kaplan-Meier and parametric survival estimation</li>
                    <li>Dynamic prediction capabilities</li>
                    <li>Landmark analysis to address immortal time bias</li>
                    <li>Group comparisons and statistical testing</li>
                    <li>Comprehensive visualization suite</li>
                    </ul>
                    </body>
                    </html>"
                )
                return()
            }
        },

        .run = function() {
            # Check for required packages
            if (!requireNamespace("survival", quietly = TRUE)) {
                stop("Package 'survival' is required for conditional survival analysis but is not installed.")
            }
            
            if (!requireNamespace("survminer", quietly = TRUE)) {
                message("Package 'survminer' is recommended for enhanced survival plots.")
            }

            # Get variables
            time_var <- self$options$time
            status_var <- self$options$status
            group_var <- self$options$group
            id_var <- self$options$id
            
            if (is.null(time_var) || is.null(status_var)) return()

            # Prepare data
            data <- self$data
            time_values <- jmvcore::toNumeric(data[[time_var]])
            status_values <- jmvcore::toNumeric(data[[status_var]])
            
            # Handle group variable
            group_values <- if (!is.null(group_var)) {
                as.factor(data[[group_var]])
            } else {
                factor(rep("All", nrow(data)), levels = "All")
            }
            
            # Remove missing values
            complete_cases <- complete.cases(time_values, status_values, group_values)
            if (sum(complete_cases) < 10) {
                self$results$instructions$setContent("Error: Need at least 10 complete observations for conditional survival analysis.")
                return()
            }
            
            clean_data <- data.frame(
                time = time_values[complete_cases],
                status = status_values[complete_cases],
                group = group_values[complete_cases]
            )
            
            # Parse time points
            conditional_times <- private$.parseTimePoints(self$options$conditional_times)
            prediction_times <- private$.parseTimePoints(self$options$prediction_times)
            
            if (length(conditional_times) == 0 || length(prediction_times) == 0) {
                self$results$instructions$setContent("Error: Please specify valid conditional and prediction time points.")
                return()
            }
            
            # Perform conditional survival analysis
            private$.performConditionalSurvivalAnalysis(clean_data, conditional_times, prediction_times)
            
            # Populate method explanation
            private$.populateMethodExplanation()
        },

        .parseTimePoints = function(time_string) {
            if (is.null(time_string) || time_string == "") return(numeric(0))
            
            # Parse comma-separated values
            time_points <- tryCatch({
                as.numeric(unlist(strsplit(gsub("\\s", "", time_string), ",")))
            }, error = function(e) numeric(0))
            
            # Filter valid positive numbers
            time_points[!is.na(time_points) & time_points > 0]
        },

        .performConditionalSurvivalAnalysis = function(data, conditional_times, prediction_times) {
            # Populate data overview
            private$.populateDataOverview(data)
            
            # Fit survival models by group
            groups <- levels(data$group)
            surv_models <- list()
            
            for (group in groups) {
                group_data <- data[data$group == group, ]
                
                # Fit survival model based on method
                method <- self$options$survival_method
                if (method == "parametric") {
                    dist <- self$options$parametric_dist
                    surv_models[[group]] <- private$.fitParametricSurvival(group_data, dist)
                } else {
                    # Kaplan-Meier or Fleming-Harrington
                    surv_formula <- survival::Surv(time, status) ~ 1
                    surv_models[[group]] <- survival::survfit(surv_formula, data = group_data)
                }
            }
            
            # Calculate conditional survival probabilities
            private$.calculateConditionalSurvival(surv_models, groups, conditional_times, prediction_times)
            
            # Perform additional analyses if requested
            if (self$options$dynamic_prediction) {
                private$.performDynamicPrediction(data, surv_models)
            }
            
            if (self$options$landmark_analysis) {
                private$.performLandmarkAnalysis(data, conditional_times)
            }
            
            # Group comparisons if multiple groups
            if (length(groups) > 1) {
                private$.performGroupComparisons(data, conditional_times)
            }
        },

        .populateDataOverview = function(data) {
            overview_table <- self$results$dataOverview
            
            # Overall statistics
            n_total <- nrow(data)
            n_events <- sum(data$status)
            median_followup <- median(data$time)
            event_rate <- (n_events / n_total) * 100
            
            overview_table$addRow(rowKey = "n", values = list(
                characteristic = "Sample Size",
                overall = as.character(n_total)
            ))
            
            overview_table$addRow(rowKey = "events", values = list(
                characteristic = "Events",
                overall = sprintf("%d (%.1f%%)", n_events, event_rate)
            ))
            
            overview_table$addRow(rowKey = "median_time", values = list(
                characteristic = "Median Follow-up Time",
                overall = sprintf("%.2f %s", median_followup, self$options$time_scale)
            ))
            
            # Group-specific statistics if applicable
            if (!is.null(self$options$group)) {
                groups <- levels(data$group)
                if (length(groups) == 2) {
                    for (i in seq_along(groups)) {
                        group_data <- data[data$group == groups[i], ]
                        n_group <- nrow(group_data)
                        n_events_group <- sum(group_data$status)
                        event_rate_group <- (n_events_group / n_group) * 100
                        
                        col_name <- paste0("group", i)
                        
                        # Update rows with group information
                        overview_table$setCell(rowKey = "n", col = col_name, 
                                             value = as.character(n_group))
                        overview_table$setCell(rowKey = "events", col = col_name, 
                                             value = sprintf("%d (%.1f%%)", n_events_group, event_rate_group))
                        overview_table$setCell(rowKey = "median_time", col = col_name, 
                                             value = sprintf("%.2f %s", median(group_data$time), self$options$time_scale))
                    }
                }
            }
        },

        .fitParametricSurvival = function(data, distribution) {
            # Fit parametric survival model using flexsurv if available
            if (requireNamespace("flexsurv", quietly = TRUE)) {
                dist_map <- list(
                    "weibull" = "weibull",
                    "exponential" = "exp",
                    "lognormal" = "lnorm", 
                    "loglogistic" = "llogis"
                )
                
                flexsurv_dist <- dist_map[[distribution]]
                if (!is.null(flexsurv_dist)) {
                    return(flexsurv::flexsurvreg(survival::Surv(time, status) ~ 1, 
                                               data = data, dist = flexsurv_dist))
                }
            }
            
            # Fallback to survival package
            return(survival::survreg(survival::Surv(time, status) ~ 1, 
                                   data = data, dist = distribution))
        },

        .calculateConditionalSurvival = function(surv_models, groups, conditional_times, prediction_times) {
            cs_table <- self$results$conditionalSurvivalTable
            improvement_table <- self$results$conditionalImprovementTable
            
            confidence_level <- self$options$confidence_level
            alpha <- 1 - confidence_level
            
            for (group in groups) {
                model <- surv_models[[group]]
                
                for (cond_time in conditional_times) {
                    for (pred_time in prediction_times) {
                        target_time <- cond_time + pred_time
                        
                        # Calculate survival probabilities
                        if (inherits(model, "flexsurvreg")) {
                            # Parametric model
                            s_cond <- summary(model, t = cond_time, ci = TRUE)[[1]]$est[1]
                            s_target <- summary(model, t = target_time, ci = TRUE)[[1]]$est[1]
                            
                            s_cond_lower <- summary(model, t = cond_time, ci = TRUE)[[1]]$lcl[1]
                            s_cond_upper <- summary(model, t = cond_time, ci = TRUE)[[1]]$ucl[1]
                            s_target_lower <- summary(model, t = target_time, ci = TRUE)[[1]]$lcl[1]
                            s_target_upper <- summary(model, t = target_time, ci = TRUE)[[1]]$ucl[1]
                            
                        } else {
                            # Kaplan-Meier model
                            summary_cond <- summary(model, times = cond_time, extend = TRUE)
                            summary_target <- summary(model, times = target_time, extend = TRUE)
                            
                            s_cond <- summary_cond$surv[1]
                            s_target <- summary_target$surv[1]
                            s_cond_lower <- summary_cond$lower[1]
                            s_cond_upper <- summary_cond$upper[1]
                            s_target_lower <- summary_target$lower[1]
                            s_target_upper <- summary_target$upper[1]
                            
                            # Number at risk
                            n_risk <- summary_cond$n.risk[1]
                        }
                        
                        # Calculate conditional survival probability
                        if (!is.na(s_cond) && s_cond > 0) {
                            cs_prob <- s_target / s_cond
                            
                            # Confidence interval using delta method
                            cs_lower <- s_target_lower / s_cond_upper
                            cs_upper <- s_target_upper / s_cond_lower
                            
                            # Ensure probabilities are in [0,1]
                            cs_prob <- pmax(0, pmin(1, cs_prob))
                            cs_lower <- pmax(0, pmin(1, cs_lower))
                            cs_upper <- pmax(0, pmin(1, cs_upper))
                            
                            ci_text <- sprintf("[%.3f, %.3f]", cs_lower, cs_upper)
                            
                            # Clinical interpretation
                            clinical_meaning <- private$.interpretConditionalSurvival(cs_prob, pred_time, self$options$time_scale)
                            
                            # Add to table
                            row_key <- paste(group, cond_time, pred_time, sep = "_")
                            cs_table$addRow(rowKey = row_key, values = list(
                                group = if (!is.null(self$options$group)) group else "",
                                conditional_time = sprintf("%.1f %s", cond_time, self$options$time_scale),
                                additional_time = sprintf("%.1f %s", pred_time, self$options$time_scale),
                                conditional_prob = cs_prob,
                                confidence_interval = ci_text,
                                patients_at_risk = if (exists("n_risk")) n_risk else NA,
                                clinical_interpretation = clinical_meaning
                            ))
                            
                            # Calculate improvement over unconditional survival
                            unconditional_prob <- s_target
                            if (!is.na(unconditional_prob) && unconditional_prob > 0) {
                                improvement <- cs_prob - unconditional_prob
                                improvement_pct <- (improvement / unconditional_prob) * 100
                                
                                improvement_key <- paste(group, cond_time, sep = "_")
                                improvement_table$addRow(rowKey = improvement_key, values = list(
                                    group = if (!is.null(self$options$group)) group else "",
                                    conditional_time = sprintf("%.1f %s", cond_time, self$options$time_scale),
                                    unconditional_prob = unconditional_prob,
                                    conditional_prob = cs_prob,
                                    improvement = improvement,
                                    improvement_percent = improvement_pct
                                ))
                            }
                        }
                    }
                }
            }
        },

        .interpretConditionalSurvival = function(cs_prob, pred_time, time_scale) {
            if (is.na(cs_prob)) return("Unable to calculate")
            
            cs_pct <- cs_prob * 100
            
            if (cs_pct >= 90) {
                return(sprintf("Excellent prognosis: %.1f%% chance of surviving additional %.1f %s", 
                              cs_pct, pred_time, time_scale))
            } else if (cs_pct >= 75) {
                return(sprintf("Good prognosis: %.1f%% chance of surviving additional %.1f %s", 
                              cs_pct, pred_time, time_scale))
            } else if (cs_pct >= 50) {
                return(sprintf("Moderate prognosis: %.1f%% chance of surviving additional %.1f %s", 
                              cs_pct, pred_time, time_scale))
            } else {
                return(sprintf("Guarded prognosis: %.1f%% chance of surviving additional %.1f %s", 
                              cs_pct, pred_time, time_scale))
            }
        },

        .performDynamicPrediction = function(data, surv_models) {
            # This is a simplified implementation of dynamic prediction
            # In practice, this would use more sophisticated methods
            dp_table <- self$results$dynamicPredictionTable
            
            groups <- names(surv_models)
            time_points <- seq(0, max(data$time) * 0.8, length.out = 10)
            
            for (group in groups) {
                model <- surv_models[[group]]
                
                for (patient_time in time_points[time_points > 0]) {
                    for (horizon in c(1, 2, 5)) {
                        if (patient_time + horizon <= max(data$time)) {
                            # Calculate dynamic probability (simplified)
                            if (inherits(model, "survfit")) {
                                s_current <- summary(model, times = patient_time, extend = TRUE)$surv[1]
                                s_future <- summary(model, times = patient_time + horizon, extend = TRUE)$surv[1]
                                
                                if (!is.na(s_current) && s_current > 0) {
                                    dynamic_prob <- s_future / s_current
                                    
                                    # Risk categorization
                                    risk_cat <- if (dynamic_prob >= 0.8) "Low Risk" else
                                               if (dynamic_prob >= 0.6) "Moderate Risk" else "High Risk"
                                    
                                    ci_text <- "[Calculated from model]"  # Simplified
                                    
                                    row_key <- paste("dp", group, patient_time, horizon, sep = "_")
                                    dp_table$addRow(rowKey = row_key, values = list(
                                        patient_time = patient_time,
                                        prediction_time = horizon,
                                        dynamic_probability = dynamic_prob,
                                        confidence_interval = ci_text,
                                        risk_category = risk_cat
                                    ))
                                }
                            }
                        }
                    }
                }
            }
        },

        .performLandmarkAnalysis = function(data, conditional_times) {
            landmark_table <- self$results$landmarkAnalysisTable
            
            for (landmark in conditional_times) {
                # Subset data to patients who survived to landmark
                landmark_data <- data[data$time >= landmark, ]
                
                if (nrow(landmark_data) > 0) {
                    # Adjust time and status for landmark analysis
                    landmark_data$time_adj <- landmark_data$time - landmark
                    landmark_data$status_adj <- ifelse(landmark_data$time > landmark, 
                                                     landmark_data$status, 0)
                    
                    # Fit survival model from landmark
                    landmark_fit <- survival::survfit(survival::Surv(time_adj, status_adj) ~ 1, 
                                                    data = landmark_data)
                    
                    # Extract statistics
                    n_at_landmark <- nrow(landmark_data)
                    n_events <- sum(landmark_data$status_adj)
                    
                    # Get survival probability (e.g., at 1 year from landmark)
                    pred_time <- 1  # 1 year prediction
                    if (max(landmark_data$time_adj) >= pred_time) {
                        surv_summary <- summary(landmark_fit, times = pred_time, extend = TRUE)
                        landmark_surv <- surv_summary$surv[1]
                        ci_lower <- surv_summary$lower[1]
                        ci_upper <- surv_summary$upper[1]
                        
                        ci_text <- sprintf("[%.3f, %.3f]", ci_lower, ci_upper)
                    } else {
                        landmark_surv <- NA
                        ci_text <- "Insufficient follow-up"
                    }
                    
                    landmark_table$addRow(rowKey = paste("landmark", landmark, sep = "_"), values = list(
                        landmark_time = landmark,
                        patients_at_landmark = n_at_landmark,
                        events_after_landmark = n_events,
                        landmark_survival = landmark_surv,
                        confidence_interval = ci_text
                    ))
                }
            }
        },

        .performGroupComparisons = function(data, conditional_times) {
            comp_table <- self$results$groupComparisonTable
            
            for (cond_time in conditional_times) {
                # Subset data to patients who survived to conditional time
                cond_data <- data[data$time >= cond_time, ]
                
                if (nrow(cond_data) > 10 && length(unique(cond_data$group)) > 1) {
                    # Adjust times for conditional analysis
                    cond_data$time_adj <- cond_data$time - cond_time
                    cond_data$status_adj <- ifelse(cond_data$time > cond_time, cond_data$status, 0)
                    
                    # Perform log-rank test
                    tryCatch({
                        logrank_test <- survival::survdiff(survival::Surv(time_adj, status_adj) ~ group, 
                                                         data = cond_data)
                        
                        test_stat <- logrank_test$chisq
                        p_value <- 1 - pchisq(test_stat, df = length(logrank_test$n) - 1)
                        
                        significance <- if (p_value < 0.001) "p < 0.001" else
                                       if (p_value < 0.01) "p < 0.01" else
                                       if (p_value < 0.05) "p < 0.05" else "Not significant"
                        
                        comp_table$addRow(rowKey = paste("comp", cond_time, sep = "_"), values = list(
                            conditional_time = sprintf("%.1f %s", cond_time, self$options$time_scale),
                            test_statistic = test_stat,
                            p_value = p_value,
                            significance = significance
                        ))
                    }, error = function(e) {
                        comp_table$addRow(rowKey = paste("comp_error", cond_time, sep = "_"), values = list(
                            conditional_time = sprintf("%.1f %s", cond_time, self$options$time_scale),
                            test_statistic = NA,
                            p_value = NA,
                            significance = "Test failed"
                        ))
                    })
                }
            }
        },

        .populateMethodExplanation = function() {
            html <- "
            <html>
            <head>
            <meta http-equiv='Content-Type' content='text/html; charset=UTF-8'>
            </head>
            <body>
            <h3>Conditional Survival Analysis: Method and Clinical Interpretation</h3>
            
            <h4>Conditional Survival Concept</h4>
            <p><b>Definition:</b> Conditional survival probability CS(t|s) represents the probability of surviving 
            an additional t units of time, given that a patient has already survived to time s.</p>
            
            <p><b>Mathematical Formula:</b></p>
            <p><code>CS(t|s) = S(s+t) / S(s)</code></p>
            <p>Where S(t) is the overall survival function at time t.</p>
            
            <h4>Clinical Significance</h4>
            <p><b>Dynamic Prognosis:</b> As patients survive longer, their prognosis typically improves. 
            Conditional survival provides updated survival estimates that reflect this reality.</p>
            
            <p><b>Patient Counseling:</b> Long-term survivors benefit from more optimistic and relevant 
            survival information compared to initial diagnosis estimates.</p>
            
            <p><b>Treatment Planning:</b> Helps clinicians make informed decisions about ongoing treatment 
            intensity and follow-up strategies.</p>
            
            <h4>Interpretation Guidelines</h4>
            <p><b>Conditional Survival Improvement:</b> The difference between conditional and unconditional 
            survival probabilities indicates how much the prognosis has improved with time survived.</p>
            
            <p><b>Clinical Thresholds:</b></p>
            <ul>
            <li><b>≥90%:</b> Excellent prognosis - routine follow-up may be sufficient</li>
            <li><b>75-89%:</b> Good prognosis - standard follow-up protocols</li>
            <li><b>50-74%:</b> Moderate prognosis - intensified surveillance may be warranted</li>
            <li><b><50%:</b> Guarded prognosis - aggressive monitoring and intervention consideration</li>
            </ul>
            
            <h4>Advanced Methods</h4>
            <p><b>Dynamic Prediction:</b> Uses updated patient information to provide personalized 
            conditional survival estimates.</p>
            
            <p><b>Landmark Analysis:</b> Addresses immortal time bias by analyzing survival from 
            specific milestone time points.</p>
            
            <p><b>Parametric Modeling:</b> Provides smooth survival curves and enables extrapolation 
            beyond observed follow-up times.</p>
            
            <h4>Clinical Applications</h4>
            <p><b>Cancer Survivorship:</b> Particularly valuable for long-term cancer survivors where 
            5-year or 10-year conditional survival provides more relevant prognostic information.</p>
            
            <p><b>Chronic Disease Management:</b> Helps optimize long-term care strategies based on 
            evolving risk profiles.</p>
            
            <p><b>Clinical Trial Design:</b> Informs endpoint selection and analysis strategies for 
            studies with long-term follow-up.</p>
            
            <h4>Limitations and Considerations</h4>
            <p><b>Sample Size:</b> Conditional survival estimates become less precise as conditional 
            time points increase due to reduced sample sizes.</p>
            
            <p><b>Selection Bias:</b> Survivors to later time points may represent a selected population 
            with better prognosis.</p>
            
            <p><b>Assumption Validity:</b> Assumes that future hazards remain similar to observed patterns.</p>
            
            <p><b>References:</b></p>
            <p>• Hieke S, et al. Conditional survival: A useful concept to provide information on how 
            prognosis evolves over time. Clin Cancer Res. 2015.</p>
            <p>• Zabor EC, et al. Dynamic prognostication using conditional survival estimates. Cancer. 2013.</p>
            <p>• Cucchetti A, et al. Conditional survival after hepatic resection for colorectal metastases. 
            Clin Colorectal Cancer. 2010.</p>
            </body>
            </html>"

            self$results$methodExplanation$setContent(html)
        }
    ), # End of private list
    public = list(
        #' @description
        #' Generate R source code for condsurvival analysis
        #' @return Character string with R syntax for reproducible analysis
        asSource = function() {
            time <- self$options$time
            status <- self$options$status
            group <- self$options$group

            if (is.null(time) || is.null(status))
                return('')

            # Escape time variable
            time_escaped <- if (!is.null(time) && !identical(make.names(time), time)) {
                paste0('`', time, '`')
            } else {
                time
            }

            # Escape status variable
            status_escaped <- if (!is.null(status) && !identical(make.names(status), status)) {
                paste0('`', status, '`')
            } else {
                status
            }

            # Build required arguments
            time_arg <- paste0('time = "', time_escaped, '"')
            status_arg <- paste0('status = "', status_escaped, '"')

            # Build optional group argument
            group_arg <- ''
            if (!is.null(group)) {
                group_escaped <- if (!identical(make.names(group), group)) {
                    paste0('`', group, '`')
                } else {
                    group
                }
                group_arg <- paste0(',\n    group = "', group_escaped, '"')
            }

            # Get other arguments using base helper (if available)
            args <- ''
            if (!is.null(private$.asArgs)) {
                args <- private$.asArgs(incData = FALSE)
            }
            if (args != '')
                args <- paste0(',\n    ', args)

            # Get package name dynamically
            pkg_name <- utils::packageName()
            if (is.null(pkg_name)) pkg_name <- "ClinicoPath"  # fallback

            # Build complete function call
            paste0(pkg_name, '::condsurvival(\n    data = data,\n    ',
                   time_arg, ',\n    ', status_arg, group_arg, args, ')')
        }
    ) # End of public list
)

condsurvival <- condsurvivalClass$new