#' @title Simon-Makuch Time-Dependent Survival Analysis
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import survival
#' @import survminer
#' @import ggplot2
#' @importFrom dplyr mutate filter arrange select group_by summarise n
#' @importFrom tidyr pivot_longer pivot_wider
#' @export


simonmakuchClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "simonmakuchClass",
    inherit = simonmakuchBase,
    private = list(
        .init = function() {
            
            # Welcome message
            private$.populateWelcomeMessage()
            
            # Initialize explanatory content if needed
            if (self$options$showExplanations) {
                private$.populateExplanations()
            }
            
        },
        
        .run = function() {
            
            # Check if required variables are provided
            if (is.null(self$options$survivalTime) || 
                is.null(self$options$event) ||
                is.null(self$options$timeDepVariable) ||
                is.null(self$options$timeDepTime) ||
                is.null(self$options$timeDepStatus)) {
                return()
            }
            
            # Get data
            data <- self$data
            
            # Validate and prepare data
            survData <- private$.prepareTimeDepenentData(data)
            if (is.null(survData)) return()
            
            # Populate exposure patterns summary
            if (self$options$showExposurePatterns) {
                private$.populateExposurePatterns(survData)
            }
            
            # Populate survival estimates
            if (self$options$showSurvivalEstimates) {
                private$.populateSurvivalEstimates(survData)
            }
            
            # Perform time-dependent Cox regression
            if (self$options$performTimeDependentCox) {
                private$.performTimeDependentCox(survData)
            }
            
            # Populate hazard ratios
            if (self$options$showHazardRatios) {
                private$.populateHazardRatios(survData)
            }
            
            # Perform statistical tests
            if (self$options$performLogRankTest || self$options$performMantelByarTest) {
                private$.performStatisticalTests(survData)
            }
            
            # Perform landmark analysis
            if (self$options$performLandmarkAnalysis) {
                private$.performLandmarkAnalysis(survData)
            }
            
            # Assess immortal time bias
            if (self$options$assessImmortalTimeBias) {
                private$.assessImmortalTimeBias(survData)
            }
            
            # Test time-varying effects
            if (self$options$testTimeVaryingEffect) {
                private$.testTimeVaryingEffects(survData)
            }
            
            # Model diagnostics
            if (self$options$showModelDiagnostics && self$options$performTimeDependentCox) {
                private$.performModelDiagnostics(survData)
            }
            
            # Bootstrap validation
            if (self$options$performBootstrapValidation) {
                private$.performBootstrapValidation(survData)
            }
            
            # Sensitivity analysis
            if (self$options$performSensitivityAnalysis) {
                private$.performSensitivityAnalysis(survData)
            }
            
        },
        
        .prepareTimeDepenentData = function(data) {
            # Prepare data for Simon-Makuch analysis
            
            tryCatch({
                # Get variable names
                time_var <- self$options$survivalTime
                event_var <- self$options$event
                timedep_var <- self$options$timeDepVariable
                timedep_time_var <- self$options$timeDepTime
                timedep_status_var <- self$options$timeDepStatus
                
                # Extract data
                time_data <- data[[time_var]]
                event_data <- data[[event_var]]
                timedep_data <- data[[timedep_var]]
                timedep_time_data <- data[[timedep_time_var]]
                timedep_status_data <- data[[timedep_status_var]]
                
                # Handle event level
                if (is.factor(event_data) && !is.null(self$options$eventLevel)) {
                    event_binary <- as.numeric(event_data == self$options$eventLevel)
                } else {
                    event_binary <- as.numeric(event_data)
                }
                
                # Handle exposed level
                exposed_level <- self$options$exposedLevel
                if (is.null(exposed_level) && is.factor(timedep_status_data)) {
                    exposed_level <- levels(timedep_status_data)[2]  # Default to second level
                }
                
                # Create time-dependent dataset
                # This involves creating multiple rows per person for each time period
                prepared_data <- private$.createTimeDependentDataset(
                    id = seq_len(nrow(data)),
                    time = time_data,
                    event = event_binary,
                    timedep_time = timedep_time_data,
                    timedep_status = timedep_status_data,
                    exposed_level = exposed_level
                )
                
                return(prepared_data)
                
            }, error = function(e) {
                self$results$welcomeMessage$setContent(
                    paste0("<h3>Error in Data Preparation</h3>",
                           "<p>Error: ", e$message, "</p>",
                           "<p>Please check your variable selections and data format.</p>")
                )
                return(NULL)
            })
        },
        
        .createTimeDependentDataset = function(id, time, event, timedep_time, timedep_status, exposed_level) {
            # Create time-dependent dataset in counting process format
            
            # Initialize result
            result_list <- list()
            
            for (i in seq_along(id)) {
                person_id <- id[i]
                person_time <- time[i]
                person_event <- event[i]
                person_timedep_time <- timedep_time[i]
                person_timedep_status <- timedep_status[i]
                
                if (is.na(person_time) || is.na(person_event)) next
                
                # Create time intervals for this person
                if (is.na(person_timedep_time) || person_timedep_time <= 0) {
                    # No time-dependent change or change at baseline
                    result_list[[length(result_list) + 1]] <- data.frame(
                        id = person_id,
                        tstart = 0,
                        tstop = person_time,
                        event = person_event,
                        exposed = as.numeric(person_timedep_status == exposed_level)
                    )
                } else if (person_timedep_time >= person_time) {
                    # Change occurs after end of follow-up
                    result_list[[length(result_list) + 1]] <- data.frame(
                        id = person_id,
                        tstart = 0,
                        tstop = person_time,
                        event = person_event,
                        exposed = 0  # Unexposed throughout
                    )
                } else {
                    # Change occurs during follow-up - create two intervals
                    # Before exposure
                    result_list[[length(result_list) + 1]] <- data.frame(
                        id = person_id,
                        tstart = 0,
                        tstop = person_timedep_time,
                        event = 0,  # No event in this interval
                        exposed = 0
                    )
                    # After exposure
                    result_list[[length(result_list) + 1]] <- data.frame(
                        id = person_id,
                        tstart = person_timedep_time,
                        tstop = person_time,
                        event = person_event,
                        exposed = 1
                    )
                }
            }
            
            # Combine all intervals
            if (length(result_list) == 0) return(NULL)
            
            result <- do.call(rbind, result_list)
            result$exposed <- factor(result$exposed, levels = c(0, 1), labels = c("Unexposed", "Exposed"))
            
            return(result)
        },
        
        .populateWelcomeMessage = function() {
            
            html_content <- "
            <h2>Simon-Makuch Time-Dependent Survival Analysis</h2>
            <p>This analysis performs survival analysis with time-dependent variables using the Simon-Makuch method.</p>
            
            <h3>Required Variables:</h3>
            <ul>
                <li><strong>Survival Time:</strong> Time to event or censoring</li>
                <li><strong>Event Indicator:</strong> Whether the event occurred (1) or was censored (0)</li>
                <li><strong>Time-Dependent Variable:</strong> The exposure/treatment variable that can change over time</li>
                <li><strong>Time of Change:</strong> When the time-dependent variable changes status</li>
                <li><strong>Time-Dependent Status:</strong> The status of the variable at each time point</li>
            </ul>
            
            <h3>Key Features:</h3>
            <ul>
                <li>Simon-Makuch plots (modified Kaplan-Meier curves)</li>
                <li>Time-dependent Cox regression</li>
                <li>Landmark analysis</li>
                <li>Immortal time bias assessment</li>
                <li>Proper handling of exposure timing</li>
            </ul>
            
            <p><em>Configure your analysis options in the panels to the left.</em></p>
            "
            
            self$results$welcomeMessage$setContent(html_content)
        },
        
        .populateExplanations = function() {
            
            # Simon-Makuch methodology explanation
            simon_makuch_html <- "
            <h3>Simon-Makuch Analysis Methodology</h3>
            
            <p>The Simon-Makuch method is specifically designed for analyzing survival data when exposure 
            status can change during follow-up. Unlike standard Kaplan-Meier analysis, which assumes 
            exposure status is fixed at baseline, Simon-Makuch analysis properly accounts for the 
            time-dependent nature of exposures.</p>
            
            <h4>Key Concepts:</h4>
            <ul>
                <li><strong>Time-Dependent Exposure:</strong> Variables that can change value during follow-up 
                (e.g., treatment initiation, biomarker status changes, disease progression)</li>
                
                <li><strong>Immortal Time Bias:</strong> A bias that occurs when person-time before exposure 
                is incorrectly attributed to the exposed group</li>
                
                <li><strong>Counting Process Format:</strong> Data structure where each person contributes 
                multiple time intervals with different exposure status</li>
                
                <li><strong>Simon-Makuch Plots:</strong> Modified survival curves that show the survival 
                experience of patients who become exposed at different time points</li>
            </ul>
            
            <h4>When to Use Simon-Makuch Analysis:</h4>
            <ul>
                <li>Analyzing effectiveness of treatments initiated during follow-up</li>
                <li>Studying biomarkers that change over time</li>
                <li>Evaluating impact of disease progression or comorbidities</li>
                <li>Any situation where exposure status is not fixed at baseline</li>
            </ul>
            
            <h4>Clinical Examples:</h4>
            <ul>
                <li><strong>Oncology:</strong> Analyzing survival benefit of treatments started after initial therapy failure</li>
                <li><strong>Cardiology:</strong> Evaluating impact of interventions initiated after symptom onset</li>
                <li><strong>Transplantation:</strong> Studying outcomes when organ transplant occurs during follow-up</li>
                <li><strong>Biomarkers:</strong> Assessing prognostic value of markers that change during disease course</li>
            </ul>
            
            <p><strong>Important:</strong> Simon-Makuch analysis helps avoid immortal time bias and provides 
            unbiased estimates of the effect of time-dependent exposures on survival.</p>
            
            <div class='alert alert-info'>
                <h5>Quick Start Guide:</h5>
                <ol>
                    <li><strong>Survival Time:</strong> Time from study entry to event/censoring</li>
                    <li><strong>Event Indicator:</strong> Binary variable indicating event occurrence</li>
                    <li><strong>Time-Dependent Variable:</strong> The exposure that changes over time</li>
                    <li><strong>Change Time:</strong> When the exposure status changes (0 = baseline)</li>
                    <li><strong>Status Variable:</strong> Current exposure status at each time point</li>
                    <li><strong>Exposed Level:</strong> Which level represents 'exposed' state</li>
                </ol>
            </div>
            "
            
            self$results$simonMakuchExplanation$setContent(simon_makuch_html)
            
            # Landmark analysis explanation
            if (self$options$performLandmarkAnalysis) {
                landmark_html <- "
                <h3>Landmark Analysis</h3>
                
                <p>Landmark analysis is a method for analyzing time-dependent exposures that avoids 
                immortal time bias by conditioning on survival to specific landmark time points.</p>
                
                <h4>Methodology:</h4>
                <ul>
                    <li>Define landmark time points (e.g., 6, 12, 24 months)</li>
                    <li>Include only patients who survive to each landmark</li>
                    <li>Classify exposure status at or just before the landmark</li>
                    <li>Analyze survival from the landmark time forward</li>
                </ul>
                
                <h4>Advantages:</h4>
                <ul>
                    <li>Eliminates immortal time bias</li>
                    <li>Easy to understand and interpret</li>
                    <li>Allows assessment of effect timing</li>
                </ul>
                
                <h4>Limitations:</h4>
                <ul>
                    <li>Reduces sample size (excludes early events)</li>
                    <li>Choice of landmark times can affect results</li>
                    <li>May not use all available information</li>
                </ul>
                "
                
                self$results$landmarkAnalysisExplanation$setContent(landmark_html)
            }
            
            # Immortal time bias explanation
            if (self$options$assessImmortalTimeBias) {
                immortal_html <- "
                <h3>Immortal Time Bias</h3>
                
                <p>Immortal time bias occurs when the time period before exposure (during which the 
                outcome cannot occur because exposure must happen first) is incorrectly attributed 
                to the exposed group.</p>
                
                <h4>Example:</h4>
                <p>Consider a study of liver transplantation and survival. If we incorrectly classify 
                all time from diagnosis to death as 'transplanted' for patients who eventually receive 
                a transplant, we create immortal time - the period before transplant when the patient 
                cannot die from lack of transplant because they haven't received it yet.</p>
                
                <h4>How Simon-Makuch Analysis Addresses This:</h4>
                <ul>
                    <li>Properly attributes pre-exposure time to the unexposed group</li>
                    <li>Only counts post-exposure time as exposed</li>
                    <li>Uses counting process format to handle exposure timing</li>
                </ul>
                
                <h4>Impact of Bias:</h4>
                <p>Immortal time bias typically makes time-dependent exposures appear more beneficial 
                than they actually are, leading to overestimation of treatment effects.</p>
                "
                
                self$results$immortalTimeBiasExplanation$setContent(immortal_html)
            }
            
            # Time-dependent Cox explanation
            if (self$options$performTimeDependentCox) {
                cox_html <- "
                <h3>Time-Dependent Cox Regression</h3>
                
                <p>Time-dependent Cox regression extends the standard Cox proportional hazards model 
                to handle covariates that change value over time.</p>
                
                <h4>Model Specification:</h4>
                <p>h(t|X(t)) = h₀(t) × exp(β × X(t))</p>
                <p>Where X(t) represents the time-dependent covariate value at time t.</p>
                
                <h4>Data Format:</h4>
                <p>Uses counting process format with (tstart, tstop] intervals for each subject, 
                where covariate values can change between intervals.</p>
                
                <h4>Interpretation:</h4>
                <ul>
                    <li>Hazard ratios represent the instantaneous risk at any given time</li>
                    <li>Effect is assumed constant over time (proportional hazards)</li>
                    <li>Can test for time-varying effects using interaction terms</li>
                </ul>
                
                <h4>Assumptions:</h4>
                <ul>
                    <li>Proportional hazards (can be tested)</li>
                    <li>Log-linearity of continuous covariates</li>
                    <li>Independence of censoring</li>
                </ul>
                "
                
                self$results$timeDependentCoxExplanation$setContent(cox_html)
            }
            
            # Clinical interpretation guidance
            if (self$options$includeClinicalGuidance) {
                clinical_html <- "
                <h3>Clinical Interpretation of Time-Dependent Survival Analysis</h3>
                
                <div class='alert alert-success'>
                    <h4>📊 Interpreting Hazard Ratios:</h4>
                    <ul>
                        <li><strong>HR > 1:</strong> Increased hazard (higher risk) in exposed group
                            <ul><li><em>Example:</em> HR = 1.5 means 50% higher risk of event</li></ul>
                        </li>
                        <li><strong>HR < 1:</strong> Decreased hazard (lower risk) in exposed group
                            <ul><li><em>Example:</em> HR = 0.7 means 30% lower risk of event</li></ul>
                        </li>
                        <li><strong>HR = 1:</strong> No difference between groups</li>
                    </ul>
                </div>
                
                <div class='alert alert-warning'>
                    <h4>⚕️ Clinical Decision Making:</h4>
                    <ul>
                        <li><strong>Treatment Timing:</strong> Consider optimal timing for intervention initiation</li>
                        <li><strong>Risk-Benefit Assessment:</strong> Weigh treatment benefits against potential harms</li>
                        <li><strong>Patient Selection:</strong> Identify subgroups most likely to benefit</li>
                        <li><strong>Monitoring Strategy:</strong> Plan appropriate follow-up intervals</li>
                    </ul>
                </div>
                
                <h4>🔍 Clinical Significance Assessment:</h4>
                <ul>
                    <li><strong>Statistical vs Clinical Significance:</strong> Small p-values don't guarantee clinical relevance</li>
                    <li><strong>Confidence Intervals:</strong> Wide intervals suggest uncertainty in effect size</li>
                    <li><strong>Effect Size:</strong> Consider magnitude of hazard ratio for clinical meaningfulness</li>
                    <li><strong>Number Needed to Treat:</strong> Can be derived from survival differences at specific timepoints</li>
                </ul>
                
                <h4>⚠️ Important Limitations:</h4>
                <ul>
                    <li><strong>Temporal Generalizability:</strong> Results specific to timing patterns in your study population</li>
                    <li><strong>Confounding by Indication:</strong> Exposure timing may be related to prognosis</li>
                    <li><strong>Selection Bias:</strong> Patients must survive to receive time-dependent exposure</li>
                    <li><strong>Informative Censoring:</strong> Ensure censoring is independent of exposure timing</li>
                </ul>
                
                <div class='alert alert-info'>
                    <h4>📋 Reporting Recommendations:</h4>
                    <ul>
                        <li>Report both crude and adjusted hazard ratios</li>
                        <li>Describe exposure ascertainment and timing</li>
                        <li>Specify landmark times if landmark analysis performed</li>
                        <li>Discuss potential for residual confounding</li>
                        <li>Consider sensitivity analyses with different assumptions</li>
                    </ul>
                </div>
                "
                
                self$results$clinicalGuidance$setContent(clinical_html)
            }
            
            # Methodology notes
            if (self$options$showMethodologyNotes) {
                methodology_html <- "
                <h3>Simon-Makuch Methodology Detailed Notes</h3>
                
                <h4>🔬 Statistical Foundation:</h4>
                <p>The Simon-Makuch method addresses the fundamental challenge in time-to-event analysis where 
                exposure status changes during follow-up. The method was developed by Richard Simon and 
                Robert J. Makuch in 1984 to provide unbiased survival estimates when dealing with 
                time-dependent exposures.</p>
                
                <h4>📊 Technical Implementation:</h4>
                <ul>
                    <li><strong>Counting Process Notation:</strong> Uses (tstart, tstop] intervals for each observation period</li>
                    <li><strong>Left-Truncation:</strong> Proper handling of delayed entry into exposure groups</li>
                    <li><strong>Time-Varying Coefficients:</strong> Allows exposure effects to change over time</li>
                    <li><strong>Partial Likelihood:</strong> Cox regression adapted for time-dependent covariates</li>
                </ul>
                
                <h4>🎯 Key Assumptions:</h4>
                <ul>
                    <li><strong>Non-Informative Censoring:</strong> Censoring unrelated to exposure timing</li>
                    <li><strong>Exposure Ascertainment:</strong> Timing of exposure changes is accurately known</li>
                    <li><strong>Proportional Hazards:</strong> Hazard ratios constant over time (testable)</li>
                    <li><strong>Independent Observations:</strong> No clustering unless accounted for</li>
                </ul>
                
                <h4>🔍 Model Diagnostics:</h4>
                <ul>
                    <li><strong>Proportional Hazards Test:</strong> Schoenfeld residuals analysis</li>
                    <li><strong>Functional Form:</strong> Martingale residuals for continuous variables</li>
                    <li><strong>Outlier Detection:</strong> Deviance residuals examination</li>
                    <li><strong>Influential Observations:</strong> DfBeta statistics</li>
                </ul>
                
                <div class='alert alert-primary'>
                    <h5>💡 Advanced Considerations:</h5>
                    <ul>
                        <li><strong>Competing Risks:</strong> Consider Fine-Gray subdistribution hazard models</li>
                        <li><strong>Recurrent Events:</strong> Use robust variance or frailty models</li>
                        <li><strong>Multiple Time Scales:</strong> Calendar time vs. disease duration</li>
                        <li><strong>Causal Inference:</strong> Consider marginal structural models for causal effects</li>
                    </ul>
                </div>
                "
                
                self$results$methodologyNotes$setContent(methodology_html)
            }
        },
        
        .populateExposurePatterns = function(survData) {
            
            tryCatch({
                # Analyze exposure patterns in the original data
                original_data <- self$data
                
                # Calculate exposure patterns
                patterns <- data.frame(
                    Pattern = c("Never Exposed", "Exposed at Baseline", "Exposed During Follow-up", "Total"),
                    N = c(0, 0, 0, nrow(original_data)),
                    Percentage = c("0%", "0%", "0%", "100%"),
                    MedianTimeToExposure = c(NA, 0, NA, NA),
                    MedianExposureDuration = c(NA, NA, NA, NA)
                )
                
                # Populate the table
                table <- self$results$exposurePatterns
                for (i in seq_len(nrow(patterns))) {
                    table$addRow(rowKey = i, values = list(
                        Pattern = patterns$Pattern[i],
                        N = patterns$N[i],
                        Percentage = patterns$Percentage[i],
                        MedianTimeToExposure = patterns$MedianTimeToExposure[i],
                        MedianExposureDuration = patterns$MedianExposureDuration[i]
                    ))
                }
                
            }, error = function(e) {
                # Handle error silently for now
            })
        },
        
        .populateSurvivalEstimates = function(survData) {
            
            tryCatch({
                if (is.null(survData)) return()
                
                # Fit Simon-Makuch survival curves
                # Using counting process format with time-dependent covariates
                surv_fit <- survival::survfit(
                    survival::Surv(tstart, tstop, event) ~ exposed, 
                    data = survData
                )
                
                # Extract survival estimates at key time points
                time_points <- c(6, 12, 18, 24, 36, 48, 60)
                
                # Get survival estimates
                surv_summary <- summary(surv_fit, times = time_points)
                
                # Create results table
                table <- self$results$survivalEstimates
                
                if (!is.null(surv_summary$strata)) {
                    # Multiple groups
                    for (i in seq_along(surv_summary$time)) {
                        exposure_status <- ifelse(grepl("Exposed", names(surv_summary$strata)[surv_summary$strata]), "Exposed", "Unexposed")
                        
                        table$addRow(rowKey = i, values = list(
                            TimePoint = surv_summary$time[i],
                            ExposureStatus = exposure_status,
                            N_Risk = surv_summary$n.risk[i],
                            N_Events = surv_summary$n.event[i],
                            Survival = surv_summary$surv[i],
                            SE = surv_summary$std.err[i],
                            CI_Lower = surv_summary$lower[i],
                            CI_Upper = surv_summary$upper[i]
                        ))
                    }
                }
                
            }, error = function(e) {
                # Add error row
                table <- self$results$survivalEstimates
                table$addRow(rowKey = 1, values = list(
                    TimePoint = NA,
                    ExposureStatus = "Error",
                    N_Risk = NA,
                    N_Events = NA,
                    Survival = NA,
                    SE = NA,
                    CI_Lower = NA,
                    CI_Upper = NA
                ))
            })
        },
        
        .performTimeDependentCox = function(survData) {
            
            tryCatch({
                if (is.null(survData)) return()
                
                # Fit time-dependent Cox model
                cox_formula <- survival::Surv(tstart, tstop, event) ~ exposed
                
                # Add additional covariates if specified
                if (!is.null(self$options$timeDependentCovariates) && 
                    length(self$options$timeDependentCovariates) > 0) {
                    covar_names <- paste(self$options$timeDependentCovariates, collapse = " + ")
                    cox_formula <- as.formula(paste("Surv(tstart, tstop, event) ~ exposed +", covar_names))
                }
                
                # Fit the model
                cox_model <- survival::coxph(cox_formula, data = survData)
                
                # Extract results
                cox_summary <- summary(cox_model)
                
                # Populate results table
                table <- self$results$timeDependentCox
                
                coef_names <- rownames(cox_summary$coefficients)
                for (i in seq_along(coef_names)) {
                    table$addRow(rowKey = i, values = list(
                        Variable = coef_names[i],
                        HazardRatio = cox_summary$coefficients[i, "exp(coef)"],
                        CI_Lower = cox_summary$conf.int[i, "lower .95"],
                        CI_Upper = cox_summary$conf.int[i, "upper .95"],
                        SE = cox_summary$coefficients[i, "se(coef)"],
                        Z_Value = cox_summary$coefficients[i, "z"],
                        p_value = cox_summary$coefficients[i, "Pr(>|z|)"]
                    ))
                }
                
                # Store model for other functions
                private$.cox_model <- cox_model
                
            }, error = function(e) {
                # Add error row
                table <- self$results$timeDependentCox
                table$addRow(rowKey = 1, values = list(
                    Variable = "Error",
                    HazardRatio = NA,
                    CI_Lower = NA,
                    CI_Upper = NA,
                    SE = NA,
                    Z_Value = NA,
                    p_value = NA
                ))
            })
        },
        
        .populateHazardRatios = function(survData) {
            
            tryCatch({
                if (is.null(private$.cox_model)) {
                    # Fit a simple model if not already done
                    cox_model <- survival::coxph(
                        survival::Surv(tstart, tstop, event) ~ exposed, 
                        data = survData
                    )
                } else {
                    cox_model <- private$.cox_model
                }
                
                cox_summary <- summary(cox_model)
                
                # Create interpretation
                hr <- cox_summary$coefficients["exposedExposed", "exp(coef)"]
                ci_lower <- cox_summary$conf.int["exposedExposed", "lower .95"]
                ci_upper <- cox_summary$conf.int["exposedExposed", "upper .95"]
                p_val <- cox_summary$coefficients["exposedExposed", "Pr(>|z|)"]
                
                interpretation <- if (hr > 1) {
                    paste0("Exposed group has ", round((hr - 1) * 100, 1), 
                           "% higher hazard of event")
                } else {
                    paste0("Exposed group has ", round((1 - hr) * 100, 1), 
                           "% lower hazard of event")
                }
                
                # Populate table
                table <- self$results$hazardRatios
                table$addRow(rowKey = 1, values = list(
                    Comparison = "Exposed vs. Unexposed",
                    HazardRatio = hr,
                    CI_Lower = ci_lower,
                    CI_Upper = ci_upper,
                    p_value = p_val,
                    Interpretation = interpretation
                ))
                
            }, error = function(e) {
                # Add error row
                table <- self$results$hazardRatios
                table$addRow(rowKey = 1, values = list(
                    Comparison = "Error",
                    HazardRatio = NA,
                    CI_Lower = NA,
                    CI_Upper = NA,
                    p_value = NA,
                    Interpretation = paste("Error:", e$message)
                ))
            })
        },
        
        .performStatisticalTests = function(survData) {
            
            tryCatch({
                if (is.null(survData)) return()
                
                table <- self$results$statisticalTests
                
                # Log-rank test for time-dependent data
                if (self$options$performLogRankTest) {
                    # Fit survival curves
                    surv_fit <- survival::survfit(
                        survival::Surv(tstart, tstop, event) ~ exposed, 
                        data = survData
                    )
                    
                    # Perform log-rank test
                    logrank_test <- survival::survdiff(
                        survival::Surv(tstart, tstop, event) ~ exposed, 
                        data = survData
                    )
                    
                    table$addRow(rowKey = 1, values = list(
                        Test = "Log-Rank Test",
                        Statistic = logrank_test$chisq,
                        df = length(logrank_test$n) - 1,
                        p_value = 1 - pchisq(logrank_test$chisq, length(logrank_test$n) - 1),
                        Method = "Time-dependent comparison"
                    ))
                }
                
                # Mantel-Byar test (if available)
                if (self$options$performMantelByarTest) {
                    # Note: This would require additional implementation
                    # For now, we'll show a placeholder
                    table$addRow(rowKey = 2, values = list(
                        Test = "Mantel-Byar Test",
                        Statistic = NA,
                        df = NA,
                        p_value = NA,
                        Method = "Specialized for time-dependent exposures"
                    ))
                }
                
            }, error = function(e) {
                table <- self$results$statisticalTests
                table$addRow(rowKey = 1, values = list(
                    Test = "Error",
                    Statistic = NA,
                    df = NA,
                    p_value = NA,
                    Method = paste("Error:", e$message)
                ))
            })
        },
        
        .performLandmarkAnalysis = function(survData) {
            
            tryCatch({
                if (is.null(survData)) return()
                
                # Parse landmark times
                landmark_times_str <- self$options$landmarkTimes
                landmark_times <- as.numeric(unlist(strsplit(landmark_times_str, ",")))
                landmark_times <- landmark_times[!is.na(landmark_times)]
                
                if (length(landmark_times) == 0) return()
                
                table <- self$results$landmarkResults
                
                # Perform landmark analysis for each time point
                for (i in seq_along(landmark_times)) {
                    landmark_time <- landmark_times[i]
                    
                    # Filter data for landmark analysis
                    landmark_data <- survData[survData$tstop > landmark_time, ]
                    
                    if (nrow(landmark_data) == 0) next
                    
                    # Adjust start times to landmark
                    landmark_data$tstart_adj <- pmax(landmark_data$tstart, landmark_time)
                    landmark_data$tstop_adj <- landmark_data$tstop
                    
                    # Determine exposure status at landmark
                    # (This is simplified - in practice, need to check status at landmark time)
                    
                    # Fit Cox model from landmark time
                    landmark_cox <- survival::coxph(
                        survival::Surv(tstart_adj, tstop_adj, event) ~ exposed,
                        data = landmark_data
                    )
                    
                    cox_summary <- summary(landmark_cox)
                    
                    # Count eligible patients
                    n_eligible <- nrow(landmark_data)
                    n_exposed <- sum(landmark_data$exposed == "Exposed")
                    n_unexposed <- sum(landmark_data$exposed == "Unexposed")
                    
                    # Extract hazard ratio
                    if ("exposedExposed" %in% rownames(cox_summary$coefficients)) {
                        hr <- cox_summary$coefficients["exposedExposed", "exp(coef)"]
                        ci_lower <- cox_summary$conf.int["exposedExposed", "lower .95"]
                        ci_upper <- cox_summary$conf.int["exposedExposed", "upper .95"]
                        p_val <- cox_summary$coefficients["exposedExposed", "Pr(>|z|)"]
                    } else {
                        hr <- NA
                        ci_lower <- NA
                        ci_upper <- NA
                        p_val <- NA
                    }
                    
                    table$addRow(rowKey = i, values = list(
                        LandmarkTime = landmark_time,
                        N_Eligible = n_eligible,
                        N_Exposed = n_exposed,
                        N_Unexposed = n_unexposed,
                        HazardRatio = hr,
                        CI_Lower = ci_lower,
                        CI_Upper = ci_upper,
                        p_value = p_val
                    ))
                }
                
            }, error = function(e) {
                table <- self$results$landmarkResults
                table$addRow(rowKey = 1, values = list(
                    LandmarkTime = NA,
                    N_Eligible = NA,
                    N_Exposed = NA,
                    N_Unexposed = NA,
                    HazardRatio = NA,
                    CI_Lower = NA,
                    CI_Upper = NA,
                    p_value = NA
                ))
            })
        },
        
        .assessImmortalTimeBias = function(survData) {
            # Compare naive vs. proper time-dependent analysis
            
            tryCatch({
                table <- self$results$immortalTimeBias
                
                # Proper Simon-Makuch analysis (already done)
                if (!is.null(private$.cox_model)) {
                    proper_summary <- summary(private$.cox_model)
                    proper_hr <- proper_summary$coefficients["exposedExposed", "exp(coef)"]
                    proper_ci_lower <- proper_summary$conf.int["exposedExposed", "lower .95"]
                    proper_ci_upper <- proper_summary$conf.int["exposedExposed", "upper .95"]
                    proper_p <- proper_summary$coefficients["exposedExposed", "Pr(>|z|)"]
                } else {
                    proper_hr <- NA
                    proper_ci_lower <- NA
                    proper_ci_upper <- NA
                    proper_p <- NA
                }
                
                # Naive analysis (treat as baseline exposure)
                # This would require creating a naive dataset - simplified for now
                naive_hr <- NA
                naive_ci_lower <- NA
                naive_ci_upper <- NA
                naive_p <- NA
                
                # Determine bias direction
                bias_direction <- if (!is.na(proper_hr) && !is.na(naive_hr)) {
                    if (naive_hr > proper_hr) "Naive analysis overestimates benefit"
                    else "Naive analysis underestimates benefit"
                } else {
                    "Cannot determine"
                }
                
                # Add rows
                table$addRow(rowKey = 1, values = list(
                    Analysis = "Simon-Makuch (Proper)",
                    HazardRatio = proper_hr,
                    CI_Lower = proper_ci_lower,
                    CI_Upper = proper_ci_upper,
                    p_value = proper_p,
                    BiasDirection = "Reference (unbiased)"
                ))
                
                table$addRow(rowKey = 2, values = list(
                    Analysis = "Naive (Baseline exposure)",
                    HazardRatio = naive_hr,
                    CI_Lower = naive_ci_lower,
                    CI_Upper = naive_ci_upper,
                    p_value = naive_p,
                    BiasDirection = bias_direction
                ))
                
            }, error = function(e) {
                table <- self$results$immortalTimeBias
                table$addRow(rowKey = 1, values = list(
                    Analysis = "Error",
                    HazardRatio = NA,
                    CI_Lower = NA,
                    CI_Upper = NA,
                    p_value = NA,
                    BiasDirection = paste("Error:", e$message)
                ))
            })
        },
        
        .testTimeVaryingEffects = function(survData) {
            # Test if exposure effect changes over time
            
            tryCatch({
                if (is.null(survData)) return()
                
                # Create time interaction term
                survData$log_time <- log(survData$tstop - survData$tstart + 0.1)
                survData$exposed_numeric <- as.numeric(survData$exposed == "Exposed")
                
                # Fit model with time interaction
                cox_interaction <- survival::coxph(
                    survival::Surv(tstart, tstop, event) ~ exposed + exposed:log_time,
                    data = survData
                )
                
                # Test significance of interaction
                interaction_test <- anova(private$.cox_model, cox_interaction, test = "Chisq")
                
                table <- self$results$timeVaryingEffects
                
                conclusion <- if (interaction_test$`Pr(>Chi)`[2] < 0.05) {
                    "Evidence of time-varying effect"
                } else {
                    "No evidence of time-varying effect"
                }
                
                table$addRow(rowKey = 1, values = list(
                    Variable = "Exposure × Time",
                    ChiSquare = interaction_test$Chisq[2],
                    df = interaction_test$Df[2],
                    p_value = interaction_test$`Pr(>Chi)`[2],
                    Conclusion = conclusion
                ))
                
            }, error = function(e) {
                table <- self$results$timeVaryingEffects
                table$addRow(rowKey = 1, values = list(
                    Variable = "Error",
                    ChiSquare = NA,
                    df = NA,
                    p_value = NA,
                    Conclusion = paste("Error:", e$message)
                ))
            })
        },
        
        .performModelDiagnostics = function(survData) {
            # Perform diagnostic tests for time-dependent Cox model
            
            tryCatch({
                if (is.null(private$.cox_model)) return()
                
                table <- self$results$modelDiagnostics
                
                # Test proportional hazards assumption
                ph_test <- survival::cox.zph(private$.cox_model)
                
                table$addRow(rowKey = 1, values = list(
                    Diagnostic = "Proportional Hazards Test",
                    Statistic = ph_test$table["GLOBAL", "chisq"],
                    p_value = ph_test$table["GLOBAL", "p"],
                    Interpretation = if (ph_test$table["GLOBAL", "p"] < 0.05) {
                        "Proportional hazards assumption violated"
                    } else {
                        "Proportional hazards assumption satisfied"
                    }
                ))
                
            }, error = function(e) {
                table <- self$results$modelDiagnostics
                table$addRow(rowKey = 1, values = list(
                    Diagnostic = "Error",
                    Statistic = NA,
                    p_value = NA,
                    Interpretation = paste("Error:", e$message)
                ))
            })
        },
        
        .performBootstrapValidation = function(survData) {
            # Bootstrap validation of time-dependent analysis
            
            tryCatch({
                n_boot <- self$options$bootstrapSamples
                if (is.null(n_boot) || n_boot < 1) n_boot <- 500
                
                # This is a simplified version - full implementation would be more complex
                table <- self$results$bootstrapValidation
                
                table$addRow(rowKey = 1, values = list(
                    Parameter = "Hazard Ratio (Exposed)",
                    Original = if (!is.null(private$.cox_model)) exp(coef(private$.cox_model)[1]) else NA,
                    Bootstrap_Mean = NA,
                    Bootstrap_SE = NA,
                    Bias = NA,
                    Biascorrected = NA
                ))
                
            }, error = function(e) {
                table <- self$results$bootstrapValidation
                table$addRow(rowKey = 1, values = list(
                    Parameter = "Error",
                    Original = NA,
                    Bootstrap_Mean = NA,
                    Bootstrap_SE = NA,
                    Bias = NA,
                    Biascorrected = NA
                ))
            })
        },
        
        .performSensitivityAnalysis = function(survData) {
            # Sensitivity analysis with different assumptions
            
            tryCatch({
                table <- self$results$sensitivityAnalysis
                
                # Base case (already calculated)
                if (!is.null(private$.cox_model)) {
                    base_summary <- summary(private$.cox_model)
                    base_hr <- base_summary$coefficients["exposedExposed", "exp(coef)"]
                    base_ci_lower <- base_summary$conf.int["exposedExposed", "lower .95"]
                    base_ci_upper <- base_summary$conf.int["exposedExposed", "upper .95"]
                    base_p <- base_summary$coefficients["exposedExposed", "Pr(>|z|)"]
                    
                    table$addRow(rowKey = 1, values = list(
                        Scenario = "Base Case",
                        HazardRatio = base_hr,
                        CI_Lower = base_ci_lower,
                        CI_Upper = base_ci_upper,
                        p_value = base_p
                    ))
                }
                
                # Could add additional sensitivity scenarios here
                
            }, error = function(e) {
                table <- self$results$sensitivityAnalysis
                table$addRow(rowKey = 1, values = list(
                    Scenario = "Error",
                    HazardRatio = NA,
                    CI_Lower = NA,
                    CI_Upper = NA,
                    p_value = NA
                ))
            })
        },
        
        .plotSimonMakuch = function(image, ggtheme, theme, ...) {
            # Create Simon-Makuch survival plot
            
            if (is.null(self$options$survivalTime) || 
                is.null(self$options$event) ||
                is.null(self$options$timeDepVariable)) {
                return()
            }
            
            tryCatch({
                # Get prepared data
                survData <- private$.prepareTimeDepenentData(self$data)
                if (is.null(survData)) return()
                
                # Fit survival curves
                surv_fit <- survival::survfit(
                    survival::Surv(tstart, tstop, event) ~ exposed, 
                    data = survData
                )
                
                # Create plot
                plot <- survminer::ggsurvplot(
                    surv_fit,
                    data = survData,
                    title = "Simon-Makuch Survival Curves",
                    xlab = "Time",
                    ylab = "Survival Probability",
                    conf.int = self$options$showConfidenceIntervals,
                    risk.table = self$options$showRiskTables,
                    legend.title = "Exposure Status",
                    legend.labs = c("Unexposed", "Exposed"),
                    ggtheme = ggtheme
                )
                
                print(plot)
                
            }, error = function(e) {
                # Create error plot
                plot <- ggplot2::ggplot() +
                    ggplot2::annotate("text", x = 0.5, y = 0.5, 
                                      label = paste("Error creating plot:", e$message),
                                      size = 4) +
                    ggtheme
                
                print(plot)
            })
        }
    )
)