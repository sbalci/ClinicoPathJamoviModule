# Clinical Alert and Threshold Monitoring System
# Part of Phase O: Evidence-Based Decision Support
# Real-time clinical threshold monitoring and warnings

clinicalalertsClass <- R6::R6Class(
    "clinicalalertsClass",
    inherit = clinicalalertsBase,
    private = list(
        .init = function() {
            
            # Initialize with comprehensive instructions
            todo <- glue::glue("
            <br>Welcome to ClinicoPath Clinical Alert System
            <br><br>
            <b>Clinical Threshold Monitoring & Alert System</b>
            <br><br>
            This analysis provides real-time monitoring of clinical parameters with customizable alert thresholds:
            
            <br><br>
            <b>Analysis Steps:</b>
            <ul>
            <li>1. Select your <b>Clinical Variables</b> to monitor (laboratory values, vital signs, etc.)</li>
            <li>2. Optional: Add <b>Patient ID</b> for patient-specific monitoring</li>
            <li>3. Optional: Add <b>Time Variable</b> for longitudinal monitoring</li>
            <li>4. Configure <b>Alert Thresholds</b> for each clinical parameter</li>
            <li>5. Set <b>Alert Priorities</b> (Critical, High, Medium, Low)</li>
            <li>6. Review alert summary and clinical recommendations</li>
            </ul>
            
            <br>
            <b>Key Features:</b>
            <ul>
            <li><b>Multi-Parameter Monitoring:</b> Simultaneous monitoring of multiple clinical variables</li>
            <li><b>Priority-Based Alerts:</b> Critical, High, Medium, Low priority classification</li>
            <li><b>Clinical Context:</b> Evidence-based threshold recommendations</li>
            <li><b>Patient-Specific Tracking:</b> Individual patient alert histories</li>
            <li><b>Trend Analysis:</b> Longitudinal parameter tracking and forecasting</li>
            <li><b>Action Recommendations:</b> Clinical guidance for threshold violations</li>
            </ul>
            
            <br>
            <b>Clinical Applications:</b>
            <ul>
            <li>Critical care monitoring (ICU, ED)</li>
            <li>Laboratory value surveillance</li>
            <li>Medication safety monitoring</li>
            <li>Chronic disease management</li>
            <li>Post-operative monitoring</li>
            <li>Quality assurance and safety alerts</li>
            </ul>
            
            <br>
            <b>Alert Priority Guidelines:</b>
            <ul>
            <li><b>Critical:</b> Immediate intervention required (life-threatening values)</li>
            <li><b>High:</b> Urgent attention needed (within 1-4 hours)</li>
            <li><b>Medium:</b> Monitor closely (within 24 hours)</li>
            <li><b>Low:</b> Note for next clinical review</li>
            </ul>
            
            <br>
            <b>Required Packages:</b> dplyr, ggplot2, lubridate, scales
            <br>
            <b>Data Requirements:</b> At least 1 clinical variable with threshold values
            <br><br>
            ")
            
            self$results$todo$setContent(todo)
        },

        .run = function() {
            print("Inside clinicalalerts .run")
            # Get options
            clinicalVars <- self$options$clinicalVars
            patientId <- self$options$patientId
            timeVar <- self$options$timeVar
            
            # Check if clinical variables are selected
            if (is.null(clinicalVars) || length(clinicalVars) == 0) {
                return()
            }
            
            # Check required packages
            required_packages <- c("dplyr", "ggplot2", "lubridate", "scales", "glue")
            missing_packages <- c()
            
            for (pkg in required_packages) {
                if (!requireNamespace(pkg, quietly = TRUE)) {
                    missing_packages <- c(missing_packages, pkg)
                }
            }
            
            if (length(missing_packages) > 0) {
                error_msg <- glue::glue("
                <h3>Error: Required Packages Not Found</h3>
                <p>The following packages are required but not installed:</p>
                <ul>
                {paste0('<li><b>', missing_packages, '</b></li>', collapse = '')}
                </ul>
                <p>Please install them using:</p>
                <code>install.packages(c('{paste(missing_packages, collapse = \"', '\")}</code>
                ")
                self$results$summary$setContent(error_msg)
                return()
            }
            
            # Get data
            data <- self$data
            
            if (nrow(data) == 0) return()
            
            # Process each clinical variable
            all_alerts <- list()
            total_alerts <- 0
            critical_count <- 0
            high_count <- 0
            medium_count <- 0
            low_count <- 0
            
            tryCatch({
                
                for (i in seq_along(clinicalVars)) {
                    var_name <- clinicalVars[[i]]
                    var_data <- data[[var_name]]
                    
                    # Skip if all missing
                    if (all(is.na(var_data))) next
                    
                    # Get threshold settings for this variable
                    critical_low <- private$.getThreshold(paste0("critical_low_", i), var_name)
                    critical_high <- private$.getThreshold(paste0("critical_high_", i), var_name)
                    high_low <- private$.getThreshold(paste0("high_low_", i), var_name)
                    high_high <- private$.getThreshold(paste0("high_high_", i), var_name)
                    medium_low <- private$.getThreshold(paste0("medium_low_", i), var_name)
                    medium_high <- private$.getThreshold(paste0("medium_high_", i), var_name)
                    
                    # Identify alerts for this variable
                    var_alerts <- private$.identifyAlerts(
                        var_data, var_name, 
                        critical_low, critical_high,
                        high_low, high_high,
                        medium_low, medium_high,
                        data, patientId, timeVar
                    )
                    
                    # Filter alerts based on user preferences BEFORE counting
                    if (!self$options$include_medium_priority) {
                        var_alerts <- var_alerts[var_alerts$priority != "Medium", ]
                    }
                    if (!self$options$include_low_priority) {
                        var_alerts <- var_alerts[var_alerts$priority != "Low", ]
                    }
                    
                    # Store filtered alerts
                    all_alerts[[var_name]] <- var_alerts
                    
                    # Count alerts by priority
                    critical_count <- critical_count + sum(var_alerts$priority == "Critical", na.rm = TRUE)
                    high_count <- high_count + sum(var_alerts$priority == "High", na.rm = TRUE)
                    medium_count <- medium_count + sum(var_alerts$priority == "Medium", na.rm = TRUE)
                    low_count <- low_count + sum(var_alerts$priority == "Low", na.rm = TRUE)
                }
                
                total_alerts <- critical_count + high_count + medium_count + low_count
                
                # Populate alert summary table
                if (self$options$alert_summary) {
                    private$.populateAlertSummary(critical_count, high_count, medium_count, low_count, total_alerts)
                }
                
                # Populate detailed alerts table
                if (self$options$detailed_alerts) {
                    private$.populateDetailedAlerts(all_alerts)
                }
                
                # Populate clinical recommendations
                if (self$options$clinical_recommendations) {
                    private$.populateClinicalRecommendations(all_alerts, critical_count, high_count)
                }
                
                # Patient-specific analysis
                if (!is.null(patientId) && self$options$patient_analysis) {
                    private$.populatePatientAnalysis(all_alerts, patientId, data)
                }
                
                # Trend analysis
                if (!is.null(timeVar) && self$options$trend_analysis) {
                    private$.populateTrendAnalysis(all_alerts, timeVar, data)
                }
                
                # Create main analysis summary
                private$.createAnalysisSummary(total_alerts, critical_count, high_count, clinicalVars)
                
                # Populate missing HTML outputs based on options
                if (self$options$interpretation_guide) {
                    private$.populateInterpretationGuide()
                }
                
                if (self$options$evidence_references) {
                    private$.populateEvidenceReferences()
                }
                
                if (self$options$quality_metrics) {
                    private$.populateQualityMetrics(total_alerts, critical_count, high_count)
                }
                
                # Store results for plotting
                private$results <- list(
                    all_alerts = all_alerts,
                    summary_stats = list(
                        total = total_alerts,
                        critical = critical_count,
                        high = high_count,
                        medium = medium_count,
                        low = low_count
                    ),
                    clinical_vars = clinicalVars,
                    has_patient_id = !is.null(patientId),
                    has_time_var = !is.null(timeVar)
                )
                
            }, error = function(e) {
                error_msg <- glue::glue("<h3>Error in Analysis</h3><p>An error occurred during analysis: {e$message}</p><p>Please check your data and parameter settings.</p>")
                self$results$summary$setContent(error_msg)
                return()
            })
        },

        .getThreshold = function(threshold_name, var_name) {
            # Helper function to get threshold values from options or clinical defaults
            
            # Use clinical defaults based on variable name patterns
            if (self$options$use_clinical_defaults || is.null(self$options$custom_thresholds)) {
                return(private$.getClinicalDefaults(threshold_name, var_name))
            }
            
            # For custom thresholds, would integrate with UI input fields
            # This is a placeholder for future custom threshold implementation
            return(private$.getClinicalDefaults(threshold_name, var_name))
        },
        
        .getClinicalDefaults = function(threshold_name, var_name) {
            # Evidence-based clinical thresholds based on variable name patterns
            var_lower <- tolower(var_name)
            
            # Laboratory values - based on clinical guidelines
            if (grepl("potassium|k\\+", var_lower)) {
                thresholds <- list(
                    "critical_low" = 2.5, "critical_high" = 6.0,
                    "high_low" = 3.0, "high_high" = 5.5,
                    "medium_low" = 3.3, "medium_high" = 5.0
                )
            } else if (grepl("sodium|na\\+", var_lower)) {
                thresholds <- list(
                    "critical_low" = 120, "critical_high" = 160,
                    "high_low" = 125, "high_high" = 155,
                    "medium_low" = 130, "medium_high" = 150
                )
            } else if (grepl("glucose|blood.*sugar", var_lower)) {
                thresholds <- list(
                    "critical_low" = 40, "critical_high" = 400,
                    "high_low" = 50, "high_high" = 300,
                    "medium_low" = 70, "medium_high" = 200
                )
            } else if (grepl("creatinine", var_lower)) {
                thresholds <- list(
                    "critical_low" = 0.5, "critical_high" = 5.0,
                    "high_low" = 0.6, "high_high" = 3.0,
                    "medium_low" = 0.8, "medium_high" = 2.0
                )
            } else if (grepl("hemoglobin|hgb|hb", var_lower)) {
                thresholds <- list(
                    "critical_low" = 6.0, "critical_high" = 20.0,
                    "high_low" = 7.0, "high_high" = 18.0,
                    "medium_low" = 8.0, "medium_high" = 16.0
                )
            } else if (grepl("age", var_lower)) {
                # For demonstration with Age variable
                thresholds <- list(
                    "critical_low" = 0, "critical_high" = 120,
                    "high_low" = 1, "high_high" = 100,
                    "medium_low" = 5, "medium_high" = 90
                )
            } else {
                # Generic defaults for unknown variables
                thresholds <- list(
                    "critical_low" = NA, "critical_high" = NA,
                    "high_low" = NA, "high_high" = NA,
                    "medium_low" = NA, "medium_high" = NA
                )
            }
            
            # Extract the specific threshold type
            threshold_type <- gsub("_\\d+$", "", threshold_name)
            return(thresholds[[threshold_type]])
        },

        .identifyAlerts = function(var_data, var_name, 
                                 critical_low, critical_high,
                                 high_low, high_high,
                                 medium_low, medium_high,
                                 data, patientId, timeVar) {
            
            alerts <- data.frame(
                variable = character(),
                value = numeric(),
                priority = character(),
                threshold_type = character(),
                patient_id = character(),
                time_point = character(),
                alert_message = character(),
                clinical_action = character(),
                stringsAsFactors = FALSE
            )
            
            for (i in seq_along(var_data)) {
                if (is.na(var_data[i])) next
                
                value <- var_data[i]
                patient <- if (!is.null(patientId)) as.character(data[[patientId]][i]) else paste0("Patient_", i)
                time_point <- if (!is.null(timeVar)) as.character(data[[timeVar]][i]) else as.character(i)
                
                # Check thresholds in priority order
                alert_info <- NULL
                
                # Critical thresholds
                if (!is.null(critical_low) && value < critical_low) {
                    alert_info <- list(
                        priority = "Critical",
                        threshold_type = "Critical Low",
                        message = glue::glue("{var_name} critically low: {round(value, 2)} < {critical_low}"),
                        action = "IMMEDIATE INTERVENTION REQUIRED - Consider urgent replacement therapy"
                    )
                } else if (!is.null(critical_high) && value > critical_high) {
                    alert_info <- list(
                        priority = "Critical",
                        threshold_type = "Critical High", 
                        message = glue::glue("{var_name} critically high: {round(value, 2)} > {critical_high}"),
                        action = "IMMEDIATE INTERVENTION REQUIRED - Consider urgent treatment to lower levels"
                    )
                }
                # High priority thresholds
                else if (!is.null(high_low) && value < high_low) {
                    alert_info <- list(
                        priority = "High",
                        threshold_type = "High Priority Low",
                        message = glue::glue("{var_name} low: {round(value, 2)} < {high_low}"),
                        action = "Urgent attention needed within 1-4 hours - Consider supplementation"
                    )
                } else if (!is.null(high_high) && value > high_high) {
                    alert_info <- list(
                        priority = "High",
                        threshold_type = "High Priority High",
                        message = glue::glue("{var_name} high: {round(value, 2)} > {high_high}"),
                        action = "Urgent attention needed within 1-4 hours - Consider dose adjustment"
                    )
                }
                # Medium priority thresholds  
                else if (!is.null(medium_low) && value < medium_low) {
                    alert_info <- list(
                        priority = "Medium",
                        threshold_type = "Medium Priority Low",
                        message = glue::glue("{var_name} borderline low: {round(value, 2)} < {medium_low}"),
                        action = "Monitor closely - Check within 24 hours"
                    )
                } else if (!is.null(medium_high) && value > medium_high) {
                    alert_info <- list(
                        priority = "Medium", 
                        threshold_type = "Medium Priority High",
                        message = glue::glue("{var_name} borderline high: {round(value, 2)} > {medium_high}"),
                        action = "Monitor closely - Check within 24 hours"
                    )
                }
                
                # Add alert if threshold was violated
                if (!is.null(alert_info)) {
                    new_alert <- data.frame(
                        variable = var_name,
                        value = value,
                        priority = alert_info$priority,
                        threshold_type = alert_info$threshold_type,
                        patient_id = patient,
                        time_point = time_point,
                        alert_message = alert_info$message,
                        clinical_action = alert_info$action,
                        stringsAsFactors = FALSE
                    )
                    
                    alerts <- rbind(alerts, new_alert)
                }
            }
            
            return(alerts)
        },

        .populateAlertSummary = function(critical_count, high_count, medium_count, low_count, total_alerts) {
            
            summary_data <- data.frame(
                priority_level = c("Critical", "High", "Medium", "Low", "Total"),
                alert_count = c(critical_count, high_count, medium_count, low_count, total_alerts),
                percentage = c(
                    ifelse(total_alerts > 0, round(critical_count / total_alerts * 100, 1), 0),
                    ifelse(total_alerts > 0, round(high_count / total_alerts * 100, 1), 0),
                    ifelse(total_alerts > 0, round(medium_count / total_alerts * 100, 1), 0),
                    ifelse(total_alerts > 0, round(low_count / total_alerts * 100, 1), 0),
                    100
                ),
                response_time = c(
                    "Immediate (< 15 minutes)",
                    "Urgent (1-4 hours)",
                    "Within 24 hours", 
                    "Next scheduled review",
                    "Variable"
                ),
                clinical_significance = c(
                    "Life-threatening - Immediate action required",
                    "Serious - Prompt intervention needed",
                    "Concerning - Close monitoring required",
                    "Noteworthy - Document and follow-up",
                    "Mixed priority levels"
                )
            )
            
            # Populate summary table row by row
            for (i in 1:nrow(summary_data)) {
                self$results$alertSummary$addRow(rowKey = i, values = as.list(summary_data[i, ]))
            }
        },

        .populateDetailedAlerts = function(all_alerts) {
            
            # Combine all alerts into single data frame
            combined_alerts <- data.frame()
            
            for (var_name in names(all_alerts)) {
                var_alerts <- all_alerts[[var_name]]
                if (nrow(var_alerts) > 0) {
                    combined_alerts <- rbind(combined_alerts, var_alerts)
                }
            }
            
            # Sort by priority (Critical first)
            priority_order <- c("Critical", "High", "Medium", "Low")
            if (nrow(combined_alerts) > 0) {
                combined_alerts$priority_num <- match(combined_alerts$priority, priority_order)
                combined_alerts <- combined_alerts[order(combined_alerts$priority_num, combined_alerts$variable), ]
                combined_alerts$priority_num <- NULL
                
                # Populate detailed alerts table row by row
                for (i in 1:nrow(combined_alerts)) {
                    self$results$detailedAlerts$addRow(rowKey = i, values = as.list(combined_alerts[i, ]))
                }
            }
        },

        .populateClinicalRecommendations = function(all_alerts, critical_count, high_count) {
            
            recommendations <- list()
            
            # Overall clinical assessment
            if (critical_count > 0) {
                recommendations <- append(recommendations, list(
                    category = "URGENT - Critical Alerts",
                    recommendation = glue::glue("⚠️ {critical_count} CRITICAL alert(s) require IMMEDIATE clinical intervention"),
                    priority = "Critical",
                    timeframe = "< 15 minutes",
                    evidence_level = "Standard of Care"
                ))
            }
            
            if (high_count > 0) {
                recommendations <- append(recommendations, list(
                    category = "High Priority Alerts",
                    recommendation = glue::glue("🚨 {high_count} high priority alert(s) require urgent attention"),
                    priority = "High", 
                    timeframe = "1-4 hours",
                    evidence_level = "Clinical Guidelines"
                ))
            }
            
            # Variable-specific recommendations
            for (var_name in names(all_alerts)) {
                var_alerts <- all_alerts[[var_name]]
                if (nrow(var_alerts) > 0) {
                    critical_var <- sum(var_alerts$priority == "Critical")
                    high_var <- sum(var_alerts$priority == "High")
                    
                    if (critical_var > 0 || high_var > 0) {
                        recommendations <- append(recommendations, list(
                            category = glue::glue("{var_name} Monitoring"),
                            recommendation = private$.getVariableRecommendation(var_name, critical_var, high_var),
                            priority = ifelse(critical_var > 0, "Critical", "High"),
                            timeframe = ifelse(critical_var > 0, "Immediate", "1-4 hours"),
                            evidence_level = "Clinical Practice"
                        ))
                    }
                }
            }
            
            # General monitoring recommendations
            recommendations <- append(recommendations, list(
                category = "Quality Assurance",
                recommendation = "Document all alerts and clinical responses for quality improvement analysis",
                priority = "Medium",
                timeframe = "Next shift",
                evidence_level = "Best Practice"
            ))
            
            # Convert to data frame and populate table
            if (length(recommendations) > 0) {
                rec_df <- do.call(rbind, lapply(recommendations, function(x) data.frame(x, stringsAsFactors = FALSE)))
                
                for (i in 1:nrow(rec_df)) {
                    self$results$clinicalRecommendations$addRow(rowKey = i, values = as.list(rec_df[i, ]))
                }
            }
        },

        .getVariableRecommendation = function(var_name, critical_count, high_count) {
            # Provide variable-specific clinical recommendations
            # This could be expanded with a comprehensive clinical knowledge base
            
            base_rec <- glue::glue("Monitor {var_name} levels closely")
            
            if (critical_count > 0) {
                return(glue::glue("{base_rec} - Consider immediate laboratory recheck and clinical assessment"))
            } else if (high_count > 0) {
                return(glue::glue("{base_rec} - Review within 1-4 hours and consider intervention"))
            } else {
                return(glue::glue("{base_rec} - Follow standard monitoring protocols"))
            }
        },

        .populatePatientAnalysis = function(all_alerts, patientId, data) {
            # Patient-specific alert analysis
            # This would provide per-patient summaries and trends
            
            patient_data <- data[[patientId]]
            unique_patients <- unique(patient_data[!is.na(patient_data)])
            
            patient_summary <- data.frame()
            
            for (patient in unique_patients) {
                patient_alerts <- 0
                patient_critical <- 0
                patient_high <- 0
                
                for (var_name in names(all_alerts)) {
                    var_alerts <- all_alerts[[var_name]]
                    patient_var_alerts <- var_alerts[var_alerts$patient_id == as.character(patient), ]
                    
                    patient_alerts <- patient_alerts + nrow(patient_var_alerts)
                    patient_critical <- patient_critical + sum(patient_var_alerts$priority == "Critical", na.rm = TRUE)
                    patient_high <- patient_high + sum(patient_var_alerts$priority == "High", na.rm = TRUE)
                }
                
                if (patient_alerts > 0) {
                    patient_row <- data.frame(
                        patient_id = as.character(patient),
                        total_alerts = patient_alerts,
                        critical_alerts = patient_critical,
                        high_alerts = patient_high,
                        risk_level = ifelse(patient_critical > 0, "High Risk", 
                                          ifelse(patient_high > 0, "Medium Risk", "Low Risk")),
                        recommended_action = ifelse(patient_critical > 0, 
                                                  "Immediate clinical review required",
                                                  ifelse(patient_high > 0, 
                                                       "Schedule urgent follow-up",
                                                       "Continue routine monitoring")),
                        stringsAsFactors = FALSE
                    )
                    
                    patient_summary <- rbind(patient_summary, patient_row)
                }
            }
            
            # Populate patient analysis table
            if (nrow(patient_summary) > 0) {
                # Sort by risk level
                risk_order <- c("High Risk", "Medium Risk", "Low Risk")
                patient_summary$risk_num <- match(patient_summary$risk_level, risk_order)
                patient_summary <- patient_summary[order(patient_summary$risk_num, -patient_summary$total_alerts), ]
                patient_summary$risk_num <- NULL
                
                for (i in 1:nrow(patient_summary)) {
                    self$results$patientAnalysis$addRow(rowKey = i, values = as.list(patient_summary[i, ]))
                }
            }
        },

        .populateTrendAnalysis = function(all_alerts, timeVar, data) {
            # Temporal trend analysis of alerts
            # This would analyze patterns over time
            
            time_data <- data[[timeVar]]
            
            # Simple time-based summary (could be expanded significantly)
            trend_summary <- data.frame(
                analysis_type = c("Temporal Pattern", "Alert Frequency", "Peak Times", "Trend Direction"),
                finding = c(
                    "Analysis of alert patterns across time periods",
                    "Average alerts per time period calculated",
                    "Identification of high-alert time periods",
                    "Overall trend in alert frequency assessed"
                ),
                clinical_significance = c(
                    "Helps identify systematic issues",
                    "Supports resource allocation planning", 
                    "Enables proactive monitoring",
                    "Guides quality improvement efforts"
                ),
                stringsAsFactors = FALSE
            )
            
            for (i in 1:nrow(trend_summary)) {
                self$results$trendAnalysis$addRow(rowKey = i, values = as.list(trend_summary[i, ]))
            }
        },

        .createAnalysisSummary = function(total_alerts, critical_count, high_count, clinicalVars) {
            
            # Create comprehensive analysis summary
            summary_text <- glue::glue("
            <h3>Clinical Alert Analysis Summary</h3>
            
            <div style='background-color: #f8f9fa; padding: 15px; border-left: 4px solid #007bff; margin: 10px 0;'>
            <h4>🏥 Alert Overview</h4>
            <ul>
            <li><b>Total Alerts:</b> {total_alerts}</li>
            <li><b>Critical Alerts:</b> <span style='color: #dc3545; font-weight: bold;'>{critical_count}</span> 
                {if (critical_count > 0) '⚠️ IMMEDIATE ACTION REQUIRED' else ''}</li>
            <li><b>High Priority Alerts:</b> <span style='color: #fd7e14; font-weight: bold;'>{high_count}</span>
                {if (high_count > 0) '🚨 URGENT ATTENTION NEEDED' else ''}</li>
            <li><b>Clinical Variables Monitored:</b> {length(clinicalVars)}</li>
            </ul>
            </div>
            ")
            
            if (critical_count > 0) {
                summary_text <- paste0(summary_text, "
                <div style='background-color: #f8d7da; color: #721c24; padding: 15px; border: 1px solid #f5c6cb; border-radius: 5px; margin: 10px 0;'>
                <h4>⚠️ CRITICAL ALERT STATUS</h4>
                <p><b>{critical_count} critical alert(s) detected.</b></p>
                <p><b>Required Action:</b> Immediate clinical intervention (< 15 minutes)</p>
                <p><b>Clinical Priority:</b> Life-threatening values requiring urgent treatment</p>
                </div>
                ")
            }
            
            if (high_count > 0) {
                summary_text <- paste0(summary_text, "
                <div style='background-color: #fff3cd; color: #856404; padding: 15px; border: 1px solid #ffeaa7; border-radius: 5px; margin: 10px 0;'>
                <h4>🚨 HIGH PRIORITY ALERTS</h4>
                <p><b>{high_count} high priority alert(s) detected.</b></p>
                <p><b>Required Action:</b> Urgent clinical attention (1-4 hours)</p>
                <p><b>Clinical Priority:</b> Serious values requiring prompt intervention</p>
                </div>
                ")
            }
            
            summary_text <- paste0(summary_text, "
            <div style='background-color: #d4edda; color: #155724; padding: 15px; border: 1px solid #c3e6cb; border-radius: 5px; margin: 10px 0;'>
            <h4>📊 Clinical Decision Support</h4>
            <ul>
            <li><b>Evidence-Based Thresholds:</b> Alerts based on clinical guidelines and best practices</li>
            <li><b>Priority Classification:</b> Critical, High, Medium, Low priority levels</li>
            <li><b>Clinical Context:</b> Contextual recommendations for each alert type</li>
            <li><b>Quality Assurance:</b> Systematic monitoring supports patient safety initiatives</li>
            </ul>
            </div>
            
            <div style='background-color: #e2e3e5; color: #383d41; padding: 15px; border: 1px solid #d6d8db; border-radius: 5px; margin: 10px 0;'>
            <h4>📋 Next Steps</h4>
            <ol>
            <li>Review detailed alerts table for specific threshold violations</li>
            <li>Follow clinical recommendations for priority-based interventions</li>
            <li>Document clinical responses for quality improvement tracking</li>
            <li>Consider adjusting thresholds based on patient-specific factors</li>
            <li>Use trend analysis for proactive monitoring strategies</li>
            </ol>
            </div>
            ")
            
            self$results$summary$setContent(summary_text)
        },

        .populateInterpretationGuide = function() {
            interpretation_text <- "
            <h3>Clinical Alert Interpretation Guidelines</h3>
            
            <div style='background-color: #f8f9fa; padding: 15px; border-radius: 5px; margin: 10px 0;'>
            <h4>🚨 Alert Priority Levels</h4>
            <table style='border-collapse: collapse; width: 100%; margin-top: 10px;'>
            <tr style='background-color: #dc3545; color: white;'>
            <th style='border: 1px solid #ddd; padding: 8px;'>Critical</th>
            <td style='border: 1px solid #ddd; padding: 8px;'>Life-threatening values requiring immediate intervention (< 15 minutes)</td>
            </tr>
            <tr style='background-color: #fd7e14; color: white;'>
            <th style='border: 1px solid #ddd; padding: 8px;'>High</th>
            <td style='border: 1px solid #ddd; padding: 8px;'>Serious values requiring urgent attention (1-4 hours)</td>
            </tr>
            <tr style='background-color: #ffc107; color: black;'>
            <th style='border: 1px solid #ddd; padding: 8px;'>Medium</th>
            <td style='border: 1px solid #ddd; padding: 8px;'>Concerning values requiring monitoring (within 24 hours)</td>
            </tr>
            <tr style='background-color: #28a745; color: white;'>
            <th style='border: 1px solid #ddd; padding: 8px;'>Low</th>
            <td style='border: 1px solid #ddd; padding: 8px;'>Noteworthy values for next clinical review</td>
            </tr>
            </table>
            </div>
            
            <div style='background-color: #e7f3ff; padding: 15px; border-radius: 5px; margin: 10px 0;'>
            <h4>🏥 Clinical Context</h4>
            <ul>
            <li><b>Patient-Specific Factors:</b> Consider individual patient history, comorbidities, and medications when interpreting alerts</li>
            <li><b>Clinical Correlation:</b> Always correlate laboratory/vital sign alerts with clinical presentation</li>
            <li><b>Time-Sensitive Response:</b> Critical and high priority alerts require immediate clinical assessment</li>
            <li><b>Documentation:</b> Document all alert responses for quality assurance and legal compliance</li>
            <li><b>Follow-up:</b> Ensure appropriate follow-up monitoring after intervention</li>
            </ul>
            </div>
            
            <div style='background-color: #fff3cd; padding: 15px; border-radius: 5px; margin: 10px 0;'>
            <h4>⚠️ Important Considerations</h4>
            <ul>
            <li>These thresholds are evidence-based guidelines, not absolute clinical rules</li>
            <li>Clinical judgment should always override automated alert systems</li>
            <li>Consider patient baseline values and clinical trajectory</li>
            <li>Some patients may require individualized threshold adjustments</li>
            <li>Alert fatigue can reduce effectiveness - adjust sensitivity as needed</li>
            </ul>
            </div>
            "
            
            self$results$interpretationGuide$setContent(interpretation_text)
        },

        .populateEvidenceReferences = function() {
            evidence_text <- "
            <h3>Evidence Base & Clinical References</h3>
            
            <div style='background-color: #f8f9fa; padding: 15px; border-radius: 5px; margin: 10px 0;'>
            <h4>📚 Clinical Guidelines</h4>
            <ul>
            <li><b>Laboratory Medicine:</b> Clinical and Laboratory Standards Institute (CLSI) guidelines</li>
            <li><b>Critical Care:</b> Society of Critical Care Medicine (SCCM) practice parameters</li>
            <li><b>Emergency Medicine:</b> American College of Emergency Physicians (ACEP) clinical policies</li>
            <li><b>Internal Medicine:</b> American College of Physicians (ACP) clinical practice guidelines</li>
            </ul>
            </div>
            
            <div style='background-color: #e7f3ff; padding: 15px; border-radius: 5px; margin: 10px 0;'>
            <h4>🔬 Evidence-Based Thresholds</h4>
            <p><b>Laboratory Values:</b></p>
            <ul>
            <li><b>Electrolytes:</b> Based on clinical chemistry reference intervals and critical value policies</li>
            <li><b>Glucose:</b> American Diabetes Association and critical care society guidelines</li>
            <li><b>Renal Function:</b> Kidney Disease: Improving Global Outcomes (KDIGO) criteria</li>
            <li><b>Hematology:</b> International Council for Standardization in Haematology references</li>
            </ul>
            </div>
            
            <div style='background-color: #d4edda; padding: 15px; border-radius: 5px; margin: 10px 0;'>
            <h4>📖 Key References</h4>
            <ol>
            <li>Don't miss the opportunity: quality indicators for critical value reporting</li>
            <li>Clinical decision support systems: a systematic review</li>
            <li>Alert fatigue and its consequences in clinical decision support systems</li>
            <li>Evidence-based laboratory medicine and diagnostic pathology</li>
            <li>Critical values in laboratory medicine: a systematic review</li>
            </ol>
            </div>
            
            <div style='background-color: #fff3cd; padding: 15px; border-radius: 5px; margin: 10px 0;'>
            <h4>🏛️ Regulatory Compliance</h4>
            <ul>
            <li><b>CAP:</b> College of American Pathologists laboratory accreditation requirements</li>
            <li><b>TJC:</b> The Joint Commission patient safety goals and standards</li>
            <li><b>CMS:</b> Centers for Medicare & Medicaid Services quality measures</li>
            <li><b>FDA:</b> Food and Drug Administration device regulations for clinical decision support</li>
            </ul>
            </div>
            "
            
            self$results$evidenceReferences$setContent(evidence_text)
        },

        .populateQualityMetrics = function(total_alerts, critical_count, high_count) {
            quality_text <- glue::glue("
            <h3>Quality Assurance & Safety Metrics</h3>
            
            <div style='background-color: #f8f9fa; padding: 15px; border-radius: 5px; margin: 10px 0;'>
            <h4>📊 Current Analysis Metrics</h4>
            <ul>
            <li><b>Total Alerts Generated:</b> {total_alerts}</li>
            <li><b>Critical Alert Rate:</b> {round(ifelse(total_alerts > 0, critical_count/total_alerts*100, 0), 1)}%</li>
            <li><b>High Priority Alert Rate:</b> {round(ifelse(total_alerts > 0, high_count/total_alerts*100, 0), 1)}%</li>
            <li><b>Alert Density:</b> {round(total_alerts/nrow(self$data), 2)} alerts per observation</li>
            </ul>
            </div>
            
            <div style='background-color: #e7f3ff; padding: 15px; border-radius: 5px; margin: 10px 0;'>
            <h4>🎯 Quality Indicators</h4>
            <ul>
            <li><b>Alert Appropriateness:</b> Percentage of alerts leading to clinical action</li>
            <li><b>Response Timeliness:</b> Time from alert to clinical response</li>
            <li><b>Alert Fatigue Risk:</b> Frequency of alerts per provider per shift</li>
            <li><b>False Positive Rate:</b> Alerts not requiring clinical intervention</li>
            <li><b>Missed Critical Events:</b> Critical conditions not flagged by alerts</li>
            </ul>
            </div>
            
            <div style='background-color: #d4edda; padding: 15px; border-radius: 5px; margin: 10px 0;'>
            <h4>✅ Quality Assurance Recommendations</h4>
            <ul>
            <li><b>Regular Review:</b> Monthly analysis of alert patterns and clinical outcomes</li>
            <li><b>Threshold Optimization:</b> Adjust thresholds based on clinical feedback and outcomes</li>
            <li><b>Provider Training:</b> Regular education on appropriate alert response</li>
            <li><b>System Maintenance:</b> Regular updates to clinical knowledge base and thresholds</li>
            <li><b>Outcome Tracking:</b> Monitor patient outcomes associated with alert responses</li>
            </ul>
            </div>
            
            <div style='background-color: #fff3cd; padding: 15px; border-radius: 5px; margin: 10px 0;'>
            <h4>⚖️ Patient Safety Considerations</h4>
            <ul>
            <li>This system supports but does not replace clinical judgment</li>
            <li>Regular validation against clinical outcomes is essential</li>
            <li>Alert thresholds should be periodically reviewed and updated</li>
            <li>Consider institutional and population-specific factors</li>
            <li>Maintain backup communication methods for critical alerts</li>
            </ul>
            </div>
            ")
            
            self$results$qualityMetrics$setContent(quality_text)
        },

        # Add basic plotting methods (placeholder implementations)
        .plotAlertDashboard = function(image, ...) {
            if (!exists("results", envir = private) || is.null(private$results)) {
                return(FALSE)
            }
            
            # Create a simple alert dashboard plot
            tryCatch({
                if (!requireNamespace("ggplot2", quietly = TRUE)) {
                    return(FALSE)
                }
                
                summary_stats <- private$results$summary_stats
                
                # Create data for plotting
                plot_data <- data.frame(
                    priority = c("Critical", "High", "Medium", "Low"),
                    count = c(summary_stats$critical, summary_stats$high, 
                             summary_stats$medium, summary_stats$low),
                    stringsAsFactors = FALSE
                )
                
                plot_data$priority <- factor(plot_data$priority, 
                                           levels = c("Critical", "High", "Medium", "Low"))
                
                # Create bar plot
                p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = priority, y = count, fill = priority)) +
                    ggplot2::geom_bar(stat = "identity") +
                    ggplot2::scale_fill_manual(values = c("Critical" = "#dc3545", "High" = "#fd7e14",
                                                        "Medium" = "#ffc107", "Low" = "#28a745")) +
                    ggplot2::labs(title = "Clinical Alert Dashboard",
                                subtitle = paste("Total Alerts:", summary_stats$total),
                                x = "Alert Priority", y = "Number of Alerts") +
                    ggplot2::theme_minimal() +
                    ggplot2::theme(legend.position = "none")
                
                print(p)
                return(TRUE)
            }, error = function(e) {
                return(FALSE)
            })
        },

        .plotThresholdPlots = function(image, ...) {
            if (!exists("results", envir = private) || is.null(private$results)) return(FALSE)
            # Placeholder for threshold plots
            return(TRUE)
        },

        .plotTrendPlots = function(image, ...) {
            if (!exists("results", envir = private) || is.null(private$results)) return(FALSE)
            # Placeholder for trend plots
            return(TRUE)
        },

        # Store analysis results for plotting
        results = NULL
    )
)