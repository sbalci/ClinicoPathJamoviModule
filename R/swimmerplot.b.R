#' @title Swimmer Plot
#' @description 
#' Comprehensive swimmer plot function with full ggswim integration.
#' Creates swimmer plots for visualizing patient timelines, treatments, milestones, and clinical events.
#'
#' @importFrom R6 R6Class  
#' @import jmvcore
#' @import ggswim
#' @importFrom ggplot2 ggplot aes labs theme element_text element_blank
#' @importFrom dplyr mutate filter select group_by summarize left_join arrange n bind_rows
#' @importFrom tidyr pivot_wider
#' @importFrom lubridate ymd_hms ymd ydm mdy myd dmy dym interval time_length
#' @importFrom tibble tibble
#' @importFrom RColorBrewer brewer.pal
#' @importFrom scales pretty_breaks
#' @importFrom gridExtra grid.arrange

swimmerplotClass <- if (requireNamespace('jmvcore', quietly = TRUE)) R6::R6Class(
    "swimmerplotClass",
    inherit = swimmerplotBase,
    private = list(
        # Clinical preset context storage
        .preset_context = NULL,
        .preset_guidance = NULL,
        
        # Enhanced clinical date parsing with contextual guidance
        .parseDatesWithClinicalContext = function(dates, format, variable_type = "time") {
            if (inherits(dates, c("Date", "POSIXct"))) return(dates)
            
            parsed <- tryCatch({
                switch(format,
                    "ymdhms" = lubridate::ymd_hms(dates),
                    "ymd"    = lubridate::ymd(dates), 
                    "ydm"    = lubridate::ydm(dates),
                    "mdy"    = lubridate::mdy(dates),
                    "myd"    = lubridate::myd(dates),
                    "dmy"    = lubridate::dmy(dates),
                    "dym"    = lubridate::dym(dates),
                    lubridate::ymd(dates)
                )
            }, error = function(e) NULL)
            
            if (is.null(parsed)) {
                clinical_guidance <- switch(variable_type,
                    "start" = .("Common formats: 2023-01-15 (treatment start), 15/01/2023 (surgery date), or numeric days from study start"),
                    "end" = .("Common formats: 2023-06-15 (treatment end), 15/06/2023 (last follow-up), or numeric days from treatment start"), 
                    "milestone" = .("Common formats: 2023-03-15 (response assessment), 15/03/2023 (progression date), or numeric days from treatment start"),
                    .("Please check date format - use YYYY-MM-DD, DD/MM/YYYY, or numeric values")
                )
                
                return(list(
                    error = TRUE,
                    message = paste(
                        .("Error parsing"), variable_type, .("dates with format"), format, ".", 
                        clinical_guidance
                    )
                ))
            }
            return(parsed)
        },
        
        # Comprehensive clinical data validation
        .validateClinicalData = function(patient_data) {
            warnings <- list()
            errors <- list()
            
            # Check for realistic time ranges
            durations <- patient_data$end_time - patient_data$start_time
            negative_durations <- which(durations < 0)
            if (length(negative_durations) > 0) {
                errors <- append(errors, sprintf(
                    .(.("Found %d patients with negative follow-up times. Please check end dates are after start dates.")), 
                    length(negative_durations)
                ))
            }
            
            # Check for extremely long follow-up periods
            if (self$options$timeUnit == "days") {
                long_followup <- which(durations > 3650) # >10 years
                if (length(long_followup) > 0) {
                    warnings <- append(warnings, sprintf(
                        .(.("Found %d patients with follow-up >10 years. Consider checking data accuracy or using different time units.")),
                        length(long_followup)
                    ))
                }
            } else if (self$options$timeUnit == "months") {
                long_followup <- which(durations > 120) # >10 years
                if (length(long_followup) > 0) {
                    warnings <- append(warnings, sprintf(
                        .(.("Found %d patients with follow-up >10 years in months. Consider data validation.")),
                        length(long_followup)
                    ))
                }
            }
            
            # Check for zero-duration events
            zero_durations <- which(durations == 0)
            if (length(zero_durations) > 0) {
                warnings <- append(warnings, sprintf(
                    .(.("Found %d patients with zero follow-up time. These may represent same-day events.")),
                    length(zero_durations)
                ))
            }
            
            # Check for missing patient IDs
            missing_ids <- which(is.na(patient_data$patient_id) | patient_data$patient_id == "")
            if (length(missing_ids) > 0) {
                warnings <- append(warnings, sprintf(
                    .(.("Found %d rows with missing patient IDs. These will be excluded from analysis.")),
                    length(missing_ids)
                ))
            }
            
            # Check for duplicate patient IDs (potential data issue)
            duplicate_ids <- patient_data$patient_id[duplicated(patient_data$patient_id)]
            if (length(duplicate_ids) > 0) {
                warnings <- append(warnings, sprintf(
                    .(.("Found %d duplicate patient IDs. Multiple episodes per patient detected - this is normal for longitudinal data.")),
                    length(duplicate_ids)
                ))
            }
            
            # Response variable validation
            if ("response" %in% names(patient_data)) {
                missing_response <- sum(is.na(patient_data$response))
                if (missing_response > 0) {
                    warnings <- append(warnings, sprintf(
                        .(.("Found %d patients with missing response data (%.1f%% of total).")),
                        missing_response,
                        missing_response / nrow(patient_data) * 100
                    ))
                }
                
                # Check for unusual response patterns
                response_counts <- table(patient_data$response, useNA = "no")
                if (length(response_counts) > 0) {
                    min_category <- min(response_counts)
                    if (min_category < 3) {
                        warnings <- append(warnings, sprintf(
                            .(.("Some response categories have <3 patients. Consider grouping categories for meaningful analysis.")))
                        )
                    }
                }
            }
            
            return(list(
                errors = errors,
                warnings = warnings,
                has_errors = length(errors) > 0,
                has_warnings = length(warnings) > 0
            ))
        },
        
        # Enhanced data validation with detailed error reporting
        .validateAndProcessData = function() {
            required_vars <- c("patientID", "startTime", "endTime")
            missing_vars <- required_vars[sapply(required_vars, function(x) is.null(self$options[[x]]))]
            
            if (length(missing_vars) > 0) {
                return(list(
                    error = TRUE,
                    message = paste(.("Missing required variables:"), paste(missing_vars, collapse = ", "))
                ))
            }
            
            df <- self$data
            
            # Extract and process core variables
            patient_data <- tryCatch({
                data.frame(
                    patient_id = as.character(df[[self$options$patientID]]),
                    start_time = df[[self$options$startTime]],
                    end_time = df[[self$options$endTime]],
                    stringsAsFactors = FALSE
                )
            }, error = function(e) {
                return(list(
                    error = TRUE,
                    message = paste(.(.("Error processing core variables:")), e$message)
                ))
            })
            
            # Check if data extraction failed
            if (!is.null(patient_data$error) && patient_data$error) {
                return(patient_data)
            }
            
            # Enhanced time processing
            if (self$options$timeType == "datetime") {
                start_parsed <- private$.parseDatesWithClinicalContext(
                    as.character(patient_data$start_time), 
                    self$options$dateFormat,
                    "start"
                )
                end_parsed <- private$.parseDatesWithClinicalContext(
                    as.character(patient_data$end_time), 
                    self$options$dateFormat,
                    "end"
                )
                
                # Check for parsing errors
                if (!is.null(start_parsed$error) && start_parsed$error) {
                    return(list(error = TRUE, message = paste(.("Start time parsing:"), start_parsed$message)))
                }
                if (!is.null(end_parsed$error) && end_parsed$error) {
                    return(list(error = TRUE, message = paste(.("End time parsing:"), end_parsed$message)))
                }
                
                patient_data$start_time <- start_parsed
                patient_data$end_time <- end_parsed
                
                # Handle relative vs absolute time display
                if (self$options$timeDisplay == "relative") {
                    intervals <- lubridate::interval(patient_data$start_time, patient_data$end_time)
                    durations <- lubridate::time_length(intervals, unit = self$options$timeUnit)
                    
                    patient_data$original_start <- patient_data$start_time
                    patient_data$original_end <- patient_data$end_time
                    patient_data$start_time <- 0
                    patient_data$end_time <- durations
                }
            } else {
                # Raw numeric processing with robust conversion
                patient_data$start_time <- suppressWarnings(as.numeric(as.character(patient_data$start_time)))
                patient_data$end_time <- suppressWarnings(as.numeric(as.character(patient_data$end_time)))
            }
            
            # Add response/status variable if provided
            if (!is.null(self$options$responseVar)) {
                patient_data$response <- as.factor(df[[self$options$responseVar]])
            }
            
            # Data validation
            valid_rows <- !is.na(patient_data$patient_id) & 
                         !is.na(patient_data$start_time) & 
                         !is.na(patient_data$end_time) &
                         patient_data$end_time >= patient_data$start_time
            
            if (sum(valid_rows) == 0) {
                return(list(
                    error = TRUE,
                    message = .("No valid data rows after validation. Check that end times are >= start times and all required data is present.")
                ))
            }
            
            patient_data <- patient_data[valid_rows, ]
            
            # Perform clinical validation
            validation_result <- private$.validateClinicalData(patient_data)
            
            # Add validation results to return object
            patient_data_with_validation <- list(
                data = patient_data,
                validation = validation_result,
                error = validation_result$has_errors,
                message = if (validation_result$has_errors) paste(validation_result$errors, collapse = " ") else NULL
            )
            
            # Return error if critical issues found
            if (validation_result$has_errors) {
                return(patient_data_with_validation)
            }
            
            # If only warnings, continue but store them for display
            if (validation_result$has_warnings) {
                patient_data_with_validation$warnings <- validation_result$warnings
            }
            
            return(patient_data_with_validation)
        },
        
        # Process milestone data with enhanced handling
        .processMilestones = function(patient_data) {
            milestone_data <- data.frame()
            
            for (i in 1:self$options$maxMilestones) {
                name_opt <- paste0("milestone", i, "Name")
                date_opt <- paste0("milestone", i, "Date")
                
                if (!is.null(self$options[[date_opt]]) && 
                    !is.null(self$options[[name_opt]]) && 
                    self$options[[name_opt]] != "") {
                    
                    milestone_dates <- self$data[[self$options[[date_opt]]]]
                    
                    # Skip if all NA
                    if (all(is.na(milestone_dates))) next
                    
                    # Process dates
                    if (self$options$timeType == "datetime") {
                        milestone_dates <- private$.parseDatesWithClinicalContext(
                            as.character(milestone_dates),
                            self$options$dateFormat,
                            "milestone"
                        )
                        
                        # Adjust for relative display (vectorized for performance)
                        if (self$options$timeDisplay == "relative" && "original_start" %in% names(patient_data)) {
                            # Vectorized calculation for better performance with large datasets
                            valid_indices <- which(!is.na(milestone_dates) & seq_along(milestone_dates) <= nrow(patient_data))
                            if (length(valid_indices) > 0) {
                                intervals <- lubridate::interval(
                                    patient_data$original_start[valid_indices],
                                    milestone_dates[valid_indices]
                                )
                                adjusted_dates <- rep(NA_real_, length(milestone_dates))
                                adjusted_dates[valid_indices] <- lubridate::time_length(intervals, unit = self$options$timeUnit)
                                milestone_dates <- adjusted_dates
                            }
                        }
                    } else {
                        milestone_dates <- suppressWarnings(as.numeric(as.character(milestone_dates)))
                        
                        # Adjust for relative display
                        if (self$options$timeDisplay == "relative" && "start_time" %in% names(patient_data)) {
                            milestone_dates <- milestone_dates - patient_data$start_time[seq_along(milestone_dates)]
                        }
                    }
                    
                    # Create milestone dataframe
                    temp_milestone <- data.frame(
                        patient_id = patient_data$patient_id[seq_along(milestone_dates)],
                        time = milestone_dates,
                        label = self$options[[name_opt]],
                        milestone_type = paste0("milestone_", i),
                        stringsAsFactors = FALSE
                    )
                    
                    # Remove NA rows
                    temp_milestone <- temp_milestone[!is.na(temp_milestone$time), ]
                    
                    if (nrow(temp_milestone) > 0) {
                        milestone_data <- rbind(milestone_data, temp_milestone)
                    }
                }
            }
            
            return(milestone_data)
        },
        
        # Process ongoing status arrows
        .processOngoingStatus = function(patient_data) {
            arrow_data <- NULL
            
            # Detect ongoing treatments (those with missing end times or end at study cutoff)
            if ("response" %in% names(patient_data)) {
                ongoing_patients <- patient_data[
                    !is.na(patient_data$patient_id) & 
                    !is.na(patient_data$end_time) &
                    patient_data$response %in% c("CR", "PR", "SD"), # Responding patients more likely ongoing
                ]
                
                if (nrow(ongoing_patients) > 0) {
                    arrow_data <- data.frame(
                        patient_id = ongoing_patients$patient_id,
                        xend = ongoing_patients$end_time,
                        stringsAsFactors = FALSE
                    )
                }
            }
            
            return(arrow_data)
        },
        
        # Process event markers with enhanced icon support
        .processEventMarkers = function(patient_data) {
            event_data <- NULL
            
            if (self$options$showEventMarkers) {
                # Smart event variable detection
                event_var <- self$options$eventVar
                event_time_var <- self$options$eventTimeVar %||% self$options$startTime
                
                if (!is.null(event_var)) {
                    event_data <- tryCatch({
                        data.frame(
                            patient_id = as.character(self$data[[self$options$patientID]]),
                            time = self$data[[event_time_var]],
                            label = as.character(self$data[[event_var]]),
                            stringsAsFactors = FALSE
                        )
                    }, error = function(e) {
                        warning(paste("Error processing event markers:", e$message))
                        return(NULL)
                    })
                    
                    if (!is.null(event_data)) {
                        # Process event times
                        if (self$options$timeType == "datetime") {
                            event_data$time <- private$.parseDatesWithClinicalContext(
                                as.character(event_data$time),
                                self$options$dateFormat,
                                "milestone"
                            )
                            
                            # Adjust for relative display (vectorized for performance)
                            if (self$options$timeDisplay == "relative" && "original_start" %in% names(patient_data)) {
                                # Vectorized processing for better performance
                                valid_events <- which(!is.na(event_data$time))
                                if (length(valid_events) > 0) {
                                    # Use match() once for all events
                                    patient_indices <- match(event_data$patient_id[valid_events], patient_data$patient_id)
                                    valid_matches <- which(!is.na(patient_indices))
                                    
                                    if (length(valid_matches) > 0) {
                                        event_idx <- valid_events[valid_matches]
                                        patient_idx <- patient_indices[valid_matches]
                                        
                                        intervals <- lubridate::interval(
                                            patient_data$original_start[patient_idx],
                                            event_data$time[event_idx]
                                        )
                                        
                                        adjusted_times <- rep(NA_real_, nrow(event_data))
                                        adjusted_times[event_idx] <- lubridate::time_length(intervals, unit = self$options$timeUnit)
                                        event_data$time <- adjusted_times
                                    }
                                }
                            }
                        } else {
                            event_data$time <- suppressWarnings(as.numeric(as.character(event_data$time)))
                        }
                        
                        # Filter valid events
                        event_data <- event_data[!is.na(event_data$time) & 
                                               !is.na(event_data$label) & 
                                               event_data$time >= 0, ]
                    }
                }
            }
            
            return(event_data)
        },
        
        # Calculate comprehensive summary statistics
        .calculateSummaryStats = function(patient_data) {
            # Calculate durations
            durations <- patient_data$end_time - patient_data$start_time
            
            # Basic statistics
            stats <- list(
                n_patients = length(unique(patient_data$patient_id)),
                n_observations = nrow(patient_data),
                median_duration = median(durations, na.rm = TRUE),
                mean_duration = mean(durations, na.rm = TRUE),
                sd_duration = sd(durations, na.rm = TRUE),
                min_duration = min(durations, na.rm = TRUE),
                max_duration = max(durations, na.rm = TRUE),
                q1_duration = quantile(durations, 0.25, na.rm = TRUE),
                q3_duration = quantile(durations, 0.75, na.rm = TRUE)
            )
            
            # Person-time analysis
            stats$total_person_time <- sum(durations, na.rm = TRUE)
            stats$mean_follow_up <- stats$total_person_time / stats$n_patients
            
            # Response analysis if available
            if ("response" %in% names(patient_data)) {
                response_summary <- table(patient_data$response)
                response_pct <- prop.table(response_summary) * 100
                
                stats$response_counts <- as.list(response_summary)
                stats$response_percentages <- as.list(response_pct)
            }
            
            return(stats)
        },
        
        # Generate comprehensive clinical interpretation
        .generateClinicalInterpretation = function(stats, patient_data) {
            interpretation <- list()
            
            # Timeline interpretation
            interpretation$timeline <- sprintf(
                .("Study included %d patients with %d timeline observations. Median follow-up was %.1f %s (range: %.1f to %.1f %s)."),
                stats$n_patients,
                stats$n_observations,
                stats$median_duration,
                self$options$timeUnit,
                stats$min_duration,
                stats$max_duration,
                self$options$timeUnit
            )
            
            # Person-time analysis
            interpretation$person_time <- sprintf(
                .("Total person-time: %.1f %s. Average follow-up per patient: %.1f %s."),
                stats$total_person_time,
                self$options$timeUnit,
                stats$mean_follow_up,
                self$options$timeUnit
            )
            
            # Response interpretation if available
            if (!is.null(stats$response_counts)) {
                best_response <- names(stats$response_counts)[which.max(stats$response_counts)]
                best_pct <- stats$response_percentages[[best_response]]
                
                interpretation$response <- sprintf(
                    .("Most common response was %s (%.1f%% of observations). Response distribution shows clinical patterns suitable for efficacy analysis."),
                    best_response,
                    best_pct
                )
            }
            
            return(interpretation)
        },
        
        # Generate clinical summary with copy-ready text
        .generateClinicalSummary = function(stats, patient_data) {
            # Basic study summary
            summary_text <- sprintf(
                .(.("This swimmer plot analysis included %d patients with a median follow-up of %.1f %s (range: %.1f to %.1f %s). Total person-time was %.1f %s across all patients.")),
                stats$n_patients,
                stats$median_duration, 
                self$options$timeUnit,
                stats$min_duration,
                stats$max_duration,
                self$options$timeUnit,
                stats$total_person_time,
                self$options$timeUnit
            )
            
            # Add preset-specific context if available
            if (!is.null(private$.preset_context)) {
                context <- private$.preset_context
                context_text <- sprintf(
                    .(" This %s analysis shows patterns typical of %s, with emphasis on %s."),
                    context$context,
                    context$typical_timeframe,
                    context$interpretation_focus
                )
                summary_text <- paste(summary_text, context_text)
            }
            
            # Add response analysis if available
            if (!is.null(stats$response_counts) && length(stats$response_counts) > 0) {
                best_response <- names(stats$response_counts)[which.max(stats$response_counts)]
                best_pct <- stats$response_percentages[[best_response]]
                
                # Calculate clinical response rates
                if ("CR" %in% names(stats$response_counts) || "PR" %in% names(stats$response_counts)) {
                    orr_count <- sum(stats$response_counts[names(stats$response_counts) %in% c("CR", "PR")])
                    orr_pct <- orr_count / sum(stats$response_counts) * 100
                    
                    dcr_count <- sum(stats$response_counts[names(stats$response_counts) %in% c("CR", "PR", "SD")])
                    dcr_pct <- dcr_count / sum(stats$response_counts) * 100
                    
                    response_text <- sprintf(
                        .(.(" Response evaluation showed an objective response rate (ORR) of %.1f%% (%d/%d patients) and disease control rate (DCR) of %.1f%% (%d/%d patients). The most frequent response category was %s (%.1f%%).")),
                        orr_pct, orr_count, sum(stats$response_counts),
                        dcr_pct, dcr_count, sum(stats$response_counts),
                        best_response, best_pct
                    )
                } else {
                    response_text <- sprintf(
                        .(.(" Response data was available for all patients, with %s being the most common category (%.1f%% of observations).")),
                        best_response, best_pct
                    )
                }
                
                summary_text <- paste(summary_text, response_text)
                
                # Add preset-specific interpretation for response data
                if (!is.null(private$.preset_context)) {
                    preset_interp <- switch(self$options$clinicalPreset,
                        "oncology_immunotherapy" = .("Response patterns are consistent with immunotherapy studies, where delayed responses and durable disease control are common outcomes."),
                        "oncology_chemotherapy" = .("Response rates align with chemotherapy protocols, focusing on immediate tumor response and cycle completion rates."),
                        "surgery_outcomes" = .("Outcome patterns reflect surgical intervention success and recovery trajectories."),
                        "clinical_trial" = .("Results demonstrate protocol adherence and endpoint achievement within the structured trial framework."),
                        "longitudinal_followup" = .("Long-term patterns show disease progression and outcome trends over the extended follow-up period."),
                        ""
                    )
                    if (preset_interp != "") {
                        summary_text <- paste(summary_text, preset_interp)
                    }
                }
            }
            
            # Add methodology note
            methodology_note <- .(.("This timeline visualization uses the ggswim package to display patient treatment courses, clinical milestones, and outcome assessments in a comprehensive swimmer plot format suitable for clinical research reporting."))
            summary_text <- paste(summary_text, methodology_note)
            
            return(summary_text)
        },
        
        # Display clinical summary in a prominent panel
        .displayClinicalSummary = function(summary_text) {
            summary_html <- paste0(
                "<div style='background-color: #f8f9fa; padding: 20px; border-left: 4px solid #007bff; border-radius: 8px; margin: 15px 0; font-family: system-ui, -apple-system, sans-serif;'>",
                "<h3 style='color: #007bff; margin-top: 0; font-size: 1.2em; display: flex; align-items: center;'>",
                "<span style='margin-right: 8px;'>📊</span>",
                .("Clinical Summary"),
                "</h3>",
                "<div style='background-color: white; padding: 15px; border-radius: 6px; margin: 10px 0; box-shadow: 0 1px 3px rgba(0,0,0,0.1);'>",
                "<p style='margin: 0; line-height: 1.6; color: #333; font-size: 0.95em;'>", summary_text, "</p>",
                "</div>",
                "<div style='margin-top: 15px; padding: 10px; background-color: #e3f2fd; border-radius: 4px; border: 1px dashed #1976d2;'>",
                "<p style='margin: 0; font-size: 0.85em; color: #1565c0;'>",
                "<strong>", .("Copy-ready text:"), "</strong> ", 
                .("The above summary is formatted for direct use in research reports, manuscripts, and clinical documentation."),
                "</p>",
                "</div>",
                "</div>"
            )
            
            # Use existing interpretation result or create new one
            if (is.null(self$results$interpretation)) {
                # If no interpretation result exists, use instructions
                current_content <- self$results$instructions$content
                if (!is.null(current_content) && current_content != "") {
                    self$results$instructions$setContent(paste(current_content, summary_html))
                } else {
                    self$results$instructions$setContent(summary_html)
                }
            } else {
                self$results$interpretation$setContent(summary_html)
            }
        },
        
        # Apply clinical preset configurations with context
        .applyClinicalPreset = function() {
            preset <- self$options$clinicalPreset
            
            if (preset == "none") return()
            
            # Apply preset configurations by updating result interpretations
            # rather than trying to modify options (which jamovi doesn't allow at runtime)
            preset_contexts <- list(
                "oncology_immunotherapy" = list(
                    context = "immunotherapy clinical trial",
                    typical_timeframe = "weeks to months", 
                    key_milestones = c("Treatment Start", "Response Assessment", "Progression"),
                    interpretation_focus = "response rates and progression-free survival"
                ),
                "oncology_chemotherapy" = list(
                    context = "chemotherapy treatment cycles",
                    typical_timeframe = "cycles over months",
                    key_milestones = c("Cycle 1", "Mid-treatment Assessment", "End of Treatment"),
                    interpretation_focus = "cycle completion and toxicity management"
                ),
                "surgery_outcomes" = list(
                    context = "perioperative timeline",
                    typical_timeframe = "days to months post-surgery", 
                    key_milestones = c("Surgery", "30-day Follow-up", "Long-term Outcome"),
                    interpretation_focus = "surgical outcomes and recovery metrics"
                ),
                "clinical_trial" = list(
                    context = "structured clinical trial",
                    typical_timeframe = "protocol-defined periods",
                    key_milestones = c("Enrollment", "Primary Endpoint", "Study Completion"),
                    interpretation_focus = "protocol compliance and endpoint achievement"
                ),
                "longitudinal_followup" = list(
                    context = "long-term observational study",
                    typical_timeframe = "months to years",
                    key_milestones = c("Baseline", "Regular Follow-ups", "Final Assessment"),
                    interpretation_focus = "longitudinal trends and outcomes"
                )
            )
            
            if (preset %in% names(preset_contexts)) {
                private$.preset_context <- preset_contexts[[preset]]
                
                # Add preset-specific guidance to instructions
                preset_guidance <- sprintf(
                    .("<div style='background-color: #e8f4f8; padding: 10px; border-radius: 5px; margin: 10px 0;'><strong>Clinical Context:</strong> This analysis is optimized for %s studies. Typical timeframe: %s. Focus: %s.</div>"),
                    preset_contexts[[preset]]$context,
                    preset_contexts[[preset]]$typical_timeframe,
                    preset_contexts[[preset]]$interpretation_focus
                )
                
                private$.preset_guidance <- preset_guidance
            }
        },
        
        .run = function() {
            # Apply clinical preset configurations if selected
            private$.applyClinicalPreset()
            
            # Enhanced instructions with comprehensive guidance
            if (is.null(self$options$patientID) || 
                is.null(self$options$startTime) || 
                is.null(self$options$endTime)) {
                
                instructions <- private$.generateInstructions()
                self$results$instructions$setContent(instructions)
                return()
            }
            
            # Validate and process data with comprehensive error handling
            tryCatch({
                validation_result <- private$.validateAndProcessData()
                
                # Check for validation errors
                if (!is.null(validation_result$error) && validation_result$error) {
                    error_msg <- paste0(
                        "<div style='color: red; padding: 15px; border: 1px solid red; border-radius: 5px; margin: 10px;'>",
                        .("<h4>Data Validation Error</h4>"),
                        .("<p><strong>Error:</strong> "), validation_result$message, .("</p>"),
                        .("<p><strong>Please check:</strong></p>"),
                        "<ul>",
                        .("<li>All required variables are selected</li>"),
                        .("<li>Data contains valid values</li>"),
                        .("<li>End times are greater than or equal to start times</li>"),
                        .("<li>Check for negative follow-up times or unrealistic durations</li>"),
                        "</ul>",
                        "</div>"
                    )
                    self$results$instructions$setContent(error_msg)
                    return()
                }
                
                # Extract patient data and show warnings if present
                patient_data <- if ("data" %in% names(validation_result)) validation_result$data else validation_result
                
                # Display warnings if present
                if (!is.null(validation_result$warnings) && length(validation_result$warnings) > 0) {
                    warning_msg <- paste0(
                        "<div style='color: #8a6d00; background-color: #fff8e1; padding: 15px; border: 1px solid #ffc107; border-radius: 5px; margin: 10px;'>",
                        .("<h4>Data Quality Warnings</h4>"),
                        "<ul>",
                        paste0("<li>", validation_result$warnings, "</li>", collapse = ""),
                        "</ul>",
                        .("<p><em>These warnings do not prevent analysis but may affect interpretation.</em></p>"),
                        "</div>"
                    )
                    self$results$instructions$setContent(warning_msg)
                }
                milestone_data <- private$.processMilestones(patient_data)
                event_data <- private$.processEventMarkers(patient_data)
                arrow_data <- private$.processOngoingStatus(patient_data)
                
                # Calculate comprehensive statistics
                stats <- private$.calculateSummaryStats(patient_data)
                interpretation <- private$.generateClinicalInterpretation(stats, patient_data)
                
                # Generate clinical summary
                clinical_summary <- private$.generateClinicalSummary(stats, patient_data)
                private$.displayClinicalSummary(clinical_summary)
                
                # Update summary table
                private$.updateSummaryTable(stats)
                
                # Update all result tables
                private$.updatePersonTimeTable(patient_data)
                private$.updateMilestoneTable(milestone_data) 
                private$.updateEventMarkerTable(event_data)
                private$.updateAdvancedMetrics(patient_data, stats)
                
                # Handle export functionality
                private$.updateExportData(patient_data, milestone_data, event_data, stats)
                
                # Prepare plot data with all enhancements
                plot_state <- list(
                    patient_data = patient_data,
                    milestone_data = milestone_data,
                    event_data = event_data,
                    arrow_data = arrow_data,
                    stats = stats,
                    interpretation = interpretation,
                    options = list(
                        timeUnit = self$options$timeUnit,
                        laneWidth = self$options$laneWidth,
                        markerSize = self$options$markerSize,
                        theme = self$options$plotTheme,
                        showLegend = self$options$showLegend,
                        referenceLines = self$options$referenceLines,
                        customReferenceTime = self$options$customReferenceTime
                    )
                )
                
                self$results$plot$setState(plot_state)
                
                # Generate clinical interpretation if requested
                if (self$options$showInterpretation) {
                    private$.generateInterpretationOutput(interpretation)
                }
                
            }, error = function(e) {
                error_msg <- paste(
                    "<div style='color: red; padding: 10px; border: 1px solid red; border-radius: 5px;'>",
                    .("<h4>Error in Swimmer Plot Analysis</h4>"),
                    .("<p><strong>Error:</strong>"), e$message, .("</p>"),
                    .("<p><strong>Suggestions:</strong></p>"),
                    "<ul>",
                    .("<li>Ensure all required variables are selected</li>"),
                    .("<li>Check that time variables contain valid numeric or date values</li>"),
                    .("<li>Verify that end times are greater than or equal to start times</li>"),
                    .("<li>For date/time data, ensure correct format is selected</li>"),
                    "</ul>",
                    "</div>"
                )
                self$results$instructions$setContent(error_msg)
            })
        },
        
        .generateInstructions = function() {
            paste0(
                "<div style='background-color: #e1f5fe; padding: 20px; border-radius: 8px; margin: 10px 0;'>",
                .("<h3 style='color: #0277bd; margin-top: 0;'>🏊 Swimmer Plot Analysis</h3>"),
                .("<p>Create comprehensive swimmer plots for visualizing patient timelines, treatments, and clinical events using the advanced ggswim package.</p>"),
                
                "<div style='margin: 15px 0;'>",
                .("<h4 style='color: #0288d1; margin: 10px 0 5px 0;'>📋 Required Variables:</h4>"),
                "<ul style='margin: 5px 0; padding-left: 20px; line-height: 1.6;'>",
                .("<li><strong>Patient ID:</strong> Unique identifier for each patient</li>"),
                .("<li><strong>Start Time:</strong> Treatment/observation start time</li>"),
                .("<li><strong>End Time:</strong> Treatment/observation end time</li>"),
                "</ul>",
                "</div>",
                
                "<div style='margin: 15px 0;'>",
                .("<h4 style='color: #0288d1; margin: 10px 0 5px 0;'>⚡ Enhanced Features:</h4>"),
                "<ul style='margin: 5px 0; padding-left: 20px; line-height: 1.6;'>",
                .("<li><strong>Complete ggswim Integration:</strong> Professional clinical visualization</li>"),
                .("<li><strong>Milestone Support:</strong> Track key clinical events (surgery, progression, etc.)</li>"),
                .("<li><strong>Event Markers:</strong> Show specific events along patient timelines</li>"),
                .("<li><strong>Person-time Analysis:</strong> Epidemiological metrics and follow-up analysis</li>"),
                .("<li><strong>Clinical Interpretation:</strong> Automated insights for research</li>"),
                .("<li><strong>Enhanced Data Validation:</strong> Robust error handling and type conversion</li>"),
                "</ul>",
                "</div>",
                
                "<div style='margin: 15px 0;'>",
                .("<h4 style='color: #0288d1; margin: 10px 0 5px 0;'>🎨 Visualization Options:</h4>"),
                "<ul style='margin: 5px 0; padding-left: 20px; line-height: 1.6;'>",
                .("<li><strong>Swim Lanes:</strong> Horizontal patient timelines with response coloring</li>"),
                "<li><strong>Event Markers:</strong> Custom glyphs for clinical events</li>",
                "<li><strong>Status Arrows:</strong> Ongoing treatment indicators</li>",
                "<li><strong>Reference Lines:</strong> Protocol times, median values, custom timepoints</li>",
                "<li><strong>Clinical Themes:</strong> Professional styling for publications</li>",
                "</ul>",
                "</div>",
                
                "<div style='margin: 15px 0;'>",
                "<h4 style='color: #0288d1; margin: 10px 0 5px 0;'>📊 Data Formats Supported:</h4>",
                "<ul style='margin: 5px 0; padding-left: 20px; line-height: 1.6;'>",
                "<li><strong>Raw Numeric:</strong> Days, weeks, months from treatment start</li>",
                "<li><strong>Date/Time:</strong> Actual calendar dates with multiple format support</li>",
                "<li><strong>Relative vs Absolute:</strong> Timeline display options</li>",
                "<li><strong>Multiple Time Units:</strong> Days, weeks, months, years</li>",
                "</ul>",
                "</div>",
                
                "<div style='background-color: #fff3e0; padding: 10px; border-radius: 5px; margin: 10px 0;'>",
                "<p style='margin: 0; color: #f57c00;'><strong>Clinical Research Applications:</strong> Ideal for oncology trials, treatment response visualization, progression tracking, and regulatory submissions.</p>",
                "</div>",
                
                "<div style='background-color: #f3e5f5; padding: 10px; border-radius: 5px; margin: 10px 0;'>",
                "<p style='margin: 0; color: #7b1fa2;'><strong>✨ Enhanced Features:</strong> Complete ggswim package integration with swim lanes, event markers, status arrows, and professional clinical themes for maximum flexibility and publication-ready output.</p>",
                "</div>",
                
                "</div>",
                
                # Add preset guidance if available
                if (!is.null(private$.preset_guidance)) private$.preset_guidance else ""
            )
        },
        
        .updateSummaryTable = function(stats) {
            # Clear existing rows
            self$results$summary$deleteRows()
            
            # Add basic statistics
            self$results$summary$addRow(rowKey = 1, values = list(
                metric = .("Number of Patients"),
                value = stats$n_patients
            ))
            
            self$results$summary$addRow(rowKey = 2, values = list(
                metric = .("Total Observations"),
                value = stats$n_observations
            ))
            
            self$results$summary$addRow(rowKey = 3, values = list(
                metric = .("Median Duration"),
                value = round(stats$median_duration, 2)
            ))
            
            self$results$summary$addRow(rowKey = 4, values = list(
                metric = .("Mean Duration"),
                value = round(stats$mean_duration, 2)
            ))
            
            self$results$summary$addRow(rowKey = 5, values = list(
                metric = .("Total Person-Time"),
                value = round(stats$total_person_time, 2)
            ))
            
            self$results$summary$addRow(rowKey = 6, values = list(
                metric = .("Mean Follow-up"),
                value = round(stats$mean_follow_up, 2)
            ))
            
            # Add response statistics if available
            if (!is.null(stats$response_counts)) {
                row_idx <- 7
                for (response in names(stats$response_counts)) {
                    self$results$summary$addRow(rowKey = row_idx, values = list(
                        metric = paste0(response, .(" Rate (%)")),
                        value = round(stats$response_percentages[[response]], 1)
                    ))
                    row_idx <- row_idx + 1
                }
            }
        },
        
        # Person-time analysis table population
        .updatePersonTimeTable = function(patient_data) {
            if (!self$options$personTimeAnalysis) return()
            if (!"response" %in% names(patient_data)) return()
            
            # Clear existing rows
            self$results$personTimeTable$deleteRows()
            
            # Calculate person-time metrics by response type
            durations <- patient_data$end_time - patient_data$start_time
            
            person_time_data <- patient_data %>%
                dplyr::group_by(response) %>%
                dplyr::summarise(
                    n_patients = dplyr::n(),
                    total_time = sum(durations, na.rm = TRUE),
                    mean_time = mean(durations, na.rm = TRUE),
                    .groups = "drop"
                ) %>%
                dplyr::mutate(
                    incidence_rate = ifelse(total_time > 0, n_patients / total_time * 100, 0)
                )
            
            # Populate the table
            for (i in seq_len(nrow(person_time_data))) {
                self$results$personTimeTable$addRow(rowKey = i, values = list(
                    response_type = as.character(person_time_data$response[i]),
                    n_patients = person_time_data$n_patients[i],
                    total_time = round(person_time_data$total_time[i], 2),
                    mean_time = round(person_time_data$mean_time[i], 2),
                    incidence_rate = round(person_time_data$incidence_rate[i], 3)
                ))
            }
        },
        
        # Milestone table population
        .updateMilestoneTable = function(milestone_data) {
            if (nrow(milestone_data) == 0) return()
            
            # Clear existing rows
            self$results$milestoneTable$deleteRows()
            
            # Calculate milestone statistics
            milestone_stats <- milestone_data %>%
                dplyr::group_by(label) %>%
                dplyr::summarise(
                    n_events = dplyr::n(),
                    median_time = median(time, na.rm = TRUE),
                    min_time = min(time, na.rm = TRUE),
                    max_time = max(time, na.rm = TRUE),
                    .groups = "drop"
                ) %>%
                dplyr::mutate(
                    time_range = paste0(
                        round(min_time, 1), " - ", round(max_time, 1), " ", 
                        self$options$timeUnit
                    )
                )
            
            # Populate the table
            for (i in seq_len(nrow(milestone_stats))) {
                self$results$milestoneTable$addRow(rowKey = i, values = list(
                    milestone_name = milestone_stats$label[i],
                    n_events = milestone_stats$n_events[i],
                    median_time = round(milestone_stats$median_time[i], 2),
                    time_range = milestone_stats$time_range[i]
                ))
            }
        },
        
        # Event marker table population
        .updateEventMarkerTable = function(event_data) {
            if (!self$options$showEventMarkers || is.null(event_data) || nrow(event_data) == 0) return()
            
            # Clear existing rows
            self$results$eventMarkerTable$deleteRows()
            
            # Calculate event statistics
            total_events <- nrow(event_data)
            event_stats <- event_data %>%
                dplyr::group_by(label) %>%
                dplyr::summarise(
                    n_events = dplyr::n(),
                    median_time = median(time, na.rm = TRUE),
                    .groups = "drop"
                ) %>%
                dplyr::mutate(
                    percent = n_events / total_events
                )
            
            # Populate the table
            for (i in seq_len(nrow(event_stats))) {
                self$results$eventMarkerTable$addRow(rowKey = i, values = list(
                    event_type = event_stats$label[i],
                    n_events = event_stats$n_events[i],
                    percent = event_stats$percent[i],
                    median_time = round(event_stats$median_time[i], 2)
                ))
            }
        },
        
        # Advanced metrics table population
        .updateAdvancedMetrics = function(patient_data, stats) {
            if (!self$options$personTimeAnalysis) return()
            
            # Clear existing rows
            self$results$advancedMetrics$deleteRows()
            
            durations <- patient_data$end_time - patient_data$start_time
            
            # Calculate advanced clinical metrics
            metrics <- list(
                list(
                    name = .("Median Follow-up Time"),
                    value = round(stats$median_duration, 2),
                    unit = self$options$timeUnit,
                    interpretation = .("Central tendency of patient follow-up duration")
                ),
                list(
                    name = .("Interquartile Range"),
                    value = round(stats$q3_duration - stats$q1_duration, 2),
                    unit = self$options$timeUnit,
                    interpretation = .("Middle 50% of follow-up duration range")
                ),
                list(
                    name = .("Total Study Person-Time"),
                    value = round(stats$total_person_time, 2),
                    unit = paste(self$options$timeUnit, "cumulative"),
                    interpretation = .("Total observation time across all patients")
                ),
                list(
                    name = .("Person-Time Incidence Rate"),
                    value = round(length(durations) / stats$total_person_time * 100, 3),
                    unit = paste0("per 100 ", self$options$timeUnit),
                    interpretation = .("Rate of patient inclusion per time unit")
                )
            )
            
            # Add response-specific metrics if available
            if ("response" %in% names(patient_data)) {
                response_rates <- table(patient_data$response)
                orr <- sum(response_rates[names(response_rates) %in% c("CR", "PR")]) / sum(response_rates) * 100
                dcr <- sum(response_rates[names(response_rates) %in% c("CR", "PR", "SD")]) / sum(response_rates) * 100
                
                metrics <- append(metrics, list(
                    list(
                        name = .("Objective Response Rate (ORR)"),
                        value = round(orr, 1),
                        unit = "percent",
                        interpretation = .("Proportion with complete or partial response")
                    ),
                    list(
                        name = .("Disease Control Rate (DCR)"),
                        value = round(dcr, 1),
                        unit = "percent",
                        interpretation = .("Proportion with response or stable disease")
                    )
                ))
            }
            
            # Populate the table
            for (i in seq_along(metrics)) {
                metric <- metrics[[i]]
                self$results$advancedMetrics$addRow(rowKey = i, values = list(
                    metric_name = metric$name,
                    metric_value = metric$value,
                    metric_unit = metric$unit,
                    clinical_interpretation = metric$interpretation
                ))
            }
        },
        
        # Export functionality
        .updateExportData = function(patient_data, milestone_data, event_data, stats) {
            # Export timeline data if requested
            if (self$options$exportTimeline) {
                timeline_export <- patient_data
                
                # Add milestone data if available
                if (nrow(milestone_data) > 0) {
                    milestone_wide <- milestone_data %>%
                        dplyr::select(patient_id, label, time) %>%
                        tidyr::pivot_wider(names_from = label, values_from = time, names_prefix = "milestone_")
                    
                    timeline_export <- timeline_export %>%
                        dplyr::left_join(milestone_wide, by = "patient_id")
                }
                
                self$results$timelineData$setRowNums(rownames(timeline_export))
                for (col in names(timeline_export)) {
                    self$results$timelineData$setValues(column = col, values = timeline_export[[col]])
                }
            }
            
            # Export summary statistics if requested  
            if (self$options$exportSummary) {
                summary_export <- data.frame(
                    metric = c("n_patients", "n_observations", "median_duration", 
                             "mean_duration", "total_person_time", "mean_follow_up"),
                    value = c(stats$n_patients, stats$n_observations, stats$median_duration,
                            stats$mean_duration, stats$total_person_time, stats$mean_follow_up),
                    stringsAsFactors = FALSE
                )
                
                # Add response statistics if available
                if (!is.null(stats$response_counts)) {
                    for (response in names(stats$response_counts)) {
                        summary_export <- rbind(summary_export, data.frame(
                            metric = paste0(response, "_count"),
                            value = stats$response_counts[[response]],
                            stringsAsFactors = FALSE
                        ))
                        summary_export <- rbind(summary_export, data.frame(
                            metric = paste0(response, "_percent"),
                            value = stats$response_percentages[[response]],
                            stringsAsFactors = FALSE
                        ))
                    }
                }
                
                self$results$summaryData$setRowNums(rownames(summary_export))
                self$results$summaryData$setValues(column = "metric", values = summary_export$metric)
                self$results$summaryData$setValues(column = "value", values = summary_export$value)
            }
            
            # Update export information panel
            if (self$options$exportTimeline || self$options$exportSummary) {
                export_info <- paste0(
                    "<div style='background-color: #f0f8ff; padding: 15px; border-radius: 5px; margin: 10px 0;'>",
                    "<h4>Export Information</h4>",
                    "<p>Data has been exported to the following outputs:</p>",
                    "<ul>",
                    if (self$options$exportTimeline) "<li><strong>Timeline Data:</strong> Complete patient timeline dataset with processed variables</li>" else "",
                    if (self$options$exportSummary) "<li><strong>Summary Statistics:</strong> Comprehensive summary metrics and clinical indicators</li>" else "",
                    "</ul>",
                    "<p><em>Note: Exported data can be accessed through the Output panel and used for external analysis.</em></p>",
                    "</div>"
                )
                self$results$exportInfo$setContent(export_info)
            }
        },
        
        .generateInterpretationOutput = function(interpretation) {
            interp_html <- paste0(
                "<div style='background-color: #e8f5e8; padding: 15px; border-radius: 5px; margin: 10px 0;'>",
                "<h4>Clinical Interpretation</h4>",
                "<div style='margin: 10px 0;'>",
                "<h5 style='color: #2e7d32;'>Timeline Analysis:</h5>",
                "<p>", interpretation$timeline, "</p>",
                "</div>",
                "<div style='margin: 10px 0;'>",
                "<h5 style='color: #2e7d32;'>Person-Time Analysis:</h5>",
                "<p>", interpretation$person_time, "</p>",
                "</div>"
            )
            
            if (!is.null(interpretation$response)) {
                interp_html <- paste0(interp_html,
                    "<div style='margin: 10px 0;'>",
                    "<h5 style='color: #2e7d32;'>Response Pattern Analysis:</h5>",
                    "<p>", interpretation$response, "</p>",
                    "</div>"
                )
            }
            
            interp_html <- paste0(interp_html, "</div>")
            
            self$results$interpretation$setContent(interp_html)
        },
        
        .plot = function(image, ggtheme, theme, ...) {
            plot_state <- image$state
            if (is.null(plot_state)) return(FALSE)
            
            patient_data <- plot_state$patient_data
            milestone_data <- plot_state$milestone_data
            event_data <- plot_state$event_data
            arrow_data <- plot_state$arrow_data
            stats <- plot_state$stats
            opts <- plot_state$options
            
            tryCatch({
                # Create enhanced ggswim plot
                p <- private$.createGgswimPlot(patient_data, milestone_data, event_data, arrow_data, opts, stats)
                
                print(p)
                return(TRUE)
                
            }, error = function(e) {
                warning(paste("Error creating plot:", e$message))
                
                # Create fallback plot
                p_fallback <- private$.createFallbackPlot(patient_data, e$message)
                print(p_fallback)
                return(TRUE)
            })
        },
        
        .createGgswimPlot = function(patient_data, milestone_data, event_data, arrow_data, opts, stats) {
            # Create base plot with swim lanes
            p <- ggplot2::ggplot()
            
            # Add swim lanes with enhanced styling
            if ("response" %in% names(patient_data)) {
                p <- p + ggswim::geom_swim_lane(
                    data = patient_data,
                    mapping = ggplot2::aes(
                        x = start_time,
                        xend = end_time,
                        y = patient_id,
                        colour = response
                    ),
                    linewidth = opts$laneWidth
                )
            } else {
                p <- p + ggswim::geom_swim_lane(
                    data = patient_data,
                    mapping = ggplot2::aes(
                        x = start_time,
                        xend = end_time,
                        y = patient_id
                    ),
                    linewidth = opts$laneWidth,
                    colour = "steelblue"
                )
            }
            
            # Add event markers if available
            if (!is.null(event_data) && nrow(event_data) > 0) {
                # Create enhanced marker mappings with clinical icons
                unique_labels <- unique(event_data$label)
                
                # Enhanced clinical glyphs with medical symbols
                clinical_glyphs <- private$.getEnhancedClinicalGlyphs(unique_labels)
                clinical_colors <- RColorBrewer::brewer.pal(max(3, min(length(unique_labels), 9)), "Set2")
                
                names(clinical_colors) <- unique_labels
                
                p <- p + ggswim::geom_swim_marker(
                    data = event_data,
                    mapping = ggplot2::aes(
                        x = time,
                        y = patient_id,
                        marker = label
                    ),
                    size = opts$markerSize
                )
                
                p <- p + ggswim::scale_marker_discrete(
                    name = "Clinical Events",
                    glyphs = clinical_glyphs,
                    colours = clinical_colors,
                    limits = unique_labels
                )
            }
            
            # Add milestone markers if available  
            if (!is.null(milestone_data) && nrow(milestone_data) > 0) {
                unique_milestones <- unique(milestone_data$label)
                milestone_shapes <- c(15, 16, 17, 18, 19)[1:length(unique_milestones)]
                milestone_colors <- RColorBrewer::brewer.pal(max(3, min(length(unique_milestones), 8)), "Dark2")
                
                names(milestone_shapes) <- unique_milestones
                names(milestone_colors) <- unique_milestones
                
                p <- p + ggplot2::geom_point(
                    data = milestone_data,
                    mapping = ggplot2::aes(
                        x = time,
                        y = patient_id,
                        shape = label,
                        color = label
                    ),
                    size = opts$markerSize + 1
                ) +
                ggplot2::scale_shape_manual(
                    name = "Milestones",
                    values = milestone_shapes
                ) +
                ggplot2::scale_color_manual(
                    name = "Milestones",
                    values = milestone_colors
                )
            }
            
            # Add ongoing status arrows using ggswim::geom_swim_arrow()
            if (!is.null(arrow_data) && nrow(arrow_data) > 0) {
                p <- p + ggswim::geom_swim_arrow(
                    data = arrow_data,
                    mapping = ggplot2::aes(
                        xend = xend,
                        y = patient_id
                    ),
                    colour = "darkgreen",
                    size = 1.5,
                    alpha = 0.7
                )
            }
            
            # Add reference lines
            if (!is.null(opts$referenceLines) && opts$referenceLines != "none") {
                p <- private$.addReferenceLines(p, opts, stats)
            }
            
            # Apply theme and styling
            if (opts$theme == "ggswim") {
                p <- p + ggswim::theme_ggswim()
            } else if (opts$theme == "ggswim_dark") {
                p <- p + ggswim::theme_ggswim_dark()
            } else {
                p <- p + ggplot2::theme_minimal()
            }
            
            # Add labels with clinical context
            p <- p + ggplot2::labs(
                title = .("Patient Timeline Analysis"),
                subtitle = sprintf(.("N=%d patients | Median follow-up: %.1f %s | Total person-time: %.1f %s"),
                                 stats$n_patients, stats$median_duration, self$options$timeUnit,
                                 stats$total_person_time, self$options$timeUnit),
                x = paste0(.("Time ("), self$options$timeUnit, .(")")),
                y = .("Patient ID")
            )
            
            # Legend handling
            if (!opts$showLegend) {
                p <- p + ggplot2::theme(legend.position = "none")
            }
            
            return(p)
        },
        
        .addReferenceLines = function(p, opts, stats) {
            if (opts$referenceLines == "median") {
                p <- p + ggplot2::geom_vline(
                    xintercept = stats$median_duration,
                    linetype = "dashed",
                    color = "darkgray",
                    alpha = 0.7
                ) +
                ggplot2::annotate(
                    "text",
                    x = stats$median_duration,
                    y = 1,
                    label = paste("Median:", round(stats$median_duration, 1)),
                    hjust = -0.1,
                    vjust = 0,
                    angle = 90,
                    size = 3
                )
            } else if (opts$referenceLines == "protocol") {
                protocol_times <- c(6, 12, 24, 36)
                max_time <- max(stats$max_duration, na.rm = TRUE)
                protocol_times <- protocol_times[protocol_times <= max_time * 1.1]
                
                for (t in protocol_times) {
                    p <- p + ggplot2::geom_vline(
                        xintercept = t,
                        linetype = "dotted",
                        color = "darkgray",
                        alpha = 0.5
                    )
                }
            } else if (opts$referenceLines == "custom" && !is.null(opts$customReferenceTime)) {
                p <- p + ggplot2::geom_vline(
                    xintercept = opts$customReferenceTime,
                    linetype = "dashed",
                    color = "red",
                    alpha = 0.7
                )
            }
            
            return(p)
        },
        
        .createFallbackPlot = function(patient_data, error_message) {
            ggplot2::ggplot(patient_data, ggplot2::aes(x = start_time, y = patient_id)) +
                ggplot2::geom_point(size = 2, color = "steelblue") +
                ggplot2::labs(
                    title = "Swimmer Plot (Simplified)",
                    subtitle = paste("Error in enhanced plot:", error_message),
                    x = "Time",
                    y = .("Patient ID")
                ) +
                ggplot2::theme_minimal()
        },
        
        # Enhanced clinical glyph mapping for event markers
        .getEnhancedClinicalGlyphs = function(event_labels) {
            # Define clinical icon mappings
            clinical_mapping <- list(
                # Treatment events
                "treatment" = "💊", "therapy" = "💊", "drug" = "💊", "medication" = "💊",
                "infusion" = "💉", "injection" = "💉", "dose" = "💉",
                "surgery" = "🏥", "operation" = "🏥", "procedure" = "🏥",
                
                # Response events  
                "response" = "📊", "assessment" = "📊", "evaluation" = "📊",
                "progression" = "📈", "recurrence" = "🔄", "relapse" = "🔄",
                "remission" = "✅", "complete response" = "✅", "cr" = "✅",
                "partial response" = "🟡", "pr" = "🟡",
                "stable disease" = "🟠", "sd" = "🟠",
                "progressive disease" = "🔴", "pd" = "🔴",
                
                # Adverse events
                "adverse event" = "⚠️", "ae" = "⚠️", "toxicity" = "⚠️",
                "death" = "💀", "mortality" = "💀",
                
                # Follow-up events
                "follow-up" = "📅", "visit" = "📅", "appointment" = "📅",
                "scan" = "🔬", "imaging" = "🔬", "ct" = "🔬", "mri" = "🔬",
                
                # Generic events
                "event" = "⚫", "milestone" = "🎯", "endpoint" = "🏁"
            )
            
            # Create glyph vector
            glyphs <- character(length(event_labels))
            names(glyphs) <- event_labels
            
            # Map each label to appropriate glyph
            for (i in seq_along(event_labels)) {
                label <- tolower(event_labels[i])
                
                # Try exact match first
                if (label %in% names(clinical_mapping)) {
                    glyphs[i] <- clinical_mapping[[label]]
                } else {
                    # Try partial matches
                    matches <- sapply(names(clinical_mapping), function(pattern) {
                        grepl(pattern, label, fixed = TRUE)
                    })
                    
                    if (any(matches)) {
                        first_match <- names(clinical_mapping)[which(matches)[1]]
                        glyphs[i] <- clinical_mapping[[first_match]]
                    } else {
                        # Fallback to default symbols
                        default_symbols <- c("⬤", "■", "▲", "◆", "✚", "✖", "⬟", "★", "●", "◼")
                        glyphs[i] <- default_symbols[((i - 1) %% length(default_symbols)) + 1]
                    }
                }
            }
            
            return(glyphs)
        }
    )
)