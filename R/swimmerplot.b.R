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

swimmerplotClass <- if (requireNamespace('jmvcore', quietly = TRUE)) R6::R6Class(
    "swimmerplotClass",
    inherit = swimmerplotBase,
    private = list(
        # Clinical preset context storage
        .preset_context = NULL,
        .preset_guidance = NULL,

        # Auto-detection variables (simplified since we now stop analysis)
        .auto_detected_dates = FALSE,
        .detected_format = NULL,

        # Escape variable names for safe handling
        .escapeVar = function(x) {
            # Handle variables with spaces/special characters
            gsub("[^A-Za-z0-9_]+", "_", make.names(x))
        },

        # Enhanced clinical date parsing with contextual guidance
        .parseDatesWithClinicalContext = function(dates, format, variable_type = "time") {
            if (inherits(dates, c("Date", "POSIXct", "POSIXlt"))) {
                return(list(value = dates, error = FALSE, message = NULL))
            }

            # Check if data appears to be numeric when Date/Time is selected
            sample_data <- as.character(dates[!is.na(dates)][1:min(5, sum(!is.na(dates)))])
            is_numeric_like <- length(sample_data) > 0 && all(grepl("^-?\\d*\\.?\\d+$", sample_data))

            if (is_numeric_like) {
                # User selected Date/Time but data is numeric - provide guidance
                return(list(
                    value = NULL,
                    error = TRUE,
                    data_type_mismatch = TRUE,
                    detected_type = "numeric",
                    examples = sample_data[1:min(3, length(sample_data))],
                    message = paste(
                        "Data type mismatch detected:",
                        paste0("Your ", variable_type, " variables contain numeric values (",
                               paste(sample_data[1:min(3, length(sample_data))], collapse = ", "), ")"),
                        "but you have selected 'Date/Time' as the Time Input Type."
                    )
                ))
            }

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

                na_value <- if (format %in% c("ymdhms")) {
                    as.POSIXct(rep(NA_character_, length(dates)))
                } else {
                    as.Date(rep(NA_character_, length(dates)))
                }

                return(list(
                    value = na_value,
                    error = TRUE,
                    message = paste(
                        .("Error parsing"), variable_type, .("dates with format"), format, ".",
                        clinical_guidance
                    )
                ))
            }

            list(value = parsed, error = FALSE, message = NULL)
        },

        # Parse a single custom reference date string using selected dateFormat
        .parseCustomReferenceDate = function(date_str) {
            if (is.null(date_str)) return(NULL)
            if (!is.character(date_str)) return(NULL)
            if (length(date_str) == 0 || nchar(trimws(date_str)) == 0) return(NULL)
            ds <- trimws(date_str)
            parsed <- tryCatch({
                switch(self$options$dateFormat,
                    "ymdhms" = lubridate::ymd_hms(ds),
                    "ymd"    = lubridate::ymd(ds),
                    "ydm"    = lubridate::ydm(ds),
                    "mdy"    = lubridate::mdy(ds),
                    "myd"    = lubridate::myd(ds),
                    "dmy"    = lubridate::dmy(ds),
                    "dym"    = lubridate::dym(ds),
                    suppressWarnings(lubridate::ymd(ds))
                )
            }, error = function(e) NA)
            if (is.na(parsed)) return(NULL)
            parsed
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
                    patient_id = as.character(df[[private$.escapeVar(self$options$patientID)]]),
                    start_time = df[[private$.escapeVar(self$options$startTime)]],
                    end_time = df[[private$.escapeVar(self$options$endTime)]],
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
                    patient_data$start_time, 
                    self$options$dateFormat,
                    "start"
                )
                end_parsed <- private$.parseDatesWithClinicalContext(
                    patient_data$end_time, 
                    self$options$dateFormat,
                    "end"
                )
                
                # Check for data type mismatch first (user selected Date/Time but data is numeric)
                if (isTRUE(start_parsed$data_type_mismatch) || isTRUE(end_parsed$data_type_mismatch)) {
                    mismatch_info <- if (isTRUE(start_parsed$data_type_mismatch)) start_parsed else end_parsed
                    return(list(
                        data_type_mismatch = TRUE,
                        detected_type = mismatch_info$detected_type,
                        examples = mismatch_info$examples,
                        message = mismatch_info$message
                    ))
                }

                # Check for other parsing errors
                if (isTRUE(start_parsed$error)) {
                    return(list(error = TRUE, message = paste(.("Start time parsing:"), start_parsed$message)))
                }
                if (isTRUE(end_parsed$error)) {
                    return(list(error = TRUE, message = paste(.("End time parsing:"), end_parsed$message)))
                }
                
                patient_data$start_time <- start_parsed$value
                patient_data$end_time <- end_parsed$value
                
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
                # Enhanced date format detection
                start_sample <- as.character(patient_data$start_time[1:min(3, nrow(patient_data))])

                # Detect various date formats
                date_patterns <- list(
                    "YYYY-MM-DD" = "^\\d{4}-\\d{2}-\\d{2}",
                    "MM/DD/YYYY" = "^\\d{2}/\\d{2}/\\d{4}",
                    "DD/MM/YYYY" = "^\\d{2}/\\d{2}/\\d{4}",
                    "YYYY/MM/DD" = "^\\d{4}/\\d{2}/\\d{2}"
                )

                detected_format <- NULL
                parse_function <- NULL

                for (format_name in names(date_patterns)) {
                    if (any(grepl(date_patterns[[format_name]], start_sample))) {
                        detected_format <- format_name
                        parse_function <- switch(format_name,
                            "YYYY-MM-DD" = "ymd",
                            "MM/DD/YYYY" = "mdy",
                            "DD/MM/YYYY" = "dmy",
                            "YYYY/MM/DD" = "ymd"
                        )
                        break
                    }
                }

                is_date_like <- !is.null(detected_format)

                if (is_date_like) {
                    # Return special flag to indicate date detection (not an error)
                    return(list(
                        date_detected = TRUE,
                        format = detected_format,
                        examples = start_sample[1:min(2, length(start_sample))]
                    ))
                } else {
                    # Raw numeric processing with robust conversion
                    patient_data$start_time <- suppressWarnings(as.numeric(as.character(patient_data$start_time)))
                    patient_data$end_time <- suppressWarnings(as.numeric(as.character(patient_data$end_time)))
                }
            }
            
            # Add response/status variable if provided
            if (!is.null(self$options$responseVar)) {
                patient_data$response <- as.factor(df[[private$.escapeVar(self$options$responseVar)]])
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
                    
                    milestone_dates <- self$data[[private$.escapeVar(self$options[[date_opt]])]]
                    
                    # Skip if all NA
                    if (all(is.na(milestone_dates))) next
                    
                    # Process dates
                    if (self$options$timeType == "datetime") {
                    parsed_dates <- private$.parseDatesWithClinicalContext(
                        milestone_dates,
                        self$options$dateFormat,
                        "milestone"
                    )
                    milestone_dates <- parsed_dates$value
                        
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
                        # Check if milestone dates are date-like even in raw mode
                        sample_dates <- as.character(milestone_dates[1:min(3, length(milestone_dates))])
                        is_milestone_date_like <- any(grepl("^\\d{4}-\\d{2}-\\d{2}", sample_dates[!is.na(sample_dates)]))

                        if (is_milestone_date_like && "original_start" %in% names(patient_data)) {
                            # Parse as dates and convert to relative time
                            # Use the same format detected for main timeline data
                            milestone_parse_function <- if (!is.null(private$.detected_format)) {
                                switch(private$.detected_format,
                                    "YYYY-MM-DD" = "ymd",
                                    "MM/DD/YYYY" = "mdy",
                                    "DD/MM/YYYY" = "dmy",
                                    "YYYY/MM/DD" = "ymd"
                                )
                            } else "ymd"

                            parsed_dates <- private$.parseDatesWithClinicalContext(
                                milestone_dates,
                                milestone_parse_function,
                                "milestone"
                            )

                            if (!isTRUE(parsed_dates$error)) {
                                # Calculate relative time from start dates
                                valid_indices <- which(!is.na(parsed_dates$value) & seq_along(parsed_dates$value) <= nrow(patient_data))
                                if (length(valid_indices) > 0) {
                                    intervals <- lubridate::interval(
                                        patient_data$original_start[valid_indices],
                                        parsed_dates$value[valid_indices]
                                    )
                                    milestone_dates <- rep(NA_real_, length(parsed_dates$value))
                                    milestone_dates[valid_indices] <- lubridate::time_length(intervals, unit = self$options$timeUnit)
                                }
                            } else {
                                milestone_dates <- suppressWarnings(as.numeric(as.character(milestone_dates)))
                            }
                        } else {
                            milestone_dates <- suppressWarnings(as.numeric(as.character(milestone_dates)))

                            # Adjust for relative display
                            if (self$options$timeDisplay == "relative" && "start_time" %in% names(patient_data)) {
                                milestone_dates <- milestone_dates - patient_data$start_time[seq_along(milestone_dates)]
                            }
                        }
                    }
                    
                    # Create milestone dataframe - ensure consistent lengths
                    # Handle case where milestone_dates might have different length than patient_data
                    max_length <- min(length(milestone_dates), nrow(patient_data))

                    if (max_length > 0) {
                        temp_milestone <- data.frame(
                            patient_id = patient_data$patient_id[1:max_length],
                            time = milestone_dates[1:max_length],
                            label = self$options[[name_opt]],
                            milestone_type = paste0("milestone_", i),
                            stringsAsFactors = FALSE
                        )
                    } else {
                        # Create empty data frame with correct structure
                        temp_milestone <- data.frame(
                            patient_id = character(0),
                            time = numeric(0),
                            label = character(0),
                            milestone_type = character(0),
                            stringsAsFactors = FALSE
                        )
                    }
                    
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
        .processOngoingStatus = function(patient_data, stats) {
            if (nrow(patient_data) == 0 || is.null(stats)) return(NULL)

            end_numeric <- private$.asNumericTime(patient_data$end_time)
            if (all(is.na(end_numeric))) return(NULL)

            max_end <- suppressWarnings(max(end_numeric, na.rm = TRUE))
            if (!is.finite(max_end)) return(NULL)

            tolerance <- max(abs(max_end) * 1e-6, 1e-6)
            is_at_cutoff <- !is.na(end_numeric) & abs(end_numeric - max_end) <= tolerance

            ongoing_flag <- is_at_cutoff
            if ("response" %in% names(patient_data)) {
                response_labels <- tolower(as.character(patient_data$response))
                ongoing_labels <- c("cr", "complete response", "pr", "partial response",
                                    "sd", "stable disease", "ongoing", "on treatment")
                ongoing_flag <- ongoing_flag | response_labels %in% ongoing_labels
            }

            ongoing_patients <- patient_data[ongoing_flag & !is.na(patient_data$patient_id) & !is.na(patient_data$end_time), , drop = FALSE]
            if (nrow(ongoing_patients) == 0) return(NULL)

            ongoing_patients <- ongoing_patients[!duplicated(ongoing_patients$patient_id), , drop = FALSE]

            arrow_extension <- private$.computeArrowExtension(stats$max_duration)
            arrow_end <- private$.extendTimeValue(ongoing_patients$end_time, arrow_extension)

            data.frame(
                patient_id = ongoing_patients$patient_id,
                x = ongoing_patients$end_time,
                xend = arrow_end,
                stringsAsFactors = FALSE
            )
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
                        # Ensure all variables exist and get their lengths
                        patient_ids <- as.character(self$data[[private$.escapeVar(self$options$patientID)]])
                        event_times <- self$data[[private$.escapeVar(event_time_var)]]
                        event_labels <- as.character(self$data[[private$.escapeVar(event_var)]])

                        # Find the minimum length to avoid row mismatch
                        min_length <- min(length(patient_ids), length(event_times), length(event_labels))

                        if (min_length > 0) {
                            data.frame(
                                patient_id = patient_ids[1:min_length],
                                time = event_times[1:min_length],
                                label = event_labels[1:min_length],
                                stringsAsFactors = FALSE
                            )
                        } else {
                            # Return empty data frame with correct structure
                            data.frame(
                                patient_id = character(0),
                                time = numeric(0),
                                label = character(0),
                                stringsAsFactors = FALSE
                            )
                        }
                    }, error = function(e) {
                        warning(paste("Error processing event markers:", e$message))
                        return(NULL)
                    })
                    
                    if (!is.null(event_data)) {
                        # Process event times
                        if (self$options$timeType == "datetime") {
                            parsed_event_times <- private$.parseDatesWithClinicalContext(
                                event_data$time,
                                self$options$dateFormat,
                                "milestone"
                            )
                            event_data$time <- parsed_event_times$value
                            
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
                            # Check if event times are date-like even in raw mode
                            sample_times <- as.character(event_data$time[1:min(3, nrow(event_data))])
                            is_event_date_like <- any(grepl("^\\d{4}-\\d{2}-\\d{2}", sample_times[!is.na(sample_times)]))

                            if (is_event_date_like && "original_start" %in% names(patient_data)) {
                                # Parse as dates and convert to relative time
                                # Use the same format detected for main timeline data
                                event_parse_function <- if (!is.null(private$.detected_format)) {
                                    switch(private$.detected_format,
                                        "YYYY-MM-DD" = "ymd",
                                        "MM/DD/YYYY" = "mdy",
                                        "DD/MM/YYYY" = "dmy",
                                        "YYYY/MM/DD" = "ymd"
                                    )
                                } else "ymd"

                                parsed_event_times <- private$.parseDatesWithClinicalContext(
                                    event_data$time,
                                    event_parse_function,
                                    "event"
                                )

                                if (!isTRUE(parsed_event_times$error)) {
                                    # Calculate relative time from patient start dates
                                    patient_indices <- match(event_data$patient_id, patient_data$patient_id)
                                    valid_matches <- which(!is.na(patient_indices) & !is.na(parsed_event_times$value))

                                    if (length(valid_matches) > 0) {
                                        intervals <- lubridate::interval(
                                            patient_data$original_start[patient_indices[valid_matches]],
                                            parsed_event_times$value[valid_matches]
                                        )
                                        event_data$time <- rep(NA_real_, nrow(event_data))
                                        event_data$time[valid_matches] <- lubridate::time_length(intervals, unit = self$options$timeUnit)
                                    }
                                } else {
                                    event_data$time <- suppressWarnings(as.numeric(as.character(event_data$time)))
                                }
                            } else {
                                event_data$time <- suppressWarnings(as.numeric(as.character(event_data$time)))
                            }
                        }
                        
                        # Filter valid events
                        if (inherits(event_data$time, c("Date", "POSIXct"))) {
                            # Keep only events that fall within each patient's lane window when using absolute dates
                            bounds <- unique(data.frame(
                                patient_id = as.character(patient_data$patient_id),
                                start_time = patient_data$start_time,
                                end_time = patient_data$end_time,
                                stringsAsFactors = FALSE
                            ))
                            event_data <- merge(event_data, bounds, by = "patient_id", all.x = TRUE)
                            event_data <- event_data[!is.na(event_data$time) & !is.na(event_data$label), ]
                            event_data <- event_data[event_data$time >= event_data$start_time & event_data$time <= event_data$end_time, ]
                            event_data <- event_data[, c("patient_id", "time", "label")]
                        } else {
                            event_data <- event_data[!is.na(event_data$time) & 
                                                   !is.na(event_data$label) & 
                                                   event_data$time >= 0, ]
                        }
                    }
                }
            }
            
            return(event_data)
        },

        # Convert event or milestone times into numeric durations in the selected unit
        .convertTimesToNumeric = function(times, patient_ids, patient_data, unit = self$options$timeUnit) {
            if (!inherits(times, c("Date", "POSIXct", "POSIXlt"))) {
                return(suppressWarnings(as.numeric(times)))
            }

            patient_lookup <- data.frame(
                patient_id = as.character(patient_data$patient_id),
                start_time = patient_data$start_time,
                stringsAsFactors = FALSE
            )

            ids_chr <- as.character(patient_ids)
            start_vals <- patient_lookup$start_time[match(ids_chr, patient_lookup$patient_id)]

            if (!inherits(start_vals, c("Date", "POSIXct", "POSIXlt"))) {
                return(suppressWarnings(as.numeric(times)))
            }

            intervals <- suppressWarnings(lubridate::interval(start_vals, times))
            suppressWarnings(lubridate::time_length(intervals, unit = unit))
        },

        # Helper to obtain numeric durations between start and end times (per row)
        .getDurations = function(patient_data, unit = self$options$timeUnit) {
            if (inherits(patient_data$start_time, c("Date", "POSIXct", "POSIXlt"))) {
                intervals <- suppressWarnings(lubridate::interval(patient_data$start_time, patient_data$end_time))
                return(suppressWarnings(lubridate::time_length(intervals, unit = unit)))
            }

            as.numeric(patient_data$end_time - patient_data$start_time)
        },

        # Summarise timelines at the patient level to avoid double counting
        .summarizeByPatient = function(patient_data) {
            if (nrow(patient_data) == 0) {
                return(tibble::tibble(
                    patient_id = character(),
                    start_time = numeric(),
                    end_time = numeric(),
                    follow_up = numeric(),
                    person_time = numeric(),
                    response = character()
                ))
            }

            patient_data$segment_duration <- private$.getDurations(patient_data)
            split_data <- split(patient_data, patient_data$patient_id)

            summary_list <- lapply(split_data, function(df) {
                follow_up <- private$.calculateFollowUp(df$start_time, df$end_time)
                person_time <- sum(df$segment_duration, na.rm = TRUE)
                if (is.na(person_time) || !is.finite(person_time)) person_time <- follow_up

                response_value <- NA_character_
                if ("response" %in% names(df)) {
                    non_missing <- as.character(df$response[!is.na(df$response)])
                    if (length(non_missing) > 0) {
                        response_value <- non_missing[1]
                    }
                }

                start_val <- suppressWarnings(min(df$start_time, na.rm = TRUE))
                if (!is.finite(as.numeric(start_val))) start_val <- NA

                end_val <- suppressWarnings(max(df$end_time, na.rm = TRUE))
                if (!is.finite(as.numeric(end_val))) end_val <- NA

                tibble::tibble(
                    patient_id = df$patient_id[1],
                    start_time = start_val,
                    end_time = end_val,
                    follow_up = follow_up,
                    person_time = person_time,
                    response = response_value
                )
            })

            patient_data$segment_duration <- NULL
            dplyr::bind_rows(summary_list)
        },

        # Compute follow-up duration between earliest start and latest end for one patient
        .calculateFollowUp = function(start_vals, end_vals, unit = self$options$timeUnit) {
            if (length(start_vals) == 0 || length(end_vals) == 0) return(NA_real_)

            is_date <- inherits(start_vals, c("Date", "POSIXct", "POSIXlt")) ||
                       inherits(end_vals, c("Date", "POSIXct", "POSIXlt"))

            if (is_date) {
                start_min <- suppressWarnings(min(start_vals, na.rm = TRUE))
                end_max <- suppressWarnings(max(end_vals, na.rm = TRUE))
                if (!is.finite(as.numeric(start_min)) || !is.finite(as.numeric(end_max))) return(NA_real_)

                interval <- suppressWarnings(lubridate::interval(start_min, end_max))
                return(suppressWarnings(lubridate::time_length(interval, unit = unit)))
            }

            start_min <- suppressWarnings(min(as.numeric(start_vals), na.rm = TRUE))
            end_max <- suppressWarnings(max(as.numeric(end_vals), na.rm = TRUE))

            if (!is.finite(start_min) || !is.finite(end_max)) return(NA_real_)
            end_max - start_min
        },

        # Convert time-like objects to numeric for comparisons
        .asNumericTime = function(x) {
            if (inherits(x, "Date")) {
                return(as.numeric(x))
            }
            if (inherits(x, c("POSIXct", "POSIXlt"))) {
                return(as.numeric(x))
            }
            suppressWarnings(as.numeric(x))
        },

        # Extend a time value by a numeric offset based on the configured unit
        .extendTimeValue = function(values, extension, unit = self$options$timeUnit) {
            if (length(values) == 0) return(values)

            if (inherits(values, "Date")) {
                offset_days <- switch(unit,
                    days = extension,
                    weeks = extension * 7,
                    months = extension * 30.4375,
                    years = extension * 365.25,
                    extension
                )
                return(values + offset_days)
            }

            if (inherits(values, c("POSIXct", "POSIXlt"))) {
                offset_seconds <- switch(unit,
                    days = extension * 86400,
                    weeks = extension * 7 * 86400,
                    months = extension * 30.4375 * 86400,
                    years = extension * 365.25 * 86400,
                    extension
                )
                return(values + offset_seconds)
            }

            values + extension
        },

        # Derive a sensible arrow extension based on observed timelines
        .computeArrowExtension = function(max_duration) {
            if (is.null(max_duration) || is.na(max_duration) || !is.finite(max_duration)) {
                return(1)
            }

            extension <- max_duration * 0.1
            if (!is.finite(extension) || extension <= 0) extension <- 1
            extension
        },

        .getProtocolReferenceTimes = function(max_duration, unit) {
            if (is.null(max_duration) || is.na(max_duration) || !is.finite(max_duration)) {
                return(numeric(0))
            }

            base_months <- c(3, 6, 9, 12, 18, 24, 36)
            reference_values <- switch(unit,
                days = base_months * 30.4375,
                weeks = base_months * 4.34524,
                years = base_months / 12,
                base_months
            )

            reference_values[reference_values <= max_duration * 1.1]
        },

        # Calculate comprehensive summary statistics using patient-level data
        .calculateSummaryStats = function(patient_data) {
            patient_summary <- private$.summarizeByPatient(patient_data)
            follow_up_durations <- patient_summary$follow_up
            valid_follow_up <- follow_up_durations[!is.na(follow_up_durations)]

            stats <- list(
                n_patients = nrow(patient_summary),
                n_observations = nrow(patient_data),
                median_duration = if (length(valid_follow_up) > 0) stats::median(valid_follow_up) else NA_real_,
                mean_duration = if (length(valid_follow_up) > 0) mean(valid_follow_up) else NA_real_,
                sd_duration = if (length(valid_follow_up) > 1) stats::sd(valid_follow_up) else NA_real_,
                min_duration = if (length(valid_follow_up) > 0) min(valid_follow_up) else NA_real_,
                max_duration = if (length(valid_follow_up) > 0) max(valid_follow_up) else NA_real_,
                q1_duration = if (length(valid_follow_up) > 0) stats::quantile(valid_follow_up, 0.25) else NA_real_,
                q3_duration = if (length(valid_follow_up) > 0) stats::quantile(valid_follow_up, 0.75) else NA_real_,
                patient_summary = patient_summary
            )

            # Person-time analysis
            stats$total_person_time <- sum(patient_summary$person_time, na.rm = TRUE)
            stats$mean_follow_up <- if (length(valid_follow_up) > 0) mean(valid_follow_up) else NA_real_

            # Response analysis if available
            if (self$options$responseAnalysis && "response" %in% names(patient_summary)) {
                response_summary <- table(patient_summary$response, useNA = "no")
                if (length(response_summary) > 0) {
                    response_pct <- prop.table(response_summary) * 100

                    stats$response_counts <- as.numeric(response_summary)
                    names(stats$response_counts) <- names(response_summary)

                    stats$response_percentages <- as.numeric(response_pct)
                    names(stats$response_percentages) <- names(response_pct)
                }
            }

            stats
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
            if (self$options$responseAnalysis && !is.null(stats$response_counts) && length(stats$response_counts) > 0) {
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

        .init = function() {
            # Initialize instructions when no variables selected
            if (is.null(self$options$patientID) ||
                is.null(self$options$startTime) ||
                is.null(self$options$endTime)) {
                instructions <- private$.generateInstructions()
                self$results$instructions$setContent(instructions)
            }
        },

        .run = function() {
            # Apply clinical preset configurations if selected
            # DISABLED: Clinical presets only affect text interpretation, not calculations
            # private$.applyClinicalPreset()
            
            # Enhanced instructions with comprehensive guidance
            if (is.null(self$options$patientID) || 
                is.null(self$options$startTime) || 
                is.null(self$options$endTime)) {
                
                instructions <- private$.generateInstructions()
                self$results$instructions$setContent(instructions)
                return()
            }
            
            # Validate and process data with comprehensive error handling
            debug_mode <- isTRUE(getOption("swimmerplot.debug"))

            tryCatch({
                validation_result <- private$.validateAndProcessData()

                # Check for data type mismatch (Date/Time selected but numeric data)
                if (isTRUE(validation_result$data_type_mismatch)) {
                    mismatch_guidance <- paste0(
                        "<div style='font-family: Arial, sans-serif; max-width: 800px; line-height: 1.4;'>",

                        # Main mismatch notice
                        "<div style='background: #f5f5f5; border: 2px solid #d63384; padding: 20px; margin-bottom: 20px;'>",
                        "<h2 style='margin: 0 0 10px 0; font-size: 20px; color: #d63384;'>⚠️ Data Type Mismatch</h2>",
                        "<p style='margin: 0; font-size: 14px; color: #666;'>",
                        "You selected <strong style='color: #333;'>Date/Time</strong> input type, but your data contains <strong style='color: #333;'>numeric values</strong>",
                        "</p>",
                        "<p style='margin: 10px 0 0 0; font-size: 14px; color: #666;'>",
                        "Examples: <code style='background: #e9e9e9; padding: 2px 6px; border-radius: 3px; font-family: monospace;'>",
                        paste(validation_result$examples, collapse = "</code>, <code style='background: #e9e9e9; padding: 2px 6px; border-radius: 3px; font-family: monospace;'>"),
                        "</code>",
                        "</p>",
                        "</div>",

                        # Required action section
                        "<div style='background: #f9f9f9; border-left: 4px solid #d63384; padding: 15px; margin-bottom: 20px;'>",
                        "<h3 style='margin: 0 0 10px 0; color: #333; font-size: 16px;'>📋 Required Action</h3>",
                        "<ol style='margin: 0; padding-left: 20px; font-size: 14px; line-height: 1.6;'>",
                        "<li><strong>Go to 'Time & Date Settings'</strong> section (click to expand)</li>",
                        "<li><strong>Change 'Time Input Type'</strong> from 'Date/Time' to <span style='background: #e9e9e9; padding: 2px 6px; border-radius: 3px;'>Raw Values</span></li>",
                        "<li><strong>Select appropriate 'Time Unit'</strong> (Days, Weeks, Months, or Years)</li>",
                        "<li><strong>Choose your preferred 'Time Display'</strong> mode</li>",
                        "<li><strong>Re-run the analysis</strong></li>",
                        "</ol>",
                        "</div>",

                        # Helpful tip section
                        "<div style='background: #f9f9f9; border: 1px solid #ccc; padding: 15px;'>",
                        "<h4 style='margin: 0 0 10px 0; font-size: 15px; color: #333;'>💡 Data Type Guide</h4>",
                        "<p style='margin: 0; font-size: 14px; color: #666;'>",
                        "<strong>Use Date/Time for:</strong> 2023-01-15, 15/01/2023, 2023-01-15 14:30:00<br>",
                        "<strong>Use Raw Values for:</strong> 0, 30, 90.5, 365 (numeric days/months/years)",
                        "</p>",
                        "</div>",

                        "</div>"
                    )
                    self$results$instructions$setContent(mismatch_guidance)
                    return()  # Stop here, don't process further
                }

                # Check if dates were detected (not an error, just guidance needed)
                if (isTRUE(validation_result$date_detected)) {
                    date_guidance <- paste0(
                        "<div style='font-family: Arial, sans-serif; max-width: 800px; line-height: 1.4;'>",

                        # Main detection notice with clean styling like decisionpanel
                        "<div style='background: #f5f5f5; border: 2px solid #333; padding: 20px; margin-bottom: 20px;'>",
                        "<h2 style='margin: 0 0 10px 0; font-size: 20px; color: #333;'>🔍 Date Format Detected</h2>",
                        "<p style='margin: 0; font-size: 14px; color: #666;'>",
                        "Found date format: <strong style='color: #333;'>", validation_result$format, "</strong> in your time variables",
                        "</p>",
                        "<p style='margin: 10px 0 0 0; font-size: 14px; color: #666;'>",
                        "Examples: <code style='background: #e9e9e9; padding: 2px 6px; border-radius: 3px; font-family: monospace;'>",
                        paste(validation_result$examples, collapse = "</code>, <code style='background: #e9e9e9; padding: 2px 6px; border-radius: 3px; font-family: monospace;'>"),
                        "</code>",
                        "</p>",
                        "</div>",

                        # Required action section with clean border styling
                        "<div style='background: #f9f9f9; border-left: 4px solid #333; padding: 15px; margin-bottom: 20px;'>",
                        "<h3 style='margin: 0 0 10px 0; color: #333; font-size: 16px;'>📋 Required Action</h3>",
                        "<ol style='margin: 0; padding-left: 20px; font-size: 14px; line-height: 1.6;'>",
                        "<li><strong>Go to 'Time & Date Settings'</strong> section (click to expand)</li>",
                        "<li><strong>Change 'Time Input Type'</strong> from 'Raw Values' to <span style='background: #e9e9e9; padding: 2px 6px; border-radius: 3px;'>Date/Time</span></li>",
                        "<li><strong>Select 'Date Format':</strong> <span style='background: #e9e9e9; padding: 2px 6px; border-radius: 3px;'>",
                        validation_result$format, "</span></li>",
                        "<li><strong>Choose your preferred 'Time Display'</strong> mode (Relative or Absolute)</li>",
                        "<li><strong>The analysis will be re-run with your settings.</strong></li>",
                        "</ol>",
                        "</div>",

                        # Helpful tip section with subtle styling
                        "<div style='background: #f9f9f9; border: 1px solid #ccc; padding: 15px;'>",
                        "<h4 style='margin: 0 0 10px 0; font-size: 15px; color: #333;'>💡 Important Note</h4>",
                        "<p style='margin: 0; font-size: 14px; color: #666;'>",
                        "Configuring the date settings properly ensures accurate timeline calculations ",
                        "and gives you full control over how dates are displayed in your swimmer plot.",
                        "</p>",
                        "</div>",

                        "</div>"
                    )
                    self$results$instructions$setContent(date_guidance)
                    return()  # Stop here, don't process further
                }

                # Check for validation errors
                if (isTRUE(validation_result$error)) {
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
                    stop(validation_result$message)
                }
                
                # Extract patient data and show warnings if present
                patient_data <- if ("data" %in% names(validation_result)) validation_result$data else validation_result
                
                # Apply sorting (affects y-axis order only)
                patient_data <- private$.applySorting(patient_data)
                
                # Display warnings if present
                warning_messages <- c()

                if (!is.null(validation_result$warnings) && length(validation_result$warnings) > 0) {
                    warning_messages <- c(warning_messages, validation_result$warnings)
                }

                # Note: Auto-detection warning removed since we now stop analysis
                # when dates are detected and ask user to configure manually

                if (length(warning_messages) > 0) {
                    warning_msg <- paste0(
                        "<div style='color: #8a6d00; background-color: #fff8e1; padding: 15px; border: 1px solid #ffc107; border-radius: 5px; margin: 10px;'>",
                        .("<h4>Analysis Information</h4>"),
                        "<ul>",
                        paste0("<li>", warning_messages, "</li>", collapse = ""),
                        "</ul>",
                        "</div>"
                    )
                    self$results$instructions$setContent(warning_msg)
                }
                # Show note if absolute datetime with unsupported reference lines
                is_date_scale <- inherits(patient_data$start_time, c("Date", "POSIXct"))
                if (is_date_scale && identical(self$options$timeDisplay, "absolute")) {
                    if (self$options$referenceLines %in% c("median", "protocol")) {
                        note_html <- paste0(
                            "<div style='background-color:#fff8e1; border:1px solid #f0c36d; color:#6d4c00; padding:12px; border-radius:6px; margin:10px 0;'>",
                            "<strong>", .("Reference lines on absolute dates:"), "</strong> ",
                            .("Median/Protocol reference lines are not shown for absolute date scales because patient timelines start on different calendar dates."),
                            " ", .("Use 'Custom Time' with 'Custom Reference Date' or a time offset instead."),
                            "</div>"
                        )
                        self$results$validationReport$setContent(note_html)
                        try(self$results$validationReport$setVisible(TRUE), silent = TRUE)
                    } else if (self$options$referenceLines %in% c("custom")) {
                        # If custom selected but no date provided, we fall back to offset; inform the user once
                        cref_str <- tryCatch(self$options$customReferenceDate, error = function(e) NULL)
                        if (is.null(cref_str) || nchar(trimws(as.character(cref_str))) == 0) {
                            note_html <- paste0(
                                "<div style='background-color:#e8f5e9; border:1px solid #a5d6a7; color:#1b5e20; padding:12px; border-radius:6px; margin:10px 0;'>",
                                "<strong>", .("Custom reference in absolute mode:"), "</strong> ",
                                .("No 'Custom Reference Date' provided; using 'Custom Reference Time' as an offset from the earliest start date."),
                                "</div>"
                            )
                            self$results$validationReport$setContent(note_html)
                            try(self$results$validationReport$setVisible(TRUE), silent = TRUE)
                        }
                    }
                }

                milestone_data <- private$.processMilestones(patient_data)
                event_data <- private$.processEventMarkers(patient_data)
                arrow_data <- private$.processOngoingStatus(patient_data, stats)
                
                # Calculate comprehensive statistics
                stats <- private$.calculateSummaryStats(patient_data)
                interpretation <- private$.generateClinicalInterpretation(stats, patient_data)
                
                # Generate clinical summary
                clinical_summary <- private$.generateClinicalSummary(stats, patient_data)
                private$.displayClinicalSummary(clinical_summary)
                
                # Update summary table
                private$.updateSummaryTable(stats)
                
                # Update all result tables
                private$.updatePersonTimeTable(patient_data, stats)
                private$.updateMilestoneTable(patient_data, milestone_data) 
                private$.updateEventMarkerTable(patient_data, event_data)
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

                # Generate clinical guidance outputs if requested
                if (self$options$showGlossary) {
                    private$.generateClinicalGlossary()
                }

                if (self$options$showCopyReady) {
                    private$.generateCopyReadyReport(stats, patient_data)
                }

                if (self$options$showAbout) {
                    private$.generateAboutAnalysis()
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
                stop(e)
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
        .updatePersonTimeTable = function(patient_data, stats) {
            if (!self$options$personTimeAnalysis) return()

            patient_summary <- stats$patient_summary
            if (is.null(patient_summary)) {
                patient_summary <- private$.summarizeByPatient(patient_data)
            }

            if (!"response" %in% names(patient_summary)) return()

            patient_summary <- patient_summary[!is.na(patient_summary$response), , drop = FALSE]
            if (nrow(patient_summary) == 0) return()

            self$results$personTimeTable$deleteRows()

            person_time_data <- patient_summary %>%
                dplyr::group_by(response) %>%
                dplyr::summarise(
                    n_patients = dplyr::n(),
                    total_time = sum(person_time, na.rm = TRUE),
                    mean_time = mean(person_time, na.rm = TRUE),
                    .groups = "drop"
                ) %>%
                dplyr::mutate(
                    incidence_rate = ifelse(total_time > 0, n_patients / total_time * 100, NA_real_)
                )

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
        .updateMilestoneTable = function(patient_data, milestone_data) {
            if (nrow(milestone_data) == 0) return()

            # Clear existing rows
            self$results$milestoneTable$deleteRows()

            milestone_numeric <- private$.convertTimesToNumeric(
                milestone_data$time,
                milestone_data$patient_id,
                patient_data
            )

            milestone_stats_data <- milestone_data %>%
                dplyr::mutate(time_numeric = milestone_numeric) %>%
                dplyr::filter(!is.na(time_numeric))

            if (nrow(milestone_stats_data) == 0) return()

            # Calculate milestone statistics
            milestone_stats <- milestone_stats_data %>%
                dplyr::group_by(label) %>%
                dplyr::summarise(
                    n_events = dplyr::n(),
                    median_time = median(time_numeric, na.rm = TRUE),
                    min_time = min(time_numeric, na.rm = TRUE),
                    max_time = max(time_numeric, na.rm = TRUE),
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
        .updateEventMarkerTable = function(patient_data, event_data) {
            if (!self$options$showEventMarkers || is.null(event_data) || nrow(event_data) == 0) return()

            # Clear existing rows
            self$results$eventMarkerTable$deleteRows()

            event_numeric <- private$.convertTimesToNumeric(
                event_data$time,
                event_data$patient_id,
                patient_data
            )

            event_stats_data <- event_data %>%
                dplyr::mutate(time_numeric = event_numeric) %>%
                dplyr::filter(!is.na(time_numeric))

            if (nrow(event_stats_data) == 0) return()

            total_events <- nrow(event_stats_data)

            # Calculate event statistics
            event_stats <- event_stats_data %>%
                dplyr::group_by(label) %>%
                dplyr::summarise(
                    n_events = dplyr::n(),
                    median_time = median(time_numeric, na.rm = TRUE),
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

            patient_summary <- stats$patient_summary
            if (is.null(patient_summary)) {
                patient_summary <- private$.summarizeByPatient(patient_data)
            }

            n_patients_summary <- nrow(patient_summary)

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
                    value = if (isTRUE(stats$total_person_time > 0)) round(n_patients_summary / stats$total_person_time * 100, 3) else NA_real_,
                    unit = paste0("per 100 ", self$options$timeUnit),
                    interpretation = .("Rate of patient inclusion per time unit")
                )
            )
            
            # Add response-specific metrics if available
            if (!is.null(stats$response_counts)) {
                response_counts <- stats$response_counts
                total_responses <- sum(response_counts)

                if (total_responses > 0) {
                    response_names <- tolower(names(response_counts))
                    orr_count <- sum(response_counts[response_names %in% c("cr", "pr")])
                    dcr_count <- sum(response_counts[response_names %in% c("cr", "pr", "sd")])

                    orr <- if (total_responses > 0) orr_count / total_responses * 100 else NA_real_
                    dcr <- if (total_responses > 0) dcr_count / total_responses * 100 else NA_real_

                    metrics <- append(metrics, list(
                        list(
                            name = .("Objective Response Rate (ORR)"),
                            value = if (!is.na(orr)) round(orr, 1) else NA_real_,
                            unit = "percent",
                            interpretation = .("Proportion with complete or partial response")
                        ),
                        list(
                            name = .("Disease Control Rate (DCR)"),
                            value = if (!is.na(dcr)) round(dcr, 1) else NA_real_,
                            unit = "percent",
                            interpretation = .("Proportion with response or stable disease")
                        )
                    ))
                }
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

                self$results$timelineData$setState(timeline_export)
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

                self$results$summaryData$setState(summary_export)
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
            
            if (self$options$responseAnalysis && !is.null(interpretation$response)) {
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
            # Check if ggswim is available
            if (!requireNamespace("ggswim", quietly = TRUE)) {
                warning("ggswim package not available, using fallback visualization")
                return(private$.createFallbackPlot(patient_data, milestone_data, event_data, opts, stats))
            }

            # Create base plot with swim lanes
            p <- ggplot2::ggplot()

            # Add swim lanes with enhanced styling and error boundary
            if ("response" %in% names(patient_data)) {
                p <- tryCatch({
                    p + ggswim::geom_swim_lane(
                    data = patient_data,
                    mapping = ggplot2::aes(
                        x = start_time,
                        xend = end_time,
                        y = patient_id,
                        colour = response
                    ),
                    linewidth = opts$laneWidth
                    )
                }, error = function(e) {
                    # Fallback to basic ggplot2 segments
                    p + ggplot2::geom_segment(
                        data = patient_data,
                        mapping = ggplot2::aes(
                            x = start_time,
                            xend = end_time,
                            y = patient_id,
                            yend = patient_id,
                            color = response
                        ),
                        size = opts$laneWidth
                    )
                })
            } else {
                p <- tryCatch({
                    p + ggswim::geom_swim_lane(
                        data = patient_data,
                        mapping = ggplot2::aes(
                            x = start_time,
                            xend = end_time,
                            y = patient_id
                        ),
                        linewidth = opts$laneWidth,
                        colour = "steelblue"
                    )
                }, error = function(e) {
                    # Fallback to basic ggplot2 segments
                    p + ggplot2::geom_segment(
                        data = patient_data,
                        mapping = ggplot2::aes(
                            x = start_time,
                            xend = end_time,
                            y = patient_id,
                            yend = patient_id
                        ),
                        color = "steelblue",
                        size = opts$laneWidth
                    )
                })
            }
            
            # Add event markers if available
            if (!is.null(event_data) && nrow(event_data) > 0) {
                # Create enhanced marker mappings with clinical icons
                unique_labels <- unique(event_data$label)
                
                # Enhanced clinical glyphs with medical symbols
                clinical_glyphs <- private$.getEnhancedClinicalGlyphs(unique_labels)
                base_n <- max(3, min(length(unique_labels), 8))
                base_palette <- RColorBrewer::brewer.pal(base_n, "Set2")
                clinical_colors <- grDevices::colorRampPalette(base_palette)(length(unique_labels))

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
                        x = x,
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
                p <- private$.addReferenceLines(p, opts, stats, patient_data)
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
            is_date_scale <- inherits(patient_data$start_time, c("Date", "POSIXct"))
            x_label <- if (is_date_scale) .("Date") else paste0(.("Time ("), self$options$timeUnit, .(")"))
            p <- p + ggplot2::labs(
                title = .("Patient Timeline Analysis"),
                subtitle = sprintf(.("N=%d patients | Median follow-up: %.1f %s | Total person-time: %.1f %s"),
                                 stats$n_patients, stats$median_duration, self$options$timeUnit,
                                 stats$total_person_time, self$options$timeUnit),
                x = x_label,
                y = .("Patient ID")
            )
            
            # Legend handling
            if (!opts$showLegend) {
                p <- p + ggplot2::theme(legend.position = "none")
            }
            
            return(p)
        },
        
        .addReferenceLines = function(p, opts, stats, patient_data) {
            is_date_scale <- inherits(patient_data$start_time, c("Date", "POSIXct"))
            if (opts$referenceLines == "median") {
                if (!is_date_scale) {
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
                }
            } else if (opts$referenceLines == "protocol") {
                if (!is_date_scale) {
                    protocol_times <- private$.getProtocolReferenceTimes(stats$max_duration, opts$timeUnit)
                    if (length(protocol_times) > 0) {
                        for (t in protocol_times) {
                            p <- p + ggplot2::geom_vline(
                                xintercept = t,
                                linetype = "dotted",
                                color = "darkgray",
                                alpha = 0.5
                            )
                        }
                    }
                }
            } else if (opts$referenceLines == "custom") {
                if (is_date_scale) {
                    # Prefer an explicit custom reference date if provided (string)
                    cref <- NULL
                    cref_str <- tryCatch(self$options$customReferenceDate, error = function(e) NULL)
                    if (!is.null(cref_str)) {
                        cref <- private$.parseCustomReferenceDate(cref_str)
                    }
                    if (is.null(cref) && !is.null(opts$customReferenceTime)) {
                        # Fallback: numeric offset from earliest start in selected time unit
                        anchor <- suppressWarnings(min(patient_data$start_time, na.rm = TRUE))
                        per <- switch(opts$timeUnit,
                            days   = lubridate::days(opts$customReferenceTime),
                            weeks  = lubridate::weeks(opts$customReferenceTime),
                            months = lubridate::months(opts$customReferenceTime),
                            years  = lubridate::years(opts$customReferenceTime),
                            lubridate::days(opts$customReferenceTime)
                        )
                        cref <- anchor + per
                    }
                    if (!is.null(cref)) {
                        p <- p + ggplot2::geom_vline(
                            xintercept = cref,
                            linetype = "dashed",
                            color = "red",
                            alpha = 0.7
                        )
                    }
                } else if (!is.null(opts$customReferenceTime)) {
                    p <- p + ggplot2::geom_vline(
                        xintercept = opts$customReferenceTime,
                        linetype = "dashed",
                        color = "red",
                        alpha = 0.7
                    )
                }
            }
            return(p)
        },

        # Apply patient sorting based on options (affects y-axis order)
        .applySorting = function(patient_data) {
            if (nrow(patient_data) == 0) return(patient_data)
            # Compute numeric durations for sorting when needed
            if (inherits(patient_data$start_time, c("Date", "POSIXct"))) {
                intervals <- lubridate::interval(patient_data$start_time, patient_data$end_time)
                sort_durations <- lubridate::time_length(intervals, unit = self$options$timeUnit)
            } else {
                sort_durations <- patient_data$end_time - patient_data$start_time
            }

            ord <- seq_len(nrow(patient_data))
            if (!is.null(self$options$sortVariable)) {
                sv <- self$options$sortVariable
                df <- self$data
                tmp <- data.frame(
                    patient_id = as.character(df[[private$.escapeVar(self$options$patientID)]]),
                    sort_val = df[[private$.escapeVar(sv)]],
                    stringsAsFactors = FALSE
                )
                tmp <- tmp[!is.na(tmp$patient_id) & !duplicated(tmp$patient_id), ]
                map <- stats::setNames(tmp$sort_val, tmp$patient_id)
                key <- unname(map[as.character(patient_data$patient_id)])
                ord <- order(key, na.last = TRUE, method = "auto")
            } else if (self$options$sortOrder == "patient_id") {
                ord <- order(patient_data$patient_id, method = "auto")
            } else if (self$options$sortOrder == "response" && "response" %in% names(patient_data)) {
                ord <- order(patient_data$response, patient_data$patient_id, method = "auto")
            } else {
                dec <- identical(self$options$sortOrder, "duration_desc")
                ord <- order(sort_durations, decreasing = dec, method = "auto")
            }

            ordered_ids <- as.character(patient_data$patient_id[ord])
            patient_data$patient_id <- factor(as.character(patient_data$patient_id), levels = unique(ordered_ids))
            patient_data
        },
        
        .createFallbackPlot = function(patient_data, milestone_data = NULL, event_data = NULL, opts = NULL, stats = NULL, error_message = "ggswim unavailable") {
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
        },

        # Generate clinical glossary
        .generateClinicalGlossary = function() {
            glossary_html <- paste0(
                "<div style='background-color: #f8f9fa; padding: 20px; border-radius: 8px; margin: 15px 0; font-family: system-ui, -apple-system, sans-serif;'>",
                "<h3 style='color: #007bff; margin-top: 0;'>📚 Clinical Glossary</h3>",

                "<div style='margin: 10px 0;'>",
                "<h4 style='color: #0056b3; margin: 10px 0 5px 0;'>Response Categories</h4>",
                "<ul style='margin: 5px 0; padding-left: 20px;'>",
                "<li><strong>CR (Complete Response):</strong> Complete disappearance of all target lesions</li>",
                "<li><strong>PR (Partial Response):</strong> ≥30% decrease in sum of target lesion diameters</li>",
                "<li><strong>SD (Stable Disease):</strong> Neither sufficient shrinkage for PR nor sufficient increase for PD</li>",
                "<li><strong>PD (Progressive Disease):</strong> ≥20% increase in sum of target lesion diameters</li>",
                "</ul>",
                "</div>",

                "<div style='margin: 10px 0;'>",
                "<h4 style='color: #0056b3; margin: 10px 0 5px 0;'>Clinical Metrics</h4>",
                "<ul style='margin: 5px 0; padding-left: 20px;'>",
                "<li><strong>ORR (Objective Response Rate):</strong> Proportion of patients with CR or PR</li>",
                "<li><strong>DCR (Disease Control Rate):</strong> Proportion of patients with CR, PR, or SD</li>",
                "<li><strong>Person-Time:</strong> Total observation time across all patients in the study</li>",
                "<li><strong>Median Follow-up:</strong> Time point at which half of patients have been followed longer</li>",
                "<li><strong>Incidence Rate:</strong> Number of events per unit of person-time</li>",
                "</ul>",
                "</div>",

                "<div style='margin: 10px 0;'>",
                "<h4 style='color: #0056b3; margin: 10px 0 5px 0;'>Timeline Elements</h4>",
                "<ul style='margin: 5px 0; padding-left: 20px;'>",
                "<li><strong>Swim Lanes:</strong> Horizontal bars representing individual patient treatment courses</li>",
                "<li><strong>Milestones:</strong> Key clinical events (surgery, assessment, progression)</li>",
                "<li><strong>Event Markers:</strong> Specific events occurring during treatment</li>",
                "<li><strong>Status Arrows:</strong> Indicate ongoing treatment at data cutoff</li>",
                "</ul>",
                "</div>",

                "</div>"
            )

            self$results$clinicalGlossary$setContent(glossary_html)
        },

        # Generate copy-ready manuscript text
        .generateCopyReadyReport = function(stats, patient_data) {
            # Basic study description
            basic_text <- sprintf(
                "Patient timelines were analyzed using swimmer plots to visualize treatment courses and clinical outcomes. The study included %d patients with a median follow-up of %.1f %s (range: %.1f to %.1f %s). Total person-time was %.1f %s.",
                stats$n_patients,
                stats$median_duration,
                self$options$timeUnit,
                stats$min_duration,
                stats$max_duration,
                self$options$timeUnit,
                stats$total_person_time,
                self$options$timeUnit
            )

            # Add response analysis if available
            response_text <- ""
            if (self$options$responseAnalysis && "response" %in% names(patient_data) && !is.null(stats$response_counts)) {
                orr_count <- sum(stats$response_counts[names(stats$response_counts) %in% c("CR", "PR")])
                orr_pct <- orr_count / sum(stats$response_counts) * 100

                dcr_count <- sum(stats$response_counts[names(stats$response_counts) %in% c("CR", "PR", "SD")])
                dcr_pct <- dcr_count / sum(stats$response_counts) * 100

                response_text <- sprintf(
                    " Response evaluation showed an objective response rate (ORR) of %.1f%% (%d/%d patients) and disease control rate (DCR) of %.1f%% (%d/%d patients).",
                    orr_pct, orr_count, sum(stats$response_counts),
                    dcr_pct, dcr_count, sum(stats$response_counts)
                )
            }

            # Methodology note
            methods_text <- " Timeline visualization was created using the ggswim package, providing comprehensive swimmer plots suitable for clinical research reporting and regulatory submissions."

            full_text <- paste0(basic_text, response_text, methods_text)

            copy_ready_html <- paste0(
                "<div style='background-color: #e8f5e8; padding: 20px; border-left: 4px solid #28a745; border-radius: 8px; margin: 15px 0; font-family: system-ui, -apple-system, sans-serif;'>",
                "<h3 style='color: #155724; margin-top: 0; display: flex; align-items: center;'>",
                "<span style='margin-right: 8px;'>📄</span>",
                "Copy-Ready Manuscript Text",
                "</h3>",
                "<div style='background-color: white; padding: 15px; border-radius: 6px; margin: 10px 0; box-shadow: 0 1px 3px rgba(0,0,0,0.1);'>",
                "<p style='margin: 0; line-height: 1.6; color: #333; font-size: 0.95em; text-align: justify;'>", full_text, "</p>",
                "</div>",
                "<div style='margin-top: 15px; padding: 10px; background-color: #d1ecf1; border-radius: 4px; border: 1px dashed #0c5460;'>",
                "<p style='margin: 0; font-size: 0.85em; color: #0c5460;'>",
                "<strong>Usage:</strong> This text is formatted for direct use in manuscripts, clinical reports, and regulatory submissions. Copy and paste into your document and adjust as needed for your specific requirements.",
                "</p>",
                "</div>",
                "</div>"
            )

            self$results$copyReadyReport$setContent(copy_ready_html)
        },

        # Generate about analysis information
        .generateAboutAnalysis = function() {
            about_html <- paste0(
                "<div style='background-color: #fff3cd; padding: 20px; border-left: 4px solid #ffc107; border-radius: 8px; margin: 15px 0; font-family: system-ui, -apple-system, sans-serif;'>",
                "<h3 style='color: #856404; margin-top: 0; display: flex; align-items: center;'>",
                "<span style='margin-right: 8px;'>ℹ️</span>",
                "About Swimmer Plot Analysis",
                "</h3>",

                "<div style='margin: 15px 0;'>",
                "<h4 style='color: #856404; margin: 10px 0 5px 0;'>What is a Swimmer Plot?</h4>",
                "<p style='margin: 5px 0; line-height: 1.6;'>",
                "Swimmer plots are timeline visualizations that display individual patient treatment courses, clinical events, and outcomes in a single comprehensive graph. Each horizontal 'swim lane' represents one patient's journey through treatment.",
                "</p>",
                "</div>",

                "<div style='margin: 15px 0;'>",
                "<h4 style='color: #856404; margin: 10px 0 5px 0;'>When to Use Swimmer Plots</h4>",
                "<ul style='margin: 5px 0; padding-left: 20px; line-height: 1.6;'>",
                "<li>Clinical trial data visualization and regulatory submissions</li>",
                "<li>Treatment response assessment and duration analysis</li>",
                "<li>Patient outcome tracking in longitudinal studies</li>",
                "<li>Safety event monitoring and adverse event reporting</li>",
                "<li>Milestone-based clinical pathway analysis</li>",
                "</ul>",
                "</div>",

                "<div style='margin: 15px 0;'>",
                "<h4 style='color: #856404; margin: 10px 0 5px 0;'>Required Data</h4>",
                "<ul style='margin: 5px 0; padding-left: 20px; line-height: 1.6;'>",
                "<li><strong>Patient ID:</strong> Unique identifier for each patient</li>",
                "<li><strong>Start Time:</strong> Treatment or observation start date/time</li>",
                "<li><strong>End Time:</strong> Treatment or observation end date/time</li>",
                "<li><strong>Response Variable (optional):</strong> Treatment response categories</li>",
                "<li><strong>Milestone Events (optional):</strong> Key clinical events with dates</li>",
                "</ul>",
                "</div>",

                "<div style='margin: 15px 0;'>",
                "<h4 style='color: #856404; margin: 10px 0 5px 0;'>Key Assumptions</h4>",
                "<ul style='margin: 5px 0; padding-left: 20px; line-height: 1.6;'>",
                "<li>Each row represents one patient episode or treatment course</li>",
                "<li>Time variables are either numeric (days/months) or valid date formats</li>",
                "<li>End times should be greater than or equal to start times</li>",
                "<li>Missing data is handled appropriately (excluded from calculations)</li>",
                "<li>Response categories follow standard clinical criteria (RECIST, etc.)</li>",
                "</ul>",
                "</div>",

                "<div style='margin: 15px 0;'>",
                "<h4 style='color: #856404; margin: 10px 0 5px 0;'>Output Interpretation</h4>",
                "<p style='margin: 5px 0; line-height: 1.6;'>",
                "The swimmer plot displays individual patient timelines with optional color coding for response categories. Milestone markers show key events, and summary statistics provide overall study metrics including person-time analysis and response rates suitable for clinical reporting.",
                "</p>",
                "</div>",

                "</div>"
            )

            self$results$aboutAnalysis$setContent(about_html)
        }
    )
)
