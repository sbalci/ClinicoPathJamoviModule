#' @title Comprehensive Time Interval Calculator for Survival Analysis
#'
#' @description
#' Advanced time interval calculation tool designed for survival analysis, epidemiological
#' studies, and person-time analysis. Provides robust date parsing, time interval
#' calculation, landmark analysis, and comprehensive data quality assessment.
#'
#' @param data Data frame containing date columns for analysis
#' @param dx_date Name of column containing start dates (diagnosis, entry, treatment start)
#' @param fu_date Name of column containing end dates (follow-up, event, study exit)
#' @param time_format Date format: "auto", "ymd", "dmy", "mdy", "ydm", "myd", "dym", or "ymdhms"
#' @param output_unit Output time unit: "days", "weeks", "months", or "years"
#' @param use_landmark Enable landmark analysis (conditional survival from time point)
#' @param landmark_time Landmark time point in output units
#' @param remove_negative Remove negative intervals (end before start)
#' @param remove_extreme Remove extreme outliers (>2× 99th percentile)
#' @param add_times Add calculated intervals as new variable in dataset
#' @param include_quality_metrics Include comprehensive quality assessment
#' @param confidence_level Confidence level for mean intervals (90-99%)
#'
#' @return List containing calculated intervals, filtered data, quality metrics, and metadata
#'
#' @details
#' This function provides comprehensive time interval calculation capabilities including:
#' \itemize{
#'   \item Multiple date format parsing with automatic detection
#'   \item Flexible output units (days, weeks, months, years)
#'   \item Landmark analysis for conditional survival
#'   \item Person-time calculations for epidemiological studies
#'   \item Data quality assessment and validation
#'   \item Statistical summaries with confidence intervals
#'   \item Export capabilities for downstream analysis
#' }
#'
#' @note
#'   **Landmark Analysis Exclusion:** When landmark analysis is enabled, participants with missing
#'   follow-up times (NA values) are implicitly excluded from the "at-risk" cohort because their
#'   eligibility for landmark criteria cannot be determined.
#'
#' @examples
#' # Basic time interval calculation:
#' timeinterval(
#'   data = study_data,
#'   dx_date = "diagnosis_date",
#'   fu_date = "followup_date",
#'   time_format = "ymd",
#'   output_unit = "months"
#' )
#'
#' # With landmark analysis:
#' timeinterval(
#'   data = study_data,
#'   dx_date = "start_date",
#'   fu_date = "end_date",
#'   use_landmark = TRUE,
#'   landmark_time = 6,
#'   output_unit = "months"
#' )
#'
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import lubridate
#' @import glue
#'

timeintervalClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "timeintervalClass",
    inherit = timeintervalBase,
    private = list(
        # ===================================================================
        # COMPREHENSIVE TIME INTERVAL CALCULATION FUNCTIONS
        # ===================================================================
        
        .validateInputData = function(data, dx_date, fu_date) {
            # Comprehensive input validation - returns status instead of throwing errors
            if (!is.data.frame(data)) {
                return(list(
                    valid = FALSE,
                    error = "Input must be a data frame; check your data source."
                ))
            }

            if (nrow(data) == 0) {
                return(list(
                    valid = FALSE,
                    error = "Data frame is empty; ensure your dataset has at least one row."
                ))
            }

            # Validate date columns exist
            if (!dx_date %in% names(data)) {
                available <- paste(head(names(data), 8), collapse = ", ")
                suffix <- if(ncol(data) > 8) "..." else ""
                return(list(
                    valid = FALSE,
                    error = sprintf("Start date column '%s' not found. Available columns: %s%s",
                                  dx_date, available, suffix)
                ))
            }

            if (!fu_date %in% names(data)) {
                available <- paste(head(names(data), 8), collapse = ", ")
                suffix <- if(ncol(data) > 8) "..." else ""
                return(list(
                    valid = FALSE,
                    error = sprintf("End date column '%s' not found. Available columns: %s%s",
                                  fu_date, available, suffix)
                ))
            }

            # Check for completely missing date columns
            if (all(is.na(data[[dx_date]]))) {
                return(list(
                    valid = FALSE,
                    error = "Start date column contains only missing values; cannot calculate time intervals."
                ))
            }

            if (all(is.na(data[[fu_date]]))) {
                return(list(
                    valid = FALSE,
                    error = "End date column contains only missing values; cannot calculate time intervals."
                ))
            }

            return(list(valid = TRUE))
        },
        
        .detectDateFormat = function(start_vector,
                                     end_vector,
                                     specified_format = NULL,
                                     start_name = "start",
                                     end_name = "end") {
            # Automatic date format detection if not specified
            if (!is.null(specified_format) && specified_format != "auto") {
                return(specified_format)
            }
            
            # Remove missing values for format detection
            sample_start <- start_vector[!is.na(start_vector)]
            sample_end <- end_vector[!is.na(end_vector)]
            
            if (length(sample_start) == 0 && length(sample_end) == 0) {
                stop("No valid dates found for format detection in either column")
            }
            
            sample_size <-  min(50, max(length(sample_start), length(sample_end)))
            sample_start <- head(sample_start, sample_size)
            sample_end <- head(sample_end, sample_size)
            
            # Test common formats
            formats_to_try <- c("ymd", "dmy", "mdy", "ydm", "myd", "dym", "ymdhms")
            
            best_format <- "ymd"
            best_score <- -1
            
            for (fmt in formats_to_try) {
                parser <- switch(fmt,
                    "ymdhms" = lubridate::ymd_hms,
                    "ymd" = lubridate::ymd,
                    "ydm" = lubridate::ydm,
                    "mdy" = lubridate::mdy,
                    "myd" = lubridate::myd,
                    "dmy" = lubridate::dmy,
                    "dym" = lubridate::dym
                )
                
                tryCatch({
                    parsed_start <- if (length(sample_start) > 0) parser(sample_start, quiet = TRUE) else NULL
                    parsed_end <- if (length(sample_end) > 0) parser(sample_end, quiet = TRUE) else NULL
                    
                    success_start <- if (length(parsed_start)) sum(!is.na(parsed_start)) / length(parsed_start) else 0
                    success_end <- if (length(parsed_end)) sum(!is.na(parsed_end)) / length(parsed_end) else 0
                    
                    # Require a format that works for both columns; take the weaker score
                    success_rate <- min(success_start, success_end)
                    
                    if (success_rate > best_score) {
                        best_score <- success_rate
                        best_format <- fmt
                    }
                }, error = function(e) {
                    # Continue to next format
                })
            }
            
            if (best_score < 0.5) {
                stop(glue::glue(
                    "Could not detect a common date format for columns '{start_name}' and '{end_name}'. ",
                    "Please select the correct format manually."
                ))
            }
            
            return(best_format)
        },
        
        .parseDate = function(date_vector, format, tz = "") {
            # Enhanced date parsing with better error handling
            # @param date_vector Character or numeric vector to parse
            # @param format Format string (e.g., "ymd", "dmy")
            # @param tz Timezone (default: "" for system timezone)
            # @return POSIXct or Date datetime vector
            date_parser <- switch(format,
                "ymdhms" = lubridate::ymd_hms,
                "ymd" = lubridate::ymd,
                "ydm" = lubridate::ydm,
                "mdy" = lubridate::mdy,
                "myd" = lubridate::myd,
                "dmy" = lubridate::dmy,
                "dym" = lubridate::dym,
                stop(paste("Unsupported date format:", format))
            )

            tryCatch({
                # Pass timezone to parser for formats with time component
                if (format == "ymdhms") {
                    parsed_dates <- date_parser(date_vector, quiet = TRUE, tz = tz)
                } else {
                    parsed_dates <- date_parser(date_vector, quiet = TRUE)
                }
                return(parsed_dates)
            }, error = function(e) {
                stop(paste("Error parsing dates with format", format, ":", e$message))
            })
        },
        
        .validateParsedDates = function(parsed_dates, original_vector, column_name, format_label) {
            total_non_missing <- sum(!is.na(original_vector))
            successful <- sum(!is.na(parsed_dates))
            
            if (total_non_missing == 0) {
                stop(glue::glue("Column '{column_name}' contains only missing values; cannot calculate time intervals."))
            }
            
            if (successful == 0) {
                sample_values <- paste(utils::head(unique(original_vector), 3), collapse = ", ")
                stop(glue::glue(
                    "Date parsing failed for column '{column_name}' using format '{format_label}'. ",
                    "Example values: {sample_values}"
                ))
            }
            
            success_rate <- successful / total_non_missing
            if (success_rate < 0.8) {
                stop(glue::glue(
                    "Only {round(100 * success_rate, 1)}% of non-missing values in '{column_name}' were parsed with format '{format_label}'. ",
                    "Please verify that the selected format matches all values or standardise the column."
                ))
            }
        },
        
        .calculateTimeIntervals = function(start_dates, end_dates, output_unit) {
            # Enhanced interval calculation with validation
            
            # Check for valid date objects
            if (!inherits(start_dates, "Date") && !inherits(start_dates, "POSIXct")) {
                stop("Start dates are not valid date objects")
            }
            
            if (!inherits(end_dates, "Date") && !inherits(end_dates, "POSIXct")) {
                stop("End dates are not valid date objects")
            }
            
            # Calculate intervals
            intervals <- lubridate::interval(start_dates, end_dates)
            
            # Convert to specified time unit
            # For statistical accuracy in survival analysis, we use fixed duration lengths
            # for months (30.44 days) and years (365.25 days) rather than calendar units.
            # This ensures that 'time' represents a consistent quantity of risk exposure.
            
            if (output_unit %in% c("months", "years")) {
                # Convert to duration first (seconds) then to unit
                # This uses standard length: Month = 30.4375 days, Year = 365.25 days
                calculated_time <- lubridate::time_length(lubridate::as.duration(intervals), output_unit)
            } else {
                # Days and weeks are standard
                calculated_time <- lubridate::time_length(intervals, output_unit)
            }
            
            return(calculated_time)
        },

        .calculateCalendarIntervals = function(start_dates, end_dates, output_unit) {
            # Calendar-aware interval calculation that respects varying month lengths
            if (!inherits(start_dates, "Date") && !inherits(start_dates, "POSIXct")) {
                stop("Start dates are not valid date objects")
            }
            if (!inherits(end_dates, "Date") && !inherits(end_dates, "POSIXct")) {
                stop("End dates are not valid date objects")
            }

            intervals <- lubridate::interval(start_dates, end_dates)
            days <- lubridate::time_length(intervals, "days")

            # Handle simple units directly
            if (output_unit == "days") return(days)
            if (output_unit == "weeks") return(days / 7)

            # Calendar months/years: count whole months, then proportion of remaining month
            whole_months <- intervals %/% months(1)
            # Remaining interval after removing whole months
            remainder_start <- start_dates + months(whole_months)
            remainder_int <- lubridate::interval(remainder_start, end_dates)
            remainder_days <- lubridate::time_length(remainder_int, "days")

            # Avoid division by zero when remainder_start is NA
            days_in_month_start <- ifelse(is.na(remainder_start), NA_real_, lubridate::days_in_month(remainder_start))
            fraction_months <- remainder_days / days_in_month_start
            total_months <- whole_months + fraction_months

            if (output_unit == "months") return(total_months)
            if (output_unit == "years") return(total_months / 12)

            stop("Unsupported output unit for calendar-based calculation")
        },
        
        .applyLandmarkAnalysis = function(calculated_time, data, landmark_time, output_unit) {
            # Enhanced landmark analysis with comprehensive reporting
            
            if (is.null(landmark_time) || landmark_time == 0) {
                return(list(
                    time = calculated_time,
                    data = data,
                    excluded_count = 0,
                    landmark_time = 0
                ))
            }
            
            if (!is.numeric(landmark_time) || landmark_time < 0) {
                stop("Landmark time must be a non-negative number")
            }
            
            # Identify cases before landmark time
            # Handle NAs by treating them as excluded from valid set (FALSE)
            valid_cases <- calculated_time >= landmark_time
            valid_cases[is.na(valid_cases)] <- FALSE
            excluded_count <- sum(!valid_cases)
            
            # Filter and adjust times
            adjusted_time <- calculated_time - landmark_time
            filtered_data <- data[valid_cases, ]
            final_time <- adjusted_time[valid_cases]
            
            return(list(
                time = final_time,
                data = filtered_data,
                excluded_count = excluded_count,
                landmark_time = landmark_time,
                original_n = length(calculated_time),
                final_n = length(final_time)
            ))
        },
        
        .calculateCI = function(mean, sd, n, conf_level) {
            # Calculate confidence interval for mean
            if (n <= 1) return(list(lower = NA, upper = NA))
            alpha <- 1 - (conf_level / 100)
            se <- sd / sqrt(n)
            margin <- qt(1 - alpha/2, n - 1) * se
            list(lower = mean - margin, upper = mean + margin)
        },

        .assessDataQuality = function(calculated_time, start_dates, end_dates) {
            # Comprehensive data quality assessment

            total_obs <- length(calculated_time)
            non_missing <- sum(!is.na(calculated_time))
            suppressWarnings({
                q99 <- quantile(calculated_time, 0.99, na.rm = TRUE, names = FALSE)
            })
            extreme_threshold <- if (is.na(q99)) Inf else q99 * 2

            quality_metrics <- list(
                total_observations = total_obs,
                missing_values = sum(is.na(calculated_time)),
                negative_intervals = if (non_missing > 0) sum(calculated_time < 0, na.rm = TRUE) else 0,
                zero_intervals = if (non_missing > 0) sum(calculated_time == 0, na.rm = TRUE) else 0,
                extreme_values = if (is.finite(extreme_threshold)) sum(calculated_time > extreme_threshold, na.rm = TRUE) else 0,
                missing_start_dates = sum(is.na(start_dates)),
                missing_end_dates = sum(is.na(end_dates)),
                future_dates = sum(start_dates > Sys.Date(), na.rm = TRUE) + sum(end_dates > Sys.Date(), na.rm = TRUE)
            )
            
            # Generate quality warnings
            warnings <- character()
            
            if (non_missing == 0) {
                warnings <- c(warnings, "No valid time intervals after parsing start/end dates.")
            }
            
            if (quality_metrics$negative_intervals > 0) {
                warnings <- c(warnings, paste(quality_metrics$negative_intervals, "negative time intervals detected (end date before start date)"))
            }
            
            if (quality_metrics$missing_values > 0) {
                warnings <- c(warnings, paste(quality_metrics$missing_values, "missing time intervals due to missing dates"))
            }
            
            if (quality_metrics$future_dates > 0) {
                warnings <- c(warnings, paste(quality_metrics$future_dates, "dates in the future detected"))
            }
            
            if (quality_metrics$extreme_values > 0) {
                warnings <- c(warnings, paste(quality_metrics$extreme_values, "potentially extreme time intervals detected"))
            }
            
            quality_metrics$warnings <- warnings
            quality_metrics$overall_quality <- ifelse(length(warnings) == 0, "Good", 
                                                    ifelse(length(warnings) <= 2, "Fair", "Poor"))
            
            return(quality_metrics)
        },
        
        # Main enhanced calculation function
        .calculate_survival_time = function(data,
                                            dx_date = NULL,
                                            fu_date = NULL,
                                            time_format = "ymd",
                                            output_unit = "months",
                                            time_basis = "standardized",
                                            landmark_time = NULL,
                                            timezone_setting = "system") {

            # Note: Input validation already performed in .run() before calling this method
            # Redundant validation call removed to avoid duplicate checks

            # Detect date format if needed
            detected_format <- private$.detectDateFormat(
                data[[dx_date]],
                data[[fu_date]],
                specified_format = time_format,
                start_name = dx_date,
                end_name = fu_date
            )

            # Convert timezone setting to lubridate format
            tz <- if (timezone_setting == "utc") "UTC" else ""

            # Parse dates with enhanced error handling
            start_dates <- private$.parseDate(data[[dx_date]], detected_format, tz = tz)
            end_dates <- private$.parseDate(data[[fu_date]], detected_format, tz = tz)

            # Validate parse success for both columns
            private$.validateParsedDates(start_dates, data[[dx_date]], dx_date, detected_format)
            private$.validateParsedDates(end_dates, data[[fu_date]], fu_date, detected_format)

            # CRITICAL FIX: Validate parsing success
            if (all(is.na(start_dates)) || all(is.na(end_dates))) {
                stop(glue::glue(
                    "Date parsing failed. All values became NA using format '{detected_format}'.\n",
                    "Please verify:\n",
                    "- Date format setting matches your data\n",
                    "- Date columns contain valid date values\n",
                    "- Dates are not stored as numeric codes without proper formatting"
                ))
            }

            # Calculate time intervals
            calculated_time_raw <- if (identical(time_basis, "calendar")) {
                private$.calculateCalendarIntervals(start_dates, end_dates, output_unit)
            } else {
                private$.calculateTimeIntervals(start_dates, end_dates, output_unit)
            }

            if (all(is.na(calculated_time_raw))) {
                stop("No valid time intervals could be calculated; please verify start/end dates and selected format.")
            }

            # Preserve original parsed vectors for quality assessment
            start_dates_raw <- start_dates
            end_dates_raw <- end_dates

            # Apply data quality filters if requested (combined for performance)
            original_data <- data
            valid_idx <- rep(TRUE, length(calculated_time_raw))
            filter_applied <- FALSE
            calculated_time <- calculated_time_raw
            removed_negative <- 0
            removed_extreme <- 0
            extreme_threshold <- NA

            # Explicitly handle negative intervals before any filtering
            negative_idx <- which(!is.na(calculated_time_raw) & calculated_time_raw < 0)
            if (length(negative_idx) > 0 && !self$options$remove_negative) {
                example_rows <- head(negative_idx, 3)
                examples <- paste0(
                    "Row ", example_rows, ": Start=", format(start_dates_raw[example_rows]),
                    ", End=", format(end_dates_raw[example_rows])
                )
                stop(glue::glue(
                    "Negative time intervals detected (end date before start date) in {length(negative_idx)} rows.\n",
                    "Please correct the dates or enable 'Remove Negative Intervals'.\n",
                    "Examples:\n{paste(examples, collapse = '\\n')}"
                ))
            }

            if (self$options$remove_negative) {
                removed_negative <- length(negative_idx)
                valid_idx <- valid_idx & (calculated_time_raw >= 0 | is.na(calculated_time_raw))
                filter_applied <- TRUE
            }

            if (self$options$remove_extreme) {
                suppressWarnings({
                    q99 <- quantile(calculated_time_raw, 0.99, na.rm = TRUE, names = FALSE)
                })
                if (!is.na(q99) && is.finite(q99)) {
                    extreme_threshold <- q99 * self$options$extreme_multiplier
                    removed_extreme <- sum(calculated_time_raw > extreme_threshold, na.rm = TRUE)
                    valid_idx <- valid_idx & (calculated_time_raw <= extreme_threshold | is.na(calculated_time_raw))
                    filter_applied <- TRUE
                }
            }

            # Apply combined filter in single operation
            if (filter_applied && !all(valid_idx)) {
                calculated_time <- calculated_time_raw[valid_idx]
                data <- data[valid_idx, ]
                start_dates <- start_dates[valid_idx]
                end_dates <- end_dates[valid_idx]
            } else {
                calculated_time <- calculated_time_raw
            }

            # Assess data quality
            quality_assessment <- private$.assessDataQuality(calculated_time_raw, start_dates_raw, end_dates_raw)
            
            # Apply landmark analysis if specified
            landmark_result <- private$.applyLandmarkAnalysis(calculated_time, data, landmark_time, output_unit)

            return(list(
                time = landmark_result$time,
                data = landmark_result$data,
                quality = quality_assessment,
                landmark = landmark_result,
                original_data = data,
                detected_format = detected_format,
                filter = list(
                    removed_negative = removed_negative,
                    removed_extreme = removed_extreme,
                    extreme_threshold = extreme_threshold
                )
            ))
        },

        # Run analysis ----
        .run = function() {
            # Initialize messages list for errors, warnings, and info
            messages <- list()

            # Helper function to add messages
            add_message <- function(type, content) {
                color <- switch(type,
                    "error" = list(bg = "#f8d7da", border = "#dc3545", text = "#721c24", icon = "❌"),
                    "strong_warning" = list(bg = "#fff3cd", border = "#ff8800", text = "#856404", icon = "⚠️"),
                    "warning" = list(bg = "#fff3cd", border = "#ffc107", text = "#856404", icon = "⚠️"),
                    "info" = list(bg = "#d1ecf1", border = "#17a2b8", text = "#0c5460", icon = "ℹ️"),
                    list(bg = "#e2e3e5", border = "#6c757d", text = "#383d41", icon = "•")
                )
                messages <<- c(messages, list(sprintf(
                    "<div style='background-color: %s; padding: 12px; border-left: 4px solid %s; margin: 10px 0; color: %s;'>
                        <strong>%s %s:</strong> %s
                    </div>",
                    color$bg, color$border, color$text, color$icon,
                    tools::toTitleCase(gsub("_", " ", type)), content
                )))
            }

            # Validate required inputs
            if (is.null(self$options$dx_date) || is.null(self$options$fu_date)) {
                # Show initial message
                todo <- "
                    <br>Welcome to Time Interval Calculator
                    <br><br>
                    This tool helps you calculate time intervals from:
                    <br>- Pre-calculated time values
                    <br>- Date columns using various date formats
                    <br><br>
                    Please select your input method and variables."

                html <- self$results$todo
                html$setContent(todo)
                return()
            }

            # Validate input data structure
            validation <- private$.validateInputData(
                self$data,
                self$options$dx_date,
                self$options$fu_date
            )

            if (!validation$valid) {
                # Add validation error message
                add_message("error", validation$error)
                self$results$messages$setContent(paste(messages, collapse = "\n"))
                return()
            }

            # Try to calculate time intervals with error handling
            calculated_times <- NULL
            tryCatch({
                calculated_times <- private$.calculate_survival_time(
                    data = self$data,
                    dx_date = self$options$dx_date,
                    fu_date = self$options$fu_date,
                    time_format = self$options$time_format,
                    output_unit = self$options$output_unit,
                    time_basis = self$options$time_basis,
                    landmark_time = if(self$options$use_landmark) self$options$landmark_time else NULL,
                    timezone_setting = self$options$timezone
                )
            }, error = function(e) {
                # Add calculation error message
                add_message("error", as.character(e$message))
                self$results$messages$setContent(paste(messages, collapse = "\n"))
            })

            # If calculation failed, stop here
            if (is.null(calculated_times)) {
                return()
            }

            # Add calculated times to results if requested
            if (self$options$add_times && !is.null(calculated_times)) {
                # Extract time values if calculated_times is a list
                if (is.list(calculated_times) && "time" %in% names(calculated_times)) {
                    time_values_for_output <- calculated_times$time
                    # CRITICAL FIX: Use filtered data row numbers if landmark analysis was applied
                    filtered_data <- if ("data" %in% names(calculated_times)) {
                        calculated_times$data
                    } else {
                        self$data
                    }
                } else {
                    time_values_for_output <- calculated_times
                    filtered_data <- self$data
                }

                # Debug: Check if we have valid values to set
                if (!is.null(time_values_for_output) && length(time_values_for_output) > 0) {
                    self$results$calculated_time$setRowNums(rownames(filtered_data))
                    self$results$calculated_time$setValues(time_values_for_output)
                    
                    # Add debug message
                    self$results$todo$setContent(
                        paste("<p><strong>✅ Success:</strong> Added", length(time_values_for_output), "calculated time values to data output.</p>")
                    )
                } else {
                    self$results$todo$setContent(
                        "<p><strong>⚠️ Warning:</strong> No valid time values available to add to data output.</p>"
                    )
                }
            } else {
                if (!self$options$add_times) {
                    self$results$todo$setContent(
                        "<p><strong>ℹ️ Note:</strong> add_times option is disabled. Enable it to add calculated times to data.</p>"
                    )
                } else {
                    self$results$todo$setContent(
                        "<p><strong>⚠️ Warning:</strong> calculated_times is null, cannot add to data output.</p>"
                    )
                }
            }

            # Generate person-time information
            person_time_info <- glue::glue("
                <p><b>Person-Time Follow-Up</b> represents the total observation time contributed by all
                participants in a study. Unlike simple participant counts, person-time captures both the number
                of subjects and their observation duration. This is essential for calculating accurate incidence
                rates and properly accounting for varying follow-up periods.</p>
                
                <p><b>Key Concepts:</b></p>
                <ul>
                    <li><b>Total Person-Time:</b> Sum of all individual follow-up periods</li>
                    <li><b>Incidence Rate:</b> Number of events ÷ Total person-time</li>
                    <li><b>Time Units:</b> Typically expressed as person-{self$options$output_unit}</li>
                    <li><b>Censoring:</b> Accounts for participants leaving the study early</li>
                </ul>
                
                <p><b>Applications:</b></p>
                <ul>
                    <li>Calculate event rates in epidemiological studies</li>
                    <li>Compare incidence between different populations</li>
                    <li>Adjust for varying follow-up periods in survival analysis</li>
                    <li>Provide accurate denominators for rate calculations</li>
                </ul>
            ")

            self$results$personTimeInfo$setContent(person_time_info)

            # Populate About panel
            about_html <- "
                <div style='background-color: #f0f7ff; padding: 15px; border-left: 4px solid #0066cc; margin: 15px 0;'>
                    <h4 style='margin-top: 0; color: #004085;'>📘 What does this analysis do?</h4>
                    <p>Calculates time intervals between two dates, designed for survival analysis and epidemiological studies.</p>

                    <h4 style='color: #004085;'>🎯 When to use:</h4>
                    <ul style='margin: 5px 0;'>
                        <li>Computing follow-up time for survival analysis (e.g., diagnosis to death/last contact)</li>
                        <li>Calculating person-time denominators for incidence rate studies</li>
                        <li>Quality-checking date data before formal statistical analysis</li>
                        <li>Preparing time variables for Cox regression or Kaplan-Meier analysis</li>
                    </ul>

                    <h4 style='color: #004085;'>📊 Key outputs:</h4>
                    <ul style='margin: 5px 0;'>
                        <li><strong>Calculated intervals:</strong> Time between dates in your chosen units (days/weeks/months/years)</li>
                        <li><strong>Summary statistics:</strong> Mean, median, range, and confidence intervals</li>
                        <li><strong>Total person-time:</strong> Sum of all intervals (denominator for incidence rates)</li>
                        <li><strong>Quality assessment:</strong> Flags negative intervals, missing values, and outliers</li>
                    </ul>

                    <h4 style='color: #004085;'>⚡ Quick start:</h4>
                    <ol style='margin: 5px 0;'>
                        <li>Select your <strong>start date</strong> variable (e.g., diagnosis date, study entry)</li>
                        <li>Select your <strong>end date</strong> variable (e.g., death date, last follow-up)</li>
                        <li>Choose date format (or use auto-detect)</li>
                        <li>Select output time unit (days, weeks, months, or years)</li>
                        <li>Optionally enable quality assessment to check data integrity</li>
                    </ol>
                </div>
            "
            self$results$aboutPanel$setContent(about_html)

            # Extract time values from the result list
            if (!is.null(calculated_times) && is.list(calculated_times) && "time" %in% names(calculated_times)) {
                time_values <- calculated_times$time
            } else {
                time_values <- calculated_times  # fallback if it's already a vector
            }
            
            filter_info <- if (is.list(calculated_times) && "filter" %in% names(calculated_times)) {
                calculated_times$filter
            } else {
                list(removed_negative = 0, removed_extreme = 0, extreme_threshold = NA)
            }
            landmark_info <- if (is.list(calculated_times) && "landmark" %in% names(calculated_times)) {
                calculated_times$landmark
            } else {
                list(excluded_count = 0, landmark_time = NA)
            }
            
            filter_lines <- c()
            if (self$options$remove_negative && filter_info$removed_negative > 0) {
                filter_lines <- c(filter_lines, glue::glue("{filter_info$removed_negative} negative interval(s) removed"))
            }
            if (self$options$remove_extreme && filter_info$removed_extreme > 0) {
                threshold_txt <- if (!is.na(filter_info$extreme_threshold)) round(filter_info$extreme_threshold, 2) else "threshold"
                filter_lines <- c(filter_lines, glue::glue("{filter_info$removed_extreme} extreme interval(s) removed (> {threshold_txt} {self$options$output_unit})"))
            }
            if (self$options$use_landmark && !is.null(landmark_info$excluded_count) && landmark_info$excluded_count > 0) {
                filter_lines <- c(filter_lines, glue::glue("{landmark_info$excluded_count} participant(s) excluded by landmark ({self$options$landmark_time} {self$options$output_unit})"))
            }
            filter_text <- if (length(filter_lines) > 0) paste(filter_lines, collapse = "; ") else "None"
            
            # Generate summary statistics
            valid_time_values <- if (!is.null(time_values)) time_values[!is.na(time_values)] else numeric(0)

            if (!is.null(time_values) && length(valid_time_values) > 0) {
                summary_stats <- list(
                    n = length(valid_time_values),
                    mean = mean(valid_time_values, na.rm = TRUE),
                    median = median(valid_time_values, na.rm = TRUE),
                    sd = sd(valid_time_values, na.rm = TRUE),
                    min = min(valid_time_values, na.rm = TRUE),
                    max = max(valid_time_values, na.rm = TRUE),
                    missing = sum(is.na(time_values)),
                    negative = sum(valid_time_values < 0, na.rm = TRUE),
                    total_person_time = sum(valid_time_values, na.rm = TRUE)
                )

                # Calculate confidence intervals if quality metrics requested
                if (self$options$include_quality_metrics) {
                    ci <- private$.calculateCI(
                        summary_stats$mean,
                        summary_stats$sd,
                        summary_stats$n,
                        self$options$confidence_level
                    )
                    summary_stats$ci_lower <- ci$lower
                    summary_stats$ci_upper <- ci$upper
                } else {
                    summary_stats$ci_lower <- NA
                    summary_stats$ci_upper <- NA
                }

                # Create summary text with person-time metrics
                ci_text <- if (!is.na(summary_stats$ci_lower)) {
                    paste0(" (", self$options$confidence_level, "% CI: ",
                           round(summary_stats$ci_lower, 2), " to ",
                           round(summary_stats$ci_upper, 2), ")")
                } else {
                    ""
                }

                summary_text <- glue::glue("

                    <br><b>Time Interval Summary ({self$options$output_unit})</b><br>

                    Number of observations: {summary_stats$n}<br>

                    Time basis: {if (self$options$time_basis == 'calendar') 'Calendar-aware (actual month lengths)' else 'Standardized (30.44-day months, 365.25-day years)'}<br>

                    Total person-time: {round(summary_stats$total_person_time, 2)} person-{self$options$output_unit}<br>

                                    Mean time: {round(summary_stats$mean, 2)}{ci_text}<br>

                                    Median time: {round(summary_stats$median, 2)}<br>

                                    Standard deviation: {round(summary_stats$sd, 2)}<br>

                    Range: {round(summary_stats$min, 2)} to {round(summary_stats$max, 2)}<br>

                    Missing values: {summary_stats$missing}<br>

                    Filters applied: {filter_text}<br>

                    {if(summary_stats$negative > 0) paste('Warning:', summary_stats$negative, 'negative time intervals detected') else ''}

                

                                    <div style='background-color: #e8f5e9; padding: 12px; margin-top: 12px; border-left: 3px solid #4caf50;'>

                                        <strong>💡 Interpretation Example:</strong><br>

                                        With a mean follow-up of {round(summary_stats$mean, 1)} {self$options$output_unit}

                                        (range: {round(summary_stats$min, 1)} to {round(summary_stats$max, 1)} {self$options$output_unit}),

                                        this cohort provides {if(summary_stats$mean > summary_stats$median) 'adequate' else 'good'} observation time.

                                        The total person-time ({round(summary_stats$total_person_time, 1)} person-{self$options$output_unit})

                                        serves as the denominator for calculating incidence rates

                                        (e.g., events per 100 person-{self$options$output_unit}).

                                    </div>

                                ")

                self$results$summary$setContent(summary_text)

                # Small sample size guards
                if (summary_stats$n < 10 && summary_stats$n > 1) {
                    add_message('strong_warning', sprintf('Critically small sample (n=%d). Statistical summaries are unreliable with fewer than 10 observations. Results should be considered exploratory only. Minimum n=20 recommended for basic descriptive analysis.',
                                summary_stats$n))
                } else if (summary_stats$n < 20 && summary_stats$n >= 10) {
                    add_message('warning', sprintf('Small sample size (n=%d). Confidence intervals may be very wide and unreliable with fewer than 20 observations. Consider collecting more data or interpreting results cautiously.',
                                summary_stats$n))
                }

                # Add completion info message
                add_message('info', sprintf('Analysis completed using %d observations with mean follow-up %.1f %s (total person-time: %.1f person-%s).',
                            summary_stats$n, summary_stats$mean, self$options$output_unit,
                            summary_stats$total_person_time, self$options$output_unit))

            } else {
                # Handle case with no valid calculated times
                error_summary <- "
                    <div style='background-color: #f8d7da; padding: 15px; border-left: 4px solid #dc3545; margin: 15px 0;'>
                        <h4 style='margin-top: 0; color: #721c24;'>⚠️ No Valid Time Intervals</h4>
                        <p>No valid time intervals could be calculated from the provided data.</p>
                        <p><strong>Please check:</strong></p>
                        <ul>
                            <li>Date format settings match your data</li>
                            <li>Date columns contain valid dates</li>
                            <li>End dates occur after start dates</li>
                            <li>Data contains non-missing values</li>
                        </ul>
                    </div>"

                self$results$summary$setContent(error_summary)
            }

            # Generate natural-language summary if requested
            if (self$options$show_summary && length(valid_time_values) > 0) {
                n_obs <- summary_stats$n
                mean_time <- round(summary_stats$mean, 1)
                median_time <- round(summary_stats$median, 1)
                total_pt <- round(summary_stats$total_person_time, 1)
                unit <- self$options$output_unit

                landmark_text <- if (self$options$use_landmark && !is.null(calculated_times$landmark)) {
                    sprintf(
                        " after excluding %d participants with follow-up < %s %s (landmark analysis)",
                        calculated_times$landmark$excluded_count,
                        self$options$landmark_time,
                        unit
                    )
                } else {
                    ""
                }

                quality_text <- if (summary_stats$negative > 0 || summary_stats$missing > 0) {
                    warnings <- c()
                    if (summary_stats$negative > 0)
                        warnings <- c(warnings, sprintf("%d negative intervals", summary_stats$negative))
                    if (summary_stats$missing > 0)
                        warnings <- c(warnings, sprintf("%d missing values", summary_stats$missing))
                    sprintf(" Note: %s were detected.", paste(warnings, collapse = " and "))
                } else {
                    ""
                }

                summary_html <- glue::glue("
                    <div style='background-color: #e8f4f8; padding: 20px; border-left: 5px solid #0066cc; margin: 15px 0;'>
                        <h3 style='margin-top: 0; color: #004080;'>📋 Clinical Summary</h3>
                        <p style='font-size: 1.1em; line-height: 1.6;'>
                        <strong>Time interval analysis</strong> was performed on <strong>{n_obs} participants</strong>{landmark_text}.
                        The mean follow-up was <strong>{mean_time} {unit}</strong> (median: {median_time} {unit}),
                        contributing a total of <strong>{total_pt} person-{unit}</strong> of observation.{quality_text}
                        </p>

                        <div style='background-color: #f0f7ff; padding: 15px; margin-top: 15px; border-radius: 5px;'>
                            <p style='font-size: 0.95em; color: #333; margin: 0;'>
                            <strong>📄 Copy-Ready Sentence:</strong><br>
                            <em style='color: #555;'>\"Follow-up data were available for {n_obs} participants
                            (mean {mean_time} {unit}, median {median_time} {unit}), contributing {total_pt} person-{unit}
                            of observation time.\"</em>
                            </p>
                        </div>
                    </div>
                ")

                self$results$nlSummary$setContent(summary_html)
            }

            # Populate Glossary if requested
            if (self$options$show_glossary) {
                glossary_html <- "
                    <div style='background-color: #f3e5f5; padding: 15px; border-left: 4px solid #9c27b0; margin: 15px 0;'>
                        <h4 style='margin-top: 0; color: #4a148c;'>📖 Key Terms Explained</h4>

                        <dl style='margin: 5px 0;'>
                            <dt style='font-weight: bold; margin-top: 10px;'>Person-Time</dt>
                            <dd style='margin-left: 20px;'>Total observation duration across all participants.
                            Example: 100 people followed for 2 years = 200 person-years. This accounts for varying follow-up periods.</dd>

                            <dt style='font-weight: bold; margin-top: 10px;'>Incidence Rate</dt>
                            <dd style='margin-left: 20px;'>Number of new events ÷ person-time.
                            Example: 10 deaths ÷ 200 person-years = 0.05 deaths per person-year (or 5 per 100 person-years).</dd>

                            <dt style='font-weight: bold; margin-top: 10px;'>Landmark Analysis</dt>
                            <dd style='margin-left: 20px;'>Start follow-up from a specific time point, excluding early events.
                            Example: Only include 6-month survivors to study long-term outcomes, avoiding guarantee-time bias.</dd>

                            <dt style='font-weight: bold; margin-top: 10px;'>Negative Interval</dt>
                            <dd style='margin-left: 20px;'>Time interval where end date occurs before start date.
                            Usually indicates data entry error (e.g., dates swapped, wrong year entered).</dd>

                            <dt style='font-weight: bold; margin-top: 10px;'>Censoring</dt>
                            <dd style='margin-left: 20px;'>Participants who leave the study before experiencing the event.
                            Their follow-up time contributes to person-time even though the event wasn't observed.</dd>

                            <dt style='font-weight: bold; margin-top: 10px;'>Confidence Interval (CI)</dt>
                            <dd style='margin-left: 20px;'>Range likely to contain the true mean follow-up time.
                            Example: Mean = 12 months (95% CI: 10-14) means we're 95% confident the true mean is between 10 and 14 months.</dd>
                        </dl>
                    </div>
                "
                self$results$glossaryPanel$setContent(glossary_html)
            }

            # Generate contextual warnings for data quality issues using static Notices
            if (!is.null(calculated_times) && is.list(calculated_times) &&
                !is.null(calculated_times$quality)) {
                quality <- calculated_times$quality

                # Note: Negative intervals with remove_negative=FALSE are handled as ERROR
                # (stop at line 491, caught by tryCatch above). No warning needed here.

                # High missing data WARNING
                if (quality$missing_values > 0) {
                    pct <- round(100 * quality$missing_values / quality$total_observations, 1)
                    if (pct > 10) {
                        add_message('warning', sprintf('%d observations (%.1f%%) have missing time intervals. Investigate missing date values as this may affect study conclusions.',
                                    quality$missing_values, pct))
                    }
                }

                # Future dates STRONG_WARNING
                if (quality$future_dates > 0) {
                    add_message('strong_warning', sprintf('%d date values are in the future. Review date columns for data entry errors or incorrect date formats.',
                                quality$future_dates))
                }
            }

            # Populate quality assessment if requested
            if (self$options$include_quality_metrics && !is.null(calculated_times) &&
                is.list(calculated_times) && !is.null(calculated_times$quality)) {
                quality <- calculated_times$quality

                quality_html <- glue::glue(
                    "<div style='background-color: #f8f9fa; padding: 15px; border-left: 4px solid #007bff; margin: 15px 0;'>",
                        "<h4 style='margin-top: 0; color: #004085;'>📊 Data Quality Assessment</h4>",

                        "<p><strong>Overall Quality:</strong> {quality$overall_quality}</p>",

                        "<table style='width: 100%; border-collapse: collapse; margin-top: 10px;'>",
                            "<tr style='background-color: #e9ecef;'>",
                                "<th style='padding: 8px; text-align: left; border: 1px solid #dee2e6;'>Metric</th>",
                                "<th style='padding: 8px; text-align: right; border: 1px solid #dee2e6;'>Count</th>",
                                "<th style='padding: 8px; text-align: right; border: 1px solid #dee2e6;'>%</th>",
                            "</tr>",
                            "<tr>",
                                "<td style='padding: 8px; border: 1px solid #dee2e6;'>Total Observations</td>",
                                "<td style='padding: 8px; text-align: right; border: 1px solid #dee2e6;'>{quality$total_observations}</td>",
                                "<td style='padding: 8px; text-align: right; border: 1px solid #dee2e6;'>100%</td>",
                            "</tr>",
                            "<tr style='background-color: #fff3cd;'>",
                                "<td style='padding: 8px; border: 1px solid #dee2e6;'>Missing Values</td>",
                                "<td style='padding: 8px; text-align: right; border: 1px solid #dee2e6;'>{quality$missing_values}</td>",
                                "<td style='padding: 8px; text-align: right; border: 1px solid #dee2e6;'>{round(100*quality$missing_values/quality$total_observations, 1)}%</td>",
                            "</tr>",
                            "<tr style='background-color: #f8d7da;'>",
                                "<td style='padding: 8px; border: 1px solid #dee2e6;'>Negative Intervals</td>",
                                "<td style='padding: 8px; text-align: right; border: 1px solid #dee2e6;'>{quality$negative_intervals}</td>",
                                "<td style='padding: 8px; text-align: right; border: 1px solid #dee2e6;'>{round(100*quality$negative_intervals/quality$total_observations, 1)}%</td>",
                            "</tr>",
                            "<tr>",
                                "<td style='padding: 8px; border: 1px solid #dee2e6;'>Zero Intervals</td>",
                                "<td style='padding: 8px; text-align: right; border: 1px solid #dee2e6;'>{quality$zero_intervals}</td>",
                                "<td style='padding: 8px; text-align: right; border: 1px solid #dee2e6;'>{round(100*quality$zero_intervals/quality$total_observations, 1)}%</td>",
                            "</tr>",
                            "<tr style='background-color: #fff3cd;'>",
                                "<td style='padding: 8px; border: 1px solid #dee2e6;'>Extreme Values</td>",
                                "<td style='padding: 8px; text-align: right; border: 1px solid #dee2e6;'>{quality$extreme_values}</td>",
                                "<td style='padding: 8px; text-align: right; border: 1px solid #dee2e6;'>{round(100*quality$extreme_values/quality$total_observations, 1)}%</td>",
                            "</tr>",
                        "</table>",

                        "{if (length(filter_lines) > 0) paste0('<p><strong>Filters applied:</strong> ', filter_text, '</p>') else ''}",

                        "{if(length(quality$warnings) > 0) paste0('<p style=\"margin-top: 15px;\"><strong>⚠️ Warnings:</strong></p><ul>', paste0('<li>', quality$warnings, '</li>', collapse=''), '</ul>') else ''}",
                    "</div>"
                )

                self$results$qualityAssessment$setContent(quality_html)

                # Populate Caveats panel (only when quality metrics enabled)
                caveats_html <- "
                    <div style='background-color: #fff8e1; padding: 15px; border-left: 4px solid #ff9800; margin: 15px 0;'>
                        <h4 style='margin-top: 0; color: #7f5006;'>⚠️ Important Assumptions</h4>
                        <ul style='margin: 5px 0;'>
                            <li><strong>Time Units (Months/Years):</strong> To ensure statistical consistency for survival analysis, this tool uses <strong>standardized durations</strong> (1 month = 30.4375 days, 1 year = 365.25 days) rather than calendar units. This prevents bias from varying month lengths (28-31 days).</li>
                            <li><strong>End dates should occur on or after start dates</strong> - Negative intervals usually indicate data entry errors</li>
                            <li><strong>Date formats must be consistent</strong> - All dates in a column should use the same format</li>
                            <li><strong>Landmark analysis excludes participants</strong> - Only those with follow-up ≥ landmark time are included</li>
                            <li><strong>Missing dates produce missing intervals</strong> - These are excluded from summary statistics</li>
                        </ul>

                        <h4 style='color: #7f5006;'>🔍 Common Pitfalls</h4>
                        <ul style='margin: 5px 0;'>
                            <li><strong>Mixed date formats:</strong> DD/MM/YYYY vs MM/DD/YYYY in same column → Use manual format selection</li>
                            <li><strong>Text vs numeric dates:</strong> Ensure dates are stored consistently (all text or all numeric)</li>
                            <li><strong>Future dates:</strong> End dates after today's date may indicate data errors</li>
                            <li><strong>Extreme outliers:</strong> Very long intervals may be real (long follow-up) or errors</li>
                        </ul>

                        <h4 style='color: #7f5006;'>💡 Troubleshooting</h4>
                        <ul style='margin: 5px 0;'>
                            <li>If auto-detection fails, manually select your date format</li>
                            <li>Check for negative intervals - these indicate date column errors</li>
                            <li>Review extreme values - anything > 2× the 99th percentile is flagged</li>
                            <li>Ensure date columns don't contain non-date values (text, codes, etc.)</li>
                        </ul>
                    </div>
                "
                self$results$caveatsPanel$setContent(caveats_html)
            }

            # Output all collected messages
            if (length(messages) > 0) {
                self$results$messages$setContent(paste(messages, collapse = "\n"))
            }
        }
    )
)
