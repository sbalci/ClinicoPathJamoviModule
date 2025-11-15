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
            # Comprehensive input validation
            if (!is.data.frame(data)) {
                stop("Input must be a data frame")
            }
            
            if (nrow(data) == 0) {
                stop("Data frame is empty")
            }
            
            # Validate date columns exist
            if (!dx_date %in% names(data)) {
                available <- paste(head(names(data), 8), collapse = ", ")
                suffix <- if(ncol(data) > 8) "..." else ""
                stop(glue::glue(
                    "Start date column '{dx_date}' not found.\n",
                    "Available columns: {available}{suffix}"
                ))
            }

            if (!fu_date %in% names(data)) {
                available <- paste(head(names(data), 8), collapse = ", ")
                suffix <- if(ncol(data) > 8) "..." else ""
                stop(glue::glue(
                    "End date column '{fu_date}' not found.\n",
                    "Available columns: {available}{suffix}"
                ))
            }
            
            # Check for completely missing date columns
            if (all(is.na(data[[dx_date]]))) {
                stop("Start date column contains only missing values")
            }
            
            if (all(is.na(data[[fu_date]]))) {
                stop("End date column contains only missing values")
            }
            
            return(TRUE)
        },
        
        .detectDateFormat = function(date_vector, specified_format = NULL) {
            # Automatic date format detection if not specified
            if (!is.null(specified_format) && specified_format != "auto") {
                return(specified_format)
            }
            
            # Remove missing values for format detection
            sample_dates <- date_vector[!is.na(date_vector)]
            if (length(sample_dates) == 0) {
                stop("No valid dates found for format detection")
            }
            
            # Take a sample for format detection
            sample_dates <- head(sample_dates, min(10, length(sample_dates)))
            
            # Test common formats
            formats_to_try <- c("ymd", "dmy", "mdy", "ydm", "myd", "dym", "ymdhms")
            
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
                    parsed_dates <- parser(sample_dates, quiet = TRUE)
                    # If most dates parse successfully, use this format
                    if (sum(!is.na(parsed_dates)) / length(sample_dates) > 0.8) {
                        return(fmt)
                    }
                }, error = function(e) {
                    # Continue to next format
                })
            }
            
            # If no format works well, default to ymd
            warning("Could not reliably detect date format. Using YMD format. Please specify format manually if incorrect.")
            return("ymd")
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
            calculated_time <- lubridate::time_length(intervals, output_unit)
            
            return(calculated_time)
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
            valid_cases <- calculated_time >= landmark_time
            excluded_count <- sum(!valid_cases, na.rm = TRUE)
            
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

            quality_metrics <- list(
                total_observations = length(calculated_time),
                missing_values = sum(is.na(calculated_time)),
                negative_intervals = sum(calculated_time < 0, na.rm = TRUE),
                zero_intervals = sum(calculated_time == 0, na.rm = TRUE),
                extreme_values = sum(calculated_time > quantile(calculated_time, 0.99, na.rm = TRUE) * 2, na.rm = TRUE),
                missing_start_dates = sum(is.na(start_dates)),
                missing_end_dates = sum(is.na(end_dates)),
                future_dates = sum(start_dates > Sys.Date(), na.rm = TRUE) + sum(end_dates > Sys.Date(), na.rm = TRUE)
            )
            
            # Generate quality warnings
            warnings <- character()
            
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
                                            landmark_time = NULL,
                                            timezone_setting = "system") {

            # Comprehensive input validation
            private$.validateInputData(data, dx_date, fu_date)

            # Detect date format if needed
            detected_format <- private$.detectDateFormat(data[[dx_date]], time_format)

            # Convert timezone setting to lubridate format
            tz <- if (timezone_setting == "utc") "UTC" else ""

            # Parse dates with enhanced error handling
            start_dates <- private$.parseDate(data[[dx_date]], detected_format, tz = tz)
            end_dates <- private$.parseDate(data[[fu_date]], detected_format, tz = tz)

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
            calculated_time <- private$.calculateTimeIntervals(start_dates, end_dates, output_unit)

            # Apply data quality filters if requested (combined for performance)
            original_data <- data
            valid_idx <- rep(TRUE, length(calculated_time))
            filter_applied <- FALSE

            if (self$options$remove_negative) {
                valid_idx <- valid_idx & (calculated_time >= 0 | is.na(calculated_time))
                filter_applied <- TRUE
            }

            if (self$options$remove_extreme) {
                q99 <- quantile(calculated_time, 0.99, na.rm = TRUE)
                threshold <- q99 * self$options$extreme_multiplier
                valid_idx <- valid_idx & (calculated_time <= threshold | is.na(calculated_time))
                filter_applied <- TRUE
            }

            # Apply combined filter in single operation
            if (filter_applied && !all(valid_idx)) {
                calculated_time <- calculated_time[valid_idx]
                data <- data[valid_idx, ]
                start_dates <- start_dates[valid_idx]
                end_dates <- end_dates[valid_idx]
            }

            # Assess data quality
            quality_assessment <- private$.assessDataQuality(calculated_time, start_dates, end_dates)
            
            # Apply landmark analysis if specified
            landmark_result <- private$.applyLandmarkAnalysis(calculated_time, data, landmark_time, output_unit)

            return(list(
                time = landmark_result$time,
                data = landmark_result$data,
                quality = quality_assessment,
                landmark = landmark_result,
                original_data = data,
                detected_format = detected_format
            ))
        },

        # Run analysis ----
        .run = function() {
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

            # Calculate time intervals
            calculated_times <- private$.calculate_survival_time(
                data = self$data,
                dx_date = self$options$dx_date,
                fu_date = self$options$fu_date,
                time_format = self$options$time_format,
                output_unit = self$options$output_unit,
                landmark_time = if(self$options$use_landmark) self$options$landmark_time else NULL,
                timezone_setting = self$options$timezone
            )

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
            
            # Generate summary statistics
            if (!is.null(time_values) && length(time_values) > 0) {
                summary_stats <- list(
                    n = length(time_values),
                    mean = mean(time_values, na.rm = TRUE),
                    median = median(time_values, na.rm = TRUE),
                    sd = sd(time_values, na.rm = TRUE),
                    min = min(time_values, na.rm = TRUE),
                    max = max(time_values, na.rm = TRUE),
                    missing = sum(is.na(time_values)),
                    negative = sum(time_values < 0, na.rm = TRUE),
                    total_person_time = sum(time_values, na.rm = TRUE)
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
                    Total person-time: {round(summary_stats$total_person_time, 2)} person-{self$options$output_unit}<br>
                    Mean time: {round(summary_stats$mean, 2)}{ci_text}<br>
                    Median time: {round(summary_stats$median, 2)}<br>
                    Standard deviation: {round(summary_stats$sd, 2)}<br>
                    Range: {round(summary_stats$min, 2)} to {round(summary_stats$max, 2)}<br>
                    Missing values: {summary_stats$missing}<br>
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
            if (self$options$show_summary && !is.null(time_values) && length(time_values) > 0) {
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

            # Generate contextual warnings for data quality issues
            if (!is.null(calculated_times) && is.list(calculated_times) &&
                !is.null(calculated_times$quality)) {
                quality <- calculated_times$quality
                warnings_html <- ""

                # Negative intervals warning
                if (quality$negative_intervals > 0 && !self$options$remove_negative) {
                    pct <- round(100 * quality$negative_intervals / quality$total_observations, 1)
                    warnings_html <- paste0(warnings_html, sprintf("
                        <div style='background-color: #fff3cd; padding: 15px; margin: 10px 0; border-left: 4px solid #ffc107;'>
                            <strong>⚠️ Warning: Negative Intervals Detected</strong><br>
                            %d observations (%.1f%%) have end dates before start dates.<br>
                            <strong>Recommendation:</strong> Enable 'Remove Negative Intervals' or review your date columns for errors.
                        </div>
                    ", quality$negative_intervals, pct))
                }

                # High missing data warning
                if (quality$missing_values > 0) {
                    pct <- round(100 * quality$missing_values / quality$total_observations, 1)
                    if (pct > 10) {
                        warnings_html <- paste0(warnings_html, sprintf("
                            <div style='background-color: #fff3cd; padding: 15px; margin: 10px 0; border-left: 4px solid #ffc107;'>
                                <strong>⚠️ Warning: High Missing Data</strong><br>
                                %d observations (%.1f%%) have missing time intervals.<br>
                                <strong>Recommendation:</strong> Investigate missing date values. This may affect study conclusions.
                            </div>
                        ", quality$missing_values, pct))
                    }
                }

                # Future dates warning
                if (quality$future_dates > 0) {
                    warnings_html <- paste0(warnings_html, sprintf("
                        <div style='background-color: #f8d7da; padding: 15px; margin: 10px 0; border-left: 4px solid #dc3545;'>
                            <strong>🚨 Data Quality Issue: Future Dates</strong><br>
                            %d date values are in the future.<br>
                            <strong>Action Required:</strong> Review date columns for data entry errors or incorrect date formats.
                        </div>
                    ", quality$future_dates))
                }

                # Display warnings if any exist
                if (warnings_html != "") {
                    current_todo <- self$results$todo$content
                    if (!is.null(current_todo) && current_todo != "") {
                        # Append to existing content
                        self$results$todo$setContent(paste0(current_todo, warnings_html))
                    } else {
                        self$results$todo$setContent(warnings_html)
                    }
                }
            }

            # Populate quality assessment if requested
            if (self$options$include_quality_metrics && !is.null(calculated_times) &&
                is.list(calculated_times) && !is.null(calculated_times$quality)) {
                quality <- calculated_times$quality

                quality_html <- glue::glue("
                    <div style='background-color: #f8f9fa; padding: 15px; border-left: 4px solid #007bff; margin: 15px 0;'>
                        <h4 style='margin-top: 0; color: #004085;'>📊 Data Quality Assessment</h4>

                        <p><strong>Overall Quality:</strong> {quality$overall_quality}</p>

                        <table style='width: 100%; border-collapse: collapse; margin-top: 10px;'>
                            <tr style='background-color: #e9ecef;'>
                                <th style='padding: 8px; text-align: left; border: 1px solid #dee2e6;'>Metric</th>
                                <th style='padding: 8px; text-align: right; border: 1px solid #dee2e6;'>Count</th>
                                <th style='padding: 8px; text-align: right; border: 1px solid #dee2e6;'>%</th>
                            </tr>
                            <tr>
                                <td style='padding: 8px; border: 1px solid #dee2e6;'>Total Observations</td>
                                <td style='padding: 8px; text-align: right; border: 1px solid #dee2e6;'>{quality$total_observations}</td>
                                <td style='padding: 8px; text-align: right; border: 1px solid #dee2e6;'>100%</td>
                            </tr>
                            <tr style='background-color: #fff3cd;'>
                                <td style='padding: 8px; border: 1px solid #dee2e6;'>Missing Values</td>
                                <td style='padding: 8px; text-align: right; border: 1px solid #dee2e6;'>{quality$missing_values}</td>
                                <td style='padding: 8px; text-align: right; border: 1px solid #dee2e6;'>{round(100*quality$missing_values/quality$total_observations, 1)}%</td>
                            </tr>
                            <tr style='background-color: #f8d7da;'>
                                <td style='padding: 8px; border: 1px solid #dee2e6;'>Negative Intervals</td>
                                <td style='padding: 8px; text-align: right; border: 1px solid #dee2e6;'>{quality$negative_intervals}</td>
                                <td style='padding: 8px; text-align: right; border: 1px solid #dee2e6;'>{round(100*quality$negative_intervals/quality$total_observations, 1)}%</td>
                            </tr>
                            <tr>
                                <td style='padding: 8px; border: 1px solid #dee2e6;'>Zero Intervals</td>
                                <td style='padding: 8px; text-align: right; border: 1px solid #dee2e6;'>{quality$zero_intervals}</td>
                                <td style='padding: 8px; text-align: right; border: 1px solid #dee2e6;'>{round(100*quality$zero_intervals/quality$total_observations, 1)}%</td>
                            </tr>
                            <tr style='background-color: #fff3cd;'>
                                <td style='padding: 8px; border: 1px solid #dee2e6;'>Extreme Values</td>
                                <td style='padding: 8px; text-align: right; border: 1px solid #dee2e6;'>{quality$extreme_values}</td>
                                <td style='padding: 8px; text-align: right; border: 1px solid #dee2e6;'>{round(100*quality$extreme_values/quality$total_observations, 1)}%</td>
                            </tr>
                        </table>

                        {if(length(quality$warnings) > 0) paste0('<p style=\"margin-top: 15px;\"><strong>⚠️ Warnings:</strong></p><ul>', paste0('<li>', quality$warnings, '</li>', collapse=''), '</ul>') else ''}
                    </div>
                ")

                self$results$qualityAssessment$setContent(quality_html)

                # Populate Caveats panel (only when quality metrics enabled)
                caveats_html <- "
                    <div style='background-color: #fff8e1; padding: 15px; border-left: 4px solid #ff9800; margin: 15px 0;'>
                        <h4 style='margin-top: 0; color: #7f5006;'>⚠️ Important Assumptions</h4>
                        <ul style='margin: 5px 0;'>
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
        }
    )
)
