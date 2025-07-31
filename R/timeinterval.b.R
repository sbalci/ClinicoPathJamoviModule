#' @title Comprehensive Time Interval Calculator for Survival Analysis
#' 
#' @description 
#' Advanced time interval calculation tool designed for survival analysis, epidemiological 
#' studies, and person-time analysis. Provides robust date parsing, time interval 
#' calculation, landmark analysis, and comprehensive data quality assessment.
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
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import lubridate
#' @import glue
#' @import dplyr
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
                stop(paste("Start date column '", dx_date, "' not found in data"))
            }
            
            if (!fu_date %in% names(data)) {
                stop(paste("End date column '", fu_date, "' not found in data"))
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
                    parsed_dates <- parser(sample_dates)
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
        
        .parseDate = function(date_vector, format) {
            # Enhanced date parsing with better error handling
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
                parsed_dates <- date_parser(date_vector)
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
                                            landmark_time = NULL) {

            # Comprehensive input validation
            private$.validateInputData(data, dx_date, fu_date)

            # Detect date format if needed
            detected_format <- private$.detectDateFormat(data[[dx_date]], time_format)
            
            # Parse dates with enhanced error handling
            start_dates <- private$.parseDate(data[[dx_date]], detected_format)
            end_dates <- private$.parseDate(data[[fu_date]], detected_format)

            # Calculate time intervals
            calculated_time <- private$.calculateTimeIntervals(start_dates, end_dates, output_unit)
            
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
                landmark_time = self$options$landmark_time
            )

            # Add calculated times to results if requested
            if (self$options$add_times && !is.null(calculated_times)) {
                # Extract time values if calculated_times is a list
                if (is.list(calculated_times) && "time" %in% names(calculated_times)) {
                    time_values_for_output <- calculated_times$time
                } else {
                    time_values_for_output <- calculated_times
                }
                
                # Debug: Check if we have valid values to set
                if (!is.null(time_values_for_output) && length(time_values_for_output) > 0) {
                    self$results$calculated_time$setRowNums(rownames(self$data))
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

                # if (any(time_data$time < 0, na.rm = TRUE)) {
                #     warnings$negative_intervals <- "Negative intervals found (end date before start date) and removed"
                #     time_data$time[time_data$time < 0] <- NA
                # }

                # Apply landmark time if specified
                # if (!is.null(landmark)) {
                #     if (!is.numeric(landmark) || landmark < 0) {
                #         stop("Landmark time must be a non-negative number")
                #     }
                #
                #     original_n <- nrow(time_data)
                #     time_data <- time_data[time_data$time >= landmark, ]
                #     time_data$time <- time_data$time - landmark
                #
                #     if (nrow(time_data) < original_n) {
                #         warnings$landmark_filtering <- sprintf(
                #             "%d observations removed due to landmark time of %f %s",
                #             original_n - nrow(time_data),
                #             landmark,
                #             output_units
                #         )
                #     }
                # }



                # Create summary text with person-time metrics
                summary_text <- glue::glue("
                    <br><b>Time Interval Summary ({self$options$output_unit})</b><br>
                    Number of observations: {summary_stats$n}<br>
                    Total person-time: {round(summary_stats$total_person_time, 2)} person-{self$options$output_unit}<br>
                    Mean time: {round(summary_stats$mean, 2)}<br>
                    Median time: {round(summary_stats$median, 2)}<br>
                    Standard deviation: {round(summary_stats$sd, 2)}<br>
                    Range: {round(summary_stats$min, 2)} to {round(summary_stats$max, 2)}<br>
                    Missing values: {summary_stats$missing}<br>
                    {if(summary_stats$negative > 0) paste('Warning:', summary_stats$negative, 'negative time intervals detected') else ''}
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
        },
        
        # ===================================================================
        # ADDITIONAL HELPER FUNCTIONS FOR ENHANCED FUNCTIONALITY
        # ===================================================================
        
        .exportResults = function(calculated_times, original_data, landmark_info) {
            # Prepare data for export with comprehensive metadata
            
            if (is.null(calculated_times) || length(calculated_times) == 0) {
                return(NULL)
            }
            
            # Create export data frame
            if (!is.null(landmark_info$landmark_time) && landmark_info$landmark_time > 0) {
                export_data <- landmark_info$data
                export_data$calculated_time_intervals <- calculated_times
                export_data$landmark_adjusted <- TRUE
                export_data$landmark_time <- landmark_info$landmark_time
            } else {
                export_data <- original_data[seq_along(calculated_times), ]
                export_data$calculated_time_intervals <- calculated_times
                export_data$landmark_adjusted <- FALSE
            }
            
            return(export_data)
        }
    )
)
