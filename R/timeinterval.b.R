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
#' @param time_basis "standardized" (30.4375-day months, 365.25-day years) or "calendar"
#'   (actual month lengths). Standardized is the default and suits person-time denominators.
#' @param use_landmark Enable landmark analysis (conditional survival from time point)
#' @param landmark_time Landmark time point in output units
#' @param remove_negative Remove negative intervals (end before start)
#' @param remove_extreme Remove extreme outliers (above extreme_multiplier x the 99th
#'   percentile) from every statistic, including total person-time
#' @param extreme_multiplier Multiplier applied to the 99th percentile to set the
#'   extreme-value threshold (1.5-5.0, default 2.0)
#' @param add_times Add calculated intervals as new variable in dataset
#' @param include_quality_metrics Include comprehensive quality assessment
#' @param confidence_level Confidence level for mean intervals (90-99%)
#' @param show_summary Emit a plain-language, copy-ready clinical summary
#' @param show_glossary Show definitions of person-time, incidence rate, landmark analysis
#' @param timezone "system" or "utc"; affects only the "ymdhms" format, the sole format
#'   carrying a time of day
#'
#' @return A `timeintervalResults` object: a jamovi results group holding the summary,
#'   person-time, quality-assessment and glossary panels plus the optional calculated-time
#'   output column. Not a plain list.
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
#' \dontrun{
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
#' }
#'
#' @importFrom R6 R6Class
#'

timeintervalClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "timeintervalClass",
    inherit = timeintervalBase,
    private = list(
        # ===================================================================
        # COMPREHENSIVE TIME INTERVAL CALCULATION FUNCTIONS
        # ===================================================================

        # Holds a human-readable note when auto date-format detection is
        # ambiguous (two or more formats parse the data equally well).
        .formatDetectionNote = NULL,

        # Holds a human-readable note when extreme-value filtering was requested
        # but the "multiplier x 99th percentile" rule is not usable (q99 <= 0).
        .extremeSkipReason = NULL,

        .validateInputData = function(data, dx_date, fu_date) {
            # Input validation - returns status instead of throwing errors.
            #
            # Deliberately does NOT re-check is.data.frame(data) or that the two
            # columns exist. jmvcore resolves the Variable options against the
            # dataset before .run() is entered and throws first ("Argument 'dx_date'
            # contains 'x' which is not present in the dataset"), so those branches
            # were unreachable. Only the checks jmvcore does not make live here.
            if (nrow(data) == 0) {
                return(list(
                    valid = FALSE,
                    error = "Data frame is empty; ensure your dataset has at least one row."
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

        # Reject spreadsheet / statistics-package day-count serials before any
        # parser sees them. Exported as plain numbers they are just integers, and a
        # lenient parser splits the digits ("42370" -> 4/23/70 -> 1970-04-23), scoring
        # 1.00 in .detectDateFormat(). The measured result was a cohort with 12 months
        # of follow-up reporting 792 months and 15843 person-months.
        #
        # The test is DIGIT WIDTH on a genuinely numeric column, which is the only
        # thing that actually separates the two encodings:
        #   * every day-count serial for a realistic calendar year is 5 digits --
        #     1927-06-01 = 10014 and 2143-06-01 = 88907 (spreadsheet epoch
        #     1899-12-30), and R's own numeric Date origin lands in the same band.
        #   * every packed calendar integer is 6 or 8 -- YYYYMMDD (20200115),
        #     YYMMDD / MMDDYY / DDMMYY (200115, 110195, 130195).
        # An earlier version asked "does lubridate::ymd() refuse it?" instead. That
        # was wrong in BOTH directions: ymd(40101) succeeds (as year 4), so a
        # 2009-2012 cohort in serials sailed through, while ymd(110195) fails, so
        # numeric MMDDYY was rejected even with mdy explicitly selected.
        #
        # 5-digit numbers are genuinely ambiguous -- 20115 is both YMMDD 2002-01-15
        # and serial 1955-01-26 -- so they are refused rather than guessed. Guessing
        # wrong is exactly the silent two-orders-of-magnitude error this prevents,
        # and YMMDD is a lubridate leniency nobody exports, while serials are what
        # every spreadsheet produces.
        .checkDateSerial = function(date_vector, column_name) {
            # NUMERIC COLUMNS ONLY. A text or factor column of real dates is never a
            # serial column, and testing it by coercion would let a single stray
            # numeric cell -- an SPSS/Excel missing code such as "99999", or a typo --
            # condemn the whole column, because as.numeric() turns every genuine text
            # date into NA and the vote would then be taken over that one cell.
            if (!is.numeric(date_vector))
                return(invisible(NULL))

            v <- utils::head(date_vector[!is.na(date_vector)], 50)  # as .detectDateFormat()
            if (length(v) == 0)
                return(invisible(NULL))

            serial <- v >= 10000 & v < 100000 & v == trunc(v)
            if (mean(serial) < 0.8)
                return(invisible(NULL))

            example <- v[serial][1]
            # Each .() wraps one complete sentence; the newlines that lay the message
            # out are joined OUTSIDE the translated strings, so no translatable unit
            # contains a line break.
            hint <- if (example >= 10000 && example <= 88907)
                        .fmt(.("Read as a spreadsheet day count, {value} is {date}."),
                             value = base::format(example),
                             date = base::format(as.Date(example, origin = "1899-12-30")))
                    else ""
            headline <- .fmt(
                .("Column '{column}' holds five-digit numbers such as {value}, which cannot be read as dates unambiguously."),
                column = column_name, value = base::format(example))
            jmvcore::reject(paste(
                paste(c(headline, hint), collapse = " "),
                .("Spreadsheets and statistics packages store a date as a count of days since an epoch, and exporting the column as plain numbers loses the date formatting."),
                .("Guessed as a packed date instead, the same digits give a different year, so the time intervals and the person-time would be wrong by decades without any visible sign."),
                .("Fix the source data: format the column as a date before exporting (in Excel: Format Cells > Date), or convert it to text in YYYY-MM-DD form, then re-import."),
                sep = "\n"))
        },

        .detectDateFormat = function(start_vector,
                                     end_vector,
                                     specified_format = NULL,
                                     start_name = "start",
                                     end_name = "end") {
            # Automatic date format detection if not specified
            private$.formatDetectionNote <- NULL
            if (!is.null(specified_format) && specified_format != "auto") {
                return(specified_format)
            }
            
            # Remove missing values for format detection
            sample_start <- start_vector[!is.na(start_vector)]
            sample_end <- end_vector[!is.na(end_vector)]
            
            if (length(sample_start) == 0 && length(sample_end) == 0) {
                jmvcore::reject(.("No valid dates found for format detection in either column"))
            }
            
            sample_size <-  min(50, max(length(sample_start), length(sample_end)))
            sample_start <- head(sample_start, sample_size)
            sample_end <- head(sample_end, sample_size)
            
            # Test common formats
            formats_to_try <- c("ymd", "dmy", "mdy", "ydm", "myd", "dym", "ymdhms")
            
            best_format <- "ymd"
            best_score <- -1
            format_scores <- stats::setNames(rep(-1, length(formats_to_try)), formats_to_try)

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
                    format_scores[fmt] <- success_rate

                    if (success_rate > best_score) {
                        best_score <- success_rate
                        best_format <- fmt
                    }
                }, error = function(e) {
                    # Continue to next format
                })
            }

            # Flag ambiguity: two or more plausible formats parse the data
            # equally well. The first candidate (in formats_to_try order) is
            # used, but the user should verify it to avoid silent misparsing.
            tied_formats <- names(format_scores)[
                format_scores >= (best_score - 1e-9) & format_scores >= 0.5]
            if (best_score >= 0.5 && length(tied_formats) > 1) {
                private$.formatDetectionNote <- sprintf(
                    "Auto-detection is ambiguous: %s all parse these dates equally well (%.0f%% success). The '%s' format was used - please verify it matches your data or select the format manually.",
                    paste(tied_formats, collapse = ", "),
                    100 * best_score, best_format)
            }

            if (best_score < 0.5) {
                jmvcore::reject(.fmt(
                    .("Could not detect a common date format for columns '{start}' and '{end}'. Please select the correct format manually."),
                    start = start_name, end = end_name
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
                jmvcore::reject(.fmt(
                    .("Unsupported date format: {format}"), format = format))
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
                jmvcore::reject(.fmt(
                    .("Error parsing dates with format {format}: {message}"),
                    format = format, message = conditionMessage(e)))
            })
        },
        
        .validateParsedDates = function(parsed_dates, original_vector, column_name, format_label) {
            total_non_missing <- sum(!is.na(original_vector))
            successful <- sum(!is.na(parsed_dates))
            
            if (total_non_missing == 0) {
                jmvcore::reject(.fmt(
                    .("Column '{column}' contains only missing values; cannot calculate time intervals."),
                    column = column_name))
            }
            
            if (successful == 0) {
                sample_values <- paste(utils::head(unique(original_vector), 3), collapse = ", ")
                jmvcore::reject(.fmt(
                    .("Date parsing failed for column '{column}' using format '{format}'. Example values: {examples}"),
                    column = column_name, format = format_label, examples = sample_values
                ))
            }
            
            success_rate <- successful / total_non_missing
            if (success_rate < 0.8) {
                jmvcore::reject(.fmt(
                    .("Only {percent}% of non-missing values in '{column}' were parsed with format '{format}'. Please verify that the selected format matches all values or standardise the column."),
                    percent = round(100 * success_rate, 1), column = column_name, format = format_label
                ))
            }
        },
        
        .calculateTimeIntervals = function(start_dates, end_dates, output_unit) {
            # Enhanced interval calculation with validation
            
            # Check for valid date objects
            if (!inherits(start_dates, "Date") && !inherits(start_dates, "POSIXct")) {
                jmvcore::reject(.("Start dates are not valid date objects"))
            }
            
            if (!inherits(end_dates, "Date") && !inherits(end_dates, "POSIXct")) {
                jmvcore::reject(.("End dates are not valid date objects"))
            }
            
            # Calculate intervals
            intervals <- lubridate::interval(start_dates, end_dates)
            
            # Convert to specified time unit
            # For statistical accuracy in survival analysis, we use fixed duration lengths
            # for months (30.4375 days) and years (365.25 days) rather than calendar units.
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
                jmvcore::reject(.("Start dates are not valid date objects"))
            }
            if (!inherits(end_dates, "Date") && !inherits(end_dates, "POSIXct")) {
                jmvcore::reject(.("End dates are not valid date objects"))
            }

            intervals <- lubridate::interval(start_dates, end_dates)
            days <- lubridate::time_length(intervals, "days")

            # Handle simple units directly
            if (output_unit == "days") return(days)
            if (output_unit == "weeks") return(days / 7)

            # Calendar months/years: count whole months, then proportion of remaining month
            whole_months <- intervals %/% lubridate::period(month = 1)
            # Remaining interval after removing whole months.
            # Use add_with_rollback so end-of-month start dates
            # (e.g. Jan 31 + 1 month) roll back to the last valid day
            # (Feb 28/29) instead of becoming NA and being dropped.
            remainder_start <- lubridate::add_with_rollback(start_dates, lubridate::period(month = whole_months))
            remainder_int <- lubridate::interval(remainder_start, end_dates)
            remainder_days <- lubridate::time_length(remainder_int, "days")

            # Avoid division by zero when remainder_start is NA
            days_in_month_start <- ifelse(is.na(remainder_start), NA_real_, lubridate::days_in_month(remainder_start))
            fraction_months <- remainder_days / days_in_month_start
            total_months <- whole_months + fraction_months

            if (output_unit == "months") return(total_months)
            if (output_unit == "years") return(total_months / 12)

            jmvcore::reject(.("Unsupported output unit for calendar-based calculation"))
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
                jmvcore::reject(.("Landmark time must be a non-negative number"))
            }
            
            # Identify cases before landmark time
            # Handle NAs by treating them as excluded from valid set (FALSE)
            valid_cases <- calculated_time >= landmark_time
            valid_cases[is.na(valid_cases)] <- FALSE
            excluded_count <- sum(!valid_cases)

            # Distinguish the two reasons for exclusion so downstream reporting
            # does not conflate missing follow-up with follow-up < landmark.
            na_excluded <- sum(is.na(calculated_time))
            below_excluded <- sum(!is.na(calculated_time) & calculated_time < landmark_time)

            # Filter and adjust times
            adjusted_time <- calculated_time - landmark_time
            # drop = FALSE: `self$data` holds only the columns this analysis asked
            # for, so it has ONE column whenever the same variable is chosen as
            # both the start and the end date. Without drop = FALSE the subset
            # silently returns a bare vector, rownames() on it is NULL, and
            # .run()'s setRowNums(rownames(filtered_data)) then writes the
            # calculated-time column back to the spreadsheet with no row mapping.
            filtered_data <- data[valid_cases, , drop = FALSE]
            final_time <- adjusted_time[valid_cases]

            return(list(
                time = final_time,
                data = filtered_data,
                excluded_count = excluded_count,
                na_excluded = na_excluded,
                below_excluded = below_excluded,
                landmark_time = landmark_time,
                original_n = length(calculated_time),
                final_n = length(final_time)
            ))
        },
        
        .calculateCI = function(mean_val, sd, n, conf_level) {
            # Normal-theory (t) interval for the ARITHMETIC mean interval. A mean
            # duration cannot be negative, so the interval is intersected with the
            # parameter space [0, Inf). Intersecting a CI with a set that contains the
            # true value with probability 1 leaves coverage unchanged -- but a Wald
            # limit below 0 does signal that the normal approximation is poor here, so
            # the untruncated limit is returned for the caller to disclose.
            if (n <= 1) return(list(lower = NA, upper = NA, raw_lower = NA, truncated = FALSE))
            alpha <- 1 - (conf_level / 100)
            se <- sd / sqrt(n)
            margin <- stats::qt(1 - alpha/2, n - 1) * se
            raw_lower <- mean_val - margin
            truncated <- is.finite(raw_lower) && raw_lower < 0
            list(
                lower = if (truncated) 0 else raw_lower,
                upper = mean_val + margin,
                raw_lower = raw_lower,
                truncated = truncated
            )
        },

        .assessDataQuality = function(calculated_time, start_dates, end_dates,
                                      extreme_multiplier = 2) {
            # Comprehensive data quality assessment

            total_obs <- length(calculated_time)
            non_missing <- sum(!is.na(calculated_time))
            suppressWarnings({
                q99 <- stats::quantile(calculated_time, 0.99, na.rm = TRUE, names = FALSE)
            })
            # Use the same multiplier as the removal filter so the flagged
            # "Extreme Values" count matches what remove_extreme would drop --
            # including the q99 <= 0 guard, so the panel does not report a count
            # of "extreme" intervals that the filter itself declines to act on.
            extreme_threshold <- if (is.na(q99) || !is.finite(q99) || q99 <= 0) Inf
                                 else q99 * extreme_multiplier

            quality_metrics <- list(
                total_observations = total_obs,
                missing_values = sum(is.na(calculated_time)),
                negative_intervals = if (non_missing > 0) sum(calculated_time < 0, na.rm = TRUE) else 0,
                zero_intervals = if (non_missing > 0) sum(calculated_time == 0, na.rm = TRUE) else 0,
                extreme_values = if (is.finite(extreme_threshold)) sum(calculated_time > extreme_threshold, na.rm = TRUE) else 0,
                missing_start_dates = sum(is.na(start_dates)),
                missing_end_dates = sum(is.na(end_dates)),
                future_dates = sum(start_dates > Sys.Date(), na.rm = TRUE) + sum(end_dates > Sys.Date(), na.rm = TRUE),
                # Longest interval BEFORE any landmark or filter. The implausible-
                # duration backstop in .run() must test this, not the post-landmark
                # max: subtracting a landmark shortens every interval, which would
                # quietly switch the backstop off on exactly the misparsed data it
                # exists to catch.
                max_interval = if (non_missing > 0) max(calculated_time, na.rm = TRUE) else NA_real_,
                # Share of same-day intervals, so .run() can warn without requiring
                # the quality panel (which is off by default) to be switched on.
                zero_share = if (non_missing > 0)
                    sum(calculated_time == 0, na.rm = TRUE) / non_missing else NA_real_
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

            # A cohort where a large share of patients start and end on the same day
            # is a data problem (unfilled follow-up dates defaulted to the surgery
            # date, or a merge that copied one column onto the other), and it drags
            # the person-time denominator toward zero. Counted but never warned
            # before, so 19 zero intervals out of 20 still scored "Good".
            if (non_missing > 0) {
                zero_pct <- 100 * quality_metrics$zero_intervals / non_missing
                if (zero_pct >= 20) {
                    warnings <- c(warnings, paste0(
                        quality_metrics$zero_intervals, " zero-length intervals (",
                        round(zero_pct, 1), "% of valid intervals) - start and end date are the same day, ",
                        "which contributes no person-time"))
                }
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

            # Trust boundary: both date columns are `permitted: [factor, numeric]`,
            # so a raw spreadsheet export can arrive as day-count serials. Checked
            # before format detection so the guard covers an explicitly chosen
            # (mis)format too, not just auto-detect.
            private$.checkDateSerial(data[[dx_date]], dx_date)
            private$.checkDateSerial(data[[fu_date]], fu_date)

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

            # (An all-NA guard used to sit here. It was dead code: .validateParsedDates()
            # above already rejects when a column yields zero successful parses, and
            # again below 80% success, so all(is.na(...)) can never be TRUE at this
            # point. Its message was also the only remaining multi-line .() string.)

            # Calculate time intervals
            calculated_time_raw <- if (identical(time_basis, "calendar")) {
                private$.calculateCalendarIntervals(start_dates, end_dates, output_unit)
            } else {
                private$.calculateTimeIntervals(start_dates, end_dates, output_unit)
            }

            if (all(is.na(calculated_time_raw))) {
                jmvcore::reject(.("No valid time intervals could be calculated; please verify start/end dates and selected format."))
            }

            # Preserve original parsed vectors for quality assessment
            start_dates_raw <- start_dates
            end_dates_raw <- end_dates

            # Apply data quality filters if requested (combined for performance)
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
                    "Row ", example_rows, ": Start=", base::format(start_dates_raw[example_rows]),
                    ", End=", base::format(end_dates_raw[example_rows])
                )
                # The old message named a checkbox that does not exist ("Remove
                # Negative Intervals"); the real .u.yaml label is "Negative-interval
                # exclusion". It also gave no denominator and no cause. A handful of
                # negatives in an otherwise clean column is the signature of a
                # mis-detected day/month order -- a wrong order flips only the rows
                # whose day-of-month is 12 or less -- so naming that cause matters
                # more than the count. Each .() is one whole sentence; the newlines
                # that lay the message out are joined outside the translated units.
                jmvcore::reject(paste(
                    .fmt(.("Negative time intervals detected (end date before start date) in {count} of {total} rows ({pct}%)."),
                         count = length(negative_idx),
                         total = length(calculated_time_raw),
                         pct = round(100 * length(negative_idx) / length(calculated_time_raw), 1)),
                    .("These rows cannot be analysed: a negative interval subtracts from total person-time and would corrupt any incidence rate computed from that denominator."),
                    .("Usual causes are that the start and end date columns are swapped, or that the date format was mis-detected - a wrong day/month order flips only the rows whose day-of-month is 12 or less, which is why a few rows can be negative while the rest look correct."),
                    .("Correct the dates at source, or tick 'Negative-interval exclusion' under Data Quality & Statistics to drop these rows from every statistic including person-time."),
                    .("Examples:"),
                    paste(examples, collapse = "\n"),
                    sep = "\n"))
            }

            if (self$options$remove_negative) {
                removed_negative <- length(negative_idx)
                valid_idx <- valid_idx & (calculated_time_raw >= 0 | is.na(calculated_time_raw))
                filter_applied <- TRUE
            }

            if (self$options$remove_extreme) {
                suppressWarnings({
                    q99 <- stats::quantile(calculated_time_raw, 0.99, na.rm = TRUE, names = FALSE)
                })
                # The rule is "more than `multiplier` times the 99th percentile",
                # which only orders correctly for a POSITIVE q99. At q99 == 0 the
                # threshold is 0, so every non-zero interval counts as extreme --
                # in a cohort where 99% of patients enter and exit on the same day
                # that silently DELETES the handful of genuine follow-ups. At a
                # negative q99 multiplying moves the threshold the wrong way and
                # flags the entire column. Neither is a meaningful outlier rule, so
                # skip the filter and say why rather than dropping real rows.
                if (!is.na(q99) && is.finite(q99) && q99 > 0) {
                    extreme_threshold <- q99 * self$options$extreme_multiplier
                    removed_extreme <- sum(calculated_time_raw > extreme_threshold, na.rm = TRUE)
                    valid_idx <- valid_idx & (calculated_time_raw <= extreme_threshold | is.na(calculated_time_raw))
                    filter_applied <- TRUE
                } else if (!is.na(q99) && is.finite(q99)) {
                    private$.extremeSkipReason <- sprintf(
                        paste0("Extreme-value filtering was skipped: the 99th percentile of the intervals is %.4g, ",
                               "so a '%.4g x 99th percentile' threshold cannot separate long follow-up from typical ",
                               "follow-up. Review the interval distribution directly."),
                        q99, self$options$extreme_multiplier)
                }
            }

            # Apply combined filter in single operation
            if (filter_applied && !all(valid_idx)) {
                calculated_time <- calculated_time_raw[valid_idx]
                # drop = FALSE for the same reason as in .applyLandmarkAnalysis():
                # a single-column `data` would otherwise collapse to a vector and
                # lose the rownames that the write-back to the dataset relies on.
                data <- data[valid_idx, , drop = FALSE]
                start_dates <- start_dates[valid_idx]
                end_dates <- end_dates[valid_idx]
            } else {
                calculated_time <- calculated_time_raw
            }

            # Assess data quality
            quality_assessment <- private$.assessDataQuality(
                calculated_time_raw, start_dates_raw, end_dates_raw,
                extreme_multiplier = self$options$extreme_multiplier)
            
            # Apply landmark analysis if specified
            landmark_result <- private$.applyLandmarkAnalysis(calculated_time, data, landmark_time, output_unit)

            return(list(
                time = landmark_result$time,
                data = landmark_result$data,
                quality = quality_assessment,
                landmark = landmark_result,
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
            # Per-run state reset so a note from a previous run cannot persist.
            private$.extremeSkipReason <- NULL

            # None of the Html items declare clearWith, and .run() has several early
            # returns, so anything written on a previous run survives into this one:
            # deselect a date variable after a successful run and the old summary,
            # quality panel and message banner stay on screen next to "Getting
            # Started". Blank every panel up front; each is rewritten below when the
            # run actually produces it.
            for (item in c("messages", "todo", "aboutPanel", "personTimeInfo",
                           "qualityAssessment", "caveatsPanel", "summary",
                           "nlSummary", "glossaryPanel"))
                self$results[[item]]$setContent("")

            # Initialize messages list (backed by an environment so the nested
            # add_message() helper can append without `<<-`).
            msg_env <- new.env(parent = emptyenv())
            msg_env$messages <- list()

            # Helper function to add messages. Messages are collected with their
            # severity and only ordered at render time (see .renderMessages below),
            # because they are raised in computation order: the "analysis completed"
            # INFO is emitted before the data-quality warnings, so insertion order
            # used to put a reassuring green banner above a strong warning.
            SEVERITY <- c(error = 1L, strong_warning = 2L, warning = 3L, info = 4L)
            add_message <- function(type, content) {
                color <- switch(type,
                    "error" = list(bg = "#f8d7da", border = "#dc3545", text = "#721c24", icon = ""),
                    "strong_warning" = list(bg = "#fff3cd", border = "#ff8800", text = "#856404", icon = ""),
                    "warning" = list(bg = "#fff3cd", border = "#ffc107", text = "#856404", icon = ""),
                    "info" = list(bg = "#d1ecf1", border = "#17a2b8", text = "#0c5460", icon = ""),
                    list(bg = "#e2e3e5", border = "#6c757d", text = "#383d41", icon = "\u2022")
                )
                rank <- SEVERITY[[type]]
                if (is.null(rank)) rank <- 5L
                msg_env$messages <- c(msg_env$messages, list(list(
                    rank = rank,
                    html = sprintf(
                        "<div style='background-color: %s; padding: 12px; border-left: 4px solid %s; margin: 10px 0; color: %s;'>
                        <strong>%s %s:</strong> %s
                    </div>",
                        color$bg, color$border, color$text, color$icon,
                        tools::toTitleCase(gsub("_", " ", type)),
                        gsub("\n", "<br>", htmltools::htmlEscape(content))
                    ))))
            }

            # Render collected messages most-severe first. order() is stable, so
            # messages of equal severity keep the order they were raised in.
            render_messages <- function() {
                if (length(msg_env$messages) == 0) {
                    self$results$messages$setContent("")
                    return(invisible(NULL))
                }
                ranks <- vapply(msg_env$messages, function(m) m$rank, integer(1))
                html  <- vapply(msg_env$messages, function(m) m$html, character(1))
                self$results$messages$setContent(paste(html[order(ranks)], collapse = "\n"))
            }

            # Validate required inputs
            if (is.null(self$options$dx_date) || is.null(self$options$fu_date)) {
                # Show initial message
                todo <- "
                    <br>Welcome to Time Interval Calculator
                    <br><br>
                    This tool calculates the time interval between two date columns
                    (for example, diagnosis date to last follow-up date) for survival
                    and person-time analysis.
                    <br><br>
                    To begin, select your <b>Start Date</b> and <b>End Date</b> variables,
                    then choose a date format (or leave it on Auto-detect) and an output
                    time unit (days, weeks, months, or years)."

                html <- self$results$todo
                html$setContent(todo)
                return()
            }

            # (The getting-started panel no longer needs clearing here; the blanket
            # reset at the top of .run() already did it, for every panel.)

            # Same column chosen for both ends: every interval is exactly zero, so
            # the person-time denominator is zero and no rate can be computed from
            # it. Cheap to do by accident in the variable supplier, and silent until
            # now.
            if (identical(self$options$dx_date, self$options$fu_date)) {
                add_message('strong_warning', sprintf(
                    'The same variable (%s) is selected as both the start and the end date, so every interval is exactly zero and the total person-time is zero. Select the follow-up or event date as the End Date Variable.',
                    self$options$dx_date))
            }

            # (The timezone notice is raised AFTER parsing, once the format actually
            # used is known -- under "auto" the detector may well pick ymdhms, and
            # claiming the setting is inert would then be false.)

            # Validate input data structure
            validation <- private$.validateInputData(
                self$data,
                self$options$dx_date,
                self$options$fu_date
            )

            if (!validation$valid) {
                # Add validation error message
                add_message("error", validation$error)
                render_messages()
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
                render_messages()
            })

            # If calculation failed, stop here
            if (is.null(calculated_times)) {
                return()
            }

            # Surface ambiguous auto-detection so users can verify the chosen
            # format instead of relying on a silent dmy/mdy guess.
            if (identical(self$options$time_format, "auto") &&
                !is.null(private$.formatDetectionNote)) {
                add_message('warning', private$.formatDetectionNote)
            }

            # Timezone reaches only a parser with a time component; every date-only
            # format yields a Date, which carries no zone. Tested against the format
            # ACTUALLY used, because under "auto" the detector may pick ymdhms and
            # the setting would then be live.
            if (identical(self$options$timezone, "utc") &&
                !identical(calculated_times$detected_format, "ymdhms")) {
                add_message('info', sprintf(
                    'The UTC timezone setting applies only to the "YYYY-MM-DD HH:MM:SS" format, the only one carrying a time of day. These dates were read as "%s", so they are calendar days and the timezone has no effect on the intervals.',
                    calculated_times$detected_format))
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

                # Write the calculated intervals back to the dataset when valid
                if (!is.null(time_values_for_output) && length(time_values_for_output) > 0) {
                    self$results$calculated_time$setRowNums(rownames(filtered_data))
                    self$results$calculated_time$setValues(time_values_for_output)
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
                    <li><b>Incidence Rate:</b> Number of events \u00f7 Total person-time</li>
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
                <div style='background-color: rgba(33, 137, 255, 0.07); padding: 15px; border-left: 4px solid #0066cc; margin: 15px 0; color: inherit;'>
                    <h4 style='margin-top: 0; color: inherit;'> What does this analysis do?</h4>
                    <p>Calculates time intervals between two dates, designed for survival analysis and epidemiological studies.</p>

                    <h4 style='color: inherit;'> When to use:</h4>
                    <ul style='margin: 5px 0;'>
                        <li>Computing follow-up time for survival analysis (e.g., diagnosis to death/last contact)</li>
                        <li>Calculating person-time denominators for incidence rate studies</li>
                        <li>Quality-checking date data before formal statistical analysis</li>
                        <li>Preparing time variables for Cox regression or Kaplan-Meier analysis</li>
                    </ul>

                    <h4 style='color: inherit;'> Key outputs:</h4>
                    <ul style='margin: 5px 0;'>
                        <li><strong>Calculated intervals:</strong> Time between dates in your chosen units (days/weeks/months/years)</li>
                        <li><strong>Summary statistics:</strong> Mean, median, range, and confidence intervals</li>
                        <li><strong>Total person-time:</strong> Sum of all intervals (denominator for incidence rates)</li>
                        <li><strong>Quality assessment:</strong> Flags negative intervals, missing values, and outliers</li>
                    </ul>

                    <h4 style='color: inherit;'> Quick start:</h4>
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

            # .applyLandmarkAnalysis() SUBTRACTS the landmark from every retained
            # interval, so with a landmark active every duration reported below is
            # time measured FROM the landmark, not from the start date. Every surface
            # that reports a duration must say so. isTRUE() absorbs the NA carried by
            # the fallback landmark_info above; a bare `&&` would throw there.
            lm_on   <- isTRUE(landmark_info$landmark_time > 0)
            lm_unit <- self$options$output_unit
            if (lm_on) {
                # Rounded before it reaches any label: the raw option value can be a
                # non-terminating decimal, which would print 15 significant digits.
                lm_val    <- round(landmark_info$landmark_time, 4)
                lm_unit_1 <- sub("s$", "", lm_unit)                                          # "months" -> "month"
                lm_amount <- paste(lm_val, if (isTRUE(lm_val == 1)) lm_unit_1 else lm_unit)  # "6 months"
                lm_adj    <- paste0(lm_val, "-", lm_unit_1)                                  # "6-month"
                lm_hdr_suffix <- paste0(", measured from the ", lm_adj, " landmark")
                lm_pt_label   <- paste0("Total post-landmark person-time (from ", lm_amount, " onward)")
                lm_mean_label <- "Mean post-landmark time"
                lm_fu_phrase  <- "mean post-landmark follow-up"
                lm_pt_phrase  <- "The total post-landmark person-time"
            } else {
                lm_val <- NA; lm_amount <- ""; lm_adj <- ""
                lm_hdr_suffix <- ""
                lm_pt_label   <- "Total person-time"
                lm_mean_label <- "Mean time"
                lm_fu_phrase  <- "mean follow-up"
                lm_pt_phrase  <- "The total person-time"
            }

            filter_lines <- c()
            if (self$options$remove_negative && filter_info$removed_negative > 0) {
                filter_lines <- c(filter_lines, glue::glue("{filter_info$removed_negative} negative interval(s) removed"))
            }
            if (self$options$remove_extreme && !is.null(private$.extremeSkipReason)) {
                filter_lines <- c(filter_lines, private$.extremeSkipReason)
            }
            if (self$options$remove_extreme && filter_info$removed_extreme > 0) {
                threshold_txt <- if (!is.na(filter_info$extreme_threshold)) round(filter_info$extreme_threshold, 2) else "threshold"
                filter_lines <- c(filter_lines, glue::glue("{filter_info$removed_extreme} extreme interval(s) removed (> {threshold_txt} {self$options$output_unit})"))
                # Dropping the longest intervals shortens total person-time, which is
                # the denominator of every incidence rate computed downstream. The
                # summary already lists the filter, but a denominator that moved
                # deserves a message of its own rather than one line of small print.
                add_message('warning', sprintf(
                    'Extreme-value removal dropped %d of the longest interval(s) (above %s %s) from the analysis. These rows are excluded from the mean, the median and the total person-time, so the person-time denominator here is smaller than the follow-up actually observed. Long follow-up is not automatically an error: check the removed rows before using this person-time for an incidence rate.',
                    filter_info$removed_extreme, threshold_txt, self$options$output_unit))
            }
            if (self$options$use_landmark && !is.null(landmark_info$excluded_count) && landmark_info$excluded_count > 0) {
                filter_lines <- c(filter_lines, glue::glue("{landmark_info$excluded_count} participant(s) excluded by landmark ({lm_amount})"))
            }
            filter_text <- if (length(filter_lines) > 0) paste(filter_lines, collapse = "; ") else "None"
            
            # Generate summary statistics
            valid_time_values <- if (!is.null(time_values)) time_values[!is.na(time_values)] else numeric(0)

            if (!is.null(time_values) && length(valid_time_values) > 0) {
                summary_stats <- list(
                    n = length(valid_time_values),
                    mean = mean(valid_time_values, na.rm = TRUE),
                    median = stats::median(valid_time_values, na.rm = TRUE),
                    sd = stats::sd(valid_time_values, na.rm = TRUE),
                    min = min(valid_time_values, na.rm = TRUE),
                    max = max(valid_time_values, na.rm = TRUE),
                    missing = sum(is.na(time_values)),
                    negative = sum(valid_time_values < 0, na.rm = TRUE),
                    total_person_time = sum(valid_time_values, na.rm = TRUE)
                )

                # The CI on the mean is an ordinary descriptive statistic, not part
                # of the quality panel, so it is not gated. .calculateCI() returns NA
                # for n <= 1 and ci_text below collapses to "" in that case.
                ci <- private$.calculateCI(
                    summary_stats$mean,
                    summary_stats$sd,
                    summary_stats$n,
                    self$options$confidence_level
                )
                summary_stats$ci_lower     <- ci$lower
                summary_stats$ci_upper     <- ci$upper
                summary_stats$ci_raw_lower <- ci$raw_lower
                summary_stats$ci_truncated <- isTRUE(ci$truncated)

                # Create summary text with person-time metrics
                ci_text <- if (!is.na(summary_stats$ci_lower)) {
                    paste0(" (", self$options$confidence_level, "% CI: ",
                           round(summary_stats$ci_lower, 2), " to ",
                           round(summary_stats$ci_upper, 2), ")",
                           if (isTRUE(summary_stats$ci_truncated)) " *" else "")
                } else {
                    ""
                }

                # The clamp changes a printed number, so the computed limit has to be
                # disclosed. Built with paste0, not glue: this string is later
                # interpolated into a glue::glue() block and must contain no braces.
                # sd == 0 means every interval is identical, so the t interval
                # collapses to a point. Printing "95% CI: 12.02 to 12.02" with no
                # comment reads as extreme precision rather than as no spread.
                ci_degenerate <- isTRUE(is.finite(summary_stats$sd) && summary_stats$sd == 0 &&
                                        summary_stats$n > 1)
                ci_note <- if (ci_degenerate) {
                    paste0("<span style='font-size: 0.9em;'>All ", summary_stats$n,
                           " intervals are identical, so the confidence interval has zero width. It reflects the absence of any spread in these data, not precision of estimation.</span><br>")
                } else if (isTRUE(summary_stats$ci_truncated)) {
                    paste0("<span style='font-size: 0.9em;'>* The lower confidence limit is shown as 0, not as computed: the ",
                           self$options$confidence_level, "% normal-theory (t) limit was ",
                           signif(summary_stats$ci_raw_lower, 3), " ", self$options$output_unit,
                           ", and a mean follow-up duration cannot be negative. Restricting the interval to the possible range does not change its coverage, but a computed limit below zero means the t interval approximates these data poorly (strongly right-skewed or zero-inflated follow-up, or too few observations): read the median and range instead. The mean and the total person-time are unaffected.</span><br>")
                } else {
                    ""
                }

                # Surface the date format actually used (auto-detected or manual)
                detected_fmt <- if (is.list(calculated_times) && !is.null(calculated_times$detected_format)) {
                    calculated_times$detected_format
                } else {
                    self$options$time_format
                }
                fmt_label <- if (identical(self$options$time_format, "auto")) {
                    paste0(detected_fmt, " (auto-detected)")
                } else {
                    detected_fmt
                }

                summary_text <- glue::glue("

                    <br><b>Time Interval Summary ({self$options$output_unit}{lm_hdr_suffix})</b><br>

                    Number of observations: {summary_stats$n}<br>

                    Date format used: {fmt_label}<br>

                    Time basis: {if (self$options$time_basis == 'calendar') 'Calendar-aware (actual month lengths)' else 'Standardized (30.4375-day months, 365.25-day years)'}<br>

                    {lm_pt_label}: {round(summary_stats$total_person_time, 2)} person-{self$options$output_unit}<br>

                                    {lm_mean_label}: {round(summary_stats$mean, 2)}{ci_text}<br>

                                    Median time: {round(summary_stats$median, 2)}<br>

                                    Standard deviation: {round(summary_stats$sd, 2)}<br>

                    Range: {round(summary_stats$min, 2)} to {round(summary_stats$max, 2)}<br>

                    Missing values: {summary_stats$missing}<br>

                    Filters applied: {filter_text}<br>

                    {ci_note}

                                    <div style='background-color: rgba(33, 159, 43, 0.1); padding: 12px; margin-top: 12px; border-left: 3px solid #4caf50; color: inherit;'>

                                        <strong> Interpretation Example:</strong><br>

                                        With a {lm_fu_phrase} of {round(summary_stats$mean, 1)} {self$options$output_unit}

                                        (range: {round(summary_stats$min, 1)} to {round(summary_stats$max, 1)} {self$options$output_unit}),

                                        {if(summary_stats$mean > summary_stats$median) 'the mean sits above the median, which usually indicates a right-skewed follow-up distribution - a minority of cases followed much longer than the rest' else 'the mean sits at or below the median, which gives no indication of a right-skewed follow-up distribution'} (no skewness coefficient is computed here; compare the Median time and Range lines above).

                                        {lm_pt_phrase} ({round(summary_stats$total_person_time, 1)} person-{self$options$output_unit})

                                        serves as the denominator for calculating incidence rates

                                        (e.g., events per 100 person-{self$options$output_unit}).

                                    </div>

                                ")

                self$results$summary$setContent(summary_text)

                # Small sample size guards. n == 1 is the MOST degenerate case, not an
                # exempt one: the old "n > 1" lower bound let a single observation
                # through with no warning at all, beside a summary reporting
                # "Standard deviation: NA".
                if (summary_stats$n == 1) {
                    add_message('strong_warning', 'Only one interval could be calculated. No spread can be estimated from a single observation, so the standard deviation and the confidence interval are reported as NA, and the mean, median and range are all that one value. This is not a basis for any statistical statement.')
                } else if (summary_stats$n < 10) {
                    add_message('strong_warning', sprintf('Critically small sample (n=%d). Statistical summaries are unreliable with fewer than 10 observations. Results should be considered exploratory only. Minimum n=20 recommended for basic descriptive analysis.',
                                summary_stats$n))
                } else if (summary_stats$n < 20) {
                    add_message('warning', sprintf('Small sample size (n=%d). Confidence intervals may be very wide and unreliable with fewer than 20 observations. Consider collecting more data or interpreting results cautiously.',
                                summary_stats$n))
                }

                # Implausible-duration backstop for a misparse the serial guard
                # cannot see (mixed columns, two-digit years read as 19xx). 50 years
                # exceeds the per-patient follow-up of essentially every clinical
                # cohort and is below the ~100-year ceiling of a legitimate
                # birth-to-event interval; the measured signature of a two-digit-year
                # misparse is a ~66-year mean, which a 100-year rule would miss.
                # Expressed in years so the trigger does not move with output_unit.
                years_per_unit <- switch(self$options$output_unit,
                    days = 1 / 365.25, weeks = 7 / 365.25, months = 1 / 12,
                    years = 1, NA_real_)
                # Test the RAW longest interval, before any landmark or filter.
                # summary_stats$max is post-landmark, and subtracting a landmark
                # shortens every interval -- on the misparsed data this exists to
                # catch, a landmark would drag the max under 50 years and silently
                # switch the backstop off. Fall back to the post-landmark max only
                # if the quality object is unavailable.
                raw_max <- if (!is.null(calculated_times$quality$max_interval))
                               calculated_times$quality$max_interval else summary_stats$max
                max_years <- raw_max * years_per_unit
                if (!is.na(max_years) && is.finite(max_years) && max_years > 50) {
                    add_message('warning', sprintf(
                        'Longest interval in the data is %.1f years (%.2f %s, before any landmark or filter). Intervals beyond 50 years exceed the follow-up of essentially every clinical cohort, and usually mean the dates were parsed with the wrong format rather than observed. Check the Date Format setting and the raw date columns before reporting this person-time. If the intervals are genuinely this long (a lifetime cohort, or an age computed from date of birth), this message can be ignored.',
                        max_years, raw_max, self$options$output_unit))
                }

                # Same-day intervals contribute no person-time. The quality panel
                # already lists the count, but it is off by default, so a cohort
                # whose denominator has collapsed would otherwise be announced with
                # nothing but a green "analysis completed" banner.
                zshare <- calculated_times$quality$zero_share
                if (!is.null(zshare) && !is.na(zshare) && zshare >= 0.2) {
                    add_message('warning', sprintf(
                        '%.0f%% of intervals are zero-length (start and end date on the same day). These contribute nothing to total person-time, so the denominator here is smaller than the number of participants suggests. Check whether unfilled follow-up dates were defaulted to the start date.',
                        100 * zshare))
                }

                # Add completion info message
                if (lm_on) {
                    add_message('info', sprintf('Analysis completed using %d observations that reached the %s landmark. All reported times are measured FROM the landmark, not from the start date (%s was subtracted from every interval): mean post-landmark follow-up %.1f %s, total post-landmark person-time %.1f person-%s.',
                                summary_stats$n, lm_adj, lm_amount,
                                summary_stats$mean, lm_unit,
                                summary_stats$total_person_time, lm_unit))
                } else {
                    add_message('info', sprintf('Analysis completed using %d observations with mean follow-up %.1f %s (total person-time: %.1f person-%s).',
                                summary_stats$n, summary_stats$mean, lm_unit,
                                summary_stats$total_person_time, lm_unit))
                }

            } else {
                # No intervals survived. There are three routes here and they need
                # different advice: the landmark excluded everyone, the quality
                # filters removed everyone, or the dates genuinely did not parse.
                # Previously all three printed the date-format checklist, which sent
                # users hunting a parsing bug that did not exist -- and printed no
                # message at all, so the results pane looked merely empty.
                lm_emptied <- lm_on && isTRUE(landmark_info$original_n > 0) &&
                              isTRUE(landmark_info$final_n == 0)
                removed_n  <- (if (is.null(filter_info$removed_negative)) 0 else filter_info$removed_negative) +
                              (if (is.null(filter_info$removed_extreme))  0 else filter_info$removed_extreme)
                filters_emptied <- !lm_emptied && removed_n > 0

                if (lm_emptied) {
                    cause_html <- glue::glue(
                        "<p>All {landmark_info$original_n} participants were excluded by the landmark: none had follow-up reaching <strong>{lm_amount}</strong>.</p>",
                        "<p><strong>What to do:</strong></p>",
                        "<ul>",
                        "<li>Lower the landmark time, or switch it off, and re-read the interval range in the summary</li>",
                        "<li>Check that the landmark is expressed in the same unit as the results ({lm_unit})</li>",
                        "</ul>")
                    add_message('error', sprintf(
                        'Landmark analysis excluded every participant: none of the %d observations reached the %s landmark. Lower the landmark time or switch landmark analysis off.',
                        landmark_info$original_n, lm_amount))
                } else if (filters_emptied) {
                    cause_html <- glue::glue(
                        "<p>Every observation was removed by the data quality filters ({removed_n} in total).</p>",
                        "<p><strong>What to do:</strong></p>",
                        "<ul>",
                        "<li>Switch off the quality filters under Data Quality &amp; Statistics and inspect the raw intervals first</li>",
                        "<li>If every interval was negative, the start and end date columns are probably swapped</li>",
                        "</ul>")
                    add_message('error', sprintf(
                        'All observations were removed by the data quality filters (%d rows). Switch the filters off to inspect the raw intervals; if every interval was negative, the start and end date columns are probably swapped.',
                        removed_n))
                } else {
                    cause_html <- paste0(
                        "<p>No valid time intervals could be calculated from the provided data.</p>",
                        "<p><strong>Please check:</strong></p>",
                        "<ul>",
                        "<li>Date format settings match your data</li>",
                        "<li>Date columns contain valid dates</li>",
                        "<li>End dates occur after start dates</li>",
                        "<li>Data contains non-missing values</li>",
                        "</ul>")
                    add_message('error', 'No valid time intervals could be calculated. Check that the date format setting matches the data and that both date columns contain readable dates.')
                }

                error_summary <- paste0(
                    "<div style='background-color: rgba(216, 33, 50, 0.18); padding: 15px; border-left: 4px solid #dc3545; margin: 15px 0; color: inherit;'>",
                    "<h4 style='margin-top: 0; color: inherit;'> No Valid Time Intervals</h4>",
                    cause_html,
                    "</div>")

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
                    lm <- calculated_times$landmark
                    below_n <- if (!is.null(lm$below_excluded)) lm$below_excluded else lm$excluded_count
                    na_n <- if (!is.null(lm$na_excluded)) lm$na_excluded else 0
                    excl_parts <- c()
                    if (below_n > 0)
                        # "&lt;" not "<": this string is interpolated into an HTML
                        # panel, where a bare "<" is invalid markup and breaks the
                        # XML-based Word/PDF export paths even though browsers
                        # tolerate it.
                        excl_parts <- c(excl_parts, sprintf("%d with follow-up &lt; %s",
                                        below_n, lm_amount))
                    if (na_n > 0)
                        excl_parts <- c(excl_parts, sprintf("%d with missing follow-up", na_n))
                    if (length(excl_parts) > 0)
                        sprintf(" after excluding %s (landmark analysis)",
                                paste(excl_parts, collapse = " and "))
                    else
                        ""
                } else {
                    ""
                }

                quality_text <- if (summary_stats$missing > 0) {
                    sprintf(" Note: %d missing values were detected.", summary_stats$missing)
                } else {
                    ""
                }

                # A landmark re-zeroes the clock; the pasted sentence must state it.
                # It asserts only the follow-up-time criterion -- this analysis has no
                # event indicator, so it cannot claim participants were event-free.
                lm_clause <- if (lm_on)
                    glue::glue(" Follow-up time was measured from the {lm_adj} landmark rather than from the start date.")
                else ""
                copy_sentence <- if (lm_on)
                    glue::glue("\"A landmark analysis was performed at {lm_amount}: {n_obs} participants with at least {lm_amount} of follow-up were included, and follow-up time was measured from the landmark rather than from the start date. Post-landmark follow-up was a mean of {mean_time} {unit} (median {median_time} {unit}), contributing {total_pt} post-landmark person-{unit} of observation time.\"")
                else
                    glue::glue("\"Follow-up data were available for {n_obs} participants (mean {mean_time} {unit}, median {median_time} {unit}), contributing {total_pt} person-{unit} of observation time.\"")

                summary_html <- glue::glue("
                    <div style='background-color: rgba(33, 149, 188, 0.1); padding: 20px; border-left: 5px solid #0066cc; margin: 15px 0; color: inherit;'>
                        <h3 style='margin-top: 0; color: inherit;'> Clinical Summary</h3>
                        <p style='font-size: 1.1em; line-height: 1.6;'>
                        <strong>Time interval analysis</strong> was performed on <strong>{n_obs} participants</strong>{landmark_text}.{lm_clause}
                        The {lm_fu_phrase} was <strong>{mean_time} {unit}</strong> (median: {median_time} {unit}),
                        contributing a total of <strong>{total_pt} {if (lm_on) 'post-landmark ' else ''}person-{unit}</strong> of observation.{quality_text}
                        </p>

                        <div style='background-color: rgba(33, 137, 255, 0.07); padding: 15px; margin-top: 15px; border-radius: 5px; color: inherit;'>
                            <p style='font-size: 0.95em; color: inherit; margin: 0;'>
                            <strong> Copy-Ready Sentence:</strong><br>
                            <em style='color: inherit;'>{copy_sentence}</em>
                            </p>
                        </div>
                    </div>
                ")

                self$results$nlSummary$setContent(summary_html)
            }

            # Populate Glossary if requested
            if (self$options$show_glossary) {
                glossary_html <- "
                    <div style='background-color: rgba(153, 33, 170, 0.12); padding: 15px; border-left: 4px solid #9c27b0; margin: 15px 0; color: inherit;'>
                        <h4 style='margin-top: 0; color: inherit;'> Key Terms Explained</h4>

                        <dl style='margin: 5px 0;'>
                            <dt style='font-weight: bold; margin-top: 10px;'>Person-Time</dt>
                            <dd style='margin-left: 20px;'>Total observation duration across all participants.
                            Example: 100 people followed for 2 years = 200 person-years. This accounts for varying follow-up periods.</dd>

                            <dt style='font-weight: bold; margin-top: 10px;'>Incidence Rate</dt>
                            <dd style='margin-left: 20px;'>Number of new events \u00f7 person-time.
                            Example: 10 deaths \u00f7 200 person-years = 0.05 deaths per person-year (or 5 per 100 person-years).</dd>

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
                            <dd style='margin-left: 20px;'>Range of mean follow-up values compatible with these data.
                            Example: Mean = 12 months (95% CI: 10-14) means values from 10 to 14 are compatible with these data; over repeated studies 95% of such intervals contain the true mean.</dd>
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
                    "<div style='background-color: rgba(138, 155, 172, 0.06); padding: 15px; border-left: 4px solid #007bff; margin: 15px 0; color: inherit;'>",
                        "<h4 style='margin-top: 0; color: inherit;'> Data Quality Assessment</h4>",

                        "<p><strong>Overall Quality:</strong> {quality$overall_quality}</p>",

                        # The counts below are computed on the RAW input, before any
                        # landmark or quality filter, so "Total Observations" here
                        # will not match "Number of observations" in the summary
                        # whenever a filter is active. Say so rather than leaving two
                        # different Ns side by side unexplained.
                        "<p style='font-size: 0.9em;'>Counts below describe the data as supplied, before any filter or landmark is applied, so the total may exceed the number of observations in the statistical summary.</p>",

                        "<table style='width: 100%; border-collapse: collapse; margin-top: 10px;'>",
                            "<tr style='background-color: rgba(33, 63, 94, 0.1); color: inherit;'>",
                                "<th style='padding: 8px; text-align: left; border: 1px solid #dee2e6;'>Metric</th>",
                                "<th style='padding: 8px; text-align: right; border: 1px solid #dee2e6;'>Count</th>",
                                "<th style='padding: 8px; text-align: right; border: 1px solid #dee2e6;'>%</th>",
                            "</tr>",
                            "<tr>",
                                "<td style='padding: 8px; border: 1px solid #dee2e6;'>Total Observations</td>",
                                "<td style='padding: 8px; text-align: right; border: 1px solid #dee2e6;'>{quality$total_observations}</td>",
                                "<td style='padding: 8px; text-align: right; border: 1px solid #dee2e6;'>100%</td>",
                            "</tr>",
                            "<tr style='background-color: rgba(255, 202, 33, 0.23); color: inherit;'>",
                                "<td style='padding: 8px; border: 1px solid #dee2e6;'>Missing Values</td>",
                                "<td style='padding: 8px; text-align: right; border: 1px solid #dee2e6;'>{quality$missing_values}</td>",
                                "<td style='padding: 8px; text-align: right; border: 1px solid #dee2e6;'>{round(100*quality$missing_values/quality$total_observations, 1)}%</td>",
                            "</tr>",
                            "<tr style='background-color: rgba(216, 33, 50, 0.18); color: inherit;'>",
                                "<td style='padding: 8px; border: 1px solid #dee2e6;'>Negative Intervals</td>",
                                "<td style='padding: 8px; text-align: right; border: 1px solid #dee2e6;'>{quality$negative_intervals}</td>",
                                "<td style='padding: 8px; text-align: right; border: 1px solid #dee2e6;'>{round(100*quality$negative_intervals/quality$total_observations, 1)}%</td>",
                            "</tr>",
                            "<tr>",
                                "<td style='padding: 8px; border: 1px solid #dee2e6;'>Zero Intervals</td>",
                                "<td style='padding: 8px; text-align: right; border: 1px solid #dee2e6;'>{quality$zero_intervals}</td>",
                                "<td style='padding: 8px; text-align: right; border: 1px solid #dee2e6;'>{round(100*quality$zero_intervals/quality$total_observations, 1)}%</td>",
                            "</tr>",
                            "<tr style='background-color: rgba(255, 202, 33, 0.23); color: inherit;'>",
                                "<td style='padding: 8px; border: 1px solid #dee2e6;'>Extreme Values</td>",
                                "<td style='padding: 8px; text-align: right; border: 1px solid #dee2e6;'>{quality$extreme_values}</td>",
                                "<td style='padding: 8px; text-align: right; border: 1px solid #dee2e6;'>{round(100*quality$extreme_values/quality$total_observations, 1)}%</td>",
                            "</tr>",
                        "</table>",

                        "{if (length(filter_lines) > 0) paste0('<p><strong>Filters applied:</strong> ', filter_text, '</p>') else ''}",

                        "{if(length(quality$warnings) > 0) paste0('<p style=\"margin-top: 15px;\"><strong> Warnings:</strong></p><ul>', paste0('<li>', quality$warnings, '</li>', collapse=''), '</ul>') else ''}",
                    "</div>"
                )

                self$results$qualityAssessment$setContent(quality_html)

                # Populate Caveats panel (only when quality metrics enabled).
                # glue(), not a plain string: the time-basis bullet has to describe
                # the basis this run actually used, and the extreme-value bullet has
                # to quote the user's multiplier rather than a hardcoded 2x.
                basis_caveat <- if (identical(self$options$time_basis, "calendar"))
                    "This run used the <strong>calendar-aware</strong> basis, so months and years follow actual month lengths (28-31 days). Switch to the standardized basis (1 month = 30.4375 days, 1 year = 365.25 days) if you need a constant unit of risk exposure for person-time denominators."
                else
                    "To ensure statistical consistency for survival analysis, this run used <strong>standardized durations</strong> (1 month = 30.4375 days, 1 year = 365.25 days) rather than calendar units. This prevents bias from varying month lengths (28-31 days)."
                caveats_html <- glue::glue("
                    <div style='background-color: rgba(255, 203, 33, 0.14); padding: 15px; border-left: 4px solid #ff9800; margin: 15px 0; color: inherit;'>
                        <h4 style='margin-top: 0; color: inherit;'> Important Assumptions</h4>
                        <ul style='margin: 5px 0;'>
                            <li><strong>Time Units (Months/Years):</strong> {basis_caveat}</li>
                            <li><strong>End dates should occur on or after start dates</strong> - Negative intervals usually indicate data entry errors</li>
                            <li><strong>Date formats must be consistent</strong> - All dates in a column should use the same format</li>
                            <li><strong>Landmark analysis excludes participants</strong> - Only those with follow-up \u2265 landmark time are included; participants with missing follow-up are also excluded, because their eligibility cannot be determined</li>
                            <li><strong>Landmark analysis re-zeroes the clock</strong> - When a landmark is active, the landmark is subtracted from every interval, so every duration reported here (mean, median, SD, range, total person-time) is time measured <em>from the landmark</em>, not from the start date. A participant with 12 months of follow-up and a 6-month landmark contributes 6 months, and the person-time shown is post-landmark person-time. Add the landmark to each value to recover time from the start date</li>
                            <li><strong>Missing dates produce missing intervals</strong> - These are excluded from summary statistics</li>
                        </ul>

                        <h4 style='color: inherit;'> Common Pitfalls</h4>
                        <ul style='margin: 5px 0;'>
                            <li><strong>Mixed date formats:</strong> DD/MM/YYYY vs MM/DD/YYYY in same column \u2192 Use manual format selection</li>
                            <li><strong>Text vs numeric dates:</strong> Ensure dates are stored consistently (all text or all numeric)</li>
                            <li><strong>Future dates:</strong> End dates after today's date may indicate data errors</li>
                            <li><strong>Extreme outliers:</strong> Very long intervals may be real (long follow-up) or errors</li>
                        </ul>

                        <h4 style='color: inherit;'> Troubleshooting</h4>
                        <ul style='margin: 5px 0;'>
                            <li>If auto-detection fails, manually select your date format</li>
                            <li>Check for negative intervals - these indicate date column errors</li>
                            <li>Review extreme values - anything above {self$options$extreme_multiplier}\u00d7 the 99th percentile is counted as extreme</li>
                            <li>Ensure date columns don't contain non-date values (text, codes, etc.)</li>
                        </ul>
                    </div>
                ")
                self$results$caveatsPanel$setContent(caveats_html)
            }

            # Output all collected messages, most severe first. Called
            # unconditionally: with no messages it blanks the element, so a banner
            # from a previous run cannot survive into a clean one.
            render_messages()
        }
    )
)
