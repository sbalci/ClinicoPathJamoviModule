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

        # Holds the mixed-day/month-order finding (list) when some rows appear to
        # have been entered in the opposite order from the rest of the column.
        .mixedOrderNote = NULL,

        # Always-recorded day/month ambiguity exposure (list): how many rows
        # could have been typed either way, and how much person-time they carry.
        # Recorded even when the mixed-order test does NOT fire, because the
        # residual undetectable case is real and must at least be disclosed.
        .ambiguousExposure = NULL,

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
                    error = .("Data frame is empty; ensure your dataset has at least one row.")
                ))
            }

            # Check for completely missing date columns
            if (all(is.na(data[[dx_date]]))) {
                return(list(
                    valid = FALSE,
                    error = .("Start date column contains only missing values; cannot calculate time intervals.")
                ))
            }

            if (all(is.na(data[[fu_date]]))) {
                return(list(
                    valid = FALSE,
                    error = .("End date column contains only missing values; cannot calculate time intervals.")
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
            v <- utils::head(date_vector[!is.na(date_vector)], 50)  # as .detectDateFormat()
            if (length(v) == 0)
                return(invisible(NULL))

            # A NUMERIC column is tested numerically; a text or factor column is
            # tested on the STRING, never by coercion. That distinction is the whole
            # safety argument. as.numeric() turns every genuine text date into NA, so
            # a vote taken over the coercible cells alone would let one stray numeric
            # cell -- an SPSS/Excel missing code such as "99999", or a typo -- condemn
            # the entire column. A regex keeps every real date in the denominator:
            # "2016-01-01" and "15/01/2016" simply fail to match, so the stray cell
            # scores 1/50 and the guard stays silent.
            #
            # Widening to text is not merely safe, it is unambiguously safe in a way
            # the numeric case is not. The YMMDD-vs-serial ambiguity that forces the
            # numeric branch to refuse rather than guess does not exist here: no
            # 5-digit STRING parses under any lubridate order (ymd("20115") is NA
            # where ymd(20115) is 2002-01-15), so there is no competing reading to
            # clobber. And no legitimate clinical date column is all bare 5-digit
            # integers -- YYMMDD/MMDDYY/DDMMYY are six characters and YYYYMMDD is
            # eight; the only real 5-digit encodings are day counts. Before this, a
            # serial column re-exported as text fell through to two dead-end messages
            # ("Could not detect a common date format", "Date parsing failed ...
            # Example values: 42370"), both of which told the user to choose a format
            # manually when no format on the list can parse "42370".
            serial <- if (is.numeric(v))
                          v >= 10000 & v < 100000 & v == trunc(v)
                      else
                          grepl("^[0-9]{5}$", trimws(as.character(v)))
            if (mean(serial) < 0.8)
                return(invisible(NULL))

            example <- as.numeric(as.character(v[serial][1]))
            # Each .() wraps one complete sentence; the newlines that lay the message
            # out are joined OUTSIDE the translated strings, so no translatable unit
            # contains a line break.
            #
            # The example date names its epoch, and says the others exist, because
            # the epoch is NOT knowable from the number. Spreadsheets count from
            # 1899-12-30, SAS and Stata from 1960-01-01, R from 1970-01-01 -- and all
            # three land inside this same five-digit band, so quoting one reading as
            # if it were the reading can be wrong by decades (a value of 20696 is
            # 1956-08-29 on the spreadsheet epoch and 2026-08-31 on R's). Stating one
            # date unqualified is exactly the sort of confident-and-wrong detail a
            # reader fixates on and then dismisses the whole message over.
            hint <- if (example >= 10000 && example <= 88907)
                        .fmt(.("Counted from the spreadsheet epoch, {value} would be {date}; SAS and Stata count from 1960-01-01 and R from 1970-01-01, so the same digits mean a date decades apart depending on which program wrote them."),
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

        # One place that maps a format name to its lubridate parser, so the
        # detector, the tie-break and the mixed-order check cannot drift apart.
        .tryParse = function(x, fmt) {
            parser <- switch(fmt,
                "ymdhms" = lubridate::ymd_hms,
                "ymd" = lubridate::ymd,
                "ydm" = lubridate::ydm,
                "mdy" = lubridate::mdy,
                "myd" = lubridate::myd,
                "dmy" = lubridate::dmy,
                "dym" = lubridate::dym,
                NULL)
            if (is.null(parser) || length(x) == 0)
                return(rep(as.POSIXct(NA), max(length(x), 0)))
            suppressWarnings(tryCatch(parser(x, quiet = TRUE),
                                      error = function(e) rep(as.POSIXct(NA), length(x))))
        },

        # Detect rows entered in the OPPOSITE day/month order from the rest of the
        # column. This is the single commonest real-world date defect -- a registry
        # column typed DD/MM/YYYY in which a handful of rows were entered
        # MM/DD/YYYY -- and before this check it was invisible to every guard here:
        # the bad rows parse SUCCESSFULLY under the dominant order, so they are
        # never NA; their swapped dates still give positive intervals, so the
        # negative-interval filter never sees them; they sit inside 2 x the 99th
        # percentile; and they are far under the 50-year backstop. Measured on a
        # 20-row cohort with 5 such rows: person-time reported 40.9% high, quality
        # panel "Good", the only banner a green "Analysis completed", and a
        # copy-ready manuscript sentence containing the wrong number.
        #
        # WHY NOT SIMPLY WARN ON AMBIGUITY. A row is order-ambiguous when both its
        # components are <= 12, so it reads as a valid date under either order. In a
        # perfectly CLEAN dd/mm column roughly 12/31 = 39% of rows are ambiguous --
        # MORE than the 25% in the broken fixture above. A bare ambiguity percentage
        # therefore cannot separate the two, and a banner that fires on almost every
        # correct dataset just teaches users to dismiss banners.
        #
        # WHAT ACTUALLY SEPARATES THEM. Day-of-month is unrelated to length of
        # follow-up, so in a clean column the ambiguous rows are a RANDOM SUBSET of
        # the rest and the two groups' intervals are exchangeable. Rows typed in
        # the other order break that exchangeability, because their intervals are
        # computed from the wrong dates. So the test is at GROUP level: a
        # rank-sum test of the ambiguous rows' intervals against the unambiguous
        # rows', which are the ones that prove the column's order.
        #
        # A per-row "is this row an outlier, and does swapping fix it" test was
        # tried first and is wrong: a row typed in the other order does not
        # generally become typical when swapped -- its true follow-up may itself be
        # atypical -- so that test misses the very fixture it was built for.
        #
        # NOT CRYING WOLF. alpha is 0.001, not 0.05, so a clean dataset trips this
        # about one time in a thousand rather than one in twenty; a material effect
        # size is required as well, so a trivial difference in a large cohort stays
        # quiet; and both groups need a real size before the test runs at all.
        .checkMixedDateOrder = function(start_raw, end_raw, fmt, start_dates, end_dates) {
            private$.mixedOrderNote <- NULL
            private$.ambiguousExposure <- NULL
            # Only day/month orders can be confused this way. ymd/ydm text is
            # unambiguous, and packed numeric dates have no separator to swap.
            swap <- switch(fmt, dmy = "mdy", mdy = "dmy", NULL)
            if (is.null(swap)) return(invisible(NULL))
            if (!is.character(start_raw)) start_raw <- as.character(start_raw)
            if (!is.character(end_raw))   end_raw   <- as.character(end_raw)

            sw_start <- private$.tryParse(start_raw, swap)
            sw_end   <- private$.tryParse(end_raw, swap)

            usable <- !is.na(start_dates) & !is.na(end_dates)
            # A row is ambiguous when BOTH its dates also parse under the swapped
            # order AND at least one of them lands on a different day.
            amb <- usable & !is.na(sw_start) & !is.na(sw_end) &
                   (as.Date(sw_start) != as.Date(start_dates) |
                    as.Date(sw_end)   != as.Date(end_dates))
            ref <- usable & !amb                      # rows that prove the order
            days <- function(a, b) as.numeric(difftime(b, a, units = "days"))
            iv_chosen  <- days(start_dates, end_dates)
            iv_swapped <- days(sw_start, sw_end)

            # ALWAYS record the exposure, whether or not the test below fires.
            # Measured on 200 simulated cohorts with 15% of rows typed in the other
            # order: 13% were rejected outright for negative intervals, 82% already
            # tripped the missing-data warning (a mis-typed row whose day exceeds 12
            # cannot be parsed at all and becomes NA), and 4.5% ran silently with
            # person-time wrong by up to 13%. Those last are rows where BOTH dates
            # happen to have day <= 12, so they parse cleanly -- one or two rows in
            # fifty, which no statistical test can distinguish from chance. That
            # residue is not detectable, so it is disclosed instead of guessed at:
            # the quality panel states how many rows could have been typed either
            # way and how much person-time rides on them.
            if (sum(amb) > 0 && sum(usable) > 0) {
                pt_all <- sum(iv_chosen[usable], na.rm = TRUE)
                private$.ambiguousExposure <- list(
                    n = sum(amb), total = sum(usable), proven_by = sum(ref),
                    fmt = fmt, swap = swap,
                    pt_share = if (is.finite(pt_all) && pt_all > 0)
                                   100 * sum(iv_chosen[amb], na.rm = TRUE) / pt_all else NA_real_)
            }

            # Both groups must be big enough for the rank-sum test to mean
            # anything, and the reference group must actually establish the order.
            if (sum(ref) < 8 || sum(amb) < 4) return(invisible(NULL))

            a_iv <- iv_chosen[amb]; r_iv <- iv_chosen[ref]
            a_iv <- a_iv[is.finite(a_iv)]; r_iv <- r_iv[is.finite(r_iv)]
            if (length(a_iv) < 4 || length(r_iv) < 8) return(invisible(NULL))

            p <- tryCatch(
                suppressWarnings(stats::wilcox.test(a_iv, r_iv, exact = FALSE)$p.value),
                error = function(e) NA_real_)
            if (!is.finite(p) || p >= 0.001) return(invisible(NULL))

            # Effect size as well as significance: in a large cohort a trivial
            # difference can clear any p threshold, and a trivial difference is not
            # a mis-typed date.
            m_a <- stats::median(a_iv); m_r <- stats::median(r_iv)
            if (!is.finite(m_a) || !is.finite(m_r) || m_r <= 0) return(invisible(NULL))
            ratio <- m_a / m_r
            if (ratio > 0.67 && ratio < 1.5) return(invisible(NULL))

            # Name the individual rows that move most when swapped -- "check rows
            # 16, 17, 18" is actionable in a way "check your dates" is not.
            shift <- abs(iv_chosen - iv_swapped)
            shift[!amb | !is.finite(shift)] <- -Inf
            worst <- utils::head(order(shift, decreasing = TRUE), min(5L, sum(amb)))

            private$.mixedOrderNote <- list(
                n = sum(amb), total = sum(usable), rows = sort(worst),
                fmt = fmt, swap = swap, p = p,
                median_amb = m_a, median_ref = m_r,
                example_chosen = iv_chosen[worst[1]], example_swapped = iv_swapped[worst[1]])
            invisible(NULL)
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

            # Two or more formats can parse the data equally well.
            tied_formats <- names(format_scores)[
                format_scores >= (best_score - 1e-9) & format_scores >= 0.5]

            # TIE-BREAK ON YEAR SPAN, not on the order of formats_to_try.
            #
            # The old rule was "first candidate in formats_to_try order wins", and
            # ymd is first, so ymd won every tie. For any DD/MM/YY column with
            # years <= 2031 that is ALWAYS wrong and ALWAYS a tie: ymd reads the
            # DAY as the year, so "22/09/18" becomes 2022-09-18 instead of
            # 2018-09-22. Measured on a 30-row cohort, ymd parsed 30/30 -- tying
            # dmy at 1.00 -- and produced a 7.7x wrong mean follow-up.
            #
            # The discriminator is that a clinical cohort spans a few years, not
            # thirty. Reading the day-of-month as a year scatters the years across
            # 1..31, so the wrong format's parsed years span ~25-30 years while the
            # right one spans the cohort's true accrual window. Measured on the same
            # data: ymd span 25 years, dmy span 0. Genuine ISO text never reaches
            # this branch (dmy cannot parse "2020-03-05" at all), and a true
            # dmy/mdy ambiguity has an identical year component in both readings,
            # so the span is equal and the original order still decides -- that
            # case is handled downstream by .checkMixedDateOrder() instead.
            if (length(tied_formats) > 1) {
                year_span <- vapply(tied_formats, function(fmt) {
                    p <- private$.tryParse(c(sample_start, sample_end), fmt)
                    y <- lubridate::year(p[!is.na(p)])
                    if (length(y) == 0) NA_real_ else as.numeric(diff(range(y)))
                }, numeric(1))
                if (any(is.finite(year_span))) {
                    # Strictly smaller only, so an exact tie keeps the previous
                    # (formats_to_try order) winner and nothing silently reshuffles.
                    narrowest <- tied_formats[which.min(year_span)]
                    if (is.finite(year_span[[narrowest]]) &&
                        (!is.finite(year_span[[best_format]]) ||
                         year_span[[narrowest]] < year_span[[best_format]])) {
                        best_format <- narrowest
                    }
                }
            }

            # Flag the ambiguity regardless of how it was resolved: the user should
            # still verify the choice rather than trust a heuristic silently.
            if (best_score >= 0.5 && length(tied_formats) > 1) {
                private$.formatDetectionNote <- .fmt(
                    .("Auto-detection is ambiguous: {formats} all parse these dates equally well ({pct}% success). The '{format}' format was used, because it places these dates in the narrowest range of years - please verify it matches your data or select the format manually."),
                    formats = paste(tied_formats, collapse = ", "),
                    pct = base::formatC(100 * best_score, format = "f", digits = 0),
                    format = best_format)
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
                # Median of the SAME raw vector, for the landmark sanity checks in
                # .run(). It has to be the pre-landmark median: the landmark
                # subtracts itself from every retained interval, so a post-landmark
                # median cannot say whether the landmark is late or early relative
                # to the follow-up actually observed.
                median_interval = if (non_missing > 0) stats::median(calculated_time, na.rm = TRUE) else NA_real_,
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

            # Rows entered in the opposite day/month order parse cleanly and stay
            # positive, so nothing downstream can catch them. Check here, on the
            # raw strings, while both readings are still recoverable.
            private$.checkMixedDateOrder(data[[dx_date]], data[[fu_date]],
                                         detected_format, start_dates, end_dates)

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
                    .("A third cause makes every row negative by less than a day: a start column carrying a time of day against an end column recorded at midnight, which is what many laboratory systems export - check the Examples below for a time component on one side only."),
                    .("Correct the dates at source, or tick 'Remove negative intervals' under Data Quality & Statistics to drop these rows from every statistic including person-time."),
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
            private$.mixedOrderNote <- NULL
            private$.ambiguousExposure <- NULL

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
                # The severity label is user-visible, so it has to be translatable.
                # A switch() on the severity KEY is the only way to get a literal
                # the catalogue can hold: tools::toTitleCase() derives its output
                # from the already-built English word, and a derived string has no
                # msgid. The fallback branch keeps the old derivation for any
                # severity not in the list above, so behaviour is unchanged there.
                label <- switch(type,
                    "error" = .("Error"),
                    "strong_warning" = .("Strong Warning"),
                    "warning" = .("Warning"),
                    "info" = .("Info"),
                    tools::toTitleCase(gsub("_", " ", type)))
                msg_env$messages <- c(msg_env$messages, list(list(
                    rank = rank,
                    html = sprintf(
                        "<div style='background-color: %s; padding: 12px; border-left: 4px solid %s; margin: 10px 0; color: %s;'>
                        <strong>%s %s:</strong> %s
                    </div>",
                        color$bg, color$border, color$text, color$icon,
                        label,
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
                add_message('strong_warning', .fmt(
                    .("The same variable ({variable}) is selected as both the start and the end date, so every interval is exactly zero and the total person-time is zero. Select the follow-up or event date as the End Date Variable."),
                    variable = self$options$dx_date))
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
                # Raise the ambiguity note HERE too, not only on the success path
                # below: .detectDateFormat() runs BEFORE any rejection, so on a
                # failed run the single most diagnostic sentence we have ("ymd and
                # dmy both fit these dates equally well; I used ymd") was computed
                # and then thrown away by the early return under this handler --
                # exactly when the user is trying to work out what went wrong.
                # render_messages() sorts by severity, so the error still shows first.
                if (identical(self$options$time_format, "auto") &&
                    !is.null(private$.formatDetectionNote))
                    add_message('warning', private$.formatDetectionNote)
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

            # Rows that look like they were typed in the other day/month order.
            # Strong warning, not an error: this is strong evidence, not proof,
            # and the user is the one who can check the named rows against the
            # source records. It names rows because "check your dates" is not
            # actionable and "check rows 16, 17, 18" is.
            mox <- private$.mixedOrderNote
            if (!is.null(mox)) {
                add_message('strong_warning', .fmt(
                    .("Some rows may have been entered with the day and month the other way round from the rest of this column. {count} of {total} rows read as a valid date under both \"{format}\" and \"{swapped}\"; the other {others} can only be read as \"{format}\", which is how the format was chosen. Those two groups should have the same follow-up on average, because the day of the month has nothing to do with how long a patient is followed - but here the ambiguous rows run at a median of {medianamb} days against {medianref} days for the rest (rank-sum p = {p}), which is what a mis-typed day/month order looks like. Check rows {rows} first: the largest of them spans {examplechosen} days as read and {exampleswapped} days with the day and month swapped. Dates typed in two different orders in one column parse without error and stay positive, so nothing else on this page can see them, and the total person-time is wrong by however much those rows are wrong."),
                    count = mox$n, total = mox$total, format = mox$fmt, swapped = mox$swap,
                    others = mox$total - mox$n,
                    medianamb = base::formatC(mox$median_amb, format = "f", digits = 0),
                    medianref = base::formatC(mox$median_ref, format = "f", digits = 0),
                    p = base::formatC(mox$p, format = "g", digits = 4),
                    rows = paste(mox$rows, collapse = ", "),
                    examplechosen = base::formatC(mox$example_chosen, format = "f", digits = 0),
                    exampleswapped = base::formatC(mox$example_swapped, format = "f", digits = 0)))
            }

            # Timezone reaches only a parser with a time component; every date-only
            # format yields a Date, which carries no zone. Tested against the format
            # ACTUALLY used, because under "auto" the detector may pick ymdhms and
            # the setting would then be live.
            if (identical(self$options$timezone, "utc") &&
                !identical(calculated_times$detected_format, "ymdhms")) {
                add_message('info', .fmt(
                    .("The UTC timezone setting applies only to the \"YYYY-MM-DD HH:MM:SS\" format, the only one carrying a time of day. These dates were read as \"{format}\", so they are calendar days and the timezone has no effect on the intervals."),
                    format = calculated_times$detected_format))
            }

            # Add calculated times to the dataset.
            # Gated on isNotFilled() ALONE, deliberately -- do NOT add
            # `self$options$calculated_time &&`. An Output option is not an argument
            # of the generated R wrapper, so from the R API it is permanently FALSE
            # and gating on it would make the column unreachable headless. In jamovi
            # the delivery is already gated for us: Output$asProtoBuf() emits nothing
            # unless Output$enabled, which reads the option of the same name. Same
            # idiom as decisioncombine.b.R, ctdnadynamics.b.R and categorize.b.R.
            if (self$results$calculated_time$isNotFilled() && !is.null(calculated_times)) {
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
                    # Put the unit in the appended column's name. The static
                    # varTitle in the .r.yaml cannot say whether the column holds
                    # days, months or years -- and an unlabelled time column landing
                    # in a survival dataset is exactly how unit mix-ups happen.
                    # varTitle interpolation is not an option here: jmvcore::format's
                    # placeholder regex excludes underscores, so `${ output_unit }`
                    # would ship literally. setTitle() writes the column title that
                    # asProtoBuf() sends, and .run() is after the varTitle was applied.
                    # The landmark belongs in the column NAME, not only in the
                    # Caveats panel. With a landmark active every written value is
                    # the landmark shorter than the interval the name otherwise
                    # promises, on a silently reduced cohort -- and this column is
                    # advertised for downstream survival analysis, where feeding a
                    # rebased time variable alongside an unrebased event indicator
                    # is a real and invisible error. The Caveats panel does explain
                    # it, but it is gated on include_quality_metrics, which is off
                    # by default, so by default nothing on screen said so.
                    lm_written <- if (!is.null(calculated_times$landmark))
                                      calculated_times$landmark$landmark_time else 0
                    self$results$calculated_time$setTitle(
                        if (isTRUE(lm_written > 0))
                            sprintf("Calculated Time (%s, from %s %s landmark)",
                                    self$options$output_unit,
                                    base::format(round(lm_written, 4)),
                                    self$options$output_unit)
                        else
                            sprintf("Calculated Time (%s)", self$options$output_unit))
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
                    <li><b>Censoring:</b> Person-time counts observed follow-up whether or not the event occurred. This analysis has no event indicator, so it cannot separate an event date from a censoring date - supply the event status to a survival analysis for that</li>
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
                # A negative interval is not an unusual value, it is an impossible
                # one, and these rows are being deleted from the person-time
                # denominator. The count used to reach the user only as one line of
                # small print, while the far softer extreme-value filter got a banner
                # of its own -- exactly the wrong way round. Fires at ANY count: with
                # remove_negative OFF a single negative row is a hard rejection, so
                # with it ON the same row cannot be silent. Escalates at 10%, where
                # the diagnosis changes from sporadic entry error to a systematic
                # fault that also puts the RETAINED rows in doubt.
                # nrow(self$data) is the right denominator: removed_negative was
                # counted on that same raw vector, before any other filter.
                neg_total <- nrow(self$data)
                neg_pct   <- 100 * filter_info$removed_negative / neg_total
                neg_systematic <- isTRUE(neg_pct >= 10)
                # Naming the format actually used turns "check the Date Format
                # setting" into a one-second check, and a mis-detected day/month
                # order is the leading cause of a handful of negatives.
                neg_fmt <- if (!is.null(calculated_times$detected_format))
                               calculated_times$detected_format else self$options$time_format
                # Two sentences, two catalogue entries: an `if` inside a .() string
                # would bake an English-only branch into the msgid. paste() with the
                # default single-space separator reproduces the leading space the old
                # trailing "%s" clause carried.
                neg_msg <- .fmt(
                    .("Removing negative intervals dropped {count} of {total} rows ({pct}%) whose end date fell before their start date. A negative interval is not an unusual measurement but an impossible one: at least one of the two dates in each of those rows is wrong. Those rows are dropped from the mean, the median and the total person-time, so the person-time denominator reported here covers fewer participants than you supplied. These dates were read with the \"{format}\" format; the usual causes are the start and end date columns being swapped for those rows, or a mis-detected day/month order, which flips only the rows whose day-of-month is 12 or less. Correct the dates at source rather than leaving them to this filter before reporting this person-time."),
                    count = filter_info$removed_negative, total = neg_total,
                    pct = base::formatC(neg_pct, format = "f", digits = 1),
                    format = neg_fmt)
                if (neg_systematic)
                    neg_msg <- paste(neg_msg,
                        .("At this share the fault is systematic rather than sporadic: the rows that were kept came from the same two columns and the same parsing, so they cannot be assumed correct merely because their interval came out positive - check the date columns and the Date Format setting before using any number on this page."))
                add_message(if (neg_systematic) 'strong_warning' else 'warning', neg_msg)
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
                add_message('warning', .fmt(
                    .("Extreme-value removal dropped {count} of the longest interval(s) (above {threshold} {unit}) from the analysis. These rows are excluded from the mean, the median and the total person-time, so the person-time denominator here is smaller than the follow-up actually observed. Long follow-up is not automatically an error: check the removed rows before using this person-time for an incidence rate."),
                    count = filter_info$removed_extreme, threshold = threshold_txt,
                    unit = self$options$output_unit))
            }
            if (self$options$use_landmark && !is.null(landmark_info$excluded_count) && landmark_info$excluded_count > 0) {
                # Two different facts hide behind one count: follow-up shorter than
                # the landmark, and no interval at all. .applyLandmarkAnalysis()
                # already keeps them apart (its own comment says so); only this line
                # used to throw the split away, so a cohort with missing dates saw
                # them silently absorbed into "excluded by landmark".
                #
                # "shorter than", not "<": the same sentence is rendered on three
                # surfaces with two different escaping rules -- this one goes to
                # setContent() raw (a bare "<" is invalid markup and breaks the
                # XML-based Word/PDF export), while add_message() below htmlEscapes
                # its content (where "&lt;" would show up literally as "&amp;lt;").
                # One plain-English phrase is correct on both and lets the panels be
                # read side by side. paste0, not glue: filter_text is interpolated
                # into a glue::glue() block and must carry no braces.
                lm_below <- if (!is.null(landmark_info$below_excluded)) landmark_info$below_excluded else 0
                lm_na    <- if (!is.null(landmark_info$na_excluded)) landmark_info$na_excluded else 0
                lm_why   <- c(if (lm_below > 0) paste0(lm_below, " with follow-up shorter than ", lm_amount),
                              if (lm_na > 0) paste0(lm_na, " with missing follow-up"))
                filter_lines <- c(filter_lines, paste0(
                    landmark_info$excluded_count, " participant(s) excluded by landmark (", lm_amount, ")",
                    if (length(lm_why) > 0) paste0(": ", paste(lm_why, collapse = " and ")) else ""))

                # A landmark excludes early cases BY DESIGN, so attrition alone is
                # not a defect and a bare percentage threshold would fire on routine
                # practice -- a 12-month landmark, the commonest choice in oncology,
                # excludes 59.7% of this package's own histopathology cohort. A
                # banner that fires on a correct analysis only teaches users to
                # ignore banners. The diagnostic question is not "how many were
                # excluded" but "is this landmark placed sensibly inside the
                # follow-up this cohort actually has", which the median answers.
                lm_median <- calculated_times$quality$median_interval
                if (!is.null(lm_median) && is.finite(lm_median) && lm_median > 0 &&
                    landmark_info$landmark_time > lm_median) {
                    # NOTE: deliberately does NOT invoke guarantee-time (immortal-time)
                    # bias. Landmarking is the REMEDY for that bias, not a cause of it,
                    # and the glossary panel in this same analysis says so; naming it
                    # here would contradict the glossary in one results pane.
                    # Conditional, not assertive, about selection: this analysis has no
                    # event indicator, so it cannot tell early death from recent accrual.
                    add_message('warning', .fmt(
                        .("The {landmark} landmark is later than the median follow-up in these data ({median} {unit}), so more than half the cohort could not reach it and {excluded} of {total} observations were excluded. What remains is a subgroup defined by having been followed to the landmark, and the total person-time above is the denominator for that subgroup only: a rate computed from it answers \"among those still under observation at {amount}, how often did events occur\", not the same question for the cohort you enrolled. This analysis has no event indicator, so it cannot tell you whether the excluded participants had short follow-up because they died early or simply because they were enrolled recently - check that before interpreting the selection. Report the landmark, the number excluded and the reason alongside any rate taken from this analysis."),
                        landmark = lm_adj,
                        median = base::formatC(lm_median, format = "f", digits = 1),
                        unit = lm_unit,
                        excluded = landmark_info$excluded_count,
                        total = landmark_info$original_n, amount = lm_amount))
                }
            } else if (isTRUE(self$options$use_landmark) &&
                       isTRUE(landmark_info$landmark_time == 0)) {
                # The box is ticked and the landmark is zero, so nothing happened at
                # all -- no exclusion, no re-basing. Silent before; a user who went
                # to the trouble of enabling landmark analysis should be told it did
                # not run rather than left to infer it from unchanged numbers.
                add_message('warning', .(
                    'Landmark analysis is switched on but the landmark time is 0, so nothing was excluded and no interval was shortened - these results are the same as with landmark analysis off. Set a landmark greater than zero, in the same unit as the results, or untick landmark analysis.'))
            } else if (lm_on && isTRUE(landmark_info$excluded_count == 0)) {
                # The landmark excluded nobody yet still subtracted itself from every
                # interval. Legitimate in a cohort with uniformly long follow-up, but
                # it is also the exact signature of a unit mix-up: landmark_time is
                # expressed in output_unit and defaults to 6, so a user who means
                # "6 months" while the results are in days silently shortens every
                # interval by 6 days and changes nothing else. That produces a wrong
                # number that looks entirely reasonable, which is worse than an error.
                lm_median <- calculated_times$quality$median_interval
                if (!is.null(lm_median) && is.finite(lm_median) && lm_median > 0 &&
                    landmark_info$landmark_time < 0.05 * lm_median) {
                    add_message('warning', .fmt(
                        .("The {landmark} landmark excluded no participants, because it is only {share}% of the median follow-up in these data ({median} {unit}). It still subtracted {amount} from every interval, so every duration and the total person-time below are that much smaller than the follow-up observed. Check that the landmark is expressed in the same unit as the results ({unit}): entering a value meant as months while the output unit is days is the usual cause."),
                        landmark = lm_adj,
                        share = base::formatC(100 * landmark_info$landmark_time / lm_median,
                                              format = "f", digits = 1),
                        median = base::formatC(lm_median, format = "f", digits = 1),
                        unit = lm_unit, amount = lm_amount))
                }
            }
            filter_text <- if (length(filter_lines) > 0) paste(filter_lines, collapse = "; ") else "None"
            
            # Generate summary statistics
            valid_time_values <- if (!is.null(time_values)) time_values[!is.na(time_values)] else numeric(0)

            # .applyLandmarkAnalysis() DROPS the rows whose interval is NA, so the
            # post-landmark vector never holds one and sum(is.na()) would report 0
            # while the messages panel warns about those very rows -- two
            # contradictory statements on one screen. The landmark step already
            # counted them separately; take its count.
            #
            # CONTRACT for summary_stats$missing, because it is NOT simply
            # "NAs in the analysed vector" and the two readings differ:
            #   landmark OFF -> intervals that could not be calculated among the
            #                   rows that survived the quality filters. (Both
            #                   filters deliberately PRESERVE NA rows -- see the
            #                   `| is.na(...)` arms in .calculate_survival_time --
            #                   so this equals the raw missing count.)
            #   landmark ON  -> intervals that could not be calculated among the
            #                   rows the landmark considered. Structurally these
            #                   rows are NOT in the analysed vector at all; they
            #                   were excluded BY the landmark, which is why the
            #                   count has to come from landmark_info.
            # Either way it counts rows that are absent from `summary_stats$n`,
            # which is what the rendered line says. Consumers today:
            #   * the "Missing values:" line in the summary panel (via missing_text)
            #   * the Clinical Summary note, suppressed under a landmark because
            #     landmark_text already itemises the same rows
            # A third consumer must re-read the two cases above before using it.
            missing_n <- if (lm_on && !is.null(landmark_info$na_excluded)) {
                landmark_info$na_excluded
            } else if (!is.null(time_values)) {
                sum(is.na(time_values))
            } else {
                0
            }

            if (!is.null(time_values) && length(valid_time_values) > 0) {
                summary_stats <- list(
                    n = length(valid_time_values),
                    mean = mean(valid_time_values, na.rm = TRUE),
                    median = stats::median(valid_time_values, na.rm = TRUE),
                    sd = stats::sd(valid_time_values, na.rm = TRUE),
                    min = min(valid_time_values, na.rm = TRUE),
                    max = max(valid_time_values, na.rm = TRUE),
                    missing = missing_n,
                    # (`negative` used to be computed here and was never read. It was
                    # also provably always 0: without remove_negative the run is
                    # rejected on the first negative interval, with it they are
                    # filtered out, and the landmark keeps only values >= the
                    # landmark before subtracting it.)
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

                # A missing interval is never part of the observation count above,
                # with or without a landmark, so the number needs that said next to
                # it -- a bare "Missing values: 10" one line under "Number of
                # observations: 26" reads as though the 10 were inside the 26.
                # Built with paste0, not glue: this string is interpolated into a
                # glue::glue() block and must therefore contain no braces (the same
                # constraint the neighbouring ci_note documents).
                missing_text <- if (summary_stats$missing > 0) {
                    paste0(summary_stats$missing,
                           " (no interval could be calculated; these rows are excluded from the observations above)")
                } else {
                    "0"
                }

                # "Divide by this" is an instruction, and it was printed
                # unconditionally -- including for a cohort whose total person-time
                # is exactly 0 (telling the reader to divide by zero), and on the
                # same page as a strong warning saying a systematic date fault had
                # just been filtered out and the surviving rows could not be assumed
                # correct. A denominator that has just been declared unusable must
                # not carry an unqualified instruction to use it. paste0, not glue:
                # this is interpolated into a glue block and must stay brace-free.
                pt_zero <- isTRUE(summary_stats$total_person_time <= 0)
                pt_suspect <- isTRUE(self$options$remove_negative) &&
                              isTRUE(filter_info$removed_negative /
                                     max(nrow(self$data), 1) >= 0.10)
                denominator_clause <- if (pt_zero) {
                    "is zero, so no incidence rate can be computed from it. Every interval is zero-length or missing; check the date columns before going further."
                } else if (pt_suspect) {
                    paste0("would normally serve as the denominator for incidence rates, but a systematic share of rows was removed as impossible above. Resolve that first: a rate computed from this denominator describes only the rows that happened to survive the filter.")
                } else {
                    paste0("serves as the denominator for calculating incidence rates (for example, events per 100 person-",
                           self$options$output_unit, ").")
                }

                # The fuller explanation lives in the Data Quality panel, but that
                # panel is off by default -- and the residual undetectable case
                # (one or two mis-typed rows in fifty, which no test can separate
                # from chance) is precisely the case with no banner. So the bare
                # fact goes here, in the always-visible summary, as one small-print
                # line. It is a property of the data, not an accusation, so it is
                # phrased as such and is not a banner. paste0: brace-free for glue.
                axp0 <- private$.ambiguousExposure
                amb_line <- if (is.null(axp0)) "" else paste0(
                    "<span style='font-size: 0.9em;'>Day/month ambiguity: ", axp0$n, " of ",
                    axp0$total, " rows could have been typed in either order and cannot be verified here",
                    if (is.finite(axp0$pt_share)) paste0(" (", round(axp0$pt_share),
                                                         "% of the person-time above)") else "",
                    ".</span><br>")

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

                    Missing values: {missing_text}<br>

                    Filters applied: {filter_text}<br>

                    {amb_line}

                    {ci_note}

                                    <div style='background-color: rgba(33, 159, 43, 0.1); padding: 12px; margin-top: 12px; border-left: 3px solid #4caf50; color: inherit;'>

                                        <strong> Interpretation Example:</strong><br>

                                        With a {lm_fu_phrase} of {round(summary_stats$mean, 1)} {self$options$output_unit}

                                        (range: {round(summary_stats$min, 1)} to {round(summary_stats$max, 1)} {self$options$output_unit}),

                                        {if(summary_stats$mean > summary_stats$median) 'the mean sits above the median, which usually indicates a right-skewed follow-up distribution - a minority of cases followed much longer than the rest' else 'the mean sits at or below the median, which gives no indication of a right-skewed follow-up distribution'} (no skewness coefficient is computed here; compare the Median time and Range lines above).

                                        {lm_pt_phrase} ({round(summary_stats$total_person_time, 1)} person-{self$options$output_unit})

                                        {denominator_clause}

                                    </div>

                                ")

                self$results$summary$setContent(summary_text)

                # Small sample size guards. n == 1 is the MOST degenerate case, not an
                # exempt one: the old "n > 1" lower bound let a single observation
                # through with no warning at all, beside a summary reporting
                # "Standard deviation: NA".
                if (summary_stats$n == 1) {
                    add_message('strong_warning', .("Only one interval could be calculated. No spread can be estimated from a single observation, so the standard deviation and the confidence interval are reported as NA, and the mean, median and range are all that one value. This is not a basis for any statistical statement."))
                } else if (summary_stats$n < 10) {
                    add_message('strong_warning', .fmt(
                        .("Critically small sample (n={n}). Statistical summaries are unreliable with fewer than 10 observations. Results should be considered exploratory only. Minimum n=20 recommended for basic descriptive analysis."),
                        n = summary_stats$n))
                } else if (summary_stats$n < 20) {
                    add_message('warning', .fmt(
                        .("Small sample size (n={n}). Confidence intervals may be very wide and unreliable with fewer than 20 observations. Consider collecting more data or interpreting results cautiously."),
                        n = summary_stats$n))
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
                    add_message('warning', .fmt(
                        .("Longest interval in the data is {years} years ({value} {unit}, before any landmark or filter). Intervals beyond 50 years exceed the follow-up of essentially every clinical cohort, and usually mean the dates were parsed with the wrong format rather than observed. Check the Date Format setting and the raw date columns before reporting this person-time. If the intervals are genuinely this long (a lifetime cohort, or an age computed from date of birth), this message can be ignored."),
                        years = base::formatC(max_years, format = "f", digits = 1),
                        value = base::formatC(raw_max, format = "f", digits = 2),
                        unit = self$options$output_unit))
                }

                # Same-day intervals contribute no person-time. The quality panel
                # already lists the count, but it is off by default, so a cohort
                # whose denominator has collapsed would otherwise be announced with
                # nothing but a green "analysis completed" banner.
                zshare <- calculated_times$quality$zero_share
                if (!is.null(zshare) && !is.na(zshare) && zshare >= 0.2) {
                    add_message('warning', .fmt(
                        .("{pct}% of intervals are zero-length (start and end date on the same day). These contribute nothing to total person-time, so the denominator here is smaller than the number of participants suggests. Check whether unfilled follow-up dates were defaulted to the start date."),
                        pct = base::formatC(100 * zshare, format = "f", digits = 0)))
                }

                # Add completion info message
                if (lm_on) {
                    add_message('info', .fmt(
                        .("Analysis completed using {n} observations that reached the {landmark} landmark. All reported times are measured FROM the landmark, not from the start date ({amount} was subtracted from every interval): mean post-landmark follow-up {mean} {unit}, total post-landmark person-time {total} person-{unit}."),
                        n = summary_stats$n, landmark = lm_adj, amount = lm_amount,
                        mean = base::formatC(summary_stats$mean, format = "f", digits = 1),
                        unit = lm_unit,
                        total = base::formatC(summary_stats$total_person_time,
                                              format = "f", digits = 1)))
                } else {
                    add_message('info', .fmt(
                        .("Analysis completed using {n} observations with mean follow-up {mean} {unit} (total person-time: {total} person-{unit})."),
                        n = summary_stats$n,
                        mean = base::formatC(summary_stats$mean, format = "f", digits = 1),
                        unit = lm_unit,
                        total = base::formatC(summary_stats$total_person_time,
                                              format = "f", digits = 1)))
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
                        "<p>All {landmark_info$original_n} {if (landmark_info$original_n == 1) 'participant was' else 'participants were'} excluded by the landmark: none had follow-up reaching <strong>{lm_amount}</strong>.</p>",
                        "<p><strong>What to do:</strong></p>",
                        "<ul>",
                        "<li>Lower the landmark time, or switch it off, and re-read the interval range in the summary</li>",
                        "<li>Check that the landmark is expressed in the same unit as the results ({lm_unit})</li>",
                        "</ul>")
                    add_message('error', .fmt(
                        .("Landmark analysis excluded every participant: none of the {n} observations reached the {amount} landmark. Lower the landmark time or switch landmark analysis off."),
                        n = landmark_info$original_n, amount = lm_amount))
                } else if (filters_emptied) {
                    cause_html <- glue::glue(
                        "<p>Every observation was removed by the data quality filters ({removed_n} in total).</p>",
                        "<p><strong>What to do:</strong></p>",
                        "<ul>",
                        "<li>Switch off the quality filters under Data Quality &amp; Statistics and inspect the raw intervals first</li>",
                        "<li>If every interval was negative, the start and end date columns are probably swapped</li>",
                        "</ul>")
                    add_message('error', .fmt(
                        .("All observations were removed by the data quality filters ({n} rows). Switch the filters off to inspect the raw intervals; if every interval was negative, the start and end date columns are probably swapped."),
                        n = removed_n))
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
                    add_message('error', .("No valid time intervals could be calculated. Check that the date format setting matches the data and that both date columns contain readable dates."))
                }

                error_summary <- paste0(
                    "<div style='background-color: rgba(216, 33, 50, 0.18); padding: 15px; border-left: 4px solid #dc3545; margin: 15px 0; color: inherit;'>",
                    "<h4 style='margin-top: 0; color: inherit;'> No Valid Time Intervals</h4>",
                    cause_html,
                    "</div>")

                self$results$summary$setContent(error_summary)
            }

            # Generate natural-language summary if requested
            # The Clinical Summary exists to be pasted into a manuscript. Withhold
            # it entirely when the dates themselves are under suspicion: a
            # copy-ready sentence is the single worst place for a number the
            # analysis has just warned may be wrong, because it travels beyond the
            # results window where the warning cannot follow it. Two triggers, both
            # already surfaced as banners: rows that look mis-typed in the other
            # day/month order, and a systematic (>=10%) negative-interval fault that
            # was filtered away rather than corrected at source.
            manuscript_unsafe <- !is.null(private$.mixedOrderNote) ||
                (isTRUE(self$options$remove_negative) &&
                 isTRUE(filter_info$removed_negative / max(nrow(self$data), 1) >= 0.10))

            if (self$options$show_summary && manuscript_unsafe) {
                self$results$nlSummary$setContent(paste0(
                    "<div style='background-color: rgba(216, 33, 50, 0.18); padding: 15px; ",
                    "border-left: 4px solid #dc3545; margin: 15px 0; color: inherit;'>",
                    "<h3 style='margin-top: 0; color: inherit;'>Clinical Summary withheld</h3>",
                    "<p>A copy-ready sentence is not produced while the dates in this analysis ",
                    "are under suspicion - see the warning above. Resolve that first: a sentence ",
                    "written to be pasted into a report would carry the number out of this window, ",
                    "leaving the warning behind.</p></div>"))
            }

            if (self$options$show_summary && !manuscript_unsafe && length(valid_time_values) > 0) {
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
                        # "shorter than" rather than "&lt;" or a bare "<". The same
                        # exclusion is stated on three surfaces with two different
                        # escaping rules (this one and the summary render raw, where
                        # a bare "<" breaks the XML-based Word/PDF export; the
                        # messages banner is htmlEscaped, where "&lt;" would appear
                        # literally). One plain-English phrase is right everywhere.
                        excl_parts <- c(excl_parts, sprintf("%d with follow-up shorter than %s",
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

                # Under a landmark these same rows are already itemised in
                # landmark_text above as "N with missing follow-up"; repeating them
                # here would state one exclusion twice in a single sentence.
                quality_text <- if (!lm_on && summary_stats$missing > 0) {
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
                    glue::glue("\"A landmark analysis was performed at {lm_amount}: {n_obs} {if (n_obs == 1) 'participant' else 'participants'} with at least {lm_amount} of follow-up {if (n_obs == 1) 'was' else 'were'} included, and follow-up time was measured from the landmark rather than from the start date. Post-landmark follow-up was a mean of {mean_time} {unit} (median {median_time} {unit}), contributing {total_pt} post-landmark person-{unit} of observation time.\"")
                else
                    glue::glue("\"Follow-up data {if (n_obs == 1) 'was' else 'were'} available for {n_obs} {if (n_obs == 1) 'participant' else 'participants'} (mean {mean_time} {unit}, median {median_time} {unit}), contributing {total_pt} person-{unit} of observation time.\"")

                summary_html <- glue::glue("
                    <div style='background-color: rgba(33, 149, 188, 0.1); padding: 20px; border-left: 5px solid #0066cc; margin: 15px 0; color: inherit;'>
                        <h3 style='margin-top: 0; color: inherit;'> Clinical Summary</h3>
                        <p style='font-size: 1.1em; line-height: 1.6;'>
                        <strong>Time interval analysis</strong> was performed on <strong>{n_obs} {if (n_obs == 1) 'participant' else 'participants'}</strong>{trimws(landmark_text)}.{lm_clause}
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
                            <dd style='margin-left: 20px;'>Restrict the analysis to participants whose follow-up reaches a chosen time point, and measure time from there.
                            Example: include only participants with at least 6 months of follow-up, so that outcomes are not credited to a period nobody could have been observed in.
                            This analysis has no event indicator, so it selects on length of follow-up, not on survival: short follow-up may mean early death or simply recent enrolment.</dd>

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
                        add_message('warning', .fmt(
                            .("{count} observations ({pct}%) have missing time intervals. Investigate missing date values as this may affect study conclusions."),
                            count = quality$missing_values,
                            pct = base::formatC(pct, format = "f", digits = 1)))
                    }
                }

                # Future dates STRONG_WARNING
                if (quality$future_dates > 0) {
                    add_message('strong_warning', .fmt(
                        .("{count} date values are in the future. Review date columns for data entry errors or incorrect date formats."),
                        count = quality$future_dates))
                }
            }

            # Populate quality assessment if requested
            if (self$options$include_quality_metrics && !is.null(calculated_times) &&
                is.list(calculated_times) && !is.null(calculated_times$quality)) {
                quality <- calculated_times$quality

                # paste0, not glue: interpolated into the glue block below, so it
                # must contain no braces of its own.
                axp <- private$.ambiguousExposure
                amb_exposure_html <- if (is.null(axp)) "" else paste0(
                    "<p style='font-size: 0.9em;'><strong>Day/month ambiguity:</strong> ",
                    axp$n, " of ", axp$total, " rows read as a valid date under both '",
                    axp$fmt, "' and '", axp$swap, "'",
                    if (is.finite(axp$pt_share))
                        paste0(", carrying ", round(axp$pt_share), "% of the total person-time")
                    else "",
                    ". The '", axp$fmt, "' reading was used because ", axp$proven_by,
                    " other rows can only be read that way. If any of the ambiguous rows were ",
                    "typed in the other order their intervals are wrong, and because they parse ",
                    "without error and stay positive no check on this page can detect it. ",
                    "This is a property of the data, not a fault found in it.</p>")

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

                        # Day/month ambiguity is an EXPOSURE, not a defect, so it is
                        # stated here rather than raised as a banner: in a clean
                        # dd/mm column a sixth of rows typically have day <= 12 in
                        # both dates, and a banner that fires on most correct
                        # datasets only teaches users to ignore banners. A row typed
                        # in the other order whose day exceeds 12 cannot be parsed at
                        # all and is already counted as missing above; this line
                        # covers the remainder, which no test can distinguish from
                        # chance and which is therefore disclosed instead.
                        "{amb_exposure_html}",


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
                            # The two indented rows split the count above by column, which is
                            # the next thing a pathologist needs to know: WHICH column to go
                            # and fix. They count a parsed date that came out NA, so they also
                            # expose values that were present but unreadable --
                            # .validateParsedDates() tolerates up to 20% of those per column
                            # and they are otherwise invisible everywhere in this analysis.
                            # They can sum to more than "Missing Values": a row missing both
                            # dates is counted once above and once in each row here.
                            "<tr>",
                                "<td style='padding: 8px 8px 8px 28px; border: 1px solid #dee2e6;'>Start dates missing or unreadable</td>",
                                "<td style='padding: 8px; text-align: right; border: 1px solid #dee2e6;'>{quality$missing_start_dates}</td>",
                                "<td style='padding: 8px; text-align: right; border: 1px solid #dee2e6;'>{round(100*quality$missing_start_dates/quality$total_observations, 1)}%</td>",
                            "</tr>",
                            "<tr>",
                                "<td style='padding: 8px 8px 8px 28px; border: 1px solid #dee2e6;'>End dates missing or unreadable</td>",
                                "<td style='padding: 8px; text-align: right; border: 1px solid #dee2e6;'>{quality$missing_end_dates}</td>",
                                "<td style='padding: 8px; text-align: right; border: 1px solid #dee2e6;'>{round(100*quality$missing_end_dates/quality$total_observations, 1)}%</td>",
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
                            <li><strong>Mixed date orders in one column:</strong> if some rows were typed DD/MM/YYYY and others MM/DD/YYYY, choosing a format manually does NOT fix it \u2192 whichever order you pick, the rows typed the other way are read as a different date, silently and without error. Only rows whose day exceeds 12 give themselves away by failing to parse. The fix is in the source data, not here. See the day/month ambiguity figure above for how many rows in this dataset cannot be verified either way</li>
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
