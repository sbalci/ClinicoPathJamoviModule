#' @title DateTime Converter and Component Extractor
#'
#' @description
#' Convert datetime variables to standardized format and extract datetime components
#' (year, month, day, hour, minute, day name, week number, quarter, etc.).
#' Features automatic format detection, quality assessment, and preview of converted data.
#'
#' @details
#' This function provides comprehensive datetime conversion capabilities including:
#' \itemize{
#'   \item Automatic datetime format detection
#'   \item Multiple datetime format parsing options
#'   \item Quality assessment with min/max/missing statistics
#'   \item Preview of converted data
#'   \item Component extraction (year, month, day, hour, minute, etc.)
#'   \item Day names and month names
#'   \item Week numbers, quarters, day of year
#' }
#'
#' @importFrom R6 R6Class
#'
#' @return An \code{R6} class generator object for the \code{datetimeconverterClass} backend; used internally by the jamovi analysis wrapper and not called directly.

datetimeconverterClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "datetimeconverterClass",
    inherit = datetimeconverterBase,
    private = list(
        # Initialize notice collection list
        .noticeList = list(),
        .twoDigitYearSource = FALSE,
        .pivotSuspected = FALSE,

        # Add a notice to the collection
        .addNotice = function(type, title, content, class = NULL) {
            # Drop an exact duplicate. Some helpers are legitimately reached more than
            # once in a run -- .resolveTimezone() is now called from both
            # .prepareDatetimeInput() and .run(), which showed the "Invalid Timezone"
            # warning twice on the POSIXct path -- and a repeated banner reads as two
            # separate problems. Keyed on the rendered content, so two genuinely
            # different messages with the same title both survive.
            for (n in private$.noticeList)
                if (identical(n$title, title) && identical(n$content, content))
                    return(invisible(NULL))
            private$.noticeList[[length(private$.noticeList) + 1]] <- list(
                type = type,
                title = title,
                content = content,
                # Machine-readable tag. Downstream logic MUST branch on this, never on
                # the title or content: those are now wrapped in .() and change under
                # any locale, so a grep for "Ambiguous" silently stops matching the day
                # a translator fills in tr.po -- and the quality grade would reappear
                # underneath a warning saying the dates are in doubt.
                class = class
            )
        },

        # Render collected notices as HTML
        .renderNotices = function() {
            if (length(private$.noticeList) == 0) {
                # Clear, don't bare-return: the analysis object persists across run
                # cycles, so a clean run used to leave the PREVIOUS run's notices on
                # screen verbatim.
                self$results$notices$setContent("")
                return()
            }

            # Map notice types to colors
            typeStyles <- list(
                # Translucent tints, not the opaque pastels these used to be: an
                # opaque light card is a bright island in jamovi's dark theme, and it
                # forced an explicit dark body colour that then had to be maintained.
                # These composite to the same shade over white and stay legible on any
                # ground because the body text inherits.
                ERROR = list(color = "#dc2626", bgcolor = "rgba(220, 38, 38, 0.10)", border = "#fca5a5"),
                STRONG_WARNING = list(color = "#ea580c", bgcolor = "rgba(234, 88, 12, 0.10)", border = "#fdba74"),
                WARNING = list(color = "#ca8a04", bgcolor = "rgba(202, 138, 4, 0.12)", border = "#fde047"),
                INFO = list(color = "#2563eb", bgcolor = "rgba(37, 99, 235, 0.09)", border = "#93c5fd")
            )

            html <- "<div style='margin: 10px 0;'>"

            for (notice in private$.noticeList) {
                # Normalise before lookup: typeStyles is keyed ERROR / STRONG_WARNING /
                # WARNING / INFO, and an unmatched key silently fell back to INFO -- so a
                # severity written "strong_warning" would render as a blue informational
                # box. There is no unmatched-value signal from [[ ]], so it was silent.
                .key <- switch(gsub("[^a-z]", "", tolower(notice$type)),
                    "error" = "ERROR", "strongwarning" = "STRONG_WARNING",
                    "warning" = "WARNING", "info" = "INFO", "WARNING")
                style <- typeStyles[[.key]] %||% typeStyles$INFO

                # Sanitize content for HTML (entity-encode &, <, >, ", ')
                safe_title <- htmltools::htmlEscape(notice$title)
                safe_content <- htmltools::htmlEscape(notice$content)

                html <- paste0(html,
                    "<div style='background-color: ", style$bgcolor, "; ",
                    "border-left: 4px solid ", style$border, "; ",
                    "padding: 12px; margin: 8px 0; border-radius: 4px;'>",
                    "<strong style='color: ", style$color, ";'>",
                    safe_title, "</strong><br>",
                    "<span style='color: inherit;'>", safe_content, "</span>",
                    "</div>"
                )
            }

            html <- paste0(html, "</div>")

            self$results$notices$setContent(html)
        },

        # ===================================================================
        # DATETIME FORMAT DETECTION
        # ===================================================================

        # Detect datetime format from vector
        # Automatically detects the datetime format by testing common patterns against sample values
        # @param datetime_vector Character or numeric vector containing datetime values
        # @return List with $format -- the detected format key (e.g. "ymd", "dmy_hms"),
        #   or "unsure" when no format parses more than 80% of the sample -- and
        #   $warnings, a character vector of ambiguity/failure messages. There is no
        #   "ymd" fallback: "unsure" is returned and .run() skips parsing entirely.
        .detectDatetimeFormat = function(datetime_vector) {
            format_warnings <- character()
            sample_dates <- datetime_vector[!is.na(datetime_vector)]
            if (length(sample_dates) == 0) {
                private$.addNotice(
                    type = "ERROR",
                    title = .("No Valid Datetime Values Found"),
                    content = .("All values in the selected variable are missing (NA). \u2022 Select a different variable or check your data source.")
                )
                return(list(format = "unsure", warnings = 'Auto-detection unavailable: all values missing.'))
            }

            sample_dates <- head(sample_dates, min(40, length(sample_dates)))
            formats_to_try <- c(
                "ymd_hms", "dmy_hms", "mdy_hms",
                "ymd_hm", "dmy_hm", "mdy_hm",
                "ymd", "dmy", "mdy",
                "ydm", "myd", "dym"
            )

            eval_results <- setNames(vector("list", length(formats_to_try)), formats_to_try)
            for (fmt in formats_to_try)
                eval_results[[fmt]] <- private$.evaluateFormat(sample_dates, fmt)

            # Find top two formats by success rate
            success_vec <- vapply(eval_results, function(x) x$success_rate, numeric(1))
            top_order <- names(sort(success_vec, decreasing = TRUE))

            # TIE-BREAK BY REJECTING IMPLAUSIBLY WIDE READINGS.
            #
            # sort() is stable, so the old rule was "first candidate in
            # formats_to_try order wins" -- and ymd is listed before dmy. Any
            # DD/MM/YY column with years <= 2031 is ALWAYS a tie (ymd, dmy, mdy and
            # myd all parse 100%), so ymd ALWAYS won and read the DAY as the year:
            # "05/03/21" became 2005-03-21 instead of 2021-03-05, ~16 years out.
            # Measured 200/200 wrong at every n from 5 to 100.
            #
            # The discriminator is that reading a day-of-month as a two-digit year
            # scatters the parsed years across 1..31, while a real cohort spans a
            # few years. So we DEMOTE any tied reading whose years span more than
            # 20, and keep formats_to_try order among the survivors.
            #
            # It is deliberately demote-only. The tempting rule -- "promote the
            # narrowest" -- is really "the field that varies least is the year",
            # which is wrong whenever the day-of-month is MORE clustered than the
            # cohort's year span. Month-start, quarterly, annual and anniversary
            # columns written yy-mm-dd are exactly that shape: "16-01-01" parses
            # correctly as ymd today, and promote-the-narrowest turns it into
            # 2001-01-16 because dmy pins every row to year 2001 (span 0). That
            # regression is 100% deterministic, so only the >20-year demotion is
            # applied. It also keeps ydm/myd/dym from winning on a degenerate
            # zero span.
            #
            # Genuine ISO text never reaches this branch (dmy cannot parse
            # "2020-03-05" at all), and a true DD/MM vs MM/DD ambiguity has the same
            # year component under both readings, so neither is demoted, the
            # original order still decides, and .warnAmbiguousFormat() below still
            # flags it. .evaluateFormat() already returned $parsed, so this costs no
            # extra parsing.
            #
            # Known remaining gap: a DD/MM/YY column whose day-of-month is itself
            # clustered (e.g. always the 1st) still has no wide reading to demote
            # and is still detected as ymd.
            tied <- top_order[success_vec[top_order] >= (max(success_vec) - 1e-9)]
            if (length(tied) > 1) {
                # NO STRING-SHAPE RULE CAN SETTLE THIS.
                # A rule demoting year-first readings of "2 digits, / or ., ..." was
                # tried here to rescue month-coarsened registry columns (day always the
                # 1st), where the year-span test below has no wide reading to demote and
                # "01/05/19" was read as 2001-05-19 instead of 2019-05-01. It worked for
                # that shape and BROKE the mirror shape: yy/mm/dd and yy.mm.dd are the
                # SAME six-digit string, so the rule read every one of them day-first
                # instead - 0/40 correct, up to 18.9 years out. Measured over a
                # 144-family grid it was net negative (109 vs 111 exactly-correct
                # families), because it traded one silent error for another.
                #
                # "01/05/19" simply is ambiguous. Both readings are flagged by
                # .warnAmbiguousFormat() below, which is the honest outcome: tell the
                # user rather than guess. Only the year-span test remains, which is
                # principled - it demotes a reading that scatters the cohort over more
                # than 20 years, which is what reading a day-of-month as a year does.
                year_span <- vapply(tied, function(fmt) {
                    p <- eval_results[[fmt]]$parsed
                    y <- lubridate::year(p[!is.na(p)])
                    if (length(y) == 0) Inf else as.numeric(diff(range(y)))
                }, numeric(1))
                plausible <- tied[is.finite(year_span) & year_span <= 20]
                if (length(plausible) > 0) tied <- plausible
                top_order <- c(tied, setdiff(top_order, tied))
            }

            top_fmt <- top_order[1]
            second_fmt <- top_order[2]

            # Require top format to clear 0.8; if close competitor disagrees, mark ambiguous
            # >= not >. At exactly 0.8 the old test converted NOTHING: 32 of 40 valid
            # ISO dates were discarded with "Could not reliably detect datetime format"
            # and a 0% success rate, and 40 is the sample cap so 32/40 is very reachable
            # (so are 4/5, 8/10, 16/20). Converting 32 rows and letting the existing
            # low-success-rate warning carry the caveat beats converting none.
            if (!is.null(top_fmt) && success_vec[[top_fmt]] >= 0.8) {
                ambiguity <- private$.warnAmbiguousFormat(top_fmt, eval_results, second_fmt = second_fmt)
                if (length(ambiguity) > 0) {
                    format_warnings <- c(format_warnings, ambiguity)
                }
                return(list(format = top_fmt, warnings = format_warnings))
            }

            private$.addNotice(
                type = "WARNING",
                title = .("Format Detection Failed"),
                content = .fmt(
                    .("Could not reliably detect the datetime format. \u2022 The closest match was {fmt}, which read only {pct}% of the sampled values - below the 80% needed to proceed. \u2022 No dates were converted. Choose the format explicitly (for example DD-MM-YYYY or MM-DD-YYYY), or check the column for mixed formats."),
                    fmt = private$.formatLabel(top_fmt),
                    pct = base::format(round(success_vec[[top_fmt]] * 100, 1)))
            )

            return(list(format = "unsure", warnings = c(format_warnings, 'Auto-detection failed; specify format manually.')))
        },

        .evaluateFormat = function(sample_dates, fmt) {
            parser <- private$.getParser(fmt)
            tryCatch({
                parsed_dates <- suppressWarnings(parser(sample_dates))
                list(
                    success_rate = sum(!is.na(parsed_dates)) / length(sample_dates),
                    parsed = parsed_dates,
                    input = sample_dates
                )
            }, error = function(e) {
                list(
                    success_rate = 0,
                    parsed = rep(as.POSIXct(NA), length(sample_dates)),
                    input = sample_dates
                )
            })
        },

        # `second_fmt` is the runner-up by success rate and is always supplied by
        # .detectDatetimeFormat(): all 12 candidate formats are scored, so top_order[2]
        # is never absent. A hard-coded dmy<->mdy pair list used to sit here as a
        # fallback; it was unreachable, and narrower than what it guarded -- ranking by
        # success rate also catches ymd/ydm and myd/dym confusions, which the pair list
        # never mentioned. Removed rather than kept as dead alternative logic.
        .warnAmbiguousFormat = function(primary_fmt, eval_results, second_fmt) {
            alt_fmt <- second_fmt
            if (is.null(alt_fmt) || is.na(alt_fmt) || is.null(eval_results[[alt_fmt]]))
                return(character())

            primary <- eval_results[[primary_fmt]]
            alternate <- eval_results[[alt_fmt]]
            if (alternate$success_rate <= 0.8)
                return(character())

            disagree <- any(!is.na(primary$parsed) & !is.na(alternate$parsed) & primary$parsed != alternate$parsed)
            if (!disagree)
                return(character())

            # SHOW THE DISAGREEMENT, DO NOT JUST NAME IT.
            # NB: placeholder names are camelCase, never snake_case -- jmvcore's
            # placeholder regex is \{ *[A-Za-z][A-Za-z0-9]* *\}, so {a_date} ships as
            # literal braces with no warning of any kind.
            # The old text always said "ambiguous day/month order", which is the wrong
            # axis for the commonest case: "01/05/19" is a YEAR/DAY swap (2001-05-19 vs
            # 2019-05-01), and it offered a four-digit-year label for a string that has
            # no four-digit year. A worked example from the user's own first row settles
            # it in one glance, whichever axis is actually in doubt.
            .i <- which(!is.na(primary$parsed) & !is.na(alternate$parsed) &
                        primary$parsed != alternate$parsed)[1]
            .raw <- if (!is.na(.i)) as.character(eval_results[[primary_fmt]]$input[.i]) else NA_character_
            msg <- if (!is.na(.i) && !is.na(.raw)) .fmt(
                .("This column can be read two ways and both fit: {a} gives {aDate} while {b} gives {bDate} for the value {value}. \u2022 {a} was used. \u2022 Check the preview, and set DateTime Format explicitly if that reading is wrong."),
                a = private$.formatLabel(primary_fmt),
                aDate = base::format(primary$parsed[.i], '%Y-%m-%d'),
                b = private$.formatLabel(alt_fmt),
                bDate = base::format(alternate$parsed[.i], '%Y-%m-%d'),
                value = .raw) else .fmt(
                .("This column can be read two ways and both fit: {a} and {b} each parsed more than 80% of the values but produced different dates. \u2022 {a} was used. \u2022 Check the preview, and set DateTime Format explicitly if that reading is wrong."),
                a = private$.formatLabel(primary_fmt),
                b = private$.formatLabel(alt_fmt))

            private$.addNotice(
                type = "WARNING",
                title = .("Ambiguous Format Detected"),
                class = "date-suspect",
                content = msg
            )

            return(msg)
        },

        .formatLabel = function(fmt) {
            labels <- list(
                ymd = "YYYY-MM-DD",
                dmy = "DD-MM-YYYY",
                mdy = "MM-DD-YYYY",
                ymd_hms = "YYYY-MM-DD HH:MM:SS",
                dmy_hms = "DD-MM-YYYY HH:MM:SS",
                mdy_hms = "MM-DD-YYYY HH:MM:SS",
                ymd_hm = "YYYY-MM-DD HH:MM",
                dmy_hm = "DD-MM-YYYY HH:MM",
                mdy_hm = "MM-DD-YYYY HH:MM",
                ymdhms = "YYYY-MM-DD HH:MM:SS",
                dmyhms = "DD-MM-YYYY HH:MM:SS",
                mdyhms = "MM-DD-YYYY HH:MM:SS",
                ymdhm = "YYYY-MM-DD HH:MM",
                dmyhm = "DD-MM-YYYY HH:MM",
                mdyhm = "MM-DD-YYYY HH:MM",
                ydm = "YYYY-DD-MM",
                myd = "MM-YYYY-DD",
                dym = "DD-YYYY-MM",
                excel_serial = "Excel serial",
                excel_serial_1904 = "Excel serial (1904 system)",
                unix_epoch = "Unix epoch",
                unix_epoch_ms = "Unix epoch milliseconds"
            )
            return(labels[[fmt]] %||% toupper(fmt))
        },

        # Get lubridate parser function for format
        # @param format Character string specifying datetime format
        # @return Lubridate parser function (e.g., lubridate::ymd, lubridate::dmy_hms)
        # TRUE when the column's year token cannot be four digits, i.e. every value
        # is free of a 4-digit run. That is the only situation in which the century
        # pivot can be wrong, so every two-digit-year behaviour is gated on it.
        .hasTwoDigitYear = function(x) {
            v <- as.character(x)
            v <- v[!is.na(v) & nzchar(trimws(v))]
            if (length(v) == 0) return(FALSE)
            !any(grepl("[0-9]{4}", v))
        },

        .getParser = function(format) {
            # Return appropriate lubridate parser for format
            parser <- switch(format,
                "ymd_hms" = lubridate::ymd_hms,
                "dmy_hms" = lubridate::dmy_hms,
                "mdy_hms" = lubridate::mdy_hms,
                "ymd_hm" = lubridate::ymd_hm,
                "dmy_hm" = lubridate::dmy_hm,
                "mdy_hm" = lubridate::mdy_hm,
                "dmyhms" = lubridate::dmy_hms,
                "mdyhms" = lubridate::mdy_hms,
                "ymdhms" = lubridate::ymd_hms,
                "dmyhm" = lubridate::dmy_hm,
                "mdyhm" = lubridate::mdy_hm,
                "ymdhm" = lubridate::ymd_hm,
                "ymd" = lubridate::ymd,
                "dmy" = lubridate::dmy,
                "mdy" = lubridate::mdy,
                "ydm" = lubridate::ydm,
                "myd" = lubridate::myd,
                "dym" = lubridate::dym,
                stop("Unsupported datetime format: ", format)
            )
            return(parser)
        },

        .prepareDatetimeInput = function(vector) {
            # Normalise input prior to parsing and keep user-friendly display copy

            notes <- character()
            format_hint <- NULL
            parsed_dates <- NULL

            quality_vector <- vector

            # Every format() call in this file is qualified base::format on purpose.
            #
            # NAMESPACE does a blanket `import(jmvcore)`, and jmvcore exports its own
            # format() -- a {}-placeholder string templater. Inside this package a bare
            # `format(x, "%Y")` therefore resolves to jmvcore's, which IGNORES the
            # format string and just stringifies its argument, and ignores
            # scientific = FALSE as well. That silently produced:
            #   * a false "Implausible Dates Detected" warning on every conversion, and
            #   * "1.7e+09" where the original value 1700000000 should appear -- in the
            #     numeric fallback branch below that string is also what gets PARSED,
            #     so a mangled number becomes an unparseable date rather than a
            #     visible error.
            # Keep the base:: qualifier on any format() added here.
            if (inherits(vector, c('POSIXct', 'POSIXt'))) {
                # HONOUR THE TIMEZONE OPTION HERE TOO.
                # as_datetime() keeps the incoming tzone, so the option had NO effect
                # on this branch -- while the Summary went on asserting "Timezone: UTC".
                # A multi-centre study standardised to UTC got local-clock hours, and at
                # day boundaries the wrong DATE, under a label saying otherwise.
                # with_tz() re-expresses the same instant in the requested zone: the
                # moment is unchanged, only the calendar/clock fields the components are
                # read from. tz = "" (the "system" default) means local, which is what
                # this branch already did, so the default path is unchanged.
                parsed_dates <- lubridate::as_datetime(vector)
                .tz_req <- private$.resolveTimezone()$tz
                if (nzchar(.tz_req))
                    parsed_dates <- lubridate::with_tz(parsed_dates, tzone = .tz_req)
                original_display <- base::format(vector, usetz = TRUE)
                notes <- c(notes, 'Detected POSIXct/POSIXt input; using supplied datetimes directly.')
                format_hint <- 'posixct'
                return(list(
                    original_display = original_display,
                    parsing_vector = parsed_dates,
                    quality_vector = quality_vector,
                    parsed_dates = parsed_dates,
                    already_parsed = TRUE,
                    format_hint = format_hint,
                    notes = notes
                ))
            }

            if (inherits(vector, 'Date')) {
                parsed_dates <- as.POSIXct(vector)
                original_display <- base::format(vector)
                notes <- c(notes, 'Detected Date input; converted to POSIXct at midnight.')
                format_hint <- 'date'
                return(list(
                    original_display = original_display,
                    parsing_vector = parsed_dates,
                    quality_vector = quality_vector,
                    parsed_dates = parsed_dates,
                    already_parsed = TRUE,
                    format_hint = format_hint,
                    notes = notes
                ))
            }

            if (is.factor(vector)) {
                vector <- as.character(vector)
                quality_vector <- vector
                notes <- c(notes, 'Converted factor input to character.')
            }

            if (is.numeric(vector) && !inherits(vector, 'Date')) {
                numeric_override <- self$options$datetime_format
                numeric_force <- if (!is.null(numeric_override) && numeric_override %in% c('excel_serial', 'excel_serial_1904', 'unix_epoch'))
                    numeric_override else NULL
                return(private$.processNumericVector(
                    numeric_vector = vector,
                    notes = notes,
                    quality_vector = quality_vector,
                    force_format = numeric_force
                ))
            }

            char_vals <- as.character(vector)
            char_vals <- trimws(char_vals)
            blank_idx <- which(char_vals == '')
            if (length(blank_idx) > 0) {
                char_vals[blank_idx] <- NA_character_
                notes <- c(notes, paste0('Converted ', length(blank_idx), ' blank entries to missing.'))
            }

            non_blank <- !is.na(char_vals)
            numeric_guess <- suppressWarnings(as.numeric(char_vals))
            numeric_guess[!non_blank] <- NA_real_
            
            # Check for manual override of heuristics via datetime_format option
            manual_format <- self$options$datetime_format
            
            if (manual_format == "excel_serial") {
                notes <- c(notes, 'Manual override: forcing Excel serial number interpretation.')
                return(private$.processNumericVector(
                    numeric_vector = numeric_guess,
                    notes = notes,
                    quality_vector = char_vals,
                    original_display = char_vals,
                    force_format = 'excel_serial'
                ))
            }
            
            if (manual_format == "excel_serial_1904") {
                notes <- c(notes, 'Manual override: forcing Excel serial number interpretation (1904 system).')
                return(private$.processNumericVector(
                    numeric_vector = numeric_guess,
                    notes = notes,
                    quality_vector = char_vals,
                    original_display = char_vals,
                    force_format = 'excel_serial_1904'
                ))
            }

            if (manual_format == "unix_epoch") {
                notes <- c(notes, 'Manual override: forcing Unix epoch interpretation.')
                return(private$.processNumericVector(
                    numeric_vector = numeric_guess,
                    notes = notes,
                    quality_vector = char_vals,
                    original_display = char_vals,
                    force_format = 'unix_epoch'
                ))
            }

            numeric_ratio <- if (sum(non_blank) == 0) 0 else sum(!is.na(numeric_guess[non_blank])) / sum(non_blank)
            if (numeric_ratio >= 0.8) {
                notes <- c(notes, 'Detected numeric serial values stored as text; automatically converted before parsing.')
                return(private$.processNumericVector(
                    numeric_vector = numeric_guess,
                    notes = notes,
                    quality_vector = char_vals,
                    original_display = char_vals
                ))
            }

            return(list(
                original_display = char_vals,
                parsing_vector = char_vals,
                quality_vector = char_vals,
                parsed_dates = NULL,
                already_parsed = FALSE,
                format_hint = NULL,
                notes = notes
            ))
        },

        .processNumericVector = function(numeric_vector, notes, quality_vector, original_display = NULL, force_format = NULL) {
            quality_vector <- quality_vector %||% numeric_vector
            if (is.null(original_display)) {
                original_display <- base::format(numeric_vector, trim = TRUE, scientific = FALSE)
                original_display[is.na(numeric_vector)] <- NA_character_
            }

            # A NEGATIVE SERIAL IS NOT A DATE IN EITHER EXCEL EPOCH.
            # -99 / -1 are ordinary missing-data sentinels in clinical CSVs. They
            # used to be the ONLY thing that could reach the 1904 branch, so a single
            # sentinel silently reinterpreted the whole column on the wrong origin;
            # with that branch gone they would instead push the column out of
            # excel_like entirely and NA every valid serial in it. Mask them here so
            # the survivors convert normally and the quality panel reports the true
            # success rate.
            # ...but NOT on the Unix path: a negative epoch second is a perfectly
            # valid instant (any date before 1970-01-01). Masking there silently NA'd
            # half a date-of-birth column and then advised "try a different datetime
            # format" -- when the selected format was the correct one. The auto path
            # needs no exemption: unix_like requires >= 1e9, so a negative can never
            # reach it.
            .neg <- !is.na(numeric_vector) & numeric_vector < 0 &
                    !identical(force_format, 'unix_epoch')
            if (any(.neg)) {
                numeric_vector[.neg] <- NA_real_
                notes <- c(notes, sprintf(
                    'Set %d negative value(s) to missing: a negative serial is not a valid date in either Excel epoch.',
                    sum(.neg)))
            }

            non_missing <- numeric_vector[!is.na(numeric_vector)]
            if (length(non_missing) == 0) {
                empty_vals <- rep(NA_character_, length(numeric_vector))
                return(list(
                    original_display = original_display,
                    parsing_vector = empty_vals,
                    quality_vector = quality_vector,
                    parsed_dates = NULL,
                    already_parsed = FALSE,
                    format_hint = NULL,
                    notes = c(notes, 'Numeric column contained only missing values.')
                ))
            }

            # Honor manual override: force the requested numeric interpretation
            # regardless of the range-based heuristics below.
            if (!is.null(force_format) && force_format == 'excel_serial') {
                # Excel serials count DAYS; as.POSIXct.numeric reads SECONDS. Without
                # the * 86400 every Excel date collapsed onto the origin itself.
                parsed_dates <- as.POSIXct(round(numeric_vector * 86400), origin = '1899-12-30', tz = 'UTC')
                notes <- c(notes, 'Forced Excel serial interpretation (1900 system); converted using origin 1899-12-30 (UTC).')
                return(list(
                    original_display = original_display,
                    parsing_vector = parsed_dates,
                    quality_vector = quality_vector,
                    parsed_dates = parsed_dates,
                    already_parsed = TRUE,
                    format_hint = 'excel_serial',
                    notes = notes
                ))
            }

            # ROUND TO THE SECOND. An Excel serial's fractional part is a binary
            # double, so 00:05 is stored as 45000.003472222219 and *86400 lands at
            # 00:04:59.999999 -- one minute LOW once the minute is extracted.
            # Measured over all 1440 minutes of a day: 160 wrong (11.1%), and the
            # extracted second came back as 59.999999 against a varDescription that
            # promises 0-59. Hours were never wrong, which is why it survived casual
            # testing. readxl and openxlsx round for exactly this reason.
            if (!is.null(force_format) && force_format == 'excel_serial_1904') {
                # Legacy Mac Excel (1904 system): serial 0 is 1904-01-01, exactly
                # 1462 days after the 1900 system's 1899-12-30 origin. Excel serials
                # count DAYS while as.POSIXct.numeric reads SECONDS, hence * 86400.
                parsed_dates <- as.POSIXct(round(numeric_vector * 86400), origin = '1904-01-01', tz = 'UTC')
                notes <- c(notes, 'Forced Excel serial interpretation (1904 system); converted using origin 1904-01-01 (UTC).')
                return(list(
                    original_display = original_display,
                    parsing_vector = parsed_dates,
                    quality_vector = quality_vector,
                    parsed_dates = parsed_dates,
                    already_parsed = TRUE,
                    format_hint = 'excel_serial_1904',
                    notes = notes
                ))
            }

            if (!is.null(force_format) && force_format == 'unix_epoch') {
                # Mirror of the Excel guard below. Epoch seconds for any realistic
                # clinical date are >= ~1e8 (1973-03); an Excel serial column fed here
                # collapses into a single day in 1970, which is the commonest numeric
                # misclick and produced no warning at all.
                if (length(non_missing) > 0 &&
                    (max(non_missing) < 1e8 || min(non_missing) > 4e9)) {
                    .edge_u <- if (max(non_missing) < 1e8) max(non_missing) else min(non_missing)
                    private$.addNotice(
                        type = "WARNING",
                        title = .("Numeric Column May Not Be Unix Timestamps"),
                        class = "date-suspect",
                        content = .fmt(
                            .("This column was converted as Unix epoch seconds, but its {which} value ({value}) converts to {date}. \u2022 Epoch seconds for a realistic clinical date are roughly 1e8 to 4e9 (1973 to 2096). \u2022 If these are Excel serial numbers, choose an Excel Serial format instead; if they are measurements, select a real date column."),
                            which = if (max(non_missing) < 1e8) "largest" else "smallest",
                            value = base::format(.edge_u, trim = TRUE, scientific = FALSE),
                            date = base::format(as.POSIXct(.edge_u, origin = '1970-01-01', tz = 'UTC'), '%Y-%m-%d')))
                }
                parsed_dates <- as.POSIXct(numeric_vector, origin = '1970-01-01', tz = 'UTC')
                notes <- c(notes, 'Forced Unix epoch interpretation; converted using origin 1970-01-01 (UTC).')
                return(list(
                    original_display = original_display,
                    parsing_vector = parsed_dates,
                    quality_vector = quality_vector,
                    parsed_dates = parsed_dates,
                    already_parsed = TRUE,
                    format_hint = 'unix_epoch',
                    notes = notes
                ))
            }

            # 1900 and 1904 serials are numerically indistinguishable, so there is
            # deliberately no 1904 auto-branch: the user selects excel_serial_1904
            # explicitly. The old excel_1904_like range was unreachable for valid
            # data anyway -- excel_like is tested first and matches every
            # all-non-negative serial set -- so its only live trigger was a NEGATIVE
            # value, which is not a valid date in either system (1900 starts at
            # serial 1, 1904 at serial 0). Negatives are masked to NA just above, so
            # one -99 missing-data sentinel no longer decides the epoch for the
            # whole column.
            excel_like <- all(non_missing >= 0 & non_missing <= 600000)
            unix_like <- all(non_missing >= 1e9 & non_missing <= 4e9)
            unix_ms_like <- all(non_missing >= 1e12 & non_missing <= 4e12)

            # SMALL SERIALS ARE MEASUREMENTS, NOT DATES.
            # excel_like accepts [0, 600000], and .detectMisuse only looks for
            # year < 1900, future dates and >100-year spans -- so serials 2..55000
            # land in 1900-2050 and were converted with no word to the user. An age,
            # count, score or days-of-follow-up column silently became a 1900 date.
            # Judge by what the conversion PRODUCED: a genuine clinical Excel date is
            # >= ~20000 (1954 onward), so a column whose LARGEST value is under 10000
            # (1927-05-18) is almost certainly not dates.
            #
            # Checked here rather than in .detectMisuse because that method receives
            # only parsed_dates and no format hint, so the same rule there would also
            # fire on a genuinely text-parsed 1920s date column and show an
            # Excel-serial message to someone who never touched Excel. Here we know
            # the Excel branch was taken and still have the raw serials.
            #
            # ponytail: magnitude only. A spread rule was measured and lost true
            # positives without catching anything extra.
            # Two-sided, and on min/max rather than both-ends-agree, so no column falls
            # between this warning and the epoch INFO below. Serial 10000 = 1927-05-18,
            # 55000 = 2050-08-19; a clinical date column lives well inside that.
            .too_small <- max(non_missing) < 10000
            .too_large <- min(non_missing) > 55000
            if (excel_like && (.too_small || .too_large)) {
                .edge <- if (.too_small) max(non_missing) else min(non_missing)
                max_date <- as.POSIXct(.edge * 86400, origin = '1899-12-30', tz = 'UTC')
                private$.addNotice(
                    type = "WARNING",
                    title = .("Numeric Column May Not Be Dates"),
                class = "date-suspect",
                    content = if (.too_small) .fmt(
                        .("This column was converted as Excel serial dates, but its largest value ({value}) converts to {date}, so every converted date falls in {year} or earlier. \u2022 Genuine Excel dates from clinical spreadsheets are normally between 20000 and 50000 (1954 to 2036). \u2022 If this column holds measurements - age, counts, scores, lab values, days of follow-up - the converted dates are meaningless; select a real date column instead. \u2022 If these really are early dates, no action is needed."),
                        value = base::format(.edge, trim = TRUE, scientific = FALSE),
                        date = base::format(max_date, '%Y-%m-%d'),
                        year = base::format(max_date, '%Y')) else .fmt(
                        .("This column was converted as Excel serial dates, but its smallest value ({value}) converts to {date}, so every converted date falls on or after {year}. \u2022 Genuine Excel dates from clinical spreadsheets are normally between 20000 and 50000 (1954 to 2036). \u2022 If this column holds measurements - platelet counts, lab values, scores - the converted dates are meaningless; select a real date column instead. \u2022 If these really are dates that far ahead, no action is needed."),
                        value = base::format(.edge, trim = TRUE, scientific = FALSE),
                        date = base::format(max_date, '%Y-%m-%d'),
                        year = base::format(max_date, '%Y')
                    )
                )
            }

            if (excel_like) {
                # Excel serials count DAYS; as.POSIXct.numeric reads SECONDS. Without
                # the * 86400 every Excel date collapsed onto the origin itself.
                parsed_dates <- as.POSIXct(round(numeric_vector * 86400), origin = '1899-12-30', tz = 'UTC')
                notes <- c(notes, 'Detected Excel serial numbers (1900 system); converted using origin 1899-12-30 (UTC).')
                # 1900 and 1904 serials are numerically identical; no heuristic can
                # separate them. Default to 1900 (Windows Excel, and Mac Excel 2011+)
                # and DISCLOSE the assumption with a worked example from this data.
                #
                # Gated on min >= 10000 (serial 10000 = 1927-05-18) so it fires only
                # where the column plausibly IS a date column. Ungated it also fired
                # on ages and counts -- which excel_like happily accepts -- where it
                # would assert the values ARE Excel serials and offer the wrong epoch
                # as the only alternative. Those columns get the "may not be dates"
                # warning below instead.
                if (min(non_missing) >= 10000 && max(non_missing) <= 55000) {
                    sample_serial <- non_missing[1]
                    private$.addNotice(
                        type = "INFO",
                        title = .("Excel Serial Origin Assumed (1900 System)"),
                        content = .fmt(
                            .("Excel serial numbers do not record which epoch they came from, so the 1900 system was assumed. \u2022 Serial {serial} was read as {readAs}; under the legacy Mac 1904 system the same number is {alt} (a difference of 1462 days, about 4 years). \u2022 The 1900 system is correct for Windows Excel and for Mac Excel 2011 and later. \u2022 If this file came from Mac Excel 2008 or earlier, set DateTime Format to Excel Serial (Days since 1904, legacy Mac)."),
                            serial = base::format(sample_serial, trim = TRUE, scientific = FALSE),
                            readAs = base::format(as.POSIXct(sample_serial * 86400, origin = '1899-12-30', tz = 'UTC'), '%Y-%m-%d'),
                            alt = base::format(as.POSIXct(sample_serial * 86400, origin = '1904-01-01', tz = 'UTC'), '%Y-%m-%d'))
                    )
                }
                return(list(
                    original_display = original_display,
                    parsing_vector = parsed_dates,
                    quality_vector = quality_vector,
                    parsed_dates = parsed_dates,
                    already_parsed = TRUE,
                    format_hint = 'excel_serial',
                    notes = notes
                ))
            }

            if (unix_like) {
                parsed_dates <- as.POSIXct(numeric_vector, origin = '1970-01-01', tz = 'UTC')
                notes <- c(notes, 'Detected Unix epoch seconds; converted using origin 1970-01-01 (UTC).')
                return(list(
                    original_display = original_display,
                    parsing_vector = parsed_dates,
                    quality_vector = quality_vector,
                    parsed_dates = parsed_dates,
                    already_parsed = TRUE,
                    format_hint = 'unix_epoch',
                    notes = notes
                ))
            }

            if (unix_ms_like) {
                parsed_dates <- as.POSIXct(numeric_vector/1000, origin = '1970-01-01', tz = 'UTC')
                notes <- c(notes, 'Detected Unix epoch milliseconds; converted using origin 1970-01-01 (UTC).')
                return(list(
                    original_display = original_display,
                    parsing_vector = parsed_dates,
                    quality_vector = quality_vector,
                    parsed_dates = parsed_dates,
                    already_parsed = TRUE,
                    format_hint = 'unix_epoch_ms',
                    notes = notes
                ))
            }

            char_vals <- if (!is.null(original_display)) original_display else {
                tmp <- base::format(numeric_vector, trim = TRUE, scientific = FALSE)
                tmp[is.na(numeric_vector)] <- NA_character_
                tmp
            }
            char_vals <- trimws(char_vals)
            char_vals[char_vals == ''] <- NA_character_
            notes <- c(notes, 'Treated numeric values as formatted strings for parsing.')
            return(list(
                original_display = char_vals,
                parsing_vector = char_vals,
                quality_vector = quality_vector,
                parsed_dates = NULL,
                already_parsed = FALSE,
                format_hint = NULL,
                notes = notes
            ))
        },
        # ===================================================================
        # CHARACTER CONVERSION UTILITY
        # ===================================================================

        # Safe character conversion with NA protection
        # Three-layer NA protection for factor/datetime to character conversion
        # Prevents "0" or empty strings from appearing as NA values
        # @param source_vector Vector to convert (factor, datetime, or other)
        # @return Character vector with proper NA preservation
        .safeCharacterConversion = function(source_vector) {
            # Three-layer NA protection for factor/datetime to character conversion
            # Prevents "0" or empty strings from appearing as NA values

            char_vector <- rep(NA_character_, length(source_vector))
            valid_idx <- !is.na(source_vector)

            if (any(valid_idx)) {
                char_vector[valid_idx] <- as.character(source_vector[valid_idx])
            }

            # Safety filter: remove spurious values
            char_vector[char_vector == "0" | char_vector == "" | is.na(char_vector)] <- NA_character_

            return(char_vector)
        },

        # ===================================================================
        # DATETIME PARSING
        # ===================================================================

        # Parse datetime using detected or selected format
        # @param datetime_vector Character/numeric vector to parse
        # @param format Format string (e.g., "ymd", "dmy_hms")
        # @param tz Timezone (default: "" for system timezone)
        # @return POSIXct datetime vector
        .parseDatetime = function(datetime_vector, format, tz = "") {
            # Parse datetime using detected or selected format

            parser <- private$.getParser(format)

            tryCatch({
                # Apply timezone if parser supports it
                parsed_dates <- parser(datetime_vector, tz = tz)
                # Plausibility check: flag years out of range
                # base::format, NOT the bare `format` -- see the note in
                # .prepareDatetimeInput(). Unqualified, `format(parsed_dates, "%Y")`
                # returned the whole datetime string, as.integer() then produced the
                # epoch seconds (1710498030 for 2024-03-15), and the upper bound
                # as.integer(format(Sys.Date()+365, "%Y")) came out as 21033 -- so
                # every ordinary date compared "> 21033" and this warning fired on
                # 100%-successful conversions of perfectly valid 2022-2024 dates.
                # A warning that cries wolf on every run is worse than no warning:
                # it teaches the user to ignore the one signal that a wrong format
                # has silently mis-parsed their dates.
                yrs <- suppressWarnings(as.integer(base::format(parsed_dates, "%Y")))
                if (any(!is.na(yrs) & (yrs < 1900 | yrs > as.integer(base::format(Sys.Date() + 365, "%Y"))))) {
                    private$.addNotice(
                        type = "WARNING",
                        title = .("Implausible Dates Detected"),
                class = "date-suspect",
                        content = .("Detected parsed dates outside plausible range (<1900 or >1 year in future). Review preview to confirm correct parsing.")
                    )
                }
                return(parsed_dates)
            }, error = function(e) {
                private$.addNotice(
                    type = "ERROR",
                    title = .("Parsing Error"),
                    content = .fmt(
                        .("Error parsing datetimes with format {fmt}. \u2022 Parser error: {msg} \u2022 Try selecting a different format. \u2022 Check that your data matches the selected format."),
                        fmt = format, msg = e$message)
                )
                # Return NA vector to allow analysis to continue
                return(rep(as.POSIXct(NA), length(datetime_vector)))
            })
        },

        .resolveTimezone = function() {
            tz_option <- self$options$timezone
            tz_option <- if (is.null(tz_option)) "system" else trimws(tz_option)
            tz_lower <- tolower(tz_option)
            system_label <- Sys.timezone()
            if (is.null(system_label) || is.na(system_label))
                system_label <- "system default"

            if (tz_lower == "" || tz_lower == "system") {
                return(list(
                    tz = "",
                    note = paste0("String-to-datetime conversions use the system default timezone (", system_label, ")."),
                    summary = system_label
                ))
            }

            if (tz_lower == "utc") {
                return(list(
                    tz = "UTC",
                    note = "String-to-datetime conversions use UTC.",
                    summary = "UTC"
                ))
            }

            if (! tz_option %in% OlsonNames()) {
                private$.addNotice(
                    type = "WARNING",
                    title = .("Invalid Timezone"),
                    content = .fmt(
                        .("Timezone {requested} is not a recognised Olson timezone. Falling back to the system default ({fallback})."),
                        requested = tz_option, fallback = system_label)
                )
                return(list(
                    tz = "",
                    note = sprintf("Requested timezone '%s' was invalid; reverted to system default (%s).", tz_option, system_label),
                    summary = system_label
                ))
            }

            return(list(
                tz = tz_option,
                note = sprintf("String-to-datetime conversions use the '%s' timezone.", tz_option),
                summary = tz_option
            ))
        },

        # ===================================================================
        # QUALITY ASSESSMENT
        # ===================================================================

        # Assess quality of datetime parsing
        # @param original Original input vector (before parsing)
        # @param parsed Parsed POSIXct datetime vector
        # @return List with quality metrics (total_observations, success_rate, min/max datetime, etc.)
        .assessQuality = function(original, parsed) {
            # Comprehensive quality assessment

            total_obs <- length(original)
            original_na <- sum(is.na(original))

            successful <- sum(!is.na(parsed) & !is.na(original))
            failed_parsing <- sum(is.na(parsed) & !is.na(original))
            non_missing_original <- total_obs - original_na

            success_rate <- if (non_missing_original > 0) {
                round(successful / non_missing_original * 100, 2)
            } else {
                NA_real_
            }

            # Note: DMY/MDY ambiguity is surfaced via the .warnAmbiguousFormat notice,
            # so no per-observation ambiguity metric is computed here.

            quality_metrics <- list(
                total_observations = total_obs,
                original_missing = original_na,
                successfully_parsed = successful,
                failed_parsing = failed_parsing,
                success_rate = success_rate,
                min_datetime = if (successful > 0) min(parsed, na.rm = TRUE) else NA,
                max_datetime = if (successful > 0) max(parsed, na.rm = TRUE) else NA
            )

            # Calculate date range
            if (successful > 0 && !is.na(quality_metrics$min_datetime) && !is.na(quality_metrics$max_datetime)) {
                range_days <- as.numeric(difftime(
                    quality_metrics$max_datetime,
                    quality_metrics$min_datetime,
                    units = "days"
                ))
                quality_metrics$range_days <- round(range_days, 1)
                quality_metrics$range_years <- round(range_days / 365.25, 2)
            } else {
                quality_metrics$range_days <- NA
                quality_metrics$range_years <- NA
            }

            # Get sample of failed values
            if (failed_parsing > 0) {
                failed_indices <- which(is.na(parsed) & !is.na(original))
                sample_size <- min(10, length(failed_indices))
                quality_metrics$failed_samples <- as.character(original[head(failed_indices, sample_size)])
            } else {
                quality_metrics$failed_samples <- character(0)
            }

            return(quality_metrics)
        },

        # ===================================================================
        # COMPONENT EXTRACTION
        # ===================================================================

        # Extract datetime components (year, month, day, etc.)
        # @param parsed_dates POSIXct datetime vector
        # @return List of extracted components based on user selections
        .extractComponents = function(parsed_dates) {
            # Extract all datetime components

            components <- list()

            # Check if EITHER preview extraction OR data output is requested
            if (self$options$extract_year || self$options$year_out) {
                components$year <- lubridate::year(parsed_dates)
            }

            if (self$options$extract_month || self$options$month_out) {
                components$month <- lubridate::month(parsed_dates)
            }

            if (self$options$extract_monthname || self$options$monthname_out) {
                monthname_factor <- lubridate::month(parsed_dates, label = TRUE, abbr = FALSE)
                # Ensure NA values are preserved, not converted to empty strings or zeros
                monthname_factor[is.na(parsed_dates)] <- NA
                components$monthname <- monthname_factor
            }

            if (self$options$extract_day || self$options$day_out) {
                components$day <- lubridate::day(parsed_dates)
            }

            if (self$options$extract_hour || self$options$hour_out) {
                components$hour <- lubridate::hour(parsed_dates)
            }

            if (self$options$extract_minute || self$options$minute_out) {
                components$minute <- lubridate::minute(parsed_dates)
            }

            if (self$options$extract_second || self$options$second_out) {
                components$second <- lubridate::second(parsed_dates)
            }

            if (self$options$extract_dayname || self$options$dayname_out) {
                dayname_factor <- lubridate::wday(parsed_dates, label = TRUE, abbr = FALSE)
                # Ensure NA values are preserved, not converted to empty strings or zeros
                dayname_factor[is.na(parsed_dates)] <- NA
                components$dayname <- dayname_factor
            }

            if (self$options$extract_weeknum || self$options$weeknum_out) {
                components$weeknum <- lubridate::week(parsed_dates)
            }

            if (self$options$extract_quarter || self$options$quarter_out) {
                components$quarter <- lubridate::quarter(parsed_dates)
            }

            if (self$options$extract_dayofyear || self$options$dayofyear_out) {
                components$dayofyear <- lubridate::yday(parsed_dates)
            }

            return(components)
        },

        # ===================================================================
        # HTML PREVIEW GENERATION
        # ===================================================================

        # Update output column titles with actual variable name
        # @param datetime_var Name of the datetime variable selected by user
        # @details Replaces template strings in varTitle/varDescription with actual variable name
        .updateOutputTitles = function(datetime_var) {
            if (is.null(datetime_var) || datetime_var == "")
                return()

            fmt <- function(template) {
                if (is.null(template))
                    return(NULL)
                gsub('\\$?\\{ *datetime_var *\\}', datetime_var, template)
            }

            update_output <- function(result, title_template = NULL, description_template = NULL) {
                if (is.null(result))
                    return()
                if (!is.null(title_template))
                    result$setTitle(fmt(title_template))
                if (!is.null(description_template))
                    result$setDescription(fmt(description_template))
            }

            update_output(self$results$corrected_datetime_char,
                "Corrected DateTime - from {datetime_var}",
                "DateTime variable {datetime_var} converted to standardized format (as character string)")
            update_output(self$results$corrected_datetime_numeric,
                "Corrected DateTime Numeric - from {datetime_var}",
                "DateTime variable {datetime_var} as Unix epoch seconds for calculations")
            update_output(self$results$year_out,
                "Year - from {datetime_var}",
                "Extracted year component from {datetime_var}")
            update_output(self$results$month_out,
                "Month - from {datetime_var}",
                "Extracted month component (1-12) from {datetime_var}")
            update_output(self$results$monthname_out,
                "Month Name - from {datetime_var}",
                "Extracted month name from {datetime_var}")
            update_output(self$results$day_out,
                "Day - from {datetime_var}",
                "Extracted day of month (1-31) from {datetime_var}")
            update_output(self$results$hour_out,
                "Hour - from {datetime_var}",
                "Extracted hour component (0-23) from {datetime_var}")
            update_output(self$results$minute_out,
                "Minute - from {datetime_var}",
                "Extracted minute component (0-59) from {datetime_var}")
            update_output(self$results$second_out,
                "Second - from {datetime_var}",
                "Extracted second component (0-59) from {datetime_var}")
            update_output(self$results$dayname_out,
                "Day Name - from {datetime_var}",
                "Extracted day of week name from {datetime_var}")
            update_output(self$results$weeknum_out,
                "Week Number - from {datetime_var}",
                "Extracted week number of year (1-53) from {datetime_var}")
            update_output(self$results$quarter_out,
                "Quarter - from {datetime_var}",
                "Extracted quarter (1-4) from {datetime_var}")
            update_output(self$results$dayofyear_out,
                "Day of Year - from {datetime_var}",
                "Extracted day of year (1-366) from {datetime_var}")
        },

        # Generate HTML preview table showing conversion results
        # @param original Original display values
        # @param parsed Parsed POSIXct datetime values
        # @param n Number of rows to display
        # @return HTML string containing formatted preview table
        .generatePreviewTable = function(original, parsed, n = 50) {
            # Generate HTML preview table

            n_show <- min(n, length(original))

            if (n_show == 0) {
                return("<p>No data to preview.</p>")
            }

            table_html <- paste0(
                "<div style='background-color: rgba(138, 155, 172, 0.06); padding: 15px; border-radius: 8px; overflow-x: auto; color: inherit;'>",
                "<p><strong>Showing first ", n_show, " of ", length(original), " observations</strong></p>",
                "<table style='width: 100%; border-collapse: collapse; font-size: 12px;'>",
                "<thead><tr style='background-color: #6c757d; color: #ffffff;'>",
                "<th style='padding: 6px; border: 1px solid #dee2e6;'>Row</th>",
                "<th style='padding: 6px; border: 1px solid #dee2e6;'>Original Value</th>",
                "<th style='padding: 6px; border: 1px solid #dee2e6;'>Converted DateTime</th>",
                "<th style='padding: 6px; border: 1px solid #dee2e6;'>Status</th>",
                "</tr></thead><tbody>"
            )

            for (i in 1:n_show) {
                row_bg <- if (i %% 2 == 0) "transparent" else "rgba(138, 155, 172, 0.07)"

                original_val <- if (is.na(original[i])) {
                    "<em>NA</em>"
                } else {
                    htmltools::htmlEscape(as.character(original[i]))
                }

                parsed_val <- if (is.na(parsed[i])) {
                    "<em>NA</em>"
                } else {
                    as.character(parsed[i])
                }

            # Status: success if parsed is not NA (unless original was NA)
            if (is.na(original[i])) {
                status <- "<span style='opacity: 0.6;'>-</span>"
            } else if (is.na(parsed[i])) {
                # The glyph was empty: an earlier non-ASCII sweep stripped the tick and
                # cross and left the coloured spans behind, so the Status column showed
                # nothing at all for both success and failure. Words, not symbols -- they
                # survive any encoding pass, screen-read correctly, and do not rely on
                # colour alone (the failed row's tint is the only other signal).
                status <- "<span style='font-weight: bold;'>Failed</span>"
                row_bg <- "rgba(220, 53, 69, 0.14)"
            } else {
                status <- "<span style='font-weight: bold; opacity: 0.85;'>OK</span>"
            }

                table_html <- paste0(table_html,
                    "<tr style='background-color: ", row_bg, "; color: inherit;'>",
                    "<td style='padding: 6px; border: 1px solid #dee2e6;'>", i, "</td>",
                    "<td style='padding: 6px; border: 1px solid #dee2e6;'>", original_val, "</td>",
                    "<td style='padding: 6px; border: 1px solid #dee2e6;'>", parsed_val, "</td>",
                    "<td style='padding: 6px; border: 1px solid #dee2e6; text-align: center;'>", status, "</td>",
                    "</tr>"
                )
            }

            table_html <- paste0(table_html, "</tbody></table></div>")

            return(table_html)
        },

        # Generate HTML preview of extracted datetime components
        # @param components List of extracted components (year, month, day, etc.)
        # @param n Number of rows to display
        # @return HTML string containing formatted component preview table
        .generateComponentPreview = function(components, n = 50) {
            # Generate HTML preview of extracted components

            if (length(components) == 0) {
                return("<p>No components selected for extraction.</p>")
            }

            n_show <- min(n, length(components[[1]]))

            table_html <- paste0(
                "<div style='background-color: rgba(138, 155, 172, 0.06); padding: 15px; border-radius: 8px; overflow-x: auto; color: inherit;'>",
                "<p><strong>Showing first ", n_show, " of ", length(components[[1]]), " observations</strong></p>",
                "<table style='width: 100%; border-collapse: collapse; font-size: 12px;'>",
                "<thead><tr style='background-color: #6c757d; color: #ffffff;'>",
                "<th style='padding: 6px; border: 1px solid #dee2e6;'>Row</th>"
            )

            # Add column headers for each component
            for (comp_name in names(components)) {
                display_name <- switch(comp_name,
                    "year" = "Year",
                    "month" = "Month",
                    "monthname" = "Month Name",
                    "day" = "Day",
                    "hour" = "Hour",
                    "minute" = "Minute",
                    "second" = "Second",
                    "dayname" = "Day Name",
                    "weeknum" = "Week #",
                    "quarter" = "Quarter",
                    "dayofyear" = "Day of Year",
                    comp_name
                )
                table_html <- paste0(table_html,
                    "<th style='padding: 6px; border: 1px solid #dee2e6;'>", display_name, "</th>"
                )
            }

            table_html <- paste0(table_html, "</tr></thead><tbody>")

            # Add data rows
            for (i in 1:n_show) {
                row_bg <- if (i %% 2 == 0) "transparent" else "rgba(138, 155, 172, 0.07)"

                table_html <- paste0(table_html,
                    "<tr style='background-color: ", row_bg, "; color: inherit;'>",
                    "<td style='padding: 6px; border: 1px solid #dee2e6;'>", i, "</td>"
                )

                for (comp_name in names(components)) {
                    comp_val <- if (is.na(components[[comp_name]][i])) {
                        "<em>NA</em>"
                    } else {
                        as.character(components[[comp_name]][i])
                    }

                    table_html <- paste0(table_html,
                        "<td style='padding: 6px; border: 1px solid #dee2e6;'>", comp_val, "</td>"
                    )
                }

                table_html <- paste0(table_html, "</tr>")
            }

            table_html <- paste0(table_html, "</tbody></table></div>")

            return(table_html)
        },

        # ===================================================================
        # CLINICIAN-FRIENDLY PANEL METHODS
        # ===================================================================

        .populateQualityAssessment = function(quality, parsed_dates, misuse_warnings = NULL) {
            # Populate quality assessment panel (controlled by checkbox)
            if (!self$options$show_quality_metrics) {
                return()
            }

            # Detect misuse warnings (reuse the value computed in .run when supplied)
            if (is.null(misuse_warnings))
                misuse_warnings <- private$.detectMisuse(parsed_dates)

            # Calculate percentages.
            # Every row of this table is a percentage OF THE TOTAL, so Successful Parses
            # must use the same denominator. It previously printed success_rate, which is
            # over non-missing values, so 50 missing of 100 (50%) sat directly above 50
            # parsed shown as 100% and the column summed to 250%. The non-missing rate is
            # the more useful number, so it is still reported -- as the parse rate below.
            missing_pct <- if (quality$total_observations > 0) {
                round(quality$original_missing / quality$total_observations * 100, 1)
            } else {
                0
            }
            parsed_pct <- if (quality$total_observations > 0) {
                round(quality$successfully_parsed / quality$total_observations * 100, 1)
            } else {
                0
            }

            # PARSE RATE IS NOT DATA QUALITY.
            # success_rate only measures whether lubridate returned a non-NA value. A
            # column read in the wrong format parses at 100% and every date in it is
            # wrong -- a month-coarsened registry column read as year-first was graded
            # "Excellent" while all 40 rows were up to 18 years out. The grade is now
            # withheld whenever a notice is questioning the dates themselves, so it can
            # no longer contradict the warning printed directly above it.
            .grade_unsafe <- any(vapply(private$.noticeList, function(n)
                identical(n$class, "date-suspect"), logical(1)))
            quality_summary <- if (is.na(quality$success_rate)) {
                "N/A (no non-missing values)"
            } else if (.grade_unsafe) {
                sprintf("%s%% of non-missing values produced a date, but see the warnings above: a column read in the wrong format still parses at a high rate.",
                        base::format(round(quality$success_rate, 1)))
            } else if (quality$success_rate >= 95) {
                "Excellent (\u226595% success)"
            } else if (quality$success_rate >= 80) {
                "Good (80-94% success)"
            } else if (quality$success_rate >= 50) {
                "Fair (50-79% success)"
            } else {
                "Poor (<50% success)"
            }

            # Conditional styling
            # The literals used to carry their own quotes into an already single-quoted
            # style attribute, emitting  style='background-color: '#fff3cd''  -- the
            # attribute terminated at the second quote, so the row highlighting silently
            # did nothing at all. Fixed, and moved to translucent tints + color: inherit
            # so restoring it does not create three new opaque light rows that are
            # unreadable in jamovi's dark theme.
            missing_bg <- if (quality$original_missing > 0)
                "rgba(255, 202, 33, 0.16); color: inherit;" else "transparent;"
            parsing_bg <- if (quality$failed_parsing > 0)
                "rgba(220, 53, 69, 0.14); color: inherit;" else "rgba(40, 167, 69, 0.13); color: inherit;"

            quality_html <- glue::glue("
                <div style='background-color: rgba(33, 137, 255, 0.07); padding: 15px; border-left: 4px solid #2196F3; color: inherit;'>
                    <h4 style='margin-top: 0;'> Data Quality Assessment</h4>

                    <table style='width: 100%; border-collapse: collapse; margin-top: 10px;'>
                        <tr style='background-color: rgba(33, 152, 239, 0.13); color: inherit;'>
                            <th style='padding: 8px; text-align: left; border: 1px solid #90caf9;'>Metric</th>
                            <th style='padding: 8px; text-align: right; border: 1px solid #90caf9;'>Count</th>
                            <th style='padding: 8px; text-align: right; border: 1px solid #90caf9;'>%</th>
                        </tr>
                        <tr>
                            <td style='padding: 8px; border: 1px solid #90caf9;'>Total Observations</td>
                            <td style='padding: 8px; text-align: right; border: 1px solid #90caf9;'>{quality$total_observations}</td>
                            <td style='padding: 8px; text-align: right; border: 1px solid #90caf9;'>100%</td>
                        </tr>
                        <tr style='background-color: {missing_bg}'>
                            <td style='padding: 8px; border: 1px solid #90caf9;'>Missing Values</td>
                            <td style='padding: 8px; text-align: right; border: 1px solid #90caf9;'>{quality$original_missing}</td>
                            <td style='padding: 8px; text-align: right; border: 1px solid #90caf9;'>{missing_pct}%</td>
                        </tr>
                        <tr style='background-color: {parsing_bg}'>
                            <td style='padding: 8px; border: 1px solid #90caf9;'>Successful Parses</td>
                            <td style='padding: 8px; text-align: right; border: 1px solid #90caf9;'>{quality$successfully_parsed}</td>
                            <td style='padding: 8px; text-align: right; border: 1px solid #90caf9;'>{parsed_pct}%</td>
                        </tr>
                        <tr>
                            <td style='padding: 8px; border: 1px solid #90caf9;'>Failed Parsing</td>
                            <td style='padding: 8px; text-align: right; border: 1px solid #90caf9;'>{quality$failed_parsing}</td>
                            <td style='padding: 8px; text-align: right; border: 1px solid #90caf9;'>-</td>
                        </tr>
                    </table>

                    <p style='margin-top: 10px;'><strong>Parse rate:</strong> {quality_summary}</p>
                    {if (length(misuse_warnings) > 0) paste0('<div style=\"background-color: rgba(255, 202, 33, 0.23); color: inherit; padding: 10px; margin-top: 10px; border-left: 3px solid #ffc107;\"><strong> Warnings:</strong><ul>', paste0('<li>', misuse_warnings, '</li>', collapse=''), '</ul></div>') else ''}
                </div>
            ")

            self$results$qualityAssessment$setContent(quality_html)
        },

        .populateNaturalLanguageSummary = function(datetime_var, detected_format, quality, components, timezone_label = NULL) {
            # Populate natural-language summary (controlled by checkbox)
            if (!self$options$show_summary) {
                return()
            }

            # Count extractions created
            extractions <- c()
            if (self$options$extract_year || self$options$year_out) extractions <- c(extractions, "Year")
            if (self$options$extract_month || self$options$month_out) extractions <- c(extractions, "Month")
            if (self$options$extract_monthname || self$options$monthname_out) extractions <- c(extractions, "Month Name")
            if (self$options$extract_day || self$options$day_out) extractions <- c(extractions, "Day")
            if (self$options$extract_hour || self$options$hour_out) extractions <- c(extractions, "Hour")
            if (self$options$extract_minute || self$options$minute_out) extractions <- c(extractions, "Minute")
            if (self$options$extract_second || self$options$second_out) extractions <- c(extractions, "Second")
            if (self$options$extract_dayname || self$options$dayname_out) extractions <- c(extractions, "Day Name")
            if (self$options$extract_weeknum || self$options$weeknum_out) extractions <- c(extractions, "Week Number")
            if (self$options$extract_quarter || self$options$quarter_out) extractions <- c(extractions, "Quarter")
            if (self$options$extract_dayofyear || self$options$dayofyear_out) extractions <- c(extractions, "Day of Year")

            extraction_text <- if (length(extractions) > 0) {
                paste(extractions, collapse = ", ")
            } else {
                "none (preview only)"
            }

            tz_display <- timezone_label %||% if (self$options$timezone == "utc") "UTC" else Sys.timezone()
            if (is.null(tz_display) || is.na(tz_display) || tz_display == "")
                tz_display <- "System default"

            datetime_var_safe <- htmltools::htmlEscape(datetime_var)
            tz_display_safe <- htmltools::htmlEscape(tz_display)

            # ONE DENOMINATOR. success_rate is over NON-MISSING values, and pairing it
            # with total_observations produced "50/100 (100%)" -- and a copy-ready
            # sentence claiming "100 datetime values ... 100% successful parsing" when
            # there were 50 values and 50 missing. This sentence is offered for pasting
            # into a manuscript, so it has to be arithmetically true on its own.
            n_values <- quality$total_observations - quality$original_missing
            missing_clause <- if (quality$original_missing > 0)
                sprintf("; %d of the %d rows were missing",
                        quality$original_missing, quality$total_observations) else ""
            summary_html <- glue::glue("
                <div style='background-color: rgba(33, 137, 255, 0.07); padding: 15px; border: 1px solid #b3d9ff; border-radius: 5px; color: inherit;'>
                    <h4 style='margin-top: 0;'> Analysis Summary</h4>
                    <p><strong>Source column:</strong> {datetime_var_safe}</p>
                    <p><strong>Format detected/used:</strong> {detected_format}</p>
                    <p><strong>Timezone:</strong> {tz_display_safe}</p>
                    <p><strong>Successful conversions:</strong> {quality$successfully_parsed}/{n_values} non-missing ({round(quality$success_rate, 1)}%)</p>
                    <p><strong>Components extracted:</strong> {extraction_text}</p>

                    <div style='background-color: rgba(33, 149, 188, 0.1); padding: 10px; margin-top: 15px; border-radius: 3px; color: inherit;'>
                        <p style='margin: 0;'><strong> Copy-Ready Summary:</strong></p>
                        <p style='font-family: monospace; font-size: 0.9em; margin: 10px 0;'>
                        We extracted {extraction_text} from {n_values} datetime values in column '{datetime_var_safe}' using {detected_format} format, with {round(quality$success_rate, 1)}% producing a valid date{missing_clause}.
                        </p>
                    </div>
                </div>
            ")

            self$results$nlSummary$setContent(summary_html)
        },

        .populateExplanatoryPanels = function() {
            # Populate about and caveats panels (both controlled by show_explanations checkbox)
            if (!self$options$show_explanations) {
                return()
            }

            # About Panel
            about_html <- "
            <div style='background-color: rgba(155, 155, 155, 0.06); padding: 15px; border-radius: 5px; color: inherit;'>
                <h4 style='margin-top: 0;'> What This Function Does</h4>
                <p>The DateTime Converter extracts components (year, month, day, hour, etc.)
                from datetime columns and creates new variables for downstream analysis.</p>

                <h4>When to Use</h4>
                <ul>
                    <li><strong>Cohort stratification:</strong> Extract year to group patients by diagnosis year</li>
                    <li><strong>Seasonal analysis:</strong> Extract month/quarter for temporal patterns</li>
                    <li><strong>Temporal patterns:</strong> Extract day-of-week for treatment schedules</li>
                    <li><strong>Data cleaning:</strong> Convert Excel dates to standardized format</li>
                    <li><strong>Time calculations:</strong> Extract components for survival time calculations</li>
                </ul>

                <h4>Typical Outputs</h4>
                <p>New columns containing numeric or text representations of datetime components.
                These can be used for grouping, filtering, or creating time-based variables.</p>

                <h4>Clinical Examples</h4>
                <ul>
                    <li>Extract year from diagnosis date to study temporal trends in cancer incidence</li>
                    <li>Extract month to analyze seasonal variation in disease presentation</li>
                    <li>Extract day-of-week to study weekend vs weekday treatment outcomes</li>
                </ul>
            </div>
            "
            self$results$aboutPanel$setContent(about_html)

            # Caveats Panel
            caveats_html <- "
            <div style='background-color: rgba(255, 202, 33, 0.23); padding: 15px; border-left: 4px solid #ffc107; border-radius: 3px; color: inherit;'>
                <h4 style='margin-top: 0;'> Important Considerations</h4>
                <ul>
                    <li><strong>Format matching:</strong> Date format selection must match your data.
                    Incorrect format leads to parsing failures or wrong dates.</li>

                    <li><strong>Timezone consistency:</strong> UTC vs system timezone affects time extraction.
                    Use UTC for international studies, system timezone for local data.</li>

                    <li><strong>Two-digit years:</strong> a year written as two digits is read with the
                    standard 1969-2068 pivot, so 55 becomes 1955 but 37 becomes 2037. For dates of birth,
                    diagnosis or specimen collection - which cannot be in the future - set
                    <em>Two-digit years</em> to <em>Always in the past</em>. Four-digit years are never
                    affected.</li>

                    <li><strong>Week number is not the ISO week:</strong> weeks are counted in 7-day blocks
                    from 1 January, so 1-7 January is always week 1 and week 53 is a 1-2 day stub. This
                    differs from ISO-8601 and from the MMWR/epidemiological week on about a quarter of all
                    days, and the two disagree most around the new year: 2021-01-01 is week 1 here but ISO
                    week 53 of 2020. Do not use it where a reporting standard requires ISO or epi weeks.</li>

                    <li><strong>Numeric datetime (Unix epoch):</strong> The corrected datetime numeric output
                    represents seconds since 1970-01-01 00:00:00 UTC. This value is:
                        <ul>
                            <li> OS-independent (same on Windows, Mac, Linux)</li>
                            <li><strong>Dependent on the Timezone setting above.</strong> A date without a
                            time is midnight <em>in the selected timezone</em>, so the same input yields a
                            different number under different settings: <code>2024-01-15</code> becomes
                            1705276800 under UTC but 1705266000 under Europe/Istanbul (3 hours earlier).
                            The default is your machine's timezone, so two collaborators on different
                            machines will not get identical numbers. <strong>Set Timezone to UTC if the
                            numeric value must be reproducible across machines</strong> or is going to be
                            compared against an external source.</li>
                            <li> Suitable for calculations and comparisons</li>
                            <li> Very large numbers (billions) - use scientific notation if needed</li>
                        </ul>
                    </li>

                    <li><strong>Excel serial dates:</strong> Different epochs exist:
                        <ul>
                            <li>Windows Excel (1900 system): serial 1 is nominally 1900-01-01, but Excel
                            wrongly treats 1900 as a leap year, so conversion uses the origin 1899-12-30.
                            Dates from 1900-03-01 (serial 61) onward are exact; serials 1-59 land one day
                            earlier than Excel shows and serial 60 is Excel's non-existent 1900-02-29.
                            This matches how readxl and openxlsx convert, and does not affect any
                            realistic clinical date.</li>
                            <li>Mac Excel 2008 and earlier (1904 system): origin 1904-01-01, exactly
                            1462 days later than the 1900 system. Serials from the two systems look
                            identical, so auto-detection always assumes 1900; choose the DateTime Format
                            'Excel Serial (Days since 1904, legacy Mac)' if your file came from an old
                            Mac Excel.</li>
                        </ul>
                    </li>

                    <li><strong>Missing values:</strong> Invalid dates become NA in output columns.
                    Check quality metrics to ensure acceptable parsing success rate.</li>

                    <li><strong>Daylight saving time:</strong> Can cause ambiguous or non-existent times.
                    Consider using UTC for medical studies to avoid DST complications.</li>

                    <li><strong>Leap years and leap seconds:</strong> Automatically handled by lubridate,
                    but be aware when working with precise time calculations.</li>
                </ul>

                <p style='margin-top: 15px;'><strong> Best Practice:</strong> Always review the
                Quality Assessment and Preview tables before adding columns to your dataset.</p>
            </div>
            "
            self$results$caveatsPanel$setContent(caveats_html)
        },

        .populateGlossary = function() {
            # Populate glossary panel (controlled by checkbox)
            if (!self$options$show_glossary) {
                return()
            }

            glossary_html <- "
            <div style='background-color: rgba(153, 33, 170, 0.12); padding: 15px; border-radius: 5px; color: inherit;'>
                <h4 style='margin-top: 0;'> Key Terms & Concepts</h4>
                <dl style='line-height: 1.6;'>
                    <dt style='font-weight: bold; margin-top: 10px;'>Excel Serial Date</dt>
                    <dd style='margin-left: 20px;'>Number of days counted from Excel's origin: the 1900
                    system (Windows) or the 1904 system (Mac). Example: serial 45000 is
                    <strong>15 March 2023</strong>. Because Excel's 1900 system wrongly counts
                    1900-02-29, conversion uses the origin 1899-12-30; that is exact for every date
                    from 1900-03-01 onward. Commonly used when exporting data from Excel to text files.</dd>

                    <dt style='font-weight: bold; margin-top: 10px;'>Unix Epoch</dt>
                    <dd style='margin-left: 20px;'>Seconds since January 1, 1970 00:00:00 UTC.
                    Example: 1609459200 represents January 1, 2021 00:00:00 UTC. The number identifies an
                    <strong>instant</strong>, so it is OS-independent - but converting a date to that
                    instant requires a timezone, and this module uses the Timezone option (your machine's
                    zone by default). The same written date therefore produces different numbers under
                    different Timezone settings; choose UTC when the value must be reproducible across
                    machines. Used in databases, programming systems, and recommended for datetime
                    calculations.</dd>

                    <dt style='font-weight: bold; margin-top: 10px;'>ISO 8601</dt>
                    <dd style='margin-left: 20px;'>International standard date format: YYYY-MM-DD or
                    YYYY-MM-DD HH:MM:SS. Recommended for data exchange as it's unambiguous across
                    cultures and time zones.</dd>

                    <dt style='font-weight: bold; margin-top: 10px;'>UTC (Coordinated Universal Time)</dt>
                    <dd style='margin-left: 20px;'>Global time standard with no daylight saving
                    adjustments. Recommended for multi-center studies and international collaborations
                    to avoid timezone confusion.</dd>

                    <dt style='font-weight: bold; margin-top: 10px;'>System Timezone</dt>
                    <dd style='margin-left: 20px;'>Your computer's local timezone setting, which may
                    include daylight saving time adjustments. Appropriate for local studies within
                    a single timezone.</dd>

                    <dt style='font-weight: bold; margin-top: 10px;'>POSIXct</dt>
                    <dd style='margin-left: 20px;'>R's internal datetime format storing time as seconds
                    since Unix epoch. Allows efficient date/time calculations.</dd>

                    <dt style='font-weight: bold; margin-top: 10px;'>Parsing</dt>
                    <dd style='margin-left: 20px;'>Converting text or numeric representations of dates
                    into standardized datetime objects that can be manipulated and analyzed.</dd>

                    <dt style='font-weight: bold; margin-top: 10px;'>Quarter</dt>
                    <dd style='margin-left: 20px;'>Three-month period used in financial and medical
                    reporting: Q1 (Jan-Mar), Q2 (Apr-Jun), Q3 (Jul-Sep), Q4 (Oct-Dec).</dd>
                                <dt><b>Week number (as used here)</b></dt>
                <dd>The day of the year divided into 7-day blocks counting from 1 January, i.e. lubridate::week(). It is NOT the ISO-8601 week and NOT the MMWR/epidemiological week; those start on a fixed weekday and can place late-December dates in week 1 of the following year. Use ISO or epi weeks if your reporting standard requires them.</dd>

                <dt><b>Excel serial date</b></dt>
                <dd>A date stored as a count of days. Windows Excel counts from 30 December 1899; legacy Mac Excel (2008 and earlier) counts from 1 January 1904, exactly 1462 days later. The number itself does not record which system produced it, so this analysis assumes the 1900 system and says so.</dd>

            </dl>
            </div>
            "
            self$results$glossaryPanel$setContent(glossary_html)
        },

        .detectMisuse = function(parsed_dates) {
            # Detect potential misuse patterns and return warnings
            warnings <- c()

            # Check if numeric column selected for text parsing
            datetime_var <- self$options$datetime_var
            original_vector <- self$data[[datetime_var]]

            if (!(self$options$datetime_format %in% c("auto", "excel_serial", "excel_serial_1904", "unix_epoch")) &&
                is.numeric(original_vector) && !all(is.na(original_vector))) {
                warnings <- c(warnings,
                    .("A text date format is selected but this column is numeric. \u2022 Choose Excel Serial or Unix Epoch if these really are serial numbers, or select a text date column instead."))
            }

            # Check for dates before 1900 (unusual in medical data)
            if (!is.null(parsed_dates) && any(!is.na(parsed_dates))) {
                years <- lubridate::year(parsed_dates)
                if (any(years < 1900, na.rm = TRUE)) {
                    count_old <- sum(years < 1900, na.rm = TRUE)
                    warnings <- c(warnings, .fmt(
                        .("{count} date(s) fall before 1900, which usually means the format was read wrongly or the source has data-entry errors."),
                        count = count_old))
                }

                # Check for future dates.
                # This is the primary - often only - signal that a two-digit-year column
                # has been pivoted into the next century, so when that is the likely
                # cause say so instead of offering the benign explanation. A DOB column
                # written dd/mm/yy puts every birth before 1969 exactly 100 years ahead,
                # and the old text ("verify this is intentional, e.g. planned follow-up
                # dates") pointed the reader away from the real problem.
                future_dates <- parsed_dates > Sys.time()
                if (any(future_dates, na.rm = TRUE)) {
                    count_future <- sum(future_dates, na.rm = TRUE)
                    n_dated <- sum(!is.na(parsed_dates))
                    frac_future <- if (n_dated > 0) count_future / n_dated else 0
                    private$.pivotSuspected <- isTRUE(private$.twoDigitYearSource) && frac_future > 0.05
                    warnings <- if (private$.pivotSuspected) {
                        c(warnings, .fmt(
                            .("{count} of {total} dates ({pct}%) are in the future, and this column has two-digit years: a year such as 55 is read as 2055, not 1955. If these dates cannot be in the future - dates of birth, diagnosis or specimen dates - set Two-digit years to 'Always in the past'."),
                            count = count_future, total = n_dated,
                            pct = base::format(round(frac_future * 100, 1))))
                    } else {
                        c(warnings, .fmt(
                            .("{count} date(s) are in the future. Verify this is intentional (for example planned follow-up dates)."),
                            count = count_future))
                    }
                }

                # Check for very wide date range (may indicate mixed formats)
                date_range <- diff(range(parsed_dates, na.rm = TRUE))
                if (!is.na(date_range) && as.numeric(date_range, units = "days") > 36525) { # > 100 years
                    warnings <- c(warnings,
                        .("The converted dates span more than 100 years, which usually means the column mixes two date formats."))
                }
            }

            return(warnings)
        },

        # ===================================================================
        # MAIN RUN METHOD
        # ===================================================================

        .run = function() {

            # Notices accumulate on the instance, which persists across run cycles;
            # without this reset the same notice is re-rendered once per option change.
            private$.noticeList <- list()

            # Same hazard, different field: .twoDigitYearSource is set only on the
            # text-parsing branch, so a later run on a POSIXct or Excel-serial column
            # never reaches the assignment and would inherit the previous run's TRUE --
            # making the future-date notice blame a century pivot on a column that has
            # no two-digit years at all. Reset every field that survives a run here.
            private$.twoDigitYearSource <- FALSE
            private$.pivotSuspected <- FALSE

            # Show welcome message if no variable selected
            if (is.null(self$options$datetime_var) || self$options$datetime_var == "") {
                welcome_msg <- "
                <div style='background-color: rgba(33, 152, 239, 0.13); padding: 20px; border-radius: 8px; margin: 20px 0; color: inherit;'>
                <h3 style='margin-top: 0;'> Welcome to DateTime Converter!</h3>
                <p><strong>Convert datetime variables and extract components for analysis</strong></p>

                <h4>Quick Start:</h4>
                <ol>
                <li><strong>Select DateTime Variable:</strong> Choose a column containing datetime information</li>
                <li><strong>Choose Format:</strong> Auto-detect or manually specify the datetime format</li>
                <li><strong>Extract Components:</strong> Select which datetime components to extract</li>
                <li><strong>Review Preview:</strong> Check conversion quality before adding to dataset</li>
                </ol>

                <h4>Features:</h4>
                <ul>
                <li><strong>Automatic Format Detection:</strong> Intelligently identifies datetime format</li>
                <li><strong>Quality Assessment:</strong> Min/max values, success rate, missing data</li>
                <li><strong>Component Extraction:</strong> Year, month, day, hour, minute, day name, week number, quarter, etc.</li>
                <li><strong>Preview Before Adding:</strong> See converted data before adding to dataset</li>
                </ul>

                <h4>Supported Formats:</h4>
                <ul>
                <li>YYYY-MM-DD HH:MM:SS (ISO standard with time)</li>
                <li>YYYY-MM-DD (ISO date)</li>
                <li>DD-MM-YYYY or DD/MM/YYYY (European format)</li>
                <li>MM-DD-YYYY or MM/DD/YYYY (US format)</li>
                <li>And many more variations...</li>
                </ul>

                <p style='font-size: 12px; opacity: 0.75; margin-top: 20px;'>
                 <em>Perfect for preparing temporal data for survival analysis, time series, and longitudinal studies</em>
                </p>
                </div>"

                self$results$welcome$setContent(welcome_msg)
                return()
            }

            # Get data
            data <- self$data
            datetime_var <- self$options$datetime_var
            if (length(datetime_var) > 1)
                datetime_var <- datetime_var[1]

            private$.updateOutputTitles(datetime_var)

            # Validate datetime variable exists in dataset
            # (null/empty selection is already handled by the welcome-message guard above)
            if (!datetime_var %in% names(data)) {
                available_vars <- names(data)
                available_preview <- if (length(available_vars) > 10) {
                    paste(paste(head(available_vars, 10), collapse = ", "), "...")
                } else {
                    paste(available_vars, collapse = ", ")
                }

                private$.addNotice(
                    type = "ERROR",
                    title = .("Variable Not Found"),
                    content = .fmt(
                        .("Selected variable {name} was not found in the dataset. \u2022 The column may have been renamed or removed. \u2022 Please select a different variable from the left panel. \u2022 Available variables: {available}"),
                        name = datetime_var, available = available_preview)
                )
                private$.renderNotices()
                return()
            }

            if (nrow(data) == 0) {
                private$.addNotice(
                    type = "ERROR",
                    title = .("Empty Dataset"),
                    content = 'Dataset contains no rows. \u2022 Please ensure your dataset has at least one observation. \u2022 Check for data loading or filtering issues.'
                )
                private$.renderNotices()
                return()
            }

            # Prepare datetime values for parsing
            datetime_vector <- data[[datetime_var]]
            if (all(is.na(datetime_vector))) {
                private$.addNotice(
                    type = "ERROR",
                    title = .("All Values Missing"),
                    content = .fmt(
                        .("All values in {name} are missing (NA). \u2022 Please select a column with valid datetime entries before proceeding."),
                        name = datetime_var)
                )
                private$.renderNotices()
                return()
            }

            prepared <- private$.prepareDatetimeInput(datetime_vector)

            original_display <- prepared$original_display
            parsing_vector <- prepared$parsing_vector
            quality_vector <- prepared$quality_vector
            preprocessing_notes <- prepared$notes
            format_warnings <- character()

            # Detect or use specified format
            # Determine timezone to use
            tz_info <- private$.resolveTimezone()
            tz_to_use <- tz_info$tz

            if (prepared$already_parsed) {
                parsed_dates <- prepared$parsed_dates
                detected_format <- if (!is.null(prepared$format_hint)) {
                    prepared$format_hint
                } else {
                    "preparsed"
                }
                # Only for TRUE POSIXct/Date input. The numeric force_format branches also
                # return already_parsed = TRUE, and for those the selection was honoured,
                # not ignored -- the note contradicted the "Forced ... interpretation"
                # line printed directly beside it.
                if (self$options$datetime_format != "auto" &&
                    !identical(prepared$format_hint, self$options$datetime_format)) {
                    preprocessing_notes <- c(preprocessing_notes, .fmt(
                        .("Manual format selection ({fmt}) was ignored because the column is already stored as datetime values."),
                        fmt = private$.formatLabel(self$options$datetime_format)))
                }
            } else {
                if (self$options$datetime_format == "auto") {
                    detection <- private$.detectDatetimeFormat(parsing_vector)
                    detected_format <- detection$format
                    format_warnings <- c(format_warnings, detection$warnings %||% character())
                } else {
                    detected_format <- self$options$datetime_format
                }
                if (detected_format %in% c("unsure", "excel_serial", "excel_serial_1904", "unix_epoch")) {
                    # Two ways to arrive here, both needing the same NA branch.
                    #
                    # "unsure": auto-detection could not resolve a format and the
                    # "Format Detection Failed" WARNING is already queued. Skip
                    # parsing rather than passing "unsure" to .getParser(), which
                    # would stop() and emit a developer-flavoured error notice.
                    #
                    # The three NUMERIC tokens: reaching this line at all means the
                    # numeric path already declined the column (a text column cannot
                    # produce serials), so .getParser() has no parser for them and
                    # would stop() with "Unsupported datetime format: excel_serial".
                    # That raw R error was the visible result of the commonest
                    # mis-click there is -- picking an Excel/Unix format on a text
                    # date column -- so say what happened instead.
                    if (!identical(detected_format, "unsure"))
                        private$.addNotice(
                            type = "WARNING",
                            title = .("Numeric Format Selected For A Text Column"),
                            content = .fmt(
                                .("DateTime Format is set to a numeric format ({fmt}) but this column does not hold numeric serial values, so nothing could be converted. \u2022 Choose a text format such as YYYY-MM-DD or DD-MM-YYYY, or select Auto-detect."),
                                fmt = private$.formatLabel(detected_format)))
                    parsed_dates <- as.POSIXct(rep(NA_real_, length(parsing_vector)),
                                               origin = "1970-01-01", tz = tz_to_use)
                } else {
                    parsed_dates <- private$.parseDatetime(parsing_vector, detected_format, tz = tz_to_use)

                    # TWO-DIGIT YEARS AND THE CENTURY PIVOT.
                    # lubridate pivots 00-68 to 20xx and 69-99 to 19xx, with no way to
                    # change it. For a date-of-birth column written dd/mm/yy that puts
                    # every birth before 1969 exactly 100 years in the FUTURE -- measured
                    # 167 of 200 rows on a realistic 1935-1975 cohort, each off by 36525
                    # days. The column parses at 100% and was graded "Excellent".
                    # `two_digit_year = 'past'` moves any such date back a century.
                    # Gated on the source having NO four-digit run, so a column with real
                    # four-digit years (where a future date is usually a planned visit)
                    # is never touched.
                    private$.twoDigitYearSource <- private$.hasTwoDigitYear(parsing_vector)
                    if (identical(self$options$two_digit_year, "past") &&
                        !private$.twoDigitYearSource) {
                        # The gate is all-or-nothing over the column, so ONE value with a
                        # four-digit run -- including free text like "unknown (2019 chart)"
                        # that never parses -- turns the setting the user just chose into a
                        # no-op. Silence there is the worst outcome: they believe the
                        # correction was applied.
                        .four <- as.character(parsing_vector)
                        .four <- .four[!is.na(.four) & grepl("[0-9]{4}", .four)]
                        private$.addNotice(
                            type = "WARNING",
                            title = .("Two-digit-year Correction Not Applied"),
                            content = .fmt(
                                .("Two-digit years is set to 'Always in the past', but {count} value(s) in this column contain a four-digit year (first: {example}), so the column is not a two-digit-year column and nothing was changed. \u2022 Remove or correct those values if the rest of the column really does use two-digit years."),
                                count = length(.four),
                                example = if (length(.four)) .four[1] else ""))
                    }
                    if (identical(self$options$two_digit_year, "past") &&
                        private$.twoDigitYearSource) {
                        .future <- !is.na(parsed_dates) & parsed_dates > Sys.time()
                        if (any(.future)) {
                            parsed_dates[.future] <- parsed_dates[.future] - lubridate::years(100)
                            preprocessing_notes <- c(preprocessing_notes, .fmt(
                                .("Two-digit years: {count} date(s) would have fallen in the future and were moved back one century."),
                                count = sum(.future)))
                            # .parseDatetime() raised "Implausible Dates Detected" on the
                            # PRE-shift dates, and it is never retracted -- so the module
                            # corrected the dates and then went on telling the user they
                            # were wrong, and the quality grade stayed withheld on their
                            # account. Drop it if the shift resolved every offending row.
                            .yrs <- lubridate::year(parsed_dates)
                            if (!any(!is.na(.yrs) & (.yrs < 1900 |
                                    .yrs > as.integer(base::format(Sys.Date() + 365, "%Y")))))
                                private$.noticeList <- Filter(function(n)
                                    !identical(n$title, .("Implausible Dates Detected")),
                                    private$.noticeList)
                        }
                    }
                }
            }

            # Assess quality
            quality <- private$.assessQuality(quality_vector, parsed_dates)

            # Add quality threshold notices
            if (!is.na(quality$success_rate) && quality$success_rate < 85) {
                severity <- if (quality$success_rate < 70) {
                    "STRONG_WARNING"
                } else {
                    "WARNING"
                }

                if (quality$success_rate < 70) {
                    private$.addNotice(
                        type = severity,
                        title = .("Low Parsing Success Rate"),
                        content = .fmt(
                            .("Low datetime parsing success rate: {pct}% \u2022 Only {parsed} of {total} non-missing values were successfully parsed. \u2022 This may indicate incorrect format selection or data quality issues. \u2022 Try a different datetime format, review the failed samples in the Quality Assessment panel, and check the data source for systematic errors. \u2022 Clinical analysis may be unreliable below 70% success."),
                            pct = base::format(round(quality$success_rate, 1)),
                            parsed = quality$successfully_parsed,
                            total = quality$total_observations - quality$original_missing)
                    )
                } else {
                    private$.addNotice(
                        type = severity,
                        title = .("Moderate Parsing Success Rate"),
                        content = .fmt(
                            .("Moderate datetime parsing success rate: {pct}% \u2022 {parsed} of {total} non-missing values were successfully parsed. \u2022 Review the failed samples in the Quality Assessment panel. \u2022 Consider specifying the format manually if auto-detection is incorrect."),
                            pct = base::format(round(quality$success_rate, 1)),
                            parsed = quality$successfully_parsed,
                            total = quality$total_observations - quality$original_missing)
                    )
                }
            }

            # Add misuse warnings as INFO notices
            misuse_warnings <- private$.detectMisuse(parsed_dates)
            if (length(misuse_warnings) > 0) {
                # A two-digit-year column pivoted into the next century used to be
                # reported at INFO severity, rendered in the same blue as the
                # "Conversion Completed" success notice directly below it.
                .pivot_suspected <- isTRUE(private$.twoDigitYearSource) &&
                    isTRUE(private$.pivotSuspected)
                private$.addNotice(
                    type = if (.pivot_suspected) "STRONG_WARNING" else "INFO",
                    class = if (.pivot_suspected) "date-suspect" else NULL,
                    title = if (.pivot_suspected)
                        .("Dates Read Into The Wrong Century") else .("Potential Data Quality Issues"),
                    content = paste(misuse_warnings, collapse = ' \u2022 ')
                )
            }

            # Generate format info
            format_display <- switch(detected_format,
                "ymd_hms" = "YYYY-MM-DD HH:MM:SS",
                "dmy_hms" = "DD-MM-YYYY HH:MM:SS",
                "mdy_hms" = "MM-DD-YYYY HH:MM:SS",
                "ymd_hm" = "YYYY-MM-DD HH:MM",
                "dmy_hm" = "DD-MM-YYYY HH:MM",
                "mdy_hm" = "MM-DD-YYYY HH:MM",
                "ymdhms" = "YYYY-MM-DD HH:MM:SS",
                "dmyhms" = "DD-MM-YYYY HH:MM:SS",
                "mdyhms" = "MM-DD-YYYY HH:MM:SS",
                "ymdhm" = "YYYY-MM-DD HH:MM",
                "dmyhm" = "DD-MM-YYYY HH:MM",
                "mdyhm" = "MM-DD-YYYY HH:MM",
                "ydm" = "YYYY-DD-MM",
                "myd" = "MM-YYYY-DD",
                "dym" = "DD-YYYY-MM",
                "ymd" = "YYYY-MM-DD",
                "dmy" = "DD-MM-YYYY",
                "mdy" = "MM-DD-YYYY",
                "excel_serial" = "Excel Serial (days since 1899-12-30)",
                "excel_serial_1904" = "Excel Serial, legacy Mac (days since 1904-01-01)",
                "unix_epoch" = "Unix Epoch Seconds (since 1970-01-01)",
                "unix_epoch_ms" = "Unix Epoch Milliseconds (since 1970-01-01)",
                "posixct" = "Already formatted POSIXct/POSIXt",
                "date" = "R Date class (converted to midnight)",
                "preparsed" = "Pre-parsed datetime values",
                "unsure" = "Undetermined (please specify the format manually)",
                detected_format
            )

            format_context <- if (prepared$already_parsed) {
                "<p><em>Input was already stored as datetime values; original encoding was preserved.</em></p>"
            } else if (self$options$datetime_format == "auto") {
                "<p><em>Format was automatically detected. If results look incorrect, try manually specifying the format.</em></p>"
            } else {
                paste0("<p><em>Parsing enforced manually using format <code>",
                    self$options$datetime_format, "</code>.</em></p>")
            }

            tz_summary <- tz_info$summary
            note_lines <- preprocessing_notes
            timezone_lines <- character()
            if (detected_format %in% c("excel_serial", "excel_serial_1904", "unix_epoch", "unix_epoch_ms")) {
                timezone_lines <- "Conversions from numeric formats (Excel/Unix) always use UTC regardless of the timezone option."
                tz_summary <- "UTC (numeric conversion)"
            } else if (nzchar(tz_info$note)) {
                # Previously gated on !already_parsed, so a POSIXct column was told
                # nothing about the timezone at all -- the one branch where the option
                # used to be silently ignored.
                timezone_lines <- if (prepared$already_parsed)
                    sub("^String-to-datetime conversions use",
                        "Components were read in", tz_info$note)
                else tz_info$note
            }
            note_lines <- c(note_lines, timezone_lines, format_warnings)
            note_lines <- note_lines[nzchar(note_lines)]

            notes_html <- ""
            if (length(note_lines) > 0) {
                notes_html <- paste0(
                    "<ul style='margin-top: 10px;'>",
                    paste0("<li>", htmltools::htmlEscape(note_lines), "</li>", collapse = ""),
                    "</ul>"
                )
            }

            format_html <- paste0(
                "<div style='background-color: rgba(255, 169, 33, 0.14); padding: 15px; border-radius: 8px; color: inherit;'>",
                "<h4 style='margin-top: 0;'>Format Detection</h4>",
                "<p><strong>Detected/Selected Format:</strong> ", format_display, " (", detected_format, ")</p>",
                format_context,
                notes_html,
                "</div>"
            )
            self$results$formatInfo$setContent(format_html)

            # Generate quality metrics
            success_rate_display <- if (is.na(quality$success_rate)) {
                "N/A"
            } else {
                paste0(quality$success_rate, "%")
            }

            quality_html <- paste0(
                "<div style='background-color: rgba(33, 159, 33, 0.1); padding: 15px; border-radius: 8px; color: inherit;'>",
                "<h4 style='margin-top: 0;'>Quality Assessment</h4>",
                "<table style='width: 100%; border-collapse: collapse;'>",
                "<tr><td style='padding: 6px; border: 1px solid #ddd;'><strong>Total Observations:</strong></td><td style='padding: 6px; border: 1px solid #ddd;'>", quality$total_observations, "</td></tr>",
                "<tr><td style='padding: 6px; border: 1px solid #ddd;'><strong>Originally Missing:</strong></td><td style='padding: 6px; border: 1px solid #ddd;'>", quality$original_missing, "</td></tr>",
                "<tr><td style='padding: 6px; border: 1px solid #ddd;'><strong>Successfully Parsed:</strong></td><td style='padding: 6px; border: 1px solid #ddd;'>", quality$successfully_parsed, " (", success_rate_display, ")</td></tr>",
                "<tr><td style='padding: 6px; border: 1px solid #ddd;'><strong>Failed Parsing:</strong></td><td style='padding: 6px; border: 1px solid #ddd;'>", quality$failed_parsing, "</td></tr>"
            )

            if (!is.na(quality$min_datetime)) {
                quality_html <- paste0(quality_html,
                    "<tr><td style='padding: 6px; border: 1px solid #ddd;'><strong>Minimum DateTime:</strong></td><td style='padding: 6px; border: 1px solid #ddd;'>", quality$min_datetime, "</td></tr>",
                    "<tr><td style='padding: 6px; border: 1px solid #ddd;'><strong>Maximum DateTime:</strong></td><td style='padding: 6px; border: 1px solid #ddd;'>", quality$max_datetime, "</td></tr>",
                    "<tr><td style='padding: 6px; border: 1px solid #ddd;'><strong>Date Range:</strong></td><td style='padding: 6px; border: 1px solid #ddd;'>", quality$range_days, " days (", quality$range_years, " years)</td></tr>"
                )
            }

            quality_html <- paste0(quality_html, "</table>")

            # Add sample of failed values if any
            if (length(quality$failed_samples) > 0) {
                quality_html <- paste0(quality_html,
                    "<h5 style='margin-top: 15px; font-weight: bold;'>Sample of Failed Values:</h5>",
                    "<ul style='font-weight: bold;'>"
                )
                for (failed_val in quality$failed_samples) {
                    quality_html <- paste0(quality_html, "<li>", htmltools::htmlEscape(failed_val), "</li>")
                }
                quality_html <- paste0(quality_html, "</ul>")
            }

            quality_html <- paste0(quality_html, "</div>")
            self$results$qualityMetrics$setContent(quality_html)

            # Generate preview table
            preview_html <- private$.generatePreviewTable(
                original_display,
                parsed_dates,
                self$options$preview_rows
            )
            self$results$previewTable$setContent(preview_html)

            # Extract components (always extract to show preview)
            components <- private$.extractComponents(parsed_dates)

            # Generate component preview
            if (length(components) > 0) {
                component_preview_html <- private$.generateComponentPreview(
                    components,
                    self$options$preview_rows
                )
                self$results$componentPreview$setContent(component_preview_html)
            } else {
                self$results$componentPreview$setContent(
                    "<p><em>No components selected for extraction. Use the preview checkboxes or 'Add to Data' toggles in the left panel to extract datetime components.</em></p>"
                )
            }

            # rownames() on a data.frame with automatic row names rebuilds the whole
            # character vector on every call; the 13 writes below shared it 13 times.
            # jmvcore's Output$setRowNums() does a bare as.integer(), so a frame with
            # character row names stores NA row numbers against correct values -- a
            # silent mis-mapping. jamovi's own data always has integer-coercible names;
            # this only guards the R-API path.
            out_row_nums <- rownames(data)
            if (anyNA(suppressWarnings(as.integer(out_row_nums))))
                out_row_nums <- seq_len(nrow(data))

            # Add outputs to dataset.
            #
            # Gated on isNotFilled() ALONE, never on self$options$<name>: an Output option
            # is not an argument of the generated R wrapper (see R/datetimeconverter.h.R --
            # the signature ends at show_glossary), so it is permanently FALSE from the R
            # API and gating on it made all 13 columns unreachable headless. jamovi already
            # gates delivery: jmvcore's Output$asProtoBuf() wraps the whole payload in
            # `if (self$enabled)`, and `enabled` reads the option, so an unticked box ships
            # nothing regardless. isNotFilled() still prevents duplicate writes.
            #
            # The 11 component columns additionally test is.null(components[["<x>"]]) --
            # EXACT indexing, never `components[["day"]]`, which PARTIAL-MATCHES `dayname` and
            # would write weekday indices into a column labelled 'Day of month' -- because
            # .extractComponents() only computes what the preview/output checkboxes asked
            # for. That is an availability check, not an enable check: without it an
            # untouched component would reach setValues() as NULL and store a zero-length
            # vector against a full-length setRowNums(). Headless, `extract_<x> = TRUE` --
            # which IS a wrapper argument -- is what makes the column available.
            if (self$results$corrected_datetime_char$isNotFilled()) {
                self$results$corrected_datetime_char$setRowNums(out_row_nums)
                self$results$corrected_datetime_char$setValues(
                    private$.safeCharacterConversion(parsed_dates)
                )
            }

            # Add corrected datetime as numeric
            if (self$results$corrected_datetime_numeric$isNotFilled()) {
                self$results$corrected_datetime_numeric$setRowNums(out_row_nums)
                corrected_numeric <- as.numeric(parsed_dates)
                self$results$corrected_datetime_numeric$setValues(corrected_numeric)
            }

            # Add component outputs
            if (!is.null(components[["year"]]) && self$results$year_out$isNotFilled()) {
                self$results$year_out$setRowNums(out_row_nums)
                self$results$year_out$setValues(as.numeric(components[["year"]]))
            }

            if (!is.null(components[["month"]]) && self$results$month_out$isNotFilled()) {
                self$results$month_out$setRowNums(out_row_nums)
                self$results$month_out$setValues(as.numeric(components[["month"]]))
            }

            if (!is.null(components[["monthname"]]) && self$results$monthname_out$isNotFilled()) {
                self$results$monthname_out$setRowNums(out_row_nums)
                self$results$monthname_out$setValues(
                    private$.safeCharacterConversion(components[["monthname"]])
                )
            }

            if (!is.null(components[["day"]]) && self$results$day_out$isNotFilled()) {
                self$results$day_out$setRowNums(out_row_nums)
                self$results$day_out$setValues(as.numeric(components[["day"]]))
            }

            if (!is.null(components[["hour"]]) && self$results$hour_out$isNotFilled()) {
                self$results$hour_out$setRowNums(out_row_nums)
                self$results$hour_out$setValues(as.numeric(components[["hour"]]))
            }

            if (!is.null(components[["minute"]]) && self$results$minute_out$isNotFilled()) {
                self$results$minute_out$setRowNums(out_row_nums)
                self$results$minute_out$setValues(as.numeric(components[["minute"]]))
            }

            if (!is.null(components[["second"]]) && self$results$second_out$isNotFilled()) {
                self$results$second_out$setRowNums(out_row_nums)
                self$results$second_out$setValues(as.numeric(components[["second"]]))
            }

            if (!is.null(components[["dayname"]]) && self$results$dayname_out$isNotFilled()) {
                self$results$dayname_out$setRowNums(out_row_nums)
                self$results$dayname_out$setValues(
                    private$.safeCharacterConversion(components[["dayname"]])
                )
            }

            if (!is.null(components[["weeknum"]]) && self$results$weeknum_out$isNotFilled()) {
                self$results$weeknum_out$setRowNums(out_row_nums)
                self$results$weeknum_out$setValues(as.numeric(components[["weeknum"]]))
            }

            if (!is.null(components[["quarter"]]) && self$results$quarter_out$isNotFilled()) {
                self$results$quarter_out$setRowNums(out_row_nums)
                self$results$quarter_out$setValues(as.numeric(components[["quarter"]]))
            }

            if (!is.null(components[["dayofyear"]]) && self$results$dayofyear_out$isNotFilled()) {
                self$results$dayofyear_out$setRowNums(out_row_nums)
                self$results$dayofyear_out$setValues(as.numeric(components[["dayofyear"]]))
            }

            # Add completion notice
            if (quality$successfully_parsed > 0) {
                # Counts what is DELIVERED to the dataset, which jamovi gates on the
                # Output option (Output$asProtoBuf wraps the payload in `if (self$enabled)`,
                # and `enabled` reads the option). Deliberately NOT a count of what the
                # write block above filled: outside the GUI those options are always FALSE,
                # so an isNotFilled()-based count would claim "Added 2 component column(s)
                # to dataset" in the GUI when the boxes are unticked and nothing shipped.
                components_added <- sum(c(
                    self$options$year_out, self$options$month_out, self$options$monthname_out,
                    self$options$day_out, self$options$hour_out, self$options$minute_out,
                    self$options$second_out, self$options$dayname_out, self$options$weeknum_out,
                    self$options$quarter_out, self$options$dayofyear_out
                ))

                private$.addNotice(
                    type = "INFO",
                    title = .("Conversion Completed"),
                    content = .fmt(
                        .("DateTime conversion completed. \u2022 Processed {rows} rows from variable {name}. \u2022 Successfully parsed {parsed} of {nonmissing} non-missing values ({pct}%). \u2022 Added {added} component column(s) to the dataset. \u2022 Review the preview tables before proceeding with analysis."),
                        rows = quality$total_observations, name = datetime_var,
                        parsed = quality$successfully_parsed,
                        nonmissing = quality$total_observations - quality$original_missing,
                        pct = base::format(round(quality$success_rate, 1)),
                        added = components_added)
                )
            }

            # Populate new clinician-friendly panels
            private$.populateQualityAssessment(quality, parsed_dates, misuse_warnings)
            private$.populateNaturalLanguageSummary(datetime_var, detected_format,
                                                     quality, components, tz_summary)
            private$.populateExplanatoryPanels()
            private$.populateGlossary()

            # Render all collected notices
            private$.renderNotices()
        }
    )
)
