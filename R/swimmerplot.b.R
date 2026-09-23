.datatable.aware <- TRUE

# jmvcore's `.()` translator is a BARE SYMBOL: listing jmvcore in Imports: puts
# nothing in scope, so the @importFrom tag below has to travel with the analysis.
# swimmerplot is OncoPath's only analysis now that waterfall moved to JamoviTest,
# and waterfall.b.R was the file that used to carry the tag for that module --
# without it swimmerplot-html.R dies with `could not find function "."` in the
# installed module (2026-09-20; same class as the 2026-09-16 %>% audit).

#' @title Swimmer Plot
#' @description 
#' Comprehensive swimmer plot function with full ggswim integration.
#' Creates swimmer plots for visualizing patient timelines, treatments, milestones, and clinical events.
#'
#' @importFrom R6 R6Class
#' @importFrom ggplot2 ggplot aes labs theme element_text element_blank
#' @importFrom dplyr mutate filter group_by summarize left_join arrange n bind_rows
#' @importFrom lubridate ymd_hms ymd ydm mdy myd dmy dym interval time_length
#' @importFrom tibble tibble
#' @importFrom RColorBrewer brewer.pal
#' @importFrom ggswim geom_swim_lane geom_swim_marker scale_marker_discrete geom_swim_arrow theme_ggswim theme_ggswim_dark
#' @importFrom data.table as.data.table
#' @importFrom jmvcore .
#' @return An \code{R6} class generator object for the \code{swimmerplotClass} backend; used internally by the jamovi analysis wrapper and not called directly.

swimmerplotClass <- if (requireNamespace('jmvcore', quietly = TRUE)) R6::R6Class(
    "swimmerplotClass",
    inherit = swimmerplotBase,
    private = list(
        # Notice collection helpers. A single Preformatted (plain-text) output item.
        # library-audit 2026-09-22 OncoPath [LOW] REJECTED: Notice renders single-line plain text;
        #   Preformatted avoids HTML and accumulates safely across run cycles (guide section 13)
        .noticeList = list(),
        # Which numeric censoring convention the data turned out to use, so the
        # run can state the assumption instead of making it silently.
        .censor_coding = NULL,

        .addNotice = function(type, title, content) {
            # Skip exact duplicates: shared helpers (.responseRates) run once per
            # consumer within a cycle and must not double-report.
            for (n in private$.noticeList)
                if (identical(n$type, type) && identical(n$title, title) &&
                    identical(n$content, content))
                    return(invisible(NULL))
            private$.noticeList[[length(private$.noticeList) + 1]] <- list(
                type = type,
                title = title,
                content = content
            )
            # Render immediately so early-return validation aborts still display the notice
            private$.renderNotices()
        },

        .renderNotices = function() {
            if (length(private$.noticeList) == 0) {
                self$results$notices$setContent("")
                return()
            }

            # Plain text only - notices avoid HTML by project convention; the Preformatted
            # output item renders this literally (no markup, no injection surface).
            # STRONG_WARNING must not print as a plain "WARNING: " too, or the level
            # has no effect: a clinician cannot tell "n = 3 patients, treat as
            # exploratory" from "a milestone date could not be parsed". The two
            # analyses here that already distinguish them (jjbarstats,
            # nogoldstandard) print "STRONG WARNING: ", so that is what this uses.
            #
            # ": " is composed OUTSIDE .(): the bare words are existing msgids, already
            # translated in the UMBRELLA catalog (HATA / GUCLU UYARI / UYARI / NOT),
            # whereas ".(\"ERROR: \")" would mint a new msgid shipping with an empty
            # msgstr. The submodule catalogs do not carry them yet, so until
            # jmvtools::i18nUpdate() runs there a non-English user sees an English
            # prefix on a translated title. Hoisted out of the closure so the .()
            # calls evaluate in this method's frame.
            prefix_error   <- paste0(.("ERROR"), ": ")
            prefix_strong  <- paste0(.("STRONG WARNING"), ": ")
            prefix_warning <- paste0(.("WARNING"), ": ")
            prefix_note    <- paste0(.("NOTE"), ": ")
            # Most severe first. The list is in insertion order, which put a
            # fatal ERROR underneath three routine NOTEs; the prefix work gave
            # the levels distinct names but not distinct positions.
            sev <- vapply(private$.noticeList, function(n) n$type, "")
            ordered <- private$.noticeList[order(match(
                sev, c("ERROR", "STRONG_WARNING", "WARNING", "INFO")))]
            blocks <- vapply(ordered, function(notice) {
                prefix <- switch(notice$type,
                    ERROR          = prefix_error,
                    STRONG_WARNING = prefix_strong,
                    WARNING        = prefix_warning,
                    INFO           = prefix_note,
                    "")
                paste0(prefix, notice$title, "\n", notice$content)
            }, character(1))

            self$results$notices$setContent(paste(blocks, collapse = "\n\n"))
        },

        # Row index of each patient's EARLIEST start, one entry per row.
        #
        # "Relative (all start from 0)" must anchor each PATIENT at zero, not each
        # ROW. A swimmer plot is multi-row per patient by construction - the module
        # merges per-patient intervals precisely for that - so rebasing row-wise
        # stacks every episode of a patient back onto t=0. Follow-up then collapses
        # to the longest single episode and merged person-time to the union of the
        # stacked episodes, silently corrupting total person-time, mean/median
        # duration, follow-up density and the reverse-KM median in the DEFAULT
        # configuration.
        .patientAnchorIndex = function(patient_id, start_vals) {
            v <- as.numeric(start_vals)
            stats::ave(
                seq_along(v), as.character(patient_id),
                FUN = function(ix) {
                    vals <- v[ix]
                    if (all(is.na(vals))) ix[1] else ix[which.min(vals)]
                }
            )
        },

        # The origin milestones and events are measured FROM. Always the patient's
        # earliest start (anchor_start), never the row's own start: a patient with
        # two episodes would otherwise have episode-1's milestone re-based on
        # episode-2's start and come out negative.
        .shiftBasis = function(patient_data) {
            if ("anchor_start" %in% names(patient_data)) patient_data$anchor_start
            else patient_data$original_start
        },

        # Does x measure duration from each patient's own start?
        #
        # A median or protocol reference line is a DURATION, so it can only be
        # drawn on an axis that measures one. Date scales were already excluded;
        # raw numeric times with Relative off were not, and the line was drawn
        # silently at the wrong place - lanes spanning study time 100-256 got a
        # "Median: 12.5" line off the left-hand end, and protocol lines at
        # 3/6/9/12/18/24 landed before any patient existed. Equal start times are
        # the exception: there the absolute axis IS the duration axis.
        .isDurationAxis = function(patient_data) {
            if (inherits(patient_data$start_time, c("Date", "POSIXct", "POSIXlt")))
                return(FALSE)
            if (!identical(self$options$timeDisplay, "absolute"))
                return(TRUE)
            s <- suppressWarnings(as.numeric(patient_data$start_time))
            s <- s[is.finite(s)]
            length(s) == 0 || isTRUE(all.equal(min(s), max(s)))
        },

        # Single source of truth for ORR/DCR.
        #
        # ORR and DCR are defined only for RECIST-coded responses. When the
        # response variable uses some other coding ("Responder", 0/1, "Grade 1")
        # nothing normalises to CR/PR/SD/PD and the numerator is legitimately
        # zero - which read as a genuine "ORR 0.0%" with an exact binomial CI.
        # The guard lived only in .updateAdvancedMetrics, so the copy-ready
        # manuscript text went on asserting 0.0% in the same output that the
        # metrics table refused to make the claim in. Both call this now.
        .responseRates = function(response_counts, n_patients = NULL) {
            if (is.null(response_counts)) return(NULL)

            nm <- names(response_counts)
            total <- sum(response_counts)
            recist_n <- sum(response_counts[nm %in% c("CR", "PR", "SD", "PD")])
            n_excluded <- total - recist_n

            # Everyone in the analysis, including patients whose response is
            # missing entirely (they have no row in `response_counts`).
            n_all <- if (is.null(n_patients) || !is.finite(n_patients) ||
                         n_patients < total) total else n_patients
            n_missing <- n_all - total

            if (!is.finite(total) || total <= 0 || recist_n == 0) {
                return(list(evaluable = FALSE, n = total, n_all = n_all,
                            n_evaluable = 0L, n_excluded = n_excluded,
                            n_missing = n_missing,
                            orr_count = NA_integer_, dcr_count = NA_integer_,
                            orr = NA_real_, dcr = NA_real_,
                            orr_evaluable = NA_real_, dcr_evaluable = NA_real_))
            }

            orr_count <- sum(response_counts[nm %in% c("CR", "PR")])
            dcr_count <- sum(response_counts[nm %in% c("CR", "PR", "SD")])

            # DENOMINATOR: every patient in the analysis.
            #
            # RECIST 1.1 section 4.9.1 is explicit: "Trial conclusions should be
            # based on the response rate for all eligible (or all treated)
            # patients and should not be based on a selected 'evaluable'
            # subset." NE is not an exclusion - it is one of the five outcomes a
            # patient can be assigned, and such patients count as non-responders.
            #
            # This analysis previously divided by the CR/PR/SD/PD subset, so on
            # a 12-patient cohort with 2 NE and 1 missing response it reported
            # ORR 44.4% (4/9) where the RECIST-conformant figure is 33.3%
            # (4/12) - and the same page showed per-category rates over 11 and a
            # Fisher test over 11. The evaluable-subset rate is still returned,
            # but as a labelled secondary, never as "the" ORR.
            if (n_excluded > 0 || n_missing > 0) {
                private$.addNotice(
                    "INFO",
                    .("How ORR and DCR are counted"),
                    sprintf(
                        .("ORR and DCR are computed over all %d patients, as RECIST 1.1 section 4.9.1 requires: %d with a response that is not CR/PR/SD/PD (for example NE) and %d with no recorded response count as non-responders rather than being dropped. Over the %d CR/PR/SD/PD patients alone the rates would be ORR %.1f%% and DCR %.1f%%; those are per-protocol figures and are not what the tables report."),
                        n_all, n_excluded, n_missing, recist_n,
                        if (recist_n > 0) orr_count / recist_n * 100 else NA_real_,
                        if (recist_n > 0) dcr_count / recist_n * 100 else NA_real_))
            }

            list(evaluable = TRUE, n = total, n_all = n_all,
                 n_evaluable = recist_n, n_excluded = n_excluded,
                 n_missing = n_missing,
                 orr_count = orr_count, dcr_count = dcr_count,
                 orr = orr_count / n_all * 100,
                 dcr = dcr_count / n_all * 100,
                 orr_evaluable = orr_count / recist_n * 100,
                 dcr_evaluable = dcr_count / recist_n * 100)
        },

        # TRUE when the first few non-missing values look like ISO calendar
        # dates (YYYY-MM-DD...). Shared by the milestone and event raw-mode
        # refusal checks so the two cannot drift.
        .looksLikeIsoDates = function(x) {
            smp <- as.character(utils::head(x[!is.na(x)], 3))
            any(grepl("^\\d{4}-\\d{2}-\\d{2}", smp))
        },

        # Enhanced clinical date parsing with contextual guidance
        .parseDatesWithClinicalContext = function(dates, format, variable_type = "time") {
            if (inherits(dates, c("Date", "POSIXct", "POSIXlt"))) {
                return(list(value = dates, error = FALSE, message = NULL))
            }

            # Check if data appears to be numeric when Date/Time is selected
            sample_data <- as.character(utils::head(dates[!is.na(dates)], 5))
            is_numeric_like <- length(sample_data) > 0 && all(grepl("^-?\\d*\\.?\\d+$", sample_data))

            if (is_numeric_like) {
                # User selected Date/Time but data is numeric - provide guidance
                return(list(
                    value = NULL,
                    error = TRUE,
                    data_type_mismatch = TRUE,
                    detected_type = "numeric",
                    examples = sample_data[seq_len(min(3, length(sample_data)))],
                    message = paste(
                        "Data type mismatch detected:",
                        paste0("Your ", variable_type, " variables contain numeric values (",
                               paste(sample_data[seq_len(min(3, length(sample_data)))], collapse = ", "), ")"),
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
                        sprintf(.("Error parsing %s dates with format %s."), variable_type, format),
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
            
            # Check for realistic time ranges.
            # Compute durations in the SELECTED time unit. For Date/POSIXct input a raw
            # subtraction yields an auto-unit difftime (secs/hours/days), which would make
            # the day/month thresholds below meaningless; convert via lubridate so the
            # >10-year / zero-duration checks are correct in datetime/absolute mode too.
            if (inherits(patient_data$start_time, c("Date", "POSIXct", "POSIXlt")) ||
                inherits(patient_data$end_time, c("Date", "POSIXct", "POSIXlt"))) {
                intervals <- suppressWarnings(lubridate::interval(patient_data$start_time, patient_data$end_time))
                durations <- suppressWarnings(lubridate::time_length(intervals, unit = self$options$timeUnit))
            } else {
                durations <- as.numeric(patient_data$end_time) - as.numeric(patient_data$start_time)
            }
            # (No negative-duration or missing-ID checks here: those rows are
            # filtered and disclosed by .validateAndProcessData BEFORE this
            # function runs, so such branches could never fire.)

            # EVERY check below is about patients, so it counts patients.
            #
            # They used to count ROWS while saying "patients", and a swimmer plot
            # is multi-row per patient by construction: six patients on two lines
            # each, with the response recorded on line one, were reported as
            # "6 duplicate patient IDs" and "6 patients with missing response data
            # (50.0%)" on the same page where the summary classified all six and
            # showed none missing. A patient's follow-up is likewise their whole
            # span, not one episode.
            pid <- as.character(patient_data$patient_id)
            by_patient <- function(f) vapply(split(seq_along(pid), pid), f, 0)
            n_patients <- length(unique(pid))
            spans <- by_patient(function(ix) {
                d <- durations[ix]
                d <- d[is.finite(d)]
                if (length(d) == 0) NA_real_ else sum(d)
            })

            # Check for extremely long follow-up periods
            # 10 years expressed in the selected unit. weeks and years had no arm,
            # so is.finite() was FALSE and the whole check was skipped: the same
            # data under timeUnit="years" published "median follow-up 332.0 years"
            # with no plausibility notice at all.
            long_limit <- switch(self$options$timeUnit,
                                 days = 3650, weeks = 522, months = 120, years = 10,
                                 NA_real_)
            if (is.finite(long_limit)) {
                n_long <- sum(spans > long_limit, na.rm = TRUE)
                if (n_long > 0) {
                    warnings <- append(warnings, sprintf(
                        .("Total follow-up exceeds 10 years for %d of %d patients. Consider checking data accuracy or using different time units."),
                        n_long, n_patients
                    ))
                }
            }

            # Check for zero-duration events
            n_zero <- sum(spans == 0, na.rm = TRUE)
            if (n_zero > 0) {
                warnings <- append(warnings, sprintf(
                    .("Zero follow-up time for %d of %d patients. These may represent same-day events."),
                    n_zero, n_patients
                ))
            }

            # Multiple episodes per patient
            n_multi <- sum(by_patient(function(ix) length(ix)) > 1)
            if (n_multi > 0) {
                warnings <- append(warnings, sprintf(
                    .("Multiple episodes recorded for %d of %d patients. This is normal for longitudinal data."),
                    n_multi, n_patients
                ))
            }

            # Response variable validation
            if ("response" %in% names(patient_data)) {
                # A patient is missing a response only when NONE of their rows has
                # one; a blank follow-up line is not a missing response.
                missing_response <- sum(by_patient(
                    function(ix) all(is.na(patient_data$response[ix]))))
                if (missing_response > 0) {
                    warnings <- append(warnings, sprintf(
                        .("No response recorded for %d of %d patients (%.1f%%)."),
                        missing_response, n_patients,
                        missing_response / max(n_patients, 1) * 100
                    ))
                }

                # Check for unusual response patterns. Tabulated on the same best
                # response every other table uses, so the panel cannot contradict
                # the summary it sits above.
                best <- vapply(split(seq_along(pid), pid), function(ix) {
                    keep <- ix[!is.na(patient_data$response[ix])]
                    if (length(keep) == 0) return(NA_character_)
                    private$.normalizeResponse(private$.getBestResponse(
                        as.character(patient_data$response[keep]),
                        patient_data$start_time[keep]))
                }, "")
                response_counts <- table(best, useNA = "no")
                if (length(response_counts) > 0 && min(response_counts) < 3) {
                    warnings <- append(warnings,
                        .("Some response categories have <3 patients. Consider grouping categories for meaningful analysis.")
                    )
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
                    message = .fmt(.("Missing required variables: {vars}"), vars = paste(missing_vars, collapse = ", "))
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
                    message = .fmt(.("Error processing core variables: {message}"), message = e$message)
                ))
            })
            
            # Check if data extraction failed
            if (!is.null(patient_data$error) && patient_data$error) {
                return(patient_data)
            }
            
            # Enhanced time processing
            if (self$options$timeType != "datetime") {
                private$.addNotice("INFO", .("Time units"), sprintf(
                    .("Raw start and end values are taken as already expressed in %s; no conversion is applied."),
                    private$.timeUnitWord()))
            }
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
                    return(list(error = TRUE, message = .fmt(.("Start time parsing: {message}"), message = start_parsed$message)))
                }
                if (isTRUE(end_parsed$error)) {
                    return(list(error = TRUE, message = .fmt(.("End time parsing: {message}"), message = end_parsed$message)))
                }
                
                # Remember what was blank BEFORE parsing, so the exclusion
                # notice can tell "the cell was empty" from "the cell held
                # something this Date Format cannot read" - both used to be
                # reported as "missing start or end time".
                pre_missing <- is.na(patient_data$start_time) | is.na(patient_data$end_time) |
                               !nzchar(trimws(as.character(patient_data$start_time))) |
                               !nzchar(trimws(as.character(patient_data$end_time)))
                patient_data$start_time <- start_parsed$value
                patient_data$end_time <- end_parsed$value
                attr(patient_data, "pre_missing") <- pre_missing

                # A wrong Date Format choice parses to all-NA WITHOUT an error
                # (lubridate returns NA), and the rows then died in the validity
                # filter with a message about end < start - misdirecting the
                # user away from the actual cause. Name it here instead.
                if (all(is.na(patient_data$start_time)) || all(is.na(patient_data$end_time))) {
                    return(list(error = TRUE, message = .fmt(
                        .("None of the start/end values could be parsed as dates with the selected Date Format ({fmt}). Choose the format that matches how your dates are written (e.g. 2023-01-15 needs YYYY-MM-DD)."),
                        fmt = private$.dateFormatWord())))
                }

                # Handle relative vs absolute time display, anchored PER PATIENT
                if (self$options$timeDisplay == "relative") {
                    patient_data$original_start <- patient_data$start_time
                    patient_data$original_end <- patient_data$end_time

                    aidx <- private$.patientAnchorIndex(patient_data$patient_id,
                                                        patient_data$start_time)
                    anchor <- patient_data$original_start[aidx]
                    patient_data$anchor_start <- anchor

                    patient_data$start_time <- lubridate::time_length(
                        lubridate::interval(anchor, patient_data$original_start),
                        unit = self$options$timeUnit)
                    patient_data$end_time <- lubridate::time_length(
                        lubridate::interval(anchor, patient_data$original_end),
                        unit = self$options$timeUnit)
                }
            } else {
                # Enhanced date format detection
                start_sample <- as.character(patient_data$start_time[seq_len(min(3, nrow(patient_data)))])

                # Detect various date formats. The two-digit/two-digit/four-digit
                # shape is genuinely ambiguous (12/01/2023 could be Dec 1 or
                # Jan 12), so it is reported as such and the user must choose -
                # the previous code always claimed "MM/DD/YYYY", and following
                # that guidance on European data silently shifted every date.
                date_patterns <- list(
                    "YYYY-MM-DD" = "^\\d{4}-\\d{2}-\\d{2}",
                    "MM/DD/YYYY or DD/MM/YYYY (choose the one matching your data)" = "^\\d{2}/\\d{2}/\\d{4}",
                    "YYYY/MM/DD" = "^\\d{4}/\\d{2}/\\d{2}"
                )

                detected_format <- NULL

                for (format_name in names(date_patterns)) {
                    if (any(grepl(date_patterns[[format_name]], start_sample))) {
                        detected_format <- format_name
                        break
                    }
                }

                is_date_like <- !is.null(detected_format)

                if (is_date_like) {
                    # Return special flag to indicate date detection (not an error)
                    return(list(
                        date_detected = TRUE,
                        format = detected_format,
                        examples = start_sample[seq_len(min(2, length(start_sample)))]
                    ))
                } else {
                    # Raw numeric processing with robust conversion
                    raw_start <- as.character(patient_data$start_time)
                    raw_end   <- as.character(patient_data$end_time)
                    # Same bookkeeping as the datetime branch: what was blank
                    # before the conversion, so the exclusion notice can tell an
                    # empty cell from one holding text.
                    attr(patient_data, "pre_missing") <-
                        is.na(raw_start) | is.na(raw_end) |
                        !nzchar(trimws(raw_start)) | !nzchar(trimws(raw_end))
                    patient_data$start_time <- suppressWarnings(as.numeric(raw_start))
                    patient_data$end_time <- suppressWarnings(as.numeric(raw_end))

                    # Name the actual cause. The date sniffer above only knows
                    # YYYY-MM-DD, NN/NN/YYYY and YYYY/NN/NN, and only looks at
                    # the first three start values, so "1/5/2023" or
                    # "15.01.2023" fell through to as.numeric(), became NA, and
                    # died in the validity filter as "end times are >= start
                    # times" - a message about an ordering problem for data that
                    # contains no numbers at all.
                    lost <- function(raw, num) sum(!is.na(raw) & nzchar(trimws(raw)) & is.na(num))
                    n_nan <- lost(raw_start, patient_data$start_time) +
                             lost(raw_end, patient_data$end_time)
                    if (n_nan > 0) {
                        bad <- c(raw_start[!is.na(raw_start) & nzchar(trimws(raw_start)) &
                                           is.na(patient_data$start_time)],
                                 raw_end[!is.na(raw_end) & nzchar(trimws(raw_end)) &
                                         is.na(patient_data$end_time)])
                        if (all(is.na(patient_data$start_time)) || all(is.na(patient_data$end_time)))
                            return(list(error = TRUE, message = sprintf(
                                .("None of the start/end values are numbers (for example '%s'). Time Input Type is set to Raw Values, which expects a number of %s. If these are calendar dates, switch Time Input Type to Date/Time and choose the matching Date Format."),
                                as.character(bad[1]), private$.timeUnitWord())))
                        private$.addNotice("WARNING", .("Values that are not numbers"), sprintf(
                            .("%d start/end values are not numbers (for example '%s') and those rows are excluded. Time Input Type is set to Raw Values; if these are calendar dates, switch it to Date/Time."),
                            n_nan, as.character(bad[1])))
                    }

                    # Handle relative vs absolute time display.
                    #
                    # The datetime branch above does this; the raw-numeric branch
                    # did not, so "Relative (all start from 0)" left the lanes at
                    # their absolute positions while milestones WERE shifted - a
                    # milestone drawn at t=5 against a lane running 10..30.
                    if (self$options$timeDisplay == "relative") {
                        patient_data$original_start <- patient_data$start_time
                        patient_data$original_end   <- patient_data$end_time

                        aidx <- private$.patientAnchorIndex(patient_data$patient_id,
                                                            patient_data$start_time)
                        anchor <- patient_data$original_start[aidx]
                        patient_data$anchor_start <- anchor

                        patient_data$start_time <- patient_data$original_start - anchor
                        patient_data$end_time   <- patient_data$original_end - anchor
                    }
                }
            }
            
            # Add response/status variable if provided
            if (!is.null(self$options$responseVar)) {
                patient_data$response <- as.factor(df[[self$options$responseVar]])

                # A second, NORMALISED column for the figure. The lanes were
                # coloured by the raw factor while every table tabulated the
                # normalised label, so a file mixing "complete response",
                # "Complete Response" and "CR" drew three different colours and
                # three legend keys for the one row the summary called "CR".
                # Ordered clinically (CR, PR, SD, PD, NE, then anything else)
                # rather than alphabetically, which interleaved PD between PR
                # and SD in the legend.
                lab <- private$.normalizeResponses(patient_data$response)
                known <- c("CR", "PR", "SD", "PD", "NE")
                lvls <- c(known[known %in% lab], sort(unique(lab[!lab %in% known & !is.na(lab)])))
                patient_data$response_label <- factor(lab, levels = lvls)
            }

            # Add censoring/event status variable if provided
            if (!is.null(self$options$censorVar)) {
                patient_data$censor_status <- df[[self$options$censorVar]]
            }

            # Add grouping variable if provided
            if (!is.null(self$options$groupVar)) {
                patient_data$patient_group <- as.factor(df[[self$options$groupVar]])
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
            
            # Disclose the exclusion. .validateClinicalData() below runs on the
            # ALREADY-filtered frame, so its "these will be excluded from
            # analysis" warnings can never fire for the rows this filter drops -
            # patients disappeared from the figure and from every denominator
            # with nothing said. Row counts drive clinical interpretation, so the
            # exclusion has to be visible.
            n_dropped <- sum(!valid_rows)
            if (n_dropped > 0) {
                n_bad_id    <- sum(is.na(patient_data$patient_id))
                n_bad_time  <- sum(!is.na(patient_data$patient_id) &
                                   (is.na(patient_data$start_time) |
                                    is.na(patient_data$end_time)))
                n_bad_order <- sum(!is.na(patient_data$patient_id) &
                                   !is.na(patient_data$start_time) &
                                   !is.na(patient_data$end_time) &
                                   patient_data$end_time < patient_data$start_time)

                # Each reason is a COMPLETE sentence, appended after the summary
                # sentence rather than glued with "; " inside its parenthesis.
                # The old shape handed a translator four verb-less fragments and
                # a container with a %s hole, which Turkish word order cannot be
                # arranged around (guide section 5.2).
                reasons <- character(0)
                if (n_bad_id > 0)
                    reasons <- c(reasons, sprintf(.("%d had no patient ID."), n_bad_id))
                if (n_bad_time > 0) {
                    pre <- attr(patient_data, "pre_missing")
                    bad_time <- !is.na(patient_data$patient_id) &
                                (is.na(patient_data$start_time) | is.na(patient_data$end_time))
                    n_unparsed <- if (is.null(pre)) 0L else sum(bad_time & !pre)
                    n_empty <- n_bad_time - n_unparsed
                    if (n_empty > 0)
                        reasons <- c(reasons, sprintf(.("%d had no start or end time."), n_empty))
                    if (n_unparsed > 0)
                        reasons <- c(reasons, if (identical(self$options$timeType, "datetime"))
                            sprintf(
                                .("%d had a start or end that could not be read with the selected Date Format (%s)."),
                                n_unparsed, private$.dateFormatWord())
                            else sprintf(
                                .("%d had a start or end that is not a number."), n_unparsed))
                }
                if (n_bad_order > 0)
                    reasons <- c(reasons, sprintf(.("%d had an end time before the start time."), n_bad_order))

                private$.addNotice(
                    "WARNING",
                    .("Rows excluded from analysis"),
                    paste(c(sprintf(
                        .("%d of %d rows were excluded before analysis. All counts, rates and person-time below are based on the remaining %d rows."),
                        n_dropped, length(valid_rows), sum(valid_rows)),
                        reasons), collapse = " ")
                )
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

            # Collapse to ONE ROW PER PATIENT before doing anything else.
            #
            # Everything below pairs the milestone column with patient_data
            # positionally. patient_data has one row per EPISODE, so for a patient
            # with two episodes match() handed the same milestone value to both
            # rows: the Milestone Event Summary counted it twice, and the copy
            # attached to the later episode was re-based on that episode's start
            # and emerged negative. Multi-episode input is explicitly supported -
            # the module detects it and says so - so it has to be handled here.
            patient_data <- patient_data[
                !duplicated(as.character(patient_data$patient_id)), , drop = FALSE]
            
            # Resolve one label per slot BEFORE the loop, and say what is being
            # ignored. Three ways to lose an assigned milestone without a word:
            # "Maximum milestones" below the slot number simply never reaches it;
            # a blank name skipped the slot even though a variable was chosen;
            # and two slots sharing a name collapsed into one table row, so two
            # different columns of 10 events read as one "Surgery" of 20.
            n_slots <- 5
            assigned <- vapply(seq_len(n_slots), function(i)
                !is.null(self$options[[paste0("milestone", i, "Date")]]), TRUE)
            slot_names <- vapply(seq_len(n_slots), function(i) {
                nm <- self$options[[paste0("milestone", i, "Name")]]
                if (is.null(nm)) "" else trimws(as.character(nm))
            }, "")
            slot_vars <- vapply(seq_len(n_slots), function(i) {
                v <- self$options[[paste0("milestone", i, "Date")]]
                if (is.null(v)) "" else as.character(v)
            }, "")

            # A blank name falls back to the variable's own name.
            blank <- assigned & !nzchar(slot_names)
            slot_names[blank] <- slot_vars[blank]

            # Colliding names are qualified by the variable they came from.
            labels <- slot_names
            dup <- assigned & nzchar(slot_names) &
                   slot_names %in% slot_names[assigned][duplicated(slot_names[assigned])]
            labels[dup] <- paste0(slot_names[dup], " (", slot_vars[dup], ")")

            max_ms <- self$options$maxMilestones
            ignored <- which(assigned & seq_len(n_slots) > max_ms)
            if (length(ignored) > 0) {
                private$.addNotice(
                    "INFO",
                    .("Milestone slots not shown"),
                    sprintf(
                        .("Milestone slot(s) %s have a variable assigned but 'Maximum milestones' is set to %d, so they are not shown. Raise it to include them."),
                        paste(ignored, collapse = ", "), max_ms))
            }
            if (any(blank)) {
                private$.addNotice(
                    "INFO",
                    .("Milestone name taken from the variable"),
                    sprintf(
                        .("Milestone slot(s) %s have no name, so the variable name is used instead."),
                        paste(which(blank), collapse = ", ")))
            }
            if (any(dup)) {
                private$.addNotice(
                    "INFO",
                    .("Milestone names repeated"),
                    sprintf(
                        .("Milestone slot(s) %s share a name, so each is labelled with the variable it came from to keep them apart in the summary."),
                        paste(which(dup), collapse = ", ")))
            }

            for (i in 1:max_ms) {
                date_opt <- paste0("milestone", i, "Date")
                milestone_label <- labels[i]

                if (!is.null(self$options[[date_opt]]) && nzchar(milestone_label)) {

                    milestone_dates <- self$data[[self$options[[date_opt]]]]

                    # Realign to the validated patient table BY PATIENT ID,
                    # taking each patient's FIRST NON-MISSING milestone value.
                    #
                    # milestone_dates is read from self$data (unfiltered), while
                    # patient_data has had rows removed and deduplicated. A plain
                    # match() took the FIRST source row per patient, so in
                    # multi-episode data a milestone recorded on a later episode
                    # row was silently lost from the plot and the summary table.
                    if (!is.null(self$options$patientID) &&
                        self$options$patientID %in% names(self$data)) {
                        src_ids <- as.character(self$data[[self$options$patientID]])
                        nn <- which(!is.na(milestone_dates))
                        first_nn <- nn[!duplicated(src_ids[nn])]
                        lookup <- stats::setNames(first_nn, src_ids[first_nn])
                        align <- unname(lookup[as.character(patient_data$patient_id)])
                        milestone_dates <- milestone_dates[align]
                    }

                    # Skip if all NA
                    if (all(is.na(milestone_dates))) next
                    
                    # Process dates
                    if (self$options$timeType == "datetime") {
                        parsed_dates <- private$.parseDatesWithClinicalContext(
                            milestone_dates,
                            self$options$dateFormat,
                            "milestone"
                        )
                        # A failed parse used to fall through as an all-NA vector
                        # and the milestone simply vanished with nothing said.
                        if (isTRUE(parsed_dates$error)) {
                            private$.addNotice(
                                "WARNING",
                                .("Milestone dates could not be parsed"),
                                sprintf(
                                    .("Milestone '%s' was skipped. %s"),
                                    milestone_label,
                                    as.character(parsed_dates$message)))
                            next
                        }
                        milestone_dates <- parsed_dates$value

                        # Adjust for relative display (vectorized for performance)
                        if (self$options$timeDisplay == "relative" && "original_start" %in% names(patient_data)) {
                            # Vectorized calculation for better performance with large datasets
                            basis <- private$.shiftBasis(patient_data)
                            valid_indices <- which(!is.na(milestone_dates) & seq_along(milestone_dates) <= nrow(patient_data))
                            if (length(valid_indices) > 0) {
                                intervals <- lubridate::interval(
                                    basis[valid_indices],
                                    milestone_dates[valid_indices]
                                )
                                adjusted_dates <- rep(NA_real_, length(milestone_dates))
                                adjusted_dates[valid_indices] <- lubridate::time_length(intervals, unit = self$options$timeUnit)
                                milestone_dates <- adjusted_dates
                            }
                        }
                    } else {
                        # Raw-numeric timeline: a calendar date cannot be placed
                        # on a numeric axis. The previous conversion fed the
                        # numeric anchor into lubridate::interval(), anchoring at
                        # 1970 and publishing ~600-month milestone medians as if
                        # they were real statistics. Refuse with guidance instead.
                        if (private$.looksLikeIsoDates(milestone_dates)) {
                            private$.addNotice(
                                "WARNING",
                                .("Milestone skipped: calendar dates on a numeric timeline"),
                                sprintf(
                                    .("Milestone '%s' contains calendar dates, but the timeline uses raw numeric times, so these dates cannot be placed on the time axis. The milestone was skipped. Switch Time Input Type to Date/Time (or supply numeric milestone times) to show it."),
                                    milestone_label))
                            next
                        }

                        milestone_dates <- suppressWarnings(as.numeric(as.character(milestone_dates)))

                        # Adjust for relative display. Shift by original_start:
                        # start_time has already been zeroed by the relative
                        # conversion, so subtracting it would be a no-op.
                        if (self$options$timeDisplay == "relative" &&
                            "original_start" %in% names(patient_data)) {
                            milestone_dates <- milestone_dates - private$.shiftBasis(patient_data)
                        }
                    }
                    
                    # Create milestone dataframe - ensure consistent lengths
                    # Handle case where milestone_dates might have different length than patient_data
                    max_length <- min(length(milestone_dates), nrow(patient_data))

                    if (max_length > 0) {
                        temp_milestone <- data.frame(
                            patient_id = patient_data$patient_id[1:max_length],
                            time = milestone_dates[1:max_length],
                            label = milestone_label,
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
            
            # Milestones outside the patient's own window, disclosed the same way
            # event markers already are.
            #
            # .processEventMarkers filters to [earliest start, latest end] and
            # names what it dropped; this path never compared a milestone to the
            # timeline at all. In the module's own swimmerplot_test that is not
            # hypothetical: 6 of 19 Progression values and 7 of 30 BestResponse
            # values fall AFTER the patient's end time (PT004 ends at 103 with
            # Progression 274), so the figure drew triangles floating past the
            # end of the lane and the Milestone Event Summary pooled them into
            # its median. The markers are kept - a progression recorded after the
            # last follow-up line is real - but the reader is told.
            if (nrow(milestone_data) > 0 && nrow(patient_data) > 0) {
                pid_chr <- as.character(patient_data$patient_id)
                win_lo <- tapply(private$.asNumericTime(patient_data$start_time),
                                 pid_chr, min, na.rm = TRUE)
                win_hi <- tapply(private$.asNumericTime(patient_data$end_time),
                                 pid_chr, max, na.rm = TRUE)
                ms_t  <- private$.asNumericTime(milestone_data$time)
                ms_lo <- win_lo[as.character(milestone_data$patient_id)]
                ms_hi <- win_hi[as.character(milestone_data$patient_id)]
                outside <- !is.na(ms_t) & !is.na(ms_lo) & !is.na(ms_hi) &
                           (ms_t < ms_lo | ms_t > ms_hi)
                if (any(outside)) {
                    by_label <- table(as.character(milestone_data$label)[outside])
                    private$.addNotice(
                        "WARNING",
                        .("Milestones outside the patient timeline"),
                        sprintf(
                            .("%d of %d milestone values fall outside the patient's own start-to-end window (%s). They are still drawn and still counted in the Milestone Event Summary, so its median and range include times the timeline does not cover. Check that the milestone variable is measured on the same clock as Start Time and End Time."),
                            sum(outside), length(outside),
                            paste(sprintf("%s: %d", names(by_label), as.integer(by_label)),
                                  collapse = "; ")))
                }
            }

            return(milestone_data)
        },
        
        # Process ongoing status arrows
        .processOngoingStatus = function(patient_data, stats) {
            if (nrow(patient_data) == 0 || is.null(stats)) return(NULL)

            end_numeric <- private$.asNumericTime(patient_data$end_time)
            if (all(is.na(end_numeric))) return(NULL)

            # Determine ongoing status based on censoring variable if provided
            if (!is.null(self$options$censorVar) && "censor_status" %in% names(patient_data)) {
                # Explicit censoring variable, decided PER PATIENT from the LAST
                # classifiable status (the same rule .summarizeByPatient feeds to
                # the reverse-KM estimate), and drawn at the patient's LATEST
                # end. The old per-row logic drew arrows mid-lane at the end of
                # whichever episode row happened to be coded censored, and could
                # disagree with the estimator for multi-episode patients.
                pid <- as.character(patient_data$patient_id)
                status <- private$.classifyCensoring(patient_data$censor_status)

                # State the convention that was inferred. Reading a censoring
                # column is a guess about which value means "event", and getting
                # it backwards halves the median follow-up; the user has to be
                # able to see which way it was read.
                if (identical(private$.censor_coding, "surv12")) {
                    private$.addNotice("WARNING", .("Censoring coded 1/2"), sprintf(
                        .("The censoring variable '%s' contains only the values 1 and 2, so it was read using the survival package's convention: 1 = censored (still at risk), 2 = event. If your data instead codes 1 as the event, recode it to 0 = censored and 1 = event, because every follow-up figure below depends on which way round this is."),
                        self$options$censorVar))
                } else if (identical(private$.censor_coding, "zero_one")) {
                    private$.addNotice("INFO", .("Censoring coding"), sprintf(
                        .("The censoring variable '%s' was read as 0 = censored (still at risk), 1 = event."),
                        self$options$censorVar))
                }

                ongoing_by_pt <- vapply(split(seq_along(status), pid), function(ix) {
                    identical(private$.valueAtLastEpisode(status[ix], end_numeric[ix]),
                              "censored")
                }, logical(1))

                idx_max <- vapply(split(seq_along(end_numeric), pid), function(ix) {
                    v <- end_numeric[ix]
                    if (all(is.na(v))) ix[1] else ix[which.max(v)]
                }, integer(1))

                rep_rows <- idx_max[names(idx_max) %in% names(ongoing_by_pt)[ongoing_by_pt]]
                if (length(rep_rows) == 0) return(NULL)

                ongoing_patients <- patient_data[rep_rows, , drop = FALSE]
                ongoing_patients <- ongoing_patients[
                    !is.na(ongoing_patients$patient_id) & !is.na(ongoing_patients$end_time), ,
                    drop = FALSE]
                if (nrow(ongoing_patients) == 0) return(NULL)

            } else {
                # No censoring variable: draw no arrows.
                #
                # This used to fall back to "whoever has the largest end time is
                # still on treatment". An arrow is a per-patient clinical claim -
                # the glossary states it means ongoing treatment at data cutoff -
                # and having the longest record is not evidence for it. The patient
                # with the longest follow-up is very often the one who died last.
                private$.addNotice(
                    "INFO",
                    .("Status arrows not drawn"),
                    .("Status arrows mark patients who were censored / still at risk at the data cutoff, which requires a censoring/event status variable. Without one that cannot be determined from the timeline alone, so no arrows are drawn. Supply a censoring variable (0/FALSE/no/censored/alive for still at risk, 1/TRUE/yes/event/dead for completed) to show them.")
                )
                return(NULL)
            }

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
                        patient_ids <- as.character(self$data[[self$options$patientID]])
                        event_times <- self$data[[event_time_var]]
                        event_labels <- as.character(self$data[[event_var]])

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
                        # jamovi never surfaces warning(); the markers would just
                        # silently vanish from the plot and the table.
                        private$.addNotice(
                            "WARNING",
                            .("Event markers could not be processed"),
                            sprintf(
                                .("Event markers were skipped because of a processing error: %s"),
                                e$message))
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
                            if (isTRUE(parsed_event_times$error)) {
                                private$.addNotice(
                                    "WARNING",
                                    .("Event times could not be parsed"),
                                    sprintf(
                                        .("Event markers were skipped. %s"),
                                        as.character(parsed_event_times$message)))
                                return(NULL)
                            }
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
                                            private$.shiftBasis(patient_data)[patient_idx],
                                            event_data$time[event_idx]
                                        )
                                        
                                        adjusted_times <- rep(NA_real_, nrow(event_data))
                                        adjusted_times[event_idx] <- lubridate::time_length(intervals, unit = self$options$timeUnit)
                                        event_data$time <- adjusted_times
                                    }
                                }
                            }
                        } else {
                            # Raw-numeric timeline: calendar-dated events cannot
                            # be placed on a numeric axis (the previous conversion
                            # anchored at 1970 via interval(<numeric>, <Date>)).
                            if (private$.looksLikeIsoDates(event_data$time)) {
                                private$.addNotice(
                                    "WARNING",
                                    .("Event markers skipped: calendar dates on a numeric timeline"),
                                    .("The event time variable contains calendar dates, but the timeline uses raw numeric times, so these events cannot be placed on the time axis. Switch Time Input Type to Date/Time (or supply numeric event times) to show them."))
                                return(NULL)
                            }
                            event_data$time <- suppressWarnings(as.numeric(as.character(event_data$time)))
                        }

                        # Raw numeric events were never shifted for relative
                        # display, so they stayed on the absolute scale while the
                        # lanes moved to 0. Align them the same way milestones are.
                        # ONLY the raw path: in datetime mode the events were
                        # already rebased calendar-aware above, and running this
                        # block on those numerics subtracted the anchor's raw
                        # epoch value a SECOND time, catapulting every event to a
                        # huge negative number that the window filter then
                        # deleted - all event markers silently vanished in the
                        # default (relative) display whenever dates were used.
                        if (self$options$timeType != "datetime" &&
                            self$options$timeDisplay == "relative" &&
                            "original_start" %in% names(patient_data) &&
                            is.numeric(event_data$time)) {
                            pidx <- match(as.character(event_data$patient_id),
                                          as.character(patient_data$patient_id))
                            shift <- private$.asNumericTime(private$.shiftBasis(patient_data))[pidx]
                            shift[is.na(shift)] <- 0
                            event_data$time <- event_data$time - shift
                        }

                        # Keep events inside each patient's OVERALL window
                        # (earliest start to LATEST end). The old numeric filter
                        # looked up a duplicate-named vector - taking the FIRST
                        # episode's end and dropping valid events in later
                        # episodes - and the old per-episode date merge duplicated
                        # any event falling inside overlapping episode windows.
                        pid_chr <- as.character(patient_data$patient_id)
                        win_lo <- tapply(private$.asNumericTime(patient_data$start_time),
                                         pid_chr, min, na.rm = TRUE)
                        win_hi <- tapply(private$.asNumericTime(patient_data$end_time),
                                         pid_chr, max, na.rm = TRUE)
                        ev_t <- private$.asNumericTime(event_data$time)
                        ev_lo <- win_lo[as.character(event_data$patient_id)]
                        ev_hi <- win_hi[as.character(event_data$patient_id)]
                        keep <- !is.na(ev_t) & !is.na(event_data$label) &
                                !is.na(ev_lo) & !is.na(ev_hi) &
                                ev_t >= ev_lo & ev_t <= ev_hi

                        # Say what was discarded. A death recorded after the last
                        # follow-up line ends - which is the normal way a death is
                        # recorded - fell outside the window, vanished from the
                        # plot AND from the event table, and the table's
                        # percentages were then computed over the survivors only:
                        # 10 events became "Scan 4 (80%), Toxicity 1 (20%)" with
                        # all three Deaths gone and nothing said.
                        n_drop <- sum(!keep)
                        if (n_drop > 0) {
                            reasons <- character(0)
                            n_no_time  <- sum(is.na(ev_t))
                            n_no_label <- sum(!is.na(ev_t) & is.na(event_data$label))
                            n_no_pt    <- sum(!is.na(ev_t) & !is.na(event_data$label) &
                                              (is.na(ev_lo) | is.na(ev_hi)))
                            inwin      <- !is.na(ev_t) & !is.na(event_data$label) &
                                          !is.na(ev_lo) & !is.na(ev_hi)
                            n_before   <- sum(inwin & ev_t < ev_lo)
                            n_after    <- sum(inwin & ev_t > ev_hi)
                            if (n_no_time > 0)
                                reasons <- c(reasons, sprintf(.("%d with no event time"), n_no_time))
                            if (n_no_label > 0)
                                reasons <- c(reasons, sprintf(.("%d with no event type"), n_no_label))
                            if (n_no_pt > 0)
                                reasons <- c(reasons, sprintf(.("%d for patients not in the analysis"), n_no_pt))
                            if (n_before > 0)
                                reasons <- c(reasons, sprintf(.("%d before the patient's first start"), n_before))
                            if (n_after > 0)
                                reasons <- c(reasons, sprintf(.("%d after the patient's last end"), n_after))
                            private$.addNotice(
                                "WARNING",
                                .("Event markers outside the patient timeline"),
                                sprintf(
                                    .("%d of %d event markers are not shown (%s). The Event Marker Summary counts and percentages below describe only the %d markers that fall inside a patient's timeline. Events recorded after follow-up ends, such as death, need an end time that covers them."),
                                    n_drop, length(keep), paste(reasons, collapse = "; "), sum(keep)))
                        }

                        event_data <- event_data[keep, c("patient_id", "time", "label"), drop = FALSE]
                    }
                }
            }

            return(event_data)
        },

        # Convert event or milestone times into numeric durations in the selected unit.
        #
        # ALWAYS measured from the patient's own earliest start, whatever the axis
        # shows. `time` arrives in the plot's coordinates, so with Relative off it
        # is a study-time POSITION: five patients starting 0/10/20/30/40 whose
        # surgery is 3 months in each published a "Median Time" of 22 instead of 3,
        # and the same data entered as dates gave 3 in both display modes. A median
        # axis position is not a statistic.
        #
        # One rule covers all four modes: subtract each patient's smallest
        # start_time in whatever coordinate system start_time currently uses
        # (zeroed under Relative, so the subtraction is a no-op there). The
        # SMALLEST, not the first row - match() re-based a patient's episode-1
        # milestone on whichever episode the file happened to list first.
        .convertTimesToNumeric = function(times, patient_ids, patient_data, unit = self$options$timeUnit) {
            ids_chr <- as.character(patient_ids)
            anchor_idx <- private$.patientAnchorIndex(
                patient_data$patient_id, patient_data$start_time)
            first_of <- !duplicated(as.character(patient_data$patient_id))
            anchors <- patient_data$start_time[anchor_idx][first_of]
            names(anchors) <- as.character(patient_data$patient_id)[first_of]
            start_vals <- anchors[ids_chr]

            if (inherits(times, c("Date", "POSIXct", "POSIXlt"))) {
                if (!inherits(start_vals, c("Date", "POSIXct", "POSIXlt")))
                    return(suppressWarnings(as.numeric(times)))
                intervals <- suppressWarnings(lubridate::interval(start_vals, times))
                return(suppressWarnings(lubridate::time_length(intervals, unit = unit)))
            }

            times_num <- suppressWarnings(as.numeric(times))
            start_num <- suppressWarnings(as.numeric(start_vals))
            if (length(start_num) != length(times_num) || all(is.na(start_num)))
                return(times_num)
            times_num - ifelse(is.na(start_num), 0, start_num)
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


            # Performance optimization: Use data.table for large datasets (>1000 rows)
            use_fast_path <- nrow(patient_data) > 1000 && requireNamespace("data.table", quietly = TRUE)

            if (use_fast_path) {
                # Fast path with data.table (5-10x faster for large datasets)
                dt <- data.table::as.data.table(patient_data)

                # Group by patient and aggregate
                summary_list <- dt[, {
                    ct <- private$.calendarTimes(
                        start_time, end_time,
                        if ("original_start" %in% names(.SD)) original_start else NULL,
                        if ("original_end" %in% names(.SD)) original_end else NULL)
                    follow_up <- private$.calculateFollowUp(ct$start, ct$end)
                    person_time <- private$.mergeIntervalsAndSum(ct$start, ct$end)
                    if (is.na(person_time) || !is.finite(person_time)) person_time <- follow_up

                    response_value <- NA_character_
                    if ("response" %in% names(.SD)) {
                        keep_resp <- !is.na(response)
                        non_missing <- as.character(response[keep_resp])
                        if (length(non_missing) > 0) {
                            # Normalise here so every consumer agrees. .getBestResponse
                            # returns the ORIGINAL string, .calculateSummaryStats
                            # normalised it before tabulating, but .updatePersonTimeTable
                            # grouped on the raw label - so "CR", "Complete Response" and
                            # "complete response" became three rows of n=1 that
                            # contradicted every other table on the page.
                            response_value <- private$.normalizeResponse(
                                private$.getBestResponse(non_missing,
                                                         start_time[keep_resp]))
                        }
                    }

                    # The NA sentinel must carry the COLUMN's type, not logical.
                    # data.table requires every group to return the same column
                    # type, so a patient whose censor value is entirely missing
                    # used to contribute a logical NA beside other patients'
                    # doubles and the whole analysis died with "Column 6 of
                    # result for group 2 is type 'double' but expecting type
                    # 'logical'" - no plot, no tables, and a message from
                    # data.table's internals. `x[NA_integer_]` is a typed NA of
                    # x's own type and works for numeric, integer, character and
                    # factor alike. `response_value` above was already correct,
                    # which is why a missing response never triggered it.
                    censor_value <- NA
                    if ("censor_status" %in% names(.SD)) {
                        censor_value <- private$.valueAtLastEpisode(censor_status, end_time)
                    }

                    group_value <- NA_character_
                    if ("patient_group" %in% names(.SD)) {
                        group_first <- patient_group[!is.na(patient_group)]
                        # as.character: the base path returns character, and a
                        # factor here kept NA-dropped levels alive in the Fisher
                        # contingency tables above 1000 rows.
                        if (length(group_first) > 0) group_value <- as.character(group_first[1])
                        # else: NA_character_ from above, matching as.character()
                    }

                    list(
                        start_time = min(start_time, na.rm = TRUE),
                        end_time = max(end_time, na.rm = TRUE),
                        follow_up = follow_up,
                        person_time = person_time,
                        response = response_value,
                        censor_status = censor_value,
                        patient_group = group_value
                    )
                }, by = patient_id]

                # Mirror the base (slow) path: only retain censor_status / patient_group
                # when they actually carry information. The j-expression above always
                # emits both columns, so an unselected censor/group variable would leave
                # an all-NA phantom column. A phantom censor_status makes
                # .calculateMedianFollowUp() take the reverse-KM branch with all patients
                # as non-events, so the curve never reaches 0.5 and median follow-up
                # flips to NA (diverging from the base path's simple median).
                if (all(is.na(summary_list$censor_status)))
                    summary_list[, censor_status := NULL]
                if (all(is.na(summary_list$patient_group)))
                    summary_list[, patient_group := NULL]

                # NOT split() here. The j-expression above has already produced
                # exactly one row per patient; splitting it into n one-row
                # data.tables so the shared bind_rows() below can reassemble them
                # cost 70% of the whole run (Rprof, 10 000 patients: bind_rows
                # 14.31 s of 20.48 s), and it is superlinear because
                # .applySorting has already made patient_id a factor with one
                # level per patient. Hand the finished table straight on.
                summary_list <- as.data.frame(summary_list)
            } else {
                # Standard path with base R (works for all dataset sizes)
                split_data <- split(patient_data, patient_data$patient_id)

                summary_list <- lapply(split_data, function(df) {
                    ct <- private$.calendarTimes(df$start_time, df$end_time,
                                                 df$original_start, df$original_end)
                    follow_up <- private$.calculateFollowUp(ct$start, ct$end)

                    # Calculate person-time by merging overlapping intervals to avoid double-counting
                    # This ensures unique observation time is counted
                    person_time <- private$.mergeIntervalsAndSum(ct$start, ct$end)
                    if (is.na(person_time) || !is.finite(person_time)) person_time <- follow_up

                # Get BEST response for ORR/DCR calculation (clinical standard in oncology)
                # Hierarchy: CR > PR > SD > PD > NE/Other
                response_value <- NA_character_
                if ("response" %in% names(df)) {
                    keep_resp <- !is.na(df$response)
                    non_missing <- as.character(df$response[keep_resp])
                    if (length(non_missing) > 0) {
                        # Normalised at the source - see the data.table path above.
                        response_value <- private$.normalizeResponse(
                            private$.getBestResponse(non_missing,
                                                     df$start_time[keep_resp]))
                    }
                }

                # Get LAST censoring status (most relevant for follow-up calculations)
                censor_value <- NA
                if ("censor_status" %in% names(df)) {
                    non_missing_censor <- df$censor_status[!is.na(df$censor_status)]
                    if (length(non_missing_censor) > 0) {
                        # Use the last non-missing censor status
                        censor_value <- private$.valueAtLastEpisode(df$censor_status, df$end_time)
                    }
                }

                # Get patient group (should be consistent per patient)
                group_value <- NA
                if ("patient_group" %in% names(df)) {
                    non_missing_group <- df$patient_group[!is.na(df$patient_group)]
                    if (length(non_missing_group) > 0) {
                        group_value <- as.character(non_missing_group[1])
                    }
                }

                start_val <- suppressWarnings(min(df$start_time, na.rm = TRUE))
                if (!is.finite(as.numeric(start_val))) start_val <- NA

                end_val <- suppressWarnings(max(df$end_time, na.rm = TRUE))
                if (!is.finite(as.numeric(end_val))) end_val <- NA

                result <- tibble::tibble(
                    patient_id = df$patient_id[1],
                    start_time = start_val,
                    end_time = end_val,
                    follow_up = follow_up,
                    person_time = person_time,
                    response = response_value
                )

                # Add censor_status if present
                if (!is.na(censor_value)) {
                    result$censor_status <- censor_value
                }

                # Add patient_group if present
                if (!is.na(group_value)) {
                    result$patient_group <- group_value
                }

                    result
                })
            }  # End of if/else for performance optimization

            # The base path still returns a list of per-patient rows; the fast
            # path is already a data.frame.
            if (is.data.frame(summary_list)) summary_list else dplyr::bind_rows(summary_list)
        },

        # Merge overlapping time intervals and sum unique observation time
        # This prevents double-counting when a patient has overlapping segments
        .mergeIntervalsAndSum = function(start_times, end_times) {
            if (length(start_times) == 0 || length(end_times) == 0) {
                return(NA_real_)
            }

            # Convert to numeric for interval operations
            starts <- private$.asNumericTime(start_times)
            ends <- private$.asNumericTime(end_times)

            # Remove invalid intervals
            valid <- !is.na(starts) & !is.na(ends) & is.finite(starts) & is.finite(ends) & ends >= starts
            if (!any(valid)) return(NA_real_)

            starts <- starts[valid]
            ends <- ends[valid]

            if (length(starts) == 0) return(NA_real_)

            # Sort intervals by start time
            ord <- order(starts)
            starts <- starts[ord]
            ends <- ends[ord]

            # Merge overlapping intervals using sweep-line algorithm
            merged_starts <- starts[1]
            merged_ends <- ends[1]

            if (length(starts) > 1) {
                for (i in 2:length(starts)) {
                    # Check if current interval overlaps or is adjacent to last merged interval
                    if (starts[i] <= merged_ends[length(merged_ends)]) {
                        # Extend the current merged interval
                        merged_ends[length(merged_ends)] <- max(merged_ends[length(merged_ends)], ends[i])
                    } else {
                        # Start a new merged interval
                        merged_starts <- c(merged_starts, starts[i])
                        merged_ends <- c(merged_ends, ends[i])
                    }
                }
            }

            # Sum the lengths of merged intervals
            total_time <- sum(merged_ends - merged_starts, na.rm = TRUE)
            if (!is.finite(total_time)) return(NA_real_)

            # Convert to the unit the results are LABELLED in.
            #
            # .asNumericTime() returns raw epoch units - seconds for POSIXct,
            # days for Date - whereas .calculateFollowUp() returns the selected
            # timeUnit. Both feed the same person_time column (the caller falls
            # back to follow_up when this returns NA), and every table and
            # interpretation string reports it as timeUnit. Without this
            # conversion a datetime dataset reports seconds under a "months"
            # label, inflating total person-time ~2.6 million-fold and making
            # the incidence rate meaningless.
            is_date <- inherits(start_times, c("Date", "POSIXct", "POSIXlt")) ||
                       inherits(end_times, c("Date", "POSIXct", "POSIXlt"))

            if (is_date) {
                # Measure each merged interval CALENDAR-aware, the same way
                # .calculateFollowUp() does. Converting the summed epoch seconds
                # with lubridate::duration() instead uses a fixed 30.4375-day
                # "month", which disagrees with the calendar months that
                # .calculateFollowUp() and the relative-display conversion produce:
                # total person-time then shifted (14.92 vs 15.00 months on a
                # two-episode test) purely from toggling timeDisplay, which is a
                # display option and must not move a reported statistic.
                # .asNumericTime() now returns epoch seconds for every time
                # class, so no per-class rescaling is needed (and a mixed
                # Date/POSIXct pair can no longer pick the wrong one).
                seg_start <- as.POSIXct(merged_starts, origin = "1970-01-01", tz = "UTC")
                seg_end   <- as.POSIXct(merged_ends, origin = "1970-01-01", tz = "UTC")

                total_time <- sum(lubridate::time_length(
                    lubridate::interval(seg_start, seg_end),
                    unit = self$options$timeUnit), na.rm = TRUE)

                if (!is.finite(total_time)) return(NA_real_)
            }

            total_time
        },

        # Single shared synonym table mapping lower-case response variants to the
        # standard RECIST abbreviation. Used by BOTH .getBestResponse (via the rank of
        # the abbreviation) and .normalizeResponse, so the mapping is defined once.
        .responseSynonymMap = function() {
            c(
                "cr" = "CR", "complete response" = "CR", "complete" = "CR",
                "pr" = "PR", "partial response" = "PR", "partial" = "PR",
                "sd" = "SD", "stable disease" = "SD", "stable" = "SD",
                "pd" = "PD", "progressive disease" = "PD", "progression" = "PD", "progressive" = "PD",
                "ne" = "NE", "not evaluable" = "NE", "na" = "NE"
            )
        },

        # Get best response based on oncology hierarchy
        # CR (Complete Response) > PR (Partial Response) > SD (Stable Disease) > PD (Progressive Disease) > Other
        .getBestResponse = function(responses, order_by = NULL) {
            if (length(responses) == 0) return(NA_character_)

            syn <- private$.responseSynonymMap()
            # Rank of each standard abbreviation (lower rank = better response)
            response_rank <- c("CR" = 1, "PR" = 2, "SD" = 3, "PD" = 4, "NE" = 5)

            # RECIST 1.1: the best overall response is the best assessment
            # recorded from the start of treatment UNTIL progression. An
            # assessment made after the disease has progressed does not
            # contribute to it. Without this, four patients who each progressed
            # in their first episode and were then recorded CR or SD in a second
            # episode were reported as ORR 50% / DCR 100% when the correct
            # answer is 0% / 0%.
            #
            # `order_by` carries each assessment's time so the sequence is
            # chronological rather than whatever order the rows arrived in.
            # Without it the truncation would depend on row order, which is the
            # same class of defect.
            if (!is.null(order_by) && length(order_by) == length(responses)) {
                ord <- order(order_by, na.last = TRUE)
                responses <- responses[ord]
            }

            responses_lower <- tolower(trimws(responses))

            std_all <- unname(syn[responses_lower])
            pd_at <- which(!is.na(std_all) & std_all == "PD")
            if (length(pd_at) > 0) {
                keep <- seq_len(pd_at[1])
                responses <- responses[keep]
                responses_lower <- responses_lower[keep]
            }

            # Find the best (lowest ranked) response
            best_rank <- Inf
            best_response <- responses[1]  # Default to first if no match

            for (i in seq_along(responses_lower)) {
                std <- unname(syn[responses_lower[i]])  # NA if unrecognized
                rank <- if (!is.na(std)) response_rank[[std]] else NA
                if (!is.na(rank) && rank < best_rank) {
                    best_rank <- rank
                    best_response <- responses[i]  # Keep original case
                }
            }

            # If no recognized response, return the first one
            if (is.infinite(best_rank)) {
                return(responses[1])
            }

            best_response
        },

        # Normalize response category to standard abbreviation
        # Handles case-insensitive matching for clinical response categories
        # Returns: "CR", "PR", "SD", "PD", "NE", or the original value if unrecognized
        # Vectorised form. The scalar version below is driven element-by-element
        # over every row or patient at four sites; measured over 40 000 values
        # that costs 0.93 s against 0.010 s for one vectorised lookup.
        .normalizeResponses = function(x) {
            x <- as.character(x)
            syn <- private$.responseSynonymMap()
            out <- unname(syn[tolower(trimws(x))])
            keep <- is.na(out)
            out[keep] <- x[keep]
            out
        },

        .normalizeResponse = function(response_str) {
            if (is.na(response_str) || length(response_str) == 0) return(response_str)

            response_lower <- tolower(trimws(response_str))
            std <- unname(private$.responseSynonymMap()[response_lower])  # NA if unrecognized
            if (!is.na(std)) return(std)

            # Return original if not recognized
            return(response_str)
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

        # The values follow-up and person-time must be measured on.
        #
        # Under "Relative (all start from 0)" start_time/end_time have already been
        # rewritten as durations measured FROM THE PATIENT'S ANCHOR, and a calendar
        # month measured from the anchor is not the same length as one measured from
        # the episode's own start, so subtracting two of them drifts. The same
        # dataset reported 3.98 months of person-time relative and 4.02 absolute
        # (calendar answer 4.0207). timeDisplay decides what the axis shows; it must
        # not move a reported statistic. Both estimators therefore work from the
        # original Date/POSIXct values whenever the relative conversion kept them.
        .calendarTimes = function(start_times, end_times, original_start, original_end) {
            if (!is.null(original_start) && !is.null(original_end) &&
                inherits(original_start, c("Date", "POSIXct", "POSIXlt")))
                return(list(start = original_start, end = original_end))
            list(start = start_times, end = end_times)
        },

        # Convert time-like objects to numeric for comparisons
        # Every time class on ONE scale: epoch seconds.
        #
        # This used to return each class's own raw epoch unit - DAYS for Date,
        # SECONDS for POSIXct - and every caller compares a start against an end
        # (.mergeIntervalsAndSum, the event-marker window filter, the sort
        # durations, the export columns). A Date start with a POSIXct end
        # therefore compared 18262 against 1583020800: measured, Total
        # Person-Time came out 104,106,728 months beside a correct Mean Duration
        # of 2.5, silently, because .calculateFollowUp builds a
        # lubridate::interval on the original objects and are immune. Nothing
        # here is ever compared against an external day-scale constant, so one
        # common scale is safe for all six call sites.
        .asNumericTime = function(x) {
            if (inherits(x, "Date")) {
                return(as.numeric(x) * 86400)
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
                # same 30.4375-day month as every other conversion in this file
                # (the old 4.34524 constant implied a 365-day year)
                weeks = base_months * 30.4375 / 7,
                years = base_months / 12,
                base_months
            )

            reference_values[reference_values <= max_duration * 1.1]
        },

        # Median follow-up.
        #
        # Returns BOTH the value and the estimator actually used. The reverse
        # Kaplan-Meier method (Schemper & Smith 1996) needs censoring information;
        # without it - or when the censoring variable uses a coding this cannot
        # classify - the function falls back to the plain median of observed
        # durations. That fallback used to be invisible: the results row said
        # "(reverse Kaplan-Meier)" unconditionally, so a Yes/No indicator, which
        # matched none of the recognised tokens, made every patient an event, the
        # reverse curve never reached 0.5, and the naive median was published
        # under the reverse-KM name (33% low in the reviewer's test case).
        .calculateMedianFollowUp = function(patient_summary) {
            none <- list(value = NA_real_, method = "none")
            if (nrow(patient_summary) == 0) return(none)

            follow_up_times <- patient_summary$follow_up
            valid_idx <- !is.na(follow_up_times) & is.finite(follow_up_times)
            if (sum(valid_idx) == 0) return(none)

            fu <- follow_up_times[valid_idx]
            observed <- list(value = stats::median(fu), method = "observed")

            if (!("censor_status" %in% names(patient_summary))) return(observed)

            status <- private$.classifyCensoring(patient_summary$censor_status[valid_idx])

            # A PARTIALLY unrecognised coding is disclosed too: unclassifiable
            # patients are treated as completed follow-up (reverse-KM events),
            # which biases median follow-up downward if they were in fact
            # ongoing - previously only the all-unrecognised case said anything.
            raw_vals <- patient_summary$censor_status[valid_idx]
            n_unrec <- sum(is.na(status) & !is.na(raw_vals))

            # A patient with NO censoring value at all is a different case from
            # an unrecognised one, and it used to be counted by neither check:
            # `status %in% "censored"` is FALSE for NA, so as.numeric() made
            # them 0 - a terminal event - which drags the reverse Kaplan-Meier
            # median downward exactly as if they had completed follow-up. They
            # are excluded from the estimate instead, and the exclusion is said
            # out loud.
            n_missing_status <- sum(is.na(raw_vals))
            if (n_missing_status > 0) {
                private$.addNotice(
                    "WARNING",
                    .("Censoring status missing for some patients"),
                    sprintf(
                        .("%d of %d patients have no censoring/event value. They are excluded from the reverse Kaplan-Meier median follow-up rather than counted as completed follow-up, which would bias it downward. Their timelines are still drawn, without a status arrow."),
                        n_missing_status, length(status)))
                keep_status <- !is.na(raw_vals)
                fu <- fu[keep_status]
                status <- status[keep_status]
                raw_vals <- raw_vals[keep_status]
                if (length(fu) == 0) return(none)
                observed <- list(value = stats::median(fu), method = "observed")
            }
            if (n_unrec > 0 && !all(is.na(status))) {
                private$.addNotice(
                    "WARNING",
                    .("Some censoring values not recognised"),
                    sprintf(
                        .("%d of %d censoring/event values could not be interpreted; those patients are treated as completed follow-up (events) in the reverse Kaplan-Meier estimate and receive no status arrow. Use 0/FALSE/no/censored/alive for ongoing and 1/TRUE/yes/event/dead for completed."),
                        n_unrec, length(status)))
            }

            # Nothing classifiable -> say so rather than silently degrading.
            if (all(is.na(status))) {
                private$.addNotice(
                    "WARNING",
                    .("Censoring variable not recognised"),
                    .("None of the values in the censoring/event status variable could be interpreted as censored or event. Median follow-up is therefore the plain median of observed durations, not the reverse Kaplan-Meier estimate, and ongoing-status arrows may be wrong. Use 0/FALSE/no/censored/alive for ongoing patients and 1/TRUE/yes/event/dead for completed follow-up.")
                )
                return(list(value = observed$value, method = "unrecognised"))
            }

            # Reverse KM: censored patients become the "events". The estimator
            # is shared -- .medianFollowUp() in R/utils-followup.R -- which also
            # handles the not-estimable case (the reversed curve never reaching
            # 50%) and reports why. Only the status CLASSIFICATION above is
            # specific to this analysis, so only that stays here.
            mfu <- .medianFollowUp(fu, as.numeric(status %in% "censored"))
            if (!isTRUE(mfu$reverse)) {
                # The censoring variable WAS supplied and every value WAS
                # classified - the reverse curve simply never reaches 50%, or no
                # patient was censored. Labelling that "no censoring
                # information" told the reader the opposite of what happened,
                # and mfu$reason, which says which of the two it was, had no
                # consumer anywhere in the module.
                return(list(value = observed$value, method = "not_estimable",
                            reason = mfu$reason %||% "",
                            reason_code = mfu$reason_code %||% ""))
            }

            list(value = mfu$value, method = "reverse_km",
                 ci_lower = mfu$ci_lower, ci_upper = mfu$ci_upper,
                 n_total = mfu$n_total, n_censored = mfu$n_censored,
                 reason = "", reason_code = "")
        },

        # Map a censoring/event status value to "censored", "event", or NA.
        # Shared by median follow-up and the ongoing-status arrows so the two
        # cannot disagree about what a given coding means.
        # A patient's status is whatever was true at their LAST episode, which
        # is the one with the greatest end time - not whichever row the file
        # happened to list last. Reading it by row order meant re-sorting the
        # same data changed the ongoing-treatment arrows and the reverse
        # Kaplan-Meier median follow-up, while the arrow's POSITION was already
        # (correctly) taken from which.max(end_time). Returns a typed NA when
        # the patient has no non-missing value at all.
        .valueAtLastEpisode = function(values, times) {
            keep <- !is.na(values)
            if (!any(keep)) return(values[NA_integer_])
            v <- values[keep]
            t <- if (length(times) == length(values)) times[keep] else rep(NA_real_, length(v))
            if (all(is.na(t))) return(v[length(v)])
            v[which.max(t)]
        },

        .classifyCensoring = function(x) {
            v <- tolower(trimws(as.character(x)))
            num <- suppressWarnings(as.numeric(v))

            out <- rep(NA_character_, length(v))
            out[v %in% c("0", "false", "f", "no", "n", "censored", "cens",
                         "alive", "ongoing", "active", "continuing")] <- "censored"
            out[v %in% c("1", "true", "t", "yes", "y", "event", "dead", "died",
                         "death", "progressed", "progression", "completed")] <- "event"

            # Numeric coding. 0/1 is the documented default and the only one
            # this analysis may assume silently.
            #
            # {1, 2} is survival::Surv's convention (1 = censored, 2 = event).
            # The old rule sent every non-zero value to "event", so a 1/2 column
            # became ALL events: no arrows were drawn, the reverse Kaplan-Meier
            # was abandoned for "observed durations; no censoring information",
            # and the reported median follow-up came out about half the true
            # value with nothing said. Recognise that coding and record it, so
            # the run can disclose the assumption it made.
            obs <- num[!is.na(num)]
            surv_coding <- length(obs) > 0 && all(obs %in% c(1, 2)) && any(obs == 2)
            if (surv_coding) {
                out[!is.na(num) & num == 1] <- "censored"
                out[!is.na(num) & num == 2] <- "event"
                private$.censor_coding <- "surv12"
            } else {
                out[is.na(out) & !is.na(num) & num == 0] <- "censored"
                out[is.na(out) & !is.na(num) & num != 0] <- "event"
                if (length(obs) > 0 && all(obs %in% c(0, 1)))
                    private$.censor_coding <- "zero_one"
            }
            out[is.na(v)] <- NA_character_
            out
        },

        # Calculate comprehensive summary statistics using patient-level data
        .calculateSummaryStats = function(patient_data) {
            patient_summary <- private$.summarizeByPatient(patient_data)
            follow_up_durations <- patient_summary$follow_up
            valid_follow_up <- follow_up_durations[!is.na(follow_up_durations)]

            # Use reverse Kaplan-Meier for median follow-up (gold standard with censoring)
            median_fu_res <- private$.calculateMedianFollowUp(patient_summary)
            median_fu <- median_fu_res$value

            stats <- list(
                n_patients = nrow(patient_summary),
                n_observations = nrow(patient_data),
                # Two different estimators, kept separate and named, because they
                # answer different questions and disagree under censoring:
                #   median_followup_km - reverse Kaplan-Meier (Schemper & Smith 1996),
                #     estimates how long patients WOULD be followed. Correct for
                #     "median follow-up".
                #   median_duration    - plain median of the OBSERVED durations, the
                #     partner of mean/SD/Q1/Q3 below.
                # Reporting the KM value beside a naive mean and naive quartiles made
                # the summary incoherent: the median could sit outside its own IQR
                # (verified: KM median 23.0 with Q1 4.75, Q3 22.5), and median >> mean
                # read as strong skew that was purely an artefact of mixing estimators.
                median_followup_km = median_fu,
                median_followup_method = median_fu_res$method,
                # Reverse-KM median carries a confidence interval; the naive
                # fallbacks do not, so these stay NA and the metrics table shows
                # a blank rather than a fabricated interval.
                median_followup_ci_lower = median_fu_res$ci_lower %||% NA_real_,
                median_followup_ci_upper = median_fu_res$ci_upper %||% NA_real_,
                median_followup_reason = median_fu_res$reason %||% "",
                median_followup_reason_code = median_fu_res$reason_code %||% "",
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
                # Normalize response categories to standard abbreviations (CR, PR, SD, PD)
                # This ensures case-insensitive matching and handles various input formats
                normalized_responses <- private$.normalizeResponses(patient_summary$response)

                response_summary <- table(normalized_responses, useNA = "no")
                if (length(response_summary) > 0) {
                    # Percentages over EVERY patient in the analysis, the same
                    # denominator ORR and DCR use. These rows used to divide by
                    # the patients who had any recorded response, so on a
                    # 12-patient cohort with one missing response they were x/11
                    # while the ORR beside them was x/9 and the Fisher test
                    # below them used yet another n. A reader could not add the
                    # rows up or reconcile them with anything else on the page.
                    n_all_patients <- nrow(patient_summary)
                    response_pct <- response_summary / n_all_patients * 100

                    stats$response_counts <- as.numeric(response_summary)
                    names(stats$response_counts) <- names(response_summary)

                    stats$response_percentages <- as.numeric(response_pct)
                    names(stats$response_percentages) <- names(response_pct)

                    # The patients with no recorded response at all are the
                    # remainder; carry them so the rows account for everyone.
                    stats$n_response_missing <- n_all_patients - sum(response_summary)
                    stats$response_counts_named <- response_summary
                    stats$response_denominator <- n_all_patients
                }
            }

            stats
        },
        
        # Generate comprehensive clinical interpretation
        .generateClinicalInterpretation = function(stats, patient_data) {
            interpretation <- list()
            
            # Timeline interpretation
            interpretation$timeline <- sprintf(
                # "Median follow-up" is the reverse-KM quantity reported in the
                # Advanced Metrics table. What is printed here is the plain median
                # of observed durations, shown beside the observed range - so it is
                # named for what it is. Two different numbers under one name had
                # the interpretation text and the metrics table disagreeing (10.5
                # vs 15.5 months in the reviewer's case).
                .("Study included %d patients with %d timeline observations. Median observed duration was %.1f %s (range: %.1f to %.1f %s)."),
                stats$n_patients,
                stats$n_observations,
                stats$median_duration,
                private$.timeUnitWord(),
                stats$min_duration,
                stats$max_duration,
                private$.timeUnitWord()
            )
            
            # Person-time analysis
            interpretation$person_time <- sprintf(
                .("Total person-time: %.1f %s. Average follow-up per patient: %.1f %s."),
                stats$total_person_time,
                private$.timeUnitWord(),
                stats$mean_follow_up,
                private$.timeUnitWord()
            )
            
            # Response interpretation if available
            if (!is.null(stats$response_counts)) {
                # which.max() silently takes the FIRST of a tie, in whatever
                # order the table happens to be in, and the tally ignored
                # patients with no recorded response entirely - so 3 CR and 5 NA
                # read "Most common response was CR (37.5%)" beside a summary row
                # saying "No recorded response (5/8) 62.5". Count the
                # no-response group as its own category and name a tie.
                counts <- stats$response_counts
                if (isTRUE(stats$n_response_missing > 0))
                    counts <- c(counts,
                                stats::setNames(stats$n_response_missing,
                                                .("no recorded response")))
                top <- names(counts)[counts == max(counts)]
                denom <- if (!is.null(stats$response_denominator))
                    stats$response_denominator else sum(counts)
                best_pct <- max(counts) / denom * 100

                interpretation$response <- if (length(top) > 1)
                    sprintf(
                        .("Most common response was a tie between %s, each in %.1f%% of patients."),
                        htmltools::htmlEscape(paste(top, collapse = ", ")), best_pct)
                else
                    sprintf(
                        .("Most common response was %s (%.1f%% of patients)."),
                        htmltools::htmlEscape(top), best_pct)
            }
            
            return(interpretation)
        },
        

        # Add clinical profile notices based on data characteristics
        .addClinicalProfileNotices = function(patient_data, stats) {
            # STRONG_WARNING: Small sample size (<10 patients)
            if (!is.null(stats$n_patients) && stats$n_patients < 10) {
                private$.addNotice('STRONG_WARNING', .("Small sample size"), sprintf(
                    .("Very small sample size (n=%d patients). Results may have limited statistical power and generalizability. Consider interpreting findings as exploratory."),
                    stats$n_patients
                ))
            }
        },

        # library-audit 2026-09-16 meddecide [LOW] DONE (same class): advancedMetrics has a fixed row set, so .init()
        # scaffolds the rows and .run() fills them with setRow()
        # Four follow-up rows whenever person-time analysis is on, plus ORR and DCR
        # when a response variable is analysed (left blank when the data cannot
        # support them). summaryData is not converted: its per-category response
        # rows depend on the levels present in the data.
        .medianFollowUpLabel = function(method) {
            switch(
                method %||% "observed",
                reverse_km = .("Median Follow-up Time (reverse Kaplan-Meier)"),
                unrecognised = .("Median Follow-up Time (observed durations; censoring not recognised)"),
                not_estimable = .("Median Follow-up Time (observed durations; reverse Kaplan-Meier not estimable)"),
                .("Median Follow-up Time (observed durations; no censoring information)")
            )
        },

        # The time unit as a WORD, for text people read.
        #
        # `self$options$timeUnit` is the option KEY - "months", "days" - and it
        # was spliced straight into two dozen translated sentences through %s
        # and {unit}. Every one of those sentences translated while the unit
        # stayed in English, which is the failure mode only a non-English user
        # ever sees. lubridate still gets the key; this is for display only.
        # The date format as the DROPDOWN shows it, for text people read.
        #
        # self$options$dateFormat is the option key ("ymd"), and three translated
        # sentences spliced it in raw while telling the user to check a control
        # labelled "YYYY-MM-DD". lubridate still gets the key; this is display
        # only, exactly like .timeUnitWord() below.
        .dateFormatWord = function(fmt = self$options$dateFormat) {
            switch(as.character(fmt),
                ymdhms = "YYYY-MM-DD HH:MM:SS",
                ymd = "YYYY-MM-DD",
                ydm = "YYYY-DD-MM",
                mdy = "MM-DD-YYYY",
                myd = "MM-YYYY-DD",
                dmy = "DD-MM-YYYY",
                dym = "DD-YYYY-MM",
                as.character(fmt))
        },

        .timeUnitWord = function(unit = self$options$timeUnit) {
            switch(as.character(unit),
                days = .("days"),
                weeks = .("weeks"),
                months = .("months"),
                years = .("years"),
                as.character(unit))
        },

        # The translated form of .medianFollowUp()'s fallback reason.
        #
        # The shared helper R/utils-followup.R cannot call .() - it is a
        # file-level function with no `self`, and jmvcore's translator reads
        # `self` from the calling frame - so it returns a code and the sentence
        # is chosen here. Falls back to the helper's English prose for any code
        # this method does not know, so a new code degrades rather than vanishes.
        .followUpReasonText = function(code, fallback_text = "") {
            switch(as.character(code),
                no_censoring = .("No patient was censored, so the reversed curve has no events and never reaches 50%. Every patient was observed to the terminal outcome, so the observed times are the complete follow-up."),
                never_reaches_50 = .("The reversed Kaplan-Meier curve never falls to 50%, so its median is undefined. That depends on WHEN patients were still under observation, not merely how many: a few censored late can make the median estimable while many censored early cannot."),
                fallback_text)
        },

        # Format the reverse-KM confidence interval.
        #
        # The whole interval used to be dropped whenever either bound was NA,
        # and with a reverse Kaplan-Meier the UPPER bound routinely is: the
        # reversed curve often never falls far enough for its upper confidence
        # limit to be reached. survfit reported "18 (95% CI 12 - NA)" and the
        # module showed an empty cell, throwing away a lower bound that is the
        # useful half. "NR" (not reached) is the manuscript convention.
        .followUpCIText = function(lower, upper) {
            lower <- suppressWarnings(as.numeric(lower %||% NA_real_))
            upper <- suppressWarnings(as.numeric(upper %||% NA_real_))
            if (!is.finite(lower) && !is.finite(upper)) return(NA_character_)
            fmt <- function(x, nr) if (is.finite(x)) sprintf("%.2f", x) else nr
            paste(fmt(lower, .("NE")), "-", fmt(upper, .("NR")))
        },

        # The follow-up estimator is known only in .run(); .init() labels that row
        # with the one the options imply (reverse KM needs a censoring variable).
        .advancedMetricLabels = function() {
            # ORR and DCR belong to "Response analysis", not to "Person-time
            # analysis". They used to require BOTH, so unticking person-time
            # removed the response rates as well - the headline numbers of the
            # analysis - while leaving a person-time paragraph in the
            # interpretation that no longer had a table behind it.
            labels <- character(0)
            if (isTRUE(self$options$personTimeAnalysis))
                labels <- c(
                    median_followup = private$.medianFollowUpLabel(
                        if (is.null(self$options$censorVar)) "observed" else "reverse_km"),
                    iqr = .("Interquartile Range (observed durations)"),
                    person_time = .("Total Study Person-Time"),
                    followup_density = .("Follow-up Density"))
            if (isTRUE(self$options$responseAnalysis) && !is.null(self$options$responseVar))
                labels <- c(labels,
                    orr = .("Objective Response Rate (ORR)"),
                    dcr = .("Disease Control Rate (DCR)"))
            labels
        },

        # Apply clinical preset configurations with context

        # The five fixed summary rows, in order. Factored out of .init() so that
        # .updateSummaryTable() can rebuild them when it has to prune a stale
        # response row, and so the labels are re-emitted every run rather than
        # surviving from whichever language .init() first ran in.
        # ("Mean Follow-up" was dropped: it was the identical statistic as
        # "Mean Duration" printed twice under two clinical names.)
        .summaryMetricLabels = function() {
            c(
                .("Number of Patients"),
                .("Total Observations"),
                .("Median Duration (observed)"),
                .("Mean Duration"),
                .("Total Person-Time")
            )
        },

        # Put the summary table back to its .init() state: the five fixed rows with
        # no values, and no response rows at all. Called at the top of every run so
        # an aborted run cannot leave last run's numbers under an error message,
        # and by .updateSummaryTable() when the set of response categories changes.
        .resetSummaryTable = function() {
            tbl <- self$results$summary
            labs <- private$.summaryMetricLabels()
            tbl$deleteRows()
            for (i in seq_along(labs))
                tbl$addRow(rowKey = i, values = list(metric = labs[i], value = NA_real_))
        },

        .init = function() {
            # (The missing-variable ERROR notice and the instructions panel used to
            # be raised here too. .run() blanks .noticeList and `instructions` at
            # its top and then re-creates both verbatim, and jamovi always calls
            # .run() after .init(), so that copy could never reach the user.)

            # Only the values are computed in .run(), which fills them with
            # setRow(); the response-rate rows that follow depend on the levels
            # actually present and are managed by .updateSummaryTable().
            summary_metrics <- private$.summaryMetricLabels()
            if (self$results$summary$rowCount == 0)
                for (i in seq_along(summary_metrics))
                    self$results$summary$addRow(
                        rowKey = i, values = list(metric = summary_metrics[i]))

            # Unlike the summary block above, this row set is NOT fixed:
            # .advancedMetricLabels() returns 0, 2, 4 or 6 keys depending on
            # personTimeAnalysis and responseAnalysis. A "rowCount == 0" guard
            # copied from above would freeze whichever set was built first, and
            # .run() fills these with setRow(), which REJECTS a missing key - so a
            # frozen set would abort the analysis rather than merely duplicate
            # rows. An unconditional addRow loop is wrong too: addRow accepts
            # duplicate keys silently, so a second .init() doubled the table.
            #
            # Rebuild only when the set actually differs. An unconditional
            # deleteRows() would do, except that jmvcore's Table$deleteRows()
            # clears .rowKeys and .rowCount but NOT .rowNames: rebuilding to an
            # EMPTY set then leaves phantom row names with no keys, which
            # Table$fromProtoBuf indexes out of bounds. Comparing first means
            # deleteRows() never runs on the path that repeats an identical set.
            advanced_labels <- private$.advancedMetricLabels()
            want <- names(advanced_labels)
            have <- as.character(unlist(self$results$advancedMetrics$rowKeys))
            if (!identical(have, as.character(want))) {
                self$results$advancedMetrics$deleteRows()
                for (key in want)
                    self$results$advancedMetrics$addRow(rowKey = key,
                        values = list(metric_name = advanced_labels[[key]]))
            }
        },

        .run = function() {
            # Reset notice collection AND re-render immediately: a run that
            # produces zero notices must clear the previous run's text, which
            # only .addNotice() used to do.
            private$.noticeList <- list()
            private$.renderNotices()

            # `instructions` has no clearWith rule covering every trigger, so an
            # earlier run's guidance would otherwise linger. Severity messages no
            # longer live here or in a second Html item - they all go through
            # .addNotice(), which the .noticeList reset above already clears.
            self$results$instructions$setContent('')

            # Clear every repopulated output too. Each .update*() starts with its
            # own deleteRows(), but those all run AFTER the four early returns
            # below, so a run that aborts on validation used to leave the previous
            # run's tables and prose sitting under the error. clearWith does not
            # cover it: editing a cell in the spreadsheet changes no option.
            for (tbl in c("personTimeTable", "milestoneTable", "eventMarkerTable",
                          "groupComparisonTest", "timelineData", "summaryData"))
                self$results[[tbl]]$deleteRows()
            self$results$interpretation$setContent('')
            self$results$copyReadyReport$setContent('')

            # summary keeps its five scaffolded rows (setRow rejects a missing key)
            # but must not keep their values, nor any response row from last time.
            private$.resetSummaryTable()

            # advancedMetrics rows are scaffolded in .init(), and jamovi can restore
            # a previous run's values into them. Blank them here, so an early
            # return or error below leaves empty cells, not the last numbers.
            advanced_labels <- private$.advancedMetricLabels()
            scaffolded <- as.character(unlist(self$results$advancedMetrics$rowKeys))
            for (key in names(advanced_labels))
                if (key %in% scaffolded)
                    self$results$advancedMetrics$setRow(rowKey = key, values = list(
                        metric_name = advanced_labels[[key]], metric_value = NA_real_,
                        confidence_interval = NA_character_, metric_unit = NA_character_,
                        clinical_interpretation = NA_character_))

            # Static educational panels: populate whenever requested, BEFORE any
            # early return - previously an incomplete selection or a validation
            # abort left visible-but-empty "Clinical Glossary" / "About" shells.
            if (self$options$showGlossary)
                private$.generateClinicalGlossary()
            if (self$options$showAbout)
                private$.generateAboutAnalysis()

            # Enhanced instructions with comprehensive guidance
            n_required <- sum(!vapply(
                list(self$options$patientID, self$options$startTime, self$options$endTime),
                is.null, logical(1)))

            if (n_required < 3L) {
                # Setting up an analysis is not an error. An analysis the user has just opened
                # has nothing assigned yet, and one that is half filled in is simply not
                # finished - in both cases the answer is guidance, not a red ERROR telling
                # them they did something wrong. Nothing assigned: the instructions panel
                # below already names the three variables and what each is for, so it speaks
                # for itself. Partly assigned: say which boxes are still empty, as a NOTE.
                #
                # Each sentence is a whole translatable unit joined with paste(), the pattern
                # the report builders use, rather than splicing variable names into a
                # template - a spliced noun phrase cannot be inflected correctly in Turkish.
                if (n_required > 0L) {
                    still_empty <- c(
                        if (is.null(self$options$patientID)) .("Patient ID is still empty."),
                        if (is.null(self$options$startTime)) .("Start Time is still empty."),
                        if (is.null(self$options$endTime))   .("End Time is still empty.")
                    )
                    private$.addNotice('INFO', .("Keep going - a few variables to add"),
                        paste(c(still_empty,
                                .("Fill them in under Core Data Variables and the plot will appear.")),
                              collapse = " "))
                }

                # Keep detailed HTML guidance
                instructions <- private$.generateInstructions()
                self$results$instructions$setContent(instructions)
                return()
            }

            # Validate and process data with comprehensive error handling
            tryCatch({
                validation_result <- private$.validateAndProcessData()

                # Check for data type mismatch (Date/Time selected but numeric data)
                if (isTRUE(validation_result$data_type_mismatch)) {
                    # Notice rendered via HTML guidance panel and ERROR notice
                    # Escape user-derived example values before HTML interpolation
                    safe_examples <- if (!is.null(validation_result$examples)) {
                        htmltools::htmlEscape(as.character(validation_result$examples))
                    } else {
                        character(0)
                    }
                    # Detailed HTML guidance (body in R/swimmerplot-html.R)
                    mismatch_guidance <- swimmerplot_mismatch_guidance_html(self, safe_examples)
                    self$results$instructions$setContent(mismatch_guidance)
                    # Also surface via the ERROR-notice channel for consistent,
                    # machine-readable error reporting.
                    private$.addNotice('ERROR', .("Data type mismatch"),
                                       .("Date/Time input type was selected but the time variables contain numeric values. Switch Time Input Type to Raw Values (or correct the data) and re-run."))
                    return()  # Stop here, don't process further
                }

                # Check if dates were detected (not an error, just guidance needed)
                if (isTRUE(validation_result$date_detected)) {
                    # Escape user-derived format / example strings before HTML interpolation
                    safe_format <- htmltools::htmlEscape(as.character(validation_result$format %||% ""))
                    safe_examples_date <- if (!is.null(validation_result$examples)) {
                        htmltools::htmlEscape(as.character(validation_result$examples))
                    } else {
                        character(0)
                    }
                    # (body in R/swimmerplot-html.R)
                    date_guidance <- swimmerplot_date_guidance_html(self, safe_format, safe_examples_date)
                    self$results$instructions$setContent(date_guidance)
                    # The analysis stops here. Without this the only notice on
                    # the page was "Time units", and every summary value came
                    # out NA with nothing saying why.
                    private$.addNotice("ERROR", .("Dates found on a raw numeric timeline"), sprintf(
                        .("The start/end variables look like calendar dates (%s), but Time Input Type is set to Raw Values, so no timeline could be built. Switch Time Input Type to Date/Time and choose the matching Date Format."),
                        as.character(validation_result$format %||% "")))
                    private$.renderNotices()
                    return()  # Stop here, don't process further
                }

                # Check for validation errors
                if (isTRUE(validation_result$error)) {
                    error_msg <- paste0(
                        "<div style='color: red; padding: 15px; border: 1px solid red; border-radius: 5px; margin: 10px;'>",
                        "<h4>", .("Data Validation Error"), "</h4>",
                        .fmt(
                            .("<p><strong>Error:</strong> {message}</p>"),
                            message = htmltools::htmlEscape(
                                as.character(validation_result$message)
                            )
                        ),
                        "<p><strong>", .("Please check:"), "</strong></p>",
                        "<ul>",
                        "<li>", .("All required variables are selected"), "</li>",
                        "<li>", .("Data contains valid values"), "</li>",
                        "<li>", .("End times are greater than or equal to start times"), "</li>",
                        "<li>", .("Check for negative follow-up times or unrealistic durations"), "</li>",
                        "</ul>",
                        "</div>"
                    )
                    self$results$instructions$setContent(error_msg)
                    # library-audit 2026-09-22 OncoPath [MEDIUM] DEFERRED: this is the same
                    #   fatal-via-banner shape as waterfall's processing failure, which now calls
                    #   jmvcore::reject(). It cannot here: .run() wraps this whole block in a
                    #   catch-all tryCatch(error=) at the "Validate and process data" step, and that
                    #   handler does not re-raise, so a reject() condition would be swallowed and the
                    #   analysis would report success (guide sections 16 and 25). Revisit when that
                    #   catch-all is narrowed to the third-party calls that need it.
                    private$.addNotice('ERROR', .("Data validation error"),
                                       as.character(validation_result$message))
                    return()
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

                # One notice, not one per message: .addNotice() dedupes only on an
                # exact (type, title, content) match, so four messages under the
                # same title would print as four separate "NOTE: Analysis
                # Information" blocks.
                if (length(warning_messages) > 0)
                    private$.addNotice("INFO", .("Analysis Information"),
                        paste(warning_messages, collapse = "\n"))
                # Show note if an absolute axis makes a duration reference line meaningless.
                # This used to be gated on dates alone, so raw numeric times with
                # Relative off drew the median and protocol lines at duration
                # values on a study-time axis and said nothing.
                is_date_scale <- inherits(patient_data$start_time, c("Date", "POSIXct"))
                if (!private$.isDurationAxis(patient_data) &&
                    self$options$referenceLines %in% c("median", "protocol")) {
                    advice <- if (is_date_scale)
                        .("Use 'Custom Time' with 'Custom Reference Date' or a time offset instead.")
                    else
                        .("Switch Time Display to 'Relative (all start from 0)' to show them, or use 'Custom Time'.")
                    private$.addNotice("WARNING",
                        .("Reference lines on an absolute axis"),
                        paste(.("Median/Protocol reference lines measure a duration from each patient's own start, so they cannot be placed on an absolute axis where patients begin at different points."),
                              advice))
                }
                if (is_date_scale && identical(self$options$timeDisplay, "absolute")) {
                    if (self$options$referenceLines %in% c("custom")) {
                        # If custom selected but no date provided, we fall back to offset; inform the user once
                        cref_str <- tryCatch(self$options$customReferenceDate, error = function(e) NULL)
                        cref_blank <- is.null(cref_str) || nchar(trimws(as.character(cref_str))) == 0
                        if (cref_blank) {
                            private$.addNotice("INFO",
                                .("Custom reference in absolute mode"),
                                .("No 'Custom Reference Date' provided; using 'Custom Reference Time' as an offset from the earliest start date."))
                        } else if (is.null(private$.parseCustomReferenceDate(cref_str))) {
                            # A date that cannot be parsed used to fall through to
                            # the same silent offset fallback as an EMPTY box, so
                            # the reference line was drawn somewhere the user did
                            # not ask for and nothing said the date had been
                            # discarded. It is parsed with the Date Format chosen
                            # for the data, so an ISO string against dmy data
                            # fails - which is why the format is named here.
                            private$.addNotice("WARNING", .("Custom reference date not understood"), sprintf(
                                .("The Custom Reference Date '%s' could not be read using the selected Date Format (%s), so it was ignored and the reference line falls back to Custom Reference Time measured from the earliest start date. Enter the date in the same format as your data, or clear the box to use the offset deliberately."),
                                as.character(cref_str), private$.dateFormatWord()))
                        }
                    }
                }

                # Calculate comprehensive statistics
                stats <- private$.calculateSummaryStats(patient_data)
                
                milestone_data <- private$.processMilestones(patient_data)
                event_data <- private$.processEventMarkers(patient_data)
                arrow_data <- private$.processOngoingStatus(patient_data, stats)
                interpretation <- private$.generateClinicalInterpretation(stats, patient_data)

                # NOTE: .generateClinicalSummary()/.displayClinicalSummary() were dead:
                # they wrote into `interpretation`, which is then either overwritten by
                # .generateInterpretationOutput() (showInterpretation = TRUE) or hidden
                # (visible:(showInterpretation)). The summary was never shown, so the
                # calls were removed to avoid wasted computation and confusion.

                # Update summary table
                private$.updateSummaryTable(stats)
                
                # Update all result tables
                private$.updatePersonTimeTable(patient_data, stats)
                private$.updateMilestoneTable(patient_data, milestone_data)
                private$.updateEventMarkerTable(patient_data, event_data)
                private$.updateAdvancedMetrics(patient_data, stats)
                private$.updateGroupComparisonTests(patient_data, stats)

                # Add clinical profile notices (small sample warnings, completion info)
                private$.addClinicalProfileNotices(patient_data, stats)

                # Handle export functionality
                private$.updateExportData(patient_data, milestone_data, event_data, stats)
                
                # Prepare plot data with all enhancements
                # Keep the state to what the renderer actually draws from.
                # It carried the whole `stats` list - including
                # `patient_summary`, a row per patient - and `interpretation`,
                # which no renderer has ever read: 0.30 MB at 2000 patients and
                # 1.51 MB at 10000, saved into every .omv and re-read on every
                # resize. The renderer uses exactly these four scalars.
                # Only the columns the renderer draws from. .createGgswimPlot and
                # .addReferenceLines read patient_id, start_time, end_time and
                # response_label; the other six (original_start, original_end,
                # anchor_start, response, censor_status, patient_group) were 42%
                # of patient_data - 785 KB at 10 000 patients - written into
                # every .omv and re-read on every resize.
                plot_cols <- intersect(
                    c("patient_id", "start_time", "end_time", "response_label", "response"),
                    names(patient_data))
                plot_state <- list(
                    patient_data = patient_data[, plot_cols, drop = FALSE],
                    milestone_data = milestone_data,
                    event_data = event_data,
                    arrow_data = arrow_data,
                    stats = list(
                        n_patients = stats$n_patients,
                        median_duration = stats$median_duration,
                        max_duration = stats$max_duration,
                        total_person_time = stats$total_person_time
                    ),
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

                # The renderer silently swaps High Contrast for viridis above 8
                # categories; say so here, where a notice can still be rendered.
                if (identical(self$options$colorPalette, "contrast") &&
                    "response_label" %in% names(patient_data)) {
                    n_lvl <- length(unique(stats::na.omit(
                        as.character(patient_data$response_label))))
                    if (n_lvl > 8)
                        private$.addNotice("INFO", .("Palette changed"), sprintf(
                            .("The High Contrast palette provides 8 colours and the response variable has %d categories, so the plot uses the Viridis palette instead. Group the response categories to keep High Contrast."),
                            n_lvl))
                }
                
                # Generate clinical interpretation if requested
                if (self$options$showInterpretation) {
                    private$.generateInterpretationOutput(interpretation)
                }

                # (Glossary and About are populated at the top of .run(), before
                # the early-return branches, because their content is static.)
                if (self$options$showCopyReady) {
                    private$.generateCopyReadyReport(stats, patient_data)
                }

            }, error = function(e) {
                # Blank whatever was already written before the failure. One
                # tryCatch wraps ~250 lines from validation to the copy-ready
                # report, so a failure partway through left the Summary table
                # fully populated with real numbers sitting beside a red error
                # box, with nothing saying those numbers come from a run that did
                # not finish. Same reset the top of .run() performs.
                for (tbl in c("personTimeTable", "milestoneTable", "eventMarkerTable",
                              "groupComparisonTest", "timelineData", "summaryData"))
                    try(self$results[[tbl]]$deleteRows(), silent = TRUE)
                try(self$results$interpretation$setContent(''), silent = TRUE)
                try(self$results$copyReadyReport$setContent(''), silent = TRUE)
                try(private$.resetSummaryTable(), silent = TRUE)

                error_msg <- paste(
                    "<div style='color: red; padding: 10px; border: 1px solid red; border-radius: 5px;'>",
                    "<h4>", .("Error in Swimmer Plot Analysis"), "</h4>",
                    .fmt(
                        .("<p><strong>Error:</strong> {message}</p>"),
                        message = htmltools::htmlEscape(as.character(e$message))
                    ),
                    "<p><strong>", .("Suggestions:"), "</strong></p>",
                    "<ul>",
                    "<li>", .("Ensure all required variables are selected"), "</li>",
                    "<li>", .("Check that time variables contain valid numeric or date values"), "</li>",
                    "<li>", .("Verify that end times are greater than or equal to start times"), "</li>",
                    "<li>", .("For date/time data, ensure correct format is selected"), "</li>",
                    "</ul>",
                    "</div>"
                )
                self$results$instructions$setContent(error_msg)
                # Surface the failure via the ERROR-notice channel. NOT re-raised:
                # stop(e) put jamovi into its raw error state, which duplicated
                # and undermined the tailored guidance just written above.
                private$.addNotice('ERROR', .("Error in Swimmer Plot Analysis"),
                                   as.character(e$message))
            })
        },
        
        .generateInstructions = function() {
            # Static welcome/instructions panel; body lives in
            # R/swimmerplot-html.R (fully .()-wrapped).
            swimmerplot_instructions_html(self)
        },
        
        .updateSummaryTable = function(stats) {
            summary_table <- self$results$summary

            # The five metric rows and their labels are created in .init(); only
            # the values are computed here. deleteRows() would take those rows
            # with it, and a subsequent setRow() on a missing key aborts the
            # analysis, so the response rows below are cleared selectively.
            summary_table$setRow(rowKey = 1L, values = list(value = stats$n_patients))
            summary_table$setRow(rowKey = 2L, values = list(value = stats$n_observations))
            summary_table$setRow(rowKey = 3L, values = list(value = round(stats$median_duration, 2)))
            summary_table$setRow(rowKey = 4L, values = list(value = round(stats$mean_duration, 2)))
            summary_table$setRow(rowKey = 5L, values = list(value = round(stats$total_person_time, 2)))

            # This table has one Value column carrying counts, durations and
            # percentages, and it used to have no note at all unless a response
            # variable was selected - so a reader could not tell whether 249 was
            # days or months. It also prints Mean Duration and Total Person-Time
            # three rows apart on DIFFERENT definitions (9.33 x 3 = 28 against a
            # reported 22 on a gapped fixture), which nothing reconciled.
            summary_table$setNote("units", .fmt(
                .("Durations and person-time are in {unit}; rate rows are percentages. Mean Duration spans each patient's first start to last end, so it includes any gaps between episodes, while Total Person-Time is the union of the episodes themselves and excludes them - the two therefore differ whenever a patient has a treatment gap."),
                unit = private$.timeUnitWord()))

            # Response rows are appended fresh each run: .resetSummaryTable(), called
            # at the top of .run(), leaves only the five fixed rows, so a category
            # that disappeared from the data cannot survive into this run. (It used
            # to: the rows were appended once and thereafter setRow()'d with nothing
            # ever removing one, so deleting the last SD patient left a stale "SD
            # Rate (2/8)" row and the rates summed to 125%, contradicting the
            # footnote below that promises the counts add to the denominator.)

            # Add response statistics if available. These rows are appended, so
            # re-set a key a previous run already created rather than adding it
            # twice; a changed responseVar clears the table via clearWith.
            if (!is.null(stats$response_counts)) {
                for (response in names(stats$response_counts)) {
                    row_key <- paste0("response_", response)
                    row_values <- list(
                        metric = .fmt(.("{response} Rate ({n}/{N})"),
                                      response = response,
                                      n = stats$response_counts[[response]],
                                      N = stats$response_denominator),
                        value = round(stats$response_percentages[[response]], 1)
                    )
                    if (any(vapply(summary_table$rowKeys, identical, logical(1), row_key)))
                        summary_table$setRow(rowKey = row_key, values = row_values)
                    else
                        summary_table$addRow(rowKey = row_key, values = row_values)
                }

                # Every rate above divides by all patients, so the patients with
                # no recorded response are the remainder. Showing them is what
                # lets a reader add the rows to 100% and see the denominator.
                if (isTRUE(stats$n_response_missing > 0)) {
                    miss_values <- list(
                        metric = .fmt(.("No recorded response ({n}/{N})"),
                                      n = stats$n_response_missing,
                                      N = stats$response_denominator),
                        value = round(stats$n_response_missing /
                                      stats$response_denominator * 100, 1))
                    # NOT "response_<x>": a response level literally spelled
                    # "missing" produces exactly that key, and this row then
                    # overwrote the category's own row.
                    if (any(vapply(summary_table$rowKeys, identical, logical(1), "no_recorded_response")))
                        summary_table$setRow(rowKey = "no_recorded_response", values = miss_values)
                    else
                        summary_table$addRow(rowKey = "no_recorded_response", values = miss_values)
                }

                # Name the denominator once, beside the rows that use it.
                summary_table$setNote("response_denominator", sprintf(
                    .("Each row shows the count over all %d patients in the analysis, the same denominator as the Objective and Disease Control Rates. RECIST 1.1 section 4.9.1 requires rates over every patient rather than over a selected evaluable subset, so patients recorded as NE, with an unrecognised label, or with no response at all remain in the denominator and count as non-responders. The percentages are each rounded independently, so they need not add to exactly 100; the counts do."),
                    stats$response_denominator))
            }
        },
        
        # Person-time analysis table population
        .updatePersonTimeTable = function(patient_data, stats) {
            # Clear FIRST: the early returns below used to precede deleteRows(),
            # so a run whose data no longer supports the table kept stale rows.
            self$results$personTimeTable$deleteRows()

            # This table is person-time BY BEST RESPONSE, so it needs both
            # options. With "Response analysis" off it went on printing CR/PR/
            # SD/PD rows for the analysis the user had switched off.
            if (!self$options$personTimeAnalysis) return()
            if (!isTRUE(self$options$responseAnalysis)) return()

            patient_summary <- stats$patient_summary
            if (is.null(patient_summary)) {
                patient_summary <- private$.summarizeByPatient(patient_data)
            }

            if (!"response" %in% names(patient_summary)) return()

            n_all_pt <- nrow(patient_summary)
            pt_all <- sum(patient_summary$person_time, na.rm = TRUE)
            patient_summary <- patient_summary[!is.na(patient_summary$response), , drop = FALSE]
            n_excl_pt <- n_all_pt - nrow(patient_summary)
            if (nrow(patient_summary) == 0) {
                self$results$personTimeTable$setNote("empty",
                    .("No person-time by response to show: no patient in the analysis has a recorded response."))
                return()
            }

            person_time_data <- patient_summary %>%
                dplyr::group_by(response) %>%
                dplyr::summarise(
                    n_patients = dplyr::n(),
                    total_time = sum(person_time, na.rm = TRUE),
                    mean_time = mean(person_time, na.rm = TRUE),
                    .groups = "drop"
                ) %>%
                dplyr::mutate(
                    # Follow-up density: patients per unit of person-time (not an incidence rate)
                    followup_density = ifelse(total_time > 0, n_patients / total_time * 100, NA_real_)
                )

            for (i in seq_len(nrow(person_time_data))) {
                self$results$personTimeTable$addRow(rowKey = i, values = list(
                    response_type = jmvcore::htmlEscape(as.character(person_time_data$response[i])),
                    n_patients = person_time_data$n_patients[i],
                    total_time = round(person_time_data$total_time[i], 2),
                    mean_time = round(person_time_data$mean_time[i], 2),
                    incidence_rate = round(person_time_data$followup_density[i], 3)
                ))
            }
            # The rows sum to less than Total Person-Time whenever a patient has no
            # recorded response, and nothing said so: a 6-patient fixture summed
            # to 50 against a reported 60. The summary table was given a
            # "No recorded response" row for exactly this reason; this one gets
            # the figure in a note instead, because it is keyed by response.
            if (n_excl_pt > 0)
                self$results$personTimeTable$setNote("excluded", .fmt(
                    .("{n} of {N} patients have no recorded response and are not shown here, so these rows sum to {shown} of the {total} {unit} of total person-time reported in the summary."),
                    n = n_excl_pt, N = n_all_pt,
                    shown = round(sum(patient_summary$person_time, na.rm = TRUE), 2),
                    total = round(pt_all, 2), unit = private$.timeUnitWord()))

            self$results$personTimeTable$setNote("density", .fmt(
                .("Follow-up density = patients per 100 {unit} of person-time (a descriptive measure, not an event rate). It is exactly 100 divided by Mean Time in the same row, so it carries no information beyond that column. Times are in {unit}."),
                unit = private$.timeUnitWord()))
            # Guarantee-time bias. Splitting follow-up by BEST response is the
            # textbook case: a patient must survive long enough to be assessed
            # as a responder, so responders have longer follow-up by
            # construction, whatever the treatment does.
            self$results$personTimeTable$setNote("guarantee_time",
                .("Descriptive only. Time is split by BEST overall response, which a patient can only achieve by living long enough to be assessed, so responders accumulate more follow-up by construction (guarantee-time bias). These rows must not be read as a survival benefit of responding: use a landmark analysis or treat response as a time-dependent covariate."))
        },
        
        # Milestone table population
        .updateMilestoneTable = function(patient_data, milestone_data) {
            # Clear FIRST so an empty-input run does not keep stale rows
            self$results$milestoneTable$deleteRows()

            # The table is visible as soon as a milestone variable is chosen, so
            # an empty one has to say why instead of sitting there blank.
            if (nrow(milestone_data) == 0) {
                self$results$milestoneTable$setNote("empty",
                    .("No milestones to summarise. The milestone variable has no usable value for any patient in the analysis, or every milestone slot was skipped - see the notices above."))
                return()
            }

            milestone_numeric <- private$.convertTimesToNumeric(
                milestone_data$time,
                milestone_data$patient_id,
                patient_data
            )

            milestone_stats_data <- milestone_data %>%
                dplyr::mutate(time_numeric = milestone_numeric) %>%
                dplyr::filter(!is.na(time_numeric))

            if (nrow(milestone_stats_data) == 0) {
                self$results$milestoneTable$setNote("empty",
                    .("No milestones to summarise: none of the milestone values could be converted to a time on this timeline."))
                return()
            }

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
                        private$.timeUnitWord()
                    )
                )
            
            # Populate the table
            for (i in seq_len(nrow(milestone_stats))) {
                self$results$milestoneTable$addRow(rowKey = i, values = list(
                    milestone_name = jmvcore::htmlEscape(as.character(milestone_stats$label[i])),
                    n_events = milestone_stats$n_events[i],
                    median_time = round(milestone_stats$median_time[i], 2),
                    time_range = milestone_stats$time_range[i]
                ))
            }
        },
        
        # Event marker table population
        .updateEventMarkerTable = function(patient_data, event_data) {
            # Clear FIRST so an empty-input run does not keep stale rows
            self$results$eventMarkerTable$deleteRows()

            if (!self$options$showEventMarkers) return()
            if (is.null(event_data) || nrow(event_data) == 0) {
                self$results$eventMarkerTable$setNote("empty",
                    .("No event markers to summarise. Either no event type/time variable is selected, or none of the events falls inside a patient's timeline - see the notices above."))
                return()
            }

            event_numeric <- private$.convertTimesToNumeric(
                event_data$time,
                event_data$patient_id,
                patient_data
            )

            event_stats_data <- event_data %>%
                dplyr::mutate(time_numeric = event_numeric) %>%
                dplyr::filter(!is.na(time_numeric))

            if (nrow(event_stats_data) == 0) {
                self$results$eventMarkerTable$setNote("empty",
                    .("No event markers to summarise: none of the event times could be converted to a time on this timeline."))
                return()
            }

            total_events <- nrow(event_stats_data)
            n_pat_ev <- length(unique(as.character(event_stats_data$patient_id)))

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
                    event_type = jmvcore::htmlEscape(as.character(event_stats$label[i])),
                    n_events = event_stats$n_events[i],
                    percent = event_stats$percent[i],
                    median_time = round(event_stats$median_time[i], 2)
                ))
            }

            # The percentage divides by event RECORDS, not patients, so one
            # patient contributing three markers dominates it: a 3-patient
            # fixture where 2 of 3 ever had an AE printed "80%". Name the
            # denominator, and the unit for the median column, which the
            # milestone table already prints in its own range column.
            self$results$eventMarkerTable$setNote("denominator", .fmt(
                .("Percentages are over the {n} event markers shown, not over the {p} patients who have one or the {N} patients in the analysis - a patient with several markers counts several times. Median Time is measured from each patient's own start, in {unit}."),
                n = total_events, p = n_pat_ev,
                N = if (!is.null(patient_data)) length(unique(as.character(patient_data$patient_id))) else n_pat_ev,
                unit = private$.timeUnitWord()))
        },
        
        # Advanced metrics table population
        .updateAdvancedMetrics = function(patient_data, stats) {
            # The rows exist only when the matching option is on (.init()) and
            # were blanked at the start of .run(), so there is nothing to clear
            # here. Each family is gated by the option that names it.
            want_pt <- isTRUE(self$options$personTimeAnalysis)
            want_resp <- isTRUE(self$options$responseAnalysis) &&
                         !is.null(self$options$responseVar)
            if (!want_pt && !want_resp) return()

            patient_summary <- stats$patient_summary
            if (is.null(patient_summary)) {
                patient_summary <- private$.summarizeByPatient(patient_data)
            }

            n_patients_summary <- nrow(patient_summary)

            # Calculate advanced clinical metrics
            metrics <- if (!want_pt) list() else list(
                list(
                    key = "median_followup",
                    value = round(stats$median_followup_km, 2),
                    ci = private$.followUpCIText(stats$median_followup_ci_lower,
                                                 stats$median_followup_ci_upper),
                    unit = private$.timeUnitWord(),
                    interpretation = if (identical(stats$median_followup_method, "reverse_km"))
                        .("Reverse Kaplan-Meier estimate (Schemper & Smith 1996): event and censoring roles are swapped, so this estimates how long patients would have been observed. The plain median of observed durations is the median time to event-or-censoring and understates follow-up when events are common. The interval is survfit's default log transformation, not the log-log (Brookmeyer-Crowley) interval some trial reports use; the two differ materially with heavy censoring. Response-rate rows in this column instead carry exact binomial (Clopper-Pearson) intervals.")
                    else
                        # The estimator already records WHY it fell back - no
                        # patient censored, or the reversed curve never reaching
                        # 50%. That string previously had no consumer anywhere
                        # in the module, so the user saw the fallback without
                        # ever being told which of the two had happened.
                        paste(
                            .("Plain median of the observed durations, because the reverse Kaplan-Meier estimate was not available. Read it as the median time to event-or-censoring, not as the length of follow-up."),
                            private$.followUpReasonText(
                                stats$median_followup_reason_code %||% "",
                                stats$median_followup_reason %||% ""))
                ),
                list(
                    key = "iqr",
                    value = round(stats$q3_duration - stats$q1_duration, 2),
                    ci = NA_character_,
                    unit = private$.timeUnitWord(),
                    interpretation = .("Middle 50% of follow-up duration range")
                ),
                list(
                    key = "person_time",
                    value = round(stats$total_person_time, 2),
                    ci = NA_character_,
                    unit = sprintf(.("%s (cumulative)"), private$.timeUnitWord()),
                    interpretation = .("Total observation time across all patients")
                ),
                list(
                    key = "followup_density",
                    value = if (isTRUE(stats$total_person_time > 0)) round(n_patients_summary / stats$total_person_time * 100, 3) else NA_real_,
                    ci = NA_character_,
                    unit = sprintf(.("per 100 %s"), private$.timeUnitWord()),
                    interpretation = .("Number of patients per 100 units of observation time, i.e. 100 divided by the mean person-time per patient. It is not an event rate. Note it divides by person-time (the union of treatment episodes), not by Mean Duration, which spans first start to last end and so includes any gaps.")
                )
            )
            
            # Add response-specific metrics if available
            if (want_resp && !is.null(stats$response_counts)) {
                response_counts <- stats$response_counts
                total_responses <- sum(response_counts)

                if (total_responses > 0) {
                    rates <- private$.responseRates(response_counts, n_patients_summary)

                    if (!rates$evaluable) {
                        private$.addNotice(
                            "WARNING",
                            .("Response rates not calculated"),
                            .("None of the values in the response variable could be recognised as RECIST categories (CR, PR, SD, PD). Objective Response Rate and Disease Control Rate are defined only for RECIST-coded responses and have been omitted rather than reported as 0%. Recode the response variable to CR/PR/SD/PD to obtain ORR and DCR.")
                        )
                    }

                    orr_count <- rates$orr_count
                    dcr_count <- rates$dcr_count
                    orr <- rates$orr
                    dcr <- rates$dcr
                    # SAME denominator as the point estimates (RECIST-evaluable):
                    # computing the CI over all responses while the rate used the
                    # evaluable count printed an estimate outside its own interval.
                    n_eval <- rates$n_all

                    # Calculate exact binomial 95% confidence intervals
                    orr_ci <- NA_character_
                    dcr_ci <- NA_character_

                    if (!is.na(orr) && n_eval > 0) {
                        orr_test <- tryCatch({
                            binom.test(orr_count, n_eval, conf.level = 0.95)
                        }, error = function(e) NULL)

                        if (!is.null(orr_test)) {
                            orr_ci <- sprintf("%.1f - %.1f",
                                            orr_test$conf.int[1] * 100,
                                            orr_test$conf.int[2] * 100)
                        }
                    }

                    if (!is.na(dcr) && n_eval > 0) {
                        dcr_test <- tryCatch({
                            binom.test(dcr_count, n_eval, conf.level = 0.95)
                        }, error = function(e) NULL)

                        if (!is.null(dcr_test)) {
                            dcr_ci <- sprintf("%.1f - %.1f",
                                            dcr_test$conf.int[1] * 100,
                                            dcr_test$conf.int[2] * 100)
                        }
                    }

                    metrics <- append(metrics, list(
                        list(
                            key = "orr",
                            value = if (!is.na(orr)) round(orr, 1) else NA_real_,
                            ci = orr_ci,
                            unit = .("percent"),
                            interpretation = .("Proportion with complete or partial response")
                        ),
                        list(
                            key = "dcr",
                            value = if (!is.na(dcr)) round(dcr, 1) else NA_real_,
                            ci = dcr_ci,
                            unit = .("percent"),
                            interpretation = .("Proportion with response or stable disease")
                        )
                    ))
                }
            }

            # Fill the rows .init() scaffolded. Labels come from
            # .advancedMetricLabels() alone; only the follow-up label is replaced,
            # by the estimator this run actually used. The key check keeps a
            # metric whose row was not scaffolded from aborting the analysis in setRow().
            labels <- private$.advancedMetricLabels()
            labels[["median_followup"]] <- private$.medianFollowUpLabel(stats$median_followup_method)
            for (metric in metrics) {
                if (!metric$key %in% names(labels)) next
                self$results$advancedMetrics$setRow(rowKey = metric$key, values = list(
                    metric_name = labels[[metric$key]],
                    metric_value = metric$value,
                    confidence_interval = metric$ci,
                    metric_unit = metric$unit,
                    clinical_interpretation = metric$interpretation
                ))
            }
        },

        # Group comparison statistical tests (Fisher's exact for ORR/DCR)
        # Build the 2x2 with a FIXED orientation: rows are the groups in the
        # user's own factor order, columns are (non-responder, responder).
        # table() on a character vector sorted the groups alphabetically, so
        # renaming a group could invert the odds ratio, and nothing on screen
        # said which way it pointed.
        .groupContingency = function(group, flag, levels_order) {
            g <- factor(as.character(group), levels = levels_order)
            f <- factor(flag, levels = c(FALSE, TRUE))
            table(g, f)
        },

        # R's fisher.test() on such a table estimates the odds of the TRUE
        # column in ROW 2 relative to ROW 1 (verified numerically), so the label
        # names row 2 first. It also carries the interval fisher.test already
        # computed and this table used to discard.
        # A 2x2 whose outcome column is entirely empty - every patient a
        # responder, or none - carries no information: fisher.test returns
        # p = 1 and an odds ratio of 0 or Inf. Forcing both columns to exist
        # (so the orientation is fixed) means that case must be screened here
        # rather than falling out of table() having one column.
        .groupTableIsTestable = function(tab) {
            !is.null(tab) && nrow(tab) >= 2 && ncol(tab) == 2 &&
                all(rowSums(tab) > 0) && all(colSums(tab) > 0)
        },

        .groupTestStatistic = function(tab, test) {
            if (is.null(test$estimate) || nrow(tab) != 2)
                return(.("Fisher's exact test"))
            lv <- rownames(tab)
            ci <- test$conf.int
            est <- unname(test$estimate)
            # A zero cell sends the odds ratio (and one interval bound) to Inf,
            # and the whole interval used to be discarded - throwing away the
            # finite bound, which is the half carrying the evidence. A DCR row
            # read "OR = Inf" with no interval while fisher.test had returned
            # (1.20, Inf). .followUpCIText solved this for the reverse-KM median;
            # format each bound independently here too.
            fmt <- function(x) if (is.finite(x)) sprintf("%.2f", x) else .("NR")
            est_txt <- if (is.finite(est)) sprintf("%.2f", est) else .("not estimable (a zero cell)")
            # Level names come from the user's data and this string is a table
            # CELL, which jamovi renders with renderMode = "rich".
            g2 <- jmvcore::htmlEscape(lv[2]); g1 <- jmvcore::htmlEscape(lv[1])
            if (is.null(ci) || !any(is.finite(ci)))
                return(sprintf(.("Fisher's exact test, OR (%s vs %s) = %s"),
                               g2, g1, est_txt))
            sprintf(.("Fisher's exact test, OR (%s vs %s) = %s, 95%% CI %s to %s"),
                    g2, g1, est_txt, fmt(ci[1]), fmt(ci[2]))
        },

        .updateGroupComparisonTests = function(patient_data, stats) {
            # Clear FIRST so a run without a group variable does not keep stale rows
            self$results$groupComparisonTest$deleteRows()

            if (is.null(self$options$groupVar)) return()

            # "Response analysis" off used to leave these ORR/DCR Fisher tests
            # running and reporting p-values for the analysis the user had just
            # switched off.
            if (!isTRUE(self$options$responseAnalysis)) return()

            # Every abandonment below says why. The table is visible whenever a
            # group variable is chosen, so a bare return() left an empty table
            # on the page with nothing to explain it.
            explain <- function(msg) {
                self$results$groupComparisonTest$setNote("not_run", msg)
                invisible(NULL)
            }

            # Need patient_summary with both response and group
            patient_summary <- stats$patient_summary
            if (is.null(patient_summary)) return()

            # Check if we have both response and group data
            # Ask the OPTION, not the column: .summarizeByPatient always emits a
            # `response` column, all NA when no variable was chosen, so the
            # column test passed and the table went on to publish a denominator
            # note reading "A: 0 of 4 responded" for data that records no
            # responses at all.
            if (is.null(self$options$responseVar) ||
                !"response" %in% names(patient_summary)) {
                explain(.("No comparison was run: the group tests compare objective response and disease control between groups, and no Response/Status variable is selected."))
                return()
            }
            if (!"patient_group" %in% names(patient_summary)) return()

            # Keep every patient who has a GROUP. A patient with no recorded
            # response is a non-responder, not an exclusion - `NA %in% c("CR",
            # "PR")` is FALSE, so they land in the non-responder column by
            # construction. Dropping them made this test divide by a different
            # cohort from the ORR printed above it: on the audit's 12-patient
            # example the odds ratio came from n = 11 while the ORR came from
            # n = 9, and neither matched the 12 in "Study included 12 patients".
            df <- patient_summary[!is.na(patient_summary$patient_group), ]
            if (nrow(df) == 0) {
                explain(.("No comparison was run: no patient has a value in the group variable."))
                return()
            }

            # A4: patients with no group value are dropped here. The missing-
            # censoring path discloses its exclusions; this one did not, so the
            # note could say "over all 24 patients" while the ORR above was over
            # 30, with nothing connecting the two.
            n_no_group <- sum(is.na(patient_summary$patient_group))
            if (n_no_group > 0)
                private$.addNotice("WARNING", .("Patients excluded from the group comparison"), sprintf(
                    .("%d of %d patients have no value in the grouping variable and are excluded from this comparison only. The response rates reported above still cover all %d patients, so the two denominators differ."),
                    n_no_group, nrow(patient_summary), nrow(patient_summary)))

            # A3: the same RECIST guard .updateAdvancedMetrics uses. Without it
            # this table published "Arm A: 0 of 19 responded (0.0%)" for a
            # response variable the notices panel had just refused to score,
            # directly contradicting the warning printed above it.
            if (!is.null(stats$response_counts)) {
                rates_chk <- private$.responseRates(stats$response_counts, nrow(df))
                if (!is.null(rates_chk) && !isTRUE(rates_chk$evaluable)) {
                    explain(.("No comparison was run: objective response and disease control are defined only for RECIST-coded responses (CR, PR, SD, PD), and none of the values in the response variable could be recognised as one. Recode the response variable to compare groups."))
                    return()
                }
            }

            # Normalize responses (should already be normalized, but ensure consistency)
            df$response <- private$.normalizeResponses(df$response)

            # The user's own factor order, not alphabetical. patient_data keeps
            # the factor made when the variable was read; the per-patient
            # summary flattened it to character.
            levels_order <- if (!is.null(patient_data) &&
                                "patient_group" %in% names(patient_data) &&
                                is.factor(patient_data$patient_group))
                levels(patient_data$patient_group)
            else sort(unique(as.character(df$patient_group)))
            levels_order <- levels_order[levels_order %in% as.character(df$patient_group)]

            groups <- levels_order
            if (length(groups) < 2) {
                explain(sprintf(
                    .("No comparison was run: Fisher's exact test needs at least two groups and the group variable has %d in these data."),
                    length(groups)))
                return()
            }

            # A4: a grouping variable with one level per patient is almost always
            # a continuous column chosen by mistake - the option permits numeric,
            # and a continuous time column produced a silent 24-group Fisher test
            # in which every group read "1 of 1 responded". Say so rather than
            # publishing a p-value for it.
            grp_n <- vapply(groups, function(g) sum(as.character(df$patient_group) == g), 0L)
            if (length(groups) > 10 || any(grp_n < 2))
                private$.addNotice("STRONG_WARNING", .("Grouping variable may not be categorical"), sprintf(
                    .("The grouping variable splits %d patients into %d groups, %d of which contain a single patient. A comparison across that many tiny groups has almost no power and is usually a sign that a continuous variable was selected by mistake. Choose a variable with a small number of clinically meaningful categories."),
                    nrow(df), length(groups), sum(grp_n < 2)))

            # Per-group counts, so the reader can see the cohort the test used
            # and check it against the rates above. Without these the odds ratio
            # was a bare number with no way to tell which group did better.
            per_group <- vapply(levels_order, function(g) {
                inb <- as.character(df$patient_group) == g
                resp <- sum(df$response[inb] %in% c("CR", "PR"))
                sprintf(.("%s: %d of %d responded (%.1f%%)"),
                        jmvcore::htmlEscape(g), resp, sum(inb),
                        if (sum(inb) > 0) resp / sum(inb) * 100 else NA_real_)
            }, character(1))
            self$results$groupComparisonTest$setNote("denominator", paste(
                sprintf(
                    .("Rates are over all %d patients in the comparison who have a group value, split by group below: not-evaluable, unrecognised and unrecorded responses count as non-responders, the same rule the Objective Response Rate uses. This is what lets the test and the rate above it be read together."),
                    nrow(df)),
                sprintf(.("Objective response by group - %s."), paste(per_group, collapse = "; "))))

            # Perform Fisher's exact test for ORR (CR + PR vs others)
            orr_contingency <- tryCatch({
                df$responder <- df$response %in% c("CR", "PR")
                private$.groupContingency(df$patient_group, df$responder, levels_order)
            }, error = function(e) NULL)

            orr_ran <- FALSE
            if (private$.groupTableIsTestable(orr_contingency)) {
                orr_test <- tryCatch({
                    fisher.test(orr_contingency)
                }, error = function(e) NULL)

                if (!is.null(orr_test)) {
                    orr_interpretation <- if (orr_test$p.value < 0.05) {
                        .("Statistically significant difference in response rates between groups (p < 0.05)")
                    } else {
                        .("No statistically significant difference in response rates was detected; this does not establish that the rates are equal, as the test may lack power")
                    }

                    self$results$groupComparisonTest$addRow(rowKey = 1, values = list(
                        comparison = .("Objective Response Rate (ORR)"),
                        # fisher.test() returns an odds ratio only for a 2x2 table;
                        # with 3+ groups this printed "OR = NA".
                        test_statistic = private$.groupTestStatistic(orr_contingency, orr_test),
                        p_value = orr_test$p.value,
                        interpretation = orr_interpretation
                    ))
                    orr_ran <- TRUE
                }
            }

            # Perform Fisher's exact test for DCR (CR + PR + SD vs others)
            dcr_contingency <- tryCatch({
                df$disease_control <- df$response %in% c("CR", "PR", "SD")
                private$.groupContingency(df$patient_group, df$disease_control, levels_order)
            }, error = function(e) NULL)

            dcr_ran <- FALSE
            if (private$.groupTableIsTestable(dcr_contingency)) {
                dcr_test <- tryCatch({
                    fisher.test(dcr_contingency)
                }, error = function(e) NULL)

                if (!is.null(dcr_test)) {
                    dcr_interpretation <- if (dcr_test$p.value < 0.05) {
                        .("Statistically significant difference in disease control rates between groups (p < 0.05)")
                    } else {
                        .("No statistically significant difference in disease control rates was detected; this does not establish that the rates are equal, as the test may lack power")
                    }

                    self$results$groupComparisonTest$addRow(rowKey = 2, values = list(
                        comparison = .("Disease Control Rate (DCR)"),
                        test_statistic = private$.groupTestStatistic(dcr_contingency, dcr_test),
                        p_value = dcr_test$p.value,
                        interpretation = dcr_interpretation
                    ))
                    dcr_ran <- TRUE
                }
            }

            if (self$results$groupComparisonTest$rowCount > 1) {
                self$results$groupComparisonTest$setNote("multiplicity",
                    .("Two Fisher's exact tests (ORR and DCR) are reported with unadjusted p-values; interpret them jointly rather than as independent evidence."))
            }

            # S34: a table that is visible and empty, or missing one of its two
            # rows, must say why rather than leaving the reader to guess.
            if (!orr_ran && !dcr_ran) {
                self$results$groupComparisonTest$setNote("not_testable",
                    .("No group comparison could be run. Fisher's exact test needs at least two groups and both outcomes present: here every patient in the comparison had the same outcome (all responders or none, and likewise for disease control), so there is nothing to compare. The per-group counts are in the note above."))
            } else if (!orr_ran) {
                self$results$groupComparisonTest$setNote("orr_not_testable",
                    .("The objective-response row is absent because every patient in the comparison had the same response status - all responders or none - so Fisher's exact test has nothing to compare on that outcome."))
            } else if (!dcr_ran) {
                self$results$groupComparisonTest$setNote("dcr_not_testable",
                    .("The disease-control row is absent because every patient in the comparison had the same disease-control status, so Fisher's exact test has nothing to compare on that outcome."))
            }

            # S24: only warn about the cells of a test that was actually
            # reported. This used to read min() off any contingency table that
            # had been built, so a run with no test at all still showed
            # "Fisher exact test has cells with counts below 5".
            min_cell_orr <- if (orr_ran) min(orr_contingency) else NA
            min_cell_dcr <- if (dcr_ran) min(dcr_contingency) else NA
            min_cells <- c(min_cell_orr, min_cell_dcr)
            # min(c(NA, NA), na.rm = TRUE) is Inf plus an R warning.
            min_cell <- if (all(is.na(min_cells))) NA else min(min_cells, na.rm = TRUE)

            if (!is.na(min_cell) && min_cell < 5) {
                private$.addNotice("WARNING",
                    .("Small cell counts in Fisher's exact test"),
                    sprintf(
                        .("Fisher exact test has cells with counts below 5 (minimum cell count = %d). The test remains valid, but interpret p-values cautiously with small cell counts. Consider grouping categories or collecting more data."),
                        min_cell))
            }
        },

        # Export functionality
        .updateExportData = function(patient_data, milestone_data, event_data, stats) {
            # Export timeline data if requested
            if (self$options$exportTimeline) {
                tbl <- self$results$timelineData
                # deleteRows first: this table (and summaryData below) were the
                # only repopulated tables never cleared, so any rerun outside
                # their clearWith (laneWidth, showLegend, ...) duplicated every
                # exported row.
                tbl$deleteRows()

                per_patient <- stats$patient_summary
                if (!is.null(per_patient) && nrow(per_patient) > 0) {
                    # Put the rows in the order the FIGURE reads, top first.
                    # .applySorting sets levels to rev(unique(ordered_ids))
                    # because ggplot draws level 1 at the bottom, so taking
                    # per_patient[1:500] took the plot's BOTTOM 500 - with the
                    # default duration_desc that is the 500 SHORTEST timelines,
                    # and the 100 longest-followed patients were the ones
                    # dropped, under a note claiming the order matched the plot.
                    if (is.factor(patient_data$patient_id)) {
                        top_down <- rev(levels(patient_data$patient_id))
                        ord <- order(match(as.character(per_patient$patient_id), top_down))
                        per_patient <- per_patient[ord, , drop = FALSE]
                    }
                    has_response <- "response" %in% names(per_patient)

                    # Absolute-date timelines used to export Start/End as raw
                    # epoch numbers (days/seconds since 1970) beside a Duration
                    # in the selected unit - three columns in two undocumented
                    # scales. Export offsets from the earliest start instead.
                    is_date <- inherits(per_patient$start_time, c("Date", "POSIXct", "POSIXlt"))
                    if (is_date) {
                        origin <- suppressWarnings(min(per_patient$start_time, na.rm = TRUE))
                        st_exp <- suppressWarnings(lubridate::time_length(
                            lubridate::interval(origin, per_patient$start_time),
                            unit = self$options$timeUnit))
                        en_exp <- suppressWarnings(lubridate::time_length(
                            lubridate::interval(origin, per_patient$end_time),
                            unit = self$options$timeUnit))
                        tbl$setNote("scale", .fmt(
                            .("Start/End are offsets from the earliest start date ({origin}), in {unit}; Duration is in {unit}."),
                            origin = format(origin, "%Y-%m-%d"),
                            unit = private$.timeUnitWord()))
                    } else {
                        st_exp <- private$.asNumericTime(per_patient$start_time)
                        en_exp <- private$.asNumericTime(per_patient$end_time)
                    }

                    # jmvcore's addRow() is quadratic in the number of rows
                    # already in the table - measured on a bare Table, 250 rows
                    # take 2.1 s, 500 take 8.3 s and 1000 take 32.9 s, with the
                    # values themselves costing nothing. A 2000-patient cohort
                    # spent 135 s of every run inside this loop. There is no
                    # cheaper Table API (setRow on pre-added rows is identical),
                    # so the on-screen export is capped and the cap is stated.
                    export_cap <- 500L
                    n_export <- min(nrow(per_patient), export_cap)
                    for (i in seq_len(n_export)) {
                        tbl$addRow(rowKey = i, values = list(
                            patient_id = as.character(per_patient$patient_id[i]),
                            start_time = st_exp[i],
                            end_time   = en_exp[i],
                            duration   = per_patient$follow_up[i],
                            response   = if (has_response)
                                as.character(per_patient$response[i]) else ""
                        ))
                    }
                    if (nrow(per_patient) > export_cap)
                        tbl$setNote("capped", sprintf(
                            .("Showing the first %d of %d patients, in the order the plot uses. Building this table is slow for large cohorts, so it is capped. The plot, the statistics and every other table use all %d patients."),
                            n_export, nrow(per_patient), nrow(per_patient)))
                }
            }

            # Export summary statistics if requested  
            if (self$options$exportSummary) {
                summary_export <- data.frame(
                    # mean_follow_up was byte-identical to mean_duration (both
                    # mean(valid_follow_up)); the summary table already dropped it.
                    metric = c("n_patients", "n_observations", "median_duration",
                             "mean_duration", "total_person_time"),
                    value = c(stats$n_patients, stats$n_observations, stats$median_duration,
                            stats$mean_duration, stats$total_person_time),
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

                tbl <- self$results$summaryData
                tbl$deleteRows()
                for (i in seq_len(nrow(summary_export))) {
                    tbl$addRow(rowKey = i, values = list(
                        metric = as.character(summary_export$metric[i]),
                        value  = suppressWarnings(as.numeric(summary_export$value[i]))
                    ))
                }
            }
            
            # Update export information panel
            if (self$options$exportTimeline || self$options$exportSummary) {
                export_info <- paste0(
                    "<div style='background-color: rgba(33, 152, 255, 0.07); padding: 15px; border-radius: 5px; margin: 10px 0; color: inherit;'>",
                    "<h4>", .("Export Information"), "</h4>",
                    "<p>", .("Data has been exported to the following outputs:"), "</p>",
                    "<ul>",
                    if (self$options$exportTimeline) paste0("<li><strong>", .("Timeline Data:"), "</strong> ", .("Complete patient timeline dataset with processed variables"), "</li>") else "",
                    if (self$options$exportSummary) paste0("<li><strong>", .("Summary Statistics:"), "</strong> ", .("Comprehensive summary metrics and clinical indicators"), "</li>") else "",
                    "</ul>",
                    "<p><em>", .("Note: Exported data can be accessed through the Output panel and used for external analysis."), "</em></p>",
                    "</div>"
                )
                self$results$exportInfo$setContent(export_info)
            }
        },
        
        .generateInterpretationOutput = function(interpretation) {
            interp_html <- paste0(
                "<div style='background-color: rgba(33, 159, 33, 0.1); padding: 15px; border-radius: 5px; margin: 10px 0; color: inherit;'>",
                "<h4>", .("Clinical Interpretation"), "</h4>",
                "<div style='margin: 10px 0;'>",
                "<h5 style='color: inherit;'>", .("Timeline Analysis:"), "</h5>",
                "<p>", interpretation$timeline, "</p>",
                "</div>",
                # Unticking "Person-time analysis" removed the table but left
                # this paragraph, so the interpretation went on quoting a total
                # person-time that no longer appeared anywhere on the page.
                if (isTRUE(self$options$personTimeAnalysis) &&
                    !is.null(interpretation$person_time))
                    paste0(
                        "<div style='margin: 10px 0;'>",
                        "<h5 style='color: inherit;'>", .("Person-Time Analysis:"), "</h5>",
                        "<p>", interpretation$person_time, "</p>",
                        "</div>")
                else ""
            )

            if (self$options$responseAnalysis && !is.null(interpretation$response)) {
                interp_html <- paste0(interp_html,
                    "<div style='margin: 10px 0;'>",
                    "<h5 style='color: inherit;'>", .("Response Pattern Analysis:"), "</h5>",
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
                p <- private$.createGgswimPlot(patient_data, milestone_data, event_data, arrow_data, opts, stats, theme, ggtheme)
                
                print(p)
                return(TRUE)
                
            }, error = function(e) {
                # The fallback plot's subtitle carries e$message; jamovi does
                # not surface warning(), so nothing else is needed here.

                # Create fallback plot
                # Name the argument: the signature is
                # (patient_data, milestone_data, event_data, opts, stats, error_message)
                # so a positional second argument put the real error into
                # milestone_data, where it was discarded, and the fallback subtitle
                # always read "ggswim unavailable" whatever had actually failed.
                p_fallback <- private$.createFallbackPlot(patient_data, error_message = e$message)
                print(p_fallback)
                return(TRUE)
            })
        },
        
        .createGgswimPlot = function(patient_data, milestone_data, event_data, arrow_data, opts, stats, theme = NULL, ggtheme = NULL) {
            # Check if ggswim is available
            if (!requireNamespace("ggswim", quietly = TRUE)) {
                return(private$.createFallbackPlot(patient_data, milestone_data, event_data, opts, stats,
                    error_message = .("the ggswim package is not installed")))
            }

            # Create base plot with swim lanes
            p <- ggplot2::ggplot()

            # Colour by the NORMALISED label so the figure's legend and the
            # tables name the same categories (see .validateAndProcessData).
            # Older saved states carry only `response`.
            if ("response_label" %in% names(patient_data))
                patient_data$response <- patient_data$response_label

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
                        linewidth = opts$laneWidth
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
                        linewidth = opts$laneWidth
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
                    name = .("Clinical Events"),
                    glyphs = clinical_glyphs,
                    colours = clinical_colors,
                    limits = unique_labels
                )
            }
            
            # Add milestone markers if available  
            if (!is.null(milestone_data) && nrow(milestone_data) > 0) {
                unique_milestones <- unique(milestone_data$label)
                milestone_shapes <- c(15, 16, 17, 18, 19)[seq_along(unique_milestones)]
                names(milestone_shapes) <- unique_milestones
                
                # Milestones are distinguished by SHAPE only, drawn in one fixed
                # colour. Mapping them to `color` as well added a second
                # scale_color_manual() whose values contain only milestone names -
                # and ggplot allows one colour scale per plot, so it replaced the
                # lane scale and every response category (CR/PR/SD/PD) fell through
                # to NA grey. Adding a single milestone silently destroyed the
                # response colouring of the entire figure.
                p <- p + ggplot2::geom_point(
                    data = milestone_data,
                    mapping = ggplot2::aes(
                        x = time,
                        y = patient_id,
                        shape = label
                    ),
                    colour = "grey15",
                    size = opts$markerSize + 1
                ) +
                ggplot2::scale_shape_manual(
                    name = .("Milestones"),
                    values = milestone_shapes
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
                    linewidth = 1.5,
                    alpha = 0.7
                )
            }
            
            # Add reference lines
            if (!is.null(opts$referenceLines) && opts$referenceLines != "none") {
                p <- private$.addReferenceLines(p, opts, stats, patient_data)
            }
            
            # Apply theme and styling. "jamovi" follows the theme the user chose
            # in jamovi's preferences (dark UI -> dark plot), like every other
            # module's plot; it must be added BEFORE the palette scales below,
            # because ggtheme is a complete theme PLUS discrete fill/colour
            # scales and silently discards anything added before it.
            if (identical(opts$theme, "jamovi") && !is.null(ggtheme)) {
                p <- p + ggtheme
            } else if (opts$theme == "ggswim") {
                p <- p + ggswim::theme_ggswim()
            } else if (opts$theme == "ggswim_dark") {
                p <- p + ggswim::theme_ggswim_dark()
            } else {
                p <- p + ggplot2::theme_minimal()
            }

            # Apply color palette (colorblind-safe options)
            if (!is.null(self$options$colorPalette) && self$options$colorPalette != "default") {
                if (self$options$colorPalette == "jamovi") {
                    # Library-review rule: offer the global jamovi palette so a
                    # figure matches the rest of the user's output. `theme` is a
                    # parameter of every render function and carries the palette
                    # the user chose in jamovi's preferences; fall back to "jmv"
                    # when a caller (a test harness) passes none.
                    pal <- if (!is.null(theme)) theme$palette else NULL
                    n_lv <- if ("response" %in% names(patient_data))
                        length(unique(stats::na.omit(as.character(patient_data$response)))) else 1L
                    cols <- jmvcore::colorPalette(max(n_lv, 1L), pal %||% "jmv", "color")
                    p <- p + ggplot2::scale_color_manual(values = unname(cols))
                    p <- p + ggplot2::scale_fill_manual(values = unname(cols))
                } else if (self$options$colorPalette == "viridis") {
                    # Viridis palette - perceptually uniform and colorblind-safe
                    p <- p + ggplot2::scale_color_viridis_d(option = "D", end = 0.9)
                    p <- p + ggplot2::scale_fill_viridis_d(option = "D", end = 0.9)
                } else if (self$options$colorPalette == "contrast") {
                    # High contrast palette (Okabe-Ito colorblind-safe palette).
                    # It has exactly 8 colours, and a manual scale ERRORS when
                    # the data has more levels ("Insufficient values in manual
                    # scale") - the error was caught by the renderer's fallback,
                    # so choosing High Contrast with 9+ response categories
                    # silently replaced the whole swimmer plot with the
                    # simplified one. Hand those cases to viridis, which
                    # generates as many colours as are needed.
                    contrast_colors <- c("#000000", "#E69F00", "#56B4E9", "#009E73",
                                        "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
                    n_lvl <- if ("response" %in% names(patient_data))
                        length(unique(stats::na.omit(as.character(patient_data$response)))) else 0
                    if (n_lvl > length(contrast_colors)) {
                        p <- p + ggplot2::scale_color_viridis_d(option = "D", end = 0.9)
                        p <- p + ggplot2::scale_fill_viridis_d(option = "D", end = 0.9)
                    } else {
                        p <- p + ggplot2::scale_color_manual(values = contrast_colors)
                        p <- p + ggplot2::scale_fill_manual(values = contrast_colors)
                    }
                } else if (self$options$colorPalette == "monochrome") {
                    # Monochrome with varying shades for grayscale publications
                    p <- p + ggplot2::scale_color_grey(start = 0.2, end = 0.8)
                    p <- p + ggplot2::scale_fill_grey(start = 0.2, end = 0.8)
                }
            }

            # Add labels with clinical context
            is_date_scale <- inherits(patient_data$start_time, c("Date", "POSIXct"))
            x_label <- if (is_date_scale) .("Date") else .fmt(.("Time ({unit})"), unit = private$.timeUnitWord(opts$timeUnit))
            p <- p + ggplot2::labs(
                title = .("Patient Timeline Analysis"),
                subtitle = sprintf(.("N=%d patients | Median duration: %.1f %s | Total person-time: %.1f %s"),
                                 stats$n_patients, stats$median_duration, private$.timeUnitWord(opts$timeUnit),
                                 stats$total_person_time, private$.timeUnitWord(opts$timeUnit)),
                x = x_label,
                y = .("Patient ID"),
                # ggswim's arrow layer maps no aesthetic, so it never produces a
                # legend key: the reader saw green arrows with nothing naming
                # them, and the only explanation lived in a glossary panel that
                # is hidden by default and does not travel with an exported
                # image. It is also not "still on treatment" - ANY censored or
                # still-at-risk status draws one.
                caption = if (!is.null(arrow_data) && nrow(arrow_data) > 0)
                    .("Arrow: patient still at risk (censored / alive) at the data cutoff.")
                else NULL
            )
            
            # Legend handling
            if (!opts$showLegend) {
                p <- p + ggplot2::theme(legend.position = "none")
            }
            
            return(p)
        },
        
        .addReferenceLines = function(p, opts, stats, patient_data) {
            is_date_scale <- inherits(patient_data$start_time, c("Date", "POSIXct"))
            duration_axis <- private$.isDurationAxis(patient_data)
            if (opts$referenceLines == "median") {
                if (duration_axis) {
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
                        label = sprintf(.("Median: %s"), round(stats$median_duration, 1)),
                        hjust = -0.1,
                        vjust = 0,
                        angle = 90,
                        size = 3
                    )
                }
            } else if (opts$referenceLines == "protocol") {
                if (duration_axis) {
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

                        # lubridate::duration(), not the Period constructors.
                        # lubridate does NOT export months() - `months` is a base
                        # generic - so lubridate::months() threw, and months is the
                        # DEFAULT timeUnit: a custom reference line on an absolute
                        # date scale killed the whole plot out of the box. The
                        # Period constructors also reject fractional amounts, and
                        # customReferenceTime is a Number with no integer
                        # constraint, so 12.5 crashed too. duration() handles both.
                        cref <- anchor + lubridate::duration(
                            opts$customReferenceTime, units = opts$timeUnit)
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
            # Duration key = the PATIENT's total span (earliest start to latest
            # end), constant across a patient's rows - sorting by per-row episode
            # duration scattered multi-episode patients by whichever episode
            # happened to come first.
            pid_chr <- as.character(patient_data$patient_id)
            startn <- private$.asNumericTime(patient_data$start_time)
            endn <- private$.asNumericTime(patient_data$end_time)
            sort_durations <-
                stats::ave(endn, pid_chr, FUN = function(v) suppressWarnings(max(v, na.rm = TRUE))) -
                stats::ave(startn, pid_chr, FUN = function(v) suppressWarnings(min(v, na.rm = TRUE)))

            ord <- seq_len(nrow(patient_data))
            if (!is.null(self$options$sortVariable)) {
                sv <- self$options$sortVariable
                df <- self$data
                tmp <- data.frame(
                    patient_id = as.character(df[[self$options$patientID]]),
                    sort_val = df[[sv]],
                    stringsAsFactors = FALSE
                )
                tmp <- tmp[!is.na(tmp$patient_id) & !duplicated(tmp$patient_id), ]
                map <- stats::setNames(tmp$sort_val, tmp$patient_id)
                key <- unname(map[as.character(patient_data$patient_id)])
                ord <- order(key, na.last = TRUE, method = "auto")

                # A Sort Variable wins over Sort Order, which is reasonable but
                # was invisible: duration_desc, duration_asc and patient_id all
                # produced the identical picture and nothing said why.
                if (!identical(self$options$sortOrder, "duration_desc")) {
                    private$.addNotice(
                        "INFO",
                        .("Sort order overridden"),
                        sprintf(
                            .("Patients are ordered by the Sort Variable '%s'. The Sort Order setting is not applied while a Sort Variable is selected; clear it to sort by duration, patient ID or response."),
                            sv))
                }
            } else if (self$options$sortOrder == "patient_id") {
                # Numeric-looking IDs sort numerically. Plain order() is
                # lexicographic, so 1, 2, 3, 10, 20 came out "1 10 2 20 3".
                ids <- as.character(patient_data$patient_id)
                num <- suppressWarnings(as.numeric(ids))
                ord <- if (!any(is.na(num) & !is.na(ids)))
                    order(num, method = "auto") else order(ids, method = "auto")
            } else if (self$options$sortOrder == "response" && "response" %in% names(patient_data)) {
                # Clinical hierarchy (CR > PR > SD > PD > NE), not alphabetical
                # factor order, which interleaved PD between PR and SD.
                rank <- match(private$.normalizeResponses(patient_data$response),
                              c("CR", "PR", "SD", "PD", "NE"))
                ord <- order(rank, patient_data$patient_id, na.last = TRUE, method = "auto")
            } else {
                if (self$options$sortOrder == "response") {
                    private$.addNotice(
                        "INFO",
                        .("Response sorting unavailable"),
                        .("Sort order 'Response Type' requires a Response/Status variable; none is selected, so patients are sorted by duration (longest first)."))
                }
                # duration_desc, and any fallthrough, sorts longest-first; only
                # an explicit duration_asc sorts shortest-first.
                dec <- !identical(self$options$sortOrder, "duration_asc")
                ord <- order(sort_durations, decreasing = dec, method = "auto")
            }

            # ggplot puts factor level 1 at the BOTTOM of a discrete y axis, so
            # the levels are reversed here: "Duration (Longest First)" now reads
            # longest-first from the TOP, which is how every reader scans a
            # swimmer plot. Before this, all four sort orders were shown upside
            # down.
            ordered_ids <- as.character(patient_data$patient_id[ord])
            patient_data$patient_id <- factor(as.character(patient_data$patient_id),
                                              levels = rev(unique(ordered_ids)))
            patient_data
        },
        
        .createFallbackPlot = function(patient_data, milestone_data = NULL, event_data = NULL, opts = NULL, stats = NULL, error_message = "ggswim unavailable") {
            ggplot2::ggplot(patient_data, ggplot2::aes(x = start_time, y = patient_id)) +
                ggplot2::geom_point(size = 2, color = "steelblue") +
                ggplot2::labs(
                    title = .("Swimmer Plot (Simplified)"),
                    subtitle = sprintf(.("Error in enhanced plot: %s"), error_message),
                    x = .("Time"),
                    y = .("Patient ID")
                ) +
                ggplot2::theme_minimal()
        },
        
        # Clinical glyph mapping for event markers.
        #
        # These are print-safe geometric symbols (BMP Unicode, escaped for
        # R CMD check), NOT emoji. Every entry in this table was previously the
        # empty string "" - the emoji that once lived here were deleted rather
        # than escaped during a non-ASCII sweep, so ggswim::scale_marker_discrete()
        # drew nothing and event markers were invisible for every labelled event.
        # Geometric symbols also render correctly in PDF/Word exports and
        # regulatory documents, which the emoji did not.
        .getEnhancedClinicalGlyphs = function(event_labels) {
            CIRCLE   <- "\u25cf"   # black circle
            SQUARE   <- "\u25a0"   # black square
            UP       <- "\u25b2"   # black up-pointing triangle
            DOWN     <- "\u25bc"   # black down-pointing triangle
            DIAMOND  <- "\u25c6"   # black diamond
            STAR     <- "\u2605"   # black star
            CROSS    <- "\u271a"   # heavy greek cross
            XMARK    <- "\u2716"   # heavy multiplication x
            HALF     <- "\u25d1"   # circle with right half black
            RING     <- "\u25ce"   # bullseye

            # Define clinical icon mappings
            clinical_mapping <- list(
                # Treatment events
                "treatment" = CROSS, "therapy" = CROSS, "drug" = CROSS,
                "medication" = CROSS, "infusion" = CROSS, "injection" = CROSS,
                "dose" = CROSS,
                "surgery" = DIAMOND, "operation" = DIAMOND, "procedure" = DIAMOND,

                # Response events
                "response" = HALF, "assessment" = HALF, "evaluation" = HALF,
                "progression" = UP, "recurrence" = UP, "relapse" = UP,
                "remission" = STAR, "complete response" = STAR, "cr" = STAR,
                "partial response" = HALF, "pr" = HALF,
                "stable disease" = SQUARE, "sd" = SQUARE,
                "progressive disease" = UP, "pd" = UP,

                # Adverse events
                "adverse event" = XMARK, "ae" = XMARK, "toxicity" = XMARK,
                "death" = XMARK, "mortality" = XMARK,

                # Follow-up events
                "follow-up" = CIRCLE, "visit" = CIRCLE, "appointment" = CIRCLE,
                "scan" = RING, "imaging" = RING, "ct" = RING, "mri" = RING,

                # Generic events
                "event" = DOWN, "milestone" = DOWN, "endpoint" = DOWN
            )

            # Create glyph vector
            glyphs <- character(length(event_labels))
            names(glyphs) <- event_labels

            default_symbols <- c(CIRCLE, SQUARE, UP, DIAMOND, STAR,
                                 DOWN, CROSS, XMARK, HALF, RING)

            # Map each label to appropriate glyph
            for (i in seq_along(event_labels)) {
                label <- tolower(event_labels[i])

                # Try exact match first
                if (label %in% names(clinical_mapping)) {
                    glyphs[i] <- clinical_mapping[[label]]
                } else {
                    # Try partial matches, longest pattern first so a specific
                    # phrase wins over a substring of it. Short patterns must
                    # match as whole words: a bare fixed-string "ct" also matches
                    # "Infarction" and "Reaction", which handed those events the
                    # CT-imaging glyph.
                    patterns <- names(clinical_mapping)
                    patterns <- patterns[order(nchar(patterns), decreasing = TRUE)]

                    matches <- vapply(patterns, function(pattern) {
                        if (nchar(pattern) <= 3) {
                            grepl(paste0("\\b", pattern, "\\b"), label, perl = TRUE)
                        } else {
                            grepl(pattern, label, fixed = TRUE)
                        }
                    }, logical(1))

                    if (any(matches)) {
                        first_match <- patterns[which(matches)[1]]
                        glyphs[i] <- clinical_mapping[[first_match]]
                    } else {
                        # Fallback to default symbols
                        glyphs[i] <- default_symbols[((i - 1) %% length(default_symbols)) + 1]
                    }
                }
            }

            return(glyphs)
        },

        # Generate clinical glossary (static; body in R/swimmerplot-html.R)
        .generateClinicalGlossary = function() {
            self$results$clinicalGlossary$setContent(swimmerplot_glossary_html(self))
        },

        # Generate copy-ready manuscript text
        .generateCopyReadyReport = function(stats, patient_data) {
            # Basic study description
            # Manuscript convention is to report median follow-up by the reverse
            # Kaplan-Meier method and to name the method. Print the estimator that
            # was actually used, and keep the observed range attached to the
            # observed median rather than to the KM one - pairing a KM median with
            # an observed range is how the summary table came to show a median
            # outside its own range.
            fu_method <- stats$median_followup_method %||% "observed"
            fu_value  <- if (identical(fu_method, "reverse_km"))
                stats$median_followup_km else stats$median_duration
            # library-audit 2026-09-16 OncoPath [LOW] DEFERRED: the follow-up label is spliced into sentences via %s;
            #   revisit when these sentences are next rewritten or translated (guide section 9)
            fu_label  <- switch(
                fu_method,
                reverse_km   = .("median follow-up (reverse Kaplan-Meier)"),
                unrecognised = .("median observed follow-up duration (censoring coding not recognised)"),
                .("median observed follow-up duration")
            )

            # The interval belongs in the pasted sentence too - it was computed,
            # shown in the table and then dropped from the text people actually
            # publish.
            fu_ci <- if (identical(fu_method, "reverse_km"))
                private$.followUpCIText(stats$median_followup_ci_lower,
                                        stats$median_followup_ci_upper) else NA_character_
            fu_value_text <- if (!is.na(fu_ci))
                sprintf(.("%.1f %s (95%% CI %s)"), fu_value, private$.timeUnitWord(), fu_ci)
                else sprintf("%.1f %s", fu_value, private$.timeUnitWord())

            # "1 patients" appeared in text offered for direct use in a
            # manuscript. Two complete alternatives rather than a bare %d, so a
            # translator can inflect each properly.
            n_pat_text <- if (isTRUE(stats$n_patients == 1)) .("1 patient")
                          else sprintf(.("%d patients"), stats$n_patients)

            basic_text <- sprintf(
                .("Patient timelines were analyzed using swimmer plots to visualize treatment courses and clinical outcomes. The study included %s with a %s of %s; observed durations ranged from %.1f to %.1f %s. Total person-time was %.1f %s."),
                n_pat_text,
                fu_label,
                fu_value_text,
                stats$min_duration,
                stats$max_duration,
                private$.timeUnitWord(),
                stats$total_person_time,
                private$.timeUnitWord()
            )

            # Add response analysis if available
            response_text <- ""
            if (self$options$responseAnalysis && "response" %in% names(patient_data) && !is.null(stats$response_counts)) {
                rates <- private$.responseRates(stats$response_counts, stats$n_patients)
                # All patients in the analysis - the same denominator the
                # point estimates and their intervals use, so the pasted
                # sentence cannot quote a rate and an interval computed over
                # different cohorts. RECIST 1.1 section 4.9.1 requires the rate
                # over every patient, not over an evaluable subset; this
                # sentence used to say "4/9 RECIST-evaluable patients" for a
                # 12-patient cohort.
                n_eval <- rates$n_all
                orr_count <- rates$orr_count
                orr_pct <- rates$orr
                dcr_count <- rates$dcr_count
                dcr_pct <- rates$dcr

                # Calculate 95% CIs for copy-ready text
                orr_ci_text <- ""
                dcr_ci_text <- ""

                orr_test <- if (!rates$evaluable) NULL else tryCatch({
                    binom.test(orr_count, n_eval, conf.level = 0.95)
                }, error = function(e) NULL)

                if (!is.null(orr_test)) {
                    orr_ci_text <- sprintf("; 95%% CI: %.1f%%-%.1f%%",
                                          orr_test$conf.int[1] * 100,
                                          orr_test$conf.int[2] * 100)
                }

                dcr_test <- if (!rates$evaluable) NULL else tryCatch({
                    binom.test(dcr_count, n_eval, conf.level = 0.95)
                }, error = function(e) NULL)

                if (!is.null(dcr_test)) {
                    dcr_ci_text <- sprintf("; 95%% CI: %.1f%%-%.1f%%",
                                          dcr_test$conf.int[1] * 100,
                                          dcr_test$conf.int[2] * 100)
                }

                response_text <- if (!rates$evaluable) {
                    .("Response categories were not RECIST-coded, so objective response and disease control rates were not calculated.")
                } else {
                    # sprintf, NOT jmvcore::format: format() silently leaves
                    # underscored placeholder names ({orr_n}) unreplaced, so this
                    # sentence used to paste literal "{orr_n}"/"{orr_ci}" into
                    # manuscripts.
                    sprintf(
                        .("Response evaluation showed an objective response rate (ORR) of %.1f%% (%d/%d patients%s) and a disease control rate (DCR) of %.1f%% (%d/%d patients%s). Rates are over all patients analysed, with not-evaluable and unrecorded responses counted as non-responders (RECIST 1.1 section 4.9.1)."),
                        orr_pct, orr_count, n_eval, orr_ci_text,
                        dcr_pct, dcr_count, n_eval, dcr_ci_text)
                }
            }

            # Methodology note
            methods_text <- .("Timeline visualization was created using the ggswim package.")

            # The small-sample STRONG WARNING lives in the notices panel and does
            # not travel with text the user copies into a manuscript, so an n=1
            # cohort produced "ORR 100.0% (1/1 patients; 95% CI 2.5%-100.0%)"
            # under the heading "formatted for direct use". Carry the caveat.
            caveat_text <- if (isTRUE(stats$n_patients < 10))
                sprintf(.("With only %d patients these estimates are exploratory: the confidence intervals are wide and the rates should not be read as precise."),
                        stats$n_patients) else ""

            full_text <- paste(Filter(nzchar, c(basic_text, response_text, methods_text, caveat_text)), collapse = " ")

            copy_ready_html <- paste0(
                "<div style='background-color: rgba(33, 159, 33, 0.1); padding: 20px; border-left: 4px solid #28a745; border-radius: 8px; margin: 15px 0; font-family: system-ui, -apple-system, sans-serif; color: inherit;'>",
                "<h3 style='color: inherit; margin-top: 0; display: flex; align-items: center;'>",
                "<span style='margin-right: 8px;'></span>",
                .("Copy-Ready Manuscript Text"),
                "</h3>",
                "<div style='background-color: rgba(255, 255, 255, 0.06); color: inherit; padding: 15px; border-radius: 6px; margin: 10px 0; box-shadow: 0 1px 3px rgba(0,0,0,0.1);'>",
                "<p style='margin: 0; line-height: 1.6; color: inherit; font-size: 0.95em; text-align: justify;'>", full_text, "</p>",
                "</div>",
                "<div style='margin-top: 15px; padding: 10px; background-color: rgba(33, 163, 188, 0.21); border-radius: 4px; border: 1px dashed #0c5460; color: inherit;'>",
                "<p style='margin: 0; font-size: 0.85em; color: inherit;'>",
                "<strong>", .("Usage:"), "</strong> ",
                .("This text is formatted for direct use in manuscripts and clinical reports. Copy and paste into your document and adjust as needed for your specific requirements."),
                "</p>",
                "</div>",
                "</div>"
            )

            self$results$copyReadyReport$setContent(copy_ready_html)
        },

        # Generate about analysis information (static; body in R/swimmerplot-html.R)
        .generateAboutAnalysis = function() {
            self$results$aboutAnalysis$setContent(swimmerplot_about_html(self))
        }
    ), # End of private list
    public = list(
        #' @description
        #' Generate R source code for Swimmer Plot analysis
        #' @return Character string with R syntax for reproducible analysis
        asSource = function() {
            if (is.null(self$options$startTime) || is.null(self$options$endTime))
                return('')

            # Build the argument list in option-declaration order.
            #
            # Every variable-name option (single OptionVariable or multi-variable
            # OptionVariables) is emitted as a deparse()'d string literal. deparse()
            # produces valid, fully-escaped R for names containing spaces, quotes or
            # backslashes (e.g. `My Var`); the previous manual `paste0('\`', name, '\`')`
            # embedded a literal backtick INSIDE the quoted string, which is invalid.
            # Detecting the option by CLASS (not by name) means any variable option added
            # later is escaped automatically.
            #
            # Variables are NOT re-emitted through private$.asArgs() - doing so previously
            # duplicated startTime/endTime/groupVar in the generated syntax (the known
            # "double variables" codegen bug). All non-variable options keep jmvcore's
            # per-option sourcify so formatting stays consistent with jamovi.
            args <- character(0)
            for (option in private$.options$options) {
                if (option$name == 'data')
                    next
                # Variables go through deparse(), NOT .sourcifyOption(), and that
                # divergence is deliberate - do not "unify" it. Measured on a
                # column named  weird"name :
                #   .sourcifyOption -> responseVar = weird"name    (unbalanced quote)
                #   deparse         -> responseVar = "weird\"name"  (correct)
                # .sourcifyOption does not quote a Variable value at all, so any
                # name containing a quote, a backslash or a space produces a
                # syntax-pane snippet the user cannot paste back.
                if (inherits(option, 'OptionVariable') || inherits(option, 'OptionVariables')) {
                    val <- option$value
                    if (!is.null(val) && length(val) > 0)
                        args <- c(args, paste0(option$name, ' = ',
                                               paste0(deparse(val), collapse = '')))
                } else {
                    as <- private$.sourcifyOption(option)
                    if (!identical(as, ''))
                        args <- c(args, as)
                }
            }

            # Get package name dynamically
            pkg_name <- utils::packageName()
            if (is.null(pkg_name)) pkg_name <- "ClinicoPath"  # fallback

            # Build complete function call
            paste0(pkg_name, '::swimmerplot(\n    data = data,\n    ',
                   paste(args, collapse = ',\n    '), ')')
        }
    ) # End of public list
)
