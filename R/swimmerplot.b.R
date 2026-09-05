.datatable.aware <- TRUE

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
#' @return An \code{R6} class generator object for the \code{swimmerplotClass} backend; used internally by the jamovi analysis wrapper and not called directly.

swimmerplotClass <- if (requireNamespace('jmvcore', quietly = TRUE)) R6::R6Class(
    "swimmerplotClass",
    inherit = swimmerplotBase,
    private = list(
        # Notice collection helpers. A single Preformatted (plain-text) output item:
        # avoids both the jmvcore::Notice serialization error and any HTML in
        # notices (project convention:
        # notice content must be plain text). ====
        .noticeList = list(),

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
            blocks <- vapply(private$.noticeList, function(notice) {
                prefix <- switch(notice$type,
                    ERROR          = "ERROR: ",
                    STRONG_WARNING = "WARNING: ",
                    WARNING        = "WARNING: ",
                    INFO           = "NOTE: ",
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

        # Single source of truth for ORR/DCR.
        #
        # ORR and DCR are defined only for RECIST-coded responses. When the
        # response variable uses some other coding ("Responder", 0/1, "Grade 1")
        # nothing normalises to CR/PR/SD/PD and the numerator is legitimately
        # zero - which read as a genuine "ORR 0.0%" with an exact binomial CI.
        # The guard lived only in .updateAdvancedMetrics, so the copy-ready
        # manuscript text went on asserting 0.0% in the same output that the
        # metrics table refused to make the claim in. Both call this now.
        .responseRates = function(response_counts) {
            if (is.null(response_counts)) return(NULL)

            nm <- names(response_counts)
            total <- sum(response_counts)
            recist_n <- sum(response_counts[nm %in% c("CR", "PR", "SD", "PD")])
            n_excluded <- total - recist_n

            if (!is.finite(total) || total <= 0 || recist_n == 0) {
                return(list(evaluable = FALSE, n = total, n_evaluable = 0L,
                            n_excluded = n_excluded,
                            orr_count = NA_integer_, dcr_count = NA_integer_,
                            orr = NA_real_, dcr = NA_real_))
            }

            # RECIST-evaluable denominator (CR/PR/SD/PD), matching the module's
            # waterfall analysis. NE and unrecognised labels previously sat in
            # the denominator UNDISCLOSED, deflating both rates and their exact
            # binomial CIs whenever codings were mixed.
            if (n_excluded > 0) {
                private$.addNotice(
                    "WARNING",
                    .("Non-RECIST responses excluded from ORR/DCR"),
                    sprintf(
                        .("%d of %d patients have a response that is not CR/PR/SD/PD (e.g. NE or an unrecognised label) and are excluded from the ORR and DCR denominators. Rates below are computed over the %d RECIST-evaluable patients."),
                        n_excluded, total, recist_n))
            }

            orr_count <- sum(response_counts[nm %in% c("CR", "PR")])
            dcr_count <- sum(response_counts[nm %in% c("CR", "PR", "SD")])

            list(evaluable = TRUE, n = total, n_evaluable = recist_n,
                 n_excluded = n_excluded,
                 orr_count = orr_count, dcr_count = dcr_count,
                 orr = orr_count / recist_n * 100,
                 dcr = dcr_count / recist_n * 100)
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

            # Check for extremely long follow-up periods
            if (self$options$timeUnit == "days") {
                long_followup <- which(durations > 3650) # >10 years
                if (length(long_followup) > 0) {
                    warnings <- append(warnings, sprintf(
                        .("Found %d patients with follow-up >10 years. Consider checking data accuracy or using different time units."),
                        length(long_followup)
                    ))
                }
            } else if (self$options$timeUnit == "months") {
                long_followup <- which(durations > 120) # >10 years
                if (length(long_followup) > 0) {
                    warnings <- append(warnings, sprintf(
                        .("Found %d patients with follow-up >10 years in months. Consider data validation."),
                        length(long_followup)
                    ))
                }
            }

            # Check for zero-duration events
            zero_durations <- which(durations == 0)
            if (length(zero_durations) > 0) {
                warnings <- append(warnings, sprintf(
                    .("Found %d patients with zero follow-up time. These may represent same-day events."),
                    length(zero_durations)
                ))
            }

            # Check for duplicate patient IDs (potential data issue)
            duplicate_ids <- patient_data$patient_id[duplicated(patient_data$patient_id)]
            if (length(duplicate_ids) > 0) {
                warnings <- append(warnings, sprintf(
                    .("Found %d duplicate patient IDs. Multiple episodes per patient detected - this is normal for longitudinal data."),
                    length(duplicate_ids)
                ))
            }

            # Response variable validation
            if ("response" %in% names(patient_data)) {
                missing_response <- sum(is.na(patient_data$response))
                if (missing_response > 0) {
                    warnings <- append(warnings, sprintf(
                        .("Found %d patients with missing response data (%.1f%% of total)."),
                        missing_response,
                        missing_response / nrow(patient_data) * 100
                    ))
                }

                # Check for unusual response patterns
                response_counts <- table(patient_data$response, useNA = "no")
                if (length(response_counts) > 0) {
                    min_category <- min(response_counts)
                    if (min_category < 3) {
                        warnings <- append(warnings,
                            .("Some response categories have <3 patients. Consider grouping categories for meaningful analysis.")
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
                    self$options$timeUnit))
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
                
                patient_data$start_time <- start_parsed$value
                patient_data$end_time <- end_parsed$value

                # A wrong Date Format choice parses to all-NA WITHOUT an error
                # (lubridate returns NA), and the rows then died in the validity
                # filter with a message about end < start - misdirecting the
                # user away from the actual cause. Name it here instead.
                if (all(is.na(patient_data$start_time)) || all(is.na(patient_data$end_time))) {
                    return(list(error = TRUE, message = .fmt(
                        .("None of the start/end values could be parsed as dates with the selected Date Format ({fmt}). Choose the format that matches how your dates are written (e.g. 2023-01-15 needs YYYY-MM-DD)."),
                        fmt = self$options$dateFormat)))
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
                    patient_data$start_time <- suppressWarnings(as.numeric(as.character(patient_data$start_time)))
                    patient_data$end_time <- suppressWarnings(as.numeric(as.character(patient_data$end_time)))

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

                reasons <- character(0)
                if (n_bad_id > 0)
                    reasons <- c(reasons, sprintf(.("%d with a missing patient ID"), n_bad_id))
                if (n_bad_time > 0)
                    reasons <- c(reasons, sprintf(.("%d with a missing start or end time"), n_bad_time))
                if (n_bad_order > 0)
                    reasons <- c(reasons, sprintf(.("%d where the end time precedes the start time"), n_bad_order))

                private$.addNotice(
                    "WARNING",
                    .("Rows excluded from analysis"),
                    sprintf(
                        .("%d of %d rows were excluded before analysis (%s). All counts, rates and person-time below are based on the remaining %d rows."),
                        n_dropped, length(valid_rows),
                        paste(reasons, collapse = "; "), sum(valid_rows)
                    )
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
            
            for (i in 1:self$options$maxMilestones) {
                name_opt <- paste0("milestone", i, "Name")
                date_opt <- paste0("milestone", i, "Date")
                
                if (!is.null(self$options[[date_opt]]) && 
                    !is.null(self$options[[name_opt]]) && 
                    self$options[[name_opt]] != "") {
                    
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
                                    self$options[[name_opt]],
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
                                    self$options[[name_opt]]))
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
                ongoing_by_pt <- vapply(split(status, pid), function(s) {
                    s <- s[!is.na(s)]
                    length(s) > 0 && identical(s[length(s)], "censored")
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
                    .("Ongoing-treatment arrows not drawn"),
                    .("Ongoing-status arrows require a censoring/event status variable. Without one, whether a patient was still on treatment at data cutoff cannot be determined from the timeline alone, so no arrows are drawn. Supply a censoring variable (0/FALSE/no/censored/alive for ongoing, 1/TRUE/yes/event/dead for completed) to show them.")
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
                        event_data <- event_data[keep, c("patient_id", "time", "label"), drop = FALSE]
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

            # Performance optimization: Use data.table for large datasets (>1000 rows)
            use_fast_path <- nrow(patient_data) > 1000 && requireNamespace("data.table", quietly = TRUE)

            if (use_fast_path) {
                # Fast path with data.table (5-10x faster for large datasets)
                dt <- data.table::as.data.table(patient_data)

                # Group by patient and aggregate
                summary_list <- dt[, {
                    follow_up <- private$.calculateFollowUp(start_time, end_time)
                    person_time <- private$.mergeIntervalsAndSum(start_time, end_time)
                    if (is.na(person_time) || !is.finite(person_time)) person_time <- follow_up

                    response_value <- NA_character_
                    if ("response" %in% names(.SD)) {
                        non_missing <- as.character(response[!is.na(response)])
                        if (length(non_missing) > 0) {
                            # Normalise here so every consumer agrees. .getBestResponse
                            # returns the ORIGINAL string, .calculateSummaryStats
                            # normalised it before tabulating, but .updatePersonTimeTable
                            # grouped on the raw label - so "CR", "Complete Response" and
                            # "complete response" became three rows of n=1 that
                            # contradicted every other table on the page.
                            response_value <- private$.normalizeResponse(
                                private$.getBestResponse(non_missing))
                        }
                    }

                    censor_value <- NA
                    if ("censor_status" %in% names(.SD)) {
                        censor_last <- censor_status[!is.na(censor_status)]
                        if (length(censor_last) > 0) censor_value <- tail(censor_last, 1)
                    }

                    group_value <- NA
                    if ("patient_group" %in% names(.SD)) {
                        group_first <- patient_group[!is.na(patient_group)]
                        # as.character: the base path returns character, and a
                        # factor here kept NA-dropped levels alive in the Fisher
                        # contingency tables above 1000 rows.
                        if (length(group_first) > 0) group_value <- as.character(group_first[1])
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

                summary_list <- split(summary_list, summary_list$patient_id)
            } else {
                # Standard path with base R (works for all dataset sizes)
                split_data <- split(patient_data, patient_data$patient_id)

                summary_list <- lapply(split_data, function(df) {
                    follow_up <- private$.calculateFollowUp(df$start_time, df$end_time)

                    # Calculate person-time by merging overlapping intervals to avoid double-counting
                    # This ensures unique observation time is counted
                    person_time <- private$.mergeIntervalsAndSum(df$start_time, df$end_time)
                    if (is.na(person_time) || !is.finite(person_time)) person_time <- follow_up

                # Get BEST response for ORR/DCR calculation (clinical standard in oncology)
                # Hierarchy: CR > PR > SD > PD > NE/Other
                response_value <- NA_character_
                if ("response" %in% names(df)) {
                    non_missing <- as.character(df$response[!is.na(df$response)])
                    if (length(non_missing) > 0) {
                        # Normalised at the source - see the data.table path above.
                        response_value <- private$.normalizeResponse(
                            private$.getBestResponse(non_missing))
                    }
                }

                # Get LAST censoring status (most relevant for follow-up calculations)
                censor_value <- NA
                if ("censor_status" %in% names(df)) {
                    non_missing_censor <- df$censor_status[!is.na(df$censor_status)]
                    if (length(non_missing_censor) > 0) {
                        # Use the last non-missing censor status
                        censor_value <- non_missing_censor[length(non_missing_censor)]
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

            patient_data$segment_duration <- NULL
            dplyr::bind_rows(summary_list)
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
                epoch_secs <- if (inherits(start_times, "Date") ||
                                  inherits(end_times, "Date")) 86400 else 1

                seg_start <- as.POSIXct(merged_starts * epoch_secs,
                                        origin = "1970-01-01", tz = "UTC")
                seg_end   <- as.POSIXct(merged_ends * epoch_secs,
                                        origin = "1970-01-01", tz = "UTC")

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
        .getBestResponse = function(responses) {
            if (length(responses) == 0) return(NA_character_)

            syn <- private$.responseSynonymMap()
            # Rank of each standard abbreviation (lower rank = better response)
            response_rank <- c("CR" = 1, "PR" = 2, "SD" = 3, "PD" = 4, "NE" = 5)

            responses_lower <- tolower(trimws(responses))

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
            if (n_unrec > 0 && !all(is.na(status))) {
                private$.addNotice(
                    "WARNING",
                    .("Some censoring values not recognised"),
                    sprintf(
                        .("%d of %d censoring/event values could not be interpreted; those patients are treated as completed follow-up (events) in the reverse Kaplan-Meier estimate and receive no ongoing-treatment arrow. Use 0/FALSE/no/censored/alive for ongoing and 1/TRUE/yes/event/dead for completed."),
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

            # Reverse KM: censored patients become the "events".
            reverse_status <- as.numeric(status %in% "censored")

            surv_obj <- tryCatch(
                survival::Surv(time = fu, event = reverse_status),
                error = function(e) NULL)
            if (is.null(surv_obj)) return(observed)

            km_fit <- tryCatch(survival::survfit(surv_obj ~ 1), error = function(e) NULL)
            if (is.null(km_fit)) return(observed)

            median_fu <- tryCatch(
                stats::quantile(km_fit, probs = 0.5)$quantile,
                error = function(e) NA_real_)

            # quantile() returns NA (not an error) when the reverse curve never
            # reaches 0.5 - too few censored patients to estimate it.
            if (length(median_fu) == 0 || is.na(median_fu)) return(observed)

            list(value = unname(median_fu), method = "reverse_km")
        },

        # Map a censoring/event status value to "censored", "event", or NA.
        # Shared by median follow-up and the ongoing-status arrows so the two
        # cannot disagree about what a given coding means.
        .classifyCensoring = function(x) {
            v <- tolower(trimws(as.character(x)))
            num <- suppressWarnings(as.numeric(v))

            out <- rep(NA_character_, length(v))
            out[v %in% c("0", "false", "f", "no", "n", "censored", "cens",
                         "alive", "ongoing", "active", "continuing")] <- "censored"
            out[v %in% c("1", "true", "t", "yes", "y", "event", "dead", "died",
                         "death", "progressed", "progression", "completed")] <- "event"

            out[is.na(out) & !is.na(num) & num == 0] <- "censored"
            out[is.na(out) & !is.na(num) & num != 0] <- "event"
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
                normalized_responses <- sapply(patient_summary$response, private$.normalizeResponse, USE.NAMES = FALSE)

                response_summary <- table(normalized_responses, useNA = "no")
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
                    .("Most common response was %s (%.1f%% of patients)."),
                    htmltools::htmlEscape(best_response),
                    best_pct
                )
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

        # Apply clinical preset configurations with context

        .init = function() {
            # Initialize instructions when no variables selected
            if (is.null(self$options$patientID) ||
                is.null(self$options$startTime) ||
                is.null(self$options$endTime)) {

                # ERROR notice for missing required variables
                private$.addNotice('ERROR', .("Missing required variables"),
                    .("Patient ID, Start Time, and End Time are required to generate a swimmer plot. Please select all three variables in the Core Data Variables section."))

                # Keep detailed HTML guidance
                instructions <- private$.generateInstructions()
                self$results$instructions$setContent(instructions)
            }

            # Fixed row structure for the summary table: the same five metrics on
            # every run. Only the values are computed, so .updateSummaryTable()
            # fills them with setRow(). The response-rate rows that follow them
            # depend on the levels actually present and stay in .run().
            # ("Mean Follow-up" was dropped: it was the identical statistic as
            # "Mean Duration" printed twice under two clinical names.)
            summary_metrics <- c(
                .("Number of Patients"),
                .("Total Observations"),
                .("Median Duration (observed)"),
                .("Mean Duration"),
                .("Total Person-Time")
            )
            if (self$results$summary$rowCount == 0)
                for (i in seq_along(summary_metrics))
                    self$results$summary$addRow(
                        rowKey = i, values = list(metric = summary_metrics[i]))
        },

        .run = function() {
            # Reset notice collection AND re-render immediately: a run that
            # produces zero notices must clear the previous run's text, which
            # only .addNotice() used to do.
            private$.noticeList <- list()
            private$.renderNotices()

            # Reset stale HTML notice outputs so content from a previous run does not
            # linger after the triggering condition (low Fisher cell counts, absolute-date
            # reference lines, an earlier validation error in `instructions`) has
            # cleared. None of these items has a clearWith rule that covers every
            # trigger.
            self$results$warningNotice$setContent('')
            self$results$warningNotice$setVisible(FALSE)
            self$results$validationReport$setContent('')
            self$results$validationReport$setVisible(FALSE)
            self$results$instructions$setContent('')

            # Static educational panels: populate whenever requested, BEFORE any
            # early return - previously an incomplete selection or a validation
            # abort left visible-but-empty "Clinical Glossary" / "About" shells.
            if (self$options$showGlossary)
                private$.generateClinicalGlossary()
            if (self$options$showAbout)
                private$.generateAboutAnalysis()

            # Enhanced instructions with comprehensive guidance
            if (is.null(self$options$patientID) ||
                is.null(self$options$startTime) ||
                is.null(self$options$endTime)) {

                # ERROR notice for missing required variables
                private$.addNotice('ERROR', .("Missing required variables"),
                    .("Patient ID, Start Time, and End Time are required to generate a swimmer plot. Please select all three variables in the Core Data Variables section."))

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
                    # REPLACED Notice with HTML to prevent serialization errors
                    # Escape user-derived example values before HTML interpolation
                    safe_examples <- if (!is.null(validation_result$examples)) {
                        htmltools::htmlEscape(as.character(validation_result$examples))
                    } else {
                        character(0)
                    }
                    # Detailed HTML guidance (body in R/swimmerplot_html.R)
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
                    # (body in R/swimmerplot_html.R)
                    date_guidance <- swimmerplot_date_guidance_html(self, safe_format, safe_examples_date)
                    self$results$instructions$setContent(date_guidance)
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
                    # Surface via the ERROR-notice channel too, then return early so the
                    # tailored guidance above is preserved. Previously this stop()-ed into
                    # the generic outer error handler, which overwrote the specific message.
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

                if (length(warning_messages) > 0) {
                    warning_msg <- paste0(
                        "<div style='color: inherit; background-color: rgba(255, 203, 33, 0.14); padding: 15px; border: 1px solid #ffc107; border-radius: 5px; margin: 10px;'>",
                        "<h4>", .("Analysis Information"), "</h4>",
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
                            "<div style='background-color: rgba(255, 203, 33, 0.14); border:1px solid #f0c36d; color: inherit; padding:12px; border-radius:6px; margin:10px 0;'>",
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
                                "<div style='background-color: rgba(33, 159, 43, 0.1); border:1px solid #a5d6a7; color: inherit; padding:12px; border-radius:6px; margin:10px 0;'>",
                                "<strong>", .("Custom reference in absolute mode:"), "</strong> ",
                                .("No 'Custom Reference Date' provided; using 'Custom Reference Time' as an offset from the earliest start date."),
                                "</div>"
                            )
                            self$results$validationReport$setContent(note_html)
                            try(self$results$validationReport$setVisible(TRUE), silent = TRUE)
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
                        colorPalette = self$options$colorPalette,
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

                # (Glossary and About are populated at the top of .run(), before
                # the early-return branches, because their content is static.)
                if (self$options$showCopyReady) {
                    private$.generateCopyReadyReport(stats, patient_data)
                }

            }, error = function(e) {
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
            # R/swimmerplot_html.R (fully .()-wrapped).
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

            # Add response statistics if available. These rows are appended, so
            # re-set a key a previous run already created rather than adding it
            # twice; a changed responseVar clears the table via clearWith.
            if (!is.null(stats$response_counts)) {
                for (response in names(stats$response_counts)) {
                    row_key <- paste0("response_", response)
                    row_values <- list(
                        metric = .fmt(.("{response} Rate (%)"), response = response),
                        value = round(stats$response_percentages[[response]], 1)
                    )
                    if (any(vapply(summary_table$rowKeys, identical, logical(1), row_key)))
                        summary_table$setRow(rowKey = row_key, values = row_values)
                    else
                        summary_table$addRow(rowKey = row_key, values = row_values)
                }
            }
        },
        
        # Person-time analysis table population
        .updatePersonTimeTable = function(patient_data, stats) {
            # Clear FIRST: the early returns below used to precede deleteRows(),
            # so a run whose data no longer supports the table kept stale rows.
            self$results$personTimeTable$deleteRows()

            if (!self$options$personTimeAnalysis) return()

            patient_summary <- stats$patient_summary
            if (is.null(patient_summary)) {
                patient_summary <- private$.summarizeByPatient(patient_data)
            }

            if (!"response" %in% names(patient_summary)) return()

            patient_summary <- patient_summary[!is.na(patient_summary$response), , drop = FALSE]
            if (nrow(patient_summary) == 0) return()

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
                    response_type = as.character(person_time_data$response[i]),
                    n_patients = person_time_data$n_patients[i],
                    total_time = round(person_time_data$total_time[i], 2),
                    mean_time = round(person_time_data$mean_time[i], 2),
                    incidence_rate = round(person_time_data$followup_density[i], 3)
                ))
            }
            self$results$personTimeTable$setNote("density", .fmt(
                .("Follow-up density = patients per 100 {unit} of person-time (a descriptive measure, not an event rate). Times are in {unit}."),
                unit = self$options$timeUnit))
        },
        
        # Milestone table population
        .updateMilestoneTable = function(patient_data, milestone_data) {
            # Clear FIRST so an empty-input run does not keep stale rows
            self$results$milestoneTable$deleteRows()

            if (nrow(milestone_data) == 0) return()

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
            # Clear FIRST so an empty-input run does not keep stale rows
            self$results$eventMarkerTable$deleteRows()

            if (!self$options$showEventMarkers || is.null(event_data) || nrow(event_data) == 0) return()

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
            # Clear FIRST so a run without the option does not keep stale rows
            self$results$advancedMetrics$deleteRows()

            if (!self$options$personTimeAnalysis) return()

            patient_summary <- stats$patient_summary
            if (is.null(patient_summary)) {
                patient_summary <- private$.summarizeByPatient(patient_data)
            }

            n_patients_summary <- nrow(patient_summary)

            # Calculate advanced clinical metrics
            metrics <- list(
                list(
                    name = switch(
                        stats$median_followup_method %||% "observed",
                        reverse_km = .("Median Follow-up Time (reverse Kaplan-Meier)"),
                        unrecognised = .("Median Follow-up Time (observed durations; censoring not recognised)"),
                        .("Median Follow-up Time (observed durations; no censoring information)")
                    ),
                    value = round(stats$median_followup_km, 2),
                    ci = NA_character_,
                    unit = self$options$timeUnit,
                    interpretation = .("Central tendency of patient follow-up duration")
                ),
                list(
                    name = .("Interquartile Range (observed durations)"),
                    value = round(stats$q3_duration - stats$q1_duration, 2),
                    ci = NA_character_,
                    unit = self$options$timeUnit,
                    interpretation = .("Middle 50% of follow-up duration range")
                ),
                list(
                    name = .("Total Study Person-Time"),
                    value = round(stats$total_person_time, 2),
                    ci = NA_character_,
                    unit = sprintf(.("%s (cumulative)"), self$options$timeUnit),
                    interpretation = .("Total observation time across all patients")
                ),
                list(
                    name = .("Follow-up Density"),
                    value = if (isTRUE(stats$total_person_time > 0)) round(n_patients_summary / stats$total_person_time * 100, 3) else NA_real_,
                    ci = NA_character_,
                    unit = sprintf(.("per 100 %s"), self$options$timeUnit),
                    interpretation = .("Number of patients per 100 units of observation time (descriptive metric)")
                )
            )
            
            # Add response-specific metrics if available
            if (!is.null(stats$response_counts)) {
                response_counts <- stats$response_counts
                total_responses <- sum(response_counts)

                if (total_responses > 0) {
                    rates <- private$.responseRates(response_counts)

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
                    n_eval <- rates$n_evaluable

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
                            name = .("Objective Response Rate (ORR)"),
                            value = if (!is.na(orr)) round(orr, 1) else NA_real_,
                            ci = orr_ci,
                            unit = .("percent"),
                            interpretation = .("Proportion with complete or partial response")
                        ),
                        list(
                            name = .("Disease Control Rate (DCR)"),
                            value = if (!is.na(dcr)) round(dcr, 1) else NA_real_,
                            ci = dcr_ci,
                            unit = .("percent"),
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
                    confidence_interval = metric$ci,
                    metric_unit = metric$unit,
                    clinical_interpretation = metric$interpretation
                ))
            }
        },

        # Group comparison statistical tests (Fisher's exact for ORR/DCR)
        .updateGroupComparisonTests = function(patient_data, stats) {
            # Clear FIRST so a run without a group variable does not keep stale rows
            self$results$groupComparisonTest$deleteRows()

            if (is.null(self$options$groupVar)) return()

            # Need patient_summary with both response and group
            patient_summary <- stats$patient_summary
            if (is.null(patient_summary)) return()

            # Check if we have both response and group data
            if (!"response" %in% names(patient_summary) || !"patient_group" %in% names(patient_summary)) return()

            # Remove rows with missing response or group
            df <- patient_summary[!is.na(patient_summary$response) & !is.na(patient_summary$patient_group), ]
            if (nrow(df) == 0) return()

            # Normalize responses (should already be normalized, but ensure consistency)
            df$response <- sapply(df$response, private$.normalizeResponse, USE.NAMES = FALSE)

            # Get unique groups
            groups <- unique(df$patient_group)
            if (length(groups) < 2) {
                # No comparison possible with <2 groups
                return()
            }

            # Perform Fisher's exact test for ORR (CR + PR vs others)
            orr_contingency <- tryCatch({
                df$responder <- df$response %in% c("CR", "PR")
                table(df$patient_group, df$responder)
            }, error = function(e) NULL)

            if (!is.null(orr_contingency) && nrow(orr_contingency) >= 2 && ncol(orr_contingency) >= 2) {
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
                        test_statistic = if (!is.null(orr_test$estimate))
                            sprintf(.("Fisher's exact test, OR = %.2f"), orr_test$estimate)
                        else
                            .("Fisher's exact test"),
                        p_value = orr_test$p.value,
                        interpretation = orr_interpretation
                    ))
                }
            }

            # Perform Fisher's exact test for DCR (CR + PR + SD vs others)
            dcr_contingency <- tryCatch({
                df$disease_control <- df$response %in% c("CR", "PR", "SD")
                table(df$patient_group, df$disease_control)
            }, error = function(e) NULL)

            if (!is.null(dcr_contingency) && nrow(dcr_contingency) >= 2 && ncol(dcr_contingency) >= 2) {
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
                        test_statistic = if (!is.null(dcr_test$estimate))
                            sprintf(.("Fisher's exact test, OR = %.2f"), dcr_test$estimate)
                        else
                            .("Fisher's exact test"),
                        p_value = dcr_test$p.value,
                        interpretation = dcr_interpretation
                    ))
                }
            }

            if (self$results$groupComparisonTest$rowCount > 1) {
                self$results$groupComparisonTest$setNote("multiplicity",
                    .("Two Fisher's exact tests (ORR and DCR) are reported with unadjusted p-values; interpret them jointly rather than as independent evidence."))
            }

            # Check for low cell counts in contingency tables
            min_cell_orr <- if (!is.null(orr_contingency)) min(orr_contingency) else NA
            min_cell_dcr <- if (!is.null(dcr_contingency)) min(dcr_contingency) else NA
            min_cells <- c(min_cell_orr, min_cell_dcr)
            # min(c(NA, NA), na.rm = TRUE) is Inf plus an R warning.
            min_cell <- if (all(is.na(min_cells))) NA else min(min_cells, na.rm = TRUE)

            if (!is.na(min_cell) && min_cell < 5) {
                # REPLACED Notice with HTML to prevent serialization errors
                warning_html <- paste0(
                    "<div style='background-color: rgba(255, 202, 33, 0.23);border-left:4px solid #ffc107;padding:12px;margin:10px 0;font-family:Arial,sans-serif; color: inherit;'>",
                    "<strong style='color: inherit;'>", .("Warning:"), "</strong> ",
                    sprintf(
                        .("Fisher exact test has cells with counts below 5 (minimum cell count = %d). The test remains valid, but interpret p-values cautiously with small cell counts. Consider grouping categories or collecting more data."),
                        min_cell),
                    "</div>"
                )
                self$results$warningNotice$setContent(warning_html)
                self$results$warningNotice$setVisible(TRUE)
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
                            unit = self$options$timeUnit))
                    } else {
                        st_exp <- private$.asNumericTime(per_patient$start_time)
                        en_exp <- private$.asNumericTime(per_patient$end_time)
                    }

                    for (i in seq_len(nrow(per_patient))) {
                        tbl$addRow(rowKey = i, values = list(
                            patient_id = as.character(per_patient$patient_id[i]),
                            start_time = st_exp[i],
                            end_time   = en_exp[i],
                            duration   = per_patient$follow_up[i],
                            response   = if (has_response)
                                as.character(per_patient$response[i]) else ""
                        ))
                    }
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
                    if (self$options$exportTimeline) paste0("<li>", .("<strong>Timeline Data:</strong> Complete patient timeline dataset with processed variables"), "</li>") else "",
                    if (self$options$exportSummary) paste0("<li>", .("<strong>Summary Statistics:</strong> Comprehensive summary metrics and clinical indicators"), "</li>") else "",
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
                "<div style='margin: 10px 0;'>",
                "<h5 style='color: inherit;'>", .("Person-Time Analysis:"), "</h5>",
                "<p>", interpretation$person_time, "</p>",
                "</div>"
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
                p <- private$.createGgswimPlot(patient_data, milestone_data, event_data, arrow_data, opts, stats)
                
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
        
        .createGgswimPlot = function(patient_data, milestone_data, event_data, arrow_data, opts, stats) {
            # Check if ggswim is available
            if (!requireNamespace("ggswim", quietly = TRUE)) {
                return(private$.createFallbackPlot(patient_data, milestone_data, event_data, opts, stats,
                    error_message = .("the ggswim package is not installed")))
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

            # Apply color palette (colorblind-safe options)
            if (!is.null(self$options$colorPalette) && self$options$colorPalette != "default") {
                if (self$options$colorPalette == "viridis") {
                    # Viridis palette - perceptually uniform and colorblind-safe
                    p <- p + ggplot2::scale_color_viridis_d(option = "D", end = 0.9)
                    p <- p + ggplot2::scale_fill_viridis_d(option = "D", end = 0.9)
                } else if (self$options$colorPalette == "contrast") {
                    # High contrast palette (Okabe-Ito colorblind-safe palette)
                    contrast_colors <- c("#000000", "#E69F00", "#56B4E9", "#009E73",
                                        "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
                    p <- p + ggplot2::scale_color_manual(values = contrast_colors)
                    p <- p + ggplot2::scale_fill_manual(values = contrast_colors)
                } else if (self$options$colorPalette == "monochrome") {
                    # Monochrome with varying shades for grayscale publications
                    p <- p + ggplot2::scale_color_grey(start = 0.2, end = 0.8)
                    p <- p + ggplot2::scale_fill_grey(start = 0.2, end = 0.8)
                }
            }

            # Add labels with clinical context
            is_date_scale <- inherits(patient_data$start_time, c("Date", "POSIXct"))
            x_label <- if (is_date_scale) .("Date") else .fmt(.("Time ({unit})"), unit = self$options$timeUnit)
            p <- p + ggplot2::labs(
                title = .("Patient Timeline Analysis"),
                subtitle = sprintf(.("N=%d patients | Median duration: %.1f %s | Total person-time: %.1f %s"),
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
                        label = sprintf(.("Median: %s"), round(stats$median_duration, 1)),
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
            } else if (self$options$sortOrder == "patient_id") {
                ord <- order(patient_data$patient_id, method = "auto")
            } else if (self$options$sortOrder == "response" && "response" %in% names(patient_data)) {
                # Clinical hierarchy (CR > PR > SD > PD > NE), not alphabetical
                # factor order, which interleaved PD between PR and SD.
                rank <- match(
                    vapply(as.character(patient_data$response),
                           private$.normalizeResponse, character(1), USE.NAMES = FALSE),
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

            ordered_ids <- as.character(patient_data$patient_id[ord])
            patient_data$patient_id <- factor(as.character(patient_data$patient_id), levels = unique(ordered_ids))
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
            CIRCLE   <- "\u{25cf}"   # black circle
            SQUARE   <- "\u{25a0}"   # black square
            UP       <- "\u{25b2}"   # black up-pointing triangle
            DOWN     <- "\u{25bc}"   # black down-pointing triangle
            DIAMOND  <- "\u{25c6}"   # black diamond
            STAR     <- "\u{2605}"   # black star
            CROSS    <- "\u{271a}"   # heavy greek cross
            XMARK    <- "\u{2716}"   # heavy multiplication x
            HALF     <- "\u{25d1}"   # circle with right half black
            RING     <- "\u{25ce}"   # bullseye

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

        # Generate clinical glossary (static; body in R/swimmerplot_html.R)
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
            fu_label  <- switch(
                fu_method,
                reverse_km   = .("median follow-up (reverse Kaplan-Meier)"),
                unrecognised = .("median observed follow-up duration (censoring coding not recognised)"),
                .("median observed follow-up duration")
            )

            basic_text <- sprintf(
                .("Patient timelines were analyzed using swimmer plots to visualize treatment courses and clinical outcomes. The study included %d patients with a %s of %.1f %s; observed durations ranged from %.1f to %.1f %s. Total person-time was %.1f %s."),
                stats$n_patients,
                fu_label,
                fu_value,
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
                rates <- private$.responseRates(stats$response_counts)
                # RECIST-evaluable denominator throughout - the same one the
                # point estimates use, so the pasted sentence cannot quote a
                # rate and an interval computed over different cohorts.
                n_eval <- rates$n_evaluable
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
                        .("Response evaluation showed an objective response rate (ORR) of %.1f%% (%d/%d RECIST-evaluable patients%s) and a disease control rate (DCR) of %.1f%% (%d/%d RECIST-evaluable patients%s)."),
                        orr_pct, orr_count, n_eval, orr_ci_text,
                        dcr_pct, dcr_count, n_eval, dcr_ci_text)
                }
            }

            # Methodology note
            methods_text <- .("Timeline visualization was created using the ggswim package.")

            full_text <- paste(Filter(nzchar, c(basic_text, response_text, methods_text)), collapse = " ")

            copy_ready_html <- paste0(
                "<div style='background-color: rgba(33, 159, 33, 0.1); padding: 20px; border-left: 4px solid #28a745; border-radius: 8px; margin: 15px 0; font-family: system-ui, -apple-system, sans-serif; color: inherit;'>",
                "<h3 style='color: inherit; margin-top: 0; display: flex; align-items: center;'>",
                "<span style='margin-right: 8px;'></span>",
                .("Copy-Ready Manuscript Text"),
                "</h3>",
                "<div style='background-color: white; padding: 15px; border-radius: 6px; margin: 10px 0; box-shadow: 0 1px 3px rgba(0,0,0,0.1);'>",
                "<p style='margin: 0; line-height: 1.6; color: inherit; font-size: 0.95em; text-align: justify;'>", full_text, "</p>",
                "</div>",
                "<div style='margin-top: 15px; padding: 10px; background-color: rgba(33, 163, 188, 0.21); border-radius: 4px; border: 1px dashed #0c5460; color: inherit;'>",
                "<p style='margin: 0; font-size: 0.85em; color: inherit;'>",
                .("<strong>Usage:</strong> This text is formatted for direct use in manuscripts and clinical reports. Copy and paste into your document and adjust as needed for your specific requirements."),
                "</p>",
                "</div>",
                "</div>"
            )

            self$results$copyReadyReport$setContent(copy_ready_html)
        },

        # Generate about analysis information (static; body in R/swimmerplot_html.R)
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
