#' @title Survival Analysis for Continuous Explanatory Variable
#'
#' @description
#' Comprehensive survival analysis for continuous explanatory variables with optimal
#' cut-off determination, multiple cut-offs analysis, RMST analysis, residual diagnostics,
#' and advanced visualization options.
#'
#' @details
#' This function provides advanced survival analysis specifically designed for continuous
#' explanatory variables. It includes:
#'
#' **Core Features:**
#' - Optimal cut-off determination using maximally selected rank statistics
#' - Multiple cut-offs analysis with 4 different methods (quantile, recursive, tree-based, minimum p-value)
#' - Person-time analysis with interval stratification
#' - Date-based time calculation with multiple format support
#' - Multiple event level support (overall, cause-specific, competing risks)
#' - Landmark analysis for time-dependent effects
#'
#' **Advanced Analytics:**
#' - Restricted Mean Survival Time (RMST) analysis
#' - Cox model residual diagnostics (Martingale, Deviance, Score, Schoenfeld)
#' - Log-log plots for proportional hazards assessment
#' - Enhanced error handling and data validation
#'
#' **Visualization Options:**
#' - Kaplan-Meier survival curves with optimal cut-offs
#' - Multiple cut-offs histogram with cut-point annotations
#' - Cumulative events and hazard plots
#' - KMunicate-style plots for publication
#' - Residual diagnostic plots (4-panel layout)
#' - Log-log plots for assumption checking
#'
#' @examples
#' \dontrun{
#' # Basic survival analysis with optimal cut-off
#' data("lung", package = "survival")
#' lung$status_binary <- ifelse(lung$status == 2, 1, 0)
#'
#' result1 <- survivalcont(
#'   data = lung,
#'   elapsedtime = "time",
#'   outcome = "status_binary",
#'   outcomeLevel = NULL, dod = NULL, dooc = NULL, awd = NULL, awod = NULL,
#'   contexpl = "age",
#'   findcut = TRUE,
#'   sc = TRUE
#' )
#'
#' # Multiple cut-offs analysis with different methods
#' result2 <- survivalcont(
#'   data = lung,
#'   elapsedtime = "time",
#'   outcome = "status_binary",
#'   outcomeLevel = NULL, dod = NULL, dooc = NULL, awd = NULL, awod = NULL,
#'   contexpl = "ph.karno",
#'   multiple_cutoffs = TRUE,
#'   num_cutoffs = "three",
#'   cutoff_method = "recursive",
#'   min_group_size = 15,
#'   sc = TRUE
#' )
#'
#' # RMST analysis with residual diagnostics
#' result3 <- survivalcont(
#'   data = lung,
#'   elapsedtime = "time",
#'   outcome = "status_binary",
#'   outcomeLevel = NULL, dod = NULL, dooc = NULL, awd = NULL, awod = NULL,
#'   contexpl = "wt.loss",
#'   findcut = TRUE,
#'   rmst_analysis = TRUE,
#'   rmst_tau = 500,
#'   residual_diagnostics = TRUE,
#'   loglog = TRUE
#' )
#'
#' # Person-time analysis with date calculation
#' # Create sample data with dates
#' set.seed(123)
#' n <- 200
#' sample_data <- data.frame(
#'   biomarker = rnorm(n, 100, 25),
#'   event = rbinom(n, 1, 0.6),
#'   dx_date = as.Date("2020-01-01") + sample(0:365, n, replace = TRUE),
#'   fu_date = as.Date("2020-01-01") + sample(366:1095, n, replace = TRUE)
#' )
#'
#' result4 <- survivalcont(
#'   data = sample_data,
#'   tint = TRUE,
#'   dxdate = "dx_date",
#'   fudate = "fu_date",
#'   timetypedata = "ymd",
#'   timetypeoutput = "months",
#'   outcome = "event",
#'   outcomeLevel = NULL, dod = NULL, dooc = NULL, awd = NULL, awod = NULL,
#'   contexpl = "biomarker",
#'   person_time = TRUE,
#'   time_intervals = "6, 12, 24",
#'   rate_multiplier = 1000
#' )
#'
#' # Comprehensive analysis with all features
#' result5 <- survivalcont(
#'   data = lung,
#'   elapsedtime = "time",
#'   outcome = "status_binary",
#'   outcomeLevel = NULL, dod = NULL, dooc = NULL, awd = NULL, awod = NULL,
#'   contexpl = "meal.cal",
#'   findcut = TRUE,
#'   multiple_cutoffs = TRUE,
#'   num_cutoffs = "two",
#'   cutoff_method = "quantile",
#'   rmst_analysis = TRUE,
#'   rmst_tau = 400,
#'   residual_diagnostics = TRUE,
#'   person_time = TRUE,
#'   time_intervals = "100, 300, 500",
#'   sc = TRUE,
#'   ce = TRUE,
#'   ch = TRUE,
#'   kmunicate = TRUE,
#'   loglog = TRUE,
#'   ci95 = TRUE,
#'   risktable = TRUE
#' )
#' }
#'
#' @references
#' Hothorn, T., & Zeileis, A. (2008). Generalized maximally selected statistics.
#' Biometrics, 64(4), 1263-1269.
#'
#' Royston, P., & Parmar, M. K. (2013). Restricted mean survival time: an alternative
#' to the hazard ratio for the design and analysis of randomized trials with a
#' time-to-event outcome. BMC Medical Research Methodology, 13(1), 152.
#'
#' Morris, T. P., et al. (2019). Proposals on Kaplan-Meier plots in medical research
#' and a survey of stakeholder views: KMunicate. BMJ Open, 9(9), e030874.
#'
#' @importFrom R6 R6Class
#' @return An \code{R6} class generator object for the \code{survivalcontClass} backend; used internally by the jamovi analysis wrapper and not called directly.

survivalcontClass <- if (requireNamespace("jmvcore")) {
    R6::R6Class(
        "survivalcontClass",
        inherit = survivalcontBase,
        private = list(
            # Private fields for storing analysis results
            residuals_data = NULL,
            # Result of .defineEventIndicator(), kept so .run() can render the
            # recode disclosure without redoing the work.
            .eventRecode = NULL,
            # TRUE below 10 events: descriptive output runs, cut-offs suppressed.
            .lowEventCount = FALSE,
            # What actually ran this cycle, as opposed to what the user ticked.
            .cutoffRan = FALSE,
            .multicutRan = FALSE,
            # Why .multipleCutoffs() returned NULL this cycle; .run() shows it.
            .multicutFailReason = NULL,

            # Competing Risk Helper Functions ----
            .isCompetingRisk = function(state = NULL) {
                # Check if competing risk analysis is active.
                #
                # The STATUS VECTOR decides this, not the options. This used to
                # read `self$options$multievent && analysistype == "compete"`
                # alone, which is blind to the outcomeorganizer hand-off: a
                # recoded Censored/Event/Competing column arrives already 0/1/2
                # with multievent = FALSE -- the user never fills
                # dod/dooc/awd/awod, that is the whole point of the recoded
                # column. The guard was therefore FALSE and the 0/1/2 vector
                # went into an ordinary survival::Surv(), which for a max status
                # of 2 subtracts 1 and NAs anything outside 0/1: Censored became
                # NA (row silently DELETED), Event became censored, and
                # Competing became the event. If this ever reverts to testing
                # the options alone, competing-risk data is analysed backwards
                # again with no warning.
                #
                # `state` is a plot's image$state. jmvcore's .load() restores
                # results from disk without calling .run(), so a renderer can
                # execute where private$.eventRecode is still NULL; the flag
                # then has to come off the serialised state.
                isTRUE(state$has_competing) ||
                    isTRUE(private$.eventRecode$has_competing) ||
                    (isTRUE(self$options$multievent) &&
                         identical(self$options$analysistype, "compete"))
            },

            .getDefaultCutpoints = function() {
                # Return time-unit aware default cutpoints (like singlearm)
                time_unit <- self$options$timetypeoutput
                switch(time_unit,
                    "days" = c(365, 1095, 1825),      # 1, 3, 5 years in days
                    "weeks" = c(52, 156, 260),        # 1, 3, 5 years in weeks
                    "months" = c(12, 36, 60),         # 1, 3, 5 years in months
                    "years" = c(1, 3, 5),             # 1, 3, 5 years
                    c(12, 36, 60)  # default to months if unknown
                )
            },

            # After a landmark, every reported time is measured FROM the landmark,
            # but the numbers carry no marker saying so -- a "median survival of 18
            # months" is really 18 months beyond the landmark. Stamp the tables that
            # print times on that scale.
            # x-axis label, including the landmark caveat when one is in force.
            .timeAxisLabel = function() {
                base <- paste0("Time (", self$options$timetypeoutput, ")")
                if (isTRUE(self$options$uselandmark) &&
                    is.finite(jmvcore::toNumeric(self$options$landmark)) &&
                    jmvcore::toNumeric(self$options$landmark) > 0)
                    paste0(base, " from landmark")
                else base
            },

            .landmarkNote = function(tbl, results) {
                offset <- results$landmark_offset
                if (is.null(offset) || !is.finite(offset) || offset <= 0) return(invisible(NULL))
                tbl$setNote("landmark", jmvcore::format(.("Times are measured from the landmark at {offset} {unit}, not from diagnosis; add {offset} to express a time as time from diagnosis."), offset = round(offset, 2), unit = self$options$timetypeoutput))
                invisible(NULL)
            },

            # One year expressed in the selected output time scale. Single source
            # of truth for the factory-value rescaling in .plotEndTime()/.plotBy().
            .oneYear = function() {
                switch(self$options$timetypeoutput,
                    "days" = 365, "weeks" = 52, "months" = 12, "years" = 1, 12)
            },

            # Plot x-axis extent, unit-aware.
            #
            # `endplot` is an Integer option, so its default can only be a single
            # static number -- 60, which is five years ONLY in months. On a
            # day-scale study every survival curve was clipped at 60 days; on a
            # year-scale study the axis ran to 60 years. Treat the untouched
            # factory value the same way .parseSurvivalTimePoints() already treats
            # the factory "12, 36, 60" for cutp: as a request for the unit-aware
            # 5-year point. Any other value is the user's own and is used as-is.
            .plotEndTime = function() {
                endplot <- jmvcore::toNumeric(self$options$endplot)
                if (!is.finite(endplot) || endplot <= 0) return(5 * private$.oneYear())
                if (identical(as.numeric(endplot), 60)) return(5 * private$.oneYear())
                endplot
            },

            # Axis tick spacing, same factory-value rule (12 = one year in months).
            .plotBy = function() {
                byplot <- jmvcore::toNumeric(self$options$byplot)
                if (!is.finite(byplot) || byplot <= 0) return(private$.oneYear())
                if (identical(as.numeric(byplot), 12)) return(private$.oneYear())
                byplot
            },

            # Backtick-quote non-syntactic names for formula strings (jmvcore does
            # the quoting and escapes embedded backticks/backslashes).
            .escapeVariableNames = function(var_names) {
                vapply(var_names, jmvcore::composeTerm, character(1), USE.NAMES = FALSE)
            },

            # Map outcome to event-of-interest indicator respecting analysis type
            .eventOfInterestIndicator = function(outcome_vec) {
                if (!self$options$multievent) {
                    return(outcome_vec)
                }

                analysistype <- self$options$analysistype
                if (analysistype == "compete") {
                    # Cause-specific handling for event of interest; competing events censored
                    return(ifelse(outcome_vec == 1, 1,
                                  ifelse(is.na(outcome_vec), NA, 0)))
                }

                # overall and cause mappings are already 0/1
                return(outcome_vec)
            },

            # Helper function to create clinical tooltips and explanations
            .createClinicalTooltip = function(term, definition, example = NULL) {
                example_text <- if (!is.null(example)) {
                    paste0('<p style="margin: 8px 0 0 0; font-style: italic;"><strong>Example:</strong> ', example, '</p>')
                } else {
                    ""
                }

                tooltip_html <- glue::glue(
                    '<div style="background-color: rgba(33, 152, 239, 0.13); padding: 12px; border-radius: 6px; margin: 10px 0; border-left: 4px solid #1976d2; color: inherit;">
                        <h4 style="margin: 0 0 8px 0; color: inherit;">{term}</h4>
                        <p style="margin: 0;">{definition}</p>
                        {example_text}
                    </div>'
                )
                return(tooltip_html)
            },

            # Helper function to create clinical interpretation boxes
            .createInterpretationBox = function(title, content, warning = NULL) {
                # Every caller passes `warning = TRUE` as a FLAG ("style this as a
                # warning"), not as text -- so the box rendered a literal "TRUE"
                # under the content. Only interpolate `warning` when it is actual
                # message text.
                warning_html <- if (is.null(warning) || is.logical(warning)) {
                    ""
                } else {
                    glue::glue('<div class="warning-box"> {warning}</div>')
                }

                interpretation_html <- glue::glue(
                    '<div class="interpretation-box">
                    <h4 class="interpretation-title"> {title}</h4>
                    <div class="interpretation-content">{content}</div>
                    {warning_html}
                    </div>'
                )
                return(interpretation_html)
            },

            # Helper function to generate copy-ready clinical sentences
            .generateClinicalSentence = function(analysis_type, variable_name, result_values) {
                switch(analysis_type,
                    "cox_regression" = {
                        # `hr` arrives from finalfit pre-formatted, e.g.
                        # "0.30 (0.24-0.38, p<0.001)". as.numeric() on that returns
                        # NA *with a coercion warning*, and the warning handler
                        # below turned it into "N/A" -- so the sentence read
                        # "could not determine hazard ratio" for every perfectly
                        # valid model. Prefer a raw numeric when the caller
                        # supplies one (hr_numeric), otherwise pull the leading
                        # number out of the formatted string.
                        val <- result_values$hr_numeric
                        if (is.null(val) || length(val) == 0)
                            val <- result_values$hr

                        # `hr` can be a vector when a group has several rows; `||`
                        # on a vector errors under R >= 4.3, so reduce first.
                        if (length(val) > 1) val <- val[1]

                        if (is.null(val) || length(val) == 0 || all(is.na(val)) ||
                            identical(as.character(val), "-")) {
                            hr_val <- "N/A"
                        } else if (is.numeric(val)) {
                            hr_val <- round(val, 2)
                        } else {
                            # Leading number of a formatted "est (lo-hi, p=...)" string.
                            num <- suppressWarnings(
                                as.numeric(sub("^\\s*([0-9.eE+-]+).*$", "\\1", as.character(val))))
                            hr_val <- if (is.na(num)) "N/A" else round(num, 2)
                        }

                        if (identical(hr_val, "N/A")) {
                            glue::glue(.('Analysis of {variable} could not determine hazard ratio.'), variable = variable_name)
                        } else {
                            glue::glue(.('When {variable} increases by 1 unit, the instantaneous hazard rate of the event is multiplied by {hr} (a hazard ratio, not a risk ratio).'),
                                     variable = variable_name, hr = hr_val)
                        }
                    },

                    "median_survival" = {
                        median_val <- ifelse(is.null(result_values$median), "N/A", round(result_values$median, 1))
                        time_unit <- ifelse(is.null(result_values$time_unit), "time units", result_values$time_unit)
                        group_name <- ifelse(is.null(result_values$group), "this group", result_values$group)

                        if (median_val == "N/A") {
                            glue::glue(.('Median survival time for {group} could not be determined.'), group = group_name)
                        } else {
                            glue::glue(.('The median survival time for patients with {group} was {median} {time_unit}.'),
                                     group = group_name, median = median_val, time_unit = time_unit)
                        }
                    },

                    "cutoff_analysis" = {
                        cutoff_val <- ifelse(is.null(result_values$cutoff), "N/A", round(as.numeric(result_values$cutoff), 2))
                        if (cutoff_val == "N/A") {
                            glue::glue(.('Optimal cut-off point for {variable} could not be determined.'), variable = variable_name)
                        } else {
                            glue::glue(.('The data-derived cut-off point for {variable} is {cutoff}; it separates lower and higher marker values in this dataset.'),
                                     variable = variable_name, cutoff = cutoff_val)
                        }
                    },

                    # Default case
                    glue::glue(.('Analysis completed for {variable}.'), variable = variable_name)
                )
            },

            # Error recovery wrapper for safe analysis execution
            .safeAnalysis = function(analysis_function, context = "", fallback_value = NULL) {
                reported_warnings <- character()
                tryCatch({
                    withCallingHandlers(
                        analysis_function(),
                        warning = function(w) {
                            warning_message <- conditionMessage(w)
                            if (!warning_message %in% reported_warnings) {
                                reported_warnings <<- c(reported_warnings, warning_message)
                                warning_title <- if (nzchar(context)) {
                                    paste(context, .("warning"))
                                } else {
                                    .("Analysis warning")
                                }
                                private$.addHtmlMessage(
                                    "warning",
                                    warning_title,
                                    warning_message
                                )
                            }
                            invokeRestart("muffleWarning")
                        }
                    )
                }, error = function(e) {
                    # Analysis failed - return fallback value silently
                    # Error details are already handled by specific error notices
                    return(fallback_value)
                })
            },

            # Clinical assumption checking
            .checkClinicalAssumptions = function(data, time_var, outcome_var, contexpl_var = NULL) {
                warnings <- list()

                # Sample size checks
                n <- nrow(data)
                if (n < 30) {
                    warnings <- append(warnings, glue::glue(
                        .('Very small sample size (n = {n}). Results may be unreliable. Recommend n \u2265 50 for stable cut-off analysis.'),
                        n = n
                    ))
                } else if (n < 50) {
                    warnings <- append(warnings, glue::glue(
                        .('Small sample size (n = {n}). Consider larger sample for more reliable cut-off analysis.'),
                        n = n
                    ))
                }

                # Event rate checks
                events <- sum(data[[outcome_var]], na.rm = TRUE)
                event_rate <- events / n
                if (event_rate < 0.1) {
                    warnings <- append(warnings, glue::glue(
                        .('Low event rate ({rate}%). May need larger sample or longer follow-up for reliable survival analysis.'),
                        rate = round(event_rate * 100, 1)
                    ))
                } else if (event_rate > 0.9) {
                    warnings <- append(warnings, glue::glue(
                        .('Very high event rate ({rate}%). Consider competing risks or cause-specific analysis.'),
                        rate = round(event_rate * 100, 1)
                    ))
                }

                # Follow-up time checks
                #
                # The threshold has to follow the selected output time scale. It was
                # hard-coded at 6, which is "6 months" only by coincidence: with the
                # scale set to years a perfectly ordinary 2.8-year median follow-up
                # was reported as "may be insufficient for meaningful survival
                # analysis", and with days a 100-day median was never flagged at all.
                # 6 months is the intended clinical threshold; express it in the
                # active unit.
                # The quantity graded here must be the FOLLOW-UP, by reverse
                # Kaplan-Meier (Schemper & Smith 1996). It was median(time),
                # which is the median time to event-or-censoring: in a
                # high-event cohort that sits far below the true observation
                # window, so this warning fired on cohorts whose follow-up was
                # perfectly adequate. See .medianFollowUp() in
                # R/survival_utils.R.
                mfu <- .medianFollowUp(data[[time_var]], data[[outcome_var]] == 0)
                median_time <- mfu$value
                short_followup <- switch(self$options$timetypeoutput,
                    "days"   = 183,
                    "weeks"  = 26,
                    "months" = 6,
                    "years"  = 0.5,
                    6)
                if (!is.na(median_time) && median_time < short_followup) {
                    warnings <- append(warnings, if (isTRUE(mfu$reverse)) glue::glue(
                        .('Short median follow-up ({time} {units}, reverse Kaplan-Meier). May be insufficient for meaningful survival analysis.'),
                        time = round(median_time, 1),
                        units = self$options$timetypeoutput
                    ) else glue::glue(
                        .('Short median observed time ({time} {units}). Median follow-up could not be estimated by reverse Kaplan-Meier, so this is the median time to event-or-censoring, which understates how long the cohort was observed. Interpret the adequacy of follow-up with that in mind.'),
                        time = round(median_time, 1),
                        units = self$options$timetypeoutput
                    ))
                }

                # Continuous variable distribution checks
                if (!is.null(contexpl_var) && contexpl_var %in% names(data)) {
                    cont_var <- data[[contexpl_var]]
                    if (length(unique(cont_var[!is.na(cont_var)])) < 10) {
                        warnings <- append(warnings, .('Limited variability in continuous explanatory variable. Consider treating as categorical.'))
                    }
                }

                return(warnings)
            },

            # Helper function to restore original variable names in output tables
            .restoreOriginalNamesInTable = function(table_data, all_labels) {
                if (is.null(table_data) || nrow(table_data) == 0) return(table_data)

                # Create a mapping from cleaned names to original names
                name_mapping <- setNames(unlist(all_labels), names(all_labels))

                # Restore names in the first column (which typically contains variable names)
                if (ncol(table_data) > 0) {
                    first_col <- table_data[[1]]

                    # Process each row in the first column
                    for (i in seq_along(first_col)) {
                        current_name <- first_col[i]

                        # Skip if it's not a string or is empty
                        if (is.na(current_name) || current_name == "" || !is.character(current_name)) next

                        # Check if this name exists in our mapping and replace it
                        if (current_name %in% names(name_mapping)) {
                            table_data[i, 1] <- name_mapping[current_name]
                        }

                        # Also handle factor level names (e.g., "variable_name=level")
                        for (clean_name in names(name_mapping)) {
                            original_name <- name_mapping[clean_name]
                            pattern <- paste0("^", clean_name, "([=:].*)?$")
                            if (grepl(pattern, current_name)) {
                                table_data[i, 1] <- gsub(clean_name, original_name, current_name)
                                break
                            }
                        }
                    }
                }

                return(table_data)
            },

            .detectCommonMisuses = function() {
                warnings <- c()

                # Detect inappropriate cut-off hunting
                # (num_cutoffs is capped at "four" in the UI, so the historical
                # "many cut-offs (>5)" warning branch could never fire; removed.)

                # Detect potential data snooping
                if (self$options$findcut && self$options$multiple_cutoffs) {
                    warnings <- c(warnings, .(
                        "Using both optimal cut-off and multiple cut-offs may lead to overfitting. Consider validation in independent dataset or cross-validation."
                    ))
                }

                # Detect inappropriate landmark analysis
                if (self$options$uselandmark && !is.null(self$options$landmark)) {
                    landmark_time <- as.numeric(self$options$landmark)
                    if (landmark_time > 0 && nrow(self$data) > 0) {
                        # Check if landmark time removes too many patients
                        time_var <- self$options$elapsedtime
                        if (!is.null(time_var) && time_var %in% names(self$data)) {
                            time_data <- self$data[[time_var]]
                            patients_excluded <- sum(time_data < landmark_time, na.rm = TRUE)
                            exclusion_rate <- patients_excluded / length(time_data)
                            if (exclusion_rate > 0.3) {
                                warnings <- c(warnings, .(
                                    "Landmark analysis excludes {rate}% of patients. High exclusion rates may introduce bias."
                                ) %>% glue::glue(rate = round(exclusion_rate * 100, 1)))
                            }
                        }
                    }
                }

                return(warnings)
            },

            # init ----
            .init = function() {
                # Initialize HTML message outputs
                private$.initializeMessageOutputs()
                # Result visibility is declared once in survivalcont.r.yaml
                # (`visible:`); it is not mirrored here.
            }

            # getData ----
            ,
            .getData = function() {
                # The cleaned <-> original name mapping below is one-to-one:
                # janitor::clean_names() de-duplicates collisions with numeric
                # suffixes ("Age (yrs)", "Age yrs", "age.yrs" -> age_yrs,
                # age_yrs_2, age_yrs_3), and data.frame names are already unique,
                # so `all_labels == <original>` always matches exactly one column.
                mydata <- self$data

                mydata$row_names <- rownames(mydata)

                original_names <- names(mydata)

                labels <- setNames(original_names, original_names)

                mydata <- mydata %>% janitor::clean_names()

                corrected_labels <-
                    setNames(original_names, names(mydata))

                mydata <- labelled::set_variable_labels(
                    .data = mydata,
                    .labels = corrected_labels
                )

                all_labels <- labelled::var_label(mydata)


                mytime <-
                    names(all_labels)[all_labels == self$options$elapsedtime]

                myoutcome <-
                    names(all_labels)[all_labels == self$options$outcome]

                mydxdate <-
                    names(all_labels)[all_labels == self$options$dxdate]

                myfudate <-
                    names(all_labels)[all_labels == self$options$fudate]

                mycontexpl <-
                    names(all_labels)[all_labels == self$options$contexpl]

                return(list(
                    "mydata_labelled" = mydata,
                    "mytime_labelled" = mytime,
                    "myoutcome_labelled" = myoutcome,
                    "mydxdate_labelled" = mydxdate,
                    "myfudate_labelled" = myfudate,
                    "mycontexpl_labelled" = mycontexpl,
                    "all_labels" = all_labels
                ))
            }

            # HTML Message Helper Methods ----
            ,
            .initializeMessageOutputs = function() {
                # Initialize all HTML message outputs as empty and invisible
                self$results$errors$setContent("")
                self$results$errors$setVisible(FALSE)

                self$results$strongWarnings$setContent("")
                self$results$strongWarnings$setVisible(FALSE)

                self$results$warnings$setContent("")
                self$results$warnings$setVisible(FALSE)

                self$results$infoMessages$setContent("")
                self$results$infoMessages$setVisible(FALSE)
            }

            ,
            .addHtmlMessage = function(type, title, message) {
                # Add a message to the appropriate HTML output
                # type: "error", "strongWarning", "warning", "info"

                # Determine which output to use and CSS class
                output_name <- switch(type,
                    "error" = "errors",
                    "strongWarning" = "strongWarnings",
                    "warning" = "warnings",
                    "info" = "infoMessages",
                    "warnings"  # default
                )

                css_class <- switch(type,
                    "error" = "error-message",
                    "strongWarning" = "strong-warning-message",
                    "warning" = "warning-message",
                    "info" = "info-message",
                    "warning-message"  # default
                )

                # Get current content
                current_content <- self$results[[output_name]]$content
                if (is.null(current_content)) {
                    current_content <- ""
                }

                # Create HTML for the new message
                new_message <- sprintf(
                    '<div class="%s" style="margin: 10px 0; padding: 10px; border-left: 4px solid; background-color: rgba(138, 155, 172, 0.06); color: inherit;">
                        <strong>%s:</strong> %s
                    </div>',
                    css_class,
                    htmltools::htmlEscape(title),
                    htmltools::htmlEscape(message)
                )

                # Append to current content
                updated_content <- paste0(current_content, new_message)

                # Update the output
                self$results[[output_name]]$setContent(updated_content)
                self$results[[output_name]]$setVisible(TRUE)
            }

            # todo ----
            ,
            .todo = function() {
                todo <- glue::glue('
                <div style="padding: 15px; background-color: rgba(138, 155, 172, 0.06); border-left: 4px solid #007bff; color: inherit;">
                    <h3 style="margin-top: 0; color: inherit;">Welcome to ClinicoPath - Survival Analysis for Continuous Variables</h3>

                    <p><strong>Purpose:</strong> This tool helps you calculate an optimal cut-off for a continuous variable based on survival outcomes.</p>

                    <h4>Analysis Features:</h4>
                    <ul>
                        <li>Automatic cut-off determination using maximally selected rank statistics</li>
                        <li>Median survival calculations</li>
                        <li>1, 3, 5-year survival estimates</li>
                        <li>Cox regression for continuous variables</li>
                        <li>Kaplan-Meier survival curves</li>
                    </ul>

                    <h4>Variable Requirements:</h4>
                    <ul>
                        <li><strong>Explanatory Variable:</strong> Must be continuous (numeric)</li>
                        <li><strong>Outcome Variable:</strong> Select the level representing death or event occurrence
                            <ul>
                                <li>Use basic outcome level for simple analysis</li>
                                <li>Use advanced outcome options (Dead of Disease, Dead of Other, etc.) for cause-specific or competing risk analysis</li>
                            </ul>
                        </li>
                        <li><strong>Survival Time:</strong> Should be numeric and continuous
                            <ul>
                                <li>Enter directly, OR</li>
                                <li>Calculate from diagnosis/follow-up dates using advanced time options</li>
                            </ul>
                        </li>
                    </ul>

                    <h4>References:</h4>
                    <p>This function uses <code>survival</code>, <code>survminer</code>, and <code>finalfit</code> packages. Please cite jamovi and these packages in your publications.</p>

                    <p><a href="https://cran.r-project.org/web/packages/survival/vignettes/survival.pdf" target="_blank">See survival package documentation here</a></p>
                </div>
                ')

                html <- self$results$todo
                html$setContent(todo)
            }




            # Define Survival Time ----
            ,
            .definemytime = function() {

            ## Read Labelled Data ----

            labelled_data <- private$.getData()

            mydata <- labelled_data$mydata_labelled
            mytime_labelled <- labelled_data$mytime_labelled
            mydxdate_labelled <- labelled_data$mydxdate_labelled
            myfudate_labelled <- labelled_data$myfudate_labelled

            tint <- self$options$tint


                if (!tint) {
                    ### Precalculated Time ----

                    mydata[["mytime"]] <-
                        jmvcore::toNumeric(mydata[[mytime_labelled]])


                } else if (tint) {
                    ### Time Interval ----

                    dxdate <- mydxdate_labelled
                    fudate <- myfudate_labelled
                    timetypedata <- self$options$timetypedata


                    # Check if input is numeric (Unix epoch) or text (requires parsing)
                    is_numeric_dx <- is.numeric(mydata[[dxdate]])
                    is_numeric_fu <- is.numeric(mydata[[fudate]])

                    if (is_numeric_dx && is_numeric_fu) {
                        # Handle numeric Unix epoch input (from DateTime Converter)
                        #
                        # A numeric date column is interpreted as epoch SECONDS.
                        # An R Date coerced to numeric, or an Excel serial date, is
                        # a DAY count -- 86400x smaller. Feeding one in produced
                        # survival times of a few thousandths of a unit: no median
                        # table at all, a singular Cox fit, and not one message
                        # saying why. Day-count magnitudes cannot be told apart
                        # from seconds by type, only by size: as epoch seconds,
                        # anything under ~1e6 is January 1970, which no clinical
                        # follow-up date is. Reject rather than silently divide the
                        # study duration by 86400.
                        epoch_vals <- c(mydata[[dxdate]], mydata[[fudate]])
                        epoch_vals <- epoch_vals[is.finite(epoch_vals) & epoch_vals != 0]
                        if (length(epoch_vals) > 0 && max(abs(epoch_vals)) < 1e6) {
                            private$.addHtmlMessage(
                                "error",
                                .("Numeric dates are not epoch seconds"),
                                jmvcore::format(.("The numeric date columns hold values too small to be Unix epoch seconds (the largest is {max_val}, which is January 1970). They look like day counts, as produced by an R Date column or an Excel serial date. Numeric dates must be seconds since 1970-01-01 -- use the DateTime Converter's corrected_datetime_numeric output, or supply the dates as text (e.g. \"2024-01-15\") and set the matching input time type."), max_val = base::format(max(abs(epoch_vals)), big.mark = ",")))
                            return(NULL)
                        }
                        mydata[["start"]] <- as.POSIXct(mydata[[dxdate]], origin="1970-01-01", tz="UTC")
                        mydata[["end"]] <- as.POSIXct(mydata[[fudate]], origin="1970-01-01", tz="UTC")
                    } else if (!is_numeric_dx && !is_numeric_fu) {
                        # Handle text datetime input via lubridate
                        lubridate_functions <- list(
                            ymdhms = lubridate::ymd_hms,
                            ymd = lubridate::ymd,
                            ydm = lubridate::ydm,
                            mdy = lubridate::mdy,
                            myd = lubridate::myd,
                            dmy = lubridate::dmy,
                            dym = lubridate::dym
                        )

                        if (timetypedata %in% names(lubridate_functions)) {
                            func <- lubridate_functions[[timetypedata]]
                            tryCatch({
                                mydata[["start"]] <- suppressWarnings(func(mydata[[dxdate]]))
                                mydata[["end"]] <- suppressWarnings(func(mydata[[fudate]]))
                            }, error = function(e) {
                                return(NULL)
                            })

                            # lubridate reports a format mismatch as a WARNING
                            # ("19 failed to parse."), not an error, so the tryCatch
                            # above never fired and only the all-NA case was treated
                            # as failure. Rows that failed to parse were then removed
                            # by naOmit and reported as "excluded because ... was
                            # missing" -- blaming the data instead of the wrong
                            # 'Input time type'. Worse, on a dd/mm vs mm/dd mismatch
                            # the rows that DO parse have day and month transposed.
                            n_fail <- sum(is.na(mydata[["start"]]) & !is.na(mydata[[dxdate]])) +
                                      sum(is.na(mydata[["end"]]) & !is.na(mydata[[fudate]]))
                            n_dates <- sum(!is.na(mydata[[dxdate]])) + sum(!is.na(mydata[[fudate]]))
                            if (n_fail > 0) {
                                private$.addHtmlMessage(
                                    "strongWarning",
                                    .("Dates could not be read"),
                                    sprintf(
                                        .("%d of %d date value(s) did not match the selected input time type '%s'. Those records are dropped. Values that DO match an ambiguous format (for example 03/04/2024 read as %s) are converted using it, so check 'Input time type' before interpreting these results."),
                                        n_fail, n_dates, timetypedata, timetypedata))
                                if (n_dates > 0 && n_fail / n_dates > 0.2) {
                                    private$.addHtmlMessage(
                                        "error",
                                        .("Date format does not match the data"),
                                        sprintf(
                                            .("%.0f%% of date values could not be read as '%s'. This is a format mismatch rather than missing data. Set 'Input time type' to the format actually used in your columns."),
                                            100 * n_fail / n_dates, timetypedata))
                                    return(NULL)
                                }
                            }
                        } else {
                            return(NULL)
                        }
                    } else {
                        # Mixed types error
                        return(NULL)
                    }


                    if ( sum(!is.na(mydata[["start"]])) == 0 || sum(!is.na(mydata[["end"]])) == 0)  {
                        return(NULL)
                    }

                    timetypeoutput <-
                        jmvcore::constructFormula(terms = self$options$timetypeoutput)


                    mydata <- mydata %>%
                        dplyr::mutate(interval = lubridate::interval(start, end))


                    mydata <- mydata %>%
                        dplyr::mutate(mytime = lubridate::time_length(interval,
                                                                      timetypeoutput))

                }


                df_time <- mydata %>% jmvcore::select(c("row_names", "mytime"))

                return(df_time)


            }

            # Define Outcome ----
            ,
            .definemyoutcome = function() {


            labelled_data <- private$.getData()

            mydata <- labelled_data$mydata_labelled
            myoutcome_labelled <- labelled_data$myoutcome_labelled


                # Delegated to the shared coder in survival_utils.R. This block
                # previously had no null-check on outcomeLevel (it crashed with a
                # raw "replacement has 0 rows" error) and returned NULL silently
                # on a bad numeric outcome, leaving the user with a blank
                # analysis and no explanation.
                res <- .defineEventIndicator(
                    outcome      = mydata[[myoutcome_labelled]],
                    outcomeLevel = self$options$outcomeLevel,
                    multievent   = self$options$multievent,
                    analysistype = self$options$analysistype,
                    dod          = self$options$dod,
                    dooc         = self$options$dooc,
                    awd          = self$options$awd,
                    awod         = self$options$awod,
                    outcome_name = self$options$outcome
                )

                if (!is.null(res$error)) {
                    private$.addHtmlMessage("error", .("Outcome variable problem"), res$error)
                    return(NULL)
                }

                private$.eventRecode <- res
                mydata[["myoutcome"]] <- res$status

                df_outcome <- mydata %>% jmvcore::select(c("row_names", "myoutcome"))

                return(df_outcome)

            }


            # Define Factor ----
            ,

            .definemyfactor = function() {


            labelled_data <- private$.getData()

            mydata_labelled <- labelled_data$mydata_labelled
            mycontexpl_labelled <- labelled_data$mycontexpl_labelled

            mydata <- mydata_labelled

            mydata[["myfactor"]] <- mydata[[mycontexpl_labelled]]


            df_factor <- mydata %>% jmvcore::select(c("row_names","myfactor"))

            return(df_factor)

            }

            # Validation Methods ----
            ,
            .validateAnalysisRequirements = function() {
                # Basic variable checks
                has_outcome <- !is.null(self$options$outcome)

                # If multi-event analysis is enabled, check for required event levels
                if (self$options$multievent) {
                    has_required_events <- !is.null(self$options$dod) || !is.null(self$options$dooc)
                    outcome_valid <- has_outcome && has_required_events
                } else {
                    outcome_valid <- has_outcome
                }

                # Check time variable requirements
                if (self$options$tint) {
                    # Time calculation from dates
                    time_valid <- !is.null(self$options$dxdate) && !is.null(self$options$fudate)
                } else {
                    # Direct elapsed time
                    time_valid <- !is.null(self$options$elapsedtime)
                }

                # Check continuous explanatory variable
                contexpl_valid <- !is.null(self$options$contexpl)

                basic_requirements <- outcome_valid && time_valid && contexpl_valid

                # Misuse Detection Guards
                #
                # These used to be written into the `todo` panel. `todo` is the
                # "you have not selected your variables yet" placeholder, and
                # .run() hides it (setVisible(FALSE)) on the very next line
                # whenever the requirements ARE met -- which is the only situation
                # in which a misuse warning is ever generated. So the data-snooping
                # and >30%-landmark-exclusion warnings were composed on every run
                # and never shown to anyone. They belong in the notices stream,
                # which .run() does not hide.
                if (basic_requirements && nrow(self$data) > 0) {
                    for (misuse_warning in private$.detectCommonMisuses())
                        private$.addHtmlMessage(
                            "warning",
                            .("Statistical Analysis Warning"),
                            misuse_warning)
                }

                return(basic_requirements)
            }
            ,
            .handleIncompleteAnalysis = function() {
                private$.todo()
                # Hide all analysis results
                self$results$coxSummary$setVisible(FALSE)
                self$results$coxTable$setVisible(FALSE)
                self$results$tCoxtext2$setVisible(FALSE)
                self$results$rescutTable$setVisible(FALSE)
                self$results$medianSummary$setVisible(FALSE)
                self$results$medianTable$setVisible(FALSE)
                self$results$survTableSummary$setVisible(FALSE)
                self$results$survTable$setVisible(FALSE)
                self$results$plot4$setVisible(FALSE)
                self$results$plot5$setVisible(FALSE)
                self$results$plot2$setVisible(FALSE)
                self$results$plot3$setVisible(FALSE)
                self$results$plot6$setVisible(FALSE)
                self$results$todo$setVisible(TRUE)
            }

            # Clean Data For Analysis ----
            ,
            .cleandata = function() {

            labelled_data <- private$.getData()

            mydata_labelled        <- labelled_data$mydata_labelled
            mytime_labelled        <- labelled_data$mytime_labelled
            myoutcome_labelled     <- labelled_data$myoutcome_labelled
            mydxdate_labelled      <- labelled_data$mydxdate_labelled
            myfudate_labelled      <- labelled_data$myfudate_labelled
            mycontexpl_labelled <- labelled_data$mycontexpl_labelled

                time <- private$.definemytime()
                outcome <- private$.definemyoutcome()
                factor <- private$.definemyfactor()

                if (is.null(time)) {
                    private$.addHtmlMessage(
                        "error",
                        .("Survival time problem"),
                        .("Survival time could not be calculated. Check the elapsed-time variable or the selected date variables and date format.")
                    )
                    return(NULL)
                }
                if (is.null(outcome))
                    return(NULL)
                if (is.null(factor)) {
                    private$.addHtmlMessage(
                        "error",
                        .("Continuous variable problem"),
                        .("The selected continuous explanatory variable could not be read.")
                    )
                    return(NULL)
                }

                private$.checkpoint()

                cleanData <- dplyr::left_join(time, outcome, by = "row_names") %>%
                    dplyr::left_join(factor, by = "row_names")

                # Negative follow-up must be diagnosed BEFORE the landmark filter.
                #
                # .validateInputs() reports negative times, but it only runs after
                # .cleandata() -- and the landmark filter below deletes every row
                # with mytime < landmark first. Transposed diagnosis/follow-up dates
                # make every time negative, so with a landmark set the user was told
                # "No patients remain at risk at landmark time 1.00. Choose an
                # earlier landmark" and would keep lowering the landmark forever
                # instead of learning that their two date columns are swapped.
                n_negative <- sum(cleanData$mytime < 0, na.rm = TRUE)
                if (n_negative > 0) {
                    private$.addHtmlMessage(
                        "error",
                        .("Negative survival time"),
                        sprintf(
                            .("%d of %d record(s) have negative follow-up time. Follow-up cannot be negative; this usually means the diagnosis and follow-up date variables are swapped, or that the elapsed-time variable contains negative values."),
                            n_negative, nrow(cleanData)))
                    return(NULL)
                }

                # Landmark ----
                # https://www.emilyzabor.com/tutorials/survival_analysis_in_r_tutorial.html#landmark_method
                if (self$options$uselandmark) {

                  landmark <- jmvcore::toNumeric(self$options$landmark)

                    if (!is.finite(landmark) || landmark < 0) {
                        private$.addHtmlMessage(
                            "error",
                            .("Invalid landmark time"),
                            .("Landmark time must be a finite, non-negative value.")
                        )
                        return(NULL)
                    }

                    # Account for the landmark BEFORE the naOmit counter below, which
                    # otherwise attributes landmark exclusions to missing data. The
                    # >30% exclusion check in .detectCommonMisuses() reads
                    # self$options$elapsedtime and so never fires in date mode; this
                    # runs on the computed times and covers both.
                    n_pre_landmark <- nrow(cleanData)
                    n_na_time <- sum(is.na(cleanData$mytime))
                    cleanData <- cleanData %>%
                        dplyr::filter(!is.na(mytime) & mytime >= landmark) %>%
                        dplyr::mutate(mytime = mytime - landmark)
                    n_landmark_excluded <- n_pre_landmark - nrow(cleanData) - n_na_time

                    if (n_landmark_excluded > 0) {
                        excl_pct <- 100 * n_landmark_excluded / n_pre_landmark
                        private$.addHtmlMessage(
                            if (excl_pct > 30) "strongWarning" else "info",
                            .("Landmark exclusions"),
                            sprintf(
                                paste0(.("Landmark at %.2f %s: %d of %d patient(s) (%.1f%%) had follow-up shorter than the landmark and were excluded%s; %d remain at risk. Reported times are measured FROM the landmark."),
                                       if (excl_pct > 30)
                                           .(" Excluding this share of the cohort can introduce selection bias; the landmark should be prespecified.")
                                       else ""),
                                landmark, self$options$timetypeoutput,
                                n_landmark_excluded, n_pre_landmark, excl_pct,
                                if (n_na_time > 0) sprintf(.(" , plus %d with missing follow-up time"), n_na_time) else "",
                                nrow(cleanData)))
                    }

                    if (nrow(cleanData) == 0) {
                        private$.addHtmlMessage(
                            "error",
                            .("Landmark beyond follow-up"),
                            sprintf(
                                .("No patients remain at risk at landmark time %.2f. Choose an earlier landmark."),
                                landmark
                            )
                        )
                        return(NULL)
                    }
                }

                # Names cleanData ----

                if (self$options$tint) {
                    name1time <- "CalculatedTime"
                }

                if (!self$options$tint &&
                    !is.null(self$options$elapsedtime)) {
                    name1time <- mytime_labelled
                }

                name2outcome <- myoutcome_labelled

                if (self$options$multievent) {
                    name2outcome <- "CalculatedOutcome"
                }

                if (!is.null(self$options$contexpl)
                    ) {
                    name3contexpl <- mycontexpl_labelled
                    }

                cleanData <- cleanData %>%
                    dplyr::rename(
                        !!name1time := mytime,
                        !!name2outcome := myoutcome,
                        !!name3contexpl := myfactor
                    )

                analysis_outcome <- paste0(name2outcome, "_event")
                cleanData[[analysis_outcome]] <- private$.eventOfInterestIndicator(cleanData[[name2outcome]])

                # naOmit ----
                n_before_complete_cases <- nrow(cleanData)
                cleanData <- jmvcore::naOmit(cleanData)
                n_excluded <- n_before_complete_cases - nrow(cleanData)

                if (n_excluded > 0) {
                    private$.addHtmlMessage(
                        "warning",
                        .("Incomplete records excluded"),
                        sprintf(
                            .("%d of %d record(s) were excluded because survival time, outcome, or the continuous explanatory variable was missing."),
                            n_excluded,
                            n_before_complete_cases
                        )
                    )
                }

                if (nrow(cleanData) == 0) {
                    private$.addHtmlMessage(
                        "error",
                        .("No complete records"),
                        .("No complete records remain after applying the selected variables and landmark restriction.")
                    )
                    return(NULL)
                }

                # Return Data ----

                return(
                    list(
                        "landmark_offset" = if (self$options$uselandmark)
                            jmvcore::toNumeric(self$options$landmark) else 0,
                        # Raw dxdate->fudate interval for EVERY row, taken before
                        # the landmark shift/filter and naOmit; this is what the
                        # calculatedtime Output exports.
                        "calculated_time" = time,
                        "name1time" = name1time,
                        "name2outcome" = name2outcome,
                        "analysis_outcome" = analysis_outcome,
                        "name3contexpl" = name3contexpl,
                        "cleanData" = cleanData,
                        "mytime_labelled" = mytime_labelled,
                        "myoutcome_labelled" = myoutcome_labelled,
                        "mydxdate_labelled" = mydxdate_labelled,
                        "myfudate_labelled" = myfudate_labelled,
                        "mycontexpl_labelled" = mycontexpl_labelled
                    )
                )

            }



            # Run Analysis ----
            ,
            .run = function() {

                # Reset accumulating HTML message outputs at the start of every run
                # so notices are not duplicated across re-run cycles.
                private$.initializeMessageOutputs()
                self$results$clinicalWarnings$setContent("")
                self$results$clinicalWarnings$setVisible(FALSE)
                private$.eventRecode <- NULL
                private$.cutoffRan <- FALSE
                private$.multicutRan <- FALSE
                private$.multicutFailReason <- NULL
                self$results$eventRecodeInfo$setContent("")

                # Prevent plots from a previous valid run remaining visible after
                # a new invalid option or variable selection.
                for (plot_name in c(
                    "plot2", "plot3", "plot4", "plot5", "plot6", "plot7",
                    "plotMultipleCutoffs", "plotMultipleSurvival", "residualsPlot"
                )) {
                    self$results[[plot_name]]$setState(NULL)
                }

                # Errors, Warnings ----

                ## No variable todo ----

                ## Validate Analysis Requirements ----
                not_continue_analysis <- !private$.validateAnalysisRequirements()


                if (not_continue_analysis) {
                    private$.handleIncompleteAnalysis()
                    return()
                } else {
                    self$results$todo$setVisible(FALSE)
                }


                ## Input Validation and Data Checks ----

                # Enhanced input validation using helper method
                if (nrow(self$data) == 0) {
                    return()
                }

                private$.checkpoint()

                # Get Clean Data ----
                results <- private$.cleandata()

                if (is.null(results))
                    return()

                # Always disclose how the outcome was recoded. A silent recode is a
                # clinical-safety hazard: the reader of a survival curve cannot otherwise
                # see which levels were collapsed into "censored", nor which estimand
                # the probability-scale outputs actually correspond to.
                if (!is.null(private$.eventRecode))
                    self$results$eventRecodeInfo$setContent(
                        .describeEventIndicator(private$.eventRecode, self$options$outcome))

                ## Competing risks are not implemented here ----
                # Gated once, rather than at each consumer: this analysis has ~15
                # places that build a survfit/coxph and only .cox was ever guarded.
                # survival::Surv() accepts the 0/1/2 competing-risk coding with a
                # warning jamovi never displays and then remaps 1 to censored,
                # 2 to event and 0 to NA -- so every one of those outputs would
                # render inverted. There is also no cumulative-incidence
                # implementation here to fall back on.
                #
                # This gate sits AFTER .cleandata(), not before it. It used to test
                # `multievent && analysistype == "compete"` at the very top of
                # .run(), which is FALSE for an outcomeorganizer hand-off -- the
                # 0/1/2 status arrives with multievent unset -- so exactly the case
                # that needs blocking walked straight past. .isCompetingRisk() reads
                # private$.eventRecode, which only exists once .cleandata() has run,
                # so the gate has to follow it. Running .cleandata() first also lets
                # the recode disclosure above explain WHY the analysis stopped.
                if (private$.isCompetingRisk()) {
                    private$.addHtmlMessage(
                        "error",
                        .("Competing risks not available in this analysis"),
                        .("Cut-off analysis for a continuous predictor does not support competing risks. Use Survival Analysis or Multivariable Survival Analysis for competing-risk models, or set survival type to Overall, Cause Specific, or Disease-Free here."))
                    # Returning here means the setState() block further down never
                    # runs, so a plot state left over from the PREVIOUS (valid)
                    # outcome variable would still be rendered -- a correct-looking
                    # curve sitting under an error message. Drop them.
                    for (p in c("plot2", "plot3", "plot4", "plot5", "plot6", "plot7",
                                "plotMultipleCutoffs", "plotMultipleSurvival",
                                "residualsPlot"))
                        self$results[[p]]$setState(NULL)
                    return()
                }

                # Event Count Validation (Critical for Survival Analysis) ----
                if (!is.null(results) && !is.null(results$cleanData) && !is.null(results$name2outcome)) {
                    # Calculate event count
                    n_events <- sum(results$cleanData[[results$name2outcome]] == 1, na.rm = TRUE)
                    n_total <- nrow(results$cleanData)

                    # ERROR: < 10 events (insufficient for analysis)
                    # Warn and continue with descriptive output; cut-point
                    # determination stays suppressed because a cut chosen from
                    # this few events would not generalise.
                    private$.lowEventCount <- n_events < 10
                    if (private$.lowEventCount) {
                        private$.addHtmlMessage(
                            type = "strongWarning",
                            title = sprintf(.("Only %d event(s) observed"), n_events),
                            message = sprintf(.("Only %d event(s) detected (n=%d total). Descriptive results are shown, but estimates from this many events are unstable with very wide confidence intervals. Cut-off determination is suppressed."), n_events, n_total)
                        )
                    }

                    # STRONG_WARNING: 10-19 events (very limited reliability)
                    if (n_events >= 10 && n_events < 20) {
                        private$.addHtmlMessage(
                            type = "strongWarning",
                            title = .("Limited Events"),
                            message = sprintf(.("Only %d events detected (n=%d total). Survival analysis with fewer than 20 events has very limited statistical reliability. Confidence intervals will be wide, median survival may be undefined, and Cox regression estimates are unstable. Consider: (1) collecting more data, (2) combining with external datasets, or (3) performing descriptive analysis only without hypothesis testing."), n_events, n_total)
                        )
                    }

                    # WARNING: 20-49 events (limited but acceptable)
                    if (n_events >= 20 && n_events < 50) {
                        private$.addHtmlMessage(
                            type = "warning",
                            title = .("Moderate Event Count"),
                            message = sprintf(.("%d events detected (n=%d total). Analysis is feasible but statistical power is limited. Confidence intervals may be wider than ideal. Results should be interpreted cautiously and ideally validated in larger cohorts."), n_events, n_total)
                        )
                    }

                    # EPV Check for Cox Regression (if continuous variable present)
                    if (!is.null(results$name3contexpl) && n_events >= 10) {
                        n_covariates <- 1  # Single continuous variable
                        epv <- n_events / n_covariates

                        if (epv < 10) {
                            private$.addHtmlMessage(
                                type = "strongWarning",
                                title = .("Low Events Per Variable (EPV)"),
                                message = sprintf(.("Events per variable (EPV) = %d/%d = %.1f. Recommended minimum is 10 EPV for reliable Cox regression. With EPV < 10, coefficient estimates may be biased, standard errors inflated, and overfitting likely. Consider: (1) collecting more events, (2) simpler models, or (3) penalized regression methods."), n_events, n_covariates, epv)
                            )
                        }
                    }

                    # Small sample size warning
                    if (n_total < 50) {
                        private$.addHtmlMessage(
                            type = "warning",
                            title = .("Small Sample Size"),
                            message = sprintf(.("Total sample size n=%d is small for survival analysis. Asymptotic assumptions for confidence intervals and hypothesis tests may not hold. Results should be considered preliminary and validated in larger datasets."), n_total)
                        )
                    }

                    # Additional Data Quality Checks ----
                    # High censoring rate (event rate < 20% means >80% censored)
                    event_rate <- n_events / n_total
                    if (event_rate < 0.20) {
                        censoring_rate <- 100 * (1 - event_rate)
                        private$.addHtmlMessage(
                            type = "strongWarning",
                            title = .("High Censoring Rate"),
                            message = sprintf(.("%.1f%% of observations are censored (only %d events out of %d). With such heavy censoring, survival estimates beyond the median may be unreliable or undefined. Confidence intervals will be very wide in the tail. Consider: (1) longer follow-up, (2) focusing on earlier time points, or (3) alternative endpoints with higher event rates."), censoring_rate, n_events, n_total)
                        )
                    } else if (event_rate < 0.30 && event_rate >= 0.20) {
                        censoring_rate <- 100 * (1 - event_rate)
                        private$.addHtmlMessage(
                            type = "warning",
                            title = .("Moderate Censoring"),
                            message = sprintf(.("%.1f%% of observations are censored. While analysis is feasible, statistical power is reduced and late survival estimates may have wide confidence intervals."), censoring_rate)
                        )
                    }

                    # Short median follow-up is checked once, in
                    # .checkClinicalAssumptions() below, which renders into the
                    # Clinical Assumptions box. A second copy lived here and fired
                    # only for months (< 6) and years (< 2) with hard-coded
                    # thresholds, so month-scale data was warned twice in two
                    # different boxes while day- and week-scale data was never
                    # warned at all. Removed in favour of the unit-aware check.

                    # Limited variability in continuous explanatory variable
                    if (!is.null(results$name3contexpl) && !is.null(results$cleanData)) {
                        cont_var <- results$cleanData[[results$name3contexpl]]
                        if (!is.null(cont_var)) {
                            n_unique <- length(unique(cont_var[!is.na(cont_var)]))
                            if (n_unique < 10) {
                                private$.addHtmlMessage(
                                    type = "strongWarning",
                                    title = .("Very Limited Variability"),
                                    message = sprintf(.("Continuous variable \"%s\" has only %d unique values. This severely limits cut-off analysis and Cox regression assumptions. Consider: (1) treating as categorical, (2) verifying data quality, or (3) using a different variable with more variation."), results$name3contexpl, n_unique)
                                )
                            } else if (n_unique < 20) {
                                private$.addHtmlMessage(
                                    type = "warning",
                                    title = .("Limited Variability"),
                                    message = sprintf(.("Continuous variable \"%s\" has only %d unique values. While analysis is possible, optimal cut-off determination and Cox regression may be limited. Results should be interpreted cautiously."), results$name3contexpl, n_unique)
                                )
                            }
                        }
                    }
                }

                # Clinical Assumption Checking ----
                if (!is.null(results) && !is.null(results$cleanData)) {
                    clinical_warnings <- private$.checkClinicalAssumptions(
                        results$cleanData,
                        results$name1time,
                        results$name2outcome,
                        results$name3contexpl
                    )

                    if (length(clinical_warnings) > 0) {
                        warning_content <- private$.createInterpretationBox(
                            .("Clinical Assumptions Warning"),
                            paste(clinical_warnings, collapse = "<br><br>"),
                            warning = TRUE
                        )

                        # Store warnings for display
                        self$results$clinicalWarnings$setContent(warning_content)
                        self$results$clinicalWarnings$setVisible(TRUE)

                        # Notice removed - warnings displayed in HTML output only
                    }
                }

                # Additional validation after data cleaning
                if (!is.null(results$cleanData)) {
                    inputs_valid <- private$.validateInputs(
                        data = results$cleanData,
                        time_var = results$name1time,
                        outcome_var = results$name2outcome,
                        contexpl_var = results$name3contexpl
                    )

                    # Memory usage monitoring for large datasets
                    private$.checkMemoryUsage(results$cleanData)

                    # Halt if survival times/outcomes are invalid (NA, non-finite,
                    # negative, or with no positive follow-up at all);
                    # .validateInputs has already emitted an explanatory error notice.
                    if (isFALSE(inputs_valid)) {
                        return()
                    }
                }


                # Run Analysis ----

                ## Run Continious Cox Regression ----
                private$.cox(results)



                ## Add the person-time analysis ----
                private$.checkpoint()  # Add checkpoint here

                # Run person-time analysis if enabled
                if (self$options$person_time) {
                    private$.personTimeAnalysis(results)
                }



                ## Run RMST analysis before cutoff (if enabled) ----
                if (self$options$rmst_analysis) {
                    private$.calculateRMST(results)
                }

                ## Run Residual diagnostics before cutoff (if enabled) ----
                if (self$options$residual_diagnostics) {
                    private$.calculateResiduals(results)
                }

                ## Stratified Cox (if enabled) ----
                if (self$options$stratified_cox) {
                    private$.stratifiedCox(results)
                }

                ## Run Multiple Cut-offs Analysis (INDEPENDENT) ----
                multicut_results <- NULL



                if (self$options$multiple_cutoffs) {
                    # Use the original clean data, before any single cutoff processing
                    multicut_results <- if (private$.lowEventCount) NULL
                                        else private$.multipleCutoffs(results)
                    if (!is.null(multicut_results)) {
                        private$.multicutRan <- TRUE
                        private$.multipleCutoffTables(multicut_results)

                        # Persist only the vectors each renderer needs. Storing the
                        # complete clean-data/results object twice made saved .omv files
                        # unnecessarily large and duplicated fitted survfit objects.
                        self$results$plotMultipleCutoffs$setState(list(
                            values = results$cleanData[[results$name3contexpl]],
                            cutoff_values = multicut_results$cutoff_values,
                            method = multicut_results$method
                        ))
                        self$results$plotMultipleSurvival$setState(list(
                            time = results$cleanData[[results$name1time]],
                            outcome = results$cleanData[[results$analysis_outcome]],
                            risk_groups = multicut_results$risk_groups,
                            method = multicut_results$method
                        ))

                        # Add multiple cutoff groups to data
                        if (self$options$calculatedmulticut &&
                            self$results$calculatedmulticut$isNotFilled()) {
                            self$results$calculatedmulticut$setRowNums(results$cleanData$row_names)
                            self$results$calculatedmulticut$setValues(multicut_results$risk_groups)
                        }
                    } else {
                        # No results this cycle: drop the previous cycle's rows
                        # rather than leaving them under a failure message.
                        self$results$multipleCutTable$deleteRows()
                        self$results$multipleMedianTable$deleteRows()
                        self$results$multipleSurvTable$deleteRows()
                    }
                    if (is.null(multicut_results) && !private$.lowEventCount) {
                        reason <- private$.multicutFailReason
                        private$.addHtmlMessage(
                            "warning",
                            .("Multiple cut-offs unavailable"),
                            paste0(
                                .("The requested multiple cut-offs could not be estimated from these data"),
                                if (!is.null(reason)) paste0(": ", reason) else "",
                                .(". Check variability, group-size constraints, and event counts, or use the continuous Cox model.")
                            )
                        )
                    }
                }

                # Add Calculated Time to Data (independent of cut-off analysis) ----
                # Export the raw interval, not cleanData$CalculatedTime: with a
                # landmark active that column is time FROM the landmark and rows
                # below the landmark are gone, so the exported "time from dxdate
                # to fudate" silently became a different quantity.
                if (self$options$tint && self$options$calculatedtime && self$results$calculatedtime$isNotFilled()) {
                    self$results$calculatedtime$setRowNums(results$calculated_time$row_names)
                    self$results$calculatedtime$setValues(results$calculated_time$mytime)
                }

                # Add Redefined Outcome to Data (independent of cut-off analysis) ----
                if (self$options$multievent  && self$options$outcomeredefined && self$results$outcomeredefined$isNotFilled()) {
                    self$results$outcomeredefined$setRowNums(results$cleanData$row_names)
                    self$results$outcomeredefined$setValues(results$cleanData$CalculatedOutcome)
                }

                ## Run Cut-off calculation and further analysis ----
                # Only the cut-off-specific block below depends on findcut; the
                # completion notices and educational explanations further down always run.
                if (self$options$findcut && !private$.lowEventCount) {


                ## Run Cut-off calculation ----
                res.cut <- private$.cutoff(results)

                if (is.null(res.cut)) {
                    private$.addHtmlMessage(
                        "warning",
                        .("Optimal cut-off unavailable"),
                        .("An optimal cut-off could not be estimated from these data. The continuous Cox model remains the primary analysis; check predictor variability, event count, and minimum group size.")
                    )
                    private$.addExplanations()
                    return()
                }

                private$.cutoffRan <- TRUE

                ## Run Cut-off Table ----
                private$.cutoffTable(res.cut)

                ## Run Categorise Data ----
                cutoffdata <- private$.cutoff2(res.cut)

                ## Validate group sizes after cut-off ----
                # Check for small groups that may produce unreliable statistics
                if (!is.null(cutoffdata) && !is.null(results$name3contexpl)) {
                    group_var <- results$name3contexpl
                    if (group_var %in% names(cutoffdata)) {
                        group_counts <- table(cutoffdata[[group_var]])
                        min_group_size <- min(group_counts)
                        min_group_name <- names(group_counts)[which.min(group_counts)]

                        # Count events in each group
                        outcome_var <- results$analysis_outcome
                        if (outcome_var %in% names(cutoffdata)) {
                            for (grp in names(group_counts)) {
                                grp_data <- cutoffdata[cutoffdata[[group_var]] == grp, ]
                                n_events_grp <- sum(grp_data[[outcome_var]], na.rm = TRUE)

                                # STRONG_WARNING for very small groups (n<10)
                                if (group_counts[grp] < 10) {
                                    private$.addHtmlMessage(
                                        type = "strongWarning",
                                        title = .("Very Small Group After Cut-off"),
                                        message = sprintf(.("Very small group size after cut-off: \"%s\" has only %d observations (%d events). Statistical tests (log-rank, Cox regression) are unreliable with such small groups. Consider: (1) alternative cut-off methods, (2) treating variable as continuous, or (3) collecting more data."), grp, group_counts[grp], n_events_grp)
                                    )
                                } else if (group_counts[grp] < 20) {
                                    # WARNING for small groups (10-19)
                                    private$.addHtmlMessage(
                                        type = "warning",
                                        title = .("Small Group After Cut-off"),
                                        message = sprintf(.("Small group size after cut-off: \"%s\" has %d observations (%d events). Statistical power is limited. Confidence intervals may be wide. Interpret results cautiously."), grp, group_counts[grp], n_events_grp)
                                    )
                                }

                                # Check events per group (minimum 5 for reliable survival analysis)
                                if (n_events_grp < 5 && n_events_grp > 0) {
                                    private$.addHtmlMessage(
                                        type = "strongWarning",
                                        title = .("Very Few Events in Group"),
                                        message = sprintf(.("Very few events in group \"%s\" (%d events out of %d observations). Survival estimates and confidence intervals are highly unstable. Median survival may be undefined. Cox regression unreliable."), grp, n_events_grp, group_counts[grp])
                                    )
                                }
                            }
                        }
                    }
                }

                ## Run RMST analysis with cutoff data (if enabled) ----
                if (self$options$rmst_analysis) {
                    private$.calculateRMST(results, cutoffdata)
                }

                ## Run Residual diagnostics with cutoff data (if enabled) ----
                if (self$options$residual_diagnostics) {
                    private$.calculateResiduals(results, cutoffdata)
                }








                ## Run median cutoff ----

                private$.mediancutoff(cutoffdata, results)

                ## Run life table cutoff ----

                private$.lifetablecutoff(cutoffdata, results)







                # Prepare Data For Plots ----

                plotData1 <- list(res.cut = res.cut,
                                  name3contexpl = results$name3contexpl
                                  # ,
                                  # not_continue_analysis = not_continue_analysis
                )



                image4 <- self$results$plot4
                image4$setState(plotData1)

                plotData2 <- list(
                    cutoffdata = cutoffdata,
                    results = list(
                        name1time = results$name1time,
                        analysis_outcome = results$analysis_outcome,
                        name3contexpl = results$name3contexpl
                    )
                    # ,
                    # not_continue_analysis = not_continue_analysis
                    )

                image5 <- self$results$plot5
                image5$setState(plotData2)

                image2 <- self$results$plot2
                image2$setState(plotData2)

                image3 <- self$results$plot3
                image3$setState(plotData2)

                image6 <- self$results$plot6
                image6$setState(plotData2)

                # Set state for new plots
                image7 <- self$results$plot7
                image7$setState(plotData2)

                # (residualsPlot state is set inside .calculateResiduals with the actual
                # residual data; do NOT overwrite it here with plotData2.)



                # Add calculatedcutoff to Data ----

                cutoffgr <- cutoffdata[[results$name3contexpl]]

                if (self$options$calculatedcutoff &&
                        self$results$calculatedcutoff$isNotFilled()) {
                        self$results$calculatedcutoff$setRowNums(results$cleanData$row_names)
                        self$results$calculatedcutoff$setValues(cutoffgr)
                }

                }
                # ---- End of cut-off-specific analysis block (findcut) ----

            # Analysis Completion INFO Notices ----
            # Add completion notices at the bottom (high position numbers)
            if (!is.null(results$cleanData) && !is.null(results$name3contexpl)) {
                # List completed analyses
                completed_analyses <- c()
                # These used to be built from the OPTIONS, so a suppressed or failed
                # cut-off search still announced "Successfully completed: cut-off
                # analysis" next to an empty table.
                if (isTRUE(private$.cutoffRan)) completed_analyses <- append(completed_analyses, "cut-off analysis")
                if (isTRUE(private$.multicutRan)) completed_analyses <- append(completed_analyses, "multiple cut-off analysis")
                if (self$options$rmst_analysis) completed_analyses <- append(completed_analyses, "RMST analysis")
                if (self$options$residual_diagnostics) completed_analyses <- append(completed_analyses, "residual diagnostics")
                if (self$options$person_time) completed_analyses <- append(completed_analyses, "person-time analysis")

                # Cox regression is always run for continuous variables
                completed_analyses <- append(completed_analyses, "Cox regression")

                if (length(completed_analyses) > 0) {
                    analyses_list <- paste(completed_analyses, collapse = ", ")
                    private$.addHtmlMessage(
                        type = "info",
                        title = .("Analysis Complete"),
                        message = sprintf(.("Successfully completed: %s. Results are displayed in the tables and plots below. Review all sections carefully, paying special attention to any warnings or clinical assumptions."), analyses_list)
                    )
                }

                # Methodology notice (cite if publishing)
                if (isTRUE(private$.cutoffRan)) {
                    private$.addHtmlMessage(
                        type = "info",
                        title = .("Methodology Reference"),
                        message = .("The cut-off is the value that maximises the standardised log-rank statistic (maximally selected rank statistic, survminer::surv_cutpoint / maxstat). No multiplicity-adjusted p-value is computed or reported for the selected cut-off; the group comparison p-values shown after the split are exploratory because the split was chosen from these same data. Report the continuous Cox model as the primary analysis and validate the cut-off in independent data. When publishing, cite survminer and maxstat.")
                    )
                }
            }

            # Educational Explanations ----
            # Always add explanation content - visibility is controlled by YAML conditions
            private$.addExplanations()

            }





            # Continious Cox Regression ----
            ,
            .cox = function(results) {

                private$.checkpoint()

                ## Cox Regression ----

                # Wrap Cox regression analysis in error recovery
                cox_result <- private$.safeAnalysis(function() {

                    mytime <- results$name1time
                    mytime <- jmvcore::constructFormula(terms = mytime)

                    myoutcome <- results$analysis_outcome
                    myoutcome <-
                        jmvcore::constructFormula(terms = myoutcome)

                    myfactor <- results$name3contexpl
                    myfactor <-
                        jmvcore::constructFormula(terms = myfactor)

                    mydata <- results$cleanData

                    mydata[[mytime]] <-
                        jmvcore::toNumeric(mydata[[mytime]])

                    myformula <-
                        paste("Surv(", mytime, ",", myoutcome, ")")

                    # cont_cut = 0 is load-bearing.
                    #
                    # finalfit's default is cont_cut = 5: a numeric explanatory
                    # variable with fewer than 5 distinct values is mutate_at'd to a
                    # FACTOR before the model is fitted. A 4-level integer score
                    # (Gleason group, Allred, budding tier) was therefore reported as
                    # four per-level hazard ratios -- 0.54 / 0.47 / 0.41 against a
                    # reference level -- where the continuous model gives a single
                    # HR of 0.7466. The table's own footnote still said "a one-unit
                    # increase", and every other path in this analysis (cox.zph, the
                    # cut-point search, RMST grouping, the residual model) kept using
                    # the untouched numeric column, so the displayed hazard ratios
                    # came from a different model than the rest of the report.
                    finalfit::finalfit(
                        .data = mydata,
                        dependent = myformula,
                        explanatory = myfactor,
                        metrics = TRUE,
                        cont_cut = 0
                    )
                }, context = .("Cox regression analysis"), fallback_value = list(NULL, NULL))

                tCox <- cox_result

                # Check if Cox analysis was successful
                if (is.null(tCox) || is.null(tCox[[1]]) || is.null(tCox[[2]])) {
                    error_msg <- .(
                        "Cox regression analysis failed. This may be due to insufficient data, convergence issues, or inappropriate data structure. Please check your variables and try again."
                    )
                    self$results$tCoxtext2$setContent(private$.createInterpretationBox(
                        .("Analysis Error"), error_msg, warning = TRUE
                    ))
                    return()
                }

                # Restore original variable names in finalfit output table
                if (!is.null(tCox[[1]]) && nrow(tCox[[1]]) > 0) {
                    labelled_data <- private$.getData()
                    tCox[[1]] <- private$.restoreOriginalNamesInTable(tCox[[1]], labelled_data$all_labels)
                }

                # Test Proportional Hazards Assumption using cox.zph() ----
                # This is critical for validating Cox model assumptions
                tryCatch({
                    mytime <- results$name1time
                    mytime <- jmvcore::constructFormula(terms = mytime)
                    myoutcome <- results$analysis_outcome
                    myoutcome <- jmvcore::constructFormula(terms = myoutcome)
                    myfactor <- results$name3contexpl
                    myfactor <- jmvcore::constructFormula(terms = myfactor)
                    mydata <- results$cleanData
                    mydata[[mytime]] <- jmvcore::toNumeric(mydata[[mytime]])

                    # Fit Cox model for PH testing
                    cox_formula_str <- paste0("survival::Surv(", mytime, ",", myoutcome, ") ~ ", myfactor)
                    cox_model_ph <- survival::coxph(.asSurvivalFormula(cox_formula_str), data = mydata)

                    # Test proportional hazards assumption
                    zph_test <- survival::cox.zph(cox_model_ph)

                    # Check global test (overall model assumption)
                    global_p <- zph_test$table["GLOBAL", "p"]

                    if (!is.na(global_p) && global_p < 0.05) {
                        # PH assumption violated
                        private$.addHtmlMessage(
                            type = "strongWarning",
                            title = .("Proportional Hazards Assumption Violated"),
                            message = sprintf(.("Proportional hazards assumption violated (Schoenfeld residual test p=%.3f). Cox model estimates may be unreliable. Hazard ratios may change over time. Consider: (1) stratified Cox regression (Advanced Options), (2) time-varying coefficients, (3) log-log plot for visual assessment, or (4) parametric survival models."), global_p)
                        )
                    }

                    # Check individual covariate if applicable
                    if (nrow(zph_test$table) > 1) {
                        # Multiple rows mean we have individual covariate tests
                        covariate_rows <- rownames(zph_test$table)[rownames(zph_test$table) != "GLOBAL"]
                        for (var_name in covariate_rows) {
                            var_p <- zph_test$table[var_name, "p"]
                            if (!is.na(var_p) && var_p < 0.05) {
                                private$.addHtmlMessage(
                                    type = "strongWarning",
                                    title = .("Variable-Specific PH Violation"),
                                    message = sprintf(.("Proportional hazards assumption violated for \"%s\" (p=%.3f). The effect of this variable on hazard changes over time. Cox HR may not accurately represent the relationship across all time points."), var_name, var_p)
                                )
                            }
                        }
                    }
                }, error = function(e) {
                    # A failed PH test must never look like a passed one.
                    #
                    # This used to suppress the notice exactly when the message
                    # matched "singular" or "convergence" -- the two cases most
                    # worth reporting. The user then saw no proportional-hazards
                    # warning anywhere and had every reason to conclude the
                    # assumption held, when in fact the test never ran. The
                    # notice is now unconditional; only its wording varies.
                    fit_failure <- grepl("singular|convergence|infinite|did not converge",
                                         e$message, ignore.case = TRUE)

                    private$.addHtmlMessage(
                        type = "warning",
                        title = .("Proportional Hazards Test Could Not Be Performed"),
                        message = if (fit_failure) {
                            sprintf(.("The Cox model underlying the proportional hazards test did not converge (%s), so the assumption could NOT be tested. This is not evidence that proportional hazards holds. It usually means a covariate is collinear or perfectly separates the outcome, or that there are too few events. Check the Cox results below for implausibly large hazard ratios or very wide confidence intervals before interpreting them, and consider a log-log plot for visual assessment."),
                                    trimws(e$message))
                        } else {
                            .("The proportional hazards assumption could NOT be tested, so no conclusion about it should be drawn from the absence of a warning. This may occur with very small samples or perfect separation. Interpret the Cox results cautiously and consider visual inspection with log-log plots.")
                        }
                    )
                })

                # Create enhanced Cox results with clinical context
                cox_tooltip <- private$.createClinicalTooltip(
                    .("Cox Regression Analysis"),
                    .("Cox regression estimates the hazard ratio (HR) for a one-unit increase in the continuous predictor. HR > 1 indicates a higher instantaneous event rate and HR < 1 a lower rate, assuming a log-linear effect and proportional hazards."),
                    .("HR = 1.05 means each one-unit increase multiplies the hazard by 1.05; it is not a cumulative risk ratio and does not by itself establish causality.")
                )

                tCoxtext2 <- glue::glue(.(
                    '{tooltip}
                    **Model Metrics:**
                    {metrics}'),
                    tooltip = cox_tooltip,
                    metrics = unlist(tCox[[2]]))

                if (private$.isCompetingRisk()) {
                    tCoxtext2 <- glue::glue(.(
                        '{previous_text}

                        **Competing risk note:** Analyses use cause-specific hazards (competing events censored). For subdistribution hazards, use a Fine-Gray model.'),
                        previous_text = tCoxtext2)
                }

                if (self$options$uselandmark) {
                    landmark <- jmvcore::toNumeric(self$options$landmark)

                    tCoxtext2 <- glue::glue(.(
                        '{previous_text}

                        **Landmark Analysis:** Analysis is conditional on remaining under observation and event-free through {time} {units}. This can address guarantee-time bias when the landmark is prespecified, but it changes the target population and does not remove ordinary confounding or lead-time bias.'),
                        previous_text = tCoxtext2,
                        time = landmark,
                        units = self$options$timetypeoutput)
                }

                self$results$tCoxtext2$setContent(tCoxtext2)


                tCox_df <-
                    tibble::as_tibble(tCox[[1]], .name_repair = "minimal") %>%
                    janitor::clean_names(dat = ., case = "snake")


                # Continious Cox-Regression Table ----

                coxTable <- self$results$coxTable
                # .run() is re-invoked on every option change, including options
                # in no clearWith list (showSummaries, showExplanations, ...). Without
                # this reset addRow() appended a second copy of every row on each
                # cycle: after three runs the survival table showed 18 rows instead of
                # 6 and each cut-off group appeared three times.
                coxTable$deleteRows()

                coxTable$setNote(
                    "scale",
                    jmvcore::format(.("Hazard ratios are unadjusted and correspond to a one-unit increase in '{var}'. The model assumes a linear association with log hazard and proportional hazards over time."), var = self$options$contexpl)
                )

                data_frame <- tCox_df

                # The first column was named "contexpl", which matches no column
                # declared in survivalcont.r.yaml (the column key there is
                # "Explanatory"). addRow() silently ignores an unmatched name, so
                # the variable-name column of the Cox table rendered empty on every
                # run -- the reader could not see WHICH predictor the hazard ratio
                # belonged to.
                names(data_frame) <- c(
                    "Explanatory",
                    "Levels",
                    "all",
                    "HR_univariable",
                    "HR_multivariable"
                )

                for (i in seq_along(data_frame[, 1, drop = TRUE])) {
                    coxTable$addRow(rowKey = i, values = c(data_frame[i, ]))
                }


                # Continious coxTable explanation ----


                tCox_df <-
                    tibble::as_tibble(tCox[[1]], .name_repair = "minimal") %>%
                    janitor::clean_names(dat = ., case = "snake")

                names(tCox_df) <- names(data_frame) <- c(
                    "Explanatory",
                    "Levels",
                    "all",
                    "HR_univariable",
                    "HR_multivariable"
                )


                # https://stackoverflow.com/questions/38470355/r-fill-empty-cell-with-value-of-last-non-empty-cell

                while (length(ind <-
                    which(tCox_df$Explanatory == "")) > 0) {
                    tCox_df$Explanatory[ind] <- tCox_df$Explanatory[ind - 1]
                }

                # https://stackoverflow.com/questions/51180290/mutate-by-group-in-r

                # Enhanced clinical language summary
                tCox_df %>%
                    dplyr::group_by(Explanatory) %>%
                    dplyr::mutate(firstlevel = dplyr::first(Levels)) %>%
                    dplyr::mutate(
                        coxdescription = private$.generateClinicalSentence(
                            "cox_regression",
                            Explanatory,
                            list(hr = HR_multivariable, hr_univariate = HR_univariable)
                        )
                    ) %>%
                    dplyr::filter(HR_univariable != "-") %>%
                    dplyr::pull(coxdescription) -> coxSummary

                coxSummary <- htmltools::htmlEscape(coxSummary)

                # Add clinical interpretation box
                if (length(coxSummary) > 0) {
                    clinical_summary <- private$.createInterpretationBox(
                        .("Clinical Interpretation"),
                        paste(coxSummary, collapse = "<br><br>")
                    )

                    enhanced_summary <- paste(
                        clinical_summary,
                        "<br><hr><br>",
                        .("Copy-ready summary for clinical reports:"),
                        "<br><em>", paste(coxSummary, collapse = " "), "</em>",
                        sep = ""
                    )

                    self$results$coxSummary$setContent(enhanced_summary)
                } else {
                    # Reaching here means the filter above removed every row (no
                    # usable HR), not that anything was tested and found null.
                    # Reporting it as "no significant associations" invented a
                    # substantive negative finding out of an empty result.
                    self$results$coxSummary$setContent(
                        .("No Cox regression results are available to summarise. This usually means the model could not be fitted, or every hazard ratio was missing."))
                }
            }



                # Continuous Optimal Cut-off ----
            ,
            .cutoff = function(results) {

                # Wrap cutoff analysis in error recovery
                cutoff_result <- private$.safeAnalysis(function() {

                    mytime <- results$name1time
                    mytime <- jmvcore::constructFormula(terms = mytime)

                    myoutcome <- results$analysis_outcome
                    myoutcome <-
                        jmvcore::constructFormula(terms = myoutcome)

                    myfactor <- results$name3contexpl
                    myfactor <-
                        jmvcore::constructFormula(terms = myfactor)

                    mydata <- results$cleanData

                    mydata[[mytime]] <-
                        jmvcore::toNumeric(mydata[[mytime]])

                    private$.checkpoint()

                    # https://rpkgs.datanovia.com/survminer/reference/surv_cutpoint.html

                    res.cut <- survminer::surv_cutpoint(
                        mydata,
                        time = mytime,
                        event = myoutcome,
                        variables = myfactor,
                        minprop = self$options$min_group_size / 100
                        # ,
                        # progressbar = TRUE
                    )

                    return(res.cut)

                }, context = .("Optimal cutoff analysis"), fallback_value = NULL)

                return(cutoff_result)

            }

            # Cut-off Table ----
            ,
            .cutoffTable = function(res.cut) {
                rescut_summary <- summary(res.cut)

                rescutTable <- self$results$rescutTable
                rescutTable$deleteRows()

                # Simple plain text title (table titles don't render HTML)
                rescutTable$setTitle(
                    jmvcore::format(.("{var} Optimal Cut-off Analysis"), var = self$options$contexpl)
                )

                # Add plain text interpretation note if cutoff was found
                if (!is.null(rescut_summary) && nrow(rescut_summary) > 0) {
                    cutoff_value <- rescut_summary[1, "cutpoint"]
                    variable_name <- self$options$contexpl

                    clinical_note <- jmvcore::format(.("The data-derived cut-off point for {variable_name} is {cutoff_value}; it separates lower and higher marker values in this dataset."), variable_name = variable_name, cutoff_value = round(cutoff_value, 2))

                    # Set table note with plain text (notes don't render HTML)
                    rescutTable$setNote("clinical", clinical_note)
                }

                # Multiplicity caution (plain text)
                rescutTable$setNote("multiplicity", .("The statistic is the maximally selected standardised log-rank statistic; no multiplicity-adjusted p-value is reported for this cut-off. Downstream group comparisons are exploratory and should be validated in independent data."))

                data_frame <- rescut_summary
                for (i in seq_along(data_frame[, 1, drop = TRUE])) {
                    rescutTable$addRow(rowKey = i, values = c(data_frame[i, ]))
                }
            }

            # Categorise Data ----
            ,
            .cutoff2 = function(res.cut) {
                res.cat <- survminer::surv_categorize(res.cut)
                return(res.cat)
            }


            # Median ----
            ,
            .mediancutoff = function(cutoffdata, results) {

                private$.checkpoint()

                # 'results' is passed in from .run() (already computed via .cleandata())
                # to avoid re-running clean_names/labelled processing on every call.

                mydata <- cutoffdata

                ## Median Survival Table ----

                mytime <- results$name1time
                myoutcome <- results$analysis_outcome
                mycontexpl <- results$name3contexpl


                mytime <-
                    jmvcore::constructFormula(terms = mytime)

                myoutcome <-
                    jmvcore::constructFormula(terms = myoutcome)

                mycontexpl <-
                    jmvcore::constructFormula(terms = mycontexpl)


                mydata[[mytime]] <-
                    jmvcore::toNumeric(mydata[[mytime]])


                # Escape variable names for safe formula construction
                escaped_time <- private$.escapeVariableNames(mytime)
                escaped_outcome <- private$.escapeVariableNames(myoutcome)
                escaped_contexpl <- private$.escapeVariableNames(mycontexpl)

                formula <-
                    paste('survival::Surv(',
                          escaped_time,
                          ',',
                          escaped_outcome,
                          ') ~ ',
                          escaped_contexpl)

                formula <- .asSurvivalFormula(formula)

                km_fit <- survival::survfit(formula, data = mydata)


                km_fit_median_df <- summary(km_fit)

                results1html <-
                    as.data.frame(km_fit_median_df$table) %>%
                    janitor::clean_names(dat = ., case = "snake") %>%
                    tibble::rownames_to_column(.data = .)


                results1html[, 1] <- gsub(
                    pattern = ", ",
                    replacement = " and ",
                    x = results1html[, 1]
                )

                results1table <- results1html

                results1table <- results1html

                names(results1table)[1] <- "factor"


                results2table <- results1table

                # Apply name restoration for better display
                labelled_data <- private$.getData()
                results2table$factor <- gsub(pattern = paste0(mycontexpl,"="),
                                             replacement = paste0(self$options$contexpl, " = "),
                                             x = results1table$factor)



                medianTable <- self$results$medianTable
                medianTable$deleteRows()
                private$.landmarkNote(self$results$medianTable, results)
                data_frame <- results2table
                for (i in seq_along(data_frame[, 1, drop = TRUE])) {
                    medianTable$addRow(rowKey = i, values = c(data_frame[i,]))
                }


                ## Median Survival Summary ----

                results1table %>%
                    dplyr::mutate(
                        description =
                            glue::glue(.(
                                'When {factor_val}, median survival is {median_val} [{lower} - {upper}, 95% CI] {units}.'),
                                factor_val = factor,
                                median_val = round(median, digits = 1),
                                lower = round(x0_95lcl, digits = 1),
                                upper = round(x0_95ucl, digits = 1),
                                units = self$options$timetypeoutput)
                    ) %>%
                    dplyr::mutate(
                        description = dplyr::case_when(
                            is.na(median) ~ paste0(
                                glue::glue(.(
                                    '{desc}. Note that when {factor_val}, the survival curve does not drop below 1/2 during the observation period, thus the median survival is undefined.'),
                                    desc = description, factor_val = factor)),
                            TRUE ~ paste0(description)
                        )
                    ) %>%
                    dplyr::mutate(description = gsub(
                        pattern = "=",
                        replacement = " is ",
                        x = description
                    )) %>%
                    dplyr::mutate(description = gsub(
                        pattern = mycontexpl,
                        replacement = self$options$contexpl,
                        x = description
                    )) %>%
                    dplyr::select(description) %>%
                    dplyr::pull(.) -> km_fit_median_definition

                km_fit_median_definition <- htmltools::htmlEscape(km_fit_median_definition)

                medianSummary <- c(km_fit_median_definition,
                                   "The median survival time is when 50% of subjects have experienced the event.",
                                   "This means that 50% of subjects in this group survived longer than this time period."
                )


                self$results$medianSummary$setContent(medianSummary)

            }


            # Life Table ----
            ,
            .lifetablecutoff = function(cutoffdata, results) {

                private$.checkpoint()


                # survival table with flexible time points (preserving 1,3,5-yr default) ----

                # Use enhanced parsing method with 1,3,5 year defaults
                utimes <- private$.parseSurvivalTimePoints(
                    self$options$cutp,
                    default_points = private$.getDefaultCutpoints()
                )

                # 'results' is passed in from .run() (already computed via .cleandata())
                # to avoid re-running clean_names/labelled processing on every call.

                mydata <- cutoffdata

                mytime <- results$name1time
                myoutcome <- results$analysis_outcome
                mycontexpl <- results$name3contexpl


                mytime <-
                    jmvcore::constructFormula(terms = mytime)

                myoutcome <-
                    jmvcore::constructFormula(terms = myoutcome)

                mycontexpl <-
                    jmvcore::constructFormula(terms = mycontexpl)


                mydata[[mytime]] <-
                    jmvcore::toNumeric(mydata[[mytime]])


                # Escape variable names for safe formula construction
                escaped_time <- private$.escapeVariableNames(mytime)
                escaped_outcome <- private$.escapeVariableNames(myoutcome)
                escaped_contexpl <- private$.escapeVariableNames(mycontexpl)

                formula <-
                    paste('survival::Surv(',
                          escaped_time,
                          ',',
                          escaped_outcome,
                          ') ~ ',
                          escaped_contexpl)

                formula <- .asSurvivalFormula(formula)

                km_fit <- survival::survfit(formula, data = mydata)

                # Do not report survival at time points nobody was followed to.
                #
                # extend = TRUE carries the last Kaplan-Meier value and its interval
                # forward and relabels them with the requested time, so a cohort with
                # 30 months of follow-up printed a 60-month survival of 17.4%
                # (7.1-42.4) at n.risk = 0 -- a report-ready 5-year figure from 2.5
                # years of data. The multiple-cut-off table calls summary() WITHOUT
                # extend and prints NA for the same groups, so one result object
                # carried two contradictory 5-year statements; the RMST path already
                # hard-rejects horizons beyond observed follow-up.
                max_observed <- max(mydata[[mytime]], na.rm = TRUE)
                dropped <- utimes[utimes > max_observed]
                utimes <- utimes[utimes <= max_observed]
                if (length(dropped) > 0)
                    self$results$survTable$setNote("horizon", jmvcore::format(.("Time point(s) {dropped} omitted: they exceed the longest observed follow-up ({max} {unit}). Survival cannot be estimated beyond the data."), dropped = paste(dropped, collapse = ", "), max = round(max_observed, 2), unit = self$options$timetypeoutput))
                if (length(utimes) == 0) {
                    self$results$survTable$setNote("horizon", jmvcore::format(.("No survival time point could be reported: every requested time exceeds the longest observed follow-up ({max} {unit}). Choose earlier time points."), max = round(max_observed, 2), unit = self$options$timetypeoutput))
                    self$results$survTableSummary$setContent("")
                    return()
                }

                km_fit_summary <- summary(km_fit, times = utimes, extend = TRUE)

                km_fit_df <-
                    as.data.frame(km_fit_summary[c(
                        "strata",
                        "time",
                        "n.risk",
                        "n.event",
                        "surv",
                        "std.err",
                        "lower",
                        "upper"
                    )])

                km_fit_df2 <- km_fit_df

                km_fit_df2$strata <- gsub(pattern = paste0(mycontexpl,"="),
                                             replacement = paste0(self$options$contexpl, " = "),
                                             x =km_fit_df2$strata)

                data_frame <- km_fit_df2

                survTable <- self$results$survTable
                survTable$deleteRows()
                private$.landmarkNote(self$results$survTable, results)


                for (i in seq_along(data_frame[, 1, drop = TRUE])) {
                    survTable$addRow(rowKey = i, values = c(data_frame[i, ]))
                }




                # survTableSummary 1,3,5-yr survival summary ----

                km_fit_df[, 1] <- gsub(
                    pattern = paste0(mycontexpl,"="),
                    replacement = paste0(self$options$contexpl, " is "),
                    x = km_fit_df[, 1]
                )


                km_fit_df %>%
                    dplyr::mutate(
                        description =
                            glue::glue(.(
                                'When {strata_val}, {time_val} {units} survival is {survival} [{ci_low}-{ci_high}, 95% CI].'),
                                strata_val = strata, time_val = time,
                                survival = scales::percent(surv),
                                ci_low = scales::percent(lower),
                                ci_high = scales::percent(upper),
                                units = self$options$timetypeoutput)
                    ) %>%
                    dplyr::select(description) %>%
                    dplyr::pull(.) -> survTableSummary

                survTableSummary <- htmltools::htmlEscape(survTableSummary)

                self$results$survTableSummary$setContent(survTableSummary)
            }


            ,
            # Person-Time Analysis Function ----
            .personTimeAnalysis = function(results) {
                # Check if person_time option is enabled
                if (!self$options$person_time) {
                    return()
                }

                # Extract data
                mytime <- results$name1time
                myoutcome <- results$analysis_outcome
                mydata <- results$cleanData


                # Ensure time is numeric
                mydata[[mytime]] <- jmvcore::toNumeric(mydata[[mytime]])

                # Get total observed time
                total_time <- sum(mydata[[mytime]])

                # Count only event-of-interest (competing events are censored when present)
                total_events <- sum(mydata[[myoutcome]] == 1, na.rm = TRUE)

                # Get time unit
                time_unit <- self$options$timetypeoutput

                # Get rate multiplier
                rate_multiplier <- self$options$rate_multiplier

                # Calculate overall incidence rate
                overall_rate <- (total_events / total_time) * rate_multiplier

                # Calculate confidence intervals using Poisson exact method
                ci_lower <- (stats::qchisq(0.025, 2*total_events) / 2) / total_time * rate_multiplier
                ci_upper <- (stats::qchisq(0.975, 2*(total_events + 1)) / 2) / total_time * rate_multiplier

                # Exact (Garwood) bounds: a row with almost no accrued person-time
                # genuinely cannot rule out a very high rate. Correct, but it looks
                # like a bug without a footnote saying so. Do not cap it.
                self$results$personTimeTable$setNote(
                    "ci",
                    .("Exact (Garwood) Poisson 95% CI. Rows with 0 events give a one-sided 97.5% upper bound; intervals with very little accrued person-time yield correspondingly wide bounds."))

                # Add to personTimeTable - first the overall row
                self$results$personTimeTable$deleteRows()
                private$.landmarkNote(self$results$personTimeTable, results)
                self$results$personTimeTable$addRow(rowKey=1, values=list(
                    interval=paste0("Overall (0-max)"),
                    events=total_events,
                    person_time=round(total_time, 2),
                    rate=round(overall_rate, 2),
                    rate_ci_lower=round(ci_lower, 2),
                    rate_ci_upper=round(ci_upper, 2)
                ))

                # Parse time intervals for stratified analysis
                # "[,\\s]+" is the regex [,\s]+, which inside a TRE bracket
                # expression means comma, literal backslash, or the letter "s" --
                # NOT whitespace. "12 36 60" therefore parsed to NA and every
                # boundary was silently dropped. Same class of bug as the one fixed
                # in .parseSurvivalTimePoints; use the same unambiguous pattern.
                raw_intervals <- suppressWarnings(as.numeric(unlist(strsplit(
                    self$options$time_intervals,
                    "[,[:space:]]+"
                ))))
                time_intervals <- sort(unique(raw_intervals[
                    is.finite(raw_intervals) & raw_intervals > 0 &
                        raw_intervals < max(mydata[[mytime]], na.rm = TRUE)
                ]))
                if (length(time_intervals) < length(raw_intervals)) {
                    private$.addHtmlMessage(
                        "warning",
                        .("Person-time intervals adjusted"),
                        .("Duplicate, non-positive, non-numeric, or out-of-range interval boundaries were ignored.")
                    )
                }

                if (length(time_intervals) > 0) {
                    # Create time intervals with configurable multiplier
                    max_time_extended <- private$.calculateTimeIntervals(mydata[[mytime]])
                    breaks <- c(0, time_intervals, max_time_extended)

                    # Loop through intervals
                    for (i in 1:(length(breaks)-1)) {
                        start_time <- breaks[i]
                        end_time <- breaks[i+1]

                        # Add checkpoint for responsiveness using configurable frequency
                        private$.performCheckpoint(i)

                        # Filter data for this interval
                        if (i == 1) {
                            # For first interval, include patients from the beginning
                            interval_data <- mydata
                            # But truncate follow-up time to the interval end
                            follow_up_times <- pmin(mydata[[mytime]], end_time)
                            # Count only event-of-interest inside interval
                            events_in_interval <- sum(mydata[[myoutcome]] == 1 & mydata[[mytime]] <= end_time, na.rm = TRUE)
                        } else {
                            # For later intervals, include only patients who survived past the previous cutpoint
                            survivors <- mydata[[mytime]] > start_time
                            interval_data <- mydata[survivors, ]

                            if (nrow(interval_data) == 0) {
                                # Skip if no patients in this interval
                                next
                            }

                            # Adjust entry time and follow-up time
                            adjusted_entry_time <- rep(start_time, nrow(interval_data))
                            adjusted_exit_time <- pmin(interval_data[[mytime]], end_time)
                            follow_up_times <- adjusted_exit_time - adjusted_entry_time

                            # Count only event-of-interest inside interval
                            events_in_interval <- sum(interval_data[[myoutcome]] == 1 &
                                                          interval_data[[mytime]] <= end_time &
                                                          interval_data[[mytime]] > start_time, na.rm = TRUE)
                        }

                        # Sum person-time in this interval
                        person_time_in_interval <- sum(follow_up_times)

                        # Calculate interval incidence rate
                        if (person_time_in_interval > 0) {
                            interval_rate <- (events_in_interval / person_time_in_interval) * rate_multiplier

                            # Calculate confidence intervals
                            if (events_in_interval > 0) {
                                interval_ci_lower <- (stats::qchisq(0.025, 2*events_in_interval) / 2) / person_time_in_interval * rate_multiplier
                                interval_ci_upper <- (stats::qchisq(0.975, 2*(events_in_interval + 1)) / 2) / person_time_in_interval * rate_multiplier
                            } else {
                                interval_ci_lower <- 0
                                interval_ci_upper <- (stats::qchisq(0.975, 2) / 2) / person_time_in_interval * rate_multiplier
                            }

                            # Add to personTimeTable
                            # The last break is max(time) * 1.1, an artefact of
                            # .calculateTimeIntervals() used to bound the pmin();
                            # printing it as the interval end advertised follow-up
                            # that does not exist (e.g. "60-385.715" when the
                            # longest observed time was 350.65). It is open-ended.
                            interval_label <- if (i == length(breaks) - 1)
                                paste0(start_time, "+") else paste0(start_time, "-", end_time)
                            self$results$personTimeTable$addRow(rowKey=i+1, values=list(
                                interval=interval_label,
                                events=events_in_interval,
                                person_time=round(person_time_in_interval, 2),
                                rate=round(interval_rate, 2),
                                rate_ci_lower=round(interval_ci_lower, 2),
                                rate_ci_upper=round(interval_ci_upper, 2)
                            ))
                        }
                    }
                }

                # Create summary text with interpretation
                event_scope <- if (private$.isCompetingRisk()) {
                    .("Event counts reflect the event of interest; competing events are treated as censored for rates.")
                } else if (self$options$multievent && self$options$analysistype == "overall") {
                    .("Event counts include all-cause events as defined in the outcome mapping.")
                } else {
                    .("Event counts reflect the specified event of interest.")
                }

                summary_html <- glue::glue(.(
                    '<h4>Person-Time Analysis Summary</h4>
                    <p>Total follow-up time: <b>{total_time_val} {units}</b></p>
                    <p>Number of events: <b>{events}</b></p>
                    <p>Overall incidence rate: <b>{rate}</b> per {multiplier} {units} [95% CI: {lower}-{upper}]</p>
                    <p>{scope}</p>
                    <p>This represents the rate at which events occurred in your study population. The incidence rate is calculated as the number of events divided by the total person-time at risk.</p>'),
                    total_time_val = round(total_time, 1), units = time_unit,
                    events = total_events, rate = round(overall_rate, 2),
                    multiplier = rate_multiplier,
                    lower = round(ci_lower, 2), upper = round(ci_upper, 2),
                    scope = event_scope)

                self$results$personTimeSummary$setContent(summary_html)
            }







            # Cut-off Plot ----
            ,
            .plot4 = function(image4, ggtheme, theme, ...) {

                if (!self$options$findcut) {
                    return()
                }

                plotData <- image4$state

                if (is.null(plotData)) {
                    return()
                }

                # if (plotData$not_continue_analysis) {
                #     return()
                # }

                private$.checkpoint()

                res.cut <- plotData$res.cut

                name3contexpl <- plotData$name3contexpl

                plot4 <-
                    plot(res.cut, name3contexpl, palette = "npg")

                print(plot4)
                TRUE
            }


            # Survival Curve with new cut-off ----
            ,
            .plot5 = function(image5, ggtheme, theme, ...) {



                if (!self$options$findcut) {
                    return()
                }

                plotData <- image5$state

                if (is.null(plotData)) {
                    return()
                }

                # if (plotData$not_continue_analysis) {
                #     return()
                # }

                private$.checkpoint()

                res.cat <- plotData$cutoffdata

                results <- plotData$results

                mytime <- results$name1time
                myoutcome <- results$analysis_outcome
                mycontexpl <- results$name3contexpl


                mytime <-
                    jmvcore::constructFormula(terms = mytime)

                myoutcome <-
                    jmvcore::constructFormula(terms = myoutcome)

                mycontexpl <-
                    jmvcore::constructFormula(terms = mycontexpl)


                # Escape variable names for safe formula construction
                escaped_time <- private$.escapeVariableNames(mytime)
                escaped_outcome <- private$.escapeVariableNames(myoutcome)
                escaped_contexpl <- private$.escapeVariableNames(mycontexpl)

                formula <-
                    paste('survival::Surv(',
                          escaped_time,
                          ',',
                          escaped_outcome,
                          ') ~ ',
                          escaped_contexpl)

                myformula <- .asSurvivalFormula(formula)


                fit <- .quietly(survminer::surv_fit(
                    formula = myformula,
                    data = res.cat
                ))

                # The "Plot Customization" panel advertises these controls for the
                # survival curves, but this plot -- the primary Kaplan-Meier output --
                # read only risk.table and conf.int. Plot End Time, Time Interval,
                # Y-axis Start/End, Censored observations and Median survival line
                # silently did nothing here, while .plot2/.plot3 already honoured
                # them. Applying them makes the panel mean what it says (and does
                # bound the x-axis at Plot End Time, as it already did elsewhere).
                plot5 <- .quietly(survminer::ggsurvplot(
                    fit,
                    data = res.cat,
                    xlab = private$.timeAxisLabel(),
                    break.time.by = private$.plotBy(),
                    xlim = c(0, private$.plotEndTime()),
                    ylim = c(self$options$ybegin_plot, self$options$yend_plot),
                    risk.table = self$options$risktable,
                    conf.int = self$options$ci95,
                    censor = self$options$censored,
                    surv.median.line = self$options$medianline
                ))
                .quietly(print(plot5))
                TRUE
            }


            # Cumulative Events with new cut-off ----
            # https://rpkgs.datanovia.com/survminer/survminer_cheatsheet.pdf
            ,
            .plot2 = function(image2, ggtheme, theme, ...) {

                if (!self$options$findcut) {
                    return()
                }

                if (!self$options$ce) {
                    return()
                }

                plotData <- image2$state


                if (is.null(plotData)) {
                    return()
                }

                # if (plotData$not_continue_analysis) {
                #     return()
                # }

                res.cat <- plotData$cutoffdata

                results <- plotData$results

                mytime <- results$name1time
                myoutcome <- results$analysis_outcome
                mycontexpl <- results$name3contexpl

                mytime <-
                    jmvcore::constructFormula(terms = mytime)

                myoutcome <-
                    jmvcore::constructFormula(terms = myoutcome)

                mycontexpl <-
                    jmvcore::constructFormula(terms = mycontexpl)


                myformula <-
                    paste0('survival::Surv(',
                          mytime,
                          ',',
                          myoutcome,
                          ')')

                title2 <- as.character(mycontexpl)

                plot2 <- res.cat %>%
                    finalfit::surv_plot(
                        .data = .,
                        dependent = myformula,
                        explanatory = mycontexpl,
                        xlab = private$.timeAxisLabel(),
                        # pval = TRUE,
                        legend = "none",
                        break.time.by = private$.plotBy(),
                        xlim = c(0, private$.plotEndTime()),
                        ylim = c(
                            self$options$ybegin_plot,
                            self$options$yend_plot),
                        title = paste0("Cumulative Events ", title2),
                        fun = "event",
                        risk.table = self$options$risktable,
                        conf.int = self$options$ci95,
                        censor = self$options$censored,
                        surv.median.line = self$options$medianline

                    )


                print(plot2)
                TRUE
            }



            # Cumulative Hazard with new cut-off ----
            ,
            .plot3 = function(image3, ggtheme, theme, ...) {

                if (!self$options$findcut) {
                    return()
                }

                if (!self$options$ch) {
                    return()
                }

                plotData <- image3$state

                if (is.null(plotData)) {
                    return()
                }

                # if (plotData$not_continue_analysis) {
                #     return()
                # }

                res.cat <- plotData$cutoffdata

                results <- plotData$results

                mytime <- results$name1time
                myoutcome <- results$analysis_outcome
                mycontexpl <- results$name3contexpl


                mytime <-
                    jmvcore::constructFormula(terms = mytime)

                myoutcome <-
                    jmvcore::constructFormula(terms = myoutcome)

                mycontexpl <-
                    jmvcore::constructFormula(terms = mycontexpl)


                myformula <-
                    paste('survival::Surv(',
                          mytime,
                          ',',
                          myoutcome,
                          ')')

                title2 <- as.character(mycontexpl)

                plot3 <- res.cat %>%
                    finalfit::surv_plot(
                        .data = .,
                        dependent = myformula,
                        explanatory = mycontexpl,
                        xlab = private$.timeAxisLabel(),
                        ylab = "Cumulative Hazard",
                        # pval = TRUE,
                        legend = "none",
                        break.time.by = private$.plotBy(),
                        xlim = c(0, private$.plotEndTime()),
                        # For cumulative hazard, use NULL to allow auto-scaling beyond 1.0
                        ylim = NULL,
                        title = paste0("Cumulative Hazard ", title2),
                        fun = "cumhaz",
                        risk.table = self$options$risktable,
                        conf.int = self$options$ci95,
                        censor = self$options$censored,
                        surv.median.line = self$options$medianline
                    )


                print(plot3)
                TRUE
            }


            # KMunicate Style with new cut-off ----
            ,
            .plot6 = function(image6, ggtheme, theme, ...) {

                if (!self$options$findcut) {
                    return()
                }

                if (!self$options$kmunicate) {
                    return()
                }

                plotData <- image6$state

                if (is.null(plotData)) {
                    return()
                }

                # if (plotData$not_continue_analysis) {
                #     return()
                # }

                res.cat <- plotData$cutoffdata

                results <- plotData$results

                mytime <- results$name1time
                myoutcome <- results$analysis_outcome
                mycontexpl <- results$name3contexpl


                mytime <-
                    jmvcore::constructFormula(terms = mytime)

                myoutcome <-
                    jmvcore::constructFormula(terms = myoutcome)

                mycontexpl <-
                    jmvcore::constructFormula(terms = mycontexpl)


                myformula <-
                    paste('survival::Surv(',
                          mytime,
                          ',',
                          myoutcome,
                          ') ~ ',
                          mycontexpl)

                myformula <- .asSurvivalFormula(myformula)

                # myformula <-
                #     paste0("survival::Surv(mytime, myoutcome) ~ ", contfactor)

                km_fit <- survival::survfit(myformula, data = res.cat)

                time_scale <-
                    seq(0, private$.plotEndTime(), by = private$.plotBy())


                plot6 <-
                    KMunicate::KMunicate(
                        fit = km_fit,
                        time_scale = time_scale,
                        .xlab = paste0("Time in ", self$options$timetypeoutput)
                    )


                print(plot6)
                TRUE
            }

            # Multiple Cut-offs Analysis ----
            ,
            .multipleCutoffs = function(results) {
                # On failure this records WHY in private$.multicutFailReason and
                # returns NULL; .run() folds the reason into its single
                # "Multiple cut-offs unavailable" notice. (Formerly bare warning()
                # calls, which reached the Analysis Notes panel as a second,
                # differently worded copy of that notice.)
                fail <- function(reason) {
                    private$.multicutFailReason <- reason
                    NULL
                }
                tryCatch({
                    mytime <- results$name1time
                    myoutcome <- results$analysis_outcome
                    mycontexpl <- results$name3contexpl
                    mydata <- results$cleanData

                    # Convert to numeric
                    mydata[[mytime]] <- jmvcore::toNumeric(mydata[[mytime]])

                    # Extract continuous variable values
                    cont_var <- mydata[[mycontexpl]]
                    if (is.null(cont_var)) {
                        return(fail(sprintf(
                            .("the continuous variable '%s' was not found in the analysis data"),
                            self$options$contexpl)))
                    }

                    cont_var <- cont_var[!is.na(cont_var)]

                    # Check if we have enough data
                    if (length(cont_var) < 10) {
                        return(fail(sprintf(
                            .("only %d non-missing value(s) of '%s' are available; at least 10 are required"),
                            length(cont_var), self$options$contexpl)))
                    }






                    # Determine number of cutoffs
                    num_cuts <- switch(self$options$num_cutoffs,
                                       "two" = 2,
                                       "three" = 3,
                                       "four" = 4)

                    # Calculate cutoffs based on method
                    cutoff_values <- switch(self$options$cutoff_method,
                        "quantile" = private$.quantileCutoffs(cont_var, num_cuts),
                        "recursive" = private$.recursiveCutoffs(mydata, mytime, myoutcome, mycontexpl, num_cuts),
                        "tree" = private$.treeCutoffs(mydata, mytime, myoutcome, mycontexpl, num_cuts),
                        "minpval" = private$.minPvalueCutoffs(mydata, mytime, myoutcome, mycontexpl, num_cuts)
                    )

                    # Check if cutoffs were successfully calculated
                    if (is.null(cutoff_values) || length(cutoff_values) == 0) {
                        return(fail(sprintf(
                            .("the '%s' method returned no cut-off value"),
                            self$options$cutoff_method)))
                    }

                    # Sanitise, then ENFORCE the minimum group size.
                    #
                    # "Minimum Group Size (%)" was only ever honoured by the single
                    # cut-off path (survminer::surv_cutpoint's minprop). None of the
                    # four multiple-cut-off methods applied it to the resulting
                    # groups: on a 240-patient test set with the minimum set to 10%
                    # (24 patients), minimum-p-value produced groups of 182/3/55,
                    # recursive 109/14/117 and tree 19/90/131. A 3-patient "risk
                    # group" carries a median survival and a hazard ratio into a
                    # clinical report with no indication that it is unusable.
                    #
                    # Fixing it here rather than in each method: all four route
                    # through this call site, and the constraint is a property of the
                    # partition, not of how the candidate cut-points were found.
                    # Dropping non-finite/duplicate values here also disarms
                    # .treeCutoffs' `unique(cutoffs)[1:num_cuts]`, which pads with NA
                    # when de-duplication leaves fewer than num_cuts values.
                    cutoff_values <- sort(unique(cutoff_values[is.finite(cutoff_values)]))
                    if (length(cutoff_values) == 0) {
                        return(fail(sprintf(
                            .("the '%s' method returned no finite cut-off value"),
                            self$options$cutoff_method)))
                    }

                    min_n <- ceiling(length(cont_var) * self$options$min_group_size / 100)
                    n_requested <- length(cutoff_values)
                    repeat {
                        sizes <- as.integer(table(cut(
                            mydata[[mycontexpl]],
                            breaks = c(-Inf, cutoff_values, Inf)
                        )))
                        if (length(cutoff_values) <= 1 || all(sizes >= min_n))
                            break
                        # Remove the cut-point bounding the smallest group: the one
                        # whose removal merges it into a neighbour.
                        smallest <- which.min(sizes)
                        drop_at <- if (smallest == 1) 1L else smallest - 1L
                        cutoff_values <- cutoff_values[-drop_at]
                    }

                    # The loop stops at one cut-off even if that cut-off still
                    # leaves an undersized group, so the reduction notice alone could
                    # imply a constraint that was not actually met.
                    final_sizes <- as.integer(table(cut(
                        mydata[[mycontexpl]], breaks = c(-Inf, cutoff_values, Inf))))
                    if (min(final_sizes) < min_n)
                        private$.addHtmlMessage(
                            "warning",
                            .("Minimum group size not met"),
                            sprintf(
                                .("The smallest reported group still has %d patient(s) (%.1f%%), below the %g%% minimum. No further cut-off can be removed without collapsing the grouping entirely. Interpret group-level estimates with caution."),
                                min(final_sizes), 100 * min(final_sizes) / length(cont_var),
                                self$options$min_group_size))

                    if (length(cutoff_values) < n_requested)
                        private$.addHtmlMessage(
                            "warning",
                            .("Cut-offs reduced to respect minimum group size"),
                            sprintf(
                                .("%d of the %d requested cut-off(s) produced a group smaller than the %g%% minimum (%d patients) and were removed. %d cut-off(s) defining %d group(s) are reported. Lower the minimum group size, request fewer cut-offs, or use the continuous Cox model."),
                                n_requested - length(cutoff_values), n_requested,
                                self$options$min_group_size, min_n,
                                length(cutoff_values), length(cutoff_values) + 1))

                    # Create risk groups
                    risk_groups <- private$.createRiskGroups(mydata[[mycontexpl]], cutoff_values)


                    # Calculate survival statistics for each group
                    group_stats <- private$.calculateGroupStats(mydata, mytime, myoutcome, risk_groups)

                    return(list(
                        cutoff_values = cutoff_values,
                        risk_groups = risk_groups,
                        group_stats = group_stats,
                        method = self$options$cutoff_method,
                        num_cuts = length(cutoff_values),
                        original_data = mydata,
                        mytime = mytime,
                        myoutcome = myoutcome,
                        mycontexpl = mycontexpl
                    ))
                }, error = function(e) {
                    fail(sprintf(.("the analysis stopped with an error: %s"), conditionMessage(e)))
                })
            }

            # Quantile-based cutoffs ----
            ,
            .quantileCutoffs = function(cont_var, num_cuts) {
                # Equally spaced interior quantiles for ANY number of cuts.
                #
                # This previously had branches only for num_cuts 2, 3 and 4, so
                # `quantiles` was never assigned for num_cuts == 1 and the call
                # below threw "object 'quantiles' not found". That throw escaped
                # the inner tryCatch in .recursiveCutoffs and took out the whole
                # multiple-cut-off feature -- no tables, no plot, no output
                # column -- leaving only a warning() the user never sees.
                num_cuts <- as.integer(num_cuts)
                if (is.na(num_cuts) || num_cuts < 1) return(numeric(0))

                quantiles <- seq_len(num_cuts) / (num_cuts + 1)
                cutoffs <- stats::quantile(cont_var, probs = quantiles, na.rm = TRUE)
                return(unname(as.numeric(cutoffs)))
            }

            # Recursive optimal cutoffs ----
            ,
            .recursiveCutoffs = function(mydata, mytime, myoutcome, mycontexpl, num_cuts) {
                if (!requireNamespace("survminer", quietly = TRUE)) {
                    return(private$.quantileCutoffs(mydata[[mycontexpl]], num_cuts))
                }

                cutoffs <- numeric(num_cuts)
                current_data <- mydata

                for (i in seq_len(num_cuts)) {
                    private$.performCheckpoint(i, frequency = 1)
                    fit_err <- tryCatch({
                        res.cut <- .quietly(survminer::surv_cutpoint(
                            current_data,
                            time = mytime,
                            event = myoutcome,
                            variables = mycontexpl,
                            minprop = self$options$min_group_size / 100
                        ))

                        cutoffs[i] <- summary(res.cut)$cutpoint

                        # Remove data around cutpoint for next iteration
                        if (i < num_cuts) {
                            cutoff_val <- cutoffs[i]
                            margin <- 0.1 * sd(current_data[[mycontexpl]], na.rm = TRUE)
                            current_data <- current_data[
                                abs(current_data[[mycontexpl]] - cutoff_val) > margin,
                            ]
                        }
                        NULL
                    }, error = function(e) e)
                    if (!is.null(fit_err)) {
                        # Fallback to quantile method (applied here with `<-`
                        # instead of `<<-` from inside the error handler).
                        remaining_cuts <- num_cuts - i + 1
                        fallback_cuts <- private$.quantileCutoffs(current_data[[mycontexpl]], remaining_cuts)
                        cutoffs[i:num_cuts] <- fallback_cuts
                        private$.addHtmlMessage(
                            "warning",
                            .("Recursive cut-off search incomplete"),
                            .("A recursive optimal split could not be estimated. Quantile-based cut-points were used for the remaining groups.")
                        )
                        break
                    }
                }

                return(sort(cutoffs))
            }

            # Tree-based partitioning ----
            ,
            .treeCutoffs = function(mydata, mytime, myoutcome, mycontexpl, num_cuts) {
                if (!requireNamespace("rpart", quietly = TRUE)) {
                    return(private$.quantileCutoffs(mydata[[mycontexpl]], num_cuts))
                }

                tryCatch({
                    # Prepare survival formula with escaped variable names
                    escaped_time <- private$.escapeVariableNames(mytime)
                    escaped_outcome <- private$.escapeVariableNames(myoutcome)
                    escaped_contexpl <- private$.escapeVariableNames(mycontexpl)

                    formula_str <- paste0("survival::Surv(", escaped_time, ", ", escaped_outcome, ") ~ ", escaped_contexpl)
                    formula <- .asSurvivalFormula(formula_str)

                    # Fit survival tree with specified depth
                    tree_fit <- rpart::rpart(
                        formula,
                        data = mydata,
                        method = "exp",
                        control = rpart::rpart.control(
                            maxdepth = num_cuts + 1,
                            minsplit = max(10, nrow(mydata) * self$options$min_group_size / 100),
                            cp = 0.01
                        )
                    )

                    # Extract split points
                    splits <- tree_fit$splits
                    if (is.null(splits) || nrow(splits) == 0) {
                        return(private$.quantileCutoffs(mydata[[mycontexpl]], num_cuts))
                    }

                    # rpart stores the splitting variable in rownames(splits),
                    # not in a column. The old test `splits[,1] == mycontexpl`
                    # compared the numeric `count` column against the variable
                    # NAME (e.g. "228" == "Age"), so it was FALSE on every row and
                    # `cutoffs` was always numeric(0). The rpart fit above was
                    # computed and thrown away, and the block below quietly
                    # substituted quantile cut-points -- making "tree" a silent
                    # alias for "quantile".
                    #
                    # Keep primary continuous splits only (ncat != 0 marks
                    # categorical splits, whose `index` is a category code rather
                    # than a cut-point).
                    split_rows <- rownames(splits)
                    is_var <- if (is.null(split_rows)) rep(FALSE, nrow(splits))
                              else split_rows == mycontexpl
                    # In rpart, a continuous split has abs(ncat) == 1 (the sign is
                    # the direction); anything larger is a categorical split whose
                    # `index` is a category code, not a cut-point.
                    if ("ncat" %in% colnames(splits))
                        is_var <- is_var & abs(splits[, "ncat"]) == 1
                    is_var[is.na(is_var)] <- FALSE

                    cutoffs <- if (any(is_var))
                        sort(unique(as.numeric(splits[is_var, "index"]))) else numeric(0)

                    if (length(cutoffs) == 0) {
                        # Say so rather than silently returning quantiles under a
                        # "tree" label.
                        private$.addHtmlMessage(
                            "warning",
                            .("Tree-based cut-points unavailable"),
                            jmvcore::format(.("The survival tree produced no usable split for '{var}'. Quantile cut-points are shown instead. This usually means the tree found no split meeting the complexity and minimum group-size criteria."), var = mycontexpl))
                    }

                    if (length(cutoffs) > num_cuts) {
                        # Keep the tree's STRONGEST splits, not the numerically
                        # smallest ones. rpart reports an `improve` column that was
                        # never read; sorting by cut-point value and taking the first
                        # num_cuts discarded the root split. On a test cohort with
                        # hazard steps at 5, 10 and 31 the root split (index 30.94,
                        # improve 385.11) was dropped in favour of 5.03 and 9.96
                        # (improve 139.12 and 22.64). Because maxdepth is num_cuts + 1
                        # the tree normally returns more splits than requested, so
                        # this truncation was the common path and biased every
                        # tree-derived grouping toward low marker values.
                        var_splits <- splits[is_var, , drop = FALSE]
                        if ("improve" %in% colnames(var_splits)) {
                            keep <- order(var_splits[, "improve"], decreasing = TRUE)[seq_len(num_cuts)]
                            cutoffs <- sort(unique(as.numeric(var_splits[keep, "index"])))
                        } else {
                            cutoffs <- cutoffs[1:num_cuts]
                        }
                    } else if (length(cutoffs) < num_cuts) {
                        # Supplement with quantile cutoffs
                        additional_cuts <- private$.quantileCutoffs(mydata[[mycontexpl]], num_cuts - length(cutoffs))
                        cutoffs <- sort(c(cutoffs, additional_cuts))
                        cutoffs <- unique(cutoffs)[1:num_cuts]
                    }

                    return(cutoffs)
                }, error = function(e) {
                    return(private$.quantileCutoffs(mydata[[mycontexpl]], num_cuts))
                })
            }

            # Minimum p-value cutoffs ----
            ,
            .minPvalueCutoffs = function(mydata, mytime, myoutcome, mycontexpl, num_cuts) {
                cont_var <- mydata[[mycontexpl]]
                # Make search reproducible with a user-configurable seed
                # (defaults to 12345). withr::local_seed sets the RNG here and
                # restores the previous state when this method returns, without
                # manually touching the global environment.
                seed_val <- self$options$seed
                if (is.null(seed_val)) seed_val <- 12345
                withr::local_seed(seed_val)
                sorted_vals <- sort(unique(cont_var))

                # Trim candidate cut-points by OBSERVATION rank, not by position in
                # the unique-value vector.
                #
                # min_n counts patients but was used to index sorted_vals, which
                # counts distinct values. For an ordinal marker with, say, 8 distinct
                # values in 200 patients, 8 <= 2*20+1 held and the method returned
                # quantile cut-points -- while multipleCutTable, the plot and the
                # method note all still said "Minimum P-value". Quantiles of the
                # observations give the same guarantee without conflating the two
                # counts.
                min_prop <- self$options$min_group_size / 100
                lo <- stats::quantile(cont_var, min_prop, na.rm = TRUE, names = FALSE)
                hi <- stats::quantile(cont_var, 1 - min_prop, na.rm = TRUE, names = FALSE)
                valid_cuts <- sorted_vals[sorted_vals >= lo & sorted_vals <= hi]

                if (length(valid_cuts) < num_cuts) {
                    private$.addHtmlMessage(
                        "warning",
                        .("Minimum p-value search not possible"),
                        sprintf(
                            .("'%s' has too few distinct values between the %g%% and %g%% quantiles to place %d cut-off(s). Quantile-based cut-points are shown instead of minimum-p-value cut-points."),
                            self$options$contexpl, self$options$min_group_size,
                            100 - self$options$min_group_size, num_cuts))
                    return(private$.quantileCutoffs(cont_var, num_cuts))
                }

                # Enumerate every combination when the candidate space is small.
                # For larger spaces, evaluate 1000 UNIQUE combinations selected with
                # the user-controlled seed. The previous loop sampled independently,
                # repeated combinations, and could miss the optimum even when fewer
                # than 1000 combinations existed.
                total_combinations <- choose(length(valid_cuts), num_cuts)
                exhaustive <- is.finite(total_combinations) && total_combinations <= 1000
                if (exhaustive) {
                    candidate_sets <- utils::combn(
                        valid_cuts,
                        num_cuts,
                        simplify = FALSE
                    )
                } else {
                    target <- 1000L
                    candidate_sets <- vector("list", target)
                    seen <- new.env(hash = TRUE, parent = emptyenv())
                    n_found <- 0L
                    attempts <- 0L
                    max_attempts <- 100000L

                    while (n_found < target && attempts < max_attempts) {
                        attempts <- attempts + 1L
                        idx <- sort(sample.int(length(valid_cuts), num_cuts))
                        key <- paste(idx, collapse = ",")
                        if (!exists(key, envir = seen, inherits = FALSE)) {
                            n_found <- n_found + 1L
                            candidate_sets[[n_found]] <- valid_cuts[idx]
                            assign(key, TRUE, envir = seen)
                        }
                        private$.performCheckpoint(attempts, frequency = 250)
                    }
                    candidate_sets <- candidate_sets[seq_len(n_found)]

                    private$.addHtmlMessage(
                        "info",
                        .("Approximate minimum p-value search"),
                        sprintf(
                            .("The candidate space contains %s cut-off combinations. A reproducible random sample of %d unique combinations was evaluated using seed %d. The reported solution is approximate and must be validated independently."),
                            base::format(total_combinations, scientific = total_combinations >= 1e6,
                                         digits = 4, trim = TRUE),
                            length(candidate_sets),
                            seed_val
                        )
                    )
                }

                best_pval <- Inf
                best_cuts <- NULL
                min_n <- ceiling(length(cont_var) * min_prop)

                # Hoist the iteration-invariant formula and base data frame out of the loop;
                # only the grouping vector changes each iteration.
                escaped_time <- private$.escapeVariableNames(mytime)
                escaped_outcome <- private$.escapeVariableNames(myoutcome)
                formula_str <- paste0("survival::Surv(", escaped_time, ", ", escaped_outcome, ") ~ test_groups")
                formula <- .asSurvivalFormula(formula_str)
                test_data <- mydata

                tryCatch({
                    for (i in seq_along(candidate_sets)) {
                        private$.performCheckpoint(i, frequency = 25)
                        test_cuts <- sort(candidate_sets[[i]])
                        test_groups <- private$.createRiskGroups(cont_var, test_cuts)

                        # Enforce the requested group-size constraint during the
                        # optimization. Removing a selected cut-point afterwards can
                        # produce a grouping that was never optimized.
                        group_sizes <- as.integer(table(test_groups))
                        if (length(group_sizes) != num_cuts + 1L ||
                            any(group_sizes < min_n)) {
                            next
                        }

                        # Calculate log-rank test p-value
                        test_data$test_groups <- test_groups

                        pval <- tryCatch({
                            logrank_test <- survival::survdiff(formula, data = test_data)
                            stats::pchisq(
                                logrank_test$chisq,
                                df = length(logrank_test$n) - 1,
                                lower.tail = FALSE
                            )
                        }, error = function(e) NA_real_)

                        if (is.finite(pval) && pval < best_pval) {
                            best_pval <- pval
                            best_cuts <- test_cuts
                        }
                    }

                    if (is.null(best_cuts)) {
                        private$.addHtmlMessage(
                            "warning",
                            .("Minimum p-value search found no admissible split"),
                            .("No evaluated combination both satisfied the minimum group-size requirement and produced a finite log-rank statistic. Quantile-based cut-points are shown instead."))
                        return(private$.quantileCutoffs(cont_var, num_cuts))
                    }

                    return(best_cuts)
                }, error = function(e) {
                    private$.addHtmlMessage(
                        "warning",
                        .("Minimum p-value search failed"),
                        sprintf(.("The minimum-p-value search could not be completed (%s). Quantile-based cut-points are shown instead."), conditionMessage(e)))
                    return(private$.quantileCutoffs(cont_var, num_cuts))
                })
            }

            # Create risk groups from cutoffs ----
            ,
            .createRiskGroups = function(cont_var, cutoffs) {
                # Labels describe the MARKER VALUE, not risk.
                #
                # These groups are formed purely by ordering the biomarker, with
                # no reference to the hazard direction. Calling the lowest-value
                # group "Low Risk" is therefore an assumption that high marker
                # values are harmful -- and it is exactly backwards for a
                # protective marker. On a test dataset where high values were
                # strongly protective, the group carrying 76/80 events was
                # labelled "Low Risk" and the group with 19/80 events "High
                # Risk". Neutral value-based labels cannot be wrong in that way;
                # the hazard ratios in the tables tell the reader the direction.
                if (length(cutoffs) == 1) {
                    # Reachable since the minimum-group-size enforcement in
                    # .multipleCutoffs can reduce a request to a single cut-off.
                    # Without this branch it fell through to the generic cut()
                    # fallback and relabelled the groups "Group 1"/"Group 2".
                    groups <- ifelse(cont_var <= cutoffs[1], "Low marker", "High marker")
                    level_order <- c("Low marker", "High marker")
                } else if (length(cutoffs) == 2) {
                    groups <- ifelse(cont_var <= cutoffs[1], "Low marker",
                                   ifelse(cont_var <= cutoffs[2], "Middle marker", "High marker"))
                    level_order <- c("Low marker", "Middle marker", "High marker")
                } else if (length(cutoffs) == 3) {
                    groups <- ifelse(cont_var <= cutoffs[1], "Lowest marker",
                                   ifelse(cont_var <= cutoffs[2], "Low-middle marker",
                                         ifelse(cont_var <= cutoffs[3], "High-middle marker", "Highest marker")))
                    level_order <- c("Lowest marker", "Low-middle marker", "High-middle marker", "Highest marker")
                } else if (length(cutoffs) == 4) {
                    groups <- ifelse(cont_var <= cutoffs[1], "Lowest marker",
                                   ifelse(cont_var <= cutoffs[2], "Low marker",
                                         ifelse(cont_var <= cutoffs[3], "Middle marker",
                                               ifelse(cont_var <= cutoffs[4], "High marker", "Highest marker"))))
                    level_order <- c("Lowest marker", "Low marker", "Middle marker", "High marker", "Highest marker")
                } else {
                    # Fallback for other numbers of cutoffs
                    groups <- cut(cont_var, breaks = c(-Inf, cutoffs, Inf),
                                labels = paste("Group", 1:(length(cutoffs) + 1)))
                    level_order <- paste("Group", 1:(length(cutoffs) + 1))
                }

                # Filter level_order to only include levels that actually exist in the data
                existing_levels <- intersect(level_order, unique(groups))
                return(factor(groups, levels = existing_levels))
            }

            # Calculate survival statistics by group ----
            ,
            .calculateGroupStats = function(mydata, mytime, myoutcome, risk_groups) {
                stats_list <- list()

                for (group in levels(risk_groups)) {
                    group_data <- mydata[risk_groups == group, ]

                    if (nrow(group_data) > 0) {
                        # Calculate median survival
                        escaped_time <- private$.escapeVariableNames(mytime)
                        escaped_outcome <- private$.escapeVariableNames(myoutcome)
                        formula_str <- paste0("survival::Surv(", escaped_time, ", ", escaped_outcome, ") ~ 1")
                        formula <- .asSurvivalFormula(formula_str)

                        km_fit <- survival::survfit(formula, data = group_data)

                        # Extract median survival statistics safely
                        surv_summary <- summary(km_fit)
                        median_val <- if (!is.null(surv_summary$table)) {
                            surv_summary$table["median"]
                        } else {
                            NA
                        }

                        lower_val <- if (!is.null(surv_summary$table)) {
                            surv_summary$table["0.95LCL"]
                        } else {
                            NA
                        }

                        upper_val <- if (!is.null(surv_summary$table)) {
                            surv_summary$table["0.95UCL"]
                        } else {
                            NA
                        }

                            stats_list[[group]] <- list(
                                group = group,
                                n = nrow(group_data),
                                events = sum(group_data[[myoutcome]] == 1, na.rm = TRUE),
                            median_surv = as.numeric(median_val),
                            median_lower = as.numeric(lower_val),
                            median_upper = as.numeric(upper_val),
                            surv_fit = km_fit  # Store the survival fit object for time-specific survival calculations
                        )
                    }
                }

                return(stats_list)
            }

            # Populate multiple cutoffs tables ----
            ,
            .multipleCutoffTables = function(multicut_results) {
                # Check if results are valid
                if (is.null(multicut_results) ||
                    is.null(multicut_results$cutoff_values) ||
                    is.null(multicut_results$group_stats)) {
                    return()
                }

                # Populate cut-off points table (without statistical columns)
                cutoff_table <- self$results$multipleCutTable
                cutoff_table$deleteRows()  # Clear existing rows

                # Name the group this cut-off opens using the same vocabulary as
                # every other table ("Low marker", "Middle marker", ...). It used to
                # read "Group 2"/"Group 3", which matched nothing else on screen.
                group_labels <- levels(multicut_results$risk_groups)
                for (i in seq_along(multicut_results$cutoff_values)) {
                    cutoff_table$addRow(rowKey = i, values = list(
                        cutpoint_number = i,
                        cutpoint_value = round(multicut_results$cutoff_values[i], 2),
                        group_created = if (length(group_labels) >= i + 1)
                            group_labels[i + 1] else paste("Group", i + 1)
                    ))
                }
                cutoff_table$setNote("multiplicity", .("Warning: Multiple cut-off searches inflate type I error; treat p-values as exploratory and validate externally."))

                # Calculate and display overall log-rank test as separate text
                if (!is.null(multicut_results$risk_groups) && length(unique(multicut_results$risk_groups)) > 1) {
                    # Get the original data
                    mydata <- multicut_results$original_data
                    mydata$risk_groups <- multicut_results$risk_groups
                    mytime <- multicut_results$mytime
                    myoutcome <- multicut_results$myoutcome
                    mycontexpl <- multicut_results$mycontexpl

                    # Perform log-rank test comparing all groups
                    escaped_time <- private$.escapeVariableNames(mytime)
                    escaped_outcome <- private$.escapeVariableNames(myoutcome)
                    formula_str <- paste0("survival::Surv(", escaped_time, ", ", escaped_outcome, ") ~ risk_groups")
                    tryCatch({
                        logrank_test <- survival::survdiff(.asSurvivalFormula(formula_str), data = mydata)
                        overall_chisq <- logrank_test$chisq
                        overall_pval <- stats::pchisq(logrank_test$chisq, df = length(logrank_test$n) - 1, lower.tail = FALSE)

                        # Set the log-rank test results as text
                        logrank_text <- paste0("Overall Log-rank Test: \u03c7\u00b2 = ", round(overall_chisq, 3),
                                             " (df = ", length(logrank_test$n) - 1, "), p = ",
                                             ifelse(overall_pval < 0.001, "< 0.001", round(overall_pval, 3)))

                        interpretation <- .("The log-rank p above is EXPLORATORY: the cut-points were selected from these same data, so it is the largest of many statistics reported as a single test and is optimistic. No selection-adjusted p-value is reported: a single-split maximally selected-rank test does not adjust a multiple-cutoff procedure. Validate every cut-point in independent data before clinical use.")

                        full_text <- paste(logrank_text, interpretation, sep = "\n\n")

                        # Store in a text result (we'll need to check what text output is available)
                        # For now, let's use a preformatted result
                        if (!is.null(self$results$multipleCutTable)) {
                            # We can add a note to the table or create a separate text output
                            # Let's add it as a note for now
                            self$results$multipleCutTable$setNote("logrank", full_text)
                        }

                    }, error = function(e) {
                        # Log-rank test failed - silently continue
                    })
                }

                # Populate median survival table
                median_table <- self$results$multipleMedianTable
                median_table$deleteRows()  # Clear existing rows

                for (group_name in names(multicut_results$group_stats)) {
                    stats <- multicut_results$group_stats[[group_name]]
                    if (!is.null(stats)) {
                        median_table$addRow(rowKey = group_name, values = list(
                            risk_group = stats$group,
                            n_patients = stats$n,
                            events = stats$events,
                            median_survival = if(is.na(stats$median_surv)) "NR" else round(stats$median_surv, 1),
                            median_lower = if(is.na(stats$median_lower)) NA else round(stats$median_lower, 1),
                            median_upper = if(is.na(stats$median_upper)) NA else round(stats$median_upper, 1)
                        ))
                    }
                }

                # Populate survival estimates table
                survtable <- self$results$multipleSurvTable
                survtable$deleteRows()  # Clear existing rows

                # Calculate survival at flexible time points (defaults to 1, 3, 5 years)
                time_points <- private$.parseSurvivalTimePoints(
                    self$options$cutp,
                    default_points = private$.getDefaultCutpoints()
                )

                for (group_name in names(multicut_results$group_stats)) {
                    stats <- multicut_results$group_stats[[group_name]]
                    if (!is.null(stats) && !is.null(stats$surv_fit)) {
                        for (time_point in time_points) {
                            tryCatch({
                                # Extract survival probability at specific time point
                                surv_summary <- summary(stats$surv_fit, times = time_point)

                                if (length(surv_summary$surv) > 0) {
                                    # Don't multiply by 100 - the YAML format: pc does this automatically
                                    surv_prob <- surv_summary$surv[1]  # Keep as proportion (0-1)
                                    lower_ci <- surv_summary$lower[1]
                                    upper_ci <- surv_summary$upper[1]
                                    n_at_risk <- surv_summary$n.risk[1]

                                    survtable$addRow(rowKey = paste(group_name, time_point, sep = "_"),
                                                   values = list(
                                        risk_group = stats$group,
                                        time_point = time_point,
                                        n_risk = n_at_risk,  # Match YAML column name
                                        survival_prob = round(surv_prob, 3),  # Keep as proportion
                                        surv_lower = round(lower_ci, 3),  # Match YAML column name
                                        surv_upper = round(upper_ci, 3)   # Match YAML column name
                                    ))
                                } else {
                                    # No survival data available at this time point
                                    survtable$addRow(rowKey = paste(group_name, time_point, sep = "_"),
                                                   values = list(
                                        risk_group = stats$group,
                                        time_point = time_point,
                                        n_risk = 0,  # Match YAML column name
                                        survival_prob = NA,
                                        surv_lower = NA,  # Match YAML column name
                                        surv_upper = NA   # Match YAML column name
                                    ))
                                }
                            }, error = function(e) {
                                # Survival calculation failed - silently continue
                            })
                        }
                    }
                }
            }

            # Multiple cutoffs visualization ----
            ,
            .plotMultipleCutoffs = function(image, ggtheme, theme, ...) {
                if (!self$options$multiple_cutoffs) {
                    return()
                }

                # Get the stored multiple cutoffs results
                plotData <- image$state
                if (is.null(plotData) || is.null(plotData$cutoff_values) ||
                    is.null(plotData$values)) {
                    # Create fallback visualization
                    plot <- ggplot2::ggplot() +
                        ggplot2::geom_text(ggplot2::aes(x = 0.5, y = 0.5,
                                                      label = "Multiple Cutoffs Analysis\nRun analysis to see visualization"),
                                          size = 6) +
                        ggplot2::xlim(0, 1) + ggplot2::ylim(0, 1) +
                        ggplot2::theme_void()
                    print(plot)
                    return(TRUE)
                }

                tryCatch({
                    cont_var <- plotData$values
                    cutoff_values <- plotData$cutoff_values

                    # Create histogram with cutoff lines
                    hist_data <- data.frame(values = cont_var)

                    plot <- ggplot2::ggplot(hist_data, ggplot2::aes(x = values)) +
                        ggplot2::geom_histogram(bins = 30, alpha = 0.7, fill = "lightblue", color = "black") +
                        ggplot2::geom_vline(
                            xintercept = cutoff_values,
                            color = "red",
                            linetype = "dashed",
                            linewidth = 1
                        ) +
                        ggplot2::labs(
                            title = paste0("Multiple Cut-offs for ", self$options$contexpl),
                            subtitle = paste0("Method: ", plotData$method,
                                            " | Number of cut-offs: ", length(cutoff_values)),
                            x = self$options$contexpl,
                            y = "Frequency"
                        ) +
                        ggplot2::theme_minimal() +
                        ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                                      plot.subtitle = ggplot2::element_text(hjust = 0.5))

                    # Add cutoff value annotations
                    for (i in seq_along(cutoff_values)) {
                        plot <- plot + ggplot2::annotate("text",
                                                        x = cutoff_values[i],
                                                        y = Inf,
                                                        label = paste0("Cut ", i, ": ", round(cutoff_values[i], 2)),
                                                        vjust = 1.2,
                                                        color = "red",
                                                        size = 3,
                                                        angle = 90)
                    }

                    print(plot)
                }, error = function(e) {
                    # Fallback plot in case of error
                    plot <- ggplot2::ggplot() +
                        ggplot2::geom_text(ggplot2::aes(x = 0.5, y = 0.5,
                                                      label = "Multiple Cutoffs Visualization\nError in plot generation"),
                                          size = 6) +
                        ggplot2::xlim(0, 1) + ggplot2::ylim(0, 1) +
                        ggplot2::theme_void()
                    print(plot)
                })

                TRUE
            }

            # Multiple cutoffs survival plot ----
            ,
            .plotMultipleSurvival = function(image, ggtheme, theme, ...) {
                if (!self$options$multiple_cutoffs || !self$options$sc) {
                    return()
                }

                # Get the stored multiple cutoffs results
                plotData <- image$state
                if (is.null(plotData) || is.null(plotData$time) ||
                    is.null(plotData$outcome) || is.null(plotData$risk_groups)) {
                    plot <- ggplot2::ggplot() +
                        ggplot2::geom_text(ggplot2::aes(x = 0.5, y = 0.5,
                                                      label = "Multiple Cutoffs Survival Plot\nRun analysis to see visualization"),
                                          size = 6) +
                        ggplot2::xlim(0, 1) + ggplot2::ylim(0, 1) +
                        ggplot2::theme_void()
                    print(plot)
                    return(TRUE)
                }

                tryCatch({
                    plot_data <- data.frame(
                        .time = plotData$time,
                        .outcome = plotData$outcome,
                        risk_groups = plotData$risk_groups
                    )

                    # Create survival formula
                    formula_str <- "survival::Surv(.time, .outcome) ~ risk_groups"
                    surv_formula <- .asSurvivalFormula(formula_str)

                    # Fit survival model
                    fit <- survival::survfit(surv_formula, data = plot_data)
                    # survminer re-parses fit$call$formula; passing the formula through a variable
                    # leaves a bare symbol there and ggsurvplot dies with
                    # "object of type 'symbol' is not subsettable".
                    fit$call$formula <- surv_formula

                    # Create survival plot
                    surv_plot <- .quietly(survminer::ggsurvplot(
                        fit,
                        data = plot_data,
                        title = paste0("Survival Curves - Multiple Cut-offs for ", self$options$contexpl),
                        subtitle = paste0("Method: ", plotData$method,
                                          " | Groups: ", length(levels(plotData$risk_groups)),
                                          " | log-rank p is exploratory: groups were chosen from these data"),
                        xlab = private$.timeAxisLabel(),
                        ylab = "Survival Probability",
                        legend.title = .("Marker groups"),
                        risk.table = self$options$risktable,
                        conf.int = self$options$ci95,
                        censor = self$options$censored,
                        surv.median.line = self$options$medianline,
                        pval = TRUE,
                        pval.coord = c(0.1, 0.1),
                        break.time.by = private$.plotBy(),
                        xlim = c(0, private$.plotEndTime()),
                        ylim = c(self$options$ybegin_plot, self$options$yend_plot),
                        palette = "jco",
                        ggtheme = ggplot2::theme_minimal()
                    ))

                    .quietly(print(surv_plot))
                }, error = function(e) {
                    # Fallback plot
                    plot <- ggplot2::ggplot() +
                        ggplot2::geom_text(ggplot2::aes(x = 0.5, y = 0.5,
                                                      label = "Multiple Cutoffs Survival Plot\nError in plot generation"),
                                          size = 6) +
                        ggplot2::xlim(0, 1) + ggplot2::ylim(0, 1) +
                        ggplot2::theme_void()
                    print(plot)
                })

                TRUE
            }

            # RMST Analysis ----
            ,
            .calculateRMST = function(results, cutoffdata = NULL) {
                if (!self$options$rmst_analysis) {
                    return()
                }

                # Clear rows from any previous (pre-cutoff) invocation so the table is not
                # double-populated when findcut is enabled (pre-cutoff + cutoff both call this).
                #
                # The narrative must be cleared with the table. With findcut on,
                # .calculateRMST runs twice: once on the whole cohort, then again on
                # the cut-off groups with tau bounded by the SMALLER group's support.
                # A tau between the two limits let the first pass write a summary and
                # the second pass return early -- leaving an empty table beside a
                # 276-character interpretation of an RMST that is no longer shown.
                self$results$rmstTable$deleteRows()
                self$results$rmstSummary$setContent("")
                private$.landmarkNote(self$results$rmstTable, results)

                # Use cutoffdata if provided (for cutoff analysis), otherwise use original data
                data_to_use <- if (!is.null(cutoffdata)) cutoffdata else results$cleanData

                mytime <- results$name1time
                myoutcome <- results$analysis_outcome

                # For cutoff analysis, use the selected cut-off groups. Without a
                # cut-off, report the cohort RMST instead of silently dichotomising
                # the continuous predictor at its sample median.
                if (!is.null(cutoffdata) && results$name3contexpl %in% names(cutoffdata)) {
                    mygroup <- results$name3contexpl
                } else {
                    data_to_use$rmst_groups <- factor("Overall")
                    mygroup <- "rmst_groups"
                }

                # Comparisons require a common horizon supported by every group.
                group_max <- tapply(
                    data_to_use[[mytime]],
                    data_to_use[[mygroup]],
                    max,
                    na.rm = TRUE
                )
                max_supported_tau <- min(group_max[is.finite(group_max)])
                if (!is.finite(max_supported_tau) || max_supported_tau <= 0) {
                    self$results$rmstTable$setNote(
                        "support",
                        .("RMST could not be calculated because no follow-up horizon is supported in every group.")
                    )
                    return()
                }

                # Get tau from options or use the 75th percentile, bounded by
                # common observed support.
                if (!is.null(self$options$rmst_tau) && self$options$rmst_tau > 0) {
                    tau <- self$options$rmst_tau
                    if (!is.finite(tau) || tau > max_supported_tau) {
                        self$results$rmstTable$setNote(
                            "support",
                            sprintf(
                                .("RMST time horizon must be no larger than %.2f, the maximum follow-up supported in every displayed group."),
                                max_supported_tau
                            )
                        )
                        private$.addHtmlMessage(
                            "error",
                            .("Unsupported RMST horizon"),
                            sprintf(
                                .("The requested RMST horizon %.2f exceeds the common observed follow-up limit of %.2f."),
                                tau,
                                max_supported_tau
                            )
                        )
                        return()
                    }
                } else {
                    tau <- min(
                        as.numeric(stats::quantile(
                            data_to_use[[mytime]],
                            0.75,
                            na.rm = TRUE,
                            names = FALSE
                        )),
                        max_supported_tau
                    )
                }

                # Calculate RMST for each group
                rmst_results <- list()
                groups <- unique(data_to_use[[mygroup]])

                for (group in groups) {
                    group_data <- data_to_use[data_to_use[[mygroup]] == group, ]

                    if (nrow(group_data) > 0) {
                        surv_obj <- survival::Surv(
                            time = group_data[[mytime]],
                            event = group_data[[myoutcome]]
                        )
                        km_fit <- survival::survfit(surv_obj ~ 1, data = group_data)

                        rmean_tbl <- summary(km_fit, rmean = tau, extend = TRUE)$table
                        rmst <- if (is.matrix(rmean_tbl))
                            unname(rmean_tbl[1, "rmean"]) else unname(rmean_tbl[["rmean"]])
                        se_rmst <- if (is.matrix(rmean_tbl))
                            unname(rmean_tbl[1, "se(rmean)"]) else unname(rmean_tbl[["se(rmean)"]])

                        if (!is.finite(rmst)) {
                            private$.addHtmlMessage(
                                type = "warning",
                                title = .("Insufficient Data for RMST"),
                                message = sprintf(.("RMST could not be estimated for group \"%s\"."), as.character(group))
                            )
                            next
                        }

                        if (!is.finite(se_rmst)) {
                            private$.addHtmlMessage(
                                type = "warning",
                                title = .("RMST standard error unavailable"),
                                message = sprintf(.("The restricted mean survival time for group \"%s\" is shown without a confidence interval because its standard error could not be estimated."), as.character(group))
                            )
                            se_rmst <- NA_real_
                        }

                        rmst_results[[as.character(group)]] <- list(
                            group = as.character(group),
                            rmst = rmst,
                            se = se_rmst,
                            ci_lower = rmst - 1.96 * se_rmst,
                            ci_upper = rmst + 1.96 * se_rmst,
                            tau = tau,
                            n = nrow(group_data)
                        )
                    }
                }

                # Populate RMST table
                rmst_table <- self$results$rmstTable
                for (result in rmst_results) {
                    rmst_table$addRow(rowKey = result$group, values = result)
                }
                rmst_table$setNote(
                    "method",
                    .("RMST and its Greenwood-based standard error are calculated from the Kaplan-Meier estimator using survival::survfit at a common observed time horizon. Wald 95% confidence intervals are shown.")
                )

                # Create RMST summary
                if (length(rmst_results) > 0) {
                    summary_text <- paste0(
                        "Restricted Mean Survival Time (RMST) Analysis\n",
                        "Time horizon (\u03c4): ", round(tau, 1), " ", self$options$timetypeoutput, "\n\n",
                        "The RMST represents the average time a patient can expect to survive up to the specified time horizon.\n",
                        "This metric is particularly useful when the survival curves do not reach 50% (median undefined).\n\n"
                    )

                    # Add group comparisons if multiple groups
                    if (length(rmst_results) == 2) {
                        group_names <- names(rmst_results)
                        diff_rmst <- rmst_results[[group_names[2]]]$rmst - rmst_results[[group_names[1]]]$rmst
                        diff_se <- sqrt(
                            rmst_results[[group_names[2]]]$se^2 +
                                rmst_results[[group_names[1]]]$se^2
                        )
                        diff_ci <- diff_rmst + c(-1, 1) * 1.96 * diff_se
                        summary_text <- paste0(
                            summary_text,
                            "Difference in RMST (", group_names[2], " vs ", group_names[1], "): ",
                            round(diff_rmst, 2), " ", self$options$timetypeoutput,
                            " [95% CI: ", round(diff_ci[1], 2), " to ", round(diff_ci[2], 2), "]\n",
                            "Descriptive interpretation: Patients in '", group_names[2], "' group had on average ",
                            abs(round(diff_rmst, 2)), " ",
                            if (diff_rmst > 0) "more" else "fewer",
                            " ", self$options$timetypeoutput,
                            " of observed restricted survival up to ", round(tau, 1), " ", self$options$timetypeoutput,
                            ". Cut-off groups are data-derived, so this comparison is exploratory and requires external validation."
                        )
                    }

                    summary_text <- htmltools::htmlEscape(summary_text)
                    self$results$rmstSummary$setContent(summary_text)
                }
            }

            # Residual Diagnostics ----
            ,
            .calculateResiduals = function(results, cutoffdata = NULL) {
                if (!self$options$residual_diagnostics) {
                    return()
                }

                # Clear rows from any previous (pre-cutoff) invocation so integer rowKeys are
                # not duplicated (addRow with an existing rowKey errors) when findcut is enabled.
                self$results$residualsTable$deleteRows()
                self$results$schoenfeldResidualsTable$deleteRows()

                # Use cutoffdata if provided, otherwise use original data
                data_to_use <- if (!is.null(cutoffdata)) cutoffdata else results$cleanData

                mytime <- results$name1time
                myoutcome <- results$analysis_outcome

                # Same column name in both frames: in cleanData it holds the raw
                # continuous marker, in surv_categorize() output it holds the
                # categorised groups. Which model was fitted is disclosed in the
                # table note below.
                myexplanatory <- results$name3contexpl

                tryCatch({
                    # Create Cox model formula
                    escaped_time <- private$.escapeVariableNames(mytime)
                    escaped_outcome <- private$.escapeVariableNames(myoutcome)
                    escaped_explanatory <- private$.escapeVariableNames(myexplanatory)
                    formula_str <- paste0("survival::Surv(", escaped_time, ", ", escaped_outcome, ") ~ ", escaped_explanatory)
                    cox_formula <- .asSurvivalFormula(formula_str)

                    # Fit Cox model
                    cox_model <- survival::coxph(cox_formula, data = data_to_use)

                    # Calculate residuals
                    martingale_resid <- residuals(cox_model, type = "martingale")
                    deviance_resid <- residuals(cox_model, type = "deviance")
                    score_resid <- residuals(cox_model, type = "score")
                    schoenfeld_resid <- residuals(cox_model, type = "schoenfeld")

                    n_obs <- length(martingale_resid)

                    # Martingale, deviance, and score residuals are case-level.
                    # Schoenfeld residuals exist only at event times and cannot
                    # be aligned to these rows; they are reported separately.
                    # surv_categorize() output (the cutoffdata path) carries only
                    # time, event and the categorised marker -- no `row_names`
                    # column. `NULL[i]` is NULL, so as.integer() returned
                    # integer(0) and the is.na() test below threw "argument is of
                    # length zero". The whole tryCatch fell into its error handler,
                    # so BOTH residual tables showed a single all-NA row whenever
                    # residual diagnostics ran with findcut enabled.
                    row_ids <- data_to_use[["row_names"]]
                    if (is.null(row_ids)) row_ids <- rownames(data_to_use)

                    residuals_table <- self$results$residualsTable
                    n_shown <- min(100, n_obs)
                    for (i in 1:n_shown) {  # Limit to first 100 observations for display
                        observation_id <- suppressWarnings(as.integer(row_ids[i]))
                        if (length(observation_id) != 1L || is.na(observation_id))
                            observation_id <- i
                        residuals_table$addRow(rowKey = i, values = list(
                            observation = observation_id,
                            martingale = round(martingale_resid[i], 4),
                            deviance = round(deviance_resid[i], 4),
                            score = if (is.matrix(score_resid)) round(score_resid[i, 1], 4) else round(score_resid[i], 4),
                            schoenfeld = NA_real_
                        ))
                    }
                    residuals_table$setNote(
                        "alignment",
                        .("Schoenfeld residuals are indexed by event time, not by patient row, and are therefore reported in the separate event-time table.")
                    )
                    # A silent truncation reads as "these are all the cases".
                    if (n_obs > n_shown)
                        residuals_table$setNote("truncated", sprintf(
                            .("Showing the first %d of %d cases."), n_shown, n_obs))
                    residuals_table$setNote("model", sprintf(
                        .("Residuals are from a Cox model of survival on %s."),
                        if (!is.null(cutoffdata))
                            sprintf(.("the cut-off groups of '%s'"), self$options$contexpl)
                        else sprintf(.("'%s' as a continuous predictor"), self$options$contexpl)))

                    schoenfeld_values <- if (is.matrix(schoenfeld_resid)) {
                        schoenfeld_resid[, 1]
                    } else {
                        schoenfeld_resid
                    }
                    event_times <- suppressWarnings(as.numeric(
                        if (is.matrix(schoenfeld_resid)) rownames(schoenfeld_resid)
                        else names(schoenfeld_resid)
                    ))
                    if (length(event_times) != length(schoenfeld_values))
                        event_times <- rep(NA_real_, length(schoenfeld_values))

                    schoenfeld_table <- self$results$schoenfeldResidualsTable
                    n_sch_shown <- min(100, length(schoenfeld_values))
                    if (length(schoenfeld_values) > n_sch_shown)
                        schoenfeld_table$setNote("truncated", sprintf(
                            .("Showing the first %d of %d event times."),
                            n_sch_shown, length(schoenfeld_values)))
                    for (i in seq_len(n_sch_shown)) {
                        schoenfeld_table$addRow(
                            rowKey = i,
                            values = list(
                                event_time = event_times[i],
                                residual = round(schoenfeld_values[i], 4)
                            )
                        )
                    }

                    # Store residuals for plotting. setState() persists the data with the
                    # results so the plot renders after a saved .omv is reopened without re-running.
                    residuals_state <- list(
                        martingale = martingale_resid,
                        deviance = deviance_resid,
                        fitted = cox_model$linear.predictors
                    )
                    private$residuals_data <- residuals_state
                    self$results$residualsPlot$setState(residuals_state)

                }, error = function(e) {
                    # If residual calculation fails, show error message
                    residuals_table <- self$results$residualsTable
                    residuals_table$addRow(rowKey = 1, values = list(
                        observation = 1,
                        martingale = NA,
                        deviance = NA,
                        score = NA,
                        schoenfeld = NA
                    ))
                    residuals_table$setNote("error", sprintf(.("Residual calculation failed: %s"), htmltools::htmlEscape(e$message)))
                    self$results$schoenfeldResidualsTable$setNote(
                        "error",
                        sprintf(.("Schoenfeld residual calculation failed: %s"), htmltools::htmlEscape(e$message))
                    )
                })
            }

            # Stratified Cox Regression ----
            ,
            .stratifiedCox = function(results) {
                # Previously a stub: it was never called from anywhere, and even
                # if it had been it only appended a note telling the user to go
                # and use a different analysis. Both options (stratified_cox,
                # strata_variable) were therefore inert, while the
                # PH-violation warning actively recommended them.
                #
                # The stratified model is always fitted on the CONTINUOUS predictor
                # (the primary analysis). It used to accept a `cutoffdata` argument
                # that no caller ever passed and that would have broken the
                # row_names match below, since surv_categorize() output has no
                # row_names column.
                if (!self$options$stratified_cox) return()

                tbl <- self$results$stratifiedCoxTable
                tbl$deleteRows()

                strata_var <- self$options$strata_variable
                if (is.null(strata_var) || length(strata_var) == 0) {
                    tbl$setNote("novar", .("Select a stratification variable to fit a stratified Cox model."))
                    return()
                }

                mydata <- results$cleanData
                mytime    <- results$name1time
                myoutcome <- results$analysis_outcome
                if (is.null(myoutcome)) myoutcome <- results$name2outcome

                # The stratification variable is not carried through .cleandata(),
                # so pull it from the source frame and align by row name.
                labelled <- private$.getData()
                sv <- names(labelled$all_labels)[labelled$all_labels == strata_var]
                if (length(sv) == 0 || !sv[1] %in% names(labelled$mydata_labelled)) {
                    tbl$setNote("missing", .("The stratification variable could not be located in the data."))
                    return()
                }
                src <- labelled$mydata_labelled
                mydata[[".strata"]] <- src[[sv[1]]][
                    match(as.character(mydata$row_names), as.character(src$row_names))
                ]
                mydata <- mydata[!is.na(mydata[[".strata"]]), , drop = FALSE]
                mydata[[".strata"]] <- droplevels(as.factor(mydata[[".strata"]]))

                if (nlevels(mydata[[".strata"]]) < 2) {
                    tbl$setNote("onelevel", .("The stratification variable has fewer than two levels in the analysed rows."))
                    return()
                }

                # A continuous variable dropped in here yields one stratum per
                # patient. coxph does not error: it returns a non-converged fit that
                # was tabulated as a perfectly ordinary HR 1.00 (0.00, Inf), p = 1.00
                # with a note announcing "150 levels" -- a meaningless model with the
                # visual authority of a real one. Stratification is only meaningful
                # with a handful of reasonably sized strata.
                n_strata <- nlevels(mydata[[".strata"]])
                if (n_strata > max(10, floor(nrow(mydata) / 10))) {
                    tbl$setNote("toomany", sprintf(
                        .("Not fitted: '%s' has %d levels across %d analysed rows (about %.1f patients per stratum). Stratification needs a small number of reasonably sized groups; a near-continuous variable gives one stratum per patient and no estimable hazard ratio. Use a categorical variable, or add this variable as a covariate in Multivariable Survival Analysis instead."),
                        strata_var, n_strata, nrow(mydata), nrow(mydata) / n_strata))
                    return()
                }

                # A model can technically fit with a stratum containing almost no
                # patients or events, but that stratum contributes little or no
                # information and can make the pooled coefficient unstable. Disclose
                # this before showing a precise-looking hazard ratio.
                stratum_n <- table(mydata[[".strata"]])
                stratum_events <- tapply(
                    mydata[[myoutcome]] == 1,
                    mydata[[".strata"]],
                    sum,
                    na.rm = TRUE
                )
                sparse <- names(stratum_n)[
                    as.integer(stratum_n) < 10L |
                    as.integer(stratum_events[names(stratum_n)]) < 3L
                ]
                if (length(sparse) > 0L) {
                    sparse_details <- vapply(sparse, function(level) {
                        sprintf(
                            "%s (n=%d, events=%d)",
                            level,
                            stratum_n[[level]],
                            stratum_events[[level]]
                        )
                    }, character(1))
                    sparse_message <- sprintf(
                        .("Sparse strata detected: %s. Strata with fewer than 10 patients or 3 events provide limited information; interpret the pooled hazard ratio and proportional-hazards assessment cautiously."),
                        paste(sparse_details, collapse = "; ")
                    )
                    tbl$setNote("sparse", sparse_message)
                    private$.addHtmlMessage(
                        "warning",
                        .("Sparse strata in stratified Cox model"),
                        sparse_message
                    )
                }

                contexpl <- results$name3contexpl

                fit <- tryCatch({
                    fml <- .asSurvivalFormula(paste0(
                        "survival::Surv(", private$.escapeVariableNames(mytime), ", ",
                        private$.escapeVariableNames(myoutcome), ") ~ ",
                        private$.escapeVariableNames(contexpl), " + strata(.strata)"))
                    survival::coxph(fml, data = mydata)
                }, error = function(e) e)

                if (inherits(fit, "error")) {
                    tbl$setNote("err", sprintf(.("Stratified Cox model could not be fitted: %s"), conditionMessage(fit)))
                    return()
                }

                sm <- summary(fit)$coefficients
                ci <- summary(fit)$conf.int
                # rownames() carry the janitor-cleaned column name, so a predictor
                # the user selected as "Ki-67 index" was tabulated as "ki_67_index".
                term_labels <- vapply(rownames(sm), function(nm) {
                    orig <- labelled$all_labels[[nm]]
                    if (is.null(orig)) nm else as.character(orig)
                }, character(1))
                for (i in seq_len(nrow(sm))) {
                    tbl$addRow(rowKey = i, values = list(
                        term     = unname(term_labels[i]),
                        hr       = unname(sm[i, "exp(coef)"]),
                        ci_lower = unname(ci[i, "lower .95"]),
                        ci_upper = unname(ci[i, "upper .95"]),
                        pvalue   = unname(sm[i, ncol(sm)])
                    ))
                }

                tbl$setNote("method", sprintf(
                    .("Baseline hazard allowed to differ across the %d levels of '%s'. Stratification removes the proportional-hazards assumption for that variable, so no hazard ratio is estimated for it."),
                    nlevels(mydata[[".strata"]]), strata_var))
            }

            # Log-Log Plot Function ----
            ,
            .plot7 = function(image, ggtheme, theme, ...) {
                if (!self$options$loglog || !self$options$findcut) {
                    return()
                }

                # Get the plot data from previous analysis
                plotData <- image$state
                if (is.null(plotData)) {
                    return()
                }

                res.cat <- plotData$cutoffdata
                results <- plotData$results

                if (is.null(res.cat) || is.null(results)) {
                    return()
                }

                mytime <- results$name1time
                myoutcome <- results$analysis_outcome
                mycontexpl <- results$name3contexpl

                # Create formula with escaped variable names
                escaped_time <- private$.escapeVariableNames(mytime)
                escaped_outcome <- private$.escapeVariableNames(myoutcome)
                escaped_contexpl <- private$.escapeVariableNames(mycontexpl)

                formula_str <- paste0('survival::Surv(', escaped_time, ',', escaped_outcome, ') ~ ', escaped_contexpl)
                myformula <- .asSurvivalFormula(formula_str)

                # Fit survival model
                fit <- survival::survfit(myformula, data = res.cat)
                # survminer re-parses fit$call$formula; passing the formula through a variable
                # leaves a bare symbol there and ggsurvplot dies with
                # "object of type 'symbol' is not subsettable".
                fit$call$formula <- myformula

                tryCatch({
                    # Create log-log plot using survminer
                    loglog_plot <- .quietly(survminer::ggsurvplot(
                        fit,
                        data = res.cat,
                        fun = "cloglog",
                        xlab = paste0("log(Time in ", self$options$timetypeoutput, ")"),
                        ylab = "log(-log(Survival))",
                        title = paste0("Log-Log Plot for ", self$options$contexpl),
                        legend.title = self$options$contexpl,
                        risk.table = FALSE,
                        conf.int = FALSE
                    ))

                    .quietly(print(loglog_plot))
                }, error = function(e) {
                    # Fallback: create simple log-log plot with ggplot2
                    surv_data <- data.frame(
                        time = fit$time,
                        surv = fit$surv,
                        strata = rep(names(fit$strata), fit$strata)
                    )

                    # Remove zero survival values for log transformation
                    surv_data <- surv_data[surv_data$surv > 0 & surv_data$time > 0, ]

                    if (nrow(surv_data) > 0) {
                        surv_data$log_time <- log(surv_data$time)
                        surv_data$cloglog <- log(-log(surv_data$surv))

                        loglog_plot <- ggplot2::ggplot(surv_data, ggplot2::aes(x = log_time, y = cloglog, color = strata)) +
                            ggplot2::geom_line() +
                            ggplot2::labs(
                                x = paste0("log(Time in ", self$options$timetypeoutput, ")"),
                                y = "log(-log(Survival))",
                                title = paste0("Log-Log Plot for ", self$options$contexpl),
                                color = self$options$contexpl
                            ) +
                            ggplot2::theme_minimal()

                        print(loglog_plot)
                    }
                })

                TRUE
            }

            # Residuals Plot Function ----
            ,
            .plot9 = function(image, ggtheme, theme, ...) {
                # Prefer image state (persists across .omv reloads); fall back to the
                # private field for the same-session render path.
                residuals_data <- image$state
                if (is.null(residuals_data)) {
                    residuals_data <- private$residuals_data
                }
                if (!self$options$residual_diagnostics || is.null(residuals_data)) {
                    return()
                }

                tryCatch({

                    # Create a 2x2 plot layout
                    plot_data <- data.frame(
                        fitted = residuals_data$fitted,
                        martingale = residuals_data$martingale,
                        deviance = residuals_data$deviance
                    )

                    # Martingale residuals vs fitted values
                    p1 <- ggplot2::ggplot(plot_data, ggplot2::aes(x = fitted, y = martingale)) +
                        ggplot2::geom_point(alpha = 0.6) +
                        ggplot2::geom_smooth(method = "loess", se = FALSE, color = "red") +
                        ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
                        ggplot2::labs(
                            x = "Linear Predictors",
                            y = "Martingale Residuals",
                            title = "Martingale Residuals vs Fitted"
                        ) +
                        ggplot2::theme_minimal()

                    # Deviance residuals vs fitted values
                    p2 <- ggplot2::ggplot(plot_data, ggplot2::aes(x = fitted, y = deviance)) +
                        ggplot2::geom_point(alpha = 0.6) +
                        ggplot2::geom_smooth(method = "loess", se = FALSE, color = "red") +
                        ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
                        ggplot2::labs(
                            x = "Linear Predictors",
                            y = "Deviance Residuals",
                            title = "Deviance Residuals vs Fitted"
                        ) +
                        ggplot2::theme_minimal()

                    # QQ plot of deviance residuals
                    p3 <- ggplot2::ggplot(plot_data, ggplot2::aes(sample = deviance)) +
                        ggplot2::stat_qq() +
                        ggplot2::stat_qq_line() +
                        ggplot2::labs(
                            x = "Theoretical Quantiles",
                            y = "Sample Quantiles",
                            title = "Q-Q Plot of Deviance Residuals"
                        ) +
                        ggplot2::theme_minimal()

                    # Histogram of deviance residuals
                    p4 <- ggplot2::ggplot(plot_data, ggplot2::aes(x = deviance)) +
                        ggplot2::geom_histogram(bins = 30, alpha = 0.7, fill = "skyblue") +
                        ggplot2::geom_density(alpha = 0.3, fill = "red") +
                        ggplot2::labs(
                            x = "Deviance Residuals",
                            y = "Frequency",
                            title = "Distribution of Deviance Residuals"
                        ) +
                        ggplot2::theme_minimal()

                    # Combine plots using patchwork if available, otherwise show first plot
                    if (requireNamespace("patchwork", quietly = TRUE)) {
                        combined_plot <- (p1 + p2) / (p3 + p4)
                        print(combined_plot)
                    } else {
                        print(p1)
                    }

                }, error = function(e) {
                    # Fallback plot
                    fallback_plot <- ggplot2::ggplot() +
                        ggplot2::geom_text(ggplot2::aes(x = 0.5, y = 0.5,
                                                      label = "Residuals plot unavailable"),
                                          size = 6) +
                        ggplot2::xlim(0, 1) + ggplot2::ylim(0, 1) +
                        ggplot2::theme_void()
                    print(fallback_plot)
                })

                TRUE
            }

            # Educational Explanations ----
            ,
            .addExplanations = function() {
                # Cox Regression Explanation
                private$.setExplanationContent("coxRegressionExplanation", '
                <div class="explanation-box" style="background-color: rgba(33, 152, 255, 0.07); padding: 15px; border-radius: 8px; margin: 10px 0; color: inherit;">
                    <h3 style="color: inherit; margin-top: 0;"> Understanding Cox Regression for Continuous Variables</h3>

                    <div style="background-color: rgba(255, 255, 255, 0.08); padding: 12px; border-radius: 5px; margin: 10px 0; color: inherit;">
                        <h4 style="color: inherit; margin-top: 0;">What is Cox Regression with Continuous Variables?</h4>
                        <p style="margin: 8px 0;">Cox regression with continuous variables analyzes how <strong>each unit increase</strong> in a continuous predictor (e.g., age, biomarker level) affects survival risk.</p>

                        <div style="background-color: rgba(33, 184, 255, 0.11); padding: 10px; border-radius: 5px; margin: 10px 0; color: inherit;">
                            <strong> Key Concept:</strong> The reported HR assumes a linear change in log hazard for each unit increase. This is a model assumption, not proof of a biological dose-response relationship.
                        </div>
                    </div>

                    <div style="background-color: rgba(246, 163, 33, 0.11); padding: 12px; border-radius: 5px; margin: 10px 0; color: inherit;">
                        <h4 style="color: inherit; margin-top: 0;"> Interpreting Hazard Ratios (HR)</h4>
                        <table style="width: 100%; border-collapse: collapse; margin: 10px 0;">
                            <tr style="background-color: rgba(255, 202, 33, 0.23); color: inherit;">
                                <th style="padding: 8px; text-align: left; border: 1px solid #ffc107;">HR Value</th>
                                <th style="padding: 8px; text-align: left; border: 1px solid #ffc107;">Meaning</th>
                                <th style="padding: 8px; text-align: left; border: 1px solid #ffc107;">Clinical Example</th>
                            </tr>
                            <tr>
                                <td style="padding: 8px; border: 1px solid #ffc107;"><strong>HR = 1.0</strong></td>
                                <td style="padding: 8px; border: 1px solid #ffc107;">No effect</td>
                                <td style="padding: 8px; border: 1px solid #ffc107;">Variable does not affect survival</td>
                            </tr>
                            <tr style="background-color: rgba(255, 196, 33, 0.07); color: inherit;">
                                <td style="padding: 8px; border: 1px solid #ffc107;"><strong>HR > 1.0</strong></td>
                                <td style="padding: 8px; border: 1px solid #ffc107;">Increased risk</td>
                                <td style="padding: 8px; border: 1px solid #ffc107;">HR = 1.05: 5% higher hazard per unit</td>
                            </tr>
                            <tr>
                                <td style="padding: 8px; border: 1px solid #ffc107;"><strong>HR < 1.0</strong></td>
                                <td style="padding: 8px; border: 1px solid #ffc107;">Decreased risk (protective)</td>
                                <td style="padding: 8px; border: 1px solid #ffc107;">HR = 0.95: 5% lower hazard per unit</td>
                            </tr>
                        </table>
                    </div>

                    <div style="background-color: rgba(33, 159, 43, 0.1); padding: 12px; border-radius: 5px; margin: 10px 0; color: inherit;">
                        <h4 style="color: inherit; margin-top: 0;"> Clinical Examples</h4>

                        <div style="background-color: rgba(255, 255, 255, 0.08); padding: 10px; border-radius: 5px; margin: 10px 0; color: inherit;">
                            <strong>Example 1: Age and Cancer Survival</strong>
                            <p style="margin: 5px 0;">Age HR = 1.03 (95% CI: 1.01-1.05, p=0.001)</p>
                            <ul style="margin: 5px 0; padding-left: 20px;">
                                <li><strong>Interpretation:</strong> Each additional year of age multiplies the hazard by 1.03</li>
                                <li><strong>10-year difference:</strong> 1.03^10 = 1.34 x the hazard for 70 vs 60 years old - a hazard ratio, not a 34% cumulative-risk difference</li>
                                <li><strong>Significance:</strong> p<0.05 means this effect is statistically significant</li>
                            </ul>
                        </div>

                        <div style="background-color: rgba(153, 33, 170, 0.12); padding: 10px; border-radius: 5px; margin: 10px 0; color: inherit;">
                            <strong>Example 2: Biomarker Level</strong>
                            <p style="margin: 5px 0;">Protein X HR = 0.98 (95% CI: 0.96-0.99, p=0.02)</p>
                            <ul style="margin: 5px 0; padding-left: 20px;">
                                <li><strong>Protective factor:</strong> Higher protein X levels multiply the hazard by 0.98 per unit</li>
                                <li><strong>Range effect:</strong> 20-point increase \u2192 0.98^20 = 0.67 x the hazard; absolute benefit depends on baseline risk</li>
                            </ul>
                        </div>
                    </div>

                    <div style="background-color: rgba(255, 169, 33, 0.14); padding: 10px; border-radius: 5px; margin-top: 10px; border-left: 4px solid #ff9800; color: inherit;">
                        <strong> Important Assumptions:</strong>
                        <ul style="margin: 5px 0; padding-left: 20px;">
                            <li><strong>Linear relationship:</strong> Effect is constant across all values (may not always be true)</li>
                            <li><strong>Proportional hazards:</strong> Relative effect stays constant over time</li>
                            <li><strong>Consider cut-offs:</strong> If relationship is non-linear, consider categorizing the variable</li>
                        </ul>
                    </div>
                </div>
                ')

                # Cut-off Point Analysis Explanation
                private$.setExplanationContent("cutoffAnalysisExplanation", '
                <div class="explanation-box" style="background-color: rgba(33, 152, 255, 0.07); padding: 15px; border-radius: 8px; margin: 10px 0; color: inherit;">
                    <h3 style="color: inherit; margin-top: 0;"> Understanding Cut-off Point Analysis</h3>

                    <div style="background-color: rgba(255, 255, 255, 0.08); padding: 12px; border-radius: 5px; margin: 10px 0; color: inherit;">
                        <h4 style="color: inherit; margin-top: 0;">What is Cut-off Point Analysis?</h4>
                        <p style="margin: 8px 0;">Cut-off analysis transforms a <strong>continuous variable into lower- and higher-value groups</strong> using a data-derived threshold. The direction of risk must be read from the survival estimates; higher marker values are not automatically higher risk.</p>

                        <div style="background-color: rgba(33, 184, 255, 0.11); padding: 10px; border-radius: 5px; margin: 10px 0; color: inherit;">
                            <strong> Goal:</strong> Find the value that creates two groups with the <strong>maximum survival difference</strong>
                        </div>
                    </div>

                    <div style="background-color: rgba(246, 163, 33, 0.11); padding: 12px; border-radius: 5px; margin: 10px 0; color: inherit;">
                        <h4 style="color: inherit; margin-top: 0;"> How It Works</h4>

                        <div style="background-color: rgba(255, 255, 255, 0.08); padding: 10px; border-radius: 5px; margin: 10px 0; color: inherit;">
                            <strong>1. Maximally Selected Rank Statistics Method:</strong>
                            <ul style="margin: 5px 0; padding-left: 20px;">
                                <li>Tests every candidate cut-off value (respecting the minimum group size)</li>
                                <li>For each cut-off, computes the standardised log-rank statistic</li>
                                <li>Selects the cut-off with the largest statistic (biggest difference)</li>
                                <li>No multiplicity-adjusted p-value is reported for the selected cut-off; validate it in independent data</li>
                            </ul>
                        </div>

                        <table style="width: 100%; border-collapse: collapse; margin: 10px 0;">
                            <tr style="background-color: rgba(255, 202, 33, 0.23); color: inherit;">
                                <th style="padding: 8px; text-align: left; border: 1px solid #ffc107;">Step</th>
                                <th style="padding: 8px; text-align: left; border: 1px solid #ffc107;">Process</th>
                                <th style="padding: 8px; text-align: left; border: 1px solid #ffc107;">Output</th>
                            </tr>
                            <tr>
                                <td style="padding: 8px; border: 1px solid #ffc107;"><strong>1. Testing</strong></td>
                                <td style="padding: 8px; border: 1px solid #ffc107;">Try multiple cut-offs</td>
                                <td style="padding: 8px; border: 1px solid #ffc107;">Range of log-rank statistics</td>
                            </tr>
                            <tr style="background-color: rgba(255, 196, 33, 0.07); color: inherit;">
                                <td style="padding: 8px; border: 1px solid #ffc107;"><strong>2. Selection</strong></td>
                                <td style="padding: 8px; border: 1px solid #ffc107;">Find the maximum statistic</td>
                                <td style="padding: 8px; border: 1px solid #ffc107;">Optimal cut-off value</td>
                            </tr>
                            <tr>
                                <td style="padding: 8px; border: 1px solid #ffc107;"><strong>3. Validation</strong></td>
                                <td style="padding: 8px; border: 1px solid #ffc107;">Independent data (no adjusted p-value is computed here)</td>
                                <td style="padding: 8px; border: 1px solid #ffc107;">Confirmed or rejected cut-off</td>
                            </tr>
                        </table>
                    </div>

                    <div style="background-color: rgba(33, 159, 43, 0.1); padding: 12px; border-radius: 5px; margin: 10px 0; color: inherit;">
                        <h4 style="color: inherit; margin-top: 0;"> Clinical Benefits</h4>

                        <div style="background-color: rgba(255, 255, 255, 0.08); padding: 10px; border-radius: 5px; margin: 10px 0; color: inherit;">
                            <strong> Advantages:</strong>
                            <ul style="margin: 5px 0; padding-left: 20px;">
                                <li><strong>Exploratory presentation:</strong> Grouped curves can illustrate a possible non-linear pattern</li>
                                <li><strong>Information loss:</strong> Dichotomising a continuous predictor discards information and can reduce transportability</li>
                                <li><strong>Validation:</strong> A data-derived threshold must be tested in independent data</li>
                                <li><strong>Clinical use:</strong> Treatment or monitoring decisions require evidence beyond this analysis</li>
                            </ul>
                        </div>

                        <div style="background-color: rgba(153, 33, 170, 0.12); padding: 10px; border-radius: 5px; margin: 10px 0; color: inherit;">
                            <strong> Clinical Example:</strong>
                            <p style="margin: 5px 0;">Biomarker X data-derived cut-off = 25.3 ng/mL</p>
                            <ul style="margin: 5px 0; padding-left: 20px;">
                                <li>Higher-value group (\u226525.3): 40% 5-year survival in the development sample</li>
                                <li>Lower-value group (<25.3): 75% 5-year survival in the development sample</li>
                                <li>Interpretation: a hypothesis for external validation, not a monitoring recommendation</li>
                            </ul>
                        </div>
                    </div>

                    <div style="background-color: rgba(255, 33, 67, 0.09); padding: 12px; border-radius: 5px; margin: 10px 0; color: inherit;">
                        <h4 style="color: inherit; margin-top: 0;"> Important Limitations</h4>
                        <ul style="margin: 5px 0; padding-left: 20px;">
                            <li><strong>Data-dependent:</strong> Optimal cut-off may vary between studies</li>
                            <li><strong>Information loss:</strong> Converting continuous to binary loses precision</li>
                            <li><strong>Validation needed:</strong> Cut-off should be confirmed in independent datasets</li>
                            <li><strong>Population-specific:</strong> May not apply to different populations</li>
                        </ul>
                    </div>

                    <div style="background-color: rgba(255, 169, 33, 0.14); padding: 10px; border-radius: 5px; margin-top: 10px; border-left: 4px solid #ff9800; color: inherit;">
                        <strong> Best Practices:</strong>
                        <ul style="margin: 5px 0; padding-left: 20px;">
                            <li>Validate cut-off in independent cohort when possible</li>
                            <li>Consider clinical relevance, not just statistical significance</li>
                            <li>Report both continuous and cut-off analyses</li>
                            <li>Ensure adequate sample size in both groups</li>
                        </ul>
                    </div>
                </div>
                ')

                # Multiple Cutoffs Explanation
                private$.setExplanationContent("multipleCutoffsExplanation", '
                <div style="margin-bottom: 20px; padding: 15px; background-color: rgba(255, 202, 33, 0.23); border-left: 4px solid #ffc107; color: inherit;">
                    <h4 style="margin-top: 0; color: inherit;">Understanding Multiple Cut-offs Analysis</h4>
                    <p><strong>Marker-value grouping:</strong> Creates ordered groups from a continuous variable; the survival ordering is estimated from the data rather than assumed by the labels.</p>
                    <ul>
                        <li><strong>Multiple Cut-offs:</strong> Derives 2-4 candidate cut-points</li>
                        <li><strong>Groups:</strong> Creates ordered marker-value categories for exploratory comparison</li>
                        <li><strong>Method Options:</strong> Quantile-based, tree-based, or minimum p-value approaches</li>
                        <li><strong>Group Validation:</strong> Ensures adequate sample size in each risk group</li>
                    </ul>
                    <p><em>Important:</em> Data-derived groups may suggest non-linear patterns but also increase overfitting and multiplicity; report the continuous model and validate cut-offs externally.</p>
                </div>
                ')

                # Person-Time Analysis Explanation
                private$.setExplanationContent("personTimeExplanation", '
                <div style="margin-bottom: 20px; padding: 15px; background-color: rgba(33, 162, 64, 0.19); border-left: 4px solid #28a745; color: inherit;">
                    <h4 style="margin-top: 0; color: inherit;">Understanding Person-Time Analysis</h4>
                    <p><strong>Person-Time:</strong> Accounts for both number of participants and their observation duration.</p>
                    <ul>
                        <li><strong>Incidence Rate:</strong> Events per person-time unit (e.g., per 100 person-years)</li>
                        <li><strong>Time Intervals:</strong> Analyzes rates across different follow-up periods</li>
                        <li><strong>Rate Comparison:</strong> Compares incidence rates between risk groups</li>
                        <li><strong>Confidence Intervals:</strong> Provide precision estimates for rates</li>
                    </ul>
                    <p><em>Clinical use:</em> Essential for comparing event rates when follow-up times vary between groups.</p>
                </div>
                ')

                # RMST Analysis Explanation
                private$.setExplanationContent("rmstExplanation", '
                <div style="margin-bottom: 20px; padding: 15px; background-color: rgba(33, 41, 56, 0.13); border-left: 4px solid #6c757d; color: inherit;">
                    <h4 style="margin-top: 0; color: inherit;">Understanding Restricted Mean Survival Time (RMST)</h4>
                    <p><strong>RMST:</strong> Average survival time up to a specified time horizon (\u03c4).</p>
                    <ul>
                        <li><strong>Time-Limited Analysis:</strong> Mean survival within a defined observation period</li>
                        <li><strong>Group Comparison:</strong> Difference in RMST between risk groups</li>
                        <li><strong>Robust Measure:</strong> Less sensitive to tail behavior than median survival</li>
                        <li><strong>Clinical Interpretation:</strong> Direct measure of expected survival time</li>
                    </ul>
                    <p><em>When to use:</em> Particularly valuable when median survival cannot be estimated or for time-limited analyses.</p>
                </div>
                ')

                # Residual Diagnostics Explanation
                private$.setExplanationContent("residualDiagnosticsExplanation", '
                <div style="margin-bottom: 20px; padding: 15px; background-color: rgba(255, 202, 33, 0.4); border-left: 4px solid #fdcb6e; color: inherit;">
                    <h4 style="margin-top: 0; color: inherit;">Understanding Cox Model Residual Diagnostics</h4>
                    <p><strong>Model Residuals:</strong> Assess Cox model fit and identify potential issues.</p>
                    <ul>
                        <li><strong>Martingale Residuals:</strong> Detect functional form problems (should scatter around 0)</li>
                        <li><strong>Deviance Residuals:</strong> Standardized residuals for outlier detection</li>
                        <li><strong>Score Residuals:</strong> Assess influence of observations on coefficients</li>
                        <li><strong>Schoenfeld Residuals:</strong> Event-time diagnostics for time-varying coefficient patterns; the formal proportional-hazards test is reported separately</li>
                    </ul>
                    <p><em>Clinical interpretation:</em> Large residuals may indicate patients with unusual survival patterns requiring further investigation.</p>
                </div>
                ')

                # Log-Log Plot Explanation
                private$.setExplanationContent("loglogPlotExplanation", '
                <div style="margin-bottom: 20px; padding: 15px; background-color: rgba(33, 139, 255, 0.09); border-left: 4px solid #3182ce; color: inherit;">
                    <h4 style="margin-top: 0; color: inherit;">Understanding Log-Log Plots for the Proportional Hazards Assumption</h4>
                    <p><strong>Purpose:</strong> The complementary log-log plot displays log(-log(S(t))) against log(time) for each group and is a visual check of the proportional hazards (PH) assumption underlying Cox regression.</p>
                    <ul>
                        <li><strong>Parallel curves:</strong> Roughly parallel, non-crossing lines support the PH assumption (an approximately constant hazard ratio over time).</li>
                        <li><strong>Crossing or converging curves:</strong> Suggest the hazard ratio changes over time (PH violated); interpret the single Cox hazard ratio with caution.</li>
                        <li><strong>Constant vertical gap:</strong> The distance between curves approximates the log hazard ratio between groups.</li>
                    </ul>
                    <p><em>Clinical interpretation:</em> If the curves are clearly non-parallel, consider stratified Cox regression, time-varying effects, or a parametric survival model rather than relying on a single hazard ratio.</p>
                </div>
                ')

                # Survival Plots Explanation
                private$.setExplanationContent("survivalPlotsExplanation", '
                <div class="explanation-box" style="background-color: rgba(33, 152, 255, 0.07); padding: 15px; border-radius: 8px; margin: 10px 0; color: inherit;">
                    <h3 style="color: inherit; margin-top: 0;"> Understanding Survival Curves for Continuous Variables</h3>

                    <div style="background-color: rgba(255, 255, 255, 0.08); padding: 12px; border-radius: 5px; margin: 10px 0; color: inherit;">
                        <h4 style="color: inherit; margin-top: 0;"> Survival Curves with Cut-offs</h4>
                        <p style="margin: 8px 0;">When analyzing continuous variables, survival plots show <strong>separate curves for lower and higher marker-value groups</strong> based on a data-derived cut-off.</p>

                        <div style="background-color: rgba(33, 184, 255, 0.11); padding: 10px; border-radius: 5px; margin: 10px 0; color: inherit;">
                            <strong> How to Read the Plot:</strong>
                            <ul style="margin: 5px 0; padding-left: 20px;">
                                <li><strong>Two curves:</strong> Higher marker values (above cut-off) vs lower marker values (below cut-off)</li>
                                <li><strong>Separation:</strong> Wider gap between curves = stronger prognostic effect</li>
                                <li><strong>P-value:</strong> Tests whether group survival differs significantly</li>
                                <li><strong>Risk tables:</strong> Show number of patients at each time point</li>
                            </ul>
                        </div>
                    </div>

                    <div style="background-color: rgba(246, 163, 33, 0.11); padding: 12px; border-radius: 5px; margin: 10px 0; color: inherit;">
                        <h4 style="color: inherit; margin-top: 0;"> Curve Interpretation Patterns</h4>
                        <table style="width: 100%; border-collapse: collapse; margin: 10px 0;">
                            <tr style="background-color: rgba(255, 202, 33, 0.23); color: inherit;">
                                <th style="padding: 8px; text-align: left; border: 1px solid #ffc107;">Pattern</th>
                                <th style="padding: 8px; text-align: left; border: 1px solid #ffc107;">Clinical Meaning</th>
                            </tr>
                            <tr>
                                <td style="padding: 8px; border: 1px solid #ffc107;"><strong> Wide separation early</strong></td>
                                <td style="padding: 8px; border: 1px solid #ffc107;">Strong early prognostic effect</td>
                            </tr>
                            <tr style="background-color: rgba(255, 196, 33, 0.07); color: inherit;">
                                <td style="padding: 8px; border: 1px solid #ffc107;"><strong> Curves converge later</strong></td>
                                <td style="padding: 8px; border: 1px solid #ffc107;">Effect diminishes over time</td>
                            </tr>
                            <tr>
                                <td style="padding: 8px; border: 1px solid #ffc107;"><strong> Parallel curves</strong></td>
                                <td style="padding: 8px; border: 1px solid #ffc107;">Consistent proportional hazards</td>
                            </tr>
                            <tr style="background-color: rgba(255, 196, 33, 0.07); color: inherit;">
                                <td style="padding: 8px; border: 1px solid #ffc107;"><strong> Crossing curves</strong></td>
                                <td style="padding: 8px; border: 1px solid #ffc107;">Time-dependent effects (complex interpretation)</td>
                            </tr>
                        </table>
                    </div>

                    <div style="background-color: rgba(33, 159, 43, 0.1); padding: 12px; border-radius: 5px; margin: 10px 0; color: inherit;">
                        <h4 style="color: inherit; margin-top: 0;"> Clinical Application Tips</h4>

                        <div style="background-color: rgba(255, 255, 255, 0.08); padding: 10px; border-radius: 5px; margin: 10px 0; color: inherit;">
                            <strong> Risk Stratification:</strong>
                            <ul style="margin: 5px 0; padding-left: 20px;">
                                <li>Use the curves and estimates to determine which marker-value group had the higher observed event rate</li>
                                <li>Do not infer treatment benefit or causality from prognostic separation</li>
                                <li>Consider clinical context, not just statistical significance</li>
                            </ul>
                        </div>

                        <div style="background-color: rgba(153, 33, 170, 0.12); padding: 10px; border-radius: 5px; margin: 10px 0; color: inherit;">
                            <strong> Biomarker Validation:</strong>
                            <p style="margin: 5px 0;">Strong separation, even with a small p-value, is exploratory after data-driven cut-off selection and does not establish clinical utility. Before clinical use, evaluate:</p>
                            <ul style="margin: 5px 0; padding-left: 20px;">
                                <li>Independent external validation of the cut-off and effect size</li>
                                <li>Calibration, discrimination, and incremental value beyond established predictors</li>
                                <li>Clinical consequences and decision-impact studies</li>
                            </ul>
                        </div>
                    </div>

                    <div style="background-color: rgba(255, 169, 33, 0.14); padding: 10px; border-radius: 5px; margin-top: 10px; border-left: 4px solid #ff9800; color: inherit;">
                        <strong> Important Considerations:</strong>
                        <ul style="margin: 5px 0; padding-left: 20px;">
                            <li><strong>Cut-off validation:</strong> Confirm cut-off value in independent dataset</li>
                            <li><strong>Clinical relevance:</strong> Ensure the difference is clinically meaningful</li>
                            <li><strong>Sample size:</strong> Both groups should have adequate numbers for reliable estimates</li>
                            <li><strong>Follow-up:</strong> Longer follow-up provides more reliable late-time estimates</li>
                        </ul>
                    </div>
                </div>
                ')
            },

            # === EXTRACTED HELPER METHODS FOR MAINTAINABILITY ===

            # Helper function to safely set explanation content
            .setExplanationContent = function(result_name, content) {
                tryCatch({
                    self$results[[result_name]]$setContent(content)
                }, error = function(e) {
                    # Silently ignore if result does not exist
                })
            },

            # Input validation helper - prevents common data issues
            .validateInputs = function(data, time_var, outcome_var, contexpl_var) {
                if (is.null(data) || nrow(data) == 0) {
                    return(FALSE)
                }

                if (any(is.na(data[[time_var]]))) {
                    n_missing <- sum(is.na(data[[time_var]]))
                    private$.addHtmlMessage(
                        type = "error",
                        title = .("Invalid Survival Time"),
                        message = sprintf(.("Survival time contains %d missing (NA) value(s) after data cleaning. Every observation must have a valid finite follow-up time. Analysis was halted. Check the time variable (and the date inputs if time is calculated from dates)."), n_missing)
                    )
                    return(FALSE)
                }

                if (any(!is.finite(data[[time_var]]))) {
                    n_invalid <- sum(!is.finite(data[[time_var]]))
                    private$.addHtmlMessage(
                        type = "error",
                        title = .("Non-Finite Survival Time"),
                        message = sprintf(.("Survival time contains %d infinite or non-numeric value(s). Analysis was halted. Correct the elapsed-time variable or date inputs."), n_invalid)
                    )
                    return(FALSE)
                }

                if (any(data[[time_var]] < 0, na.rm = TRUE)) {
                    n_invalid <- sum(data[[time_var]] < 0, na.rm = TRUE)
                    private$.addHtmlMessage(
                        type = "error",
                        title = .("Negative Survival Time"),
                        message = sprintf(.("Survival time contains %d negative value(s). Follow-up time cannot be negative. Zero-time observations are retained because right-censored survival methods permit them."), n_invalid)
                    )
                    return(FALSE)
                }

                if (!any(data[[time_var]] > 0, na.rm = TRUE)) {
                    private$.addHtmlMessage(
                        type = "error",
                        title = .("No Positive Follow-up"),
                        message = .("At least one observation must have positive follow-up time. Zero-time observations may be retained, but an all-zero time variable cannot support survival estimation.")
                    )
                    return(FALSE)
                }

                if (any(is.na(data[[outcome_var]]))) {
                    n_missing <- sum(is.na(data[[outcome_var]]))
                    private$.addHtmlMessage(
                        type = "error",
                        title = .("Invalid Outcome"),
                        message = sprintf(.("Outcome/event indicator contains %d missing (NA) value(s) after data cleaning. Every observation must be classified as an event or as censored. Analysis was halted. Check the outcome variable and the selected event level."), n_missing)
                    )
                    return(FALSE)
                }

                if (!is.null(contexpl_var) && any(is.na(data[[contexpl_var]]))) {
                    n_missing <- sum(is.na(data[[contexpl_var]]))
                    pct_missing <- 100 * n_missing / nrow(data)
                    private$.addHtmlMessage(
                        type = "warning",
                        title = .("Missing Values in Explanatory Variable"),
                        message = sprintf(.("Variable \"%s\" has %d missing values (%.1f%%). These observations will be excluded from analysis. Consider investigating the pattern of missingness and whether imputation is appropriate."), contexpl_var, n_missing, pct_missing)
                    )
                }

                if (!is.null(contexpl_var) && any(!is.finite(data[[contexpl_var]]))) {
                    n_invalid <- sum(!is.finite(data[[contexpl_var]]))
                    private$.addHtmlMessage(
                        type = "error",
                        title = .("Non-Finite Continuous Variable"),
                        message = sprintf(.("The continuous explanatory variable contains %d infinite or non-numeric value(s). Analysis was halted."), n_invalid)
                    )
                    return(FALSE)
                }

                # Check for sufficient sample size
                if (nrow(data) < 20) {
                    private$.addHtmlMessage(
                        type = "strongWarning",
                        title = .("Very Small Sample"),
                        message = sprintf(.("Sample size (n=%d) is very small for survival analysis. Statistical inference is highly unreliable. Results should be considered exploratory only. Collect more data before drawing conclusions."), nrow(data))
                    )
                }

                return(TRUE)
            },

            # Large-data warning
            .checkMemoryUsage = function(data, warn_threshold = 50000) {
                n_rows <- nrow(data)

                if (n_rows > warn_threshold) {
                    private$.addHtmlMessage(
                        type = "info",
                        title = .("Large dataset detected"),
                        message = sprintf(
                            .("Dataset contains %d rows. Analysis may take longer than usual, especially with data-driven cut-off searches and diagnostic plots."),
                            n_rows
                        )
                    )
                }

                return(n_rows)
            },

            # Configurable checkpoint frequency for responsiveness
            .performCheckpoint = function(iteration, frequency = 5) {
                if (iteration %% frequency == 0) {
                    private$.checkpoint(FALSE)
                }
            },

            # Enhanced survival time points parsing with flexible options
            .parseSurvivalTimePoints = function(cutp_string, default_points = c(12, 36, 60)) {
                normalized <- if (is.null(cutp_string)) {
                    ""
                } else {
                    tolower(gsub("[[:space:]]+", "", cutp_string))
                }

                # "12, 36, 60" is the historical factory value. Treat it as
                # the unit-aware default so old saved analyses do not request
                # 12-, 36-, and 60-year estimates when the selected scale is years.
                if (normalized %in% c("", "default", "12,36,60")) {
                    return(default_points)
                }

                # Parse comma- or whitespace-separated values.
                #
                # The pattern was "[,\\\\s]+", i.e. the regex [,\s]+ -- comma, literal
                # BACKSLASH, or the letter "s". Whitespace was therefore not a
                # separator, so "6 12 24" parsed to NA and fell back to the defaults
                # while the table went on claiming to show the requested points. The
                # sibling parser in .personTimeAnalysis already uses [,\s]+.
                time_points <- tryCatch({
                    suppressWarnings(as.numeric(unlist(strsplit(cutp_string, "[,[:space:]]+"))))
                }, error = function(e) NA_real_)

                # Remove invalid values
                time_points <- time_points[!is.na(time_points) & time_points > 0]

                if (length(time_points) == 0) {
                    # Silently substituting defaults let the table label itself with
                    # times the user never asked for.
                    private$.addHtmlMessage(
                        "warning",
                        .("Survival time points not understood"),
                        sprintf(.("'%s' could not be read as a list of time points. Default points (%s) are shown instead. Enter positive numbers separated by commas."),
                                cutp_string, paste(default_points, collapse = ", ")))
                    return(default_points)
                }

                # Sort and return unique values
                return(sort(unique(time_points)))
            },

            # Flexible interval calculation with configurable multiplier
            .calculateTimeIntervals = function(time_var, max_multiplier = 1.1) {
                max_time <- max(time_var, na.rm = TRUE)
                return(max_time * max_multiplier)
            }
        ), # End of private list
        public = list(
            #' @description
            #' Generate R source code for survivalcont analysis
            #' @return Character string with R syntax for reproducible analysis
            asSource = function() {
                elapsedtime <- self$options$elapsedtime
                outcome <- self$options$outcome

                if (is.null(elapsedtime) || is.null(outcome))
                    return('')

                # Build the argument list in option-declaration order.
                #
                # This used to emit elapsedtime and outcome by hand AND then append
                # private$.asArgs(), which emits every option -- so both appeared
                # twice and the snippet failed with 'formal argument "elapsedtime"
                # matched by multiple actual arguments'. The manual escaping was also
                # wrong: it wrapped the name in backticks INSIDE a quoted string
                # ("`My Var`") and did nothing for names containing " or \.
                #
                # Every variable-name option is now emitted once, as a deparse()'d
                # string literal -- valid, fully escaped R for any column name.
                # Detecting by CLASS means variable options added later are handled
                # automatically. Matches condsurvival/finegray and siblings.
                args <- character(0)
                for (option in private$.options$options) {
                    if (option$name == "data")
                        next
                    if (inherits(option, "OptionVariable") || inherits(option, "OptionVariables")) {
                        val <- option$value
                        if (!is.null(val) && length(val) > 0)
                            args <- c(args, paste0(
                                option$name, " = ",
                                paste0(deparse(val), collapse = "")))
                    } else {
                        as <- private$.sourcifyOption(option)
                        if (!identical(as, ""))
                            args <- c(args, as)
                    }
                }

                # Get package name dynamically
                pkg_name <- utils::packageName()
                if (is.null(pkg_name)) pkg_name <- "ClinicoPath"  # fallback

                paste0(pkg_name, '::survivalcont(\n    data = data,\n    ',
                       paste(args, collapse = ",\n    "), ')')
            }
        ) # End of public list
    )
}
