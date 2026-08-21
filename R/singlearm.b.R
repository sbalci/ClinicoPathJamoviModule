#' @title Single Arm Survival
#' @importFrom R6 R6Class
#' @importFrom ggplot2 ggplot aes geom_text geom_line geom_point labs theme_void theme element_blank scale_x_continuous scale_y_continuous annotate
#' @importFrom gridExtra grid.arrange
#' @importFrom survminer ggsurvplot ggcompetingrisks
#'
#' @description
#' This function prepares and cleans data for single-arm survival analysis by
#' calculating survival time, filtering based on landmark time, and merging
#' survival outcomes with other factors.
#'
#' @return A list containing cleaned data and metadata for plotting and analysis.
#' @note Ensure the input data contains the required variables (elapsed time,
#' outcome) and meets specified formatting criteria.

#' @noRd
NULL

# Helper function to create styled HTML notice (replaces jmvcore::Notice to avoid serialization errors)
.singlearmNoticeHTML <- function(message, type = c("ERROR", "STRONG_WARNING", "WARNING", "INFO")) {
    type <- match.arg(type)

    # Define styles for each notice type
    styles <- list(
        ERROR = list(
            bg = "#f8d7da",
            border = "#dc3545",
            icon = "",
            title_color = "#721c24"
        ),
        STRONG_WARNING = list(
            bg = "#fff3cd",
            border = "#ff9800",
            icon = "",
            title_color = "#856404"
        ),
        WARNING = list(
            bg = "#fff3cd",
            border = "#ffc107",
            icon = "",
            title_color = "#856404"
        ),
        INFO = list(
            bg = "#d1ecf1",
            border = "#17a2b8",
            icon = "",
            title_color = "#0c5460"
        )
    )

    style <- styles[[type]]

    html <- paste0(
        "<div style='background-color: ", style$bg, "; ",
        "padding: 15px; margin: 10px 0; border-radius: 5px; ",
        "border-left: 4px solid ", style$border, ";'>",
        "<p style='margin: 0; color: ", style$title_color, ";'>",
        "<strong>", style$icon, " ", type, ":</strong> ",
        htmltools::htmlEscape(message),
        "</p>",
        "</div>"
    )

    return(html)
}

singlearmClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "singlearmClass",
    inherit = singlearmBase,
    # Constants and cache
    private = list(
        .cache = new.env(parent = emptyenv()),
        # Result of .defineEventIndicator(), kept so .run() can render the
        # recode disclosure without redoing the work.
        .eventRecode = NULL,
        # row_names + the calculated time / recoded outcome for EVERY row of the
        # source data, before complete-case filtering and before the landmark
        # shift. The "add to data" exports are written from these, so that the
        # exported columns mean what their names say.
        .rawTime = NULL,
        .rawOutcome = NULL,
        .errorMessages = character(0),
        .warningMessages = character(0),
        .infoMessages = character(0),

      .init = function() {
          # R6 field defaults are shallow-copied. An environment declared in
          # `private = list()` is therefore shared by every instance unless it
          # is replaced here. A shared cache retained cleaned data and survfit
          # objects from unrelated analyses for the lifetime of the R session.
          # Keep the cache analysis-local; .run() also empties it so old data
          # from earlier states of this analysis are not retained indefinitely.
          private$.cache <- new.env(parent = emptyenv())

          # Initialize all outputs to FALSE first
          self$results$medianSummary$setVisible(FALSE)
          self$results$survTableSummary$setVisible(FALSE)
          self$results$personTimeHeading2$setVisible(FALSE)
          self$results$plot$setVisible(FALSE)
          self$results$plot2$setVisible(FALSE)
          self$results$plot3$setVisible(FALSE)
          self$results$plot6$setVisible(FALSE)
          self$results$medianSurvivalExplanation$setVisible(FALSE)
          self$results$survivalPlotsHeading3$setVisible(FALSE)
          self$results$medianHeading3$setVisible(FALSE)
          self$results$survivalProbabilityExplanation$setVisible(FALSE)
          self$results$personTimeHeading$setVisible(FALSE)
          self$results$personTimeTable$setVisible(FALSE)
          self$results$personTimeSummary$setVisible(FALSE)
          self$results$personTimeHeading3$setVisible(FALSE)
          self$results$personTimeExplanation$setVisible(FALSE)
          self$results$survivalPlotsExplanation$setVisible(FALSE)
          self$results$baselineHazardHeading$setVisible(FALSE)
          self$results$baselineHazardTable$setVisible(FALSE)
          self$results$baselineHazardPlot$setVisible(FALSE)
          self$results$smoothedHazardPlot$setVisible(FALSE)
          self$results$baselineHazardSummary$setVisible(FALSE)
          self$results$baselineHazardHeading3$setVisible(FALSE)
          self$results$baselineHazardExplanation$setVisible(FALSE)
          self$results$dataQualityHeading$setVisible(FALSE)
          self$results$dataQualityTable$setVisible(FALSE)
          self$results$dataQualitySummary$setVisible(FALSE)
          # Section headings for the median / survival tables start hidden and
          # are revealed only once their analyses populate, so empty titles do
          # not show alongside the welcome/todo message.
          self$results$medianHeading$setVisible(FALSE)
          self$results$survTableHeading$setVisible(FALSE)

          # Handle showSummaries visibility
          if (self$options$showSummaries) {
            self$results$medianSummary$setVisible(TRUE)
            self$results$survTableSummary$setVisible(TRUE)
            # Person-time summary (and its heading) require both showSummaries AND person_time
            if (self$options$person_time) {
              self$results$personTimeHeading2$setVisible(TRUE)
              self$results$personTimeSummary$setVisible(TRUE)
            }
          }

          # Handle showExplanations visibility
          if (self$options$showExplanations) {
            self$results$medianHeading3$setVisible(TRUE)
            self$results$medianSurvivalExplanation$setVisible(TRUE)
            self$results$survivalProbabilityExplanation$setVisible(TRUE)
            
            # Survival plots explanation requires showExplanations AND at least one plot
            if (self$options$sc || self$options$ce || self$options$ch || self$options$kmunicate) {
              self$results$survivalPlotsHeading3$setVisible(TRUE)
              self$results$survivalPlotsExplanation$setVisible(TRUE)
            }
            
            # Person-time explanation requires both showExplanations AND person_time
            if (self$options$person_time) {
              self$results$personTimeHeading3$setVisible(TRUE)
              self$results$personTimeExplanation$setVisible(TRUE)
            }
          }

          # Handle person_time visibility
          if (self$options$person_time) {
            self$results$personTimeHeading$setVisible(TRUE)
            self$results$personTimeTable$setVisible(TRUE)
          }

          # Handle baseline hazard visibility
          if (self$options$baseline_hazard) {
            self$results$baselineHazardHeading$setVisible(TRUE)
            self$results$baselineHazardTable$setVisible(TRUE)
            self$results$baselineHazardPlot$setVisible(TRUE)
            # Summary requires both baseline_hazard AND showSummaries
            if (self$options$showSummaries) {
              self$results$baselineHazardSummary$setVisible(TRUE)
            }
            # Explanation requires both baseline_hazard AND showExplanations
            if (self$options$showExplanations) {
              self$results$baselineHazardHeading3$setVisible(TRUE)
              self$results$baselineHazardExplanation$setVisible(TRUE)
            }
          }

          # Handle hazard smoothing visibility
          if (self$options$hazard_smoothing) {
            self$results$smoothedHazardPlot$setVisible(TRUE)
          }

          # Handle advanced diagnostics visibility
          if (self$options$advancedDiagnostics) {
            self$results$dataQualityHeading$setVisible(TRUE)
            self$results$dataQualityTable$setVisible(TRUE)
            # Summary requires both advancedDiagnostics AND showSummaries
            if (self$options$showSummaries) {
              self$results$dataQualitySummary$setVisible(TRUE)
            }
          }

          # Handle plot visibility based on their options
          if (self$options$sc) {
            self$results$plot$setVisible(TRUE)
          }
          if (self$options$ce) {
            self$results$plot2$setVisible(TRUE)
          }
          if (self$options$ch) {
            self$results$plot3$setVisible(TRUE)
          }
          if (self$options$kmunicate) {
            self$results$plot6$setVisible(TRUE)
          }

      },

      # Message Accumulation Methods (to avoid serialization errors from dynamic Notices) ----
      .addError = function(message) {
        private$.errorMessages <- c(private$.errorMessages, message)
      },

      .addWarning = function(message) {
        private$.warningMessages <- c(private$.warningMessages, message)
      },

      .addInfo = function(message) {
        private$.infoMessages <- c(private$.infoMessages, message)
      },

      .clearMessages = function() {
        private$.errorMessages <- character(0)
        private$.warningMessages <- character(0)
        private$.infoMessages <- character(0)

        # Html contents and visibility persist across .run() calls. Reset the
        # actual result objects as well as the in-memory message vectors, so an
        # incomplete or newly invalid configuration cannot show a notice from
        # the preceding analysis state.
        for (nm in c("errors", "warnings", "info")) {
          self$results[[nm]]$setContent("")
          self$results[[nm]]$setVisible(FALSE)
        }
      },

      .resetRunOutputs = function() {
        # Tables, Html contents, notes and image states can all outlive a call
        # to .run(). `clearWith` remains the normal jamovi invalidation layer,
        # but an explicit reset is also required for programmatic reruns and
        # for early-return paths reached after an option combination becomes
        # invalid.
        for (nm in c("medianTable", "survTable", "personTimeTable",
                     "baselineHazardTable", "dataQualityTable")) {
          try(self$results[[nm]]$deleteRows(), silent = TRUE)
        }

        for (spec in list(
          c("medianTable", "rmst"), c("medianTable", "cr_ci"),
          c("survTable", "cif_note"), c("survTable", "boundary_ci"),
          c("personTimeTable", "ci"),
          c("baselineHazardTable", "method"),
          c("dataQualityTable", "grading"))) {
          try(self$results[[spec[1]]]$setNote(spec[2], NULL), silent = TRUE)
        }

        for (nm in c("eventRecodeInfo", "clinicalSummary", "medianSummary",
                     "medianSurvivalExplanation", "survTableSummary",
                     "survivalProbabilityExplanation", "personTimeSummary",
                     "personTimeExplanation", "survivalPlotsExplanation",
                     "baselineHazardSummary", "baselineHazardExplanation",
                     "dataQualitySummary")) {
          try(self$results[[nm]]$setContent(""), silent = TRUE)
        }

        for (nm in c("plot", "plot_cif", "plot2", "plot3", "plot6",
                     "baselineHazardPlot", "smoothedHazardPlot")) {
          try(self$results[[nm]]$setState(NULL), silent = TRUE)
        }

        self$results$medianHeading$setVisible(FALSE)
        self$results$survTableHeading$setVisible(FALSE)
        self$results$plot$setVisible(FALSE)
        self$results$plot_cif$setVisible(FALSE)
      },

      .displayMessages = function() {
        # Display accumulated error messages
        if (length(private$.errorMessages) > 0) {
          html_content <- paste(sapply(private$.errorMessages, function(msg) {
            .singlearmNoticeHTML(msg, "ERROR")
          }), collapse = "")
          self$results$errors$setContent(html_content)
          self$results$errors$setVisible(TRUE)
        } else {
          self$results$errors$setVisible(FALSE)
        }

        # Display accumulated warning messages
        if (length(private$.warningMessages) > 0) {
          html_content <- paste(sapply(private$.warningMessages, function(msg) {
            # Determine if it's a STRONG_WARNING or regular WARNING based on keywords
            type <- if (grepl("Very few events|critically", msg, ignore.case = TRUE)) "STRONG_WARNING" else "WARNING"
            .singlearmNoticeHTML(msg, type)
          }), collapse = "")
          self$results$warnings$setContent(html_content)
          self$results$warnings$setVisible(TRUE)
        } else {
          self$results$warnings$setVisible(FALSE)
        }

        # Display accumulated info messages
        if (length(private$.infoMessages) > 0) {
          html_content <- paste(sapply(private$.infoMessages, function(msg) {
            .singlearmNoticeHTML(msg, "INFO")
          }), collapse = "")
          self$results$info$setContent(html_content)
          self$results$info$setVisible(TRUE)
        } else {
          self$results$info$setVisible(FALSE)
        }
      },

      # Utility Helper Functions ----
      .isCompetingRisk = function(state = NULL) {
        # Check if competing risk analysis is active.
        #
        # The STATUS VECTOR decides this, not the options. This used to read
        # `self$options$multievent && analysistype == "compete"` alone, which is
        # blind to the outcomeorganizer hand-off: a recoded
        # Censored/Event/Competing column arrives already 0/1/2 with
        # multievent = FALSE -- the user never fills dod/dooc/awd/awod, that is
        # the whole point of the recoded column. The guard was therefore FALSE
        # and the 0/1/2 vector went into an ordinary survival::Surv(), which for
        # a max status of 2 subtracts 1 and NAs anything outside 0/1: Censored
        # became NA (row silently DELETED), Event became censored, and Competing
        # became the event. A 6-row fixture reported 4 records, 2 events and a
        # median at the midpoint of the competing times. If this ever reverts to
        # testing the options alone, competing-risk data is analysed backwards
        # again with no warning.
        #
        # `state` is a plot's image$state. jmvcore's .load() restores results
        # (and image states) from disk without calling .run(), so a renderer can
        # execute in an instance where private$.eventRecode is still NULL; the
        # flag then has to come off the serialised state.
        isTRUE(state$has_competing) ||
          isTRUE(private$.eventRecode$has_competing) ||
          (isTRUE(self$options$multievent) &&
             identical(self$options$analysistype, "compete"))
      },

      .estimandMeta = function(state = NULL) {
        estimand <- state$estimand
        if (is.null(estimand) && !is.null(private$.eventRecode))
          estimand <- private$.eventRecode$estimand
        if (is.null(estimand) || length(estimand) == 0)
          estimand <- ""
        estimand <- as.character(estimand[[1]])

        switch(estimand,
          "overall survival" = list(
            probability = "Overall survival",
            median = "Median overall survival",
            median_lower = "median overall survival",
            curve = "Overall Survival"),
          "cause-specific survival" = list(
            probability = "Cause-specific survival",
            median = "Median cause-specific survival",
            median_lower = "median cause-specific survival",
            curve = "Cause-Specific Survival"),
          "disease-free survival" = list(
            probability = "Disease-free survival",
            median = "Median disease-free survival",
            median_lower = "median disease-free survival",
            curve = "Disease-Free Survival"),
          list(
            probability = "Kaplan-Meier event-free probability",
            median = "Median event-free time",
            median_lower = "median event-free time",
            curve = "Event-Free Probability for the Selected Event")
        )
      },

      .yearInUnits = function() {
        # One year expressed in the selected display unit, used for default
        # cutpoints and time-scale plausibility checks. These used to hard-code
        # month values, so selecting days or years silently changed their
        # meaning.
        switch(self$options$timetypeoutput,
          "days"   = 365.25,
          "weeks"  = 52.18,
          "months" = 12,
          "years"  = 1,
          12)
      },

      .getDefaultCutpoints = function() {
        round(c(1, 3, 5) * private$.yearInUnits())
      },

      .resolveCutpoints = function(optString, what = "Cutpoints", allow_zero = TRUE) {
        # Values are ALWAYS used exactly as typed.
        #
        # The default string is written in MONTHS ('12, 36, 60') and jamovi
        # gives no way to tell "the user never touched this box" from "the user
        # deliberately typed these numbers". A previous pass guessed
        # "untouched", threw away the parsed values, substituted 1/3/5 years and
        # announced "Enter your own values to override this" -- to a user who
        # had just entered exactly those values. Guessing wrong silently changes
        # the analysis, so we honour the input and only say what it means.
        #
        # Sorted (and de-duplicated, by .parseNumericList) centrally because
        # every consumer needs an ascending vector: summary.survfit() does NOT
        # sort `times`, and its n.event is "events since the previous element of
        # the SUPPLIED vector". An unsorted request therefore neither partitions
        # follow-up nor cumulates -- while the survival column stays perfectly
        # correct, which is what made the broken events column look trustworthy.
        nums <- private$.parseNumericList(optString)
        malformed <- attr(nums, "malformed")
        nums <- sort(nums)

        # Do not silently discard typographical errors such as "l2" in
        # "6, l2, 24". The usable numeric values can still be analysed, but the
        # ignored tokens must be visible so a mistyped clinical milestone is
        # not mistaken for an intentionally omitted row.
        if (length(malformed) > 0) {
          private$.addWarning(sprintf(
            '%s contain non-numeric value(s): %s ignored. Enter comma-separated numeric time points.',
            what, paste(sprintf('"%s"', malformed), collapse = ", ")))
        }

        # Domain check. Every consumer of this function treats the values as
        # elapsed time measured from the start of follow-up, so only finite,
        # strictly positive numbers mean anything:
        #   * cutp = "-2, 1.5, 4" reported a survival probability AT TIME -2
        #     (summary.survfit(extend = TRUE) happily carries S(t) = 1 backwards);
        #   * time_intervals = "-5, 5" built breaks c(0, -5, 5, ...) and the
        #     interval "-5 to 5" accrued 90 units of person-time from a cohort
        #     that contains 55 units in total, because every subject was credited
        #     with the 5 units between -5 and 0 that nobody was ever observed for.
        # A survival/CIF cutpoint at time zero is legitimate when an event is
        # recorded at the origin: survival::survfit() reports the post-event
        # estimate there. A zero-width PERSON-TIME boundary is not useful, so
        # that caller opts out explicitly with allow_zero = FALSE.
        # Separate REDUNDANT from INVALID.
        #
        # For person-time (allow_zero = FALSE) a leading 0 is not an error: the
        # caller prepends 0 to `breaks` itself, so the first interval is 0-t1
        # either way. Lumping it in with negatives produced a warning reading
        # "0 ignored" on the entirely ordinary input "0, 12, 24" -- while the
        # table was byte-identical to "12, 24" and its first row WAS 0-12. The
        # warning was simply false, and false warnings on valid input teach
        # users to ignore all of them.
        invalid <- !is.finite(nums) | nums < 0
        if (any(invalid)) {
          private$.addWarning(sprintf(
            '%s must be finite and zero or positive: %s ignored. Time is measured forward from the start of follow-up, so a negative or infinite time point is not valid.',
            what, paste(base::format(nums[invalid], trim = TRUE), collapse = ", ")))
          nums <- nums[!invalid]
        }
        # Drop a redundant zero SILENTLY for callers that supply their own
        # origin. Nothing is lost and nothing needs saying.
        if (!allow_zero) nums <- nums[nums > 0]
        if (length(nums) == 0) {
          nums <- private$.getDefaultCutpoints()
          private$.addWarning(sprintf(
            'No usable %s remained, so the built-in defaults (%s %s) were used instead. Enter one or more valid time points to override them.',
            tolower(what), paste(nums, collapse = ", "), self$options$timetypeoutput))
        }

        if (identical(gsub("[[:space:]]", "", as.character(optString)), "12,36,60") &&
            !identical(self$options$timetypeoutput, "months")) {
          private$.addInfo(sprintf(
            '%s were used exactly as entered (%s) and are read in %s, the selected time unit. Note that "12, 36, 60" is also the built-in default, which is written in months; if you meant 1, 3 and 5 years, enter %s.',
            what, paste(nums, collapse = ", "), self$options$timetypeoutput,
            paste(private$.getDefaultCutpoints(), collapse = ", ")))
        }
        nums
      },

      .supportedCutpoints = function(utimes, time, status) {
        # Drop requested times the data cannot support -- but ONLY those that
        # genuinely are unsupported.
        #
        # Both survival tables call summary(fit, times = ..., extend = TRUE),
        # which carries the last estimate forward forever: on a cohort with 6
        # months of follow-up it happily printed a survival probability, a
        # confidence interval and "Number at Risk = 0" at 24 and 120 months.
        #
        # But that is only meaningless when the LAST observation is CENSORED.
        # When every subject observed at the longest follow-up time had an
        # event, the risk set is empty because everyone had the event: S(t) = 0
        # exactly, and stays 0, for all later t. "5-year survival 0%" is a fully
        # defined number and the ordinary situation in aggressive-disease
        # series. A previous pass dropped those rows too and told the reader the
        # estimate "carries no information", deleting a valid and clinically
        # important zero.
        max_followup <- max(time, na.rm = TRUE)
        beyond <- utimes > max_followup
        if (!any(beyond)) return(utimes)

        # Any non-zero status is an event: a competing event also removes the
        # subject from the risk set, so the curve is equally determined after it.
        tail_status <- status[!is.na(time) & time == max_followup]
        exhausted <- length(tail_status) > 0 &&
          all(!is.na(tail_status) & tail_status != 0)

        if (exhausted) {
          final_text <- if (private$.isCompetingRisk())
            paste0('the cumulative-incidence curves have reached their final values and ',
                   'remain unchanged. The requested rows report those final state probabilities')
          else
            paste0('no one remains event-free, so survival has reached 0% and remains there. ',
                   'The requested rows report 0% survival with no confidence interval')
          private$.addInfo(sprintf(
            'Requested time point(s) %s lie beyond the longest follow-up in the data (%s %s). Every subject observed at that time had a terminal event, so %s.',
            paste(utimes[beyond], collapse = ", "),
            format(round(max_followup, 1)), self$options$timetypeoutput,
            final_text))
          return(utimes)
        }

        private$.addWarning(sprintf(
          'Requested time point(s) %s are beyond the longest follow-up in the data (%s %s) and were omitted: the longest observation is censored, so subjects were still event-free when observation stopped and the estimate is undefined past that point.',
          paste(utimes[beyond], collapse = ", "),
          format(round(max_followup, 1)), self$options$timetypeoutput))
        utimes[!beyond]
      },

      .ciText = function(lower, upper) {
        # Blank rather than "[NA-NA, 95% CI]" when the interval is undefined --
        # which is exactly what survfit returns at a time where the curve has
        # reached 0 (see .supportedCutpoints).
        ifelse(is.na(lower) | is.na(upper), "",
               paste0(" [", scales::percent(lower), "-",
                      scales::percent(upper), ", 95% CI]"))
      },

      .medianFollowUp = function(time, status) {
        # Reverse Kaplan-Meier (Schemper & Smith): the censoring indicator
        # becomes the "event", so the estimate answers "how long was this
        # cohort actually watched".
        #
        # This used to be median(observed times), which is the median time to
        # event-or-censoring. In a cohort where most subjects have the event
        # early that number is the median SURVIVAL, not the median follow-up,
        # and it understates the observation window -- exactly the number a
        # reader uses to judge whether a 5-year estimate is supported at all.
        fallback <- list(value = stats::median(time, na.rm = TRUE), reverse = FALSE)
        cens <- as.integer(!is.na(status) & status == 0)
        if (sum(cens) == 0) return(fallback)
        fit <- try(survival::survfit(survival::Surv(time, cens) ~ 1), silent = TRUE)
        if (inherits(fit, "try-error")) return(fallback)
        m <- unname(summary(fit)$table[["median"]])
        if (is.na(m)) return(fallback)
        list(value = m, reverse = TRUE)
      },

      .safeExecute = function(expr, context = "analysis", silent = FALSE) {
        tryCatch(expr, error = function(e) {
          user_msg <- switch(context,
            "data_processing" = .("Data processing failed. Please check your input variables."),
            "survival_calculation" = .("Survival calculation failed. This may be due to insufficient data or data quality issues."),
            "plot_generation" = .("Plot generation failed. Try adjusting plot parameters or checking data quality."),
            "baseline_hazard" = .("Piecewise hazard-rate calculation failed. This may occur with very sparse data."),
            "person_time" = .("Person-time analysis failed. Please check time intervals and event data."),
            paste("An error occurred during", context)
          )
          
          if (!silent) {
            warning(paste(user_msg, .("Technical details:"), e$message))
          }
          
          return(NULL)
        })
      },

      .validatePlotParameters = function(check_y = TRUE) {
        # Validate plot end time
        if (!is.finite(self$options$endplot) || self$options$endplot <= 0) {
          private$.addError('Plot end time must be a finite positive number. Please enter a valid positive number for the maximum time to display on plots.')
          return(FALSE)
        }

        # Validate the tick interval. break.time.by = 0 or a negative value
        # produces an unusable axis (survminer builds seq(0, xlim, by = ...)),
        # and nothing checked it -- the option is only ever read straight into
        # the plot call.
        if (!is.finite(self$options$byplot) || self$options$byplot <= 0) {
          private$.addError('Time interval between axis ticks must be a finite positive number. Please enter a positive value for "Time Interval".')
          return(FALSE)
        }

        # Validate Y-axis range.
        #
        # The axis carries a survival PROBABILITY, so it only has meaning inside
        # 0-1; both option descriptions already say "a number between 0 and 1"
        # and nothing enforced it, so a range of -5 to 50 drew a probability
        # curve squashed against the floor of a nonsensical axis.
        #
        # Known ceiling: this also rejects a deliberate 1.05 used purely for
        # head-room above the curve. Head-room on a bounded scale is not worth a
        # second option; use the plot's own margins instead.
        if (check_y) {
          y0 <- self$options$ybegin_plot
          y1 <- self$options$yend_plot
          if (!is.finite(y0) || !is.finite(y1) || y0 < 0 || y1 > 1) {
            private$.addError(sprintf('Y-axis limits must lie within 0 and 1 (received %s to %s): the axis shows a survival probability. Please adjust plot axis settings.',
                                      format(y0), format(y1)))
            return(FALSE)
          }
          if (y0 >= y1) {
            private$.addError('Y-axis range invalid: start value must be less than end value. Please adjust plot axis settings.')
            return(FALSE)
          }
        }

        return(TRUE)
      },

      .getCachedSurvfit = function(formula, data, cache_key_suffix = "") {
        if (!requireNamespace('digest', quietly = TRUE)) {
          # Fallback if digest not available
          return(survival::survfit(formula, data = data))
        }
        
        cache_key <- paste0("survfit_", 
                           digest::digest(list(as.character(formula), data, cache_key_suffix)))
        
        if (exists(cache_key, envir = private$.cache)) {
          return(get(cache_key, envir = private$.cache))
        }
        
        result <- survival::survfit(formula, data = data)
        assign(cache_key, result, envir = private$.cache)
        return(result)
      },

      .calculateAdaptiveSpan = function(n_points) {
        # More sophisticated span calculation based on data characteristics
        if (n_points <= 10) return(0.8)
        if (n_points <= 30) return(0.5)
        if (n_points <= 60) return(0.3)
        
        # For larger datasets, use logarithmic scaling
        base_span <- 0.75 / log10(n_points + 1)
        return(pmax(0.1, pmin(0.8, base_span)))
      },

      .systematicSample = function(data, target_size = 50) {
        n <- nrow(data)
        if (n <= target_size) return(data)

        # Use systematic sampling to preserve distribution
        keep_indices <- round(seq(1, n, length.out = target_size))
        return(data[keep_indices, ])
      },

      .hazardIntervals = function(time, status, target_events = 10L,
                                  max_bins = 10L) {
        # Nonparametric data do not identify an "instantaneous" hazard at each
        # observed event time. Estimate descriptive piecewise rates instead,
        # using equal-width intervals and the exact person-time accrued by every
        # subject inside each interval.
        keep <- is.finite(time) & !is.na(status) & time >= 0
        time <- as.numeric(time[keep])
        status <- as.integer(status[keep] == 1)
        if (length(time) == 0 || sum(status) == 0)
          return(data.frame())

        # An event at the time origin is an atom in the event-time
        # distribution, not an occurrence generated over a positive amount of
        # person-time. Putting it in the first positive-width interval would
        # manufacture a finite rate by dividing that mass by other subjects'
        # later exposure. KM/CIF can retain such events, but this continuous-
        # time rate summary cannot represent them faithfully.
        if (any(status == 1 & time == 0))
          return(data.frame())

        max_fu <- max(time)
        n_events <- sum(status)
        if (!is.finite(max_fu) || max_fu <= 0)
          return(data.frame(start = 0, end = max_fu, events = n_events,
                            person_time = 0, rate = NA_real_, lower = NA_real_,
                            upper = NA_real_))

        n_bins <- max(1L, min(as.integer(max_bins),
                              floor(n_events / as.integer(target_events))))

        # Boundaries must not be chosen from the observed event quantiles and
        # then fed to a Poisson interval as though the bins had been fixed.
        # That post-selection forces similar event counts into every row and
        # invalidates the stated interval model. Equal-width boundaries depend
        # on the follow-up scale, not on where target events happened.
        breaks <- seq(0, max_fu, length.out = n_bins + 1L)
        if (length(breaks) < 2L) return(data.frame())

        rows <- lapply(seq_len(length(breaks) - 1L), function(i) {
          start <- breaks[i]
          end <- breaks[i + 1L]
          at_event <- status == 1 & time <= end &
            if (i == 1L) time >= start else time > start
          events <- sum(at_event)
          person_time <- sum(pmax(0, pmin(time, end) - start))
          rate <- if (person_time > 0) events / person_time else NA_real_
          lower <- if (person_time > 0 && events > 0)
            stats::qchisq(0.025, 2 * events) / 2 / person_time else
            if (person_time > 0) 0 else NA_real_
          upper <- if (person_time > 0)
            stats::qchisq(0.975, 2 * (events + 1)) / 2 / person_time else NA_real_
          data.frame(start = start, end = end, events = events,
                     person_time = person_time, rate = rate,
                     lower = lower, upper = upper)
        })
        do.call(rbind, rows)
      },

      .parseNumericList = function(x, default_vals) {
        if (is.null(x) || length(x) == 0 || all(is.na(x))) {
          nums <- numeric(0)
          attr(nums, "malformed") <- character(0)
          if (length(nums) == 0 && !missing(default_vals))
            nums <- default_vals
          return(nums)
        }

        tokens <- trimws(unlist(strsplit(as.character(x), ",", fixed = TRUE)))
        tokens <- tokens[!is.na(tokens) & nzchar(tokens)]
        parsed <- suppressWarnings(as.numeric(tokens))
        malformed <- unique(tokens[is.na(parsed)])
        nums <- unique(parsed[!is.na(parsed)])
        attr(nums, "malformed") <- malformed
        if (length(nums) == 0 && !missing(default_vals)) {
          nums <- default_vals
        }
        return(nums)
      },

      .assessDataQuality = function(results) {
        mydata <- results$cleanData
        mytime <- results$name1time
        myoutcome <- results$name2outcome

        # Basic data quality metrics
        n_total <- nrow(mydata)
        # Count the EVENT OF INTEREST only.
        #
        # `>= 1` swept competing events (code 2) in with target events (code 1),
        # so a cohort with 2 disease deaths and 88 deaths from other causes read
        # as 90 events and the "fewer than 10 events" warning never fired -- on a
        # dataset with far too few events to support any inference about the
        # event of interest. For competing risks, a competing event is not an
        # event of interest; it is a distinct terminal state.
        n_events    <- sum(mydata[[myoutcome]] == 1, na.rm = TRUE)
        n_competing <- sum(mydata[[myoutcome]] == 2, na.rm = TRUE)
        n_censored  <- n_total - n_events - n_competing

        if (n_competing > 0) {
            private$.addInfo(sprintf(
                paste0("%d competing event(s) are present. Counts and the minimum-event ",
                "summaries below refer to the event of interest only; competing events ",
                       "are a separate terminal state, not events."),
                n_competing))
        }
        
        # Time-related quality checks
        time_vals <- mydata[[mytime]]
        min_time <- min(time_vals, na.rm = TRUE)
        max_time <- max(time_vals, na.rm = TRUE)

        # Median FOLLOW-UP, by reverse Kaplan-Meier -- not median(time_vals),
        # which is the median time to event-or-censoring. See .medianFollowUp().
        mfu <- private$.medianFollowUp(time_vals, mydata[[myoutcome]])

        # Data quality warnings.
        #
        # Event scarcity is NOT assessed here. It used to be, in a shorter and
        # vaguer form ("Very few events observed - results may be unreliable",
        # "Low event rate - consider longer follow-up"), while .run() emitted its
        # own richer notices from the same two numbers -- so every low-event
        # cohort collected two near-identical warnings from here plus two more
        # from there. One assessment, in one place: see .run().
        #
        # `event_rate` is still returned below, as a number for the caller to
        # report. It is not graded: the proportion of a cohort that has had the
        # event is a property of the disease and of when the data were censused,
        # not of the quality of the data.
        warnings <- character()

        # No universal follow-up-duration threshold is clinically defensible.
        # A 90-day window may be complete for an acute endpoint and inadequate
        # for an indolent cancer. Estimability, risk sets, and confidence
        # intervals are reported directly instead of grading against one year.

        return(list(
          n_total = n_total,
          n_events = n_events,
          n_competing = n_competing,
          n_censored = n_censored,
          event_rate = if (n_total > 0) round(n_events / n_total * 100, 1) else NA_real_,
          median_followup = round(mfu$value, 1),
          median_followup_reverse_km = mfu$reverse,
          min_time = round(min_time, 1),
          max_time = round(max_time, 1),
          warnings = warnings
        ))
      },

      # Validation Helper Function ----
      .validateInputs = function() {
        ### Define subconditions ----
        subcondition1a <- !is.null(self$options$outcome)
        subcondition1b1 <- self$options$multievent
        subcondition1b2 <- !is.null(self$options$dod)
        subcondition1b3 <- !is.null(self$options$dooc)
        subcondition2a <- !is.null(self$options$elapsedtime)
        subcondition2b1 <- self$options$tint
        subcondition2b2 <- !is.null(self$options$dxdate)
        subcondition2b3 <- !is.null(self$options$fudate)

        # Outcome validation: either simple outcome OR multi-event with all necessary levels
        if (!subcondition1b1) {
          outcome_valid <- subcondition1a
        } else {
          # Multievent is "configured enough to run" once the outcome variable
          # and at least ONE category mapping are present. Everything past that
          # -- unmapped levels, a level assigned twice, an unknown analysis
          # type -- is decided by .defineEventIndicator() in survival_utils.R,
          # which returns a specific error the user can act on.
          #
          # This used to demand all four mappings and then check each against
          # levels(self$data[[outcome]]). Both halves were wrong. The shared
          # recoder deliberately allows an EMPTY category (a cohort with nobody
          # Alive with Disease is ordinary), so requiring four blanked the
          # analysis into the welcome screen with no explanation. And levels()
          # is NULL for a numeric outcome, so `val %in% NULL` was FALSE for
          # every mapping and numeric multievent outcomes could never run at
          # all -- while .defineEventIndicator() compares as.character() and
          # handles them fine.
          mappings <- list(self$options$dod, self$options$dooc,
                           self$options$awd, self$options$awod)
          any_mapped <- any(vapply(mappings,
                                   function(m) !is.null(m) && length(m) > 0,
                                   logical(1)))
          outcome_valid <- subcondition1a && any_mapped
        }

        # Time validation: either date calculation OR pre-calculated time
        # Only the ACTIVE input path matters. jamovi can retain stale date
        # selections after the user turns date calculation off; requiring those
        # inactive options to be NULL made a perfectly configured elapsed-time
        # analysis fall back to the welcome screen. The converse is the same:
        # an old elapsed-time selection is ignored while dates are active.
        time_valid <- (subcondition2b1 && subcondition2b2 && subcondition2b3) ||
                     (!subcondition2b1 && subcondition2a)

        # Check if variables exist in data
        if (subcondition1a && !self$options$outcome %in% names(self$data)) {
            outcome_valid <- FALSE
        }

        if (subcondition2a && !self$options$elapsedtime %in% names(self$data)) {
            time_valid <- FALSE
        }
        if (subcondition2b1) {
            if (is.null(self$options$dxdate) || is.null(self$options$fudate)) {
              time_valid <- FALSE
            } else if (!all(c(self$options$dxdate, self$options$fudate) %in% names(self$data))) {
              time_valid <- FALSE
            }
        }

        return(list(
          outcome_valid = outcome_valid,
          time_valid = time_valid,
          continue_analysis = outcome_valid && time_valid
        ))
      },

      # get and label Data ----
      .getData = function() {

        # Memoize: janitor::clean_names + labelled on the whole dataset is
        # expensive and .getData() is called several times per run. Cache the
        # result keyed on the data plus the options that drive the name lookups
        # (mirrors the caching pattern used by .getCachedSurvfit).
        cache_key <- NULL
        if (requireNamespace('digest', quietly = TRUE)) {
          cache_key <- paste0("getData_", digest::digest(list(
            self$data,
            self$options$elapsedtime,
            self$options$outcome,
            self$options$dxdate,
            self$options$fudate
          )))
          if (exists(cache_key, envir = private$.cache)) {
            return(get(cache_key, envir = private$.cache))
          }
        }

        # Prepend a private join key before name cleaning. Assigning with
        # `mydata$row_names <-` overwrote a user's real variable named
        # `row_names`; selecting that variable as time or outcome then analysed
        # the internal row numbers instead of the supplied measurements.
        source_data <- self$data
        mydata <- data.frame(
          # Output row numbers are positional jamovi row numbers. Custom R
          # data-frame row names may be non-numeric and are not a safe join or
          # export key.
          row_names = seq_len(nrow(source_data)),
          source_data,
          check.names = FALSE,
          stringsAsFactors = FALSE
        )

        # The first label belongs to the private key. If the user also supplied
        # `row_names`, clean_names() calls it row_names_2 and retains its real
        # label, while the internal key remains the hard-coded join column.
        original_names <- c("..ClinicoPath internal row id..", names(source_data))

        mydata <- mydata %>% janitor::clean_names()

        corrected_labels <-
          setNames(original_names, names(mydata))

        mydata <- labelled::set_variable_labels(.data = mydata,
                                                .labels = corrected_labels)

        all_labels <- labelled::var_label(mydata)


        mytime <-
          names(all_labels)[all_labels == self$options$elapsedtime]

        myoutcome <-
          names(all_labels)[all_labels == self$options$outcome]

        mydxdate <-
          names(all_labels)[all_labels == self$options$dxdate]

        myfudate <-
          names(all_labels)[all_labels == self$options$fudate]

        result <- list(
          "mydata_labelled" = mydata
          , "mytime_labelled" = mytime
          , "myoutcome_labelled" = myoutcome
          , "mydxdate_labelled" = mydxdate
          , "myfudate_labelled" = myfudate
        )

        if (!is.null(cache_key))
          assign(cache_key, result, envir = private$.cache)

        return(result)


      }

      # todo ----
      ,
      .todo = function() {

        todo <- glue::glue(
          "
    <b>Welcome to Single-Arm Survival Analysis</b>
    <br><br>
    This tool analyzes survival outcomes for a single cohort of patients, calculating:
    <ul>
        <li><b>Median time-to-event:</b> The first time the Kaplan-Meier curve reaches 50% or lower, when that time is estimable</li>
        <li><b>Time-specific estimates:</b> Survival or cumulative-incidence estimates at the selected time points</li>
        <li><b>Curves:</b> Kaplan-Meier survival, or cumulative incidence when competing risks are specified</li>
    </ul>

    <b>Input Requirements:</b>
    <ul>
        <li><b>Time Variable:</b> Either:
            <ul>
                <li>Pre-calculated follow-up time (numeric, continuous)</li>
                <li>Start and end dates (will be converted to time intervals)</li>
            </ul>
        </li>
        <li><b>Outcome Variable:</b> Event indicator showing whether each subject experienced the event
            <ul>
                <li>For binary variables: Select the level representing the event</li>
                <li>For multiple outcomes: Use advanced options to specify event types</li>
            </ul>
        </li>
    </ul>

    <b>Analysis Options:</b>
    <ul>
        <li>Conditional analysis among subjects followed beyond a selected landmark; this does not by itself remove immortal-time or selection bias</li>
        <li>Various plot types: survival curves, cumulative hazard, cumulative events</li>
        <li>Customizable time units and axis scales</li>
        <li>Risk tables and confidence intervals</li>
    </ul>

    <b>Methodology:</b>
    Utilizes the Kaplan-Meier method to estimate survival probabilities, handling right-censored data appropriately.
    <br><br>
    This analysis is implemented using the survival, survminer, and finalfit R packages. Please cite both jamovi and these packages in publications.
    <br><hr>
    For detailed information about survival analysis methods, see the
    <a href='https://cran.r-project.org/web/packages/survival/vignettes/survival.pdf'>survival package documentation</a>.
    "
        )

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
          ## Precalculated Time ----

          # No conversion is performed and none is possible: the elapsed-time
          # column carries no unit. "Time Type in Output" is therefore a
          # DECLARATION of the unit the column is already in, not a conversion
          # -- it tells the analysis how to scale cutpoints and follow-up
          # thresholds and how to label axes. The option used to be presented
          # as if it converted (and was disabled in the UI in this branch), so
          # a column of days analysed with the label "months" produced 1/3/5
          # "year" survival read off days 12, 36 and 60.
          private$.addInfo(sprintf(
            'Pre-calculated elapsed time is used exactly as supplied; no unit conversion is performed. The analysis assumes "%s" is the unit "%s" is already recorded in, and scales cutpoints, thresholds and axis labels accordingly. If that is not the unit of your data, change "Time Type in Output".',
            self$options$timetypeoutput, self$options$elapsedtime))

          mydata[["mytime"]] <-
            jmvcore::toNumeric(mydata[[mytime_labelled]])


        } else if (tint) {
          ## Time Interval ----

          dxdate <- mydxdate_labelled # self$options$dxdate
          fudate <- myfudate_labelled #self$options$fudate
          timetypedata <- self$options$timetypedata


          # Numeric date columns do NOT all count in seconds.
          #
          # Every numeric column used to be fed to as.POSIXct(origin=
          # "1970-01-01"), which reads its input as Unix epoch SECONDS. A
          # numeric R Date counts DAYS since the same origin, so a one-year
          # interval (18262 -> 18628) was read as 366 seconds and came out as
          # 0.0000139 years / 0.000139 months -- a number small enough to look
          # like a rounding artefact rather than a unit blunder.
          #
          # Magnitude separates the usual modern-study encodings: as days, 1e5
          # is the year 2243; as seconds, 1e5 is 2 Jan 1970. It is still a
          # heuristic, so ambiguous mixtures are rejected rather than silently
          # coerced.
          .isBareNumeric <- function(x) is.numeric(x) && !inherits(x, c("Date", "POSIXt"))
          .classifyBareNumeric <- function(x, label) {
            if (!.isBareNumeric(x)) return(NA_character_)
            vals <- abs(as.numeric(x))
            vals <- vals[is.finite(vals)]
            if (length(vals) == 0) return(NA_character_)
            if (any(vals < 1e5) && any(vals >= 1e5)) {
              private$.addError(sprintf(
                paste0('Numeric date variable "%s" contains values on both sides of the ',
                       'date-encoding boundary (100000). This can indicate mixed R-Date ',
                       'days and Unix seconds, or a sentinel/mistyped value. Recode the ',
                       'entire column to one explicit date representation before analysis.'),
                label))
              return("mixed")
            }
            if (stats::median(vals) < 1e5) "days" else "seconds"
          }

          dx_scale <- .classifyBareNumeric(mydata[[dxdate]], self$options$dxdate)
          fu_scale <- .classifyBareNumeric(mydata[[fudate]], self$options$fudate)
          if (identical(dx_scale, "mixed") || identical(fu_scale, "mixed"))
            return(NULL)

          bare_scales <- stats::na.omit(c(dx_scale, fu_scale))
          if (length(unique(bare_scales)) > 1) {
            private$.addError(sprintf(
              paste0('Diagnosis date "%s" and follow-up date "%s" appear to use ',
                     'different numeric encodings (%s versus %s). Convert both columns ',
                     'to R Date values or to the same numeric epoch scale before analysis.'),
              self$options$dxdate, self$options$fudate, dx_scale, fu_scale))
            return(NULL)
          }

          # Default to days only when no bare numeric column supplies a scale
          # (e.g., both columns are already Date/POSIXct).
          numeric_scale <- if (length(bare_scales) == 0) "days" else bare_scales[[1]]
          date_scale <- if (identical(numeric_scale, "seconds")) 1 else 86400

          .toDateTime <- function(x) {
              if (inherits(x, "POSIXt")) return(x)
              if (inherits(x, "Date"))
                  return(as.POSIXct(as.numeric(x) * 86400, origin = "1970-01-01", tz = "UTC"))
              as.POSIXct(as.numeric(x) * date_scale, origin = "1970-01-01", tz = "UTC")
          }
          .isDateLike <- function(x) is.numeric(x) || inherits(x, c("Date", "POSIXt"))

          is_numeric_dx <- .isDateLike(mydata[[dxdate]])
          is_numeric_fu <- .isDateLike(mydata[[fudate]])

          if (is_numeric_dx && is_numeric_fu) {
              mydata[["start"]] <- .toDateTime(mydata[[dxdate]])
              mydata[["end"]]   <- .toDateTime(mydata[[fudate]])
              if (length(bare_scales) > 0)
                  private$.addInfo(sprintf('Bare numeric date columns were interpreted as %s since 1970-01-01. Each numeric column was classified separately and inconsistent or mixed encodings were rejected.',
                                           if (date_scale == 86400) 'DAYS (the R Date encoding)' else 'SECONDS (the Unix epoch encoding)'))
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
                  mydata[["start"]] <- func(mydata[[dxdate]])
                  mydata[["end"]] <- func(mydata[[fudate]])
              } else {
                  private$.addError(sprintf('Unsupported date format: %s. Supported formats are: %s. Please select the correct format from Time Type options.', timetypedata, paste(names(lubridate_functions), collapse = ', ')))
                  return(NULL)
              }
          } else {
              # Mixed types error
              private$.addError('Diagnosis date and follow-up date must be in the same format (both numeric or both text). Please ensure date columns are consistently formatted.')
              return(NULL)
          }



          if ( sum(!is.na(mydata[["start"]])) == 0 || sum(!is.na(mydata[["end"]])) == 0)  {
            start_valid <- sum(!is.na(mydata[["start"]]))
            end_valid <- sum(!is.na(mydata[["end"]]))
            private$.addError(sprintf('Date parsing failed. Start date valid: %d, End date valid: %d. Please verify date format matches selected type (%s) and check for missing/invalid date values.', start_valid, end_valid, self$options$timetypedata))
            return(NULL)
          }


          timetypeoutput <-
            jmvcore::constructFormula(terms = self$options$timetypeoutput)


          mydata <- mydata %>%
            dplyr::mutate(interval = lubridate::interval(start, end))



          mydata <- mydata %>%
            dplyr::mutate(mytime = lubridate::time_length(interval, timetypeoutput))


        }


        df_time <- mydata %>% jmvcore::select(c("row_names", "mytime"))

        # Check for missing values in time and warn user
        if (any(is.na(df_time$mytime))) {
          n_missing <- sum(is.na(df_time$mytime))
          if (self$options$tint) {
            private$.addWarning(sprintf('Calculated time from dates contains %d missing value%s. These observations will be excluded from the analysis. Please verify date format matches selected type and check for missing or invalid dates.',
                                       n_missing, ifelse(n_missing == 1, '', 's')))
          } else {
            private$.addWarning(sprintf('Time variable contains %d missing value%s. These observations will be excluded from the analysis.',
                                       n_missing, ifelse(n_missing == 1, '', 's')))
          }
        }
        # Infinite follow-up is not follow-up.
        #
        # Only `<= 0` was checked, and Inf passes that. An Inf then poisoned
        # every downstream summary at once and none of them complained: total
        # person-time Inf, incidence rate events/Inf = 0 with a 0-0 confidence
        # interval, restricted mean NaN, and the data-quality grader read
        # max(time) = Inf as "Long-term follow-up". A rate of exactly zero in a
        # cohort with events is not a borderline estimate, it is arithmetic on a
        # value that should never have entered. NA is deliberately excluded from
        # this test: a missing time is a normal, separately reported exclusion.
        n_nonfinite <- sum(!is.na(df_time$mytime) & !is.finite(df_time$mytime))
        if (n_nonfinite > 0) {
          private$.addError(sprintf('%d time value%s infinite (Inf). Follow-up time must be a finite number; an infinite time makes total person-time, incidence rates and the restricted mean undefined. Replace the value with the observed follow-up duration, or leave the cell empty to have the subject excluded as missing.',
                                    n_nonfinite, ifelse(n_nonfinite == 1, ' is', 's are')))
          return(NULL)
        }
        if (any(df_time$mytime < 0, na.rm = TRUE)) {
          private$.addError('Time values must be zero or positive. Negative follow-up means the event/follow-up date precedes study entry; verify the date order and the elapsed-time variable.')
          return(NULL)
        }
        n_zero <- sum(df_time$mytime == 0, na.rm = TRUE)
        if (n_zero > 0)
          private$.addWarning(sprintf(
            '%d observation(s) have follow-up time zero. They are retained: events at time zero change the Kaplan-Meier or cumulative-incidence estimate at the origin, while zero-time censored observations contribute no person-time. Confirm that same-day events and the time origin are coded as intended.',
            n_zero))

        # Sanity-check the DECLARED unit against the magnitude of the column.
        #
        # In the !tint branch nothing compares the two, so a column of days
        # declared as "years" was believed without comment: the table was headed
        # "1, 3, 5 year Survival" while its rows were read off days 1, 3 and 5,
        # and every one reported 100% survival. The implausibility was already
        # being computed and printed ("Median follow-up: 1393.0 years") -- it was
        # simply never flagged. Median, not max, so a single 9999 sentinel cannot
        # trigger it. Warn only: the user is never overridden.
        #
        # Known ceiling: adjacent-unit swaps are undetectable in principle
        # (60 months declared as 60 years is indistinguishable from a real birth
        # cohort). No lower bound either -- genuine ICU cohorts have follow-up of
        # days.
        #
        # The same check now covers the DATES path. There the unit is derived
        # rather than declared, so an implausible result means the dates
        # themselves were read wrongly -- an epoch-scale misread, a sentinel
        # like 999999, or a swapped century. That path used to have no check at
        # all, which is how ~48-year survival times were printed silently.
        yr <- private$.yearInUnits()
        med <- stats::median(df_time$mytime, na.rm = TRUE)
        med_years <- med / yr
        if (!tint) {
          if (is.finite(med_years) && med_years > 100)
            private$.addWarning(sprintf(
              'Median follow-up in "%s" is %s, which under the declared unit "%s" is %.0f years - longer than a human lifetime. The unit is probably mis-declared (a column of days or weeks read as %s). Check "Time Type in Output"; nothing has been changed automatically.',
              self$options$elapsedtime, format(round(med, 1)),
              self$options$timetypeoutput, med_years, self$options$timetypeoutput))
        } else {
          max_years <- suppressWarnings(max(df_time$mytime, na.rm = TRUE)) / yr
          n_impl <- sum(df_time$mytime / yr > 150, na.rm = TRUE)
          if (n_impl > 0) {
            # Do not include biologically implausible intervals in clinical
            # estimates. A warning that said "still included" allowed a single
            # sentinel to dominate person-time and restricted means.
            private$.addError(sprintf(
              paste0('Calculated follow-up from "%s" and "%s" is implausible: ',
                     '%d observation(s) exceed 150 years (longest %.0f years; ',
                     'median %.0f years). This usually indicates a sentinel, ',
                     'mistyped date, wrong century, or incorrect encoding. ',
                     'Correct the source dates before analysis; these rows have ',
                     'not been analysed.'),
              self$options$dxdate, self$options$fudate,
              n_impl, max_years, med_years))
            return(NULL)
          } else if (is.finite(med_years) && med_years > 100) {
            private$.addError(sprintf(
              paste0('Median calculated follow-up from "%s" and "%s" is %.0f ',
                     'years, which is implausible for a clinical follow-up ',
                     'interval. Check the date encoding, century, and source ',
                     'values before analysis.'),
              self$options$dxdate, self$options$fudate, med_years))
            return(NULL)
          }
        }

        return(df_time)


      }

      # Define Outcome ----
      ,
      .definemyoutcome = function() {


        labelled_data <- private$.getData()

        mydata <- labelled_data$mydata_labelled
        myoutcome_labelled <- labelled_data$myoutcome_labelled


        # Delegated to the shared coder in survival_utils.R so that all five
        # analyses that build an event indicator agree on validation and on what
        # happens to unselected levels and to NA.
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
          private$.addError(res$error)
          return(NULL)
        }

        private$.eventRecode <- res
        mydata[["myoutcome"]] <- res$status

        if (res$n_missing > 0) {
          private$.addWarning(sprintf('Outcome variable contains %d missing value%s. These observations will be excluded from the analysis.',
                                      res$n_missing, ifelse(res$n_missing == 1, '', 's')))
        }

        # (The old 0/1/2 range check and second missing-value warning lived here.
        # .defineEventIndicator() now guarantees the range, and warning twice in
        # one run is exactly how notices end up duplicated across run cycles.)

        df_outcome <- mydata %>% jmvcore::select(c("row_names", "myoutcome"))

        return(df_outcome)

      }


      # Define Factor ----
      ,
      .definemyfactor = function() {


        labelled_data <- private$.getData()

        mydata_labelled <- labelled_data$mydata_labelled

        mydata <- mydata_labelled

        mydata[["myfactor"]] <- "1"


        df_factor <- mydata %>% jmvcore::select(c("row_names","myfactor"))

        return(df_factor)

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

        time <- private$.definemytime()
        outcome <- private$.definemyoutcome()
        factor <- private$.definemyfactor()

        # Check if any returned NULL (validation failed)
        if (is.null(time) || is.null(outcome) || is.null(factor)) {
          private$.displayMessages()
          return(NULL)
        }

        # Keep the UNFILTERED frames for the "add to data" exports. Everything
        # below this line subsets and shifts the analysis data set; the exported
        # columns describe the source data, not the analysis subset. See the
        # export block in .run().
        private$.rawTime <- time
        private$.rawOutcome <- outcome

        cleanData <- dplyr::left_join(time, outcome, by = "row_names") %>%
          dplyr::left_join(factor, by = "row_names")

        # Remove rows with missing time or outcome (complete case analysis)
        n_before <- nrow(cleanData)
        cleanData <- cleanData %>%
          dplyr::filter(!is.na(mytime) & !is.na(myoutcome))
        n_after <- nrow(cleanData)

        # Report if rows were removed
        if (n_before > n_after) {
          n_removed <- n_before - n_after
          private$.addInfo(sprintf('Excluded %d observation%s with missing time or outcome values. Analysis based on %d complete cases.',
                                  n_removed, ifelse(n_removed == 1, '', 's'), n_after))
        }

        # Check if any data remains after removing missing values
        if (n_after == 0) {
          private$.addError('No complete cases available for analysis. All observations have missing time or outcome values.')
          private$.displayMessages()
          return(NULL)
        }

        # Landmark ----
        # https://www.emilyzabor.com/tutorials/survival_analysis_in_r_tutorial.html#landmark_method
        if (self$options$uselandmark) {

          landmark <- jmvcore::toNumeric(self$options$landmark)

          # A negative landmark used to run: the filter kept everyone and the
          # mutate ADDED |landmark| to every time, inflating the whole cohort's
          # follow-up with no notice at all. There is no landmark before the
          # start of follow-up. (The .a.yaml now sets min: 0 for the GUI; this
          # guard covers programmatic calls, which bypass it.)
          if (is.na(landmark) || landmark < 0) {
            private$.addError(sprintf('Landmark time must be zero or positive (received %s). A landmark is a time point during follow-up; a negative value would shift every subject\'s follow-up forward instead of conditioning on surviving to it.',
                                      format(self$options$landmark)))
            private$.displayMessages()
            return(NULL)
          }

          n_before <- nrow(cleanData)
          max_before <- max(cleanData$mytime, na.rm = TRUE)
          # Strictly greater than the landmark, not >=. A subject whose
          # follow-up ends exactly AT the landmark contributes zero residual
          # time, which contradicts the strictly-positive-time rule enforced in
          # .definemytime() and adds a row that carries no information.
          cleanData <- cleanData %>%
            dplyr::filter(mytime > landmark) %>%
            dplyr::mutate(mytime = mytime - landmark)
          n_after <- nrow(cleanData)

          # Nothing checked nrow() here. With a landmark past the longest
          # follow-up the frame was emptied and the analysis continued into the
          # data-quality step, where `if (n_events / n_total < 0.1)` evaluated
          # 0/0 and aborted the run with "missing value where TRUE/FALSE
          # needed" -- an R internals message, for a plain configuration error.
          if (n_after == 0) {
            private$.addError(sprintf('No subjects remain after the landmark at %s %s: every subject\'s follow-up ended at or before that time (longest follow-up was %s %s). Choose a landmark inside the observed follow-up range.',
                                      landmark, self$options$timetypeoutput,
                                      format(round(max_before, 1)),
                                      self$options$timetypeoutput))
            private$.displayMessages()
            return(NULL)
          }

          if (n_after < n_before) {
            private$.addInfo(sprintf('Landmark analysis removed %d subject(s) whose follow-up ended at or before %s %s; %d remain. Time is measured from the landmark, and estimates are conditional on surviving to it.',
                                     n_before - n_after, landmark, self$options$timetypeoutput, n_after))
          }
        }

        # Time Dependent Covariate ----
        # https://www.emilyzabor.com/tutorials/survival_analysis_in_r_tutorial.html#time-dependent_covariate




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


        name3explanatory <- "SingleArm"

        cleanData <- cleanData %>%
          dplyr::rename(
            !!name1time := mytime,
            !!name2outcome := myoutcome,
            !!name3explanatory := myfactor
          )

        # naOmit ----

        cleanData <- jmvcore::naOmit(cleanData)


        # Prepare Data For Plots ----

        # has_competing travels WITH the plot state: renderers may run after
        # .load() restored results from disk without .run(), and private$
        # .eventRecode is empty in that instance. Without it the plots fall back
        # to the options and render inverted competing-risk curves.
        #
        # The event labels travel with it for the same reason: the CIF legend
        # would otherwise read the raw cmprsk codes 1 and 2, which no clinician
        # can map back onto their own outcome levels.
        plotData <- list(
          "name1time" = name1time,
          "name2outcome" = name2outcome,
          "name3explanatory" = name3explanatory,
          "cleanData" = cleanData,
          "has_competing" = private$.isCompetingRisk(),
          "estimand" = private$.eventRecode$estimand,
          "event_label" = private$.eventRecode$event_label,
          "competing_labels" = private$.eventRecode$competing_labels
        )

        image <- self$results$plot
        image$setState(plotData)

        imageCIF <- self$results$plot_cif
        imageCIF$setState(plotData)

        image2 <- self$results$plot2
        image2$setState(plotData)

        image3 <- self$results$plot3
        image3$setState(plotData)

        image6 <- self$results$plot6
        image6$setState(plotData)

        # Set state for baseline hazard plots
        baselineHazardImage <- self$results$baselineHazardPlot
        baselineHazardImage$setState(plotData)

        smoothedHazardImage <- self$results$smoothedHazardPlot
        smoothedHazardImage$setState(plotData)

        # Return Data ----

        return(
          list(
            "name1time" = name1time,
            "name2outcome" = name2outcome,
            "name3explanatory" = name3explanatory,
            "cleanData" = cleanData,
            "mytime_labelled" = mytime_labelled,
            "myoutcome_labelled" = myoutcome_labelled,
            "mydxdate_labelled" = mydxdate_labelled,
            "myfudate_labelled" = myfudate_labelled
          )
        )

      }


      # Run Analysis ----
      ,
      .run = function() {
        # Clear any previous messages
        private$.clearMessages()
        private$.resetRunOutputs()

        # Retain cache benefits inside this run (.getData() and survfit are
        # requested repeatedly), but never retain objects belonging to an old
        # data/options state. This also bounds memory use for long-lived jamovi
        # sessions.
        old_cache_keys <- ls(envir = private$.cache, all.names = TRUE)
        if (length(old_cache_keys) > 0)
          rm(list = old_cache_keys, envir = private$.cache)

        # These fields belong to the current run. Reset them before validation
        # so a newly incomplete configuration cannot inherit a competing-risk
        # flag or exported values from the preceding run of the same analysis.
        private$.eventRecode <- NULL
        private$.rawTime <- NULL
        private$.rawOutcome <- NULL

        # Input Validation ----
        validation_result <- private$.validateInputs()

        if (!validation_result$continue_analysis) {
          # Configuration incomplete - show todo guidance without error message
          # The todo will guide users on what variables to select
          private$.todo()
          self$results$todo$setVisible(TRUE)
          return()
        } else {
          self$results$todo$setVisible(FALSE)
        }

        # Active analysis roles must be distinct. Without this guard the same
        # column could be used as both time and outcome (or as both dates),
        # yielding mechanically valid but clinically meaningless arithmetic.
        role_errors <- character()
        if (isTRUE(self$options$tint)) {
          if (identical(self$options$dxdate, self$options$fudate))
            role_errors <- c(role_errors,
              "Diagnosis/start date and follow-up/end date must be different variables.")
          if (self$options$outcome %in% c(self$options$dxdate, self$options$fudate))
            role_errors <- c(role_errors,
              "The outcome variable must be different from both date variables.")
        } else if (identical(self$options$elapsedtime, self$options$outcome)) {
          role_errors <- c(role_errors,
            "Elapsed time and outcome must be different variables.")
        }
        if (length(role_errors) > 0) {
          for (msg in unique(role_errors)) private$.addError(msg)
          private$.displayMessages()
          return()
        }

        # jamovi column names are normally unique, but the exported R function
        # accepts ordinary data frames, which can contain duplicates when
        # check.names = FALSE. A selection such as "time" is then ambiguous and
        # .getData() can match multiple cleaned columns. Refuse the ambiguity
        # instead of analysing whichever duplicate happens to be encountered.
        active_roles <- c(
          self$options$outcome,
          if (isTRUE(self$options$tint))
            c(self$options$dxdate, self$options$fudate) else
            self$options$elapsedtime
        )
        active_roles <- unique(active_roles[!is.na(active_roles) & nzchar(active_roles)])
        duplicated_roles <- active_roles[vapply(
          active_roles,
          function(nm) sum(names(self$data) == nm) > 1L,
          logical(1)
        )]
        if (length(duplicated_roles) > 0) {
          private$.addError(sprintf(
            paste0(
              "Selected variable name(s) are duplicated in the data: %s. ",
              "Rename the duplicate columns so each analysis role identifies exactly one variable."),
            paste(sprintf('"%s"', duplicated_roles), collapse = ", ")))
          private$.displayMessages()
          return()
        }

        ## Empty data ----

        if (nrow(self$data) == 0) {
          private$.addError('Dataset contains no complete rows. Please ensure your data is properly loaded and contains observations.')
          private$.displayMessages()
          return()
        }

        private$.checkpoint()

        ## Get Clean Data ----
        results <- private$.cleandata()

        # Always disclose how the outcome was recoded. A silent recode is a
        # clinical-safety hazard: the reader of a survival curve cannot otherwise
        # see which levels were collapsed into "censored", nor which estimand
        # the probability-scale outputs actually correspond to.
        if (!is.null(private$.eventRecode))
            self$results$eventRecodeInfo$setContent(
                .describeEventIndicator(private$.eventRecode, self$options$outcome))

        # Check if cleandata failed (returned NULL due to validation errors)
        if (is.null(results)) {
          return()
        }

        # Show exactly one of the two "sc" plots.
        #
        # jamovi/singlearm.r.yaml already splits them on the options, but a
        # visible: expression can only see options, and competing risks can
        # also arrive in the DATA: an outcomeorganizer hand-off delivers a
        # 0/1/2 outcome with multievent = FALSE. Re-asserting from the recode
        # here means those users still get the CIF instead of a refusal panel
        # under a "Survival Plot" heading.
        cr <- private$.isCompetingRisk()
        self$results$plot$setVisible(isTRUE(self$options$sc) && !cr)
        self$results$plot_cif$setVisible(isTRUE(self$options$sc) && cr)

        # The piecewise-hazard section is not computed under competing risks
        # (the renderers refuse and .baselineHazardAnalysis() returns early),
        # but .init() had already made the whole section visible from
        # `baseline_hazard` alone. The result was a heading, a table showing
        # column headers over NO ROWS, and an empty explanations panel -- bare
        # scaffolding announcing an analysis that was deliberately not run.
        # .init() cannot decide this: private$.eventRecode does not exist yet,
        # so the outcomeorganizer hand-off (0/1/2 outcome, multievent = FALSE)
        # looks like ordinary survival there. Same blind spot as the CIF plot
        # above, which is why both are re-asserted here.
        if (cr) {
          for (nm in c("baselineHazardHeading", "baselineHazardTable",
                       "baselineHazardPlot", "smoothedHazardPlot",
                       "baselineHazardSummary", "baselineHazardHeading3",
                       "baselineHazardExplanation")) {
            it <- try(self$results[[nm]], silent = TRUE)
            if (!inherits(it, "try-error") && !is.null(it))
              try(it$setVisible(FALSE), silent = TRUE)
          }
        }

        ## Data Quality Assessment ----
        private$.checkpoint()
        data_quality <- private$.assessDataQuality(results)

        # Store data quality for potential use in outputs
        results$data_quality <- data_quality

        # WARNING for data quality issues from assessment
        if (length(data_quality$warnings) > 0) {
          for (i in seq_along(data_quality$warnings)) {
            private$.addWarning(data_quality$warnings[i])
          }
        }

        ## Run Analysis ----

        ### Median Survival ----
        private$.checkpoint()

        private$.medianSurv(results)


        ### Survival Table ----
        private$.checkpoint()

        private$.survTable(results)


        ### Person-Time Analysis ----
        private$.checkpoint()
        private$.personTimeAnalysis(results)

        ### Plot Explanations ----
        # Deliberately NOT inside .personTimeAnalysis(), where it used to live:
        # that function returns early unless person-time metrics are requested.
        private$.plotExplanations()

        ### Plot Parameter Validation ----
        # Also validated here, not only inside each renderer.
        #
        # Renderers run AFTER .run() has finished, so the error text they add is
        # appended to a message list that has already been rendered: an invalid
        # plot end time, tick interval or y-axis range produced a blank panel and
        # complete silence. Running the same check inside .run() puts the message
        # where the user can see it, while the renderers keep their own call so
        # they still decline to draw.
        if (self$options$sc || self$options$ce || self$options$ch || self$options$kmunicate)
          private$.validatePlotParameters(check_y = self$options$sc || self$options$ce)
        if (private$.isCompetingRisk() && self$options$sc) {
          unavailable <- character()
          if (isTRUE(self$options$risktable))
            unavailable <- c(unavailable, "the numbers-at-risk panel")
          if (isTRUE(self$options$censored))
            unavailable <- c(unavailable, "censoring marks")
          if (!identical(self$options$medianline, "none"))
            unavailable <- c(unavailable, "median reference lines")
          if (length(unavailable) > 0L)
            private$.addInfo(paste0(
              "The competing-risk CIF plot does not display ",
              paste(unavailable, collapse = ", "),
              ". The cumulative-incidence table remains available for estimates, confidence intervals, and counts at selected times."))
        }
        if (!private$.isCompetingRisk() && isTRUE(self$options$kmunicate) &&
            (isTRUE(self$options$censored) ||
             !identical(self$options$medianline, "none"))) {
          private$.addInfo(paste0(
            "The KMunicate-style plot follows the CI and risk-table options, but ",
            "does not draw individual censoring marks or median reference lines. ",
            "Those two display options apply to the standard Kaplan-Meier and ",
            "cumulative-event plots; the KMunicate risk panel reports censoring ",
            "counts when it is shown."))
        }

        ### Baseline Hazard Analysis ----
        private$.checkpoint()
        if (self$options$baseline_hazard || self$options$hazard_smoothing) {
          private$.baselineHazardAnalysis(results)
        }

        ### Advanced Diagnostics ----
        private$.checkpoint()
        if (self$options$advancedDiagnostics) {
          private$.populateDataQuality(results)
        }

        ### Clinical Summary ----
        private$.checkpoint()
        if (self$options$showSummaries) {
          private$.generateClinicalSummary(results)
        }


        ## Add Calculated Time to Data ----

        # Export the RAW interval between the two dates, for every row of the
        # dataset -- which is what the column's own title, description and
        # varDescription ("Calculated Time from given Dates") promise, and the
        # only thing it is useful for: checking the date arithmetic.
        #
        # It used to be written from results$cleanData, i.e. AFTER the landmark
        # subtraction and AFTER complete-case exclusion. With a landmark of 3 the
        # exported column was every subject's follow-up minus 3, under a name and
        # description that said it was the interval between the dates; and rows
        # excluded from the analysis simply had no value, so the column silently
        # depended on options that have nothing to do with date arithmetic. An
        # exported column that differs from what its name promises is exactly the
        # defect class that produced a blocker in outcomeorganizer.
        #
        # Known consequence, deliberate: the column now carries a value for rows
        # the ANALYSIS excluded. It is a data-checking column, not a membership
        # indicator; the analysis subset is reported in the notices instead.
        if (self$options$tint && self$options$calculatedtime &&
            !is.null(private$.rawTime) && self$results$calculatedtime$isNotFilled()) {
          self$results$calculatedtime$setRowNums(private$.rawTime$row_names)
          self$results$calculatedtime$setValues(private$.rawTime$mytime)
        }


        ## Add Redefined Outcome to Data ----

        # Same reasoning: the recoded 0/1/2 indicator for every row, as produced
        # by .defineEventIndicator(), not just the complete and landmark-eligible
        # ones. NA stays NA, which is the honest value for a row whose original
        # outcome was missing or was mapped to no category.
        if (self$options$multievent && self$options$outcomeredefined &&
            !is.null(private$.rawOutcome) && self$results$outcomeredefined$isNotFilled()) {
          self$results$outcomeredefined$setRowNums(private$.rawOutcome$row_names)
          self$results$outcomeredefined$setValues(private$.rawOutcome$myoutcome)
        }

        ## Analysis Completion Notice ----
        analysis_type <- if(private$.isCompetingRisk()) 'Competing risk' else 'Standard'
        method_used <- if(private$.isCompetingRisk())
          'cumulative-incidence (Aalen-Johansen/cmprsk)' else 'Kaplan-Meier'
        # The counts must add up to the total. Competing events are neither
        # events of interest nor censored observations (.assessDataQuality
        # subtracts them out of both), so listing only events and censored left
        # a silent shortfall -- 200 observations reported as "31 events, 0
        # censored" with the other 169 unaccounted for.
        n_competing_total <- data_quality$n_competing
        counts_text <- if (n_competing_total > 0)
          sprintf('%d observations (%d event(s) of interest, %d competing event(s), %d censored)',
                  data_quality$n_total, data_quality$n_events,
                  n_competing_total, data_quality$n_censored)
        else
          sprintf('%d observations (%d events, %d censored)',
                  data_quality$n_total, data_quality$n_events, data_quality$n_censored)
        followup_label <- if (isTRUE(data_quality$median_followup_reverse_km))
          'Median follow-up (reverse Kaplan-Meier)' else
          'Median observed time (reverse Kaplan-Meier follow-up not estimable)'
        private$.addInfo(sprintf('Analysis completed: %s. %s: %.1f %s. %s analysis using %s method.',
                                 counts_text,
                                 followup_label,
                                 data_quality$median_followup,
                                 self$options$timetypeoutput,
                                 analysis_type,
                                 method_used))

        # Display all accumulated messages
        private$.displayMessages()
      }

      # Competing Risk Analysis Function ----
      ,
      .competingRiskCumInc = function(results) {
        # Proper competing risk analysis using cmprsk::cuminc()
        # Returns cumulative incidence estimates for the event of interest

        mytime <- results$name1time
        myoutcome <- results$name2outcome
        mydata <- results$cleanData

        # Ensure time is numeric
        mydata[[mytime]] <- jmvcore::toNumeric(mydata[[mytime]])

        # For competing risk: outcome is 0=censored, 1=event of interest, 2=competing event
        # cmprsk::cuminc requires this format

        tryCatch({
          # Run cumulative incidence analysis
          cuminc_fit <- cmprsk::cuminc(
            ftime = mydata[[mytime]],
            fstatus = mydata[[myoutcome]],
            cencode = 0  # 0 is censored
          )

          return(cuminc_fit)

        }, error = function(e) {
          private$.addError(sprintf('Competing risk analysis failed: %s. Please verify outcome is coded as 0 (censored), 1 (event of interest), 2 (competing event).', e$message))
          return(NULL)
        })
      }

      # Median Survival Function ----
      ,
      .medianSurv = function(results) {
        mytime <- results$name1time
        myoutcome <- results$name2outcome
        myfactor <- results$name3explanatory

        mydata <- results$cleanData
        estimand_meta <- private$.estimandMeta()

        # Reveal the section heading now that the median-survival analysis runs
        self$results$medianHeading$setVisible(TRUE)

        mydata[[mytime]] <-
          jmvcore::toNumeric(mydata[[mytime]])

        ## Median Survival Table ----

        private$.checkpoint()

        # Check if competing risk analysis
        if (private$.isCompetingRisk()) {
          # PROPER COMPETING RISK ANALYSIS using cmprsk

          # Decide from the DATA what is a failure and what is a zero.
          #
          # cmprsk omits the "1 1" element entirely when no event of interest
          # occurred, and errors outright ("NAs in foreign function call") when
          # NOTHING occurred at all. Both used to end here with an error notice
          # and an early return, which left this section empty AND the median
          # table unpopulated -- and .generateClinicalSummary() returns as soon
          # as medianTable$rowCount is 0, so the copy-ready clinical summary came
          # out blank as well, while the cumulative-incidence table below
          # correctly reported 0. A cohort with no event of interest is a
          # reportable result, not a failure.
          n_event_of_interest <- sum(mydata[[myoutcome]] == 1, na.rm = TRUE)
          n_competing_events  <- sum(mydata[[myoutcome]] == 2, na.rm = TRUE)

          cif_1 <- NULL
          if (n_event_of_interest + n_competing_events > 0) {
            cuminc_fit <- private$.competingRiskCumInc(results)
            cif_1 <- cuminc_fit$`1 1`  # Event type 1, group 1 (no stratification)
          }

          if (is.null(cif_1) && n_event_of_interest > 0) {
            # Events of interest exist but no curve came back: a genuine failure.
            private$.addError(sprintf('No cumulative incidence found for the event of interest, although %d such event(s) are present. Please verify the event of interest is coded as 1 in your outcome variable for competing risk analysis.', n_event_of_interest))
            private$.displayMessages()
            return()
          }

          # Calculate median time to event (time when CIF reaches 0.5)
          median_time <- NA
          cif_times <- if (is.null(cif_1)) numeric(0) else cif_1$time
          cif_est   <- if (is.null(cif_1)) numeric(0) else cif_1$est

          if (any(is.finite(cif_est)) && max(cif_est, na.rm = TRUE) >= 0.5) {
            # Find first time where CIF >= 0.5
            median_idx <- which(cif_est >= 0.5)[1]
            median_time <- cif_times[median_idx]
          }

          # No confidence interval is reported for the competing-risk median.
          #
          # What used to be here was not a confidence interval. It transformed the
          # POINTWISE variance of the CIF at the median to the cloglog scale,
          # inverted those probability bounds against the POINT ESTIMATE of the
          # CIF to read off two times, and -- when cmprsk returned no variance,
          # or when the resulting bounds fell on the wrong side of the estimate --
          # substituted median * 0.8 and median * 1.2. Those +/-20% numbers have
          # no inferential basis whatsoever and were printed as a 95% CI.
          #
          # A valid interval for a quantile is obtained by inverting a confidence
          # BAND for the CIF (the set of times whose band contains 0.5), or by
          # bootstrapping. Until one of those is implemented, reporting nothing is
          # the honest option: survfit itself returns NA rather than guessing.
          median_lower <- NA
          median_upper <- NA

          # Create results table in same format as KM
          n_total <- nrow(mydata)
          n_events <- n_event_of_interest
          n_censored <- sum(mydata[[myoutcome]] == 0, na.rm = TRUE)
          n_competing <- n_competing_events

          results1table <- data.frame(
            records = n_total,
            events = n_events,
            rmean = NA,  # Restricted mean not applicable for CIF
            se_rmean = NA,
            median = median_time,
            # Deliberately NA -- see the comment above. (The trailing "# Approximate"
            # that used to sit here described the removed median * 0.8 / 1.2 fudge.)
            x0_95lcl = median_lower,
            x0_95ucl = median_upper
          )
          if (is.na(median_time)) {
            private$.addInfo(if (n_event_of_interest == 0)
              sprintf('No event of interest (outcome code 1) was observed, so its cumulative incidence is 0 throughout and no median time to the event of interest exists. %d competing event(s) and %d censored observation(s) were analysed. Check that the intended event level is selected before reporting this as a zero risk.',
                      n_competing_events, sum(mydata[[myoutcome]] == 0, na.rm = TRUE))
              else 'Cumulative incidence did not reach 50%. Median time to event not estimable. This is common in competing risk analyses with frequent competing events.')
          }

        } else {
          # STANDARD SURVIVAL ANALYSIS using Kaplan-Meier - composeTerm backtick-
          # escapes user column names; asFormula validates against jamovi's
          # allow-list (Surv is allow-listed).
          formula <-
            paste0('Surv(',
                   jmvcore::composeTerm(mytime),
                   ', ',
                   jmvcore::composeTerm(myoutcome),
                   ') ~ ',
                   jmvcore::composeTerm(myfactor))

          formula <- jmvcore::asFormula(formula, additional_allowed_functions = c("Surv"))

          km_fit <- private$.safeExecute({
            private$.getCachedSurvfit(formula, mydata, "median")
          }, context = "survival_calculation")

          if (is.null(km_fit)) {
            private$.addError('Unable to perform survival analysis. Please check for: (1) sufficient events, (2) valid time values, (3) properly coded outcome variable.')
            return()
          }

          km_fit_median_df <- summary(km_fit)

          # Process survival fit results for table display
          results1table <-
            as.data.frame(km_fit_median_df$table) %>%
            t() %>%
            as.data.frame() %>%
            janitor::clean_names(dat = ., case = "snake")
        }

        ## Populate Median Table ----
        # results1table is already created above (either from CIF or KM)

        medianTable <- self$results$medianTable
        # Name the estimand in the title. The fixed schema says "Median Survival"
        # for both branches, but a CIF quantile is not median survival; the
        # titles are therefore set on EVERY run (both branches) so that a title
        # from a previous competing-risk run cannot stick to a KM run.
        if (private$.isCompetingRisk()) {
          self$results$medianHeading$setTitle(
            .("Median Time to Event of Interest (Cumulative Incidence)"))
          medianTable$setTitle(
            .("Median Time to Event of Interest (Cumulative Incidence)"))
          medianTable$getColumn("median")$setTitle(.("Median time to event"))
          medianTable$getColumn("rmean")$setTitle(.("Restricted mean survival time"))
          medianTable$getColumn("se_rmean")$setTitle(.("SE of restricted mean"))
          # The summary and explanation panels are titled after the estimand too;
          # a panel headed "Median Survival" over cumulative-incidence prose is
          # the same estimand confusion one level up.
          self$results$medianSummary$setTitle(
            .("Median Time to Event of Interest: Natural Language Summary"))
          self$results$medianHeading3$setTitle(
            .("Median Time to Event of Interest: Explanations"))
          self$results$medianSurvivalExplanation$setTitle(
            .("Understanding the Median Time to the Event of Interest"))
          private$.addInfo('Competing risk analysis: Median time represents cumulative incidence of event of interest, properly accounting for competing events.')
          # Say WHY the CI cells are blank. Under a column headed "95% Confidence
          # Interval" an empty cell is otherwise indistinguishable from the KM
          # "not estimable" case, and a reader may assume the interval was simply
          # too wide to print rather than never computed.
          # Clear the KM-only note: a competing-risk table has no RMST.
          medianTable$setNote("rmst", NULL)
          medianTable$setNote(
            "cr_ci",
            .("No confidence interval is computed for the cumulative-incidence median. Restricted-mean survival columns are not applicable to this cumulative-incidence quantile and are left empty."))
        } else {
          median_title <- paste0(estimand_meta$median, " Analysis")
          self$results$medianHeading$setTitle(median_title)
          medianTable$setTitle(paste0(estimand_meta$median, " Table"))
          medianTable$getColumn("median")$setTitle(estimand_meta$median)
          medianTable$getColumn("rmean")$setTitle(
            if (identical(private$.eventRecode$estimand, "overall survival"))
              .("Restricted mean survival time") else
              .("Restricted mean event-free time"))
          medianTable$getColumn("se_rmean")$setTitle(.("SE of restricted mean"))
          # Clear the competing-risk note: this IS a KM table and it does
          # show a confidence interval, so cr_ci would contradict the column
          # beside it.
          medianTable$setNote("cr_ci", NULL)
          medianTable$setNote(
            "rmst",
            sprintf(.("Restricted mean survival time is the area under the Kaplan-Meier curve from time 0 to the largest observed time in this analysis (%s %s). It is therefore horizon-dependent and should only be compared when the same restriction time is used."),
                    format(round(max(mydata[[mytime]], na.rm = TRUE), 2), trim = TRUE),
                    self$options$timetypeoutput))
          self$results$medianSummary$setTitle(
            paste0(estimand_meta$median, ": Natural Language Summary"))
          self$results$medianHeading3$setTitle(
            paste0(estimand_meta$median, ": Explanations"))
          self$results$medianSurvivalExplanation$setTitle(
            paste0("Understanding ", estimand_meta$median))
        }
        data_frame <- results1table

        # Populate only columns declared in singlearm.r.yaml. summary.survfit()
        # also returns n.max and n.start; passing those undeclared fields relied
        # on result-engine tolerance and made schema drift invisible.
        median_columns <- c(
          "records", "events", "rmean", "se_rmean", "median",
          "x0_95lcl", "x0_95ucl")
        missing_columns <- setdiff(median_columns, names(data_frame))
        for (nm in missing_columns) data_frame[[nm]] <- NA_real_
        data_frame <- data_frame[, median_columns, drop = FALSE]

        for (i in seq_along(data_frame[, 1, drop = T])) {
          medianTable$addRow(rowKey = i, values = c(data_frame[i,]))
        }


        ## Median Survival Summary ----

        # FIX: Use correct time unit in narrative
        time_unit <- self$options$timetypeoutput

        # A median that does not exist must not be printed as one.
        #
        # Both narratives below used to interpolate the value straight into the
        # sentence, so a cohort with no reached median read "Median survival is
        # NA [NA - NA, 95% CI] months." Zero-event cohorts are now accepted
        # rather than rejected (see .defineEventIndicator), which makes the
        # not-reached case ordinary rather than exotic. One bound can also be
        # missing on its own -- an upper limit beyond follow-up is the usual
        # case -- so the bounds are formatted individually instead of the whole
        # interval being dropped or printed as "NA".
        .med   <- suppressWarnings(as.numeric(results1table$median))
        .lcl   <- suppressWarnings(as.numeric(results1table$x0_95lcl))
        .ucl   <- suppressWarnings(as.numeric(results1table$x0_95ucl))
        .bound <- function(x) ifelse(is.finite(x), format(round(x, 1), trim = TRUE),
                                     "not reached")
        .ci_txt <- ifelse(is.finite(.lcl) | is.finite(.ucl),
                          paste0(" [95% CI: ", .bound(.lcl), " - ", .bound(.ucl), "]"), "")

        # Different narrative for competing risk vs standard survival
        if (private$.isCompetingRisk()) {
          # Competing risk: report median time to event (cumulative incidence)
          km_fit_median_definition <- ifelse(
            !is.finite(.med) & results1table$events == 0,
            paste0("There is no median time to the event of interest: no event of interest was ",
                   "observed, so its cumulative incidence is 0 throughout the observed follow-up. ",
                   "Only competing events and censoring occurred."),
            ifelse(
              !is.finite(.med),
              paste0("Median time to event of interest not reached (the cumulative incidence of ",
                     "the event of interest did not exceed 50% within the observed follow-up). ",
                     "This is common in competing risk analyses where the competing event is frequent."),
              paste0("Median time to event of interest is ", round(.med, 2), " ", time_unit,
                     " (based on the cumulative incidence function, accounting for competing risks). ",
                     "This is the first time the estimated cumulative incidence of the event ",
                     "of interest reaches 50% or greater; the step curve need not equal 50% ",
                     "exactly. No confidence interval is computed for this quantile.")))

        } else {
          # Standard survival analysis narrative
          km_fit_median_definition <- ifelse(
            !is.finite(.med) & results1table$events == 0,
            paste0(estimand_meta$median, " cannot be estimated: no events were observed, so the ",
                   "Kaplan-Meier event-free probability is 100% throughout the observed ",
                   "follow-up and the curve never falls to 50%."),
            ifelse(
              !is.finite(.med),
              paste0(estimand_meta$median, " was not reached: the Kaplan-Meier curve did not ",
                     "fall to 50% within the observed follow-up."),
              paste0(estimand_meta$median, " is ", round(.med, 1), " ", time_unit, .ci_txt, ".")))
        }  # End of if/else for competing risk vs standard


        # Add additional statistical information
        # Use results1table which is available for both standard and competing risk
        n_events <- results1table$events
        n_total <- results1table$records
        event_rate <- round((n_events / n_total) * 100, 1)
        
        # Include data quality information if available
        quality_info <- ""
        if (!is.null(results$data_quality)) {
          dq <- results$data_quality
          quality_info <- paste0(
            "Descriptive follow-up: range ", dq$min_time, "-", dq$max_time, " ",
            self$options$timetypeoutput, ". ",
            if (length(dq$warnings) > 0) paste("Considerations:", paste(dq$warnings, collapse = "; "), ".") else ""
          )
        }
        
        # Does a median actually exist?
        #
        # The sentences below describe what a median MEANS, and were emitted
        # unconditionally. On a cohort with zero events the summary read
        # "Median survival is NA ... This means that 50% of subjects in this
        # group survived longer than this time period" -- asserting a survival
        # split that cannot be computed, on data where nobody had the event.
        # A copy-ready paragraph stating a fact about patients that the data
        # does not support is the most damaging kind of error this file can
        # make, and it is the same defect that was removed from the sibling
        # clinical summary a few lines away.
        .median_estimable <- tryCatch({
          mv <- suppressWarnings(as.numeric(results1table$median))
          length(mv) > 0 && any(is.finite(mv))
        }, error = function(e) FALSE)

        .median_meaning <- if (.median_estimable)
          c("The Kaplan-Meier median is the first time at which estimated event-free probability is 50% or lower.",
            "Because the curve changes in steps, it need not equal exactly 50%; this is a cohort-level estimate, not a prediction that exactly half of individual patients survive beyond that time.")
        else
          c(paste0("The median was not reached: the Kaplan-Meier curve did not reach 50% ",
                   "before follow-up ended, so no median time-to-event can be estimated from these data.",
                   " This is not a statement that survival is good or poor - it reflects the number of",
                   " events and the length of follow-up."))

        # Different explanations for competing risk vs standard
        if (private$.isCompetingRisk()) {
          medianSummary <- c(km_fit_median_definition,
                             paste0("Observed proportion with the event of interest: ", event_rate, "% (", n_events, " of ", n_total, " subjects)."),
                             quality_info,
                             "This analysis uses proper competing risk methods (cumulative incidence functions).",
                             "The median time is the first time estimated cumulative incidence reaches 50% or greater for the event of interest; the step curve need not equal exactly 50% there,",
                             "accounting for the presence of competing events that prevent the event of interest from occurring."
          )
        } else {
          medianSummary <- c(km_fit_median_definition,
                             paste0("Observed event proportion: ", event_rate, "% (", n_events, " of ", n_total, " subjects). This is a crude proportion, not an incidence rate."),
                             quality_info,
                             .median_meaning,
                             "Note: Confidence intervals use survfit's default log-transformation method (conf.type='log'), based on Greenwood's variance estimate."
          )
        }


        self$results$medianSummary$setContent(medianSummary)

        # Add explanatory output for median survival
        #
        # The long panel below describes ordinary Kaplan-Meier median survival
        # and its 95% CI. Under competing risks the number in the table is a
        # quantile of the cumulative incidence function and has no confidence
        # interval at all, so that panel would be explaining something this run
        # did not compute.
        if (self$options$showExplanations && private$.isCompetingRisk()) {
            self$results$medianSurvivalExplanation$setContent(paste0(
              '<div class="explanation-box" style="background-color: rgba(33, 152, 255, 0.07); padding: 15px; border-radius: 8px; margin: 10px 0; color: inherit;">',
              '<h3 style="color: #2c5282; margin-top: 0;">', .("Understanding the Median Time to the Event of Interest"), '</h3>',
              '<p>', .("This is not median survival. It is the first time at which the estimated cumulative incidence of the event of interest reaches 50% or greater, with competing risks accounted for. Because the curve changes in steps, it need not equal exactly 50% at that time; the estimate indicates that at least half the cohort has had the event of interest by then."), '</p>',
              '<ul>',
              '<li>', .("<b>Not reached</b> is common and expected here: whenever competing events are frequent, the cumulative incidence of the event of interest can plateau below 50%, so no such time exists no matter how long follow-up continues."), '</li>',
              '<li>', .("<b>No confidence interval is reported.</b> A valid interval for this quantile requires inverting a confidence band for the cumulative incidence curve, or bootstrapping; a pointwise variance at the median does not give one. The cells are left empty rather than filled with an approximation."), '</li>',
              '<li>', .("<b>Do not compare it with a Kaplan-Meier median</b> from the same data. A Kaplan-Meier analysis censors the competing event and therefore reports a shorter time to a risk that is over-stated."), '</li>',
              '</ul>',
              '<p><em>', .("Descriptive estimates from a single cohort with no comparison group."), '</em></p>',
              '</div>'))
        } else if (self$options$showExplanations) {
            median_explanation_html <- '
            <div class="explanation-box" style="background-color: rgba(33, 152, 255, 0.07); padding: 15px; border-radius: 8px; margin: 10px 0; color: inherit;">
                <h3 style="color: #2c5282; margin-top: 0;"> Understanding the Kaplan-Meier Median</h3>
                
                <div style="background-color: white; padding: 12px; border-radius: 5px; margin: 10px 0;">
                    <h4 style="color: #2d3748; margin-top: 0;">What is the Kaplan-Meier Median?</h4>
                    <p style="margin: 8px 0;">It is the <strong>first time at which the estimated event-free probability is 50% or lower</strong>. Because the curve changes in steps, the estimate need not equal exactly 50% at that time. Its clinical name depends on the selected endpoint.</p>
                    
                    <div style="background-color: rgba(33, 184, 255, 0.11); padding: 10px; border-radius: 5px; margin: 10px 0; color: inherit;">
                        <strong> Key Concept:</strong> If the median time-to-event = 24 months, it means:
                        <ul style="margin: 5px 0; padding-left: 20px;">
                            <li>The estimated event-free curve has reached 50% or lower by 24 months</li>
                            <li>This is a cohort-level summary, not an individual prediction</li>
                            <li>Read it with its confidence interval and the number at risk</li>
                        </ul>
                    </div>
                </div>
                
                <div style="background-color: rgba(246, 163, 33, 0.11); padding: 12px; border-radius: 5px; margin: 10px 0; color: inherit;">
                    <h4 style="color: #d68910; margin-top: 0;"> Understanding the Results Table</h4>
                    <table style="width: 100%; border-collapse: collapse; margin: 10px 0;">
                        <tr style="background-color: rgba(255, 202, 33, 0.23); color: inherit;">
                            <th style="padding: 8px; text-align: left; border: 1px solid #ffc107;">Measure</th>
                            <th style="padding: 8px; text-align: left; border: 1px solid #ffc107;">Meaning</th>
                        </tr>
                        <tr>
                            <td style="padding: 8px; border: 1px solid #ffc107;"><strong>Records</strong></td>
                            <td style="padding: 8px; border: 1px solid #ffc107;">Total number of patients in analysis</td>
                        </tr>
                        <tr style="background-color: rgba(255, 196, 33, 0.07); color: inherit;">
                            <td style="padding: 8px; border: 1px solid #ffc107;"><strong>Events</strong></td>
                            <td style="padding: 8px; border: 1px solid #ffc107;">Number who experienced the event</td>
                        </tr>
                        <tr>
                            <td style="padding: 8px; border: 1px solid #ffc107;"><strong>Median</strong></td>
                            <td style="padding: 8px; border: 1px solid #ffc107;">First time the estimated event-free probability reaches 50% or lower</td>
                        </tr>
                        <tr style="background-color: rgba(255, 196, 33, 0.07); color: inherit;">
                            <td style="padding: 8px; border: 1px solid #ffc107;"><strong>95% CI</strong></td>
                            <td style="padding: 8px; border: 1px solid #ffc107;">Range of plausible values</td>
                        </tr>
                    </table>
                </div>
                
                <div style="background-color: rgba(33, 159, 43, 0.1); padding: 12px; border-radius: 5px; margin: 10px 0; color: inherit;">
                    <h4 style="color: #2e7d32; margin-top: 0;"> Clinical Interpretation Guide</h4>
                    
                    <div style="background-color: white; padding: 10px; border-radius: 5px; margin: 10px 0;">
                        <strong> When Median is Reached:</strong>
                        <p style="margin: 5px 0;">"The median time-to-event is 36 months (95% CI: 28-45 months)"</p>
                        <ul style="margin: 5px 0; padding-left: 20px;">
                            <li>The estimated event-free curve first reached 50% or lower at 3 years</li>
                            <li>Medians from 28 to 45 months are compatible with these data</li>
                            <li>This describes the observed cohort; individual prognosis depends on factors not in this model</li>
                        </ul>
                    </div>
                    
                    <div style="background-color: rgba(153, 33, 170, 0.12); padding: 10px; border-radius: 5px; margin: 10px 0; color: inherit;">
                        <strong> When Median is "Not Reached" (NR):</strong>
                        <p style="margin: 5px 0;">The estimated event-free curve remains above 50% during observed follow-up</p>
                        <ul style="margin: 5px 0; padding-left: 20px;">
                            <li>Short follow-up or heavy censoring can produce the same result, so read it together with the number at risk.</li>
                            <li>Longer follow-up may make the median estimable, but it can remain undefined if the curve plateaus above 50%</li>
                            <li>Time-specific event-free estimates can still be reported where supported by follow-up</li>
                        </ul>
                    </div>
                </div>
                
                <div style="background-color: rgba(255, 169, 33, 0.14); padding: 10px; border-radius: 5px; margin-top: 10px; border-left: 4px solid #ff9800; color: inherit;">
                    <strong> Practical Tips:</strong>
                    <ul style="margin: 5px 0; padding-left: 20px;">
                        <li><strong>Robust measure:</strong> Less affected by extreme values than mean survival</li>
                        <li><strong>Communication:</strong> State that the estimated event-free curve first reached 50% or lower at X months</li>
                        <li><strong>Contextual comparisons:</strong> Confidence intervals do not remove differences in case mix, endpoint definition, or follow-up</li>
                        <li><strong>Clinical context:</strong> Always interpret alongside patient characteristics and treatment details</li>
                    </ul>
                </div>
            </div>
            '

            
            
            self$results$medianSurvivalExplanation$setContent(median_explanation_html)
        }


      }


      # Survival Table Function ----
      ,
      .survTable = function(results) {
        mytime <- results$name1time
        myoutcome <- results$name2outcome
        myfactor <- results$name3explanatory

        mydata <- results$cleanData
        estimand_meta <- private$.estimandMeta()

        # Reveal the section heading now that the survival-table analysis runs
        self$results$survTableHeading$setVisible(TRUE)

        # Title the table after what it actually contains. The fixed schema said
        # "1, 3, 5 year Survival" whatever the cutpoints and whatever the time
        # unit, and called cumulative incidence "Survival". Set on every run in
        # both branches so a title never carries over from a previous mode.
        # The time points themselves are in the table's first column; naming
        # them in the title as well would go stale, because cutpoints outside
        # the observed follow-up are dropped further down.
        if (private$.isCompetingRisk()) {
          surv_title <- sprintf(.("Cumulative Incidence of the Event of Interest at Selected Time Points (%s)"),
                                self$options$timetypeoutput)
          self$results$survTable$getColumn("surv")$setTitle(.("Cumulative incidence"))
          self$results$survTableSummary$setTitle(
            .("Cumulative Incidence Natural Language Summary"))
          self$results$survTableHeading3$setTitle(.("Cumulative Incidence Explanations"))
          self$results$survivalProbabilityExplanation$setTitle(
            .("Understanding Cumulative Incidence at Selected Time Points"))
        } else {
          surv_title <- sprintf("%s at Selected Time Points (%s)",
                                estimand_meta$probability,
                                self$options$timetypeoutput)
          self$results$survTable$getColumn("surv")$setTitle(
            estimand_meta$probability)
          self$results$survTableSummary$setTitle(
            paste0(estimand_meta$probability, ": Natural Language Summary"))
          self$results$survTableHeading3$setTitle(
            paste0(estimand_meta$probability, ": Explanations"))
          self$results$survivalProbabilityExplanation$setTitle(
            paste0("Understanding ", estimand_meta$probability))
        }
        self$results$survTableHeading$setTitle(surv_title)
        self$results$survTable$setTitle(surv_title)

        mydata[[mytime]] <-
          jmvcore::toNumeric(mydata[[mytime]])

        ## Median Survival Table ----

        private$.checkpoint()


        # Handle Competing Risk Analysis for Survival Table
        if (private$.isCompetingRisk()) {
          # For competing risk, we calculate Cumulative Incidence
          
          # Build the multi-state status as a FACTOR with explicit levels.
          #
          # Surv(time, status, type = "mstate") with a NUMERIC status treats the
          # LOWEST OBSERVED value as censoring -- not the value 0. In a cohort
          # with no censored subjects the observed values are {1, 2}, so code 1
          # (the event of interest) BECAME the censoring code, survfit returned
          # states ("(s0)", "2"), the lookup for state "1" failed, and the table
          # reported 0% cumulative incidence for an event that occurred in 169
          # of 200 subjects -- with a footnote saying it never happened. R also
          # warns that type = "mstate" is deprecated, for exactly this reason.
          #
          # With an explicit factor the states come back as
          # ("(s0)", "event", "competing") whichever codes happen to be present,
          # so the column can be selected by NAME and cannot be misread.
          mydata[["myoutcome_mstate"]] <- factor(
            mydata[[myoutcome]],
            levels = c(0, 1, 2),
            labels = c("censored", "event", "competing"))

          # composeTerm backtick-escapes user column names; asFormula validates
          # against jamovi's allow-list (Surv is allow-listed).
          f_str <- paste0('Surv(',
                          jmvcore::composeTerm(mytime),
                          ', ',
                          jmvcore::composeTerm("myoutcome_mstate"),
                          ') ~ 1')
          formula_mstate <- jmvcore::asFormula(f_str, additional_allowed_functions = c("Surv"))

          fit_mstate <- private$.safeExecute({
            private$.getCachedSurvfit(formula_mstate, mydata, "survtable_mstate")
          }, context = "survival_calculation")

          if (is.null(fit_mstate)) {
            private$.addError('Unable to perform competing risk analysis. Verify outcome is coded as 0 (censored), 1 (event of interest), 2 (competing event) and sufficient events exist.')
            return()
          }
          
          utimes <- private$.resolveCutpoints(self$options$cutp)
          utimes <- private$.supportedCutpoints(utimes, mydata[[mytime]], mydata[[myoutcome]])
          if (length(utimes) == 0) {
            private$.addWarning('No requested time point falls within the observed follow-up, so the cumulative incidence table is empty. Enter cutpoints inside the follow-up range.')
            return()
          }

          s_summary <- summary(fit_mstate, times = utimes, extend = TRUE)

          # Extract the CIF of the event of interest BY STATE NAME. The factor
          # levels above guarantee the "event" state exists whether or not any
          # code-1 event was observed, so this lookup is deterministic.
          states <- s_summary$states
          if (is.null(states)) states <- colnames(s_summary$pstate)
          ev_col <- match("event", states)

          if (is.na(ev_col)) {
            # Unreachable with the factor fit above. If it ever happens the fit
            # is not the object we built and NOTHING here is safe to report -- a
            # failed state lookup is an ERROR condition, not a zero. The previous
            # pass zero-filled here, which is how a real 84.5% cumulative
            # incidence was printed as 0% under a footnote asserting the event
            # was never observed.
            private$.addError('Competing risk analysis failed: the fitted model does not contain the event-of-interest state. Verify the outcome is coded 0 (censored), 1 (event of interest), 2 (competing event).')
            return()
          }

          # summary.survfitms returns times x states matrices, but tolerate a
          # dropped dimension rather than silently indexing a vector by column.
          pick <- function(x, col) if (is.null(dim(x))) x[col] else x[, col]

          n_risk_val <- pick(s_summary$n.risk, 1)
          cif_est <- pick(s_summary$pstate, ev_col)
          cif_se <- pick(s_summary$std.err, ev_col)
          cif_lower <- pick(s_summary$lower, ev_col)
          cif_upper <- pick(s_summary$upper, ev_col)
          n_event_val <- pick(s_summary$n.event, ev_col)

          # Standard large-sample transformations degenerate at the probability
          # boundaries: before the first target event they can print 0%-0%, and
          # at a terminal boundary 100%-100%. Those are zero estimated-variance
          # artefacts, not proof that the population probability is known.
          cif_boundary <- is.finite(cif_est) & (cif_est <= 0 | cif_est >= 1)
          cif_lower[cif_boundary] <- NA_real_
          cif_upper[cif_boundary] <- NA_real_

          # A genuine zero is decided from the DATA, never from a lookup result.
          n_event_of_interest <- sum(mydata[[myoutcome]] == 1, na.rm = TRUE)
          if (n_event_of_interest == 0) {
            private$.addWarning('No events of interest (outcome code 1) were observed. The cumulative incidence point estimate is 0 at every time point; only competing events and/or censoring were observed. Check the selected event level before interpreting this as low risk.')
            # Aalen-Johansen's asymptotic variance is zero when the observed
            # count is zero, producing a displayed 0%-0% interval. That is not
            # evidence that the population risk is known exactly; it is a
            # boundary failure of the large-sample interval. Leave the interval
            # blank and say why rather than presenting false certainty.
            cif_lower[] <- NA_real_
            cif_upper[] <- NA_real_
          }

          km_fit_df <- data.frame(
            time = s_summary$time,
            n.risk = n_risk_val,
            n.event = n_event_val,
            surv = cif_est,
            std.err = cif_se,
            lower = cif_lower,
            upper = cif_upper
          )

          survTable <- self$results$survTable
          for (i in seq_along(km_fit_df[, 1, drop = T])) {
            survTable$addRow(rowKey = i, values = c(km_fit_df[i,]))
          }
          
          km_fit_df$ci <- private$.ciText(km_fit_df$lower, km_fit_df$upper)

          km_fit_df %>%
            dplyr::mutate(
              description = glue::glue(
                "At {time} {self$options$timetypeoutput}, the cumulative incidence of the event of interest is {scales::percent(surv)}{ci}."
              ),
            ) %>%
            dplyr::select(description) %>%
            dplyr::pull(.) -> survTableSummary
            
          self$results$survTableSummary$setContent(survTableSummary)
          
          # The footnote used to make this claim unconditionally, which is how
          # a table of competing-event incidence passed for the event of
          # interest. Key it on the observed data, not on a lookup result.
          self$results$survTable$setNote("boundary_ci", NULL)
          self$results$survTable$setNote(
            key = "cif_note",
            note = if (n_event_of_interest == 0)
              "Note: this table reports the Cumulative Incidence of the event of interest (outcome code 1), not survival. No such event was observed, so the point estimate is 0 throughout. The asymptotic Aalen-Johansen interval degenerates to 0%-0% at this boundary and is therefore left blank; absence of observed events does not prove zero population risk."
            else
              "Note: this table reports the Cumulative Incidence of the event of interest (outcome code 1), not survival. It is not 1 minus a Kaplan-Meier estimate. Competing events (code 2) are not counted as events; they remove subjects from the population still able to have the event of interest. Confidence limits are left blank where the point estimate is exactly 0% or 100%, because the usual large-sample interval degenerates at those boundaries."
          )

          # This branch used to return here, leaving the "Understanding Survival
          # Probabilities" panel visible and EMPTY whenever explanations were
          # on. The panel that follows describes Kaplan-Meier survival, which is
          # not what this branch computed, so it gets its own text rather than
          # falling through to the standard one.
          if (self$options$showExplanations) {
            self$results$survivalProbabilityExplanation$setContent(paste0(
              '<div class="explanation-box" style="background-color: rgba(33, 152, 255, 0.07); padding: 15px; border-radius: 8px; margin: 10px 0; color: inherit;">',
              '<h3 style="color: #2c5282; margin-top: 0;">', .("Understanding Cumulative Incidence at Selected Time Points"), '</h3>',
              '<p>', .("Each row gives the estimated probability that the event of interest has occurred by that time, accounting for the competing event. It is a cumulative incidence function (CIF), not a survival probability, and it is not 1 minus a Kaplan-Meier estimate: subjects who have the competing event can no longer experience the event of interest, and a Kaplan-Meier analysis that censored them would over-state the risk."), '</p>',
              '<ul>',
              '<li>', .("<b>Number at Risk</b> - subjects still under follow-up and still free of both events at that time."), '</li>',
              '<li>', .("<b>Cumulative incidence</b> - the proportion of the original cohort who have had the event of interest by that time. It never decreases."), '</li>',
              '<li>', .("<b>95% CI</b> - pointwise interval for the incidence at that single time point; it is not a confidence band over the whole curve, so do not read a series of them as one simultaneous statement."), '</li>',
              '<li>', .("The cumulative incidence of the event of interest and that of the competing event add up to the total probability of leaving the event-free state; neither on its own reaches 100%."), '</li>',
              '</ul>',
              '<p><em>', .("These are descriptive estimates from a single cohort with no comparison group."), '</em></p>',
              '</div>'))
          }

          return()
        }
        
        # composeTerm backtick-escapes user column names; asFormula validates
        # against jamovi's allow-list (Surv is allow-listed).
        formula <-
          paste0('Surv(',
                 jmvcore::composeTerm(mytime),
                 ', ',
                 jmvcore::composeTerm(myoutcome),
                 ') ~ ',
                 jmvcore::composeTerm(myfactor))

        formula <- jmvcore::asFormula(formula, additional_allowed_functions = c("Surv"))

        km_fit <- private$.safeExecute({
          private$.getCachedSurvfit(formula, mydata, "survtable")
        }, context = "survival_calculation")

        if (is.null(km_fit)) {
          private$.addError('Unable to perform survival analysis for time-specific estimates. Check for sufficient events at requested time points and valid survival data.')
          private$.displayMessages()
          return()
        }

        utimes <- private$.resolveCutpoints(self$options$cutp)
        utimes <- private$.supportedCutpoints(utimes, mydata[[mytime]], mydata[[myoutcome]])
        if (length(utimes) == 0) {
          private$.addWarning('No requested time point falls within the observed follow-up, so the survival table is empty. Enter cutpoints inside the follow-up range.')
          self$results$survTableSummary$setContent("")
          return()
        }

        private$.checkpoint()

        km_fit_summary <- summary(km_fit, times = utimes, extend = TRUE)

        km_fit_df <-
          as.data.frame(km_fit_summary[c(
                                         "time",
                                         "n.risk",
                                         "n.event",
                                         "surv",
                                         "std.err",
                                         "lower",
                                         "upper")])

        # At a time where the curve has reached 0 (see .supportedCutpoints)
        # survfit returns std.err = NaN. NA renders as an empty cell; NaN does
        # not necessarily.
        for (col in c("std.err", "lower", "upper"))
          km_fit_df[[col]][!is.finite(km_fit_df[[col]])] <- NA_real_

        # Greenwood/log intervals have zero estimated variance while the curve
        # is exactly 1 (and can degenerate at 0). A displayed 100%-100% interval
        # before any event, especially in an all-censored cohort, is easily read
        # as certainty about population survival. Leave boundary limits blank;
        # the point estimate and risk set remain reportable.
        km_boundary <- is.finite(km_fit_df$surv) &
          (km_fit_df$surv <= 0 | km_fit_df$surv >= 1)
        if (any(km_boundary)) {
          km_fit_df$lower[km_boundary] <- NA_real_
          km_fit_df$upper[km_boundary] <- NA_real_
          # This is a survival table; the cumulative-incidence note must not
          # survive a switch back from competing-risks mode.
          self$results$survTable$setNote("cif_note", NULL)
          self$results$survTable$setNote(
            "boundary_ci",
            .("Confidence limits are left blank where the Kaplan-Meier event-free estimate is exactly 0% or 100%. The usual Greenwood-based large-sample interval degenerates at a probability boundary; the absence of observed events does not establish zero population risk."))
        }

        survTable <- self$results$survTable

        data_frame <- km_fit_df
        for (i in seq_along(data_frame[, 1, drop = T])) {
          survTable$addRow(rowKey = i, values = c(data_frame[i,]))
        }


        ## survTableSummary 1,3,5-yr survival summary ----

        # summary.survfit(times = ...) reports n.event as the events occurring
        # SINCE THE PREVIOUS REQUESTED TIME, not a running total, and n.risk as
        # the number still at risk at that time. The old narrative called
        # n.event "events [that] had occurred" (cumulative) and then divided it
        # by n.risk[1] -- the risk set at the FIRST CUTPOINT, which it called
        # "the initial cohort". Both are wrong whenever the first cutpoint is
        # after time zero or anyone was censored before it: with 10 subjects
        # and a first cutpoint at 2 months it reported 1/9 = "11% of the
        # initial cohort". Use the real cohort size and state both the interval
        # count and the running total explicitly.
        #
        # cumsum() and the "since the previous cutpoint" interval both assume an
        # ASCENDING time vector; .resolveCutpoints() now guarantees one. Before
        # that, "12, 36, 60, 24" produced n.event 0/1/1/1 against 76 real events
        # and the sentence "1 event(s) occurred between 60 and 24 months".
        cohort_n <- km_fit$n[1]
        km_fit_df$events_cum <- cumsum(km_fit_df$n.event)
        km_fit_df$prev_time <- c(0, utils::head(km_fit_df$time, -1))
        km_fit_df$ci <- private$.ciText(km_fit_df$lower, km_fit_df$upper)

        km_fit_df %>%
          dplyr::mutate(
            description =
              glue::glue(
                "At {time} {self$options$timetypeoutput}, {tolower(estimand_meta$probability)} was {scales::percent(surv)}{ci}. \n At this time point {n.risk} of the {cohort_n} subjects were still at risk. \n {n.event} event(s) occurred between {prev_time} and {time} {self$options$timetypeoutput}; {events_cum} event(s) had been observed by {time} {self$options$timetypeoutput}. The observed event count is not 1 minus the Kaplan-Meier estimate because censoring is accounted for in the estimate."
              )
          ) %>%
          dplyr::select(description) %>%
          dplyr::pull(.) -> survTableSummary


        self$results$survTableSummary$setContent(survTableSummary)

        # Add explanatory output for survival probabilities
        if (self$options$showExplanations) {
            survival_probability_explanation_html <- '
            <div class="explanation-box" style="background-color: rgba(33, 152, 255, 0.07); padding: 15px; border-radius: 8px; margin: 10px 0; color: inherit;">
                <h3 style="color: #2c5282; margin-top: 0;"> Understanding Kaplan-Meier Time-Specific Estimates</h3>
                
                <div style="background-color: white; padding: 12px; border-radius: 5px; margin: 10px 0;">
                    <h4 style="color: #2d3748; margin-top: 0;">What are Time-Specific Event-Free Probabilities?</h4>
                    <p style="margin: 8px 0;">These show the <strong>estimated percentage of the cohort remaining event-free</strong> at specific milestone time points.
                    The displayed time points are exactly those selected for this analysis, in the declared output unit.</p>
                    
                    <div style="background-color: rgba(33, 184, 255, 0.11); padding: 10px; border-radius: 5px; margin: 10px 0; color: inherit;">
                        <strong> Example Interpretation:</strong>
                        <p style="margin: 5px 0;">If the event-free estimate at a selected time is 75% (95% CI: 68-82%)</p>
                        <ul style="margin: 5px 0; padding-left: 20px;">
                            <li>an estimated 75% remained event-free at 5 years</li>
                            <li>an estimated 25% had the event by 5 years</li>
                            <li>The confidence interval summarizes sampling uncertainty under the model assumptions; it is not an individual-patient range</li>
                        </ul>
                    </div>
                </div>
                
                <div style="background-color: rgba(246, 163, 33, 0.11); padding: 12px; border-radius: 5px; margin: 10px 0; color: inherit;">
                    <h4 style="color: #d68910; margin-top: 0;"> Understanding Each Column</h4>
                    <table style="width: 100%; border-collapse: collapse; margin: 10px 0;">
                        <tr style="background-color: rgba(255, 202, 33, 0.23); color: inherit;">
                            <th style="padding: 8px; text-align: left; border: 1px solid #ffc107;">Column</th>
                            <th style="padding: 8px; text-align: left; border: 1px solid #ffc107;">Meaning</th>
                            <th style="padding: 8px; text-align: left; border: 1px solid #ffc107;">Clinical Use</th>
                        </tr>
                        <tr>
                            <td style="padding: 8px; border: 1px solid #ffc107;"><strong>Time</strong></td>
                            <td style="padding: 8px; border: 1px solid #ffc107;">Milestone timepoint</td>
                            <td style="padding: 8px; border: 1px solid #ffc107;">Standard follow-up intervals</td>
                        </tr>
                        <tr style="background-color: rgba(255, 196, 33, 0.07); color: inherit;">
                            <td style="padding: 8px; border: 1px solid #ffc107;"><strong>Number at Risk</strong></td>
                            <td style="padding: 8px; border: 1px solid #ffc107;">Subjects event-free and uncensored just before the time point</td>
                            <td style="padding: 8px; border: 1px solid #ffc107;">Reliability of estimates</td>
                        </tr>
                        <tr>
                            <td style="padding: 8px; border: 1px solid #ffc107;"><strong>Number of Events</strong></td>
                            <td style="padding: 8px; border: 1px solid #ffc107;">Events in interval</td>
                            <td style="padding: 8px; border: 1px solid #ffc107;">Observed event counts between requested time points</td>
                        </tr>
                        <tr style="background-color: rgba(255, 196, 33, 0.07); color: inherit;">
                            <td style="padding: 8px; border: 1px solid #ffc107;"><strong>Event-free estimate</strong></td>
                            <td style="padding: 8px; border: 1px solid #ffc107;">Kaplan-Meier probability for the selected endpoint</td>
                            <td style="padding: 8px; border: 1px solid #ffc107;">Descriptive cohort context</td>
                        </tr>
                        <tr>
                            <td style="padding: 8px; border: 1px solid #ffc107;"><strong>95% CI</strong></td>
                            <td style="padding: 8px; border: 1px solid #ffc107;">Uncertainty range</td>
                            <td style="padding: 8px; border: 1px solid #ffc107;">Precision assessment</td>
                        </tr>
                    </table>
                </div>
                
                <div style="background-color: rgba(33, 159, 43, 0.1); padding: 12px; border-radius: 5px; margin: 10px 0; color: inherit;">
                    <h4 style="color: #2e7d32; margin-top: 0;"> Descriptive Clinical Context</h4>
                    
                    <div style="background-color: white; padding: 10px; border-radius: 5px; margin: 10px 0;">
                        <strong> Cohort description:</strong>
                        <p style="margin: 5px 0;">"In this cohort, about 8 out of 10 patients were event-free at 3 years"</p>
                    </div>
                    
                    <div style="background-color: rgba(153, 33, 170, 0.12); padding: 10px; border-radius: 5px; margin: 10px 0; color: inherit;">
                        <strong> Descriptive Interpretation:</strong>
                        <ul style="margin: 5px 0; padding-left: 20px;">
                            <li>These estimates describe this cohort; they do not identify a treatment effect or prescribe a follow-up schedule</li>
                            <li>Later estimates should be read with the number at risk and the confidence interval</li>
                            <li>Wide confidence intervals indicate limited precision, which may reflect few events or a small late risk set</li>
                        </ul>
                    </div>
                    
                    <div style="background-color: rgba(33, 152, 239, 0.13); padding: 10px; border-radius: 5px; margin: 10px 0; color: inherit;">
                        <strong> Comparison with Standards:</strong>
                        <p style="margin: 5px 0;">Contextual comparisons may use the sources below, but differences in case mix, entry dates, outcome definitions, and follow-up can invalidate direct comparisons:</p>
                        <ul style="margin: 5px 0; padding-left: 20px;">
                            <li>Historical controls from your institution</li>
                            <li>Published literature for similar populations</li>
                            <li>Registry data (SEER, national cancer registries)</li>
                        </ul>
                    </div>
                </div>
                
                <div style="background-color: rgba(255, 169, 33, 0.14); padding: 10px; border-radius: 5px; margin-top: 10px; border-left: 4px solid #ff9800; color: inherit;">
                    <strong> Important Considerations:</strong>
                    <ul style="margin: 5px 0; padding-left: 20px;">
                        <li><strong>Sample size matters:</strong> Fewer patients at later time points = less reliable estimates</li>
                        <li><strong>Confidence intervals:</strong> Wider intervals = more uncertainty</li>
                        <li><strong>Clinical context:</strong> Consider patient selection, treatment changes over time</li>
                        <li><strong>Censoring:</strong> Administrative censoring, withdrawal, and loss to follow-up are distinct; Kaplan-Meier estimation assumes censoring is non-informative conditional on the analysis</li>
                    </ul>
                </div>
            </div>
            '
            
            
            self$results$survivalProbabilityExplanation$setContent(survival_probability_explanation_html)
        }


      }

      # Person-Time Analysis Function ----
      ,
      .personTimeAnalysis = function(results) {
        # Check if person_time option is enabled
        if (!self$options$person_time) {
          return()
        }

        # Extract data
        mytime <- results$name1time
        myoutcome <- results$name2outcome
        mydata <- results$cleanData

        if (private$.isCompetingRisk()) {
          private$.addInfo(paste0(
            'Person-time output is a crude cause-specific occurrence/exposure rate for ',
            'the event of interest (code 1), not cumulative incidence or absolute risk. ',
            'Competing events (code 2) are not counted as target events and stop their ',
            'subjects\' subsequent person-time at the observed competing-event time.'))
        }

        # Ensure time is numeric
        mydata[[mytime]] <- jmvcore::toNumeric(mydata[[mytime]])

        # Get total observed time
        total_time <- sum(mydata[[mytime]])

        if (!is.finite(total_time) || total_time <= 0) {
          private$.addError(
            'Person-time rates cannot be calculated because the cohort accrued no positive follow-up time. Survival/CIF estimates at time zero remain valid, but a rate needs a positive person-time denominator.')
          return()
        }

        # Define event indicator consistently
        if (private$.isCompetingRisk()) {
          event_indicator <- mydata[[myoutcome]] == 1  # event of interest only
        } else {
          event_indicator <- mydata[[myoutcome]] >= 1  # any event
        }
        total_events <- sum(event_indicator, na.rm = TRUE)

        if (any(event_indicator & mydata[[mytime]] == 0, na.rm = TRUE)) {
          private$.addWarning(paste0(
            'Person-time rates were not calculated because one or more target events ',
            'occurred at time zero. Such events form a probability mass at the origin ',
            'and cannot be divided by other subjects\' later follow-up to create a ',
            'finite continuous occurrence rate. The Kaplan-Meier or cumulative-',
            'incidence results remain valid for reporting the time-zero event mass.'))
          return()
        }

        # Get time unit
        time_unit <- self$options$timetypeoutput

        person_table <- self$results$personTimeTable
        if (private$.isCompetingRisk()) {
          person_table$setTitle(.("Crude Cause-Specific Person-Time Rate"))
          person_table$getColumn("rate")$setTitle(
            .("Crude cause-specific rate"))
        } else {
          person_table$setTitle(.("Person-Time Analysis"))
          person_table$getColumn("rate")$setTitle(.("Crude event rate"))
        }

        # Get rate multiplier.
        #
        # It is a UNIT OF EXPRESSION ("per 100 person-years"), not a coefficient:
        # every rate and both confidence limits are multiplied by it. Nothing
        # checked its sign, so rate_multiplier = -100 printed an incidence rate
        # of -9.09 per -100 person-months with a "95% CI" of -2.95 to -21.22 --
        # negative, and with the limits in reverse order, for a cohort with 5
        # events. A count per unit time cannot be negative and there is no
        # "per zero" unit either.
        rate_multiplier <- self$options$rate_multiplier
        if (!is.finite(rate_multiplier) || rate_multiplier <= 0) {
          private$.addError(sprintf('Rate multiplier must be a finite positive number (received %s). It is the unit rates are expressed in - 100 for "events per 100 person-%s", 1000 for "per 1000". A negative or zero multiplier would report negative incidence rates. Person-time analysis was not performed.',
                                    format(self$options$rate_multiplier),
                                    self$options$timetypeoutput))
          return()
        }

        # Calculate overall incidence rate
        overall_rate <- (total_events / total_time) * rate_multiplier

        # Calculate confidence intervals using Poisson exact method
        ci_lower <- (stats::qchisq(0.025, 2*total_events) / 2) / total_time * rate_multiplier
        ci_upper <- (stats::qchisq(0.975, 2*(total_events + 1)) / 2) / total_time * rate_multiplier

        # The bounds below are Garwood limits under the stated Poisson count
        # model, so a row with almost no accrued person-time genuinely cannot
        # rule out a very high rate -- a sliver of a
        # final interval left by one long-surviving subject legitimately produces
        # an upper bound in the thousands. That is correct and must not be capped;
        # what was missing was any statement of what the reader is looking at.
        person_table$setNote(
          "ci",
          .("Garwood Poisson 95% CI, conditional on a Poisson count model and the observed person-time. Rows with 0 events give a one-sided 97.5% upper bound; intervals with very little accrued person-time yield correspondingly wide bounds. The interval does not address informative censoring, competing-risk absolute incidence, time-varying rates within an interval, or between-patient heterogeneity."))

        # Add to personTimeTable - first the overall row
        person_table$addRow(rowKey=1, values=list(
          interval=paste0("Overall (0-max)"),
          events=total_events,
          person_time=round(total_time, 2),
          rate=round(overall_rate, 2),
          rate_ci_lower=round(ci_lower, 2),
          rate_ci_upper=round(ci_upper, 2)
        ))

        # Parse time intervals for stratified analysis
        time_intervals <- private$.resolveCutpoints(self$options$time_intervals,
                                                    "Person-time intervals",
                                                    allow_zero = FALSE)
        # (.resolveCutpoints() already sorts; kept as a cheap belt-and-braces.)
        time_intervals <- sort(time_intervals)

        # Drop cut-points that lie beyond the observed follow-up BEFORE building
        # the breaks vector.
        #
        # breaks was built as c(0, time_intervals, max(time) * 1.1). With the
        # default cut-points 12/36/60 and a cohort followed only 40 months that
        # gives c(0, 12, 36, 60, 44) -- NOT monotonic. The final interval then
        # ran from 60 to 44, so any subject still at risk past 60 contributed
        # 44 - 60 = -16 months of person-time, and a negative denominator went
        # straight into an incidence rate. .resolveCutpoints() only parses and
        # sorts; it does not filter, which is why this survived the unit fixes.
        # Person-time genuinely cannot accrue past the last observation, so
        # unlike .supportedCutpoints() this filter is unconditional.
        .max_fu <- suppressWarnings(max(mydata[[mytime]], na.rm = TRUE))
        if (is.finite(.max_fu)) {
          .dropped <- time_intervals[time_intervals >= .max_fu]
          time_intervals <- time_intervals[time_intervals < .max_fu]
          if (length(.dropped) > 0)
            private$.addInfo(sprintf(
              .("Person-time interval boundaries %s are at or beyond the longest observed follow-up (%s %s), so no person-time can accrue past them. They were omitted rather than producing an empty or negative interval."),
              paste(base::format(.dropped, trim = TRUE), collapse = ", "),
              format(round(.max_fu, 1), trim = TRUE),
              self$options$timetypeoutput))
        }

        if (length(time_intervals) > 0) {
          # Create time intervals with slightly extended upper bound to capture all events
          # Example: if intervals are [12, 36, 60], breaks = [0, 12, 36, 60, max_time*1.1]
          breaks <- c(0, time_intervals, max(mydata[[mytime]]) * 1.1)

          # Strategy for interval-specific person-time calculation:
          # - First interval [0, t1]: All patients enter at time 0 (standard analysis)
          # - Later intervals (t_i, t_{i+1}]: Left-truncate at start_time (conditional survival)
          #   Only patients who survived past t_i contribute person-time to interval i+1

          # Loop through each time interval
          for (i in 1:(length(breaks)-1)) {
            start_time <- breaks[i]
            end_time <- breaks[i+1]

            if (i == 1) {
              # First interval [0, end_time]: All patients enter at time 0
              interval_data <- mydata

              # Calculate person-time for each patient in this interval
              # Person-time = min(observed_time, interval_end) - 0 (right-censoring at interval end)
              follow_up_times <- pmin(mydata[[mytime]], end_time)

              # Count events that occurred within [0, end_time]
              events_in_interval <- sum(event_indicator & mydata[[mytime]] <= end_time, na.rm = TRUE)

            } else {
              # Later intervals (start_time, end_time]: Left-truncate at interval start
              # Only include patients who survived beyond start_time (conditional survival)
              # This implements proper left-truncation for interval-specific rates
              survivors <- mydata[[mytime]] > start_time
              interval_data <- mydata[survivors, ]

              if (nrow(interval_data) == 0) {
                # No patients remaining in this interval - skip to next
                next
              }

              # Calculate person-time contribution for each patient in this interval
              # Entry time: start_time (left-truncated - patients enter interval if they survived past start)
              # Exit time: min(actual_exit_time, interval_end) (right-censored if still alive at end)
              adjusted_entry_time <- rep(start_time, nrow(interval_data))
              adjusted_exit_time <- pmin(interval_data[[mytime]], end_time)

              # Person-time in interval = exit - entry (accounting for both truncation and censoring)
              follow_up_times <- adjusted_exit_time - adjusted_entry_time

              # Count events within this interval: (start_time, end_time]
              # Left-open interval to avoid double-counting boundary events
              interval_events_flag <- event_indicator[survivors]
              events_in_interval <- sum(
                interval_events_flag &
                interval_data[[mytime]] <= end_time &
                interval_data[[mytime]] > start_time,  # Left-open, right-closed interval
                na.rm = TRUE
              )
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
              interval_label <- if (i == length(breaks) - 1L)
                paste0(base::format(start_time, trim = TRUE), "+") else
                paste0(base::format(start_time, trim = TRUE), "-",
                       base::format(end_time, trim = TRUE))
              person_table$addRow(rowKey=i+1, values=list(
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

        # Calculate additional statistics
        mean_follow_up <- round(total_time / nrow(mydata), 2)
        # Reverse Kaplan-Meier, not median(observed times) -- see .medianFollowUp().
        mfu <- private$.medianFollowUp(mydata[[mytime]], mydata[[myoutcome]])
        median_follow_up <- round(mfu$value, 2)
        median_follow_up_label <- if (mfu$reverse)
          "Median follow-up (reverse Kaplan-Meier)" else
          "Median observed time (reverse Kaplan-Meier not estimable)"
                # Create summary text with interpretation
        summary_html <- glue::glue(
          "
    In this study, {nrow(mydata)} subjects were followed for a total of {round(total_time, 1)} {time_unit},
    with an average follow-up duration of {mean_follow_up} {time_unit} per person. During this observation period,
    {total_events} events occurred, resulting in an incidence rate of {round(overall_rate, 2)} events per {rate_multiplier} person-{time_unit}.

    <p><b>Understanding the Rate Multiplier:</b> The rate multiplier of {rate_multiplier} is used to express incidence rates in
    clinically meaningful terms. Instead of reporting small decimal numbers (e.g., 0.05 events per {time_unit}), we scale the rate
    to show events per {rate_multiplier} person-{time_unit}. For example, {rate_multiplier} patients followed for one {time_unit} each
    contribute {rate_multiplier} person-{time_unit}; the observed aggregate rate corresponds to {round(overall_rate, 1)} events per that amount of person-time.
    This standardized expression makes it easier to understand and compare rates across different studies.</p>

    The 95% confidence interval for this rate is {round(ci_lower, 2)} to {round(ci_upper, 2)} per {rate_multiplier} person-{time_unit},
    indicating the precision of our estimate based on the observed data.


    <h4>Person-Time Analysis Summary</h4>
    <p>Total follow-up time: <b>{round(total_time, 1)} {time_unit}</b></p>
    <p>Mean follow-up time: <b>{mean_follow_up} {time_unit}</b></p>
    <p>{median_follow_up_label}: <b>{median_follow_up} {time_unit}</b></p>
    <p>Number of events: <b>{total_events}</b> out of <b>{nrow(mydata)}</b> subjects</p>
    <p>Overall incidence rate: <b>{round(overall_rate, 2)}</b> per {rate_multiplier} person-{time_unit} [95% CI: {round(ci_lower, 2)}-{round(ci_upper, 2)}]</p>
    <p><i>Interpretation:</i> This is a crude occurrence/exposure rate, not the probability that an individual has the event. It is calculated as target events divided by observed person-time at risk. Under competing risks it is cause-specific and is not cumulative incidence. The interval is a Garwood interval conditional on a Poisson count model and does not account for unmeasured heterogeneity, informative censoring, or changes in the rate over follow-up.</p>
  ")

        self$results$personTimeSummary$setContent(summary_html)

        # Add explanatory output for person-time analysis
        if (self$options$showExplanations) {
            person_time_explanation_html <- '
            <div style="margin-bottom: 20px; padding: 15px; background-color: rgba(33, 152, 255, 0.07); border-left: 4px solid #4169e1; color: inherit;">
                <h4 style="margin-top: 0; color: #2c3e50;">Understanding Person-Time Analysis</h4>
                <p style="margin-bottom: 10px;">Person-time analysis calculates incidence rates by accounting for the total time each patient was at risk:</p>
                <ul style="margin-left: 20px;">
                    <li><strong>Person-Time:</strong> Sum of individual follow-up periods for all patients</li>
                    <li><strong>Incidence Rate:</strong> Events per unit of person-time in the selected time unit</li>
                    <li><strong>Rate Multiplier:</strong> Scaling factor to express rates per standard unit</li>
                    <li><strong>95% CI:</strong> Garwood Poisson interval, conditional on the count model and observed person-time</li>
                </ul>
                <p style="margin-bottom: 5px;"><strong>Clinical interpretation:</strong></p>
                <ul style="margin-left: 20px;">
                    <li>Different follow-up durations contribute appropriately to the denominator</li>
                    <li>Rates are not risks or individual event probabilities</li>
                    <li>With competing events, this is a crude cause-specific rate and not the cumulative incidence or absolute risk</li>
                    <li>Cross-study comparisons require comparable populations, outcome definitions, time origins, and censoring mechanisms</li>
                    <li>Interval-specific rates are exploratory and may be unstable when events or person-time are sparse</li>
                    <li>Do not use a visual peak alone to choose follow-up or treatment timing</li>
                </ul>
            </div>
            '
            self$results$personTimeExplanation$setContent(person_time_explanation_html)
        }
        
      }

      # Plot Explanations ----
      ,
      # Explanations of the PLOTS have nothing to do with person-time, but they
      # used to be generated at the bottom of .personTimeAnalysis(), which
      # returns immediately when "Calculate person-time metrics" is off. So
      # ticking a plot plus "Analysis explanations" produced a heading (.init
      # makes the panel visible from the options alone) above an empty box,
      # unless the user happened to also want person-time. Called from .run()
      # now, gated on nothing but its own two options.
      .plotExplanations = function() {
        if (!self$options$showExplanations ||
            !(self$options$sc || self$options$ce || self$options$ch || self$options$kmunicate))
          return()

        unit <- htmltools::htmlEscape(self$options$timetypeoutput)
        if (private$.isCompetingRisk()) {
          txt <- paste0(
            '<div class="explanation-box" style="background-color: rgba(33, 152, 255, 0.07);padding:15px;border-radius:8px;margin:10px 0; color: inherit;">',
            '<h3 style="color:#2c5282;margin-top:0;">Understanding the Cumulative-Incidence Plot</h3>',
            '<p>The plot shows the cumulative incidence of each terminal event state. ',
            'For the event of interest, this is the estimated probability that it has occurred by time <i>t</i>, ',
            'with competing events accounted for. It is not 1 minus a Kaplan-Meier curve.</p>',
            '<ul><li><strong>X-axis:</strong> time in ', unit, '.</li>',
            '<li><strong>Y-axis:</strong> cumulative incidence on a 0-1 probability scale.</li>',
            '<li><strong>Steps:</strong> observed transitions to an event state; a competing event prevents the target event thereafter.</li>',
            '<li><strong>Confidence intervals:</strong> pointwise uncertainty, not a simultaneous confidence band.</li></ul>',
            '<p>The cumulative-event, cumulative-hazard and KMunicate plots are not produced in competing-risk mode because they assume one event type. ',
            'A numbers-at-risk panel is also not available on this CIF graphic; use the cumulative-incidence table for counts at selected times.</p>',
            '<p><em>These are descriptive estimates from one cohort and do not establish treatment effects or individual prognosis.</em></p></div>')
        } else {
          txt <- paste0(
            '<div class="explanation-box" style="background-color: rgba(33, 152, 255, 0.07);padding:15px;border-radius:8px;margin:10px 0; color: inherit;">',
            '<h3 style="color:#2c5282;margin-top:0;">Understanding Survival Curves and Plots</h3>',
            '<p>The Kaplan-Meier curve estimates the probability of remaining event-free over time. ',
            'It steps down at event times; censoring changes the risk set but does not make the curve step down.</p>',
            '<ul><li><strong>X-axis:</strong> time in ', unit, '.</li>',
            '<li><strong>Y-axis:</strong> survival/event-free probability on a 0-1 scale.</li>',
            '<li><strong>Censor marks:</strong> follow-up ended without the event at that time; this includes administrative censoring and is not synonymous with loss to follow-up.</li>',
            '<li><strong>Confidence intervals:</strong> pointwise uncertainty; late estimates often widen as the risk set shrinks.</li>',
            '<li><strong>Median:</strong> the first time estimated survival is 50% or lower, if reached.</li></ul>',
            '<p>The cumulative-event plot is 1 - S(t). The cumulative-hazard plot is an accumulated hazard measure, not an event probability and is not bounded by 1.</p>',
            '<p>Interpret all curves under the assumption that censoring is non-informative conditional on the analysis. Visual changes in steepness are exploratory and do not by themselves identify clinically optimal intervention times.</p></div>')
        }
        self$results$survivalPlotsExplanation$setContent(txt)
      }

      # Baseline Hazard Analysis Function ----
      ,
      .baselineHazardAnalysis = function(results) {
        if (!self$options$baseline_hazard && !self$options$hazard_smoothing)
          return()
        if (private$.isCompetingRisk()) {
          private$.addInfo(
            'Piecewise hazard rates are not computed for competing-risk outcomes. Use cause-specific or subdistribution-hazard methods when a hazard estimand is required; use the cumulative-incidence output for absolute risk.')
          return()
        }

        mytime <- results$name1time
        myoutcome <- results$name2outcome
        mydata <- results$cleanData
        mydata[[mytime]] <- jmvcore::toNumeric(mydata[[mytime]])
        total_events <- sum(mydata[[myoutcome]] == 1, na.rm = TRUE)
        if (total_events == 0) {
          private$.addInfo(
            'Piecewise hazard rates were not estimated because no events were observed. A non-zero event count and positive accrued person-time are required.')
          return()
        }

        # The smoother needs at least three interval-rate estimates. With the
        # documented automatic binning (about one bin per 10 events), fewer
        # intervals cannot support even the local-constant trend requested by
        # this output. State that algorithmic limitation in the results rather
        # than leaving a visible, blank image.
        if (self$options$hazard_smoothing) {
          hz_smooth <- private$.hazardIntervals(
            mydata[[mytime]], mydata[[myoutcome]])
          hz_smooth <- hz_smooth[
            is.finite(hz_smooth$rate) & hz_smooth$person_time > 0,
            , drop = FALSE]
          if (nrow(hz_smooth) < 3L) {
            private$.addInfo(sprintf(
              paste0(
                "The smoothed hazard curve was not estimated: automatic ",
                "binning produced %d usable interval(s) from %d event(s), but ",
                "at least three interval-rate estimates are needed for the ",
                "local-constant smoother. The unsmoothed piecewise table can ",
                "still be reported when requested."),
              nrow(hz_smooth), total_events))
          }
        }

        if (!self$options$baseline_hazard) return()

        hz <- private$.hazardIntervals(mydata[[mytime]], mydata[[myoutcome]])
        usable <- nrow(hz) > 0 && any(is.finite(hz$rate))
        if (!usable) {
          private$.addInfo(
            'Piecewise hazard rates were not estimated because one or more events occurred at time zero, or because no positive person-time was available. A time-zero event is a probability mass at the origin rather than a finite continuous hazard; report it with the survival or cumulative-incidence output.')
          return()
        }
        hz <- hz[is.finite(hz$rate), , drop = FALSE]

        self$results$baselineHazardTable$setNote(
          "method",
          paste0(
            "Each row is a piecewise occurrence/exposure rate for (previous endpoint, Time], ",
            "with the first interval including time zero. Equal-width intervals span the ",
            "observed follow-up; their number is limited to about one interval per 10 total ",
            "events (maximum 10) to reduce sparsity. Person-time is calculated exactly ",
            "from individual follow-up. Limits are Garwood Poisson 95% intervals conditional ",
            "on the observed exposure. These are interval rates, not pointwise instantaneous ",
            "hazards or Cox-model coefficients."))
        for (i in seq_len(nrow(hz))) {
          self$results$baselineHazardTable$addRow(rowKey = i, values = list(
            time = round(hz$end[i], 2),
            hazard = round(hz$rate[i], 4),
            hazard_lower = round(hz$lower[i], 4),
            hazard_upper = round(hz$upper[i], 4)))
        }

        n_bins <- nrow(hz)
        pooled_rate <- sum(hz$events) / sum(hz$person_time)
        if (n_bins >= 3L) {
          peak_text <- sprintf(
            '<b>%.4f</b> in the interval ending at %.1f %s (highest of %d equal-width intervals)',
            max(hz$rate), hz$end[which.max(hz$rate)],
            self$options$timetypeoutput, n_bins)
          variation_text <- sprintf(
            '<b>%.4f to %.4f</b> across %d equal-width intervals; this descriptive range is not a test of a constant-hazard, exponential, or proportional-hazards model',
            min(hz$rate), max(hz$rate), n_bins)
        } else {
          peak_text <- sprintf(
            'not separable from the pooled rate - %d event(s) support only %d interval(s)',
            total_events, n_bins)
          variation_text <- sprintf(
            '<b>not summarized</b> - automatic binning produced only %d interval(s) from %d event(s), so variation over time is not described',
            n_bins, total_events)
        }

        if (self$options$showSummaries) {
          self$results$baselineHazardSummary$setContent(glue::glue("
            <div style='background-color: rgba(255, 202, 33, 0.11);border-left:4px solid #ffc107;padding:12px;margin-bottom:15px; color: inherit;'>
              <p style='margin:5px 0;'><strong>Methodological note:</strong>
              This output reports piecewise event rates, computed as events divided by exact
              person-time within equal-width intervals. The number of intervals is limited to
              about one per 10 total events (maximum 10) to reduce sparse-bin artefacts. It does not
              estimate an exact instantaneous hazard at each event time. Garwood Poisson
              intervals are conditional on the count/exposure model and do not account for the
              data-dependent choice of interval boundaries.</p>
            </div>
            <h4>Exploratory Piecewise Hazard-Rate Summary</h4>
            <ul>
              <li>Pooled event rate: <b>{round(pooled_rate, 4)}</b> events per person-{self$options$timetypeoutput}</li>
              <li>Highest interval rate: {peak_text}</li>
              <li>Variation over time: {variation_text}</li>
            </ul>
            <p>Interval peaks are bandwidth- and boundary-dependent. They must not be used alone
            to time treatment, determine surveillance schedules, or make individual prognostic
            claims. Those decisions require clinical context and a prespecified, validated model.</p>"))
        }

        if (self$options$showExplanations) {
          self$results$baselineHazardExplanation$setContent(paste0(
            '<div class="explanation-box" style="background-color: rgba(33, 152, 255, 0.07);padding:15px;border-radius:8px;margin:10px 0; color: inherit;">',
            '<h3 style="color:#2c5282;margin-top:0;">Understanding Piecewise Hazard-Rate Estimates</h3>',
            '<p>A hazard is an event rate among subjects still at risk, expressed per unit of person-time. ',
            'It is not an event probability and can exceed 1 per time unit. This table groups follow-up into equal-width intervals and divides events by the exact person-time accrued in each interval.</p>',
            '<ul><li><strong>Time:</strong> upper endpoint of the interval; the row covers the preceding endpoint up to and including this time.</li>',
            '<li><strong>Rate:</strong> events/person-time within that interval.</li>',
            '<li><strong>95% CI:</strong> Garwood Poisson interval conditional on the observed exposure.</li>',
            '<li><strong>Smoothed curve:</strong> a bandwidth-dependent exploratory trend; different smoothing choices can move or remove peaks.</li></ul>',
            '<p>Neither visual constancy nor a coefficient of variation tests an exponential model, and proportional hazards cannot be assessed in a one-arm model with no contrast. ',
            'Censoring assumptions and sparse late risk sets remain important.</p></div>'))
        }
      }











      # Survival Curve ----
      ,
      .plot = function(image, ggtheme, theme, ...) {
        sc <- self$options$sc

        if (!sc)
          return()

        results <- image$state

        if (is.null(results)) {
          return()
        }

        # Validate plot parameters
        if (!private$.validatePlotParameters()) return()

        mytime <- results$name1time
        myoutcome <- results$name2outcome

        myfactor <- results$name3explanatory
        myfactor <-
        jmvcore::constructFormula(terms = myfactor)

        plotData <- results$cleanData

        plotData[[mytime]] <-
          jmvcore::toNumeric(plotData[[mytime]])

        # Get labels for variable name restoration
        labelled_data <- private$.getData()
        all_labels <- labelled::var_label(labelled_data$mydata_labelled)
        
        # Retain the user's time-variable label for the x axis. Formula
        # construction stays on the safely cleaned names below.
        original_time_name <- NULL
        for (clean_name in names(all_labels)) {
            if (clean_name == mytime) {
                original_time_name <- all_labels[[clean_name]]
            }
        }
        
        private$.checkpoint()

        # `results` is image$state -- see .isCompetingRisk(): a renderer can run
        # without .run() in this instance, so the flag comes off the state.
        #
        # This item is Kaplan-Meier only; the cumulative-incidence curve moved
        # to .plotCIF() so that the "Survival Plot" heading stops contradicting
        # its own contents. The refusal below is a SAFETY NET, not the gate:
        # jamovi/singlearm.r.yaml hides this item under competing risks and
        # .run() re-asserts that from the recode, but a stale .omv or an
        # R-syntax call can still reach this renderer with sc = TRUE, and a
        # blank panel is indistinguishable from a plot that failed to render.
        if (private$.isCompetingRisk(results)) {
            return(private$.competingRiskPlotRefusal(
              .("The Kaplan-Meier survival plot")))

        } else {
            # Standard KM Plot
            estimand_meta <- private$.estimandMeta(results)
            # Build the formula against the cleaned data, using jamovi's term
            # composer to escape spaces, operators and embedded backticks.
            # Restoring raw user names and surrounding them with literal
            # backticks was both unnecessary and unsafe for names containing a
            # backtick; it also risked duplicate restored column names.
            myformula <- paste0(
              "survival::Surv(", jmvcore::composeTerm(mytime), ", ",
              jmvcore::composeTerm(myoutcome), ")")

            plot_result <- plotData %>%
              finalfit::surv_plot(
                .data = .,
                dependent = myformula,
                explanatory = myfactor,
                xlab = if (!is.null(original_time_name))
                  paste0(original_time_name, " (", self$options$timetypeoutput, ")") else
                  paste0('Time (', self$options$timetypeoutput, ')'),
                legend = 'none',
                break.time.by = self$options$byplot,
                xlim = c(0, self$options$endplot),
                ylim = c(
                  self$options$ybegin_plot,
                  self$options$yend_plot),
                title = estimand_meta$curve,
                subtitle = .("Based on Kaplan-Meier estimates"),
                risk.table = self$options$risktable,
                conf.int = self$options$ci95,
                censor = self$options$censored,
                surv.median.line = self$options$medianline
              )

            # Extract plot object (surv_plot returns a list or ggsurvplot object)
            if (inherits(plot_result, "ggsurvplot")) {
              plot_obj <- plot_result$plot
            } else if (inherits(plot_result, "gg")) {
              plot_obj <- plot_result
            } else {
              stop("Unexpected plot result type from surv_plot")
            }

            # Apply colorblind-safe theme and colors
            plot_obj <- plot_obj +
              ggplot2::scale_color_manual(values = c("#0173B2", "#DE8F05", "#CC78BC", "#029E73", "#D55E00")) +
              ggplot2::scale_fill_manual(values = c("#0173B2", "#DE8F05", "#CC78BC", "#029E73", "#D55E00")) +
              ggtheme

            # Draw the risk table that was asked for.
            #
            # risk.table = TRUE was passed to surv_plot and then thrown away:
            # ggsurvplot returns a LIST whose $plot is the curve and whose $table
            # is the numbers-at-risk panel, and only $plot was kept and printed.
            # The option therefore did nothing at all on this plot -- silently,
            # because the curve rendered perfectly. Printing the ggsurvplot
            # object itself is survminer's own convention: its print method
            # arranges curve and table on one device.
            #
            # Known consequence: with the table shown the curve gets roughly
            # three quarters of the panel height, so a plot sized for the curve
            # alone becomes shorter. That is the cost of displaying the table
            # and only applies when the user asks for it.
            if (isTRUE(self$options$risktable) && !is.null(plot_result$table)) {
              plot_result$plot <- plot_obj
              print(plot_result)
              return(TRUE)
            }
        }

        print(plot_obj)
        TRUE
      }


      # Cumulative Incidence Function ----
      ,
      # The competing-risks counterpart of .plot(). It used to live inside
      # .plot() as a branch, which meant a cumulative-incidence curve was drawn
      # under a heading that read "Survival Plot" -- the one number a reader of
      # a competing-risks analysis must not confuse with 1 - S(t).
      .plotCIF = function(image, ggtheme, theme, ...) {
        if (!self$options$sc)
          return()

        results <- image$state

        if (is.null(results))
          return()

        # Same safety-net reasoning as .plot(): the r.yaml visible: expression
        # and .run()'s setVisible() keep this item off screen outside
        # competing-risk mode, but a stale .omv or an R-syntax call can still
        # reach the renderer, and cmprsk::cuminc() on a plain 0/1 status would
        # draw a curve that looks plausible and answers the wrong question.
        if (!private$.isCompetingRisk(results))
          return()

        if (!private$.validatePlotParameters()) return()

        mytime <- results$name1time
        myoutcome <- results$name2outcome

        plotData <- results$cleanData
        plotData[[mytime]] <- jmvcore::toNumeric(plotData[[mytime]])

        private$.checkpoint()

        status <- plotData[[myoutcome]]
        cr_subtitle <- .("For Competing Risks (Event of Interest vs. Competing Event)")

        if (!any(status != 0, na.rm = TRUE)) {
          # cmprsk::cuminc() has no curve to return when every observation
          # is censored, but the estimand is still defined: every CIF point
          # estimate is zero. Draw that valid boundary result explicitly.
          flat <- data.frame(time = c(0, max(plotData[[mytime]], na.rm = TRUE)),
                             incidence = c(0, 0))
          plot_obj <- ggplot2::ggplot(flat,
                                      ggplot2::aes(x = time, y = incidence)) +
            ggplot2::geom_step(color = "#0173B2", linewidth = 1.2) +
            ggplot2::labs(
              title = .("Cumulative Incidence Function (CIF)"),
              x = paste0('Time (', self$options$timetypeoutput, ')')) +
            ggplot2::coord_cartesian(
              xlim = c(0, self$options$endplot),
              ylim = c(self$options$ybegin_plot, self$options$yend_plot))
          cr_subtitle <- .("No terminal event observed; point estimate remains 0")
        } else {
          cuminc_fit <- private$.competingRiskCumInc(results)
          if (is.null(cuminc_fit))
            return(private$.competingRiskPlotRefusal(
              .("The cumulative-incidence plot could not be estimated")))

          plot_obj <- survminer::ggcompetingrisks(
              fit = cuminc_fit,
              conf.int = self$options$ci95,
              title = .("Cumulative Incidence Function (CIF)"),
              xlab = paste0('Time (', self$options$timetypeoutput, ')'),
              xlim = c(0, self$options$endplot),
              ylim = c(self$options$ybegin_plot, self$options$yend_plot),
              risk.table = FALSE)

          # Name the curves.
          #
          # survminer maps cmprsk's raw failure codes onto the colour scale, so
          # the legend read "event  1  2". Those are the internal codes from
          # .defineEventIndicator(), and nothing on the panel told the clinician
          # which of their own outcome levels each curve stood for. Relabelling
          # the data (rather than the scale) also fixes the confidence-band
          # fill, which is mapped to the same variable.
          ev_lab <- if (length(results$event_label) > 0 &&
                        nzchar(results$event_label[1]))
            results$event_label[1] else .("Event of interest")
          cr_lab <- if (length(results$competing_labels) > 0)
            paste(results$competing_labels, collapse = ", ") else
            .("Competing event")

          if (!is.null(plot_obj$data) && !is.null(plot_obj$data$event)) {
            # cmprsk/ggcompetingrisks omits a failure-type curve when that type
            # has zero observed events. In a declared competing-risk analysis
            # the missing curve is a valid flat-zero estimate, not an absent
            # estimand. Add explicit zero curves for either missing terminal
            # state so a cohort with only competing events does not visually
            # erase the target event from the legend and panel.
            add_flat_curve <- function(dat, code) {
              if (as.character(code) %in% as.character(dat$event) || nrow(dat) == 0)
                return(dat)
              flat <- dat[rep(1L, 2L), , drop = FALSE]
              flat$time <- c(0, max(plotData[[mytime]], na.rm = TRUE))
              if ("est" %in% names(flat)) flat$est <- 0
              if ("var" %in% names(flat)) flat$var <- NA_real_
              if ("std" %in% names(flat)) flat$std <- NA_real_
              if ("name" %in% names(flat)) flat$name <- paste("1", code)
              if ("group" %in% names(flat)) flat$group <- "1"
              flat$event <- as.character(code)
              rbind(dat, flat)
            }
            plot_obj$data <- add_flat_curve(plot_obj$data, 1L)
            plot_obj$data <- add_flat_curve(plot_obj$data, 2L)

            lab <- as.character(plot_obj$data$event)
            lab[lab == "1"] <- ev_lab
            lab[lab == "2"] <- cr_lab
            plot_obj$data$event <- factor(
              lab, levels = unique(c(ev_lab, cr_lab, lab)))
          }

          # Only name aesthetics that exist. ggplot2 prints "Ignoring unknown
          # labels" for the others, and the flat all-censored branch below maps
          # neither colour nor fill.
          plot_obj <- plot_obj + ggplot2::labs(color = .("Event type"))
          if (isTRUE(self$options$ci95))
            plot_obj <- plot_obj + ggplot2::labs(fill = .("Event type"))
        }

        plot_obj <- plot_obj +
          # ggcompetingrisks facets by group; a single-arm cohort is one group
          # called "1", so the panel carried a strip labelled "1" that meant
          # nothing to the reader.
          ggplot2::facet_null() +
          ggtheme +
          ggplot2::labs(
            subtitle = cr_subtitle,
            # survminer's default y label is "Probability of an event", which
            # is exactly the reading a competing-risks plot must not invite:
            # this is the cumulative incidence, not 1 - S(t).
            y = .("Cumulative incidence")) +
          ggplot2::scale_x_continuous(
            breaks = seq(0, self$options$endplot, by = self$options$byplot)) +
          ggplot2::theme(legend.position = 'bottom')

        print(plot_obj)
        TRUE
      }



      # Cumulative Events ----
      # https://rpkgs.datanovia.com/survminer/survminer_cheatsheet.pdf
      ,
      # Draw the reason instead of returning an empty panel.
      #
      # These renderers used to `return()` under competing risks, leaving a
      # blank plot area. A blank panel is indistinguishable from a plot that
      # failed to render, so the user cannot tell "deliberately not shown"
      # from "broken". survival.b.R already draws its refusals; singlearm was
      # missed when that change was scoped.
      .competingRiskPlotRefusal = function(feature) {
        # The advice has to match WHY competing risks are active.
        #
        # The shared text ends "set survival type to Overall or Cause Specific".
        # That is actionable only when the user picked Competing Risk. When the
        # 0/1/2 coding arrives WITH THE DATA (an outcomeorganizer hand-off sets
        # multievent FALSE and .isCompetingRisk() fires on the recode), Survival
        # Type is already Overall -- the panel was telling the user to set a
        # setting that is already in that state and cannot restore the plot.
        by_option <- isTRUE(self$options$multievent) &&
                     identical(self$options$analysistype, "compete")
        outcome_name <- if (is.null(self$options$outcome))
          .("the outcome variable") else self$options$outcome

        msg <- if (by_option)
          sprintf(.('%s is not available for competing-risks analysis: it assumes a single event type, and the competing-risk outcome is coded 0/1/2. Read the cumulative incidence in the median and survival tables instead, or set "Survival Type" to Overall, Cause Specific or Disease-Free to analyse these data as ordinary survival.'),
                  feature)
        else
          sprintf(.('%s is not available for competing-risks analysis: it assumes a single event type, and "%s" is coded 0/1/2, where 2 marks a competing event. "Survival Type" is not what put the analysis in this mode, so changing it will not bring this plot back. Read the cumulative incidence in the median and survival tables, which already account for the competing event, or supply an outcome with censored and event values only.'),
                  feature, outcome_name)

        p <- ggplot2::ggplot() +
          ggplot2::annotate("text", x = 0, y = 0, hjust = 0.5, vjust = 0.5,
                            size = 4, lineheight = 1.2,
                            label = paste(strwrap(msg, width = 60), collapse = "\n")) +
          ggplot2::theme_void()
        print(p)
        TRUE
      }
      ,
      .plot2 = function(image2, ggtheme, theme, ...) {
        # Competing-risk mode codes the outcome 0/1/2. survival::Surv() does
        # not reject that -- it warns and remaps 1 to censored, 2 to event and
        # 0 to NA, so this plot would render inverted with no visible warning.
        # Ask .isCompetingRisk(), not the options: the outcomeorganizer hand-off
        # delivers a 0/1/2 status with multievent = FALSE, and the option test
        # alone let exactly that case through to be plotted backwards.
        if (private$.isCompetingRisk(image2$state))
          return(private$.competingRiskPlotRefusal(.("The cumulative event probability plot")))

        ce <- self$options$ce

        if (!ce)
          return()

        results <- image2$state

        if (is.null(results)) {
          return()
        }

        # Validate plot parameters
        if (!private$.validatePlotParameters()) return()

        mytime <- results$name1time
        mytime <- jmvcore::constructFormula(terms = mytime)

        myoutcome <- results$name2outcome
        myoutcome <-
          jmvcore::constructFormula(terms = myoutcome)


        myfactor <- results$name3explanatory
        myfactor <-
        jmvcore::constructFormula(terms = myfactor)

        plotData <- results$cleanData

        plotData[[mytime]] <-
          jmvcore::toNumeric(plotData[[mytime]])

        # Unqualified `Surv` (globally allow-listed); mytime/myoutcome already
        # backtick-escaped via jmvcore::constructFormula above.
        myformula <-
          paste0("Surv(", mytime, ", ", myoutcome, ")")

        private$.checkpoint()

        plot2 <- plotData %>%
          finalfit::surv_plot(
            .data = .,
            dependent = myformula,
            explanatory = myfactor,
            xlab = paste0('Time (', self$options$timetypeoutput, ')'),
            # pval = self$options$pplot,
            # pval.method	= self$options$pplot,
            legend = 'none',
            break.time.by = self$options$byplot,
            xlim = c(0, self$options$endplot),
            ylim = c(
              self$options$ybegin_plot,
              self$options$yend_plot),
            # fun = "event" draws 1 - S(t): the cumulative PROBABILITY of the
            # event, not a running count. The probability is the right quantity
            # -- a raw count ignores censoring and is not comparable across
            # cohorts -- so the labels and the option text were corrected to
            # match the plot rather than the plot changed to match the labels.
            ylab = .("Cumulative event probability"),
            title = .("Cumulative Event Probability of the Whole Group"),
            fun = "event",
            risk.table = self$options$risktable,
            conf.int = self$options$ci95,
            censor = self$options$censored,
            surv.median.line = self$options$medianline
          )

        if (inherits(plot2, "ggsurvplot")) {
          plot2$plot <- plot2$plot + ggtheme
          print(plot2)
        } else {
          print(plot2 + ggtheme)
        }
        TRUE

      }



      # Cumulative Hazard ----
      ,
      .plot3 = function(image3, ggtheme, theme, ...) {
        # Competing-risk mode codes the outcome 0/1/2. survival::Surv() does
        # not reject that -- it warns and remaps 1 to censored, 2 to event and
        # 0 to NA, so this plot would render inverted with no visible warning.
        # Ask .isCompetingRisk(), not the options: the outcomeorganizer hand-off
        # delivers a 0/1/2 status with multievent = FALSE, and the option test
        # alone let exactly that case through to be plotted backwards.
        if (private$.isCompetingRisk(image3$state))
          return(private$.competingRiskPlotRefusal(.("The cumulative hazard plot")))

        ch <- self$options$ch

        if (!ch)
          return()

        results <- image3$state

        if (is.null(results)) {
          return()
        }

        # Validate plot parameters
        if (!private$.validatePlotParameters(check_y = FALSE)) return()

        mytime <- results$name1time
        mytime <- jmvcore::constructFormula(terms = mytime)

        myoutcome <- results$name2outcome
        myoutcome <-
          jmvcore::constructFormula(terms = myoutcome)


        myfactor <- results$name3explanatory
        myfactor <-
        jmvcore::constructFormula(terms = myfactor)

        plotData <- results$cleanData

        plotData[[mytime]] <-
          jmvcore::toNumeric(plotData[[mytime]])

        # Unqualified `Surv` (globally allow-listed); mytime/myoutcome already
        # backtick-escaped via jmvcore::constructFormula above.
        myformula <-
          paste0("Surv(", mytime, ", ", myoutcome, ")")

        private$.checkpoint()

        plot3 <- plotData %>%
          finalfit::surv_plot(
            .data = .,
            dependent = myformula,
            explanatory = myfactor,
            xlab = paste0('Time (', self$options$timetypeoutput, ')'),
            ylab = "Cumulative Hazard",
            # pval = self$options$pplot,
            # pval.method	= self$options$pplot,
            legend = 'none',
            break.time.by = self$options$byplot,
            xlim = c(0, self$options$endplot),
            # For cumulative hazard, use NULL to allow auto-scaling beyond 1.0
            ylim = NULL,
            title = .("Cumulative Hazard of the Whole Group"),
            fun = "cumhaz",
            risk.table = self$options$risktable,
            conf.int = self$options$ci95,
            censor = self$options$censored,
            # survminer refuses median lines when fun = "cumhaz". Add the
            # correctly transformed reference lines below instead: S(t)=0.5
            # corresponds to -log(S(t))=log(2).
            surv.median.line = "none"
          )

        if (inherits(plot3, "ggsurvplot")) {
          plot_obj <- plot3$plot + ggtheme
          median_formula <- jmvcore::asFormula(
            paste0(myformula, " ~ ", myfactor),
            additional_allowed_functions = c("Surv"))
          median_time <- suppressWarnings(
            as.numeric(summary(survival::survfit(median_formula, data = plotData))
                       $table[["median"]]))
          if (length(median_time) > 0 && is.finite(median_time)) {
            if (self$options$medianline %in% c("h", "hv"))
              plot_obj <- plot_obj + ggplot2::geom_hline(
                yintercept = log(2), linetype = "dashed", color = "grey40")
            if (self$options$medianline %in% c("v", "hv"))
              plot_obj <- plot_obj + ggplot2::geom_vline(
                xintercept = median_time, linetype = "dashed", color = "grey40")
          }
          plot3$plot <- plot_obj
          print(plot3)
        } else {
          print(plot3 + ggtheme)
        }
        TRUE
      }


      # KMunicate Style ----
      ,
      .plot6 = function(image6, ggtheme, theme, ...) {
        # Competing-risk mode codes the outcome 0/1/2. survival::Surv() does
        # not reject that -- it warns and remaps 1 to censored, 2 to event and
        # 0 to NA, so this plot would render inverted with no visible warning.
        # Ask .isCompetingRisk(), not the options: the outcomeorganizer hand-off
        # delivers a 0/1/2 status with multievent = FALSE, and the option test
        # alone let exactly that case through to be plotted backwards.
        if (private$.isCompetingRisk(image6$state))
          return(private$.competingRiskPlotRefusal(.("The KMunicate plot")))

        kmunicate <- self$options$kmunicate

        if (!kmunicate)
          return()

        # This renderer builds seq(0, endplot, by = byplot) and was the only one
        # that never validated those options: byplot = 0 aborted it with R's
        # "invalid '(to - from)/by' in seq()".
        if (!private$.validatePlotParameters(check_y = FALSE)) return()

        results <- image6$state

        if (is.null(results)) {
          return()
        }

        mytime <- results$name1time
        mytime <- jmvcore::constructFormula(terms = mytime)

        myoutcome <- results$name2outcome
        myoutcome <-
          jmvcore::constructFormula(terms = myoutcome)


        myfactor <- results$name3explanatory
        myfactor <-
          jmvcore::constructFormula(terms = myfactor)

        plotData <- results$cleanData

        plotData[[mytime]] <-
          jmvcore::toNumeric(plotData[[mytime]])


        # mytime/myoutcome/myfactor are already backtick-escaped via
        # jmvcore::constructFormula above. Switch to unqualified `Surv`
        # (allow-listed) and asFormula for parse-tree validation.
        myformula <-
          paste0('Surv(', mytime, ', ', myoutcome, ') ~ ', myfactor)

        myformula <- jmvcore::asFormula(myformula, additional_allowed_functions = c("Surv"))

        km_fit <-
          survival::survfit(myformula, data = plotData)

        time_scale <-
          seq(0, self$options$endplot, by = self$options$byplot)

        private$.checkpoint()

        estimand_meta <- private$.estimandMeta(results)
        plot6 <-
          KMunicate::KMunicate(
            fit = km_fit,
            time_scale = time_scale,
            .risk_table = if (isTRUE(self$options$risktable)) "KMunicate" else NULL,
            .theme = ggtheme,
            .xlab = paste0('Time in ', self$options$timetypeoutput),
            .title = estimand_meta$curve,
            # KMunicate always constructs the pointwise interval ribbon; alpha
            # zero makes the separate CI option behave as advertised.
            .alpha = if (isTRUE(self$options$ci95)) 0.25 else 0
          )


        print(plot6)
        TRUE

      },

      # Baseline Hazard Plot Function ----
      .baselineHazardPlot = function(image, ggtheme, theme, ...) {
        # Competing-risk mode codes the outcome 0/1/2. survival::Surv() does
        # not reject that -- it warns and remaps 1 to censored, 2 to event and
        # 0 to NA, so this plot would render inverted with no visible warning.
        # Ask .isCompetingRisk(), not the options: the outcomeorganizer hand-off
        # delivers a 0/1/2 status with multievent = FALSE, and the option test
        # alone let exactly that case through to be plotted backwards.
        if (private$.isCompetingRisk(image$state))
          return(private$.competingRiskPlotRefusal(.("This hazard plot")))

        if (!self$options$baseline_hazard)
          return()

        # Get the analysis results from image state (like other plot functions)
        results <- image$state
        
        if (is.null(results)) {
          return()
        }

        # Extract data like other plot functions do
        mytime <- results$name1time
        myoutcome <- results$name2outcome
        plotData <- results$cleanData

        if (is.null(plotData) || nrow(plotData) == 0) {
          return()
        }

        plotData[[mytime]] <- jmvcore::toNumeric(plotData[[mytime]])

        result <- private$.safeExecute({
          hz <- private$.hazardIntervals(plotData[[mytime]], plotData[[myoutcome]])
          hz <- hz[is.finite(hz$rate), , drop = FALSE]
          if (nrow(hz) == 0) return(NULL)

          # geom_rect makes the interval support explicit. A step located only
          # at event times visually implies pointwise instantaneous estimates,
          # which the data do not identify.
          plot <- ggplot2::ggplot(hz) +
            ggplot2::geom_rect(
              ggplot2::aes(xmin = start, xmax = end, ymin = 0, ymax = rate),
              fill = "#0173B2", alpha = 0.28, color = "#0173B2") +
            ggplot2::geom_point(
              ggplot2::aes(x = (start + end) / 2, y = rate),
              color = "#0173B2", size = 2) +
            ggplot2::labs(
              title = .("Piecewise Hazard-Rate Estimates"),
              subtitle = .("Equal-width intervals; events divided by exact person-time"),
              x = paste0("Time (", self$options$timetypeoutput, ")"),
              y = paste0("Events per person-", self$options$timetypeoutput)) +
            ggplot2::theme_minimal()
          if (!missing(ggtheme) && !is.null(ggtheme))
            plot <- plot + ggtheme

          print(plot)
          TRUE
        }, context = "baseline_hazard")
        
        if (is.null(result)) {
          return()
        }
      },

      # Smoothed Hazard Plot Function ----
      .smoothedHazardPlot = function(image, ggtheme, theme, ...) {
        # Competing-risk mode codes the outcome 0/1/2. survival::Surv() does
        # not reject that -- it warns and remaps 1 to censored, 2 to event and
        # 0 to NA, so this plot would render inverted with no visible warning.
        # Ask .isCompetingRisk(), not the options: the outcomeorganizer hand-off
        # delivers a 0/1/2 status with multievent = FALSE, and the option test
        # alone let exactly that case through to be plotted backwards.
        if (private$.isCompetingRisk(image$state))
          return(private$.competingRiskPlotRefusal(.("This hazard plot")))

        if (!self$options$hazard_smoothing)
          return()

        # Get the analysis results from image state (like other plot functions)
        results <- image$state
        
        if (is.null(results)) {
          return()
        }
        
        # Extract data like other plot functions do
        mytime <- results$name1time
        myoutcome <- results$name2outcome
        plotData <- results$cleanData

        if (is.null(plotData) || nrow(plotData) == 0) {
          return()
        }

        plotData[[mytime]] <- jmvcore::toNumeric(plotData[[mytime]])

        # A continuous smoothed hazard is not identified by an all-censored
        # cohort, and an event mass at the time origin cannot be represented by
        # a finite continuous hazard. Draw the reason instead of a misleading
        # flat-zero curve or a blank panel.
        hazard_refusal <- function(message) {
          p <- ggplot2::ggplot() +
            ggplot2::annotate(
              "text", x = 0, y = 0, hjust = 0.5, vjust = 0.5,
              size = 4, lineheight = 1.2,
              label = paste(strwrap(message, width = 62), collapse = "\n")) +
            ggplot2::theme_void()
          print(p)
          TRUE
        }
        event_mask <- plotData[[myoutcome]] == 1
        if (!any(event_mask, na.rm = TRUE))
          return(hazard_refusal(
            .("A smoothed hazard was not estimated because no events were observed. A flat zero line would be a boundary point estimate, not evidence that the population hazard is exactly zero.")))
        if (any(event_mask & plotData[[mytime]] == 0, na.rm = TRUE))
          return(hazard_refusal(
            .("A smoothed continuous hazard was not estimated because one or more events occurred at time zero. Such events create a probability mass at the origin rather than a finite continuous hazard; use the survival or cumulative-incidence output to report them.")))

        result <- private$.safeExecute({
          # Create survival object
          surv_obj <- survival::Surv(time = plotData[[mytime]], event = plotData[[myoutcome]])

          # Per-interval occurrence/exposure rate, smoothed with PERSON-TIME
          # WEIGHTS -- the same estimator as baselineHazardTable (see the block
          # around line 2270), not a second, incompatible one.
          #
          # What was here differenced basehaz() and fed the resulting points to
          # an UNWEIGHTED loess. Each point d_i/(n_i*dt_i) is a rate estimated
          # from typically one event, and for continuous times n_i*dt_i is
          # roughly exponential, so 1/(n_i*dt_i) is reciprocal-exponential: it
          # has NO finite mean. loess fits a local MEAN, so the curve was not a
          # noisy estimate of the hazard, it had no target at all -- simulating
          # exponential(0.1) with n from 100 to 6400 the sample mean wandered
          # between 0.68 and 1.82 and never converged (the median converges
          # correctly to lambda/log 2). On the bundled example data the plotted
          # level was wrong by roughly an order of magnitude, so no caption
          # calling it "exploratory" would have made it safe to read.
          #
          # Weighting each point by its person-time makes the local fit a local
          # sum(d)/sum(n*dt) -- the standard kernel hazard estimator. The
          # event-free intervals must therefore be KEPT: dropping them (the old
          # `inst_hazard > 0` filter) throws away exactly the exposure that makes
          # a rate a rate, and biases the curve upward.
          hz <- private$.hazardIntervals(plotData[[mytime]], plotData[[myoutcome]])
          hz <- hz[is.finite(hz$rate) & hz$person_time > 0, , drop = FALSE]
          if (nrow(hz) < 3)
            return(hazard_refusal(sprintf(
              paste0(
                "A smoothed hazard was not estimated because automatic binning ",
                "produced %d usable interval(s). At least three interval-rate ",
                "estimates are needed for this exploratory smoother."),
              nrow(hz))))

          smooth_data <- data.frame(
            time    = (hz$start + hz$end) / 2,
            hazard  = hz$rate,
            w       = hz$person_time
          )

          if (nrow(smooth_data) >= 3) {
            # Use improved adaptive smoothing algorithm
            n_points <- nrow(smooth_data)
            adaptive_span <- private$.calculateAdaptiveSpan(n_points)

            # A weighted local-constant fit is exactly the local
            # sum(events)/sum(person-time) estimator described above. A
            # degree-1 fit is a local linear regression and can be negative;
            # clipping its predictions at zero changed its estimand and hid the
            # mathematical inconsistency.
            smooth_fit <- stats::loess(hazard ~ time, data = smooth_data,
                                      span = adaptive_span, degree = 0,
                                      weights = smooth_data$w)

            # Predict smoothed values
            time_seq <- seq(min(smooth_data$time), max(smooth_data$time), length.out = 100)
            smooth_hazard <- stats::predict(smooth_fit, newdata = data.frame(time = time_seq))
            
            # Create plot data
            plot_data <- data.frame(
              time = time_seq,
              hazard = smooth_hazard
            )
            
            # Create the smoothed hazard plot with both smooth line and original points
            plot <- ggplot2::ggplot(plot_data, ggplot2::aes(x = time, y = hazard)) +
              ggplot2::geom_line(color = "#DE8F05", linewidth = 1.2) +
              # Add original hazard points as reference
              ggplot2::geom_point(data = smooth_data, 
                                 ggplot2::aes(x = time, y = hazard),
                                 color = "#DE8F05", alpha = 0.4, size = 1) +
              ggplot2::labs(
                title = .("Smoothed Hazard Function"),
                subtitle = jmvcore::format(
                  .("Person-time weighted local-constant LOESS, span = {span}. Each point is one interval's events / person-time at risk."),
                  span = round(adaptive_span, 2)
                ),
                x = paste0("Time (", self$options$timetypeoutput, ")"),
                y = "Smoothed Hazard Rate"
              ) +
              ggplot2::theme_minimal()
            if (!missing(ggtheme) && !is.null(ggtheme))
              plot <- plot + ggtheme

            print(plot)
            return(TRUE)
          } else {
            return(NULL)
          }
        }, context = "baseline_hazard")
        
        if (is.null(result)) {
          return()
        }
      },

      # Data Quality Assessment Function ----
      .populateDataQuality = function(results) {
        if (!self$options$advancedDiagnostics) {
          return()
        }

        dq <- results$data_quality
        if (is.null(dq)) {
          return()
        }

        # Populate data quality table
        quality_table <- self$results$dataQualityTable
        
        # Report measurements without categorical grades. Cutoffs such as 30
        # events or 95% completeness are not validated universal thresholds for
        # a survival analysis; adequacy depends on the estimand, target time,
        # risk set, censoring, missingness mechanism, and intended decision.
        not_graded <- .("not graded")
        quality_table$setNote(
          "grading",
          .("Automated adequacy grades are not assigned. Interpret event counts, completeness, follow-up, confidence intervals, and time-specific risk sets in the context of the endpoint and intended use."))

        # Add rows to table
        quality_table$addRow(rowKey = 1, values = list(
          metric = .("Sample Size"),
          value = paste(dq$n_total, .("subjects")),
          assessment = not_graded
        ))

        quality_table$addRow(rowKey = 2, values = list(
          metric = .("Number of Events"),
          value = paste(dq$n_events, .("events")),
          assessment = not_graded
        ))

        # Event rate: reported, not graded. "20% or more = Good" said that a
        # cohort in which more patients had died was better data.
        quality_table$addRow(rowKey = 3, values = list(
          metric = .("Observed Event Proportion"),
          value = paste0(dq$event_rate, "%"),
          assessment = not_graded
        ))

        # The observed RANGE, reported ungraded. This row used to be graded on
        # max(time), so one subject followed for six years made a series with
        # six months of median follow-up read "Long-term" -- the grade was
        # driven by the single most extreme observation in the data set. The
        # grade now lives on the median follow-up row below, where a robust
        # summary of the observation window belongs.
        quality_table$addRow(rowKey = 4, values = list(
          metric = .("Follow-up Range (min-max)"),
          value = paste0(dq$min_time, "-", dq$max_time, " ", self$options$timetypeoutput),
          assessment = not_graded
        ))

        quality_table$addRow(rowKey = 5, values = list(
          metric = if (isTRUE(dq$median_followup_reverse_km))
            .("Median Follow-up (reverse KM)") else
            .("Median Observed Time (reverse KM not estimable)"),
          value = paste(dq$median_followup, self$options$timetypeoutput),
          assessment = not_graded
        ))

        # Memory footprint: a note about this machine, not a property of the
        # data, so it is reported without a grade like the other two ungraded
        # rows. It also called plain format(), which inside this package is
        # jmvcore::format(str, ..., context) because the NAMESPACE imports all
        # of jmvcore -- so format.object_size never ran, the cell received a raw
        # object_size, and the old grader read its BYTE count as megabytes and
        # labelled a 2 KB data set "Large".
        quality_table$addRow(rowKey = 6, values = list(
          metric = .("Dataset Memory Usage"),
          value = base::format(utils::object.size(results$cleanData), units = "auto"),
          assessment = not_graded
        ))
        
        # Data completeness, measured on the RAW dataset.
        #
        # This used to count NAs in results$cleanData -- the frame that
        # complete-case filtering and jmvcore::naOmit() have already emptied of
        # every incomplete row. The answer was therefore 100% by construction:
        # a 4-row probe with a missing time and a missing outcome reported both
        # variables "100% Excellent". Completeness is a property of the data
        # the user supplied, so it has to be read before the exclusions.
        raw <- self$data
        n_raw <- nrow(raw)
        if (n_raw > 0) {
          time_src <- if (self$options$tint)
            c(self$options$dxdate, self$options$fudate) else self$options$elapsedtime
          time_src <- time_src[!is.na(time_src) & time_src %in% names(raw)]

          pct_complete <- function(cols) {
            if (length(cols) == 0) return(NA_real_)
            sum(stats::complete.cases(raw[, cols, drop = FALSE])) / n_raw * 100
          }

          time_complete <- pct_complete(time_src)
          outcome_complete <- pct_complete(
            if (!is.null(self$options$outcome) && self$options$outcome %in% names(raw))
              self$options$outcome else character(0))

          fmt <- function(pct) if (is.na(pct)) .("n/a") else paste0(round(pct, 1), "%")

          quality_table$addRow(rowKey = 7, values = list(
            metric = .("Time Variable Completeness (before exclusions)"),
            value = fmt(time_complete),
            assessment = not_graded
          ))

          quality_table$addRow(rowKey = 8, values = list(
            metric = .("Outcome Variable Completeness (before exclusions)"),
            value = fmt(outcome_complete),
            assessment = not_graded
          ))
        }

        # Generate data quality summary
        if (self$options$showSummaries) {
          warning_text <- if (length(dq$warnings) > 0) {
            paste("<div style='background-color: rgba(255, 202, 33, 0.23); padding: 10px; border-radius: 5px; margin: 10px 0; border-left: 4px solid #ffc107; color: inherit;'>",
                  "<strong> Data Quality Considerations:</strong><ul>",
                  paste0("<li>", dq$warnings, "</li>", collapse = ""),
                  "</ul></div>")
          } else {
            paste0("<div style='background-color: rgba(33, 152, 239, 0.13);padding:10px;border-radius:5px;margin:10px 0;border-left:4px solid #2196f3; color: inherit;'>",
                   "<strong>Automated grading:</strong> No universal adequacy grade is assigned. Review the event counts, risk sets, confidence intervals, follow-up, missingness, and endpoint context directly.</div>")
          }

          count_text <- if (dq$n_competing > 0)
            paste0(dq$n_events, " event(s) of interest, ", dq$n_competing,
                   " competing event(s), and ", dq$n_censored, " censored") else
            paste0(dq$n_events, " event(s) and ", dq$n_censored, " censored")

          summary_html <- paste0(
            "<div style='background-color: rgba(138, 155, 172, 0.06); padding: 15px; border-radius: 8px; margin: 10px 0; color: inherit;'>",
            "<h4 style='color: #2c3e50; margin-top: 0;'> Descriptive Data Diagnostics</h4>",
            "<p>This analysis includes <strong>", dq$n_total, " subjects</strong>: <strong>",
            count_text, "</strong> (", dq$event_rate, "% observed target-event proportion) over an observed-time range of ",
            dq$min_time, " to ", dq$max_time, " ", self$options$timetypeoutput, ".</p>",
            warning_text,
            "<div style='background-color: rgba(33, 152, 239, 0.13); padding: 10px; border-radius: 5px; margin: 10px 0; color: inherit;'>",
            "<strong>Interpretive limitations:</strong>",
            "<ul style='margin: 5px 0; padding-left: 20px;'>",
            "<li>Rows missing time or outcome are excluded by complete-case analysis; bias depends on why values are missing.</li>",
            "<li>Independent/non-informative censoring cannot be verified from these summaries.</li>",
            "<li>Precision at a specific time depends on events and the number still at risk there, not sample size alone.</li>",
            "<li>This is a descriptive single-cohort analysis and does not estimate a treatment effect or establish individual prognosis.</li>",
            "</ul></div>",
            "</div>"
          )
          
          self$results$dataQualitySummary$setContent(summary_html)
        }
      },

      # Clinical Summary Generation ----
      .generateClinicalSummary = function(results) {
        if (self$results$medianTable$rowCount == 0) {
          return()  # Exit if median table hasn't been populated yet
        }

        # Extract key survival metrics directly from the median table data
        tryCatch({
          # Get data from the median table - we know this has been populated since rowCount > 0
          # The table structure is: records, events, rmean, se_rmean, median, x0_95lcl, x0_95ucl

          # Access the first (and only) row of data directly from the table
          median_table_row <- 1  # First row
          estimand_meta <- private$.estimandMeta()

          n_total <- self$results$medianTable$getCell(rowNo = median_table_row, "records")$value
          n_events <- self$results$medianTable$getCell(rowNo = median_table_row, "events")$value
          median_survival <- self$results$medianTable$getCell(rowNo = median_table_row, "median")$value
          ci_lower <- self$results$medianTable$getCell(rowNo = median_table_row, "x0_95lcl")$value
          ci_upper <- self$results$medianTable$getCell(rowNo = median_table_row, "x0_95ucl")$value
          
          # Get event rate
          event_rate <- round((n_events / n_total) * 100, 1)
          n_competing <- if (!is.null(results$data_quality$n_competing))
            results$data_quality$n_competing else 0
          n_censored <- if (!is.null(results$data_quality$n_censored))
            results$data_quality$n_censored else n_total - n_events - n_competing
          
          time_unit <- self$options$timetypeoutput

          # NO PROGNOSIS VERDICT.
          #
          # This block used to label the cohort "favorable" / "moderate" /
          # "concerning" by comparing the median against 60 and 24 -- month
          # numbers applied unchanged to days, weeks and years, so 3 years of
          # follow-up graded "concerning" and 30 days graded "favorable". Worse,
          # a median of NA was reported as "favorable (median not reached)",
          # which is precisely backwards: a median is not reached when there is
          # too little follow-up or too few events just as readily as when
          # patients do well, so a cohort with ZERO events and two months of
          # follow-up produced copy-ready prose calling its prognosis
          # favorable. This output is labelled copy-ready for clinical reports,
          # which makes a fabricated prognostic claim the most dangerous thing
          # in the file. Single-arm descriptive statistics support description,
          # not prognosis; the summary now reports what was observed and says
          # what it does not establish.
          max_followup <- if (!is.null(results$data_quality)) results$data_quality$max_time else NA_real_

          preset_context <- .("This single-arm survival analysis")

          summary_parts <- c()

          if (!is.na(median_survival)) {
            # The CI clause is built separately because the bounds can legitimately
            # be absent. In competing-risk mode .medianSurv() writes NA to
            # x0_95lcl/x0_95ucl on purpose (no valid interval exists for a CIF
            # quantile without inverting a confidence band), and sprintf("%.1f", NA)
            # renders the literal string "NA" -- so this block, labelled
            # "Copy-ready for clinical reports", was emitting "95% CI: NA-NA".
            # The noun matters too: the competing-risk median is the time at which
            # the cumulative incidence of the event of interest reaches 50%, which
            # is not median survival.
            median_label <- if (private$.isCompetingRisk())
              .("Median time to event of interest was %.1f %s") else
              paste0(estimand_meta$median, " was %.1f %s")
            median_text <- sprintf(median_label, median_survival, time_unit)
            if (is.finite(ci_lower) && is.finite(ci_upper))
              median_text <- paste0(median_text, " ", sprintf(
                .("(95%% CI: %.1f-%.1f %s)"), ci_lower, ci_upper, time_unit))
          } else if (n_events == 0) {
            # Same estimand distinction as above: with competing risks there is
            # no median survival to be estimated in the first place.
            no_median_label <- if (private$.isCompetingRisk())
              .("The median time to the event of interest could not be estimated: no event of interest was observed (longest follow-up %.1f %s)") else
              paste0(estimand_meta$median, " could not be estimated: no events were observed (longest follow-up %.1f %s)")
            median_text <- sprintf(no_median_label, max_followup, time_unit)
          } else if (private$.isCompetingRisk()) {
            median_text <- sprintf(
              .("The median time to the event of interest was not reached: the cumulative incidence of the event of interest stayed below 50%% within the observed follow-up (longest follow-up %.1f %s). With competing risks this is expected whenever the competing event is frequent, since the cumulative incidence can plateau below 50%% however long follow-up continues; it is not a statement that outcomes were good"),
              max_followup, time_unit
            )
          } else {
            median_text <- sprintf(
              paste0(estimand_meta$median, " was not reached: the Kaplan-Meier curve stayed above 50%% within the observed follow-up (longest follow-up %.1f %s). This can reflect the event process, short follow-up, or censoring, so no conclusion about outcome should be drawn from its absence"),
              max_followup, time_unit
            )
          }

          summary_parts <- c(summary_parts, median_text)

          event_text <- if (private$.isCompetingRisk()) sprintf(
            .("with %d target event(s), %d competing event(s), and %d censored observation(s) among %d subjects (%.1f%% crude target-event proportion)."),
            n_events, n_competing, n_censored, n_total, event_rate
          ) else sprintf(
            .("with %d event(s) and %d censored observation(s) among %d subjects (%.1f%% crude event proportion)."),
            n_events, n_censored, n_total, event_rate)

          summary_parts <- c(summary_parts, event_text)

          summary_parts <- c(summary_parts,
            .("These are descriptive estimates for one cohort with no comparison group; they describe what was observed here and do not establish prognosis for an individual patient or the effect of any treatment."))

          # No universal event-count cutoff separates an adequate descriptive
          # Kaplan-Meier/CIF analysis from an inadequate one. Precision is
          # exposed through confidence intervals and time-specific risk sets.
          recommendations <- c(
            .("Report these estimates alongside the number at risk and the follow-up duration; comparison with an external cohort is not a treatment-effect estimate."))

          # Format the complete summary
          summary_html <- paste0(
            "<div style='background-color: rgba(138, 155, 172, 0.06); padding: 20px; border-radius: 8px; margin: 15px 0; border-left: 4px solid #007bff; color: inherit;'>",
            "<h4 style='color: #2c3e50; margin-top: 0; margin-bottom: 15px;'> ", .("Descriptive Cohort Summary"), "</h4>",
            "<p style='margin-bottom: 15px; font-size: 16px; line-height: 1.6;'>",
            "<strong>", preset_context, ":</strong> ", paste(summary_parts, collapse = " "), "</p>"
          )
          
          if (length(recommendations) > 0) {
            summary_html <- paste0(summary_html,
              "<div style='background-color: rgba(33, 152, 239, 0.13); padding: 15px; border-radius: 5px; margin-top: 15px; color: inherit;'>",
              "<h5 style='color: #1976d2; margin-top: 0; margin-bottom: 10px;'> ", .("Clinical Considerations"), "</h5>",
              "<ul style='margin: 0; padding-left: 20px;'>",
              paste0("<li>", recommendations, "</li>", collapse = ""),
              "</ul></div>"
            )
          }
          
          # Add copy button functionality
          summary_html <- paste0(summary_html,
            "<div style='text-align: right; margin-top: 15px;'>",
            "<small style='color: #6c757d;'>", .("Copy-ready descriptive cohort summary"), "</small>",
            "</div></div>"
          )
          
          self$results$clinicalSummary$setContent(summary_html)
          
        }, error = function(e) {
          # Do not expose internal object details in a clinical-facing result.
          # The primary tables remain available even if this optional narrative
          # cannot be assembled.
          fallback_html <- paste0(
            "<div style='background-color: rgba(255, 202, 33, 0.23); padding: 15px; border-radius: 5px; margin: 10px 0; color: inherit;'>",
            "<p>", .("The optional descriptive cohort summary could not be generated. The numerical tables above remain the authoritative results."), "</p>",
            "</div>"
          )
          self$results$clinicalSummary$setContent(fallback_html)
        })
      }


    )
  )
