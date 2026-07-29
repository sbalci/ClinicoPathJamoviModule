# ============================================================================
# CLINICOPATH SURVIVAL UTILITY FUNCTIONS
# ============================================================================
# Shared helper functions specifically for survival and time-to-event analyses.

#' Build a survival formula safely via jmvcore::asFormula
#'
#' Wraps `jmvcore::asFormula` with the function allow-list extended to cover
#' common survival modelling helpers (`Surv`, `strata`, `cluster`, `frailty`,
#' `tt`, `pspline`, `ns`, `bs`, `I`, `const`, `finegray`). Use this instead of
#' base `stats::as.formula()` in survival / Cox / Fine-Gray paths so the formula
#' goes through jmvcore's allow-listed parser.
#'
#' The returned formula's environment is set to the CALLER's frame. `asFormula`
#' otherwise leaves it pointing at its own evaluation frame, which does not hold
#' the caller's `mydata`. Model fitting still works, because `coxph(fml, data=)`
#' is handed the data directly — but any method that later re-evaluates the
#' model call does not get it. `cox.zph()` is the visible casualty: ticking
#' "Proportional hazards assumption" aborted the entire analysis with
#' "object 'mydata' not found", taking every other result down with it.
#'
#' @param x A character formula string (e.g. `"survival::Surv(t, d) ~ x"`).
#' @param env Environment to attach to the returned formula. Defaults to the
#'   calling frame, which is what downstream re-evaluation needs.
#' @return A parsed formula object.
#' @keywords internal
.asSurvivalFormula <- function(x, env = parent.frame()) {
    fml <- jmvcore::asFormula(
        x,
        additional_allowed_functions = c(
            "Surv", "strata", "cluster", "frailty", "tt",
            "pspline", "ns", "bs", "I", "const", "finegray"
        )
    )
    if (inherits(fml, "formula")) environment(fml) <- env
    fml
}

#' Build the survival event indicator from a raw outcome column
#'
#' Single source of truth for turning a user's outcome variable into the 0/1
#' (or 0/1/2 competing-risk) status vector consumed by `survival::Surv()`.
#' Replaces five near-identical `.definemyoutcome()` blocks that had drifted
#' apart in their validation.
#'
#' Semantics, stated explicitly because they are easy to get wrong:
#' \itemize{
#'   \item On the single-event-level path every level that is *not* the selected
#'     event level becomes 0 (censored). With three or more levels this is
#'     cause-specific censoring: Cox hazard ratios stay valid, but Kaplan-Meier
#'     survival, median survival and x-year survival are biased upward. The
#'     caller is expected to surface `estimand` and `censored_labels` to the user.
#'   \item `NA` is never converted to a value. It stays `NA` and the row is
#'     dropped downstream by `jmvcore::naOmit()` (complete-case analysis).
#'     Coding it 0 would fabricate censoring.
#'   \item In `multievent` mode every observed level must be assigned to one of
#'     the four categories. An unmapped level is an error, never a silent `NA`
#'     that later gets deleted along with the patient.
#' }
#'
#' @param outcome The raw outcome vector (factor, character, numeric or logical).
#' @param outcomeLevel The level representing the event, for the single-event
#'   path. Honoured for numeric outcomes too, not only factors.
#' @param multievent Logical. Use the four-category mapping.
#' @param analysistype One of `"overall"`, `"cause"`, `"dfs"`, `"compete"`.
#' @param dod,dooc,awd,awod Level labels for Dead of Disease, Dead of Other
#'   Causes, Alive with Disease, Alive without Disease.
#' @param outcome_name Display name of the outcome variable, used in messages.
#' @return A list with `status` (0/1 or 0/1/2, `NA` preserved), `status_factor`
#'   (Censored/Event/Competing, only when competing), the counts `n_event`,
#'   `n_censored`, `n_competing`, `n_missing`, the labels `event_label` and
#'   `censored_labels`, `n_levels`, `has_competing`, `estimand`, and `error`
#'   (`NULL`, or a ready-to-display message the caller should reject with).
#' @keywords internal
.defineEventIndicator <- function(outcome,
                                  outcomeLevel = NULL,
                                  multievent = FALSE,
                                  analysistype = "overall",
                                  dod = NULL, dooc = NULL, awd = NULL, awod = NULL,
                                  outcome_name = "outcome") {

    fail <- function(msg) list(status = NULL, error = msg)

    ok <- function(status, event_label, censored_labels, estimand,
                   n_levels, status_factor = NULL) {
        list(
            status          = status,
            status_factor   = status_factor,
            n_event         = sum(status == 1, na.rm = TRUE),
            n_censored      = sum(status == 0, na.rm = TRUE),
            n_competing     = sum(status == 2, na.rm = TRUE),
            n_missing       = sum(is.na(status)),
            event_label     = event_label,
            censored_labels = censored_labels,
            n_levels        = n_levels,
            has_competing   = any(status == 2, na.rm = TRUE),
            estimand        = estimand,
            error           = NULL
        )
    }

    if (is.null(outcome) || length(outcome) == 0)
        return(fail("No outcome variable available. Please select an outcome variable."))

    # Ordered factors compare awkwardly against a plain level string.
    if (is.ordered(outcome)) outcome <- factor(outcome, ordered = FALSE)

    observed <- unique(outcome[!is.na(outcome)])
    observed_chr <- as.character(observed)

    if (length(observed) == 0)
        return(fail(sprintf(
            "Outcome variable '%s' contains only missing values.", outcome_name)))

    # ---- multievent: four-category mapping -------------------------------
    if (isTRUE(multievent)) {

        buckets <- list(dod = dod, dooc = dooc, awd = awd, awod = awod)
        titles  <- c(dod  = "Dead of Disease",   dooc = "Dead of Other Causes",
                     awd  = "Alive with Disease", awod = "Alive without Disease")
        filled  <- vapply(buckets, function(b) !is.null(b) && length(b) > 0, logical(1))

        if (!any(filled))
            return(fail(paste0(
                "Multiple Event Levels is enabled but no outcome levels have been assigned. ",
                "Assign each level of '", outcome_name, "' to one of: ",
                paste(titles, collapse = ", "), ".")))

        selected <- unlist(buckets[filled], use.names = FALSE)

        # Distinguish "you reused a level" from "you left one blank" -- these
        # used to share one misleading message.
        dupes <- unique(selected[duplicated(selected)])
        if (length(dupes) > 0)
            return(fail(paste0(
                "Each outcome level may be assigned to only one category. ",
                "Assigned to more than one: ", paste(dupes, collapse = ", "), ".")))

        # Note the asymmetry with the single-event path below: a category with no
        # patients (nobody Alive with Disease in this cohort) is perfectly normal
        # and must not error. The resulting count shows up in the disclosure block.
        # It is the other direction -- a level in the data with no category --
        # that is a real problem.
        #
        # Every observed level must be assigned. Leaving one out used to make it
        # NA, and naOmit then deleted those patients -- silently shrinking the
        # denominator and inflating the event rate.
        unmapped <- setdiff(observed_chr, as.character(selected))
        if (length(unmapped) > 0)
            return(fail(paste0(
                "Outcome level(s) not assigned to any category: ",
                paste(unmapped, collapse = ", "),
                ". Assign every level of '", outcome_name, "' to one of: ",
                paste(titles[!filled], collapse = ", "),
                ". Unassigned levels would otherwise be dropped from the analysis.")))

        # Codes per analysis type. "dfs" is the only one in which Alive with
        # Disease counts as an event -- which is what disease-free survival means.
        codes <- switch(analysistype,
            overall = c(awd = 0, awod = 0, dod = 1, dooc = 1),
            cause   = c(awd = 0, awod = 0, dod = 1, dooc = 0),
            dfs     = c(awd = 1, awod = 0, dod = 1, dooc = 1),
            compete = c(awd = 0, awod = 0, dod = 1, dooc = 2),
            NULL)

        if (is.null(codes))
            return(fail(paste0("Unknown survival type '", analysistype, "'.")))

        status <- rep(NA_integer_, length(outcome))
        for (b in names(buckets)[filled])
            status[!is.na(outcome) & as.character(outcome) == as.character(buckets[[b]])] <-
                as.integer(codes[[b]])

        estimand <- switch(analysistype,
            overall = "overall survival",
            cause   = "cause-specific survival",
            dfs     = "disease-free survival",
            compete = "competing risks")

        status_factor <- NULL
        if (identical(analysistype, "compete"))
            status_factor <- factor(
                c("Censored", "Event", "Competing")[status + 1L],
                levels = c("Censored", "Event", "Competing"))

        event_labels <- unlist(buckets[filled][codes[names(buckets)[filled]] == 1])
        cens_labels  <- unlist(buckets[filled][codes[names(buckets)[filled]] == 0])

        return(ok(status,
                  event_label     = paste(event_labels, collapse = ", "),
                  censored_labels = as.character(cens_labels),
                  estimand        = estimand,
                  n_levels        = length(observed),
                  status_factor   = status_factor))
    }

    # ---- labelled cause factor handed over from outcomeorganizer ----------
    # outcomeorganizer emits Censored/Event/Competing so the competing-risk
    # coding survives the hand-off instead of being silently binarised.
    #
    # The test has to be strict. Matching on `"Event" %in% levels(outcome)` alone
    # hijacked ordinary user data: a perfectly normal outcome with levels
    # "Event"/"No event" was read as a hand-off, so choosing "No event" as the
    # event level silently selected "Event" instead AND mapped every "No event"
    # row to NA -- deleting those patients and reporting a 100% event rate.
    # Require that EVERY observed level is one of the three emitted names, and
    # that the user has not pointed outcomeLevel somewhere else.
    cause_levels <- c("Censored", "Event", "Competing")
    looks_like_handoff <- is.factor(outcome) &&
        all(observed_chr %in% cause_levels) &&
        (is.null(outcomeLevel) || length(outcomeLevel) == 0 ||
         identical(as.character(outcomeLevel), "") ||
         as.character(outcomeLevel) %in% cause_levels)

    if (looks_like_handoff) {
        # An explicit choice of "Competing" as the event means the user wants a
        # cause-specific analysis of the competing cause: it becomes the event
        # and the original Event level is censored. Previously the selection was
        # ignored and Event was always coded 1.
        focus_competing <- !is.null(outcomeLevel) && length(outcomeLevel) > 0 &&
                           identical(as.character(outcomeLevel), "Competing")

        status <- rep(NA_integer_, length(outcome))
        status[!is.na(outcome) & outcome == "Censored"]  <- 0L
        if (focus_competing) {
            status[!is.na(outcome) & outcome == "Competing"] <- 1L
            status[!is.na(outcome) & outcome == "Event"]     <- 2L
        } else {
            status[!is.na(outcome) & outcome == "Event"]     <- 1L
            status[!is.na(outcome) & outcome == "Competing"] <- 2L
        }
        has_comp <- any(status == 2, na.rm = TRUE)
        return(ok(status,
                  event_label     = if (focus_competing) "Competing" else "Event",
                  censored_labels = "Censored",
                  estimand        = if (has_comp) "competing risks" else "overall survival",
                  n_levels        = length(observed),
                  status_factor   = if (has_comp) factor(
                      c("Censored", "Event", "Competing")[status + 1L],
                      levels = c("Censored", "Event", "Competing")) else NULL))
    }

    # ---- single event level ----------------------------------------------
    if (is.logical(outcome)) {
        # Honour an explicitly selected level. TRUE is the event by default, but
        # selecting FALSE used to be read and then ignored, silently inverting
        # the analysis.
        lvl <- if (!is.null(outcomeLevel) && length(outcomeLevel) > 0)
            toupper(as.character(outcomeLevel)) else "TRUE"
        if (identical(lvl, "FALSE"))
            return(ok(as.integer(!outcome), "FALSE", "TRUE", "overall survival", 2L))
        return(ok(as.integer(outcome), "TRUE", "FALSE", "overall survival", 2L))
    }

    if (is.factor(outcome) || is.character(outcome)) {

        if (is.null(outcomeLevel) || length(outcomeLevel) == 0 ||
            identical(as.character(outcomeLevel), ""))
            return(fail(paste0(
                "Event Level is not selected. Outcome variable '", outcome_name,
                "' has levels: ", paste(observed_chr, collapse = ", "),
                ". Select the level that represents the event (e.g. death, recurrence).")))

        # A level with zero observations -- left over after a row filter or a
        # variable change -- used to yield an all-zero indicator and a flat
        # survival curve at 1.0 with no error at all.
        if (!as.character(outcomeLevel) %in% observed_chr)
            return(fail(paste0(
                "Selected Event Level '", outcomeLevel, "' does not occur in the data. ",
                "Values found in '", outcome_name, "': ",
                paste(observed_chr, collapse = ", "), ".")))

        status <- ifelse(as.character(outcome) == as.character(outcomeLevel), 1L, 0L)
        cens   <- setdiff(observed_chr, as.character(outcomeLevel))

        return(ok(status,
                  event_label     = as.character(outcomeLevel),
                  censored_labels = cens,
                  # Three or more levels means the extras are being cause-specific
                  # censored, which biases the probability-scale outputs.
                  estimand        = if (length(observed) > 2) "cause-specific survival"
                                    else "overall survival",
                  n_levels        = length(observed)))
    }

    if (is.numeric(outcome)) {

        # Honour an explicitly chosen event level for numeric outcomes too.
        # Previously outcomeLevel was read and then ignored here, so a
        # 0 = dead / 1 = alive registry column ran inverted without warning.
        if (!is.null(outcomeLevel) && length(outcomeLevel) > 0 &&
            !identical(as.character(outcomeLevel), "")) {

            lvl <- suppressWarnings(as.numeric(as.character(outcomeLevel)))
            if (is.na(lvl) || !any(observed == lvl))
                return(fail(paste0(
                    "Selected Event Level '", outcomeLevel, "' does not occur in the data. ",
                    "Values found in '", outcome_name, "': ",
                    paste(sort(observed), collapse = ", "), ".")))

            status <- ifelse(outcome == lvl, 1L, 0L)
            return(ok(status,
                      event_label     = as.character(lvl),
                      censored_labels = as.character(sort(setdiff(observed, lvl))),
                      estimand        = if (length(observed) > 2) "cause-specific survival"
                                        else "overall survival",
                      n_levels        = length(observed)))
        }

        # No level chosen: accept only genuine 0/1 coding. A single distinct
        # value is legitimate (a cohort in which everyone had the event, or
        # nobody did) and used to be rejected. `sum(unique(x)) == 1` was the old
        # test in three of the five copies and wrongly accepted e.g. {-1, 2}.
        if (all(observed %in% c(0, 1)))
            return(ok(as.integer(outcome), "1", "0", "overall survival",
                      length(observed)))

        return(fail(paste0(
            "Outcome variable '", outcome_name, "' is numeric but not coded 0/1. ",
            "Values found: ", paste(sort(observed), collapse = ", "),
            ". Either recode as 0 = censored / 1 = event, or select which value ",
            "represents the event using the Event Level option.")))
    }

    fail(paste0(
        "Outcome variable '", outcome_name, "' has an unsupported type (",
        class(outcome)[1], "). It must be a factor, character, logical, or ",
        "numeric 0/1 variable."))
}

#' Describe an event recode for display to the user
#'
#' Turns the result of `.defineEventIndicator()` into the HTML disclosure block
#' shown by every analysis that builds an event indicator. A silent recode is a
#' clinical-safety hazard: the reader of a survival curve cannot otherwise see
#' which levels were collapsed into "censored".
#'
#' @param res The list returned by `.defineEventIndicator()`.
#' @param outcome_name Display name of the outcome variable.
#' @return A character string of HTML.
#' @keywords internal
.describeEventIndicator <- function(res, outcome_name = "outcome") {

    if (is.null(res) || !is.null(res$error)) return("")

    cens <- if (length(res$censored_labels) > 0)
        paste(sprintf('"%s"', res$censored_labels), collapse = ", ") else "(none)"

    rows <- paste0(
        "<tr><td>Event level</td><td>\"", res$event_label,
            "\"</td><td align='right'>", res$n_event, "</td></tr>",
        "<tr><td>Censored</td><td>", cens,
            "</td><td align='right'>", res$n_censored, "</td></tr>",
        if (res$n_competing > 0)
            paste0("<tr><td>Competing event</td><td>&nbsp;</td><td align='right'>",
                   res$n_competing, "</td></tr>") else "",
        "<tr><td>Excluded (missing outcome)</td><td>&nbsp;</td><td align='right'>",
            res$n_missing, "</td></tr>",
        "<tr><td>Estimand</td><td colspan='2'>", res$estimand, "</td></tr>")

    warn <- ""
    if (res$n_levels > 2 && res$n_competing == 0) {
        warn <- paste0(
            "<p style='margin-top:8px'><b>Note.</b> '", outcome_name,
            "' has ", res$n_levels, " levels but only one was selected as the event. ",
            "The remaining level(s) are being treated as <i>censored</i>, which assumes ",
            "censoring is independent of the event. Cox hazard ratios remain valid under ",
            "this assumption, but if any collapsed level is a competing event (for example ",
            "death from another cause) the Kaplan-Meier curve, median survival and x-year ",
            "survival are biased upward. For competing events use Multiple Event Levels ",
            "with survival type Competing Risk.</p>")
    }

    paste0(
        "<div><b>Outcome recode for '", outcome_name, "'</b>",
        "<table style='margin-top:4px'>", rows, "</table>", warn, "</div>")
}

#' Message shown when an output is unavailable in competing-risks mode
#'
#' `survival::Surv()` does not reject a 0/1/2 status vector. It emits only a
#' warning and remaps 1 to censored, 2 to event and 0 to `NA`, which jamovi
#' never surfaces. Any output that cannot handle competing risks must therefore
#' be blocked explicitly and say why, rather than silently rendering inverted
#' results.
#'
#' @param feature Display name of the output being blocked.
#' @return A character string.
#' @keywords internal
.competingRiskUnavailable <- function(feature) {
    paste0(feature, " is not available for competing-risks analysis. It assumes a ",
           "single event type, and the competing-risk outcome is coded 0/1/2. Use the ",
           "cumulative incidence output instead, or set survival type to Overall or ",
           "Cause Specific.")
}

#' Build a survival model formula from variable names
#'
#' Consolidated helper used across the survival-analysis backends (e.g.
#' `multisurvival.b.R`) to assemble a `survival::Surv(...) ~ ...` formula
#' from raw variable names, with safe escaping of non-syntactic names via
#' `.escapeVariableNames()` and safe parsing via `.asSurvivalFormula()`.
#'
#' @param time_var Character. Time variable name (or start time for
#'   `"standard"`/`"interval"` types).
#' @param outcome_var Character. Event/outcome variable name.
#' @param predictors Character vector of predictor (main-effect) variable
#'   names.
#' @param survival_type One of `"standard"`, `"counting"`, `"interval"`.
#' @param start_var Character. Start-time variable name (required when
#'   `survival_type = "counting"`).
#' @param stop_var Character. Stop-time variable name (required when
#'   `survival_type` is `"counting"` or `"interval"`).
#' @param strata_vars Character vector of stratification variable names.
#' @param interaction_terms Character vector of already-escaped, `:`-joined
#'   interaction terms (e.g. `` "`Arm`:`Bio`" ``) appended to the right-hand
#'   side after main effects and before strata.
#' @return A parsed formula object (see `.asSurvivalFormula()`).
#' @keywords internal
.buildSurvivalFormula <- function(time_var, outcome_var, predictors, survival_type = "standard", start_var = NULL, stop_var = NULL, strata_vars = NULL, interaction_terms = NULL) {
  # Escape all variable names for safe formula construction
  escaped_time <- .escapeVariableNames(time_var)
  escaped_outcome <- .escapeVariableNames(outcome_var)
  escaped_predictors <- .escapeVariableNames(predictors)

  # Build left-hand side based on survival type
  lhs <- switch(survival_type,
    "standard" = paste0("survival::Surv(", escaped_time, ", ", escaped_outcome, ")"),
    "counting" = {
      if (is.null(start_var) || is.null(stop_var)) {
        jmvcore::reject("Start and stop variables required for counting process format")
      }
      escaped_start <- .escapeVariableNames(start_var)
      escaped_stop <- .escapeVariableNames(stop_var)
      paste0("survival::Surv(", escaped_start, ", ", escaped_stop, ", ", escaped_outcome, ")")
    },
    "interval" = {
      if (is.null(stop_var)) {
        jmvcore::reject("Stop time variable required for interval censoring")
      }
      escaped_stop <- .escapeVariableNames(stop_var)
      paste0("survival::Surv(", escaped_time, ", ", escaped_stop, ", ", escaped_outcome, ")")
    },
    jmvcore::reject("Unknown survival type: ", survival_type)
  )

  # Build right-hand side: main effects + interaction terms (already escaped)
  main_terms <- if (length(escaped_predictors) == 0) character(0) else escaped_predictors
  int_terms  <- if (length(interaction_terms) == 0) character(0) else interaction_terms
  rhs_terms  <- c(main_terms, int_terms)

  if (length(rhs_terms) == 0) {
    rhs <- "1"  # Null model
  } else {
    rhs <- paste(rhs_terms, collapse = " + ")
  }

  # Add stratification if specified (applies whether or not predictors exist)
  if (!is.null(strata_vars) && length(strata_vars) > 0) {
    escaped_strata <- .escapeVariableNames(strata_vars)
    strata_term <- paste0("strata(", paste(escaped_strata, collapse = ", "), ")")
    rhs <- if (identical(rhs, "1")) strata_term else paste(rhs, strata_term, sep = " + ")
  }

  formula_string <- paste0(lhs, " ~ ", rhs)
  return(.asSurvivalFormula(formula_string))
}
