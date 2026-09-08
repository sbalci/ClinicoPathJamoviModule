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
#'     event level becomes 0 (right-censored). Labels alone cannot establish
#'     whether this is ordinary censoring or a competing terminal event. In the
#'     latter case Kaplan-Meier outputs describe net/cause-specific survival,
#'     not absolute event risk. The caller is expected to surface `estimand` and
#'     `censored_labels` to the user.
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
#'   `censored_labels`, `competing_labels`, `n_levels`, `has_competing`,
#'   `estimand`, and `error`
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
                   n_levels, status_factor = NULL,
                   competing_labels = character(0), competing_mode = NULL) {
        list(
            status          = status,
            status_factor   = status_factor,
            n_event         = sum(status == 1, na.rm = TRUE),
            n_censored      = sum(status == 0, na.rm = TRUE),
            n_competing     = sum(status == 2, na.rm = TRUE),
            n_missing       = sum(is.na(status)),
            event_label     = event_label,
            censored_labels = censored_labels,
            competing_labels = competing_labels,
            n_levels        = n_levels,
            # This flag means that competing-risk METHODS are required. It must
            # remain TRUE when a competing state is declared but happens to have
            # zero observations in the current cohort; otherwise the same
            # outcome silently changes from Aalen-Johansen/CIF to Kaplan-Meier
            # merely because one state is absent in a subset.
            has_competing   = if (is.null(competing_mode))
                any(status == 2, na.rm = TRUE) else isTRUE(competing_mode),
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
        comp_labels  <- unlist(buckets[filled][codes[names(buckets)[filled]] == 2])

        return(ok(status,
                  event_label     = paste(event_labels, collapse = ", "),
                  censored_labels = as.character(cens_labels),
                  estimand        = estimand,
                  n_levels        = length(observed),
                  status_factor   = status_factor,
                  competing_labels = as.character(comp_labels),
                  competing_mode  = identical(analysistype, "compete")))
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
    # Require the complete DECLARED level set, not merely a subset of the names
    # among observed rows. A user-created binary factor with levels
    # Censored/Event is an ordinary outcome; conversely, an outcomeorganizer
    # hand-off remains a competing-risk outcome when its Competing level is
    # declared but unused in this particular cohort.
    cause_levels <- c("Censored", "Event", "Competing")
    looks_like_handoff <- is.factor(outcome) &&
        setequal(levels(outcome), cause_levels) &&
        all(observed_chr %in% cause_levels) &&
        (is.null(outcomeLevel) || length(outcomeLevel) == 0 ||
         identical(as.character(outcomeLevel), "") ||
         as.character(outcomeLevel) %in% cause_levels)

    if (looks_like_handoff) {
        # "Censored" has an explicit structural meaning in this interchange
        # format. Treating it as the event would turn both actual event states
        # into censoring and produce a clinically meaningless analysis that
        # nevertheless looks valid. Refuse the configuration instead.
        if (!is.null(outcomeLevel) && length(outcomeLevel) > 0 &&
            identical(as.character(outcomeLevel), "Censored"))
            return(fail(paste0(
                "'Censored' cannot be selected as the event level for a ",
                "Censored/Event/Competing outcome. Select 'Event' or 'Competing'.")))

        # An explicit choice of "Competing" as the event refocuses the
        # competing-risk analysis: it becomes code 1 and the original Event
        # becomes code 2. Previously the selection was ignored and Event was
        # always coded 1.
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
        return(ok(status,
                  event_label     = if (focus_competing) "Competing" else "Event",
                  censored_labels = "Censored",
                  competing_labels = if (focus_competing) "Event" else "Competing",
                  estimand        = "competing risks",
                  n_levels        = length(observed),
                  status_factor   = factor(
                      c("Censored", "Event", "Competing")[status + 1L],
                      levels = c("Censored", "Event", "Competing")),
                  competing_mode  = TRUE))
    }

    # ---- single event level ----------------------------------------------
    if (is.logical(outcome)) {
        # Honour an explicitly selected level. TRUE is the event by default, but
        # selecting FALSE used to be read and then ignored, silently inverting
        # the analysis.
        lvl <- if (!is.null(outcomeLevel) && length(outcomeLevel) > 0)
            toupper(as.character(outcomeLevel)) else "TRUE"
        if (!lvl %in% c("TRUE", "FALSE"))
            return(fail(paste0(
                "Selected Event Level '", outcomeLevel,
                "' is invalid for logical outcome '", outcome_name,
                "'. Select TRUE or FALSE.")))
        if (identical(lvl, "FALSE"))
            return(ok(as.integer(!outcome), "FALSE", "TRUE",
                      "Kaplan-Meier survival for the coded event", 2L))
        return(ok(as.integer(outcome), "TRUE", "FALSE",
                  "Kaplan-Meier survival for the coded event", 2L))
    }

    if (is.factor(outcome) || is.character(outcome)) {

        if (is.null(outcomeLevel) || length(outcomeLevel) == 0 ||
            identical(as.character(outcomeLevel), ""))
            return(fail(paste0(
                "Event Level is not selected. Outcome variable '", outcome_name,
                "' has levels: ", paste(observed_chr, collapse = ", "),
                ". Select the level that represents the event (e.g. death, recurrence).")))

        # A DECLARED level with zero observations is a legitimate cohort, not an
        # error. Everyone is censored: S(t) = 1 throughout, the median is not
        # reached, the number at risk and the follow-up are reportable. Refusing
        # it threw a valid analysis away -- and did so inconsistently, since an
        # all-zero NUMERIC outcome was accepted a few dozen lines below.
        #
        # What remains unusable is a level the variable does not have AT ALL
        # (a stale selection left behind when the outcome variable changed, or a
        # typo): nothing can be coded from it, so that still fails.
        #
        # WHAT THIS COSTS: picking the WRONG event level now produces a silent
        # all-censored analysis where it used to error. `.describeEventIndicator()`
        # therefore flags any zero-event recode prominently -- if that block is
        # ever removed, restore the rejection with it.
        declared <- if (is.factor(outcome)) levels(outcome) else observed_chr
        if (!as.character(outcomeLevel) %in% declared)
            return(fail(paste0(
                "Selected Event Level '", outcomeLevel, "' is not a level of '",
                outcome_name, "'. Levels available: ",
                paste(declared, collapse = ", "), ".")))

        status <- ifelse(as.character(outcome) == as.character(outcomeLevel), 1L, 0L)
        cens   <- setdiff(observed_chr, as.character(outcomeLevel))

        return(ok(status,
                  event_label     = as.character(outcomeLevel),
                  censored_labels = cens,
                  # Labels alone cannot establish that the selected event is
                  # death from any cause. Calling every binary factor "overall
                  # survival" silently turns recurrence, discharge, toxicity,
                  # or one of two causes of death into OS.
                  estimand        = "Kaplan-Meier survival for the selected event",
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
                      estimand        = "Kaplan-Meier survival for the selected event",
                      n_levels        = length(observed)))
        }

        # No level chosen: accept only genuine 0/1 coding. A single distinct
        # value is legitimate (a cohort in which everyone had the event, or
        # nobody did) and used to be rejected. `sum(unique(x)) == 1` was the old
        # test in three of the five copies and wrongly accepted e.g. {-1, 2}.
        if (all(observed %in% c(0, 1)))
            return(ok(as.integer(outcome), "1", "0",
                      "Kaplan-Meier survival for the coded event",
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

    esc <- function(x) as.character(htmltools::htmlEscape(as.character(x)))
    event_label <- esc(res$event_label)
    outcome_name <- esc(outcome_name)
    censored_labels <- esc(res$censored_labels)
    competing_labels <- esc(res$competing_labels)

    cens <- if (length(res$censored_labels) > 0)
        paste(sprintf('"%s"', censored_labels), collapse = ", ") else "(none)"
    comp <- if (length(res$competing_labels) > 0)
        paste(sprintf('"%s"', competing_labels), collapse = ", ") else "(not labelled)"

    rows <- paste0(
        "<tr><td>Event level</td><td>\"", event_label,
            "\"</td><td align='right'>", res$n_event, "</td></tr>",
        "<tr><td>Censored</td><td>", cens,
            "</td><td align='right'>", res$n_censored, "</td></tr>",
        if (isTRUE(res$has_competing))
            paste0("<tr><td>Competing event</td><td>", comp,
                   "</td><td align='right'>",
                   res$n_competing, "</td></tr>") else "",
        "<tr><td>Excluded (missing outcome)</td><td></td><td align='right'>",
            res$n_missing, "</td></tr>",
        "<tr><td>Estimand</td><td colspan='2'>", res$estimand, "</td></tr>")

    warn <- ""
    # Every Kaplan-Meier or cumulative-incidence analysis relies on an
    # independent/non-informative right-censoring assumption. Previously this
    # disclosure appeared only for two selected-event estimands, so an overall,
    # disease-free, logical, numeric, or competing-risk analysis could be shown
    # without stating its central identifying assumption.
    if (length(res$censored_labels) > 0) {
        collapsed_terminal <- res$estimand %in% c(
            "cause-specific survival",
            "Kaplan-Meier survival for the selected event",
            "Kaplan-Meier survival for the coded event") &&
            res$n_competing == 0
        warn <- paste0(
            "<p style='margin-top:8px'><b>Censoring assumption.</b> Level(s) ", cens,
            " are being treated as <i>right-censored</i>. The estimates require censoring ",
            "to be independent/non-informative for the endpoint, conditional on the analysis. ",
            if (collapsed_terminal) paste0(
                "If a collapsed level is a competing terminal event (for example death from ",
                "another cause), Kaplan-Meier estimates net/cause-specific survival rather ",
                "than absolute event risk and generally overstates the real-world probability ",
                "of remaining event-free. Use Multiple Event Levels with survival type ",
                "Competing Risk to estimate cumulative incidence."
            ) else "This assumption cannot be verified from the displayed summaries.",
            "</p>")
    }

    # A zero-event cohort is now ACCEPTED rather than rejected (see the declared-
    # level check in .defineEventIndicator). That is the right call statistically
    # -- a fully censored series is analysable -- but it means a mis-selected
    # event level no longer announces itself as an error, so it has to announce
    # itself here instead. This block is the safety net for that change and must
    # stay as loud as the error it replaced.
    zero <- ""
    if (isTRUE(res$n_event == 0)) {
        zero <- paste0(
            "<p style='margin-top:8px;padding:8px;border-left:4px solid #c0392b;",
            "background-color: rgba(234, 54, 33, 0.09); color: inherit'><b>No events: check the event level.</b> ",
            "The event level (", event_label, ") occurs in <b>0</b> of the ",
            res$n_event + res$n_censored + res$n_competing,
            " rows with a non-missing '", outcome_name, "' value. ",
            if (isTRUE(res$has_competing)) paste0(
                res$n_competing, " competing event(s) did occur; these are a separate ",
                "terminal state, not censoring, and the target-event cumulative incidence ",
                "is estimated as 0 throughout") else
                "This is a fully censored analysis cohort: every subject is treated as censored and the Kaplan-Meier point estimate is 100% throughout. This boundary estimate does not prove 100% population survival",
            if (isTRUE(res$has_competing))
                ". The median time to the target event is not reached" else
                ". The Kaplan-Meier median time-to-event is not reached",
            ", and the numbers at risk and the ",
            "follow-up duration are still reportable. <b>But if you expected events, ",
            "the wrong event level is selected</b>: check the level named above against ",
            "your data before using any of the results below.</p>")
    }

    # Disease-free survival makes a timing assumption that nothing in the data
    # can verify, so it has to be stated wherever the user chooses DFS. Alive
    # with Disease is coded as an EVENT at whatever time the time variable holds
    # for that subject; if that time is diagnosis-to-last-follow-up (what a
    # follow-up date normally means, and what the UI suggests) the recurrence is
    # recorded later than it happened and DFS is biased upward. This is a silent
    # bias, not a detectable error -- there is no signature in the data to test.
    dfs <- ""
    if (identical(res$estimand, "disease-free survival")) {
        dfs <- paste0(
            "<p style='margin-top:8px;padding:8px;border-left:4px solid #d68910;",
            "background-color: rgba(246, 163, 33, 0.11); color: inherit'><b>Disease-free survival requires a time to the ",
            "DFS event.</b> \"Alive with Disease\" is counted as an event, and it is placed ",
            "at whatever time the time variable gives for that subject. These results are ",
            "correct only if that time is the time to recurrence or progression. If it is ",
            "instead the time from diagnosis to the LAST FOLLOW-UP, every recurrence is ",
            "dated later than it occurred and disease-free survival is over-estimated. ",
            "For subjects with disease, supply the date of recurrence as the follow-up date ",
            "(or an elapsed time measured to recurrence).</p>")
    }

    paste0(
        "<div><b>Outcome recode for '", outcome_name,
        "' (before analysis-specific row exclusions)</b>",
        "<table style='margin-top:4px'>", rows, "</table>", warn, zero, dfs, "</div>")
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
.competingRiskUnavailable <- function(feature, self = NULL) {
    if (is.null(self)) {
        paste0(feature, " is not available for competing-risks analysis. It assumes a ",
               "single event type, and the competing-risk outcome is coded 0/1/2. Use the ",
               "cumulative incidence output instead, or set survival type to Overall or ",
               "Cause Specific.")
    } else {
        jmvcore::format(.("{feature} is not available for competing-risks analysis. It assumes a single event type, and the competing-risk outcome is coded 0/1/2. Use the cumulative incidence output instead, or set survival type to Overall or Cause Specific."), feature = feature)
    }
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

    # One strata() call PER variable, not a single multi-argument strata().
    #
    # `strata(a, b)` collapses the variables into one combined factor whose
    # levels are pasted labels ("Control, I"). Anything that re-evaluates the
    # model terms on new data -- predict.coxph, and therefore riskRegression's
    # Brier/AUC scoring -- rebuilds that factor from the new data and gets a
    # level set that does not match the one recorded at fit time, failing with
    # "factor strata(a, b) has new levels Control, I, I, ...". Separate
    # strata() terms are handled per variable and survive the round trip.
    #
    # This is a representation change only: survival crosses multiple strata
    # terms internally, so `strata(a) + strata(b)` and `strata(a, b)` give
    # identical coefficients and log-likelihood (verified).
    strata_term <- paste0("strata(", escaped_strata, ")")
    strata_term <- paste(strata_term, collapse = " + ")
    rhs <- if (identical(rhs, "1")) strata_term else paste(rhs, strata_term, sep = " + ")
  }

  formula_string <- paste0(lhs, " ~ ", rhs)
  # Forward OUR caller's frame. Omitting `env=` makes `.asSurvivalFormula()`
  # default to parent.frame() = this builder's frame, which holds only
  # time_var/escaped_* and no data -- so cox.zph() and anything else that
  # re-evaluates the model call fails with "object 'mydata' not found".
  return(.asSurvivalFormula(formula_string, env = parent.frame()))
}

#' Format a follow-up time for display in an interval label
#'
#' Interval labels are built from raw numeric bounds, and the final bound is the
#' observed maximum follow-up -- an unrounded double that rendered as
#' "60-134.449661066093". Whole numbers stay integer-looking; anything else is
#' shown to one decimal, which is the precision follow-up times are reported in.
#'
#' @param x numeric time value
#' @return character scalar
#' @keywords internal
.fmtTimeLabel <- function(x) {
    if (length(x) == 0 || is.na(x)) return("NA")
    if (isTRUE(all.equal(x, round(x)))) format(round(x), trim = TRUE)
    else format(round(x, 1), nsmall = 1, trim = TRUE)
}

# ============================================================================
# MEDIAN FOLLOW-UP (REVERSE KAPLAN-MEIER)
# ============================================================================
# Consolidated 2026-09-07. Three analyses (singlearm, multisurvival, swimmerplot)
# each carried their own hand-copied reverse-KM implementation while fifteen
# others reported median(time) under a "median follow-up" label. The correct
# computation now lives here once; every caller reports the same number, the
# same not-estimable fallback, and the same honest label.
#
# Two naive estimators this replaces, and why both are wrong:
#
#   median(all observed times)   is the median time to EVENT-OR-CENSORING. In a
#     cohort where most subjects have the event early it is close to the median
#     SURVIVAL and badly understates how long the cohort was watched -- which is
#     exactly the number a reader uses to judge whether a 5-year estimate is
#     supported at all.
#
#   median(times among the censored only)   discards everyone who had the event
#     and conditions on having survived long enough to still be at risk, so it
#     OVERSTATES follow-up.
#
# The reverse Kaplan-Meier uses every subject: someone who died at 3 months
# contributes "watched for at least 3 months" as a censored observation in the
# reversed problem, rather than being either counted as a short follow-up or
# thrown away.

#' Median follow-up by the reverse Kaplan-Meier method
#'
#' Estimates how long a cohort was actually observed, by swapping the roles of
#' event and censoring and fitting an ordinary Kaplan-Meier curve to the result
#' (Schemper & Smith 1996). The median of that reversed curve estimates the
#' median of the potential-follow-up distribution: the time each subject would
#' have been observed had the event not intervened.
#'
#' `censored` must mark subjects whose observation ended WITHOUT a terminal
#' outcome -- alive at last contact, lost to follow-up, administratively
#' censored. In a competing-risks setting a competing death is a terminal
#' outcome and must NOT be marked censored here: it ends potential follow-up
#' just as the event of interest does, and counting it as a reverse-KM event
#' understates the reported follow-up.
#'
#' The median is undefined whenever the reversed curve never falls to 50%. That
#' depends on WHEN subjects were still under observation, not simply on how many
#' were: a handful censored late can make the median estimable, while a cohort
#' whose censored subjects all left early cannot -- so do not read a fallback as
#' meaning "too little censoring". It is a real and common state, not an error,
#' so the function falls back to the plain median of observed times and reports
#' `reverse = FALSE` plus a `reason`. Callers MUST
#' surface that distinction -- printing the fallback under a "reverse
#' Kaplan-Meier" label is a quiet lie. Use [.medianFollowUpLabel()] rather than
#' hardcoding a label.
#'
#' @param time Numeric vector of observed times (event or censoring).
#' @param censored Logical or 0/1 vector, `TRUE`/`1` where the subject was still
#'   under observation when follow-up ended. Recycled length-1 values are not
#'   accepted; it must be parallel to `time`.
#' @param conf_level Confidence level for the interval around the median.
#'   Default 0.95.
#' @return A list with `value` (the estimate), `ci_lower` / `ci_upper` (`NA`
#'   unless the reverse fit succeeded and the interval is defined), `reverse`
#'   (`TRUE` when the reverse-KM median was estimable), `method`
#'   (`"reverse_km"` or `"observed_median"`), `reason` (empty when
#'   `reverse = TRUE`, otherwise why the fallback was used), `n_total` and
#'   `n_censored`.
#' @references
#' Schemper, M., & Smith, T. L. (1996). A note on quantifying follow-up in
#' studies of failure time. Controlled Clinical Trials, 17(4), 343-346.
#' \doi{10.1016/0197-2456(96)00075-X}
#' @keywords internal
.medianFollowUp <- function(time, censored, conf_level = 0.95) {

    fallback <- function(reason, n_total = 0L, n_censored = 0L) {
        list(value = suppressWarnings(stats::median(time, na.rm = TRUE)),
             ci_lower = NA_real_, ci_upper = NA_real_,
             reverse = FALSE, method = "observed_median", reason = reason,
             n_total = n_total, n_censored = n_censored)
    }

    time <- suppressWarnings(as.numeric(time))
    if (length(time) == 0L || all(is.na(time)))
        return(fallback("no observed times were available"))

    # A caller that cannot classify its status column at all passes NULL rather
    # than silently treating everyone as an event.
    if (is.null(censored))
        return(fallback("the censoring indicator could not be interpreted",
                        n_total = sum(!is.na(time))))
    if (length(censored) != length(time))
        return(fallback("the censoring indicator and time vector had different lengths",
                        n_total = sum(!is.na(time))))

    cens <- as.integer(!is.na(censored) & (censored == 1 | censored %in% TRUE))
    usable <- !is.na(time) & is.finite(time) & time >= 0
    time_u <- time[usable]
    cens_u <- cens[usable]
    n_total <- length(time_u)
    n_censored <- sum(cens_u)

    if (n_total == 0L)
        return(fallback("no usable (finite, non-negative) times were available"))
    if (n_censored == 0L)
        return(fallback(paste0(
            "no subject was censored, so the reversed curve has no events and ",
            "never reaches 50%. Every subject was observed to the terminal ",
            "outcome, so the observed times ARE the complete follow-up"),
            n_total = n_total, n_censored = 0L))

    fit <- try(survival::survfit(survival::Surv(time_u, cens_u) ~ 1,
                                 conf.int = conf_level), silent = TRUE)
    if (inherits(fit, "try-error"))
        return(fallback("the reverse Kaplan-Meier fit could not be computed",
                        n_total = n_total, n_censored = n_censored))

    # summary()$table carries the median and its interval under names that
    # differ between survival versions ("0.95LCL" vs "lower 95% CI"), so index
    # positionally off the known layout rather than by a version-specific name.
    tbl <- try(summary(fit)$table, silent = TRUE)
    if (inherits(tbl, "try-error") || is.null(tbl))
        return(fallback("the reverse Kaplan-Meier fit returned no summary table",
                        n_total = n_total, n_censored = n_censored))

    pick <- function(nm) {
        hit <- grep(nm, names(tbl), ignore.case = TRUE, value = TRUE)
        if (length(hit) == 0L) return(NA_real_)
        suppressWarnings(as.numeric(unname(tbl[[hit[[1]]]])))
    }
    m <- pick("^median$")
    if (length(m) != 1L || is.na(m))
        return(fallback(paste0(
            "the reversed Kaplan-Meier curve never falls to 50%, so its median ",
            "is undefined. This depends on WHEN subjects were still under ",
            "observation, not merely how many: here ", n_censored, " of ",
            n_total, " were censored, but they left early relative to the rest ",
            "of the cohort. A small number of subjects censored late can make ",
            "the median estimable, while many censored early cannot"),
            n_total = n_total, n_censored = n_censored))

    list(value = m,
         ci_lower = pick("LCL|lower"), ci_upper = pick("UCL|upper"),
         reverse = TRUE, method = "reverse_km", reason = "",
         n_total = n_total, n_censored = n_censored)
}

#' Label for a median follow-up estimate
#'
#' Names the quantity that was actually computed. When the reverse-KM median was
#' not estimable the returned label says "median observed time", because calling
#' the fallback a reverse-KM follow-up misrepresents it.
#'
#' @param mfu Result of [.medianFollowUp()].
#' @return Character scalar.
#' @keywords internal
.medianFollowUpLabel <- function(mfu) {
    if (isTRUE(mfu$reverse)) "Median follow-up (reverse Kaplan-Meier)"
    else "Median observed time (reverse Kaplan-Meier not estimable)"
}

#' Formatted median follow-up value, with confidence interval when available
#'
#' @param mfu Result of [.medianFollowUp()].
#' @param unit Optional time unit appended to the value (e.g. `"months"`).
#' @param conf_level Confidence level, used only to label the interval.
#' @return Character scalar, e.g. `"25.4 months (95% CI 22.2 to 28.5)"`.
#' @keywords internal
.medianFollowUpText <- function(mfu, unit = "", conf_level = 0.95) {
    if (is.null(mfu) || length(mfu$value) == 0L || is.na(mfu$value)) return("not estimable")
    txt <- .fmtTimeLabel(mfu$value)
    if (nzchar(unit)) txt <- paste(txt, unit)
    if (!is.na(mfu$ci_lower) && !is.na(mfu$ci_upper))
        txt <- paste0(txt, " (", round(conf_level * 100), "% CI ",
                      .fmtTimeLabel(mfu$ci_lower), " to ",
                      .fmtTimeLabel(mfu$ci_upper), ")")
    txt
}

#' HTML explanation of how median follow-up was calculated and why
#'
#' Rendered next to the estimate so a clinician reading the output can tell what
#' the number means, which estimator produced it, and -- when the reverse-KM
#' median was not estimable -- exactly why the fallback appears instead.
#'
#' Colours are expressed as translucent tints over the host background rather
#' than opaque hex fills, so the block stays readable in jamovi's dark theme.
#'
#' @param mfu Result of [.medianFollowUp()].
#' @param unit Optional time unit (e.g. `"months"`).
#' @param conf_level Confidence level used for the interval.
#' @return An HTML string.
#' @keywords internal
.medianFollowUpExplanation <- function(mfu, unit = "", conf_level = 0.95) {
    if (is.null(mfu)) return("")

    label <- .medianFollowUpLabel(mfu)
    value <- .medianFollowUpText(mfu, unit, conf_level)

    how <- if (isTRUE(mfu$reverse)) paste0(
        "<p style='margin:0 0 8px 0;'><b>How this was calculated.</b> ",
        "The roles of event and censoring were swapped: subjects still under ",
        "observation when follow-up ended became the &quot;events&quot;, and ",
        "subjects who reached the terminal outcome were treated as censored. ",
        "An ordinary Kaplan-Meier curve was fitted to that reversed problem, ",
        "and its median is reported above. That median estimates the ",
        "<i>potential</i> follow-up time: how long each subject would have been ",
        "observed had the event not intervened. Here ", mfu$n_censored,
        " of ", mfu$n_total, " subjects were still under observation.</p>")
    else paste0(
        "<p style='margin:0 0 8px 0;'><b>Why a fallback is shown.</b> ",
        "The reverse Kaplan-Meier median could not be estimated because ",
        mfu$reason, ". The plain median of the observed times is reported ",
        "instead, and it is labelled as such. Read it as the median time to ",
        "event-or-censoring, <i>not</i> as the length of follow-up: in a cohort ",
        "with many early events the two differ substantially.</p>")

    why <- paste0(
        "<p style='margin:0 0 8px 0;'><b>Why not just take the median of the ",
        "observed times?</b> Because that is the median time to ",
        "event-or-censoring. In a cohort where most subjects have the event ",
        "early it approximates the median <i>survival</i> and understates how ",
        "long the cohort was watched. Taking the median among censored ",
        "subjects only has the opposite fault: it discards everyone who had ",
        "the event and overstates follow-up. The reverse Kaplan-Meier uses ",
        "every subject \u{2014} someone who died at 3 months contributes ",
        "&quot;observed for at least 3 months&quot; rather than being ",
        "discarded or counted as short follow-up.</p>")

    paste0(
        "<div style='background-color:rgba(127,127,127,0.10);",
        "border-left:3px solid rgba(127,127,127,0.55);",
        "padding:10px 12px;margin:8px 0;border-radius:3px;'>",
        "<p style='margin:0 0 8px 0;'><b>", label, ":</b> ", value, "</p>",
        how, why,
        "<p style='margin:0;font-size:0.9em;'><b>Reference.</b> ",
        "Schemper M, Smith TL. A note on quantifying follow-up in studies of ",
        "failure time. <i>Controlled Clinical Trials</i> 1996;17(4):343-346. ",
        "doi:10.1016/0197-2456(96)00075-X</p>",
        "</div>")
}
