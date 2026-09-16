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
    # base:: explicitly: under import(jmvcore), bare format() is jmvcore::format,
    # which returns the number unchanged and ignores nsmall.
    if (isTRUE(all.equal(x, round(x)))) base::format(round(x), trim = TRUE)
    else base::format(round(x, 1), nsmall = 1, trim = TRUE)
}

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
