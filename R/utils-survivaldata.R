# ============================================================================
# SURVIVAL DATA PREPARATION: factors, landmark, numeric and parsed dates
# ============================================================================
# Shared by singlearm, survival, survivalcont and multisurvival, whose hand-copied
# blocks had drifted apart: the same landmark analysed different patients and the
# same day-count dates were accepted, refused or read 86400x too small.
#
# These are file-level functions, so they cannot call .() (it needs self). They
# return counts and codes and the caller words the message.

#' Turn ordered factors into plain factors, keeping level order
#'
#' A jamovi Ordinal column arrives ordered, and contr.poly then gives unlabelled
#' `.L`/`.Q` model rows instead of comparisons with the reference level. The class
#' is edited rather than calling factor(), which would also drop empty levels
#' before .dropEmptyLevels() can report them, and strip the jamovi attributes.
#'
#' @param df A data frame.
#' @param vars Column names; NULL and names not in `df` are ignored.
#' @return `df`.
#' @noRd
.unorderFactors <- function(df, vars) {
    for (v in intersect(vars, names(df)))
        if (is.ordered(df[[v]]))
            class(df[[v]]) <- setdiff(class(df[[v]]), "ordered")
    df
}

#' Drop factor levels with no rows, after all exclusions
#'
#' With an empty first level coxph has no reference group: the dummies are
#' collinear, one is aliased to NA and the other silently becomes a different
#' comparison. Attributes are kept (droplevels() alone strips them, including the
#' variable label), and jamovi's `values` stays aligned with the levels.
#'
#' Non-factor columns are not examined. Every result but `data` and `too_few` is
#' named by variable and has one entry per factor examined.
#'
#' @param df A data frame.
#' @param vars Column names; NULL and names not in `df` are ignored.
#' @return A list: `data`; `dropped` (list of level names lost); `old_reference`
#'   and `new_reference` (first level before and after, `NA` when none remain);
#'   `reference_changed`; `n_levels` (levels remaining); and `too_few`, the
#'   variables left with fewer than two levels, which the caller must refuse.
#' @noRd
.dropEmptyLevels <- function(df, vars) {
    vars <- intersect(as.character(vars), names(df))
    vars <- stats::setNames(vars, vars)
    vars <- vars[vapply(vars, function(v) is.factor(df[[v]]), NA)]

    first_level <- function(data) vapply(vars, function(v) levels(data[[v]])[1], "")
    old_reference <- first_level(df)
    dropped <- lapply(vars, function(v) {
        x <- df[[v]]
        levels(x)[tabulate(x, nlevels(x)) == 0]
    })

    for (v in vars[lengths(dropped) > 0]) {
        x <- df[[v]]
        kept <- attributes(x)
        used <- !levels(x) %in% dropped[[v]]
        kept$levels <- levels(x)[used]
        kept$values <- kept$values[used]
        x <- droplevels(x)
        attributes(x) <- kept
        df[[v]] <- x
    }

    new_reference <- first_level(df)
    n_levels <- vapply(vars, function(v) nlevels(df[[v]]), 1L)
    list(
        data = df,
        dropped = dropped,
        old_reference = old_reference,
        new_reference = new_reference,
        reference_changed = is.na(new_reference) | new_reference != old_reference,
        n_levels = n_levels,
        too_few = unname(vars[n_levels < 2L])
    )
}

#' Apply a landmark: one rule for every analysis
#'
#' Keep T > L strictly; the new time is T - L. A subject with an event at L is not
#' event-free at L, and one censored at L adds no information. The rule has no
#' exception at L = 0, where rows with time zero are excluded and counted.
#'
#' `keep` has no NA, so `df[res$keep, ]` is safe. An invalid landmark (negative,
#' non-finite, not a single number) keeps nobody, so a caller that forgets to test
#' `valid` gets an empty cohort instead of the silent forward shift a negative
#' landmark used to give.
#'
#' @param time Numeric time, `NA` allowed.
#' @param landmark The landmark, in the units of `time`.
#' @return A list: `valid`; `keep` and `time`, both along the input, with `time`
#'   `NA` wherever `keep` is FALSE; and the counts `n_kept`, `n_excluded` (time at
#'   or before the landmark) and `n_missing` (time missing).
#' @noRd
.applyLandmark <- function(time, landmark) {
    valid <- is.numeric(landmark) && length(landmark) == 1L &&
        is.finite(landmark) && landmark >= 0
    missing_time <- is.na(time)
    if (!valid)
        return(list(
            valid = FALSE,
            keep = rep(FALSE, length(time)),
            time = rep(NA_real_, length(time)),
            n_kept = 0L, n_excluded = 0L, n_missing = sum(missing_time)
        ))

    keep <- !missing_time & time > landmark
    shifted <- time - landmark
    shifted[!keep] <- NA
    list(
        valid = TRUE,
        keep = keep,
        time = shifted,
        n_kept = sum(keep),
        n_excluded = sum(!missing_time & !keep),
        n_missing = sum(missing_time)
    )
}

#' Decide whether a numeric date column can be epoch seconds
#'
#' A bare number has no unit. Below 1e8 (1973-03-03, or 1966-10-31 when negative)
#' or above 1e10 (year 2286) it is not a date in epoch seconds, and reading it as
#' one gives survival times too small or too large by a constant, with no error.
#' Nothing is converted: every other shape is refused and the caller points the
#' user to Date-Time Converter. One such value refuses the column (`"mixed"`),
#' including a true epoch date from 1966-1973, which has to be supplied as text.
#'
#' Codes: `"epoch_seconds"` (the only accepted one); shapes, when every value is
#' under 1e8, named from the median: `"yyyymmdd"`, `"year"`, `"excel_serial"`
#' (30000 to 60000: years 1982-2064), `"day_count"` (2100 to 30000: since 1970,
#' years 1975-2052; older Excel serials land here too), `"small_number"` (ages,
#' durations), `"unknown"`; and `"too_large"` (milliseconds, SPSS seconds),
#' `"mixed"`, `"empty"` (nothing to classify), `"not_numeric"`.
#'
#' @param x A numeric column.
#' @return A list: `code`; `accepted`; `n_checked` (non-missing values);
#'   `n_refused` (those outside the range); `example` (the first of them, or `NA`).
#' @noRd
.classifyNumericDates <- function(x) {
    result <- function(code, n_checked = 0L, refused = numeric(0)) {
        list(code = code, accepted = identical(code, "epoch_seconds"),
             n_checked = n_checked, n_refused = length(refused),
             example = if (length(refused)) refused[[1]] else NA_real_)
    }

    if (!is.numeric(x)) return(result("not_numeric"))
    x <- as.numeric(x)
    x <- x[!is.na(x)]
    if (length(x) == 0) return(result("empty"))

    too_small <- abs(x) < 1e8
    too_large <- abs(x) > 1e10
    refused <- x[too_small | too_large]
    code <- if (length(refused) == 0) {
        "epoch_seconds"
    } else if (all(too_small)) {
        vals <- abs(x)
        m <- stats::median(vals)
        if (m >= 1e7) {
            parsed <- as.Date(sprintf("%.0f", vals), format = "%Y%m%d")
            if (mean(!is.na(parsed)) > 0.5) "yyyymmdd" else "unknown"
        } else if (m >= 1900 && m <= 2100 && all(vals == floor(vals))) {
            "year"
        } else if (m >= 60000) {
            "unknown"
        } else if (m >= 30000) {
            "excel_serial"
        } else if (m > 2100) {
            "day_count"
        } else {
            "small_number"
        }
    } else if (all(too_large)) {
        "too_large"
    } else {
        "mixed"
    }
    result(code, length(x), refused)
}

#' Count dates that cannot be real: at or before 1900, or from 2199 on
#'
#' Catches registry sentinels (1900-01-01, 2199-12-31), which otherwise pass as
#' dates. A date no year can be read from (infinite) counts as late.
#'
#' @param dates Date or date-time values, `NA` allowed.
#' @return A list: `n_checked` (non-missing dates), `n_early`, `n_late`,
#'   `n_implausible`, and `examples`, up to three distinct "YYYY-MM-DD" strings.
#' @noRd
.checkDatePlausibility <- function(dates) {
    lt <- as.POSIXlt(dates, tz = "UTC")
    years <- lt$year + 1900L
    present <- !is.na(dates)
    early <- present & !is.na(years) & years <= 1900L
    late <- present & (is.na(years) | years >= 2199L)
    implausible <- early | late
    list(
        n_checked = sum(present),
        n_early = sum(early),
        n_late = sum(late),
        n_implausible = sum(implausible),
        examples = utils::head(unique(base::format(lt[implausible], "%Y-%m-%d")), 3L)
    )
}

#' Count non-missing date inputs that failed to parse
#'
#' lubridate reports a mismatch as a warning, and the row then looks like missing
#' data. A blank string is missing, not a failure.
#'
#' @param raw The input column, before parsing.
#' @param parsed The parser's output, along `raw`.
#' @return A list: `n_failed`, `n_inputs` (non-missing inputs).
#' @noRd
.countDateParseFailures <- function(raw, parsed) {
    has_input <- !is.na(raw) & nzchar(trimws(as.character(raw)))
    list(n_failed = sum(has_input & is.na(parsed)), n_inputs = sum(has_input))
}
