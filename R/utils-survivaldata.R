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

#' Read the two text date columns of a survival analysis
#'
#' Reads both columns under the analysis's date type with the shape guard
#' (`.parseDateText()`), so text the type cannot have comes back missing instead
#' of as another date, and reports two-digit years with the reading they got.
#'
#' @param start_raw,end_raw The two text columns.
#' @param fmt The `timetypedata` key.
#' @return A list: `start`, `end` (parsed), `n_failed`, `n_inputs`, `n_shape`
#'   (values refused for their shape) and `two_digit` (see `.twoDigitYears()`).
#' @noRd
.readSurvivalDates <- function(start_raw, end_raw, fmt) {
    start_raw <- as.character(start_raw)
    end_raw <- as.character(end_raw)
    start <- .parseDateText(start_raw, fmt)
    end <- .parseDateText(end_raw, fmt)
    failed_start <- .countDateParseFailures(start_raw, start)
    failed_end <- .countDateParseFailures(end_raw, end)
    two_start <- .twoDigitYears(start_raw, start)
    two_end <- .twoDigitYears(end_raw, end)
    list(start = start, end = end,
         n_failed = failed_start$n_failed + failed_end$n_failed,
         n_inputs = failed_start$n_inputs + failed_end$n_inputs,
         n_shape = length(.dateMisfits(start_raw, fmt)) + length(.dateMisfits(end_raw, fmt)),
         two_digit = if (two_start$n > 0L) c(two_start, list(total = two_start$n + two_end$n))
                     else c(two_end, list(total = two_end$n)))
}

#' Notes on how the date columns of a survival analysis were read
#'
#' @param read The value of `.readSurvivalDates()`.
#' @return A list: `shape`, a sentence to append to the unreadable-dates message
#'   ("" when no value was refused for its shape), and `two_digit`, a message
#'   (NULL when no two-digit year was read).
#' @noRd
.survivalDateNotes <- function(read) {
    . <- .callerTranslator(parent.frame())   # jmvcore's .() needs `self`; a file-level helper has none
    list(
        shape = if (read$n_shape > 0L) .fmt(
            .("{n} date value(s) have a form the selected date type cannot have, such as a time without seconds or a month and year without a day. DateTime Converter can rewrite the column in one format."),
            n = read$n_shape) else "",
        two_digit = if (read$two_digit$total > 0L) .fmt(
            .("{count} date value(s) were written with a two-digit year, and the selected date type decided which two digits are the year: for example '{raw}' was read as {parsed}. If that is not the date meant, rewrite the column with four-digit years (DateTime Converter can do this) before relying on the times below."),
            count = read$two_digit$total, raw = read$two_digit$raw, parsed = read$two_digit$parsed))
}

#' Plot x-axis extent and tick spacing in the output time unit
#'
#' `endplot` and `byplot` are numeric options, so their defaults are single
#' numbers: 60 and 12, which are five years and one year only in months. On a
#' day scale every curve was cut at day 60, and on a year scale the axis ran to
#' 60 years. An untouched factory value is therefore read as "five years" and
#' "one year" in the selected unit; any other value is the user's own. When the
#' user's own window is shorter than two years, the untouched spacing becomes a
#' fifth of the window, so the axis keeps its ticks.
#'
#' @param endplot,byplot The option values.
#' @param unit The `timetypeoutput` key.
#' @return A list: `end`, `by`.
#' @noRd
.plotWindow <- function(endplot, byplot, unit) {
    year <- switch(as.character(unit)[1], days = 365, weeks = 52, months = 12, years = 1, 12)
    endplot <- suppressWarnings(as.numeric(endplot)[1])
    byplot <- suppressWarnings(as.numeric(byplot)[1])
    end <- if (!is.finite(endplot) || endplot <= 0 || endplot == 60) 5 * year else endplot
    factory_by <- !is.finite(byplot) || byplot <= 0 || byplot == 12
    by <- if (factory_by) year else byplot
    # A window the user shortened below two years would get one tick from yearly spacing;
    # give it five intervals instead. A spacing the user entered is never changed.
    if (factory_by && by > end / 2) by <- end / 5
    list(end = end, by = by)
}
