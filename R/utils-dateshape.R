# How date text is read, shared by datetimeconverter and timeinterval so that the two
# analyses decide the same way on the same column.
#
# lubridate splits a digit run wherever that yields enough fields, so a parse success says
# little. ymd_hms reads "15/01/2020 14:30" as 2015-01-20 20:14:30: the year is cut in two and
# its tail becomes the hour. dmy_hms reads the same text with the right date and a clock of
# 20:14:30. mdy reads "12/2018" as 2018-12-20 and ydm reads "2018-12" as 2020-12-18: the
# leading digits of the year become a day that was never recorded. Each of those is a 100%
# parse, and with an all-20xx cohort every date lands in the wrong year or on an invented day.
# So a reading only counts when the text has the shape of the format.
#
# The guard removes readings that are structurally impossible and nothing else. It cannot
# settle "01/05/19", which really is ambiguous: yy/mm/dd and dd/mm/yy are the same six digits,
# and a shape rule tried on them traded one silent error for another (0/40 on the mirror shape,
# net negative over a 144-family grid). .rankDateFormats() leaves that to the year-span test,
# and the callers tell the user.

.dateFormatCandidates <- c(
    "ymd_hms", "dmy_hms", "mdy_hms",
    "ymd_hm", "dmy_hm", "mdy_hm",
    "ymd", "dmy", "mdy",
    "ydm", "myd", "dym",
    "ym", "my"
)

# A month and a four-digit year, with no day: 2018-12, 12/2018, Mar 2019, 2019 Mar.
.datePartialPattern <- paste0(
    "^((18|19|20|21)[0-9]{2}[-/. ]+([0-9]{1,2}|[[:alpha:]]{3,})",
    "|([0-9]{1,2}|[[:alpha:]]{3,})[-/., ]+(18|19|20|21)[0-9]{2})$")

# Packed YYYYMM. Unlike the above this is not surely partial: 200112 is also YYMMDD, so it
# only opens the door to ym, which sits after every full-date format in the candidates.
.datePackedMonthPattern <- "^(18|19|20|21)[0-9]{2}(0[1-9]|1[0-2])$"

.dateYearPattern <- "(?<![0-9])(18|19|20|21)[0-9]{2}(?![0-9])"

# Both spellings of the time formats: datetimeconverter's option list has ymdhms, lubridate
# and the detector have ymd_hms.
.dateParser <- function(fmt) {
    switch(fmt,
        ymd_hms = , ymdhms = lubridate::ymd_hms,
        dmy_hms = , dmyhms = lubridate::dmy_hms,
        mdy_hms = , mdyhms = lubridate::mdy_hms,
        ymd_hm = , ymdhm = lubridate::ymd_hm,
        dmy_hm = , dmyhm = lubridate::dmy_hm,
        mdy_hm = , mdyhm = lubridate::mdy_hm,
        ymd = lubridate::ymd,
        dmy = lubridate::dmy,
        mdy = lubridate::mdy,
        ydm = lubridate::ydm,
        myd = lubridate::myd,
        dym = lubridate::dym,
        ym = lubridate::ym,
        my = lubridate::my,
        NULL)
}

.dateHasTime <- function(fmt) grepl("hm", fmt, fixed = TRUE)

.isPartialDate <- function(x) grepl(.datePartialPattern, trimws(as.character(x)))

# How many clock fields the text shows: 2 for HH:MM, 3 for HH:MM:SS, NA for a packed digit run,
# where nothing separates them.
.dateClockFields <- function(x) {
    clock <- sub("[+-][0-9]{2}:[0-9]{2}$", "", x)  # a zone offset brings a colon of its own
    fields <- nchar(gsub("[^:]", "", clock)) + 1L
    fields[grepl("[ T][0-9]{4}$", clock)] <- 2L
    fields[grepl("[ T][0-9]{6}$", clock)] <- 3L
    fields[grepl("^[0-9]+$", clock)] <- NA_integer_
    fields
}

# A standalone 18xx-21xx token is the year as written, so the reading must keep it. Any token
# may match, because 15/01/2020 2030 also has a clock in it. A value carrying a zone offset is
# skipped: it is converted, and 2020-12-31T23:30-05:00 is honestly in 2021.
.dateYearKept <- function(x, parsed) {
    tokens <- regmatches(x, gregexpr(.dateYearPattern, x, perl = TRUE))
    years <- base::format(parsed, "%Y")
    kept <- as.logical(mapply(function(t, y) length(t) == 0L || is.na(y) || y %in% t,
                              tokens, years, USE.NAMES = FALSE))
    kept | grepl(":[0-9]{2}(\\.[0-9]+)? ?[+-][0-9]{2}(:?[0-9]{2})?$", x)
}

# TRUE where the text can be a value of `fmt`, FALSE where `parsed` could only have come from
# splitting digits in the wrong place or inventing a day.
.dateShapeFits <- function(x, fmt, parsed) {
    x <- trimws(as.character(x))
    partial <- grepl(.datePartialPattern, x)
    fits <- if (fmt %in% c("ym", "my")) partial | grepl(.datePackedMonthPattern, x) else !partial
    if (fmt %in% c("ydm", "myd", "dym"))
        fits <- fits & !grepl("^[0-9]{5,6}$", x)  # nobody packs a date with the year inside
    if (.dateHasTime(fmt)) {
        fields <- .dateClockFields(x)
        fits <- fits & (is.na(fields) | fields == (if (endsWith(fmt, "hms")) 3L else 2L))
    }
    fits & .dateYearKept(x, parsed)
}

# Read x under fmt. A date-only format returns a Date, a time format a POSIXct in tz.
.parseDateText <- function(x, fmt, tz = "UTC") {
    parser <- .dateParser(fmt)
    parsed <- suppressWarnings(
        if (.dateHasTime(fmt)) parser(x, quiet = TRUE, tz = tz) else parser(x, quiet = TRUE))
    parsed[!.dateShapeFits(x, fmt, parsed)] <- NA
    parsed
}

# Positions the shape refuses, whether the lenient parser read them or a day is simply absent,
# so the caller can say why they came back empty instead of counting them as missing.
.dateMisfits <- function(x, fmt) {
    parsed <- suppressWarnings(.dateParser(fmt)(x, quiet = TRUE))
    which(!.dateShapeFits(x, fmt, parsed) & (!is.na(parsed) | .isPartialDate(x)))
}

# Up to `cap` non-missing values, keeping those with a 13..31 token, so the dates that settle
# day/month order are never dropped behind a prefix of day <= 12 dates.
.dateSample <- function(v, cap = 500L) {
    if (length(v) <= cap) return(v)
    high_day <- grepl("(^|[^0-9])(1[3-9]|2[0-9]|3[01])([^0-9]|$)", as.character(v))
    v[sort(utils::head(c(which(high_day), which(!high_day)), cap))]
}

# Put back a lost leading zero on packed digit strings. 50321 is DDMMYY 050321 only when the
# column shows the zero went missing: at least `serial_share` five-digit values is a serial day
# count, and reading 42370 as 04/23/70 turned those into April dates. 0.8 is the test timeinterval
# refuses on; a caller that re-reads the result under a format the user chose can go to 1.
# Seven digits are no serial.
.restoreLeadingZeros <- function(x, serial_share = 0.8) {
    digits <- !is.na(x) & grepl("^[0-9]+$", x)
    if (!all(digits[!is.na(x)]) || !any(digits)) return(x)
    width <- nchar(x[digits])
    # three digits: YYMMDD 000105 in a column that crosses 2000
    target <- if (all(width %in% 3:6)) 6L else if (all(width %in% 7:8)) 8L else return(x)
    if (target == 6L && mean(width == 5L) >= serial_share) return(x)
    x[digits] <- paste0(strrep("0", target - width), x[digits])
    x
}

# Score every candidate on the same columns (one for a converter, start and end for an
# interval, which need a single format) and rank them. A format scores its weakest column.
#
# Ties are broken by DEMOTING any reading whose years span more than 20, which is what reading a
# day-of-month as a two-digit year does: "22/09/18" under ymd scatters a cohort over 1..31,
# where a real cohort spans a few years. Candidate order decides among the survivors. It is
# deliberately demote-only. The tempting "narrowest span wins" is really "the field that varies
# least is the year", and that is wrong for month-start columns: "16-01-01" written yy-mm-dd
# becomes 2001-01-16, every row pinned to 2001, mean 4.0 months against a truth of about 20.
#
# A true dd/mm against mm/dd tie has the same years under both readings, so neither is demoted
# and `tied` still has both: the caller must flag it.
#
# @return list(format, tied, order, scores, parsed), `parsed` by format then by column.
.rankDateFormats <- function(columns) {
    read <- function(fmt) lapply(columns, function(v) tryCatch(
        .parseDateText(v, fmt), error = function(e) rep(as.POSIXct(NA), length(v))))
    parsed <- lapply(stats::setNames(.dateFormatCandidates, .dateFormatCandidates), read)
    scores <- vapply(parsed, function(p) min(vapply(p, function(v)
        if (length(v)) mean(!is.na(v)) else 0, numeric(1))), numeric(1))

    tied <- names(scores)[scores >= max(scores) - 1e-9]
    if (length(tied) > 1) {
        span <- vapply(tied, function(fmt) {
            y <- unlist(lapply(parsed[[fmt]], function(v) lubridate::year(v[!is.na(v)])))
            if (length(y) == 0) Inf else as.numeric(diff(range(y)))
        }, numeric(1))
        plausible <- tied[span <= 20]
        if (length(plausible) > 0) tied <- plausible
    }
    # A leading four-digit year is ISO: when ymd fits, ydm is not a second opinion.
    if (all(c("ymd", "ydm") %in% tied)) {
        text <- trimws(unlist(lapply(columns, as.character)))
        if (all(grepl("^[0-9]{4}([-/. ]|[0-9]{4}$)", text)))
            tied <- setdiff(tied, "ydm")
    }

    list(format = tied[1], tied = tied, scores = scores, parsed = parsed,
         order = c(tied, setdiff(names(sort(scores, decreasing = TRUE)), tied)))
}
