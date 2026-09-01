library(testthat)

# Regressions for the deep-audit pass on `datetimeconverter`.
#
# Each block pins one defect that shipped. The names state the failure mode so a
# future breakage is self-describing, and several blocks pin the ABSENCE of a
# regression that a plausible-looking version of the fix would have introduced.
#
# NOTE ON format(): ClinicoPath does a blanket `import(jmvcore)` and jmvcore
# exports its own format() which ignores format strings. Always base::format here.

fmt <- base::format

quietly <- function(expr) {
    f <- tempfile(); sink(f); on.exit(sink(), add = TRUE); suppressWarnings(force(expr))
}
strip_html <- function(x) {
    gsub("[[:space:]]+", " ", gsub("<[^>]*>", " ", paste(as.character(x), collapse = " ")))
}

# Drive the analysis object directly: `Output` options never reach the public
# wrapper, and the private helpers are the only way to observe the parsed vector.
dtc_obj <- function(data, var, ...) {
    opts <- datetimeconverterOptions$new(datetime_var = var, ...)
    an <- datetimeconverterClass$new(options = opts, data = data)
    quietly(an$run())
    an
}
dtc_priv <- function() {
    datetimeconverterClass$new(
        options = datetimeconverterOptions$new(datetime_var = "v"),
        data = data.frame(v = 1))$.__enclos_env__$private
}
dtc_notices <- function(data, var, ...) strip_html(dtc_obj(data, var, ...)$results$notices$content)
dtc_detect <- function(x) quietly(dtc_priv()$.detectDatetimeFormat(x))
# values actually stored in an Output result item
dtc_vals <- function(an, name) {
    v <- try(an$results[[name]]$.__enclos_env__$private$.values, silent = TRUE)
    if (inherits(v, "try-error") || is.null(v) || length(v) == 0) numeric(0) else v[[1]]
}


# --- Format detection: tie-break ---------------------------------------------
# sort() is stable, so ties were broken by position in formats_to_try, where ymd
# precedes dmy. Every DD/MM/YY column is a 100%-vs-100% tie, so ymd always won and
# read the day-of-month as the year: "05/03/21" -> 2005-03-21, ~16 years out.

test_that("a two-digit-year day-first column is no longer read as ymd", {
    x <- c("05/03/21", "17/07/19", "28/11/20", "02/02/21", "30/09/18", "11/12/20")
    det <- dtc_detect(x)
    expect_equal(det$format, "dmy")
    parsed <- suppressWarnings(dtc_priv()$.getParser(det$format)(x))
    expect_equal(fmt(as.Date(parsed[1])), "2021-03-05")   # was 2005-03-21
})

test_that("the tie-break does NOT regress year-first two-digit columns", {
    # The tempting rule -- "prefer the narrowest year span" -- is really "the field
    # that varies least is the year", which is WRONG whenever the day-of-month is
    # more clustered than the cohort. Quarterly / annual / month-start columns
    # written yy-mm-dd are exactly that shape, and promote-the-narrowest turns
    # "16-01-01" into 2001-01-16. Only implausibly wide readings are demoted.
    quarterly <- fmt(as.Date(paste0(rep(2016:2021, each = 4), "-",
                                    rep(c("01", "04", "07", "10"), 6), "-01")), "%y-%m-%d")
    det <- dtc_detect(quarterly)
    expect_equal(det$format, "ymd")
    expect_equal(fmt(as.Date(suppressWarnings(
        dtc_priv()$.getParser(det$format)(quarterly))[1])), "2016-01-01")

    annual <- fmt(as.Date(paste0(2005:2021, "-01-01")), "%y-%m-%d")
    expect_equal(dtc_detect(annual)$format, "ymd")
})

test_that("the tie-break leaves every unambiguous family alone", {
    expect_equal(dtc_detect(c("2021-03-05", "2021-04-17", "2021-06-22", "2021-08-30"))$format, "ymd")
    expect_equal(dtc_detect(c("25/03/2021", "17/04/2021", "28/06/2021", "19/08/2021"))$format, "dmy")
    expect_equal(dtc_detect(c("03/25/2021", "04/17/2021", "06/28/2021", "08/19/2021"))$format, "mdy")
    expect_equal(dtc_detect(c("2021-03-05 14:30:00", "2021-04-17 09:15:00"))$format, "ymd_hms")
    # a genuinely wide birth-date column must still resolve day-first
    expect_equal(dtc_detect(c("14/02/1931", "03/11/1948", "27/07/1962", "19/09/2005"))$format, "dmy")
})

test_that("a genuine day/month ambiguity is still reported", {
    det <- dtc_detect(c("05/03/2021", "07/04/2021", "09/06/2021", "11/08/2021"))
    expect_true(length(det$warnings) > 0)
})


# --- Excel epoch -------------------------------------------------------------
# excel_like is tested first and matches every all-non-negative serial set, so the
# excel_1904_like branch was reachable ONLY via a negative value. A genuine Mac
# 1904 file was therefore read on the 1900 origin -- 1462 days (~4 years) out --
# and labelled "1900 system", with no override available anywhere in the UI.

test_that("the 1904 override converts on the 1904 origin", {
    p <- dtc_priv()
    x <- c(42798, 42800, 42830)
    r04 <- quietly(p$.processNumericVector(x, character(), x, force_format = "excel_serial_1904"))
    r00 <- quietly(p$.processNumericVector(x, character(), x, force_format = "excel_serial"))

    expect_equal(r04$format_hint, "excel_serial_1904")
    expect_equal(fmt(as.Date(r04$parsed_dates[1])), "2021-03-05")
    expect_equal(fmt(as.Date(r00$parsed_dates[1])), "2017-03-04")
    expect_equal(as.numeric(difftime(r04$parsed_dates[1], r00$parsed_dates[1], units = "days")), 1462)
})

test_that("the 1904 option exists in the analysis definition", {
    # Needs a regenerated .h.R; the option is what makes the override reachable at all.
    a <- yaml::read_yaml(testthat::test_path("..", "..", "jamovi", "datetimeconverter.a.yaml"))
    f <- Filter(function(o) identical(o$name, "datetime_format"), a$options)[[1]]
    expect_true("excel_serial_1904" %in% vapply(f$options, function(o) o$name, character(1)))
})

test_that("the 1900 assumption is disclosed on a real date column", {
    n <- dtc_notices(data.frame(s = c(44000, 44100, 45000)), "s")
    expect_match(n, "Excel Serial Origin Assumed")
    expect_match(n, "1462 days")
})

test_that("the epoch notice does not fire on a measurement column", {
    # Ungated it asserted the values ARE Excel serials and offered the wrong epoch
    # as the only alternative -- to someone who had mis-selected an age column.
    n <- dtc_notices(data.frame(age = c(34, 55, 72, 61)), "age")
    expect_false(grepl("Excel Serial Origin Assumed", n))
})

test_that("the 1900 conversion still matches the bundled expected dates", {
    # 40 rows with a stated ground truth that no existing test compared against.
    skip_if_not(file.exists(testthat::test_path("..", "..", "data", "datetimeconverter_excel.rda")))
    e <- new.env()
    load(testthat::test_path("..", "..", "data", "datetimeconverter_excel.rda"), envir = e)
    d <- e$datetimeconverter_excel
    r <- quietly(dtc_priv()$.processNumericVector(
        d$excel_serial_date, character(), d$excel_serial_date, force_format = "excel_serial"))
    expect_equal(as.Date(r$parsed_dates), as.Date(d$expected_date))
})

test_that("a negative sentinel no longer decides the epoch or void the column", {
    # -99 / -1 are ordinary missing sentinels. They were the ONLY route into the
    # 1904 branch, so one of them silently reinterpreted the whole column; with
    # that branch removed they would instead push it out of excel_like entirely.
    p <- dtc_priv()
    x <- c(-99, 42798, 45000, 44000)
    r <- quietly(p$.processNumericVector(x, character(), x))
    expect_equal(r$format_hint, "excel_serial")
    expect_true(is.na(r$parsed_dates[1]))
    expect_equal(sum(!is.na(r$parsed_dates)), 3L)
    expect_equal(fmt(as.Date(r$parsed_dates[2])), "2017-03-04")
})


# --- Numeric columns that are not dates --------------------------------------
# excel_like accepts [0, 600000] and .detectMisuse only looked for year < 1900,
# future dates and >100-year spans, so serials 2..55000 landed in 1900-2050 and
# were converted with no word to the user.

test_that("a small-serial numeric column is flagged as probably not dates", {
    n <- dtc_notices(data.frame(age = c(20, 35, 47, 58, 66, 72, 81, 90)), "age")
    expect_match(n, "Numeric Column May Not Be Dates")
    expect_match(n, "1900-03-30")
    expect_match(n, "1900 or earlier")
})

test_that("the previously-silent window is now flagged across its width", {
    for (v in c(2, 50, 120, 3650, 9999))
        expect_match(dtc_notices(data.frame(x = c(0, v)), "x"),
                     "Numeric Column May Not Be Dates", info = paste("max =", v))
})

test_that("genuine clinical Excel serials are not flagged", {
    for (v in list(44000, c(20000, 25000), c(43101, 45657), c(12800, 21900)))
        expect_false(grepl("Numeric Column May Not Be Dates", dtc_notices(data.frame(x = v), "x")),
                     info = paste("serials", paste(v, collapse = "-")))
})

test_that("notices do not accumulate across run cycles", {
    an <- datetimeconverterClass$new(
        options = datetimeconverterOptions$new(datetime_var = "age"),
        data = data.frame(age = c(20, 50, 90)))
    quietly(an$run()); quietly(an$run())
    n <- strip_html(an$results$notices$content)
    expect_equal(lengths(regmatches(n, gregexpr("Numeric Column May Not Be Dates", n))), 1L)
})


# --- Output columns ----------------------------------------------------------
# Every write was gated on `self$options$<name> && isNotFilled()`. An Output option
# is not a wrapper argument (the .h.R signature ends at show_glossary), so it is
# permanently FALSE from the R API and all 13 columns were dead outside the GUI.

test_that("output columns are reachable without the GUI", {
    d <- data.frame(v = c("2021-01-02 03:04:05", NA, "2023-11-14 22:13:20"), stringsAsFactors = FALSE)
    an <- dtc_obj(d, "v", datetime_format = "ymdhms")
    expect_equal(length(dtc_vals(an, "corrected_datetime_char")), 3L)     # was 0
    expect_equal(length(dtc_vals(an, "corrected_datetime_numeric")), 3L)  # was 0

    an2 <- dtc_obj(d, "v", datetime_format = "ymdhms", extract_year = TRUE, extract_day = TRUE)
    expect_equal(as.numeric(dtc_vals(an2, "year_out")), c(2021, NA, 2023))
    expect_equal(as.numeric(dtc_vals(an2, "day_out")), c(2, NA, 14))
})

test_that("an unrequested component column stays empty despite $ partial matching", {
    # `components$day` PARTIAL-MATCHES `components$dayname` when `day` is absent, so
    # ticking extract_dayname alone wrote weekday indices (7, NA, 3) into a column
    # labelled "Day of month (1-31)". Exact [["day"]] indexing is what stops it.
    d <- data.frame(v = c("2021-01-02 03:04:05", NA, "2023-11-14 22:13:20"), stringsAsFactors = FALSE)

    an <- dtc_obj(d, "v", datetime_format = "ymdhms", extract_dayname = TRUE)
    expect_equal(as.character(dtc_vals(an, "dayname_out")), c("Saturday", NA, "Tuesday"))
    expect_equal(length(dtc_vals(an, "day_out")), 0L)

    an2 <- dtc_obj(d, "v", datetime_format = "ymdhms", extract_monthname = TRUE)
    expect_equal(length(dtc_vals(an2, "month_out")), 0L)
})

test_that("every output column is either empty or exactly as long as its row numbers", {
    # The guard against a naive version of the fix, which stored a zero-length
    # vector against a full-length setRowNums().
    d <- data.frame(v = c("2021-01-02 03:04:05", NA, "2023-11-14 22:13:20"), stringsAsFactors = FALSE)
    an <- dtc_obj(d, "v", datetime_format = "ymdhms", extract_year = TRUE, extract_dayname = TRUE)
    for (n in c("corrected_datetime_char", "corrected_datetime_numeric", "year_out", "month_out",
                "monthname_out", "day_out", "hour_out", "minute_out", "second_out",
                "dayname_out", "weeknum_out", "quarter_out", "dayofyear_out")) {
        L <- length(dtc_vals(an, n))
        rn <- length(an$results[[n]]$.__enclos_env__$private$.rowNums)
        expect_true(L == 0L || L == rn, info = paste(n, L, "vs", rn))
    }
})


# --- Smaller repairs ---------------------------------------------------------

test_that("a numeric format selected on a text column explains itself", {
    # .getParser() has no parser for excel_serial / unix_epoch, and its call sits
    # outside .parseDatetime's tryCatch, so the commonest mis-click there is
    # surfaced as `Error in private$.getParser(format)`.
    n <- dtc_notices(data.frame(ts = c("2023-01-15", "2023-02-20"), stringsAsFactors = FALSE),
                     "ts", datetime_format = "unix_epoch")
    expect_match(n, "Numeric Format Selected For A Text Column")
})

test_that("auto-detection returns unsure rather than falling back to ymd", {
    # The docstring claimed a "ymd" fallback; the code returns "unsure" and .run()
    # skips parsing. The comment was corrected to match the code.
    expect_equal(dtc_detect(c("not", "a", "date"))$format, "unsure")
})

test_that("notice titles carry no stray leading space", {
    n <- dtc_obj(data.frame(age = c(20, 50, 90)), "age")$results$notices$content
    expect_true(grepl("<strong[^>]*>[A-Z]", n))
    expect_false(grepl("<strong[^>]*> ", n))
})

test_that("timezone is in the clearWith of every panel that shows parsed values", {
    r <- yaml::read_yaml(testthat::test_path("..", "..", "jamovi", "datetimeconverter.r.yaml"))
    get <- function(nm) Filter(function(i) identical(i$name, nm), r$items)[[1]]
    for (nm in c("notices", "formatInfo", "qualityMetrics", "previewTable"))
        expect_true("timezone" %in% get(nm)$clearWith, info = nm)
    expect_true("preview_rows" %in% get("previewTable")$clearWith)
    # and no Output item lists itself
    for (i in r$items)
        if (identical(i$type, "Output"))
            expect_false(i$name %in% (i$clearWith %||% character()), info = i$name)
})

test_that("every private method is defined at the same indent", {
    src <- readLines(testthat::test_path("..", "..", "R", "datetimeconverter.b.R"))
    expect_equal(sum(grepl("^            \\.[a-zA-Z]+ = function", src)), 0L)
    expect_true(sum(grepl("^        \\.[a-zA-Z]+ = function", src)) > 20L)
})

test_that("the backend source stays pure ASCII", {
    # Notice bullets must be written as the \\u2022 escape, never a literal U+2022:
    # R CMD check reports non-ASCII strings and the package stops being portable.
    src <- readLines(testthat::test_path("..", "..", "R", "datetimeconverter.b.R"), warn = FALSE)
    expect_equal(sum(grepl("[^\x01-\x7f]", src)), 0L)
})


# --- Found by adversarial verification of the fixes above ---------------------

test_that("pre-1970 Unix timestamps survive the negative mask", {
    # The mask exists only to protect the EXCEL epoch decision, but it ran before the
    # unix_epoch branch, so every negative epoch second -- i.e. every date before
    # 1970 -- became NA. Half a date-of-birth column vanished and the module then
    # advised "try a different datetime format", when the chosen format was correct.
    v <- as.numeric(as.POSIXct(c("1955-03-02", "1962-11-20", "1975-06-15", "1981-01-09"),
                               tz = "UTC"))
    r <- quietly(dtc_priv()$.processNumericVector(v, character(), v, force_format = "unix_epoch"))
    expect_equal(sum(is.na(r$parsed_dates)), 0L)
    expect_equal(fmt(as.Date(r$parsed_dates)),
                 fmt(as.Date(as.POSIXct(v, origin = "1970-01-01", tz = "UTC"))))
})

test_that("the Excel path still masks negative serials", {
    # The exemption above must not reopen the 1904 hole.
    x <- c(-99, 42798, 45000, 44000)
    r <- quietly(dtc_priv()$.processNumericVector(x, character(), x))
    expect_equal(r$format_hint, "excel_serial")
    expect_equal(sum(is.na(r$parsed_dates)), 1L)
    expect_equal(fmt(as.Date(r$parsed_dates[2])), "2017-03-04")
})

test_that("implausibly large numeric columns are flagged too", {
    # The "may not be dates" rule had only a lower bound, so a platelet-count column
    # converted to the year 2310 while the headline notice reassured the user that
    # the values WERE Excel serials.
    n <- dtc_notices(data.frame(plt = c(150000, 220000, 310000, 450000)), "plt")
    expect_match(n, "Numeric Column May Not Be Dates")
    expect_false(grepl("Excel Serial Origin Assumed", n))
})

test_that("no numeric column falls between the two notices", {
    # min < 10000 with max >= 10000 satisfies neither gate directly; the >100-year
    # span check is what covers it.
    n <- dtc_notices(data.frame(x = c(5, 43831, 45292)), "x")
    expect_true(nzchar(trimws(n)))
    expect_match(n, "more than 100 years")
})

test_that("a clean run clears the notices pane instead of keeping the last one", {
    an <- datetimeconverterClass$new(
        options = datetimeconverterOptions$new(datetime_var = "v", datetime_format = "ymd"),
        data = data.frame(v = c("2021-01-02", "2022-02-03"), stringsAsFactors = FALSE))
    quietly(an$run())
    quietly({
        an$.__enclos_env__$private$.noticeList <- list()
        an$.__enclos_env__$private$.renderNotices()
    })
    expect_equal(nchar(an$results$notices$content), 0L)
})

test_that("every format token the module can carry has a human label", {
    p <- dtc_priv()
    for (f in c("excel_serial", "excel_serial_1904", "unix_epoch", "ymd", "dmy", "mdy"))
        expect_false(identical(p$.formatLabel(f), toupper(f)), info = f)
})

test_that("character row names do not become NA row numbers", {
    # jmvcore's setRowNums() does a bare as.integer(), which silently NAs them.
    d <- data.frame(v = c("2021-01-02", "2023-11-14"), stringsAsFactors = FALSE)
    rownames(d) <- c("a", "b")
    an <- dtc_obj(d, "v", datetime_format = "ymd", extract_year = TRUE)
    expect_false(anyNA(an$results$year_out$.__enclos_env__$private$.rowNums))
})

test_that("a six-digit two-digit-year column is reported as ambiguous, not guessed", {
    # "01/05/19" is EITHER 2001-05-19 (yy/mm/dd) or 2019-05-01 (dd/mm/yy) -- the same
    # string. A rule demoting year-first readings of slash/dot strings was tried and
    # reverted: it fixed the dd/mm/yy half and broke the yy/mm/dd half by exactly the
    # same margin (net negative over a 144-family grid). What matters is that neither
    # reading is applied silently.
    set.seed(5)
    reg <- as.Date(sprintf("%d-%02d-01", sample(2013:2019, 24, TRUE), sample(1:12, 24, TRUE)))
    for (f in c("%d/%m/%y", "%d.%m.%y", "%y/%m/%d")) {
        det <- dtc_detect(fmt(reg, f))
        expect_true(length(det$warnings) > 0, info = f)
    }

    # the message must show BOTH readings with a value from the user's own data, and
    # must not claim a day/month swap when the disagreement is year-vs-day
    n <- dtc_notices(data.frame(v = fmt(reg, "%d/%m/%y"), stringsAsFactors = FALSE), "v")
    expect_match(n, "can be read two ways")
    expect_match(n, "gives")
    expect_false(grepl("Ambiguous day/month order", n))
})

test_that("a genuine year-first two-digit column is still read year-first", {
    # This is what the reverted rule broke, so it is pinned explicitly.
    set.seed(11)
    d <- as.Date("2015-01-01") + sample(0:2900, 40, TRUE)
    for (f in c("%y/%m/%d", "%y.%m.%d", "%y-%m-%d")) {
        det <- dtc_detect(fmt(d, f))
        expect_equal(det$format, "ymd", info = f)
        expect_equal(as.Date(suppressWarnings(dtc_priv()$.getParser(det$format)(fmt(d, f)))), d,
                     info = f)
    }
    # dash-delimited quarterly/annual keys, the case the year-span rule protects
    quarterly <- fmt(as.Date(paste0(rep(2016:2021, each = 4), "-",
                                    rep(c("01", "04", "07", "10"), 6), "-01")), "%y-%m-%d")
    expect_equal(dtc_detect(quarterly)$format, "ymd")
    expect_equal(dtc_detect(fmt(as.Date("2021-01-01") + 0:30, "%Y/%m/%d"))$format, "ymd")
})


# --- Found by the code review ------------------------------------------------

test_that("Excel time-of-day is rounded to the second", {
    # An Excel serial's fraction is a binary double: 00:05 is stored as
    # 45000.003472222219, and *86400 lands at 00:04:59.999999 -- one minute LOW once
    # the minute is extracted. 160 of 1440 minutes were wrong and the extracted
    # second reached 60, against a varDescription promising 0-59. Hours were never
    # wrong, which is why it survived casual testing.
    p <- datetimeconverterClass$new(
        options = datetimeconverterOptions$new(datetime_var = "v", extract_hour = TRUE,
                                               extract_minute = TRUE, extract_second = TRUE),
        data = data.frame(v = 1))$.__enclos_env__$private
    sers <- 45000 + (0:1439) / 1440
    for (f in c("excel_serial", "excel_serial_1904")) {
        r <- quietly(p$.processNumericVector(sers, character(), sers, force_format = f))
        cm <- quietly(p$.extractComponents(r$parsed_dates))
        expect_equal(cm$minute, (0:1439) %% 60, info = f)
        expect_equal(cm$hour, (0:1439) %/% 60, info = f)
        expect_lte(max(cm$second), 59)
    }
})

test_that("the timezone option is honoured on POSIXct input", {
    # as_datetime() preserves the incoming tzone, so the option had NO effect on this
    # branch while the Summary went on asserting "Timezone: UTC". At day boundaries
    # that is the wrong DATE, not just the wrong hour.
    x <- as.POSIXct(c("2024-03-15 09:30:00", "2024-07-02 01:15:00"), tz = "Europe/Istanbul")
    get <- function(tzv) {
        an <- dtc_obj(data.frame(v = x), "v", timezone = tzv,
                      extract_hour = TRUE, extract_day = TRUE)
        p <- an$.__enclos_env__$private
        quietly(p$.extractComponents(quietly(p$.prepareDatetimeInput(x))$parsed_dates))
    }
    utc <- get("UTC")
    expect_equal(as.numeric(utc$hour), c(6, 22))
    expect_equal(as.numeric(utc$day), c(15, 1))     # the second row is 1 July in UTC

    ist <- get("Europe/Istanbul")
    expect_equal(as.numeric(ist$hour), c(9, 1))
    expect_equal(as.numeric(ist$day), c(15, 2))
})

test_that("a wide-day dd/mm/yy column round-trips through a follow-up calculation", {
    # The year-span demotion (kept) handles the case where the wrong reading scatters
    # the years. Clustered-day columns remain ambiguous by construction and are warned
    # about instead -- see the ambiguity test above.
    set.seed(5)
    surg <- as.Date("2016-01-01") + sample(0:900, 40, TRUE)
    fu   <- surg + sample(200:1900, 40, TRUE)
    conv <- function(d) {
        x <- fmt(d, "%d/%m/%y"); det <- dtc_detect(x)
        as.Date(suppressWarnings(dtc_priv()$.getParser(det$format)(x)))
    }
    expect_equal(conv(surg), surg)
    expect_equal(conv(fu), fu)
    expect_equal(as.numeric(conv(fu) - conv(surg)), as.numeric(fu - surg))
})

test_that("two-digit years can be forced into the past", {
    # lubridate pivots 00-68 to 20xx with no way to change it, so a dd/mm/yy
    # date-of-birth column puts every birth before 1969 exactly 100 years ahead.
    set.seed(5)
    dob <- as.Date("1935-01-01") + sample(0:14600, 200, TRUE)
    d <- data.frame(v = fmt(dob, "%d/%m/%y"), stringsAsFactors = FALSE)
    got <- function(mode) {
        an <- dtc_obj(d, "v", two_digit_year = mode, corrected_datetime_char = TRUE)
        as.Date(substr(dtc_vals(an, "corrected_datetime_char"), 1, 10))
    }
    expect_true(sum(got("standard") != dob) > 100)   # the defect, still the default
    expect_equal(got("past"), dob)                   # the remedy

    # a four-digit-year column must never be touched by the setting
    d4 <- data.frame(v = fmt(dob, "%d/%m/%Y"), stringsAsFactors = FALSE)
    an <- dtc_obj(d4, "v", two_digit_year = "past", corrected_datetime_char = TRUE)
    expect_equal(as.Date(substr(dtc_vals(an, "corrected_datetime_char"), 1, 10)), dob)
})

test_that("a century-pivot column is escalated and names the real cause", {
    set.seed(5)
    dob <- as.Date("1935-01-01") + sample(0:14600, 200, TRUE)
    n <- dtc_notices(data.frame(v = fmt(dob, "%d/%m/%y"), stringsAsFactors = FALSE), "v")
    expect_match(n, "Wrong Century")
    expect_match(n, "two-digit years")
    expect_match(n, "Always in the past")
    # and it must NOT still steer the reader to the benign explanation
    expect_false(grepl("planned follow-up dates", n))
})

test_that("percentages use one denominator and the copy-ready text is true", {
    d <- data.frame(v = c(fmt(as.Date("2021-01-01") + 0:49), rep(NA_character_, 50)),
                    stringsAsFactors = FALSE)
    an <- dtc_obj(d, "v", datetime_format = "ymd", show_summary = TRUE,
                  show_quality_metrics = TRUE)
    s  <- strip_html(an$results$nlSummary$content)
    qa <- strip_html(an$results$qualityAssessment$content)

    expect_match(s, "50/50 non-missing")            # was "50/100 (100%)"
    expect_match(s, "from 50 datetime values")      # was "from 100"
    expect_match(s, "50 of the 100 rows were missing")
    expect_match(qa, "Successful Parses 50 50%")    # column now sums to 100%
})

test_that("the parse rate is not presented as a quality grade when dates are in doubt", {
    amb <- dtc_obj(data.frame(v = c("05/03/2021", "07/04/2021", "09/06/2021", "11/08/2021"),
                              stringsAsFactors = FALSE), "v", show_quality_metrics = TRUE)
    q <- strip_html(amb$results$qualityAssessment$content)
    expect_match(q, "Parse rate")
    expect_false(grepl("Excellent", q))
    expect_match(q, "read in the wrong format still parses")

    clean <- dtc_obj(data.frame(v = fmt(as.Date("2021-01-01") + 0:30), stringsAsFactors = FALSE),
                     "v", datetime_format = "ymd", show_quality_metrics = TRUE)
    expect_match(strip_html(clean$results$qualityAssessment$content), "Excellent")
})

test_that("a format matching exactly 80 percent is accepted", {
    # Strict > 0.8 discarded 32 of 40 valid ISO dates and converted nothing. 40 is the
    # sample cap, so 32/40 is very reachable (as are 4/5, 8/10, 16/20).
    mk <- function(k) c(fmt(as.Date("2021-01-01") + seq_len(k)), rep("junk", 40 - k))
    expect_equal(dtc_detect(mk(32))$format, "ymd")
    expect_equal(dtc_detect(mk(31))$format, "unsure")
    # and the failure message now names the closest candidate and its rate
    expect_match(dtc_notices(data.frame(v = mk(31)), "v"), "closest match")
})

test_that("the preview Status column shows a visible verdict", {
    an <- dtc_obj(data.frame(v = c("2021-03-05", "not a date", NA), stringsAsFactors = FALSE),
                  "v", datetime_format = "ymd")
    h <- an$results$previewTable$content
    expect_match(h, ">OK<")
    expect_match(h, ">Failed<")
    expect_false(grepl("color: #28a745;'></span>", h, fixed = TRUE))
})

test_that("no output panel paints an opaque light background", {
    # jamovi renders in the viewer's theme; an opaque light fill with cells that
    # inherit the theme's text colour is light-on-white in dark mode. Also guards the
    # quality table's row highlighting, which emitted a malformed style attribute
    # (its colour literals carried their own quotes) and so did nothing at all.
    an <- dtc_obj(data.frame(v = c("2021-03-05", "not a date", NA), stringsAsFactors = FALSE),
                  "v", datetime_format = "ymd", show_quality_metrics = TRUE,
                  show_explanations = TRUE, show_glossary = TRUE)
    html <- paste(vapply(c("previewTable", "componentPreview", "qualityAssessment",
                           "nlSummary", "aboutPanel", "caveatsPanel", "glossaryPanel",
                           "formatInfo", "welcome"),
                         function(n) an$results[[n]]$content %||% "", character(1)),
                  collapse = "")
    expect_false(grepl("background-color: *#(ffffff|f8f9fa|ffebee|fff3cd|f8d7da|d4edda)", html))
    expect_false(grepl("background-color: '[^']*''", html))   # malformed attribute
})

test_that("week number and the Excel epoch are disclosed in the GUI, not only the R help", {
    an <- dtc_obj(data.frame(v = c("2021-03-05", "2021-06-09"), stringsAsFactors = FALSE),
                  "v", show_glossary = TRUE, show_explanations = TRUE)
    expect_match(an$results$glossaryPanel$content, "Week number")
    expect_match(an$results$glossaryPanel$content, "NOT the ISO-8601 week")
    expect_match(an$results$caveatsPanel$content, "ISO-8601")
    expect_match(an$results$caveatsPanel$content, "1969-2068")
})

test_that("user-visible backend strings are translatable", {
    src <- readLines(testthat::test_path("..", "..", "R", "datetimeconverter.b.R"), warn = FALSE)
    # every notice title and content goes through .() / .fmt()
    expect_equal(sum(grepl('title = "', src, fixed = TRUE)), 0L)
    expect_equal(sum(grepl("content = sprintf(", src, fixed = TRUE)), 0L)
    expect_true(sum(grepl("\\.\\(", src)) > 25)
    # and the rendered English is still correct, with placeholders substituted
    n <- dtc_notices(data.frame(v = c("2021-03-05", "2021-06-09"), stringsAsFactors = FALSE),
                     "v", datetime_format = "ymd")
    expect_match(n, "Processed 2 rows from variable v")
    expect_false(grepl("\\{[a-z_]+\\}", n))
})

test_that("the two-digit-year flag does not leak between runs", {
    # It is assigned only on the text-parsing branch, so a later run on a POSIXct or
    # Excel-serial column never reaches the assignment; without a reset it would inherit
    # the previous run's TRUE and blame a century pivot on a column with no two-digit
    # years. The analysis object persists across run cycles in jamovi.
    an <- datetimeconverterClass$new(
        options = datetimeconverterOptions$new(datetime_var = "v"),
        data = data.frame(v = as.POSIXct("2024-03-15 09:30:00", tz = "UTC")))
    an$.__enclos_env__$private$.twoDigitYearSource <- TRUE
    quietly(an$run())
    expect_false(an$.__enclos_env__$private$.twoDigitYearSource)
})

test_that("a notice reached twice in one run is shown once", {
    # .resolveTimezone() is called from both .prepareDatetimeInput() and .run(), so the
    # POSIXct path raised "Invalid Timezone" twice, which reads as two problems.
    for (d in list(data.frame(v = as.POSIXct(c("2024-03-15 09:30:00", "2024-06-01 12:00:00"), tz = "UTC")),
                   data.frame(v = c("2024-03-15", "2024-06-01"), stringsAsFactors = FALSE))) {
        n <- dtc_notices(d, "v", timezone = "Mars/Olympus")
        hits <- gregexpr("Invalid Timezone", n)[[1]]
        expect_equal(length(hits[hits > 0]), 1L)
    }
})


# --- Found by adversarially verifying the review fixes ------------------------

test_that("the two-digit-year option is in every clearWith that lists timezone", {
    # It changes the PARSED VALUE exactly as timezone does. Without it jmvcore never
    # stales the Output items, so the panels updated while the written-back column kept
    # the pre-change dates - two result items contradicting, with the durable one wrong.
    r <- yaml::read_yaml(testthat::test_path("..", "..", "jamovi", "datetimeconverter.r.yaml"))
    n_out <- 0L
    for (it in r$items) {
        cw <- it$clearWith %||% character()
        if ("timezone" %in% cw) expect_true("two_digit_year" %in% cw, info = it$name)
        if (identical(it$type, "Output") && "two_digit_year" %in% cw) n_out <- n_out + 1L
    }
    expect_equal(n_out, 13L)
})

test_that("'Always in the past' says so when it cannot apply", {
    # The gate is all-or-nothing over the column, so one value with a four-digit run -
    # including free text that never parses - makes the chosen setting a no-op.
    d <- data.frame(v = c("14/04/55", "03/07/62", "21/11/48", "unknown (2019 chart)"),
                    stringsAsFactors = FALSE)
    n <- dtc_notices(d, "v", datetime_format = "dmy", two_digit_year = "past")
    expect_match(n, "Correction Not Applied")
    expect_match(n, "unknown \\(2019 chart\\)")
})

test_that("the implausible-dates notice does not outlive the century correction", {
    # It is raised on the PRE-shift dates inside .parseDatetime(); left standing, the
    # module corrected the dates and then went on saying they were wrong, and the
    # quality grade stayed withheld on its account.
    d <- data.frame(v = c("14/04/55", "03/07/62", "21/11/48"), stringsAsFactors = FALSE)
    std <- dtc_obj(d, "v", datetime_format = "dmy", two_digit_year = "standard",
                   show_quality_metrics = TRUE)
    fixed <- dtc_obj(d, "v", datetime_format = "dmy", two_digit_year = "past",
                     show_quality_metrics = TRUE)
    expect_match(strip_html(std$results$notices$content), "Implausible")
    expect_false(grepl("Implausible", strip_html(fixed$results$notices$content)))
    expect_match(strip_html(fixed$results$qualityAssessment$content), "Excellent")
})

test_that("grade suppression keys on a class tag, not on English display text", {
    # The titles are now inside .(), so a grep for "Ambiguous" would stop matching the
    # day a translator fills in tr.po and the grade would reappear under the warning.
    an <- dtc_obj(data.frame(v = c("05/03/2021", "07/04/2021", "09/06/2021", "11/08/2021"),
                             stringsAsFactors = FALSE), "v", show_quality_metrics = TRUE)
    cls <- vapply(an$.__enclos_env__$private$.noticeList,
                  function(n) n$class %||% "", character(1))
    expect_true("date-suspect" %in% cls)
    expect_false(grepl("Excellent", strip_html(an$results$qualityAssessment$content)))
})

test_that("notices name formats the way the dropdown does", {
    n <- dtc_notices(data.frame(v = c("2023-01-15", "2023-02-20"), stringsAsFactors = FALSE),
                     "v", datetime_format = "excel_serial_1904")
    expect_match(n, "Excel serial \\(1904 system\\)")
    expect_false(grepl("excel_serial_1904", n))
})

test_that("translation placeholders contain no underscores", {
    # jmvcore's placeholder regex is \{ *[A-Za-z][A-Za-z0-9]* *\} -- an underscored name
    # ships as literal braces with no warning. Caught {a_date} and {read_as} in review.
    src <- paste(readLines(testthat::test_path("..", "..", "R", "datetimeconverter.b.R"),
                           warn = FALSE), collapse = "\n")
    for (m in regmatches(src, gregexpr('\\.\\("(?:[^"\\\\]|\\\\.)*"\\)', src))[[1]])
        expect_false(grepl("\\{[A-Za-z]*_", m), info = substr(m, 1, 60))
})


# --- Release review ----------------------------------------------------------

test_that("a forced numeric format is not reported as ignored", {
    # .processNumericVector honours force_format and returns already_parsed = TRUE, so
    # .run() printed "Manual format selection (excel_serial) ignored because data were
    # already stored as datetimes" in the same pane as "Forced Excel serial
    # interpretation". The selection was honoured and the input was not a datetime.
    f <- strip_html(dtc_obj(data.frame(v = c(44197, 44562, 45000)), "v",
                            datetime_format = "excel_serial")$results$formatInfo$content)
    expect_false(grepl("was ignored because", f))

    # a genuine POSIXct column must still say it
    f2 <- strip_html(dtc_obj(data.frame(v = as.POSIXct(c("2024-01-01 10:00:00",
                                                         "2024-02-01 11:00:00"), tz = "UTC")),
                             "v", datetime_format = "ymd")$results$formatInfo$content)
    expect_match(f2, "was ignored because")
})

test_that("implausible Unix epoch values are flagged like Excel ones", {
    # The Excel branch had a magnitude guard; the Unix branch had none, so the commonest
    # numeric misclick of all - Excel serials fed to Unix epoch - collapsed every value
    # into a single day in 1970 with nothing but "Conversion Completed".
    n <- dtc_notices(data.frame(v = c(44197, 44562, 45000)), "v", datetime_format = "unix_epoch")
    expect_match(n, "May Not Be Unix Timestamps")
    expect_false(grepl("May Not Be Unix",
                       dtc_notices(data.frame(v = c(1.7e9, 1.71e9)), "v",
                                   datetime_format = "unix_epoch")))
})

test_that("the Excel epoch matches an independently computed origin", {
    # as.Date(serial - 25569) is the standard identity; 1904 is exactly 1462 days later.
    p <- dtc_priv()
    s <- c(25569, 36526, 44927, 45292, 45658, 61, 100, 44000)
    g00 <- quietly(p$.processNumericVector(s, character(), s, force_format = "excel_serial"))
    g04 <- quietly(p$.processNumericVector(s, character(), s, force_format = "excel_serial_1904"))
    expect_equal(as.Date(g00$parsed_dates), as.Date(s - 25569, origin = "1970-01-01"))
    expect_true(all(as.numeric(difftime(g04$parsed_dates, g00$parsed_dates, units = "days")) == 1462))
})

test_that("text parsing agrees with lubridate for every supported order", {
    p <- dtc_priv()
    for (f in c("ymd", "dmy", "mdy")) {
        x <- switch(f, ymd = "2021-03-05", dmy = "05/03/2021", mdy = "03/05/2021")
        expect_equal(as.Date(quietly(p$.parseDatetime(x, f, tz = "UTC"))),
                     as.Date(suppressWarnings(getExportedValue("lubridate", f)(x))), info = f)
    }
})

test_that("no user-visible backend string is left untranslated", {
    src <- readLines(testthat::test_path("..", "..", "R", "datetimeconverter.b.R"), warn = FALSE)
    expect_equal(sum(grepl('title = "', src, fixed = TRUE)), 0L)
    expect_equal(sum(grepl("content = sprintf(", src, fixed = TRUE)), 0L)
    # .detectMisuse feeds a notice, so its strings count too
    expect_equal(sum(grepl('warnings, "', src, fixed = TRUE)), 0L)
})

test_that("no panel leaves dark text on a translucent tint", {
    # tools/theme_safe_html.py's documented blind spot. The only hardcoded colours left
    # must be white on an opaque dark table header, which is legible in both themes.
    src <- paste(readLines(testthat::test_path("..", "..", "R", "datetimeconverter.b.R"),
                           warn = FALSE), collapse = "\n")
    for (col in c("#1565c0", "#d32f2f", "#dc3545", "#28a745", "#555"))
        expect_false(grepl(paste0("color: ", col), src, fixed = TRUE), info = col)
})
