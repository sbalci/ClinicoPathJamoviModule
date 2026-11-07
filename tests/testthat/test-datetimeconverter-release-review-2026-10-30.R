library(testthat)

# Independent review 2026-10-30, findings DC-1 to DC-6, and the grid that holds the two date
# analyses to one reading of one column. Each block states what the analysis did before.
# Use base::format: jmvcore exports a format() that ignores format strings.

fmt <- base::format
strip_html <- function(x) {
    gsub("[[:space:]]+", " ", gsub("<[^>]*>", " ", paste(as.character(x), collapse = " ")))
}
dc <- function(v, datetime_format = "auto", timezone = "UTC", ...) {
    r <- suppressWarnings(suppressMessages(ClinicoPath::datetimeconverter(
        data = data.frame(x = v, stringsAsFactors = FALSE), datetime_var = "x",
        datetime_format = datetime_format, timezone = timezone, ...)))
    list(ch = r$corrected_datetime_char$.__enclos_env__$private$.values[[1]],
         notices = strip_html(r$notices$content), info = strip_html(r$formatInfo$content))
}
ti <- function(s, e, ...) {
    r <- tryCatch(suppressWarnings(suppressMessages(ClinicoPath::timeinterval(
        data = data.frame(s = s, e = e, stringsAsFactors = FALSE),
        dx_date = "s", fu_date = "e", output_unit = "days", ...))),
        error = function(err) err)
    if (inherits(r, "error")) return(list(err = conditionMessage(r)))
    list(vals = r$calculated_time$.__enclos_env__$private$.values[[1]],
         msg = strip_html(r$messages$content))
}
cohort <- function(n, from = "2018-01-01", days = 2000, seed = 20261030) {
    set.seed(seed)
    as.POSIXct(from, tz = "UTC") + floor(runif(n, 0, days)) * 86400 + sample(0:1439, n, TRUE) * 60
}
minutes <- function(ch) substr(ch, 1, 16)

test_that("DC-1: date-with-minutes text keeps its date, in an all-2000s cohort", {
    # Was: dmy_hms, 0/60. 21/02/2021 17:28 became 2020-02-21 21:17:28, every date in 2020.
    t0 <- cohort(60)
    for (f in c("%d/%m/%Y %H:%M", "%m/%d/%Y %H:%M", "%d-%m-%Y %H:%M", "%d.%m.%Y %H:%M")) {
        a <- dc(fmt(t0, f))
        expect_equal(minutes(a$ch), fmt(t0, "%Y-%m-%d %H:%M"), info = f)
        expect_false(grepl("Ambiguous", a$notices), info = f)
    }
    a <- dc(rep(c("03/15/2020 2:30 PM", "11/02/2021 11:05 AM", "01/30/2022 12:15 PM"), 3))
    expect_identical(a$ch[2], "2021-11-02 11:05:00")
})

test_that("DC-1: an explicit format the text cannot have is refused, and says why", {
    # Was: dmyhms on this text gave the sliced reading at 100%.
    t0 <- cohort(20)
    a <- dc(fmt(t0, "%d/%m/%Y %H:%M"), datetime_format = "dmyhms")
    expect_true(all(is.na(a$ch)))
    expect_match(a$notices, "20 value(s) were left empty because their text cannot have the format",
                 fixed = TRUE)
})

test_that("DC-2: a partial date keeps its year and says the day is not data", {
    # Was: ydm, year right on 11/60; 2018-12 became 2020-12-18.
    t0 <- cohort(60)
    for (f in c("%Y-%m", "%m/%Y", "%b %Y")) {
        a <- dc(fmt(t0, f))
        expect_equal(substr(a$ch, 1, 10), paste0(fmt(t0, "%Y-%m"), "-01"), info = f)
        expect_match(a$notices, "no day", fixed = TRUE, info = f)
    }
})

test_that("DC-3: six-digit dates are not Excel serials under auto, text or numeric", {
    # Was: 0/60; 061218 became 2067-08-09, others 2475, a share of rows plausible.
    d <- as.Date(cohort(60))
    a <- dc(fmt(d, "%d%m%y"))
    expect_equal(substr(a$ch, 1, 10), as.character(d))
    b <- dc(as.numeric(fmt(d, "%d%m%y")))
    expect_equal(substr(b$ch, 1, 10), as.character(d))
    expect_match(b$info, "Restored a lost leading zero", fixed = TRUE)

    # text keeps its zeros, so a column with every number under 100000 is still no serial;
    # its days are all 9 or less, so day-first and year-first both fit, and it says so
    early <- as.Date(c("2019-03-02", "2020-11-05", "2021-06-09", "2018-01-07"))
    a <- dc(fmt(early, "%d%m%y"))
    expect_false(grepl("Excel", a$info, fixed = TRUE))
    expect_match(a$notices, "Ambiguous", fixed = TRUE)

    # a stray six-digit value does not take a serial column off the serial path
    ser <- c(seq(42000, 43000, length.out = 30), 123456)
    expect_equal(substr(dc(ser)$ch[1:30], 1, 10),
                 as.character(as.Date(ser[1:30], origin = "1899-12-30")))
})

test_that("DC-4: padding never turns a serial column into April dates", {
    # Was: 0/40. 42370 padded to 042370, read 1970-04-23, every date in April.
    set.seed(1)
    ser <- sample(40200:43000, 40)
    a <- dc(ser, datetime_format = "mdy")
    expect_equal(substr(a$ch, 1, 10), as.character(as.Date(ser, origin = "1899-12-30")))
    # the maintainer's fixture: a zero visibly lost from a six-digit column
    expect_identical(substr(dc(c(50321, 150321, 311299), datetime_format = "dmy")$ch[1], 1, 10),
                     "2021-03-05")
})

test_that("DC-5: 'Always in the past' reaches packed two-digit years", {
    # Was: 2/30. [0-9]{4} matched inside the six-digit run, and the notice claimed a
    # four-digit year (first: 091160).
    set.seed(5)
    dob <- as.Date("1935-01-01") + sample(14000, 30)
    a <- dc(fmt(dob, "%d%m%y"), datetime_format = "dmy", two_digit_year = "past")
    expect_equal(substr(a$ch, 1, 10), as.character(dob))
    expect_false(grepl("Not Applied", a$notices, fixed = TRUE))
    # an eight-digit date does hold a four-digit year
    b <- dc(c("15012020", "16022021"), datetime_format = "dmy", two_digit_year = "past")
    expect_match(b$notices, "Not Applied", fixed = TRUE)
})

test_that("DC-6: a date with no time survives a zone with no midnight on that day", {
    # Was: NA, with a notice about a clock time the value does not contain; via 'past' the
    # century shift emptied the row with no notice at all.
    a <- dc(c("1964-05-15", "1985-04-20", "2000-01-01"), timezone = "Europe/Istanbul")
    expect_identical(substr(a$ch, 1, 10), c("1964-05-15", "1985-04-20", "2000-01-01"))
    expect_false(grepl("Daylight", a$notices, fixed = TRUE))
    b <- dc(c("15/05/64", "20/04/85"), datetime_format = "dmy", two_digit_year = "past",
            timezone = "Europe/Istanbul")
    expect_identical(substr(b$ch, 1, 10), c("1964-05-15", "1985-04-20"))
})

test_that("a real clock time in a daylight-saving gap is still reported as one", {
    a <- dc(c("2024-03-10 02:30:00", "2024-03-11 02:30:00"), timezone = "America/New_York")
    expect_true(is.na(a$ch[1]))
    expect_match(a$notices, "does not exist in the", fixed = TRUE)
})

test_that("the shape guard refuses only what is structurally impossible", {
    # ISO offsets bring their own colon and may change the year on conversion
    expect_identical(as.character(.dateShapeFits("2020-01-15T14:30:00+03:00", "ymd_hms",
        as.POSIXct("2020-01-15 11:30:00", tz = "UTC"))), "TRUE")
    expect_true(.dateShapeFits("2020-12-31T23:30:00-05:00", "ymd_hms",
                               as.POSIXct("2021-01-01 04:30:00", tz = "UTC")))
    # a clock written without a colon, and a packed run
    expect_false(.dateShapeFits("15/01/2020 1430", "dmy_hms",
                                as.POSIXct("2020-01-15 20:14:30", tz = "UTC")))
    expect_true(.dateShapeFits("15/01/2020 1430", "dmy_hm",
                               as.POSIXct("2020-01-15 14:30:00", tz = "UTC")))
    expect_true(.dateShapeFits("20200115143000", "ymd_hms",
                               as.POSIXct("2020-01-15 14:30:00", tz = "UTC")))
    # a real ambiguity is not its business
    expect_true(.dateShapeFits("01/05/19", "ymd", as.Date("2001-05-19")))
    expect_true(.dateShapeFits("01/05/19", "dmy", as.Date("2019-05-01")))
    expect_equal(.dateMisfits(character(0), "ymd"), integer(0))
})

test_that(".restoreLeadingZeros pads only a column that shows the loss", {
    expect_identical(.restoreLeadingZeros(c("50321", "150321", NA)), c("050321", "150321", NA))
    expect_identical(.restoreLeadingZeros(c("42370", "42371")), c("42370", "42371"))
    expect_identical(.restoreLeadingZeros(c(rep("42370", 9), "999999")), c(rep("42370", 9), "999999"))
    expect_identical(.restoreLeadingZeros(c("5012020", "15012020")), c("05012020", "15012020"))
    expect_identical(.restoreLeadingZeros(c("105", "1105", "990105")), c("000105", "001105", "990105"))
    expect_identical(.restoreLeadingZeros(c("45", "1105", "990105")), c("45", "1105", "990105"))
    expect_identical(.restoreLeadingZeros(c("2020-01-01", "50321")), c("2020-01-01", "50321"))
})

# ---- the grid --------------------------------------------------------------------------
# Every common clinical shape, day-first and month-first, all-2000s and century-spanning.
# The all-2000s cohort is the one that hid DC-1: a single 19xx row makes the sliced reading
# fail. Both analyses take the format from .rankDateFormats(), so the whole grid runs there,
# one column and two, and a shorter set runs end to end through both analyses.

grid_shapes <- c(
    "%Y-%m-%d", "%Y/%m/%d", "%Y.%m.%d", "%Y%m%d",
    "%d/%m/%Y", "%m/%d/%Y", "%d-%m-%Y", "%m-%d-%Y", "%d.%m.%Y", "%m.%d.%Y",
    "%d/%m/%y", "%m/%d/%y", "%d.%m.%y", "%d-%m-%y", "%y-%m-%d", "%y%m%d", "%d%m%y", "%m%d%y",
    "%d%m%Y", "%m%d%Y",
    "%d-%b-%Y", "%d-%b-%y", "%d %b %Y", "%b %d, %Y", "%d %B %Y", "%B %d, %Y",
    "%Y-%m-%d %H:%M:%S", "%Y-%m-%dT%H:%M:%S", "%Y-%m-%dT%H:%M:%SZ", "%Y-%m-%d %H:%M",
    "%Y/%m/%d %H:%M", "%d/%m/%Y %H:%M:%S", "%m/%d/%Y %H:%M:%S", "%d/%m/%Y %H:%M", "%m/%d/%Y %H:%M",
    "%d.%m.%Y %H:%M", "%m.%d.%Y %H:%M", "%d-%m-%Y %H:%M", "%m-%d-%Y %H:%M", "%d.%m.%Y %H:%M:%S",
    "%d/%m/%y %H:%M", "%m/%d/%y %H:%M", "%d-%b-%Y %H:%M", "%d %b %Y %H:%M", "%b %d, %Y %H:%M",
    "%d/%m/%Y %I:%M %p", "%m/%d/%Y %I:%M %p", "%m/%d/%Y %I:%M:%S %p",
    "%Y-%m", "%Y/%m", "%m/%Y", "%m-%Y", "%b %Y", "%b-%Y", "%B %Y", "%Y%m")

grid_pair <- function(from, days, n = 60) {
    t0 <- cohort(n, from, days)
    list(t0 = t0, t1 = t0 + sample(30:900, n, TRUE) * 86400 + sample(0:1439, n, TRUE) * 60)
}
grid_key <- function(f) {
    if (grepl("%H|%I", f)) "%Y-%m-%d %H:%M" else if (grepl("%d", f)) "%Y-%m-%d" else "%Y-%m"
}

for (co in list(list("2015-01-01", 3400, "all-2000s"), list("1992-01-01", 6500, "century-spanning"))) {
    test_that(paste("grid,", co[[3]], "cohort: one reading per shape, one column or two"), {
        p <- grid_pair(co[[1]], co[[2]])
        for (f in grid_shapes) {
            x <- fmt(p$t0, f); y <- fmt(p$t1, f)
            pair <- .rankDateFormats(list(x, y))
            expect_identical(.rankDateFormats(list(x))$format, pair$format, info = f)
            expect_identical(.rankDateFormats(list(y))$format, pair$format, info = f)
            got <- pair$parsed[[pair$format]][[1]]
            expect_equal(fmt(got, grid_key(f)), fmt(p$t0, grid_key(f)), info = f)
        }
    })
}

grid_row <- function(f, from, days, numeric = FALSE) {
    p <- grid_pair(from, days)
    key <- grid_key(f)
    x <- fmt(p$t0, f); y <- fmt(p$t1, f)
    if (numeric) { x <- as.numeric(x); y <- as.numeric(y) }
    # what a correct reading gives: the fields the text records, nothing it does not
    truth <- function(tt) as.POSIXct(paste0(fmt(tt, key), if (key == "%Y-%m") "-01"), tz = "UTC")
    a <- dc(x); b <- dc(y)
    expect_equal(substr(a$ch, 1, nchar(fmt(p$t0[1], key))), fmt(p$t0, key), info = f)
    iv <- ti(x, y, timezone = "utc")  # as dc(): a local daylight-saving hour is not the point
    expect_null(iv$err, info = f)
    want <- as.numeric(difftime(truth(p$t1), truth(p$t0), units = "days"))
    expect_equal(iv$vals, want, tolerance = 1e-6, info = f)
    conv <- as.numeric(difftime(as.POSIXct(b$ch, tz = "UTC"), as.POSIXct(a$ch, tz = "UTC"), units = "days"))
    expect_equal(iv$vals, conv, tolerance = 1e-6, info = paste("analyses disagree:", f))
}

test_that("end to end: the converter and timeinterval agree row by row", {
    for (f in c("%Y-%m-%d", "%d/%m/%Y", "%m/%d/%Y", "%d/%m/%y", "%d%m%y", "%Y-%m-%d %H:%M",
                "%d/%m/%Y %H:%M", "%m/%d/%Y %H:%M", "%d.%m.%Y %H:%M:%S", "%m/%d/%Y %I:%M %p",
                "%Y-%m", "%m/%Y"))
        grid_row(f, "2015-01-01", 3400)
    grid_row("%Y%m%d", "2015-01-01", 3400, numeric = TRUE)
    grid_row("%d%m%y", "2015-01-01", 3400, numeric = TRUE)
    # YYMMDD crossing 2000: 000105 is stored as 105
    grid_row("%y%m%d", "1992-01-01", 6500, numeric = TRUE)
})

test_that("numeric MMDDYY that looks like a serial is refused or flagged, never trusted", {
    # Months 1-9 lose their zero, so most rows are five digits: the width of a serial. Past
    # 80% timeinterval refuses and the converter keeps the serial reading, whose six-digit
    # rows land centuries out and raise the implausible-date warning.
    set.seed(2)
    d <- as.Date("2015-01-01") + sample(3400, 60)
    d <- d[as.integer(fmt(d, "%m")) <= 9 | seq_along(d) %% 6 == 0]
    x <- as.numeric(fmt(d, "%m%d%y"))
    expect_gte(mean(x < 1e5), 0.8)
    expect_match(dc(x)$notices, "Implausible", fixed = TRUE)
    expect_false(is.null(ti(x, x)$err))
    # a text format chosen by the user settles it for the converter
    expect_equal(substr(dc(x, datetime_format = "mdy")$ch, 1, 10), as.character(d))
})
