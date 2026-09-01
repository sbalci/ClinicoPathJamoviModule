`%||%` <- function(a, b) if (is.null(a)) b else a

# Regressions for the release-review pass on outcomeorganizer.
#
# Every block below corresponds to a defect that shipped and was found by reading
# the code against what the results window claims. They are grouped by the thing
# that was wrong, and each one states the failure mode in the test name so a
# future breakage is self-describing.

testthat::test_that("the Summary is defined on every analysis path", {
    # `handoff_coded` was computed inside ONE branch of the Summary's if/else and
    # read unconditionally by the recommendations block below it, so every path
    # other than that branch died with "object 'handoff_coded' not found" -- i.e.
    # the whole analysis, for the default analysis type.
    d1 <- data.frame(
        st = factor(c("Alive", "Dead", "Dead", "Alive", "Dead")),
        stringsAsFactors = FALSE)
    for (ty in c("os", "cause", "rfs", "pfs", "dfs", "ttp")) {
        a <- run_outcomeorganizer_obj(
            d1, outcome = "st", outcomeLevel = "Dead", analysistype = ty,
            outputTable = TRUE)
        testthat::expect_true(
            nzchar(a$results$summary$content),
            info = paste("empty Summary for analysistype =", ty))
    }

    # compete and multistate need Multiple Event Levels, so they take the
    # four-level coding -- and this is the path that carried the crash.
    d2 <- data.frame(
        st = factor(c("AWOD", "DOD", "DOOC", "AWD", "DOD")),
        stringsAsFactors = FALSE)
    for (ty in c("compete", "multistate")) {
        a <- run_outcomeorganizer_obj(
            d2, outcome = "st", multievent = TRUE, analysistype = ty,
            dod = "DOD", dooc = "DOOC", awd = "AWD", awod = "AWOD",
            outputTable = TRUE)
        testthat::expect_true(
            nzchar(a$results$summary$content),
            info = paste("empty Summary for analysistype =", ty))
    }

    # and with a single event level they must REFUSE visibly, not go quiet
    for (ty in c("compete", "multistate")) {
        a <- run_outcomeorganizer_obj(
            d1, outcome = "st", outcomeLevel = "Dead", analysistype = ty,
            outputTable = TRUE)
        testthat::expect_match(
            paste(a$results$errors$content, a$results$todo$content),
            "requires multiple event types|require multiple event types",
            info = ty)
    }
})

testthat::test_that("eventPriority below 1 is refused wherever it arrives from", {
    # `min: 1` is enforced by the GUI and the generated wrapper but NOT by
    # Options$new(), and 0 is the censored code -- giving it priority makes
    # censoring outrank every event and erases them all.
    d <- data.frame(
        id = c("P1", "P1"),
        st = factor(c("Alive", "DeadDisease"),
                    levels = c("Alive", "DeadDisease", "DeadOther")),
        ft = c(5, 10), stringsAsFactors = FALSE)
    # jmvcore::reject() THROWS -- it is jamovi's error channel, not a notice -- so
    # assert on the condition. run_outcomeorganizer_obj() wraps run() in try(), which
    # would swallow it and make this test pass on a silent no-op.
    mk <- function(p) {
        o <- do.call(.oo_fn("outcomeorganizerOptions")$new, list(
            outcome = "st", outcomeLevel = "DeadDisease", recurrenceLevel = NULL,
            multievent = TRUE, analysistype = "compete", dod = "DeadDisease",
            dooc = "DeadOther", awd = NULL, awod = "Alive", patientID = "id",
            followupTime = "ft", useHierarchy = TRUE, eventPriority = p))
        a <- .oo_fn("outcomeorganizerClass")$new(options = o, data = d)
        f <- tempfile(); sink(f); on.exit(sink(), add = TRUE)
        a$.__enclos_env__$private$.organizeOutcomes()
    }

    testthat::expect_error(mk(0), "1 or greater")

    # and the valid neighbour still runs, so the guard is not just refusing everything
    testthat::expect_silent({
        r <- suppressWarnings(mk(1)); invisible(NULL) })
    testthat::expect_equal(suppressWarnings(mk(1))$df_outcome$myoutcome, c(0, 1))
})

testthat::test_that("rows the cut-off cannot reach are disclosed, not silently exempt", {
    # `keep` drops rows missing a follow-up time or a cut-off. Those patients keep
    # their full follow-up AND their event while the rest are censored -- a partial
    # application of the cut-off. Every count was over `keep`, so the notice read
    # "truncated for 3 of 6" and never mentioned the other 4.
    d <- data.frame(
        st  = factor(rep(c("Alive", "Dead"), 5)),
        ft  = c(10, 50, 12, 60, 14, 70, 16, 80, 18, 90),
        cut = c(rep(40, 6), rep(NA_real_, 4)),
        stringsAsFactors = FALSE)
    a <- run_outcomeorganizer_obj(
        d, outcome = "st", outcomeLevel = "Dead", followupTime = "ft",
        adminCensoring = TRUE, adminDate = "cut", outputTable = TRUE)
    notices <- paste(a$results$warnings$content, a$results$strongWarnings$content,
                     a$results$infoMessages$content)

    testthat::expect_match(notices, "4 patient\\(s\\) were left untouched")
    testthat::expect_match(notices, "only part of the cohort")

    # and the bias being disclosed is real
    tt <- a$.__enclos_env__$private$.adminTime
    testthat::expect_equal(tt[1:6], pmin(d$ft[1:6], 40))   # cut-off applied
    testthat::expect_equal(tt[7:10], d$ft[7:10])           # exempt, full follow-up
})

testthat::test_that("the scale-mismatch message prescribes something the GUI allows", {
    # It used to say "Supply both as dates". followupTime is `permitted: [numeric]`,
    # so jamovi will not accept a date column there at all.
    d <- data.frame(
        st  = factor(rep(c("Alive", "Dead"), 5)),
        ft  = 1:10,
        cut = as.Date("2021-12-31") + 0:9,
        stringsAsFactors = FALSE)
    a <- run_outcomeorganizer_obj(
        d, outcome = "st", outcomeLevel = "Dead", followupTime = "ft",
        adminCensoring = TRUE, adminDate = "cut", outputTable = TRUE)
    notices <- paste(a$results$warnings$content, a$results$strongWarnings$content)

    testthat::expect_false(grepl("Supply both as dates", notices))
    testthat::expect_match(notices, "accepts numeric columns only")
})

testthat::test_that("a POSIXct cut-off keeps its own calendar date", {
    # as.Date() on a POSIXct defaults to tz = "UTC", not the timestamp's own zone,
    # so an evening timestamp west of Greenwich rolled forward a day and moved
    # every patient recorded that evening across the cut-off.
    a <- run_outcomeorganizer_obj(
        data.frame(a = 1), outcome = NULL, outcomeLevel = NULL)
    ts <- a$.__enclos_env__$private$.timeScale

    x <- as.POSIXct("2021-12-31 23:00:00", tz = "America/New_York")
    testthat::expect_equal(
        base::format(as.Date(ts(x)$v, origin = "1970-01-01")),
        base::format(x, "%Y-%m-%d", tz = "America/New_York"))

    # a plain Date is unchanged, and the kind is still reported
    testthat::expect_equal(ts(as.Date("2021-12-31"))$v,
                           as.numeric(as.Date("2021-12-31")))
    testthat::expect_equal(ts(as.Date("2021-12-31"))$kind, "date")
})

testthat::test_that("non-positive follow-up times are reported", {
    # pmin() carried -5 through and this analysis WRITES THAT COLUMN BACK, so a
    # negative duration landed in the spreadsheet ready for Surv(), which rejects it.
    d <- data.frame(
        st  = factor(c("Alive", "Dead", "Dead", "Alive")),
        ft  = c(10, -5, 20, 0),
        cut = rep(15, 4), stringsAsFactors = FALSE)
    a <- run_outcomeorganizer_obj(
        d, outcome = "st", outcomeLevel = "Dead", followupTime = "ft",
        adminCensoring = TRUE, adminDate = "cut", outputTable = TRUE)
    notices <- paste(a$results$warnings$content, a$results$strongWarnings$content)

    testthat::expect_match(notices, "NEGATIVE follow-up")
    testthat::expect_match(notices, "follow-up time of zero")
})

testthat::test_that("swapped DATE interval columns are caught", {
    # The check coerced with jmvcore::toNumeric(), a NO-OP on a Date or character
    # column. intervalStart/intervalEnd carry no `permitted:`, so exactly those
    # columns can be selected -- .il/.ir came back all-NA and the check was dead code.
    d <- data.frame(
        st = factor(rep(c("Alive", "Dead"), 5)),
        L  = as.Date("2021-06-01") + 0:9,
        R  = as.Date("2021-01-01") + 0:9,
        stringsAsFactors = FALSE)
    a <- run_outcomeorganizer_obj(
        d, outcome = "st", outcomeLevel = "Dead", intervalCensoring = TRUE,
        intervalStart = "L", intervalEnd = "R", outputTable = TRUE)

    testthat::expect_match(
        paste(a$results$warnings$content, a$results$strongWarnings$content),
        "later than their interval end|run backwards")
})

testthat::test_that("interval endpoints on different scales are refused", {
    d <- data.frame(
        st = factor(rep(c("Alive", "Dead"), 5)),
        L  = as.Date("2021-01-01") + 0:9,
        R  = 1:10, stringsAsFactors = FALSE)
    a <- run_outcomeorganizer_obj(
        d, outcome = "st", outcomeLevel = "Dead", intervalCensoring = TRUE,
        intervalStart = "L", intervalEnd = "R", outputTable = TRUE)

    testthat::expect_match(
        paste(a$results$warnings$content, a$results$strongWarnings$content),
        "not on the same scale")
})

testthat::test_that("the Summary does not imply interval columns were written back", {
    # The Diagnostics table said so, but it is off by default. This pane is always
    # visible and on its own read as "interval-censored data has been prepared".
    d <- data.frame(
        st = factor(rep(c("Alive", "Dead"), 5)),
        L  = 1:10, R = 2:11, stringsAsFactors = FALSE)
    a <- run_outcomeorganizer_obj(
        d, outcome = "st", outcomeLevel = "Dead", intervalCensoring = TRUE,
        intervalStart = "L", intervalEnd = "R", outputTable = TRUE)

    testthat::expect_match(a$results$summary$content, "not</b> written")
    testthat::expect_match(a$results$summary$content, "interval2")
})

testthat::test_that("severity names are normalised so strong warnings stay strong", {
    # .addHtmlMessage switched on "strongWarning" while every caller wrote
    # "strong_warning", so both of them fell through to the ordinary warning
    # bucket. switch() has no unmatched-value signal, so it was silent.
    a <- run_outcomeorganizer_obj(
        data.frame(a = 1), outcome = NULL, outcomeLevel = NULL)
    add <- a$.__enclos_env__$private$.addHtmlMessage

    for (spelling in c("strong_warning", "strongWarning", "STRONG_WARNING"))
        add(spelling, "t", "m")
    testthat::expect_equal(
        length(gregexpr("<div", a$results$strongWarnings$content)[[1]]), 3L)

    # and the ordinary buckets still route correctly
    add("warning", "t", "m"); add("info", "t", "m"); add("error", "t", "m")
    testthat::expect_true(nzchar(a$results$warnings$content))
    testthat::expect_true(nzchar(a$results$infoMessages$content))
    testthat::expect_true(nzchar(a$results$errors$content))
})

testthat::test_that("the glossary's RFS definition matches what RFS computes", {
    # The glossary said "death from disease"; RFS runs only on a single event level,
    # where the cause of death is not available, so it counts death from any cause.
    a <- run_outcomeorganizer_obj(
        data.frame(st = factor(c("Alive", "Dead"))),
        outcome = "st", outcomeLevel = "Dead", showGlossary = TRUE)
    g <- a$results$glossary$content

    testthat::expect_match(g, "any</b> cause")
    testthat::expect_false(grepl("recurrence or death from disease", g))

    # and the behaviour the glossary now describes
    d <- data.frame(
        st  = factor(c("Alive", "Dead", "Dead", "Alive", "Dead")),
        rec = factor(c("Yes", "No", "Yes", "No", "No")), stringsAsFactors = FALSE)
    mk <- function(ty) {
        r <- run_outcomeorganizer_obj(
            d, outcome = "st", outcomeLevel = "Dead", recurrence = "rec",
            recurrenceLevel = "Yes", analysistype = ty)
        r$.__enclos_env__$private$.organizeOutcomes()$df_outcome$myoutcome
    }
    testthat::expect_equal(mk("rfs"), c(1, 1, 1, 0, 1))   # recurrence OR any death
    testthat::expect_equal(mk("ttp"), c(1, 0, 1, 0, 0))   # progression only
})

testthat::test_that("RFS/PFS/DFS with Multiple Event Levels is refused, not silently ignored", {
    d <- data.frame(
        st  = factor(c("AWOD", "DOD", "DOD", "AWD", "DOOC")),
        rec = factor(c("Yes", "No", "Yes", "No", "No")), stringsAsFactors = FALSE)
    for (ty in c("rfs", "pfs", "dfs")) {
        o <- do.call(.oo_fn("outcomeorganizerOptions")$new, list(
            outcome = "st", outcomeLevel = NULL, recurrence = "rec",
            recurrenceLevel = "Yes", analysistype = ty, multievent = TRUE,
            dod = "DOD", dooc = "DOOC", awd = "AWD", awod = "AWOD"))
        a <- .oo_fn("outcomeorganizerClass")$new(options = o, data = d)
        f <- tempfile(); sink(f)
        # reject() throws; assert the condition, not a notice that never appears
        err <- tryCatch({ a$.__enclos_env__$private$.organizeOutcomes(); NULL },
                        error = function(e) conditionMessage(e))
        sink()
        testthat::expect_match(err %||% "", "not available with Multiple Event Levels",
                               info = ty)
    }
})

testthat::test_that("a mostly-missing outcome column is reported where the user will see it", {
    # This was an R warning() plus a Diagnostics row. Warnings land in the
    # undifferentiated "Analysis Notes" panel among package chatter, and the
    # Diagnostics table is OFF BY DEFAULT -- so losing two thirds of the cohort
    # showed up nowhere the reader of the results would look.
    d <- data.frame(
        st = factor(c("DOD", "DOD", "AWOD", "AWOD", rep(NA, 8)),
                    levels = c("DOD", "DOOC", "AWD", "AWOD")),
        stringsAsFactors = FALSE)
    a <- run_outcomeorganizer_obj(
        d, outcome = "st", multievent = TRUE, analysistype = "multistate",
        dod = "DOD", dooc = "DOOC", awd = "AWD", awod = "AWOD",
        outputTable = TRUE)

    testthat::expect_match(a$results$strongWarnings$content,
                           "Most rows have no usable outcome")
    testthat::expect_match(a$results$strongWarnings$content, "Only 4 of 12")
    testthat::expect_match(a$results$strongWarnings$content,
                           "denominator is smaller")
    # it must NOT blame the level selections: both coders reject an unassigned
    # level outright, so everything observed here is mapped
    testthat::expect_false(
        grepl("do not match the values actually present",
              a$results$strongWarnings$content))
})

testthat::test_that("an unassigned outcome level is refused before anything is dropped", {
    d <- data.frame(
        st = factor(c(rep("DOD", 2), rep("AWOD", 2), rep("Relapsed", 4))),
        stringsAsFactors = FALSE)
    o <- do.call(.oo_fn("outcomeorganizerOptions")$new, list(
        outcome = "st", outcomeLevel = NULL, recurrenceLevel = NULL,
        multievent = TRUE, analysistype = "multistate",
        dod = "DOD", dooc = "DOOC", awd = "AWD", awod = "AWOD"))
    a <- .oo_fn("outcomeorganizerClass")$new(options = o, data = d)
    f <- tempfile(); sink(f); on.exit(sink(), add = TRUE)
    testthat::expect_error(
        a$.__enclos_env__$private$.organizeOutcomes(),
        "not assigned to any state")
})
