# Regression tests for the 2026-10-30 independent review of singlearm
# (development-ideas/singlearm-independent-review-2026-10-30.md) and the shared
# decisions on dates and landmarks. F-numbers are the review's.

sa <- function(d, ...) {
    args <- list(data = d, ...)
    for (nm in c("outcomeLevel", "dod", "dooc", "awd", "awod"))
        if (is.null(args[[nm]])) args[nm] <- list(NULL)
    do.call(ClinicoPath::singlearm, args)
}
plain <- function(x) gsub("\\s+", " ", gsub("<[^>]+>", " ", paste(as.character(x), collapse = " ")))
notices <- function(r) paste(plain(r$errors$content), plain(r$warnings$content), plain(r$info$content))
note_text <- function(tb) paste(vapply(tb$notes, function(n) n$note, ""), collapse = " ")
xlab_of <- function(el) {
    f <- tempfile(fileext = ".png")
    grDevices::png(f, 700, 500)
    on.exit(grDevices::dev.off())
    el$.render(width = 700, height = 500, ppi = 72)
    ggplot2::last_plot()$labels$x
}

# Exactly the review's fixtures, so a failure here reads back to the report.
landmark_cohort <- function() {
    set.seed(42)
    tm <- round(rexp(120, 1 / 30) + .1, 1)
    ev <- rbinom(120, 1, .7)
    data.frame(OS_months = tm, e = factor(ifelse(ev == 1, "dead", "alive")))
}
cause_cohort <- function() {
    set.seed(11)
    n <- 300
    ad <- runif(n, 24, 96); tt <- rexp(n, 1 / 60); tc <- rexp(n, 1 / 40)
    tm <- round(pmin(ad, tt, tc), 2)
    s <- ifelse(tm == round(ad, 2), "AWOD", ifelse(tm == round(tt, 2), "DOD", "DOOC"))
    data.frame(t = tm, s = factor(s))
}
followup_value <- function(r) as.numeric(sub(" .*", "", r$dataQualityTable$asDF$value[5]))

# ---- F1 and the shared date decisions -----------------------------------------

test_that("numeric YYYYMMDD dates are refused with the shared sentence (F1)", {
    d <- data.frame(dx = c(20190115, 20190220, 20190311, 20190405, 20190512, 20190601),
                    fu = c(20200115, 20210220, 20200611, 20220405, 20210512, 20230601),
                    e = c(1, 0, 1, 1, 0, 1))
    r <- .audit_failed_result("singlearm", data = d, tint = TRUE, dxdate = "dx", fudate = "fu", outcome = "e")
    expect_match(plain(r$errors$content),
                 "dx contains numbers that cannot be read as dates (for example 20190115). Numeric date columns must hold seconds since 1970.",
                 fixed = TRUE)
    expect_equal(nrow(r$survTable$asDF), 0)
    expect_equal(nrow(r$medianTable$asDF), 0)
})

test_that("years, day counts and Excel serials are refused, not reinterpreted (F1)", {
    fixtures <- list(
        years = data.frame(dx = 2015:2020, fu = 2020:2025, e = c(1, 0, 1, 1, 0, 1)),
        days = data.frame(dx = 17000 + 0:5, fu = 17400 + 0:5, e = c(1, 0, 1, 1, 0, 1)),
        excel = data.frame(dx = 43000 + 0:5, fu = 43400 + 0:5, e = c(1, 0, 1, 1, 0, 1)))
    for (d in fixtures) {
        r <- .audit_failed_result("singlearm", data = d, tint = TRUE, dxdate = "dx", fudate = "fu", outcome = "e")
        expect_match(plain(r$errors$content), "cannot be read as dates", fixed = TRUE)
        expect_equal(nrow(r$medianTable$asDF), 0)
    }
})

test_that("epoch seconds and true Date columns are still read correctly", {
    dx <- as.Date("2020-01-01") + 0:5
    fu <- dx + 366
    secs <- data.frame(dx = as.numeric(as.POSIXct(dx)), fu = as.numeric(as.POSIXct(fu)),
                       e = c(1, 0, 1, 1, 0, 1))
    r <- sa(secs, tint = TRUE, dxdate = "dx", fudate = "fu", outcome = "e",
            timetypeoutput = "days", cutp = "100")
    expect_false(nzchar(plain(r$errors$content)))
    expect_equal(r$medianTable$asDF$records, 6)

    dates <- data.frame(dx = dx, fu = fu, e = c(1, 0, 1, 1, 0, 1))
    r <- sa(dates, tint = TRUE, dxdate = "dx", fudate = "fu", outcome = "e",
            timetypeoutput = "days", cutp = "100")
    expect_false(nzchar(plain(r$errors$content)))
    expect_equal(r$medianTable$asDF$median, 366)
})

test_that("a registry placeholder date is refused with the shared sentence", {
    d <- data.frame(dx = sprintf("2020-01-%02d", 1:6),
                    fu = c(sprintf("2021-01-%02d", 1:5), "2199-12-31"),
                    e = c(1, 0, 1, 1, 0, 1))
    r <- .audit_failed_result("singlearm", data = d, tint = TRUE, dxdate = "dx", fudate = "fu", outcome = "e", timetypedata = "ymd")
    expect_match(plain(r$errors$content),
                 "1 dates in fu fall outside a plausible range (for example 2199-12-31). Replace placeholder dates before analyzing; a follow-up date such as 2199-01-01 is not a censoring date.",
                 fixed = TRUE)
    expect_equal(nrow(r$medianTable$asDF), 0)
})

test_that("unreadable dates are counted once in a warning, with no literal braces", {
    d <- data.frame(dx = sprintf("2020-01-%02d", 1:6),
                    fu = c(sprintf("2021-01-%02d", 1:4), "not a date", "2021-13-45"),
                    e = c(1, 0, 1, 1, 0, 1))
    r <- sa(d, tint = TRUE, dxdate = "dx", fudate = "fu", outcome = "e", timetypedata = "ymd")
    expect_match(plain(r$warnings$content),
                 "2 of 12 dates could not be read, so those patients were excluded. Check the date format.",
                 fixed = TRUE)
    expect_false(grepl("Calculated time from dates contains", plain(r$warnings$content), fixed = TRUE))
    expect_equal(r$medianTable$asDF$records, 4)
})

# ---- landmark -------------------------------------------------------------------

test_that("the landmark keeps T > L: a subject at the landmark is not event-free after it", {
    d <- data.frame(t = c(6, 12, 12, 13, 15, 20, 30), e = c(1, 1, 0, 1, 0, 1, 0))
    r <- sa(d, elapsedtime = "t", outcome = "e", uselandmark = TRUE, landmark = 12, cutp = "1")
    expect_equal(r$medianTable$asDF$records, 4)
    expect_equal(r$medianTable$asDF$events, 2)
    expect_match(plain(r$warnings$content), "removed 3 subject(s)", fixed = TRUE)
})

test_that("a landmark is named wherever a shifted time appears, and only then (F3)", {
    d <- landmark_cohort()
    on <- sa(d, elapsedtime = "OS_months", outcome = "e", outcomeLevel = "dead",
             uselandmark = TRUE, landmark = 12, showSummaries = TRUE, sc = TRUE,
             person_time = TRUE, baseline_hazard = TRUE, advancedDiagnostics = TRUE)
    off <- sa(d, elapsedtime = "OS_months", outcome = "e", outcomeLevel = "dead",
              showSummaries = TRUE, sc = TRUE, person_time = TRUE, baseline_hazard = TRUE,
              advancedDiagnostics = TRUE)

    kept <- sum(d$OS_months > 12)
    suffix <- sprintf("(time since landmark, 12 months; %d of 120 at risk there)", kept)
    expect_match(on$medianTable$title, suffix, fixed = TRUE)
    expect_match(on$survTable$title, suffix, fixed = TRUE)
    expect_match(on$personTimeTable$title, suffix, fixed = TRUE)
    expect_match(plain(on$clinicalSummary$content), suffix, fixed = TRUE)
    expect_match(plain(on$medianSummary$content), suffix, fixed = TRUE)
    expect_match(plain(on$survTableSummary$content), suffix, fixed = TRUE)
    expect_match(plain(on$personTimeSummary$content), suffix, fixed = TRUE)

    sentence <- "Times are measured from the landmark at 12 months, not from diagnosis; add 12 to express a time as time from diagnosis."
    for (tb in list(on$medianTable, on$survTable, on$personTimeTable,
                    on$baselineHazardTable, on$dataQualityTable))
        expect_match(note_text(tb), sentence, fixed = TRUE)

    expect_equal(on$personTimeTable$asDF$interval[1], "Overall (from landmark)")
    expect_match(note_text(on$medianTable), "from the landmark to the largest observed time", fixed = TRUE)
    expect_equal(xlab_of(on$plot), "Time since landmark (months)")

    for (tb in list(off$medianTable, off$survTable, off$personTimeTable)) {
        expect_false(grepl("landmark", tb$title, ignore.case = TRUE))
        expect_false(grepl("landmark", note_text(tb), ignore.case = TRUE))
    }
    expect_false(grepl("landmark", plain(off$clinicalSummary$content), ignore.case = TRUE))
    expect_equal(off$personTimeTable$asDF$interval[1], "Overall (0-max)")
    expect_equal(xlab_of(off$plot), "OS_months (months)")
})

test_that("the competing-risk tables and CIF axis name the landmark too (F3)", {
    d <- cause_cohort()
    r <- sa(d, elapsedtime = "t", outcome = "s", multievent = TRUE, analysistype = "compete",
            dod = "DOD", dooc = "DOOC", awod = "AWOD", uselandmark = TRUE, landmark = 6,
            cutp = "12", sc = TRUE, showSummaries = TRUE, cif_all_causes = TRUE,
            endplot = 60, byplot = 12)
    expect_match(r$medianTable$title, "time since landmark", fixed = TRUE)
    expect_match(r$survTable$title, "time since landmark", fixed = TRUE)
    expect_match(plain(r$survTableSummary$content), "time since landmark", fixed = TRUE)
    expect_match(note_text(r$cifTable), "Times are measured from the landmark", fixed = TRUE)
    expect_equal(xlab_of(r$plot_cif), "Time since landmark (months)")
})

# ---- F2 follow-up ----------------------------------------------------------------

test_that(".defineEventIndicator() separates alive from status 0", {
    x <- c("AWD", "AWOD", "DOD", "DOOC", NA)
    res <- function(type) ClinicoPath:::.defineEventIndicator(
        x, multievent = TRUE, analysistype = type,
        dod = "DOD", dooc = "DOOC", awd = "AWD", awod = "AWOD")
    expect_identical(res("cause")$status, c(0L, 0L, 1L, 0L, NA))
    expect_identical(res("cause")$alive, c(TRUE, TRUE, FALSE, FALSE, NA))
    expect_identical(res("overall")$alive, c(TRUE, TRUE, FALSE, FALSE, NA))
    expect_identical(res("compete")$alive, c(TRUE, TRUE, FALSE, FALSE, NA))
    expect_identical(res("dfs")$alive, c(FALSE, TRUE, FALSE, FALSE, NA))

    one <- ClinicoPath:::.defineEventIndicator(x, outcomeLevel = "DOD")
    expect_identical(one$alive, one$status == 0L)

    handoff <- ClinicoPath:::.defineEventIndicator(
        factor(c("Censored", "Event", "Competing"), levels = c("Censored", "Event", "Competing")))
    expect_identical(handoff$alive, c(TRUE, FALSE, FALSE))
})

test_that("median follow-up is the same under Overall, Cause Specific and Competing Risk (F2)", {
    d <- cause_cohort()
    run <- function(type) sa(d, elapsedtime = "t", outcome = "s", multievent = TRUE,
                             analysistype = type, dod = "DOD", dooc = "DOOC", awod = "AWOD",
                             advancedDiagnostics = TRUE, cutp = "12")
    oracle <- round(unname(summary(survival::survfit(
        survival::Surv(d$t, d$s == "AWOD") ~ 1))$table["median"]), 1)
    expect_equal(followup_value(run("cause")), oracle)
    expect_equal(followup_value(run("overall")), oracle)
    expect_equal(followup_value(run("compete")), oracle)
})

# ---- F4, F10 recode disclosure --------------------------------------------------

test_that("recode caveats reach a default run (F4)", {
    set.seed(5)
    df <- data.frame(t = round(rexp(50, 1 / 30) + .1, 1),
                     s = factor(sample(c("AWD", "DOD", "DOOC", "AWOD"), 50, TRUE)))
    dfs <- sa(df, elapsedtime = "t", outcome = "s", multievent = TRUE, analysistype = "dfs",
              dod = "DOD", dooc = "DOOC", awd = "AWD", awod = "AWOD", cutp = "12")
    expect_true(dfs$eventRecodeInfo$visible)
    expect_match(plain(dfs$warnings$content), "recurrence or progression", fixed = TRUE)

    level <- sa(df, elapsedtime = "t", outcome = "s", outcomeLevel = "DOD", cutp = "12")
    expect_true(level$eventRecodeInfo$visible)
    expect_match(plain(level$warnings$content), "DOOC", fixed = TRUE)

    cause <- sa(df, elapsedtime = "t", outcome = "s", multievent = TRUE, analysistype = "cause",
                dod = "DOD", dooc = "DOOC", awd = "AWD", awod = "AWOD", cutp = "12")
    expect_true(cause$eventRecodeInfo$visible)
    expect_match(plain(cause$warnings$content), "overstates the absolute risk", fixed = TRUE)
})

test_that("ordinary recodes stay quiet on a default run (F4)", {
    set.seed(5)
    df <- data.frame(t = round(rexp(50, 1 / 30) + .1, 1),
                     s = factor(sample(c("AWD", "DOD", "DOOC", "AWOD"), 50, TRUE)))
    overall <- sa(df, elapsedtime = "t", outcome = "s", multievent = TRUE, analysistype = "overall",
                  dod = "DOD", dooc = "DOOC", awd = "AWD", awod = "AWOD", cutp = "12")
    expect_false(overall$eventRecodeInfo$visible)
    binary <- sa(data.frame(t = 1:6, s = factor(c("d", "a", "d", "a", "d", "a"))),
                 elapsedtime = "t", outcome = "s", outcomeLevel = "d", cutp = "2")
    expect_false(binary$eventRecodeInfo$visible)
})

test_that("the cumulative-event plot states the censoring of other-cause deaths (F4)", {
    d <- cause_cohort()
    r <- sa(d, elapsedtime = "t", outcome = "s", multievent = TRUE, analysistype = "cause",
            dod = "DOD", dooc = "DOOC", awod = "AWOD", ce = TRUE, cutp = "12")
    f <- tempfile(fileext = ".png")
    grDevices::png(f, 700, 500)
    on.exit(grDevices::dev.off())
    r$plot2$.render(width = 700, height = 500, ppi = 72)
    expect_match(ggplot2::last_plot()$labels$subtitle, "overstates the absolute risk", fixed = TRUE)
})

test_that("Disease-Free with nobody Alive with Disease says it is overall survival (F10)", {
    set.seed(5)
    df <- data.frame(t = round(rexp(50, 1 / 30) + .1, 1),
                     s = factor(sample(c("DOD", "DOOC", "AWOD"), 50, TRUE)))
    dfs <- sa(df, elapsedtime = "t", outcome = "s", multievent = TRUE, analysistype = "dfs",
              dod = "DOD", dooc = "DOOC", awod = "AWOD", cutp = "12")
    os <- sa(df, elapsedtime = "t", outcome = "s", multievent = TRUE, analysistype = "overall",
             dod = "DOD", dooc = "DOOC", awod = "AWOD", cutp = "12")
    expect_match(plain(dfs$warnings$content), "equals overall survival", fixed = TRUE)
    expect_match(plain(dfs$eventRecodeInfo$content), "No Alive with Disease rows", fixed = TRUE)
    expect_equal(dfs$survTable$asDF$surv, os$survTable$asDF$surv)
})

# ---- F5 to F9, F11, F12 ----------------------------------------------------------

test_that("a cumulative incidence of exactly one half gives the median (F5)", {
    for (n in c(22, 26, 30, 42, 66)) {
        d <- data.frame(t = seq_len(n), s = factor(c(rep("D", n / 2), rep("A", n / 2))))
        r <- sa(d, elapsedtime = "t", outcome = "s", multievent = TRUE, analysistype = "compete",
                dod = "D", awod = "A", cutp = "1")
        expect_equal(r$medianTable$asDF$median, n / 2)
    }
})

test_that("rates and hazards reach the table unrounded and name their unit (F6, F7)", {
    set.seed(3)
    d <- data.frame(t = round(rexp(200, 1 / 2000) + 1), e = rbinom(200, 1, .2))
    r <- sa(d, elapsedtime = "t", outcome = "e", timetypeoutput = "days", person_time = TRUE,
            rate_multiplier = 1, baseline_hazard = TRUE, time_intervals = "365,1825",
            showSummaries = TRUE)
    p <- r$personTimeTable$asDF
    expect_equal(p$rate[1], sum(d$e) / sum(d$t), tolerance = 1e-8)
    expect_true(all(r$baselineHazardTable$asDF$hazard_lower > 0))
    expect_match(plain(r$personTimeSummary$content),
                 sprintf("incidence rate of %s events per 1 person-days", signif(sum(d$e) / sum(d$t), 3)),
                 fixed = TRUE)

    r2 <- sa(d, elapsedtime = "t", outcome = "e", timetypeoutput = "days", person_time = TRUE,
             rate_multiplier = 1000)
    expect_match(r2$personTimeTable$title, "rates per 1000 person-days", fixed = TRUE)
    r3 <- sa(d, elapsedtime = "t", outcome = "e", timetypeoutput = "days", person_time = TRUE,
             rate_multiplier = 100000)
    expect_match(r3$personTimeTable$title, "rates per 100000 person-days", fixed = TRUE)
})

test_that("narrative percentages do not round to a false boundary (F8)", {
    d <- data.frame(t = 1:500, e = c(rep(0, 100), 1, rep(0, 399)))
    r <- sa(d, elapsedtime = "t", outcome = "e", cutp = "300", showSummaries = TRUE)
    expect_match(plain(r$survTableSummary$content), "was 99.8%", fixed = TRUE)
})

test_that("the Preformatted panels are cleared as text, not as markup (F9)", {
    d <- data.frame(t = 1:10, e = factor(c("a", "d", "a", "d", "d", "a", "d", "d", "a", "a")))
    r <- sa(d, elapsedtime = "t", outcome = "e", outcomeLevel = "d", cutp = "50, 60",
            showSummaries = TRUE)
    expect_false(grepl("<", paste(r$survTableSummary$content), fixed = TRUE))
    r <- .audit_failed_result("singlearm", data = transform(d, t = c(-1, 2:10)), elapsedtime = "t", outcome = "e", outcomeLevel = "d",
            showSummaries = TRUE)
    expect_false(grepl("<", paste(r$medianSummary$content), fixed = TRUE))
})

test_that("the median wording covers a curve sitting at exactly 50% (F11)", {
    r <- sa(data.frame(t = 1:4, e = 1), elapsedtime = "t", outcome = "e", cutp = "1",
            showSummaries = TRUE)
    expect_equal(r$medianTable$asDF$median, 2.5)
    expect_match(paste(r$medianSummary$content, collapse = " "), "midpoint", fixed = TRUE)
})

test_that("the median table states how its interval was built", {
    r <- sa(data.frame(t = 1:8, e = c(1, 0, 1, 1, 0, 1, 1, 0)), elapsedtime = "t", outcome = "e",
            cutp = "1")
    expect_match(note_text(r$medianTable), "Brookmeyer-Crowley", fixed = TRUE)
})

test_that("the CIF explanation no longer denies the risk panel (F12)", {
    d <- data.frame(t = 1:40, s = factor(rep(c("D", "O", "A", "D"), 10)))
    r <- sa(d, elapsedtime = "t", outcome = "s", multievent = TRUE, analysistype = "compete",
            dod = "D", dooc = "O", awod = "A", sc = TRUE, showExplanations = TRUE,
            endplot = 40, byplot = 10)
    expect_false(grepl("numbers-at-risk panel is also not available",
                       r$survivalPlotsExplanation$content, fixed = TRUE))
})

# ---- saved outcome ---------------------------------------------------------------

test_that("the saved competing outcome round-trips as competing risks (summary F15)", {
    d <- cause_cohort()
    a <- ClinicoPath:::singlearmClass$new(
        options = ClinicoPath:::singlearmOptions$new(
            elapsedtime = "t", outcome = "s", multievent = TRUE, analysistype = "compete",
            dod = "DOD", dooc = "DOOC", awod = "AWOD", cutp = "60", outcomeredefined = TRUE),
        data = d)
    a$init()
    a$run()
    saved <- a$results$outcomeredefined$.__enclos_env__$private$.values[[1]]
    expect_s3_class(saved, "factor")
    expect_identical(levels(saved), c("Censored", "Event", "Competing"))
    expect_equal(as.character(saved),
                 c(AWOD = "Censored", DOD = "Event", DOOC = "Competing")[as.character(d$s)],
                 ignore_attr = TRUE)

    direct <- a$results$survTable$asDF$surv
    back <- sa(data.frame(t = d$t, o = saved), elapsedtime = "t", outcome = "o", cutp = "60")
    expect_equal(back$survTable$asDF$surv, direct)
    expect_true(grepl("Cumulative Incidence", back$survTable$title, fixed = TRUE))
})
