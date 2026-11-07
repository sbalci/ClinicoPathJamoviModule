# Regressions from the 2026-10-30 independent review of survivalcont
# (development-ideas/survivalcont-independent-review-2026-10-30.md) and the
# cross-analysis summary. Each block fails on the pre-fix tree.

sc_rr_data <- function(n = 240, b = 0.5, seed = 1) {
    set.seed(seed)
    m <- rnorm(n, 50, 10)
    t0 <- rexp(n, 0.03 * exp(b * (m - 50) / 10))
    cn <- runif(n, 5, 80)
    data.frame(id = seq_len(n), m = m, time = pmin(t0, cn), ev = as.integer(t0 <= cn))
}
sc_rr <- function(d, ...) {
    run_survivalcont(data = d, elapsedtime = "time", outcome = "ev", contexpl = "m", ...)
}
sc_rr_notes <- function(table) paste(vapply(table$notes, function(n) n$note, ""), collapse = " | ")
sc_rr_text <- function(r) strip_survivalcont_html(c(
    r$errors$content, r$strongWarnings$content, r$warnings$content, r$infoMessages$content))
sc_rr_src <- function() paste(readLines(file.path("..", "..", "R", "survivalcont.b.R")), collapse = "\n")

test_that("F1: the HR keeps the digits its CI needs, in the table and in the sentence", {
    set.seed(1); n <- 300
    d <- data.frame(m = rnorm(n, 5000, 1500))
    t0 <- rexp(n, 0.03 * exp(0.0004 * (d$m - 5000))); cn <- runif(n, 5, 80)
    d$time <- pmin(t0, cn); d$ev <- as.integer(t0 <= cn)
    hr <- unname(exp(coef(survival::coxph(survival::Surv(time, ev) ~ m, d))))
    r <- sc_rr(d, showSummaries = TRUE)
    printed <- r$coxTable$asDF$HR_univariable
    expect_lt(abs(as.numeric(sub("^([0-9.]+) .*", "\\1", printed)) - hr), 5e-5)
    sentence <- strip_survivalcont_html(r$coxSummary$content)
    expect_false(grepl("multiplied by 1 ", sentence, fixed = TRUE))
    expect_match(sentence, paste("multiplied by", sub("^([0-9.]+) .*", "\\1", printed)), fixed = TRUE)
})

test_that("F1: a wide interval still prints two decimals", {
    d <- sc_rr_data()
    d$m <- d$m / 10
    r <- sc_rr(d)
    expect_match(r$coxTable$asDF$HR_univariable, "^[0-9]\\.[0-9]{2} \\(")
})

test_that("F2: pre-calculated time says it is not converted; dates do not", {
    d <- sc_rr_data()
    expect_match(sc_rr_text(sc_rr(d)), "no unit conversion is performed", fixed = TRUE)

    days <- d; days$time <- days$time * 30
    expect_match(sc_rr_text(sc_rr(days)), "more than 50 years", fixed = TRUE)
    expect_false(grepl("more than 50 years", sc_rr_text(sc_rr(d)), fixed = TRUE))

    d$dx <- format(as.Date("2015-01-10") + 1:240, "%Y-%m-%d")
    d$fu <- format(as.Date("2015-01-10") + 1:240 + round(d$time * 30), "%Y-%m-%d")
    r <- run_survivalcont(data = d, outcome = "ev", contexpl = "m", tint = TRUE, dxdate = "dx", fudate = "fu")
    expect_false(grepl("no unit conversion", sc_rr_text(r), fixed = TRUE))
})

test_that("F3: the adjusted p matches its published sources and uses the cuts actually searched", {
    skip_if_not_installed("maxstat")
    p_of <- ClinicoPath:::survivalcontClass$private_methods$.selectionAdjustedP

    # Lausen, Sauerbrei and Schumacher (1994), p. 489, as pinned by ?maxstat::pLausen94.
    expect_equal(round(p_of(2.5, 20, 5:15, 0.25), 3), 0.073)
    # Hollander, Sauerbrei and Schumacher (2004), formula 3: pmin 0.007, 10% trimming gives 0.123,
    # and 3.054 is the 5% point.
    expect_equal(round(p_of(qnorm(1 - 0.007 / 2), 1000, 100:900, 0.1), 3), 0.123)
    expect_equal(round(p_of(3.054, 1000, 100:900, 0.1), 2), 0.05)
    for (b in c(1.5, 2.5, 3.5)) {
        expect_equal(p_of(b, 150, 15:135, 0.1),
                     min(1, maxstat::pLausen92(b, 0.1, 0.9), maxstat::pLausen94(b, 150, 0.1, 0.9, m = 15:135)))
        expect_equal(p_of(b, 150, c(30, 60, 90, 120), 0.1),
                     min(1, maxstat::pLausen92(b, 0.1, 0.9), maxstat::pLausen94(b, 150, 0.1, 0.9, m = c(30, 60, 90, 120))))
    }

    table_p <- function(d) {
        r <- sc_rr(d, findcut = TRUE)
        mx <- maxstat::maxstat.test(survival::Surv(time, ev) ~ m, d, smethod = "LogRank",
                                    pmethod = "Lau94", minprop = 0.1, maxprop = 0.9)
        expect_equal(r$rescutTable$asDF$statistic, as.numeric(mx$statistic))
        expect_equal(r$rescutTable$asDF$padjusted,
                     min(1, maxstat::pLausen92(as.numeric(mx$statistic), 0.1, 0.9), as.numeric(mx$p.value)))
    }
    d <- sc_rr_data()
    table_p(d)
    # Tied marker: only the distinct cuts between the 10% and 90% ranks count.
    d$m <- round(d$m / 5) * 5
    table_p(d)
})

test_that("F3: adjusted p is never below the pointwise p and never above 1", {
    p_of <- ClinicoPath:::survivalcontClass$private_methods$.selectionAdjustedP
    expect_equal(p_of(0.2, 150, 15:135, 0.1), 1)
    expect_gte(p_of(9, 150, c(20, 100), 0.1), 2 * pnorm(9, lower.tail = FALSE))
    expect_equal(p_of(2, 150, 75, 0.1), 2 * pnorm(2, lower.tail = FALSE))
})

test_that("F3: the note says what a later p-value on the groups is worth", {
    note <- sc_rr_notes(sc_rr(sc_rr_data(), findcut = TRUE)$rescutTable)
    expect_match(note, "not a valid test", fixed = TRUE)
    expect_match(note, "exporting", fixed = TRUE)
})

test_that("F3: an outcome-guided multiple cut-off p is labelled unadjusted; a quantile one is not", {
    d <- sc_rr_data()
    guided <- sc_rr(d, multiple_cutoffs = TRUE, cutoff_method = "minpval", num_cutoffs = "two")
    note <- guided$multipleCutTable$notes$logrank$note
    expect_match(note, "Overall Log-rank Test (unadjusted)", fixed = TRUE)
    expect_match(note, "not a valid test", fixed = TRUE)
    plain <- sc_rr(d, multiple_cutoffs = TRUE, cutoff_method = "quantile", num_cutoffs = "two")
    expect_false(grepl("(unadjusted)", plain$multipleCutTable$notes$logrank$note, fixed = TRUE))
})

test_that("F3: the multiple cut-off plot prints a p only for cuts that ignored the outcome", {
    src <- sc_rr_src()
    expect_match(src, "pval = !outcome_guided", fixed = TRUE)
    expect_false(grepl("pval = TRUE,\n", gsub("# pval = TRUE,\n", "", src, fixed = TRUE), fixed = TRUE))
})

test_that("F4: the rate table carries its denominator", {
    r <- sc_rr(sc_rr_data(), person_time = TRUE, rate_multiplier = 1000L)
    expect_match(sc_rr_notes(r$personTimeTable), "per 1000 person-months", fixed = TRUE)
    r <- sc_rr(sc_rr_data(), person_time = TRUE, rate_multiplier = 100000L, timetypeoutput = "days")
    expect_match(sc_rr_notes(r$personTimeTable), "per 100000 person-days", fixed = TRUE)
})

test_that("F5: a small or precise cut-off survives into the table and the sentence", {
    d <- sc_rr_data()
    d$fr <- (d$m - 20) / 40000
    r <- run_survivalcont(data = d, elapsedtime = "time", outcome = "ev", contexpl = "fr",
                          findcut = TRUE, multiple_cutoffs = TRUE, cutoff_method = "quantile")
    q <- unname(quantile(d$fr, c(1 / 3, 2 / 3)))
    expect_equal(r$multipleCutTable$asDF$cutpoint_value, signif(q, 6))
    expect_match(r$rescutTable$notes$clinical$note,
                 formatC(r$rescutTable$asDF$cutpoint, digits = 6, format = "fg"), fixed = TRUE)
})

test_that("F6: cumulative event and hazard curves have a key", {
    src <- sc_rr_src()
    expect_false(grepl('legend = "none"', src, fixed = TRUE))
    expect_equal(lengths(regmatches(src, gregexpr('legend = "top"', src, fixed = TRUE))), 2L)
})

test_that("F7: rows with a missing time are counted when a landmark is set", {
    d <- sc_rr_data()
    d$time <- d$time + 5
    d$time[1:7] <- NA
    r <- sc_rr(d, uselandmark = TRUE, landmark = 3)
    expect_match(sc_rr_text(r), "7 of 240 record(s) were excluded because survival time was missing", fixed = TRUE)
})

test_that("cross F2: an event exactly at the landmark is out, and fractional landmarks work", {
    d <- sc_rr_data()
    d$time <- pmax(1, round(d$time))
    keep <- d$time > 12
    r <- sc_rr(d, uselandmark = TRUE, landmark = 12, person_time = TRUE)
    expect_equal(r$personTimeTable$asDF$events[1], sum(d$ev[keep]))
    expect_equal(r$personTimeTable$asDF$person_time[1], round(sum(d$time[keep] - 12), 2))

    r <- sc_rr(d, uselandmark = TRUE, landmark = 0.5, person_time = TRUE)
    expect_equal(r$personTimeTable$asDF$person_time[1], round(sum(d$time - 0.5), 2))
})

test_that("F8 and cross F3: numeric dates that are not epoch seconds are refused", {
    d <- sc_rr_data(n = 120)
    start <- as.Date("2015-01-10") + (1:120 * 7)
    end <- start + round(d$time * 30.4375)
    shapes <- list(
        yyyymmdd = function(x) as.integer(format(x, "%Y%m%d")),
        days = function(x) as.numeric(x),
        excel = function(x) as.numeric(x) + 25569
    )
    for (shape in names(shapes)) {
        d$dx <- shapes[[shape]](start)
        d$fu <- shapes[[shape]](end)
        expect_error(
            run_survivalcont(data = d, outcome = "ev", contexpl = "m", tint = TRUE,
                             dxdate = "dx", fudate = "fu"),
            "cannot be read as dates", fixed = TRUE)
    }
})

test_that("cross F4: a sentinel date is refused", {
    d <- sc_rr_data(n = 120)
    d$dx <- format(as.Date("2015-01-10") + (1:120 * 7), "%Y-%m-%d")
    d$fu <- format(as.Date("2015-01-10") + (1:120 * 7) + round(d$time * 30), "%Y-%m-%d")
    d$fu[5] <- "2199-12-31"
    expect_error(
        run_survivalcont(data = d, outcome = "ev", contexpl = "m", tint = TRUE, dxdate = "dx", fudate = "fu"),
        "1 dates in fu fall outside a plausible range (for example 2199-12-31)", fixed = TRUE)
})

test_that("cross F5: unreadable dates give the shared warning with resolved counts", {
    d <- sc_rr_data(n = 120)
    d$dx <- format(as.Date("2015-01-10") + (1:120 * 7), "%Y-%m-%d")
    d$fu <- format(as.Date("2015-01-10") + (1:120 * 7) + round(d$time * 30), "%Y-%m-%d")
    d$fu[1:10] <- "not a date"
    r <- run_survivalcont(data = d, outcome = "ev", contexpl = "m", tint = TRUE, dxdate = "dx", fudate = "fu")
    expect_match(strip_survivalcont_html(r$warnings$content),
                 "10 of 240 dates could not be read, so those patients were excluded. Check the date format:",
                 fixed = TRUE)
})

test_that("F9: a quantile fallback does not carry the search method's name", {
    r <- sc_rr(sc_rr_data(), multiple_cutoffs = TRUE, cutoff_method = "minpval",
               num_cutoffs = "four", min_group_size = 18)
    expect_match(r$plotMultipleCutoffs$state$method, "^quantile")
    # Of the sampled sets, say how many the group-size rule let through.
    r <- sc_rr(sc_rr_data(), multiple_cutoffs = TRUE, cutoff_method = "minpval",
               num_cutoffs = "four", min_group_size = 15)
    info <- strip_survivalcont_html(r$infoMessages$content)
    n <- as.integer(sub(".*and the ([0-9]+) that met the minimum group size.*", "\\1", info))
    expect_lt(n, 1000L)
})

test_that("F10: survival of 0% inside follow-up is not called beyond follow-up", {
    set.seed(5)
    d <- data.frame(m = c(rnorm(30, 80, 2), rnorm(90, 40, 5)))
    d$time <- c(seq(2, 35, length.out = 29), 36, runif(90, 20, 80))
    d$ev <- c(rep(1L, 30), rbinom(90, 1, 0.5))
    r <- sc_rr(d, findcut = TRUE, cutp = "24, 36")
    expect_false(any(grepl("Beyond the observed", capture.output(print(r$survTable)), fixed = TRUE)))
})

test_that("cross F12: a curve that reached 0 stays 0 at later time points", {
    set.seed(5)
    d <- data.frame(m = c(rnorm(30, 80, 2), rnorm(90, 40, 5)))
    d$time <- c(seq(2, 35, length.out = 29), 36, runif(90, 20, 80))
    d$ev <- c(rep(1L, 30), rbinom(90, 1, 0.5))
    tb <- sc_rr(d, findcut = TRUE, cutp = "48")$survTable$asDF
    expect_equal(tb$surv[grepl("high", tb$strata)], 0)
})

test_that("F11: a marker named like a surv_cutpoint field gets its cut-off", {
    d <- sc_rr_data()
    ref <- sc_rr(d, findcut = TRUE, multiple_cutoffs = TRUE, cutoff_method = "recursive")
    for (nm in c("cutpoint", "data", "minprop")) {
        dd <- d
        names(dd)[names(dd) == "m"] <- nm
        r <- run_survivalcont(data = dd, elapsedtime = "time", outcome = "ev", contexpl = nm,
                              findcut = TRUE, multiple_cutoffs = TRUE, cutoff_method = "recursive")
        expect_equal(r$rescutTable$asDF$cutpoint, ref$rescutTable$asDF$cutpoint)
        expect_equal(r$medianTable$rowCount, 2L)
        expect_equal(r$multipleCutTable$asDF$cutpoint_value, ref$multipleCutTable$asDF$cutpoint_value)
    }
})

test_that("F12 and the landmark rule: every time-based output names the time origin", {
    d <- sc_rr_data()
    r <- sc_rr(d, uselandmark = TRUE, landmark = 3, residual_diagnostics = TRUE, findcut = TRUE,
               showSummaries = TRUE, rmst_analysis = TRUE, rmst_tau = 30)
    expect_match(sc_rr_notes(r$schoenfeldResidualsTable), "measured from the landmark at 3 months", fixed = TRUE)
    for (text in list(r$medianSummary$content, r$survTableSummary$content, r$rmstSummary$content))
        expect_match(paste(text, collapse = " "), "measured from the landmark at 3 months", fixed = TRUE)
})

test_that("F13 and F14: the statistic and the events column say what they are", {
    r <- sc_rr(sc_rr_data(), findcut = TRUE)
    expect_match(sc_rr_notes(r$rescutTable), "permutation variance", fixed = TRUE)
    expect_identical(r$survTable$getColumn("n.event")$title, "Events since previous time")
})

test_that("cross F8: median tables state how their CI was made", {
    r <- sc_rr(sc_rr_data(), findcut = TRUE, multiple_cutoffs = TRUE, cutoff_method = "quantile")
    expect_match(sc_rr_notes(r$medianTable), "Brookmeyer-Crowley", fixed = TRUE)
    expect_match(sc_rr_notes(r$multipleMedianTable), "Brookmeyer-Crowley", fixed = TRUE)
})

test_that("median follow-up under Cause Specific does not treat other-cause deaths as still observed", {
    set.seed(2); n <- 200
    d <- data.frame(id = seq_len(n), m = rnorm(n, 50, 10), status = rep(c("DOC", "Alive", "DOD"), c(120, 60, 20)))
    d$time <- c(runif(120, 1, 3), runif(60, 24, 36), runif(20, 4, 20))
    r <- run_survivalcont(
        data = d, elapsedtime = "time", outcome = "status", contexpl = "m", outcomeLevel = "DOD",
        multievent = TRUE, analysistype = "cause", dod = "DOD", dooc = "DOC", awd = NULL, awod = "Alive")
    expect_no_match(paste(r$clinicalWarnings$content, collapse = " "), "Short median follow-up", fixed = TRUE)
})

test_that("a small incidence rate keeps its value in the table", {
  d <- sc_rr_data()
  d$time <- d$time * 30.44
  r <- sc_rr(d, person_time = TRUE, rate_multiplier = 1, timetypeoutput = "days")
  pt <- r$personTimeTable$asDF
  expect_equal(pt$rate[1], sum(d$ev == 1) / sum(d$time), tolerance = 1e-8)
  expect_gt(pt$rate_ci_lower[1], 0)
})
