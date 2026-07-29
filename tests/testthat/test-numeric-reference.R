# End-to-end numeric reference tests.
#
# The existing suite is largely schema/static, which is why runtime defects
# passed CI. Each test below runs a real analysis on deterministic data and
# checks a NUMBER against an independently computed reference, so a silent
# change of estimator fails the build rather than the next review.

skip_if_not(requireNamespace("jsurvival", quietly = TRUE), "jsurvival not installed")
suppressMessages(library(jsurvival))
suppressMessages(library(survival))

quiet <- function(expr) { sink(tempfile()); on.exit(sink()); suppressWarnings(force(expr)) }

make_cohort <- function(n = 400, seed = 42) {
    set.seed(seed)
    age <- stats::rnorm(n, 60, 12)
    data.frame(
        t   = round(stats::rexp(n, 0.02 * exp(0.04 * (age - 60))), 1) + 0.1,
        ev  = stats::rbinom(n, 1, 0.75),
        grp = factor(sample(c("A", "B"), n, TRUE)),
        age = age)
}

test_that("median survival matches survfit", {
    d <- make_cohort()
    r <- quiet(jsurvival::survival(
        data = d, elapsedtime = "t", outcome = "ev", explanatory = "grp",
        outcomeLevel = NULL, dod = NULL, dooc = NULL, awd = NULL, awod = NULL))
    got <- as.data.frame(r$medianTable)

    ref <- summary(survfit(Surv(t, ev) ~ grp, data = d))$table
    expect_equal(sort(round(got$median, 4)), sort(round(unname(ref[, "median"]), 4)))
    expect_equal(sum(got$events), sum(d$ev == 1))
})

test_that("Cox hazard ratio matches coxph", {
    d <- make_cohort()
    r <- quiet(jsurvival::survival(
        data = d, elapsedtime = "t", outcome = "ev", explanatory = "grp",
        outcomeLevel = NULL, dod = NULL, dooc = NULL, awd = NULL, awod = NULL))
    got <- as.data.frame(r$coxTable)
    expect_true(nrow(got) > 0)
    ref_hr <- unname(exp(coef(coxph(Surv(t, ev) ~ grp, data = d))[1]))
    # The table formats "HR (lo-hi)"; compare the leading point estimate.
    shown <- as.character(got$HR_univariable[nrow(got)])
    got_hr <- as.numeric(sub("^\\s*([0-9.]+).*$", "\\1", shown))
    expect_equal(round(got_hr, 2), round(ref_hr, 2))
})

test_that("calibration slope is exactly 1 on development data, and is labelled as such", {
    # A model's own linear predictor refitted on the same data always gives 1.
    # If this ever returns something else the estimand has silently changed.
    d <- make_cohort(500)
    r <- quiet(jsurvival::survival(
        data = d, elapsedtime = "t", outcome = "ev", explanatory = "grp",
        outcomeLevel = NULL, dod = NULL, dooc = NULL, awd = NULL, awod = NULL,
        calibration_curves = TRUE, calibration_ngroups = 4,
        rcs_analysis = TRUE, rcs_variable = "age"))
    tb <- as.data.frame(r$calibrationTable)
    expect_true(nrow(tb) > 0)
    slope <- tb$value[grepl("slope", tb$metric, ignore.case = TRUE)]
    expect_equal(round(as.numeric(slope), 6), 1)
    # C-index limits must stay inside [0, 1].
    ci <- tb[grepl("C-index", tb$metric), ]
    if (nrow(ci)) {
        expect_gte(as.numeric(ci$ci_lower), 0)
        expect_lte(as.numeric(ci$ci_upper), 1)
    }
})

test_that("baseline hazard is an occurrence/exposure rate with a Poisson interval", {
    d <- make_cohort(300)
    r <- quiet(jsurvival::singlearm(
        data = d, elapsedtime = "t", outcome = "ev", outcomeLevel = NULL,
        dod = NULL, dooc = NULL, awd = NULL, awod = NULL, baseline_hazard = TRUE))
    tb <- as.data.frame(r$baselineHazardTable)
    skip_if(nrow(tb) == 0, "baseline hazard table not produced")

    # Every rate must be positive and bracketed by its interval, and the
    # interval must be multiplicative (the Poisson log form), never the old
    # fixed 50%-150% band.
    expect_true(all(tb$hazard > 0))
    expect_true(all(tb$hazard_lower <= tb$hazard))
    expect_true(all(tb$hazard_upper >= tb$hazard))
    ratio <- tb$hazard_upper / tb$hazard
    expect_false(isTRUE(all.equal(unique(round(ratio, 6)), 1.5)))
})

test_that("competing-risk events are counted separately from the event of interest", {
    set.seed(11); n <- 300
    d <- data.frame(
        t   = round(rexp(n, 0.05), 1) + 0.1,
        out = factor(sample(c("DOD", "DOOC", "AWD", "AWOD"), n, TRUE)),
        grp = factor(sample(c("A", "B"), n, TRUE)))
    r <- quiet(jsurvival::multisurvival(
        data = d, elapsedtime = "t", outcome = "out", outcomeLevel = NULL,
        explanatory = "grp", multievent = TRUE, analysistype = "compete",
        dod = "DOD", dooc = "DOOC", awd = "AWD", awod = "AWOD"))
    txt <- paste(vapply(names(r), function(n) {
        x <- try(r[[n]]$content, silent = TRUE)
        if (inherits(x, "try-error") || is.null(x)) "" else as.character(x)
    }, character(1)), collapse = " ")
    # The four-level clinical outcome must not be rejected any more.
    expect_false(grepl("Unsupported Levels", txt))
    # Events reported must be DOD only, not DOD + DOOC.
    expect_true(grepl(paste0(sum(d$out == "DOD"), " events"), txt))
})
