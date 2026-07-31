# Regression tests for BH-1 / BH-2: baseline hazard summaries and plots.
#
# The summary statistics used to average per-interval rates d_i / (n_risk * dt_i)
# without weighting by exposure. Interval widths shrink toward zero, and 1/dt has
# no finite expectation, so the mean, the peak and the SD were all driven by
# whichever interval happened to be narrowest. On data simulated with a perfectly
# CONSTANT hazard of 0.05 the old code reported a mean of 0.173 (3.5x too high),
# a peak of 5.26 (105x too high) and a coefficient of variation of 2.2 -- which,
# against the 0.5 threshold the module uses, told the clinician the hazard was
# "highly variable" when by construction it was constant.

.bh_ns <- NULL
for (.p in c("ClinicoPath", "jsurvival")) {
    if (.p %in% loadedNamespaces() || requireNamespace(.p, quietly = TRUE)) {
        .cand <- asNamespace(.p)
        if (exists("singlearm", envir = .cand, inherits = FALSE)) {
            .bh_ns <- .cand
            break
        }
    }
}
skip_if(is.null(.bh_ns), "singlearm not available in this distribution")

quiet <- function(expr) { sink(tempfile()); on.exit(sink()); suppressWarnings(force(expr)) }

.bh_data <- function(lambda = 0.05, n = 500, seed = 7) {
    set.seed(seed)
    tt <- rexp(n, lambda); cens <- rexp(n, 0.01)
    data.frame(t = round(pmin(tt, cens), 2) + 0.01, ev = as.integer(tt <= cens))
}

test_that("the mean baseline hazard recovers the true constant rate", {
    lambda <- 0.05
    d <- .bh_data(lambda)

    r <- quiet(get("singlearm", envir = .bh_ns)(
        data = d, elapsedtime = "t", outcome = "ev", outcomeLevel = NULL,
        dod = NULL, dooc = NULL, awd = NULL, awod = NULL,
        baseline_hazard = TRUE, showSummaries = TRUE))

    txt <- as.character(r$baselineHazardSummary$content)
    txt <- gsub("[[:space:]]+", " ", gsub("<[^>]*>", " ", txt))
    skip_if(!nzchar(trimws(txt)), "summary not rendered in this harness")

    grab <- function(label) {
        m <- regmatches(txt, regexpr(paste0(label, "[^0-9]{0,50}[0-9.]+"), txt))
        if (length(m) == 0) return(NA_real_)
        as.numeric(regmatches(m, regexpr("[0-9.]+$", m)))
    }

    mean_hz <- grab("Pooled event rate")
    expect_false(is.na(mean_hz))

    # The pooled occurrence/exposure rate is the correct estimator, and on this
    # data it equals the simulated lambda to three decimals.
    pooled <- sum(d$ev) / sum(d$t)
    expect_equal(mean_hz, round(pooled, 4), tolerance = 1e-3)
    expect_equal(mean_hz, lambda, tolerance = 0.005)

    # The old unweighted mean was 0.1728 here. Anything near that is the bug back.
    expect_lt(mean_hz, 0.10)

    # The peak must be a plausible hazard, not a one-event artefact (old: 5.26).
    peak <- grab("Peak hazard rate")
    if (!is.na(peak)) {
        expect_lt(peak, 10 * lambda)
        expect_gt(peak, 0)
    }
})

test_that("constant-hazard data is not described as highly variable", {
    d <- .bh_data(0.05)
    r <- quiet(get("singlearm", envir = .bh_ns)(
        data = d, elapsedtime = "t", outcome = "ev", outcomeLevel = NULL,
        dod = NULL, dooc = NULL, awd = NULL, awod = NULL,
        baseline_hazard = TRUE, showSummaries = TRUE))

    txt <- as.character(r$baselineHazardSummary$content)
    txt <- gsub("[[:space:]]+", " ", gsub("<[^>]*>", " ", txt))
    skip_if(!nzchar(trimws(txt)), "summary not rendered in this harness")

    # The module classifies CV < 0.5 as "little variation". With a genuinely
    # constant hazard it must not reach the "substantial variation" verdict.
    #
    # Asserted unconditionally on purpose. This used to read
    #   if (grepl("highly variable|relatively constant", txt)) expect_match(...)
    # and when the verdict wording was reworded to "little/substantial variation"
    # the guard stopped matching, the body never ran, and testthat recorded the
    # whole test as an empty SKIP -- a rewording silently disarmed the only check
    # that the constant-hazard verdict is right. n = 500 at lambda = 0.05 yields
    # 423 events / 42 candidate bins, far past the >= 3 bins the verdict needs,
    # so this branch is always reachable and a miss is a real failure.
    expect_match(txt, "little variation")
    expect_false(grepl("substantial variation", txt, fixed = TRUE))
})

test_that("the exposure-weighted rate differs from the unweighted interval mean", {
    # Documents WHY the fix was needed: the two estimators are far apart, so a
    # future refactor that reverts to mean(per-interval) fails loudly.
    d <- .bh_data(0.05)
    na <- survival::survfit(survival::Surv(t, ev) ~ 1, data = d)
    dt <- diff(c(0, na$time))
    pt <- na$n.risk * dt
    hz <- ifelse(pt > 0, na$n.event / pt, NA_real_)
    keep <- is.finite(hz) & na$n.event > 0 & pt > 0

    pooled <- sum(na$n.event) / sum(pt)
    unweighted <- mean(hz[keep])

    expect_equal(pooled, 0.05, tolerance = 0.005)   # correct estimator
    expect_gt(unweighted, 3 * pooled)               # the old one, badly biased
})

test_that("the piecewise hazard plot uses exact interval person-time", {
    gen <- get("singlearmClass", envir = .bh_ns)
    f <- gen$private_methods$.hazardIntervals
    set.seed(19)
    d <- .bh_data(lambda = 0.08, n = 240, seed = 19)
    hz <- f(d$t, d$ev)

    # Adaptive intervals form a non-overlapping partition of follow-up, so
    # exposure and events must add back to the cohort totals exactly.
    expect_equal(sum(hz$person_time), sum(d$t), tolerance = 1e-10)
    expect_equal(sum(hz$events), sum(d$ev))
    expect_equal(sum(hz$events) / sum(hz$person_time),
                 sum(d$ev) / sum(d$t), tolerance = 1e-12)
    expect_true(all(hz$lower <= hz$rate & hz$rate <= hz$upper))
})
