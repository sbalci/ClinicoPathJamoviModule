# Reverse Kaplan-Meier median follow-up (R/survival_utils.R).
#
# The helper these tests cover replaced fifteen call sites that reported
# median(observed time) under a "median follow-up" label. The first test is the
# one that would have caught that: it builds a cohort whose TRUE median
# potential follow-up is known by construction and asserts the naive estimator
# misses it while the reverse-KM estimate covers it.

# .medianFollowUp lives in a plain utility file, not a namespace-exported
# object, so bind it the same way whether we are under load_all or an installed
# build. Sourcing directly keeps the test independent of collation order.
if (!exists(".medianFollowUp"))
  source(file.path("..", "..", "R", "survival_utils.R"))

simulate_cohort <- function(n, event_scale, max_followup, seed) {
  set.seed(seed)
  potential <- stats::runif(n, 0, max_followup)
  event_at  <- stats::rexp(n, rate = 1 / event_scale)
  list(time     = pmin(event_at, potential),
       censored = as.integer(potential <= event_at),
       truth    = stats::median(potential))
}

test_that("reverse KM recovers median potential follow-up where median(time) does not", {
  skip_if_not_installed("survival")

  scenarios <- list(
    list(tag = "heavy early events", n = 2000, scale =  6, maxfu = 48, seed = 42),
    list(tag = "moderate events",    n = 2000, scale = 24, maxfu = 48, seed = 43),
    list(tag = "few events",         n = 2000, scale = 60, maxfu = 48, seed = 44),
    list(tag = "smaller cohort",     n =  400, scale = 12, maxfu = 36, seed = 45)
  )

  for (s in scenarios) {
    d <- simulate_cohort(s$n, s$scale, s$maxfu, s$seed)
    mfu <- .medianFollowUp(d$time, d$censored)

    expect_true(mfu$reverse, info = s$tag)
    expect_identical(mfu$method, "reverse_km", info = s$tag)

    # The estimate's interval covers the truth ...
    expect_lte(mfu$ci_lower, d$truth)
    expect_gte(mfu$ci_upper, d$truth)

    # ... and the naive estimator this replaced does not even come close: it is
    # biased DOWNWARD, badly, because it is really the median time to
    # event-or-censoring.
    naive <- stats::median(d$time)
    expect_lt(naive, d$truth)
    expect_lt(naive, mfu$value)
  }
})

test_that("median among censored subjects only is biased and is not what we compute", {
  skip_if_not_installed("survival")
  d <- simulate_cohort(2000, 6, 48, seed = 42)
  # progressionsurvival.b.R used to do exactly this.
  censored_only <- stats::median(d$time[d$censored == 1])
  mfu <- .medianFollowUp(d$time, d$censored)
  expect_false(isTRUE(all.equal(censored_only, mfu$value)))
})

test_that("competing events must not be counted as reverse-KM events", {
  skip_if_not_installed("survival")
  set.seed(7)
  n <- 1500
  potential <- stats::runif(n, 0, 40)
  event_at  <- stats::rexp(n, rate = 1 / 20)
  time      <- pmin(event_at, potential)
  # A third of the terminal outcomes are a COMPETING death rather than the
  # event of interest. Neither ends potential follow-up any differently, so
  # neither may be marked censored.
  terminal  <- event_at < potential
  competing <- terminal & stats::runif(n) < 0.33

  correct <- .medianFollowUp(time, as.integer(!terminal))
  # The bug this guards: treating "not the event of interest" as censoring,
  # which sweeps competing deaths into the reverse-KM event set.
  wrong   <- .medianFollowUp(time, as.integer(!terminal | competing))

  expect_true(correct$reverse)
  expect_lt(wrong$value, correct$value)   # understated, as the comment claims
})

test_that("not-estimable cases fall back honestly and say why", {
  skip_if_not_installed("survival")
  time <- stats::runif(500, 0, 30)

  # Non-estimability is driven by WHEN subjects were still under observation,
  # not by how many: 5% censored late is estimable, while half the cohort
  # censored early is not. Use a configuration that is reliably undefined --
  # the censored subjects are the earliest-leaving ones, so the reversed curve
  # takes all its steps while the risk set is still large and never reaches 50%.
  time <- sort(time)
  cases <- list(
    "no censoring"           = rep(0L, 500),
    "unusable indicator"     = NULL,
    "censored subjects left early" = as.integer(seq_len(500) <= 240),
    "length mismatch"        = rep(0L, 10)
  )

  for (nm in names(cases)) {
    mfu <- .medianFollowUp(time, cases[[nm]])
    expect_false(mfu$reverse, info = nm)
    expect_identical(mfu$method, "observed_median", info = nm)
    expect_true(nzchar(mfu$reason), info = nm)
    expect_equal(mfu$value, stats::median(time), info = nm)
    # The label must never claim reverse KM for a fallback value.
    expect_false(grepl("Median follow-up", .medianFollowUpLabel(mfu)), info = nm)
  }
})

test_that("label, text and explanation stay consistent with the method used", {
  skip_if_not_installed("survival")
  d <- simulate_cohort(2000, 24, 48, seed = 43)
  ok <- .medianFollowUp(d$time, d$censored)

  expect_match(.medianFollowUpLabel(ok), "reverse Kaplan-Meier", fixed = TRUE)
  expect_match(.medianFollowUpText(ok, "months"), "^[0-9.]+ months \\(95% CI ")

  html_ok <- .medianFollowUpExplanation(ok, "months")
  expect_match(html_ok, "How this was calculated", fixed = TRUE)
  expect_match(html_ok, "Schemper", fixed = TRUE)
  expect_match(html_ok, "0197-2456", fixed = TRUE)     # the DOI must be present

  bad <- .medianFollowUp(d$time, rep(0L, length(d$time)))
  html_bad <- .medianFollowUpExplanation(bad, "months")
  expect_match(html_bad, "Why a fallback is shown", fixed = TRUE)
  expect_false(grepl("How this was calculated", html_bad, fixed = TRUE))

  # Theme safety: the block must not paint an opaque background without also
  # setting a text colour, which is unreadable in jamovi's dark theme.
  expect_false(grepl("background-color:\\s*#", html_ok))
})

test_that("degenerate inputs do not error", {
  skip_if_not_installed("survival")
  expect_false(.medianFollowUp(numeric(0), integer(0))$reverse)
  expect_false(.medianFollowUp(c(NA, NA), c(1, 1))$reverse)
  expect_false(.medianFollowUp(c(-1, -2), c(1, 1))$reverse)
  expect_silent(.medianFollowUp(c(1, 2, 3), c(TRUE, FALSE, TRUE)))
  # Logical and 0/1 codings must agree.
  expect_equal(.medianFollowUp(1:10, rep(c(TRUE, FALSE), 5))$value,
               .medianFollowUp(1:10, rep(c(1L, 0L), 5))$value)
})

test_that("summary()$table median matches quantile(fit, 0.5) -- swimmerplot's old estimator", {
  skip_if_not_installed("survival")
  # swimmerplot previously read the median off stats::quantile(km_fit, 0.5) while
  # singlearm/multisurvival read it off summary(fit)$table[["median"]]. The
  # shared helper standardises on the latter, so the two must agree or the
  # consolidation silently changed swimmerplot's published numbers.
  cmp <- function(fu, ev) {
    fit <- survival::survfit(survival::Surv(fu, 1 - ev) ~ 1)
    via_summary  <- unname(summary(fit)$table[["median"]])
    via_quantile <- unname(stats::quantile(fit, 0.5)$quantile)
    helper       <- .medianFollowUp(fu, ev == 0)$value
    expect_equal(via_summary, via_quantile)
    expect_equal(via_summary, helper)
  }
  # The exact vectors asserted on by test-swimmerplot-release-review.R.
  cmp(c(2, 3, 4, 5, 6, 7, 18, 20, 22, 24, 26, 28), c(rep(1, 6), rep(0, 6)))
  cmp(c(4, 6, 8, 10, 12, 18, 24, 30), c(0, 0, 0, 1, 1, 1, 0, 0))

  set.seed(3)
  for (i in 1:6) {
    n <- sample(20:300, 1)
    cmp(round(stats::runif(n, 1, 60), 2), stats::rbinom(n, 1, stats::runif(1, .2, .8)))
  }
})

test_that("the naive estimator understates follow-up badly at a realistic event rate", {
  skip_if_not_installed("survival")
  # survivalcont graded median(time) against a "short follow-up" threshold, so
  # this gap is what made that warning fire on adequate cohorts.
  set.seed(2026)
  n <- 240
  marker <- round(stats::rnorm(n, 50, 12), 1)
  t_ev  <- stats::rexp(n, rate = 0.012 * exp(0.045 * (marker - 50)))
  t_cen <- stats::rexp(n, rate = 0.006)
  time   <- round(pmin(t_ev, t_cen) + 0.5, 2)
  status <- as.integer(t_ev <= t_cen)

  expect_gt(mean(status), 0.6)                      # high-event cohort
  mfu <- .medianFollowUp(time, status == 0)
  expect_true(mfu$reverse)
  expect_gt(mfu$value, 3 * stats::median(time))     # naive is >3x too small
})
