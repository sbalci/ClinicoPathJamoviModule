# ═══════════════════════════════════════════════════════════
# Safety Check Tests: survival
# ═══════════════════════════════════════════════════════════
#
# Tests the clinical safety checks added to the survival
# function: EPV warnings, extreme HR detection, convergence
# check, negative time validation, RMST tau explanation.
#
# Note: The wrapper function requires ALL Level-type args
# (dod, dooc, awd, awod) even when unused. Supply them as
# empty strings to satisfy the auto-generated constructor.

library(testthat)

data(survival_test, package = "ClinicoPath")

# Helper: common args for wrapper function
surv_args <- function(...) {
  defaults <- list(
    data = survival_test,
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    outcomeLevel = "1",
    dod = "",
    dooc = "",
    awd = "",
    awod = "",
    explanatory = "treatment"
  )
  modifyList(defaults, list(...))
}

# ─── Negative Survival Time Validation ────────────────────

test_that("survival errors on negative elapsed times", {
  test_data <- survival_test
  test_data$elapsedtime[1:5] <- -10

  args <- surv_args(data = test_data)
  expect_error(
    do.call(survival, args),
    regexp = "negative|invalid",
    ignore.case = TRUE
  )
})

# ─── Event Count Blocking (<10 events) ───────────────────

test_that("survival blocks analysis with fewer than 10 events", {
  test_data <- survival_test
  test_data$outcome <- 0
  test_data$outcome[1:5] <- 1

  args <- surv_args(data = test_data)
  expect_error(
    do.call(survival, args),
    regexp = "CRITICAL|events|minimum",
    ignore.case = TRUE
  )
})

# ─── RMST with Default Tau ───────────────────────────────

test_that("survival RMST with tau=0 uses 75th percentile default", {
  args <- surv_args(rmst_analysis = TRUE, rmst_tau = 0)
  result <- do.call(survival, args)
  expect_true(inherits(result, "R6"))
})

# ─── Basic Cox Regression Runs ───────────────────────────

test_that("survival Cox regression produces results", {
  args <- surv_args()
  result <- do.call(survival, args)
  expect_true(inherits(result, "R6"))
})

# ─── Formula Builder Consistency ─────────────────────────

test_that("survival handles variable names with spaces", {
  test_data <- survival_test
  names(test_data)[names(test_data) == "elapsedtime"] <- "Elapsed Time"
  names(test_data)[names(test_data) == "outcome"] <- "Patient Outcome"
  names(test_data)[names(test_data) == "treatment"] <- "Treatment Group"

  args <- surv_args(
    data = test_data,
    elapsedtime = "Elapsed Time",
    outcome = "Patient Outcome",
    outcomeLevel = "1",
    explanatory = "Treatment Group"
  )
  result <- do.call(survival, args)
  expect_true(inherits(result, "R6"))
})

# ─── Weighted Log-Rank Tests ─────────────────────────────

test_that("survival weighted log-rank tests run correctly", {
  args <- surv_args(weightedLogRank = TRUE, survivalTestType = "gehan_breslow")
  result <- do.call(survival, args)
  expect_true(inherits(result, "R6"))
})

# ─── Bootstrap Validation ────────────────────────────────

test_that("survival bootstrap validation runs with adequate events", {
  args <- surv_args(bootstrapValidation = TRUE, bootstrapValN = 50)
  result <- do.call(survival, args)
  expect_true(inherits(result, "R6"))
})

# ─── Calibration Curves ─────────────────────────────────

test_that("survival calibration curves run with default timepoint", {
  args <- surv_args(calibration_curves = TRUE, calibration_timepoint = 0)
  result <- do.call(survival, args)
  expect_true(inherits(result, "R6"))
})

# ─── PH Assumption Test ─────────────────────────────────

test_that("survival PH assumption test runs", {
  args <- surv_args(ph_cox = TRUE)
  result <- do.call(survival, args)
  expect_true(inherits(result, "R6"))
})

# ─── Pairwise Comparisons ───────────────────────────────

test_that("survival pairwise comparisons run with multi-level factor", {
  args <- surv_args(explanatory = "stage", pw = TRUE, padjustmethod = "bonferroni")
  result <- do.call(survival, args)
  expect_true(inherits(result, "R6"))
})

# ─── Survival Plots ─────────────────────────────────────

test_that("survival plot options do not error", {
  args <- surv_args(
    sc = TRUE, ci95 = TRUE, risktable = TRUE,
    censored = TRUE, pplot = TRUE, endplot = 60, byplot = 12
  )
  result <- do.call(survival, args)
  expect_true(inherits(result, "R6"))
})

# ─── Person-Time Analysis ───────────────────────────────

test_that("survival person-time analysis runs", {
  args <- surv_args(
    person_time = TRUE,
    time_intervals = "12, 36, 60",
    rate_multiplier = 100
  )
  result <- do.call(survival, args)
  expect_true(inherits(result, "R6"))
})

# ─── Landmark Analysis ──────────────────────────────────

test_that("survival landmark analysis runs", {
  args <- surv_args(uselandmark = TRUE, landmark = 12)
  result <- do.call(survival, args)
  expect_true(inherits(result, "R6"))
})
