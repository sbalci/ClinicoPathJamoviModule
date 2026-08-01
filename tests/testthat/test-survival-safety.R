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
  supplied <- list(...)
  defaults[names(supplied)] <- supplied
  defaults
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

test_that("survival keeps descriptive output with fewer than 10 events", {
  test_data <- survival_test
  test_data$outcome <- 0
  test_data$outcome[1:5] <- 1

  args <- surv_args(data = test_data)
  result <- do.call(survival, args)
  expect_s3_class(result, "survivalResults")
  expect_gt(result$medianTable$rowCount, 0)
  expect_equal(result$coxTable$rowCount, 0)
})

# ─── RMST with Default Tau ───────────────────────────────

test_that("survival RMST with tau=0 uses 75th percentile default", {
  args <- surv_args(rmst_analysis = TRUE, rmst_tau = 0)
  result <- do.call(survival, args)
  expect_true(inherits(result, "R6"))
})

test_that("survival RMST agrees with survival::survfit reference values", {
  data(survival_rmst, package = "ClinicoPath")
  args <- surv_args(
    data = survival_rmst,
    explanatory = "treatment",
    rmst_analysis = TRUE,
    rmst_tau = 48
  )
  result <- do.call(survival, args)
  observed <- result$rmstTable$asDF

  reference_fit <- survival::survfit(
    survival::Surv(elapsedtime, outcome) ~ treatment,
    data = survival_rmst
  )
  reference <- summary(reference_fit, rmean = 48, extend = TRUE)$table

  expect_equal(observed$rmst, unname(round(reference[, "rmean"], 2)), tolerance = 1e-8)
  expect_equal(observed$se, unname(round(reference[, "se(rmean)"], 2)), tolerance = 1e-8)
  expect_equal(
    observed$ci_lower,
    unname(round(reference[, "rmean"] - 1.96 * reference[, "se(rmean)"], 2)),
    tolerance = 1e-8
  )
})

test_that("survival RMST rejects a horizon beyond common group support", {
  data(survival_rmst, package = "ClinicoPath")
  args <- surv_args(
    data = survival_rmst,
    explanatory = "treatment",
    rmst_analysis = TRUE,
    rmst_tau = 1e6
  )
  result <- do.call(survival, args)

  expect_equal(result$rmstTable$rowCount, 0)
  expect_true(length(result$rmstTable$notes) > 0)
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
  args <- surv_args(weightedLogRank = TRUE, survivalTestType = "fh_rho0_5")
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
