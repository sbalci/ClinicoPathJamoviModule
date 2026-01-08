# ═══════════════════════════════════════════════════════════
# Integration Tests: timeinterval
# ═══════════════════════════════════════════════════════════

library(testthat)
library(ClinicoPath)

data(timeinterval_test, package = "ClinicoPath")
data(timeinterval_trial, package = "ClinicoPath")
data(timeinterval_shortterm, package = "ClinicoPath")
data(timeinterval_longterm, package = "ClinicoPath")

# ═══ CLINICAL TRIAL WORKFLOWS ═══
test_that("timeinterval complete trial analysis workflow", {
  result <- timeinterval(
    data = timeinterval_trial,
    dx_date = "enrollment_date",
    fu_date = "followup_date",
    time_format = "ymd",
    output_unit = "months",
    add_times = TRUE,
    include_quality_metrics = TRUE,
    confidence_level = 95,
    show_summary = TRUE
  )
  expect_no_error(result)
})

test_that("timeinterval landmark analysis workflow", {
  result <- timeinterval(
    data = timeinterval_trial,
    dx_date = "enrollment_date",
    fu_date = "followup_date",
    use_landmark = TRUE,
    landmark_time = 6,
    output_unit = "months",
    remove_negative = TRUE,
    include_quality_metrics = TRUE
  )
  expect_no_error(result)
})

# ═══ DIFFERENT TIME SCALES ═══
test_that("timeinterval short-term study workflow", {
  result <- timeinterval(
    data = timeinterval_shortterm,
    dx_date = "admission_date",
    fu_date = "discharge_date",
    output_unit = "days",
    time_basis = "calendar"
  )
  expect_no_error(result)
})

test_that("timeinterval long-term study workflow", {
  result <- timeinterval(
    data = timeinterval_longterm,
    dx_date = "baseline_date",
    fu_date = "last_followup",
    output_unit = "years",
    time_basis = "standardized"
  )
  expect_no_error(result)
})

# ═══ DATA QUALITY PIPELINE ═══
test_that("timeinterval quality assessment pipeline", {
  result <- timeinterval(
    data = timeinterval_test,
    dx_date = "diagnosis_date",
    fu_date = "followup_date",
    remove_negative = TRUE,
    remove_extreme = TRUE,
    extreme_multiplier = 2.0,
    include_quality_metrics = TRUE,
    show_summary = TRUE
  )
  expect_no_error(result)
})

# ═══ PERSON-TIME ANALYSIS ═══
test_that("timeinterval person-time calculation workflow", {
  result <- timeinterval(
    data = timeinterval_trial,
    dx_date = "enrollment_date",
    fu_date = "followup_date",
    output_unit = "months",
    time_basis = "standardized",
    add_times = TRUE,
    include_quality_metrics = TRUE
  )
  expect_no_error(result)
})

# ═══ PROGRESSIVE ANALYSIS ═══
test_that("timeinterval progressive analysis workflow", {
  # Step 1: Basic calculation
  result1 <- timeinterval(
    data = timeinterval_test,
    dx_date = "diagnosis_date",
    fu_date = "followup_date"
  )
  expect_no_error(result1)
  
  # Step 2: Add quality assessment
  result2 <- timeinterval(
    data = timeinterval_test,
    dx_date = "diagnosis_date",
    fu_date = "followup_date",
    include_quality_metrics = TRUE
  )
  expect_no_error(result2)
  
  # Step 3: Full analysis with landmark
  result3 <- timeinterval(
    data = timeinterval_test,
    dx_date = "diagnosis_date",
    fu_date = "followup_date",
    use_landmark = TRUE,
    landmark_time = 6,
    remove_negative = TRUE,
    include_quality_metrics = TRUE,
    add_times = TRUE
  )
  expect_no_error(result3)
})

# ═══ COMPARATIVE ANALYSIS ═══
test_that("timeinterval compare time bases", {
  result_std <- timeinterval(
    data = timeinterval_test,
    dx_date = "diagnosis_date",
    fu_date = "followup_date",
    time_basis = "standardized",
    output_unit = "months"
  )
  
  result_cal <- timeinterval(
    data = timeinterval_test,
    dx_date = "diagnosis_date",
    fu_date = "followup_date",
    time_basis = "calendar",
    output_unit = "months"
  )
  
  expect_no_error(result_std)
  expect_no_error(result_cal)
})

test_that("timeinterval compare output units", {
  units <- c("days", "weeks", "months", "years")
  
  results <- lapply(units, function(unit) {
    timeinterval(
      data = timeinterval_test,
      dx_date = "diagnosis_date",
      fu_date = "followup_date",
      output_unit = unit
    )
  })
  
  expect_true(all(sapply(results, function(r) inherits(r, "timeintervalClass"))))
})
