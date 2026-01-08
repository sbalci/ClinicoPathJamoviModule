# ═══════════════════════════════════════════════════════════
# Argument Combination Tests: timeinterval
# ═══════════════════════════════════════════════════════════

library(testthat)
library(ClinicoPath)

data(timeinterval_test, package = "ClinicoPath")
data(timeinterval_landmark, package = "ClinicoPath")
data(timeinterval_trial, package = "ClinicoPath")

# ═══ LANDMARK ANALYSIS ═══
test_that("timeinterval landmark analysis basic", {
  result <- timeinterval(
    data = timeinterval_landmark,
    dx_date = "enrollment_date",
    fu_date = "last_contact",
    use_landmark = TRUE,
    landmark_time = 6,
    output_unit = "months"
  )
  expect_no_error(result)
})

test_that("timeinterval landmark with different time points", {
  landmarks <- c(3, 6, 12, 24)
  for (lm in landmarks) {
    result <- timeinterval(
      data = timeinterval_landmark,
      dx_date = "enrollment_date",
      fu_date = "last_contact",
      use_landmark = TRUE,
      landmark_time = lm,
      output_unit = "months"
    )
    expect_no_error(result)
  }
})

# ═══ DATA QUALITY OPTIONS ═══
test_that("timeinterval remove negative intervals", {
  result <- timeinterval(
    data = timeinterval_test,
    dx_date = "diagnosis_date",
    fu_date = "followup_date",
    remove_negative = TRUE
  )
  expect_no_error(result)
})

test_that("timeinterval flag extreme values", {
  result <- timeinterval(
    data = timeinterval_test,
    dx_date = "diagnosis_date",
    fu_date = "followup_date",
    remove_extreme = TRUE,
    extreme_multiplier = 2.0
  )
  expect_no_error(result)
})

test_that("timeinterval different extreme multipliers", {
  multipliers <- c(1.5, 2.0, 3.0, 5.0)
  for (mult in multipliers) {
    result <- timeinterval(
      data = timeinterval_test,
      dx_date = "diagnosis_date",
      fu_date = "followup_date",
      remove_extreme = TRUE,
      extreme_multiplier = mult
    )
    expect_no_error(result)
  }
})

# ═══ ALL OPTIONS COMBINED ═══
test_that("timeinterval maximal option combination", {
  result <- timeinterval(
    data = timeinterval_trial,
    dx_date = "enrollment_date",
    fu_date = "followup_date",
    time_format = "ymd",
    output_unit = "months",
    time_basis = "standardized",
    use_landmark = TRUE,
    landmark_time = 6,
    remove_negative = TRUE,
    remove_extreme = TRUE,
    extreme_multiplier = 2.0,
    add_times = TRUE,
    include_quality_metrics = TRUE,
    confidence_level = 95,
    show_summary = TRUE,
    show_glossary = TRUE,
    timezone = "system"
  )
  expect_no_error(result)
})

# ═══ FORMAT COMBINATIONS ═══
test_that("timeinterval format with different units", {
  formats <- c("ymd", "dmy", "mdy")
  units <- c("days", "weeks", "months", "years")
  
  for (fmt in formats) {
    for (unit in units) {
      result <- timeinterval(
        data = timeinterval_test,
        dx_date = "diagnosis_date",
        fu_date = "followup_date",
        time_format = fmt,
        output_unit = unit
      )
      expect_no_error(result)
    }
  }
})

# ═══ TIME BASIS COMBINATIONS ═══
test_that("timeinterval time basis with different units", {
  bases <- c("standardized", "calendar")
  units <- c("months", "years")
  
  for (basis in bases) {
    for (unit in units) {
      result <- timeinterval(
        data = timeinterval_test,
        dx_date = "diagnosis_date",
        fu_date = "followup_date",
        time_basis = basis,
        output_unit = unit
      )
      expect_no_error(result)
    }
  }
})
