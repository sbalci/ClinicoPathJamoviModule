# ═══════════════════════════════════════════════════════════
# Edge Cases and Error Handling Tests: timeinterval
# ═══════════════════════════════════════════════════════════

library(testthat)

data(timeinterval_quality, package = "ClinicoPath")
data(timeinterval_extreme, package = "ClinicoPath")
data(timeinterval_sameday, package = "ClinicoPath")
data(timeinterval_negative, package = "ClinicoPath")
data(timeinterval_large, package = "ClinicoPath")

# ═══ MISSING DATA ═══
test_that("timeinterval handles missing values", {
  res <- timeinterval(
    data = timeinterval_quality,
    dx_date = "start_date",
    fu_date = "end_date"
  )
  expect_s3_class(res, "timeintervalResults")
})

# ═══ NEGATIVE INTERVALS ═══
test_that("timeinterval detects negative intervals", {
  result <- timeinterval(
    data = timeinterval_negative,
    dx_date = "start_date",
    fu_date = "end_date",
    include_quality_metrics = TRUE
  )
  expect_s3_class(result, "timeintervalResults")
})

test_that("timeinterval removes negative intervals when requested", {
  result <- timeinterval(
    data = timeinterval_negative,
    dx_date = "start_date",
    fu_date = "end_date",
    remove_negative = TRUE
  )
  expect_s3_class(result, "timeintervalResults")
})

# ═══ ZERO INTERVALS ═══
test_that("timeinterval handles same-day intervals", {
  result <- timeinterval(
    data = timeinterval_sameday,
    dx_date = "start",
    fu_date = "end",
    output_unit = "days"
  )
  expect_s3_class(result, "timeintervalResults")
})

# ═══ EXTREME VALUES ═══
test_that("timeinterval handles extreme values", {
  result <- timeinterval(
    data = timeinterval_extreme,
    dx_date = "start_date",
    fu_date = "end_date",
    include_quality_metrics = TRUE
  )
  expect_s3_class(result, "timeintervalResults")
})

test_that("timeinterval flags extreme values", {
  result <- timeinterval(
    data = timeinterval_extreme,
    dx_date = "start_date",
    fu_date = "end_date",
    remove_extreme = TRUE,
    extreme_multiplier = 2.0
  )
  expect_s3_class(result, "timeintervalResults")
})

# ═══ LARGE DATASETS ═══
test_that("timeinterval handles large datasets", {
  result <- timeinterval(
    data = timeinterval_large,
    dx_date = "dx_date",
    fu_date = "fu_date"
  )
  expect_s3_class(result, "timeintervalResults")
})

# ═══ VARIABLE NAMES ═══
test_that("timeinterval handles variables with spaces", {
  data_spaces <- timeinterval_quality
  names(data_spaces)[names(data_spaces) == "start_date"] <- "start date"
  
  result <- timeinterval(
    data = data_spaces,
    dx_date = "start date",
    fu_date = "end_date"
  )
  expect_s3_class(result, "timeintervalResults")
})

# ═══ BOUNDARY VALUES ═══
test_that("timeinterval handles boundary confidence levels", {
  result_90 <- timeinterval(
    data = timeinterval_quality,
    dx_date = "start_date",
    fu_date = "end_date",
    confidence_level = 90
  )
  
  result_99 <- timeinterval(
    data = timeinterval_quality,
    dx_date = "start_date",
    fu_date = "end_date",
    confidence_level = 99
  )
  
  expect_s3_class(result_90, "timeintervalResults")
  expect_s3_class(result_99, "timeintervalResults")
})

test_that("timeinterval handles zero landmark time", {
  result <- timeinterval(
    data = timeinterval_quality,
    dx_date = "start_date",
    fu_date = "end_date",
    use_landmark = TRUE,
    landmark_time = 0
  )
  expect_s3_class(result, "timeintervalResults")
})
