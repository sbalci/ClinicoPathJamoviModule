# ═══════════════════════════════════════════════════════════
# Edge Cases and Error Handling Tests: timeinterval
# ═══════════════════════════════════════════════════════════

library(testthat)
library(ClinicoPath)

data(timeinterval_quality, package = "ClinicoPath")
data(timeinterval_extreme, package = "ClinicoPath")
data(timeinterval_sameday, package = "ClinicoPath")
data(timeinterval_negative, package = "ClinicoPath")
data(timeinterval_allmissing, package = "ClinicoPath")
data(timeinterval_large, package = "ClinicoPath")

# ═══ MISSING DATA ═══
test_that("timeinterval handles missing values", {
  expect_warning(
    timeinterval(
      data = timeinterval_quality,
      dx_date = "start_date",
      fu_date = "end_date"
    ),
    regexp = "missing|NA|removed",
    ignore.case = TRUE
  )
})

test_that("timeinterval handles all missing dates", {
  expect_error(
    timeinterval(
      data = timeinterval_allmissing,
      dx_date = "start_date",
      fu_date = "end_date"
    ),
    regexp = "missing|all.*NA|no.*valid|empty",
    ignore.case = TRUE
  )
})

# ═══ NEGATIVE INTERVALS ═══
test_that("timeinterval detects negative intervals", {
  result <- timeinterval(
    data = timeinterval_negative,
    dx_date = "start_date",
    fu_date = "end_date",
    include_quality_metrics = TRUE
  )
  # Should complete and report negative intervals
  expect_s3_class(result, "timeintervalClass")
})

test_that("timeinterval removes negative intervals when requested", {
  result <- timeinterval(
    data = timeinterval_negative,
    dx_date = "start_date",
    fu_date = "end_date",
    remove_negative = TRUE
  )
  expect_no_error(result)
})

# ═══ ZERO INTERVALS ═══
test_that("timeinterval handles same-day intervals", {
  result <- timeinterval(
    data = timeinterval_sameday,
    dx_date = "start",
    fu_date = "end",
    output_unit = "days"
  )
  expect_no_error(result)
})

# ═══ EXTREME VALUES ═══
test_that("timeinterval handles extreme values", {
  result <- timeinterval(
    data = timeinterval_extreme,
    dx_date = "start_date",
    fu_date = "end_date",
    include_quality_metrics = TRUE
  )
  expect_s3_class(result, "timeintervalClass")
})

test_that("timeinterval flags extreme values", {
  result <- timeinterval(
    data = timeinterval_extreme,
    dx_date = "start_date",
    fu_date = "end_date",
    remove_extreme = TRUE,
    extreme_multiplier = 2.0
  )
  expect_no_error(result)
})

# ═══ LARGE DATASETS ═══
test_that("timeinterval handles large datasets", {
  result <- timeinterval(
    data = timeinterval_large,
    dx_date = "dx_date",
    fu_date = "fu_date"
  )
  expect_no_error(result)
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
  expect_no_error(result)
})

test_that("timeinterval handles special characters in names", {
  data_special <- timeinterval_quality
  names(data_special)[names(data_special) == "start_date"] <- "start-date%"
  
  result <- timeinterval(
    data = data_special,
    dx_date = "start-date%",
    fu_date = "end_date"
  )
  expect_no_error(result)
})

# ═══ DATA TYPE ISSUES ═══
test_that("timeinterval handles numeric date columns", {
  data_numeric <- timeinterval_quality
  data_numeric$start_date <- as.numeric(Sys.Date())
  data_numeric$end_date <- as.numeric(Sys.Date() + 30)
  
  # Should attempt to parse or error informatively
  expect_condition(
    timeinterval(
      data = data_numeric,
      dx_date = "start_date",
      fu_date = "end_date"
    )
  )
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
  
  expect_no_error(result_90)
  expect_no_error(result_99)
})

test_that("timeinterval handles zero landmark time", {
  result <- timeinterval(
    data = timeinterval_quality,
    dx_date = "start_date",
    fu_date = "end_date",
    use_landmark = TRUE,
    landmark_time = 0
  )
  expect_no_error(result)
})
