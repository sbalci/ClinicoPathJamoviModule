# ═══════════════════════════════════════════════════════════
# Basic Functionality Tests: multisurvival
# ═══════════════════════════════════════════════════════════
#
# Tests basic functionality, required arguments, and expected outputs
# for the multisurvival jamovi function

library(testthat)

# Load test data
data(multisurvival_test, package = "ClinicoPath")

test_that("multisurvival function exists and is accessible", {
  expect_true(exists("multisurvival"))
  expect_type(multisurvival, "closure")
})

test_that("multisurvival runs with minimal required arguments", {
  result <- .run_multisurvival(
    data = multisurvival_test,
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    explanatory = "treatment"
  )
  
  expect_s3_class(result, "multisurvivalResults")
  expect_true("multivariableCoxSummary" %in% names(result))
  expect_false(result$todo$visible)
  expect_true(result$text$visible)
})

test_that("multisurvival handles categorical explanatory variables", {
  result <- .run_multisurvival(
    data = multisurvival_test,
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    explanatory = c("treatment", "stage", "grade")
  )
  
  expect_s3_class(result, "multisurvivalResults")
})

test_that("multisurvival handles continuous explanatory variables", {
  result <- .run_multisurvival(
    data = multisurvival_test,
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    contexpl = c("age", "nodes", "biomarker")
  )
  
  expect_s3_class(result, "multisurvivalResults")
})

test_that("multisurvival handles mixed explanatory variables", {
  result <- .run_multisurvival(
    data = multisurvival_test,
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    explanatory = c("treatment", "stage"),
    contexpl = c("age", "nodes")
  )
  
  expect_s3_class(result, "multisurvivalResults")
})

test_that("multisurvival handles date-based time calculation", {
  data(multisurvival_dates, package = "ClinicoPath")
  
  result <- .run_multisurvival(
    data = multisurvival_dates,
    tint = TRUE,
    dxdate = "dxdate",
    fudate = "fudate",
    timetypedata = "ymd",
    timetypeoutput = "months",
    outcome = "outcome"
  )
  
  expect_s3_class(result, "multisurvivalResults")
})

test_that("multisurvival handles different time output units", {
  # Days
  result_days <- .run_multisurvival(
    data = multisurvival_test,
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    timetypeoutput = "days"
  )
  expect_s3_class(result_days, "multisurvivalResults")
  
  # Weeks
  result_weeks <- .run_multisurvival(
    data = multisurvival_test,
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    timetypeoutput = "weeks"
  )
  expect_s3_class(result_weeks, "multisurvivalResults")
  
  # Years
  result_years <- .run_multisurvival(
    data = multisurvival_test,
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    timetypeoutput = "years"
  )
  expect_s3_class(result_years, "multisurvivalResults")
})

test_that("multisurvival generates hazard ratio plot", {
  result <- .run_multisurvival(
    data = multisurvival_test,
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    explanatory = c("treatment", "stage"),
    contexpl = "age",
    hr = TRUE
  )
  
  expect_s3_class(result, "multisurvivalResults")
})

test_that("multisurvival generates Kaplan-Meier plot", {
  result <- .run_multisurvival(
    data = multisurvival_test,
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    explanatory = "treatment",
    km = TRUE
  )
  
  expect_s3_class(result, "multisurvivalResults")
})

test_that("multisurvival tests proportional hazards assumption", {
  result <- .run_multisurvival(
    data = multisurvival_test,
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    explanatory = "treatment",
    contexpl = "age",
    ph_cox = TRUE
  )
  
  expect_s3_class(result, "multisurvivalResults")
})

test_that("multisurvival handles overall survival analysis", {
  result <- .run_multisurvival(
    data = multisurvival_test,
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    analysistype = "overall",
    explanatory = "treatment"
  )
  
  expect_s3_class(result, "multisurvivalResults")
})

test_that("multisurvival handles small dataset", {
  data(multisurvival_small, package = "ClinicoPath")
  
  result <- .run_multisurvival(
    data = multisurvival_small,
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    explanatory = "treatment"
  )
  
  expect_s3_class(result, "multisurvivalResults")
})

test_that("multisurvival handles binary outcome coded as 0/1", {
  result <- .run_multisurvival(
    data = multisurvival_test,
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    outcomeLevel = "1",
    explanatory = "treatment"
  )
  
  expect_s3_class(result, "multisurvivalResults")
})

test_that("multisurvival handles factor outcome variable", {
  data(multisurvival_dates, package = "ClinicoPath")
  
  result <- .run_multisurvival(
    data = multisurvival_dates,
    tint = TRUE,
    dxdate = "dxdate",
    fudate = "fudate",
    outcome = "outcome",
    outcomeLevel = "Dead",
    explanatory = "treatment"
  )
  
  expect_s3_class(result, "multisurvivalResults")
  expect_false(result$todo$visible)
})
