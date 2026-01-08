# ═══════════════════════════════════════════════════════════
# Basic Functionality Tests: multisurvival
# ═══════════════════════════════════════════════════════════
#
# Tests basic functionality, required arguments, and expected outputs
# for the multisurvival jamovi function

library(testthat)
library(ClinicoPath)

# Load test data
data(multisurvival_test, package = "ClinicoPath")

test_that("multisurvival function exists and is accessible", {
  expect_true(exists("multisurvival"))
  expect_type(multisurvival, "closure")
})

test_that("multisurvival runs with minimal required arguments", {
  result <- multisurvival(
    data = multisurvival_test,
    elapsedtime = "elapsedtime",
    outcome = "outcome"
  )
  
  expect_s3_class(result, "multisurvivalClass")
  expect_true("results" %in% names(result))
})

test_that("multisurvival handles categorical explanatory variables", {
  result <- multisurvival(
    data = multisurvival_test,
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    explanatory = c("treatment", "stage", "grade")
  )
  
  expect_s3_class(result, "multisurvivalClass")
})

test_that("multisurvival handles continuous explanatory variables", {
  result <- multisurvival(
    data = multisurvival_test,
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    contexpl = c("age", "nodes", "biomarker")
  )
  
  expect_s3_class(result, "multisurvivalClass")
})

test_that("multisurvival handles mixed explanatory variables", {
  result <- multisurvival(
    data = multisurvival_test,
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    explanatory = c("treatment", "stage"),
    contexpl = c("age", "nodes")
  )
  
  expect_s3_class(result, "multisurvivalClass")
})

test_that("multisurvival handles date-based time calculation", {
  data(multisurvival_dates, package = "ClinicoPath")
  
  result <- multisurvival(
    data = multisurvival_dates,
    tint = TRUE,
    dxdate = "dxdate",
    fudate = "fudate",
    timetypedata = "ymd",
    timetypeoutput = "months",
    outcome = "outcome"
  )
  
  expect_s3_class(result, "multisurvivalClass")
})

test_that("multisurvival handles different time output units", {
  # Days
  result_days <- multisurvival(
    data = multisurvival_test,
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    timetypeoutput = "days"
  )
  expect_s3_class(result_days, "multisurvivalClass")
  
  # Weeks
  result_weeks <- multisurvival(
    data = multisurvival_test,
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    timetypeoutput = "weeks"
  )
  expect_s3_class(result_weeks, "multisurvivalClass")
  
  # Years
  result_years <- multisurvival(
    data = multisurvival_test,
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    timetypeoutput = "years"
  )
  expect_s3_class(result_years, "multisurvivalClass")
})

test_that("multisurvival generates hazard ratio plot", {
  result <- multisurvival(
    data = multisurvival_test,
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    explanatory = c("treatment", "stage"),
    contexpl = "age",
    hr = TRUE
  )
  
  expect_s3_class(result, "multisurvivalClass")
})

test_that("multisurvival generates Kaplan-Meier plot", {
  result <- multisurvival(
    data = multisurvival_test,
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    explanatory = "treatment",
    km = TRUE
  )
  
  expect_s3_class(result, "multisurvivalClass")
})

test_that("multisurvival tests proportional hazards assumption", {
  result <- multisurvival(
    data = multisurvival_test,
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    explanatory = "treatment",
    contexpl = "age",
    ph_cox = TRUE
  )
  
  expect_s3_class(result, "multisurvivalClass")
})

test_that("multisurvival handles overall survival analysis", {
  result <- multisurvival(
    data = multisurvival_test,
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    analysistype = "overall",
    explanatory = "treatment"
  )
  
  expect_s3_class(result, "multisurvivalClass")
})

test_that("multisurvival handles small dataset", {
  data(multisurvival_small, package = "ClinicoPath")
  
  result <- multisurvival(
    data = multisurvival_small,
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    explanatory = "treatment"
  )
  
  expect_s3_class(result, "multisurvivalClass")
})

test_that("multisurvival handles binary outcome coded as 0/1", {
  result <- multisurvival(
    data = multisurvival_test,
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    outcomeLevel = "1",
    explanatory = "treatment"
  )
  
  expect_s3_class(result, "multisurvivalClass")
})

test_that("multisurvival handles factor outcome variable", {
  data(multisurvival_dates, package = "ClinicoPath")
  
  result <- multisurvival(
    data = multisurvival_dates,
    elapsedtime = "dxdate",  # Will use as elapsed if tint=FALSE  
    outcome = "outcome",
    outcomeLevel = "Dead",
    explanatory = "treatment"
  )
  
  # This may error as expected, so we just check it doesn't crash unexpectedly
  expect_s3_class(result, "multisurvivalClass")
})
