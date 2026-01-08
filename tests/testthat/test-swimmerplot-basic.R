# ═══════════════════════════════════════════════════════════
# Basic Functionality Tests: swimmerplot
# ═══════════════════════════════════════════════════════════
#
# Tests basic functionality, required arguments, and expected outputs
# for the swimmerplot jamovi function

library(testthat)
library(ClinicoPath)

# Load test data
data(swimmerplot_test, package = "ClinicoPath")

test_that("swimmerplot function exists and is accessible", {
  # Check function exists
  expect_true(exists("swimmerplot"))

  # Check it's a function
  expect_type(swimmerplot, "closure")
})

test_that("swimmerplot runs with minimal required arguments", {
  # Only patientID, startTime, and endTime are required
  result <- swimmerplot(
    data = swimmerplot_test,
    patientID = "PatientID",
    startTime = "StartTime",
    endTime = "EndTime"
  )

  # Should return a result object
  expect_s3_class(result, "swimmerplotClass")

  # Should have a results component
  expect_true("results" %in% names(result))
})

test_that("swimmerplot runs with response variable", {
  result <- swimmerplot(
    data = swimmerplot_test,
    patientID = "PatientID",
    startTime = "StartTime",
    endTime = "EndTime",
    responseVar = "Response"
  )

  expect_s3_class(result, "swimmerplotClass")
})

test_that("swimmerplot handles single milestone", {
  result <- swimmerplot(
    data = swimmerplot_test,
    patientID = "PatientID",
    startTime = "StartTime",
    endTime = "EndTime",
    milestone1 = "FirstAssessment",
    milestone1_name = "First Assessment"
  )

  expect_s3_class(result, "swimmerplotClass")
})

test_that("swimmerplot handles multiple milestones", {
  result <- swimmerplot(
    data = swimmerplot_test,
    patientID = "PatientID",
    startTime = "StartTime",
    endTime = "EndTime",
    milestone1 = "TreatmentStart",
    milestone1_name = "Treatment Start",
    milestone2 = "FirstAssessment",
    milestone2_name = "First Assessment",
    milestone3 = "BestResponse",
    milestone3_name = "Best Response"
  )

  expect_s3_class(result, "swimmerplotClass")
})

test_that("swimmerplot handles all five milestones", {
  data(swimmerplot_milestones, package = "ClinicoPath")

  result <- swimmerplot(
    data = swimmerplot_milestones,
    patientID = "PatientID",
    startTime = "StartTime",
    endTime = "EndTime",
    milestone1 = "Diagnosis",
    milestone1_name = "Diagnosis",
    milestone2 = "Surgery",
    milestone2_name = "Surgery",
    milestone3 = "ChemoStart",
    milestone3_name = "Chemo Start",
    milestone4 = "Recurrence",
    milestone4_name = "Recurrence",
    milestone5 = "Death",
    milestone5_name = "Death"
  )

  expect_s3_class(result, "swimmerplotClass")
})

test_that("swimmerplot handles event markers", {
  result <- swimmerplot(
    data = swimmerplot_test,
    patientID = "PatientID",
    startTime = "StartTime",
    endTime = "EndTime",
    eventVar = "AdverseEvent",
    eventTimeVar = "EventTime"
  )

  expect_s3_class(result, "swimmerplotClass")
})

test_that("swimmerplot handles censoring variable", {
  result <- swimmerplot(
    data = swimmerplot_test,
    patientID = "PatientID",
    startTime = "StartTime",
    endTime = "EndTime",
    censorVar = "Censored"
  )

  expect_s3_class(result, "swimmerplotClass")
})

test_that("swimmerplot handles grouping variable", {
  result <- swimmerplot(
    data = swimmerplot_test,
    patientID = "PatientID",
    startTime = "StartTime",
    endTime = "EndTime",
    groupVar = "TreatmentArm"
  )

  expect_s3_class(result, "swimmerplotClass")
})

test_that("swimmerplot handles different time types", {
  time_types <- c("raw", "datetime")

  for (time_type in time_types) {
    result <- swimmerplot(
      data = swimmerplot_test,
      patientID = "PatientID",
      startTime = "StartTime",
      endTime = "EndTime",
      timeType = time_type
    )

    expect_s3_class(result, "swimmerplotClass")
  }
})

test_that("swimmerplot handles different time units", {
  time_units <- c("days", "weeks", "months")

  for (unit in time_units) {
    result <- swimmerplot(
      data = swimmerplot_test,
      patientID = "PatientID",
      startTime = "StartTime",
      endTime = "EndTime",
      timeUnit = unit
    )

    expect_s3_class(result, "swimmerplotClass")
  }
})

test_that("swimmerplot handles different date formats", {
  data(swimmerplot_dates, package = "ClinicoPath")

  date_formats <- c("YYYY-MM-DD", "MM/DD/YYYY", "DD-MM-YYYY")

  for (fmt in date_formats) {
    result <- swimmerplot(
      data = swimmerplot_dates,
      patientID = "PatientID",
      startTime = "StartTime",
      endTime = "EndTime",
      timeType = "datetime",
      dateFormat = fmt
    )

    expect_s3_class(result, "swimmerplotClass")
  }
})

test_that("swimmerplot handles different sorting options", {
  sort_options <- c("none", "duration", "response")

  for (sort_opt in sort_options) {
    result <- swimmerplot(
      data = swimmerplot_test,
      patientID = "PatientID",
      startTime = "StartTime",
      endTime = "EndTime",
      responseVar = "Response",
      sortBy = sort_opt
    )

    expect_s3_class(result, "swimmerplotClass")
  }
})

test_that("swimmerplot handles color palettes", {
  palettes <- c("default", "Set1", "Set2", "Dark2", "viridis")

  for (palette in palettes) {
    result <- swimmerplot(
      data = swimmerplot_test,
      patientID = "PatientID",
      startTime = "StartTime",
      endTime = "EndTime",
      responseVar = "Response",
      colorPalette = palette
    )

    expect_s3_class(result, "swimmerplotClass")
  }
})

test_that("swimmerplot handles reference lines", {
  result <- swimmerplot(
    data = swimmerplot_test,
    patientID = "PatientID",
    startTime = "StartTime",
    endTime = "EndTime",
    showReferenceLine = TRUE,
    referenceLineValue = 180
  )

  expect_s3_class(result, "swimmerplotClass")
})

test_that("swimmerplot runs with small dataset", {
  data(swimmerplot_small, package = "ClinicoPath")

  result <- swimmerplot(
    data = swimmerplot_small,
    patientID = "PatientID",
    startTime = "StartTime",
    endTime = "EndTime",
    responseVar = "Response"
  )

  expect_s3_class(result, "swimmerplotClass")
})

test_that("swimmerplot handles comprehensive visualization", {
  result <- swimmerplot(
    data = swimmerplot_test,
    patientID = "PatientID",
    startTime = "StartTime",
    endTime = "EndTime",
    responseVar = "Response",
    milestone1 = "TreatmentStart",
    milestone1_name = "Treatment",
    milestone2 = "FirstAssessment",
    milestone2_name = "Assessment",
    milestone3 = "Progression",
    milestone3_name = "Progression",
    eventVar = "AdverseEvent",
    eventTimeVar = "EventTime",
    censorVar = "Censored",
    groupVar = "TreatmentArm",
    sortBy = "duration",
    showReferenceLine = TRUE,
    referenceLineValue = 180
  )

  expect_s3_class(result, "swimmerplotClass")
})
