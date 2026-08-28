# ═══════════════════════════════════════════════════════════
# Basic Functionality Tests: swimmerplot
# ═══════════════════════════════════════════════════════════
#
# Tests basic functionality, required arguments, and expected outputs
# for the swimmerplot jamovi function

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
  expect_s3_class(result, "swimmerplotResults")

  # swimmerplot() returns the results object itself, not a wrapper with a
  # $results slot; assert on a real element declared in swimmerplot.r.yaml.
  expect_true(!is.null(result$summary))
})

test_that("swimmerplot runs with response variable", {
  result <- swimmerplot(
    data = swimmerplot_test,
    patientID = "PatientID",
    startTime = "StartTime",
    endTime = "EndTime",
    responseVar = "Response"
  )

  expect_s3_class(result, "swimmerplotResults")
})

test_that("swimmerplot handles single milestone", {
  result <- swimmerplot(
    data = swimmerplot_test,
    patientID = "PatientID",
    startTime = "StartTime",
    endTime = "EndTime",
    milestone1Date = "FirstAssessment",
    milestone1Name = "First Assessment"
  )

  expect_s3_class(result, "swimmerplotResults")
})

test_that("swimmerplot handles multiple milestones", {
  result <- swimmerplot(
    data = swimmerplot_test,
    patientID = "PatientID",
    startTime = "StartTime",
    endTime = "EndTime",
    milestone1Date = "TreatmentStart",
    milestone1Name = "Treatment Start",
    milestone2Date = "FirstAssessment",
    milestone2Name = "First Assessment",
    milestone3Date = "BestResponse",
    milestone3Name = "Best Response"
  )

  expect_s3_class(result, "swimmerplotResults")
})

test_that("swimmerplot handles all five milestones", {
  data(swimmerplot_milestones, package = "ClinicoPath")

  result <- swimmerplot(
    data = swimmerplot_milestones,
    patientID = "PatientID",
    startTime = "StartTime",
    endTime = "EndTime",
    milestone1Date = "Diagnosis",
    milestone1Name = "Diagnosis",
    milestone2Date = "Surgery",
    milestone2Name = "Surgery",
    milestone3Date = "ChemoStart",
    milestone3Name = "Chemo Start",
    milestone4Date = "Recurrence",
    milestone4Name = "Recurrence",
    milestone5Date = "Death",
    milestone5Name = "Death"
  )

  expect_s3_class(result, "swimmerplotResults")
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

  expect_s3_class(result, "swimmerplotResults")
})

test_that("swimmerplot handles censoring variable", {
  result <- swimmerplot(
    data = swimmerplot_test,
    patientID = "PatientID",
    startTime = "StartTime",
    endTime = "EndTime",
    censorVar = "Censored"
  )

  expect_s3_class(result, "swimmerplotResults")
})

test_that("swimmerplot handles grouping variable", {
  result <- swimmerplot(
    data = swimmerplot_test,
    patientID = "PatientID",
    startTime = "StartTime",
    endTime = "EndTime",
    groupVar = "TreatmentArm"
  )

  expect_s3_class(result, "swimmerplotResults")
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

    expect_s3_class(result, "swimmerplotResults")
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

    expect_s3_class(result, "swimmerplotResults")
  }
})

test_that("swimmerplot handles different date formats", {
  data(swimmerplot_dates, package = "ClinicoPath")

  date_formats <- c("ymd", "mdy", "dmy")

  for (fmt in date_formats) {
    result <- swimmerplot(
      data = swimmerplot_dates,
      patientID = "PatientID",
      startTime = "StartTime",
      endTime = "EndTime",
      timeType = "datetime",
      dateFormat = fmt
    )

    expect_s3_class(result, "swimmerplotResults")
  }
})

test_that("swimmerplot handles different sorting options", {
  sort_options <- c("duration_desc", "duration_asc", "patient_id", "response")

  for (sort_opt in sort_options) {
    result <- swimmerplot(
      data = swimmerplot_test,
      patientID = "PatientID",
      startTime = "StartTime",
      endTime = "EndTime",
      responseVar = "Response",
      sortOrder = sort_opt
    )

    expect_s3_class(result, "swimmerplotResults")
  }
})

test_that("swimmerplot handles color palettes", {
  palettes <- c("default", "viridis", "contrast", "monochrome")

  for (palette in palettes) {
    result <- swimmerplot(
      data = swimmerplot_test,
      patientID = "PatientID",
      startTime = "StartTime",
      endTime = "EndTime",
      responseVar = "Response",
      colorPalette = palette
    )

    expect_s3_class(result, "swimmerplotResults")
  }
})

test_that("swimmerplot handles reference lines", {
  result <- swimmerplot(
    data = swimmerplot_test,
    patientID = "PatientID",
    startTime = "StartTime",
    endTime = "EndTime",
    referenceLines = "median"
  )

  expect_s3_class(result, "swimmerplotResults")
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

  expect_s3_class(result, "swimmerplotResults")
})

test_that("swimmerplot handles comprehensive visualization", {
  result <- swimmerplot(
    data = swimmerplot_test,
    patientID = "PatientID",
    startTime = "StartTime",
    endTime = "EndTime",
    responseVar = "Response",
    milestone1Date = "TreatmentStart",
    milestone1Name = "Treatment",
    milestone2Date = "FirstAssessment",
    milestone2Name = "Assessment",
    milestone3Date = "Progression",
    milestone3Name = "Progression",
    eventVar = "AdverseEvent",
    eventTimeVar = "EventTime",
    censorVar = "Censored",
    groupVar = "TreatmentArm",
    sortOrder = "duration_desc",
    referenceLines = "median"
  )

  expect_s3_class(result, "swimmerplotResults")
})
