# ═══════════════════════════════════════════════════════════
# Argument Combination Tests: swimmerplot
# ═══════════════════════════════════════════════════════════
#
# Tests all argument combinations and option interactions
# for the swimmerplot jamovi function

library(testthat)
library(ClinicoPath)

# Load test data
data(swimmerplot_test, package = "ClinicoPath")
data(swimmerplot_immuno, package = "ClinicoPath")
data(swimmerplot_surgery, package = "ClinicoPath")
data(swimmerplot_grouped, package = "ClinicoPath")

test_that("swimmerplot handles immunotherapy timeline data", {
  result <- swimmerplot(
    data = swimmerplot_immuno,
    patientID = "PatientID",
    startTime = "StartTime",
    endTime = "EndTime",
    responseVar = "Response",
    milestone1 = "ImmunotherapyStart",
    milestone1_name = "IO Start",
    milestone2 = "FirstResponse",
    milestone2_name = "First Response",
    milestone3 = "ConfirmedResponse",
    milestone3_name = "Confirmed Response",
    eventVar = "irAE",
    eventTimeVar = "irAE_Time",
    groupVar = "PDL1_Status",
    censorVar = "Censored"
  )

  expect_s3_class(result, "swimmerplotClass")
})

test_that("swimmerplot handles surgical outcomes timeline", {
  result <- swimmerplot(
    data = swimmerplot_surgery,
    patientID = "PatientID",
    startTime = "StartTime",
    endTime = "EndTime",
    responseVar = "Outcome",
    milestone1 = "SurgeryDate",
    milestone1_name = "Surgery",
    milestone2 = "Discharge",
    milestone2_name = "Discharge",
    milestone3 = "FirstVisit",
    milestone3_name = "First Visit",
    milestone4 = "ComplicationDate",
    milestone4_name = "Complication",
    eventVar = "ComplicationType",
    eventTimeVar = "ComplicationDate",
    groupVar = "SurgeryType"
  )

  expect_s3_class(result, "swimmerplotClass")
})

test_that("swimmerplot handles multiple event markers", {
  data(swimmerplot_events, package = "ClinicoPath")

  result <- swimmerplot(
    data = swimmerplot_events,
    patientID = "PatientID",
    startTime = "StartTime",
    endTime = "EndTime",
    responseVar = "Response",
    eventVar = "Event1_Type",
    eventTimeVar = "Event1_Time",
    groupVar = "TreatmentLine"
  )

  expect_s3_class(result, "swimmerplotClass")
})

test_that("swimmerplot handles date/time format data", {
  data(swimmerplot_dates, package = "ClinicoPath")

  result <- swimmerplot(
    data = swimmerplot_dates,
    patientID = "PatientID",
    startTime = "StartTime",
    endTime = "EndTime",
    responseVar = "Response",
    milestone1 = "Milestone1_Days",
    milestone1_name = "Treatment Start",
    milestone2 = "Milestone2_Days",
    milestone2_name = "First Response",
    milestone3 = "Milestone3_Days",
    milestone3_name = "Progression",
    timeType = "raw",
    timeUnit = "days",
    groupVar = "Cohort",
    censorVar = "Censored"
  )

  expect_s3_class(result, "swimmerplotClass")
})

test_that("swimmerplot handles grouped comparison data", {
  result <- swimmerplot(
    data = swimmerplot_grouped,
    patientID = "PatientID",
    startTime = "StartTime",
    endTime = "EndTime",
    responseVar = "Response",
    milestone1 = "TreatmentStart",
    milestone1_name = "Treatment",
    milestone2 = "FirstAssessment",
    milestone2_name = "Assessment",
    milestone3 = "BestResponse",
    milestone3_name = "Best Response",
    milestone4 = "Progression",
    milestone4_name = "Progression",
    eventVar = "AdverseEvent",
    eventTimeVar = "EventTime",
    groupVar = "Group",
    censorVar = "Censored",
    sortBy = "duration"
  )

  expect_s3_class(result, "swimmerplotClass")
})

test_that("swimmerplot handles all display options combined", {
  result <- swimmerplot(
    data = swimmerplot_test,
    patientID = "PatientID",
    startTime = "StartTime",
    endTime = "EndTime",
    responseVar = "Response",
    milestone1 = "TreatmentStart",
    milestone1_name = "Treatment Start",
    milestone2 = "FirstAssessment",
    milestone2_name = "First Assessment",
    milestone3 = "BestResponse",
    milestone3_name = "Best Response",
    eventVar = "AdverseEvent",
    eventTimeVar = "EventTime",
    censorVar = "Censored",
    groupVar = "TreatmentArm",
    timeType = "raw",
    timeUnit = "days",
    sortBy = "duration",
    colorPalette = "Set2",
    showReferenceLine = TRUE,
    referenceLineValue = 180,
    showLegend = TRUE,
    plotTitle = "Clinical Trial Timeline",
    xAxisTitle = "Time (Days)",
    yAxisTitle = "Patients"
  )

  expect_s3_class(result, "swimmerplotClass")
})

test_that("swimmerplot handles response-based sorting", {
  result <- swimmerplot(
    data = swimmerplot_test,
    patientID = "PatientID",
    startTime = "StartTime",
    endTime = "EndTime",
    responseVar = "Response",
    sortBy = "response"
  )

  expect_s3_class(result, "swimmerplotClass")
})

test_that("swimmerplot handles duration-based sorting", {
  result <- swimmerplot(
    data = swimmerplot_test,
    patientID = "PatientID",
    startTime = "StartTime",
    endTime = "EndTime",
    sortBy = "duration"
  )

  expect_s3_class(result, "swimmerplotClass")
})

test_that("swimmerplot handles no sorting", {
  result <- swimmerplot(
    data = swimmerplot_test,
    patientID = "PatientID",
    startTime = "StartTime",
    endTime = "EndTime",
    sortBy = "none"
  )

  expect_s3_class(result, "swimmerplotClass")
})

test_that("swimmerplot handles all color palettes", {
  palettes <- c("default", "Set1", "Set2", "Set3", "Paired",
                "Dark2", "Accent", "Pastel1", "Pastel2",
                "viridis", "plasma", "inferno", "magma")

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

test_that("swimmerplot handles weeks time unit", {
  result <- swimmerplot(
    data = swimmerplot_test,
    patientID = "PatientID",
    startTime = "StartTime",
    endTime = "EndTime",
    timeUnit = "weeks"
  )

  expect_s3_class(result, "swimmerplotClass")
})

test_that("swimmerplot handles months time unit", {
  result <- swimmerplot(
    data = swimmerplot_test,
    patientID = "PatientID",
    startTime = "StartTime",
    endTime = "EndTime",
    timeUnit = "months"
  )

  expect_s3_class(result, "swimmerplotClass")
})

test_that("swimmerplot handles custom axis titles", {
  result <- swimmerplot(
    data = swimmerplot_test,
    patientID = "PatientID",
    startTime = "StartTime",
    endTime = "EndTime",
    plotTitle = "Patient Follow-up Timeline",
    xAxisTitle = "Days from Enrollment",
    yAxisTitle = "Patient ID"
  )

  expect_s3_class(result, "swimmerplotClass")
})

test_that("swimmerplot handles multiple reference lines (via single value)", {
  result <- swimmerplot(
    data = swimmerplot_test,
    patientID = "PatientID",
    startTime = "StartTime",
    endTime = "EndTime",
    showReferenceLine = TRUE,
    referenceLineValue = 90
  )

  expect_s3_class(result, "swimmerplotClass")
})

test_that("swimmerplot handles legend display options", {
  # With legend
  result_with <- swimmerplot(
    data = swimmerplot_test,
    patientID = "PatientID",
    startTime = "StartTime",
    endTime = "EndTime",
    responseVar = "Response",
    showLegend = TRUE
  )

  expect_s3_class(result_with, "swimmerplotClass")

  # Without legend
  result_without <- swimmerplot(
    data = swimmerplot_test,
    patientID = "PatientID",
    startTime = "StartTime",
    endTime = "EndTime",
    responseVar = "Response",
    showLegend = FALSE
  )

  expect_s3_class(result_without, "swimmerplotClass")
})

test_that("swimmerplot handles response analysis options", {
  result <- swimmerplot(
    data = swimmerplot_test,
    patientID = "PatientID",
    startTime = "StartTime",
    endTime = "EndTime",
    responseVar = "Response",
    showResponseSummary = TRUE
  )

  expect_s3_class(result, "swimmerplotClass")
})

test_that("swimmerplot handles milestone name customization", {
  data(swimmerplot_milestones, package = "ClinicoPath")

  result <- swimmerplot(
    data = swimmerplot_milestones,
    patientID = "PatientID",
    startTime = "StartTime",
    endTime = "EndTime",
    milestone1 = "Diagnosis",
    milestone1_name = "Dx",
    milestone2 = "Surgery",
    milestone2_name = "Sx",
    milestone3 = "ChemoStart",
    milestone3_name = "Chemo",
    milestone4 = "Recurrence",
    milestone4_name = "Recur",
    milestone5 = "Death",
    milestone5_name = "Death"
  )

  expect_s3_class(result, "swimmerplotClass")
})
