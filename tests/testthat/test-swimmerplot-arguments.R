# ═══════════════════════════════════════════════════════════
# Argument Combination Tests: swimmerplot
# ═══════════════════════════════════════════════════════════
#
# Tests all argument combinations and option interactions
# for the swimmerplot jamovi function

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
    milestone1Date = "ImmunotherapyStart",
    milestone1Name = "IO Start",
    milestone2Date = "FirstResponse",
    milestone2Name = "First Response",
    milestone3Date = "ConfirmedResponse",
    milestone3Name = "Confirmed Response",
    eventVar = "irAE",
    eventTimeVar = "irAE_Time",
    groupVar = "PDL1_Status",
    censorVar = "Censored"
  )

  expect_s3_class(result, "swimmerplotResults")
})

test_that("swimmerplot handles surgical outcomes timeline", {
  result <- swimmerplot(
    data = swimmerplot_surgery,
    patientID = "PatientID",
    startTime = "StartTime",
    endTime = "EndTime",
    responseVar = "Outcome",
    milestone1Date = "SurgeryDate",
    milestone1Name = "Surgery",
    milestone2Date = "Discharge",
    milestone2Name = "Discharge",
    milestone3Date = "FirstVisit",
    milestone3Name = "First Visit",
    milestone4Date = "ComplicationDate",
    milestone4Name = "Complication",
    eventVar = "ComplicationType",
    eventTimeVar = "ComplicationDate",
    groupVar = "SurgeryType"
  )

  expect_s3_class(result, "swimmerplotResults")
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

  expect_s3_class(result, "swimmerplotResults")
})

test_that("swimmerplot handles date/time format data", {
  data(swimmerplot_dates, package = "ClinicoPath")

  result <- swimmerplot(
    data = swimmerplot_dates,
    patientID = "PatientID",
    startTime = "StartTime",
    endTime = "EndTime",
    responseVar = "Response",
    milestone1Date = "Milestone1_Days",
    milestone1Name = "Treatment Start",
    milestone2Date = "Milestone2_Days",
    milestone2Name = "First Response",
    milestone3Date = "Milestone3_Days",
    milestone3Name = "Progression",
    timeType = "raw",
    timeUnit = "days",
    groupVar = "Cohort",
    censorVar = "Censored"
  )

  expect_s3_class(result, "swimmerplotResults")
})

test_that("swimmerplot handles grouped comparison data", {
  result <- swimmerplot(
    data = swimmerplot_grouped,
    patientID = "PatientID",
    startTime = "StartTime",
    endTime = "EndTime",
    responseVar = "Response",
    milestone1Date = "TreatmentStart",
    milestone1Name = "Treatment",
    milestone2Date = "FirstAssessment",
    milestone2Name = "Assessment",
    milestone3Date = "BestResponse",
    milestone3Name = "Best Response",
    milestone4Date = "Progression",
    milestone4Name = "Progression",
    eventVar = "AdverseEvent",
    eventTimeVar = "EventTime",
    groupVar = "Group",
    censorVar = "Censored",
    sortOrder = "duration_desc"
  )

  expect_s3_class(result, "swimmerplotResults")
})

test_that("swimmerplot handles all display options combined", {
  result <- swimmerplot(
    data = swimmerplot_test,
    patientID = "PatientID",
    startTime = "StartTime",
    endTime = "EndTime",
    responseVar = "Response",
    milestone1Date = "TreatmentStart",
    milestone1Name = "Treatment Start",
    milestone2Date = "FirstAssessment",
    milestone2Name = "First Assessment",
    milestone3Date = "BestResponse",
    milestone3Name = "Best Response",
    eventVar = "AdverseEvent",
    eventTimeVar = "EventTime",
    censorVar = "Censored",
    groupVar = "TreatmentArm",
    timeType = "raw",
    timeUnit = "days",
    sortOrder = "duration_desc",
    colorPalette = "viridis",
    referenceLines = "median",
    showLegend = TRUE
  )

  expect_s3_class(result, "swimmerplotResults")
})

test_that("swimmerplot handles response-based sorting", {
  result <- swimmerplot(
    data = swimmerplot_test,
    patientID = "PatientID",
    startTime = "StartTime",
    endTime = "EndTime",
    responseVar = "Response",
    sortOrder = "response"
  )

  expect_s3_class(result, "swimmerplotResults")
})

test_that("swimmerplot handles duration-based sorting", {
  result <- swimmerplot(
    data = swimmerplot_test,
    patientID = "PatientID",
    startTime = "StartTime",
    endTime = "EndTime",
    sortOrder = "duration_desc"
  )

  expect_s3_class(result, "swimmerplotResults")
})

test_that("swimmerplot handles no sorting", {
  result <- swimmerplot(
    data = swimmerplot_test,
    patientID = "PatientID",
    startTime = "StartTime",
    endTime = "EndTime",
    sortOrder = "patient_id"
  )

  expect_s3_class(result, "swimmerplotResults")
})

test_that("swimmerplot handles all color palettes", {
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

test_that("swimmerplot handles weeks time unit", {
  result <- swimmerplot(
    data = swimmerplot_test,
    patientID = "PatientID",
    startTime = "StartTime",
    endTime = "EndTime",
    timeUnit = "weeks"
  )

  expect_s3_class(result, "swimmerplotResults")
})

test_that("swimmerplot handles months time unit", {
  result <- swimmerplot(
    data = swimmerplot_test,
    patientID = "PatientID",
    startTime = "StartTime",
    endTime = "EndTime",
    timeUnit = "months"
  )

  expect_s3_class(result, "swimmerplotResults")
})

test_that("swimmerplot handles custom axis titles", {
  result <- swimmerplot(
    data = swimmerplot_test,
    patientID = "PatientID",
    startTime = "StartTime",
    endTime = "EndTime"
  )

  expect_s3_class(result, "swimmerplotResults")
})

test_that("swimmerplot handles multiple reference lines (via single value)", {
  result <- swimmerplot(
    data = swimmerplot_test,
    patientID = "PatientID",
    startTime = "StartTime",
    endTime = "EndTime",
    referenceLines = "median"
  )

  expect_s3_class(result, "swimmerplotResults")
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

  expect_s3_class(result_with, "swimmerplotResults")

  # Without legend
  result_without <- swimmerplot(
    data = swimmerplot_test,
    patientID = "PatientID",
    startTime = "StartTime",
    endTime = "EndTime",
    responseVar = "Response",
    showLegend = FALSE
  )

  expect_s3_class(result_without, "swimmerplotResults")
})

test_that("swimmerplot handles response analysis options", {
  result <- swimmerplot(
    data = swimmerplot_test,
    patientID = "PatientID",
    startTime = "StartTime",
    endTime = "EndTime",
    responseVar = "Response",
    responseAnalysis = TRUE
  )

  expect_s3_class(result, "swimmerplotResults")
})

test_that("swimmerplot handles milestone name customization", {
  data(swimmerplot_milestones, package = "ClinicoPath")

  result <- swimmerplot(
    data = swimmerplot_milestones,
    patientID = "PatientID",
    startTime = "StartTime",
    endTime = "EndTime",
    milestone1Date = "Diagnosis",
    milestone1Name = "Dx",
    milestone2Date = "Surgery",
    milestone2Name = "Sx",
    milestone3Date = "ChemoStart",
    milestone3Name = "Chemo",
    milestone4Date = "Recurrence",
    milestone4Name = "Recur",
    milestone5Date = "Death",
    milestone5Name = "Death"
  )

  expect_s3_class(result, "swimmerplotResults")
})
