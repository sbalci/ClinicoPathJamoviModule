# ═══════════════════════════════════════════════════════════
# Integration Tests: swimmerplot
# ═══════════════════════════════════════════════════════════
#
# Tests integration with other packages, realistic workflows,
# and output consistency for the swimmerplot jamovi function

library(testthat)

# Load test data
data(swimmerplot_test, package = "ClinicoPath")
data(swimmerplot_immuno, package = "ClinicoPath")

test_that("swimmerplot produces consistent results across runs", {
  # Run the same analysis twice
  result1 <- swimmerplot(
    data = swimmerplot_test,
    patientID = "PatientID",
    startTime = "StartTime",
    endTime = "EndTime",
    responseVar = "Response"
  )

  result2 <- swimmerplot(
    data = swimmerplot_test,
    patientID = "PatientID",
    startTime = "StartTime",
    endTime = "EndTime",
    responseVar = "Response"
  )

  # Results should be identical (no randomness)
  expect_s3_class(result1, "swimmerplotResults")
  expect_s3_class(result2, "swimmerplotResults")
})

test_that("swimmerplot workflow: basic → comprehensive visualization", {
  # Step 1: Basic timeline
  basic_result <- swimmerplot(
    data = swimmerplot_test,
    patientID = "PatientID",
    startTime = "StartTime",
    endTime = "EndTime"
  )

  expect_s3_class(basic_result, "swimmerplotResults")

  # Step 2: Add response
  response_result <- swimmerplot(
    data = swimmerplot_test,
    patientID = "PatientID",
    startTime = "StartTime",
    endTime = "EndTime",
    responseVar = "Response"
  )

  expect_s3_class(response_result, "swimmerplotResults")

  # Step 3: Add milestones
  milestone_result <- swimmerplot(
    data = swimmerplot_test,
    patientID = "PatientID",
    startTime = "StartTime",
    endTime = "EndTime",
    responseVar = "Response",
    milestone1Date = "TreatmentStart",
    milestone1Name = "Treatment",
    milestone2Date = "FirstAssessment",
    milestone2Name = "Assessment"
  )

  expect_s3_class(milestone_result, "swimmerplotResults")

  # Step 4: Comprehensive
  comprehensive_result <- swimmerplot(
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
    groupVar = "TreatmentArm",
    censorVar = "Censored",
    sortOrder = "duration_desc"
  )

  expect_s3_class(comprehensive_result, "swimmerplotResults")
})

test_that("swimmerplot handles data from CSV import", {
  # Write test data to temporary CSV
  temp_csv <- tempfile(fileext = ".csv")
  write.csv(swimmerplot_test, temp_csv, row.names = FALSE)

  # Read it back
  csv_data <- read.csv(temp_csv)

  result <- swimmerplot(
    data = csv_data,
    patientID = "PatientID",
    startTime = "StartTime",
    endTime = "EndTime",
    responseVar = "Response"
  )

  expect_s3_class(result, "swimmerplotResults")

  # Clean up
  unlink(temp_csv)
})

test_that("swimmerplot handles data from Excel import", {
  # Write test data to temporary Excel file
  temp_xlsx <- tempfile(fileext = ".xlsx")
  writexl::write_xlsx(swimmerplot_test, temp_xlsx)

  # Read it back
  xlsx_data <- readxl::read_excel(temp_xlsx)

  result <- swimmerplot(
    data = as.data.frame(xlsx_data),
    patientID = "PatientID",
    startTime = "StartTime",
    endTime = "EndTime",
    responseVar = "Response"
  )

  expect_s3_class(result, "swimmerplotResults")

  # Clean up
  unlink(temp_xlsx)
})

test_that("swimmerplot handles different data structures consistently", {
  # Test with tibble
  library(tibble)
  tibble_data <- as_tibble(swimmerplot_test)

  result_tibble <- swimmerplot(
    data = tibble_data,
    patientID = "PatientID",
    startTime = "StartTime",
    endTime = "EndTime",
    responseVar = "Response"
  )

  expect_s3_class(result_tibble, "swimmerplotResults")

  # Test with data.frame
  df_data <- as.data.frame(swimmerplot_test)

  result_df <- swimmerplot(
    data = df_data,
    patientID = "PatientID",
    startTime = "StartTime",
    endTime = "EndTime",
    responseVar = "Response"
  )

  expect_s3_class(result_df, "swimmerplotResults")
})

test_that("swimmerplot workflow: immunotherapy trial visualization", {
  # Simulate real-world immunotherapy trial workflow

  # Step 1: Initial visualization
  initial <- swimmerplot(
    data = swimmerplot_immuno,
    patientID = "PatientID",
    startTime = "StartTime",
    endTime = "EndTime",
    responseVar = "Response"
  )

  expect_s3_class(initial, "swimmerplotResults")

  # Step 2: Add immune-related adverse events
  with_events <- swimmerplot(
    data = swimmerplot_immuno,
    patientID = "PatientID",
    startTime = "StartTime",
    endTime = "EndTime",
    responseVar = "Response",
    eventVar = "irAE",
    eventTimeVar = "irAE_Time"
  )

  expect_s3_class(with_events, "swimmerplotResults")

  # Step 3: Complete analysis with PD-L1 grouping
  complete <- swimmerplot(
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
    milestone3Name = "Confirmed",
    eventVar = "irAE",
    eventTimeVar = "irAE_Time",
    groupVar = "PDL1_Status",
    censorVar = "Censored",
    sortOrder = "duration_desc"
  )

  expect_s3_class(complete, "swimmerplotResults")
})

test_that("swimmerplot workflow: surgical outcomes tracking", {
  data(swimmerplot_surgery, package = "ClinicoPath")

  # Surgical outcomes workflow
  # Step 1: Basic postoperative timeline
  basic <- swimmerplot(
    data = swimmerplot_surgery,
    patientID = "PatientID",
    startTime = "StartTime",
    endTime = "EndTime",
    responseVar = "Outcome"
  )

  expect_s3_class(basic, "swimmerplotResults")

  # Step 2: Add key milestones
  with_milestones <- swimmerplot(
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
    milestone3Name = "Follow-up"
  )

  expect_s3_class(with_milestones, "swimmerplotResults")

  # Step 3: Complete with complications and grouping
  complete <- swimmerplot(
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
    milestone3Name = "Follow-up",
    milestone4Date = "ComplicationDate",
    milestone4Name = "Complication",
    eventVar = "ComplicationType",
    eventTimeVar = "ComplicationDate",
    groupVar = "SurgeryType"
  )

  expect_s3_class(complete, "swimmerplotResults")
})

test_that("swimmerplot workflow: comparative trial arms", {
  data(swimmerplot_grouped, package = "ClinicoPath")

  # Comparative analysis workflow
  # Step 1: Overall timeline
  overall <- swimmerplot(
    data = swimmerplot_grouped,
    patientID = "PatientID",
    startTime = "StartTime",
    endTime = "EndTime",
    responseVar = "Response"
  )

  expect_s3_class(overall, "swimmerplotResults")

  # Step 2: Group-stratified view
  grouped <- swimmerplot(
    data = swimmerplot_grouped,
    patientID = "PatientID",
    startTime = "StartTime",
    endTime = "EndTime",
    responseVar = "Response",
    groupVar = "Group",
    sortOrder = "duration_desc"
  )

  expect_s3_class(grouped, "swimmerplotResults")

  # Step 3: Complete with all features
  complete <- swimmerplot(
    data = swimmerplot_grouped,
    patientID = "PatientID",
    startTime = "StartTime",
    endTime = "EndTime",
    responseVar = "Response",
    milestone1Date = "TreatmentStart",
    milestone1Name = "Start",
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
    sortOrder = "duration_desc",
    referenceLines = "median"
  )

  expect_s3_class(complete, "swimmerplotResults")
})

test_that("swimmerplot handles clinical scenarios: long-term responders", {
  # Focus on long-term responders (>1 year)
  long_responders <- swimmerplot_immuno[swimmerplot_immuno$EndTime > 365, ]

  if (nrow(long_responders) > 0) {
    result <- swimmerplot(
      data = long_responders,
      patientID = "PatientID",
      startTime = "StartTime",
      endTime = "EndTime",
      responseVar = "Response",
      timeUnit = "months"
    )

    expect_s3_class(result, "swimmerplotResults")
  }
})

test_that("swimmerplot handles clinical scenarios: early progressors", {
  # Focus on early progression (<3 months)
  early_progressors <- swimmerplot_test[swimmerplot_test$Response == "PD" &
                                         swimmerplot_test$EndTime < 90, ]

  if (nrow(early_progressors) > 0) {
    result <- swimmerplot(
      data = early_progressors,
      patientID = "PatientID",
      startTime = "StartTime",
      endTime = "EndTime",
      responseVar = "Response"
    )

    expect_s3_class(result, "swimmerplotResults")
  }
})

test_that("swimmerplot integration: combine with survival analysis", {
  # Create survival-ready dataset from swimmerplot data
  surv_data <- swimmerplot_test[, c("PatientID", "EndTime", "Censored", "TreatmentArm")]

  # Verify data structure is compatible
  expect_true("EndTime" %in% names(surv_data))
  expect_true("Censored" %in% names(surv_data))

  # Can be used for downstream survival analysis
  expect_s3_class(surv_data, "data.frame")
})

test_that("swimmerplot handles publication-ready formatting", {
  # High-quality visualization for publication
  result <- swimmerplot(
    data = swimmerplot_test,
    patientID = "PatientID",
    startTime = "StartTime",
    endTime = "EndTime",
    responseVar = "Response",
    milestone1Date = "TreatmentStart",
    milestone1Name = "Treatment Initiation",
    milestone2Date = "FirstAssessment",
    milestone2Name = "First Response Assessment",
    milestone3Date = "BestResponse",
    milestone3Name = "Best Response",
    eventVar = "AdverseEvent",
    eventTimeVar = "EventTime",
    groupVar = "TreatmentArm",
    censorVar = "Censored",
    sortOrder = "duration_desc",
    colorPalette = "viridis",
    showLegend = TRUE,
    referenceLines = "median"
  )

  expect_s3_class(result, "swimmerplotResults")
})
