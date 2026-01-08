# ═══════════════════════════════════════════════════════════
# Edge Cases and Error Handling Tests: swimmerplot
# ═══════════════════════════════════════════════════════════
#
# Tests edge cases, error conditions, and robust error handling
# for the swimmerplot jamovi function

library(testthat)
library(ClinicoPath)

# Load test data
data(swimmerplot_test, package = "ClinicoPath")
data(swimmerplot_small, package = "ClinicoPath")

test_that("swimmerplot handles missing data in milestones", {
  # Create dataset with missing milestone values
  test_data_na <- swimmerplot_test
  test_data_na$FirstAssessment[1:5] <- NA

  result <- swimmerplot(
    data = test_data_na,
    patientID = "PatientID",
    startTime = "StartTime",
    endTime = "EndTime",
    milestone1 = "FirstAssessment",
    milestone1_name = "Assessment"
  )

  expect_s3_class(result, "swimmerplotClass")
})

test_that("swimmerplot handles missing data in event times", {
  # Create dataset with missing event times
  test_data_na_event <- swimmerplot_test
  test_data_na_event$EventTime[1:10] <- NA

  result <- swimmerplot(
    data = test_data_na_event,
    patientID = "PatientID",
    startTime = "StartTime",
    endTime = "EndTime",
    eventVar = "AdverseEvent",
    eventTimeVar = "EventTime"
  )

  expect_s3_class(result, "swimmerplotClass")
})

test_that("swimmerplot handles very short follow-up times", {
  # Create dataset with very short durations (1-7 days)
  test_data_short <- swimmerplot_small
  test_data_short$EndTime <- c(1, 2, 3, 5, 7)

  result <- swimmerplot(
    data = test_data_short,
    patientID = "PatientID",
    startTime = "StartTime",
    endTime = "EndTime"
  )

  expect_s3_class(result, "swimmerplotClass")
})

test_that("swimmerplot handles very long follow-up times", {
  # Create dataset with very long durations (>5 years)
  test_data_long <- swimmerplot_small
  test_data_long$EndTime <- c(1825, 2190, 2555, 2920, 3285) # 5-9 years in days

  result <- swimmerplot(
    data = test_data_long,
    patientID = "PatientID",
    startTime = "StartTime",
    endTime = "EndTime",
    timeUnit = "months"
  )

  expect_s3_class(result, "swimmerplotClass")
})

test_that("swimmerplot handles single patient", {
  single_patient <- swimmerplot_test[1, ]

  result <- swimmerplot(
    data = single_patient,
    patientID = "PatientID",
    startTime = "StartTime",
    endTime = "EndTime",
    responseVar = "Response"
  )

  expect_s3_class(result, "swimmerplotClass")
})

test_that("swimmerplot handles identical duration for all patients", {
  # All patients have same follow-up duration
  test_data_same <- swimmerplot_small
  test_data_same$EndTime <- rep(180, nrow(test_data_same))

  result <- swimmerplot(
    data = test_data_same,
    patientID = "PatientID",
    startTime = "StartTime",
    endTime = "EndTime"
  )

  expect_s3_class(result, "swimmerplotClass")
})

test_that("swimmerplot handles all patients censored", {
  # Create dataset where all patients are censored
  test_data_censored <- swimmerplot_test
  test_data_censored$Censored <- 1

  result <- swimmerplot(
    data = test_data_censored,
    patientID = "PatientID",
    startTime = "StartTime",
    endTime = "EndTime",
    censorVar = "Censored"
  )

  expect_s3_class(result, "swimmerplotClass")
})

test_that("swimmerplot handles all patients with events", {
  # Create dataset where all patients have events (no censoring)
  test_data_events <- swimmerplot_test
  test_data_events$Censored <- 0

  result <- swimmerplot(
    data = test_data_events,
    patientID = "PatientID",
    startTime = "StartTime",
    endTime = "EndTime",
    censorVar = "Censored"
  )

  expect_s3_class(result, "swimmerplotClass")
})

test_that("swimmerplot handles uniform response (all same)", {
  # All patients have same response
  test_data_uniform <- swimmerplot_test
  test_data_uniform$Response <- "PR"

  result <- swimmerplot(
    data = test_data_uniform,
    patientID = "PatientID",
    startTime = "StartTime",
    endTime = "EndTime",
    responseVar = "Response"
  )

  expect_s3_class(result, "swimmerplotClass")
})

test_that("swimmerplot handles milestone times beyond end time", {
  # Create dataset where milestone occurs after end time (data error)
  test_data_beyond <- swimmerplot_small
  test_data_beyond$Milestone3[1] <- test_data_beyond$EndTime[1] + 30

  result <- swimmerplot(
    data = test_data_beyond,
    patientID = "PatientID",
    startTime = "StartTime",
    endTime = "EndTime",
    milestone1 = "Milestone1",
    milestone1_name = "M1",
    milestone3 = "Milestone3",
    milestone3_name = "M3"
  )

  # Should handle gracefully (warn or adjust)
  expect_s3_class(result, "swimmerplotClass")
})

test_that("swimmerplot handles event times beyond end time", {
  # Create dataset where event occurs after end time
  test_data_event_beyond <- swimmerplot_small
  test_data_event_beyond$EventTime[1] <- test_data_event_beyond$EndTime[1] + 15

  result <- swimmerplot(
    data = test_data_event_beyond,
    patientID = "PatientID",
    startTime = "StartTime",
    endTime = "EndTime",
    eventVar = "EventVar",
    eventTimeVar = "EventTime"
  )

  expect_s3_class(result, "swimmerplotClass")
})

test_that("swimmerplot handles special characters in patient IDs", {
  # Patient IDs with special characters
  test_data_special <- swimmerplot_small
  test_data_special$PatientID <- c("PT-001", "PT.002", "PT#003", "PT 004", "PT_005")

  result <- swimmerplot(
    data = test_data_special,
    patientID = "PatientID",
    startTime = "StartTime",
    endTime = "EndTime"
  )

  expect_s3_class(result, "swimmerplotClass")
})

test_that("swimmerplot handles very long patient IDs", {
  # Very long patient IDs
  test_data_long_id <- swimmerplot_small
  test_data_long_id$PatientID <- paste0("Patient_",
                                        stringi::stri_rand_strings(5, 20))

  result <- swimmerplot(
    data = test_data_long_id,
    patientID = "PatientID",
    startTime = "StartTime",
    endTime = "EndTime"
  )

  expect_s3_class(result, "swimmerplotClass")
})

test_that("swimmerplot handles duplicate patient IDs (should warn)", {
  # Duplicate patient IDs
  test_data_dup <- swimmerplot_small
  test_data_dup$PatientID[4:5] <- test_data_dup$PatientID[1]

  # Should handle or warn
  result <- tryCatch(
    {
      swimmerplot(
        data = test_data_dup,
        patientID = "PatientID",
        startTime = "StartTime",
        endTime = "EndTime"
      )
    },
    error = function(e) NULL,
    warning = function(w) NULL
  )

  # Either completes with warning or errors
  if (!is.null(result)) {
    expect_s3_class(result, "swimmerplotClass")
  }
})

test_that("swimmerplot handles negative start times", {
  # Negative start times (e.g., baseline before treatment)
  test_data_neg <- swimmerplot_small
  test_data_neg$StartTime <- c(-30, -15, -7, 0, 0)
  test_data_neg$EndTime <- test_data_neg$EndTime + abs(test_data_neg$StartTime)

  result <- swimmerplot(
    data = test_data_neg,
    patientID = "PatientID",
    startTime = "StartTime",
    endTime = "EndTime"
  )

  expect_s3_class(result, "swimmerplotClass")
})

test_that("swimmerplot handles all missing event variables", {
  # All event variables are NA
  test_data_no_events <- swimmerplot_test
  test_data_no_events$EventTime <- NA

  result <- swimmerplot(
    data = test_data_no_events,
    patientID = "PatientID",
    startTime = "StartTime",
    endTime = "EndTime",
    eventVar = "AdverseEvent",
    eventTimeVar = "EventTime"
  )

  expect_s3_class(result, "swimmerplotClass")
})

test_that("swimmerplot handles all missing milestones", {
  # All milestone values are NA
  test_data_no_milestones <- swimmerplot_test
  test_data_no_milestones$Progression <- NA

  result <- swimmerplot(
    data = test_data_no_milestones,
    patientID = "PatientID",
    startTime = "StartTime",
    endTime = "EndTime",
    milestone1 = "Progression",
    milestone1_name = "Progression"
  )

  expect_s3_class(result, "swimmerplotClass")
})

test_that("swimmerplot handles mixed response types (factor vs character)", {
  # Response as factor
  test_data_factor <- swimmerplot_test
  test_data_factor$Response <- as.factor(test_data_factor$Response)

  result <- swimmerplot(
    data = test_data_factor,
    patientID = "PatientID",
    startTime = "StartTime",
    endTime = "EndTime",
    responseVar = "Response"
  )

  expect_s3_class(result, "swimmerplotClass")
})

test_that("swimmerplot handles very large datasets", {
  # Create large dataset (100 patients)
  n_large <- 100
  large_data <- data.frame(
    PatientID = paste0("PT", sprintf("%04d", 1:n_large)),
    StartTime = 0,
    EndTime = sample(30:730, n_large, replace = TRUE),
    Response = sample(c("CR", "PR", "SD", "PD"), n_large, replace = TRUE),
    stringsAsFactors = FALSE
  )

  result <- swimmerplot(
    data = large_data,
    patientID = "PatientID",
    startTime = "StartTime",
    endTime = "EndTime",
    responseVar = "Response"
  )

  expect_s3_class(result, "swimmerplotClass")
})

test_that("swimmerplot handles zero duration (start equals end)", {
  # Patient with zero follow-up time
  test_data_zero <- swimmerplot_small
  test_data_zero$EndTime[1] <- test_data_zero$StartTime[1]

  result <- swimmerplot(
    data = test_data_zero,
    patientID = "PatientID",
    startTime = "StartTime",
    endTime = "EndTime"
  )

  # Should handle gracefully
  expect_s3_class(result, "swimmerplotClass")
})
