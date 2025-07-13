test_that("swimmerplotUnified function exists and basic functionality works", {
  
  # Test function existence
  expect_true(exists("swimmerplotUnified"))
  expect_true(is.function(swimmerplotUnified))
  
  # Test with minimal valid parameters - should not error
  expect_no_error({
    test_data <- data.frame(
      patient_id = paste0("PT", 1:5),
      start_time = c(0, 0, 0, 0, 0),
      end_time = c(12, 8, 15, 6, 9),
      stringsAsFactors = FALSE
    )
    
    result <- swimmerplotUnified(
      data = test_data,
      patientID = "patient_id",
      startTime = "start_time",
      endTime = "end_time"
    )
  })
})

test_that("swimmerplotUnified handles raw numeric data correctly", {
  skip_if_not_installed("ggswim")
  skip_if_not_installed("dplyr")
  
  # Create test data with raw numeric times
  test_data <- data.frame(
    patient_id = paste0("PT", sprintf("%03d", 1:10)),
    start_time = rep(0, 10),
    end_time = sample(6:24, 10, replace = TRUE),
    response = sample(c("CR", "PR", "SD", "PD"), 10, replace = TRUE),
    stringsAsFactors = FALSE
  )
  
  result <- swimmerplotUnified(
    data = test_data,
    patientID = "patient_id",
    startTime = "start_time",
    endTime = "end_time",
    responseVar = "response",
    timeType = "raw",
    timeUnit = "months"
  )
  
  # Basic checks
  expect_s3_class(result, "swimmerplotUnifiedResults")
  expect_true("plot" %in% names(result))
  expect_true("summary" %in% names(result))
})

test_that("swimmerplotUnified handles datetime data correctly", {
  skip_if_not_installed("ggswim")
  skip_if_not_installed("lubridate")
  
  # Create test data with datetime
  test_data <- data.frame(
    patient_id = paste0("PT", 1:5),
    start_date = c("2023-01-01", "2023-01-15", "2023-02-01", "2023-02-15", "2023-03-01"),
    end_date = c("2023-06-01", "2023-04-15", "2023-08-01", "2023-05-15", "2023-09-01"),
    response = c("CR", "PR", "SD", "PD", "CR"),
    stringsAsFactors = FALSE
  )
  
  result <- swimmerplotUnified(
    data = test_data,
    patientID = "patient_id",
    startTime = "start_date",
    endTime = "end_date",
    responseVar = "response",
    timeType = "datetime",
    dateFormat = "ymd",
    timeUnit = "months",
    timeDisplay = "relative"
  )
  
  expect_s3_class(result, "swimmerplotUnifiedResults")
  expect_true(result$plot$visible)
})

test_that("swimmerplotUnified milestone functionality works", {
  skip_if_not_installed("ggswim")
  
  # Create test data with milestones
  test_data <- data.frame(
    patient_id = paste0("PT", 1:8),
    start_time = rep(0, 8),
    end_time = sample(6:24, 8, replace = TRUE),
    response = sample(c("CR", "PR", "SD", "PD"), 8, replace = TRUE),
    surgery = sample(c(1, 2, 3, NA), 8, replace = TRUE),
    progression = sample(c(8, 12, 16, NA), 8, replace = TRUE),
    stringsAsFactors = FALSE
  )
  
  result <- swimmerplotUnified(
    data = test_data,
    patientID = "patient_id",
    startTime = "start_time",
    endTime = "end_time",
    responseVar = "response",
    milestone1Name = "Surgery",
    milestone1Date = "surgery",
    milestone2Name = "Progression",
    milestone2Date = "progression",
    timeType = "raw",
    timeUnit = "months"
  )
  
  expect_s3_class(result, "swimmerplotUnifiedResults")
  expect_true("milestoneTable" %in% names(result))
})

test_that("swimmerplotUnified event markers work correctly", {
  skip_if_not_installed("ggswim")
  
  # Create test data with event markers
  test_data <- data.frame(
    patient_id = rep(paste0("PT", 1:5), each = 2),
    start_time = rep(c(0, 6), 5),
    end_time = rep(c(6, 12), 5),
    response = rep(c("PR", "CR"), 5),
    event_type = rep(c("Treatment Start", "Response Assessment"), 5),
    stringsAsFactors = FALSE
  )
  
  result <- swimmerplotUnified(
    data = test_data,
    patientID = "patient_id",
    startTime = "start_time",
    endTime = "end_time",
    responseVar = "response",
    showEventMarkers = TRUE,
    eventVar = "event_type",
    eventTimeVar = "start_time"
  )
  
  expect_s3_class(result, "swimmerplotUnifiedResults")
  expect_true(result$eventMarkerTable$visible)
})

test_that("swimmerplotUnified handles different sorting options", {
  skip_if_not_installed("ggswim")
  
  test_data <- data.frame(
    patient_id = paste0("PT", 1:6),
    start_time = rep(0, 6),
    end_time = c(12, 8, 15, 6, 9, 18),
    response = c("CR", "PR", "SD", "PD", "CR", "PR"),
    priority = c(1, 3, 2, 1, 2, 3),
    stringsAsFactors = FALSE
  )
  
  # Test duration sorting
  result1 <- swimmerplotUnified(
    data = test_data,
    patientID = "patient_id",
    startTime = "start_time",
    endTime = "end_time",
    responseVar = "response",
    sortOrder = "duration_desc"
  )
  expect_s3_class(result1, "swimmerplotUnifiedResults")
  
  # Test custom variable sorting
  result2 <- swimmerplotUnified(
    data = test_data,
    patientID = "patient_id",
    startTime = "start_time",
    endTime = "end_time",
    responseVar = "response",
    sortVariable = "priority"
  )
  expect_s3_class(result2, "swimmerplotUnifiedResults")
})

test_that("swimmerplotUnified person-time analysis works", {
  skip_if_not_installed("ggswim")
  
  test_data <- data.frame(
    patient_id = paste0("PT", 1:10),
    start_time = rep(0, 10),
    end_time = sample(6:24, 10, replace = TRUE),
    response = sample(c("CR", "PR", "SD", "PD"), 10, replace = TRUE),
    stringsAsFactors = FALSE
  )
  
  result <- swimmerplotUnified(
    data = test_data,
    patientID = "patient_id",
    startTime = "start_time",
    endTime = "end_time",
    responseVar = "response",
    personTimeAnalysis = TRUE,
    responseAnalysis = TRUE
  )
  
  expect_s3_class(result, "swimmerplotUnifiedResults")
  expect_true(result$personTimeTable$visible)
  expect_true(result$advancedMetrics$visible)
})

test_that("swimmerplotUnified handles reference lines correctly", {
  skip_if_not_installed("ggswim")
  
  test_data <- data.frame(
    patient_id = paste0("PT", 1:5),
    start_time = rep(0, 5),
    end_time = c(12, 8, 15, 6, 9),
    stringsAsFactors = FALSE
  )
  
  # Test median reference line
  result1 <- swimmerplotUnified(
    data = test_data,
    patientID = "patient_id",
    startTime = "start_time",
    endTime = "end_time",
    referenceLines = "median"
  )
  expect_s3_class(result1, "swimmerplotUnifiedResults")
  
  # Test protocol reference lines
  result2 <- swimmerplotUnified(
    data = test_data,
    patientID = "patient_id",
    startTime = "start_time",
    endTime = "end_time",
    referenceLines = "protocol"
  )
  expect_s3_class(result2, "swimmerplotUnifiedResults")
  
  # Test custom reference line
  result3 <- swimmerplotUnified(
    data = test_data,
    patientID = "patient_id",
    startTime = "start_time",
    endTime = "end_time",
    referenceLines = "custom",
    customReferenceTime = 10
  )
  expect_s3_class(result3, "swimmerplotUnifiedResults")
})

test_that("swimmerplotUnified theme options work", {
  skip_if_not_installed("ggswim")
  
  test_data <- data.frame(
    patient_id = paste0("PT", 1:3),
    start_time = c(0, 0, 0),
    end_time = c(12, 8, 15),
    stringsAsFactors = FALSE
  )
  
  # Test ggswim theme
  result1 <- swimmerplotUnified(
    data = test_data,
    patientID = "patient_id",
    startTime = "start_time",
    endTime = "end_time",
    plotTheme = "ggswim"
  )
  expect_s3_class(result1, "swimmerplotUnifiedResults")
  
  # Test ggswim dark theme
  result2 <- swimmerplotUnified(
    data = test_data,
    patientID = "patient_id",
    startTime = "start_time",
    endTime = "end_time",
    plotTheme = "ggswim_dark"
  )
  expect_s3_class(result2, "swimmerplotUnifiedResults")
  
  # Test minimal theme
  result3 <- swimmerplotUnified(
    data = test_data,
    patientID = "patient_id",
    startTime = "start_time",
    endTime = "end_time",
    plotTheme = "minimal"
  )
  expect_s3_class(result3, "swimmerplotUnifiedResults")
})

test_that("swimmerplotUnified export functionality works", {
  skip_if_not_installed("ggswim")
  
  test_data <- data.frame(
    patient_id = paste0("PT", 1:5),
    start_time = rep(0, 5),
    end_time = c(12, 8, 15, 6, 9),
    response = c("CR", "PR", "SD", "PD", "CR"),
    stringsAsFactors = FALSE
  )
  
  result <- swimmerplotUnified(
    data = test_data,
    patientID = "patient_id",
    startTime = "start_time",
    endTime = "end_time",
    responseVar = "response",
    exportTimeline = TRUE,
    exportSummary = TRUE
  )
  
  expect_s3_class(result, "swimmerplotUnifiedResults")
  expect_true(result$timelineData$visible)
  expect_true(result$summaryData$visible)
  expect_true(result$exportInfo$visible)
})

test_that("swimmerplotUnified handles data validation correctly", {
  # Test missing required variables
  expect_error({
    test_data <- data.frame(patient_id = 1:3)
    swimmerplotUnified(
      data = test_data,
      patientID = "patient_id"
      # Missing startTime and endTime
    )
  })
  
  # Test invalid time data
  expect_error({
    test_data <- data.frame(
      patient_id = paste0("PT", 1:3),
      start_time = c(10, 5, 8),
      end_time = c(5, 3, 7)  # End times before start times
    )
    swimmerplotUnified(
      data = test_data,
      patientID = "patient_id",
      startTime = "start_time",
      endTime = "end_time"
    )
  })
})

test_that("swimmerplotUnified handles edge cases", {
  skip_if_not_installed("ggswim")
  
  # Test with minimal data
  test_data <- data.frame(
    patient_id = "PT001",
    start_time = 0,
    end_time = 12,
    stringsAsFactors = FALSE
  )
  
  expect_no_error({
    result <- swimmerplotUnified(
      data = test_data,
      patientID = "patient_id",
      startTime = "start_time",
      endTime = "end_time"
    )
  })
  
  # Test with all same durations
  test_data2 <- data.frame(
    patient_id = paste0("PT", 1:5),
    start_time = rep(0, 5),
    end_time = rep(12, 5),  # All same duration
    stringsAsFactors = FALSE
  )
  
  expect_no_error({
    result <- swimmerplotUnified(
      data = test_data2,
      patientID = "patient_id",
      startTime = "start_time",
      endTime = "end_time"
    )
  })
})

test_that("swimmerplotUnified clinical interpretation works", {
  skip_if_not_installed("ggswim")
  
  test_data <- data.frame(
    patient_id = paste0("PT", 1:10),
    start_time = rep(0, 10),
    end_time = sample(6:24, 10, replace = TRUE),
    response = sample(c("CR", "PR", "SD", "PD"), 10, replace = TRUE),
    stringsAsFactors = FALSE
  )
  
  result <- swimmerplotUnified(
    data = test_data,
    patientID = "patient_id",
    startTime = "start_time",
    endTime = "end_time",
    responseVar = "response",
    showInterpretation = TRUE,
    personTimeAnalysis = TRUE,
    responseAnalysis = TRUE
  )
  
  expect_s3_class(result, "swimmerplotUnifiedResults")
  expect_true(result$interpretation$visible)
})

test_that("swimmerplotUnified handles missing milestone data gracefully", {
  skip_if_not_installed("ggswim")
  
  test_data <- data.frame(
    patient_id = paste0("PT", 1:5),
    start_time = rep(0, 5),
    end_time = c(12, 8, 15, 6, 9),
    milestone1 = c(3, NA, 5, NA, 2),  # Some missing values
    milestone2 = rep(NA, 5),          # All missing values
    stringsAsFactors = FALSE
  )
  
  expect_no_error({
    result <- swimmerplotUnified(
      data = test_data,
      patientID = "patient_id",
      startTime = "start_time",
      endTime = "end_time",
      milestone1Name = "Event1",
      milestone1Date = "milestone1",
      milestone2Name = "Event2",
      milestone2Date = "milestone2"
    )
  })
})

test_that("swimmerplotUnified performance with larger datasets", {
  skip_if_not_installed("ggswim")
  
  # Create larger test dataset
  n_patients <- 100
  test_data <- data.frame(
    patient_id = paste0("PT", sprintf("%03d", 1:n_patients)),
    start_time = rep(0, n_patients),
    end_time = sample(6:48, n_patients, replace = TRUE),
    response = sample(c("CR", "PR", "SD", "PD"), n_patients, replace = TRUE),
    stringsAsFactors = FALSE
  )
  
  expect_no_error({
    start_time <- Sys.time()
    result <- swimmerplotUnified(
      data = test_data,
      patientID = "patient_id",
      startTime = "start_time",
      endTime = "end_time",
      responseVar = "response",
      personTimeAnalysis = TRUE
    )
    end_time <- Sys.time()
    
    # Should complete in reasonable time (less than 30 seconds)
    expect_lt(as.numeric(end_time - start_time), 30)
  })
})