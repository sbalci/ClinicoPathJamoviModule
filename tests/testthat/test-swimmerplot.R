test_that("swimmerplot function exists and basic functionality works", {
  
  # Test function existence
  expect_true(exists("swimmerplot"))
  expect_true(is.function(swimmerplot))
  
  # Test with minimal valid parameters - should not error
  expect_no_error({
    test_data <- data.frame(
      patient_id = paste0("PT", 1:5),
      start_time = c(0, 0, 0, 0, 0),
      end_time = c(12, 8, 15, 6, 9),
      stringsAsFactors = FALSE
    )
    
    result <- swimmerplot(
      data = test_data,
      patientID = "patient_id",
      startTime = "start_time",
      endTime = "end_time"
    )
  })
})

test_that("swimmerplot handles raw numeric data correctly", {
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
  
  result <- swimmerplot(
    data = test_data,
    patientID = "patient_id",
    startTime = "start_time",
    endTime = "end_time",
    responseVar = "response",
    timeType = "raw",
    timeUnit = "months"
  )
  
  # Basic checks
  expect_s3_class(result, "swimmerplotResults")
  expect_true("plot" %in% names(result))
  expect_true("summary" %in% names(result))
})

test_that("swimmerplot handles datetime data correctly", {
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
  
  result <- swimmerplot(
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
  
  expect_s3_class(result, "swimmerplotResults")
  expect_true(result$plot$visible)
})

test_that("swimmerplot milestone functionality works", {
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
  
  result <- swimmerplot(
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
  
  expect_s3_class(result, "swimmerplotResults")
  expect_true("milestoneTable" %in% names(result))
})

test_that("swimmerplot event markers work correctly", {
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
  
  result <- swimmerplot(
    data = test_data,
    patientID = "patient_id",
    startTime = "start_time",
    endTime = "end_time",
    responseVar = "response",
    showEventMarkers = TRUE,
    eventVar = "event_type",
    eventTimeVar = "start_time"
  )
  
  expect_s3_class(result, "swimmerplotResults")
  expect_true(result$eventMarkerTable$visible)
})

test_that("swimmerplot handles different sorting options", {
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
  result1 <- swimmerplot(
    data = test_data,
    patientID = "patient_id",
    startTime = "start_time",
    endTime = "end_time",
    responseVar = "response",
    sortOrder = "duration_desc"
  )
  expect_s3_class(result1, "swimmerplotResults")
  # Verify order by duration (desc)
  pd1 <- result1$plot$state$patient_data
  durs1 <- pd1$end_time - pd1$start_time
  # Factor levels reflect desired order
  lev1 <- levels(pd1$patient_id)
  ord_expected <- lev1[order(durs1[match(lev1, as.character(pd1$patient_id))], decreasing = TRUE)]
  expect_identical(lev1, ord_expected)
  
  # Test custom variable sorting
  result2 <- swimmerplot(
    data = test_data,
    patientID = "patient_id",
    startTime = "start_time",
    endTime = "end_time",
    responseVar = "response",
    sortVariable = "priority"
  )
  expect_s3_class(result2, "swimmerplotResults")
  # Verify order by priority (ascending)
  pd2 <- result2$plot$state$patient_data
  df <- test_data[!duplicated(test_data$patient_id), c("patient_id", "priority")]
  rownames(df) <- df$patient_id
  lev2 <- levels(pd2$patient_id)
  ord_expected2 <- lev2[order(df[lev2, "priority"])]
  expect_identical(lev2, ord_expected2)
})

test_that("swimmerplot handles absolute datetime with custom reference", {
  skip_if_not_installed("ggswim")
  skip_if_not_installed("lubridate")
  
  test_data <- data.frame(
    patient_id = paste0("PT", 1:3),
    start_date = as.Date(c("2023-01-01", "2023-01-08", "2023-01-15")),
    end_date = as.Date(c("2023-03-01", "2023-02-15", "2023-04-01")),
    stringsAsFactors = FALSE
  )
  
  result <- swimmerplot(
    data = test_data,
    patientID = "patient_id",
    startTime = "start_date",
    endTime = "end_date",
    timeType = "datetime",
    dateFormat = "ymd",
    timeUnit = "weeks",
    timeDisplay = "absolute",
    referenceLines = "custom",
    customReferenceTime = 10
  )
  expect_s3_class(result, "swimmerplotResults")
  expect_true(result$plot$visible)
  # Ensure absolute dates are used internally
  expect_true(inherits(result$plot$state$patient_data$start_time, c("Date", "POSIXct")))
})

test_that("swimmerplot person-time analysis works", {
  skip_if_not_installed("ggswim")
  
  test_data <- data.frame(
    patient_id = paste0("PT", 1:10),
    start_time = rep(0, 10),
    end_time = sample(6:24, 10, replace = TRUE),
    response = sample(c("CR", "PR", "SD", "PD"), 10, replace = TRUE),
    stringsAsFactors = FALSE
  )
  
  result <- swimmerplot(
    data = test_data,
    patientID = "patient_id",
    startTime = "start_time",
    endTime = "end_time",
    responseVar = "response",
    personTimeAnalysis = TRUE,
    responseAnalysis = TRUE
  )
  
  expect_s3_class(result, "swimmerplotResults")
  expect_true(result$personTimeTable$visible)
  expect_true(result$advancedMetrics$visible)
})

test_that("swimmerplot handles reference lines correctly", {
  skip_if_not_installed("ggswim")
  
  test_data <- data.frame(
    patient_id = paste0("PT", 1:5),
    start_time = rep(0, 5),
    end_time = c(12, 8, 15, 6, 9),
    stringsAsFactors = FALSE
  )
  
  # Test median reference line
  result1 <- swimmerplot(
    data = test_data,
    patientID = "patient_id",
    startTime = "start_time",
    endTime = "end_time",
    referenceLines = "median"
  )
  expect_s3_class(result1, "swimmerplotResults")
  
  # Test protocol reference lines
  result2 <- swimmerplot(
    data = test_data,
    patientID = "patient_id",
    startTime = "start_time",
    endTime = "end_time",
    referenceLines = "protocol"
  )
  expect_s3_class(result2, "swimmerplotResults")
  
  # Test custom reference line
  result3 <- swimmerplot(
    data = test_data,
    patientID = "patient_id",
    startTime = "start_time",
    endTime = "end_time",
    referenceLines = "custom",
    customReferenceTime = 10
  )
  expect_s3_class(result3, "swimmerplotResults")
})

test_that("swimmerplot theme options work", {
  skip_if_not_installed("ggswim")
  
  test_data <- data.frame(
    patient_id = paste0("PT", 1:3),
    start_time = c(0, 0, 0),
    end_time = c(12, 8, 15),
    stringsAsFactors = FALSE
  )
  
  # Test ggswim theme
  result1 <- swimmerplot(
    data = test_data,
    patientID = "patient_id",
    startTime = "start_time",
    endTime = "end_time",
    plotTheme = "ggswim"
  )
  expect_s3_class(result1, "swimmerplotResults")
  
  # Test ggswim dark theme
  result2 <- swimmerplot(
    data = test_data,
    patientID = "patient_id",
    startTime = "start_time",
    endTime = "end_time",
    plotTheme = "ggswim_dark"
  )
  expect_s3_class(result2, "swimmerplotResults")
  
  # Test minimal theme
  result3 <- swimmerplot(
    data = test_data,
    patientID = "patient_id",
    startTime = "start_time",
    endTime = "end_time",
    plotTheme = "minimal"
  )
  expect_s3_class(result3, "swimmerplotResults")
})

test_that("swimmerplot export functionality works", {
  skip_if_not_installed("ggswim")
  
  test_data <- data.frame(
    patient_id = paste0("PT", 1:5),
    start_time = rep(0, 5),
    end_time = c(12, 8, 15, 6, 9),
    response = c("CR", "PR", "SD", "PD", "CR"),
    stringsAsFactors = FALSE
  )
  
  result <- swimmerplot(
    data = test_data,
    patientID = "patient_id",
    startTime = "start_time",
    endTime = "end_time",
    responseVar = "response",
    exportTimeline = TRUE,
    exportSummary = TRUE
  )
  
  expect_s3_class(result, "swimmerplotResults")
  expect_true(result$timelineData$visible)
  expect_true(result$summaryData$visible)
  expect_true(result$exportInfo$visible)
})

test_that("swimmerplot handles data validation correctly", {
  # Test missing required variables
  expect_error({
    test_data <- data.frame(patient_id = 1:3)
    swimmerplot(
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
    swimmerplot(
      data = test_data,
      patientID = "patient_id",
      startTime = "start_time",
      endTime = "end_time"
    )
  })
})

test_that("swimmerplot handles edge cases", {
  skip_if_not_installed("ggswim")
  
  # Test with minimal data
  test_data <- data.frame(
    patient_id = "PT001",
    start_time = 0,
    end_time = 12,
    stringsAsFactors = FALSE
  )
  
  expect_no_error({
    result <- swimmerplot(
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
    result <- swimmerplot(
      data = test_data2,
      patientID = "patient_id",
      startTime = "start_time",
      endTime = "end_time"
    )
  })
})

test_that("swimmerplot clinical interpretation works", {
  skip_if_not_installed("ggswim")
  
  test_data <- data.frame(
    patient_id = paste0("PT", 1:10),
    start_time = rep(0, 10),
    end_time = sample(6:24, 10, replace = TRUE),
    response = sample(c("CR", "PR", "SD", "PD"), 10, replace = TRUE),
    stringsAsFactors = FALSE
  )
  
  result <- swimmerplot(
    data = test_data,
    patientID = "patient_id",
    startTime = "start_time",
    endTime = "end_time",
    responseVar = "response",
    showInterpretation = TRUE,
    personTimeAnalysis = TRUE,
    responseAnalysis = TRUE
  )
  
  expect_s3_class(result, "swimmerplotResults")
  expect_true(result$interpretation$visible)
})

test_that("swimmerplot handles missing milestone data gracefully", {
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
    result <- swimmerplot(
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

test_that("swimmerplot performance with larger datasets", {
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
    result <- swimmerplot(
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

test_that("swimmerplot enhanced clinical glyphs work correctly", {
  skip_if_not_installed("ggswim")
  
  # Test clinical event mapping
  test_data <- data.frame(
    patient_id = paste0("PT", 1:6),
    start_time = rep(0, 6),
    end_time = sample(6:18, 6, replace = TRUE),
    response = sample(c("CR", "PR", "SD", "PD"), 6, replace = TRUE),
    event_type = c("surgery", "treatment", "progression", "adverse event", "death", "follow-up"),
    event_time = c(2, 4, 8, 6, 12, 3),
    stringsAsFactors = FALSE
  )
  
  result <- swimmerplot(
    data = test_data,
    patientID = "patient_id",
    startTime = "start_time",
    endTime = "end_time",
    responseVar = "response",
    showEventMarkers = TRUE,
    eventVar = "event_type",
    eventTimeVar = "event_time"
  )
  
  expect_s3_class(result, "swimmerplotResults")
  expect_true(result$eventMarkerTable$visible)
  expect_true(result$plot$visible)
})

test_that("swimmerplot ongoing treatment arrows work", {
  skip_if_not_installed("ggswim")
  
  # Test ongoing treatment detection with responding patients
  test_data <- data.frame(
    patient_id = paste0("PT", 1:8),
    start_time = rep(0, 8),
    end_time = sample(6:24, 8, replace = TRUE),
    response = c("CR", "PR", "SD", "PD", "CR", "PR", "SD", "PD"),
    stringsAsFactors = FALSE
  )
  
  result <- swimmerplot(
    data = test_data,
    patientID = "patient_id",
    startTime = "start_time",
    endTime = "end_time",
    responseVar = "response",
    plotTheme = "ggswim"
  )
  
  expect_s3_class(result, "swimmerplotResults")
  expect_true(result$plot$visible)
  
  # Should handle ongoing patients (CR, PR, SD responses)
  expect_true("response" %in% names(result$plot$state$patient_data))
})

test_that("swimmerplot handles complex clinical scenarios", {
  skip_if_not_installed("ggswim")
  skip_if_not_installed("lubridate")
  
  # Comprehensive clinical trial scenario
  test_data <- data.frame(
    patient_id = paste0("PT", sprintf("%03d", 1:15)),
    start_date = seq(as.Date("2023-01-01"), by = "weeks", length.out = 15),
    end_date = seq(as.Date("2023-01-01"), by = "weeks", length.out = 15) + sample(50:200, 15),
    response = sample(c("CR", "PR", "SD", "PD"), 15, replace = TRUE, prob = c(0.2, 0.3, 0.3, 0.2)),
    surgery_date = seq(as.Date("2023-01-01"), by = "weeks", length.out = 15) + sample(7:21, 15),
    progression_date = seq(as.Date("2023-01-01"), by = "weeks", length.out = 15) + sample(70:150, 15),
    event_type = sample(c("treatment", "scan", "adverse event", "response assessment"), 15, replace = TRUE),
    event_date = seq(as.Date("2023-01-01"), by = "weeks", length.out = 15) + sample(30:100, 15),
    priority = sample(1:3, 15, replace = TRUE),
    stringsAsFactors = FALSE
  )
  
  result <- swimmerplot(
    data = test_data,
    patientID = "patient_id",
    startTime = "start_date",
    endTime = "end_date",
    responseVar = "response",
    timeType = "datetime",
    dateFormat = "ymd",
    timeUnit = "weeks",
    timeDisplay = "relative",
    milestone1Name = "Surgery",
    milestone1Date = "surgery_date",
    milestone2Name = "Progression",
    milestone2Date = "progression_date",
    showEventMarkers = TRUE,
    eventVar = "event_type",
    eventTimeVar = "event_date",
    sortVariable = "priority",
    referenceLines = "median",
    showInterpretation = TRUE,
    personTimeAnalysis = TRUE,
    responseAnalysis = TRUE
  )
  
  expect_s3_class(result, "swimmerplotResults")
  expect_true(result$plot$visible)
  expect_true(result$summary$visible)
  expect_true(result$interpretation$visible)
  expect_true(result$personTimeTable$visible)
  expect_true(result$milestoneTable$visible)
  expect_true(result$eventMarkerTable$visible)
  expect_true(result$advancedMetrics$visible)
})

test_that("swimmerplot fallback plot works on errors", {
  skip_if_not_installed("ggswim")
  
  # Test with problematic data that might cause ggswim errors
  test_data <- data.frame(
    patient_id = paste0("PT", 1:3),
    start_time = c(0, 0, 0),
    end_time = c(12, 8, 15),
    response = c("INVALID_RESPONSE", "ANOTHER_INVALID", "THIRD_INVALID"),
    stringsAsFactors = FALSE
  )
  
  # Should not error even with invalid response values
  expect_no_error({
    result <- swimmerplot(
      data = test_data,
      patientID = "patient_id",
      startTime = "start_time",
      endTime = "end_time",
      responseVar = "response"
    )
  })
})

test_that("customReferenceDate parses in absolute datetime mode", {
  skip_if_not_installed("ggswim")
  skip_if_not_installed("lubridate")
  # Skip until codegen adds the new option to the function signature
  if (!("customReferenceDate" %in% names(formals(swimmerplot)))) {
    skip("customReferenceDate not available until options are regenerated")
  }

  # Try both ymd and ymdhms formats
  formats <- list(
    list(dateFormat = "ymd",    value = "2023-06-01"),
    list(dateFormat = "ymdhms", value = "2023-06-01 12:34:56")
  )

  for (cfg in formats) {
    test_data <- data.frame(
      patient_id = paste0("PT", 1:3),
      start_date = as.Date(c("2023-01-01", "2023-01-08", "2023-01-15")),
      end_date = as.Date(c("2023-03-01", "2023-02-15", "2023-04-01")),
      stringsAsFactors = FALSE
    )

    expect_no_error({
      res <- swimmerplot(
        data = test_data,
        patientID = "patient_id",
        startTime = "start_date",
        endTime = "end_date",
        timeType = "datetime",
        dateFormat = cfg$dateFormat,
        timeUnit = "weeks",
        timeDisplay = "absolute",
        referenceLines = "custom",
        customReferenceDate = cfg$value
      )
      expect_s3_class(res, "swimmerplotResults")
      expect_true(res$plot$visible)
      expect_true(inherits(res$plot$state$patient_data$start_time, c("Date", "POSIXct")))
    })
  }
})
