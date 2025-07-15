# Test suite for waterfall function
# This file contains comprehensive tests for the waterfall treatment response analysis

library(testthat)
library(dplyr)
library(ClinicoPath)

# Test datasets
test_data_percentage <- data.frame(
  PatientID = paste0("PT", 1:5),
  Response = c(-60, -35, -10, 15, 45),
  stringsAsFactors = FALSE
)

test_data_raw <- data.frame(
  PatientID = rep(paste0("PT", 1:3), each = 3),
  Time = rep(c(0, 2, 4), 3),
  Measurement = c(
    50, 30, 25,    # PT1: Good response
    60, 45, 40,    # PT2: Partial response  
    55, 50, 52     # PT3: Stable disease
  ),
  stringsAsFactors = FALSE
)

test_data_edge_cases <- data.frame(
  PatientID = paste0("PT", 1:4),
  Response = c(-150, 500, -30, 20),  # Invalid shrinkage, large growth, boundaries
  stringsAsFactors = FALSE
)

test_data_single <- data.frame(
  PatientID = "PT001",
  Response = -45,
  stringsAsFactors = FALSE
)

test_data_missing_baseline <- data.frame(
  PatientID = rep(paste0("PT", 1:2), each = 2),
  Time = rep(c(2, 4), 2),  # No baseline (time = 0)
  Measurement = c(30, 25, 45, 40),
  stringsAsFactors = FALSE
)

# =================================
# Test Data Validation
# =================================

test_that("Data validation works correctly", {
  
  # Test empty data
  expect_error(
    waterfall(data.frame(), patientID = "PatientID", responseVar = "Response"),
    "No data provided"
  )
  
  # Test missing required columns
  expect_error(
    waterfall(data.frame(ID = "PT1"), patientID = "PatientID", responseVar = "Response"),
    "required columns"
  )
  
  # Test valid percentage data
  expect_silent({
    result <- waterfall(
      data = test_data_percentage,
      patientID = "PatientID", 
      responseVar = "Response",
      inputType = "percentage"
    )
  })
  
  # Test valid raw data
  expect_silent({
    result <- waterfall(
      data = test_data_raw,
      patientID = "PatientID",
      responseVar = "Measurement", 
      timeVar = "Time",
      inputType = "raw"
    )
  })
})

# =================================
# Test RECIST Categorization
# =================================

test_that("RECIST categorization works correctly", {
  
  result <- waterfall(
    data = test_data_percentage,
    patientID = "PatientID",
    responseVar = "Response", 
    inputType = "percentage"
  )
  
  # Check that summary table has correct categories
  summary_table <- result$summaryTable$asDF
  expect_true("CR" %in% summary_table$category || 
              "PR" %in% summary_table$category || 
              "SD" %in% summary_table$category || 
              "PD" %in% summary_table$category)
  
  # Check that percentages sum to 100 (allowing for rounding)
  expect_true(abs(sum(summary_table$percent) - 100) < 0.1)
})

# =================================
# Test Data Processing
# =================================

test_that("Data processing works correctly", {
  
  # Test percentage data processing
  result_pct <- waterfall(
    data = test_data_percentage,
    patientID = "PatientID",
    responseVar = "Response",
    inputType = "percentage"
  )
  
  expect_s3_class(result_pct, "waterfallResults")
  expect_true(!is.null(result_pct$summaryTable))
  expect_true(!is.null(result_pct$clinicalMetrics))
  
  # Test raw data processing
  result_raw <- waterfall(
    data = test_data_raw,
    patientID = "PatientID",
    responseVar = "Measurement",
    timeVar = "Time", 
    inputType = "raw"
  )
  
  expect_s3_class(result_raw, "waterfallResults")
  expect_true(!is.null(result_raw$summaryTable))
  expect_true(!is.null(result_raw$personTimeTable))
})

# =================================
# Test Best Response Selection
# =================================

test_that("Best response selection works correctly", {
  
  # Create test data where we know the best response
  test_best_response <- data.frame(
    PatientID = rep("PT001", 4),
    Time = c(0, 2, 4, 6),
    Measurement = c(100, 80, 60, 70)  # Best response should be 60 (-40%)
  )
  
  result <- waterfall(
    data = test_best_response,
    patientID = "PatientID",
    responseVar = "Measurement",
    timeVar = "Time",
    inputType = "raw"
  )
  
  # The best response should be -40% (60 vs 100 baseline)
  # This should be categorized as PR (Partial Response)
  summary_table <- result$summaryTable$asDF
  expect_true(any(summary_table$category == "PR" & summary_table$n > 0))
})

# =================================
# Test Edge Cases
# =================================

test_that("Edge cases are handled correctly", {
  
  # Test invalid shrinkage values (should be capped at -100%)
  result_edge <- waterfall(
    data = test_data_edge_cases,
    patientID = "PatientID",
    responseVar = "Response",
    inputType = "percentage"
  )
  
  expect_s3_class(result_edge, "waterfallResults")
  
  # Test single patient
  result_single <- waterfall(
    data = test_data_single,
    patientID = "PatientID",
    responseVar = "Response",
    inputType = "percentage"
  )
  
  expect_s3_class(result_single, "waterfallResults")
})

# =================================
# Test Raw Data Validation
# =================================

test_that("Raw data validation works correctly", {
  
  # Test missing baseline validation
  expect_error(
    waterfall(
      data = test_data_missing_baseline,
      patientID = "PatientID",
      responseVar = "Measurement",
      timeVar = "Time",
      inputType = "raw"
    ),
    "Baseline Measurements"
  )
  
  # Test missing time variable for raw data
  expect_error(
    waterfall(
      data = test_data_raw,
      patientID = "PatientID", 
      responseVar = "Measurement",
      inputType = "raw"
    ),
    "Time Variable"
  )
})

# =================================
# Test Plot Generation
# =================================

test_that("Plot generation works correctly", {
  
  # Test waterfall plot
  result <- waterfall(
    data = test_data_percentage,
    patientID = "PatientID",
    responseVar = "Response",
    inputType = "percentage",
    showWaterfallPlot = TRUE
  )
  
  expect_true(!is.null(result$waterfallplot))
  
  # Test spider plot with time data
  result_spider <- waterfall(
    data = test_data_raw,
    patientID = "PatientID",
    responseVar = "Measurement",
    timeVar = "Time",
    inputType = "raw",
    showSpiderPlot = TRUE
  )
  
  expect_true(!is.null(result_spider$spiderplot))
})

# =================================
# Test Clinical Metrics
# =================================

test_that("Clinical metrics calculation works correctly", {
  
  result <- waterfall(
    data = test_data_percentage,
    patientID = "PatientID",
    responseVar = "Response",
    inputType = "percentage"
  )
  
  clinical_metrics <- result$clinicalMetrics$asDF
  
  # Check that ORR and DCR are calculated
  expect_true(any(grepl("Objective Response Rate", clinical_metrics$metric)))
  expect_true(any(grepl("Disease Control Rate", clinical_metrics$metric)))
  
  # Check that values are valid percentages
  orr_row <- clinical_metrics[grepl("Objective Response Rate", clinical_metrics$metric), ]
  if (nrow(orr_row) > 0) {
    orr_value <- as.numeric(gsub("%", "", orr_row$value))
    expect_true(orr_value >= 0 && orr_value <= 100)
  }
})

# =================================
# Test Person-Time Analysis
# =================================

test_that("Person-time analysis works correctly", {
  
  result <- waterfall(
    data = test_data_raw,
    patientID = "PatientID",
    responseVar = "Measurement",
    timeVar = "Time",
    inputType = "raw"
  )
  
  # Check that person-time table is generated
  expect_true(!is.null(result$personTimeTable))
  
  # Check that additional clinical metrics are calculated
  clinical_metrics <- result$clinicalMetrics$asDF
  expect_true(any(grepl("Time to Response", clinical_metrics$metric)))
  expect_true(any(grepl("Duration of Response", clinical_metrics$metric)))
})

# =================================
# Test Response Category Addition
# =================================

test_that("Response category addition works correctly", {
  
  result <- waterfall(
    data = test_data_percentage,
    patientID = "PatientID",
    responseVar = "Response",
    inputType = "percentage"
  )
  
  # Check that response categories are added
  expect_true(!is.null(result$addResponseCategory))
  
  # Check that categories are valid RECIST categories
  categories <- result$addResponseCategory$asDF
  if (nrow(categories) > 0) {
    expect_true(all(categories$values %in% c("CR", "PR", "SD", "PD", "Unknown")))
  }
})

# =================================
# Test Function Parameters
# =================================

test_that("Function parameters work correctly", {
  
  # Test different sorting options
  result_sort_response <- waterfall(
    data = test_data_percentage,
    patientID = "PatientID",
    responseVar = "Response",
    inputType = "percentage",
    sortBy = "response"
  )
  
  result_sort_id <- waterfall(
    data = test_data_percentage,
    patientID = "PatientID",
    responseVar = "Response",
    inputType = "percentage",
    sortBy = "id"
  )
  
  expect_s3_class(result_sort_response, "waterfallResults")
  expect_s3_class(result_sort_id, "waterfallResults")
  
  # Test different color schemes
  result_simple <- waterfall(
    data = test_data_percentage,
    patientID = "PatientID",
    responseVar = "Response",
    inputType = "percentage",
    colorScheme = "simple"
  )
  
  result_recist <- waterfall(
    data = test_data_percentage,
    patientID = "PatientID",
    responseVar = "Response",
    inputType = "percentage",
    colorScheme = "recist"
  )
  
  expect_s3_class(result_simple, "waterfallResults")
  expect_s3_class(result_recist, "waterfallResults")
})

# =================================
# Test Error Handling
# =================================

test_that("Error handling works correctly", {
  
  # Test invalid input type
  expect_error(
    waterfall(
      data = test_data_percentage,
      patientID = "PatientID",
      responseVar = "Response",
      inputType = "invalid"
    ),
    "should be one of"
  )
  
  # Test invalid sort option
  expect_error(
    waterfall(
      data = test_data_percentage,
      patientID = "PatientID",
      responseVar = "Response",
      inputType = "percentage",
      sortBy = "invalid"
    ),
    "should be one of"
  )
})

# =================================
# Integration Tests
# =================================

test_that("Integration tests work correctly", {
  
  # Test full workflow with percentage data
  result_full <- waterfall(
    data = test_data_percentage,
    patientID = "PatientID",
    responseVar = "Response",
    inputType = "percentage",
    showWaterfallPlot = TRUE,
    showSpiderPlot = FALSE,
    showThresholds = TRUE,
    labelOutliers = TRUE,
    colorScheme = "recist"
  )
  
  expect_s3_class(result_full, "waterfallResults")
  expect_true(!is.null(result_full$summaryTable))
  expect_true(!is.null(result_full$clinicalMetrics))
  expect_true(!is.null(result_full$waterfallplot))
  
  # Test full workflow with raw data
  result_full_raw <- waterfall(
    data = test_data_raw,
    patientID = "PatientID",
    responseVar = "Measurement",
    timeVar = "Time",
    inputType = "raw",
    showWaterfallPlot = TRUE,
    showSpiderPlot = TRUE,
    showThresholds = TRUE
  )
  
  expect_s3_class(result_full_raw, "waterfallResults")
  expect_true(!is.null(result_full_raw$summaryTable))
  expect_true(!is.null(result_full_raw$clinicalMetrics))
  expect_true(!is.null(result_full_raw$personTimeTable))
  expect_true(!is.null(result_full_raw$waterfallplot))
  expect_true(!is.null(result_full_raw$spiderplot))
})