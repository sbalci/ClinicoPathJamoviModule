# Comprehensive tests for datecorrection function
# Testing actual date correction accuracy, edge cases, and clinical scenarios

library(testthat)
devtools::load_all()

context("datecorrection - Comprehensive Clinical Scenarios")

# Helper function to run datecorrection and extract results
run_correction <- function(data, vars, method = "datefixr", date_format = "auto") {
  results <- datecorrection(
    data = data,
    date_vars = vars,
    correction_method = method,
    date_format = date_format,
    day_impute = 1,
    month_impute = 7,
    handle_excel = FALSE,
    timezone = "UTC",
    show_correction_table = FALSE,
    show_quality_assessment = FALSE,
    show_format_analysis = FALSE,
    show_correction_summary = FALSE,
    show_interpretation = FALSE
  )
  return(results)
}

# Test 1: ISO date format (YYYY-MM-DD) - should parse correctly
test_that("ISO date format parses correctly", {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()
  test_data <- data.frame(
    date_col = c("2020-01-15", "2021-06-30", "2019-12-25")
  )

  results <- run_correction(test_data, "date_col")

  # Check that results object exists
  expect_s3_class(results, "datecorrectionClass")

  # Check corrected_data table exists and has data
  expect_true("corrected_data" %in% names(results$results))
  table_data <- results$results$corrected_data

  # Verify we have rows
  expect_gt(nrow(table_data$asDF()), 0)

  # Check that all dates were successfully corrected
  df <- table_data$asDF()
  expect_equal(sum(df$status == "Success"), 3)

  # Verify actual dates match expected
  expect_equal(df$corrected[1], "2020-01-15")
  expect_equal(df$corrected[2], "2021-06-30")
  expect_equal(df$corrected[3], "2019-12-25")
})

# Test 2: Mixed separators - common in clinical databases
test_that("Mixed date separators are handled", {
  test_data <- data.frame(
    mixed_dates = c("2020/01/15", "2021-06-30", "2019.12.25", "2022 03 10")
  )

  results <- run_correction(test_data, "mixed_dates")
  df <- results$results$corrected_data$asDF()

  # All should be corrected
  expect_equal(sum(df$status == "Success"), 4)

  # Check dates are in ISO format
  expect_match(df$corrected[1], "^\\d{4}-\\d{2}-\\d{2}$")
  expect_match(df$corrected[2], "^\\d{4}-\\d{2}-\\d{2}$")
})

# Test 3: Text month names
test_that("Text month names are parsed", {
  test_data <- data.frame(
    text_months = c("15 Jan 2020", "June 30, 2021", "25-Dec-2019")
  )

  results <- run_correction(test_data, "text_months")
  df <- results$results$corrected_data$asDF()

  # Should successfully parse most text dates
  success_count <- sum(df$status == "Success")
  expect_gte(success_count, 2)  # At least 2 out of 3 should work
})

# Test 4: Excel serial dates
test_that("Excel serial dates are converted", {
  test_data <- data.frame(
    excel_dates = c("43831", "44377", "43824")  # Excel serial numbers
  )

  results <- datecorrection(
    data = test_data,
    date_vars = "excel_dates",
    correction_method = "datefixr",
    handle_excel = TRUE,  # Enable Excel conversion
    show_correction_table = FALSE,
    show_quality_assessment = FALSE,
    show_format_analysis = FALSE,
    show_correction_summary = FALSE,
    show_interpretation = FALSE
  )

  df <- results$results$corrected_data$asDF()

  # Should recognize and convert Excel dates
  expect_gte(sum(df$status == "Success"), 2)
})

# Test 5: Partial dates (missing components)
test_that("Partial dates with missing components", {
  test_data <- data.frame(
    partial_dates = c("2020-01", "2021-06", "2019")  # Missing day or day+month
  )

  results <- run_correction(test_data, "partial_dates")
  df <- results$results$corrected_data$asDF()

  # datefixR should impute missing components
  # Check that some dates were corrected (may not be all depending on parser)
  expect_gte(sum(df$status == "Success"), 1)
})

# Test 6: NA and missing values
test_that("NA values are handled gracefully", {
  test_data <- data.frame(
    dates_with_na = c("2020-01-15", NA, "2021-06-30", NA)
  )

  results <- run_correction(test_data, "dates_with_na")
  df <- results$results$corrected_data$asDF()

  # Should have 4 rows
  expect_equal(nrow(df), 4)

  # NAs should be marked as failed or handled
  na_rows <- df[is.na(test_data$dates_with_na), ]
  expect_true(all(na_rows$status %in% c("Failed", "Success")))

  # Non-NA dates should succeed
  non_na_rows <- df[!is.na(test_data$dates_with_na), ]
  expect_equal(sum(non_na_rows$status == "Success"), 2)
})

# Test 7: Empty strings and whitespace
test_that("Empty strings and whitespace", {
  test_data <- data.frame(
    messy_dates = c("2020-01-15", "", "  ", "2021-06-30")
  )

  results <- run_correction(test_data, "messy_dates")
  df <- results$results$corrected_data$asDF()

  # Empty strings should fail or be marked appropriately
  expect_equal(nrow(df), 4)

  # Valid dates should succeed
  valid_indices <- c(1, 4)
  expect_equal(sum(df$status[valid_indices] == "Success"), 2)
})

# Test 8: Ambiguous dates (MM/DD vs DD/MM)
test_that("Ambiguous date formats follow specified format", {
  test_data <- data.frame(
    ambiguous_dates = c("03/04/2020", "05/06/2021", "12/01/2019")
  )

  # Test with DMY format
  results_dmy <- datecorrection(
    data = test_data,
    date_vars = "ambiguous_dates",
    correction_method = "lubridate",
    date_format = "dmy",
    show_correction_table = FALSE,
    show_quality_assessment = FALSE,
    show_format_analysis = FALSE,
    show_correction_summary = FALSE,
    show_interpretation = FALSE
  )

  # Test with MDY format
  results_mdy <- datecorrection(
    data = test_data,
    date_vars = "ambiguous_dates",
    correction_method = "lubridate",
    date_format = "mdy",
    show_correction_table = FALSE,
    show_quality_assessment = FALSE,
    show_format_analysis = FALSE,
    show_correction_summary = FALSE,
    show_interpretation = FALSE
  )

  df_dmy <- results_dmy$results$corrected_data$asDF()
  df_mdy <- results_mdy$results$corrected_data$asDF()

  # Both should succeed
  expect_gte(sum(df_dmy$status == "Success"), 2)
  expect_gte(sum(df_mdy$status == "Success"), 2)

  # Results should differ (different interpretation)
  expect_false(identical(df_dmy$corrected, df_mdy$corrected))
})

# Test 9: Invalid date detection
test_that("Invalid dates are flagged", {
  test_data <- data.frame(
    invalid_dates = c("2020-13-01", "2020-02-30", "not-a-date", "99/99/9999")
  )

  results <- run_correction(test_data, "invalid_dates")
  df <- results$results$corrected_data$asDF()

  # All should fail or be corrected with errors noted
  expect_equal(nrow(df), 4)

  # Check that failures are documented
  failed_rows <- df[df$status == "Failed", ]
  expect_gt(nrow(failed_rows), 0)

  # Check that errors column has content for failed rows
  failed_with_errors <- sum(nchar(failed_rows$errors) > 0)
  expect_gt(failed_with_errors, 0)
})

# Test 10: Multiple variables
test_that("Multiple date variables in one dataset", {
  test_data <- data.frame(
    diagnosis_date = c("2020-01-15", "2021-06-30", "2019-12-25"),
    treatment_date = c("2020-02-01", "2021-07-15", "2020-01-10")
  )

  results <- run_correction(test_data, c("diagnosis_date", "treatment_date"))
  df <- results$results$corrected_data$asDF()

  # Should have 6 rows total (3 for each variable)
  expect_equal(nrow(df), 6)

  # Check both variables are present
  expect_true("diagnosis_date" %in% df$variable)
  expect_true("treatment_date" %in% df$variable)

  # All should succeed
  expect_equal(sum(df$status == "Success"), 6)
})

# Test 11: Column name with spaces (variable escaping)
test_that("Variable names with spaces are handled", {
  test_data <- data.frame(
    `Date of Birth` = c("2020-01-15", "2021-06-30", "2019-12-25"),
    check.names = FALSE
  )

  # Should not error
  expect_error(
    run_correction(test_data, "Date of Birth"),
    NA  # No error expected
  )
})

# Test 12: Consensus method with conflicts
test_that("Consensus method reports conflicts", {
  test_data <- data.frame(
    dates = c("2020-01-15", "06/30/2021", "25-12-2019")
  )

  results <- datecorrection(
    data = test_data,
    date_vars = "dates",
    correction_method = "consensus",
    show_correction_table = FALSE,
    show_quality_assessment = TRUE,  # Enable to see method performance
    show_format_analysis = FALSE,
    show_correction_summary = FALSE,
    show_interpretation = FALSE
  )

  df <- results$results$corrected_data$asDF()

  # Consensus should work on most dates
  expect_gte(sum(df$status == "Success"), 2)

  # Check that method column shows which methods were used
  expect_true(any(nchar(df$method) > 0))
})

# Test 13: Error when variable doesn't exist
test_that("Error raised when variable not found", {
  test_data <- data.frame(
    real_var = c("2020-01-15", "2021-06-30")
  )

  # Should throw error about missing variable
  expect_error(
    run_correction(test_data, "nonexistent_var"),
    "Selected variables not found"
  )
})

# Test 14: Error when no variables selected
test_that("Error raised when no variables selected", {
  test_data <- data.frame(
    date_col = c("2020-01-15", "2021-06-30")
  )

  # Should throw error about no variables
  expect_error(
    run_correction(test_data, character(0)),
    "No date variables selected"
  )
})

# Test 15: Large dataset performance
test_that("Large dataset is handled efficiently", {
  n_rows <- 1000
  test_data <- data.frame(
    dates = sample(
      c("2020-01-15", "2021-06-30", "2019-12-25", "2022-03-10"),
      n_rows,
      replace = TRUE
    )
  )

  # Should not timeout or error
  start_time <- Sys.time()
  results <- run_correction(test_data, "dates")
  end_time <- Sys.time()

  df <- results$results$corrected_data$asDF()

  # Should process all rows
  expect_equal(nrow(df), n_rows)

  # Should be reasonably fast (< 30 seconds for 1000 rows)
  elapsed <- as.numeric(difftime(end_time, start_time, units = "secs"))
  expect_lt(elapsed, 30)
})

context("datecorrection - Output Validation")

# Test 16: Corrected data table structure
test_that("Corrected data table has required columns", {
  test_data <- data.frame(
    dates = c("2020-01-15", "2021-06-30")
  )

  results <- run_correction(test_data, "dates")
  df <- results$results$corrected_data$asDF()

  # Check all required columns exist
  required_cols <- c("row_num", "variable", "original", "corrected",
                     "status", "method", "errors")
  expect_true(all(required_cols %in% names(df)))
})

# Test 17: Row numbers are sequential
test_that("Row numbers are sequential and accurate", {
  test_data <- data.frame(
    dates = c("2020-01-15", "2021-06-30", "2019-12-25")
  )

  results <- run_correction(test_data, "dates")
  df <- results$results$corrected_data$asDF()

  # Row numbers should be 1, 2, 3
  expect_equal(df$row_num, c(1, 2, 3))
})

# Test 18: Original values preserved
test_that("Original values are preserved in output", {
  test_data <- data.frame(
    dates = c("2020-01-15", "invalid", "2021-06-30")
  )

  results <- run_correction(test_data, "dates")
  df <- results$results$corrected_data$asDF()

  # Original values should match input
  expect_equal(df$original[1], "2020-01-15")
  expect_equal(df$original[2], "invalid")
  expect_equal(df$original[3], "2021-06-30")
})

message("\n========================================")
message("Comprehensive datecorrection tests completed")
message("These tests verify:")
message("  - Actual date parsing accuracy")
message("  - Edge case handling (NA, empty, invalid)")
message("  - Multiple date formats and separators")
message("  - Excel serial date conversion")
message("  - Error detection and reporting")
message("  - Output structure and auditability")
message("  - Variable name escaping")
message("  - Multi-variable processing")
message("========================================\n")
