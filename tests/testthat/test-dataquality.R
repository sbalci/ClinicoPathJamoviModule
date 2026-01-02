# Test file for dataquality function
library(testthat)
devtools::load_all()

test_that("dataquality basic functionality", {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()
  # Load test data
  data("histopathology", package = "ClinicoPath")

  # Test that function exists and has correct class structure
  expect_true(exists("dataqualityClass"))
  expect_true(is.function(dataquality))

  # Basic execution test
  result <- dataquality(
    data = histopathology,
    vars = c("Age", "Sex"),
    check_duplicates = TRUE,
    check_missing = TRUE,
    complete_cases_only = FALSE,
    plot_data_overview = FALSE,
    plot_missing_patterns = FALSE,
    plot_data_types = FALSE,
    missing_threshold_visual = 10
  )

  expect_s3_class(result, "dataqualityResults")
  expect_true("todo" %in% names(result))
  expect_true("text" %in% names(result))
})

test_that("dataquality parameter validation", {
  # Load test data
  data("histopathology", package = "ClinicoPath")

  # Test parameter structure based on YAML configuration (without vis_expect)
  expected_params <- c("data", "vars", "check_duplicates", "check_missing",
                      "complete_cases_only", "plot_data_overview", "plot_missing_patterns",
                      "plot_data_types", "missing_threshold_visual")

  # Get function arguments
  func_args <- names(formals(dataquality))

  # Check that all expected parameters are present
  for (param in expected_params) {
    expect_true(param %in% func_args,
                info = paste("Parameter", param, "should be present in function signature"))
  }
})

test_that("dataquality missing data analysis", {
  # Load test data
  data("histopathology", package = "ClinicoPath")

  # Test missing data analysis specifically
  result <- dataquality(
    data = histopathology,
    vars = c("Age", "Sex", "Grade"),
    check_duplicates = FALSE,
    check_missing = TRUE,
    complete_cases_only = FALSE,
    plot_data_overview = FALSE,
    plot_missing_patterns = FALSE,
    plot_data_types = FALSE,
    missing_threshold_visual = 10
  )

  expect_s3_class(result, "dataqualityResults")

  # Check that missing data analysis is performed
  expect_true("text" %in% names(result))
})

test_that("dataquality duplicate detection - value level", {
  # Load test data
  data("histopathology", package = "ClinicoPath")

  # Test duplicate detection at value level (complete_cases_only = FALSE)
  result <- dataquality(
    data = histopathology,
    vars = c("Sex", "Grade"),
    check_duplicates = TRUE,
    check_missing = FALSE,
    complete_cases_only = FALSE,  # Check duplicates within each variable
    plot_data_overview = FALSE,
    plot_missing_patterns = FALSE,
    plot_data_types = FALSE,
    missing_threshold_visual = 10
  )

  expect_s3_class(result, "dataqualityResults")
  expect_true("text" %in% names(result))
})

test_that("dataquality duplicate detection - row level", {
  # Load test data
  data("histopathology", package = "ClinicoPath")

  # Test duplicate detection at row level (complete_cases_only = TRUE)
  result <- dataquality(
    data = histopathology,
    vars = c("Age", "Sex", "Grade"),
    check_duplicates = TRUE,
    check_missing = FALSE,
    complete_cases_only = TRUE,  # Check for duplicate rows
    plot_data_overview = FALSE,
    plot_missing_patterns = FALSE,
    plot_data_types = FALSE,
    missing_threshold_visual = 10
  )

  expect_s3_class(result, "dataqualityResults")
  expect_true("text" %in% names(result))
})

test_that("dataquality completeness analysis", {
  # Load test data
  data("histopathology", package = "ClinicoPath")

  # Completeness should always be calculated when 2+ variables selected
  result <- dataquality(
    data = histopathology,
    vars = c("Age", "Sex", "Grade"),
    check_duplicates = FALSE,
    check_missing = FALSE,
    complete_cases_only = FALSE,
    plot_data_overview = FALSE,
    plot_missing_patterns = FALSE,
    plot_data_types = FALSE,
    missing_threshold_visual = 10
  )

  expect_s3_class(result, "dataqualityResults")
  expect_true("text" %in% names(result))
})

test_that("dataquality visual analysis plots", {
  skip_if_not_installed("visdat")

  # Load test data
  data("histopathology", package = "ClinicoPath")

  # Test with individual plot options
  result <- dataquality(
    data = histopathology,
    vars = c("Age", "Sex"),
    check_duplicates = FALSE,
    check_missing = FALSE,
    complete_cases_only = FALSE,
    plot_data_overview = TRUE,
    plot_missing_patterns = TRUE,
    plot_data_types = TRUE,
    missing_threshold_visual = 15
  )

  expect_s3_class(result, "dataqualityResults")
  expect_true("plotDataOverview" %in% names(result))
  expect_true("plotMissingPatterns" %in% names(result))
  expect_true("plotDataTypes" %in% names(result))
})

test_that("dataquality missing threshold effects", {
  skip_if_not_installed("visdat")

  # Load test data
  data("histopathology", package = "ClinicoPath")

  # Test different missing thresholds
  thresholds <- c(5, 10, 20, 50)

  for (threshold in thresholds) {
    result <- dataquality(
      data = histopathology,
      vars = c("Age", "Sex"),
      check_duplicates = FALSE,
      check_missing = TRUE,
      complete_cases_only = FALSE,
      plot_data_overview = FALSE,
      plot_missing_patterns = TRUE,  # This plot uses threshold
      plot_data_types = FALSE,
      missing_threshold_visual = threshold
    )

    expect_s3_class(result, "dataqualityResults")
  }
})

test_that("dataquality with different variable types", {
  # Load test data
  data("histopathology", package = "ClinicoPath")

  # Continuous variables only
  result_cont <- dataquality(
    data = histopathology,
    vars = c("Age", "OverallTime"),
    check_duplicates = TRUE,
    check_missing = TRUE,
    complete_cases_only = FALSE,
    plot_data_overview = FALSE,
    plot_missing_patterns = FALSE,
    plot_data_types = FALSE,
    missing_threshold_visual = 10
  )

  expect_s3_class(result_cont, "dataqualityResults")

  # Categorical variables only
  result_cat <- dataquality(
    data = histopathology,
    vars = c("Sex", "Grade"),
    check_duplicates = TRUE,
    check_missing = TRUE,
    complete_cases_only = FALSE,
    plot_data_overview = FALSE,
    plot_missing_patterns = FALSE,
    plot_data_types = FALSE,
    missing_threshold_visual = 10
  )

  expect_s3_class(result_cat, "dataqualityResults")

  # Mixed variables
  result_mixed <- dataquality(
    data = histopathology,
    vars = c("Age", "Sex", "Grade"),
    check_duplicates = TRUE,
    check_missing = TRUE,
    complete_cases_only = FALSE,
    plot_data_overview = FALSE,
    plot_missing_patterns = FALSE,
    plot_data_types = FALSE,
    missing_threshold_visual = 10
  )

  expect_s3_class(result_mixed, "dataqualityResults")
})

test_that("dataquality dependencies", {
  # Test that required dependencies are available
  expect_true(requireNamespace("dplyr", quietly = TRUE),
              info = "dplyr package should be available")
  expect_true(requireNamespace("magrittr", quietly = TRUE),
              info = "magrittr package should be available")

  # visdat is optional - test availability but don't require it
  visdat_available <- requireNamespace("visdat", quietly = TRUE)

  if (visdat_available) {
    message("visdat package is available - visual analysis tests can be performed")
  } else {
    message("visdat package not available - visual analysis will show informative messages")
  }
})

test_that("dataquality error handling - missing columns", {
  # Load test data
  data("histopathology", package = "ClinicoPath")

  # Test with invalid variable names - should now throw error
  expect_error(
    dataquality(
      data = histopathology,
      vars = "NonExistentVariable",
      check_duplicates = TRUE,
      check_missing = TRUE,
      complete_cases_only = FALSE,
      plot_data_overview = FALSE,
      plot_missing_patterns = FALSE,
      plot_data_types = FALSE,
      missing_threshold_visual = 10
    ),
    regexp = "do not exist in the dataset",
    info = "Should error with non-existent variable"
  )
})

test_that("dataquality error handling - empty data", {
  # Load test data
  data("histopathology", package = "ClinicoPath")

  # Test with empty data
  empty_data <- histopathology[0, ]
  expect_error(
    dataquality(
      data = empty_data,
      vars = c("Age", "Sex"),
      check_duplicates = TRUE,
      check_missing = TRUE,
      complete_cases_only = FALSE,
      plot_data_overview = FALSE,
      plot_missing_patterns = FALSE,
      plot_data_types = FALSE,
      missing_threshold_visual = 10
    ),
    regexp = "no rows",
    info = "Should error with empty dataset"
  )
})

test_that("dataquality welcome message when no variables", {
  # Load test data
  data("histopathology", package = "ClinicoPath")

  # Test with no variables selected - should show welcome message
  result <- dataquality(
    data = histopathology,
    vars = c(),  # No variables
    check_duplicates = FALSE,
    check_missing = FALSE,
    complete_cases_only = FALSE,
    plot_data_overview = FALSE,
    plot_missing_patterns = FALSE,
    plot_data_types = FALSE,
    missing_threshold_visual = 10
  )

  expect_s3_class(result, "dataqualityResults")
  expect_true("todo" %in% names(result))
})

test_that("dataquality variable name escaping", {
  # Load test data
  data("histopathology", package = "ClinicoPath")

  # Create test data with spaces in column names
  test_data <- histopathology
  names(test_data)[1:2] <- c("Patient Age", "Patient Sex")

  # Should handle variable names with spaces via composeTerm
  result <- dataquality(
    data = test_data,
    vars = c("Patient Age", "Patient Sex"),
    check_duplicates = TRUE,
    check_missing = TRUE,
    complete_cases_only = FALSE,
    plot_data_overview = FALSE,
    plot_missing_patterns = FALSE,
    plot_data_types = FALSE,
    missing_threshold_visual = 10
  )

  expect_s3_class(result, "dataqualityResults")
})
