context("test-dataquality")

# Load required library
library(ClinicoPath)

test_that("dataquality basic functionality", {
  
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
    plot_data_overview = FALSE,  # Disable to avoid visdat dependency issues
    plot_missing_patterns = FALSE,
    plot_data_types = FALSE,
    plot_value_expectations = FALSE,
    missing_threshold_visual = 10
  )
  
  expect_s3_class(result, "dataqualityResults")
  expect_true("todo" %in% names(result))
  expect_true("text" %in% names(result))
  expect_true("plot" %in% names(result))
})

test_that("dataquality parameter validation", {
  
  # Load test data
  data("histopathology", package = "ClinicoPath")
  
  # Test parameter structure based on YAML configuration
  expected_params <- c("data", "vars", "check_duplicates", "check_missing",
                      "complete_cases_only", "plot_data_overview", "plot_missing_patterns",
                      "plot_data_types", "plot_value_expectations", "missing_threshold_visual")
  
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
    plot_value_expectations = FALSE,
    missing_threshold_visual = 10
  )
  
  expect_s3_class(result, "dataqualityResults")
  
  # Check that missing data analysis is performed
  # The function should generate content in the text component
  expect_true("text" %in% names(result))
})

test_that("dataquality duplicate detection", {
  
  # Load test data
  data("histopathology", package = "ClinicoPath")
  
  # Test duplicate detection specifically
  result <- dataquality(
    data = histopathology,
    vars = c("Sex", "Grade"),
    check_duplicates = TRUE,
    check_missing = FALSE,
    complete_cases_only = FALSE,
    plot_data_overview = FALSE,
    plot_missing_patterns = FALSE,
    plot_data_types = FALSE,
    plot_value_expectations = FALSE,
    missing_threshold_visual = 10
  )
  
  expect_s3_class(result, "dataqualityResults")
  
  # Check that duplicate analysis is performed
  expect_true("text" %in% names(result))
})

test_that("dataquality complete cases analysis", {
  
  # Load test data
  data("histopathology", package = "ClinicoPath")
  
  # Test complete cases analysis
  result <- dataquality(
    data = histopathology,
    vars = c("Age", "Sex", "Grade"),
    check_duplicates = TRUE,
    check_missing = TRUE,
    complete_cases_only = TRUE,
    plot_data_overview = FALSE,
    plot_missing_patterns = FALSE,
    plot_data_types = FALSE,
    plot_value_expectations = FALSE,
    missing_threshold_visual = 10
  )
  
  expect_s3_class(result, "dataqualityResults")
  
  # Check that complete cases analysis is performed
  expect_true("text" %in% names(result))
})

test_that("dataquality visdat analysis types", {
  
  # Load test data
  data("histopathology", package = "ClinicoPath")
  
  # Test different visdat types
  # Test individual plot functionality
    result <- dataquality(
      data = histopathology,
      vars = c("Age", "Sex"),
      check_duplicates = FALSE,
      check_missing = FALSE,
      complete_cases_only = FALSE,
      plot_data_overview = TRUE,
    plot_missing_patterns = TRUE,
    plot_data_types = TRUE,
    plot_value_expectations = TRUE,
      missing_threshold_visual = 10,
      )
    
    expect_s3_class(result, "dataqualityResults")
    expect_true("plot" %in% names(result))
  }
})

test_that("dataquality missing threshold validation", {
  
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
      plot_data_overview = TRUE,
    plot_missing_patterns = TRUE,
    plot_data_types = TRUE,
    plot_value_expectations = TRUE,
      missing_threshold_visual = threshold,
      )
    
    expect_s3_class(result, "dataqualityResults")
  }
})

test_that("dataquality export functionality", {
  
  # Load test data
  data("histopathology", package = "ClinicoPath")
  
  # Test export functionality
  result <- dataquality(
    data = histopathology,
    vars = c("Age", "Sex"),
    check_duplicates = FALSE,
    check_missing = FALSE,
    complete_cases_only = FALSE,
    plot_data_overview = TRUE,
    plot_missing_patterns = TRUE,
    plot_data_types = TRUE,
    plot_value_expectations = TRUE,
    visdat_type = "vis_dat",
    missing_threshold_visual = 10,
  )
  
  expect_s3_class(result, "dataqualityResults")
  
  # Check that export information is included
  expect_true("plot" %in% names(result))
})

test_that("dataquality with different variable types", {
  
  # Load test data
  data("histopathology", package = "ClinicoPath")
  
  # Test with different variable combinations
  
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
    plot_value_expectations = FALSE,
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
    plot_value_expectations = FALSE,
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
    plot_value_expectations = FALSE,
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
    cat("visdat package is available - visual analysis tests can be performed\n")
  } else {
    cat("visdat package not available - visual analysis will show informative messages\n")
  }
})

test_that("dataquality error handling", {
  
  # Load test data
  data("histopathology", package = "ClinicoPath")
  
  # Test with invalid variable names
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
    plot_value_expectations = FALSE,
      visdat_type = "vis_dat",
      missing_threshold_visual = 10,
      ),
    info = "Should error with non-existent variable"
  )
  
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
    plot_value_expectations = FALSE,
      visdat_type = "vis_dat",
      missing_threshold_visual = 10,
      ),
    info = "Should error with empty dataset"
  )
})

test_that("dataquality YAML configuration", {
  
  # Test that YAML files exist and are properly structured
  yaml_files <- c(
    "/Users/serdarbalci/Documents/GitHub/ClinicoPathJamoviModule/jamovi/dataquality.a.yaml",
    "/Users/serdarbalci/Documents/GitHub/ClinicoPathJamoviModule/jamovi/dataquality.u.yaml", 
    "/Users/serdarbalci/Documents/GitHub/ClinicoPathJamoviModule/jamovi/dataquality.r.yaml"
  )
  
  for (yaml_file in yaml_files) {
    expect_true(file.exists(yaml_file),
                info = paste("YAML file", basename(yaml_file), "should exist"))
  }
})

test_that("dataquality documentation", {
  
  # Test that documentation exists
  man_file <- "/Users/serdarbalci/Documents/GitHub/ClinicoPathJamoviModule/man/dataquality.Rd"
  expect_true(file.exists(man_file),
              info = "Manual page should exist")
  
  # Test that implementation file exists
  impl_file <- "/Users/serdarbalci/Documents/GitHub/ClinicoPathJamoviModule/R/dataquality.b.R"
  expect_true(file.exists(impl_file),
              info = "Implementation file should exist")
})

test_that("dataquality data requirements", {
  
  # Load test data
  data("histopathology", package = "ClinicoPath")
  
  # Test that required data structure exists
  expect_true(is.data.frame(histopathology))
  expect_true(nrow(histopathology) > 0)
  
  # Test that required variables for testing are available
  required_vars <- c("Age", "Sex", "Grade", "Group", "LVI", "PNI")
  for (var in required_vars) {
    expect_true(var %in% names(histopathology),
                info = paste("Variable", var, "should be in histopathology dataset"))
  }
  
  # Test that variables have appropriate characteristics for quality assessment
  expect_true(is.numeric(histopathology$Age))
  expect_true(length(unique(histopathology$Sex)) >= 2)
  expect_true(length(unique(histopathology$Grade)) >= 2)
})

# Future tests to add when advanced features are implemented:

test_that("dataquality advanced visual analysis (TODO)", {
  skip("Pending enhancement of visual analysis features")
  
  # When enhanced, these tests should work:
  # - Test all visdat types with actual plot generation
  # - Test plot export functionality
  # - Test visual analysis insights generation
  # - Test missing pattern visualization
})

test_that("dataquality performance tests (TODO)", {
  skip("Pending performance optimization features")
  
  # When enhanced, test performance with:
  # - Large datasets (1000+ rows)
  # - Wide datasets (100+ variables)
  # - Datasets with high missing data rates
  # - Memory usage monitoring
})

test_that("dataquality advanced pattern detection (TODO)", {
  skip("Pending advanced pattern detection features")
  
  # When enhanced, test:
  # - Systematic missing data pattern detection
  # - Temporal pattern analysis
  # - Outlier pattern recognition
  # - Data drift detection
})