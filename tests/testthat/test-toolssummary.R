context("Enhanced Tools for Data Summary with summarytools Integration")

library(testthat)

# Load test datasets directly from files
clinical_data_path <- file.path("..", "..", "data", "toolssummary_clinical_demographics.rda")
lab_data_path <- file.path("..", "..", "data", "toolssummary_laboratory_results.rda")
mixed_data_path <- file.path("..", "..", "data", "toolssummary_mixed_datatypes.rda")
timeseries_data_path <- file.path("..", "..", "data", "toolssummary_timeseries_data.rda")
edge_data_path <- file.path("..", "..", "data", "toolssummary_edge_cases.rda")
small_data_path <- file.path("..", "..", "data", "toolssummary_small_sample.rda")

if (file.exists(clinical_data_path)) load(clinical_data_path)
if (file.exists(lab_data_path)) load(lab_data_path)
if (file.exists(mixed_data_path)) load(mixed_data_path)
if (file.exists(timeseries_data_path)) load(timeseries_data_path)
if (file.exists(edge_data_path)) load(edge_data_path)
if (file.exists(small_data_path)) load(small_data_path)

# =============================================================================
# Test Dataset Availability and Structure
# =============================================================================

test_that("toolssummary test datasets were created successfully", {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()
  
  # Check if main test dataset exists and has correct structure
  expect_true(exists("toolssummary_clinical_demographics"))
  expect_true(is.data.frame(toolssummary_clinical_demographics))
  expect_equal(nrow(toolssummary_clinical_demographics), 300)
  expect_equal(ncol(toolssummary_clinical_demographics), 16)
  
  # Check required columns exist
  required_cols <- c("patient_id", "age", "sex", "treatment_group", "bmi")
  expect_true(all(required_cols %in% names(toolssummary_clinical_demographics)))
  
  # Check data types are appropriate
  expect_true(is.numeric(toolssummary_clinical_demographics$age))
  expect_true(is.factor(toolssummary_clinical_demographics$sex))
  expect_true(is.factor(toolssummary_clinical_demographics$treatment_group))
  expect_true(is.numeric(toolssummary_clinical_demographics$bmi))
})

test_that("laboratory dataset has expected characteristics", {
  
  if (exists("toolssummary_laboratory_results")) {
    expect_equal(nrow(toolssummary_laboratory_results), 200)
    expect_equal(ncol(toolssummary_laboratory_results), 15)
    
    # Check for laboratory variables
    lab_vars <- c("wbc", "rbc", "sodium", "potassium", "alt")
    expect_true(all(lab_vars %in% names(toolssummary_laboratory_results)))
    
    # Check visit structure
    expect_true("visit" %in% names(toolssummary_laboratory_results))
    expect_true(is.factor(toolssummary_laboratory_results$visit))
    
    # Check longitudinal structure
    visit_levels <- levels(toolssummary_laboratory_results$visit)
    expect_true("Baseline" %in% visit_levels)
    expect_true("Week 12" %in% visit_levels)
  }
})

test_that("mixed data types dataset handles various data structures", {
  
  if (exists("toolssummary_mixed_datatypes")) {
    expect_equal(nrow(toolssummary_mixed_datatypes), 250)
    expect_equal(ncol(toolssummary_mixed_datatypes), 15)
    
    # Check for mixed data types
    expect_true(is.numeric(toolssummary_mixed_datatypes$score_continuous))
    expect_true(is.factor(toolssummary_mixed_datatypes$severity))
    expect_true(inherits(toolssummary_mixed_datatypes$assessment_date, "Date"))
    
    # Check ordered factors
    if ("severity" %in% names(toolssummary_mixed_datatypes)) {
      expect_true(is.ordered(toolssummary_mixed_datatypes$severity))
    }
    if ("priority_level" %in% names(toolssummary_mixed_datatypes)) {
      expect_true(is.ordered(toolssummary_mixed_datatypes$priority_level))
    }
  }
})

test_that("time series dataset has proper longitudinal structure", {
  
  if (exists("toolssummary_timeseries_data")) {
    expect_equal(nrow(toolssummary_timeseries_data), 200)
    
    # Should have timepoint variable
    expect_true("timepoint" %in% names(toolssummary_timeseries_data))
    expect_true(is.factor(toolssummary_timeseries_data$timepoint))
    
    # Should have 5 timepoints
    expect_equal(length(levels(toolssummary_timeseries_data$timepoint)), 5)
    
    # Should have subject_id
    expect_true("subject_id" %in% names(toolssummary_timeseries_data))
    
    # Should have 40 unique subjects
    n_subjects <- length(unique(toolssummary_timeseries_data$subject_id))
    expect_equal(n_subjects, 40)
    
    # Each subject should have multiple timepoints
    subject_counts <- table(toolssummary_timeseries_data$subject_id)
    expect_true(all(subject_counts >= 1))
    expect_true(any(subject_counts > 1))
  }
})

test_that("edge cases dataset contains challenging scenarios", {
  
  if (exists("toolssummary_edge_cases")) {
    expect_equal(nrow(toolssummary_edge_cases), 150)
    
    # Should have scenario variable
    expect_true("scenario" %in% names(toolssummary_edge_cases))
    expect_true(is.factor(toolssummary_edge_cases$scenario))
    
    # Should contain extreme values
    if ("numeric_extreme" %in% names(toolssummary_edge_cases)) {
      # Should have some very large or very small values
      expect_true(any(abs(toolssummary_edge_cases$numeric_extreme) > 100, na.rm = TRUE))
    }
    
    # Should have missing values
    expect_true(any(is.na(toolssummary_edge_cases)))
    
    # Should have constant variables
    if ("constant_numeric" %in% names(toolssummary_edge_cases)) {
      unique_values <- unique(toolssummary_edge_cases$constant_numeric)
      expect_equal(length(unique_values), 1)
      expect_equal(unique_values[1], 42)
    }
  }
})

test_that("small sample dataset is minimal but complete", {
  
  if (exists("toolssummary_small_sample")) {
    expect_equal(nrow(toolssummary_small_sample), 20)
    expect_equal(ncol(toolssummary_small_sample), 7)
    
    # Should have at least one missing value
    expect_true(any(is.na(toolssummary_small_sample)))
    
    # Basic variables should exist
    basic_vars <- c("id", "group", "value_numeric", "category_binary")
    expect_true(all(basic_vars %in% names(toolssummary_small_sample)))
  }
})

# =============================================================================
# Package Dependency Tests
# =============================================================================

test_that("required packages are available", {
  
  # Skip tests if packages not available
  skip_if_not_installed("summarytools")
  skip_if_not_installed("jmvcore")
  skip_if_not_installed("dplyr")
  
  # Test basic summarytools functionality
  simple_data <- data.frame(
    var1 = 1:10,
    var2 = letters[1:10]
  )
  
  # Basic summarytools functions should work
  expect_no_error({
    freq_result <- summarytools::freq(simple_data$var2)
  })
  
  expect_no_error({
    descr_result <- summarytools::descr(simple_data$var1)
  })
})

# =============================================================================
# Basic Functionality Tests
# =============================================================================

test_that("toolssummary function handles basic scenarios", {
  
  skip_if_not_installed("jmvcore")
  
  if (exists("toolssummary_clinical_demographics")) {
    data <- toolssummary_clinical_demographics
    
    # Test basic function call
    expect_no_error({
      result <- toolssummary(
        data = data,
        vars = c("age", "sex", "bmi"),
        useSummarytools = FALSE
      )
    })
    
    # Test with summarytools enabled
    skip_if_not_installed("summarytools")
    expect_no_error({
      result <- toolssummary(
        data = data,
        vars = c("age", "sex", "bmi"),
        useSummarytools = TRUE,
        showDfSummary = TRUE,
        showDescr = TRUE,
        showFreq = TRUE
      )
    })
  }
})

test_that("toolssummary handles different data types correctly", {
  
  skip_if_not_installed("jmvcore")
  
  if (exists("toolssummary_mixed_datatypes")) {
    data <- toolssummary_mixed_datatypes
    
    # Test with mixed data types
    expect_no_error({
      result <- toolssummary(
        data = data,
        vars = c("score_continuous", "severity", "binary_flag"),
        useSummarytools = FALSE
      )
    })
    
    # Test numeric variables
    numeric_vars <- c("score_continuous", "measurement_value")
    expect_no_error({
      result <- toolssummary(
        data = data,
        vars = numeric_vars,
        showStats = TRUE
      )
    })
    
    # Test categorical variables
    cat_vars <- c("severity", "priority_level", "binary_flag")
    expect_no_error({
      result <- toolssummary(
        data = data,
        vars = cat_vars,
        showFreq = TRUE
      )
    })
  }
})

test_that("toolssummary handles missing data appropriately", {
  
  skip_if_not_installed("jmvcore")
  
  if (exists("toolssummary_clinical_demographics")) {
    data <- toolssummary_clinical_demographics
    
    # Test with missing data included
    expect_no_error({
      result <- toolssummary(
        data = data,
        vars = c("age", "bmi", "cholesterol"),
        excludeNA = FALSE
      )
    })
    
    # Test with missing data excluded
    expect_no_error({
      result <- toolssummary(
        data = data,
        vars = c("age", "bmi", "cholesterol"),
        excludeNA = TRUE
      )
    })
  }
})

# =============================================================================
# summarytools Integration Tests
# =============================================================================

test_that("summarytools dfSummary integration works", {
  
  skip_if_not_installed("summarytools")
  skip_if_not_installed("jmvcore")
  
  if (exists("toolssummary_clinical_demographics")) {
    data <- toolssummary_clinical_demographics
    
    # Test dfSummary functionality
    expect_no_error({
      result <- toolssummary(
        data = data,
        vars = c("age", "sex", "bmi"),
        useSummarytools = TRUE,
        showDfSummary = TRUE
      )
    })
    
    # Check that dfSummary result exists
    expect_true("dfSummary" %in% names(result))
  }
})

test_that("summarytools descr integration works", {
  
  skip_if_not_installed("summarytools")
  skip_if_not_installed("jmvcore")
  
  if (exists("toolssummary_clinical_demographics")) {
    data <- toolssummary_clinical_demographics
    
    # Test descr functionality with numeric variables
    expect_no_error({
      result <- toolssummary(
        data = data,
        vars = c("age", "bmi", "systolic_bp"),
        useSummarytools = TRUE,
        showDescr = TRUE
      )
    })
    
    # Check that descr result exists
    expect_true("descrStats" %in% names(result))
  }
})

test_that("summarytools freq integration works", {
  
  skip_if_not_installed("summarytools")
  skip_if_not_installed("jmvcore")
  
  if (exists("toolssummary_clinical_demographics")) {
    data <- toolssummary_clinical_demographics
    
    # Test freq functionality with categorical variables
    expect_no_error({
      result <- toolssummary(
        data = data,
        vars = c("sex", "treatment_group", "diabetes"),
        useSummarytools = TRUE,
        showFreq = TRUE
      )
    })
    
    # Check that freq result exists
    expect_true("summaryToolsFreq" %in% names(result))
  }
})

test_that("summarytools ctable integration works", {
  
  skip_if_not_installed("summarytools")
  skip_if_not_installed("jmvcore")
  
  if (exists("toolssummary_clinical_demographics")) {
    data <- toolssummary_clinical_demographics
    
    # Test ctable functionality with grouping variable
    expect_no_error({
      result <- toolssummary(
        data = data,
        vars = c("sex", "diabetes", "smoking_status"),
        groupVar = "treatment_group",
        useSummarytools = TRUE,
        showCrosstabs = TRUE
      )
    })
    
    # Check that crosstabs result exists
    expect_true("crosstabs" %in% names(result))
  }
})

# =============================================================================
# Edge Cases and Robustness Tests
# =============================================================================

test_that("toolssummary handles edge cases gracefully", {
  
  skip_if_not_installed("jmvcore")
  
  if (exists("toolssummary_edge_cases")) {
    data <- toolssummary_edge_cases
    
    # Test with extreme values
    expect_no_error({
      result <- toolssummary(
        data = data,
        vars = c("numeric_extreme", "categorical_many"),
        useSummarytools = FALSE
      )
    })
    
    # Test with constant variables
    expect_no_error({
      result <- toolssummary(
        data = data,
        vars = c("constant_numeric", "constant_factor"),
        useSummarytools = FALSE
      )
    })
  }
})

test_that("toolssummary works with small sample sizes", {
  
  skip_if_not_installed("jmvcore")
  
  if (exists("toolssummary_small_sample")) {
    data <- toolssummary_small_sample
    
    # Test with very small dataset
    expect_no_error({
      result <- toolssummary(
        data = data,
        vars = c("value_numeric", "category_binary"),
        useSummarytools = FALSE
      )
    })
    
    # Test with summarytools on small data
    skip_if_not_installed("summarytools")
    expect_no_error({
      result <- toolssummary(
        data = data,
        vars = c("value_numeric", "group"),
        useSummarytools = TRUE,
        showDfSummary = TRUE
      )
    })
  }
})

test_that("toolssummary handles empty variable selection", {
  
  skip_if_not_installed("jmvcore")
  
  if (exists("toolssummary_clinical_demographics")) {
    data <- toolssummary_clinical_demographics
    
    # Test with no variables selected (should show welcome message)
    expect_no_error({
      result <- toolssummary(
        data = data,
        vars = character(0),
        useSummarytools = FALSE
      )
    })
    
    # Check that todo message exists
    expect_true("todo" %in% names(result))
  }
})

# =============================================================================
# Longitudinal Data Tests
# =============================================================================

test_that("toolssummary handles longitudinal data correctly", {
  
  skip_if_not_installed("jmvcore")
  
  if (exists("toolssummary_timeseries_data")) {
    data <- toolssummary_timeseries_data
    
    # Test basic longitudinal analysis
    expect_no_error({
      result <- toolssummary(
        data = data,
        vars = c("primary_outcome", "response_status"),
        useSummarytools = FALSE
      )
    })
    
    # Test grouped by timepoint
    expect_no_error({
      result <- toolssummary(
        data = data,
        vars = c("primary_outcome", "secondary_outcome_1"),
        groupVar = "timepoint",
        useSummarytools = FALSE
      )
    })
  }
})

# =============================================================================
# Data Quality and Structure Tests
# =============================================================================

test_that("clinical demographics data has realistic ranges", {
  
  if (exists("toolssummary_clinical_demographics")) {
    data <- toolssummary_clinical_demographics
    
    # Age should be reasonable
    expect_true(all(data$age >= 18 & data$age <= 85, na.rm = TRUE))
    
    # BMI should be in realistic range
    expect_true(all(data$bmi >= 15 & data$bmi <= 50, na.rm = TRUE))
    
    # Blood pressure should be realistic
    expect_true(all(data$systolic_bp >= 90 & data$systolic_bp <= 200, na.rm = TRUE))
    expect_true(all(data$diastolic_bp >= 60 & data$diastolic_bp <= 120, na.rm = TRUE))
    
    # Treatment groups should be balanced
    group_counts <- table(data$treatment_group)
    expect_true(all(group_counts >= 80))  # Each group should have reasonable size
  }
})

test_that("laboratory data has clinical ranges", {
  
  if (exists("toolssummary_laboratory_results")) {
    data <- toolssummary_laboratory_results
    
    # White blood cell count should be in normal range
    if ("wbc" %in% names(data)) {
      expect_true(all(data$wbc >= 2.0 & data$wbc <= 15.0, na.rm = TRUE))
    }
    
    # Sodium should be in physiological range
    if ("sodium" %in% names(data)) {
      expect_true(all(data$sodium >= 130 & data$sodium <= 150, na.rm = TRUE))
    }
    
    # Potassium should be in safe range
    if ("potassium" %in% names(data)) {
      expect_true(all(data$potassium >= 3.0 & data$potassium <= 5.5, na.rm = TRUE))
    }
  }
})

test_that("missing data patterns are appropriate", {
  
  if (exists("toolssummary_clinical_demographics")) {
    data <- toolssummary_clinical_demographics
    
    # Should have some missing values but not too many
    missing_pct_bmi <- mean(is.na(data$bmi))
    expect_true(missing_pct_bmi >= 0.01 && missing_pct_bmi <= 0.15)  # 1-15% missing
    
    # Should have some missing cholesterol values
    if ("cholesterol" %in% names(data)) {
      missing_chol <- mean(is.na(data$cholesterol))
      expect_true(missing_chol >= 0.01 && missing_chol <= 0.15)
    }
  }
  
  if (exists("toolssummary_edge_cases")) {
    # Edge cases should have systematic missing patterns
    expect_true(any(is.na(toolssummary_edge_cases)))
  }
})

# =============================================================================
# Performance and Integration Tests
# =============================================================================

test_that("toolssummary performance with large datasets", {
  
  skip_if_not_installed("jmvcore")
  
  if (exists("toolssummary_clinical_demographics")) {
    data <- toolssummary_clinical_demographics
    
    # Test performance with many variables
    all_vars <- names(data)[2:8]  # Skip ID column and limit variables
    
    start_time <- Sys.time()
    expect_no_error({
      result <- toolssummary(
        data = data,
        vars = all_vars,
        useSummarytools = FALSE
      )
    })
    end_time <- Sys.time()
    
    # Should complete within reasonable time (10 seconds)
    expect_true(as.numeric(end_time - start_time, units = "secs") < 10)
  }
})

test_that("dataset summary files were created", {
  
  # Check summary files exist
  summary_path <- file.path("..", "..", "data", "toolssummary_datasets_summary.rda")
  scenarios_path <- file.path("..", "..", "data", "toolssummary_test_scenarios.rda")
  
  expect_true(file.exists(summary_path))
  expect_true(file.exists(scenarios_path))
  
  # Load and check summary
  if (file.exists(summary_path)) {
    load(summary_path)
    expect_true(exists("summary_stats"))
    expect_true(is.data.frame(summary_stats))
    expect_equal(nrow(summary_stats), 6)  # 6 datasets
    
    # Check required columns
    summary_cols <- c("Dataset", "Observations", "Variables", "Description")
    expect_true(all(summary_cols %in% names(summary_stats)))
  }
  
  # Check scenarios file
  if (file.exists(scenarios_path)) {
    load(scenarios_path)
    expect_true(exists("test_scenarios"))
    expect_true(is.data.frame(test_scenarios))
    expect_true(nrow(test_scenarios) >= 10)  # Multiple test scenarios
  }
})

# =============================================================================
# Clean up
# =============================================================================

# Clean up any objects created during testing
rm(list = ls(pattern = "^(clinical_data_path|lab_data_path|.*_path)$"))
