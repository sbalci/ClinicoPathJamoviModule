context("Modern Table Formatting with TinyTable")

library(testthat)

# Load test datasets directly from files
clinical_data_path <- file.path("..", "..", "data", "tinytable_clinical_demographics.rda")
lab_data_path <- file.path("..", "..", "data", "tinytable_laboratory_results.rda")
multimodal_data_path <- file.path("..", "..", "data", "tinytable_multimodal_summary.rda")
small_data_path <- file.path("..", "..", "data", "tinytable_small_sample.rda")
edge_data_path <- file.path("..", "..", "data", "tinytable_edge_cases.rda")

if (file.exists(clinical_data_path)) load(clinical_data_path)
if (file.exists(lab_data_path)) load(lab_data_path)
if (file.exists(multimodal_data_path)) load(multimodal_data_path)
if (file.exists(small_data_path)) load(small_data_path)
if (file.exists(edge_data_path)) load(edge_data_path)

# =============================================================================
# Basic Functionality Tests
# =============================================================================

test_that("tinytable test datasets were created successfully", {
  
  # Check if main test dataset exists and has correct structure
  expect_true(exists("tinytable_clinical_demographics"))
  expect_true(is.data.frame(tinytable_clinical_demographics))
  expect_equal(nrow(tinytable_clinical_demographics), 250)
  expect_equal(ncol(tinytable_clinical_demographics), 14)
  
  # Check required columns exist
  required_cols <- c("age", "sex", "treatment_group", "bmi")
  expect_true(all(required_cols %in% names(tinytable_clinical_demographics)))
  
  # Check data types are appropriate
  expect_true(is.numeric(tinytable_clinical_demographics$age))
  expect_true(is.factor(tinytable_clinical_demographics$sex))
  expect_true(is.factor(tinytable_clinical_demographics$treatment_group))
  expect_true(is.numeric(tinytable_clinical_demographics$bmi))
})

test_that("laboratory dataset has expected characteristics", {
  
  if (exists("tinytable_laboratory_results")) {
    expect_equal(nrow(tinytable_laboratory_results), 180)
    expect_equal(ncol(tinytable_laboratory_results), 15)
    
    # Check for laboratory variables
    lab_vars <- c("wbc", "rbc", "sodium", "potassium", "alt")
    expect_true(all(lab_vars %in% names(tinytable_laboratory_results)))
    
    # Check visit structure
    expect_true("visit" %in% names(tinytable_laboratory_results))
    expect_true(is.factor(tinytable_laboratory_results$visit))
  }
})

test_that("multimodal dataset handles mixed data types", {
  
  if (exists("tinytable_multimodal_summary")) {
    expect_equal(nrow(tinytable_multimodal_summary), 200)
    
    # Check for mixed data types
    expect_true(is.numeric(tinytable_multimodal_summary$score_1))
    expect_true(is.factor(tinytable_multimodal_summary$severity))
    expect_true(inherits(tinytable_multimodal_summary$start_date, "Date"))
    
    # Check ordered factors
    if ("severity" %in% names(tinytable_multimodal_summary)) {
      expect_true(is.ordered(tinytable_multimodal_summary$severity))
    }
  }
})

test_that("small sample dataset is minimal but complete", {
  
  if (exists("tinytable_small_sample")) {
    expect_equal(nrow(tinytable_small_sample), 15)
    expect_equal(ncol(tinytable_small_sample), 6)
    
    # Should have at least one missing value
    expect_true(any(is.na(tinytable_small_sample)))
    
    # Basic variables should exist
    basic_vars <- c("id", "group", "value_1", "value_2")
    expect_true(all(basic_vars %in% names(tinytable_small_sample)))
  }
})

test_that("edge cases dataset contains challenging scenarios", {
  
  if (exists("tinytable_edge_cases")) {
    expect_equal(nrow(tinytable_edge_cases), 100)
    
    # Should have scenario variable
    expect_true("scenario" %in% names(tinytable_edge_cases))
    expect_true(is.factor(tinytable_edge_cases$scenario))
    
    # Should contain some extreme values
    if ("numeric_var_1" %in% names(tinytable_edge_cases)) {
      # Should have some very large or very small values
      expect_true(any(tinytable_edge_cases$numeric_var_1 > 100, na.rm = TRUE) || 
                  any(tinytable_edge_cases$numeric_var_1 < 0.01, na.rm = TRUE))
    }
    
    # Should have missing values
    expect_true(any(is.na(tinytable_edge_cases)))
  }
})

# =============================================================================
# Package Dependency Tests
# =============================================================================

test_that("required packages are available", {
  
  # Skip tests if tinytable not available
  skip_if_not_installed("tinytable")
  skip_if_not_installed("dplyr")
  skip_if_not_installed("stringr")
  
  # Test basic tinytable functionality
  simple_data <- data.frame(
    var1 = 1:5,
    var2 = letters[1:5]
  )
  
  # Basic tinytable creation should work
  expect_no_error({
    tt_obj <- tinytable::tt(simple_data)
  })
})

# =============================================================================
# Data Quality Tests
# =============================================================================

test_that("clinical demographics data has realistic ranges", {
  
  if (exists("tinytable_clinical_demographics")) {
    data <- tinytable_clinical_demographics
    
    # Age should be reasonable
    expect_true(all(data$age >= 18 & data$age <= 90, na.rm = TRUE))
    
    # BMI should be in realistic range
    expect_true(all(data$bmi >= 15 & data$bmi <= 50, na.rm = TRUE))
    
    # Blood pressure should be realistic
    expect_true(all(data$systolic_bp >= 80 & data$systolic_bp <= 220, na.rm = TRUE))
    expect_true(all(data$diastolic_bp >= 50 & data$diastolic_bp <= 130, na.rm = TRUE))
    
    # Treatment groups should be balanced
    group_counts <- table(data$treatment_group)
    expect_true(all(group_counts >= 50))  # Each group should have reasonable size
  }
})

test_that("laboratory data has clinical ranges", {
  
  if (exists("tinytable_laboratory_results")) {
    data <- tinytable_laboratory_results
    
    # White blood cell count should be in normal range
    if ("wbc" %in% names(data)) {
      expect_true(all(data$wbc >= 1 & data$wbc <= 20, na.rm = TRUE))
    }
    
    # Sodium should be in physiological range
    if ("sodium" %in% names(data)) {
      expect_true(all(data$sodium >= 125 & data$sodium <= 155, na.rm = TRUE))
    }
    
    # Potassium should be in safe range
    if ("potassium" %in% names(data)) {
      expect_true(all(data$potassium >= 2.5 & data$potassium <= 6.0, na.rm = TRUE))
    }
  }
})

# =============================================================================
# Missing Data Pattern Tests
# =============================================================================

test_that("missing data patterns are appropriate", {
  
  if (exists("tinytable_clinical_demographics")) {
    data <- tinytable_clinical_demographics
    
    # Should have some missing values but not too many
    missing_pct <- mean(is.na(data$bmi))
    expect_true(missing_pct >= 0.01 && missing_pct <= 0.10)  # 1-10% missing
    
    # Should have some missing cholesterol values
    if ("cholesterol" %in% names(data)) {
      missing_chol <- mean(is.na(data$cholesterol))
      expect_true(missing_chol >= 0.01 && missing_chol <= 0.10)
    }
  }
  
  if (exists("tinytable_edge_cases")) {
    # Edge cases should have systematic missing patterns
    expect_true(any(is.na(tinytable_edge_cases)))
    
    # Should have complete missingness for some scenarios
    scenario_6_indices <- which(tinytable_edge_cases$scenario == 6)
    if (length(scenario_6_indices) > 0) {
      expect_true(all(is.na(tinytable_edge_cases$numeric_var_1[scenario_6_indices])))
    }
  }
})

# =============================================================================
# Data Summary and Structure Tests
# =============================================================================

test_that("dataset summary files were created", {
  
  # Check summary files exist
  summary_path <- file.path("..", "..", "data", "tinytable_datasets_summary.rda")
  scenarios_path <- file.path("..", "..", "data", "tinytable_test_scenarios.rda")
  
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
# Data Type Validation Tests
# =============================================================================

test_that("factor variables have appropriate levels", {
  
  if (exists("tinytable_clinical_demographics")) {
    data <- tinytable_clinical_demographics
    
    # Sex should have exactly 2 levels
    expect_equal(length(levels(data$sex)), 2)
    expect_true(all(levels(data$sex) %in% c("Male", "Female")))
    
    # Treatment group should have 3 levels
    expect_equal(length(levels(data$treatment_group)), 3)
    expect_true("Control" %in% levels(data$treatment_group))
    
    # Diabetes should have appropriate levels
    if ("diabetes" %in% names(data)) {
      diabetes_levels <- levels(data$diabetes)
      expect_true("No" %in% diabetes_levels)
      expect_true(any(c("Type 1", "Type 2") %in% diabetes_levels))
    }
  }
})

test_that("numeric variables have reasonable distributions", {
  
  if (exists("tinytable_clinical_demographics")) {
    data <- tinytable_clinical_demographics
    
    # Age should have reasonable mean and variance
    age_mean <- mean(data$age, na.rm = TRUE)
    expect_true(age_mean >= 50 && age_mean <= 80)
    
    # BMI should have realistic distribution
    if ("bmi" %in% names(data)) {
      bmi_mean <- mean(data$bmi, na.rm = TRUE)
      expect_true(bmi_mean >= 20 && bmi_mean <= 35)
    }
  }
  
  if (exists("tinytable_multimodal_summary")) {
    data <- tinytable_multimodal_summary
    
    # Score_1 should be 0-100 scale
    if ("score_1" %in% names(data)) {
      expect_true(all(data$score_1 >= 0 & data$score_1 <= 100, na.rm = TRUE))
    }
    
    # Score_2 should be 1-5 scale
    if ("score_2" %in% names(data)) {
      expect_true(all(data$score_2 >= 1 & data$score_2 <= 5, na.rm = TRUE))
    }
  }
})

# =============================================================================
# Longitudinal Data Structure Tests
# =============================================================================

test_that("timeseries data has proper longitudinal structure", {
  
  # Load timeseries data if available
  timeseries_path <- file.path("..", "..", "data", "tinytable_timeseries_summary.rda")
  if (file.exists(timeseries_path)) {
    load(timeseries_path)
    
    if (exists("tinytable_timeseries_summary")) {
      data <- tinytable_timeseries_summary
      
      # Should have 150 observations (30 subjects × 5 timepoints)
      expect_equal(nrow(data), 150)
      
      # Should have timepoint variable
      expect_true("timepoint" %in% names(data))
      expect_true(is.factor(data$timepoint))
      
      # Should have 5 timepoints
      expect_equal(length(levels(data$timepoint)), 5)
      
      # Should have subject_id
      expect_true("subject_id" %in% names(data))
      
      # Should have 30 unique subjects
      n_subjects <- length(unique(data$subject_id))
      expect_equal(n_subjects, 30)
      
      # Each subject should have multiple timepoints
      subject_counts <- table(data$subject_id)
      expect_true(all(subject_counts >= 1))
      expect_true(any(subject_counts > 1))  # Some subjects should have multiple visits
    }
  }
})

# =============================================================================
# Special Characters and Edge Cases Tests
# =============================================================================

test_that("edge cases dataset contains special challenges", {
  
  if (exists("tinytable_edge_cases")) {
    data <- tinytable_edge_cases
    
    # Should have special characters variable
    if ("special_characters" %in% names(data)) {
      special_levels <- levels(data$special_characters)
      
      # Should contain some challenging character patterns
      has_special <- any(grepl("[@#$]", special_levels)) ||
                     any(grepl("é", special_levels)) ||
                     any(grepl(" ", special_levels))
      expect_true(has_special)
    }
    
    # Should have constant variables for edge testing
    if ("constant_numeric" %in% names(data)) {
      # All values should be the same (42)
      expect_true(all(data$constant_numeric == 42, na.rm = TRUE))
    }
    
    # Should have many categories variable
    if ("many_categories" %in% names(data)) {
      n_categories <- length(levels(data$many_categories))
      expect_true(n_categories >= 20)  # Should have many levels
    }
  }
})

# =============================================================================
# Data Integrity Tests
# =============================================================================

test_that("datasets maintain referential integrity", {
  
  if (exists("tinytable_clinical_demographics")) {
    data <- tinytable_clinical_demographics
    
    # Patient IDs should be unique
    expect_equal(length(unique(data$patient_id)), nrow(data))
    
    # No unexpected factor levels
    expect_true(all(data$sex %in% c("Male", "Female")))
    
    # Numeric ranges should be consistent
    if ("systolic_bp" %in% names(data) && "diastolic_bp" %in% names(data)) {
      # Systolic should generally be higher than diastolic
      valid_bp <- data$systolic_bp >= data$diastolic_bp
      expect_true(mean(valid_bp, na.rm = TRUE) > 0.8)  # Most should be valid
    }
  }
})

test_that("all datasets can be loaded without errors", {
  
  # Test loading all datasets
  dataset_files <- c(
    "tinytable_clinical_demographics.rda",
    "tinytable_laboratory_results.rda", 
    "tinytable_multimodal_summary.rda",
    "tinytable_timeseries_summary.rda",
    "tinytable_edge_cases.rda",
    "tinytable_small_sample.rda"
  )
  
  for (file in dataset_files) {
    file_path <- file.path("..", "..", "data", file)
    if (file.exists(file_path)) {
      expect_no_error(load(file_path))
    }
  }
  
  # CSV files should also exist
  csv_files <- sub("\\.rda$", ".csv", dataset_files)
  for (file in csv_files) {
    csv_path <- file.path("..", "..", "data", file)
    if (file.exists(csv_path)) {
      expect_no_error(read.csv(csv_path))
    }
  }
})

# =============================================================================
# Performance and Size Tests
# =============================================================================

test_that("datasets are appropriately sized for testing", {
  
  # Total observations across all datasets should be reasonable
  total_obs <- 0
  
  if (exists("tinytable_clinical_demographics")) {
    total_obs <- total_obs + nrow(tinytable_clinical_demographics)
  }
  if (exists("tinytable_laboratory_results")) {
    total_obs <- total_obs + nrow(tinytable_laboratory_results)
  }
  if (exists("tinytable_multimodal_summary")) {
    total_obs <- total_obs + nrow(tinytable_multimodal_summary)
  }
  
  # Should have reasonable total size for testing (not too large, not too small)
  expect_true(total_obs >= 500 && total_obs <= 2000)
})

# =============================================================================
# Clean up
# =============================================================================

# Clean up any objects created during testing
rm(list = ls(pattern = "^(clinical_data_path|lab_data_path|.*_path)$"))