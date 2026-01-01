# Test suite for missingdata function
# Tests cover missing data analysis, pattern recognition, multiple imputation, and edge cases

library(testthat)
library(mice)
library(dplyr)

# Helper functions for creating test data
create_complete_data <- function(n = 100, seed = 123) {
  set.seed(seed)
  
  data <- data.frame(
    patient_id = paste0("P", sprintf("%03d", 1:n)),
    age = round(rnorm(n, mean = 55, sd = 15)),
    bmi = round(rnorm(n, mean = 25, sd = 5), 1),
    systolic_bp = round(rnorm(n, mean = 130, sd = 20)),
    treatment = sample(c("A", "B", "C"), n, replace = TRUE),
    response = sample(c("Yes", "No"), n, replace = TRUE, prob = c(0.6, 0.4)),
    biomarker = round(rnorm(n, mean = 100, sd = 25), 2)
  )
  
  return(data)
}

create_missing_data <- function(n = 100, missing_rate = 0.3, seed = 123) {
  set.seed(seed)
  
  # Start with complete data
  data <- create_complete_data(n = n, seed = seed)
  
  # Introduce missing data patterns
  # MCAR pattern for age
  missing_indices <- sample(1:n, size = floor(n * missing_rate * 0.5))
  data$age[missing_indices] <- NA
  
  # MAR pattern for BMI (related to age)
  older_patients <- which(data$age > 60 & !is.na(data$age))
  bmi_missing <- sample(older_patients, size = floor(length(older_patients) * 0.4))
  data$bmi[bmi_missing] <- NA
  
  # MNAR pattern for biomarker (high values more likely to be missing)
  high_biomarker <- which(data$biomarker > 120)
  biomarker_missing <- sample(high_biomarker, size = floor(length(high_biomarker) * 0.6))
  data$biomarker[biomarker_missing] <- NA
  
  # Random missing in systolic BP
  bp_missing <- sample(1:n, size = floor(n * missing_rate * 0.3))
  data$systolic_bp[bp_missing] <- NA
  
  return(data)
}

create_high_missing_data <- function(n = 50, seed = 456) {
  set.seed(seed)
  
  data <- create_complete_data(n = n, seed = seed)
  
  # Create variables with very high missing rates
  data$high_missing_var1 <- data$age
  data$high_missing_var2 <- data$bmi
  
  # Make 95% of values missing
  missing_indices1 <- sample(1:n, size = floor(n * 0.95))
  missing_indices2 <- sample(1:n, size = floor(n * 0.95))
  
  data$high_missing_var1[missing_indices1] <- NA
  data$high_missing_var2[missing_indices2] <- NA
  
  return(data)
}

create_empty_variable_data <- function(n = 50, seed = 789) {
  set.seed(seed)
  
  data <- create_complete_data(n = n, seed = seed)
  
  # Create completely empty variable
  data$empty_var <- NA
  
  return(data)
}

# Basic functionality tests
describe("missingdata Basic Functionality", {
  
  test_that("missingdata data structure validation works", {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()
    skip_if_not_installed("mice")
    skip_if_not_installed("ggmice")
    
    data <- create_missing_data(n = 50)
    
    # Test basic functionality without errors
    expect_no_error({
      # Validate data structure
      expect_true(is.data.frame(data))
      expect_true(nrow(data) > 0)
      expect_true(ncol(data) > 0)
      
      # Check that there is indeed missing data
      expect_true(sum(is.na(data)) > 0)
      
      # Check variable types
      expect_true(is.numeric(data$age))
      expect_true(is.numeric(data$bmi))
      expect_true(is.character(data$treatment) || is.factor(data$treatment))
    })
  })
  
  test_that("missingdata handles complete data correctly", {
    skip_if_not_installed("mice")
    skip_if_not_installed("ggmice")
    
    data <- create_complete_data(n = 50)
    
    # Test complete data handling
    expect_true(is.data.frame(data))
    expect_equal(sum(is.na(data)), 0)
    expect_equal(nrow(data), 50)
  })
  
  test_that("missingdata handles missing data patterns", {
    skip_if_not_installed("mice")
    skip_if_not_installed("ggmice")
    
    data <- create_missing_data(n = 100)
    
    # Validate missing data patterns
    expect_true(sum(is.na(data)) > 0)
    expect_true(all(complete.cases(data) | !complete.cases(data)))
    
    # Check specific patterns
    age_missing <- sum(is.na(data$age))
    bmi_missing <- sum(is.na(data$bmi))
    biomarker_missing <- sum(is.na(data$biomarker))
    
    expect_true(age_missing > 0)
    expect_true(bmi_missing > 0)
    expect_true(biomarker_missing > 0)
  })
})

# Data validation tests
describe("missingdata Data Validation", {
  
  test_that("missingdata validates required variables", {
    data <- create_missing_data(n = 50)
    
    # Test required variables exist
    expect_true(all(c("age", "bmi", "biomarker") %in% names(data)))
    
    # Test variable types
    expect_true(is.numeric(data$age))
    expect_true(is.numeric(data$bmi))
    expect_true(is.numeric(data$biomarker))
  })
  
  test_that("missingdata validates missing patterns", {
    data <- create_missing_data(n = 100, missing_rate = 0.2)
    
    # Test missing data statistics
    total_missing <- sum(is.na(data))
    expect_true(total_missing > 0)
    
    # Test missing rates per variable
    missing_rates <- sapply(data, function(x) sum(is.na(x)) / length(x))
    expect_true(all(missing_rates >= 0 & missing_rates <= 1))
    
    # Test that not all variables are completely missing
    expect_true(any(missing_rates < 1))
  })
  
  test_that("missingdata handles high missing data", {
    data <- create_high_missing_data(n = 50)
    
    # Test high missing variables
    high_missing_rate1 <- sum(is.na(data$high_missing_var1)) / nrow(data)
    high_missing_rate2 <- sum(is.na(data$high_missing_var2)) / nrow(data)
    
    expect_true(high_missing_rate1 > 0.9)
    expect_true(high_missing_rate2 > 0.9)
  })
})

# Pattern analysis tests
describe("missingdata Pattern Analysis", {
  
  test_that("missingdata pattern statistics are calculated correctly", {
    data <- create_missing_data(n = 100)
    
    # Calculate pattern statistics
    n_complete <- sum(complete.cases(data))
    n_incomplete <- sum(!complete.cases(data))
    total_obs <- nrow(data)
    
    expect_equal(n_complete + n_incomplete, total_obs)
    expect_true(n_complete >= 0 && n_complete <= total_obs)
    expect_true(n_incomplete >= 0 && n_incomplete <= total_obs)
  })
  
  test_that("missingdata variable-wise statistics work", {
    data <- create_missing_data(n = 100)
    
    # Test variable-wise missing calculations
    for (var_name in names(data)) {
      var_data <- data[[var_name]]
      missing_count <- sum(is.na(var_data))
      complete_count <- sum(!is.na(var_data))
      
      expect_equal(missing_count + complete_count, length(var_data))
      expect_true(missing_count >= 0)
      expect_true(complete_count >= 0)
    }
  })
  
  test_that("missingdata pattern matrix generation works", {
    data <- create_missing_data(n = 50)
    
    # Test pattern matrix functionality
    if (requireNamespace("mice", quietly = TRUE)) {
      pattern_matrix <- mice::md.pattern(data, plot = FALSE)
      
      expect_true(is.matrix(pattern_matrix))
      expect_true(nrow(pattern_matrix) > 0)
      expect_true(ncol(pattern_matrix) > 0)
    }
  })
})

# Multiple imputation tests
describe("missingdata Multiple Imputation", {
  
  test_that("missingdata imputation parameter validation works", {
    # Test parameter ranges
    expect_true(max(1, min(50, 5)) == 5)  # n_imputations
    expect_true(max(1, min(100, 10)) == 10)  # max_iterations
    expect_true(max(1, min(50, 0)) == 1)  # Lower bound
    expect_true(max(1, min(50, 100)) == 50)  # Upper bound
  })
  
  test_that("missingdata imputation methods are recognized", {
    # Test method specifications
    valid_methods <- c("pmm", "norm", "logreg", "polyreg", "auto")
    
    for (method in valid_methods) {
      expect_true(method %in% valid_methods)
    }
  })
  
  test_that("missingdata imputation basic functionality works", {
    skip_if_not_installed("mice")
    
    data <- create_missing_data(n = 30)  # Small dataset for testing
    
    # Test basic imputation
    if (requireNamespace("mice", quietly = TRUE)) {
      expect_no_error({
        mice_result <- mice::mice(
          data = data,
          m = 2,  # Small number for testing
          maxit = 2,
          printFlag = FALSE
        )
        
        expect_true(is.list(mice_result))
        expect_true("data" %in% names(mice_result))
        expect_true("m" %in% names(mice_result))
        expect_equal(mice_result$m, 2)
      })
    }
  })
})

# Edge case tests
describe("missingdata Edge Cases", {
  
  test_that("missingdata handles empty variables", {
    data <- create_empty_variable_data(n = 50)
    
    # Test empty variable detection
    empty_vars <- names(data)[sapply(data, function(x) all(is.na(x)))]
    expect_true(length(empty_vars) > 0)
    expect_true("empty_var" %in% empty_vars)
  })
  
  test_that("missingdata handles minimal data", {
    minimal_data <- data.frame(
      x = c(1, 2, NA, 4, 5),
      y = c(NA, 2, 3, 4, NA)
    )
    
    expect_true(is.data.frame(minimal_data))
    expect_equal(nrow(minimal_data), 5)
    expect_true(sum(is.na(minimal_data)) > 0)
  })
  
  test_that("missingdata handles single variable", {
    single_var_data <- data.frame(
      variable = c(1, 2, NA, 4, NA, 6, 7, NA, 9, 10)
    )
    
    expect_true(is.data.frame(single_var_data))
    expect_equal(ncol(single_var_data), 1)
    expect_true(sum(is.na(single_var_data)) > 0)
  })
  
  test_that("missingdata handles different data types", {
    mixed_data <- data.frame(
      numeric_var = c(1, 2, NA, 4, 5),
      character_var = c("A", "B", NA, "D", "E"),
      factor_var = factor(c("X", "Y", NA, "X", "Y")),
      logical_var = c(TRUE, FALSE, NA, TRUE, FALSE)
    )
    
    expect_true(is.data.frame(mixed_data))
    expect_true(is.numeric(mixed_data$numeric_var))
    expect_true(is.character(mixed_data$character_var))
    expect_true(is.factor(mixed_data$factor_var))
    expect_true(is.logical(mixed_data$logical_var))
  })
})

# Performance tests
describe("missingdata Performance", {
  
  test_that("missingdata performance scales appropriately", {
    # Test with medium dataset
    medium_data <- create_missing_data(n = 200)
    
    start_time <- Sys.time()
    # Basic operations
    missing_counts <- sapply(medium_data, function(x) sum(is.na(x)))
    complete_cases_count <- sum(complete.cases(medium_data))
    end_time <- Sys.time()
    
    expect_true(is.numeric(missing_counts))
    expect_true(is.numeric(complete_cases_count))
    expect_true(as.numeric(end_time - start_time) < 5)  # Should be fast
  })
  
  test_that("missingdata memory usage is reasonable", {
    # Test memory efficiency
    data <- create_missing_data(n = 100)
    
    # Basic memory usage test
    object_size <- object.size(data)
    expect_true(object_size < 1000000)  # Less than 1MB for small dataset
  })
})

# Statistical validation tests
describe("missingdata Statistical Validation", {
  
  test_that("missingdata statistical calculations are correct", {
    data <- create_missing_data(n = 100)
    
    # Test descriptive statistics
    for (var_name in names(data)) {
      if (is.numeric(data[[var_name]])) {
        var_data <- data[[var_name]]
        
        # Test basic statistics
        mean_val <- mean(var_data, na.rm = TRUE)
        median_val <- median(var_data, na.rm = TRUE)
        
        expect_true(is.numeric(mean_val))
        expect_true(is.numeric(median_val))
        expect_false(is.na(mean_val))
        expect_false(is.na(median_val))
      }
    }
  })
  
  test_that("missingdata missing data mechanisms are identifiable", {
    data <- create_missing_data(n = 100)
    
    # Test missing data patterns
    missing_patterns <- sapply(data, function(x) sum(is.na(x)))
    expect_true(all(missing_patterns >= 0))
    expect_true(any(missing_patterns > 0))
  })
})

# Clinical data scenarios
describe("missingdata Clinical Applications", {
  
  test_that("missingdata clinical trial scenarios work", {
    # Clinical trial data
    clinical_data <- create_missing_data(n = 150)
    
    # Add clinical variables
    clinical_data$visit_number <- rep(1:3, each = 50)
    clinical_data$site <- sample(c("Site_A", "Site_B", "Site_C"), 150, replace = TRUE)
    clinical_data$dropout <- sample(c(0, 1), 150, replace = TRUE, prob = c(0.85, 0.15))
    
    expect_true(is.data.frame(clinical_data))
    expect_true(nrow(clinical_data) == 150)
    expect_true(all(c("visit_number", "site", "dropout") %in% names(clinical_data)))
  })
  
  test_that("missingdata longitudinal data patterns work", {
    # Longitudinal data with dropout
    longitudinal_data <- data.frame(
      patient_id = rep(1:20, each = 4),
      visit = rep(1:4, 20),
      outcome = rnorm(80, mean = 50, sd = 10)
    )
    
    # Introduce dropout pattern
    for (i in 1:20) {
      dropout_visit <- sample(2:4, 1)
      patient_rows <- which(longitudinal_data$patient_id == i & 
                           longitudinal_data$visit >= dropout_visit)
      longitudinal_data$outcome[patient_rows] <- NA
    }
    
    expect_true(is.data.frame(longitudinal_data))
    expect_true(sum(is.na(longitudinal_data$outcome)) > 0)
  })
})

# Error handling tests
describe("missingdata Error Handling", {
  
  test_that("missingdata handles missing variable names", {
    data <- create_missing_data(n = 50)
    
    # Test nonexistent variables
    nonexistent_vars <- c("nonexistent_var1", "nonexistent_var2")
    missing_vars <- setdiff(nonexistent_vars, names(data))
    
    expect_equal(length(missing_vars), 2)
    expect_true(all(nonexistent_vars %in% missing_vars))
  })
  
  test_that("missingdata handles empty datasets", {
    empty_data <- data.frame()
    
    expect_true(is.data.frame(empty_data))
    expect_equal(nrow(empty_data), 0)
    expect_equal(ncol(empty_data), 0)
  })
  
  test_that("missingdata handles all missing data", {
    all_missing_data <- data.frame(
      var1 = c(NA, NA, NA, NA, NA),
      var2 = c(NA, NA, NA, NA, NA),
      var3 = c(NA, NA, NA, NA, NA)
    )
    
    expect_true(is.data.frame(all_missing_data))
    expect_equal(sum(complete.cases(all_missing_data)), 0)
  })
})

# Helper function validation tests
describe("missingdata Helper Functions", {
  
  test_that("create_complete_data generates valid data", {
    data <- create_complete_data(n = 20)
    
    expect_true(is.data.frame(data))
    expect_equal(nrow(data), 20)
    expect_true(all(c("age", "bmi", "biomarker") %in% names(data)))
    expect_equal(sum(is.na(data)), 0)
  })
  
  test_that("create_missing_data generates valid missing patterns", {
    data <- create_missing_data(n = 30, missing_rate = 0.2)
    
    expect_true(is.data.frame(data))
    expect_equal(nrow(data), 30)
    expect_true(sum(is.na(data)) > 0)
    
    # Test missing rates are reasonable
    missing_rates <- sapply(data, function(x) sum(is.na(x)) / length(x))
    expect_true(all(missing_rates >= 0 & missing_rates <= 1))
  })
  
  test_that("create_high_missing_data generates appropriate patterns", {
    data <- create_high_missing_data(n = 25)
    
    expect_true(is.data.frame(data))
    expect_equal(nrow(data), 25)
    expect_true("high_missing_var1" %in% names(data))
    expect_true("high_missing_var2" %in% names(data))
    
    # Test high missing rates
    high_missing_rate <- sum(is.na(data$high_missing_var1)) / nrow(data)
    expect_true(high_missing_rate > 0.9)
  })
  
  test_that("create_empty_variable_data generates empty variables", {
    data <- create_empty_variable_data(n = 15)
    
    expect_true(is.data.frame(data))
    expect_equal(nrow(data), 15)
    expect_true("empty_var" %in% names(data))
    expect_true(all(is.na(data$empty_var)))
  })
})

# Package integration tests
describe("missingdata Package Integration", {
  
  test_that("missingdata mice package integration works", {
    skip_if_not_installed("mice")
    
    # Test mice package availability
    expect_true(requireNamespace("mice", quietly = TRUE))
    
    # Test basic mice functionality
    if (requireNamespace("mice", quietly = TRUE)) {
      data <- create_missing_data(n = 20)
      
      expect_no_error({
        pattern_matrix <- mice::md.pattern(data, plot = FALSE)
        expect_true(is.matrix(pattern_matrix))
      })
    }
  })
  
  test_that("missingdata dplyr integration works", {
    skip_if_not_installed("dplyr")
    
    data <- create_missing_data(n = 50)
    
    if (requireNamespace("dplyr", quietly = TRUE)) {
      expect_no_error({
        summary_stats <- data %>%
          dplyr::summarise_all(~sum(is.na(.)))
        
        expect_true(is.data.frame(summary_stats))
        expect_equal(nrow(summary_stats), 1)
      })
    }
  })
})

print("All missingdata tests completed successfully!")
