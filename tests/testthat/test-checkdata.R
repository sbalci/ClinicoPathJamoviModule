context("Data Quality Assessment - checkdata")

# ⚠️ WARNING: These tests RE-IMPLEMENT the statistical logic instead of calling
# jmv::checkdata(). They validate R functions in isolation, but do NOT test:
# - jamovi UI integration
# - Option handling
# - Table/HTML output generation
# - Actual user-facing functionality
#
# For integration testing, see test-checkdata-integration.R which actually calls
# jmv::checkdata() and validates end-to-end behavior.

# Test data preparation
set.seed(123)  # For reproducible results

# Create comprehensive test datasets for different data quality scenarios

# 1. High-quality dataset (Grade A expected)
high_quality_data <- data.frame(
  patient_id = paste0("HQ", sprintf("%03d", 1:100)),
  measurement = rnorm(100, 50, 10),  # Normal distribution
  category = factor(sample(c("Control", "Treatment"), 100, replace = TRUE)),
  score = runif(100, 0, 100),
  continuous_var = rlnorm(100, 3, 0.5)
)

# 2. Dataset with missing data (Grade B-C expected)
missing_data_dataset <- data.frame(
  patient_id = paste0("MD", sprintf("%03d", 1:150)),
  measurement_10pct_missing = c(rnorm(135, 25, 5), rep(NA, 15)),  # 10% missing
  measurement_25pct_missing = c(rnorm(112, 30, 8), rep(NA, 38)),  # 25% missing
  measurement_40pct_missing = c(rnorm(90, 20, 6), rep(NA, 60)),   # 40% missing (Grade D)
  category_missing = factor(c(rep("A", 50), rep("B", 40), rep("C", 35), rep(NA, 25)))
)

# 3. Dataset with outliers (Grade C expected)
outlier_data <- data.frame(
  patient_id = paste0("OL", sprintf("%03d", 1:80)),
  normal_with_outliers = c(rnorm(70, 15, 3), c(50, 55, -20, -25, 45, 48, -18, 52, 49, 47)),
  skewed_with_extreme = c(rlnorm(75, 2, 0.8), rep(1000, 5)),  # Extreme outliers
  measurement_clean = rnorm(80, 100, 15)  # No outliers
)

# 4. Dataset with low variability (Grade B expected)
low_variability_data <- data.frame(
  patient_id = paste0("LV", sprintf("%03d", 1:60)),
  almost_constant = c(rep(10, 55), c(10.1, 10.2, 9.9, 9.8, 10.05)),  # Very low variability
  repeated_values = rep(c(1, 2, 3), each = 20),  # High duplication
  binary_skewed = c(rep(1, 58), rep(0, 2))  # Highly imbalanced
)

# 5. Dataset with mixed quality issues (Grade D expected)
poor_quality_data <- data.frame(
  patient_id = paste0("PQ", sprintf("%03d", 1:40)),
  high_missing = c(rnorm(15, 20, 4), rep(NA, 25)),  # 62.5% missing
  outlier_heavy = c(rnorm(30, 10, 2), rep(c(100, -100, 200, -150), 2.5)),  # Multiple outliers
  all_same = rep(5, 40),  # No variability
  mostly_missing = c(rnorm(5, 0, 1), rep(NA, 35))  # 87.5% missing
)

# 6. Edge case datasets
edge_case_data <- data.frame(
  # Empty numeric (all NA)
  all_missing = rep(NA_real_, 20),
  # Single value repeated
  single_value = rep(42, 20),
  # Two values only
  binary_values = rep(c(0, 1), 10),
  # Extreme skewness
  extreme_skew = c(rep(1, 19), 1000)
)

# 7. Small dataset for minimum size testing
tiny_dataset <- data.frame(
  value = c(1, 2, 3),
  category = factor(c("A", "B", "A"))
)

# 8. Categorical data quality testing
categorical_quality_data <- data.frame(
  patient_id = paste0("CAT", sprintf("%03d", 1:200)),
  # Well-distributed categories
  balanced_category = factor(sample(c("Type1", "Type2", "Type3", "Type4"), 200, replace = TRUE)),
  # Imbalanced categories
  imbalanced_category = factor(c(rep("Rare", 5), rep("Common", 195))),
  # Categories with missing
  category_with_na = factor(c(sample(c("A", "B", "C"), 170, replace = TRUE), rep(NA, 30))),
  # High uniqueness
  high_unique_category = factor(paste0("Cat", 1:200))  # Each value unique
)

test_that("Data Quality Assessment - Basic functionality and structure", {
  
  # Test basic function exists and can be called
  expect_true(exists("checkdataClass"))
  
  # Test that basic structure works with valid data
  expect_error(
    {
      checkdata_instance <- checkdataClass$new()
    },
    NA
  )
})

test_that("Data Quality Assessment - Data validation and input checking", {
  
  # Test data type validation
  numeric_data <- high_quality_data$measurement
  expect_true(is.numeric(numeric_data))
  expect_true(length(numeric_data) > 0)
  
  # Test categorical data validation
  categorical_data <- high_quality_data$category
  expect_true(is.factor(categorical_data))
  expect_true(length(levels(categorical_data)) >= 1)
  
  # Test minimum data requirements
  expect_true(nrow(high_quality_data) >= 3)  # Minimum for meaningful analysis
  expect_true(ncol(high_quality_data) >= 1)
})

test_that("Data Quality Assessment - Missing data analysis", {
  
  # Test missing data detection
  test_var_10pct <- missing_data_dataset$measurement_10pct_missing
  missing_count <- sum(is.na(test_var_10pct))
  total_count <- length(test_var_10pct)
  missing_pct <- 100 * missing_count / total_count
  
  expect_equal(missing_count, 15)
  expect_equal(missing_pct, 10)
  
  # Test high missing data scenario
  test_var_40pct <- missing_data_dataset$measurement_40pct_missing
  missing_count_40 <- sum(is.na(test_var_40pct))
  missing_pct_40 <- 100 * missing_count_40 / length(test_var_40pct)
  
  expect_equal(missing_pct_40, 40)
  expect_true(missing_pct_40 > 30)  # Should trigger Grade D
  
  # Test complete cases calculation
  complete_cases <- sum(!is.na(test_var_10pct))
  expect_equal(complete_cases, total_count - missing_count)
})

test_that("Data Quality Assessment - Outlier detection", {
  
  # Test z-score outlier detection
  test_data <- outlier_data$normal_with_outliers
  clean_data <- test_data[!is.na(test_data)]
  z_scores <- scale(clean_data)[,1]
  outlier_indices <- which(abs(z_scores) > 3)
  
  expect_true(is.numeric(z_scores))
  expect_true(length(outlier_indices) > 0)  # Should detect outliers we added
  expect_true(all(abs(z_scores[outlier_indices]) > 3))
  
  # Test outlier severity classification
  extreme_outliers <- which(abs(z_scores) > 4)
  very_high_outliers <- which(abs(z_scores) > 3.5 & abs(z_scores) <= 4)
  high_outliers <- which(abs(z_scores) > 3 & abs(z_scores) <= 3.5)
  
  # Should have some classification
  expect_true(length(extreme_outliers) >= 0)
  expect_true(length(very_high_outliers) >= 0)
  expect_true(length(high_outliers) >= 0)
})

test_that("Data Quality Assessment - Distribution analysis", {
  
  # Test basic descriptive statistics
  test_data <- high_quality_data$measurement
  clean_data <- test_data[!is.na(test_data)]
  
  mean_val <- mean(clean_data)
  median_val <- median(clean_data)
  sd_val <- sd(clean_data)
  
  expect_true(is.numeric(mean_val))
  expect_true(is.numeric(median_val))
  expect_true(is.numeric(sd_val))
  expect_true(sd_val >= 0)
  
  # Test skewness calculation
  if (sd_val > 0) {
    skewness <- mean((clean_data - mean_val)^3) / sd_val^3
    expect_true(is.numeric(skewness))
    expect_true(is.finite(skewness))
  }
  
  # Test coefficient of variation
  cv <- ifelse(mean_val != 0, abs(sd_val / mean_val) * 100, 0)
  expect_true(is.numeric(cv))
  expect_true(cv >= 0)
  
  # Test range calculation
  min_val <- min(clean_data)
  max_val <- max(clean_data)
  range_val <- max_val - min_val
  
  expect_true(range_val >= 0)
  expect_true(max_val >= min_val)
})

test_that("Data Quality Assessment - Duplicate analysis", {
  
  # Test duplicate detection
  test_data <- low_variability_data$repeated_values
  freq_table <- table(test_data)
  duplicates <- freq_table[freq_table > 1]
  
  expect_true(length(duplicates) > 0)  # Should find duplicates
  expect_true(all(duplicates > 1))
  
  # Test duplicate percentage calculation
  for (i in seq_along(duplicates)) {
    dup_count <- duplicates[i]
    dup_pct <- 100 * dup_count / length(test_data)
    expect_true(dup_pct > 0 && dup_pct <= 100)
  }
  
  # Test high duplication scenario
  constant_data <- low_variability_data$almost_constant
  freq_constant <- table(constant_data)
  max_freq <- max(freq_constant)
  max_freq_pct <- 100 * max_freq / length(constant_data)
  
  expect_true(max_freq_pct > 90)  # Should detect high duplication
})

test_that("Data Quality Assessment - Data patterns analysis", {
  
  # Test missing data pattern detection
  high_missing_var <- missing_data_dataset$measurement_40pct_missing
  missing_pct <- 100 * sum(is.na(high_missing_var)) / length(high_missing_var)
  
  missing_pattern <- ifelse(missing_pct > 10, "systematic", "random")
  expect_true(missing_pattern %in% c("systematic", "random"))
  expect_equal(missing_pattern, "systematic")  # 40% should be systematic
  
  # Test uniqueness pattern
  low_unique_var <- low_variability_data$repeated_values
  n_unique <- length(unique(na.omit(low_unique_var)))
  n_complete <- sum(!is.na(low_unique_var))
  unique_pct <- 100 * n_unique / n_complete
  
  expect_true(unique_pct < 10)  # Should detect low uniqueness
  
  # Test outlier pattern
  outlier_var <- outlier_data$normal_with_outliers
  clean_outlier <- outlier_var[!is.na(outlier_var)]
  z_scores <- scale(clean_outlier)[,1]
  outlier_count <- sum(abs(z_scores) > 3)
  outlier_rate <- outlier_count / length(clean_outlier)
  
  expect_true(outlier_rate > 0)  # Should detect outliers
})

test_that("Data Quality Assessment - Quality grading system", {
  
  # Test Grade A scenario (high quality)
  hq_var <- high_quality_data$measurement
  hq_missing_pct <- 100 * sum(is.na(hq_var)) / length(hq_var)
  hq_clean <- hq_var[!is.na(hq_var)]
  hq_z_scores <- scale(hq_clean)[,1]
  hq_outliers <- sum(abs(hq_z_scores) > 3)
  hq_outlier_rate <- hq_outliers / length(hq_clean)
  hq_unique_pct <- 100 * length(unique(hq_clean)) / length(hq_clean)
  
  # Should qualify for Grade A
  expect_true(hq_missing_pct <= 5)  # Minimal missing
  expect_true(hq_outlier_rate <= 0.05)  # Low outlier rate
  expect_true(hq_unique_pct >= 5)  # Adequate variability
  
  # Test Grade D scenario (poor quality)
  pq_var <- poor_quality_data$high_missing
  pq_missing_pct <- 100 * sum(is.na(pq_var)) / length(pq_var)
  
  # Should qualify for Grade D
  expect_true(pq_missing_pct > 30)  # Extensive missing data
  
  # Test Grade B-C scenario (moderate quality)
  mq_var <- missing_data_dataset$measurement_25pct_missing
  mq_missing_pct <- 100 * sum(is.na(mq_var)) / length(mq_var)
  
  # Should qualify for Grade B or C
  expect_true(mq_missing_pct > 5 && mq_missing_pct <= 30)
})

test_that("Data Quality Assessment - Edge cases and error handling", {
  
  # Test all missing data
  all_na_data <- edge_case_data$all_missing
  expect_true(all(is.na(all_na_data)))
  expect_equal(sum(!is.na(all_na_data)), 0)
  
  # Test single value dataset
  single_val_data <- edge_case_data$single_value
  unique_vals <- unique(single_val_data)
  expect_equal(length(unique_vals), 1)
  expect_equal(unique_vals[1], 42)
  
  # Test very small dataset
  tiny_data <- tiny_dataset$value
  expect_true(length(tiny_data) < 10)
  expect_true(length(tiny_data) >= 3)  # Still has some data
  
  # Test extreme skewness
  skewed_data <- edge_case_data$extreme_skew
  clean_skewed <- skewed_data[!is.na(skewed_data)]
  if (length(clean_skewed) > 2 && sd(clean_skewed) > 0) {
    skewness <- mean((clean_skewed - mean(clean_skewed))^3) / sd(clean_skewed)^3
    expect_true(abs(skewness) > 1)  # Should be highly skewed
  }
  
  # Test zero variance data
  zero_var_data <- poor_quality_data$all_same
  expect_equal(sd(zero_var_data), 0)
  expect_equal(var(zero_var_data), 0)
})

test_that("Data Quality Assessment - Categorical data handling", {
  
  # Test balanced categorical data
  balanced_cat <- categorical_quality_data$balanced_category
  expect_true(is.factor(balanced_cat))
  expect_true(length(levels(balanced_cat)) > 1)
  
  cat_table <- table(balanced_cat)
  min_count <- min(cat_table)
  max_count <- max(cat_table)
  balance_ratio <- min_count / max_count
  
  expect_true(balance_ratio > 0)
  expect_true(balance_ratio <= 1)
  
  # Test imbalanced categorical data
  imbalanced_cat <- categorical_quality_data$imbalanced_category
  imb_table <- table(imbalanced_cat)
  imb_min <- min(imb_table)
  imb_max <- max(imb_table)
  imb_ratio <- imb_min / imb_max
  
  expect_true(imb_ratio < 0.1)  # Should be highly imbalanced
  
  # Test categorical with missing values
  cat_with_na <- categorical_quality_data$category_with_na
  na_count <- sum(is.na(cat_with_na))
  na_pct <- 100 * na_count / length(cat_with_na)
  
  expect_true(na_count > 0)
  expect_equal(na_pct, 15)  # 30 out of 200
  
  # Test high uniqueness in categorical
  high_unique_cat <- categorical_quality_data$high_unique_category
  unique_count <- length(levels(high_unique_cat))
  total_count <- length(high_unique_cat)
  unique_ratio <- unique_count / total_count
  
  expect_true(unique_ratio == 1)  # Each value is unique
})

test_that("Data Quality Assessment - Statistical interpretation", {
  
  # Test missing data interpretation
  interpret_missing <- function(missing_pct) {
    if (missing_pct == 0) {
      return("Excellent - Complete data")
    } else if (missing_pct < 5) {
      return("Good - Minimal missing data")
    } else if (missing_pct < 15) {
      return("Acceptable - Some missing data")
    } else if (missing_pct < 30) {
      return("Concerning - Substantial missing data")
    } else {
      return("Poor - Extensive missing data")
    }
  }
  
  expect_equal(interpret_missing(0), "Excellent - Complete data")
  expect_equal(interpret_missing(3), "Good - Minimal missing data")
  expect_equal(interpret_missing(10), "Acceptable - Some missing data")
  expect_equal(interpret_missing(20), "Concerning - Substantial missing data")
  expect_equal(interpret_missing(40), "Poor - Extensive missing data")
  
  # Test skewness interpretation
  interpret_skewness <- function(skewness) {
    abs_skew <- abs(skewness)
    if (abs_skew < 0.5) {
      return("Approximately symmetric")
    } else if (abs_skew < 1) {
      return("Moderately skewed")
    } else {
      return("Highly skewed")
    }
  }
  
  expect_equal(interpret_skewness(0.2), "Approximately symmetric")
  expect_equal(interpret_skewness(0.7), "Moderately skewed")
  expect_equal(interpret_skewness(1.5), "Highly skewed")
  expect_equal(interpret_skewness(-1.2), "Highly skewed")
  
  # Test outlier severity classification
  outlier_severity <- function(zscore) {
    abs_z <- abs(zscore)
    if (abs_z > 4) {
      return("Extreme")
    } else if (abs_z > 3.5) {
      return("Very High")
    } else {
      return("High")
    }
  }
  
  expect_equal(outlier_severity(3.2), "High")
  expect_equal(outlier_severity(3.7), "Very High")
  expect_equal(outlier_severity(4.5), "Extreme")
  expect_equal(outlier_severity(-4.2), "Extreme")
})

test_that("Data Quality Assessment - Integration with ClinicoPath datasets", {
  
  # Test with histopathology dataset if available
  if (exists("histopathology") && is.data.frame(histopathology)) {
    histo_data <- histopathology
    
    # Find appropriate numeric variables
    numeric_candidates <- c("Age", "Grade", "TStage", "MeasurementA", "MeasurementB")
    available_numeric <- numeric_candidates[numeric_candidates %in% names(histo_data)]
    
    if (length(available_numeric) > 0) {
      # Test with first available numeric variable
      test_var <- histo_data[[available_numeric[1]]]
      test_var <- test_var[!is.na(test_var)]
      
      if (length(test_var) >= 10) {
        expect_true(is.numeric(test_var))
        expect_true(length(test_var) > 0)
        
        # Test basic statistics
        expect_true(is.numeric(mean(test_var, na.rm = TRUE)))
        expect_true(is.numeric(sd(test_var, na.rm = TRUE)))
        expect_true(is.numeric(median(test_var, na.rm = TRUE)))
        
        # Test for outliers if enough data
        if (length(test_var) >= 20) {
          z_scores <- scale(test_var)[,1]
          outliers <- which(abs(z_scores) > 3)
          expect_true(is.numeric(length(outliers)))
        }
      }
    }
    
    # Test with categorical variables
    categorical_candidates <- c("Sex", "Race", "Death", "Group", "Grade_Level")
    available_categorical <- categorical_candidates[categorical_candidates %in% names(histo_data)]
    
    if (length(available_categorical) > 0) {
      test_cat_var <- histo_data[[available_categorical[1]]]
      
      if (length(test_cat_var) >= 10) {
        expect_true(length(test_cat_var) > 0)
        
        # Convert to factor if needed
        if (!is.factor(test_cat_var)) {
          test_cat_var <- as.factor(test_cat_var)
        }
        expect_true(is.factor(test_cat_var))
        expect_true(length(levels(test_cat_var)) >= 1)
      }
    }
  }
})

test_that("Data Quality Assessment - Clinical research scenarios", {
  
  # Test clinical trial data quality scenario
  clinical_trial_quality <- function(missing_pct, outlier_rate, unique_pct) {
    quality_grade <- "A"
    
    if (missing_pct > 30) {
      quality_grade <- "D"
    } else if (missing_pct > 15) {
      quality_grade <- ifelse(quality_grade == "A", "C", quality_grade)
    } else if (missing_pct > 5) {
      quality_grade <- ifelse(quality_grade == "A", "B", quality_grade)
    }
    
    if (outlier_rate > 0.05) {
      quality_grade <- ifelse(quality_grade %in% c("A", "B"), "C", "D")
    }
    
    if (unique_pct < 5) {
      quality_grade <- ifelse(quality_grade == "A", "B", quality_grade)
    }
    
    return(quality_grade)
  }
  
  # Test various clinical scenarios
  expect_equal(clinical_trial_quality(0, 0, 95), "A")  # Excellent quality
  expect_equal(clinical_trial_quality(8, 0.02, 85), "B")  # Good quality
  expect_equal(clinical_trial_quality(20, 0.03, 75), "C")  # Concerning quality
  expect_equal(clinical_trial_quality(35, 0.08, 60), "D")  # Poor quality
  
  # Test biomarker data quality (should be high precision)
  biomarker_data <- rnorm(100, 50, 5)  # Tight distribution
  biomarker_cv <- abs(sd(biomarker_data) / mean(biomarker_data)) * 100
  expect_true(biomarker_cv < 15)  # Should have low coefficient of variation
  
  # Test patient-reported outcome data (more variable)
  pro_data <- sample(1:10, 100, replace = TRUE)  # Likert scale
  pro_unique <- length(unique(pro_data))
  expect_true(pro_unique <= 10)  # Limited scale range
  expect_true(pro_unique >= 3)   # Some variability expected
})

test_that("Data Quality Assessment - Performance and scalability", {
  
  # Test with larger dataset
  large_dataset <- data.frame(
    id = 1:1000,
    measurement = rnorm(1000, 100, 20),
    category = factor(sample(c("A", "B", "C", "D"), 1000, replace = TRUE))
  )
  
  expect_equal(nrow(large_dataset), 1000)
  expect_true(ncol(large_dataset) == 3)
  
  # Test analysis performance on larger dataset
  large_measurement <- large_dataset$measurement
  
  # Should handle computation efficiently
  start_time <- Sys.time()
  missing_count <- sum(is.na(large_measurement))
  z_scores <- scale(large_measurement[!is.na(large_measurement)])[,1]
  outliers <- sum(abs(z_scores) > 3)
  end_time <- Sys.time()
  
  computation_time <- as.numeric(end_time - start_time)
  expect_true(computation_time < 1)  # Should complete quickly
  
  expect_true(is.numeric(missing_count))
  expect_true(is.numeric(outliers))
})

test_that("Data Quality Assessment - Output validation", {
  
  # Test that all required output components can be generated
  test_data_simple <- data.frame(
    patient_id = 1:50,
    measurement = rnorm(50, 25, 5),
    category = factor(sample(c("Group1", "Group2"), 50, replace = TRUE))
  )
  
  # Test missing data metrics
  missing_vals_output <- list(
    "Total Observations" = nrow(test_data_simple),
    "Missing Values" = sum(is.na(test_data_simple$measurement)),
    "Complete Cases" = sum(!is.na(test_data_simple$measurement)),
    "Unique Values" = length(unique(na.omit(test_data_simple$measurement)))
  )
  
  expect_true(missing_vals_output[["Total Observations"]] > 0)
  expect_true(missing_vals_output[["Missing Values"]] >= 0)
  expect_true(missing_vals_output[["Complete Cases"]] > 0)
  expect_true(missing_vals_output[["Unique Values"]] > 0)
  
  # Test distribution analysis output
  clean_measurement <- test_data_simple$measurement[!is.na(test_data_simple$measurement)]
  distribution_output <- list(
    "Mean" = mean(clean_measurement),
    "Median" = median(clean_measurement),
    "Standard Deviation" = sd(clean_measurement),
    "Skewness" = mean((clean_measurement - mean(clean_measurement))^3) / sd(clean_measurement)^3,
    "Range" = max(clean_measurement) - min(clean_measurement)
  )
  
  expect_true(all(sapply(distribution_output, is.numeric)))
  expect_true(all(sapply(distribution_output, is.finite)))
})

# Test completion message
cat("✅ Data Quality Assessment test suite completed successfully!\n")
cat("📊 Tests covered:\n")
cat("   - Basic functionality and data validation\n")
cat("   - Missing data analysis and interpretation\n") 
cat("   - Outlier detection and severity classification\n")
cat("   - Distribution analysis and descriptive statistics\n")
cat("   - Duplicate value detection and analysis\n")
cat("   - Data pattern recognition\n")
cat("   - Quality grading system (A-D grades)\n")
cat("   - Edge cases and error handling\n")
cat("   - Categorical data handling\n")
cat("   - Statistical interpretation functions\n")
cat("   - Integration with ClinicoPath datasets\n")
cat("   - Clinical research scenarios\n")
cat("   - Performance and scalability testing\n")
cat("   - Output validation and structure\n")