# Tests for nogoldstandard function

library(testthat)
library(ClinicoPath)

# Load test data
data(nogoldstandard_test_data)
data(nogoldstandard_test_data_small)

test_that("nogoldstandard basic functionality works", {
  # Test with basic parameters
  result <- nogoldstandard(
    data = nogoldstandard_test_data_small,
    test1 = "test1_result",
    test1Positive = "positive",
    test2 = "test2_result", 
    test2Positive = "positive",
    method = "composite",
    bootstrap = FALSE
  )
  
  expect_s3_class(result, "nogoldstandardResults")
  
  # Check that prevalence table exists and has correct structure
  prevalence_df <- result$prevalence$asDF
  expect_equal(nrow(prevalence_df), 1)
  expect_true("estimate" %in% names(prevalence_df))
  expect_true("ci_lower" %in% names(prevalence_df))
  expect_true("ci_upper" %in% names(prevalence_df))
  
  # Check that test_metrics table exists
  metrics_df <- result$test_metrics$asDF
  expect_equal(nrow(metrics_df), 2)  # Should have 2 rows for 2 tests
  expect_true(all(c("test", "sensitivity", "specificity", "ppv", "npv") %in% names(metrics_df)))
})

test_that("nogoldstandard works with all methods", {
  methods <- c("latent_class", "composite", "all_positive", "any_positive", "bayesian")
  
  for (method in methods) {
    # Skip LCA if poLCA not available
    if (method == "latent_class" && !requireNamespace("poLCA", quietly = TRUE)) {
      skip("poLCA package not available")
    }
    
    result <- nogoldstandard(
      data = nogoldstandard_test_data_small,
      test1 = "test1_result",
      test1Positive = "positive",
      test2 = "test2_result", 
      test2Positive = "positive",
      test3 = "test3_result",
      test3Positive = "positive",
      method = method,
      bootstrap = FALSE
    )
    
    expect_s3_class(result, "nogoldstandardResults")
    
    # Check prevalence
    prevalence_df <- result$prevalence$asDF
    expect_true(is.numeric(prevalence_df$estimate))
    expect_true(prevalence_df$estimate >= 0 && prevalence_df$estimate <= 1)
    
    # Check model fit for LCA
    if (method == "latent_class") {
      fit_df <- result$model_fit$asDF
      expect_true(nrow(fit_df) > 0)
    }
  }
})

test_that("nogoldstandard handles missing data correctly", {
  # Create data with missing values
  test_data <- nogoldstandard_test_data_small
  test_data$test3_result[c(5, 10, 15)] <- NA
  
  result <- nogoldstandard(
    data = test_data,
    test1 = "test1_result",
    test1Positive = "positive",
    test2 = "test2_result", 
    test2Positive = "positive",
    test3 = "test3_result",
    test3Positive = "positive",
    method = "composite",
    bootstrap = FALSE
  )
  
  expect_s3_class(result, "nogoldstandardResults")
  
  # Check that results are still produced
  metrics_df <- result$test_metrics$asDF
  expect_equal(nrow(metrics_df), 3)
})

test_that("nogoldstandard bootstrap CI works", {
  # Test with small number of bootstrap samples for speed
  result <- nogoldstandard(
    data = nogoldstandard_test_data_small,
    test1 = "test1_result",
    test1Positive = "positive",
    test2 = "test2_result", 
    test2Positive = "positive",
    method = "composite",
    bootstrap = TRUE,
    nboot = 50,  # Small number for testing
    alpha = 0.05
  )
  
  expect_s3_class(result, "nogoldstandardResults")
  
  # Check that CIs are populated
  prevalence_df <- result$prevalence$asDF
  expect_true(!is.na(prevalence_df$ci_lower))
  expect_true(!is.na(prevalence_df$ci_upper))
  expect_true(prevalence_df$ci_lower <= prevalence_df$estimate)
  expect_true(prevalence_df$ci_upper >= prevalence_df$estimate)
})

test_that("nogoldstandard validates inputs correctly", {
  # Test missing required variables
  expect_error(
    nogoldstandard(
      data = nogoldstandard_test_data_small,
      test1 = "test1_result",
      test1Positive = "positive"
      # Missing test2
    ),
    "At least two tests must be specified"
  )
  
  # Test invalid positive level
  expect_error(
    nogoldstandard(
      data = nogoldstandard_test_data_small,
      test1 = "test1_result",
      test1Positive = "invalid_level",  # This level doesn't exist
      test2 = "test2_result",
      test2Positive = "positive"
    ),
    "Level 'invalid_level' not found"
  )
  
  # Test non-factor variable (if data has non-factor columns)
  test_data <- nogoldstandard_test_data_small
  test_data$numeric_col <- rnorm(nrow(test_data))
  
  expect_error(
    nogoldstandard(
      data = test_data,
      test1 = "numeric_col",  # Not a factor
      test1Positive = "positive",
      test2 = "test2_result",
      test2Positive = "positive"
    ),
    "must be a factor"
  )
})

test_that("nogoldstandard works with up to 5 tests", {
  result <- nogoldstandard(
    data = nogoldstandard_test_data,
    test1 = "test1_result",
    test1Positive = "positive",
    test2 = "test2_result", 
    test2Positive = "positive",
    test3 = "test3_result",
    test3Positive = "positive",
    test4 = "test4_result",
    test4Positive = "positive",
    test5 = "test5_result",
    test5Positive = "positive",
    method = "composite",
    bootstrap = FALSE
  )
  
  expect_s3_class(result, "nogoldstandardResults")
  
  # Check that all 5 tests are in the results
  metrics_df <- result$test_metrics$asDF
  expect_equal(nrow(metrics_df), 5)
  expect_true(all(c("test1_result", "test2_result", "test3_result", 
                   "test4_result", "test5_result") %in% metrics_df$test))
})

test_that("nogoldstandard PPV/NPV calculations are reasonable", {
  result <- nogoldstandard(
    data = nogoldstandard_test_data,
    test1 = "test1_result",
    test1Positive = "positive",
    test2 = "test2_result", 
    test2Positive = "positive",
    method = "composite",
    bootstrap = FALSE
  )
  
  metrics_df <- result$test_metrics$asDF
  
  # Check PPV and NPV are within valid range
  expect_true(all(metrics_df$ppv >= 0 & metrics_df$ppv <= 1, na.rm = TRUE))
  expect_true(all(metrics_df$npv >= 0 & metrics_df$npv <= 1, na.rm = TRUE))
  
  # PPV should be higher for tests with higher specificity
  # NPV should be higher for tests with higher sensitivity
  # (This is a general trend, not always exact due to prevalence effects)
})

test_that("nogoldstandard handles empty data appropriately", {
  empty_data <- nogoldstandard_test_data_small[FALSE, ]
  
  expect_error(
    nogoldstandard(
      data = empty_data,
      test1 = "test1_result",
      test1Positive = "positive",
      test2 = "test2_result", 
      test2Positive = "positive"
    ),
    "Data contains no rows"
  )
})

test_that("nogoldstandard method differences are reasonable", {
  # Run all methods on same data
  methods <- c("composite", "all_positive", "any_positive")
  prevalences <- numeric(length(methods))
  
  for (i in seq_along(methods)) {
    result <- nogoldstandard(
      data = nogoldstandard_test_data,
      test1 = "test1_result",
      test1Positive = "positive",
      test2 = "test2_result", 
      test2Positive = "positive",
      test3 = "test3_result",
      test3Positive = "positive",
      method = methods[i],
      bootstrap = FALSE
    )
    
    prevalences[i] <- result$prevalence$asDF$estimate[1]
  }
  
  # Check expected ordering: all_positive < composite < any_positive
  expect_true(prevalences[2] < prevalences[1])  # all_positive < composite
  expect_true(prevalences[1] < prevalences[3])  # composite < any_positive
})