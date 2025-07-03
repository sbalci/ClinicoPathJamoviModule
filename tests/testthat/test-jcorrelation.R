# Load required libraries and data
library(ClinicoPath)
data(histopathology, package = "ClinicoPath")

test_that("correlation module loads correctly", {
  expect_true(exists("correlationClass"))
  expect_true(is.function(correlation))
})

test_that("correlation handles basic input validation", {
  # Test with insufficient variables
  expect_error(
    correlation(data = histopathology, vars = "Age"),
    NA  # Should not error during initialization, only during run
  )
  
  # Test with no variables
  expect_error(
    correlation(data = histopathology, vars = character(0)),
    NA  # Should not error during initialization
  )
})

test_that("correlation works with basic numeric variables", {
  # Test basic functionality with numeric variables
  result <- correlation(
    data = histopathology,
    vars = c("Age", "OverallTime")
  )
  
  expect_s3_class(result, "correlationClass")
  expect_true("Age" %in% names(histopathology))
  expect_true("OverallTime" %in% names(histopathology))
})

test_that("correlation handles multiple variables", {
  # Test with multiple numeric variables
  result <- correlation(
    data = histopathology,
    vars = c("Age", "OverallTime", "MeasurementA", "MeasurementB")
  )
  
  expect_s3_class(result, "correlationClass")
  
  # Check that results contain expected components
  expect_true("matrix" %in% names(result$results))
  expect_true("tests" %in% names(result$results))
  expect_true("summary" %in% names(result$results))
})

test_that("correlation handles different correlation methods", {
  # Test Pearson correlation
  result_pearson <- correlation(
    data = histopathology,
    vars = c("Age", "OverallTime", "MeasurementA"),
    method = "pearson"
  )
  
  expect_s3_class(result_pearson, "correlationClass")
  
  # Test Spearman correlation
  result_spearman <- correlation(
    data = histopathology,
    vars = c("Age", "OverallTime", "MeasurementA"),
    method = "spearman"
  )
  
  expect_s3_class(result_spearman, "correlationClass")
  
  # Test Kendall correlation
  result_kendall <- correlation(
    data = histopathology,
    vars = c("Age", "OverallTime", "MeasurementA"),
    method = "kendall"
  )
  
  expect_s3_class(result_kendall, "correlationClass")
})

test_that("correlation handles confidence intervals", {
  # Test with confidence intervals
  result <- correlation(
    data = histopathology,
    vars = c("Age", "OverallTime", "MeasurementA"),
    ci = TRUE,
    ciWidth = 95
  )
  
  expect_s3_class(result, "correlationClass")
  
  # Test different confidence levels
  result_99 <- correlation(
    data = histopathology,
    vars = c("Age", "OverallTime"),
    ci = TRUE,
    ciWidth = 99
  )
  
  expect_s3_class(result_99, "correlationClass")
})

test_that("correlation handles alternative hypotheses", {
  # Test two-sided hypothesis
  result_two <- correlation(
    data = histopathology,
    vars = c("Age", "OverallTime"),
    alternative = "two.sided"
  )
  
  expect_s3_class(result_two, "correlationClass")
  
  # Test greater hypothesis
  result_greater <- correlation(
    data = histopathology,
    vars = c("Age", "OverallTime"),
    alternative = "greater"
  )
  
  expect_s3_class(result_greater, "correlationClass")
  
  # Test less hypothesis
  result_less <- correlation(
    data = histopathology,
    vars = c("Age", "OverallTime"),
    alternative = "less"
  )
  
  expect_s3_class(result_less, "correlationClass")
})

test_that("correlation handles significance flagging", {
  # Test with significance flagging enabled
  result <- correlation(
    data = histopathology,
    vars = c("Age", "OverallTime", "MeasurementA", "MeasurementB"),
    flag = TRUE,
    flagAlpha = 0.05
  )
  
  expect_s3_class(result, "correlationClass")
  
  # Test with different alpha levels
  result_strict <- correlation(
    data = histopathology,
    vars = c("Age", "OverallTime", "MeasurementA"),
    flag = TRUE,
    flagAlpha = 0.01
  )
  
  expect_s3_class(result_strict, "correlationClass")
})

test_that("correlation handles grouping variables", {
  # Test with grouping variable
  result <- correlation(
    data = histopathology,
    vars = c("Age", "OverallTime", "MeasurementA"),
    group = "Sex"
  )
  
  expect_s3_class(result, "correlationClass")
  
  # Test with another grouping variable
  result_group2 <- correlation(
    data = histopathology,
    vars = c("Age", "OverallTime"),
    group = "Group"
  )
  
  expect_s3_class(result_group2, "correlationClass")
})

test_that("correlation handles plot options", {
  # Test with plots enabled
  result <- correlation(
    data = histopathology,
    vars = c("Age", "OverallTime", "MeasurementA"),
    plots = TRUE,
    plotType = "matrix"
  )
  
  expect_s3_class(result, "correlationClass")
  
  # Test different plot types
  result_pairs <- correlation(
    data = histopathology,
    vars = c("Age", "OverallTime", "MeasurementA"),
    plots = TRUE,
    plotType = "pairs"
  )
  
  expect_s3_class(result_pairs, "correlationClass")
  
  # Test with plots disabled
  result_no_plots <- correlation(
    data = histopathology,
    vars = c("Age", "OverallTime"),
    plots = FALSE
  )
  
  expect_s3_class(result_no_plots, "correlationClass")
})

test_that("correlation handles natural language reporting", {
  # Test with reporting enabled
  result <- correlation(
    data = histopathology,
    vars = c("Age", "OverallTime", "MeasurementA"),
    report = TRUE
  )
  
  expect_s3_class(result, "correlationClass")
  
  # Test with reporting disabled
  result_no_report <- correlation(
    data = histopathology,
    vars = c("Age", "OverallTime"),
    report = FALSE
  )
  
  expect_s3_class(result_no_report, "correlationClass")
})

test_that("correlation handles edge cases", {
  # Test with small dataset
  small_data <- histopathology[1:10, ]
  
  expect_error({
    result <- correlation(
      data = small_data,
      vars = c("Age", "OverallTime")
    )
  }, NA)  # Should not error during initialization
  
  # Test with variables that have perfect correlation
  perfect_data <- data.frame(
    x = 1:20,
    y = 1:20,  # Perfect positive correlation
    z = 20:1   # Perfect negative correlation with x
  )
  
  expect_error({
    result <- correlation(
      data = perfect_data,
      vars = c("x", "y", "z")
    )
  }, NA)
})

test_that("correlation handles missing data appropriately", {
  # Create dataset with missing values
  test_data <- histopathology[1:50, ]
  test_data$Age[1:5] <- NA
  test_data$OverallTime[6:10] <- NA
  
  expect_error({
    result <- correlation(
      data = test_data,
      vars = c("Age", "OverallTime", "MeasurementA")
    )
  }, NA)  # Should handle missing data gracefully
})

test_that("correlation handles different measurement scales", {
  # Test with measurement variables (continuous)
  result <- correlation(
    data = histopathology,
    vars = c("MeasurementA", "MeasurementB", "Measurement1", "Measurement2")
  )
  
  expect_s3_class(result, "correlationClass")
  
  # Test with mixed scales (age, time, measurements)
  result_mixed <- correlation(
    data = histopathology,
    vars = c("Age", "OverallTime", "MeasurementA", "Anti-X-intensity")
  )
  
  expect_s3_class(result_mixed, "correlationClass")
})

test_that("correlation handles large number of variables", {
  # Test with many variables (should trigger summary statistics)
  numeric_vars <- c("Age", "Grade", "TStage", "Anti-X-intensity", "Anti-Y-intensity", 
                   "OverallTime", "MeasurementA", "MeasurementB", "Measurement1", "Measurement2")
  
  result <- correlation(
    data = histopathology,
    vars = numeric_vars[1:6]  # Use first 6 to ensure summary table appears
  )
  
  expect_s3_class(result, "correlationClass")
})

test_that("correlation comprehensive test with all options", {
  # Test with all options enabled
  result <- correlation(
    data = histopathology,
    vars = c("Age", "OverallTime", "MeasurementA", "MeasurementB"),
    method = "pearson",
    alternative = "two.sided",
    ci = TRUE,
    ciWidth = 95,
    flag = TRUE,
    flagAlpha = 0.05,
    plots = TRUE,
    plotType = "matrix",
    report = TRUE
  )
  
  expect_s3_class(result, "correlationClass")
  
  # Verify the structure of results
  expect_true("matrix" %in% names(result$results))
  expect_true("tests" %in% names(result$results))
  expect_true("summary" %in% names(result$results))
  expect_true("report" %in% names(result$results))
  expect_true("plot" %in% names(result$results))
})

test_that("correlation handles special correlation scenarios", {
  # Test with highly correlated variables
  correlated_data <- data.frame(
    x = rnorm(100),
    y = rnorm(100),
    z = rnorm(100)
  )
  correlated_data$w <- correlated_data$x + rnorm(100, 0, 0.1)  # Highly correlated with x
  
  result <- correlation(
    data = correlated_data,
    vars = c("x", "y", "z", "w"),
    flag = TRUE,
    flagAlpha = 0.05
  )
  
  expect_s3_class(result, "correlationClass")
})

test_that("correlation validates input types", {
  # Test that non-numeric variables are handled appropriately
  mixed_data <- histopathology
  
  # Should work with numeric variables
  result <- correlation(
    data = mixed_data,
    vars = c("Age", "OverallTime")  # These are numeric
  )
  
  expect_s3_class(result, "correlationClass")
})

test_that("correlation handles zero variance variables", {
  # Test with constant variable
  constant_data <- data.frame(
    x = rep(5, 50),      # Constant variable
    y = rnorm(50),       # Normal variable
    z = rnorm(50)        # Another normal variable
  )
  
  expect_error({
    result <- correlation(
      data = constant_data,
      vars = c("x", "y", "z")
    )
  }, NA)  # Should handle gracefully, though correlation with constant will be undefined
})

test_that("correlation performance with different sample sizes", {
  # Test with different sample sizes
  small_sample <- histopathology[1:20, ]
  medium_sample <- histopathology[1:100, ]
  large_sample <- histopathology
  
  # Small sample
  expect_error({
    result_small <- correlation(
      data = small_sample,
      vars = c("Age", "OverallTime", "MeasurementA")
    )
  }, NA)
  
  # Medium sample
  expect_error({
    result_medium <- correlation(
      data = medium_sample,
      vars = c("Age", "OverallTime", "MeasurementA")
    )
  }, NA)
  
  # Large sample
  expect_error({
    result_large <- correlation(
      data = large_sample,
      vars = c("Age", "OverallTime", "MeasurementA")
    )
  }, NA)
})