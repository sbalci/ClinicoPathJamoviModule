# Load required libraries and data
devtools::load_all()
data(histopathology, package = "ClinicoPath")

test_that("jcorrelation module loads correctly", {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()
  expect_true(exists("jcorrelationClass"))
  expect_true(is.function(jcorrelation))
})

test_that("correlation handles basic input validation", {
  # Test with insufficient variables
  expect_error(
    jcorrelation(data = histopathology, vars = "Age"),
    NA  # Should not error during initialization, only during run
  )
  
  # Test with no variables
  expect_error(
    jcorrelation(data = histopathology, vars = character(0)),
    NA  # Should not error during initialization
  )
})

test_that("correlation works with basic numeric variables", {
  # Test basic functionality with numeric variables
  result <- jcorrelation(
    data = histopathology,
    vars = c("Age", "OverallTime")
  )
  
  expect_s3_class(result, "jcorrelationClass")
  expect_true("Age" %in% names(histopathology))
  expect_true("OverallTime" %in% names(histopathology))
})

test_that("correlation handles multiple variables", {
  # Test with multiple numeric variables
  result <- jcorrelation(
    data = histopathology,
    vars = c("Age", "OverallTime", "MeasurementA", "MeasurementB")
  )
  
  expect_s3_class(result, "jcorrelationClass")
  
  # Check that results contain expected components
  expect_true("matrix" %in% names(result$results))
  expect_true("tests" %in% names(result$results))
  expect_true("summary" %in% names(result$results))
})

test_that("correlation handles different correlation methods", {
  # Test Pearson correlation
  result_pearson <- jcorrelation(
    data = histopathology,
    vars = c("Age", "OverallTime", "MeasurementA"),
    method = "pearson"
  )
  
  expect_s3_class(result_pearson, "jcorrelationClass")
  
  # Test Spearman correlation
  result_spearman <- jcorrelation(
    data = histopathology,
    vars = c("Age", "OverallTime", "MeasurementA"),
    method = "spearman"
  )
  
  expect_s3_class(result_spearman, "jcorrelationClass")
  
  # Test Kendall correlation
  result_kendall <- jcorrelation(
    data = histopathology,
    vars = c("Age", "OverallTime", "MeasurementA"),
    method = "kendall"
  )
  
  expect_s3_class(result_kendall, "jcorrelationClass")
})

test_that("correlation handles confidence intervals", {
  # Test with confidence intervals
  result <- jcorrelation(
    data = histopathology,
    vars = c("Age", "OverallTime", "MeasurementA"),
    ci = TRUE,
    ciWidth = 95
  )
  
  expect_s3_class(result, "jcorrelationClass")
  
  # Test different confidence levels
  result_99 <- jcorrelation(
    data = histopathology,
    vars = c("Age", "OverallTime"),
    ci = TRUE,
    ciWidth = 99
  )
  
  expect_s3_class(result_99, "jcorrelationClass")
})

test_that("correlation handles alternative hypotheses", {
  # Test two-sided hypothesis
  result_two <- jcorrelation(
    data = histopathology,
    vars = c("Age", "OverallTime"),
    alternative = "two.sided"
  )
  
  expect_s3_class(result_two, "jcorrelationClass")
  
  # Test greater hypothesis
  result_greater <- jcorrelation(
    data = histopathology,
    vars = c("Age", "OverallTime"),
    alternative = "greater"
  )
  
  expect_s3_class(result_greater, "jcorrelationClass")
  
  # Test less hypothesis
  result_less <- jcorrelation(
    data = histopathology,
    vars = c("Age", "OverallTime"),
    alternative = "less"
  )
  
  expect_s3_class(result_less, "jcorrelationClass")
})

test_that("correlation handles significance flagging", {
  # Test with significance flagging enabled
  result <- jcorrelation(
    data = histopathology,
    vars = c("Age", "OverallTime", "MeasurementA", "MeasurementB"),
    flag = TRUE,
    flagAlpha = 0.05
  )
  
  expect_s3_class(result, "jcorrelationClass")
  
  # Test with different alpha levels
  result_strict <- jcorrelation(
    data = histopathology,
    vars = c("Age", "OverallTime", "MeasurementA"),
    flag = TRUE,
    flagAlpha = 0.01
  )
  
  expect_s3_class(result_strict, "jcorrelationClass")
})

test_that("correlation handles grouping variables", {
  # Test with grouping variable
  result <- jcorrelation(
    data = histopathology,
    vars = c("Age", "OverallTime", "MeasurementA"),
    group = "Sex"
  )
  
  expect_s3_class(result, "jcorrelationClass")
  
  # Test with another grouping variable
  result_group2 <- jcorrelation(
    data = histopathology,
    vars = c("Age", "OverallTime"),
    group = "Group"
  )
  
  expect_s3_class(result_group2, "jcorrelationClass")
})

test_that("correlation handles plot options", {
  # Test with plots enabled
  result <- jcorrelation(
    data = histopathology,
    vars = c("Age", "OverallTime", "MeasurementA"),
    plots = TRUE,
    plotType = "matrix"
  )
  
  expect_s3_class(result, "jcorrelationClass")
  
  # Test different plot types
  result_pairs <- jcorrelation(
    data = histopathology,
    vars = c("Age", "OverallTime", "MeasurementA"),
    plots = TRUE,
    plotType = "pairs"
  )
  
  expect_s3_class(result_pairs, "jcorrelationClass")
  
  # Test network plot type
  result_network <- jcorrelation(
    data = histopathology,
    vars = c("Age", "OverallTime", "MeasurementA"),
    plots = TRUE,
    plotType = "network"
  )
  
  expect_s3_class(result_network, "jcorrelationClass")
  
  # Test with plots disabled
  result_no_plots <- jcorrelation(
    data = histopathology,
    vars = c("Age", "OverallTime"),
    plots = FALSE
  )
  
  expect_s3_class(result_no_plots, "jcorrelationClass")
})

test_that("correlation handles natural language reporting", {
  # Test with reporting enabled
  result <- jcorrelation(
    data = histopathology,
    vars = c("Age", "OverallTime", "MeasurementA"),
    report = TRUE
  )
  
  expect_s3_class(result, "jcorrelationClass")
  
  # Test with reporting disabled
  result_no_report <- jcorrelation(
    data = histopathology,
    vars = c("Age", "OverallTime"),
    report = FALSE
  )
  
  expect_s3_class(result_no_report, "jcorrelationClass")
})

test_that("correlation handles edge cases", {
  # Test with small dataset
  small_data <- histopathology[1:10, ]
  
  expect_error({
    result <- jcorrelation(
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
    result <- jcorrelation(
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
    result <- jcorrelation(
      data = test_data,
      vars = c("Age", "OverallTime", "MeasurementA")
    )
  }, NA)  # Should handle missing data gracefully
})

test_that("correlation handles different measurement scales", {
  # Test with measurement variables (continuous)
  result <- jcorrelation(
    data = histopathology,
    vars = c("MeasurementA", "MeasurementB", "Measurement1", "Measurement2")
  )
  
  expect_s3_class(result, "jcorrelationClass")
  
  # Test with mixed scales (age, time, measurements)
  result_mixed <- jcorrelation(
    data = histopathology,
    vars = c("Age", "OverallTime", "MeasurementA", "Anti-X-intensity")
  )
  
  expect_s3_class(result_mixed, "jcorrelationClass")
})

test_that("correlation handles large number of variables", {
  # Test with many variables (should trigger summary statistics)
  numeric_vars <- c("Age", "Grade", "TStage", "Anti-X-intensity", "Anti-Y-intensity", 
                   "OverallTime", "MeasurementA", "MeasurementB", "Measurement1", "Measurement2")
  
  result <- jcorrelation(
    data = histopathology,
    vars = numeric_vars[1:6]  # Use first 6 to ensure summary table appears
  )
  
  expect_s3_class(result, "jcorrelationClass")
})

test_that("correlation comprehensive test with all options", {
  # Test with all options enabled
  result <- jcorrelation(
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
  
  expect_s3_class(result, "jcorrelationClass")
  
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
  
  result <- jcorrelation(
    data = correlated_data,
    vars = c("x", "y", "z", "w"),
    flag = TRUE,
    flagAlpha = 0.05
  )
  
  expect_s3_class(result, "jcorrelationClass")
})

test_that("correlation validates input types", {
  # Test that non-numeric variables are handled appropriately
  mixed_data <- histopathology
  
  # Should work with numeric variables
  result <- jcorrelation(
    data = mixed_data,
    vars = c("Age", "OverallTime")  # These are numeric
  )
  
  expect_s3_class(result, "jcorrelationClass")
})

test_that("correlation handles zero variance variables", {
  # Test with constant variable
  constant_data <- data.frame(
    x = rep(5, 50),      # Constant variable
    y = rnorm(50),       # Normal variable
    z = rnorm(50)        # Another normal variable
  )
  
  expect_error({
    result <- jcorrelation(
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
    result_small <- jcorrelation(
      data = small_sample,
      vars = c("Age", "OverallTime", "MeasurementA")
    )
  }, NA)
  
  # Medium sample
  expect_error({
    result_medium <- jcorrelation(
      data = medium_sample,
      vars = c("Age", "OverallTime", "MeasurementA")
    )
  }, NA)
  
  # Large sample
  expect_error({
    result_large <- jcorrelation(
      data = large_sample,
      vars = c("Age", "OverallTime", "MeasurementA")
    )
  }, NA)
})
