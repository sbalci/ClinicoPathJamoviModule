test_that("jpps works with single predictor analysis", {
  # Create test data with known relationship
  test_data <- data.frame(
    target = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
    predictor = c(2, 4, 6, 8, 10, 12, 14, 16, 18, 20)  # strong linear relationship
  )
  
  # Test basic functionality
  result <- jpps(
    data = test_data,
    analysis_type = "single",
    target_var = "target",
    predictor_var = "predictor",
    algorithm = "tree",
    cv_folds = 2,
    sample_size = 0
  )
  
  expect_s3_class(result, "Group")
  expect_true("pps_scores" %in% names(result))
  expect_true("instructions" %in% names(result))
})

test_that("jpps works with multiple predictors analysis", {
  # Create test data with multiple predictors
  set.seed(123)
  test_data <- data.frame(
    target = runif(30, 1, 100),
    pred1 = runif(30, 10, 50),
    pred2 = runif(30, 20, 80),
    pred3 = runif(30, 5, 25)
  )
  
  result <- jpps(
    data = test_data,
    analysis_type = "predictors",
    target_var = "target",
    predictor_vars = c("pred1", "pred2", "pred3"),
    algorithm = "auto",
    cv_folds = 3
  )
  
  expect_s3_class(result, "Group")
  expect_true("pps_scores" %in% names(result))
})

test_that("jpps works with matrix analysis", {
  # Create test data for matrix analysis
  set.seed(456)
  test_data <- data.frame(
    var1 = runif(20, 0, 10),
    var2 = runif(20, 5, 15),
    var3 = runif(20, 10, 20)
  )
  
  result <- jpps(
    data = test_data,
    analysis_type = "matrix",
    matrix_vars = c("var1", "var2", "var3"),
    algorithm = "tree",
    cv_folds = 2,
    show_heatmap = TRUE
  )
  
  expect_s3_class(result, "Group")
  expect_true("pps_scores" %in% names(result))
  expect_true("pps_heatmap" %in% names(result))
})

test_that("jpps works with correlation comparison", {
  # Create test data with known correlations
  set.seed(789)
  n <- 25
  x1 <- rnorm(n)
  x2 <- 0.8 * x1 + 0.6 * rnorm(n)  # correlated with x1
  x3 <- rnorm(n)  # independent
  
  test_data <- data.frame(x1 = x1, x2 = x2, x3 = x3)
  
  result <- jpps(
    data = test_data,
    analysis_type = "compare",
    matrix_vars = c("x1", "x2", "x3"),
    show_correlation_comparison = TRUE,
    correlation_method = "pearson",
    cv_folds = 2
  )
  
  expect_s3_class(result, "Group")
  expect_true("correlation_comparison" %in% names(result))
  expect_true("comparison_plot" %in% names(result))
})

test_that("jpps handles different algorithms", {
  test_data <- data.frame(
    target = 1:15,
    predictor = (1:15)^2  # non-linear relationship
  )
  
  algorithms <- c("tree", "forest", "auto")
  
  for (algo in algorithms) {
    result <- jpps(
      data = test_data,
      analysis_type = "single",
      target_var = "target",
      predictor_var = "predictor",
      algorithm = algo,
      cv_folds = 2
    )
    
    expect_s3_class(result, "Group")
  }
})

test_that("jpps handles different cross-validation folds", {
  test_data <- data.frame(
    y = rnorm(40),
    x = rnorm(40)
  )
  
  cv_values <- c(2, 3, 5, 10)
  
  for (cv in cv_values) {
    result <- jpps(
      data = test_data,
      analysis_type = "single",
      target_var = "y",
      predictor_var = "x",
      cv_folds = cv
    )
    
    expect_s3_class(result, "Group")
  }
})

test_that("jpps handles sample size limits", {
  # Create large dataset
  set.seed(111)
  large_data <- data.frame(
    target = rnorm(200),
    pred1 = rnorm(200),
    pred2 = rnorm(200)
  )
  
  # Test with sample size limit
  result <- jpps(
    data = large_data,
    analysis_type = "predictors",
    target_var = "target",
    predictor_vars = c("pred1", "pred2"),
    sample_size = 50,
    cv_folds = 2
  )
  
  expect_s3_class(result, "Group")
})

test_that("jpps handles different correlation methods", {
  test_data <- data.frame(
    x = 1:20,
    y = (1:20) + rnorm(20, 0, 2),
    z = rnorm(20)
  )
  
  methods <- c("pearson", "spearman", "kendall")
  
  for (method in methods) {
    result <- jpps(
      data = test_data,
      analysis_type = "compare",
      matrix_vars = c("x", "y", "z"),
      show_correlation_comparison = TRUE,
      correlation_method = method,
      cv_folds = 2
    )
    
    expect_s3_class(result, "Group")
  }
})

test_that("jpps handles threshold filtering", {
  test_data <- data.frame(
    target = runif(20),
    pred1 = runif(20),
    pred2 = runif(20)
  )
  
  thresholds <- c(0, 0.1, 0.5, 0.8)
  
  for (threshold in thresholds) {
    result <- jpps(
      data = test_data,
      analysis_type = "predictors",
      target_var = "target",
      predictor_vars = c("pred1", "pred2"),
      min_pps_threshold = threshold,
      cv_folds = 2
    )
    
    expect_s3_class(result, "Group")
  }
})

test_that("jpps handles different sorting options", {
  test_data <- data.frame(
    target = 1:15,
    pred_a = runif(15),
    pred_b = runif(15),
    pred_c = runif(15)
  )
  
  sort_methods <- c("pps_desc", "pps_asc", "alphabetical", "none")
  
  for (sort_method in sort_methods) {
    result <- jpps(
      data = test_data,
      analysis_type = "predictors",
      target_var = "target",
      predictor_vars = c("pred_a", "pred_b", "pred_c"),
      sort_results = sort_method,
      cv_folds = 2
    )
    
    expect_s3_class(result, "Group")
  }
})

test_that("jpps handles plot customization options", {
  test_data <- data.frame(
    x = rnorm(15),
    y = rnorm(15),
    z = rnorm(15)
  )
  
  result <- jpps(
    data = test_data,
    analysis_type = "matrix",
    matrix_vars = c("x", "y", "z"),
    show_heatmap = TRUE,
    color_scheme = "viridis",
    show_values_on_plot = TRUE,
    plot_title = "Custom Title",
    cv_folds = 2
  )
  
  expect_s3_class(result, "Group")
  expect_true("pps_heatmap" %in% names(result))
})

test_that("jpps handles custom color schemes", {
  test_data <- data.frame(
    var1 = 1:10,
    var2 = 2:11
  )
  
  color_schemes <- c("viridis", "blues", "reds", "greens", "custom")
  
  for (scheme in color_schemes) {
    result <- jpps(
      data = test_data,
      analysis_type = "matrix",
      matrix_vars = c("var1", "var2"),
      color_scheme = scheme,
      custom_color_low = "#FFFFFF",
      custom_color_high = "#0000FF",
      cv_folds = 2
    )
    
    expect_s3_class(result, "Group")
  }
})

test_that("jpps handles summary and interpretation options", {
  test_data <- data.frame(
    target = rnorm(20),
    predictor = rnorm(20)
  )
  
  # Test with summary enabled
  result_with_summary <- jpps(
    data = test_data,
    analysis_type = "single",
    target_var = "target",
    predictor_var = "predictor",
    show_summary = TRUE,
    show_interpretation = TRUE,
    cv_folds = 2
  )
  
  expect_s3_class(result_with_summary, "Group")
  expect_true("summary_stats" %in% names(result_with_summary))
  expect_true("interpretation" %in% names(result_with_summary))
  
  # Test with summary disabled
  result_no_summary <- jpps(
    data = test_data,
    analysis_type = "single",
    target_var = "target",
    predictor_var = "predictor",
    show_summary = FALSE,
    show_interpretation = FALSE,
    cv_folds = 2
  )
  
  expect_s3_class(result_no_summary, "Group")
})

test_that("jpps handles missing values correctly", {
  # Create data with missing values
  test_data <- data.frame(
    target = c(1, 2, NA, 4, 5, 6, 7, 8, 9, 10),
    predictor = c(2, NA, 6, 8, 10, 12, 14, 16, 18, 20)
  )
  
  result <- jpps(
    data = test_data,
    analysis_type = "single",
    target_var = "target",
    predictor_var = "predictor",
    cv_folds = 2
  )
  
  expect_s3_class(result, "Group")
})

test_that("jpps handles edge cases gracefully", {
  # Test with minimal data
  minimal_data <- data.frame(
    x = c(1, 2),
    y = c(3, 4)
  )
  
  result <- jpps(
    data = minimal_data,
    analysis_type = "single",
    target_var = "y",
    predictor_var = "x",
    cv_folds = 2
  )
  
  expect_s3_class(result, "Group")
})

test_that("jpps validates input parameters correctly", {
  test_data <- data.frame(
    var1 = 1:10,
    var2 = 11:20
  )
  
  # Test with missing target variable (should show instructions)
  result_missing_target <- jpps(
    data = test_data,
    analysis_type = "single",
    predictor_var = "var1"
  )
  
  expect_s3_class(result_missing_target, "Group")
  
  # Test with insufficient matrix variables (should show instructions)
  result_insufficient_vars <- jpps(
    data = test_data,
    analysis_type = "matrix",
    matrix_vars = "var1"  # Only one variable
  )
  
  expect_s3_class(result_insufficient_vars, "Group")
})

test_that("jpps performance optimizations work", {
  # Test that caching methods exist and function properly
  set.seed(123)
  test_data <- data.frame(
    target = rnorm(50),
    pred1 = rnorm(50),
    pred2 = rnorm(50)
  )
  
  # Create a class instance to test caching
  options <- ClinicoPath:::jppsOptions$new(
    analysis_type = "predictors",
    target_var = "target",
    predictor_vars = c("pred1", "pred2"),
    cv_folds = 2
  )
  
  analysis <- ClinicoPath:::jppsClass$new(
    options = options,
    data = test_data
  )
  
  # Test that optimization methods exist
  expect_true(exists(".canUseCache", envir = analysis$.__enclos_env__$private))
  expect_true(exists(".calculateDataHash", envir = analysis$.__enclos_env__$private))
  expect_true(exists(".calculateOptionsHash", envir = analysis$.__enclos_env__$private))
  expect_true(exists(".updateCache", envir = analysis$.__enclos_env__$private))
})

test_that("jpps works with categorical variables", {
  # Create mixed data types
  test_data <- data.frame(
    numeric_target = rnorm(20),
    categorical_pred = factor(sample(c("A", "B", "C"), 20, replace = TRUE)),
    numeric_pred = rnorm(20),
    stringsAsFactors = FALSE
  )
  
  result <- jpps(
    data = test_data,
    analysis_type = "predictors",
    target_var = "numeric_target",
    predictor_vars = c("categorical_pred", "numeric_pred"),
    cv_folds = 2
  )
  
  expect_s3_class(result, "Group")
})

test_that("jpps handles different variable types in matrix", {
  # Create mixed data for matrix analysis
  set.seed(456)
  test_data <- data.frame(
    num1 = rnorm(25),
    num2 = runif(25, 0, 10),
    cat1 = factor(sample(c("X", "Y"), 25, replace = TRUE)),
    stringsAsFactors = FALSE
  )
  
  result <- jpps(
    data = test_data,
    analysis_type = "matrix",
    matrix_vars = c("num1", "num2", "cat1"),
    cv_folds = 2
  )
  
  expect_s3_class(result, "Group")
})

test_that("jpps error handling works correctly", {
  test_data <- data.frame(
    var1 = 1:5,
    var2 = 6:10
  )
  
  # Test with empty data
  empty_result <- jpps(
    data = data.frame(),
    analysis_type = "single",
    target_var = "nonexistent",
    predictor_var = "also_nonexistent"
  )
  
  expect_s3_class(empty_result, "Group")
})

test_that("jpps works with large datasets efficiently", {
  # Test with larger dataset to check performance optimizations
  set.seed(789)
  large_data <- data.frame(
    target = rnorm(100),
    pred1 = rnorm(100),
    pred2 = rnorm(100),
    pred3 = rnorm(100)
  )
  
  # This should complete without significant delay due to optimizations
  start_time <- Sys.time()
  result <- jpps(
    data = large_data,
    analysis_type = "predictors",
    target_var = "target",
    predictor_vars = c("pred1", "pred2", "pred3"),
    sample_size = 50,  # Use sampling for speed
    cv_folds = 2
  )
  end_time <- Sys.time()
  
  expect_s3_class(result, "Group")
  # Performance test - should complete reasonably quickly
  expect_lt(as.numeric(end_time - start_time), 30) # Less than 30 seconds
})

test_that("jpps handles extreme values", {
  # Test with extreme values
  extreme_data <- data.frame(
    low_values = c(0.0001, 0.0002, 0.0001, 0.0003, 0.0002),
    high_values = c(1000000, 999999, 1000001, 999998, 1000002),
    normal_values = c(1, 2, 3, 4, 5)
  )
  
  result <- jpps(
    data = extreme_data,
    analysis_type = "matrix",
    matrix_vars = c("low_values", "high_values", "normal_values"),
    cv_folds = 2
  )
  
  expect_s3_class(result, "Group")
})

test_that("jpps works with identical values", {
  # Test with identical values
  identical_data <- data.frame(
    constant = rep(5, 10),
    target = 1:10
  )
  
  # Should handle gracefully without errors
  result <- jpps(
    data = identical_data,
    analysis_type = "single",
    target_var = "target",
    predictor_var = "constant",
    cv_folds = 2
  )
  
  expect_s3_class(result, "Group")
})

test_that("jpps barplot functionality works", {
  test_data <- data.frame(
    target = rnorm(15),
    pred1 = rnorm(15),
    pred2 = rnorm(15)
  )
  
  result <- jpps(
    data = test_data,
    analysis_type = "predictors",
    target_var = "target",
    predictor_vars = c("pred1", "pred2"),
    show_barplot = TRUE,
    cv_folds = 2
  )
  
  expect_s3_class(result, "Group")
  expect_true("pps_barplot" %in% names(result))
})

test_that("jpps handles all analysis types", {
  test_data <- data.frame(
    var1 = rnorm(20),
    var2 = rnorm(20),
    var3 = rnorm(20)
  )
  
  analysis_types <- c("single", "predictors", "matrix", "compare")
  
  for (type in analysis_types) {
    if (type == "single") {
      result <- jpps(
        data = test_data,
        analysis_type = type,
        target_var = "var1",
        predictor_var = "var2",
        cv_folds = 2
      )
    } else if (type == "predictors") {
      result <- jpps(
        data = test_data,
        analysis_type = type,
        target_var = "var1",
        predictor_vars = c("var2", "var3"),
        cv_folds = 2
      )
    } else {
      result <- jpps(
        data = test_data,
        analysis_type = type,
        matrix_vars = c("var1", "var2", "var3"),
        show_correlation_comparison = (type == "compare"),
        cv_folds = 2
      )
    }
    
    expect_s3_class(result, "Group")
  }
})