test_that("jjwithinstats works with basic two measurements", {
  # Create repeated measures test data
  test_data <- data.frame(
    pre_treatment = c(85, 92, 78, 88, 94, 82, 90, 76, 89, 93),
    post_treatment = c(75, 78, 68, 82, 84, 70, 83, 65, 79, 86)
  )
  
  # Test basic functionality
  result <- jjwithinstats(
    data = test_data,
    dep1 = "pre_treatment",
    dep2 = "post_treatment",
    typestatistics = "parametric",
    violin = TRUE,
    boxplot = TRUE
  )
  
  expect_s3_class(result, "Group")
  expect_true("plot" %in% names(result))
  expect_true("todo" %in% names(result))
})

test_that("jjwithinstats works with three measurements", {
  # Create three-measurement test data
  test_data <- data.frame(
    baseline = c(100, 95, 88, 92, 97, 85, 90, 93, 89, 96),
    week_4 = c(85, 78, 75, 82, 88, 70, 83, 79, 76, 84),
    week_8 = c(70, 65, 60, 75, 80, 58, 73, 68, 63, 78)
  )
  
  result <- jjwithinstats(
    data = test_data,
    dep1 = "baseline",
    dep2 = "week_4",
    dep3 = "week_8",
    typestatistics = "nonparametric",
    pairwisecomparisons = TRUE
  )
  
  expect_s3_class(result, "Group")
})

test_that("jjwithinstats works with four measurements", {
  # Create four-measurement test data
  test_data <- data.frame(
    time1 = runif(15, 80, 100),
    time2 = runif(15, 75, 95),
    time3 = runif(15, 70, 90),
    time4 = runif(15, 65, 85)
  )
  
  result <- jjwithinstats(
    data = test_data,
    dep1 = "time1",
    dep2 = "time2",
    dep3 = "time3",
    dep4 = "time4",
    typestatistics = "robust",
    centralityplotting = TRUE
  )
  
  expect_s3_class(result, "Group")
})

test_that("jjwithinstats handles different statistical test types", {
  test_data <- data.frame(
    pre = c(50, 55, 48, 52, 58, 45, 51, 49, 56, 54),
    post = c(45, 48, 40, 47, 52, 38, 46, 42, 50, 49)
  )
  
  # Test different statistical types
  stat_types <- c("parametric", "nonparametric", "robust", "bayes")
  
  for (stat_type in stat_types) {
    result <- jjwithinstats(
      data = test_data,
      dep1 = "pre",
      dep2 = "post",
      typestatistics = stat_type,
      pairwisecomparisons = FALSE
    )
    
    expect_s3_class(result, "Group")
  }
})

test_that("jjwithinstats handles pairwise comparison options", {
  test_data <- data.frame(
    measure1 = c(80, 85, 78, 82, 88, 75, 83, 79, 86, 81),
    measure2 = c(70, 75, 68, 72, 78, 65, 73, 69, 76, 71),
    measure3 = c(60, 65, 58, 62, 68, 55, 63, 59, 66, 61)
  )
  
  # Test pairwise display options
  pairwise_displays <- c("significant", "non-significant", "everything")
  
  for (display in pairwise_displays) {
    result <- jjwithinstats(
      data = test_data,
      dep1 = "measure1",
      dep2 = "measure2",
      dep3 = "measure3",
      pairwisecomparisons = TRUE,
      pairwisedisplay = display
    )
    
    expect_s3_class(result, "Group")
  }
})

test_that("jjwithinstats handles p-value adjustment methods", {
  test_data <- data.frame(
    pre = c(90, 88, 92, 85, 94, 87, 91, 89, 93, 86),
    mid = c(80, 78, 82, 75, 84, 77, 81, 79, 83, 76),
    post = c(70, 68, 72, 65, 74, 67, 71, 69, 73, 66)
  )
  
  # Test different adjustment methods
  adjust_methods <- c("holm", "bonferroni", "BH", "none")
  
  for (method in adjust_methods) {
    result <- jjwithinstats(
      data = test_data,
      dep1 = "pre",
      dep2 = "mid",
      dep3 = "post",
      pairwisecomparisons = TRUE,
      padjustmethod = method
    )
    
    expect_s3_class(result, "Group")
  }
})

test_that("jjwithinstats handles effect size types", {
  test_data <- data.frame(
    before = c(100, 95, 105, 90, 110, 85, 98, 102, 88, 107),
    after = c(85, 80, 90, 75, 95, 70, 83, 87, 73, 92)
  )
  
  # Test different effect size types
  effect_types <- c("biased", "unbiased", "eta", "omega")
  
  for (effect_type in effect_types) {
    result <- jjwithinstats(
      data = test_data,
      dep1 = "before",
      dep2 = "after",
      typestatistics = "parametric",
      effsizetype = effect_type
    )
    
    expect_s3_class(result, "Group")
  }
})

test_that("jjwithinstats handles centrality options", {
  test_data <- data.frame(
    time_a = c(75, 80, 78, 82, 77, 85, 73, 79, 81, 76),
    time_b = c(65, 70, 68, 72, 67, 75, 63, 69, 71, 66)
  )
  
  # Test centrality types
  centrality_types <- c("parametric", "nonparametric", "robust", "bayes")
  
  for (centrality_type in centrality_types) {
    result <- jjwithinstats(
      data = test_data,
      dep1 = "time_a",
      dep2 = "time_b",
      centralityplotting = TRUE,
      centralitytype = centrality_type,
      centralitypath = TRUE
    )
    
    expect_s3_class(result, "Group")
  }
})

test_that("jjwithinstats handles plot customization options", {
  test_data <- data.frame(
    measurement1 = c(60, 65, 58, 62, 67, 55, 63, 59, 66, 61),
    measurement2 = c(50, 55, 48, 52, 57, 45, 53, 49, 56, 51)
  )
  
  # Test plot customization
  result <- jjwithinstats(
    data = test_data,
    dep1 = "measurement1",
    dep2 = "measurement2",
    violin = TRUE,
    boxplot = TRUE,
    point = TRUE,
    pointpath = TRUE,
    mytitle = "Custom Title",
    xtitle = "Time Points",
    ytitle = "Measurement Values"
  )
  
  expect_s3_class(result, "Group")
})

test_that("jjwithinstats handles plot component toggles", {
  test_data <- data.frame(
    score1 = c(70, 75, 68, 72, 78, 65, 73, 69, 76, 71),
    score2 = c(60, 65, 58, 62, 68, 55, 63, 59, 66, 61)
  )
  
  # Test with violin plot only
  result_violin <- jjwithinstats(
    data = test_data,
    dep1 = "score1",
    dep2 = "score2",
    violin = TRUE,
    boxplot = FALSE,
    point = FALSE
  )
  expect_s3_class(result_violin, "Group")
  
  # Test with boxplot only
  result_boxplot <- jjwithinstats(
    data = test_data,
    dep1 = "score1",
    dep2 = "score2",
    violin = FALSE,
    boxplot = TRUE,
    point = FALSE
  )
  expect_s3_class(result_boxplot, "Group")
  
  # Test with points only
  result_points <- jjwithinstats(
    data = test_data,
    dep1 = "score1",
    dep2 = "score2",
    violin = FALSE,
    boxplot = FALSE,
    point = TRUE
  )
  expect_s3_class(result_points, "Group")
})

test_that("jjwithinstats handles theme options", {
  test_data <- data.frame(
    variable1 = c(80, 85, 78, 82, 87, 75, 83, 79, 86, 81),
    variable2 = c(70, 75, 68, 72, 77, 65, 73, 69, 76, 71)
  )
  
  # Test with original ggstatsplot theme
  result_original <- jjwithinstats(
    data = test_data,
    dep1 = "variable1",
    dep2 = "variable2",
    originaltheme = TRUE
  )
  expect_s3_class(result_original, "Group")
  
  # Test with custom theme
  result_custom <- jjwithinstats(
    data = test_data,
    dep1 = "variable1",
    dep2 = "variable2",
    originaltheme = FALSE
  )
  expect_s3_class(result_custom, "Group")
})

test_that("jjwithinstats handles results subtitle option", {
  test_data <- data.frame(
    pre_score = c(90, 88, 92, 85, 94, 87, 91, 89, 93, 86),
    post_score = c(80, 78, 82, 75, 84, 77, 81, 79, 83, 76)
  )
  
  # Test with results subtitle
  result_subtitle <- jjwithinstats(
    data = test_data,
    dep1 = "pre_score",
    dep2 = "post_score",
    resultssubtitle = TRUE
  )
  expect_s3_class(result_subtitle, "Group")
  
  # Test without results subtitle
  result_no_subtitle <- jjwithinstats(
    data = test_data,
    dep1 = "pre_score",
    dep2 = "post_score",
    resultssubtitle = FALSE
  )
  expect_s3_class(result_no_subtitle, "Group")
})

test_that("jjwithinstats handles edge cases", {
  test_data <- data.frame(
    var1 = c(50, 55, 48, 52, 58, 45, 51, 49, 56, 54),
    var2 = c(45, 48, 40, 47, 52, 38, 46, 42, 50, 49)
  )
  
  # Test with missing first measurement (should return early)
  result_no_dep1 <- jjwithinstats(
    data = test_data,
    dep1 = NULL,
    dep2 = "var2"
  )
  expect_s3_class(result_no_dep1, "Group")
  
  # Test with missing second measurement (should return early)
  result_no_dep2 <- jjwithinstats(
    data = test_data,
    dep1 = "var1",
    dep2 = NULL
  )
  expect_s3_class(result_no_dep2, "Group")
})

test_that("jjwithinstats handles missing values", {
  # Create data with missing values
  test_data <- data.frame(
    measurement_a = c(70, 75, NA, 72, 78, 65, 73, 69, 76, 71),
    measurement_b = c(60, NA, 58, 62, 68, 55, 63, 59, 66, 61)
  )
  
  result <- jjwithinstats(
    data = test_data,
    dep1 = "measurement_a",
    dep2 = "measurement_b",
    typestatistics = "nonparametric"
  )
  
  expect_s3_class(result, "Group")
})

test_that("jjwithinstats validates input parameters", {
  # Test with empty data
  expect_error(
    jjwithinstats(
      data = data.frame(),
      dep1 = "var1",
      dep2 = "var2"
    ),
    "Argument 'dep1' contains 'var1' which is not present in the dataset"
  )
})

test_that("jjwithinstats performance optimization works", {
  # Test that cached data is being used efficiently
  test_data <- data.frame(
    pre_test = runif(50, 70, 100),
    mid_test = runif(50, 65, 95),
    post_test = runif(50, 60, 90)
  )
  
  # Create a class instance to test caching
  options <- ClinicoPath:::jjwithinstatsOptions$new(
    dep1 = "pre_test",
    dep2 = "mid_test",
    dep3 = "post_test"
  )
  
  analysis <- ClinicoPath:::jjwithinstatsClass$new(
    options = options,
    data = test_data
  )
  
  # Test that optimization methods exist
  expect_true(exists(".prepareData", envir = analysis$.__enclos_env__$private))
  expect_true(exists(".prepareOptions", envir = analysis$.__enclos_env__$private))
})

test_that("jjwithinstats works with clinical data", {
  # Clinical trial repeated measures example
  clinical_data <- data.frame(
    baseline_pain = c(8, 9, 7, 8, 9, 8, 7, 9, 8, 7, 8, 9, 7, 8, 9),
    week_2_pain = c(6, 7, 5, 6, 7, 6, 5, 7, 6, 5, 6, 7, 5, 6, 7),
    week_4_pain = c(4, 5, 3, 4, 5, 4, 3, 5, 4, 3, 4, 5, 3, 4, 5),
    week_8_pain = c(2, 3, 1, 2, 3, 2, 1, 3, 2, 1, 2, 3, 1, 2, 3)
  )
  
  result <- jjwithinstats(
    data = clinical_data,
    dep1 = "baseline_pain",
    dep2 = "week_2_pain",
    dep3 = "week_4_pain",
    dep4 = "week_8_pain",
    typestatistics = "nonparametric",
    pairwisecomparisons = TRUE,
    mytitle = "Pain Reduction Over Time",
    xtitle = "Time Points",
    ytitle = "Pain Score (0-10)"
  )
  
  expect_s3_class(result, "Group")
})

test_that("jjwithinstats handles large datasets efficiently", {
  # Test with larger dataset to check performance optimizations
  large_data <- data.frame(
    time_1 = runif(200, 80, 120),
    time_2 = runif(200, 75, 115),
    time_3 = runif(200, 70, 110)
  )
  
  # This should complete without significant delay due to optimizations
  start_time <- Sys.time()
  result <- jjwithinstats(
    data = large_data,
    dep1 = "time_1",
    dep2 = "time_2",
    dep3 = "time_3",
    typestatistics = "parametric"
  )
  end_time <- Sys.time()
  
  expect_s3_class(result, "Group")
  # Performance test - should complete reasonably quickly
  expect_lt(as.numeric(end_time - start_time), 30) # Less than 30 seconds
})

test_that("jjwithinstats handles identical values", {
  # Test with identical values across time points
  identical_data <- data.frame(
    time1 = rep(50, 10),
    time2 = rep(50, 10)
  )
  
  # Should handle gracefully without errors
  result <- jjwithinstats(
    data = identical_data,
    dep1 = "time1",
    dep2 = "time2",
    typestatistics = "nonparametric"
  )
  
  expect_s3_class(result, "Group")
})

test_that("jjwithinstats handles extreme values", {
  # Test with extreme values
  extreme_data <- data.frame(
    low_values = c(0.001, 0.002, 0.001, 0.003, 0.002),
    high_values = c(1000000, 999999, 1000001, 999998, 1000002)
  )
  
  result <- jjwithinstats(
    data = extreme_data,
    dep1 = "low_values",
    dep2 = "high_values",
    typestatistics = "robust"
  )
  
  expect_s3_class(result, "Group")
})

test_that("jjwithinstats works with minimal data", {
  # Test with minimal viable dataset
  minimal_data <- data.frame(
    pre = c(10, 15),
    post = c(8, 12)
  )
  
  result <- jjwithinstats(
    data = minimal_data,
    dep1 = "pre",
    dep2 = "post",
    typestatistics = "nonparametric",
    pairwisecomparisons = FALSE
  )
  
  expect_s3_class(result, "Group")
})