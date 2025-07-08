test_that("jjstreamgraph works with basic stream graph", {
  # Create test data for time series
  test_data <- data.frame(
    time = rep(1:10, 3),
    value = c(runif(10, 10, 20), runif(10, 15, 25), runif(10, 5, 15)),
    group = factor(rep(c("A", "B", "C"), each = 10))
  )
  
  # Test basic functionality
  result <- jjstreamgraph(
    data = test_data,
    timeVar = "time",
    valueVar = "value",
    groupVar = "group",
    offset = "silhouette",
    interpolate = "cardinal",
    palette = "Blues"
  )
  
  expect_s3_class(result, "Group")
  expect_true("StreamGraph" %in% names(result))
})

test_that("jjstreamgraph handles different offset types", {
  test_data <- data.frame(
    time = rep(1:5, 2),
    value = runif(10, 5, 15),
    group = factor(rep(c("Group1", "Group2"), each = 5))
  )
  
  offset_types <- c("silhouette", "zero", "expand")
  
  for (offset_type in offset_types) {
    result <- jjstreamgraph(
      data = test_data,
      timeVar = "time",
      valueVar = "value",
      groupVar = "group",
      offset = offset_type,
      interpolate = "cardinal",
      palette = "Blues"
    )
    
    expect_s3_class(result, "Group")
  }
})

test_that("jjstreamgraph handles different interpolation types", {
  test_data <- data.frame(
    time = rep(1:5, 2),
    value = runif(10, 5, 15),
    group = factor(rep(c("Group1", "Group2"), each = 5))
  )
  
  interpolate_types <- c("cardinal", "linear", "step")
  
  for (interpolate_type in interpolate_types) {
    result <- jjstreamgraph(
      data = test_data,
      timeVar = "time",
      valueVar = "value",
      groupVar = "group",
      offset = "silhouette",
      interpolate = interpolate_type,
      palette = "Blues"
    )
    
    expect_s3_class(result, "Group")
  }
})

test_that("jjstreamgraph handles different color palettes", {
  test_data <- data.frame(
    time = rep(1:5, 2),
    value = runif(10, 5, 15),
    group = factor(rep(c("Group1", "Group2"), each = 5))
  )
  
  palettes <- c("Blues", "Greens", "Reds", "Pastel1", "Set1")
  
  for (palette in palettes) {
    result <- jjstreamgraph(
      data = test_data,
      timeVar = "time",
      valueVar = "value",
      groupVar = "group",
      offset = "silhouette",
      interpolate = "cardinal",
      palette = palette
    )
    
    expect_s3_class(result, "Group")
  }
})

test_that("jjstreamgraph handles custom dimensions", {
  test_data <- data.frame(
    time = rep(1:5, 2),
    value = runif(10, 5, 15),
    group = factor(rep(c("Group1", "Group2"), each = 5))
  )
  
  # Test with custom width and height
  result <- jjstreamgraph(
    data = test_data,
    timeVar = "time",
    valueVar = "value",
    groupVar = "group",
    width = "1000px",
    height = "600px"
  )
  
  expect_s3_class(result, "Group")
})

test_that("jjstreamgraph handles edge cases", {
  test_data <- data.frame(
    time = rep(1:5, 2),
    value = runif(10, 5, 15),
    group = factor(rep(c("Group1", "Group2"), each = 5))
  )
  
  # Test with missing timeVar (should return early)
  result_no_time <- jjstreamgraph(
    data = test_data,
    timeVar = NULL,
    valueVar = "value",
    groupVar = "group"
  )
  
  expect_s3_class(result_no_time, "Group")
  
  # Test with missing valueVar (should return early)
  result_no_value <- jjstreamgraph(
    data = test_data,
    timeVar = "time",
    valueVar = NULL,
    groupVar = "group"
  )
  
  expect_s3_class(result_no_value, "Group")
  
  # Test with missing groupVar (should return early)
  result_no_group <- jjstreamgraph(
    data = test_data,
    timeVar = "time",
    valueVar = "value",
    groupVar = NULL
  )
  
  expect_s3_class(result_no_group, "Group")
})

test_that("jjstreamgraph handles missing values", {
  # Create data with missing values
  test_data <- data.frame(
    time = rep(1:10, 2),
    value = c(runif(8, 5, 15), NA, NA, runif(8, 10, 20), NA, NA),
    group = factor(rep(c("Group1", "Group2"), each = 10))
  )
  
  result <- jjstreamgraph(
    data = test_data,
    timeVar = "time",
    valueVar = "value",
    groupVar = "group"
  )
  
  expect_s3_class(result, "Group")
})

test_that("jjstreamgraph validates input parameters", {
  # Test with empty data
  expect_error(
    jjstreamgraph(
      data = data.frame(),
      timeVar = "time",
      valueVar = "value",
      groupVar = "group"
    ),
    "Argument 'timeVar' contains 'time' which is not present in the dataset"
  )
})

test_that("jjstreamgraph performance optimization works", {
  # Test that cached data is being used efficiently
  test_data <- data.frame(
    time = rep(1:5, 2),
    value = runif(10, 5, 15),
    group = factor(rep(c("Group1", "Group2"), each = 5))
  )
  
  # Create a class instance to test caching
  options <- ClinicoPath:::jjstreamgraphOptions$new(
    timeVar = "time",
    valueVar = "value",
    groupVar = "group"
  )
  
  analysis <- ClinicoPath:::jjstreamgraphClass$new(
    options = options,
    data = test_data
  )
  
  # Test that optimization methods exist
  expect_true(exists(".prepareData", envir = analysis$.__enclos_env__$private))
  expect_true(exists(".prepareOptions", envir = analysis$.__enclos_env__$private))
})

test_that("jjstreamgraph works with time series data", {
  # Create realistic time series data with numeric time
  test_data <- data.frame(
    time = rep(1:30, 3),  # Use numeric time instead of dates
    value = c(
      cumsum(rnorm(30, 1, 0.5)) + 10,  # Trend 1
      cumsum(rnorm(30, 0.5, 0.3)) + 5, # Trend 2
      cumsum(rnorm(30, 0.8, 0.4)) + 8  # Trend 3
    ),
    category = factor(rep(c("Product A", "Product B", "Product C"), each = 30))
  )
  
  result <- jjstreamgraph(
    data = test_data,
    timeVar = "time",
    valueVar = "value",
    groupVar = "category",
    offset = "zero",
    interpolate = "linear"
  )
  
  expect_s3_class(result, "Group")
})

test_that("jjstreamgraph works with multiple groups", {
  # Test with more groups
  test_data <- data.frame(
    time = rep(1:10, 5),
    value = runif(50, 1, 20),
    group = factor(rep(c("A", "B", "C", "D", "E"), each = 10))
  )
  
  result <- jjstreamgraph(
    data = test_data,
    timeVar = "time",
    valueVar = "value",
    groupVar = "group",
    palette = "Set1"
  )
  
  expect_s3_class(result, "Group")
})

test_that("jjstreamgraph handles numeric conversion", {
  # Test with numeric data that should work
  test_data <- data.frame(
    time = rep(1:5, 2),
    value = runif(10, 5, 15),
    group = factor(rep(c("Group1", "Group2"), each = 5))
  )
  
  result <- jjstreamgraph(
    data = test_data,
    timeVar = "time",
    valueVar = "value",
    groupVar = "group"
  )
  
  expect_s3_class(result, "Group")
})

test_that("jjstreamgraph handles large datasets efficiently", {
  # Test with larger dataset to check performance optimizations
  large_data <- data.frame(
    time = rep(1:100, 5),
    value = runif(500, 1, 50),
    group = factor(rep(c("A", "B", "C", "D", "E"), each = 100))
  )
  
  # This should complete without significant delay due to optimizations
  start_time <- Sys.time()
  result <- jjstreamgraph(
    data = large_data,
    timeVar = "time",
    valueVar = "value",
    groupVar = "group"
  )
  end_time <- Sys.time()
  
  expect_s3_class(result, "Group")
  # Performance test - should complete reasonably quickly
  expect_lt(as.numeric(end_time - start_time), 30) # Less than 30 seconds
})