test_that("jjscatterstats works with basic scatter plot", {
  # Load test data
  data(iris)
  
  # Test basic functionality
  result <- jjscatterstats(
    data = iris,
    dep = "Sepal.Length",
    group = "Petal.Length",
    grvar = NULL,
    typestatistics = "parametric"
  )
  
  expect_s3_class(result, "Group")
  expect_true("plot" %in% names(result))
  expect_true("todo" %in% names(result))
})

test_that("jjscatterstats works with grouping variable", {
  data(iris)
  
  # Test with grouping variable
  result <- jjscatterstats(
    data = iris,
    dep = "Sepal.Length",
    group = "Petal.Length",
    grvar = "Species",
    typestatistics = "parametric"
  )
  
  expect_s3_class(result, "Group")
  expect_true("plot2" %in% names(result))
  expect_true("plot" %in% names(result))
})

test_that("jjscatterstats handles different statistical types", {
  data(iris)
  
  stat_types <- c("parametric", "nonparametric", "robust", "bayes")
  
  for (stat_type in stat_types) {
    result <- jjscatterstats(
      data = iris,
      dep = "Sepal.Length",
      group = "Petal.Length",
      grvar = NULL,
      typestatistics = stat_type
    )
    
    expect_s3_class(result, "Group")
  }
})

test_that("jjscatterstats handles customization options", {
  data(iris)
  
  # Test with custom titles and options
  result <- jjscatterstats(
    data = iris,
    dep = "Sepal.Length",
    group = "Petal.Length",
    grvar = NULL,
    typestatistics = "parametric",
    mytitle = "Custom Title",
    xtitle = "Custom X Title",
    ytitle = "Custom Y Title",
    resultssubtitle = TRUE,
    originaltheme = FALSE
  )
  
  expect_s3_class(result, "Group")
})

test_that("jjscatterstats handles edge cases", {
  data(iris)
  
  # Test with no dep (should return early)
  result_no_dep <- jjscatterstats(
    data = iris,
    dep = NULL,
    group = "Petal.Length",
    grvar = NULL,
    typestatistics = "parametric"
  )
  
  expect_s3_class(result_no_dep, "Group")
  
  # Test with no group (should return early)
  result_no_group <- jjscatterstats(
    data = iris,
    dep = "Sepal.Length",
    group = NULL,
    grvar = NULL,
    typestatistics = "parametric"
  )
  
  expect_s3_class(result_no_group, "Group")
  
  # Test with missing values
  iris_na <- iris
  iris_na[1:5, "Sepal.Length"] <- NA
  iris_na[6:10, "Petal.Length"] <- NA
  
  result_na <- jjscatterstats(
    data = iris_na,
    dep = "Sepal.Length",
    group = "Petal.Length",
    grvar = NULL,
    typestatistics = "parametric"
  )
  
  expect_s3_class(result_na, "Group")
})

test_that("jjscatterstats validates input parameters", {
  data(iris)
  
  # Test with empty data
  expect_error(
    jjscatterstats(
      data = data.frame(),
      dep = "Sepal.Length",
      group = "Petal.Length",
      grvar = NULL,
      typestatistics = "parametric"
    ),
    "Argument 'dep' contains 'Sepal.Length' which is not present in the dataset"
  )
})

test_that("jjscatterstats performance optimization works", {
  # Test that cached data is being used efficiently
  data(iris)
  
  # Create a class instance to test caching
  options <- ClinicoPath:::jjscatterstatsOptions$new(
    dep = "Sepal.Length",
    group = "Petal.Length",
    typestatistics = "parametric"
  )
  
  analysis <- ClinicoPath:::jjscatterstatsClass$new(
    options = options,
    data = iris
  )
  
  # Test that optimization methods exist
  expect_true(exists(".prepareData", envir = analysis$.__enclos_env__$private))
  expect_true(exists(".prepareOptions", envir = analysis$.__enclos_env__$private))
})

test_that("jjscatterstats works with grouped analysis", {
  data(iris)
  
  # Test grouped analysis
  result <- jjscatterstats(
    data = iris,
    dep = "Sepal.Length",
    group = "Petal.Length",
    grvar = "Species",
    typestatistics = "parametric",
    resultssubtitle = TRUE
  )
  
  expect_s3_class(result, "Group")
  expect_true("plot2" %in% names(result))
})

test_that("jjscatterstats handles theme options", {
  data(iris)
  
  # Test with original theme
  result_original <- jjscatterstats(
    data = iris,
    dep = "Sepal.Length",
    group = "Petal.Length",
    grvar = NULL,
    originaltheme = TRUE
  )
  
  expect_s3_class(result_original, "Group")
  
  # Test with custom theme
  result_custom <- jjscatterstats(
    data = iris,
    dep = "Sepal.Length",
    group = "Petal.Length",
    grvar = NULL,
    originaltheme = FALSE
  )
  
  expect_s3_class(result_custom, "Group")
})

test_that("jjscatterstats handles results subtitle options", {
  data(iris)
  
  # Test with results subtitle enabled
  result_subtitle <- jjscatterstats(
    data = iris,
    dep = "Sepal.Length",
    group = "Petal.Length",
    grvar = NULL,
    resultssubtitle = TRUE
  )
  
  expect_s3_class(result_subtitle, "Group")
  
  # Test with results subtitle disabled
  result_no_subtitle <- jjscatterstats(
    data = iris,
    dep = "Sepal.Length",
    group = "Petal.Length",
    grvar = NULL,
    resultssubtitle = FALSE
  )
  
  expect_s3_class(result_no_subtitle, "Group")
})

test_that("jjscatterstats works with continuous variables", {
  # Create test data with continuous variables
  test_data <- data.frame(
    x_var = rnorm(100, 50, 10),
    y_var = rnorm(100, 25, 5),
    group_var = factor(rep(c("A", "B", "C"), length.out = 100))
  )
  
  result <- jjscatterstats(
    data = test_data,
    dep = "x_var",
    group = "y_var",
    grvar = "group_var",
    typestatistics = "parametric"
  )
  
  expect_s3_class(result, "Group")
})

test_that("jjscatterstats handles large datasets efficiently", {
  # Test with larger dataset to check performance optimizations
  large_data <- data.frame(
    x = rnorm(1000),
    y = rnorm(1000),
    group = factor(rep(c("A", "B", "C", "D"), length.out = 1000))
  )
  
  # This should complete without significant delay due to optimizations
  start_time <- Sys.time()
  result <- jjscatterstats(
    data = large_data,
    dep = "x",
    group = "y",
    grvar = "group",
    typestatistics = "parametric"
  )
  end_time <- Sys.time()
  
  expect_s3_class(result, "Group")
  # Performance test - should complete reasonably quickly
  expect_lt(as.numeric(end_time - start_time), 30) # Less than 30 seconds
})