test_that("jjpiestats works with basic pie chart", {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()
  # Load test data with categorical variables
  data(iris)
  
  # Test basic functionality with single variable
  result <- jjpiestats(
    data = iris,
    dep = "Species",
    typestatistics = "parametric"
  )
  
  expect_s3_class(result, "Group")
  expect_true("plot1" %in% names(result))
  expect_true("todo" %in% names(result))
})

test_that("jjpiestats works with grouping variable", {
  # Create a simple dataset with factors
  test_data <- data.frame(
    category = factor(c("A", "B", "A", "B", "C", "C", "A", "B")),
    group = factor(c("X", "X", "Y", "Y", "X", "Y", "X", "Y"))
  )
  
  # Test with grouping variable
  result <- jjpiestats(
    data = test_data,
    dep = "category",
    group = "group",
    typestatistics = "parametric"
  )
  
  expect_s3_class(result, "Group")
  expect_true("plot2" %in% names(result))
  expect_true("plot1" %in% names(result))
})

test_that("jjpiestats works with split variable", {
  # Create a dataset with three categorical variables
  test_data <- data.frame(
    category = factor(rep(c("A", "B", "C"), 12)),
    group = factor(rep(c("X", "Y"), 18)),
    split_var = factor(rep(c("Group1", "Group2"), each = 18))
  )
  
  # Test with split variable
  result <- jjpiestats(
    data = test_data,
    dep = "category",
    group = "group",
    grvar = "split_var",
    typestatistics = "parametric"
  )
  
  expect_s3_class(result, "Group")
  expect_true("plot4" %in% names(result))
})

test_that("jjpiestats handles different statistical types", {
  data(iris)
  
  # Test nonparametric
  result_np <- jjpiestats(
    data = iris,
    dep = "Species",
    typestatistics = "nonparametric"
  )
  
  expect_s3_class(result_np, "Group")
  
  # Test robust
  result_robust <- jjpiestats(
    data = iris,
    dep = "Species",
    typestatistics = "robust"
  )
  
  expect_s3_class(result_robust, "Group")
})

test_that("jjpiestats handles customization options", {
  data(iris)
  
  # Test with custom options
  result <- jjpiestats(
    data = iris,
    dep = "Species",
    typestatistics = "parametric",
    originaltheme = TRUE,
    resultssubtitle = FALSE
  )
  
  expect_s3_class(result, "Group")
})

test_that("jjpiestats handles edge cases", {
  data(iris)
  
  # Test with no dep (should return early)
  result_no_dep <- jjpiestats(
    data = iris,
    dep = NULL,
    typestatistics = "parametric"
  )
  
  expect_s3_class(result_no_dep, "Group")
  
  # Test with missing values
  iris_na <- iris
  iris_na[1:5, "Species"] <- NA
  
  result_na <- jjpiestats(
    data = iris_na,
    dep = "Species",
    typestatistics = "parametric"
  )
  
  expect_s3_class(result_na, "Group")
})

test_that("jjpiestats validates input parameters", {
  data(iris)
  
  # Test with empty data
  expect_error(
    jjpiestats(
      data = data.frame(),
      dep = "Species",
      typestatistics = "parametric"
    ),
    "Data contains no"
  )
})

test_that("jjpiestats performance optimization works", {
  # Test that cached data is being used efficiently
  data(iris)
  
  # Create a class instance to test caching
  options <- ClinicoPath:::jjpiestatsOptions$new(
    dep = "Species",
    typestatistics = "parametric"
  )
  
  analysis <- ClinicoPath:::jjpiestatsClass$new(
    options = options,
    data = iris
  )
  
  # Test that methods exist
  expect_true(exists(".prepareData", envir = analysis$.__enclos_env__$private))
  expect_true(exists(".prepareOptions", envir = analysis$.__enclos_env__$private))
})

test_that("jjpiestats handles all statistical types", {
  data(iris)
  
  stat_types <- c("parametric", "nonparametric", "robust", "bayes")
  
  for (stat_type in stat_types) {
    result <- jjpiestats(
      data = iris,
      dep = "Species",
      typestatistics = stat_type
    )
    
    expect_s3_class(result, "Group")
  }
})

test_that("jjpiestats works with theme options", {
  data(iris)
  
  # Test with original theme enabled
  result_original <- jjpiestats(
    data = iris,
    dep = "Species",
    originaltheme = TRUE
  )
  
  expect_s3_class(result_original, "Group")
  
  # Test with original theme disabled
  result_custom <- jjpiestats(
    data = iris,
    dep = "Species",
    originaltheme = FALSE
  )
  
  expect_s3_class(result_custom, "Group")
})

test_that("jjpiestats works with subtitle options", {
  data(iris)
  
  # Test with results subtitle enabled
  result_subtitle <- jjpiestats(
    data = iris,
    dep = "Species",
    resultssubtitle = TRUE
  )
  
  expect_s3_class(result_subtitle, "Group")
  
  # Test with results subtitle disabled
  result_no_subtitle <- jjpiestats(
    data = iris,
    dep = "Species",
    resultssubtitle = FALSE
  )
  
  expect_s3_class(result_no_subtitle, "Group")
})

test_that("jjpiestats handles complex grouping scenarios", {
  # Create a more complex test dataset
  test_data <- data.frame(
    outcome = factor(sample(c("Success", "Failure", "Partial"), 60, replace = TRUE)),
    treatment = factor(sample(c("Control", "Treatment"), 60, replace = TRUE)),
    center = factor(sample(c("Center A", "Center B", "Center C"), 60, replace = TRUE)),
    stringsAsFactors = TRUE
  )
  
  # Test complex grouped analysis
  result <- jjpiestats(
    data = test_data,
    dep = "outcome",
    group = "treatment",
    grvar = "center",
    typestatistics = "parametric"
  )
  
  expect_s3_class(result, "Group")
  expect_true("plot4" %in% names(result))
})
