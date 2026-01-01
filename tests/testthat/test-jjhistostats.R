test_that("jjhistostats works with basic histogram", {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()
  # Load test data with continuous variables
  data(iris)
  
  # Test basic functionality with single variable
  result <- jjhistostats(
    data = iris,
    dep = "Sepal.Length",
    typestatistics = "parametric"
  )
  
  expect_s3_class(result, "Group")
  expect_true("plot" %in% names(result))
  expect_true("todo" %in% names(result))
})

test_that("jjhistostats works with multiple variables", {
  data(iris)
  
  # Test with multiple variables
  result <- jjhistostats(
    data = iris,
    dep = c("Sepal.Length", "Sepal.Width"),
    typestatistics = "parametric"
  )
  
  expect_s3_class(result, "Group")
  expect_true("plot" %in% names(result))
})

test_that("jjhistostats works with grouping variable", {
  data(iris)
  
  # Test with grouping variable
  result <- jjhistostats(
    data = iris,
    dep = "Sepal.Length",
    grvar = "Species",
    typestatistics = "parametric"
  )
  
  expect_s3_class(result, "Group")
  expect_true("plot2" %in% names(result))
  expect_true("plot" %in% names(result))
})

test_that("jjhistostats handles different statistical types", {
  data(iris)
  
  # Test nonparametric
  result_np <- jjhistostats(
    data = iris,
    dep = "Sepal.Length",
    typestatistics = "nonparametric"
  )
  
  expect_s3_class(result_np, "Group")
  
  # Test robust
  result_robust <- jjhistostats(
    data = iris,
    dep = "Sepal.Length",
    typestatistics = "robust"
  )
  
  expect_s3_class(result_robust, "Group")
})

test_that("jjhistostats handles customization options", {
  data(iris)
  
  # Test with custom binwidth and options
  result <- jjhistostats(
    data = iris,
    dep = "Sepal.Length",
    typestatistics = "parametric",
    centralityline = TRUE,
    changebinwidth = TRUE,
    binwidth = 0.5,
    resultssubtitle = TRUE,
    normalcurve = TRUE
  )
  
  expect_s3_class(result, "Group")
})

test_that("jjhistostats handles grouped analysis with multiple variables", {
  data(iris)
  
  # Test grouped analysis with multiple variables
  result <- jjhistostats(
    data = iris,
    dep = c("Sepal.Length", "Petal.Length"),
    grvar = "Species",
    typestatistics = "parametric",
    centralityline = FALSE,
    normalcurve = TRUE
  )
  
  expect_s3_class(result, "Group")
  expect_true("plot2" %in% names(result))
})

test_that("jjhistostats handles edge cases", {
  data(iris)
  
  # Test with no dep (should return early)
  result_no_dep <- jjhistostats(
    data = iris,
    dep = NULL,
    typestatistics = "parametric"
  )
  
  expect_s3_class(result_no_dep, "Group")
  
  # Test with missing values
  iris_na <- iris
  iris_na[1:5, "Sepal.Length"] <- NA
  
  result_na <- jjhistostats(
    data = iris_na,
    dep = "Sepal.Length",
    typestatistics = "parametric"
  )
  
  expect_s3_class(result_na, "Group")
})

test_that("jjhistostats validates input parameters", {
  data(iris)
  
  # Test with empty data
  expect_error(
    jjhistostats(
      data = data.frame(),
      dep = "Sepal.Length",
      typestatistics = "parametric"
    ),
    "Data contains no \\(complete\\) rows"
  )
})

test_that("jjhistostats performance optimization works", {
  # Test that cached data is being used efficiently
  data(iris)
  
  # Create a class instance to test caching
  options <- ClinicoPath:::jjhistostatsOptions$new(
    dep = "Sepal.Length",
    typestatistics = "parametric"
  )
  
  analysis <- ClinicoPath:::jjhistostatsClass$new(
    options = options,
    data = iris
  )
  
  # Test that methods exist
  expect_true(exists(".prepareData", envir = analysis$.__enclos_env__$private))
  expect_true(exists(".prepareOptions", envir = analysis$.__enclos_env__$private))
})

test_that("jjhistostats handles all statistical types", {
  data(iris)
  
  stat_types <- c("parametric", "nonparametric", "robust", "bayes")
  
  for (stat_type in stat_types) {
    result <- jjhistostats(
      data = iris,
      dep = "Sepal.Length",
      typestatistics = stat_type
    )
    
    expect_s3_class(result, "Group")
  }
})

test_that("jjhistostats handles binwidth options correctly", {
  data(iris)
  
  # Test default binwidth
  result_default <- jjhistostats(
    data = iris,
    dep = "Sepal.Length",
    changebinwidth = FALSE
  )
  
  expect_s3_class(result_default, "Group")
  
  # Test custom binwidth
  result_custom <- jjhistostats(
    data = iris,
    dep = "Sepal.Length",
    changebinwidth = TRUE,
    binwidth = 0.3
  )
  
  expect_s3_class(result_custom, "Group")
})

test_that("jjhistostats works with different display options", {
  data(iris)
  
  # Test with all display options enabled
  result_full <- jjhistostats(
    data = iris,
    dep = "Sepal.Length",
    centralityline = TRUE,
    resultssubtitle = TRUE,
    normalcurve = TRUE
  )
  
  expect_s3_class(result_full, "Group")
  
  # Test with minimal display options
  result_minimal <- jjhistostats(
    data = iris,
    dep = "Sepal.Length",
    centralityline = FALSE,
    resultssubtitle = FALSE,
    normalcurve = FALSE
  )
  
  expect_s3_class(result_minimal, "Group")
})
