test_that("jjcorrmat works with basic correlation matrix", {
  # Load test data with multiple continuous variables
  data(iris)
  
  # Test basic functionality
  result <- jjcorrmat(
    data = iris,
    dep = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"),
    typestatistics = "parametric"
  )
  
  expect_s3_class(result, "Group")
  expect_true("plot" %in% names(result))
  expect_true("todo" %in% names(result))
})

test_that("jjcorrmat works with grouping variable", {
  # Load test data
  data(iris)
  
  # Test with grouping variable
  result <- jjcorrmat(
    data = iris,
    dep = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"),
    grvar = "Species",
    typestatistics = "parametric"
  )
  
  expect_s3_class(result, "Group")
  expect_true("plot2" %in% names(result))
  expect_true("plot" %in% names(result))
})

test_that("jjcorrmat handles different statistical types", {
  data(iris)
  
  # Test nonparametric
  result_np <- jjcorrmat(
    data = iris,
    dep = c("Sepal.Length", "Sepal.Width"),
    typestatistics = "nonparametric"
  )
  
  expect_s3_class(result_np, "Group")
  
  # Test robust
  result_robust <- jjcorrmat(
    data = iris,
    dep = c("Sepal.Length", "Sepal.Width"),
    typestatistics = "robust"
  )
  
  expect_s3_class(result_robust, "Group")
})

test_that("jjcorrmat handles edge cases", {
  data(iris)
  
  # Test with single variable (should return early)
  result_single <- jjcorrmat(
    data = iris,
    dep = "Sepal.Length",
    typestatistics = "parametric"
  )
  
  expect_s3_class(result_single, "Group")
  
  # Test with missing values
  iris_na <- iris
  iris_na[1:5, "Sepal.Length"] <- NA
  
  result_na <- jjcorrmat(
    data = iris_na,
    dep = c("Sepal.Length", "Sepal.Width"),
    typestatistics = "parametric"
  )
  
  expect_s3_class(result_na, "Group")
})

test_that("jjcorrmat validates input parameters", {
  data(iris)
  
  # Test with empty data
  expect_error(
    jjcorrmat(
      data = data.frame(),
      dep = c("Sepal.Length", "Sepal.Width"),
      typestatistics = "parametric"
    ),
    "Data contains no \\(complete\\) rows"
  )
})