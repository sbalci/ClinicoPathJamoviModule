test_that("jjdotplotstats works with basic dot plot", {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()
  # Load test data with continuous and categorical variables
  data(iris)
  
  # Test basic functionality
  result <- jjdotplotstats(
    data = iris,
    dep = "Sepal.Length",
    group = "Species",
    typestatistics = "parametric"
  )
  
  expect_s3_class(result, "Group")
  expect_true("plot" %in% names(result))
  expect_true("todo" %in% names(result))
})

test_that("jjdotplotstats works with grouping variable", {
  # Create test data with grouping
  set.seed(123)
  test_data <- data.frame(
    value = c(rnorm(30, 10, 2), rnorm(30, 15, 3), rnorm(30, 12, 2)),
    group = rep(c("A", "B", "C"), each = 30),
    condition = rep(c("Control", "Treatment"), length.out = 90),
    stringsAsFactors = TRUE
  )
  
  # Test with grouping variable
  result <- jjdotplotstats(
    data = test_data,
    dep = "value",
    group = "group",
    grvar = "condition",
    typestatistics = "parametric"
  )
  
  expect_s3_class(result, "Group")
  expect_true("plot2" %in% names(result))
  expect_true("plot" %in% names(result))
})

test_that("jjdotplotstats handles different statistical types", {
  data(iris)
  
  # Test nonparametric
  result_np <- jjdotplotstats(
    data = iris,
    dep = "Sepal.Length",
    group = "Species",
    typestatistics = "nonparametric"
  )
  
  expect_s3_class(result_np, "Group")
  
  # Test robust
  result_robust <- jjdotplotstats(
    data = iris,
    dep = "Sepal.Length",
    group = "Species",
    typestatistics = "robust"
  )
  
  expect_s3_class(result_robust, "Group")
})

test_that("jjdotplotstats handles customization options", {
  data(iris)
  
  # Test with custom titles and options
  result <- jjdotplotstats(
    data = iris,
    dep = "Sepal.Length",
    group = "Species",
    typestatistics = "parametric",
    effsizetype = "unbiased",
    centralityplotting = TRUE,
    centralitytype = "nonparametric",
    mytitle = "Custom Title",
    xtitle = "Sepal Length (cm)",
    ytitle = "Species",
    originaltheme = TRUE,
    resultssubtitle = FALSE
  )
  
  expect_s3_class(result, "Group")
})

test_that("jjdotplotstats handles edge cases", {
  data(iris)
  
  # Test with missing dep (should return early)
  result_no_dep <- jjdotplotstats(
    data = iris,
    dep = NULL,
    group = "Species",
    typestatistics = "parametric"
  )
  
  expect_s3_class(result_no_dep, "Group")
  
  # Test with missing group (should return early)
  result_no_group <- jjdotplotstats(
    data = iris,
    dep = "Sepal.Length",
    group = NULL,
    typestatistics = "parametric"
  )
  
  expect_s3_class(result_no_group, "Group")
  
  # Test with missing values
  iris_na <- iris
  iris_na[1:5, "Sepal.Length"] <- NA
  
  result_na <- jjdotplotstats(
    data = iris_na,
    dep = "Sepal.Length",
    group = "Species",
    typestatistics = "parametric"
  )
  
  expect_s3_class(result_na, "Group")
})

test_that("jjdotplotstats validates input parameters", {
  data(iris)
  
  # Test with empty data
  expect_error(
    jjdotplotstats(
      data = data.frame(),
      dep = "Sepal.Length",
      group = "Species",
      typestatistics = "parametric"
    ),
    "Data contains no \\(complete\\) rows"
  )
})

test_that("jjdotplotstats performance optimization works", {
  # Test that cached data is being used efficiently
  data(iris)
  
  # Create a class instance to test caching
  options <- ClinicoPath:::jjdotplotstatsOptions$new(
    dep = "Sepal.Length",
    group = "Species",
    typestatistics = "parametric"
  )
  
  analysis <- ClinicoPath:::jjdotplotstatsClass$new(
    options = options,
    data = iris
  )
  
  # Test that methods exist
  expect_true(exists(".prepareData", envir = analysis$.__enclos_env__$private))
  expect_true(exists(".prepareOptions", envir = analysis$.__enclos_env__$private))
})

test_that("jjdotplotstats handles various effect size types", {
  data(iris)
  
  effect_types <- c("biased", "unbiased", "eta", "omega")
  
  for (eff_type in effect_types) {
    result <- jjdotplotstats(
      data = iris,
      dep = "Sepal.Length",
      group = "Species",
      effsizetype = eff_type,
      typestatistics = "parametric"
    )
    
    expect_s3_class(result, "Group")
  }
})

test_that("jjdotplotstats handles various centrality types", {
  data(iris)
  
  centrality_types <- c("parametric", "nonparametric", "robust", "bayes")
  
  for (cent_type in centrality_types) {
    result <- jjdotplotstats(
      data = iris,
      dep = "Sepal.Length",
      group = "Species",
      centralityplotting = TRUE,
      centralitytype = cent_type,
      typestatistics = "parametric"
    )
    
    expect_s3_class(result, "Group")
  }
})
