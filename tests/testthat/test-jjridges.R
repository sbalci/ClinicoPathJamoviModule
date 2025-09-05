test_that("jjridges works with basic ridgeline plot", {
  # Load test data with continuous and categorical variables
  data(iris)
  
  # Test basic functionality
  result <- jjridges(
    data = iris,
    dep = "Sepal.Length",
    group = "Species",
    plotStyle = "density"
  )
  
  expect_s3_class(result, "Group")
  expect_true("plot" %in% names(result))
  expect_true("todo" %in% names(result))
})

test_that("jjridges works with different plot styles", {
  data(iris)
  
  # Test density plot
  result_density <- jjridges(
    data = iris,
    dep = "Sepal.Length",
    group = "Species",
    plotStyle = "density"
  )
  
  expect_s3_class(result_density, "Group")
  
  # Test histogram plot
  result_histogram <- jjridges(
    data = iris,
    dep = "Sepal.Length", 
    group = "Species",
    plotStyle = "histogram"
  )
  
  expect_s3_class(result_histogram, "Group")
  
  # Test gradient plot
  result_gradient <- jjridges(
    data = iris,
    dep = "Sepal.Length",
    group = "Species", 
    plotStyle = "gradient"
  )
  
  expect_s3_class(result_gradient, "Group")
})

test_that("jjridges handles customization options", {
  data(iris)
  
  # Test with custom scaling and bandwidth
  result <- jjridges(
    data = iris,
    dep = "Sepal.Length",
    group = "Species",
    plotStyle = "density",
    scaling = 2.0,
    bandwidth = 0.5,
    fill = TRUE
  )
  
  expect_s3_class(result, "Group")
})

test_that("jjridges works with different color schemes", {
  data(iris)
  
  color_schemes <- c("viridis", "plasma", "magma", "blues", "custom")
  
  for (scheme in color_schemes) {
    result <- jjridges(
      data = iris,
      dep = "Sepal.Length",
      group = "Species",
      colorscheme = scheme,
      customColor = "#FF5733"
    )
    
    expect_s3_class(result, "Group")
  }
})

test_that("jjridges works with different themes", {
  data(iris)
  
  themes <- c("minimal", "classic", "dark")
  
  for (theme in themes) {
    result <- jjridges(
      data = iris,
      dep = "Sepal.Length",
      group = "Species",
      themeChoice = theme
    )
    
    expect_s3_class(result, "Group")
  }
})

test_that("jjridges handles legend positioning", {
  data(iris)
  
  legend_positions <- c("none", "right", "bottom")
  
  for (pos in legend_positions) {
    result <- jjridges(
      data = iris,
      dep = "Sepal.Length",
      group = "Species",
      legendPosition = pos,
      fill = TRUE
    )
    
    expect_s3_class(result, "Group")
  }
})

test_that("jjridges handles custom titles", {
  data(iris)
  
  # Test with custom titles
  result <- jjridges(
    data = iris,
    dep = "Sepal.Length",
    group = "Species",
    mytitle = "Distribution of Sepal Length by Species",
    xtitle = "Sepal Length (cm)",
    ytitle = "Species"
  )
  
  expect_s3_class(result, "Group")
})

test_that("jjridges handles edge cases", {
  data(iris)
  
  # Test with no dep (should return early)
  result_no_dep <- jjridges(
    data = iris,
    dep = NULL,
    group = "Species"
  )
  
  expect_s3_class(result_no_dep, "Group")
  
  # Test with no group (should return early)
  result_no_group <- jjridges(
    data = iris,
    dep = "Sepal.Length",
    group = NULL
  )
  
  expect_s3_class(result_no_group, "Group")
  
  # Test with missing values
  iris_na <- iris
  iris_na[1:5, "Sepal.Length"] <- NA
  
  result_na <- jjridges(
    data = iris_na,
    dep = "Sepal.Length",
    group = "Species"
  )
  
  expect_s3_class(result_na, "Group")
})

test_that("jjridges validates input parameters", {
  data(iris)
  
  # Test with empty data
  expect_error(
    jjridges(
      data = data.frame(),
      dep = "Sepal.Length",
      group = "Species"
    ),
    "Data contains no \\\\(complete\\\\) rows"
  )
})

test_that("jjridges performance optimization works", {
  # Test that cached data is being used efficiently
  data(iris)
  
  # Create a class instance to test caching
  options <- ClinicoPath:::jjridgesOptions$new(
    dep = "Sepal.Length",
    group = "Species"
  )
  
  analysis <- ClinicoPath:::jjridgesClass$new(
    options = options,
    data = iris
  )
  
  # Test that methods exist
  expect_true(exists(".prepareData", envir = analysis$.__enclos_env__$private))
  expect_true(exists(".prepareOptions", envir = analysis$.__enclos_env__$private))
})

test_that("jjridges works with histogram parameters", {
  data(iris)
  
  # Test histogram with different bin widths
  result <- jjridges(
    data = iris,
    dep = "Sepal.Length",
    group = "Species",
    plotStyle = "histogram",
    binwidth = 0.2
  )
  
  expect_s3_class(result, "Group")
})

test_that("jjridges works with fill options", {
  data(iris)
  
  # Test with fill enabled
  result_fill <- jjridges(
    data = iris,
    dep = "Sepal.Length",
    group = "Species",
    fill = TRUE
  )
  
  expect_s3_class(result_fill, "Group")
  
  # Test with fill disabled
  result_no_fill <- jjridges(
    data = iris,
    dep = "Sepal.Length",
    group = "Species",
    fill = FALSE
  )
  
  expect_s3_class(result_no_fill, "Group")
})

test_that("jjridges handles complex datasets", {
  # Create a more complex test dataset
  test_data <- data.frame(
    values = c(rnorm(50, 10, 2), rnorm(50, 15, 3), rnorm(50, 12, 1.5)),
    groups = factor(rep(c("Group A", "Group B", "Group C"), each = 50)),
    stringsAsFactors = TRUE
  )
  
  # Test with complex data
  result <- jjridges(
    data = test_data,
    dep = "values",
    group = "groups",
    plotStyle = "density",
    scaling = 1.5,
    bandwidth = 0.8
  )
  
  expect_s3_class(result, "Group")
})

test_that("jjridges works with many groups", {
  # Create dataset with many groups
  test_data <- data.frame(
    values = rnorm(200),
    groups = factor(rep(paste("Group", 1:8), each = 25))
  )
  
  # Test with many groups
  result <- jjridges(
    data = test_data,
    dep = "values",
    group = "groups",
    plotStyle = "density"
  )
  
  expect_s3_class(result, "Group")
})

test_that("jjridges handles extreme parameter values", {
  data(iris)
  
  # Test with extreme scaling
  result_extreme <- jjridges(
    data = iris,
    dep = "Sepal.Length",
    group = "Species",
    scaling = 5.0,
    bandwidth = 0.1
  )
  
  expect_s3_class(result_extreme, "Group")
})