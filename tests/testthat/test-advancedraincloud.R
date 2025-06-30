test_that("advancedraincloud module loads correctly", {
  expect_true(exists("advancedraincloudClass"))
  expect_true(is.function(advancedraincloud))
})

test_that("advancedraincloud handles basic input validation", {
  # Test with missing required variables
  expect_error(
    advancedraincloud(data = histopathology, y_var = NULL, x_var = "Group"),
    NA  # Should not error during initialization, only during run
  )
  
  expect_error(
    advancedraincloud(data = histopathology, y_var = "Age", x_var = NULL),
    NA  # Should not error during initialization, only during run
  )
})

test_that("advancedraincloud works with valid inputs", {
  # Test basic functionality
  result <- advancedraincloud(
    data = histopathology,
    y_var = "Age",
    x_var = "Group"
  )
  
  expect_s3_class(result, "advancedraincloudClass")
  expect_true("Group" %in% names(histopathology))
  expect_true("Age" %in% names(histopathology))
})

test_that("advancedraincloud handles optional variables correctly", {
  # Test with fill variable
  expect_error({
    result <- advancedraincloud(
      data = histopathology,
      y_var = "Age",
      x_var = "Group",
      fill_var = "Sex"
    )
  }, NA)
  
  # Test with ID variable for longitudinal connections
  expect_error({
    result <- advancedraincloud(
      data = histopathology,
      y_var = "Age",
      x_var = "Group",
      id_var = "ID"
    )
  }, NA)
  
  # Test with covariate variable
  expect_error({
    result <- advancedraincloud(
      data = histopathology,
      y_var = "OverallTime",
      x_var = "Group",
      cov_var = "Age"
    )
  }, NA)
})

test_that("advancedraincloud rain positioning works correctly", {
  # Test different rain side positions
  positions <- c("l", "r", "f")
  
  for (position in positions) {
    expect_error({
      result <- advancedraincloud(
        data = histopathology,
        y_var = "Age",
        x_var = "Group",
        rain_side = position
      )
    }, NA, info = paste("rain_side:", position))
  }
})

test_that("advancedraincloud color palettes work correctly", {
  # Test different color palettes
  palettes <- c("clinical", "viridis", "set1", "set2", "pastel", "dark2")
  
  for (palette in palettes) {
    expect_error({
      result <- advancedraincloud(
        data = histopathology,
        y_var = "Age",
        x_var = "Group",
        color_palette = palette
      )
    }, NA, info = paste("color_palette:", palette))
  }
})

test_that("advancedraincloud advanced features work correctly", {
  # Test Likert mode
  expect_error({
    result <- advancedraincloud(
      data = histopathology,
      y_var = "Grade",  # Discrete variable
      x_var = "Group",
      likert_mode = TRUE
    )
  }, NA)
  
  # Test longitudinal connections
  expect_error({
    result <- advancedraincloud(
      data = histopathology,
      y_var = "Age",
      x_var = "Group",
      id_var = "ID",
      show_longitudinal = TRUE
    )
  }, NA)
})

test_that("advancedraincloud statistical options work correctly", {
  # Test with statistics enabled
  expect_error({
    result <- advancedraincloud(
      data = histopathology,
      y_var = "Age",
      x_var = "Group",
      show_statistics = TRUE
    )
  }, NA)
  
  # Test with group comparisons
  expect_error({
    result <- advancedraincloud(
      data = histopathology,
      y_var = "Age",
      x_var = "Group",
      show_comparisons = TRUE
    )
  }, NA)
  
  # Test with interpretation guide
  expect_error({
    result <- advancedraincloud(
      data = histopathology,
      y_var = "Age",
      x_var = "Group",
      show_interpretation = TRUE
    )
  }, NA)
})

test_that("advancedraincloud customization parameters work correctly", {
  # Test numeric parameter validation
  expect_error({
    result <- advancedraincloud(
      data = histopathology,
      y_var = "Age",
      x_var = "Group",
      point_size = 2.0,
      point_alpha = 0.8,
      violin_alpha = 0.6,
      boxplot_width = 0.15
    )
  }, NA)
  
  # Test boundary values
  expect_error({
    result <- advancedraincloud(
      data = histopathology,
      y_var = "Age",
      x_var = "Group",
      point_size = 0.1,    # Minimum
      point_alpha = 1.0,   # Maximum
      violin_alpha = 0.0,  # Minimum
      boxplot_width = 1.0  # Maximum
    )
  }, NA)
})

test_that("advancedraincloud handles missing data appropriately", {
  # Create dataset with missing values
  test_data <- histopathology
  test_data$Age[1:5] <- NA
  test_data$Group[6:10] <- NA
  
  expect_error({
    result <- advancedraincloud(
      data = test_data,
      y_var = "Age",
      x_var = "Group"
    )
  }, NA)
})

test_that("advancedraincloud handles different variable types", {
  # Test with numeric y variable
  expect_error({
    result <- advancedraincloud(
      data = histopathology,
      y_var = "OverallTime",  # Numeric variable
      x_var = "Group"
    )
  }, NA)
  
  # Test with factor x variable
  expect_error({
    test_data <- histopathology
    test_data$Group <- factor(test_data$Group)
    result <- advancedraincloud(
      data = test_data,
      y_var = "Age",
      x_var = "Group"
    )
  }, NA)
})

test_that("advancedraincloud complex combinations work", {
  # Test complex parameter combinations
  expect_error({
    result <- advancedraincloud(
      data = histopathology,
      y_var = "OverallTime",
      x_var = "Group",
      fill_var = "Sex",
      id_var = "ID",
      cov_var = "Age",
      rain_side = "f",
      likert_mode = FALSE,
      show_longitudinal = TRUE,
      point_size = 1.8,
      point_alpha = 0.7,
      violin_alpha = 0.6,
      boxplot_width = 0.12,
      color_palette = "clinical",
      show_statistics = TRUE,
      show_comparisons = TRUE,
      show_interpretation = TRUE
    )
  }, NA)
})

test_that("advancedraincloud ggrain dependency handling", {
  # Test function behavior when ggrain is available
  if (requireNamespace("ggrain", quietly = TRUE)) {
    expect_error({
      result <- advancedraincloud(
        data = histopathology,
        y_var = "Age",
        x_var = "Group"
      )
    }, NA)
  } else {
    # Skip test if ggrain not available
    skip("ggrain package not available")
  }
})

test_that("advancedraincloud handles small datasets", {
  # Test with small dataset
  small_data <- histopathology[1:10, ]
  
  expect_error({
    result <- advancedraincloud(
      data = small_data,
      y_var = "Age",
      x_var = "Group"
    )
  }, NA)
})

test_that("advancedraincloud works with different continuous variables", {
  # Test with different continuous variables from histopathology
  continuous_vars <- c("Age", "OverallTime", "Grade", "MeasurementA", "MeasurementB")
  
  for (var in continuous_vars) {
    if (var %in% names(histopathology)) {
      expect_error({
        result <- advancedraincloud(
          data = histopathology,
          y_var = var,
          x_var = "Group"
        )
      }, NA, info = paste("y_var:", var))
    }
  }
})

test_that("advancedraincloud works with different grouping variables", {
  # Test with different categorical variables
  categorical_vars <- c("Group", "Sex", "Grade_Level", "Race")
  
  for (var in categorical_vars) {
    if (var %in% names(histopathology)) {
      expect_error({
        result <- advancedraincloud(
          data = histopathology,
          y_var = "Age",
          x_var = var
        )
      }, NA, info = paste("x_var:", var))
    }
  }
})

test_that("advancedraincloud plot customization works", {
  # Test plot title and axis labels
  expect_error({
    result <- advancedraincloud(
      data = histopathology,
      y_var = "Age",
      x_var = "Group",
      plot_title = "Custom Title",
      x_label = "Custom X Label",
      y_label = "Custom Y Label"
    )
  }, NA)
})

test_that("advancedraincloud vs standard raincloud compatibility", {
  # Test that advanced raincloud works with similar data as standard raincloud
  expect_error({
    result <- advancedraincloud(
      data = histopathology,
      y_var = "Age",        # Similar to dep_var in standard raincloud
      x_var = "Group"       # Similar to group_var in standard raincloud
    )
  }, NA)
  
  # Test that results object has expected structure
  result <- advancedraincloud(
    data = histopathology,
    y_var = "Age",
    x_var = "Group"
  )
  
  expect_true(exists("plot", envir = result))
  expect_true(exists("statistics", envir = result))
  expect_true(exists("interpretation", envir = result))
})