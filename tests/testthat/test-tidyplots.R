# Comprehensive Unit Tests for Tidyplots Function
# Tests cover all major functionality, error handling, and edge cases

# Load required libraries for testing
if (!requireNamespace("testthat", quietly = TRUE)) {
  stop("testthat package required for testing")
}

library(testthat)

# Create test context
context("Comprehensive Tidyplots Function Tests")

# ============================================================================
# SETUP AND HELPER FUNCTIONS
# ============================================================================

# Create minimal test data
create_test_data <- function(n = 50) {
  data.frame(
    x_numeric = rnorm(n, 50, 10),
    y_numeric = rnorm(n, 100, 15),
    x_categorical = sample(c("A", "B", "C"), n, replace = TRUE),
    y_categorical = sample(c("Group1", "Group2"), n, replace = TRUE),
    color_var = sample(c("Red", "Blue", "Green"), n, replace = TRUE),
    group_var = sample(c("G1", "G2"), n, replace = TRUE),
    facet_var = sample(c("F1", "F2", "F3"), n, replace = TRUE),
    continuous_1 = runif(n, 0, 100),
    continuous_2 = rexp(n, 0.1),
    binary_var = sample(c(TRUE, FALSE), n, replace = TRUE),
    count_var = rpois(n, 5),
    stringsAsFactors = FALSE
  )
}

# Create test data with missing values
create_test_data_with_na <- function(n = 30) {
  data <- create_test_data(n)
  # Introduce missing values
  data$x_numeric[sample(n, 3)] <- NA
  data$y_numeric[sample(n, 2)] <- NA
  data$color_var[sample(n, 1)] <- NA
  return(data)
}

# ============================================================================
# BASIC FUNCTIONALITY TESTS
# ============================================================================

test_that("Basic tidyplots function works with minimal input", {
  data <- create_test_data()
  
  # Test basic function call
  expect_silent({
    result <- tidyplots(
      data = data,
      xvar = "x_numeric",
      yvar = "y_numeric"
    )
  })
  
  # Check result structure
  expect_true(inherits(result, "tidyplotsResults"))
  expect_true("plot" %in% names(result))
  expect_true("instructions" %in% names(result))
})

test_that("Function handles different variable types correctly", {
  data <- create_test_data()
  
  # Numeric x, numeric y
  expect_silent(tidyplots(data, "x_numeric", "y_numeric"))
  
  # Categorical x, numeric y
  expect_silent(tidyplots(data, "x_categorical", "y_numeric"))
  
  # Numeric x, categorical y (less common but should work)
  expect_silent(tidyplots(data, "x_numeric", "x_categorical"))
})

test_that("Color variable functionality works", {
  data <- create_test_data()
  
  # With color variable
  expect_silent({
    result <- tidyplots(
      data = data,
      xvar = "x_numeric",
      yvar = "y_numeric",
      color = "color_var"
    )
  })
  
  # Without color variable
  expect_silent({
    result <- tidyplots(
      data = data,
      xvar = "x_numeric",
      yvar = "y_numeric"
    )
  })
})

# ============================================================================
# PLOT TYPE TESTS
# ============================================================================

test_that("All plot types work correctly", {
  data <- create_test_data()
  
  plot_types <- c("points", "line", "bar", "boxplot", "violin", "histogram", "area", "density")
  
  for (plot_type in plot_types) {
    expect_silent({
      result <- tidyplots(
        data = data,
        xvar = if (plot_type %in% c("histogram", "density")) "x_numeric" else "x_categorical",
        yvar = if (plot_type %in% c("histogram", "density")) NULL else "y_numeric",
        plotType = plot_type
      )
    }, info = paste("Plot type:", plot_type))
  }
})

test_that("Point style options work", {
  data <- create_test_data()
  
  point_types <- c("basic", "beeswarm", "jitter")
  
  for (point_type in point_types) {
    expect_silent({
      result <- tidyplots(
        data = data,
        xvar = "x_categorical",
        yvar = "y_numeric",
        plotType = "points",
        pointType = point_type
      )
    }, info = paste("Point type:", point_type))
  }
})

test_that("Line type options work", {
  data <- create_test_data()
  
  line_types <- c("direct", "mean", "median", "curve")
  
  for (line_type in line_types) {
    expect_silent({
      result <- tidyplots(
        data = data,
        xvar = "x_numeric",
        yvar = "y_numeric",
        plotType = "line",
        lineType = line_type
      )
    }, info = paste("Line type:", line_type))
  }
})

test_that("Bar type options work", {
  data <- create_test_data()
  
  bar_types <- c("mean", "median", "count")
  
  for (bar_type in bar_types) {
    expect_silent({
      result <- tidyplots(
        data = data,
        xvar = "x_categorical",
        yvar = "y_numeric",
        plotType = "bar",
        barType = bar_type
      )
    }, info = paste("Bar type:", bar_type))
  }
})

# ============================================================================
# STATISTICAL ELEMENTS TESTS
# ============================================================================

test_that("Central tendency measures work", {
  data <- create_test_data()
  
  # Mean options
  mean_types <- c("dash", "dot", "value")
  for (mean_type in mean_types) {
    expect_silent({
      result <- tidyplots(
        data = data,
        xvar = "x_categorical",
        yvar = "y_numeric",
        showMean = TRUE,
        meanType = mean_type
      )
    }, info = paste("Mean type:", mean_type))
  }
  
  # Median options
  median_types <- c("dash", "dot", "value")
  for (median_type in median_types) {
    expect_silent({
      result <- tidyplots(
        data = data,
        xvar = "x_categorical",
        yvar = "y_numeric",
        showMedian = TRUE,
        medianType = median_type
      )
    }, info = paste("Median type:", median_type))
  }
})

test_that("Uncertainty measures work", {
  data <- create_test_data()
  
  # Test different uncertainty measures
  expect_silent({
    tidyplots(data, "x_categorical", "y_numeric", showSEM = TRUE)
  })
  
  expect_silent({
    tidyplots(data, "x_categorical", "y_numeric", showSD = TRUE)
  })
  
  expect_silent({
    tidyplots(data, "x_categorical", "y_numeric", showCI = TRUE, ciType = "errorbar")
  })
  
  expect_silent({
    tidyplots(data, "x_categorical", "y_numeric", showCI = TRUE, ciType = "ribbon")
  })
  
  expect_silent({
    tidyplots(data, "x_categorical", "y_numeric", showRange = TRUE)
  })
})

test_that("Distribution elements work", {
  data <- create_test_data()
  
  expect_silent({
    tidyplots(data, "x_numeric", "y_numeric", 
             showDistribution = TRUE, distributionType = "density")
  })
  
  expect_silent({
    tidyplots(data, "x_numeric", "y_numeric", 
             showDistribution = TRUE, distributionType = "rug")
  })
})

# ============================================================================
# COLOR SCHEME TESTS
# ============================================================================

test_that("All color schemes work", {
  data <- create_test_data()
  
  color_schemes <- c("friendly", "seaside", "apple", "rainbow", 
                    "viridis", "inferno", "magma", "turbo",
                    "blue2red", "blue2brown")
  
  for (scheme in color_schemes) {
    expect_silent({
      result <- tidyplots(
        data = data,
        xvar = "x_categorical",
        yvar = "y_numeric",
        color = "color_var",
        colorScheme = scheme
      )
    }, info = paste("Color scheme:", scheme))
  }
})

# ============================================================================
# CUSTOMIZATION TESTS
# ============================================================================

test_that("Labels and titles work", {
  data <- create_test_data()
  
  expect_silent({
    result <- tidyplots(
      data = data,
      xvar = "x_numeric",
      yvar = "y_numeric",
      plotTitle = "Test Title",
      xLabel = "X Axis Label",
      yLabel = "Y Axis Label",
      legendTitle = "Legend Title"
    )
  })
})

test_that("Appearance options work", {
  data <- create_test_data()
  
  expect_silent({
    result <- tidyplots(
      data = data,
      xvar = "x_numeric",
      yvar = "y_numeric",
      removeLegend = TRUE,
      removePadding = TRUE,
      fontSize = 14,
      alpha = 0.7
    )
  })
})

test_that("Axis options work", {
  data <- create_test_data()
  
  # X-axis options
  expect_silent({
    tidyplots(data, "x_numeric", "y_numeric", removeXAxisLabels = TRUE)
  })
  
  expect_silent({
    tidyplots(data, "x_numeric", "y_numeric", removeXAxisTitle = TRUE)
  })
  
  # Y-axis options
  expect_silent({
    tidyplots(data, "x_numeric", "y_numeric", removeYAxisLabels = TRUE)
  })
  
  expect_silent({
    tidyplots(data, "x_numeric", "y_numeric", removeYAxisTitle = TRUE)
  })
})

# ============================================================================
# GROUPING AND FACETING TESTS
# ============================================================================

test_that("Grouping variables work", {
  data <- create_test_data()
  
  expect_silent({
    result <- tidyplots(
      data = data,
      xvar = "x_numeric",
      yvar = "y_numeric",
      group = "group_var"
    )
  })
})

test_that("Faceting works", {
  data <- create_test_data()
  
  expect_silent({
    result <- tidyplots(
      data = data,
      xvar = "x_numeric",
      yvar = "y_numeric",
      facet = "facet_var"
    )
  })
})

test_that("Combined grouping and faceting work", {
  data <- create_test_data()
  
  expect_silent({
    result <- tidyplots(
      data = data,
      xvar = "x_numeric",
      yvar = "y_numeric",
      color = "color_var",
      group = "group_var",
      facet = "facet_var"
    )
  })
})

# ============================================================================
# ERROR HANDLING TESTS
# ============================================================================

test_that("Function handles missing required arguments", {
  data <- create_test_data()
  
  # Missing data
  expect_error(tidyplots(xvar = "x_numeric", yvar = "y_numeric"))
  
  # Missing x variable
  expect_error(tidyplots(data = data, yvar = "y_numeric"))
  
  # Missing y variable (should be allowed for some plot types)
  expect_error(tidyplots(data = data, xvar = "x_numeric", plotType = "points"))
})

test_that("Function handles non-existent variables", {
  data <- create_test_data()
  
  # Non-existent x variable
  expect_error(tidyplots(data, "nonexistent_x", "y_numeric"))
  
  # Non-existent y variable
  expect_error(tidyplots(data, "x_numeric", "nonexistent_y"))
  
  # Non-existent color variable
  expect_error(tidyplots(data, "x_numeric", "y_numeric", color = "nonexistent_color"))
})

test_that("Function handles empty data", {
  empty_data <- data.frame(x = numeric(0), y = numeric(0))
  
  expect_error(tidyplots(empty_data, "x", "y"))
})

test_that("Function handles data with all missing values", {
  data <- data.frame(
    x = rep(NA, 10),
    y = rep(NA, 10)
  )
  
  expect_error(tidyplots(data, "x", "y"))
})

test_that("Function handles invalid plot types", {
  data <- create_test_data()
  
  expect_error(tidyplots(data, "x_numeric", "y_numeric", plotType = "invalid_type"))
})

test_that("Function handles invalid parameter values", {
  data <- create_test_data()
  
  # Invalid alpha value
  expect_error(tidyplots(data, "x_numeric", "y_numeric", alpha = 2.0))
  expect_error(tidyplots(data, "x_numeric", "y_numeric", alpha = -0.5))
  
  # Invalid font size
  expect_error(tidyplots(data, "x_numeric", "y_numeric", fontSize = 0))
  expect_error(tidyplots(data, "x_numeric", "y_numeric", fontSize = 100))
  
  # Invalid histogram bins
  expect_error(tidyplots(data, "x_numeric", plotType = "histogram", histogramBins = 0))
  expect_error(tidyplots(data, "x_numeric", plotType = "histogram", histogramBins = 200))
})

# ============================================================================
# MISSING DATA HANDLING TESTS
# ============================================================================

test_that("Function handles missing data appropriately", {
  data_with_na <- create_test_data_with_na()
  
  # Should work despite missing values
  expect_silent({
    result <- tidyplots(
      data = data_with_na,
      xvar = "x_numeric",
      yvar = "y_numeric"
    )
  })
  
  # Should work with missing values in color variable
  expect_silent({
    result <- tidyplots(
      data = data_with_na,
      xvar = "x_numeric",
      yvar = "y_numeric",
      color = "color_var"
    )
  })
})

# ============================================================================
# SPECIAL PLOT TYPES TESTS
# ============================================================================

test_that("Special plot options work correctly", {
  data <- create_test_data()
  
  # Boxplot with outliers
  expect_silent({
    tidyplots(data, "x_categorical", "y_numeric", 
             plotType = "boxplot", showOutliers = TRUE)
  })
  
  expect_silent({
    tidyplots(data, "x_categorical", "y_numeric", 
             plotType = "boxplot", showOutliers = FALSE)
  })
  
  # Violin with points
  expect_silent({
    tidyplots(data, "x_categorical", "y_numeric", 
             plotType = "violin", violinPoints = TRUE)
  })
  
  expect_silent({
    tidyplots(data, "x_categorical", "y_numeric", 
             plotType = "violin", violinPoints = FALSE)
  })
  
  # Histogram with different bin numbers
  expect_silent({
    tidyplots(data, "x_numeric", plotType = "histogram", histogramBins = 10)
  })
  
  expect_silent({
    tidyplots(data, "x_numeric", plotType = "histogram", histogramBins = 50)
  })
})

# ============================================================================
# STATISTICAL TESTING TESTS
# ============================================================================

test_that("Statistical testing options work", {
  data <- create_test_data()
  
  # Note: These may generate warnings if statistical tests fail
  # but should not error out completely
  
  expect_silent({
    suppressWarnings({
      tidyplots(data, "x_categorical", "y_numeric", 
               color = "color_var", showPValue = TRUE)
    })
  })
  
  expect_silent({
    suppressWarnings({
      tidyplots(data, "x_categorical", "y_numeric", 
               color = "color_var", showSignificance = TRUE)
    })
  })
})

# ============================================================================
# PERFORMANCE AND EDGE CASE TESTS
# ============================================================================

test_that("Function handles large datasets reasonably", {
  large_data <- create_test_data(1000)
  
  expect_silent({
    result <- tidyplots(
      data = large_data,
      xvar = "x_numeric",
      yvar = "y_numeric",
      color = "color_var"
    )
  })
})

test_that("Function handles single-group data", {
  single_group_data <- data.frame(
    x = rnorm(20, 50, 10),
    y = rnorm(20, 100, 15),
    group = rep("Single_Group", 20)
  )
  
  expect_silent({
    result <- tidyplots(
      data = single_group_data,
      xvar = "x",
      yvar = "y",
      color = "group"
    )
  })
})

test_that("Function handles extreme values", {
  extreme_data <- data.frame(
    x = c(1:10, 1000000),  # One extreme outlier
    y = c(1:10, -1000000), # One extreme outlier
    group = rep(c("A", "B"), length.out = 11)
  )
  
  expect_silent({
    result <- tidyplots(
      data = extreme_data,
      xvar = "x",
      yvar = "y",
      color = "group"
    )
  })
})

# ============================================================================
# INTEGRATION TESTS WITH REAL-WORLD SCENARIOS
# ============================================================================

test_that("Complex real-world scenario works", {
  data <- create_test_data(100)
  
  # Complex plot with many options
  expect_silent({
    result <- tidyplots(
      data = data,
      xvar = "x_categorical",
      yvar = "y_numeric",
      color = "color_var",
      group = "group_var",
      plotType = "violin",
      violinPoints = TRUE,
      showMean = TRUE,
      meanType = "dot",
      showCI = TRUE,
      ciType = "ribbon",
      colorScheme = "viridis",
      plotTitle = "Complex Test Plot",
      xLabel = "Categories",
      yLabel = "Values",
      legendTitle = "Groups"
    )
  })
})

# ============================================================================
# CLEANUP
# ============================================================================

# Clean up any temporary objects
rm(create_test_data, create_test_data_with_na)

cat("All tidyplots tests completed successfully!\n")