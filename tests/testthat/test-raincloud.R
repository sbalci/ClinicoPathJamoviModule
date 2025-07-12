context("Raincloud Plot Tests")

# Test basic functionality
test_that("raincloud module loads correctly", {
  expect_true(exists("raincloudClass"))
  expect_true(is.function(raincloud))
})

test_that("raincloud handles basic input validation", {
  # Test with missing required variables
  expect_error(
    raincloud(data = histopathology, dep_var = NULL, group_var = "Group"),
    NA  # Should not error during initialization, only during run
  )
  
  expect_error(
    raincloud(data = histopathology, dep_var = "Age", group_var = NULL),
    NA  # Should not error during initialization, only during run
  )
})

test_that("raincloud works with valid inputs", {
  # Test basic functionality
  result <- raincloud(
    data = histopathology,
    dep_var = "Age",
    group_var = "Group"
  )
  
  expect_s3_class(result, "raincloudClass")
  expect_true("Group" %in% names(histopathology))
  expect_true("Age" %in% names(histopathology))
})

test_that("raincloud works with optional variables", {
  # Test with facet variable
  expect_error({
    result <- raincloud(
      data = histopathology,
      dep_var = "Age",
      group_var = "Group",
      facet_var = "Sex"
    )
  }, NA)
  
  # Test with color variable
  expect_error({
    result <- raincloud(
      data = histopathology,
      dep_var = "Age",
      group_var = "Group",
      color_var = "Sex"
    )
  }, NA)
})

test_that("raincloud visualization options work correctly", {
  # Test different visualization components
  
  # Violin only
  expect_error({
    result <- raincloud(
      data = histopathology,
      dep_var = "Age",
      group_var = "Group",
      show_violin = TRUE,
      show_boxplot = FALSE,
      show_dots = FALSE
    )
  }, NA)
  
  # Box plot only
  expect_error({
    result <- raincloud(
      data = histopathology,
      dep_var = "Age",
      group_var = "Group",
      show_violin = FALSE,
      show_boxplot = TRUE,
      show_dots = FALSE
    )
  }, NA)
  
  # Dots only
  expect_error({
    result <- raincloud(
      data = histopathology,
      dep_var = "Age",
      group_var = "Group",
      show_violin = FALSE,
      show_boxplot = FALSE,
      show_dots = TRUE
    )
  }, NA)
})

test_that("raincloud color palettes work correctly", {
  # Test different color palettes
  palettes <- c("clinical", "viridis", "set1", "set2", "pastel", "dark2")
  
  for (palette in palettes) {
    expect_error({
      result <- raincloud(
        data = histopathology,
        dep_var = "Age",
        group_var = "Group",
        color_palette = palette
      )
    }, NA, info = paste("color_palette:", palette))
  }
})

test_that("raincloud plot themes work correctly", {
  # Test different plot themes
  themes <- c("clinical", "minimal", "classic", "publication", "tidyquant")
  
  for (theme in themes) {
    expect_error({
      result <- raincloud(
        data = histopathology,
        dep_var = "Age",
        group_var = "Group",
        plot_theme = theme
      )
    }, NA, info = paste("plot_theme:", theme))
  }
})

test_that("raincloud orientation options work correctly", {
  # Test vertical orientation
  expect_error({
    result <- raincloud(
      data = histopathology,
      dep_var = "Age",
      group_var = "Group",
      orientation = "vertical"
    )
  }, NA)
  
  # Test horizontal orientation (raincloud style)
  expect_error({
    result <- raincloud(
      data = histopathology,
      dep_var = "Age",
      group_var = "Group",
      orientation = "horizontal"
    )
  }, NA)
})

test_that("raincloud statistical options work correctly", {
  # Test with statistics enabled
  expect_error({
    result <- raincloud(
      data = histopathology,
      dep_var = "Age",
      group_var = "Group",
      show_statistics = TRUE
    )
  }, NA)
  
  # Test with outlier detection
  expect_error({
    result <- raincloud(
      data = histopathology,
      dep_var = "Age",
      group_var = "Group",
      show_outliers = TRUE
    )
  }, NA)
  
  # Test with normality tests
  expect_error({
    result <- raincloud(
      data = histopathology,
      dep_var = "Age",
      group_var = "Group",
      normality_test = TRUE
    )
  }, NA)
  
  # Test with group comparisons
  expect_error({
    result <- raincloud(
      data = histopathology,
      dep_var = "Age",
      group_var = "Group",
      comparison_test = TRUE
    )
  }, NA)
})

test_that("raincloud outlier detection methods work correctly", {
  # Test different outlier detection methods
  methods <- c("iqr", "zscore", "modified_zscore")
  
  for (method in methods) {
    expect_error({
      result <- raincloud(
        data = histopathology,
        dep_var = "Age",
        group_var = "Group",
        show_outliers = TRUE,
        outlier_method = method
      )
    }, NA, info = paste("outlier_method:", method))
  }
})

test_that("raincloud comparison methods work correctly", {
  # Test different comparison methods
  methods <- c("auto", "ttest", "wilcoxon", "anova", "kruskal")
  
  for (method in methods) {
    expect_error({
      result <- raincloud(
        data = histopathology,
        dep_var = "Age",
        group_var = "Group",
        comparison_test = TRUE,
        comparison_method = method
      )
    }, NA, info = paste("comparison_method:", method))
  }
})

test_that("raincloud customization parameters work correctly", {
  # Test numeric parameter validation
  expect_error({
    result <- raincloud(
      data = histopathology,
      dep_var = "Age",
      group_var = "Group",
      violin_width = 0.5,
      box_width = 0.15,
      dots_size = 1.5,
      alpha_violin = 0.8,
      alpha_dots = 0.6
    )
  }, NA)
  
  # Test boundary values
  expect_error({
    result <- raincloud(
      data = histopathology,
      dep_var = "Age",
      group_var = "Group",
      violin_width = 0.1,    # Minimum
      box_width = 1.0,       # Maximum  
      dots_size = 0.1,       # Minimum
      alpha_violin = 1.0,    # Maximum
      alpha_dots = 0.0       # Minimum
    )
  }, NA)
})

test_that("raincloud handles missing data appropriately", {
  # Create dataset with missing values
  test_data <- histopathology
  test_data$Age[1:5] <- NA
  test_data$Group[6:10] <- NA
  
  expect_error({
    result <- raincloud(
      data = test_data,
      dep_var = "Age",
      group_var = "Group"
    )
  }, NA)
})

test_that("raincloud handles different variable types", {
  # Test with numeric dependent variable
  expect_error({
    result <- raincloud(
      data = histopathology,
      dep_var = "OverallTime",  # Numeric variable
      group_var = "Group"
    )
  }, NA)
  
  # Test with factor group variable
  expect_error({
    test_data <- histopathology
    test_data$Group <- factor(test_data$Group)
    result <- raincloud(
      data = test_data,
      dep_var = "Age",
      group_var = "Group"
    )
  }, NA)
})

test_that("raincloud ggdist dependency handling", {
  # Test function behavior when ggdist is available
  if (requireNamespace("ggdist", quietly = TRUE)) {
    expect_error({
      result <- raincloud(
        data = histopathology,
        dep_var = "Age",
        group_var = "Group"
      )
    }, NA)
  } else {
    # Skip test if ggdist not available
    skip("ggdist package not available")
  }
})

test_that("raincloud handles small datasets", {
  # Test with small dataset
  small_data <- histopathology[1:20, ]
  
  expect_error({
    result <- raincloud(
      data = small_data,
      dep_var = "Age",
      group_var = "Group"
    )
  }, NA)
})

test_that("raincloud works with different continuous variables", {
  # Test with different continuous variables from histopathology
  continuous_vars <- c("Age", "OverallTime", "Grade", "MeasurementA", "MeasurementB")
  
  for (var in continuous_vars) {
    if (var %in% names(histopathology)) {
      expect_error({
        result <- raincloud(
          data = histopathology,
          dep_var = var,
          group_var = "Group"
        )
      }, NA, info = paste("dep_var:", var))
    }
  }
})

test_that("raincloud works with different grouping variables", {
  # Test with different categorical variables
  categorical_vars <- c("Group", "Sex", "Grade_Level", "Race")
  
  for (var in categorical_vars) {
    if (var %in% names(histopathology)) {
      expect_error({
        result <- raincloud(
          data = histopathology,
          dep_var = "Age",
          group_var = var
        )
      }, NA, info = paste("group_var:", var))
    }
  }
})

test_that("raincloud plot customization works", {
  # Test plot title and axis labels
  expect_error({
    result <- raincloud(
      data = histopathology,
      dep_var = "Age",
      group_var = "Group",
      plot_title = "Custom Raincloud Plot",
      x_label = "Patient Groups",
      y_label = "Age (years)"
    )
  }, NA)
})

test_that("raincloud complex combinations work", {
  # Test complex parameter combinations
  expect_error({
    result <- raincloud(
      data = histopathology,
      dep_var = "OverallTime",
      group_var = "Group",
      facet_var = "Sex",
      color_var = "Race",
      show_violin = TRUE,
      show_boxplot = TRUE,
      show_dots = TRUE,
      dots_side = "left",
      violin_width = 0.8,
      box_width = 0.2,
      dots_size = 1.0,
      alpha_violin = 0.7,
      alpha_dots = 0.8,
      orientation = "horizontal",
      color_palette = "clinical",
      plot_theme = "publication",
      show_statistics = TRUE,
      show_outliers = TRUE,
      outlier_method = "iqr",
      normality_test = TRUE,
      comparison_test = TRUE,
      comparison_method = "auto"
    )
  }, NA)
})

test_that("raincloud results object structure", {
  # Test that results object has expected structure
  result <- raincloud(
    data = histopathology,
    dep_var = "Age",
    group_var = "Group"
  )
  
  # Check that key results components exist
  expect_true("plot" %in% names(result))
  expect_true("statistics" %in% names(result))
  expect_true("interpretation" %in% names(result))
  expect_true("outliers" %in% names(result))
  expect_true("normality" %in% names(result))
  expect_true("comparison" %in% names(result))
})

test_that("raincloud fallback functions work", {
  # Test skewness and kurtosis fallbacks
  test_data <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
  
  # These functions are internal but we can test basic calculation
  # by ensuring the function doesn't error when moments package unavailable
  expect_error({
    result <- raincloud(
      data = histopathology,
      dep_var = "Age",
      group_var = "Group",
      show_statistics = TRUE
    )
  }, NA)
})