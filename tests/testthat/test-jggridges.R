# Comprehensive tests for jggridges function
library(ClinicoPath)

# Load test datasets
data(histopathology, package = "ClinicoPath")

test_that("jggridges module loads correctly", {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()
  expect_true(exists("jggridgesClass"))
  expect_true(is.function(jggridges))
})

test_that("jggridges handles basic input validation", {
  # Test with no variables specified
  expect_error(
    jggridges(data = histopathology),
    NA  # Should not error during initialization
  )
  
  # Test with no data
  expect_error(
    jggridges(data = NULL, x_var = "Age", y_var = "Grade_Level"),
    NA  # Should not error during initialization
  )
  
  # Test with invalid x variable (non-numeric)
  expect_error(
    jggridges(data = histopathology, x_var = "Sex", y_var = "Grade_Level"),
    "X variable must be numeric"
  )
  
  # Test with y variable with too few groups
  single_group_data <- histopathology[histopathology$Grade_Level == "low", ]
  expect_error(
    jggridges(data = single_group_data, x_var = "Age", y_var = "Grade_Level"),
    "Y variable must have at least 2 groups"
  )
})

test_that("jggridges creates basic ridge plots", {
  # Basic ridge plot with Age and Grade_Level
  result <- jggridges(
    data = histopathology,
    x_var = "Age",
    y_var = "Grade_Level",
    plot_type = "density_ridges"
  )
  
  expect_s3_class(result, "jggridgesClass")
  expect_true(!is.null(result$results$plot))
})

test_that("jggridges handles different plot types", {
  plot_types <- c("ridgeline", "density_ridges", "density_ridges_gradient", "violin_ridges")
  
  for (type in plot_types) {
    result <- jggridges(
      data = histopathology,
      x_var = "Age",
      y_var = "Grade_Level",
      plot_type = type
    )
    
    expect_s3_class(result, "jggridgesClass")
    expect_equal(result$options$plot_type, type)
  }
})

test_that("jggridges handles color variables", {
  # Test with color variable
  result <- jggridges(
    data = histopathology,
    x_var = "Age",
    y_var = "Grade_Level",
    color_var = "Sex",
    color_palette = "viridis"
  )
  
  expect_s3_class(result, "jggridgesClass")
  expect_equal(result$options$color_var, "Sex")
})

test_that("jggridges handles facet variables", {
  # Test with facet variable
  result <- jggridges(
    data = histopathology,
    x_var = "Age",
    y_var = "Grade_Level",
    facet_var = "LVI"
  )
  
  expect_s3_class(result, "jggridgesClass")
  expect_equal(result$options$facet_var, "LVI")
})

test_that("jggridges handles different bandwidth methods", {
  bandwidth_methods <- c("nrd0", "nrd", "ucv", "bcv", "SJ")
  
  for (method in bandwidth_methods) {
    result <- jggridges(
      data = histopathology,
      x_var = "Age",
      y_var = "Grade_Level",
      plot_type = "density_ridges",
      bandwidth = method
    )
    
    expect_s3_class(result, "jggridgesClass")
    expect_equal(result$options$bandwidth, method)
  }
})

test_that("jggridges handles quantile lines", {
  # Test quantile lines
  result <- jggridges(
    data = histopathology,
    x_var = "Age",
    y_var = "Grade_Level",
    quantile_lines = TRUE,
    quantiles = "0.25, 0.5, 0.75"
  )
  
  expect_s3_class(result, "jggridgesClass")
  expect_true(result$options$quantile_lines)
  expect_equal(result$options$quantiles, "0.25, 0.5, 0.75")
})

test_that("jggridges handles jittered points", {
  # Test jittered points
  result <- jggridges(
    data = histopathology,
    x_var = "Age",
    y_var = "Grade_Level",
    jittered_points = TRUE,
    point_alpha = 0.3
  )
  
  expect_s3_class(result, "jggridgesClass")
  expect_true(result$options$jittered_points)
  expect_equal(result$options$point_alpha, 0.3)
})

test_that("jggridges handles mean lines", {
  # Test mean lines
  result <- jggridges(
    data = histopathology,
    x_var = "Age",
    y_var = "Grade_Level",
    show_mean = TRUE
  )
  
  expect_s3_class(result, "jggridgesClass")
  expect_true(result$options$show_mean)
})

test_that("jggridges handles different themes", {
  themes <- c("theme_ridges", "theme_minimal", "theme_classic", "theme_gray", "theme_bw")
  
  for (theme in themes) {
    result <- jggridges(
      data = histopathology,
      x_var = "Age",
      y_var = "Grade_Level",
      theme_style = theme
    )
    
    expect_s3_class(result, "jggridgesClass")
    expect_equal(result$options$theme_style, theme)
  }
})

test_that("jggridges handles color palettes", {
  palettes <- c("viridis", "plasma", "inferno", "magma", "Set1", "Set2", "Dark2", "Paired")
  
  for (palette in palettes) {
    result <- jggridges(
      data = histopathology,
      x_var = "Age",
      y_var = "Grade_Level",
      color_var = "Sex",
      color_palette = palette
    )
    
    expect_s3_class(result, "jggridgesClass")
    expect_equal(result$options$color_palette, palette)
  }
})

test_that("jggridges handles custom labels", {
  # Test custom labels
  result <- jggridges(
    data = histopathology,
    x_var = "Age",
    y_var = "Grade_Level",
    x_label = "Patient Age (years)",
    y_label = "Tumor Grade",
    plot_title = "Age Distribution by Grade",
    plot_subtitle = "Ridge plot analysis"
  )
  
  expect_s3_class(result, "jggridgesClass")
  expect_equal(result$options$x_label, "Patient Age (years)")
  expect_equal(result$options$y_label, "Tumor Grade")
  expect_equal(result$options$plot_title, "Age Distribution by Grade")
  expect_equal(result$options$plot_subtitle, "Ridge plot analysis")
})

test_that("jggridges handles reverse order", {
  # Test reverse order
  result <- jggridges(
    data = histopathology,
    x_var = "Age",
    y_var = "Grade_Level",
    reverse_order = TRUE
  )
  
  expect_s3_class(result, "jggridgesClass")
  expect_true(result$options$reverse_order)
})

test_that("jggridges handles scale and transparency", {
  # Test scale and transparency
  result <- jggridges(
    data = histopathology,
    x_var = "Age",
    y_var = "Grade_Level",
    scale = 2.0,
    alpha = 0.5,
    rel_min_height = 0.02
  )
  
  expect_s3_class(result, "jggridgesClass")
  expect_equal(result$options$scale, 2.0)
  expect_equal(result$options$alpha, 0.5)
  expect_equal(result$options$rel_min_height, 0.02)
})

test_that("jggridges handles statistics output", {
  # Test statistics output
  result <- jggridges(
    data = histopathology,
    x_var = "Age",
    y_var = "Grade_Level",
    show_statistics = TRUE
  )
  
  expect_s3_class(result, "jggridgesClass")
  expect_true(result$options$show_statistics)
  expect_true(!is.null(result$results$statistics))
})

test_that("jggridges handles interpretation output", {
  # Test interpretation output
  result <- jggridges(
    data = histopathology,
    x_var = "Age",
    y_var = "Grade_Level",
    show_interpretation = TRUE
  )
  
  expect_s3_class(result, "jggridgesClass")
  expect_true(result$options$show_interpretation)
  expect_true(!is.null(result$results$interpretation))
})

test_that("jggridges handles panel expansion", {
  # Test panel expansion
  result <- jggridges(
    data = histopathology,
    x_var = "Age",
    y_var = "Grade_Level",
    expand_panel = FALSE
  )
  
  expect_s3_class(result, "jggridgesClass")
  expect_false(result$options$expand_panel)
})

test_that("jggridges handles complex quantiles", {
  # Test various quantile formats
  quantile_formats <- list(
    "0.5",
    "0.25, 0.75",
    "0.1, 0.25, 0.5, 0.75, 0.9",
    "0.25,0.5,0.75",  # No spaces
    "0.25 0.5 0.75"   # Space separated
  )
  
  for (quant in quantile_formats) {
    result <- jggridges(
      data = histopathology,
      x_var = "Age",
      y_var = "Grade_Level",
      quantile_lines = TRUE,
      quantiles = quant
    )
    
    expect_s3_class(result, "jggridgesClass")
  }
})

test_that("jggridges comprehensive test", {
  # Test with all major features enabled
  result <- jggridges(
    data = histopathology,
    x_var = "Age",
    y_var = "Grade_Level",
    color_var = "Sex",
    facet_var = "LVI",
    plot_type = "density_ridges_gradient",
    scale = 1.5,
    rel_min_height = 0.01,
    alpha = 0.8,
    bandwidth = "nrd0",
    quantile_lines = TRUE,
    quantiles = "0.25, 0.5, 0.75",
    jittered_points = FALSE,
    point_alpha = 0.5,
    color_palette = "viridis",
    reverse_order = FALSE,
    expand_panel = TRUE,
    theme_style = "theme_ridges",
    x_label = "Age (years)",
    y_label = "Tumor Grade",
    plot_title = "Comprehensive Ridge Plot",
    plot_subtitle = "All features enabled",
    show_mean = TRUE,
    show_statistics = TRUE,
    show_interpretation = TRUE
  )
  
  expect_s3_class(result, "jggridgesClass")
  
  # Verify all options are set correctly
  expect_equal(result$options$x_var, "Age")
  expect_equal(result$options$y_var, "Grade_Level")
  expect_equal(result$options$color_var, "Sex")
  expect_equal(result$options$facet_var, "LVI")
  expect_equal(result$options$plot_type, "density_ridges_gradient")
  expect_equal(result$options$scale, 1.5)
  expect_true(result$options$quantile_lines)
  expect_true(result$options$show_mean)
  expect_true(result$options$show_statistics)
  expect_true(result$options$show_interpretation)
})

test_that("jggridges handles multiple grouping variables", {
  # Test with different grouping variables
  grouping_vars <- c("Grade_Level", "TStage", "Group", "LVI")
  
  for (group_var in grouping_vars) {
    # Check if the variable has enough groups
    n_groups <- length(unique(histopathology[[group_var]]))
    
    if (n_groups >= 2) {
      result <- jggridges(
        data = histopathology,
        x_var = "Age",
        y_var = group_var
      )
      
      expect_s3_class(result, "jggridgesClass")
    }
  }
})

test_that("jggridges handles different continuous variables", {
  # Test with different continuous variables
  continuous_vars <- c("Age", "MeasurementA", "MeasurementB", "OverallTime")
  
  for (cont_var in continuous_vars) {
    result <- jggridges(
      data = histopathology,
      x_var = cont_var,
      y_var = "Grade_Level"
    )
    
    expect_s3_class(result, "jggridgesClass")
  }
})

test_that("jggridges handles missing data appropriately", {
  # Create data with missing values
  test_data <- histopathology
  test_data$Age[1:10] <- NA
  
  result <- jggridges(
    data = test_data,
    x_var = "Age",
    y_var = "Grade_Level"
  )
  
  expect_s3_class(result, "jggridgesClass")
  # Should still work with complete cases
})

test_that("jggridges handles edge cases", {
  # Test with small dataset
  small_data <- histopathology[1:20, ]
  
  result <- jggridges(
    data = small_data,
    x_var = "Age",
    y_var = "Sex"
  )
  
  expect_s3_class(result, "jggridgesClass")
  
  # Test with many groups
  # Create a variable with many groups
  test_data <- histopathology
  test_data$many_groups <- factor(rep(letters[1:10], length.out = nrow(test_data)))
  
  result <- jggridges(
    data = test_data,
    x_var = "Age",
    y_var = "many_groups"
  )
  
  expect_s3_class(result, "jggridgesClass")
})
