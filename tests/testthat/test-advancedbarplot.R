test_that("advancedbarplot module loads correctly", {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()
  expect_true(exists("advancedbarplotClass"))
  expect_true(is.function(advancedbarplot))
})

test_that("advancedbarplot handles basic input validation", {
  # Test with missing required variables
  expect_error(
    advancedbarplot(data = histopathology, x_var = NULL, y_var = "Age"),
    NA  # Should not error during initialization, only during run
  )
  
  expect_error(
    advancedbarplot(data = histopathology, x_var = "Group", y_var = NULL),
    NA  # Should not error during initialization, only during run
  )
})

test_that("advancedbarplot works with valid inputs", {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()
  
  # Test basic functionality
  result <- advancedbarplot(
    data = histopathology,
    x_var = "Group",
    y_var = "Age",
    chart_approach = "basic"
  )
  
  expect_s3_class(result, "advancedbarplotClass")
  expect_true("Group" %in% names(histopathology))
  expect_true("Age" %in% names(histopathology))
  
  # Export OMV
  omv_path <- file.path('omv_output', 'advancedbarplot.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')
  expect_no_error(jmvReadWrite::write_omv(result, omv_path))
  expect_true(file.exists(omv_path))
})

test_that("advancedbarplot statistical summaries work correctly", {
  # Test different statistical summaries
  stat_types <- c("mean", "median", "sum", "count", "prop")
  
  for (stat_type in stat_types) {
    expect_error({
      result <- advancedbarplot(
        data = histopathology,
        x_var = "Group", 
        y_var = "Age",
        stat_type = stat_type,
        chart_approach = "basic"
      )
    }, NA, info = paste("stat_type:", stat_type))
  }
})

test_that("advancedbarplot chart approaches work correctly", {
  # Test all chart approaches
  approaches <- c("basic", "polished", "statistical", "interactive", 
                 "publication", "bbc_style", "prism_style")
  
  for (approach in approaches) {
    expect_error({
      result <- advancedbarplot(
        data = histopathology,
        x_var = "Group",
        y_var = "Age", 
        chart_approach = approach
      )
    }, NA, info = paste("chart_approach:", approach))
  }
})

test_that("advancedbarplot color palettes work correctly", {
  # Test key color palettes
  palettes <- c("default", "bbc_blue", "bbc_orange", "bbc_multi", 
               "viridis", "clinical", "colorblind", "prism_floral")
  
  for (palette in palettes) {
    expect_error({
      result <- advancedbarplot(
        data = histopathology,
        x_var = "Group",
        y_var = "Age",
        color_palette = palette,
        chart_approach = "polished"
      )
    }, NA, info = paste("color_palette:", palette))
  }
})

test_that("advancedbarplot statistical tests work correctly", {
  # Test statistical testing functionality
  stat_methods <- c("anova", "ttest", "wilcox", "kruskal")
  
  for (method in stat_methods) {
    expect_error({
      result <- advancedbarplot(
        data = histopathology,
        x_var = "Group",
        y_var = "Age",
        add_statistics = TRUE,
        stat_method = method,
        chart_approach = "statistical"
      )
    }, NA, info = paste("stat_method:", method))
  }
})

test_that("advancedbarplot handles grouping variables correctly", {
  # Test with fill variable
  expect_error({
    result <- advancedbarplot(
      data = histopathology,
      x_var = "Group",
      y_var = "Age", 
      fill_var = "Sex",
      chart_approach = "polished"
    )
  }, NA)
  
  # Test with facet variable  
  expect_error({
    result <- advancedbarplot(
      data = histopathology,
      x_var = "Group",
      y_var = "Age",
      facet_var = "Sex", 
      chart_approach = "basic"
    )
  }, NA)
})

test_that("advancedbarplot bar positioning works correctly", {
  # Test different bar positions
  positions <- c("dodge", "stack", "fill", "identity")
  
  for (position in positions) {
    expect_error({
      result <- advancedbarplot(
        data = histopathology,
        x_var = "Group",
        y_var = "Age",
        fill_var = "Sex",
        bar_position = position,
        chart_approach = "polished"
      )
    }, NA, info = paste("bar_position:", position))
  }
})

test_that("advancedbarplot error bars work correctly", {
  # Test different error bar types
  error_types <- c("none", "se", "sd", "ci95")
  
  for (error_type in error_types) {
    expect_error({
      result <- advancedbarplot(
        data = histopathology,
        x_var = "Group",
        y_var = "Age",
        error_bars = error_type,
        chart_approach = "statistical"
      )
    }, NA, info = paste("error_bars:", error_type))
  }
})

test_that("advancedbarplot orientation settings work correctly", {
  # Test vertical and horizontal orientations
  orientations <- c("vertical", "horizontal")
  
  for (orientation in orientations) {
    expect_error({
      result <- advancedbarplot(
        data = histopathology,
        x_var = "Group",
        y_var = "Age",
        orientation = orientation,
        chart_approach = "basic"
      )
    }, NA, info = paste("orientation:", orientation))
  }
})

test_that("advancedbarplot value formatting works correctly", {
  # Test different value formats
  formats <- c("auto", "integer", "decimal1", "decimal2", "percentage", "scientific")
  
  for (format in formats) {
    expect_error({
      result <- advancedbarplot(
        data = histopathology,
        x_var = "Group",
        y_var = "Age",
        show_values = TRUE,
        value_format = format,
        chart_approach = "polished"
      )
    }, NA, info = paste("value_format:", format))
  }
})

test_that("advancedbarplot sorting options work correctly", {
  # Test different sorting options
  sort_options <- c("none", "asc", "desc", "alpha")
  
  for (sort_option in sort_options) {
    expect_error({
      result <- advancedbarplot(
        data = histopathology,
        x_var = "Group",
        y_var = "Age",
        sort_bars = sort_option,
        chart_approach = "basic"
      )
    }, NA, info = paste("sort_bars:", sort_option))
  }
})

test_that("advancedbarplot legend positioning works correctly", {
  # Test different legend positions
  positions <- c("right", "left", "top", "bottom", "none")
  
  for (position in positions) {
    expect_error({
      result <- advancedbarplot(
        data = histopathology,
        x_var = "Group",
        y_var = "Age",
        fill_var = "Sex",
        legend_position = position,
        chart_approach = "polished"
      )
    }, NA, info = paste("legend_position:", position))
  }
})

test_that("advancedbarplot handles numeric parameters correctly", {
  # Test numeric parameter validation
  expect_error({
    result <- advancedbarplot(
      data = histopathology,
      x_var = "Group",
      y_var = "Age",
      bar_width = 0.8,
      transparency = 0.9,
      plot_width = 10,
      plot_height = 6,
      chart_approach = "basic"
    )
  }, NA)
  
  # Test boundary values
  expect_error({
    result <- advancedbarplot(
      data = histopathology,
      x_var = "Group", 
      y_var = "Age",
      bar_width = 0.1,  # Minimum
      transparency = 1.0,  # Maximum
      chart_approach = "basic"
    )
  }, NA)
})

test_that("advancedbarplot handles missing data appropriately", {
  # Create dataset with missing values
  test_data <- histopathology
  test_data$Age[1:5] <- NA
  test_data$Group[6:10] <- NA
  
  expect_error({
    result <- advancedbarplot(
      data = test_data,
      x_var = "Group",
      y_var = "Age", 
      chart_approach = "basic"
    )
  }, NA)
})

test_that("advancedbarplot BBC style implementation works", {
  # Test BBC-specific features
  expect_error({
    result <- advancedbarplot(
      data = histopathology,
      x_var = "Group",
      y_var = "Age",
      chart_approach = "bbc_style",
      color_palette = "bbc_multi",
      plot_title = "BBC Style Test",
      subtitle_text = "Test Subtitle",
      source_text = "Test Source"
    )
  }, NA)
})

test_that("advancedbarplot Prism style implementation works", {
  # Test Prism-specific features
  expect_error({
    result <- advancedbarplot(
      data = histopathology,
      x_var = "Group",
      y_var = "Age",
      chart_approach = "prism_style",
      color_palette = "prism_floral",
      theme_style = "prism_default"
    )
  }, NA)
})

test_that("advancedbarplot works with different data types", {
  # Test with factor x variable
  expect_error({
    test_data <- histopathology
    test_data$Group <- factor(test_data$Group)
    result <- advancedbarplot(
      data = test_data,
      x_var = "Group",
      y_var = "Age",
      chart_approach = "basic"
    )
  }, NA)
  
  # Test with numeric y variable
  expect_error({
    result <- advancedbarplot(
      data = histopathology,
      x_var = "Group",
      y_var = "Grade",  # Numeric variable
      chart_approach = "basic"
    )
  }, NA)
})

test_that("advancedbarplot complex combinations work", {
  # Test complex parameter combinations
  expect_error({
    result <- advancedbarplot(
      data = histopathology,
      x_var = "Grade_Level",
      y_var = "OverallTime",
      fill_var = "Sex",
      chart_approach = "statistical",
      stat_type = "mean",
      bar_position = "dodge",
      add_statistics = TRUE,
      stat_method = "anova",
      error_bars = "se",
      color_palette = "clinical",
      show_values = TRUE,
      value_format = "decimal1",
      orientation = "vertical",
      legend_position = "top"
    )
  }, NA)
})

test_that("advancedbarplot export options work", {
  # Test export optimization
  expect_error({
    result <- advancedbarplot(
      data = histopathology,
      x_var = "Group",
      y_var = "Age",
      chart_approach = "publication",
      export_options = TRUE,
      plot_width = 12,
      plot_height = 8
    )
  }, NA)
})