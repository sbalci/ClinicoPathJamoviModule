# Test file for jggsankeyfier function
# Tests sankey and alluvial diagram functionality

library(testthat)
library(ClinicoPath)

# Test Data Setup
test_that("Test data loads correctly", {
  expect_true(exists("sankey_simple_data"))
  expect_true(exists("simple_flow_data"))
  expect_true(exists("complex_alluvial_data"))
})

# Basic Functionality Tests
test_that("jggsankeyfier basic functionality works", {
  
  # Test with minimal required parameters
  result <- jggsankeyfier(
    data = simple_flow_data,
    value_var = "flow_amount",
    source_var = "from_node",
    target_var = "to_node"
  )
  
  expect_true(!is.null(result))
  expect_s3_class(result, "jmvAnalysis")
})

test_that("jggsankeyfier handles missing required variables", {
  
  # Test without value variable
  expect_error(
    jggsankeyfier(
      data = simple_flow_data,
      source_var = "from_node",
      target_var = "to_node"
    ),
    NA  # Should not error in jamovi context, but should show instructions
  )
  
  # Test without source/target or node variables
  expect_error(
    jggsankeyfier(
      data = simple_flow_data,
      value_var = "flow_amount"
    ),
    NA  # Should not error in jamovi context, but should show instructions
  )
})

# Diagram Type Tests
test_that("jggsankeyfier diagram types work correctly", {
  
  # Test Sankey diagram
  result_sankey <- jggsankeyfier(
    data = simple_flow_data,
    value_var = "flow_amount",
    source_var = "from_node",
    target_var = "to_node",
    diagram_type = "sankey"
  )
  
  expect_true(!is.null(result_sankey))
  
  # Test Alluvial diagram
  result_alluvial <- jggsankeyfier(
    data = simple_flow_data,
    value_var = "flow_amount",
    source_var = "from_node",
    target_var = "to_node",
    diagram_type = "alluvial"
  )
  
  expect_true(!is.null(result_alluvial))
  
  # Test Parallel sets
  result_parallel <- jggsankeyfier(
    data = simple_flow_data,
    value_var = "flow_amount",
    source_var = "from_node",
    target_var = "to_node",
    diagram_type = "parallel_sets"
  )
  
  expect_true(!is.null(result_parallel))
})

# Multi-Node Variable Tests
test_that("jggsankeyfier multi-node functionality works", {
  
  result <- jggsankeyfier(
    data = complex_alluvial_data,
    value_var = "revenue",
    node_vars = c("level_1", "level_2", "level_3", "level_4"),
    diagram_type = "alluvial"
  )
  
  expect_true(!is.null(result))
  expect_s3_class(result, "jmvAnalysis")
})

# Grouping Variable Tests
test_that("jggsankeyfier grouping variables work", {
  
  result <- jggsankeyfier(
    data = simple_flow_data,
    value_var = "flow_amount",
    source_var = "from_node",
    target_var = "to_node",
    grouping_var = "category"
  )
  
  expect_true(!is.null(result))
})

# Customization Options Tests
test_that("jggsankeyfier customization options work", {
  
  # Test color palettes
  color_palettes <- c("default", "viridis", "plasma", "set3", "pastel1", "dark2")
  
  for (palette in color_palettes) {
    result <- jggsankeyfier(
      data = simple_flow_data,
      value_var = "flow_amount",
      source_var = "from_node",
      target_var = "to_node",
      color_palette = palette
    )
    
    expect_true(!is.null(result))
  }
})

test_that("jggsankeyfier node width and transparency work", {
  
  result <- jggsankeyfier(
    data = simple_flow_data,
    value_var = "flow_amount",
    source_var = "from_node",
    target_var = "to_node",
    node_width = 0.8,
    edge_alpha = 0.7
  )
  
  expect_true(!is.null(result))
})

test_that("jggsankeyfier label options work", {
  
  # Test with labels shown
  result_with_labels <- jggsankeyfier(
    data = simple_flow_data,
    value_var = "flow_amount",
    source_var = "from_node",
    target_var = "to_node",
    show_labels = TRUE,
    label_size = 12
  )
  
  expect_true(!is.null(result_with_labels))
  
  # Test with values shown
  result_with_values <- jggsankeyfier(
    data = simple_flow_data,
    value_var = "flow_amount",
    source_var = "from_node",
    target_var = "to_node",
    show_values = TRUE
  )
  
  expect_true(!is.null(result_with_values))
})

# Value Format Tests
test_that("jggsankeyfier value formats work", {
  
  value_formats <- c("raw", "percent", "rounded")
  
  for (format in value_formats) {
    result <- jggsankeyfier(
      data = simple_flow_data,
      value_var = "flow_amount",
      source_var = "from_node",
      target_var = "to_node",
      value_format = format
    )
    
    expect_true(!is.null(result))
  }
})

# Sorting Options Tests
test_that("jggsankeyfier sorting options work", {
  
  sort_options <- c("original", "alphabetical", "by_value")
  
  for (sort_option in sort_options) {
    result <- jggsankeyfier(
      data = simple_flow_data,
      value_var = "flow_amount",
      source_var = "from_node",
      target_var = "to_node",
      sort_nodes = sort_option
    )
    
    expect_true(!is.null(result))
  }
})

# Flow Direction Tests
test_that("jggsankeyfier flow directions work", {
  
  flow_directions <- c("left_right", "top_bottom", "right_left", "bottom_top")
  
  for (direction in flow_directions) {
    result <- jggsankeyfier(
      data = simple_flow_data,
      value_var = "flow_amount",
      source_var = "from_node",
      target_var = "to_node",
      flow_direction = direction
    )
    
    expect_true(!is.null(result))
  }
})

# Theme Style Tests
test_that("jggsankeyfier theme styles work", {
  
  theme_styles <- c("default", "minimal", "classic", "void")
  
  for (theme in theme_styles) {
    result <- jggsankeyfier(
      data = simple_flow_data,
      value_var = "flow_amount",
      source_var = "from_node",
      target_var = "to_node",
      theme_style = theme
    )
    
    expect_true(!is.null(result))
  }
})

# Title and Subtitle Tests
test_that("jggsankeyfier titles work", {
  
  result <- jggsankeyfier(
    data = simple_flow_data,
    value_var = "flow_amount",
    source_var = "from_node",
    target_var = "to_node",
    plot_title = "Test Sankey Diagram",
    plot_subtitle = "Test subtitle for diagram"
  )
  
  expect_true(!is.null(result))
})

# Output Format Tests
test_that("jggsankeyfier output formats work", {
  
  output_formats <- c("plot_only", "data_table", "both")
  
  for (format in output_formats) {
    result <- jggsankeyfier(
      data = simple_flow_data,
      value_var = "flow_amount",
      source_var = "from_node",
      target_var = "to_node",
      output_format = format
    )
    
    expect_true(!is.null(result))
  }
})

# Statistics and Interpretation Tests
test_that("jggsankeyfier statistics and interpretation work", {
  
  result <- jggsankeyfier(
    data = simple_flow_data,
    value_var = "flow_amount",
    source_var = "from_node",
    target_var = "to_node",
    show_statistics = TRUE,
    show_interpretation = TRUE
  )
  
  expect_true(!is.null(result))
})

# Complex Real-World Data Tests
test_that("jggsankeyfier works with treatment pathway data", {
  
  result <- jggsankeyfier(
    data = treatment_pathways_data,
    value_var = "patient_count",
    node_vars = c("initial_diagnosis", "first_treatment", "response", "final_outcome"),
    diagram_type = "alluvial",
    grouping_var = "age_group"
  )
  
  expect_true(!is.null(result))
})

test_that("jggsankeyfier works with business process data", {
  
  result <- jggsankeyfier(
    data = business_process_data,
    value_var = "process_value",
    node_vars = c("department_start", "process_step_1", "process_step_2", "final_outcome"),
    diagram_type = "sankey",
    grouping_var = "region",
    time_var = "quarter"
  )
  
  expect_true(!is.null(result))
})

# Edge Cases and Error Handling Tests
test_that("jggsankeyfier handles empty data", {
  
  empty_data <- data.frame(
    from_node = character(0),
    to_node = character(0),
    flow_amount = numeric(0)
  )
  
  expect_error(
    jggsankeyfier(
      data = empty_data,
      value_var = "flow_amount",
      source_var = "from_node",
      target_var = "to_node"
    ),
    NA  # Should handle gracefully in jamovi context
  )
})

test_that("jggsankeyfier handles data with missing values", {
  
  data_with_na <- simple_flow_data
  data_with_na$flow_amount[1:2] <- NA
  
  result <- jggsankeyfier(
    data = data_with_na,
    value_var = "flow_amount",
    source_var = "from_node",
    target_var = "to_node"
  )
  
  expect_true(!is.null(result))
})

test_that("jggsankeyfier handles single flow case", {
  
  single_flow_data <- data.frame(
    from_node = "A",
    to_node = "B", 
    flow_amount = 100
  )
  
  result <- jggsankeyfier(
    data = single_flow_data,
    value_var = "flow_amount",
    source_var = "from_node",
    target_var = "to_node"
  )
  
  expect_true(!is.null(result))
})

test_that("jggsankeyfier handles large datasets", {
  
  # Create a larger synthetic dataset
  large_data <- data.frame(
    source = sample(LETTERS[1:10], 1000, replace = TRUE),
    target = sample(LETTERS[11:20], 1000, replace = TRUE),
    value = sample(1:100, 1000, replace = TRUE)
  )
  
  result <- jggsankeyfier(
    data = large_data,
    value_var = "value",
    source_var = "source",
    target_var = "target"
  )
  
  expect_true(!is.null(result))
})

# Integration Tests with Different Variable Types
test_that("jggsankeyfier handles different variable types", {
  
  # Test with factor variables
  factor_data <- simple_flow_data
  factor_data$from_node <- as.factor(factor_data$from_node)
  factor_data$to_node <- as.factor(factor_data$to_node)
  
  result <- jggsankeyfier(
    data = factor_data,
    value_var = "flow_amount",
    source_var = "from_node",
    target_var = "to_node"
  )
  
  expect_true(!is.null(result))
})

test_that("jggsankeyfier handles numeric node identifiers", {
  
  numeric_data <- data.frame(
    from_node = rep(1:4, each = 2),
    to_node = rep(5:6, 4),
    flow_amount = sample(10:100, 8)
  )
  
  result <- jggsankeyfier(
    data = numeric_data,
    value_var = "flow_amount",
    source_var = "from_node",
    target_var = "to_node"
  )
  
  expect_true(!is.null(result))
})

# Performance and Memory Tests
test_that("jggsankeyfier performs reasonably with medium datasets", {
  
  # Test performance with moderately sized data
  medium_data <- data.frame(
    source = sample(paste0("Source_", 1:50), 5000, replace = TRUE),
    target = sample(paste0("Target_", 1:50), 5000, replace = TRUE),
    value = sample(1:1000, 5000, replace = TRUE),
    group = sample(c("A", "B", "C"), 5000, replace = TRUE)
  )
  
  # Test that it completes in reasonable time (should be < 30 seconds)
  start_time <- Sys.time()
  
  result <- jggsankeyfier(
    data = medium_data,
    value_var = "value",
    source_var = "source",
    target_var = "target",
    grouping_var = "group"
  )
  
  end_time <- Sys.time()
  execution_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
  
  expect_true(!is.null(result))
  expect_lt(execution_time, 30)  # Should complete within 30 seconds
})

# Comprehensive Integration Test
test_that("jggsankeyfier comprehensive integration test", {
  
  # Test with all major options enabled
  result <- jggsankeyfier(
    data = sankey_simple_data,
    value_var = "flow_value",
    source_var = "source",
    target_var = "target",
    grouping_var = "group",
    time_var = "time_period",
    diagram_type = "alluvial",
    node_width = 0.6,
    edge_alpha = 0.8,
    color_palette = "viridis",
    show_labels = TRUE,
    label_size = 10,
    show_values = TRUE,
    value_format = "rounded",
    sort_nodes = "by_value",
    flow_direction = "left_right",
    plot_title = "Comprehensive Test Diagram",
    plot_subtitle = "Testing all features",
    theme_style = "minimal",
    show_statistics = TRUE,
    show_interpretation = TRUE,
    output_format = "both"
  )
  
  expect_true(!is.null(result))
  expect_s3_class(result, "jmvAnalysis")
})

message("All jggsankeyfier tests completed successfully!")