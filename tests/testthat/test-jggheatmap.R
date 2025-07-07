# Comprehensive tests for jggheatmap function
library(ClinicoPath)

# Load test datasets
data(histopathology, package = "ClinicoPath")

test_that("jggheatmap module loads correctly", {
  expect_true(exists("jggheatmapClass"))
  expect_true(is.function(jggheatmap))
})

test_that("jggheatmap handles basic input validation", {
  # Test with no variables specified
  expect_error(
    jggheatmap(data = histopathology),
    NA  # Should not error during initialization
  )
  
  # Test with no data
  expect_error(
    jggheatmap(data = NULL, matrix_vars = c("Age", "TStage")),
    NA  # Should not error during initialization
  )
})

test_that("jggheatmap works with matrix variables approach", {
  # Create numeric subset for matrix
  numeric_vars <- c("Age", "TStage", "Grade")
  
  result <- jggheatmap(
    data = histopathology,
    matrix_vars = numeric_vars,
    cluster_rows = TRUE,
    cluster_cols = TRUE,
    scaling = "none",
    color_scheme = "blue_red"
  )
  
  expect_s3_class(result, "jggheatmapClass")
  expect_true(all(numeric_vars %in% names(histopathology)))
})

test_that("jggheatmap works with pivot approach (row/col/value)", {
  # Create test data for pivot approach
  pivot_data <- histopathology[1:50, ] %>%
    dplyr::select(PatientID, Grade, TStage) %>%
    dplyr::mutate(
      PatientID = paste0("Patient_", PatientID),
      Grade = paste0("Grade_", Grade),
      TStage = as.numeric(TStage)
    )
  
  result <- jggheatmap(
    data = pivot_data,
    row_var = "PatientID",
    col_var = "Grade", 
    value_var = "TStage",
    cluster_rows = FALSE,
    cluster_cols = FALSE
  )
  
  expect_s3_class(result, "jggheatmapClass")
})

test_that("jggheatmap handles different scaling options", {
  numeric_vars <- c("Age", "TStage", "Grade")
  
  # Test row scaling
  result_row <- jggheatmap(
    data = histopathology,
    matrix_vars = numeric_vars,
    scaling = "row"
  )
  
  expect_s3_class(result_row, "jggheatmapClass")
  
  # Test column scaling
  result_col <- jggheatmap(
    data = histopathology,
    matrix_vars = numeric_vars,
    scaling = "column"
  )
  
  expect_s3_class(result_col, "jggheatmapClass")
  
  # Test global scaling
  result_global <- jggheatmap(
    data = histopathology,
    matrix_vars = numeric_vars,
    scaling = "global"
  )
  
  expect_s3_class(result_global, "jggheatmapClass")
})

test_that("jggheatmap handles different color schemes", {
  numeric_vars <- c("Age", "TStage", "Grade")
  
  # Test viridis
  result_viridis <- jggheatmap(
    data = histopathology,
    matrix_vars = numeric_vars,
    color_scheme = "viridis"
  )
  
  expect_s3_class(result_viridis, "jggheatmapClass")
  
  # Test plasma
  result_plasma <- jggheatmap(
    data = histopathology,
    matrix_vars = numeric_vars,
    color_scheme = "plasma"
  )
  
  expect_s3_class(result_plasma, "jggheatmapClass")
  
  # Test spectral
  result_spectral <- jggheatmap(
    data = histopathology,
    matrix_vars = numeric_vars,
    color_scheme = "spectral"
  )
  
  expect_s3_class(result_spectral, "jggheatmapClass")
})

test_that("jggheatmap handles clustering options", {
  numeric_vars <- c("Age", "TStage", "Grade")
  
  # Test different clustering methods
  clustering_methods <- c("complete", "average", "single", "ward.D2")
  
  for (method in clustering_methods) {
    result <- jggheatmap(
      data = histopathology,
      matrix_vars = numeric_vars,
      cluster_rows = TRUE,
      cluster_cols = TRUE,
      clustering_method = method
    )
    
    expect_s3_class(result, "jggheatmapClass")
  }
})

test_that("jggheatmap handles different distance methods", {
  numeric_vars <- c("Age", "TStage", "Grade")
  
  # Test different distance methods
  distance_methods <- c("euclidean", "manhattan", "maximum", "pearson", "spearman")
  
  for (method in distance_methods) {
    result <- jggheatmap(
      data = histopathology,
      matrix_vars = numeric_vars,
      cluster_rows = TRUE,
      cluster_cols = TRUE,
      distance_method = method
    )
    
    expect_s3_class(result, "jggheatmapClass")
  }
})

test_that("jggheatmap handles cell value display options", {
  numeric_vars <- c("Age", "TStage", "Grade")
  
  # Test showing cell values
  result_values <- jggheatmap(
    data = histopathology,
    matrix_vars = numeric_vars,
    show_values = TRUE,
    value_format = "decimal2",
    text_size = 8
  )
  
  expect_s3_class(result_values, "jggheatmapClass")
  
  # Test different value formats
  value_formats <- c("auto", "integer", "decimal1", "decimal2", "scientific")
  
  for (format_type in value_formats) {
    result <- jggheatmap(
      data = histopathology,
      matrix_vars = numeric_vars,
      show_values = TRUE,
      value_format = format_type
    )
    
    expect_s3_class(result, "jggheatmapClass")
  }
})

test_that("jggheatmap handles label display options", {
  numeric_vars <- c("Age", "TStage", "Grade")
  
  # Test hiding row labels
  result_no_row <- jggheatmap(
    data = histopathology,
    matrix_vars = numeric_vars,
    show_row_labels = FALSE
  )
  
  expect_s3_class(result_no_row, "jggheatmapClass")
  
  # Test hiding column labels
  result_no_col <- jggheatmap(
    data = histopathology,
    matrix_vars = numeric_vars,
    show_col_labels = FALSE
  )
  
  expect_s3_class(result_no_col, "jggheatmapClass")
  
  # Test label sizes
  result_sizes <- jggheatmap(
    data = histopathology,
    matrix_vars = numeric_vars,
    row_label_size = 12,
    col_label_size = 14
  )
  
  expect_s3_class(result_sizes, "jggheatmapClass")
})

test_that("jggheatmap handles plot customization", {
  numeric_vars <- c("Age", "TStage", "Grade")
  
  # Test plot title and dimensions
  result_custom <- jggheatmap(
    data = histopathology,
    matrix_vars = numeric_vars,
    plot_title = "Custom Heatmap Title",
    plot_width = 800,
    plot_height = 700
  )
  
  expect_s3_class(result_custom, "jggheatmapClass")
  expect_equal(result_custom$options$plot_title, "Custom Heatmap Title")
  expect_equal(result_custom$options$plot_width, 800)
  expect_equal(result_custom$options$plot_height, 700)
})

test_that("jggheatmap handles colorbar options", {
  numeric_vars <- c("Age", "TStage", "Grade")
  
  # Test hiding colorbar
  result_no_colorbar <- jggheatmap(
    data = histopathology,
    matrix_vars = numeric_vars,
    show_colorbar = FALSE
  )
  
  expect_s3_class(result_no_colorbar, "jggheatmapClass")
  
  # Test custom colorbar title
  result_colorbar_title <- jggheatmap(
    data = histopathology,
    matrix_vars = numeric_vars,
    colorbar_title = "Expression Level"
  )
  
  expect_s3_class(result_colorbar_title, "jggheatmapClass")
  expect_equal(result_colorbar_title$options$colorbar_title, "Expression Level")
})

test_that("jggheatmap handles output format options", {
  numeric_vars <- c("Age", "TStage", "Grade")
  
  # Test plot only
  result_plot_only <- jggheatmap(
    data = histopathology,
    matrix_vars = numeric_vars,
    output_format = "plot_only"
  )
  
  expect_s3_class(result_plot_only, "jggheatmapClass")
  
  # Test data matrix output
  result_data_matrix <- jggheatmap(
    data = histopathology,
    matrix_vars = numeric_vars,
    output_format = "data_matrix"
  )
  
  expect_s3_class(result_data_matrix, "jggheatmapClass")
  
  # Test both outputs
  result_both <- jggheatmap(
    data = histopathology,
    matrix_vars = numeric_vars,
    output_format = "both"
  )
  
  expect_s3_class(result_both, "jggheatmapClass")
})

test_that("jggheatmap handles cell appearance options", {
  numeric_vars <- c("Age", "TStage", "Grade")
  
  # Test different cell shapes
  cell_shapes <- c("square", "circle", "triangle")
  
  for (shape in cell_shapes) {
    result <- jggheatmap(
      data = histopathology,
      matrix_vars = numeric_vars,
      cell_shape = shape
    )
    
    expect_s3_class(result, "jggheatmapClass")
    expect_equal(result$options$cell_shape, shape)
  }
  
  # Test border and NA colors
  result_colors <- jggheatmap(
    data = histopathology,
    matrix_vars = numeric_vars,
    border_color = "black",
    na_color = "gray50"
  )
  
  expect_s3_class(result_colors, "jggheatmapClass")
  expect_equal(result_colors$options$border_color, "black")
  expect_equal(result_colors$options$na_color, "gray50")
})

test_that("jggheatmap handles dendrogram options", {
  numeric_vars <- c("Age", "TStage", "Grade")
  
  # Test showing dendrograms
  result_dendro <- jggheatmap(
    data = histopathology,
    matrix_vars = numeric_vars,
    cluster_rows = TRUE,
    cluster_cols = TRUE,
    show_dendrograms = TRUE,
    dendrogram_height = 0.3
  )
  
  expect_s3_class(result_dendro, "jggheatmapClass")
  expect_true(result_dendro$options$show_dendrograms)
  expect_equal(result_dendro$options$dendrogram_height, 0.3)
})

test_that("jggheatmap handles annotation options", {
  numeric_vars <- c("Age", "TStage", "Grade")
  
  # Test annotation variables
  annotation_colors <- c("default", "set1", "dark2", "paired")
  
  for (color_set in annotation_colors) {
    result <- jggheatmap(
      data = histopathology,
      matrix_vars = numeric_vars,
      annotation_var = "Grade",
      annotation_colors = color_set
    )
    
    expect_s3_class(result, "jggheatmapClass")
    expect_equal(result$options$annotation_colors, color_set)
  }
})

test_that("jggheatmap handles edge cases", {
  # Test with small dataset
  small_data <- histopathology[1:10, c("Age", "TStage", "Grade")]
  
  expect_error({
    result_small <- jggheatmap(
      data = small_data,
      matrix_vars = c("Age", "TStage", "Grade")
    )
  }, NA)
  
  # Test with missing values
  missing_data <- histopathology[1:20, c("Age", "TStage", "Grade")]
  missing_data$Age[1:5] <- NA
  
  expect_error({
    result_missing <- jggheatmap(
      data = missing_data,
      matrix_vars = c("Age", "TStage", "Grade"),
      na_color = "red"
    )
  }, NA)
})

test_that("jggheatmap validates data types", {
  # Test with mixed data types
  mixed_data <- histopathology[1:20, c("PatientID", "Age", "Grade", "LVI")]
  
  # Should handle numeric variables only
  expect_error({
    result_mixed <- jggheatmap(
      data = mixed_data,
      matrix_vars = c("Age", "Grade")  # Only numeric
    )
  }, NA)
})

test_that("jggheatmap comprehensive test", {
  # Test with all major features enabled
  numeric_vars <- c("Age", "TStage", "Grade")
  
  result_comprehensive <- jggheatmap(
    data = histopathology,
    matrix_vars = numeric_vars,
    cluster_rows = TRUE,
    cluster_cols = TRUE,
    clustering_method = "ward.D2",
    distance_method = "euclidean",
    scaling = "row",
    color_scheme = "viridis",
    cell_shape = "square",
    show_values = TRUE,
    value_format = "decimal2",
    text_size = 8,
    show_row_labels = TRUE,
    show_col_labels = TRUE,
    row_label_size = 10,
    col_label_size = 10,
    show_dendrograms = TRUE,
    dendrogram_height = 0.2,
    plot_title = "Comprehensive Heatmap Analysis",
    plot_width = 800,
    plot_height = 800,
    show_colorbar = TRUE,
    colorbar_title = "Z-Score",
    border_color = "white",
    na_color = "grey90",
    output_format = "both"
  )
  
  expect_s3_class(result_comprehensive, "jggheatmapClass")
  
  # Verify all options are set correctly
  expect_true(result_comprehensive$options$cluster_rows)
  expect_true(result_comprehensive$options$cluster_cols)
  expect_equal(result_comprehensive$options$clustering_method, "ward.D2")
  expect_equal(result_comprehensive$options$distance_method, "euclidean")
  expect_equal(result_comprehensive$options$scaling, "row")
  expect_equal(result_comprehensive$options$color_scheme, "viridis")
  expect_equal(result_comprehensive$options$output_format, "both")
})

test_that("jggheatmap performance with different data sizes", {
  # Test with medium dataset
  medium_vars <- c("Age", "TStage", "Grade", "PNI", "LVI")
  
  expect_error({
    result_medium <- jggheatmap(
      data = histopathology,
      matrix_vars = medium_vars,
      cluster_rows = TRUE,
      cluster_cols = TRUE
    )
  }, NA)
  
  # Test with correlation matrix approach
  correlation_data <- cor(histopathology[c("Age", "TStage", "Grade")], use = "complete.obs")
  correlation_df <- as.data.frame(correlation_data)
  correlation_df$Row <- rownames(correlation_df)
  
  # Convert to long format for pivot approach
  correlation_long <- correlation_df %>%
    tidyr::pivot_longer(cols = -Row, names_to = "Column", values_to = "Correlation")
  
  expect_error({
    result_correlation <- jggheatmap(
      data = correlation_long,
      row_var = "Row",
      col_var = "Column",
      value_var = "Correlation",
      cluster_rows = FALSE,
      cluster_cols = FALSE,
      color_scheme = "blue_red"
    )
  }, NA)
})