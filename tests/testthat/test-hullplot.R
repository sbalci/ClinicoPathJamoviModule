## Test file for hullplot function
## Testing comprehensive hull plot visualization functionality

test_that("hullplot loads test data correctly", {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()
  # Load test datasets
  data(hullplot_customer_data, package = "ClinicoPath")
  data(hullplot_clinical_data, package = "ClinicoPath")
  data(hullplot_experimental_data, package = "ClinicoPath")
  data(hullplot_survey_data, package = "ClinicoPath")
  data(hullplot_quality_data, package = "ClinicoPath")
  
  # Test dataset dimensions
  expect_equal(nrow(hullplot_customer_data), 500)
  expect_equal(ncol(hullplot_customer_data), 10)
  
  expect_equal(nrow(hullplot_clinical_data), 400)
  expect_equal(ncol(hullplot_clinical_data), 11)
  
  expect_equal(nrow(hullplot_experimental_data), 350)
  expect_equal(ncol(hullplot_experimental_data), 11)
  
  expect_equal(nrow(hullplot_survey_data), 450)
  expect_equal(ncol(hullplot_survey_data), 11)
  
  expect_equal(nrow(hullplot_quality_data), 300)
  expect_equal(ncol(hullplot_quality_data), 12)
})

test_that("hullplot creates basic hull plots", {
  # Load test dataset
  data(hullplot_customer_data, package = "ClinicoPath")
  
  # Test basic hull plot creation
  expect_no_error({
    result <- ClinicoPath::hullplot(
      data = hullplot_customer_data,
      x_var = "annual_spending",
      y_var = "purchase_frequency",
      group_var = "segment"
    )
  })
})

test_that("hullplot handles customer segmentation data", {
  # Load test dataset
  data(hullplot_customer_data, package = "ClinicoPath")
  
  # Test customer segmentation hull plot
  expect_no_error({
    result <- ClinicoPath::hullplot(
      data = hullplot_customer_data,
      x_var = "annual_spending",
      y_var = "purchase_frequency",
      group_var = "segment",
      color_var = "preferred_category",
      size_var = "satisfaction",
      hull_alpha = 0.3,
      show_labels = TRUE
    )
  })
})

test_that("hullplot handles clinical clustering data", {
  # Load test dataset
  data(hullplot_clinical_data, package = "ClinicoPath")
  
  # Test clinical clustering hull plot
  expect_no_error({
    result <- ClinicoPath::hullplot(
      data = hullplot_clinical_data,
      x_var = "biomarker_x",
      y_var = "biomarker_y",
      group_var = "disease_subtype",
      color_var = "treatment_response",
      show_statistics = TRUE,
      outlier_detection = TRUE
    )
  })
})

test_that("hullplot handles experimental data", {
  # Load test dataset
  data(hullplot_experimental_data, package = "ClinicoPath")
  
  # Test experimental data hull plot
  expect_no_error({
    result <- ClinicoPath::hullplot(
      data = hullplot_experimental_data,
      x_var = "response_x",
      y_var = "response_y",
      group_var = "condition",
      color_var = "timepoint",
      size_var = "dose_level",
      confidence_ellipses = TRUE
    )
  })
})

test_that("hullplot handles survey data", {
  # Load test dataset
  data(hullplot_survey_data, package = "ClinicoPath")
  
  # Test survey data hull plot
  expect_no_error({
    result <- ClinicoPath::hullplot(
      data = hullplot_survey_data,
      x_var = "economic_conservatism",
      y_var = "social_conservatism",
      group_var = "political_orientation",
      color_var = "region",
      hull_concavity = 1.5,
      show_labels = TRUE
    )
  })
})

test_that("hullplot handles quality control data", {
  # Load test dataset
  data(hullplot_quality_data, package = "ClinicoPath")
  
  # Test quality control hull plot
  expect_no_error({
    result <- ClinicoPath::hullplot(
      data = hullplot_quality_data,
      x_var = "dimensional_accuracy",
      y_var = "surface_finish",
      group_var = "quality_group",
      color_var = "material_grade",
      show_statistics = TRUE
    )
  })
})

test_that("hullplot handles different hull parameters", {
  # Load test dataset
  data(hullplot_customer_data, package = "ClinicoPath")
  
  # Test different hull concavity values
  expect_no_error({
    result <- ClinicoPath::hullplot(
      data = hullplot_customer_data,
      x_var = "annual_spending",
      y_var = "purchase_frequency",
      group_var = "segment",
      hull_concavity = 0.5
    )
  })
  
  # Test different hull alpha values
  expect_no_error({
    result <- ClinicoPath::hullplot(
      data = hullplot_customer_data,
      x_var = "annual_spending",
      y_var = "purchase_frequency",
      group_var = "segment",
      hull_alpha = 0.8
    )
  })
  
  # Test different hull expansion
  expect_no_error({
    result <- ClinicoPath::hullplot(
      data = hullplot_customer_data,
      x_var = "annual_spending",
      y_var = "purchase_frequency",
      group_var = "segment",
      hull_expand = 0.2
    )
  })
})

test_that("hullplot handles different color palettes", {
  # Load test dataset
  data(hullplot_clinical_data, package = "ClinicoPath")
  
  # Test viridis palette
  expect_no_error({
    result <- ClinicoPath::hullplot(
      data = hullplot_clinical_data,
      x_var = "biomarker_x",
      y_var = "biomarker_y",
      group_var = "disease_subtype",
      color_palette = "viridis"
    )
  })
  
  # Test clinical palette
  expect_no_error({
    result <- ClinicoPath::hullplot(
      data = hullplot_clinical_data,
      x_var = "biomarker_x",
      y_var = "biomarker_y",
      group_var = "disease_subtype",
      color_palette = "clinical"
    )
  })
  
  # Test ColorBrewer palettes
  expect_no_error({
    result <- ClinicoPath::hullplot(
      data = hullplot_clinical_data,
      x_var = "biomarker_x",
      y_var = "biomarker_y",
      group_var = "disease_subtype",
      color_palette = "set1"
    )
  })
})

test_that("hullplot handles different plot themes", {
  # Load test dataset
  data(hullplot_survey_data, package = "ClinicoPath")
  
  # Test different themes
  themes_to_test <- c("minimal", "classic", "light", "dark", "clinical")
  
  for (theme in themes_to_test) {
    expect_no_error({
      result <- ClinicoPath::hullplot(
        data = hullplot_survey_data,
        x_var = "economic_conservatism",
        y_var = "social_conservatism",
        group_var = "political_orientation",
        plot_theme = theme
      )
    })
  }
})

test_that("hullplot handles point customization", {
  # Load test dataset
  data(hullplot_experimental_data, package = "ClinicoPath")
  
  # Test different point sizes
  expect_no_error({
    result <- ClinicoPath::hullplot(
      data = hullplot_experimental_data,
      x_var = "response_x",
      y_var = "response_y",
      group_var = "condition",
      point_size = 4
    )
  })
  
  # Test different point transparency
  expect_no_error({
    result <- ClinicoPath::hullplot(
      data = hullplot_experimental_data,
      x_var = "response_x",
      y_var = "response_y",
      group_var = "condition",
      point_alpha = 0.5
    )
  })
})

test_that("hullplot handles custom labels", {
  # Load test dataset
  data(hullplot_quality_data, package = "ClinicoPath")
  
  # Test custom labels and titles
  expect_no_error({
    result <- ClinicoPath::hullplot(
      data = hullplot_quality_data,
      x_var = "dimensional_accuracy",
      y_var = "surface_finish",
      group_var = "quality_group",
      plot_title = "Quality Control Analysis",
      x_label = "Dimensional Accuracy (%)",
      y_label = "Surface Finish Quality"
    )
  })
})

test_that("hullplot handles statistical features", {
  # Load test dataset
  data(hullplot_clinical_data, package = "ClinicoPath")
  
  # Test statistics generation
  expect_no_error({
    result <- ClinicoPath::hullplot(
      data = hullplot_clinical_data,
      x_var = "biomarker_x",
      y_var = "biomarker_y",
      group_var = "disease_subtype",
      show_statistics = TRUE
    )
  })
  
  # Test outlier detection
  expect_no_error({
    result <- ClinicoPath::hullplot(
      data = hullplot_clinical_data,
      x_var = "biomarker_x",
      y_var = "biomarker_y",
      group_var = "disease_subtype",
      outlier_detection = TRUE
    )
  })
  
  # Test confidence ellipses
  expect_no_error({
    result <- ClinicoPath::hullplot(
      data = hullplot_clinical_data,
      x_var = "biomarker_x",
      y_var = "biomarker_y",
      group_var = "disease_subtype",
      confidence_ellipses = TRUE
    )
  })
})

test_that("hullplot error handling works correctly", {
  # Load test dataset
  data(hullplot_customer_data, package = "ClinicoPath")
  
  # Test error handling for missing required variables
  expect_error({
    result <- ClinicoPath::hullplot(
      data = hullplot_customer_data,
      x_var = NULL,
      y_var = "purchase_frequency",
      group_var = "segment"
    )
  })
  
  # Test error handling for non-existent variables
  expect_error({
    result <- ClinicoPath::hullplot(
      data = hullplot_customer_data,
      x_var = "nonexistent_variable",
      y_var = "purchase_frequency",
      group_var = "segment"
    )
  })
})

test_that("hullplot validates data structure", {
  # Test with empty data
  expect_error({
    result <- ClinicoPath::hullplot(
      data = data.frame(),
      x_var = "x",
      y_var = "y",
      group_var = "group"
    )
  })
  
  # Test with valid data structure
  data(hullplot_customer_data, package = "ClinicoPath")
  expect_no_error({
    result <- ClinicoPath::hullplot(
      data = hullplot_customer_data,
      x_var = "annual_spending",
      y_var = "purchase_frequency",
      group_var = "segment"
    )
  })
})

test_that("hullplot handles complex grouping scenarios", {
  # Load test dataset
  data(hullplot_experimental_data, package = "ClinicoPath")
  
  # Test with many groups
  expect_no_error({
    result <- ClinicoPath::hullplot(
      data = hullplot_experimental_data,
      x_var = "response_x",
      y_var = "response_y",
      group_var = "condition",
      color_var = "batch",
      show_labels = TRUE
    )
  })
})

test_that("hullplot handles missing values appropriately", {
  # Create test data with missing values
  data(hullplot_customer_data, package = "ClinicoPath")
  test_data <- hullplot_customer_data
  test_data$annual_spending[1:5] <- NA
  test_data$purchase_frequency[6:10] <- NA
  
  # Test with missing values
  expect_no_error({
    result <- ClinicoPath::hullplot(
      data = test_data,
      x_var = "annual_spending",
      y_var = "purchase_frequency",
      group_var = "segment"
    )
  })
})

test_that("hullplot integration features work correctly", {
  # Load test dataset
  data(hullplot_survey_data, package = "ClinicoPath")
  
  # Test all features together
  expect_no_error({
    result <- ClinicoPath::hullplot(
      data = hullplot_survey_data,
      x_var = "economic_conservatism",
      y_var = "social_conservatism",
      group_var = "political_orientation",
      color_var = "region",
      size_var = "institutional_trust",
      hull_concavity = 1.8,
      hull_alpha = 0.4,
      hull_expand = 0.08,
      show_labels = TRUE,
      show_statistics = TRUE,
      outlier_detection = TRUE,
      confidence_ellipses = TRUE,
      color_palette = "viridis",
      plot_theme = "clinical",
      plot_title = "Political Orientation Analysis",
      x_label = "Economic Conservatism",
      y_label = "Social Conservatism"
    )
  })
})

test_that("hullplot handles edge cases", {
  # Create minimal dataset
  minimal_data <- data.frame(
    x = c(1, 2, 3, 4, 5),
    y = c(1, 2, 3, 4, 5),
    group = factor(c("A", "A", "B", "B", "B"))
  )
  
  # Test with minimal data
  expect_no_error({
    result <- ClinicoPath::hullplot(
      data = minimal_data,
      x_var = "x",
      y_var = "y",
      group_var = "group"
    )
  })
})

test_that("hullplot parameter validation works", {
  # Load test dataset
  data(hullplot_customer_data, package = "ClinicoPath")
  
  # Test valid parameter ranges
  expect_no_error({
    result <- ClinicoPath::hullplot(
      data = hullplot_customer_data,
      x_var = "annual_spending",
      y_var = "purchase_frequency",
      group_var = "segment",
      hull_concavity = 0,  # Min value
      hull_alpha = 1,     # Max value
      point_alpha = 0.1   # Low value
    )
  })
})
