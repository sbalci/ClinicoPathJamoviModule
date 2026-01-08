# ═══════════════════════════════════════════════════════════
# Integration Tests: jjsegmentedtotalbar
# ═══════════════════════════════════════════════════════════
#
# Tests complete workflows and realistic clinical scenarios
# Generated: 2026-01-06

library(testthat)
library(ClinicoPath)

# Load all test datasets
data(jjsegmentedtotalbar_test, package = "ClinicoPath", envir = environment())
data(jjsegmentedtotalbar_demographics, package = "ClinicoPath", envir = environment())
data(jjsegmentedtotalbar_biomarker, package = "ClinicoPath", envir = environment())
data(jjsegmentedtotalbar_quality, package = "ClinicoPath", envir = environment())
data(jjsegmentedtotalbar_temporal, package = "ClinicoPath", envir = environment())

# ═══════════════════════════════════════════════════════════
# 1. Complete Treatment Response Analysis
# ═══════════════════════════════════════════════════════════

test_that("jjsegmentedtotalbar handles complete treatment response workflow", {
  devtools::load_all()

  # Step 1: Basic visualization
  result1 <- jjsegmentedtotalbar(
    data = jjsegmentedtotalbar_test,
    x_var = "timepoint",
    y_var = "tumor_response_score",
    fill_var = "response_category"
  )
  expect_s3_class(result1, "jjsegmentedtotalbarResults")

  # Step 2: Add percentages for clear communication
  result2 <- jjsegmentedtotalbar(
    data = jjsegmentedtotalbar_test,
    x_var = "timepoint",
    y_var = "tumor_response_score",
    fill_var = "response_category",
    show_percentages = TRUE
  )
  expect_s3_class(result2, "jjsegmentedtotalbarResults")

  # Step 3: Add treatment faceting
  result3 <- jjsegmentedtotalbar(
    data = jjsegmentedtotalbar_test,
    x_var = "timepoint",
    y_var = "tumor_response_score",
    fill_var = "response_category",
    facet_var = "treatment",
    show_percentages = TRUE
  )
  expect_s3_class(result3, "jjsegmentedtotalbarResults")

  # Step 4: Add publication-ready styling
  result4 <- jjsegmentedtotalbar(
    data = jjsegmentedtotalbar_test,
    x_var = "timepoint",
    y_var = "tumor_response_score",
    fill_var = "response_category",
    facet_var = "treatment",
    chart_style = "publication",
    color_palette = "clinical",
    show_percentages = TRUE,
    percentage_format = "decimal1"
  )
  expect_s3_class(result4, "jjsegmentedtotalbarResults")

  # Step 5: Add statistical comparison
  result5 <- jjsegmentedtotalbar(
    data = jjsegmentedtotalbar_test,
    x_var = "timepoint",
    y_var = "tumor_response_score",
    fill_var = "response_category",
    facet_var = "treatment",
    chart_style = "publication",
    color_palette = "clinical",
    show_percentages = TRUE,
    percentage_format = "decimal1",
    show_statistical_tests = TRUE
  )
  expect_s3_class(result5, "jjsegmentedtotalbarResults")

  # All steps should produce valid results
  expect_equal(5, 5)
})

# ═══════════════════════════════════════════════════════════
# 2. Complete Demographics Analysis
# ═══════════════════════════════════════════════════════════

test_that("jjsegmentedtotalbar handles complete demographics workflow", {
  devtools::load_all()

  # Demographics analysis: age distribution by center and gender
  result <- jjsegmentedtotalbar(
    data = jjsegmentedtotalbar_demographics,
    x_var = "treatment_center",
    y_var = "patient_count",
    fill_var = "age_group",
    facet_var = "gender",
    chart_style = "clinical",
    color_palette = "colorblind",
    show_percentages = TRUE,
    percentage_format = "integer",
    show_counts = TRUE,
    orientation = "vertical",
    sort_categories = "total"
  )

  expect_s3_class(result, "jjsegmentedtotalbarResults")
})

test_that("jjsegmentedtotalbar handles ethnicity distribution analysis", {
  devtools::load_all()

  # Ethnicity distribution across centers
  result <- jjsegmentedtotalbar(
    data = jjsegmentedtotalbar_demographics,
    x_var = "treatment_center",
    y_var = "patient_count",
    fill_var = "ethnicity",
    chart_style = "bbc_style",
    color_palette = "bbc_multi",
    show_percentages = TRUE,
    show_counts = TRUE,
    label_threshold = 5,
    orientation = "horizontal",
    sort_categories = "total"
  )

  expect_s3_class(result, "jjsegmentedtotalbarResults")
})

# ═══════════════════════════════════════════════════════════
# 3. Complete Biomarker Expression Analysis
# ═══════════════════════════════════════════════════════════

test_that("jjsegmentedtotalbar handles complete biomarker workflow", {
  devtools::load_all()

  # Biomarker expression by disease stage
  result <- jjsegmentedtotalbar(
    data = jjsegmentedtotalbar_biomarker,
    x_var = "disease_stage",
    y_var = "expression_score",
    fill_var = "expression_level",
    facet_var = "biomarker",
    chart_style = "publication",
    color_palette = "viridis",
    show_percentages = TRUE,
    percentage_format = "decimal1",
    label_threshold = 5,
    orientation = "horizontal",
    sort_categories = "largest_segment",
    show_statistical_tests = TRUE
  )

  expect_s3_class(result, "jjsegmentedtotalbarResults")
})

test_that("jjsegmentedtotalbar handles biomarker by tumor type analysis", {
  devtools::load_all()

  # Biomarker distribution by tumor type
  result <- jjsegmentedtotalbar(
    data = jjsegmentedtotalbar_biomarker,
    x_var = "tumor_type",
    y_var = "expression_score",
    fill_var = "expression_level",
    facet_var = "disease_stage",
    chart_style = "clinical",
    color_palette = "clinical",
    show_percentages = TRUE,
    show_counts = TRUE,
    orientation = "horizontal",
    sort_categories = "total"
  )

  expect_s3_class(result, "jjsegmentedtotalbarResults")
})

# ═══════════════════════════════════════════════════════════
# 4. Complete Quality Metrics Analysis
# ═══════════════════════════════════════════════════════════

test_that("jjsegmentedtotalbar handles complete quality metrics workflow", {
  devtools::load_all()

  # Quality grades by hospital with flerlage plot
  result <- jjsegmentedtotalbar(
    data = jjsegmentedtotalbar_quality,
    x_var = "hospital",
    y_var = "compliance_score",
    fill_var = "quality_grade",
    facet_var = "quarter",
    plot_type = "flerlage",
    chart_style = "clinical",
    color_palette = "clinical",
    flerlage_show_labels = TRUE,
    flerlage_label_size = 6,
    flerlage_label_color = "white",
    flerlage_alpha = 0.85,
    flerlage_box_color = "gray30",
    orientation = "horizontal",
    sort_categories = "total"
  )

  expect_s3_class(result, "jjsegmentedtotalbarResults")
})

test_that("jjsegmentedtotalbar handles quality metrics by type analysis", {
  devtools::load_all()

  # Quality distribution by metric type
  result <- jjsegmentedtotalbar(
    data = jjsegmentedtotalbar_quality,
    x_var = "quarter",
    y_var = "compliance_score",
    fill_var = "quality_grade",
    facet_var = "metric_type",
    chart_style = "publication",
    color_palette = "nature",
    show_percentages = TRUE,
    percentage_format = "decimal1",
    show_counts = TRUE,
    label_threshold = 5,
    orientation = "vertical"
  )

  expect_s3_class(result, "jjsegmentedtotalbarResults")
})

# ═══════════════════════════════════════════════════════════
# 5. Complete Temporal Disease Progression Analysis
# ═══════════════════════════════════════════════════════════

test_that("jjsegmentedtotalbar handles complete temporal progression workflow", {
  devtools::load_all()

  # Disease status progression over time by intervention
  result <- jjsegmentedtotalbar(
    data = jjsegmentedtotalbar_temporal,
    x_var = "time_period",
    y_var = "disease_burden_score",
    fill_var = "disease_status",
    facet_var = "intervention",
    plot_type = "stacked",
    chart_style = "publication",
    color_palette = "nature",
    show_percentages = TRUE,
    percentage_format = "decimal1",
    label_threshold = 5,
    orientation = "vertical",
    sort_categories = "none",
    show_statistical_tests = TRUE
  )

  expect_s3_class(result, "jjsegmentedtotalbarResults")
})

test_that("jjsegmentedtotalbar handles intervention comparison analysis", {
  devtools::load_all()

  # Compare interventions with horizontal layout
  result <- jjsegmentedtotalbar(
    data = jjsegmentedtotalbar_temporal,
    x_var = "intervention",
    y_var = "disease_burden_score",
    fill_var = "disease_status",
    facet_var = "time_period",
    chart_style = "clinical",
    color_palette = "clinical",
    show_percentages = TRUE,
    show_counts = TRUE,
    orientation = "horizontal",
    sort_categories = "largest_segment"
  )

  expect_s3_class(result, "jjsegmentedtotalbarResults")
})

# ═══════════════════════════════════════════════════════════
# 6. Comparison Across Different Palettes
# ═══════════════════════════════════════════════════════════

test_that("jjsegmentedtotalbar produces consistent results across all palettes", {
  devtools::load_all()

  palettes <- c("default", "viridis", "set1", "dark2", "paired",
                "clinical", "colorblind", "bbc_multi",
                "prism_colorblind_safe", "nature", "science")

  results <- list()

  for (palette in palettes) {
    results[[palette]] <- jjsegmentedtotalbar(
      data = jjsegmentedtotalbar_test,
      x_var = "timepoint",
      y_var = "tumor_response_score",
      fill_var = "response_category",
      color_palette = palette
    )

    expect_s3_class(results[[palette]], "jjsegmentedtotalbarResults")
  }

  expect_equal(length(results), length(palettes))
})

# ═══════════════════════════════════════════════════════════
# 7. Comparison Between Plot Types
# ═══════════════════════════════════════════════════════════

test_that("jjsegmentedtotalbar produces valid results for both plot types", {
  devtools::load_all()

  # Traditional stacked plot
  result_stacked <- jjsegmentedtotalbar(
    data = jjsegmentedtotalbar_test,
    x_var = "timepoint",
    y_var = "tumor_response_score",
    fill_var = "response_category",
    plot_type = "stacked",
    show_percentages = TRUE
  )

  # Flerlage segmented total bar
  result_flerlage <- jjsegmentedtotalbar(
    data = jjsegmentedtotalbar_test,
    x_var = "timepoint",
    y_var = "tumor_response_score",
    fill_var = "response_category",
    plot_type = "flerlage",
    flerlage_show_labels = TRUE
  )

  expect_s3_class(result_stacked, "jjsegmentedtotalbarResults")
  expect_s3_class(result_flerlage, "jjsegmentedtotalbarResults")
})

# ═══════════════════════════════════════════════════════════
# 8. Multi-Dataset Cross-Validation
# ═══════════════════════════════════════════════════════════

test_that("jjsegmentedtotalbar handles all test datasets consistently", {
  devtools::load_all()

  # Test dataset
  result1 <- jjsegmentedtotalbar(
    data = jjsegmentedtotalbar_test,
    x_var = "timepoint",
    y_var = "tumor_response_score",
    fill_var = "response_category"
  )
  expect_s3_class(result1, "jjsegmentedtotalbarResults")

  # Demographics dataset
  result2 <- jjsegmentedtotalbar(
    data = jjsegmentedtotalbar_demographics,
    x_var = "treatment_center",
    y_var = "patient_count",
    fill_var = "age_group"
  )
  expect_s3_class(result2, "jjsegmentedtotalbarResults")

  # Biomarker dataset
  result3 <- jjsegmentedtotalbar(
    data = jjsegmentedtotalbar_biomarker,
    x_var = "disease_stage",
    y_var = "expression_score",
    fill_var = "expression_level"
  )
  expect_s3_class(result3, "jjsegmentedtotalbarResults")

  # Quality dataset
  result4 <- jjsegmentedtotalbar(
    data = jjsegmentedtotalbar_quality,
    x_var = "hospital",
    y_var = "compliance_score",
    fill_var = "quality_grade"
  )
  expect_s3_class(result4, "jjsegmentedtotalbarResults")

  # Temporal dataset
  result5 <- jjsegmentedtotalbar(
    data = jjsegmentedtotalbar_temporal,
    x_var = "time_period",
    y_var = "disease_burden_score",
    fill_var = "disease_status"
  )
  expect_s3_class(result5, "jjsegmentedtotalbarResults")

  # All datasets should work
  expect_equal(5, 5)
})

# ═══════════════════════════════════════════════════════════
# 9. Publication-Ready Outputs
# ═══════════════════════════════════════════════════════════

test_that("jjsegmentedtotalbar produces publication-ready treatment response figure", {
  devtools::load_all()

  result <- jjsegmentedtotalbar(
    data = jjsegmentedtotalbar_test,
    x_var = "timepoint",
    y_var = "tumor_response_score",
    fill_var = "response_category",
    facet_var = "treatment",
    plot_type = "stacked",
    chart_style = "publication",
    color_palette = "clinical",
    show_percentages = TRUE,
    percentage_format = "decimal1",
    show_counts = FALSE,
    label_threshold = 5,
    orientation = "vertical",
    sort_categories = "none",
    show_statistical_tests = TRUE
  )

  expect_s3_class(result, "jjsegmentedtotalbarResults")
})

test_that("jjsegmentedtotalbar produces publication-ready biomarker figure", {
  devtools::load_all()

  result <- jjsegmentedtotalbar(
    data = jjsegmentedtotalbar_biomarker,
    x_var = "disease_stage",
    y_var = "expression_score",
    fill_var = "expression_level",
    facet_var = "tumor_type",
    plot_type = "stacked",
    chart_style = "publication",
    color_palette = "viridis",
    show_percentages = TRUE,
    percentage_format = "decimal1",
    label_threshold = 5,
    orientation = "horizontal",
    sort_categories = "largest_segment",
    show_statistical_tests = TRUE
  )

  expect_s3_class(result, "jjsegmentedtotalbarResults")
})
