# ═══════════════════════════════════════════════════════════
# Basic Functionality Tests: jjsegmentedtotalbar
# ═══════════════════════════════════════════════════════════
#
# Tests core functionality with single parameters
# Generated: 2026-01-06

library(testthat)
library(ClinicoPath)

# Load test datasets
data(jjsegmentedtotalbar_test, package = "ClinicoPath", envir = environment())
data(jjsegmentedtotalbar_demographics, package = "ClinicoPath", envir = environment())
data(jjsegmentedtotalbar_biomarker, package = "ClinicoPath", envir = environment())
data(jjsegmentedtotalbar_quality, package = "ClinicoPath", envir = environment())
data(jjsegmentedtotalbar_temporal, package = "ClinicoPath", envir = environment())

# ═══════════════════════════════════════════════════════════
# 1. Basic Function Execution
# ═══════════════════════════════════════════════════════════

test_that("jjsegmentedtotalbar function exists and runs with minimal arguments", {
  devtools::load_all()

  result <- jjsegmentedtotalbar(
    data = jjsegmentedtotalbar_test,
    x_var = "timepoint",
    y_var = "tumor_response_score",
    fill_var = "response_category"
  )

  expect_s3_class(result, "jjsegmentedtotalbarResults")
})

test_that("jjsegmentedtotalbar handles demographics data", {
  devtools::load_all()

  result <- jjsegmentedtotalbar(
    data = jjsegmentedtotalbar_demographics,
    x_var = "treatment_center",
    y_var = "patient_count",
    fill_var = "age_group"
  )

  expect_s3_class(result, "jjsegmentedtotalbarResults")
})

test_that("jjsegmentedtotalbar handles biomarker data", {
  devtools::load_all()

  result <- jjsegmentedtotalbar(
    data = jjsegmentedtotalbar_biomarker,
    x_var = "disease_stage",
    y_var = "expression_score",
    fill_var = "expression_level"
  )

  expect_s3_class(result, "jjsegmentedtotalbarResults")
})

# ═══════════════════════════════════════════════════════════
# 2. Plot Types
# ═══════════════════════════════════════════════════════════

test_that("jjsegmentedtotalbar handles stacked plot type", {
  devtools::load_all()

  result <- jjsegmentedtotalbar(
    data = jjsegmentedtotalbar_test,
    x_var = "timepoint",
    y_var = "tumor_response_score",
    fill_var = "response_category",
    plot_type = "stacked"
  )

  expect_s3_class(result, "jjsegmentedtotalbarResults")
})

test_that("jjsegmentedtotalbar handles flerlage plot type", {
  devtools::load_all()

  result <- jjsegmentedtotalbar(
    data = jjsegmentedtotalbar_test,
    x_var = "timepoint",
    y_var = "tumor_response_score",
    fill_var = "response_category",
    plot_type = "flerlage"
  )

  expect_s3_class(result, "jjsegmentedtotalbarResults")
})

# ═══════════════════════════════════════════════════════════
# 3. Chart Styles
# ═══════════════════════════════════════════════════════════

test_that("jjsegmentedtotalbar handles clean chart style", {
  devtools::load_all()

  result <- jjsegmentedtotalbar(
    data = jjsegmentedtotalbar_quality,
    x_var = "hospital",
    y_var = "compliance_score",
    fill_var = "quality_grade",
    chart_style = "clean"
  )

  expect_s3_class(result, "jjsegmentedtotalbarResults")
})

test_that("jjsegmentedtotalbar handles publication chart style", {
  devtools::load_all()

  result <- jjsegmentedtotalbar(
    data = jjsegmentedtotalbar_quality,
    x_var = "hospital",
    y_var = "compliance_score",
    fill_var = "quality_grade",
    chart_style = "publication"
  )

  expect_s3_class(result, "jjsegmentedtotalbarResults")
})

test_that("jjsegmentedtotalbar handles presentation chart style", {
  devtools::load_all()

  result <- jjsegmentedtotalbar(
    data = jjsegmentedtotalbar_quality,
    x_var = "hospital",
    y_var = "compliance_score",
    fill_var = "quality_grade",
    chart_style = "presentation"
  )

  expect_s3_class(result, "jjsegmentedtotalbarResults")
})

test_that("jjsegmentedtotalbar handles clinical chart style", {
  devtools::load_all()

  result <- jjsegmentedtotalbar(
    data = jjsegmentedtotalbar_biomarker,
    x_var = "disease_stage",
    y_var = "expression_score",
    fill_var = "expression_level",
    chart_style = "clinical"
  )

  expect_s3_class(result, "jjsegmentedtotalbarResults")
})

test_that("jjsegmentedtotalbar handles bbc_style chart style", {
  devtools::load_all()

  result <- jjsegmentedtotalbar(
    data = jjsegmentedtotalbar_demographics,
    x_var = "treatment_center",
    y_var = "patient_count",
    fill_var = "gender",
    chart_style = "bbc_style"
  )

  expect_s3_class(result, "jjsegmentedtotalbarResults")
})

test_that("jjsegmentedtotalbar handles prism_style chart style", {
  devtools::load_all()

  result <- jjsegmentedtotalbar(
    data = jjsegmentedtotalbar_temporal,
    x_var = "time_period",
    y_var = "disease_burden_score",
    fill_var = "disease_status",
    chart_style = "prism_style"
  )

  expect_s3_class(result, "jjsegmentedtotalbarResults")
})

# ═══════════════════════════════════════════════════════════
# 4. Color Palettes
# ═══════════════════════════════════════════════════════════

test_that("jjsegmentedtotalbar handles default palette", {
  devtools::load_all()

  result <- jjsegmentedtotalbar(
    data = jjsegmentedtotalbar_test,
    x_var = "timepoint",
    y_var = "tumor_response_score",
    fill_var = "response_category",
    color_palette = "default"
  )

  expect_s3_class(result, "jjsegmentedtotalbarResults")
})

test_that("jjsegmentedtotalbar handles viridis palette", {
  devtools::load_all()

  result <- jjsegmentedtotalbar(
    data = jjsegmentedtotalbar_biomarker,
    x_var = "disease_stage",
    y_var = "expression_score",
    fill_var = "expression_level",
    color_palette = "viridis"
  )

  expect_s3_class(result, "jjsegmentedtotalbarResults")
})

test_that("jjsegmentedtotalbar handles Set1 palette", {
  devtools::load_all()

  result <- jjsegmentedtotalbar(
    data = jjsegmentedtotalbar_demographics,
    x_var = "treatment_center",
    y_var = "patient_count",
    fill_var = "age_group",
    color_palette = "set1"
  )

  expect_s3_class(result, "jjsegmentedtotalbarResults")
})

test_that("jjsegmentedtotalbar handles Dark2 palette", {
  devtools::load_all()

  result <- jjsegmentedtotalbar(
    data = jjsegmentedtotalbar_quality,
    x_var = "quarter",
    y_var = "compliance_score",
    fill_var = "quality_grade",
    color_palette = "dark2"
  )

  expect_s3_class(result, "jjsegmentedtotalbarResults")
})

test_that("jjsegmentedtotalbar handles Paired palette", {
  devtools::load_all()

  result <- jjsegmentedtotalbar(
    data = jjsegmentedtotalbar_temporal,
    x_var = "time_period",
    y_var = "disease_burden_score",
    fill_var = "disease_status",
    color_palette = "paired"
  )

  expect_s3_class(result, "jjsegmentedtotalbarResults")
})

test_that("jjsegmentedtotalbar handles clinical palette", {
  devtools::load_all()

  result <- jjsegmentedtotalbar(
    data = jjsegmentedtotalbar_test,
    x_var = "treatment",
    y_var = "tumor_response_score",
    fill_var = "response_category",
    color_palette = "clinical"
  )

  expect_s3_class(result, "jjsegmentedtotalbarResults")
})

test_that("jjsegmentedtotalbar handles colorblind palette", {
  devtools::load_all()

  result <- jjsegmentedtotalbar(
    data = jjsegmentedtotalbar_biomarker,
    x_var = "tumor_type",
    y_var = "expression_score",
    fill_var = "expression_level",
    color_palette = "colorblind"
  )

  expect_s3_class(result, "jjsegmentedtotalbarResults")
})

test_that("jjsegmentedtotalbar handles bbc_multi palette", {
  devtools::load_all()

  result <- jjsegmentedtotalbar(
    data = jjsegmentedtotalbar_demographics,
    x_var = "treatment_center",
    y_var = "patient_count",
    fill_var = "ethnicity",
    color_palette = "bbc_multi"
  )

  expect_s3_class(result, "jjsegmentedtotalbarResults")
})

test_that("jjsegmentedtotalbar handles prism_colorblind_safe palette", {
  devtools::load_all()

  result <- jjsegmentedtotalbar(
    data = jjsegmentedtotalbar_quality,
    x_var = "hospital",
    y_var = "compliance_score",
    fill_var = "quality_grade",
    color_palette = "prism_colorblind_safe"
  )

  expect_s3_class(result, "jjsegmentedtotalbarResults")
})

test_that("jjsegmentedtotalbar handles nature palette", {
  devtools::load_all()

  result <- jjsegmentedtotalbar(
    data = jjsegmentedtotalbar_temporal,
    x_var = "intervention",
    y_var = "disease_burden_score",
    fill_var = "disease_status",
    color_palette = "nature"
  )

  expect_s3_class(result, "jjsegmentedtotalbarResults")
})

test_that("jjsegmentedtotalbar handles science palette", {
  devtools::load_all()

  result <- jjsegmentedtotalbar(
    data = jjsegmentedtotalbar_biomarker,
    x_var = "disease_stage",
    y_var = "expression_score",
    fill_var = "biomarker",
    color_palette = "science"
  )

  expect_s3_class(result, "jjsegmentedtotalbarResults")
})

# ═══════════════════════════════════════════════════════════
# 5. Label Options
# ═══════════════════════════════════════════════════════════

test_that("jjsegmentedtotalbar handles show_percentages", {
  devtools::load_all()

  result <- jjsegmentedtotalbar(
    data = jjsegmentedtotalbar_test,
    x_var = "timepoint",
    y_var = "tumor_response_score",
    fill_var = "response_category",
    show_percentages = TRUE
  )

  expect_s3_class(result, "jjsegmentedtotalbarResults")
})

test_that("jjsegmentedtotalbar handles show_counts", {
  devtools::load_all()

  result <- jjsegmentedtotalbar(
    data = jjsegmentedtotalbar_demographics,
    x_var = "treatment_center",
    y_var = "patient_count",
    fill_var = "age_group",
    show_counts = TRUE
  )

  expect_s3_class(result, "jjsegmentedtotalbarResults")
})

test_that("jjsegmentedtotalbar handles both percentages and counts", {
  devtools::load_all()

  result <- jjsegmentedtotalbar(
    data = jjsegmentedtotalbar_quality,
    x_var = "hospital",
    y_var = "compliance_score",
    fill_var = "quality_grade",
    show_percentages = TRUE,
    show_counts = TRUE
  )

  expect_s3_class(result, "jjsegmentedtotalbarResults")
})

# ═══════════════════════════════════════════════════════════
# 6. Percentage Format Options
# ═══════════════════════════════════════════════════════════

test_that("jjsegmentedtotalbar handles integer percentage format", {
  devtools::load_all()

  result <- jjsegmentedtotalbar(
    data = jjsegmentedtotalbar_test,
    x_var = "timepoint",
    y_var = "tumor_response_score",
    fill_var = "response_category",
    show_percentages = TRUE,
    percentage_format = "integer"
  )

  expect_s3_class(result, "jjsegmentedtotalbarResults")
})

test_that("jjsegmentedtotalbar handles decimal1 percentage format", {
  devtools::load_all()

  result <- jjsegmentedtotalbar(
    data = jjsegmentedtotalbar_biomarker,
    x_var = "disease_stage",
    y_var = "expression_score",
    fill_var = "expression_level",
    show_percentages = TRUE,
    percentage_format = "decimal1"
  )

  expect_s3_class(result, "jjsegmentedtotalbarResults")
})

test_that("jjsegmentedtotalbar handles decimal2 percentage format", {
  devtools::load_all()

  result <- jjsegmentedtotalbar(
    data = jjsegmentedtotalbar_temporal,
    x_var = "time_period",
    y_var = "disease_burden_score",
    fill_var = "disease_status",
    show_percentages = TRUE,
    percentage_format = "decimal2"
  )

  expect_s3_class(result, "jjsegmentedtotalbarResults")
})

# ═══════════════════════════════════════════════════════════
# 7. Orientation Options
# ═══════════════════════════════════════════════════════════

test_that("jjsegmentedtotalbar handles vertical orientation", {
  devtools::load_all()

  result <- jjsegmentedtotalbar(
    data = jjsegmentedtotalbar_demographics,
    x_var = "treatment_center",
    y_var = "patient_count",
    fill_var = "gender",
    orientation = "vertical"
  )

  expect_s3_class(result, "jjsegmentedtotalbarResults")
})

test_that("jjsegmentedtotalbar handles horizontal orientation", {
  devtools::load_all()

  result <- jjsegmentedtotalbar(
    data = jjsegmentedtotalbar_quality,
    x_var = "hospital",
    y_var = "compliance_score",
    fill_var = "quality_grade",
    orientation = "horizontal"
  )

  expect_s3_class(result, "jjsegmentedtotalbarResults")
})

# ═══════════════════════════════════════════════════════════
# 8. Sort Options
# ═══════════════════════════════════════════════════════════

test_that("jjsegmentedtotalbar handles no sorting", {
  devtools::load_all()

  result <- jjsegmentedtotalbar(
    data = jjsegmentedtotalbar_test,
    x_var = "timepoint",
    y_var = "tumor_response_score",
    fill_var = "response_category",
    sort_categories = "none"
  )

  expect_s3_class(result, "jjsegmentedtotalbarResults")
})

test_that("jjsegmentedtotalbar handles sort by total", {
  devtools::load_all()

  result <- jjsegmentedtotalbar(
    data = jjsegmentedtotalbar_demographics,
    x_var = "treatment_center",
    y_var = "patient_count",
    fill_var = "age_group",
    sort_categories = "total"
  )

  expect_s3_class(result, "jjsegmentedtotalbarResults")
})

test_that("jjsegmentedtotalbar handles sort by largest segment", {
  devtools::load_all()

  result <- jjsegmentedtotalbar(
    data = jjsegmentedtotalbar_biomarker,
    x_var = "tumor_type",
    y_var = "expression_score",
    fill_var = "expression_level",
    sort_categories = "largest_segment"
  )

  expect_s3_class(result, "jjsegmentedtotalbarResults")
})

test_that("jjsegmentedtotalbar handles alphabetical sorting", {
  devtools::load_all()

  result <- jjsegmentedtotalbar(
    data = jjsegmentedtotalbar_temporal,
    x_var = "intervention",
    y_var = "disease_burden_score",
    fill_var = "disease_status",
    sort_categories = "alpha"
  )

  expect_s3_class(result, "jjsegmentedtotalbarResults")
})

# ═══════════════════════════════════════════════════════════
# 9. Faceting
# ═══════════════════════════════════════════════════════════

test_that("jjsegmentedtotalbar handles faceting by 2-level variable", {
  devtools::load_all()

  result <- jjsegmentedtotalbar(
    data = jjsegmentedtotalbar_demographics,
    x_var = "treatment_center",
    y_var = "patient_count",
    fill_var = "age_group",
    facet_var = "gender"
  )

  expect_s3_class(result, "jjsegmentedtotalbarResults")
})

test_that("jjsegmentedtotalbar handles faceting by 3-level variable", {
  devtools::load_all()

  result <- jjsegmentedtotalbar(
    data = jjsegmentedtotalbar_test,
    x_var = "timepoint",
    y_var = "tumor_response_score",
    fill_var = "response_category",
    facet_var = "treatment"
  )

  expect_s3_class(result, "jjsegmentedtotalbarResults")
})

test_that("jjsegmentedtotalbar handles faceting by 4-level variable", {
  devtools::load_all()

  result <- jjsegmentedtotalbar(
    data = jjsegmentedtotalbar_test,
    x_var = "treatment",
    y_var = "tumor_response_score",
    fill_var = "response_category",
    facet_var = "disease_stage"
  )

  expect_s3_class(result, "jjsegmentedtotalbarResults")
})

# ═══════════════════════════════════════════════════════════
# 10. Statistical Tests
# ═══════════════════════════════════════════════════════════

test_that("jjsegmentedtotalbar handles chi-square statistical test", {
  devtools::load_all()

  result <- jjsegmentedtotalbar(
    data = jjsegmentedtotalbar_test,
    x_var = "timepoint",
    y_var = "tumor_response_score",
    fill_var = "response_category",
    show_statistical_tests = TRUE
  )

  expect_s3_class(result, "jjsegmentedtotalbarResults")
})

# ═══════════════════════════════════════════════════════════
# 11. Flerlage-Specific Options
# ═══════════════════════════════════════════════════════════

test_that("jjsegmentedtotalbar handles flerlage with labels", {
  devtools::load_all()

  result <- jjsegmentedtotalbar(
    data = jjsegmentedtotalbar_quality,
    x_var = "hospital",
    y_var = "compliance_score",
    fill_var = "quality_grade",
    plot_type = "flerlage",
    flerlage_show_labels = TRUE
  )

  expect_s3_class(result, "jjsegmentedtotalbarResults")
})

test_that("jjsegmentedtotalbar handles flerlage label customization", {
  devtools::load_all()

  result <- jjsegmentedtotalbar(
    data = jjsegmentedtotalbar_temporal,
    x_var = "time_period",
    y_var = "disease_burden_score",
    fill_var = "disease_status",
    plot_type = "flerlage",
    flerlage_show_labels = TRUE,
    flerlage_label_size = 8,
    flerlage_label_color = "white"
  )

  expect_s3_class(result, "jjsegmentedtotalbarResults")
})

test_that("jjsegmentedtotalbar handles flerlage alpha transparency", {
  devtools::load_all()

  result <- jjsegmentedtotalbar(
    data = jjsegmentedtotalbar_biomarker,
    x_var = "disease_stage",
    y_var = "expression_score",
    fill_var = "expression_level",
    plot_type = "flerlage",
    flerlage_alpha = 0.7
  )

  expect_s3_class(result, "jjsegmentedtotalbarResults")
})

test_that("jjsegmentedtotalbar handles flerlage box color", {
  devtools::load_all()

  result <- jjsegmentedtotalbar(
    data = jjsegmentedtotalbar_test,
    x_var = "timepoint",
    y_var = "tumor_response_score",
    fill_var = "response_category",
    plot_type = "flerlage",
    flerlage_box_color = "gray30"
  )

  expect_s3_class(result, "jjsegmentedtotalbarResults")
})
