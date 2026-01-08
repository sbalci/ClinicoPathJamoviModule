# ═══════════════════════════════════════════════════════════
# Argument Combination Tests: jjsegmentedtotalbar
# ═══════════════════════════════════════════════════════════
#
# Tests all parameter combinations and interactions
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
# 1. Plot Type + Chart Style Combinations
# ═══════════════════════════════════════════════════════════

test_that("jjsegmentedtotalbar handles stacked + publication style", {
  devtools::load_all()

  result <- jjsegmentedtotalbar(
    data = jjsegmentedtotalbar_test,
    x_var = "timepoint",
    y_var = "tumor_response_score",
    fill_var = "response_category",
    plot_type = "stacked",
    chart_style = "publication"
  )

  expect_s3_class(result, "jjsegmentedtotalbarResults")
})

test_that("jjsegmentedtotalbar handles flerlage + clinical style", {
  devtools::load_all()

  result <- jjsegmentedtotalbar(
    data = jjsegmentedtotalbar_biomarker,
    x_var = "disease_stage",
    y_var = "expression_score",
    fill_var = "expression_level",
    plot_type = "flerlage",
    chart_style = "clinical"
  )

  expect_s3_class(result, "jjsegmentedtotalbarResults")
})

test_that("jjsegmentedtotalbar handles stacked + prism_style", {
  devtools::load_all()

  result <- jjsegmentedtotalbar(
    data = jjsegmentedtotalbar_temporal,
    x_var = "time_period",
    y_var = "disease_burden_score",
    fill_var = "disease_status",
    plot_type = "stacked",
    chart_style = "prism_style"
  )

  expect_s3_class(result, "jjsegmentedtotalbarResults")
})

# ═══════════════════════════════════════════════════════════
# 2. Chart Style + Color Palette Combinations
# ═══════════════════════════════════════════════════════════

test_that("jjsegmentedtotalbar handles publication style + viridis palette", {
  devtools::load_all()

  result <- jjsegmentedtotalbar(
    data = jjsegmentedtotalbar_quality,
    x_var = "hospital",
    y_var = "compliance_score",
    fill_var = "quality_grade",
    chart_style = "publication",
    color_palette = "viridis"
  )

  expect_s3_class(result, "jjsegmentedtotalbarResults")
})

test_that("jjsegmentedtotalbar handles clinical style + clinical palette", {
  devtools::load_all()

  result <- jjsegmentedtotalbar(
    data = jjsegmentedtotalbar_test,
    x_var = "treatment",
    y_var = "tumor_response_score",
    fill_var = "response_category",
    chart_style = "clinical",
    color_palette = "clinical"
  )

  expect_s3_class(result, "jjsegmentedtotalbarResults")
})

test_that("jjsegmentedtotalbar handles bbc_style + bbc_multi palette", {
  devtools::load_all()

  result <- jjsegmentedtotalbar(
    data = jjsegmentedtotalbar_demographics,
    x_var = "treatment_center",
    y_var = "patient_count",
    fill_var = "ethnicity",
    chart_style = "bbc_style",
    color_palette = "bbc_multi"
  )

  expect_s3_class(result, "jjsegmentedtotalbarResults")
})

test_that("jjsegmentedtotalbar handles prism_style + prism_colorblind_safe palette", {
  devtools::load_all()

  result <- jjsegmentedtotalbar(
    data = jjsegmentedtotalbar_biomarker,
    x_var = "tumor_type",
    y_var = "expression_score",
    fill_var = "expression_level",
    chart_style = "prism_style",
    color_palette = "prism_colorblind_safe"
  )

  expect_s3_class(result, "jjsegmentedtotalbarResults")
})

# ═══════════════════════════════════════════════════════════
# 3. Labels + Percentages + Counts Combinations
# ═══════════════════════════════════════════════════════════

test_that("jjsegmentedtotalbar handles percentages with integer format", {
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

test_that("jjsegmentedtotalbar handles percentages + counts with decimal1 format", {
  devtools::load_all()

  result <- jjsegmentedtotalbar(
    data = jjsegmentedtotalbar_demographics,
    x_var = "treatment_center",
    y_var = "patient_count",
    fill_var = "age_group",
    show_percentages = TRUE,
    show_counts = TRUE,
    percentage_format = "decimal1"
  )

  expect_s3_class(result, "jjsegmentedtotalbarResults")
})

test_that("jjsegmentedtotalbar handles percentages with decimal2 format and threshold", {
  devtools::load_all()

  result <- jjsegmentedtotalbar(
    data = jjsegmentedtotalbar_quality,
    x_var = "quarter",
    y_var = "compliance_score",
    fill_var = "quality_grade",
    show_percentages = TRUE,
    percentage_format = "decimal2",
    label_threshold = 10
  )

  expect_s3_class(result, "jjsegmentedtotalbarResults")
})

# ═══════════════════════════════════════════════════════════
# 4. Orientation + Sorting Combinations
# ═══════════════════════════════════════════════════════════

test_that("jjsegmentedtotalbar handles horizontal + sort by total", {
  devtools::load_all()

  result <- jjsegmentedtotalbar(
    data = jjsegmentedtotalbar_biomarker,
    x_var = "disease_stage",
    y_var = "expression_score",
    fill_var = "expression_level",
    orientation = "horizontal",
    sort_categories = "total"
  )

  expect_s3_class(result, "jjsegmentedtotalbarResults")
})

test_that("jjsegmentedtotalbar handles vertical + sort by largest segment", {
  devtools::load_all()

  result <- jjsegmentedtotalbar(
    data = jjsegmentedtotalbar_temporal,
    x_var = "intervention",
    y_var = "disease_burden_score",
    fill_var = "disease_status",
    orientation = "vertical",
    sort_categories = "largest_segment"
  )

  expect_s3_class(result, "jjsegmentedtotalbarResults")
})

test_that("jjsegmentedtotalbar handles horizontal + alphabetical sort", {
  devtools::load_all()

  result <- jjsegmentedtotalbar(
    data = jjsegmentedtotalbar_demographics,
    x_var = "treatment_center",
    y_var = "patient_count",
    fill_var = "gender",
    orientation = "horizontal",
    sort_categories = "alpha"
  )

  expect_s3_class(result, "jjsegmentedtotalbarResults")
})

# ═══════════════════════════════════════════════════════════
# 5. Faceting + Chart Style Combinations
# ═══════════════════════════════════════════════════════════

test_that("jjsegmentedtotalbar handles faceting + publication style", {
  devtools::load_all()

  result <- jjsegmentedtotalbar(
    data = jjsegmentedtotalbar_test,
    x_var = "timepoint",
    y_var = "tumor_response_score",
    fill_var = "response_category",
    facet_var = "treatment",
    chart_style = "publication"
  )

  expect_s3_class(result, "jjsegmentedtotalbarResults")
})

test_that("jjsegmentedtotalbar handles faceting + clinical style", {
  devtools::load_all()

  result <- jjsegmentedtotalbar(
    data = jjsegmentedtotalbar_biomarker,
    x_var = "disease_stage",
    y_var = "expression_score",
    fill_var = "expression_level",
    facet_var = "tumor_type",
    chart_style = "clinical"
  )

  expect_s3_class(result, "jjsegmentedtotalbarResults")
})

# ═══════════════════════════════════════════════════════════
# 6. Faceting + Labels Combinations
# ═══════════════════════════════════════════════════════════

test_that("jjsegmentedtotalbar handles faceting + percentages", {
  devtools::load_all()

  result <- jjsegmentedtotalbar(
    data = jjsegmentedtotalbar_demographics,
    x_var = "treatment_center",
    y_var = "patient_count",
    fill_var = "age_group",
    facet_var = "gender",
    show_percentages = TRUE
  )

  expect_s3_class(result, "jjsegmentedtotalbarResults")
})

test_that("jjsegmentedtotalbar handles faceting + counts", {
  devtools::load_all()

  result <- jjsegmentedtotalbar(
    data = jjsegmentedtotalbar_quality,
    x_var = "hospital",
    y_var = "compliance_score",
    fill_var = "quality_grade",
    facet_var = "quarter",
    show_counts = TRUE
  )

  expect_s3_class(result, "jjsegmentedtotalbarResults")
})

# ═══════════════════════════════════════════════════════════
# 7. Flerlage Complete Customization
# ═══════════════════════════════════════════════════════════

test_that("jjsegmentedtotalbar handles flerlage with all options", {
  devtools::load_all()

  result <- jjsegmentedtotalbar(
    data = jjsegmentedtotalbar_temporal,
    x_var = "time_period",
    y_var = "disease_burden_score",
    fill_var = "disease_status",
    plot_type = "flerlage",
    flerlage_show_labels = TRUE,
    flerlage_label_size = 7,
    flerlage_label_color = "white",
    flerlage_alpha = 0.8,
    flerlage_box_color = "gray20"
  )

  expect_s3_class(result, "jjsegmentedtotalbarResults")
})

# ═══════════════════════════════════════════════════════════
# 8. Statistical Tests + Other Options
# ═══════════════════════════════════════════════════════════

test_that("jjsegmentedtotalbar handles statistics + percentages", {
  devtools::load_all()

  result <- jjsegmentedtotalbar(
    data = jjsegmentedtotalbar_test,
    x_var = "treatment",
    y_var = "tumor_response_score",
    fill_var = "response_category",
    show_statistical_tests = TRUE,
    show_percentages = TRUE
  )

  expect_s3_class(result, "jjsegmentedtotalbarResults")
})

test_that("jjsegmentedtotalbar handles statistics + faceting", {
  devtools::load_all()

  result <- jjsegmentedtotalbar(
    data = jjsegmentedtotalbar_biomarker,
    x_var = "disease_stage",
    y_var = "expression_score",
    fill_var = "expression_level",
    facet_var = "biomarker",
    show_statistical_tests = TRUE
  )

  expect_s3_class(result, "jjsegmentedtotalbarResults")
})

# ═══════════════════════════════════════════════════════════
# 9. Complete Treatment Response Workflow
# ═══════════════════════════════════════════════════════════

test_that("jjsegmentedtotalbar handles complete treatment response analysis", {
  devtools::load_all()

  result <- jjsegmentedtotalbar(
    data = jjsegmentedtotalbar_test,
    x_var = "timepoint",
    y_var = "tumor_response_score",
    fill_var = "response_category",
    facet_var = "treatment",
    chart_style = "publication",
    color_palette = "clinical",
    show_percentages = TRUE,
    percentage_format = "decimal1",
    show_counts = TRUE,
    label_threshold = 5,
    orientation = "vertical",
    sort_categories = "none",
    show_statistical_tests = TRUE
  )

  expect_s3_class(result, "jjsegmentedtotalbarResults")
})

# ═══════════════════════════════════════════════════════════
# 10. Complete Demographics Workflow
# ═══════════════════════════════════════════════════════════

test_that("jjsegmentedtotalbar handles complete demographics analysis", {
  devtools::load_all()

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

# ═══════════════════════════════════════════════════════════
# 11. Complete Biomarker Workflow
# ═══════════════════════════════════════════════════════════

test_that("jjsegmentedtotalbar handles complete biomarker analysis", {
  devtools::load_all()

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
    orientation = "horizontal",
    sort_categories = "largest_segment",
    show_statistical_tests = TRUE
  )

  expect_s3_class(result, "jjsegmentedtotalbarResults")
})

# ═══════════════════════════════════════════════════════════
# 12. Complete Quality Metrics Workflow
# ═══════════════════════════════════════════════════════════

test_that("jjsegmentedtotalbar handles complete quality metrics analysis", {
  devtools::load_all()

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
    flerlage_alpha = 0.85,
    orientation = "horizontal",
    sort_categories = "total"
  )

  expect_s3_class(result, "jjsegmentedtotalbarResults")
})

# ═══════════════════════════════════════════════════════════
# 13. Complete Temporal Progression Workflow
# ═══════════════════════════════════════════════════════════

test_that("jjsegmentedtotalbar handles complete temporal progression analysis", {
  devtools::load_all()

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
    show_counts = FALSE,
    label_threshold = 5,
    orientation = "vertical",
    sort_categories = "none",
    show_statistical_tests = TRUE
  )

  expect_s3_class(result, "jjsegmentedtotalbarResults")
})

# ═══════════════════════════════════════════════════════════
# 14. Minimal vs Maximum Configuration
# ═══════════════════════════════════════════════════════════

test_that("jjsegmentedtotalbar handles minimal configuration", {
  devtools::load_all()

  result <- jjsegmentedtotalbar(
    data = jjsegmentedtotalbar_test,
    x_var = "timepoint",
    y_var = "tumor_response_score",
    fill_var = "response_category"
  )

  expect_s3_class(result, "jjsegmentedtotalbarResults")
})

test_that("jjsegmentedtotalbar handles maximum configuration", {
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
    show_counts = TRUE,
    label_threshold = 5,
    orientation = "vertical",
    sort_categories = "total",
    show_statistical_tests = TRUE
  )

  expect_s3_class(result, "jjsegmentedtotalbarResults")
})

# ═══════════════════════════════════════════════════════════
# 15. Progressive Feature Addition
# ═══════════════════════════════════════════════════════════

test_that("jjsegmentedtotalbar handles progressive feature addition", {
  devtools::load_all()

  # Step 1: Basic plot
  result1 <- jjsegmentedtotalbar(
    data = jjsegmentedtotalbar_test,
    x_var = "timepoint",
    y_var = "tumor_response_score",
    fill_var = "response_category"
  )
  expect_s3_class(result1, "jjsegmentedtotalbarResults")

  # Step 2: Add chart style
  result2 <- jjsegmentedtotalbar(
    data = jjsegmentedtotalbar_test,
    x_var = "timepoint",
    y_var = "tumor_response_score",
    fill_var = "response_category",
    chart_style = "publication"
  )
  expect_s3_class(result2, "jjsegmentedtotalbarResults")

  # Step 3: Add color palette
  result3 <- jjsegmentedtotalbar(
    data = jjsegmentedtotalbar_test,
    x_var = "timepoint",
    y_var = "tumor_response_score",
    fill_var = "response_category",
    chart_style = "publication",
    color_palette = "clinical"
  )
  expect_s3_class(result3, "jjsegmentedtotalbarResults")

  # Step 4: Add percentages
  result4 <- jjsegmentedtotalbar(
    data = jjsegmentedtotalbar_test,
    x_var = "timepoint",
    y_var = "tumor_response_score",
    fill_var = "response_category",
    chart_style = "publication",
    color_palette = "clinical",
    show_percentages = TRUE
  )
  expect_s3_class(result4, "jjsegmentedtotalbarResults")

  # Step 5: Add faceting
  result5 <- jjsegmentedtotalbar(
    data = jjsegmentedtotalbar_test,
    x_var = "timepoint",
    y_var = "tumor_response_score",
    fill_var = "response_category",
    facet_var = "treatment",
    chart_style = "publication",
    color_palette = "clinical",
    show_percentages = TRUE
  )
  expect_s3_class(result5, "jjsegmentedtotalbarResults")

  # Step 6: Add statistical tests
  result6 <- jjsegmentedtotalbar(
    data = jjsegmentedtotalbar_test,
    x_var = "timepoint",
    y_var = "tumor_response_score",
    fill_var = "response_category",
    facet_var = "treatment",
    chart_style = "publication",
    color_palette = "clinical",
    show_percentages = TRUE,
    show_statistical_tests = TRUE
  )
  expect_s3_class(result6, "jjsegmentedtotalbarResults")
})

# ═══════════════════════════════════════════════════════════
# 16. Different Chart Styles with Same Data
# ═══════════════════════════════════════════════════════════

test_that("jjsegmentedtotalbar produces consistent results across chart styles", {
  devtools::load_all()

  chart_styles <- c("clean", "publication", "presentation", "clinical", "bbc_style", "prism_style")
  results <- list()

  for (style in chart_styles) {
    results[[style]] <- jjsegmentedtotalbar(
      data = jjsegmentedtotalbar_demographics,
      x_var = "treatment_center",
      y_var = "patient_count",
      fill_var = "gender",
      chart_style = style
    )

    expect_s3_class(results[[style]], "jjsegmentedtotalbarResults")
  }

  expect_equal(length(results), length(chart_styles))
})
