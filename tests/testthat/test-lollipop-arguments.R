# ═══════════════════════════════════════════════════════════
# Argument Combination Tests: lollipop
# ═══════════════════════════════════════════════════════════
#
# Tests all possible argument combinations for the lollipop function
# Generated: 2026-01-05

library(testthat)
library(ClinicoPath)

# Load test data
data(lollipop_test, package = "ClinicoPath", envir = environment())
data(lollipop_treatment, package = "ClinicoPath", envir = environment())
data(lollipop_biomarkers, package = "ClinicoPath", envir = environment())

# ═══════════════════════════════════════════════════════════
# 1. Conditional Coloring
# ═══════════════════════════════════════════════════════════

test_that("lollipop handles conditional coloring with threshold", {
  devtools::load_all()

  result <- lollipop(
    data = lollipop_test,
    dep = "creatinine",
    group = "age_group",
    conditionalColor = TRUE,
    colorThreshold = 1.2
  )
  expect_s3_class(result, "lollipopResults")
})

test_that("lollipop handles conditional coloring without threshold (uses 0)", {
  devtools::load_all()

  result <- lollipop(
    data = lollipop_treatment,
    dep = "tumor_reduction",
    group = "treatment",
    conditionalColor = TRUE
  )
  expect_s3_class(result, "lollipopResults")
})

# ═══════════════════════════════════════════════════════════
# 2. Baseline Value
# ═══════════════════════════════════════════════════════════

test_that("lollipop handles custom baseline values", {
  devtools::load_all()

  # Zero baseline (default)
  result_zero <- lollipop(
    data = lollipop_test,
    dep = "platelet_count",
    group = "hospital",
    baseline = 0
  )
  expect_s3_class(result_zero, "lollipopResults")

  # Clinical baseline (e.g., lower normal limit)
  result_clinical <- lollipop(
    data = lollipop_test,
    dep = "platelet_count",
    group = "hospital",
    baseline = 150
  )
  expect_s3_class(result_clinical, "lollipopResults")
})

# ═══════════════════════════════════════════════════════════
# 3. Point Size and Line Width Combinations
# ═══════════════════════════════════════════════════════════

test_that("lollipop handles different point sizes", {
  devtools::load_all()

  # Small points
  result_small <- lollipop(
    data = lollipop_test,
    dep = "hemoglobin",
    group = "treatment_group",
    pointSize = 1
  )
  expect_s3_class(result_small, "lollipopResults")

  # Large points
  result_large <- lollipop(
    data = lollipop_test,
    dep = "hemoglobin",
    group = "treatment_group",
    pointSize = 10
  )
  expect_s3_class(result_large, "lollipopResults")
})

test_that("lollipop handles different line widths", {
  devtools::load_all()

  # Thin lines
  result_thin <- lollipop(
    data = lollipop_test,
    dep = "albumin",
    group = "disease_severity",
    lineWidth = 0.5
  )
  expect_s3_class(result_thin, "lollipopResults")

  # Thick lines
  result_thick <- lollipop(
    data = lollipop_test,
    dep = "albumin",
    group = "disease_severity",
    lineWidth = 5
  )
  expect_s3_class(result_thick, "lollipopResults")
})

# ═══════════════════════════════════════════════════════════
# 4. Complete Feature Combinations
# ═══════════════════════════════════════════════════════════

test_that("lollipop handles all features together", {
  devtools::load_all()

  result <- lollipop(
    data = lollipop_test,
    dep = "white_blood_cells",
    group = "treatment_group",
    useHighlight = TRUE,
    highlight = "Drug A",
    aggregation = "mean",
    sortBy = "value_desc",
    orientation = "horizontal",
    showValues = TRUE,
    showMean = TRUE,
    colorScheme = "clinical",
    theme = "publication",
    pointSize = 4,
    lineWidth = 2,
    lineType = "dashed",
    baseline = 4,
    conditionalColor = TRUE,
    colorThreshold = 11,
    xlabel = "WBC Count",
    ylabel = "Treatment",
    title = "White Blood Cell Counts by Treatment"
  )
  expect_s3_class(result, "lollipopResults")
})

# ═══════════════════════════════════════════════════════════
# 5. Aggregation + Sorting Combinations
# ═══════════════════════════════════════════════════════════

test_that("lollipop handles aggregation with different sort orders", {
  devtools::load_all()

  # Mean + sort by value ascending
  result1 <- lollipop(
    data = lollipop_test,
    dep = "hemoglobin",
    group = "treatment_group",
    aggregation = "mean",
    sortBy = "value_asc"
  )
  expect_s3_class(result1, "lollipopResults")

  # Median + sort by value descending
  result2 <- lollipop(
    data = lollipop_test,
    dep = "albumin",
    group = "disease_severity",
    aggregation = "median",
    sortBy = "value_desc"
  )
  expect_s3_class(result2, "lollipopResults")

  # Sum + alphabetical sort
  result3 <- lollipop(
    data = lollipop_test,
    dep = "platelet_count",
    group = "hospital",
    aggregation = "sum",
    sortBy = "group_alpha"
  )
  expect_s3_class(result3, "lollipopResults")
})

# ═══════════════════════════════════════════════════════════
# 6. Highlight + Conditional Color Combinations
# ═══════════════════════════════════════════════════════════

test_that("lollipop handles highlight with conditional coloring", {
  devtools::load_all()

  result <- lollipop(
    data = lollipop_test,
    dep = "creatinine",
    group = "age_group",
    useHighlight = TRUE,
    highlight = "61-80",
    conditionalColor = TRUE,
    colorThreshold = 1.2
  )
  expect_s3_class(result, "lollipopResults")
})

# ═══════════════════════════════════════════════════════════
# 7. Orientation + Sorting Combinations
# ═══════════════════════════════════════════════════════════

test_that("lollipop handles horizontal orientation with sorting", {
  devtools::load_all()

  result <- lollipop(
    data = lollipop_test,
    dep = "alt",
    group = "treatment_group",
    orientation = "horizontal",
    sortBy = "value_desc",
    aggregation = "mean"
  )
  expect_s3_class(result, "lollipopResults")
})

# ═══════════════════════════════════════════════════════════
# 8. Theme + Color Scheme Combinations
# ═══════════════════════════════════════════════════════════

test_that("lollipop handles all theme and color scheme combinations", {
  devtools::load_all()

  themes <- c("default", "minimal", "classic", "publication")
  color_schemes <- c("default", "clinical", "viridis", "colorblind")

  for (theme_val in themes) {
    for (color_val in color_schemes) {
      result <- lollipop(
        data = lollipop_test,
        dep = "hemoglobin",
        group = "treatment_group",
        theme = theme_val,
        colorScheme = color_val,
        aggregation = "mean"
      )
      expect_s3_class(result, "lollipopResults")
    }
  }
})

# ═══════════════════════════════════════════════════════════
# 9. Visual Enhancements Combinations
# ═══════════════════════════════════════════════════════════

test_that("lollipop handles multiple visual enhancements", {
  devtools::load_all()

  result <- lollipop(
    data = lollipop_test,
    dep = "platelet_count",
    group = "disease_severity",
    showValues = TRUE,
    showMean = TRUE,
    lineType = "dotted",
    pointSize = 5,
    orientation = "horizontal"
  )
  expect_s3_class(result, "lollipopResults")
})

# ═══════════════════════════════════════════════════════════
# 10. Biomarker Panel with Highlighting
# ═══════════════════════════════════════════════════════════

test_that("lollipop handles biomarker data with highlighting", {
  devtools::load_all()

  result <- lollipop(
    data = lollipop_biomarkers,
    dep = "cea",
    group = "cancer_type",
    useHighlight = TRUE,
    highlight = "Type A",
    aggregation = "median",
    sortBy = "value_desc",
    colorScheme = "clinical",
    theme = "publication"
  )
  expect_s3_class(result, "lollipopResults")
})

# ═══════════════════════════════════════════════════════════
# 11. Treatment Response with All Features
# ═══════════════════════════════════════════════════════════

test_that("lollipop handles treatment response data comprehensively", {
  devtools::load_all()

  result <- lollipop(
    data = lollipop_treatment,
    dep = "qol_score",
    group = "treatment",
    sortBy = "value_desc",
    showValues = TRUE,
    showMean = TRUE,
    conditionalColor = TRUE,
    colorThreshold = 70,
    baseline = 60,
    orientation = "horizontal",
    colorScheme = "clinical",
    theme = "publication",
    title = "Quality of Life by Treatment",
    xlabel = "QoL Score (0-100)",
    ylabel = "Treatment Groups"
  )
  expect_s3_class(result, "lollipopResults")
})

# ═══════════════════════════════════════════════════════════
# 12. Minimal vs Maximum Configuration
# ═══════════════════════════════════════════════════════════

test_that("lollipop handles minimal configuration", {
  devtools::load_all()

  result_min <- lollipop(
    data = lollipop_test,
    dep = "hemoglobin",
    group = "treatment_group"
  )
  expect_s3_class(result_min, "lollipopResults")
})

test_that("lollipop handles maximum configuration", {
  devtools::load_all()

  result_max <- lollipop(
    data = lollipop_test,
    dep = "hemoglobin",
    group = "treatment_group",
    useHighlight = TRUE,
    highlight = "Drug A",
    aggregation = "mean",
    sortBy = "value_desc",
    orientation = "horizontal",
    showValues = TRUE,
    showMean = TRUE,
    colorScheme = "clinical",
    theme = "publication",
    pointSize = 5,
    lineWidth = 2.5,
    lineType = "dashed",
    baseline = 12,
    conditionalColor = TRUE,
    colorThreshold = 14,
    xlabel = "Hemoglobin Level",
    ylabel = "Treatment Group",
    title = "Hemoglobin Response by Treatment",
    width = 1000,
    height = 800
  )
  expect_s3_class(result_max, "lollipopResults")
})

# ═══════════════════════════════════════════════════════════
# 13. Different Lab Parameters
# ═══════════════════════════════════════════════════════════

test_that("lollipop handles different clinical lab parameters", {
  devtools::load_all()

  lab_params <- c("hemoglobin", "albumin", "creatinine",
                  "platelet_count", "white_blood_cells", "alt", "crp")

  for (param in lab_params) {
    result <- lollipop(
      data = lollipop_test,
      dep = param,
      group = "treatment_group",
      aggregation = "mean"
    )
    expect_s3_class(result, "lollipopResults")
  }
})
