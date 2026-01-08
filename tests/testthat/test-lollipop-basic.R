# ═══════════════════════════════════════════════════════════
# Basic Functionality Tests: lollipop
# ═══════════════════════════════════════════════════════════
#
# Tests core functionality of the lollipop function
# Generated: 2026-01-05

library(testthat)
library(ClinicoPath)

# Load test data
data(lollipop_test, package = "ClinicoPath", envir = environment())
data(lollipop_treatment, package = "ClinicoPath", envir = environment())
data(lollipop_small, package = "ClinicoPath", envir = environment())

# ═══════════════════════════════════════════════════════════
# 1. Function Existence and Basic Execution
# ═══════════════════════════════════════════════════════════

test_that("lollipop function exists and runs with minimal arguments", {
  devtools::load_all()

  result <- lollipop(
    data = lollipop_test,
    dep = "hemoglobin",
    group = "treatment_group"
  )

  expect_s3_class(result, "lollipopResults")
})

test_that("lollipop runs with basic clinical lab data", {
  devtools::load_all()

  result <- lollipop(
    data = lollipop_test,
    dep = "albumin",
    group = "disease_severity"
  )

  expect_s3_class(result, "lollipopResults")
})

test_that("lollipop handles treatment comparison data", {
  devtools::load_all()

  result <- lollipop(
    data = lollipop_treatment,
    dep = "tumor_reduction",
    group = "treatment"
  )

  expect_s3_class(result, "lollipopResults")
})

# ═══════════════════════════════════════════════════════════
# 2. Aggregation Options
# ═══════════════════════════════════════════════════════════

test_that("lollipop handles different aggregation methods", {
  devtools::load_all()

  # No aggregation
  result_none <- lollipop(
    data = lollipop_test,
    dep = "hemoglobin",
    group = "treatment_group",
    aggregation = "none"
  )
  expect_s3_class(result_none, "lollipopResults")

  # Mean aggregation
  result_mean <- lollipop(
    data = lollipop_test,
    dep = "hemoglobin",
    group = "treatment_group",
    aggregation = "mean"
  )
  expect_s3_class(result_mean, "lollipopResults")

  # Median aggregation
  result_median <- lollipop(
    data = lollipop_test,
    dep = "hemoglobin",
    group = "treatment_group",
    aggregation = "median"
  )
  expect_s3_class(result_median, "lollipopResults")

  # Sum aggregation
  result_sum <- lollipop(
    data = lollipop_test,
    dep = "platelet_count",
    group = "hospital",
    aggregation = "sum"
  )
  expect_s3_class(result_sum, "lollipopResults")
})

# ═══════════════════════════════════════════════════════════
# 3. Sorting Options
# ═══════════════════════════════════════════════════════════

test_that("lollipop handles different sort orders", {
  devtools::load_all()

  # Original order
  result_orig <- lollipop(
    data = lollipop_small,
    dep = "measurement",
    group = "category",
    sortBy = "original"
  )
  expect_s3_class(result_orig, "lollipopResults")

  # Sort by value ascending
  result_asc <- lollipop(
    data = lollipop_small,
    dep = "measurement",
    group = "category",
    sortBy = "value_asc"
  )
  expect_s3_class(result_asc, "lollipopResults")

  # Sort by value descending
  result_desc <- lollipop(
    data = lollipop_small,
    dep = "measurement",
    group = "category",
    sortBy = "value_desc"
  )
  expect_s3_class(result_desc, "lollipopResults")

  # Sort by group alphabetically
  result_alpha <- lollipop(
    data = lollipop_small,
    dep = "measurement",
    group = "category",
    sortBy = "group_alpha"
  )
  expect_s3_class(result_alpha, "lollipopResults")
})

# ═══════════════════════════════════════════════════════════
# 4. Orientation Options
# ═══════════════════════════════════════════════════════════

test_that("lollipop handles vertical and horizontal orientations", {
  devtools::load_all()

  # Vertical orientation
  result_vert <- lollipop(
    data = lollipop_test,
    dep = "creatinine",
    group = "age_group",
    orientation = "vertical"
  )
  expect_s3_class(result_vert, "lollipopResults")

  # Horizontal orientation
  result_horiz <- lollipop(
    data = lollipop_test,
    dep = "creatinine",
    group = "age_group",
    orientation = "horizontal"
  )
  expect_s3_class(result_horiz, "lollipopResults")
})

# ═══════════════════════════════════════════════════════════
# 5. Visual Options
# ═══════════════════════════════════════════════════════════

test_that("lollipop handles showValues option", {
  devtools::load_all()

  result_values <- lollipop(
    data = lollipop_test,
    dep = "white_blood_cells",
    group = "hospital",
    showValues = TRUE
  )
  expect_s3_class(result_values, "lollipopResults")
})

test_that("lollipop handles showMean option", {
  devtools::load_all()

  result_mean <- lollipop(
    data = lollipop_test,
    dep = "platelet_count",
    group = "disease_severity",
    showMean = TRUE
  )
  expect_s3_class(result_mean, "lollipopResults")
})

# ═══════════════════════════════════════════════════════════
# 6. Highlighting Feature
# ═══════════════════════════════════════════════════════════

test_that("lollipop handles highlighting", {
  devtools::load_all()

  result <- lollipop(
    data = lollipop_test,
    dep = "albumin",
    group = "disease_severity",
    useHighlight = TRUE,
    highlight = "Severe"
  )
  expect_s3_class(result, "lollipopResults")
})

# ═══════════════════════════════════════════════════════════
# 7. Color Schemes
# ═══════════════════════════════════════════════════════════

test_that("lollipop handles different color schemes", {
  devtools::load_all()

  # Default
  result_default <- lollipop(
    data = lollipop_test,
    dep = "hemoglobin",
    group = "treatment_group",
    colorScheme = "default"
  )
  expect_s3_class(result_default, "lollipopResults")

  # Clinical
  result_clinical <- lollipop(
    data = lollipop_test,
    dep = "hemoglobin",
    group = "treatment_group",
    colorScheme = "clinical"
  )
  expect_s3_class(result_clinical, "lollipopResults")

  # Viridis
  result_viridis <- lollipop(
    data = lollipop_test,
    dep = "hemoglobin",
    group = "treatment_group",
    colorScheme = "viridis"
  )
  expect_s3_class(result_viridis, "lollipopResults")

  # Colorblind
  result_colorblind <- lollipop(
    data = lollipop_test,
    dep = "hemoglobin",
    group = "treatment_group",
    colorScheme = "colorblind"
  )
  expect_s3_class(result_colorblind, "lollipopResults")
})

# ═══════════════════════════════════════════════════════════
# 8. Plot Themes
# ═══════════════════════════════════════════════════════════

test_that("lollipop handles different plot themes", {
  devtools::load_all()

  # Default
  result_default <- lollipop(
    data = lollipop_test,
    dep = "albumin",
    group = "disease_severity",
    theme = "default"
  )
  expect_s3_class(result_default, "lollipopResults")

  # Minimal
  result_minimal <- lollipop(
    data = lollipop_test,
    dep = "albumin",
    group = "disease_severity",
    theme = "minimal"
  )
  expect_s3_class(result_minimal, "lollipopResults")

  # Classic
  result_classic <- lollipop(
    data = lollipop_test,
    dep = "albumin",
    group = "disease_severity",
    theme = "classic"
  )
  expect_s3_class(result_classic, "lollipopResults")

  # Publication
  result_pub <- lollipop(
    data = lollipop_test,
    dep = "albumin",
    group = "disease_severity",
    theme = "publication"
  )
  expect_s3_class(result_pub, "lollipopResults")
})

# ═══════════════════════════════════════════════════════════
# 9. Line Types
# ═══════════════════════════════════════════════════════════

test_that("lollipop handles different line types", {
  devtools::load_all()

  line_types <- c("solid", "dashed", "dotted", "dotdash")

  for (lt in line_types) {
    result <- lollipop(
      data = lollipop_small,
      dep = "measurement",
      group = "category",
      lineType = lt
    )
    expect_s3_class(result, "lollipopResults")
  }
})

# ═══════════════════════════════════════════════════════════
# 10. Custom Labels and Title
# ═══════════════════════════════════════════════════════════

test_that("lollipop handles custom labels and titles", {
  devtools::load_all()

  result <- lollipop(
    data = lollipop_test,
    dep = "hemoglobin",
    group = "treatment_group",
    xlabel = "Treatment Groups",
    ylabel = "Hemoglobin (g/dL)",
    title = "Hemoglobin Levels by Treatment"
  )
  expect_s3_class(result, "lollipopResults")
})

# ═══════════════════════════════════════════════════════════
# 11. All Datasets Compatibility
# ═══════════════════════════════════════════════════════════

test_that("lollipop works with all test datasets", {
  devtools::load_all()

  # lollipop_test
  result1 <- lollipop(
    data = lollipop_test,
    dep = "hemoglobin",
    group = "treatment_group"
  )
  expect_s3_class(result1, "lollipopResults")

  # lollipop_treatment
  result2 <- lollipop(
    data = lollipop_treatment,
    dep = "tumor_reduction",
    group = "treatment"
  )
  expect_s3_class(result2, "lollipopResults")

  # lollipop_small
  result3 <- lollipop(
    data = lollipop_small,
    dep = "measurement",
    group = "category"
  )
  expect_s3_class(result3, "lollipopResults")
})
