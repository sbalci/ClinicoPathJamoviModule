# ═══════════════════════════════════════════════════════════
# Integration Tests: lollipop
# ═══════════════════════════════════════════════════════════
#
# Tests complete workflows and integration with all datasets
# Generated: 2026-01-05

library(testthat)
library(ClinicoPath)

# Load all test datasets
data(lollipop_test, package = "ClinicoPath", envir = environment())
data(lollipop_treatment, package = "ClinicoPath", envir = environment())
data(lollipop_biomarkers, package = "ClinicoPath", envir = environment())
data(lollipop_hospital, package = "ClinicoPath", envir = environment())
data(lollipop_small, package = "ClinicoPath", envir = environment())
data(lollipop_aggregated, package = "ClinicoPath", envir = environment())

# ═══════════════════════════════════════════════════════════
# 1. Clinical Lab Workflow - Complete Analysis
# ═══════════════════════════════════════════════════════════

test_that("lollipop handles complete clinical lab analysis workflow", {
  devtools::load_all()

  # Step 1: Basic visualization
  result1 <- lollipop(
    data = lollipop_test,
    dep = "hemoglobin",
    group = "treatment_group",
    aggregation = "mean"
  )
  expect_s3_class(result1, "lollipopResults")

  # Step 2: With highlighting
  result2 <- lollipop(
    data = lollipop_test,
    dep = "albumin",
    group = "disease_severity",
    useHighlight = TRUE,
    highlight = "Severe",
    aggregation = "median"
  )
  expect_s3_class(result2, "lollipopResults")

  # Step 3: With conditional coloring
  result3 <- lollipop(
    data = lollipop_test,
    dep = "creatinine",
    group = "age_group",
    conditionalColor = TRUE,
    colorThreshold = 1.2,
    aggregation = "mean"
  )
  expect_s3_class(result3, "lollipopResults")

  # Step 4: Publication-ready figure
  result4 <- lollipop(
    data = lollipop_test,
    dep = "platelet_count",
    group = "hospital",
    aggregation = "mean",
    sortBy = "value_desc",
    showValues = TRUE,
    baseline = 150,
    colorScheme = "clinical",
    theme = "publication",
    title = "Mean Platelet Counts by Hospital",
    ylabel = "Platelet Count (×10³/μL)"
  )
  expect_s3_class(result4, "lollipopResults")
})

# ═══════════════════════════════════════════════════════════
# 2. Treatment Comparison Workflow
# ═══════════════════════════════════════════════════════════

test_that("lollipop handles treatment comparison workflow", {
  devtools::load_all()

  # Primary outcome
  result1 <- lollipop(
    data = lollipop_treatment,
    dep = "tumor_reduction",
    group = "treatment",
    sortBy = "value_desc",
    showValues = TRUE,
    showMean = TRUE,
    conditionalColor = TRUE,
    colorThreshold = 30,
    baseline = 0,
    title = "Tumor Size Reduction by Treatment",
    ylabel = "Reduction (%)"
  )
  expect_s3_class(result1, "lollipopResults")

  # Secondary outcome
  result2 <- lollipop(
    data = lollipop_treatment,
    dep = "qol_score",
    group = "treatment",
    sortBy = "value_desc",
    orientation = "horizontal",
    showValues = TRUE,
    colorScheme = "viridis",
    theme = "publication"
  )
  expect_s3_class(result2, "lollipopResults")

  # Safety endpoint
  result3 <- lollipop(
    data = lollipop_treatment,
    dep = "side_effects",
    group = "treatment",
    sortBy = "value_asc",  # Lower is better
    conditionalColor = TRUE,
    colorThreshold = 20,
    colorScheme = "clinical",
    title = "Side Effect Scores by Treatment (Lower is Better)"
  )
  expect_s3_class(result3, "lollipopResults")
})

# ═══════════════════════════════════════════════════════════
# 3. Biomarker Panel Workflow
# ═══════════════════════════════════════════════════════════

test_that("lollipop handles biomarker panel workflow", {
  devtools::load_all()

  biomarkers <- c("cea", "ca125", "ca199", "afp", "psa")

  for (marker in biomarkers) {
    result <- lollipop(
      data = lollipop_biomarkers,
      dep = marker,
      group = "cancer_type",
      aggregation = "median",
      sortBy = "value_desc",
      colorScheme = "clinical",
      theme = "publication"
    )
    expect_s3_class(result, "lollipopResults")
  }
})

# ═══════════════════════════════════════════════════════════
# 4. Hospital Quality Metrics Workflow
# ═══════════════════════════════════════════════════════════

test_that("lollipop handles hospital quality metrics workflow", {
  devtools::load_all()

  # Survival rate (higher is better)
  result1 <- lollipop(
    data = lollipop_hospital,
    dep = "survival_rate",
    group = "hospital",
    sortBy = "value_desc",
    orientation = "horizontal",
    showValues = TRUE,
    conditionalColor = TRUE,
    colorThreshold = 80,
    title = "Hospital Survival Rates"
  )
  expect_s3_class(result1, "lollipopResults")

  # Complication rate (lower is better)
  result2 <- lollipop(
    data = lollipop_hospital,
    dep = "complication_rate",
    group = "hospital",
    sortBy = "value_asc",
    orientation = "horizontal",
    showValues = TRUE,
    conditionalColor = TRUE,
    colorThreshold = 15,
    title = "Hospital Complication Rates (Lower is Better)"
  )
  expect_s3_class(result2, "lollipopResults")

  # Patient satisfaction
  result3 <- lollipop(
    data = lollipop_hospital,
    dep = "patient_satisfaction",
    group = "hospital",
    sortBy = "value_desc",
    showMean = TRUE,
    colorScheme = "viridis",
    theme = "publication"
  )
  expect_s3_class(result3, "lollipopResults")
})

# ═══════════════════════════════════════════════════════════
# 5. All Datasets with Consistent Parameters
# ═══════════════════════════════════════════════════════════

test_that("lollipop handles all datasets with consistent parameters", {
  devtools::load_all()

  # lollipop_test
  result1 <- lollipop(
    data = lollipop_test,
    dep = "hemoglobin",
    group = "treatment_group",
    aggregation = "mean",
    sortBy = "value_desc",
    colorScheme = "clinical"
  )
  expect_s3_class(result1, "lollipopResults")

  # lollipop_treatment
  result2 <- lollipop(
    data = lollipop_treatment,
    dep = "tumor_reduction",
    group = "treatment",
    sortBy = "value_desc",
    colorScheme = "clinical"
  )
  expect_s3_class(result2, "lollipopResults")

  # lollipop_biomarkers
  result3 <- lollipop(
    data = lollipop_biomarkers,
    dep = "cea",
    group = "cancer_type",
    aggregation = "median",
    sortBy = "value_desc",
    colorScheme = "clinical"
  )
  expect_s3_class(result3, "lollipopResults")

  # lollipop_hospital
  result4 <- lollipop(
    data = lollipop_hospital,
    dep = "survival_rate",
    group = "hospital",
    sortBy = "value_desc",
    colorScheme = "clinical"
  )
  expect_s3_class(result4, "lollipopResults")

  # lollipop_small
  result5 <- lollipop(
    data = lollipop_small,
    dep = "measurement",
    group = "category",
    sortBy = "value_desc",
    colorScheme = "clinical"
  )
  expect_s3_class(result5, "lollipopResults")

  # lollipop_aggregated
  result6 <- lollipop(
    data = lollipop_aggregated,
    dep = "mean_income",
    group = "region",
    sortBy = "value_desc",
    colorScheme = "clinical"
  )
  expect_s3_class(result6, "lollipopResults")
})

# ═══════════════════════════════════════════════════════════
# 6. Comparative Analysis - Vertical vs Horizontal
# ═══════════════════════════════════════════════════════════

test_that("lollipop produces consistent results for vertical and horizontal orientations", {
  devtools::load_all()

  # Vertical
  result_vert <- lollipop(
    data = lollipop_test,
    dep = "albumin",
    group = "disease_severity",
    aggregation = "mean",
    orientation = "vertical"
  )
  expect_s3_class(result_vert, "lollipopResults")

  # Horizontal
  result_horiz <- lollipop(
    data = lollipop_test,
    dep = "albumin",
    group = "disease_severity",
    aggregation = "mean",
    orientation = "horizontal"
  )
  expect_s3_class(result_horiz, "lollipopResults")
})

# ═══════════════════════════════════════════════════════════
# 7. Progressive Feature Addition
# ═══════════════════════════════════════════════════════════

test_that("lollipop handles progressive feature addition", {
  devtools::load_all()

  # Step 1: Basic
  result1 <- lollipop(
    data = lollipop_test,
    dep = "white_blood_cells",
    group = "treatment_group"
  )
  expect_s3_class(result1, "lollipopResults")

  # Step 2: Add aggregation
  result2 <- lollipop(
    data = lollipop_test,
    dep = "white_blood_cells",
    group = "treatment_group",
    aggregation = "mean"
  )
  expect_s3_class(result2, "lollipopResults")

  # Step 3: Add sorting
  result3 <- lollipop(
    data = lollipop_test,
    dep = "white_blood_cells",
    group = "treatment_group",
    aggregation = "mean",
    sortBy = "value_desc"
  )
  expect_s3_class(result3, "lollipopResults")

  # Step 4: Add highlighting
  result4 <- lollipop(
    data = lollipop_test,
    dep = "white_blood_cells",
    group = "treatment_group",
    aggregation = "mean",
    sortBy = "value_desc",
    useHighlight = TRUE,
    highlight = "Drug A"
  )
  expect_s3_class(result4, "lollipopResults")

  # Step 5: Add conditional coloring
  result5 <- lollipop(
    data = lollipop_test,
    dep = "white_blood_cells",
    group = "treatment_group",
    aggregation = "mean",
    sortBy = "value_desc",
    useHighlight = TRUE,
    highlight = "Drug A",
    conditionalColor = TRUE,
    colorThreshold = 11
  )
  expect_s3_class(result5, "lollipopResults")

  # Step 6: Add visual enhancements
  result6 <- lollipop(
    data = lollipop_test,
    dep = "white_blood_cells",
    group = "treatment_group",
    aggregation = "mean",
    sortBy = "value_desc",
    useHighlight = TRUE,
    highlight = "Drug A",
    conditionalColor = TRUE,
    colorThreshold = 11,
    showValues = TRUE,
    showMean = TRUE
  )
  expect_s3_class(result6, "lollipopResults")

  # Step 7: Publication-ready
  result7 <- lollipop(
    data = lollipop_test,
    dep = "white_blood_cells",
    group = "treatment_group",
    aggregation = "mean",
    sortBy = "value_desc",
    useHighlight = TRUE,
    highlight = "Drug A",
    conditionalColor = TRUE,
    colorThreshold = 11,
    showValues = TRUE,
    showMean = TRUE,
    colorScheme = "clinical",
    theme = "publication",
    title = "WBC Count by Treatment",
    ylabel = "WBC (×10³/μL)"
  )
  expect_s3_class(result7, "lollipopResults")
})

# ═══════════════════════════════════════════════════════════
# 8. Multiple Lab Parameters from Same Dataset
# ═══════════════════════════════════════════════════════════

test_that("lollipop handles multiple analyses from same dataset", {
  devtools::load_all()

  lab_params <- c("hemoglobin", "albumin", "creatinine",
                  "platelet_count", "white_blood_cells", "alt", "crp")

  results <- list()

  for (param in lab_params) {
    results[[param]] <- lollipop(
      data = lollipop_test,
      dep = param,
      group = "treatment_group",
      aggregation = "mean",
      sortBy = "value_desc",
      showMean = TRUE,
      colorScheme = "clinical",
      theme = "publication"
    )
    expect_s3_class(results[[param]], "lollipopResults")
  }

  # All should complete successfully
  expect_equal(length(results), length(lab_params))
})

# ═══════════════════════════════════════════════════════════
# 9. Repeated Analysis with Same Parameters
# ═══════════════════════════════════════════════════════════

test_that("lollipop produces consistent results on repeated calls", {
  devtools::load_all()

  # Run same analysis 3 times
  result1 <- lollipop(
    data = lollipop_test,
    dep = "hemoglobin",
    group = "treatment_group",
    aggregation = "mean"
  )

  result2 <- lollipop(
    data = lollipop_test,
    dep = "hemoglobin",
    group = "treatment_group",
    aggregation = "mean"
  )

  result3 <- lollipop(
    data = lollipop_test,
    dep = "hemoglobin",
    group = "treatment_group",
    aggregation = "mean"
  )

  expect_s3_class(result1, "lollipopResults")
  expect_s3_class(result2, "lollipopResults")
  expect_s3_class(result3, "lollipopResults")
})

# ═══════════════════════════════════════════════════════════
# 10. Multi-center Study Comparison
# ═══════════════════════════════════════════════════════════

test_that("lollipop handles multi-center comparison workflow", {
  devtools::load_all()

  # By hospital - hemoglobin
  result1 <- lollipop(
    data = lollipop_test,
    dep = "hemoglobin",
    group = "hospital",
    aggregation = "mean",
    sortBy = "value_desc",
    showValues = TRUE,
    colorScheme = "clinical"
  )
  expect_s3_class(result1, "lollipopResults")

  # By hospital - platelet count
  result2 <- lollipop(
    data = lollipop_test,
    dep = "platelet_count",
    group = "hospital",
    aggregation = "mean",
    sortBy = "value_desc",
    baseline = 150,
    showValues = TRUE,
    colorScheme = "clinical"
  )
  expect_s3_class(result2, "lollipopResults")
})
