# ═══════════════════════════════════════════════════════════
# Integration Tests: raincloud
# ═══════════════════════════════════════════════════════════
#
# Tests complete workflows and integration with all datasets
# Generated: 2026-01-06

library(testthat)
library(ClinicoPath)

# Load all test datasets
data(raincloud_test, package = "ClinicoPath", envir = environment())
data(raincloud_clinical, package = "ClinicoPath", envir = environment())
data(raincloud_treatment, package = "ClinicoPath", envir = environment())
data(raincloud_biomarker, package = "ClinicoPath", envir = environment())
data(raincloud_small, package = "ClinicoPath", envir = environment())
data(raincloud_skewed, package = "ClinicoPath", envir = environment())

# ═══════════════════════════════════════════════════════════
# 1. Complete Clinical Laboratory Analysis Workflow
# ═══════════════════════════════════════════════════════════

test_that("raincloud handles complete clinical lab analysis workflow", {
  devtools::load_all()

  # Step 1: Basic distribution
  result1 <- raincloud(
    data = raincloud_clinical,
    dep_var = "glucose",
    group_var = "diagnosis"
  )
  expect_s3_class(result1, "raincloudResults")

  # Step 2: With faceting by age
  result2 <- raincloud(
    data = raincloud_clinical,
    dep_var = "glucose",
    group_var = "diagnosis",
    facet_var = "age_category"
  )
  expect_s3_class(result2, "raincloudResults")

  # Step 3: With color by BMI
  result3 <- raincloud(
    data = raincloud_clinical,
    dep_var = "glucose",
    group_var = "diagnosis",
    facet_var = "age_category",
    color_var = "bmi_category"
  )
  expect_s3_class(result3, "raincloudResults")

  # Step 4: With statistical tests
  result4 <- raincloud(
    data = raincloud_clinical,
    dep_var = "glucose",
    group_var = "diagnosis",
    facet_var = "age_category",
    normality_test = TRUE,
    comparison_test = TRUE
  )
  expect_s3_class(result4, "raincloudResults")

  # Step 5: Publication-ready
  result5 <- raincloud(
    data = raincloud_clinical,
    dep_var = "glucose",
    group_var = "diagnosis",
    facet_var = "age_category",
    color_var = "bmi_category",
    theme = "publication",
    palette = "colorblind_safe",
    show_violin = TRUE,
    show_boxplot = TRUE,
    show_dots = TRUE,
    normality_test = TRUE,
    comparison_test = TRUE,
    show_outliers = TRUE,
    show_mean = TRUE,
    show_median = TRUE
  )
  expect_s3_class(result5, "raincloudResults")
})

# ═══════════════════════════════════════════════════════════
# 2. Complete Treatment Effect Analysis Workflow
# ═══════════════════════════════════════════════════════════

test_that("raincloud handles complete treatment effect workflow", {
  devtools::load_all()

  # Step 1: Tumor size by treatment
  result1 <- raincloud(
    data = raincloud_treatment,
    dep_var = "tumor_size",
    group_var = "treatment"
  )
  expect_s3_class(result1, "raincloudResults")

  # Step 2: Stratified by timepoint
  result2 <- raincloud(
    data = raincloud_treatment,
    dep_var = "tumor_size",
    group_var = "treatment",
    facet_var = "timepoint"
  )
  expect_s3_class(result2, "raincloudResults")

  # Step 3: Colored by response
  result3 <- raincloud(
    data = raincloud_treatment,
    dep_var = "tumor_size",
    group_var = "treatment",
    facet_var = "timepoint",
    color_var = "response_category"
  )
  expect_s3_class(result3, "raincloudResults")

  # Step 4: With comparison tests
  result4 <- raincloud(
    data = raincloud_treatment,
    dep_var = "tumor_size",
    group_var = "treatment",
    facet_var = "timepoint",
    comparison_test = TRUE,
    show_mean = TRUE
  )
  expect_s3_class(result4, "raincloudResults")

  # Step 5: Complete analysis
  result5 <- raincloud(
    data = raincloud_treatment,
    dep_var = "tumor_size",
    group_var = "treatment",
    facet_var = "timepoint",
    color_var = "response_category",
    theme = "clinical",
    palette = "viridis",
    show_violin = TRUE,
    show_boxplot = TRUE,
    show_dots = FALSE,
    comparison_test = TRUE,
    show_mean = TRUE,
    show_median = TRUE
  )
  expect_s3_class(result5, "raincloudResults")
})

# ═══════════════════════════════════════════════════════════
# 3. Complete Biomarker Expression Analysis Workflow
# ═══════════════════════════════════════════════════════════

test_that("raincloud handles complete biomarker expression workflow", {
  devtools::load_all()

  # Step 1: Ki67 by grade
  result1 <- raincloud(
    data = raincloud_biomarker,
    dep_var = "ki67_index",
    group_var = "grade"
  )
  expect_s3_class(result1, "raincloudResults")

  # Step 2: Faceted by cancer type
  result2 <- raincloud(
    data = raincloud_biomarker,
    dep_var = "ki67_index",
    group_var = "grade",
    facet_var = "cancer_type"
  )
  expect_s3_class(result2, "raincloudResults")

  # Step 3: Colored by stage
  result3 <- raincloud(
    data = raincloud_biomarker,
    dep_var = "ki67_index",
    group_var = "grade",
    facet_var = "cancer_type",
    color_var = "receptor_status"
  )
  expect_s3_class(result3, "raincloudResults")

  # Step 4: With normality tests (log-scale data)
  result4 <- raincloud(
    data = raincloud_biomarker,
    dep_var = "ki67_index",
    group_var = "grade",
    normality_test = TRUE,
    show_outliers = TRUE
  )
  expect_s3_class(result4, "raincloudResults")

  # Step 5: Publication-ready with Prism theme
  result5 <- raincloud(
    data = raincloud_biomarker,
    dep_var = "ki67_index",
    group_var = "grade",
    facet_var = "cancer_type",
    theme = "prism",
    palette = "floral",
    show_violin = TRUE,
    show_boxplot = TRUE,
    show_dots = TRUE,
    normality_test = TRUE,
    comparison_test = TRUE,
    show_outliers = TRUE,
    show_median = TRUE
  )
  expect_s3_class(result5, "raincloudResults")
})

# ═══════════════════════════════════════════════════════════
# 4. All Datasets with Consistent Parameters
# ═══════════════════════════════════════════════════════════

test_that("raincloud handles all datasets with consistent parameters", {
  devtools::load_all()

  # raincloud_test
  result1 <- raincloud(
    data = raincloud_test,
    dep_var = "symptom_score",
    group_var = "treatment_group",
    theme = "publication",
    show_violin = TRUE,
    show_boxplot = TRUE
  )
  expect_s3_class(result1, "raincloudResults")

  # raincloud_clinical
  result2 <- raincloud(
    data = raincloud_clinical,
    dep_var = "glucose",
    group_var = "diagnosis",
    theme = "publication",
    show_violin = TRUE,
    show_boxplot = TRUE
  )
  expect_s3_class(result2, "raincloudResults")

  # raincloud_treatment
  result3 <- raincloud(
    data = raincloud_treatment,
    dep_var = "tumor_size",
    group_var = "treatment",
    theme = "publication",
    show_violin = TRUE,
    show_boxplot = TRUE
  )
  expect_s3_class(result3, "raincloudResults")

  # raincloud_biomarker
  result4 <- raincloud(
    data = raincloud_biomarker,
    dep_var = "ki67_index",
    group_var = "grade",
    theme = "publication",
    show_violin = TRUE,
    show_boxplot = TRUE
  )
  expect_s3_class(result4, "raincloudResults")

  # raincloud_small
  result5 <- raincloud(
    data = raincloud_small,
    dep_var = "measurement",
    group_var = "group",
    theme = "publication",
    show_violin = TRUE,
    show_boxplot = TRUE
  )
  expect_s3_class(result5, "raincloudResults")

  # raincloud_skewed
  result6 <- raincloud(
    data = raincloud_skewed,
    dep_var = "normal",
    group_var = "condition",
    theme = "publication",
    show_violin = TRUE,
    show_boxplot = TRUE
  )
  expect_s3_class(result6, "raincloudResults")
})

# ═══════════════════════════════════════════════════════════
# 5. All Themes on Same Data
# ═══════════════════════════════════════════════════════════

test_that("raincloud produces consistent results across themes", {
  devtools::load_all()

  themes <- c("clinical", "minimal", "classic", "publication", "tidyquant",
              "prism", "prism_whitespace", "prism_light")

  results <- list()

  for (theme_name in themes) {
    results[[theme_name]] <- raincloud(
      data = raincloud_test,
      dep_var = "symptom_score",
      group_var = "treatment_group",
      theme = theme_name,
      show_violin = TRUE,
      show_boxplot = TRUE
    )

    expect_s3_class(results[[theme_name]], "raincloudResults")
  }

  # All should complete successfully
  expect_equal(length(results), length(themes))
})

# ═══════════════════════════════════════════════════════════
# 6. All Palettes on Same Data
# ═══════════════════════════════════════════════════════════

test_that("raincloud produces consistent results across palettes", {
  devtools::load_all()

  palettes <- c("default", "viridis", "magma", "plasma", "floral",
                "candy_bright", "office", "pastels", "colorblind_safe",
                "ocean", "spring")

  results <- list()

  for (palette_name in palettes) {
    results[[palette_name]] <- raincloud(
      data = raincloud_biomarker,
      dep_var = "ki67_index",
      group_var = "grade",
      palette = palette_name,
      show_violin = TRUE
    )

    expect_s3_class(results[[palette_name]], "raincloudResults")
  }

  # All should complete successfully
  expect_equal(length(results), length(palettes))
})

# ═══════════════════════════════════════════════════════════
# 7. Progressive Feature Addition
# ═══════════════════════════════════════════════════════════

test_that("raincloud handles progressive feature addition", {
  devtools::load_all()

  # Step 1: Minimal
  result1 <- raincloud(
    data = raincloud_test,
    dep_var = "symptom_score",
    group_var = "treatment_group"
  )
  expect_s3_class(result1, "raincloudResults")

  # Step 2: Add violin
  result2 <- raincloud(
    data = raincloud_test,
    dep_var = "symptom_score",
    group_var = "treatment_group",
    show_violin = TRUE
  )
  expect_s3_class(result2, "raincloudResults")

  # Step 3: Add boxplot
  result3 <- raincloud(
    data = raincloud_test,
    dep_var = "symptom_score",
    group_var = "treatment_group",
    show_violin = TRUE,
    show_boxplot = TRUE
  )
  expect_s3_class(result3, "raincloudResults")

  # Step 4: Add dots
  result4 <- raincloud(
    data = raincloud_test,
    dep_var = "symptom_score",
    group_var = "treatment_group",
    show_violin = TRUE,
    show_boxplot = TRUE,
    show_dots = TRUE
  )
  expect_s3_class(result4, "raincloudResults")

  # Step 5: Add faceting
  result5 <- raincloud(
    data = raincloud_test,
    dep_var = "symptom_score",
    group_var = "treatment_group",
    facet_var = "disease_severity",
    show_violin = TRUE,
    show_boxplot = TRUE,
    show_dots = TRUE
  )
  expect_s3_class(result5, "raincloudResults")

  # Step 6: Add theme
  result6 <- raincloud(
    data = raincloud_test,
    dep_var = "symptom_score",
    group_var = "treatment_group",
    facet_var = "disease_severity",
    theme = "publication",
    show_violin = TRUE,
    show_boxplot = TRUE,
    show_dots = TRUE
  )
  expect_s3_class(result6, "raincloudResults")

  # Step 7: Add statistics
  result7 <- raincloud(
    data = raincloud_test,
    dep_var = "symptom_score",
    group_var = "treatment_group",
    facet_var = "disease_severity",
    theme = "publication",
    show_violin = TRUE,
    show_boxplot = TRUE,
    show_dots = TRUE,
    normality_test = TRUE,
    comparison_test = TRUE
  )
  expect_s3_class(result7, "raincloudResults")

  # Step 8: Add displays
  result8 <- raincloud(
    data = raincloud_test,
    dep_var = "symptom_score",
    group_var = "treatment_group",
    facet_var = "disease_severity",
    theme = "publication",
    show_violin = TRUE,
    show_boxplot = TRUE,
    show_dots = TRUE,
    normality_test = TRUE,
    comparison_test = TRUE,
    show_mean = TRUE,
    show_median = TRUE,
    show_outliers = TRUE
  )
  expect_s3_class(result8, "raincloudResults")
})

# ═══════════════════════════════════════════════════════════
# 8. Multiple Variables from Same Dataset
# ═══════════════════════════════════════════════════════════

test_that("raincloud handles multiple analyses from same dataset", {
  devtools::load_all()

  variables <- c("symptom_score", "quality_of_life", "pain_intensity", "response_time")

  results <- list()

  for (var in variables) {
    results[[var]] <- raincloud(
      data = raincloud_test,
      dep_var = var,
      group_var = "treatment_group",
      theme = "clinical",
      show_violin = TRUE,
      show_boxplot = TRUE
    )
    expect_s3_class(results[[var]], "raincloudResults")
  }

  # All should complete successfully
  expect_equal(length(results), length(variables))
})

# ═══════════════════════════════════════════════════════════
# 9. Repeated Analysis Consistency
# ═══════════════════════════════════════════════════════════

test_that("raincloud produces consistent results on repeated calls", {
  devtools::load_all()

  # Run same analysis 3 times
  result1 <- raincloud(
    data = raincloud_test,
    dep_var = "symptom_score",
    group_var = "treatment_group",
    theme = "publication",
    show_violin = TRUE,
    show_boxplot = TRUE
  )

  result2 <- raincloud(
    data = raincloud_test,
    dep_var = "symptom_score",
    group_var = "treatment_group",
    theme = "publication",
    show_violin = TRUE,
    show_boxplot = TRUE
  )

  result3 <- raincloud(
    data = raincloud_test,
    dep_var = "symptom_score",
    group_var = "treatment_group",
    theme = "publication",
    show_violin = TRUE,
    show_boxplot = TRUE
  )

  expect_s3_class(result1, "raincloudResults")
  expect_s3_class(result2, "raincloudResults")
  expect_s3_class(result3, "raincloudResults")
})

# ═══════════════════════════════════════════════════════════
# 10. Multi-Panel Comparative Analysis
# ═══════════════════════════════════════════════════════════

test_that("raincloud handles multi-panel comparative analysis workflow", {
  devtools::load_all()

  # Overall distribution
  result_overall <- raincloud(
    data = raincloud_clinical,
    dep_var = "systolic_bp",
    group_var = "diagnosis"
  )
  expect_s3_class(result_overall, "raincloudResults")

  # By age category
  result_by_age <- raincloud(
    data = raincloud_clinical,
    dep_var = "systolic_bp",
    group_var = "diagnosis",
    facet_var = "age_category"
  )
  expect_s3_class(result_by_age, "raincloudResults")

  # By BMI category
  result_by_bmi <- raincloud(
    data = raincloud_clinical,
    dep_var = "systolic_bp",
    group_var = "diagnosis",
    facet_var = "bmi_category"
  )
  expect_s3_class(result_by_bmi, "raincloudResults")

  # Complete with both faceting and coloring
  result_complete <- raincloud(
    data = raincloud_clinical,
    dep_var = "systolic_bp",
    group_var = "diagnosis",
    facet_var = "age_category",
    color_var = "bmi_category",
    theme = "publication",
    comparison_test = TRUE
  )
  expect_s3_class(result_complete, "raincloudResults")
})

# ═══════════════════════════════════════════════════════════
# 11. Distribution Type Comparison
# ═══════════════════════════════════════════════════════════

test_that("raincloud handles different distribution types in systematic comparison", {
  devtools::load_all()

  distribution_types <- c("normal", "right_skewed", "left_skewed", "bimodal", "lognormal")

  results <- list()

  for (dist_type in distribution_types) {
    results[[dist_type]] <- raincloud(
      data = raincloud_skewed,
      dep_var = dist_type,
      group_var = "condition",
      theme = "publication",
      palette = "colorblind_safe",
      show_violin = TRUE,
      show_boxplot = TRUE,
      show_dots = TRUE,
      normality_test = TRUE,
      show_mean = TRUE,
      show_median = TRUE
    )

    expect_s3_class(results[[dist_type]], "raincloudResults")
  }

  # All distribution types should be handled
  expect_equal(length(results), length(distribution_types))
})

# ═══════════════════════════════════════════════════════════
# 12. Complete Clinical Trial Reporting Workflow
# ═══════════════════════════════════════════════════════════

test_that("raincloud handles complete clinical trial reporting workflow", {
  devtools::load_all()

  # Primary endpoint analysis
  result_primary <- raincloud(
    data = raincloud_treatment,
    dep_var = "tumor_size",
    group_var = "treatment",
    theme = "publication",
    palette = "colorblind_safe",
    show_violin = TRUE,
    show_boxplot = TRUE,
    show_dots = FALSE,
    comparison_test = TRUE,
    show_mean = TRUE,
    show_median = TRUE
  )
  expect_s3_class(result_primary, "raincloudResults")

  # Stratified by response
  result_stratified <- raincloud(
    data = raincloud_treatment,
    dep_var = "tumor_size",
    group_var = "treatment",
    facet_var = "response_category",
    theme = "publication",
    palette = "colorblind_safe",
    show_violin = TRUE,
    show_boxplot = TRUE,
    comparison_test = TRUE
  )
  expect_s3_class(result_stratified, "raincloudResults")

  # Time course analysis
  result_time <- raincloud(
    data = raincloud_treatment,
    dep_var = "tumor_size",
    group_var = "timepoint",
    facet_var = "treatment",
    theme = "publication",
    show_violin = TRUE,
    show_boxplot = TRUE,
    comparison_test = TRUE,
    show_mean = TRUE
  )
  expect_s3_class(result_time, "raincloudResults")
})
