# ═══════════════════════════════════════════════════════════
# Argument Combination Tests: diagnosticmeta
# ═══════════════════════════════════════════════════════════
#
# Tests all argument combinations and option interactions
# for the diagnosticmeta jamovi function

library(testthat)
library(ClinicoPath)

# Load test data
data(diagnosticmeta_test, package = "ClinicoPath")
data(diagnosticmeta_test_categorical, package = "ClinicoPath")

test_that("diagnosticmeta handles meta-regression with continuous covariate", {
  result <- diagnosticmeta(
    data = diagnosticmeta_test,
    study = "study",
    true_positives = "true_positives",
    false_positives = "false_positives",
    false_negatives = "false_negatives",
    true_negatives = "true_negatives",
    covariate = "year",
    meta_regression = TRUE
  )

  expect_s3_class(result, "diagnosticmetaClass")
})

test_that("diagnosticmeta handles meta-regression with categorical covariate", {
  result <- diagnosticmeta(
    data = diagnosticmeta_test_categorical,
    study = "study",
    true_positives = "true_positives",
    false_positives = "false_positives",
    false_negatives = "false_negatives",
    true_negatives = "true_negatives",
    covariate = "imaging_modality",
    meta_regression = TRUE
  )

  expect_s3_class(result, "diagnosticmetaClass")
})

test_that("diagnosticmeta handles zero-cell correction methods", {
  # Load dataset with zero cells
  data(diagnosticmeta_test_zeros, package = "ClinicoPath")

  correction_methods <- c("none", "constant", "treatment_arm", "empirical")

  for (method in correction_methods) {
    result <- diagnosticmeta(
      data = diagnosticmeta_test_zeros,
      study = "study",
      true_positives = "true_positives",
      false_positives = "false_positives",
      false_negatives = "false_negatives",
      true_negatives = "true_negatives",
      zero_cell_correction = method
    )

    expect_s3_class(result, "diagnosticmetaClass")
  }
})

test_that("diagnosticmeta handles all analysis options combined", {
  result <- diagnosticmeta(
    data = diagnosticmeta_test,
    study = "study",
    true_positives = "true_positives",
    false_positives = "false_positives",
    false_negatives = "false_negatives",
    true_negatives = "true_negatives",
    bivariate_analysis = TRUE,
    hsroc_analysis = TRUE,
    meta_regression = TRUE,
    covariate = "year",
    heterogeneity_analysis = TRUE,
    publication_bias = TRUE,
    confidence_level = 95,
    method = "reml"
  )

  expect_s3_class(result, "diagnosticmetaClass")
})

test_that("diagnosticmeta handles all plot options combined", {
  result <- diagnosticmeta(
    data = diagnosticmeta_test,
    study = "study",
    true_positives = "true_positives",
    false_positives = "false_positives",
    false_negatives = "false_negatives",
    true_negatives = "true_negatives",
    forest_plot = TRUE,
    sroc_plot = TRUE,
    funnel_plot = TRUE,
    show_individual_studies = TRUE,
    color_palette = "colorblind_safe"
  )

  expect_s3_class(result, "diagnosticmetaClass")
})

test_that("diagnosticmeta handles all color palettes", {
  color_palettes <- c("standard", "colorblind_safe", "high_contrast",
                      "viridis", "plasma")

  for (palette in color_palettes) {
    result <- diagnosticmeta(
      data = diagnosticmeta_test,
      study = "study",
      true_positives = "true_positives",
      false_positives = "false_positives",
      false_negatives = "false_negatives",
      true_negatives = "true_negatives",
      forest_plot = TRUE,
      color_palette = palette
    )

    expect_s3_class(result, "diagnosticmetaClass")
  }
})

test_that("diagnosticmeta handles all display options", {
  result <- diagnosticmeta(
    data = diagnosticmeta_test,
    study = "study",
    true_positives = "true_positives",
    false_positives = "false_positives",
    false_negatives = "false_negatives",
    true_negatives = "true_negatives",
    show_individual_studies = TRUE,
    show_interpretation = TRUE,
    show_methodology = TRUE,
    show_analysis_summary = TRUE,
    show_plot_explanations = TRUE
  )

  expect_s3_class(result, "diagnosticmetaClass")
})

test_that("diagnosticmeta handles HSROC analysis separately", {
  # HSROC without bivariate
  result_hsroc <- diagnosticmeta(
    data = diagnosticmeta_test,
    study = "study",
    true_positives = "true_positives",
    false_positives = "false_positives",
    false_negatives = "false_negatives",
    true_negatives = "true_negatives",
    bivariate_analysis = FALSE,
    hsroc_analysis = TRUE
  )

  expect_s3_class(result_hsroc, "diagnosticmetaClass")

  # Both HSROC and bivariate
  result_both <- diagnosticmeta(
    data = diagnosticmeta_test,
    study = "study",
    true_positives = "true_positives",
    false_positives = "false_positives",
    false_negatives = "false_negatives",
    true_negatives = "true_negatives",
    bivariate_analysis = TRUE,
    hsroc_analysis = TRUE
  )

  expect_s3_class(result_both, "diagnosticmetaClass")
})

test_that("diagnosticmeta handles heterogeneity analysis option", {
  result <- diagnosticmeta(
    data = diagnosticmeta_test,
    study = "study",
    true_positives = "true_positives",
    false_positives = "false_positives",
    false_negatives = "false_negatives",
    true_negatives = "true_negatives",
    heterogeneity_analysis = TRUE
  )

  expect_s3_class(result, "diagnosticmetaClass")

  # Check that heterogeneity results exist (if implemented)
  # expect_true(!is.null(result$results$heterogeneity))
})

test_that("diagnosticmeta handles publication bias assessment", {
  result <- diagnosticmeta(
    data = diagnosticmeta_test,
    study = "study",
    true_positives = "true_positives",
    false_positives = "false_positives",
    false_negatives = "false_negatives",
    true_negatives = "true_negatives",
    publication_bias = TRUE,
    funnel_plot = TRUE
  )

  expect_s3_class(result, "diagnosticmetaClass")
})

test_that("diagnosticmeta handles large dataset efficiently", {
  # Load large test data
  data(diagnosticmeta_test_large, package = "ClinicoPath")

  # Time the analysis
  start_time <- Sys.time()

  result <- diagnosticmeta(
    data = diagnosticmeta_test_large,
    study = "study",
    true_positives = "true_positives",
    false_positives = "false_positives",
    false_negatives = "false_negatives",
    true_negatives = "true_negatives",
    bivariate_analysis = TRUE
  )

  end_time <- Sys.time()
  time_taken <- as.numeric(difftime(end_time, start_time, units = "secs"))

  expect_s3_class(result, "diagnosticmetaClass")

  # Should complete in reasonable time (< 30 seconds)
  expect_lt(time_taken, 30)
})

test_that("diagnosticmeta handles different combinations of estimation method and correction", {
  methods <- c("reml", "ml")
  corrections <- c("none", "constant")

  for (method in methods) {
    for (correction in corrections) {
      result <- diagnosticmeta(
        data = diagnosticmeta_test,
        study = "study",
        true_positives = "true_positives",
        false_positives = "false_positives",
        false_negatives = "false_negatives",
        true_negatives = "true_negatives",
        method = method,
        zero_cell_correction = correction
      )

      expect_s3_class(result, "diagnosticmetaClass")
    }
  }
})

test_that("diagnosticmeta handles quality score as covariate", {
  result <- diagnosticmeta(
    data = diagnosticmeta_test,
    study = "study",
    true_positives = "true_positives",
    false_positives = "false_positives",
    false_negatives = "false_negatives",
    true_negatives = "true_negatives",
    covariate = "quality_score",
    meta_regression = TRUE
  )

  expect_s3_class(result, "diagnosticmetaClass")
})
