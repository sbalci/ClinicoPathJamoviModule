# ═══════════════════════════════════════════════════════════
# Argument Tests: nogoldstandard
# ═══════════════════════════════════════════════════════════
library(testthat)
library(ClinicoPath)
data(nogoldstandard_test, package = "ClinicoPath")
data(nogoldstandard_pathology, package = "ClinicoPath")
data(nogoldstandard_validation, package = "ClinicoPath")

test_that("nogoldstandard respects all analysis methods", {
  methods <- c("latent_class", "composite", "all_positive", "any_positive", "bayesian")

  for (method in methods) {
    result <- nogoldstandard(
      data = nogoldstandard_test,
      test1 = "Test1",
      test1Positive = "Positive",
      test2 = "Test2",
      test2Positive = "Positive",
      method = method
    )
    expect_s3_class(result, "nogoldstandardClass")
  }
})

test_that("nogoldstandard respects bootstrap parameter", {
  # Without bootstrap
  result_no_boot <- nogoldstandard(
    data = nogoldstandard_test,
    test1 = "Test1",
    test1Positive = "Positive",
    test2 = "Test2",
    test2Positive = "Positive",
    method = "latent_class",
    bootstrap = FALSE
  )
  expect_s3_class(result_no_boot, "nogoldstandardClass")

  # With bootstrap (small nboot for testing)
  result_boot <- nogoldstandard(
    data = nogoldstandard_test,
    test1 = "Test1",
    test1Positive = "Positive",
    test2 = "Test2",
    test2Positive = "Positive",
    method = "latent_class",
    bootstrap = TRUE,
    nboot = 100
  )
  expect_s3_class(result_boot, "nogoldstandardClass")
})

test_that("nogoldstandard respects nboot parameter", {
  # Small number
  result_small <- nogoldstandard(
    data = nogoldstandard_test,
    test1 = "Test1",
    test1Positive = "Positive",
    test2 = "Test2",
    test2Positive = "Positive",
    method = "latent_class",
    bootstrap = TRUE,
    nboot = 100
  )
  expect_no_error(result_small)

  # Larger number
  result_large <- nogoldstandard(
    data = nogoldstandard_test,
    test1 = "Test1",
    test1Positive = "Positive",
    test2 = "Test2",
    test2Positive = "Positive",
    method = "latent_class",
    bootstrap = TRUE,
    nboot = 500
  )
  expect_no_error(result_large)
})

test_that("nogoldstandard respects alpha parameter", {
  # 95% CI (alpha = 0.05)
  result_95 <- nogoldstandard(
    data = nogoldstandard_test,
    test1 = "Test1",
    test1Positive = "Positive",
    test2 = "Test2",
    test2Positive = "Positive",
    alpha = 0.05
  )
  expect_no_error(result_95)

  # 99% CI (alpha = 0.01)
  result_99 <- nogoldstandard(
    data = nogoldstandard_test,
    test1 = "Test1",
    test1Positive = "Positive",
    test2 = "Test2",
    test2Positive = "Positive",
    alpha = 0.01
  )
  expect_no_error(result_99)
})

test_that("nogoldstandard respects verbose parameter", {
  # Verbose off
  result_quiet <- nogoldstandard(
    data = nogoldstandard_test,
    test1 = "Test1",
    test1Positive = "Positive",
    test2 = "Test2",
    test2Positive = "Positive",
    verbose = FALSE
  )
  expect_no_error(result_quiet)

  # Verbose on
  result_verbose <- nogoldstandard(
    data = nogoldstandard_test,
    test1 = "Test1",
    test1Positive = "Positive",
    test2 = "Test2",
    test2Positive = "Positive",
    verbose = TRUE
  )
  expect_no_error(result_verbose)
})

test_that("nogoldstandard respects clinical presets", {
  presets <- c("none", "diagnostic_validation", "pathology_agreement",
               "tumor_markers", "screening_evaluation")

  for (preset in presets) {
    result <- nogoldstandard(
      data = nogoldstandard_test,
      test1 = "Test1",
      test1Positive = "Positive",
      test2 = "Test2",
      test2Positive = "Positive",
      clinicalPreset = preset
    )
    expect_s3_class(result, "nogoldstandardClass")
  }
})

test_that("nogoldstandard handles diagnostic_validation preset", {
  result <- nogoldstandard(
    data = nogoldstandard_validation,
    test1 = "New_Test",
    test1Positive = "Positive",
    test2 = "Reference1",
    test2Positive = "Positive",
    test3 = "Reference2",
    test3Positive = "Positive",
    clinicalPreset = "diagnostic_validation"
  )
  expect_no_error(result)
})

test_that("nogoldstandard handles pathology_agreement preset", {
  result <- nogoldstandard(
    data = nogoldstandard_pathology,
    test1 = "Pathologist1",
    test1Positive = "Malignant",
    test2 = "Pathologist2",
    test2Positive = "Malignant",
    test3 = "Pathologist3",
    test3Positive = "Malignant",
    clinicalPreset = "pathology_agreement"
  )
  expect_no_error(result)
})

test_that("nogoldstandard handles different numbers of tests", {
  # Two tests
  result_2 <- nogoldstandard(
    data = nogoldstandard_test,
    test1 = "Test1",
    test1Positive = "Positive",
    test2 = "Test2",
    test2Positive = "Positive"
  )
  expect_no_error(result_2)

  # Three tests
  result_3 <- nogoldstandard(
    data = nogoldstandard_pathology,
    test1 = "Pathologist1",
    test1Positive = "Malignant",
    test2 = "Pathologist2",
    test2Positive = "Malignant",
    test3 = "Pathologist3",
    test3Positive = "Malignant"
  )
  expect_no_error(result_3)
})
