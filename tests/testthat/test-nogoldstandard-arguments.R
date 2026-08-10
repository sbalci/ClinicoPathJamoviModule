# ═══════════════════════════════════════════════════════════
# Argument Tests: nogoldstandard
# ═══════════════════════════════════════════════════════════
library(testthat)
data(nogoldstandard_test, package = "ClinicoPath")
data(nogoldstandard_pathology, package = "ClinicoPath")
data(nogoldstandard_validation, package = "ClinicoPath")

test_that("nogoldstandard respects all analysis methods", {
  # nogoldstandard_test has only Test1/Test2. A two-class latent model over two binary tests
  # has 5 parameters against 3 degrees of freedom, so latent_class is not identified and
  # refuses rather than returning starting-value-dependent numbers (1.0.4).
  run <- function(method) nogoldstandard(
    data = nogoldstandard_test,
    test1 = "Test1", test1Positive = "Positive",
    test2 = "Test2", test2Positive = "Positive",
    method = method,
    test3Positive = NULL, test4Positive = NULL, test5Positive = NULL)

  for (method in c("composite", "all_positive", "any_positive", "bayesian"))
    expect_s3_class(run(method), "nogoldstandardResults")

  expect_error(run("latent_class"), "at least 3 tests")
})

test_that("nogoldstandard respects bootstrap parameter", {
  # Without bootstrap
  result_no_boot <- nogoldstandard(
    data = nogoldstandard_test,
    test1 = "Test1",
    test1Positive = "Positive",
    test2 = "Test2",
    test2Positive = "Positive",
    # 2 tests: latent_class (the default since 1.0.4) requires 3+
    method = "composite",
    bootstrap = FALSE,
    test3Positive = NULL,
    test4Positive = NULL,
    test5Positive = NULL
  )
  expect_s3_class(result_no_boot, "nogoldstandardResults")

  # With bootstrap (small nboot for testing)
  result_boot <- nogoldstandard(
    data = nogoldstandard_test,
    test1 = "Test1",
    test1Positive = "Positive",
    test2 = "Test2",
    test2Positive = "Positive",
    # 2 tests: latent_class (the default since 1.0.4) requires 3+
    method = "composite",
    bootstrap = TRUE,
    nboot = 100,
    test3Positive = NULL,
    test4Positive = NULL,
    test5Positive = NULL
  )
  expect_s3_class(result_boot, "nogoldstandardResults")
})

test_that("nogoldstandard respects nboot parameter", {
  # Small number
  result_small <- nogoldstandard(
    data = nogoldstandard_test,
    test1 = "Test1",
    test1Positive = "Positive",
    test2 = "Test2",
    test2Positive = "Positive",
    # 2 tests: latent_class (the default since 1.0.4) requires 3+
    method = "composite",
    bootstrap = TRUE,
    nboot = 100,
    test3Positive = NULL,
    test4Positive = NULL,
    test5Positive = NULL
  )
  expect_no_error(result_small)

  # Larger number
  result_large <- nogoldstandard(
    data = nogoldstandard_test,
    test1 = "Test1",
    test1Positive = "Positive",
    test2 = "Test2",
    test2Positive = "Positive",
    # 2 tests: latent_class (the default since 1.0.4) requires 3+
    method = "composite",
    bootstrap = TRUE,
    nboot = 500,
    test3Positive = NULL,
    test4Positive = NULL,
    test5Positive = NULL
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
    alpha = 0.05,
    test3Positive = NULL,
    test4Positive = NULL,
    test5Positive = NULL,
      # 2 tests: latent_class (the default since 1.0.4) requires 3+
      method = "composite"
    )
  expect_no_error(result_95)

  # 99% CI (alpha = 0.01)
  result_99 <- nogoldstandard(
    data = nogoldstandard_test,
    test1 = "Test1",
    test1Positive = "Positive",
    test2 = "Test2",
    test2Positive = "Positive",
    alpha = 0.01,
    test3Positive = NULL,
    test4Positive = NULL,
    test5Positive = NULL,
      # 2 tests: latent_class (the default since 1.0.4) requires 3+
      method = "composite"
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
    verbose = FALSE,
    test3Positive = NULL,
    test4Positive = NULL,
    test5Positive = NULL,
      # 2 tests: latent_class (the default since 1.0.4) requires 3+
      method = "composite"
    )
  expect_no_error(result_quiet)

  # Verbose on
  result_verbose <- nogoldstandard(
    data = nogoldstandard_test,
    test1 = "Test1",
    test1Positive = "Positive",
    test2 = "Test2",
    test2Positive = "Positive",
    verbose = TRUE,
    test3Positive = NULL,
    test4Positive = NULL,
    test5Positive = NULL,
      # 2 tests: latent_class (the default since 1.0.4) requires 3+
      method = "composite"
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
      clinicalPreset = preset,
      test3Positive = NULL,
      test4Positive = NULL,
      test5Positive = NULL,
      # 2 tests: latent_class (the default since 1.0.4) requires 3+
      method = "composite"
    )
    expect_s3_class(result, "nogoldstandardResults")
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
    clinicalPreset = "diagnostic_validation",
    test4Positive = NULL,
    test5Positive = NULL
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
    clinicalPreset = "pathology_agreement",
    test4Positive = NULL,
    test5Positive = NULL
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
    test2Positive = "Positive",
    test3Positive = NULL,
    test4Positive = NULL,
    test5Positive = NULL,
      # 2 tests: latent_class (the default since 1.0.4) requires 3+
      method = "composite"
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
    test3Positive = "Malignant",
    test4Positive = NULL,
    test5Positive = NULL
  )
  expect_no_error(result_3)
})
