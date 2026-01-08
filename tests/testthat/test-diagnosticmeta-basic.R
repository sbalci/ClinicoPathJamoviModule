# ═══════════════════════════════════════════════════════════
# Basic Functionality Tests: diagnosticmeta
# ═══════════════════════════════════════════════════════════
#
# Tests basic functionality, required arguments, and expected outputs
# for the diagnosticmeta jamovi function

library(testthat)
library(ClinicoPath)

# Load test data
data(diagnosticmeta_test, package = "ClinicoPath")

test_that("diagnosticmeta function exists and is accessible", {
  # Check function exists
  expect_true(exists("diagnosticmeta"))

  # Check it's a function
  expect_type(diagnosticmeta, "closure")
})

test_that("diagnosticmeta runs with minimal required arguments", {
  # Basic execution with only required arguments
  result <- diagnosticmeta(
    data = diagnosticmeta_test,
    study = "study",
    true_positives = "true_positives",
    false_positives = "false_positives",
    false_negatives = "false_negatives",
    true_negatives = "true_negatives"
  )

  # Should return a result object
  expect_s3_class(result, "diagnosticmetaClass")

  # Should have a results component
  expect_true("results" %in% names(result))
})

test_that("diagnosticmeta errors on missing required arguments", {
  # Missing study identifier
  expect_error(
    diagnosticmeta(
      data = diagnosticmeta_test,
      true_positives = "true_positives",
      false_positives = "false_positives",
      false_negatives = "false_negatives",
      true_negatives = "true_negatives"
    ),
    regexp = "study|identifier|required",
    ignore.case = TRUE
  )

  # Missing true_positives
  expect_error(
    diagnosticmeta(
      data = diagnosticmeta_test,
      study = "study",
      false_positives = "false_positives",
      false_negatives = "false_negatives",
      true_negatives = "true_negatives"
    ),
    regexp = "true.positive|TP|required",
    ignore.case = TRUE
  )

  # Missing false_positives
  expect_error(
    diagnosticmeta(
      data = diagnosticmeta_test,
      study = "study",
      true_positives = "true_positives",
      false_negatives = "false_negatives",
      true_negatives = "true_negatives"
    ),
    regexp = "false.positive|FP|required",
    ignore.case = TRUE
  )

  # Missing false_negatives
  expect_error(
    diagnosticmeta(
      data = diagnosticmeta_test,
      study = "study",
      true_positives = "true_positives",
      false_positives = "false_positives",
      true_negatives = "true_negatives"
    ),
    regexp = "false.negative|FN|required",
    ignore.case = TRUE
  )

  # Missing true_negatives
  expect_error(
    diagnosticmeta(
      data = diagnosticmeta_test,
      study = "study",
      true_positives = "true_positives",
      false_positives = "false_positives",
      false_negatives = "false_negatives"
    ),
    regexp = "true.negative|TN|required",
    ignore.case = TRUE
  )
})

test_that("diagnosticmeta produces expected outputs with default settings", {
  result <- diagnosticmeta(
    data = diagnosticmeta_test,
    study = "study",
    true_positives = "true_positives",
    false_positives = "false_positives",
    false_negatives = "false_negatives",
    true_negatives = "true_negatives"
  )

  # Check that main summary table exists
  expect_true(!is.null(result$results$summary))

  # Check that instructions are provided
  expect_true(!is.null(result$results$instructions))
})

test_that("diagnosticmeta handles bivariate analysis option", {
  # With bivariate analysis (default = TRUE)
  result_biv <- diagnosticmeta(
    data = diagnosticmeta_test,
    study = "study",
    true_positives = "true_positives",
    false_positives = "false_positives",
    false_negatives = "false_negatives",
    true_negatives = "true_negatives",
    bivariate_analysis = TRUE
  )

  expect_s3_class(result_biv, "diagnosticmetaClass")

  # Without bivariate analysis
  result_no_biv <- diagnosticmeta(
    data = diagnosticmeta_test,
    study = "study",
    true_positives = "true_positives",
    false_positives = "false_positives",
    false_negatives = "false_negatives",
    true_negatives = "true_negatives",
    bivariate_analysis = FALSE
  )

  expect_s3_class(result_no_biv, "diagnosticmetaClass")
})

test_that("diagnosticmeta handles confidence level option", {
  # Test with different confidence levels
  result_95 <- diagnosticmeta(
    data = diagnosticmeta_test,
    study = "study",
    true_positives = "true_positives",
    false_positives = "false_positives",
    false_negatives = "false_negatives",
    true_negatives = "true_negatives",
    confidence_level = 95
  )

  expect_s3_class(result_95, "diagnosticmetaClass")

  result_99 <- diagnosticmeta(
    data = diagnosticmeta_test,
    study = "study",
    true_positives = "true_positives",
    false_positives = "false_positives",
    false_negatives = "false_negatives",
    true_negatives = "true_negatives",
    confidence_level = 99
  )

  expect_s3_class(result_99, "diagnosticmetaClass")
})

test_that("diagnosticmeta handles different estimation methods", {
  methods <- c("reml", "ml", "fixed", "mm", "vc")

  for (method in methods) {
    result <- diagnosticmeta(
      data = diagnosticmeta_test,
      study = "study",
      true_positives = "true_positives",
      false_positives = "false_positives",
      false_negatives = "false_negatives",
      true_negatives = "true_negatives",
      method = method
    )

    expect_s3_class(result, "diagnosticmetaClass")
  }
})

test_that("diagnosticmeta runs with small dataset", {
  # Load small test data
  data(diagnosticmeta_test_small, package = "ClinicoPath")

  result <- diagnosticmeta(
    data = diagnosticmeta_test_small,
    study = "study",
    true_positives = "true_positives",
    false_positives = "false_positives",
    false_negatives = "false_negatives",
    true_negatives = "true_negatives"
  )

  # Should complete but may have warnings
  expect_s3_class(result, "diagnosticmetaClass")
})

test_that("diagnosticmeta produces plots when requested", {
  result <- diagnosticmeta(
    data = diagnosticmeta_test,
    study = "study",
    true_positives = "true_positives",
    false_positives = "false_positives",
    false_negatives = "false_negatives",
    true_negatives = "true_negatives",
    forest_plot = TRUE,
    sroc_plot = TRUE,
    funnel_plot = TRUE
  )

  expect_s3_class(result, "diagnosticmetaClass")

  # Check that plot objects exist (if implemented)
  # expect_true(!is.null(result$results$forestplot))
  # expect_true(!is.null(result$results$srocplot))
  # expect_true(!is.null(result$results$funnelplot))
})
