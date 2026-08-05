# ═══════════════════════════════════════════════════════════
# Basic Functionality Tests: diagnosticmeta
# ═══════════════════════════════════════════════════════════
#
# Tests basic functionality, required arguments, and expected outputs
# for the diagnosticmeta jamovi function

library(testthat)

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
  expect_s3_class(result, "diagnosticmetaResults")

  # Should have a results component
  # The public wrapper returns the results object itself (class
  # diagnosticmetaResults); there is no nested `results` element.
  expect_true(!is.null(result$bivariateresults))
})

test_that("omitting a required variable shows guidance instead of throwing", {
  # A jamovi analysis must not error when the user has not yet chosen variables -
  # the GUI calls .run() on every keystroke. These assertions previously demanded
  # an error, i.e. they asserted the opposite of the correct behaviour, and were
  # red because the analysis handles it properly.
  partials <- list(
    list(true_positives = "true_positives", false_positives = "false_positives",
         false_negatives = "false_negatives", true_negatives = "true_negatives"),
    list(study = "study", false_positives = "false_positives",
         false_negatives = "false_negatives", true_negatives = "true_negatives"),
    list(study = "study", true_positives = "true_positives",
         false_negatives = "false_negatives", true_negatives = "true_negatives"),
    list(study = "study", true_positives = "true_positives",
         false_positives = "false_positives", true_negatives = "true_negatives"),
    list(study = "study", true_positives = "true_positives",
         false_positives = "false_positives", false_negatives = "false_negatives")
  )
  for (args in partials) {
    res <- do.call(diagnosticmeta, c(list(data = diagnosticmeta_test), args))
    expect_s3_class(res, "diagnosticmetaResults")
    # guidance is shown rather than an exception raised
    expect_true(nzchar(res$instructions$content))
  }
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
  expect_true(!is.null(result$summary))

  # Check that instructions are provided
  expect_true(!is.null(result$instructions))
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

  expect_s3_class(result_biv, "diagnosticmetaResults")

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

  expect_s3_class(result_no_biv, "diagnosticmetaResults")
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

  expect_s3_class(result_95, "diagnosticmetaResults")

  result_99 <- diagnosticmeta(
    data = diagnosticmeta_test,
    study = "study",
    true_positives = "true_positives",
    false_positives = "false_positives",
    false_negatives = "false_negatives",
    true_negatives = "true_negatives",
    confidence_level = 99
  )

  expect_s3_class(result_99, "diagnosticmetaResults")
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

    expect_s3_class(result, "diagnosticmetaResults")
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
  expect_s3_class(result, "diagnosticmetaResults")
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

  expect_s3_class(result, "diagnosticmetaResults")

  # Check that plot objects exist (if implemented)
  # expect_true(!is.null(result$results$forestplot))
  # expect_true(!is.null(result$results$srocplot))
  # expect_true(!is.null(result$results$funnelplot))
})
