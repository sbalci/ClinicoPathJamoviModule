# ═══════════════════════════════════════════════════════════
# Basic Functionality Tests: enhancedROC
# ═══════════════════════════════════════════════════════════
#
# Tests basic functionality and required arguments
# for the enhancedROC jamovi function

library(testthat)
library(ClinicoPath)

# Load test data
data(enhancedroc_biomarker, package = "ClinicoPath")

test_that("enhancedROC function exists", {
  expect_true(exists("enhancedROC"))
  expect_true(is.function(enhancedROC))
})

test_that("enhancedROC runs with minimal required arguments", {
  result <- enhancedROC(
    data = enhancedroc_biomarker,
    outcome = "disease_status",
    positiveClass = "Disease",
    predictors = "biomarker1"
  )

  expect_s3_class(result, "enhancedROCClass")
  expect_true("results" %in% names(result))
})

test_that("enhancedROC handles single predictor", {
  result <- enhancedROC(
    data = enhancedroc_biomarker,
    outcome = "disease_status",
    positiveClass = "Disease",
    predictors = "biomarker1",
    analysisType = "single"
  )

  expect_s3_class(result, "enhancedROCClass")
})

test_that("enhancedROC handles multiple predictors", {
  result <- enhancedROC(
    data = enhancedroc_biomarker,
    outcome = "disease_status",
    positiveClass = "Disease",
    predictors = c("biomarker1", "biomarker2", "biomarker3")
  )

  expect_s3_class(result, "enhancedROCClass")
})

test_that("enhancedROC errors on missing outcome", {
  expect_error(
    enhancedROC(
      data = enhancedroc_biomarker,
      predictors = "biomarker1"
    ),
    regexp = "outcome.*required|missing",
    ignore.case = TRUE
  )
})

test_that("enhancedROC errors on missing predictors", {
  expect_error(
    enhancedROC(
      data = enhancedroc_biomarker,
      outcome = "disease_status",
      positiveClass = "Disease"
    ),
    regexp = "predictor.*required|missing",
    ignore.case = TRUE
  )
})

test_that("enhancedROC handles binary outcome correctly", {
  result <- enhancedROC(
    data = enhancedroc_biomarker,
    outcome = "disease_status",
    positiveClass = "Disease",
    predictors = "biomarker1"
  )

  expect_s3_class(result, "enhancedROCClass")
})

test_that("enhancedROC handles continuous predictor", {
  result <- enhancedROC(
    data = enhancedroc_biomarker,
    outcome = "disease_status",
    positiveClass = "Disease",
    predictors = "biomarker1"
  )

  expect_s3_class(result, "enhancedROCClass")
})

test_that("enhancedROC produces expected output structure", {
  result <- enhancedROC(
    data = enhancedroc_biomarker,
    outcome = "disease_status",
    positiveClass = "Disease",
    predictors = "biomarker1"
  )

  # Check that result has results component
  expect_true("results" %in% names(result))

  # Results should be a list
  expect_type(result$results, "list")
})

test_that("enhancedROC handles auto direction detection", {
  result <- enhancedROC(
    data = enhancedroc_biomarker,
    outcome = "disease_status",
    positiveClass = "Disease",
    predictors = "biomarker1",
    direction = "auto"
  )

  expect_s3_class(result, "enhancedROCClass")
})

test_that("enhancedROC handles specified direction (higher)", {
  result <- enhancedROC(
    data = enhancedroc_biomarker,
    outcome = "disease_status",
    positiveClass = "Disease",
    predictors = "biomarker1",
    direction = "higher"
  )

  expect_s3_class(result, "enhancedROCClass")
})

test_that("enhancedROC handles specified direction (lower)", {
  result <- enhancedROC(
    data = enhancedroc_biomarker,
    outcome = "disease_status",
    positiveClass = "Disease",
    predictors = "biomarker1",
    direction = "lower"
  )

  expect_s3_class(result, "enhancedROCClass")
})

test_that("enhancedROC handles small dataset", {
  data(enhancedroc_small, package = "ClinicoPath")

  result <- enhancedROC(
    data = enhancedroc_small,
    outcome = "disease",
    positiveClass = "Positive",
    predictors = "marker"
  )

  expect_s3_class(result, "enhancedROCClass")
})

test_that("enhancedROC accepts default options", {
  # Test with all default options
  result <- enhancedROC(
    data = enhancedroc_biomarker,
    outcome = "disease_status",
    positiveClass = "Disease",
    predictors = "biomarker1"
  )

  expect_s3_class(result, "enhancedROCClass")
  expect_no_error(result)
})

test_that("enhancedROC handles different positive class selections", {
  # Test with "Healthy" as positive class
  result <- enhancedROC(
    data = enhancedroc_biomarker,
    outcome = "disease_status",
    positiveClass = "Healthy",
    predictors = "biomarker1"
  )

  expect_s3_class(result, "enhancedROCClass")
})

test_that("enhancedROC handles risk score predictor", {
  result <- enhancedROC(
    data = enhancedroc_biomarker,
    outcome = "disease_status",
    positiveClass = "Disease",
    predictors = "clinical_risk_score"
  )

  expect_s3_class(result, "enhancedROCClass")
})
