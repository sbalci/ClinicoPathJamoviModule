# ═══════════════════════════════════════════════════════════
# Argument Combination Tests: enhancedROC
# ═══════════════════════════════════════════════════════════
#
# Tests all valid argument combinations and option interactions
# for the enhancedROC jamovi function

library(testthat)
library(ClinicoPath)

# Load test data
data(enhancedroc_biomarker, package = "ClinicoPath")
data(enhancedroc_comparative, package = "ClinicoPath")

test_that("enhancedROC performs Youden Index optimization", {
  result <- enhancedROC(
    data = enhancedroc_biomarker,
    outcome = "disease_status",
    positiveClass = "Disease",
    predictors = "biomarker1",
    youdenOptimization = TRUE
  )

  expect_s3_class(result, "enhancedROCClass")
})

test_that("enhancedROC evaluates custom cutoffs", {
  result <- enhancedROC(
    data = enhancedroc_biomarker,
    outcome = "disease_status",
    positiveClass = "Disease",
    predictors = "biomarker1",
    customCutoffs = "10, 15, 20, 25"
  )

  expect_s3_class(result, "enhancedROCClass")
})

test_that("enhancedROC applies sensitivity threshold", {
  result <- enhancedROC(
    data = enhancedroc_biomarker,
    outcome = "disease_status",
    positiveClass = "Disease",
    predictors = "biomarker1",
    sensitivityThreshold = 0.9
  )

  expect_s3_class(result, "enhancedROCClass")
})

test_that("enhancedROC applies specificity threshold", {
  result <- enhancedROC(
    data = enhancedroc_biomarker,
    outcome = "disease_status",
    positiveClass = "Disease",
    predictors = "biomarker1",
    specificityThreshold = 0.9
  )

  expect_s3_class(result, "enhancedROCClass")
})

test_that("enhancedROC uses different confidence levels", {
  # Test with 90% CI
  result1 <- enhancedROC(
    data = enhancedroc_biomarker,
    outcome = "disease_status",
    positiveClass = "Disease",
    predictors = "biomarker1",
    confidenceLevel = 90
  )
  expect_s3_class(result1, "enhancedROCClass")

  # Test with 99% CI
  result2 <- enhancedROC(
    data = enhancedroc_biomarker,
    outcome = "disease_status",
    positiveClass = "Disease",
    predictors = "biomarker1",
    confidenceLevel = 99
  )
  expect_s3_class(result2, "enhancedROCClass")
})

test_that("enhancedROC uses bootstrap confidence intervals", {
  result <- enhancedROC(
    data = enhancedroc_biomarker,
    outcome = "disease_status",
    positiveClass = "Disease",
    predictors = "biomarker1",
    useBootstrap = TRUE,
    bootstrapSamples = 500  # Reduced for testing speed
  )

  expect_s3_class(result, "enhancedROCClass")
})

test_that("enhancedROC handles different bootstrap methods", {
  # Test percentile method
  result1 <- enhancedROC(
    data = enhancedroc_biomarker,
    outcome = "disease_status",
    positiveClass = "Disease",
    predictors = "biomarker1",
    useBootstrap = TRUE,
    bootstrapSamples = 200,
    bootstrapMethod = "percentile"
  )
  expect_s3_class(result1, "enhancedROCClass")

  # Test BCa method
  result2 <- enhancedROC(
    data = enhancedroc_biomarker,
    outcome = "disease_status",
    positiveClass = "Disease",
    predictors = "biomarker1",
    useBootstrap = TRUE,
    bootstrapSamples = 200,
    bootstrapMethod = "bca"
  )
  expect_s3_class(result2, "enhancedROCClass")
})

test_that("enhancedROC performs comparative ROC analysis", {
  result <- enhancedROC(
    data = enhancedroc_comparative,
    outcome = "cancer_status",
    positiveClass = "Cancer",
    predictors = c("established_marker", "novel_marker1", "novel_marker2"),
    analysisType = "comparative"
  )

  expect_s3_class(result, "enhancedROCClass")
})

test_that("enhancedROC performs pairwise comparisons", {
  result <- enhancedROC(
    data = enhancedroc_comparative,
    outcome = "cancer_status",
    positiveClass = "Cancer",
    predictors = c("established_marker", "novel_marker1"),
    pairwiseComparisons = TRUE
  )

  expect_s3_class(result, "enhancedROCClass")
})

test_that("enhancedROC handles different comparison methods", {
  # DeLong method
  result1 <- enhancedROC(
    data = enhancedroc_comparative,
    outcome = "cancer_status",
    positiveClass = "Cancer",
    predictors = c("established_marker", "novel_marker1"),
    pairwiseComparisons = TRUE,
    comparisonMethod = "delong"
  )
  expect_s3_class(result1, "enhancedROCClass")

  # Bootstrap method
  result2 <- enhancedROC(
    data = enhancedroc_comparative,
    outcome = "cancer_status",
    positiveClass = "Cancer",
    predictors = c("established_marker", "novel_marker1"),
    pairwiseComparisons = TRUE,
    comparisonMethod = "bootstrap"
  )
  expect_s3_class(result2, "enhancedROCClass")
})

test_that("enhancedROC generates ROC curve plot", {
  result <- enhancedROC(
    data = enhancedroc_biomarker,
    outcome = "disease_status",
    positiveClass = "Disease",
    predictors = "biomarker1",
    rocCurve = TRUE
  )

  expect_s3_class(result, "enhancedROCClass")
})

test_that("enhancedROC displays AUC table", {
  result <- enhancedROC(
    data = enhancedroc_biomarker,
    outcome = "disease_status",
    positiveClass = "Disease",
    predictors = c("biomarker1", "biomarker2"),
    aucTable = TRUE
  )

  expect_s3_class(result, "enhancedROCClass")
})

test_that("enhancedROC displays cutoff analysis table", {
  result <- enhancedROC(
    data = enhancedroc_biomarker,
    outcome = "disease_status",
    positiveClass = "Disease",
    predictors = "biomarker1",
    cutoffTable = TRUE
  )

  expect_s3_class(result, "enhancedROCClass")
})

test_that("enhancedROC displays optimal cutoffs", {
  result <- enhancedROC(
    data = enhancedroc_biomarker,
    outcome = "disease_status",
    positiveClass = "Disease",
    predictors = "biomarker1",
    optimalCutoffs = TRUE,
    youdenOptimization = TRUE
  )

  expect_s3_class(result, "enhancedROCClass")
})

test_that("enhancedROC displays diagnostic metrics", {
  result <- enhancedROC(
    data = enhancedroc_biomarker,
    outcome = "disease_status",
    positiveClass = "Disease",
    predictors = "biomarker1",
    diagnosticMetrics = TRUE
  )

  expect_s3_class(result, "enhancedROCClass")
})

test_that("enhancedROC displays clinical metrics (PPV, NPV, LR)", {
  result <- enhancedROC(
    data = enhancedroc_biomarker,
    outcome = "disease_status",
    positiveClass = "Disease",
    predictors = "biomarker1",
    clinicalMetrics = TRUE,
    prevalence = 0.15
  )

  expect_s3_class(result, "enhancedROCClass")
})

test_that("enhancedROC uses observed prevalence", {
  result <- enhancedROC(
    data = enhancedroc_biomarker,
    outcome = "disease_status",
    positiveClass = "Disease",
    predictors = "biomarker1",
    clinicalMetrics = TRUE,
    useObservedPrevalence = TRUE
  )

  expect_s3_class(result, "enhancedROCClass")
})

test_that("enhancedROC performs partial AUC analysis", {
  result <- enhancedROC(
    data = enhancedroc_biomarker,
    outcome = "disease_status",
    positiveClass = "Disease",
    predictors = "biomarker1",
    partialAuc = TRUE,
    partialAucType = "specificity",
    partialRange = "0.8,1.0"
  )

  expect_s3_class(result, "enhancedROCClass")
})

test_that("enhancedROC handles different smoothing methods", {
  # No smoothing
  result1 <- enhancedROC(
    data = enhancedroc_biomarker,
    outcome = "disease_status",
    positiveClass = "Disease",
    predictors = "biomarker1",
    smoothMethod = "none"
  )
  expect_s3_class(result1, "enhancedROCClass")

  # Binormal smoothing
  result2 <- enhancedROC(
    data = enhancedroc_biomarker,
    outcome = "disease_status",
    positiveClass = "Disease",
    predictors = "biomarker1",
    smoothMethod = "binormal"
  )
  expect_s3_class(result2, "enhancedROCClass")
})

test_that("enhancedROC applies clinical context", {
  # Screening context
  result1 <- enhancedROC(
    data = enhancedroc_biomarker,
    outcome = "disease_status",
    positiveClass = "Disease",
    predictors = "biomarker1",
    clinicalContext = "screening"
  )
  expect_s3_class(result1, "enhancedROCClass")

  # Diagnosis context
  result2 <- enhancedROC(
    data = enhancedroc_biomarker,
    outcome = "disease_status",
    positiveClass = "Disease",
    predictors = "biomarker1",
    clinicalContext = "diagnosis"
  )
  expect_s3_class(result2, "enhancedROCClass")
})

test_that("enhancedROC applies clinical presets", {
  # Biomarker screening preset
  result1 <- enhancedROC(
    data = enhancedroc_biomarker,
    outcome = "disease_status",
    positiveClass = "Disease",
    predictors = "biomarker1",
    clinicalPresets = "biomarker_screening"
  )
  expect_s3_class(result1, "enhancedROCClass")

  # Confirmatory testing preset
  result2 <- enhancedROC(
    data = enhancedroc_biomarker,
    outcome = "disease_status",
    positiveClass = "Disease",
    predictors = "biomarker1",
    clinicalPresets = "confirmatory_testing"
  )
  expect_s3_class(result2, "enhancedROCClass")
})

test_that("enhancedROC handles different plot themes", {
  # Clinical theme
  result1 <- enhancedROC(
    data = enhancedroc_biomarker,
    outcome = "disease_status",
    positiveClass = "Disease",
    predictors = "biomarker1",
    rocCurve = TRUE,
    plotTheme = "clinical"
  )
  expect_s3_class(result1, "enhancedROCClass")

  # Modern theme
  result2 <- enhancedROC(
    data = enhancedroc_biomarker,
    outcome = "disease_status",
    positiveClass = "Disease",
    predictors = "biomarker1",
    rocCurve = TRUE,
    plotTheme = "modern"
  )
  expect_s3_class(result2, "enhancedROCClass")
})

test_that("enhancedROC shows cutoff points on plot", {
  result <- enhancedROC(
    data = enhancedroc_biomarker,
    outcome = "disease_status",
    positiveClass = "Disease",
    predictors = "biomarker1",
    rocCurve = TRUE,
    showCutoffPoints = TRUE,
    youdenOptimization = TRUE
  )

  expect_s3_class(result, "enhancedROCClass")
})

test_that("enhancedROC shows confidence bands", {
  result <- enhancedROC(
    data = enhancedroc_biomarker,
    outcome = "disease_status",
    positiveClass = "Disease",
    predictors = "biomarker1",
    rocCurve = TRUE,
    showConfidenceBands = TRUE,
    useBootstrap = TRUE,
    bootstrapSamples = 200
  )

  expect_s3_class(result, "enhancedROCClass")
})

test_that("enhancedROC detects class imbalance", {
  data(enhancedroc_imbalanced, package = "ClinicoPath")

  result <- enhancedROC(
    data = enhancedroc_imbalanced,
    outcome = "rare_disease",
    positiveClass = "Positive",
    predictors = "screening_marker",
    detectImbalance = TRUE,
    showImbalanceWarning = TRUE
  )

  expect_s3_class(result, "enhancedROCClass")
})

test_that("enhancedROC combines multiple features", {
  # Comprehensive analysis with many options
  result <- enhancedROC(
    data = enhancedroc_biomarker,
    outcome = "disease_status",
    positiveClass = "Disease",
    predictors = c("biomarker1", "biomarker2"),
    analysisType = "comparative",
    youdenOptimization = TRUE,
    customCutoffs = "15, 20, 25",
    rocCurve = TRUE,
    aucTable = TRUE,
    cutoffTable = TRUE,
    diagnosticMetrics = TRUE,
    clinicalMetrics = TRUE,
    useObservedPrevalence = TRUE,
    pairwiseComparisons = TRUE
  )

  expect_s3_class(result, "enhancedROCClass")
})
