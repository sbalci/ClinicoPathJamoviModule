# ═══════════════════════════════════════════════════════════
# Edge Case Tests: enhancedROC
# ═══════════════════════════════════════════════════════════
#
# Tests edge cases, error handling, and boundary conditions
# for the enhancedROC jamovi function

library(testthat)
library(ClinicoPath)

# Load test data
data(enhancedroc_biomarker, package = "ClinicoPath")

test_that("enhancedROC handles missing values in predictor", {
  test_data_na <- enhancedroc_biomarker
  test_data_na$biomarker1[1:10] <- NA

  # Should warn about missing data or handle gracefully
  expect_warning(
    enhancedROC(
      data = test_data_na,
      outcome = "disease_status",
      positiveClass = "Disease",
      predictors = "biomarker1"
    ),
    regexp = "missing|NA|removed",
    ignore.case = TRUE
  )
})

test_that("enhancedROC handles missing values in outcome", {
  test_data_na <- enhancedroc_biomarker
  test_data_na$disease_status[1:10] <- NA

  expect_warning(
    enhancedROC(
      data = test_data_na,
      outcome = "disease_status",
      positiveClass = "Disease",
      predictors = "biomarker1"
    ),
    regexp = "missing|NA|removed",
    ignore.case = TRUE
  )
})

test_that("enhancedROC handles all positive outcomes", {
  test_data_all_pos <- enhancedroc_biomarker
  test_data_all_pos$disease_status <- factor(rep("Disease", nrow(enhancedroc_biomarker)))

  # Should error about no variation in outcome
  expect_error(
    enhancedROC(
      data = test_data_all_pos,
      outcome = "disease_status",
      positiveClass = "Disease",
      predictors = "biomarker1"
    ),
    regexp = "variation|constant|all.*same|binary",
    ignore.case = TRUE
  )
})

test_that("enhancedROC handles all negative outcomes", {
  test_data_all_neg <- enhancedroc_biomarker
  test_data_all_neg$disease_status <- factor(rep("Healthy", nrow(enhancedroc_biomarker)))

  expect_error(
    enhancedROC(
      data = test_data_all_neg,
      outcome = "disease_status",
      positiveClass = "Disease",
      predictors = "biomarker1"
    ),
    regexp = "variation|constant|all.*same|binary",
    ignore.case = TRUE
  )
})

test_that("enhancedROC handles constant predictor", {
  test_data_const <- enhancedroc_biomarker
  test_data_const$constant_marker <- 10  # All same value

  # Should error or warn about no variation
  expect_error(
    enhancedROC(
      data = test_data_const,
      outcome = "disease_status",
      positiveClass = "Disease",
      predictors = "constant_marker"
    ),
    regexp = "variation|constant|no.*variability",
    ignore.case = TRUE
  )
})

test_that("enhancedROC handles very small sample size", {
  small_data <- enhancedroc_biomarker[1:15, ]

  # Should warn about small sample
  result <- enhancedROC(
    data = small_data,
    outcome = "disease_status",
    positiveClass = "Disease",
    predictors = "biomarker1"
  )

  expect_s3_class(result, "enhancedROCClass")
})

test_that("enhancedROC handles single observation per class", {
  # Create minimal dataset with only 2 observations
  minimal_data <- enhancedroc_biomarker[c(1, 100), ]

  # Should error with informative message
  expect_error(
    enhancedROC(
      data = minimal_data,
      outcome = "disease_status",
      positiveClass = "Disease",
      predictors = "biomarker1"
    ),
    regexp = "insufficient|not enough|too few|sample size",
    ignore.case = TRUE
  )
})

test_that("enhancedROC handles extreme class imbalance", {
  data(enhancedroc_imbalanced, package = "ClinicoPath")

  # 5% prevalence should still work but may warn
  result <- enhancedROC(
    data = enhancedroc_imbalanced,
    outcome = "rare_disease",
    positiveClass = "Positive",
    predictors = "screening_marker"
  )

  expect_s3_class(result, "enhancedROCClass")
})

test_that("enhancedROC handles negative predictor values", {
  test_data_neg <- enhancedroc_biomarker
  test_data_neg$biomarker1 <- test_data_neg$biomarker1 - 20  # Make some values negative

  result <- enhancedROC(
    data = test_data_neg,
    outcome = "disease_status",
    positiveClass = "Disease",
    predictors = "biomarker1"
  )

  expect_s3_class(result, "enhancedROCClass")
})

test_that("enhancedROC handles very large predictor values", {
  test_data_large <- enhancedroc_biomarker
  test_data_large$biomarker1 <- test_data_large$biomarker1 * 1000

  result <- enhancedROC(
    data = test_data_large,
    outcome = "disease_status",
    positiveClass = "Disease",
    predictors = "biomarker1"
  )

  expect_s3_class(result, "enhancedROCClass")
})

test_that("enhancedROC handles tied predictor values", {
  test_data_ties <- enhancedroc_biomarker
  test_data_ties$biomarker1 <- round(test_data_ties$biomarker1)  # Create many ties

  result <- enhancedROC(
    data = test_data_ties,
    outcome = "disease_status",
    positiveClass = "Disease",
    predictors = "biomarker1"
  )

  expect_s3_class(result, "enhancedROCClass")
})

test_that("enhancedROC handles perfect separation", {
  # Create perfect separation scenario
  test_data_perfect <- enhancedroc_biomarker
  test_data_perfect$perfect_marker <- ifelse(
    test_data_perfect$disease_status == "Disease", 100, 0
  )

  result <- enhancedROC(
    data = test_data_perfect,
    outcome = "disease_status",
    positiveClass = "Disease",
    predictors = "perfect_marker"
  )

  # Should complete (AUC = 1.0)
  expect_s3_class(result, "enhancedROCClass")
})

test_that("enhancedROC handles no discrimination (AUC ~ 0.5)", {
  # Create random predictor with no discrimination
  set.seed(123)
  test_data_random <- enhancedroc_biomarker
  test_data_random$random_marker <- rnorm(nrow(enhancedroc_biomarker))

  result <- enhancedROC(
    data = test_data_random,
    outcome = "disease_status",
    positiveClass = "Disease",
    predictors = "random_marker"
  )

  expect_s3_class(result, "enhancedROCClass")
})

test_that("enhancedROC handles invalid cutoff range", {
  # Cutoffs outside predictor range
  result <- enhancedROC(
    data = enhancedroc_biomarker,
    outcome = "disease_status",
    positiveClass = "Disease",
    predictors = "biomarker1",
    customCutoffs = "100, 200, 300"  # Way above actual range
  )

  # Should complete but may warn
  expect_s3_class(result, "enhancedROCClass")
})

test_that("enhancedROC handles extreme prevalence values", {
  # Very low prevalence
  result1 <- enhancedROC(
    data = enhancedroc_biomarker,
    outcome = "disease_status",
    positiveClass = "Disease",
    predictors = "biomarker1",
    clinicalMetrics = TRUE,
    prevalence = 0.001  # 0.1% prevalence
  )
  expect_s3_class(result1, "enhancedROCClass")

  # Very high prevalence
  result2 <- enhancedROC(
    data = enhancedroc_biomarker,
    outcome = "disease_status",
    positiveClass = "Disease",
    predictors = "biomarker1",
    clinicalMetrics = TRUE,
    prevalence = 0.95  # 95% prevalence
  )
  expect_s3_class(result2, "enhancedROCClass")
})

test_that("enhancedROC handles extreme sensitivity threshold", {
  # Very high sensitivity requirement
  result <- enhancedROC(
    data = enhancedroc_biomarker,
    outcome = "disease_status",
    positiveClass = "Disease",
    predictors = "biomarker1",
    sensitivityThreshold = 0.99
  )

  expect_s3_class(result, "enhancedROCClass")
})

test_that("enhancedROC handles extreme specificity threshold", {
  # Very high specificity requirement
  result <- enhancedROC(
    data = enhancedroc_biomarker,
    outcome = "disease_status",
    positiveClass = "Disease",
    predictors = "biomarker1",
    specificityThreshold = 0.99
  )

  expect_s3_class(result, "enhancedROCClass")
})

test_that("enhancedROC handles wrong positive class specification", {
  # Specify non-existent level
  expect_error(
    enhancedROC(
      data = enhancedroc_biomarker,
      outcome = "disease_status",
      positiveClass = "InvalidLevel",
      predictors = "biomarker1"
    ),
    regexp = "level.*not found|invalid.*class|positive.*class",
    ignore.case = TRUE
  )
})

test_that("enhancedROC handles duplicate observations", {
  # Create exact duplicates
  test_data_dup <- rbind(enhancedroc_biomarker, enhancedroc_biomarker[1:20, ])

  result <- enhancedROC(
    data = test_data_dup,
    outcome = "disease_status",
    positiveClass = "Disease",
    predictors = "biomarker1"
  )

  expect_s3_class(result, "enhancedROCClass")
})

test_that("enhancedROC handles partial AUC with invalid range", {
  # Range outside [0,1]
  expect_error(
    enhancedROC(
      data = enhancedroc_biomarker,
      outcome = "disease_status",
      positiveClass = "Disease",
      predictors = "biomarker1",
      partialAuc = TRUE,
      partialRange = "0.5,1.5"  # Invalid (> 1)
    ),
    regexp = "range|invalid|between 0 and 1",
    ignore.case = TRUE
  )
})

test_that("enhancedROC handles very small bootstrap samples", {
  # Minimum bootstrap samples
  result <- enhancedROC(
    data = enhancedroc_biomarker,
    outcome = "disease_status",
    positiveClass = "Disease",
    predictors = "biomarker1",
    useBootstrap = TRUE,
    bootstrapSamples = 100  # Minimum allowed
  )

  expect_s3_class(result, "enhancedROCClass")
})

test_that("enhancedROC handles multiclass outcome with binary analysis", {
  data(enhancedroc_multiclass, package = "ClinicoPath")

  # Try to use multiclass data without multiClassROC enabled
  # Should error or convert to binary
  expect_condition(
    enhancedROC(
      data = enhancedroc_multiclass,
      outcome = "disease_severity",
      positiveClass = "Severe",
      predictors = "biomarker_A"
    )
  )
})

test_that("enhancedROC handles extremely unbalanced groups in comparison", {
  # Create data where one predictor has perfect discrimination
  test_data_unbal <- enhancedroc_biomarker
  test_data_unbal$perfect_marker <- ifelse(
    test_data_unbal$disease_status == "Disease", 100, 0
  )

  result <- enhancedROC(
    data = test_data_unbal,
    outcome = "disease_status",
    positiveClass = "Disease",
    predictors = c("biomarker1", "perfect_marker"),
    pairwiseComparisons = TRUE
  )

  expect_s3_class(result, "enhancedROCClass")
})

test_that("enhancedROC handles predictor with single unique value per class", {
  # Extreme case of separation
  test_data_extreme <- enhancedroc_biomarker
  test_data_extreme$extreme_marker <- ifelse(
    test_data_extreme$disease_status == "Disease", 50, 10
  )

  result <- enhancedROC(
    data = test_data_extreme,
    outcome = "disease_status",
    positiveClass = "Disease",
    predictors = "extreme_marker"
  )

  expect_s3_class(result, "enhancedROCClass")
})

test_that("enhancedROC handles conflicting direction and actual data", {
  # Specify direction="higher" when actually lower values indicate disease
  # (Deliberately wrong direction)
  test_data_inverse <- enhancedroc_biomarker
  test_data_inverse$inverse_marker <- -test_data_inverse$biomarker1

  result <- enhancedROC(
    data = test_data_inverse,
    outcome = "disease_status",
    positiveClass = "Disease",
    predictors = "inverse_marker",
    direction = "higher"  # Wrong direction
  )

  # Should complete but with poor AUC
  expect_s3_class(result, "enhancedROCClass")
})
