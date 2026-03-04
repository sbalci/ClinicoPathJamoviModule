enhancedROC <- function(...) {
  args <- list(...)
  f_args <- formals(ClinicoPath::enhancedROC)
  for(arg in names(f_args)) {
    if(arg %in% c('...', 'data')) next
    if(!(arg %in% names(args))) {
      if(is.name(f_args[[arg]]) && as.character(f_args[[arg]]) == '') {
        args[[arg]] <- ""
      }
    }
  }
  do.call(ClinicoPath::enhancedROC, args)
}

# ═══════════════════════════════════════════════════════════
# Edge Case Tests: enhancedROC
# ═══════════════════════════════════════════════════════════
#
# Tests edge cases, error handling, and boundary conditions
# for the enhancedROC jamovi function

library(testthat)

# Load test data
data(enhancedroc_biomarker, package = "ClinicoPath")

test_that("enhancedROC handles missing values in predictor", {
  test_data_na <- enhancedroc_biomarker
  test_data_na$biomarker1[1:10] <- NA

  result <- suppressWarnings(
    enhancedROC(
      data = test_data_na,
      outcome = "disease_status",
      positiveClass = "Disease",
      predictors = "biomarker1"
    )
  )
  expect_s3_class(result, "enhancedROCResults")
})

test_that("enhancedROC handles missing values in outcome", {
  test_data_na <- enhancedroc_biomarker
  test_data_na$disease_status[1:10] <- NA

  result <- suppressWarnings(
    enhancedROC(
      data = test_data_na,
      outcome = "disease_status",
      positiveClass = "Disease",
      predictors = "biomarker1"
    )
  )
  expect_s3_class(result, "enhancedROCResults")
})

test_that("enhancedROC handles all positive outcomes", {
  test_data_all_pos <- enhancedroc_biomarker
  test_data_all_pos$disease_status <- factor(rep("Disease", nrow(enhancedroc_biomarker)))

  result <- suppressWarnings(
    enhancedROC(
      data = test_data_all_pos,
      outcome = "disease_status",
      positiveClass = "Disease",
      predictors = "biomarker1"
    )
  )
  expect_s3_class(result, "enhancedROCResults")
})

test_that("enhancedROC handles all negative outcomes", {
  test_data_all_neg <- enhancedroc_biomarker
  test_data_all_neg$disease_status <- factor(rep("Healthy", nrow(enhancedroc_biomarker)))

  result <- suppressWarnings(
    enhancedROC(
      data = test_data_all_neg,
      outcome = "disease_status",
      positiveClass = "Disease",
      predictors = "biomarker1"
    )
  )
  expect_s3_class(result, "enhancedROCResults")
})

test_that("enhancedROC handles constant predictor", {
  test_data_const <- enhancedroc_biomarker
  test_data_const$constant_marker <- 10  # All same value

  result <- suppressWarnings(
    enhancedROC(
      data = test_data_const,
      outcome = "disease_status",
      positiveClass = "Disease",
      predictors = "constant_marker"
    )
  )
  expect_s3_class(result, "enhancedROCResults")
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

  expect_s3_class(result, "enhancedROCResults")
})

test_that("enhancedROC handles single observation per class", {
  # Create minimal dataset with only 2 observations
  minimal_data <- enhancedroc_biomarker[c(1, 100), ]

  result <- suppressWarnings(
    enhancedROC(
      data = minimal_data,
      outcome = "disease_status",
      positiveClass = "Disease",
      predictors = "biomarker1"
    )
  )
  expect_s3_class(result, "enhancedROCResults")
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

  expect_s3_class(result, "enhancedROCResults")
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

  expect_s3_class(result, "enhancedROCResults")
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

  expect_s3_class(result, "enhancedROCResults")
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

  expect_s3_class(result, "enhancedROCResults")
})

test_that("enhancedROC handles perfect separation", {
  # Create perfect separation scenario
  test_data_perfect <- enhancedroc_biomarker
  test_data_perfect$perfect_marker <- ifelse(
    test_data_perfect$disease_status == "Disease", 100, 0
  )

  result <- suppressWarnings(
    enhancedROC(
      data = test_data_perfect,
      outcome = "disease_status",
      positiveClass = "Disease",
      predictors = "perfect_marker"
    )
  )

  # Should complete (AUC = 1.0)
  expect_s3_class(result, "enhancedROCResults")
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

  expect_s3_class(result, "enhancedROCResults")
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
  expect_s3_class(result, "enhancedROCResults")
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
  expect_s3_class(result1, "enhancedROCResults")

  # Very high prevalence
  result2 <- enhancedROC(
    data = enhancedroc_biomarker,
    outcome = "disease_status",
    positiveClass = "Disease",
    predictors = "biomarker1",
    clinicalMetrics = TRUE,
    prevalence = 0.95  # 95% prevalence
  )
  expect_s3_class(result2, "enhancedROCResults")
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

  expect_s3_class(result, "enhancedROCResults")
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

  expect_s3_class(result, "enhancedROCResults")
})

test_that("enhancedROC handles wrong positive class specification", {
  # Specify non-existent level
  result <- suppressWarnings(
    enhancedROC(
      data = enhancedroc_biomarker,
      outcome = "disease_status",
      positiveClass = "InvalidLevel",
      predictors = "biomarker1"
    )
  )
  expect_s3_class(result, "enhancedROCResults")
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

  expect_s3_class(result, "enhancedROCResults")
})

test_that("enhancedROC handles partial AUC with invalid range", {
  # Range outside [0,1]
  result <- suppressWarnings(
    enhancedROC(
      data = enhancedroc_biomarker,
      outcome = "disease_status",
      positiveClass = "Disease",
      predictors = "biomarker1",
      partialAuc = TRUE,
      partialRange = "0.5,1.5"  # Invalid (> 1)
    )
  )
  expect_s3_class(result, "enhancedROCResults")
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

  expect_s3_class(result, "enhancedROCResults")
})

test_that("enhancedROC handles multiclass outcome with binary analysis", {
  data(enhancedroc_multiclass, package = "ClinicoPath")

  # Try to use multiclass data without multiClassROC enabled
  # Should error or convert to binary
  result <- suppressWarnings(
    enhancedROC(
      data = enhancedroc_multiclass,
      outcome = "disease_severity",
      positiveClass = "Severe",
      predictors = "biomarker_A"
    )
  )
  expect_s3_class(result, "enhancedROCResults")
})

test_that("enhancedROC handles extremely unbalanced groups in comparison", {
  # Create data where one predictor has perfect discrimination
  test_data_unbal <- enhancedroc_biomarker
  test_data_unbal$perfect_marker <- ifelse(
    test_data_unbal$disease_status == "Disease", 100, 0
  )

  result <- suppressWarnings(
    enhancedROC(
      data = test_data_unbal,
      outcome = "disease_status",
      positiveClass = "Disease",
      predictors = c("biomarker1", "perfect_marker"),
      pairwiseComparisons = TRUE
    )
  )

  expect_s3_class(result, "enhancedROCResults")
})

test_that("enhancedROC handles predictor with single unique value per class", {
  # Extreme case of separation
  test_data_extreme <- enhancedroc_biomarker
  test_data_extreme$extreme_marker <- ifelse(
    test_data_extreme$disease_status == "Disease", 50, 10
  )

  result <- suppressWarnings(
    enhancedROC(
      data = test_data_extreme,
      outcome = "disease_status",
      positiveClass = "Disease",
      predictors = "extreme_marker"
    )
  )

  expect_s3_class(result, "enhancedROCResults")
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
  expect_s3_class(result, "enhancedROCResults")
})
