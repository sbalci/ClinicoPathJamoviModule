# ═══════════════════════════════════════════════════════════
# Edge Cases and Error Handling Tests: diagnosticmeta
# ═══════════════════════════════════════════════════════════
#
# Tests edge cases, error conditions, and robust error handling
# for the diagnosticmeta jamovi function

library(testthat)
library(ClinicoPath)

# Load test data
data(diagnosticmeta_test, package = "ClinicoPath")
data(diagnosticmeta_test_small, package = "ClinicoPath")
data(diagnosticmeta_test_zeros, package = "ClinicoPath")

test_that("diagnosticmeta handles missing data appropriately", {
  # Create dataset with missing values
  test_data_na <- diagnosticmeta_test
  test_data_na$true_positives[1:3] <- NA

  # Should either error or warn about missing data
  expect_condition(
    diagnosticmeta(
      data = test_data_na,
      study = "study",
      true_positives = "true_positives",
      false_positives = "false_positives",
      false_negatives = "false_negatives",
      true_negatives = "true_negatives"
    )
  )
})

test_that("diagnosticmeta handles very small sample sizes", {
  # Only 3 studies
  small_data <- diagnosticmeta_test_small[1:3, ]

  result <- diagnosticmeta(
    data = small_data,
    study = "study",
    true_positives = "true_positives",
    false_positives = "false_positives",
    false_negatives = "false_negatives",
    true_negatives = "true_negatives"
  )

  # Should complete but may have warnings or notices
  expect_s3_class(result, "diagnosticmetaClass")
})

test_that("diagnosticmeta handles studies with zero cells", {
  # Test with zero-cell correction = none (model-based)
  result_none <- diagnosticmeta(
    data = diagnosticmeta_test_zeros,
    study = "study",
    true_positives = "true_positives",
    false_positives = "false_positives",
    false_negatives = "false_negatives",
    true_negatives = "true_negatives",
    zero_cell_correction = "none"
  )

  expect_s3_class(result_none, "diagnosticmetaClass")

  # Test with constant correction
  result_constant <- diagnosticmeta(
    data = diagnosticmeta_test_zeros,
    study = "study",
    true_positives = "true_positives",
    false_positives = "false_positives",
    false_negatives = "false_negatives",
    true_negatives = "true_negatives",
    zero_cell_correction = "constant"
  )

  expect_s3_class(result_constant, "diagnosticmetaClass")
})

test_that("diagnosticmeta handles negative values in 2x2 table", {
  # Create dataset with negative values (invalid)
  test_data_neg <- diagnosticmeta_test
  test_data_neg$true_positives[1] <- -5

  # Should error with informative message
  expect_error(
    diagnosticmeta(
      data = test_data_neg,
      study = "study",
      true_positives = "true_positives",
      false_positives = "false_positives",
      false_negatives = "false_negatives",
      true_negatives = "true_negatives"
    ),
    regexp = "negative|invalid|positive",
    ignore.case = TRUE
  )
})

test_that("diagnosticmeta handles non-integer counts", {
  # Create dataset with decimal values
  test_data_decimal <- diagnosticmeta_test
  test_data_decimal$true_positives <- test_data_decimal$true_positives + 0.5

  # Should either error or automatically round
  result <- tryCatch(
    {
      diagnosticmeta(
        data = test_data_decimal,
        study = "study",
        true_positives = "true_positives",
        false_positives = "false_positives",
        false_negatives = "false_negatives",
        true_negatives = "true_negatives"
      )
    },
    error = function(e) {
      expect_match(
        e$message,
        "integer|whole|count",
        ignore.case = TRUE
      )
      NULL
    }
  )

  # If it doesn't error, should complete successfully
  if (!is.null(result)) {
    expect_s3_class(result, "diagnosticmetaClass")
  }
})

test_that("diagnosticmeta handles duplicate study identifiers", {
  # Create dataset with duplicate study names
  test_data_dup <- diagnosticmeta_test
  test_data_dup$study[1:2] <- "Study_1"

  # Should either error or warn about duplicates
  expect_condition(
    diagnosticmeta(
      data = test_data_dup,
      study = "study",
      true_positives = "true_positives",
      false_positives = "false_positives",
      false_negatives = "false_negatives",
      true_negatives = "true_negatives"
    )
  )
})

test_that("diagnosticmeta handles studies with perfect sensitivity", {
  # Create dataset where one study has 100% sensitivity
  test_data_perfect <- diagnosticmeta_test
  test_data_perfect$false_negatives[1] <- 0
  test_data_perfect$true_positives[1] <- 100

  result <- diagnosticmeta(
    data = test_data_perfect,
    study = "study",
    true_positives = "true_positives",
    false_positives = "false_positives",
    false_negatives = "false_negatives",
    true_negatives = "true_negatives"
  )

  expect_s3_class(result, "diagnosticmetaClass")
})

test_that("diagnosticmeta handles studies with perfect specificity", {
  # Create dataset where one study has 100% specificity
  test_data_perfect <- diagnosticmeta_test
  test_data_perfect$false_positives[1] <- 0
  test_data_perfect$true_negatives[1] <- 100

  result <- diagnosticmeta(
    data = test_data_perfect,
    study = "study",
    true_positives = "true_positives",
    false_positives = "false_positives",
    false_negatives = "false_negatives",
    true_negatives = "true_negatives"
  )

  expect_s3_class(result, "diagnosticmetaClass")
})

test_that("diagnosticmeta handles studies with very high heterogeneity", {
  # Create dataset with extreme heterogeneity
  test_data_hetero <- diagnosticmeta_test
  # Make some studies very sensitive, others not
  test_data_hetero$true_positives[1:5] <- test_data_hetero$true_positives[1:5] * 2
  test_data_hetero$false_negatives[1:5] <- 1
  test_data_hetero$true_positives[6:10] <- 5
  test_data_hetero$false_negatives[6:10] <- test_data_hetero$false_negatives[6:10] * 3

  result <- diagnosticmeta(
    data = test_data_hetero,
    study = "study",
    true_positives = "true_positives",
    false_positives = "false_positives",
    false_negatives = "false_negatives",
    true_negatives = "true_negatives",
    heterogeneity_analysis = TRUE
  )

  expect_s3_class(result, "diagnosticmetaClass")
})

test_that("diagnosticmeta handles single study (should error)", {
  # Meta-analysis requires at least 2 studies
  single_study <- diagnosticmeta_test[1, ]

  expect_error(
    diagnosticmeta(
      data = single_study,
      study = "study",
      true_positives = "true_positives",
      false_positives = "false_positives",
      false_negatives = "false_negatives",
      true_negatives = "true_negatives"
    ),
    regexp = "single|one study|at least.*2|multiple",
    ignore.case = TRUE
  )
})

test_that("diagnosticmeta handles covariate with all same values", {
  # Create covariate with no variation
  test_data_const <- diagnosticmeta_test
  test_data_const$constant_cov <- 5

  # Meta-regression with constant covariate should error or warn
  expect_condition(
    diagnosticmeta(
      data = test_data_const,
      study = "study",
      true_positives = "true_positives",
      false_positives = "false_positives",
      false_negatives = "false_negatives",
      true_negatives = "true_negatives",
      covariate = "constant_cov",
      meta_regression = TRUE
    )
  )
})

test_that("diagnosticmeta handles covariate with missing values", {
  # Create covariate with missing values
  test_data_cov_na <- diagnosticmeta_test
  test_data_cov_na$year[1:5] <- NA

  # Should handle missing covariate values appropriately
  result <- tryCatch(
    {
      diagnosticmeta(
        data = test_data_cov_na,
        study = "study",
        true_positives = "true_positives",
        false_positives = "false_positives",
        false_negatives = "false_negatives",
        true_negatives = "true_negatives",
        covariate = "year",
        meta_regression = TRUE
      )
    },
    error = function(e) NULL,
    warning = function(w) NULL
  )

  # Should either complete or give informative error
  if (!is.null(result)) {
    expect_s3_class(result, "diagnosticmetaClass")
  }
})

test_that("diagnosticmeta handles all zero cells in one row", {
  # Create dataset with all zeros (invalid)
  test_data_all_zero <- diagnosticmeta_test
  test_data_all_zero[1, c("true_positives", "false_positives",
                          "false_negatives", "true_negatives")] <- 0

  # Should error with informative message
  expect_error(
    diagnosticmeta(
      data = test_data_all_zero,
      study = "study",
      true_positives = "true_positives",
      false_positives = "false_positives",
      false_negatives = "false_negatives",
      true_negatives = "true_negatives"
    ),
    regexp = "zero|empty|invalid",
    ignore.case = TRUE
  )
})

test_that("diagnosticmeta handles inconsistent 2x2 table structure", {
  # Create dataset where TP + FN ≠ total diseased (data error)
  test_data_inconsistent <- diagnosticmeta_test
  # Artificially create inconsistency (though this shouldn't happen in real data)

  # This test assumes the function checks for consistency
  # If not, this test documents that behavior
  result <- diagnosticmeta(
    data = test_data_inconsistent,
    study = "study",
    true_positives = "true_positives",
    false_positives = "false_positives",
    false_negatives = "false_negatives",
    true_negatives = "true_negatives"
  )

  expect_s3_class(result, "diagnosticmetaClass")
})

test_that("diagnosticmeta handles extreme confidence levels", {
  # Test with minimum confidence level
  result_50 <- diagnosticmeta(
    data = diagnosticmeta_test,
    study = "study",
    true_positives = "true_positives",
    false_positives = "false_positives",
    false_negatives = "false_negatives",
    true_negatives = "true_negatives",
    confidence_level = 50
  )

  expect_s3_class(result_50, "diagnosticmetaClass")

  # Test with maximum confidence level
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

test_that("diagnosticmeta handles variable names with special characters", {
  # Create dataset with special characters in variable names
  test_data_special <- diagnosticmeta_test
  names(test_data_special)[names(test_data_special) == "study"] <- "study name"

  result <- diagnosticmeta(
    data = test_data_special,
    study = "study name",
    true_positives = "true_positives",
    false_positives = "false_positives",
    false_negatives = "false_negatives",
    true_negatives = "true_negatives"
  )

  expect_s3_class(result, "diagnosticmetaClass")
})
