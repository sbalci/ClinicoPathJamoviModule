# ═══════════════════════════════════════════════════════════
# Edge Case Tests: decisioncombine
# ═══════════════════════════════════════════════════════════
#
# Tests edge cases, error handling, and boundary conditions
# for the decisioncombine jamovi function

library(testthat)

# Load test data
data(decisioncombine_pathology, package = "ClinicoPath")

# decisioncombine reports problems as jamovi notices rendered into an Html output, not
# as R conditions -- the convention across the whole meddecide family. Several tests below
# were written against expect_warning()/expect_error(), which no analysis in this module
# ever satisfies, so they asserted nothing. They now check the notice a user actually sees.
dc_notices <- function(result) {
  gsub("[[:space:]]+", " ", gsub("<[^>]+>", " ", paste(result$notices$content, collapse = " ")))
}


test_that("decisioncombine discloses cases dropped for a missing gold standard", {
  test_data_na <- decisioncombine_pathology
  test_data_na$gold_standard[1:10] <- NA

  res <- decisioncombine(
    data = test_data_na, gold = "gold_standard", goldPositive = "Malignant",
    test1 = "rater1", test1Positive = "Positive",
    test2 = "rater2", test2Positive = "Positive", test3Positive = NULL)

  # Cases used to be dropped with no disclosure at all.
  expect_match(dc_notices(res), "Removed 10 case\\(s\\) with missing values")
  expect_match(dc_notices(res), "Complete-case analysis uses")

  n_used <- with(res$combinationTable$asDF[1, ], tp + fp + fn + tn)
  expect_equal(n_used, nrow(test_data_na) - 10)
})

test_that("decisioncombine discloses cases dropped for a missing test1", {
  test_data_na <- decisioncombine_pathology
  test_data_na$rater1[1:5] <- NA

  res <- decisioncombine(
    data = test_data_na, gold = "gold_standard", goldPositive = "Malignant",
    test1 = "rater1", test1Positive = "Positive",
    test2 = "rater2", test2Positive = "Positive", test3Positive = NULL)

  expect_match(dc_notices(res), "Removed 5 case\\(s\\) with missing values")
  expect_equal(with(res$combinationTable$asDF[1, ], tp + fp + fn + tn),
               nrow(test_data_na) - 5)
})

test_that("decisioncombine discloses cases dropped for a missing test2", {
  test_data_na <- decisioncombine_pathology
  test_data_na$rater2[1:8] <- NA

  res <- decisioncombine(
    data = test_data_na, gold = "gold_standard", goldPositive = "Malignant",
    test1 = "rater1", test1Positive = "Positive",
    test2 = "rater2", test2Positive = "Positive", test3Positive = NULL)

  expect_match(dc_notices(res), "Removed 8 case\\(s\\) with missing values")
})

test_that("decisioncombine flags a gold standard with only one outcome (all positive)", {
  d <- decisioncombine_pathology
  d$gold_standard <- factor("Malignant", levels = levels(factor(d$gold_standard)))

  res <- decisioncombine(
    data = d, gold = "gold_standard", goldPositive = "Malignant",
    test1 = "rater1", test1Positive = "Positive",
    test2 = "rater2", test2Positive = "Positive", test3Positive = NULL)

  # Specificity and NPV are undefined without disease-absent cases; that used to come
  # back as a bare NA with nothing to explain it.
  expect_match(dc_notices(res), "Gold Standard Has Only One Outcome")
  expect_match(dc_notices(res), "no disease-absent cases")
  expect_true(all(is.na(res$combinationTable$asDF$spec)))
})

test_that("decisioncombine flags a gold standard with only one outcome (all negative)", {
  d <- decisioncombine_pathology
  d$gold_standard <- factor("Benign", levels = levels(factor(d$gold_standard)))

  res <- decisioncombine(
    data = d, gold = "gold_standard", goldPositive = "Malignant",
    test1 = "rater1", test1Positive = "Positive",
    test2 = "rater2", test2Positive = "Positive", test3Positive = NULL)

  expect_match(dc_notices(res), "Gold Standard Has Only One Outcome")
  expect_true(all(is.na(res$combinationTable$asDF$sens)))
})

test_that("a constant test1 still yields valid combinations, with zero cells flagged", {
  test_data_const <- decisioncombine_pathology
  test_data_const$rater1 <- factor(rep("Positive", nrow(decisioncombine_pathology)))

  res <- decisioncombine(
    data = test_data_const, gold = "gold_standard", goldPositive = "Malignant",
    test1 = "rater1", test1Positive = "Positive",
    test2 = "rater2", test2Positive = "Positive", test3Positive = NULL)

  # Every patient is test1-positive, so no "-/x" pattern has any members. The analysis
  # still runs; the empty cells are disclosed as continuity corrections.
  expect_gt(res$combinationTable$rowCount, 0L)
  expect_match(dc_notices(res), "Continuity Correction")

  ct <- res$combinationTable$asDF
  # test1 alone can never be negative, so "-/+" and "-/-" hold nobody
  expect_equal(sum(ct$tp[ct$pattern == "-/+"], ct$fp[ct$pattern == "-/+"]), 0)
})

test_that("a positive level absent from test2 halts with an explanatory notice", {
  test_data_const <- decisioncombine_pathology
  test_data_const$rater2 <- factor(rep("Negative", nrow(decisioncombine_pathology)))

  res <- decisioncombine(
    data = test_data_const, gold = "gold_standard", goldPositive = "Malignant",
    test1 = "rater1", test1Positive = "Positive",
    test2 = "rater2", test2Positive = "Positive", test3Positive = NULL)

  expect_match(dc_notices(res), "Missing Level")
  expect_match(dc_notices(res), "rater2")
  expect_equal(res$combinationTable$rowCount, 0L)
})

test_that("decisioncombine handles very small sample size", {
  small_data <- decisioncombine_pathology[1:15, ]

  # Should warn about small sample or complete successfully
  result <- decisioncombine(
    data = small_data,
    gold = "gold_standard",
    goldPositive = "Malignant",
    test1 = "rater1",
    test1Positive = "Positive",
    test2 = "rater2",
    test2Positive = "Positive",
    test3Positive = NULL
  )

  expect_s3_class(result, "decisioncombineResults")
})

test_that("decisioncombine handles perfect concordance", {
  # Both tests give identical results
  test_data_perfect <- decisioncombine_pathology
  test_data_perfect$rater2 <- test_data_perfect$rater1  # Make tests identical

  result <- decisioncombine(
    data = test_data_perfect,
    gold = "gold_standard",
    goldPositive = "Malignant",
    test1 = "rater1",
    test1Positive = "Positive",
    test2 = "rater2",
    test2Positive = "Positive",
    test3Positive = NULL
  )

  # Should complete (may have special interpretation)
  expect_s3_class(result, "decisioncombineResults")
})

test_that("decisioncombine handles perfect test performance (AUC=1)", {
  # Create test that perfectly matches gold standard
  test_data_perfect <- decisioncombine_pathology
  test_data_perfect$perfect_test <- test_data_perfect$gold_standard
  levels(test_data_perfect$perfect_test) <- c("Negative", "Positive")

  result <- decisioncombine(
    data = test_data_perfect,
    gold = "gold_standard",
    goldPositive = "Malignant",
    test1 = "perfect_test",
    test1Positive = "Positive",
    test2 = "rater2",
    test2Positive = "Positive",
    test3Positive = NULL
  )

  expect_s3_class(result, "decisioncombineResults")
})

test_that("decisioncombine handles completely random test (AUC=0.5)", {
  # Create random test
  set.seed(123)
  test_data_random <- decisioncombine_pathology
  test_data_random$random_test <- factor(
    sample(c("Negative", "Positive"), nrow(decisioncombine_pathology), replace = TRUE)
  )

  result <- decisioncombine(
    data = test_data_random,
    gold = "gold_standard",
    goldPositive = "Malignant",
    test1 = "random_test",
    test1Positive = "Positive",
    test2 = "rater2",
    test2Positive = "Positive",
    test3Positive = NULL
  )

  expect_s3_class(result, "decisioncombineResults")
})

test_that("decisioncombine explains a positive level that is not in the data", {
  res <- decisioncombine(
    data = decisioncombine_pathology,
    gold = "gold_standard", goldPositive = "InvalidLevel",
    test1 = "rater1", test1Positive = "Positive",
    test2 = "rater2", test2Positive = "Positive", test3Positive = NULL)

  # The analysis halts. The notice explaining WHY used to be collected and then
  # discarded, because .renderNotices() sat after the early return.
  expect_match(dc_notices(res), "Missing Level")
  expect_match(dc_notices(res), "InvalidLevel")
  expect_equal(res$combinationTable$rowCount, 0L)
})

test_that("decisioncombine handles duplicate observations", {
  # Create exact duplicates
  test_data_dup <- rbind(decisioncombine_pathology, decisioncombine_pathology[1:20, ])

  result <- decisioncombine(
    data = test_data_dup,
    gold = "gold_standard",
    goldPositive = "Malignant",
    test1 = "rater1",
    test1Positive = "Positive",
    test2 = "rater2",
    test2Positive = "Positive",
    test3Positive = NULL
  )

  expect_s3_class(result, "decisioncombineResults")
})

test_that("decisioncombine handles extreme prevalence", {
  # Very low prevalence (5 positive out of 200)
  test_data_low_prev <- decisioncombine_pathology
  test_data_low_prev$gold_standard[6:200] <- "Benign"

  result <- decisioncombine(
    data = test_data_low_prev,
    gold = "gold_standard",
    goldPositive = "Malignant",
    test1 = "rater1",
    test1Positive = "Positive",
    test2 = "rater2",
    test2Positive = "Positive",
    test3Positive = NULL
  )

  # Should complete but may have wide confidence intervals
  expect_s3_class(result, "decisioncombineResults")
})

test_that("decisioncombine handles high prevalence", {
  # Very high prevalence (195 positive out of 200)
  test_data_high_prev <- decisioncombine_pathology
  test_data_high_prev$gold_standard[1:195] <- "Malignant"

  result <- decisioncombine(
    data = test_data_high_prev,
    gold = "gold_standard",
    goldPositive = "Malignant",
    test1 = "rater1",
    test1Positive = "Positive",
    test2 = "rater2",
    test2Positive = "Positive",
    test3Positive = NULL
  )

  expect_s3_class(result, "decisioncombineResults")
})

test_that("decisioncombine handles all four test patterns present", {
  # Ensure all 4 patterns (+/+, +/-, -/+, -/-) are represented
  # This should be typical case
  result <- decisioncombine(
    data = decisioncombine_pathology,
    gold = "gold_standard",
    goldPositive = "Malignant",
    test1 = "rater1",
    test1Positive = "Positive",
    test2 = "rater2",
    test2Positive = "Positive",
    test3Positive = NULL
  )

  expect_s3_class(result, "decisioncombineResults")
})

test_that("decisioncombine handles missing pattern (no +/+ cases)", {
  # Create data where both tests are never simultaneously positive
  test_data_no_both_pos <- decisioncombine_pathology
  both_pos_mask <- (test_data_no_both_pos$rater1 == "Positive" &
                   test_data_no_both_pos$rater2 == "Positive")
  if (sum(both_pos_mask) > 0) {
    test_data_no_both_pos$rater2[both_pos_mask] <- "Negative"
  }

  result <- decisioncombine(
    data = test_data_no_both_pos,
    gold = "gold_standard",
    goldPositive = "Malignant",
    test1 = "rater1",
    test1Positive = "Positive",
    test2 = "rater2",
    test2Positive = "Positive",
    test3Positive = NULL
  )

  # Should handle gracefully (zero counts for that pattern)
  expect_s3_class(result, "decisioncombineResults")
})

test_that("decisioncombine handles three-test with missing patterns", {
  data(decisioncombine_threetest, package = "ClinicoPath")

  # Small sample may not have all 8 patterns
  small_three_test <- decisioncombine_threetest[1:30, ]

  result <- decisioncombine(
    data = small_three_test,
    gold = "gold_standard",
    goldPositive = "Disease",
    test1 = "clinical_exam",
    test1Positive = "Positive",
    test2 = "lab_test",
    test2Positive = "Positive",
    test3 = "imaging",
    test3Positive = "Positive"
  )

  expect_s3_class(result, "decisioncombineResults")
})

test_that("decisioncombine handles non-factor variables", {
  # Convert factors to character
  test_data_char <- decisioncombine_pathology
  test_data_char$gold_standard <- as.character(test_data_char$gold_standard)
  test_data_char$rater1 <- as.character(test_data_char$rater1)
  test_data_char$rater2 <- as.character(test_data_char$rater2)

  result <- decisioncombine(
    data = test_data_char,
    gold = "gold_standard",
    goldPositive = "Malignant",
    test1 = "rater1",
    test1Positive = "Positive",
    test2 = "rater2",
    test2Positive = "Positive",
    test3Positive = NULL
  )

  # Should handle by converting to factors
  expect_s3_class(result, "decisioncombineResults")
})

test_that("decisioncombine handles variables with unusual level names", {
  # Test with numbers, special characters
  test_data_unusual <- decisioncombine_pathology
  levels(test_data_unusual$gold_standard) <- c("0", "1")
  levels(test_data_unusual$rater1) <- c("0", "1")
  levels(test_data_unusual$rater2) <- c("0", "1")

  result <- decisioncombine(
    data = test_data_unusual,
    gold = "gold_standard",
    goldPositive = "1",
    test1 = "rater1",
    test1Positive = "1",
    test2 = "rater2",
    test2Positive = "1",
    test3Positive = NULL
  )

  expect_s3_class(result, "decisioncombineResults")
})

test_that("a gold standard with more than two levels is flagged, not silently dichotomised", {
  set.seed(1)
  test_data_multi <- decisioncombine_pathology
  test_data_multi$multi_gold <- factor(
    sample(c("Benign", "Borderline", "Malignant"), nrow(decisioncombine_pathology), replace = TRUE))

  res <- decisioncombine(
    data = test_data_multi, gold = "multi_gold", goldPositive = "Malignant",
    test1 = "rater1", test1Positive = "Positive",
    test2 = "rater2", test2Positive = "Positive", test3Positive = NULL)

  # It runs -- one-vs-rest is a legitimate choice -- but folding "Borderline" into the
  # negative arm inflates specificity and NPV, and that used to happen silently.
  expect_gt(res$combinationTable$rowCount, 0L)
  expect_match(dc_notices(res), "has 3 levels")
  expect_match(dc_notices(res), "counted as NEGATIVE")
  expect_match(dc_notices(res), "inflates specificity and NPV")
})

test_that("decisioncombine handles single observation per pattern cell", {
  # Very small dataset ensuring minimal observations
  minimal_data <- decisioncombine_pathology[1:8, ]

  result <- decisioncombine(
    data = minimal_data,
    gold = "gold_standard",
    goldPositive = "Malignant",
    test1 = "rater1",
    test1Positive = "Positive",
    test2 = "rater2",
    test2Positive = "Positive",
    test3Positive = NULL
  )

  # Should complete but with wide/undefined confidence intervals
  expect_s3_class(result, "decisioncombineResults")
})
