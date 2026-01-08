# ═══════════════════════════════════════════════════════════
# Edge Case Tests: decisioncombine
# ═══════════════════════════════════════════════════════════
#
# Tests edge cases, error handling, and boundary conditions
# for the decisioncombine jamovi function

library(testthat)
library(ClinicoPath)

# Load test data
data(decisioncombine_pathology, package = "ClinicoPath")

test_that("decisioncombine handles missing values in gold standard", {
  test_data_na <- decisioncombine_pathology
  test_data_na$gold_standard[1:10] <- NA

  # Should warn about missing data or handle gracefully
  expect_warning(
    decisioncombine(
      data = test_data_na,
      gold = "gold_standard",
      goldPositive = "Malignant",
      test1 = "rater1",
      test1Positive = "Positive",
      test2 = "rater2",
      test2Positive = "Positive"
    ),
    regexp = "missing|NA|removed",
    ignore.case = TRUE
  )
})

test_that("decisioncombine handles missing values in test1", {
  test_data_na <- decisioncombine_pathology
  test_data_na$rater1[1:10] <- NA

  expect_warning(
    decisioncombine(
      data = test_data_na,
      gold = "gold_standard",
      goldPositive = "Malignant",
      test1 = "rater1",
      test1Positive = "Positive",
      test2 = "rater2",
      test2Positive = "Positive"
    ),
    regexp = "missing|NA|removed",
    ignore.case = TRUE
  )
})

test_that("decisioncombine handles missing values in test2", {
  test_data_na <- decisioncombine_pathology
  test_data_na$rater2[1:10] <- NA

  expect_warning(
    decisioncombine(
      data = test_data_na,
      gold = "gold_standard",
      goldPositive = "Malignant",
      test1 = "rater1",
      test1Positive = "Positive",
      test2 = "rater2",
      test2Positive = "Positive"
    ),
    regexp = "missing|NA|removed",
    ignore.case = TRUE
  )
})

test_that("decisioncombine handles all positive gold standard", {
  test_data_all_pos <- decisioncombine_pathology
  test_data_all_pos$gold_standard <- factor(rep("Malignant", nrow(decisioncombine_pathology)))

  # Should error about no variation in gold standard
  expect_error(
    decisioncombine(
      data = test_data_all_pos,
      gold = "gold_standard",
      goldPositive = "Malignant",
      test1 = "rater1",
      test1Positive = "Positive",
      test2 = "rater2",
      test2Positive = "Positive"
    ),
    regexp = "variation|constant|all.*same|binary",
    ignore.case = TRUE
  )
})

test_that("decisioncombine handles all negative gold standard", {
  test_data_all_neg <- decisioncombine_pathology
  test_data_all_neg$gold_standard <- factor(rep("Benign", nrow(decisioncombine_pathology)))

  expect_error(
    decisioncombine(
      data = test_data_all_neg,
      gold = "gold_standard",
      goldPositive = "Malignant",
      test1 = "rater1",
      test1Positive = "Positive",
      test2 = "rater2",
      test2Positive = "Positive"
    ),
    regexp = "variation|constant|all.*same|binary",
    ignore.case = TRUE
  )
})

test_that("decisioncombine handles constant test1 results", {
  test_data_const <- decisioncombine_pathology
  test_data_const$rater1 <- factor(rep("Positive", nrow(decisioncombine_pathology)))

  # Should error or warn about no variation
  expect_condition(
    decisioncombine(
      data = test_data_const,
      gold = "gold_standard",
      goldPositive = "Malignant",
      test1 = "rater1",
      test1Positive = "Positive",
      test2 = "rater2",
      test2Positive = "Positive"
    )
  )
})

test_that("decisioncombine handles constant test2 results", {
  test_data_const <- decisioncombine_pathology
  test_data_const$rater2 <- factor(rep("Negative", nrow(decisioncombine_pathology)))

  expect_condition(
    decisioncombine(
      data = test_data_const,
      gold = "gold_standard",
      goldPositive = "Malignant",
      test1 = "rater1",
      test1Positive = "Positive",
      test2 = "rater2",
      test2Positive = "Positive"
    )
  )
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
    test2Positive = "Positive"
  )

  expect_s3_class(result, "decisioncombineClass")
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
    test2Positive = "Positive"
  )

  # Should complete (may have special interpretation)
  expect_s3_class(result, "decisioncombineClass")
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
    test2Positive = "Positive"
  )

  expect_s3_class(result, "decisioncombineClass")
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
    test2Positive = "Positive"
  )

  expect_s3_class(result, "decisioncombineClass")
})

test_that("decisioncombine handles wrong positive class specification", {
  # Specify non-existent level
  expect_error(
    decisioncombine(
      data = decisioncombine_pathology,
      gold = "gold_standard",
      goldPositive = "InvalidLevel",
      test1 = "rater1",
      test1Positive = "Positive",
      test2 = "rater2",
      test2Positive = "Positive"
    ),
    regexp = "level.*not found|invalid.*class",
    ignore.case = TRUE
  )
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
    test2Positive = "Positive"
  )

  expect_s3_class(result, "decisioncombineClass")
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
    test2Positive = "Positive"
  )

  # Should complete but may have wide confidence intervals
  expect_s3_class(result, "decisioncombineClass")
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
    test2Positive = "Positive"
  )

  expect_s3_class(result, "decisioncombineClass")
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
    test2Positive = "Positive"
  )

  expect_s3_class(result, "decisioncombineClass")
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
    test2Positive = "Positive"
  )

  # Should handle gracefully (zero counts for that pattern)
  expect_s3_class(result, "decisioncombineClass")
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

  expect_s3_class(result, "decisioncombineClass")
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
    test2Positive = "Positive"
  )

  # Should handle by converting to factors
  expect_s3_class(result, "decisioncombineClass")
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
    test2Positive = "1"
  )

  expect_s3_class(result, "decisioncombineClass")
})

test_that("decisioncombine handles gold standard with more than 2 levels initially", {
  # Create multi-level gold standard
  test_data_multi <- decisioncombine_pathology
  test_data_multi$multi_gold <- factor(
    sample(c("Benign", "Borderline", "Malignant"), nrow(decisioncombine_pathology), replace = TRUE)
  )

  # Should error or require binary gold standard
  expect_error(
    decisioncombine(
      data = test_data_multi,
      gold = "multi_gold",
      goldPositive = "Malignant",
      test1 = "rater1",
      test1Positive = "Positive",
      test2 = "rater2",
      test2Positive = "Positive"
    ),
    regexp = "binary|2 levels|exactly 2",
    ignore.case = TRUE
  )
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
    test2Positive = "Positive"
  )

  # Should complete but with wide/undefined confidence intervals
  expect_s3_class(result, "decisioncombineClass")
})
