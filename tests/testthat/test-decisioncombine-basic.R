# ═══════════════════════════════════════════════════════════
# Basic Functionality Tests: decisioncombine
# ═══════════════════════════════════════════════════════════
#
# Tests basic functionality and required arguments
# for the decisioncombine jamovi function

library(testthat)
library(ClinicoPath)

# Load test data
data(decisioncombine_pathology, package = "ClinicoPath")

test_that("decisioncombine function exists", {
  expect_true(exists("decisioncombine"))
  expect_true(is.function(decisioncombine))
})

test_that("decisioncombine runs with minimal required arguments", {
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
  expect_true("results" %in% names(result))
})

test_that("decisioncombine handles two-test combination", {
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

test_that("decisioncombine errors on missing gold standard", {
  expect_error(
    decisioncombine(
      data = decisioncombine_pathology,
      test1 = "rater1",
      test1Positive = "Positive",
      test2 = "rater2",
      test2Positive = "Positive"
    ),
    regexp = "gold.*required|missing.*gold",
    ignore.case = TRUE
  )
})

test_that("decisioncombine errors on missing test1", {
  expect_error(
    decisioncombine(
      data = decisioncombine_pathology,
      gold = "gold_standard",
      goldPositive = "Malignant",
      test2 = "rater2",
      test2Positive = "Positive"
    ),
    regexp = "test.*required|missing.*test",
    ignore.case = TRUE
  )
})

test_that("decisioncombine errors on missing test2", {
  expect_error(
    decisioncombine(
      data = decisioncombine_pathology,
      gold = "gold_standard",
      goldPositive = "Malignant",
      test1 = "rater1",
      test1Positive = "Positive"
    ),
    regexp = "test2.*required|missing.*test",
    ignore.case = TRUE
  )
})

test_that("decisioncombine handles binary gold standard correctly", {
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

test_that("decisioncombine handles binary test results", {
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

test_that("decisioncombine produces expected output structure", {
  result <- decisioncombine(
    data = decisioncombine_pathology,
    gold = "gold_standard",
    goldPositive = "Malignant",
    test1 = "rater1",
    test1Positive = "Positive",
    test2 = "rater2",
    test2Positive = "Positive"
  )

  # Check that result has results component
  expect_true("results" %in% names(result))

  # Results should be a list
  expect_type(result$results, "list")
})

test_that("decisioncombine handles different positive class labels", {
  result <- decisioncombine(
    data = decisioncombine_pathology,
    gold = "gold_standard",
    goldPositive = "Benign",  # Use opposite class
    test1 = "rater1",
    test1Positive = "Negative",
    test2 = "rater2",
    test2Positive = "Negative"
  )

  expect_s3_class(result, "decisioncombineClass")
})

test_that("decisioncombine handles small dataset", {
  data(decisioncombine_small, package = "ClinicoPath")

  result <- decisioncombine(
    data = decisioncombine_small,
    gold = "gold_standard",
    goldPositive = "Positive",
    test1 = "test1",
    test1Positive = "Positive",
    test2 = "test2",
    test2Positive = "Positive"
  )

  expect_s3_class(result, "decisioncombineClass")
})

test_that("decisioncombine accepts default options", {
  # Test with all default options
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
  expect_no_error(result)
})

test_that("decisioncombine handles concordant tests", {
  data(decisioncombine_concordant, package = "ClinicoPath")

  result <- decisioncombine(
    data = decisioncombine_concordant,
    gold = "gold_standard",
    goldPositive = "Disease Present",
    test1 = "test_a",
    test1Positive = "Positive",
    test2 = "test_b",
    test2Positive = "Positive"
  )

  expect_s3_class(result, "decisioncombineClass")
})

test_that("decisioncombine handles discordant tests", {
  data(decisioncombine_discordant, package = "ClinicoPath")

  result <- decisioncombine(
    data = decisioncombine_discordant,
    gold = "gold_standard",
    goldPositive = "Positive",
    test1 = "sensitive_test",
    test1Positive = "Positive",
    test2 = "specific_test",
    test2Positive = "Positive"
  )

  expect_s3_class(result, "decisioncombineClass")
})
