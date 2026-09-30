# ═══════════════════════════════════════════════════════════
# Basic Functionality Tests: decisioncombine
# ═══════════════════════════════════════════════════════════
#
# Tests basic functionality and required arguments
# for the decisioncombine jamovi function

library(testthat)

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
    test2Positive = "Positive",
    test3Positive = NULL
  )

  expect_s3_class(result, "decisioncombineResults")
  expect_true("combinationTable" %in% names(result))
})

test_that("decisioncombine handles two-test combination", {
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

test_that("decisioncombine waits silently when a gold standard is not selected", {
  # A jamovi analysis does not error on an incomplete variable selection: it returns and
  # leaves the results empty until the user finishes choosing. These tests previously
  # asserted expect_error(), which no jamovi analysis satisfies, so they checked nothing.
  expect_no_error(
    res <- decisioncombine(
      data = decisioncombine_pathology,
      test1 = "rater1", test1Positive = "Positive",
      test2 = "rater2", test2Positive = "Positive",
      goldPositive = NULL, test3Positive = NULL
    )
  )
  expect_equal(res$combinationTable$rowCount, 0L)
})

test_that("decisioncombine waits silently when test 1 is not selected", {
  # A jamovi analysis does not error on an incomplete variable selection: it returns and
  # leaves the results empty until the user finishes choosing. These tests previously
  # asserted expect_error(), which no jamovi analysis satisfies, so they checked nothing.
  expect_no_error(
    res <- decisioncombine(
      data = decisioncombine_pathology,
      gold = "gold_standard", goldPositive = "Malignant",
      test2 = "rater2", test2Positive = "Positive",
      test1Positive = NULL, test3Positive = NULL
    )
  )
  expect_equal(res$combinationTable$rowCount, 0L)
})

test_that("the Getting Started panel shows until the gold standard and Test 1 are chosen", {
  # A freshly opened analysis showed three empty tables and nothing else, while
  # .validateInputs() returned quietly to "let the instructions panel speak" -- a panel
  # that did not exist. (With NO variable at all the R wrapper stops inside
  # jmvcore::select() before the analysis runs, a path jamovi never takes, so each half
  # of the visible: expression is exercised by one partial selection instead.)
  r1 <- decisioncombine(data = decisioncombine_pathology,
                        gold = "gold_standard", goldPositive = "Malignant",
                        test1Positive = NULL, test2Positive = NULL, test3Positive = NULL)
  expect_true(r1$welcome$visible)
  expect_true(nzchar(r1$welcome$content))
  expect_match(r1$welcome$content, "Quick Start", fixed = TRUE)

  r0 <- decisioncombine(data = decisioncombine_pathology,
                        test1 = "rater1", test1Positive = "Positive",
                        goldPositive = NULL, test2Positive = NULL, test3Positive = NULL)
  expect_true(r0$welcome$visible)

  # and with both chosen the panel steps out of the way
  r2 <- decisioncombine(data = decisioncombine_pathology,
                        gold = "gold_standard", goldPositive = "Malignant",
                        test1 = "rater1", test1Positive = "Positive",
                        test2Positive = NULL, test3Positive = NULL)
  expect_false(r2$welcome$visible)
})

test_that("punctuation, quotes and non-ASCII in names and levels change nothing and export parseable syntax", {
  # VAL-03 covers spaces. Columns are indexed by name (never through a formula or a regex)
  # and .sourcifyOption() writes each variable and level as an escaped string literal, so a
  # bracket, a quote, a plus sign or a Turkish letter must give the plain-name numbers.
  d <- decisioncombine_pathology[, c("gold_standard", "rater1", "rater2")]
  gold <- "Grade (2/3) \"final\""
  t1 <- "Tümör+ IHC"
  t2 <- "Rater #1; 50%"
  names(d) <- c(gold, t1, t2)
  levels(d[[gold]]) <- c("Benign", "Malign \"çok\"")   # was c("Benign", "Malignant")

  a <- decisioncombineClass$new(
    options = decisioncombineOptions$new(
      gold = gold, goldPositive = "Malign \"çok\"",
      test1 = t1, test1Positive = "Positive",
      test2 = t2, test2Positive = "Positive"),
    data = d)
  suppressWarnings(suppressMessages({a$init(); a$run()}))

  plain <- decisioncombine(
    data = decisioncombine_pathology, gold = "gold_standard", goldPositive = "Malignant",
    test1 = "rater1", test1Positive = "Positive", test2 = "rater2", test2Positive = "Positive",
    test3Positive = NULL)
  expect_gt(a$results$combinationTable$rowCount, 0L)
  expect_identical(a$results$combinationTable$asDF, plain$combinationTable$asDF)

  src <- a$asSource()
  expect_silent(parse(text = src))
  data <- d
  # eval() of the analysis's own exported syntax is what is under test
  res <- suppressWarnings(suppressMessages(eval(parse(text = src))))
  expect_identical(res$combinationTable$asDF, a$results$combinationTable$asDF)
})

test_that("decisioncombine reports the single test when only test 1 is selected", {
  # Gold + one test is enough to run: there is nothing to combine, so the analysis
  # reports that test on its own rather than erroring or waiting.
  expect_no_error(
    res <- decisioncombine(
      data = decisioncombine_pathology,
      gold = "gold_standard", goldPositive = "Malignant",
      test1 = "rater1", test1Positive = "Positive",
      test2Positive = NULL, test3Positive = NULL
    )
  )
  expect_equal(res$combinationTable$rowCount, 1L)
  expect_equal(as.character(res$combinationTable$asDF$pattern[1]), "Test 1")
})

test_that("decisioncombine handles binary gold standard correctly", {
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

test_that("decisioncombine handles binary test results", {
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

test_that("decisioncombine produces expected output structure", {
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

  # Check that result has results component
  expect_true("combinationTable" %in% names(result))

  # Results should be a list
  expect_true("combinationTableCI" %in% names(result))
})

test_that("decisioncombine handles different positive class labels", {
  result <- decisioncombine(
    data = decisioncombine_pathology,
    gold = "gold_standard",
    goldPositive = "Benign",  # Use opposite class
    test1 = "rater1",
    test1Positive = "Negative",
    test2 = "rater2",
    test2Positive = "Negative",
    test3Positive = NULL
  )

  expect_s3_class(result, "decisioncombineResults")
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
    test2Positive = "Positive",
    test3Positive = NULL
  )

  expect_s3_class(result, "decisioncombineResults")
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
    test2Positive = "Positive",
    test3Positive = NULL
  )

  expect_s3_class(result, "decisioncombineResults")
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
    test2Positive = "Positive",
    test3Positive = NULL
  )

  expect_s3_class(result, "decisioncombineResults")
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
    test2Positive = "Positive",
    test3Positive = NULL
  )

  expect_s3_class(result, "decisioncombineResults")
})
