# ═══════════════════════════════════════════════════════════
# Edge Cases Tests: decisioncompare
# ═══════════════════════════════════════════════════════════
library(testthat)
data(decisioncompare_small, package = "ClinicoPath")
data(decisioncompare_large, package = "ClinicoPath")
data(decisioncompare_perfect, package = "ClinicoPath")
data(decisioncompare_poor, package = "ClinicoPath")
data(decisioncompare_rare, package = "ClinicoPath")
data(decisioncompare_common, package = "ClinicoPath")
data(decisioncompare_identical, package = "ClinicoPath")
data(decisioncompare_missing, package = "ClinicoPath")
data(decisioncompare_indeterminate, package = "ClinicoPath")

test_that("decisioncompare handles small datasets", {
  result <- call_decisioncompare(
    data = decisioncompare_small,
    gold = "GoldStandard",
    goldPositive = "Positive",
    goldNegative = "Negative",
    test1 = "Test1",
    test1Positive = "Positive",
    test1Negative = "Negative",
    test2 = "Test2",
    test2Positive = "Positive",
    test2Negative = "Negative",
    test3Positive = NULL,
    test3Negative = NULL
  )
  expect_s3_class(result, "decisioncompareResults")
  expect_true(nrow(decisioncompare_small) == 30)
})

test_that("decisioncompare handles large datasets efficiently", {
  result <- call_decisioncompare(
    data = decisioncompare_large,
    gold = "GoldStandard",
    goldPositive = "Positive",
    goldNegative = "Negative",
    test1 = "Test1",
    test1Positive = "Positive",
    test1Negative = "Negative",
    test2 = "Test2",
    test2Positive = "Positive",
    test2Negative = "Negative",
    test3 = "Test3",
    test3Positive = "Positive",
    test3Negative = "Negative"
  )
  expect_s3_class(result, "decisioncompareResults")
  expect_true(nrow(decisioncompare_large) == 500)
})

test_that("decisioncompare handles perfect test performance", {
  result <- call_decisioncompare(
    data = decisioncompare_perfect,
    gold = "GoldStandard",
    goldPositive = "Positive",
    goldNegative = "Negative",
    test1 = "PerfectTest",
    test1Positive = "Positive",
    test1Negative = "Negative",
    test2 = "ImperfectTest",
    test2Positive = "Positive",
    test2Negative = "Negative",
    test3Positive = NULL,
    test3Negative = NULL
  )
  expect_s3_class(result, "decisioncompareResults")
})

test_that("decisioncompare handles poor test performance", {
  result <- call_decisioncompare(
    data = decisioncompare_poor,
    gold = "GoldStandard",
    goldPositive = "Positive",
    goldNegative = "Negative",
    test1 = "PoorTest1",
    test1Positive = "Positive",
    test1Negative = "Negative",
    test2 = "PoorTest2",
    test2Positive = "Positive",
    test2Negative = "Negative",
    test3Positive = NULL,
    test3Negative = NULL
  )
  expect_s3_class(result, "decisioncompareResults")
})

test_that("decisioncompare handles rare disease prevalence", {
  result <- call_decisioncompare(
    data = decisioncompare_rare,
    gold = "GoldStandard",
    goldPositive = "Positive",
    goldNegative = "Negative",
    test1 = "Test1",
    test1Positive = "Positive",
    test1Negative = "Negative",
    test2 = "Test2",
    test2Positive = "Positive",
    test2Negative = "Negative",
    test3Positive = NULL,
    test3Negative = NULL
  )
  expect_s3_class(result, "decisioncompareResults")
  # Check prevalence is low
  prevalence <- mean(decisioncompare_rare$GoldStandard == "Positive")
  expect_lt(prevalence, 0.10)
})

test_that("decisioncompare handles common disease prevalence", {
  result <- call_decisioncompare(
    data = decisioncompare_common,
    gold = "GoldStandard",
    goldPositive = "Positive",
    goldNegative = "Negative",
    test1 = "Test1",
    test1Positive = "Positive",
    test1Negative = "Negative",
    test2 = "Test2",
    test2Positive = "Positive",
    test2Negative = "Negative",
    test3Positive = NULL,
    test3Negative = NULL
  )
  expect_s3_class(result, "decisioncompareResults")
  # Check prevalence is high
  prevalence <- mean(decisioncompare_common$GoldStandard == "Positive")
  expect_gt(prevalence, 0.50)
})

test_that("decisioncompare handles identical tests", {
  result <- call_decisioncompare(
    data = decisioncompare_identical,
    gold = "GoldStandard",
    goldPositive = "Positive",
    goldNegative = "Negative",
    test1 = "Test1",
    test1Positive = "Positive",
    test1Negative = "Negative",
    test2 = "Test2",
    test2Positive = "Positive",
    test2Negative = "Negative",
    test3Positive = NULL,
    test3Negative = NULL
  )
  # Tests are identical - should complete without error
  expect_s3_class(result, "decisioncompareResults")
})

test_that("decisioncompare handles missing data", {
  result <- call_decisioncompare(
    data = decisioncompare_missing,
    gold = "GoldStandard",
    goldPositive = "Positive",
    goldNegative = "Negative",
    test1 = "Test1",
    test1Positive = "Positive",
    test1Negative = "Negative",
    test2 = "Test2",
    test2Positive = "Positive",
    test2Negative = "Negative",
    test3 = "Test3",
    test3Positive = "Positive",
    test3Negative = "Negative"
  )
  # Should handle with warnings or listwise deletion
  expect_s3_class(result, "decisioncompareResults")
  expect_true(any(is.na(decisioncompare_missing$GoldStandard)))
})

test_that("decisioncompare handles indeterminate results", {
  # Additional observed levels require an explicit exclusion choice.
  expect_error(call_decisioncompare(
    data = decisioncompare_indeterminate,
    gold = "GoldStandard",
    goldPositive = "Positive",
    goldNegative = "Negative",
    test1 = "Test1",
    test1Positive = "Positive",
    test1Negative = "Negative",
    test2 = "Test2",
    test2Positive = "Positive",
    test2Negative = "Negative",
    excludeIndeterminate = FALSE,
    test3Positive = NULL,
    test3Negative = NULL
  ), "outside the selected positive/negative pair")

  # With indeterminate results excluded
  result_exclude <- call_decisioncompare(
    data = decisioncompare_indeterminate,
    gold = "GoldStandard",
    goldPositive = "Positive",
    goldNegative = "Negative",
    test1 = "Test1",
    test1Positive = "Positive",
    test1Negative = "Negative",
    test2 = "Test2",
    test2Positive = "Positive",
    test2Negative = "Negative",
    excludeIndeterminate = TRUE,
    test3Positive = NULL,
    test3Negative = NULL
  )
  expect_s3_class(result_exclude, "decisioncompareResults")
})

test_that("decisioncompare waits silently until required variables are selected", {
  # jamovi analyses do not error on an incomplete selection -- .hasRequiredVars()
  # returns FALSE and .run() returns, leaving the results empty until the user
  # finishes choosing. The assertion is therefore "nothing was computed", not
  # "an error was thrown".

  # Fewer than two tests: nothing to compare
  expect_no_error(
    res1 <- call_decisioncompare(
      data = decisioncompare_small,
      gold = "GoldStandard",
      goldPositive = "Positive",
      goldNegative = "Negative",
      test1 = "Test1",
      test1Positive = "Positive",
      test1Negative = "Negative",
      test2Positive = NULL,
      test2Negative = NULL,
      test3Positive = NULL,
      test3Negative = NULL
    )
  )
  # library-audit 2026-10-23 meddecide [MEDIUM] DONE: rows come from .init(); nothing is computed
  expect_true(all(is.na(res1$comparisonTable$asDF$Sens)))
  # No gold standard: nothing to compare against
  expect_no_error(
    res2 <- call_decisioncompare(
      data = decisioncompare_small,
      test1 = "Test1",
      test1Positive = "Positive",
      test1Negative = "Negative",
      test2 = "Test2",
      test2Positive = "Positive",
      test2Negative = "Negative",
      goldPositive = NULL,
      goldNegative = NULL,
      test3Positive = NULL,
      test3Negative = NULL
    )
  )
  # library-audit 2026-10-23 meddecide [MEDIUM] DONE: rows come from .init(); nothing is computed
  expect_true(all(is.na(res2$comparisonTable$asDF$Sens)))
  # ...but a complete selection does produce results
  res3 <- call_decisioncompare(
    data = decisioncompare_small,
    gold = "GoldStandard", goldPositive = "Positive", goldNegative = "Negative",
    test1 = "Test1", test1Positive = "Positive", test1Negative = "Negative",
    test2 = "Test2", test2Positive = "Positive", test2Negative = "Negative",
    test3 = NULL, test3Positive = NULL, test3Negative = NULL, stratify = NULL
  )
  expect_gt(res3$comparisonTable$rowCount, 0L)
})

test_that("decisioncompare validates prevalence bounds", {
  # Prevalence too low
  expect_error(
    call_decisioncompare(
      data = decisioncompare_small,
      gold = "GoldStandard",
      goldPositive = "Positive",
      goldNegative = "Negative",
      test1 = "Test1",
      test1Positive = "Positive",
      test1Negative = "Negative",
      test2 = "Test2",
      test2Positive = "Positive",
      test2Negative = "Negative",
      pp = TRUE,
      # Bounds are 1e-9 .. 1-1e-9 since the explicit-mapping release (rare-disease screening
      # prevalence such as 0.0005 is valid); 0 is out of range.
      pprob = 0,
      test3Positive = NULL,
      test3Negative = NULL
    ),
    regexp = "prevalence|probability|pprob|1e-09|0.999",
    ignore.case = TRUE
  )

  # Prevalence too high
  expect_error(
    call_decisioncompare(
      data = decisioncompare_small,
      gold = "GoldStandard",
      goldPositive = "Positive",
      goldNegative = "Negative",
      test1 = "Test1",
      test1Positive = "Positive",
      test1Negative = "Negative",
      test2 = "Test2",
      test2Positive = "Positive",
      test2Negative = "Negative",
      pp = TRUE,
      pprob = 1.0,  # Above maximum (0.999)
      test3Positive = NULL,
      test3Negative = NULL
    ),
    regexp = "prevalence|probability|0.001|0.999",
    ignore.case = TRUE
  )
})

test_that("decisioncompare handles extreme prevalence values", {
  # Very low prevalence (near minimum)
  result_low <- call_decisioncompare(
    data = decisioncompare_small,
    gold = "GoldStandard",
    goldPositive = "Positive",
    goldNegative = "Negative",
    test1 = "Test1",
    test1Positive = "Positive",
    test1Negative = "Negative",
    test2 = "Test2",
    test2Positive = "Positive",
    test2Negative = "Negative",
    pp = TRUE,
    pprob = 0.001,
    test3Positive = NULL,
    test3Negative = NULL
  )
  expect_no_error(result_low)

  # Very high prevalence (near maximum)
  result_high <- call_decisioncompare(
    data = decisioncompare_small,
    gold = "GoldStandard",
    goldPositive = "Positive",
    goldNegative = "Negative",
    test1 = "Test1",
    test1Positive = "Positive",
    test1Negative = "Negative",
    test2 = "Test2",
    test2Positive = "Positive",
    test2Negative = "Negative",
    pp = TRUE,
    pprob = 0.999,
    test3Positive = NULL,
    test3Negative = NULL
  )
  expect_no_error(result_high)
})

test_that("decisioncompare handles all output options with small sample", {
  result <- call_decisioncompare(
    data = decisioncompare_small,
    gold = "GoldStandard",
    goldPositive = "Positive",
    goldNegative = "Negative",
    test1 = "Test1",
    test1Positive = "Positive",
    test1Negative = "Negative",
    test2 = "Test2",
    test2Positive = "Positive",
    test2Negative = "Negative",
    ci = TRUE,
    plot = TRUE,
    statComp = TRUE,
    od = TRUE,
    test3Positive = NULL,
    test3Negative = NULL
  )
  expect_s3_class(result, "decisioncompareResults")
})
