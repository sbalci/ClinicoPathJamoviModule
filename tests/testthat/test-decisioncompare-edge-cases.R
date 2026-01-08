# ═══════════════════════════════════════════════════════════
# Edge Cases Tests: decisioncompare
# ═══════════════════════════════════════════════════════════
library(testthat)
library(ClinicoPath)
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
  result <- decisioncompare(
    data = decisioncompare_small,
    gold = "GoldStandard",
    goldPositive = "Positive",
    test1 = "Test1",
    test1Positive = "Positive",
    test2 = "Test2",
    test2Positive = "Positive"
  )
  expect_s3_class(result, "decisioncompareClass")
  expect_true(nrow(decisioncompare_small) == 30)
})

test_that("decisioncompare handles large datasets efficiently", {
  result <- decisioncompare(
    data = decisioncompare_large,
    gold = "GoldStandard",
    goldPositive = "Positive",
    test1 = "Test1",
    test1Positive = "Positive",
    test2 = "Test2",
    test2Positive = "Positive",
    test3 = "Test3",
    test3Positive = "Positive"
  )
  expect_s3_class(result, "decisioncompareClass")
  expect_true(nrow(decisioncompare_large) == 500)
})

test_that("decisioncompare handles perfect test performance", {
  result <- decisioncompare(
    data = decisioncompare_perfect,
    gold = "GoldStandard",
    goldPositive = "Positive",
    test1 = "PerfectTest",
    test1Positive = "Positive",
    test2 = "ImperfectTest",
    test2Positive = "Positive"
  )
  expect_s3_class(result, "decisioncompareClass")
})

test_that("decisioncompare handles poor test performance", {
  result <- decisioncompare(
    data = decisioncompare_poor,
    gold = "GoldStandard",
    goldPositive = "Positive",
    test1 = "PoorTest1",
    test1Positive = "Positive",
    test2 = "PoorTest2",
    test2Positive = "Positive"
  )
  expect_s3_class(result, "decisioncompareClass")
})

test_that("decisioncompare handles rare disease prevalence", {
  result <- decisioncompare(
    data = decisioncompare_rare,
    gold = "GoldStandard",
    goldPositive = "Positive",
    test1 = "Test1",
    test1Positive = "Positive",
    test2 = "Test2",
    test2Positive = "Positive"
  )
  expect_s3_class(result, "decisioncompareClass")
  # Check prevalence is low
  prevalence <- mean(decisioncompare_rare$GoldStandard == "Positive")
  expect_lt(prevalence, 0.10)
})

test_that("decisioncompare handles common disease prevalence", {
  result <- decisioncompare(
    data = decisioncompare_common,
    gold = "GoldStandard",
    goldPositive = "Positive",
    test1 = "Test1",
    test1Positive = "Positive",
    test2 = "Test2",
    test2Positive = "Positive"
  )
  expect_s3_class(result, "decisioncompareClass")
  # Check prevalence is high
  prevalence <- mean(decisioncompare_common$GoldStandard == "Positive")
  expect_gt(prevalence, 0.50)
})

test_that("decisioncompare handles identical tests", {
  result <- decisioncompare(
    data = decisioncompare_identical,
    gold = "GoldStandard",
    goldPositive = "Positive",
    test1 = "Test1",
    test1Positive = "Positive",
    test2 = "Test2",
    test2Positive = "Positive"
  )
  # Tests are identical - should complete without error
  expect_s3_class(result, "decisioncompareClass")
})

test_that("decisioncompare handles missing data", {
  result <- decisioncompare(
    data = decisioncompare_missing,
    gold = "GoldStandard",
    goldPositive = "Positive",
    test1 = "Test1",
    test1Positive = "Positive",
    test2 = "Test2",
    test2Positive = "Positive",
    test3 = "Test3",
    test3Positive = "Positive"
  )
  # Should handle with warnings or listwise deletion
  expect_s3_class(result, "decisioncompareClass")
  expect_true(any(is.na(decisioncompare_missing$GoldStandard)))
})

test_that("decisioncompare handles indeterminate results", {
  # With indeterminate results included (treated as negative)
  result_include <- decisioncompare(
    data = decisioncompare_indeterminate,
    gold = "GoldStandard",
    goldPositive = "Positive",
    test1 = "Test1",
    test1Positive = "Positive",
    test2 = "Test2",
    test2Positive = "Positive",
    excludeIndeterminate = FALSE
  )
  expect_s3_class(result_include, "decisioncompareClass")

  # With indeterminate results excluded
  result_exclude <- decisioncompare(
    data = decisioncompare_indeterminate,
    gold = "GoldStandard",
    goldPositive = "Positive",
    test1 = "Test1",
    test1Positive = "Positive",
    test2 = "Test2",
    test2Positive = "Positive",
    excludeIndeterminate = TRUE
  )
  expect_s3_class(result_exclude, "decisioncompareClass")
})

test_that("decisioncompare validates required arguments", {
  # Missing test2
  expect_error(
    decisioncompare(
      data = decisioncompare_small,
      gold = "GoldStandard",
      goldPositive = "Positive",
      test1 = "Test1",
      test1Positive = "Positive"
    ),
    regexp = "test2|required|missing",
    ignore.case = TRUE
  )

  # Missing gold standard
  expect_error(
    decisioncompare(
      data = decisioncompare_small,
      test1 = "Test1",
      test1Positive = "Positive",
      test2 = "Test2",
      test2Positive = "Positive"
    ),
    regexp = "gold|required|missing",
    ignore.case = TRUE
  )
})

test_that("decisioncompare validates prevalence bounds", {
  # Prevalence too low
  expect_error(
    decisioncompare(
      data = decisioncompare_small,
      gold = "GoldStandard",
      goldPositive = "Positive",
      test1 = "Test1",
      test1Positive = "Positive",
      test2 = "Test2",
      test2Positive = "Positive",
      pp = TRUE,
      pprob = 0.0005  # Below minimum (0.001)
    ),
    regexp = "prevalence|probability|0.001|0.999",
    ignore.case = TRUE
  )

  # Prevalence too high
  expect_error(
    decisioncompare(
      data = decisioncompare_small,
      gold = "GoldStandard",
      goldPositive = "Positive",
      test1 = "Test1",
      test1Positive = "Positive",
      test2 = "Test2",
      test2Positive = "Positive",
      pp = TRUE,
      pprob = 1.0  # Above maximum (0.999)
    ),
    regexp = "prevalence|probability|0.001|0.999",
    ignore.case = TRUE
  )
})

test_that("decisioncompare handles extreme prevalence values", {
  # Very low prevalence (near minimum)
  result_low <- decisioncompare(
    data = decisioncompare_small,
    gold = "GoldStandard",
    goldPositive = "Positive",
    test1 = "Test1",
    test1Positive = "Positive",
    test2 = "Test2",
    test2Positive = "Positive",
    pp = TRUE,
    pprob = 0.001
  )
  expect_no_error(result_low)

  # Very high prevalence (near maximum)
  result_high <- decisioncompare(
    data = decisioncompare_small,
    gold = "GoldStandard",
    goldPositive = "Positive",
    test1 = "Test1",
    test1Positive = "Positive",
    test2 = "Test2",
    test2Positive = "Positive",
    pp = TRUE,
    pprob = 0.999
  )
  expect_no_error(result_high)
})

test_that("decisioncompare handles all output options with small sample", {
  result <- decisioncompare(
    data = decisioncompare_small,
    gold = "GoldStandard",
    goldPositive = "Positive",
    test1 = "Test1",
    test1Positive = "Positive",
    test2 = "Test2",
    test2Positive = "Positive",
    ci = TRUE,
    plot = TRUE,
    statComp = TRUE,
    od = TRUE
  )
  expect_s3_class(result, "decisioncompareClass")
})
