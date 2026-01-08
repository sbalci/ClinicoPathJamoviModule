# ═══════════════════════════════════════════════════════════
# Edge Cases Tests: decision
# ═══════════════════════════════════════════════════════════
library(testthat)
library(ClinicoPath)
data(decision_small, package = "ClinicoPath")
data(decision_large, package = "ClinicoPath")
data(decision_perfect, package = "ClinicoPath")
data(decision_poor, package = "ClinicoPath")
data(decision_rare, package = "ClinicoPath")
data(decision_common, package = "ClinicoPath")
data(decision_missing, package = "ClinicoPath")
data(decision_multilevel, package = "ClinicoPath")

test_that("decision handles small datasets", {
  result <- decision(
    data = decision_small,
    gold = "GoldStandard",
    goldPositive = "Positive",
    newtest = "NewTest",
    testPositive = "Positive"
  )
  expect_s3_class(result, "decisionClass")
  expect_true(nrow(decision_small) == 30)
})

test_that("decision handles large datasets efficiently", {
  result <- decision(
    data = decision_large,
    gold = "GoldStandard",
    goldPositive = "Positive",
    newtest = "NewTest",
    testPositive = "Positive"
  )
  expect_s3_class(result, "decisionClass")
  expect_true(nrow(decision_large) == 500)
})

test_that("decision handles perfect test performance", {
  result <- decision(
    data = decision_perfect,
    gold = "GoldStandard",
    goldPositive = "Positive",
    newtest = "PerfectTest",
    testPositive = "Positive"
  )
  expect_s3_class(result, "decisionClass")
})

test_that("decision handles poor test performance", {
  result <- decision(
    data = decision_poor,
    gold = "GoldStandard",
    goldPositive = "Positive",
    newtest = "PoorTest",
    testPositive = "Positive"
  )
  expect_s3_class(result, "decisionClass")
})

test_that("decision handles rare disease prevalence", {
  result <- decision(
    data = decision_rare,
    gold = "GoldStandard",
    goldPositive = "Positive",
    newtest = "NewTest",
    testPositive = "Positive"
  )
  expect_s3_class(result, "decisionClass")
  # Check prevalence is low
  prevalence <- mean(decision_rare$GoldStandard == "Positive")
  expect_lt(prevalence, 0.05)
})

test_that("decision handles common disease prevalence", {
  result <- decision(
    data = decision_common,
    gold = "GoldStandard",
    goldPositive = "Positive",
    newtest = "NewTest",
    testPositive = "Positive"
  )
  expect_s3_class(result, "decisionClass")
  # Check prevalence is high
  prevalence <- mean(decision_common$GoldStandard == "Positive")
  expect_gt(prevalence, 0.60)
})

test_that("decision handles missing data", {
  result <- decision(
    data = decision_missing,
    gold = "GoldStandard",
    goldPositive = "Positive",
    newtest = "NewTest",
    testPositive = "Positive"
  )
  expect_s3_class(result, "decisionClass")
  expect_true(any(is.na(decision_missing$GoldStandard)) ||
              any(is.na(decision_missing$NewTest)))
})

test_that("decision handles multilevel variables", {
  # With indeterminate level treated as negative
  result <- decision(
    data = decision_multilevel,
    gold = "GoldStandard",
    goldPositive = "Positive",
    newtest = "NewTest",
    testPositive = "Positive"
  )
  expect_s3_class(result, "decisionClass")
})

test_that("decision validates required arguments", {
  # Missing newtest
  expect_error(
    decision(
      data = decision_small,
      gold = "GoldStandard",
      goldPositive = "Positive"
    ),
    regexp = "newtest|required|missing",
    ignore.case = TRUE
  )

  # Missing gold standard
  expect_error(
    decision(
      data = decision_small,
      newtest = "NewTest",
      testPositive = "Positive"
    ),
    regexp = "gold|required|missing",
    ignore.case = TRUE
  )
})

test_that("decision validates prevalence bounds", {
  # Prevalence too low
  expect_error(
    decision(
      data = decision_small,
      gold = "GoldStandard",
      goldPositive = "Positive",
      newtest = "NewTest",
      testPositive = "Positive",
      pp = TRUE,
      pprob = 0.0005  # Below minimum (0.001)
    ),
    regexp = "prevalence|probability|0.001|0.999",
    ignore.case = TRUE
  )

  # Prevalence too high
  expect_error(
    decision(
      data = decision_small,
      gold = "GoldStandard",
      goldPositive = "Positive",
      newtest = "NewTest",
      testPositive = "Positive",
      pp = TRUE,
      pprob = 1.0  # Above maximum (0.999)
    ),
    regexp = "prevalence|probability|0.001|0.999",
    ignore.case = TRUE
  )
})

test_that("decision handles extreme prevalence values", {
  # Very low prevalence (near minimum)
  result_low <- decision(
    data = decision_small,
    gold = "GoldStandard",
    goldPositive = "Positive",
    newtest = "NewTest",
    testPositive = "Positive",
    pp = TRUE,
    pprob = 0.001
  )
  expect_no_error(result_low)

  # Very high prevalence (near maximum)
  result_high <- decision(
    data = decision_small,
    gold = "GoldStandard",
    goldPositive = "Positive",
    newtest = "NewTest",
    testPositive = "Positive",
    pp = TRUE,
    pprob = 0.999
  )
  expect_no_error(result_high)
})

test_that("decision handles maxCasesShow bounds", {
  # Minimum value
  result_min <- decision(
    data = decision_small,
    gold = "GoldStandard",
    goldPositive = "Positive",
    newtest = "NewTest",
    testPositive = "Positive",
    showMisclassified = TRUE,
    maxCasesShow = 10
  )
  expect_no_error(result_min)

  # Maximum value
  result_max <- decision(
    data = decision_large,
    gold = "GoldStandard",
    goldPositive = "Positive",
    newtest = "NewTest",
    testPositive = "Positive",
    showMisclassified = TRUE,
    maxCasesShow = 500
  )
  expect_no_error(result_max)
})

test_that("decision handles all output options with small sample", {
  result <- decision(
    data = decision_small,
    gold = "GoldStandard",
    goldPositive = "Positive",
    newtest = "NewTest",
    testPositive = "Positive",
    ci = TRUE,
    od = TRUE,
    fnote = TRUE,
    fagan = TRUE,
    showNaturalLanguage = TRUE,
    showClinicalInterpretation = TRUE,
    showMisclassified = TRUE
  )
  expect_s3_class(result, "decisionClass")
})
