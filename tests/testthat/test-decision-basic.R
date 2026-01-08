# ═══════════════════════════════════════════════════════════
# Basic Tests: decision
# ═══════════════════════════════════════════════════════════
library(testthat)
library(ClinicoPath)
data(decision_test, package = "ClinicoPath")
data(decision_screening, package = "ClinicoPath")
data(decision_diagnostic, package = "ClinicoPath")
data(decision_biomarker, package = "ClinicoPath")

test_that("decision returns proper class", {
  result <- decision(
    data = decision_test,
    gold = "GoldStandard",
    goldPositive = "Positive",
    newtest = "NewTest",
    testPositive = "Positive"
  )
  expect_s3_class(result, "decisionClass")
})

test_that("decision handles basic two-level test", {
  result <- decision(
    data = decision_test,
    gold = "GoldStandard",
    goldPositive = "Positive",
    newtest = "NewTest",
    testPositive = "Positive"
  )
  expect_no_error(result)
})

test_that("decision handles screening scenario (low prevalence)", {
  result <- decision(
    data = decision_screening,
    gold = "Biopsy",
    goldPositive = "Malignant",
    newtest = "ScreeningTest",
    testPositive = "Positive"
  )
  expect_no_error(result)
})

test_that("decision handles diagnostic scenario (high prevalence)", {
  result <- decision(
    data = decision_diagnostic,
    gold = "GoldStandard",
    goldPositive = "Present",
    newtest = "ClinicalTest",
    testPositive = "Positive"
  )
  expect_no_error(result)
})

test_that("decision handles cardiac biomarker test", {
  result <- decision(
    data = decision_biomarker,
    gold = "Angiography",
    goldPositive = "MI",
    newtest = "Troponin",
    testPositive = "Elevated"
  )
  expect_no_error(result)
})

test_that("decision produces results with confidence intervals", {
  result <- decision(
    data = decision_test,
    gold = "GoldStandard",
    goldPositive = "Positive",
    newtest = "NewTest",
    testPositive = "Positive",
    ci = TRUE
  )
  expect_no_error(result)
})

test_that("decision works with custom prevalence", {
  result <- decision(
    data = decision_test,
    gold = "GoldStandard",
    goldPositive = "Positive",
    newtest = "NewTest",
    testPositive = "Positive",
    pp = TRUE,
    pprob = 0.15
  )
  expect_no_error(result)
})

test_that("decision generates Fagan nomogram", {
  result <- decision(
    data = decision_test,
    gold = "GoldStandard",
    goldPositive = "Positive",
    newtest = "NewTest",
    testPositive = "Positive",
    fagan = TRUE
  )
  expect_no_error(result)
})

test_that("decision shows natural language summary", {
  result <- decision(
    data = decision_test,
    gold = "GoldStandard",
    goldPositive = "Positive",
    newtest = "NewTest",
    testPositive = "Positive",
    showNaturalLanguage = TRUE
  )
  expect_no_error(result)
})

test_that("decision shows misclassified cases", {
  result <- decision(
    data = decision_test,
    gold = "GoldStandard",
    goldPositive = "Positive",
    newtest = "NewTest",
    testPositive = "Positive",
    showMisclassified = TRUE
  )
  expect_no_error(result)
})
