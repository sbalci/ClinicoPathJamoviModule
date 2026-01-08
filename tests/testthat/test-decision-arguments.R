# ═══════════════════════════════════════════════════════════
# Argument Tests: decision
# ═══════════════════════════════════════════════════════════
library(testthat)
library(ClinicoPath)
data(decision_test, package = "ClinicoPath")
data(decision_screening, package = "ClinicoPath")

test_that("decision respects pp parameter", {
  # With custom prevalence
  result_custom <- decision(
    data = decision_test,
    gold = "GoldStandard",
    goldPositive = "Positive",
    newtest = "NewTest",
    testPositive = "Positive",
    pp = TRUE,
    pprob = 0.10
  )
  expect_no_error(result_custom)

  # With dataset prevalence
  result_default <- decision(
    data = decision_test,
    gold = "GoldStandard",
    goldPositive = "Positive",
    newtest = "NewTest",
    testPositive = "Positive",
    pp = FALSE
  )
  expect_no_error(result_default)
})

test_that("decision respects different prevalence values", {
  # Low prevalence (screening)
  result_low <- decision(
    data = decision_test,
    gold = "GoldStandard",
    goldPositive = "Positive",
    newtest = "NewTest",
    testPositive = "Positive",
    pp = TRUE,
    pprob = 0.05
  )
  expect_no_error(result_low)

  # High prevalence (clinical)
  result_high <- decision(
    data = decision_test,
    gold = "GoldStandard",
    goldPositive = "Positive",
    newtest = "NewTest",
    testPositive = "Positive",
    pp = TRUE,
    pprob = 0.70
  )
  expect_no_error(result_high)
})

test_that("decision respects ci parameter", {
  # With CI
  result_ci <- decision(
    data = decision_test,
    gold = "GoldStandard",
    goldPositive = "Positive",
    newtest = "NewTest",
    testPositive = "Positive",
    ci = TRUE
  )
  expect_no_error(result_ci)

  # Without CI
  result_no_ci <- decision(
    data = decision_test,
    gold = "GoldStandard",
    goldPositive = "Positive",
    newtest = "NewTest",
    testPositive = "Positive",
    ci = FALSE
  )
  expect_no_error(result_no_ci)
})

test_that("decision respects od parameter", {
  # Show original data
  result_od <- decision(
    data = decision_test,
    gold = "GoldStandard",
    goldPositive = "Positive",
    newtest = "NewTest",
    testPositive = "Positive",
    od = TRUE
  )
  expect_no_error(result_od)

  # Hide original data
  result_no_od <- decision(
    data = decision_test,
    gold = "GoldStandard",
    goldPositive = "Positive",
    newtest = "NewTest",
    testPositive = "Positive",
    od = FALSE
  )
  expect_no_error(result_no_od)
})

test_that("decision respects fnote parameter", {
  result <- decision(
    data = decision_test,
    gold = "GoldStandard",
    goldPositive = "Positive",
    newtest = "NewTest",
    testPositive = "Positive",
    fnote = TRUE
  )
  expect_no_error(result)
})

test_that("decision respects fagan parameter", {
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

test_that("decision respects showNaturalLanguage parameter", {
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

test_that("decision respects showClinicalInterpretation parameter", {
  result <- decision(
    data = decision_test,
    gold = "GoldStandard",
    goldPositive = "Positive",
    newtest = "NewTest",
    testPositive = "Positive",
    showClinicalInterpretation = TRUE
  )
  expect_no_error(result)
})

test_that("decision respects showMisclassified with maxCasesShow", {
  result <- decision(
    data = decision_test,
    gold = "GoldStandard",
    goldPositive = "Positive",
    newtest = "NewTest",
    testPositive = "Positive",
    showMisclassified = TRUE,
    maxCasesShow = 20
  )
  expect_no_error(result)
})

test_that("decision handles all options combined", {
  result <- decision(
    data = decision_screening,
    gold = "Biopsy",
    goldPositive = "Malignant",
    newtest = "ScreeningTest",
    testPositive = "Positive",
    pp = TRUE,
    pprob = 0.05,
    ci = TRUE,
    od = TRUE,
    fnote = TRUE,
    fagan = TRUE,
    showNaturalLanguage = TRUE,
    showClinicalInterpretation = TRUE
  )
  expect_no_error(result)
})
