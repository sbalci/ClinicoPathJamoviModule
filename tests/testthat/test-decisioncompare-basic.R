# ═══════════════════════════════════════════════════════════
# Basic Tests: decisioncompare
# ═══════════════════════════════════════════════════════════
library(testthat)
library(ClinicoPath)
data(decisioncompare_test, package = "ClinicoPath")
data(decisioncompare_threetest, package = "ClinicoPath")
data(decisioncompare_imaging, package = "ClinicoPath")
data(decisioncompare_screening, package = "ClinicoPath")

test_that("decisioncompare creates proper class", {
  result <- decisioncompare(
    data = decisioncompare_test,
    gold = "GoldStandard",
    goldPositive = "Positive",
    test1 = "Test1",
    test1Positive = "Positive",
    test2 = "Test2",
    test2Positive = "Positive"
  )
  expect_s3_class(result, "decisioncompareClass")
})

test_that("decisioncompare handles basic two-test comparison", {
  result <- decisioncompare(
    data = decisioncompare_test,
    gold = "GoldStandard",
    goldPositive = "Positive",
    test1 = "Test1",
    test1Positive = "Positive",
    test2 = "Test2",
    test2Positive = "Positive"
  )
  expect_s3_class(result, "decisioncompareClass")
  expect_true(length(result$results) > 0)
})

test_that("decisioncompare handles three-test comparison", {
  result <- decisioncompare(
    data = decisioncompare_threetest,
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
})

test_that("decisioncompare handles imaging comparison", {
  result <- decisioncompare(
    data = decisioncompare_imaging,
    gold = "Pathology",
    goldPositive = "Malignant",
    test1 = "CT_Scan",
    test1Positive = "Abnormal",
    test2 = "MRI",
    test2Positive = "Abnormal",
    test3 = "Biomarker",
    test3Positive = "Elevated"
  )
  expect_s3_class(result, "decisioncompareClass")
})

test_that("decisioncompare handles screening vs diagnostic tests", {
  result <- decisioncompare(
    data = decisioncompare_screening,
    gold = "Biopsy",
    goldPositive = "Positive",
    test1 = "ScreeningTest",
    test1Positive = "Positive",
    test2 = "DiagnosticTest",
    test2Positive = "Positive"
  )
  expect_s3_class(result, "decisioncompareClass")
})

test_that("decisioncompare handles confidence intervals", {
  result <- decisioncompare(
    data = decisioncompare_test,
    gold = "GoldStandard",
    goldPositive = "Positive",
    test1 = "Test1",
    test1Positive = "Positive",
    test2 = "Test2",
    test2Positive = "Positive",
    ci = TRUE
  )
  expect_no_error(result)
})

test_that("decisioncompare handles comparison plot", {
  result <- decisioncompare(
    data = decisioncompare_test,
    gold = "GoldStandard",
    goldPositive = "Positive",
    test1 = "Test1",
    test1Positive = "Positive",
    test2 = "Test2",
    test2Positive = "Positive",
    plot = TRUE
  )
  expect_no_error(result)
})

test_that("decisioncompare handles radar plot", {
  result <- decisioncompare(
    data = decisioncompare_threetest,
    gold = "GoldStandard",
    goldPositive = "Positive",
    test1 = "Test1",
    test1Positive = "Positive",
    test2 = "Test2",
    test2Positive = "Positive",
    test3 = "Test3",
    test3Positive = "Positive",
    radarplot = TRUE
  )
  expect_no_error(result)
})

test_that("decisioncompare handles statistical comparison", {
  result <- decisioncompare(
    data = decisioncompare_test,
    gold = "GoldStandard",
    goldPositive = "Positive",
    test1 = "Test1",
    test1Positive = "Positive",
    test2 = "Test2",
    test2Positive = "Positive",
    statComp = TRUE
  )
  expect_s3_class(result, "decisioncompareClass")
})

test_that("decisioncompare handles original data display", {
  result <- decisioncompare(
    data = decisioncompare_test,
    gold = "GoldStandard",
    goldPositive = "Positive",
    test1 = "Test1",
    test1Positive = "Positive",
    test2 = "Test2",
    test2Positive = "Positive",
    od = TRUE
  )
  expect_no_error(result)
})
