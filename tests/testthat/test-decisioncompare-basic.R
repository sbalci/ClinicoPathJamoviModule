# ═══════════════════════════════════════════════════════════
# Basic Tests: decisioncompare
# ═══════════════════════════════════════════════════════════
library(testthat)
data(decisioncompare_test, package = "ClinicoPath")
data(decisioncompare_threetest, package = "ClinicoPath")
data(decisioncompare_imaging, package = "ClinicoPath")
data(decisioncompare_screening, package = "ClinicoPath")

test_that("decisioncompare creates proper class", {
  result <- call_decisioncompare(
    data = decisioncompare_test,
    gold = "GoldStandard",
    goldPositive = "Positive",
    goldNegative = NULL,
    test1 = "Test1",
    test1Positive = "Positive",
    test1Negative = NULL,
    test2 = "Test2",
    test2Positive = "Positive",
    test2Negative = NULL,
    test3Positive = NULL,
    test3Negative = NULL
  )
  expect_s3_class(result, "decisioncompareResults")
})

test_that("decisioncompare handles basic two-test comparison", {
  result <- call_decisioncompare(
    data = decisioncompare_test,
    gold = "GoldStandard",
    goldPositive = "Positive",
    goldNegative = NULL,
    test1 = "Test1",
    test1Positive = "Positive",
    test1Negative = NULL,
    test2 = "Test2",
    test2Positive = "Positive",
    test2Negative = NULL,
    test3Positive = NULL,
    test3Negative = NULL
  )
  expect_s3_class(result, "decisioncompareResults")
  expect_true(length(result$results) > 0)
})

test_that("decisioncompare handles three-test comparison", {
  result <- call_decisioncompare(
    data = decisioncompare_threetest,
    gold = "GoldStandard",
    goldPositive = "Positive",
    goldNegative = NULL,
    test1 = "Test1",
    test1Positive = "Positive",
    test1Negative = NULL,
    test2 = "Test2",
    test2Positive = "Positive",
    test2Negative = NULL,
    test3 = "Test3",
    test3Positive = "Positive",
    test3Negative = NULL
  )
  expect_s3_class(result, "decisioncompareResults")
})

test_that("decisioncompare handles imaging comparison", {
  result <- call_decisioncompare(
    data = decisioncompare_imaging,
    gold = "Pathology",
    goldPositive = "Malignant",
    goldNegative = NULL,
    test1 = "CT_Scan",
    test1Positive = "Abnormal",
    test1Negative = NULL,
    test2 = "MRI",
    test2Positive = "Abnormal",
    test2Negative = NULL,
    test3 = "Biomarker",
    test3Positive = "Elevated",
    test3Negative = NULL
  )
  expect_s3_class(result, "decisioncompareResults")
})

test_that("decisioncompare handles screening vs diagnostic tests", {
  result <- call_decisioncompare(
    data = decisioncompare_screening,
    gold = "Biopsy",
    goldPositive = "Positive",
    goldNegative = NULL,
    test1 = "ScreeningTest",
    test1Positive = "Positive",
    test1Negative = NULL,
    test2 = "DiagnosticTest",
    test2Positive = "Positive",
    test2Negative = NULL,
    test3Positive = NULL,
    test3Negative = NULL
  )
  expect_s3_class(result, "decisioncompareResults")
})

test_that("decisioncompare handles confidence intervals", {
  result <- call_decisioncompare(
    data = decisioncompare_test,
    gold = "GoldStandard",
    goldPositive = "Positive",
    goldNegative = NULL,
    test1 = "Test1",
    test1Positive = "Positive",
    test1Negative = NULL,
    test2 = "Test2",
    test2Positive = "Positive",
    test2Negative = NULL,
    ci = TRUE,
    test3Positive = NULL,
    test3Negative = NULL
  )
  expect_no_error(result)
})

test_that("decisioncompare handles comparison plot", {
  result <- call_decisioncompare(
    data = decisioncompare_test,
    gold = "GoldStandard",
    goldPositive = "Positive",
    goldNegative = NULL,
    test1 = "Test1",
    test1Positive = "Positive",
    test1Negative = NULL,
    test2 = "Test2",
    test2Positive = "Positive",
    test2Negative = NULL,
    plot = TRUE,
    test3Positive = NULL,
    test3Negative = NULL
  )
  expect_no_error(result)
})

test_that("decisioncompare handles radar plot", {
  result <- call_decisioncompare(
    data = decisioncompare_threetest,
    gold = "GoldStandard",
    goldPositive = "Positive",
    goldNegative = NULL,
    test1 = "Test1",
    test1Positive = "Positive",
    test1Negative = NULL,
    test2 = "Test2",
    test2Positive = "Positive",
    test2Negative = NULL,
    test3 = "Test3",
    test3Positive = "Positive",
    test3Negative = NULL,
    radarplot = TRUE
  )
  expect_no_error(result)
})

test_that("decisioncompare handles statistical comparison", {
  result <- call_decisioncompare(
    data = decisioncompare_test,
    gold = "GoldStandard",
    goldPositive = "Positive",
    goldNegative = NULL,
    test1 = "Test1",
    test1Positive = "Positive",
    test1Negative = NULL,
    test2 = "Test2",
    test2Positive = "Positive",
    test2Negative = NULL,
    statComp = TRUE,
    test3Positive = NULL,
    test3Negative = NULL
  )
  expect_s3_class(result, "decisioncompareResults")
})

test_that("decisioncompare handles original data display", {
  result <- call_decisioncompare(
    data = decisioncompare_test,
    gold = "GoldStandard",
    goldPositive = "Positive",
    goldNegative = NULL,
    test1 = "Test1",
    test1Positive = "Positive",
    test1Negative = NULL,
    test2 = "Test2",
    test2Positive = "Positive",
    test2Negative = NULL,
    od = TRUE,
    test3Positive = NULL,
    test3Negative = NULL
  )
  expect_no_error(result)
})
