# ═══════════════════════════════════════════════════════════
# Argument Combination Tests: decisioncombine
# ═══════════════════════════════════════════════════════════
#
# Tests all valid argument combinations and option interactions
# for the decisioncombine jamovi function

library(testthat)
library(ClinicoPath)

# Load test data
data(decisioncombine_pathology, package = "ClinicoPath")
data(decisioncombine_threetest, package = "ClinicoPath")

test_that("decisioncombine shows individual test statistics", {
  result <- decisioncombine(
    data = decisioncombine_pathology,
    gold = "gold_standard",
    goldPositive = "Malignant",
    test1 = "rater1",
    test1Positive = "Positive",
    test2 = "rater2",
    test2Positive = "Positive",
    showIndividual = TRUE
  )

  expect_s3_class(result, "decisioncombineClass")
})

test_that("decisioncombine shows frequency tables", {
  result <- decisioncombine(
    data = decisioncombine_pathology,
    gold = "gold_standard",
    goldPositive = "Malignant",
    test1 = "rater1",
    test1Positive = "Positive",
    test2 = "rater2",
    test2Positive = "Positive",
    showFrequency = TRUE
  )

  expect_s3_class(result, "decisioncombineClass")
})

test_that("decisioncombine generates bar plot", {
  result <- decisioncombine(
    data = decisioncombine_pathology,
    gold = "gold_standard",
    goldPositive = "Malignant",
    test1 = "rater1",
    test1Positive = "Positive",
    test2 = "rater2",
    test2Positive = "Positive",
    showBarPlot = TRUE
  )

  expect_s3_class(result, "decisioncombineClass")
})

test_that("decisioncombine generates heatmap", {
  result <- decisioncombine(
    data = decisioncombine_pathology,
    gold = "gold_standard",
    goldPositive = "Malignant",
    test1 = "rater1",
    test1Positive = "Positive",
    test2 = "rater2",
    test2Positive = "Positive",
    showHeatmap = TRUE
  )

  expect_s3_class(result, "decisioncombineClass")
})

test_that("decisioncombine generates forest plot", {
  result <- decisioncombine(
    data = decisioncombine_pathology,
    gold = "gold_standard",
    goldPositive = "Malignant",
    test1 = "rater1",
    test1Positive = "Positive",
    test2 = "rater2",
    test2Positive = "Positive",
    showForest = TRUE
  )

  expect_s3_class(result, "decisioncombineClass")
})

test_that("decisioncombine generates decision tree", {
  result <- decisioncombine(
    data = decisioncombine_pathology,
    gold = "gold_standard",
    goldPositive = "Malignant",
    test1 = "rater1",
    test1Positive = "Positive",
    test2 = "rater2",
    test2Positive = "Positive",
    showDecisionTree = TRUE
  )

  expect_s3_class(result, "decisioncombineClass")
})

test_that("decisioncombine shows optimal pattern recommendation", {
  result <- decisioncombine(
    data = decisioncombine_pathology,
    gold = "gold_standard",
    goldPositive = "Malignant",
    test1 = "rater1",
    test1Positive = "Positive",
    test2 = "rater2",
    test2Positive = "Positive",
    showRecommendation = TRUE
  )

  expect_s3_class(result, "decisioncombineClass")
})

test_that("decisioncombine adds pattern to data", {
  result <- decisioncombine(
    data = decisioncombine_pathology,
    gold = "gold_standard",
    goldPositive = "Malignant",
    test1 = "rater1",
    test1Positive = "Positive",
    test2 = "rater2",
    test2Positive = "Positive",
    addPatternToData = TRUE
  )

  expect_s3_class(result, "decisioncombineClass")
})

test_that("decisioncombine filters by sensitivity", {
  result <- decisioncombine(
    data = decisioncombine_pathology,
    gold = "gold_standard",
    goldPositive = "Malignant",
    test1 = "rater1",
    test1Positive = "Positive",
    test2 = "rater2",
    test2Positive = "Positive",
    filterStatistic = "sens"
  )

  expect_s3_class(result, "decisioncombineClass")
})

test_that("decisioncombine filters by specificity", {
  result <- decisioncombine(
    data = decisioncombine_pathology,
    gold = "gold_standard",
    goldPositive = "Malignant",
    test1 = "rater1",
    test1Positive = "Positive",
    test2 = "rater2",
    test2Positive = "Positive",
    filterStatistic = "spec"
  )

  expect_s3_class(result, "decisioncombineClass")
})

test_that("decisioncombine filters by PPV", {
  result <- decisioncombine(
    data = decisioncombine_pathology,
    gold = "gold_standard",
    goldPositive = "Malignant",
    test1 = "rater1",
    test1Positive = "Positive",
    test2 = "rater2",
    test2Positive = "Positive",
    filterStatistic = "ppv"
  )

  expect_s3_class(result, "decisioncombineClass")
})

test_that("decisioncombine filters by Youden's J", {
  result <- decisioncombine(
    data = decisioncombine_pathology,
    gold = "gold_standard",
    goldPositive = "Malignant",
    test1 = "rater1",
    test1Positive = "Positive",
    test2 = "rater2",
    test2Positive = "Positive",
    filterStatistic = "youden"
  )

  expect_s3_class(result, "decisioncombineClass")
})

test_that("decisioncombine filters by pattern: all positive", {
  result <- decisioncombine(
    data = decisioncombine_pathology,
    gold = "gold_standard",
    goldPositive = "Malignant",
    test1 = "rater1",
    test1Positive = "Positive",
    test2 = "rater2",
    test2Positive = "Positive",
    filterPattern = "allPositive"
  )

  expect_s3_class(result, "decisioncombineClass")
})

test_that("decisioncombine filters by pattern: all negative", {
  result <- decisioncombine(
    data = decisioncombine_pathology,
    gold = "gold_standard",
    goldPositive = "Malignant",
    test1 = "rater1",
    test1Positive = "Positive",
    test2 = "rater2",
    test2Positive = "Positive",
    filterPattern = "allNegative"
  )

  expect_s3_class(result, "decisioncombineClass")
})

test_that("decisioncombine filters by pattern: parallel strategy", {
  result <- decisioncombine(
    data = decisioncombine_pathology,
    gold = "gold_standard",
    goldPositive = "Malignant",
    test1 = "rater1",
    test1Positive = "Positive",
    test2 = "rater2",
    test2Positive = "Positive",
    filterPattern = "parallel"
  )

  expect_s3_class(result, "decisioncombineClass")
})

test_that("decisioncombine filters by pattern: serial strategy", {
  result <- decisioncombine(
    data = decisioncombine_pathology,
    gold = "gold_standard",
    goldPositive = "Malignant",
    test1 = "rater1",
    test1Positive = "Positive",
    test2 = "rater2",
    test2Positive = "Positive",
    filterPattern = "serial"
  )

  expect_s3_class(result, "decisioncombineClass")
})

test_that("decisioncombine handles three-test combination", {
  result <- decisioncombine(
    data = decisioncombine_threetest,
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

test_that("decisioncombine three-test with majority rule filter", {
  result <- decisioncombine(
    data = decisioncombine_threetest,
    gold = "gold_standard",
    goldPositive = "Disease",
    test1 = "clinical_exam",
    test1Positive = "Positive",
    test2 = "lab_test",
    test2Positive = "Positive",
    test3 = "imaging",
    test3Positive = "Positive",
    filterPattern = "majority"
  )

  expect_s3_class(result, "decisioncombineClass")
})

test_that("decisioncombine combines multiple visualizations", {
  result <- decisioncombine(
    data = decisioncombine_pathology,
    gold = "gold_standard",
    goldPositive = "Malignant",
    test1 = "rater1",
    test1Positive = "Positive",
    test2 = "rater2",
    test2Positive = "Positive",
    showBarPlot = TRUE,
    showHeatmap = TRUE,
    showForest = TRUE
  )

  expect_s3_class(result, "decisioncombineClass")
})

test_that("decisioncombine combines all output options", {
  result <- decisioncombine(
    data = decisioncombine_pathology,
    gold = "gold_standard",
    goldPositive = "Malignant",
    test1 = "rater1",
    test1Positive = "Positive",
    test2 = "rater2",
    test2Positive = "Positive",
    showIndividual = TRUE,
    showFrequency = TRUE,
    showBarPlot = TRUE,
    showHeatmap = TRUE,
    showForest = TRUE,
    showDecisionTree = TRUE,
    showRecommendation = TRUE
  )

  expect_s3_class(result, "decisioncombineClass")
})

test_that("decisioncombine handles different test scenarios", {
  # Screening data
  data(decisioncombine_screening, package = "ClinicoPath")

  result_screening <- decisioncombine(
    data = decisioncombine_screening,
    gold = "gold_standard",
    goldPositive = "Disease",
    test1 = "screening_test",
    test1Positive = "Positive",
    test2 = "confirmatory_test",
    test2Positive = "Positive"
  )
  expect_s3_class(result_screening, "decisioncombineClass")

  # Imaging data
  data(decisioncombine_imaging, package = "ClinicoPath")

  result_imaging <- decisioncombine(
    data = decisioncombine_imaging,
    gold = "gold_standard",
    goldPositive = "Malignant",
    test1 = "ct_scan",
    test1Positive = "Positive",
    test2 = "mri_scan",
    test2Positive = "Positive"
  )
  expect_s3_class(result_imaging, "decisioncombineClass")
})

test_that("decisioncombine handles serial testing scenario", {
  data(decisioncombine_serial, package = "ClinicoPath")

  result <- decisioncombine(
    data = decisioncombine_serial,
    gold = "gold_standard",
    goldPositive = "Positive",
    test1 = "initial_test",
    test1Positive = "Positive",
    test2 = "repeat_test",
    test2Positive = "Positive",
    filterPattern = "serial"
  )

  expect_s3_class(result, "decisioncombineClass")
})
