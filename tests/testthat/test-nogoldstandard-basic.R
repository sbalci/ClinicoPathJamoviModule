# ═══════════════════════════════════════════════════════════
# Basic Tests: nogoldstandard
# ═══════════════════════════════════════════════════════════
library(testthat)
library(ClinicoPath)
data(nogoldstandard_test, package = "ClinicoPath")
data(nogoldstandard_pathology, package = "ClinicoPath")
data(nogoldstandard_tumormarker, package = "ClinicoPath")
data(nogoldstandard_screening, package = "ClinicoPath")

test_that("nogoldstandard creates proper class", {
  result <- nogoldstandard(
    data = nogoldstandard_test,
    test1 = "Test1",
    test1Positive = "Positive",
    test2 = "Test2",
    test2Positive = "Positive"
  )
  expect_s3_class(result, "nogoldstandardClass")
})

test_that("nogoldstandard handles basic two-test analysis", {
  result <- nogoldstandard(
    data = nogoldstandard_test,
    test1 = "Test1",
    test1Positive = "Positive",
    test2 = "Test2",
    test2Positive = "Positive",
    method = "all_positive"
  )
  expect_s3_class(result, "nogoldstandardClass")
  expect_true(length(result$results) > 0)
})

test_that("nogoldstandard handles latent class analysis", {
  result <- nogoldstandard(
    data = nogoldstandard_test,
    test1 = "Test1",
    test1Positive = "Positive",
    test2 = "Test2",
    test2Positive = "Positive",
    method = "latent_class"
  )
  expect_s3_class(result, "nogoldstandardClass")
})

test_that("nogoldstandard handles three-test pathology data", {
  result <- nogoldstandard(
    data = nogoldstandard_pathology,
    test1 = "Pathologist1",
    test1Positive = "Malignant",
    test2 = "Pathologist2",
    test2Positive = "Malignant",
    test3 = "Pathologist3",
    test3Positive = "Malignant",
    method = "latent_class"
  )
  expect_s3_class(result, "nogoldstandardClass")
})

test_that("nogoldstandard handles four-test tumor marker data", {
  result <- nogoldstandard(
    data = nogoldstandard_tumormarker,
    test1 = "CA125",
    test1Positive = "Elevated",
    test2 = "HE4",
    test2Positive = "Elevated",
    test3 = "CEA",
    test3Positive = "Elevated",
    test4 = "AFP",
    test4Positive = "Elevated",
    method = "composite"
  )
  expect_s3_class(result, "nogoldstandardClass")
})

test_that("nogoldstandard handles five-test screening data", {
  result <- nogoldstandard(
    data = nogoldstandard_screening,
    test1 = "Imaging",
    test1Positive = "Abnormal",
    test2 = "ClinicalExam",
    test2Positive = "Abnormal",
    test3 = "Biomarker",
    test3Positive = "Abnormal",
    test4 = "Questionnaire",
    test4Positive = "Positive",
    test5 = "AI_Algorithm",
    test5Positive = "Positive",
    method = "all_positive"
  )
  expect_s3_class(result, "nogoldstandardClass")
})

test_that("nogoldstandard handles composite reference method", {
  result <- nogoldstandard(
    data = nogoldstandard_test,
    test1 = "Test1",
    test1Positive = "Positive",
    test2 = "Test2",
    test2Positive = "Positive",
    method = "composite"
  )
  expect_s3_class(result, "nogoldstandardClass")
})

test_that("nogoldstandard handles all positive method", {
  result <- nogoldstandard(
    data = nogoldstandard_pathology,
    test1 = "Pathologist1",
    test1Positive = "Malignant",
    test2 = "Pathologist2",
    test2Positive = "Malignant",
    test3 = "Pathologist3",
    test3Positive = "Malignant",
    method = "all_positive"
  )
  expect_no_error(result)
})

test_that("nogoldstandard handles any positive method", {
  result <- nogoldstandard(
    data = nogoldstandard_pathology,
    test1 = "Pathologist1",
    test1Positive = "Malignant",
    test2 = "Pathologist2",
    test2Positive = "Malignant",
    test3 = "Pathologist3",
    test3Positive = "Malignant",
    method = "any_positive"
  )
  expect_no_error(result)
})

test_that("nogoldstandard handles Bayesian analysis method", {
  result <- nogoldstandard(
    data = nogoldstandard_test,
    test1 = "Test1",
    test1Positive = "Positive",
    test2 = "Test2",
    test2Positive = "Positive",
    method = "bayesian"
  )
  expect_s3_class(result, "nogoldstandardClass")
})
