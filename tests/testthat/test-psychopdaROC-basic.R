# ═══════════════════════════════════════════════════════════
# Basic Tests: psychopdaROC
# ═══════════════════════════════════════════════════════════
library(testthat)
library(ClinicoPath)
data(psychopdaROC_test, package = "ClinicoPath")
data(psychopdaROC_screening, package = "ClinicoPath")
data(psychopdaROC_cardiac, package = "ClinicoPath")
data(psychopdaROC_multibiomarker, package = "ClinicoPath")

test_that("psychopdaROC creates proper class", {
  result <- psychopdaROC(
    data = psychopdaROC_test,
    dependentVars = "biomarker",
    classVar = "disease_status"
  )
  expect_s3_class(result, "psychopdaROCClass")
})

test_that("psychopdaROC handles basic ROC analysis", {
  result <- psychopdaROC(
    data = psychopdaROC_test,
    dependentVars = "biomarker",
    classVar = "disease_status",
    positiveClass = "Disease"
  )
  expect_s3_class(result, "psychopdaROCClass")
  expect_true(length(result$results) > 0)
})

test_that("psychopdaROC handles cancer screening data", {
  result <- psychopdaROC(
    data = psychopdaROC_screening,
    dependentVars = c("psa_level", "ca125"),
    classVar = "cancer",
    positiveClass = "Cancer"
  )
  expect_s3_class(result, "psychopdaROCClass")
})

test_that("psychopdaROC handles cardiac biomarkers", {
  result <- psychopdaROC(
    data = psychopdaROC_cardiac,
    dependentVars = c("troponin", "creatinine", "bnp"),
    classVar = "mi_status",
    positiveClass = "MI"
  )
  expect_s3_class(result, "psychopdaROCClass")
})

test_that("psychopdaROC handles multiple biomarkers", {
  result <- psychopdaROC(
    data = psychopdaROC_multibiomarker,
    dependentVars = c("marker1", "marker2", "marker3", "combined_score"),
    classVar = "diagnosis",
    positiveClass = "Positive"
  )
  expect_s3_class(result, "psychopdaROCClass")
})

test_that("psychopdaROC handles Youden index method", {
  result <- psychopdaROC(
    data = psychopdaROC_test,
    dependentVars = "biomarker",
    classVar = "disease_status",
    positiveClass = "Disease",
    method = "maximize_metric",
    metric = "youden"
  )
  expect_s3_class(result, "psychopdaROCClass")
})

test_that("psychopdaROC handles different classification directions", {
  # Higher values = positive
  result_higher <- psychopdaROC(
    data = psychopdaROC_test,
    dependentVars = "biomarker",
    classVar = "disease_status",
    positiveClass = "Disease",
    direction = ">="
  )
  expect_no_error(result_higher)

  # Lower values = positive
  result_lower <- psychopdaROC(
    data = psychopdaROC_test,
    dependentVars = "biomarker",
    classVar = "disease_status",
    positiveClass = "Disease",
    direction = "<="
  )
  expect_no_error(result_lower)
})

test_that("psychopdaROC handles different optimization metrics", {
  metrics <- c("youden", "accuracy", "F1_score", "sum_sens_spec")

  for (m in metrics) {
    result <- psychopdaROC(
      data = psychopdaROC_test,
      dependentVars = "biomarker",
      classVar = "disease_status",
      metric = m
    )
    expect_s3_class(result, "psychopdaROCClass")
  }
})

test_that("psychopdaROC handles clinical mode selection", {
  # Basic mode
  result_basic <- psychopdaROC(
    data = psychopdaROC_test,
    dependentVars = "biomarker",
    classVar = "disease_status",
    clinicalMode = "basic"
  )
  expect_no_error(result_basic)

  # Advanced mode
  result_advanced <- psychopdaROC(
    data = psychopdaROC_test,
    dependentVars = "biomarker",
    classVar = "disease_status",
    clinicalMode = "advanced"
  )
  expect_no_error(result_advanced)
})

test_that("psychopdaROC handles clinical presets", {
  # Screening preset
  result_screen <- psychopdaROC(
    data = psychopdaROC_test,
    dependentVars = "biomarker",
    classVar = "disease_status",
    clinicalPreset = "screening"
  )
  expect_no_error(result_screen)

  # Confirmation preset
  result_confirm <- psychopdaROC(
    data = psychopdaROC_test,
    dependentVars = "biomarker",
    classVar = "disease_status",
    clinicalPreset = "confirmation"
  )
  expect_no_error(result_confirm)
})
