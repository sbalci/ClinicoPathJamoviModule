# ═══════════════════════════════════════════════════════════
# Argument Tests: psychopdaROC
# ═══════════════════════════════════════════════════════════
library(testthat)
library(ClinicoPath)
data(psychopdaROC_test, package = "ClinicoPath")
data(psychopdaROC_subgroup, package = "ClinicoPath")
data(psychopdaROC_costbenefit, package = "ClinicoPath")

test_that("psychopdaROC respects different cutpoint methods", {
  methods <- c("maximize_metric", "oc_youden_kernel", "oc_equal_sens_spec", "oc_closest_01")

  for (method in methods) {
    result <- psychopdaROC(
      data = psychopdaROC_test,
      dependentVars = "biomarker",
      classVar = "disease_status",
      method = method
    )
    expect_s3_class(result, "psychopdaROCClass")
  }
})

test_that("psychopdaROC respects subgroup analysis", {
  result <- psychopdaROC(
    data = psychopdaROC_subgroup,
    dependentVars = "test_score",
    classVar = "disease",
    positiveClass = "Disease",
    subGroup = "age_group"
  )
  expect_s3_class(result, "psychopdaROCClass")
})

test_that("psychopdaROC respects manual cutpoint", {
  result <- psychopdaROC(
    data = psychopdaROC_test,
    dependentVars = "biomarker",
    classVar = "disease_status",
    method = "oc_manual",
    specifyCutScore = "60"
  )
  expect_s3_class(result, "psychopdaROCClass")
})

test_that("psychopdaROC respects tie-breaking methods", {
  tie_methods <- c("mean", "median", "c")

  for (tie_method in tie_methods) {
    result <- psychopdaROC(
      data = psychopdaROC_test,
      dependentVars = "biomarker",
      classVar = "disease_status",
      break_ties = tie_method
    )
    expect_s3_class(result, "psychopdaROCClass")
  }
})

test_that("psychopdaROC respects metric tolerance", {
  result <- psychopdaROC(
    data = psychopdaROC_test,
    dependentVars = "biomarker",
    classVar = "disease_status",
    tol_metric = 0.1
  )
  expect_s3_class(result, "psychopdaROCClass")
})

test_that("psychopdaROC handles bootstrapped methods", {
  result <- psychopdaROC(
    data = psychopdaROC_test,
    dependentVars = "biomarker",
    classVar = "disease_status",
    method = "maximize_boot_metric"
  )
  expect_s3_class(result, "psychopdaROCClass")
})

test_that("psychopdaROC handles LOESS smoothed methods", {
  result <- psychopdaROC(
    data = psychopdaROC_test,
    dependentVars = "biomarker",
    classVar = "disease_status",
    method = "maximize_loess_metric"
  )
  expect_s3_class(result, "psychopdaROCClass")
})

test_that("psychopdaROC handles cost-benefit optimization", {
  result <- psychopdaROC(
    data = psychopdaROC_costbenefit,
    dependentVars = "risk_score",
    classVar = "outcome",
    positiveClass = "Event",
    method = "oc_cost_ratio"
  )
  expect_s3_class(result, "psychopdaROCClass")
})

test_that("psychopdaROC handles different metrics with minimize method", {
  result <- psychopdaROC(
    data = psychopdaROC_test,
    dependentVars = "biomarker",
    classVar = "disease_status",
    method = "minimize_metric",
    metric = "abs_d_sens_spec"
  )
  expect_s3_class(result, "psychopdaROCClass")
})
