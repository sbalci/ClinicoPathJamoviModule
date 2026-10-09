# `positiveClass` is a `type: Level` option. The jamovi compiler forbids a default on a Level, so
# it compiles to a REQUIRED argument of the generated wrapper; this helper supplies just that one.
# (`refVar`, Reference Test for IDI/NRI, is a `type: Variable` with a NULL default since 2026-10-09.)
#
# It used to fill EVERY argument that had no default with "" -- including `dependentVars` and
# `classVar` -- so a test that forgot a genuinely required variable was silently handed an empty
# string instead of failing. Narrowing it keeps that contract intact.
psychopdaROC <- function(...) {
  args <- list(...)
  if (!("positiveClass" %in% names(args))) args$positiveClass <- ""
  do.call(ClinicoPath::psychopdaROC, args)
}

# ═══════════════════════════════════════════════════════════
# Argument Tests: psychopdaROC
# ═══════════════════════════════════════════════════════════
library(testthat)
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
      positiveClass = "Disease",
      method = method
    )
    expect_s3_class(result, "psychopdaROCResults")
    expect_equal(result$resultsTable$get(key = "biomarker")$rowCount, 1, info = method)
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
  expect_s3_class(result, "psychopdaROCResults")
})

test_that("psychopdaROC respects manual cutpoint", {
  result <- psychopdaROC(
    data = psychopdaROC_test,
    dependentVars = "biomarker",
    classVar = "disease_status",
    positiveClass = "Disease",
    method = "oc_manual",
    specifyCutScore = "60"
  )
  expect_s3_class(result, "psychopdaROCResults")
  expect_equal(as.numeric(result$resultsTable$get(key = "biomarker")$asDF$cutpoint), 60)
})

test_that("psychopdaROC respects tie-breaking methods", {
  tie_methods <- c("mean", "median", "c")

  for (tie_method in tie_methods) {
    result <- psychopdaROC(
      data = psychopdaROC_test,
      dependentVars = "biomarker",
      classVar = "disease_status",
      positiveClass = "Disease",
      break_ties = tie_method
    )
    expect_s3_class(result, "psychopdaROCResults")
    expect_equal(result$resultsTable$get(key = "biomarker")$rowCount, 1, info = tie_method)
  }
})

test_that("psychopdaROC respects metric tolerance", {
  result <- psychopdaROC(
    data = psychopdaROC_test,
    dependentVars = "biomarker",
    classVar = "disease_status",
    positiveClass = "Disease",
    tol_metric = 0.1
  )
  expect_s3_class(result, "psychopdaROCResults")
  expect_equal(result$resultsTable$get(key = "biomarker")$rowCount, 1)
})

test_that("psychopdaROC handles bootstrapped methods", {
  result <- psychopdaROC(
    data = psychopdaROC_test,
    dependentVars = "biomarker",
    classVar = "disease_status",
    positiveClass = "Disease",
    method = "maximize_boot_metric"
  )
  expect_s3_class(result, "psychopdaROCResults")
  expect_equal(result$resultsTable$get(key = "biomarker")$rowCount, 1)
})

test_that("psychopdaROC handles LOESS smoothed methods", {
  result <- psychopdaROC(
    data = psychopdaROC_test,
    dependentVars = "biomarker",
    classVar = "disease_status",
    positiveClass = "Disease",
    method = "maximize_loess_metric"
  )
  expect_s3_class(result, "psychopdaROCResults")
  expect_equal(result$resultsTable$get(key = "biomarker")$rowCount, 1)
})

test_that("psychopdaROC handles cost-benefit optimization", {
  result <- psychopdaROC(
    data = psychopdaROC_costbenefit,
    dependentVars = "risk_score",
    classVar = "outcome",
    positiveClass = "Event",
    method = "oc_cost_ratio"
  )
  expect_s3_class(result, "psychopdaROCResults")
})

test_that("psychopdaROC handles different metrics with minimize method", {
  result <- psychopdaROC(
    data = psychopdaROC_test,
    dependentVars = "biomarker",
    classVar = "disease_status",
    positiveClass = "Disease",
    method = "minimize_metric",
    metric = "abs_d_sens_spec"
  )
  expect_s3_class(result, "psychopdaROCResults")
  expect_equal(result$resultsTable$get(key = "biomarker")$rowCount, 1)
})
