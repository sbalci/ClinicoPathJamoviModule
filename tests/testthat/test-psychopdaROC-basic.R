# `positiveClass` and `refVar` are `type: Level` options. The jamovi compiler forbids a default
# on a Level, so both compile to REQUIRED arguments of the generated wrapper even though the
# analysis runs perfectly well without a reference variable. This helper supplies just those two.
#
# It used to fill EVERY argument that had no default with "" -- including `dependentVars` and
# `classVar` -- so a test that forgot a genuinely required variable was silently handed an empty
# string instead of failing. Narrowing it keeps that contract intact.
psychopdaROC <- function(...) {
  args <- list(...)
  if (!("positiveClass" %in% names(args))) args$positiveClass <- ""
  if (!("refVar" %in% names(args))) args["refVar"] <- list(NULL)  # [[<- would drop the element
  do.call(ClinicoPath::psychopdaROC, args)
}

# ═══════════════════════════════════════════════════════════
# Basic Tests: psychopdaROC
# ═══════════════════════════════════════════════════════════
library(testthat)
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
  expect_s3_class(result, "psychopdaROCResults")
})

test_that("psychopdaROC handles basic ROC analysis", {
  result <- psychopdaROC(
    data = psychopdaROC_test,
    dependentVars = "biomarker",
    classVar = "disease_status",
    positiveClass = "Disease"
  )
  expect_s3_class(result, "psychopdaROCResults")
})

test_that("psychopdaROC handles cancer screening data", {
  result <- psychopdaROC(
    data = psychopdaROC_screening,
    dependentVars = c("psa_level", "ca125"),
    classVar = "cancer",
    positiveClass = "Cancer"
  )
  expect_s3_class(result, "psychopdaROCResults")
})

test_that("psychopdaROC handles cardiac biomarkers", {
  result <- psychopdaROC(
    data = psychopdaROC_cardiac,
    dependentVars = c("troponin", "creatinine", "bnp"),
    classVar = "mi_status",
    positiveClass = "MI"
  )
  expect_s3_class(result, "psychopdaROCResults")
})

test_that("psychopdaROC handles multiple biomarkers", {
  result <- psychopdaROC(
    data = psychopdaROC_multibiomarker,
    dependentVars = c("marker1", "marker2", "marker3", "combined_score"),
    classVar = "diagnosis",
    positiveClass = "Positive"
  )
  expect_s3_class(result, "psychopdaROCResults")
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
  expect_s3_class(result, "psychopdaROCResults")
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
    expect_s3_class(result, "psychopdaROCResults")
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

test_that("the removed clinical-preset option is gone from the schema", {
  # clinicalPreset was removed: all four presets were a byte-identical no-op
  # because the handler wrote into `instructions` from .init(), which is then
  # overwritten. This test used to assert only expect_no_error() on two of them,
  # which passed throughout the entire period the option did nothing at all.
  # Guard the removal instead, so the dead option cannot quietly return.
  a_yaml <- readLines(test_path("..", "..", "jamovi", "psychopdaROC.a.yaml"), warn = FALSE)
  u_yaml <- readLines(test_path("..", "..", "jamovi", "psychopdaROC.u.yaml"), warn = FALSE)
  expect_false(any(grepl("clinicalPreset", a_yaml, fixed = TRUE)))
  expect_false(any(grepl("clinicalPreset", u_yaml, fixed = TRUE)))
  # The .b.R must not read it any more either.
  b_src <- readLines(test_path("..", "..", "R", "psychopdaROC.b.R"), warn = FALSE)
  code <- b_src[!grepl("^\\s*#", b_src)]
  expect_false(any(grepl("clinicalPreset", code, fixed = TRUE)))

  # Passing it should eventually be a hard "unused argument" error, but the R
  # wrapper lives in the GENERATED R/psychopdaROC.h.R, which still declares the
  # option until someone runs jmvtools::prepare(). Skip rather than fail while
  # the generated header is stale, so this starts guarding the moment it is
  # regenerated instead of going red for a reason the source cannot fix.
  h_src <- readLines(test_path("..", "..", "R", "psychopdaROC.h.R"), warn = FALSE)
  skip_if(any(grepl("clinicalPreset", h_src, fixed = TRUE)),
          "R/psychopdaROC.h.R is stale - run jmvtools::prepare()")
  expect_error(
    psychopdaROC(data = psychopdaROC_test, dependentVars = "biomarker",
                 classVar = "disease_status", clinicalPreset = "screening"),
    "unused argument")
})
