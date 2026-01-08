# ═══════════════════════════════════════════════════════════
# Basic Tests: survivalcont
# ═══════════════════════════════════════════════════════════
library(testthat)
library(ClinicoPath)
data(survivalcont_test, package = "ClinicoPath")
data(survivalcont_ki67, package = "ClinicoPath")
data(survivalcont_psa, package = "ClinicoPath")
data(survivalcont_hemoglobin, package = "ClinicoPath")
data(survivalcont_tumorsize, package = "ClinicoPath")
data(survivalcont_age, package = "ClinicoPath")
data(survivalcont_compete, package = "ClinicoPath")

test_that("survivalcont creates proper class", {
  result <- survivalcont(
    data = survivalcont_test,
    elapsedtime = "time_months",
    outcome = "outcome",
    contexpl = "biomarker"
  )
  expect_s3_class(result, "survivalcontClass")
})

test_that("survivalcont handles basic continuous variable", {
  result <- survivalcont(
    data = survivalcont_test,
    elapsedtime = "time_months",
    outcome = "outcome",
    outcomeLevel = "Dead",
    contexpl = "biomarker"
  )
  expect_s3_class(result, "survivalcontClass")
  expect_true(length(result$results) > 0)
})

test_that("survivalcont handles Ki67 biomarker", {
  result <- survivalcont(
    data = survivalcont_ki67,
    elapsedtime = "time_months",
    outcome = "outcome",
    outcomeLevel = "Dead",
    contexpl = "ki67_percent"
  )
  expect_s3_class(result, "survivalcontClass")
})

test_that("survivalcont handles PSA levels", {
  result <- survivalcont(
    data = survivalcont_psa,
    elapsedtime = "time_months",
    outcome = "outcome",
    outcomeLevel = "Dead",
    contexpl = "psa_level"
  )
  expect_s3_class(result, "survivalcontClass")
})

test_that("survivalcont handles hemoglobin levels", {
  result <- survivalcont(
    data = survivalcont_hemoglobin,
    elapsedtime = "time_months",
    outcome = "outcome",
    outcomeLevel = "Dead",
    contexpl = "hemoglobin_gL"
  )
  expect_s3_class(result, "survivalcontClass")
})

test_that("survivalcont handles tumor size", {
  result <- survivalcont(
    data = survivalcont_tumorsize,
    elapsedtime = "time_months",
    outcome = "outcome",
    outcomeLevel = "Dead",
    contexpl = "tumor_size_cm"
  )
  expect_s3_class(result, "survivalcontClass")
})

test_that("survivalcont handles age as continuous variable", {
  result <- survivalcont(
    data = survivalcont_age,
    elapsedtime = "time_months",
    outcome = "outcome",
    outcomeLevel = "Dead",
    contexpl = "age_years"
  )
  expect_s3_class(result, "survivalcontClass")
})

test_that("survivalcont handles competing risks", {
  result <- survivalcont(
    data = survivalcont_compete,
    elapsedtime = "time_months",
    outcome = "outcome",
    analysistype = "compete",
    contexpl = "biomarker_score",
    dod = "Dead_Disease",
    dooc = "Dead_Other"
  )
  expect_s3_class(result, "survivalcontClass")
})

test_that("survivalcont handles survival plot", {
  result <- survivalcont(
    data = survivalcont_test,
    elapsedtime = "time_months",
    outcome = "outcome",
    outcomeLevel = "Dead",
    contexpl = "biomarker",
    sc = TRUE
  )
  expect_s3_class(result, "survivalcontClass")
  expect_true(!is.null(result$results$survivalplot))
})

test_that("survivalcont handles KMunicate plot", {
  result <- survivalcont(
    data = survivalcont_test,
    elapsedtime = "time_months",
    outcome = "outcome",
    outcomeLevel = "Dead",
    contexpl = "biomarker",
    kmunicate = TRUE
  )
  expect_s3_class(result, "survivalcontClass")
})

test_that("survivalcont handles cumulative events", {
  result <- survivalcont(
    data = survivalcont_test,
    elapsedtime = "time_months",
    outcome = "outcome",
    outcomeLevel = "Dead",
    contexpl = "biomarker",
    ce = TRUE
  )
  expect_s3_class(result, "survivalcontClass")
})

test_that("survivalcont handles cumulative hazard", {
  result <- survivalcont(
    data = survivalcont_test,
    elapsedtime = "time_months",
    outcome = "outcome",
    outcomeLevel = "Dead",
    contexpl = "biomarker",
    ch = TRUE
  )
  expect_s3_class(result, "survivalcontClass")
})

test_that("survivalcont handles cutoff finding", {
  result <- survivalcont(
    data = survivalcont_test,
    elapsedtime = "time_months",
    outcome = "outcome",
    outcomeLevel = "Dead",
    contexpl = "biomarker",
    findcut = TRUE
  )
  expect_s3_class(result, "survivalcontClass")
})

test_that("survivalcont handles custom cutpoints", {
  result <- survivalcont(
    data = survivalcont_test,
    elapsedtime = "time_months",
    outcome = "outcome",
    outcomeLevel = "Dead",
    contexpl = "biomarker",
    cutp = "6, 12, 24, 36"
  )
  expect_s3_class(result, "survivalcontClass")
})
