# ═══════════════════════════════════════════════════════════
# Basic Tests: survivalcont
# ═══════════════════════════════════════════════════════════
library(testthat)
data(survivalcont_test, package = "ClinicoPath")
data(survivalcont_ki67, package = "ClinicoPath")
data(survivalcont_psa, package = "ClinicoPath")
data(survivalcont_hemoglobin, package = "ClinicoPath")
data(survivalcont_tumorsize, package = "ClinicoPath")
data(survivalcont_age, package = "ClinicoPath")
data(survivalcont_compete, package = "ClinicoPath")

test_that("survivalcont creates proper class", {
  result <- run_survivalcont(
    data = survivalcont_test,
    elapsedtime = "time_months",
    outcome = "outcome",
    outcomeLevel = "Dead",
    contexpl = "biomarker"
  )
  expect_s3_class(result, "survivalcontResults")
})

test_that("survivalcont handles basic continuous variable", {
  result <- run_survivalcont(
    data = survivalcont_test,
    elapsedtime = "time_months",
    outcome = "outcome",
    outcomeLevel = "Dead",
    contexpl = "biomarker"
  )
  expect_s3_class(result, "survivalcontResults")
  expect_gt(result$coxTable$rowCount, 0)
})

test_that("survivalcont handles Ki67 biomarker", {
  result <- run_survivalcont(
    data = survivalcont_ki67,
    elapsedtime = "time_months",
    outcome = "outcome",
    outcomeLevel = "Dead",
    contexpl = "ki67_percent"
  )
  expect_s3_class(result, "survivalcontResults")
})

test_that("survivalcont handles PSA levels", {
  result <- run_survivalcont(
    data = survivalcont_psa,
    elapsedtime = "time_months",
    outcome = "outcome",
    outcomeLevel = "Dead",
    contexpl = "psa_level"
  )
  expect_s3_class(result, "survivalcontResults")
})

test_that("survivalcont handles hemoglobin levels", {
  result <- run_survivalcont(
    data = survivalcont_hemoglobin,
    elapsedtime = "time_months",
    outcome = "outcome",
    outcomeLevel = "Dead",
    contexpl = "hemoglobin_gL"
  )
  expect_s3_class(result, "survivalcontResults")
})

test_that("survivalcont handles tumor size", {
  result <- run_survivalcont(
    data = survivalcont_tumorsize,
    elapsedtime = "time_months",
    outcome = "outcome",
    outcomeLevel = "Dead",
    contexpl = "tumor_size_cm"
  )
  expect_s3_class(result, "survivalcontResults")
})

test_that("survivalcont handles age as continuous variable", {
  result <- run_survivalcont(
    data = survivalcont_age,
    elapsedtime = "time_months",
    outcome = "outcome",
    outcomeLevel = "Dead",
    contexpl = "age_years"
  )
  expect_s3_class(result, "survivalcontResults")
})

test_that("survivalcont gives a safe competing-risk handoff", {
  result <- run_survivalcont(
    data = survivalcont_compete,
    elapsedtime = "time_months",
    outcome = "outcome",
    outcomeLevel = "Dead",
    analysistype = "compete",
    multievent = TRUE,
    contexpl = "biomarker_score",
    dod = "Dead_Disease",
    dooc = "Dead_Other",
    awd = "Alive_Disease",
    awod = "Alive_NED"
  )
  expect_s3_class(result, "survivalcontResults")
  expect_match(result$errors$content, "Competing risks not available")
  expect_equal(result$coxTable$rowCount, 0)
})

test_that("survivalcont handles survival plot", {
  result <- run_survivalcont(
    data = survivalcont_test,
    elapsedtime = "time_months",
    outcome = "outcome",
    outcomeLevel = "Dead",
    contexpl = "biomarker",
    findcut = TRUE,
    sc = TRUE
  )
  expect_s3_class(result, "survivalcontResults")
  expect_false(is.null(result$plot5))
})

test_that("survivalcont handles KMunicate plot", {
  result <- run_survivalcont(
    data = survivalcont_test,
    elapsedtime = "time_months",
    outcome = "outcome",
    outcomeLevel = "Dead",
    contexpl = "biomarker",
    kmunicate = TRUE
  )
  expect_s3_class(result, "survivalcontResults")
})

test_that("survivalcont handles cumulative events", {
  result <- run_survivalcont(
    data = survivalcont_test,
    elapsedtime = "time_months",
    outcome = "outcome",
    outcomeLevel = "Dead",
    contexpl = "biomarker",
    ce = TRUE
  )
  expect_s3_class(result, "survivalcontResults")
})

test_that("survivalcont handles cumulative hazard", {
  result <- run_survivalcont(
    data = survivalcont_test,
    elapsedtime = "time_months",
    outcome = "outcome",
    outcomeLevel = "Dead",
    contexpl = "biomarker",
    ch = TRUE
  )
  expect_s3_class(result, "survivalcontResults")
})

test_that("survivalcont handles cutoff finding", {
  result <- run_survivalcont(
    data = survivalcont_test,
    elapsedtime = "time_months",
    outcome = "outcome",
    outcomeLevel = "Dead",
    contexpl = "biomarker",
    findcut = TRUE
  )
  expect_s3_class(result, "survivalcontResults")
})

test_that("survivalcont handles custom cutpoints", {
  result <- run_survivalcont(
    data = survivalcont_test,
    elapsedtime = "time_months",
    outcome = "outcome",
    outcomeLevel = "Dead",
    contexpl = "biomarker",
    cutp = "6, 12, 24, 36"
  )
  expect_s3_class(result, "survivalcontResults")
})
