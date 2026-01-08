# ═══════════════════════════════════════════════════════════
# Basic Functionality Tests: oddsratio
# ═══════════════════════════════════════════════════════════
library(testthat)
library(ClinicoPath)
data(oddsratio_test, package = "ClinicoPath")

test_that("oddsratio function exists", {
  expect_true(exists("oddsratio"))
  expect_true(is.function(oddsratio))
})

test_that("oddsratio runs with minimal arguments", {
  result <- oddsratio(
    data = oddsratio_test,
    explanatory = "stage",
    outcome = "outcome",
    outcomeLevel = "Dead"
  )
  expect_s3_class(result, "oddsratioClass")
})

test_that("oddsratio handles multiple predictors", {
  result <- oddsratio(
    data = oddsratio_test,
    explanatory = c("stage", "treatment", "biomarker_status"),
    outcome = "outcome",
    outcomeLevel = "Dead"
  )
  expect_no_error(result)
})

test_that("oddsratio handles continuous predictors", {
  result <- oddsratio(
    data = oddsratio_test,
    explanatory = c("age", "tumor_size"),
    outcome = "outcome"
  )
  expect_no_error(result)
})

test_that("oddsratio handles mixed predictors", {
  result <- oddsratio(
    data = oddsratio_test,
    explanatory = c("age", "stage", "tumor_size"),
    outcome = "outcome"
  )
  expect_no_error(result)
})

test_that("oddsratio errors on missing required arguments", {
  expect_error(oddsratio(data = oddsratio_test, outcome = "outcome"))
  expect_error(oddsratio(data = oddsratio_test, explanatory = "stage"))
})

test_that("oddsratio handles nomogram option", {
  result <- oddsratio(
    data = oddsratio_diagnostic,
    explanatory = "test_result",
    outcome = "disease_status",
    showNomogram = TRUE
  )
  expect_no_error(result)
})

test_that("oddsratio handles explanations option", {
  result <- oddsratio(
    data = oddsratio_test,
    explanatory = "stage",
    outcome = "outcome",
    showExplanations = TRUE
  )
  expect_no_error(result)
})
