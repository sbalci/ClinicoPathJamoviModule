# ═══════════════════════════════════════════════════════════
# Basic Functionality Tests: oddsratio
# ═══════════════════════════════════════════════════════════
library(testthat)
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
    outcomeLevel = "Dead",
    predictorLevel = NULL
  )
  expect_s3_class(result, "oddsratioResults")
})

test_that("oddsratio handles multiple predictors", {
  result <- oddsratio(
    data = oddsratio_test,
    explanatory = c("stage", "treatment", "biomarker_status"),
    outcome = "outcome",
    outcomeLevel = "Dead",
    predictorLevel = NULL
  )
  expect_no_error(result)
})

test_that("oddsratio handles continuous predictors", {
  result <- oddsratio(
    data = oddsratio_test,
    explanatory = c("age", "tumor_size"),
    outcome = "outcome",
    outcomeLevel = NULL,
    predictorLevel = NULL
  )
  expect_no_error(result)
})

test_that("oddsratio handles mixed predictors", {
  result <- oddsratio(
    data = oddsratio_test,
    explanatory = c("age", "stage", "tumor_size"),
    outcome = "outcome",
    outcomeLevel = NULL,
    predictorLevel = NULL
  )
  expect_no_error(result)
})

test_that("oddsratio sets todo notice on missing required arguments", {
  res1 <- oddsratio(data = oddsratio_test, outcome = "outcome", outcomeLevel = NULL, predictorLevel = NULL)
  expect_true(nchar(res1$todo$content) > 0)
  res2 <- oddsratio(data = oddsratio_test, explanatory = "stage", outcomeLevel = NULL, predictorLevel = NULL)
  expect_true(nchar(res2$todo$content) > 0)
})

test_that("oddsratio handles nomogram option", {
  result <- oddsratio(
    data = oddsratio_diagnostic,
    explanatory = "test_result",
    outcome = "disease_status",
    showNomogram = TRUE,
    outcomeLevel = NULL,
    predictorLevel = NULL
  )
  expect_no_error(result)
})

test_that("oddsratio handles explanations option", {
  result <- oddsratio(
    data = oddsratio_test,
    explanatory = "stage",
    outcome = "outcome",
    showExplanations = TRUE,
    outcomeLevel = NULL,
    predictorLevel = NULL
  )
  expect_no_error(result)
})
