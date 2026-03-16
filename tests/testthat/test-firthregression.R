# ===============================================================
# Core Tests: firthregression (Firth Penalized Likelihood)
# ===============================================================

library(testthat)

firth_opts <- function(...) {
  defaults <- list(
    analysisType = "logistic",
    outcome = "mortality",
    outcomeLevel = "Dead",
    predictors = c("age", "grade", "tumor_size", "lvi", "marker"),
    suitabilityCheck = FALSE,
    separationCheck = FALSE,
    compareStandard = FALSE,
    showModelFit = FALSE,
    forestPlot = FALSE,
    separationPlot = FALSE,
    showSummary = FALSE,
    showExplanations = FALSE
  )
  do.call(firthregressionOptions$new, modifyList(defaults, list(...)))
}

test_that("firthregression runs logistic mode", {
  skip_if_not_installed("logistf")
  data(firth_standard, package = "ClinicoPath")

  o <- firth_opts()
  a <- firthregressionClass$new(options = o, data = firth_standard)
  expect_no_error(a$run())
  expect_true(nrow(a$results$coefficients$asDF) > 0)
})

test_that("firthregression runs Cox mode", {
  skip_if_not_installed("coxphf")
  data(firth_standard, package = "ClinicoPath")

  o <- firth_opts(
    analysisType = "cox",
    time = "follow_up_time",
    outcome = "status",
    outcomeLevel = "Dead"
  )
  a <- firthregressionClass$new(options = o, data = firth_standard)
  expect_no_error(a$run())
  expect_true(nrow(a$results$coefficients$asDF) > 0)
})

test_that("firthregression handles separation data", {
  skip_if_not_installed("logistf")
  data(firth_separation, package = "ClinicoPath")

  o <- firth_opts(
    outcome = "outcome",
    outcomeLevel = "Recurrence",
    predictors = c("age", "bmi", "grade", "margin_positive"),
    separationCheck = TRUE
  )
  a <- firthregressionClass$new(options = o, data = firth_separation)
  expect_no_error(a$run())
  expect_true(nrow(a$results$separationDiagnostics$asDF) > 0)
})

test_that("firthregression with all outputs", {
  skip_if_not_installed("logistf")
  data(firth_standard, package = "ClinicoPath")

  o <- firth_opts(
    suitabilityCheck = TRUE,
    separationCheck = TRUE,
    compareStandard = TRUE,
    showModelFit = TRUE,
    showSummary = TRUE,
    showExplanations = TRUE
  )
  a <- firthregressionClass$new(options = o, data = firth_standard)
  expect_no_error(a$run())
  expect_true(nrow(a$results$coefficients$asDF) > 0)
  expect_true(nrow(a$results$modelFit$asDF) > 0)
  expect_true(nrow(a$results$separationDiagnostics$asDF) > 0)
  expect_true(nrow(a$results$comparisonTable$asDF) > 0)
  expect_true(nchar(a$results$suitabilityReport$content %||% "") > 0)
  expect_true(nchar(a$results$summaryText$content %||% "") > 0)
  expect_true(nchar(a$results$explanationText$content %||% "") > 0)
})
