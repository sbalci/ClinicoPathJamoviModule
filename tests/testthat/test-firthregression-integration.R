# ===============================================================
# Integration Tests: firthregression
# ===============================================================

library(testthat)

test_that("full logistic pipeline with all outputs", {
  skip_if_not_installed("logistf")
  data(firth_standard, package = "ClinicoPath")

  o <- firthregressionOptions$new(
    analysisType = "logistic",
    outcome = "mortality",
    outcomeLevel = "Dead",
    predictors = c("age", "grade", "tumor_size", "lvi", "marker"),
    suitabilityCheck = TRUE,
    separationCheck = TRUE,
    compareStandard = TRUE,
    showModelFit = TRUE,
    forestPlot = FALSE,
    separationPlot = FALSE,
    showSummary = TRUE,
    showExplanations = TRUE
  )
  a <- firthregressionClass$new(options = o, data = firth_standard)
  expect_no_error(a$run())

  expect_true(nrow(a$results$coefficients$asDF) >= 5)
  expect_true(nrow(a$results$modelFit$asDF) > 0)
  expect_true(nrow(a$results$separationDiagnostics$asDF) > 0)
  expect_true(nrow(a$results$comparisonTable$asDF) > 0)
  expect_true(nchar(a$results$suitabilityReport$content %||% "") > 0)
  expect_true(nchar(a$results$summaryText$content %||% "") > 0)
  expect_true(nchar(a$results$explanationText$content %||% "") > 0)
})

test_that("full Cox pipeline", {
  skip_if_not_installed("coxphf")
  data(firth_standard, package = "ClinicoPath")

  o <- firthregressionOptions$new(
    analysisType = "cox",
    time = "follow_up_time",
    outcome = "status",
    outcomeLevel = "Dead",
    predictors = c("age", "grade", "tumor_size", "lvi", "marker"),
    suitabilityCheck = TRUE,
    compareStandard = TRUE,
    showModelFit = TRUE,
    showSummary = TRUE,
    showExplanations = TRUE,
    forestPlot = FALSE,
    separationPlot = FALSE
  )
  a <- firthregressionClass$new(options = o, data = firth_standard)
  expect_no_error(a$run())
  expect_true(nrow(a$results$coefficients$asDF) >= 5)
  expect_true(nchar(a$results$summaryText$content %||% "") > 0)
})

test_that("reproducibility: same data gives same results", {
  skip_if_not_installed("logistf")
  data(firth_standard, package = "ClinicoPath")

  o <- firthregressionOptions$new(
    analysisType = "logistic",
    outcome = "mortality",
    outcomeLevel = "Dead",
    predictors = c("age", "tumor_size"),
    forestPlot = FALSE, separationPlot = FALSE
  )

  a1 <- firthregressionClass$new(options = o, data = firth_standard)
  a1$run()
  a2 <- firthregressionClass$new(options = o, data = firth_standard)
  a2$run()

  c1 <- a1$results$coefficients$asDF$coefficient
  c2 <- a2$results$coefficients$asDF$coefficient
  expect_equal(c1, c2)
})
