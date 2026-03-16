# ===============================================================
# Edge Cases Tests: firthregression
# ===============================================================

library(testthat)

firth_opts <- function(...) {
  defaults <- list(
    analysisType = "logistic",
    outcome = "mortality",
    outcomeLevel = "Dead",
    predictors = c("age", "tumor_size", "marker"),
    suitabilityCheck = FALSE, separationCheck = FALSE, compareStandard = FALSE,
    showModelFit = FALSE, forestPlot = FALSE, separationPlot = FALSE,
    showSummary = FALSE, showExplanations = FALSE
  )
  do.call(firthregressionOptions$new, modifyList(defaults, list(...)))
}

test_that("firthregression returns silently without variables", {
  skip_if_not_installed("logistf")
  data(firth_standard, package = "ClinicoPath")

  o <- firth_opts(predictors = character(0))
  a <- firthregressionClass$new(options = o, data = firth_standard)
  expect_no_error(a$run())
  # Should show welcome, not crash
})

test_that("firthregression handles rare events", {
  skip_if_not_installed("logistf")
  data(firth_separation, package = "ClinicoPath")

  o <- firth_opts(
    outcome = "outcome",
    outcomeLevel = "Recurrence",
    predictors = c("age", "bmi", "grade"),
    suitabilityCheck = TRUE
  )
  a <- firthregressionClass$new(options = o, data = firth_separation)
  expect_no_error(a$run())
  expect_true(nrow(a$results$coefficients$asDF) > 0)
})

test_that("firthregression handles separation with bias reduction", {
  skip_if_not_installed("logistf")
  data(firth_separation, package = "ClinicoPath")

  o <- firth_opts(
    outcome = "outcome",
    outcomeLevel = "Recurrence",
    predictors = c("age", "grade", "margin_positive"),
    compareStandard = TRUE,
    separationCheck = TRUE
  )
  a <- firthregressionClass$new(options = o, data = firth_separation)
  expect_no_error(a$run())
  # Should have bias reduction values
  coefs <- a$results$coefficients$asDF
  expect_true(nrow(coefs) > 0)
})

test_that("firthregression Cox with small sample", {
  skip_if_not_installed("coxphf")
  data(firth_smallcox, package = "ClinicoPath")

  o <- firth_opts(
    analysisType = "cox",
    time = "time",
    outcome = "status",
    outcomeLevel = "Dead",
    predictors = c("age", "treatment", "biomarker"),
    suitabilityCheck = TRUE
  )
  a <- firthregressionClass$new(options = o, data = firth_smallcox)
  expect_no_error(a$run())
  expect_true(nrow(a$results$coefficients$asDF) > 0)
})

test_that("firthregression handles single predictor", {
  skip_if_not_installed("logistf")
  data(firth_standard, package = "ClinicoPath")

  o <- firth_opts(predictors = "age")
  a <- firthregressionClass$new(options = o, data = firth_standard)
  expect_no_error(a$run())
  expect_true(nrow(a$results$coefficients$asDF) >= 1)
})

test_that("firthregression handles missing data", {
  skip_if_not_installed("logistf")
  data(firth_standard, package = "ClinicoPath")

  d <- firth_standard
  set.seed(99)
  d$marker[sample(nrow(d), 15)] <- NA

  o <- firth_opts()
  a <- firthregressionClass$new(options = o, data = d)
  expect_no_error(a$run())
})
