# ===============================================================
# Argument Tests: firthregression
# ===============================================================

library(testthat)

firth_opts <- function(...) {
  defaults <- list(
    analysisType = "logistic",
    outcome = "mortality",
    outcomeLevel = "Dead",
    predictors = c("age", "grade", "tumor_size", "lvi", "marker"),
    suitabilityCheck = FALSE, separationCheck = FALSE, compareStandard = FALSE,
    showModelFit = FALSE, forestPlot = FALSE, separationPlot = FALSE,
    showSummary = FALSE, showExplanations = FALSE
  )
  do.call(firthregressionOptions$new, modifyList(defaults, list(...)))
}

test_that("ciLevel changes CI bounds", {
  skip_if_not_installed("logistf")
  data(firth_standard, package = "ClinicoPath")

  o1 <- firth_opts(ciLevel = 0.95)
  a1 <- firthregressionClass$new(options = o1, data = firth_standard)
  a1$run()

  o2 <- firth_opts(ciLevel = 0.99)
  a2 <- firthregressionClass$new(options = o2, data = firth_standard)
  a2$run()

  ci1 <- a1$results$coefficients$asDF$upper_ci[1]
  ci2 <- a2$results$coefficients$asDF$upper_ci[1]
  expect_false(identical(ci1, ci2))
})

test_that("ciMethod profile vs wald produces different CIs", {
  skip_if_not_installed("logistf")
  data(firth_standard, package = "ClinicoPath")

  o1 <- firth_opts(ciMethod = "profile")
  a1 <- firthregressionClass$new(options = o1, data = firth_standard)
  a1$run()

  o2 <- firth_opts(ciMethod = "wald")
  a2 <- firthregressionClass$new(options = o2, data = firth_standard)
  a2$run()

  ci1 <- a1$results$coefficients$asDF$lower_ci[1]
  ci2 <- a2$results$coefficients$asDF$lower_ci[1]
  expect_false(identical(ci1, ci2))
})

test_that("compareStandard populates comparison table", {
  skip_if_not_installed("logistf")
  data(firth_standard, package = "ClinicoPath")

  o <- firth_opts(compareStandard = TRUE)
  a <- firthregressionClass$new(options = o, data = firth_standard)
  a$run()
  expect_true(nrow(a$results$comparisonTable$asDF) > 0)
})

test_that("showModelFit=FALSE produces 0 rows", {
  skip_if_not_installed("logistf")
  data(firth_standard, package = "ClinicoPath")

  o <- firth_opts(showModelFit = FALSE)
  a <- firthregressionClass$new(options = o, data = firth_standard)
  a$run()
  expect_equal(nrow(a$results$modelFit$asDF), 0)
})

test_that("showModelFit=TRUE populates model fit", {
  skip_if_not_installed("logistf")
  data(firth_standard, package = "ClinicoPath")

  o <- firth_opts(showModelFit = TRUE)
  a <- firthregressionClass$new(options = o, data = firth_standard)
  a$run()
  expect_true(nrow(a$results$modelFit$asDF) > 0)
})

test_that("suitabilityCheck produces report", {
  skip_if_not_installed("logistf")
  data(firth_standard, package = "ClinicoPath")

  o <- firth_opts(suitabilityCheck = TRUE)
  a <- firthregressionClass$new(options = o, data = firth_standard)
  a$run()
  expect_true(nchar(a$results$suitabilityReport$content %||% "") > 0)
})

test_that("Cox mode with wald CI shows info notice", {
  skip_if_not_installed("coxphf")
  data(firth_standard, package = "ClinicoPath")

  o <- firth_opts(
    analysisType = "cox", time = "follow_up_time",
    outcome = "status", outcomeLevel = "Dead",
    ciMethod = "wald"
  )
  a <- firthregressionClass$new(options = o, data = firth_standard)
  a$run()
  notices <- a$results$notices$content %||% ""
  expect_true(grepl("profile penalized likelihood", notices))
})
