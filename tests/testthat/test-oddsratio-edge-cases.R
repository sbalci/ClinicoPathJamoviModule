# ═══════════════════════════════════════════════════════════
# Edge Cases Tests: oddsratio
# ═══════════════════════════════════════════════════════════
library(testthat)
library(ClinicoPath)
data(oddsratio_small, package = "ClinicoPath")
data(oddsratio_large, package = "ClinicoPath")
data(oddsratio_perfect, package = "ClinicoPath")
data(oddsratio_zerocell, package = "ClinicoPath")

test_that("oddsratio handles small datasets", {
  result <- oddsratio(
    data = oddsratio_small,
    explanatory = "predictor",
    outcome = "outcome"
  )
  expect_s3_class(result, "oddsratioClass")
})

test_that("oddsratio handles large datasets", {
  result <- oddsratio(
    data = oddsratio_large,
    explanatory = c("var1", "var2"),
    outcome = "outcome"
  )
  expect_no_error(result)
})

test_that("oddsratio handles perfect separation", {
  result <- oddsratio(
    data = oddsratio_perfect,
    explanatory = "predictor",
    outcome = "outcome"
  )
  # May warn about perfect separation
  expect_s3_class(result, "oddsratioClass")
})

test_that("oddsratio handles zero cells", {
  result <- oddsratio(
    data = oddsratio_zerocell,
    explanatory = "predictor",
    outcome = "outcome"
  )
  # May warn about zero cells
  expect_s3_class(result, "oddsratioClass")
})
