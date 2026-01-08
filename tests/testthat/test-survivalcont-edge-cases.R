# ═══════════════════════════════════════════════════════════
# Edge Cases Tests: survivalcont
# ═══════════════════════════════════════════════════════════
library(testthat)
library(ClinicoPath)
data(survivalcont_small, package = "ClinicoPath")
data(survivalcont_nocutoff, package = "ClinicoPath")
data(survivalcont_extreme, package = "ClinicoPath")
data(survivalcont_missing, package = "ClinicoPath")
data(survivalcont_constant, package = "ClinicoPath")
data(survivalcont_large, package = "ClinicoPath")

test_that("survivalcont handles small datasets", {
  result <- survivalcont(
    data = survivalcont_small,
    elapsedtime = "time_months",
    outcome = "outcome",
    contexpl = "continuous_var"
  )
  expect_s3_class(result, "survivalcontClass")
  expect_true(nrow(survivalcont_small) == 20)
})

test_that("survivalcont handles variable with no clear cutoff", {
  result <- survivalcont(
    data = survivalcont_nocutoff,
    elapsedtime = "time_months",
    outcome = "outcome",
    contexpl = "random_marker",
    findcut = TRUE
  )
  expect_s3_class(result, "survivalcontClass")
})

test_that("survivalcont handles extreme values", {
  result <- survivalcont(
    data = survivalcont_extreme,
    elapsedtime = "time_months",
    outcome = "outcome",
    contexpl = "extreme_values"
  )
  expect_s3_class(result, "survivalcontClass")
})

test_that("survivalcont handles missing data in continuous variable", {
  result <- survivalcont(
    data = survivalcont_missing,
    elapsedtime = "time_months",
    outcome = "outcome",
    contexpl = "biomarker"
  )
  expect_s3_class(result, "survivalcontClass")
  # Should have missing values
  expect_true(any(is.na(survivalcont_missing$biomarker)))
})

test_that("survivalcont handles missing time values", {
  result <- survivalcont(
    data = survivalcont_missing,
    elapsedtime = "time_months",
    outcome = "outcome",
    contexpl = "biomarker"
  )
  expect_s3_class(result, "survivalcontClass")
  # Should have some NA times
  expect_true(any(is.na(survivalcont_missing$time_months)))
})

test_that("survivalcont handles constant continuous variable", {
  result <- survivalcont(
    data = survivalcont_constant,
    elapsedtime = "time_months",
    outcome = "outcome",
    contexpl = "constant_marker"
  )
  expect_s3_class(result, "survivalcontClass")
  # All values should be the same
  expect_true(length(unique(survivalcont_constant$constant_marker)) == 1)
})

test_that("survivalcont handles large datasets efficiently", {
  result <- survivalcont(
    data = survivalcont_large,
    elapsedtime = "time_months",
    outcome = "outcome",
    outcomeLevel = "Dead",
    contexpl = "biomarker"
  )
  expect_s3_class(result, "survivalcontClass")
  expect_true(nrow(survivalcont_large) == 500)
})

test_that("survivalcont handles cutoff finding with small dataset", {
  result <- survivalcont(
    data = survivalcont_small,
    elapsedtime = "time_months",
    outcome = "outcome",
    contexpl = "continuous_var",
    findcut = TRUE
  )
  expect_s3_class(result, "survivalcontClass")
})

test_that("survivalcont handles multiple cutoffs with small groups", {
  result <- survivalcont(
    data = survivalcont_small,
    elapsedtime = "time_months",
    outcome = "outcome",
    contexpl = "continuous_var",
    multiple_cutoffs = TRUE,
    num_cutoffs = "two",
    min_group_size = 20
  )
  expect_s3_class(result, "survivalcontClass")
})

test_that("survivalcont handles single observation", {
  test_data <- survivalcont_small[1, ]
  result <- survivalcont(
    data = test_data,
    elapsedtime = "time_months",
    outcome = "outcome",
    contexpl = "continuous_var"
  )
  expect_s3_class(result, "survivalcontClass")
})

test_that("survivalcont handles very high continuous values", {
  result <- survivalcont(
    data = survivalcont_extreme,
    elapsedtime = "time_months",
    outcome = "outcome",
    contexpl = "extreme_values",
    findcut = TRUE
  )
  expect_s3_class(result, "survivalcontClass")
})

test_that("survivalcont handles zero variance in subgroups", {
  # After finding cutoff, one group might have little variance
  result <- survivalcont(
    data = survivalcont_constant,
    elapsedtime = "time_months",
    outcome = "outcome",
    contexpl = "constant_marker",
    findcut = TRUE
  )
  expect_s3_class(result, "survivalcontClass")
})
