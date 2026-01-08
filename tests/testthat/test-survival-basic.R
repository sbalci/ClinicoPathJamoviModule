# ═══════════════════════════════════════════════════════════
# Basic Functionality Tests: survival
# ═══════════════════════════════════════════════════════════
#
# Tests basic functionality and required arguments
# for the survival jamovi function

library(testthat)
library(ClinicoPath)

# Load test data
data(survival_test, package = "ClinicoPath")

test_that("survival function exists", {
  expect_true(exists("survival"))
  expect_true(is.function(survival))
})

test_that("survival runs with minimal required arguments", {
  result <- survival(
    data = survival_test,
    elapsedtime = "elapsedtime",
    outcome = "outcome"
  )

  expect_s3_class(result, "survivalClass")
  expect_true("results" %in% names(result))
})

test_that("survival runs with explanatory variable", {
  result <- survival(
    data = survival_test,
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    explanatory = "treatment"
  )

  expect_s3_class(result, "survivalClass")
})

test_that("survival handles numeric outcome correctly", {
  # Binary numeric outcome (0/1)
  result <- survival(
    data = survival_test,
    elapsedtime = "elapsedtime",
    outcome = "outcome"
  )

  expect_s3_class(result, "survivalClass")
})

test_that("survival handles factor explanatory variable", {
  result <- survival(
    data = survival_test,
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    explanatory = "treatment"
  )

  expect_s3_class(result, "survivalClass")
})

test_that("survival errors on missing elapsedtime", {
  expect_error(
    survival(
      data = survival_test,
      outcome = "outcome",
      explanatory = "treatment"
    ),
    regexp = "elapsed|time.*required|missing",
    ignore.case = TRUE
  )
})

test_that("survival errors on missing outcome", {
  expect_error(
    survival(
      data = survival_test,
      elapsedtime = "elapsedtime",
      explanatory = "treatment"
    ),
    regexp = "outcome.*required|missing",
    ignore.case = TRUE
  )
})

test_that("survival handles continuous time variable", {
  result <- survival(
    data = survival_test,
    elapsedtime = "elapsedtime",
    outcome = "outcome"
  )

  expect_s3_class(result, "survivalClass")
})

test_that("survival handles multiple treatment groups", {
  result <- survival(
    data = survival_test,
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    explanatory = "treatment"  # 3 levels: Control, Treatment A, Treatment B
  )

  expect_s3_class(result, "survivalClass")
})

test_that("survival handles binary grouping variable", {
  result <- survival(
    data = survival_test,
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    explanatory = "sex"  # 2 levels: Male, Female
  )

  expect_s3_class(result, "survivalClass")
})

test_that("survival handles ordinal grouping variable", {
  result <- survival(
    data = survival_test,
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    explanatory = "stage"  # 4 ordered levels: I-IV
  )

  expect_s3_class(result, "survivalClass")
})

test_that("survival produces expected output structure", {
  result <- survival(
    data = survival_test,
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    explanatory = "treatment"
  )

  # Check that result has results component
  expect_true("results" %in% names(result))

  # Results should be a list
  expect_type(result$results, "list")
})

test_that("survival handles small dataset", {
  data(survival_small, package = "ClinicoPath")

  result <- survival(
    data = survival_small,
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    explanatory = "treatment"
  )

  expect_s3_class(result, "survivalClass")
})

test_that("survival runs without grouping variable", {
  # Overall survival without groups
  result <- survival(
    data = survival_test,
    elapsedtime = "elapsedtime",
    outcome = "outcome"
  )

  expect_s3_class(result, "survivalClass")
})

test_that("survival accepts default options", {
  # Test with all default options
  result <- survival(
    data = survival_test,
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    explanatory = "treatment"
  )

  expect_s3_class(result, "survivalClass")
  expect_no_error(result)
})
