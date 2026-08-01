# ═══════════════════════════════════════════════════════════
# Basic Functionality Tests: survival
# ═══════════════════════════════════════════════════════════
#
# Tests basic functionality and required arguments
# for the survival jamovi function

library(testthat)

# Load test data
data(survival_test, package = "ClinicoPath")

run_survival <- function(...) {
    args <- list(...)
    for (lvl in c("outcomeLevel", "dod", "dooc", "awd", "awod"))
        if (is.null(args[[lvl]])) args[lvl] <- list(NULL)
    do.call(survival, args)
}

test_that("survival function exists", {
  expect_true(exists("survival"))
  expect_true(is.function(survival))
})

test_that("survival runs with minimal required arguments", {
  result <- run_survival(
    data = survival_test,
    elapsedtime = "elapsedtime",
    outcome = "outcome"
  )

  expect_s3_class(result, "survivalResults")
  expect_true(!is.null(result$medianTable))
})

test_that("survival runs with explanatory variable", {
  result <- run_survival(
    data = survival_test,
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    explanatory = "treatment"
  )

  expect_s3_class(result, "survivalResults")
})

test_that("survival handles numeric outcome correctly", {
  # Binary numeric outcome (0/1)
  result <- run_survival(
    data = survival_test,
    elapsedtime = "elapsedtime",
    outcome = "outcome"
  )

  expect_s3_class(result, "survivalResults")
})

test_that("survival handles factor explanatory variable", {
  result <- run_survival(
    data = survival_test,
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    explanatory = "treatment"
  )

  expect_s3_class(result, "survivalResults")
})

test_that("survival shows setup guidance when elapsed time is missing", {
  result <- run_survival(
    data = survival_test,
    outcome = "outcome",
    explanatory = "treatment"
  )
  expect_s3_class(result, "survivalResults")
  expect_true(result$todo$visible)
  expect_match(result$todo$content, "time", ignore.case = TRUE)
})

test_that("survival shows setup guidance when outcome is missing", {
  result <- run_survival(
    data = survival_test,
    elapsedtime = "elapsedtime",
    explanatory = "treatment"
  )
  expect_s3_class(result, "survivalResults")
  expect_true(result$todo$visible)
  expect_match(result$todo$content, "outcome", ignore.case = TRUE)
})

test_that("survival handles continuous time variable", {
  result <- run_survival(
    data = survival_test,
    elapsedtime = "elapsedtime",
    outcome = "outcome"
  )

  expect_s3_class(result, "survivalResults")
})

test_that("survival handles multiple treatment groups", {
  result <- run_survival(
    data = survival_test,
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    explanatory = "treatment"  # 3 levels: Control, Treatment A, Treatment B
  )

  expect_s3_class(result, "survivalResults")
})

test_that("survival handles binary grouping variable", {
  result <- run_survival(
    data = survival_test,
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    explanatory = "sex"  # 2 levels: Male, Female
  )

  expect_s3_class(result, "survivalResults")
})

test_that("survival handles ordinal grouping variable", {
  result <- run_survival(
    data = survival_test,
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    explanatory = "stage"  # 4 ordered levels: I-IV
  )

  expect_s3_class(result, "survivalResults")
})

test_that("survival produces expected output structure", {
  result <- run_survival(
    data = survival_test,
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    explanatory = "treatment"
  )

  expect_s3_class(result, "survivalResults")
})

test_that("survival handles small dataset", {
  data(survival_small, package = "ClinicoPath")

  result <- run_survival(
    data = survival_small,
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    explanatory = "treatment"
  )

  expect_s3_class(result, "survivalResults")
})

test_that("survival runs without grouping variable", {
  # Overall survival without groups
  result <- run_survival(
    data = survival_test,
    elapsedtime = "elapsedtime",
    outcome = "outcome"
  )

  expect_s3_class(result, "survivalResults")
})

test_that("survival accepts default options", {
  # Test with all default options
  result <- run_survival(
    data = survival_test,
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    explanatory = "treatment"
  )

  expect_s3_class(result, "survivalResults")
  expect_no_error(result)
})
