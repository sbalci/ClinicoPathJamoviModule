# Tests for diagnosticsamplesize function

library(testthat)
library(jmvcore)

# Load the package
if (requireNamespace("devtools", quietly = TRUE)) {
  devtools::load_all()
}

test_that("diagnosticsamplesize works with default parameters", {
  result <- diagnosticsamplesize(
    study_purpose = "diagnostic",
    target_sensitivity = 0.90,
    target_specificity = 0.90,
    prevalence = 0.10,
    ci_width = 0.10,
    alpha = 0.05,
    nonresponse_rate = 0,
    show_statement = TRUE,
    show_methodology = FALSE,
    show_references = FALSE,
    show_comparison_table = FALSE,
    show_method_comparison = FALSE
  )
  
  expect_true(inherits(result, "diagnosticsamplesizeResults"))
})

test_that("diagnosticsamplesize screening sensitivity mode works", {
  result <- diagnosticsamplesize(
    study_purpose = "screening_sens",
    target_sensitivity = 0.95,
    target_specificity = 0.80,
    prevalence = 0.05,
    ci_width = 0.10,
    alpha = 0.05
  )
  
  expect_true(inherits(result, "diagnosticsamplesizeResults"))
})

test_that("diagnosticsamplesize screening specificity mode works", {
  result <- diagnosticsamplesize(
    study_purpose = "screening_spec",
    target_sensitivity = 0.80,
    target_specificity = 0.95,
    prevalence = 0.20,
    ci_width = 0.10,
    alpha = 0.05
  )
  
  expect_true(inherits(result, "diagnosticsamplesizeResults"))
})
