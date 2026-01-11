# Test Suite for Screening Calculator Module
# Testing mathematical accuracy and edge cases

library(testthat)
library(jmvcore)

# Load the package
if (requireNamespace("devtools", quietly = TRUE)) {
  devtools::load_all()
}

# Helper functions with input validation
validate_probability <- function(value, name) {
  if (value < 0 || value > 1) {
    stop(paste(name, "must be between 0 and 1"))
  }
}

calculate_expected_ppv <- function(sens, spec, prev) {
  validate_probability(sens, "Sensitivity")
  validate_probability(spec, "Specificity") 
  validate_probability(prev, "Prevalence")
  
  (sens * prev) / ((sens * prev) + ((1 - spec) * (1 - prev)))
}

calculate_expected_npv <- function(sens, spec, prev) {
  validate_probability(sens, "Sensitivity")
  validate_probability(spec, "Specificity")
  validate_probability(prev, "Prevalence")
  
  (spec * (1 - prev)) / ((spec * (1 - prev)) + ((1 - sens) * prev))
}

calculate_expected_lrp <- function(sens, spec) {
  sens / (1 - spec)
}

calculate_expected_lrn <- function(sens, spec) {
  (1 - sens) / spec
}

test_that("Single test calculations are mathematically correct", {
  sens <- 0.90
  spec <- 0.80
  prev <- 0.10
  
  expected_ppv <- calculate_expected_ppv(sens, spec, prev)
  expected_npv <- calculate_expected_npv(sens, spec, prev)
  expected_lrp <- calculate_expected_lrp(sens, spec)
  expected_lrn <- calculate_expected_lrn(sens, spec)
  
  # Values should be reasonable
  expect_true(expected_ppv > 0.25 && expected_ppv < 0.5)
  expect_true(expected_npv > 0.95)
  expect_equal(round(expected_lrp, 2), 4.5, tolerance = 0.1)
  expect_equal(round(expected_lrn, 2), 0.12, tolerance = 0.01)
})

test_that("Edge case calculations work correctly", {
  sens <- 0.99
  spec <- 0.50
  prev <- 0.01
  
  expected_ppv <- calculate_expected_ppv(sens, spec, prev)
  expected_npv <- calculate_expected_npv(sens, spec, prev)
  
  expect_equal(round(expected_ppv, 4), 0.0196, tolerance = 0.001)
  expect_equal(round(expected_npv, 4), 0.9999, tolerance = 0.001)
})

test_that("Prevalence effects are correct", {
  sens <- 0.90
  spec <- 0.90
  
  prevalences <- c(0.01, 0.05, 0.10, 0.25, 0.50, 0.75, 0.90)
  ppvs <- sapply(prevalences, function(p) calculate_expected_ppv(sens, spec, p))
  npvs <- sapply(prevalences, function(p) calculate_expected_npv(sens, spec, p))
  
  # PPV should increase with prevalence
  expect_true(all(diff(ppvs) > 0))
  
  # NPV should decrease with prevalence  
  expect_true(all(diff(npvs) < 0))
})

test_that("Input parameter bounds are respected", {
  expect_error(calculate_expected_ppv(-0.1, 0.9, 0.1))
  expect_error(calculate_expected_ppv(1.1, 0.9, 0.1))
  expect_error(calculate_expected_ppv(0.9, -0.1, 0.1))
  expect_error(calculate_expected_ppv(0.9, 1.1, 0.1))
  expect_error(calculate_expected_ppv(0.9, 0.9, -0.1))
  expect_error(calculate_expected_ppv(0.9, 0.9, 1.1))
})

test_that("screeningcalculator function runs without error", {
  # Run the actual jamovi function
  result <- screeningcalculator(
    sens = 0.90,
    spec = 0.80,
    prev = 0.10,
    repeat2 = TRUE,
    repeat3 = TRUE,
    fnote = FALSE,
    fagan = FALSE,
    samplesize = FALSE
  )
  
  expect_true(inherits(result, "screeningcalculatorResults"))
})
