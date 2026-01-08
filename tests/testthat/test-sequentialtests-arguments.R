# ═══════════════════════════════════════════════════════════
# Argument Tests: sequentialtests
# ═══════════════════════════════════════════════════════════
library(testthat)
library(ClinicoPath)

test_that("sequentialtests respects all presets", {
  presets <- c(
    "covid_screening_confirmation",
    "breast_cancer_screening",
    "mi_emergency_parallel",
    "tb_screening_confirmation",
    "prostate_screening_exclusion",
    "hiv_screening_confirmation",
    "stroke_emergency_parallel"
  )

  for (preset in presets) {
    result <- sequentialtests(preset = preset)
    expect_no_error(result)
  }
})

test_that("sequentialtests respects strategy parameter", {
  strategies <- c("serial_positive", "serial_negative", "parallel")

  for (strategy in strategies) {
    result <- sequentialtests(
      test1_sens = 0.90,
      test1_spec = 0.85,
      test2_sens = 0.85,
      test2_spec = 0.95,
      prevalence = 0.10,
      strategy = strategy
    )
    expect_no_error(result)
  }
})

test_that("sequentialtests respects show_explanation parameter", {
  result_with <- sequentialtests(
    test1_sens = 0.90,
    test1_spec = 0.85,
    test2_sens = 0.85,
    test2_spec = 0.95,
    prevalence = 0.10,
    show_explanation = TRUE
  )
  expect_no_error(result_with)

  result_without <- sequentialtests(
    test1_sens = 0.90,
    test1_spec = 0.85,
    test2_sens = 0.85,
    test2_spec = 0.95,
    prevalence = 0.10,
    show_explanation = FALSE
  )
  expect_no_error(result_without)
})

test_that("sequentialtests respects show_formulas parameter", {
  result <- sequentialtests(
    test1_sens = 0.90,
    test1_spec = 0.85,
    test2_sens = 0.85,
    test2_spec = 0.95,
    prevalence = 0.10,
    show_formulas = TRUE
  )
  expect_no_error(result)
})

test_that("sequentialtests respects show_cost_analysis parameter", {
  result_with <- sequentialtests(
    test1_sens = 0.90,
    test1_spec = 0.85,
    test1_cost = 50,
    test2_sens = 0.85,
    test2_spec = 0.95,
    test2_cost = 300,
    prevalence = 0.10,
    show_cost_analysis = TRUE
  )
  expect_no_error(result_with)

  result_without <- sequentialtests(
    test1_sens = 0.90,
    test1_spec = 0.85,
    test2_sens = 0.85,
    test2_spec = 0.95,
    prevalence = 0.10,
    show_cost_analysis = FALSE
  )
  expect_no_error(result_without)
})

test_that("sequentialtests respects show_nomogram parameter", {
  result_with <- sequentialtests(
    test1_sens = 0.90,
    test1_spec = 0.85,
    test2_sens = 0.85,
    test2_spec = 0.95,
    prevalence = 0.10,
    show_nomogram = TRUE
  )
  expect_no_error(result_with)

  result_without <- sequentialtests(
    test1_sens = 0.90,
    test1_spec = 0.85,
    test2_sens = 0.85,
    test2_spec = 0.95,
    prevalence = 0.10,
    show_nomogram = FALSE
  )
  expect_no_error(result_without)
})

test_that("sequentialtests handles different prevalence values", {
  prevalences <- c(0.001, 0.01, 0.05, 0.10, 0.30, 0.50, 0.80, 0.99)

  for (prev in prevalences) {
    result <- sequentialtests(
      test1_sens = 0.90,
      test1_spec = 0.85,
      test2_sens = 0.85,
      test2_spec = 0.95,
      prevalence = prev,
      strategy = "serial_positive"
    )
    expect_no_error(result)
  }
})

test_that("sequentialtests handles test names", {
  result <- sequentialtests(
    test1_name = "Rapid Antigen Test",
    test1_sens = 0.85,
    test1_spec = 0.95,
    test2_name = "RT-PCR",
    test2_sens = 0.95,
    test2_spec = 0.99,
    prevalence = 0.05,
    strategy = "serial_positive"
  )
  expect_no_error(result)
})

test_that("sequentialtests handles cost parameters", {
  result <- sequentialtests(
    test1_sens = 0.90,
    test1_spec = 0.85,
    test1_cost = 100,
    test2_sens = 0.85,
    test2_spec = 0.95,
    test2_cost = 500,
    prevalence = 0.10,
    strategy = "serial_positive",
    show_cost_analysis = TRUE
  )
  expect_no_error(result)
})

test_that("sequentialtests handles all options combined", {
  result <- sequentialtests(
    test1_name = "Screening Test",
    test1_sens = 0.90,
    test1_spec = 0.85,
    test1_cost = 50,
    test2_name = "Confirmatory Test",
    test2_sens = 0.85,
    test2_spec = 0.95,
    test2_cost = 300,
    prevalence = 0.10,
    population_size = 10000,
    strategy = "serial_positive",
    show_explanation = TRUE,
    show_formulas = TRUE,
    show_cost_analysis = TRUE,
    show_nomogram = TRUE
  )
  expect_no_error(result)
})
