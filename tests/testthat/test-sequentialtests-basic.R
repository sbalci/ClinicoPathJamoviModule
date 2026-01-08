# ═══════════════════════════════════════════════════════════
# Basic Tests: sequentialtests
# ═══════════════════════════════════════════════════════════
library(testthat)
library(ClinicoPath)

test_that("sequentialtests returns proper class", {
  result <- sequentialtests(
    test1_sens = 0.90,
    test1_spec = 0.85,
    test2_sens = 0.85,
    test2_spec = 0.95,
    prevalence = 0.10,
    strategy = "serial_positive"
  )
  expect_s3_class(result, "sequentialtestsClass")
})

test_that("sequentialtests handles serial positive strategy", {
  result <- sequentialtests(
    test1_name = "Screening Test",
    test1_sens = 0.90,
    test1_spec = 0.85,
    test2_name = "Confirmatory Test",
    test2_sens = 0.85,
    test2_spec = 0.95,
    prevalence = 0.10,
    strategy = "serial_positive"
  )
  expect_no_error(result)
})

test_that("sequentialtests handles serial negative strategy", {
  result <- sequentialtests(
    test1_sens = 0.90,
    test1_spec = 0.85,
    test2_sens = 0.85,
    test2_spec = 0.95,
    prevalence = 0.10,
    strategy = "serial_negative"
  )
  expect_no_error(result)
})

test_that("sequentialtests handles parallel strategy", {
  result <- sequentialtests(
    test1_sens = 0.95,
    test1_spec = 0.80,
    test2_sens = 0.90,
    test2_spec = 0.92,
    prevalence = 0.15,
    strategy = "parallel"
  )
  expect_no_error(result)
})

test_that("sequentialtests handles COVID preset", {
  result <- sequentialtests(
    preset = "covid_screening_confirmation"
  )
  expect_no_error(result)
})

test_that("sequentialtests handles breast cancer preset", {
  result <- sequentialtests(
    preset = "breast_cancer_screening"
  )
  expect_no_error(result)
})

test_that("sequentialtests includes cost analysis", {
  result <- sequentialtests(
    test1_sens = 0.90,
    test1_spec = 0.85,
    test1_cost = 50,
    test2_sens = 0.85,
    test2_spec = 0.95,
    test2_cost = 300,
    prevalence = 0.10,
    strategy = "serial_positive",
    show_cost_analysis = TRUE
  )
  expect_no_error(result)
})

test_that("sequentialtests shows nomogram", {
  result <- sequentialtests(
    test1_sens = 0.90,
    test1_spec = 0.85,
    test2_sens = 0.85,
    test2_spec = 0.95,
    prevalence = 0.10,
    strategy = "serial_positive",
    show_nomogram = TRUE
  )
  expect_no_error(result)
})

test_that("sequentialtests shows explanations", {
  result <- sequentialtests(
    test1_sens = 0.90,
    test1_spec = 0.85,
    test2_sens = 0.85,
    test2_spec = 0.95,
    prevalence = 0.10,
    strategy = "serial_positive",
    show_explanation = TRUE
  )
  expect_no_error(result)
})

test_that("sequentialtests handles different population sizes", {
  result <- sequentialtests(
    test1_sens = 0.90,
    test1_spec = 0.85,
    test2_sens = 0.85,
    test2_spec = 0.95,
    prevalence = 0.10,
    strategy = "serial_positive",
    population_size = 100000
  )
  expect_no_error(result)
})
