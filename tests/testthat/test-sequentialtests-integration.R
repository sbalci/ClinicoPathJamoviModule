# ═══════════════════════════════════════════════════════════
# Integration Tests: sequentialtests
# Purpose: Realistic clinical workflows and complete analyses
# ═══════════════════════════════════════════════════════════
library(testthat)
library(ClinicoPath)

# COVID-19 Testing Workflows ----
test_that("COVID-19 workflow: screening to confirmation strategy", {
  # Serial positive: Rapid test → PCR confirmation
  result <- sequentialtests(
    test1_name = "Rapid Antigen Test",
    test1_sens = 0.85,
    test1_spec = 0.95,
    test1_cost = 5,
    test2_name = "RT-PCR",
    test2_sens = 0.95,
    test2_spec = 0.99,
    test2_cost = 100,
    strategy = "serial_positive",
    prevalence = 0.05,
    population_size = 10000,
    show_cost_analysis = TRUE,
    show_nomogram = TRUE
  )

  expect_s3_class(result, "sequentialtestsClass")
})

test_that("COVID-19 workflow: using clinical preset", {
  result <- sequentialtests(
    preset = "covid_screening_confirmation",
    population_size = 5000,
    show_explanation = TRUE,
    show_cost_analysis = TRUE
  )

  expect_s3_class(result, "sequentialtestsClass")
})

# Cancer Screening Workflows ----
test_that("breast cancer workflow: mammography to biopsy", {
  # Serial positive strategy for cancer confirmation
  result <- sequentialtests(
    test1_name = "Mammography",
    test1_sens = 0.87,
    test1_spec = 0.88,
    test1_cost = 100,
    test2_name = "Tissue Biopsy",
    test2_sens = 0.95,
    test2_spec = 0.98,
    test2_cost = 500,
    strategy = "serial_positive",
    prevalence = 0.006,
    population_size = 100000,
    show_cost_analysis = TRUE
  )

  expect_s3_class(result, "sequentialtestsClass")
})

test_that("breast cancer workflow: using clinical preset", {
  result <- sequentialtests(
    preset = "breast_cancer_screening",
    population_size = 50000,
    show_explanation = TRUE,
    show_formulas = TRUE
  )

  expect_s3_class(result, "sequentialtestsClass")
})

# Emergency Medicine Workflows ----
test_that("MI rule-out workflow: parallel testing strategy", {
  # Parallel testing for emergency diagnosis
  result <- sequentialtests(
    test1_name = "High-Sensitivity Troponin",
    test1_sens = 0.85,
    test1_spec = 0.80,
    test1_cost = 50,
    test2_name = "ECG",
    test2_sens = 0.75,
    test2_spec = 0.85,
    test2_cost = 30,
    strategy = "parallel",
    prevalence = 0.20,
    population_size = 1000,
    show_explanation = TRUE,
    show_cost_analysis = TRUE
  )

  expect_s3_class(result, "sequentialtestsClass")
})

test_that("MI rule-out workflow: using clinical preset", {
  result <- sequentialtests(
    preset = "mi_emergency_parallel",
    population_size = 2000,
    show_nomogram = TRUE
  )

  expect_s3_class(result, "sequentialtestsClass")
})

test_that("stroke workflow: clinical assessment to imaging", {
  result <- sequentialtests(
    preset = "stroke_emergency_parallel",
    population_size = 1500,
    show_explanation = TRUE,
    show_cost_analysis = TRUE
  )

  expect_s3_class(result, "sequentialtestsClass")
})

# Infectious Disease Workflows ----
test_that("TB workflow: chest X-ray to sputum culture", {
  result <- sequentialtests(
    preset = "tb_screening_confirmation",
    population_size = 5000,
    show_cost_analysis = TRUE
  )

  expect_s3_class(result, "sequentialtestsClass")
})

test_that("HIV workflow: ELISA to Western blot", {
  result <- sequentialtests(
    preset = "hiv_screening_confirmation",
    population_size = 10000,
    show_explanation = TRUE,
    show_nomogram = TRUE
  )

  expect_s3_class(result, "sequentialtestsClass")
})

# Prostate Cancer Screening ----
test_that("prostate cancer workflow: PSA exclusion strategy", {
  # Serial negative: Only MRI if PSA negative (exclusion)
  result <- sequentialtests(
    preset = "prostate_screening_exclusion",
    population_size = 8000,
    show_cost_analysis = TRUE,
    show_formulas = TRUE
  )

  expect_s3_class(result, "sequentialtestsClass")
})

# Strategy Comparison Workflows ----
test_that("strategy comparison: serial_positive vs serial_negative", {
  # Same tests, different strategies
  test_params <- list(
    test1_sens = 0.90,
    test1_spec = 0.85,
    test1_cost = 50,
    test2_sens = 0.85,
    test2_spec = 0.95,
    test2_cost = 300,
    prevalence = 0.10,
    population_size = 10000,
    show_cost_analysis = TRUE
  )

  # Serial positive (confirmation)
  result_sp <- do.call(sequentialtests, c(test_params, strategy = "serial_positive"))
  expect_s3_class(result_sp, "sequentialtestsClass")

  # Serial negative (exclusion)
  result_sn <- do.call(sequentialtests, c(test_params, strategy = "serial_negative"))
  expect_s3_class(result_sn, "sequentialtestsClass")
})

test_that("strategy comparison: all three strategies", {
  # Compare serial_positive, serial_negative, and parallel
  strategies <- c("serial_positive", "serial_negative", "parallel")

  for (strategy in strategies) {
    result <- sequentialtests(
      test1_name = "Test A",
      test1_sens = 0.88,
      test1_spec = 0.92,
      test1_cost = 75,
      test2_name = "Test B",
      test2_sens = 0.92,
      test2_spec = 0.88,
      test2_cost = 150,
      strategy = strategy,
      prevalence = 0.15,
      population_size = 5000,
      show_cost_analysis = TRUE
    )
    expect_s3_class(result, "sequentialtestsClass")
  }
})

# Cost-Effectiveness Workflows ----
test_that("cost-effectiveness: low-cost screening vs high-cost confirmation", {
  result <- sequentialtests(
    test1_name = "Low-Cost Screening",
    test1_sens = 0.90,
    test1_spec = 0.80,
    test1_cost = 10,
    test2_name = "High-Cost Confirmation",
    test2_sens = 0.95,
    test2_spec = 0.98,
    test2_cost = 1000,
    strategy = "serial_positive",
    prevalence = 0.05,
    population_size = 20000,
    show_cost_analysis = TRUE
  )

  expect_s3_class(result, "sequentialtestsClass")
})

test_that("cost-effectiveness: expensive screening inappropriate", {
  # Shows why expensive test shouldn't be used for screening
  result <- sequentialtests(
    test1_name = "Expensive Screening (Inappropriate)",
    test1_sens = 0.95,
    test1_spec = 0.90,
    test1_cost = 800,
    test2_name = "Cheaper Confirmation",
    test2_sens = 0.90,
    test2_spec = 0.95,
    test2_cost = 200,
    strategy = "serial_positive",
    prevalence = 0.01,
    population_size = 10000,
    show_cost_analysis = TRUE
  )

  expect_s3_class(result, "sequentialtestsClass")
})

# Prevalence Sensitivity Analysis ----
test_that("prevalence sensitivity: rare disease (0.1%)", {
  result <- sequentialtests(
    test1_sens = 0.95,
    test1_spec = 0.90,
    test2_sens = 0.90,
    test2_spec = 0.98,
    strategy = "serial_positive",
    prevalence = 0.001,
    population_size = 100000,
    show_nomogram = TRUE
  )

  expect_s3_class(result, "sequentialtestsClass")
})

test_that("prevalence sensitivity: common disease (50%)", {
  result <- sequentialtests(
    test1_sens = 0.85,
    test1_spec = 0.85,
    test2_sens = 0.90,
    test2_spec = 0.92,
    strategy = "serial_negative",
    prevalence = 0.50,
    population_size = 5000
  )

  expect_s3_class(result, "sequentialtestsClass")
})

# Teaching and Educational Workflows ----
test_that("teaching scenario: perfect tests in sequence", {
  result <- sequentialtests(
    test1_name = "Perfect Screening Test",
    test1_sens = 0.99,
    test1_spec = 0.99,
    test2_name = "Perfect Confirmation Test",
    test2_sens = 0.99,
    test2_spec = 0.99,
    strategy = "serial_positive",
    prevalence = 0.10,
    population_size = 1000,
    show_explanation = TRUE,
    show_formulas = TRUE
  )

  expect_s3_class(result, "sequentialtestsClass")
})

test_that("teaching scenario: poor tests demonstrate strategy impact", {
  result <- sequentialtests(
    test1_name = "Poor Screening",
    test1_sens = 0.65,
    test1_spec = 0.70,
    test2_name = "Poor Confirmation",
    test2_sens = 0.70,
    test2_spec = 0.75,
    strategy = "serial_positive",
    prevalence = 0.10,
    population_size = 1000,
    show_explanation = TRUE
  )

  expect_s3_class(result, "sequentialtestsClass")
})

# Comprehensive Analysis Workflows ----
test_that("comprehensive workflow: all options enabled", {
  result <- sequentialtests(
    test1_name = "Comprehensive Test 1",
    test1_sens = 0.88,
    test1_spec = 0.92,
    test1_cost = 100,
    test2_name = "Comprehensive Test 2",
    test2_sens = 0.92,
    test2_spec = 0.95,
    test2_cost = 400,
    strategy = "serial_positive",
    prevalence = 0.12,
    population_size = 15000,
    show_explanation = TRUE,
    show_formulas = TRUE,
    show_cost_analysis = TRUE,
    show_nomogram = TRUE
  )

  expect_s3_class(result, "sequentialtestsClass")
})

test_that("publication-ready workflow: detailed reporting", {
  # Realistic scenario with all reporting options
  result <- sequentialtests(
    test1_name = "Initial Diagnostic Test",
    test1_sens = 0.85,
    test1_spec = 0.88,
    test1_cost = 75,
    test2_name = "Confirmatory Diagnostic Test",
    test2_sens = 0.92,
    test2_spec = 0.96,
    test2_cost = 350,
    strategy = "serial_positive",
    prevalence = 0.08,
    population_size = 12000,
    show_explanation = TRUE,
    show_formulas = TRUE,
    show_cost_analysis = TRUE,
    show_nomogram = TRUE
  )

  expect_s3_class(result, "sequentialtestsClass")
})

# Clinical Preset Comprehensive Testing ----
test_that("all clinical presets execute without error", {
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
    result <- sequentialtests(
      preset = preset,
      population_size = 10000,
      show_cost_analysis = TRUE
    )
    expect_s3_class(result, "sequentialtestsClass")
  }
})

# Multi-Step Sequential Analysis ----
test_that("multi-step workflow: triage → screening → confirmation", {
  # Step 1: Clinical triage (cheap, sensitive)
  triage <- sequentialtests(
    test1_name = "Clinical Symptoms",
    test1_sens = 0.95,
    test1_spec = 0.60,
    test1_cost = 0,
    test2_name = "Rapid Test",
    test2_sens = 0.85,
    test2_spec = 0.90,
    test2_cost = 10,
    strategy = "serial_positive",
    prevalence = 0.15,
    population_size = 10000
  )
  expect_s3_class(triage, "sequentialtestsClass")

  # Step 2: Rapid test → PCR confirmation (specific)
  confirmation <- sequentialtests(
    test1_name = "Rapid Test",
    test1_sens = 0.85,
    test1_spec = 0.90,
    test1_cost = 10,
    test2_name = "Laboratory PCR",
    test2_sens = 0.98,
    test2_spec = 0.99,
    test2_cost = 150,
    strategy = "serial_positive",
    prevalence = 0.40,  # Higher after triage
    population_size = 1000
  )
  expect_s3_class(confirmation, "sequentialtestsClass")
})

# Real-World Decision Making ----
test_that("clinical decision: when to use serial_positive", {
  # High specificity needed (confirmation)
  result <- sequentialtests(
    test1_name = "Sensitive Screening",
    test1_sens = 0.95,
    test1_spec = 0.75,
    test1_cost = 20,
    test2_name = "Specific Confirmation",
    test2_sens = 0.80,
    test2_spec = 0.98,
    test2_cost = 400,
    strategy = "serial_positive",
    prevalence = 0.05,
    population_size = 10000,
    show_explanation = TRUE,
    show_cost_analysis = TRUE
  )

  expect_s3_class(result, "sequentialtestsClass")
})

test_that("clinical decision: when to use serial_negative", {
  # High sensitivity needed (exclusion)
  result <- sequentialtests(
    test1_name = "Specific Screening",
    test1_sens = 0.75,
    test1_spec = 0.95,
    test1_cost = 50,
    test2_name = "Sensitive Follow-up",
    test2_sens = 0.95,
    test2_spec = 0.80,
    test2_cost = 300,
    strategy = "serial_negative",
    prevalence = 0.20,
    population_size = 5000,
    show_explanation = TRUE,
    show_cost_analysis = TRUE
  )

  expect_s3_class(result, "sequentialtestsClass")
})

test_that("clinical decision: when to use parallel", {
  # Maximum sensitivity needed (emergency)
  result <- sequentialtests(
    test1_name = "Test A",
    test1_sens = 0.85,
    test1_spec = 0.82,
    test1_cost = 60,
    test2_name = "Test B",
    test2_sens = 0.80,
    test2_spec = 0.88,
    test2_cost = 80,
    strategy = "parallel",
    prevalence = 0.30,
    population_size = 2000,
    show_explanation = TRUE,
    show_cost_analysis = TRUE
  )

  expect_s3_class(result, "sequentialtestsClass")
})

# Population Size Impact ----
test_that("population size impact: small clinic", {
  result <- sequentialtests(
    test1_sens = 0.90,
    test1_spec = 0.85,
    test2_sens = 0.85,
    test2_spec = 0.95,
    strategy = "serial_positive",
    prevalence = 0.10,
    population_size = 100,
    show_cost_analysis = TRUE
  )

  expect_s3_class(result, "sequentialtestsClass")
})

test_that("population size impact: large screening program", {
  result <- sequentialtests(
    test1_sens = 0.90,
    test1_spec = 0.85,
    test2_sens = 0.85,
    test2_spec = 0.95,
    strategy = "serial_positive",
    prevalence = 0.10,
    population_size = 100000,
    show_cost_analysis = TRUE
  )

  expect_s3_class(result, "sequentialtestsClass")
})
