# ═══════════════════════════════════════════════════════════
# Integration Tests: decisioncalculator
# ═══════════════════════════════════════════════════════════
#
# Tests integration with realistic workflows and output consistency
# for the decisioncalculator (Medical Decision Calculator) jamovi function

library(testthat)

# Load test data
data(decisioncalculator_scenarios, package = "ClinicoPath")
data(decisioncalculator_screening, package = "ClinicoPath")
data(decisioncalculator_confirmatory, package = "ClinicoPath")
data(decisioncalculator_biomarker, package = "ClinicoPath")
data(decisioncalculator_prevalence, package = "ClinicoPath")
data(decisioncalculator_multiplecuts, package = "ClinicoPath")
data(decisioncalculator_imaging, package = "ClinicoPath")
data(decisioncalculator_pointofcare, package = "ClinicoPath")

# ═══════════════════════════════════════════════════════════
# Consistency Tests
# ═══════════════════════════════════════════════════════════

test_that("produces consistent results across multiple runs", {
  result1 <- decisioncalculator(
    TP = 90, TN = 80, FP = 20, FN = 10,
    ci = TRUE
  )

  result2 <- decisioncalculator(
    TP = 90, TN = 80, FP = 20, FN = 10,
    ci = TRUE
  )

  expect_s3_class(result1, "decisioncalculatorResults")
  expect_s3_class(result2, "decisioncalculatorResults")
})

# ═══════════════════════════════════════════════════════════
# Workflow: Basic Screening Test Evaluation
# ═══════════════════════════════════════════════════════════

test_that("workflow: screening test evaluation", {
  # Step 1: Basic evaluation
  basic <- decisioncalculator(
    TP = decisioncalculator_screening$TP,
    TN = decisioncalculator_screening$TN,
    FP = decisioncalculator_screening$FP,
    FN = decisioncalculator_screening$FN
  )
  expect_s3_class(basic, "decisioncalculatorResults")

  # Step 2: Add confidence intervals
  with_ci <- decisioncalculator(
    TP = decisioncalculator_screening$TP,
    TN = decisioncalculator_screening$TN,
    FP = decisioncalculator_screening$FP,
    FN = decisioncalculator_screening$FN,
    ci = TRUE
  )
  expect_s3_class(with_ci, "decisioncalculatorResults")

  # Step 3: Add population prevalence and Fagan nomogram
  complete <- decisioncalculator(
    TP = decisioncalculator_screening$TP,
    TN = decisioncalculator_screening$TN,
    FP = decisioncalculator_screening$FP,
    FN = decisioncalculator_screening$FN,
    ci = TRUE,
    pp = TRUE,
    pprob = 0.005,  # Population prevalence
    fagan = TRUE
  )
  expect_s3_class(complete, "decisioncalculatorResults")
})

# ═══════════════════════════════════════════════════════════
# Workflow: Confirmatory Test After Screening
# ═══════════════════════════════════════════════════════════

test_that("workflow: sequential screening then confirmatory testing", {
  # First: Screening test
  screening <- decisioncalculator(
    TP = 45, TN = 808, FP = 142, FN = 5,
    pp = TRUE,
    pprob = 0.005,
    ci = TRUE
  )
  expect_s3_class(screening, "decisioncalculatorResults")

  # Second: Confirmatory test on screen-positives
  confirmatory <- decisioncalculator(
    TP = decisioncalculator_confirmatory$TP,
    TN = decisioncalculator_confirmatory$TN,
    FP = decisioncalculator_confirmatory$FP,
    FN = decisioncalculator_confirmatory$FN,
    pp = TRUE,
    pprob = 0.24,  # Adjusted prevalence after screening
    ci = TRUE,
    fagan = TRUE
  )
  expect_s3_class(confirmatory, "decisioncalculatorResults")
})

# ═══════════════════════════════════════════════════════════
# Workflow: Biomarker Validation Study
# ═══════════════════════════════════════════════════════════

test_that("workflow: biomarker validation with full documentation", {
  result <- decisioncalculator(
    TP = decisioncalculator_biomarker$TP,
    TN = decisioncalculator_biomarker$TN,
    FP = decisioncalculator_biomarker$FP,
    FN = decisioncalculator_biomarker$FN,
    ci = TRUE,
    pp = TRUE,
    pprob = 0.15,
    fagan = TRUE,
    fnote = TRUE,
    showSummary = TRUE,
    showAbout = TRUE,
    showGlossary = TRUE
  )

  expect_s3_class(result, "decisioncalculatorResults")
})

# ═══════════════════════════════════════════════════════════
# Workflow: Multiple Cut-off Optimization
# ═══════════════════════════════════════════════════════════

test_that("workflow: compare multiple diagnostic thresholds", {
  # Extract data from multiplecuts dataset
  scenario1 <- decisioncalculator_multiplecuts[1, ]
  scenario2 <- decisioncalculator_multiplecuts[2, ]
  scenario3 <- decisioncalculator_multiplecuts[3, ]

  # Compare all three cut-offs
  result <- decisioncalculator(
    TP = scenario1$TP,
    TN = scenario1$TN,
    FP = scenario1$FP,
    FN = scenario1$FN,
    multiplecuts = TRUE,
    cutoff1 = "Standard",
    tp1 = scenario2$TP,
    tn1 = scenario2$TN,
    fp1 = scenario2$FP,
    fn1 = scenario2$FN,
    cutoff2 = "Aggressive",
    tp2 = scenario3$TP,
    tn2 = scenario3$TN,
    fp2 = scenario3$FP,
    fn2 = scenario3$FN,
    ci = TRUE
  )

  expect_s3_class(result, "decisioncalculatorResults")
})

# ═══════════════════════════════════════════════════════════
# Workflow: Prevalence Effect Demonstration
# ═══════════════════════════════════════════════════════════

test_that("workflow: demonstrate prevalence effect on PPV/NPV", {
  # Same test, low prevalence
  low_prev <- decisioncalculator(
    TP = 43, TN = 903, FP = 47, FN = 7,
    pp = TRUE,
    pprob = 0.05,
    ci = TRUE,
    fagan = TRUE
  )
  expect_s3_class(low_prev, "decisioncalculatorResults")

  # Same test characteristics, high prevalence
  high_prev <- decisioncalculator(
    TP = 85, TN = 133, FP = 7, FN = 15,
    pp = TRUE,
    pprob = 0.417,
    ci = TRUE,
    fagan = TRUE
  )
  expect_s3_class(high_prev, "decisioncalculatorResults")
})

# ═══════════════════════════════════════════════════════════
# Workflow: Point-of-Care Test Evaluation
# ═══════════════════════════════════════════════════════════

test_that("workflow: rapid point-of-care test assessment", {
  result <- decisioncalculator(
    TP = decisioncalculator_pointofcare$TP,
    TN = decisioncalculator_pointofcare$TN,
    FP = decisioncalculator_pointofcare$FP,
    FN = decisioncalculator_pointofcare$FN,
    ci = TRUE,
    pp = TRUE,
    pprob = 0.10,
    fagan = TRUE,
    showSummary = TRUE
  )

  expect_s3_class(result, "decisioncalculatorResults")
})

# ═══════════════════════════════════════════════════════════
# Workflow: Imaging Study Evaluation
# ═══════════════════════════════════════════════════════════

test_that("workflow: imaging study performance assessment", {
  result <- decisioncalculator(
    TP = decisioncalculator_imaging$TP,
    TN = decisioncalculator_imaging$TN,
    FP = decisioncalculator_imaging$FP,
    FN = decisioncalculator_imaging$FN,
    ci = TRUE,
    pp = TRUE,
    pprob = 0.25,
    fagan = TRUE,
    showSummary = TRUE,
    showGlossary = TRUE
  )

  expect_s3_class(result, "decisioncalculatorResults")
})

# ═══════════════════════════════════════════════════════════
# Workflow: Publication-Ready Analysis
# ═══════════════════════════════════════════════════════════

test_that("workflow: complete publication-ready analysis", {
  result <- decisioncalculator(
    TP = 90,
    TN = 80,
    FP = 20,
    FN = 10,
    ci = TRUE,
    pp = TRUE,
    pprob = 0.15,
    fagan = TRUE,
    fnote = TRUE,
    showSummary = TRUE,
    showAbout = TRUE
  )

  expect_s3_class(result, "decisioncalculatorResults")
})

# ═══════════════════════════════════════════════════════════
# Iterative Analysis Workflows
# ═══════════════════════════════════════════════════════════

test_that("workflow: iterative analysis from simple to comprehensive", {
  # Start simple
  step1 <- decisioncalculator(
    TP = 90, TN = 80, FP = 20, FN = 10
  )
  expect_s3_class(step1, "decisioncalculatorResults")

  # Add CI
  step2 <- decisioncalculator(
    TP = 90, TN = 80, FP = 20, FN = 10,
    ci = TRUE
  )
  expect_s3_class(step2, "decisioncalculatorResults")

  # Add prevalence
  step3 <- decisioncalculator(
    TP = 90, TN = 80, FP = 20, FN = 10,
    ci = TRUE,
    pp = TRUE,
    pprob = 0.15
  )
  expect_s3_class(step3, "decisioncalculatorResults")

  # Add visualization
  step4 <- decisioncalculator(
    TP = 90, TN = 80, FP = 20, FN = 10,
    ci = TRUE,
    pp = TRUE,
    pprob = 0.15,
    fagan = TRUE
  )
  expect_s3_class(step4, "decisioncalculatorResults")

  # Complete analysis
  step5 <- decisioncalculator(
    TP = 90, TN = 80, FP = 20, FN = 10,
    ci = TRUE,
    pp = TRUE,
    pprob = 0.15,
    fagan = TRUE,
    fnote = TRUE,
    showSummary = TRUE
  )
  expect_s3_class(step5, "decisioncalculatorResults")
})

# ═══════════════════════════════════════════════════════════
# Scenario Comparison Workflows
# ═══════════════════════════════════════════════════════════

test_that("workflow: compare screening vs confirmatory tests", {
  # High sensitivity screening
  screening <- decisioncalculator(
    TP = 95, TN = 150, FP = 50, FN = 5,
    ci = TRUE
  )
  expect_s3_class(screening, "decisioncalculatorResults")

  # High specificity confirmatory
  confirmatory <- decisioncalculator(
    TP = 85, TN = 195, FP = 5, FN = 15,
    ci = TRUE
  )
  expect_s3_class(confirmatory, "decisioncalculatorResults")
})

test_that("workflow: evaluate all scenarios from dataset", {
  # Iterate through all 15 scenarios
  for (i in 1:nrow(decisioncalculator_scenarios)) {
    scenario <- decisioncalculator_scenarios[i, ]

    result <- decisioncalculator(
      TP = scenario$TP,
      TN = scenario$TN,
      FP = scenario$FP,
      FN = scenario$FN,
      ci = TRUE
    )

    expect_s3_class(result, "decisioncalculatorResults")
  }
})

# ═══════════════════════════════════════════════════════════
# Educational Workflows
# ═══════════════════════════════════════════════════════════

test_that("workflow: demonstrate all documentation features for teaching", {
  result <- decisioncalculator(
    TP = 90,
    TN = 80,
    FP = 20,
    FN = 10,
    ci = TRUE,
    pp = TRUE,
    pprob = 0.15,
    fagan = TRUE,
    fnote = TRUE,
    showWelcome = TRUE,
    showSummary = TRUE,
    showAbout = TRUE,
    showGlossary = TRUE
  )

  expect_s3_class(result, "decisioncalculatorResults")
})

# ═══════════════════════════════════════════════════════════
# Sensitivity Analysis Workflows
# ═══════════════════════════════════════════════════════════

test_that("workflow: sensitivity analysis across prevalence values", {
  prevalences <- c(0.01, 0.05, 0.10, 0.20, 0.30, 0.50)

  for (prev in prevalences) {
    result <- decisioncalculator(
      TP = 90, TN = 80, FP = 20, FN = 10,
      pp = TRUE,
      pprob = prev,
      ci = TRUE
    )

    expect_s3_class(result, "decisioncalculatorResults")
  }
})

# ═══════════════════════════════════════════════════════════
# Data Format Handling
# ═══════════════════════════════════════════════════════════

test_that("workflow: extracting counts from dataset and using in calculator", {
  # Extract from screening dataset
  result <- decisioncalculator(
    TP = as.numeric(decisioncalculator_screening$TP),
    TN = as.numeric(decisioncalculator_screening$TN),
    FP = as.numeric(decisioncalculator_screening$FP),
    FN = as.numeric(decisioncalculator_screening$FN),
    ci = TRUE
  )

  expect_s3_class(result, "decisioncalculatorResults")
})

test_that("workflow: handles integer inputs", {
  result <- decisioncalculator(
    TP = as.integer(90),
    TN = as.integer(80),
    FP = as.integer(20),
    FN = as.integer(10)
  )

  expect_s3_class(result, "decisioncalculatorResults")
})

# ═══════════════════════════════════════════════════════════
# Clinical Decision Making Workflows
# ═══════════════════════════════════════════════════════════

test_that("workflow: evaluate test for rule-in vs rule-out", {
  # High specificity for rule-in (confirm diagnosis)
  rule_in <- decisioncalculator(
    TP = 75, TN = 195, FP = 5, FN = 25,
    pp = TRUE,
    pprob = 0.20,
    fagan = TRUE,
    showSummary = TRUE
  )
  expect_s3_class(rule_in, "decisioncalculatorResults")

  # High sensitivity for rule-out (exclude diagnosis)
  rule_out <- decisioncalculator(
    TP = 95, TN = 150, FP = 50, FN = 5,
    pp = TRUE,
    pprob = 0.20,
    fagan = TRUE,
    showSummary = TRUE
  )
  expect_s3_class(rule_out, "decisioncalculatorResults")
})

# ═══════════════════════════════════════════════════════════
# Meta-Analysis Preparation Workflows
# ═══════════════════════════════════════════════════════════

test_that("workflow: evaluate multiple studies for meta-analysis", {
  # Study 1
  study1 <- decisioncalculator(
    TP = 90, TN = 180, FP = 20, FN = 10,
    ci = TRUE
  )
  expect_s3_class(study1, "decisioncalculatorResults")

  # Study 2
  study2 <- decisioncalculator(
    TP = 85, TN = 170, FP = 30, FN = 15,
    ci = TRUE
  )
  expect_s3_class(study2, "decisioncalculatorResults")

  # Study 3
  study3 <- decisioncalculator(
    TP = 95, TN = 190, FP = 10, FN = 5,
    ci = TRUE
  )
  expect_s3_class(study3, "decisioncalculatorResults")
})

# ═══════════════════════════════════════════════════════════
# Quality Improvement Workflows
# ═══════════════════════════════════════════════════════════

test_that("workflow: monitor test performance over time", {
  # Quarter 1
  q1 <- decisioncalculator(
    TP = 88, TN = 175, FP = 25, FN = 12,
    ci = TRUE
  )
  expect_s3_class(q1, "decisioncalculatorResults")

  # Quarter 2 (improved)
  q2 <- decisioncalculator(
    TP = 92, TN = 182, FP = 18, FN = 8,
    ci = TRUE
  )
  expect_s3_class(q2, "decisioncalculatorResults")

  # Quarter 3 (further improved)
  q3 <- decisioncalculator(
    TP = 95, TN = 188, FP = 12, FN = 5,
    ci = TRUE
  )
  expect_s3_class(q3, "decisioncalculatorResults")
})
