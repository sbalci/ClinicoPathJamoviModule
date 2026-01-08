# ═══════════════════════════════════════════════════════════
# Arguments Tests: decisioncalculator
# ═══════════════════════════════════════════════════════════
#
# Tests all parameter combinations and option handling for the
# decisioncalculator (Medical Decision Calculator) jamovi function

library(testthat)
library(ClinicoPath)

# Load test data
data(decisioncalculator_scenarios, package = "ClinicoPath")
data(decisioncalculator_screening, package = "ClinicoPath")
data(decisioncalculator_prevalence, package = "ClinicoPath")
data(decisioncalculator_multiplecuts, package = "ClinicoPath")

# ═══════════════════════════════════════════════════════════
# Confidence Interval Options
# ═══════════════════════════════════════════════════════════

test_that("ci option works", {
  result <- decisioncalculator(
    TP = 90,
    TN = 80,
    FP = 20,
    FN = 10,
    ci = TRUE
  )

  expect_s3_class(result, "decisioncalculator Class")
})

test_that("ci option disabled by default", {
  result <- decisioncalculator(
    TP = 90,
    TN = 80,
    FP = 20,
    FN = 10,
    ci = FALSE
  )

  expect_s3_class(result, "decisioncalculatorClass")
})

# ═══════════════════════════════════════════════════════════
# Population Prevalence Options
# ═══════════════════════════════════════════════════════════

test_that("pp option enables population prevalence", {
  result <- decisioncalculator(
    TP = 90,
    TN = 80,
    FP = 20,
    FN = 10,
    pp = TRUE,
    pprob = 0.15
  )

  expect_s3_class(result, "decisioncalculatorClass")
})

test_that("pprob option accepts valid prevalence values", {
  # Low prevalence
  result1 <- decisioncalculator(
    TP = 90, TN = 80, FP = 20, FN = 10,
    pp = TRUE, pprob = 0.001
  )
  expect_s3_class(result1, "decisioncalculatorClass")

  # Moderate prevalence
  result2 <- decisioncalculator(
    TP = 90, TN = 80, FP = 20, FN = 10,
    pp = TRUE, pprob = 0.300
  )
  expect_s3_class(result2, "decisioncalculatorClass")

  # High prevalence
  result3 <- decisioncalculator(
    TP = 90, TN = 80, FP = 20, FN = 10,
    pp = TRUE, pprob = 0.999
  )
  expect_s3_class(result3, "decisioncalculatorClass")
})

test_that("pprob option rejects invalid values", {
  # Too low (≤ 0)
  expect_error(
    decisioncalculator(
      TP = 90, TN = 80, FP = 20, FN = 10,
      pp = TRUE, pprob = 0
    )
  )

  # Too high (≥ 1)
  expect_error(
    decisioncalculator(
      TP = 90, TN = 80, FP = 20, FN = 10,
      pp = TRUE, pprob = 1.0
    )
  )

  # Negative
  expect_error(
    decisioncalculator(
      TP = 90, TN = 80, FP = 20, FN = 10,
      pp = TRUE, pprob = -0.1
    )
  )
})

# ═══════════════════════════════════════════════════════════
# Visualization Options
# ═══════════════════════════════════════════════════════════

test_that("fagan option generates Fagan nomogram", {
  result <- decisioncalculator(
    TP = 90,
    TN = 80,
    FP = 20,
    FN = 10,
    fagan = TRUE
  )

  expect_s3_class(result, "decisioncalculatorClass")
})

test_that("fagan option requires population prevalence", {
  result <- decisioncalculator(
    TP = 90,
    TN = 80,
    FP = 20,
    FN = 10,
    pp = TRUE,
    pprob = 0.15,
    fagan = TRUE
  )

  expect_s3_class(result, "decisioncalculatorClass")
})

# ═══════════════════════════════════════════════════════════
# Documentation Options
# ═══════════════════════════════════════════════════════════

test_that("fnote option shows explanatory footnotes", {
  result <- decisioncalculator(
    TP = 90,
    TN = 80,
    FP = 20,
    FN = 10,
    fnote = TRUE
  )

  expect_s3_class(result, "decisioncalculatorClass")
})

test_that("showWelcome option controls welcome message", {
  result1 <- decisioncalculator(
    TP = 90, TN = 80, FP = 20, FN = 10,
    showWelcome = TRUE
  )
  expect_s3_class(result1, "decisioncalculatorClass")

  result2 <- decisioncalculator(
    TP = 90, TN = 80, FP = 20, FN = 10,
    showWelcome = FALSE
  )
  expect_s3_class(result2, "decisioncalculatorClass")
})

test_that("showSummary option shows plain-language summary", {
  result <- decisioncalculator(
    TP = 90,
    TN = 80,
    FP = 20,
    FN = 10,
    showSummary = TRUE
  )

  expect_s3_class(result, "decisioncalculatorClass")
})

test_that("showAbout option shows methodology information", {
  result <- decisioncalculator(
    TP = 90,
    TN = 80,
    FP = 20,
    FN = 10,
    showAbout = TRUE
  )

  expect_s3_class(result, "decisioncalculatorClass")
})

test_that("showGlossary option shows clinical terms glossary", {
  result <- decisioncalculator(
    TP = 90,
    TN = 80,
    FP = 20,
    FN = 10,
    showGlossary = TRUE
  )

  expect_s3_class(result, "decisioncalculatorClass")
})

# ═══════════════════════════════════════════════════════════
# Multiple Cut-off Evaluation
# ═══════════════════════════════════════════════════════════

test_that("multiplecuts option enables cut-off comparison", {
  result <- decisioncalculator(
    TP = 90, TN = 80, FP = 20, FN = 10,
    multiplecuts = TRUE,
    cutoff1 = "Conservative",
    tp1 = 85, tn1 = 190, fp1 = 10, fn1 = 15,
    cutoff2 = "Aggressive",
    tp2 = 95, tn2 = 175, fp2 = 25, fn2 = 5
  )

  expect_s3_class(result, "decisioncalculatorClass")
})

test_that("cutoff1 option accepts custom scenario name", {
  result <- decisioncalculator(
    TP = 90, TN = 80, FP = 20, FN = 10,
    multiplecuts = TRUE,
    cutoff1 = "High Sensitivity",
    tp1 = 95, tn1 = 170, fp1 = 30, fn1 = 5
  )

  expect_s3_class(result, "decisioncalculatorClass")
})

test_that("cutoff2 option accepts custom scenario name", {
  result <- decisioncalculator(
    TP = 90, TN = 80, FP = 20, FN = 10,
    multiplecuts = TRUE,
    cutoff2 = "High Specificity",
    tp2 = 75, tn2 = 195, fp2 = 5, fn2 = 25
  )

  expect_s3_class(result, "decisioncalculatorClass")
})

test_that("multiple cut-offs with all counts specified", {
  result <- decisioncalculator(
    TP = 90, TN = 80, FP = 20, FN = 10,
    multiplecuts = TRUE,
    cutoff1 = "Cut-off A",
    tp1 = 85, tn1 = 190, fp1 = 10, fn1 = 15,
    cutoff2 = "Cut-off B",
    tp2 = 95, tn2 = 175, fp2 = 25, fn2 = 5
  )

  expect_s3_class(result, "decisioncalculatorClass")
})

# ═══════════════════════════════════════════════════════════
# Combined Options
# ═══════════════════════════════════════════════════════════

test_that("comprehensive analysis with all options", {
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
    showAbout = TRUE,
    showGlossary = TRUE
  )

  expect_s3_class(result, "decisioncalculatorClass")
})

test_that("multiple cut-offs with full analysis", {
  result <- decisioncalculator(
    TP = 90, TN = 80, FP = 20, FN = 10,
    ci = TRUE,
    pp = TRUE,
    pprob = 0.15,
    fagan = TRUE,
    multiplecuts = TRUE,
    cutoff1 = "Conservative",
    tp1 = 85, tn1 = 190, fp1 = 10, fn1 = 15,
    cutoff2 = "Aggressive",
    tp2 = 95, tn2 = 175, fp2 = 25, fn2 = 5
  )

  expect_s3_class(result, "decisioncalculatorClass")
})

test_that("minimal options (defaults only)", {
  result <- decisioncalculator(
    TP = 90,
    TN = 80,
    FP = 20,
    FN = 10
  )

  expect_s3_class(result, "decisioncalculatorClass")
})

# ═══════════════════════════════════════════════════════════
# Different Test Performance Levels
# ═══════════════════════════════════════════════════════════

test_that("high sensitivity scenario", {
  result <- decisioncalculator(
    TP = 95,   # High sensitivity (95%)
    TN = 150,
    FP = 50,   # Lower specificity (75%)
    FN = 5
  )

  expect_s3_class(result, "decisioncalculatorClass")
})

test_that("high specificity scenario", {
  result <- decisioncalculator(
    TP = 70,   # Lower sensitivity (70%)
    TN = 195,  # High specificity (97.5%)
    FP = 5,
    FN = 30
  )

  expect_s3_class(result, "decisioncalculatorClass")
})

test_that("balanced moderate performance", {
  result <- decisioncalculator(
    TP = 70,   # 70% sensitivity
    TN = 160,  # 80% specificity
    FP = 40,
    FN = 30
  )

  expect_s3_class(result, "decisioncalculatorClass")
})

test_that("excellent test performance", {
  result <- decisioncalculator(
    TP = 98,   # 98% sensitivity
    TN = 192,  # 96% specificity
    FP = 8,
    FN = 2
  )

  expect_s3_class(result, "decisioncalculatorClass")
})

# ═══════════════════════════════════════════════════════════
# Different Prevalence Scenarios
# ═══════════════════════════════════════════════════════════

test_that("rare disease prevalence", {
  result <- decisioncalculator(
    TP = 9,      # Very few cases
    TN = 9890,   # Many healthy
    FP = 100,
    FN = 1,
    pp = TRUE,
    pprob = 0.001  # 0.1% prevalence
  )

  expect_s3_class(result, "decisioncalculatorClass")
})

test_that("common disease prevalence", {
  result <- decisioncalculator(
    TP = 85,
    TN = 210,
    FP = 90,
    FN = 15,
    pp = TRUE,
    pprob = 0.30  # 30% prevalence
  )

  expect_s3_class(result, "decisioncalculatorClass")
})

test_that("high prevalence setting (ICU)", {
  result <- decisioncalculator(
    TP = 180,
    TN = 120,
    FP = 80,
    FN = 20,
    pp = TRUE,
    pprob = 0.50  # 50% prevalence
  )

  expect_s3_class(result, "decisioncalculatorClass")
})

# ═══════════════════════════════════════════════════════════
# Large vs Small Sample Sizes
# ═══════════════════════════════════════════════════════════

test_that("large sample size", {
  result <- decisioncalculator(
    TP = 900,
    TN = 8000,
    FP = 2000,
    FN = 100,
    ci = TRUE
  )

  expect_s3_class(result, "decisioncalculatorClass")
})

test_that("very small sample size", {
  result <- decisioncalculator(
    TP = 9,
    TN = 8,
    FP = 2,
    FN = 1,
    ci = TRUE
  )

  expect_s3_class(result, "decisioncalculatorClass")
})

# ═══════════════════════════════════════════════════════════
# Option Interactions
# ═══════════════════════════════════════════════════════════

test_that("Fagan plot requires population prevalence", {
  # Should work when both are enabled
  result1 <- decisioncalculator(
    TP = 90, TN = 80, FP = 20, FN = 10,
    pp = TRUE,
    pprob = 0.15,
    fagan = TRUE
  )
  expect_s3_class(result1, "decisioncalculatorClass")

  # Fagan without pp should handle gracefully
  result2 <- decisioncalculator(
    TP = 90, TN = 80, FP = 20, FN = 10,
    fagan = TRUE
  )
  expect_s3_class(result2, "decisioncalculatorClass")
})

test_that("all documentation options together", {
  result <- decisioncalculator(
    TP = 90, TN = 80, FP = 20, FN = 10,
    fnote = TRUE,
    showWelcome = TRUE,
    showSummary = TRUE,
    showAbout = TRUE,
    showGlossary = TRUE
  )

  expect_s3_class(result, "decisioncalculatorClass")
})

test_that("minimum documentation (all disabled)", {
  result <- decisioncalculator(
    TP = 90, TN = 80, FP = 20, FN = 10,
    fnote = FALSE,
    showWelcome = FALSE,
    showSummary = FALSE,
    showAbout = FALSE,
    showGlossary = FALSE
  )

  expect_s3_class(result, "decisioncalculatorClass")
})
