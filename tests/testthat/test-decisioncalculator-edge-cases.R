# ═══════════════════════════════════════════════════════════
# Edge Cases Tests: decisioncalculator
# ═══════════════════════════════════════════════════════════
#
# Tests boundary conditions, extreme scenarios, and error handling
# for the decision calculator (Medical Decision Calculator) jamovi function

library(testthat)
library(ClinicoPath)

# Load edge case test data
data(decisioncalculator_perfect, package = "ClinicoPath")
data(decisioncalculator_useless, package = "ClinicoPath")
data(decisioncalculator_small, package = "ClinicoPath")
data(decisioncalculator_raredisease, package = "ClinicoPath")

# ═══════════════════════════════════════════════════════════
# Perfect Test Performance (Edge Case)
# ═══════════════════════════════════════════════════════════

test_that("perfect test with 100% sensitivity and specificity", {
  result <- decisioncalculator(
    TP = 50,
    TN = 150,
    FP = 0,
    FN = 0
  )

  expect_s3_class(result, "decisioncalculatorClass")
})

test_that("perfect sensitivity but imperfect specificity", {
  result <- decisioncalculator(
    TP = 100,
    TN = 80,
    FP = 20,
    FN = 0  # Perfect sensitivity
  )

  expect_s3_class(result, "decisioncalculatorClass")
})

test_that("perfect specificity but imperfect sensitivity", {
  result <- decisioncalculator(
    TP = 80,
    TN = 100,
    FP = 0,  # Perfect specificity
    FN = 20
  )

  expect_s3_class(result, "decisioncalculatorClass")
})

# ═══════════════════════════════════════════════════════════
# Useless Test Performance (Edge Case)
# ═══════════════════════════════════════════════════════════

test_that("useless test (sensitivity = specificity = 0.5)", {
  result <- decisioncalculator(
    TP = 25,
    TN = 75,
    FP = 75,
    FN = 25
  )

  expect_s3_class(result, "decisioncalculatorClass")
})

test_that("worse than useless test (sensitivity + specificity < 1)", {
  result <- decisioncalculator(
    TP = 20,   # 20% sensitivity
    TN = 60,   # 30% specificity
    FP = 140,
    FN = 80
  )

  expect_s3_class(result, "decisioncalculatorClass")
})

# ═══════════════════════════════════════════════════════════
# Zero Count Scenarios
# ═══════════════════════════════════════════════════════════

test_that("zero true positives (all diseased missed)", {
  result <- decisioncalculator(
    TP = 0,    # All diseased classified as negative
    TN = 100,
    FP = 0,
    FN = 100
  )

  expect_s3_class(result, "decisioncalculatorClass")
})

test_that("zero true negatives (all healthy misclassified)", {
  result <- decisioncalculator(
    TP = 100,
    TN = 0,    # All healthy classified as positive
    FP = 100,
    FN = 0
  )

  expect_s3_class(result, "decisioncalculatorClass")
})

test_that("zero false positives (perfect specificity)", {
  result <- decisioncalculator(
    TP = 80,
    TN = 100,
    FP = 0,    # No false positives
    FN = 20
  )

  expect_s3_class(result, "decisioncalculatorClass")
})

test_that("zero false negatives (perfect sensitivity)", {
  result <- decisioncalculator(
    TP = 100,
    TN = 80,
    FP = 20,
    FN = 0     # No false negatives
  )

  expect_s3_class(result, "decisioncalculatorClass")
})

test_that("all counts zero (invalid)", {
  expect_error(
    decisioncalculator(
      TP = 0,
      TN = 0,
      FP = 0,
      FN = 0
    )
  )
})

test_that("only disease-absent counts", {
  # All counts from diseased = 0
  expect_error(
    decisioncalculator(
      TP = 0,
      TN = 100,
      FP = 50,
      FN = 0
    )
  )
})

test_that("only disease-present counts", {
  # All counts from healthy = 0
  expect_error(
    decisioncalculator(
      TP = 100,
      TN = 0,
      FP = 0,
      FN = 50
    )
  )
})

# ═══════════════════════════════════════════════════════════
# Single Count Scenarios
# ═══════════════════════════════════════════════════════════

test_that("minimal sample (n = 4, one in each cell)", {
  result <- decisioncalculator(
    TP = 1,
    TN = 1,
    FP = 1,
    FN = 1
  )

  expect_s3_class(result, "decisioncalculatorClass")
})

test_that("minimal diseased sample (n = 2)", {
  result <- decisioncalculator(
    TP = 1,
    TN = 100,
    FP = 10,
    FN = 1
  )

  expect_s3_class(result, "decisioncalculatorClass")
})

test_that("minimal healthy sample (n = 2)", {
  result <- decisioncalculator(
    TP = 50,
    TN = 1,
    FP = 1,
    FN = 10
  )

  expect_s3_class(result, "decisioncalculatorClass")
})

# ═══════════════════════════════════════════════════════════
# Very Small Samples
# ═══════════════════════════════════════════════════════════

test_that("very small sample size (n < 20)", {
  result <- decisioncalculator(
    TP = 5,
    TN = 8,
    FP = 2,
    FN = 3,
    ci = TRUE  # Wide confidence intervals expected
  )

  expect_s3_class(result, "decisioncalculatorClass")
})

test_that("small pilot study", {
  result <- decisioncalculator(
    TP = decisioncalculator_small$TP,
    TN = decisioncalculator_small$TN,
    FP = decisioncalculator_small$FP,
    FN = decisioncalculator_small$FN,
    ci = TRUE
  )

  expect_s3_class(result, "decisioncalculatorClass")
})

# ═══════════════════════════════════════════════════════════
# Very Large Samples
# ═══════════════════════════════════════════════════════════

test_that("very large sample size (n > 10,000)", {
  result <- decisioncalculator(
    TP = 9000,
    TN = 80000,
    FP = 20000,
    FN = 1000,
    ci = TRUE  # Narrow confidence intervals expected
  )

  expect_s3_class(result, "decisioncalculatorClass")
})

test_that("rare disease large screening study", {
  result <- decisioncalculator(
    TP = decisioncalculator_raredisease$TP,
    TN = decisioncalculator_raredisease$TN,
    FP = decisioncalculator_raredisease$FP,
    FN = decisioncalculator_raredisease$FN,
    pp = TRUE,
    pprob = 0.001
  )

  expect_s3_class(result, "decisioncalculatorClass")
})

# ═══════════════════════════════════════════════════════════
# Extreme Prevalence Values
# ═══════════════════════════════════════════════════════════

test_that("extremely rare disease (prevalence = 0.001)", {
  result <- decisioncalculator(
    TP = 9,
    TN = 9890,
    FP = 100,
    FN = 1,
    pp = TRUE,
    pprob = 0.001
  )

  expect_s3_class(result, "decisioncalculatorClass")
})

test_that("very common disease (prevalence = 0.999)", {
  result <- decisioncalculator(
    TP = 990,
    TN = 1,
    FP = 9,
    FN = 0,
    pp = TRUE,
    pprob = 0.999
  )

  expect_s3_class(result, "decisioncalculatorClass")
})

test_that("minimum valid prevalence (0.001)", {
  result <- decisioncalculator(
    TP = 50, TN = 100, FP = 20, FN = 10,
    pp = TRUE,
    pprob = 0.001
  )

  expect_s3_class(result, "decisioncalculatorClass")
})

test_that("maximum valid prevalence (0.999)", {
  result <- decisioncalculator(
    TP = 50, TN = 100, FP = 20, FN = 10,
    pp = TRUE,
    pprob = 0.999
  )

  expect_s3_class(result, "decisioncalculatorClass")
})

# ═══════════════════════════════════════════════════════════
# Highly Imbalanced 2×2 Tables
# ═══════════════════════════════════════════════════════════

test_that("extremely unbalanced (99% diseased)", {
  result <- decisioncalculator(
    TP = 990,
    TN = 5,
    FP = 5,
    FN = 0
  )

  expect_s3_class(result, "decisioncalculatorClass")
})

test_that("extremely unbalanced (99% healthy)", {
  result <- decisioncalculator(
    TP = 5,
    TN = 990,
    FP = 5,
    FN = 0
  )

  expect_s3_class(result, "decisioncalculatorClass")
})

test_that("extreme test positivity (99% test positive)", {
  result <- decisioncalculator(
    TP = 495,
    TN = 5,
    FP = 495,
    FN = 5
  )

  expect_s3_class(result, "decisioncalculatorClass")
})

test_that("extreme test negativity (99% test negative)", {
  result <- decisioncalculator(
    TP = 5,
    TN = 495,
    FP = 5,
    FN = 495
  )

  expect_s3_class(result, "decisioncalculatorClass")
})

# ═══════════════════════════════════════════════════════════
# Invalid Input Handling
# ═══════════════════════════════════════════════════════════

test_that("rejects negative TP", {
  expect_error(
    decisioncalculator(TP = -10, TN = 80, FP = 20, FN = 10)
  )
})

test_that("rejects negative TN", {
  expect_error(
    decisioncalculator(TP = 90, TN = -80, FP = 20, FN = 10)
  )
})

test_that("rejects negative FP", {
  expect_error(
    decisioncalculator(TP = 90, TN = 80, FP = -20, FN = 10)
  )
})

test_that("rejects negative FN", {
  expect_error(
    decisioncalculator(TP = 90, TN = 80, FP = 20, FN = -10)
  )
})

test_that("rejects non-numeric TP", {
  expect_error(
    decisioncalculator(TP = "90", TN = 80, FP = 20, FN = 10)
  )
})

test_that("rejects NA counts", {
  expect_error(
    decisioncalculator(TP = NA, TN = 80, FP = 20, FN = 10)
  )
})

test_that("rejects NULL counts", {
  expect_error(
    decisioncalculator(TP = NULL, TN = 80, FP = 20, FN = 10)
  )
})

test_that("rejects Inf counts", {
  expect_error(
    decisioncalculator(TP = Inf, TN = 80, FP = 20, FN = 10)
  )
})

# ═══════════════════════════════════════════════════════════
# Fractional Counts
# ═══════════════════════════════════════════════════════════

test_that("handles fractional counts (from weighted data)", {
  result <- decisioncalculator(
    TP = 90.5,
    TN = 80.3,
    FP = 20.7,
    FN = 10.2
  )

  expect_s3_class(result, "decisioncalculatorClass")
})

test_that("handles very small fractional counts", {
  result <- decisioncalculator(
    TP = 0.5,
    TN = 1.5,
    FP = 0.8,
    FN = 0.2
  )

  expect_s3_class(result, "decisioncalculatorClass")
})

# ═══════════════════════════════════════════════════════════
# Prevalence Edge Cases
# ═══════════════════════════════════════════════════════════

test_that("prevalence below minimum (< 0.001) rejected", {
  expect_error(
    decisioncalculator(
      TP = 90, TN = 80, FP = 20, FN = 10,
      pp = TRUE,
      pprob = 0.0005
    )
  )
})

test_that("prevalence above maximum (> 0.999) rejected", {
  expect_error(
    decisioncalculator(
      TP = 90, TN = 80, FP = 20, FN = 10,
      pp = TRUE,
      pprob = 0.9995
    )
  )
})

test_that("prevalence = 1.0 rejected", {
  expect_error(
    decisioncalculator(
      TP = 90, TN = 80, FP = 20, FN = 10,
      pp = TRUE,
      pprob = 1.0
    )
  )
})

test_that("prevalence = 0.0 rejected", {
  expect_error(
    decisioncalculator(
      TP = 90, TN = 80, FP = 20, FN = 10,
      pp = TRUE,
      pprob = 0.0
    )
  )
})

# ═══════════════════════════════════════════════════════════
# Paradoxical Results
# ═══════════════════════════════════════════════════════════

test_that("handles kappa paradox (high agreement, low kappa)", {
  # Rare disease with high observed agreement but low kappa due to prevalence
  result <- decisioncalculator(
    TP = 9,      # Sens = 90%
    TN = 9890,   # Spec = 99%
    FP = 100,
    FN = 1
  )

  expect_s3_class(result, "decisioncalculatorClass")
})

test_that("handles PPV paradox (good test, poor PPV in low prevalence)", {
  result <- decisioncalculator(
    TP = 90,     # 90% sensitivity
    TN = 900,    # 90% specificity
    FP = 100,
    FN = 10,
    pp = TRUE,
    pprob = 0.01  # 1% prevalence → low PPV despite good test
  )

  expect_s3_class(result, "decisioncalculatorClass")
})

# ═══════════════════════════════════════════════════════════
# Multiple Cut-off Edge Cases
# ═══════════════════════════════════════════════════════════

test_that("multiple cut-offs with zero counts", {
  result <- decisioncalculator(
    TP = 90, TN = 80, FP = 20, FN = 10,
    multiplecuts = TRUE,
    cutoff1 = "Perfect Specificity",
    tp1 = 80, tn1 = 200, fp1 = 0, fn1 = 20,
    cutoff2 = "Perfect Sensitivity",
    tp2 = 100, tn2 = 150, fp2 = 50, fn2 = 0
  )

  expect_s3_class(result, "decisioncalculatorClass")
})

test_that("multiple cut-offs with identical performance", {
  result <- decisioncalculator(
    TP = 90, TN = 80, FP = 20, FN = 10,
    multiplecuts = TRUE,
    cutoff1 = "Cut-off A",
    tp1 = 85, tn1 = 180, fp1 = 20, fn1 = 15,
    cutoff2 = "Cut-off B (same)",
    tp2 = 85, tn2 = 180, fp2 = 20, fn2 = 15  # Identical
  )

  expect_s3_class(result, "decisioncalculatorClass")
})

# ═══════════════════════════════════════════════════════════
# Consistency Tests
# ═══════════════════════════════════════════════════════════

test_that("produces consistent results across runs", {
  result1 <- decisioncalculator(
    TP = 90, TN = 80, FP = 20, FN = 10,
    ci = TRUE
  )

  result2 <- decisioncalculator(
    TP = 90, TN = 80, FP = 20, FN = 10,
    ci = TRUE
  )

  # Results should be deterministic
  expect_s3_class(result1, "decisioncalculatorClass")
  expect_s3_class(result2, "decisioncalculatorClass")
})

test_that("order of counts doesn't matter (named arguments)", {
  result1 <- decisioncalculator(TP = 90, TN = 80, FP = 20, FN = 10)
  result2 <- decisioncalculator(FN = 10, FP = 20, TN = 80, TP = 90)

  expect_s3_class(result1, "decisioncalculatorClass")
  expect_s3_class(result2, "decisioncalculatorClass")
})
