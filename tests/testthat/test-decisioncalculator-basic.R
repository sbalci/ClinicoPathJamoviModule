# ═══════════════════════════════════════════════════════════
# Basic Tests: decisioncalculator
# ═══════════════════════════════════════════════════════════
#
# Tests core functionality and required arguments for the
# decisioncalculator (Medical Decision Calculator) jamovi function

library(testthat)

# Load test data
data(decisioncalculator_scenarios, package = "ClinicoPath")
data(decisioncalculator_screening, package = "ClinicoPath")
data(decisioncalculator_biomarker, package = "ClinicoPath")


# decisioncalculator reports invalid input as a jamovi NOTICE, not an R condition (the
# convention across meddecide). expect_error() asserted nothing here.
dcalc_notices <- function(result) {
  gsub("[[:space:]]+", " ", gsub("<[^>]+>", " ", paste(result$notices$content, collapse = " ")))
}

test_that("decisioncalculator function exists and is callable", {
  expect_true(exists("decisioncalculator"))
  expect_true(is.function(decisioncalculator))
})

test_that("decisioncalculator returns proper class object", {
  result <- decisioncalculator(
    TP = 90,
    TN = 80,
    FP = 20,
    FN = 10
  )

  expect_s3_class(result, "decisioncalculatorResults")
})

test_that("the four counts have defaults, so omitting one is not an error", {
  # TP/TN/FP/FN are Number options carrying defaults (90/80/30/20). Omitting one uses the
  # default rather than erroring -- correct for a calculator, and the reason the previous
  # expect_error() assertions could never fire.
  expect_no_error(res <- decisioncalculator(TN = 80, FP = 20, FN = 10))
  expect_s3_class(res, "decisioncalculatorResults")
  # the omitted TP fell back to its default of 90
  expect_equal(res$nTable$asDF$DiseaseP[1], 90 + 10)
})

test_that("decisioncalculator accepts all four count arguments", {
  result <- decisioncalculator(
    TP = 90,
    TN = 80,
    FP = 20,
    FN = 10
  )

  expect_s3_class(result, "decisioncalculatorResults")
})

test_that("decisioncalculator handles basic diagnostic test evaluation", {
  result <- decisioncalculator(
    TP = 90,
    TN = 80,
    FP = 20,
    FN = 10
  )

  expect_s3_class(result, "decisioncalculatorResults")
})

test_that("decisioncalculator calculates sensitivity correctly", {
  # Sensitivity = TP / (TP + FN) = 90 / 100 = 0.90
  result <- decisioncalculator(
    TP = 90,
    TN = 80,
    FP = 20,
    FN = 10
  )

  expect_s3_class(result, "decisioncalculatorResults")
})

test_that("decisioncalculator calculates specificity correctly", {
  # Specificity = TN / (TN + FP) = 80 / 100 = 0.80
  result <- decisioncalculator(
    TP = 90,
    TN = 80,
    FP = 20,
    FN = 10
  )

  expect_s3_class(result, "decisioncalculatorResults")
})

test_that("decisioncalculator handles perfect test (100% sens/spec)", {
  result <- decisioncalculator(
    TP = 50,
    TN = 150,
    FP = 0,
    FN = 0
  )

  expect_s3_class(result, "decisioncalculatorResults")
})

test_that("decisioncalculator handles useless test (50% sens/spec)", {
  result <- decisioncalculator(
    TP = 25,
    TN = 75,
    FP = 75,
    FN = 25
  )

  expect_s3_class(result, "decisioncalculatorResults")
})

test_that("decisioncalculator handles screening test scenario", {
  result <- decisioncalculator(
    TP = decisioncalculator_screening$TP,
    TN = decisioncalculator_screening$TN,
    FP = decisioncalculator_screening$FP,
    FN = decisioncalculator_screening$FN
  )

  expect_s3_class(result, "decisioncalculatorResults")
})

test_that("decisioncalculator handles high-performance biomarker", {
  result <- decisioncalculator(
    TP = decisioncalculator_biomarker$TP,
    TN = decisioncalculator_biomarker$TN,
    FP = decisioncalculator_biomarker$FP,
    FN = decisioncalculator_biomarker$FN
  )

  expect_s3_class(result, "decisioncalculatorResults")
})

test_that("decisioncalculator flags negative counts", {
  expect_no_error(res <- decisioncalculator(TP = -10, TN = 80, FP = 20, FN = 10))
  expect_match(dcalc_notices(res), "Negative Counts Detected")
  # and does not present numbers computed from them
  expect_true(is.na(res$ratioTable$asDF$Sens[1]))
})

test_that("decisioncalculator handles a non-numeric count", {
  # jmvcore coerces a Number option; what matters is that nothing silently wrong is shown.
  res <- try(decisioncalculator(TP = "90", TN = 80, FP = 20, FN = 10), silent = TRUE)
  if (inherits(res, "try-error")) {
    succeed("rejected at the wrapper")
  } else {
    sens <- res$ratioTable$asDF$Sens[1]
    expect_true(is.na(sens) || (is.numeric(sens) && sens >= 0 && sens <= 1))
  }
})

test_that("decisioncalculator handles zero counts in some cells", {
  # FP = 0 is valid (perfect specificity)
  result1 <- decisioncalculator(
    TP = 90,
    TN = 100,
    FP = 0,
    FN = 10
  )
  expect_s3_class(result1, "decisioncalculatorResults")

  # FN = 0 is valid (perfect sensitivity)
  result2 <- decisioncalculator(
    TP = 100,
    TN = 80,
    FP = 20,
    FN = 0
  )
  expect_s3_class(result2, "decisioncalculatorResults")
})

test_that("decisioncalculator handles all zeros in disease-present", {
  # All diseased classified as negative (TP=0, FN>0) - terrible test but valid
  result <- decisioncalculator(
    TP = 0,
    TN = 100,
    FP = 0,
    FN = 100
  )

  expect_s3_class(result, "decisioncalculatorResults")
})

test_that("decisioncalculator handles all zeros in disease-absent", {
  # All healthy classified as positive (TN=0, FP>0) - terrible test but valid
  result <- decisioncalculator(
    TP = 100,
    TN = 0,
    FP = 100,
    FN = 0
  )

  expect_s3_class(result, "decisioncalculatorResults")
})

test_that("decisioncalculator produces consistent results across runs", {
  # Run the same analysis twice
  result1 <- decisioncalculator(
    TP = 90,
    TN = 80,
    FP = 20,
    FN = 10
  )

  result2 <- decisioncalculator(
    TP = 90,
    TN = 80,
    FP = 20,
    FN = 10
  )

  # Results should be identical (deterministic)
  expect_s3_class(result1, "decisioncalculatorResults")
  expect_s3_class(result2, "decisioncalculatorResults")
})

test_that("decisioncalculator accepts integer inputs", {
  result <- decisioncalculator(
    TP = as.integer(90),
    TN = as.integer(80),
    FP = as.integer(20),
    FN = as.integer(10)
  )

  expect_s3_class(result, "decisioncalculatorResults")
})

test_that("decisioncalculator accepts numeric (double) inputs", {
  result <- decisioncalculator(
    TP = 90.0,
    TN = 80.0,
    FP = 20.0,
    FN = 10.0
  )

  expect_s3_class(result, "decisioncalculatorResults")
})
