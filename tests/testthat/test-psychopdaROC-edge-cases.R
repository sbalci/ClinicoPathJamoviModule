# ═══════════════════════════════════════════════════════════
# Edge Cases Tests: psychopdaROC
# ═══════════════════════════════════════════════════════════
library(testthat)
library(ClinicoPath)
data(psychopdaROC_small, package = "ClinicoPath")
data(psychopdaROC_perfect, package = "ClinicoPath")
data(psychopdaROC_poor, package = "ClinicoPath")
data(psychopdaROC_overlap, package = "ClinicoPath")
data(psychopdaROC_rare, package = "ClinicoPath")
data(psychopdaROC_imbalanced, package = "ClinicoPath")
data(psychopdaROC_missing, package = "ClinicoPath")
data(psychopdaROC_constant, package = "ClinicoPath")
data(psychopdaROC_large, package = "ClinicoPath")

test_that("psychopdaROC handles small datasets", {
  result <- psychopdaROC(
    data = psychopdaROC_small,
    dependentVars = "marker",
    classVar = "class"
  )
  expect_s3_class(result, "psychopdaROCClass")
  expect_true(nrow(psychopdaROC_small) == 30)
})

test_that("psychopdaROC handles perfect separation", {
  result <- psychopdaROC(
    data = psychopdaROC_perfect,
    dependentVars = "perfect_test",
    classVar = "condition",
    positiveClass = "Positive"
  )
  expect_s3_class(result, "psychopdaROCClass")
})

test_that("psychopdaROC handles poor discrimination", {
  result <- psychopdaROC(
    data = psychopdaROC_poor,
    dependentVars = "poor_marker",
    classVar = "status"
  )
  expect_s3_class(result, "psychopdaROCClass")
})

test_that("psychopdaROC handles overlapping distributions", {
  result <- psychopdaROC(
    data = psychopdaROC_overlap,
    dependentVars = "test_value",
    classVar = "diagnosis",
    positiveClass = "Diseased"
  )
  expect_s3_class(result, "psychopdaROCClass")
})

test_that("psychopdaROC handles rare disease prevalence", {
  result <- psychopdaROC(
    data = psychopdaROC_rare,
    dependentVars = "biomarker",
    classVar = "rare_disease",
    positiveClass = "Disease"
  )
  expect_s3_class(result, "psychopdaROCClass")
  # Check prevalence is low
  prevalence <- mean(psychopdaROC_rare$rare_disease == "Disease")
  expect_lt(prevalence, 0.1)
})

test_that("psychopdaROC handles severely imbalanced classes", {
  result <- psychopdaROC(
    data = psychopdaROC_imbalanced,
    dependentVars = "predictor",
    classVar = "rare_outcome",
    positiveClass = "Event"
  )
  expect_s3_class(result, "psychopdaROCClass")
})

test_that("psychopdaROC handles missing data in predictors", {
  result <- psychopdaROC(
    data = psychopdaROC_missing,
    dependentVars = c("test_a", "test_b"),
    classVar = "diagnosis"
  )
  expect_s3_class(result, "psychopdaROCClass")
  # Should have some NA values
  expect_true(any(is.na(psychopdaROC_missing$test_a)))
})

test_that("psychopdaROC handles missing data in class variable", {
  result <- psychopdaROC(
    data = psychopdaROC_missing,
    dependentVars = "test_a",
    classVar = "diagnosis"
  )
  expect_s3_class(result, "psychopdaROCClass")
  # Should have some NA in diagnosis
  expect_true(any(is.na(psychopdaROC_missing$diagnosis)))
})

test_that("psychopdaROC handles constant predictor", {
  result <- psychopdaROC(
    data = psychopdaROC_constant,
    dependentVars = "constant_marker",
    classVar = "outcome"
  )
  expect_s3_class(result, "psychopdaROCClass")
  # All values should be the same
  expect_true(length(unique(psychopdaROC_constant$constant_marker)) == 1)
})

test_that("psychopdaROC handles large datasets efficiently", {
  result <- psychopdaROC(
    data = psychopdaROC_large,
    dependentVars = c("biomarker1", "biomarker2"),
    classVar = "disease_status",
    positiveClass = "Disease"
  )
  expect_s3_class(result, "psychopdaROCClass")
  expect_true(nrow(psychopdaROC_large) == 500)
})

test_that("psychopdaROC handles single predictor with small sample", {
  test_data <- psychopdaROC_small[1:15, ]
  result <- psychopdaROC(
    data = test_data,
    dependentVars = "marker",
    classVar = "class"
  )
  expect_s3_class(result, "psychopdaROCClass")
})

test_that("psychopdaROC handles all positive cases", {
  test_data <- psychopdaROC_small
  test_data$class <- "Positive"

  # This should handle gracefully or error informatively
  expect_condition(
    psychopdaROC(
      data = test_data,
      dependentVars = "marker",
      classVar = "class"
    )
  )
})

test_that("psychopdaROC handles extreme values in predictor", {
  test_data <- psychopdaROC_test
  test_data$biomarker[1:5] <- c(1000, -1000, 500, -500, 0)

  result <- psychopdaROC(
    data = test_data,
    dependentVars = "biomarker",
    classVar = "disease_status"
  )
  expect_s3_class(result, "psychopdaROCClass")
})
