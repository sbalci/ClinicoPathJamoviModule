# ═══════════════════════════════════════════════════════════
# Edge Cases Tests: nogoldstandard
# ═══════════════════════════════════════════════════════════
library(testthat)
library(ClinicoPath)
data(nogoldstandard_small, package = "ClinicoPath")
data(nogoldstandard_large, package = "ClinicoPath")
data(nogoldstandard_highagreement, package = "ClinicoPath")
data(nogoldstandard_lowagreement, package = "ClinicoPath")
data(nogoldstandard_perfect, package = "ClinicoPath")
data(nogoldstandard_rare, package = "ClinicoPath")
data(nogoldstandard_common, package = "ClinicoPath")
data(nogoldstandard_allpositive, package = "ClinicoPath")
data(nogoldstandard_allnegative, package = "ClinicoPath")
data(nogoldstandard_imbalanced, package = "ClinicoPath")
data(nogoldstandard_missing, package = "ClinicoPath")

test_that("nogoldstandard handles small datasets", {
  result <- nogoldstandard(
    data = nogoldstandard_small,
    test1 = "Test1",
    test1Positive = "Positive",
    test2 = "Test2",
    test2Positive = "Positive"
  )
  expect_s3_class(result, "nogoldstandardClass")
  expect_true(nrow(nogoldstandard_small) == 30)
})

test_that("nogoldstandard handles large datasets efficiently", {
  result <- nogoldstandard(
    data = nogoldstandard_large,
    test1 = "Test1",
    test1Positive = "Positive",
    test2 = "Test2",
    test2Positive = "Positive",
    test3 = "Test3",
    test3Positive = "Positive"
  )
  expect_s3_class(result, "nogoldstandardClass")
  expect_true(nrow(nogoldstandard_large) == 500)
})

test_that("nogoldstandard handles high agreement between tests", {
  result <- nogoldstandard(
    data = nogoldstandard_highagreement,
    test1 = "Test1",
    test1Positive = "Positive",
    test2 = "Test2",
    test2Positive = "Positive",
    method = "latent_class"
  )
  expect_s3_class(result, "nogoldstandardClass")
})

test_that("nogoldstandard handles low agreement between tests", {
  result <- nogoldstandard(
    data = nogoldstandard_lowagreement,
    test1 = "Test1",
    test1Positive = "Positive",
    test2 = "Test2",
    test2Positive = "Positive",
    method = "latent_class"
  )
  expect_s3_class(result, "nogoldstandardClass")
})

test_that("nogoldstandard handles perfect agreement", {
  result <- nogoldstandard(
    data = nogoldstandard_perfect,
    test1 = "Test1",
    test1Positive = "Positive",
    test2 = "Test2",
    test2Positive = "Positive"
  )
  # Should complete, though may have warnings about perfect agreement
  expect_s3_class(result, "nogoldstandardClass")
})

test_that("nogoldstandard handles rare disease prevalence", {
  result <- nogoldstandard(
    data = nogoldstandard_rare,
    test1 = "Test1",
    test1Positive = "Positive",
    test2 = "Test2",
    test2Positive = "Positive",
    test3 = "Test3",
    test3Positive = "Positive",
    method = "latent_class"
  )
  expect_s3_class(result, "nogoldstandardClass")
})

test_that("nogoldstandard handles common disease prevalence", {
  result <- nogoldstandard(
    data = nogoldstandard_common,
    test1 = "Test1",
    test1Positive = "Positive",
    test2 = "Test2",
    test2Positive = "Positive",
    method = "latent_class"
  )
  expect_s3_class(result, "nogoldstandardClass")
})

test_that("nogoldstandard handles all positive results", {
  # Should handle gracefully or error informatively
  expect_condition(
    nogoldstandard(
      data = nogoldstandard_allpositive,
      test1 = "Test1",
      test1Positive = "Positive",
      test2 = "Test2",
      test2Positive = "Positive"
    )
  )
})

test_that("nogoldstandard handles all negative results", {
  # Should handle gracefully or error informatively
  expect_condition(
    nogoldstandard(
      data = nogoldstandard_allnegative,
      test1 = "Test1",
      test1Positive = "Positive",
      test2 = "Test2",
      test2Positive = "Positive"
    )
  )
})

test_that("nogoldstandard handles imbalanced test characteristics", {
  result <- nogoldstandard(
    data = nogoldstandard_imbalanced,
    test1 = "Sensitive_Test",
    test1Positive = "Positive",
    test2 = "Specific_Test",
    test2Positive = "Positive",
    method = "latent_class"
  )
  expect_s3_class(result, "nogoldstandardClass")
})

test_that("nogoldstandard handles missing data", {
  result <- nogoldstandard(
    data = nogoldstandard_missing,
    test1 = "Test1",
    test1Positive = "Positive",
    test2 = "Test2",
    test2Positive = "Positive",
    test3 = "Test3",
    test3Positive = "Positive"
  )
  # Should handle with warnings or listwise deletion
  expect_s3_class(result, "nogoldstandardClass")
  expect_true(any(is.na(nogoldstandard_missing$Test1)))
})

test_that("nogoldstandard handles bootstrap with small nboot", {
  result <- nogoldstandard(
    data = nogoldstandard_small,
    test1 = "Test1",
    test1Positive = "Positive",
    test2 = "Test2",
    test2Positive = "Positive",
    bootstrap = TRUE,
    nboot = 100
  )
  expect_s3_class(result, "nogoldstandardClass")
})

test_that("nogoldstandard handles different positive level specifications", {
  # First level as positive
  result <- nogoldstandard(
    data = nogoldstandard_allnegative,
    test1 = "Test1",
    test1Positive = "Negative",
    test2 = "Test2",
    test2Positive = "Negative"
  )
  expect_condition(result)
})

test_that("nogoldstandard validates required test arguments", {
  # Missing test2
  expect_error(
    nogoldstandard(
      data = nogoldstandard_small,
      test1 = "Test1",
      test1Positive = "Positive"
    ),
    regexp = "test2|required|missing",
    ignore.case = TRUE
  )

  # Missing test1Positive
  expect_error(
    nogoldstandard(
      data = nogoldstandard_small,
      test1 = "Test1",
      test2 = "Test2",
      test2Positive = "Positive"
    ),
    regexp = "positive|level|required",
    ignore.case = TRUE
  )
})
