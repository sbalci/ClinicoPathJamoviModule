# ═══════════════════════════════════════════════════════════
# Edge Cases Tests: nogoldstandard
# ═══════════════════════════════════════════════════════════
library(testthat)
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
    test2Positive = "Positive",
    test3Positive = NULL,
    test4Positive = NULL,
    test5Positive = NULL,
      # 2 tests: latent_class (the default since 1.0.4) requires 3+
      method = "composite"
    )
  expect_s3_class(result, "nogoldstandardResults")
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
    test3Positive = "Positive",
    test4Positive = NULL,
    test5Positive = NULL
  )
  expect_s3_class(result, "nogoldstandardResults")
  expect_true(nrow(nogoldstandard_large) == 500)
})

test_that("nogoldstandard handles high agreement between tests", {
  # A two-class latent model over two binary tests has five parameters and three
  # degrees of freedom, so it is not identified. The analysis refuses rather than
  # returning numbers that are determined by the starting values.
  expect_error(
    nogoldstandard(
      data = nogoldstandard_highagreement,
      test1 = "Test1", test1Positive = "Positive",
      test2 = "Test2", test2Positive = "Positive",
      method = "latent_class",
      test3Positive = NULL, test4Positive = NULL, test5Positive = NULL
    ),
    "at least 3 tests"
  )
})

test_that("nogoldstandard handles low agreement between tests", {
  # A two-class latent model over two binary tests has five parameters and three
  # degrees of freedom, so it is not identified. The analysis refuses rather than
  # returning numbers that are determined by the starting values.
  expect_error(
    nogoldstandard(
      data = nogoldstandard_lowagreement,
      test1 = "Test1", test1Positive = "Positive",
      test2 = "Test2", test2Positive = "Positive",
      method = "latent_class",
      test3Positive = NULL, test4Positive = NULL, test5Positive = NULL
    ),
    "at least 3 tests"
  )
})

test_that("nogoldstandard handles perfect agreement", {
  result <- nogoldstandard(
    data = nogoldstandard_perfect,
    test1 = "Test1",
    test1Positive = "Positive",
    test2 = "Test2",
    test2Positive = "Positive",
    test3Positive = NULL,
    test4Positive = NULL,
    test5Positive = NULL,
      # 2 tests: latent_class (the default since 1.0.4) requires 3+
      method = "composite"
    )
  # Should complete, though may have warnings about perfect agreement
  expect_s3_class(result, "nogoldstandardResults")
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
    method = "latent_class",
    test4Positive = NULL,
    test5Positive = NULL
  )
  expect_s3_class(result, "nogoldstandardResults")
})

test_that("nogoldstandard handles common disease prevalence", {
  # A two-class latent model over two binary tests has five parameters and three
  # degrees of freedom, so it is not identified. The analysis refuses rather than
  # returning numbers that are determined by the starting values.
  expect_error(
    nogoldstandard(
      data = nogoldstandard_common,
      test1 = "Test1", test1Positive = "Positive",
      test2 = "Test2", test2Positive = "Positive",
      method = "latent_class",
      test3Positive = NULL, test4Positive = NULL, test5Positive = NULL
    ),
    "at least 3 tests"
  )
})

test_that("nogoldstandard handles all positive results", {
  # A column with no variation used to be expected to throw. It does not, and should
  # not: the analysis completes on the default reference-rule method. What matters is
  # that it does not silently present a degenerate column as an accuracy estimate.
  expect_no_error(
    res <- nogoldstandard(
      data = nogoldstandard_allpositive,
      test1 = "Test1", test1Positive = "Positive",
      test2 = "Test2", test2Positive = "Positive",
      test3Positive = NULL, test4Positive = NULL, test5Positive = NULL,
      # 2 tests: latent_class (the default since 1.0.4) requires 3+
      method = "composite"
    )
  )
  expect_s3_class(res, "nogoldstandardResults")
  # composite over 2 tests is any_positive (a 1-of-2 tie passes >= 0.5), so specificity
  # and PPV are fixed at 1 by construction and are blanked rather than reported
  expect_true(all(is.na(res$test_metrics$asDF$specificity)))
  expect_true(all(is.na(res$test_metrics$asDF$ppv)))
})

test_that("nogoldstandard handles all negative results", {
  # A column with no variation used to be expected to throw. It does not, and should
  # not: the analysis completes on the default reference-rule method. What matters is
  # that it does not silently present a degenerate column as an accuracy estimate.
  expect_no_error(
    res <- nogoldstandard(
      data = nogoldstandard_allnegative,
      test1 = "Test1", test1Positive = "Positive",
      test2 = "Test2", test2Positive = "Positive",
      test3Positive = NULL, test4Positive = NULL, test5Positive = NULL,
      # 2 tests: latent_class (the default since 1.0.4) requires 3+
      method = "composite"
    )
  )
  expect_s3_class(res, "nogoldstandardResults")
  # default method is all_positive, whose sensitivity is fixed at 1 by construction
  # and is therefore blanked rather than reported
  expect_true(all(is.na(res$test_metrics$asDF$sensitivity)))
})

test_that("nogoldstandard handles imbalanced test characteristics", {
  # A two-class latent model over two binary tests has five parameters and three
  # degrees of freedom, so it is not identified. The analysis refuses rather than
  # returning numbers that are determined by the starting values.
  expect_error(
    nogoldstandard(
      data = nogoldstandard_imbalanced,
      test1 = "Sensitive_Test", test1Positive = "Positive",
      test2 = "Specific_Test", test2Positive = "Positive",
      method = "latent_class",
      test3Positive = NULL, test4Positive = NULL, test5Positive = NULL
    ),
    "at least 3 tests"
  )
})

test_that("nogoldstandard handles missing data", {
  result <- nogoldstandard(
    data = nogoldstandard_missing,
    test1 = "Test1",
    test1Positive = "Positive",
    test2 = "Test2",
    test2Positive = "Positive",
    test3 = "Test3",
    test3Positive = "Positive",
    test4Positive = NULL,
    test5Positive = NULL
  )
  # Should handle with warnings or listwise deletion
  expect_s3_class(result, "nogoldstandardResults")
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
    nboot = 100,
    test3Positive = NULL,
    test4Positive = NULL,
    test5Positive = NULL,
      # 2 tests: latent_class (the default since 1.0.4) requires 3+
      method = "composite"
    )
  expect_s3_class(result, "nogoldstandardResults")
})

test_that("nogoldstandard handles different positive level specifications", {
  # Naming the OTHER level as positive is legitimate -- it inverts each test. The old
  # expect_condition(result) asserted on an already-evaluated value, which can never
  # signal a condition, so it tested nothing.
  expect_no_error(
    result <- nogoldstandard(
      data = nogoldstandard_allnegative,
      test1 = "Test1", test1Positive = "Negative",
      test2 = "Test2", test2Positive = "Negative",
      test3Positive = NULL, test4Positive = NULL, test5Positive = NULL,
      # 2 tests: latent_class (the default since 1.0.4) requires 3+
      method = "composite"
    )
  )
  expect_s3_class(result, "nogoldstandardResults")
  expect_gt(result$test_metrics$rowCount, 0L)
})

test_that("nogoldstandard waits rather than erroring when a second test is missing", {
  # A jamovi analysis does not error on an incomplete variable selection: it returns and
  # leaves the results empty until the user finishes choosing.
  expect_no_error(
    res <- nogoldstandard(
      data = nogoldstandard_small,
      test1 = "Test1", test1Positive = "Positive",
      test2Positive = NULL, test3Positive = NULL,
      test4Positive = NULL, test5Positive = NULL,
      # 2 tests: latent_class (the default since 1.0.4) requires 3+
      method = "composite"
    )
  )
  expect_equal(res$test_metrics$rowCount, 0L)
})
