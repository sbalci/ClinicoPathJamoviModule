# ═══════════════════════════════════════════════════════════
# Argument Tests: decisioncompare
# ═══════════════════════════════════════════════════════════
library(testthat)
data(decisioncompare_test, package = "ClinicoPath")
data(decisioncompare_threetest, package = "ClinicoPath")
data(decisioncompare_raters, package = "ClinicoPath")
data(decisioncompare_indeterminate, package = "ClinicoPath")

test_that("decisioncompare respects prior probability parameter", {
  # With custom prevalence
  result_custom <- decisioncompare(
    data = decisioncompare_test,
    gold = "GoldStandard",
    goldPositive = "Positive",
    test1 = "Test1",
    test1Positive = "Positive",
    test2 = "Test2",
    test2Positive = "Positive",
    pp = TRUE,
    pprob = 0.15,
    test3Positive = NULL
  )
  expect_s3_class(result_custom, "decisioncompareClass")

  # Without custom prevalence (use dataset prevalence)
  result_default <- decisioncompare(
    data = decisioncompare_test,
    gold = "GoldStandard",
    goldPositive = "Positive",
    test1 = "Test1",
    test1Positive = "Positive",
    test2 = "Test2",
    test2Positive = "Positive",
    pp = FALSE,
    test3Positive = NULL
  )
  expect_s3_class(result_default, "decisioncompareClass")
})

test_that("decisioncompare respects different prevalence values", {
  # Screening prevalence (low)
  result_screen <- decisioncompare(
    data = decisioncompare_test,
    gold = "GoldStandard",
    goldPositive = "Positive",
    test1 = "Test1",
    test1Positive = "Positive",
    test2 = "Test2",
    test2Positive = "Positive",
    pp = TRUE,
    pprob = 0.05,
    test3Positive = NULL
  )
  expect_no_error(result_screen)

  # Clinical prevalence (high)
  result_clinical <- decisioncompare(
    data = decisioncompare_test,
    gold = "GoldStandard",
    goldPositive = "Positive",
    test1 = "Test1",
    test1Positive = "Positive",
    test2 = "Test2",
    test2Positive = "Positive",
    pp = TRUE,
    pprob = 0.60,
    test3Positive = NULL
  )
  expect_no_error(result_clinical)
})

test_that("decisioncompare respects ci parameter", {
  # With CI
  result_ci <- decisioncompare(
    data = decisioncompare_test,
    gold = "GoldStandard",
    goldPositive = "Positive",
    test1 = "Test1",
    test1Positive = "Positive",
    test2 = "Test2",
    test2Positive = "Positive",
    ci = TRUE,
    test3Positive = NULL
  )
  expect_no_error(result_ci)

  # Without CI
  result_no_ci <- decisioncompare(
    data = decisioncompare_test,
    gold = "GoldStandard",
    goldPositive = "Positive",
    test1 = "Test1",
    test1Positive = "Positive",
    test2 = "Test2",
    test2Positive = "Positive",
    ci = FALSE,
    test3Positive = NULL
  )
  expect_no_error(result_no_ci)
})

test_that("decisioncompare respects plot parameter", {
  # With plot
  result_plot <- decisioncompare(
    data = decisioncompare_test,
    gold = "GoldStandard",
    goldPositive = "Positive",
    test1 = "Test1",
    test1Positive = "Positive",
    test2 = "Test2",
    test2Positive = "Positive",
    plot = TRUE,
    test3Positive = NULL
  )
  expect_no_error(result_plot)

  # Without plot
  result_no_plot <- decisioncompare(
    data = decisioncompare_test,
    gold = "GoldStandard",
    goldPositive = "Positive",
    test1 = "Test1",
    test1Positive = "Positive",
    test2 = "Test2",
    test2Positive = "Positive",
    plot = FALSE,
    test3Positive = NULL
  )
  expect_no_error(result_no_plot)
})

test_that("decisioncompare respects radarplot parameter", {
  # With radar plot (requires 3 tests)
  result_radar <- decisioncompare(
    data = decisioncompare_threetest,
    gold = "GoldStandard",
    goldPositive = "Positive",
    test1 = "Test1",
    test1Positive = "Positive",
    test2 = "Test2",
    test2Positive = "Positive",
    test3 = "Test3",
    test3Positive = "Positive",
    radarplot = TRUE
  )
  expect_no_error(result_radar)

  # Without radar plot
  result_no_radar <- decisioncompare(
    data = decisioncompare_threetest,
    gold = "GoldStandard",
    goldPositive = "Positive",
    test1 = "Test1",
    test1Positive = "Positive",
    test2 = "Test2",
    test2Positive = "Positive",
    test3 = "Test3",
    test3Positive = "Positive",
    radarplot = FALSE
  )
  expect_no_error(result_no_radar)
})

test_that("decisioncompare respects statComp parameter", {
  # With statistical comparison
  result_stat <- decisioncompare(
    data = decisioncompare_test,
    gold = "GoldStandard",
    goldPositive = "Positive",
    test1 = "Test1",
    test1Positive = "Positive",
    test2 = "Test2",
    test2Positive = "Positive",
    statComp = TRUE,
    test3Positive = NULL
  )
  expect_no_error(result_stat)

  # Without statistical comparison
  result_no_stat <- decisioncompare(
    data = decisioncompare_test,
    gold = "GoldStandard",
    goldPositive = "Positive",
    test1 = "Test1",
    test1Positive = "Positive",
    test2 = "Test2",
    test2Positive = "Positive",
    statComp = FALSE,
    test3Positive = NULL
  )
  expect_no_error(result_no_stat)
})

test_that("decisioncompare respects excludeIndeterminate parameter", {
  # Include indeterminate (treat as negative)
  result_include <- decisioncompare(
    data = decisioncompare_indeterminate,
    gold = "GoldStandard",
    goldPositive = "Positive",
    test1 = "Test1",
    test1Positive = "Positive",
    test2 = "Test2",
    test2Positive = "Positive",
    excludeIndeterminate = FALSE,
    test3Positive = NULL
  )
  expect_s3_class(result_include, "decisioncompareClass")

  # Exclude indeterminate
  result_exclude <- decisioncompare(
    data = decisioncompare_indeterminate,
    gold = "GoldStandard",
    goldPositive = "Positive",
    test1 = "Test1",
    test1Positive = "Positive",
    test2 = "Test2",
    test2Positive = "Positive",
    excludeIndeterminate = TRUE,
    test3Positive = NULL
  )
  expect_s3_class(result_exclude, "decisioncompareClass")
})

test_that("decisioncompare respects od parameter", {
  # Show original data tables
  result_od <- decisioncompare(
    data = decisioncompare_test,
    gold = "GoldStandard",
    goldPositive = "Positive",
    test1 = "Test1",
    test1Positive = "Positive",
    test2 = "Test2",
    test2Positive = "Positive",
    od = TRUE,
    test3Positive = NULL
  )
  expect_no_error(result_od)

  # Hide original data tables
  result_no_od <- decisioncompare(
    data = decisioncompare_test,
    gold = "GoldStandard",
    goldPositive = "Positive",
    test1 = "Test1",
    test1Positive = "Positive",
    test2 = "Test2",
    test2Positive = "Positive",
    od = FALSE,
    test3Positive = NULL
  )
  expect_no_error(result_no_od)
})

test_that("decisioncompare handles all options combined", {
  result <- decisioncompare(
    data = decisioncompare_threetest,
    gold = "GoldStandard",
    goldPositive = "Positive",
    test1 = "Test1",
    test1Positive = "Positive",
    test2 = "Test2",
    test2Positive = "Positive",
    test3 = "Test3",
    test3Positive = "Positive",
    pp = TRUE,
    pprob = 0.25,
    ci = TRUE,
    plot = TRUE,
    radarplot = TRUE,
    statComp = TRUE,
    od = TRUE
  )
  expect_s3_class(result, "decisioncompareClass")
})
