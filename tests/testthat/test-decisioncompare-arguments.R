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
  result_custom <- call_decisioncompare(
    data = decisioncompare_test,
    gold = "GoldStandard",
    goldPositive = "Positive",
    goldNegative = NULL,
    test1 = "Test1",
    test1Positive = "Positive",
    test1Negative = NULL,
    test2 = "Test2",
    test2Positive = "Positive",
    test2Negative = NULL,
    pp = TRUE,
    pprob = 0.15,
    test3Positive = NULL,
    test3Negative = NULL
  )
  expect_s3_class(result_custom, "decisioncompareResults")

  # Without custom prevalence (use dataset prevalence)
  result_default <- call_decisioncompare(
    data = decisioncompare_test,
    gold = "GoldStandard",
    goldPositive = "Positive",
    goldNegative = NULL,
    test1 = "Test1",
    test1Positive = "Positive",
    test1Negative = NULL,
    test2 = "Test2",
    test2Positive = "Positive",
    test2Negative = NULL,
    pp = FALSE,
    test3Positive = NULL,
    test3Negative = NULL
  )
  expect_s3_class(result_default, "decisioncompareResults")
})

test_that("decisioncompare respects different prevalence values", {
  # Screening prevalence (low)
  result_screen <- call_decisioncompare(
    data = decisioncompare_test,
    gold = "GoldStandard",
    goldPositive = "Positive",
    goldNegative = NULL,
    test1 = "Test1",
    test1Positive = "Positive",
    test1Negative = NULL,
    test2 = "Test2",
    test2Positive = "Positive",
    test2Negative = NULL,
    pp = TRUE,
    pprob = 0.05,
    test3Positive = NULL,
    test3Negative = NULL
  )
  expect_no_error(result_screen)

  # Clinical prevalence (high)
  result_clinical <- call_decisioncompare(
    data = decisioncompare_test,
    gold = "GoldStandard",
    goldPositive = "Positive",
    goldNegative = NULL,
    test1 = "Test1",
    test1Positive = "Positive",
    test1Negative = NULL,
    test2 = "Test2",
    test2Positive = "Positive",
    test2Negative = NULL,
    pp = TRUE,
    pprob = 0.60,
    test3Positive = NULL,
    test3Negative = NULL
  )
  expect_no_error(result_clinical)
})

test_that("decisioncompare respects ci parameter", {
  # With CI
  result_ci <- call_decisioncompare(
    data = decisioncompare_test,
    gold = "GoldStandard",
    goldPositive = "Positive",
    goldNegative = NULL,
    test1 = "Test1",
    test1Positive = "Positive",
    test1Negative = NULL,
    test2 = "Test2",
    test2Positive = "Positive",
    test2Negative = NULL,
    ci = TRUE,
    test3Positive = NULL,
    test3Negative = NULL
  )
  expect_no_error(result_ci)

  # Without CI
  result_no_ci <- call_decisioncompare(
    data = decisioncompare_test,
    gold = "GoldStandard",
    goldPositive = "Positive",
    goldNegative = NULL,
    test1 = "Test1",
    test1Positive = "Positive",
    test1Negative = NULL,
    test2 = "Test2",
    test2Positive = "Positive",
    test2Negative = NULL,
    ci = FALSE,
    test3Positive = NULL,
    test3Negative = NULL
  )
  expect_no_error(result_no_ci)
})

test_that("decisioncompare respects plot parameter", {
  # With plot
  result_plot <- call_decisioncompare(
    data = decisioncompare_test,
    gold = "GoldStandard",
    goldPositive = "Positive",
    goldNegative = NULL,
    test1 = "Test1",
    test1Positive = "Positive",
    test1Negative = NULL,
    test2 = "Test2",
    test2Positive = "Positive",
    test2Negative = NULL,
    plot = TRUE,
    test3Positive = NULL,
    test3Negative = NULL
  )
  expect_no_error(result_plot)

  # Without plot
  result_no_plot <- call_decisioncompare(
    data = decisioncompare_test,
    gold = "GoldStandard",
    goldPositive = "Positive",
    goldNegative = NULL,
    test1 = "Test1",
    test1Positive = "Positive",
    test1Negative = NULL,
    test2 = "Test2",
    test2Positive = "Positive",
    test2Negative = NULL,
    plot = FALSE,
    test3Positive = NULL,
    test3Negative = NULL
  )
  expect_no_error(result_no_plot)
})

test_that("decisioncompare respects radarplot parameter", {
  # With radar plot (requires 3 tests)
  result_radar <- call_decisioncompare(
    data = decisioncompare_threetest,
    gold = "GoldStandard",
    goldPositive = "Positive",
    goldNegative = NULL,
    test1 = "Test1",
    test1Positive = "Positive",
    test1Negative = NULL,
    test2 = "Test2",
    test2Positive = "Positive",
    test2Negative = NULL,
    test3 = "Test3",
    test3Positive = "Positive",
    test3Negative = NULL,
    radarplot = TRUE
  )
  expect_no_error(result_radar)

  # Without radar plot
  result_no_radar <- call_decisioncompare(
    data = decisioncompare_threetest,
    gold = "GoldStandard",
    goldPositive = "Positive",
    goldNegative = NULL,
    test1 = "Test1",
    test1Positive = "Positive",
    test1Negative = NULL,
    test2 = "Test2",
    test2Positive = "Positive",
    test2Negative = NULL,
    test3 = "Test3",
    test3Positive = "Positive",
    test3Negative = NULL,
    radarplot = FALSE
  )
  expect_no_error(result_no_radar)
})

test_that("decisioncompare respects statComp parameter", {
  # With statistical comparison
  result_stat <- call_decisioncompare(
    data = decisioncompare_test,
    gold = "GoldStandard",
    goldPositive = "Positive",
    goldNegative = NULL,
    test1 = "Test1",
    test1Positive = "Positive",
    test1Negative = NULL,
    test2 = "Test2",
    test2Positive = "Positive",
    test2Negative = NULL,
    statComp = TRUE,
    test3Positive = NULL,
    test3Negative = NULL
  )
  expect_no_error(result_stat)

  # Without statistical comparison
  result_no_stat <- call_decisioncompare(
    data = decisioncompare_test,
    gold = "GoldStandard",
    goldPositive = "Positive",
    goldNegative = NULL,
    test1 = "Test1",
    test1Positive = "Positive",
    test1Negative = NULL,
    test2 = "Test2",
    test2Positive = "Positive",
    test2Negative = NULL,
    statComp = FALSE,
    test3Positive = NULL,
    test3Negative = NULL
  )
  expect_no_error(result_no_stat)
})

test_that("decisioncompare respects excludeIndeterminate parameter", {
  # Include indeterminate (treat as negative)
  result_include <- call_decisioncompare(
    data = decisioncompare_indeterminate,
    gold = "GoldStandard",
    goldPositive = "Positive",
    goldNegative = NULL,
    test1 = "Test1",
    test1Positive = "Positive",
    test1Negative = NULL,
    test2 = "Test2",
    test2Positive = "Positive",
    test2Negative = NULL,
    excludeIndeterminate = FALSE,
    test3Positive = NULL,
    test3Negative = NULL
  )
  expect_s3_class(result_include, "decisioncompareResults")

  # Exclude indeterminate
  result_exclude <- call_decisioncompare(
    data = decisioncompare_indeterminate,
    gold = "GoldStandard",
    goldPositive = "Positive",
    goldNegative = NULL,
    test1 = "Test1",
    test1Positive = "Positive",
    test1Negative = NULL,
    test2 = "Test2",
    test2Positive = "Positive",
    test2Negative = NULL,
    excludeIndeterminate = TRUE,
    test3Positive = NULL,
    test3Negative = NULL
  )
  expect_s3_class(result_exclude, "decisioncompareResults")
})

test_that("decisioncompare respects od parameter", {
  # Show original data tables
  result_od <- call_decisioncompare(
    data = decisioncompare_test,
    gold = "GoldStandard",
    goldPositive = "Positive",
    goldNegative = NULL,
    test1 = "Test1",
    test1Positive = "Positive",
    test1Negative = NULL,
    test2 = "Test2",
    test2Positive = "Positive",
    test2Negative = NULL,
    od = TRUE,
    test3Positive = NULL,
    test3Negative = NULL
  )
  expect_no_error(result_od)

  # Hide original data tables
  result_no_od <- call_decisioncompare(
    data = decisioncompare_test,
    gold = "GoldStandard",
    goldPositive = "Positive",
    goldNegative = NULL,
    test1 = "Test1",
    test1Positive = "Positive",
    test1Negative = NULL,
    test2 = "Test2",
    test2Positive = "Positive",
    test2Negative = NULL,
    od = FALSE,
    test3Positive = NULL,
    test3Negative = NULL
  )
  expect_no_error(result_no_od)
})

test_that("decisioncompare handles all options combined", {
  # `pp` and `ci` are mutually exclusive by design: with a user-supplied prior the
  # comparison table reports Bayes-adjusted PPV/NPV, while the epiR CI table reports
  # them at the observed sample prevalence, so the two panes would disagree. The UI
  # enforces this with `enable: (!pp)` on the ci checkbox; a programmatic caller gets
  # an explicit error. Exercise each branch separately rather than the invalid pair.
  common <- list(
    data = decisioncompare_threetest,
    gold = "GoldStandard", goldPositive = "Positive", goldNegative = NULL,
    test1 = "Test1", test1Positive = "Positive", test1Negative = NULL,
    test2 = "Test2", test2Positive = "Positive", test2Negative = NULL,
    test3 = "Test3", test3Positive = "Positive", test3Negative = NULL,
    stratify = NULL,
    plot = TRUE, radarplot = TRUE, heatmap = TRUE,
    statComp = TRUE, opa = TRUE, od = TRUE, fnote = TRUE,
    showSummary = TRUE, showExplanations = TRUE, showReportSentence = TRUE)

  with_ci <- do.call(call_decisioncompare, utils::modifyList(common, list(ci = TRUE)))
  expect_s3_class(with_ci, "decisioncompareResults")
  expect_gt(with_ci$comparisonTable$rowCount, 0L)
  expect_gt(with_ci$epirTable1$rowCount, 0L)

  with_prior <- do.call(call_decisioncompare,
                        utils::modifyList(common, list(pp = TRUE, pprob = 0.25)))
  expect_s3_class(with_prior, "decisioncompareResults")
  expect_gt(with_prior$comparisonTable$rowCount, 0L)

  # and the invalid pair is refused with an actionable message, not a cryptic one
  expect_error(
    do.call(call_decisioncompare, utils::modifyList(common, list(ci = TRUE, pp = TRUE))),
    "Validation failed")
})
