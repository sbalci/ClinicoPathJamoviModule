# ═══════════════════════════════════════════════════════════
# Arguments Tests: agreement
# ═══════════════════════════════════════════════════════════
#
# Tests all parameter combinations and option handling for the
# agreement (Interrater Reliability Analysis) jamovi function

library(testthat)
library(ClinicoPath)

# Load test data
data(agreement_pathology, package = "ClinicoPath")
data(agreement_threeRater, package = "ClinicoPath")
data(agreement_ordinal, package = "ClinicoPath")
data(agreement_continuous, package = "ClinicoPath")
data(agreement_multiRater, package = "ClinicoPath")
data(agreement_binary, package = "ClinicoPath")

# ═══════════════════════════════════════════════════════════
# Categorical Agreement Measures
# ═══════════════════════════════════════════════════════════

test_that("cohensKappa option works", {
  result <- agreement(
    data = agreement_pathology,
    vars = c("Pathologist1", "Pathologist2"),
    cohensKappa = TRUE
  )

  expect_s3_class(result, "agreementClass")
})

test_that("fleissKappa option works with 3+ raters", {
  result <- agreement(
    data = agreement_threeRater,
    vars = c("Rater1", "Rater2", "Rater3"),
    fleissKappa = TRUE
  )

  expect_s3_class(result, "agreementClass")
})

test_that("lightKappa option works with 3+ raters", {
  result <- agreement(
    data = agreement_threeRater,
    vars = c("Rater1", "Rater2", "Rater3"),
    lightKappa = TRUE
  )

  expect_s3_class(result, "agreementClass")
})

test_that("kripp option calculates Krippendorff's Alpha", {
  result <- agreement(
    data = agreement_threeRater,
    vars = c("Rater1", "Rater2", "Rater3"),
    kripp = TRUE
  )

  expect_s3_class(result, "agreementClass")
})

test_that("gwet option calculates Gwet's AC1/AC2", {
  result <- agreement(
    data = agreement_pathology,
    vars = c("Pathologist1", "Pathologist2"),
    gwet = TRUE
  )

  expect_s3_class(result, "agreementClass")
})

test_that("finn option calculates Finn coefficient", {
  result <- agreement(
    data = agreement_threeRater,
    vars = c("Rater1", "Rater2", "Rater3"),
    finn = TRUE
  )

  expect_s3_class(result, "agreementClass")
})

test_that("kendallW option calculates Kendall's W", {
  result <- agreement(
    data = agreement_threeRater,
    vars = c("Rater1", "Rater2", "Rater3"),
    kendallW = TRUE
  )

  expect_s3_class(result, "agreementClass")
})

test_that("robinsonA option calculates Robinson's A", {
  result <- agreement(
    data = agreement_threeRater,
    vars = c("Rater1", "Rater2", "Rater3"),
    robinsonA = TRUE
  )

  expect_s3_class(result, "agreementClass")
})

test_that("meanSpearman option calculates Mean Spearman Rho", {
  result <- agreement(
    data = agreement_ordinal,
    vars = c("PathologistA", "PathologistB"),
    meanSpearman = TRUE
  )

  expect_s3_class(result, "agreementClass")
})

# ═══════════════════════════════════════════════════════════
# Weighted Kappa Options
# ═══════════════════════════════════════════════════════════

test_that("wght option accepts 'unweighted'", {
  result <- agreement(
    data = agreement_ordinal,
    vars = c("PathologistA", "PathologistB"),
    wght = "unweighted"
  )

  expect_s3_class(result, "agreementClass")
})

test_that("wght option accepts 'linear'", {
  result <- agreement(
    data = agreement_ordinal,
    vars = c("PathologistA", "PathologistB"),
    wght = "linear"
  )

  expect_s3_class(result, "agreementClass")
})

test_that("wght option accepts 'squared'", {
  result <- agreement(
    data = agreement_ordinal,
    vars = c("PathologistA", "PathologistB"),
    wght = "squared"
  )

  expect_s3_class(result, "agreementClass")
})

# ═══════════════════════════════════════════════════════════
# Continuous Agreement Measures
# ═══════════════════════════════════════════════════════════

test_that("icc option calculates ICC", {
  result <- agreement(
    data = agreement_continuous,
    vars = c("PathologistA", "PathologistB"),
    icc = TRUE
  )

  expect_s3_class(result, "agreementClass")
})

test_that("iccType option accepts 'icc11'", {
  result <- agreement(
    data = agreement_continuous,
    vars = c("PathologistA", "PathologistB"),
    icc = TRUE,
    iccType = "icc11"
  )

  expect_s3_class(result, "agreementClass")
})

test_that("iccType option accepts 'icc21'", {
  result <- agreement(
    data = agreement_continuous,
    vars = c("PathologistA", "PathologistB"),
    icc = TRUE,
    iccType = "icc21"
  )

  expect_s3_class(result, "agreementClass")
})

test_that("iccType option accepts 'icc31'", {
  result <- agreement(
    data = agreement_continuous,
    vars = c("PathologistA", "PathologistB"),
    icc = TRUE,
    iccType = "icc31"
  )

  expect_s3_class(result, "agreementClass")
})

test_that("iccType option accepts 'icc1k'", {
  result <- agreement(
    data = agreement_continuous,
    vars = c("PathologistA", "PathologistB"),
    icc = TRUE,
    iccType = "icc1k"
  )

  expect_s3_class(result, "agreementClass")
})

test_that("iccType option accepts 'icc2k'", {
  result <- agreement(
    data = agreement_continuous,
    vars = c("PathologistA", "PathologistB"),
    icc = TRUE,
    iccType = "icc2k"
  )

  expect_s3_class(result, "agreementClass")
})

test_that("iccType option accepts 'icc3k'", {
  result <- agreement(
    data = agreement_continuous,
    vars = c("PathologistA", "PathologistB"),
    icc = TRUE,
    iccType = "icc3k"
  )

  expect_s3_class(result, "agreementClass")
})

test_that("blandAltman option calculates Bland-Altman analysis", {
  result <- agreement(
    data = agreement_continuous,
    vars = c("PathologistA", "PathologistB"),
    blandAltman = TRUE
  )

  expect_s3_class(result, "agreementClass")
})

test_that("linCCC option calculates Lin's CCC", {
  result <- agreement(
    data = agreement_continuous,
    vars = c("PathologistA", "PathologistB"),
    linCCC = TRUE
  )

  expect_s3_class(result, "agreementClass")
})

test_that("meanPearson option calculates Mean Pearson", {
  result <- agreement(
    data = agreement_continuous,
    vars = c("PathologistA", "PathologistB"),
    meanPearson = TRUE
  )

  expect_s3_class(result, "agreementClass")
})

test_that("tdi option calculates Total Deviation Index", {
  result <- agreement(
    data = agreement_continuous,
    vars = c("PathologistA", "PathologistB"),
    tdi = TRUE
  )

  expect_s3_class(result, "agreementClass")
})

# ═══════════════════════════════════════════════════════════
# Specific Agreement for Binary Data
# ═══════════════════════════════════════════════════════════

test_that("specificAgreement option calculates PSA/NSA", {
  result <- agreement(
    data = agreement_binary,
    vars = c("Pathologist1", "Pathologist2"),
    specificAgreement = TRUE
  )

  expect_s3_class(result, "agreementClass")
})

# ═══════════════════════════════════════════════════════════
# Visualization Options
# ═══════════════════════════════════════════════════════════

test_that("heatmap option generates heatmap", {
  result <- agreement(
    data = agreement_pathology,
    vars = c("Pathologist1", "Pathologist2"),
    heatmap = TRUE
  )

  expect_s3_class(result, "agreementClass")
})

test_that("blandAltmanPlot option generates Bland-Altman plot", {
  result <- agreement(
    data = agreement_continuous,
    vars = c("PathologistA", "PathologistB"),
    blandAltman = TRUE,
    blandAltmanPlot = TRUE
  )

  expect_s3_class(result, "agreementClass")
})

test_that("profilePlot option generates profile plot", {
  result <- agreement(
    data = agreement_continuous,
    vars = c("PathologistA", "PathologistB"),
    profilePlot = TRUE
  )

  expect_s3_class(result, "agreementClass")
})

test_that("forestPlot option generates forest plot", {
  result <- agreement(
    data = agreement_multiRater,
    vars = c("Expert1", "Expert2", "Expert3", "Resident1", "Resident2"),
    forestPlot = TRUE
  )

  expect_s3_class(result, "agreementClass")
})

# ═══════════════════════════════════════════════════════════
# Clustering Options
# ═══════════════════════════════════════════════════════════

test_that("raterClustering option works", {
  result <- agreement(
    data = agreement_multiRater,
    vars = c("Expert1", "Expert2", "Expert3", "Resident1", "Resident2"),
    raterClustering = TRUE
  )

  expect_s3_class(result, "agreementClass")
})

test_that("caseClustering option works", {
  result <- agreement(
    data = agreement_pathology,
    vars = c("Pathologist1", "Pathologist2"),
    caseClustering = TRUE
  )

  expect_s3_class(result, "agreementClass")
})

test_that("raterDendrogram option generates dendrogram", {
  result <- agreement(
    data = agreement_multiRater,
    vars = c("Expert1", "Expert2", "Expert3", "Resident1", "Resident2"),
    raterClustering = TRUE,
    raterDendrogram = TRUE
  )

  expect_s3_class(result, "agreementClass")
})

test_that("caseDendrogram option generates dendrogram", {
  result <- agreement(
    data = agreement_pathology,
    vars = c("Pathologist1", "Pathologist2"),
    caseClustering = TRUE,
    caseDendrogram = TRUE
  )

  expect_s3_class(result, "agreementClass")
})

# ═══════════════════════════════════════════════════════════
# Combined Measures
# ═══════════════════════════════════════════════════════════

test_that("multiple categorical measures can be requested simultaneously", {
  result <- agreement(
    data = agreement_threeRater,
    vars = c("Rater1", "Rater2", "Rater3"),
    fleissKappa = TRUE,
    lightKappa = TRUE,
    kripp = TRUE,
    gwet = TRUE,
    finn = TRUE,
    kendallW = TRUE
  )

  expect_s3_class(result, "agreementClass")
})

test_that("multiple continuous measures can be requested simultaneously", {
  result <- agreement(
    data = agreement_continuous,
    vars = c("PathologistA", "PathologistB"),
    icc = TRUE,
    blandAltman = TRUE,
    linCCC = TRUE,
    meanPearson = TRUE,
    tdi = TRUE
  )

  expect_s3_class(result, "agreementClass")
})

test_that("multiple visualizations can be requested simultaneously", {
  result <- agreement(
    data = agreement_continuous,
    vars = c("PathologistA", "PathologistB"),
    blandAltman = TRUE,
    blandAltmanPlot = TRUE,
    profilePlot = TRUE,
    heatmap = TRUE
  )

  expect_s3_class(result, "agreementClass")
})

# ═══════════════════════════════════════════════════════════
# Bootstrap and Confidence Intervals
# ═══════════════════════════════════════════════════════════

test_that("bootstrap option works", {
  result <- agreement(
    data = agreement_pathology,
    vars = c("Pathologist1", "Pathologist2"),
    cohensKappa = TRUE,
    bootstrap = TRUE,
    nboot = 100  # Small number for testing speed
  )

  expect_s3_class(result, "agreementClass")
})

test_that("ci option works", {
  result <- agreement(
    data = agreement_pathology,
    vars = c("Pathologist1", "Pathologist2"),
    cohensKappa = TRUE,
    ci = TRUE
  )

  expect_s3_class(result, "agreementClass")
})

test_that("ciWidth option accepts custom value", {
  result <- agreement(
    data = agreement_pathology,
    vars = c("Pathologist1", "Pathologist2"),
    cohensKappa = TRUE,
    ci = TRUE,
    ciWidth = 90
  )

  expect_s3_class(result, "agreementClass")
})
