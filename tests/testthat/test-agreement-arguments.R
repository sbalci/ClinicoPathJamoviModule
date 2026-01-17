# Arguments Tests: agreement
#
# Tests all parameter combinations and option handling for the
# agreement (Interrater Reliability Analysis) jamovi function

library(testthat)
devtools::load_all()

# Load test data
data(agreement_pathology, package = "ClinicoPath")
data(agreement_threeRater, package = "ClinicoPath")
data(agreement_ordinal, package = "ClinicoPath")
data(agreement_continuous, package = "ClinicoPath")
data(agreement_multiRater, package = "ClinicoPath")
data(agreement_binary, package = "ClinicoPath")

# Categorical Agreement Measures

test_that("Agreement works with default options", {
  result <- agreement(
    data = agreement_pathology,
    vars = c("Pathologist1", "Pathologist2")
  )
  expect_s3_class(result, "agreementResults")
})

test_that("lightKappa option works with 3+ raters", {
  result <- agreement(
    data = agreement_threeRater,
    vars = c("Rater1", "Rater2", "Rater3"),
    lightKappa = TRUE
  )
  expect_s3_class(result, "agreementResults")
})

test_that("kripp option calculates Krippendorff's Alpha", {
  result <- agreement(
    data = agreement_threeRater,
    vars = c("Rater1", "Rater2", "Rater3"),
    kripp = TRUE
  )
  expect_s3_class(result, "agreementResults")
})

test_that("gwet option calculates Gwet's AC1/AC2", {
  result <- agreement(
    data = agreement_pathology,
    vars = c("Pathologist1", "Pathologist2"),
    gwet = TRUE
  )
  expect_s3_class(result, "agreementResults")
})

test_that("finn option calculates Finn coefficient", {
  result <- agreement(
    data = agreement_threeRater,
    vars = c("Rater1", "Rater2", "Rater3"),
    finn = TRUE,
    finnLevels = 3
  )
  expect_s3_class(result, "agreementResults")
})

test_that("kendallW option calculates Kendall's W", {
  result <- agreement(
    data = agreement_threeRater,
    vars = c("Rater1", "Rater2", "Rater3"),
    kendallW = TRUE
  )
  expect_s3_class(result, "agreementResults")
})

test_that("robinsonA option calculates Robinson's A", {
  result <- agreement(
    data = agreement_threeRater,
    vars = c("Rater1", "Rater2", "Rater3"),
    robinsonA = TRUE
  )
  expect_s3_class(result, "agreementResults")
})

test_that("meanSpearman option calculates Mean Spearman Rho", {
  data(agreement_ordinal, package = "ClinicoPath")
  result <- agreement(
    data = agreement_ordinal,
    vars = c("PathologistA", "PathologistB"),
    meanSpearman = TRUE
  )
  expect_s3_class(result, "agreementResults")
})

# Weighted Kappa Options

test_that("wght option accepts 'unweighted'", {
  data(agreement_ordinal, package = "ClinicoPath")
  result <- agreement(
    data = agreement_ordinal,
    vars = c("PathologistA", "PathologistB"),
    wght = "unweighted"
  )
  expect_s3_class(result, "agreementResults")
})

test_that("wght option accepts 'equal'", {
  data(agreement_ordinal, package = "ClinicoPath")
  agreement_ordinal$PathologistA <- factor(agreement_ordinal$PathologistA, ordered = TRUE)
  agreement_ordinal$PathologistB <- factor(agreement_ordinal$PathologistB, ordered = TRUE)
  result <- agreement(
    data = agreement_ordinal,
    vars = c("PathologistA", "PathologistB"),
    wght = "equal"
  )
  expect_s3_class(result, "agreementResults")
})

test_that("wght option accepts 'squared'", {
  data(agreement_ordinal, package = "ClinicoPath")
  agreement_ordinal$PathologistA <- factor(agreement_ordinal$PathologistA, ordered = TRUE)
  agreement_ordinal$PathologistB <- factor(agreement_ordinal$PathologistB, ordered = TRUE)
  result <- agreement(
    data = agreement_ordinal,
    vars = c("PathologistA", "PathologistB"),
    wght = "squared"
  )
  expect_s3_class(result, "agreementResults")
})

# Continuous Agreement Measures

test_that("icc option calculates ICC", {
  data(agreement_continuous, package = "ClinicoPath")
  result <- agreement(
    data = agreement_continuous,
    vars = c("MeasurementA", "MeasurementB"),
    icc = TRUE
  )
  expect_s3_class(result, "agreementResults")
})

test_that("iccType option accepts 'icc21'", {
  data(agreement_continuous, package = "ClinicoPath")
  result <- agreement(
    data = agreement_continuous,
    vars = c("MeasurementA", "MeasurementB"),
    icc = TRUE,
    iccType = "icc21"
  )
  expect_s3_class(result, "agreementResults")
})

test_that("blandAltmanPlot option generates Bland-Altman plot", {
  data(agreement_continuous, package = "ClinicoPath")
  result <- agreement(
    data = agreement_continuous,
    vars = c("MeasurementA", "MeasurementB"),
    blandAltmanPlot = TRUE
  )
  expect_s3_class(result, "agreementResults")
})

test_that("linCCC option calculates Lin's CCC", {
  data(agreement_continuous, package = "ClinicoPath")
  result <- agreement(
    data = agreement_continuous,
    vars = c("MeasurementA", "MeasurementB"),
    linCCC = TRUE
  )
  expect_s3_class(result, "agreementResults")
})

test_that("meanPearson option calculates Mean Pearson", {
  data(agreement_continuous, package = "ClinicoPath")
  result <- agreement(
    data = agreement_continuous,
    vars = c("MeasurementA", "MeasurementB"),
    meanPearson = TRUE
  )
  expect_s3_class(result, "agreementResults")
})

test_that("tdi option calculates Total Deviation Index", {
  data(agreement_continuous, package = "ClinicoPath")
  result <- agreement(
    data = agreement_continuous,
    vars = c("MeasurementA", "MeasurementB"),
    tdi = TRUE
  )
  expect_s3_class(result, "agreementResults")
})

# Specific Agreement for Binary Data

test_that("specificAgreement option calculates PSA/NSA", {
  data(agreement_binary, package = "ClinicoPath")
  result <- agreement(
    data = agreement_binary,
    vars = c("PathologistX", "PathologistY"),
    specificAgreement = TRUE
  )
  expect_s3_class(result, "agreementResults")
})

# Visualization Options

test_that("agreementHeatmap option generates heatmap", {
  result <- agreement(
    data = agreement_pathology,
    vars = c("Pathologist1", "Pathologist2"),
    agreementHeatmap = TRUE
  )
  expect_s3_class(result, "agreementResults")
})

test_that("raterProfiles option generates profile plot", {
  data(agreement_continuous, package = "ClinicoPath")
  result <- agreement(
    data = agreement_continuous,
    vars = c("MeasurementA", "MeasurementB"),
    raterProfiles = TRUE
  )
  expect_s3_class(result, "agreementResults")
})

# Clustering Options

test_that("raterClustering option works", {
  data(agreement_multiRater, package = "ClinicoPath")
  result <- agreement(
    data = agreement_multiRater,
    vars = c("SeniorPath1", "SeniorPath2", "MidLevelPath", "JuniorPath1", "JuniorPath2"),
    raterClustering = TRUE
  )
  expect_s3_class(result, "agreementResults")
})

test_that("caseClustering option works", {
  result <- agreement(
    data = agreement_pathology,
    vars = c("Pathologist1", "Pathologist2"),
    caseClustering = TRUE
  )
  expect_s3_class(result, "agreementResults")
})

# Bootstrap and Confidence Intervals

test_that("bootstrap option works", {
  result <- agreement(
    data = agreement_pathology,
    vars = c("Pathologist1", "Pathologist2"),
    bootstrap = TRUE
  )
  expect_s3_class(result, "agreementResults")
})
