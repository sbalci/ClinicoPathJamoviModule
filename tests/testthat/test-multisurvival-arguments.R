# ═══════════════════════════════════════════════════════════
# Argument Combination Tests: multisurvival
# ═══════════════════════════════════════════════════════════
#
# Tests all valid argument combinations, option interactions,
# and parameter ranges for the multisurvival jamovi function

library(testthat)
library(ClinicoPath)

# Load test data
data(multisurvival_test, package = "ClinicoPath")
data(multisurvival_dates, package = "ClinicoPath")
data(multisurvival_competing, package = "ClinicoPath")
data(multisurvival_risk, package = "ClinicoPath")
data(multisurvival_landmark, package = "ClinicoPath")
data(multisurvival_stratify, package = "ClinicoPath")
data(multisurvival_persontime, package = "ClinicoPath")

test_that("multisurvival handles all time format combinations", {
  # YMD format
  result_ymd <- multisurvival(
    data = multisurvival_dates,
    tint = TRUE,
    dxdate = "dxdate",
    fudate = "fudate",
    timetypedata = "ymd",
    outcome = "outcome"
  )
  expect_s3_class(result_ymd, "multisurvivalClass")
})

test_that("multisurvival handles competing risks analysis", {
  result <- multisurvival(
    data = multisurvival_competing,
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    outcomeLevel = "Dead of Disease",
    dod = "Dead of Disease",
    dooc = "Dead of Other",
    awd = "Alive w Disease",
    awod = "Alive w/o Disease",
    analysistype = "compete",
    multievent = TRUE,
    explanatory = c("treatment", "stage")
  )
  
  expect_s3_class(result, "multisurvivalClass")
})

test_that("multisurvival handles cause-specific survival", {
  result <- multisurvival(
    data = multisurvival_competing,
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    outcomeLevel = "Dead of Disease",
    analysistype = "cause",
    explanatory = "treatment"
  )
  
  expect_s3_class(result, "multisurvivalClass")
})

test_that("multisurvival calculates risk scores", {
  result <- multisurvival(
    data = multisurvival_risk,
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    explanatory = c("stage", "grade", "molecular_subtype"),
    contexpl = c("age", "ki67"),
    calculateRiskScore = TRUE,
    numRiskGroups = "three"
  )
  
  expect_s3_class(result, "multisurvivalClass")
})

test_that("multisurvival handles different risk group numbers", {
  # Two groups
  result_two <- multisurvival(
    data = multisurvival_risk,
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    explanatory = "stage",
    contexpl = "age",
    calculateRiskScore = TRUE,
    numRiskGroups = "two"
  )
  expect_s3_class(result_two, "multisurvivalClass")
  
  # Four groups
  result_four <- multisurvival(
    data = multisurvival_risk,
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    explanatory = "stage",
    contexpl = "age",
    calculateRiskScore = TRUE,
    numRiskGroups = "four"
  )
  expect_s3_class(result_four, "multisurvivalClass")
})

test_that("multisurvival plots risk group survival", {
  result <- multisurvival(
    data = multisurvival_risk,
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    explanatory = c("stage", "grade"),
    contexpl = "age",
    calculateRiskScore = TRUE,
    plotRiskGroups = TRUE,
    numRiskGroups = "three"
  )
  
  expect_s3_class(result, "multisurvivalClass")
})

test_that("multisurvival handles landmark analysis", {
  result <- multisurvival(
    data = multisurvival_landmark,
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    uselandmark = TRUE,
    landmark = 3,  # 3-month landmark
    explanatory = "response_3mo",
    contexpl = "age"
  )
  
  expect_s3_class(result, "multisurvivalClass")
})

test_that("multisurvival handles stratification", {
  result <- multisurvival(
    data = multisurvival_stratify,
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    explanatory = "treatment",
    contexpl = "age",
    use_stratify = TRUE,
    stratvar = "sex"
  )
  
  expect_s3_class(result, "multisurvivalClass")
})

test_that("multisurvival calculates person-time metrics", {
  result <- multisurvival(
    data = multisurvival_persontime,
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    explanatory = "treatment_group",
    person_time = TRUE,
    time_intervals = "12, 36, 60",
    rate_multiplier = 1000
  )
  
  expect_s3_class(result, "multisurvivalClass")
})

test_that("multisurvival handles adjusted survival curves", {
  # Average method
  result_avg <- multisurvival(
    data = multisurvival_test,
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    explanatory = "stage",
    contexpl = "age",
    ac = TRUE,
    adjexplanatory = "treatment",
    ac_method = "average"
  )
  expect_s3_class(result_avg, "multisurvivalClass")
  
  # Conditional method
  result_cond <- multisurvival(
    data = multisurvival_test,
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    explanatory = "stage",
    contexpl = "age",
    ac = TRUE,
    adjexplanatory = "treatment",
    ac_method = "conditional"
  )
  expect_s3_class(result_cond, "multisurvivalClass")
})

test_that("multisurvival shows nomogram", {
  result <- multisurvival(
    data = multisurvival_test,
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    explanatory = c("stage", "grade"),
    contexpl = c("age", "nodes"),
    showNomogram = TRUE
  )
  
  expect_s3_class(result, "multisurvivalClass")
})

test_that("multisurvival handles different HR plot styles", {
  # finalfit style
  result_finalfit <- multisurvival(
    data = multisurvival_test,
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    explanatory = "treatment",
    contexpl = "age",
    hr = TRUE,
    sty = "t1"
  )
  expect_s3_class(result_finalfit, "multisurvivalClass")
  
  # survminer style
  result_survminer <- multisurvival(
    data = multisurvival_test,
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    explanatory = "treatment",
    contexpl = "age",
    hr = TRUE,
    sty = "t3"
  )
  expect_s3_class(result_survminer, "multisurvivalClass")
})

test_that("multisurvival handles KM plot options", {
  result <- multisurvival(
    data = multisurvival_test,
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    explanatory = "treatment",
    km = TRUE,
    ci95 = TRUE,
    risktable = TRUE,
    censored = TRUE,
    pplot = TRUE,
    medianline = "hv",
    endplot = 60,
    byplot = 12
  )
  
  expect_s3_class(result, "multisurvivalClass")
})

test_that("multisurvival handles median line options", {
  # Horizontal
  result_h <- multisurvival(
    data = multisurvival_test,
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    explanatory = "treatment",
    km = TRUE,
    medianline = "h"
  )
  expect_s3_class(result_h, "multisurvivalClass")
  
  # Vertical
  result_v <- multisurvival(
    data = multisurvival_test,
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    explanatory = "treatment",
    km = TRUE,
    medianline = "v"
  )
  expect_s3_class(result_v, "multisurvivalClass")
})

test_that("multisurvival shows explanations and summaries", {
  result <- multisurvival(
    data = multisurvival_test,
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    explanatory = "treatment",
    contexpl = "age",
    showExplanations = TRUE,
    showSummaries = TRUE
  )
  
  expect_s3_class(result, "multisurvivalClass")
})

test_that("multisurvival handles comprehensive multivariable model", {
  result <- multisurvival(
    data = multisurvival_test,
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    outcomeLevel = "1",
    explanatory = c("treatment", "stage", "grade", "sex"),
    contexpl = c("age", "nodes", "biomarker"),
    hr = TRUE,
    km = TRUE,
    ph_cox = TRUE,
    risktable = TRUE,
    ci95 = TRUE,
    showSummaries = TRUE
  )
  
  expect_s3_class(result, "multisurvivalClass")
})

test_that("multisurvival handles complete workflow with all features", {
  result <- multisurvival(
    data = multisurvival_risk,
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    explanatory = c("stage", "grade", "molecular_subtype"),
    contexpl = c("age", "ki67", "nodes_positive"),
    calculateRiskScore = TRUE,
    numRiskGroups = "three",
    plotRiskGroups = TRUE,
    hr = TRUE,
    km = TRUE,
    ph_cox = TRUE,
    showNomogram = TRUE,
    risktable = TRUE,
    ci95 = TRUE,
    censored = TRUE,
    medianline = "hv",
    pplot = TRUE,
    showExplanations = TRUE,
    showSummaries = TRUE
  )
  
  expect_s3_class(result, "multisurvivalClass")
})
