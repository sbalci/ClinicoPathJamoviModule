# ═══════════════════════════════════════════════════════════
# Basic Tests: singlearm
# ═══════════════════════════════════════════════════════════
library(testthat)
library(ClinicoPath)
data(singlearm_test, package = "ClinicoPath")
data(singlearm_dates, package = "ClinicoPath")
data(singlearm_compete, package = "ClinicoPath")
data(singlearm_causespecific, package = "ClinicoPath")
data(singlearm_landmark, package = "ClinicoPath")

test_that("singlearm creates proper class", {
  result <- singlearm(
    data = singlearm_test,
    elapsedtime = "time_months",
    outcome = "outcome"
  )
  expect_s3_class(result, "singlearmClass")
})

test_that("singlearm handles overall survival", {
  result <- singlearm(
    data = singlearm_test,
    elapsedtime = "time_months",
    outcome = "outcome",
    outcomeLevel = "Dead",
    analysistype = "overall"
  )
  expect_s3_class(result, "singlearmClass")
  expect_true(length(result$results) > 0)
})

test_that("singlearm handles date-based time calculation", {
  result <- singlearm(
    data = singlearm_dates,
    tint = TRUE,
    dxdate = "diagnosis_date",
    fudate = "followup_date",
    outcome = "outcome",
    outcomeLevel = "Dead",
    timetypedata = "ymd",
    timetypeoutput = "months"
  )
  expect_s3_class(result, "singlearmClass")
})

test_that("singlearm handles competing risks analysis", {
  result <- singlearm(
    data = singlearm_compete,
    elapsedtime = "time_months",
    outcome = "outcome",
    analysistype = "compete",
    dod = "Dead_Disease",
    dooc = "Dead_Other",
    awd = "Alive_Disease",
    awod = "Alive_NED"
  )
  expect_s3_class(result, "singlearmClass")
})

test_that("singlearm handles cause-specific analysis", {
  result <- singlearm(
    data = singlearm_causespecific,
    elapsedtime = "time_months",
    outcome = "outcome",
    analysistype = "cause",
    dod = "Dead_Cancer"
  )
  expect_s3_class(result, "singlearmClass")
})

test_that("singlearm handles landmark analysis", {
  result <- singlearm(
    data = singlearm_landmark,
    elapsedtime = "time_months",
    outcome = "outcome",
    outcomeLevel = "Dead",
    uselandmark = TRUE,
    landmark = 3
  )
  expect_s3_class(result, "singlearmClass")
})

test_that("singlearm handles Kaplan-Meier plot", {
  result <- singlearm(
    data = singlearm_test,
    elapsedtime = "time_months",
    outcome = "outcome",
    outcomeLevel = "Dead",
    sc = TRUE
  )
  expect_s3_class(result, "singlearmClass")
  expect_true(!is.null(result$results$survivalplot))
})

test_that("singlearm handles KMunicate plot", {
  result <- singlearm(
    data = singlearm_test,
    elapsedtime = "time_months",
    outcome = "outcome",
    outcomeLevel = "Dead",
    kmunicate = TRUE
  )
  expect_s3_class(result, "singlearmClass")
  expect_true(!is.null(result$results$kmunicateplot))
})

test_that("singlearm handles cumulative events plot", {
  result <- singlearm(
    data = singlearm_test,
    elapsedtime = "time_months",
    outcome = "outcome",
    outcomeLevel = "Dead",
    ce = TRUE
  )
  expect_s3_class(result, "singlearmClass")
})

test_that("singlearm handles cumulative hazard plot", {
  result <- singlearm(
    data = singlearm_test,
    elapsedtime = "time_months",
    outcome = "outcome",
    outcomeLevel = "Dead",
    ch = TRUE
  )
  expect_s3_class(result, "singlearmClass")
})

test_that("singlearm handles custom cutpoints", {
  result <- singlearm(
    data = singlearm_test,
    elapsedtime = "time_months",
    outcome = "outcome",
    outcomeLevel = "Dead",
    cutp = "6, 12, 24, 36, 60"
  )
  expect_s3_class(result, "singlearmClass")
})

test_that("singlearm handles person-time metrics", {
  result <- singlearm(
    data = singlearm_test,
    elapsedtime = "time_months",
    outcome = "outcome",
    outcomeLevel = "Dead",
    person_time = TRUE
  )
  expect_s3_class(result, "singlearmClass")
})

test_that("singlearm handles baseline hazard analysis", {
  result <- singlearm(
    data = singlearm_test,
    elapsedtime = "time_months",
    outcome = "outcome",
    outcomeLevel = "Dead",
    baseline_hazard = TRUE
  )
  expect_s3_class(result, "singlearmClass")
})

test_that("singlearm handles confidence intervals", {
  result <- singlearm(
    data = singlearm_test,
    elapsedtime = "time_months",
    outcome = "outcome",
    outcomeLevel = "Dead",
    ci95 = TRUE
  )
  expect_s3_class(result, "singlearmClass")
})

test_that("singlearm handles risk table", {
  result <- singlearm(
    data = singlearm_test,
    elapsedtime = "time_months",
    outcome = "outcome",
    outcomeLevel = "Dead",
    risktable = TRUE
  )
  expect_s3_class(result, "singlearmClass")
})

test_that("singlearm handles censored markers", {
  result <- singlearm(
    data = singlearm_test,
    elapsedtime = "time_months",
    outcome = "outcome",
    outcomeLevel = "Dead",
    sc = TRUE,
    censored = TRUE
  )
  expect_s3_class(result, "singlearmClass")
})

test_that("singlearm handles median survival line", {
  result <- singlearm(
    data = singlearm_test,
    elapsedtime = "time_months",
    outcome = "outcome",
    outcomeLevel = "Dead",
    sc = TRUE,
    medianline = "hv"
  )
  expect_s3_class(result, "singlearmClass")
})
