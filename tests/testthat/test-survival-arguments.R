# ═══════════════════════════════════════════════════════════
# Argument Combination Tests: survival
# ═══════════════════════════════════════════════════════════
#
# Tests all valid argument combinations and option interactions
# for the survival jamovi function

library(testthat)

# Load test data
data(survival_test, package = "ClinicoPath")

test_that("survival handles date-based time calculation", {
  data(survival_dates, package = "ClinicoPath")

  result <- run_survival(
    data = survival_dates,
    tint = TRUE,
    dxdate = "dxdate",
    fudate = "fudate",
    outcome = "outcome",
    explanatory = "treatment",
    timetypedata = "ymd",
    timetypeoutput = "months"
  )

  expect_s3_class(result, "survivalResults")
})

test_that("survival handles competing risks analysis", {
  data(survival_competing, package = "ClinicoPath")

  result <- run_survival(
    data = survival_competing,
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    outcomeLevel = "Dead of Disease",
    dod = "Dead of Disease",
    dooc = "Dead of Other",
    awd = "Alive w Disease",
    awod = "Alive w/o Disease",
    analysistype = "compete",
    multievent = TRUE,
    explanatory = "treatment"
  )

  expect_s3_class(result, "survivalResults")
})

test_that("survival handles landmark analysis", {
  data(survival_landmark, package = "ClinicoPath")

  result <- run_survival(
    data = survival_landmark,
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    uselandmark = TRUE,
    landmark = 6,  # 6 months landmark
    explanatory = "response_6mo"
  )

  expect_s3_class(result, "survivalResults")
})

test_that("survival handles stratified Cox regression", {
  data(survival_stratified, package = "ClinicoPath")

  result <- run_survival(
    data = survival_stratified,
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    explanatory = "treatment",
    stratified_cox = TRUE,
    strata_variable = "sex"
  )

  expect_s3_class(result, "survivalResults")
})

test_that("survival calculates person-time metrics", {
  data(survival_person_time, package = "ClinicoPath")

  result <- run_survival(
    data = survival_person_time,
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    explanatory = "risk_category",
    person_time = TRUE,
    time_intervals = "12, 36, 60",
    rate_multiplier = 100
  )

  expect_s3_class(result, "survivalResults")
})

test_that("survival performs RMST analysis", {
  data(survival_rmst, package = "ClinicoPath")

  result <- run_survival(
    data = survival_rmst,
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    explanatory = "treatment",
    rmst_analysis = TRUE,
    rmst_tau = 48  # 48 month time horizon
  )

  expect_s3_class(result, "survivalResults")
})

test_that("survival generates survival plots", {
  result <- run_survival(
    data = survival_test,
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    explanatory = "treatment",
    sc = TRUE,
    endplot = 60,
    byplot = 12
  )

  expect_s3_class(result, "survivalResults")
})

test_that("survival generates KMunicate-style plot", {
  result <- run_survival(
    data = survival_test,
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    explanatory = "treatment",
    kmunicate = TRUE
  )

  expect_s3_class(result, "survivalResults")
})

test_that("survival includes risk table", {
  result <- run_survival(
    data = survival_test,
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    explanatory = "treatment",
    sc = TRUE,
    risktable = TRUE
  )

  expect_s3_class(result, "survivalResults")
})

test_that("survival includes 95% confidence intervals", {
  result <- run_survival(
    data = survival_test,
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    explanatory = "treatment",
    sc = TRUE,
    ci95 = TRUE
  )

  expect_s3_class(result, "survivalResults")
})

test_that("survival marks censored observations", {
  result <- run_survival(
    data = survival_test,
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    explanatory = "treatment",
    sc = TRUE,
    censored = TRUE
  )

  expect_s3_class(result, "survivalResults")
})

test_that("survival shows p-value on plot", {
  result <- run_survival(
    data = survival_test,
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    explanatory = "treatment",
    sc = TRUE,
    pplot = TRUE
  )

  expect_s3_class(result, "survivalResults")
})

test_that("survival shows median survival line", {
  result <- run_survival(
    data = survival_test,
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    explanatory = "treatment",
    sc = TRUE,
    medianline = "hv"  # both horizontal and vertical
  )

  expect_s3_class(result, "survivalResults")
})

test_that("survival generates cumulative events plot", {
  result <- run_survival(
    data = survival_test,
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    explanatory = "treatment",
    ce = TRUE
  )

  expect_s3_class(result, "survivalResults")
})

test_that("survival generates cumulative hazard plot", {
  result <- run_survival(
    data = survival_test,
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    explanatory = "treatment",
    ch = TRUE
  )

  expect_s3_class(result, "survivalResults")
})

test_that("survival tests proportional hazards assumption", {
  result <- run_survival(
    data = survival_test,
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    explanatory = "treatment",
    ph_cox = TRUE
  )

  expect_s3_class(result, "survivalResults")
})

test_that("survival generates log-log plot", {
  result <- run_survival(
    data = survival_test,
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    explanatory = "treatment",
    loglog = TRUE
  )

  expect_s3_class(result, "survivalResults")
})

test_that("survival performs pairwise comparisons", {
  result <- run_survival(
    data = survival_test,
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    explanatory = "treatment",  # 3 groups
    pw = TRUE,
    padjustmethod = "holm"
  )

  expect_s3_class(result, "survivalResults")
})

test_that("survival handles different p-value adjustment methods", {
  # Test with Bonferroni
  result1 <- run_survival(
    data = survival_test,
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    explanatory = "treatment",
    pw = TRUE,
    padjustmethod = "bonferroni"
  )
  expect_s3_class(result1, "survivalResults")

  # Test with FDR
  result2 <- run_survival(
    data = survival_test,
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    explanatory = "treatment",
    pw = TRUE,
    padjustmethod = "fdr"
  )
  expect_s3_class(result2, "survivalResults")
})

test_that("survival calculates residual diagnostics", {
  result <- run_survival(
    data = survival_test,
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    explanatory = "treatment",
    residual_diagnostics = TRUE
  )

  expect_s3_class(result, "survivalResults")
})

test_that("survival shows analysis explanations", {
  result <- run_survival(
    data = survival_test,
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    explanatory = "treatment",
    showExplanations = TRUE
  )

  expect_s3_class(result, "survivalResults")
})

test_that("survival shows natural language summaries", {
  result <- run_survival(
    data = survival_test,
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    explanatory = "treatment",
    showSummaries = TRUE
  )

  expect_s3_class(result, "survivalResults")
})

test_that("survival handles custom cutpoints", {
  result <- run_survival(
    data = survival_test,
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    explanatory = "treatment",
    cutp = "6, 12, 24, 36"
  )

  expect_s3_class(result, "survivalResults")
})

test_that("survival handles different time output types", {
  data(survival_dates, package = "ClinicoPath")

  # Test with years output
  result1 <- run_survival(
    data = survival_dates,
    tint = TRUE,
    dxdate = "dxdate",
    fudate = "fudate",
    outcome = "outcome",
    timetypedata = "ymd",
    timetypeoutput = "years"
  )
  expect_s3_class(result1, "survivalResults")

  # Test with days output
  result2 <- run_survival(
    data = survival_dates,
    tint = TRUE,
    dxdate = "dxdate",
    fudate = "fudate",
    outcome = "outcome",
    timetypedata = "ymd",
    timetypeoutput = "days"
  )
  expect_s3_class(result2, "survivalResults")
})

test_that("survival handles cause-specific survival", {
  data(survival_competing, package = "ClinicoPath")

  result <- run_survival(
    data = survival_competing,
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    outcomeLevel = "Dead of Disease",
    analysistype = "cause",
    explanatory = "treatment"
  )

  expect_s3_class(result, "survivalResults")
})

test_that("survival combines multiple features", {
  # Comprehensive analysis with many options
  result <- run_survival(
    data = survival_test,
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    explanatory = "treatment",
    sc = TRUE,
    risktable = TRUE,
    ci95 = TRUE,
    censored = TRUE,
    pplot = TRUE,
    medianline = "hv",
    ph_cox = TRUE,
    pw = TRUE,
    padjustmethod = "holm",
    person_time = TRUE,
    showExplanations = TRUE,
    showSummaries = TRUE
  )

  expect_s3_class(result, "survivalResults")
})
