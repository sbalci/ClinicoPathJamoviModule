# ═══════════════════════════════════════════════════════════
# Argument Tests: survivalcont
# ═══════════════════════════════════════════════════════════
library(testthat)
library(ClinicoPath)
data(survivalcont_test, package = "ClinicoPath")
data(survivalcont_dates, package = "ClinicoPath")
data(survivalcont_landmark, package = "ClinicoPath")
data(survivalcont_multicut, package = "ClinicoPath")

test_that("survivalcont respects different analysis types", {
  # Overall survival
  result_overall <- survivalcont(
    data = survivalcont_test,
    elapsedtime = "time_months",
    outcome = "outcome",
    contexpl = "biomarker",
    analysistype = "overall"
  )
  expect_no_error(result_overall)

  # Cause-specific
  result_cause <- survivalcont(
    data = survivalcont_test,
    elapsedtime = "time_months",
    outcome = "outcome",
    contexpl = "biomarker",
    analysistype = "cause",
    dod = "Dead"
  )
  expect_no_error(result_cause)
})

test_that("survivalcont respects date-based time calculation", {
  result <- survivalcont(
    data = survivalcont_dates,
    tint = TRUE,
    dxdate = "diagnosis_date",
    fudate = "followup_date",
    outcome = "outcome",
    outcomeLevel = "Dead",
    contexpl = "continuous_marker",
    timetypedata = "ymd",
    timetypeoutput = "months"
  )
  expect_no_error(result)
})

test_that("survivalcont respects different time output formats", {
  # Days
  result_days <- survivalcont(
    data = survivalcont_test,
    elapsedtime = "time_months",
    outcome = "outcome",
    contexpl = "biomarker",
    timetypeoutput = "days"
  )
  expect_no_error(result_days)

  # Months
  result_months <- survivalcont(
    data = survivalcont_test,
    elapsedtime = "time_months",
    outcome = "outcome",
    contexpl = "biomarker",
    timetypeoutput = "months"
  )
  expect_no_error(result_months)

  # Years
  result_years <- survivalcont(
    data = survivalcont_test,
    elapsedtime = "time_months",
    outcome = "outcome",
    contexpl = "biomarker",
    timetypeoutput = "years"
  )
  expect_no_error(result_years)
})

test_that("survivalcont respects landmark analysis option", {
  result <- survivalcont(
    data = survivalcont_landmark,
    elapsedtime = "time_months",
    outcome = "outcome",
    outcomeLevel = "Dead",
    contexpl = "early_response_score",
    uselandmark = TRUE,
    landmark = 6
  )
  expect_no_error(result)
})

test_that("survivalcont respects plot customization options", {
  result <- survivalcont(
    data = survivalcont_test,
    elapsedtime = "time_months",
    outcome = "outcome",
    outcomeLevel = "Dead",
    contexpl = "biomarker",
    sc = TRUE,
    endplot = 48,
    ybegin_plot = 0.25,
    yend_plot = 1.0,
    byplot = 6
  )
  expect_no_error(result)
})

test_that("survivalcont respects multiple cutoff options", {
  # 2 cutoffs (3 groups)
  result_2cut <- survivalcont(
    data = survivalcont_multicut,
    elapsedtime = "time_months",
    outcome = "outcome",
    outcomeLevel = "Dead",
    contexpl = "risk_score",
    multiple_cutoffs = TRUE,
    num_cutoffs = "two"
  )
  expect_no_error(result_2cut)

  # 3 cutoffs (4 groups)
  result_3cut <- survivalcont(
    data = survivalcont_multicut,
    elapsedtime = "time_months",
    outcome = "outcome",
    outcomeLevel = "Dead",
    contexpl = "risk_score",
    multiple_cutoffs = TRUE,
    num_cutoffs = "three"
  )
  expect_no_error(result_3cut)
})

test_that("survivalcont respects different cutoff methods", {
  # Quantile-based
  result_quantile <- survivalcont(
    data = survivalcont_multicut,
    elapsedtime = "time_months",
    outcome = "outcome",
    outcomeLevel = "Dead",
    contexpl = "risk_score",
    multiple_cutoffs = TRUE,
    cutoff_method = "quantile"
  )
  expect_no_error(result_quantile)

  # Recursive optimal
  result_recursive <- survivalcont(
    data = survivalcont_multicut,
    elapsedtime = "time_months",
    outcome = "outcome",
    outcomeLevel = "Dead",
    contexpl = "risk_score",
    multiple_cutoffs = TRUE,
    cutoff_method = "recursive"
  )
  expect_no_error(result_recursive)
})

test_that("survivalcont respects minimum group size", {
  result <- survivalcont(
    data = survivalcont_multicut,
    elapsedtime = "time_months",
    outcome = "outcome",
    outcomeLevel = "Dead",
    contexpl = "risk_score",
    multiple_cutoffs = TRUE,
    min_group_size = 15
  )
  expect_no_error(result)
})

test_that("survivalcont handles all visualization options together", {
  result <- survivalcont(
    data = survivalcont_test,
    elapsedtime = "time_months",
    outcome = "outcome",
    outcomeLevel = "Dead",
    contexpl = "biomarker",
    sc = TRUE,
    kmunicate = TRUE,
    ce = TRUE,
    ch = TRUE,
    findcut = TRUE
  )
  expect_no_error(result)
})
