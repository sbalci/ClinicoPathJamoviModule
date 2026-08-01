# ═══════════════════════════════════════════════════════════
# Argument Tests: singlearm
# ═══════════════════════════════════════════════════════════
library(testthat)
data(singlearm_test, package = "ClinicoPath")
data(singlearm_dates, package = "ClinicoPath")
data(singlearm_dmy, package = "ClinicoPath")
data(singlearm_mdy, package = "ClinicoPath")
data(singlearm_datetime, package = "ClinicoPath")
data(singlearm_persontime, package = "ClinicoPath")

run_singlearm <- function(...) {
    args <- list(...)
    for (lvl in c("outcomeLevel", "dod", "dooc", "awd", "awod"))
        if (is.null(args[[lvl]])) args[lvl] <- list(NULL)
    do.call(singlearm, args)
}

test_that("singlearm respects different time output formats", {
  # Days
  result_days <- run_singlearm(
    data = singlearm_test,
    elapsedtime = "time_months",
    outcome = "outcome",
    outcomeLevel = "Dead",
    timetypeoutput = "days"
  )
  expect_no_error(result_days)

  # Weeks
  result_weeks <- run_singlearm(
    data = singlearm_test,
    elapsedtime = "time_months",
    outcome = "outcome",
    outcomeLevel = "Dead",
    timetypeoutput = "weeks"
  )
  expect_no_error(result_weeks)

  # Months
  result_months <- run_singlearm(
    data = singlearm_test,
    elapsedtime = "time_months",
    outcome = "outcome",
    outcomeLevel = "Dead",
    timetypeoutput = "months"
  )
  expect_no_error(result_months)

  # Years
  result_years <- run_singlearm(
    data = singlearm_test,
    elapsedtime = "time_months",
    outcome = "outcome",
    outcomeLevel = "Dead",
    timetypeoutput = "years"
  )
  expect_no_error(result_years)
})

test_that("singlearm respects different date formats", {
  # YMD format
  result_ymd <- run_singlearm(
    data = singlearm_dates,
    tint = TRUE,
    dxdate = "diagnosis_date",
    fudate = "followup_date",
    outcome = "outcome",
    outcomeLevel = "Dead",
    timetypedata = "ymd"
  )
  expect_no_error(result_ymd)

  # DMY format
  result_dmy <- run_singlearm(
    data = singlearm_dmy,
    tint = TRUE,
    dxdate = "diagnosis_date",
    fudate = "followup_date",
    outcome = "outcome",
    outcomeLevel = "Dead",
    timetypedata = "dmy"
  )
  expect_no_error(result_dmy)

  # MDY format
  result_mdy <- run_singlearm(
    data = singlearm_mdy,
    tint = TRUE,
    dxdate = "diagnosis_date",
    fudate = "followup_date",
    outcome = "outcome",
    outcomeLevel = "Dead",
    timetypedata = "mdy"
  )
  expect_no_error(result_mdy)

  # YMDHMS format
  result_hms <- run_singlearm(
    data = singlearm_datetime,
    tint = TRUE,
    dxdate = "diagnosis_datetime",
    fudate = "followup_datetime",
    outcome = "outcome",
    outcomeLevel = "Dead",
    timetypedata = "ymdhms"
  )
  expect_no_error(result_hms)
})

test_that("singlearm respects plot customization options", {
  result <- run_singlearm(
    data = singlearm_test,
    elapsedtime = "time_months",
    outcome = "outcome",
    outcomeLevel = "Dead",
    sc = TRUE,
    endplot = 48,
    ybegin_plot = 0.25,
    yend_plot = 1.0,
    byplot = 6
  )
  expect_no_error(result)
})

test_that("singlearm respects person-time analysis options", {
  result <- run_singlearm(
    data = singlearm_persontime,
    elapsedtime = "time_months",
    outcome = "outcome",
    outcomeLevel = "Dead",
    person_time = TRUE,
    time_intervals = "12, 24, 36, 48",
    rate_multiplier = 1000
  )
  expect_no_error(result)
})

test_that("singlearm respects explanation and summary options", {
  result <- run_singlearm(
    data = singlearm_test,
    elapsedtime = "time_months",
    outcome = "outcome",
    outcomeLevel = "Dead",
    showExplanations = TRUE,
    showSummaries = TRUE
  )
  expect_no_error(result)
})

test_that("singlearm respects advanced diagnostics option", {
  result <- run_singlearm(
    data = singlearm_test,
    elapsedtime = "time_months",
    outcome = "outcome",
    outcomeLevel = "Dead",
    advancedDiagnostics = TRUE
  )
  expect_no_error(result)
})

test_that("singlearm handles all visualization options together", {
  result <- run_singlearm(
    data = singlearm_test,
    elapsedtime = "time_months",
    outcome = "outcome",
    outcomeLevel = "Dead",
    sc = TRUE,
    kmunicate = TRUE,
    ce = TRUE,
    ch = TRUE,
    ci95 = TRUE,
    risktable = TRUE,
    censored = TRUE,
    medianline = "hv"
  )
  expect_no_error(result)
})

test_that("singlearm handles hazard smoothing options", {
  result <- run_singlearm(
    data = singlearm_test,
    elapsedtime = "time_months",
    outcome = "outcome",
    outcomeLevel = "Dead",
    baseline_hazard = TRUE,
    hazard_smoothing = TRUE
  )
  expect_no_error(result)
})
