# Extracted from test-singlearm-arguments.R:128

# prequel ----------------------------------------------------------------------
library(testthat)
data(singlearm_test, package = "ClinicoPath")
data(singlearm_dates, package = "ClinicoPath")
data(singlearm_dmy, package = "ClinicoPath")
data(singlearm_mdy, package = "ClinicoPath")
data(singlearm_datetime, package = "ClinicoPath")
data(singlearm_persontime, package = "ClinicoPath")

# test -------------------------------------------------------------------------
result <- singlearm(
    data = singlearm_persontime,
    elapsedtime = "time_months",
    outcome = "outcome",
    outcomeLevel = "Dead",
    person_time = TRUE,
    time_intervals = "12, 24, 36, 48",
    rate_multiplier = 1000
  )
