# Extracted from test-singlearm-arguments.R:151

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
    data = singlearm_test,
    elapsedtime = "time_months",
    outcome = "outcome",
    outcomeLevel = "Dead",
    advancedDiagnostics = TRUE
  )
