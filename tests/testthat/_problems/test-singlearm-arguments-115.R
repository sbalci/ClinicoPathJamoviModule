# Extracted from test-singlearm-arguments.R:115

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
    sc = TRUE,
    endplot = 48,
    ybegin_plot = 0.25,
    yend_plot = 1.0,
    byplot = 6
  )
