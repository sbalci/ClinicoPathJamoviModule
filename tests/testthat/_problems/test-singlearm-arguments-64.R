# Extracted from test-singlearm-arguments.R:64

# prequel ----------------------------------------------------------------------
library(testthat)
data(singlearm_test, package = "ClinicoPath")
data(singlearm_dates, package = "ClinicoPath")
data(singlearm_dmy, package = "ClinicoPath")
data(singlearm_mdy, package = "ClinicoPath")
data(singlearm_datetime, package = "ClinicoPath")
data(singlearm_persontime, package = "ClinicoPath")

# test -------------------------------------------------------------------------
result_ymd <- singlearm(
    data = singlearm_dates,
    tint = TRUE,
    dxdate = "diagnosis_date",
    fudate = "followup_date",
    outcome = "outcome",
    outcomeLevel = "Dead",
    timetypedata = "ymd"
  )
