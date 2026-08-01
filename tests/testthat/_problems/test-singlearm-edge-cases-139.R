# Extracted from test-singlearm-edge-cases.R:139

# prequel ----------------------------------------------------------------------
library(testthat)
data(singlearm_small, package = "ClinicoPath")
data(singlearm_censored, package = "ClinicoPath")
data(singlearm_allevents, package = "ClinicoPath")
data(singlearm_early, package = "ClinicoPath")
data(singlearm_missing, package = "ClinicoPath")
data(singlearm_zerotime, package = "ClinicoPath")
data(singlearm_large, package = "ClinicoPath")
data(singlearm_shortfu, package = "ClinicoPath")
data(singlearm_longfu, package = "ClinicoPath")
data(singlearm_test, package = "ClinicoPath")

# test -------------------------------------------------------------------------
test_data <- singlearm_small[1, ]
result <- singlearm(
    data = test_data,
    elapsedtime = "time_months",
    outcome = "outcome",
    outcomeLevel = "Dead"
  )
