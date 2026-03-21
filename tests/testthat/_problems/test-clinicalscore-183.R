# Extracted from test-clinicalscore.R:183

# prequel ----------------------------------------------------------------------
cs <- function(data, ...) {
  defaults <- list(
    data = data,
    modelType = "logistic",
    outcome = NULL, outcomeLevel = NULL,
    elapsedtime = NULL, explanatory = NULL,
    autoCategorize = FALSE, categorizeMethod = "median",
    customBreaks = "",
    scoringMethod = "schneeweiss", maxPoints = 10,
    bootstrapValidation = FALSE, bootstrapN = 50,
    scoreLookup = FALSE, showNomogram = FALSE,
    showCalibration = FALSE, calibrationGroups = 4,
    showDiscrimination = FALSE, showDecisionCurve = FALSE,
    showTRIPOD = FALSE, suitabilityCheck = FALSE,
    showSummary = FALSE, showExplanations = FALSE,
    random_seed = 42
  )
  args <- modifyList(defaults, list(...))
  do.call(clinicalscore, args)
}

# test -------------------------------------------------------------------------
set.seed(42)
d <- data.frame(
    y = factor(c(rep("neg", 15), rep("pos", 15))),
    x1 = rnorm(30), x2 = rnorm(30), x3 = rnorm(30),
    x4 = rnorm(30), x5 = rnorm(30)
  )
expect_no_error(
    cs(d, outcome = "y", outcomeLevel = "pos",
       explanatory = c("x1", "x2", "x3", "x4", "x5"),
       suitabilityCheck = TRUE)
  )
