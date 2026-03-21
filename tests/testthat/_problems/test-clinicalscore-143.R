# Extracted from test-clinicalscore.R:143

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
    y = factor(c(rep("neg", 40), rep("pos", 40))),
    x1 = c(rnorm(40, 3), rnorm(40, 6)),
    x2 = rnorm(80)
  )
result <- cs(d, outcome = "y", outcomeLevel = "pos",
               explanatory = c("x1", "x2"),
               bootstrapValidation = TRUE, bootstrapN = 50)
