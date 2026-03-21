# Extracted from test-lassologistic.R:71

# prequel ----------------------------------------------------------------------
ll <- function(data, ...) {
  defaults <- list(
    data = data,
    outcome = NULL, outcomeLevel = NULL, explanatory = NULL,
    penalty = "lasso", alpha = 0.5,
    lambda = "lambda.1se", nfolds = 5,
    random_seed = 42, standardize = TRUE,
    suitabilityCheck = FALSE,
    bootstrapValidation = FALSE, bootstrapN = 50,
    cv_plot = FALSE, coef_plot = FALSE, roc_plot = FALSE,
    scoringSystem = FALSE, scoringMethod = "schneeweiss",
    scoringMaxPoints = 10, scoreLookupTable = FALSE,
    predictions = NULL,
    showSummary = FALSE, showExplanations = FALSE,
    showMethodologyNotes = FALSE, includeClinicalGuidance = FALSE,
    showVariableImportance = FALSE, showModelComparison = FALSE
  )
  args <- modifyList(defaults, list(...))
  do.call(lassologistic, args)
}

# test -------------------------------------------------------------------------
set.seed(42)
d <- data.frame(
    y = factor(rep(c("A", "B"), each = 50)),
    x1 = rnorm(100), x2 = rnorm(100), x3 = rnorm(100)
  )
result <- ll(d, outcome = "y", explanatory = c("x1", "x2", "x3"),
               penalty = "ridge")
