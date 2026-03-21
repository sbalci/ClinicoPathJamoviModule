# Extracted from test-lassologistic.R:131

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
n <- 100
d <- data.frame(
    y = factor(c(rep("neg", 50), rep("pos", 50))),
    x1 = c(rnorm(50, 3), rnorm(50, 7)),
    x2 = c(rnorm(50, 5), rnorm(50, 8))
  )
result <- ll(d, outcome = "y", outcomeLevel = "pos",
               explanatory = c("x1", "x2"),
               scoringSystem = TRUE, scoringMethod = "compare")
