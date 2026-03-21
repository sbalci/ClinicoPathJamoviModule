# Extracted from test-lassologistic.R:52

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
x1 <- rnorm(n)
x2 <- rnorm(n)
x3 <- rnorm(n)
y <- factor(ifelse(x1 + 0.5 * x2 + rnorm(n, 0, 0.5) > 0, "pos", "neg"))
d <- data.frame(y = y, x1 = x1, x2 = x2, x3 = x3)
result <- ll(d, outcome = "y", outcomeLevel = "pos",
               explanatory = c("x1", "x2", "x3"))
