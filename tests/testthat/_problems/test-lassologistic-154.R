# Extracted from test-lassologistic.R:154

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
    showSummary = FALSE, showExplanations = FALSE,
    showMethodologyNotes = FALSE, includeClinicalGuidance = FALSE,
    showVariableImportance = FALSE, showModelComparison = FALSE
  )
  args <- modifyList(defaults, list(...))
  do.call(lassologistic, args)
}

# test -------------------------------------------------------------------------
set.seed(42)
n <- 80
d <- data.frame(
    y = factor(c(rep("neg", 40), rep("pos", 40))),
    x1 = c(rnorm(40, 3), rnorm(40, 6)),
    x2 = c(rnorm(40, 5), rnorm(40, 7))
  )
result <- ll(d, outcome = "y", outcomeLevel = "pos",
               explanatory = c("x1", "x2"),
               bootstrapValidation = TRUE, bootstrapN = 50)
val_df <- result$validationTable$asDF
expect_equal(nrow(val_df), 1)
