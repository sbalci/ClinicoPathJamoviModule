# User-supplied model labels reach two jamovi sinks that parse them:
#   - Table$setNote() re-translates the composed note and cuts it at " [", so a model named
#     "Model [v2]" reduced the range-of-benefit note to "Model";
#   - table cells and column titles are parsed as HTML, so "Risk<High>" displayed as "Risk".
# Both are routed through .rocLabel() (security audit 2026-10-14).

library(testthat)

dca_label_fixture <- function() {
  set.seed(7)
  n <- 200
  y <- rbinom(n, 1, 0.3)
  data.frame(
    outcome = factor(ifelse(y == 1, "Yes", "No"), levels = c("No", "Yes")),
    # below every threshold: classifies nobody positive, so NB = 0 and it never beats
    # the references -> the "none" note names it
    flat = rep(0.02, n),
    good = plogis(-1 + 2.5 * y + rnorm(n))
  )
}

test_that("bracketed model name does not truncate the range-of-benefit note", {
  res <- decisioncurve(
    data = dca_label_fixture(), outcome = "outcome", outcomePositive = "Yes",
    models = c("flat", "good"), modelNames = "Model [v2], Risk<High>",
    showBenefitRange = TRUE, decisionRulePositive = NULL
  )
  note <- res$benefitRangeTable$notes[["none"]]$note
  expect_true(nzchar(note))
  expect_match(note, "never exceeds both reference strategies", fixed = TRUE)
  expect_match(note, "Model (v2)", fixed = TRUE)
})

test_that("a '<' before a letter in a model name survives in cells and column titles", {
  res <- decisioncurve(
    data = dca_label_fixture(), outcome = "outcome", outcomePositive = "Yes",
    models = c("flat", "good"), modelNames = "Model [v2], Risk<High>",
    showBenefitRange = TRUE, showTable = TRUE, decisionRulePositive = NULL
  )
  titles <- vapply(res$resultsTable$columns, function(c) c$title, character(1))
  expect_true("Risk< High>" %in% titles)
  cells <- res$benefitRangeTable$asDF$model
  expect_true("Risk< High>" %in% cells)
  # cells are never re-translated, so the real bracket is kept there
  expect_true("Model [v2]" %in% cells)
})
