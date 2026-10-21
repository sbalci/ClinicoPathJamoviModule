# Regression tests for the 2026-10-21 independent review (development-ideas/
# decisioncurve-independent-review-2026-10-21.md), findings DC-21-01, DC-21-02 and DC-21-03.
# Level arguments are passed explicitly through do.call(): the wrapper reads a bare symbol
# passed to a variable option as a column name.

library(testthat)

rf_run <- function(d, ...) {
  args <- list(data = d, outcome = "y", outcomePositive = "event", models = "r",
               decisionRuleVar = NULL, decisionRulePositive = NULL)
  extra <- list(...)
  for (nm in names(extra)) args[nm] <- list(extra[[nm]])
  suppressWarnings(do.call(ClinicoPath::decisioncurve, args))
}

rf_notes <- function(tbl) paste(vapply(tbl$notes, function(z) z$note, ""), collapse = " ")

test_that("DC-21-01 a null predictor's range is stated as apparent, with no uncertainty", {
  # N11 fixture, first replicate: r is independent of y yet shows a short range.
  set.seed(1e7 + 1)
  d <- data.frame(y = factor(ifelse(rbinom(200, 1, 0.2) == 1, "event", "none"), c("none", "event")),
                  r = runif(200))
  res <- rf_run(d, showBenefitRange = TRUE, showInterpretation = TRUE, showTable = FALSE)
  expect_true(is.finite(res$benefitRangeTable$asDF$range_start))      # precondition
  note <- rf_notes(res$benefitRangeTable)
  expect_match(note, "apparent (in-sample)", fixed = TRUE)
  expect_match(note, "no allowance for sampling variation", fixed = TRUE)
  expect_false(grepl("only worth using within this range", note, fixed = TRUE))
  expect_match(note, "Turn on Bootstrap confidence intervals", fixed = TRUE)
  txt <- res$summaryText$content
  expect_match(txt, "apparent (in-sample) net benefit exceeds both", fixed = TRUE)
  expect_false(grepl("the range over which this model beats both", txt, fixed = TRUE))
  # with the bands drawn, the pointer reads them instead
  res_ci <- rf_run(d, showBenefitRange = TRUE, showTable = FALSE, confidenceIntervals = TRUE,
                   bootReps = 100)
  expect_match(rf_notes(res_ci$benefitRangeTable), "bootstrap interval on the decision curve plot",
               fixed = TRUE)
})

test_that("DC-21-02 an exact 1:3 split also discloses case-control sampling; 1:1 still does", {
  set.seed(5)
  mk <- function(ne, nn) data.frame(y = factor(rep(c("event", "none"), c(ne, nn)), c("none", "event")),
                                    r = c(runif(ne, 0.2, 0.9), runif(nn, 0.05, 0.6)))
  has <- function(res) grepl("Outcome Split Matches a Case-Control Ratio", res$notices$content, fixed = TRUE)
  r13 <- rf_run(mk(50, 150))
  expect_true(has(r13))
  expect_match(r13$notices$content, "exact 1:3 ratio", fixed = TRUE)
  expect_match(r13$notices$content, "25.0% prevalence", fixed = TRUE)
  # More events than non-events: the ratio is printed events:non-events (3:1), not 1:3.
  r31 <- rf_run(mk(60, 20))
  expect_match(r31$notices$content, "exact 3:1 ratio", fixed = TRUE)
  expect_true(has(rf_run(mk(60, 60))))
  expect_false(has(rf_run(mk(50, 151))))      # not an exact ratio
  expect_false(has(rf_run(mk(30, 150))))      # 1:5 is outside the 1:1-1:4 band
  expect_false(has(rf_run(mk(19, 57))))       # fewer than 20 in the smaller group
})

test_that("DC-21-03 messages name the controls by their current labels", {
  set.seed(101)
  n <- 400
  p <- runif(n, 0, 0.6)
  d <- data.frame(y = factor(ifelse(rbinom(n, 1, p) == 1, "event", "none"), c("none", "event")),
                  r = p)
  inv <- rf_run(d, outcomePositive = "none")
  expect_match(inv$notices$content, "Possible Inversion", fixed = TRUE)
  expect_false(grepl("Positive Outcome Level", inv$notices$content, fixed = TRUE))
  d$rule <- factor(sample(c("a", "b", "c"), n, replace = TRUE))
  err <- tryCatch(rf_run(d, clinicalDecisionRule = TRUE, decisionRuleVar = "rule",
                         decisionRulePositive = "a"), error = function(e) conditionMessage(e))
  expect_match(err, "Rule Not Binary", fixed = TRUE)
  expect_false(grepl("Clinical Decision Rule Integration", err, fixed = TRUE))
  expect_match(err, "Clinical decision rule", fixed = TRUE)
})
