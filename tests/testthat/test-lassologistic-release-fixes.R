# Regression tests for the 2026-10-21 independent review (development-ideas/
# lassologistic-independent-review-2026-10-21.md), findings LR-01 to LR-04.

rf_run <- function(data, ...) {
  skip_if_not_installed("glmnet")
  suppressWarnings(do.call(ClinicoPath::lassologistic, c(list(data = data), list(...))))
}
rf_note <- function(tbl, key) tbl$.__enclos_env__$private$.notes[[key]]$note
rf_row <- function(tbl, key) { df <- tbl$asDF; df[gsub('^"|"$', "", rownames(df)) == key, ] }

# Audit fixture D01: five noise predictors, nothing retained under the 1SE rule.
rf_null <- function() {
  set.seed(2); n0 <- 100
  dn <- as.data.frame(matrix(rnorm(n0 * 5), n0, dimnames = list(NULL, paste0("n", 1:5))))
  dn$y <- rbinom(n0, 1, 0.4)
  dn
}

# Informative synthetic fixture: x1 and x2 carry signal, x3 and x4 are noise.
rf_signal <- function() {
  set.seed(11); n <- 200
  X <- matrix(rnorm(n * 4), n, dimnames = list(NULL, paste0("x", 1:4)))
  d <- data.frame(X)
  d$dx <- factor(ifelse(rbinom(n, 1, plogis(-0.5 + 1.2 * X[, 1] - 0.9 * X[, 2])) == 1, "ev", "no"))
  d
}

test_that("LR-01 the corrected AUC is always described as internal validation that can still be optimistic", {
  r <- rf_run(rf_signal(), outcome = "dx", outcomeLevel = "ev", explanatory = paste0("x", 1:4),
              bootstrapValidation = TRUE, bootstrapN = 50)
  expect_gt(rf_row(r$validationTable, "auc")$corrected, 0.7)      # outside the old < 0.7 caveat
  note <- rf_note(r$validationTable, "corrected_meaning")
  expect_match(note, "optimism-corrected internal validation", fixed = TRUE)
  expect_match(note, "can still be too high", fixed = TRUE)
  ow <- rf_note(r$validationTable, "optimism_warning")
  if (!is.null(ow)) expect_false(grepl("realistic", ow, fixed = TRUE))
})

test_that("LR-02 with no term retained the notices explain the empty Coefficient Plot", {
  r <- rf_run(rf_null(), outcome = "y", outcomeLevel = NULL, explanatory = paste0("n", 1:5), coef_plot = TRUE)
  expect_match(r$notices$content, "No Variables Selected", fixed = TRUE)
  expect_match(r$notices$content, "The Coefficient Plot is empty because no term was retained.", fixed = TRUE)
  off <- rf_run(rf_null(), outcome = "y", outcomeLevel = NULL, explanatory = paste0("n", 1:5), coef_plot = FALSE)
  expect_false(grepl("Coefficient Plot is empty", off$notices$content, fixed = TRUE))
})

test_that("LR-03 the 0.500 fallback threshold is not labelled Youden index", {
  r <- rf_run(rf_null(), outcome = "y", outcomeLevel = NULL, explanatory = paste0("n", 1:5))
  th <- rf_row(r$performance, "threshold")
  expect_equal(th$value, "0.500")
  expect_equal(th$interpretation, "Default (no discrimination)")
  ok <- rf_run(rf_signal(), outcome = "dx", outcomeLevel = "ev", explanatory = paste0("x", 1:4))
  expect_equal(rf_row(ok$performance, "threshold")$interpretation, "Youden index")
})

test_that("LR-04 the scoring-method reference row names the fitted penalty", {
  args <- list(outcome = "dx", outcomeLevel = "ev", explanatory = paste0("x", 1:4),
               scoringSystem = TRUE, scoringMethod = "compare", nfolds = 5)
  ridge <- do.call(rf_run, c(list(rf_signal(), penalty = "ridge"), args))
  expect_equal(rf_row(ridge$methodComparison, "full")$method, "Full Ridge (L2) model (continuous)")
  lasso <- do.call(rf_run, c(list(rf_signal(), penalty = "lasso"), args))
  expect_equal(rf_row(lasso$methodComparison, "full")$method, "Full LASSO (L1) model (continuous)")
})
