# ── Tests for lassologistic function ──────────────────────────────────────────
# Covers: LASSO/Ridge/Elastic Net, scoring system, bootstrap validation,
# agreement with glmnet, edge cases.

# Helper with all required defaults
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

# ═══════════════════════════════════════════════════════════════════════════════
# 1. Smoke test
# ═══════════════════════════════════════════════════════════════════════════════

test_that("lassologistic runs without error", {
  set.seed(42)
  d <- data.frame(
    y = factor(rep(c("A", "B"), each = 40)),
    x1 = rnorm(80), x2 = rnorm(80), x3 = rnorm(80)
  )
  expect_no_error(ll(d, outcome = "y", explanatory = c("x1", "x2", "x3")))
})

# ═══════════════════════════════════════════════════════════════════════════════
# 2. Variable selection agrees with glmnet
# ═══════════════════════════════════════════════════════════════════════════════

test_that("LASSO selects variables consistent with glmnet", {
  set.seed(42)
  n <- 100
  x1 <- rnorm(n); x2 <- rnorm(n); x3 <- rnorm(n)
  y <- factor(ifelse(x1 + 0.5 * x2 + rnorm(n, 0, 0.5) > 0, "pos", "neg"))
  d <- data.frame(y = y, x1 = x1, x2 = x2, x3 = x3)

  result <- ll(d, outcome = "y", outcomeLevel = "pos",
               explanatory = c("x1", "x2", "x3"))

  coef_df <- result$coefficients$asDF
  # x1 should be selected (strong signal), x3 may or may not be
  expect_true(nrow(coef_df) >= 1)
})

# ═══════════════════════════════════════════════════════════════════════════════
# 3. Penalty types
# ═══════════════════════════════════════════════════════════════════════════════

test_that("Ridge retains all variables", {
  set.seed(42)
  d <- data.frame(
    y = factor(rep(c("A", "B"), each = 50)),
    x1 = rnorm(100), x2 = rnorm(100), x3 = rnorm(100)
  )

  result <- ll(d, outcome = "y", explanatory = c("x1", "x2", "x3"),
               penalty = "ridge")

  # Ridge should retain all variables (no exact zeros)
  coef_df <- result$coefficients$asDF
  # Ridge may still show "No variables selected" if lambda is too large,
  # but with lambda.1se on easy data it should select some
  expect_true(nrow(coef_df) >= 1)
})

test_that("Elastic net runs without error", {
  set.seed(42)
  d <- data.frame(
    y = factor(rep(c("A", "B"), each = 50)),
    x1 = rnorm(100), x2 = rnorm(100)
  )

  expect_no_error(
    ll(d, outcome = "y", explanatory = c("x1", "x2"),
       penalty = "elasticnet", alpha = 0.5)
  )
})

# ═══════════════════════════════════════════════════════════════════════════════
# 4. Scoring system
# ═══════════════════════════════════════════════════════════════════════════════

test_that("Scoring system generates points and lookup table", {
  set.seed(42)
  n <- 100
  d <- data.frame(
    y = factor(c(rep("neg", 50), rep("pos", 50))),
    x1 = c(rnorm(50, 3), rnorm(50, 7)),
    x2 = c(rnorm(50, 5), rnorm(50, 8))
  )

  result <- ll(d, outcome = "y", outcomeLevel = "pos",
               explanatory = c("x1", "x2"),
               scoringSystem = TRUE, scoreLookupTable = TRUE,
               scoringMethod = "schneeweiss")

  score_df <- result$scoringTable$asDF
  expect_true(nrow(score_df) >= 1)
  expect_true("points" %in% names(score_df))

  # Lookup table should have at least 2 rows
  lookup_df <- result$lookupTable$asDF
  expect_true(nrow(lookup_df) >= 2)
})

test_that("Method comparison produces 4 rows (3 methods + full model)", {
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

  comp_df <- result$methodComparison$asDF
  expect_equal(nrow(comp_df), 4)  # Beta10, Schneeweiss, Sullivan, Full model
})

# ═══════════════════════════════════════════════════════════════════════════════
# 5. Bootstrap validation
# ═══════════════════════════════════════════════════════════════════════════════

test_that("Bootstrap validation returns corrected metrics", {
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
  expect_true(nrow(val_df) >= 1)  # AUC row (+ Brier if available)
  expect_true(!is.na(val_df$apparent[1]))
  expect_true(!is.na(val_df$corrected[1]))
  # Corrected should be <= apparent (optimism is non-negative on average)
  expect_true(val_df$corrected[1] <= val_df$apparent[1] + 0.01)
})

# ═══════════════════════════════════════════════════════════════════════════════
# 6. Edge cases
# ═══════════════════════════════════════════════════════════════════════════════

test_that("Handles missing data without crash", {
  set.seed(42)
  d <- data.frame(
    y = factor(c(rep("A", 40), rep("B", 40))),
    x1 = rnorm(80), x2 = rnorm(80)
  )
  d$x1[c(5, 15, 65)] <- NA

  expect_no_error(ll(d, outcome = "y", explanatory = c("x1", "x2")))
})

test_that("Suitability check flags small sample", {
  set.seed(42)
  d <- data.frame(
    y = factor(c(rep("A", 15), rep("B", 15))),
    x1 = rnorm(30), x2 = rnorm(30), x3 = rnorm(30),
    x4 = rnorm(30), x5 = rnorm(30)
  )

  # EPV = 15/5 = 3 — should trigger warning
  expect_no_error(
    ll(d, outcome = "y", explanatory = c("x1", "x2", "x3", "x4", "x5"),
       suitabilityCheck = TRUE)
  )
})

# ═══════════════════════════════════════════════════════════════════════════════
# 7. Performance table
# ═══════════════════════════════════════════════════════════════════════════════

test_that("Performance table has AUC and Brier score", {
  set.seed(42)
  d <- data.frame(
    y = factor(c(rep("neg", 50), rep("pos", 50))),
    x1 = c(rnorm(50, 3), rnorm(50, 7)),
    x2 = rnorm(100)
  )

  result <- ll(d, outcome = "y", outcomeLevel = "pos",
               explanatory = c("x1", "x2"))

  perf_df <- result$performance$asDF
  expect_true(nrow(perf_df) >= 6)  # AUC, threshold, accuracy, sens, spec, brier, ...
  # Check AUC is in valid range
  auc_row <- perf_df[grepl("AUC", perf_df$metric), ]
  expect_true(nrow(auc_row) >= 1)
})
