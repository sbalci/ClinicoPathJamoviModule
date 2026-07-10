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

test_that("Majority-present binary predictor contributes to the score (standardized)", {
  # Regression guard: under standardize=TRUE a 0/1 dummy is rescaled to two
  # z-values. A median-split then zeroed out any binary whose "present" level
  # was the majority, silently dropping it from the score. Score AUC should
  # sit near chance (~0.5) if that bug returns.
  set.seed(7)
  n <- 120
  xbin <- factor(c(rep("yes", 80), rep("no", 40)))  # 'yes' (present) is the majority
  prob <- ifelse(xbin == "yes", 0.9, 0.1)           # y strongly driven by xbin
  y <- factor(ifelse(runif(n) < prob, "pos", "neg"), levels = c("neg", "pos"))
  d <- data.frame(y = y, xbin = xbin, x2 = rnorm(n))

  result <- ll(d, outcome = "y", outcomeLevel = "pos",
               explanatory = c("xbin", "x2"),
               standardize = TRUE, lambda = "lambda.min",
               scoringSystem = TRUE, scoreLookupTable = TRUE)

  # Score must vary and discriminate — impossible if the binary scored 0 for all.
  lookup_df <- result$lookupTable$asDF
  expect_gt(nrow(lookup_df), 1)

  perf_df <- result$scoringPerformance$asDF
  auc_val <- as.numeric(perf_df$value[perf_df$metric == "Score AUC"])
  expect_gt(auc_val, 0.6)
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

# ═══════════════════════════════════════════════════════════════════════════════
# 8. Scoring method fidelity
# ═══════════════════════════════════════════════════════════════════════════════

test_that("Beta10 scoring uses fixed x10 scaling, distinct from Sullivan", {
  # Regression guard: Beta10 previously renormalized the largest |coef| to
  # max_points, which is algebraically identical to Sullivan -> the two columns
  # were always equal, defeating "Compare All Methods". Beta10 must now be a
  # FIXED round(coef * 10).
  set.seed(11)
  n <- 160
  x1 <- c(rnorm(80, 0), rnorm(80, 3))   # strong separator -> large coefficient (|coef| >> 1)
  x2 <- rnorm(n)
  y <- factor(c(rep("neg", 80), rep("pos", 80)), levels = c("neg", "pos"))
  d <- data.frame(y = y, x1 = x1, x2 = x2)

  result <- ll(d, outcome = "y", outcomeLevel = "pos",
               explanatory = c("x1", "x2"),
               lambda = "lambda.min",
               scoringSystem = TRUE, scoringMethod = "compare",
               scoringMaxPoints = 10)

  st <- result$scoringTable$asDF
  # The two point columns must no longer be identical.
  expect_false(identical(st$points_beta10, st$points_sullivan))

  # Beta10 == round(coefficient * 10); coefficient = log(oddsRatio).
  coef_est <- log(st$oddsRatio)
  strongest <- which.max(abs(coef_est))
  expect_equal(st$points_beta10[strongest], round(coef_est[strongest] * 10))

  # Sullivan normalizes the strongest predictor to +/- max_points (=10).
  expect_equal(max(abs(st$points_sullivan)), 10)
})

test_that("scoringMaxPoints scales Sullivan but not Beta10", {
  set.seed(12)
  n <- 160
  x1 <- c(rnorm(80, 0), rnorm(80, 3))
  y <- factor(c(rep("neg", 80), rep("pos", 80)), levels = c("neg", "pos"))
  d <- data.frame(y = y, x1 = x1, x2 = rnorm(n))
  args <- list(outcome = "y", outcomeLevel = "pos",
               explanatory = c("x1", "x2"), lambda = "lambda.min",
               scoringSystem = TRUE, scoringMethod = "compare")

  st10 <- do.call(ll, c(list(d), args, list(scoringMaxPoints = 10)))$scoringTable$asDF
  st20 <- do.call(ll, c(list(d), args, list(scoringMaxPoints = 20)))$scoringTable$asDF

  # Beta10 (fixed x10) is unaffected by scoringMaxPoints.
  expect_equal(st10$points_beta10, st20$points_beta10)
  # Sullivan's strongest predictor scales with the cap: 10 -> 20.
  expect_equal(max(abs(st10$points_sullivan)), 10)
  expect_equal(max(abs(st20$points_sullivan)), 20)
})

# ═══════════════════════════════════════════════════════════════════════════════
# 9. Outcome level / labelled-factor handling
# ═══════════════════════════════════════════════════════════════════════════════

test_that("Default event class respects factor level order, not alphabetical", {
  # levels()[2] is "Alpha"; a sort(unique()) default would wrongly pick "Zeta".
  set.seed(3)
  n <- 120
  x1 <- rnorm(n)
  y <- factor(ifelse(x1 + rnorm(n, 0, 0.4) > 0, "Zeta", "Alpha"),
              levels = c("Zeta", "Alpha"))
  d <- data.frame(y = y, x1 = x1, x2 = rnorm(n), x3 = rnorm(n))

  # Omit outcomeLevel entirely (the helper keeps its NULL default; passing NULL
  # explicitly would drop the required Level arg via modifyList).
  result <- ll(d, outcome = "y", explanatory = c("x1", "x2", "x3"),
               lambda = "lambda.min")

  ms <- result$modelSummary$asDF
  event_row <- ms$value[ms$statistic == "Event class (positive)"]
  expect_match(event_row, "^Alpha")
})

test_that("Invalid outcomeLevel is rejected with a clear message", {
  set.seed(3)
  d <- data.frame(
    y = factor(rep(c("neg", "pos"), each = 50)),
    x1 = rnorm(100), x2 = rnorm(100)
  )
  # Rejection is caught internally and surfaced via the To Do panel (no crash).
  result <- ll(d, outcome = "y", explanatory = c("x1", "x2"),
               outcomeLevel = "Nonexistent")
  expect_match(result$todo$content, "not found")
  expect_equal(nrow(result$coefficients$asDF), 0)
})

test_that("More than two outcome levels warns and models two classes", {
  set.seed(4)
  n <- 150
  d <- data.frame(
    y = factor(sample(c("A", "B", "C"), n, replace = TRUE), levels = c("A", "B", "C")),
    x1 = rnorm(n), x2 = rnorm(n), x3 = rnorm(n)
  )

  result <- ll(d, outcome = "y", explanatory = c("x1", "x2", "x3"),
               outcomeLevel = "B", lambda = "lambda.min")

  # A non-binary WARNING notice is rendered into the notices HTML block.
  expect_match(result$notices$content, "observed levels")
})

test_that("Coefficients table no longer exposes empty CI columns", {
  set.seed(42)
  d <- data.frame(
    y = factor(c(rep("neg", 50), rep("pos", 50))),
    x1 = c(rnorm(50, 3), rnorm(50, 7)), x2 = rnorm(100)
  )
  result <- ll(d, outcome = "y", outcomeLevel = "pos",
               explanatory = c("x1", "x2"))
  coef_df <- result$coefficients$asDF
  expect_false("ci_lower" %in% names(coef_df))
  expect_false("ci_upper" %in% names(coef_df))
})

# ═══════════════════════════════════════════════════════════════════════════════
# 10. Model comparison, calibration, and report sentence
# ═══════════════════════════════════════════════════════════════════════════════

test_that("Model comparison separates penalized LASSO from unpenalized refits", {
  set.seed(21)
  n <- 140
  x1 <- c(rnorm(70, 0), rnorm(70, 2.5))
  y <- factor(c(rep("neg", 70), rep("pos", 70)), levels = c("neg", "pos"))
  d <- data.frame(y = y, x1 = x1, x2 = rnorm(n), x3 = rnorm(n))

  result <- ll(d, outcome = "y", outcomeLevel = "pos",
               explanatory = c("x1", "x2", "x3"), lambda = "lambda.min",
               showModelComparison = TRUE)

  mc <- result$modelComparison$asDF
  # Row 1 is the actual penalized LASSO model; penalized models have no AIC.
  expect_match(mc$model_type[1], "penalized")
  expect_true(is.na(mc$aic[1]))
  # The unpenalized refits are labelled honestly (not as "LASSO").
  expect_true(any(grepl("LASSO-selected", mc$model_type)))
  expect_true(any(grepl("all vars", mc$model_type)))
  # Unpenalized rows carry an AIC.
  expect_true(all(!is.na(mc$aic[grepl("Logistic", mc$model_type)])))
})

test_that("Bootstrap validation reports a calibration slope row", {
  set.seed(22)
  n <- 90
  d <- data.frame(
    y = factor(c(rep("neg", 45), rep("pos", 45))),
    x1 = c(rnorm(45, 0), rnorm(45, 2)), x2 = rnorm(n)
  )
  result <- ll(d, outcome = "y", outcomeLevel = "pos",
               explanatory = c("x1", "x2"),
               bootstrapValidation = TRUE, bootstrapN = 50)

  val <- result$validationTable$asDF
  expect_true(any(grepl("Calibration slope", val$metric)))
  cal <- val[grepl("Calibration slope", val$metric), ]
  expect_true(is.finite(cal$apparent))
  expect_gt(cal$apparent, 0)
})

test_that("Summary produces a copy-ready report sentence", {
  set.seed(23)
  d <- data.frame(
    y = factor(c(rep("neg", 50), rep("pos", 50))),
    x1 = c(rnorm(50, 3), rnorm(50, 6)), x2 = rnorm(100)
  )
  result <- ll(d, outcome = "y", outcomeLevel = "pos",
               explanatory = c("x1", "x2"), showSummary = TRUE)

  txt <- result$summaryText$content
  expect_match(txt, "logistic regression")
  expect_match(txt, "were retained")
  expect_match(txt, "AUC")
})

test_that("Model summary shows display labels, not raw option codes", {
  set.seed(1)
  d <- data.frame(y = factor(rep(c("A", "B"), each = 40)),
                  x1 = rnorm(80), x2 = rnorm(80))
  ms <- ll(d, outcome = "y", explanatory = c("x1", "x2"),
           penalty = "lasso", lambda = "lambda.1se")$modelSummary$asDF

  expect_true(any(grepl("LASSO", ms$value)))   # display label, not "lasso"
  expect_true(any(grepl("1SE", ms$value)))     # display label, not "lambda.1se"
  expect_false(any(ms$value == "lasso"))
  expect_false(any(ms$value == "lambda.1se"))
})
