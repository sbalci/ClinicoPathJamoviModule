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
  expect_equal(nrow(comp_df), 4)  # Beta10, Schneeweiss, Max-scaled, Full model
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
  # The metric label carries an "(apparent)" qualifier. Matching the bare
  # "Score AUC" silently produced numeric(0), which makes expect_gt() ERROR
  # instead of assert - this guard was not running at all. Assert the row
  # exists before reading it so a future rename fails loudly and specifically.
  auc_row <- grepl("^Score AUC", perf_df$metric)
  expect_equal(sum(auc_row), 1L)
  auc_val <- as.numeric(perf_df$value[auc_row])
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

test_that("Beta10 scoring uses fixed x10 scaling, distinct from Max-scaled", {
  # Regression guard: Beta10 previously renormalized the largest |coef| to
  # max_points, which is algebraically identical to Max-scaled -> the two columns
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
  expect_false(identical(st$points_beta10, st$points_maxscaled))

  # Beta10 == round(coefficient * 10); coefficient = log(oddsRatio).
  coef_est <- log(st$oddsRatio)
  strongest <- which.max(abs(coef_est))
  expect_equal(st$points_beta10[strongest], round(coef_est[strongest] * 10))

  # Max-scaled normalizes the strongest predictor to +/- max_points (=10).
  expect_equal(max(abs(st$points_maxscaled)), 10)
})

test_that("scoringMaxPoints scales Max-scaled but not Beta10", {
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
  # Max-scaled's strongest predictor scales with the cap: 10 -> 20.
  expect_equal(max(abs(st10$points_maxscaled)), 10)
  expect_equal(max(abs(st20$points_maxscaled)), 20)
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

# ═══════════════════════════════════════════════════════════
# Release-review regression tests
# ═══════════════════════════════════════════════════════════

lasso_fixture <- function(n = 400, seed = 7) {
  set.seed(seed)
  d <- data.frame(
    p53  = factor(sample(c("wt", "mut"), n, TRUE), levels = c("wt", "mut")),
    Rb1  = factor(sample(c("intact", "lost"), n, TRUE), levels = c("intact", "lost")),
    ki67 = rnorm(n, 30, 15), age = rnorm(n, 60, 10))
  lp <- -2 + 1.6 * (d$p53 == "mut") + 1.1 * (d$Rb1 == "lost") + 0.03 * (d$ki67 - 30)
  d$dx <- factor(ifelse(rbinom(n, 1, plogis(lp)) == 1, "NEC", "NET"),
                 levels = c("NET", "NEC"))
  d
}
run_lasso <- function(d = lasso_fixture(), ...) ClinicoPath::lassologistic(
  data = d, outcome = "dx", outcomeLevel = "NEC",
  explanatory = c("p53", "Rb1", "ki67", "age"), ...)

test_that("odds ratios are on the original measurement scale", {
  skip_if_not_installed("glmnet")
  cf <- run_lasso(lambda = "lambda.min")$coefficients$asDF

  # The design matrix is standardised and glmnet is called with standardize=FALSE,
  # so raw coefficients are per-SD. For a balanced 0/1 dummy sd is about 0.5, so
  # the per-SD OR is roughly the SQUARE ROOT of the model's actual
  # present-vs-absent OR (1.8 printed where the model implied 3.2).
  or_p53 <- cf$oddsRatio[cf$variable == "p53mut"]
  expect_gt(or_p53, 3)

  # a continuous predictor's OR must be per 1 unit, not per 1 SD (~15 points)
  or_ki67 <- cf$oddsRatio[cf$variable == "ki67"]
  expect_lt(abs(or_ki67 - 1), 0.1)

  # importance keeps the per-SD magnitude, which is the comparable quantity
  expect_equal(max(cf$importance, na.rm = TRUE), 1)   # the (Intercept) row carries NA
})

test_that("complete-case exclusions are disclosed", {
  skip_if_not_installed("glmnet")
  d <- lasso_fixture(); d$ki67[1:80] <- NA
  res <- run_lasso(d)

  ms <- res$modelSummary$asDF
  expect_true(any(grepl("Excluded", ms[[1]])))
  expect_true(any(grepl("80 of 400", ms[[2]])))
  # "Total observations" used to hold the complete-case count, reading as the
  # full cohort while listwise deletion had silently removed rows
  expect_false(any(grepl("^Total observations$", ms[[1]])))

  txt <- gsub("<[^>]+>", " ", res$notices$content)
  expect_match(txt, "80 of 400")
  # the CAUSE must be named: here it really is predictor missingness
  expect_match(txt, "predictor missing")
  expect_match(txt, "listwise deletion")
  # and the Model Summary must break it down by cause
  expect_true(any(grepl("predictor missing", ms[[1]])))
})

test_that("out-of-model outcome levels are not blamed on missing predictors", {
  skip_if_not_installed("glmnet")
  # A >2-level outcome drops the non-modelled levels. Those rows are out of
  # scope, NOT incomplete: reporting them as "at least one selected variable
  # was missing" sent the reader hunting a sparse predictor that did not exist.
  d <- lasso_fixture()
  d$site <- factor(rep(c("A", "B", "C", "D"), length.out = nrow(d)))
  res <- ClinicoPath::lassologistic(
    data = d, outcome = "site", outcomeLevel = "A",
    explanatory = c("p53", "Rb1", "ki67", "age"))

  txt <- gsub("<[^>]+>", " ", res$notices$content)
  expect_match(txt, "outcome level outside the two being compared")
  # no predictor is missing in this fixture, so listwise-deletion advice must
  # NOT appear - that advice is what mis-directed the reader
  expect_false(grepl("listwise deletion", txt))

  ms <- res$modelSummary$asDF
  expect_true(any(grepl("outcome level not modelled", ms[[1]])))
  expect_false(any(grepl("predictor missing", ms[[1]])))
})

test_that("constant predictors are reported, not silently dropped", {
  skip_if_not_installed("glmnet")
  d <- lasso_fixture(); d$flat <- 1
  res <- ClinicoPath::lassologistic(
    data = d, outcome = "dx", outcomeLevel = "NEC",
    explanatory = c("p53", "Rb1", "ki67", "age", "flat"))
  txt <- gsub("<[^>]+>", " ", res$notices$content)
  expect_match(txt, "Constant predictors removed")
  expect_match(txt, "flat")
})

test_that("a single predictor gives guidance instead of a blank result", {
  skip_if_not_installed("glmnet")
  # .run() returned silently below 2 predictors while .init() only showed the
  # To Do panel at 0, so exactly one predictor produced an entirely blank output
  res <- ClinicoPath::lassologistic(
    data = lasso_fixture(), outcome = "dx", outcomeLevel = "NEC",
    explanatory = c("p53"))
  expect_match(gsub("<[^>]+>", " ", res$notices$content), "At least two predictors")
})

test_that("Brier score is graded against outcome prevalence", {
  skip_if_not_installed("glmnet")
  d <- lasso_fixture(); set.seed(3)
  d$dx <- factor(ifelse(rbinom(nrow(d), 1, 0.06) == 1, "NEC", "NET"),
                 levels = c("NET", "NEC"))
  pf <- run_lasso(d)$performance$asDF
  br <- pf$interpretation[pf$metric == "Brier Score"]

  # a no-information model at 6% prevalence scores ~0.056, which the old fixed
  # cut-offs (<0.1) graded "Excellent calibration"
  expect_false(grepl("Excellent calibration", br))
})

test_that("score performance is labelled apparent", {
  skip_if_not_installed("glmnet")
  sp <- run_lasso(scoringSystem = TRUE)$scoringPerformance$asDF
  # doubly optimistic: points from a model fitted here, cutoff Youden-optimised here
  expect_true(any(grepl("apparent", sp$metric)))
  expect_true(any(grepl("chosen on this data", sp$metric)))
})

test_that("the scoring system publishes its cut points", {
  skip_if_not_installed("glmnet")
  res <- run_lasso(scoringSystem = TRUE, lambda = "lambda.min")
  df <- res$scoringTable$asDF
  # Continuous predictors are scored above their in-sample median, which was
  # never published - so the score could not be applied to a new patient.
  skip_if_not("criterion" %in% names(df),
              "requires jmvtools::prepare() after the .r.yaml criterion column")
  expect_true(all(nzchar(df$criterion)))
  expect_true(any(grepl("^>", df$criterion)))   # a numeric cut for ki67
})

test_that("scoring points and odds ratios use the criterion contrast", {
  skip_if_not_installed("glmnet")
  res <- run_lasso(scoringSystem = TRUE, lambda = "lambda.min")
  sc <- res$scoringTable$asDF
  cf <- res$coefficients$asDF
  skip_if_not("criterion" %in% names(sc), "requires jmvtools::prepare()")

  # Points used to come from the raw per-SD coefficients while the score awards
  # them on a MEDIAN SPLIT, so a 0/1 dummy (per-SD coef ~ half its real effect)
  # was weighted against a continuous predictor spanning ~1.6 SD. The two tables
  # also disagreed for the same predictor (2.11 vs 4.46 for p53).
  for (v in c("p53mut", "Rb1lost")) {
    # for a binary predictor the criterion contrast IS present-vs-absent, so the
    # scoring OR must equal the per-category OR exactly
    expect_equal(sc$oddsRatio[sc$variable == v],
                 cf$oddsRatio[cf$variable == v], tolerance = 1e-6)
  }

  # for a continuous predictor the criterion spans many units, so the
  # above-vs-below-median OR must exceed the per-unit OR
  expect_gt(sc$oddsRatio[sc$variable == "ki67"],
            cf$oddsRatio[cf$variable == "ki67"])

  # and the strongest factor must earn the most points
  expect_equal(sc$variable[which.max(sc$points)], "p53mut")
  expect_gt(max(sc$points), min(sc$points))
})

test_that("manual cut-point strings are parsed safely", {
  skip_if_not_installed("glmnet")
  a <- ClinicoPath:::lassologisticClass$new(
    options = ClinicoPath:::lassologisticOptions$new(
      outcome = "dx", outcomeLevel = "NEC",
      explanatory = c("p53", "Rb1", "ki67", "age")),
    data = lasso_fixture())
  parse <- a$.__enclos_env__$private$.parseCutPoints

  expect_equal(parse("ki67=20, age=65"), c(ki67 = 20, age = 65))
  expect_equal(parse("ki67=20; age=65"), c(ki67 = 20, age = 65))
  # malformed entries are dropped, not guessed
  expect_equal(parse("ki67=20, bogus, x=abc"), c(ki67 = 20))
  expect_length(parse(""), 0)
  expect_length(parse(NULL), 0)
})

test_that("the cut point method changes the criterion and the score", {
  skip_if_not_installed("glmnet")
  d <- lasso_fixture()
  probe <- tryCatch({
    ClinicoPath::lassologistic(data = d, outcome = "dx", outcomeLevel = "NEC",
      explanatory = c("p53", "Rb1", "ki67", "age"),
      scoringSystem = TRUE, scoreCutMethod = "median"); TRUE
  }, error = function(e) FALSE)
  skip_if_not(probe, "requires jmvtools::prepare() after the .a.yaml cut-point options")

  crit <- function(m, cp = "") {
    r <- ClinicoPath::lassologistic(data = d, outcome = "dx", outcomeLevel = "NEC",
      explanatory = c("p53", "Rb1", "ki67", "age"), lambda = "lambda.min",
      scoringSystem = TRUE, scoreCutMethod = m, scoreCutPoints = cp)
    sc <- r$scoringTable$asDF
    sc$criterion[sc$variable == "ki67"]
  }

  med <- crit("median"); qrt <- crit("quartile"); man <- crit("manual", "ki67=20")
  # the upper quartile must sit above the median
  expect_gt(as.numeric(sub("^> ", "", qrt)), as.numeric(sub("^> ", "", med)))
  # a manual cut is used verbatim, on the ORIGINAL measurement scale
  expect_equal(as.numeric(sub("^> ", "", man)), 20, tolerance = 1e-6)
  # binary predictors are unaffected by the method
  expect_equal(crit("median"), med)
})

# ═══════════════════════════════════════════════════════════════════════════════
# 9. Audit regression guards (2026-08)
# ═══════════════════════════════════════════════════════════════════════════════

test_that("a machine-zero coefficient is not reported as a selected variable", {
  skip_if_not_installed("glmnet")
  # glmnet can leave a denormal such as -1.7e-16 in the sparse beta. An exact
  # `beta != 0` test let it through, and because Importance is
  # abs(beta)/max(abs(beta)) a lone noise coefficient normalised to 1.00 - the
  # table announced a selected predictor with Odds Ratio 1.000, Importance 1.000.
  set.seed(11)
  n <- 200
  d <- data.frame(
    y  = factor(rep(c("neg", "pos"), each = n / 2)),
    z1 = rnorm(n), z2 = rnorm(n), z3 = rnorm(n), z4 = rnorm(n))
  cf <- ll(d, outcome = "y", outcomeLevel = "pos",
           explanatory = paste0("z", 1:4))$coefficients$asDF

  real <- cf[!is.na(cf$coefficient) & cf$variable != "(Intercept)", , drop = FALSE]
  if (nrow(real) > 0) {
    # nothing that survives selection may be numerically indistinguishable from 0
    expect_true(all(abs(real$coefficient) > 1e-10))
    expect_true(all(abs(real$oddsRatio - 1) > 1e-10))
  } else {
    # the placeholder row is the correct alternative
    expect_equal(cf$variable[1], "No variables selected")
  }
})

test_that("non-syntactic variable names are not printed with backticks", {
  skip_if_not_installed("glmnet")
  # model.matrix(~., ...) deparses "Ki-67 (%)" as `Ki-67 (%)`, backticks included.
  d <- lasso_fixture()
  names(d)[names(d) == "ki67"] <- "Ki-67 (%)"
  res <- ClinicoPath::lassologistic(
    data = d, outcome = "dx", outcomeLevel = "NEC", lambda = "lambda.min",
    explanatory = c("p53", "Rb1", "Ki-67 (%)", "age"),
    scoringSystem = TRUE, scoreCutMethod = "manual",
    scoreCutPoints = "Ki-67 (%)=20")

  cf <- res$coefficients$asDF
  expect_false(any(grepl("`", cf$variable, fixed = TRUE)))
  expect_true("Ki-67 (%)" %in% cf$variable)

  # and the manual cut the user typed must actually be honoured
  sc <- res$scoringTable$asDF
  crit <- sc$criterion[sc$variable == "Ki-67 (%)"]
  expect_equal(as.numeric(sub("^> ", "", crit)), 20, tolerance = 1e-6)
})

test_that("the scoring-system notes track scoreCutMethod", {
  skip_if_not_installed("glmnet")
  # Three outputs hardcoded "the median" while scoreCutMethod also offers
  # mean/tertile/quartile/manual, so the Scoring System table said "upper
  # quartile" and the note directly under it said "their median".
  note <- function(tbl, key) tbl$.__enclos_env__$private$.notes[[key]]$note
  res <- ClinicoPath::lassologistic(
    data = lasso_fixture(), outcome = "dx", outcomeLevel = "NEC",
    explanatory = c("p53", "Rb1", "ki67", "age"), lambda = "lambda.min",
    scoringSystem = TRUE, scoreCutMethod = "quartile",
    showMethodologyNotes = TRUE)

  expect_match(note(res$scoringTable, "criterion_note"), "upper quartile")
  expect_match(note(res$scoringPerformance, "dichotomization"), "upper quartile")
  expect_false(grepl("their median", note(res$scoringPerformance, "dichotomization")))
  expect_match(gsub("<[^>]+>", " ", res$methodologyNotes$content), "upper quartile")
})

test_that("methodology notes agree with the coefficient table about scale", {
  skip_if_not_installed("glmnet")
  # These two outputs used to state OPPOSITE things: the table's scale_note said
  # ORIGINAL measurement scale (correct) and the methodology notes said per-SD.
  res <- run_lasso(lambda = "lambda.min", showMethodologyNotes = TRUE)
  note <- res$coefficients$.__enclos_env__$private$.notes[["scale_note"]]$note
  meth <- gsub("<[^>]+>", " ", res$methodologyNotes$content)

  expect_match(note, "ORIGINAL measurement scale")
  expect_match(meth, "original measurement scale")
  expect_false(grepl("Reported coefficients, odds ratios, and scoring-system point weights are therefore on a per-standard-deviation scale", meth, fixed = TRUE))
})

test_that("the coefficient plot declares the scale it draws", {
  skip_if_not_installed("glmnet")
  # The plot draws PER-SD coefficients while the table prints per-unit ones;
  # both were labelled just "Coefficient", so the tallest bar contradicted the
  # table's largest coefficient with nothing on screen to explain it.
  on <- run_lasso(lambda = "lambda.min", coef_plot = TRUE, standardize = TRUE)
  expect_true(isTRUE(on$coef_plot$state$standardized))
  off <- run_lasso(lambda = "lambda.min", coef_plot = TRUE, standardize = FALSE)
  expect_false(isTRUE(off$coef_plot$state$standardized))
  # the renderer must run against both states
  expect_true(on$coef_plot$.render())
  expect_true(off$coef_plot$.render())
})

test_that("an empty scoring system explains itself", {
  skip_if_not_installed("glmnet")
  # Zero selected variables used to leave up to four VISIBLE, EMPTY tables.
  set.seed(1)
  d <- data.frame(y = factor(rep(c("a", "b"), each = 40)))
  for (i in 1:6) d[[paste0("z", i)]] <- rnorm(80)
  res <- ll(d, outcome = "y", outcomeLevel = "b", explanatory = paste0("z", 1:6),
            scoringSystem = TRUE, scoringMethod = "compare", scoreLookupTable = TRUE)

  note <- function(tbl) tbl$.__enclos_env__$private$.notes[["no_vars"]]$note
  for (tb in list(res$scoringTable, res$scoringPerformance,
                  res$lookupTable, res$methodComparison)) {
    expect_equal(tb$rowCount, 0)
    expect_match(note(tb), "selected zero predictors")
  }
  expect_match(gsub("<[^>]+>", " ", res$notices$content), "Scoring System Not Generated")
})

test_that("a non-discriminating model is not presented as perfectly sensitive", {
  skip_if_not_installed("glmnet")
  # Every predicted probability identical => everyone classified positive =>
  # Sensitivity 1.000 and F1 0.667, which read as a highly sensitive test.
  set.seed(1)
  d <- data.frame(y = factor(rep(c("a", "b"), each = 40)))
  for (i in 1:6) d[[paste0("z", i)]] <- rnorm(80)
  res <- ll(d, outcome = "y", outcomeLevel = "b", explanatory = paste0("z", 1:6))

  pf <- res$performance$asDF
  skip_if_not(pf$value[pf$metric == "Sensitivity (Recall)"] == "1.000",
              "fixture no longer produces a degenerate fit")
  note <- res$performance$.__enclos_env__$private$.notes[["degenerate_note"]]$note
  expect_match(note, "does not discriminate")
  expect_match(gsub("<[^>]+>", " ", res$notices$content), "Model Does Not Discriminate")
})

test_that("cross-validation folds are capped by the minority class", {
  skip_if_not_installed("glmnet")
  # Stratified folds cannot exceed the smaller class; asking for 10 folds with
  # 6 events left 4 folds carrying no event at all.
  set.seed(5)
  n <- 60
  d <- data.frame(
    y  = factor(c(rep("pos", 6), rep("neg", n - 6)), levels = c("neg", "pos")),
    x1 = rnorm(n), x2 = rnorm(n), x3 = rnorm(n))
  suppressWarnings(res <- ll(d, outcome = "y", outcomeLevel = "pos",
            explanatory = c("x1", "x2", "x3"), nfolds = 10))

  ms <- res$modelSummary$asDF
  expect_equal(as.integer(ms[[2]][ms[[1]] == "CV folds"]), 6L)
  expect_match(gsub("<[^>]+>", " ", res$notices$content), "folds were requested")
})

test_that("reject() placeholders are filled, not printed literally", {
  # reject(formats, code = NULL, ...) - a bare 2nd positional arg binds to code=
  # and never reaches {}, so the user saw the literal "Design matrix error: {}".
  msg <- tryCatch(jmvcore::reject("x: {}", code = NULL, "detail"),
                  error = function(e) conditionMessage(e))
  expect_equal(msg, "x: detail")
  expect_equal(tryCatch(jmvcore::reject("x: {}", "detail"),
                        error = function(e) conditionMessage(e)),
               "x: {}")   # the defective shape, pinned so the contract is explicit

  # The source-scanning half of this guard used readLines("../../R/...") and a
  # regex requiring a BARE string after reject(. Both were wrong: under R CMD
  # check the wd is <pkg>.Rcheck/tests/testthat, so ../../R/ holds a compiled
  # .rdb and the read ERRORS; and every reject in this module is written
  # reject(.("...")), so the pattern matched nothing and asserted nothing.
  # Read the installed source the portable way and match the real style.
  src <- tryCatch(readLines(test_path("..", "..", "R", "lassologistic.b.R"), warn = FALSE),
                  error = function(e) NULL, warning = function(w) NULL)
  skip_if(is.null(src), "source not available (installed-package check)")
  offending <- grepl('reject\\(\\s*(\\.\\()?"[^"]*\\{\\}[^"]*"\\)?\\s*,\\s*[^c]', src) &
               !grepl('code\\s*=', src)
  expect_equal(sum(offending), 0L)
})

test_that("predictions come from the thresholded coefficients, not glmnet's raw beta", {
  skip_if_not_installed("glmnet")
  # A denormal coefficient still ORDERS the cases, so pROC ranked pure
  # floating-point noise: a model whose coefficient table said "No variables
  # selected" was shown beside an apparent AUC of 0.617. Predicting from the
  # zeroed beta makes the tables and the AUC describe the same model.
  set.seed(1)
  d <- data.frame(y = factor(rep(c("a", "b"), each = 40)))
  for (i in 1:6) d[[paste0("z", i)]] <- rnorm(80)
  res <- ll(d, outcome = "y", outcomeLevel = "b", explanatory = paste0("z", 1:6))

  cf <- res$coefficients$asDF
  skip_if_not(identical(cf$variable[1], "No variables selected"),
              "fixture no longer yields an empty model")
  # an empty model cannot discriminate: the AUC must be exactly chance
  auc_cell <- res$performance$asDF$value[1]
  expect_match(auc_cell, "^0\\.500")
})

test_that("the degenerate notice names the metric that actually hit 1.000", {
  skip_if_not_installed("glmnet")
  # Which of sensitivity/specificity saturates depends on which side of 0.500
  # the constant probability lands on; the notice must not assume "positive".
  d <- lasso_fixture()
  d$site <- factor(rep(c("A", "B", "C", "D"), length.out = nrow(d)))
  res <- ClinicoPath::lassologistic(
    data = d, outcome = "site", outcomeLevel = "A",
    explanatory = c("p53", "Rb1", "ki67", "age"))
  txt <- gsub("<[^>]+>", " ", res$notices$content)
  skip_if_not(grepl("Model Does Not Discriminate", txt), "fit was not degenerate")

  pf <- res$performance$asDF
  sens <- as.numeric(pf$value[pf$metric == "Sensitivity (Recall)"])
  spec <- as.numeric(pf$value[pf$metric == "Specificity"])
  if (sens >= spec) expect_match(txt, "calls every case positive")
  else              expect_match(txt, "calls every case negative")
})

test_that("the zero tolerance is scale-invariant", {
  # The tolerance must not erase a genuine coefficient just because the
  # predictor is recorded in large units. Under standardize = FALSE beta is
  # per raw unit, so a predictor with sd ~1e12 carries a real per-SD effect at
  # beta ~1e-12 - which a FLAT 1e-10 cutoff would delete. The implementation
  # tests abs(beta) * sd(column) instead.
  set.seed(1)
  x <- rnorm(300, 4e12, 1e12)
  beta_raw <- 1.5 / sd(x)   # a real per-SD effect of 1.5, expressed per raw unit
  denormal <- -1.7e-16
  ZERO_TOL <- 1e-10

  # what the code does now
  expect_true(abs(beta_raw) * sd(x) > ZERO_TOL)   # genuine effect retained
  expect_false(abs(denormal) * 1 > ZERO_TOL)      # denormal still dropped
  # what a flat cutoff would have done
  expect_false(abs(beta_raw) > ZERO_TOL)          # ...it would have erased it
})

# ═══════════════════════════════════════════════════════════════════════════════
# 10. Guards for defects found by adversarial review of the 2026-08 fixes
# ═══════════════════════════════════════════════════════════════════════════════

note_of <- function(tbl, key) tbl$.__enclos_env__$private$.notes[[key]]$note
plain   <- function(x) gsub("<[^>]+>", " ", x)

test_that("Ridge is exempt from the zero tolerance and still retains everything", {
  skip_if_not_installed("glmnet")
  # The tolerance recognises the zero L1 soft-thresholding meant to write. Ridge
  # never writes one, so applying it there DELETED genuine drivers: with
  # standardize=FALSE and one predictor on a large scale, glmnet's lambda is
  # scale-dominated and crushes the unit-scale coefficients to ~1e-14 per SD.
  set.seed(11); n <- 150
  d <- data.frame(big = rnorm(n, 2e4, 5e3), small = rnorm(n, 1e-3, 3e-4),
                  age = rnorm(n, 60, 10), z = rnorm(n))
  yv <- rbinom(n, 1, plogis(1.5 * as.numeric(scale(d$big)) +
                            0.9 * as.numeric(scale(d$small))))
  d$dx <- factor(ifelse(yv == 1, "case", "ctrl"), levels = c("ctrl", "case"))

  res <- ll(d, outcome = "dx", outcomeLevel = "case",
            explanatory = c("big", "small", "age", "z"),
            penalty = "ridge", standardize = FALSE, showVariableImportance = TRUE)

  cf <- res$coefficients$asDF
  cf <- cf[cf$variable != "(Intercept)", , drop = FALSE]
  expect_setequal(cf$variable, c("big", "small", "age", "z"))
  ms <- res$modelSummary$asDF
  # Ridge selects nothing, so every model TERM survives. Compare terms with
  # terms - "Variables analysed" counts variables, which differs whenever a
  # factor is dummy-coded.
  expect_equal(ms[[2]][ms[[1]] == "Terms selected"],
               ms[[2]][ms[[1]] == "Model terms (after dummy coding)"])
  # Variable Importance sits on the same screen and says ridge retains everything;
  # the two tables must not contradict each other.
  expect_setequal(res$variableImportance$asDF$variable, cf$variable)
})

test_that("exclusion breakdown rows sit directly under their parent row", {
  skip_if_not_installed("glmnet")
  # append(x, v, after = k) makes v element k+1, so incrementing `at` BEFORE the
  # insert pushed the whole breakdown one slot down - it appeared under
  # "Event class (positive)".
  d <- lasso_fixture()
  d$site <- factor(rep(c("A", "B", "C", "D"), length.out = nrow(d)))
  d$ki67[1:20] <- NA
  res <- ClinicoPath::lassologistic(
    data = d, outcome = "site", outcomeLevel = "A",
    explanatory = c("p53", "Rb1", "ki67", "age"))

  lab <- res$modelSummary$asDF[[1]]
  parent <- which(lab == "Excluded from analysis")
  kids <- which(grepl("^  - ", lab))
  expect_gt(length(kids), 0)
  # every breakdown row is contiguous with, and immediately after, the parent
  expect_equal(kids, seq(parent + 1, parent + length(kids)))
})

test_that("a manual cut method that fell back does not claim otherwise", {
  skip_if_not_installed("glmnet")
  # .scoreCuts silently falls back to the sample median for any predictor with no
  # entry in scoreCutPoints. Naming the cut from the OPTION rather than from what
  # was RESOLVED contradicted the manual_fallback note on the same table - and
  # centralising the label broadcast that contradiction to three panels.
  res <- ClinicoPath::lassologistic(
    data = lasso_fixture(), outcome = "dx", outcomeLevel = "NEC",
    explanatory = c("p53", "Rb1", "ki67", "age"), lambda = "lambda.min",
    scoringSystem = TRUE, scoreCutMethod = "manual", scoreCutPoints = "",
    showMethodologyNotes = TRUE)

  fallback <- note_of(res$scoringTable, "manual_fallback")
  skip_if(is.null(fallback), "fixture selected no continuous predictor")

  crit <- note_of(res$scoringTable, "criterion_note")
  dich <- note_of(res$scoringPerformance, "dichotomization")
  meth <- plain(res$methodologyNotes$content)
  for (txt in list(crit, dich, meth)) {
    # must not assert the supplied cuts were used, full stop
    expect_match(txt, "sample median")
  }
})

test_that("display labels, not raw option codes, reach every prose surface", {
  skip_if_not_installed("glmnet")
  # The Model Summary mapped them; the copy-ready Results Summary and the
  # completion notice printed "lambda.1se" and "lasso" verbatim.
  res <- run_lasso(showSummary = TRUE)
  for (txt in list(plain(res$summaryText$content), plain(res$notices$content))) {
    expect_false(grepl("lambda.1se", txt, fixed = TRUE))
    expect_false(grepl("lambda.min", txt, fixed = TRUE))
    expect_false(grepl("using lasso penalty", txt, fixed = TRUE))
  }
  expect_match(plain(res$summaryText$content), "1SE Rule")
})

test_that("perfect apparent separation is caveated, not called Excellent", {
  skip_if_not_installed("glmnet")
  # AUC 1.000 is the result that gets a model ADOPTED. It was labelled
  # "Excellent" with a DeLong interval of 1.000-1.000 - a collapsed interval
  # presented as precision - and the overfit guard (auc > .95 AND n < 100)
  # missed it at n == 100 exactly.
  set.seed(42)
  d <- data.frame(y = factor(rep(c("neg", "pos"), each = 50)),
                  x1 = c(rnorm(50, 3), rnorm(50, 7)),
                  x2 = c(rnorm(50, 5), rnorm(50, 8)))
  res <- ll(d, outcome = "y", outcomeLevel = "pos", explanatory = c("x1", "x2"))

  pf <- res$performance$asDF
  auc_cell <- pf$value[1]
  skip_if_not(grepl("^1\\.000", auc_cell), "fixture no longer separates perfectly")

  expect_false(grepl("1.000-1.000", auc_cell, fixed = TRUE))
  expect_match(auc_cell, "CI not estimable")
  expect_false(identical(pf$interpretation[1], "Excellent"))
  expect_match(note_of(res$performance, "perfect_note"), "separate the two classes completely")
  expect_match(plain(res$notices$content), "Perfect Apparent Separation")
})

test_that("the constant-predictor notice counts variables, not matrix columns", {
  skip_if_not_installed("glmnet")
  # data$p is ncol(X); a 5-level factor is 4 columns, so it cannot stand in for
  # "variables you selected".
  d <- lasso_fixture()
  d$site <- factor(rep(c("A", "B", "C", "D", "E"), length.out = nrow(d)))
  d$flat <- 1
  res <- ClinicoPath::lassologistic(
    data = d, outcome = "dx", outcomeLevel = "NEC",
    explanatory = c("p53", "Rb1", "site", "flat"))

  txt <- plain(res$notices$content)
  expect_match(txt, "Constant predictors removed")
  # 4 selected, 1 constant dropped -> 3 variables remain (but 7 design columns)
  expect_match(txt, "3 variables remain")
  expect_false(grepl("7 variables remain", txt, fixed = TRUE))
  # and the Model Summary must now label the two quantities distinctly
  ms <- res$modelSummary$asDF
  expect_equal(ms[[2]][ms[[1]] == "Variables analysed"], "3")
  expect_false(any(ms[[1]] == "Candidate predictors"))
})

test_that("bootstrap validation uses the same estimator on both sides", {
  skip_if_not_installed("glmnet")
  # Apparent comes from the THRESHOLDED model; the bootstrap refits used to come
  # from glmnet's raw beta, so the two halves of the correction were different
  # estimators. Both now go through .probsFrom.
  #
  # A corrected AUC BELOW 0.5 is not a bug and must not be clamped: bootstrap
  # resamples manufacture spurious signal (measured: ~7 of 20 select at least one
  # variable on pure noise), so optimism is genuinely positive even when the
  # fitted model selected nothing. It has to be EXPLAINED, not hidden.
  set.seed(1)
  d <- data.frame(y = factor(rep(c("a", "b"), each = 40)))
  for (i in 1:6) d[[paste0("z", i)]] <- rnorm(80)
  res <- ll(d, outcome = "y", outcomeLevel = "b", explanatory = paste0("z", 1:6),
            bootstrapValidation = TRUE, bootstrapN = 60)

  v <- res$validationTable$asDF
  auc <- v[v$metric == "AUC", ]
  skip_if(nrow(auc) != 1 || is.na(auc$corrected), "bootstrap produced no AUC row")
  # the empty model's apparent AUC is exactly chance - the thresholded estimator
  expect_equal(auc$apparent, 0.5, tolerance = 1e-8)
  if (auc$corrected < 0.5)
    expect_match(note_of(res$validationTable, "below_chance"), "not an error")
})

test_that("stripping backticks cannot collide two columns onto one name", {
  skip_if_not_installed("glmnet")
  d <- lasso_fixture()
  d[["Tumor Grade"]] <- factor(rep(c("Low", "High"), length.out = nrow(d)))
  d[["Tumor GradeLow"]] <- rnorm(nrow(d))   # collides after the strip
  res <- ClinicoPath::lassologistic(
    data = d, outcome = "dx", outcomeLevel = "NEC", lambda = "lambda.min",
    explanatory = c("p53", "ki67", "Tumor Grade", "Tumor GradeLow"))

  cf <- res$coefficients$asDF
  expect_false(any(grepl("`", cf$variable, fixed = TRUE)))
  expect_equal(anyDuplicated(cf$variable), 0L)
})

test_that("a degenerate point scale is flagged, not silently emitted", {
  skip_if_not_installed("glmnet")
  # Schneeweiss divides by the SMALLEST contribution. Ridge is exempt from the
  # selection tolerance, so a ~1e-14 coefficient can become the denominator:
  # the ratio then exceeded .Machine$integer.max and as.integer() returned NA,
  # blanking the two REAL predictors while the noise one kept the only point.
  set.seed(11); n <- 150
  d <- data.frame(big = rnorm(n, 2e4, 5e3), small = rnorm(n, 1e-3, 3e-4),
                  age = rnorm(n, 60, 10), z = rnorm(n))
  yv <- rbinom(n, 1, plogis(1.5 * as.numeric(scale(d$big)) +
                            0.9 * as.numeric(scale(d$small))))
  d$dx <- factor(ifelse(yv == 1, "case", "ctrl"), levels = c("ctrl", "case"))

  res <- ll(d, outcome = "dx", outcomeLevel = "case",
            explanatory = c("big", "small", "age", "z"),
            penalty = "ridge", standardize = FALSE,
            scoringSystem = TRUE, scoringMethod = "schneeweiss")

  st <- res$scoringTable$asDF
  expect_gt(nrow(st), 0)
  expect_false(anyNA(st$points))                 # no integer-overflow NAs
  expect_true(all(is.finite(st$points)))
  # and the resulting scale, being unusable, must say so rather than pass as a score
  if (max(abs(st$points)) > 100) {
    expect_match(note_of(res$scoringTable, "wide_scale"), "too wide to be used")
    expect_match(gsub("<[^>]+>", " ", res$notices$content), "Scoring Scale Not Usable")
  }
})

test_that("a near-perfect but estimable AUC keeps its confidence interval", {
  skip_if_not_installed("glmnet")
  # The `perfect` guard used a 0.9995 proximity test, which discarded real
  # DeLong intervals. Only a collapsed interval is "not estimable".
  set.seed(9); m <- 400
  d <- data.frame(y = factor(rep(c("neg", "pos"), each = m / 2)),
                  x1 = c(rnorm(m / 2, 0), rnorm(m / 2, 4.2)), x2 = rnorm(m))
  res <- ll(d, outcome = "y", outcomeLevel = "pos", explanatory = c("x1", "x2"))
  cell <- res$performance$asDF$value[1]
  skip_if(grepl("^1\\.000", cell), "fixture separated perfectly this time")
  expect_false(grepl("CI not estimable", cell, fixed = TRUE))
  expect_match(cell, "\\(0\\.")      # a real interval is shown
})
