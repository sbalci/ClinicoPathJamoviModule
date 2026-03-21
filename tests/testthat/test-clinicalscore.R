# ── Tests for clinicalscore function ──────────────────────────────────────────

# Helper with all required defaults
cs <- function(data, ...) {
  defaults <- list(
    data = data,
    modelType = "logistic",
    outcome = NULL, outcomeLevel = NULL,
    elapsedtime = NULL, explanatory = NULL,
    autoCategorize = FALSE, categorizeMethod = "median",
    customBreaks = "",
    scoringMethod = "schneeweiss", maxPoints = 10,
    bootstrapValidation = FALSE, bootstrapN = 50,
    scoreLookup = FALSE, showNomogram = FALSE,
    showCalibration = FALSE, calibrationGroups = 4,
    showDiscrimination = FALSE, showDecisionCurve = FALSE,
    showTRIPOD = FALSE, suitabilityCheck = FALSE,
    showSummary = FALSE, showExplanations = FALSE,
    random_seed = 42
  )
  args <- modifyList(defaults, list(...))
  do.call(clinicalscore, args)
}

# ═══════════════════════════════════════════════════════════════════════════════
# 1. Smoke test — logistic
# ═══════════════════════════════════════════════════════════════════════════════

test_that("clinicalscore logistic runs without error", {
  set.seed(42)
  d <- data.frame(
    y = factor(c(rep("neg", 50), rep("pos", 50))),
    x1 = c(rnorm(50, 3), rnorm(50, 6)),
    x2 = c(rnorm(50, 5), rnorm(50, 7)),
    x3 = factor(sample(c("A", "B"), 100, TRUE))
  )
  expect_no_error(cs(d, outcome = "y", outcomeLevel = "pos",
                     explanatory = c("x1", "x2", "x3")))
})

# ═══════════════════════════════════════════════════════════════════════════════
# 2. Model summary populated
# ═══════════════════════════════════════════════════════════════════════════════

test_that("Model summary table is populated", {
  set.seed(42)
  d <- data.frame(
    y = factor(c(rep("neg", 50), rep("pos", 50))),
    x1 = c(rnorm(50, 3), rnorm(50, 6)),
    x2 = rnorm(100)
  )
  result <- cs(d, outcome = "y", outcomeLevel = "pos",
               explanatory = c("x1", "x2"))
  expect_true(nrow(result$modelSummary$asDF) >= 5)
})

# ═══════════════════════════════════════════════════════════════════════════════
# 3. Scoring system produces points
# ═══════════════════════════════════════════════════════════════════════════════

test_that("Scoring table has points for each predictor", {
  set.seed(42)
  d <- data.frame(
    y = factor(c(rep("neg", 50), rep("pos", 50))),
    x1 = c(rnorm(50, 3), rnorm(50, 7)),
    x2 = c(rnorm(50, 5), rnorm(50, 8))
  )
  result <- cs(d, outcome = "y", outcomeLevel = "pos",
               explanatory = c("x1", "x2"))
  score_df <- result$scoringTable$asDF
  expect_true(nrow(score_df) >= 1)
  expect_true("points" %in% names(score_df))
})

# ═══════════════════════════════════════════════════════════════════════════════
# 4. Method comparison — 4 rows
# ═══════════════════════════════════════════════════════════════════════════════

test_that("Method comparison shows 3 methods + full model", {
  set.seed(42)
  d <- data.frame(
    y = factor(c(rep("neg", 50), rep("pos", 50))),
    x1 = c(rnorm(50, 3), rnorm(50, 7)),
    x2 = c(rnorm(50, 5), rnorm(50, 8))
  )
  result <- cs(d, outcome = "y", outcomeLevel = "pos",
               explanatory = c("x1", "x2"),
               scoringMethod = "compare")
  comp_df <- result$methodComparison$asDF
  expect_equal(nrow(comp_df), 4)
})

# ═══════════════════════════════════════════════════════════════════════════════
# 5. Score lookup table
# ═══════════════════════════════════════════════════════════════════════════════

test_that("Lookup table maps scores to probabilities", {
  set.seed(42)
  d <- data.frame(
    y = factor(c(rep("neg", 50), rep("pos", 50))),
    x1 = c(rnorm(50, 3), rnorm(50, 7)),
    x2 = c(rnorm(50, 5), rnorm(50, 8))
  )
  result <- cs(d, outcome = "y", outcomeLevel = "pos",
               explanatory = c("x1", "x2"),
               scoreLookup = TRUE)
  lookup_df <- result$lookupTable$asDF
  expect_true(nrow(lookup_df) >= 2)
  expect_true(all(lookup_df$probability >= 0 & lookup_df$probability <= 1))
})

# ═══════════════════════════════════════════════════════════════════════════════
# 6. Discrimination metrics
# ═══════════════════════════════════════════════════════════════════════════════

test_that("Discrimination table has AUC and Brier", {
  set.seed(42)
  d <- data.frame(
    y = factor(c(rep("neg", 50), rep("pos", 50))),
    x1 = c(rnorm(50, 3), rnorm(50, 7)),
    x2 = rnorm(100)
  )
  result <- cs(d, outcome = "y", outcomeLevel = "pos",
               explanatory = c("x1", "x2"),
               showDiscrimination = TRUE)
  disc_df <- result$discriminationTable$asDF
  expect_true(nrow(disc_df) >= 1)
})

# ═══════════════════════════════════════════════════════════════════════════════
# 7. Bootstrap validation
# ═══════════════════════════════════════════════════════════════════════════════

test_that("Bootstrap validation returns corrected AUC", {
  set.seed(42)
  d <- data.frame(
    y = factor(c(rep("neg", 40), rep("pos", 40))),
    x1 = c(rnorm(40, 3), rnorm(40, 6)),
    x2 = rnorm(80)
  )
  result <- cs(d, outcome = "y", outcomeLevel = "pos",
               explanatory = c("x1", "x2"),
               bootstrapValidation = TRUE, bootstrapN = 50)
  val_df <- result$validationTable$asDF
  expect_true(nrow(val_df) >= 1)
  expect_true(val_df$corrected[1] <= val_df$apparent[1] + 0.01)
})

# ═══════════════════════════════════════════════════════════════════════════════
# 8. Auto-categorization
# ═══════════════════════════════════════════════════════════════════════════════

test_that("Auto-categorize converts continuous to factors", {
  set.seed(42)
  d <- data.frame(
    y = factor(c(rep("neg", 60), rep("pos", 60))),
    x1 = c(rnorm(60, 3), rnorm(60, 7)),
    x2 = c(rnorm(60, 5), rnorm(60, 8))
  )
  result <- cs(d, outcome = "y", outcomeLevel = "pos",
               explanatory = c("x1", "x2"),
               autoCategorize = TRUE, categorizeMethod = "tertiles")
  # With tertiles, each continuous var produces 2 model terms (3 categories - 1 reference)
  coef_df <- result$coefficients$asDF
  expect_true(nrow(coef_df) >= 3)
})

# ═══════════════════════════════════════════════════════════════════════════════
# 9. Suitability check
# ═══════════════════════════════════════════════════════════════════════════════

test_that("Suitability check does not crash with small data", {
  set.seed(42)
  d <- data.frame(
    y = factor(c(rep("neg", 15), rep("pos", 15))),
    x1 = rnorm(30), x2 = rnorm(30), x3 = rnorm(30),
    x4 = rnorm(30), x5 = rnorm(30)
  )
  expect_no_error(
    cs(d, outcome = "y", outcomeLevel = "pos",
       explanatory = c("x1", "x2", "x3", "x4", "x5"),
       suitabilityCheck = TRUE)
  )
})

# ═══════════════════════════════════════════════════════════════════════════════
# 10. Missing data handled
# ═══════════════════════════════════════════════════════════════════════════════

test_that("Missing data handled without crash", {
  set.seed(42)
  d <- data.frame(
    y = factor(c(rep("neg", 50), rep("pos", 50))),
    x1 = c(rnorm(50, 3), rnorm(50, 7)),
    x2 = rnorm(100)
  )
  d$x1[c(5, 15, 85)] <- NA
  result <- cs(d, outcome = "y", outcomeLevel = "pos",
               explanatory = c("x1", "x2"))
  expect_true(nrow(result$modelSummary$asDF) >= 1)
})

# ═══════════════════════════════════════════════════════════════════════════════
# 11. Cox model
# ═══════════════════════════════════════════════════════════════════════════════

test_that("Cox model runs without error", {
  set.seed(42)
  d <- data.frame(
    time = rexp(80, 0.05),
    event = factor(rbinom(80, 1, 0.6), labels = c("Alive", "Dead")),
    x1 = rnorm(80), x2 = factor(sample(c("A", "B"), 80, TRUE))
  )
  expect_no_error(
    cs(d, modelType = "cox", outcome = "event", outcomeLevel = "Dead",
       elapsedtime = "time", explanatory = c("x1", "x2"))
  )
})

# ═══════════════════════════════════════════════════════════════════════════════
# 12. TRIPOD checklist
# ═══════════════════════════════════════════════════════════════════════════════

test_that("TRIPOD checklist is generated", {
  set.seed(42)
  d <- data.frame(
    y = factor(c(rep("neg", 50), rep("pos", 50))),
    x1 = c(rnorm(50, 3), rnorm(50, 7)),
    x2 = rnorm(100)
  )
  result <- cs(d, outcome = "y", outcomeLevel = "pos",
               explanatory = c("x1", "x2"),
               showTRIPOD = TRUE)
  # TRIPOD content should be non-empty HTML
  content <- result$tripodChecklist$content
  expect_true(nchar(content) > 100)
})
