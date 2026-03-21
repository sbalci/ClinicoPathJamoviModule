# ── Tests for misclassificationbias function ─────────────────────────────────

mb <- function(data, ...) {
  defaults <- list(
    data = data,
    outcome = NULL, outcomeLevel = NULL,
    exposure = NULL, exposureLevel = NULL,
    misclassType = "nondifferential",
    senExposure = 0.85, specExposure = 0.90,
    senExposureCase = 0.85, specExposureCase = 0.90,
    senExposureControl = 0.85, specExposureControl = 0.90,
    effectMeasure = "or",
    rangeAnalysis = FALSE, senRange = "0.70,0.80,0.90",
    specRange = "0.70,0.80,0.90",
    nSimulations = 1000, random_seed = 42
  )
  args <- modifyList(defaults, list(...))
  do.call(misclassificationbias, args)
}

# ═══════════════════════════════════════════════════════════════════════════════
# 1. Smoke test
# ═══════════════════════════════════════════════════════════════════════════════

test_that("misclassificationbias runs without error", {
  d <- data.frame(
    outcome = factor(c(rep("Alive", 60), rep("Dead", 40))),
    exposure = factor(c(rep("Low", 30), rep("High", 30), rep("Low", 20), rep("High", 20)))
  )
  expect_no_error(mb(d, outcome = "outcome", outcomeLevel = "Dead",
                     exposure = "exposure", exposureLevel = "High"))
})

# ═══════════════════════════════════════════════════════════════════════════════
# 2. Non-differential correction — bias toward null
# ═══════════════════════════════════════════════════════════════════════════════

test_that("Non-differential misclassification biases OR toward null", {
  # True OR = 6 (40*30)/(10*20)
  d <- data.frame(
    outcome = factor(c(rep("Case", 50), rep("Control", 50))),
    exposure = factor(c(rep("Exp", 40), rep("Unexp", 10),
                        rep("Exp", 20), rep("Unexp", 30)))
  )
  result <- mb(d, outcome = "outcome", outcomeLevel = "Case",
               exposure = "exposure", exposureLevel = "Exp",
               senExposure = 0.85, specExposure = 0.90)

  bias_df <- result$biasAnalysis$asDF
  observed_or <- bias_df$observed[1]
  adjusted_or <- bias_df$adjusted[1]
  # Corrected OR should be larger (further from null) than observed
  expect_true(adjusted_or > observed_or)
})

# ═══════════════════════════════════════════════════════════════════════════════
# 3. Perfect classifier — no correction needed
# ═══════════════════════════════════════════════════════════════════════════════

test_that("Perfect classifier (sen=1, spec=1) gives same OR", {
  d <- data.frame(
    outcome = factor(c(rep("Case", 50), rep("Control", 50))),
    exposure = factor(c(rep("Exp", 35), rep("Unexp", 15),
                        rep("Exp", 15), rep("Unexp", 35)))
  )
  result <- mb(d, outcome = "outcome", outcomeLevel = "Case",
               exposure = "exposure", exposureLevel = "Exp",
               senExposure = 1.0, specExposure = 1.0)

  bias_df <- result$biasAnalysis$asDF
  # With perfect classification, adjusted should equal observed
  expect_equal(bias_df$adjusted[1], bias_df$observed[1], tolerance = 0.1)
})

# ═══════════════════════════════════════════════════════════════════════════════
# 4. Risk Ratio and Risk Difference
# ═══════════════════════════════════════════════════════════════════════════════

test_that("Risk ratio effect measure works", {
  d <- data.frame(
    outcome = factor(c(rep("Case", 50), rep("Control", 50))),
    exposure = factor(c(rep("Exp", 35), rep("Unexp", 15),
                        rep("Exp", 15), rep("Unexp", 35)))
  )
  result <- mb(d, outcome = "outcome", outcomeLevel = "Case",
               exposure = "exposure", exposureLevel = "Exp",
               effectMeasure = "rr")
  expect_true(nrow(result$biasAnalysis$asDF) == 1)
})

test_that("Risk difference effect measure works", {
  d <- data.frame(
    outcome = factor(c(rep("Case", 50), rep("Control", 50))),
    exposure = factor(c(rep("Exp", 35), rep("Unexp", 15),
                        rep("Exp", 15), rep("Unexp", 35)))
  )
  result <- mb(d, outcome = "outcome", outcomeLevel = "Case",
               exposure = "exposure", exposureLevel = "Exp",
               effectMeasure = "rd")
  expect_true(nrow(result$biasAnalysis$asDF) == 1)
})

# ═══════════════════════════════════════════════════════════════════════════════
# 5. Differential misclassification
# ═══════════════════════════════════════════════════════════════════════════════

test_that("Differential misclassification runs", {
  d <- data.frame(
    outcome = factor(c(rep("Case", 50), rep("Control", 50))),
    exposure = factor(c(rep("Exp", 35), rep("Unexp", 15),
                        rep("Exp", 15), rep("Unexp", 35)))
  )
  expect_no_error(
    mb(d, outcome = "outcome", outcomeLevel = "Case",
       exposure = "exposure", exposureLevel = "Exp",
       misclassType = "differential",
       senExposureCase = 0.90, specExposureCase = 0.85,
       senExposureControl = 0.80, specExposureControl = 0.90)
  )
})

# ═══════════════════════════════════════════════════════════════════════════════
# 6. Range analysis
# ═══════════════════════════════════════════════════════════════════════════════

test_that("Range analysis produces grid of results", {
  d <- data.frame(
    outcome = factor(c(rep("Case", 50), rep("Control", 50))),
    exposure = factor(c(rep("Exp", 35), rep("Unexp", 15),
                        rep("Exp", 15), rep("Unexp", 35)))
  )
  result <- mb(d, outcome = "outcome", outcomeLevel = "Case",
               exposure = "exposure", exposureLevel = "Exp",
               rangeAnalysis = TRUE,
               senRange = "0.70,0.80,0.90", specRange = "0.70,0.80,0.90")
  range_df <- result$rangeTable$asDF
  # 3 sen x 3 spec = 9 combinations
  expect_true(nrow(range_df) >= 9)
})

# ═══════════════════════════════════════════════════════════════════════════════
# 7. Observed table structure
# ═══════════════════════════════════════════════════════════════════════════════

test_that("Observed table has 3 rows (cases, controls, total)", {
  d <- data.frame(
    outcome = factor(c(rep("Case", 50), rep("Control", 50))),
    exposure = factor(c(rep("Exp", 35), rep("Unexp", 15),
                        rep("Exp", 15), rep("Unexp", 35)))
  )
  result <- mb(d, outcome = "outcome", outcomeLevel = "Case",
               exposure = "exposure", exposureLevel = "Exp")
  expect_equal(nrow(result$observedTable$asDF), 3)
})

# ═══════════════════════════════════════════════════════════════════════════════
# 8. Edge case: missing data
# ═══════════════════════════════════════════════════════════════════════════════

test_that("Missing data handled without crash", {
  d <- data.frame(
    outcome = factor(c(rep("Case", 50), rep("Control", 50))),
    exposure = factor(c(rep("Exp", 35), rep("Unexp", 15),
                        rep("Exp", 15), rep("Unexp", 35)))
  )
  d$outcome[c(5, 15)] <- NA
  d$exposure[c(25, 75)] <- NA
  expect_no_error(
    mb(d, outcome = "outcome", outcomeLevel = "Case",
       exposure = "exposure", exposureLevel = "Exp")
  )
})
