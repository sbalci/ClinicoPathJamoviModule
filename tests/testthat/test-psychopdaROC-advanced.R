psychopdaROC <- function(...) {
  args <- list(...)
  f_args <- formals(ClinicoPath::psychopdaROC)
  for(arg in names(f_args)) {
    if(arg %in% c('...', 'data')) next
    if(!(arg %in% names(args))) {
      if(is.name(f_args[[arg]]) && as.character(f_args[[arg]]) == '') {
        args[[arg]] <- ""
      }
    }
  }
  do.call(ClinicoPath::psychopdaROC, args)
}

# ═══════════════════════════════════════════════════════════
# Advanced Feature Tests: psychopdaROC
# ═══════════════════════════════════════════════════════════
# Tests for: Effect Size, Power Analysis, Bayesian ROC,
# Clinical Utility, Meta-Analysis, Fixed Sens/Spec,
# Partial AUC, Bootstrap CIs, ROC Smoothing
# ═══════════════════════════════════════════════════════════

library(testthat)
library(jmvcore)

# Source required files
if (file.exists("../../R/utils.R")) source("../../R/utils.R")
if (file.exists("../../R/psychopdaROC_utilities.R")) source("../../R/psychopdaROC_utilities.R")

# ═══════════════════════════════════════════════════════════
# Helper: Create test data for advanced features
# ═══════════════════════════════════════════════════════════
create_advanced_test_data <- function(n = 250, seed = 42) {
  set.seed(seed)

  diagnosis <- factor(
    sample(c("Negative", "Positive"), n, replace = TRUE, prob = c(0.65, 0.35)),
    levels = c("Negative", "Positive")
  )

  data.frame(
    diagnosis = diagnosis,
    # 5 markers with varying discriminatory ability
    marker_excellent = rnorm(n, ifelse(diagnosis == "Positive", 90, 45), 12),
    marker_good      = rnorm(n, ifelse(diagnosis == "Positive", 80, 50), 15),
    marker_moderate  = rnorm(n, ifelse(diagnosis == "Positive", 70, 55), 18),
    marker_fair      = rnorm(n, ifelse(diagnosis == "Positive", 65, 55), 20),
    marker_poor      = rnorm(n, ifelse(diagnosis == "Positive", 58, 55), 22),
    site = factor(sample(c("Center_A", "Center_B", "Center_C"), n, replace = TRUE)),
    stringsAsFactors = FALSE
  )
}

# ═══════════════════════════════════════════════════════════
# EFFECT SIZE ANALYSIS
# ═══════════════════════════════════════════════════════════

test_that("effect size analysis works with two variables", {
  data <- create_advanced_test_data(n = 150)

  expect_no_error({
    result <- psychopdaROC(
      data = data,
      dependentVars = c("marker_excellent", "marker_good"),
      classVar = "diagnosis",
      positiveClass = "Positive",
      effectSizeAnalysis = TRUE
    )
  })
})

test_that("effect size analysis works with multiple variables", {
  data <- create_advanced_test_data(n = 200)

  expect_no_error({
    result <- psychopdaROC(
      data = data,
      dependentVars = c("marker_excellent", "marker_good", "marker_moderate"),
      classVar = "diagnosis",
      positiveClass = "Positive",
      effectSizeAnalysis = TRUE
    )
  })
})

# ═══════════════════════════════════════════════════════════
# POWER ANALYSIS
# ═══════════════════════════════════════════════════════════

test_that("post-hoc power analysis works", {
  data <- create_advanced_test_data(n = 150)

  expect_no_error({
    result <- psychopdaROC(
      data = data,
      dependentVars = "marker_excellent",
      classVar = "diagnosis",
      positiveClass = "Positive",
      powerAnalysis = TRUE,
      powerAnalysisType = "post_hoc"
    )
  })
})

test_that("prospective power analysis works", {
  data <- create_advanced_test_data(n = 100)

  expect_no_error({
    result <- psychopdaROC(
      data = data,
      dependentVars = "marker_excellent",
      classVar = "diagnosis",
      positiveClass = "Positive",
      powerAnalysis = TRUE,
      powerAnalysisType = "prospective",
      expectedAUCDifference = 0.1,
      targetPower = 0.8,
      significanceLevel = 0.05
    )
  })
})

test_that("sample size estimation works", {
  data <- create_advanced_test_data(n = 100)

  expect_no_error({
    result <- psychopdaROC(
      data = data,
      dependentVars = "marker_excellent",
      classVar = "diagnosis",
      positiveClass = "Positive",
      powerAnalysis = TRUE,
      powerAnalysisType = "sample_size",
      expectedAUCDifference = 0.15,
      targetPower = 0.9
    )
  })
})

# ═══════════════════════════════════════════════════════════
# BAYESIAN / BOOTSTRAP ROC ANALYSIS
# ═══════════════════════════════════════════════════════════

test_that("Bayesian/bootstrap ROC analysis works with defaults", {
  data <- create_advanced_test_data(n = 100)

  expect_no_error({
    result <- psychopdaROC(
      data = data,
      dependentVars = "marker_excellent",
      classVar = "diagnosis",
      positiveClass = "Positive",
      bayesianAnalysis = TRUE,
      bootstrapReps = 200  # Reduced for testing speed
    )
  })
})

test_that("Bayesian analysis respects prior parameters", {
  data <- create_advanced_test_data(n = 100)

  expect_no_error({
    result <- psychopdaROC(
      data = data,
      dependentVars = "marker_excellent",
      classVar = "diagnosis",
      positiveClass = "Positive",
      bayesianAnalysis = TRUE,
      priorAUC = 0.8,
      priorPrecision = 20,
      bootstrapReps = 200
    )
  })
})

test_that("Bayesian analysis works with multiple variables", {
  data <- create_advanced_test_data(n = 100)

  expect_no_error({
    result <- psychopdaROC(
      data = data,
      dependentVars = c("marker_excellent", "marker_good"),
      classVar = "diagnosis",
      positiveClass = "Positive",
      bayesianAnalysis = TRUE,
      bootstrapReps = 200
    )
  })
})

# ═══════════════════════════════════════════════════════════
# CLINICAL UTILITY ANALYSIS
# ═══════════════════════════════════════════════════════════

test_that("clinical utility analysis works with defaults", {
  data <- create_advanced_test_data(n = 150)

  expect_no_error({
    result <- psychopdaROC(
      data = data,
      dependentVars = "marker_excellent",
      classVar = "diagnosis",
      positiveClass = "Positive",
      clinicalUtilityAnalysis = TRUE
    )
  })
})

test_that("clinical utility respects treatment threshold range", {
  data <- create_advanced_test_data(n = 150)

  expect_no_error({
    result <- psychopdaROC(
      data = data,
      dependentVars = "marker_excellent",
      classVar = "diagnosis",
      positiveClass = "Positive",
      clinicalUtilityAnalysis = TRUE,
      treatmentThreshold = "0.1,0.4,0.1"
    )
  })
})

test_that("clinical utility respects harm-benefit ratio", {
  data <- create_advanced_test_data(n = 150)

  expect_no_error({
    result <- psychopdaROC(
      data = data,
      dependentVars = "marker_excellent",
      classVar = "diagnosis",
      positiveClass = "Positive",
      clinicalUtilityAnalysis = TRUE,
      harmBenefitRatio = 0.5
    )
  })
})

test_that("clinical utility with intervention cost analysis", {
  data <- create_advanced_test_data(n = 150)

  expect_no_error({
    result <- psychopdaROC(
      data = data,
      dependentVars = "marker_excellent",
      classVar = "diagnosis",
      positiveClass = "Positive",
      clinicalUtilityAnalysis = TRUE,
      interventionCost = TRUE
    )
  })
})

# ═══════════════════════════════════════════════════════════
# META-ANALYSIS (Requires 3+ Variables)
# ═══════════════════════════════════════════════════════════

test_that("meta-analysis works with 3+ variables", {
  data <- create_advanced_test_data(n = 200)

  expect_no_error({
    result <- psychopdaROC(
      data = data,
      dependentVars = c("marker_excellent", "marker_good", "marker_moderate"),
      classVar = "diagnosis",
      positiveClass = "Positive",
      metaAnalysis = TRUE,
      overrideMetaAnalysisWarning = TRUE
    )
  })
})

test_that("meta-analysis fixed effects model works", {
  data <- create_advanced_test_data(n = 200)

  expect_no_error({
    result <- psychopdaROC(
      data = data,
      dependentVars = c("marker_excellent", "marker_good", "marker_moderate", "marker_fair"),
      classVar = "diagnosis",
      positiveClass = "Positive",
      metaAnalysis = TRUE,
      metaAnalysisMethod = "fixed",
      overrideMetaAnalysisWarning = TRUE
    )
  })
})

test_that("meta-analysis random effects model works", {
  data <- create_advanced_test_data(n = 200)

  expect_no_error({
    result <- psychopdaROC(
      data = data,
      dependentVars = c("marker_excellent", "marker_good", "marker_moderate"),
      classVar = "diagnosis",
      positiveClass = "Positive",
      metaAnalysis = TRUE,
      metaAnalysisMethod = "random",
      overrideMetaAnalysisWarning = TRUE
    )
  })
})

test_that("meta-analysis both models works", {
  data <- create_advanced_test_data(n = 200)

  expect_no_error({
    result <- psychopdaROC(
      data = data,
      dependentVars = c("marker_excellent", "marker_good", "marker_moderate"),
      classVar = "diagnosis",
      positiveClass = "Positive",
      metaAnalysis = TRUE,
      metaAnalysisMethod = "both",
      heterogeneityTest = TRUE,
      overrideMetaAnalysisWarning = TRUE
    )
  })
})

test_that("meta-analysis forest plot works", {
  data <- create_advanced_test_data(n = 200)

  expect_no_error({
    result <- psychopdaROC(
      data = data,
      dependentVars = c("marker_excellent", "marker_good", "marker_moderate"),
      classVar = "diagnosis",
      positiveClass = "Positive",
      metaAnalysis = TRUE,
      forestPlot = TRUE,
      overrideMetaAnalysisWarning = TRUE
    )
  })
})

test_that("meta-analysis with 5 variables works", {
  data <- create_advanced_test_data(n = 250)

  expect_no_error({
    result <- psychopdaROC(
      data = data,
      dependentVars = c("marker_excellent", "marker_good", "marker_moderate",
                        "marker_fair", "marker_poor"),
      classVar = "diagnosis",
      positiveClass = "Positive",
      metaAnalysis = TRUE,
      metaAnalysisMethod = "both",
      heterogeneityTest = TRUE,
      forestPlot = TRUE,
      overrideMetaAnalysisWarning = TRUE
    )
  })
})

# ═══════════════════════════════════════════════════════════
# FIXED SENSITIVITY/SPECIFICITY ANALYSIS
# ═══════════════════════════════════════════════════════════

test_that("fixed sensitivity analysis works", {
  data <- create_advanced_test_data(n = 150)

  expect_no_error({
    result <- psychopdaROC(
      data = data,
      dependentVars = "marker_excellent",
      classVar = "diagnosis",
      positiveClass = "Positive",
      fixedSensSpecAnalysis = TRUE,
      fixedAnalysisType = "sensitivity",
      fixedSensitivityValue = 0.90
    )
  })
})

test_that("fixed specificity analysis works", {
  data <- create_advanced_test_data(n = 150)

  expect_no_error({
    result <- psychopdaROC(
      data = data,
      dependentVars = "marker_excellent",
      classVar = "diagnosis",
      positiveClass = "Positive",
      fixedSensSpecAnalysis = TRUE,
      fixedAnalysisType = "specificity",
      fixedSpecificityValue = 0.95
    )
  })
})

test_that("fixed sensitivity with different interpolation methods", {
  data <- create_advanced_test_data(n = 150)

  for (interp in c("linear", "nearest", "stepwise")) {
    expect_no_error({
      result <- psychopdaROC(
        data = data,
        dependentVars = "marker_excellent",
        classVar = "diagnosis",
        positiveClass = "Positive",
        fixedSensSpecAnalysis = TRUE,
        fixedAnalysisType = "sensitivity",
        fixedSensitivityValue = 0.90,
        fixedInterpolation = interp
      )
    })
  }
})

test_that("fixed sensitivity ROC plot works", {
  data <- create_advanced_test_data(n = 150)

  expect_no_error({
    result <- psychopdaROC(
      data = data,
      dependentVars = "marker_excellent",
      classVar = "diagnosis",
      positiveClass = "Positive",
      fixedSensSpecAnalysis = TRUE,
      fixedAnalysisType = "sensitivity",
      fixedSensitivityValue = 0.90,
      showFixedROC = TRUE,
      showFixedExplanation = TRUE
    )
  })
})

test_that("fixed analysis with multiple variables", {
  data <- create_advanced_test_data(n = 150)

  expect_no_error({
    result <- psychopdaROC(
      data = data,
      dependentVars = c("marker_excellent", "marker_good"),
      classVar = "diagnosis",
      positiveClass = "Positive",
      fixedSensSpecAnalysis = TRUE,
      fixedAnalysisType = "sensitivity",
      fixedSensitivityValue = 0.85
    )
  })
})

# ═══════════════════════════════════════════════════════════
# PARTIAL AUC
# ═══════════════════════════════════════════════════════════

test_that("partial AUC with default range", {
  data <- create_advanced_test_data(n = 150)

  expect_no_error({
    result <- psychopdaROC(
      data = data,
      dependentVars = "marker_excellent",
      classVar = "diagnosis",
      positiveClass = "Positive",
      partialAUC = TRUE,
      partialAUCfrom = 0.8,
      partialAUCto = 1.0
    )
  })
})

test_that("partial AUC with custom range", {
  data <- create_advanced_test_data(n = 150)

  expect_no_error({
    result <- psychopdaROC(
      data = data,
      dependentVars = "marker_excellent",
      classVar = "diagnosis",
      positiveClass = "Positive",
      partialAUC = TRUE,
      partialAUCfrom = 0.6,
      partialAUCto = 0.9
    )
  })
})

test_that("partial AUC with multiple variables", {
  data <- create_advanced_test_data(n = 150)

  expect_no_error({
    result <- psychopdaROC(
      data = data,
      dependentVars = c("marker_excellent", "marker_good"),
      classVar = "diagnosis",
      positiveClass = "Positive",
      partialAUC = TRUE,
      partialAUCfrom = 0.8,
      partialAUCto = 1.0
    )
  })
})

# ═══════════════════════════════════════════════════════════
# BOOTSTRAP CONFIDENCE INTERVALS
# ═══════════════════════════════════════════════════════════

test_that("bootstrap CIs work with reduced replications", {
  data <- create_advanced_test_data(n = 100)

  expect_no_error({
    result <- psychopdaROC(
      data = data,
      dependentVars = "marker_excellent",
      classVar = "diagnosis",
      positiveClass = "Positive",
      bootstrapCI = TRUE,
      bootstrapReps = 200
    )
  })
})

test_that("bootstrap CIs work with multiple variables", {
  data <- create_advanced_test_data(n = 100)

  expect_no_error({
    result <- psychopdaROC(
      data = data,
      dependentVars = c("marker_excellent", "marker_good"),
      classVar = "diagnosis",
      positiveClass = "Positive",
      bootstrapCI = TRUE,
      bootstrapReps = 200
    )
  })
})

# ═══════════════════════════════════════════════════════════
# ROC SMOOTHING METHODS
# ═══════════════════════════════════════════════════════════

test_that("binormal ROC smoothing works", {
  data <- create_advanced_test_data(n = 150)

  expect_no_error({
    result <- psychopdaROC(
      data = data,
      dependentVars = "marker_excellent",
      classVar = "diagnosis",
      positiveClass = "Positive",
      rocSmoothingMethod = "binormal"
    )
  })
})

test_that("density-based ROC smoothing works", {
  data <- create_advanced_test_data(n = 150)

  expect_no_error({
    result <- psychopdaROC(
      data = data,
      dependentVars = "marker_excellent",
      classVar = "diagnosis",
      positiveClass = "Positive",
      rocSmoothingMethod = "density"
    )
  })
})

# ═══════════════════════════════════════════════════════════
# IDI/NRI WITH ADVANCED OPTIONS
# ═══════════════════════════════════════════════════════════

test_that("IDI with reference variable selection", {
  data <- create_advanced_test_data(n = 150)

  expect_no_error({
    result <- psychopdaROC(
      data = data,
      dependentVars = c("marker_excellent", "marker_good", "marker_moderate"),
      classVar = "diagnosis",
      positiveClass = "Positive",
      calculateIDI = TRUE,
      refVar = "marker_excellent",
      idiNriBootRuns = 200
    )
  })
})

test_that("NRI with custom thresholds", {
  data <- create_advanced_test_data(n = 150)

  expect_no_error({
    result <- psychopdaROC(
      data = data,
      dependentVars = c("marker_excellent", "marker_good"),
      classVar = "diagnosis",
      positiveClass = "Positive",
      calculateNRI = TRUE,
      refVar = "marker_excellent",
      nriThresholds = "0.2,0.5,0.8",
      idiNriBootRuns = 200
    )
  })
})

test_that("continuous NRI works (empty thresholds)", {
  data <- create_advanced_test_data(n = 150)

  expect_no_error({
    result <- psychopdaROC(
      data = data,
      dependentVars = c("marker_excellent", "marker_good"),
      classVar = "diagnosis",
      positiveClass = "Positive",
      calculateNRI = TRUE,
      refVar = "marker_excellent",
      nriThresholds = "",
      idiNriBootRuns = 200
    )
  })
})

test_that("combined IDI and NRI analysis", {
  data <- create_advanced_test_data(n = 150)

  expect_no_error({
    result <- psychopdaROC(
      data = data,
      dependentVars = c("marker_excellent", "marker_good"),
      classVar = "diagnosis",
      positiveClass = "Positive",
      calculateIDI = TRUE,
      calculateNRI = TRUE,
      refVar = "marker_excellent",
      idiNriBootRuns = 200
    )
  })
})

# ═══════════════════════════════════════════════════════════
# CLASSIFIER COMPARISON
# ═══════════════════════════════════════════════════════════

test_that("classifier comparison with multiple variables", {
  data <- create_advanced_test_data(n = 150)

  expect_no_error({
    result <- psychopdaROC(
      data = data,
      dependentVars = c("marker_excellent", "marker_good", "marker_moderate"),
      classVar = "diagnosis",
      positiveClass = "Positive",
      compareClassifiers = TRUE
    )
  })
})

# ═══════════════════════════════════════════════════════════
# PRIOR PREVALENCE
# ═══════════════════════════════════════════════════════════

test_that("prior prevalence adjustment works", {
  data <- create_advanced_test_data(n = 150)

  expect_no_error({
    result <- psychopdaROC(
      data = data,
      dependentVars = "marker_excellent",
      classVar = "diagnosis",
      positiveClass = "Positive",
      usePriorPrev = TRUE,
      priorPrev = 0.1
    )
  })
})

test_that("prior prevalence with high prevalence", {
  data <- create_advanced_test_data(n = 150)

  expect_no_error({
    result <- psychopdaROC(
      data = data,
      dependentVars = "marker_excellent",
      classVar = "diagnosis",
      positiveClass = "Positive",
      usePriorPrev = TRUE,
      priorPrev = 0.8
    )
  })
})

# ═══════════════════════════════════════════════════════════
# VISUALIZATION OPTIONS (Advanced)
# ═══════════════════════════════════════════════════════════

test_that("clean publication plot works", {
  data <- create_advanced_test_data(n = 100)

  expect_no_error({
    result <- psychopdaROC(
      data = data,
      dependentVars = "marker_excellent",
      classVar = "diagnosis",
      positiveClass = "Positive",
      plotROC = TRUE,
      cleanPlot = TRUE
    )
  })
})

test_that("confidence bands on ROC curve", {
  data <- create_advanced_test_data(n = 100)

  expect_no_error({
    result <- psychopdaROC(
      data = data,
      dependentVars = "marker_excellent",
      classVar = "diagnosis",
      positiveClass = "Positive",
      plotROC = TRUE,
      showConfidenceBands = TRUE
    )
  })
})

test_that("quantile CIs on ROC curve", {
  data <- create_advanced_test_data(n = 100)

  expect_no_error({
    result <- psychopdaROC(
      data = data,
      dependentVars = "marker_excellent",
      classVar = "diagnosis",
      positiveClass = "Positive",
      plotROC = TRUE,
      quantileCIs = TRUE,
      quantiles = "0.25,0.5,0.75"
    )
  })
})

test_that("direct curve labels work", {
  data <- create_advanced_test_data(n = 100)

  expect_no_error({
    result <- psychopdaROC(
      data = data,
      dependentVars = c("marker_excellent", "marker_good"),
      classVar = "diagnosis",
      positiveClass = "Positive",
      plotROC = TRUE,
      combinePlots = TRUE,
      directLabel = TRUE
    )
  })
})

test_that("different legend positions work", {
  data <- create_advanced_test_data(n = 100)

  for (pos in c("none", "right", "bottom", "topleft", "topright")) {
    expect_no_error({
      result <- psychopdaROC(
        data = data,
        dependentVars = c("marker_excellent", "marker_good"),
        classVar = "diagnosis",
        positiveClass = "Positive",
        plotROC = TRUE,
        combinePlots = TRUE,
        legendPosition = pos
      )
    })
  }
})

test_that("LOESS smoothing with SE bands", {
  data <- create_advanced_test_data(n = 100)

  expect_no_error({
    result <- psychopdaROC(
      data = data,
      dependentVars = "marker_excellent",
      classVar = "diagnosis",
      positiveClass = "Positive",
      plotROC = TRUE,
      smoothing = TRUE,
      displaySE = TRUE
    )
  })
})
