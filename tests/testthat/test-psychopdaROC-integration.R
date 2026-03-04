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
# SCENARIO 2: Biomarker Comparison for Research Publication
# A researcher wants to compare 3 biomarkers with statistical tests
# ═══════════════════════════════════════════════════════════

create_clinical_data <- function(n = 200) {
  set.seed(42)
  diagnosis <- sample(c("Positive", "Negative"), n, replace = TRUE)
  is_pos <- diagnosis == "Positive"
  
  data.frame(
    diagnosis = factor(diagnosis, levels = c("Negative", "Positive")),
    marker_a = rnorm(n, mean = ifelse(is_pos, 5, 2), sd = 1),
    marker_b = rnorm(n, mean = ifelse(is_pos, 4, 3), sd = 1.5),
    marker_c = rnorm(n, mean = ifelse(is_pos, 6, 2), sd = 2),
    marker_d = rnorm(n, mean = ifelse(is_pos, 7, 3), sd = 1.2),
    hospital = factor(sample(c("Hospital A", "Hospital B"), n, replace = TRUE))
  )
}

test_that("research publication workflow: DeLong + IDI/NRI + effect size", {
  data <- create_clinical_data(n = 200)

  expect_no_error({
    result <- psychopdaROC(
      data = data,
      dependentVars = c("marker_a", "marker_b", "marker_c"),
      classVar = "diagnosis",
      positiveClass = "Positive",
      clinicalMode = "comprehensive",
      # Statistical comparisons
      delongTest = TRUE,
      calculateIDI = TRUE,
      calculateNRI = TRUE,
      refVar = "marker_a",
      idiNriBootRuns = 200,
      # Effect sizes
      effectSizeAnalysis = TRUE,
      # Visualization
      plotROC = TRUE,
      combinePlots = TRUE,
      cleanPlot = TRUE,
      precisionRecallCurve = TRUE
    )
  })
})

# ═══════════════════════════════════════════════════════════
# SCENARIO 3: Comprehensive Analysis with All Output Types
# Testing all tables and plots together
# ═══════════════════════════════════════════════════════════

test_that("comprehensive output: all tables and basic plots", {
  data <- create_clinical_data(n = 150)

  expect_no_error({
    result <- psychopdaROC(
      data = data,
      dependentVars = c("marker_a", "marker_b"),
      classVar = "diagnosis",
      positiveClass = "Positive",
      # All table outputs
      sensSpecTable = TRUE,
      showThresholdTable = TRUE,
      maxThresholds = 10,
      delongTest = TRUE,
      compareClassifiers = TRUE,
      # All plot outputs
      plotROC = TRUE,
      combinePlots = TRUE,
      showCriterionPlot = TRUE,
      showPrevalencePlot = TRUE,
      showDotPlot = TRUE,
      precisionRecallCurve = TRUE
    )
  })
})

# ═══════════════════════════════════════════════════════════
# SCENARIO 4: Clinical Utility with Decision Curve Analysis
# ═══════════════════════════════════════════════════════════

test_that("clinical utility workflow: decision curves + fixed sens/spec", {
  data <- create_clinical_data(n = 200)

  expect_no_error({
    result <- psychopdaROC(
      data = data,
      dependentVars = "marker_a",
      classVar = "diagnosis",
      positiveClass = "Positive",
      # Clinical utility
      clinicalUtilityAnalysis = TRUE,
      treatmentThreshold = "0.05,0.5,0.05",
      harmBenefitRatio = 0.25,
      # Fixed sensitivity
      fixedSensSpecAnalysis = TRUE,
      fixedAnalysisType = "sensitivity",
      fixedSensitivityValue = 0.95,
      showFixedROC = TRUE,
      showFixedExplanation = TRUE
    )
  })
})

# ═══════════════════════════════════════════════════════════
# SCENARIO 5: Advanced ROC with Bootstrap and Partial AUC
# ═══════════════════════════════════════════════════════════

test_that("advanced ROC: bootstrap CI + partial AUC + smoothing", {
  data <- create_clinical_data(n = 150)

  expect_no_error({
    result <- psychopdaROC(
      data = data,
      dependentVars = "marker_a",
      classVar = "diagnosis",
      positiveClass = "Positive",
      # Bootstrap
      bootstrapCI = TRUE,
      bootstrapReps = 200,
      # Partial AUC
      partialAUC = TRUE,
      partialAUCfrom = 0.8,
      partialAUCto = 1.0,
      # Smoothing
      rocSmoothingMethod = "binormal",
      # Plot
      plotROC = TRUE,
      showOptimalPoint = TRUE
    )
  })
})

# ═══════════════════════════════════════════════════════════
# SCENARIO 6: Power Analysis + Bayesian for Grant Application
# ═══════════════════════════════════════════════════════════

test_that("power and Bayesian workflow: power analysis + Bayesian ROC", {
  data <- create_clinical_data(n = 150)

  expect_no_error({
    result <- psychopdaROC(
      data = data,
      dependentVars = "marker_a",
      classVar = "diagnosis",
      positiveClass = "Positive",
      # Power analysis
      powerAnalysis = TRUE,
      powerAnalysisType = "post_hoc",
      targetPower = 0.8,
      expectedAUCDifference = 0.1,
      # Bayesian
      bayesianAnalysis = TRUE,
      priorAUC = 0.7,
      priorPrecision = 10,
      bootstrapReps = 200
    )
  })
})

# ═══════════════════════════════════════════════════════════
# SCENARIO 7: Meta-analysis Across Multiple Markers
# ═══════════════════════════════════════════════════════════

test_that("meta-analysis workflow: 4 markers with forest plot", {
  data <- create_clinical_data(n = 200)

  expect_no_error({
    result <- psychopdaROC(
      data = data,
      dependentVars = c("marker_a", "marker_b", "marker_c", "marker_d"),
      classVar = "diagnosis",
      positiveClass = "Positive",
      # Meta-analysis
      metaAnalysis = TRUE,
      metaAnalysisMethod = "both",
      heterogeneityTest = TRUE,
      forestPlot = TRUE,
      overrideMetaAnalysisWarning = TRUE,
      # Also show individual ROCs
      plotROC = TRUE,
      combinePlots = TRUE
    )
  })
})

# ═══════════════════════════════════════════════════════════
# SCENARIO 8: Subgroup Analysis with All Features
# ═══════════════════════════════════════════════════════════

test_that("subgroup workflow: stratified ROC by hospital", {
  data <- create_clinical_data(n = 200)

  expect_no_error({
    result <- psychopdaROC(
      data = data,
      dependentVars = "marker_a",
      classVar = "diagnosis",
      positiveClass = "Positive",
      subGroup = "hospital",
      method = "maximize_metric",
      metric = "youden",
      plotROC = TRUE,
      sensSpecTable = TRUE,
      showCriterionPlot = TRUE
    )
  })
})

# ═══════════════════════════════════════════════════════════
# SCENARIO 9: Prior Prevalence Adjustment
# ═══════════════════════════════════════════════════════════

test_that("prevalence adjustment: prior prevalence + prevalence plot", {
  data <- create_clinical_data(n = 150)

  expect_no_error({
    result <- psychopdaROC(
      data = data,
      dependentVars = "marker_a",
      classVar = "diagnosis",
      positiveClass = "Positive",
      usePriorPrev = TRUE,
      priorPrev = 0.05,  # Low prevalence screening
      showPrevalencePlot = TRUE,
      sensSpecTable = TRUE
    )
  })
})

# ═══════════════════════════════════════════════════════════
# SCENARIO 10: Kitchen Sink - Everything Enabled
# ═══════════════════════════════════════════════════════════

test_that("kitchen sink: all non-conflicting features enabled", {
  data <- create_clinical_data(n = 200)

  expect_no_error({
    result <- psychopdaROC(
      data = data,
      dependentVars = c("marker_a", "marker_b", "marker_c"),
      classVar = "diagnosis",
      positiveClass = "Positive",
      clinicalMode = "comprehensive",
      # Cutpoint
      method = "maximize_metric",
      metric = "youden",
      direction = ">=",
      # Output tables
      sensSpecTable = TRUE,
      showThresholdTable = TRUE,
      maxThresholds = 10,
      delongTest = TRUE,
      compareClassifiers = TRUE,
      # IDI/NRI
      calculateIDI = TRUE,
      calculateNRI = TRUE,
      refVar = "marker_a",
      idiNriBootRuns = 200,
      # Advanced
      effectSizeAnalysis = TRUE,
      partialAUC = TRUE,
      partialAUCfrom = 0.8,
      partialAUCto = 1.0,
      bootstrapCI = TRUE,
      bootstrapReps = 200,
      # Plots
      plotROC = TRUE,
      combinePlots = TRUE,
      showOptimalPoint = TRUE,
      showCriterionPlot = TRUE,
      showPrevalencePlot = TRUE,
      showDotPlot = TRUE,
      precisionRecallCurve = TRUE
    )
  })
})

# ═══════════════════════════════════════════════════════════
# SCENARIO 11: Reversed Direction
# ═══════════════════════════════════════════════════════════

test_that("reversed direction with comprehensive analysis", {
  data <- create_clinical_data(n = 150)

  expect_no_error({
    result <- psychopdaROC(
      data = data,
      dependentVars = "marker_a",
      classVar = "diagnosis",
      positiveClass = "Positive",
      direction = "<=",
      plotROC = TRUE,
      sensSpecTable = TRUE,
      showThresholdTable = TRUE
    )
  })
})

# ═══════════════════════════════════════════════════════════
# SCENARIO 12: Performance Under Load
# ═══════════════════════════════════════════════════════════

test_that("performance: large dataset with multiple features", {
  data <- create_clinical_data(n = 500)

  start_time <- Sys.time()
  expect_no_error({
    result <- psychopdaROC(
      data = data,
      dependentVars = c("marker_a", "marker_b", "marker_c"),
      classVar = "diagnosis",
      positiveClass = "Positive",
      delongTest = TRUE,
      compareClassifiers = TRUE,
      plotROC = TRUE,
      combinePlots = TRUE,
      sensSpecTable = TRUE
    )
  })
  elapsed <- as.numeric(Sys.time() - start_time, units = "secs")

  # Should complete within 60 seconds even with all features
  expect_true(elapsed < 60,
              info = paste("Elapsed time:", round(elapsed, 1), "seconds"))
})
