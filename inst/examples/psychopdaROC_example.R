# ═══════════════════════════════════════════════════════════
# Advanced ROC Analysis Examples
# psychopdaROC - Comprehensive ROC Curve Analysis
# ═══════════════════════════════════════════════════════════
library(ClinicoPath)

# ═══════════════════════════════════════════════════════════
# BASIC EXAMPLES
# ═══════════════════════════════════════════════════════════

# ═══ EXAMPLE 1: Basic ROC Analysis ═══
data(psychopdaROC_test)
psychopdaROC(
  data = psychopdaROC_test,
  dependentVars = "biomarker",
  classVar = "disease_status",
  positiveClass = "Disease"
)

# ═══ EXAMPLE 2: Cancer Screening with Multiple Markers ═══
data(psychopdaROC_screening)
psychopdaROC(
  data = psychopdaROC_screening,
  dependentVars = c("psa_level", "ca125"),
  classVar = "cancer",
  positiveClass = "Cancer",
  clinicalMode = "advanced"
)

# ═══ EXAMPLE 3: Cardiac Biomarker Comparison ═══
data(psychopdaROC_cardiac)
psychopdaROC(
  data = psychopdaROC_cardiac,
  dependentVars = c("troponin", "creatinine", "bnp"),
  classVar = "mi_status",
  positiveClass = "MI",
  method = "maximize_metric",
  metric = "youden"
)

# ═══════════════════════════════════════════════════════════
# CLINICAL PRESET EXAMPLES
# ═══════════════════════════════════════════════════════════

# ═══ EXAMPLE 4: Screening Test (High Sensitivity Priority) ═══
data(psychopdaROC_screening)
psychopdaROC(
  data = psychopdaROC_screening,
  dependentVars = "psa_level",
  classVar = "cancer",
  positiveClass = "Cancer",
  clinicalPreset = "screening"
)

# ═══ EXAMPLE 5: Confirmation Test (High Specificity Priority) ═══
data(psychopdaROC_test)
psychopdaROC(
  data = psychopdaROC_test,
  dependentVars = "biomarker",
  classVar = "disease_status",
  positiveClass = "Disease",
  clinicalPreset = "confirmation"
)

# ═══════════════════════════════════════════════════════════
# CUTPOINT METHODS
# ═══════════════════════════════════════════════════════════

# ═══ EXAMPLE 6: Subgroup Analysis ═══
data(psychopdaROC_subgroup)
psychopdaROC(
  data = psychopdaROC_subgroup,
  dependentVars = "test_score",
  classVar = "disease",
  positiveClass = "Disease",
  subGroup = "age_group"
)

# ═══ EXAMPLE 7: Cost-Benefit Optimization ═══
data(psychopdaROC_costbenefit)
psychopdaROC(
  data = psychopdaROC_costbenefit,
  dependentVars = "risk_score",
  classVar = "outcome",
  positiveClass = "Event",
  method = "oc_cost_ratio",
  costratioFP = 0.1  # FN costs 10x more than FP
)

# ═══ EXAMPLE 8: Manual Cutpoint ═══
data(psychopdaROC_test)
psychopdaROC(
  data = psychopdaROC_test,
  dependentVars = "biomarker",
  classVar = "disease_status",
  positiveClass = "Disease",
  method = "oc_manual",
  specifyCutScore = "60"
)

# ═══ EXAMPLE 9: Equal Sensitivity and Specificity ═══
data(psychopdaROC_test)
psychopdaROC(
  data = psychopdaROC_test,
  dependentVars = "biomarker",
  classVar = "disease_status",
  positiveClass = "Disease",
  method = "oc_equal_sens_spec"
)

# ═══════════════════════════════════════════════════════════
# STATISTICAL COMPARISON EXAMPLES
# ═══════════════════════════════════════════════════════════

# ═══ EXAMPLE 10: DeLong Test for AUC Comparison ═══
data(psychopdaROC_multibiomarker)
psychopdaROC(
  data = psychopdaROC_multibiomarker,
  dependentVars = c("marker1", "marker2", "marker3"),
  classVar = "diagnosis",
  positiveClass = "Positive",
  delongTest = TRUE,
  compareClassifiers = TRUE
)

# ═══ EXAMPLE 11: IDI and NRI Analysis ═══
data(psychopdaROC_multibiomarker)
psychopdaROC(
  data = psychopdaROC_multibiomarker,
  dependentVars = c("marker1", "marker2"),
  classVar = "diagnosis",
  positiveClass = "Positive",
  calculateIDI = TRUE,
  calculateNRI = TRUE,
  refVar = "marker1",
  nriThresholds = "0.3,0.7",
  idiNriBootRuns = 500
)

# ═══ EXAMPLE 12: Effect Size Analysis ═══
data(psychopdaROC_multibiomarker)
psychopdaROC(
  data = psychopdaROC_multibiomarker,
  dependentVars = c("marker1", "marker2", "marker3"),
  classVar = "diagnosis",
  positiveClass = "Positive",
  effectSizeAnalysis = TRUE
)

# ═══════════════════════════════════════════════════════════
# ADVANCED ROC ANALYSIS
# ═══════════════════════════════════════════════════════════

# ═══ EXAMPLE 13: Partial AUC ═══
data(psychopdaROC_test)
psychopdaROC(
  data = psychopdaROC_test,
  dependentVars = "biomarker",
  classVar = "disease_status",
  positiveClass = "Disease",
  partialAUC = TRUE,
  partialAUCfrom = 0.8,
  partialAUCto = 1.0
)

# ═══ EXAMPLE 14: Bootstrap Confidence Intervals ═══
data(psychopdaROC_test)
psychopdaROC(
  data = psychopdaROC_test,
  dependentVars = "biomarker",
  classVar = "disease_status",
  positiveClass = "Disease",
  bootstrapCI = TRUE,
  bootstrapReps = 2000,
  seed = 42
)

# ═══ EXAMPLE 15: Fixed Sensitivity Analysis ═══
# Find the cutpoint that achieves 95% sensitivity
data(psychopdaROC_test)
psychopdaROC(
  data = psychopdaROC_test,
  dependentVars = "biomarker",
  classVar = "disease_status",
  positiveClass = "Disease",
  fixedSensSpecAnalysis = TRUE,
  fixedAnalysisType = "sensitivity",
  fixedSensitivityValue = 0.95,
  showFixedROC = TRUE,
  showFixedExplanation = TRUE
)

# ═══ EXAMPLE 16: Fixed Specificity Analysis ═══
# Find the cutpoint that achieves 90% specificity
data(psychopdaROC_test)
psychopdaROC(
  data = psychopdaROC_test,
  dependentVars = "biomarker",
  classVar = "disease_status",
  positiveClass = "Disease",
  fixedSensSpecAnalysis = TRUE,
  fixedAnalysisType = "specificity",
  fixedSpecificityValue = 0.90,
  fixedInterpolation = "linear"
)

# ═══════════════════════════════════════════════════════════
# BAYESIAN AND POWER ANALYSIS
# ═══════════════════════════════════════════════════════════

# ═══ EXAMPLE 17: Bootstrap ROC Analysis with Prior ═══
data(psychopdaROC_test)
psychopdaROC(
  data = psychopdaROC_test,
  dependentVars = "biomarker",
  classVar = "disease_status",
  positiveClass = "Disease",
  bayesianAnalysis = TRUE,
  priorAUC = 0.7,
  priorPrecision = 10,
  bootstrapReps = 2000
)

# ═══ EXAMPLE 18: Statistical Power Analysis ═══
data(psychopdaROC_test)
psychopdaROC(
  data = psychopdaROC_test,
  dependentVars = "biomarker",
  classVar = "disease_status",
  positiveClass = "Disease",
  powerAnalysis = TRUE,
  powerAnalysisType = "post_hoc",
  targetPower = 0.8,
  expectedAUCDifference = 0.1,
  significanceLevel = 0.05
)

# ═══ EXAMPLE 19: Sample Size Estimation ═══
data(psychopdaROC_test)
psychopdaROC(
  data = psychopdaROC_test,
  dependentVars = "biomarker",
  classVar = "disease_status",
  positiveClass = "Disease",
  powerAnalysis = TRUE,
  powerAnalysisType = "sample_size",
  expectedAUCDifference = 0.15,
  targetPower = 0.9
)

# ═══════════════════════════════════════════════════════════
# CLINICAL UTILITY AND DECISION ANALYSIS
# ═══════════════════════════════════════════════════════════

# ═══ EXAMPLE 20: Clinical Utility / Decision Curve Analysis ═══
data(psychopdaROC_test)
psychopdaROC(
  data = psychopdaROC_test,
  dependentVars = "biomarker",
  classVar = "disease_status",
  positiveClass = "Disease",
  clinicalUtilityAnalysis = TRUE,
  treatmentThreshold = "0.05,0.5,0.05",
  harmBenefitRatio = 0.25
)

# ═══ EXAMPLE 21: Prior Prevalence Adjustment ═══
data(psychopdaROC_test)
psychopdaROC(
  data = psychopdaROC_test,
  dependentVars = "biomarker",
  classVar = "disease_status",
  positiveClass = "Disease",
  usePriorPrev = TRUE,
  priorPrev = 0.05,  # Screening in low-prevalence population
  showPrevalencePlot = TRUE,
  sensSpecTable = TRUE
)

# ═══════════════════════════════════════════════════════════
# META-ANALYSIS
# ═══════════════════════════════════════════════════════════

# ═══ EXAMPLE 22: Meta-Analysis of Multiple Markers ═══
# NOTE: Requires 3+ test variables; use with caution
# as within-study markers are not independent
data(psychopdaROC_multibiomarker)
psychopdaROC(
  data = psychopdaROC_multibiomarker,
  dependentVars = c("marker1", "marker2", "marker3", "combined_score"),
  classVar = "diagnosis",
  positiveClass = "Positive",
  metaAnalysis = TRUE,
  metaAnalysisMethod = "both",
  heterogeneityTest = TRUE,
  forestPlot = TRUE,
  overrideMetaAnalysisWarning = TRUE
)

# ═══════════════════════════════════════════════════════════
# COMPREHENSIVE RESEARCH ANALYSIS (All Features)
# ═══════════════════════════════════════════════════════════

# ═══ EXAMPLE 23: Publication-Ready Comprehensive Analysis ═══
data(psychopdaROC_multibiomarker)
psychopdaROC(
  data = psychopdaROC_multibiomarker,
  dependentVars = c("marker1", "marker2", "marker3"),
  classVar = "diagnosis",
  positiveClass = "Positive",
  clinicalMode = "comprehensive",
  # Cutpoint optimization
  method = "maximize_metric",
  metric = "youden",
  direction = ">=",
  # Statistical comparisons
  delongTest = TRUE,
  calculateIDI = TRUE,
  calculateNRI = TRUE,
  refVar = "marker1",
  idiNriBootRuns = 1000,
  compareClassifiers = TRUE,
  effectSizeAnalysis = TRUE,
  # Advanced ROC
  partialAUC = TRUE,
  partialAUCfrom = 0.8,
  partialAUCto = 1.0,
  bootstrapCI = TRUE,
  bootstrapReps = 2000,
  # Clinical utility
  clinicalUtilityAnalysis = TRUE,
  treatmentThreshold = "0.05,0.5,0.05",
  # Output
  sensSpecTable = TRUE,
  showThresholdTable = TRUE,
  # Visualization
  plotROC = TRUE,
  combinePlots = TRUE,
  cleanPlot = TRUE,
  showOptimalPoint = TRUE,
  showCriterionPlot = TRUE,
  showPrevalencePlot = TRUE,
  precisionRecallCurve = TRUE
)

# ═══════════════════════════════════════════════════════════
# VISUALIZATION EXAMPLES
# ═══════════════════════════════════════════════════════════

# ═══ EXAMPLE 24: Publication-Quality Clean Plot ═══
data(psychopdaROC_cardiac)
psychopdaROC(
  data = psychopdaROC_cardiac,
  dependentVars = c("troponin", "creatinine", "bnp"),
  classVar = "mi_status",
  positiveClass = "MI",
  plotROC = TRUE,
  combinePlots = TRUE,
  cleanPlot = TRUE,
  legendPosition = "bottom"
)

# ═══ EXAMPLE 25: ROC with Confidence Bands ═══
data(psychopdaROC_test)
psychopdaROC(
  data = psychopdaROC_test,
  dependentVars = "biomarker",
  classVar = "disease_status",
  positiveClass = "Disease",
  plotROC = TRUE,
  showConfidenceBands = TRUE,
  showOptimalPoint = TRUE
)

# ═══ EXAMPLE 26: All Additional Plots ═══
data(psychopdaROC_test)
psychopdaROC(
  data = psychopdaROC_test,
  dependentVars = "biomarker",
  classVar = "disease_status",
  positiveClass = "Disease",
  plotROC = TRUE,
  showCriterionPlot = TRUE,
  showPrevalencePlot = TRUE,
  showDotPlot = TRUE,
  precisionRecallCurve = TRUE
)
