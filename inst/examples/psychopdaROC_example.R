# ═══════════════════════════════════════════════════════════
# Advanced ROC Analysis Examples
# ═══════════════════════════════════════════════════════════
library(ClinicoPath)

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
  method = "oc_cost_ratio"
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

# ═══ EXAMPLE 10: Comprehensive Research Analysis ═══
data(psychopdaROC_multibiomarker)
psychopdaROC(
  data = psychopdaROC_multibiomarker,
  dependentVars = c("marker1", "marker2", "marker3", "combined_score"),
  classVar = "diagnosis",
  positiveClass = "Positive",
  clinicalMode = "comprehensive",
  method = "maximize_metric",
  metric = "youden",
  direction = ">=",
  break_ties = "mean"
)
