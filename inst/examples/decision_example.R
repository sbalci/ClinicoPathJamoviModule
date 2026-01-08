# ═══════════════════════════════════════════════════════════
# Medical Decision Analysis Examples
# ═══════════════════════════════════════════════════════════
library(ClinicoPath)

# ═══ EXAMPLE 1: Basic Diagnostic Test Evaluation ═══
data(decision_test)
decision(
  data = decision_test,
  gold = "GoldStandard",
  goldPositive = "Positive",
  newtest = "NewTest",
  testPositive = "Positive"
)

# ═══ EXAMPLE 2: Cancer Screening Test (Low Prevalence) ═══
data(decision_screening)
decision(
  data = decision_screening,
  gold = "Biopsy",
  goldPositive = "Malignant",
  newtest = "ScreeningTest",
  testPositive = "Positive",
  ci = TRUE,
  fagan = TRUE
)

# ═══ EXAMPLE 3: Clinical Diagnostic Test with Custom Prevalence ═══
data(decision_diagnostic)
decision(
  data = decision_diagnostic,
  gold = "GoldStandard",
  goldPositive = "Present",
  newtest = "ClinicalTest",
  testPositive = "Positive",
  pp = TRUE,
  pprob = 0.45,  # 45% prevalence in clinical setting
  ci = TRUE,
  od = TRUE
)

# ═══ EXAMPLE 4: Cardiac Biomarker Test Evaluation ═══
data(decision_biomarker)
decision(
  data = decision_biomarker,
  gold = "Angiography",
  goldPositive = "MI",
  newtest = "Troponin",
  testPositive = "Elevated",
  ci = TRUE,
  showNaturalLanguage = TRUE,
  fagan = TRUE
)

# ═══ EXAMPLE 5: Imaging Test Performance ═══
data(decision_imaging)
decision(
  data = decision_imaging,
  gold = "Pathology",
  goldPositive = "Malignant",
  newtest = "CT_Scan",
  testPositive = "Abnormal",
  ci = TRUE,
  showClinicalInterpretation = TRUE
)

# ═══ EXAMPLE 6: Infectious Disease Rapid Test ═══
data(decision_infectious)
decision(
  data = decision_infectious,
  gold = "Culture",
  goldPositive = "Positive",
  newtest = "RapidTest",
  testPositive = "Positive",
  pp = TRUE,
  pprob = 0.20,
  ci = TRUE,
  fagan = TRUE,
  showNaturalLanguage = TRUE
)

# ═══ EXAMPLE 7: Pathology Frozen Section Evaluation ═══
data(decision_pathology)
decision(
  data = decision_pathology,
  gold = "Biopsy",
  goldPositive = "Malignant",
  newtest = "FrozenSection",
  testPositive = "Malignant",
  ci = TRUE,
  showMisclassified = TRUE,
  maxCasesShow = 30
)

# ═══ EXAMPLE 8: Point-of-Care Test ═══
data(decision_pointofcare)
decision(
  data = decision_pointofcare,
  gold = "LabTest",
  goldPositive = "Positive",
  newtest = "PointOfCare",
  testPositive = "Positive",
  ci = TRUE,
  od = TRUE,
  fagan = TRUE
)

# ═══ EXAMPLE 9: Rare Disease Screening ═══
data(decision_rare)
decision(
  data = decision_rare,
  gold = "GoldStandard",
  goldPositive = "Positive",
  newtest = "NewTest",
  testPositive = "Positive",
  pp = TRUE,
  pprob = 0.01,  # 1% prevalence
  ci = TRUE,
  fagan = TRUE,
  showClinicalInterpretation = TRUE
)

# ═══ EXAMPLE 10: Comprehensive Analysis with All Features ═══
data(decision_diagnostic)
decision(
  data = decision_diagnostic,
  gold = "GoldStandard",
  goldPositive = "Present",
  newtest = "ClinicalTest",
  testPositive = "Positive",
  pp = TRUE,
  pprob = 0.50,
  ci = TRUE,
  od = TRUE,
  fnote = TRUE,
  fagan = TRUE,
  showNaturalLanguage = TRUE,
  showClinicalInterpretation = TRUE,
  showMisclassified = TRUE,
  maxCasesShow = 50
)
