# ═══════════════════════════════════════════════════════════
# Compare Medical Decision Tests Examples
# ═══════════════════════════════════════════════════════════
library(ClinicoPath)

# ═══ EXAMPLE 1: Basic Two-Test Comparison ═══
data(decisioncompare_test)
decisioncompare(
  data = decisioncompare_test,
  gold = "GoldStandard",
  goldPositive = "Positive",
  goldNegative = NULL,
  test1 = "Test1",
  test1Positive = "Positive",
  test1Negative = NULL,
  test2 = "Test2",
  test2Positive = "Positive",
  test2Negative = NULL,
  test3Negative = NULL
)

# ═══ EXAMPLE 2: Three-Test Comparison with Statistical Analysis ═══
data(decisioncompare_threetest)
decisioncompare(
  data = decisioncompare_threetest,
  gold = "GoldStandard",
  goldPositive = "Positive",
  goldNegative = NULL,
  test1 = "Test1",
  test1Positive = "Positive",
  test1Negative = NULL,
  test2 = "Test2",
  test2Positive = "Positive",
  test2Negative = NULL,
  test3 = "Test3",
  test3Positive = "Positive",
  test3Negative = NULL,
  statComp = TRUE,
  radarplot = TRUE
)

# ═══ EXAMPLE 3: Imaging Modalities Comparison ═══
data(decisioncompare_imaging)
decisioncompare(
  data = decisioncompare_imaging,
  gold = "Pathology",
  goldPositive = "Malignant",
  goldNegative = NULL,
  test1 = "CT_Scan",
  test1Positive = "Abnormal",
  test1Negative = NULL,
  test2 = "MRI",
  test2Positive = "Abnormal",
  test2Negative = NULL,
  test3 = "Biomarker",
  test3Positive = "Elevated",
  test3Negative = NULL,
  ci = TRUE,
  plot = TRUE,
  radarplot = TRUE
)

# ═══ EXAMPLE 4: Screening vs Diagnostic Test ═══
data(decisioncompare_screening)
decisioncompare(
  data = decisioncompare_screening,
  gold = "Biopsy",
  goldPositive = "Positive",
  goldNegative = NULL,
  test1 = "ScreeningTest",
  test1Positive = "Positive",
  test1Negative = NULL,
  test2 = "DiagnosticTest",
  test2Positive = "Positive",
  test2Negative = NULL,
  test3Negative = NULL,
  ci = TRUE,
  plot = TRUE,
  statComp = TRUE
)

# ═══ EXAMPLE 5: Using Custom Prevalence for Clinical Setting ═══
data(decisioncompare_test)
decisioncompare(
  data = decisioncompare_test,
  gold = "GoldStandard",
  goldPositive = "Positive",
  goldNegative = NULL,
  test1 = "Test1",
  test1Positive = "Positive",
  test1Negative = NULL,
  test2 = "Test2",
  test2Positive = "Positive",
  test2Negative = NULL,
  test3Negative = NULL,
  pp = TRUE,
  pprob = 0.15,  # 15% prevalence in screening population
  ci = TRUE,
  od = TRUE
)

# ═══ EXAMPLE 6: Inter-Rater Agreement Comparison ═══
data(decisioncompare_raters)
decisioncompare(
  data = decisioncompare_raters,
  gold = "ConsensusPanel",
  goldPositive = "Disease",
  goldNegative = NULL,
  test1 = "Rater1",
  test1Positive = "Disease",
  test1Negative = NULL,
  test2 = "Rater2",
  test2Positive = "Disease",
  test2Negative = NULL,
  test3 = "Rater3",
  test3Positive = "Disease",
  test3Negative = NULL,
  statComp = TRUE,
  ci = TRUE,
  radarplot = TRUE
)

# ═══ EXAMPLE 7: Handling Indeterminate Results ═══
data(decisioncompare_indeterminate)
# Exclude indeterminate results
decisioncompare(
  data = decisioncompare_indeterminate,
  gold = "GoldStandard",
  goldPositive = "Positive",
  goldNegative = NULL,
  test1 = "Test1",
  test1Positive = "Positive",
  test1Negative = NULL,
  test2 = "Test2",
  test2Positive = "Positive",
  test2Negative = NULL,
  test3Negative = NULL,
  excludeIndeterminate = TRUE,
  ci = TRUE
)

# ═══ EXAMPLE 8: Perfect vs Imperfect Test ═══
data(decisioncompare_perfect)
decisioncompare(
  data = decisioncompare_perfect,
  gold = "GoldStandard",
  goldPositive = "Positive",
  goldNegative = NULL,
  test1 = "PerfectTest",
  test1Positive = "Positive",
  test1Negative = NULL,
  test2 = "ImperfectTest",
  test2Positive = "Positive",
  test2Negative = NULL,
  test3Negative = NULL,
  ci = TRUE,
  plot = TRUE,
  statComp = TRUE
)

# ═══ EXAMPLE 9: Rare Disease Screening ═══
data(decisioncompare_rare)
decisioncompare(
  data = decisioncompare_rare,
  gold = "GoldStandard",
  goldPositive = "Positive",
  goldNegative = NULL,
  test1 = "Test1",
  test1Positive = "Positive",
  test1Negative = NULL,
  test2 = "Test2",
  test2Positive = "Positive",
  test2Negative = NULL,
  test3Negative = NULL,
  pp = TRUE,
  pprob = 0.05,  # 5% prevalence in screening
  ci = TRUE,
  plot = TRUE
)

# ═══ EXAMPLE 10: Comprehensive Analysis ═══
data(decisioncompare_threetest)
decisioncompare(
  data = decisioncompare_threetest,
  gold = "GoldStandard",
  goldPositive = "Positive",
  goldNegative = NULL,
  test1 = "Test1",
  test1Positive = "Positive",
  test1Negative = NULL,
  test2 = "Test2",
  test2Positive = "Positive",
  test2Negative = NULL,
  test3 = "Test3",
  test3Positive = "Positive",
  test3Negative = NULL,
  pp = TRUE,
  pprob = 0.30,
  ci = TRUE,
  plot = TRUE,
  radarplot = TRUE,
  statComp = TRUE,
  od = TRUE
)
