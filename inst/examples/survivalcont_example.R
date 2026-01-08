# ═══════════════════════════════════════════════════════════
# Survival Continuous Variable Examples
# ═══════════════════════════════════════════════════════════
library(ClinicoPath)

# ═══ EXAMPLE 1: Basic Biomarker Analysis ═══
data(survivalcont_test)
survivalcont(
  data = survivalcont_test,
  elapsedtime = "time_months",
  outcome = "outcome",
  outcomeLevel = "Dead",
  contexpl = "biomarker",
  sc = TRUE
)

# ═══ EXAMPLE 2: Find Optimal Cutoff ═══
data(survivalcont_ki67)
survivalcont(
  data = survivalcont_ki67,
  elapsedtime = "time_months",
  outcome = "outcome",
  outcomeLevel = "Dead",
  contexpl = "ki67_percent",
  findcut = TRUE,
  sc = TRUE
)

# ═══ EXAMPLE 3: PSA Level Analysis ═══
data(survivalcont_psa)
survivalcont(
  data = survivalcont_psa,
  elapsedtime = "time_months",
  outcome = "outcome",
  outcomeLevel = "Dead",
  contexpl = "psa_level",
  findcut = TRUE,
  cutp = "12, 24, 36, 60",
  sc = TRUE,
  kmunicate = TRUE
)

# ═══ EXAMPLE 4: Hemoglobin Level with Competing Risks ═══
data(survivalcont_hemoglobin)
survivalcont(
  data = survivalcont_hemoglobin,
  elapsedtime = "time_months",
  outcome = "outcome",
  outcomeLevel = "Dead",
  contexpl = "hemoglobin_gL",
  findcut = TRUE,
  sc = TRUE,
  ce = TRUE,
  ch = TRUE
)

# ═══ EXAMPLE 5: Tumor Size Analysis ═══
data(survivalcont_tumorsize)
survivalcont(
  data = survivalcont_tumorsize,
  elapsedtime = "time_months",
  outcome = "outcome",
  outcomeLevel = "Dead",
  contexpl = "tumor_size_cm",
  findcut = TRUE,
  sc = TRUE
)

# ═══ EXAMPLE 6: Age as Continuous Predictor ═══
data(survivalcont_age)
survivalcont(
  data = survivalcont_age,
  elapsedtime = "time_months",
  outcome = "outcome",
  outcomeLevel = "Dead",
  contexpl = "age_years",
  findcut = TRUE,
  sc = TRUE,
  kmunicate = TRUE
)

# ═══ EXAMPLE 7: Multiple Cutoffs (Risk Stratification) ═══
data(survivalcont_multicut)
survivalcont(
  data = survivalcont_multicut,
  elapsedtime = "time_months",
  outcome = "outcome",
  outcomeLevel = "Dead",
  contexpl = "risk_score",
  multiple_cutoffs = TRUE,
  num_cutoffs = "two",
  cutoff_method = "quantile",
  sc = TRUE
)

# ═══ EXAMPLE 8: Gene Expression Analysis ═══
data(survivalcont_expression)
survivalcont(
  data = survivalcont_expression,
  elapsedtime = "time_months",
  outcome = "outcome",
  outcomeLevel = "Dead",
  contexpl = "gene_expression",
  findcut = TRUE,
  sc = TRUE
)

# ═══ EXAMPLE 9: Landmark Analysis ═══
data(survivalcont_landmark)
survivalcont(
  data = survivalcont_landmark,
  elapsedtime = "time_months",
  outcome = "outcome",
  outcomeLevel = "Dead",
  contexpl = "early_response_score",
  uselandmark = TRUE,
  landmark = 6,
  findcut = TRUE,
  sc = TRUE
)

# ═══ EXAMPLE 10: Complete Analysis with All Features ═══
data(survivalcont_test)
survivalcont(
  data = survivalcont_test,
  elapsedtime = "time_months",
  outcome = "outcome",
  outcomeLevel = "Dead",
  contexpl = "biomarker",
  findcut = TRUE,
  multiple_cutoffs = TRUE,
  num_cutoffs = "two",
  cutoff_method = "recursive",
  min_group_size = 10,
  cutp = "12, 24, 36, 48, 60",
  sc = TRUE,
  kmunicate = TRUE,
  ce = TRUE,
  ch = TRUE
)
