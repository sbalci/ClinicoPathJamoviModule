# ═══════════════════════════════════════════════════════════
# Example Usage: jjpiestats
# ═══════════════════════════════════════════════════════════
#
# Comprehensive examples for the jjpiestats jamovi function
# Generated: 2026-01-06

library(ClinicoPath)

# Load test datasets
data(jjpiestats_test)
data(jjpiestats_diagnostic)
data(jjpiestats_treatment)
data(jjpiestats_biomarker)

# ═══════════════════════════════════════════════════════════
# BASIC PIE CHARTS
# ═══════════════════════════════════════════════════════════

# Example 1: Simple categorical distribution
jjpiestats(
  data = jjpiestats_test,
  dep = "treatment_response"
)

# Example 2: With percentage labels
jjpiestats(
  data = jjpiestats_test,
  dep = "disease_severity",
  label = "percentage"
)

# Example 3: With count labels
jjpiestats(
  data = jjpiestats_test,
  dep = "tumor_stage",
  label = "counts"
)

# Example 4: With both percentage and counts
jjpiestats(
  data = jjpiestats_test,
  dep = "treatment_response",
  label = "both"
)

# ═══════════════════════════════════════════════════════════
# CONTINGENCY TABLE ANALYSIS
# ═══════════════════════════════════════════════════════════

# Example 5: Response by treatment arm (parametric test)
jjpiestats(
  data = jjpiestats_test,
  dep = "treatment_response",
  group = "treatment_arm",
  typestatistics = "parametric",
  resultssubtitle = TRUE
)

# Example 6: Disease severity by gender (nonparametric test)
jjpiestats(
  data = jjpiestats_test,
  dep = "disease_severity",
  group = "gender",
  typestatistics = "nonparametric",
  resultssubtitle = TRUE
)

# Example 7: Bayesian analysis
jjpiestats(
  data = jjpiestats_test,
  dep = "treatment_response",
  group = "gender",
  typestatistics = "bayes",
  resultssubtitle = TRUE,
  bfmessage = TRUE
)

# ═══════════════════════════════════════════════════════════
# DIAGNOSTIC TEST EVALUATION
# ═══════════════════════════════════════════════════════════

# Example 8: 2×2 diagnostic test table
jjpiestats(
  data = jjpiestats_diagnostic,
  dep = "test_result",
  group = "disease_status",
  clinicalpreset = "diagnostic",
  resultssubtitle = TRUE,
  showSummary = TRUE
)

# Example 9: Diagnostic test with assumptions
jjpiestats(
  data = jjpiestats_diagnostic,
  dep = "test_result",
  group = "disease_status",
  clinicalpreset = "diagnostic",
  showAssumptions = TRUE,
  showInterpretation = TRUE
)

# Example 10: Diagnostic test stratified by clinical site
jjpiestats(
  data = jjpiestats_diagnostic,
  dep = "test_result",
  group = "disease_status",
  grvar = "clinical_site",
  clinicalpreset = "diagnostic",
  label = "both"
)

# ═══════════════════════════════════════════════════════════
# TREATMENT COMPARISON
# ═══════════════════════════════════════════════════════════

# Example 11: Treatment outcomes
jjpiestats(
  data = jjpiestats_treatment,
  dep = "outcome",
  group = "treatment",
  clinicalpreset = "treatment",
  resultssubtitle = TRUE
)

# Example 12: Treatment outcomes with donut chart
jjpiestats(
  data = jjpiestats_treatment,
  dep = "outcome",
  group = "treatment",
  clinicalpreset = "treatment",
  addGGPubrDonut = TRUE,
  ggpubrDonutPalette = "jco"
)

# Example 13: Treatment outcomes stratified by study site
jjpiestats(
  data = jjpiestats_treatment,
  dep = "outcome",
  group = "treatment",
  grvar = "study_site",
  typestatistics = "parametric",
  resultssubtitle = TRUE
)

# ═══════════════════════════════════════════════════════════
# BIOMARKER DISTRIBUTION
# ═══════════════════════════════════════════════════════════

# Example 14: Biomarker expression levels
jjpiestats(
  data = jjpiestats_biomarker,
  dep = "expression_level",
  clinicalpreset = "biomarker"
)

# Example 15: Biomarker by receptor status
jjpiestats(
  data = jjpiestats_biomarker,
  dep = "expression_level",
  group = "receptor_status",
  clinicalpreset = "biomarker",
  resultssubtitle = TRUE
)

# Example 16: Biomarker stratified by cancer type
jjpiestats(
  data = jjpiestats_biomarker,
  dep = "expression_level",
  grvar = "cancer_type",
  clinicalpreset = "biomarker",
  label = "percentage"
)

# ═══════════════════════════════════════════════════════════
# GROUPED/SPLIT ANALYSIS
# ═══════════════════════════════════════════════════════════

# Example 17: Disease severity by hospital site
jjpiestats(
  data = jjpiestats_test,
  dep = "disease_severity",
  grvar = "hospital_site",
  typestatistics = "parametric"
)

# Example 18: Response by treatment, split by hospital
jjpiestats(
  data = jjpiestats_test,
  dep = "treatment_response",
  group = "treatment_arm",
  grvar = "hospital_site",
  resultssubtitle = TRUE
)

# ═══════════════════════════════════════════════════════════
# ADVANCED FEATURES
# ═══════════════════════════════════════════════════════════

# Example 19: Publication-ready with all features
jjpiestats(
  data = jjpiestats_diagnostic,
  dep = "test_result",
  group = "disease_status",
  typestatistics = "parametric",
  clinicalpreset = "diagnostic",
  label = "both",
  resultssubtitle = TRUE,
  showSummary = TRUE,
  showAssumptions = TRUE,
  showInterpretation = TRUE,
  showexplanations = TRUE,
  conflevel = 0.95,
  digits = 3
)

# Example 20: Complete treatment analysis with donut
jjpiestats(
  data = jjpiestats_treatment,
  dep = "outcome",
  group = "treatment",
  grvar = "study_site",
  typestatistics = "parametric",
  clinicalpreset = "treatment",
  label = "both",
  resultssubtitle = TRUE,
  showSummary = TRUE,
  showInterpretation = TRUE,
  addGGPubrDonut = TRUE,
  ggpubrDonutPalette = "jco",
  conflevel = 0.95,
  digits = 2
)
