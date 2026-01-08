# ═══════════════════════════════════════════════════════════
# Example Usage: jjbarstats (Categorical vs Categorical Bar Charts)
# ═══════════════════════════════════════════════════════════
#
# This file demonstrates comprehensive usage of the jjbarstats jamovi function
# which creates bar charts for categorical variables with chi-square tests.
#
# Created: 2026-01-05
# Test data: jjbarstats_test, jjbarstats_diagnostic, jjbarstats_paired,
#            jjbarstats_aggregated, jjbarstats_biomarker

library(ClinicoPath)

# ═══════════════════════════════════════════════════════════
# Example 1: Basic Chi-Square Test (Treatment × Response)
# ═══════════════════════════════════════════════════════════
# Scenario: Compare response rates across treatment groups

data(jjbarstats_test, package = "ClinicoPath")

jjbarstats(
  data = jjbarstats_test,
  dep = "response",
  group = "treatment",
  typestatistics = "parametric"
)

# ═══════════════════════════════════════════════════════════
# Example 2: With Pairwise Comparisons
# ═══════════════════════════════════════════════════════════
# Scenario: Post-hoc tests for treatment group differences

jjbarstats(
  data = jjbarstats_test,
  dep = "response",
  group = "treatment",
  pairwisecomparisons = TRUE,
  padjustmethod = "holm",
  label = "both"  # Show counts and percentages
)

# ═══════════════════════════════════════════════════════════
# Example 3: Split By Sex (Grouped Bar Charts)
# ═══════════════════════════════════════════════════════════
# Scenario: Subgroup analysis by demographic factor

jjbarstats(
  data = jjbarstats_test,
  dep = "response",
  group = "treatment",
  grvar = "sex",
  typestatistics = "parametric"
)

# ═══════════════════════════════════════════════════════════
# Example 4: Diagnostic Test Analysis (2×2 Table)
# ═══════════════════════════════════════════════════════════
# Scenario: Evaluate diagnostic test performance
# Uses clinical preset for automatic configuration

data(jjbarstats_diagnostic, package = "ClinicoPath")

jjbarstats(
  data = jjbarstats_diagnostic,
  dep = "diagnosis",
  group = "test_result",
  clinicalpreset = "diagnostic",
  typestatistics = "parametric",
  showSummary = TRUE,
  showAssumptions = TRUE
)

# ═══════════════════════════════════════════════════════════
# Example 5: Paired/Repeated Measures (McNemar's Test)
# ═══════════════════════════════════════════════════════════
# Scenario: Before/after treatment status comparison

data(jjbarstats_paired, package = "ClinicoPath")

jjbarstats(
  data = jjbarstats_paired,
  dep = "baseline_status",
  group = "followup_status",
  paired = TRUE,
  label = "both"
)

# ═══════════════════════════════════════════════════════════
# Example 6: Aggregated Data with Counts
# ═══════════════════════════════════════════════════════════
# Scenario: Analyze pre-summarized contingency table

data(jjbarstats_aggregated, package = "ClinicoPath")

jjbarstats(
  data = jjbarstats_aggregated,
  dep = "response_category",
  group = "treatment_group",
  counts = "count",
  typestatistics = "parametric"
)

# ═══════════════════════════════════════════════════════════
# Example 7: Biomarker Expression Analysis
# ═══════════════════════════════════════════════════════════
# Scenario: HER2 status distribution across cancer subtypes

data(jjbarstats_biomarker, package = "ClinicoPath")

jjbarstats(
  data = jjbarstats_biomarker,
  dep = "her2_status",
  group = "subtype",
  clinicalpreset = "biomarker",
  typestatistics = "parametric",
  pairwisecomparisons = TRUE
)

# ═══════════════════════════════════════════════════════════
# Example 8: Risk Factor Analysis
# ═══════════════════════════════════════════════════════════
# Scenario: Association between smoking and disease recurrence

jjbarstats(
  data = jjbarstats_test,
  dep = "disease_status",
  group = "smoking_status",
  clinicalpreset = "riskfactor",
  proportiontest = TRUE,
  typestatistics = "parametric"
)

# ═══════════════════════════════════════════════════════════
# Example 9: Bayesian Analysis with Bayes Factor
# ═══════════════════════════════════════════════════════════
# Scenario: Evidence-based comparison of treatment effects

jjbarstats(
  data = jjbarstats_test,
  dep = "response",
  group = "treatment",
  typestatistics = "bayes",
  bfmessage = TRUE
)

# ═══════════════════════════════════════════════════════════
# Example 10: Proportion Test with Expected Ratios
# ═══════════════════════════════════════════════════════════
# Scenario: Test if response rates match expected distribution

jjbarstats(
  data = jjbarstats_test,
  dep = "response",
  group = "treatment",
  proportiontest = TRUE,
  ratio = "0.333,0.333,0.334",  # Equal proportions expected
  typestatistics = "parametric"
)

# ═══════════════════════════════════════════════════════════
# Example 11: Nonparametric Analysis (Fisher's Exact Test)
# ═══════════════════════════════════════════════════════════
# Scenario: Small sample size or sparse contingency table

# Create small sample subset
small_sample <- jjbarstats_test[1:30, ]

jjbarstats(
  data = small_sample,
  dep = "response",
  group = "treatment",
  typestatistics = "nonparametric"
)

# ═══════════════════════════════════════════════════════════
# Example 12: Multi-Level Grouping with Age Groups
# ═══════════════════════════════════════════════════════════
# Scenario: Treatment response stratified by age

jjbarstats(
  data = jjbarstats_test,
  dep = "response",
  group = "treatment",
  grvar = "age_group",
  typestatistics = "parametric",
  pairwisecomparisons = TRUE
)

# ═══════════════════════════════════════════════════════════
# Example 13: Publication-Quality Figure
# ═══════════════════════════════════════════════════════════
# Scenario: High-quality figure with GGStatsPlot theming

jjbarstats(
  data = jjbarstats_test,
  dep = "response",
  group = "treatment",
  originaltheme = TRUE,
  resultssubtitle = TRUE,
  label = "percentage",
  digitsperc = 1,
  conflevel = 0.95
)

# ═══════════════════════════════════════════════════════════
# Example 14: Balloon Plot for Contingency Table
# ═══════════════════════════════════════════════════════════
# Scenario: Alternative visualization using ggpubr balloon plot

jjbarstats(
  data = jjbarstats_biomarker,
  dep = "her2_status",
  group = "subtype",
  addGGPubrBalloon = TRUE,
  ggpubrBalloonPalette = "jco"
)

# ═══════════════════════════════════════════════════════════
# Example 15: Comprehensive Clinical Report
# ═══════════════════════════════════════════════════════════
# Scenario: Full analysis with all output options

jjbarstats(
  data = jjbarstats_test,
  dep = "response",
  group = "treatment",
  grvar = "tumor_stage",
  typestatistics = "parametric",
  pairwisecomparisons = TRUE,
  padjustmethod = "bonferroni",
  clinicalpreset = "treatment",
  showSummary = TRUE,
  showAssumptions = TRUE,
  showInterpretation = TRUE,
  showexplanations = TRUE,
  label = "both",
  digits = 2,
  digitsperc = 1,
  conflevel = 0.95
)
