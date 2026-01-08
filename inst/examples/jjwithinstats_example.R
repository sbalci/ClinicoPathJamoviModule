# ═══════════════════════════════════════════════════════════
# Example Usage: jjwithinstats (Within-Subjects Box-Violin Plots)
# ═══════════════════════════════════════════════════════════
#
# This file demonstrates comprehensive usage of the jjwithinstats jamovi function
# which creates box-violin plots for within-subjects/repeated measures comparisons.
#
# Created: 2026-01-05
# Test data: jjwithinstats_test, jjwithinstats_biomarker, jjwithinstats_paired,
#            jjwithinstats_laboratory, jjwithinstats_qol, jjwithinstats_symptoms

library(ClinicoPath)

# ═══════════════════════════════════════════════════════════
# Example 1: Basic Three-Timepoint Analysis
# ═══════════════════════════════════════════════════════════
# Scenario: Tumor size reduction over 12 weeks

data(jjwithinstats_test, package = "ClinicoPath")

jjwithinstats(
  data = jjwithinstats_test,
  dep1 = "baseline",
  dep2 = "week4",
  dep3 = "week12",
  typestatistics = "parametric"
)

# ═══════════════════════════════════════════════════════════
# Example 2: With Individual Trajectories and Pairwise Tests
# ═══════════════════════════════════════════════════════════
# Scenario: Show patient-level changes with post-hoc comparisons

jjwithinstats(
  data = jjwithinstats_test,
  dep1 = "baseline",
  dep2 = "week4",
  dep3 = "week12",
  pairwisecomparisons = TRUE,
  padjustmethod = "bonferroni",
  pointpath = TRUE,  # Show individual patient trajectories
  centralityplotting = TRUE,
  centralitytype = "parametric",
  mytitle = "Tumor Response Over 12 Weeks"
)

# ═══════════════════════════════════════════════════════════
# Example 3: Two-Timepoint Paired Analysis
# ═══════════════════════════════════════════════════════════
# Scenario: Pre-post treatment comparison (paired t-test)

data(jjwithinstats_paired, package = "ClinicoPath")

jjwithinstats(
  data = jjwithinstats_paired,
  dep1 = "pre_treatment",
  dep2 = "post_treatment",
  typestatistics = "parametric",
  pointpath = TRUE,
  centralityplotting = TRUE,
  mytitle = "Pain Reduction: Pre vs Post Treatment",
  xtitle = "Assessment Time",
  ytitle = "Pain Score (VAS 0-100)"
)

# ═══════════════════════════════════════════════════════════
# Example 4: Four-Timepoint Biomarker Tracking (Nonparametric)
# ═══════════════════════════════════════════════════════════
# Scenario: Biomarker trajectory with log-normal distribution

data(jjwithinstats_biomarker, package = "ClinicoPath")

jjwithinstats(
  data = jjwithinstats_biomarker,
  dep1 = "month0",
  dep2 = "month1",
  dep3 = "month3",
  dep4 = "month6",
  clinicalpreset = "biomarker",
  typestatistics = "nonparametric",  # Friedman test
  pairwisecomparisons = TRUE,
  padjustmethod = "holm",
  centralityplotting = TRUE,
  centralitytype = "nonparametric",  # Show medians
  mytitle = "Biomarker Decline Over 6 Months"
)

# ═══════════════════════════════════════════════════════════
# Example 5: Laboratory Values with Outliers (Robust)
# ═══════════════════════════════════════════════════════════
# Scenario: Lab monitoring with outlier-contaminated data

data(jjwithinstats_laboratory, package = "ClinicoPath")

jjwithinstats(
  data = jjwithinstats_laboratory,
  dep1 = "baseline_lab",
  dep2 = "week2_lab",
  dep3 = "week4_lab",
  dep4 = "week8_lab",
  clinicalpreset = "laboratory",
  typestatistics = "robust",  # Trimmed means
  centralityplotting = TRUE,
  centralitytype = "robust",
  mytitle = "Laboratory Value Monitoring (Robust Analysis)"
)

# ═══════════════════════════════════════════════════════════
# Example 6: Bayesian Repeated Measures
# ═══════════════════════════════════════════════════════════
# Scenario: Evidence-based analysis with Bayes Factor

jjwithinstats(
  data = jjwithinstats_test,
  dep1 = "baseline",
  dep2 = "week4",
  dep3 = "week12",
  typestatistics = "bayes",
  bfmessage = TRUE,
  centralityplotting = TRUE,
  centralitytype = "bayes",
  mytitle = "Bayesian Repeated Measures Analysis"
)

# ═══════════════════════════════════════════════════════════
# Example 7: Quality of Life Trajectory
# ═══════════════════════════════════════════════════════════
# Scenario: QoL improvement with centrality path

data(jjwithinstats_qol, package = "ClinicoPath")

jjwithinstats(
  data = jjwithinstats_qol,
  dep1 = "qol_baseline",
  dep2 = "qol_month1",
  dep3 = "qol_month3",
  clinicalpreset = "treatment",
  pairwisecomparisons = TRUE,
  padjustmethod = "bonferroni",
  pointpath = TRUE,
  centralitypath = TRUE,  # Connect centrality points
  centralityplotting = TRUE,
  mytitle = "Quality of Life Improvement Over Treatment",
  ytitle = "QoL Score (0-100)"
)

# ═══════════════════════════════════════════════════════════
# Example 8: Symptom Severity Over Time
# ═══════════════════════════════════════════════════════════
# Scenario: Four-timepoint symptom tracking

data(jjwithinstats_symptoms, package = "ClinicoPath")

jjwithinstats(
  data = jjwithinstats_symptoms,
  dep1 = "symptom_pre",
  dep2 = "symptom_week4",
  dep3 = "symptom_week8",
  dep4 = "symptom_week12",
  typestatistics = "parametric",
  pairwisecomparisons = TRUE,
  pointpath = TRUE,
  violin = TRUE,
  boxplot = TRUE,
  point = TRUE,
  mytitle = "Symptom Severity Over 12 Weeks",
  ytitle = "Symptom Severity (1-10 scale)"
)

# ═══════════════════════════════════════════════════════════
# Example 9: Publication-Ready with ggpubr (Paired Lines)
# ═══════════════════════════════════════════════════════════
# Scenario: Manuscript-quality figure with individual lines

jjwithinstats(
  data = jjwithinstats_test,
  dep1 = "baseline",
  dep2 = "week4",
  dep3 = "week12",
  addGGPubrPlot = TRUE,
  ggpubrPlotType = "paired",  # Shows individual connecting lines
  ggpubrPalette = "jco",
  ggpubrShowLines = TRUE,
  ggpubrAddStats = TRUE,
  mytitle = "Tumor Response: Publication Figure",
  xtitle = "Assessment Time",
  ytitle = "Tumor Size (mm)"
)

# ═══════════════════════════════════════════════════════════
# Example 10: ggpubr Box Plot Variant
# ═══════════════════════════════════════════════════════════
# Scenario: Alternative visualization with box plots

jjwithinstats(
  data = jjwithinstats_paired,
  dep1 = "pre_treatment",
  dep2 = "post_treatment",
  addGGPubrPlot = TRUE,
  ggpubrPlotType = "boxplot",
  ggpubrPalette = "lancet",
  ggpubrAddStats = TRUE,
  ggpubrAddPoints = TRUE,
  mytitle = "Pre-Post Pain Comparison (ggpubr)"
)

# ═══════════════════════════════════════════════════════════
# Example 11: Centrality Path with No Individual Paths
# ═══════════════════════════════════════════════════════════
# Scenario: Focus on mean trajectory only

jjwithinstats(
  data = jjwithinstats_test,
  dep1 = "baseline",
  dep2 = "week4",
  dep3 = "week12",
  pointpath = FALSE,  # Hide individual paths
  centralitypath = TRUE,  # Show mean trajectory
  centralityplotting = TRUE,
  centralitytype = "parametric",
  mytitle = "Mean Tumor Size Trajectory"
)

# ═══════════════════════════════════════════════════════════
# Example 12: Different Adjustment Methods Comparison
# ═══════════════════════════════════════════════════════════
# Scenario: Compare Bonferroni vs Holm adjustments

# Bonferroni (conservative)
jjwithinstats(
  data = jjwithinstats_test,
  dep1 = "baseline",
  dep2 = "week4",
  dep3 = "week12",
  pairwisecomparisons = TRUE,
  padjustmethod = "bonferroni",
  mytitle = "Bonferroni Adjustment"
)

# Holm (less conservative)
jjwithinstats(
  data = jjwithinstats_test,
  dep1 = "baseline",
  dep2 = "week4",
  dep3 = "week12",
  pairwisecomparisons = TRUE,
  padjustmethod = "holm",
  mytitle = "Holm Adjustment"
)

# ═══════════════════════════════════════════════════════════
# Example 13: Colorblind-Safe Palette
# ═══════════════════════════════════════════════════════════
# Scenario: Accessible visualization

jjwithinstats(
  data = jjwithinstats_qol,
  dep1 = "qol_baseline",
  dep2 = "qol_month1",
  dep3 = "qol_month3",
  colorblindSafe = TRUE,
  centralityplotting = TRUE,
  mytitle = "Colorblind-Safe QoL Plot"
)

# ═══════════════════════════════════════════════════════════
# Example 14: Custom Plot Dimensions
# ═══════════════════════════════════════════════════════════
# Scenario: Adjust plot size for presentations

jjwithinstats(
  data = jjwithinstats_test,
  dep1 = "baseline",
  dep2 = "week4",
  dep3 = "week12",
  plotwidth = 800,
  plotheight = 600,
  mytitle = "Custom Size Plot for Presentation"
)

# ═══════════════════════════════════════════════════════════
# Example 15: Comprehensive Clinical Report
# ═══════════════════════════════════════════════════════════
# Scenario: Full analysis with all outputs and explanations

jjwithinstats(
  data = jjwithinstats_test,
  dep1 = "baseline",
  dep2 = "week4",
  dep3 = "week12",
  typestatistics = "parametric",
  pairwisecomparisons = TRUE,
  padjustmethod = "bonferroni",
  centralityplotting = TRUE,
  centralitytype = "parametric",
  pointpath = TRUE,
  centralitypath = TRUE,
  showexplanations = TRUE,
  resultssubtitle = TRUE,
  mytitle = "Comprehensive Tumor Response Analysis",
  xtitle = "Assessment Time",
  ytitle = "Tumor Size (mm)",
  k = 2,
  conflevel = 0.95
)
