# ═══════════════════════════════════════════════════════════
# Example Usage: jjbetweenstats (Box-Violin Plots Between Groups)
# ═══════════════════════════════════════════════════════════
#
# This file demonstrates comprehensive usage of the jjbetweenstats jamovi function
# which creates box-violin plots for comparing continuous variables between groups.
#
# Created: 2026-01-05
# Test data: jjbetweenstats_test, jjbetweenstats_twogroup, jjbetweenstats_outliers,
#            jjbetweenstats_skewed, jjbetweenstats_fourgroup

library(ClinicoPath)

# ═══════════════════════════════════════════════════════════
# Example 1: Basic Three-Group Comparison (ANOVA)
# ═══════════════════════════════════════════════════════════
# Scenario: Compare tumor reduction across treatment groups

data(jjbetweenstats_test, package = "ClinicoPath")

jjbetweenstats(
  data = jjbetweenstats_test,
  dep = "tumor_reduction",
  group = "treatment",
  typestatistics = "parametric"
)

# ═══════════════════════════════════════════════════════════
# Example 2: With Pairwise Comparisons
# ═══════════════════════════════════════════════════════════
# Scenario: Post-hoc tests for treatment differences

jjbetweenstats(
  data = jjbetweenstats_test,
  dep = "tumor_reduction",
  group = "treatment",
  pairwisecomparisons = TRUE,
  padjustmethod = "bonferroni",
  centralityplotting = TRUE,
  centralitytype = "parametric"
)

# ═══════════════════════════════════════════════════════════
# Example 3: Split By Sex (Grouped Analysis)
# ═══════════════════════════════════════════════════════════
# Scenario: Treatment effect stratified by sex

jjbetweenstats(
  data = jjbetweenstats_test,
  dep = "qol_score",
  group = "treatment",
  grvar = "sex",
  typestatistics = "parametric",
  pairwisecomparisons = TRUE
)

# ═══════════════════════════════════════════════════════════
# Example 4: Multiple Dependent Variables
# ═══════════════════════════════════════════════════════════
# Scenario: Analyze multiple endpoints simultaneously

jjbetweenstats(
  data = jjbetweenstats_test,
  dep = c("tumor_reduction", "pain_score", "qol_score"),
  group = "treatment",
  pairwisecomparisons = TRUE,
  padjustmethod = "holm",
  multiEndpointCorrection = "bonferroni"
)

# ═══════════════════════════════════════════════════════════
# Example 5: Two-Group Comparison (t-test)
# ═══════════════════════════════════════════════════════════
# Scenario: Simple control vs treatment comparison

data(jjbetweenstats_twogroup, package = "ClinicoPath")

jjbetweenstats(
  data = jjbetweenstats_twogroup,
  dep = "outcome",
  group = "group",
  typestatistics = "parametric",
  centralityplotting = TRUE,
  mytitle = "Treatment vs Control Comparison",
  xtitle = "Group",
  ytitle = "Outcome Score"
)

# ═══════════════════════════════════════════════════════════
# Example 6: Nonparametric Test for Skewed Data
# ═══════════════════════════════════════════════════════════
# Scenario: Kruskal-Wallis test for non-normal distributions

data(jjbetweenstats_skewed, package = "ClinicoPath")

jjbetweenstats(
  data = jjbetweenstats_skewed,
  dep = "tumor_marker",
  group = "treatment",
  typestatistics = "nonparametric",
  pairwisecomparisons = TRUE,
  centralityplotting = TRUE,
  centralitytype = "nonparametric"  # Show medians
)

# ═══════════════════════════════════════════════════════════
# Example 7: Robust Statistics with Outliers
# ═══════════════════════════════════════════════════════════
# Scenario: Trimmed means for outlier-contaminated data

data(jjbetweenstats_outliers, package = "ClinicoPath")

jjbetweenstats(
  data = jjbetweenstats_outliers,
  dep = "tumor_reduction",
  group = "treatment",
  typestatistics = "robust",
  centralityplotting = TRUE,
  centralitytype = "robust",
  mytitle = "Robust Analysis with Outliers"
)

# ═══════════════════════════════════════════════════════════
# Example 8: Bayesian Analysis with Bayes Factor
# ═══════════════════════════════════════════════════════════
# Scenario: Evidence-based comparison using Bayesian methods

jjbetweenstats(
  data = jjbetweenstats_test,
  dep = "biomarker_level",
  group = "treatment",
  typestatistics = "bayes",
  bfmessage = TRUE,
  centralityplotting = TRUE,
  centralitytype = "bayes"
)

# ═══════════════════════════════════════════════════════════
# Example 9: Four-Group Comparison (Multi-Level ANOVA)
# ═══════════════════════════════════════════════════════════
# Scenario: Compare four different treatments

data(jjbetweenstats_fourgroup, package = "ClinicoPath")

jjbetweenstats(
  data = jjbetweenstats_fourgroup,
  dep = "efficacy_score",
  group = "treatment",
  typestatistics = "parametric",
  pairwisecomparisons = TRUE,
  padjustmethod = "holm",
  mytitle = "Efficacy Comparison Across Four Treatment Arms"
)

# ═══════════════════════════════════════════════════════════
# Example 10: Split By Age Group
# ═══════════════════════════════════════════════════════════
# Scenario: Treatment effect stratified by age

jjbetweenstats(
  data = jjbetweenstats_test,
  dep = "tumor_reduction",
  group = "treatment",
  grvar = "age_group",
  typestatistics = "parametric",
  pairwisecomparisons = TRUE
)

# ═══════════════════════════════════════════════════════════
# Example 11: Publication-Ready Figure with ggpubr
# ═══════════════════════════════════════════════════════════
# Scenario: High-quality figure for manuscript

jjbetweenstats(
  data = jjbetweenstats_test,
  dep = "tumor_reduction",
  group = "treatment",
  typestatistics = "parametric",
  pairwisecomparisons = TRUE,
  addGGPubrPlot = TRUE,
  ggpubrPlotType = "boxviolin",
  ggpubrPalette = "jco",
  ggpubrAddStats = TRUE,
  mytitle = "Tumor Response by Treatment Arm",
  xtitle = "Treatment Group",
  ytitle = "Tumor Size Reduction (mm)"
)

# ═══════════════════════════════════════════════════════════
# Example 12: Welch's Test (Unequal Variances)
# ═══════════════════════════════════════════════════════════
# Scenario: Groups with heterogeneous variances

jjbetweenstats(
  data = jjbetweenstats_test,
  dep = "pain_score",
  group = "treatment",
  typestatistics = "parametric",
  varequal = FALSE,  # Welch's ANOVA
  pairwisecomparisons = TRUE
)

# ═══════════════════════════════════════════════════════════
# Example 13: Colorblind-Safe Palette
# ═══════════════════════════════════════════════════════════
# Scenario: Accessible visualization for publications

jjbetweenstats(
  data = jjbetweenstats_test,
  dep = "qol_score",
  group = "treatment",
  colorblindSafe = TRUE,
  centralityplotting = TRUE
)

# ═══════════════════════════════════════════════════════════
# Example 14: Custom Plot Dimensions
# ═══════════════════════════════════════════════════════════
# Scenario: Adjust plot size for presentations

jjbetweenstats(
  data = jjbetweenstats_test,
  dep = "tumor_reduction",
  group = "treatment",
  plotwidth = 800,
  plotheight = 600,
  mytitle = "Custom Size Plot"
)

# ═══════════════════════════════════════════════════════════
# Example 15: Comprehensive Clinical Report
# ═══════════════════════════════════════════════════════════
# Scenario: Full analysis with all outputs

jjbetweenstats(
  data = jjbetweenstats_test,
  dep = c("tumor_reduction", "pain_score", "qol_score", "biomarker_level"),
  group = "treatment",
  grvar = "tumor_stage",
  typestatistics = "parametric",
  pairwisecomparisons = TRUE,
  padjustmethod = "bonferroni",
  centralityplotting = TRUE,
  centralitytype = "parametric",
  multiEndpointCorrection = "bonferroni",
  showexplanations = TRUE,
  mytitle = "Comprehensive Clinical Trial Analysis",
  k = 2,
  conflevel = 0.95
)
