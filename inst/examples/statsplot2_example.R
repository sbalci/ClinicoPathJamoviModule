# ═══════════════════════════════════════════════════════════
# Example Usage: statsplot2 (Automatic Plot Selection)
# ═══════════════════════════════════════════════════════════
#
# This file demonstrates comprehensive usage of the statsplot2 jamovi function
# which automatically selects appropriate plot types based on variable data types.
#
# Created: 2026-01-05
# Test data: statsplot2_test, statsplot2_clinical, statsplot2_repeated,
#            statsplot2_outliers, statsplot2_skewed

library(ClinicoPath)

# ═══════════════════════════════════════════════════════════
# Example 1: Basic Continuous vs Categorical (Violin Plot)
# ═══════════════════════════════════════════════════════════
# Automatic selection: Violin plot with statistical comparison
# Scenario: Compare tumor reduction across treatment groups

data(statsplot2_test, package = "ClinicoPath")

statsplot2(
  data = statsplot2_test,
  dep = "tumor_reduction",
  group = "treatment",
  direction = "independent",
  distribution = "p"
)

# ═══════════════════════════════════════════════════════════
# Example 2: Categorical vs Categorical (Bar Chart)
# ═══════════════════════════════════════════════════════════
# Automatic selection: Grouped bar chart with proportions
# Scenario: Compare response rates across treatment arms

statsplot2(
  data = statsplot2_test,
  dep = "response_status",
  group = "treatment",
  direction = "independent"
)

# ═══════════════════════════════════════════════════════════
# Example 3: Continuous vs Continuous (Scatter Plot)
# ═══════════════════════════════════════════════════════════
# Automatic selection: Scatter plot with correlation
# Scenario: Biomarker level vs tumor reduction

statsplot2(
  data = statsplot2_test,
  dep = "tumor_reduction",
  group = "biomarker_level",
  direction = "independent",
  distribution = "p"
)

# ═══════════════════════════════════════════════════════════
# Example 4: Grouped/Faceted Plots by Split Variable
# ═══════════════════════════════════════════════════════════
# Automatic selection: Violin plots split by tumor stage
# Scenario: Treatment effect stratified by disease severity

statsplot2(
  data = statsplot2_test,
  dep = "pain_score",
  group = "treatment",
  grvar = "tumor_stage",
  direction = "independent",
  distribution = "np"  # Nonparametric for skewed pain scores
)

# ═══════════════════════════════════════════════════════════
# Example 5: Repeated Measures - Continuous Outcome
# ═══════════════════════════════════════════════════════════
# Automatic selection: Repeated measures plot with trajectories
# Scenario: Symptom severity change over time

data(statsplot2_repeated, package = "ClinicoPath")

statsplot2(
  data = statsplot2_repeated,
  dep = "symptom_severity",
  group = "timepoint",
  direction = "repeated",
  distribution = "p"
)

# ═══════════════════════════════════════════════════════════
# Example 6: Repeated Measures - Treatment Comparison
# ═══════════════════════════════════════════════════════════
# Automatic selection: Repeated measures with between-group factor
# Scenario: Treatment arm comparison over time

statsplot2(
  data = statsplot2_repeated,
  dep = "symptom_severity",
  group = "timepoint",
  grvar = "treatment_arm",
  direction = "repeated",
  distribution = "p"
)

# ═══════════════════════════════════════════════════════════
# Example 7: Nonparametric Analysis for Skewed Data
# ═══════════════════════════════════════════════════════════
# Distribution: Nonparametric (Mann-Whitney/Kruskal-Wallis)
# Scenario: Skewed biomarker distribution

data(statsplot2_skewed, package = "ClinicoPath")

statsplot2(
  data = statsplot2_skewed,
  dep = "tumor_reduction",
  group = "treatment",
  direction = "independent",
  distribution = "np"  # Nonparametric tests
)

# ═══════════════════════════════════════════════════════════
# Example 8: Robust Statistics with Outliers
# ═══════════════════════════════════════════════════════════
# Distribution: Robust (trimmed means)
# Scenario: Dataset with extreme values

data(statsplot2_outliers, package = "ClinicoPath")

statsplot2(
  data = statsplot2_outliers,
  dep = "tumor_reduction",
  group = "treatment",
  direction = "independent",
  distribution = "r"  # Robust statistics
)

# ═══════════════════════════════════════════════════════════
# Example 9: Bayesian Analysis with Bayes Factor
# ═══════════════════════════════════════════════════════════
# Distribution: Bayesian framework
# Scenario: Evidence-based comparison of treatment effects

statsplot2(
  data = statsplot2_test,
  dep = "qol_score",
  group = "treatment",
  direction = "independent",
  distribution = "bf"  # Bayesian with Bayes Factor
)

# ═══════════════════════════════════════════════════════════
# Example 10: Multi-Level Grouping with Sex and Age
# ═══════════════════════════════════════════════════════════
# Scenario: Subgroup analysis by demographics

statsplot2(
  data = statsplot2_test,
  dep = "tumor_reduction",
  group = "sex",
  grvar = "age_group",
  direction = "independent",
  distribution = "p"
)

# ═══════════════════════════════════════════════════════════
# Example 11: Clinical Trial Publication Figure
# ═══════════════════════════════════════════════════════════
# Scenario: High-quality figure for manuscript
# Features: Clean labels, appropriate statistics, professional appearance

statsplot2(
  data = statsplot2_test,
  dep = "tumor_reduction",
  group = "treatment",
  direction = "independent",
  distribution = "p",
  plotTitle = "Tumor Response by Treatment Arm",
  xlab = "Treatment Group",
  ylab = "Tumor Size Reduction (mm)"
)

# ═══════════════════════════════════════════════════════════
# Example 12: Comprehensive Clinical Report
# ═══════════════════════════════════════════════════════════
# Scenario: Multi-outcome analysis across all endpoints

# Primary outcome
primary_analysis <- statsplot2(
  data = statsplot2_test,
  dep = "tumor_reduction",
  group = "treatment",
  grvar = "tumor_stage",
  direction = "independent",
  distribution = "p"
)

# Secondary outcome 1: Pain scores
pain_analysis <- statsplot2(
  data = statsplot2_test,
  dep = "pain_score",
  group = "treatment",
  direction = "independent",
  distribution = "np"  # Nonparametric for VAS
)

# Secondary outcome 2: Quality of life
qol_analysis <- statsplot2(
  data = statsplot2_test,
  dep = "qol_score",
  group = "treatment",
  direction = "independent",
  distribution = "p"
)

# Categorical outcome: Response rates
response_analysis <- statsplot2(
  data = statsplot2_test,
  dep = "response_status",
  group = "treatment",
  direction = "independent"
)
