# ═══════════════════════════════════════════════════════════
# Example Usage: jjdotplotstats (Dot Chart for Group Comparisons)
# ═══════════════════════════════════════════════════════════
#
# This file demonstrates comprehensive usage of the jjdotplotstats jamovi function
# which creates dot-style comparisons of continuous variables between groups.
#
# Created: 2026-01-05
# Test data: jjdotplotstats_test, jjdotplotstats_twogroup, jjdotplotstats_fourgroup,
#            jjdotplotstats_skewed, jjdotplotstats_outliers, jjdotplotstats_reference,
#            jjdotplotstats_qol, jjdotplotstats_labvalues

library(ClinicoPath)

# ═══════════════════════════════════════════════════════════
# Example 1: Basic Three-Group Comparison
# ═══════════════════════════════════════════════════════════
# Scenario: Tumor reduction across treatment groups

data(jjdotplotstats_test, package = "ClinicoPath")

jjdotplotstats(
  data = jjdotplotstats_test,
  dep = "tumor_reduction",
  group = "treatment",
  typestatistics = "parametric"
)

# ═══════════════════════════════════════════════════════════
# Example 2: With Centrality Lines and Statistics
# ═══════════════════════════════════════════════════════════
# Scenario: Show group means with statistical results

jjdotplotstats(
  data = jjdotplotstats_test,
  dep = "tumor_reduction",
  group = "treatment",
  centralityplotting = TRUE,
  centralitytype = "parametric",
  resultssubtitle = TRUE,
  mytitle = "Tumor Response by Treatment",
  xtitle = "Tumor Reduction (%)",
  ytitle = "Treatment Group"
)

# ═══════════════════════════════════════════════════════════
# Example 3: Two-Group Pre-Post Comparison
# ═══════════════════════════════════════════════════════════
# Scenario: Pain before and after treatment

data(jjdotplotstats_twogroup, package = "ClinicoPath")

jjdotplotstats(
  data = jjdotplotstats_twogroup,
  dep = "pain_score",
  group = "timepoint",
  typestatistics = "parametric",
  centralityplotting = TRUE,
  centralitytype = "parametric",
  resultssubtitle = TRUE,
  mytitle = "Pain Reduction: Pre vs Post Treatment",
  xtitle = "Pain Score (0-10 VAS)"
)

# ═══════════════════════════════════════════════════════════
# Example 4: Four-Group Dose-Response
# ═══════════════════════════════════════════════════════════
# Scenario: Efficacy across dose levels

data(jjdotplotstats_fourgroup, package = "ClinicoPath")

jjdotplotstats(
  data = jjdotplotstats_fourgroup,
  dep = "efficacy_score",
  group = "dose",
  typestatistics = "parametric",
  effsizetype = "unbiased",  # Hedge's g for small samples
  centralityplotting = TRUE,
  resultssubtitle = TRUE,
  mytitle = "Dose-Response Relationship"
)

# ═══════════════════════════════════════════════════════════
# Example 5: Nonparametric for Skewed Data
# ═══════════════════════════════════════════════════════════
# Scenario: Log-normal biomarker requiring Mann-Whitney U

data(jjdotplotstats_skewed, package = "ClinicoPath")

jjdotplotstats(
  data = jjdotplotstats_skewed,
  dep = "biomarker_level",
  group = "treatment",
  typestatistics = "nonparametric",
  centralityplotting = TRUE,
  centralitytype = "nonparametric",  # Show medians
  resultssubtitle = TRUE,
  mytitle = "Biomarker Response (Nonparametric)"
)

# ═══════════════════════════════════════════════════════════
# Example 6: Robust Statistics with Outliers
# ═══════════════════════════════════════════════════════════
# Scenario: Data with extreme values

data(jjdotplotstats_outliers, package = "ClinicoPath")

jjdotplotstats(
  data = jjdotplotstats_outliers,
  dep = "response",
  group = "group",
  typestatistics = "robust",
  centralityplotting = TRUE,
  centralitytype = "robust",  # Trimmed means
  mytitle = "Robust Analysis with Outliers"
)

# ═══════════════════════════════════════════════════════════
# Example 7: Testing Against Reference Value
# ═══════════════════════════════════════════════════════════
# Scenario: BP reduction vs clinical threshold (10 mmHg)

data(jjdotplotstats_reference, package = "ClinicoPath")

jjdotplotstats(
  data = jjdotplotstats_reference,
  dep = "bp_reduction",
  group = "drug",
  testvalue = 10,  # Clinical threshold
  testvalueline = TRUE,
  centralityplotting = TRUE,
  resultssubtitle = TRUE,
  mytitle = "BP Reduction vs Target (≥10 mmHg)",
  xtitle = "Blood Pressure Reduction (mmHg)"
)

# ═══════════════════════════════════════════════════════════
# Example 8: Grouped Analysis by Hospital Site
# ═══════════════════════════════════════════════════════════
# Scenario: Treatment effects stratified by site

jjdotplotstats(
  data = jjdotplotstats_test,
  dep = "tumor_reduction",
  group = "treatment",
  grvar = "hospital",
  centralityplotting = TRUE,
  typestatistics = "parametric"
)

# ═══════════════════════════════════════════════════════════
# Example 9: Bayesian Analysis with Bayes Factor
# ═══════════════════════════════════════════════════════════
# Scenario: Evidence-based group comparison

jjdotplotstats(
  data = jjdotplotstats_test,
  dep = "tumor_reduction",
  group = "treatment",
  typestatistics = "bayes",
  bfmessage = TRUE,
  centralityplotting = TRUE,
  mytitle = "Bayesian Group Comparison"
)

# ═══════════════════════════════════════════════════════════
# Example 10: Publication-Ready Figure
# ═══════════════════════════════════════════════════════════
# Scenario: Comprehensive manuscript figure

jjdotplotstats(
  data = jjdotplotstats_test,
  dep = "tumor_reduction",
  group = "treatment",
  typestatistics = "parametric",
  effsizetype = "biased",  # Cohen's d
  centralityplotting = TRUE,
  centralitytype = "parametric",
  centralityparameter = "mean",
  resultssubtitle = TRUE,
  mytitle = "Tumor Response Across Treatment Arms",
  xtitle = "Tumor Reduction (%)",
  ytitle = "Treatment Group",
  conflevel = 0.95,
  k = 2,
  plotwidth = 800,
  plotheight = 600
)

# ═══════════════════════════════════════════════════════════
# Example 11: Quality of Life with Effect Sizes
# ═══════════════════════════════════════════════════════════
# Scenario: QoL improvement with multiple effect size measures

data(jjdotplotstats_qol, package = "ClinicoPath")

jjdotplotstats(
  data = jjdotplotstats_qol,
  dep = "qol_score",
  group = "intervention",
  typestatistics = "parametric",
  effsizetype = "eta",  # Eta-squared
  centralityplotting = TRUE,
  resultssubtitle = TRUE,
  mytitle = "Quality of Life by Intervention"
)

# ═══════════════════════════════════════════════════════════
# Example 12: Lab Values with Median Display
# ═══════════════════════════════════════════════════════════
# Scenario: Hemoglobin levels across anemia groups

data(jjdotplotstats_labvalues, package = "ClinicoPath")

jjdotplotstats(
  data = jjdotplotstats_labvalues,
  dep = "hemoglobin",
  group = "anemia",
  typestatistics = "parametric",
  centralityplotting = TRUE,
  centralityparameter = "median",  # Show median lines
  mytitle = "Hemoglobin by Anemia Severity",
  xtitle = "Hemoglobin (g/dL)"
)

# ═══════════════════════════════════════════════════════════
# Example 13: Grouped by Gender
# ═══════════════════════════════════════════════════════════
# Scenario: Treatment effects stratified by gender

jjdotplotstats(
  data = jjdotplotstats_twogroup,
  dep = "pain_score",
  group = "timepoint",
  grvar = "gender",
  centralityplotting = TRUE,
  typestatistics = "parametric"
)

# ═══════════════════════════════════════════════════════════
# Example 14: High Precision Display
# ═══════════════════════════════════════════════════════════
# Scenario: Show results with 3 decimal places

jjdotplotstats(
  data = jjdotplotstats_reference,
  dep = "bp_reduction",
  group = "drug",
  k = 3,  # 3 decimal places
  centralityk = 3,
  resultssubtitle = TRUE,
  mytitle = "High Precision Results"
)

# ═══════════════════════════════════════════════════════════
# Example 15: Custom Confidence Level
# ═══════════════════════════════════════════════════════════
# Scenario: 99% confidence intervals

jjdotplotstats(
  data = jjdotplotstats_fourgroup,
  dep = "efficacy_score",
  group = "dose",
  conflevel = 0.99,  # 99% CI
  resultssubtitle = TRUE,
  mytitle = "Efficacy with 99% Confidence Intervals"
)
