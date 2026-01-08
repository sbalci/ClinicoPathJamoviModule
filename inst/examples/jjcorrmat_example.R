# ═══════════════════════════════════════════════════════════
# Example Usage: jjcorrmat (Correlation Matrix)
# ═══════════════════════════════════════════════════════════
#
# This file demonstrates comprehensive usage of the jjcorrmat jamovi function
# which creates correlation matrix visualizations with significance testing.
#
# Created: 2026-01-05
# Test data: jjcorrmat_test, jjcorrmat_biomarker, jjcorrmat_labvalues,
#            jjcorrmat_imaging, jjcorrmat_vitals, jjcorrmat_mixed

library(ClinicoPath)

# ═══════════════════════════════════════════════════════════
# Example 1: Basic Correlation Matrix (Pearson)
# ═══════════════════════════════════════════════════════════
# Scenario: Examine relationships among clinical tumor metrics

data(jjcorrmat_test, package = "ClinicoPath")

jjcorrmat(
  data = jjcorrmat_test,
  dep = c("tumor_size", "ki67_index", "mitotic_count",
          "necrosis_percent", "age", "bmi"),
  typestatistics = "parametric"
)

# ═══════════════════════════════════════════════════════════
# Example 2: With Multiple Testing Correction
# ═══════════════════════════════════════════════════════════
# Scenario: Control family-wise error rate with Bonferroni adjustment

jjcorrmat(
  data = jjcorrmat_test,
  dep = c("tumor_size", "ki67_index", "mitotic_count", "necrosis_percent"),
  typestatistics = "parametric",
  padjustmethod = "bonferroni",
  matrixtype = "upper",
  matrixmethod = "circle",
  siglevel = 0.05
)

# ═══════════════════════════════════════════════════════════
# Example 3: Grouped Correlation Matrix
# ═══════════════════════════════════════════════════════════
# Scenario: Compare correlations across tumor stages

jjcorrmat(
  data = jjcorrmat_test,
  dep = c("tumor_size", "ki67_index", "mitotic_count"),
  grvar = "tumor_stage",
  typestatistics = "parametric",
  matrixtype = "upper"
)

# ═══════════════════════════════════════════════════════════
# Example 4: Biomarker Correlations (Nonparametric)
# ═══════════════════════════════════════════════════════════
# Scenario: Tumor markers with log-normal distributions (Spearman's rho)

data(jjcorrmat_biomarker, package = "ClinicoPath")

jjcorrmat(
  data = jjcorrmat_biomarker,
  dep = c("cea", "ca199", "afp", "ldh", "crp", "albumin"),
  typestatistics = "nonparametric",
  matrixtype = "lower",
  padjustmethod = "holm",
  title = "Tumor Biomarker Correlations (Spearman)"
)

# ═══════════════════════════════════════════════════════════
# Example 5: Laboratory Values with Custom Colors
# ═══════════════════════════════════════════════════════════
# Scenario: Metabolic panel with publication colors

data(jjcorrmat_labvalues, package = "ClinicoPath")

jjcorrmat(
  data = jjcorrmat_labvalues,
  dep = c("glucose", "cholesterol", "triglycerides", "hdl", "ldl"),
  typestatistics = "parametric",
  lowcolor = "blue",
  midcolor = "white",
  highcolor = "red",
  matrixmethod = "circle",
  title = "Metabolic Panel Correlations",
  k = 3
)

# ═══════════════════════════════════════════════════════════
# Example 6: Partial Correlations
# ═══════════════════════════════════════════════════════════
# Scenario: Control for all other variables to find unique associations

jjcorrmat(
  data = jjcorrmat_test,
  dep = c("tumor_size", "ki67_index", "mitotic_count", "age"),
  partial = TRUE,
  typestatistics = "parametric",
  matrixtype = "full",
  title = "Partial Correlations (Controlling for Other Variables)"
)

# ═══════════════════════════════════════════════════════════
# Example 7: Imaging Metrics with Strong Correlations
# ═══════════════════════════════════════════════════════════
# Scenario: Radiological measurements showing size relationships

data(jjcorrmat_imaging, package = "ClinicoPath")

jjcorrmat(
  data = jjcorrmat_imaging,
  dep = c("tumor_volume", "tumor_longest_diameter",
          "tumor_shortest_diameter", "suv_max", "suv_mean", "adc"),
  typestatistics = "parametric",
  matrixmethod = "circle",
  matrixtype = "upper",
  k = 3,
  title = "Imaging Metric Correlations"
)

# ═══════════════════════════════════════════════════════════
# Example 8: Vital Signs with Robust Correlation
# ═══════════════════════════════════════════════════════════
# Scenario: Physiological measurements using robust methods

data(jjcorrmat_vitals, package = "ClinicoPath")

jjcorrmat(
  data = jjcorrmat_vitals,
  dep = c("systolic_bp", "diastolic_bp", "heart_rate",
          "respiratory_rate", "temperature", "oxygen_saturation"),
  typestatistics = "robust",
  siglevel = 0.01,
  matrixtype = "lower",
  title = "Vital Sign Correlations (Robust)"
)

# ═══════════════════════════════════════════════════════════
# Example 9: Bayesian Correlation Matrix
# ═══════════════════════════════════════════════════════════
# Scenario: Evidence-based correlation assessment with Bayes Factors

data(jjcorrmat_mixed, package = "ClinicoPath")

jjcorrmat(
  data = jjcorrmat_mixed,
  dep = c("var_a", "var_b", "var_c", "var_d", "var_e"),
  typestatistics = "bayes",
  matrixtype = "full",
  title = "Bayesian Correlation Matrix"
)

# ═══════════════════════════════════════════════════════════
# Example 10: Pairwise Deletion for Missing Data
# ═══════════════════════════════════════════════════════════
# Scenario: Use all available data pairs despite missing values

jjcorrmat(
  data = jjcorrmat_test,
  dep = c("tumor_size", "ki67_index", "necrosis_percent", "age"),
  naHandling = "pairwise",
  typestatistics = "parametric",
  matrixtype = "upper"
)

# ═══════════════════════════════════════════════════════════
# Example 11: Complete Laboratory Panel
# ═══════════════════════════════════════════════════════════
# Scenario: Comprehensive clinical chemistry relationships

jjcorrmat(
  data = jjcorrmat_labvalues,
  dep = c("glucose", "cholesterol", "triglycerides", "hdl", "ldl",
          "creatinine", "alt", "ast"),
  typestatistics = "parametric",
  padjustmethod = "BH",  # False discovery rate control
  matrixtype = "upper",
  matrixmethod = "square",
  title = "Complete Laboratory Panel Correlations"
)

# ═══════════════════════════════════════════════════════════
# Example 12: Grouped Biomarker Analysis by Cancer Type
# ═══════════════════════════════════════════════════════════
# Scenario: Compare biomarker patterns across cancer types

jjcorrmat(
  data = jjcorrmat_biomarker,
  dep = c("cea", "ca199", "ldh", "crp"),
  grvar = "cancer_type",
  typestatistics = "nonparametric",
  matrixtype = "upper"
)

# ═══════════════════════════════════════════════════════════
# Example 13: High Precision Display
# ═══════════════════════════════════════════════════════════
# Scenario: Show correlation coefficients with 4 decimal places

jjcorrmat(
  data = jjcorrmat_imaging,
  dep = c("tumor_volume", "tumor_longest_diameter", "tumor_shortest_diameter"),
  typestatistics = "parametric",
  k = 4,
  matrixtype = "full",
  title = "High Precision Correlations (4 decimals)"
)

# ═══════════════════════════════════════════════════════════
# Example 14: Holm Adjustment (Recommended)
# ═══════════════════════════════════════════════════════════
# Scenario: Step-down procedure for multiple testing (less conservative than Bonferroni)

jjcorrmat(
  data = jjcorrmat_vitals,
  dep = c("systolic_bp", "diastolic_bp", "heart_rate", "respiratory_rate"),
  typestatistics = "parametric",
  padjustmethod = "holm",
  matrixtype = "upper",
  title = "Vital Signs with Holm Adjustment"
)

# ═══════════════════════════════════════════════════════════
# Example 15: Comprehensive Publication Figure
# ═══════════════════════════════════════════════════════════
# Scenario: Publication-ready correlation matrix with all customizations

jjcorrmat(
  data = jjcorrmat_test,
  dep = c("tumor_size", "ki67_index", "mitotic_count", "necrosis_percent"),
  typestatistics = "parametric",
  padjustmethod = "bonferroni",
  matrixtype = "upper",
  matrixmethod = "circle",
  siglevel = 0.05,
  conflevel = 0.95,
  lowcolor = "#0571B0",
  midcolor = "white",
  highcolor = "#CA0020",
  title = "Tumor Characteristics Correlation Matrix",
  subtitle = "Pearson correlations with Bonferroni adjustment",
  caption = "* p < 0.05; ** p < 0.01; *** p < 0.001",
  k = 2,
  plotwidth = 700,
  plotheight = 600,
  showexplanations = TRUE
)

# ═══════════════════════════════════════════════════════════
# Example 16: Listwise vs Pairwise Comparison
# ═══════════════════════════════════════════════════════════
# Scenario: Compare different missing data handling approaches

# Listwise deletion (complete cases only)
jjcorrmat(
  data = jjcorrmat_test,
  dep = c("tumor_size", "ki67_index", "necrosis_percent"),
  naHandling = "listwise",
  typestatistics = "parametric",
  title = "Listwise Deletion (Complete Cases)"
)

# Pairwise deletion (all available pairs)
jjcorrmat(
  data = jjcorrmat_test,
  dep = c("tumor_size", "ki67_index", "necrosis_percent"),
  naHandling = "pairwise",
  typestatistics = "parametric",
  title = "Pairwise Deletion (All Available Pairs)"
)

# ═══════════════════════════════════════════════════════════
# Example 17: Circle vs Square Matrix Methods
# ═══════════════════════════════════════════════════════════
# Scenario: Compare visualization methods

# Square method (default)
jjcorrmat(
  data = jjcorrmat_mixed,
  dep = c("var_a", "var_b", "var_c", "var_d"),
  matrixmethod = "square",
  matrixtype = "upper",
  title = "Square Matrix Method"
)

# Circle method
jjcorrmat(
  data = jjcorrmat_mixed,
  dep = c("var_a", "var_b", "var_c", "var_d"),
  matrixmethod = "circle",
  matrixtype = "upper",
  title = "Circle Matrix Method"
)

# ═══════════════════════════════════════════════════════════
# Example 18: Grouped Vital Signs by Patient Status
# ═══════════════════════════════════════════════════════════
# Scenario: Explore how physiological correlations differ by clinical status

jjcorrmat(
  data = jjcorrmat_vitals,
  dep = c("systolic_bp", "diastolic_bp", "heart_rate", "temperature"),
  grvar = "patient_status",
  typestatistics = "parametric",
  matrixtype = "upper"
)
