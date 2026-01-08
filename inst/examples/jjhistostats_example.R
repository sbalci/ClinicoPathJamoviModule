# ═══════════════════════════════════════════════════════════
# Example Usage: jjhistostats (Histogram with Statistical Annotations)
# ═══════════════════════════════════════════════════════════
#
# This file demonstrates comprehensive usage of the jjhistostats jamovi function
# which creates histograms with statistical annotations for continuous variables.
#
# Created: 2026-01-05
# Test data: jjhistostats_test, jjhistostats_labvalues, jjhistostats_skewed,
#            jjhistostats_bimodal, jjhistostats_pathology, jjhistostats_grouped,
#            jjhistostats_small, jjhistostats_uniform

library(ClinicoPath)

# ═══════════════════════════════════════════════════════════
# Example 1: Basic Single Variable Histogram
# ═══════════════════════════════════════════════════════════
# Scenario: Age distribution in patient cohort

data(jjhistostats_test, package = "ClinicoPath")

jjhistostats(
  data = jjhistostats_test,
  dep = "age_years",
  typestatistics = "parametric"
)

# ═══════════════════════════════════════════════════════════
# Example 2: With Centrality Line and Statistical Results
# ═══════════════════════════════════════════════════════════
# Scenario: Show mean line with normality test results

jjhistostats(
  data = jjhistostats_test,
  dep = "age_years",
  typestatistics = "parametric",
  centralityline = TRUE,
  resultssubtitle = TRUE,
  title = "Age Distribution in Study Cohort",
  xlab = "Age (years)"
)

# ═══════════════════════════════════════════════════════════
# Example 3: Multiple Variables
# ═══════════════════════════════════════════════════════════
# Scenario: Examine distributions of multiple clinical variables

jjhistostats(
  data = jjhistostats_test,
  dep = c("age_years", "tumor_size_mm", "bmi", "hemoglobin"),
  centralityline = TRUE,
  typestatistics = "parametric"
)

# ═══════════════════════════════════════════════════════════
# Example 4: Skewed Biomarker Data
# ═══════════════════════════════════════════════════════════
# Scenario: Right-skewed PSA levels requiring nonparametric approach

jjhistostats(
  data = jjhistostats_test,
  dep = "psa_level",
  typestatistics = "nonparametric",
  centralityline = TRUE,
  resultssubtitle = TRUE,
  title = "PSA Level Distribution",
  xlab = "PSA (ng/mL)"
)

# ═══════════════════════════════════════════════════════════
# Example 5: Lab Values with Clinical Preset
# ═══════════════════════════════════════════════════════════
# Scenario: Laboratory values analysis

data(jjhistostats_labvalues, package = "ClinicoPath")

jjhistostats(
  data = jjhistostats_labvalues,
  dep = "glucose",
  clinicalPreset = "lab_values",
  title = "Fasting Glucose Distribution",
  xlab = "Glucose (mg/dL)"
)

# ═══════════════════════════════════════════════════════════
# Example 6: Multiple Lab Values
# ═══════════════════════════════════════════════════════════
# Scenario: Comprehensive lab panel assessment

jjhistostats(
  data = jjhistostats_labvalues,
  dep = c("glucose", "cholesterol", "triglycerides", "creatinine"),
  clinicalPreset = "lab_values",
  centralityline = TRUE,
  resultssubtitle = TRUE
)

# ═══════════════════════════════════════════════════════════
# Example 7: One-Sample Test Against Reference Value
# ═══════════════════════════════════════════════════════════
# Scenario: Test if cholesterol differs from population norm (200 mg/dL)

jjhistostats(
  data = jjhistostats_labvalues,
  dep = "cholesterol",
  typestatistics = "parametric",
  enableOneSampleTest = TRUE,
  test.value = 200,
  centralityline = TRUE,
  resultssubtitle = TRUE,
  title = "Cholesterol vs Population Norm (200 mg/dL)",
  xlab = "Total Cholesterol (mg/dL)"
)

# ═══════════════════════════════════════════════════════════
# Example 8: Grouped Analysis by Disease Stage
# ═══════════════════════════════════════════════════════════
# Scenario: Compare biomarker distributions across disease stages

data(jjhistostats_grouped, package = "ClinicoPath")

jjhistostats(
  data = jjhistostats_grouped,
  dep = "biomarker_level",
  grvar = "disease_stage",
  typestatistics = "parametric",
  centralityline = TRUE,
  resultssubtitle = TRUE
)

# ═══════════════════════════════════════════════════════════
# Example 9: Grouped Analysis by Treatment
# ═══════════════════════════════════════════════════════════
# Scenario: Age distribution stratified by treatment arm

jjhistostats(
  data = jjhistostats_grouped,
  dep = "age_years",
  grvar = "treatment",
  typestatistics = "parametric",
  centralityline = TRUE
)

# ═══════════════════════════════════════════════════════════
# Example 10: Skewed Biomarkers with Nonparametric Tests
# ═══════════════════════════════════════════════════════════
# Scenario: Log-normal tumor markers

data(jjhistostats_skewed, package = "ClinicoPath")

jjhistostats(
  data = jjhistostats_skewed,
  dep = c("cea", "ca199", "afp"),
  typestatistics = "nonparametric",
  centralityline = TRUE,
  centralitytype = "nonparametric",  # Show median
  resultssubtitle = TRUE
)

# ═══════════════════════════════════════════════════════════
# Example 11: Robust Statistics with Outliers
# ═══════════════════════════════════════════════════════════
# Scenario: Data with potential outliers

jjhistostats(
  data = jjhistostats_skewed,
  dep = "cea",
  typestatistics = "robust",
  centralityline = TRUE,
  centralitytype = "robust",  # Trimmed mean
  resultssubtitle = TRUE,
  title = "CEA Distribution (Robust Analysis)"
)

# ═══════════════════════════════════════════════════════════
# Example 12: Bayesian Analysis
# ═══════════════════════════════════════════════════════════
# Scenario: Bayesian approach with Bayes Factor

jjhistostats(
  data = jjhistostats_test,
  dep = "age_years",
  typestatistics = "bayes",
  bf.message = TRUE,
  centralityline = TRUE,
  centralitytype = "bayes",
  resultssubtitle = TRUE,
  title = "Age Distribution (Bayesian Analysis)"
)

# ═══════════════════════════════════════════════════════════
# Example 13: Custom Bin Width
# ═══════════════════════════════════════════════════════════
# Scenario: Manual control over histogram resolution

jjhistostats(
  data = jjhistostats_test,
  dep = "tumor_size_mm",
  typestatistics = "parametric",
  changebinwidth = TRUE,
  binwidth = 5.0,  # 5mm bins
  centralityline = TRUE,
  title = "Tumor Size Distribution (5mm bins)",
  xlab = "Tumor Size (mm)"
)

# ═══════════════════════════════════════════════════════════
# Example 14: Pathology Scores
# ═══════════════════════════════════════════════════════════
# Scenario: Ki-67 proliferation index distribution

data(jjhistostats_pathology, package = "ClinicoPath")

jjhistostats(
  data = jjhistostats_pathology,
  dep = "ki67_index",
  clinicalPreset = "pathology_scores",
  title = "Ki-67 Proliferation Index",
  xlab = "Ki-67 Index (%)"
)

# ═══════════════════════════════════════════════════════════
# Example 15: Multiple Pathology Variables
# ═══════════════════════════════════════════════════════════
# Scenario: Comprehensive pathology assessment

jjhistostats(
  data = jjhistostats_pathology,
  dep = c("ki67_index", "mitotic_count", "tumor_cellularity", "necrosis_percent"),
  clinicalPreset = "pathology_scores",
  centralityline = TRUE
)

# ═══════════════════════════════════════════════════════════
# Example 16: Pathology Grouped by Tumor Grade
# ═══════════════════════════════════════════════════════════
# Scenario: Ki-67 stratified by tumor grade

jjhistostats(
  data = jjhistostats_pathology,
  dep = "ki67_index",
  grvar = "tumor_grade",
  typestatistics = "nonparametric",
  centralityline = TRUE,
  resultssubtitle = TRUE
)

# ═══════════════════════════════════════════════════════════
# Example 17: Bimodal Distribution Detection
# ═══════════════════════════════════════════════════════════
# Scenario: Identify bimodal pattern in age distribution

data(jjhistostats_bimodal, package = "ClinicoPath")

jjhistostats(
  data = jjhistostats_bimodal,
  dep = "age_bimodal",
  typestatistics = "nonparametric",
  centralityline = TRUE,
  resultssubtitle = TRUE,
  title = "Age Distribution (Bimodal Pattern)",
  xlab = "Age (years)"
)

# ═══════════════════════════════════════════════════════════
# Example 18: Bimodal Response Score
# ═══════════════════════════════════════════════════════════
# Scenario: Responders vs non-responders

jjhistostats(
  data = jjhistostats_bimodal,
  dep = "response_score",
  typestatistics = "nonparametric",
  changebinwidth = TRUE,
  binwidth = 5.0,
  centralityline = TRUE,
  title = "Treatment Response Distribution",
  xlab = "Response Score (0-100)"
)

# ═══════════════════════════════════════════════════════════
# Example 19: Custom Aesthetics
# ═══════════════════════════════════════════════════════════
# Scenario: Publication-ready figure with custom colors

jjhistostats(
  data = jjhistostats_test,
  dep = "hemoglobin",
  typestatistics = "parametric",
  centralityline = TRUE,
  resultssubtitle = TRUE,
  binfill = "#0073C2FF",
  bincolor = "#000000",
  binalpha = 0.8,
  centralitylinecolor = "#E64B35FF",
  centralitylinewidth = 1.5,
  centralitylinetype = "dashed",
  title = "Hemoglobin Distribution",
  xlab = "Hemoglobin (g/dL)",
  plotwidth = 800,
  plotheight = 600
)

# ═══════════════════════════════════════════════════════════
# Example 20: High Precision Display
# ═══════════════════════════════════════════════════════════
# Scenario: Show results with high decimal precision

jjhistostats(
  data = jjhistostats_labvalues,
  dep = "creatinine",
  typestatistics = "parametric",
  centralityline = TRUE,
  resultssubtitle = TRUE,
  digits = 3,  # 3 decimal places
  title = "Serum Creatinine Distribution",
  xlab = "Creatinine (mg/dL)"
)

# ═══════════════════════════════════════════════════════════
# Example 21: 99% Confidence Interval
# ═══════════════════════════════════════════════════════════
# Scenario: More stringent confidence level

jjhistostats(
  data = jjhistostats_test,
  dep = "bmi",
  typestatistics = "parametric",
  conf.level = 0.99,
  centralityline = TRUE,
  resultssubtitle = TRUE,
  title = "BMI Distribution (99% CI)",
  xlab = "Body Mass Index (kg/m²)"
)

# ═══════════════════════════════════════════════════════════
# Example 22: ggpubr Additional Plot
# ═══════════════════════════════════════════════════════════
# Scenario: Add publication-ready ggpubr histogram

jjhistostats(
  data = jjhistostats_test,
  dep = "age_years",
  typestatistics = "parametric",
  addGGPubrPlot = TRUE,
  ggpubrPalette = "#00AFBB",
  ggpubrAddDensity = TRUE,
  ggpubrAddMean = TRUE,
  title = "Age Distribution with Density Curve"
)

# ═══════════════════════════════════════════════════════════
# Example 23: Distribution Diagnostics
# ═══════════════════════════════════════════════════════════
# Scenario: Comprehensive normality assessment with QQ and ECDF plots

jjhistostats(
  data = jjhistostats_labvalues,
  dep = "cholesterol",
  typestatistics = "parametric",
  addDistributionDiagnostics = TRUE,
  ggpubrShowQQ = TRUE,
  ggpubrShowECDF = TRUE,
  title = "Cholesterol Distribution with Diagnostic Plots"
)

# ═══════════════════════════════════════════════════════════
# Example 24: Small Sample Size
# ═══════════════════════════════════════════════════════════
# Scenario: Analysis with limited data (n=25)

data(jjhistostats_small, package = "ClinicoPath")

jjhistostats(
  data = jjhistostats_small,
  dep = "measurement",
  typestatistics = "parametric",
  centralityline = TRUE,
  resultssubtitle = TRUE,
  title = "Small Sample Analysis (n=25)"
)

# ═══════════════════════════════════════════════════════════
# Example 25: Uniform Distribution
# ═══════════════════════════════════════════════════════════
# Scenario: Non-normal uniform distribution

data(jjhistostats_uniform, package = "ClinicoPath")

jjhistostats(
  data = jjhistostats_uniform,
  dep = "uniform_score",
  typestatistics = "nonparametric",
  centralityline = TRUE,
  resultssubtitle = TRUE,
  title = "Uniform Distribution Example",
  xlab = "Score (0-100)"
)

# ═══════════════════════════════════════════════════════════
# Example 26: Clinical Interpretation
# ═══════════════════════════════════════════════════════════
# Scenario: Generate automated clinical interpretation

jjhistostats(
  data = jjhistostats_test,
  dep = "psa_level",
  typestatistics = "nonparametric",
  centralityline = TRUE,
  resultssubtitle = TRUE,
  showInterpretation = TRUE,
  title = "PSA with Clinical Interpretation"
)

# ═══════════════════════════════════════════════════════════
# Example 27: Compare Distribution Types
# ═══════════════════════════════════════════════════════════
# Scenario: Normal, skewed, bimodal, and uniform in one analysis

jjhistostats(
  data = jjhistostats_test,
  dep = c("age_years", "psa_level"),
  typestatistics = "nonparametric",
  centralityline = TRUE,
  resultssubtitle = TRUE
)

data(jjhistostats_bimodal)
jjhistostats(
  data = jjhistostats_bimodal,
  dep = "response_score",
  typestatistics = "nonparametric",
  centralityline = TRUE
)

data(jjhistostats_uniform)
jjhistostats(
  data = jjhistostats_uniform,
  dep = "uniform_score",
  typestatistics = "nonparametric",
  centralityline = TRUE
)

# ═══════════════════════════════════════════════════════════
# Example 28: Discrete Variable Histogram
# ═══════════════════════════════════════════════════════════
# Scenario: Mitotic count (discrete values)

jjhistostats(
  data = jjhistostats_pathology,
  dep = "mitotic_count",
  typestatistics = "nonparametric",
  changebinwidth = TRUE,
  binwidth = 1.0,  # 1-count bins
  centralityline = TRUE,
  title = "Mitotic Count Distribution",
  xlab = "Mitotic Count (per 10 HPF)"
)

# ═══════════════════════════════════════════════════════════
# Example 29: Complete Publication Figure
# ═══════════════════════════════════════════════════════════
# Scenario: Comprehensive publication-ready histogram

jjhistostats(
  data = jjhistostats_test,
  dep = "tumor_size_mm",
  typestatistics = "parametric",
  centralityline = TRUE,
  centralitytype = "parametric",
  resultssubtitle = TRUE,
  enableOneSampleTest = TRUE,
  test.value = 30,  # Test against 30mm threshold
  binfill = "#0073C2FF",
  bincolor = "#000000",
  binalpha = 0.7,
  centralitylinecolor = "#E64B35FF",
  centralitylinewidth = 1.5,
  centralitylinetype = "dashed",
  title = "Tumor Size Distribution vs 30mm Threshold",
  xlab = "Tumor Size (mm)",
  caption = "Study cohort n=150",
  conf.level = 0.95,
  digits = 2,
  plotwidth = 800,
  plotheight = 600
)

# ═══════════════════════════════════════════════════════════
# Example 30: Multi-Panel Pathology Dashboard
# ═══════════════════════════════════════════════════════════
# Scenario: Comprehensive pathology metrics panel

jjhistostats(
  data = jjhistostats_pathology,
  dep = c("ki67_index", "mitotic_count", "tumor_cellularity", "necrosis_percent"),
  clinicalPreset = "pathology_scores",
  centralityline = TRUE,
  resultssubtitle = TRUE,
  plotwidth = 600,
  plotheight = 450
)
