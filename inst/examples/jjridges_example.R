# ═══════════════════════════════════════════════════════════
# Example Usage: jjridges
# ═══════════════════════════════════════════════════════════
#
# Comprehensive examples for the jjridges jamovi function
# Generated: 2026-01-06

library(ClinicoPath)

# Load test datasets
data(jjridges_test)
data(jjridges_clinical)
data(jjridges_treatment)
data(jjridges_biomarker)
data(jjridges_survival)

# ═══════════════════════════════════════════════════════════
# BASIC RIDGELINE PLOTS
# ═══════════════════════════════════════════════════════════

# Example 1: Simple ridgeline plot
jjridges(
  data = jjridges_test,
  x_var = "ki67_index",
  y_var = "tumor_stage"
)

# Example 2: Density ridges (default)
jjridges(
  data = jjridges_test,
  x_var = "tumor_size",
  y_var = "tumor_grade",
  plot_type = "density_ridges"
)

# Example 3: Density ridges with gradient fill
jjridges(
  data = jjridges_test,
  x_var = "ki67_index",
  y_var = "tumor_stage",
  plot_type = "density_ridges_gradient",
  gradient_low = "#0000FF",
  gradient_high = "#FF0000"
)

# Example 4: Histogram ridges
jjridges(
  data = jjridges_test,
  x_var = "age",
  y_var = "tumor_grade",
  plot_type = "histogram_ridges",
  binwidth = 5
)

# Example 5: Violin ridges
jjridges(
  data = jjridges_test,
  x_var = "response_score",
  y_var = "treatment_arm",
  plot_type = "violin_ridges"
)

# ═══════════════════════════════════════════════════════════
# RIDGE HEIGHT AND OVERLAP
# ═══════════════════════════════════════════════════════════

# Example 6: Separated ridges (no overlap)
jjridges(
  data = jjridges_test,
  x_var = "ki67_index",
  y_var = "tumor_stage",
  scale = 0.5
)

# Example 7: Overlapping ridges (classic "joy plot" style)
jjridges(
  data = jjridges_test,
  x_var = "tumor_size",
  y_var = "tumor_stage",
  scale = 2.0
)

# ═══════════════════════════════════════════════════════════
# ADVANCED VISUALIZATION FEATURES
# ═══════════════════════════════════════════════════════════

# Example 8: With boxplot inside
jjridges(
  data = jjridges_test,
  x_var = "ki67_index",
  y_var = "tumor_stage",
  add_boxplot = TRUE
)

# Example 9: With quantile lines
jjridges(
  data = jjridges_test,
  x_var = "tumor_size",
  y_var = "tumor_grade",
  add_quantiles = TRUE,
  quantiles = "0.25, 0.5, 0.75"
)

# Example 10: With mean and median markers
jjridges(
  data = jjridges_test,
  x_var = "response_score",
  y_var = "treatment_arm",
  add_mean = TRUE,
  add_median = TRUE
)

# Example 11: All features combined
jjridges(
  data = jjridges_test,
  x_var = "ki67_index",
  y_var = "tumor_stage",
  add_boxplot = TRUE,
  add_quantiles = TRUE,
  quantiles = "0.25, 0.75",
  add_mean = TRUE
)

# ═══════════════════════════════════════════════════════════
# STATISTICAL COMPARISONS
# ═══════════════════════════════════════════════════════════

# Example 12: Parametric tests (ANOVA/t-test)
jjridges(
  data = jjridges_test,
  x_var = "response_score",
  y_var = "treatment_arm",
  show_stats = TRUE,
  test_type = "parametric",
  effsize_type = "d"
)

# Example 13: Nonparametric tests (Kruskal-Wallis/Mann-Whitney)
jjridges(
  data = jjridges_test,
  x_var = "lymph_nodes",  # Right-skewed variable
  y_var = "tumor_stage",
  show_stats = TRUE,
  test_type = "nonparametric",
  effsize_type = "cliff_delta"
)

# Example 14: Robust tests with Hedge's g
jjridges(
  data = jjridges_clinical,
  x_var = "triglycerides",
  y_var = "bmi_category",
  show_stats = TRUE,
  test_type = "robust",
  effsize_type = "g"
)

# Example 15: Bayesian analysis
jjridges(
  data = jjridges_test,
  x_var = "protein_expression",
  y_var = "receptor_status",
  show_stats = TRUE,
  test_type = "bayes"
)

# Example 16: With p-value adjustment (FDR)
jjridges(
  data = jjridges_test,
  x_var = "ki67_index",
  y_var = "tumor_stage",
  show_stats = TRUE,
  test_type = "parametric",
  p_adjust_method = "fdr"
)

# ═══════════════════════════════════════════════════════════
# CLINICAL LABORATORY DATA
# ═══════════════════════════════════════════════════════════

# Example 17: Glucose levels by diagnosis
jjridges(
  data = jjridges_clinical,
  x_var = "glucose",
  y_var = "diagnosis",
  clinicalPreset = "lab_values_by_group"
)

# Example 18: HbA1c distribution
jjridges(
  data = jjridges_clinical,
  x_var = "hemoglobin_a1c",
  y_var = "diagnosis",
  add_boxplot = TRUE,
  show_stats = TRUE,
  test_type = "parametric"
)

# Example 19: Triglycerides by BMI (log-scale data)
jjridges(
  data = jjridges_clinical,
  x_var = "triglycerides",
  y_var = "bmi_category",
  show_stats = TRUE,
  test_type = "nonparametric",
  add_median = TRUE
)

# Example 20: Blood pressure by age group
jjridges(
  data = jjridges_clinical,
  x_var = "systolic_bp",
  y_var = "age_group",
  add_quantiles = TRUE,
  quantiles = "0.25, 0.5, 0.75"
)

# ═══════════════════════════════════════════════════════════
# TREATMENT RESPONSE OVER TIME
# ═══════════════════════════════════════════════════════════

# Example 21: Pain scores by timepoint
jjridges(
  data = jjridges_treatment,
  x_var = "pain_score",
  y_var = "timepoint",
  clinicalPreset = "treatment_response"
)

# Example 22: Response by treatment group
jjridges(
  data = jjridges_treatment,
  x_var = "response_score",
  y_var = "treatment_group",
  facet_var = "timepoint",
  add_mean = TRUE
)

# Example 23: Quality of life progression
jjridges(
  data = jjridges_treatment,
  x_var = "qol_score",
  y_var = "timepoint",
  facet_var = "disease_severity",
  show_stats = TRUE,
  test_type = "parametric"
)

# ═══════════════════════════════════════════════════════════
# BIOMARKER EXPRESSION
# ═══════════════════════════════════════════════════════════

# Example 24: Mutation burden across cancer types
jjridges(
  data = jjridges_biomarker,
  x_var = "mutation_burden",
  y_var = "cancer_type",
  clinicalPreset = "biomarker_distribution"
)

# Example 25: PD-L1 expression by cancer type
jjridges(
  data = jjridges_biomarker,
  x_var = "pdl1_expression",
  y_var = "cancer_type",
  add_boxplot = TRUE,
  show_stats = TRUE,
  test_type = "nonparametric"
)

# Example 26: TILs by stage
jjridges(
  data = jjridges_biomarker,
  x_var = "tils_percent",
  y_var = "stage",
  fill_var = "histology",
  add_median = TRUE
)

# ═══════════════════════════════════════════════════════════
# SURVIVAL TIME DISTRIBUTION
# ═══════════════════════════════════════════════════════════

# Example 27: Survival months by treatment
jjridges(
  data = jjridges_survival,
  x_var = "survival_months",
  y_var = "treatment",
  clinicalPreset = "survival_time_distribution"
)

# Example 28: Survival by disease stage
jjridges(
  data = jjridges_survival,
  x_var = "survival_months",
  y_var = "disease_stage",
  facet_var = "age_category",
  add_quantiles = TRUE,
  quantiles = "0.5"
)

# Example 29: Disease-free survival
jjridges(
  data = jjridges_survival,
  x_var = "dfs_months",
  y_var = "treatment",
  show_stats = TRUE,
  test_type = "nonparametric",
  effsize_type = "hodges_lehmann"
)

# ═══════════════════════════════════════════════════════════
# THEMES AND PALETTES
# ═══════════════════════════════════════════════════════════

# Example 30: Publication theme with clinical palette
jjridges(
  data = jjridges_test,
  x_var = "ki67_index",
  y_var = "tumor_stage",
  theme_style = "theme_pubr",
  color_palette = "clinical_colorblind",
  add_boxplot = TRUE,
  show_stats = TRUE,
  test_type = "parametric"
)

# Example 31: Minimal theme with viridis palette
jjridges(
  data = jjridges_biomarker,
  x_var = "mutation_burden",
  y_var = "cancer_type",
  theme_style = "theme_minimal",
  color_palette = "viridis"
)

# Example 32: Classic theme with Set2 palette
jjridges(
  data = jjridges_clinical,
  x_var = "glucose",
  y_var = "diagnosis",
  theme_style = "theme_classic",
  color_palette = "Set2"
)

# ═══════════════════════════════════════════════════════════
# FILL AND FACET VARIABLES
# ═══════════════════════════════════════════════════════════

# Example 33: With fill variable
jjridges(
  data = jjridges_test,
  x_var = "ki67_index",
  y_var = "tumor_stage",
  fill_var = "receptor_status",
  show_fill_legend = TRUE,
  legend_position = "right"
)

# Example 34: With facet variable
jjridges(
  data = jjridges_test,
  x_var = "tumor_size",
  y_var = "tumor_grade",
  facet_var = "receptor_status"
)

# Example 35: Both fill and facet
jjridges(
  data = jjridges_test,
  x_var = "response_score",
  y_var = "tumor_stage",
  fill_var = "receptor_status",
  facet_var = "treatment_arm"
)

# ═══════════════════════════════════════════════════════════
# PUBLICATION-READY COMPLETE EXAMPLES
# ═══════════════════════════════════════════════════════════

# Example 36: Complete biomarker analysis
jjridges(
  data = jjridges_test,
  x_var = "ki67_index",
  y_var = "tumor_stage",
  facet_var = "receptor_status",
  plot_type = "density_ridges",
  scale = 1.5,
  theme_style = "theme_pubr",
  color_palette = "clinical_colorblind",
  add_boxplot = TRUE,
  add_quantiles = TRUE,
  quantiles = "0.25, 0.75",
  add_median = TRUE,
  show_stats = TRUE,
  test_type = "nonparametric",
  effsize_type = "cliff_delta",
  p_adjust_method = "fdr",
  add_sample_size = TRUE,
  alpha = 0.7
)

# Example 37: Complete clinical laboratory report
jjridges(
  data = jjridges_clinical,
  x_var = "glucose",
  y_var = "diagnosis",
  facet_var = "bmi_category",
  plot_type = "density_ridges",
  theme_style = "theme_pubr",
  color_palette = "viridis",
  add_boxplot = TRUE,
  add_mean = TRUE,
  show_stats = TRUE,
  test_type = "parametric",
  effsize_type = "d",
  p_adjust_method = "bonferroni",
  add_sample_size = TRUE
)

# Example 38: Complete treatment response analysis
jjridges(
  data = jjridges_treatment,
  x_var = "pain_score",
  y_var = "timepoint",
  facet_var = "treatment_group",
  plot_type = "violin_ridges",
  scale = 1.0,
  theme_style = "theme_ridges",
  color_palette = "Set2",
  add_boxplot = TRUE,
  add_mean = TRUE,
  add_median = TRUE,
  show_stats = TRUE,
  test_type = "parametric",
  effsize_type = "g",
  p_adjust_method = "bonferroni",
  add_sample_size = TRUE
)
