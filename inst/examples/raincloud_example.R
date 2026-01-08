# ═══════════════════════════════════════════════════════════
# Example Usage: raincloud
# ═══════════════════════════════════════════════════════════
#
# Comprehensive examples for the raincloud jamovi function
# Generated: 2026-01-06

library(ClinicoPath)

# Load test datasets
data(raincloud_test)
data(raincloud_clinical)
data(raincloud_treatment)
data(raincloud_biomarker)
data(raincloud_skewed)

# ═══════════════════════════════════════════════════════════
# BASIC RAINCLOUD PLOTS
# ═══════════════════════════════════════════════════════════

# Example 1: Simple distribution comparison
raincloud(
  data = raincloud_test,
  dep_var = "symptom_score",
  group_var = "treatment_group"
)

# Example 2: With violin plot only
raincloud(
  data = raincloud_test,
  dep_var = "symptom_score",
  group_var = "treatment_group",
  show_violin = TRUE,
  show_boxplot = FALSE,
  show_dots = FALSE
)

# Example 3: With boxplot only
raincloud(
  data = raincloud_test,
  dep_var = "quality_of_life",
  group_var = "disease_severity",
  show_violin = FALSE,
  show_boxplot = TRUE,
  show_dots = FALSE
)

# Example 4: With all components (violin + box + dots)
raincloud(
  data = raincloud_test,
  dep_var = "pain_intensity",
  group_var = "treatment_group",
  show_violin = TRUE,
  show_boxplot = TRUE,
  show_dots = TRUE
)

# ═══════════════════════════════════════════════════════════
# FACETING AND GROUPING
# ═══════════════════════════════════════════════════════════

# Example 5: Faceted by disease severity
raincloud(
  data = raincloud_test,
  dep_var = "symptom_score",
  group_var = "treatment_group",
  facet_var = "disease_severity"
)

# Example 6: With additional color variable
raincloud(
  data = raincloud_test,
  dep_var = "quality_of_life",
  group_var = "disease_severity",
  color_var = "gender"
)

# Example 7: Both faceting and coloring
raincloud(
  data = raincloud_test,
  dep_var = "symptom_score",
  group_var = "treatment_group",
  facet_var = "disease_severity",
  color_var = "gender"
)

# Example 8: Multi-site comparison
raincloud(
  data = raincloud_test,
  dep_var = "response_time",
  group_var = "age_group",
  facet_var = "hospital_site"
)

# ═══════════════════════════════════════════════════════════
# CLINICAL LABORATORY DATA
# ═══════════════════════════════════════════════════════════

# Example 9: Glucose levels by diagnosis
raincloud(
  data = raincloud_clinical,
  dep_var = "glucose",
  group_var = "diagnosis",
  theme = "clinical"
)

# Example 10: HbA1c across diagnoses with normality test
raincloud(
  data = raincloud_clinical,
  dep_var = "hemoglobin_a1c",
  group_var = "diagnosis",
  normality_test = TRUE,
  show_mean = TRUE
)

# Example 11: Blood pressure by age, faceted by diagnosis
raincloud(
  data = raincloud_clinical,
  dep_var = "systolic_bp",
  group_var = "age_category",
  facet_var = "diagnosis",
  comparison_test = TRUE
)

# Example 12: Cholesterol by BMI with outlier detection
raincloud(
  data = raincloud_clinical,
  dep_var = "cholesterol",
  group_var = "bmi_category",
  show_outliers = TRUE,
  show_median = TRUE
)

# ═══════════════════════════════════════════════════════════
# TREATMENT EFFECT VISUALIZATION
# ═══════════════════════════════════════════════════════════

# Example 13: Tumor size by treatment
raincloud(
  data = raincloud_treatment,
  dep_var = "tumor_size",
  group_var = "treatment"
)

# Example 14: Treatment effect over time
raincloud(
  data = raincloud_treatment,
  dep_var = "tumor_size",
  group_var = "timepoint",
  facet_var = "treatment",
  comparison_test = TRUE
)

# Example 15: Response stratified analysis
raincloud(
  data = raincloud_treatment,
  dep_var = "tumor_size",
  group_var = "treatment",
  facet_var = "response_category",
  color_var = "timepoint"
)

# Example 16: Symptom burden analysis
raincloud(
  data = raincloud_treatment,
  dep_var = "symptom_burden",
  group_var = "response_category",
  show_mean = TRUE,
  show_median = TRUE
)

# ═══════════════════════════════════════════════════════════
# BIOMARKER EXPRESSION ANALYSIS
# ═══════════════════════════════════════════════════════════

# Example 17: Ki67 index by tumor grade
raincloud(
  data = raincloud_biomarker,
  dep_var = "ki67_index",
  group_var = "grade",
  palette = "floral"
)

# Example 18: Protein expression by receptor status
raincloud(
  data = raincloud_biomarker,
  dep_var = "protein_expression",
  group_var = "receptor_status",
  comparison_test = TRUE,
  show_mean = TRUE
)

# Example 19: Mutation burden across cancer types
raincloud(
  data = raincloud_biomarker,
  dep_var = "mutation_burden",
  group_var = "cancer_type",
  facet_var = "stage",
  normality_test = TRUE
)

# Example 20: Immune score stratified analysis
raincloud(
  data = raincloud_biomarker,
  dep_var = "immune_score",
  group_var = "stage",
  facet_var = "cancer_type",
  color_var = "receptor_status",
  palette = "colorblind_safe"
)

# ═══════════════════════════════════════════════════════════
# DISTRIBUTION TYPES
# ═══════════════════════════════════════════════════════════

# Example 21: Normal distribution
raincloud(
  data = raincloud_skewed,
  dep_var = "normal",
  group_var = "condition",
  normality_test = TRUE,
  show_violin = TRUE,
  show_boxplot = TRUE
)

# Example 22: Right-skewed distribution (reaction times)
raincloud(
  data = raincloud_skewed,
  dep_var = "right_skewed",
  group_var = "condition",
  normality_test = TRUE,
  show_outliers = TRUE
)

# Example 23: Bimodal distribution (responders/non-responders)
raincloud(
  data = raincloud_skewed,
  dep_var = "bimodal",
  group_var = "condition",
  show_violin = TRUE,
  show_mean = TRUE,
  show_median = TRUE
)

# Example 24: Log-normal distribution (biological measurements)
raincloud(
  data = raincloud_skewed,
  dep_var = "lognormal",
  group_var = "condition",
  normality_test = TRUE
)

# ═══════════════════════════════════════════════════════════
# THEMES AND PALETTES
# ═══════════════════════════════════════════════════════════

# Example 25: Clinical theme
raincloud(
  data = raincloud_clinical,
  dep_var = "glucose",
  group_var = "diagnosis",
  theme = "clinical",
  palette = "viridis"
)

# Example 26: Publication-ready theme
raincloud(
  data = raincloud_test,
  dep_var = "symptom_score",
  group_var = "treatment_group",
  theme = "publication",
  palette = "colorblind_safe",
  show_violin = TRUE,
  show_boxplot = TRUE
)

# Example 27: GraphPad Prism theme with floral palette
raincloud(
  data = raincloud_biomarker,
  dep_var = "ki67_index",
  group_var = "grade",
  theme = "prism",
  palette = "floral",
  show_violin = TRUE,
  show_boxplot = TRUE,
  show_dots = TRUE
)

# Example 28: Prism whitespace theme
raincloud(
  data = raincloud_biomarker,
  dep_var = "protein_expression",
  group_var = "receptor_status",
  theme = "prism_whitespace",
  palette = "candy_bright"
)

# ═══════════════════════════════════════════════════════════
# STATISTICAL FEATURES
# ═══════════════════════════════════════════════════════════

# Example 29: With normality and comparison tests
raincloud(
  data = raincloud_test,
  dep_var = "symptom_score",
  group_var = "treatment_group",
  normality_test = TRUE,
  comparison_test = TRUE
)

# Example 30: With outlier detection and summary statistics
raincloud(
  data = raincloud_clinical,
  dep_var = "systolic_bp",
  group_var = "diagnosis",
  show_outliers = TRUE,
  show_mean = TRUE,
  show_median = TRUE
)

# ═══════════════════════════════════════════════════════════
# ADVANCED CUSTOMIZATION
# ═══════════════════════════════════════════════════════════

# Example 31: Custom component sizes
raincloud(
  data = raincloud_test,
  dep_var = "quality_of_life",
  group_var = "disease_severity",
  violin_width = 0.8,
  box_width = 0.3,
  dots_size = 2.5,
  show_violin = TRUE,
  show_boxplot = TRUE,
  show_dots = TRUE
)

# Example 32: Custom transparency levels
raincloud(
  data = raincloud_biomarker,
  dep_var = "ki67_index",
  group_var = "grade",
  violin_alpha = 0.6,
  box_alpha = 0.9,
  dots_alpha = 0.5,
  show_violin = TRUE,
  show_boxplot = TRUE,
  show_dots = TRUE
)

# ═══════════════════════════════════════════════════════════
# PUBLICATION-READY COMPLETE EXAMPLES
# ═══════════════════════════════════════════════════════════

# Example 33: Complete clinical lab analysis
raincloud(
  data = raincloud_clinical,
  dep_var = "glucose",
  group_var = "diagnosis",
  facet_var = "age_category",
  color_var = "bmi_category",
  theme = "publication",
  palette = "colorblind_safe",
  show_violin = TRUE,
  show_boxplot = TRUE,
  show_dots = FALSE,
  normality_test = TRUE,
  comparison_test = TRUE,
  show_outliers = TRUE,
  show_mean = TRUE,
  show_median = TRUE,
  violin_width = 0.7,
  box_width = 0.2
)

# Example 34: Complete treatment effect analysis
raincloud(
  data = raincloud_treatment,
  dep_var = "tumor_size",
  group_var = "treatment",
  facet_var = "timepoint",
  color_var = "response_category",
  theme = "clinical",
  palette = "viridis",
  show_violin = TRUE,
  show_boxplot = TRUE,
  show_dots = FALSE,
  comparison_test = TRUE,
  show_mean = TRUE,
  show_median = TRUE
)

# Example 35: Complete biomarker analysis with Prism theme
raincloud(
  data = raincloud_biomarker,
  dep_var = "ki67_index",
  group_var = "grade",
  facet_var = "cancer_type",
  theme = "prism",
  palette = "floral",
  show_violin = TRUE,
  show_boxplot = TRUE,
  show_dots = TRUE,
  normality_test = TRUE,
  comparison_test = TRUE,
  show_outliers = TRUE,
  show_median = TRUE,
  violin_width = 0.75,
  dots_size = 1.5
)
