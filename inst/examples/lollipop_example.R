# ═══════════════════════════════════════════════════════════
# Example Usage: lollipop
# ═══════════════════════════════════════════════════════════
#
# Comprehensive examples for the lollipop jamovi function
# Generated: 2026-01-05

library(ClinicoPath)

# Load test datasets
data(lollipop_test)
data(lollipop_treatment)
data(lollipop_biomarkers)
data(lollipop_hospital)
data(lollipop_aggregated)

# ═══════════════════════════════════════════════════════════
# BASIC LOLLIPOP CHARTS
# ═══════════════════════════════════════════════════════════

# Example 1: Basic lollipop chart - Hemoglobin by treatment
lollipop(
  data = lollipop_test,
  dep = "hemoglobin",
  group = "treatment_group",
  aggregation = "mean"
)

# Example 2: Vertical orientation with sorted values
lollipop(
  data = lollipop_test,
  dep = "albumin",
  group = "disease_severity",
  aggregation = "mean",
  sortBy = "value_desc"
)

# Example 3: Horizontal orientation
lollipop(
  data = lollipop_test,
  dep = "creatinine",
  group = "age_group",
  aggregation = "median",
  orientation = "horizontal"
)

# ═══════════════════════════════════════════════════════════
# HIGHLIGHTING FEATURES
# ═══════════════════════════════════════════════════════════

# Example 4: Highlight severe disease cases
lollipop(
  data = lollipop_test,
  dep = "albumin",
  group = "disease_severity",
  useHighlight = TRUE,
  highlight = "Severe",
  aggregation = "mean",
  colorScheme = "clinical"
)

# Example 5: Highlight specific treatment group
lollipop(
  data = lollipop_test,
  dep = "hemoglobin",
  group = "treatment_group",
  useHighlight = TRUE,
  highlight = "Drug A",
  aggregation = "mean",
  showValues = TRUE
)

# ═══════════════════════════════════════════════════════════
# CONDITIONAL COLORING
# ═══════════════════════════════════════════════════════════

# Example 6: Color by creatinine threshold (>1.2 mg/dL)
lollipop(
  data = lollipop_test,
  dep = "creatinine",
  group = "age_group",
  conditionalColor = TRUE,
  colorThreshold = 1.2,
  aggregation = "mean",
  sortBy = "value_asc"
)

# Example 7: Color by WBC threshold (>11 ×10³/μL)
lollipop(
  data = lollipop_test,
  dep = "white_blood_cells",
  group = "treatment_group",
  conditionalColor = TRUE,
  colorThreshold = 11,
  aggregation = "mean",
  orientation = "horizontal"
)

# ═══════════════════════════════════════════════════════════
# CLINICAL BASELINES
# ═══════════════════════════════════════════════════════════

# Example 8: Platelet count with normal lower limit baseline
lollipop(
  data = lollipop_test,
  dep = "platelet_count",
  group = "hospital",
  baseline = 150,  # Lower normal limit
  aggregation = "mean",
  showMean = TRUE,
  colorScheme = "clinical"
)

# Example 9: Hemoglobin with custom baseline
lollipop(
  data = lollipop_test,
  dep = "hemoglobin",
  group = "treatment_group",
  baseline = 12,  # Minimum normal for females
  aggregation = "mean",
  sortBy = "value_desc"
)

# ═══════════════════════════════════════════════════════════
# TREATMENT COMPARISON
# ═══════════════════════════════════════════════════════════

# Example 10: Tumor reduction by treatment dose
lollipop(
  data = lollipop_treatment,
  dep = "tumor_reduction",
  group = "treatment",
  sortBy = "value_desc",
  showValues = TRUE,
  conditionalColor = TRUE,
  colorThreshold = 30,
  title = "Tumor Size Reduction by Treatment",
  ylabel = "Reduction (%)"
)

# Example 11: Quality of life scores
lollipop(
  data = lollipop_treatment,
  dep = "qol_score",
  group = "treatment",
  sortBy = "value_desc",
  orientation = "horizontal",
  showValues = TRUE,
  baseline = 60,
  colorScheme = "viridis",
  title = "Quality of Life by Treatment"
)

# Example 12: Side effects (lower is better)
lollipop(
  data = lollipop_treatment,
  dep = "side_effects",
  group = "treatment",
  sortBy = "value_asc",  # Lower is better
  showValues = TRUE,
  conditionalColor = TRUE,
  colorThreshold = 20,
  title = "Side Effect Scores (Lower is Better)"
)

# ═══════════════════════════════════════════════════════════
# BIOMARKER VISUALIZATION
# ═══════════════════════════════════════════════════════════

# Example 13: CEA levels by cancer type
lollipop(
  data = lollipop_biomarkers,
  dep = "cea",
  group = "cancer_type",
  aggregation = "median",
  sortBy = "value_desc",
  showValues = TRUE,
  colorScheme = "clinical",
  title = "CEA Levels by Cancer Type",
  ylabel = "CEA (ng/mL)"
)

# Example 14: CA-125 with highlighting
lollipop(
  data = lollipop_biomarkers,
  dep = "ca125",
  group = "cancer_type",
  useHighlight = TRUE,
  highlight = "Type B",
  aggregation = "median",
  sortBy = "value_desc",
  colorScheme = "clinical"
)

# ═══════════════════════════════════════════════════════════
# HOSPITAL QUALITY METRICS
# ═══════════════════════════════════════════════════════════

# Example 15: Hospital survival rates
lollipop(
  data = lollipop_hospital,
  dep = "survival_rate",
  group = "hospital",
  sortBy = "value_desc",
  orientation = "horizontal",
  showValues = TRUE,
  conditionalColor = TRUE,
  colorThreshold = 80,
  title = "Hospital Survival Rates (%)"
)

# Example 16: Complication rates (lower is better)
lollipop(
  data = lollipop_hospital,
  dep = "complication_rate",
  group = "hospital",
  sortBy = "value_asc",
  orientation = "horizontal",
  showValues = TRUE,
  title = "Hospital Complication Rates (Lower is Better)"
)

# Example 17: Patient satisfaction scores
lollipop(
  data = lollipop_hospital,
  dep = "patient_satisfaction",
  group = "hospital",
  sortBy = "value_desc",
  showMean = TRUE,
  showValues = TRUE,
  baseline = 50,
  colorScheme = "viridis"
)

# ═══════════════════════════════════════════════════════════
# VISUAL CUSTOMIZATION
# ═══════════════════════════════════════════════════════════

# Example 18: Different line types
lollipop(
  data = lollipop_test,
  dep = "hemoglobin",
  group = "treatment_group",
  aggregation = "mean",
  lineType = "dashed",
  pointSize = 4,
  lineWidth = 2
)

# Example 19: Custom colors and theme
lollipop(
  data = lollipop_test,
  dep = "platelet_count",
  group = "disease_severity",
  aggregation = "mean",
  colorScheme = "colorblind",
  theme = "minimal",
  pointSize = 5
)

# Example 20: Publication-ready figure
lollipop(
  data = lollipop_test,
  dep = "white_blood_cells",
  group = "treatment_group",
  aggregation = "mean",
  sortBy = "value_desc",
  showValues = TRUE,
  showMean = TRUE,
  colorScheme = "clinical",
  theme = "publication",
  lineType = "solid",
  pointSize = 4,
  lineWidth = 1.5,
  title = "White Blood Cell Counts by Treatment",
  xlabel = "Treatment Groups",
  ylabel = "WBC (×10³/μL)",
  width = 1000,
  height = 700
)

# ═══════════════════════════════════════════════════════════
# AGGREGATION METHODS
# ═══════════════════════════════════════════════════════════

# Example 21: Mean aggregation
lollipop(
  data = lollipop_test,
  dep = "albumin",
  group = "disease_severity",
  aggregation = "mean",
  sortBy = "value_desc"
)

# Example 22: Median aggregation (robust to outliers)
lollipop(
  data = lollipop_test,
  dep = "crp",
  group = "treatment_group",
  aggregation = "median",
  sortBy = "value_desc"
)

# Example 23: Sum aggregation
lollipop(
  data = lollipop_test,
  dep = "platelet_count",
  group = "hospital",
  aggregation = "sum",
  title = "Total Platelet Count by Hospital"
)

# ═══════════════════════════════════════════════════════════
# REGIONAL/DEMOGRAPHIC DATA
# ═══════════════════════════════════════════════════════════

# Example 24: Regional income comparison
lollipop(
  data = lollipop_aggregated,
  dep = "mean_income",
  group = "region",
  sortBy = "value_desc",
  showValues = TRUE,
  orientation = "horizontal",
  title = "Mean Income by Region",
  ylabel = "Region",
  xlabel = "Mean Income ($)"
)

# Example 25: Population density
lollipop(
  data = lollipop_aggregated,
  dep = "population_density",
  group = "region",
  sortBy = "value_desc",
  colorScheme = "viridis"
)

# ═══════════════════════════════════════════════════════════
# ADVANCED COMBINATIONS
# ═══════════════════════════════════════════════════════════

# Example 26: All features combined - Clinical lab analysis
lollipop(
  data = lollipop_test,
  dep = "hemoglobin",
  group = "treatment_group",
  useHighlight = TRUE,
  highlight = "Drug A",
  aggregation = "mean",
  sortBy = "value_desc",
  orientation = "horizontal",
  showValues = TRUE,
  showMean = TRUE,
  colorScheme = "clinical",
  theme = "publication",
  pointSize = 4,
  lineWidth = 2,
  lineType = "solid",
  baseline = 12,
  conditionalColor = TRUE,
  colorThreshold = 14,
  xlabel = "Hemoglobin Level (g/dL)",
  ylabel = "Treatment Groups",
  title = "Hemoglobin Response by Treatment (Normal >14 g/dL)",
  width = 1000,
  height = 600
)

# Example 27: Multi-lab parameter comparison - Different groups
lollipop(
  data = lollipop_test,
  dep = "alt",
  group = "age_group",
  aggregation = "median",
  sortBy = "value_desc",
  conditionalColor = TRUE,
  colorThreshold = 40,  # Upper normal limit
  showValues = TRUE,
  orientation = "horizontal",
  colorScheme = "clinical",
  title = "ALT Levels by Age Group (ULN: 40 U/L)"
)

# Example 28: Hospital comparison with baseline
lollipop(
  data = lollipop_test,
  dep = "creatinine",
  group = "hospital",
  aggregation = "mean",
  sortBy = "value_asc",
  baseline = 1.0,
  showMean = TRUE,
  showValues = TRUE,
  colorScheme = "colorblind",
  theme = "classic",
  title = "Mean Creatinine by Hospital"
)

# ═══════════════════════════════════════════════════════════
# CLINICAL WORKFLOW EXAMPLES
# ═══════════════════════════════════════════════════════════

# Example 29: Complete clinical lab panel analysis
# Step 1: Hemoglobin
lollipop(
  data = lollipop_test,
  dep = "hemoglobin",
  group = "treatment_group",
  aggregation = "mean",
  sortBy = "value_desc",
  showValues = TRUE,
  baseline = 12,
  conditionalColor = TRUE,
  colorThreshold = 14,
  colorScheme = "clinical",
  theme = "publication",
  title = "Hemoglobin by Treatment"
)

# Step 2: Platelet count
lollipop(
  data = lollipop_test,
  dep = "platelet_count",
  group = "treatment_group",
  aggregation = "mean",
  sortBy = "value_desc",
  showValues = TRUE,
  baseline = 150,
  conditionalColor = TRUE,
  colorThreshold = 450,
  colorScheme = "clinical",
  theme = "publication",
  title = "Platelet Count by Treatment"
)

# Example 30: Treatment efficacy and safety dashboard
# Efficacy: Tumor reduction
lollipop(
  data = lollipop_treatment,
  dep = "tumor_reduction",
  group = "treatment",
  sortBy = "value_desc",
  showValues = TRUE,
  conditionalColor = TRUE,
  colorThreshold = 30,
  colorScheme = "viridis",
  theme = "publication",
  title = "Efficacy: Tumor Reduction"
)

# Safety: Side effects
lollipop(
  data = lollipop_treatment,
  dep = "side_effects",
  group = "treatment",
  sortBy = "value_asc",
  showValues = TRUE,
  conditionalColor = TRUE,
  colorThreshold = 20,
  colorScheme = "clinical",
  theme = "publication",
  title = "Safety: Side Effects (Lower is Better)"
)

# Patient benefit: QoL
lollipop(
  data = lollipop_treatment,
  dep = "qol_score",
  group = "treatment",
  sortBy = "value_desc",
  showValues = TRUE,
  baseline = 60,
  conditionalColor = TRUE,
  colorThreshold = 70,
  colorScheme = "viridis",
  theme = "publication",
  title = "Patient Benefit: Quality of Life"
)
