# ═══════════════════════════════════════════════════════════
# Example Usage: jjsegmentedtotalbar
# ═══════════════════════════════════════════════════════════
#
# Comprehensive examples for the jjsegmentedtotalbar jamovi function
# Generated: 2026-01-06

library(ClinicoPath)

# Load test datasets
data(jjsegmentedtotalbar_test)
data(jjsegmentedtotalbar_demographics)
data(jjsegmentedtotalbar_biomarker)
data(jjsegmentedtotalbar_quality)
data(jjsegmentedtotalbar_temporal)

# ═══════════════════════════════════════════════════════════
# BASIC SEGMENTED TOTAL BAR CHARTS
# ═══════════════════════════════════════════════════════════

# Example 1: Simple treatment response over time
jjsegmentedtotalbar(
  data = jjsegmentedtotalbar_test,
  x_var = "timepoint",
  y_var = "tumor_response_score",
  fill_var = "response_category"
)

# Example 2: Demographics distribution by center
jjsegmentedtotalbar(
  data = jjsegmentedtotalbar_demographics,
  x_var = "treatment_center",
  y_var = "patient_count",
  fill_var = "age_group"
)

# Example 3: Biomarker expression by disease stage
jjsegmentedtotalbar(
  data = jjsegmentedtotalbar_biomarker,
  x_var = "disease_stage",
  y_var = "expression_score",
  fill_var = "expression_level"
)

# ═══════════════════════════════════════════════════════════
# PLOT TYPES
# ═══════════════════════════════════════════════════════════

# Example 4: Traditional stacked bar (100%)
jjsegmentedtotalbar(
  data = jjsegmentedtotalbar_test,
  x_var = "timepoint",
  y_var = "tumor_response_score",
  fill_var = "response_category",
  plot_type = "stacked"
)

# Example 5: Flerlage segmented total bar
jjsegmentedtotalbar(
  data = jjsegmentedtotalbar_quality,
  x_var = "hospital",
  y_var = "compliance_score",
  fill_var = "quality_grade",
  plot_type = "flerlage"
)

# ═══════════════════════════════════════════════════════════
# CHART STYLES
# ═══════════════════════════════════════════════════════════

# Example 6: Clean style (minimal)
jjsegmentedtotalbar(
  data = jjsegmentedtotalbar_demographics,
  x_var = "treatment_center",
  y_var = "patient_count",
  fill_var = "gender",
  chart_style = "clean"
)

# Example 7: Publication style
jjsegmentedtotalbar(
  data = jjsegmentedtotalbar_test,
  x_var = "timepoint",
  y_var = "tumor_response_score",
  fill_var = "response_category",
  chart_style = "publication"
)

# Example 8: Presentation style (bold, large text)
jjsegmentedtotalbar(
  data = jjsegmentedtotalbar_biomarker,
  x_var = "disease_stage",
  y_var = "expression_score",
  fill_var = "expression_level",
  chart_style = "presentation"
)

# Example 9: Clinical style
jjsegmentedtotalbar(
  data = jjsegmentedtotalbar_quality,
  x_var = "quarter",
  y_var = "compliance_score",
  fill_var = "quality_grade",
  chart_style = "clinical"
)

# Example 10: BBC style
jjsegmentedtotalbar(
  data = jjsegmentedtotalbar_demographics,
  x_var = "treatment_center",
  y_var = "patient_count",
  fill_var = "ethnicity",
  chart_style = "bbc_style"
)

# Example 11: Prism style
jjsegmentedtotalbar(
  data = jjsegmentedtotalbar_temporal,
  x_var = "time_period",
  y_var = "disease_burden_score",
  fill_var = "disease_status",
  chart_style = "prism_style"
)

# ═══════════════════════════════════════════════════════════
# COLOR PALETTES
# ═══════════════════════════════════════════════════════════

# Example 12: Viridis palette (perceptually uniform)
jjsegmentedtotalbar(
  data = jjsegmentedtotalbar_biomarker,
  x_var = "disease_stage",
  y_var = "expression_score",
  fill_var = "expression_level",
  color_palette = "viridis"
)

# Example 13: Clinical palette
jjsegmentedtotalbar(
  data = jjsegmentedtotalbar_test,
  x_var = "treatment",
  y_var = "tumor_response_score",
  fill_var = "response_category",
  color_palette = "clinical"
)

# Example 14: Colorblind-friendly palette
jjsegmentedtotalbar(
  data = jjsegmentedtotalbar_demographics,
  x_var = "treatment_center",
  y_var = "patient_count",
  fill_var = "age_group",
  color_palette = "colorblind"
)

# Example 15: Nature journal palette
jjsegmentedtotalbar(
  data = jjsegmentedtotalbar_temporal,
  x_var = "intervention",
  y_var = "disease_burden_score",
  fill_var = "disease_status",
  color_palette = "nature"
)

# Example 16: Science journal palette
jjsegmentedtotalbar(
  data = jjsegmentedtotalbar_biomarker,
  x_var = "tumor_type",
  y_var = "expression_score",
  fill_var = "biomarker",
  color_palette = "science"
)

# ═══════════════════════════════════════════════════════════
# LABELS AND PERCENTAGES
# ═══════════════════════════════════════════════════════════

# Example 17: Show percentages (integer format)
jjsegmentedtotalbar(
  data = jjsegmentedtotalbar_test,
  x_var = "timepoint",
  y_var = "tumor_response_score",
  fill_var = "response_category",
  show_percentages = TRUE,
  percentage_format = "integer"
)

# Example 18: Show percentages (one decimal)
jjsegmentedtotalbar(
  data = jjsegmentedtotalbar_demographics,
  x_var = "treatment_center",
  y_var = "patient_count",
  fill_var = "age_group",
  show_percentages = TRUE,
  percentage_format = "decimal1"
)

# Example 19: Show percentages (two decimals)
jjsegmentedtotalbar(
  data = jjsegmentedtotalbar_quality,
  x_var = "hospital",
  y_var = "compliance_score",
  fill_var = "quality_grade",
  show_percentages = TRUE,
  percentage_format = "decimal2"
)

# Example 20: Show both percentages and counts
jjsegmentedtotalbar(
  data = jjsegmentedtotalbar_demographics,
  x_var = "treatment_center",
  y_var = "patient_count",
  fill_var = "gender",
  show_percentages = TRUE,
  show_counts = TRUE
)

# Example 21: Label threshold (only show >10%)
jjsegmentedtotalbar(
  data = jjsegmentedtotalbar_biomarker,
  x_var = "disease_stage",
  y_var = "expression_score",
  fill_var = "expression_level",
  show_percentages = TRUE,
  label_threshold = 10
)

# ═══════════════════════════════════════════════════════════
# ORIENTATION
# ═══════════════════════════════════════════════════════════

# Example 22: Vertical orientation (default)
jjsegmentedtotalbar(
  data = jjsegmentedtotalbar_test,
  x_var = "timepoint",
  y_var = "tumor_response_score",
  fill_var = "response_category",
  orientation = "vertical"
)

# Example 23: Horizontal orientation
jjsegmentedtotalbar(
  data = jjsegmentedtotalbar_quality,
  x_var = "hospital",
  y_var = "compliance_score",
  fill_var = "quality_grade",
  orientation = "horizontal",
  show_percentages = TRUE
)

# ═══════════════════════════════════════════════════════════
# SORTING
# ═══════════════════════════════════════════════════════════

# Example 24: No sorting (preserve original order)
jjsegmentedtotalbar(
  data = jjsegmentedtotalbar_test,
  x_var = "timepoint",
  y_var = "tumor_response_score",
  fill_var = "response_category",
  sort_categories = "none"
)

# Example 25: Sort by total (highest to lowest)
jjsegmentedtotalbar(
  data = jjsegmentedtotalbar_demographics,
  x_var = "treatment_center",
  y_var = "patient_count",
  fill_var = "age_group",
  sort_categories = "total"
)

# Example 26: Sort by largest segment
jjsegmentedtotalbar(
  data = jjsegmentedtotalbar_biomarker,
  x_var = "tumor_type",
  y_var = "expression_score",
  fill_var = "expression_level",
  sort_categories = "largest_segment"
)

# Example 27: Alphabetical sorting
jjsegmentedtotalbar(
  data = jjsegmentedtotalbar_temporal,
  x_var = "intervention",
  y_var = "disease_burden_score",
  fill_var = "disease_status",
  sort_categories = "alpha"
)

# ═══════════════════════════════════════════════════════════
# FACETING
# ═══════════════════════════════════════════════════════════

# Example 28: Facet by treatment
jjsegmentedtotalbar(
  data = jjsegmentedtotalbar_test,
  x_var = "timepoint",
  y_var = "tumor_response_score",
  fill_var = "response_category",
  facet_var = "treatment"
)

# Example 29: Facet by gender
jjsegmentedtotalbar(
  data = jjsegmentedtotalbar_demographics,
  x_var = "treatment_center",
  y_var = "patient_count",
  fill_var = "age_group",
  facet_var = "gender"
)

# Example 30: Facet by quarter
jjsegmentedtotalbar(
  data = jjsegmentedtotalbar_quality,
  x_var = "hospital",
  y_var = "compliance_score",
  fill_var = "quality_grade",
  facet_var = "quarter"
)

# ═══════════════════════════════════════════════════════════
# STATISTICAL TESTS
# ═══════════════════════════════════════════════════════════

# Example 31: Chi-square test for proportion differences
jjsegmentedtotalbar(
  data = jjsegmentedtotalbar_test,
  x_var = "treatment",
  y_var = "tumor_response_score",
  fill_var = "response_category",
  show_statistical_tests = TRUE
)

# Example 32: Statistical test with faceting
jjsegmentedtotalbar(
  data = jjsegmentedtotalbar_biomarker,
  x_var = "disease_stage",
  y_var = "expression_score",
  fill_var = "expression_level",
  facet_var = "biomarker",
  show_statistical_tests = TRUE
)

# ═══════════════════════════════════════════════════════════
# FLERLAGE PLOT CUSTOMIZATION
# ═══════════════════════════════════════════════════════════

# Example 33: Flerlage with labels
jjsegmentedtotalbar(
  data = jjsegmentedtotalbar_quality,
  x_var = "hospital",
  y_var = "compliance_score",
  fill_var = "quality_grade",
  plot_type = "flerlage",
  flerlage_show_labels = TRUE
)

# Example 34: Flerlage with custom label size
jjsegmentedtotalbar(
  data = jjsegmentedtotalbar_temporal,
  x_var = "time_period",
  y_var = "disease_burden_score",
  fill_var = "disease_status",
  plot_type = "flerlage",
  flerlage_show_labels = TRUE,
  flerlage_label_size = 8
)

# Example 35: Flerlage with custom label color
jjsegmentedtotalbar(
  data = jjsegmentedtotalbar_test,
  x_var = "timepoint",
  y_var = "tumor_response_score",
  fill_var = "response_category",
  plot_type = "flerlage",
  flerlage_show_labels = TRUE,
  flerlage_label_color = "white"
)

# Example 36: Flerlage with transparency
jjsegmentedtotalbar(
  data = jjsegmentedtotalbar_biomarker,
  x_var = "disease_stage",
  y_var = "expression_score",
  fill_var = "expression_level",
  plot_type = "flerlage",
  flerlage_alpha = 0.7
)

# Example 37: Flerlage with custom box color
jjsegmentedtotalbar(
  data = jjsegmentedtotalbar_demographics,
  x_var = "treatment_center",
  y_var = "patient_count",
  fill_var = "age_group",
  plot_type = "flerlage",
  flerlage_box_color = "gray30"
)

# Example 38: Flerlage complete customization
jjsegmentedtotalbar(
  data = jjsegmentedtotalbar_quality,
  x_var = "hospital",
  y_var = "compliance_score",
  fill_var = "quality_grade",
  plot_type = "flerlage",
  flerlage_show_labels = TRUE,
  flerlage_label_size = 7,
  flerlage_label_color = "white",
  flerlage_alpha = 0.85,
  flerlage_box_color = "gray20"
)

# ═══════════════════════════════════════════════════════════
# COMPLETE CLINICAL WORKFLOWS
# ═══════════════════════════════════════════════════════════

# Example 39: Treatment response analysis (publication-ready)
jjsegmentedtotalbar(
  data = jjsegmentedtotalbar_test,
  x_var = "timepoint",
  y_var = "tumor_response_score",
  fill_var = "response_category",
  facet_var = "treatment",
  chart_style = "publication",
  color_palette = "clinical",
  show_percentages = TRUE,
  percentage_format = "decimal1",
  show_counts = TRUE,
  label_threshold = 5,
  orientation = "vertical",
  sort_categories = "none",
  show_statistical_tests = TRUE
)

# Example 40: Demographics analysis (clinical report)
jjsegmentedtotalbar(
  data = jjsegmentedtotalbar_demographics,
  x_var = "treatment_center",
  y_var = "patient_count",
  fill_var = "age_group",
  facet_var = "gender",
  chart_style = "clinical",
  color_palette = "colorblind",
  show_percentages = TRUE,
  percentage_format = "integer",
  show_counts = TRUE,
  orientation = "vertical",
  sort_categories = "total"
)

# Example 41: Biomarker distribution (horizontal layout)
jjsegmentedtotalbar(
  data = jjsegmentedtotalbar_biomarker,
  x_var = "disease_stage",
  y_var = "expression_score",
  fill_var = "expression_level",
  facet_var = "biomarker",
  chart_style = "publication",
  color_palette = "viridis",
  show_percentages = TRUE,
  percentage_format = "decimal1",
  label_threshold = 5,
  orientation = "horizontal",
  sort_categories = "largest_segment",
  show_statistical_tests = TRUE
)

# Example 42: Quality metrics (Flerlage style)
jjsegmentedtotalbar(
  data = jjsegmentedtotalbar_quality,
  x_var = "hospital",
  y_var = "compliance_score",
  fill_var = "quality_grade",
  facet_var = "quarter",
  plot_type = "flerlage",
  chart_style = "clinical",
  color_palette = "clinical",
  flerlage_show_labels = TRUE,
  flerlage_label_size = 6,
  flerlage_label_color = "white",
  flerlage_alpha = 0.85,
  flerlage_box_color = "gray30",
  orientation = "horizontal",
  sort_categories = "total"
)

# Example 43: Temporal disease progression
jjsegmentedtotalbar(
  data = jjsegmentedtotalbar_temporal,
  x_var = "time_period",
  y_var = "disease_burden_score",
  fill_var = "disease_status",
  facet_var = "intervention",
  chart_style = "publication",
  color_palette = "nature",
  show_percentages = TRUE,
  percentage_format = "decimal1",
  label_threshold = 5,
  orientation = "vertical",
  sort_categories = "none",
  show_statistical_tests = TRUE
)

# Example 44: Ethnicity distribution (BBC style)
jjsegmentedtotalbar(
  data = jjsegmentedtotalbar_demographics,
  x_var = "treatment_center",
  y_var = "patient_count",
  fill_var = "ethnicity",
  chart_style = "bbc_style",
  color_palette = "bbc_multi",
  show_percentages = TRUE,
  percentage_format = "integer",
  show_counts = TRUE,
  label_threshold = 5,
  orientation = "horizontal",
  sort_categories = "total"
)

# Example 45: ECOG performance status distribution
jjsegmentedtotalbar(
  data = jjsegmentedtotalbar_demographics,
  x_var = "treatment_center",
  y_var = "patient_count",
  fill_var = "ecog_performance_status",
  facet_var = "gender",
  chart_style = "clinical",
  color_palette = "clinical",
  show_percentages = TRUE,
  show_counts = TRUE,
  orientation = "vertical"
)

# Example 46: Biomarker by tumor type
jjsegmentedtotalbar(
  data = jjsegmentedtotalbar_biomarker,
  x_var = "tumor_type",
  y_var = "expression_score",
  fill_var = "expression_level",
  facet_var = "disease_stage",
  chart_style = "publication",
  color_palette = "viridis",
  show_percentages = TRUE,
  orientation = "horizontal",
  sort_categories = "total"
)

# Example 47: Quality by metric type
jjsegmentedtotalbar(
  data = jjsegmentedtotalbar_quality,
  x_var = "quarter",
  y_var = "compliance_score",
  fill_var = "quality_grade",
  facet_var = "metric_type",
  chart_style = "publication",
  color_palette = "nature",
  show_percentages = TRUE,
  percentage_format = "decimal1",
  show_counts = TRUE,
  label_threshold = 5,
  orientation = "vertical"
)

# Example 48: Intervention comparison
jjsegmentedtotalbar(
  data = jjsegmentedtotalbar_temporal,
  x_var = "intervention",
  y_var = "disease_burden_score",
  fill_var = "disease_status",
  facet_var = "time_period",
  chart_style = "clinical",
  color_palette = "clinical",
  show_percentages = TRUE,
  show_counts = TRUE,
  orientation = "horizontal",
  sort_categories = "largest_segment"
)

# Example 49: Disease stage comparison (Prism style)
jjsegmentedtotalbar(
  data = jjsegmentedtotalbar_test,
  x_var = "disease_stage",
  y_var = "tumor_response_score",
  fill_var = "response_category",
  facet_var = "treatment",
  chart_style = "prism_style",
  color_palette = "prism_colorblind_safe",
  show_percentages = TRUE,
  show_statistical_tests = TRUE
)

# Example 50: Complete presentation figure
jjsegmentedtotalbar(
  data = jjsegmentedtotalbar_test,
  x_var = "timepoint",
  y_var = "tumor_response_score",
  fill_var = "response_category",
  facet_var = "treatment",
  chart_style = "presentation",
  color_palette = "clinical",
  show_percentages = TRUE,
  percentage_format = "integer",
  label_threshold = 8,
  orientation = "vertical",
  sort_categories = "none"
)
