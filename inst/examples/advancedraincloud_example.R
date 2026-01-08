# ═══════════════════════════════════════════════════════════
# Advanced Raincloud Plot Examples
# ═══════════════════════════════════════════════════════════
#
# This file demonstrates usage of the advancedraincloud function
# with various clinical scenarios and visualization options.

\dontrun{

# Load test data
data(advancedraincloud_test, package = "ClinicoPath")

# ═══════════════════════════════════════════════════════════
# Example 1: Basic Raincloud Plot (Cross-sectional)
# ═══════════════════════════════════════════════════════════

# Compare pain scores across treatment groups at baseline
baseline_data <- subset(advancedraincloud_test, timepoint == "Baseline")

advancedraincloud(
  data = baseline_data,
  y_var = "pain_score",
  x_var = "treatment",
  rain_side = "l",
  color_palette = "clinical",
  plot_title = "Baseline Pain Scores by Treatment Group",
  show_statistics = TRUE,
  show_comparisons = TRUE
)

# ═══════════════════════════════════════════════════════════
# Example 2: Longitudinal Analysis with Connections
# ═══════════════════════════════════════════════════════════

# Track individual patient trajectories over time
advancedraincloud(
  data = advancedraincloud_test,
  y_var = "pain_score",
  x_var = "timepoint",
  fill_var = "treatment",
  id_var = "patient_id",
  show_longitudinal = TRUE,
  rain_side = "f",
  color_palette = "viridis",
  plot_title = "Pain Score Trajectories by Treatment",
  show_statistics = TRUE
)

# ═══════════════════════════════════════════════════════════
# Example 3: Likert Scale Visualization
# ═══════════════════════════════════════════════════════════

# Visualize patient satisfaction (ordinal 1-7 scale)
advancedraincloud(
  data = advancedraincloud_test,
  y_var = "satisfaction",
  x_var = "treatment",
  likert_mode = TRUE,
  rain_side = "l",
  color_palette = "pastel",
  plot_title = "Patient Satisfaction by Treatment",
  y_label = "Satisfaction Score (1-7)",
  show_comparisons = TRUE
)

# ═══════════════════════════════════════════════════════════
# Example 4: Clinical Trial Analysis with Effect Sizes
# ═══════════════════════════════════════════════════════════

# Comprehensive trial analysis with change scores and effect sizes
advancedraincloud(
  data = advancedraincloud_test,
  y_var = "pain_score",
  x_var = "timepoint",
  fill_var = "treatment",
  show_effect_size = TRUE,
  effect_size_type = "cohens_d",
  show_change_scores = TRUE,
  baseline_group = "Baseline",
  clinical_cutoff = 50,
  show_mcid = TRUE,
  mcid_value = 10,
  responder_threshold = 20,
  plot_title = "Clinical Trial: Pain Score Analysis",
  show_sample_size = TRUE
)

# ═══════════════════════════════════════════════════════════
# Example 5: Biomarker Analysis with Log Transform
# ═══════════════════════════════════════════════════════════

# Analyze CRP levels (log-normal biomarker)
advancedraincloud(
  data = subset(advancedraincloud_test, timepoint == "Week 12"),
  y_var = "crp_level",
  x_var = "treatment",
  log_transform = TRUE,
  clinical_cutoff = 3,  # Clinical threshold at 3 mg/L
  reference_range_min = 0,
  reference_range_max = 3,
  show_cv_bands = TRUE,
  cv_band_1 = 15,
  cv_band_2 = 20,
  plot_title = "CRP Levels at Week 12",
  y_label = "CRP (mg/L, log scale)",
  color_palette = "set1",
  show_comparisons = TRUE
)

# ═══════════════════════════════════════════════════════════
# Example 6: Subgroup Analysis
# ═══════════════════════════════════════════════════════════

# Analyze treatment effects stratified by disease severity
week12_data <- subset(advancedraincloud_test, timepoint == "Week 12")

advancedraincloud(
  data = week12_data,
  y_var = "pain_score",
  x_var = "treatment",
  fill_var = "disease_severity",
  cov_var = "age",
  rain_side = "f2x2",  # Grouped flanking for 2x2 comparison
  plot_title = "Treatment Effects by Disease Severity",
  color_palette = "dark2",
  show_statistics = TRUE
)

# ═══════════════════════════════════════════════════════════
# Example 7: Paired Comparison (Flanking 1x1)
# ═══════════════════════════════════════════════════════════

# Compare baseline vs Week 12 for High Dose group
high_dose_data <- subset(
  advancedraincloud_test,
  treatment == "High Dose" & (timepoint == "Baseline" | timepoint == "Week 12")
)

advancedraincloud(
  data = high_dose_data,
  y_var = "pain_score",
  x_var = "timepoint",
  id_var = "patient_id",
  show_longitudinal = TRUE,
  rain_side = "f1x1",  # Paired flanking
  color_palette = "clinical",
  plot_title = "High Dose: Baseline vs Week 12",
  show_comparisons = TRUE,
  show_effect_size = TRUE
)

# ═══════════════════════════════════════════════════════════
# Example 8: Publication-Ready Plot with Journal Style
# ═══════════════════════════════════════════════════════════

# Create publication-ready figure with NEJM styling
advancedraincloud(
  data = advancedraincloud_test,
  y_var = "pain_score",
  x_var = "timepoint",
  fill_var = "treatment",
  journal_style = "nejm",
  p_value_position = "above",
  show_sample_size = TRUE,
  show_comparisons = TRUE,
  plot_title = "Pain Reduction Over Time by Treatment Arm",
  trial_arms = "Placebo,Low Dose,High Dose",
  time_labels = "Baseline,4 Weeks,12 Weeks",
  population_type = "itt",
  generate_report = TRUE,
  include_methods = TRUE
)

# ═══════════════════════════════════════════════════════════
# Example 9: Outlier Handling
# ═══════════════════════════════════════════════════════════

# Handle extreme values with IQR method
advancedraincloud(
  data = baseline_data,
  y_var = "pain_score",
  x_var = "treatment",
  outlier_method = "iqr",
  rain_side = "l",
  color_palette = "viridis",
  plot_title = "Pain Scores (Outliers Handled via IQR)",
  show_statistics = TRUE
)

# ═══════════════════════════════════════════════════════════
# Example 10: Custom Visual Parameters
# ═══════════════════════════════════════════════════════════

# Fine-tune visual appearance
advancedraincloud(
  data = baseline_data,
  y_var = "pain_score",
  x_var = "treatment",
  rain_side = "l",
  point_size = 2.5,
  point_alpha = 0.5,
  violin_alpha = 0.8,
  boxplot_width = 0.15,
  jitter_seed = 123,
  color_palette = "set2",
  plot_title = "Customized Raincloud Plot",
  x_label = "Treatment Arm",
  y_label = "Pain Score (0-100 VAS)"
)

}
