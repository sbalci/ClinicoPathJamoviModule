# ═══════════════════════════════════════════════════════════
# Example Usage: hullplot (Hull Plot - Group Visualization)
# ═══════════════════════════════════════════════════════════
#
# This file demonstrates comprehensive usage of the hullplot jamovi function
# which creates scatter plots with hull polygons around groups of data points.
#
# Created: 2026-01-05
# Test data: hullplot_test, hullplot_clusters, hullplot_overlap,
#            hullplot_outliers, hullplot_clinical, hullplot_small,
#            hullplot_fourgroup, hullplot_unbalanced

library(ClinicoPath)

# ═══════════════════════════════════════════════════════════
# Example 1: Basic Two-Group Hull Plot
# ═══════════════════════════════════════════════════════════
# Scenario: Visualize patient groups

data(hullplot_test, package = "ClinicoPath")

hullplot(
  data = hullplot_test,
  x_var = "x",
  y_var = "y",
  group_var = "group"
)

# ═══════════════════════════════════════════════════════════
# Example 2: With Group Labels
# ═══════════════════════════════════════════════════════════
# Scenario: Show group labels inside hulls

hullplot(
  data = hullplot_test,
  x_var = "x",
  y_var = "y",
  group_var = "group",
  show_labels = TRUE,
  plot_title = "Patient Group Visualization"
)

# ═══════════════════════════════════════════════════════════
# Example 3: Custom Hull Concavity
# ═══════════════════════════════════════════════════════════
# Scenario: Adjust hull tightness around points

hullplot(
  data = hullplot_test,
  x_var = "x",
  y_var = "y",
  group_var = "group",
  hull_concavity = 1.0,  # More concave hulls
  show_labels = TRUE,
  plot_title = "Concave Hull Boundaries"
)

# ═══════════════════════════════════════════════════════════
# Example 4: With Size Variable
# ═══════════════════════════════════════════════════════════
# Scenario: Point size represents additional variable

hullplot(
  data = hullplot_test,
  x_var = "x",
  y_var = "y",
  group_var = "group",
  size_var = "size_var",
  show_labels = TRUE,
  plot_title = "Hull Plot with Variable Point Sizes"
)

# ═══════════════════════════════════════════════════════════
# Example 5: With Separate Color Variable
# ═══════════════════════════════════════════════════════════
# Scenario: Color points by different variable than hull groups

hullplot(
  data = hullplot_test,
  x_var = "x",
  y_var = "y",
  group_var = "group",
  color_var = "color_category",
  show_labels = TRUE,
  plot_title = "Hull Groups with Independent Color Variable"
)

# ═══════════════════════════════════════════════════════════
# Example 6: Three Distinct Clusters
# ═══════════════════════════════════════════════════════════
# Scenario: Visualize well-separated clusters

data(hullplot_clusters, package = "ClinicoPath")

hullplot(
  data = hullplot_clusters,
  x_var = "x",
  y_var = "y",
  group_var = "cluster",
  show_labels = TRUE,
  hull_alpha = 0.3,
  plot_title = "Three Distinct Clusters"
)

# ═══════════════════════════════════════════════════════════
# Example 7: Clusters with Statistics
# ═══════════════════════════════════════════════════════════
# Scenario: Show group statistics in output

hullplot(
  data = hullplot_clusters,
  x_var = "x",
  y_var = "y",
  group_var = "cluster",
  show_labels = TRUE,
  show_statistics = TRUE,
  plot_title = "Clusters with Summary Statistics"
)

# ═══════════════════════════════════════════════════════════
# Example 8: Clusters with Size by Biomarker
# ═══════════════════════════════════════════════════════════
# Scenario: Point size represents biomarker level

hullplot(
  data = hullplot_clusters,
  x_var = "x",
  y_var = "y",
  group_var = "cluster",
  size_var = "biomarker",
  show_labels = TRUE,
  plot_title = "Clusters with Biomarker-Sized Points"
)

# ═══════════════════════════════════════════════════════════
# Example 9: Overlapping Groups
# ═══════════════════════════════════════════════════════════
# Scenario: Visualize groups with spatial overlap

data(hullplot_overlap, package = "ClinicoPath")

hullplot(
  data = hullplot_overlap,
  x_var = "x",
  y_var = "y",
  group_var = "treatment",
  hull_alpha = 0.2,  # More transparent for overlap
  show_labels = TRUE,
  plot_title = "Overlapping Treatment Groups"
)

# ═══════════════════════════════════════════════════════════
# Example 10: Overlapping with Confidence Ellipses
# ═══════════════════════════════════════════════════════════
# Scenario: Add confidence ellipses for statistical regions

hullplot(
  data = hullplot_overlap,
  x_var = "x",
  y_var = "y",
  group_var = "treatment",
  confidence_ellipses = TRUE,
  hull_alpha = 0.2,
  plot_title = "Hull Plot with Confidence Ellipses"
)

# ═══════════════════════════════════════════════════════════
# Example 11: Groups with Outlier Detection
# ═══════════════════════════════════════════════════════════
# Scenario: Identify outliers within groups

data(hullplot_outliers, package = "ClinicoPath")

hullplot(
  data = hullplot_outliers,
  x_var = "x",
  y_var = "y",
  group_var = "disease_type",
  outlier_detection = TRUE,
  show_labels = TRUE,
  plot_title = "Disease Groups with Outlier Detection"
)

# ═══════════════════════════════════════════════════════════
# Example 12: Outliers with Severity Sizing
# ═══════════════════════════════════════════════════════════
# Scenario: Size points by severity score

hullplot(
  data = hullplot_outliers,
  x_var = "x",
  y_var = "y",
  group_var = "disease_type",
  size_var = "severity",
  outlier_detection = TRUE,
  show_labels = TRUE,
  plot_title = "Disease Groups with Severity Indicators"
)

# ═══════════════════════════════════════════════════════════
# Example 13: Clinical Trial Data
# ═══════════════════════════════════════════════════════════
# Scenario: Tumor volume vs Ki-67 by treatment arm

data(hullplot_clinical, package = "ClinicoPath")

hullplot(
  data = hullplot_clinical,
  x_var = "tumor_volume",
  y_var = "ki67_index",
  group_var = "treatment_arm",
  show_labels = TRUE,
  x_label = "Tumor Volume (cm³)",
  y_label = "Ki-67 Index (%)",
  plot_title = "Clinical Trial: Tumor Characteristics by Treatment"
)

# ═══════════════════════════════════════════════════════════
# Example 14: Clinical Data with Statistics
# ═══════════════════════════════════════════════════════════
# Scenario: Include group statistics in output

hullplot(
  data = hullplot_clinical,
  x_var = "tumor_volume",
  y_var = "ki67_index",
  group_var = "treatment_arm",
  show_labels = TRUE,
  show_statistics = TRUE,
  color_palette = "clinical",
  x_label = "Tumor Volume (cm³)",
  y_label = "Ki-67 Index (%)",
  plot_title = "Treatment Arms with Summary Statistics"
)

# ═══════════════════════════════════════════════════════════
# Example 15: Clinical Data with Response Status Coloring
# ═══════════════════════════════════════════════════════════
# Scenario: Hull by treatment, color by response

hullplot(
  data = hullplot_clinical,
  x_var = "tumor_volume",
  y_var = "ki67_index",
  group_var = "treatment_arm",
  color_var = "response_status",
  show_labels = TRUE,
  plot_title = "Treatment Groups Colored by Response Status"
)

# ═══════════════════════════════════════════════════════════
# Example 16: Small Sample Size
# ═══════════════════════════════════════════════════════════
# Scenario: Hull plot with limited data (n=30)

data(hullplot_small, package = "ClinicoPath")

hullplot(
  data = hullplot_small,
  x_var = "measurement_x",
  y_var = "measurement_y",
  group_var = "category",
  show_labels = TRUE,
  plot_title = "Small Sample Hull Plot (n=30)"
)

# ═══════════════════════════════════════════════════════════
# Example 17: Four-Group Gene Expression
# ═══════════════════════════════════════════════════════════
# Scenario: Molecular subtypes in quadrant arrangement

data(hullplot_fourgroup, package = "ClinicoPath")

hullplot(
  data = hullplot_fourgroup,
  x_var = "gene_a",
  y_var = "gene_b",
  group_var = "subtype",
  show_labels = TRUE,
  x_label = "Gene A Expression",
  y_label = "Gene B Expression",
  plot_title = "Molecular Subtypes by Gene Expression"
)

# ═══════════════════════════════════════════════════════════
# Example 18: Four Groups with Survival Data
# ═══════════════════════════════════════════════════════════
# Scenario: Point size represents survival duration

hullplot(
  data = hullplot_fourgroup,
  x_var = "gene_a",
  y_var = "gene_b",
  group_var = "subtype",
  size_var = "survival_months",
  show_labels = TRUE,
  x_label = "Gene A Expression",
  y_label = "Gene B Expression",
  plot_title = "Molecular Subtypes with Survival Duration"
)

# ═══════════════════════════════════════════════════════════
# Example 19: Four Groups with Different Color Palette
# ═══════════════════════════════════════════════════════════
# Scenario: Use viridis color palette

hullplot(
  data = hullplot_fourgroup,
  x_var = "gene_a",
  y_var = "gene_b",
  group_var = "subtype",
  color_palette = "viridis",
  show_labels = TRUE,
  plot_title = "Molecular Subtypes (Viridis Palette)"
)

# ═══════════════════════════════════════════════════════════
# Example 20: Unbalanced Groups
# ═══════════════════════════════════════════════════════════
# Scenario: Risk groups with different sample sizes

data(hullplot_unbalanced, package = "ClinicoPath")

hullplot(
  data = hullplot_unbalanced,
  x_var = "biomarker1",
  y_var = "biomarker2",
  group_var = "risk_group",
  show_labels = TRUE,
  x_label = "Biomarker 1",
  y_label = "Biomarker 2",
  plot_title = "Risk Groups (Unbalanced Sizes)"
)

# ═══════════════════════════════════════════════════════════
# Example 21: Unbalanced with Age Sizing
# ═══════════════════════════════════════════════════════════
# Scenario: Point size by patient age

hullplot(
  data = hullplot_unbalanced,
  x_var = "biomarker1",
  y_var = "biomarker2",
  group_var = "risk_group",
  size_var = "age",
  show_labels = TRUE,
  plot_title = "Risk Groups with Age-Sized Points"
)

# ═══════════════════════════════════════════════════════════
# Example 22: Custom Point and Hull Aesthetics
# ═══════════════════════════════════════════════════════════
# Scenario: Fine-tune visual appearance

hullplot(
  data = hullplot_test,
  x_var = "x",
  y_var = "y",
  group_var = "group",
  point_size = 3,
  point_alpha = 0.8,
  hull_alpha = 0.2,
  hull_concavity = 1.5,
  show_labels = TRUE,
  plot_title = "Custom Aesthetics"
)

# ═══════════════════════════════════════════════════════════
# Example 23: Expanded Hull Boundaries
# ═══════════════════════════════════════════════════════════
# Scenario: Increase hull boundary expansion

hullplot(
  data = hullplot_test,
  x_var = "x",
  y_var = "y",
  group_var = "group",
  hull_expand = 0.15,  # Larger expansion
  show_labels = TRUE,
  plot_title = "Expanded Hull Boundaries"
)

# ═══════════════════════════════════════════════════════════
# Example 24: Different Plot Themes
# ═══════════════════════════════════════════════════════════

# Classic theme
hullplot(
  data = hullplot_clusters,
  x_var = "x",
  y_var = "y",
  group_var = "cluster",
  plot_theme = "classic",
  show_labels = TRUE,
  plot_title = "Classic Theme"
)

# Dark theme
hullplot(
  data = hullplot_clusters,
  x_var = "x",
  y_var = "y",
  group_var = "cluster",
  plot_theme = "dark",
  show_labels = TRUE,
  plot_title = "Dark Theme"
)

# Clinical theme
hullplot(
  data = hullplot_clusters,
  x_var = "x",
  y_var = "y",
  group_var = "cluster",
  plot_theme = "clinical",
  color_palette = "clinical",
  show_labels = TRUE,
  plot_title = "Clinical Theme"
)

# ═══════════════════════════════════════════════════════════
# Example 25: Natural Language Summary
# ═══════════════════════════════════════════════════════════
# Scenario: Generate interpretable text summary

hullplot(
  data = hullplot_clinical,
  x_var = "tumor_volume",
  y_var = "ki67_index",
  group_var = "treatment_arm",
  show_summary = TRUE,
  plot_title = "Clinical Trial with Natural Language Summary"
)

# ═══════════════════════════════════════════════════════════
# Example 26: Assumptions and Guidelines
# ═══════════════════════════════════════════════════════════
# Scenario: Display data requirements and usage guidelines

hullplot(
  data = hullplot_test,
  x_var = "x",
  y_var = "y",
  group_var = "group",
  show_assumptions = TRUE,
  plot_title = "Hull Plot with Assumptions Display"
)

# ═══════════════════════════════════════════════════════════
# Example 27: Complete Publication Figure
# ═══════════════════════════════════════════════════════════
# Scenario: Publication-ready hull plot with all features

hullplot(
  data = hullplot_clinical,
  x_var = "tumor_volume",
  y_var = "ki67_index",
  group_var = "treatment_arm",
  show_labels = TRUE,
  show_statistics = TRUE,
  confidence_ellipses = TRUE,
  hull_concavity = 1.8,
  hull_alpha = 0.25,
  point_size = 2.5,
  point_alpha = 0.75,
  color_palette = "clinical",
  plot_theme = "minimal",
  x_label = "Tumor Volume (cm³)",
  y_label = "Ki-67 Proliferation Index (%)",
  plot_title = "Treatment Response: Tumor Characteristics",
  show_summary = TRUE
)

# ═══════════════════════════════════════════════════════════
# Example 28: Minimal Concavity (Convex Hulls)
# ═══════════════════════════════════════════════════════════
# Scenario: Maximum convexity (standard convex hull)

hullplot(
  data = hullplot_fourgroup,
  x_var = "gene_a",
  y_var = "gene_b",
  group_var = "subtype",
  hull_concavity = 2.0,  # Maximum convexity
  show_labels = TRUE,
  plot_title = "Convex Hulls (Maximum Concavity = 2.0)"
)

# ═══════════════════════════════════════════════════════════
# Example 29: Maximum Concavity (Tight Fit)
# ═══════════════════════════════════════════════════════════
# Scenario: Tightest possible hull fit

hullplot(
  data = hullplot_fourgroup,
  x_var = "gene_a",
  y_var = "gene_b",
  group_var = "subtype",
  hull_concavity = 0.5,  # High concavity (tight fit)
  show_labels = TRUE,
  plot_title = "Concave Hulls (Low Concavity = 0.5)"
)

# ═══════════════════════════════════════════════════════════
# Example 30: Without Group Labels
# ═══════════════════════════════════════════════════════════
# Scenario: Clean visualization without labels

hullplot(
  data = hullplot_clusters,
  x_var = "x",
  y_var = "y",
  group_var = "cluster",
  show_labels = FALSE,
  hull_alpha = 0.3,
  plot_title = "Clean Hull Visualization (No Labels)"
)
