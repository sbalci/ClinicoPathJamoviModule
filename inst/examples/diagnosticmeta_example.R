# ═══════════════════════════════════════════════════════════
# Example Usage: diagnosticmeta
# ═══════════════════════════════════════════════════════════
#
# Comprehensive examples for diagnostic test meta-analysis
# in pathology research using the diagnosticmeta jamovi function

library(ClinicoPath)

# ───────────────────────────────────────────────────────────
# Example 1: Basic Diagnostic Test Meta-Analysis
# ───────────────────────────────────────────────────────────

# Load test data
data(diagnosticmeta_test)

# Basic bivariate meta-analysis
basic_result <- diagnosticmeta(
  data = diagnosticmeta_test,
  study = "study",
  true_positives = "true_positives",
  false_positives = "false_positives",
  false_negatives = "false_negatives",
  true_negatives = "true_negatives",
  bivariate_analysis = TRUE
)

# View results
# basic_result

# ───────────────────────────────────────────────────────────
# Example 2: Meta-Analysis with Heterogeneity Assessment
# ───────────────────────────────────────────────────────────

heterogeneity_result <- diagnosticmeta(
  data = diagnosticmeta_test,
  study = "study",
  true_positives = "true_positives",
  false_positives = "false_positives",
  false_negatives = "false_negatives",
  true_negatives = "true_negatives",
  bivariate_analysis = TRUE,
  heterogeneity_analysis = TRUE,
  confidence_level = 95
)

# ───────────────────────────────────────────────────────────
# Example 3: Meta-Regression with Continuous Covariate
# ───────────────────────────────────────────────────────────

# Explore whether publication year affects diagnostic accuracy
metareg_continuous <- diagnosticmeta(
  data = diagnosticmeta_test,
  study = "study",
  true_positives = "true_positives",
  false_positives = "false_positives",
  false_negatives = "false_negatives",
  true_negatives = "true_negatives",
  bivariate_analysis = TRUE,
  meta_regression = TRUE,
  covariate = "year",
  heterogeneity_analysis = TRUE
)

# ───────────────────────────────────────────────────────────
# Example 4: Meta-Regression with Categorical Covariate
# ───────────────────────────────────────────────────────────

# Load data with categorical covariate
data(diagnosticmeta_test_categorical)

# Explore whether imaging modality affects diagnostic accuracy
metareg_categorical <- diagnosticmeta(
  data = diagnosticmeta_test_categorical,
  study = "study",
  true_positives = "true_positives",
  false_positives = "false_positives",
  false_negatives = "false_negatives",
  true_negatives = "true_negatives",
  bivariate_analysis = TRUE,
  meta_regression = TRUE,
  covariate = "imaging_modality",
  heterogeneity_analysis = TRUE
)

# ───────────────────────────────────────────────────────────
# Example 5: HSROC Analysis
# ───────────────────────────────────────────────────────────

# Hierarchical summary ROC analysis
hsroc_result <- diagnosticmeta(
  data = diagnosticmeta_test,
  study = "study",
  true_positives = "true_positives",
  false_positives = "false_positives",
  false_negatives = "false_negatives",
  true_negatives = "true_negatives",
  bivariate_analysis = TRUE,
  hsroc_analysis = TRUE,
  sroc_plot = TRUE
)

# ───────────────────────────────────────────────────────────
# Example 6: Publication Bias Assessment
# ───────────────────────────────────────────────────────────

# Assess publication bias using Deeks' funnel plot test
pubbias_result <- diagnosticmeta(
  data = diagnosticmeta_test,
  study = "study",
  true_positives = "true_positives",
  false_positives = "false_positives",
  false_negatives = "false_negatives",
  true_negatives = "true_negatives",
  publication_bias = TRUE,
  funnel_plot = TRUE
)

# ───────────────────────────────────────────────────────────
# Example 7: Zero-Cell Correction Methods
# ───────────────────────────────────────────────────────────

# Load data with zero cells
data(diagnosticmeta_test_zeros)

# Method 1: Model-based (recommended)
zero_none <- diagnosticmeta(
  data = diagnosticmeta_test_zeros,
  study = "study",
  true_positives = "true_positives",
  false_positives = "false_positives",
  false_negatives = "false_negatives",
  true_negatives = "true_negatives",
  zero_cell_correction = "none"
)

# Method 2: Constant correction (+0.5 to all cells)
zero_constant <- diagnosticmeta(
  data = diagnosticmeta_test_zeros,
  study = "study",
  true_positives = "true_positives",
  false_positives = "false_positives",
  false_negatives = "false_negatives",
  true_negatives = "true_negatives",
  zero_cell_correction = "constant"
)

# ───────────────────────────────────────────────────────────
# Example 8: Complete Meta-Analysis with All Features
# ───────────────────────────────────────────────────────────

comprehensive_result <- diagnosticmeta(
  data = diagnosticmeta_test,
  study = "study",
  true_positives = "true_positives",
  false_positives = "false_positives",
  false_negatives = "false_negatives",
  true_negatives = "true_negatives",
  # Analysis options
  bivariate_analysis = TRUE,
  hsroc_analysis = TRUE,
  meta_regression = TRUE,
  covariate = "year",
  heterogeneity_analysis = TRUE,
  publication_bias = TRUE,
  # Display options
  forest_plot = TRUE,
  sroc_plot = TRUE,
  funnel_plot = TRUE,
  show_individual_studies = TRUE,
  show_interpretation = TRUE,
  show_methodology = TRUE,
  show_analysis_summary = TRUE,
  # Plot options
  color_palette = "colorblind_safe",
  show_plot_explanations = TRUE,
  # Statistical options
  confidence_level = 95,
  method = "reml"
)

# ───────────────────────────────────────────────────────────
# Example 9: Comparing Different Estimation Methods
# ───────────────────────────────────────────────────────────

# REML (Restricted Maximum Likelihood) - Recommended
result_reml <- diagnosticmeta(
  data = diagnosticmeta_test,
  study = "study",
  true_positives = "true_positives",
  false_positives = "false_positives",
  false_negatives = "false_negatives",
  true_negatives = "true_negatives",
  method = "reml"
)

# Maximum Likelihood
result_ml <- diagnosticmeta(
  data = diagnosticmeta_test,
  study = "study",
  true_positives = "true_positives",
  false_positives = "false_positives",
  false_negatives = "false_negatives",
  true_negatives = "true_negatives",
  method = "ml"
)

# Fixed Effects (for sensitivity analysis)
result_fixed <- diagnosticmeta(
  data = diagnosticmeta_test,
  study = "study",
  true_positives = "true_positives",
  false_positives = "false_positives",
  false_negatives = "false_negatives",
  true_negatives = "true_negatives",
  method = "fixed"
)

# ───────────────────────────────────────────────────────────
# Example 10: Forest Plot with Different Color Palettes
# ───────────────────────────────────────────────────────────

# Standard colors
forest_standard <- diagnosticmeta(
  data = diagnosticmeta_test,
  study = "study",
  true_positives = "true_positives",
  false_positives = "false_positives",
  false_negatives = "false_negatives",
  true_negatives = "true_negatives",
  forest_plot = TRUE,
  color_palette = "standard"
)

# Color-blind safe palette
forest_cb_safe <- diagnosticmeta(
  data = diagnosticmeta_test,
  study = "study",
  true_positives = "true_positives",
  false_positives = "false_positives",
  false_negatives = "false_negatives",
  true_negatives = "true_negatives",
  forest_plot = TRUE,
  color_palette = "colorblind_safe"
)

# High contrast (for presentations)
forest_high_contrast <- diagnosticmeta(
  data = diagnosticmeta_test,
  study = "study",
  true_positives = "true_positives",
  false_positives = "false_positives",
  false_negatives = "false_negatives",
  true_negatives = "true_negatives",
  forest_plot = TRUE,
  color_palette = "high_contrast"
)

# ───────────────────────────────────────────────────────────
# Example 11: Study Quality as Covariate
# ───────────────────────────────────────────────────────────

# Explore whether study quality affects diagnostic accuracy
quality_metareg <- diagnosticmeta(
  data = diagnosticmeta_test,
  study = "study",
  true_positives = "true_positives",
  false_positives = "false_positives",
  false_negatives = "false_negatives",
  true_negatives = "true_negatives",
  meta_regression = TRUE,
  covariate = "quality_score",
  heterogeneity_analysis = TRUE
)

# ───────────────────────────────────────────────────────────
# Example 12: Workflow for Publication
# ───────────────────────────────────────────────────────────

# Step 1: Initial screening
initial <- diagnosticmeta(
  data = diagnosticmeta_test,
  study = "study",
  true_positives = "true_positives",
  false_positives = "false_positives",
  false_negatives = "false_negatives",
  true_negatives = "true_negatives"
)

# Step 2: Check heterogeneity
heterogeneity <- diagnosticmeta(
  data = diagnosticmeta_test,
  study = "study",
  true_positives = "true_positives",
  false_positives = "false_positives",
  false_negatives = "false_negatives",
  true_negatives = "true_negatives",
  heterogeneity_analysis = TRUE
)

# Step 3: If heterogeneity is high, explore with meta-regression
if_heterogeneous <- diagnosticmeta(
  data = diagnosticmeta_test,
  study = "study",
  true_positives = "true_positives",
  false_positives = "false_positives",
  false_negatives = "false_negatives",
  true_negatives = "true_negatives",
  meta_regression = TRUE,
  covariate = "year",
  heterogeneity_analysis = TRUE
)

# Step 4: Final comprehensive analysis for publication
publication_ready <- diagnosticmeta(
  data = diagnosticmeta_test,
  study = "study",
  true_positives = "true_positives",
  false_positives = "false_positives",
  false_negatives = "false_negatives",
  true_negatives = "true_negatives",
  bivariate_analysis = TRUE,
  hsroc_analysis = TRUE,
  heterogeneity_analysis = TRUE,
  publication_bias = TRUE,
  forest_plot = TRUE,
  sroc_plot = TRUE,
  funnel_plot = TRUE,
  show_individual_studies = TRUE,
  show_interpretation = TRUE,
  show_methodology = TRUE,
  color_palette = "colorblind_safe",
  confidence_level = 95
)

# ───────────────────────────────────────────────────────────
# Notes on Clinical Interpretation
# ───────────────────────────────────────────────────────────

# Sensitivity: Proportion of diseased correctly identified (true positive rate)
#   - High sensitivity: Good for screening tests (few false negatives)
#   - Clinical use: "When negative, rules OUT disease" (SnNOUT)

# Specificity: Proportion of healthy correctly identified (true negative rate)
#   - High specificity: Good for confirmation tests (few false positives)
#   - Clinical use: "When positive, rules IN disease" (SpPIN)

# Diagnostic Odds Ratio (DOR): Summary measure of test performance
#   - DOR > 1: Test discriminates between diseased and healthy
#   - Higher DOR: Better diagnostic performance

# Heterogeneity (I²): Variation in results across studies
#   - I² < 25%: Low heterogeneity
#   - I² 25-75%: Moderate heterogeneity
#   - I² > 75%: High heterogeneity (explore with meta-regression)

# Publication Bias: Tendency to publish positive results
#   - Deeks' test p < 0.05: Suggests publication bias
#   - Funnel plot asymmetry: Visual check for bias

# ───────────────────────────────────────────────────────────
# Tips for Pathologists
# ───────────────────────────────────────────────────────────

# 1. For AI algorithm validation:
#    - Use bivariate random-effects model (accounts for correlation)
#    - Check heterogeneity (different datasets/conditions)
#    - Explore covariates (imaging type, tissue type, staining)

# 2. For biomarker diagnostic accuracy:
#    - Compare across different thresholds (HSROC)
#    - Check publication bias (small studies may be selective)
#    - Consider meta-regression for protocol differences

# 3. For IHC marker validation:
#    - Account for inter-observer variability
#    - Check heterogeneity across laboratories
#    - Explore staining protocol differences

# 4. Reporting guidelines:
#    - Follow PRISMA-DTA guidelines
#    - Report pooled sensitivity and specificity with 95% CI
#    - Include forest plot, SROC curve, and funnel plot
#    - Discuss heterogeneity and meta-regression results

# ───────────────────────────────────────────────────────────
# Example Real-World Applications
# ───────────────────────────────────────────────────────────

# Application 1: PD-L1 IHC for immunotherapy selection
# Application 2: AI-based tumor detection in WSI
# Application 3: Liquid biopsy for cancer detection
# Application 4: Molecular biomarkers for diagnosis
# Application 5: Digital pathology algorithm validation
