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
# Example 5: Proportional-Hazards SROC Analysis
# ───────────────────────────────────────────────────────────

# Holling proportional-hazards SROC model (mada::phm): relates sensitivity (p)
# and false-positive rate (u) through u^theta = p, reporting theta as the
# accuracy parameter and tau-squared as between-study variation. It is NOT the
# Rutter-Gatsonis HSROC model. The SROC curve itself comes from the bivariate
# model, not from this table.
phm_result <- diagnosticmeta(
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

# Method 1: Default (recommended) - no correction to the data itself.
#   The bivariate model still adds +0.5 to studies with a zero cell at fitting
#   time (mada's "single" correction), while the univariate heterogeneity and
#   meta-regression models exclude those studies instead, so they rest on fewer
#   studies. Both are disclosed in the output.
zero_none <- diagnosticmeta(
  data = diagnosticmeta_test_zeros,
  study = "study",
  true_positives = "true_positives",
  false_positives = "false_positives",
  false_negatives = "false_negatives",
  true_negatives = "true_negatives",
  zero_cell_correction = "none"
)

# Method 2: +0.5 to all four cells of the studies that CONTAIN a zero cell
#   (studies without a zero cell are left untouched), applied before any analysis
zero_constant <- diagnosticmeta(
  data = diagnosticmeta_test_zeros,
  study = "study",
  true_positives = "true_positives",
  false_positives = "false_positives",
  false_negatives = "false_negatives",
  true_negatives = "true_negatives",
  zero_cell_correction = "constant"
)

# Method 3: +0.5 to the zero cells only (the non-zero cells of the same study
#   keep their observed counts)
zero_cells_only <- diagnosticmeta(
  data = diagnosticmeta_test_zeros,
  study = "study",
  true_positives = "true_positives",
  false_positives = "false_positives",
  false_negatives = "false_negatives",
  true_negatives = "true_negatives",
  zero_cell_correction = "zero_cells"
)

# Method 4: +1/N to all cells of the affected studies, N = that study's total
#   sample size (a smaller nudge in large studies than a flat +0.5)
zero_reciprocal <- diagnosticmeta(
  data = diagnosticmeta_test_zeros,
  study = "study",
  true_positives = "true_positives",
  false_positives = "false_positives",
  false_negatives = "false_negatives",
  true_negatives = "true_negatives",
  zero_cell_correction = "reciprocal_n"
)

# ───────────────────────────────────────────────────────────
# Example 7b: Zero Cells in Deeks' Test and the Funnel Plot
# ───────────────────────────────────────────────────────────

# Deeks' test and the funnel plot see the data AFTER zero_cell_correction has
# been applied. If a zero cell still remains at that point - which happens only
# under the default "none" - they both add +0.5 to EVERY study, not just the
# affected ones. Why uniformly? Correcting only the affected studies shrinks
# their log DOR while leaving the rest alone, and those studies are the small,
# near-perfect ones at the low effective-sample-size end of the regression,
# which builds a size-related trend into the test's own outcome variable. The
# table note states how many studies had a zero cell.
#
# "constant", "zero_cells" and "reciprocal_n" each leave no zero cell, so this
# uniform step never fires under them and a verdict is always reported.
deeks_with_zeros <- diagnosticmeta(
  data = diagnosticmeta_test_zeros,
  study = "study",
  true_positives = "true_positives",
  false_positives = "false_positives",
  false_negatives = "false_negatives",
  true_negatives = "true_negatives",
  zero_cell_correction = "none",  # the only setting that leaves zeros for Deeks' own +0.5
  publication_bias = TRUE,
  funnel_plot = TRUE
)

# Under "none", past a quarter of studies with a zero cell, that correction alone
# produces "significant" asymmetry far more often than 5% of the time even when
# nothing is missing, so no verdict is reported: the interpretation column reads
# "Not interpretable: too many zero cells" and the statistic is descriptive only.
# (Choose any other correction and the share the test sees is zero, so a verdict
# is given - compare the two runs below.)
deeks_many_zeros <- diagnosticmeta(
  data = head(diagnosticmeta_test_zeros, 10),  # 5 of these 10 have a zero cell
  study = "study",
  true_positives = "true_positives",
  false_positives = "false_positives",
  false_negatives = "false_negatives",
  true_negatives = "true_negatives",
  publication_bias = TRUE
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
#   - Deeks' test p < 0.05: Funnel-plot asymmetry, which can come from publication
#     bias, between-study heterogeneity, or a threshold effect - not bias by itself
#   - p >= 0.05 does not rule bias out; the test is underpowered below 10 studies
#   - Funnel plot asymmetry: Visual check, on the same log DOR vs 1/sqrt(ESS) axes

# ───────────────────────────────────────────────────────────
# Tips for Pathologists
# ───────────────────────────────────────────────────────────

# 1. For AI algorithm validation:
#    - Use bivariate random-effects model (accounts for correlation)
#    - Check heterogeneity (different datasets/conditions)
#    - Explore covariates (imaging type, tissue type, staining)

# 2. For biomarker diagnostic accuracy:
#    - For threshold variation, read the SROC curve (sroc_plot, bivariate model);
#      hsroc_analysis adds the Holling proportional-hazards accuracy summary,
#      which is not a Rutter-Gatsonis HSROC threshold/accuracy table
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
