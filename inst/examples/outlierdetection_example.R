# ═══════════════════════════════════════════════════════════
# OUTLIER DETECTION - COMPREHENSIVE USAGE EXAMPLES
# ═══════════════════════════════════════════════════════════
#
# This file demonstrates all features of the outlierdetection function
# using the outlierdetection_test dataset from the easystats performance package.
#
# Generated: 2026-01-04

library(ClinicoPath)
data(outlierdetection_test)

# ═══════════════════════════════════════════════════════════
# UNIVARIATE METHODS
# ═══════════════════════════════════════════════════════════

# ───────────────────────────────────────────────────────────
# EXAMPLE 1: Robust Z-Score (MAD-based) - Recommended Default
# ───────────────────────────────────────────────────────────
# Best for: Normal and mildly skewed distributions
# Uses median absolute deviation instead of SD for robustness

outlierdetection(
  data = outlierdetection_test,
  vars = c('hemoglobin_mild', 'creatinine_extreme', 'sodium_symmetric'),
  method_category = 'univariate',
  univariate_methods = 'zscore_robust',
  zscore_threshold = 3.29,  # 99.9% confidence
  show_outlier_table = TRUE,
  show_visualization = TRUE
)

# ───────────────────────────────────────────────────────────
# EXAMPLE 2: Standard Z-Score
# ───────────────────────────────────────────────────────────
# Best for: Strictly normal distributions
# More sensitive to extreme values than robust z-score

outlierdetection(
  data = outlierdetection_test,
  vars = c('age_clean', 'glucose_clean'),  # Clean normal distributions
  method_category = 'univariate',
  univariate_methods = 'zscore',
  zscore_threshold = 3.0,  # 99.7% confidence
  show_outlier_table = TRUE
)

# ───────────────────────────────────────────────────────────
# EXAMPLE 3: Interquartile Range (IQR) - Tukey's Method
# ───────────────────────────────────────────────────────────
# Best for: Skewed distributions, heavy-tailed data
# Most robust to distribution assumptions

outlierdetection(
  data = outlierdetection_test,
  vars = c('alt_asymmetric', 'crp_skewed', 'psa_heavy_tail'),
  method_category = 'univariate',
  univariate_methods = 'iqr',
  iqr_multiplier = 1.7,  # Conservative (default 1.5 is more sensitive)
  show_outlier_table = TRUE,
  show_visualization = TRUE
)

# ───────────────────────────────────────────────────────────
# EXAMPLE 4: Equal-Tailed Interval (ETI)
# ───────────────────────────────────────────────────────────
# Best for: Symmetric confidence interval approach
# Identifies values outside specified confidence level

outlierdetection(
  data = outlierdetection_test,
  vars = c('temperature_symmetric', 'potassium_clinical'),
  method_category = 'univariate',
  univariate_methods = 'eti',
  confidence_level = 0.999,  # 99.9%
  show_outlier_table = TRUE
)

# ───────────────────────────────────────────────────────────
# EXAMPLE 5: Highest Density Interval (HDI)
# ───────────────────────────────────────────────────────────
# Best for: Bayesian credible intervals
# Works well with asymmetric distributions

outlierdetection(
  data = outlierdetection_test,
  vars = c('ddimer_asymmetric', 'ca125_biomarker'),
  method_category = 'univariate',
  univariate_methods = 'hdi',
  confidence_level = 0.999,
  show_outlier_table = TRUE,
  show_interpretation = TRUE
)

# ═══════════════════════════════════════════════════════════
# MULTIVARIATE METHODS
# ═══════════════════════════════════════════════════════════

# ───────────────────────────────────────────────────────────
# EXAMPLE 6: Mahalanobis Distance - Classical
# ───────────────────────────────────────────────────────────
# Best for: Correlated variables, multivariate normal data
# Detects observations with unusual patterns across variables

outlierdetection(
  data = outlierdetection_test,
  vars = c('biomarker1_multivar', 'biomarker2_multivar', 'biomarker3_multivar'),
  method_category = 'multivariate',
  multivariate_methods = 'mahalanobis',
  show_outlier_table = TRUE,
  show_visualization = TRUE,
  show_interpretation = TRUE
)

# ───────────────────────────────────────────────────────────
# EXAMPLE 7: Robust Mahalanobis Distance
# ───────────────────────────────────────────────────────────
# Best for: Multivariate data with suspected outlier contamination
# More resistant to outliers than classical Mahalanobis

outlierdetection(
  data = outlierdetection_test,
  vars = c('biomarker1_multivar', 'biomarker2_multivar', 'biomarker3_multivar'),
  method_category = 'multivariate',
  multivariate_methods = 'mahalanobis_robust',
  show_outlier_table = TRUE,
  show_method_comparison = TRUE
)

# ───────────────────────────────────────────────────────────
# EXAMPLE 8: Minimum Covariance Determinant (MCD)
# ───────────────────────────────────────────────────────────
# Best for: Robust covariance estimation
# Identifies outliers using robust covariance matrix

outlierdetection(
  data = outlierdetection_test,
  vars = c('hemoglobin_mild', 'wbc_extreme', 'platelets_clinical'),
  method_category = 'multivariate',
  multivariate_methods = 'mcd',
  show_outlier_table = TRUE
)

# ───────────────────────────────────────────────────────────
# EXAMPLE 9: OPTICS Clustering
# ───────────────────────────────────────────────────────────
# Best for: Density-based outlier detection
# Identifies low-density regions as outliers

outlierdetection(
  data = outlierdetection_test,
  vars = c('cholesterol_bimodal', 'hba1c_biomarker'),
  method_category = 'multivariate',
  multivariate_methods = 'optics',
  show_outlier_table = TRUE,
  show_visualization = TRUE
)

# ───────────────────────────────────────────────────────────
# EXAMPLE 10: Local Outlier Factor (LOF)
# ───────────────────────────────────────────────────────────
# Best for: Local density deviation detection
# Compares local density to neighbors

outlierdetection(
  data = outlierdetection_test,
  vars = c('height_errors', 'weight_errors'),
  method_category = 'multivariate',
  multivariate_methods = 'lof',
  show_outlier_table = TRUE,
  show_exclusion_summary = TRUE
)

# ═══════════════════════════════════════════════════════════
# COMPOSITE METHODS (Multiple Algorithms)
# ═══════════════════════════════════════════════════════════

# ───────────────────────────────────────────────────────────
# EXAMPLE 11: Composite Detection (Default Threshold 0.5)
# ───────────────────────────────────────────────────────────
# Best for: Robust consensus across multiple methods
# Identifies observations flagged by ≥50% of methods

outlierdetection(
  data = outlierdetection_test,
  vars = c('hemoglobin_mild', 'creatinine_extreme', 'wbc_extreme',
           'troponin_extreme'),
  method_category = 'composite',
  composite_threshold = 0.5,  # 50% agreement required
  show_outlier_table = TRUE,
  show_method_comparison = TRUE,
  show_visualization = TRUE
)

# ───────────────────────────────────────────────────────────
# EXAMPLE 12: Conservative Composite (Threshold 0.7)
# ───────────────────────────────────────────────────────────
# Stricter: Requires 70% of methods to agree

outlierdetection(
  data = outlierdetection_test,
  vars = c('alt_asymmetric', 'ddimer_asymmetric', 'ca125_biomarker'),
  method_category = 'composite',
  composite_threshold = 0.7,  # 70% agreement (more conservative)
  show_outlier_table = TRUE,
  show_method_comparison = TRUE,
  show_exclusion_summary = TRUE
)

# ───────────────────────────────────────────────────────────
# EXAMPLE 13: Sensitive Composite (Threshold 0.3)
# ───────────────────────────────────────────────────────────
# More sensitive: Only 30% agreement needed

outlierdetection(
  data = outlierdetection_test,
  vars = c('temperature_symmetric', 'sodium_symmetric', 'potassium_clinical'),
  method_category = 'composite',
  composite_threshold = 0.3,  # 30% agreement (more sensitive)
  show_outlier_table = TRUE,
  show_visualization = TRUE
)

# ═══════════════════════════════════════════════════════════
# ALL METHODS (Comprehensive Analysis)
# ═══════════════════════════════════════════════════════════

# ───────────────────────────────────────────────────────────
# EXAMPLE 14: Complete Analysis with All Methods
# ───────────────────────────────────────────────────────────
# Runs all univariate and multivariate methods
# Best for comprehensive outlier investigation

outlierdetection(
  data = outlierdetection_test,
  vars = c('hemoglobin_mild', 'creatinine_extreme', 'sodium_symmetric'),
  method_category = 'all',
  show_outlier_table = TRUE,
  show_method_comparison = TRUE,
  show_exclusion_summary = TRUE,
  show_visualization = TRUE,
  show_interpretation = TRUE
)

# ═══════════════════════════════════════════════════════════
# CLINICAL RESEARCH SCENARIOS
# ═══════════════════════════════════════════════════════════

# ───────────────────────────────────────────────────────────
# SCENARIO 1: Hematology Lab Quality Control
# ───────────────────────────────────────────────────────────
# Screen CBC panel for outliers before analysis

outlierdetection(
  data = outlierdetection_test,
  vars = c('hemoglobin_mild', 'wbc_extreme', 'platelets_clinical'),
  method_category = 'univariate',
  univariate_methods = 'zscore_robust',
  zscore_threshold = 3.29,  # Clinical QC standard
  show_outlier_table = TRUE,
  show_exclusion_summary = TRUE,
  show_interpretation = TRUE
)

# ───────────────────────────────────────────────────────────
# SCENARIO 2: Chemistry Panel Critical Values
# ───────────────────────────────────────────────────────────
# Identify critical electrolyte abnormalities

outlierdetection(
  data = outlierdetection_test,
  vars = c('sodium_symmetric', 'potassium_clinical', 'creatinine_extreme'),
  method_category = 'univariate',
  univariate_methods = 'iqr',
  iqr_multiplier = 1.5,  # Sensitive detection for critical values
  show_outlier_table = TRUE,
  show_exclusion_summary = TRUE
)

# ───────────────────────────────────────────────────────────
# SCENARIO 3: Cardiac Biomarker Panel
# ───────────────────────────────────────────────────────────
# Detect acute MI outliers in troponin

outlierdetection(
  data = outlierdetection_test,
  vars = c('troponin_extreme'),
  method_category = 'univariate',
  univariate_methods = 'hdi',  # Asymmetric distribution
  confidence_level = 0.999,
  show_outlier_table = TRUE,
  show_visualization = TRUE
)

# ───────────────────────────────────────────────────────────
# SCENARIO 4: Liver Function Test Screening
# ───────────────────────────────────────────────────────────
# Detect hepatitis/liver damage (asymmetric outliers)

outlierdetection(
  data = outlierdetection_test,
  vars = c('alt_asymmetric'),
  method_category = 'univariate',
  univariate_methods = 'zscore_robust',  # Robust to skewness
  zscore_threshold = 3.0,
  show_outlier_table = TRUE,
  show_interpretation = TRUE
)

# ───────────────────────────────────────────────────────────
# SCENARIO 5: Tumor Marker Surveillance
# ───────────────────────────────────────────────────────────
# Monitor cancer progression with highly variable markers

outlierdetection(
  data = outlierdetection_test,
  vars = c('ca125_biomarker', 'psa_heavy_tail'),
  method_category = 'composite',
  composite_threshold = 0.6,
  show_outlier_table = TRUE,
  show_method_comparison = TRUE,
  show_interpretation = TRUE
)

# ───────────────────────────────────────────────────────────
# SCENARIO 6: Anthropometric Data Entry Validation
# ───────────────────────────────────────────────────────────
# Detect measurement errors (unit conversions, decimal points)

outlierdetection(
  data = outlierdetection_test,
  vars = c('height_errors', 'weight_errors'),
  method_category = 'multivariate',
  multivariate_methods = 'lof',  # Detects unusual combinations
  show_outlier_table = TRUE,
  show_exclusion_summary = TRUE
)

# ───────────────────────────────────────────────────────────
# SCENARIO 7: Metabolic Panel Analysis
# ───────────────────────────────────────────────────────────
# Screen glucose and HbA1c for diabetes outliers

outlierdetection(
  data = outlierdetection_test,
  vars = c('glucose_clean', 'hba1c_biomarker'),
  method_category = 'multivariate',
  multivariate_methods = 'mahalanobis',
  show_outlier_table = TRUE,
  show_visualization = TRUE
)

# ───────────────────────────────────────────────────────────
# SCENARIO 8: Vital Signs Monitoring
# ───────────────────────────────────────────────────────────
# Detect abnormal temperature and blood pressure

outlierdetection(
  data = outlierdetection_test,
  vars = c('temperature_symmetric', 'systolic_bp_mild'),
  method_category = 'univariate',
  univariate_methods = 'zscore',
  zscore_threshold = 2.5,  # More sensitive for vital signs
  show_outlier_table = TRUE,
  show_exclusion_summary = TRUE
)

# ───────────────────────────────────────────────────────────
# SCENARIO 9: Biomarker Panel Correlation Analysis
# ───────────────────────────────────────────────────────────
# Detect multivariate outliers breaking correlation patterns

outlierdetection(
  data = outlierdetection_test,
  vars = c('biomarker1_multivar', 'biomarker2_multivar', 'biomarker3_multivar'),
  method_category = 'all',  # Compare all methods
  show_outlier_table = TRUE,
  show_method_comparison = TRUE,
  show_visualization = TRUE
)

# ───────────────────────────────────────────────────────────
# SCENARIO 10: Coagulation Panel
# ───────────────────────────────────────────────────────────
# Screen for thrombosis risk (D-dimer, platelets)

outlierdetection(
  data = outlierdetection_test,
  vars = c('ddimer_asymmetric', 'platelets_clinical'),
  method_category = 'composite',
  composite_threshold = 0.5,
  show_outlier_table = TRUE,
  show_exclusion_summary = TRUE
)

# ═══════════════════════════════════════════════════════════
# THRESHOLD COMPARISON EXAMPLES
# ═══════════════════════════════════════════════════════════

# ───────────────────────────────────────────────────────────
# EXAMPLE 15: Z-Score Threshold Comparison
# ───────────────────────────────────────────────────────────

# Standard screening (3.0 SD)
outlierdetection(
  data = outlierdetection_test,
  vars = c('hemoglobin_mild'),
  method_category = 'univariate',
  univariate_methods = 'zscore_robust',
  zscore_threshold = 3.0,  # 99.7% confidence (~0.3% outliers)
  show_outlier_table = TRUE
)

# Stringent clinical QC (3.29 SD)
outlierdetection(
  data = outlierdetection_test,
  vars = c('hemoglobin_mild'),
  method_category = 'univariate',
  univariate_methods = 'zscore_robust',
  zscore_threshold = 3.29,  # 99.9% confidence (~0.1% outliers)
  show_outlier_table = TRUE
)

# Sensitive research screening (2.5 SD)
outlierdetection(
  data = outlierdetection_test,
  vars = c('hemoglobin_mild'),
  method_category = 'univariate',
  univariate_methods = 'zscore_robust',
  zscore_threshold = 2.5,  # 98.8% confidence (~1.2% outliers)
  show_outlier_table = TRUE
)

# ───────────────────────────────────────────────────────────
# EXAMPLE 16: IQR Multiplier Comparison
# ───────────────────────────────────────────────────────────

# Tukey's standard (1.5)
outlierdetection(
  data = outlierdetection_test,
  vars = c('crp_skewed'),
  method_category = 'univariate',
  univariate_methods = 'iqr',
  iqr_multiplier = 1.5,  # Standard, more sensitive (~0.7% if normal)
  show_outlier_table = TRUE
)

# Conservative clinical (1.7)
outlierdetection(
  data = outlierdetection_test,
  vars = c('crp_skewed'),
  method_category = 'univariate',
  univariate_methods = 'iqr',
  iqr_multiplier = 1.7,  # Recommended default
  show_outlier_table = TRUE
)

# Very conservative (2.0)
outlierdetection(
  data = outlierdetection_test,
  vars = c('crp_skewed'),
  method_category = 'univariate',
  univariate_methods = 'iqr',
  iqr_multiplier = 2.0,  # For critical biomarkers
  show_outlier_table = TRUE
)

# ═══════════════════════════════════════════════════════════
# INTERPRETATION GUIDE
# ═══════════════════════════════════════════════════════════

# When to use each method:
#
# UNIVARIATE METHODS:
# - Robust Z-Score: Default choice, works well for most clinical data
# - Standard Z-Score: Only for strictly normal distributions
# - IQR: Best for skewed data (liver enzymes, inflammatory markers)
# - ETI/HDI: Interval-based approaches, good for symmetric/asymmetric data
#
# MULTIVARIATE METHODS:
# - Mahalanobis: When variables are correlated (biomarker panels, CBC)
# - Robust Mahalanobis/MCD: When outlier contamination suspected
# - OPTICS: For clustering-based detection, bimodal distributions
# - LOF: For local density deviations, data entry errors
#
# COMPOSITE:
# - Recommended for robust detection across different data patterns
# - Adjust threshold based on desired sensitivity:
#   - 0.3-0.4: Sensitive (more outliers detected)
#   - 0.5: Balanced (default)
#   - 0.6-0.7: Conservative (fewer outliers)
#
# ALL METHODS:
# - Use for comprehensive investigation
# - Compare method agreement to assess outlier robustness

# ═══════════════════════════════════════════════════════════
# RECOMMENDED WORKFLOWS
# ═══════════════════════════════════════════════════════════

# Workflow 1: Initial Screening (Quick)
# 1. Run composite method with default threshold
# 2. Review outlier table
# 3. Investigate flagged observations

# Workflow 2: Comprehensive Analysis
# 1. Run all methods
# 2. Compare method agreement
# 3. Examine outliers flagged by multiple methods
# 4. Clinical review of suspicious values
# 5. Document exclusion decisions

# Workflow 3: Clinical QC Protocol
# 1. Define acceptable thresholds (e.g., 3.29 SD for z-score)
# 2. Run univariate robust z-score on all lab values
# 3. Generate exclusion summary
# 4. Flag for clinical review
# 5. Apply corrections or exclusions

# ═══════════════════════════════════════════════════════════
