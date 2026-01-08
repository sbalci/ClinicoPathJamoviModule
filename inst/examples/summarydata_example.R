# ═══════════════════════════════════════════════════════════
# SUMMARYDATA - COMPREHENSIVE USAGE EXAMPLES
# ═══════════════════════════════════════════════════════════
#
# This file demonstrates all features of the summarydata function
# (Summary of Continuous Variables with Distribution Diagnostics)
# using the summarydata_test dataset.
#
# Generated: 2026-01-04

library(ClinicoPath)
data(summarydata_test)

# ═══════════════════════════════════════════════════════════
# BASIC USAGE
# ═══════════════════════════════════════════════════════════

# Example 1: Single Variable Summary
summarydata(
  data = summarydata_test,
  vars = 'age_normal'
)

# Example 2: Multiple Variables
summarydata(
  data = summarydata_test,
  vars = c('age_normal', 'weight_normal', 'cholesterol_normal')
)

# Example 3: With Custom Decimal Places (Default: 2)
summarydata(
  data = summarydata_test,
  vars = c('troponin', 'creatinine_lab'),
  decimal_places = 3  # High precision for sensitive assays
)

# ═══════════════════════════════════════════════════════════
# DISTRIBUTION DIAGNOSTICS
# ═══════════════════════════════════════════════════════════

# Example 4: Normal Distribution (Pass Shapiro-Wilk)
summarydata(
  data = summarydata_test,
  vars = 'age_normal',
  distr = TRUE  # Enable distribution diagnostics
)

# Example 5: Right-Skewed Distribution
summarydata(
  data = summarydata_test,
  vars = c('psa_mild_skew', 'crp_moderate_skew', 'income_strong_skew'),
  distr = TRUE
)

# Example 6: Left-Skewed Distribution
summarydata(
  data = summarydata_test,
  vars = c('test_score_mild_left', 'age_at_diagnosis_left'),
  distr = TRUE
)

# Example 7: Bimodal Distribution
summarydata(
  data = summarydata_test,
  vars = c('bimodal_biomarker', 'trimodal_response'),
  distr = TRUE
)

# Example 8: Kurtosis Variations
summarydata(
  data = summarydata_test,
  vars = c('uniform_var',      # Platykurtic (light tails)
           'mesokurtic_var',   # Normal tails
           'leptokurtic_var'), # Heavy tails
  distr = TRUE
)

# ═══════════════════════════════════════════════════════════
# OUTLIER DETECTION
# ═══════════════════════════════════════════════════════════

# Example 9: Variable with Few Outliers (5%)
summarydata(
  data = summarydata_test,
  vars = 'hemoglobin_few_outliers',
  outliers = TRUE
)

# Example 10: Variable with Many Outliers (15%)
summarydata(
  data = summarydata_test,
  vars = 'glucose_many_outliers',
  outliers = TRUE
)

# Example 11: Variable with Extreme Outliers
summarydata(
  data = summarydata_test,
  vars = 'creatinine_extreme',
  outliers = TRUE,
  distr = TRUE
)

# Example 12: Clean Data (No Outliers)
summarydata(
  data = summarydata_test,
  vars = 'albumin_no_outliers',
  outliers = TRUE
)

# ═══════════════════════════════════════════════════════════
# REPORT SENTENCES (Clinical Documentation)
# ═══════════════════════════════════════════════════════════

# Example 13: Generate Clinical Report Sentences
summarydata(
  data = summarydata_test,
  vars = c('age_normal', 'weight_normal', 'cholesterol_normal'),
  report_sentences = TRUE
)

# Example 14: Complete Clinical Report
summarydata(
  data = summarydata_test,
  vars = c('hemoglobin_lab', 'wbc_count', 'platelet_count'),
  report_sentences = TRUE,
  decimal_places = 1
)

# ═══════════════════════════════════════════════════════════
# ALL FEATURES COMBINED
# ═══════════════════════════════════════════════════════════

# Example 15: Comprehensive Analysis
summarydata(
  data = summarydata_test,
  vars = c('age_normal', 'psa_mild_skew'),
  distr = TRUE,
  outliers = TRUE,
  report_sentences = TRUE,
  decimal_places = 2
)

# ═══════════════════════════════════════════════════════════
# MISSING DATA HANDLING
# ═══════════════════════════════════════════════════════════

# Example 16: Complete Data (0% Missing)
summarydata(
  data = summarydata_test,
  vars = 'bmi_complete',
  distr = TRUE
)

# Example 17: Low Missing (3%)
summarydata(
  data = summarydata_test,
  vars = 'systolic_bp_low_missing',
  distr = TRUE
)

# Example 18: Moderate Missing (15%)
summarydata(
  data = summarydata_test,
  vars = 'ldl_moderate_missing',
  distr = TRUE
)

# Example 19: High Missing (35%)
summarydata(
  data = summarydata_test,
  vars = 'vitamin_d_high_missing',
  distr = TRUE
)

# Example 20: Very High Missing (60%)
summarydata(
  data = summarydata_test,
  vars = 'genetic_score_very_high_missing',
  distr = TRUE
)

# Example 21: Compare Variables with Different Missing %
summarydata(
  data = summarydata_test,
  vars = c('bmi_complete',                      # 0%
           'systolic_bp_low_missing',           # 3%
           'ldl_moderate_missing',              # 15%
           'vitamin_d_high_missing'),           # 35%
  distr = TRUE
)

# ═══════════════════════════════════════════════════════════
# CLINICAL LAB VALUES (Different Scales)
# ═══════════════════════════════════════════════════════════

# Example 22: High Precision Values (3 Decimals)
summarydata(
  data = summarydata_test,
  vars = 'troponin',
  decimal_places = 3,
  distr = TRUE
)

# Example 23: Medium Precision (1 Decimal)
summarydata(
  data = summarydata_test,
  vars = 'hemoglobin_lab',
  decimal_places = 1,
  outliers = TRUE
)

# Example 24: Integer Values (0 Decimals)
summarydata(
  data = summarydata_test,
  vars = c('wbc_count', 'platelet_count'),
  decimal_places = 0,
  outliers = TRUE
)

# Example 25: Complete Blood Count Panel
summarydata(
  data = summarydata_test,
  vars = c('hemoglobin_lab', 'wbc_count', 'platelet_count'),
  decimal_places = 1,
  distr = TRUE,
  outliers = TRUE
)

# ═══════════════════════════════════════════════════════════
# EDGE CASES
# ═══════════════════════════════════════════════════════════

# Example 26: Constant Variable (Zero Variance)
# Expected: Warning about zero variance
summarydata(
  data = summarydata_test,
  vars = 'constant_var',
  distr = TRUE
)

# Example 27: Nearly Constant (Minimal Variance)
summarydata(
  data = summarydata_test,
  vars = 'nearly_constant',
  distr = TRUE,
  decimal_places = 3
)

# Example 28: Extreme Range (Millions)
summarydata(
  data = summarydata_test,
  vars = 'extreme_range',
  decimal_places = 0
)

# Example 29: Negative Values Allowed
summarydata(
  data = summarydata_test,
  vars = 'profit_loss',
  decimal_places = 0,
  distr = TRUE
)

# Example 30: All Missing Data
# Expected: Error or warning
summarydata(
  data = summarydata_test,
  vars = 'all_missing'
)

# ═══════════════════════════════════════════════════════════
# SHAPIRO-WILK SAMPLE SIZE EDGE CASES
# ═══════════════════════════════════════════════════════════

# Example 31: Small Sample (n=10)
summarydata(
  data = summarydata_test,
  vars = 'small_sample_var',
  distr = TRUE
)

# Example 32: Minimum Sample for Shapiro-Wilk (n=3)
summarydata(
  data = summarydata_test,
  vars = 'tiny_sample_var',
  distr = TRUE
)

# Example 33: Insufficient Sample for Shapiro-Wilk (n=2)
# Expected: "Normality test not applicable"
summarydata(
  data = summarydata_test,
  vars = 'insufficient_sample_var',
  distr = TRUE
)

# ═══════════════════════════════════════════════════════════
# ANTHROPOMETRIC AND VITAL SIGNS
# ═══════════════════════════════════════════════════════════

# Example 34: Anthropometric Measurements
summarydata(
  data = summarydata_test,
  vars = c('height_cm', 'weight_kg', 'bmi_calculated'),
  decimal_places = 1,
  distr = TRUE
)

# Example 35: Vital Signs
summarydata(
  data = summarydata_test,
  vars = c('diastolic_bp', 'heart_rate'),
  decimal_places = 0,
  outliers = TRUE
)

# ═══════════════════════════════════════════════════════════
# BIOMARKERS
# ═══════════════════════════════════════════════════════════

# Example 36: Tumor Markers
summarydata(
  data = summarydata_test,
  vars = 'ca19_9',
  decimal_places = 1,
  distr = TRUE,
  outliers = TRUE
)

# Example 37: Metabolic Markers
summarydata(
  data = summarydata_test,
  vars = c('hba1c', 'ferritin', 'tsh'),
  decimal_places = 1,
  distr = TRUE
)

# ═══════════════════════════════════════════════════════════
# CLINICAL RESEARCH SCENARIOS
# ═══════════════════════════════════════════════════════════

# Scenario 1: Baseline Patient Characteristics
summarydata(
  data = summarydata_test,
  vars = c('age_normal', 'weight_normal', 'height_cm', 'bmi_calculated'),
  decimal_places = 1,
  distr = TRUE,
  report_sentences = TRUE
)

# Scenario 2: Laboratory Values Quality Control
summarydata(
  data = summarydata_test,
  vars = c('hemoglobin_lab', 'wbc_count', 'platelet_count', 'creatinine_lab'),
  decimal_places = 1,
  outliers = TRUE,
  distr = TRUE
)

# Scenario 3: Biomarker Distribution Assessment
summarydata(
  data = summarydata_test,
  vars = c('psa_mild_skew', 'crp_moderate_skew', 'ca19_9'),
  decimal_places = 2,
  distr = TRUE,
  outliers = TRUE
)

# Scenario 4: Data Quality Assessment (Missing Data)
summarydata(
  data = summarydata_test,
  vars = c('bmi_complete',
           'systolic_bp_low_missing',
           'ldl_moderate_missing',
           'vitamin_d_high_missing',
           'genetic_score_very_high_missing'),
  decimal_places = 1
)

# Scenario 5: Vital Signs Monitoring
summarydata(
  data = summarydata_test,
  vars = c('systolic_bp_low_missing', 'diastolic_bp',
           'heart_rate', 'temperature_normal'),
  decimal_places = 1,
  outliers = TRUE
)

# Scenario 6: Metabolic Panel
summarydata(
  data = summarydata_test,
  vars = c('glucose_many_outliers', 'hba1c', 'cholesterol_normal', 'ldl_moderate_missing'),
  decimal_places = 1,
  distr = TRUE,
  outliers = TRUE,
  report_sentences = TRUE
)

# Scenario 7: Renal Function Assessment
summarydata(
  data = summarydata_test,
  vars = c('creatinine_lab', 'creatinine_extreme'),
  decimal_places = 2,
  outliers = TRUE,
  distr = TRUE
)

# Scenario 8: Hematology Panel
summarydata(
  data = summarydata_test,
  vars = c('hemoglobin_lab', 'hemoglobin_few_outliers',
           'wbc_count', 'platelet_count'),
  decimal_places = 1,
  outliers = TRUE
)

# ═══════════════════════════════════════════════════════════
# INTERPRETATION GUIDE
# ═══════════════════════════════════════════════════════════

# OUTPUT INTERPRETATION:
#
# 1. **Basic Statistics**:
#    - Mean ± SD: Central tendency and spread
#    - Median: Robust center (not affected by outliers)
#    - Min-Max: Range of values
#
# 2. **Distribution Diagnostics** (when distr = TRUE):
#    - Shapiro-Wilk p-value:
#      - p > 0.05: Data appears normally distributed
#      - p ≤ 0.05: Data does not appear normal
#    - Skewness:
#      - ~0: Symmetric
#      - >0: Right-skewed (long right tail)
#      - <0: Left-skewed (long left tail)
#    - Kurtosis:
#      - ~3: Normal distribution
#      - >3: Heavy tails (leptokurtic)
#      - <3: Light tails (platykurtic)
#
# 3. **Outlier Detection** (when outliers = TRUE):
#    - Uses IQR method: values beyond Q1 - 1.5×IQR or Q3 + 1.5×IQR
#    - Lists identified outlier values
#    - Consider data entry errors vs true extreme values
#
# 4. **Clinical Report Sentences** (when report_sentences = TRUE):
#    - Copy-ready text for medical documentation
#    - Includes context and interpretation
#    - Follows medical reporting standards
#
# ═══════════════════════════════════════════════════════════
# BEST PRACTICES
# ═══════════════════════════════════════════════════════════

# 1. **Decimal Places**:
#    - Match precision to measurement instrument
#    - Lab values: 1-3 decimals based on assay sensitivity
#    - Vital signs: 0-1 decimals
#    - Anthropometric: 1 decimal
#
# 2. **Distribution Diagnostics**:
#    - Always check for non-normal distributions
#    - Helps choose appropriate statistical tests
#    - Identifies need for transformations
#
# 3. **Outlier Detection**:
#    - Verify outliers are not data entry errors
#    - Consider clinical plausibility
#    - Document outlier handling decisions
#
# 4. **Missing Data**:
#    - <5%: Generally acceptable
#    - 5-20%: May need imputation
#    - >20%: Investigate patterns, consider excluding variable
#
# 5. **Clinical Reporting**:
#    - Use report_sentences for documentation
#    - Include units in variable names
#    - Provide clinical context
#
# ═══════════════════════════════════════════════════════════
# TROUBLESHOOTING
# ═══════════════════════════════════════════════════════════

# Error: "Variable is not numeric"
# Solution: Ensure variable is continuous/numeric, not factor/character
# Example:
# summarydata(data = summarydata_test, vars = 'patient_id')  # Error!
# summarydata(data = summarydata_test, vars = 'age_normal')  # OK!

# Warning: "Variable contains only missing values"
# Solution: Variable has all NA, excluded from analysis
# Example:
# summarydata(data = summarydata_test, vars = 'all_missing')  # Warning

# Note: "Normality test not applicable due to sample size"
# Cause: Sample size < 3 or > 5000 for Shapiro-Wilk test
# Solution: Use other normality tests (K-S test, Q-Q plots)

# Issue: Extreme values in output
# Cause: Outliers or wide range
# Solution: Check outliers = TRUE, investigate extreme values

# ═══════════════════════════════════════════════════════════
