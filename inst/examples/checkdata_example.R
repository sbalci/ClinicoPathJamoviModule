# ═══════════════════════════════════════════════════════════
# SINGLE VARIABLE QUALITY CHECK - COMPREHENSIVE EXAMPLES
# ═══════════════════════════════════════════════════════════
#
# This file demonstrates all features of the checkdata function
# using the checkdata_test dataset.
#
# Generated: 2026-01-04

library(ClinicoPath)
data(checkdata_test)

# ═══════════════════════════════════════════════════════════
# BASIC QUALITY CHECKS
# ═══════════════════════════════════════════════════════════

# Example 1: Perfect Quality Variable (No Issues)
checkdata(
  data = checkdata_test,
  var = 'age_perfect',
  showOutliers = TRUE,
  showDistribution = TRUE,
  showDuplicates = TRUE
)

# Example 2: Quick Summary Check
checkdata(
  data = checkdata_test,
  var = 'bmi_low_missing',
  showSummary = TRUE  # Plain-language summary
)

# ═══════════════════════════════════════════════════════════
# OUTLIER DETECTION
# ═══════════════════════════════════════════════════════════

# Example 3: Mild Outliers (Both Tails)
checkdata(
  data = checkdata_test,
  var = 'glucose_mild_outliers',
  showOutliers = TRUE,
  showDistribution = TRUE,
  showSummary = TRUE
)

# Example 4: Extreme Outliers
checkdata(
  data = checkdata_test,
  var = 'creatinine_extreme_outliers',
  showOutliers = TRUE,
  showDistribution = TRUE
)

# Example 5: Asymmetric Outliers (High Tail Only)
checkdata(
  data = checkdata_test,
  var = 'alt_high_outliers',
  showOutliers = TRUE,
  outlierTransform = 'log',  # Log transform for skewed data
  showDistribution = TRUE
)

# Example 6: Asymmetric Outliers (Low Tail Only)
checkdata(
  data = checkdata_test,
  var = 'hemoglobin_low_outliers',
  showOutliers = TRUE,
  showDistribution = TRUE
)

# ═══════════════════════════════════════════════════════════
# MISSING DATA ANALYSIS
# ═══════════════════════════════════════════════════════════

# Example 7: Low Missing Data (3%)
checkdata(
  data = checkdata_test,
  var = 'bmi_low_missing',
  showPatterns = TRUE,
  showSummary = TRUE
)

# Example 8: Moderate Missing Data (15%)
checkdata(
  data = checkdata_test,
  var = 'cholesterol_moderate_missing',
  showOutliers = TRUE,
  showPatterns = TRUE,
  mcarTest = TRUE  # Test if missing completely at random
)

# Example 9: High Missing Data (40%)
checkdata(
  data = checkdata_test,
  var = 'genetic_test_high_missing',
  showDuplicates = TRUE,
  showPatterns = TRUE,
  showSummary = TRUE
)

# Example 10: All Missing Data (100%)
checkdata(
  data = checkdata_test,
  var = 'future_biomarker_all_missing',
  showPatterns = TRUE
)

# ═══════════════════════════════════════════════════════════
# DUPLICATE ANALYSIS
# ═══════════════════════════════════════════════════════════

# Example 11: High Duplicates (Few Unique Values)
checkdata(
  data = checkdata_test,
  var = 'tumor_grade_duplicates',
  showDuplicates = TRUE,
  showDistribution = TRUE
)

# Example 12: Many Duplicates (Repeated Measurements)
checkdata(
  data = checkdata_test,
  var = 'visit_number_duplicates',
  showDuplicates = TRUE
)

# Example 13: Few Duplicates (Mostly Unique)
checkdata(
  data = checkdata_test,
  var = 'patient_code_few_duplicates',
  showDuplicates = TRUE
)

# ═══════════════════════════════════════════════════════════
# RARE CATEGORY DETECTION
# ═══════════════════════════════════════════════════════════

# Example 14: One Rare Category (<5%)
checkdata(
  data = checkdata_test,
  var = 'histology_rare_category',
  showDuplicates = TRUE,
  rareCategoryThreshold = 5  # Default: flag categories <5%
)

# Example 15: Multiple Rare Categories
checkdata(
  data = checkdata_test,
  var = 'diagnosis_multiple_rare',
  showDuplicates = TRUE,
  rareCategoryThreshold = 3  # More stringent: flag <3%
)

# Example 16: Adjust Rare Category Threshold
checkdata(
  data = checkdata_test,
  var = 'diagnosis_multiple_rare',
  showDuplicates = TRUE,
  rareCategoryThreshold = 10  # More lenient: flag <10%
)

# ═══════════════════════════════════════════════════════════
# CLINICAL VALIDATION
# ═══════════════════════════════════════════════════════════

# Example 17: Age Validation
checkdata(
  data = checkdata_test,
  var = 'age_clinical',
  showOutliers = TRUE,
  clinicalValidation = TRUE,
  showSummary = TRUE
)

# Example 18: Height Validation (Metric)
checkdata(
  data = checkdata_test,
  var = 'height_metric',
  showOutliers = TRUE,
  clinicalValidation = TRUE,
  unitSystem = 'metric'
)

# Example 19: Height with Unit Errors (Mixed Metric/Imperial)
checkdata(
  data = checkdata_test,
  var = 'height_mixed_units',
  showOutliers = TRUE,
  clinicalValidation = TRUE,
  unitSystem = 'auto',  # Auto-detect unit issues
  showSummary = TRUE
)

# Example 20: Weight Validation
checkdata(
  data = checkdata_test,
  var = 'weight_metric',
  showOutliers = TRUE,
  clinicalValidation = TRUE,
  unitSystem = 'metric'
)

# Example 21: Weight with Mixed Units (kg and lb)
checkdata(
  data = checkdata_test,
  var = 'weight_mixed_units',
  showOutliers = TRUE,
  clinicalValidation = TRUE,
  unitSystem = 'auto'
)

# Example 22: Blood Pressure Validation
checkdata(
  data = checkdata_test,
  var = 'systolic_bp_clinical',
  showOutliers = TRUE,
  clinicalValidation = TRUE
)

# Example 23: Temperature Validation
checkdata(
  data = checkdata_test,
  var = 'temperature_clinical',
  showOutliers = TRUE,
  clinicalValidation = TRUE,
  unitSystem = 'metric'
)

# Example 24: Temperature with Mixed Units (°C and °F)
checkdata(
  data = checkdata_test,
  var = 'temperature_mixed_units',
  showOutliers = TRUE,
  clinicalValidation = TRUE,
  unitSystem = 'auto'
)

# Example 25: Lab Value Validation (Hemoglobin)
checkdata(
  data = checkdata_test,
  var = 'hemoglobin_clinical',
  showOutliers = TRUE,
  clinicalValidation = TRUE,
  showDistribution = TRUE
)

# Example 26: Lab Value Validation (WBC Count)
checkdata(
  data = checkdata_test,
  var = 'wbc_clinical',
  showOutliers = TRUE,
  clinicalValidation = TRUE
)

# ═══════════════════════════════════════════════════════════
# DISTRIBUTION ANALYSIS
# ═══════════════════════════════════════════════════════════

# Example 27: Right-Skewed Distribution
checkdata(
  data = checkdata_test,
  var = 'psa_right_skewed',
  showOutliers = TRUE,
  showDistribution = TRUE,
  outlierTransform = 'none'  # Show raw distribution
)

# Example 28: Right-Skewed with Log Transform
checkdata(
  data = checkdata_test,
  var = 'psa_right_skewed',
  showOutliers = TRUE,
  showDistribution = TRUE,
  outlierTransform = 'log',  # Apply log transform
  showSummary = TRUE
)

# Example 29: Left-Skewed Distribution
checkdata(
  data = checkdata_test,
  var = 'time_to_event_left_skewed',
  showOutliers = TRUE,
  showDistribution = TRUE
)

# Example 30: Bimodal Distribution
checkdata(
  data = checkdata_test,
  var = 'biomarker_bimodal',
  showOutliers = TRUE,
  showDistribution = TRUE
)

# Example 31: Uniform Distribution
checkdata(
  data = checkdata_test,
  var = 'random_uniform',
  showOutliers = TRUE,
  showDistribution = TRUE
)

# Example 32: Heavy-Tailed Distribution
checkdata(
  data = checkdata_test,
  var = 'heavy_tailed_var',
  showOutliers = TRUE,
  showDistribution = TRUE
)

# ═══════════════════════════════════════════════════════════
# TRANSFORMATION ANALYSIS
# ═══════════════════════════════════════════════════════════

# Example 33: Log Transformation Needed
checkdata(
  data = checkdata_test,
  var = 'crp_log_transform',
  showOutliers = TRUE,
  showDistribution = TRUE,
  outlierTransform = 'log'
)

# Example 34: Square Root Transformation Needed
checkdata(
  data = checkdata_test,
  var = 'platelet_count_sqrt',
  showOutliers = TRUE,
  showDistribution = TRUE,
  outlierTransform = 'sqrt'
)

# Example 35: Compare Transformations
# Raw data
checkdata(
  data = checkdata_test,
  var = 'crp_log_transform',
  outlierTransform = 'none'
)

# Log transform
checkdata(
  data = checkdata_test,
  var = 'crp_log_transform',
  outlierTransform = 'log'
)

# Sqrt transform
checkdata(
  data = checkdata_test,
  var = 'crp_log_transform',
  outlierTransform = 'sqrt'
)

# ═══════════════════════════════════════════════════════════
# EDGE CASES
# ═══════════════════════════════════════════════════════════

# Example 36: Constant Variable
checkdata(
  data = checkdata_test,
  var = 'constant_var',
  showDuplicates = TRUE,
  showDistribution = TRUE
)

# Example 37: Nearly Constant Variable
checkdata(
  data = checkdata_test,
  var = 'nearly_constant',
  showOutliers = TRUE,
  showDuplicates = TRUE
)

# Example 38: Single Unique Value (with Missing)
checkdata(
  data = checkdata_test,
  var = 'single_value_var',
  showDuplicates = TRUE,
  showPatterns = TRUE
)

# Example 39: Tiny Variance
checkdata(
  data = checkdata_test,
  var = 'tiny_variance',
  showOutliers = TRUE,
  showDistribution = TRUE
)

# Example 40: Infinite Values
checkdata(
  data = checkdata_test,
  var = 'with_infinite',
  showOutliers = TRUE,
  showDistribution = TRUE
)

# Example 41: Extreme Range
checkdata(
  data = checkdata_test,
  var = 'extreme_range',
  showOutliers = TRUE,
  showDistribution = TRUE,
  showSummary = TRUE
)

# ═══════════════════════════════════════════════════════════
# COMPREHENSIVE ANALYSIS
# ═══════════════════════════════════════════════════════════

# Example 42: Full Analysis (All Options)
checkdata(
  data = checkdata_test,
  var = 'cholesterol_moderate_missing',
  showOutliers = TRUE,
  showDistribution = TRUE,
  showDuplicates = TRUE,
  showPatterns = TRUE,
  rareCategoryThreshold = 5,
  clinicalValidation = TRUE,
  unitSystem = 'auto',
  outlierTransform = 'none',
  mcarTest = FALSE,
  showSummary = TRUE,
  showAbout = TRUE,
  showCaveats = TRUE
)

# ═══════════════════════════════════════════════════════════
# CLINICAL RESEARCH SCENARIOS
# ═══════════════════════════════════════════════════════════

# Scenario 1: Pre-Analysis Data Screening
# Check key clinical variables before statistical analysis
checkdata(
  data = checkdata_test,
  var = 'age_clinical',
  showOutliers = TRUE,
  showDistribution = TRUE,
  clinicalValidation = TRUE,
  showSummary = TRUE
)

# Scenario 2: Lab Value Quality Control
# Verify lab measurements are within plausible ranges
checkdata(
  data = checkdata_test,
  var = 'creatinine_extreme_outliers',
  showOutliers = TRUE,
  clinicalValidation = TRUE,
  outlierTransform = 'log',
  showSummary = TRUE
)

# Scenario 3: Data Entry Validation
# Detect unit conversion errors and implausible values
checkdata(
  data = checkdata_test,
  var = 'height_mixed_units',
  showOutliers = TRUE,
  clinicalValidation = TRUE,
  unitSystem = 'auto',
  showSummary = TRUE
)

# Scenario 4: Missing Data Assessment
# Evaluate impact of missing data on analysis
checkdata(
  data = checkdata_test,
  var = 'genetic_test_high_missing',
  showPatterns = TRUE,
  showDuplicates = TRUE,
  mcarTest = TRUE,
  showSummary = TRUE
)

# Scenario 5: Categorical Variable Check
# Verify adequate sample sizes in all categories
checkdata(
  data = checkdata_test,
  var = 'histology_rare_category',
  showDuplicates = TRUE,
  rareCategoryThreshold = 5,
  showSummary = TRUE
)

# Scenario 6: Biomarker Distribution Assessment
# Check if transformation needed for statistical modeling
checkdata(
  data = checkdata_test,
  var = 'psa_right_skewed',
  showOutliers = TRUE,
  showDistribution = TRUE,
  outlierTransform = 'log',
  showSummary = TRUE
)

# ═══════════════════════════════════════════════════════════
# INTERPRETATION GUIDE
# ═══════════════════════════════════════════════════════════

# OUTLIER INTERPRETATION:
# - Z-score |z| > 3: Outlier (flagged)
# - Z-score |z| > 4: Extreme outlier
# - For skewed data: Use log or sqrt transform before detection

# MISSING DATA INTERPRETATION:
# - 0%: Excellent (complete data)
# - <5%: Good (minimal missing)
# - 5-15%: Acceptable
# - 15-30%: Concerning (may need imputation)
# - >30%: Poor (extensive missing, consider excluding variable)

# RARE CATEGORY THRESHOLD:
# - Default 5%: Standard for chi-squared test assumptions
# - Adjust based on sample size and analysis needs
# - Categories <5% may need grouping or exclusion

# CLINICAL VALIDATION:
# - Detects physiologically implausible values
# - Auto-detects unit conversion errors (inches vs cm, lb vs kg, °F vs °C)
# - Provides context-specific ranges for common clinical variables

# TRANSFORMATION SELECTION:
# - Log: Right-skewed data, multiplicative errors
# - Sqrt: Count data, variance proportional to mean
# - None: Symmetric or uniform data

# ═══════════════════════════════════════════════════════════
