# ═══════════════════════════════════════════════════════════
# DATA QUALITY ASSESSMENT - COMPREHENSIVE USAGE EXAMPLES
# ═══════════════════════════════════════════════════════════
#
# This file demonstrates all features of the dataquality function
# using the dataquality_test dataset.
#
# Generated: 2026-01-04

library(ClinicoPath)
data(dataquality_test)

# ═══════════════════════════════════════════════════════════
# EXAMPLE 1: Basic Data Quality Overview
# ═══════════════════════════════════════════════════════════
# Check all variables for basic quality metrics

dataquality(
  data = dataquality_test
)

# ═══════════════════════════════════════════════════════════
# EXAMPLE 2: Missing Value Analysis Only
# ═══════════════════════════════════════════════════════════
# Focus on variables with missing data

dataquality(
  data = dataquality_test,
  vars = c('tumor_size_low_missing', 'psa_moderate_missing',
           'biomarker_high_missing', 'genetic_test_very_high_missing'),
  check_missing = TRUE,
  showSummary = TRUE,
  showRecommendations = TRUE
)

# ═══════════════════════════════════════════════════════════
# EXAMPLE 3: Missing Value Analysis with Visual Patterns
# ═══════════════════════════════════════════════════════════
# Use visdat to visualize missing patterns

dataquality(
  data = dataquality_test,
  vars = c('tumor_size_low_missing', 'psa_moderate_missing',
           'biomarker_high_missing', 'genetic_test_very_high_missing',
           'platelets_mixed_issues'),
  check_missing = TRUE,
  plot_missing_patterns = TRUE,
  missing_threshold_visual = 10,
  showRecommendations = TRUE
)

# ═══════════════════════════════════════════════════════════
# EXAMPLE 4: Duplicate Value Detection
# ═══════════════════════════════════════════════════════════
# Check variables for duplicate values

dataquality(
  data = dataquality_test,
  vars = c('tumor_grade_duplicates', 'stage_duplicates',
           'treatment_cycles_duplicates', 'hospital_site_constant'),
  check_duplicates = TRUE,
  complete_cases_only = FALSE,  # Check within-variable duplicates
  showSummary = TRUE
)

# ═══════════════════════════════════════════════════════════
# EXAMPLE 5: Complete Duplicate Rows Detection
# ═══════════════════════════════════════════════════════════
# Find complete duplicate rows across multiple variables

dataquality(
  data = dataquality_test,
  vars = c('age_perfect', 'sex_perfect', 'stage_duplicates',
           'tumor_grade_duplicates'),
  check_duplicates = TRUE,
  complete_cases_only = TRUE,  # Check for complete duplicate rows
  showRecommendations = TRUE
)

# ═══════════════════════════════════════════════════════════
# EXAMPLE 6: Near-Zero Variance Detection
# ═══════════════════════════════════════════════════════════
# Identify variables with very little variation

dataquality(
  data = dataquality_test,
  vars = c('hospital_site_constant', 'surgery_type_near_zero',
           'temperature_near_zero'),
  showSummary = TRUE,
  showRecommendations = TRUE,
  showExplanations = TRUE
)

# ═══════════════════════════════════════════════════════════
# EXAMPLE 7: High Cardinality Detection
# ═══════════════════════════════════════════════════════════
# Identify variables with too many unique values

dataquality(
  data = dataquality_test,
  vars = c('biomarker_continuous_high_card', 'pathology_report_id_high_card',
           'physician_notes_high_card'),
  showSummary = TRUE,
  showRecommendations = TRUE
)

# ═══════════════════════════════════════════════════════════
# EXAMPLE 8: Outlier Detection
# ═══════════════════════════════════════════════════════════
# Check numeric variables for outliers (IQR method)

dataquality(
  data = dataquality_test,
  vars = c('wbc_count_with_outliers', 'hospital_stay_with_outliers',
           'creatinine_with_outliers'),
  showSummary = TRUE,
  showRecommendations = TRUE,
  showExplanations = TRUE
)

# ═══════════════════════════════════════════════════════════
# EXAMPLE 9: Data Type Analysis
# ═══════════════════════════════════════════════════════════
# Analyze different data types in the dataset

dataquality(
  data = dataquality_test,
  vars = c('num_biopsies_integer', 'hemoglobin_numeric',
           'diagnosis_character', 'ecog_status_factor',
           'diagnosis_date', 'smoking_history_logical'),
  plot_data_types = TRUE,
  showSummary = TRUE
)

# ═══════════════════════════════════════════════════════════
# EXAMPLE 10: Comprehensive Data Overview with vis_dat
# ═══════════════════════════════════════════════════════════
# Visual overview of entire dataset structure

dataquality(
  data = dataquality_test,
  vars = c('age_perfect', 'sex_perfect', 'tumor_size_low_missing',
           'psa_moderate_missing', 'biomarker_high_missing',
           'stage_duplicates', 'wbc_count_with_outliers'),
  plot_data_overview = TRUE,
  missing_threshold_visual = 5,
  showSummary = TRUE
)

# ═══════════════════════════════════════════════════════════
# EXAMPLE 11: All Visual Plots Combined
# ═══════════════════════════════════════════════════════════
# Use all visdat plotting features

dataquality(
  data = dataquality_test,
  vars = c('tumor_size_low_missing', 'psa_moderate_missing',
           'biomarker_high_missing', 'wbc_count_with_outliers',
           'diagnosis_character', 'ecog_status_factor'),
  plot_data_overview = TRUE,
  plot_missing_patterns = TRUE,
  plot_data_types = TRUE,
  missing_threshold_visual = 10,
  showSummary = TRUE
)

# ═══════════════════════════════════════════════════════════
# EXAMPLE 12: Mixed Quality Issues Analysis
# ═══════════════════════════════════════════════════════════
# Variables with multiple quality problems

dataquality(
  data = dataquality_test,
  vars = c('platelets_mixed_issues', 'comorbidity_mixed'),
  check_duplicates = TRUE,
  check_missing = TRUE,
  plot_missing_patterns = TRUE,
  showSummary = TRUE,
  showRecommendations = TRUE
)

# ═══════════════════════════════════════════════════════════
# EXAMPLE 13: High Missing Threshold Warning
# ═══════════════════════════════════════════════════════════
# Trigger strong warning for very high missing values (>50%)

dataquality(
  data = dataquality_test,
  vars = c('genetic_test_very_high_missing'),
  check_missing = TRUE,
  plot_missing_patterns = TRUE,
  showRecommendations = TRUE
)

# ═══════════════════════════════════════════════════════════
# EXAMPLE 14: Custom Missing Threshold Visual
# ═══════════════════════════════════════════════════════════
# Adjust visual highlighting threshold for missing data

dataquality(
  data = dataquality_test,
  vars = c('tumor_size_low_missing', 'psa_moderate_missing',
           'biomarker_high_missing', 'genetic_test_very_high_missing'),
  plot_missing_patterns = TRUE,
  missing_threshold_visual = 20,  # Highlight if >20% missing
  showSummary = TRUE
)

# ═══════════════════════════════════════════════════════════
# EXAMPLE 15: Educational Mode with Explanations
# ═══════════════════════════════════════════════════════════
# Show detailed explanations of quality metrics

dataquality(
  data = dataquality_test,
  vars = c('wbc_count_with_outliers', 'hospital_site_constant',
           'biomarker_continuous_high_card', 'platelets_mixed_issues'),
  check_duplicates = TRUE,
  check_missing = TRUE,
  showSummary = TRUE,
  showRecommendations = TRUE,
  showExplanations = TRUE
)

# ═══════════════════════════════════════════════════════════
# EXAMPLE 16: Complete Comprehensive Analysis
# ═══════════════════════════════════════════════════════════
# All features enabled for thorough quality assessment

dataquality(
  data = dataquality_test,
  vars = c('age_perfect', 'sex_perfect',
           'tumor_size_low_missing', 'psa_moderate_missing',
           'biomarker_high_missing', 'genetic_test_very_high_missing',
           'tumor_grade_duplicates', 'stage_duplicates',
           'hospital_site_constant', 'surgery_type_near_zero',
           'biomarker_continuous_high_card', 'pathology_report_id_high_card',
           'wbc_count_with_outliers', 'creatinine_with_outliers',
           'platelets_mixed_issues', 'comorbidity_mixed'),
  check_duplicates = TRUE,
  check_missing = TRUE,
  complete_cases_only = FALSE,
  plot_data_overview = TRUE,
  plot_missing_patterns = TRUE,
  plot_data_types = TRUE,
  missing_threshold_visual = 10,
  showSummary = TRUE,
  showRecommendations = TRUE,
  showExplanations = TRUE
)

# ═══════════════════════════════════════════════════════════
# CLINICAL RESEARCH SCENARIOS
# ═══════════════════════════════════════════════════════════

# ───────────────────────────────────────────────────────────
# SCENARIO 1: Pre-Analysis Data Screening
# ───────────────────────────────────────────────────────────
# Before running statistical analyses, check data quality
# of key clinical variables

dataquality(
  data = dataquality_test,
  vars = c('age_perfect', 'sex_perfect', 'stage_duplicates',
           'tumor_grade_duplicates', 'psa_moderate_missing',
           'wbc_count_with_outliers'),
  check_duplicates = FALSE,
  check_missing = TRUE,
  plot_data_overview = TRUE,
  plot_missing_patterns = TRUE,
  showSummary = TRUE,
  showRecommendations = TRUE
)

# ───────────────────────────────────────────────────────────
# SCENARIO 2: Data Cleaning Preparation
# ───────────────────────────────────────────────────────────
# Identify all quality issues before data cleaning

dataquality(
  data = dataquality_test,
  check_duplicates = TRUE,
  check_missing = TRUE,
  complete_cases_only = TRUE,  # Find duplicate patient records
  plot_data_overview = TRUE,
  missing_threshold_visual = 15,
  showSummary = TRUE,
  showRecommendations = TRUE
)

# ───────────────────────────────────────────────────────────
# SCENARIO 3: Biomarker Validation Study
# ───────────────────────────────────────────────────────────
# Check quality of biomarker measurements

dataquality(
  data = dataquality_test,
  vars = c('biomarker_high_missing', 'biomarker_continuous_high_card',
           'platelets_mixed_issues', 'creatinine_with_outliers'),
  check_missing = TRUE,
  plot_missing_patterns = TRUE,
  showSummary = TRUE,
  showRecommendations = TRUE,
  showExplanations = TRUE
)

# ───────────────────────────────────────────────────────────
# SCENARIO 4: Multi-Center Study Data Harmonization
# ───────────────────────────────────────────────────────────
# Check consistency across sites and identify site-specific issues

dataquality(
  data = dataquality_test,
  vars = c('hospital_site_constant', 'diagnosis_character',
           'ecog_status_factor', 'comorbidity_mixed'),
  check_duplicates = TRUE,
  plot_data_types = TRUE,
  showSummary = TRUE,
  showRecommendations = TRUE
)

# ───────────────────────────────────────────────────────────
# SCENARIO 5: Genetic/Molecular Testing Quality Control
# ───────────────────────────────────────────────────────────
# Assess completeness of expensive genetic tests

dataquality(
  data = dataquality_test,
  vars = c('genetic_test_very_high_missing', 'num_biopsies_integer',
           'pathology_report_id_high_card'),
  check_missing = TRUE,
  plot_missing_patterns = TRUE,
  missing_threshold_visual = 25,
  showSummary = TRUE,
  showRecommendations = TRUE
)

# ═══════════════════════════════════════════════════════════
# INTERPRETATION GUIDE
# ═══════════════════════════════════════════════════════════

# MISSING VALUES:
#   - <5%: Generally acceptable, minimal impact on analysis
#   - 5-15%: Moderate, may require imputation or sensitivity analysis
#   - 15-30%: High, imputation methods should be carefully considered
#   - >50%: Very high, variable may not be usable (triggers warning)

# DUPLICATES:
#   - High duplicates in categorical variables are expected (e.g., sex, grade)
#   - Constant variables (100% duplicates) provide no information
#   - Complete duplicate rows suggest data entry errors

# NEAR-ZERO VARIANCE:
#   - Variables with <1% variance are uninformative
#   - Constant variables should be removed before modeling
#   - Very low variance may indicate measurement error

# HIGH CARDINALITY:
#   - >50 unique values in categorical variables
#   - May indicate need for grouping or treating as continuous
#   - Free text fields require special handling

# OUTLIERS:
#   - Detected using IQR method (Q1 - 1.5*IQR, Q3 + 1.5*IQR)
#   - May indicate measurement errors or true extreme values
#   - Clinical context is crucial for interpretation
#   - Consider robust statistical methods if outliers are legitimate

# ═══════════════════════════════════════════════════════════
# RECOMMENDED WORKFLOW
# ═══════════════════════════════════════════════════════════

# Step 1: Initial overview
#   - Run dataquality() with plot_data_overview = TRUE
#   - Identify obvious problems

# Step 2: Missing value analysis
#   - Use check_missing = TRUE and plot_missing_patterns = TRUE
#   - Determine missingness mechanism (MCAR, MAR, MNAR)
#   - Decide on imputation strategy

# Step 3: Duplicate detection
#   - Use check_duplicates = TRUE
#   - Check complete_cases_only = TRUE for duplicate records
#   - Investigate and resolve duplicates

# Step 4: Variable screening
#   - Remove constant variables (near-zero variance)
#   - Address high cardinality categorical variables
#   - Investigate outliers with clinical team

# Step 5: Documentation
#   - Document all quality issues found
#   - Record decisions made about problematic data
#   - Save cleaned dataset with version control

# ═══════════════════════════════════════════════════════════
