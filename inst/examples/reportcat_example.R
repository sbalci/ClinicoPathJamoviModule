# ═══════════════════════════════════════════════════════════
# REPORTCAT - COMPREHENSIVE USAGE EXAMPLES
# ═══════════════════════════════════════════════════════════
#
# This file demonstrates all features of the reportcat function
# (Summary of Categorical Variables) using the reportcat_test dataset.
#
# Generated: 2026-01-04

library(ClinicoPath)
data(reportcat_test)

# ═══════════════════════════════════════════════════════════
# BASIC USAGE
# ═══════════════════════════════════════════════════════════

# Example 1: Single Variable Summary
reportcat(
  data = reportcat_test,
  vars = 'sex'
)

# Example 2: Multiple Variables
reportcat(
  data = reportcat_test,
  vars = c('sex', 'vital_status', 'test_result')
)

# Example 3: Three to Five Variables
reportcat(
  data = reportcat_test,
  vars = c('treatment_type', 'tumor_location', 'ethnicity', 'marital_status')
)

# ═══════════════════════════════════════════════════════════
# BINARY CATEGORICAL VARIABLES (2 Levels)
# ═══════════════════════════════════════════════════════════

# Example 4: Balanced Binary Variable (50/50)
reportcat(
  data = reportcat_test,
  vars = 'sex'
)

# Example 5: Imbalanced Binary Variable (75/25)
reportcat(
  data = reportcat_test,
  vars = 'vital_status'
)

# Example 6: Highly Skewed Binary Variable (85/15)
reportcat(
  data = reportcat_test,
  vars = 'test_result'
)

# Example 7: Binary Variable with Missing Data (5% missing)
reportcat(
  data = reportcat_test,
  vars = 'response'
)

# Example 8: Multiple Binary Variables
reportcat(
  data = reportcat_test,
  vars = c('sex', 'vital_status', 'test_result', 'response')
)

# ═══════════════════════════════════════════════════════════
# ORDINAL CATEGORICAL VARIABLES (Ordered Levels)
# ═══════════════════════════════════════════════════════════

# Example 9: Tumor Stage (I-IV, Ordered)
reportcat(
  data = reportcat_test,
  vars = 'stage'
)

# Example 10: Tumor Grade (1-3, Ordered)
reportcat(
  data = reportcat_test,
  vars = 'grade'
)

# Example 11: Pain Severity (5 Ordered Levels)
reportcat(
  data = reportcat_test,
  vars = 'pain_severity'
)

# Example 12: Performance Status (ECOG 0-4)
reportcat(
  data = reportcat_test,
  vars = 'performance_status'
)

# Example 13: Tumor Size Categories (Ordered by Size)
reportcat(
  data = reportcat_test,
  vars = 'tumor_size_category'
)

# Example 14: Age Groups (Ordered)
reportcat(
  data = reportcat_test,
  vars = 'age_group'
)

# Example 15: Multiple Ordinal Variables
reportcat(
  data = reportcat_test,
  vars = c('stage', 'grade', 'pain_severity', 'performance_status')
)

# ═══════════════════════════════════════════════════════════
# NOMINAL CATEGORICAL VARIABLES (Unordered, 3-5 Levels)
# ═══════════════════════════════════════════════════════════

# Example 16: Treatment Type (3 Categories)
reportcat(
  data = reportcat_test,
  vars = 'treatment_type'
)

# Example 17: Tumor Location (4 Categories)
reportcat(
  data = reportcat_test,
  vars = 'tumor_location'
)

# Example 18: Ethnicity (5 Categories)
reportcat(
  data = reportcat_test,
  vars = 'ethnicity'
)

# Example 19: Marital Status (4 Categories)
reportcat(
  data = reportcat_test,
  vars = 'marital_status'
)

# Example 20: Multiple Nominal Variables (3-5 Levels)
reportcat(
  data = reportcat_test,
  vars = c('treatment_type', 'tumor_location', 'ethnicity', 'marital_status')
)

# ═══════════════════════════════════════════════════════════
# NOMINAL VARIABLES WITH MANY LEVELS (6-10 Levels)
# ═══════════════════════════════════════════════════════════

# Example 21: Histology Type (8 Categories)
reportcat(
  data = reportcat_test,
  vars = 'histology'
)

# Example 22: Primary Site (10 Anatomical Locations)
reportcat(
  data = reportcat_test,
  vars = 'primary_site'
)

# Example 23: Comorbidity (7 Conditions)
reportcat(
  data = reportcat_test,
  vars = 'comorbidity'
)

# Example 24: Multiple Variables with Many Levels
reportcat(
  data = reportcat_test,
  vars = c('histology', 'primary_site', 'comorbidity')
)

# ═══════════════════════════════════════════════════════════
# BALANCED DISTRIBUTION VARIABLES
# ═══════════════════════════════════════════════════════════

# Example 25: Perfectly Balanced 3 Categories (33% Each)
reportcat(
  data = reportcat_test,
  vars = 'treatment_arm'
)

# Example 26: Perfectly Balanced 4 Categories (25% Each)
reportcat(
  data = reportcat_test,
  vars = 'blood_type'
)

# Example 27: Multiple Balanced Variables
reportcat(
  data = reportcat_test,
  vars = c('treatment_arm', 'blood_type')
)

# ═══════════════════════════════════════════════════════════
# SKEWED DISTRIBUTION VARIABLES (Rare Categories)
# ═══════════════════════════════════════════════════════════

# Example 28: Variable with One Very Rare Category (<2%)
reportcat(
  data = reportcat_test,
  vars = 'mutation_status'
)

# Example 29: Variable with Multiple Rare Categories
reportcat(
  data = reportcat_test,
  vars = 'diagnosis_rare'
)

# Example 30: Compare Balanced vs Skewed Distributions
reportcat(
  data = reportcat_test,
  vars = c('treatment_arm',      # Balanced
           'mutation_status',    # One rare
           'diagnosis_rare')     # Multiple rare
)

# ═══════════════════════════════════════════════════════════
# VARIABLES WITH DIFFERENT MISSING DATA PERCENTAGES
# ═══════════════════════════════════════════════════════════

# Example 31: Low Missing Data (3%)
reportcat(
  data = reportcat_test,
  vars = 'smoking_status'
)

# Example 32: Moderate Missing Data (15%)
reportcat(
  data = reportcat_test,
  vars = 'alcohol_use'
)

# Example 33: High Missing Data (35%)
reportcat(
  data = reportcat_test,
  vars = 'genetic_marker'
)

# Example 34: Very High Missing Data (60%)
reportcat(
  data = reportcat_test,
  vars = 'biomarker_optional'
)

# Example 35: Compare Variables with Different Missing Percentages
reportcat(
  data = reportcat_test,
  vars = c('smoking_status',      # 3% missing
           'alcohol_use',         # 15% missing
           'genetic_marker',      # 35% missing
           'biomarker_optional')  # 60% missing
)

# ═══════════════════════════════════════════════════════════
# VARIABLES WITH SPECIAL CHARACTERS IN LEVELS
# ═══════════════════════════════════════════════════════════

# Example 36: Levels with Parentheses, Commas, NOS
reportcat(
  data = reportcat_test,
  vars = 'diagnosis_detailed'
)

# Example 37: Levels with Accented Characters (International)
reportcat(
  data = reportcat_test,
  vars = 'country_origin'
)

# Example 38: Levels with Numbers and Units
reportcat(
  data = reportcat_test,
  vars = 'tumor_size_category'
)

# Example 39: Multiple Variables with Special Characters
reportcat(
  data = reportcat_test,
  vars = c('diagnosis_detailed', 'country_origin', 'tumor_size_category')
)

# ═══════════════════════════════════════════════════════════
# EDGE CASES
# ═══════════════════════════════════════════════════════════

# Example 40: All Missing Data (100% Missing)
# Expected: Error or warning about no valid data
reportcat(
  data = reportcat_test,
  vars = 'future_test'
)

# Example 41: Constant Variable (Single Level Only)
# Expected: Summary showing 100% in one category
reportcat(
  data = reportcat_test,
  vars = 'study_site'
)

# Example 42: Variable with One Very Rare Observation
# Expected: Shows n=299 in one category, n=1 in rare category
reportcat(
  data = reportcat_test,
  vars = 'rare_mutation'
)

# Example 43: Factor with Levels but All NA
# Expected: Error or warning about empty variable
reportcat(
  data = reportcat_test,
  vars = 'filter_fail'
)

# ═══════════════════════════════════════════════════════════
# CHARACTER VECTORS (Non-Factor Variables)
# ═══════════════════════════════════════════════════════════

# Example 44: Character Vector (Will be Converted to Factor)
reportcat(
  data = reportcat_test,
  vars = 'protocol'
)

# Example 45: Character Vector with Missing Values
reportcat(
  data = reportcat_test,
  vars = 'region'
)

# Example 46: Mix of Factors and Characters
reportcat(
  data = reportcat_test,
  vars = c('sex',       # Factor
           'stage',     # Ordered factor
           'protocol',  # Character
           'region')    # Character with NA
)

# ═══════════════════════════════════════════════════════════
# COMPREHENSIVE SUMMARIES
# ═══════════════════════════════════════════════════════════

# Example 47: Comprehensive Patient Demographics
reportcat(
  data = reportcat_test,
  vars = c('age_group', 'sex', 'ethnicity', 'marital_status')
)

# Example 48: Comprehensive Clinical Characteristics
reportcat(
  data = reportcat_test,
  vars = c('primary_site', 'histology', 'stage', 'grade')
)

# Example 49: Comprehensive Treatment Summary
reportcat(
  data = reportcat_test,
  vars = c('treatment_type', 'treatment_arm', 'performance_status')
)

# Example 50: Comprehensive Outcome Summary
reportcat(
  data = reportcat_test,
  vars = c('vital_status', 'test_result', 'response')
)

# ═══════════════════════════════════════════════════════════
# CLINICAL RESEARCH SCENARIOS
# ═══════════════════════════════════════════════════════════

# Scenario 1: Baseline Patient Characteristics Table
# Summarize demographic and clinical variables for Table 1 of a manuscript
reportcat(
  data = reportcat_test,
  vars = c('age_group',
           'sex',
           'ethnicity',
           'marital_status',
           'smoking_status',
           'alcohol_use',
           'comorbidity')
)

# Scenario 2: Tumor Characteristics Summary
# Describe tumor-related variables for pathology report
reportcat(
  data = reportcat_test,
  vars = c('primary_site',
           'histology',
           'stage',
           'grade',
           'tumor_location',
           'tumor_size_category')
)

# Scenario 3: Treatment Distribution Analysis
# Assess how patients were distributed across treatment groups
reportcat(
  data = reportcat_test,
  vars = c('treatment_type',
           'treatment_arm',
           'performance_status')
)

# Scenario 4: Molecular/Genetic Profiling Summary
# Summarize biomarker and mutation status
reportcat(
  data = reportcat_test,
  vars = c('mutation_status',
           'genetic_marker',
           'biomarker_optional',
           'test_result')
)

# Scenario 5: Outcome Variables Summary
# Summarize clinical outcomes and response variables
reportcat(
  data = reportcat_test,
  vars = c('vital_status',
           'response',
           'test_result')
)

# Scenario 6: Data Quality Assessment
# Identify variables with missing data or rare categories
reportcat(
  data = reportcat_test,
  vars = c('smoking_status',      # 3% missing - excellent
           'alcohol_use',         # 15% missing - acceptable
           'genetic_marker',      # 35% missing - concerning
           'biomarker_optional',  # 60% missing - poor
           'mutation_status',     # Has rare category
           'diagnosis_rare')      # Multiple rare categories
)

# Scenario 7: Subgroup Analysis Preparation
# Check distribution of subgroups before stratified analysis
reportcat(
  data = reportcat_test,
  vars = c('treatment_arm',      # Randomization check (should be balanced)
           'stage',              # Disease severity
           'performance_status', # Patient fitness
           'age_group')          # Age stratification
)

# Scenario 8: Regulatory/Clinical Trial Reporting
# Comprehensive summary for regulatory submission or trial report
reportcat(
  data = reportcat_test,
  vars = c('age_group',
           'sex',
           'ethnicity',
           'treatment_arm',
           'stage',
           'grade',
           'performance_status',
           'comorbidity',
           'vital_status')
)

# ═══════════════════════════════════════════════════════════
# INTERPRETATION GUIDE
# ═══════════════════════════════════════════════════════════

# OUTPUT INTERPRETATION:
#
# The reportcat function produces:
# 1. **Text Summary**: Level-by-level breakdown with counts and percentages
#    - Shows each category with: "Level: n = count, X% of valid cases"
#    - Reports total observations and number of levels
#    - Lists missing values separately
#
# 2. **Visual Summary Table** (gtExtras):
#    - Automatically generated visual summary
#    - Shows distribution of categories
#    - Highlights missing data patterns
#
# MISSING DATA INTERPRETATION:
# - Reported as separate count after valid categories
# - Percentages calculated using valid (non-missing) observations
# - Variables with high missing % may need attention
#
# RARE CATEGORY IDENTIFICATION:
# - Look for categories with very low counts (n < 5-10)
# - Categories <5% may violate chi-squared test assumptions
# - Consider grouping or excluding rare categories
#
# BALANCED VS SKEWED:
# - Balanced: Categories have similar frequencies
# - Skewed: One or more categories dominate
# - Important for stratified analyses and randomization checks
#
# SAMPLE SIZE ADEQUACY:
# - Each category should have sufficient n for planned analyses
# - Rule of thumb: n ≥ 5-10 per category for chi-squared tests
# - Larger n required for regression models
#
# ═══════════════════════════════════════════════════════════
# BEST PRACTICES
# ═══════════════════════════════════════════════════════════

# 1. **Variable Selection**:
#    - Select only categorical variables (factors or characters)
#    - Numeric variables will cause errors
#    - Ordinal variables should be properly ordered factors

# 2. **Handling Missing Data**:
#    - Missing values are reported separately
#    - Consider imputation if missing % is high (>20%)
#    - Document missing data patterns in methods

# 3. **Rare Categories**:
#    - Categories <5% may need grouping as "Other"
#    - Consider Fisher's exact test instead of chi-squared
#    - Document any category grouping decisions

# 4. **Clinical Reporting**:
#    - Use for Table 1 (baseline characteristics)
#    - Report both n and percentages
#    - Include missing data counts
#    - Consider stratifying by key variables (e.g., treatment arm)

# 5. **Quality Control**:
#    - Check for unexpected categories (data entry errors)
#    - Verify category names and ordering
#    - Ensure factors are properly leveled
#    - Check that percentages sum to 100% (within rounding)

# ═══════════════════════════════════════════════════════════
# TROUBLESHOOTING
# ═══════════════════════════════════════════════════════════

# Error: "Non-categorical variables detected"
# Solution: Ensure selected variables are factors or characters
# Example fix:
# reportcat_test$age_numeric <- as.numeric(reportcat_test$age_group)
# reportcat(data = reportcat_test, vars = 'age_numeric')  # Error!
# reportcat(data = reportcat_test, vars = 'age_group')    # Works!

# Warning: "Variables with no valid levels or all missing values"
# Solution: Variable has all NA or zero levels
# Example:
# reportcat(data = reportcat_test, vars = 'future_test')  # All NA
# These variables are excluded from analysis

# Issue: Character vectors instead of factors
# Solution: reportcat automatically converts characters to factors
# Example:
# str(reportcat_test$protocol)  # Character
# reportcat(data = reportcat_test, vars = 'protocol')  # Works! Converted to factor

# Issue: Percentages don't sum to 100%
# Reason: Rounding to whole percentages
# Solution: This is normal and acceptable for reporting

# ═══════════════════════════════════════════════════════════
