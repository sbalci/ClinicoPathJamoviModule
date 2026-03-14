# ===============================================================
# Example Usage: lassocox (LASSO Cox Regression)
# ===============================================================
#
# Comprehensive examples demonstrating LASSO-penalized Cox regression
# for variable selection in survival analysis across diverse clinical
# scenarios.

library(ClinicoPath)

# ===============================================================
# Example 1: Breast Cancer -- Standard Clinicopathological Variables
# ===============================================================
# Classic scenario: moderate number of predictors, well-established
# risk factors. LASSO identifies which pathological features are
# most prognostic after regularization.

data(lassocox_breast_cancer, package = "ClinicoPath")

result_breast <- lassocox(
  data = lassocox_breast_cancer,
  elapsedtime = "survival_months",
  outcome = "death",
  outcomeLevel = "Dead",
  explanatory = c("age", "tumor_size_cm", "grade", "stage",
                   "lymph_nodes_positive", "er_status", "pr_status",
                   "her2_status", "ki67_percent", "histology", "lvi",
                   "chemotherapy", "radiation", "albumin"),
  lambda = "lambda.1se",
  nfolds = 10,
  suitabilityCheck = TRUE,
  cv_plot = TRUE,
  coef_plot = TRUE,
  survival_plot = TRUE
)

# Clinical Interpretation:
# - Grade, lymph node count, and tumor size are typically retained
# - ER/PR status provides protective signal (negative coefficient)
# - Ki-67 captures proliferative activity
# - The 1SE rule provides a more parsimonious model than lambda.min


# ===============================================================
# Example 2: Lung Cancer -- Clinical Trial with Missing Data
# ===============================================================
# Real-world scenario: mixed continuous/categorical predictors,
# some missing values in lab results and tumor measurements.

data(lassocox_lung_cancer, package = "ClinicoPath")

result_lung <- lassocox(
  data = lassocox_lung_cancer,
  elapsedtime = "follow_up_months",
  outcome = "progression",
  outcomeLevel = "Yes",
  explanatory = c("age", "gender", "smoking_status", "histology",
                   "stage", "tumor_size_cm", "ecog_performance_status",
                   "hemoglobin_g_dl", "wbc_count_k_ul",
                   "platelet_count_k_ul", "creatinine_mg_dl",
                   "treatment_type"),
  lambda = "lambda.min",
  nfolds = 10,
  standardize = TRUE,
  showVariableImportance = TRUE,
  showModelComparison = TRUE
)

# Clinical Interpretation:
# - Stage and ECOG performance status are strong prognostic factors
# - Smoking status contributes independently
# - lambda.min retains more variables; useful for exploratory analysis
# - Model comparison shows LASSO vs standard Cox trade-offs


# ===============================================================
# Example 3: Cardiovascular Risk -- Correlated Risk Factors
# ===============================================================
# Challenge: Many risk factors are correlated (e.g., BP measures,
# lipid panel). LASSO selects representative variables.

data(lassocox_cardiovascular, package = "ClinicoPath")

result_cvd <- lassocox(
  data = lassocox_cardiovascular,
  elapsedtime = "time_to_event_months",
  outcome = "cv_event",
  outcomeLevel = "Event",
  explanatory = c("age_years", "gender", "bmi_kg_m2",
                   "systolic_bp_mmhg", "diastolic_bp_mmhg",
                   "total_cholesterol_mg_dl",
                   "hdl_cholesterol_mg_dl",
                   "ldl_cholesterol_mg_dl",
                   "diabetes_mellitus", "hypertension",
                   "smoking_status", "family_history_cvd",
                   "ace_inhibitor_use", "statin_use",
                   "aspirin_use"),
  lambda = "lambda.1se",
  nfolds = 10,
  suitabilityCheck = TRUE,
  includeClinicalGuidance = TRUE
)

# Clinical Interpretation:
# - Age, diabetes, and hypertension are typically selected
# - Among lipid measures, LASSO picks the most informative (often HDL)
# - Medication use (ACE inhibitor, statin) may appear as protective factors
# - BP measures are correlated; usually one is retained


# ===============================================================
# Example 4: Small Cohort -- Testing Robustness
# ===============================================================
# Challenge: Limited sample (n=75), potential for instability.
# Reduced CV folds ensure reliable cross-validation.

data(lassocox_small_cohort, package = "ClinicoPath")

result_small <- lassocox(
  data = lassocox_small_cohort,
  elapsedtime = "time_months",
  outcome = "event_occurred",
  outcomeLevel = "Yes",
  explanatory = c("age", "gender", "biomarker_a", "biomarker_b",
                   "biomarker_c", "treatment_group", "severity_score"),
  lambda = "lambda.1se",
  nfolds = 5,
  suitabilityCheck = TRUE,
  showExplanations = TRUE,
  showMethodologyNotes = TRUE
)

# Clinical Interpretation:
# - With small n, the suitability assessment may flag EPV concerns
# - Results should be interpreted cautiously and validated externally
# - The 1SE rule is especially important here to avoid overfitting
# - Biomarker A and B have true effects; C is noise


# ===============================================================
# Example 5: Exploring Variable Importance
# ===============================================================
# Use variable importance analysis to understand which predictors
# are most stable across the regularization path.

result_importance <- lassocox(
  data = lassocox_breast_cancer,
  elapsedtime = "survival_months",
  outcome = "death",
  outcomeLevel = "Dead",
  explanatory = c("age", "tumor_size_cm", "grade", "stage",
                   "lymph_nodes_positive", "er_status",
                   "her2_status", "ki67_percent", "lvi",
                   "albumin", "hemoglobin"),
  showVariableImportance = TRUE,
  showModelComparison = TRUE,
  cv_plot = TRUE,
  coef_plot = TRUE,
  survival_plot = TRUE
)

# Clinical Interpretation:
# - Variables with high importance scores enter the model early
#   on the regularization path and persist across lambda values
# - High path inclusion proportion = stable selection
# - Model comparison: LASSO should match or exceed standard Cox
#   with fewer variables, indicating regularization benefit
