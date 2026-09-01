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
# risk factors. LASSO estimates a sparse set of retained predictor columns.

data(lassocox_breast_cancer, package = "ClinicoPath")

result_breast <- lassocox(
  data = lassocox_breast_cancer,
  elapsedtime = "survival_months",
  outcome = "death",
  outcomeLevel = "Dead",
  censorLevel = "Alive",
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

# Inspect the retained columns and penalized coefficients; the selected set is
# data-dependent. The 1-SE rule is the more penalized of the two tuning rules.


# ===============================================================
# Example 2: Lung Cancer -- Input Validation
# ===============================================================
# This stress fixture contains follow-up values rounded to zero and only two
# censored observations. The analysis should return an actionable validation
# message instead of silently changing times or fitting an unstable model.

data(lassocox_lung_cancer, package = "ClinicoPath")

result_lung <- lassocox(
  data = lassocox_lung_cancer,
  elapsedtime = "follow_up_months",
  outcome = "progression",
  outcomeLevel = "Yes",
  censorLevel = "No",
  explanatory = c("age", "gender", "smoking_status", "histology",
                   "stage", "tumor_size_cm", "ecog_performance_status",
                   "hemoglobin_g_dl", "wbc_count_k_ul",
                   "platelet_count_k_ul", "creatinine_mg_dl",
                   "treatment_type"),
  lambda = "lambda.min",
  nfolds = 10,
  standardize = TRUE
)

# Expected result: a message asking the analyst to check the time origin and
# measurement resolution. Correct the source data using subject-matter knowledge
# before fitting; do not add an arbitrary constant automatically.


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
  censorLevel = "No Event",
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

# Correlated columns can compete under an L1 penalty, so retained columns may
# change with the data and fold assignment. Do not interpret medication
# coefficients causally.


# ===============================================================
# Example 4: Small Cohort -- Input Validation
# ===============================================================
# This stress fixture also contains follow-up values rounded to zero. It
# demonstrates the same strictly-positive-time validation in a small cohort.

data(lassocox_small_cohort, package = "ClinicoPath")

result_small <- lassocox(
  data = lassocox_small_cohort,
  elapsedtime = "time_months",
  outcome = "event_occurred",
  outcomeLevel = "Yes",
  censorLevel = "No",
  explanatory = c("age", "gender", "biomarker_a", "biomarker_b",
                   "biomarker_c", "treatment_group", "severity_score"),
  lambda = "lambda.1se",
  nfolds = 5,
  suitabilityCheck = TRUE,
  showExplanations = TRUE,
  showMethodologyNotes = TRUE
)

# Expected result: a strictly-positive-time validation message. After a
# defensible correction to the source data, a small development sample still
# requires cautious interpretation and validation of the full modeling process.


# ===============================================================
# Example 5: Exploring Variable Importance
# ===============================================================
# Use coefficient descriptives to inspect scale-adjusted magnitude and the
# proportion of the fitted lambda path on which each retained column is nonzero.

result_importance <- lassocox(
  data = lassocox_breast_cancer,
  elapsedtime = "survival_months",
  outcome = "death",
  outcomeLevel = "Dead",
  censorLevel = "Alive",
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

# Path inclusion is not resampling-based selection frequency or evidence of
# stability. The model-comparison statistics are apparent and selection-biased.
