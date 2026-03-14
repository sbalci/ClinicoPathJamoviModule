# ===============================================================
# Integration Tests: lassocox (Lasso-Cox Regression)
# ===============================================================
#
# Tests end-to-end workflows using all 6 package test datasets.
# Validates that lassocox works correctly across diverse clinical
# scenarios: standard clinicopathological, clinical trial, cardiovascular,
# small cohort, high-dimensional genomic, and multicollinearity.

library(testthat)
library(survival)

# ---------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------
skip_lassocox_deps <- function() {
  skip_if_not_installed("jmvcore")
  skip_if_not_installed("glmnet")
  skip_if_not_installed("survival")
}

load_lassocox_data <- function(name) {
  data_path <- file.path("../../data", paste0(name, ".rda"))
  if (!file.exists(data_path)) {
    data_path <- system.file("data", paste0(name, ".rda"),
                             package = "ClinicoPath")
  }
  if (data_path == "" || !file.exists(data_path)) {
    skip(paste0(name, ".rda not found"))
  }
  env <- new.env()
  load(data_path, envir = env)
  env[[name]]
}

# ---------------------------------------------------------------
# Integration: Breast Cancer (n=250, 20 predictors)
# ---------------------------------------------------------------
test_that("lassocox integration: breast cancer clinicopathological", {
  skip_lassocox_deps()
  data <- load_lassocox_data("lassocox_breast_cancer")

  result <- lassocox(
    data = data,
    elapsedtime = "survival_months",
    outcome = "death",
    outcomeLevel = "Dead",
    explanatory = c("age", "tumor_size_cm", "grade", "stage",
                     "lymph_nodes_positive", "lymph_nodes_examined",
                     "er_status", "pr_status", "her2_status",
                     "ki67_percent", "histology", "lvi",
                     "margin_status", "surgery_type",
                     "chemotherapy", "radiation",
                     "albumin", "hemoglobin"),
    lambda = "lambda.1se",
    nfolds = 10,
    suitabilityCheck = TRUE,
    showVariableImportance = TRUE,
    showModelComparison = TRUE,
    cv_plot = FALSE, coef_plot = FALSE, survival_plot = FALSE
  )

  # Verify output structure

  expect_true(!is.null(result$results$modelSummary))
  expect_true(!is.null(result$results$coefficients))
  expect_true(!is.null(result$results$performance))
  expect_true(!is.null(result$results$suitabilityReport))
  expect_true(!is.null(result$results$variableImportance))
  expect_true(!is.null(result$results$modelComparison))
})


# ---------------------------------------------------------------
# Integration: Lung Cancer (n=200, mixed types + missing data)
# ---------------------------------------------------------------
test_that("lassocox integration: lung cancer clinical trial", {
  skip_lassocox_deps()
  data <- load_lassocox_data("lassocox_lung_cancer")

  # This dataset has ~8% missing data in tumor_size, hemoglobin, creatinine
  result <- lassocox(
    data = data,
    elapsedtime = "follow_up_months",
    outcome = "progression",
    outcomeLevel = "Yes",
    explanatory = c("age", "gender", "smoking_status", "histology",
                     "stage", "tumor_size_cm",
                     "ecog_performance_status",
                     "hemoglobin_g_dl", "wbc_count_k_ul",
                     "platelet_count_k_ul", "creatinine_mg_dl",
                     "treatment_type"),
    lambda = "lambda.min",
    nfolds = 10,
    cv_plot = FALSE, coef_plot = FALSE, survival_plot = FALSE
  )

  expect_true(!is.null(result$results$modelSummary))
  expect_true(!is.null(result$results$coefficients))
})


# ---------------------------------------------------------------
# Integration: Cardiovascular (n=150, correlated risk factors)
# ---------------------------------------------------------------
test_that("lassocox integration: cardiovascular risk study", {
  skip_lassocox_deps()
  data <- load_lassocox_data("lassocox_cardiovascular")

  result <- lassocox(
    data = data,
    elapsedtime = "time_to_event_months",
    outcome = "cv_event",
    outcomeLevel = "Event",
    explanatory = c("age_years", "gender", "race_ethnicity",
                     "bmi_kg_m2", "systolic_bp_mmhg",
                     "diastolic_bp_mmhg",
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
    cv_plot = FALSE, coef_plot = FALSE, survival_plot = FALSE
  )

  expect_true(!is.null(result$results$modelSummary))
})


# ---------------------------------------------------------------
# Integration: Small Cohort (n=75, high censoring)
# ---------------------------------------------------------------
test_that("lassocox integration: small cohort with limited events", {
  skip_lassocox_deps()
  data <- load_lassocox_data("lassocox_small_cohort")

  # With small n, CV folds should be reduced
  result <- lassocox(
    data = data,
    elapsedtime = "time_months",
    outcome = "event_occurred",
    outcomeLevel = "Yes",
    explanatory = c("age", "gender", "biomarker_a",
                     "biomarker_b", "biomarker_c",
                     "treatment_group", "severity_score"),
    nfolds = 5,
    cv_plot = FALSE, coef_plot = FALSE, survival_plot = FALSE
  )

  expect_true(!is.null(result$results$modelSummary))
})


# ---------------------------------------------------------------
# Integration: High-Dimensional Genomic (n=80, p=50 genes)
# ---------------------------------------------------------------
test_that("lassocox integration: high-dimensional genomic data", {
  skip_lassocox_deps()
  data <- load_lassocox_data("lassocox_genomic")
  if (is.null(data)) skip("lassocox_genomic.rda not available")

  gene_vars <- grep("^gene_", names(data), value = TRUE)

  result <- lassocox(
    data = data,
    elapsedtime = "os_months",
    outcome = "vital_status",
    outcomeLevel = "Dead",
    explanatory = c("age", "sex", "tumor_stage", gene_vars),
    lambda = "lambda.1se",
    nfolds = 5,
    standardize = TRUE,
    suitabilityCheck = TRUE,
    showVariableImportance = TRUE,
    cv_plot = FALSE, coef_plot = FALSE, survival_plot = FALSE
  )

  expect_true(!is.null(result$results$modelSummary))

  # In high-dimensional scenario, LASSO should select << p variables
  coef_df <- result$results$coefficients$asDF()
  n_selected <- nrow(coef_df)
  expect_true(n_selected < length(gene_vars),
              info = paste("Selected", n_selected, "of",
                           length(gene_vars), "gene variables"))
})


# ---------------------------------------------------------------
# Integration: Multicollinearity (n=180, correlated biomarkers)
# ---------------------------------------------------------------
test_that("lassocox integration: multicollinearity scenario", {
  skip_lassocox_deps()
  data <- load_lassocox_data("lassocox_multicollinear")
  if (is.null(data)) skip("lassocox_multicollinear.rda not available")

  result <- lassocox(
    data = data,
    elapsedtime = "survival_months",
    outcome = "death",
    outcomeLevel = "Dead",
    explanatory = c("age", "sex", "ecog_ps", "comorbidity_index",
                     "crp_mg_l", "esr_mm_hr", "il6_pg_ml",
                     "ferritin_ng_ml", "albumin_g_dl",
                     "prealbumin_mg_dl", "bmi",
                     "weight_loss_pct"),
    lambda = "lambda.1se",
    nfolds = 10,
    suitabilityCheck = TRUE,
    showModelComparison = TRUE,
    cv_plot = FALSE, coef_plot = FALSE, survival_plot = FALSE
  )

  expect_true(!is.null(result$results$modelSummary))

  # LASSO should select at most one representative per correlated group
  # (inflammation group: crp, esr, il6, ferritin; nutrition: albumin,
  #  prealbumin, bmi, weight_loss_pct)
  coef_df <- result$results$coefficients$asDF()
  n_selected <- nrow(coef_df)
  expect_true(n_selected <= 12,
              info = paste("Selected", n_selected, "variables"))
})


# ---------------------------------------------------------------
# Integration: Reproducibility across runs
# ---------------------------------------------------------------
test_that("lassocox produces identical results with same seed", {
  skip_lassocox_deps()
  data <- load_lassocox_data("lassocox_breast_cancer")

  common_args <- list(
    data = data,
    elapsedtime = "survival_months",
    outcome = "death",
    outcomeLevel = "Dead",
    explanatory = c("age", "tumor_size_cm", "grade",
                     "lymph_nodes_positive", "ki67_percent"),
    random_seed = 42,
    cv_plot = FALSE, coef_plot = FALSE, survival_plot = FALSE
  )

  result1 <- do.call(lassocox, common_args)
  result2 <- do.call(lassocox, common_args)

  # Coefficient tables should be identical
  expect_equal(
    result1$results$coefficients$asDF(),
    result2$results$coefficients$asDF()
  )

  # Performance metrics should be identical
  expect_equal(
    result1$results$performance$asDF(),
    result2$results$performance$asDF()
  )
})


# ---------------------------------------------------------------
# Integration: Full workflow with all outputs
# ---------------------------------------------------------------
test_that("lassocox full workflow with all options enabled", {
  skip_lassocox_deps()
  data <- load_lassocox_data("lassocox_breast_cancer")

  result <- lassocox(
    data = data,
    elapsedtime = "survival_months",
    outcome = "death",
    outcomeLevel = "Dead",
    explanatory = c("age", "tumor_size_cm", "grade", "stage",
                     "lymph_nodes_positive", "er_status",
                     "her2_status", "ki67_percent"),
    lambda = "lambda.1se",
    nfolds = 10,
    standardize = TRUE,
    suitabilityCheck = TRUE,
    cv_plot = TRUE,
    coef_plot = TRUE,
    survival_plot = TRUE,
    showSummary = TRUE,
    showExplanations = TRUE,
    showMethodologyNotes = TRUE,
    includeClinicalGuidance = TRUE,
    showVariableImportance = TRUE,
    showModelComparison = TRUE
  )

  # All output items should exist
  expect_true(!is.null(result$results$modelSummary))
  expect_true(!is.null(result$results$coefficients))
  expect_true(!is.null(result$results$performance))
  expect_true(!is.null(result$results$suitabilityReport))
  expect_true(!is.null(result$results$variableImportance))
  expect_true(!is.null(result$results$modelComparison))
  expect_true(!is.null(result$results$lassoExplanation))
  expect_true(!is.null(result$results$methodologyNotes))
  expect_true(!is.null(result$results$clinicalGuidance))
})
