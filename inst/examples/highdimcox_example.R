# ===============================================================
# Example Usage: highdimcox (High-Dimensional Cox Regression)
# ===============================================================
#
# This example demonstrates High-Dimensional Cox regression
# using glmnet-based regularization for survival analysis with
# many predictors (genes, proteins, clinical features).

library(ClinicoPath)

# ---------------------------------------------------------------
# Example 1: Genomic survival analysis with Elastic Net
# ---------------------------------------------------------------

data(highdimcox_genomic)

# Select gene expression predictors (100 genes)
gene_vars <- paste0("GENE_", sprintf("%03d", 1:100))

result_genomic <- highdimcox(
  data = highdimcox_genomic,
  elapsedtime = "survival_months",
  outcome = "vital_status",
  outcomeLevel = "Dead",
  censorLevel = "Alive",
  predictors = gene_vars,
  regularization_method = "elastic_net",
  alpha_value = 0.5,
  cv_method = "cv_1se",
  cv_folds = 10,
  show_coefficients_table = TRUE,
  show_variable_importance = TRUE,
  show_cv_plot = TRUE,
  suitabilityCheck = TRUE
)

# ---------------------------------------------------------------
# Example 2: Proteomic data with LASSO + stability selection
# ---------------------------------------------------------------

data(highdimcox_proteomic)

protein_vars <- paste0("PROT_", sprintf("%02d", 1:50))

result_proteomic <- highdimcox(
  data = highdimcox_proteomic,
  elapsedtime = "follow_up_months",
  outcome = "event_status",
  outcomeLevel = "Dead",
  censorLevel = "Alive",
  predictors = protein_vars,
  regularization_method = "lasso",
  stability_selection = TRUE,
  subsampling_iterations = 200,
  stability_threshold = 0.7,
  show_coefficients_table = TRUE,
  showSummaries = TRUE
)

# ---------------------------------------------------------------
# Example 3: Mixed clinical + molecular with Adaptive LASSO
# ---------------------------------------------------------------

mixed_vars <- c("age", "gender", "stage", "grade", "treatment",
                paste0("GENE_", sprintf("%03d", 1:30)))

result_mixed <- highdimcox(
  data = highdimcox_genomic,
  elapsedtime = "survival_months",
  outcome = "vital_status",
  outcomeLevel = "Dead",
  censorLevel = "Alive",
  predictors = mixed_vars,
  regularization_method = "adaptive_lasso",
  show_regularization_path = TRUE,
  show_model_diagnostics = TRUE,
  showExplanations = TRUE,
  suitabilityCheck = TRUE
)
