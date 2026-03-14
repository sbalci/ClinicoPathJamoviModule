# ═══════════════════════════════════════════════════════════
# Example Usage: plscox (PLS Cox Regression)
# ═══════════════════════════════════════════════════════════
#
# PLS Cox regression for high-dimensional survival data.
# Designed for jamovi GUI; R syntax shown for reference.

devtools::load_all(".")

# --- Example 1: Metabolomics data (n=120, p=80) ---
data(plscox_metabolomics)
metab_cols <- grep("^METAB_", names(plscox_metabolomics), value = TRUE)

plscox(
  data = plscox_metabolomics,
  time = "survival_months",
  status = "death",
  outcomeLevel = "Dead",
  censorLevel = "Alive",
  predictors = metab_cols,
  pls_components = 5,
  cross_validation = "k10",
  component_selection = "cv_loglik",
  scaling_method = "standardize",
  risk_groups = 3,
  feature_importance = TRUE,
  prediction_accuracy = TRUE,
  confidence_intervals = TRUE,
  plot_components = TRUE,
  plot_loadings = TRUE,
  plot_scores = TRUE,
  plot_validation = TRUE,
  plot_survival = TRUE
)

# --- Example 2: Small dataset with LOO CV ---
data(plscox_small)
marker_cols <- grep("^MARKER_", names(plscox_small), value = TRUE)

plscox(
  data = plscox_small,
  time = "time_months",
  status = "status",
  outcomeLevel = "Dead",
  censorLevel = "Alive",
  predictors = marker_cols,
  pls_components = 3,
  cross_validation = "loo",
  component_selection = "cv_loglik",
  scaling_method = "standardize",
  risk_groups = 2,
  feature_importance = TRUE,
  prediction_accuracy = TRUE,
  confidence_intervals = TRUE
)

# --- Example 3: Genomic p>>n with sparse PLS ---
data(plscox_genomic)
gene_cols <- grep("^GENE_", names(plscox_genomic), value = TRUE)

plscox(
  data = plscox_genomic,
  time = "os_time",
  status = "os_event",
  outcomeLevel = "1",
  censorLevel = "0",
  predictors = gene_cols,
  pls_components = 3,
  cross_validation = "k5",
  component_selection = "cv_loglik",
  scaling_method = "standardize",
  sparse_pls = TRUE,
  limQ2set = 0.0975,
  risk_groups = 3,
  feature_importance = TRUE,
  prediction_accuracy = TRUE
)

# --- Example 4: AIC/BIC component selection (no CV) ---
plscox(
  data = plscox_small,
  time = "time_months",
  status = "status",
  outcomeLevel = "Dead",
  censorLevel = "Alive",
  predictors = marker_cols,
  pls_components = 5,
  cross_validation = "none",
  component_selection = "bic",
  scaling_method = "standardize",
  tie_method = "efron",
  risk_groups = 3,
  feature_importance = TRUE,
  prediction_accuracy = TRUE,
  confidence_intervals = TRUE
)

# --- Example 5: Bootstrap validation ---
plscox(
  data = plscox_small,
  time = "time_months",
  status = "status",
  outcomeLevel = "Dead",
  censorLevel = "Alive",
  predictors = marker_cols,
  pls_components = 2,
  cross_validation = "none",
  component_selection = "manual",
  scaling_method = "standardize",
  bootstrap_validation = TRUE,
  n_bootstrap = 100,
  risk_groups = 2,
  feature_importance = TRUE,
  prediction_accuracy = TRUE
)
