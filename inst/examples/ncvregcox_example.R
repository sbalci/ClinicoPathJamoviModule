# ═══════════════════════════════════════════════════════════
# Example Usage: ncvregcox (SCAD/MCP Cox Regression)
# ═══════════════════════════════════════════════════════════
#
# SCAD/MCP penalized Cox regression for high-dimensional
# survival data. Provides automatic variable selection with
# oracle properties, avoiding LASSO's shrinkage bias.
#
# Requires: ncvreg, survival packages

library(ClinicoPath)

# Load test datasets
data(ncvregcox_clinical)
data(ncvregcox_sparse)


# ── Example 1: Basic SCAD Cox regression (clinical data) ──

ncvregcox(
  data = ncvregcox_clinical,
  time = "time",
  event = "event",
  outcomeLevel = "1",
  censorLevel = "0",
  covariates = c("age", "tumor_diameter", "ldh_level", "cea_level",
                 "bmi", "crp", "t_stage", "n_stage"),
  penalty = "SCAD",
  cv_folds = 10,
  lambda_type = "min",
  gamma = 3.7,
  alpha = 1.0,
  standardize = TRUE,
  plot_path = TRUE,
  plot_cv = TRUE,
  variable_importance = TRUE,
  suitabilityCheck = TRUE
)


# ── Example 2: MCP penalty with 1-SE rule ──

ncvregcox(
  data = ncvregcox_clinical,
  time = "time",
  event = "event",
  outcomeLevel = "1",
  censorLevel = "0",
  covariates = c("age", "tumor_diameter", "ldh_level", "cea_level",
                 "bmi", "crp", "albumin", "wbc_count",
                 "gender", "t_stage", "n_stage", "histology"),
  penalty = "MCP",
  cv_folds = 10,
  lambda_type = "1se",
  gamma = 3.0,
  alpha = 1.0,
  standardize = TRUE,
  plot_path = TRUE,
  plot_cv = TRUE,
  variable_importance = TRUE,
  suitabilityCheck = TRUE
)


# ── Example 3: High-dimensional sparse data (p=30, n=100) ──

ncvregcox(
  data = ncvregcox_sparse,
  time = "time",
  event = "event",
  outcomeLevel = "1",
  censorLevel = "0",
  covariates = paste0("x", 1:30),
  penalty = "SCAD",
  cv_folds = 5,
  lambda_type = "min",
  gamma = 3.7,
  alpha = 1.0,
  standardize = TRUE,
  plot_path = TRUE,
  plot_cv = TRUE,
  variable_importance = TRUE,
  suitabilityCheck = TRUE
)


# ── Example 4: Elastic net mixing (alpha < 1) ──
# Useful when covariates are highly correlated

ncvregcox(
  data = ncvregcox_sparse,
  time = "time",
  event = "event",
  outcomeLevel = "1",
  censorLevel = "0",
  covariates = paste0("x", 1:30),
  penalty = "SCAD",
  cv_folds = 5,
  lambda_type = "min",
  gamma = 3.7,
  alpha = 0.5,
  standardize = TRUE,
  plot_path = TRUE,
  plot_cv = TRUE,
  variable_importance = TRUE,
  suitabilityCheck = FALSE
)
