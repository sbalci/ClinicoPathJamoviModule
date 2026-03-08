# Testing Guide: ncvregcox (SCAD/MCP Cox Regression)

## Overview

This guide covers systematic testing of the `ncvregcox` function, which performs penalized Cox regression using SCAD, MCP, or Lasso penalties for variable selection in survival data. Every option from `ncvregcox.a.yaml` is covered with specific test scenarios.

## Test Datasets

Two datasets are provided in `data/`:

| Dataset | n | Covariates | True Effects | Purpose |
|---------|---|------------|--------------|---------|
| `ncvregcox_clinical` | 200 | 14 (10 continuous + 4 categorical) | 5-6 | Realistic clinical study with mixed types and moderate correlations |
| `ncvregcox_sparse` | 100 | 30 (all continuous) | 4 | High-dimensional sparse scenario for penalty comparison |

### ncvregcox_clinical Variable Map

| Variable | Type | True Effect | Notes |
|----------|------|-------------|-------|
| `time` | numeric | -- | Survival time in months |
| `event` | factor (0/1) | -- | Event indicator |
| `age` | numeric | Weak (+) | Mean 62, SD 11 |
| `bmi` | numeric | None (noise) | Mean 26.5 |
| `tumor_diameter` | numeric | Strong (+) | Log-normal, correlated with LDH |
| `ldh_level` | numeric | Moderate (+) | Mean 220, correlated with tumor_diameter |
| `crp` | numeric | None (noise) | Log-normal, correlated with WBC |
| `albumin` | numeric | None (noise) | Mean 3.8 |
| `cea_level` | numeric | Weak-Moderate (+) | Log-normal |
| `wbc_count` | numeric | None (noise) | Mean 7.5 |
| `neutrophil_ratio` | numeric | None (noise) | Mean 60% |
| `platelet_count` | numeric | None (noise) | Mean 250 |
| `gender` | factor (M/F) | None (noise) | 55% male |
| `t_stage` | factor (T1-T4) | Strong (T3, T4) | Ordinal categories |
| `n_stage` | factor (N0-N2) | Strong (N2) | Ordinal categories |
| `histology` | factor (3 levels) | None (noise) | Adenocarcinoma/Squamous/Other |

### ncvregcox_sparse Variable Map

| Variable | True Coefficient | Block Correlation |
|----------|-----------------|-------------------|
| x1 | +0.8 | Block 1 (rho=0.4 with x2-x5) |
| x5 | -0.6 | Block 1 (rho=0.4 with x1-x4) |
| x12 | +0.7 | Block 3 (rho=0.2 with x11,x13-x15) |
| x20 | -0.4 | Independent |
| All others | 0 | Various |

---

## Test Scenarios

### 1. Penalty Function Comparison (SCAD vs MCP vs Lasso)

**Option:** `penalty` (List: SCAD / MCP / lasso)

Use `ncvregcox_sparse` to compare the three penalties with identical settings otherwise.

#### Test 1a: SCAD (default)
```
Dataset: ncvregcox_sparse
time: time
event: event
covariates: x1, x2, ..., x30
penalty: SCAD
gamma: 3.7
alpha: 1.0
cv_folds: 10
lambda_type: min
standardize: true
```
**Expected:** Should select x1, x5, x12, x20. May include 1-2 correlated noise variables from Block 1.

#### Test 1b: MCP
```
Same as 1a, but penalty: MCP, gamma: 3.0
```
**Expected:** MCP with default gamma=3.0 tends to produce sparser solutions than SCAD. Should select the 4 true variables with high probability.

#### Test 1c: Lasso
```
Same as 1a, but penalty: lasso
```
**Expected:** Lasso may over-select variables from correlated Block 1 (x2, x3, x4 alongside x1). This demonstrates why non-convex penalties are preferred.

**Comparison check:** The Variable Importance table and Selected Variables table should show differences across penalties. SCAD/MCP coefficient estimates for true variables should be closer to the true values (0.8, -0.6, 0.7, -0.4) than Lasso estimates, which are biased toward zero.

---

### 2. Gamma Parameter Variations

**Option:** `gamma` (Number: 1.1-10.0, default 3.7)

The gamma parameter controls the concavity of SCAD and MCP penalties.

#### Test 2a: Low gamma (aggressive penalization)
```
Dataset: ncvregcox_sparse
penalty: SCAD
gamma: 2.0
```
**Expected:** Stronger penalization of medium-sized coefficients. May drop x20 (weakest true effect at -0.4) while retaining x1, x5, x12.

#### Test 2b: Default gamma
```
penalty: SCAD
gamma: 3.7
```
**Expected:** Balanced variable selection. The recommended default for SCAD (Fan & Li, 2001).

#### Test 2c: High gamma (approaches Lasso behavior)
```
penalty: SCAD
gamma: 8.0
```
**Expected:** As gamma increases, SCAD approaches Lasso behavior. Variable selection should look more like Test 1c.

#### Test 2d: Gamma with MCP
```
penalty: MCP
gamma: 1.5 (near minimum of 1.1)
```
**Expected:** Very aggressive MCP penalty. The sparsest model. Only the strongest effects (x1, x12) may survive.

```
penalty: MCP
gamma: 5.0
```
**Expected:** More permissive MCP. All 4 true variables should be selected.

---

### 3. Lambda Selection (min vs 1se)

**Option:** `lambda_type` (List: min / 1se)

#### Test 3a: Lambda min
```
Dataset: ncvregcox_clinical
covariates: all 14
penalty: SCAD
lambda_type: min
```
**Expected:** Selects more variables. The model that minimizes cross-validated error. Should include most/all true effects plus possibly some noise variables.

#### Test 3b: Lambda 1se (one standard error rule)
```
Same as 3a, but lambda_type: 1se
```
**Expected:** Sparser model. Selects the lambda within one standard error of the minimum -- sacrifices a tiny amount of predictive accuracy for a simpler model. Should retain only the strongest effects (tumor_diameter, t_stage T3/T4, n_stage N2).

**Comparison check:** The Model Comparison table should show both lambda.min and lambda.1se models side by side with their CV errors, number of variables, and C-index.

---

### 4. Alpha (Elastic Net Mixing)

**Option:** `alpha` (Number: 0.0-1.0, default 1.0)

#### Test 4a: Pure penalty (alpha = 1.0, default)
```
Dataset: ncvregcox_sparse
penalty: SCAD
alpha: 1.0
```
**Expected:** Pure SCAD penalty. Standard variable selection behavior.

#### Test 4b: Elastic net mixing (alpha = 0.5)
```
penalty: SCAD
alpha: 0.5
```
**Expected:** Blend of SCAD and ridge. Should handle correlated variables (Block 1) better by grouping them rather than selecting one arbitrarily. May select more variables from Block 1 but with smaller coefficients.

#### Test 4c: Near-ridge (alpha = 0.1)
```
penalty: SCAD
alpha: 0.1
```
**Expected:** Almost pure ridge penalty. Very little variable selection -- most coefficients will be non-zero but shrunken. The Selected Variables table should show nearly all 30 variables.

#### Test 4d: Alpha with clinical data
```
Dataset: ncvregcox_clinical
penalty: SCAD
alpha: 0.7
```
**Expected:** With clinical data containing both continuous and categorical variables, alpha < 1 may help with the dummy-variable expansion of categorical predictors by grouping related dummies.

---

### 5. Standardization On/Off

**Option:** `standardize` (Bool, default true)

#### Test 5a: Standardize = true (default)
```
Dataset: ncvregcox_clinical
standardize: true
```
**Expected:** Variables on different scales (age ~62, ldh_level ~220, crp ~5, platelet_count ~250) are standardized before penalty is applied. The regularization path plot should show coefficients entering the model at roughly similar lambda values regardless of original scale.

#### Test 5b: Standardize = false
```
Dataset: ncvregcox_clinical
standardize: false
```
**Expected:** Penalty is applied to raw coefficients. Variables with large scales (ldh_level, platelet_count) will have artificially small coefficients and may be penalized less. Variable selection results may differ substantially from the standardized case. This is generally not recommended for clinical data.

**Warning:** Without standardization, the gamma parameter interpretation changes because the penalty operates on raw-scale coefficients.

---

### 6. Plot Output

**Options:** `plot_path` (Bool), `plot_cv` (Bool)

#### Test 6a: All plots enabled
```
Dataset: ncvregcox_clinical
plot_path: true
plot_cv: true
variable_importance: true
```
**Expected:**
- **Regularization Path:** Coefficient paths as a function of log(lambda). Each line = one covariate. Lines entering from the right and stabilizing show selected variables.
- **Cross-Validation Error:** CV error vs log(lambda) with error bars. Vertical lines at lambda.min and lambda.1se.
- **Variable Selection Stability:** Bar plot showing importance scores for selected variables.
- **Coefficient Comparison:** Always visible. Shows coefficient magnitudes for the final model.

#### Test 6b: Plots disabled
```
plot_path: false
plot_cv: false
variable_importance: false
```
**Expected:** Only tables and HTML interpretation should appear. No images rendered. The regularization_path, cv_error_plot, and variable_selection_plot items should be hidden.

#### Test 6c: Selective plots
```
plot_path: true
plot_cv: false
variable_importance: true
```
**Expected:** Regularization path and variable importance plots visible. CV error plot hidden.

---

### 7. Variable Importance Analysis

**Option:** `variable_importance` (Bool, default true)

#### Test 7a: Enabled
```
Dataset: ncvregcox_clinical
variable_importance: true
```
**Expected:** Variable Importance table appears with columns: Variable, Importance Score, Rank, Relative Importance (%). Only variables with non-zero coefficients should be listed. The variable_selection_plot image should also be visible.

#### Test 7b: Disabled
```
variable_importance: false
```
**Expected:** Variable Importance table and Variable Selection Stability plot are hidden.

---

### 8. Cross-Validation Folds

**Option:** `cv_folds` (Number: 3-20, default 10)

#### Test 8a: Default (10-fold)
```
Dataset: ncvregcox_clinical
cv_folds: 10
```
**Expected:** Standard 10-fold CV. Cross-Validation Results table shows lambda values with CV errors and standard errors.

#### Test 8b: Minimal folds (3-fold)
```
cv_folds: 3
```
**Expected:** Faster but noisier CV estimates. Larger standard errors in the CV Results table. The selected lambda may differ from 10-fold.

#### Test 8c: Leave-one-out-like (20-fold)
```
cv_folds: 20
```
**Expected:** More stable CV estimates but slower. Smaller standard errors. Results should be similar to 10-fold for n=200.

#### Test 8d: Small sample + many folds
```
Dataset: ncvregcox_sparse (n=100)
cv_folds: 15
```
**Expected:** Each fold has only ~6-7 observations. Should still run but with high variance in CV estimates. Warning might be appropriate if folds are too small.

---

### 9. Edge Cases

#### Test 9a: All noise variables
```
Dataset: ncvregcox_clinical
covariates: bmi, crp, albumin, wbc_count, neutrophil_ratio, platelet_count, gender, histology
(only noise variables)
```
**Expected:** With lambda.1se, the model should select zero or very few variables. The Selected Variables table should be empty or near-empty. Model interpretation should note no variables were selected.

#### Test 9b: Single covariate
```
covariates: tumor_diameter
```
**Expected:** Should run without error. The penalty effectively becomes a univariate shrinkage estimator. The regularization path has only one line.

#### Test 9c: All categorical covariates
```
covariates: gender, t_stage, n_stage, histology
```
**Expected:** The design matrix will contain dummy variables. ncvreg handles factor expansion internally. Check that the Selected Variables table reports meaningful variable names (not just dummy codes).

#### Test 9d: Missing data
```
Dataset: ncvregcox_clinical (has 6 NA values)
covariates: all 14
```
**Expected:** Rows with NA are dropped (complete-case analysis). The model should note how many observations were used (approximately 194-200). No errors from missing data.

#### Test 9e: Very high gamma
```
penalty: SCAD
gamma: 10.0
```
**Expected:** SCAD penalty approaches Lasso. Variable selection results should resemble the Lasso test.

#### Test 9f: Very low gamma (near boundary)
```
penalty: MCP
gamma: 1.1
```
**Expected:** Most aggressive MCP penalty. Very few or zero variables selected. Should not crash.

---

## Complete Option Coverage Checklist

| Option | YAML Name | Type | Default | Tested In |
|--------|-----------|------|---------|-----------|
| Time Variable | `time` | Variable (numeric) | -- | All tests |
| Event Variable | `event` | Variable (factor) | -- | All tests |
| Covariates | `covariates` | Variables (numeric/factor) | -- | All tests |
| Penalty Function | `penalty` | List (SCAD/MCP/lasso) | SCAD | Tests 1a-1c |
| SCAD/MCP Parameter | `gamma` | Number (1.1-10.0) | 3.7 | Tests 2a-2d, 9e-9f |
| Elastic Net Mixing | `alpha` | Number (0.0-1.0) | 1.0 | Tests 4a-4d |
| CV Folds | `cv_folds` | Number (3-20) | 10 | Tests 8a-8d |
| Lambda Selection | `lambda_type` | List (min/1se) | min | Tests 3a-3b |
| Standardize | `standardize` | Bool | true | Tests 5a-5b |
| Regularization Path Plot | `plot_path` | Bool | true | Tests 6a-6c |
| CV Error Plot | `plot_cv` | Bool | true | Tests 6a-6c |
| Variable Importance | `variable_importance` | Bool | true | Tests 7a-7b |

---

## Running Tests in R

### Quick smoke test with ncvregcox_clinical
```r
data(ncvregcox_clinical, package = "ClinicoPath")

ncvregcox(
  data = ncvregcox_clinical,
  time = "time",
  event = "event",
  covariates = c("age", "bmi", "tumor_diameter", "ldh_level", "crp",
                 "albumin", "cea_level", "wbc_count", "neutrophil_ratio",
                 "platelet_count", "gender", "t_stage", "n_stage", "histology"),
  penalty = "SCAD",
  gamma = 3.7,
  alpha = 1.0,
  cv_folds = 10,
  lambda_type = "min",
  standardize = TRUE,
  plot_path = TRUE,
  plot_cv = TRUE,
  variable_importance = TRUE
)
```

### Quick smoke test with ncvregcox_sparse
```r
data(ncvregcox_sparse, package = "ClinicoPath")

cov_names <- paste0("x", 1:30)

ncvregcox(
  data = ncvregcox_sparse,
  time = "time",
  event = "event",
  covariates = cov_names,
  penalty = "SCAD",
  gamma = 3.7,
  alpha = 1.0,
  cv_folds = 10,
  lambda_type = "min",
  standardize = TRUE,
  plot_path = TRUE,
  plot_cv = TRUE,
  variable_importance = TRUE
)
```

### Penalty comparison loop
```r
data(ncvregcox_sparse, package = "ClinicoPath")
cov_names <- paste0("x", 1:30)

for (pen in c("SCAD", "MCP", "lasso")) {
  cat("\n===", pen, "===\n")
  result <- ncvregcox(
    data = ncvregcox_sparse,
    time = "time",
    event = "event",
    covariates = cov_names,
    penalty = pen,
    gamma = ifelse(pen == "MCP", 3.0, 3.7),
    cv_folds = 10,
    lambda_type = "min"
  )
  cat("Selected variables:", result$model_summary$asDF$n_selected, "\n")
}
```

---

## Validation Criteria

For a correct implementation, the following should hold:

1. **SCAD/MCP should outperform Lasso** on the sparse dataset for recovering true variables (fewer false positives, less coefficient bias).
2. **Lambda.1se should give sparser models** than lambda.min across all penalty types.
3. **Gamma near the lower bound** should give the sparsest models (strongest penalization for medium coefficients).
4. **Alpha < 1** should select more correlated variables together (group effect).
5. **Standardization** should produce different variable selection when variables are on vastly different scales.
6. **All output tables** (model_summary, selected_variables, variable_importance, cross_validation_results, model_comparison, convergence_info) should populate without errors.
7. **All four plots** (regularization_path, cv_error_plot, variable_selection_plot, coefficient_comparison) should render when enabled.
8. **Edge cases** (single covariate, all noise, all categorical) should not crash.
