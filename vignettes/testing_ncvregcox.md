# Testing Guide: ncvregcox (SCAD/MCP Cox Regression)

## Overview

This guide covers systematic testing of the `ncvregcox` function, which performs penalized Cox regression using **SCAD or MCP** penalties for variable selection in survival data. Every option from `ncvregcox.a.yaml` is covered with specific test scenarios.

Note: ncvregcox does **not** support Lasso. For Lasso-based Cox regression, use the `lassocox` function instead.

## Test Datasets

Four datasets are provided in `data/`:

| Dataset | n | Covariates | True Effects | Purpose |
|---------|---|------------|--------------|---------|
| `ncvregcox_clinical` | 200 | 14 (10 continuous + 4 categorical) | 5-6 | Realistic clinical study with mixed types and moderate correlations |
| `ncvregcox_sparse` | 100 | 30 (all continuous) | 4 | High-dimensional sparse scenario for penalty comparison |
| `ncvregcox_small` | 25 | 5 | 2-3 | Small-sample edge case for stability and warning checks |
| `ncvregcox_collinear` | 150 | 10 (all continuous, r>0.9) | 3 | Highly collinear predictors for multicollinearity stress testing |

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

### ncvregcox_small Variable Map

| Variable | Type | Notes |
|----------|------|-------|
| `time` | numeric | Survival time |
| `event` | factor (0/1) | Event indicator |
| `x1`-`x5` | numeric | 5 continuous covariates, 2-3 true effects |

### ncvregcox_collinear Variable Map

| Variable | Type | Notes |
|----------|------|-------|
| `time` | numeric | Survival time |
| `event` | factor (0/1) | Event indicator |
| `x1`-`x10` | numeric | 10 continuous covariates with pairwise correlations > 0.9 |

---

## Test Scenarios

### 1. Penalty Function Comparison (SCAD vs MCP)

**Option:** `penalty` (List: SCAD / MCP)

Use `ncvregcox_sparse` to compare the two penalties with identical settings otherwise.

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

**Comparison check:** The Variable Importance table and Selected Variables table should show differences across penalties. Both SCAD and MCP coefficient estimates for true variables should be close to the true values (0.8, -0.6, 0.7, -0.4) thanks to the oracle property of non-convex penalties.

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

#### Test 2c: High gamma
```
penalty: SCAD
gamma: 8.0
```
**Expected:** As gamma increases, the SCAD penalty shape changes. Variable selection may become less aggressive on medium-sized coefficients.

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

**Option:** `alpha` (Number: 0.01-1.0, default 1.0)

Note: ncvreg requires alpha > 0, so the minimum is 0.01 (not 0.0). Pure ridge regression is not supported.

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

#### Test 4c: Near-ridge (alpha = 0.01)
```
penalty: SCAD
alpha: 0.01
```
**Expected:** Almost pure ridge penalty. Very little variable selection -- most coefficients will be non-zero but shrunken. The Selected Variables table should show nearly all 30 variables.

#### Test 4d: Alpha with clinical data
```
Dataset: ncvregcox_clinical
penalty: SCAD
alpha: 0.7
```
**Expected:** With clinical data containing both continuous and categorical variables, alpha < 1 may help with the dummy-variable expansion of categorical predictors by grouping related dummies.

#### Test 4e: Alpha at boundary (alpha = 0.01)
```
penalty: MCP
alpha: 0.01
```
**Expected:** Should run without error. ncvreg requires alpha > 0, so 0.01 is the minimum allowed value. Model should fit but produce minimal variable selection.

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

### 9. Outcome Level and Censor Level

**Options:** `outcomeLevel` (Level), `censorLevel` (Level)

These options allow explicit specification of which factor level represents the event and which represents censoring.

#### Test 9a: Default behavior (no levels specified)
```
Dataset: ncvregcox_clinical
time: time
event: event
(outcomeLevel and censorLevel left empty)
```
**Expected:** The function auto-detects levels. For a binary 0/1 factor, "1" is treated as the event and "0" as censored.

#### Test 9b: Explicit outcomeLevel only
```
Dataset: ncvregcox_clinical
time: time
event: event
outcomeLevel: "1"
```
**Expected:** Identical results to 9a. Explicitly setting the event level to "1" should match the auto-detection behavior.

#### Test 9c: Explicit both levels
```
Dataset: ncvregcox_clinical
time: time
event: event
outcomeLevel: "1"
censorLevel: "0"
```
**Expected:** Both levels explicitly set. Results should match 9a. Rows with event values matching neither level would be excluded (none in this dataset).

#### Test 9d: Reversed levels
```
Dataset: ncvregcox_clinical
time: time
event: event
outcomeLevel: "0"
censorLevel: "1"
```
**Expected:** The meaning of event and censoring is reversed. The model should still fit but coefficients will have opposite signs compared to 9a (hazard ratios become protective and vice versa). This tests that the level mapping is applied correctly.

#### Test 9e: Multi-level event variable
If a dataset has event coded as a factor with levels like "Dead", "Alive", "Lost":
```
outcomeLevel: "Dead"
censorLevel: "Alive"
```
**Expected:** Rows with "Lost" are excluded as missing. The model fits on only "Dead" (event=1) and "Alive" (event=0) rows. This is the primary use case for these options.

---

### 10. Suitability Check

**Option:** `suitabilityCheck` (Bool, default true)

The suitability assessment runs a traffic-light system of checks before the main analysis.

#### Test 10a: Suitability check enabled (default)
```
Dataset: ncvregcox_clinical
covariates: all 14
suitabilityCheck: true
```
**Expected:** The `suitabilityReport` HTML output appears. It should contain:
- Sample size adequacy check (n=200 with 14 covariates should pass)
- Events-per-variable ratio (should be adequate)
- Multicollinearity assessment (tumor_diameter and ldh_level are correlated -- may flag)
- Whether regularization is needed (with 14 covariates and n=200, regularization is recommended)

#### Test 10b: Suitability check disabled
```
suitabilityCheck: false
```
**Expected:** The `suitabilityReport` HTML output is hidden. The analysis proceeds directly to fitting without the pre-check. All other outputs should be unaffected.

#### Test 10c: Suitability with small sample
```
Dataset: ncvregcox_small (n=25)
covariates: x1, x2, x3, x4, x5
suitabilityCheck: true
```
**Expected:** The suitability report should flag concerns:
- Low sample size (n=25)
- Low events-per-variable ratio (with 5 covariates and likely ~10-12 events, EPV is around 2-3)
- Recommendation to reduce covariates or increase sample size
- Traffic light should show amber or red for sample size

#### Test 10d: Suitability with collinear data
```
Dataset: ncvregcox_collinear (n=150, r>0.9)
covariates: x1, x2, ..., x10
suitabilityCheck: true
```
**Expected:** The suitability report should flag severe multicollinearity (r>0.9 between predictors). The multicollinearity check should be red/amber. The report should recommend considering variable reduction or elastic net mixing (alpha < 1) to handle collinearity.

#### Test 10e: Suitability with sparse data
```
Dataset: ncvregcox_sparse (n=100, p=30)
covariates: x1, x2, ..., x30
suitabilityCheck: true
```
**Expected:** The suitability report should flag the high p/n ratio (30 covariates with only 100 observations). Regularization is clearly needed. Events-per-variable ratio will be low.

---

### 11. Small Sample Edge Cases

#### Test 11a: Small sample basic fit
```
Dataset: ncvregcox_small (n=25)
covariates: x1, x2, x3, x4, x5
penalty: SCAD
gamma: 3.7
cv_folds: 5
lambda_type: 1se
```
**Expected:** Should complete without error. With n=25, use fewer CV folds (5 is reasonable). The 1se rule should produce a very sparse model (0-2 variables). High variance in CV estimates is expected.

#### Test 11b: Small sample with MCP
```
Dataset: ncvregcox_small (n=25)
penalty: MCP
gamma: 3.0
cv_folds: 3
```
**Expected:** MCP with minimal folds on small data. Should still converge. The model may select 0 variables with aggressive penalization.

#### Test 11c: Small sample with too many folds
```
Dataset: ncvregcox_small (n=25)
cv_folds: 20
```
**Expected:** Each fold would have only ~1 observation. The function should either handle this gracefully (reducing folds automatically) or produce a meaningful error/warning. This tests the boundary condition where cv_folds approaches n.

---

### 12. Collinear Data Edge Cases

#### Test 12a: Collinear data with SCAD
```
Dataset: ncvregcox_collinear (n=150, 10 vars, r>0.9)
penalty: SCAD
alpha: 1.0
gamma: 3.7
```
**Expected:** With pure SCAD penalty and high collinearity, the model may arbitrarily select one variable from each collinear pair. Variable selection results may be unstable across CV folds.

#### Test 12b: Collinear data with elastic net mixing
```
Dataset: ncvregcox_collinear
penalty: SCAD
alpha: 0.3
```
**Expected:** Elastic net mixing (alpha=0.3) should handle collinearity better by grouping correlated variables. More variables should be selected compared to 12a, with smaller but non-zero coefficients distributed across collinear groups.

#### Test 12c: Collinear data with MCP
```
Dataset: ncvregcox_collinear
penalty: MCP
alpha: 1.0
gamma: 3.0
```
**Expected:** MCP is more aggressive than SCAD. With high collinearity, it may select fewer variables and produce a sparser model. Compare against 12a to see penalty differences under collinearity.

---

### 13. General Edge Cases

#### Test 13a: All noise variables
```
Dataset: ncvregcox_clinical
covariates: bmi, crp, albumin, wbc_count, neutrophil_ratio, platelet_count, gender, histology
(only noise variables)
```
**Expected:** With lambda.1se, the model should select zero or very few variables. The Selected Variables table should be empty or near-empty. Model interpretation should note no variables were selected.

#### Test 13b: Single covariate
```
covariates: tumor_diameter
```
**Expected:** Should run without error. The penalty effectively becomes a univariate shrinkage estimator. The regularization path has only one line.

#### Test 13c: All categorical covariates
```
covariates: gender, t_stage, n_stage, histology
```
**Expected:** The design matrix will contain dummy variables. ncvreg handles factor expansion internally. Check that the Selected Variables table reports meaningful variable names (not just dummy codes).

#### Test 13d: Missing data
```
Dataset: ncvregcox_clinical (has 6 NA values)
covariates: all 14
```
**Expected:** Rows with NA are dropped (complete-case analysis). The model should note how many observations were used (approximately 194-200). No errors from missing data.

#### Test 13e: Very high gamma
```
penalty: SCAD
gamma: 10.0
```
**Expected:** The SCAD penalty shape changes at high gamma. Variable selection may differ from the default.

#### Test 13f: Very low gamma (near boundary)
```
penalty: MCP
gamma: 1.1
```
**Expected:** Most aggressive MCP penalty. Very few or zero variables selected. Should not crash.

---

## Complete Option Coverage Checklist

All 15 options from `ncvregcox.a.yaml` are covered:

| Option | YAML Name | Type | Default | Range/Values | Tested In |
|--------|-----------|------|---------|--------------|-----------|
| Data | `data` | Data | -- | -- | All tests |
| Time Variable | `time` | Variable (numeric) | -- | -- | All tests |
| Event Variable | `event` | Variable (factor) | -- | -- | All tests |
| Event Level | `outcomeLevel` | Level | (auto) | Levels of event | Tests 9a-9e |
| Censored Level | `censorLevel` | Level | (auto) | Levels of event | Tests 9a-9e |
| Covariates | `covariates` | Variables (numeric/factor) | -- | -- | All tests |
| Penalty Function | `penalty` | List | SCAD | SCAD / MCP | Tests 1a-1b |
| CV Folds | `cv_folds` | Number | 10 | 3-20 | Tests 8a-8d, 11a-11c |
| Lambda Selection | `lambda_type` | List | min | min / 1se | Tests 3a-3b |
| Elastic Net Mixing | `alpha` | Number | 1.0 | 0.01-1.0 | Tests 4a-4e |
| SCAD/MCP Parameter | `gamma` | Number | 3.7 | 1.1-10.0 | Tests 2a-2d, 13e-13f |
| Standardize | `standardize` | Bool | true | true/false | Tests 5a-5b |
| Regularization Path Plot | `plot_path` | Bool | true | true/false | Tests 6a-6c |
| CV Error Plot | `plot_cv` | Bool | true | true/false | Tests 6a-6c |
| Variable Importance | `variable_importance` | Bool | true | true/false | Tests 7a-7b |
| Suitability Check | `suitabilityCheck` | Bool | true | true/false | Tests 10a-10e |

---

## Automated Test Files

Four test files exist under `tests/testthat/`:

| File | Coverage |
|------|----------|
| `test-ncvregcox-basic.R` | Core functionality: SCAD and MCP fitting, default options, output structure |
| `test-ncvregcox-arguments.R` | Systematic option variation: penalty, gamma, alpha, lambda_type, cv_folds, standardize, outcomeLevel, censorLevel, suitabilityCheck |
| `test-ncvregcox-edge-cases.R` | Boundary conditions: single covariate, all noise, all categorical, missing data, extreme gamma, small sample (ncvregcox_small), collinear data (ncvregcox_collinear) |
| `test-ncvregcox-integration.R` | End-to-end workflows: full clinical pipeline, sparse data pipeline, penalty comparison, plot toggling, suitability check integration |

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
  variable_importance = TRUE,
  suitabilityCheck = TRUE
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
  variable_importance = TRUE,
  suitabilityCheck = TRUE
)
```

### Smoke test with ncvregcox_small (edge case)
```r
data(ncvregcox_small, package = "ClinicoPath")

ncvregcox(
  data = ncvregcox_small,
  time = "time",
  event = "event",
  covariates = c("x1", "x2", "x3", "x4", "x5"),
  penalty = "SCAD",
  gamma = 3.7,
  alpha = 1.0,
  cv_folds = 5,
  lambda_type = "1se",
  standardize = TRUE,
  plot_path = TRUE,
  plot_cv = TRUE,
  variable_importance = TRUE,
  suitabilityCheck = TRUE
)
```

### Smoke test with ncvregcox_collinear (edge case)
```r
data(ncvregcox_collinear, package = "ClinicoPath")

ncvregcox(
  data = ncvregcox_collinear,
  time = "time",
  event = "event",
  covariates = paste0("x", 1:10),
  penalty = "SCAD",
  gamma = 3.7,
  alpha = 0.5,
  cv_folds = 10,
  lambda_type = "min",
  standardize = TRUE,
  plot_path = TRUE,
  plot_cv = TRUE,
  variable_importance = TRUE,
  suitabilityCheck = TRUE
)
```

### outcomeLevel / censorLevel test
```r
data(ncvregcox_clinical, package = "ClinicoPath")

ncvregcox(
  data = ncvregcox_clinical,
  time = "time",
  event = "event",
  outcomeLevel = "1",
  censorLevel = "0",
  covariates = c("age", "tumor_diameter", "t_stage"),
  penalty = "SCAD",
  cv_folds = 10,
  suitabilityCheck = TRUE
)
```

### Penalty comparison loop (SCAD vs MCP)
```r
data(ncvregcox_sparse, package = "ClinicoPath")
cov_names <- paste0("x", 1:30)

for (pen in c("SCAD", "MCP")) {
  cat("\n===", pen, "===\n")
  result <- ncvregcox(
    data = ncvregcox_sparse,
    time = "time",
    event = "event",
    covariates = cov_names,
    penalty = pen,
    gamma = ifelse(pen == "MCP", 3.0, 3.7),
    cv_folds = 10,
    lambda_type = "min",
    suitabilityCheck = FALSE
  )
  cat("Selected variables:", result$model_summary$asDF$n_selected, "\n")
}
```

---

## Validation Criteria

For a correct implementation, the following should hold:

1. **SCAD and MCP should both recover true variables** on the sparse dataset with few false positives and minimal coefficient bias, thanks to their oracle properties.
2. **Lambda.1se should give sparser models** than lambda.min across both penalty types.
3. **Gamma near the lower bound** should give the sparsest models (strongest penalization for medium coefficients).
4. **Alpha < 1** should select more correlated variables together (group effect) and help with collinear data.
5. **Alpha minimum is 0.01** (not 0.0) because ncvreg requires alpha > 0.
6. **Standardization** should produce different variable selection when variables are on vastly different scales.
7. **outcomeLevel/censorLevel** should correctly remap event factor levels and exclude rows that match neither level.
8. **suitabilityCheck** should produce a traffic-light HTML report flagging sample size, EPV, multicollinearity, and regularization need.
9. **Small sample data (n=25)** should complete without crash; suitability check should flag concerns.
10. **Collinear data (r>0.9)** should be handled; suitability check should flag multicollinearity; elastic net mixing should help.
11. **All output tables** (model_summary, selected_variables, variable_importance, cross_validation_results, model_comparison, convergence_info) should populate without errors.
12. **All plots** (regularization_path, cv_error_plot, variable_selection_plot) should render when enabled and hide when disabled.
13. **Edge cases** (single covariate, all noise, all categorical) should not crash.
