# Testing PLS-Cox Regression Function

All test datasets are in `data/` (RDA) and `data-raw/` (CSV). Synthetic data can be regenerated via `data-raw/create_plscox_test_data.R`.

## Test Datasets

| Dataset | File | N | Events | Predictors | Time Var | Status Var | Predictor Pattern | Key Properties |
|---|---|---|---|---|---|---|---|---|
| Metabolomics | `data/plscox_metabolomics.rda` | 120 | ~50% | 80 metabolites + 3 clinical | `survival_months` | `death` (Alive/Dead) | `METAB_001`..`METAB_080`, `age`, `gender`, `bmi` | Block-correlated, 3 latent pathways |
| Small | `data/plscox_small.rda` | 50 | ~60% | 25 biomarkers | `time_months` | `status` (Alive/Dead) | `MARKER_01`..`MARKER_25` | Small sample, 2 correlated blocks |
| Genomic | `data/plscox_genomic.rda` | 60 | ~55% | 200 genes | `os_time` | `os_event` (numeric 0/1) | `GENE_001`..`GENE_200` | True p>>n, near-zero-variance genes, ~3% missing, numeric status |

---

## 1. DEFAULT SETTINGS (Baseline)

| # | Data | Variables | Options to Test |
|---|------|-----------|-----------------|
| 1 | `plscox_metabolomics` (n=120) | time: `survival_months`, status: `death`, outcomeLevel: `Dead`, censorLevel: `Alive`, predictors: all `METAB_*` + `age`, `gender`, `bmi` | All defaults: `pls_components`: 5, `cross_validation`: k10, `component_selection`: cv_loglik, `scaling_method`: standardize, `suitabilityCheck`: true. Verify all 7 always-visible outputs populate (todo hidden, suitabilityReport, modelSummary, componentSelection, modelCoefficients, riskStratification, clinicalGuidance, technicalNotes). Verify all 5 plots render. Verify variableLoadings and modelPerformance appear (defaults: true). |

**Options covered:** `time`, `status`, `outcomeLevel`, `censorLevel`, `predictors`, `pls_components`, `cross_validation`, `component_selection`, `scaling_method`, `suitabilityCheck`, `confidence_intervals`, `feature_importance`, `prediction_accuracy`, all plot toggles (defaults: true)

---

## 2. COMPONENT SELECTION METHODS

| # | Data | Options to Test |
|---|------|-----------------|
| 2 | `plscox_metabolomics` | `component_selection`: `cv_loglik` (default). Cross-validated log-likelihood. Requires `cross_validation` != `none`. Verify componentSelection table shows CV scores for each number of components with "Yes" marking the selected row. |
| 3 | `plscox_metabolomics` | `component_selection`: `cv_cindex`. Cross-validated C-index. Passes `allCVcrit=TRUE` to `cv.plsRcox()`. Verify C-Index column in componentSelection table. |
| 4 | `plscox_metabolomics` | `component_selection`: `bic`. Bayesian Information Criterion. Does NOT require CV (works with `cross_validation`: `none`). Should favor more parsimonious models (fewer components) than CV methods. Verify cv_score column shows BIC values. |
| 5 | `plscox_metabolomics` | `component_selection`: `aic`. Akaike Information Criterion. Less penalizing than BIC, may select more components. Compare with BIC result. |
| 6 | `plscox_metabolomics` | `component_selection`: `manual` with `pls_components`: 3. No automatic selection; uses exactly 3 components. Verify componentSelection table shows a note (not a data table). |
| 7 | `plscox_metabolomics` | `component_selection`: `manual` with `pls_components`: 1. Minimal model -- single component. Loadings plot should show bar chart (not scatter). |
| 8 | `plscox_metabolomics` | `component_selection`: `manual` with `pls_components`: 15. Large number -- verify model handles max_components clamping gracefully. |
| 9 | `plscox_metabolomics` | `component_selection`: `cv_loglik` with `cross_validation`: `none`. CV-based selection but CV disabled. Verify fallback to manual with warning note in modelSummary. |

**Key verification:** BIC/AIC should work even with `cross_validation`: `none`. CV-based methods with `cross_validation=none` should fall back to manual selection with a warning note.

---

## 3. CROSS-VALIDATION TYPES

| # | Data | Options to Test |
|---|------|-----------------|
| 10 | `plscox_metabolomics` | `cross_validation`: `k10` (default). 10-fold CV. Standard choice. |
| 11 | `plscox_metabolomics` | `cross_validation`: `k5`. 5-fold CV. Less computation, more bias. |
| 12 | `plscox_small` | `cross_validation`: `loo`. Leave-one-out CV. Appropriate for small n=50. Higher variance but lower bias. Verify it completes (may be slow). Variables: time: `time_months`, status: `status`, outcomeLevel: `Dead`, censorLevel: `Alive`, predictors: `MARKER_01`..`MARKER_25`. |
| 13 | `plscox_metabolomics` | `cross_validation`: `none`. No CV. Must use `component_selection`: `bic` or `aic` or `manual`. Verify validationPlot still renders if cv_results is available (from BIC/AIC). |
| 14 | `plscox_small` | `cross_validation`: `k10` on small dataset (n=50). Each fold has only 5 observations -- verify stability. |

**Key verification:** LOO should only be practical for small datasets. `none` should still allow model fitting via information criteria or manual selection. The validationPlot requires cv_results (present for BIC/AIC but not for manual with no CV).

---

## 4. SCALING METHODS

| # | Data | Options to Test |
|---|------|-----------------|
| 15 | `plscox_metabolomics` | `scaling_method`: `standardize` (default). Z-score scaling via plsRcox's `scaleX=TRUE`. |
| 16 | `plscox_metabolomics` | `scaling_method`: `unit_variance`. Scale to unit variance only (no centering). Compare loadings with standardize. |
| 17 | `plscox_metabolomics` | `scaling_method`: `minmax`. Min-max scaling to [0,1]. All predictors on same scale. |
| 18 | `plscox_metabolomics` | `scaling_method`: `none`. No scaling. Metabolites with larger variance will dominate. |
| 19 | `plscox_small` | `scaling_method`: `none` vs `standardize`. Compare variable loadings -- without scaling, high-variance markers should dominate component loadings. |

**Key verification:** Standardize delegates to plsRcox (`scaleX=TRUE`); other methods pre-scale in R then pass `scaleX=FALSE`. No scaling should produce different loadings biased toward high-variance predictors.

---

## 5. ADVANCED PLS SETTINGS

| # | Data | Options to Test |
|---|------|-----------------|
| 20 | `plscox_metabolomics` | `tolerance`: 1e-06 (default). Standard convergence. |
| 21 | `plscox_metabolomics` | `tolerance`: 1e-03 (loose). Fewer iterations, less precise. |
| 22 | `plscox_metabolomics` | `tolerance`: 1e-10 (strict). More iterations, higher precision. |
| 23 | `plscox_metabolomics` | `tie_method`: `efron` (default). Recommended for most datasets. |
| 24 | `plscox_metabolomics` | `tie_method`: `breslow`. Alternative tie handling. Compare model performance. |
| 25 | `plscox_metabolomics` | `sparse_pls`: TRUE. Enable sparse PLS. Verify model still produces scores (check for NULL scores fallback path). May select fewer effective variables. |
| 26 | `plscox_metabolomics` | `limQ2set`: 0.0975 (default). PLS stopping criterion. |
| 27 | `plscox_metabolomics` | `limQ2set`: 0.5. Higher threshold -- may stop PLS extraction earlier. |
| 28 | `plscox_metabolomics` | `pvals_expli`: TRUE, `alpha_pvals_expli`: 0.05. Enable p-value based predictor selection during PLS fitting. |
| 29 | `plscox_metabolomics` | `pvals_expli`: TRUE, `alpha_pvals_expli`: 0.001. Very strict p-value threshold -- may exclude most predictors from components. |

**Key verification:** Sparse PLS may produce NULL scores via the `$tt` field; the backend falls back to `$variatesX` then `$scores`. If all are NULL, an error message should appear. The tolerance parameter is passed as `tol_Xi` to plsRcox.

---

## 6. BOOTSTRAP VALIDATION

| # | Data | Options to Test |
|---|------|-----------------|
| 30 | `plscox_metabolomics` | `bootstrap_validation`: TRUE, `n_bootstrap`: 50 (minimum). Verify bootstrapResults HTML appears with apparent C-index, mean optimism, corrected C-index, and 95% CI. |
| 31 | `plscox_metabolomics` | `bootstrap_validation`: TRUE, `n_bootstrap`: 200 (default). Standard bootstrap. Corrected C-index should be lower than apparent. |
| 32 | `plscox_small` | `bootstrap_validation`: TRUE, `n_bootstrap`: 100. Small dataset -- expect wider CIs and more optimism correction. Variables: time: `time_months`, status: `status`, outcomeLevel: `Dead`, censorLevel: `Alive`. |
| 33 | `plscox_metabolomics` | `bootstrap_validation`: FALSE. Verify bootstrapResults HTML is hidden. |

**Key verification:** Bootstrap uses Harrell's optimism-corrected method. Optimism = boot apparent C - boot test C. Corrected C = apparent C - mean optimism. Bootstrap should not change primary model results. Uses `concordance(reverse=TRUE)` because higher LP = worse prognosis.

---

## 7. PERMUTATION TEST

| # | Data | Options to Test |
|---|------|-----------------|
| 34 | `plscox_metabolomics` | `permutation_test`: TRUE, `n_permutations`: 50 (minimum). Verify permutationResults HTML shows original C-index, mean permuted C-index (~0.5), and p-value. |
| 35 | `plscox_metabolomics` | `permutation_test`: TRUE, `n_permutations`: 200. More permutations for stable p-value. |
| 36 | `plscox_small` | `permutation_test`: TRUE, `n_permutations`: 100. Small dataset. Variables: time: `time_months`, status: `status`. |
| 37 | `plscox_metabolomics` | `permutation_test`: FALSE. Verify permutationResults is hidden. |

**Key verification:** With true signal in the data, permutation p-value should be significant (p < 0.05). P-value formula: (n_better + 1) / (valid_perms + 1).

---

## 8. PLOT TOGGLE TESTING

| # | Data | Options to Test |
|---|------|-----------------|
| 38 | `plscox_metabolomics` | All plots TRUE (default). Verify all 5 plots render: componentPlot, loadingsPlot, scoresPlot, validationPlot, survivalPlot. |
| 39 | `plscox_metabolomics` | `plot_components`: FALSE. Verify componentPlot is hidden, other plots still render. |
| 40 | `plscox_metabolomics` | `plot_loadings`: FALSE. Verify loadingsPlot is hidden. |
| 41 | `plscox_metabolomics` | `plot_scores`: FALSE. Verify scoresPlot is hidden. |
| 42 | `plscox_metabolomics` | `plot_validation`: FALSE. Verify validationPlot is hidden. |
| 43 | `plscox_metabolomics` | `plot_survival`: FALSE. Verify survivalPlot is hidden. |
| 44 | `plscox_metabolomics` | ALL plots FALSE. Verify no plots render but tables still populate. |

**Key verification:** Each plot should be independently controllable. No protobuf serialization errors. State should contain only plain data (no model objects with function references).

**State management checks:**
- All 5 plots share the same plot_state object stored via `setState()`
- componentPlot: uses `inf_crit` field (data.frame from `pls_model$InfCrit`)
- loadingsPlot: uses `loadings_matrix` field (data.frame from `pls_model$wwetoile`)
- scoresPlot: uses `pls_scores`, `time_var`, `status_var` fields
- validationPlot: uses `cv_results` field (data.frame, may be NULL)
- survivalPlot: uses `time_var`, `status_var`, `risk_categories` fields

---

## 9. RISK GROUP STRATIFICATION

| # | Data | Options to Test |
|---|------|-----------------|
| 45 | `plscox_metabolomics` | `risk_groups`: 2. Binary stratification (high/low risk). Verify riskStratification table has 2 rows. Survival plot shows 2 curves. HR vs low should be 1.0 for Risk Group 1. |
| 46 | `plscox_metabolomics` | `risk_groups`: 3 (default). Tertile stratification. Verify 3 rows, 3 survival curves, monotonic HR trend. |
| 47 | `plscox_metabolomics` | `risk_groups`: 4. Quartile stratification. Verify 4 rows. |
| 48 | `plscox_metabolomics` | `risk_groups`: 5. Quintile stratification. With n=120, each group has ~24 patients. |
| 49 | `plscox_small` | `risk_groups`: 2. Small dataset, binary split. Variables: time: `time_months`, status: `status`. |
| 50 | `plscox_small` | `risk_groups`: 5. Small dataset (n=50), 5 groups = ~10/group. May produce unstable HR estimates -- verify graceful handling. |

**Key verification:** Each risk group should have at least some events. Hazard ratios vs low risk group should show monotonic trend (higher risk groups have higher HR). Risk Group 1 always has HR = 1.0 (reference). Uses `quantile()` for breakpoints with `unique()` to collapse when too few unique LP values.

---

## 10. DATA SUITABILITY ASSESSMENT

| # | Data | Options to Test |
|---|------|-----------------|
| 51 | `plscox_metabolomics` | `suitabilityCheck`: TRUE (default). Verify suitabilityReport HTML appears with 6 checks: EPV, Reduction Need, Sample Size, Event Rate, Multicollinearity, Data Quality. Overall should be green/yellow. |
| 52 | `plscox_small` | `suitabilityCheck`: TRUE. Small sample. May show yellow for Sample Size. |
| 53 | `plscox_genomic` | `suitabilityCheck`: TRUE. p>>n + missing data. Variables: time: `os_time`, status: `os_event`, outcomeLevel: `1`, censorLevel: `0`. Verify yellow/red for Data Quality due to missing values. EPV should be very low (red). Reduction Need should be green. |
| 54 | `plscox_metabolomics` | `suitabilityCheck`: FALSE. Verify suitabilityReport is hidden. |

**Key verification:** Traffic-light colors follow specific thresholds. Multicollinearity check is always green or green-ish for PLS since PLS handles collinearity by design. Correlation matrix is only computed when p <= 2000.

---

## 11. FEATURE IMPORTANCE & OUTPUT OPTIONS

| # | Data | Options to Test |
|---|------|-----------------|
| 55 | `plscox_metabolomics` | `feature_importance`: TRUE (default). Verify variableLoadings table populates with all predictors (or top 100), loadings on components 1-3, and Cox-weighted importance score. Top variables should come from signal blocks (METAB_001-015, METAB_025-040, METAB_055-070). |
| 56 | `plscox_metabolomics` | `feature_importance`: FALSE. Verify variableLoadings table is hidden. Loadings plot should return FALSE (skipped). |
| 57 | `plscox_metabolomics` | `confidence_intervals`: TRUE (default). Verify modelCoefficients table shows hr_lower and hr_upper columns populated. |
| 58 | `plscox_metabolomics` | `confidence_intervals`: FALSE. Verify CI columns show NA. |
| 59 | `plscox_metabolomics` | `prediction_accuracy`: TRUE (default). Verify modelPerformance table shows 4 rows: Training Concordance Index, R-squared (Nagelkerke), AIC, BIC. |
| 60 | `plscox_metabolomics` | `prediction_accuracy`: FALSE. Verify modelPerformance table is hidden. |

---

## 12. EDGE CASES

| # | Data | Options to Test |
|---|------|-----------------|
| 61 | Synthetic (n=30, p=5) | Very small dataset with 5 continuous predictors. `pls_components`: 5 (equals p). Verify max_components is clamped to min(p-1, n-2) = 4. |
| 62 | Synthetic (n=100, p=3) | `pls_components`: 10 (exceeds p). Verify clamping to min(2, 98) = 2. |
| 63 | Synthetic (n=50, all censored) | `status` all "Alive". After encoding, all status_var = 0. n_events = 0. Verify informative error: "Fewer than 5 events observed." |
| 64 | Synthetic (n=50, 3 events) | Very few events. Verify error: "Fewer than 5 events observed." |
| 65 | Synthetic with constant column | One predictor with zero variance. Verify it is silently removed (col_vars check). If all columns constant, verify error. |
| 66 | Synthetic with NA in time | Missing values in time variable. Verify listwise deletion and note about excluded observations. |
| 67 | Synthetic with NA in predictors | 10% missing in predictor columns (use `plscox_genomic`). Verify listwise deletion and missing count note. |
| 68 | `plscox_metabolomics` | Only 2 predictors selected (age, bmi). `pls_components`: 1. Minimal PLS model. |
| 69 | `plscox_genomic` | p >> n scenario (n=60, p=200). Numeric 0/1 status. Test non-factor status encoding path. Variables: time: `os_time`, status: `os_event`, outcomeLevel: `1`, censorLevel: `0`, predictors: `GENE_001`..`GENE_200`. |
| 70 | Synthetic with negative times | Observations with time <= 0. Verify they are excluded with a note and analysis proceeds on remaining data. |
| 71 | Synthetic mismatched levels | outcomeLevel and censorLevel that don't match any values. Verify error: "No rows matched the selected Event Level or Censored Level." |

---

## 13. COMBINED OPTION STRESS TESTS

| # | Data | Options to Test |
|---|------|-----------------|
| 72 | `plscox_metabolomics` | Full analysis: `cross_validation`: k5, `component_selection`: cv_cindex, `scaling_method`: minmax, `bootstrap_validation`: TRUE, `n_bootstrap`: 100, `permutation_test`: TRUE, `n_permutations`: 50, `risk_groups`: 4, all plots TRUE, all output options TRUE, `suitabilityCheck`: TRUE. Verify everything runs without errors. |
| 73 | `plscox_small` | LOO + bootstrap + permutation: `cross_validation`: loo, `component_selection`: cv_loglik, `bootstrap_validation`: TRUE, `n_bootstrap`: 50, `permutation_test`: TRUE, `n_permutations`: 50. Stress test on small data. |
| 74 | `plscox_metabolomics` | Minimal output: all plots FALSE, `feature_importance`: FALSE, `prediction_accuracy`: FALSE, `confidence_intervals`: FALSE, `bootstrap_validation`: FALSE, `permutation_test`: FALSE, `suitabilityCheck`: FALSE. Only core model summary and coefficients. |
| 75 | `plscox_genomic` | High-dimensional: `pls_components`: 10, `component_selection`: bic, `scaling_method`: standardize, `sparse_pls`: TRUE, `feature_importance`: TRUE, `risk_groups`: 3. p=200 >> n=60. |
| 76 | `plscox_metabolomics` | Advanced PLS: `sparse_pls`: TRUE, `pvals_expli`: TRUE, `alpha_pvals_expli`: 0.01, `limQ2set`: 0.5, `tolerance`: 1e-10, `tie_method`: breslow. Verify all advanced settings are passed through correctly. |

---

## COMPLETE OPTION COVERAGE CHECKLIST

All 29 user-facing options from `.a.yaml`:

- [x] `time` -- all tests
- [x] `status` -- all tests
- [x] `outcomeLevel` -- all tests (explicit in #1, #12, #53, #69)
- [x] `censorLevel` -- all tests (explicit in #1, #12, #53, #69)
- [x] `predictors` -- all tests
- [x] `pls_components` (1, 3, 5, 10, 15) -- #1, #6, #7, #8, #61, #62, #68
- [x] `cross_validation` (loo / k10 / k5 / none) -- #10, #11, #12, #13, #73
- [x] `component_selection` (cv_loglik / cv_cindex / bic / aic / manual) -- #2, #3, #4, #5, #6, #9
- [x] `scaling_method` (standardize / unit_variance / minmax / none) -- #15, #16, #17, #18, #19
- [x] `tolerance` (1e-03, 1e-06, 1e-10) -- #20, #21, #22, #76
- [x] `tie_method` (efron / breslow) -- #23, #24, #76
- [x] `sparse_pls` (TRUE / FALSE) -- #25, #75, #76
- [x] `limQ2set` (0.0975, 0.5) -- #26, #27, #76
- [x] `pvals_expli` (TRUE / FALSE) -- #28, #29, #76
- [x] `alpha_pvals_expli` (0.05, 0.001, 0.01) -- #28, #29, #76
- [x] `bootstrap_validation` (TRUE / FALSE) -- #30, #31, #32, #33
- [x] `n_bootstrap` (50, 100, 200) -- #30, #31, #32
- [x] `permutation_test` (TRUE / FALSE) -- #34, #35, #36, #37
- [x] `n_permutations` (50, 100, 200) -- #34, #35, #36
- [x] `plot_components` (TRUE / FALSE) -- #38, #39
- [x] `plot_loadings` (TRUE / FALSE) -- #38, #40
- [x] `plot_scores` (TRUE / FALSE) -- #38, #41
- [x] `plot_validation` (TRUE / FALSE) -- #38, #42
- [x] `plot_survival` (TRUE / FALSE) -- #38, #43
- [x] `risk_groups` (2, 3, 4, 5) -- #45, #46, #47, #48, #49, #50
- [x] `confidence_intervals` (TRUE / FALSE) -- #57, #58
- [x] `feature_importance` (TRUE / FALSE) -- #55, #56
- [x] `prediction_accuracy` (TRUE / FALSE) -- #59, #60
- [x] `suitabilityCheck` (TRUE / FALSE) -- #51, #52, #53, #54

**Total test scenarios:** 76 (covering all 29 user-facing options with multiple value combinations)

---

## COMPLETE OUTPUT COVERAGE CHECKLIST

All 17 output items from `.r.yaml`:

- [x] `todo` (Html) -- #1 (hidden when variables selected)
- [x] `suitabilityReport` (Html) -- #51, #52, #53, #54
- [x] `modelSummary` (Html) -- all tests
- [x] `componentSelection` (Table) -- #2, #3, #4, #5, #6
- [x] `modelCoefficients` (Table) -- #1, #57, #58
- [x] `variableLoadings` (Table) -- #55, #56
- [x] `modelPerformance` (Table) -- #59, #60
- [x] `riskStratification` (Table) -- #45, #46, #47, #48
- [x] `componentPlot` (Image) -- #38, #39
- [x] `loadingsPlot` (Image) -- #38, #40
- [x] `scoresPlot` (Image) -- #38, #41
- [x] `validationPlot` (Image) -- #38, #42
- [x] `survivalPlot` (Image) -- #38, #43
- [x] `bootstrapResults` (Html) -- #30, #31, #32, #33
- [x] `permutationResults` (Html) -- #34, #35, #36, #37
- [x] `clinicalGuidance` (Html) -- #1
- [x] `technicalNotes` (Html) -- #1
