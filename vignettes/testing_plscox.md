# Testing PLS-Cox Regression Function

All test datasets are in `data-raw/` (CSV) or `data/` (RDA/OMV). Synthetic data can be generated via `data-raw/create_plscox_test_data.R`.

---

## 1. DEFAULT SETTINGS (Baseline)

| # | Data | Variables | Options to Test |
|---|------|-----------|-----------------|
| 1 | `plscox_metabolomics` (n=120) | time: `survival_months`, status: `death`, predictors: all `METAB_*` columns + `age`, `gender`, `bmi` | All defaults: `pls_components`: 5, `pls_algorithm`: nipals, `cross_validation`: k10, `component_selection`: cv_loglik, `scaling_method`: standardize. Verify all tables populate (modelSummary, componentSelection, modelCoefficients, variableLoadings, modelPerformance, riskStratification). Verify all 5 plots render. |

**Options covered:** `time`, `status`, `predictors`, `pls_components`, `pls_algorithm`, `cross_validation`, `component_selection`, `scaling_method`, `confidence_intervals`, `feature_importance`, `prediction_accuracy`, all plot toggles (defaults: true)

---

## 2. PLS ALGORITHM COMPARISON

| # | Data | Options to Test |
|---|------|-----------------|
| 2 | `plscox_metabolomics` | `pls_algorithm`: `nipals` (default). NIPALS is the iterative algorithm; verify convergence with default `max_iterations`: 100, `tolerance`: 1e-06. |
| 3 | `plscox_metabolomics` | `pls_algorithm`: `kernel`. Kernel PLS is faster for wide data. Compare component loadings and coefficients with NIPALS -- should be numerically close. |
| 4 | `plscox_metabolomics` | `pls_algorithm`: `widekernelpls`. Wide Kernel PLS is optimized for p >> n. Compare results with kernel. |
| 5 | `plscox_small` | Run all 3 algorithms on small dataset. Verify all converge and produce similar component structures. |

**Key verification:** All 3 algorithms should produce similar (not necessarily identical) model summaries, component loadings, and risk stratifications. Any numerical differences should be small. NIPALS may require more iterations than kernel methods.

---

## 3. COMPONENT SELECTION METHODS

| # | Data | Options to Test |
|---|------|-----------------|
| 6 | `plscox_metabolomics` | `component_selection`: `cv_loglik` (default). Cross-validated log-likelihood. Requires `cross_validation` != `none`. Verify componentSelection table shows CV scores for each number of components. |
| 7 | `plscox_metabolomics` | `component_selection`: `cv_cindex`. Cross-validated C-index. Should select components maximizing concordance. Verify C-Index column in componentSelection table. |
| 8 | `plscox_metabolomics` | `component_selection`: `bic`. Bayesian Information Criterion. Does NOT require CV (works with `cross_validation`: `none`). Should favor more parsimonious models (fewer components) than CV methods. |
| 9 | `plscox_metabolomics` | `component_selection`: `aic`. Akaike Information Criterion. Less penalizing than BIC, may select more components. Compare with BIC result. |
| 10 | `plscox_metabolomics` | `component_selection`: `manual` with `pls_components`: 3. No automatic selection; uses exactly the specified number of components. Verify componentSelection table shows only 3 components. |
| 11 | `plscox_metabolomics` | `component_selection`: `manual` with `pls_components`: 1. Minimal model -- single component. |
| 12 | `plscox_metabolomics` | `component_selection`: `manual` with `pls_components`: 15. Large number -- verify model handles overfitting gracefully. |

**Key verification:** BIC/AIC should work even with `cross_validation`: `none`. CV-based methods should require `cross_validation` != `none` (expect warning or fallback if `none` selected with cv_loglik/cv_cindex).

---

## 4. CROSS-VALIDATION TYPES

| # | Data | Options to Test |
|---|------|-----------------|
| 13 | `plscox_metabolomics` | `cross_validation`: `k10` (default). 10-fold CV. Standard choice. |
| 14 | `plscox_metabolomics` | `cross_validation`: `k5`. 5-fold CV. Less computation, more bias. |
| 15 | `plscox_small` | `cross_validation`: `loo`. Leave-one-out CV. Appropriate for small n=50. Higher variance but lower bias. Verify it completes (may be slow). |
| 16 | `plscox_metabolomics` | `cross_validation`: `none`. No CV. Must use `component_selection`: `bic` or `aic` or `manual`. Verify validationPlot is suppressed or shows appropriate message. |
| 17 | `plscox_small` | `cross_validation`: `k10` on small dataset (n=50). Each fold has only 5 observations -- verify stability and that folds are handled correctly. |

**Key verification:** LOO should only be practical for small datasets. `none` should still allow model fitting via information criteria or manual selection. The validationPlot should reflect the chosen CV method.

---

## 5. SCALING METHODS

| # | Data | Options to Test |
|---|------|-----------------|
| 18 | `plscox_metabolomics` | `scaling_method`: `standardize` (default). Z-score scaling. Standard for PLS. |
| 19 | `plscox_metabolomics` | `scaling_method`: `unit_variance`. Scale to unit variance only (no centering). Compare loadings with standardize. |
| 20 | `plscox_metabolomics` | `scaling_method`: `minmax`. Min-max scaling to [0,1]. All predictors on same scale. Compare component structure. |
| 21 | `plscox_metabolomics` | `scaling_method`: `none`. No scaling. Metabolites with larger variance will dominate. Loadings should reflect raw variable scales. |
| 22 | `plscox_small` | `scaling_method`: `none` vs `standardize`. Compare variable loadings -- without scaling, high-variance markers should dominate. |

**Key verification:** Standardize and unit_variance should give similar results. No scaling should produce different loadings biased toward high-variance predictors. MinMax should be between the two extremes.

---

## 6. ALGORITHM CONVERGENCE PARAMETERS

| # | Data | Options to Test |
|---|------|-----------------|
| 23 | `plscox_metabolomics` | `max_iterations`: 50 (minimum). With NIPALS algorithm. Verify convergence warning if algorithm doesn't converge. |
| 24 | `plscox_metabolomics` | `max_iterations`: 1000 (maximum). Should always converge. |
| 25 | `plscox_metabolomics` | `tolerance`: 1e-03 (loose). Fewer iterations needed but less precise. |
| 26 | `plscox_metabolomics` | `tolerance`: 1e-10 (strict). More iterations needed, higher precision. |
| 27 | `plscox_metabolomics` | `max_iterations`: 50, `tolerance`: 1e-10, `pls_algorithm`: `nipals`. Stress test: strict tolerance with few iterations. May not converge -- verify warning. |

**Key verification:** Kernel and widekernelpls algorithms are not iterative and should ignore max_iterations/tolerance. NIPALS results should be more precise with stricter tolerance (at cost of computation time).

---

## 7. BOOTSTRAP VALIDATION

| # | Data | Options to Test |
|---|------|-----------------|
| 28 | `plscox_metabolomics` | `bootstrap_validation`: TRUE, `n_bootstrap`: 50 (minimum). Verify bootstrapResults HTML appears with optimism-corrected C-index. |
| 29 | `plscox_metabolomics` | `bootstrap_validation`: TRUE, `n_bootstrap`: 200 (default). Standard bootstrap. Check confidence intervals for model performance. |
| 30 | `plscox_small` | `bootstrap_validation`: TRUE, `n_bootstrap`: 100. Small dataset bootstrap -- expect wider CIs and more optimism correction. |
| 31 | `plscox_metabolomics` | `bootstrap_validation`: FALSE. Verify bootstrapResults is hidden. |

**Key verification:** Bootstrap C-index should be lower than apparent C-index (optimism correction). Wider CIs expected for small dataset. Bootstrap should not change the primary model results (only adds validation metrics).

---

## 8. PERMUTATION TEST

| # | Data | Options to Test |
|---|------|-----------------|
| 32 | `plscox_metabolomics` | `permutation_test`: TRUE, `n_permutations`: 50 (minimum). Verify permutationResults HTML appears with p-value for model significance. |
| 33 | `plscox_metabolomics` | `permutation_test`: TRUE, `n_permutations`: 200. More permutations for stable p-value. |
| 34 | `plscox_small` | `permutation_test`: TRUE, `n_permutations`: 100. Small dataset -- verify test runs and p-value is reported. |
| 35 | `plscox_metabolomics` | `permutation_test`: FALSE. Verify permutationResults is hidden. |

**Key verification:** With true signal in the data, permutation p-value should be significant (p < 0.05 in most runs). Permutation test is computationally expensive -- tests should have reasonable runtime.

---

## 9. PLOT TOGGLE TESTING

| # | Data | Options to Test |
|---|------|-----------------|
| 36 | `plscox_metabolomics` | All plots TRUE (default). Verify all 5 plots render: componentPlot, loadingsPlot, scoresPlot, validationPlot, survivalPlot. |
| 37 | `plscox_metabolomics` | `plot_components`: FALSE. Verify componentPlot is hidden, other plots still render. |
| 38 | `plscox_metabolomics` | `plot_loadings`: FALSE. Verify loadingsPlot is hidden. |
| 39 | `plscox_metabolomics` | `plot_scores`: FALSE. Verify scoresPlot is hidden. |
| 40 | `plscox_metabolomics` | `plot_validation`: FALSE. Verify validationPlot is hidden. |
| 41 | `plscox_metabolomics` | `plot_survival`: FALSE. Verify survivalPlot is hidden. |
| 42 | `plscox_metabolomics` | ALL plots FALSE. Verify no plots render but tables still populate. |

**Key verification:** Each plot should be independently controllable. No protobuf serialization errors. State should contain only plain data (no model objects with function references).

**State management checks:**
- componentPlot: state should contain component variance/eigenvalue data as numeric vectors
- loadingsPlot: state should contain loading matrix as data.frame (not model object)
- scoresPlot: state should contain score vectors + survival info as data.frame
- validationPlot: state should contain CV scores as numeric vectors
- survivalPlot: state should contain time, status, risk_group as data.frame

---

## 10. RISK GROUP STRATIFICATION

| # | Data | Options to Test |
|---|------|-----------------|
| 43 | `plscox_metabolomics` | `risk_groups`: 2. Binary stratification (high/low risk). Verify riskStratification table has 2 rows. Survival plot shows 2 curves. |
| 44 | `plscox_metabolomics` | `risk_groups`: 3 (default). Tertile stratification. Verify 3 rows, 3 survival curves. |
| 45 | `plscox_metabolomics` | `risk_groups`: 4. Quartile stratification. Verify 4 rows. |
| 46 | `plscox_metabolomics` | `risk_groups`: 5. Quintile stratification. With n=120, each group has ~24 patients. Verify all groups have adequate size. |
| 47 | `plscox_small` | `risk_groups`: 2. Small dataset, binary split. Verify groups are reasonably balanced. |
| 48 | `plscox_small` | `risk_groups`: 5. Small dataset (n=50), 5 groups = ~10/group. May produce unstable HR estimates -- verify warning or graceful handling. |

**Key verification:** Each risk group should have at least some events. Hazard ratios vs low risk group should show monotonic trend (higher risk groups have higher HR). Log-rank p-value should be significant for datasets with true signal.

---

## 11. FEATURE IMPORTANCE

| # | Data | Options to Test |
|---|------|-----------------|
| 49 | `plscox_metabolomics` | `feature_importance`: TRUE (default). Verify variableLoadings table populates with all predictors, component loadings (1-3), and importance score. Top variables should come from signal blocks (METAB_001-015, METAB_025-040, METAB_055-070). |
| 50 | `plscox_metabolomics` | `feature_importance`: FALSE. Verify variableLoadings table is hidden. |
| 51 | `plscox_small` | `feature_importance`: TRUE. Verify importance scores are reported for all 25 markers. |

---

## 12. CONFIDENCE INTERVALS AND PREDICTION ACCURACY

| # | Data | Options to Test |
|---|------|-----------------|
| 52 | `plscox_metabolomics` | `confidence_intervals`: TRUE (default). Verify modelCoefficients table shows hr_lower and hr_upper columns. |
| 53 | `plscox_metabolomics` | `confidence_intervals`: FALSE. Verify CI columns are hidden or empty. |
| 54 | `plscox_metabolomics` | `prediction_accuracy`: TRUE (default). Verify modelPerformance table shows C-index (and SE, CIs). |
| 55 | `plscox_metabolomics` | `prediction_accuracy`: FALSE. Verify modelPerformance table is hidden. |

---

## 13. EDGE CASES

| # | Data | Options to Test |
|---|------|-----------------|
| 56 | Synthetic (n=30, p=5) | Very small dataset with 5 continuous predictors. `pls_components`: 5 (equals p). Verify model handles max components gracefully. |
| 57 | Synthetic (n=100, p=3) | `pls_components`: 10 (exceeds p). Verify error or automatic clamping to min(n, p). |
| 58 | Synthetic (n=50, all censored) | `status` all "Alive". Verify informative error about insufficient events. |
| 59 | Synthetic (n=50, 3 events) | Very few events. Verify warning about limited events and potentially unstable results. |
| 60 | Synthetic with constant column | One METAB variable with zero variance. Verify error or automatic removal of constant predictors. |
| 61 | Synthetic with NA in time | Missing values in time variable. Verify error message about missing survival times. |
| 62 | Synthetic with NA in predictors | 10% missing in predictor columns. Verify handling (listwise deletion or error message). |
| 63 | `plscox_metabolomics` | Only 2 predictors selected (age, bmi). `pls_components`: 1. Minimal PLS model. |
| 64 | `plscox_metabolomics` | Single predictor selected. Verify error "at least 2 predictors required" or equivalent. |

---

## 14. COMBINED OPTION STRESS TESTS

| # | Data | Options to Test |
|---|------|-----------------|
| 65 | `plscox_metabolomics` | Full analysis: `pls_algorithm`: widekernelpls, `cross_validation`: k5, `component_selection`: cv_cindex, `scaling_method`: minmax, `bootstrap_validation`: TRUE, `n_bootstrap`: 100, `permutation_test`: TRUE, `n_permutations`: 50, `risk_groups`: 4, all plots TRUE, all output options TRUE. Verify everything runs without errors. |
| 66 | `plscox_small` | LOO + bootstrap + permutation: `cross_validation`: loo, `component_selection`: cv_loglik, `bootstrap_validation`: TRUE, `n_bootstrap`: 50, `permutation_test`: TRUE, `n_permutations`: 50. Stress test on small data. |
| 67 | `plscox_metabolomics` | Minimal output: all plots FALSE, `feature_importance`: FALSE, `prediction_accuracy`: FALSE, `confidence_intervals`: FALSE, `bootstrap_validation`: FALSE, `permutation_test`: FALSE. Only core model summary and coefficients. |

---

## COMPLETE OPTION COVERAGE CHECKLIST

- [x] `time` -- all tests
- [x] `status` -- all tests
- [x] `predictors` -- all tests
- [x] `pls_components` (1, 3, 5, 10, 15) -- #1, #10, #11, #12, #56, #57
- [x] `pls_algorithm` (nipals / kernel / widekernelpls) -- #2, #3, #4, #5, #65
- [x] `cross_validation` (loo / k10 / k5 / none) -- #13, #14, #15, #16, #66
- [x] `component_selection` (cv_loglik / cv_cindex / bic / aic / manual) -- #6, #7, #8, #9, #10
- [x] `scaling_method` (standardize / unit_variance / minmax / none) -- #18, #19, #20, #21
- [x] `max_iterations` (50, 100, 1000) -- #23, #24, #27
- [x] `tolerance` (1e-03, 1e-06, 1e-10) -- #25, #26, #27
- [x] `bootstrap_validation` (TRUE / FALSE) -- #28, #29, #30, #31
- [x] `n_bootstrap` (50, 100, 200) -- #28, #29, #30
- [x] `permutation_test` (TRUE / FALSE) -- #32, #33, #34, #35
- [x] `n_permutations` (50, 100, 200) -- #32, #33, #34
- [x] `plot_components` (TRUE / FALSE) -- #36, #37
- [x] `plot_loadings` (TRUE / FALSE) -- #36, #38
- [x] `plot_scores` (TRUE / FALSE) -- #36, #39
- [x] `plot_validation` (TRUE / FALSE) -- #36, #40
- [x] `plot_survival` (TRUE / FALSE) -- #36, #41
- [x] `risk_groups` (2, 3, 4, 5) -- #43, #44, #45, #46, #48
- [x] `confidence_intervals` (TRUE / FALSE) -- #52, #53
- [x] `feature_importance` (TRUE / FALSE) -- #49, #50, #51
- [x] `prediction_accuracy` (TRUE / FALSE) -- #54, #55
