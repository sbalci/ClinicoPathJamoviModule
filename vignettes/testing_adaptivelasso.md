# Testing Adaptive LASSO for Cox Models

All test datasets are in `data/` (RDA/OMV) or `data/` (CSV). Synthetic data can be generated via `data-raw/create_adaptivelasso_test_data.R`.

**Datasets:**

| Name | File | n | Events | Variables | Purpose |
|------|------|---|--------|-----------|---------|
| Main | `adaptivelasso_test_data` | 180 | ~55% | 12 predictors + strata | Full option coverage |
| Small | `adaptivelasso_small_data` | 40 | ~30% (12) | 6 predictors + strata | Edge cases, convergence |

---

## 1. STANDARD CLINICAL (Default Settings)

| # | Data | Variables | Options to Test |
|---|------|-----------|-----------------|
| 1 | `adaptivelasso_test_data` (n=180) | time: `time`, event: `event`, predictors: age, tumor_size, ki67_index, hemoglobin, crp_level, albumin, gender, stage, grade, treatment, smoking_status | Default settings: `weight_method`: ridge, `alpha`: 1.0, `gamma`: 1.0, `cv_folds`: 10, `cv_measure`: deviance, `lambda_sequence`: auto, `standardize`: TRUE. Enable `show_coefficients`, `show_cv_results`, `show_diagnostics`. Verify coefficients table, CV results table, and diagnostics table render. Confirm near-null predictors (hemoglobin, albumin) are excluded. |
| 2 | `adaptivelasso_test_data` | Same as #1, add strata: `center` | `strata`: center. Verify stratified Cox baseline hazards are separate per center. `show_selection_path`: TRUE. `plot_selection_path`: TRUE, `plot_cv_curve`: TRUE. |

**Options covered:** `time`, `event`, `predictors`, `strata`, `weight_method` (ridge), `alpha`, `gamma`, `cv_folds`, `cv_measure` (deviance), `lambda_sequence` (auto), `standardize`, `show_coefficients`, `show_selection_path`, `show_cv_results`, `show_diagnostics`, `plot_selection_path`, `plot_cv_curve`

---

## 2. WEIGHT METHODS

| # | Data | Variables | Options to Test |
|---|------|-----------|-----------------|
| 3 | `adaptivelasso_test_data` | time: `time`, event: `event`, predictors: age, tumor_size, ki67_index, stage, grade, treatment | `weight_method`: **ridge** (default). Record selected variables and coefficients. |
| 4 | Same as #3 | Same as #3 | `weight_method`: **univariate**. Compare selected variables vs #3. |
| 5 | Same as #3 | Same as #3 | `weight_method`: **cox**. Compare selected variables vs #3. Full Cox weights may be less stable. |
| 6 | Same as #3 | Same as #3 | `weight_method`: **correlation**. Compare selected variables vs #3. |
| 7 | Same as #3 | Same as #3 | `weight_method`: **equal**. This degenerates to standard LASSO. Compare selected variables vs #3. |

**Options covered:** All 5 `weight_method` levels: ridge, univariate, cox, correlation, equal

---

## 3. ALPHA AND GAMMA SENSITIVITY

| # | Data | Variables | Options to Test |
|---|------|-----------|-----------------|
| 8 | `adaptivelasso_test_data` | All predictors | `alpha`: 1.0 (pure LASSO). Note number of selected variables. |
| 9 | Same as #8 | Same as #8 | `alpha`: 0.5 (elastic net). Expect more variables selected than #8. |
| 10 | Same as #8 | Same as #8 | `alpha`: 0.0 (pure ridge, no selection). All variables should remain. |
| 11 | Same as #8 | Same as #8 | `gamma`: 0.5 (weaker adaptive penalty). Compare with #8. |
| 12 | Same as #8 | Same as #8 | `gamma`: 2.0 (stronger adaptive penalty). Expect more aggressive selection. |
| 13 | Same as #8 | Same as #8 | `gamma`: 5.0 (extreme). Verify convergence and coefficient stability. |

**Options covered:** `alpha` (0.0, 0.5, 1.0), `gamma` (0.5, 1.0, 2.0, 5.0)

---

## 4. CROSS-VALIDATION OPTIONS

| # | Data | Variables | Options to Test |
|---|------|-----------|-----------------|
| 14 | `adaptivelasso_test_data` | All predictors | `cv_folds`: 5, `cv_measure`: deviance. |
| 15 | Same as #14 | Same as #14 | `cv_folds`: 10, `cv_measure`: **C** (C-index). Compare with #14. |
| 16 | Same as #14 | Same as #14 | `cv_folds`: 20, `cv_measure`: **brier** (Integrated Brier Score). |
| 17 | Same as #14 | Same as #14 | `cv_measure`: **auc** (time-dependent AUC). |
| 18 | Same as #14 | Same as #14 | `lambda_sequence`: **custom**, `lambda_min_ratio`: 0.0001, `n_lambda`: 200. Verify finer lambda grid. |
| 19 | Same as #14 | Same as #14 | `lambda_sequence`: **single**. Verify single-lambda fitting. |

**Options covered:** `cv_folds` (5, 10, 20), `cv_measure` (deviance, C, brier, auc), `lambda_sequence` (auto, custom, single), `lambda_min_ratio`, `n_lambda`

---

## 5. STABILITY SELECTION

| # | Data | Variables | Options to Test |
|---|------|-----------|-----------------|
| 20 | `adaptivelasso_test_data` | All predictors | `stability_selection`: TRUE, `stability_threshold`: 0.6, `bootstrap_samples`: 100, `subsampling_ratio`: 0.8. Verify stabilityResults table renders with selection frequencies. `plot_stability`: TRUE. |
| 21 | Same as #20 | Same as #20 | `stability_threshold`: 0.9. Fewer variables should pass threshold. |
| 22 | Same as #20 | Same as #20 | `stability_threshold`: 0.5. More variables should pass threshold. |
| 23 | Same as #20 | Same as #20 | `bootstrap_samples`: 200, `subsampling_ratio`: 0.6. Verify different subsampling produces stable results for strong predictors (stage, treatment). |

**Options covered:** `stability_selection`, `stability_threshold` (0.5, 0.6, 0.9), `bootstrap_samples` (100, 200), `subsampling_ratio` (0.6, 0.8), `plot_stability`

---

## 6. MODEL DIAGNOSTICS

| # | Data | Variables | Options to Test |
|---|------|-----------|-----------------|
| 24 | `adaptivelasso_test_data` | All predictors | `proportional_hazards`: TRUE, `goodness_of_fit`: TRUE, `show_diagnostics`: TRUE. Verify modelDiagnostics table with PH test results and performanceMetrics table with C-index. |
| 25 | Same as #24 | Same as #24 | `influence_diagnostics`: TRUE, `plot_diagnostics`: TRUE. Verify diagnosticsPlot renders with dfbeta/leverage. |
| 26 | Same as #24 | Same as #24 | `proportional_hazards`: FALSE, `influence_diagnostics`: FALSE, `goodness_of_fit`: FALSE. Verify diagnostics tables are hidden/empty. |

**Options covered:** `proportional_hazards` (TRUE/FALSE), `influence_diagnostics` (TRUE/FALSE), `goodness_of_fit` (TRUE/FALSE), `show_diagnostics`, `plot_diagnostics`

---

## 7. RISK GROUPS AND PREDICTIONS

| # | Data | Variables | Options to Test |
|---|------|-----------|-----------------|
| 27 | `adaptivelasso_test_data` | All predictors | `risk_groups`: 2, `plot_survival_curves`: TRUE. Verify 2-group KM curves. |
| 28 | Same as #27 | Same as #27 | `risk_groups`: 3 (default). Verify 3-group KM curves with risk table. |
| 29 | Same as #27 | Same as #27 | `risk_groups`: 5. Verify 5 risk strata. |
| 30 | Same as #27 | Same as #27 | `risk_groups`: 10. With n=180, some groups may be small. Verify graceful handling. |
| 31 | `adaptivelasso_test_data` | All predictors | `time_points`: "6, 12, 24, 60", `baseline_survival`: TRUE, `plot_baseline_hazard`: TRUE. Verify predictions table at specified time points and baseline hazard plot. |
| 32 | Same as #31 | Same as #31 | `time_points`: "" (empty). Verify data-driven quantile time points are used. |

**Options covered:** `risk_groups` (2, 3, 5, 10), `time_points` (custom, empty), `baseline_survival`, `plot_survival_curves`, `plot_baseline_hazard`

---

## 8. PLOT RENDERING

| # | Data | Variables | Options to Test |
|---|------|-----------|-----------------|
| 33 | `adaptivelasso_test_data` | All predictors | All plots TRUE: `plot_selection_path`, `plot_cv_curve`, `plot_stability` (with `stability_selection`: TRUE), `plot_survival_curves`, `plot_baseline_hazard`, `plot_diagnostics`. Verify all 6 plots render without protobuf serialization errors. |
| 34 | Same as #33 | Same as #33 | All plots FALSE. Verify no plot output items are visible. |
| 35 | Same as #33 | Same as #33 | Toggle each plot individually TRUE (others FALSE) to isolate rendering issues. |

**Key verification:** All plots should render without protobuf serialization errors. setState() must use only plain numeric vectors/data.frames.

**Options covered:** `plot_selection_path`, `plot_cv_curve`, `plot_stability`, `plot_survival_curves`, `plot_baseline_hazard`, `plot_diagnostics`

---

## 9. OUTPUT TABLE TOGGLES

| # | Data | Variables | Options to Test |
|---|------|-----------|-----------------|
| 36 | `adaptivelasso_test_data` | All predictors | `show_coefficients`: TRUE, `show_selection_path`: TRUE, `show_cv_results`: TRUE, `show_diagnostics`: TRUE. All 4 output tables visible. |
| 37 | Same as #36 | Same as #36 | `show_coefficients`: FALSE. Coefficients table hidden. |
| 38 | Same as #36 | Same as #36 | `show_selection_path`: FALSE. Selection path table hidden. |
| 39 | Same as #36 | Same as #36 | `show_cv_results`: FALSE. CV results table hidden. |
| 40 | Same as #36 | Same as #36 | `show_diagnostics`: FALSE. Diagnostics table hidden. |

**Options covered:** `show_coefficients`, `show_selection_path`, `show_cv_results`, `show_diagnostics`

---

## 10. ADVANCED OPTIONS

| # | Data | Variables | Options to Test |
|---|------|-----------|-----------------|
| 41 | `adaptivelasso_test_data` | All predictors | `tie_method`: **breslow** (default). Note performance metrics. |
| 42 | Same as #41 | Same as #41 | `tie_method`: **efron**. Compare performance metrics with #41. |
| 43 | Same as #41 | Same as #41 | `standardize`: FALSE. Coefficients may differ due to unscaled predictors. |
| 44 | Same as #41 | Same as #41 | `intercept`: TRUE. Verify intercept term appears in output. |
| 45 | Same as #41 | Same as #41 | `convergence_threshold`: 1e-10, `max_iterations`: 50000. Stricter convergence. |
| 46 | Same as #41 | Same as #41 | `convergence_threshold`: 1e-4, `max_iterations`: 1000. Looser convergence; verify potential warnings. |
| 47 | Same as #41 | Same as #41 | `random_seed`: 42. Change from default (123). Verify different CV fold assignment. |
| 48 | Same as #41 | Same as #41 | `random_seed`: 123 twice in succession. Verify identical results (reproducibility). |
| 49 | Same as #41 | Same as #41 | `parallel_computing`: TRUE, `n_cores`: 2. Verify parallel CV runs without error. |
| 50 | Same as #41 | Same as #41 | `parallel_computing`: TRUE, `n_cores`: 4. Verify parallel with more cores. |

**Options covered:** `tie_method` (breslow, efron), `standardize` (TRUE/FALSE), `intercept` (TRUE/FALSE), `convergence_threshold`, `max_iterations`, `random_seed`, `parallel_computing`, `n_cores`

---

## 11. EDGE CASES

| # | Data | Variables | Options to Test |
|---|------|-----------|-----------------|
| 51 | `adaptivelasso_small_data` (n=40) | time: `time`, event: `event`, predictors: age, tumor_size, gender, stage, biomarker_a, biomarker_b | Small sample. `cv_folds`: 3 (avoid empty folds). Verify warnings about low EPV. |
| 52 | `adaptivelasso_small_data` | Same as #51, strata: `center` | Small sample with stratification. Verify stratified model converges. |
| 53 | `adaptivelasso_small_data` | Same as #51 | `stability_selection`: TRUE, `bootstrap_samples`: 50. Minimal bootstrap with small data. |
| 54 | Synthetic (n=50, all censored) | Create: all `event` = "Censored" | Verify error message about insufficient events. |
| 55 | Synthetic (n=100, 1 predictor) | Single explanatory variable | Verify error "at least 2 predictor variables required" or graceful handling. |
| 56 | Synthetic with constant variable | One predictor all same value | Verify error about zero-variance predictors. |
| 57 | Synthetic with zero times | Some `time = 0` | Verify warning about zero or negative survival times. |
| 58 | `adaptivelasso_small_data` | All predictors | `lambda_sequence`: single. Model may select no variables. Verify fallback or warning. |
| 59 | Synthetic with NA in time | NAs in time variable | Verify error "Time variable contains missing values." |
| 60 | Synthetic with NAs in predictors | 20% missing in one predictor | Verify complete-case or imputation handling. |

---

## 12. COMBINED FEATURE TESTS

| # | Data | Variables | Options to Test |
|---|------|-----------|-----------------|
| 61 | `adaptivelasso_test_data` | All predictors, strata: center | Full pipeline: `weight_method`: ridge, `stability_selection`: TRUE, `proportional_hazards`: TRUE, `influence_diagnostics`: TRUE, `goodness_of_fit`: TRUE, `risk_groups`: 4, `time_points`: "12, 24, 36, 48", `baseline_survival`: TRUE. All tables TRUE. All plots TRUE. Verify complete output without errors. |
| 62 | `adaptivelasso_test_data` | All predictors | `weight_method`: univariate, `alpha`: 0.7, `gamma`: 2.0, `cv_folds`: 5, `cv_measure`: C. Verify elastic net + adaptive penalty combination. |
| 63 | `adaptivelasso_test_data` | All predictors | Reproducibility test: run with `random_seed`: 123 twice. All coefficients and selection results must be identical. |

---

## COMPLETE OPTION COVERAGE CHECKLIST

### Input Variables
- [x] `time` -- all tests
- [x] `event` -- all tests
- [x] `predictors` -- all tests
- [x] `strata` -- #2, #52, #61

### Model Specification
- [x] `weight_method` (ridge) -- #1, #3
- [x] `weight_method` (univariate) -- #4, #62
- [x] `weight_method` (cox) -- #5
- [x] `weight_method` (correlation) -- #6
- [x] `weight_method` (equal) -- #7
- [x] `alpha` (0.0, 0.5, 1.0) -- #8, #9, #10
- [x] `gamma` (0.5, 1.0, 2.0, 5.0) -- #11, #12, #13

### Cross-Validation
- [x] `cv_folds` (3, 5, 10, 20) -- #14, #15, #16, #51
- [x] `cv_measure` (deviance) -- #1, #14
- [x] `cv_measure` (C) -- #15, #62
- [x] `cv_measure` (brier) -- #16
- [x] `cv_measure` (auc) -- #17
- [x] `lambda_sequence` (auto) -- #1
- [x] `lambda_sequence` (custom) -- #18
- [x] `lambda_sequence` (single) -- #19, #58
- [x] `lambda_min_ratio` -- #18
- [x] `n_lambda` -- #18

### Stability Selection
- [x] `stability_selection` (TRUE/FALSE) -- #20, #53
- [x] `stability_threshold` (0.5, 0.6, 0.9) -- #20, #21, #22
- [x] `bootstrap_samples` (50, 100, 200) -- #20, #23, #53
- [x] `subsampling_ratio` (0.6, 0.8) -- #20, #23

### Diagnostics
- [x] `proportional_hazards` (TRUE/FALSE) -- #24, #26
- [x] `influence_diagnostics` (TRUE/FALSE) -- #25, #26
- [x] `goodness_of_fit` (TRUE/FALSE) -- #24, #26

### Risk & Predictions
- [x] `risk_groups` (2, 3, 5, 10) -- #27, #28, #29, #30
- [x] `time_points` (custom, empty) -- #31, #32
- [x] `baseline_survival` (TRUE) -- #31

### Output Tables
- [x] `show_coefficients` (TRUE/FALSE) -- #36, #37
- [x] `show_selection_path` (TRUE/FALSE) -- #36, #38
- [x] `show_cv_results` (TRUE/FALSE) -- #36, #39
- [x] `show_diagnostics` (TRUE/FALSE) -- #36, #40

### Plots
- [x] `plot_selection_path` (TRUE/FALSE) -- #33, #34, #35
- [x] `plot_cv_curve` (TRUE/FALSE) -- #33, #34, #35
- [x] `plot_stability` (TRUE/FALSE) -- #33, #34, #35
- [x] `plot_survival_curves` (TRUE/FALSE) -- #33, #34, #35
- [x] `plot_baseline_hazard` (TRUE/FALSE) -- #33, #34, #35
- [x] `plot_diagnostics` (TRUE/FALSE) -- #33, #34, #35

### Advanced
- [x] `tie_method` (breslow, efron) -- #41, #42
- [x] `standardize` (TRUE/FALSE) -- #1, #43
- [x] `intercept` (TRUE/FALSE) -- #44
- [x] `parallel_computing` (TRUE/FALSE) -- #49, #50
- [x] `n_cores` (2, 4) -- #49, #50
- [x] `convergence_threshold` -- #45, #46
- [x] `max_iterations` -- #45, #46
- [x] `random_seed` -- #47, #48, #63
