# Testing Adaptive LASSO for Cox Models

All test datasets are in `data/` (RDA) or `data-raw/` (CSV/scripts). Synthetic data can be generated via `data-raw/create_adaptivelasso_test_data.R`.

**Changelog (2026-03-10):** Removed non-existent `intercept` option and `brier`/`auc` cv_measure values. Added `suitabilityCheck`, `event_level`, `lambda_custom_max`, `lambda_custom_min`, `lambda_single` coverage. Added `adaptivelasso_highdim_data` dataset. Expanded to 65 test scenarios covering all 42 options.

**Datasets:**

| Name | File | n | Events | Variables | Purpose |
|------|------|---|--------|-----------|---------|
| Main | `adaptivelasso_test_data` | 180 | ~55% (~99) | 12 predictors (age, tumor_size, ki67_index, hemoglobin, crp_level, albumin, gender, stage, grade, treatment, smoking_status) + strata (center) | Full option coverage |
| Small | `adaptivelasso_small_data` | 40 | ~30% (~12) | 6 predictors (age, tumor_size, gender, stage, biomarker_a, biomarker_b) + strata (center) | Edge cases, convergence |
| High-dim | `adaptivelasso_highdim_data` | TBD | TBD | Many predictors | High-dimensional testing |

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
| 16 | Same as #14 | Same as #14 | `cv_folds`: 20, `cv_measure`: deviance. Verify more folds produces stable lambda selection. |
| 17 | Same as #14 | Same as #14 | `lambda_sequence`: **custom**, `lambda_custom_max`: 1.0, `lambda_custom_min`: 0.001, `n_lambda`: 200. Verify custom lambda grid. |
| 18 | Same as #14 | Same as #14 | `lambda_sequence`: **custom**, `lambda_custom_max`: 10.0, `lambda_custom_min`: 0.0001, `lambda_min_ratio`: 0.0001. Verify wider custom range. |
| 19 | Same as #14 | Same as #14 | `lambda_sequence`: **single**, `lambda_single`: 0.01. Verify single-lambda fitting (no CV curve). |
| 20 | Same as #14 | Same as #14 | `lambda_sequence`: **single**, `lambda_single`: 0.5. Verify aggressive single lambda selects fewer variables. |

**Options covered:** `cv_folds` (5, 10, 20), `cv_measure` (deviance, C), `lambda_sequence` (auto, custom, single), `lambda_custom_max`, `lambda_custom_min`, `lambda_single`, `lambda_min_ratio`, `n_lambda`

---

## 5. STABILITY SELECTION

| # | Data | Variables | Options to Test |
|---|------|-----------|-----------------|
| 21 | `adaptivelasso_test_data` | All predictors | `stability_selection`: TRUE, `stability_threshold`: 0.6, `bootstrap_samples`: 100, `subsampling_ratio`: 0.8. Verify stabilityResults table renders with selection frequencies. `plot_stability`: TRUE. |
| 22 | Same as #21 | Same as #21 | `stability_threshold`: 0.9. Fewer variables should pass threshold. |
| 23 | Same as #21 | Same as #21 | `stability_threshold`: 0.5. More variables should pass threshold. |
| 24 | Same as #21 | Same as #21 | `bootstrap_samples`: 200, `subsampling_ratio`: 0.6. Verify different subsampling produces stable results for strong predictors (stage, treatment). |

**Options covered:** `stability_selection`, `stability_threshold` (0.5, 0.6, 0.9), `bootstrap_samples` (100, 200), `subsampling_ratio` (0.6, 0.8), `plot_stability`

---

## 6. MODEL DIAGNOSTICS

| # | Data | Variables | Options to Test |
|---|------|-----------|-----------------|
| 25 | `adaptivelasso_test_data` | All predictors | `proportional_hazards`: TRUE, `goodness_of_fit`: TRUE, `show_diagnostics`: TRUE. Verify modelDiagnostics table with PH test results and performanceMetrics table with C-index. |
| 26 | Same as #25 | Same as #25 | `influence_diagnostics`: TRUE, `plot_diagnostics`: TRUE. Verify diagnosticsPlot renders with dfbeta and residuals vs LP. |
| 27 | Same as #25 | Same as #25 | `proportional_hazards`: FALSE, `influence_diagnostics`: FALSE, `goodness_of_fit`: FALSE. Verify diagnostics tables show only basic metrics. |

**Options covered:** `proportional_hazards` (TRUE/FALSE), `influence_diagnostics` (TRUE/FALSE), `goodness_of_fit` (TRUE/FALSE), `show_diagnostics`, `plot_diagnostics`

---

## 7. RISK GROUPS AND PREDICTIONS

| # | Data | Variables | Options to Test |
|---|------|-----------|-----------------|
| 28 | `adaptivelasso_test_data` | All predictors | `risk_groups`: 2, `plot_survival_curves`: TRUE. Verify 2-group KM curves. |
| 29 | Same as #28 | Same as #28 | `risk_groups`: 3 (default). Verify 3-group KM curves with log-rank p. |
| 30 | Same as #28 | Same as #28 | `risk_groups`: 5. Verify 5 risk strata. |
| 31 | Same as #28 | Same as #28 | `risk_groups`: 10. With n=180, some groups may be small. Verify graceful handling (note in table if groups collapse). |
| 32 | `adaptivelasso_test_data` | All predictors | `time_points`: "6, 12, 24, 60", `baseline_survival`: TRUE, `plot_baseline_hazard`: TRUE. Verify predictions table at specified time points and baseline hazard plot. |
| 33 | Same as #32 | Same as #32 | `time_points`: "" (empty). Verify predictions table is suppressed when time_points is empty. |
| 34 | Same as #32 | Same as #32 | `baseline_survival`: FALSE. Verify predictions table hidden and baseline hazard plot hidden. |

**Options covered:** `risk_groups` (2, 3, 5, 10), `time_points` (custom, empty), `baseline_survival` (TRUE/FALSE), `plot_survival_curves`, `plot_baseline_hazard`

---

## 8. PLOT RENDERING

| # | Data | Variables | Options to Test |
|---|------|-----------|-----------------|
| 35 | `adaptivelasso_test_data` | All predictors | All plots TRUE: `plot_selection_path`, `plot_cv_curve`, `plot_stability` (with `stability_selection`: TRUE), `plot_survival_curves`, `plot_baseline_hazard`, `plot_diagnostics`. Verify all 6 plots render without protobuf serialization errors. |
| 36 | Same as #35 | Same as #35 | All plots FALSE. Verify no plot output items are visible. |
| 37 | Same as #35 | Same as #35 | Toggle each plot individually TRUE (others FALSE) to isolate rendering issues. |

**Key verification:** All plots should render without protobuf serialization errors. setState() must use only plain numeric vectors/data.frames.

**Options covered:** `plot_selection_path`, `plot_cv_curve`, `plot_stability`, `plot_survival_curves`, `plot_baseline_hazard`, `plot_diagnostics`

---

## 9. OUTPUT TABLE TOGGLES

| # | Data | Variables | Options to Test |
|---|------|-----------|-----------------|
| 38 | `adaptivelasso_test_data` | All predictors | `show_coefficients`: TRUE, `show_selection_path`: TRUE, `show_cv_results`: TRUE, `show_diagnostics`: TRUE. All 4 output tables visible. |
| 39 | Same as #38 | Same as #38 | `show_coefficients`: FALSE. Coefficients table and riskGroups table hidden. |
| 40 | Same as #38 | Same as #38 | `show_selection_path`: FALSE. Selection path table hidden. |
| 41 | Same as #38 | Same as #38 | `show_cv_results`: FALSE. CV results table hidden. |
| 42 | Same as #38 | Same as #38 | `show_diagnostics`: FALSE. Diagnostics table hidden. |

**Options covered:** `show_coefficients`, `show_selection_path`, `show_cv_results`, `show_diagnostics`

---

## 10. ADVANCED OPTIONS

| # | Data | Variables | Options to Test |
|---|------|-----------|-----------------|
| 43 | `adaptivelasso_test_data` | All predictors | `tie_method`: **breslow** (default). Note performance metrics. |
| 44 | Same as #43 | Same as #43 | `tie_method`: **efron**. Compare performance metrics with #43. |
| 45 | Same as #43 | Same as #43 | `standardize`: FALSE. Coefficients may differ due to unscaled predictors. |
| 46 | Same as #43 | Same as #43 | `convergence_threshold`: 1e-10, `max_iterations`: 50000. Stricter convergence. |
| 47 | Same as #43 | Same as #43 | `convergence_threshold`: 1e-4, `max_iterations`: 1000. Looser convergence; verify potential warnings. |
| 48 | Same as #43 | Same as #43 | `random_seed`: 42. Change from default (123). Verify different CV fold assignment produces different results. |
| 49 | Same as #43 | Same as #43 | `random_seed`: 123 twice in succession. Verify identical results (reproducibility). |
| 50 | Same as #43 | Same as #43 | `parallel_computing`: TRUE, `n_cores`: 2. Verify parallel CV runs without error. |
| 51 | Same as #43 | Same as #43 | `parallel_computing`: TRUE, `n_cores`: 4. Verify parallel with more cores. |

**Options covered:** `tie_method` (breslow, efron), `standardize` (TRUE/FALSE), `convergence_threshold`, `max_iterations`, `random_seed`, `parallel_computing`, `n_cores`

---

## 11. DATA SUITABILITY ASSESSMENT

| # | Data | Variables | Options to Test |
|---|------|-----------|-----------------|
| 52 | `adaptivelasso_test_data` | All predictors | `suitabilityCheck`: TRUE. Verify suitabilityReport HTML renders with EPV calculation (should be green/adequate). |
| 53 | `adaptivelasso_small_data` | All predictors | `suitabilityCheck`: TRUE. Verify suitabilityReport shows orange/red EPV warning (low EPV with small data). |
| 54 | `adaptivelasso_test_data` | All predictors | `suitabilityCheck`: FALSE. Verify suitabilityReport HTML is hidden. |

**Options covered:** `suitabilityCheck` (TRUE/FALSE)

---

## 12. EVENT LEVEL HANDLING

| # | Data | Variables | Options to Test |
|---|------|-----------|-----------------|
| 55 | `adaptivelasso_test_data` | time: `time`, event: `event` (if factor with >2 levels), `event_level`: specify one level | Verify correct binary encoding using specified event level. |
| 56 | `adaptivelasso_test_data` | time: `time`, event: `event` (binary factor), `event_level`: leave empty | Verify default second level is used as event. |

**Options covered:** `event_level` (specified, empty/default)

---

## 13. EDGE CASES

| # | Data | Variables | Options to Test |
|---|------|-----------|-----------------|
| 57 | `adaptivelasso_small_data` (n=40) | time: `time`, event: `event`, predictors: age, tumor_size, gender, stage, biomarker_a, biomarker_b | Small sample. `cv_folds`: 3 (avoid empty folds). Verify warnings about low EPV. |
| 58 | `adaptivelasso_small_data` | Same as #57, strata: `center` | Small sample with stratification. Verify stratified model converges. |
| 59 | `adaptivelasso_small_data` | Same as #57 | `stability_selection`: TRUE, `bootstrap_samples`: 50. Minimal bootstrap with small data. |
| 60 | Synthetic (n=50, all censored) | Create: all `event` = 0 | Verify error message about insufficient events (<5). |
| 61 | Synthetic (n=100, 1 predictor) | Single explanatory variable | Verify graceful handling with 1 predictor. |
| 62 | Synthetic with constant variable | One predictor all same value | Verify constant columns removed and warning if no valid predictors remain. |
| 63 | Synthetic with zero/negative times | Some `time <= 0` | Verify error about non-positive survival times. |
| 64 | `adaptivelasso_small_data` | All predictors | `lambda_sequence`: single, `lambda_single`: 1.0. Model may select no variables. Verify "No Variables Selected" warning. |
| 65 | Synthetic with NAs in predictors | 20% missing in one predictor | Verify listwise deletion and missing data notice in suitabilityReport. |

---

## 14. COMBINED FEATURE TESTS

| # | Data | Variables | Options to Test |
|---|------|-----------|-----------------|
| 66 | `adaptivelasso_test_data` | All predictors, strata: center | Full pipeline: `weight_method`: ridge, `stability_selection`: TRUE, `proportional_hazards`: TRUE, `influence_diagnostics`: TRUE, `goodness_of_fit`: TRUE, `suitabilityCheck`: TRUE, `risk_groups`: 4, `time_points`: "12, 24, 36, 48", `baseline_survival`: TRUE. All tables TRUE. All plots TRUE. Verify complete output without errors. |
| 67 | `adaptivelasso_test_data` | All predictors | `weight_method`: univariate, `alpha`: 0.7, `gamma`: 2.0, `cv_folds`: 5, `cv_measure`: C. Verify elastic net + adaptive penalty combination. |
| 68 | `adaptivelasso_test_data` | All predictors | Reproducibility test: run with `random_seed`: 123 twice. All coefficients and selection results must be identical. |

---

## COMPLETE OPTION COVERAGE CHECKLIST

### Input Variables
- [x] `time` -- all tests
- [x] `event` -- all tests
- [x] `event_level` -- #55, #56
- [x] `predictors` -- all tests
- [x] `strata` -- #2, #58, #66

### Data Assessment
- [x] `suitabilityCheck` (TRUE/FALSE) -- #52, #53, #54

### Model Specification
- [x] `weight_method` (ridge) -- #1, #3
- [x] `weight_method` (univariate) -- #4, #67
- [x] `weight_method` (cox) -- #5
- [x] `weight_method` (correlation) -- #6
- [x] `weight_method` (equal) -- #7
- [x] `alpha` (0.0, 0.5, 1.0) -- #8, #9, #10
- [x] `gamma` (0.5, 1.0, 2.0, 5.0) -- #11, #12, #13

### Cross-Validation
- [x] `cv_folds` (3, 5, 10, 20) -- #14, #15, #16, #57
- [x] `cv_measure` (deviance) -- #1, #14
- [x] `cv_measure` (C) -- #15, #67
- [x] `lambda_sequence` (auto) -- #1
- [x] `lambda_sequence` (custom) -- #17, #18
- [x] `lambda_sequence` (single) -- #19, #20, #64
- [x] `lambda_custom_max` -- #17, #18
- [x] `lambda_custom_min` -- #17, #18
- [x] `lambda_single` -- #19, #20, #64
- [x] `lambda_min_ratio` -- #18
- [x] `n_lambda` -- #17

### Stability Selection
- [x] `stability_selection` (TRUE/FALSE) -- #21, #59
- [x] `stability_threshold` (0.5, 0.6, 0.9) -- #21, #22, #23
- [x] `bootstrap_samples` (50, 100, 200) -- #21, #24, #59
- [x] `subsampling_ratio` (0.6, 0.8) -- #21, #24

### Diagnostics
- [x] `proportional_hazards` (TRUE/FALSE) -- #25, #27
- [x] `influence_diagnostics` (TRUE/FALSE) -- #26, #27
- [x] `goodness_of_fit` (TRUE/FALSE) -- #25, #27

### Risk & Predictions
- [x] `risk_groups` (2, 3, 5, 10) -- #28, #29, #30, #31
- [x] `time_points` (custom, empty) -- #32, #33
- [x] `baseline_survival` (TRUE/FALSE) -- #32, #34

### Output Tables
- [x] `show_coefficients` (TRUE/FALSE) -- #38, #39
- [x] `show_selection_path` (TRUE/FALSE) -- #38, #40
- [x] `show_cv_results` (TRUE/FALSE) -- #38, #41
- [x] `show_diagnostics` (TRUE/FALSE) -- #38, #42

### Plots
- [x] `plot_selection_path` (TRUE/FALSE) -- #35, #36, #37
- [x] `plot_cv_curve` (TRUE/FALSE) -- #35, #36, #37
- [x] `plot_stability` (TRUE/FALSE) -- #35, #36, #37
- [x] `plot_survival_curves` (TRUE/FALSE) -- #35, #36, #37
- [x] `plot_baseline_hazard` (TRUE/FALSE) -- #35, #36, #37
- [x] `plot_diagnostics` (TRUE/FALSE) -- #35, #36, #37

### Advanced
- [x] `tie_method` (breslow, efron) -- #43, #44
- [x] `standardize` (TRUE/FALSE) -- #1, #45
- [x] `parallel_computing` (TRUE/FALSE) -- #50, #51
- [x] `n_cores` (2, 4) -- #50, #51
- [x] `convergence_threshold` -- #46, #47
- [x] `max_iterations` -- #46, #47
- [x] `random_seed` -- #48, #49, #68
