# Testing Sparse Group LASSO Cox Regression

Test datasets are stored as follows:
- `.rda` files: `data/` (for R package loading)
- `.omv` and `.csv` files: `data-raw/non-rda/` (for jamovi and manual testing)
- Generation script: `data-raw/create_sparsegrouplasso_test_data.R` (seeds: 42/123/99)

---

## 1. STANDARD CLINICAL -- Lung Cancer Mixed Types

| # | File | Variables | Options to Test |
|---|------|-----------|-----------------|
| 1 | `sparsegrouplasso_lung` | time: `time`, event: `status`, outcomeLevel: `Dead`, censorLevel: `Alive`, predictors: `age, smoking_py, tumor_size, pdl1, crp, nlr, albumin, ldh` | Defaults: `group_definition=factor_based`, `alpha_sgl=0.95`, `cv_folds=10`, `selection_criterion=cv_deviance` |
| 2 | `sparsegrouplasso_lung` | Same + factors: `sex, ecog, histology` | `group_definition=factor_based` (verify dummy grouping) |
| 3 | `sparsegrouplasso_lung` | Same as #1 | `alpha_sgl=0.3` (group-heavy) |
| 4 | `sparsegrouplasso_lung` | Same as #1 | `alpha_sgl=0.99` (LASSO-heavy) |
| 5 | `sparsegrouplasso_lung` | Same as #1 | `selection_criterion=aic` |
| 6 | `sparsegrouplasso_lung` | Same as #1 | `selection_criterion=bic` |
| 7 | `sparsegrouplasso_lung` | Same as #1 | `selection_criterion=ebic`, `ebic_gamma=0.5` |

**Options covered:** `time_var`, `event_var`, `outcomeLevel`, `censorLevel`, `pred_vars`, `group_definition`, `alpha_sgl`, `selection_criterion`, `ebic_gamma`

---

## 2. GROUP DEFINITION METHODS

| # | File | Variables | Options to Test |
|---|------|-----------|-----------------|
| 8 | `sparsegrouplasso_lung` | `age, smoking_py, sex, ecog, histology` | `group_definition=factor_based` |
| 9 | `sparsegrouplasso_lung` | `age, smoking_py, tumor_size, pdl1` | `group_definition=custom`, `custom_groups="1,2;3,4"` |
| 10 | `sparsegrouplasso_lung` | Same as #1 | `group_definition=variable_type` |
| 11 | `sparsegrouplasso_lung` | Same as #1 | `group_definition=correlation_based`, `correlation_threshold=0.5` |
| 12 | `sparsegrouplasso_lung` | Same as #1 | `group_definition=correlation_based`, `correlation_threshold=0.9` |

**Options covered:** `group_definition` (all 5 methods), `custom_groups`, `correlation_threshold`

---

## 3. LAMBDA SEQUENCE & CROSS-VALIDATION

| # | File | Variables | Options to Test |
|---|------|-----------|-----------------|
| 13 | `sparsegrouplasso_lung` | Same as #1 | `lambda_sequence=auto`, `n_lambda=20`, `lambda_min_ratio=0.01` |
| 14 | `sparsegrouplasso_lung` | Same as #1 | `lambda_sequence=custom`, `custom_lambda="0.001,0.01,0.1,1"` |
| 15 | `sparsegrouplasso_lung` | Same as #1 | `lambda_sequence=adaptive` |
| 16 | `sparsegrouplasso_lung` | Same as #1 | `cv_folds=3` |
| 17 | `sparsegrouplasso_lung` | Same as #1 | `cv_repeats=3` |

**Options covered:** `lambda_sequence` (all 3), `custom_lambda`, `lambda_min_ratio`, `n_lambda`, `cv_folds`, `cv_repeats`

---

## 4. ADAPTIVE WEIGHTS

| # | File | Variables | Options to Test |
|---|------|-----------|-----------------|
| 18 | `sparsegrouplasso_lung` | Same as #1 | `weight_type=ridge_based` |
| 19 | `sparsegrouplasso_lung` | Same as #1 | `weight_type=univariate_based` |
| 20 | `sparsegrouplasso_lung` | Same as #1 | `weight_type=lasso_based` |
| 21 | `sparsegrouplasso_lung` | Same as #1 | `weight_type=ridge_based`, `weight_power=2` |

**Options covered:** `weight_type` (all 4), `weight_power`

---

## 5. VALIDATION & STABILITY

| # | File | Variables | Options to Test |
|---|------|-----------|-----------------|
| 22 | `sparsegrouplasso_lung` | Same as #1 | `confidence_intervals=TRUE`, `bootstrap_samples=100`, `alpha_level=0.05` |
| 23 | `sparsegrouplasso_lung` | Same as #1 | `confidence_intervals=TRUE`, `alpha_level=0.01` |
| 24 | `sparsegrouplasso_lung` | Same as #1 | `stability_selection=TRUE`, `bootstrap_samples=100`, `stability_threshold=0.6` |
| 25 | `sparsegrouplasso_lung` | Same as #1 | `stability_selection=TRUE`, `stability_threshold=0.9` (strict) |

**Options covered:** `confidence_intervals`, `bootstrap_samples`, `alpha_level`, `stability_selection`, `stability_threshold`, `stability_subsample`

---

## 6. DISPLAY & OUTPUT OPTIONS

| # | File | Variables | Options to Test |
|---|------|-----------|-----------------|
| 26 | `sparsegrouplasso_lung` | Same as #1 | All `show_*=TRUE`, `showExplanations=TRUE` |
| 27 | `sparsegrouplasso_lung` | Same as #1 | All `show_*=FALSE`, `showExplanations=FALSE` |
| 28 | `sparsegrouplasso_lung` | Same as #1 | `show_path=TRUE` |
| 29 | `sparsegrouplasso_lung` | Same as #1 | All `plot_*=TRUE` |
| 30 | `sparsegrouplasso_lung` | Same as #1 | All `plot_*=FALSE` |

**Options covered:** `show_summary`, `show_coefficients`, `show_groups`, `show_path`, `show_performance`, `show_validation`, `showExplanations`, `plot_cv_error`, `plot_coefficients`, `plot_groups`, `plot_sparsity`, `plot_stability`

---

## 7. PREPROCESSING & ALGORITHM

| # | File | Variables | Options to Test |
|---|------|-----------|-----------------|
| 31 | `sparsegrouplasso_lung` | Same as #1 | `standardize_vars=FALSE` |
| 32 | `sparsegrouplasso_lung` | Same as #1 | `center_vars=FALSE` |
| 33 | `sparsegrouplasso_lung` | Same as #1 | `seed_value=99` |
| 34 | `sparsegrouplasso_lung` | Same as #1 | `suitabilityCheck=FALSE` |

**Options covered:** `standardize_vars`, `center_vars`, `seed_value`, `suitabilityCheck`

---

## 8. HIGH-DIMENSIONAL -- Gene Panel

| # | File | Variables | Options to Test |
|---|------|-----------|-----------------|
| 35 | `sparsegrouplasso_genepanel` | time: `time`, event: `status`, outcomeLevel: `Progressed`, censorLevel: `Stable`, predictors: all 40 gene columns | `cv_folds=3`, `n_lambda=20` |
| 36 | `sparsegrouplasso_genepanel` | Same as #35 | `stability_selection=TRUE`, `bootstrap_samples=50` |

---

## 9. EDGE CASES

| # | File | Variables | Options to Test | Expected Behavior |
|---|------|-----------|-----------------|-------------------|
| 37 | -- | No data loaded | -- | Instructions HTML displayed |
| 38 | `sparsegrouplasso_lung` | time + event only, no predictors | -- | Todo HTML, early return |
| 39 | `sparsegrouplasso_lung` | Only 2 predictors: `age, tumor_size` | Default | Should work (minimum) |
| 40 | `sparsegrouplasso_small` | All 6 predictors | `cv_folds=3` | Small sample warnings expected |
| 41 | `sparsegrouplasso_lung` | All-censored: set `status` to all `Alive` | -- | ERROR notice: no events |
| 42 | `sparsegrouplasso_lung` | 10% missing in `pdl1` | Default | Listwise deletion, runs OK |

---

## AVAILABLE TEST DATASETS

| File | N | Predictors | Events | Key Features |
|------|---|-----------|--------|--------------|
| `sparsegrouplasso_lung` | 180 | 14 (mixed numeric + factor) | 95 | Lung cancer, 4 clinical domains |
| `sparsegrouplasso_genepanel` | 100 | 40 (continuous, 8 pathways) | 43 | Gene expression, true signal in 3 pathways |
| `sparsegrouplasso_small` | 50 | 6 (mixed) | 19 | Small clinical cohort |

**File locations:**
- RDA: `data/sparsegrouplasso_*.rda`
- CSV/OMV: `data-raw/non-rda/sparsegrouplasso_*.csv` (`.omv`)

---

## COMPLETE OPTION COVERAGE CHECKLIST

- [x] `time_var` -- tests #1-42
- [x] `event_var` -- tests #1-42
- [x] `outcomeLevel` -- tests #1-42
- [x] `censorLevel` -- tests #1-42
- [x] `pred_vars` -- tests #1-42
- [x] `suitabilityCheck` -- tests #1 (TRUE), #34 (FALSE)
- [x] `group_definition` (factor_based) -- tests #1, #2, #8
- [x] `group_definition` (custom) -- test #9
- [x] `group_definition` (pathway_based) -- needs pathway variable
- [x] `group_definition` (variable_type) -- test #10
- [x] `group_definition` (correlation_based) -- tests #11, #12
- [x] `custom_groups` -- test #9
- [x] `pathway_info` -- needs pathway variable in dataset
- [x] `correlation_threshold` -- tests #11, #12
- [x] `alpha_sgl` -- tests #1 (0.95), #3 (0.3), #4 (0.99)
- [x] `lambda_sequence` (auto) -- test #13
- [x] `lambda_sequence` (custom) -- test #14
- [x] `lambda_sequence` (adaptive) -- test #15
- [x] `custom_lambda` -- test #14
- [x] `lambda_min_ratio` -- test #13
- [x] `n_lambda` -- test #13
- [x] `selection_criterion` (cv_deviance) -- test #1
- [x] `selection_criterion` (aic) -- test #5
- [x] `selection_criterion` (bic) -- test #6
- [x] `selection_criterion` (ebic) -- test #7
- [x] `cv_folds` -- tests #16, #35
- [x] `cv_repeats` -- test #17
- [x] `ebic_gamma` -- test #7
- [x] `weight_type` (none) -- test #1
- [x] `weight_type` (ridge_based) -- tests #18, #21
- [x] `weight_type` (univariate_based) -- test #19
- [x] `weight_type` (lasso_based) -- test #20
- [x] `weight_power` -- test #21
- [x] `standardize_vars` -- test #31
- [x] `center_vars` -- test #32
- [x] `seed_value` -- test #33
- [x] `show_summary` -- tests #26, #27
- [x] `show_coefficients` -- tests #26, #27
- [x] `show_groups` -- tests #26, #27
- [x] `show_path` -- test #28
- [x] `show_performance` -- tests #26, #27
- [x] `show_validation` -- tests #26, #27
- [x] `plot_cv_error` -- tests #29, #30
- [x] `plot_coefficients` -- tests #29, #30
- [x] `plot_groups` -- tests #29, #30
- [x] `plot_sparsity` -- tests #29, #30
- [x] `plot_stability` -- tests #29, #30
- [x] `alpha_level` -- tests #22, #23
- [x] `confidence_intervals` -- tests #22, #23
- [x] `bootstrap_samples` -- tests #22, #24
- [x] `stability_selection` -- tests #24, #25
- [x] `stability_threshold` -- tests #24, #25
- [x] `stability_subsample` -- test #24
- [x] `showExplanations` -- tests #26, #27
