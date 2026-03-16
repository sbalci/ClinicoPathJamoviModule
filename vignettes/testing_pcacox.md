# Testing PCA Cox Regression

Test datasets:
- `.rda` files: `data/` (for R package loading)
- `.omv` and `.csv` files: `data-raw/non-rda/` (for jamovi and manual testing)
- Generation script: `data-raw/create_pcacox_test_data.R` (seeds: 42/99)

---

## 1. PCA METHODS

| # | File | Variables | Options to Test |
|---|------|-----------|-----------------|
| 1 | `pcacox_clinical` | time: `time`, status: `status`, outcomeLevel: `Dead`, censorLevel: `Alive`, predictors: `age, bmi, albumin, crp, ldh, hemoglobin, wbc, platelets, tumor_size, ki67` | `pca_method=standard`, `n_components=3` |
| 2 | `pcacox_clinical` | Same | `pca_method=supervised`, `survival_weighting=TRUE` |
| 3 | `pcacox_clinical` | Same | `pca_method=supervised`, `survival_weighting=FALSE` |
| 4 | `pcacox_clinical` | Same | `pca_method=sparse`, `sparse_parameter=0.1` |
| 5 | `pcacox_clinical` | Same | `pca_method=sparse`, `sparse_parameter=0.5` |
| 6 | `pcacox_clinical` | Same | `pca_method=kernel` |

**Options covered:** `pca_method` (all 4), `survival_weighting`, `sparse_parameter`

---

## 2. COMPONENT SELECTION

| # | File | Variables | Options to Test |
|---|------|-----------|-----------------|
| 7 | `pcacox_clinical` | Same as #1 | `component_selection=fixed`, `n_components=1` |
| 8 | `pcacox_clinical` | Same | `component_selection=fixed`, `n_components=8` |
| 9 | `pcacox_clinical` | Same | `component_selection=cv`, `cv_folds=3` |
| 10 | `pcacox_clinical` | Same | `component_selection=cv`, `cv_folds=5` |
| 11 | `pcacox_clinical` | Same | `component_selection=variance`, `variance_threshold=0.6` |
| 12 | `pcacox_clinical` | Same | `component_selection=variance`, `variance_threshold=0.99` |

**Options covered:** `component_selection` (3 methods), `n_components`, `cv_folds`, `variance_threshold`

---

## 3. PREPROCESSING

| # | File | Variables | Options to Test |
|---|------|-----------|-----------------|
| 13 | `pcacox_clinical` | Same as #1 | `scaling=FALSE` |
| 14 | `pcacox_clinical` | Same | `centering=FALSE` |
| 15 | `pcacox_clinical` | Same | `scaling=FALSE`, `centering=FALSE` |
| 16 | `pcacox_clinical` | Same | `confidence_level=0.99` |

**Options covered:** `scaling`, `centering`, `confidence_level`

---

## 4. VALIDATION

| # | File | Variables | Options to Test |
|---|------|-----------|-----------------|
| 17 | `pcacox_clinical` | Same as #1 | `bootstrap_validation=TRUE`, `n_bootstrap=50` |
| 18 | `pcacox_clinical` | Same | `bootstrap_validation=FALSE` |
| 19 | `pcacox_clinical` | Same | `permutation_test=TRUE`, `n_permutations=50` |

**Options covered:** `bootstrap_validation`, `n_bootstrap`, `permutation_test`, `n_permutations`

---

## 5. DISPLAY OPTIONS

| # | File | Variables | Options to Test |
|---|------|-----------|-----------------|
| 20 | `pcacox_clinical` | Same as #1 | `suitabilityCheck=TRUE` |
| 21 | `pcacox_clinical` | Same | `suitabilityCheck=FALSE` |
| 22 | `pcacox_clinical` | Same | `show_model_comparison=TRUE` |
| 23 | `pcacox_clinical` | Same | `pathway_analysis=TRUE` |
| 24 | `pcacox_clinical` | Same | `risk_score=TRUE` |
| 25 | `pcacox_clinical` | Same | `risk_score=FALSE` |
| 26 | `pcacox_clinical` | Same | `feature_importance=TRUE` |
| 27 | `pcacox_clinical` | Same | `feature_importance=FALSE` |
| 28 | `pcacox_clinical` | Same | All `plot_*=TRUE` |
| 29 | `pcacox_clinical` | Same | All `plot_*=FALSE` |

**Options covered:** `suitabilityCheck`, `show_model_comparison`, `pathway_analysis`, `risk_score`, `feature_importance`, `plot_scree`, `plot_loadings`, `plot_biplot`, `plot_survival`

---

## 6. HIGH-DIMENSIONAL

| # | File | Variables | Options to Test |
|---|------|-----------|-----------------|
| 30 | `pcacox_genomic` | time: `time`, status: `status`, outcomeLevel: `Dead`, censorLevel: `Alive`, predictors: all 30 gene columns | `n_components=5`, `component_selection=variance`, `variance_threshold=0.8` |
| 31 | `pcacox_genomic` | Same | `pca_method=sparse`, `sparse_parameter=0.3` |

---

## 7. EDGE CASES

| # | File | Variables | Options to Test | Expected Behavior |
|---|------|-----------|-----------------|-------------------|
| 32 | -- | No data | -- | Welcome HTML displayed |
| 33 | `pcacox_clinical` | Only 1 predictor | -- | Welcome HTML (< 2 predictors) |
| 34 | `pcacox_clinical` | 2 predictors: `age, albumin` | `n_components=1` | Should work (minimum) |
| 35 | `pcacox_clinical` | All-censored status | -- | ERROR notice: no events |
| 36 | `pcacox_clinical` | Missing data (~15%) | Default | Listwise deletion, runs OK |
| 37 | `pcacox_clinical` | `n_components=50` (> available) | Default | Auto-capped |

---

## AVAILABLE TEST DATASETS

| File | N | Predictors | Events | Key Features |
|------|---|-----------|--------|--------------|
| `pcacox_genomic` | 150 | 30 (6 correlated blocks) | 72 | Gene expression simulation |
| `pcacox_clinical` | 60 | 10 (mixed clinical) | 30 | Small clinical cohort |

**File locations:**
- RDA: `data/pcacox_genomic.rda`, `data/pcacox_clinical.rda`
- CSV/OMV: `data-raw/non-rda/pcacox_*.csv` (`.omv`)

---

## COMPLETE OPTION COVERAGE CHECKLIST

- [x] `time` -- tests #1-37
- [x] `status` -- tests #1-37
- [x] `outcomeLevel` -- tests #1-37
- [x] `censorLevel` -- tests #1-37
- [x] `predictors` -- tests #1-37
- [x] `clinical_vars` -- not tested explicitly (optional)
- [x] `suitabilityCheck` -- tests #20, #21
- [x] `pca_method` (supervised) -- tests #2, #3
- [x] `pca_method` (standard) -- test #1
- [x] `pca_method` (sparse) -- tests #4, #5
- [x] `pca_method` (kernel) -- test #6
- [x] `n_components` -- tests #7, #8, #37
- [x] `component_selection` (fixed) -- tests #7, #8
- [x] `component_selection` (cv) -- tests #9, #10
- [x] `component_selection` (variance) -- tests #11, #12
- [x] `cv_folds` -- tests #9, #10
- [x] `variance_threshold` -- tests #11, #12
- [x] `sparse_parameter` -- tests #4, #5
- [x] `confidence_level` -- test #16
- [x] `scaling` -- tests #13, #15
- [x] `centering` -- tests #14, #15
- [x] `survival_weighting` -- tests #2, #3
- [x] `permutation_test` -- test #19
- [x] `n_permutations` -- test #19
- [x] `bootstrap_validation` -- tests #17, #18
- [x] `n_bootstrap` -- test #17
- [x] `plot_scree` -- tests #28, #29
- [x] `plot_loadings` -- tests #28, #29
- [x] `plot_biplot` -- tests #28, #29
- [x] `plot_survival` -- tests #28, #29
- [x] `risk_score` -- tests #24, #25
- [x] `show_model_comparison` -- test #22
- [x] `pathway_analysis` -- test #23
- [x] `feature_importance` -- tests #26, #27
