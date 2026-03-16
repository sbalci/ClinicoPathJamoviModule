# Testing Group LASSO Cox Regression

Test datasets are stored as follows:
- `.rda` files: `data/` (for R package loading)
- `.omv` and `.csv` files: `data-raw/non-rda/` (for jamovi and manual testing)
- Generation script: `data-raw/create_grouplasso_test_data.R` (seeds: 42/123/99)

---

## 1. STANDARD CLINICAL — Biomarker Panel

| # | File | Variables | Options to Test |
|---|------|-----------|-----------------|
| 1 | `grouplasso_biomarker` | time: `time`, event: `status`, outcomeLevel: `Dead`, censorLevel: `Alive`, predictors: `age, bmi, tumor_size, grade, lvi, er, pr, her2, ki67, albumin, ldh, crp, chemo, radiation, hormonal` | Default settings: `group_definition=automatic`, `factor_grouping=TRUE`, `penalty_type=group_lasso`, `group_weights=sqrt_size`, `cv_folds=10` |
| 2 | `grouplasso_biomarker` | Same variables | `penalty_type=group_mcp` |
| 3 | `grouplasso_biomarker` | Same variables | `penalty_type=group_scad` |
| 4 | `grouplasso_biomarker` | Same variables | `penalty_type=adaptive_group`, `adaptive_weights_method=ridge` |
| 5 | `grouplasso_biomarker` | Same variables | `penalty_type=adaptive_group`, `adaptive_weights_method=univariate` |
| 6 | `grouplasso_biomarker` | Same variables | `group_definition=factor_based` |
| 7 | `grouplasso_biomarker` | Same variables | `group_definition=custom`, `group_structure="age:1, bmi:1, tumor_size:2, grade:2, lvi:2, er:3, pr:3, her2:3, ki67:3, albumin:4, ldh:4, crp:4, chemo:5, radiation:5, hormonal:5"` |
| 8 | `grouplasso_biomarker` | Same variables | `group_weights=equal` |
| 9 | `grouplasso_biomarker` | Same variables | `group_weights=group_size` |
| 10 | `grouplasso_biomarker` | Same variables | `group_weights=custom`, `custom_weights="1.0, 1.5, 2.0, 0.5, 1.0"` (5 groups) |

**Options covered:** `time`, `event`, `outcomeLevel`, `censorLevel`, `predictors`, `group_definition`, `factor_grouping`, `penalty_type`, `group_weights`, `custom_weights`, `adaptive_weights_method`

---

## 2. HIGH-DIMENSIONAL — Genomic Pathway Analysis

| # | File | Variables | Options to Test |
|---|------|-----------|-----------------|
| 11 | `grouplasso_genomic` | time: `time`, event: `status`, outcomeLevel: `Progressed`, censorLevel: `Stable`, predictors: `CCND1, CCNE1, CDK4, CDK6, RB1, PIK3CA, AKT1, PTEN, MTOR, TSC1, TP53, MDM2, ATM, CHEK2, CDKN2A, KRAS, BRAF, MAP2K1, ERK1, ERK2, BCL2, BAX, BIRC5, CASP3, CASP8, VEGFA, FLT1, KDR, ANGPT1, ANGPT2` | `group_definition=custom`, `group_structure="CCND1:1, CCNE1:1, CDK4:1, CDK6:1, RB1:1, PIK3CA:2, AKT1:2, PTEN:2, MTOR:2, TSC1:2, TP53:3, MDM2:3, ATM:3, CHEK2:3, CDKN2A:3, KRAS:4, BRAF:4, MAP2K1:4, ERK1:4, ERK2:4, BCL2:5, BAX:5, BIRC5:5, CASP3:5, CASP8:5, VEGFA:6, FLT1:6, KDR:6, ANGPT1:6, ANGPT2:6"` |
| 12 | `grouplasso_genomic` | Same variables | `stability_selection=TRUE`, `bootstrap_samples=50`, `stability_threshold=0.6` |
| 13 | `grouplasso_genomic` | Same variables | `nested_cv=TRUE`, `inner_cv_folds=3` |
| 14 | `grouplasso_genomic` | Same variables | `permutation_test=TRUE`, `n_permutations=50` |
| 15 | `grouplasso_genomic` | Same variables | `cv_folds=5`, `n_lambda=50`, `lambda_min_ratio=0.01` |

**Options covered:** `group_structure` (pathway grouping), `stability_selection`, `bootstrap_samples`, `stability_threshold`, `nested_cv`, `inner_cv_folds`, `permutation_test`, `n_permutations`, `cv_folds`, `n_lambda`, `lambda_min_ratio`

---

## 3. SMALL SAMPLE — Clinical Cohort

| # | File | Variables | Options to Test |
|---|------|-----------|-----------------|
| 16 | `grouplasso_small` | time: `time`, event: `status`, outcomeLevel: `Dead`, censorLevel: `Alive`, predictors: `age, ecog, tumor_size, grade, hemoglobin, wbc, platelets, ldh` | Default settings, `cv_folds=5` (small n) |
| 17 | `grouplasso_small` | Same variables | `group_definition=custom`, `group_structure="age:1, ecog:1, tumor_size:2, grade:2, hemoglobin:3, wbc:3, platelets:3, ldh:3"` |
| 18 | `grouplasso_small` | Same variables | `suitabilityCheck=TRUE` (expect yellow/red indicators) |
| 19 | `grouplasso_small` | Same variables | `suitabilityCheck=FALSE` |

**Options covered:** `suitabilityCheck` (small sample warnings), `cv_folds` (reduced for small data)

---

## 4. DISPLAY AND OUTPUT OPTIONS

| # | File | Variables | Options to Test |
|---|------|-----------|-----------------|
| 20 | `grouplasso_biomarker` | Same as #1 | `show_group_summary=TRUE`, `show_coefficients=TRUE`, `show_path_summary=TRUE`, `show_cv_results=TRUE` (all defaults) |
| 21 | `grouplasso_biomarker` | Same as #1 | `show_group_summary=FALSE`, `show_coefficients=FALSE`, `show_path_summary=FALSE`, `show_cv_results=FALSE` (all tables hidden) |
| 22 | `grouplasso_biomarker` | Same as #1 | `plot_regularization_path=TRUE`, `plot_cv_curve=TRUE`, `plot_group_importance=TRUE` (default plots) |
| 23 | `grouplasso_biomarker` | Same as #1 | `plot_stability=TRUE` with `stability_selection=TRUE` |
| 24 | `grouplasso_biomarker` | Same as #1 | `plot_group_structure=TRUE` |
| 25 | `grouplasso_biomarker` | Same as #1 | `showSummary=TRUE` |
| 26 | `grouplasso_biomarker` | Same as #1 | `showExplanations=TRUE` |
| 27 | `grouplasso_biomarker` | Same as #1 | All plots and tables disabled: `show_*=FALSE`, `plot_*=FALSE` |

**Options covered:** `show_group_summary`, `show_coefficients`, `show_path_summary`, `show_cv_results`, `plot_regularization_path`, `plot_cv_curve`, `plot_group_importance`, `plot_stability`, `plot_group_structure`, `showSummary`, `showExplanations`

---

## 5. ADVANCED SETTINGS

| # | File | Variables | Options to Test |
|---|------|-----------|-----------------|
| 28 | `grouplasso_biomarker` | Same as #1 | `max_iterations=100` (low — may not converge) |
| 29 | `grouplasso_biomarker` | Same as #1 | `max_iterations=50000` |
| 30 | `grouplasso_biomarker` | Same as #1 | `tolerance=1e-6` (tight convergence) |
| 31 | `grouplasso_biomarker` | Same as #1 | `tolerance=0.01` (loose convergence) |
| 32 | `grouplasso_biomarker` | Same as #1 | `selection_threshold=1e-4` (higher threshold — fewer variables "selected") |
| 33 | `grouplasso_biomarker` | Same as #1 | `standardize=FALSE` |
| 34 | `grouplasso_biomarker` | Same as #1 | `random_seed=42` (different seed → different CV folds) |
| 35 | `grouplasso_biomarker` | Same as #1 | `random_seed=999` |

**Options covered:** `max_iterations`, `tolerance`, `selection_threshold`, `standardize`, `random_seed`

---

## 6. EDGE CASES

| # | File | Variables | Options to Test | Expected Behavior |
|---|------|-----------|-----------------|-------------------|
| 36 | — | No data loaded | — | Instructions HTML displayed |
| 37 | `grouplasso_biomarker` | time: `time`, event: not set | — | Instructions HTML displayed |
| 38 | `grouplasso_biomarker` | time: `time`, event: `status`, predictors: empty | — | Early return (no error) |
| 39 | `grouplasso_small` | Only 2 continuous predictors: `age, tumor_size` | Default settings | Minimal groups, should still work |
| 40 | `grouplasso_biomarker` | All 15 predictors | `group_definition=custom`, `group_structure=""` (empty string) | Falls back to individual groups |
| 41 | `grouplasso_biomarker` | Same as #1 | `group_weights=custom`, `custom_weights=""` (empty) | Falls back to equal weights |
| 42 | `grouplasso_biomarker` | Same as #1 | `group_weights=custom`, `custom_weights="1,2"` (wrong count for 5+ groups) | Warning: weight mismatch, fallback to equal |
| 43 | `grouplasso_biomarker` | Same as #1 | `stability_selection=TRUE`, `bootstrap_samples=50`, `stability_threshold=0.95` (very strict) | Few or no stable groups |
| 44 | `grouplasso_genomic` | Same as #11 | `n_lambda=10` (very few lambda values) | Coarse regularization path |
| 45 | `grouplasso_genomic` | Same as #11 | `cv_folds=3` (minimum folds) | Wider CV error bars |

---

## 7. CROSS-VALIDATION EDGE CASES

| # | File | Variables | Options to Test | Expected Behavior |
|---|------|-----------|-----------------|-------------------|
| 46 | `grouplasso_small` | Same as #16 | `cv_folds=20` (near n) | May fail or produce unstable results |
| 47 | `grouplasso_biomarker` | Same as #1 | `nested_cv=TRUE`, `inner_cv_folds=10`, `cv_folds=10` | Very heavy computation |
| 48 | `grouplasso_small` | Same as #16 | `permutation_test=TRUE`, `n_permutations=50` | Should complete but p-values may be imprecise |

---

## AVAILABLE TEST DATASETS

| File | N | Predictors | Events | Key Features | Groups |
|------|---|-----------|--------|--------------|--------|
| `grouplasso_biomarker` | 200 | 15 (mixed numeric + factor) | ~90–110 | Breast cancer, 5 clinical domains | Demographics, Tumor, Biomarkers, Lab, Treatment |
| `grouplasso_genomic` | 120 | 30 (all continuous) | ~50–70 | Gene expression, 6 pathways | Cell cycle, PI3K/AKT, p53, RAS/MAPK, Apoptosis, Angiogenesis |
| `grouplasso_small` | 60 | 8 (mixed) | ~25–35 | Small cohort, 3 groups | Clinical, Pathology, Lab |

**File locations:**
- RDA: `data/grouplasso_biomarker.rda`, `data/grouplasso_genomic.rda`, `data/grouplasso_small.rda`
- CSV/OMV: `data-raw/non-rda/grouplasso_biomarker.csv` (`.omv`), etc.

---

## COMPLETE OPTION COVERAGE CHECKLIST

### Input Variables
- [x] `time` — tests #1–48
- [x] `event` — tests #1–48
- [x] `outcomeLevel` — tests #1–48
- [x] `censorLevel` — tests #1–48
- [x] `predictors` — tests #1–48

### Data Suitability
- [x] `suitabilityCheck` — tests #18, #19

### Group Definition
- [x] `group_definition` (automatic) — test #1
- [x] `group_definition` (factor_based) — test #6
- [x] `group_definition` (custom) — tests #7, #11, #17
- [x] `group_structure` — tests #7, #11, #17, #40
- [x] `factor_grouping` — test #1 (TRUE), #6 (implicit)

### Penalty
- [x] `penalty_type` (group_lasso) — test #1
- [x] `penalty_type` (group_mcp) — test #2
- [x] `penalty_type` (group_scad) — test #3
- [x] `penalty_type` (adaptive_group) — tests #4, #5
- [x] `group_weights` (equal) — test #8
- [x] `group_weights` (sqrt_size) — test #1
- [x] `group_weights` (group_size) — test #9
- [x] `group_weights` (custom) — tests #10, #41, #42
- [x] `custom_weights` — tests #10, #41, #42
- [x] `adaptive_weights_method` (ridge) — test #4
- [x] `adaptive_weights_method` (univariate) — test #5

### Cross-Validation
- [x] `cv_folds` — tests #15, #16, #45, #46
- [x] `n_lambda` — tests #15, #44
- [x] `lambda_min_ratio` — test #15

### Advanced Validation
- [x] `stability_selection` — tests #12, #23, #43
- [x] `bootstrap_samples` — tests #12, #43
- [x] `stability_threshold` — tests #12, #43
- [x] `nested_cv` — tests #13, #47
- [x] `inner_cv_folds` — tests #13, #47
- [x] `permutation_test` — tests #14, #48
- [x] `n_permutations` — tests #14, #48

### Algorithm
- [x] `max_iterations` — tests #28, #29
- [x] `tolerance` — tests #30, #31
- [x] `selection_threshold` — test #32
- [x] `standardize` — test #33
- [x] `random_seed` — tests #34, #35

### Display
- [x] `show_group_summary` — tests #20, #21
- [x] `show_coefficients` — tests #20, #21
- [x] `show_path_summary` — tests #20, #21
- [x] `show_cv_results` — tests #20, #21
- [x] `plot_regularization_path` — tests #22, #27
- [x] `plot_cv_curve` — tests #22, #27
- [x] `plot_group_importance` — tests #22, #27
- [x] `plot_stability` — test #23
- [x] `plot_group_structure` — test #24
- [x] `showSummary` — test #25
- [x] `showExplanations` — test #26
