# Testing Guide: highdimcox — High-Dimensional Cox Regression

All test datasets are in `data/` (RDA). Generate them by running:

```r
source("data-raw/create_highdimcox_test_data.R")
```

Automated tests (68 total) are in `tests/testthat/test-highdimcox-*.R`.

## Changelog

- **2026-03-12**: Resync with current `.a.yaml`. Removed stale references to
  `variable_selection`, `bootstrap_iterations`, `cv_glmnet`. Added
  `suitabilityCheck`, `censorLevel`, `subsampling_ratio` scenarios. Updated
  option names throughout. Added data suitability test section.

---

## Test Datasets

| Dataset | n | Predictors | Event Rate | outcomeLevel | censorLevel | Time Variable | Outcome Variable |
|---|---|---|---|---|---|---|---|
| `highdimcox_genomic` | 150 | 100 genes + 5 clinical | ~50% | `"Dead"` | `"Alive"` | `survival_months` | `vital_status` |
| `highdimcox_proteomic` | 80 | 50 proteins + 3 clinical | ~65% | `"Dead"` | `"Alive"` | `follow_up_months` | `event_status` |

---

## Test Scenarios

### 1. Default Run (Elastic Net, 1-SE Rule)

| # | File | Variables | Options to Test |
|---|------|-----------|-----------------|
| 1 | `highdimcox_genomic` | time: `survival_months`, event: `vital_status`, predictors: `GENE_001`–`GENE_100` | All defaults (`regularization_method="elastic_net"`, `alpha_value=0.5`, `cv_method="cv_1se"`, `cv_folds=10`) |

**Expected:** Elastic Net (alpha=0.5), 10-fold CV, 1-SE lambda. Default outputs visible: variable importance plot, coefficients table, regularization metrics, model summary. Suitability report visible. Stability selection tables/plots hidden.

**Options covered:** `elapsedtime`, `outcome`, `predictors`, `outcomeLevel`, `regularization_method`, `alpha_value`, `cv_method`, `cv_folds`, `suitabilityCheck`, `show_variable_importance`, `show_coefficients_table`

---

### 2. LASSO Regularization

| # | File | Variables | Options to Test |
|---|------|-----------|-----------------|
| 2 | `highdimcox_genomic` | time: `survival_months`, event: `vital_status`, predictors: `GENE_001`–`GENE_100` | `regularization_method="lasso"` |

**Check:**
- Regularization Metrics table reports alpha = 1.0
- Fewer selected variables than ridge
- Some coefficients exactly zero

---

### 3. Ridge Regularization

| # | File | Variables | Options to Test |
|---|------|-----------|-----------------|
| 3 | `highdimcox_genomic` | time: `survival_months`, event: `vital_status`, predictors: `GENE_001`–`GENE_100` | `regularization_method="ridge"` |

**Check:**
- Alpha reported as 0.0
- All (or nearly all) predictors retained with non-zero coefficients
- Note added about Ridge not performing variable selection

---

### 4. Adaptive LASSO

| # | File | Variables | Options to Test |
|---|------|-----------|-----------------|
| 4 | `highdimcox_proteomic` | time: `follow_up_months`, event: `event_status`, predictors: `PROT_01`–`PROT_50` | `regularization_method="adaptive_lasso"` |

**Check:**
- Model runs without error
- Internal Ridge fit produces adaptive weights
- Selected variable set may differ from standard LASSO

---

### 5. Elastic Net with Custom Alpha

| # | File | Variables | Options to Test |
|---|------|-----------|-----------------|
| 5 | `highdimcox_genomic` | time: `survival_months`, event: `vital_status`, predictors: `GENE_001`–`GENE_100` | `regularization_method="elastic_net"`, `alpha_value=0.8` |

**Check:**
- Alpha reported as 0.8 in Regularization Metrics
- Behavior closer to LASSO than default (alpha=0.5)

---

### 6. CV Method — Minimum CV Error

| # | File | Variables | Options to Test |
|---|------|-----------|-----------------|
| 6 | `highdimcox_proteomic` | time: `follow_up_months`, event: `event_status`, predictors: `PROT_01`–`PROT_50` | `cv_method="cv_min"` |

**Check:**
- Selected Lambda matches Lambda Min in Regularization Metrics
- Typically selects more variables than the 1-SE rule

---

### 7. Custom Number of CV Folds

| # | File | Variables | Options to Test |
|---|------|-----------|-----------------|
| 7a | `highdimcox_proteomic` | same as #6 | `cv_folds=3` |
| 7b | `highdimcox_proteomic` | same as #6 | `cv_folds=20` |

**Check:**
- Both extremes run without error
- Model Summary reports the correct fold count

---

### 8. Stability Selection (Default Parameters)

| # | File | Variables | Options to Test |
|---|------|-----------|-----------------|
| 8 | `highdimcox_proteomic` | time: `follow_up_months`, event: `event_status`, predictors: `PROT_01`–`PROT_50` | `regularization_method="lasso"`, `stability_selection=TRUE`, `subsampling_iterations=500`, `stability_threshold=0.8` |

**Check:**
- Stability Selection Results table visible
- Stability Selection Plot visible
- At least some variables with selection probability >= 0.8
- Variables ranked by selection probability

---

### 9. Stability Selection — Custom Parameters

| # | File | Variables | Options to Test |
|---|------|-----------|-----------------|
| 9 | `highdimcox_proteomic` | same as #8 | `stability_selection=TRUE`, `subsampling_iterations=100`, `subsampling_ratio=0.6`, `stability_threshold=0.5` |

**Check:**
- Lower threshold selects more "stable" variables
- Fewer iterations completes faster
- Custom subsampling ratio applied (60% per iteration)

---

### 10. Data Suitability Assessment

| # | File | Variables | Options to Test |
|---|------|-----------|-----------------|
| 10a | `highdimcox_genomic` | all 100 genes | `suitabilityCheck=TRUE` (default) |
| 10b | `highdimcox_genomic` | all 100 genes | `suitabilityCheck=FALSE` |

**Check:**
- 10a: Suitability report visible with 6 checks (EPV, regularization need, sample size, event rate, multicollinearity, data quality)
- 10b: Suitability report hidden

---

### 11. Toggle All Plots Off

| # | File | Variables | Options to Test |
|---|------|-----------|-----------------|
| 11 | `highdimcox_genomic` | `GENE_001`–`GENE_100` | `show_regularization_path=FALSE`, `show_cv_plot=FALSE`, `show_variable_importance=FALSE`, `show_model_diagnostics=FALSE` |

**Check:** No plot/image output. Tables still present.

---

### 12. Toggle Coefficients Table Off

| # | File | Variables | Options to Test |
|---|------|-----------|-----------------|
| 12 | `highdimcox_genomic` | `GENE_001`–`GENE_100` | `show_coefficients_table=FALSE` |

**Check:** `selectedVariables` table not visible.

---

### 13. Show Analysis Summaries

| # | File | Variables | Options to Test |
|---|------|-----------|-----------------|
| 13 | `highdimcox_genomic` | `GENE_001`–`GENE_100` | `showSummaries=TRUE` |

**Check:**
- `analysisSummary` HTML output visible
- Contains method description, lambda value, variable counts, C-index

---

### 14. Show Method Explanations

| # | File | Variables | Options to Test |
|---|------|-----------|-----------------|
| 14 | `highdimcox_genomic` | `GENE_001`–`GENE_100` | `showExplanations=TRUE` |

**Check:**
- `methodExplanation` HTML output visible
- Contains sections on regularization methods, CV, stability selection, clinical interpretation

---

### 15. Combined: Summaries + Explanations + Stability

| # | File | Variables | Options to Test |
|---|------|-----------|-----------------|
| 15 | `highdimcox_proteomic` | `PROT_01`–`PROT_50` | `stability_selection=TRUE`, `subsampling_iterations=200`, `showSummaries=TRUE`, `showExplanations=TRUE` |

**Check:**
- All tables, plots, summaries, and explanations render without error
- Stability summary included in analysis summary text

---

### 16. Mixed Predictor Types (Numeric + Factor)

| # | File | Variables | Options to Test |
|---|------|-----------|-----------------|
| 16 | `highdimcox_genomic` | time: `survival_months`, event: `vital_status`, predictors: `age`, `gender`, `stage`, `grade`, `treatment`, `GENE_001`–`GENE_020` | default |

**Check:**
- Factor variables dummy-encoded internally via `model.matrix()`
- No errors from mixed types
- Display names show "stage: II", "stage: III" for factor dummies

---

### 17. Minimal Predictors (Single Predictor)

| # | File | Variables | Options to Test |
|---|------|-----------|-----------------|
| 17 | `highdimcox_genomic` | time: `survival_months`, event: `vital_status`, predictors: `GENE_003` | default |

**Check:**
- Analysis either runs or provides a meaningful error
- glmnet requires at least 2 columns; verify graceful handling

---

### 18. All Observations Censored (Zero Events)

| # | File | Variables | Options to Test |
|---|------|-----------|-----------------|
| 18 | Modified `highdimcox_genomic` (all `vital_status = "Alive"`) | predictors: `GENE_001`–`GENE_050` | default |

**Check:**
- Validation error: "No rows match event level 'Dead'"
- No unhandled crash

---

### 19. Very Few Events (<5%)

| # | File | Variables | Options to Test |
|---|------|-----------|-----------------|
| 19 | Modified `highdimcox_genomic` (~5% events) | predictors: `GENE_001`–`GENE_050` | default |

**Check:**
- Warning about low events displayed in todo HTML
- Model either fits or reports insufficient events

---

### 20. Small Sample Size (Near Minimum)

| # | File | Variables | Options to Test |
|---|------|-----------|-----------------|
| 20 | `highdimcox_proteomic[1:35, ]` | predictors: `PROT_01`–`PROT_10`, `cv_folds=5` | default |

**Check:**
- Analysis runs (n=35 > MIN_OBSERVATIONS=30)
- Fewer CV folds appropriate for small n
- Suitability report flags small sample size (yellow)

---

### 21. Below Minimum Observations

| # | File | Variables | Options to Test |
|---|------|-----------|-----------------|
| 21 | `highdimcox_proteomic[1:20, ]` | predictors: `PROT_01`–`PROT_05` | default |

**Check:**
- Validation returns early with error about insufficient observations (<30)
- Informative error message displayed

---

### 22. Outcome Level Validation

| # | File | Variables | Options to Test |
|---|------|-----------|-----------------|
| 22a | `highdimcox_genomic` | `outcomeLevel="Dead"`, `censorLevel="Dead"` (same level) | default |
| 22b | `highdimcox_genomic` | `outcomeLevel="NonExistent"` | default |

**Check:**
- 22a: Validation error — levels must be different
- 22b: Validation error — no rows match event level

---

### 23. Missing Data in Predictors

| # | File | Variables | Options to Test |
|---|------|-----------|-----------------|
| 23 | Modified `highdimcox_genomic` (20% NA in GENE_001–GENE_005) | all genes | default |

**Check:**
- Rows with missing values excluded (complete-case analysis)
- Warning about excluded rows in todo HTML
- Suitability report flags data quality issue

---

### 24. Constant Predictors

| # | File | Variables | Options to Test |
|---|------|-----------|-----------------|
| 24 | Modified `highdimcox_genomic` (GENE_001–GENE_005 all set to constant) | all genes | default |

**Check:**
- Constant columns removed before model fitting
- Warning about removed constant predictors
- Analysis proceeds with remaining predictors

---

### 25. Method Comparison (All Four Regularizations)

| # | File | Variables | Options to Test |
|---|------|-----------|-----------------|
| 25 | `highdimcox_proteomic` | predictors: `PROT_01`–`PROT_50` | `regularization_method` = each of `lasso`, `ridge`, `elastic_net`, `adaptive_lasso` |

**Check:**
- All four complete without error
- LASSO selects fewest variables; Ridge retains most
- Elastic Net intermediate

---

### 26. Full-Feature Stress Test

| # | File | Variables | Options to Test |
|---|------|-----------|-----------------|
| 26 | `highdimcox_proteomic` | predictors: `age`, `sex`, `tumor_size_cm`, `PROT_01`–`PROT_50` | `regularization_method="elastic_net"`, `alpha_value=0.7`, `cv_method="cv_min"`, `cv_folds=5`, `stability_selection=TRUE`, `subsampling_iterations=200`, `subsampling_ratio=0.6`, `stability_threshold=0.6`, all show_* = TRUE, `showSummaries=TRUE`, `showExplanations=TRUE`, `suitabilityCheck=TRUE` |

**Check:**
- Every output section populated
- No serialization errors from plot states
- Analysis summary mentions stability selection results
- Suitability report includes all 6 checks

---

### 27. p >> n Scenario (More Predictors Than Observations)

| # | File | Variables | Options to Test |
|---|------|-----------|-----------------|
| 27 | `highdimcox_proteomic` (n=80) | all 50 proteins + all 3 clinical = 53 predictors + dummy-encoded | `regularization_method="elastic_net"`, `cv_folds=5` |

**Check:**
- Analysis handles high p/n ratio gracefully
- Suitability report flags high-dimensional setting (green for regularization need)

---

## COMPLETE OPTION COVERAGE CHECKLIST

- [x] `elapsedtime` — all scenarios
- [x] `outcome` — all scenarios
- [x] `predictors` — all scenarios
- [x] `outcomeLevel` — all scenarios, edge cases #18, #22
- [x] `censorLevel` — #22a
- [x] `suitabilityCheck` — #10a (true), #10b (false)
- [x] `regularization_method` — #1 (elastic_net), #2 (lasso), #3 (ridge), #4 (adaptive_lasso), #25 (all four)
- [x] `alpha_value` — #5 (0.8), #26 (0.7)
- [x] `cv_method` — #1 (cv_1se), #6 (cv_min), #26 (cv_min)
- [x] `cv_folds` — #7a (3), #7b (20), #20 (5), #26 (5)
- [x] `stability_selection` — #8 (true), #9 (true), #15 (true), #26 (true)
- [x] `subsampling_iterations` — #8 (500), #9 (100), #15 (200), #26 (200)
- [x] `subsampling_ratio` — #9 (0.6), #26 (0.6)
- [x] `stability_threshold` — #8 (0.8), #9 (0.5), #26 (0.6)
- [x] `show_regularization_path` — #11 (false), #26 (true)
- [x] `show_cv_plot` — #11 (false), #26 (true)
- [x] `show_variable_importance` — #11 (false), default (true)
- [x] `show_coefficients_table` — #12 (false), default (true)
- [x] `show_model_diagnostics` — #11 (false), #26 (true)
- [x] `showSummaries` — #13 (true), #15 (true), #26 (true)
- [x] `showExplanations` — #14 (true), #15 (true), #26 (true)

---

## Edge Cases Summary

| Scenario | Test # | Expected Behavior |
|---|---|---|
| Single predictor | #17 | Graceful error or degenerate model |
| All censored (0 events) | #18 | Informative validation error |
| Very few events (<5%) | #19 | Warning in todo HTML |
| n near minimum (35) | #20 | Runs with fewer CV folds |
| n below minimum (<30) | #21 | Validation rejects early |
| Same event/censor level | #22a | Validation error |
| Invalid outcome level | #22b | Validation error |
| Missing data in predictors | #23 | Complete-case exclusion, data quality warning |
| Constant predictors | #24 | Removed, warning, analysis proceeds |
| Mixed numeric + factor predictors | #16 | Automatic dummy encoding |
| All outputs enabled | #26 | Every section populated |
| All plots disabled | #11 | Only tables visible |
| p >> n | #27 | Handled by regularization |
