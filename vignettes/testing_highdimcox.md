# Testing Guide: highdimcox -- High-Dimensional Cox Regression

This document provides numbered test scenarios that cover every option in the
`highdimcox` analysis. Use the two bundled datasets (`highdimcox_genomic` and
`highdimcox_proteomic`) to run through these scenarios in jamovi or from R.

---

## Test Datasets

| Dataset | n | Predictors | Event Rate | outcomeLevel | Time Variable | Outcome Variable |
|---|---|---|---|---|---|---|
| `highdimcox_genomic` | 150 | 100 genes + 5 clinical | ~50 % | `"Dead"` | `survival_months` | `vital_status` |
| `highdimcox_proteomic` | 80 | 50 proteins + 3 clinical | ~65 % | `"Dead"` | `follow_up_months` | `event_status` |

Generate the datasets by running:

```r
source("data-raw/create_highdimcox_test_data.R")
```

---

## Test Scenarios

### 1. Default Run (Elastic Net, 1-SE Rule)

**Purpose:** Verify the analysis runs end-to-end with all default options.

```r
highdimcox(
  data = highdimcox_genomic,
  elapsedtime = "survival_months",
  outcome = "vital_status",
  outcomeLevel = "Dead",
  predictors = paste0("GENE_", sprintf("%03d", 1:100))
)
```

**Expected:** Elastic Net (alpha = 0.5), 10-fold CV, 1-SE lambda. All default
output sections visible (regularization path, CV plot, variable importance,
coefficients table, model diagnostics). Stability selection tables/plots hidden.

---

### 2. LASSO Regularization

**Purpose:** Test pure L1 penalty -- alpha should be forced to 1.0.

```r
highdimcox(
  data = highdimcox_genomic,
  elapsedtime = "survival_months",
  outcome = "vital_status",
  outcomeLevel = "Dead",
  predictors = paste0("GENE_", sprintf("%03d", 1:100)),
  regularization_method = "lasso"
)
```

**Check:**
- Regularization Metrics table reports alpha = 1.0.
- Fewer selected variables than ridge.
- Some coefficients are exactly zero.

---

### 3. Ridge Regularization

**Purpose:** Test pure L2 penalty -- alpha should be forced to 0.0.

```r
highdimcox(
  data = highdimcox_genomic,
  elapsedtime = "survival_months",
  outcome = "vital_status",
  outcomeLevel = "Dead",
  predictors = paste0("GENE_", sprintf("%03d", 1:100)),
  regularization_method = "ridge"
)
```

**Check:**
- Alpha reported as 0.0.
- All (or nearly all) predictors retained with non-zero coefficients.
- Coefficients are shrunk toward zero but not exactly zero.

---

### 4. Adaptive LASSO

**Purpose:** Test data-driven penalty weights.

```r
highdimcox(
  data = highdimcox_proteomic,
  elapsedtime = "follow_up_months",
  outcome = "event_status",
  outcomeLevel = "Dead",
  predictors = paste0("PROT_", sprintf("%02d", 1:50)),
  regularization_method = "adaptive_lasso"
)
```

**Check:**
- Model runs without error.
- Selected variable set may differ from standard LASSO.

---

### 5. Elastic Net with Custom Alpha

**Purpose:** Test user-specified alpha blending between L1 and L2.

```r
highdimcox(
  data = highdimcox_genomic,
  elapsedtime = "survival_months",
  outcome = "vital_status",
  outcomeLevel = "Dead",
  predictors = paste0("GENE_", sprintf("%03d", 1:100)),
  regularization_method = "elastic_net",
  alpha_value = 0.8
)
```

**Check:**
- Alpha reported as 0.8 in Regularization Metrics.
- Behavior closer to LASSO than default (alpha=0.5).

---

### 6. CV Method -- Minimum CV Error

**Purpose:** Select lambda.min instead of lambda.1se.

```r
highdimcox(
  data = highdimcox_proteomic,
  elapsedtime = "follow_up_months",
  outcome = "event_status",
  outcomeLevel = "Dead",
  predictors = paste0("PROT_", sprintf("%02d", 1:50)),
  cv_method = "cv_min"
)
```

**Check:**
- Selected Lambda matches Lambda Min in Regularization Metrics.
- Typically selects more variables than the 1-SE rule.

---

### 7. CV Method -- Standard K-Fold CV

**Purpose:** Test the cv_glmnet option path.

```r
highdimcox(
  data = highdimcox_proteomic,
  elapsedtime = "follow_up_months",
  outcome = "event_status",
  outcomeLevel = "Dead",
  predictors = paste0("PROT_", sprintf("%02d", 1:50)),
  cv_method = "cv_glmnet"
)
```

**Check:**
- Analysis completes. Lambda selection follows the 1-SE default path
  (cv_glmnet uses the same underlying mechanism as cv_1se in current
  implementation).

---

### 8. Custom Number of CV Folds

**Purpose:** Test non-default cv_folds values at the boundaries.

```r
# Minimum folds
highdimcox(
  data = highdimcox_proteomic,
  elapsedtime = "follow_up_months",
  outcome = "event_status",
  outcomeLevel = "Dead",
  predictors = paste0("PROT_", sprintf("%02d", 1:50)),
  cv_folds = 3
)

# Maximum folds (LOOCV-like)
highdimcox(
  data = highdimcox_proteomic,
  elapsedtime = "follow_up_months",
  outcome = "event_status",
  outcomeLevel = "Dead",
  predictors = paste0("PROT_", sprintf("%02d", 1:50)),
  cv_folds = 20
)
```

**Check:**
- Both extremes run without error.
- Model Summary reports the correct fold count.

---

### 9. Stability Selection (Default Parameters)

**Purpose:** Enable stability selection with default bootstrap_iterations=500
and stability_threshold=0.8.

```r
highdimcox(
  data = highdimcox_proteomic,
  elapsedtime = "follow_up_months",
  outcome = "event_status",
  outcomeLevel = "Dead",
  predictors = paste0("PROT_", sprintf("%02d", 1:50)),
  regularization_method = "lasso",
  stability_selection = TRUE
)
```

**Check:**
- Stability Selection Results table is visible.
- Stability Selection Plot is visible.
- At least some variables show selection probability >= 0.8.
- Variables ranked by selection probability.

---

### 10. Stability Selection -- Custom Threshold and Iterations

**Purpose:** Test boundary values for stability parameters.

```r
highdimcox(
  data = highdimcox_proteomic,
  elapsedtime = "follow_up_months",
  outcome = "event_status",
  outcomeLevel = "Dead",
  predictors = paste0("PROT_", sprintf("%02d", 1:50)),
  stability_selection = TRUE,
  bootstrap_iterations = 100,
  stability_threshold = 0.5
)
```

**Check:**
- Lower threshold selects more "stable" variables.
- Fewer bootstrap iterations makes analysis faster.
- Results still valid and informative.

---

### 11. Variable Selection -- Stepwise

**Purpose:** Test post-regularization stepwise variable selection.

```r
highdimcox(
  data = highdimcox_proteomic,
  elapsedtime = "follow_up_months",
  outcome = "event_status",
  outcomeLevel = "Dead",
  predictors = paste0("PROT_", sprintf("%02d", 1:50)),
  variable_selection = "stepwise"
)
```

**Check:**
- Analysis completes (or gracefully reports if not yet implemented).
- If implemented, the final model may have fewer variables than pure
  regularization.

---

### 12. Variable Selection -- Best Subset

```r
highdimcox(
  data = highdimcox_proteomic,
  elapsedtime = "follow_up_months",
  outcome = "event_status",
  outcomeLevel = "Dead",
  predictors = paste0("PROT_", sprintf("%02d", 1:20)),
  variable_selection = "best_subset"
)
```

**Check:** Same as scenario 11. Use fewer predictors since best subset is
computationally expensive.

---

### 13. Variable Selection -- Forward Selection

```r
highdimcox(
  data = highdimcox_proteomic,
  elapsedtime = "follow_up_months",
  outcome = "event_status",
  outcomeLevel = "Dead",
  predictors = paste0("PROT_", sprintf("%02d", 1:50)),
  variable_selection = "forward_selection"
)
```

---

### 14. Toggle All Plots Off

**Purpose:** Verify all plot Image items can be hidden.

```r
highdimcox(
  data = highdimcox_genomic,
  elapsedtime = "survival_months",
  outcome = "vital_status",
  outcomeLevel = "Dead",
  predictors = paste0("GENE_", sprintf("%03d", 1:100)),
  show_regularization_path = FALSE,
  show_cv_plot = FALSE,
  show_variable_importance = FALSE,
  show_model_diagnostics = FALSE
)
```

**Check:** No plot/image output. Tables still present.

---

### 15. Toggle Coefficients Table Off

**Purpose:** Verify the selected variables table and model summary can be hidden.

```r
highdimcox(
  data = highdimcox_genomic,
  elapsedtime = "survival_months",
  outcome = "vital_status",
  outcomeLevel = "Dead",
  predictors = paste0("GENE_", sprintf("%03d", 1:100)),
  show_coefficients_table = FALSE
)
```

**Check:** `selectedVariables` table and `modelSummary` html are not visible.

---

### 16. Show Analysis Summaries

**Purpose:** Test natural language summary generation.

```r
highdimcox(
  data = highdimcox_genomic,
  elapsedtime = "survival_months",
  outcome = "vital_status",
  outcomeLevel = "Dead",
  predictors = paste0("GENE_", sprintf("%03d", 1:100)),
  showSummaries = TRUE
)
```

**Check:**
- `analysisSummary` HTML output is visible.
- Contains method description, lambda value, and variable counts.

---

### 17. Show Method Explanations

**Purpose:** Test methodology documentation output.

```r
highdimcox(
  data = highdimcox_genomic,
  elapsedtime = "survival_months",
  outcome = "vital_status",
  outcomeLevel = "Dead",
  predictors = paste0("GENE_", sprintf("%03d", 1:100)),
  showExplanations = TRUE
)
```

**Check:**
- `methodExplanation` HTML output is visible.
- Contains sections on regularization methods, cross-validation, stability
  selection, and clinical interpretation.

---

### 18. Combined: Summaries + Explanations + Stability

**Purpose:** All optional output enabled simultaneously.

```r
highdimcox(
  data = highdimcox_proteomic,
  elapsedtime = "follow_up_months",
  outcome = "event_status",
  outcomeLevel = "Dead",
  predictors = paste0("PROT_", sprintf("%02d", 1:50)),
  stability_selection = TRUE,
  bootstrap_iterations = 200,
  showSummaries = TRUE,
  showExplanations = TRUE
)
```

**Check:**
- All tables, plots, summaries, and explanations render without error.
- Stability summary is included in the analysis summary text.

---

### 19. Mixed Predictor Types (Numeric + Factor)

**Purpose:** Include clinical factor variables alongside numeric gene data.

```r
highdimcox(
  data = highdimcox_genomic,
  elapsedtime = "survival_months",
  outcome = "vital_status",
  outcomeLevel = "Dead",
  predictors = c("age", "gender", "stage", "grade", "treatment",
                  paste0("GENE_", sprintf("%03d", 1:20)))
)
```

**Check:**
- Factor variables are converted to numeric internally.
- No errors from mixed types in the predictor matrix.

---

### 20. Minimal Predictors (Single Predictor)

**Purpose:** Edge case -- only one predictor variable.

```r
highdimcox(
  data = highdimcox_genomic,
  elapsedtime = "survival_months",
  outcome = "vital_status",
  outcomeLevel = "Dead",
  predictors = "GENE_003"
)
```

**Check:**
- Analysis either runs or provides a meaningful error message.
- glmnet requires at least 2 predictors; verify graceful handling.

---

### 21. All Observations Censored (Edge Case)

**Purpose:** Dataset with no events to verify error handling.

```r
genomic_no_events <- highdimcox_genomic
genomic_no_events$vital_status <- factor("Alive",
  levels = c("Alive", "Dead"))

highdimcox(
  data = genomic_no_events,
  elapsedtime = "survival_months",
  outcome = "vital_status",
  outcomeLevel = "Dead",
  predictors = paste0("GENE_", sprintf("%03d", 1:50))
)
```

**Check:**
- Informative error message about zero events.
- No unhandled crash.

---

### 22. Very Few Events (Edge Case)

**Purpose:** Dataset with only a handful of events.

```r
set.seed(42)
genomic_few_events <- highdimcox_genomic
genomic_few_events$vital_status <- factor(
  ifelse(runif(150) < 0.05, "Dead", "Alive"),
  levels = c("Alive", "Dead")
)

highdimcox(
  data = genomic_few_events,
  elapsedtime = "survival_months",
  outcome = "vital_status",
  outcomeLevel = "Dead",
  predictors = paste0("GENE_", sprintf("%03d", 1:50))
)
```

**Check:**
- Model either fits with a warning or reports insufficient events.

---

### 23. Small Sample Size (Near Minimum)

**Purpose:** Test with n close to the MIN_OBSERVATIONS threshold (30).

```r
small_data <- highdimcox_proteomic[1:35, ]

highdimcox(
  data = small_data,
  elapsedtime = "follow_up_months",
  outcome = "event_status",
  outcomeLevel = "Dead",
  predictors = paste0("PROT_", sprintf("%02d", 1:10)),
  cv_folds = 5
)
```

**Check:**
- Analysis runs (n=35 > MIN_OBSERVATIONS=30).
- Fewer CV folds appropriate for small n.

---

### 24. Below Minimum Observations (Edge Case)

**Purpose:** Verify rejection when n < 30.

```r
tiny_data <- highdimcox_proteomic[1:20, ]

highdimcox(
  data = tiny_data,
  elapsedtime = "follow_up_months",
  outcome = "event_status",
  outcomeLevel = "Dead",
  predictors = paste0("PROT_", sprintf("%02d", 1:5))
)
```

**Check:**
- Validation returns early.
- Informative error about insufficient observations.

---

### 25. Method Comparison (All Four Regularizations)

**Purpose:** Run all four methods on the same dataset and compare results.

```r
methods <- c("lasso", "ridge", "elastic_net", "adaptive_lasso")
predictors <- paste0("PROT_", sprintf("%02d", 1:50))

results <- lapply(methods, function(m) {
  highdimcox(
    data = highdimcox_proteomic,
    elapsedtime = "follow_up_months",
    outcome = "event_status",
    outcomeLevel = "Dead",
    predictors = predictors,
    regularization_method = m
  )
})
```

**Check:**
- All four complete without error.
- LASSO selects fewest variables; Ridge retains most.
- Elastic Net is intermediate.

---

### 26. Proteomic Dataset with Stability + All Outputs

**Purpose:** Full-feature stress test on the smaller dataset.

```r
highdimcox(
  data = highdimcox_proteomic,
  elapsedtime = "follow_up_months",
  outcome = "event_status",
  outcomeLevel = "Dead",
  predictors = c("age", "sex", "tumor_size_cm",
                  paste0("PROT_", sprintf("%02d", 1:50))),
  regularization_method = "elastic_net",
  alpha_value = 0.7,
  cv_method = "cv_min",
  cv_folds = 5,
  variable_selection = "none",
  stability_selection = TRUE,
  bootstrap_iterations = 200,
  stability_threshold = 0.6,
  show_regularization_path = TRUE,
  show_cv_plot = TRUE,
  show_variable_importance = TRUE,
  show_coefficients_table = TRUE,
  show_model_diagnostics = TRUE,
  showSummaries = TRUE,
  showExplanations = TRUE
)
```

**Check:**
- Every output section is populated.
- No serialization errors from plot states.
- Analysis summary mentions stability selection results.

---

## Option Coverage Checklist

| YAML Option | Type | Default | Tested In |
|---|---|---|---|
| `elapsedtime` | Variable | -- | All scenarios |
| `outcome` | Variable | -- | All scenarios |
| `predictors` | Variables | -- | All scenarios |
| `outcomeLevel` | String | `"1"` | All (overridden to `"Dead"`) |
| `regularization_method` | List | `elastic_net` | #1-5, #25 |
| `alpha_value` | Number | `0.5` | #5, #26 |
| `cv_method` | List | `cv_1se` | #1, #6, #7, #26 |
| `cv_folds` | Integer | `10` | #8, #23, #26 |
| `variable_selection` | List | `none` | #11, #12, #13 |
| `stability_selection` | Bool | `FALSE` | #9, #10, #18, #26 |
| `bootstrap_iterations` | Integer | `500` | #9, #10, #18, #26 |
| `stability_threshold` | Number | `0.8` | #9, #10, #26 |
| `show_regularization_path` | Bool | `TRUE` | #14, #26 |
| `show_cv_plot` | Bool | `TRUE` | #14, #26 |
| `show_variable_importance` | Bool | `TRUE` | #14, #26 |
| `show_coefficients_table` | Bool | `TRUE` | #15, #26 |
| `show_model_diagnostics` | Bool | `TRUE` | #14, #26 |
| `showSummaries` | Bool | `FALSE` | #16, #18, #26 |
| `showExplanations` | Bool | `FALSE` | #17, #18, #26 |

---

## Edge Cases Summary

| Scenario | Test # | Expected Behavior |
|---|---|---|
| Single predictor | #20 | Graceful error or degenerate model |
| All censored (0 events) | #21 | Informative error, no crash |
| Very few events (<5%) | #22 | Warning or reduced model |
| n near minimum (35) | #23 | Runs with fewer CV folds |
| n below minimum (<30) | #24 | Validation rejects early |
| Mixed numeric + factor predictors | #19 | Automatic conversion |
| All outputs enabled | #26 | Every section populated |
| All plots disabled | #14 | Only tables visible |
