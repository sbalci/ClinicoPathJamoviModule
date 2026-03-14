# High-Dimensional Cox Regression Documentation

This document provides a comprehensive overview of the High-Dimensional Cox
Regression module (`highdimcox`), detailing its features, user interface
elements, and the underlying R functions.

## Changelog

- **2026-03-12**: Resync with current codebase. Removed stale references to
  `variable_selection`, `bootstrap_iterations`, `dimensionalityReduction`.
  Updated column names (`beta`→`coefficient`, `HR`→`hazard_ratio`). Added
  `suitabilityCheck`, `censorLevel`, `subsampling_ratio` features. Corrected
  `cv_method` enum values. Fixed stability selection parameter names.

## Feature Summary

The `highdimcox` analysis provides regularized Cox proportional hazards
regression for survival data with many predictors. It is designed for the
classic "p close to or larger than n" scenario found in genomic, proteomic, and
other high-throughput biomedical studies. The module supports four
regularization strategies (LASSO, Ridge, Elastic Net, Adaptive LASSO),
cross-validated lambda selection, optional stability selection via bootstrap
resampling, and multiple diagnostic visualizations.

The module includes a traffic-light data suitability assessment that checks
events-per-variable ratio, regularization need, sample size, event rate,
multicollinearity, and data quality before running the main analysis.

All plot rendering uses protobuf-safe state serialization (plain numeric vectors
only — no model objects stored in state) to avoid jamovi serialization errors.

---

## Feature Details

The following table provides a detailed mapping of the module's features,
from the user interface to the underlying R functions.

| Feature | YAML Argument (`.a.yaml`) | UI Label | Results Section (`.r.yaml`) | R Function (`.b.R`) |
|---------|---------------------------|----------|----------------------------|---------------------|
| **Input Variables** | | | | |
| Time variable | `elapsedtime` | Time Variable | — (input) | `survival::Surv()` |
| Event variable | `outcome` | Event Variable | — (input) | `survival::Surv()` |
| Predictor variables | `predictors` | High-Dimensional Predictors | — (input) | predictor matrix / `model.matrix()` |
| Event level | `outcomeLevel` | Event Level | — (input) | factor → binary (1) encoding |
| Censored level | `censorLevel` | Censored Level | — (input) | factor → binary (0) encoding |
| **Data Quality** | | | | |
| Suitability check | `suitabilityCheck` | Data Suitability Assessment | `suitabilityReport` (Html) | `.assessSuitability()` → `.generateSuitabilityHtml()` |
| **Regularization Settings** | | | | |
| LASSO (L1) | `regularization_method = "lasso"` | LASSO (L1) | `regularizationMetrics` | `glmnet::cv.glmnet(alpha=1)` |
| Ridge (L2) | `regularization_method = "ridge"` | Ridge (L2) | `regularizationMetrics` | `glmnet::cv.glmnet(alpha=0)` |
| Elastic Net | `regularization_method = "elastic_net"` | Elastic Net (L1+L2) | `regularizationMetrics` | `glmnet::cv.glmnet(alpha=user)` |
| Adaptive LASSO | `regularization_method = "adaptive_lasso"` | Adaptive LASSO | `regularizationMetrics` | `glmnet::cv.glmnet()` with Ridge-derived weights |
| Alpha parameter | `alpha_value` | Elastic Net Alpha (0=Ridge, 1=LASSO) | `regularizationMetrics` | `glmnet::cv.glmnet(alpha=...)` |
| 1-SE Rule CV | `cv_method = "cv_1se"` | 1-SE Rule CV | `regularizationMetrics` | `cv_fit$lambda.1se` |
| Minimum CV Error | `cv_method = "cv_min"` | Minimum CV Error | `regularizationMetrics` | `cv_fit$lambda.min` |
| CV Folds | `cv_folds` | Number of CV Folds | `modelSummary` | `glmnet::cv.glmnet(nfolds=...)` |
| **Stability Selection** | | | | |
| Enable stability | `stability_selection` | Perform Stability Selection | `stabilityResults` (Table), `stabilityPlot` (Image) | `.performStabilitySelection()` |
| Subsampling iterations | `subsampling_iterations` | Subsampling Iterations | `stabilityResults` | bootstrap loop count |
| Subsampling ratio | `subsampling_ratio` | Subsampling Ratio (0.1–0.9) | `stabilityResults` | `floor(n * ratio)` per iteration |
| Stability threshold | `stability_threshold` | Stability Threshold | `stabilityResults` | selection probability cutoff |
| **Core Results** | | | | |
| Model summary | — (computed) | — | `modelSummary` (Html) | `.populateModelSummary()` |
| Selected variables | — (computed) | — | `selectedVariables` (Table) | `.populateVariablesTable()` |
| Regularization metrics | — (computed) | — | `regularizationMetrics` (Table) | `.populateRegularizationMetrics()` |
| Stability results | — (computed) | — | `stabilityResults` (Table) | `.populateStabilityResults()` |
| **Plots** | | | | |
| Regularization path | `show_regularization_path` | Show Regularization Path | `regularizationPath` (Image) | `.createRegularizationPath()` → `.plot_regularization_path()` |
| CV error plot | `show_cv_plot` | Show Cross-Validation Plot | `cvPlot` (Image) | `.createCVPlot()` → `.plot_cv()` |
| Variable importance | `show_variable_importance` | Show Variable Importance | `variableImportance` (Image) | `.createVariableImportancePlot()` → `.plot_variable_importance()` |
| Coefficients table | `show_coefficients_table` | Show Coefficients Table | `selectedVariables` (Table) | `.populateVariablesTable()` |
| Model diagnostics | `show_model_diagnostics` | Show Model Diagnostics | `modelDiagnostics` (Image) | `.createModelDiagnostics()` → `.plot_model_diagnostics()` |
| **Explanatory Output** | | | | |
| Analysis summaries | `showSummaries` | Analysis Summaries | `analysisSummary` (Html) | `.generateSummaries()` |
| Method explanations | `showExplanations` | Method Explanations | `methodExplanation` (Html) | `.generateExplanations()` |

---

## Internal Architecture

### Data Flow

```
User Input (jamovi UI / R wrapper)
  │
  ▼
.validateInputs()
  - Check elapsedtime, outcome, predictors are provided
  - Verify n >= MIN_OBSERVATIONS (30)
  - Verify outcomeLevel ≠ censorLevel, both have matching rows
  │
  ▼
.prepareData()
  - Extract time (must be numeric), event, predictor columns
  - Encode event: outcomeLevel → 1, censorLevel → 0, other → NA (excluded)
  - Complete-case filter BEFORE model.matrix() (critical for factors)
  - Dummy-encode factor predictors via model.matrix(~ . - 1)
  - Remove constant (zero-variance) predictor columns
  - Build display name mapping (model.matrix names → original names)
  - Return: list(survival, predictors, time, event, n_obs, n_vars, var_names,
                 n_excluded_outcome, n_na_outcome, n_constant_removed)
  │
  ▼
[Optional] .assessSuitability()
  - 6 traffic-light checks: EPV, regularization need, sample size,
    event rate, multicollinearity, data quality
  - Generates styled HTML table in suitabilityReport
  │
  ▼
.performHighDimCoxRegression()
  - Set alpha based on regularization_method:
      lasso → 1.0, ridge → 0.0, elastic_net → user alpha,
      adaptive_lasso → 1.0 (with Ridge-derived penalty weights)
  - Run glmnet::cv.glmnet(family="cox", alpha, nfolds=cv_folds)
  - Select lambda: cv_min → lambda.min, cv_1se → lambda.1se
  - Fit full path with glmnet::glmnet()
  - Extract coefficients at selected lambda
  - Calculate training C-index via survival::concordance(reverse=TRUE)
  - Return: list(cv_fit, final_fit, selected_lambda, coefficients,
                  selected_variables, variable_importance, alpha,
                  n_selected, concordance)
  │
  ▼
[Optional] .performStabilitySelection()
  - Fixed lambda from full-data CV (Meinshausen & Buhlmann 2010)
  - Ridge → bumped to alpha=0.5 (ridge has no selection)
  - Loop subsampling_iterations times:
      subsample floor(n * subsampling_ratio) without replacement
      fit glmnet() at fixed lambda
      record non-zero coefficients
  - Compute selection probabilities from successful iterations
  - Flag variables with prob >= stability_threshold as "stable"
  - Return: list(selection_probabilities, stable_variables, n_stable, ...)
  │
  ▼
.populateResults()
  - .populateModelSummary()           → modelSummary (Html)
  - .populateVariablesTable()         → selectedVariables (Table)
  - .populateRegularizationMetrics()  → regularizationMetrics (Table)
  - .populateStabilityResults()       → stabilityResults (Table)
  │
  ▼
.createPlots()
  - .createRegularizationPath()       → regularizationPath (Image)
  - .createCVPlot()                   → cvPlot (Image)
  - .createVariableImportancePlot()   → variableImportance (Image)
  - .createModelDiagnostics()         → modelDiagnostics (Image)
  - .createStabilityPlot()            → stabilityPlot (Image)
  │
  ▼
[Optional] .generateSummaries()    → analysisSummary (Html)
[Optional] .generateExplanations() → methodExplanation (Html)
```

---

## State Management for Plots

All plot Image items use protobuf-safe state serialization. The backend
extracts only plain numeric vectors and character vectors from model objects
before calling `image$setState()`. This avoids serialization errors caused by
storing R6/environment/function references in state.

Key rules:

1. Always wrap in `tryCatch` so a single plot failure does not crash the analysis.
2. Cast every value to `as.numeric()` or `as.character()` — never store raw model objects.
3. Do not store the full `cv.glmnet` or `glmnet` object in state.

---

## Dependencies

| Package | Purpose |
|---|---|
| `survival` | `Surv()`, `concordance()` |
| `glmnet` | `cv.glmnet()`, `glmnet()` for regularized Cox |
| `jmvcore` | jamovi framework |
| `R6` | R6 class system |
| `ggplot2` | Plot rendering (loaded at render time) |

---

## File Locations

| File | Purpose |
|---|---|
| `jamovi/highdimcox.a.yaml` | Analysis definition |
| `jamovi/highdimcox.u.yaml` | UI definition |
| `jamovi/highdimcox.r.yaml` | Results definition |
| `R/highdimcox.h.R` | Auto-generated header |
| `R/highdimcox.b.R` | Backend implementation |
| `data/highdimcox_genomic.rda` | Genomic test dataset (n=150, 100 genes + 5 clinical) |
| `data/highdimcox_proteomic.rda` | Proteomic test dataset (n=80, 50 proteins + 3 clinical) |
| `data-raw/create_highdimcox_test_data.R` | Test data generation script |
| `tests/testthat/test-highdimcox-basic.R` | Basic functionality tests (17 tests) |
| `tests/testthat/test-highdimcox-arguments.R` | Argument combination tests (16 tests) |
| `tests/testthat/test-highdimcox-edge-cases.R` | Edge case tests (13 tests) |
| `tests/testthat/test-highdimcox-integration.R` | Integration tests (22 tests) |
| `inst/examples/highdimcox_example.R` | Example usage code |

---

## Results Output Reference

### Tables

| Name | Title | Visible When | Columns |
|---|---|---|---|
| `selectedVariables` | Selected Variables | `show_coefficients_table` | variable (text), coefficient (number), hazard_ratio (number), importance_score (number) |
| `regularizationMetrics` | Regularization Metrics | always | metric (text), value (text), interpretation (text) |
| `stabilityResults` | Stability Selection Results | `stability_selection` | variable (text), selection_probability (number), stable (text), importance_rank (integer) |

### Images (Plots)

| Name | Title | Visible When | renderFun |
|---|---|---|---|
| `regularizationPath` | Regularization Path | `show_regularization_path` | `.plot_regularization_path` |
| `cvPlot` | Cross-Validation Plot | `show_cv_plot` | `.plot_cv` |
| `variableImportance` | Variable Importance Plot | `show_variable_importance` | `.plot_variable_importance` |
| `modelDiagnostics` | Model Diagnostics | `show_model_diagnostics` | `.plot_model_diagnostics` |
| `stabilityPlot` | Stability Selection Plot | `stability_selection` | `.plot_stability` |

### HTML Items

| Name | Title | Visible When |
|---|---|---|
| `todo` | Analysis | always (cleared when analysis runs) |
| `suitabilityReport` | Data Suitability Assessment | `suitabilityCheck` |
| `modelSummary` | Model Summary | always |
| `analysisSummary` | Analysis Summary | `showSummaries` |
| `methodExplanation` | Methodology | `showExplanations` |
