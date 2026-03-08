# PLS-Cox Regression Documentation

This document provides a comprehensive overview of the PLS-Cox Regression module, detailing its features, user interface elements, and the underlying R functions.

## Feature Summary

The PLS-Cox Regression module (`plscox`) performs Partial Least Squares dimensionality reduction combined with Cox proportional hazards modeling for high-dimensional survival data. It is designed for clinicopathological research with genomic, proteomic, or metabolomic datasets where the number of predictors may greatly exceed the number of observations.

The module's features can be broadly categorized as follows:

* **Dimensionality Reduction:** PLS extracts latent components from correlated high-dimensional predictors, supervised by the survival outcome.
* **Model Fitting:** Cox proportional hazards regression on the extracted PLS components.
* **Component Selection:** Automatic selection of the optimal number of components via cross-validation (log-likelihood, C-index), information criteria (BIC, AIC), or manual specification.
* **Variable Importance:** Identification of influential predictors through PLS loading weights.
* **Risk Stratification:** Patient grouping (2-5 groups) based on PLS-derived risk scores with Kaplan-Meier survival curves.
* **Validation:** Bootstrap validation for optimism-corrected performance and permutation testing for model significance.
* **Visualization:** Five dedicated plots covering component analysis, variable loadings, scores, cross-validation curves, and survival stratification.

## Feature Details

The following table provides a detailed mapping of the module's features, from the user interface to the underlying R functions.

| Feature | YAML Argument (`.a.yaml`) | UI Label | Results Section (`.r.yaml`) | R Function (`.b.R`) |
|---------|---------------------------|----------|----------------------------|---------------------|
| **Input Variables** | | | | |
| Survival time | `time` | Time Variable | -- | `.run` |
| Event status | `status` | Status Variable | -- | `.run` |
| High-dim predictors | `predictors` | High-dimensional Predictors | -- | `.run` |
| **PLS Configuration** | | | | |
| Number of components | `pls_components` | Number of PLS Components | `componentSelection` | `.run` |
| PLS algorithm | `pls_algorithm` | PLS Algorithm | `modelSummary` | `.run` |
| **Component Selection** | | | | |
| CV method | `cross_validation` | Cross-Validation Method | `componentSelection` | `.run` |
| Selection criterion | `component_selection` | Component Selection Method | `componentSelection` | `.run` |
| **Scaling** | | | | |
| Variable scaling | `scaling_method` | Variable Scaling | `modelSummary` | `.run` |
| **Algorithm Parameters** | | | | |
| Max iterations | `max_iterations` | Maximum Iterations | `technicalNotes` | `.run` |
| Convergence tolerance | `tolerance` | Convergence Tolerance | `technicalNotes` | `.run` |
| **Validation** | | | | |
| Bootstrap validation | `bootstrap_validation` | Bootstrap Validation | `bootstrapResults` | `.run` |
| Bootstrap replications | `n_bootstrap` | Bootstrap Replications | `bootstrapResults` | `.run` |
| Permutation test | `permutation_test` | Permutation Test | `permutationResults` | `.run` |
| Number of permutations | `n_permutations` | Number of Permutations | `permutationResults` | `.run` |
| **Output Tables** | | | | |
| Model summary | -- | Model Summary | `modelSummary` | `.run` |
| Component selection | -- | Component Selection Results | `componentSelection` | `.run` |
| Model coefficients | -- | PLS Cox Model Coefficients | `modelCoefficients` | `.run` |
| Variable loadings | `feature_importance` | Variable Loadings | `variableLoadings` | `.run` |
| Performance metrics | `prediction_accuracy` | Model Performance Metrics | `modelPerformance` | `.run` |
| Risk stratification | -- | Risk Group Stratification | `riskStratification` | `.run` |
| Confidence intervals | `confidence_intervals` | Confidence Intervals | `modelCoefficients` | `.run` |
| **Plots** | | | | |
| Component analysis | `plot_components` | Component Plots | `componentPlot` | `.plotComponents` |
| Variable loadings | `plot_loadings` | Variable Loadings Plot | `loadingsPlot` | `.plotLoadings` |
| Component scores | `plot_scores` | Component Scores Plot | `scoresPlot` | `.plotScores` |
| CV curves | `plot_validation` | Cross-Validation Plot | `validationPlot` | `.plotValidation` |
| Survival curves | `plot_survival` | Survival Curves | `survivalPlot` | `.plotSurvival` |
| **Risk Groups** | | | | |
| Number of groups | `risk_groups` | Number of Risk Groups | `riskStratification` | `.run` |
| **Interpretation** | | | | |
| Clinical guidance | -- | Clinical Interpretation Guide | `clinicalGuidance` | `.run` |
| Technical notes | -- | Technical Notes and Assumptions | `technicalNotes` | `.run` |

## Component Selection Methods

The module provides 5 methods for selecting the optimal number of PLS components:

| Method | Option Value | Requires CV | Description | Tendency |
|--------|-------------|-------------|-------------|----------|
| CV Log-Likelihood | `cv_loglik` | Yes | Maximizes cross-validated partial log-likelihood | Balanced |
| CV C-Index | `cv_cindex` | Yes | Maximizes cross-validated concordance index | Slightly liberal |
| BIC | `bic` | No | Minimizes Bayesian Information Criterion | Conservative (fewer components) |
| AIC | `aic` | No | Minimizes Akaike Information Criterion | Liberal (more components) |
| Manual | `manual` | No | Uses exactly `pls_components` components | User-defined |

When `component_selection` is set to `cv_loglik` or `cv_cindex`, the `cross_validation` method must not be `none`. BIC, AIC, and manual selection work independently of cross-validation.

## PLS Algorithms

| Algorithm | Option Value | Description | Best For |
|-----------|-------------|-------------|----------|
| NIPALS | `nipals` | Nonlinear Iterative Partial Least Squares | General purpose, iterative, respects `max_iterations`/`tolerance` |
| Kernel PLS | `kernel` | Kernel-based computation | Faster than NIPALS, non-iterative |
| Wide Kernel PLS | `widekernelpls` | Kernel PLS optimized for p >> n | Very high-dimensional data (genomics, proteomics) |

All three algorithms should produce similar results for well-conditioned data. NIPALS is the only iterative algorithm and is the only one affected by `max_iterations` and `tolerance` settings.

## Internal Architecture

### Data Flow

```
.init()              -> Welcome message with instructions
.run()               -> Main pipeline:
  [1] Validate inputs (time, status, predictors selected)
  [2] Check required packages (survival, pls/plsRcox)
  [3] Extract and validate data
  [4] Scale predictors (scaling_method)
  [5] Fit PLS model (pls_algorithm, pls_components)
  [6] Select optimal components (component_selection, cross_validation)
  [7] Fit Cox model on selected PLS components
  [8] Calculate variable loadings and importance
  [9] Compute risk scores and stratify patients
  [10] Populate tables (componentSelection, modelCoefficients, etc.)
  [11] Set plot states (protobuf-safe data only)
  [12] Run bootstrap validation (if enabled)
  [13] Run permutation test (if enabled)
  [14] Generate clinical guidance and technical notes

.plotComponents()    -> Render component variance/eigenvalue plot
.plotLoadings()      -> Render variable loadings bar/heatmap
.plotScores()        -> Render component scores vs survival
.plotValidation()    -> Render CV curves for component selection
.plotSurvival()      -> Render risk-stratified KM survival curves
```

### State Management (Protobuf Safety)

Plot state must use only plain numeric vectors, character vectors, and base data.frames to avoid serialization errors. Never store model objects, environments, or function references in state.

| Plot | State Contents | Type |
|------|---------------|------|
| `componentPlot` | Component index (integer), variance explained (numeric), cumulative variance (numeric) | Named list of vectors |
| `loadingsPlot` | Variable names (character), loading values per component (numeric matrix as data.frame) | data.frame |
| `scoresPlot` | Score values per component (numeric), time (numeric), status (integer) | data.frame |
| `validationPlot` | N components (integer), CV scores (numeric), CV SE (numeric), selected component index (integer) | Named list of vectors |
| `survivalPlot` | Time (numeric), status (integer), risk_group (character/factor) | data.frame |

Critical pattern for all setState calls:

```r
# Convert to base data.frame before setState to avoid protobuf issues
plotData <- as.data.frame(plotData)
image$setState(plotData)
```

### Results Items (from `.r.yaml`)

| Item | Type | Visibility | Render Function |
|------|------|-----------|----------------|
| `todo` | Html | always | -- (set in .init) |
| `modelSummary` | Html | always | -- |
| `componentSelection` | Table | always | -- |
| `modelCoefficients` | Table | always | -- |
| `variableLoadings` | Table | `(feature_importance)` | -- |
| `modelPerformance` | Table | `(prediction_accuracy)` | -- |
| `riskStratification` | Table | always | -- |
| `componentPlot` | Image 800x600 | `(plot_components)` | `.plotComponents` |
| `loadingsPlot` | Image 800x600 | `(plot_loadings)` | `.plotLoadings` |
| `scoresPlot` | Image 800x600 | `(plot_scores)` | `.plotScores` |
| `validationPlot` | Image 800x500 | `(plot_validation)` | `.plotValidation` |
| `survivalPlot` | Image 800x600 | `(plot_survival)` | `.plotSurvival` |
| `bootstrapResults` | Html | `(bootstrap_validation)` | -- |
| `permutationResults` | Html | `(permutation_test)` | -- |
| `clinicalGuidance` | Html | always | -- |
| `technicalNotes` | Html | always | -- |

## Dependencies

| Package | Usage |
|---------|-------|
| `survival` | `Surv()`, `coxph()`, `survfit()`, `survdiff()`, `concordance()` -- Cox modeling and survival objects |
| `pls` | Core PLS algorithms (`plsr()`, `mvr()`) -- NIPALS, kernel, widekernelpls |
| `plsRcox` | PLS-Cox specific methods (supervised PLS for survival) |
| `survminer` | `ggsurvplot()` -- enhanced survival curve visualization |
| `ggplot2` | Component, loading, score, and validation plots |
| `ggrepel` | Non-overlapping labels on loading plots |
| `glue` | HTML content generation |
| `grid` | Fallback plot error messages |

## File Locations

| File | Path | Purpose |
|------|------|---------|
| Analysis definition | `jamovi/plscox.a.yaml` | Options and parameters (28 options) |
| Results definition | `jamovi/plscox.r.yaml` | Output items (16 items: 6 tables, 5 plots, 5 HTML) |
| UI definition | `jamovi/plscox.u.yaml` | Interface layout and grouping |
| Backend | `R/plscox.b.R` | Implementation (R6 class with .run + 5 plot functions) |
| Auto-generated header | `R/plscox.h.R` | Compiled from YAML (do not edit directly) |
| Test data generator | `data-raw/create_plscox_test_data.R` | Synthetic metabolomics + small datasets |
| Testing guide | `vignettes/testing_plscox.md` | 67 test scenarios with full option coverage |
