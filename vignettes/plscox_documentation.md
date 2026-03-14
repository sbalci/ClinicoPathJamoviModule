# PLS-Cox Regression Feature Mapping

This document provides a comprehensive feature-to-implementation mapping for the PLS-Cox Regression module (`plscox`), covering every option, UI element, results item, and backend function.

## Feature Summary

The PLS-Cox Regression module (`plscox`) performs Partial Least Squares dimensionality reduction combined with Cox proportional hazards modeling for high-dimensional survival data. It is designed for clinicopathological research with genomic, proteomic, or metabolomic datasets where the number of predictors may greatly exceed the number of observations (the p >> n problem). The core computation engine is the `plsRcox` R package.

The module extracts latent PLS components -- supervised linear combinations of all predictors that maximally covary with the survival outcome -- and fits a Cox proportional hazards model on these components. Unlike LASSO or other penalized regression approaches that select individual variables, PLS retains information from all predictors by projecting them into a lower-dimensional space. This makes it particularly well-suited for metabolomics, gene expression, and proteomic datasets where variables are highly correlated and reflect shared underlying biological pathways.

The module includes automatic component selection (via cross-validation, BIC, or AIC), four variable scaling methods, data suitability assessment with a traffic-light system, bootstrap validation for optimism-corrected performance estimates, permutation testing for overall model significance, risk group stratification with Kaplan-Meier survival curves, and five visualization types. Advanced options expose plsRcox-specific parameters including sparse PLS mode, Q-squared stopping criterion, and p-value-based variable selection within the PLS fitting algorithm.

## Feature Details

### Input Variables

| Feature | YAML Argument (`.a.yaml`) | UI Label | Results Section (`.r.yaml`) | R Function (`.b.R`) |
|---|---|---|---|---|
| Data frame | `data` | *(implicit)* | -- | `.run` |
| Survival time | `time` | Time Variable | -- | `.run` |
| Event status | `status` | Status Variable | -- | `.run` |
| Event level | `outcomeLevel` | Event Level | -- | `.run` |
| Censored level | `censorLevel` | Censored Level | -- | `.run` |
| High-dim predictors | `predictors` | High-dimensional Predictors | -- | `.run` |

### PLS Model Settings

| Feature | YAML Argument (`.a.yaml`) | UI Label | Results Section (`.r.yaml`) | R Function (`.b.R`) |
|---|---|---|---|---|
| Number of components | `pls_components` | Number of PLS Components | `componentSelection` | `.run` |
| Component selection method | `component_selection` | Component Selection Method | `componentSelection` | `.run` |
| Cross-validation method | `cross_validation` | Cross-Validation Method | `componentSelection`, `validationPlot` | `.run` |
| Variable scaling | `scaling_method` | Variable Scaling | `modelSummary` | `.run` |

### Advanced PLS Settings

| Feature | YAML Argument (`.a.yaml`) | UI Label | Results Section (`.r.yaml`) | R Function (`.b.R`) |
|---|---|---|---|---|
| Tie handling | `tie_method` | Tie Handling Method | `modelSummary`, `technicalNotes` | `.run` |
| Convergence tolerance | `tolerance` | Convergence Tolerance | `modelSummary`, `technicalNotes` | `.run` |
| Q-squared limit | `limQ2set` | Q-squared Limit | `modelSummary` | `.run` |
| Sparse PLS | `sparse_pls` | Sparse PLS | `modelSummary` | `.run` |
| P-value variable selection | `pvals_expli` | P-value Variable Selection | `modelSummary` | `.run` |
| P-value threshold | `alpha_pvals_expli` | P-value Threshold | `modelSummary` | `.run` |

### Validation

| Feature | YAML Argument (`.a.yaml`) | UI Label | Results Section (`.r.yaml`) | R Function (`.b.R`) |
|---|---|---|---|---|
| Bootstrap validation | `bootstrap_validation` | Bootstrap Validation | `bootstrapResults` | `.run` |
| Bootstrap replications | `n_bootstrap` | Bootstrap Replications | `bootstrapResults` | `.run` |
| Permutation test | `permutation_test` | Permutation Test | `permutationResults` | `.run` |
| Number of permutations | `n_permutations` | Number of Permutations | `permutationResults` | `.run` |

### Risk Stratification

| Feature | YAML Argument (`.a.yaml`) | UI Label | Results Section (`.r.yaml`) | R Function (`.b.R`) |
|---|---|---|---|---|
| Number of risk groups | `risk_groups` | Number of Risk Groups | `riskStratification`, `survivalPlot` | `.run` |

### Output Options

| Feature | YAML Argument (`.a.yaml`) | UI Label | Results Section (`.r.yaml`) | R Function (`.b.R`) |
|---|---|---|---|---|
| Confidence intervals | `confidence_intervals` | Confidence Intervals | `modelCoefficients` (hr_lower, hr_upper) | `.run` |
| Variable importance | `feature_importance` | Variable Importance | `variableLoadings` | `.run` |
| Prediction accuracy | `prediction_accuracy` | Prediction Accuracy | `modelPerformance` | `.run` |
| Data suitability | `suitabilityCheck` | Data Suitability Assessment | `suitabilityReport` | `.assessSuitability` |

### Plots

| Feature | YAML Argument (`.a.yaml`) | UI Label | Results Section (`.r.yaml`) | R Function (`.b.R`) |
|---|---|---|---|---|
| Component analysis plot | `plot_components` | Component Plots | `componentPlot` (800x600) | `.plotComponents` |
| Variable loadings plot | `plot_loadings` | Variable Loadings Plot | `loadingsPlot` (800x600) | `.plotLoadings` |
| Component scores plot | `plot_scores` | Component Scores Plot | `scoresPlot` (800x600) | `.plotScores` |
| Cross-validation plot | `plot_validation` | Cross-Validation Plot | `validationPlot` (800x500) | `.plotValidation` |
| Survival curves | `plot_survival` | Survival Curves | `survivalPlot` (800x600) | `.plotSurvival` |

### Core Results (Always Visible)

| Feature | YAML Argument (`.a.yaml`) | UI Label | Results Section (`.r.yaml`) | R Function (`.b.R`) |
|---|---|---|---|---|
| Welcome instructions | -- | Instructions | `todo` (Html) | `.init` |
| Model summary | -- | Model Summary | `modelSummary` (Html) | `.run` |
| Component selection table | -- | Component Selection Results | `componentSelection` (Table) | `.run` |
| Model coefficients table | -- | PLS Cox Model Coefficients | `modelCoefficients` (Table) | `.run` |
| Risk group table | -- | Risk Group Stratification | `riskStratification` (Table) | `.run` |
| Clinical guidance | -- | Clinical Interpretation Guide | `clinicalGuidance` (Html) | `.run` |
| Technical notes | -- | Technical Notes and Assumptions | `technicalNotes` (Html) | `.run` |

### Explanatory Output (Conditional Visibility)

| Feature | YAML Argument (`.a.yaml`) | UI Label | Results Section (`.r.yaml`) | R Function (`.b.R`) |
|---|---|---|---|---|
| Suitability report | `suitabilityCheck` | Data Suitability Assessment | `suitabilityReport` (Html) | `.assessSuitability`, `.generateSuitabilityHtml` |
| Variable loadings table | `feature_importance` | Variable Loadings on PLS Components | `variableLoadings` (Table) | `.run` |
| Performance metrics table | `prediction_accuracy` | Model Performance Metrics | `modelPerformance` (Table) | `.run` |
| Bootstrap results | `bootstrap_validation` | Bootstrap Validation Results | `bootstrapResults` (Html) | `.run` |
| Permutation results | `permutation_test` | Permutation Test Results | `permutationResults` (Html) | `.run` |

## Component Selection Methods

| Method | Option Value | Requires CV | Description | Tendency |
|---|---|---|---|---|
| CV Log-Likelihood | `cv_loglik` | Yes | Maximizes cross-validated partial log-likelihood | Balanced |
| CV C-Index | `cv_cindex` | Yes | Maximizes cross-validated concordance index | Slightly liberal |
| BIC | `bic` | No | Minimizes Bayesian Information Criterion | Conservative (fewer components) |
| AIC | `aic` | No | Minimizes Akaike Information Criterion | Liberal (more components) |
| Manual | `manual` | No | Uses exactly `pls_components` components | User-defined |

When `component_selection` is set to `cv_loglik` or `cv_cindex`, the `cross_validation` method must not be `none`. If CV is set to `none` while a CV-based criterion is selected, the module falls back to manual selection with a warning note.

## Scaling Methods

| Method | Option Value | Centers? | Scales? | Mechanism |
|---|---|---|---|---|
| Standardization (Z-scores) | `standardize` | Yes | Yes | Delegated to plsRcox via `scaleX=TRUE` |
| Unit Variance Scaling | `unit_variance` | No | Yes | R `scale(center=FALSE, scale=TRUE)` pre-applied |
| Min-Max Scaling | `minmax` | Yes | Yes | Manual `(x - min) / (max - min)` pre-applied |
| No Scaling | `none` | No | No | Raw values passed to plsRcox |

## Suitability Assessment Checks

The data suitability assessment (enabled by `suitabilityCheck`) runs 6 checks with a traffic-light color system:

| Check | Green | Yellow | Red |
|---|---|---|---|
| Events-Per-Variable | EPV >= 10 | 1 <= EPV < 10 | EPV < 1 |
| Reduction Need | p >= n/3 | p < n/3 | -- |
| Sample Size | n >= 100 | 30 <= n < 100 | n < 30 |
| Event Rate | 20--80% | Outside 20--80% | -- |
| Multicollinearity | max\|r\| < 0.7 | -- | -- (always green for PLS) |
| Data Quality | No missing | 0--20% missing | > 20% missing |

## Dependencies

| Package | Usage |
|---|---|
| `survival` | `Surv()`, `coxph()`, `survfit()`, `concordance()` |
| `plsRcox` | `plsRcox()`, `cv.plsRcox()` |
| `survminer` | `ggsurvplot()` (optional, falls back to base plot) |
| `ggplot2` | Component, loading, score, and validation plots |
| `ggrepel` | Non-overlapping labels on loading plots (optional) |
| `glue` | HTML content generation |
| `MASS` | `mvrnorm()` in test data generation only |
