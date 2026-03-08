# Adaptive LASSO for Cox Models -- Documentation

This document provides a comprehensive overview of the Adaptive LASSO for Cox Models module, detailing its features, user interface elements, and the underlying R functions.

## Feature Summary

The Adaptive LASSO module (`adaptivelasso`) performs penalized variable selection for Cox proportional hazards models using data-driven adaptive weights. Unlike standard LASSO, the adaptive LASSO achieves the oracle property -- consistent variable selection with asymptotically unbiased estimates for non-zero coefficients.

The module's features can be broadly categorized as follows:

* **Adaptive Weight Computation:** Five methods for computing penalty weights (ridge, univariate Cox, full Cox, marginal correlation, equal).
* **Cross-Validated Model Fitting:** Automatic penalty parameter selection via cross-validation with multiple performance measures (deviance, C-index, Brier score, AUC).
* **Stability Selection:** Bootstrap-based variable selection robustness assessment with configurable thresholds and subsampling ratios.
* **Model Diagnostics:** Proportional hazards testing, influence diagnostics, and goodness-of-fit assessment.
* **Risk Stratification:** Risk group classification based on linear predictor with Kaplan-Meier curves.
* **Baseline Survival Estimation:** Breslow estimator for baseline survival and hazard functions.
* **Stratified Analysis:** Optional stratification variable for separate baseline hazards.
* **Visualization:** Six plot types covering the regularization path, cross-validation curve, stability selection, survival curves, baseline hazard, and model diagnostics.

## Feature Details

The following table provides a detailed mapping of the module's features, from the user interface to the underlying R functions.

| Feature | YAML Argument (`.a.yaml`) | UI Label | Results Section (`.r.yaml`) | R Function (`.b.R`) |
|---------|---------------------------|----------|----------------------------|---------------------|
| **Input Variables** | | | | |
| Survival time | `time` | Time Variable | -- | `.cleanData` |
| Event indicator | `event` | Event Indicator | -- | `.cleanData` |
| Candidate predictors | `predictors` | Predictor Variables | -- | `.cleanData` |
| Stratification | `strata` | Stratification Variable | -- | `.cleanData` |
| **Model Specification** | | | | |
| Weight calculation | `weight_method` | Weight Calculation Method | `coefficients` | `.computeAdaptiveWeights` |
| Elastic net mixing | `alpha` | Elastic Net Mixing Parameter | `coefficients` | `.fitAdaptiveLasso` |
| Adaptive weight power | `gamma` | Adaptive Weight Power | `coefficients` | `.computeAdaptiveWeights` |
| **Cross-Validation** | | | | |
| CV folds | `cv_folds` | Cross-Validation Folds | `cvResults` | `.fitAdaptiveLasso` |
| CV performance metric | `cv_measure` | CV Performance Measure | `cvResults` | `.fitAdaptiveLasso` |
| Lambda sequence type | `lambda_sequence` | Lambda Sequence | `cvResults` | `.fitAdaptiveLasso` |
| Lambda min ratio | `lambda_min_ratio` | Lambda Min Ratio | `cvResults` | `.fitAdaptiveLasso` |
| Lambda grid size | `n_lambda` | Number of Lambda Values | `selectionPath` | `.fitAdaptiveLasso` |
| **Stability Selection** | | | | |
| Enable stability | `stability_selection` | Stability Selection | `stabilityResults` | `.runStabilitySelection` |
| Selection threshold | `stability_threshold` | Stability Threshold | `stabilityResults` | `.runStabilitySelection` |
| Bootstrap replicates | `bootstrap_samples` | Bootstrap Samples | `stabilityResults` | `.runStabilitySelection` |
| Subsampling proportion | `subsampling_ratio` | Subsampling Ratio | `stabilityResults` | `.runStabilitySelection` |
| **Model Diagnostics** | | | | |
| PH assumption test | `proportional_hazards` | Check Proportional Hazards | `modelDiagnostics` | `.testProportionalHazards` |
| Influence analysis | `influence_diagnostics` | Influence Diagnostics | `modelDiagnostics` | `.computeInfluence` |
| Goodness of fit | `goodness_of_fit` | Goodness of Fit Tests | `performanceMetrics` | `.assessGoodnessOfFit` |
| **Risk & Prediction** | | | | |
| Risk group count | `risk_groups` | Number of Risk Groups | `riskGroups` | `.stratifyRiskGroups` |
| Prediction times | `time_points` | Prediction Time Points | `predictions` | `.predictSurvival` |
| Baseline survival | `baseline_survival` | Estimate Baseline Survival | `predictions` | `.estimateBaseline` |
| **Output Tables** | | | | |
| Coefficients table | `show_coefficients` | Show Coefficients | `coefficients` | `.populateCoefficients` |
| Selection path table | `show_selection_path` | Show Selection Path | `selectionPath` | `.populateSelectionPath` |
| CV results table | `show_cv_results` | Show CV Results | `cvResults` | `.populateCVResults` |
| Diagnostics table | `show_diagnostics` | Show Model Diagnostics | `modelDiagnostics` | `.populateDiagnostics` |
| **Plots** | | | | |
| Regularization path | `plot_selection_path` | Regularization Path Plot | `pathPlot` | `.pathPlot` |
| CV curve | `plot_cv_curve` | Cross-Validation Curve | `cvPlot` | `.cvPlot` |
| Stability plot | `plot_stability` | Stability Selection Plot | `stabilityPlot` | `.stabilityPlot` |
| Survival curves | `plot_survival_curves` | Risk Group Survival Curves | `survivalPlot` | `.survivalPlot` |
| Baseline hazard | `plot_baseline_hazard` | Baseline Hazard Plot | `baselineHazardPlot` | `.baselineHazardPlot` |
| Diagnostic plots | `plot_diagnostics` | Diagnostic Plots | `diagnosticsPlot` | `.diagnosticsPlot` |
| **Advanced** | | | | |
| Tied times handling | `tie_method` | Tied Times Method | -- | `.fitAdaptiveLasso` |
| Predictor standardization | `standardize` | Standardize Predictors | -- | `.cleanData` |
| Intercept term | `intercept` | Include Intercept | -- | `.fitAdaptiveLasso` |
| Parallel computing | `parallel_computing` | Parallel Computing | -- | `.setupParallel` |
| CPU cores | `n_cores` | Number of Cores | -- | `.setupParallel` |
| Convergence criterion | `convergence_threshold` | Convergence Threshold | -- | `.fitAdaptiveLasso` |
| Iteration limit | `max_iterations` | Maximum Iterations | -- | `.fitAdaptiveLasso` |
| Reproducibility seed | `random_seed` | Random Seed | -- | `.run` |

## Internal Architecture

### Data Flow

```
.init()                        -> Package checks, welcome message
.run()                         -> Main pipeline:
  set.seed(random_seed)        -> Reproducibility
  .cleanData()                 -> Validate time/event/predictors, create design matrix
                                  Handle strata, standardize if requested
  .computeAdaptiveWeights()    -> Initial estimates via weight_method (ridge/univariate/cox/correlation/equal)
                                  Apply gamma power: w_j = 1 / |beta_init_j|^gamma
  .fitAdaptiveLasso()          -> cv.glmnet() with penalty.factor = adaptive weights
                                  Respect alpha, lambda_sequence, cv_folds, cv_measure
                                  Extract lambda.min, lambda.1se, coefficients
  .runStabilitySelection()     -> [Optional] Bootstrap loop:
                                    Subsample data (subsampling_ratio)
                                    Refit adaptive LASSO on subsample
                                    Record selected variables
                                  Compute selection frequencies
                                  Apply stability_threshold
  .testProportionalHazards()   -> [Optional] cox.zph() on selected variables
  .computeInfluence()          -> [Optional] dfbeta, leverage via coxph residuals
  .assessGoodnessOfFit()       -> [Optional] C-index, deviance, calibration
  .stratifyRiskGroups()        -> Quantile-based risk groups from linear predictor
  .predictSurvival()           -> Survival probabilities at time_points
  .estimateBaseline()          -> Breslow baseline survival/hazard
  .populateCoefficients()      -> Fill coefficients table
  .populateSelectionPath()     -> Fill regularization path table
  .populateCVResults()         -> Fill CV results table
  .populateDiagnostics()       -> Fill diagnostics and performance tables
  .savePlotData()              -> Extract plain numerics -> setState() (protobuf-safe)
```

### State Management (Protobuf Safety)

Plot state uses only plain numeric vectors, character vectors, and base data.frames to avoid serialization errors with jamovi's protobuf system. No glmnet objects, formula objects, or function references are stored in state.

| Plot | State Contents |
|------|---------------|
| `pathPlot` | `lambda` (numeric vector), `coef_matrix` (numeric matrix of coefficients at each lambda), `var_names` (character), `lambda_min` (numeric), `lambda_1se` (numeric) |
| `cvPlot` | `lambda` (numeric), `cvm` (numeric), `cvsd` (numeric), `cvup` (numeric), `cvlo` (numeric), `lambda_min` (numeric), `lambda_1se` (numeric), `cv_measure` (character) |
| `stabilityPlot` | `var_names` (character), `selection_freq` (numeric), `threshold` (numeric) |
| `survivalPlot` | `time` (numeric), `status` (integer), `risk_group` (factor stored as integer + labels), or base data.frame with these columns |
| `baselineHazardPlot` | `time` (numeric), `hazard` (numeric), `cumhazard` (numeric), `survival` (numeric) |
| `diagnosticsPlot` | `residuals` (data.frame of numeric columns: schoenfeld, dfbeta, deviance), `var_names` (character), `time` (numeric) |

### Dependencies

| Package | Usage |
|---------|-------|
| `glmnet` | `cv.glmnet()`, `glmnet()` -- Adaptive LASSO fitting with penalty.factor for adaptive weights |
| `survival` | `Surv()`, `coxph()`, `cox.zph()`, `survfit()`, `survdiff()`, `concordance()` -- Survival objects, initial weight estimation, diagnostics, risk group analysis |
| `survminer` | `ggsurvplot()` -- Enhanced Kaplan-Meier survival curves for risk groups |
| `ggplot2` | Regularization path plot, CV curve, stability bar chart, baseline hazard, diagnostic panels |
| `grid` | Fallback plot error messages |

### Weight Computation Methods

| Method | Initial Estimator | Formula | Pros | Cons |
|--------|------------------|---------|------|------|
| `ridge` | Ridge Cox via `cv.glmnet(alpha=0)` | `w_j = 1 / \|beta_ridge_j\|^gamma` | Stable even when p > n; handles collinearity | Requires tuning ridge lambda |
| `univariate` | Individual univariate Cox models | `w_j = 1 / \|beta_univariate_j\|^gamma` | Simple, fast | Ignores covariate relationships |
| `cox` | Full unpenalized Cox via `coxph()` | `w_j = 1 / \|beta_cox_j\|^gamma` | Uses full model information | Unstable when p close to n; fails when p > n |
| `correlation` | Marginal correlation with outcome | `w_j = 1 / \|cor_j\|^gamma` | Very fast, no model fitting needed | Crude; ignores confounding |
| `equal` | All weights = 1 | `w_j = 1` for all j | Degenerates to standard LASSO | No adaptive property; no oracle guarantee |

## File Locations

| File | Path | Purpose |
|------|------|---------|
| Analysis definition | `jamovi/adaptivelasso.a.yaml` | Options/parameters (39 options) |
| Results definition | `jamovi/adaptivelasso.r.yaml` | Output items (7 tables, 6 plots, 2 HTML) |
| UI definition | `jamovi/adaptivelasso.u.yaml` | Interface layout |
| Backend | `R/adaptivelasso.b.R` | Implementation |
| Auto-generated header | `R/adaptivelasso.h.R` | Compiled from YAML |
| Test data generator | `data-raw/create_adaptivelasso_test_data.R` | Synthetic datasets (n=180 main, n=40 small) |
| Testing guide | `vignettes/testing_adaptivelasso.md` | 63 test scenarios with option coverage |
