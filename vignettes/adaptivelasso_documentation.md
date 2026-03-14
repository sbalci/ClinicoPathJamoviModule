# Adaptive LASSO for Cox Models -- Documentation

This document provides a comprehensive overview of the Adaptive LASSO for Cox Models module, detailing its features, user interface elements, and the underlying R functions.

## Changelog

- **2026-03-10**: Complete refresh — corrected all R method names to match actual `.b.R` code, removed non-existent `intercept` option and `brier`/`auc` cv_measure values, added missing `suitabilityCheck`/`event_level`/`lambda_custom_max`/`lambda_custom_min`/`lambda_single` features, updated option count to 42.

## Feature Summary

The Adaptive LASSO module (`adaptivelasso`) performs penalized variable selection for Cox proportional hazards models using data-driven adaptive weights. Unlike standard LASSO, the adaptive LASSO achieves the oracle property -- consistent variable selection with asymptotically unbiased estimates for non-zero coefficients.

The module's features can be broadly categorized as follows:

* **Data Suitability Assessment:** Automatic EPV (events per variable) evaluation with traffic-light indicators and missing data reporting.
* **Adaptive Weight Computation:** Five methods for computing penalty weights (ridge, univariate Cox, full Cox, marginal correlation, equal).
* **Cross-Validated Model Fitting:** Automatic penalty parameter selection via cross-validation with two performance measures (deviance, C-index). Supports automatic, custom, and single lambda sequences.
* **Stability Selection:** Bootstrap-based variable selection robustness assessment with configurable thresholds, subsampling ratios, and PFER error bounds.
* **Model Diagnostics:** Proportional hazards testing (Schoenfeld residuals), influence diagnostics (dfbeta), and goodness-of-fit assessment (C-index, AIC, log-likelihood).
* **Risk Stratification:** Quantile-based risk group classification with Kaplan-Meier curves and hazard ratios.
* **Baseline Survival Estimation:** Breslow estimator for baseline survival and hazard functions at user-specified time points.
* **Stratified Analysis:** Optional stratification variable for separate baseline hazards via `glmnet::stratifySurv()`.
* **Visualization:** Six plot types covering the regularization path, cross-validation curve, stability selection, survival curves, baseline hazard, and model diagnostics.

## Feature Details

The following table provides a detailed mapping of the module's features, from the user interface to the underlying R functions. All R function names correspond to actual private methods in `R/adaptivelasso.b.R`.

| Feature | YAML Argument (`.a.yaml`) | UI Label | Results Section (`.r.yaml`) | R Function (`.b.R`) |
|---------|---------------------------|----------|----------------------------|---------------------|
| **Input Variables** | | | | |
| Survival time | `time` | Time Variable | -- | `.prepareData` |
| Event indicator | `event` | Event Indicator | -- | `.prepareData`, `.encodeEventIndicator` |
| Event level selection | `event_level` | Event Level | -- | `.encodeEventIndicator` |
| Candidate predictors | `predictors` | Predictor Variables | -- | `.prepareData` |
| Stratification | `strata` | Stratification Variable (optional) | -- | `.prepareData`, `.fitAdaptiveLasso` |
| **Data Assessment** | | | | |
| Suitability check | `suitabilityCheck` | Data Suitability Assessment | `suitabilityReport` | `.assessSuitability` |
| **Model Specification** | | | | |
| Weight calculation | `weight_method` | Weight Calculation Method | `coefficients` | `.calculateAdaptiveWeights` |
| Elastic net mixing | `alpha` | Elastic Net Mixing Parameter | `coefficients` | `.fitAdaptiveLasso` |
| Adaptive weight power | `gamma` | Adaptive Weight Power | `coefficients` | `.calculateAdaptiveWeights` |
| **Cross-Validation** | | | | |
| CV folds | `cv_folds` | Cross-Validation Folds | `cvResults` | `.fitAdaptiveLasso` |
| CV performance metric | `cv_measure` | CV Performance Measure | `cvResults`, `selectionPath` | `.fitAdaptiveLasso` |
| Lambda sequence type | `lambda_sequence` | Lambda Sequence | `cvResults` | `.buildLambdaSequence` |
| Custom lambda maximum | `lambda_custom_max` | Custom Lambda Maximum | `cvResults` | `.buildLambdaSequence` |
| Custom lambda minimum | `lambda_custom_min` | Custom Lambda Minimum | `cvResults` | `.buildLambdaSequence` |
| Single lambda value | `lambda_single` | Single Lambda Value | `cvResults` | `.buildLambdaSequence` |
| Lambda min ratio | `lambda_min_ratio` | Lambda Min Ratio | `cvResults` | `.fitAdaptiveLasso` |
| Lambda grid size | `n_lambda` | Number of Lambda Values | `selectionPath` | `.fitAdaptiveLasso` |
| **Stability Selection** | | | | |
| Enable stability | `stability_selection` | Stability Selection | `stabilityResults` | `.stabilitySelection` |
| Selection threshold | `stability_threshold` | Stability Threshold | `stabilityResults` | `.stabilitySelection` |
| Bootstrap replicates | `bootstrap_samples` | Bootstrap Samples | `stabilityResults` | `.stabilitySelection` |
| Subsampling proportion | `subsampling_ratio` | Subsampling Ratio | `stabilityResults` | `.stabilitySelection` |
| **Model Diagnostics** | | | | |
| PH assumption test | `proportional_hazards` | Check Proportional Hazards | `modelDiagnostics` | `.calculateDiagnostics` |
| Influence analysis | `influence_diagnostics` | Influence Diagnostics | `modelDiagnostics` | `.calculateDiagnostics` |
| Goodness of fit | `goodness_of_fit` | Goodness of Fit Tests | `performanceMetrics` | `.populatePerformance` |
| **Risk & Prediction** | | | | |
| Risk group count | `risk_groups` | Number of Risk Groups | `riskGroups` | `.assignRiskGroups`, `.populateRiskGroups` |
| Prediction times | `time_points` | Prediction Time Points | `predictions` | `.populatePredictions` |
| Baseline survival | `baseline_survival` | Estimate Baseline Survival | `predictions` | `.populatePredictions` |
| **Output Tables** | | | | |
| Coefficients table | `show_coefficients` | Show Coefficients | `coefficients`, `riskGroups` | `.populateCoefficients`, `.populateRiskGroups` |
| Selection path table | `show_selection_path` | Show Selection Path | `selectionPath` | `.populateSelectionPath` |
| CV results table | `show_cv_results` | Show CV Results | `cvResults` | `.populateCVResults` |
| Diagnostics table | `show_diagnostics` | Show Model Diagnostics | `modelDiagnostics` | `.populateDiagnostics` |
| **Visualization** | | | | |
| Regularization path | `plot_selection_path` | Regularization Path Plot | `pathPlot` | `.plotSelectionPath`, `.renderPathPlot` |
| CV curve | `plot_cv_curve` | Cross-Validation Curve | `cvPlot` | `.plotCVCurve`, `.renderCVPlot` |
| Stability plot | `plot_stability` | Stability Selection Plot | `stabilityPlot` | `.plotStability`, `.renderStabilityPlot` |
| Survival curves | `plot_survival_curves` | Risk Group Survival Curves | `survivalPlot` | `.plotSurvivalCurves`, `.renderSurvivalPlot` |
| Baseline hazard | `plot_baseline_hazard` | Baseline Hazard Plot | `baselineHazardPlot` | `.plotBaselineHazard`, `.renderBaselineHazardPlot` |
| Diagnostic plots | `plot_diagnostics` | Diagnostic Plots | `diagnosticsPlot` | `.plotDiagnosticsData`, `.renderDiagnosticsPlot` |
| **Advanced Options** | | | | |
| Tied times handling | `tie_method` | Tied Times Method | `coefficients` | `.calculateDiagnostics` |
| Predictor standardization | `standardize` | Standardize Predictors | -- | `.prepareData` |
| Parallel computing | `parallel_computing` | Parallel Computing | -- | `.fitAdaptiveLasso` |
| CPU cores | `n_cores` | Number of Cores | -- | `.fitAdaptiveLasso` |
| Convergence criterion | `convergence_threshold` | Convergence Threshold | -- | `.fitAdaptiveLasso` |
| Iteration limit | `max_iterations` | Maximum Iterations | -- | `.fitAdaptiveLasso` |
| Reproducibility seed | `random_seed` | Random Seed | -- | `.fitAdaptiveLasso`, `.stabilitySelection` |

## Internal Architecture

### Data Flow

```
.init()                        -> Show instructions if no input, call .initResults()
.run()                         -> Main pipeline:
  .prepareData()               -> Validate time/event/predictors, create model.matrix
                                  Remove constant columns, handle strata
                                  Standardize if requested (scale())
                                  Create Surv object
  .encodeEventIndicator()      -> Handle factor/numeric event vars with event_level
  .assessSuitability()         -> [Optional] EPV calc, missing data report → suitabilityReport HTML
  .fitAdaptiveLasso()          -> Setup parallel computing (doParallel)
                                  .calculateAdaptiveWeights() → penalty.factor
                                  .buildLambdaSequence() → lambda grid
                                  cv.glmnet() or glmnet() (single lambda mode)
                                  Extract lambda.min, lambda.1se, coefficients
                                  .calculateDiagnostics() → refit unpenalized Cox
                                  .stabilitySelection() → [Optional] bootstrap loop
  .populateResults()           -> Orchestrate all table/plot population:
    .populateCoefficients()    -> coefficients table (from refitted Cox SE/CI)
    .populateSelectionPath()   -> 20-step regularization path summary
    .populateCVResults()       -> lambda.min/1se, CV error, variable counts
    .populateStabilityResults()-> Selection frequencies, PFER bound
    .populateDiagnostics()     -> Concordance, AIC, PH global p, influence
    .populatePerformance()     -> C-index with CI, N events
    .populateRiskGroups()      -> KM-based risk group stats with HRs
    .populatePredictions()     -> Baseline survival at user time points
    .plotSelectionPath()       -> setState() with coefficient paths data
    .plotCVCurve()             -> setState() with CV metrics
    .plotStability()           -> setState() with selection frequencies
    .plotSurvivalCurves()      -> setState() with time/event/risk_group
    .plotBaselineHazard()      -> setState() with cumulative hazard
    .plotDiagnosticsData()     -> setState() with residuals/dfbeta
    .addNotice() / .renderNotices() -> HTML-based notices (warnings/info)
```

### State Management (Protobuf Safety)

Plot state uses only plain numeric vectors, character vectors, and base data.frames to avoid serialization errors with jamovi's protobuf system. No glmnet objects, formula objects, or function references are stored in state.

| Plot | setState() Contents |
|------|---------------------|
| `pathPlot` | data.frame: transposed top-20 coefficients + `lambda`, `lambda_min`, `lambda_1se` columns |
| `cvPlot` | data.frame: `lambda`, `cvm`, `cvsd`, `cvup`, `cvlo`, `lambda_min`, `lambda_1se`, `measure_label` |
| `stabilityPlot` | data.frame: `selection_frequencies`, `var_names`; attr `threshold` |
| `survivalPlot` | data.frame: `time`, `event`, `risk_group`, `n_groups` |
| `baselineHazardPlot` | data.frame: `time`, `hazard` (cumulative) |
| `diagnosticsPlot` | data.frame: `linear_predictor`, `deviance_residuals`, optional `max_abs_dfbeta` |

### Dependencies

| Package | Usage |
|---------|-------|
| `glmnet` | `cv.glmnet()`, `glmnet()`, `stratifySurv()` -- Adaptive LASSO fitting with penalty.factor |
| `survival` | `Surv()`, `coxph()`, `cox.zph()`, `survfit()`, `survdiff()`, `basehaz()` -- Survival objects, diagnostics, risk groups |
| `parallel` | `detectCores()`, `makePSOCKcluster()`, `stopCluster()` -- Parallel computing |
| `doParallel` | `registerDoParallel()` -- Parallel backend for glmnet CV |
| `foreach` | `registerDoSEQ()` -- Sequential fallback after parallel |

### Weight Computation Methods

| Method | Initial Estimator | Pros | Cons |
|--------|------------------|------|------|
| `ridge` | Ridge Cox via `glmnet(alpha=0)` at lambda[n/4] | Stable even when p > n; handles collinearity | Requires internal ridge lambda selection |
| `univariate` | Individual `coxph()` for each variable | Simple, fast, works with p > n | Ignores covariate relationships |
| `cox` | Full `coxph()` (fallback to ridge if p > n/3) | Uses full model information | Unstable when p close to n |
| `correlation` | Univariate Cox z-statistics | Fast, marginal screening | Crude; ignores confounding |
| `equal` | All weights = 1 | Degenerates to standard LASSO | No adaptive property; no oracle guarantee |

## File Locations

| File | Path | Purpose |
|------|------|---------|
| Analysis definition | `jamovi/adaptivelasso.a.yaml` | 42 options/parameters |
| Results definition | `jamovi/adaptivelasso.r.yaml` | 8 tables, 6 plots, 3 HTML outputs |
| UI definition | `jamovi/adaptivelasso.u.yaml` | Interface layout (8 collapsible sections) |
| Backend | `R/adaptivelasso.b.R` | Implementation (~1810 lines, ~25 private methods) |
| Auto-generated header | `R/adaptivelasso.h.R` | Compiled from YAML |
| Test data generator | `data-raw/create_adaptivelasso_test_data.R` | Synthetic datasets |
| Test datasets | `data/adaptivelasso_test_data.rda` (n=180), `data/adaptivelasso_small_data.rda` (n=40), `data/adaptivelasso_highdim_data.rda` | RDA test data |
| Testing guide | `vignettes/testing_adaptivelasso.md` | Test scenarios with option coverage |
| Developer documentation | `vignettes/adaptivelasso-documentation.md` | Full UI→Options→Backend→Results mapping |
