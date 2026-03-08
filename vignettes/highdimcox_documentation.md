# highdimcox -- High-Dimensional Cox Regression Documentation

## Feature Summary

The `highdimcox` analysis provides regularized Cox proportional hazards
regression for survival data with many predictors. It is designed for the
classic "p close to or larger than n" scenario found in genomic, proteomic, and
other high-throughput biomedical studies. The module supports four
regularization strategies (LASSO, Ridge, Elastic Net, Adaptive LASSO),
cross-validated lambda selection, optional stability selection via bootstrap
resampling, and multiple diagnostic visualizations.

---

## Feature Details

| Feature | YAML Arg | UI Label | Results Section | R Function / Package |
|---|---|---|---|---|
| Time variable | `elapsedtime` | Time Variable | -- (input) | `survival::Surv()` |
| Event variable | `outcome` | Event Variable | -- (input) | `survival::Surv()` |
| Predictor variables | `predictors` | High-Dimensional Predictors | -- (input) | predictor matrix |
| Event level | `outcomeLevel` | Event Level | -- (input) | factor comparison |
| LASSO (L1) | `regularization_method = "lasso"` | LASSO (L1) | Regularization Metrics | `glmnet::cv.glmnet(alpha=1)` |
| Ridge (L2) | `regularization_method = "ridge"` | Ridge (L2) | Regularization Metrics | `glmnet::cv.glmnet(alpha=0)` |
| Elastic Net | `regularization_method = "elastic_net"` | Elastic Net (L1+L2) | Regularization Metrics | `glmnet::cv.glmnet(alpha=user)` |
| Adaptive LASSO | `regularization_method = "adaptive_lasso"` | Adaptive LASSO | Regularization Metrics | `glmnet::cv.glmnet()` with weights |
| Alpha parameter | `alpha_value` | Elastic Net Alpha | Regularization Metrics | `glmnet::cv.glmnet(alpha=...)` |
| Standard K-Fold CV | `cv_method = "cv_glmnet"` | Standard K-Fold CV | Regularization Metrics, CV Plot | `glmnet::cv.glmnet()` |
| 1-SE Rule CV | `cv_method = "cv_1se"` | 1-SE Rule CV | Regularization Metrics | `cv_fit$lambda.1se` |
| Minimum CV Error | `cv_method = "cv_min"` | Minimum CV Error | Regularization Metrics | `cv_fit$lambda.min` |
| CV Folds | `cv_folds` | CV Folds | Model Summary | `glmnet::cv.glmnet(nfolds=...)` |
| No additional selection | `variable_selection = "none"` | No Additional Selection | -- | -- |
| Stepwise selection | `variable_selection = "stepwise"` | Stepwise Selection | Dimensionality Summary | `stats::step()` (post-reg) |
| Best subset | `variable_selection = "best_subset"` | Best Subset Selection | Dimensionality Summary | subset enumeration |
| Forward selection | `variable_selection = "forward_selection"` | Forward Selection | Dimensionality Summary | forward addition |
| Stability selection | `stability_selection` | Perform Stability Selection | Stability Results, Stability Plot | bootstrap + LASSO |
| Bootstrap iterations | `bootstrap_iterations` | Bootstrap Iterations | Stability Results | resampling loop |
| Stability threshold | `stability_threshold` | Stability Threshold | Stability Results | selection probability cutoff |
| Regularization path plot | `show_regularization_path` | Show Regularization Path | `regularizationPath` (Image) | `glmnet::glmnet()` coefficients |
| CV error plot | `show_cv_plot` | Show Cross-Validation Plot | `cvPlot` (Image) | `glmnet::cv.glmnet()` |
| Variable importance plot | `show_variable_importance` | Show Variable Importance | `variableImportance` (Image) | absolute coefficient bar chart |
| Coefficients table | `show_coefficients_table` | Show Coefficients Table | `selectedVariables` (Table), `modelSummary` (Html) | coefficient extraction |
| Model diagnostics plot | `show_model_diagnostics` | Show Model Diagnostics | `modelDiagnostics` (Image) | concordance, residuals |
| Analysis summaries | `showSummaries` | Analysis Summaries | `analysisSummary` (Html) | natural language generation |
| Method explanations | `showExplanations` | Method Explanations | `methodExplanation` (Html) | static HTML content |

---

## Internal Architecture

### Data Flow

```
User Input (jamovi UI / R wrapper)
  |
  v
.validateInputs()
  - Check elapsedtime, outcome, predictors are provided
  - Verify n >= MIN_OBSERVATIONS (30)
  |
  v
.prepareData()
  - Extract time, event, predictor columns
  - Convert factor event -> binary numeric via outcomeLevel match
  - Convert factor predictors -> numeric
  - Remove rows with missing values
  - Create survival::Surv() object
  - Return: list(survival, predictors, time, event, n_obs, n_vars, var_names)
  |
  v
.performVariableScreening()
  - If n_vars <= 1000: skip (return all variables)
  - If n_vars > 1000: marginal Cox screening
    - Fit univariate coxph for each variable
    - Select top min(n/2, 500) by p-value
  - Return: list(screened_vars, screening_performed, n_screened)
  |
  v
.performHighDimCoxRegression()
  - Set alpha based on regularization_method:
      lasso -> 1.0, ridge -> 0.0, elastic_net -> user alpha, adaptive_lasso -> 1.0 (with weights)
  - Run glmnet::cv.glmnet(family="cox", alpha=alpha, nfolds=cv_folds)
  - Select lambda: cv_min -> lambda.min, cv_1se/cv_glmnet -> lambda.1se
  - Fit full path with glmnet::glmnet()
  - Extract coefficients at selected lambda
  - Identify non-zero variables
  - Return: list(cv_fit, final_fit, selected_lambda, coefficients,
                  selected_variables, variable_importance, n_selected)
  |
  v
[Optional] .performStabilitySelection()
  - Loop bootstrap_iterations times:
      - Subsample 80% of observations (without replacement)
      - Fit cv.glmnet(alpha=1.0, nfolds=5) on subsample
      - Record which variables have non-zero coefficients
  - Compute selection probability = proportion of times each variable was selected
  - Flag variables with probability >= stability_threshold as "stable"
  - Return: list(selection_probabilities, stable_variables, n_stable)
  |
  v
.populateResults()
  - .populateModelSummary()      -> modelSummary (Html)
  - .populateVariablesTable()    -> selectedVariables (Table)
  - .populateRegularizationMetrics() -> regularizationMetrics (Table)
  - .populateDimensionalityReduction() -> dimensionalityReduction (Table)
  - .populateStabilityResults()  -> stabilityResults (Table)
  |
  v
.createPlots()
  - .createRegularizationPath()       -> regularizationPath (Image)
  - .createCVPlot()                   -> cvPlot (Image)
  - .createVariableImportancePlot()   -> variableImportance (Image)
  - .createModelDiagnostics()         -> modelDiagnostics (Image)
  - .createStabilityPlot()            -> stabilityPlot (Image)
  |
  v
[Optional] .generateSummaries()    -> analysisSummary (Html)
[Optional] .generateExplanations() -> methodExplanation (Html)
```

### R6 Class Hierarchy

```
jmvcore::Analysis
  |
  +-- highdimcoxBase  (auto-generated from YAML, defined in highdimcox.h.R)
        |
        +-- highdimcoxClass  (backend implementation in highdimcox.b.R)
              - private$.init()
              - private$.run()
              - private$.validateInputs()
              - private$.prepareData()
              - private$.performVariableScreening()
              - private$.performHighDimCoxRegression()
              - private$.performStabilitySelection()
              - private$.initializeResultTables()
              - private$.populateResults()
              - private$.populateModelSummary()
              - private$.populateVariablesTable()
              - private$.populateRegularizationMetrics()
              - private$.populateDimensionalityReduction()
              - private$.populateStabilityResults()
              - private$.createPlots()
              - private$.createRegularizationPath()
              - private$.createCVPlot()
              - private$.createVariableImportancePlot()
              - private$.createModelDiagnostics()
              - private$.createStabilityPlot()
              - private$.generateSummaries()
              - private$.generateExplanations()
```

---

## State Management for Plots

All plot Image items use protobuf-safe state serialization. The backend
extracts only plain numeric vectors and character vectors from model objects
before calling `image$setState()`. This avoids the serialization errors caused
by storing R6/environment/function references in the state.

### Pattern Used

```r
.createCVPlot = function(model_results) {
  tryCatch({
    image <- self$results$cvPlot
    cv_fit <- model_results$cv_fit
    plot_data <- list(
      lambda     = as.numeric(cv_fit$lambda),
      cvm        = as.numeric(cv_fit$cvm),
      cvsd       = as.numeric(cv_fit$cvsd),
      cvup       = as.numeric(cv_fit$cvup),
      cvlo       = as.numeric(cv_fit$cvlo),
      lambda_min = as.numeric(cv_fit$lambda.min),
      lambda_1se = as.numeric(cv_fit$lambda.1se),
      selected_lambda = as.numeric(model_results$selected_lambda)
    )
    image$setState(plot_data)
  }, error = function(e) {
    # Skip plot on error -- does not block the analysis
  })
}
```

Key rules:

1. Always wrap in `tryCatch` so a single plot failure does not crash the
   analysis.
2. Cast every value to `as.numeric()` or `as.character()` -- never store raw
   model objects, environments, or functions.
3. Do not store the full `cv.glmnet` or `glmnet` object in state; extract only
   the vectors needed for rendering.

### Plot State Contents

| Plot | State Keys |
|---|---|
| `regularizationPath` | `lambda`, `cvm`, `cvsd`, `lambda_min`, `lambda_1se` |
| `cvPlot` | `lambda`, `cvm`, `cvsd`, `cvup`, `cvlo`, `lambda_min`, `lambda_1se`, `selected_lambda` |
| `variableImportance` | `var_names`, `importance`, `selected_vars` |
| `modelDiagnostics` | `n_selected`, `selected_vars`, `concordance` |
| `stabilityPlot` | `selection_frequencies`, `var_names`, `threshold` |

---

## Dependencies

| Package | Version | Purpose |
|---|---|---|
| `survival` | >= 3.5 | `Surv()` object, `coxph()` for screening |
| `glmnet` | >= 4.1 | `cv.glmnet()`, `glmnet()` for regularized Cox |
| `jmvcore` | >= 2.4 | jamovi framework (R6 base classes, options, results) |
| `R6` | >= 2.5 | R6 class system |

No additional packages are required. The stability selection and variable
screening steps use only `survival` and `glmnet`.

---

## File Locations

All paths are relative to the repository root
(`ClinicoPathJamoviModule/`).

| File | Purpose |
|---|---|
| `jamovi/highdimcox.a.yaml` | Analysis definition -- all options with types, defaults, constraints |
| `jamovi/highdimcox.u.yaml` | UI definition -- widget layout for jamovi interface |
| `jamovi/highdimcox.r.yaml` | Results definition -- tables, images, HTML items with visibility bindings |
| `R/highdimcox.h.R` | Auto-generated header -- `highdimcoxOptions`, `highdimcoxResults`, `highdimcoxBase`, `highdimcox()` wrapper |
| `R/highdimcox.b.R` | Backend implementation -- `highdimcoxClass` R6 class with all private methods |
| `build/js/highdimcox.src.js` | Auto-generated JavaScript for jamovi UI |
| `tests/testthat/test-highdimcox.R` | Unit tests -- class loading, basic input validation |
| `man/highdimcox.Rd` | Auto-generated R documentation for the wrapper function |
| `man/highdimcoxClass.Rd` | Auto-generated R documentation for the R6 class |
| `data-raw/create_highdimcox_test_data.R` | Test data generation script |
| `data/highdimcox_genomic.rda` | Genomic test dataset (n=150, p=100 genes + 5 clinical) |
| `data/highdimcox_proteomic.rda` | Proteomic test dataset (n=80, p=50 proteins + 3 clinical) |
| `vignettes/testing_highdimcox.md` | Testing guide with 26 numbered scenarios |
| `vignettes/highdimcox_documentation.md` | This file |

---

## Results Output Reference

### Tables

| Name | Title | Visible When | Columns |
|---|---|---|---|
| `selectedVariables` | Selected Variables | `show_coefficients_table` | variable (text), beta (number), HR (number), importance (number) |
| `regularizationMetrics` | Regularization Metrics | always | metric (text), value (text), interpretation (text) |
| `stabilityResults` | Stability Selection Results | `stability_selection` | variable (text), selection_probability (number), stable (text), importance_rank (integer) |
| `dimensionalityReduction` | Dimensionality Summary | always | stage (text), variables_input (integer), variables_selected (integer), reduction_ratio (number) |

### Images (Plots)

| Name | Title | Visible When |
|---|---|---|
| `regularizationPath` | Regularization Path | `show_regularization_path` |
| `cvPlot` | Cross-Validation Plot | `show_cv_plot` |
| `variableImportance` | Variable Importance Plot | `show_variable_importance` |
| `modelDiagnostics` | Model Diagnostics | `show_model_diagnostics` |
| `stabilityPlot` | Stability Selection Plot | `stability_selection` |

### HTML Items

| Name | Title | Visible When |
|---|---|---|
| `todo` | Analysis | always (cleared when analysis runs) |
| `modelSummary` | Model Summary | `show_coefficients_table` |
| `analysisSummary` | Analysis Summary | `showSummaries` |
| `methodExplanation` | Methodology | `showExplanations` |
