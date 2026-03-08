# ncvregcox: SCAD/MCP Cox Regression -- Developer Documentation

## Feature Summary

The `ncvregcox` function performs penalized Cox proportional hazards regression using non-convex penalties (SCAD and MCP) as well as the Lasso for automatic variable selection in survival data. It is aimed at high-dimensional clinicopathological data where identifying a parsimonious set of prognostic variables is the primary goal.

The function wraps the `ncvreg` package's `cv.ncvsurv()` and `ncvsurv()` functions within the standard jamovi 4-file architecture, providing cross-validated penalty tuning, model comparison (lambda.min vs lambda.1se), coefficient tables with hazard ratios, variable importance analysis, and four diagnostic plots.

---

## Feature Details

| Feature | Description | Implementation |
|---------|-------------|----------------|
| **SCAD penalty** | Smoothly Clipped Absolute Deviation. Oracle properties: selects the correct model with probability approaching 1, with unbiased estimates for large coefficients. Default penalty. | `ncvreg::ncvsurv(penalty = "SCAD")` |
| **MCP penalty** | Minimax Concave Penalty. Similar oracle properties to SCAD with a different functional form. More aggressive sparsity for a given gamma. | `ncvreg::ncvsurv(penalty = "MCP")` |
| **Lasso penalty** | L1 penalty for comparison. Known to over-select under collinearity and bias large coefficients toward zero. | `ncvreg::ncvsurv(penalty = "lasso")` |
| **Gamma parameter** | Controls concavity of SCAD/MCP. Lower gamma = more aggressive penalization of medium coefficients. SCAD default 3.7 (Fan & Li, 2001). Range: 1.1-10.0. | Passed directly to `ncvsurv(gamma = ...)` |
| **Alpha (elastic net mixing)** | Blends the chosen penalty with a ridge (L2) component. alpha=1 is pure penalty; alpha=0 is pure ridge. Intermediate values help with correlated predictors. | `ncvsurv(alpha = ...)` |
| **Cross-validation** | K-fold CV to select optimal lambda. Configurable from 3 to 20 folds. | `ncvreg::cv.ncvsurv(nfolds = ...)` |
| **Lambda selection** | Two strategies: `min` (minimizes CV error) or `1se` (one-standard-error rule for sparser model). | `cv_fit$lambda.min` or `cv_fit$lambda.1se` |
| **Standardization** | Standardizes covariates before fitting so that the penalty is scale-invariant. Recommended when variables are on different scales. | `ncvsurv(standardize = ...)` |
| **Regularization path plot** | Coefficient paths vs log(lambda). Shows how variables enter/exit the model. | `.plot_regularization_path` renderFun |
| **CV error plot** | Cross-validation error vs log(lambda) with error bars and vertical lines at lambda.min/lambda.1se. | `.plot_cv_error` renderFun |
| **Variable importance** | Ranks variables by absolute coefficient magnitude with relative importance (%). | `.populate_variable_importance()` method |
| **Variable selection stability plot** | Bar plot of importance scores for selected variables. | `.plot_variable_selection` renderFun |
| **Coefficient comparison plot** | Coefficient magnitudes in the final model. Always visible. | `.plot_coefficient_comparison` renderFun |
| **Model comparison table** | Side-by-side comparison of lambda.min and lambda.1se models (CV error, number of variables, C-index, AIC). | `.populate_model_comparison()` method |
| **Convergence info** | Reports convergence status, algorithm, and tolerance. | `.populate_convergence_info()` method |
| **Clinical interpretation** | HTML summary with variable selection results, clinical implications, and methodological notes. | `.create_model_interpretation()` method |

---

## Internal Architecture

### 4-File Structure

```
jamovi/ncvregcox.a.yaml    -- Option definitions (11 options)
jamovi/ncvregcox.r.yaml    -- Results definitions (6 tables, 4 plots, 2 HTML)
jamovi/ncvregcox.u.yaml    -- UI layout (3 collapse boxes)
R/ncvregcox.b.R            -- Backend R6 class (ncvregcoxClass)
R/ncvregcox.h.R            -- Auto-generated from YAML (do not edit)
```

### Class Hierarchy

```
jmvcore::Analysis
  |
  +-- ncvregcoxBase        (auto-generated in .h.R)
        |
        +-- ncvregcoxClass (defined in .b.R)
```

### Execution Flow

```
.init()
  +-- .update_instructions()        --> sets instructions HTML

.run()
  +-- guard: check time, event, covariates are set
  +-- .prepare_data()               --> creates analysis_data, handles NA
  +-- .fit_ncvreg_cox()             --> fits cv.ncvsurv + ncvsurv
  +-- .populate_results()
  |     +-- .populate_model_summary()
  |     +-- .populate_selected_variables()
  |     +-- .populate_cross_validation_results()
  |     +-- .populate_model_comparison()
  |     +-- .populate_convergence_info()
  |     +-- .create_model_interpretation()
  |     +-- .populate_variable_importance()     (if option enabled)
  +-- .create_plots()               (placeholder; actual rendering in renderFun)

Render functions (called by jamovi image framework):
  .plot_regularization_path()
  .plot_cv_error()
  .plot_variable_selection()
  .plot_coefficient_comparison()
```

### Private Fields

| Field | Type | Set By | Used By |
|-------|------|--------|---------|
| `.analysis_data` | data.frame | `.prepare_data()` | `.fit_ncvreg_cox()` |
| `.covariates` | character vector | `.prepare_data()` | `.fit_ncvreg_cox()`, `.populate_*()` |
| `.cv_fit` | cv.ncvsurv object | `.fit_ncvreg_cox()` | `.populate_*()`, plot render funs |
| `.final_fit` | ncvsurv object | `.fit_ncvreg_cox()` | `.populate_*()`, plot render funs |
| `.lambda_opt` | numeric | `.fit_ncvreg_cox()` | `.populate_*()` |
| `.X` | matrix | `.fit_ncvreg_cox()` | `.populate_model_comparison()` |
| `.Y` | Surv object | `.fit_ncvreg_cox()` | `.populate_model_comparison()` |

### State Management for Plots

The four plot render functions (`.plot_regularization_path`, `.plot_cv_error`, `.plot_variable_selection`, `.plot_coefficient_comparison`) are declared in `ncvregcox.r.yaml` as `renderFun` properties on `Image` items. They are invoked by jamovi's image rendering framework.

Key points for plot state:

- The private fields `.cv_fit`, `.final_fit`, and `.lambda_opt` must be populated before render functions execute.
- These fields are set during `.run()` via `.fit_ncvreg_cox()`.
- If `image$setState()` is used, convert data to base `data.frame` before storing to avoid protobuf serialization issues (the standard ClinicoPath pattern).
- Plot visibility is controlled by YAML expressions: `(plot_path)`, `(plot_cv)`, `(variable_importance)`, or `true`.

Example state-safe pattern:
```r
.plot_regularization_path = function(image, ggtheme, theme, ...) {
    if (is.null(private$.cv_fit)) return(FALSE)

    plot_data <- as.data.frame(
        t(private$.cv_fit$fit$beta)
    )
    plot_data$lambda <- private$.cv_fit$lambda

    image$setState(as.data.frame(plot_data))  # base data.frame for protobuf safety

    # ... ggplot2 or base plot code ...

    return(TRUE)
}
```

### Data Handling

The `.prepare_data()` method:

1. Extracts `time`, `event`, and `covariates` columns from `self$data`.
2. Creates a new data.frame with standardized internal names (`time`, `event`, plus original covariate names).
3. Calls `na.omit()` for complete-case analysis.
4. Stores the result in `private$.analysis_data`.

The event variable is a **factor** (as specified in `ncvregcox.a.yaml` with `permitted: factor`). The auto-generated wrapper in `.h.R` coerces it via `as.factor()`. When passed to `survival::Surv()`, R converts factor to numeric internally. Factor levels "0"/"1" map to numeric 0/1 correctly.

The covariates accept both numeric and factor types. When factors are passed to `ncvreg::ncvsurv()` via `as.matrix()`, they are expanded to dummy variables automatically by R's matrix coercion. Note: this means a 4-level factor (e.g., t_stage) adds 3 columns to the design matrix.

---

## Dependencies

### Required R Packages

| Package | Role | Used In |
|---------|------|---------|
| `ncvreg` | Core penalized regression engine. Provides `ncvsurv()` and `cv.ncvsurv()` for SCAD/MCP/Lasso Cox models. | `.fit_ncvreg_cox()` |
| `survival` | Provides `Surv()` for creating survival objects. | `.fit_ncvreg_cox()` |
| `jmvcore` | jamovi core framework. R6 class infrastructure, table/plot/HTML output. | Entire module |
| `R6` | R6 class system for OOP. | Class definition |

### ncvreg Package Details

- **CRAN:** https://cran.r-project.org/package=ncvreg
- **Key functions:** `ncvsurv()`, `cv.ncvsurv()`, `coef.ncvsurv()`
- **Version requirement:** >= 3.14.0 (for `cv.ncvsurv` with `returnY` parameter)
- **License:** GPL-3

### References in YAML

The `ncvregcox.r.yaml` file declares:
```yaml
refs:
    - ClinicoPathJamoviModule
    - survival
    - ncvreg
```

---

## File Locations

All paths are relative to the ClinicoPathJamoviModule repository root.

### Core Module Files

| File | Purpose |
|------|---------|
| `jamovi/ncvregcox.a.yaml` | Analysis options definition (11 options) |
| `jamovi/ncvregcox.r.yaml` | Results/output definition (6 tables, 4 images, 2 HTML) |
| `jamovi/ncvregcox.u.yaml` | User interface layout (3 collapse boxes) |
| `R/ncvregcox.b.R` | Backend implementation (ncvregcoxClass R6 class) |
| `R/ncvregcox.h.R` | Auto-generated header (do not edit directly) |

### Test and Data Files

| File | Purpose |
|------|---------|
| `tests/testthat/test-ncvregcox.R` | Unit test |
| `data-raw/create_ncvregcox_test_data.R` | Test data generation script |
| `data/ncvregcox_clinical.rda` | Clinical test dataset (n=200, 14 covariates) |
| `data/ncvregcox_sparse.rda` | Sparse high-dimensional test dataset (n=100, 30 covariates) |

### Documentation Files

| File | Purpose |
|------|---------|
| `vignettes/ncvregcox_documentation.md` | This file (developer documentation) |
| `vignettes/testing_ncvregcox.md` | Testing guide with scenarios |
| `man/ncvregcox.Rd` | Auto-generated R documentation |
| `docs/reference/ncvregcox.html` | pkgdown reference page |

### Build Artifacts

| File | Purpose |
|------|---------|
| `build/js/ncvregcox.src.js` | Compiled JavaScript for jamovi UI |

---

## Known Limitations and Development Notes

### Standard Error Approximation

The current implementation uses an approximate standard error for penalized coefficients (see `.populate_selected_variables()`, line ~242 of `ncvregcox.b.R`):

```r
se <- abs(coeff) * 0.1  # Placeholder
```

This is a known simplification. Proper standard errors for penalized estimators are non-trivial because the sampling distribution of penalized coefficients is not normal. Future improvements could use:
- Bootstrap resampling for confidence intervals
- The `ncvreg::summary.ncvsurv()` method if available
- Post-selection inference methods (e.g., selective inference)

### Deviance Explained Calculation

The deviance explained percentage in `model_summary` uses an approximate calculation based on CV error relative to variance of survival times. This is a rough proxy, not a formal R-squared analog for Cox models.

### C-index Approximation

The C-index in the `model_comparison` table is also approximated. A proper implementation would use `survival::concordance()` or `Hmisc::rcorr.cens()` on the predicted linear predictor from the fitted model.

### Factor Variable Names in Output

When categorical variables are included, `ncvreg` internally expands them to dummy variables. The coefficient names in the output may show the dummy variable names (e.g., `t_stageT3`, `t_stageT4`) rather than the original variable name. The `.populate_selected_variables()` method maps these back using `private$.covariates`, but this may not correctly handle all factor expansion patterns.

### Plot Render Functions

The `.create_plots()` method in the current `.b.R` is a placeholder. The actual plot rendering is handled by the four `renderFun` functions declared in `.r.yaml`. These render functions need access to `private$.cv_fit` and `private$.final_fit`, which are set during `.run()`. If the analysis has not run (e.g., required variables not assigned), the render functions should return `FALSE` to suppress the plot.
