# ncvregcox: SCAD/MCP Cox Regression -- Developer Documentation

> **Version:** 0.0.31 | **Last updated:** 2026-03-12 | **Status:** Release-ready

---

## 1. Overview

The `ncvregcox` function performs penalized Cox proportional hazards regression using **non-convex penalties only** -- SCAD (Smoothly Clipped Absolute Deviation) and MCP (Minimax Concave Penalty) -- for automatic variable selection in survival data. It targets high-dimensional clinicopathological data where identifying a parsimonious set of prognostic variables is the primary goal.

The function wraps the `ncvreg` package's `cv.ncvsurv()` and `ncvsurv()` functions within the standard jamovi 4-file architecture, providing cross-validated penalty tuning, model comparison (lambda.min vs lambda.1se), coefficient tables with hazard ratios, per-variable HR clinical interpretation, variable importance analysis, a data suitability assessment with 6 traffic-light checks, and three diagnostic plots.

### Key Design Decisions

| Decision | Rationale |
|----------|-----------|
| SCAD and MCP only (no Lasso) | The `lassocox` function already covers Lasso. SCAD/MCP have oracle properties that Lasso lacks. Keeping them separate avoids user confusion. |
| `survival::concordance(reverse=TRUE)` for C-index | Higher linear predictor = higher hazard = worse prognosis. `reverse=TRUE` ensures concordance is computed in the standard direction (C > 0.5 = good). |
| AIC from unpenalized Cox refit | Penalized likelihood AIC is not directly comparable across penalties. Refitting `coxph()` on the selected variables gives a standard AIC for relative comparison. |
| Protobuf-safe plot state | Model objects (cv.ncvsurv, ncvsurv) contain environments and function references that cannot be serialized by jamovi's protobuf system. Only plain vectors and data.frames are stored in `image$setState()`. |
| MCP gamma auto-adjustment | SCAD default gamma is 3.7 (Fan & Li 2001), MCP default is 3.0 (Zhang 2010). When the user selects MCP but leaves gamma at the SCAD default of 3.7, the backend silently switches to 3.0. |

---

## 1a. Changelog

| Date | Change |
|------|--------|
| 2026-03-12 | Documentation rewrite for v0.0.31 (this file). |
| 2026-03-10 | Removed Lasso penalty option; SCAD and MCP only. Added `outcomeLevel`/`censorLevel` Level selectors. Added `suitabilityCheck` option and 6-check traffic-light assessment. Added `.lambda_1se_fallback` tracking. Added per-variable HR interpretation in model_interpretation HTML. Added copy-ready report sentence. Added jmvcore::Notice integration. Protobuf-safe plot state management. Convergence iteration extraction handles per-lambda vector. Variable importance footnote when `standardize=FALSE`. |
| 2026-03-07 | Initial implementation with SCAD/MCP/Lasso, 11 options, 12 outputs including coefficient comparison plot. |

---

## 2. UI Controls to Options Map

The `.u.yaml` file defines the jamovi sidebar layout. Below is the exact mapping from each UI widget to the option it controls.

### VariableSupplier (top section)

| UI Widget | Type | Option Name | Notes |
|-----------|------|-------------|-------|
| Time Variable | VariablesListBox | `time` | maxItemCount: 1 |
| Event Variable | VariablesListBox | `event` | maxItemCount: 1 |
| Event Level | LevelSelector | `outcomeLevel` | enable: `(event)` -- only active when event is assigned |
| Censored Level | LevelSelector | `censorLevel` | enable: `(event)` |
| Covariates | VariablesListBox | `covariates` | no maxItemCount (accepts many) |

### CollapseBox: Data Suitability (collapsed: false)

| UI Widget | Type | Option Name |
|-----------|------|-------------|
| Data Suitability Assessment | CheckBox | `suitabilityCheck` |

### CollapseBox: Model Options (collapsed: false)

| UI Widget | Type | Option Name |
|-----------|------|-------------|
| Penalty Function | ComboBox | `penalty` |
| Cross-Validation Folds | TextBox (number) | `cv_folds` |
| Lambda Selection | ComboBox | `lambda_type` |

### CollapseBox: Penalty Parameters (collapsed: true)

| UI Widget | Type | Option Name |
|-----------|------|-------------|
| Elastic Net Mixing Parameter | TextBox (number) | `alpha` |
| SCAD/MCP Tuning Parameter | TextBox (number) | `gamma` |
| Standardize Variables | CheckBox | `standardize` |

### CollapseBox: Plots and Output (collapsed: true)

| UI Widget | Type | Option Name |
|-----------|------|-------------|
| Show Regularization Path | CheckBox | `plot_path` |
| Show Cross-Validation Plot | CheckBox | `plot_cv` |
| Variable Importance Analysis | CheckBox | `variable_importance` |

---

## 3. Options Reference

There are **15 options** defined in `ncvregcox.a.yaml`. The implicit `data` option is always present.

| # | Name | Type | Default | Constraints | YAML Key | Description |
|---|------|------|---------|-------------|----------|-------------|
| 1 | `data` | Data | -- | -- | `data` | The dataset (implicit, always present) |
| 2 | `time` | Variable | -- | suggested: continuous; permitted: numeric | `time` | Survival time variable |
| 3 | `event` | Variable | -- | suggested: nominal; permitted: factor | `event` | Event/status indicator |
| 4 | `outcomeLevel` | Level | -- | variable: `(event)` | `outcomeLevel` | Which level of `event` means "event occurred" |
| 5 | `censorLevel` | Level | -- | variable: `(event)` | `censorLevel` | Which level of `event` means "censored" |
| 6 | `covariates` | Variables | -- | suggested: continuous, nominal; permitted: numeric, factor | `covariates` | Predictor variables |
| 7 | `penalty` | List | `"SCAD"` | options: `"SCAD"`, `"MCP"` | `penalty` | Non-convex penalty type |
| 8 | `cv_folds` | Number | `10` | min: 3, max: 20 | `cv_folds` | K-fold cross-validation folds |
| 9 | `lambda_type` | List | `"min"` | options: `"min"`, `"1se"` | `lambda_type` | Lambda selection strategy |
| 10 | `alpha` | Number | `1.0` | min: 0.01, max: 1.0 | `alpha` | Elastic net mixing (1 = pure penalty, near 0 = ridge-like) |
| 11 | `gamma` | Number | `3.7` | min: 1.1, max: 10.0 | `gamma` | Concavity tuning parameter |
| 12 | `standardize` | Bool | `true` | -- | `standardize` | Standardize covariates before fitting |
| 13 | `plot_path` | Bool | `true` | -- | `plot_path` | Show regularization path plot |
| 14 | `plot_cv` | Bool | `true` | -- | `plot_cv` | Show CV error plot |
| 15 | `variable_importance` | Bool | `true` | -- | `variable_importance` | Compute variable importance table + plot |
| 16 | `suitabilityCheck` | Bool | `true` | -- | `suitabilityCheck` | Run data suitability assessment |

### Notes on Specific Options

- **`outcomeLevel` / `censorLevel`**: If left empty, the backend resolves them automatically -- `outcomeLevel` defaults to the second observed level, `censorLevel` to the first. Rows matching neither level are excluded (strict two-level encoding).
- **`penalty`**: Only `"SCAD"` and `"MCP"` are available. Lasso is handled by the separate `lassocox` function.
- **`gamma`**: When `penalty == "MCP"` and `gamma == 3.7` (the SCAD default), the backend auto-adjusts to 3.0 (MCP recommended default per Zhang 2010).
- **`alpha`**: ncvreg requires `alpha > 0`. Pure ridge (`alpha = 0`) is not supported. Values near 0 blend toward ridge behavior, which helps with correlated predictors.

---

## 4. Backend Usage

### 4-File Structure

```
jamovi/ncvregcox.a.yaml    -- Option definitions (16 options incl. data)
jamovi/ncvregcox.r.yaml    -- Results definitions (6 tables, 3 images, 3 HTML)
jamovi/ncvregcox.u.yaml    -- UI layout (1 VariableSupplier + 4 CollapseBoxes)
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

### Private Fields

| Field | Type | Set By | Used By |
|-------|------|--------|---------|
| `.analysis_data` | data.frame | `.prepare_data()` | `.fit_ncvreg_cox()`, `.assessSuitability()`, `.add_post_analysis_notices()`, `.create_model_interpretation()` |
| `.covariates` | character vector | `.prepare_data()` | `.fit_ncvreg_cox()`, `.assessSuitability()`, `.populate_variable_importance()`, `.create_model_interpretation()` |
| `.cv_fit` | cv.ncvsurv object | `.fit_ncvreg_cox()` | `.populate_model_summary()`, `.populate_cross_validation_results()`, `.populate_model_comparison()`, `.create_plots()` |
| `.final_fit` | ncvsurv object | `.fit_ncvreg_cox()` | `.populate_*()`, `.create_plots()`, `.create_model_interpretation()` |
| `.lambda_opt` | numeric | `.fit_ncvreg_cox()` | `.populate_*()`, `.create_plots()`, `.create_model_interpretation()` |
| `.X` | matrix | `.fit_ncvreg_cox()` | `.populate_model_summary()`, `.populate_model_comparison()`, `.add_post_analysis_notices()`, `.create_model_interpretation()` |
| `.Y` | Surv object | `.fit_ncvreg_cox()` | `.populate_model_comparison()`, `.add_post_analysis_notices()`, `.create_model_interpretation()` |
| `.warnings_collected` | character vector | `.run()` | `.append_warnings_html()`, `.run()` (post-analysis notice) |
| `.lambda_1se_fallback` | logical | `.fit_ncvreg_cox()` | `.add_post_analysis_notices()` |

### Dependencies

| Package | Role | Used In |
|---------|------|---------|
| `ncvreg` (>= 3.14.0) | Core penalized regression engine. Provides `ncvsurv()` and `cv.ncvsurv()`. | `.fit_ncvreg_cox()` |
| `survival` | `Surv()` for survival objects, `concordance()` for C-index, `coxph()` for AIC refit. | `.fit_ncvreg_cox()`, `.populate_model_summary()`, `.populate_model_comparison()` |
| `htmltools` | `htmlEscape()` for safe HTML output of variable names. | `.create_model_interpretation()`, `.append_warnings_html()` |
| `ggplot2` | All three plot render functions. | `.plot_regularization_path()`, `.plot_cv_error()`, `.plot_variable_selection()` |
| `jmvcore` | jamovi framework: R6 classes, Table, Image, Html, Notice. | Entire module |
| `R6` | R6 class system. | Class definition |

### Key Implementation Details

#### C-index Computation
```r
lp <- as.numeric(private$.X %*% coeffs)
ci <- survival::concordance(private$.Y ~ lp, reverse = TRUE)
```
`reverse = TRUE` is critical because higher linear predictor means higher hazard (worse prognosis). Without it, C-index would be inverted (1 - true_C).

#### AIC in Model Comparison
```r
sel <- which(beta_at_lambda != 0)
sel_X <- private$.X[, sel, drop = FALSE]
refit <- survival::coxph(private$.Y ~ sel_X)
AIC(refit)
```
AIC is from an **unpenalized Cox refit** on the variables selected at each lambda. A table note explains this is approximate and intended for relative comparison only.

#### MCP Gamma Auto-Adjustment
```r
gamma_val <- self$options$gamma
if (penalty_type == "MCP" && gamma_val == 3.7) {
    gamma_val <- 3.0
}
```
When the user switches from SCAD to MCP but does not change gamma from its SCAD default (3.7), the backend substitutes the MCP-recommended default (3.0).

#### Lambda 1-SE Fallback
```r
if (lambda_type == "1se" && !is.null(cv_fit$lambda.1se) && length(cv_fit$lambda.1se) == 1) {
    lambda_opt <- cv_fit$lambda.1se
} else {
    lambda_opt <- cv_fit$lambda.min
    if (lambda_type == "1se") {
        private$.lambda_1se_fallback <- TRUE
    }
}
```
If `lambda.1se` is unavailable (can happen with sparse lambda grids), the code falls back to `lambda.min` and sets the fallback flag. A WARNING notice is emitted during `.add_post_analysis_notices()`.

#### Convergence Iteration Extraction
```r
iter_vec <- final_fit$iter
if (length(iter_vec) > 1) {
    lambda_idx <- which.min(abs(final_fit$lambda - private$.lambda_opt))
    as.integer(iter_vec[lambda_idx])
} else {
    as.integer(iter_vec)
}
```
`ncvreg` stores iterations as a **per-lambda vector**. The code extracts the iteration count at the selected lambda index.

#### Variable Importance Footnote
When `standardize = FALSE`, the variable importance table gets a footnote:
> "Standardization is off. Importance scores reflect raw coefficient magnitude and may not be comparable across variables with different scales."

#### Protobuf-Safe Plot State Management
Model objects are never stored in `image$setState()`. Instead, `.create_plots()` extracts plain vectors and data.frames:

```r
self$results$regularization_path$setState(list(
    beta = as.data.frame(beta_matrix),   # plain data.frame
    lambda = lambda_vec,                  # numeric vector
    var_names = var_names,                # character vector
    lambda_opt = as.numeric(private$.lambda_opt),
    penalty = as.character(self$options$penalty)
))
```

The render functions read from `image$state`, not from private fields, ensuring saved `.omv` files can re-render plots.

#### jmvcore::Notice Integration
The `.insertNotice()` helper wraps `jmvcore::Notice$new()` + `self$results$insert()` in a tryCatch to silently skip if serialization fails (the known protobuf issue with Notice objects containing function references):

```r
.insertNotice = function(name, type, content, position = 999) {
    tryCatch({
        notice <- jmvcore::Notice$new(options = self$options, name = name, type = type)
        notice$setContent(content)
        self$results$insert(position, notice)
    }, error = function(e) NULL)
}
```

Notices are emitted for: analysis errors (ERROR), low events (STRONG_WARNING), moderate events (WARNING), no variables selected (WARNING), C-index below 0.5 (STRONG_WARNING), lambda 1-SE fallback (WARNING), missing data exclusion (WARNING), convergence warnings (WARNING), and analysis completion summary (INFO).

---

## 5. Results Definition

There are **12 output items** defined in `ncvregcox.r.yaml`.

| # | Name | Type | Visibility | Description |
|---|------|------|------------|-------------|
| 1 | `instructions` | Html | always | Analysis configuration summary and feature list |
| 2 | `suitabilityReport` | Html | `(suitabilityCheck)` | Traffic-light data suitability assessment (6 checks) |
| 3 | `model_summary` | Table (1 row) | always | Penalty, lambda, CV error, n selected, C-index |
| 4 | `selected_variables` | Table (0+ rows) | always | Variable name, coefficient, hazard ratio for each selected variable |
| 5 | `variable_importance` | Table (0+ rows) | `(variable_importance)` | Importance score, rank, relative importance (%) for non-zero variables |
| 6 | `cross_validation_results` | Table (0+ rows) | always | Lambda grid summary (up to 10 key points) |
| 7 | `model_comparison` | Table (0+ rows) | always | Lambda Min vs Lambda 1-SE comparison: CV error, n_vars, C-index, AIC |
| 8 | `convergence_info` | Table (1 row) | always | Converged (yes/no), iterations, tolerance, algorithm |
| 9 | `regularization_path` | Image (600x450) | `(plot_path)` | Coefficient paths vs log(lambda) with dashed line at selected lambda |
| 10 | `cv_error_plot` | Image (600x450) | `(plot_cv)` | CV error +/- SE vs log(lambda) with lambda.min (red) and lambda.1se (green) lines |
| 11 | `variable_selection_plot` | Image (600x450) | `(variable_importance)` | Horizontal bar chart of importance scores for selected variables (top 30) |
| 12 | `model_interpretation` | Html | always | Per-variable HR interpretation, clinical implications, methodological notes, copy-ready report sentence, warnings section |

### Table Column Details

**model_summary:**

| Column | Title | Type |
|--------|-------|------|
| `penalty` | Penalty Function | text |
| `lambda_selected` | Selected Lambda | number |
| `cv_error` | CV Error | number |
| `n_selected` | Variables Selected | integer |
| `c_index` | C-index | number (format: zto) |

**selected_variables:**

| Column | Title | Type |
|--------|-------|------|
| `variable` | Variable | text |
| `coefficient` | Coefficient | number |
| `hazard_ratio` | Hazard Ratio | number |

**variable_importance:**

| Column | Title | Type |
|--------|-------|------|
| `variable` | Variable | text |
| `importance` | Importance Score | number |
| `rank` | Rank | integer |
| `relative_importance` | Relative Importance (%) | number |

**cross_validation_results:**

| Column | Title | Type |
|--------|-------|------|
| `lambda` | Lambda | number |
| `cv_error` | CV Error | number |
| `cv_se` | CV Standard Error | number |
| `n_vars` | Number of Variables | integer |

**model_comparison:**

| Column | Title | Type |
|--------|-------|------|
| `model` | Model | text |
| `lambda` | Lambda | number |
| `cv_error` | CV Error | number |
| `n_vars` | Variables | integer |
| `c_index` | C-index | number |
| `aic` | AIC | number |

**convergence_info:**

| Column | Title | Type |
|--------|-------|------|
| `converged` | Converged | text |
| `iterations` | Iterations | integer |
| `tolerance` | Tolerance | number |
| `algorithm` | Algorithm | text |

### clearWith Dependency Chains

All tables and images (except `instructions`) clear when any of the core model options change: `time`, `event`, `outcomeLevel`, `censorLevel`, `covariates`, `penalty`, `cv_folds`, `lambda_type`, `alpha`, `gamma`, `standardize`.

Additionally:
- `suitabilityReport` also clears on `suitabilityCheck`
- `variable_importance` table and `variable_selection_plot` also clear on `variable_importance`

---

## 6. Data Flow Diagram

```
User assigns variables in jamovi UI
    |
    v
.init()
    +-- .update_instructions()  --> writes instructions Html

.run()
    +-- Guard: time, event, covariates all set?
    |   (if not, silent return)
    |
    +-- Initialize .warnings_collected = character(0)
    |
    +-- withCallingHandlers / tryCatch {
    |       |
    |       +-- .prepare_data()
    |       |       +-- Extract columns from self$data
    |       |       +-- Resolve outcomeLevel/censorLevel
    |       |       +-- Two-level numeric encoding (1=event, 0=censor)
    |       |       +-- na.omit() for complete cases
    |       |       +-- Store in private$.analysis_data, .covariates
    |       |
    |       +-- .assessSuitability()  [if suitabilityCheck == TRUE]
    |       |       +-- 6 traffic-light checks
    |       |       +-- .generateSuitabilityHtml()  --> writes suitabilityReport Html
    |       |
    |       +-- .fit_ncvreg_cox()
    |       |       +-- Build design matrix X (model.matrix for factors)
    |       |       +-- Build Surv object Y
    |       |       +-- Auto-adjust gamma for MCP
    |       |       +-- ncvreg::cv.ncvsurv() --> private$.cv_fit
    |       |       +-- Lambda selection (min or 1se with fallback)
    |       |       +-- Store .cv_fit, .final_fit, .lambda_opt, .X, .Y
    |       |
    |       +-- .populate_results()
    |       |       +-- .populate_model_summary()       --> model_summary Table
    |       |       +-- .populate_selected_variables()   --> selected_variables Table
    |       |       +-- .populate_cross_validation_results() --> cross_validation_results Table
    |       |       +-- .populate_model_comparison()     --> model_comparison Table
    |       |       +-- .populate_convergence_info()     --> convergence_info Table
    |       |       +-- .create_model_interpretation()   --> model_interpretation Html
    |       |       +-- .populate_variable_importance()  --> variable_importance Table [if enabled]
    |       |
    |       +-- .create_plots()
    |               +-- Set protobuf-safe state for regularization_path
    |               +-- Set protobuf-safe state for cv_error_plot
    |               +-- Set protobuf-safe state for variable_selection_plot [if enabled]
    |   }
    |   warnings --> appended to .warnings_collected
    |   errors --> .insertNotice(ERROR) + return early
    |
    +-- .add_post_analysis_notices()
    |       +-- Low events check (< 10: STRONG_WARNING, < 20: WARNING)
    |       +-- No variables selected check (WARNING)
    |       +-- C-index < 0.5 check (STRONG_WARNING)
    |       +-- Lambda 1-SE fallback check (WARNING)
    |       +-- Missing data exclusion check (WARNING)
    |
    +-- .append_warnings_html()  [if warnings collected]
    |       +-- Deduplicate warnings
    |       +-- Append yellow box to model_interpretation Html
    |       +-- .insertNotice(WARNING) for convergence warnings banner
    |
    +-- .insertNotice(INFO) for analysis completion summary

Render functions (called by jamovi image framework, after .run()):
    .plot_regularization_path()  -- reads from image$state, not private fields
    .plot_cv_error()             -- reads from image$state
    .plot_variable_selection()   -- reads from image$state
```

---

## 7. Execution Sequence

### Method Call Order (happy path)

```
1. .init()
2. .update_instructions()
3. .run()
4.   .prepare_data()
5.   .assessSuitability()           [conditional]
6.   .generateSuitabilityHtml()     [conditional]
7.   .fit_ncvreg_cox()
8.   .populate_results()
9.     .populate_model_summary()
10.    .populate_selected_variables()
11.    .populate_cross_validation_results()
12.    .populate_model_comparison()
13.    .populate_convergence_info()
14.    .create_model_interpretation()
15.    .generate_report_sentence()   [called by 14]
16.    .populate_variable_importance()  [conditional]
17.  .create_plots()
18.  .add_post_analysis_notices()
19.  .append_warnings_html()         [conditional]
20. .plot_regularization_path()      [jamovi image framework]
21. .plot_cv_error()                 [jamovi image framework]
22. .plot_variable_selection()       [jamovi image framework]
```

### Error Handling Strategy

The `.run()` method uses a two-layer error handling pattern:

1. **Inner layer** (`withCallingHandlers`): Captures all warnings into `.warnings_collected` and muffles them. This prevents warnings from interrupting the analysis pipeline.
2. **Outer layer** (`tryCatch`): Catches fatal errors, emits an ERROR notice via `.insertNotice()`, and sets `analysis_ok = FALSE` to skip post-analysis processing.

After the main try-catch block, post-analysis notices are emitted for clinical threshold checks (low events, poor discrimination, etc.) and any collected warnings are appended to the model interpretation HTML.

---

## 8. Change Impact Guide

Use this table to find which files need editing when making specific types of changes.

| Change | Files to Edit |
|--------|---------------|
| Add a new option | `ncvregcox.a.yaml`, `ncvregcox.u.yaml` (UI widget), `ncvregcox.b.R` (use it), then regenerate `.h.R` via `jmvtools::prepare()` |
| Add a new output table | `ncvregcox.r.yaml` (define table + columns), `ncvregcox.b.R` (populate method), then regenerate `.h.R` |
| Add a new plot | `ncvregcox.r.yaml` (Image item with renderFun), `ncvregcox.b.R` (render function + state setup in `.create_plots()`), then regenerate `.h.R` |
| Change penalty options | `ncvregcox.a.yaml` (options list), `ncvregcox.u.yaml` (ComboBox items if titles change), `ncvregcox.b.R` (`.format_penalty()` switch, `.fit_ncvreg_cox()`) |
| Change clearWith dependencies | `ncvregcox.r.yaml` (clearWith lists on affected items) |
| Change gamma auto-adjustment logic | `ncvregcox.b.R` (`.fit_ncvreg_cox()` only) |
| Change suitability checks | `ncvregcox.b.R` (`.assessSuitability()` and `.generateSuitabilityHtml()`) |
| Change notice messages | `ncvregcox.b.R` (`.add_post_analysis_notices()` and `.run()`) |
| Change clinical interpretation | `ncvregcox.b.R` (`.create_model_interpretation()` and `.generate_report_sentence()`) |
| Update menu placement | `jamovi/0000.yaml` (find `name: ncvregcox` entry) |
| Add test data | `data-raw/create_ncvregcox_test_data.R` (generate), `data/ncvregcox_*.rda` (output) |

### Files You Should Never Edit Directly

| File | Why |
|------|-----|
| `R/ncvregcox.h.R` | Auto-generated from `.a.yaml` + `.r.yaml` by `jmvtools::prepare()`. Changes will be overwritten. |
| `man/ncvregcox.Rd` | Auto-generated by `devtools::document()` from the roxygen block in `.h.R`. |

---

## 9. Example Usage

### From R Console (wrapper function)

```r
# Load the package
devtools::load_all(".")

# Basic SCAD analysis with clinical data
results <- ncvregcox(
    data = ncvregcox_clinical,
    time = "time",
    event = "event",
    outcomeLevel = "1",
    censorLevel = "0",
    covariates = c("age", "gender", "tumor_size", "grade",
                   "ki67", "er_status", "pr_status"),
    penalty = "SCAD",
    cv_folds = 10,
    lambda_type = "min",
    suitabilityCheck = TRUE
)

# MCP with 1-SE rule for sparser model
results_mcp <- ncvregcox(
    data = ncvregcox_clinical,
    time = "time",
    event = "event",
    outcomeLevel = "1",
    censorLevel = "0",
    covariates = c("age", "gender", "tumor_size", "grade",
                   "ki67", "er_status", "pr_status"),
    penalty = "MCP",
    cv_folds = 10,
    lambda_type = "1se",
    gamma = 3.0
)

# Access specific outputs
results$model_summary$asDF
results$selected_variables$asDF
results$model_comparison$asDF
```

### From R6 Class (direct instantiation for testing)

```r
# Source the files
source("R/ncvregcox.h.R")
source("R/ncvregcox.b.R")

# Create options
opts <- ncvregcoxOptions$new(
    time = "time",
    event = "event",
    outcomeLevel = "1",
    censorLevel = "0",
    covariates = c("x1", "x2", "x3"),
    penalty = "SCAD"
)

# Create and run analysis
analysis <- ncvregcoxClass$new(options = opts, data = my_data)
analysis$run()
analysis$results$model_summary$asDF
```

### Test Datasets

| Dataset | File | n | p | Description |
|---------|------|---|---|-------------|
| `ncvregcox_clinical` | `data/ncvregcox_clinical.rda` | 200 | 14 | Mixed numeric + factor covariates. Realistic clinical scenario. |
| `ncvregcox_sparse` | `data/ncvregcox_sparse.rda` | 100 | 30 | All continuous covariates. High-dimensional sparse setting (p > n/3). |
| `ncvregcox_small` | `data/ncvregcox_small.rda` | 25 | 5 | Edge case: very small sample. Tests low-EPV warnings. |
| `ncvregcox_collinear` | `data/ncvregcox_collinear.rda` | 150 | 10 | All continuous with r > 0.9 pairs. Tests multicollinearity detection. |

### Test Files

| File | Purpose |
|------|---------|
| `tests/testthat/test-ncvregcox.R` | Main unit tests |
| `tests/testthat/test-ncvregcox-basic.R` | Basic functionality tests |
| `tests/testthat/test-ncvregcox-arguments.R` | Option validation and edge cases |
| `tests/testthat/test-ncvregcox-edge-cases.R` | Small samples, no selection, extreme parameters |
| `tests/testthat/test-ncvregcox-integration.R` | End-to-end workflow tests |

---

## 10. Appendix

### A. File Locations

All paths relative to the ClinicoPathJamoviModule repository root.

**Core module files:**

| File | Purpose |
|------|---------|
| `jamovi/ncvregcox.a.yaml` | Analysis options definition (16 options incl. data) |
| `jamovi/ncvregcox.r.yaml` | Results/output definition (6 tables, 3 images, 3 HTML) |
| `jamovi/ncvregcox.u.yaml` | User interface layout (1 VariableSupplier + 4 CollapseBoxes) |
| `R/ncvregcox.b.R` | Backend implementation (ncvregcoxClass, ~1170 lines) |
| `R/ncvregcox.h.R` | Auto-generated header (do not edit) |

**Documentation files:**

| File | Purpose |
|------|---------|
| `vignettes/ncvregcox-documentation.md` | This file (developer documentation) |
| `vignettes/ncvregcox_documentation.md` | Previous version (superseded by this file) |
| `man/ncvregcox.Rd` | Auto-generated R documentation |

**Data and test files:**

| File | Purpose |
|------|---------|
| `data-raw/create_ncvregcox_test_data.R` | Test data generation script |
| `data/ncvregcox_clinical.rda` | Clinical test dataset (n=200, 14 covariates) |
| `data/ncvregcox_sparse.rda` | Sparse high-dimensional test dataset (n=100, 30 covariates) |
| `data/ncvregcox_small.rda` | Small sample edge case dataset (n=25, 5 covariates) |
| `data/ncvregcox_collinear.rda` | Collinear test dataset (n=150, 10 covariates, r > 0.9) |

### B. YAML References

The `ncvregcox.r.yaml` file declares these references:

```yaml
refs:
    - ClinicoPathJamoviModule
    - survival
    - ncvreg
```

These map to entries in `jamovi/00refs.yaml` for citation generation.

### C. Suitability Assessment: The 6 Checks

| # | Check | Green | Yellow | Red |
|---|-------|-------|--------|-----|
| 1 | Events-Per-Variable (EPV) | EPV >= 10 | 1 <= EPV < 10 | EPV < 1 (shown as yellow with "ultra-low" label) |
| 2 | Regularization Need | p >= n/3 | p < n/3 | -- (no red level) |
| 3 | Sample Size | n >= 100 | 30 <= n < 100 | n < 30 |
| 4 | Event Rate | 20%-80% | Outside 20%-80% | -- (no red level) |
| 5 | Multicollinearity | max |r| < 0.7 | 0.7 <= max |r| < 0.9 | max |r| >= 0.9 |
| 6 | Data Quality | No missing | 0% < missing <= 20% | missing > 20% |

Overall verdict: any red = "Some issues require attention"; any yellow = "Data is usable but review the flagged items"; all green = "Data is well-suited for SCAD/MCP Cox regression."

### D. Known Limitations

1. **No standard errors or p-values**: Penalized coefficients do not have valid frequentist standard errors. The selected_variables table reports only coefficients and hazard ratios. Post-selection inference methods (e.g., selective inference) are not implemented.

2. **Factor variable dummy expansion**: When categorical variables are included, `model.matrix()` expands them to dummy variables. The coefficient names show dummy names (e.g., `gradeHigh`, `gradeModerate`) rather than the original variable name. This is correct behavior but may confuse users expecting the original variable name.

3. **CV error metric**: `cv.ncvsurv` uses the partial likelihood deviance as the CV error metric. This is not directly interpretable as a probability or classification metric.

4. **AIC approximation**: AIC is from an unpenalized Cox refit, not from the penalized likelihood. It is useful for relative comparison between lambda.min and lambda.1se models but is not a true penalized AIC.

5. **Protobuf serialization of Notice objects**: The `.insertNotice()` helper is wrapped in `tryCatch` because `jmvcore::Notice` objects contain function references that can fail during jamovi's protobuf serialization. If serialization fails, the notice is silently skipped. The model interpretation HTML serves as a backup channel for warnings.

### E. References

- Fan, J. & Li, R. (2001). Variable Selection via Nonconcave Penalized Likelihood and its Oracle Properties. *JASA*, 96(456), 1348-1360.
- Zhang, C.-H. (2010). Nearly Unbiased Variable Selection Under Minimax Concave Penalty. *Annals of Statistics*, 38(2), 894-942.
- Breheny, P. & Huang, J. (2011). Coordinate Descent Algorithms for Nonconvex Penalized Regression. *Annals of Applied Statistics*, 5(1), 232-253.
- ncvreg CRAN: https://cran.r-project.org/package=ncvreg
