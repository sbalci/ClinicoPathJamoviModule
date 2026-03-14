# SCAD/MCP Cox Regression Documentation

This document provides a comprehensive overview of the SCAD/MCP Cox Regression module (`ncvregcox`), detailing its features, user interface elements, and the underlying R functions.

## Feature Summary

The `ncvregcox` function performs penalized Cox proportional hazards regression using non-convex penalties (SCAD and MCP) for automatic variable selection in high-dimensional survival data. It wraps the `ncvreg` package's `cv.ncvsurv()` function within the jamovi 4-file architecture.

The module targets clinicopathological and genomic studies where identifying a parsimonious set of prognostic variables is critical. SCAD and MCP penalties provide oracle properties -- consistent variable selection with nearly unbiased coefficient estimates for large effects -- addressing the shrinkage bias inherent in LASSO. The function includes cross-validated lambda tuning, model comparison (lambda.min vs lambda.1se), hazard ratio tables, variable importance analysis, data suitability assessment, and three diagnostic plots.

Key capabilities are organized into: **Input Variables** (survival time, event, covariates), **Model Configuration** (penalty type, CV folds, lambda selection), **Penalty Parameters** (alpha, gamma, standardization), **Diagnostics** (suitability assessment), and **Output Controls** (plots, variable importance).

## Feature Details

The following table provides a detailed mapping of the module's features, from the user interface to the underlying R functions.

| Feature | YAML Argument (`.a.yaml`) | UI Label | Results Section (`.r.yaml`) | R Function (`.b.R`) |
|---------|---------------------------|----------|----------------------------|---------------------|
| **Input Variables** | | | | |
| Survival time | `time` | Time Variable | — | `.prepare_data()` |
| Event indicator | `event` | Event Variable | — | `.prepare_data()` |
| Event level | `outcomeLevel` | Event Level | — | `.prepare_data()` |
| Censored level | `censorLevel` | Censored Level | — | `.prepare_data()` |
| Predictor variables | `covariates` | Covariates | — | `.prepare_data()`, `.fit_ncvreg_cox()` |
| **Model Configuration** | | | | |
| Penalty function | `penalty` | Penalty Function | `model_summary` | `.fit_ncvreg_cox()` |
| Cross-validation folds | `cv_folds` | Cross-Validation Folds | `cross_validation_results` | `.fit_ncvreg_cox()` |
| Lambda selection rule | `lambda_type` | Lambda Selection | `model_summary`, `model_comparison` | `.fit_ncvreg_cox()` |
| **Penalty Parameters** | | | | |
| Elastic net mixing | `alpha` | Elastic Net Mixing Parameter | `model_summary` | `.fit_ncvreg_cox()` |
| SCAD/MCP tuning | `gamma` | SCAD/MCP Tuning Parameter | `model_summary` | `.fit_ncvreg_cox()` |
| Standardize covariates | `standardize` | Standardize Variables | `variable_importance` (footnote) | `.fit_ncvreg_cox()` |
| **Diagnostics** | | | | |
| Data suitability check | `suitabilityCheck` | Data Suitability Assessment | `suitabilityReport` | `.assessSuitability()` |
| **Core Results** | | | | |
| Model summary | — | — | `model_summary` | `.populate_model_summary()` |
| Selected variables | — | — | `selected_variables` | `.populate_selected_variables()` |
| Cross-validation summary | — | — | `cross_validation_results` | `.populate_cross_validation_results()` |
| Model comparison | — | — | `model_comparison` | `.populate_model_comparison()` |
| Convergence info | — | — | `convergence_info` | `.populate_convergence_info()` |
| Clinical interpretation | — | — | `model_interpretation` | `.create_model_interpretation()` |
| Report sentence | — | — | `model_interpretation` | `.generate_report_sentence()` |
| **Plots** | | | | |
| Regularization path | `plot_path` | Show Regularization Path | `regularization_path` | `.plot_regularization_path()` |
| CV error plot | `plot_cv` | Show Cross-Validation Plot | `cv_error_plot` | `.plot_cv_error()` |
| Variable importance plot | `variable_importance` | Variable Importance Analysis | `variable_selection_plot` | `.plot_variable_selection()` |
| **Output Controls** | | | | |
| Variable importance table | `variable_importance` | Variable Importance Analysis | `variable_importance` | `.populate_variable_importance()` |
| Instructions panel | — | — | `instructions` | `.update_instructions()` |
| **Notices (dynamic)** | | | | |
| Analysis error | — | — | (inserted) | `.insertNotice()` ERROR |
| Low events warning | — | — | (inserted) | `.add_post_analysis_notices()` |
| No selection warning | — | — | (inserted) | `.add_post_analysis_notices()` |
| Poor discrimination | — | — | (inserted) | `.add_post_analysis_notices()` |
| Lambda 1-SE fallback | — | — | (inserted) | `.add_post_analysis_notices()` |
| Missing data info | — | — | (inserted) | `.add_post_analysis_notices()` |
| Convergence warnings | — | — | (inserted) | `.run()` |
| Analysis complete | — | — | (inserted) | `.run()` |

## Changelog

- **2026-03-12**: Updated for current implementation
  - Removed Lasso penalty (only SCAD/MCP supported)
  - Added `outcomeLevel`, `censorLevel`, `suitabilityCheck` options
  - Added `ncvregcox_small` and `ncvregcox_collinear` test datasets
  - Added report sentence generation and per-variable HR interpretations
  - Added lambda 1-SE fallback tracking
  - C-index now uses `survival::concordance(reverse=TRUE)`
  - AIC from unpenalized Cox refit with footnote
  - MCP gamma auto-adjusts from 3.7 to 3.0
  - Protobuf-safe plot state management
