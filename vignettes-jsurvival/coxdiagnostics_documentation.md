# Cox Proportional Hazards Model Diagnostics Documentation

This document provides a comprehensive overview of the Cox Proportional Hazards Model Diagnostics module, detailing its features, user interface elements, and the underlying R functions.

## Feature Summary

The Cox Proportional Hazards Model Diagnostics module is a powerful tool for validating Cox regression models in survival analysis. It provides various diagnostic plots and statistical tests to assess model assumptions, identify influential observations, and detect multicollinearity.

The module's features can be broadly categorized as follows:

*   **Model Fitting:** Fits a Cox proportional hazards model based on user-defined time, event, and covariate variables.
*   **Residual Analysis:** Generates Martingale, Deviance, Score, Schoenfeld, and DFBeta residual plots to assess model fit and assumptions.
*   **Proportional Hazards Assumption Testing:** Performs statistical tests (e.g., using Schoenfeld residuals) to check the proportional hazards assumption.
*   **Multicollinearity Assessment:** Calculates Variance Inflation Factor (VIF) to detect multicollinearity among covariates.
*   **Model Summary:** Provides a summary of the fitted Cox model, including coefficients, hazard ratios, and statistical tests.
*   **Interpretation Guidance:** Offers guidance on interpreting diagnostic results and recommendations for addressing potential model issues.

## Feature Details

The following table provides a detailed mapping of the module's features, from the user interface to the underlying R functions.

| Feature                          | YAML Argument (`.a.yaml`)      | UI Label                               | Results Section (`.r.yaml`)         | R Function (`.b.R`)                  |
| -------------------------------- | ------------------------------ | -------------------------------------- | ----------------------------------- | ------------------------------------ |
| **Core Model Setup**             |                                |                                        |                                     |                                      |
| Time Variable                    | `time`                         | Time Variable                          | (Model input)                       | `.fit_cox_model`                     |
| Event Variable                   | `event`                        | Event Variable                         | (Model input)                       | `.fit_cox_model`                     |
| Covariates                       | `covariates`                   | Covariates                             | (Model input)                       | `.fit_cox_model`                     |
| Strata Variable                  | `strata_var`                   | Strata Variable                        | (Model input)                       | `.fit_cox_model`                     |
| Exclude Missing Values           | `exclude_missing`              | Exclude Missing Values                 | (Data preprocessing)                | `.process_data`                      |
| Confidence Level                 | `confidence_level`             | Confidence Level                       | (Statistical tests)                 | `.generate_ph_test`                  |
| **Diagnostic Plots**             |                                |                                        |                                     |                                      |
| Show Martingale Residuals        | `show_martingale`              | Show Martingale Residuals              | `martingale_plot`                   | `.plot_martingale`                   |
| Show Deviance Residuals          | `show_deviance`                | Show Deviance Residuals                | `deviance_plot`                     | `.plot_deviance`                     |
| Show Score Residuals             | `show_score`                   | Show Score Residuals                   | `score_plot`                        | `.plot_score`                        |
| Show Schoenfeld Residuals        | `show_schoenfeld`              | Show Schoenfeld Residuals              | `schoenfeld_plot`                   | `.plot_schoenfeld`                   |
| Show DFBeta Influence            | `show_dfbeta`                  | Show DFBeta Influence Diagnostics      | `dfbeta_plot`                       | `.plot_dfbeta`                       |
| X-axis Scale                     | `ox_scale`                     | X-axis Scale for Plots                 | (Plot customization)                | (various `.plot_` functions)         |
| Add Smooth Line                  | `add_smooth`                   | Add Smooth Line                        | (Plot customization)                | (various `.plot_` functions)         |
| Add Reference Line               | `add_reference`                | Add Reference Line                     | (Plot customization)                | (various `.plot_` functions)         |
| Point Size                       | `point_size`                   | Point Size                             | (Plot customization)                | (various `.plot_` functions)         |
| Alpha Level                      | `alpha_level`                  | Transparency Level                     | (Plot customization)                | (various `.plot_` functions)         |
| **Statistical Outputs**          |                                |                                        |                                     |                                      |
| Show PH Test                     | `show_ph_test`                 | Show Proportional Hazards Test         | `ph_test_results`                   | `.generate_ph_test`                  |
| Show Model Summary               | `show_model_summary`           | Show Cox Model Summary                 | `model_summary`                     | `.generate_model_summary`            |
| Show VIF Analysis                | `show_vif`                     | Show Multicollinearity Analysis (VIF)  | `vif_results`                       | `.generate_vif_analysis`             |
| VIF Threshold                    | `vif_threshold`                | VIF Threshold                          | (VIF analysis)                      | `.generate_vif_analysis`             |
| Show Interpretation              | `show_interpretation`          | Show Diagnostic Interpretation Guide   | `interpretation`                    | `.generate_interpretation`           |
