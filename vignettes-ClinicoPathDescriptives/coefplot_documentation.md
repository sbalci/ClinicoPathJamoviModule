# Coefficient Plots Analysis Documentation

This document provides a comprehensive overview of the Coefficient Plots module, detailing its features, user interface elements, and the underlying R functions.

## Feature Summary

The Coefficient Plots module is a powerful tool for visualizing regression coefficients and their confidence intervals, often referred to as forest plots. It supports various regression models (linear, logistic, Cox, Poisson) and offers extensive customization options for plot appearance, coefficient selection, and statistical adjustments like standardization and robust standard errors. This module is essential for clearly presenting regression results in research and epidemiological studies.

The module's features can be broadly categorized as follows:

*   **Model Specification:** Define dependent and independent variables, and select the appropriate regression model type.
*   **Coefficient Selection & Customization:** Control which coefficients are displayed, their sorting order, and visual attributes like point size and line thickness.
*   **Statistical Options:** Apply standardization, robust standard errors, and exponentiate coefficients for odds/hazard/rate ratios.
*   **Output Control:** Choose to display the coefficient plot, a model summary, and/or a detailed coefficient table.
*   **Custom Titles & Labels:** Personalize plot titles and axis labels.

## Feature Details

The following table provides a detailed mapping of the module's features, from the user interface to the underlying R functions.

| Feature                          | YAML Argument (`.a.yaml`)      | UI Label                               | Results Section (`.r.yaml`)         | R Function (`.b.R`)                  |
| -------------------------------- | ------------------------------ | -------------------------------------- | ----------------------------------- | ------------------------------------ |
| **Model Specification**          |                                |                                        |                                     |                                      |
| Dependent Variable               | `dep`                          | Dependent Variable                     | `coefficient_plot`, `model_summary`, `coefficient_table` | `.run`, `.fitModel`                  |
| Covariates                       | `covs`                         | Covariates                             | `coefficient_plot`, `model_summary`, `coefficient_table` | `.run`, `.fitModel`                  |
| Model Type                       | `model_type`                   | Model Type                             | `coefficient_plot`, `model_summary`, `coefficient_table` | `.run`, `.fitModel`, `.generateCoefficientPlot`, `.generateModelSummary` |
| Time Variable (Cox only)         | `time_var`                     | Time Variable (Cox only)               | `coefficient_plot`, `model_summary`, `coefficient_table` | `.fitModel`                          |
| **Coefficient Selection**        |                                |                                        |                                     |                                      |
| Include Intercept                | `include_intercept`            | Include Intercept                      | `coefficient_plot`, `coefficient_table` | `.init`, `.generateCoefficientPlot`, `.generateCoefficientTable` |
| Coefficient Selection            | `coef_selection`               | Coefficient Selection                  | `coefficient_plot`                  | `.generateCoefficientPlot`           |
| Specific Coefficients            | `specific_coefs`               | Specific Coefficients                  | `coefficient_plot`                  | `.generateCoefficientPlot`           |
| **Plot Customization**           |                                |                                        |                                     |                                      |
| Confidence Level                 | `ci_level`                     | Confidence Level                       | `coefficient_plot`, `coefficient_table` | `.generateCoefficientPlot`, `.generateCoefficientTable` |
| Inner Confidence Level           | `inner_ci_level`               | Inner Confidence Level                 | `coefficient_plot`                  | `.generateCoefficientPlot`           |
| Sort Coefficients                | `sort_coefs`                   | Sort Coefficients                      | `coefficient_plot`                  | `.generateCoefficientPlot`           |
| Decreasing Sort Order            | `decreasing_sort`              | Decreasing Sort Order                  | `coefficient_plot`                  | `.generateCoefficientPlot`           |
| Horizontal Layout                | `horizontal_plot`              | Horizontal Layout                      | `coefficient_plot`                  | (Plotting library handles)           |
| Point Size                       | `point_size`                   | Point Size                             | `coefficient_plot`                  | `.generateCoefficientPlot`           |
| Line Thickness                   | `line_thickness`               | Line Thickness                         | `coefficient_plot`                  | (Plotting library handles)           |
| Custom Plot Title                | `custom_title`                 | Custom Plot Title                      | `coefficient_plot`                  | `.generateCoefficientPlot`           |
| Custom X-axis Label              | `custom_x_label`               | Custom X-axis Label                    | `coefficient_plot`                  | `.generateCoefficientPlot`           |
| **Statistical Options**          |                                |                                        |                                     |                                      |
| Standardize Coefficients         | `standardize`                  | Standardize Coefficients               | `coefficient_plot`, `model_summary`, `coefficient_table` | (Not directly implemented in .b.R for this feature) |
| Robust Standard Errors           | `robust_se`                    | Robust Standard Errors                 | `coefficient_plot`, `model_summary`, `coefficient_table` | (Not directly implemented in .b.R for this feature) |
| Exponentiate Coefficients        | `exp_transform`                | Exponentiate Coefficients              | `coefficient_plot`, `coefficient_table` | `.generateCoefficientPlot`, `.generateCoefficientTable` |
| **Model Comparison**             |                                |                                        |                                     |                                      |
| Compare Multiple Models          | `compare_models`               | Compare Multiple Models                | (N/A)                               | (Not directly implemented in .b.R for this feature) |
| Model 2 Covariates               | `model2_covs`                  | Model 2 Covariates                     | (N/A)                               | (Not directly implemented in .b.R for this feature) |
| Model 3 Covariates               | `model3_covs`                  | Model 3 Covariates                     | (N/A)                               | (Not directly implemented in .b.R for this feature) |
| Model Names                      | `model_names`                  | Model Names                            | (N/A)                               | (Not directly implemented in .b.R for this feature) |
| **Output Options**               |                                |                                        |                                     |                                      |
| Show Coefficient Plot            | `show_coefficient_plot`        | Show Coefficient Plot                  | `coefficient_plot`                  | `.init`, `.run`                      |
| Show Model Summary               | `show_model_summary`           | Show Model Summary                     | `model_summary`                     | `.init`, `.run`, `.generateModelSummary` |
| Show Coefficient Table           | `show_coefficient_table`       | Show Coefficient Table                 | `coefficient_table`                 | `.init`, `.run`, `.generateCoefficientTable` |
