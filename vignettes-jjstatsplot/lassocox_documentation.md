# Lasso-Cox Regression for Variable Selection in Survival Analysis Documentation

This document provides a comprehensive overview of the Lasso-Cox Regression for Variable Selection in Survival Analysis module, detailing its features, user interface elements, and the underlying R functions.

## Feature Summary

This module performs Lasso-penalized Cox proportional hazards regression for variable selection in survival analysis. It uses penalized likelihood to identify the most important predictors while preventing overfitting, making it ideal for high-dimensional survival data where the number of potential predictors may approach or exceed the sample size. The module supports automatic variable selection, cross-validation for optimal tuning parameter selection, risk score calculation and stratification, comprehensive model performance evaluation, and survival curve visualization by risk groups.

## Feature Details

The following table provides a detailed mapping of the module's features, from the user interface to the underlying R functions.

| Feature                          | YAML Argument (`.a.yaml`)      | UI Label                               | Results Section (`.r.yaml`)         | R Function (`.b.R`)                  |
| -------------------------------- | ------------------------------ | -------------------------------------- | ----------------------------------- | ------------------------------------ |
| **Core Analysis Setup**          |                                |                                        |                                     |                                      |
| Time Elapsed                     | `elapsedtime`                  | Time Elapsed                           | `modelSummary`, `coefficients`, `performance`, `cv_plot`, `coef_plot`, `survival_plot`, `riskScore` | `.run`, `.cleanData`, `.fitModel`    |
| Outcome                          | `outcome`                      | Outcome                                | `modelSummary`, `coefficients`, `performance`, `cv_plot`, `coef_plot`, `survival_plot`, `riskScore` | `.run`, `.cleanData`, `.fitModel`    |
| Event Level                      | `outcomeLevel`                 | Event Level                            | `modelSummary`, `coefficients`, `performance`, `cv_plot`, `coef_plot`, `survival_plot`, `riskScore` | `.run`, `.cleanData`, `.fitModel`    |
| Explanatory Variables            | `explanatory`                  | Explanatory Variables                  | `modelSummary`, `coefficients`, `performance`, `cv_plot`, `coef_plot`, `survival_plot`, `riskScore` | `.run`, `.cleanData`, `.fitModel`    |
| Lambda Selection Method          | `lambda`                       | Lambda Selection Method                | `modelSummary`, `coefficients`, `performance`, `cv_plot`, `coef_plot`, `survival_plot`, `riskScore` | `.run`, `.fitModel`                  |
| Number of CV Folds               | `nfolds`                       | Number of CV Folds                     | `modelSummary`, `coefficients`, `performance`, `cv_plot`, `coef_plot`, `survival_plot`, `riskScore` | `.run`, `.fitModel`                  |
| Standardize Variables            | `standardize`                  | Standardize Variables                  | `modelSummary`, `coefficients`, `performance`, `cv_plot`, `coef_plot`, `survival_plot`, `riskScore` | `.run`, `.cleanData`, `.fitModel`    |
| **Output & Visualization**       |                                |                                        |                                     |                                      |
| Cross-validation Plot            | `cv_plot`                      | Cross-validation Plot                  | `cv_plot`                           | `.cvPlot`                            |
| Coefficient Plot                 | `coef_plot`                    | Coefficient Plot                       | `coef_plot`                         | `.coefPlot`                          |
| Risk Group Survival Plot         | `survival_plot`                | Risk Group Survival Plot               | `survival_plot`                     | `.survivalPlot`                      |
| Add Risk Score to Data           | `riskScore`                    | Add Risk Score to Data                 | `riskScore`                         | `.savePlotData`                      |
