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


## R Function Details

### .init()

This function initializes the analysis, setting up the necessary objects and preparing the environment for the diagnostic checks.

### .run()

This is the main function that orchestrates the entire diagnostic process. It calls other functions in a logical sequence to fit the model, generate residuals, perform tests, and create plots.

### .process_data()

This function handles data preprocessing tasks, such as excluding missing values and ensuring that variables are in the correct format for the Cox model.

### .fit_cox_model()

This function fits the Cox proportional hazards model using the `coxph` function from the `survival` package. It serves as the foundation for all subsequent diagnostic tests.

### .generate_ph_test()

This function performs the proportional hazards assumption test using the `cox.zph` function. It extracts the test statistics and p-values for each covariate and for the global model.

### .generate_model_summary()

This function generates a summary of the fitted Cox model, including coefficients, hazard ratios, standard errors, and p-values.

### .generate_vif_analysis()

This function calculates the Variance Inflation Factor (VIF) for each covariate in the model to assess multicollinearity. It uses the `vif` function from the `car` package.

### .generate_interpretation()

This function provides a textual interpretation of the diagnostic results, offering guidance on potential issues and suggesting possible remedies.

### Plotting Functions

The following functions are responsible for generating the various diagnostic plots:

*   **`.plot_martingale()`**: Plots the Martingale residuals, which are useful for assessing the functional form of a covariate.
*   **`.plot_deviance()`**: Plots the Deviance residuals, which are a symmetrized version of the Martingale residuals and are useful for identifying outliers.
*   **`.plot_score()`**: Plots the Score residuals, which are useful for assessing the leverage of individual observations.
*   **`.plot_schoenfeld()`**: Plots the Schoenfeld residuals against time, which is the primary method for visually assessing the proportional hazards assumption.
*   **`.plot_dfbeta()`**: Plots the DFBeta values, which measure the influence of individual observations on the regression coefficients.

## Statistical Concepts

### Proportional Hazards Assumption

The fundamental assumption of the Cox proportional hazards model is that the hazard ratio between any two individuals is constant over time. In other words, the effect of a covariate on the hazard of an event does not change over time. The `cox.zph` function tests this assumption by creating time-dependent covariates and testing for their significance.

### Residuals in Survival Analysis

Residuals are used to diagnose problems with the model fit. In survival analysis, there are several types of residuals, each with a specific purpose:

*   **Martingale Residuals:** These residuals can be interpreted as the difference between the observed number of events for an individual and the expected number of events based on the model. They are useful for assessing the functional form of a covariate. A non-linear pattern in a plot of Martingale residuals against a covariate may indicate that the relationship is not linear.
*   **Deviance Residuals:** These are a transformation of the Martingale residuals that are more symmetrically distributed around zero. They are useful for identifying outliers.
*   **Score Residuals:** These residuals are used to assess the leverage or influence of individual observations on the overall model. Large score residuals indicate influential observations.
*   **Schoenfeld Residuals:** These residuals are specifically designed to test the proportional hazards assumption. A plot of Schoenfeld residuals against time should show a random scatter around a horizontal line if the assumption is met. A non-zero slope suggests a violation of the assumption.
*   **DFBeta Residuals:** These residuals measure the change in the regression coefficients when an individual observation is removed from the dataset. They are useful for identifying influential observations that have a large impact on the model parameters.

### Multicollinearity

Multicollinearity occurs when two or more covariates in a regression model are highly correlated. This can make it difficult to determine the individual effect of each covariate on the outcome. The Variance Inflation Factor (VIF) is a measure of how much the variance of a regression coefficient is inflated due to multicollinearity. A VIF value greater than 5 or 10 is often considered to be a sign of significant multicollinearity.