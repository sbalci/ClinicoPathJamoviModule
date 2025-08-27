# Comprehensive Survival Analysis Documentation

This document provides a comprehensive overview of the Comprehensive Survival Analysis module, detailing its features, user interface elements, and the underlying R functions.

## Feature Summary

The Comprehensive Survival Analysis module performs univariate and stratified survival analysis, comparing survival between groups. It calculates person-time follow-up, derives accurate survival estimates and incidence rates, and supports Cox proportional hazards modeling. The module offers extensive visualization options, including Kaplan-Meier curves, cumulative events/hazard plots, KMunicate-style plots, and log-log plots for proportional hazards assessment. Advanced features include RMST analysis, competing risks, landmark analysis, and residual diagnostics.

## Feature Details

The following table provides a detailed mapping of the module's features, from the user interface to the underlying R functions.

| Feature                          | YAML Argument (`.a.yaml`)      | UI Label                               | Results Section (`.r.yaml`)         | R Function (`.b.R`)                  |
| -------------------------------- | ------------------------------ | -------------------------------------- | ----------------------------------- | ------------------------------------ |
| **Core Survival Analysis**       |                                |                                        |                                     |                                      |
| Time Elapsed                     | `elapsedtime`                  | Time Elapsed                           | `medianSummary`, `medianTable`, `coxSummary`, `coxTable`, `tCoxtext2`, `cox_ph`, `plot8`, `survTableSummary`, `survTable`, `pairwiseSummary`, `pairwiseTable`, `plot`, `plot2`, `plot3`, `plot6`, `plot7`, `residualsPlot`, `calculatedtime`, `outcomeredefined`, `personTimeTable`, `personTimeSummary`, `rmstTable`, `rmstSummary`, `residualsTable`, `survivalExport`, `survivalExportSummary` | `.run`, `.definemytime`, `.cleandata`, `.medianSurv`, `.cox`, `.survTable`, `.pairwise`, `.personTimeAnalysis`, `.calculateRMST`, `.calculateResiduals`, `.exportSurvivalData`, `.plot`, `.plot2`, `.plot3`, `.plot6`, `.plot7`, `.plot8`, `.plot9` |
| Using Dates to Calculate Survival Time| `tint`                         | Using Dates to Calculate Survival Time | `calculatedtime`                    | `.definemytime`, `.cleandata`        |
| Diagnosis Date                   | `dxdate`                       | Diagnosis Date                         | `calculatedtime`                    | `.definemytime`, `.cleandata`        |
| Follow-up Date                   | `fudate`                       | Follow-up Date                         | `calculatedtime`                    | `.definemytime`, `.cleandata`        |
| Add Calculated Time to Data      | `calculatedtime`               | Add Calculated Time to Data            | `calculatedtime`                    | `.run`                               |
| Explanatory Variable             | `explanatory`                  | Explanatory Variable                   | `medianSummary`, `medianTable`, `coxSummary`, `coxTable`, `tCoxtext2`, `cox_ph`, `plot8`, `survTableSummary`, `survTable`, `pairwiseSummary`, `pairwiseTable`, `plot`, `plot2`, `plot3`, `plot6`, `plot7`, `residualsPlot` | `.run`, `.definemyfactor`, `.cleandata`, `.medianSurv`, `.cox`, `.survTable`, `.pairwise`, `.plot`, `.plot2`, `.plot3`, `.plot6`, `.plot7`, `.plot8`, `.plot9` |
| Outcome                          | `outcome`                      | Outcome                                | `medianSummary`, `medianTable`, `coxSummary`, `coxTable`, `tCoxtext2`, `cox_ph`, `plot8`, `survTableSummary`, `survTable`, `pairwiseSummary`, `pairwiseTable`, `plot`, `plot2`, `plot3`, `plot6`, `plot7`, `residualsPlot`, `outcomeredefined` | `.run`, `.definemyoutcome`, `.cleandata`, `.medianSurv`, `.cox`, `.survTable`, `.pairwise`, `.plot`, `.plot2`, `.plot3`, `.plot6`, `.plot7`, `.plot8`, `.plot9` |
| Event Level                      | `outcomeLevel`                 | Event Level                            | `medianSummary`, `medianTable`, `coxSummary`, `coxTable`, `tCoxtext2`, `cox_ph`, `plot8`, `survTableSummary`, `survTable`, `pairwiseSummary`, `pairwiseTable`, `plot`, `plot2`, `plot3`, `plot6`, `plot7`, `residualsPlot` | `.run`, `.definemyoutcome`, `.cleandata`, `.medianSurv`, `.cox`, `.survTable`, `.pairwise`, `.plot`, `.plot2`, `.plot3`, `.plot6`, `.plot7`, `.plot8`, `.plot9` |
| Dead of Disease                  | `dod`                          | Dead of Disease                        | `outcomeredefined`                  | `.definemyoutcome`                   |
| Dead of Other                    | `dooc`                         | Dead of Other                          | `outcomeredefined`                  | `.definemyoutcome`                   |
| Alive w Disease                  | `awd`                          | Alive w Disease                        | `outcomeredefined`                  | `.definemyoutcome`                   |
| Alive w/o Disease                | `awod`                         | Alive w/o Disease                      | `outcomeredefined`                  | `.definemyoutcome`                   |
| Survival Type                    | `analysistype`                 | Survival Type                          | `outcomeredefined`                  | `.definemyoutcome`                   |
| Add Redefined Outcome to Data    | `outcomeredefined`             | Add Redefined Outcome to Data          | `outcomeredefined`                  | `.run`                               |
| Cutpoints                        | `cutp`                         | Cutpoints                              | `survTableSummary`, `survTable`     | `.survTable`                         |
| Time Type in Data                | `timetypedata`                 | Time Type in Data                      | `calculatedtime`                    | `.definemytime`                      |
| Time Type in Output              | `timetypeoutput`               | Time Type in Output                    | `medianSummary`, `plot`, `plot2`, `plot3`, `plot6`, `plot7`, `personTimeSummary` | `.definemytime`, `.medianSurv`, `.personTimeAnalysis`, `.plot`, `.plot2`, `.plot3`, `.plot6`, `.plot7` |
| Use Landmark Time                | `uselandmark`                  | Use Landmark Time                      | `coxSummary`, `tCoxtext2`           | `.cleandata`, `.cox`                 |
| Landmark Time                    | `landmark`                     | Landmark Time                          | `coxSummary`, `tCoxtext2`           | `.cleandata`, `.cox`                 |
| Pairwise comparisons             | `pw`                           | Pairwise comparisons                   | `pairwiseSummary`, `pairwiseTable`  | `.pairwise`                          |
| Adjustment Method                | `padjustmethod`                | Adjustment Method                      | `pairwiseTable`                     | `.pairwise`                          |
| Proportional Hazards Assumption  | `ph_cox`                       | Proportional Hazards Assumption        | `cox_ph`, `plot8`                   | `.cox`, `.plot8`                     |
| Survival Plot                    | `sc`                           | Survival Plot                          | `plot`                              | `.plot`                              |
| KMunicate-Style Plot             | `kmunicate`                    | KMunicate-Style Plot                   | `plot6`                             | `.plot6`                             |
| Cumulative Events                | `ce`                           | Cumulative Events                      | `plot2`                             | `.plot2`                             |
| Cumulative Hazard                | `ch`                           | Cumulative Hazard                      | `plot3`                             | `.plot3`                             |
| Plot End Time                    | `endplot`                      | Plot End Time                          | `plot`, `plot2`, `plot3`, `plot6`, `plot7` | `.plot`, `.plot2`, `.plot3`, `.plot6`, `.plot7` |
| Start y-axis                     | `ybegin_plot`                  | Start y-axis                           | `plot`, `plot2`, `plot3`            | `.plot`, `.plot2`, `.plot3`          |
| End y-axis                       | `yend_plot`                    | End y-axis                             | `plot`, `plot2`, `plot3`            | `.plot`, `.plot2`, `.plot3`          |
| Time Interval                    | `byplot`                       | Time Interval                          | `plot`, `plot2`, `plot3`, `plot6`, `plot7` | `.plot`, `.plot2`, `.plot3`, `.plot6`, `.plot7` |
| Multiple Event Levels            | `multievent`                   | Multiple Event Levels                  | `outcomeredefined`                  | `.definemyoutcome`                   |
| 95% CI                           | `ci95`                         | 95% CI                                 | `plot`, `plot2`, `plot3`            | `.plot`, `.plot2`, `.plot3`          |
| risktable                        | `risktable`                    | risktable                              | `plot`, `plot2`, `plot3`            | `.plot`, `.plot2`, `.plot3`          |
| censored                         | `censored`                     | censored                               | `plot`, `plot2`, `plot3`            | `.plot`, `.plot2`, `.plot3`          |
| p-value                          | `pplot`                        | p-value                                | `plot`, `plot2`, `plot3`            | `.plot`, `.plot2`, `.plot3`          |
| medianline                       | `medianline`                   | medianline                             | `plot`, `plot2`, `plot3`            | `.plot`, `.plot2`, `.plot3`          |
| Calculate Person-Time Metrics    | `person_time`                  | Calculate Person-Time Metrics          | `personTimeTable`, `personTimeSummary` | `.personTimeAnalysis`                |
| Time Interval Stratification     | `time_intervals`               | Time Interval Stratification           | `personTimeTable`, `personTimeSummary` | `.personTimeAnalysis`                |
| Rate Multiplier                  | `rate_multiplier`              | Rate Multiplier                        | `personTimeTable`, `personTimeSummary` | `.personTimeAnalysis`                |
| Restricted Mean Survival Time (RMST)| `rmst_analysis`                | Restricted Mean Survival Time (RMST)   | `rmstTable`, `rmstSummary`          | `.calculateRMST`                     |
| RMST Time Horizon                | `rmst_tau`                     | RMST Time Horizon                      | `rmstTable`, `rmstSummary`          | `.calculateRMST`                     |
| Stratified Cox Regression        | `stratified_cox`               | Stratified Cox Regression              | `coxTable`                          | `.cox`                               |
| Stratification Variable          | `strata_variable`              | Stratification Variable                | `coxTable`                          | `.cox`                               |
| Model Residual Diagnostics       | `residual_diagnostics`         | Model Residual Diagnostics             | `residualsTable`, `residualsPlot`   | `.calculateResiduals`, `.plot9`      |
| Export Survival Estimates        | `export_survival_data`         | Export Survival Estimates              | `survivalExport`, `survivalExportSummary` | `.exportSurvivalData`                |
| Log-Log Plot                     | `loglog`                       | Log-Log Plot                           | `plot7`                             | `.plot7`                             |


## R Function Details

This section provides a more detailed explanation of the R functions used in the Comprehensive Survival Analysis module.

### Core Functions

#### `.run()`

*   **Purpose:** This is the main entry point for the analysis. It acts as the orchestrator, calling other functions in the correct sequence to perform the analysis.
*   **Workflow:**
    1.  Initializes the results objects.
    2.  Calls `.cleandata()` to prepare the data.
    3.  Calls the various analysis functions (e.g., `.medianSurv()`, `.cox()`, `.survTable()`).
    4.  Calls the plotting functions (e.g., `.plot()`, `.plot2()`) to generate visualizations.
    5.  Populates the results to be displayed in the jamovi interface.

#### `.cleandata()`

*   **Purpose:** This function is responsible for all data preprocessing and cleaning.
*   **Key Operations:**
    *   Handles missing values based on user settings.
    *   Calls `.definemytime()` to create the survival time variable.
    *   Calls `.definemyoutcome()` to create the event variable.
    *   Converts variables to their correct types (e.g., factors, numeric).
    *   Applies landmark time adjustments if specified.

#### `.definemytime()`

*   **Purpose:** To create the numerical time variable required for survival analysis.
*   **Functionality:**
    *   If the user provides date variables (`dxdate`, `fudate`), it calculates the time difference.
    *   It handles different time units (days, months, years) and converts them as specified by the user (`timetypedata`, `timetypeoutput`).
    *   If the user provides a pre-calculated time variable (`elapsedtime`), it uses that directly.

#### `.definemyoutcome()`

*   **Purpose:** To create the event indicator variable for the survival analysis.
*   **Functionality:**
    *   It can handle simple binary outcomes (e.g., "Alive", "Dead").
    *   It supports more complex scenarios with multiple event types (e.g., "Dead of Disease", "Dead of Other Cause"), which is essential for competing risks analysis.
    *   It allows the user to define which levels of the outcome variable constitute an event.

### Analysis Functions

#### `.medianSurv()`

*   **Purpose:** To calculate and display the median survival time.
*   **Output:** Provides the median survival time for each group, along with 95% confidence intervals.

#### `.cox()`

*   **Purpose:** To perform Cox proportional hazards regression.
*   **Functionality:**
    *   Can perform both univariate and stratified Cox regression.
    *   Calculates hazard ratios, confidence intervals, and p-values.
    *   If requested, it performs a test of the proportional hazards assumption using `cox.zph()`.

#### `.survTable()`

*   **Purpose:** To generate a table of survival probabilities at specific time points.
*   **Customization:** The user can specify the time points for which to calculate the survival probabilities.

#### `.pairwise()`

*   **Purpose:** To perform pairwise comparisons of survival curves when there are more than two groups.
*   **Method:** It uses the `pairwise_survdiff()` function from the `survminer` package.
*   **Adjustments:** It allows for p-value adjustment for multiple comparisons (e.g., Bonferroni, FDR).

#### `.personTimeAnalysis()`

*   **Purpose:** To calculate person-time and incidence rates.
*   **Use Case:** This is useful for studies where subjects are followed for different lengths of time.

#### `.calculateRMST()`

*   **Purpose:** To calculate the Restricted Mean Survival Time (RMST).
*   **Benefit:** RMST is a useful alternative to the hazard ratio, especially when the proportional hazards assumption is not met.

#### `.calculateResiduals()`

*   **Purpose:** To calculate residuals for the Cox model for diagnostic purposes.
*   **Types of Residuals:** It can calculate various types of residuals, such as Schoenfeld residuals, to check the model assumptions.

### Helper Functions

#### `.exportSurvivalData()`

*   **Purpose:** To allow users to export the survival data, including the calculated time and outcome variables, for use in other software.

### Plotting Functions

The plotting functions use the `ggplot2` and `survminer` packages to create publication-quality visualizations.

*   **`.plot()`:** Generates the main Kaplan-Meier survival plot.
*   **`.plot2()`:** Generates a plot of the cumulative number of events over time.
*   **`.plot3()`:** Generates a plot of the cumulative hazard function.
*   **`.plot6()`:** Creates a "KMunicate-style" plot, which is a more user-friendly way of presenting survival data.
*   **`.plot7()`:** Generates a log-log plot, which is another way to assess the proportional hazards assumption.
*   **`.plot8()`:** Plots the Schoenfeld residuals to test the proportional hazards assumption.
*   **`.plot9()`:** Generates various diagnostic plots based on the calculated residuals.