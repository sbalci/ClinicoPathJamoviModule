# Multivariable Survival Analysis Documentation

This document provides a comprehensive overview of the Multivariable Survival Analysis module, detailing its features, user interface elements, and the underlying R functions.

## Feature Summary

This module performs multivariable survival analysis using Cox proportional hazards regression. It is designed to analyze the effects of multiple explanatory variables on survival outcomes, accounting for varying observation periods through person-time follow-up. The module offers comprehensive features including handling time-dependent covariates, frailty models, splines for non-proportional hazards, risk score calculation, and various visualization options. It also supports advanced data preparation, model selection, and stratification.

## Feature Details

The following table provides a detailed mapping of the module's features, from the user interface to the underlying R functions.

| Feature                          | YAML Argument (`.a.yaml`)      | UI Label                               | Results Section (`.r.yaml`)         | R Function (`.b.R`)                  |
| -------------------------------- | ------------------------------ | -------------------------------------- | ----------------------------------- | ------------------------------------ |
| **Time & Outcome Variables**     |                                |                                        |                                     |                                      |
| Time Elapsed                     | `elapsedtime`                  | Time Elapsed                           | `text`, `text2`, `plot`, `plot3`, `plot8`, `plotKM`, `plot_adj`, `riskGroupPlot`, `calculatedtime`, `outcomeredefined`, `personTimeTable`, `personTimeSummary`, `nomogram_display`, `tree_summary`, `tree_plot`, `node_survival_plots`, `text_model_selection`, `selection_method`, `text2_model_selection` | `.run`, `.definemytime`, `.cleandata` |
| Using Dates to Calculate Survival Time| `tint`                         | Using Dates to Calculate Survival Time | `calculatedtime`                    | `.definemytime`, `.cleandata`        |
| Diagnosis Date                   | `dxdate`                       | Diagnosis Date                         | `calculatedtime`                    | `.definemytime`, `.cleandata`        |
| Follow-up Date                   | `fudate`                       | Follow-up Date                         | `calculatedtime`                    | `.definemytime`, `.cleandata`        |
| Add Calculated Time to Data      | `calculatedtime`               | Add Calculated Time to Data            | `calculatedtime`                    | `.run`                               |
| Outcome                          | `outcome`                      | Outcome                                | `text`, `text2`, `plot`, `plot3`, `plot8`, `plotKM`, `plot_adj`, `riskGroupPlot`, `outcomeredefined`, `personTimeTable`, `personTimeSummary`, `nomogram_display`, `tree_summary`, `tree_plot`, `node_survival_plots`, `text_model_selection`, `selection_method`, `text2_model_selection` | `.run`, `.definemyoutcome`, `.cleandata` |
| Event Level                      | `outcomeLevel`                 | Event Level                            | `text`, `text2`, `plot`, `plot3`, `plot8`, `plotKM`, `plot_adj`, `riskGroupPlot`, `nomogram_display`, `tree_summary`, `tree_plot`, `node_survival_plots`, `text_model_selection`, `selection_method`, `text2_model_selection` | `.run`, `.definemyoutcome`, `.cleandata` |
| Dead of Disease                  | `dod`                          | Dead of Disease                        | `outcomeredefined`                  | `.definemyoutcome`                   |
| Dead of Other Causes             | `dooc`                         | Dead of Other Causes                   | `outcomeredefined`                  | `.definemyoutcome`                   |
| Alive w Disease                  | `awd`                          | Alive w Disease                        | `outcomeredefined`                  | `.definemyoutcome`                   |
| Alive w/o Disease                | `awod`                         | Alive w/o Disease                      | `outcomeredefined`                  | `.definemyoutcome`                   |
| Survival Type                    | `analysistype`                 | Survival Type                          | `outcomeredefined`                  | `.definemyoutcome`                   |
| Add Redefined Outcome to Data    | `outcomeredefined`             | Add Redefined Outcome to Data          | `outcomeredefined`                  | `.run`                               |
| **Explanatory Variables**        |                                |                                        |                                     |                                      |
| Explanatory Variables            | `explanatory`                  | Explanatory Variables                  | `text`, `text2`, `plot`, `plot3`, `plot8`, `plotKM`, `plot_adj`, `riskGroupPlot`, `nomogram_display`, `tree_summary`, `tree_plot`, `node_survival_plots`, `text_model_selection`, `selection_method`, `text2_model_selection` | `.run`, `.definemyfactor`, `.cleandata` |
| Continuous Explanatory Variable  | `contexpl`                     | Continuous Explanatory Variable        | `text`, `text2`, `plot`, `plot3`, `plot8`, `plotKM`, `plot_adj`, `riskGroupPlot`, `nomogram_display`, `tree_summary`, `tree_plot`, `node_survival_plots`, `text_model_selection`, `selection_method`, `text2_model_selection` | `.run`, `.definemyfactor`, `.cleandata` |
| Multiple Event Levels            | `multievent`                   | Multiple Event Levels                  | `outcomeredefined`                  | `.definemyoutcome`                   |
| **Plots & Visualization**        |                                |                                        |                                     |                                      |
| Hazards Regression Plot          | `hr`                           | Hazards Regression Plot                | `plot`, `plot3`                     | `.plot`, `.plot3`                    |
| Plot Style                       | `sty`                          | Plot Style                             | `plot`, `plot3`                     | `.plot`, `.plot3`                    |
| Proportional Hazards Assumption  | `ph_cox`                       | Proportional Hazards Assumption        | `cox_ph`, `plot8`                   | `.cox_ph`, `.plot8`                  |
| Kaplan-Meier                     | `km`                           | Kaplan-Meier                           | `plotKM`                            | `.plotKM`                            |
| Plot End Time                    | `endplot`                      | Plot End Time                          | `plotKM`, `plot_adj`                | `.plotKM`, `.plot_adj`               |
| Time Interval                    | `byplot`                       | Time Interval                          | `plotKM`, `plot_adj`                | `.plotKM`, `.plot_adj`               |
| 95% CI                           | `ci95`                         | 95% CI                                 | `plotKM`, `plot_adj`                | `.plotKM`, `.plot_adj`               |
| risktable                        | `risktable`                    | risktable                              | `plotKM`, `plot_adj`                | `.plotKM`, `.plot_adj`               |
| censored                         | `censored`                     | censored                               | `plotKM`, `plot_adj`                | `.plotKM`, `.plot_adj`               |
| medianline                       | `medianline`                   | medianline                             | `plotKM`, `plot_adj`                | `.plotKM`, `.plot_adj`               |
| p-value                          | `pplot`                        | p-value                                | `plotKM`, `plot_adj`                | `.plotKM`, `.plot_adj`               |
| **Risk Score Analysis**          |                                |                                        |                                     |                                      |
| Calculate Risk Score             | `calculateRiskScore`           | Calculate Risk Score                   | `risk_score_analysis`, `risk_score_analysis2`, `riskScoreTable`, `riskScoreMetrics`, `riskGroupPlot`, `addRiskScore`, `addRiskGroup` | `.calculateRiskScore`, `.plotRiskGroups` |
| Number of Risk Groups            | `numRiskGroups`                | Number of Risk Groups                  | `riskScoreTable`, `riskScoreMetrics`, `riskGroupPlot` | `.calculateRiskScore`, `.plotRiskGroups` |
| Plot Risk Group Survival         | `plotRiskGroups`               | Plot Risk Group Survival               | `riskGroupPlot`                     | `.plotRiskGroups`                    |
| Add Risk Score to Data           | `addRiskScore`                 | Add Risk Score to Data                 | `addRiskScore`                      | `.run`                               |
| Add Risk Group to Data           | `addRiskGroup`                 | Add Risk Group to Data                 | `addRiskGroup`                      | `.run`                               |
| **Adjusted Survival Curves**     |                                |                                        |                                     |                                      |
| Adjusted Survival Curve          | `ac`                           | Adjusted Survival Curve                | `plot_adj`                          | `.calculateAdjustedStats`, `.plot_adj` |
| Variable for Adjusted Curve      | `adjexplanatory`               | Variable for Adjusted Curve            | `plot_adj`                          | `.calculateAdjustedStats`, `.plot_adj` |
| Adjustment Method                | `ac_method`                    | Adjustment Method                      | `plot_adj`                          | `.calculateAdjustedStats`, `.plot_adj` |
| **Nomogram**                     |                                |                                        |                                     |                                      |
| Show Nomogram                    | `showNomogram`                 | Show Nomogram                          | `plot_nomogram`, `nomogram_display` | `.nomogram`, `.plot_nomogram`        |
| **Model Selection**              |                                |                                        |                                     |                                      |
| Use Models                       | `use_modelSelection`           | Use Models                             | `text_model_selection`, `selection_method`, `text2_model_selection` | `.final_fit2`                        |
| Model Selection Method           | `modelSelection`               | Model Selection Method                 | `text_model_selection`, `selection_method`, `text2_model_selection` | `.final_fit2`                        |
| Selection Criteria               | `selectionCriteria`            | Selection Criteria                     | `text_model_selection`, `selection_method`, `text2_model_selection` | `.final_fit2`                        |
| Entry Significance               | `pEntry`                       | Entry Significance                     | `text_model_selection`, `selection_method`, `text2_model_selection` | `.final_fit2`                        |
| Removal Significance             | `pRemoval`                     | Removal Significance                   | `text_model_selection`, `selection_method`, `text2_model_selection` | `.final_fit2`                        |
| **Stratification**               |                                |                                        |                                     |                                      |
| Use Variable Stratification      | `use_stratify`                 | Use Variable Stratification            | `stratificationExplanation`         | `.final_fit`, `.cox_model`           |
| Stratification Variables         | `stratvar`                     | Stratification Variables               | `stratificationExplanation`         | `.final_fit`, `.cox_model`           |
| **Person-Time Metrics**          |                                |                                        |                                     |                                      |
| Calculate Person-Time Metrics    | `person_time`                  | Calculate Person-Time Metrics          | `personTimeTable`, `personTimeSummary` | `.personTimeAnalysis`                |
| Time Interval Stratification     | `time_intervals`               | Time Interval Stratification           | `personTimeTable`, `personTimeSummary` | `.personTimeAnalysis`                |
| Rate Multiplier                  | `rate_multiplier`              | Rate Multiplier                        | `personTimeTable`, `personTimeSummary` | `.personTimeAnalysis`                |
| **Survival Decision Tree**       |                                |                                        |                                     |                                      |
| Use Survival Decision Tree       | `use_tree`                     | Use Survival Decision Tree             | `tree_summary`, `tree_plot`, `node_survival_plots` | `.survivalTree`, `.plotTree`, `.plotNodeSurvival` |
| Minimum Node Size                | `min_node`                     | Minimum Node Size                      | `tree_summary`, `tree_plot`, `node_survival_plots` | `.survivalTree`                      |
| Complexity Parameter             | `complexity`                   | Complexity Parameter                   | `tree_summary`, `tree_plot`, `node_survival_plots` | `.survivalTree`                      |
| Maximum Tree Depth               | `max_depth`                    | Maximum Tree Depth                     | `tree_summary`, `tree_plot`, `node_survival_plots` | `.survivalTree`                      |
| Show Survival Curves for Terminal Nodes| `show_terminal_nodes`          | Show Survival Curves for Terminal Nodes| `node_survival_plots`               | `.plotNodeSurvival`                  |
| **Time-Dependent Covariates**    |                                |                                        |                                     |                                      |
| Use Time-Dependent Covariates    | `use_time_dependent`           | Use Time-Dependent Covariates          | `text`, `text2`                     | `.cox_model`                         |
| Data Format                      | `td_format`                    | Data Format                            | `text`, `text2`                     | `.cox_model`                         |
| Time-Dependent Variables         | `time_dep_vars`                | Time-Dependent Variables               | `text`, `text2`                     | `.cox_model`                         |
| Change Time Points               | `change_times`                 | Change Time Points                     | `text`, `text2`                     | `.cox_model`                         |
| Variable Suffix Pattern          | `td_suffix_pattern`            | Variable Suffix Pattern                | `text`, `text2`                     | `.cox_model`                         |
| Start Time Variable              | `start_time_var`               | Start Time Variable                    | `text`, `text2`                     | `.cox_model`                         |
| Stop Time Variable               | `stop_time_var`                | Stop Time Variable                     | `text`, `text2`                     | `.cox_model`                         |
| **Frailty Models**               |                                |                                        |                                     |                                      |
| Use Frailty Model                | `use_frailty`                  | Use Frailty Model                      | `text`, `text2`                     | `.cox_model`                         |
| Frailty Variable                 | `frailty_var`                  | Frailty Variable                       | `text`, `text2`                     | `.cox_model`                         |
| Frailty Distribution             | `frailty_distribution`         | Frailty Distribution                   | `text`, `text2`                     | `.cox_model`                         |
| **Spline Options**               |                                |                                        |                                     |                                      |
| Use Splines for Time-Varying Effects| `use_splines`                  | Use Splines for Time-Varying Effects   | `text`, `text2`                     | `.cox_model`                         |
| Variables with Time-Varying Effects| `spline_vars`                  | Variables with Time-Varying Effects    | `text`, `text2`                     | `.cox_model`                         |
| Spline Degrees of Freedom        | `spline_df`                    | Spline Degrees of Freedom              | `text`, `text2`                     | `.cox_model`                         |
| Spline Type                      | `spline_type`                  | Spline Type                            | `text`, `text2`                     | `.cox_model`                         |
