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
