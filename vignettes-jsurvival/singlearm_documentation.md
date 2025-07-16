# Single Arm Survival Analysis Documentation

This document provides a comprehensive overview of the Single Arm Survival Analysis module, detailing its features, user interface elements, and the underlying R functions.

## Feature Summary

The Single Arm Survival Analysis module is designed to analyze survival outcomes for a single cohort of patients without group comparisons. It calculates key survival metrics such as median survival time, survival rates at specified cutpoints, and person-time metrics. The module supports various input formats for time data (pre-calculated or date-based) and outcome types (binary or multi-event), offering comprehensive visualization options including Kaplan-Meier curves, cumulative events/hazard plots, and KMunicate-style plots.

## Feature Details

The following table provides a detailed mapping of the module's features, from the user interface to the underlying R functions.

| Feature                          | YAML Argument (`.a.yaml`)      | UI Label                               | Results Section (`.r.yaml`)         | R Function (`.b.R`)                  |
| -------------------------------- | ------------------------------ | -------------------------------------- | ----------------------------------- | ------------------------------------ |
| **Core Survival Analysis**       |                                |                                        |                                     |                                      |
| Time Elapsed                     | `elapsedtime`                  | Time Elapsed                           | `medianSummary`, `medianTable`, `survTableSummary`, `survTable`, `plot`, `plot2`, `plot3`, `plot6`, `personTimeTable`, `personTimeSummary`, `calculatedtime`, `outcomeredefined` | `.run`, `.definemytime`, `.cleandata`, `.medianSurv`, `.survTable`, `.personTimeAnalysis`, `.plot`, `.plot2`, `.plot3`, `.plot6` |
| Using Dates to Calculate Survival Time| `tint`                         | Using Dates to Calculate Survival Time | `calculatedtime`                    | `.definemytime`, `.cleandata`        |
| Diagnosis Date                   | `dxdate`                       | Diagnosis Date                         | `calculatedtime`                    | `.definemytime`, `.cleandata`        |
| Follow-up Date                   | `fudate`                       | Follow-up Date                         | `calculatedtime`                    | `.definemytime`, `.cleandata`        |
| Add Calculated Time to Data      | `calculatedtime`               | Add Calculated Time to Data            | `calculatedtime`                    | `.run`                               |
| Outcome                          | `outcome`                      | Outcome                                | `medianSummary`, `medianTable`, `survTableSummary`, `survTable`, `plot`, `plot2`, `plot3`, `plot6`, `personTimeTable`, `personTimeSummary`, `outcomeredefined` | `.run`, `.definemyoutcome`, `.cleandata`, `.medianSurv`, `.survTable`, `.personTimeAnalysis`, `.plot`, `.plot2`, `.plot3`, `.plot6` |
| Event Level                      | `outcomeLevel`                 | Event Level                            | `medianSummary`, `medianTable`, `survTableSummary`, `survTable`, `plot`, `plot2`, `plot3`, `plot6`, `personTimeTable`, `personTimeSummary` | `.run`, `.definemyoutcome`, `.cleandata`, `.medianSurv`, `.survTable`, `.personTimeAnalysis`, `.plot`, `.plot2`, `.plot3`, `.plot6` |
| Dead of Disease                  | `dod`                          | Dead of Disease                        | `outcomeredefined`                  | `.definemyoutcome`                   |
| Dead of Other                    | `dooc`                         | Dead of Other                          | `outcomeredefined`                  | `.definemyoutcome`                   |
| Alive w Disease                  | `awd`                          | Alive w Disease                        | `outcomeredefined`                  | `.definemyoutcome`                   |
| Alive w/o Disease                | `awod`                         | Alive w/o Disease                      | `outcomeredefined`                  | `.definemyoutcome`                   |
| Survival Type                    | `analysistype`                 | Survival Type                          | `outcomeredefined`                  | `.definemyoutcome`                   |
| Add Redefined Outcome to Data    | `outcomeredefined`             | Add Redefined Outcome to Data          | `outcomeredefined`                  | `.run`                               |
| Cutpoints                        | `cutp`                         | Cutpoints                              | `survTableSummary`, `survTable`     | `.survTable`                         |
| Time Type in Data                | `timetypedata`                 | Time Type in Data                      | `calculatedtime`                    | `.definemytime`                      |
| Time Type in Output              | `timetypeoutput`               | Time Type in Output                    | `medianSummary`, `survTableSummary`, `plot`, `plot2`, `plot3`, `plot6`, `personTimeSummary` | `.definemytime`, `.medianSurv`, `.survTable`, `.personTimeAnalysis`, `.plot`, `.plot2`, `.plot3`, `.plot6` |
| Use Landmark Time                | `uselandmark`                  | Use Landmark Time                      | `medianSummary`, `medianTable`, `survTableSummary`, `survTable`, `plot`, `plot2`, `plot3`, `plot6` | `.cleandata`, `.medianSurv`, `.survTable`, `.plot`, `.plot2`, `.plot3`, `.plot6` |
| Landmark Time                    | `landmark`                     | Landmark Time                          | `medianSummary`, `medianTable`, `survTableSummary`, `survTable`, `plot`, `plot2`, `plot3`, `plot6` | `.cleandata`, `.medianSurv`, `.survTable`, `.plot`, `.plot2`, `.plot3`, `.plot6` |
| Kaplan-Meier Survival Plot       | `sc`                           | Kaplan-Meier Survival Plot             | `plot`                              | `.plot`                              |
| KMunicate-Style Plot             | `kmunicate`                    | KMunicate-Style Plot                   | `plot6`                             | `.plot6`                             |
| Cumulative Events                | `ce`                           | Cumulative Events                      | `plot2`                             | `.plot2`                             |
| Cumulative Hazard                | `ch`                           | Cumulative Hazard                      | `plot3`                             | `.plot3`                             |
| Plot End Time                    | `endplot`                      | Plot End Time                          | `plot`, `plot2`, `plot3`, `plot6`   | `.plot`, `.plot2`, `.plot3`, `.plot6` |
| Start y-axis                     | `ybegin_plot`                  | Start y-axis                           | `plot`, `plot2`, `plot3`            | `.plot`, `.plot2`, `.plot3`          |
| End y-axis                       | `yend_plot`                    | End y-axis                             | `plot`, `plot2`, `plot3`            | `.plot`, `.plot2`, `.plot3`          |
| Time Interval                    | `byplot`                       | Time Interval                          | `plot`, `plot2`, `plot3`, `plot6`   | `.plot`, `.plot2`, `.plot3`, `.plot6` |
| Multiple Event Levels            | `multievent`                   | Multiple Event Levels                  | `outcomeredefined`                  | `.definemyoutcome`                   |
| 95% CI                           | `ci95`                         | 95% CI                                 | `plot`, `plot2`, `plot3`            | `.plot`, `.plot2`, `.plot3`          |
| risktable                        | `risktable`                    | risktable                              | `plot`, `plot2`, `plot3`            | `.plot`, `.plot2`, `.plot3`          |
| censored                         | `censored`                     | censored                               | `plot`, `plot2`, `plot3`            | `.plot`, `.plot2`, `.plot3`          |
| medianline                       | `medianline`                   | medianline                             | `plot`, `plot2`, `plot3`            | `.plot`, `.plot2`, `.plot3`          |
| Calculate Person-Time Metrics    | `person_time`                  | Calculate Person-Time Metrics          | `personTimeTable`, `personTimeSummary` | `.personTimeAnalysis`                |
| Time Interval Stratification     | `time_intervals`               | Time Interval Stratification           | `personTimeTable`, `personTimeSummary` | `.personTimeAnalysis`                |
| Rate Multiplier                  | `rate_multiplier`              | Rate Multiplier                        | `personTimeTable`, `personTimeSummary` | `.personTimeAnalysis`                |
