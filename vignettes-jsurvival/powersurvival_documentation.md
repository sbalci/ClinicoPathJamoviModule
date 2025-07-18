# Power Analysis for Survival Studies Documentation

This document provides a comprehensive overview of the Power Analysis for Survival Studies module, detailing its features, user interface elements, and the underlying R functions.

## Feature Summary

This module provides comprehensive power analysis for survival studies, allowing researchers to calculate statistical power, required sample size, or minimum detectable hazard ratio for survival analysis designs. It supports both simple designs (fixed follow-up) and complex designs with accrual periods, variable follow-up times, and loss to follow-up. The module is based on the Freedman method for survival studies with Cox proportional hazards models and offers interactive plots and comprehensive educational output.

## Feature Details

The following table provides a detailed mapping of the module's features, from the user interface to the underlying R functions.

| Feature                          | YAML Argument (`.a.yaml`)      | UI Label                               | Results Section (`.r.yaml`)         | R Function (`.b.R`)                  |
| -------------------------------- | ------------------------------ | -------------------------------------- | ----------------------------------- | ------------------------------------ |
| **Analysis Type**                |                                |                                        |                                     |                                      |
| Calculate                        | `calc_type`                    | Calculate                              | `power_result`, `sample_size_result`, `hazard_ratio_result` | `.run`                               |
| **Study Design**                 |                                |                                        |                                     |                                      |
| Study Design                     | `study_design`                 | Study Design                           | `power_result`, `sample_size_result`, `hazard_ratio_result` | `.run`                               |
| **Basic Parameters**             |                                |                                        |                                     |                                      |
| Hazard Ratio                     | `hazard_ratio`                 | Hazard Ratio                           | `power_result`, `sample_size_result`, `hazard_ratio_result` | `.run`                               |
| Power (1-β)                      | `power`                        | Power (1-β)                            | `power_result`, `sample_size_result`, `hazard_ratio_result` | `.run`                               |
| Significance Level (α)           | `alpha`                        | Significance Level (α)                 | `power_result`, `sample_size_result`, `hazard_ratio_result` | `.run`                               |
| Total Sample Size                | `sample_size`                  | Total Sample Size                      | `power_result`, `sample_size_result`, `hazard_ratio_result` | `.run`                               |
| Allocation Ratio (Control:Treatment)| `allocation_ratio`             | Allocation Ratio (Control:Treatment)   | `power_result`, `sample_size_result`, `hazard_ratio_result` | `.run`                               |
| Event Probability                | `prob_event`                   | Event Probability                      | `power_result`, `sample_size_result`, `hazard_ratio_result` | `.run`                               |
| **Advanced Parameters**          |                                |                                        |                                     |                                      |
| Accrual Time                     | `accrual_time`                 | Accrual Time                           | `power_result`, `sample_size_result`, `hazard_ratio_result` | `.run`                               |
| Follow-up Time                   | `follow_up_time`               | Follow-up Time                         | `power_result`, `sample_size_result`, `hazard_ratio_result` | `.run`                               |
| Median Survival Time (Control Group)| `median_survival`              | Median Survival Time (Control Group)   | `power_result`, `sample_size_result`, `hazard_ratio_result` | `.run`                               |
| Loss to Follow-up Rate (per Year)| `loss_followup`                | Loss to Follow-up Rate (per Year)      | `power_result`, `sample_size_result`, `hazard_ratio_result` | `.run`                               |
| **Output & Visualization**       |                                |                                        |                                     |                                      |
| Power Analysis Plot              | `power_plot`                   | Power Analysis Plot                    | `power_plot`                        | `.power_plot`                        |
