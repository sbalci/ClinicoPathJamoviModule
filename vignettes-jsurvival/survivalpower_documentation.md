# Survival Analysis Power & Sample Size Documentation

This document provides a comprehensive overview of the Survival Analysis Power & Sample Size module, detailing its features, user interface elements, and the underlying R functions.

## Feature Summary

The Survival Analysis Power & Sample Size module performs power analysis and sample size calculations for survival studies. It supports two primary methods: Lachin-Foulkes for full study design considerations (accrual, follow-up, dropout) and Schoenfeld for events-based approximations. This module is crucial for designing clinical trials with time-to-event endpoints, ensuring adequate statistical power to detect clinically meaningful differences.

## Feature Details

The following table provides a detailed mapping of the module's features, from the user interface to the underlying R functions.

| Feature                          | YAML Argument (`.a.yaml`)      | UI Label                               | Results Section (`.r.yaml`)         | R Function (`.b.R`)                  |
| -------------------------------- | ------------------------------ | -------------------------------------- | ----------------------------------- | ------------------------------------ |
| **Core Calculation Setup**       |                                |                                        |                                     |                                      |
| Calculation Type                 | `calculation_type`             | Calculation Type                       | `power_results`, `formulas`, `interpretation`, `power_plot`, `timeline_plot` | `.run`, `.calculate_power`           |
| Calculation Method               | `method`                       | Calculation Method                     | `power_results`, `formulas`, `interpretation`, `power_plot`, `timeline_plot` | `.run`, `.calculate_power`           |
| Control Group Hazard Rate        | `hazard_control`               | Control Group Hazard Rate              | `power_results`, `power_plot`, `timeline_plot` | `.calculate_lachin_foulkes`          |
| Treatment Group Hazard Rate      | `hazard_treatment`             | Treatment Group Hazard Rate            | `power_results`, `power_plot`, `timeline_plot` | `.calculate_lachin_foulkes`          |
| Hazard Ratio                     | `hazard_ratio`                 | Hazard Ratio                           | `power_results`, `power_plot`       | `.calculate_schoenfeld`              |
| Total Study Duration (Ts)        | `study_duration`               | Total Study Duration (Ts)              | `power_results`, `timeline_plot`    | `.calculate_lachin_foulkes`          |
| Accrual Duration (Tr)            | `accrual_duration`             | Accrual Duration (Tr)                  | `power_results`, `timeline_plot`    | `.calculate_lachin_foulkes`          |
| Dropout Hazard Rate              | `dropout_rate`                 | Dropout Hazard Rate                    | `power_results`                     | `.calculate_lachin_foulkes`          |
| Allocation Ratio                 | `allocation_ratio`             | Allocation Ratio                       | `power_results`                     | `.calculate_lachin_foulkes`, `.calculate_schoenfeld` |
| Type I Error Rate (α)            | `alpha`                        | Type I Error Rate (α)                  | `power_results`, `formulas`, `interpretation`, `power_plot` | `.calculate_lachin_foulkes`, `.calculate_schoenfeld` |
| Type II Error Rate (β)           | `beta`                         | Type II Error Rate (β)                 | `power_results`, `formulas`, `interpretation`, `power_plot` | `.calculate_lachin_foulkes`, `.calculate_schoenfeld` |
| Statistical Power                | `power`                        | Statistical Power                      | `power_results`, `formulas`, `interpretation`, `power_plot` | `.calculate_lachin_foulkes`, `.calculate_schoenfeld` |
| Test Type                        | `sided`                        | Test Type                              | `power_results`                     | `.calculate_lachin_foulkes`, `.calculate_schoenfeld` |
| Patient Entry Pattern            | `entry_type`                   | Patient Entry Pattern                  | `power_results`                     | `.calculate_lachin_foulkes`          |
| Exponential Entry Parameter      | `gamma`                        | Exponential Entry Parameter            | `power_results`                     | `.calculate_lachin_foulkes`          |
| Sample Size (for Power Calculation)| `sample_size_input`            | Sample Size (for Power Calculation)    | `power_results`                     | `.calculate_lachin_foulkes`, `.calculate_schoenfeld` |
| Number of Events (for Power Calculation)| `events_input`                 | Number of Events (for Power Calculation)| `power_results`                     | `.calculate_schoenfeld`              |
| **Output & Visualization**       |                                |                                        |                                     |                                      |
| Show Study Design Summary        | `show_summary`                 | Show Study Design Summary              | `power_results`                     | `.generate_summary`                  |
| Show Mathematical Formulas       | `show_formulas`                | Show Mathematical Formulas             | `formulas`                          | `.generate_formulas`                 |
| Show Clinical Interpretation     | `show_interpretation`          | Show Clinical Interpretation           | `interpretation`                    | `.generate_interpretation`           |
| Show Power Curve Plot            | `show_power_plot`              | Show Power Curve Plot                  | `power_plot`                        | `.plot_power_curve`                  |
| Show Study Timeline Plot         | `show_timeline_plot`           | Show Study Timeline Plot               | `timeline_plot`                     | `.plot_timeline`                     |
| Power Plot Sample Size Range     | `power_plot_range`             | Power Plot Sample Size Range           | `power_plot`                        | `.generate_power_curve_data`         |
| **Export Options**               |                                |                                        |                                     |                                      |
| Export Results to Data           | `export_results`               | Export Results to Data                 | `exported_results`, `export_summary`| `.export_results`, `.generate_export_summary` |
| Export Power Curve Data          | `export_power_curve`           | Export Power Curve Data                | `exported_power_curve`, `export_summary`| `.export_power_curve`, `.generate_export_summary` |
