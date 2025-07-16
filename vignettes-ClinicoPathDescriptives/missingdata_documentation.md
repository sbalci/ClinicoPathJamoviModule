# Missing Data Analysis and Multiple Imputation Documentation

This document provides a comprehensive overview of the Missing Data Analysis and Multiple Imputation module, detailing its features, user interface elements, and the underlying R functions.

## Feature Summary

This module offers comprehensive missing data analysis and multiple imputation using the `mice` and `ggmice` packages. It provides a complete workflow for analyzing missing data patterns, performing multiple imputation by chained equations (MICE), and evaluating imputation quality. Designed specifically for clinical research applications where missing data is common and proper handling is critical for valid statistical inference, it includes visual and tabular pattern analysis, convergence diagnostics, and quality evaluation comparing observed vs. imputed data.

## Feature Details

The following table provides a detailed mapping of the module's features, from the user interface to the underlying R functions.

| Feature                          | YAML Argument (`.a.yaml`)      | UI Label                               | Results Section (`.r.yaml`)         | R Function (`.b.R`)                  |
| -------------------------------- | ------------------------------ | -------------------------------------- | ----------------------------------- | ------------------------------------ |
| **Variables & Analysis Type**    |                                |                                        |                                     |                                      |
| Variables for Analysis           | `analysis_vars`                | Variables for Analysis                 | `pattern_plot`, `pattern_table`, `correlation_plot`, `flux_plot`, `trace_plot`, `density_plot`, `stripplot`, `scatterplot`, `imputation_summary`, `interpretation` | `.run`, `.validate_and_prepare_data`, `.generate_pattern_table`, `.perform_imputation` |
| Analysis Type                    | `analysis_type`                | Analysis Type                          | `pattern_plot`, `pattern_table`, `correlation_plot`, `flux_plot`, `trace_plot`, `density_plot`, `stripplot`, `scatterplot`, `imputation_summary`, `interpretation` | `.run`, `.generate_pattern_table`, `.perform_imputation` |
| **Imputation Parameters**        |                                |                                        |                                     |                                      |
| Number of Imputations            | `n_imputations`                | Number of Imputations                  | `trace_plot`, `density_plot`, `stripplot`, `scatterplot`, `imputation_summary` | `.run`, `.perform_imputation`        |
| Maximum Iterations               | `max_iterations`               | Maximum Iterations                     | `trace_plot`, `density_plot`, `stripplot`, `scatterplot`, `imputation_summary` | `.run`, `.perform_imputation`        |
| Default Imputation Method        | `imputation_method`            | Default Imputation Method              | `trace_plot`, `density_plot`, `stripplot`, `scatterplot`, `imputation_summary` | `.run`, `.perform_imputation`        |
| Random Seed                      | `seed_value`                   | Random Seed                            | `trace_plot`, `density_plot`, `stripplot`, `scatterplot`, `imputation_summary` | `.run`, `.perform_imputation`        |
| **Display Options**              |                                |                                        |                                     |                                      |
| Check Convergence                | `convergence_check`            | Check Convergence                      | `trace_plot`                        | `.run`                               |
| Show Missing Pattern Plot        | `show_pattern_plot`            | Show Missing Pattern Plot              | `pattern_plot`                      | `.plot_pattern`                      |
| Show Missing Pattern Table       | `show_pattern_table`           | Show Missing Pattern Table             | `pattern_table`                     | `.generate_pattern_table`            |
| Show Correlation Plot            | `show_correlation_plot`        | Show Correlation Plot                  | `correlation_plot`                  | `.plot_correlation`                  |
| Show Influx/Outflux Plot         | `show_flux_plot`               | Show Influx/Outflux Plot               | `flux_plot`                         | `.plot_flux`                         |
| Show Trace Plot                  | `show_trace_plot`              | Show Trace Plot                        | `trace_plot`                        | `.plot_trace`                        |
| Show Density Comparison          | `show_density_plot`            | Show Density Comparison                | `density_plot`                      | `.plot_density`                      |
| Show Strip Plot                  | `show_stripplot`               | Show Strip Plot                        | `stripplot`                         | `.plot_strip`                        |
| Show Scatter Plot                | `show_scatterplot`             | Show Scatter Plot                      | `scatterplot`                       | `.plot_scatter`                      |
| Show Imputation Summary          | `show_imputation_summary`      | Show Imputation Summary                | `imputation_summary`                | `.generate_imputation_summary`       |
| Show Usage Guide                 | `show_interpretation`          | Show Usage Guide                       | `interpretation`                    | `.generate_interpretation_guide`     |
