# Predictive Power Score Analysis Documentation

This document provides a comprehensive overview of the Predictive Power Score Analysis module, detailing its features, user interface elements, and the underlying R functions.

## Feature Summary

This module performs Predictive Power Score (PPS) analysis, which provides an asymmetric, data-type-agnostic score to detect linear or non-linear relationships between variables. PPS ranges from 0 (no predictive power) to 1 (perfect predictive power). The module supports various analysis types, including single predictor vs. target, multiple predictors vs. target, and full matrix analysis. It uses machine learning algorithms for PPS calculation, offers cross-validation, and allows comparison with traditional correlation methods. Comprehensive visualizations like heatmaps and bar plots are also available.

## Feature Details

The following table provides a detailed mapping of the module's features, from the user interface to the underlying R functions.

| Feature                          | YAML Argument (`.a.yaml`)      | UI Label                               | Results Section (`.r.yaml`)         | R Function (`.b.R`)                  |
| -------------------------------- | ------------------------------ | -------------------------------------- | ----------------------------------- | ------------------------------------ |
| **Analysis Type**                |                                |                                        |                                     |                                      |
| Analysis Type                    | `analysis_type`                | Analysis Type                          | `instructions`, `pps_scores`, `correlation_comparison`, `pps_heatmap`, `pps_barplot`, `comparison_plot`, `summary_stats`, `interpretation` | `.init`, `.run`, `.hasRequiredVariables`, `.showInstructions`, `.prepareData`, `.performPPSAnalysis`, `.performCorrelationComparison`, `.populatePPSScores`, `.populateCorrelationComparison`, `.populateSummaryStats`, `.populateInterpretation` |
| **Variable Selection**           |                                |                                        |                                     |                                      |
| Target Variable                  | `target_var`                   | Target Variable                        | `pps_scores`, `pps_barplot`, `summary_stats` | `.run`, `.prepareData`, `.performPPSAnalysis` |
| Predictor Variable               | `predictor_var`                | Predictor Variable                     | `pps_scores`, `pps_barplot`         | `.run`, `.prepareData`, `.performPPSAnalysis` |
| Predictor Variables              | `predictor_vars`               | Predictor Variables                    | `pps_scores`, `pps_barplot`         | `.run`, `.prepareData`, `.performPPSAnalysis` |
| Variables for Matrix             | `matrix_vars`                  | Variables for Matrix                   | `pps_scores`, `pps_heatmap`, `comparison_plot` | `.run`, `.prepareData`, `.performPPSAnalysis`, `.performCorrelationComparison` |
| **Algorithm & Validation**       |                                |                                        |                                     |                                      |
| Algorithm                        | `algorithm`                    | Algorithm                              | `pps_scores`                        | `.run`, `.performPPSAnalysis`        |
| Cross-Validation Folds           | `cv_folds`                     | Cross-Validation Folds                 | `pps_scores`                        | `.run`, `.performPPSAnalysis`        |
| Sample Size Limit                | `sample_size`                  | Sample Size Limit                      | `instructions`                      | `.run`, `.prepareData`               |
| **Comparison & Thresholds**      |                                |                                        |                                     |                                      |
| Show Correlation Comparison      | `show_correlation_comparison`  | Show Correlation Comparison            | `correlation_comparison`, `comparison_plot` | `.run`, `.performCorrelationComparison` |
| Correlation Method               | `correlation_method`           | Correlation Method                     | `correlation_comparison`, `comparison_plot` | `.run`, `.performCorrelationComparison` |
| Minimum PPS Threshold            | `min_pps_threshold`            | Minimum PPS Threshold                  | `pps_scores`, `summary_stats`       | `.populatePPSScores`, `.populateSummaryStats` |
| Sort Results                     | `sort_results`                 | Sort Results                           | `pps_scores`                        | `.populatePPSScores`                 |
| **Plotting Options**             |                                |                                        |                                     |                                      |
| Show PPS Heatmap                 | `show_heatmap`                 | Show PPS Heatmap                       | `pps_heatmap`                       | `.plot_heatmap`                      |
| Show PPS Barplot                 | `show_barplot`                 | Show PPS Barplot                       | `pps_barplot`                       | `.plot_barplot`                      |
| Show Values on Plot              | `show_values_on_plot`          | Show Values on Plot                    | `pps_heatmap`, `pps_barplot`        | `.plot_heatmap`, `.plot_barplot`     |
| Custom Plot Title                | `plot_title`                   | Custom Plot Title                      | `pps_heatmap`, `pps_barplot`        | `.plot_heatmap`, `.plot_barplot`     |
| Plot Color Scheme                | `color_scheme`                 | Plot Color Scheme                      | `pps_heatmap`, `pps_barplot`, `comparison_plot` | `.plot_heatmap`, `.plot_barplot`, `.plot_comparison` |
| Custom Color Low                 | `custom_color_low`             | Low Value Color                        | `pps_heatmap`                       | `.plot_heatmap`                      |
| Custom Color High                | `custom_color_high`            | High Value Color                       | `pps_heatmap`                       | `.plot_heatmap`                      |
| **Output & Interpretation**      |                                |                                        |                                     |                                      |
| Show Analysis Summary            | `show_summary`                 | Show Analysis Summary                  | `summary_stats`                     | `.populateSummaryStats`              |
| Show Interpretation Guide        | `show_interpretation`          | Show Interpretation Guide              | `interpretation`                    | `.populateInterpretation`            |
