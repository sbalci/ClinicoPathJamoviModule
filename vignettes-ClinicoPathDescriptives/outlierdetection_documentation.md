# Advanced Outlier Detection Analysis Documentation

This document provides a comprehensive overview of the Advanced Outlier Detection module, detailing its features, user interface elements, and the underlying R functions.

## Feature Summary

This module performs advanced outlier detection using multiple statistical methods from the `easystats performance` package. It offers comprehensive outlier detection through univariate methods (Z-scores, IQR, confidence intervals), multivariate methods (Mahalanobis distance, MCD, OPTICS, LOF), and composite scoring across multiple algorithms. This tool is ideal for clinical research data quality control and preprocessing, complementing existing data quality assessment modules with state-of-the-art outlier detection capabilities.

## Feature Details

The following table provides a detailed mapping of the module's features, from the user interface to the underlying R functions.

| Feature                          | YAML Argument (`.a.yaml`)      | UI Label                               | Results Section (`.r.yaml`)         | R Function (`.b.R`)                  |
| -------------------------------- | ------------------------------ | -------------------------------------- | ----------------------------------- | ------------------------------------ |
| **Variable Selection**           |                                |                                        |                                     |                                      |
| Variables for Analysis           | `vars`                         | Variables for Analysis                 | `plot`, `outlier_table`, `method_comparison`, `exclusion_summary`, `interpretation` | `.run`, `.perform_outlier_detection` |
| **Detection Method Configuration**|                                |                                        |                                     |                                      |
| Detection Method Category        | `method_category`              | Detection Method Category              | `plot`, `outlier_table`, `method_comparison`, `exclusion_summary`, `interpretation` | `.run`, `.perform_outlier_detection`, `.get_method_description` |
| Univariate Methods               | `univariate_methods`           | Univariate Methods                     | `plot`, `outlier_table`, `method_comparison`, `exclusion_summary`, `interpretation` | `.run`, `.perform_outlier_detection`, `.get_univariate_threshold`, `.get_method_description` |
| Multivariate Methods             | `multivariate_methods`         | Multivariate Methods                   | `plot`, `outlier_table`, `method_comparison`, `exclusion_summary`, `interpretation` | `.run`, `.perform_outlier_detection`, `.get_method_description` |
| Composite Score Threshold        | `composite_threshold`          | Composite Score Threshold              | `plot`, `outlier_table`, `method_comparison`, `exclusion_summary`, `interpretation` | `.run`, `.perform_outlier_detection` |
| Z-Score Threshold                | `zscore_threshold`             | Z-Score Threshold                      | `plot`, `outlier_table`, `method_comparison`, `exclusion_summary`, `interpretation` | `.run`, `.get_univariate_threshold`  |
| IQR Multiplier                   | `iqr_multiplier`               | IQR Multiplier                         | `plot`, `outlier_table`, `method_comparison`, `exclusion_summary`, `interpretation` | `.run`, `.get_univariate_threshold`  |
| Confidence Level for Intervals   | `confidence_level`             | Confidence Level for Intervals         | `plot`, `outlier_table`, `method_comparison`, `exclusion_summary`, `interpretation` | `.run`, `.get_univariate_threshold`  |
| **Output Options**               |                                |                                        |                                     |                                      |
| Show Outlier Summary Table       | `show_outlier_table`           | Show Outlier Summary Table             | `outlier_table`                     | `.generate_outlier_table`            |
| Show Method Comparison           | `show_method_comparison`       | Show Method Comparison                 | `method_comparison`                 | `.generate_method_comparison`        |
| Show Exclusion Recommendations   | `show_exclusion_summary`       | Show Exclusion Recommendations         | `exclusion_summary`                 | `.generate_exclusion_summary`        |
| Show Outlier Visualization       | `show_visualization`           | Show Outlier Visualization             | `plot`                              | `.plot`                              |
| Show Analysis Interpretation     | `show_interpretation`          | Show Analysis Interpretation           | `interpretation`                    | `.generate_interpretation_guide`     |
