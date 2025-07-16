# Enhanced Summary Statistics for Continuous and Date Variables Documentation

This document provides a comprehensive overview of the Enhanced Summary Statistics for Continuous and Date Variables module, detailing its features, user interface elements, and the underlying R functions.

## Feature Summary

This module offers comprehensive descriptive statistics for continuous and date variables, with multiple output formats and enhanced visualization capabilities. It supports various summary formats, including standard descriptives, enhanced pivot tables, and automated EDA reports using the `summarytools` package. The module is designed to provide robust data quality assessment and flexible reporting options for clinical and research data.

## Feature Details

The following table provides a detailed mapping of the module's features, from the user interface to the underlying R functions.

| Feature                          | YAML Argument (`.a.yaml`)      | UI Label                               | Results Section (`.r.yaml`)         | R Function (`.b.R`)                  |
| -------------------------------- | ------------------------------ | -------------------------------------- | ----------------------------------- | ------------------------------------ |
| **Variable Selection**           |                                |                                        |                                     |                                      |
| Variables                        | `vars`                         | Variables                              | `text`, `text1`, `pivot_summary`    | `.validateInputs`, `.run`, `.mysummary` |
| Date Variables                   | `date_vars`                    | Date Variables                         | `text`, `text1`, `pivot_summary`    | `.validateInputs`, `.run`, `.mysummary` |
| **Summary Format & Diagnostics** |                                |                                        |                                     |                                      |
| Distribution Diagnostics         | `distr`                        | Distribution Diagnostics               | `text`                              | `.mysummary`                         |
| Summary Format                   | `summary_format`               | Summary Format                         | `text`, `pivot_summary`             | `.run`, `.mysummary`, `.generate_pivot_summary`, `.generate_summarytools_output`, `.generate_comprehensive_summarytools` |
| **Grouping & Pivot Table Options**|                                |                                        |                                     |                                      |
| Group Variable                   | `grvar`                        | Group Variable                         | `text`, `pivot_summary`             | `.validateInputs`, `.run`            |
| Pivot Layout Style               | `pivot_layout`                 | Pivot Layout Style                     | `pivot_summary`                     | `.generate_pivot_summary`, `.create_styled_table` |
| Include Confidence Intervals     | `include_confidence`           | Include Confidence Intervals           | `pivot_summary`                     | `.generate_pivot_summary`            |
| Advanced Statistical Metrics     | `advanced_metrics`             | Advanced Statistical Metrics           | `pivot_summary`                     | `.generate_pivot_summary`            |
| Enable Pivot Export              | `pivot_export`                 | Enable Pivot Export                    | `pivot_export_info`                 | `.generate_export_info`              |
| **summarytools Integration**     |                                |                                        |                                     |                                      |
| Include Graphics in summarytools | `summarytools_graphs`          | Include Graphics in summarytools       | `text`                              | `.generate_comprehensive_summarytools` |
| Round Digits                     | `summarytools_round_digits`    | Round Digits                           | `text`                              | `.generate_comprehensive_summarytools` |
