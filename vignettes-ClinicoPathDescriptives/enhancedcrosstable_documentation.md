# Enhanced Cross Tables Analysis Documentation

This document provides a comprehensive overview of the Enhanced Cross Tables module, detailing its features, user interface elements, and the underlying R functions.

## Feature Summary

The Enhanced Cross Tables module is a powerful tool for performing advanced cross-tabulation analysis using the `danchaltiel/crosstable` package. It offers a wide range of features for clinical research, including automated statistical test selection, effect size calculations, flexible percentage patterns, and publication-ready output formatting.

The module's features can be broadly categorized as follows:

*   **Core Cross-tabulation:** Basic functionality to create cross-tables with customizable percentage displays.
*   **Automated Statistical Analysis:** Intelligent selection and performance of statistical tests.
*   **Effect Size Calculation:** Comprehensive calculation of effect sizes for observed associations.
*   **Output Customization:** Options for decimal places, variable labels, missing value handling, and export formats.
*   **Interpretation Guide:** Provides statistical interpretation and summary statistics.

## Feature Details

The following table provides a detailed mapping of the module's features, from the user interface to the underlying R functions.

| Feature                          | YAML Argument (`.a.yaml`)      | UI Label                               | Results Section (`.r.yaml`)         | R Function (`.b.R`)                  |
| -------------------------------- | ------------------------------ | -------------------------------------- | ----------------------------------- | ------------------------------------ |
| **Core Cross-tabulation**        |                                |                                        |                                     |                                      |
| Variables for Cross-tabulation   | `vars`                         | Variables for Cross-tabulation         | `crosstable_main`                   | `.run`, `.process_data`, `.generate_crosstable_simple` |
| Grouping Variable (Columns)      | `by_var`                       | Grouping Variable (Columns)            | `crosstable_main`                   | `.run`, `.process_data`, `.generate_crosstable_simple` |
| Percentage Display Pattern       | `percent_pattern`              | Percentage Display Pattern             | `crosstable_main`                   | `.generate_crosstable_simple`        |
| Show Total Column                | `show_total`                   | Show Total Column                      | `crosstable_main`                   | `.generate_crosstable_simple`        |
| Show Total Row                   | `show_total_row`               | Show Total Row                         | `crosstable_main`                   | `.generate_crosstable_simple`        |
| Exclude Missing Values           | `exclude_missing`              | Exclude Missing Values                 | `crosstable_main`, `summary_stats`  | `.process_data`                      |
| Show Column Sample Sizes         | `show_n_col`                   | Show Column Sample Sizes               | `crosstable_main`                   | `.generate_crosstable_simple`        |
| Margin for Percentages           | `margin`                       | Margin for Percentages                 | `crosstable_main`, `summary_stats`  | `.generate_crosstable_simple`        |
| Decimal Places                   | `digits`                       | Decimal Places                         | `crosstable_main`, `summary_stats`  | `.generate_crosstable_simple`        |
| Use Variable Labels              | `use_labels`                   | Use Variable Labels                    | `crosstable_main`                   | `.process_data`                      |
| Show Missing Values              | `showNA`                       | Show Missing Values                    | `crosstable_main`                   | `.generate_crosstable_simple`        |
| Compact Output                   | `compact`                      | Compact Output                         | `crosstable_main`                   | (Not directly implemented in .b.R for MVP) |
| **Statistical Options**          |                                |                                        |                                     |                                      |
| Automatic Statistical Tests      | `test_auto`                    | Automatic Statistical Tests            | `statistics_table`, `interpretation`| `.run`, `.generate_statistics`       |
| Calculate Effect Sizes           | `effect_size`                  | Calculate Effect Sizes                 | `effect_sizes`, `interpretation`    | `.run`, `.generate_effect_sizes`     |
| Summary Function                 | `funs_arg`                     | Summary Function                       | `summary_stats`                     | `.generate_summary`                  |
| Correlation Method               | `cor_method`                   | Correlation Method                     | `statistics_table`                  | (Not directly implemented in .b.R for MVP) |
| **Output Options**               |                                |                                        |                                     |                                      |
| Export Format                    | `export_format`                | Export Format                          | `export_data`                       | (Not directly implemented in .b.R for MVP) |
| Show Statistical Interpretation  | `show_interpretation`          | Show Statistical Interpretation        | `interpretation`                    | `.run`, `.generate_interpretation`   |
