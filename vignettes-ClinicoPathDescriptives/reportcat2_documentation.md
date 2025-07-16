# Summary of Categorical Variables Analysis Documentation

This document provides a comprehensive overview of the Summary of Categorical Variables Analysis module, detailing its features, user interface elements, and the underlying R functions.

## Feature Summary

This module generates a comprehensive summary of categorical variables, including frequency counts, percentages, and missing value information. It supports multiple output formats and sorting options for enhanced data exploration. The module automatically handles edge cases like missing values, single categories, and variable conversion, ensuring robust and reliable results for clinical and research data.

## Feature Details

The following table provides a detailed mapping of the module's features, from the user interface to the underlying R functions.

| Feature                          | YAML Argument (`.a.yaml`)      | UI Label                               | Results Section (`.r.yaml`)         | R Function (`.b.R`)                  |
| -------------------------------- | ------------------------------ | -------------------------------------- | ----------------------------------- | ------------------------------------ |
| **Core Summary**                 |                                |                                        |                                     |                                      |
| Variables                        | `vars`                         | Variables                              | `text`, `text1`                     | `.run`, `.catsummary`                |
| **Output Options**               |                                |                                        |                                     |                                      |
| Enhanced Categorical Summary (tab1 style)| `sumvar_style`                 | Enhanced Categorical Summary (tab1 style)| `text`                              | `.catsummary`                        |
| Show Proportions                 | `show_proportions`             | Show Proportions                       | `text`                              | `.catsummary`                        |
| Sort by Frequency                | `sort_by_frequency`            | Sort by Frequency                      | `text`                              | `.catsummary`                        |
