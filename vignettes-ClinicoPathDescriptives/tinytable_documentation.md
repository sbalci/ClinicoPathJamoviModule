# Modern Table Formatting Analysis Documentation

This document provides a comprehensive overview of the Modern Table Formatting module, detailing its features, user interface elements, and the underlying R functions.

## Feature Summary

The Modern Table Formatting module leverages the `tinytable` package to create publication-ready tables with a focus on clean aesthetics, flexibility, and zero dependencies. It supports various table types, including data summaries, descriptive statistics, grouped summaries, and raw data displays, with extensive customization options for styling, themes, and output formats.

## Feature Details

The following table provides a detailed mapping of the module's features, from the user interface to the underlying R functions.

| Feature                          | YAML Argument (`.a.yaml`)      | UI Label                               | Results Section (`.r.yaml`)         | R Function (`.b.R`)                  |
| -------------------------------- | ------------------------------ | -------------------------------------- | ----------------------------------- | ------------------------------------ |
| **Table Content & Data Handling**|                                |                                        |                                     |                                      |
| Variables to Display             | `vars`                         | Variables to Display                   | `table`                             | `.run`, `.create_summary_table`, `.create_descriptive_table`, `.create_grouped_table`, `.create_raw_table`, `.create_custom_table` |
| Grouping Variable (Optional)     | `group_var`                    | Grouping Variable (Optional)           | `table`                             | `.run`, `.create_summary_table`, `.create_descriptive_table`, `.create_grouped_table`, `.create_raw_table`, `.create_custom_table` |
| Table Type                       | `table_type`                   | Table Type                             | `table`                             | `.generate_table`                    |
| Include Statistics               | `show_statistics`              | Include Statistics                     | `table`                             | `.create_summary_table`              |
| Show Counts and Percentages      | `show_counts`                  | Show Counts and Percentages            | `table`                             | `.create_summary_table`              |
| Show Missing Values              | `show_missing`                 | Show Missing Values                    | `table`                             | `.create_summary_table`              |
| **Table Styling & Appearance**   |                                |                                        |                                     |                                      |
| Table Theme                      | `table_theme`                  | Table Theme                            | `table`                             | `.apply_theme`                       |
| Table Title                      | `table_title`                  | Table Title                            | `table`                             | `.generate_table`                    |
| Table Notes                      | `table_notes`                  | Table Notes                            | `table`                             | `.generate_table`                    |
| Primary Output Format            | `output_format`                | Primary Output Format                  | `table`                             | `.generate_table`                    |
| Table Width (proportion)         | `column_width`                 | Table Width (proportion)               | `table`                             | `.generate_table`                    |
| Decimal Precision                | `precision_digits`             | Decimal Precision                      | `table`                             | `.create_summary_table`, `.create_descriptive_table`, `.create_grouped_table`, `.create_raw_table`, `.create_custom_table` |
| Alternating Row Colors           | `style_alternating`            | Alternating Row Colors                 | `table`                             | `.apply_styling`                     |
| Table Borders                    | `style_borders`                | Table Borders                          | `table`                             | `.apply_styling`                     |
| Font Size                        | `font_size`                    | Font Size                              | `table`                             | `.apply_styling`                     |
| **Interpretation**               |                                |                                        |                                     |                                      |
| Show Usage Guide                 | `show_interpretation`          | Show Usage Guide                       | `interpretation`                    | `.generate_interpretation_guide`     |
