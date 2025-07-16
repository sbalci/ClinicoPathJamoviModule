# Enhanced Cross Tables (Pivot) Analysis Documentation

This document provides a comprehensive overview of the Enhanced Cross Tables (Pivot) module, detailing its features, user interface elements, and the underlying R functions.

## Feature Summary

The Enhanced Cross Tables (Pivot) module is designed to create flexible and professional cross tables using the `pivottabler` library. It offers a pivot-style layout for data analysis, allowing users to easily summarize and present categorical and numerical data. The module supports various formatting styles and includes options for statistical summaries and export capabilities.

The module's features can be broadly categorized as follows:

*   **Flexible Table Generation:** Define rows and columns using selected variables.
*   **Statistical Summaries:** Include counts and percentages in the table.
*   **Customizable Formatting:** Apply different styles (Standard, Clinical, Publication) to the table.
*   **Export Functionality:** Option to enable Excel export for the generated tables.
*   **Instructions and Information:** Provides in-app guidance and summary statistics.

## Feature Details

The following table provides a detailed mapping of the module's features, from the user interface to the underlying R functions.

| Feature                          | YAML Argument (`.a.yaml`)      | UI Label                               | Results Section (`.r.yaml`)         | R Function (`.b.R`)                  |
| -------------------------------- | ------------------------------ | -------------------------------------- | ----------------------------------- | ------------------------------------ |
| **Table Configuration**          |                                |                                        |                                     |                                      |
| Variables (Rows)                 | `vars`                         | Variables (Rows)                       | `pivot_table`                       | `.run`, `.populate_basic_table`      |
| Grouping Variable (Columns)      | `group`                        | Grouping Variable (Columns)            | `pivot_table`                       | `.run`, `.populate_basic_table`      |
| **Options**                      |                                |                                        |                                     |                                      |
| Include Statistics               | `statistics`                   | Include Statistics                     | `summary_stats`                     | `.run`, `.generate_summary_stats`    |
| Show Totals                      | `show_totals`                  | Show Totals                            | `pivot_table`                       | `.populate_basic_table`              |
| Table Format                     | `format_style`                 | Table Format                           | `pivot_table`                       | `.get_table_styling`                 |
| Export to Excel                  | `export_excel`                 | Export to Excel                        | `export_info`                       | `.show_export_info`                  |
| **Results Sections**             |                                |                                        |                                     |                                      |
| Instructions                     | (N/A)                          | (N/A)                                  | `instructions`                      | `.init`                              |
| Pivot Cross Table                | (N/A)                          | (N/A)                                  | `pivot_table`                       | `.populate_basic_table`              |
| Summary Statistics               | (N/A)                          | (N/A)                                  | `summary_stats`                     | `.generate_summary_stats`            |
| Export Information               | (N/A)                          | (N/A)                                  | `export_info`                       | `.show_export_info`                  |
