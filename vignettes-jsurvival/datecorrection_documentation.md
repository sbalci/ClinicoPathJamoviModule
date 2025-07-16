# Date Field Correction Documentation

This document provides a comprehensive overview of the Date Field Correction module, detailing its features, user interface elements, and the underlying R functions.

## Feature Summary

The Date Field Correction module is a powerful tool for standardizing and correcting messy date formats commonly found in clinical databases. It leverages multiple R packages (datefixR, anytime, lubridate) to provide robust date parsing, imputation of missing components, and handling of various date representations, including Excel numeric dates.

The module's features can be broadly categorized as follows:

*   **Core Correction Functionality:** Handles various date formats, missing components, and Excel numeric dates.
*   **Correction Methods:** Offers multiple methods (datefixR, anytime, lubridate, consensus) for flexible and robust correction.
*   **Quality Assessment:** Provides metrics on correction success rates and identifies common problems.
*   **Format Analysis:** Analyzes detected date formats and patterns in the original data.
*   **Detailed Reporting:** Generates tables and summaries of the correction process.
*   **Usage Guidance:** Offers interpretation and best practices for date correction in clinical research.

## Feature Details

The following table provides a detailed mapping of the module's features, from the user interface to the underlying R functions.

| Feature                          | YAML Argument (`.a.yaml`)      | UI Label                               | Results Section (`.r.yaml`)         | R Function (`.b.R`)                  |
| -------------------------------- | ------------------------------ | -------------------------------------- | ----------------------------------- | ------------------------------------ |
| **Core Correction**              |                                |                                        |                                     |                                      |
| Date Variables                   | `date_vars`                    | Date Variables                         | (Output new columns)                | `.perform_date_correction`           |
| Correction Method                | `correction_method`            | Correction Method                      | (Controls correction logic)         | `.perform_date_correction`           |
| Date Format                      | `date_format`                  | Date Format                            | (Influences parsing)                | `.correct_with_lubridate`            |
| Day Imputation                   | `day_impute`                   | Day of Month to Impute                 | (Influences parsing)                | `.correct_with_datefixr`             |
| Month Imputation                 | `month_impute`                 | Month to Impute                        | (Influences parsing)                | `.correct_with_datefixr`             |
| Handle Excel Dates               | `handle_excel`                 | Convert Excel Numeric Dates            | (Influences parsing)                | `.correct_with_datefixr`             |
| Timezone                         | `timezone`                     | Timezone for Output Dates              | (Influences parsing)                | `.correct_with_anytime`              |
| **Output Options**               |                                |                                        |                                     |                                      |
| Show Correction Table            | `show_correction_table`        | Show Detailed Correction Table         | `correction_table`                  | `.generate_correction_table`         |
| Show Quality Assessment          | `show_quality_assessment`      | Show Quality Assessment                | `quality_assessment`                | `.generate_quality_assessment`       |
| Show Format Analysis             | `show_format_analysis`         | Show Format Analysis                   | `format_analysis`                   | `.generate_format_analysis`          |
| Show Correction Summary          | `show_correction_summary`      | Show Correction Summary                | `correction_summary`                | `.generate_correction_summary`       |
| Show Interpretation Guide        | `show_interpretation`          | Show Usage Guide and Interpretation    | `interpretation`                    | `.generate_interpretation_guide`     |
