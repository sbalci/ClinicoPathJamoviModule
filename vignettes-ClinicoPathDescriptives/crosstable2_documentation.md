# Cross Table Analysis Documentation

This document provides a comprehensive overview of the Cross Table module, detailing its features, user interface elements, and the underlying R functions.

## Feature Summary

The Cross Table module is a powerful tool for generating descriptive tables (Table 1) for clinicopathological comparisons. It allows users to compare multiple variables against a grouping variable, automatically selecting appropriate hypothesis tests and rendering tables in various publication-ready styles.

The module's features can be broadly categorized as follows:

*   **Core Table Generation:** Creates cross tables with selected variables as rows and a grouping variable as columns.
*   **Statistical Testing:** Automatically performs appropriate statistical tests (e.g., chi-square, Fisher's exact, ANOVA, t-test, Kruskal-Wallis) based on variable types.
*   **Customizable Styles:** Supports multiple output styles including `arsenal`, `finalfit`, `gtsummary`, `nejm`, `lancet`, and `hmisc`.
*   **Missing Value Handling:** Option to exclude rows with missing values.
*   **Continuous Variable Summaries:** Allows selection of mean or median for continuous variable summaries.

## Feature Details

The following table provides a detailed mapping of the module's features, from the user interface to the underlying R functions.

| Feature                          | YAML Argument (`.a.yaml`)      | UI Label                               | Results Section (`.r.yaml`)         | R Function (`.b.R`)                  |
| -------------------------------- | ------------------------------ | -------------------------------------- | ----------------------------------- | ------------------------------------ |
| **Core Analysis**                |                                |                                        |                                     |                                      |
| Variables                        | `vars`                         | Variables (Rows)                       | `tablestyle1`, `tablestyle2`, etc.  | `.run`                               |
| Grouping Variable                | `group`                        | Grouping Variable (Columns)            | `tablestyle1`, `tablestyle2`, etc.  | `.run`                               |
| Table Style                      | `sty`                          | Table Style                            | `tablestyle1`, `tablestyle2`, etc.  | `.run`                               |
| Exclude Missing Values           | `excl`                         | Exclude Missing Values                 | (Data preprocessing)                | `.run`                               |
| Continuous Variable Summary      | `cont`                         | Continuous Variable Summary            | `tablestyle2` (finalfit)            | `.run`                               |
| Categorical P-value Test         | `pcat`                         | Categorical P-value Test               | `tablestyle2` (finalfit)            | `.run`                               |
