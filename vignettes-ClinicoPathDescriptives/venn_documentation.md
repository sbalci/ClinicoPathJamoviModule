# Venn Diagram Analysis Documentation

This document provides a comprehensive overview of the Venn Diagram Analysis module, detailing its features, user interface elements, and the underlying R functions.

## Feature Summary

The Venn Diagram Analysis module generates Venn diagrams and Upset plots to visualize overlaps and intersections between categorical variables. It allows users to define a "true" level for each variable, converting them into logical (boolean) values for analysis. The module supports up to four variables for Venn diagrams and provides advanced customization options for Upset plots using `ComplexUpset`.

The module's features can be broadly categorized as follows:

*   **Overlap Visualization:** Generates Venn diagrams for up to four variables and Upset plots for visualizing set intersections.
*   **Data Transformation:** Converts categorical variables into logical indicators based on a user-defined "true" level.
*   **Upset Plot Customization:** Offers a choice between `UpSetR` (classic) and `ComplexUpset` (advanced) for Upset plots, with options for sorting, minimum intersection size, and statistical annotations.
*   **Summary Statistics:** Provides a table summarizing the counts and percentages of the "true" level for each selected variable.

## Feature Details

The following table provides a detailed mapping of the module's features, from the user interface to the underlying R functions.

| Feature                          | YAML Argument (`.a.yaml`)      | UI Label                               | Results Section (`.r.yaml`)         | R Function (`.b.R`)                  |
| -------------------------------- | ------------------------------ | -------------------------------------- | ----------------------------------- | ------------------------------------ |
| **Overlap Visualization**        |                                |                                        |                                     |                                      |
| Variable 1 (Required)            | `var1`                         | Variable 1 (Required)                  | `plot`, `plot2`, `summary`          | `.run`, `.plot`, `.plot2`            |
| Select True Level (Var1)         | `var1true`                     | Select True Level                      | `plot`, `plot2`, `summary`          | `.run`                               |
| Variable 2 (Required)            | `var2`                         | Variable 2 (Required)                  | `plot`, `plot2`, `summary`          | `.run`, `.plot`, `.plot2`            |
| Select True Level (Var2)         | `var2true`                     | Select True Level                      | `plot`, `plot2`, `summary`          | `.run`                               |
| Variable 3 (Optional)            | `var3`                         | Variable 3 (Optional)                  | `plot`, `plot2`, `summary`          | `.run`, `.plot`, `.plot2`            |
| Select True Level (Var3)         | `var3true`                     | Select True Level                      | `plot`, `plot2`, `summary`          | `.run`                               |
| Variable 4 (Optional)            | `var4`                         | Variable 4 (Optional)                  | `plot`, `plot2`, `summary`          | `.run`, `.plot`, `.plot2`            |
| Select True Level (Var4)         | `var4true`                     | Select True Level                      | `plot`, `plot2`, `summary`          | `.run`                               |
| **Upset Plot Customization**     |                                |                                        |                                     |                                      |
| UpSet Plot Type                  | `upsetType`                    | UpSet Plot Type                        | `plot2`                             | `.plot2`                             |
| Sort Intersections By            | `sortBy`                       | Sort Intersections By                  | `plot2`                             | `.plot2`                             |
| Minimum Intersection Size        | `minSize`                      | Minimum Intersection Size              | `plot2`                             | `.plot2`                             |
| Show Statistical Annotations     | `showAnnotations`              | Show Statistical Annotations           | `plot2`                             | `.plot2`                             |
| **Summary Statistics**           |                                |                                        |                                     |                                      |
| Summary of True Counts Table     | `summary`                      | Summary of True Counts                 | `summary`                           | `.run`                               |
