# Scatter Plot (jjscatterstats) Documentation

This document provides a comprehensive overview of the `jjscatterstats` module, detailing its features, user interface elements, and the underlying R functions. This module is designed to generate scatter plots for visualizing relationships between continuous variables.

## Feature Summary

The `jjscatterstats` module is a versatile tool for exploring the relationship between two continuous variables, with an option to split the analysis by a categorical grouping variable. It leverages the `ggplot2` and `ggstatsplot` packages to provide flexible plotting and statistical analysis capabilities, including different types of statistical tests (parametric, nonparametric, robust, Bayes).

The module's features can be broadly categorized as follows:

*   **Data Input:** Specifies x-axis and y-axis variables, with an optional grouping variable for split analysis.
*   **Statistical Analysis:** Supports various statistical types for correlation analysis.
*   **Plot Customization:** Allows custom titles and axis labels, and integration with `ggstatsplot`'s default theme or custom `ggplot2` themes.
*   **Grouped Analysis:** Provides an option to generate separate plots for each level of a grouping variable.

## Feature Details

The following table provides a detailed mapping of the module's features, from the user interface to the underlying R functions.

| Feature                          | YAML Argument (`.a.yaml`)      | UI Label                               | Results Section (`.r.yaml`)         | R Function (`.b.R`)                  |
| :------------------------------- | :----------------------------- | :------------------------------------- | :---------------------------------- | :----------------------------------- |
| **Data Input**                   |                                |                                        |                                     |                                      |
| X-axis Variable                  | `dep`                          | x-axis                                 | `plot`, `plot2`                     | `.run`, `.plot`, `.plot2`            |
| Y-axis Variable                  | `group`                        | y-axis                                 | `plot`, `plot2`                     | `.run`, `.plot`, `.plot2`            |
| Split By (Optional)              | `grvar`                        | Split By (Optional)                    | `plot2`                             | `.init`, `.plot2`                    |
| **Statistical Analysis**         |                                |                                        |                                     |                                      |
| Type of Statistic                | `typestatistics`               | Type of Statistic                      | `plot`, `plot2`                     | `.plot`, `.plot2`                    |
| Statistical Results              | `resultssubtitle`              | Statistical Results                    | `plot`, `plot2`                     | `.plot`, `.plot2`                    |
| **Plot Customization**           |                                |                                        |                                     |                                      |
| Title                            | `mytitle`                      | Title                                  | `plot`                              | `.plot`                              |
| X-Title                          | `xtitle`                       | X-Title                                | `plot`                              | `.plot`                              |
| Y-Title                          | `ytitle`                       | Y-Title                                | `plot`                              | `.plot`                              |
| Add GGStatsPlot Layer            | `originaltheme`                | Add GGStatsPlot Layer                  | `plot`, `plot2`                     | `.plot`, `.plot2`                    |
