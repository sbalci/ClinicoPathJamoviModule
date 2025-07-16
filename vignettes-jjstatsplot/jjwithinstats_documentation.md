# Within-Group Comparison (jjwithinstats) Documentation

This document provides a comprehensive overview of the `jjwithinstats` module, detailing its features, user interface elements, and the underlying R functions. This module is designed to generate Violin Plots for repeated measurements, facilitating within-group comparisons.

## Feature Summary

The `jjwithinstats` module is a powerful tool for visualizing and statistically comparing repeated continuous measurements within the same subjects or groups. It leverages the `ggplot2` and `ggstatsplot` packages to provide flexible plotting and statistical analysis capabilities.

The module's features can be broadly categorized as follows:

*   **Data Input:** Handles multiple dependent variables representing repeated measurements.
*   **Plot Customization:** Offers extensive options for customizing the appearance of Violin, Box, and Point plots, including centrality indicators and paths.
*   **Statistical Analysis:** Supports various statistical types (parametric, nonparametric, robust, Bayes) for pairwise comparisons with different adjustment methods.
*   **Title and Labeling:** Allows custom titles and axis labels for clear presentation.
*   **Theme Integration:** Provides options to integrate with `ggstatsplot`'s default theme or custom `ggplot2` themes.

## Feature Details

The following table provides a detailed mapping of the module's features, from the user interface to the underlying R functions.

| Feature                          | YAML Argument (`.a.yaml`)      | UI Label                               | Results Section (`.r.yaml`)         | R Function (`.b.R`)                  |
| :------------------------------- | :----------------------------- | :------------------------------------- | :---------------------------------- | :----------------------------------- |
| **Data Input**                   |                                |                                        |                                     |                                      |
| First Measurement                | `dep1`                         | First Measurement                      | `plot`                              | `.run`, `.plot`                      |
| Second Measurement               | `dep2`                         | Second Measurement                     | `plot`                              | `.run`, `.plot`                      |
| Third Measurement (Optional)     | `dep3`                         | Third Measurement (Optional)           | `plot`                              | `.run`, `.plot`                      |
| Fourth Measurement (Optional)    | `dep4`                         | Fourth Measurement (Optional)          | `plot`                              | `.run`, `.plot`                      |
| **Plot Customization**           |                                |                                        |                                     |                                      |
| Show Point Path                  | `pointpath`                    | Show Point Path                        | `plot`                              | `.plot`                              |
| Centrality Path                  | `centralitypath`               | Centrality Path                        | `plot`                              | `.plot`                              |
| Show Centrality                  | `centralityplotting`           | Show Centrality                        | `plot`                              | `.plot`                              |
| Centrality Type                  | `centralitytype`               | Centrality Type                        | `plot`                              | `.plot`                              |
| Violin Plot                      | `violin`                       | Violin Plot                            | `plot`                              | `.plot`                              |
| Box Plot                         | `boxplot`                      | Box Plot                               | `plot`                              | `.plot`                              |
| Points                           | `point`                        | Points                                 | `plot`                              | `.plot`                              |
| **Statistical Analysis**         |                                |                                        |                                     |                                      |
| Type of Statistic                | `typestatistics`               | Type of Statistic                      | `plot`                              | `.plot`                              |
| Pairwise Comparisons             | `pairwisecomparisons`          | Pairwise Comparisons                   | `plot`                              | `.plot`                              |
| Pairwise Display                 | `pairwisedisplay`              | Pairwise Display                       | `plot`                              | `.plot`                              |
| Adjustment Method                | `padjustmethod`                | Adjustment Method                      | `plot`                              | `.plot`                              |
| Effect Size Needed for Parametric Tests | `effsizetype`                  | Effect Size Needed for Parametric Tests | `plot`                              | `.plot`                              |
| Statistical Results              | `resultssubtitle`              | Statistical Results                    | `plot`                              | `.plot`                              |
| **Title and Labeling**           |                                |                                        |                                     |                                      |
| Title                            | `mytitle`                      | Title                                  | `plot`                              | `.plot`                              |
| X-Title                          | `xtitle`                       | X-Title                                | `plot`                              | `.plot`                              |
| Y-Title                          | `ytitle`                       | Y-Title                                | `plot`                              | `.plot`                              |
| **Theme Integration**            |                                |                                        |                                     |                                      |
| Add GGStatsPlot Layer            | `originaltheme`                | Add GGStatsPlot Layer                  | `plot`                              | `.plot`                              |