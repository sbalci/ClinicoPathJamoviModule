# Bar Chart Analysis Documentation

This document provides a comprehensive overview of the Bar Chart Analysis module, detailing its features, user interface elements, and the underlying R functions.

## Feature Summary

The Bar Chart module provides a way to visualize and compare the distributions of categorical variables. It uses the `ggstatsplot` package to create bar charts that can be enriched with statistical tests of association.

The module's features can be broadly categorized as follows:

*   **Core Analysis:** Generation of bar charts for a dependent categorical variable, grouped by another categorical variable.
*   **Grouped Analysis:** Ability to split the bar charts by a third categorical variable to facilitate multi-level comparisons.
*   **Statistical Integration:** Inclusion of statistical tests (e.g., Chi-squared test) and pairwise comparisons directly on the plots.
*   **Customization:** Options to control the type of statistical test and p-value adjustments.

## Feature Details

The following table provides a detailed mapping of the module's features, from the user interface to the underlying R functions.

| Feature | YAML Argument (`.a.yaml`) | UI Label | Results Section (`.r.yaml`) | R Function (`.b.R`) |
|---|---|---|---|---|
| **Core Analysis** | | | | |
| Dependent Variable | `dep` | Dependent Variable | `plot`, `plot2` | `.plot`, `.plot2` |
| Grouping Variable | `group` | Grouping Variable | `plot`, `plot2` | `.plot`, `.plot2` |
| Split By | `grvar` | Split By (Optional) | `plot2` | `.plot2` |
| **Statistical Options** | | | | |
| Type of Statistic | `typestatistics` | Type of Statistic | (Argument to `ggbarstats`) | `.plot`, `.plot2` |
| Pairwise Comparisons | `pairwisecomparisons` | Pairwise Comparisons | (Argument to `ggbarstats`) | `.plot`, `.plot2` |
| Pairwise Display | `pairwisedisplay` | Pairwise Display | (Argument to `ggbarstats`) | `.plot`, `.plot2` |
| p-value Adjustment | `padjustmethod` | Adjustment Method | (Argument to `ggbarstats`) | `.plot`, `.plot2` |
| **Plot Customization** | | | | |
| Add GGStatsPlot Layer | `originaltheme` | Add GGStatsPlot Layer | (Theme logic) | `.plot`, `.plot2` |
| **Output** | | | | |
| To Do | `todo` | To Do | `todo` | `.run` |
| Bar Chart | `plot` | Bar Chart | `plot` | `.plot` |
| Grouped Bar Chart | `plot2` | `Bar Chart Splitted by {grvar}` | `plot2` | `.plot2` |
