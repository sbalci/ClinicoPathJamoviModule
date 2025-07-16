# Between-Groups Comparison Analysis Documentation

This document provides a comprehensive overview of the Between-Groups Comparison module, which uses box-violin plots to visualize and test differences between groups. It details the module's features, user interface elements, and the underlying R functions.

## Feature Summary

The Between-Groups Comparison module is a powerful tool for visualizing and analyzing differences in a continuous variable across various groups. It leverages the `ggstatsplot` package to create informative and publication-ready plots that combine data visualization (violin plots, box plots, individual data points) with statistical test results (e.g., ANOVA, Kruskal-Wallis) and pairwise comparisons.

The module's features can be broadly categorized as follows:

*   **Core Analysis:** Generation of plots for comparing a continuous dependent variable across a categorical grouping variable.
*   **Grouped (Faceted) Analysis:** Ability to split the main comparison by a second categorical variable, creating a panel of plots.
*   **Comprehensive Statistical Options:** A wide range of choices for statistical tests (parametric, non-parametric, robust, Bayesian), effect sizes, and p-value adjustments for multiple comparisons.
*   **Flexible Plot Customization:** Detailed control over the plot's appearance, including the display of centrality measures, individual plot elements (violin, box, points), titles, and themes.

## Feature Details

The following table provides a detailed mapping of the module's features, from the user interface to the underlying R functions.

| Feature | YAML Argument (`.a.yaml`) | UI Label | Results Section (`.r.yaml`) | R Function (`.b.R`) |
|---|---|---|---|---|
| **Core Analysis** | | | | |
| Dependent Variables | `dep` | Dependent Variables | `plot`, `plot2` | `.prepareData`, `.plot`, `.plot2` |
| Grouping Variable | `group` | Grouping Variable | `plot`, `plot2` | `.plot`, `.plot2` |
| Split By | `grvar` | Split By (Optional) | `plot2` | `.plot2` |
| **Statistical Options** | | | | |
| Type of Statistic | `typestatistics` | Type of Statistic | (Argument) | `.prepareOptions`, `.plot`, `.plot2` |
| Pairwise Comparisons | `pairwisecomparisons` | Pairwise Comparisons | (Argument) | `.prepareOptions`, `.plot`, `.plot2` |
| Pairwise Display | `pairwisedisplay` | Pairwise Display | (Argument) | `.prepareOptions`, `.plot`, `.plot2` |
| p-value Adjustment | `padjustmethod` | Adjustment Method | (Argument) | `.prepareOptions`, `.plot`, `.plot2` |
| Effect Size Type | `effsizetype` | Effect Size Needed for Parametric Tests | (Argument) | `.prepareOptions`, `.plot`, `.plot2` |
| **Plot Customization** | | | | |
| Show Centrality | `centralityplotting` | Show Centrality | (Argument) | `.prepareOptions`, `.plot`, `.plot2` |
| Centrality Type | `centralitytype` | Centrality Type | (Argument) | `.prepareOptions`, `.plot`, `.plot2` |
| Violin Plot | `violin` | Violin Plot | (Argument) | `.prepareOptions`, `.plot`, `.plot2` |
| Box Plot | `boxplot` | Box Plot | (Argument) | `.prepareOptions`, `.plot`, `.plot2` |
| Points | `point` | Points | (Argument) | `.prepareOptions`, `.plot`, `.plot2` |
| Title | `mytitle` | Title | (Argument) | `.prepareOptions`, `.plot` |
| X-Title | `xtitle` | X-Title | (Argument) | `.prepareOptions`, `.plot` |
| Y-Title | `ytitle` | Y-Title | (Argument) | `.prepareOptions`, `.plot` |
| Add GGStatsPlot Layer | `originaltheme` | Add GGStatsPlot Layer | (Theme logic) | `.plot`, `.plot2` |
| Statistical Results | `resultssubtitle` | Statistical Results | (Argument) | `.prepareOptions`, `.plot`, `.plot2` |
| **Output** | | | | |
| To Do | `todo` | To Do | `todo` | `.run` |
| Violin Plot | `plot` | Violin Plot | `plot` | `.plot` |
| Grouped Violin Plot | `plot2` | `Violin Plot by ${grvar}` | `plot2` | `.plot2` |
