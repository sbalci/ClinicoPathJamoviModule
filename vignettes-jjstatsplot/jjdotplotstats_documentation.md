# Dot Plot Analysis Documentation

This document provides a comprehensive overview of the Dot Plot Analysis module, detailing its features, user interface elements, and the underlying R functions.

## Feature Summary

The Dot Plot module offers a way to visualize the distribution of a continuous variable across different categories. It utilizes the `ggstatsplot` package to create dot plots that are not only visually informative but also include statistical comparisons between groups.

The module's features can be broadly categorized as follows:

*   **Core Analysis:** Generation of dot plots to compare a continuous variable across a categorical group.
*   **Grouped Analysis:** Ability to create separate dot plots for each level of an additional grouping variable.
*   **Statistical Integration:** Direct inclusion of statistical tests (e.g., t-test, ANOVA) and effect sizes on the plot.
*   **Customization:** Options to control the type of statistical test, effect size, and the display of centrality measures.

## Feature Details

The following table provides a detailed mapping of the module's features, from the user interface to the underlying R functions.

| Feature | YAML Argument (`.a.yaml`) | UI Label | Results Section (`.r.yaml`) | R Function (`.b.R`) |
|---|---|---|---|---|
| **Core Analysis** | | | | |
| Dependent Variable | `dep` | Dependent Variable | `plot`, `plot2` | `.prepareData`, `.plot`, `.plot2` |
| Grouping Variable | `group` | Grouping Variable | `plot`, `plot2` | `.prepareData`, `.plot`, `.plot2` |
| Split By | `grvar` | Split By (Optional) | `plot2` | `.plot2` |
| **Statistical Options** | | | | |
| Type of Statistic | `typestatistics` | Type of Statistic | (Argument to `ggdotplotstats`) | `.prepareOptions`, `.plot`, `.plot2` |
| Effect Size Type | `effsizetype` | Effect Size Needed for Parametric Tests | (Argument to `ggdotplotstats`) | `.prepareOptions`, `.plot`, `.plot2` |
| **Plot Customization** | | | | |
| Show Centrality | `centralityplotting` | Show Centrality | (Argument to `ggdotplotstats`) | `.prepareOptions`, `.plot`, `.plot2` |
| Centrality Type | `centralitytype` | Centrality Type | (Argument to `ggdotplotstats`) | `.prepareOptions`, `.plot`, `.plot2` |
| Title | `mytitle` | Title | (Argument to `ggdotplotstats`) | `.prepareOptions`, `.plot` |
| X-Title | `xtitle` | X-Title | (Argument to `ggdotplotstats`) | `.prepareOptions`, `.plot` |
| Y-Title | `ytitle` | Y-Title | (Argument to `ggdotplotstats`) | `.prepareOptions`, `.plot` |
| Add GGStatsPlot Layer | `originaltheme` | Add GGStatsPlot Layer | `plot`, `plot2` | `.plot`, `.plot2` |
| Statistical Results | `resultssubtitle` | Statistical Results | (Argument to `ggdotplotstats`) | `.plot`, `.plot2` |
| **Output** | | | | |
| To Do | `todo` | To Do | `todo` | `.run` |
| Dot Plot | `plot` | `${group} - {dep}` | `plot` | `.plot` |
| Split Dot Plot | `plot2` | `${group} - {dep} by {grvar}` | `plot2` | `.plot2` |
