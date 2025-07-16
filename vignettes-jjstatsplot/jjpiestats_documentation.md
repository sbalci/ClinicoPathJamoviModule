# Pie Chart Analysis Documentation

This document provides a comprehensive overview of the Pie Chart Analysis module, detailing its features, user interface elements, and the underlying R functions.

## Feature Summary

The Pie Chart module provides a straightforward way to visualize categorical data using pie charts. It leverages the `ggstatsplot` package to not only create the visualizations but also to include statistical tests of proportions and associations directly on the plot.

The module's features can be broadly categorized as follows:

*   **Core Analysis:** Generation of single or grouped pie charts.
*   **Statistical Integration:** Inclusion of statistical tests (e.g., Chi-squared test) as subtitles on the plots.
*   **Customization:** Options to control the appearance of the plots, including themes and statistical details.

## Feature Details

The following table provides a detailed mapping of the module's features, from the user interface to the underlying R functions.

| Feature | YAML Argument (`.a.yaml`) | UI Label | Results Section (`.r.yaml`) | R Function (`.b.R`) |
|---|---|---|---|---|
| **Core Analysis** | | | | |
| Dependent Variable | `dep` | Dependent Variable | `plot1`, `plot2`, `plot4` | `.plot1`, `.plot2`, `.plot4` |
| Grouping Variable | `group` | Grouping Variable: (Optional) | `plot2`, `plot4` | `.plot2`, `.plot4` |
| Split By | `grvar` | Split By (Optional) | `plot4` | `.plot4` |
| **Statistical Options** | | | | |
| Type of Statistic | `typestatistics` | Type of Statistic | (Argument to `ggpiestats`) | `.prepareOptions` |
| Statistical Results | `resultssubtitle` | Statistical Results | (Argument to `ggpiestats`) | `.plot1`, `.plot2` |
| **Plot Customization** | | | | |
| Add GGStatsPlot Layer | `originaltheme` | Add GGStatsPlot Layer | `plot1`, `plot2`, `plot4` | `.plot1`, `.plot2`, `.plot4` |
| **Output** | | | | |
| To Do | `todo` | To Do | `todo` | `.run` |
| Single Pie Chart | `plot1` | `${dep}` | `plot1` | `.plot1` |
| Grouped Pie Chart | `plot2` | `${group} - {dep}` | `plot2` | `.plot2` |
| Split Pie Charts | `plot4` | `${group} - {dep} by {grvar}` | `plot4` | `.plot4` |
