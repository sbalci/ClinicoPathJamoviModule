# Histogram Analysis Documentation

This document provides a comprehensive overview of the Histogram Analysis module, detailing its features, user interface elements, and the underlying R functions.

## Feature Summary

The Histogram module provides a powerful and flexible way to visualize the distribution of continuous variables. It uses the `ggstatsplot` package to create publication-ready histograms that can be enriched with statistical details, such as normality tests and measures of central tendency.

The module's features can be broadly categorized as follows:

*   **Core Analysis:** Generation of histograms for one or more continuous variables.
*   **Grouped Analysis:** Ability to split histograms by a categorical variable to compare distributions across groups.
*   **Statistical Integration:** Overlaying statistical information, such as normality tests and centrality lines, directly onto the plots.
*   **Customization:** Options to control the appearance of the histograms, including bin width and the display of a normal curve.

## Feature Details

The following table provides a detailed mapping of the module's features, from the user interface to the underlying R functions.

| Feature | YAML Argument (`.a.yaml`) | UI Label | Results Section (`.r.yaml`) | R Function (`.b.R`) |
|---|---|---|---|---|
| **Core Analysis** | | | | |
| Variables | `dep` | Variables | `plot`, `plot2` | `.prepareData`, `.plot`, `.plot2` |
| Split By | `grvar` | Split By (Optional) | `plot2` | `.plot2` |
| **Statistical Options** | | | | |
| Type of Statistic | `typestatistics` | Type of Statistic | (Argument to `gghistostats`) | `.prepareOptions`, `.plot`, `.plot2` |
| Statistical Results | `resultssubtitle` | Statistical Results | (Argument to `gghistostats`) | `.prepareOptions`, `.plot`, `.plot2` |
| **Plot Customization** | | | | |
| Centrality Line | `centralityline` | Centrality Line | (Argument to `gghistostats`) | `.prepareOptions`, `.plot`, `.plot2` |
| Normal Curve | `normalcurve` | Normal Curve | (Argument to `gghistostats`) | `.prepareOptions`, `.plot`, `.plot2` |
| Change Bin Width | `changebinwidth` | Change Bin Width | (Controls `binwidth`) | `.prepareOptions` |
| Bin Width | `binwidth` | Bin Width | (Argument to `gghistostats`) | `.prepareOptions`, `.plot`, `.plot2` |
| **Output** | | | | |
| To Do | `todo` | To Do | `todo` | `.run` |
| Histogram | `plot` | Histogram | `plot` | `.plot` |
| Split Histogram | `plot2` | `Histogram Splitted by {grvar}` | `plot2` | `.plot2` |
