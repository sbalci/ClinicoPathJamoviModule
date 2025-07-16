# Correlation Matrix Analysis Documentation

This document provides a comprehensive overview of the Correlation Matrix Analysis module, detailing its features, user interface elements, and the underlying R functions.

## Feature Summary

The Correlation Matrix module provides a visual representation of the correlations between multiple continuous variables. It uses the `ggstatsplot` package to generate a correlation matrix plot, which can be customized to show different types of correlation coefficients and can be grouped by a categorical variable.

The module's features can be broadly categorized as follows:

*   **Core Analysis:** Calculation and visualization of a correlation matrix for a set of continuous variables.
*   **Grouped Analysis:** Ability to generate separate correlation matrices for each level of a grouping variable.
*   **Statistical Options:** Selection of different correlation types (parametric, non-parametric, robust, or Bayesian).

## Feature Details

The following table provides a detailed mapping of the module's features, from the user interface to the underlying R functions.

| Feature | YAML Argument (`.a.yaml`) | UI Label | Results Section (`.r.yaml`) | R Function (`.b.R`) |
|---|---|---|---|---|
| **Core Analysis** | | | | |
| Dependent Variables | `dep` | Dependent Variables | `plot`, `plot2` | `.prepareOptions`, `.plot`, `.plot2` |
| Split By | `grvar` | Split By (Optional) | `plot2` | `.plot2` |
| **Statistical Options** | | | | |
| Type of Statistic | `typestatistics` | Type of Statistic | (Argument to `ggcorrmat`) | `.prepareOptions`, `.plot`, `.plot2` |
| **Output** | | | | |
| To Do | `todo` | To Do | `todo` | `.run` |
| Correlation Matrix | `plot` | Chart | `plot` | `.plot` |
| Grouped Correlation Matrix | `plot2` | Chart | `plot2` | `.plot2` |
