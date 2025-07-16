# Parallel Coordinates Plot Analysis Documentation

This document provides a comprehensive overview of the Parallel Coordinates Plot Analysis module, detailing its features, user interface elements, and the underlying R functions.

## Feature Summary

This module creates parallel coordinates plots for multivariate data visualization. These plots are excellent for exploring relationships between multiple continuous variables and identifying patterns, clusters, and outliers in high-dimensional data. The implementation provides comprehensive functionality, including multiple scaling methods, grouping support, interactive Plotly integration, summary statistics, and missing data handling options.

## Feature Details

The following table provides a detailed mapping of the module's features, from the user interface to the underlying R functions.

| Feature                          | YAML Argument (`.a.yaml`)      | UI Label                               | Results Section (`.r.yaml`)         | R Function (`.b.R`)                  |
| -------------------------------- | ------------------------------ | -------------------------------------- | ----------------------------------- | ------------------------------------ |
| **Core Variables**               |                                |                                        |                                     |                                      |
| Variables                        | `vars`                         | Variables                              | `plot`, `plotly`, `summary`         | `.run`, `.plot`                      |
| Grouping Variable                | `group`                        | Grouping Variable                      | `plot`, `plotly`                    | `.run`, `.plot`                      |
| **Plot Customization**           |                                |                                        |                                     |                                      |
| Variable Scaling                 | `scaling`                      | Variable Scaling                       | `plot`                              | `.run`, `.plot`, `.scaleVariables`   |
| Line Transparency                | `alpha`                        | Line Transparency                      | `plot`                              | `.run`, `.plot`                      |
| Show Missing Values              | `showMissing`                  | Show Missing Values                    | `plot`                              | `.run`, `.plot`                      |
| Interactive Plot                 | `interactive`                  | Interactive Plot                       | `plotly`                            | `.run`, `.createInteractivePlot`     |
| Color Palette                    | `colorPalette`                 | Color Palette                          | `plot`                              | `.run`, `.plot`, `.getColorPalette`  |
| **Output Tables**                |                                |                                        |                                     |                                      |
| Variable Summary                 | `summary`                      | Variable Summary                       | `summary`                           | `.run`                               |
