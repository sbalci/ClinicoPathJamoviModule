# Line Chart for Time Series and Trend Analysis Documentation

This document provides a comprehensive overview of the Line Chart for Time Series and Trend Analysis module, detailing its features, user interface elements, and the underlying R functions.

## Feature Summary

This module creates comprehensive line charts for time series analysis and trend visualization in clinical and pathological research. It supports multiple groups, confidence intervals, trend lines, and statistical overlays, making it ideal for analyzing longitudinal data, treatment responses, and biomarker trends over time. The module offers extensive customization options and statistical features to create publication-ready plots for clinical studies.

## Feature Details

The following table provides a detailed mapping of the module's features, from the user interface to the underlying R functions.

| Feature                          | YAML Argument (`.a.yaml`)      | UI Label                               | Results Section (`.r.yaml`)         | R Function (`.b.R`)                  |
| -------------------------------- | ------------------------------ | -------------------------------------- | ----------------------------------- | ------------------------------------ |
| **Core Variables**               |                                |                                        |                                     |                                      |
| X-axis Variable                  | `xvar`                         | X-axis Variable                        | `summary`, `correlation`, `plot`    | `.run`, `.cleanData`, `.calculateSummary`, `.calculateCorrelation`, `.savePlotData` |
| Y-axis Variable                  | `yvar`                         | Y-axis Variable                        | `summary`, `correlation`, `plot`    | `.run`, `.cleanData`, `.calculateSummary`, `.calculateCorrelation`, `.savePlotData` |
| Group By                         | `groupby`                      | Group By                               | `summary`, `plot`                   | `.run`, `.cleanData`, `.calculateSummary`, `.plot` |
| **Plot Customization**           |                                |                                        |                                     |                                      |
| Confidence Interval              | `confidence`                   | Confidence Interval                    | `plot`                              | `.plot`                              |
| Trend Line                       | `trendline`                    | Trend Line                             | `correlation`, `plot`               | `.run`, `.calculateCorrelation`, `.plot` |
| Show Points                      | `points`                       | Show Points                            | `plot`                              | `.plot`                              |
| Smooth Line                      | `smooth`                       | Smooth Line                            | `plot`                              | `.plot`                              |
| Reference Line Value             | `refline`                      | Reference Line Value                   | `plot`                              | `.plot`                              |
| Reference Line Label             | `reflineLabel`                 | Reference Line Label                   | `plot`                              | `.plot`                              |
| Color Palette                    | `colorPalette`                 | Color Palette                          | `plot`                              | `.plot`, `.getColorPalette`          |
| Plot Theme                       | `theme`                        | Plot Theme                             | `plot`                              | `.plot`, `.getPlotTheme`             |
| X-axis Label                     | `xlabel`                       | X-axis Label                           | `plot`                              | `.plot`                              |
| Y-axis Label                     | `ylabel`                       | Y-axis Label                           | `plot`                              | `.plot`                              |
| Plot Title                       | `title`                        | Plot Title                             | `plot`                              | `.plot`                              |
| Plot Width                       | `width`                        | Plot Width                             | `plot`                              | Not implemented in `.b.R`            |
| Plot Height                      | `height`                       | Plot Height                            | `plot`                              | Not implemented in `.b.R`            |
| **Output Tables**                |                                |                                        |                                     |                                      |
| Data Summary Table               | `summary`                      | Data Summary                           | `summary`                           | `.populateSummary`                   |
| Correlation Analysis Table       | `correlation`                  | Correlation Analysis                   | `correlation`                       | `.populateCorrelation`               |
