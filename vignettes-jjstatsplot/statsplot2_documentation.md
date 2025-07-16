# Automatic Plot Selection Analysis Documentation

This document provides a comprehensive overview of the Automatic Plot Selection module, detailing its features, user interface elements, and the underlying R functions.

## Feature Summary

This module intelligently selects and generates the most appropriate statistical visualization based on the data types of the selected variables. It supports both independent and repeated measurements designs, offering a wide array of plot types including violin plots, scatter plots, bar charts, dot plots, and alluvial diagrams. It leverages `ggstatsplot` for statistical visualizations and `ggalluvial`/`easyalluvial` for flow diagrams, ensuring statistically sound and visually appealing outputs.

## Feature Details

The following table provides a detailed mapping of the module's features, from the user interface to the underlying R functions.

| Feature                          | YAML Argument (`.a.yaml`)      | UI Label                               | Results Section (`.r.yaml`)         | R Function (`.b.R`)                  |
| -------------------------------- | ------------------------------ | -------------------------------------- | ----------------------------------- | ------------------------------------ |
| **Core Plotting Variables**      |                                |                                        |                                     |                                      |
| Dependent Variable (y-axis)      | `dep`                          | Dependent Variable (y-axis)            | `plot`, `text4`                     | `.run`, `.plot`                      |
| Grouping Variable (x-axis)       | `group`                        | Grouping Variable (x-axis)             | `plot`, `text4`                     | `.run`, `.plot`                      |
| Split By (Optional)              | `grvar`                        | Split By (Optional)                    | `plot`                              | `.run`, `.plot`                      |
| Study Design                     | `direction`                    | Study Design                           | `plot`, `text4`                     | `.run`, `.plot`                      |
| Statistical Approach             | `distribution`                 | Statistical Approach                   | `plot`, `text4`                     | `.run`, `.plot`                      |
| Alluvial Plot Style              | `alluvsty`                     | Alluvial Plot Style                    | `plot`                              | `.run`, `.plot`                      |
| Exclude Missing Values           | `excl`                         | Exclude Missing Values                 | `plot`                              | `.run`, `.plot`                      |
| Use Original Themes              | `originaltheme`                | Use Original Themes                    | `plot`                              | `.plot`                              |
