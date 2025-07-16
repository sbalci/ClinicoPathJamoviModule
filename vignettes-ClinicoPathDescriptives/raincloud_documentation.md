# Raincloud Plot Analysis Documentation

This document provides a comprehensive overview of the Raincloud Plot Analysis module, detailing its features, user interface elements, and the underlying R functions.

## Feature Summary

This module creates Raincloud plots to visualize data distributions using the `ggdist` package. Raincloud plots combine three visualization techniques: half-violin plots showing distribution density, box plots showing summary statistics, and dot plots showing individual data points. This provides a comprehensive view of data distribution that reveals patterns traditional box plots might miss, including multimodality and distribution shape. The module also offers options for statistical analysis, outlier detection, normality testing, and group comparisons.

## Feature Details

The following table provides a detailed mapping of the module's features, from the user interface to the underlying R functions.

| Feature                          | YAML Argument (`.a.yaml`)      | UI Label                               | Results Section (`.r.yaml`)         | R Function (`.b.R`)                  |
| -------------------------------- | ------------------------------ | -------------------------------------- | ----------------------------------- | ------------------------------------ |
| **Core Plotting Variables**      |                                |                                        |                                     |                                      |
| Dependent Variable               | `dep_var`                      | Dependent Variable                     | `plot`, `statistics`, `outliers`, `normality`, `comparison`, `interpretation` | `.run`, `.plot`, `.generate_statistics`, `.generate_outlier_analysis`, `.generate_normality_tests`, `.generate_group_comparisons`, `.generate_interpretation_guide` |
| Grouping Variable                | `group_var`                    | Grouping Variable                      | `plot`, `statistics`, `outliers`, `normality`, `comparison`, `interpretation` | `.run`, `.plot`, `.generate_statistics`, `.generate_outlier_analysis`, `.generate_normality_tests`, `.generate_group_comparisons`, `.generate_interpretation_guide` |
| Faceting Variable (Optional)     | `facet_var`                    | Faceting Variable (Optional)           | `plot`                              | `.run`, `.plot`                      |
| Color Variable (Optional)        | `color_var`                    | Color Variable (Optional)              | `plot`                              | `.run`, `.plot`                      |
| **Plot Components**              |                                |                                        |                                     |                                      |
| Show Half-Violin (Density)       | `show_violin`                  | Show Half-Violin (Density)             | `plot`                              | `.plot`                              |
| Show Box Plot                    | `show_boxplot`                 | Show Box Plot                          | `plot`                              | `.plot`                              |
| Show Data Points                 | `show_dots`                    | Show Data Points                       | `plot`                              | `.plot`                              |
| Dots Position                    | `dots_side`                    | Dots Position                          | `plot`                              | `.plot`                              |
| Violin Width                     | `violin_width`                 | Violin Width                           | `plot`                              | `.plot`                              |
| Box Plot Width                   | `box_width`                    | Box Plot Width                         | `plot`                              | `.plot`                              |
| Dots Size                        | `dots_size`                    | Dots Size                              | `plot`                              | `.plot`                              |
| Violin Transparency              | `alpha_violin`                 | Violin Transparency                    | `plot`                              | `.plot`                              |
| Dots Transparency                | `alpha_dots`                   | Dots Transparency                      | `plot`                              | `.plot`                              |
| Plot Orientation                 | `orientation`                  | Plot Orientation                       | `plot`                              | `.plot`                              |
| Color Palette                    | `color_palette`                | Color Palette                          | `plot`                              | `.plot`, `.get_color_palette`        |
| Plot Theme                       | `plot_theme`                   | Plot Theme                             | `plot`                              | `.plot`, `.get_plot_theme`           |
| Plot Title                       | `plot_title`                   | Plot Title                             | `plot`                              | `.plot`                              |
| X-Axis Label                     | `x_label`                      | X-Axis Label                           | `plot`                              | `.plot`                              |
| Y-Axis Label                     | `y_label`                      | Y-Axis Label                           | `plot`                              | `.plot`                              |
| **Statistical Analysis**         |                                |                                        |                                     |                                      |
| Show Summary Statistics          | `show_statistics`              | Show Summary Statistics                | `statistics`                        | `.generate_statistics`               |
| Highlight Outliers               | `show_outliers`                | Highlight Outliers                     | `outliers`                          | `.generate_outlier_analysis`         |
| Outlier Detection Method         | `outlier_method`               | Outlier Detection Method               | `outliers`                          | `.generate_outlier_analysis`         |
| Test for Normality               | `normality_test`               | Test for Normality                     | `normality`                         | `.generate_normality_tests`          |
| Group Comparison Test            | `comparison_test`              | Group Comparison Test                  | `comparison`                        | `.generate_group_comparisons`        |
| Comparison Method                | `comparison_method`            | Comparison Method                      | `comparison`                        | `.generate_group_comparisons`        |
| **Interpretation**               |                                |                                        |                                     |                                      |
| Interpretation Guide             | `interpretation`               | Interpretation Guide                   | `interpretation`                    | `.generate_interpretation_guide`     |
