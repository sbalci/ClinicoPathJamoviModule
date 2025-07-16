# Base Graphics Visualization Documentation

This document provides a comprehensive overview of the Base Graphics Visualization module, detailing its features, user interface elements, and the underlying R functions.

## Feature Summary

The Base Graphics Visualization module offers a powerful and highly customizable solution for creating various plots using pure base R graphics. It is designed for rapid data exploration and visualization, providing a wide range of plot types and extensive customization options without external dependencies.

The module's features can be broadly categorized as follows:

*   **Diverse Plot Types:** Supports scatter, line, histogram, boxplot, barplot, density, pairs, and matplot.
*   **Flexible Variable Assignment:** Allows selection of X, Y, and optional grouping variables.
*   **Extensive Customization:** Provides options for main title, axis labels, point type and size, color schemes, grid lines, and legends.
*   **Axis Limit Control:** Enables users to set custom minimum and maximum values for X and Y axes.
*   **Statistical Overlays:** Option to display basic statistics directly on the plot for certain plot types.

## Feature Details

The following table provides a detailed mapping of the module's features, from the user interface to the underlying R functions.

| Feature                          | YAML Argument (`.a.yaml`)      | UI Label                               | Results Section (`.r.yaml`)         | R Function (`.b.R`)                  |
| -------------------------------- | ------------------------------ | -------------------------------------- | ----------------------------------- | ------------------------------------ |
| **Core Plot Setup**              |                                |                                        |                                     |                                      |
| Plot Type                        | `plot_type`                    | Plot Type                              | `base_plot`                         | `.plot_base`                         |
| X Variable                       | `x_var`                        | X Variable                             | `base_plot`                         | `.process_data`, `.prepare_plot_data` |
| Y Variable                       | `y_var`                        | Y Variable                             | `base_plot`                         | `.process_data`, `.prepare_plot_data` |
| Grouping Variable                | `group_var`                    | Grouping Variable                      | `base_plot`                         | `.process_data`, `.prepare_plot_data` |
| **Customization Options**        |                                |                                        |                                     |                                      |
| Main Title                       | `main_title`                   | Main Title                             | `base_plot`                         | `.plot_base`                         |
| X-axis Label                     | `x_label`                      | X-axis Label                           | `base_plot`                         | `.plot_base`                         |
| Y-axis Label                     | `y_label`                      | Y-axis Label                           | `base_plot`                         | `.plot_base`                         |
| Point Type                       | `point_type`                   | Point Type                             | `base_plot`                         | `.plot_base`                         |
| Point Size                       | `point_size`                   | Point Size                             | `base_plot`                         | `.plot_base`                         |
| Color Scheme                     | `color_scheme`                 | Color Scheme                           | `base_plot`                         | `.plot_base`                         |
| Add Grid Lines                   | `add_grid`                     | Add Grid Lines                         | `base_plot`                         | `.plot_base`                         |
| Add Legend                       | `add_legend`                   | Add Legend                             | `base_plot`                         | `.plot_base`                         |
| Bins (Histogram)                 | `bins`                         | Bins (Histogram)                       | `base_plot`                         | `.plot_base`                         |
| Show Statistics                  | `show_statistics`              | Show Statistics                        | `base_plot`                         | `.add_statistics_to_plot`            |
| Custom Axis Limits               | `custom_limits`                | Custom Axis Limits                     | `base_plot`                         | `.plot_base`                         |
| X-axis Minimum                   | `x_min`                        | X-axis Min                             | `base_plot`                         | `.plot_base`                         |
| X-axis Maximum                   | `x_max`                        | X-axis Max                             | `base_plot`                         | `.plot_base`                         |
| Y-axis Minimum                   | `y_min`                        | Y-axis Min                             | `base_plot`                         | `.plot_base`                         |
| Y-axis Maximum                   | `y_max`                        | Y-axis Max                             | `base_plot`                         | `.plot_base`                         |
| **Output Information**           |                                |                                        |                                     |                                      |
| Instructions                     | (N/A)                          | Instructions                           | `instructions`                      | `.init`                              |
| Plot Description                 | (N/A)                          | Plot Description                       | `plot_description`                  | `.generate_description`              |
