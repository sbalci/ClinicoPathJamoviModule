# Advanced Raincloud Plot Analysis Documentation

This document provides a comprehensive overview of the Advanced Raincloud Plot Analysis module, detailing its features, user interface elements, and the underlying R functions.

## Feature Summary

The Advanced Raincloud Plot Analysis module is a powerful tool for visualizing data distributions, especially for longitudinal and ordinal data. It extends the basic raincloud plot functionality with advanced features like connecting repeated observations, Likert scale support, and flexible raincloud positioning.

The module's features can be broadly categorized as follows:

*   **Core Plotting:** Advanced options for creating raincloud plots.
*   **Longitudinal Data Support:** Connect repeated observations for the same subjects.
*   **Likert Scale Support:** Specialized handling for ordinal survey data.
*   **Customization:** A wide range of options for customizing the appearance of the plot.
*   **Statistical Analysis:** Integrated summary statistics and group comparisons.

## Feature Details

The following table provides a detailed mapping of the module's features, from the user interface to the underlying R functions.

| Feature                          | YAML Argument (`.a.yaml`)      | UI Label                               | Results Section (`.r.yaml`)         | R Function (`.b.R`)                  |
| -------------------------------- | ------------------------------ | -------------------------------------- | ----------------------------------- | ------------------------------------ |
| **Core Plotting**                |                                |                                        |                                     |                                      |
| Y-Axis Variable                  | `y_var`                        | Y-Axis Variable                        | `plot`                              | `.run`, `.plot`                      |
| X-Axis Variable (Grouping)       | `x_var`                        | X-Axis Variable (Grouping)             | `plot`                              | `.run`, `.plot`                      |
| Fill Variable                    | `fill_var`                     | Fill Variable (Optional)               | `plot`                              | `.plot`                              |
| Raincloud Position               | `rain_side`                    | Raincloud Position                     | `plot`                              | `.plot`                              |
| Point Size                       | `point_size`                   | Point Size                             | `plot`                              | `.plot`                              |
| Point Transparency               | `point_alpha`                  | Point Transparency                     | `plot`                              | `.plot`                              |
| Violin Transparency              | `violin_alpha`                 | Violin Transparency                    | `plot`                              | `.plot`                              |
| Boxplot Width                    | `boxplot_width`                | Boxplot Width                          | `plot`                              | `.plot`                              |
| Color Palette                    | `color_palette`                | Color Palette                          | `plot`                              | `.get_color_palette`                 |
| Plot Title                       | `plot_title`                   | Plot Title                             | `plot`                              | `.plot`                              |
| X-Axis Label                     | `x_label`                      | X-Axis Label                           | `plot`                              | `.plot`                              |
| Y-Axis Label                     | `y_label`                      | Y-Axis Label                           | `plot`                              | `.plot`                              |
| **Longitudinal Data Support**    |                                |                                        |                                     |                                      |
| Longitudinal ID                  | `id_var`                       | Longitudinal ID (Optional)             | `plot`                              | `.run`, `.plot`                      |
| Show Longitudinal Connections    | `show_longitudinal`            | Show Longitudinal Connections          | `plot`                              | `.plot`                              |
| **Likert Scale Support**         |                                |                                        |                                     |                                      |
| Likert Scale Mode                | `likert_mode`                  | Likert Scale Mode                      | `plot`                              | `.plot`                              |
| **Statistical Analysis**         |                                |                                        |                                     |                                      |
| Show Summary Statistics          | `show_statistics`              | Show Summary Statistics                | `statistics`                        | `.generate_statistics`               |
| Show Group Comparisons           | `show_comparisons`             | Show Group Comparisons                 | `comparisons`                       | `.generate_comparisons`              |
| **Interpretation**               |                                |                                        |                                     |                                      |
| Show Usage Guide                 | `show_interpretation`          | Show Usage Guide                       | `interpretation`                    | `.generate_interpretation_guide`     |
