# Advanced Barplot Analysis Documentation

This document provides a comprehensive overview of the Advanced Barplot Analysis module, detailing its features, user interface elements, and the underlying R functions.

## Feature Summary

The Advanced Barplot Analysis module is a versatile tool for creating a wide variety of bar charts. It offers five different approaches, each tailored to specific use cases, from basic data exploration to publication-ready figures.

The module's features can be broadly categorized as follows:

*   **Core Plotting:** Basic and advanced options for creating bar charts.
*   **Statistical Analysis:** Integrated statistical tests and annotations.
*   **Customization:** A wide range of options for customizing the appearance of the plot.
*   **Interactivity:** Support for creating interactive plots.
*   **Exporting:** Options for exporting plots in various formats.

## Feature Details

The following table provides a detailed mapping of the module's features, from the user interface to the underlying R functions.

| Feature                          | YAML Argument (`.a.yaml`)      | UI Label                               | Results Section (`.r.yaml`)         | R Function (`.b.R`)                  |
| -------------------------------- | ------------------------------ | -------------------------------------- | ----------------------------------- | ------------------------------------ |
| **Core Plotting**                |                                |                                        |                                     |                                      |
| X Variable                       | `x_var`                        | X Variable (Categories)                | `main_plot`                         | `.process_data`                      |
| Y Variable                       | `y_var`                        | Y Variable (Values)                    | `main_plot`                         | `.process_data`                      |
| Fill Variable                    | `fill_var`                     | Fill Variable (Optional)               | `main_plot`                         | `.process_data`                      |
| Facet Variable                   | `facet_var`                    | Facet Variable (Optional)              | `main_plot`                         | `.process_data`                      |
| Bar Chart Approach               | `chart_approach`               | Bar Chart Approach                     | `approach_description`, `main_plot` | `.run`, `.plot_main`                 |
| Bar Position                     | `bar_position`                 | Bar Position                           | `main_plot`                         | `.plot_main`                         |
| Statistical Summary              | `stat_type`                    | Statistical Summary                    | `summary_stats`, `main_plot`        | `.process_data`, `.generate_summary_stats` |
| Bar Orientation                  | `orientation`                  | Bar Orientation                        | `main_plot`                         | `.plot_main`                         |
| **Statistical Analysis**         |                                |                                        |                                     |                                      |
| Add Statistical Tests            | `add_statistics`               | Add Statistical Tests                  | `statistical_results`               | `.generate_statistical_results`      |
| Statistical Test                 | `stat_method`                  | Statistical Test                       | `statistical_results`               | `.generate_statistical_results`      |
| Error Bars                       | `error_bars`                   | Error Bars                             | `main_plot`                         | `.create_statistical_plot`           |
| **Customization**                |                                |                                        |                                     |                                      |
| Color Palette                    | `color_palette`                | Color Palette                          | `main_plot`                         | `.apply_color_palette`               |
| Show Data Values                 | `show_values`                  | Show Data Values                       | `main_plot`                         | `.create_basic_plot`                 |
| Value Format                     | `value_format`                 | Value Format                           | `main_plot`                         | `.format_values`                     |
| Plot Title                       | `plot_title`                   | Plot Title                             | `main_plot`                         | `.get_plot_titles`                   |
| X-axis Title                     | `x_title`                      | X-axis Title                           | `main_plot`                         | `.get_plot_titles`                   |
| Y-axis Title                     | `y_title`                      | Y-axis Title                           | `main_plot`                         | `.get_plot_titles`                   |
| Legend Position                  | `legend_position`              | Legend Position                        | `main_plot`                         | `.create_polished_plot`              |
| Theme Style                      | `theme_style`                  | Theme Style                            | `main_plot`                         | `.get_prism_theme`                   |
| Bar Width                        | `bar_width`                    | Bar Width                              | `main_plot`                         | `.create_basic_plot`                 |
| Plot Width                       | `plot_width`                   | Plot Width                             | `main_plot`                         | `.init`                              |
| Plot Height                      | `plot_height`                  | Plot Height                            | `main_plot`                         | `.init`                              |
| Sort Bars                        | `sort_bars`                    | Sort Bars                              | `main_plot`                         | `.process_data`                      |
| Add Trend Line                   | `add_trend_line`               | Add Trend Line                         | `main_plot`                         | Not implemented in `.b.R`            |
| Highlight Specific Bars          | `highlight_bars`               | Highlight Specific Bars                | `main_plot`                         | Not implemented in `.b.R`            |
| Bar Transparency                 | `transparency`                 | Bar Transparency                       | `main_plot`                         | `.create_basic_plot`                 |
| **Interactivity**                |                                |                                        |                                     |                                      |
| Interactive Plotly               | `chart_approach:interactive`   | 4. Interactive Plotly                  | `interactive_plot`                  | `.create_interactive_plot`           |
| **Exporting**                    |                                |                                        |                                     |                                      |
| Export Ready                     | `export_options`               | Export Ready                           | `main_plot`                         | Not implemented in `.b.R`            |
