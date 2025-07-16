# Clinical Research Visualization with visR Documentation

This document provides a comprehensive overview of the Clinical Research Visualization with visR module, detailing its features, user interface elements, and the underlying R functions.

## Feature Summary

This module provides fit-for-purpose clinical visualizations using the `visR` package, with sensible defaults based on graphical principles. It supports various analysis types, including Kaplan-Meier survival curves, cumulative incidence plots, Table One summaries, attrition flowcharts, and risk tables. The module offers flexible data input formats (standard or CDISC), extensive customization options for plot aesthetics, and integrated summary statistics and clinical interpretations, making it a powerful tool for clinical and medical research.

## Feature Details

The following table provides a detailed mapping of the module's features, from the user interface to the underlying R functions.

| Feature                          | YAML Argument (`.a.yaml`)      | UI Label                               | Results Section (`.r.yaml`)         | R Function (`.b.R`)                  |
| -------------------------------- | ------------------------------ | -------------------------------------- | ----------------------------------- | ------------------------------------ |
| **Analysis Type**                |                                |                                        |                                     |                                      |
| Analysis Type                    | `analysis_type`                | Analysis Type                          | `plot`, `summary`, `interpretation` | `.run`, `.runKaplanMeier`, `.runCumInc`, `.runTableOne`, `.runAttrition`, `.runRiskTable` |
| **Time & Event Variables**       |                                |                                        |                                     |                                      |
| Time Variable                    | `time_var`                     | Time Variable                          | `plot`                              | `.run`, `.prepareData`               |
| Event Variable                   | `event_var`                    | Event Variable                         | `plot`                              | `.run`, `.prepareData`               |
| Stratification Variable          | `strata_var`                   | Stratification Variable                | `plot`                              | `.run`, `.prepareData`               |
| **CDISC Format Options**         |                                |                                        |                                     |                                      |
| Use CDISC Format                 | `cdisc_format`                 | Use CDISC Format                       | `plot`                              | `.run`, `.prepareData`               |
| AVAL Variable (CDISC)            | `aval_var`                     | AVAL Variable (CDISC)                  | `plot`                              | `.run`, `.prepareData`               |
| CNSR Variable (CDISC)            | `cnsr_var`                     | CNSR Variable (CDISC)                  | `plot`                              | `.run`, `.prepareData`               |
| **Plot Customization**           |                                |                                        |                                     |                                      |
| Function Scale                   | `fun_type`                     | Function Scale                         | `plot`                              | `.jvisr_plot_enhanced`               |
| Show Confidence Intervals        | `confidence_interval`          | Show Confidence Intervals              | `plot`                              | `.jvisr_plot_enhanced`               |
| Show Risk Table                  | `risk_table`                   | Show Risk Table                        | `plot`                              | `.jvisr_plot_enhanced`               |
| Show Quantiles                   | `quantiles`                    | Show Quantiles                         | `plot`                              | Not implemented in `.b.R`            |
| Show P-value                     | `p_value`                      | Show P-value                           | `plot`                              | Not implemented in `.b.R`            |
| Legend Position                  | `legend_position`              | Legend Position                        | `plot`                              | `.jvisr_plot_enhanced`               |
| Time Axis Label                  | `time_label`                   | Time Axis Label                        | `plot`                              | `.jvisr_plot_enhanced`               |
| Time Units                       | `time_units`                   | Time Units                             | `plot`                              | `.jvisr_plot_enhanced`               |
| Survival Axis Label              | `survival_label`               | Survival Axis Label                    | `plot`                              | `.jvisr_plot_enhanced`               |
| Plot Title                       | `title`                        | Plot Title                             | `plot`                              | `.jvisr_plot_enhanced`               |
| Theme Style                      | `theme_style`                  | Theme Style                            | `plot`                              | `.jvisr_plot_enhanced`               |
| Color Palette                    | `color_palette`                | Color Palette                          | `plot`                              | `.jvisr_plot_enhanced`               |
| **Output Options**               |                                |                                        |                                     |                                      |
| Show Summary Table               | `show_summary`                 | Show Summary Table                     | `summary`                           | `.run`, `.jget_summary`              |
| Show Clinical Interpretation     | `show_interpretation`          | Show Clinical Interpretation           | `interpretation`                    | `.run`, `.jget_interpretation`       |
