# Social Science Statistical Visualization Documentation

This document provides a comprehensive overview of the Social Science Statistical Visualization module, detailing its features, user interface elements, and the underlying R functions.

## Feature Summary

This module provides social science statistical visualization using the `sjPlot` package for regression tables, model plots, and interaction visualizations. It offers a wide range of analysis types, including regression tables, coefficient plots, interaction plots, marginal effects, frequency tables, correlation matrices, and PCA visualizations. The module supports various model types (linear, generalized linear, mixed effects) and provides extensive customization options for plot appearance, confidence levels, and output formatting.

## Feature Details

The following table provides a detailed mapping of the module's features, from the user interface to the underlying R functions.

| Feature                          | YAML Argument (`.a.yaml`)      | UI Label                               | Results Section (`.r.yaml`)         | R Function (`.b.R`)                  |
| -------------------------------- | ------------------------------ | -------------------------------------- | ----------------------------------- | ------------------------------------ |
| **Analysis Type**                |                                |                                        |                                     |                                      |
| Analysis Type                    | `analysis_type`                | Analysis Type                          | `plot`, `model_table`, `statistics`, `summary` | `.run`, `.runRegressionTable`, `.runCoefficientPlot`, `.runInteractionPlot`, `.runMarginalEffects`, `.runFrequencyTable`, `.runCorrelationMatrix`, `.runPCAPlot` |
| **Model Specification**          |                                |                                        |                                     |                                      |
| Dependent Variable               | `dependent_var`                | Dependent Variable                     | `model_table`, `statistics`, `summary`, `plot` | `.buildModel`, `.buildInteractionModel` |
| Independent Variables            | `independent_vars`             | Independent Variables                  | `model_table`, `statistics`, `summary`, `plot` | `.buildModel`, `.buildInteractionModel` |
| Grouping Variable                | `grouping_var`                 | Grouping Variable                      | `model_table`, `statistics`, `summary`, `plot` | Not implemented in `.b.R`            |
| Interaction Variables            | `interaction_vars`             | Interaction Variables                  | `model_table`, `statistics`, `summary`, `plot` | `.buildInteractionModel`             |
| Model Type                       | `model_type`                   | Model Type                             | `model_table`, `statistics`, `summary` | `.buildModel`                        |
| GLM Family                       | `family`                       | GLM Family                             | `model_table`, `statistics`, `summary` | `.buildModel`                        |
| Plot Type                        | `plot_type`                    | Plot Type                              | `plot`                              | `.jplot_model`                       |
| **Plot Customization**           |                                |                                        |                                     |                                      |
| Confidence Level                 | `confidence_level`             | Confidence Level                       | `model_table`                       | `.jtab_model`, `.jplot_model`        |
| Standardized Coefficients        | `standardized`                 | Standardized Coefficients              | `model_table`                       | `.jtab_model`, `.jplot_model`        |
| Show Values                      | `show_values`                  | Show Values                            | `plot`                              | `.jplot_model`                       |
| Show P-values                    | `show_p_values`                | Show P-values                          | `model_table`, `plot`               | `.jtab_model`, `.jplot_model`        |
| Sort Estimates                   | `sort_estimates`               | Sort Estimates                         | `plot`                              | `.jplot_model`                       |
| Remove Intercept                 | `remove_intercept`             | Remove Intercept                       | `plot`                              | `.jplot_model`                       |
| Grid Breaks                      | `grid_breaks`                  | Grid Breaks                            | `plot`                              | `.jplot_model`                       |
| Dot Size                         | `dot_size`                     | Dot Size                               | `plot`                              | `.jplot_model`                       |
| Line Size                        | `line_size`                    | Line Size                              | `plot`                              | `.jplot_model`                       |
| Color Scheme                     | `colors`                       | Color Scheme                           | `plot`                              | `.jplot_model`                       |
| Theme                            | `theme_style`                  | Theme                                  | `plot`                              | `.jplot_model`, `.japply_sjplot_theme` |
| Plot Title                       | `title`                        | Plot Title                             | `plot`                              | `.jplot_model`                       |
| Custom Axis Labels               | `axis_labels`                  | Custom Axis Labels                     | `plot`                              | `.jplot_model`                       |
| Transform Axis                   | `transform_axis`               | Transform Axis                         | `plot`                              | `.jplot_model`                       |
| Show Raw Data                    | `show_data`                    | Show Raw Data                          | `plot`                              | Not implemented in `.b.R`            |
| **Output Options**               |                                |                                        |                                     |                                      |
| Show Model Statistics            | `show_statistics`              | Show Model Statistics                  | `statistics`                        | `.jmodel_statistics`                 |
| Show Model Summary               | `show_summary`                 | Show Model Summary                     | `summary`                           | `.jmodel_summary`                    |
| HTML Output                      | `html_output`                  | HTML Output                            | `model_table`                       | `.jtab_model`                        |
