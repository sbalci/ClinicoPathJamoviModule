# Lollipop Charts for Categorical Data Visualization Documentation

This document provides a comprehensive overview of the Lollipop Charts for Categorical Data Visualization module, detailing its features, user interface elements, and the underlying R functions.

## Feature Summary

This module creates comprehensive lollipop charts for categorical data visualization with an emphasis on clinical applications. Lollipop charts are particularly effective for displaying categorical data with a focus on individual values, making them ideal for patient timelines, treatment outcomes, biomarker levels, and comparative clinical assessments. The module offers flexible orientation, advanced sorting options, clinical color schemes and themes, highlighting capabilities for specific categories, and integration of statistical summaries, resulting in professional, publication-ready charts.

## Feature Details

The following table provides a detailed mapping of the module's features, from the user interface to the underlying R functions.

| Feature                          | YAML Argument (`.a.yaml`)      | UI Label                               | Results Section (`.r.yaml`)         | R Function (`.b.R`)                  |
| -------------------------------- | ------------------------------ | -------------------------------------- | ----------------------------------- | ------------------------------------ |
| **Core Variables**               |                                |                                        |                                     |                                      |
| Dependent Variable               | `dep`                          | Dependent Variable                     | `summary`, `plot`                   | `.run`, `.cleanData`, `.calculateSummary`, `.savePlotData` |
| Grouping Variable                | `group`                        | Grouping Variable                      | `summary`, `plot`                   | `.run`, `.cleanData`, `.calculateSummary`, `.savePlotData` |
| **Customization & Aesthetics**   |                                |                                        |                                     |                                      |
| Highlight Level                  | `highlight`                    | Highlight Level                        | `plot`                              | `.plot`, `.getColorScheme`           |
| Sort Order                       | `sortBy`                       | Sort Order                             | `plot`                              | `.cleanData`, `.applySorting`        |
| Orientation                      | `orientation`                  | Orientation                            | `plot`                              | `.plot`                              |
| Show Values                      | `showValues`                   | Show Values                            | `plot`                              | `.plot`                              |
| Show Mean Line                   | `showMean`                     | Show Mean Line                         | `plot`                              | `.plot`                              |
| Color Scheme                     | `colorScheme`                  | Color Scheme                           | `plot`                              | `.plot`, `.getColorScheme`           |
| Plot Theme                       | `theme`                        | Plot Theme                             | `plot`                              | `.plot`, `.getPlotTheme`             |
| Point Size                       | `pointSize`                    | Point Size                             | `plot`                              | `.plot`                              |
| Line Width                       | `lineWidth`                    | Line Width                             | `plot`                              | `.plot`                              |
| X-axis Label                     | `xlabel`                       | X-axis Label                           | `plot`                              | `.plot`                              |
| Y-axis Label                     | `ylabel`                       | Y-axis Label                           | `plot`                              | `.plot`                              |
| Plot Title                       | `title`                        | Plot Title                             | `plot`                              | `.plot`                              |
| Plot Width                       | `width`                        | Plot Width                             | `plot`                              | Not implemented in `.b.R`            |
| Plot Height                      | `height`                       | Plot Height                            | `plot`                              | Not implemented in `.b.R`            |
| **Output Tables**                |                                |                                        |                                     |                                      |
| Data Summary Table               | `summary`                      | Data Summary                           | `summary`                           | `.populateSummary`                   |
