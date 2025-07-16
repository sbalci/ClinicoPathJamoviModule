# Age Pyramid 2 Analysis Documentation

This document provides a comprehensive overview of the Age Pyramid 2 Analysis module, detailing its features, user interface elements, and the underlying R functions.

## Feature Summary

The Age Pyramid 2 Analysis module generates an age pyramid plot using `ggcharts::pyramid_chart`. It allows for customization of age bin width, plot title, and bar colors. This module provides an alternative implementation of the age pyramid with additional styling options.

The module's features can be broadly categorized as follows:

*   **Core Plotting:** Generates an age pyramid plot using `ggcharts`.
*   **Data Preparation:** Handles age and gender data, allowing specification of the female level.
*   **Customization:** Options for bin width, plot title, and custom bar colors.
*   **Tabular Output:** Provides a table of population counts by age group and gender.

## Feature Details

The following table provides a detailed mapping of the module's features, from the user interface to the underlying R functions.

| Feature                          | YAML Argument (`.a.yaml`)      | UI Label                               | Results Section (`.r.yaml`)         | R Function (`.b.R`)                  |
| -------------------------------- | ------------------------------ | -------------------------------------- | ----------------------------------- | ------------------------------------ |
| **Core Plotting**                |                                |                                        |                                     |                                      |
| Age Variable                     | `age`                          | Age                                    | `plot`                              | `.run`, `.plot`                      |
| Gender Variable                  | `gender`                       | Gender                                 | `plot`                              | `.run`, `.plot`                      |
| Select Female Level              | `female`                       | Select Female                          | `plot`                              | `.run`, `.plot`                      |
| Bin Width (Years)                | `bin_width`                    | Bin Width (Years)                      | `plot`                              | `.run`, `.plot`                      |
| Plot Title                       | `plot_title`                   | Plot Title                             | `plot`                              | `.plot`                              |
| First Color                      | `color1`                       | First Color                            | `plot`                              | `.plot`                              |
| Second Color                     | `color2`                       | Second Color                           | `plot`                              | `.plot`                              |
| **Tabular Output**               |                                |                                        |                                     |                                      |
| Population Data Table            | `pyramidTable`                 | Population Data                        | `pyramidTable`                      | `.run`                               |
