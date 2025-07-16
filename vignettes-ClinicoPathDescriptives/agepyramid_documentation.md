# Age Pyramid Analysis Documentation

This document provides a comprehensive overview of the Age Pyramid Analysis module, detailing its features, user interface elements, and the underlying R functions.

## Feature Summary

The Age Pyramid Analysis module generates an age pyramid plot, a graphical illustration that shows the distribution of various age groups in a population (typically that of a country or region of the world), which forms the shape of a pyramid when the population is growing. It allows for customization of age bin width and plot title.

The module's features can be broadly categorized as follows:

*   **Core Plotting:** Generates a standard age pyramid plot.
*   **Data Preparation:** Handles age and gender data, allowing specification of the female level.
*   **Customization:** Options for bin width and plot title.
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
| **Tabular Output**               |                                |                                        |                                     |                                      |
| Population Data Table            | `pyramidTable`                 | Population Data                        | `pyramidTable`                      | `.run`                               |
