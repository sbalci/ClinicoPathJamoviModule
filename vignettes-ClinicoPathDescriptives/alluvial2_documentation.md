# Alluvial Diagrams 2 Analysis Documentation

This document provides a comprehensive overview of the Alluvial Diagrams 2 Analysis module, detailing its features, user interface elements, and the underlying R functions.

## Feature Summary

The Alluvial Diagrams 2 module is an alternative implementation for visualizing changes in network structures or flows over time or across different categories. It is particularly useful for showing how individual entities or groups transition between different states or categories, building upon the `easyalluvial` package.

The module's features can be broadly categorized as follows:

*   **Core Plotting:** Generates alluvial diagrams to visualize flows and transitions.
*   **Data Handling:** Allows selection of multiple variables to represent different stages or categories in the flow.
*   **Customization:** Options for filling by different variables, binning labels, and plot orientation.
*   **Marginal Plots:** Ability to include marginal histograms for each variable.
*   **Condensation Plot:** Generates a condensation plot to visualize the flow of a specific variable.

## Feature Details

The following table provides a detailed mapping of the module's features, from the user interface to the underlying R functions.

| Feature                          | YAML Argument (`.a.yaml`)      | UI Label                               | Results Section (`.r.yaml`)         | R Function (`.b.R`)                  |
| -------------------------------- | ------------------------------ | -------------------------------------- | ----------------------------------- | ------------------------------------ |
| **Core Plotting**                |                                |                                        |                                     |                                      |
| Variables                        | `vars`                         | Variables                              | `plot`                              | `.run`, `.plot`                      |
| Exclude missing (NA)             | `excl`                         | Exclude missing (NA)                   | `plot`                              | `.plot`                              |
| Fill by                          | `fill`                         | Fill by                                | `plot`                              | `.plot`                              |
| Bin labels                       | `bin`                          | Bin labels                             | `plot`                              | `.plot`                              |
| Plot orientation                 | `orient`                       | Plot orientation                       | `plot`                              | `.plot`                              |
| Custom title                     | `usetitle`                     | Custom title                           | `plot`                              | `.plot`                              |
| Title                            | `mytitle`                      | Title                                  | `plot`                              | `.plot`                              |
| **Marginal Plots**               |                                |                                        |                                     |                                      |
| Marginal plots                   | `marg`                         | Marginal plots                         | `plot`                              | `.plot`                              |
| **Condensation Plot**            |                                |                                        |                                     |                                      |
| Condensation Variable            | `condensationvar`              | Condensation Variable                  | `plot2`                             | `.plot2`                             |
