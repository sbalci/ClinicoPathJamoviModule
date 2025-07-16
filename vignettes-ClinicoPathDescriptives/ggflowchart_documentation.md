# ggFlowchart Analysis Documentation

This document provides a comprehensive overview of the ggFlowchart module, detailing its features, user interface elements, and the underlying R functions.

## Feature Summary

The ggFlowchart module is a powerful tool for creating modern, ggplot2-styled flowcharts. It offers a streamlined approach to visualizing processes, decision trees, and study flows, providing a clean and customizable alternative to traditional flowcharting tools.

The module's features can be broadly categorized as follows:

*   **Core Flowcharting:** Basic functionality to define 'from' and 'to' node connections.
*   **Customizable Aesthetics:** Options for node fill colors using various palettes.
*   **Modern Visualization:** Generates clean and contemporary flowchart appearances with ggplot2 integration.
*   **Interpretation Guide:** Provides an in-app guide for understanding the flowchart and its features.

## Feature Details

The following table provides a detailed mapping of the module's features, from the user interface to the underlying R functions.

| Feature                          | YAML Argument (`.a.yaml`)      | UI Label                               | Results Section (`.r.yaml`)         | R Function (`.b.R`)                  |
| -------------------------------- | ------------------------------ | -------------------------------------- | ----------------------------------- | ------------------------------------ |
| **Core Flowcharting**            |                                |                                        |                                     |                                      |
| From Node                        | `from_var`                     | From Node                              | `plot`                              | `.plot`                              |
| To Node                          | `to_var`                       | To Node                                | `plot`                              | `.plot`                              |
| Node Grouping (Optional)         | `group_var`                    | Node Grouping (Optional)               | `plot`                              | `.plot`                              |
| Node Fill Palette                | `node_fill_palette`            | Node Fill Palette                      | `plot`                              | `.get_color_palette`                 |
| Plot Title                       | `plot_title`                   | Plot Title                             | `plot`                              | `.plot`                              |
| Show Interpretation Guide        | `show_interpretation`          | Show Interpretation Guide              | `interpretation`                    | `.generate_interpretation_guide`     |
