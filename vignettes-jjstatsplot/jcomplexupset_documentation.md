# Complex UpSet Plot Analysis Documentation

This document provides a comprehensive overview of the Complex UpSet Plot Analysis module (jcomplexupset), detailing its features, user interface elements, and the underlying R functions.

## Feature Summary

The jcomplexupset module is a specialized tool for visualizing the intersections of multiple sets, offering a more scalable and informative alternative to Venn diagrams for more than three sets. It is particularly useful for exploring complex relationships and overlaps between different categories or groups.

The module's features can be broadly categorized as follows:

*   **Core UpSet Plot Generation:** Create UpSet plots to visualize set intersections.
*   **Set and Intersection Customization:** Options for styling sets, intersections, and their associated data.
*   **Attribute Plots:** Integrate additional plots (e.g., box plots, bar plots) to visualize attributes of elements within intersections.
*   **Filtering and Ordering:** Tools for organizing and focusing on specific intersections.
*   **Export Options:** Capabilities to save the generated plots in various formats.

## Feature Details

The following table provides a detailed mapping of the module's features, from the user interface to the underlying R functions.

| Feature                          | YAML Argument (`.a.yaml`)      | UI Label                               | Results Section (`.r.yaml`)         | R Function (`.b.R`)                  |
| -------------------------------- | ------------------------------ | -------------------------------------- | ----------------------------------- | ------------------------------------ |
| **Core Analysis**                |                                |                                        |                                     |                                      |
| Set Variables                    | `setVariables`                 | Set Variables                          | `upsetOverview`                     | `.calculateUpSetData`                |
| **Visualization Options**        |                                |                                        |                                     |                                      |
| Show UpSet Plot                  | `showUpSetPlot`                | Show UpSet Plot                        | `upsetPlot`                         | `.plotUpSet`                         |
| Intersection Order               | `intersectionOrder`            | Intersection Order                     | `upsetPlot`                         | `.plotUpSet`                         |
| Show Set Sizes                   | `showSetSizes`                 | Show Set Sizes                         | `upsetPlot`                         | `.plotUpSet`                         |
| Show Intersection Sizes          | `showIntersectionSizes`        | Show Intersection Sizes                | `upsetPlot`                         | `.plotUpSet`                         |
| **Attribute Plots**              |                                |                                        |                                     |                                      |
| Add Attribute Plot               | `addAttributePlot`             | Add Attribute Plot                     | `upsetPlot`                         | `.plotUpSet`                         |
| Attribute Variable               | `attributeVariable`            | Attribute Variable                     | `upsetPlot`                         | `.plotUpSet`                         |
| Attribute Plot Type              | `attributePlotType`            | Attribute Plot Type                    | `upsetPlot`                         | `.plotUpSet`                         |
| **Advanced Features**            |                                |                                        |                                     |                                      |
| Min Intersection Size            | `minIntersectionSize`          | Minimum Intersection Size              | `upsetPlot`                         | `.plotUpSet`                         |
| Max Intersection Size            | `maxIntersectionSize`          | Maximum Intersection Size              | `upsetPlot`                         | `.plotUpSet`                         |
| Export Plot                      | `exportPlot`                   | Export Plot                            | `exportOptions`                     | `.exportUpSetPlot`                   |