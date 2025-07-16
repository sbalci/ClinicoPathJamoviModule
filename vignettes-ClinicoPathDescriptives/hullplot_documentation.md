# Hull Plot Analysis Documentation

This document provides a comprehensive overview of the Hull Plot Analysis module (hullplot), detailing its features, user interface elements, and the underlying R functions.

## Feature Summary

The hullplot module is a specialized tool for visualizing data points within a convex hull, often used to highlight clusters or groups in scatter plots. It helps in visually delineating the boundaries of data groups.

The module's features can be broadly categorized as follows:

*   **Core Hull Plot Generation:** Create convex hull plots to enclose groups of data points.
*   **Grouping and Coloring:** Define groups and apply distinct colors to their hulls.
*   **Customization Options:** Control plot aesthetics such as hull transparency, line styles, and point appearance.
*   **Data Point Display:** Option to show or hide individual data points within the hulls.
*   **Export Options:** Capabilities to save the generated plots in various formats.

## Feature Details

The following table provides a detailed mapping of the module's features, from the user interface to the underlying R functions.

| Feature                          | YAML Argument (`.a.yaml`)      | UI Label                               | Results Section (`.r.yaml`)         | R Function (`.b.R`)                  |
| -------------------------------- | ------------------------------ | -------------------------------------- | ----------------------------------- | ------------------------------------ |
| **Core Analysis**                |                                |                                        |                                     |                                      |
| X Variable                       | `xVar`                         | X Variable                             | `hullPlotOverview`                  | `.calculateHullData`                 |
| Y Variable                       | `yVar`                         | Y Variable                             | `hullPlotOverview`                  | `.calculateHullData`                 |
| Grouping Variable                | `groupVar`                     | Grouping Variable                      | `hullPlotOverview`                  | `.calculateHullData`                 |
| **Visualization Options**        |                                |                                        |                                     |                                      |
| Show Hull Plot                   | `showHullPlot`                 | Show Hull Plot                         | `hullPlot`                          | `.plotHullPlot`                      |
| Alpha (Transparency)             | `alpha`                        | Alpha (Transparency)                   | `hullPlot`                          | `.plotHullPlot`                      |
| Fill Color                       | `fillColor`                    | Fill Color                             | `hullPlot`                          | `plotHullPlot`                       |
| Line Color                       | `lineColor`                    | Line Color                             | `hullPlot`                          | `.plotHullPlot`                      |
| Line Type                        | `lineType`                     | Line Type                              | `hullPlot`                          | `.plotHullPlot`                      |
| Line Thickness                   | `lineThickness`                | Line Thickness                         | `hullPlot`                          | `.plotHullPlot`                      |
| **Advanced Features**            |                                |                                        |                                     |                                      |
| Show Data Points                 | `showDataPoints`               | Show Data Points                       | `hullPlot`                          | `.plotHullPlot`                      |
| Point Size                       | `pointSize`                    | Point Size                             | `hullPlot`                          | `.plotHullPlot`                      |
| Point Color                      | `pointColor`                   | Point Color                            | `hullPlot`                          | `.plotHullPlot`                      |
| Export Plot                      | `exportPlot`                   | Export Plot                            | `exportOptions`                     | `.exportHullPlot`                    |