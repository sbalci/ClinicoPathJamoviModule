# Segmented Total Bar Plot Analysis Documentation

This document provides a comprehensive overview of the Segmented Total Bar Plot Analysis module (jjsegmentedtotalbar), detailing its features, user interface elements, and the underlying R functions.

## Feature Summary

The jjsegmentedtotalbar module is a specialized tool for creating segmented bar plots where each bar represents a total, and segments within the bar represent proportions of different categories. It is particularly useful for visualizing compositional data and comparing proportions across different groups.

The module's features can be broadly categorized as follows:

* **Core Segmented Bar Plot Generation:** Create bar plots showing total counts or values segmented by categories.
* **Proportional Representation:** Visualize the contribution of each segment to the total.
* **Customization Options:** Control plot aesthetics such as colors, labels, and bar orientation.
* **Grouping and Faceting:** Organize plots by additional categorical variables.
* **Export Options:** Capabilities to save the generated plots in various formats.

## Feature Details

The following table provides a detailed mapping of the module's features, from the user interface to the underlying R functions.

| Feature                          | YAML Argument (`.a.yaml`)      | UI Label                               | Results Section (`.r.yaml`)         | R Function (`.b.R`)                  |
| -------------------------------- | ------------------------------ | -------------------------------------- | ----------------------------------- | ------------------------------------ |
| **Core Analysis**                |                                |                                        |                                     |                                      |
| Category Variable                | `categoryVar`                  | Category Variable                      | `segmentedBarOverview`              | `.calculateSegmentedBarData`         |
| Segment Variable                 | `segmentVar`                   | Segment Variable                       | `segmentedBarOverview`              | `.calculateSegmentedBarData`         |
| Value Variable                   | `valueVar`                     | Value Variable (Optional)              | `segmentedBarOverview`              | `.calculateSegmentedBarData`         |
| **Visualization Options**        |                                |                                        |                                     |                                      |
| Show Segmented Bar Plot          | `showSegmentedBarPlot`         | Show Segmented Bar Plot                | `segmentedBarPlot`                  | `.plotSegmentedBar`                  |
| Bar Orientation                  | `barOrientation`               | Bar Orientation                        | `segmentedBarPlot`                  | `.plotSegmentedBar`                  |
| Color Palette                    | `colorPalette`                 | Color Palette                          | `segmentedBarPlot`                  | `.plotSegmentedBar`                  |
| Show Percentages                 | `showPercentages`              | Show Percentages                       | `segmentedBarPlot`                  | `.plotSegmentedBar`                  |
| Show Values                      | `showValues`                   | Show Values                            | `segmentedBarPlot`                  | `.plotSegmentedBar`                  |
| X-axis Label                     | `xAxisLabel`                   | X-axis Label                           | `segmentedBarPlot`                  | `.plotSegmentedBar`                  |
| Y-axis Label                     | `yAxisLabel`                   | Y-axis Label                           | `segmentedBarPlot`                  | `.plotSegmentedBar`                  |
| Plot Title                       | `plotTitle`                    | Plot Title                             | `segmentedBarPlot`                  | `.plotSegmentedBar`                  |
| **Advanced Features**            |                                |                                        |                                     |                                      |
| Faceting Variable                | `facetVar`                     | Faceting Variable                      | `segmentedBarPlot`                  | `.plotSegmentedBar`                  |
| Export Plot                      | `exportPlot`                   | Export Plot                            | `exportOptions`                     | `.exportSegmentedBarPlot`            |
