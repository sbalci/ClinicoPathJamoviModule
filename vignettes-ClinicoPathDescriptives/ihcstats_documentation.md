# Immunohistochemistry (IHC) Statistics Documentation

This document provides a comprehensive overview of the Immunohistochemistry (IHC) Statistics module (ihcstats), detailing its features, user interface elements, and the underlying R functions.

## Feature Summary

The ihcstats module is a specialized tool for analyzing and visualizing data derived from immunohistochemistry experiments. It provides functionalities for quantifying staining intensity, assessing positivity, and comparing IHC markers across different samples or patient groups.

The module's features can be broadly categorized as follows:

*   **Core IHC Quantification:** Calculate various metrics for IHC staining (e.g., H-score, percentage positivity, intensity scores).
*   **Marker Comparison:** Compare expression levels of different IHC markers.
*   **Group Analysis:** Analyze IHC data across different clinical or pathological groups.
*   **Visualization:** Generate plots suitable for presenting IHC results (e.g., bar plots, box plots, heatmaps).
*   **Data Quality Checks:** Basic checks for consistency and completeness of IHC data.

## Feature Details

The following table provides a detailed mapping of the module's features, from the user interface to the underlying R functions.

| Feature                          | YAML Argument (`.a.yaml`)      | UI Label                               | Results Section (`.r.yaml`)         | R Function (`.b.R`)                  |
| -------------------------------- | ------------------------------ | -------------------------------------- | ----------------------------------- | ------------------------------------ |
| **Core Analysis**                |                                |                                        |                                     |                                      |
| IHC Marker Variable              | `ihcMarker`                    | IHC Marker Variable                    | `ihcOverview`                       | `.calculateIHCStats`                 |
| Staining Intensity               | `intensityVar`                 | Staining Intensity Variable            | `ihcOverview`                       | `.calculateIHCStats`                 |
| Percentage Positive Cells        | `positiveCellsVar`             | Percentage Positive Cells Variable     | `ihcOverview`                       | `.calculateIHCStats`                 |
| Grouping Variable                | `groupingVar`                  | Grouping Variable                      | `ihcOverview`                       | `.calculateIHCStats`                 |
| **IHC Metrics**                  |                                |                                        |                                     |                                      |
| Calculate H-score                | `calculateHScore`              | Calculate H-score                      | `hScoreResults`                     | `.calculateHScore`                   |
| Calculate Percentage Positivity  | `calculatePercentPositivity`   | Calculate Percentage Positivity        | `percentPositivityResults`          | `.calculatePercentPositivity`        |
| **Comparison & Group Analysis**  |                                |                                        |                                     |                                      |
| Compare Markers                  | `compareMarkers`               | Compare Markers                        | `markerComparison`                  | `.compareIHCMarkers`                 |
| Compare Groups                   | `compareGroups`                | Compare Groups                         | `groupComparison`                   | `.compareIHCGroups`                  |
| Statistical Test                 | `statisticalTest`              | Statistical Test                       | `groupComparison`                   | `.performStatisticalTest`            |
| **Visualizations**               |                                |                                        |                                     |                                      |
| Show Bar Plot                    | `showBarPlot`                  | Show Bar Plot                          | `ihcBarPlot`                        | `.plotIHCBarPlot`                    |
| Show Box Plot                    | `showBoxPlot`                  | Show Box Plot                          | `ihcBoxPlot`                        | `.plotIHCBoxPlot`                    |
| Show Heatmap                     | `showHeatmap`                  | Show Heatmap                           | `ihcHeatmap`                        | `.plotIHCHeatmap`                    |
| **Advanced Options**             |                                |                                        |                                     |                                      |
| Missing Data Handling            | `missingDataHandling`          | Missing Data Handling                  | `advancedOptions`                   | `.handleMissingData`                 |
| Export Results                   | `exportResults`                | Export Results                         | `exportOptions`                     | `.exportIHCResults`                  |
| Export Plot                      | `exportPlot`                   | Export Plot                            | `exportOptions`                     | `.exportIHCPlot`                     |