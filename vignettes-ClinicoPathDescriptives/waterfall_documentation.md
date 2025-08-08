# Treatment Response Analysis Documentation

This document provides a comprehensive overview of the Treatment Response Analysis module, detailing its features, user interface elements, and the underlying R functions.

## Feature Summary

The Treatment Response Analysis module creates waterfall and spider plots to visualize tumor response data, typically following RECIST (Response Evaluation Criteria in Solid Tumors) criteria. It supports both raw tumor measurements and pre-calculated percentage changes, providing a comprehensive analysis of patient response to treatment.

The module's features can be broadly categorized as follows:

*   **Plotting:** Generates waterfall plots (best response per patient) and spider plots (response trajectories over time).
*   **Data Handling:** Processes raw tumor measurements to calculate percentage changes or uses pre-calculated percentages. Handles missing data and validates input.
*   **RECIST Analysis:** Automatically categorizes responses into CR, PR, SD, PD based on RECIST criteria.
*   **Group Analysis:** Advanced group-based coloring system for comparing treatment arms, disease subtypes, or other patient groupings.
*   **Clinical Metrics:** Calculates Objective Response Rate (ORR), Disease Control Rate (DCR), and person-time metrics.
*   **Customization:** Options for sorting, thresholds, labeling, color schemes, and plot aesthetics with dual coloring strategies.
*   **Data Export:** Allows adding the calculated response category back to the data.

## Feature Details

The following table provides a detailed mapping of the module's features, from the user interface to the underlying R functions.

| Feature                          | YAML Argument (`.a.yaml`)      | UI Label                               | Results Section (`.r.yaml`)         | R Function (`.b.R`)                  |
| -------------------------------- | ------------------------------ | -------------------------------------- | ----------------------------------- | ------------------------------------ |
| **Plotting**                     |                                |                                        |                                     |                                      |
| Patient ID                       | `patientID`                    | Patient ID                             | `waterfallplot`, `spiderplot`       | `.validateData`, `.processData`      |
| Response Value                   | `responseVar`                  | Response Value                         | `waterfallplot`, `spiderplot`       | `.validateData`, `.processData`      |
| Time Variable                    | `timeVar`                      | Time Variable                          | `spiderplot`                        | `.validateData`, `.processData`      |
| Group Variable                   | `groupVar`                     | Group Variable                         | `waterfallplot`, `spiderplot`       | `.validateData`, `.processData`      |
| Input Type                       | `inputType`                    | Input Type                             | `waterfallplot`, `spiderplot`       | `.validateData`, `.processData`      |
| Sort By                          | `sortBy`                       | Sort By                                | `waterfallplot`                     | `.waterfallplot`                     |
| Show RECIST Thresholds           | `showThresholds`               | Show RECIST Thresholds                 | `waterfallplot`, `spiderplot`       | `.waterfallplot`, `.spiderplot`      |
| Label Large Changes              | `labelOutliers`                | Label Large Changes                    | `waterfallplot`                     | `.waterfallplot`                     |
| Show Median Response             | `showMedian`                   | Show Median Response                   | `waterfallplot`, `spiderplot`       | `.waterfallplot`, `.spiderplot`      |
| Show Confidence Interval         | `showCI`                       | Show Confidence Interval               | `waterfallplot`                     | `.waterfallplot`                     |
| Minimum Response for Labels (%)  | `minResponseForLabel`          | Minimum Response for Labels (%)        | `waterfallplot`                     | `.waterfallplot`                     |
| Color By                         | `colorBy`                      | Color By                               | `waterfallplot`                     | `.waterfallplot`                     |
| Color Scheme                     | `colorScheme`                  | Color Scheme                           | `waterfallplot`                     | `.waterfallplot`, `.generateGroupColors` |
| Bar Transparency                 | `barAlpha`                     | Bar Transparency                       | `waterfallplot`                     | `.waterfallplot`                     |
| Bar Width                        | `barWidth`                     | Bar Width                              | `waterfallplot`                     | `.waterfallplot`                     |
| Show Waterfall Plot              | `showWaterfallPlot`            | Show Waterfall Plot                    | `waterfallplot`                     | `.run`                               |
| Show Spider Plot                 | `showSpiderPlot`               | Show Spider Plot                       | `spiderplot`                        | `.run`                               |
| Spider Plot Color By             | `spiderColorBy`                | Spider Plot Color By                   | `spiderplot`                        | `.spiderplot`                        |
| Spider Plot Color Scheme         | `spiderColorScheme`            | Spider Plot Color Scheme               | `spiderplot`                        | `.spiderplot`, `.generateGroupColors` |
| **Clinical Metrics**             |                                |                                        |                                     |                                      |
| Response Categories Table        | `summaryTable`                 | Response Categories Based on RECIST v1.1 Criteria | `summaryTable`                      | `.calculateMetrics`                  |
| Clinical Response Metrics Table  | `clinicalMetrics`              | Clinical Response Metrics              | `clinicalMetrics`                   | `.calculateMetrics`, `.calculatePersonTimeMetrics` |
| Person-Time Analysis Table       | `personTimeTable`              | Person-Time Analysis                   | `personTimeTable`                   | `.calculatePersonTimeMetrics`        |
| **Data Export**                  |                                |                                        |                                     |                                      |
| Add Response Category to Data    | `addResponseCategory`          | Add Response Category to Data          | `addResponseCategory`               | `.run`                               |
