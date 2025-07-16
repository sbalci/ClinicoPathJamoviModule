# Comprehensive Tidy Plots Analysis Documentation

This document provides a comprehensive overview of the Comprehensive Tidy Plots Analysis module, detailing its features, user interface elements, and the underlying R functions.

## Feature Summary

The Comprehensive Tidy Plots module enables users to create publication-ready plots using the `tidyplots` framework. It offers extensive customization options, advanced statistical features, and a wide range of visualization capabilities. This module is designed to streamline scientific plotting within jamovi, providing a flexible and powerful tool for data visualization in clinical research.

## Feature Details

The following table provides a detailed mapping of the module's features, from the user interface to the underlying R functions.

| Feature                          | YAML Argument (`.a.yaml`)      | UI Label                               | Results Section (`.r.yaml`)         | R Function (`.b.R`)                  |
| -------------------------------- | ------------------------------ | -------------------------------------- | ----------------------------------- | ------------------------------------ |
| **Core Variables**               |                                |                                        |                                     |                                      |
| X Variable                       | `xvar`                         | X Variable                             | `plot`                              | `.run`, `.prepareData`, `.initializePlot` |
| Y Variable                       | `yvar`                         | Y Variable                             | `plot`                              | `.run`, `.prepareData`, `.initializePlot` |
| Color Variable                   | `color`                        | Color Variable                         | `plot`                              | `.run`, `.prepareData`, `.initializePlot` |
| Group Variable                   | `group`                        | Group Variable                         | `plot`                              | `.run`, `.prepareData`, `.initializePlot` |
| Facet Variable                   | `facet`                        | Facet Variable                         | `plot`                              | `.run`, `.prepareData`, `.applyFaceting` |
| **Plot Types**                   |                                |                                        |                                     |                                      |
| Main Plot Type                   | `plotType`                     | Main Plot Type                         | `plot`                              | `.addMainPlotElements`               |
| Point Style                      | `pointType`                    | Point Style                            | `plot`                              | `.addPointElements`                  |
| Line Type                        | `lineType`                     | Line Type                              | `plot`                              | `.addLineElements`                   |
| Bar Type                         | `barType`                      | Bar Type                               | `plot`                              | `.addBarElements`                    |
| Show Outlier Points (Boxplot)    | `showOutliers`                 | Show Outlier Points (Boxplot)          | `plot`                              | `.addBoxplotElements`                |
| Add Points to Violin Plot        | `violinPoints`                 | Add Points to Violin Plot              | `plot`                              | `.addViolinElements`                 |
| Histogram Bins                   | `histogramBins`                | Histogram Bins                         | `plot`                              | `.addHistogramElements`              |
| Area Type                        | `areaType`                     | Area Type                              | `plot`                              | `.addAreaElements`                   |
| **Statistical Elements**         |                                |                                        |                                     |                                      |
| Show Mean                        | `showMean`                     | Show Mean                              | `plot`                              | `.addStatisticalElements`            |
| Mean Display                     | `meanType`                     | Mean Display                           | `plot`                              | `.addStatisticalElements`            |
| Show Median                      | `showMedian`                   | Show Median                            | `plot`                              | `.addStatisticalElements`            |
| Median Display                   | `medianType`                   | Median Display                         | `plot`                              | `.addStatisticalElements`            |
| Show Standard Error              | `showSEM`                      | Show Standard Error                    | `plot`                              | `.addStatisticalElements`            |
| Show Standard Deviation          | `showSD`                       | Show Standard Deviation                | `plot`                              | `.addStatisticalElements`            |
| Show 95% Confidence Interval     | `showCI`                       | Show 95% Confidence Interval           | `plot`                              | `.addStatisticalElements`            |
| CI Display                       | `ciType`                       | CI Display                             | `plot`                              | `.addStatisticalElements`            |
| Show Range (Min-Max)             | `showRange`                    | Show Range (Min-Max)                   | `plot`                              | `.addStatisticalElements`            |
| Show Distribution                | `showDistribution`             | Show Distribution                      | `plot`                              | `.addDistributionElements`           |
| Distribution Type                | `distributionType`             | Distribution Type                      | `plot`                              | `.addDistributionElements`           |
| Show P-Values                    | `showPValue`                   | Show P-Values                          | `plot`                              | `.addStatisticalTests`               |
| Show Significance Asterisks      | `showSignificance`             | Show Significance Asterisks            | `plot`                              | `.addStatisticalTests`               |
| **Customization**                |                                |                                        |                                     |                                      |
| Color Scheme                     | `colorScheme`                  | Color Scheme                           | `plot`                              | `.applyColorScheme`                  |
| Transparency (0-1)               | `alpha`                        | Transparency (0-1)                     | `plot`                              | `.applyFinalAdjustments`             |
| Font Size                        | `fontSize`                     | Font Size                              | `plot`                              | `.applyTheme`                        |
| Plot Title                       | `plotTitle`                    | Plot Title                             | `plot`                              | `.applyLabels`                       |
| X Axis Label                     | `xLabel`                       | X Axis Label                           | `plot`                              | `.applyLabels`                       |
| Y Axis Label                     | `yLabel`                       | Y Axis Label                           | `plot`                              | `.applyLabels`                       |
| Legend Title                     | `legendTitle`                  | Legend Title                           | `plot`                              | `.applyLabels`                       |
| Remove Legend                    | `removeLegend`                 | Remove Legend                          | `plot`                              | `.applyTheme`                        |
| Remove Plot Padding              | `removePadding`                | Remove Plot Padding                    | `plot`                              | `.applyTheme`                        |
| Remove X Axis                    | `removeXAxis`                  | Remove X Axis                          | `plot`                              | `.applyAxisModifications`            |
| Remove X Axis Labels             | `removeXAxisLabels`            | Remove X Axis Labels                   | `plot`                              | `.applyAxisModifications`            |
| Remove X Axis Title              | `removeXAxisTitle`             | Remove X Axis Title                    | `plot`                              | `.applyAxisModifications`            |
| Remove Y Axis                    | `removeYAxis`                  | Remove Y Axis                          | `plot`                              | `.applyAxisModifications`            |
| Remove Y Axis Labels             | `removeYAxisLabels`            | Remove Y Axis Labels                   | `plot`                              | `.applyAxisModifications`            |
| Remove Y Axis Title              | `removeYAxisTitle`             | Remove Y Axis Title                    | `plot`                              | `.applyAxisModifications`            |
