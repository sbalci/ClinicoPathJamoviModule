# Biomarker Response Association Documentation

This document provides a comprehensive overview of the Biomarker Response Association module, detailing its features, user interface elements, and the underlying R functions.

## Feature Summary

The Biomarker Response Association module is designed to analyze and visualize the relationship between continuous biomarker measurements and treatment responses. It supports various response types (binary, categorical, continuous) and offers a range of statistical tests and plotting options to assess the predictive or prognostic value of biomarkers.

The module's features can be broadly categorized as follows:

*   **Biomarker-Response Analysis:** Explores the association between a continuous biomarker and a treatment response variable.
*   **Flexible Response Types:** Accommodates binary, categorical, and continuous response variables, applying appropriate statistical methods and visualizations.
*   **Threshold Analysis:** Allows for the determination and evaluation of biomarker thresholds, including optimal threshold calculation using ROC analysis for binary responses.
*   **Statistical Testing:** Performs relevant statistical tests (e.g., t-test, ANOVA, Wilcoxon, Kruskal-Wallis, Pearson/Spearman correlation) based on the response type.
*   **Visualization Options:** Generates various plot types (boxplot, scatter, violin, combined) with options for trend lines and threshold display.
*   **Data Preprocessing:** Includes options for log transformation and outlier handling to improve data quality and analysis robustness.

## Feature Details

The following table provides a detailed mapping of the module's features, from the user interface to the underlying R functions.

| Feature                          | YAML Argument (`.a.yaml`)      | UI Label                               | Results Section (`.r.yaml`)         | R Function (`.b.R`)                  |
| -------------------------------- | ------------------------------ | -------------------------------------- | ----------------------------------- | ------------------------------------ |
| **Input Variables**              |                                |                                        |                                     |                                      |
| Biomarker Variable               | `biomarker`                    | Biomarker                              | (Input)                             | `.run`, `.preprocessBiomarkerData`   |
| Response Variable                | `response`                     | Response                               | (Input)                             | `.run`                               |
| Response Type                    | `responseType`                 | Response Type                          | (Controls analysis flow)            | `.run`, `.performStatisticalTests`   |
| Grouping Variable                | `groupVariable`                | Grouping Variable                      | (Plot customization)                | `.run`, `.plot`                      |
| **Analysis Options**             |                                |                                        |                                     |                                      |
| Show Threshold                   | `showThreshold`                | Show Threshold                         | `threshold`                         | `.run`, `.plot`                      |
| Threshold Value                  | `thresholdValue`               | Threshold Value                        | (Parameter)                         | `.run`                               |
| Threshold Method                 | `thresholdMethod`              | Threshold Method                       | (Parameter)                         | `.run`, `.calculateOptimalThreshold` |
| Add Trend Line                   | `addTrendLine`                 | Add Trend Line                         | (Plot customization)                | `.plot`                              |
| Trend Method                     | `trendMethod`                  | Trend Method                           | (Parameter)                         | `.plot`                              |
| Perform Tests                    | `performTests`                 | Perform Statistical Tests              | `statisticalTests`                  | `.performStatisticalTests`           |
| Show Correlation                 | `showCorrelation`              | Show Correlation                       | `correlation`                       | `.run`                               |
| Log Transform                    | `logTransform`                 | Log Transform Biomarker                | (Data preprocessing)                | `.preprocessBiomarkerData`           |
| Outlier Handling                 | `outlierHandling`              | Outlier Handling                       | (Data preprocessing)                | `.run`, `.detectOutliers`            |
| Confidence Level                 | `confidenceLevel`              | Confidence Level                       | (Statistical tests)                 | `.performStatisticalTests`           |
| **Output Options**               |                                |                                        |                                     |                                      |
| Plot Type                        | `plotType`                     | Plot Type                              | `plot`                              | `.plot`                              |
| Instructions                     | (N/A)                          | Instructions                           | `todo`                              | `.run`                               |
| Correlation Analysis Table       | (N/A)                          | Correlation Analysis                   | `correlation`                       | `.run`                               |
| Threshold Analysis Table         | (N/A)                          | Threshold Analysis                     | `threshold`                         | `.run`                               |
| Group Comparison Table           | (N/A)                          | Response Group Comparison              | `groupComparison`                   | `.run`                               |
| Statistical Tests Table          | (N/A)                          | Statistical Tests                      | `statisticalTests`                  | `.run`                               |
