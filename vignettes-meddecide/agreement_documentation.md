# Interrater Reliability Analysis Documentation

This document provides a comprehensive overview of the Interrater Reliability Analysis module, detailing its features, user interface elements, and the underlying R functions.

## Feature Summary

The Interrater Reliability Analysis module is designed for comprehensive analysis of agreement between multiple raters or observers, with specialized features for pathology applications. It includes various statistical measures and visualizations to assess the consistency and accuracy of ratings.

The module's features can be broadly categorized as follows:

*   **Core Agreement Metrics:** Calculation of Cohen's kappa, Fleiss' kappa, and Intraclass Correlation Coefficients (ICC).
*   **Advanced Agreement Metrics:** Krippendorff's Alpha for generalized reliability.
*   **Pathology-Specific Analysis:** Diagnostic accuracy metrics, diagnostic style clustering, and identification of discordant cases.
*   **Detailed Analysis:** Pairwise rater analysis, category-specific agreement, and outlier case analysis.
*   **Visualization:** Heatmaps, dendrograms, and frequency tables for better understanding of agreement patterns.

## Feature Details

The following table provides a detailed mapping of the module's features, from the user interface to the underlying R functions.

| Feature                          | YAML Argument (`.a.yaml`)      | UI Label                               | Results Section (`.r.yaml`)         | R Function (`.b.R`)                  |
| -------------------------------- | ------------------------------ | -------------------------------------- | ----------------------------------- | ------------------------------------ |
| **Core Agreement Metrics**       |                                |                                        |                                     |                                      |
| Raters/Observers                 | `vars`                         | Raters/Observers                       | `overviewTable`                     | `.prepareData`                       |
| Weighted Kappa                   | `wght`                         | Weighted Kappa (Ordinal Variables only)| `kappaTable`                        | `.performCohensKappa`                |
| Exact Kappa                      | `exct`                         | Exact Kappa (>=3 Variables)            | `kappaTable`                        | `.performFleissKappa`                |
| Intraclass Correlation Coefficient| `icc`                          | Intraclass Correlation Coefficient     | `iccTable`                          | `.performICCAnalysis`                |
| ICC Type                         | `iccType`                      | ICC Type                               | `iccTable`                          | `.performICCAnalysis`                |
| Confidence Level                 | `confidenceLevel`              | Confidence Level                       | `kappaTable`, `iccTable`            | `.performCohensKappa`, `.performFleissKappa`, `.performICCAnalysis`, `.performPairwiseAnalysis` |
| Minimum Acceptable Agreement     | `minAgreement`                 | Minimum Acceptable Agreement           | `overviewTable`                     | Not implemented in `.b.R`            |
| **Advanced Agreement Metrics**   |                                |                                        |                                     |                                      |
| Krippendorff's Alpha             | `kripp`                        | Krippendorff's Alpha                   | `krippTable`                        | `.performKrippendorffAnalysis`       |
| Data Type for Krippendorff's Alpha| `krippMethod`                  | Data Type for Krippendorff's Alpha     | `krippTable`                        | `.performKrippendorffAnalysis`       |
| Bootstrap Confidence Intervals   | `bootstrap`                    | Bootstrap Confidence Intervals         | `krippTable`                        | `.performKrippendorffAnalysis`       |
| **Pathology-Specific Analysis**  |                                |                                        |                                     |                                      |
| Pathology-Specific Analysis      | `pathologyContext`             | Pathology-Specific Analysis            | `diagnosticAccuracyTable`           | `.performPathologyAnalysis`          |
| True Diagnosis (Optional)        | `diagnosisVar`                 | True Diagnosis (Optional)              | `diagnosticAccuracyTable`           | `.performPathologyAnalysis`          |
| Diagnostic Style Clustering      | `diagnosticStyleAnalysis`      | Diagnostic Style Clustering            | `diagnosticStyleTable`              | `.performDiagnosticStyleAnalysis`    |
| Style Clustering Method          | `styleClusterMethod`           | Style Clustering Method                | `diagnosticStyleTable`              | `.performDiagnosticStyleAnalysis`    |
| Style Distance Metric            | `styleDistanceMetric`          | Style Distance Metric                  | `diagnosticStyleTable`              | `.performDiagnosticStyleAnalysis`    |
| Number of Style Groups           | `numberOfStyleGroups`          | Number of Style Groups                 | `diagnosticStyleTable`              | `.performDiagnosticStyleAnalysis`    |
| Identify Discordant Cases        | `identifyDiscordantCases`      | Identify Discordant Cases              | `discordantCasesTable`              | `.identifyDiscordantCases`           |
| Include Rater Characteristics    | `raterCharacteristics`         | Include Rater Characteristics          | `diagnosticStyleTable`              | `.performDiagnosticStyleAnalysis`    |
| Experience Variable (Optional)   | `experienceVar`                | Experience Variable (Optional)         | `diagnosticStyleTable`              | `.performDiagnosticStyleAnalysis`    |
| Training Institution Variable (Optional)| `trainingVar`                  | Training Institution Variable (Optional)| `diagnosticStyleTable`              | `.performDiagnosticStyleAnalysis`    |
| Current Institution Variable (Optional)| `institutionVar`               | Current Institution Variable (Optional)| `diagnosticStyleTable`              | `.performDiagnosticStyleAnalysis`    |
| Specialty Variable (Optional)    | `specialtyVar`                 | Specialty Variable (Optional)          | `diagnosticStyleTable`              | `.performDiagnosticStyleAnalysis`    |
| **Detailed Analysis**            |                                |                                        |                                     |                                      |
| Outlier Case Analysis            | `outlierAnalysis`              | Outlier Case Analysis                  | `outlierTable`                      | `.performOutlierAnalysis`            |
| Pairwise Rater Analysis          | `pairwiseAnalysis`             | Pairwise Rater Analysis                | `pairwiseTable`                     | `.performPairwiseAnalysis`           |
| Category-Specific Analysis       | `categoryAnalysis`             | Category-Specific Analysis             | `categoryTable`                     | `.performCategoryAnalysis`           |
| **Visualization**                |                                |                                        |                                     |                                      |
| Frequency Tables                 | `sft`                          | Frequency Tables                       | `frequencyTables`                   | `.generateFrequencyTables`           |
| Agreement Heatmap                | `heatmap`                      | Agreement Heatmap                      | `heatmapPlot`                       | `.heatmapPlot`                       |
| Show Detailed Heatmap            | `heatmapDetails`               | Show Detailed Heatmap                  | `heatmapPlot`                       | Not implemented in `.b.R`            |
| Pairwise Agreement Plot          | `pairwiseAnalysis`             | Pairwise Rater Analysis                | `pairwisePlot`                      | `.pairwisePlot`                      |
| Category Agreement Plot          | `categoryAnalysis`             | Category-Specific Analysis             | `categoryPlot`                      | `.categoryPlot`                      |
| Confusion Matrix                 | `pathologyContext`             | Pathology-Specific Analysis            | `confusionMatrixPlot`               | `.confusionMatrixPlot`               |
| Diagnostic Style Dendrogram      | `diagnosticStyleAnalysis`      | Diagnostic Style Clustering            | `diagnosticStyleDendrogram`         | `.diagnosticStyleDendrogram`        |
| Diagnostic Style Heatmap         | `diagnosticStyleAnalysis`      | Diagnostic Style Clustering            | `diagnosticStyleHeatmap`            | `.diagnosticStyleHeatmap`            |
| **Interpretation**               |                                |                                        |                                     |                                      |
| Show Interpretation Guidelines   | `showInterpretation`           | Show Interpretation Guidelines         | `interpretationTable`               | `.showInterpretationGuidelines`      |
