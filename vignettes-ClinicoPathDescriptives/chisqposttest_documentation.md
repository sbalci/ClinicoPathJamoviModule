# Chi-squared Post-Hoc Test Documentation

This document provides a comprehensive overview of the Chi-squared Post-Hoc Test module (chisqposttest), detailing its features, user interface elements, and the underlying R functions.

## Feature Summary

The chisqposttest module is a specialized tool for performing post-hoc analyses following a significant chi-squared test of independence. When a chi-squared test indicates a significant association between two categorical variables with more than two categories, this module helps identify which specific pairs of categories contribute to that significance.

The module's features can be broadly categorized as follows:

*   **Core Post-Hoc Testing:** Perform pairwise comparisons of proportions or frequencies after a significant chi-squared test.
*   **P-value Adjustment:** Apply multiple comparison corrections (e.g., Bonferroni, Holm) to control the family-wise error rate.
*   **Effect Size Calculation:** Compute effect sizes for pairwise comparisons.
*   **Visualization:** Generate plots to visualize the results of pairwise comparisons.
*   **Export Options:** Capabilities to save the post-hoc test results and plots in various formats.

## Feature Details

The following table provides a detailed mapping of the module's features, from the user interface to the underlying R functions.

| Feature                          | YAML Argument (`.a.yaml`)      | UI Label                               | Results Section (`.r.yaml`)         | R Function (`.b.R`)                  |
| -------------------------------- | ------------------------------ | -------------------------------------- | ----------------------------------- | ------------------------------------ |
| **Core Analysis**                |                                |                                        |                                     |                                      |
| Row Variable                     | `rowVar`                       | Row Variable                           | `postHocOverview`                   | `.performPairwiseChiSq`              |
| Column Variable                  | `colVar`                       | Column Variable                        | `postHocOverview`                   | `.performPairwiseChiSq`              |
| **Post-Hoc Options**             |                                |                                        |                                     |                                      |
| Show Pairwise Comparisons        | `showPairwiseComparisons`      | Show Pairwise Comparisons              | `pairwiseResults`                   | `.performPairwiseChiSq`              |
| P-value Adjustment Method        | `pAdjustMethod`                | P-value Adjustment Method              | `pairwiseResults`                   | `.adjustPValues`                     |
| Show Effect Sizes                | `showEffectSizes`              | Show Effect Sizes                      | `pairwiseResults`                   | `.calculateEffectSizes`              |
| **Visualizations**               |                                |                                        |                                     |                                      |
| Show Comparison Plot             | `showComparisonPlot`           | Show Comparison Plot                   | `comparisonPlot`                    | `.plotPairwiseComparisons`           |
| **Advanced Options**             |                                |                                        |                                     |                                      |
| Missing Data Handling            | `missingDataHandling`          | Missing Data Handling                  | `advancedOptions`                   | `.handleMissingData`                 |
| Export Results                   | `exportResults`                | Export Results                         | `exportOptions`                     | `.exportPostHocResults`              |
| Export Plot                      | `exportPlot`                   | Export Plot                            | `exportOptions`                     | `.exportPostHocPlot`                 |