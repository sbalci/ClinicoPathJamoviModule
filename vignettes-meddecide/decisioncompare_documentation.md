# Compare Medical Decision Tests Analysis Documentation

This document provides a comprehensive overview of the Compare Medical Decision Tests module, detailing its features, user interface elements, and the underlying R functions.

## Feature Summary

The Compare Medical Decision Tests module is a powerful tool for evaluating and comparing the performance of up to three diagnostic tests against a common gold standard. It calculates key metrics such as sensitivity, specificity, predictive values, and likelihood ratios. The module also provides statistical comparisons between tests using McNemar's test and confidence intervals for differences, along with various visualization options.

The module's features can be broadly categorized as follows:

*   **Test Performance Metrics:** Calculation of sensitivity, specificity, accuracy, positive predictive value (PPV), negative predictive value (NPV), positive likelihood ratio (LR+), and negative likelihood ratio (LR-).
*   **Confidence Intervals:** Option to display 95% confidence intervals for performance metrics.
*   **Statistical Comparison:** McNemar's test for comparing paired diagnostic tests and confidence intervals for differences in metrics.
*   **Data Visualization:** Bar plots and radar plots for visual comparison of test performance.
*   **Original Data Display:** Option to show frequency tables and cross-tabulations of original data.

## Feature Details

The following table provides a detailed mapping of the module's features, from the user interface to the underlying R functions.

| Feature                          | YAML Argument (`.a.yaml`)      | UI Label                               | Results Section (`.r.yaml`)         | R Function (`.b.R`)                  |
| -------------------------------- | ------------------------------ | -------------------------------------- | ----------------------------------- | ------------------------------------ |
| **Core Analysis**                |                                |                                        |                                     |                                      |
| Golden Standard                  | `gold`                         | Golden Standard                        | `text1`, `text2`, `cTable1`, `cTable2`, `cTable3`, `comparisonTable` | `.run`                               |
| Gold Standard Positive Level     | `goldPositive`                 | Positive Level                         | `cTable1`, `cTable2`, `cTable3`     | `.run`                               |
| Test 1                           | `test1`                        | Test 1                                 | `cTable1`, `epirTable1`, `comparisonTable`, `mcnemarTable`, `diffTable`, `plot1`, `plotRadar` | `.run`                               |
| Test 1 Positive Level            | `test1Positive`                | Positive Level                         | `cTable1`                           | `.run`                               |
| Test 2                           | `test2`                        | Test 2                                 | `cTable2`, `epirTable2`, `comparisonTable`, `mcnemarTable`, `diffTable`, `plot1`, `plotRadar` | `.run`                               |
| Test 2 Positive Level            | `test2Positive`                | Positive Level                         | `cTable2`                           | `.run`                               |
| Test 3                           | `test3`                        | Test 3                                 | `cTable3`, `epirTable3`, `comparisonTable`, `mcnemarTable`, `diffTable`, `plot1`, `plotRadar` | `.run`                               |
| Test 3 Positive Level            | `test3Positive`                | Positive Level                         | `cTable3`                           | `.run`                               |
| Prior Probability (prevalence)   | `pp`, `pprob`                  | Prior Probability (prevalence)         | `comparisonTable`                   | (Not directly implemented in .b.R for this feature) |
| Original Data                    | `od`                           | Original Data                          | `text1`, `text2`                    | `.run`                               |
| Footnotes                        | `fnote`                        | Footnotes                              | `cTable1`, `cTable2`, `cTable3`, `comparisonTable` | `.run`                               |
| 95% CI                           | `ci`                           | 95% CI                                 | `epirTable1`, `epirTable2`, `epirTable3` | `.run`                               |
| Comparison Plot                  | `plot`                         | Comparison Plot                        | `plot1`                             | `.plot1`                             |
| Radar Plot                       | `radarplot`                    | Radar Plot                             | `plotRadar`                         | `.plotRadar`                         |
| Statistical Comparison           | `statComp`                     | Statistical Comparison                 | `mcnemarTable`, `diffTable`         | `.run`                               |
