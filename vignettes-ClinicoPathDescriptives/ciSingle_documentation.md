# Confidence Intervals for Mean Values Documentation

This document provides a comprehensive overview of the Confidence Intervals for Mean Values module, detailing its features, user interface elements, and the underlying R functions.

## Feature Summary

The Confidence Intervals for Mean Values module calculates and visualizes confidence intervals for one or more continuous variables. It offers various methods for interval estimation (t-distribution, normal approximation, bootstrap) and provides diagnostic information, including normality tests, to help users assess the assumptions underlying the chosen method.

The module's features can be broadly categorized as follows:

*   **Confidence Interval Calculation:** Computes confidence intervals for the mean of selected continuous variables.
*   **Multiple Estimation Methods:** Supports t-distribution (default), normal approximation (for large samples), and bootstrap methods for robust interval estimation.
*   **Grouping Variable Support:** Allows calculation of confidence intervals for different groups within the data.
*   **Diagnostic Information:** Provides Shapiro-Wilk test results for normality and indicates whether assumptions are met.
*   **Visualization:** Generates a plot to visually represent the calculated confidence intervals.

## Feature Details

The following table provides a detailed mapping of the module's features, from the user interface to the underlying R functions.

| Feature                          | YAML Argument (`.a.yaml`)      | UI Label                               | Results Section (`.r.yaml`)         | R Function (`.b.R`)                  |
| -------------------------------- | ------------------------------ | -------------------------------------- | ----------------------------------- | ------------------------------------ |
| **Input Variables**              |                                |                                        |                                     |                                      |
| Dependent Variables              | `deps`                         | Variables                              | `citable`, `diagnostics`, `plot`    | `.calculateCI`, `.testNormality`     |
| Split By Variable                | `splitBy`                      | Split By                               | `citable`, `diagnostics`, `plot`    | `.run`                               |
| **Confidence Interval Options**  |                                |                                        |                                     |                                      |
| CI Width                         | `ciWidth`                      | Confidence Interval Width              | `conflevel`, `citable`              | `.calculateCI`                       |
| Method                           | `method`                       | Method                                 | `conflevel`, `citable`              | `.calculateCI`                       |
| Bootstrap Samples                | `bootstrapSamples`             | Bootstrap Samples                      | (Internal parameter)                | `.calculateCI`                       |
| **Output Options**               |                                |                                        |                                     |                                      |
| Show Plot                        | `showPlot`                     | Display Confidence Interval Plot       | `plot`                              | `.plot`                              |
| Show Diagnostics                 | `showDiagnostics`              | Display Diagnostic Information         | `diagnostics`                       | `.testNormality`                     |
