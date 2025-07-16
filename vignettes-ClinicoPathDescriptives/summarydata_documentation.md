# Summary of Continuous Variables Analysis Documentation

This document provides a comprehensive overview of the Summary of Continuous Variables Analysis module, detailing its features, user interface elements, and the underlying R functions.

## Feature Summary

This module generates descriptive statistics for continuous variables. It provides a textual summary of key statistics (mean, standard deviation, median, min, max) and an optional visual summary table. A core feature is the ability to include distribution diagnostics, which compute and explain normality (Shapiro-Wilk test), skewness, and kurtosis, aiding in understanding the underlying data distribution.

## Feature Details

The following table provides a detailed mapping of the module's features, from the user interface to the underlying R functions.

| Feature                          | YAML Argument (`.a.yaml`)      | UI Label                               | Results Section (`.r.yaml`)         | R Function (`.b.R`)                  |
| -------------------------------- | ------------------------------ | -------------------------------------- | ----------------------------------- | ------------------------------------ |
| **Core Summary**                 |                                |                                        |                                     |                                      |
| Variables                        | `vars`                         | Variables                              | `text`, `text1`                     | `.run`, `.mysummary`                 |
| **Distribution Diagnostics**     |                                |                                        |                                     |                                      |
| Distribution Diagnostics         | `distr`                        | Distribution Diagnostics               | `text`                              | `.mysummary`                         |
