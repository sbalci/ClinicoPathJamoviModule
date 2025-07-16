# Confidence Interval Approach for the Number of Subjects Required Documentation

This document provides a comprehensive overview of the Confidence Interval Approach for the Number of Subjects Required module, detailing its features, user interface elements, and the underlying R functions.

## Feature Summary

This module calculates the sample size for interobserver agreement studies using Cohen's kappa statistic based on a confidence interval approach. It helps researchers determine the required sample size to achieve a desired precision in estimating the kappa coefficient, which is crucial for designing reliable studies evaluating agreement between raters. The function supports 2-5 outcome categories and 2-5 raters, with customizable precision requirements and significance levels.

## Feature Details

The following table provides a detailed mapping of the module's features, from the user interface to the underlying R functions.

| Feature                          | YAML Argument (`.a.yaml`)      | UI Label                               | Results Section (`.r.yaml`)         | R Function (`.b.R`)                  |
| -------------------------------- | ------------------------------ | -------------------------------------- | ----------------------------------- | ------------------------------------ |
| **Input Parameters**             |                                |                                        |                                     |                                      |
| Number of outcome level          | `outcome`                      | Number of outcome level                | `text1`, `text2`                    | `.run`, `.calculateSampleSize`, `.generateExplanation` |
| kappa0                           | `kappa0`                       | kappa0                                 | `text1`, `text2`                    | `.run`, `.calculateSampleSize`, `.generateExplanation` |
| kappaL                           | `kappaL`                       | kappaL                                 | `text1`, `text2`                    | `.run`, `.calculateSampleSize`, `.generateExplanation` |
| kappaU                           | `kappaU`                       | kappaU                                 | `text1`, `text2`                    | `.run`, `.calculateSampleSize`, `.generateExplanation` |
| Proportions of outcome level     | `props`                        | Proportions of outcome level           | `text1`, `text2`                    | `.run`, `.parseProportions`, `.calculateSampleSize`, `.generateExplanation` |
| raters                           | `raters`                       | raters                                 | `text1`, `text2`                    | `.run`, `.calculateSampleSize`, `.generateExplanation` |
| alpha                            | `alpha`                        | alpha                                  | `text1`, `text2`                    | `.run`, `.calculateSampleSize`, `.generateExplanation` |
