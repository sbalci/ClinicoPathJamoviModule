# Lowest Expected Value for a Fixed Sample Size Documentation

This document provides a comprehensive overview of the Lowest Expected Value for a Fixed Sample Size module, detailing its features, user interface elements, and the underlying R functions.

## Feature Summary

This module calculates the lowest expected value for kappa agreement given a fixed sample size. It helps researchers determine the detectable agreement level with their available resources, which is crucial for assessing the feasibility and power of inter-rater agreement studies. The function supports 2-5 outcome categories and 2-5 raters, using the `kappaSize` package implementation of power calculations for different scenarios.

## Feature Details

The following table provides a detailed mapping of the module's features, from the user interface to the underlying R functions.

| Feature                          | YAML Argument (`.a.yaml`)      | UI Label                               | Results Section (`.r.yaml`)         | R Function (`.b.R`)                  |
| -------------------------------- | ------------------------------ | -------------------------------------- | ----------------------------------- | ------------------------------------ |
| **Input Parameters**             |                                |                                        |                                     |                                      |
| Number of outcome level          | `outcome`                      | Number of outcome level                | `text1`, `text2`                    | `.run`, `.calculateKappaFixedN`, `.generateExplanation` |
| kappa0                           | `kappa0`                       | kappa0                                 | `text1`, `text2`                    | `.run`, `.calculateKappaFixedN`, `.generateExplanation` |
| Proportions of outcome level     | `props`                        | Proportions of outcome level           | `text1`, `text2`                    | `.run`, `.parseProportions`, `.calculateKappaFixedN`, `.generateExplanation` |
| raters                           | `raters`                       | raters                                 | `text1`, `text2`                    | `.run`, `.calculateKappaFixedN`, `.generateExplanation` |
| alpha                            | `alpha`                        | alpha                                  | `text1`, `text2`                    | `.run`, `.calculateKappaFixedN`, `.generateExplanation` |
| Sample size                      | `n`                            | N                                      | `text1`, `text2`                    | `.run`, `.calculateKappaFixedN`, `.generateExplanation` |
