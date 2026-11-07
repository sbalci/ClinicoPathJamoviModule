# Positive Predictive Value Calculator Analysis Documentation

> **Not yet released in meddecide.** This article documents `ppv`, which is not part of the
> meddecide 1.0.4 release. It is documented here ahead of a future release, so the
> options, defaults and output described below are subject to change and the analysis
> will not appear in your jamovi menu yet. For the analyses that ship today see the
> [meddecide articles index](https://www.serdarbalci.com/meddecide/articles/).

This document provides a comprehensive overview of the Positive Predictive Value Calculator module, detailing its features, user interface elements, and the underlying R functions.

## Feature Summary

This module calculates the Positive Predictive Value (PPV) and False Discovery Rate (FDR) for research findings based on the framework described by Ioannidis (2005). It helps researchers understand the probability that their claimed findings are actually true given various study characteristics such as prior probability of true relationships, Type I error rate (alpha level), statistical power, and the proportion of p-hacked or biased studies. The module provides a clear summary of results, including a confusion matrix and a visual representation of study outcomes.

## Feature Details

The following table provides a detailed mapping of the module's features, from the user interface to the underlying R functions.

| Feature                          | YAML Argument (`.a.yaml`)      | UI Label                               | Results Section (`.r.yaml`)         | R Function (`.b.R`)                  |
| -------------------------------- | ------------------------------ | -------------------------------------- | ----------------------------------- | ------------------------------------ |
| **Input Parameters**             |                                |                                        |                                     |                                      |
| Percentage of a priori true hypotheses| `percTrue`                     | Percentage of a priori true hypotheses | `confusion`, `ppv`, `dotPlot`       | `.run`, `.compute`                   |
| α level                          | `alpha`                        | α level                                | `confusion`, `ppv`, `dotPlot`       | `.run`, `.compute`                   |
| Power (1-β)                      | `power`                        | Power (1-β)                            | `confusion`, `ppv`, `dotPlot`       | `.run`, `.compute`                   |
| Percentage of p-hacked studies   | `percHack`                     | Percentage of p-hacked studies         | `confusion`, `ppv`, `dotPlot`       | `.run`, `.compute`                   |
| **Output Tables**                |                                |                                        |                                     |                                      |
| Percentages Table                | `confusion`                    | Percentages                            | `confusion`                         | `.populateConfusionTable`            |
| Positive Predictive Value HTML   | `ppv`                          | Positive Predictive Value              | `ppv`                               | `.populatePpv`                       |
| **Visualizations**               |                                |                                        |                                     |                                      |
| Study Outcomes Dot Plot          | `dotPlot`                      | Study Outcomes                         | `dotPlot`                           | `.prepareDotPlot`, `.dotPlot`        |
