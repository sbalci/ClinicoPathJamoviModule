# Benford's Law Analysis Documentation

This document provides a comprehensive overview of the Benford's Law Analysis module, detailing its features, user interface elements, and the underlying R functions.

## Feature Summary

The Benford's Law Analysis module is a tool for forensic accounting, auditing, and fraud detection. It tests whether the distribution of the first digits of a numeric variable conforms to Benford's Law, which can indicate data manipulation or anomalies. The module provides the Benford's Law distribution, identifies potential suspects, and visualizes the observed vs. expected digit frequencies.

The module's features can be broadly categorized as follows:

*   **Benford's Law Conformance Test:** Compares the observed first digit distribution of a numeric variable against Benford's Law.
*   **Suspect Identification:** Highlights data points that significantly deviate from the expected Benford distribution, indicating potential anomalies.
*   **Statistical Output:** Presents the Benford's Law distribution and statistical summaries.
*   **Visualizations:** Generates a plot comparing observed and expected first digit frequencies.

## Feature Details

The following table provides a detailed mapping of the module's features, from the user interface to the underlying R functions.

| Feature                          | YAML Argument (`.a.yaml`)      | UI Label                               | Results Section (`.r.yaml`)         | R Function (`.b.R`)                  |
| -------------------------------- | ------------------------------ | -------------------------------------- | ----------------------------------- | ------------------------------------ |
| **Core Analysis**                |                                |                                        |                                     |                                      |
| Variable to Analyze              | `var`                          | Variable                               | (Input)                             | `benford.analysis::benford`          |
| **Output Options**               |                                |                                        |                                     |                                      |
| Benford Analysis Output          | (N/A)                          | Benford Analysis                       | `text`                              | `benford.analysis::benford`          |
| Suspects Output                  | (N/A)                          | Suspects                               | `text2`                             | `benford.analysis::getSuspects`      |
| Benford Analysis Plot            | (N/A)                          | Benford Analysis Plot                  | `plot`                              | `.plot`                              |
| Instructions                     | (N/A)                          | Instructions                           | `todo`                              | (Internal message)                   |
