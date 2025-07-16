# Contingency Tables Documentation

This document provides a comprehensive overview of the Contingency Tables module, detailing its features, user interface elements, and the underlying R functions.

## Feature Summary

The Contingency Tables module is used to analyze the relationship between two or more categorical variables. It performs various statistical tests, calculates association measures, and displays observed and expected counts, along with percentages, to assess the independence or association between variables.

The module's features can be broadly categorized as follows:

*   **Table Construction:** Generates contingency tables from selected row, column, and optional layer variables, with support for pre-counted data.
*   **Association Tests:** Provides Chi-squared tests (with and without continuity correction), Likelihood Ratio tests, and Fisher's exact test.
*   **Measures of Association:** Calculates Contingency Coefficient, Phi and Cramer's V, Log Odds Ratio, Odds Ratio, Relative Risk, Gamma, and Kendall's Tau-b.
*   **Confidence Intervals:** Offers confidence intervals for comparative measures.
*   **Display Options:** Allows display of observed counts, expected counts, row percentages, column percentages, and total percentages.

## Feature Details

The following table provides a detailed mapping of the module's features, from the user interface to the underlying R functions.

| Feature                          | YAML Argument (`.a.yaml`)      | UI Label                               | Results Section (`.r.yaml`)         | R Function (`.b.R`)                  |
| -------------------------------- | ------------------------------ | -------------------------------------- | ----------------------------------- | ------------------------------------ |
| **Table Setup**                  |                                |                                        |                                     |                                      |
| Rows Variable                    | `rows`                         | Rows                                   | `freqs`                             | `.cleanData`, `.matrices`            |
| Columns Variable                 | `cols`                         | Columns                                | `freqs`                             | `.cleanData`, `.matrices`            |
| Counts Variable                  | `counts`                       | Counts                                 | `freqs`                             | `.cleanData`, `.matrices`            |
| Layers Variables                 | `layers`                       | Layers                                 | `freqs`                             | `.cleanData`, `.matrices`            |
| **Association Tests**            |                                |                                        |                                     |                                      |
| Chi-squared Test                 | `chiSq`                        | Chi-squared                            | `chiSq`                             | `chisq.test`                         |
| Chi-squared (Continuity Corrected) | `chiSqCorr`                    | Chi-squared (Continuity Correction)    | `chiSq`                             | `chisq.test`                         |
| Likelihood Ratio                 | `likeRat`                      | Likelihood Ratio                       | `chiSq`                             | `vcd::assocstats`                    |
| Fisher's Exact Test              | `fisher`                       | Fisher's Exact Test                    | `chiSq`                             | `stats::fisher.test`                 |
| **Measures of Association**      |                                |                                        |                                     |                                      |
| Contingency Coefficient          | `contCoef`                     | Contingency Coefficient                | `nom`                               | `vcd::assocstats`                    |
| Phi and Cramer's V               | `phiCra`                       | Phi and Cramer's V                     | `nom`                               | `vcd::assocstats`                    |
| Log Odds Ratio                   | `logOdds`                      | Log Odds Ratio                         | `odds`                              | `vcd::loddsratio`                    |
| Odds Ratio                       | `odds`                         | Odds Ratio                             | `odds`                              | `vcd::loddsratio`                    |
| Relative Risk                    | `relRisk`                      | Relative Risk                          | `odds`                              | `.relativeRisk`                      |
| Confidence Intervals             | `ci`                           | Confidence Intervals                   | `odds`, `gamma`                     | (various functions)                  |
| Confidence Interval Width        | `ciWidth`                      | Confidence Interval Width              | `odds`, `gamma`                     | (various functions)                  |
| Gamma                            | `gamma`                        | Gamma                                  | `gamma`                             | `vcdExtra::GKgamma`                  |
| Kendall's Tau-b                  | `taub`                         | Kendall's Tau-b                        | `taub`                              | `cor.test(method='kendall')`         |
| **Display Options**              |                                |                                        |                                     |                                      |
| Observed Counts                  | `obs`                          | Observed Counts                        | `freqs`                             | (internal calculation)               |
| Expected Counts                  | `exp`                          | Expected Counts                        | `freqs`                             | (internal calculation)               |
| Row Percentages                  | `pcRow`                        | Row Percentages                        | `freqs`                             | (internal calculation)               |
| Column Percentages               | `pcCol`                        | Column Percentages                     | `freqs`                             | (internal calculation)               |
| Total Percentages                | `pcTot`                        | Total Percentages                      | `freqs`                             | (internal calculation)               |
