# Summary Statistics with summarytools Documentation

This document provides a comprehensive overview of the Summary Statistics with summarytools module, detailing its features, user interface elements, and the underlying R functions.

## Feature Summary

This module offers comprehensive descriptive statistics using the `summarytools` package. It provides professional data frame summaries, frequency tables, and descriptive statistics with publication-ready output for clinical research. The module supports various analysis types, including `dfSummary` for a comprehensive dataset overview, `freq` for frequency distributions, `descr` for detailed descriptive statistics, and `ctable` for cross-tabulations. It also includes options for grouping, weighting, and extensive customization of output appearance.

## Feature Details

The following table provides a detailed mapping of the module's features, from the user interface to the underlying R functions.

| Feature                          | YAML Argument (`.a.yaml`)      | UI Label                               | Results Section (`.r.yaml`)         | R Function (`.b.R`)                  |
| -------------------------------- | ------------------------------ | -------------------------------------- | ----------------------------------- | ------------------------------------ |
| **Analysis Type & Variables**    |                                |                                        |                                     |                                      |
| Analysis Type                    | `analysis_type`                | Analysis Type                          | `instructions`, `summary_output`, `data_summary_table`, `frequency_table`, `descriptive_stats`, `crosstab_output`, `interpretation` | `.run`, `.runDfSummary`, `.runFrequency`, `.runDescriptive`, `.runCrossTable`, `.generateInstructions`, `.generateInterpretation` |
| Variables to Analyze             | `vars`                         | Variables to Analyze                   | `summary_output`, `data_summary_table`, `frequency_table`, `descriptive_stats` | `.run`, `.runDfSummary`, `.runFrequency`, `.runDescriptive` |
| Grouping Variable                | `group_var`                    | Grouping Variable                      | `summary_output`                    | `.runDfSummary`, `.runFrequency`, `.runDescriptive` |
| Weights Variable                 | `weights_var`                  | Weights Variable                       | `summary_output`                    | `.runCrossTable`                     |
| First Variable (for Cross-table) | `cross_var1`                   | First Variable (for Cross-table)       | `summary_output`, `crosstab_output` | `.runCrossTable`                     |
| Second Variable (for Cross-table)| `cross_var2`                   | Second Variable (for Cross-table)      | `summary_output`, `crosstab_output` | `.runCrossTable`                     |
| **Output Customization**         |                                |                                        |                                     |                                      |
| Show Variable Labels             | `show_labels`                  | Show Variable Labels                   | `summary_output`                    | `.runDfSummary`                      |
| Show Variable Numbers            | `show_variable_numbers`        | Show Variable Numbers                  | `summary_output`                    | `.runDfSummary`                      |
| Show Distribution Graphs         | `show_graphs`                  | Show Distribution Graphs               | `summary_output`                    | `.runDfSummary`                      |
| Show Valid Counts                | `show_valid_counts`            | Show Valid Counts                      | `summary_output`                    | `.runDfSummary`                      |
| Show Missing Data Info           | `show_na_counts`               | Show Missing Data Info                 | `summary_output`                    | `.runDfSummary`                      |
| Decimal Places                   | `round_digits`                 | Decimal Places                         | `summary_output`                    | `.runDfSummary`, `.runFrequency`, `.runDescriptive` |
| Max Distinct Values to Show      | `max_distinct_values`          | Max Distinct Values to Show            | `summary_output`                    | Not implemented in `.b.R`            |
| Include Cumulative Frequencies   | `include_cumulative`           | Include Cumulative Frequencies         | `summary_output`                    | `.runFrequency`                      |
| Report Missing Values            | `report_missing`               | Report Missing Values                  | `summary_output`                    | `.runFrequency`                      |
| Transpose Output                 | `transpose_output`             | Transpose Output                       | `summary_output`                    | `.runDescriptive`                    |
| Statistics to Include            | `stats_to_include`             | Statistics to Include                  | `summary_output`                    | `.runDescriptive`, `.getSelectedStats` |
| Include Mean                     | `include_mean`                 | Include Mean                           | `summary_output`                    | `.getSelectedStats`                  |
| Include Median                   | `include_median`               | Include Median                         | `summary_output`                    | `.getSelectedStats`                  |
| Include Mode                     | `include_mode`                 | Include Mode                           | `summary_output`                    | `.getSelectedStats`                  |
| Include Standard Deviation       | `include_sd`                   | Include Standard Deviation             | `summary_output`                    | `.getSelectedStats`                  |
| Include Variance                 | `include_var`                  | Include Variance                       | `summary_output`                    | `.getSelectedStats`                  |
| Include Range                    | `include_range`                | Include Range                          | `summary_output`                    | `.getSelectedStats`                  |
| Include Quartiles                | `include_quartiles`            | Include Quartiles                      | `summary_output`                    | `.getSelectedStats`                  |
| Include Skewness                 | `include_skewness`             | Include Skewness                       | `summary_output`                    | `.getSelectedStats`                  |
| Include Kurtosis                 | `include_kurtosis`             | Include Kurtosis                       | `summary_output`                    | `.getSelectedStats`                  |
| Cross-table Proportions          | `cross_proportions`            | Cross-table Proportions                | `summary_output`, `crosstab_output` | `.runCrossTable`                     |
| Output Style                     | `output_style`                 | Output Style                           | `summary_output`                    | `.runDfSummary`, `.runFrequency`, `.runDescriptive`, `.runCrossTable` |
| Plain ASCII Output               | `plain_ascii`                  | Plain ASCII Output                     | `summary_output`                    | `.runDfSummary`, `.runFrequency`, `.runDescriptive`, `.runCrossTable` |
| Include Headings                 | `headings`                     | Include Headings                       | `summary_output`                    | `.runDfSummary`, `.runFrequency`, `.runDescriptive`, `.runCrossTable` |
| Escape Pipe Characters           | `escape_pipe`                  | Escape Pipe Characters                 | `summary_output`                    | Not implemented in `.b.R`            |
| Bootstrap CSS                    | `bootstrap_css`                | Bootstrap CSS                          | `summary_output`                    | `.runDfSummary`, `.runFrequency`, `.runDescriptive`, `.runCrossTable` |
| Custom CSS                       | `custom_css`                   | Custom CSS                             | `summary_output`                    | Not implemented in `.b.R`            |
| **Interpretation**               |                                |                                        |                                     |                                      |
| Show Interpretation Guide        | `show_interpretation`          | Show Interpretation Guide              | `interpretation`                    | `.generateInterpretation`            |
