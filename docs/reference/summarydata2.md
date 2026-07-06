# Enhanced Descriptives

This module generates descriptive statistics for continuous variables.
It provides both a textual summary and a visually appealing summary
table. Optionally, you can enable distribution diagnostics to examine
normality, skewness, and kurtosis.

## Usage

``` r
summarydata2(
  data,
  vars,
  date_vars = NULL,
  distr = FALSE,
  summary_format = "standard",
  grvar = NULL,
  pivot_layout = "clinical",
  include_confidence = TRUE,
  advanced_metrics = FALSE,
  pivot_export = FALSE,
  summarytools_graphs = TRUE,
  summarytools_round_digits = 2
)
```

## Arguments

- data:

  The data as a data frame.

- vars:

  a string naming the variables from `data` that contains the continuous
  values used for the report

- date_vars:

  Variables containing date/time data to be analyzed with date-specific
  statistics (similar to sumvar's dist_date function)

- distr:

  If TRUE, additional distribution diagnostics (Shapiro-Wilk test,
  skewness, and kurtosis) will be computed and explained.

- summary_format:

  Choose the format for summary statistics display. New summarytools
  options provide publication-ready automated EDA summaries with
  embedded visualizations.

- grvar:

  Optional grouping variable to stratify the summary statistics by
  categories.

- pivot_layout:

  Layout style for pivottabler enhanced summaries.

- include_confidence:

  Include confidence intervals in pivot summary tables.

- advanced_metrics:

  Include advanced metrics like IQR, MAD, and robust statistics.

- pivot_export:

  Enable enhanced export capabilities for pivot tables.

- summarytools_graphs:

  Include histograms and bar charts in summarytools dfSummary output.

- summarytools_round_digits:

  Number of decimal places for summarytools output.

## Value

A results object containing:

|                             |     |     |     |     |        |
|-----------------------------|-----|-----|-----|-----|--------|
| `results$todo`              |     |     |     |     | a html |
| `results$text`              |     |     |     |     | a html |
| `results$text1`             |     |     |     |     | a html |
| `results$pivot_summary`     |     |     |     |     | a html |
| `results$pivot_export_info` |     |     |     |     | a html |
