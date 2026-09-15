# Enhanced Cross Tables with danchaltiel/crosstable

Enhanced cross-tabulation analysis using the danchaltiel/crosstable
package for advanced clinical research functionality.

## Usage

``` r
enhancedcrosstable(
  data,
  vars,
  by_var,
  percent_pattern = "col_percent",
  show_total = TRUE,
  show_total_row = TRUE,
  test_auto = TRUE,
  effect_size = FALSE,
  funs_arg = "mean_sd",
  digits = 2,
  use_labels = TRUE,
  exclude_missing = TRUE,
  show_n_col = TRUE,
  margin = "column",
  cor_method = "pearson",
  showNA = "no",
  compact = FALSE,
  export_format = "html",
  show_interpretation = TRUE
)
```

## Arguments

- data:

  The data as a data frame.

- vars:

  Variables to include in the cross-tabulation (table rows).

- by_var:

  The grouping variable for cross-tabulation (table columns).

- percent_pattern:

  Pattern for displaying counts and percentages.

- show_total:

  Whether to include a total column.

- show_total_row:

  Whether to include a total row.

- test_auto:

  Whether to automatically select and perform statistical tests.

- effect_size:

  Whether to calculate effect sizes (Cramer's V, odds ratios, etc.).

- funs_arg:

  Function for summarizing continuous variables.

- digits:

  Number of decimal places for numeric results.

- use_labels:

  Whether to use variable labels if available.

- exclude_missing:

  Whether to exclude missing values from calculations.

- show_n_col:

  Whether to show sample sizes for each column.

- margin:

  Margin for calculating percentages.

- cor_method:

  Correlation method for continuous variables.

- showNA:

  Whether and when to show missing values in the table.

- compact:

  Whether to use compact table formatting.

- export_format:

  Format for table export and display.

- show_interpretation:

  Whether to include interpretation of statistical results.

## Value

A results object containing:

|                            |     |     |     |     |        |
|----------------------------|-----|-----|-----|-----|--------|
| `results$instructions`     |     |     |     |     | a html |
| `results$crosstable_main`  |     |     |     |     | a html |
| `results$statistics_table` |     |     |     |     | a html |
| `results$effect_sizes`     |     |     |     |     | a html |
| `results$interpretation`   |     |     |     |     | a html |
| `results$export_data`      |     |     |     |     | a html |
| `results$summary_stats`    |     |     |     |     | a html |
