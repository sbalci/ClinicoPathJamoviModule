# Table One

This function generates a "Table One", a descriptive summary table
frequently used in clinicopathological research manuscripts. It supports
multiple output styles for flexible formatting.

## Usage

``` r
tableone(
  data,
  vars = NULL,
  sty = "t1",
  excl = FALSE,
  showSummary = FALSE,
  showAbout = FALSE,
  showReportSentence = FALSE
)
```

## Arguments

- data:

  The input data as a data frame.

- vars:

  A set of variable names from `data` to include in the Table One.
  Supports numeric, ordinal, and categorical variables.

- sty:

  Specify the output style for the descriptive table. 'tableone'
  provides standard medical format with means/medians and frequencies,
  'gtsummary' creates publication-ready descriptive tables, 'arsenal'
  generates comprehensive descriptive summaries, and 'janitor' produces
  simple frequency tables for categorical variables.

- excl:

  Boolean option to exclude missing values (NA) from the analysis. Note:
  Exclusion may remove entire cases.

- showSummary:

  Show detailed summary including sample size, missing data patterns,
  and data quality metrics.

- showAbout:

  Show educational content explaining Table One, when to use it, and how
  to interpret results.

- showReportSentence:

  Generate a copy-ready sentence summarizing the table for manuscript
  reporting.

## Value

A results object containing:

|                          |     |     |     |     |                |
|--------------------------|-----|-----|-----|-----|----------------|
| `results$todo`           |     |     |     |     | a html         |
| `results$tablestyle1`    |     |     |     |     | a preformatted |
| `results$tablestyle2`    |     |     |     |     | a html         |
| `results$tablestyle3`    |     |     |     |     | a html         |
| `results$tablestyle4`    |     |     |     |     | a html         |
| `results$reportSentence` |     |     |     |     | a html         |
| `results$summary`        |     |     |     |     | a html         |
| `results$about`          |     |     |     |     | a html         |
| `results$assumptions`    |     |     |     |     | a html         |
