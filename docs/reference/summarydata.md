# Summary of Continuous Variables

This module generates descriptive statistics for continuous variables.
It provides both a textual summary and a visually appealing summary
table. Optionally, you can enable distribution diagnostics to examine
normality, skewness, and kurtosis.

## Usage

``` r
summarydata(
  data,
  vars = NULL,
  distr = FALSE,
  decimal_places = 2,
  outliers = FALSE,
  report_sentences = FALSE
)
```

## Arguments

- data:

  The data as a data frame.

- vars:

  a string naming the variables from `data` that contains the continuous
  values used for the report

- distr:

  If TRUE, additional distribution diagnostics (Shapiro-Wilk test,
  skewness, and kurtosis) will be computed and explained.

- decimal_places:

  Number of decimal places to display for statistical measures. Default
  of 2 aligns with standard laboratory reporting.

- outliers:

  If TRUE, detect and report potential outliers using IQR method.
  Helpful for quality control and identifying data entry errors.

- report_sentences:

  If TRUE, generate copy-ready clinical report sentences for direct use
  in medical documentation.

## Value

A results object containing:

|                                  |     |     |     |     |        |
|----------------------------------|-----|-----|-----|-----|--------|
| `results$todo`                   |     |     |     |     | a html |
| `results$text`                   |     |     |     |     | a html |
| `results$text1`                  |     |     |     |     | a html |
| `results$clinicalInterpretation` |     |     |     |     | a html |
| `results$aboutAnalysis`          |     |     |     |     | a html |
| `results$outlierReport`          |     |     |     |     | a html |
| `results$reportSentences`        |     |     |     |     | a html |
| `results$glossary`               |     |     |     |     | a html |
