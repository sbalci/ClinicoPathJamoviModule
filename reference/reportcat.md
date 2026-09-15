# Summary of Categorical Variables

Generates a detailed summary of categorical variables including counts,
percentages, and missing value information. The output is presented in
both textual and visual formats, making it easy to interpret the
distribution of your data.

## Usage

``` r
reportcat(data, vars = NULL)
```

## Arguments

- data:

  The data as a data frame.

- vars:

  Select the variables from your data frame that you wish to summarize.
  Only categorical variables (nominal, ordinal, or factors) are allowed.

## Value

A results object containing:

|                           |     |     |     |     |        |
|---------------------------|-----|-----|-----|-----|--------|
| `results$todo`            |     |     |     |     | a html |
| `results$clinicalSummary` |     |     |     |     | a html |
| `results$aboutAnalysis`   |     |     |     |     | a html |
| `results$assumptions`     |     |     |     |     | a html |
| `results$text`            |     |     |     |     | a html |
| `results$text1`           |     |     |     |     | a html |
| `results$reportSentences` |     |     |     |     | a html |
| `results$error`           |     |     |     |     | a html |
