# Diagnostic Test Sample Size Calculator

Diagnostic Test Sample Size Calculator

## Usage

``` r
diagnosticsamplesize(
  study_purpose = "diagnostic",
  target_sensitivity = 0.9,
  target_specificity = 0.9,
  prevalence = 0.1,
  ci_width = 0.1,
  alpha = 0.05,
  nonresponse_rate = 0,
  show_statement = TRUE,
  show_methodology = TRUE,
  show_references = FALSE,
  show_comparison_table = FALSE,
  comparison_prevalences = "0.05, 0.10, 0.20, 0.30, 0.50",
  show_method_comparison = FALSE
)
```

## Arguments

- study_purpose:

  .

- target_sensitivity:

  .

- target_specificity:

  .

- prevalence:

  .

- ci_width:

  .

- alpha:

  .

- nonresponse_rate:

  .

- show_statement:

  .

- show_methodology:

  .

- show_references:

  .

- show_comparison_table:

  .

- comparison_prevalences:

  .

- show_method_comparison:

  .

## Value

A results object containing:

|                                 |     |     |     |     |         |
|---------------------------------|-----|-----|-----|-----|---------|
| `results$sample_size_summary`   |     |     |     |     | a table |
| `results$sample_size_statement` |     |     |     |     | a html  |
| `results$prevalence_table`      |     |     |     |     | a table |
| `results$method_comparison`     |     |     |     |     | a table |
| `results$methodology`           |     |     |     |     | a html  |
| `results$references`            |     |     |     |     | a html  |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$sample_size_summary$asDF`

`as.data.frame(results$sample_size_summary)`
