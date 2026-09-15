# Mantel-Haenszel Stratified Analysis

Mantel-Haenszel Stratified Analysis

## Usage

``` r
mantelhaenszel(
  data,
  rows,
  cols,
  strata,
  measure = "or",
  alpha = 0.05,
  show_stratum_tables = TRUE,
  show_homogeneity_tests = TRUE,
  show_crude_analysis = TRUE,
  continuity_correction = TRUE,
  show_methodology = FALSE,
  show_references = FALSE
)
```

## Arguments

- data:

  .

- rows:

  .

- cols:

  .

- strata:

  .

- measure:

  .

- alpha:

  .

- show_stratum_tables:

  .

- show_homogeneity_tests:

  .

- show_crude_analysis:

  .

- continuity_correction:

  .

- show_methodology:

  .

- show_references:

  .

## Value

A results object containing:

|                             |     |     |     |     |         |
|-----------------------------|-----|-----|-----|-----|---------|
| `results$mh_summary`        |     |     |     |     | a table |
| `results$crude_analysis`    |     |     |     |     | a table |
| `results$homogeneity_tests` |     |     |     |     | a table |
| `results$stratum_specific`  |     |     |     |     | a table |
| `results$interpretation`    |     |     |     |     | a html  |
| `results$methodology`       |     |     |     |     | a html  |
| `results$references`        |     |     |     |     | a html  |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$mh_summary$asDF`

`as.data.frame(results$mh_summary)`
