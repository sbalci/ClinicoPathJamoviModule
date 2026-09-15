# Cochran's Q Test for Paired Categorical Data

Cochran's Q test for analyzing paired categorical (binary) data with
more than two time points or conditions. This is the extension of
McNemar's test for repeated measures with binary outcomes, commonly used
in clinical studies to test for changes in treatment response or
diagnostic agreement across multiple time points.

## Usage

``` r
cochranq(
  data,
  variables,
  id,
  method = "asymptotic",
  alpha = 0.05,
  posthoc = TRUE,
  correction = "bonferroni",
  effect_size = TRUE,
  confidence_level = 0.95,
  show_pattern = TRUE,
  show_assumptions = TRUE,
  clinical_interpretation = TRUE
)
```

## Arguments

- data:

  the data as a data frame

- variables:

  Binary variables measured at different time points or conditions
  (minimum 3 variables required)

- id:

  Optional subject identifier for paired data structure validation

- method:

  Method for calculating p-values

- alpha:

  Alpha level for hypothesis testing

- posthoc:

  Perform pairwise post-hoc comparisons when Q test is significant

- correction:

  Multiple comparison correction method for post-hoc tests

- effect_size:

  Include effect size measures (Kendall's W coefficient of concordance)

- confidence_level:

  Confidence level for confidence intervals

- show_pattern:

  Show detailed response pattern analysis and frequencies

- show_assumptions:

  Test assumptions for Cochran's Q test validity

- clinical_interpretation:

  Provide clinical interpretation guidance for results

## Value

A results object containing:

|                                   |     |     |     |     |          |
|-----------------------------------|-----|-----|-----|-----|----------|
| `results$instructions`            |     |     |     |     | a html   |
| `results$dataInfo`                |     |     |     |     | a table  |
| `results$cochranQTest`            |     |     |     |     | a table  |
| `results$effectSize`              |     |     |     |     | a table  |
| `results$responsePatterns`        |     |     |     |     | a table  |
| `results$pairwiseComparisons`     |     |     |     |     | a table  |
| `results$marginalFrequencies`     |     |     |     |     | a table  |
| `results$assumptionTesting`       |     |     |     |     | a table  |
| `results$clinicalInterpretation`  |     |     |     |     | a table  |
| `results$responsePatternPlot`     |     |     |     |     | an image |
| `results$marginalProportionsPlot` |     |     |     |     | an image |
| `results$pairwiseComparisonPlot`  |     |     |     |     | an image |
| `results$methodExplanation`       |     |     |     |     | a html   |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$dataInfo$asDF`

`as.data.frame(results$dataInfo)`
