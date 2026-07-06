# Friedman Test for Non-Parametric Repeated Measures

Friedman test for non-parametric analysis of repeated measures data with
continuous or ordinal outcomes. This is the non-parametric alternative
to repeated measures ANOVA when normality assumptions are violated. It
tests whether the median values differ significantly across multiple
related measurements or time points.

## Usage

``` r
friedmantest(
  data,
  dependent,
  subject,
  within,
  method = "asymptotic",
  alpha = 0.05,
  posthoc = TRUE,
  posthoc_method = "nemenyi",
  correction = "bonferroni",
  effect_size = TRUE,
  confidence_level = 0.95,
  show_ranks = TRUE,
  show_descriptives = TRUE,
  show_assumptions = TRUE,
  clinical_interpretation = TRUE
)
```

## Arguments

- data:

  the data as a data frame

- dependent:

  Continuous or ordinal dependent variable measured repeatedly

- subject:

  Variable identifying subjects/cases for repeated measurements

- within:

  Factor variable indicating the repeated measure conditions/time points

- method:

  Method for calculating p-values

- alpha:

  Alpha level for hypothesis testing

- posthoc:

  Perform pairwise post-hoc comparisons when Friedman test is
  significant

- posthoc_method:

  Method for post-hoc pairwise comparisons

- correction:

  Multiple comparison correction method for post-hoc tests

- effect_size:

  Include effect size measures (Kendall's W coefficient of concordance)

- confidence_level:

  Confidence level for confidence intervals

- show_ranks:

  Show detailed rank analysis and mean rank comparisons

- show_descriptives:

  Show descriptive statistics for each condition

- show_assumptions:

  Assess assumptions and provide recommendations

- clinical_interpretation:

  Provide clinical interpretation guidance for results

## Value

A results object containing:

|                                  |     |     |     |     |          |
|----------------------------------|-----|-----|-----|-----|----------|
| `results$instructions`           |     |     |     |     | a html   |
| `results$dataInfo`               |     |     |     |     | a table  |
| `results$descriptiveStats`       |     |     |     |     | a table  |
| `results$friedmanTest`           |     |     |     |     | a table  |
| `results$effectSize`             |     |     |     |     | a table  |
| `results$rankAnalysis`           |     |     |     |     | a table  |
| `results$pairwiseComparisons`    |     |     |     |     | a table  |
| `results$assumptionAssessment`   |     |     |     |     | a table  |
| `results$clinicalInterpretation` |     |     |     |     | a table  |
| `results$boxplotByCondition`     |     |     |     |     | an image |
| `results$meanRankPlot`           |     |     |     |     | an image |
| `results$pairwiseComparisonPlot` |     |     |     |     | an image |
| `results$profilePlot`            |     |     |     |     | an image |
| `results$methodExplanation`      |     |     |     |     | a html   |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$dataInfo$asDF`

`as.data.frame(results$dataInfo)`
