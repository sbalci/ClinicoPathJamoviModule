# Page's Trend Test for Ordered Alternatives

Page's trend test for detecting ordered alternatives in repeated
measures data. This test is used when there is a specific predicted
ordering of the treatment effects or time points, making it more
powerful than the general Friedman test when the alternative hypothesis
specifies a trend or ordered pattern.

## Usage

``` r
pagetrendtest(
  data,
  dependent,
  subject,
  within,
  trend_direction = "increasing",
  custom_order = "",
  method = "asymptotic",
  alpha = 0.05,
  effect_size = TRUE,
  confidence_level = 0.95,
  show_ranks = TRUE,
  show_descriptives = TRUE,
  friedman_comparison = TRUE,
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
  (must be orderable)

- trend_direction:

  Expected direction of trend across conditions

- custom_order:

  Custom ordering of factor levels when trend_direction is 'custom'
  (e.g., 'baseline,week1,month1,month6')

- method:

  Method for calculating p-values

- alpha:

  Alpha level for hypothesis testing

- effect_size:

  Include effect size measures (Page's L coefficient and trend strength)

- confidence_level:

  Confidence level for confidence intervals

- show_ranks:

  Show detailed rank analysis and trend components

- show_descriptives:

  Show descriptive statistics for each condition

- friedman_comparison:

  Include comparison with standard Friedman test

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
| `results$trendOrdering`          |     |     |     |     | a table  |
| `results$descriptiveStats`       |     |     |     |     | a table  |
| `results$pageTrendTest`          |     |     |     |     | a table  |
| `results$effectSize`             |     |     |     |     | a table  |
| `results$rankAnalysis`           |     |     |     |     | a table  |
| `results$friedmanComparison`     |     |     |     |     | a table  |
| `results$assumptionAssessment`   |     |     |     |     | a table  |
| `results$clinicalInterpretation` |     |     |     |     | a table  |
| `results$trendPlot`              |     |     |     |     | an image |
| `results$rankTrendPlot`          |     |     |     |     | an image |
| `results$comparisonPlot`         |     |     |     |     | an image |
| `results$profilePlot`            |     |     |     |     | an image |
| `results$methodExplanation`      |     |     |     |     | a html   |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$dataInfo$asDF`

`as.data.frame(results$dataInfo)`
