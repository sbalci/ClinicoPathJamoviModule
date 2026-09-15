# Distribution Selection and Goodness-of-Fit

Distribution Selection and Goodness-of-Fit

## Usage

``` r
distributionfit(
  elapsedtime,
  outcome,
  explanatory,
  outcomeLevel = "1",
  test_weibull = TRUE,
  test_exponential = TRUE,
  test_lognormal = TRUE,
  test_loglogistic = TRUE,
  test_gamma = TRUE,
  test_gengamma = FALSE,
  test_genf = FALSE,
  selection_method = "aic",
  gof_tests = "all_tests",
  confidence_level = 0.95,
  bootstrap_gof = TRUE,
  bootstrap_samples = 1000,
  show_comparison_table = TRUE,
  show_gof_table = TRUE,
  show_parameter_table = TRUE,
  show_survival_plot = TRUE,
  show_hazard_plot = TRUE,
  show_pp_plot = TRUE,
  show_qq_plot = TRUE,
  showSummaries = FALSE,
  showExplanations = FALSE
)
```

## Arguments

- elapsedtime:

  Time to event or censoring

- outcome:

  Event indicator (1 = event, 0 = censored)

- explanatory:

  Explanatory variables for modeling

- outcomeLevel:

  Level indicating event occurrence

- test_weibull:

  Include Weibull distribution in comparison

- test_exponential:

  Include Exponential distribution in comparison

- test_lognormal:

  Include Log-normal distribution in comparison

- test_loglogistic:

  Include Log-logistic distribution in comparison

- test_gamma:

  Include Gamma distribution in comparison

- test_gengamma:

  Include Generalized Gamma distribution in comparison

- test_genf:

  Include Generalized F distribution in comparison

- selection_method:

  Model selection criterion

- gof_tests:

  Goodness-of-fit tests to perform

- confidence_level:

  Confidence level for intervals and tests

- bootstrap_gof:

  Use bootstrap for goodness-of-fit tests

- bootstrap_samples:

  Number of bootstrap samples

- show_comparison_table:

  Display model comparison statistics

- show_gof_table:

  Display goodness-of-fit test results

- show_parameter_table:

  Display parameter estimates for best model

- show_survival_plot:

  Display survival curves for all distributions

- show_hazard_plot:

  Display hazard curves for all distributions

- show_pp_plot:

  Display probability-probability plots

- show_qq_plot:

  Display quantile-quantile plots

- showSummaries:

  Generate natural language summaries

- showExplanations:

  Show methodology explanations

## Value

A results object containing:

|                               |     |     |     |     |          |
|-------------------------------|-----|-----|-----|-----|----------|
| `results$todo`                |     |     |     |     | a html   |
| `results$modelComparison`     |     |     |     |     | a table  |
| `results$goodnessOfFit`       |     |     |     |     | a table  |
| `results$parameterEstimates`  |     |     |     |     | a table  |
| `results$distributionSummary` |     |     |     |     | a table  |
| `results$survivalPlot`        |     |     |     |     | an image |
| `results$hazardPlot`          |     |     |     |     | an image |
| `results$ppPlot`              |     |     |     |     | an image |
| `results$qqPlot`              |     |     |     |     | an image |
| `results$diagnosticPlots`     |     |     |     |     | an image |
| `results$analysisSummary`     |     |     |     |     | a html   |
| `results$methodExplanation`   |     |     |     |     | a html   |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$modelComparison$asDF`

`as.data.frame(results$modelComparison)`
