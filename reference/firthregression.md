# Firth's Penalized Likelihood Regression

Firth's penalized likelihood regression for both logistic (binary
outcome) and Cox proportional hazards (survival) models. Addresses
small-sample bias, complete/quasi-complete separation, and rare events
by adding a Jeffreys-prior penalty to the likelihood. Provides profile
likelihood confidence intervals, penalized likelihood ratio tests, and
separation diagnostics. Particularly valuable in clinical settings with
low event counts, unbalanced predictors, or when standard maximum
likelihood estimates become infinite due to separation.

## Usage

``` r
firthregression(
  data,
  analysisType = "logistic",
  time,
  outcome,
  outcomeLevel,
  predictors,
  suitabilityCheck = TRUE,
  ciLevel = 0.95,
  ciMethod = "profile",
  separationCheck = TRUE,
  compareStandard = TRUE,
  showModelFit = TRUE,
  forestPlot = TRUE,
  separationPlot = FALSE,
  showSummary = FALSE,
  showExplanations = FALSE
)
```

## Arguments

- data:

  The data as a data frame.

- analysisType:

  Type of regression model. Logistic uses logistf package for binary
  outcomes with Jeffreys-prior bias reduction. Cox uses coxphf package
  for survival outcomes with Firth-type penalized partial likelihood.

- time:

  Time to event variable (numeric). Required only for Cox regression.
  For right-censored data, this is the time from study entry to event or
  censoring.

- outcome:

  Outcome variable. For logistic regression, a binary variable. For Cox
  regression, the event indicator (0 = censored, 1 = event).

- outcomeLevel:

  Level of the outcome variable to be treated as the event of interest.

- predictors:

  Variables to include in the model. Can include continuous, ordinal,
  and nominal variables.

- suitabilityCheck:

  Assess if data is suitable for the selected Firth regression model.

- ciLevel:

  Confidence level for confidence intervals. Default is 0.95 for 95
  percent confidence intervals.

- ciMethod:

  Method for computing confidence intervals. Profile likelihood CIs are
  recommended as they are more accurate, especially with small samples
  and near separation. Wald CIs are faster but can be unreliable in
  these situations.

- separationCheck:

  Check for complete and quasi-complete separation in the data.
  Separation causes standard maximum likelihood to fail with infinite
  coefficient estimates.

- compareStandard:

  Fit the standard (unpenalized) model alongside Firth's penalized model
  to show bias reduction. Useful for assessing how much the penalty
  changes the estimates.

- showModelFit:

  Display model fit statistics including log-likelihood and AIC.

- forestPlot:

  Forest plot showing odds ratios (logistic) or hazard ratios (Cox) with
  confidence intervals on a log scale.

- separationPlot:

  Diagnostic visualization showing data distributions by outcome for
  each predictor. Helps identify separation patterns.

- showSummary:

  Natural-language summary of results.

- showExplanations:

  Explanatory text about Firth's method and when to use it.

## Value

A results object containing:

|                                 |     |     |     |     |          |
|---------------------------------|-----|-----|-----|-----|----------|
| `results$instructions`          |     |     |     |     | a html   |
| `results$notices`               |     |     |     |     | a html   |
| `results$suitabilityReport`     |     |     |     |     | a html   |
| `results$coefficients`          |     |     |     |     | a table  |
| `results$modelFit`              |     |     |     |     | a table  |
| `results$separationDiagnostics` |     |     |     |     | a table  |
| `results$comparisonTable`       |     |     |     |     | a table  |
| `results$forestPlotImage`       |     |     |     |     | an image |
| `results$separationPlotImage`   |     |     |     |     | an image |
| `results$summaryText`           |     |     |     |     | a html   |
| `results$explanationText`       |     |     |     |     | a html   |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$coefficients$asDF`

`as.data.frame(results$coefficients)`
