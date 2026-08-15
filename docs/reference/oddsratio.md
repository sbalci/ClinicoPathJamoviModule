# Odds Ratio Table and Plot

Logistic regression odds-ratio table, forest plot, prediction nomogram,
and optional binary diagnostic metrics.

## Usage

``` r
oddsratio(
  data,
  explanatory = NULL,
  outcome = NULL,
  outcomeLevel,
  diagnosticPredictor = NULL,
  predictorLevel,
  usePenalized = FALSE,
  showNomogram = FALSE,
  showExplanations = FALSE
)
```

## Arguments

- data:

  The data as a data frame.

- explanatory:

  The explanatory variables to be used in the analysis.

- outcome:

  The outcome variable to be used in the analysis.

- outcomeLevel:

  Specify which outcome level is modeled as the event in logistic
  regression and used as positive for diagnostic metrics. A positive
  outcome level is required; the analysis stops with an error if it is
  not specified.

- diagnosticPredictor:

  Specify the predictor to drive likelihood ratios; must be binary.
  Defaults to the first explanatory variable.

- predictorLevel:

  Specify which level of the diagnostic predictor represents the
  positive case.

- usePenalized:

  Use Firth penalized likelihood logistic regression. This is
  recommended when there is separation (zero cells), small sample sizes,
  or low events-per-variable.

- showNomogram:

  Display a prediction nomogram from the maximum-likelihood logistic
  model and, when a binary diagnostic predictor is available, separate
  unadjusted sensitivity, specificity, and likelihood-ratio estimates.
  The prediction nomogram is not generated when Firth penalized
  regression is selected.

- showExplanations:

  Display educational explanations for odds ratios, their distinction
  from risk ratios, binary diagnostic-test performance, likelihood
  ratios, and prediction nomograms.

## Value

A results object containing:

|                                       |     |     |     |     |          |
|---------------------------------------|-----|-----|-----|-----|----------|
| `results$todo`                        |     |     |     |     | a html   |
| `results$errors`                      |     |     |     |     | a html   |
| `results$strongWarnings`              |     |     |     |     | a html   |
| `results$warnings`                    |     |     |     |     | a html   |
| `results$infoMessages`                |     |     |     |     | a html   |
| `results$text`                        |     |     |     |     | a html   |
| `results$text2`                       |     |     |     |     | a html   |
| `results$plot`                        |     |     |     |     | an image |
| `results$oddsRatioExplanation`        |     |     |     |     | a html   |
| `results$riskMeasuresExplanation`     |     |     |     |     | a html   |
| `results$diagnosticTestExplanation`   |     |     |     |     | a html   |
| `results$plot_nomogram`               |     |     |     |     | an image |
| `results$diagnosticMetrics`           |     |     |     |     | a html   |
| `results$nomogram`                    |     |     |     |     | a html   |
| `results$nomogramAnalysisExplanation` |     |     |     |     | a html   |
