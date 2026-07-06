# Odds Ratio Table and Plot

Function for Odds Ratio Table and Plot.

## Usage

``` r
oddsratio(
  data,
  explanatory,
  outcome,
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

  Specify which outcome level represents the positive case for
  likelihood ratio calculations. If not specified, the function will use
  the second level alphabetically.

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

  Display an interactive nomogram for converting pre-test to post-test
  probabilities using likelihood ratios calculated from the data.

- showExplanations:

  Display educational explanations for each analysis type to help
  interpret odds ratios, risk ratios, diagnostic test performance, ROC
  analysis, and likelihood ratios.

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
