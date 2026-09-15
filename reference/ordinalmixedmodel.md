# Ordinal Mixed Models (Cumulative Link)

Cumulative link mixed models (CLMM) for ordinal response data with
random effects. Appropriate for Likert-scale data, ordered categorical
outcomes (e.g., tumor grades, disease severity), and repeated-measures
designs with ordinal endpoints. Uses the ordinal package for fitting
cumulative link mixed models with logit, probit, or complementary
log-log links.

## Usage

``` r
ordinalmixedmodel(
  data,
  dep,
  fixedFactors = NULL,
  fixedCovs = NULL,
  randomTerms,
  link = "logit",
  threshold = "flexible",
  propOddsTest = TRUE,
  oddsRatios = TRUE,
  confInt = TRUE,
  confLevel = 0.95,
  randomEffectsTable = TRUE,
  thresholdsTable = TRUE,
  modelFit = TRUE,
  conditionMeans = FALSE
)
```

## Arguments

- data:

  the data as a data frame

- dep:

  Ordinal dependent variable (e.g., Likert scale, tumor grade, severity
  level)

- fixedFactors:

  Categorical fixed-effect predictors

- fixedCovs:

  Continuous fixed-effect predictors

- randomTerms:

  Random effects grouping variables (e.g., subject ID, rater, site)

- link:

  Link function for the cumulative model

- threshold:

  Threshold parameterization

- propOddsTest:

  Test the proportional odds (parallel lines) assumption using
  nominal_test

- oddsRatios:

  Display exponentiated coefficients as odds ratios

- confInt:

  Show confidence intervals for coefficients

- confLevel:

  Confidence level for intervals

- randomEffectsTable:

  Show random effects variance components

- thresholdsTable:

  Show threshold (intercept) coefficients

- modelFit:

  Show AIC, BIC, log-likelihood, and deviance

- conditionMeans:

  Show predicted marginal probabilities for each factor level

## Value

A results object containing:

|                             |     |     |     |     |         |
|-----------------------------|-----|-----|-----|-----|---------|
| `results$instructions`      |     |     |     |     | a html  |
| `results$modelFitTable`     |     |     |     |     | a table |
| `results$fixedEffects`      |     |     |     |     | a table |
| `results$thresholds`        |     |     |     |     | a table |
| `results$randomEffects`     |     |     |     |     | a table |
| `results$propOddsTestTable` |     |     |     |     | a table |
| `results$conditionProbs`    |     |     |     |     | a table |
| `results$methodExplanation` |     |     |     |     | a html  |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$modelFitTable$asDF`

`as.data.frame(results$modelFitTable)`
