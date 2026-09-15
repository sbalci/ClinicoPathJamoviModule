# Bayesian Decision Curve Analysis

Bayesian Decision Curve Analysis

## Usage

``` r
bayesdca(
  data,
  outcomes,
  outcomePos,
  predictors,
  thresholdMin = 0.01,
  thresholdMax = 0.5,
  thresholdPoints = 50,
  useExternalPrevalence = FALSE,
  externalCases = 100,
  externalTotal = 500,
  bayesianAnalysis = TRUE,
  priorStrength = 2,
  bootstrapCI = TRUE,
  bootstrapReps = 2000,
  calculateEVPI = FALSE,
  nDraws = 2000,
  directionIndicator = ">=",
  calculateIC = FALSE
)
```

## Arguments

- data:

  The data as a data frame.

- outcomes:

  Binary outcome variable (0/1) representing the true disease status or
  event.

- outcomePos:

  Specifies which level of the outcome variable should be treated as the
  positive class.

- predictors:

  Variables containing either probability predictions from models or
  binary results (0/1) from diagnostic tests.

- thresholdMin:

  Minimum decision threshold for the analysis.

- thresholdMax:

  Maximum decision threshold for the analysis.

- thresholdPoints:

  Number of threshold points to evaluate.

- useExternalPrevalence:

  Use external prevalence data instead of sample prevalence.

- externalCases:

  Number of cases in external prevalence data.

- externalTotal:

  Total sample size in external prevalence data.

- bayesianAnalysis:

  Perform Bayesian analysis with uncertainty quantification.

- priorStrength:

  Strength of prior (effective sample size).

- bootstrapCI:

  Calculate bootstrap confidence intervals for non-Bayesian analysis.

- bootstrapReps:

  Number of bootstrap replications for confidence intervals.

- calculateEVPI:

  Calculate Expected Value of Perfect Information.

- nDraws:

  Number of posterior draws for Bayesian analysis.

- directionIndicator:

  Direction of classification relative to the cutpoint. Use '\>=' when
  higher values predict positive outcomes.

- calculateIC:

  Calculate model comparison metrics (DIC, WAIC) for Bayesian analysis.

## Value

A results object containing:

|                                |     |     |     |     |                    |
|--------------------------------|-----|-----|-----|-----|--------------------|
| `results$instructions`         |     |     |     |     | a html             |
| `results$summary`              |     |     |     |     | a html             |
| `results$netBenefitTable`      |     |     |     |     | a table            |
| `results$modelResults`         |     |     |     |     | an array of tables |
| `results$comparisonTable`      |     |     |     |     | a table            |
| `results$evpiTable`            |     |     |     |     | a table            |
| `results$modelComparisonTable` |     |     |     |     | a table            |
| `results$mainPlot`             |     |     |     |     | an image           |
| `results$deltaPlot`            |     |     |     |     | an image           |
| `results$probPlot`             |     |     |     |     | an image           |
| `results$evpiPlot`             |     |     |     |     | an image           |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$netBenefitTable$asDF`

`as.data.frame(results$netBenefitTable)`
