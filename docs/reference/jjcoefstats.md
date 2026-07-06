# Coefficient Forest Plot (ggcoefstats)

Create publication-ready forest plots for regression coefficients and
meta-analysis results. Supports both pre-computed coefficients and
automatic model fitting from raw data. Uses ggstatsplot's ggcoefstats()
for professional visualization.

## Usage

``` r
jjcoefstats(
  data,
  inputMode = "precomputed",
  term = NULL,
  estimate = NULL,
  stdError = NULL,
  confLow = NULL,
  confHigh = NULL,
  pValue = NULL,
  degreesOfFreedom,
  outcome = NULL,
  predictors = NULL,
  modelType = "lm",
  survivalTime = NULL,
  eventStatus = NULL,
  randomEffects = NULL,
  sortCoefs = FALSE,
  excludeIntercept = FALSE,
  showPValues = FALSE,
  pValueDisplay = "numeric",
  ciLevel = 0.95,
  exponentiate = FALSE,
  expScaleLabel = "exp",
  referenceValue = 0,
  colorScheme = "default",
  plotTheme = "default",
  plotWidth = 700,
  plotHeight = 500,
  showexplanations = FALSE
)
```

## Arguments

- data:

  The data as a data frame.

- inputMode:

  Whether to use pre-computed coefficients or fit a model from raw data.

- term:

  Variable containing coefficient names (for pre-computed mode).

- estimate:

  Coefficient estimates or log-odds ratios.

- stdError:

  Standard errors of estimates.

- confLow:

  Lower confidence interval bounds.

- confHigh:

  Upper confidence interval bounds.

- pValue:

  P-values for each coefficient.

- degreesOfFreedom:

  Degrees of freedom for t-distribution. If provided, t-distribution
  will be used for confidence intervals and p-values instead of normal
  approximation. Typically n - p - 1 for regression models (n = sample
  size, p = number of predictors).

- outcome:

  Outcome variable for model fitting.

- predictors:

  Predictor variables for model fitting.

- modelType:

  Type of regression model to fit.

- survivalTime:

  Time variable for Cox proportional hazards model.

- eventStatus:

  Event indicator for Cox model (0=censored, 1=event).

- randomEffects:

  Grouping variable for random intercepts in mixed effects models (e.g.,
  subject ID, cluster ID). Used to specify random effects term like
  (1\|subject).

- sortCoefs:

  Sort coefficients by estimate magnitude.

- excludeIntercept:

  Exclude intercept from the plot.

- showPValues:

  Display p-values alongside coefficients.

- pValueDisplay:

  How to display p-values in the table and plot.

- ciLevel:

  Confidence level for intervals.

- exponentiate:

  Exponentiate coefficients (for odds ratios or hazard ratios).

- expScaleLabel:

  Label to apply to exponentiated coefficients in pre-computed mode.

- referenceValue:

  Reference value for plotting (0 for differences, 1 for ratios; will
  auto-switch to 1 when exponentiating).

- colorScheme:

  Color palette for the plot.

- plotTheme:

  ggplot2 theme for the plot.

- plotWidth:

  Plot width in pixels.

- plotHeight:

  Plot height in pixels.

- showexplanations:

  Show explanations of the statistical results

## Value

A results object containing:

|                            |     |     |     |     |          |
|----------------------------|-----|-----|-----|-----|----------|
| `results$instructions`     |     |     |     |     | a html   |
| `results$about`            |     |     |     |     | a html   |
| `results$plainSummary`     |     |     |     |     | a html   |
| `results$coefPlot`         |     |     |     |     | an image |
| `results$modelSummary`     |     |     |     |     | a html   |
| `results$coefficientTable` |     |     |     |     | a table  |
| `results$modelMetrics`     |     |     |     |     | a table  |
| `results$notices`          |     |     |     |     | a html   |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$coefficientTable$asDF`

`as.data.frame(results$coefficientTable)`
