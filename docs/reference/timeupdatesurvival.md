# Time-Updated Survival Estimates

Time-updated survival estimates using time-varying regression
coefficients and dynamic hazard functions. Incorporates real-time
patient information updates for precision medicine and personalized
survival prediction.

## Usage

``` r
timeupdatesurvival(
  data,
  time,
  status,
  covariates,
  id,
  update_times = "0.5, 1, 2, 5",
  method = "aalen_additive",
  smoothing_method = "lowess",
  bandwidth = 0.5,
  degrees_freedom = 4,
  confidence_level = 0.95,
  prediction_horizon = 5,
  bootstrap_samples = 200,
  include_residuals = TRUE,
  dynamic_prediction = TRUE,
  individual_profiles = FALSE,
  significance_testing = TRUE,
  model_comparison = TRUE
)
```

## Arguments

- data:

  the data as a data frame

- time:

  Time-to-event or follow-up time variable (numeric)

- status:

  Event indicator variable (1/TRUE = event occurred, 0/FALSE = censored)

- covariates:

  Covariates for time-varying coefficient analysis

- id:

  Subject identifier for longitudinal data and repeated measurements

- update_times:

  Comma-separated list of time points for coefficient updates (e.g.,
  "0.5, 1, 2, 5")

- method:

  Method for estimating time-varying regression coefficients

- smoothing_method:

  Smoothing method for time-varying coefficient estimation

- bandwidth:

  Bandwidth parameter for smoothing (0.1 to 2.0)

- degrees_freedom:

  Degrees of freedom for spline-based methods

- confidence_level:

  Confidence level for confidence bands

- prediction_horizon:

  Time horizon for dynamic survival predictions

- bootstrap_samples:

  Number of bootstrap samples for confidence band estimation

- include_residuals:

  Include residual analysis and model diagnostics

- dynamic_prediction:

  Enable dynamic survival prediction with updated coefficients

- individual_profiles:

  Generate individual-level time-varying coefficient profiles

- significance_testing:

  Perform tests for time-varying coefficients vs. constant coefficients

- model_comparison:

  Compare time-varying models with standard Cox regression

## Value

A results object containing:

|                                   |     |     |     |     |          |
|-----------------------------------|-----|-----|-----|-----|----------|
| `results$instructions`            |     |     |     |     | a html   |
| `results$modelSummary`            |     |     |     |     | a table  |
| `results$timeVaryingCoefficients` |     |     |     |     | a table  |
| `results$cumulativeCoefficients`  |     |     |     |     | a table  |
| `results$dynamicPredictions`      |     |     |     |     | a table  |
| `results$significanceTests`       |     |     |     |     | a table  |
| `results$modelComparison`         |     |     |     |     | a table  |
| `results$residualAnalysis`        |     |     |     |     | a table  |
| `results$goodnessOfFit`           |     |     |     |     | a table  |
| `results$timeVaryingPlot`         |     |     |     |     | an image |
| `results$cumulativePlot`          |     |     |     |     | an image |
| `results$dynamicPredictionPlot`   |     |     |     |     | an image |
| `results$residualPlots`           |     |     |     |     | an image |
| `results$smoothingDiagnostics`    |     |     |     |     | an image |
| `results$methodExplanation`       |     |     |     |     | a html   |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$modelSummary$asDF`

`as.data.frame(results$modelSummary)`
