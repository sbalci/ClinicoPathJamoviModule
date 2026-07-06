# Direct Binomial Regression for Competing Risks

Performs direct binomial regression modeling for cumulative incidence
functions in competing risks analysis using the timereg package. This
method provides direct modeling of cumulative incidence without
proportional subdistribution hazards assumptions.

## Usage

``` r
directbinomial(
  data,
  time,
  event,
  covs,
  eventOfInterest = 1,
  times = "1, 3, 5",
  conf = 0.95,
  residuals = FALSE,
  predictions = TRUE,
  bootstrap = FALSE,
  nboot = 500,
  showModel = TRUE,
  showEducational = TRUE,
  plotCIF = TRUE,
  plotResiduals = FALSE
)
```

## Arguments

- data:

  The data as a data frame.

- time:

  Follow-up time variable

- event:

  Event type variable (0=censored, 1=event of interest, 2=competing
  event, etc.)

- covs:

  Covariates to include in the model

- eventOfInterest:

  Numeric code for the event of interest (primary event)

- times:

  Comma-separated list of time points for cumulative incidence
  prediction

- conf:

  Confidence level for confidence intervals

- residuals:

  Display model residuals and goodness-of-fit statistics

- predictions:

  Display predicted cumulative incidence at specified time points

- bootstrap:

  Use bootstrap methods for confidence interval estimation

- nboot:

  Number of bootstrap samples (when bootstrap is enabled)

- showModel:

  Display detailed model summary and parameter estimates

- showEducational:

  Display educational information about direct binomial regression

- plotCIF:

  Display cumulative incidence function plots

- plotResiduals:

  Display residual plots for model diagnostics

## Value

A results object containing:

|                                    |     |     |     |     |          |
|------------------------------------|-----|-----|-----|-----|----------|
| `results$todo`                     |     |     |     |     | a html   |
| `results$modelSummary`             |     |     |     |     | a table  |
| `results$educationalInfo`          |     |     |     |     | a html   |
| `results$coefficientsTable`        |     |     |     |     | a table  |
| `results$cumulativeIncidenceTable` |     |     |     |     | a table  |
| `results$covariateEffectsTable`    |     |     |     |     | a table  |
| `results$goodnessOfFit`            |     |     |     |     | a table  |
| `results$residualAnalysis`         |     |     |     |     | a html   |
| `results$methodsInfo`              |     |     |     |     | a html   |
| `results$cifPlot`                  |     |     |     |     | an image |
| `results$residualPlot`             |     |     |     |     | an image |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$modelSummary$asDF`

`as.data.frame(results$modelSummary)`
