# Time-Dependent Decision Curve Analysis

Perform time-dependent Decision Curve Analysis (DCA) for survival data
using the dcurves package. This method extends standard DCA to evaluate
the clinical utility of prognostic models with time-to-event outcomes.
It calculates net benefit at specific time points, accounting for
censoring and time-varying risk predictions. It's essential for
evaluating survival models, recurrence prediction models, and other
longitudinal outcomes. Common applications: Serial biopsy surveillance,
recurrence vs death prediction, time-varying treatment decisions.

## Usage

``` r
timedependentdca(
  data,
  time = NULL,
  event = NULL,
  predictors = list(),
  time_points = "365, 730, 1095",
  threshold_range_min = 0.01,
  threshold_range_max = 0.99,
  threshold_steps = 100,
  reference_strategy = "both",
  estimate_survival = "kaplan_meier",
  smoothing = FALSE,
  use_bootstrap = FALSE,
  bootstrap_iterations = 500,
  ci_level = 0.95,
  plot_net_benefit = FALSE,
  plot_by_timepoint = FALSE,
  plot_interventions_avoided = FALSE,
  random_seed = 42
)
```

## Arguments

- data:

  the data as a data frame

- time:

  a string naming the time-to-event variable

- event:

  a string naming the event indicator (1=event, 0=censored)

- predictors:

  one or more risk score or prediction variables

- time_points:

  comma-separated time points at which to calculate net benefit (e.g.,
  "365, 730" for 1 and 2 years)

- threshold_range_min:

  minimum threshold probability for decision curve

- threshold_range_max:

  maximum threshold probability for decision curve

- threshold_steps:

  number of threshold probabilities to evaluate

- reference_strategy:

  reference strategy for comparison

- estimate_survival:

  method for estimating event probabilities from predictor

- smoothing:

  apply LOESS smoothing to decision curves for visualization

- use_bootstrap:

  calculate 95 percent confidence intervals for net benefit using
  bootstrap resampling (computationally intensive)

- bootstrap_iterations:

  number of bootstrap iterations for confidence interval calculation

- ci_level:

  confidence level for bootstrap intervals (e.g., 0.95 for 95 percent
  CI)

- plot_net_benefit:

  plot net benefit curves across threshold probabilities

- plot_by_timepoint:

  create separate plots for each time point (vs. overlay)

- plot_interventions_avoided:

  plot number of interventions avoided per 100 patients

- random_seed:

  random seed for reproducible bootstrap sampling

## Value

A results object containing:

|                              |     |     |     |     |          |
|------------------------------|-----|-----|-----|-----|----------|
| `results$instructionsText`   |     |     |     |     | a html   |
| `results$notices`            |     |     |     |     | a html   |
| `results$netBenefitTable`    |     |     |     |     | a table  |
| `results$summaryTable`       |     |     |     |     | a table  |
| `results$interventionsTable` |     |     |     |     | a table  |
| `results$comparisonTable`    |     |     |     |     | a table  |
| `results$netBenefitPlot`     |     |     |     |     | an image |
| `results$interventionsPlot`  |     |     |     |     | an image |
| `results$interpretationText` |     |     |     |     | a html   |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$netBenefitTable$asDF`

`as.data.frame(results$netBenefitTable)`
