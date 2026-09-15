# Landmark Analysis for Time-varying Predictors

Landmark analysis for survival data with time-varying predictors.
Provides unbiased estimates by analyzing survival from fixed landmark
times.

## Usage

``` r
landmarkanalysis(
  data,
  time = NULL,
  status = NULL,
  predictors = NULL,
  landmark_times = "6, 12, 24",
  prediction_window = 12,
  min_events = 10,
  include_baseline = TRUE,
  dynamic_prediction = TRUE,
  calibration_plot = TRUE,
  discrimination_plot = TRUE,
  supermodel = FALSE,
  bootstrap_validation = FALSE,
  n_bootstrap = 200
)
```

## Arguments

- data:

  The data as a data frame.

- time:

  Follow-up time variable

- status:

  Event status variable

- predictors:

  Predictor variables for landmark analysis

- landmark_times:

  Vector of landmark time points

- prediction_window:

  Length of prediction window from each landmark

- min_events:

  Minimum events threshold for landmark analysis

- include_baseline:

  Whether to include baseline Cox model

- dynamic_prediction:

  Calculate time-dependent risk predictions

- calibration_plot:

  Generate calibration assessment plots

- discrimination_plot:

  Generate discrimination assessment plots

- supermodel:

  Combine landmark models using super model

- bootstrap_validation:

  Use bootstrap for model validation

- n_bootstrap:

  Number of bootstrap replications

## Value

A results object containing:

|                                 |     |     |     |     |          |
|---------------------------------|-----|-----|-----|-----|----------|
| `results$todo`                  |     |     |     |     | a html   |
| `results$errors`                |     |     |     |     | a html   |
| `results$warnings`              |     |     |     |     | a html   |
| `results$summary`               |     |     |     |     | a html   |
| `results$baselineModel`         |     |     |     |     | a table  |
| `results$landmarkResults`       |     |     |     |     | a table  |
| `results$modelComparison`       |     |     |     |     | a table  |
| `results$dynamicPredictionPlot` |     |     |     |     | an image |
| `results$calibrationPlot`       |     |     |     |     | an image |
| `results$discriminationPlot`    |     |     |     |     | an image |
| `results$superModelResults`     |     |     |     |     | a table  |
| `results$validationResults`     |     |     |     |     | a html   |
| `results$clinicalGuidance`      |     |     |     |     | a html   |
| `results$technicalNotes`        |     |     |     |     | a html   |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$baselineModel$asDF`

`as.data.frame(results$baselineModel)`
