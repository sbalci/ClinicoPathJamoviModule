# Survival Model Validation

Comprehensive validation and performance assessment for survival models.
Includes prediction error curves, time-dependent ROC analysis,
calibration plots, and decision curve analysis for clinical decision
making.

## Usage

``` r
survivalvalidation(
  data,
  time = NULL,
  status = NULL,
  predicted_risk = NULL,
  covariates = NULL,
  model_formula = "",
  external_data,
  validation_method = "cv",
  cv_folds = 10,
  bootstrap_samples = 500,
  concordance_index = TRUE,
  time_dependent_auc = TRUE,
  prediction_error = TRUE,
  integrated_brier = TRUE,
  calibration_plot = TRUE,
  decision_curve = TRUE,
  time_points = "1,2,3,5",
  max_time = 0,
  plot_roc_curves = TRUE,
  plot_calibration = TRUE,
  plot_decision_curve = TRUE,
  plot_prediction_error = TRUE,
  confidence_level = 0.95,
  smoothing = TRUE,
  risk_groups = 4,
  competing_risks = FALSE,
  cause_specific = NULL,
  model_comparison = FALSE,
  model_names = "",
  net_benefit_thresholds = "0.01,0.05,0.1,0.2,0.3"
)
```

## Arguments

- data:

  The data as a data frame.

- time:

  Time to event or censoring

- status:

  Event indicator variable

- predicted_risk:

  Model predictions to validate

- covariates:

  Covariates to include in the model formula

- model_formula:

  Variables for Cox model if predictions not provided

- external_data:

  Optional external dataset for validation

- validation_method:

  Validation approach

- cv_folds:

  CV fold specification

- bootstrap_samples:

  Bootstrap iterations

- concordance_index:

  Compute C-index

- time_dependent_auc:

  Compute time-dependent AUC

- prediction_error:

  Compute prediction error curves

- integrated_brier:

  Compute IBS

- calibration_plot:

  Create calibration assessment

- decision_curve:

  DCA for clinical decision making

- time_points:

  Specific times for assessment

- max_time:

  Upper time limit

- plot_roc_curves:

  Generate ROC plots

- plot_calibration:

  Generate calibration plots

- plot_decision_curve:

  Generate DCA plots

- plot_prediction_error:

  Generate PEC plots

- confidence_level:

  CI level

- smoothing:

  Smooth curve estimation

- risk_groups:

  Risk group stratification

- competing_risks:

  Competing risks consideration

- cause_specific:

  Cause-specific event variable

- model_comparison:

  Multi-model comparison

- model_names:

  Model labels for comparison

- net_benefit_thresholds:

  DCA threshold range

## Value

A results object containing:

|                                 |     |     |     |     |          |
|---------------------------------|-----|-----|-----|-----|----------|
| `results$todo`                  |     |     |     |     | a html   |
| `results$summary`               |     |     |     |     | a html   |
| `results$performanceTable`      |     |     |     |     | a table  |
| `results$concordanceTable`      |     |     |     |     | a table  |
| `results$aucTable`              |     |     |     |     | a table  |
| `results$brierTable`            |     |     |     |     | a table  |
| `results$calibrationTable`      |     |     |     |     | a table  |
| `results$decisionTable`         |     |     |     |     | a table  |
| `results$rocPlot`               |     |     |     |     | an image |
| `results$calibrationPlot`       |     |     |     |     | an image |
| `results$decisionPlot`          |     |     |     |     | an image |
| `results$predErrorPlot`         |     |     |     |     | an image |
| `results$calibrationMetrics`    |     |     |     |     | a table  |
| `results$validationSummary`     |     |     |     |     | a html   |
| `results$modelComparison`       |     |     |     |     | a table  |
| `results$externalValidation`    |     |     |     |     | a html   |
| `results$competingRisksMetrics` |     |     |     |     | a table  |
| `results$interpretation`        |     |     |     |     | a html   |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$performanceTable$asDF`

`as.data.frame(results$performanceTable)`
