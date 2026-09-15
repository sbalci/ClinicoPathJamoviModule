# Dynamic Survival Prediction

Dynamic prediction models provide updated survival predictions as new
longitudinal data becomes available. This approach is particularly
valuable when biomarker values change over time and influence prognosis.
The analysis implements various dynamic prediction methods including
landmark approaches, joint modeling, and dynamic Cox models, allowing
for real-time risk assessment and personalized medicine applications.

## Usage

``` r
dynamicprediction(
  data,
  elapsedtime,
  outcome,
  baseline,
  longitudinal,
  time_var,
  subject_id,
  outcomeLevel = "1",
  prediction_horizon = 24,
  prediction_method = "landmark",
  landmark_times = "6,12,18",
  joint_model_type = "linear_mixed",
  biomarker_model = "linear",
  association_structure = "current_value",
  confidence_level = 0.95,
  mc_samples = 500,
  window_width = 6,
  show_prediction_table = TRUE,
  show_accuracy_metrics = TRUE,
  show_biomarker_effects = TRUE,
  show_model_comparison = FALSE,
  prediction_curves = TRUE,
  biomarker_trajectory = TRUE,
  accuracy_plot = FALSE,
  risk_stratification = FALSE,
  showSummaries = TRUE,
  showExplanations = TRUE
)
```

## Arguments

- data:

  the data as a data frame

- elapsedtime:

  Time to event or censoring

- outcome:

  Event indicator (1 = event, 0 = censored)

- baseline:

  Baseline covariates that don't change over time

- longitudinal:

  Time-varying covariates (longitudinal biomarkers)

- time_var:

  Time of longitudinal measurements

- subject_id:

  Subject identifier for longitudinal data

- outcomeLevel:

  Level indicating event occurrence

- prediction_horizon:

  Time horizon for dynamic predictions (in time units)

- prediction_method:

  Method for dynamic prediction

- landmark_times:

  Comma-separated list of landmark time points for dynamic updating

- joint_model_type:

  Type of joint model for longitudinal and survival data

- biomarker_model:

  Model for longitudinal biomarker trajectory

- association_structure:

  How longitudinal process associates with survival

- confidence_level:

  Confidence level for prediction intervals

- mc_samples:

  Number of Monte Carlo samples for prediction uncertainty

- window_width:

  Width of prediction window for dynamic updating

- show_prediction_table:

  Display table of dynamic predictions

- show_accuracy_metrics:

  Display prediction accuracy assessment

- show_biomarker_effects:

  Display estimated effects of longitudinal biomarkers

- show_model_comparison:

  Compare different prediction methods

- prediction_curves:

  Display dynamic prediction curves over time

- biomarker_trajectory:

  Display individual biomarker trajectories

- accuracy_plot:

  Display prediction accuracy over time

- risk_stratification:

  Display risk stratification based on dynamic predictions

- showSummaries:

  Show comprehensive analysis summaries

- showExplanations:

  Show detailed methodology explanations

## Value

A results object containing:

|                                  |     |     |     |     |          |
|----------------------------------|-----|-----|-----|-----|----------|
| `results$todo`                   |     |     |     |     | a html   |
| `results$predictionSummary`      |     |     |     |     | a table  |
| `results$accuracyMetrics`        |     |     |     |     | a table  |
| `results$biomarkerEffects`       |     |     |     |     | a table  |
| `results$modelComparison`        |     |     |     |     | a table  |
| `results$landmarkAnalysis`       |     |     |     |     | a table  |
| `results$jointModelResults`      |     |     |     |     | a table  |
| `results$trajectoryParameters`   |     |     |     |     | a table  |
| `results$methodologyExplanation` |     |     |     |     | a html   |
| `results$analysisSummary`        |     |     |     |     | a html   |
| `results$predictionCurves`       |     |     |     |     | an image |
| `results$biomarkerTrajectory`    |     |     |     |     | an image |
| `results$accuracyPlot`           |     |     |     |     | an image |
| `results$riskStratification`     |     |     |     |     | an image |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$predictionSummary$asDF`

`as.data.frame(results$predictionSummary)`
