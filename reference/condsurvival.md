# Conditional Survival Analysis

Conditional survival analysis for calculating the probability of
surviving an additional time period given that a patient has already
survived a certain time. Essential for long-term prognosis and dynamic
patient counseling in clinical practice.

## Usage

``` r
condsurvival(
  data,
  time,
  status,
  group,
  id,
  conditional_times = "1, 2, 5",
  prediction_times = "1, 3, 5",
  survival_method = "kaplan_meier",
  parametric_dist = "weibull",
  confidence_level = 0.95,
  time_scale = "years",
  include_plots = TRUE,
  include_tables = TRUE,
  dynamic_prediction = FALSE,
  landmark_analysis = FALSE,
  clinical_interpretation = TRUE
)
```

## Arguments

- data:

  the data as a data frame

- time:

  Time-to-event or follow-up time variable (numeric)

- status:

  Event indicator variable (1/TRUE = event occurred, 0/FALSE = censored)

- group:

  Optional grouping variable for stratified conditional survival
  analysis

- id:

  Optional subject identifier for longitudinal analysis

- conditional_times:

  Comma-separated list of time points for conditional survival (e.g.,
  "1, 2, 5")

- prediction_times:

  Comma-separated list of additional survival times to predict (e.g.,
  "1, 3, 5")

- survival_method:

  Method for estimating baseline survival curves

- parametric_dist:

  Parametric distribution for survival modeling (when parametric method
  selected)

- confidence_level:

  Confidence level for confidence intervals

- time_scale:

  Time scale for interpretation and reporting

- include_plots:

  Generate conditional survival plots and visualizations

- include_tables:

  Include detailed conditional survival probability tables

- dynamic_prediction:

  Enable dynamic prediction capabilities for individual patients

- landmark_analysis:

  Perform landmark analysis to address immortal time bias

- clinical_interpretation:

  Include clinical interpretation guidance for conditional survival
  results

## Value

A results object containing:

|                                       |     |     |     |     |          |
|---------------------------------------|-----|-----|-----|-----|----------|
| `results$instructions`                |     |     |     |     | a html   |
| `results$dataOverview`                |     |     |     |     | a table  |
| `results$conditionalSurvivalTable`    |     |     |     |     | a table  |
| `results$conditionalImprovementTable` |     |     |     |     | a table  |
| `results$dynamicPredictionTable`      |     |     |     |     | a table  |
| `results$landmarkAnalysisTable`       |     |     |     |     | a table  |
| `results$modelFitStatistics`          |     |     |     |     | a table  |
| `results$groupComparisonTable`        |     |     |     |     | a table  |
| `results$conditionalSurvivalPlot`     |     |     |     |     | an image |
| `results$conditionalImprovementPlot`  |     |     |     |     | an image |
| `results$dynamicPredictionPlot`       |     |     |     |     | an image |
| `results$landmarkPlot`                |     |     |     |     | an image |
| `results$methodExplanation`           |     |     |     |     | a html   |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$dataOverview$asDF`

`as.data.frame(results$dataOverview)`
