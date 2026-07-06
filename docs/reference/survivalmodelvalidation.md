# Enhanced Survival Model Validation

Comprehensive validation toolkit for survival models including
calibration assessment, prediction performance metrics, bootstrap
validation, and external validation frameworks. Essential for developing
reliable prognostic models.

## Usage

``` r
survivalmodelvalidation(
  data,
  time_var,
  status_var,
  risk_score,
  covariates,
  stratification_var,
  validation_type = "internal_bootstrap",
  model_type = "cox_ph",
  performance_metrics = "all_metrics",
  calibration_assessment = TRUE,
  calibration_method = "decile_based",
  bootstrap_samples = 1000,
  cv_folds = 10,
  prediction_times = "1,3,5,10",
  confidence_level = 0.95,
  optimism_correction = TRUE,
  shrinkage_estimation = TRUE,
  discrimination_plots = TRUE,
  calibration_plots = TRUE,
  roc_curves = TRUE,
  prediction_error_plots = TRUE,
  risk_distribution_plots = TRUE,
  model_comparison = FALSE,
  decision_curve_analysis = TRUE,
  net_benefit_range = "0.01,0.99",
  external_data_source = "",
  transportability_analysis = FALSE,
  subgroup_validation = TRUE,
  temporal_trends = FALSE,
  missing_data_sensitivity = FALSE,
  clinical_impact_metrics = TRUE,
  reporting_guidelines = "tripod",
  export_validation_report = FALSE
)
```

## Arguments

- data:

  The data as a data frame.

- time_var:

  Time to event or censoring

- status_var:

  Event indicator variable

- risk_score:

  Model predictions to validate

- covariates:

  Covariates for model fitting/validation

- stratification_var:

  Stratification factor

- validation_type:

  Validation methodology

- model_type:

  Model class specification

- performance_metrics:

  Metrics for model assessment

- calibration_assessment:

  Enable calibration evaluation

- calibration_method:

  Calibration evaluation approach

- bootstrap_samples:

  Bootstrap iteration count

- cv_folds:

  CV fold specification

- prediction_times:

  Times for time-dependent metrics

- confidence_level:

  CI level

- optimism_correction:

  Correct for overfitting bias

- shrinkage_estimation:

  Calculate shrinkage coefficient

- discrimination_plots:

  Generate discrimination visualizations

- calibration_plots:

  Generate calibration visualizations

- roc_curves:

  Generate ROC visualizations

- prediction_error_plots:

  Generate error curve plots

- risk_distribution_plots:

  Generate risk distribution plots

- model_comparison:

  Enable model comparison analysis

- decision_curve_analysis:

  Evaluate clinical decision-making utility

- net_benefit_range:

  Threshold range for net benefit

- external_data_source:

  External validation data source

- transportability_analysis:

  Evaluate population transportability

- subgroup_validation:

  Enable subgroup-specific validation

- temporal_trends:

  Assess time-based performance changes

- missing_data_sensitivity:

  Evaluate missing data impact

- clinical_impact_metrics:

  Assess clinical decision impact

- reporting_guidelines:

  Structured reporting format

- export_validation_report:

  Generate exportable report

## Value

A results object containing:

|  |  |  |  |  |  |
|----|----|----|----|----|----|
| `results$instructions` |  |  |  |  | Analysis instructions and overview |
| `results$modelSummary` |  |  |  |  | Summary of the survival model being validated |
| `results$performanceMetrics` |  |  |  |  | Comprehensive model performance metrics |
| `results$calibrationMetrics` |  |  |  |  | Model calibration metrics and tests |
| `results$validationResults` |  |  |  |  | Detailed validation results by method |
| `results$discriminationAnalysis` |  |  |  |  | Model discrimination assessment |
| `results$calibrationAnalysis` |  |  |  |  | Detailed calibration assessment |
| `results$subgroupAnalysis` |  |  |  |  | Performance across subgroups |
| `results$decisionCurveResults` |  |  |  |  | Clinical decision-making utility assessment |
| `results$clinicalImpact` |  |  |  |  | Clinical relevance and impact metrics |
| `results$discriminationPlot` |  |  |  |  | Discrimination visualization |
| `results$calibrationPlot` |  |  |  |  | Calibration assessment visualization |
| `results$rocCurvePlot` |  |  |  |  | Time-dependent ROC curve visualization |
| `results$predictionErrorPlot` |  |  |  |  | Prediction error assessment |
| `results$riskDistributionPlot` |  |  |  |  | Risk score distribution by outcome |
| `results$decisionCurvePlot` |  |  |  |  | Decision curve visualization |
| `results$validationReport` |  |  |  |  | Structured validation report |
| `results$methodologicalNotes` |  |  |  |  | Detailed methodology and interpretation guidance |
| `results$recommendations` |  |  |  |  | Clinical and methodological recommendations |
| `results$diagnostics` |  |  |  |  | Diagnostic information and warnings |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$performanceMetrics$asDF`

`as.data.frame(results$performanceMetrics)`
