# Clinical Risk Calculators & Nomograms

Clinical Risk Calculators & Nomograms

## Usage

``` r
clinicalcalculators(
  data,
  calculator_type = "risk_score",
  outcome_variable,
  predictor_variables,
  time_variable,
  event_variable,
  stratification_variable,
  model_type = "logistic_regression",
  validation_method = "bootstrap",
  risk_categories = 3,
  risk_thresholds = "",
  confidence_level = 0.95,
  bootstrap_samples = 1000,
  cv_folds = 10,
  include_nomogram = TRUE,
  include_calibration = TRUE,
  include_discrimination = TRUE,
  include_decision_curve = TRUE,
  include_net_benefit = TRUE,
  clinical_threshold_low = 0.1,
  clinical_threshold_high = 0.3,
  feature_selection = FALSE,
  feature_selection_method = "stepwise",
  regularization_alpha = 1,
  missing_data_method = "complete_case",
  outlier_detection = FALSE,
  outlier_method = "iqr",
  calculator_name = "Clinical Risk Calculator",
  calculator_description = "",
  target_population = "",
  units_specification = "",
  reference_ranges = "",
  include_uncertainty = TRUE,
  interactive_calculator = TRUE,
  export_format = "html",
  performance_metrics = "comprehensive",
  include_interpretation = TRUE,
  interpretation_text = "",
  risk_communication = "multiple_formats",
  validation_data_source = "",
  regulatory_compliance = "none",
  bias_assessment = TRUE,
  subgroup_analysis = FALSE,
  sensitivity_analysis = TRUE,
  implementation_guide = TRUE
)
```

## Arguments

- data:

  the data as a data frame

- calculator_type:

  the type of clinical calculator to generate

- outcome_variable:

  primary outcome variable for the calculator

- predictor_variables:

  predictor variables for the risk model

- time_variable:

  time variable for survival analysis (if applicable)

- event_variable:

  event/status variable for survival analysis (if applicable)

- stratification_variable:

  variable for subgroup stratification (optional)

- model_type:

  statistical model for the calculator

- validation_method:

  method for model validation

- risk_categories:

  number of risk categories for stratification

- risk_thresholds:

  custom risk thresholds (comma-separated percentages)

- confidence_level:

  confidence level for intervals

- bootstrap_samples:

  number of bootstrap samples for validation

- cv_folds:

  number of folds for cross-validation

- include_nomogram:

  whether to generate a nomogram visualization

- include_calibration:

  whether to include model calibration assessment

- include_discrimination:

  whether to include discrimination analysis

- include_decision_curve:

  whether to include decision curve analysis

- include_net_benefit:

  whether to include net benefit calculations

- clinical_threshold_low:

  lower threshold for clinical decision making

- clinical_threshold_high:

  upper threshold for clinical decision making

- feature_selection:

  whether to perform automatic feature selection

- feature_selection_method:

  method for automatic feature selection

- regularization_alpha:

  alpha parameter for elastic net (1=LASSO, 0=Ridge)

- missing_data_method:

  method for handling missing data

- outlier_detection:

  whether to detect and handle outliers

- outlier_method:

  method for outlier detection

- calculator_name:

  name for the clinical calculator

- calculator_description:

  description of the calculator's purpose and application

- target_population:

  description of the target patient population

- units_specification:

  units for variables (e.g., "age:years,weight:kg")

- reference_ranges:

  normal reference ranges for variables

- include_uncertainty:

  whether to include prediction uncertainty

- interactive_calculator:

  whether to generate an interactive web calculator

- export_format:

  format for exporting the calculator

- performance_metrics:

  level of performance metrics to include

- include_interpretation:

  whether to include clinical interpretation guidelines

- interpretation_text:

  custom text for result interpretation

- risk_communication:

  format for communicating risk to patients

- validation_data_source:

  source of external validation data (if applicable)

- regulatory_compliance:

  regulatory framework for compliance

- bias_assessment:

  whether to include bias and fairness assessment

- subgroup_analysis:

  whether to perform subgroup-specific analyses

- sensitivity_analysis:

  whether to include sensitivity analysis

- implementation_guide:

  whether to include clinical implementation guidelines

## Value

A results object containing:

|                              |     |     |     |     |          |
|------------------------------|-----|-----|-----|-----|----------|
| `results$instructions`       |     |     |     |     | a html   |
| `results$data_summary`       |     |     |     |     | a table  |
| `results$model_summary`      |     |     |     |     | a table  |
| `results$risk_table`         |     |     |     |     | a table  |
| `results$nomogram_plot`      |     |     |     |     | an image |
| `results$calibration_plot`   |     |     |     |     | an image |
| `results$method_explanation` |     |     |     |     | a html   |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$data_summary$asDF`

`as.data.frame(results$data_summary)`
