# Clinical Prediction Models & ML Interpretability

Develop and validate clinical prediction models using machine learning
algorithms with interpretability analysis. Includes comprehensive model
comparison, feature selection, cross-validation, and explainable AI
through SHAP and LIME methods. Designed for clinical research
applications with regulatory compliance features for model validation
and documentation.

## Usage

``` r
clinicalprediction(
  data,
  outcome_var,
  predictor_vars,
  patient_id,
  time_var,
  event_var,
  stratify_var,
  model_type = "random_forest",
  problem_type = "classification",
  outcome_type = "binary",
  feature_selection = TRUE,
  selection_method = "recursive_fe",
  feature_engineering = TRUE,
  handle_missing = "mice_imputation",
  train_proportion = 0.7,
  validation_method = "cv_10fold",
  hyperparameter_tuning = TRUE,
  tuning_method = "random_search",
  random_seed = 42,
  interpretability = TRUE,
  shap_analysis = TRUE,
  lime_analysis = FALSE,
  permutation_importance = TRUE,
  partial_dependence = TRUE,
  individual_explanations = FALSE,
  n_explanations = 10,
  performance_metrics = TRUE,
  calibration_analysis = TRUE,
  clinical_metrics = TRUE,
  roc_analysis = TRUE,
  threshold_optimization = TRUE,
  compare_models = FALSE,
  baseline_models = TRUE,
  ensemble_models = FALSE,
  risk_stratification = TRUE,
  n_risk_groups = 3,
  nomogram = TRUE,
  decision_curve = TRUE,
  external_validation = TRUE,
  bootstrap_ci = 1000,
  stability_analysis = TRUE,
  bias_analysis = TRUE,
  detailed_output = TRUE,
  clinical_report = TRUE,
  save_model = FALSE,
  export_predictions = FALSE,
  regulatory_documentation = TRUE
)
```

## Arguments

- data:

  the data as a data frame

- outcome_var:

  Target variable for prediction (binary, continuous, or survival)

- predictor_vars:

  Variables to use as predictors in the model

- patient_id:

  Patient identifier for tracking predictions

- time_var:

  Time to event variable for survival prediction models

- event_var:

  Event indicator for survival prediction models

- stratify_var:

  Variable for stratified sampling and validation

- model_type:

  Type of machine learning model to fit

- problem_type:

  Type of prediction problem

- outcome_type:

  Specific type of outcome variable

- feature_selection:

  Perform automated feature selection

- selection_method:

  Method for automatic feature selection

- feature_engineering:

  Perform automated feature engineering

- handle_missing:

  Method for handling missing data

- train_proportion:

  Proportion of data for training (70 percent = 0.7)

- validation_method:

  Method for model validation

- hyperparameter_tuning:

  Perform automated hyperparameter optimization

- tuning_method:

  Method for hyperparameter tuning

- random_seed:

  Random seed for reproducibility

- interpretability:

  Generate model interpretability analysis

- shap_analysis:

  Generate SHAP (SHapley Additive exPlanations) values

- lime_analysis:

  Generate LIME (Local Interpretable Model-agnostic Explanations)

- permutation_importance:

  Calculate permutation feature importance

- partial_dependence:

  Generate partial dependence plots for key features

- individual_explanations:

  Explain individual predictions using SHAP/LIME

- n_explanations:

  Number of individual predictions to explain in detail

- performance_metrics:

  Calculate comprehensive performance metrics

- calibration_analysis:

  Assess model calibration

- clinical_metrics:

  Calculate clinical decision analysis metrics

- roc_analysis:

  Perform ROC curve analysis with confidence intervals

- threshold_optimization:

  Optimize prediction threshold for clinical use

- compare_models:

  Compare multiple model types

- baseline_models:

  Include simple baseline models for comparison

- ensemble_models:

  Create ensemble of best performing models

- risk_stratification:

  Create risk stratification groups

- n_risk_groups:

  Number of risk stratification groups (e.g., low/medium/high)

- nomogram:

  Create clinical nomogram for risk calculation

- decision_curve:

  Perform decision curve analysis for clinical utility

- external_validation:

  Prepare model for external validation

- bootstrap_ci:

  Number of bootstrap samples for confidence intervals

- stability_analysis:

  Assess model stability across different samples

- bias_analysis:

  Analyze model bias across demographic groups

- detailed_output:

  Include detailed model diagnostics and explanations

- clinical_report:

  Generate clinical interpretation report

- save_model:

  Save trained model for future predictions

- export_predictions:

  Export individual predictions with probabilities

- regulatory_documentation:

  Include documentation for regulatory submission

## Value

A results object containing:

|                                       |     |     |     |     |          |
|---------------------------------------|-----|-----|-----|-----|----------|
| `results$overview`                    |     |     |     |     | a table  |
| `results$dataset_info`                |     |     |     |     | a table  |
| `results$feature_selection_results`   |     |     |     |     | a table  |
| `results$feature_engineering_summary` |     |     |     |     | a table  |
| `results$performance_summary`         |     |     |     |     | a table  |
| `results$classification_metrics`      |     |     |     |     | a table  |
| `results$survival_metrics`            |     |     |     |     | a table  |
| `results$feature_importance`          |     |     |     |     | a table  |
| `results$shap_summary`                |     |     |     |     | a table  |
| `results$individual_explanations`     |     |     |     |     | a table  |
| `results$model_comparison`            |     |     |     |     | a table  |
| `results$risk_stratification`         |     |     |     |     | a table  |
| `results$decision_curve_analysis`     |     |     |     |     | a table  |
| `results$cross_validation_results`    |     |     |     |     | a table  |
| `results$stability_analysis`          |     |     |     |     | a table  |
| `results$bias_fairness_analysis`      |     |     |     |     | a table  |
| `results$hyperparameter_results`      |     |     |     |     | a table  |
| `results$clinical_interpretation`     |     |     |     |     | a table  |
| `results$regulatory_summary`          |     |     |     |     | a table  |
| `results$roc_plot`                    |     |     |     |     | an image |
| `results$calibration_plot`            |     |     |     |     | an image |
| `results$feature_importance_plot`     |     |     |     |     | an image |
| `results$shap_summary_plot`           |     |     |     |     | an image |
| `results$shap_dependence_plot`        |     |     |     |     | an image |
| `results$decision_curve_plot`         |     |     |     |     | an image |
| `results$risk_distribution_plot`      |     |     |     |     | an image |
| `results$model_comparison_plot`       |     |     |     |     | an image |
| `results$stability_plot`              |     |     |     |     | an image |
| `results$nomogram_plot`               |     |     |     |     | an image |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$overview$asDF`

`as.data.frame(results$overview)`
