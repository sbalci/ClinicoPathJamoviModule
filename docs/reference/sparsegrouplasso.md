# Sparse Group LASSO

Sparse Group LASSO regularization for survival analysis

## Usage

``` r
sparsegrouplasso(
  data,
  time_var,
  event_var,
  outcomeLevel,
  censorLevel,
  pred_vars,
  suitabilityCheck = TRUE,
  group_definition = "factor_based",
  custom_groups = "",
  pathway_info,
  correlation_threshold = 0.7,
  alpha_sgl = 0.95,
  lambda_sequence = "auto",
  custom_lambda = "",
  lambda_min_ratio = 0.001,
  n_lambda = 100,
  selection_criterion = "cv_deviance",
  cv_folds = 10,
  cv_repeats = 1,
  ebic_gamma = 1,
  weight_type = "none",
  weight_power = 1,
  standardize_vars = TRUE,
  center_vars = TRUE,
  seed_value = 42,
  show_summary = TRUE,
  show_coefficients = TRUE,
  show_groups = TRUE,
  show_path = FALSE,
  show_performance = TRUE,
  show_validation = TRUE,
  plot_cv_error = TRUE,
  plot_coefficients = TRUE,
  plot_groups = TRUE,
  plot_sparsity = FALSE,
  plot_stability = FALSE,
  alpha_level = 0.05,
  confidence_intervals = FALSE,
  bootstrap_samples = 500,
  stability_selection = FALSE,
  stability_threshold = 0.8,
  stability_subsample = 0.8,
  showExplanations = TRUE
)
```

## Arguments

- data:

  the data as a data frame

- time_var:

  the time-to-event variable

- event_var:

  the event indicator variable (0/1 or FALSE/TRUE)

- outcomeLevel:

  Level of `event_var` that represents the event of interest (coded as
  1). Rows matching neither this level nor `censorLevel` are excluded as
  NA.

- censorLevel:

  Level of `event_var` that represents censoring (coded as 0). Together
  with `outcomeLevel`, this defines a strict two-level encoding: rows
  whose event value matches neither level are treated as missing and
  excluded.

- pred_vars:

  the predictor variables for the model

- suitabilityCheck:

  assess if data is suitable for the selected regularized Cox model

- group_definition:

  method for defining variable groups

- custom_groups:

  custom group specification as comma-separated lists. Example:
  "1,2;3,4,5;6" for three groups

- pathway_info:

  variable containing pathway/cluster information for grouping

- correlation_threshold:

  correlation threshold for correlation-based grouping

- alpha_sgl:

  mixing parameter between group LASSO (0) and LASSO (1). Default 0.95
  emphasizes sparsity within groups

- lambda_sequence:

  method for selecting regularization parameter sequence

- custom_lambda:

  custom lambda values as comma-separated numbers. Example:
  "0.001,0.01,0.1,1"

- lambda_min_ratio:

  ratio of smallest to largest lambda value

- n_lambda:

  number of lambda values in the sequence

- selection_criterion:

  Criterion for selecting optimal lambda. CV Deviance uses
  cross-validated partial likelihood deviance (recommended).
  AIC/BIC/EBIC add model complexity penalties to the deviance for more
  parsimonious selection.

- cv_folds:

  number of folds for cross-validation

- cv_repeats:

  number of repeated cross-validation runs

- ebic_gamma:

  gamma parameter for Extended BIC (0 = standard BIC)

- weight_type:

  type of adaptive weights for penalty terms

- weight_power:

  power parameter for adaptive weights

- standardize_vars:

  whether to standardize predictor variables

- center_vars:

  whether to center predictor variables

- seed_value:

  random seed for reproducible results

- show_summary:

  show model summary table

- show_coefficients:

  show coefficient estimates table

- show_groups:

  show group structure table

- show_path:

  show complete regularization path

- show_performance:

  show model performance metrics

- show_validation:

  show cross-validation results

- plot_cv_error:

  plot cross-validation error curve

- plot_coefficients:

  plot coefficient regularization path

- plot_groups:

  plot group selection pattern

- plot_sparsity:

  plot sparsity pattern across lambda values

- plot_stability:

  plot stability selection results

- alpha_level:

  significance level for confidence intervals

- confidence_intervals:

  whether to calculate bootstrap confidence intervals

- bootstrap_samples:

  number of bootstrap samples for confidence intervals

- stability_selection:

  whether to perform stability selection

- stability_threshold:

  threshold for stability selection

- stability_subsample:

  subsample ratio for stability selection

- showExplanations:

  show explanations for the analysis

## Value

A results object containing:

|                              |     |     |     |     |                |
|------------------------------|-----|-----|-----|-----|----------------|
| `results$notices`            |     |     |     |     | a preformatted |
| `results$instructions`       |     |     |     |     | a html         |
| `results$todo`               |     |     |     |     | a html         |
| `results$suitabilityReport`  |     |     |     |     | a html         |
| `results$summary`            |     |     |     |     | a table        |
| `results$coefficients`       |     |     |     |     | a table        |
| `results$groupStructure`     |     |     |     |     | a table        |
| `results$solutionPath`       |     |     |     |     | a table        |
| `results$performance`        |     |     |     |     | a table        |
| `results$validationResults`  |     |     |     |     | a table        |
| `results$adaptiveWeights`    |     |     |     |     | a table        |
| `results$stabilityResults`   |     |     |     |     | a table        |
| `results$comparisonTable`    |     |     |     |     | a table        |
| `results$cvErrorPlot`        |     |     |     |     | an image       |
| `results$coefficientPlot`    |     |     |     |     | an image       |
| `results$groupSelectionPlot` |     |     |     |     | an image       |
| `results$sparsityPlot`       |     |     |     |     | an image       |
| `results$stabilityPlot`      |     |     |     |     | an image       |
| `results$explanations`       |     |     |     |     | a html         |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$summary$asDF`

`as.data.frame(results$summary)`
